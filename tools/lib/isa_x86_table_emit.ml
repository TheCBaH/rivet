let ( let* ) = Result.bind
let rows_path repo = Fpath.(Repo.path repo / "targets" / "x86_family" / "x86_table_rows.ml")

let builders_path repo bits =
  Fpath.(Repo.path repo / "targets" / "x86_asm" / Printf.sprintf "x86_asm_rows%d.ml" bits)

let specs repo target =
  let* records =
    Isa_source_record.read_file (Repo.isa_db_export repo ~source:"xed_resolved" target)
  in
  Ok
    (List.filter_map
       (fun rec_ ->
         if Isa_norm_xed.table_owned (Isa_norm_xed.normalize_hand_written rec_) then
           Isa_x86_table.spec_of_record rec_
         else None)
       records
    |> Isa_x86_table.inherit_suffix_rule records
    |> Isa_x86_table.mark_directional records)

(* Every table-expressible record of one export, including those the hand-written rules own:
   twins are decided over all of them, and hand-written forms get rows too, used only when the
   hand-written encoder declines a shape (a high VEX register). *)
let all_specs repo target =
  let* records =
    Isa_source_record.read_file (Repo.isa_db_export repo ~source:"xed_resolved" target)
  in
  Ok
    (List.filter_map Isa_x86_table.spec_of_record records
    |> Isa_x86_table.inherit_suffix_rule records
    |> Isa_x86_table.mark_directional records)

let render_class : Isa_x86_table.rclass -> string = function
  | Gpr8 -> "Gpr8"
  | Gpr16 -> "Gpr16"
  | Gpr32 -> "Gpr32"
  | Gpr64 -> "Gpr64"
  | Gprv -> invalid_arg "Isa_x86_table_emit: GPRv is expanded before emission"
  | Xmm -> "Xmm"
  | Ymm -> "Ymm"
  | Zmm -> "Zmm"
  | Mmx -> "Mmx"
  | St -> "St"
  | Tmm -> "Tmm"
  | Cr -> "Cr"
  | Dr -> "Dr"
  | Kmask -> "Kmask"

let render_field : Isa_x86_table.field -> string = function
  | Modrm_reg -> "Modrm_reg"
  | Modrm_rm -> "Modrm_rm"
  | Vvvv -> "Vvvv"
  | Is4 -> "Is4"
  | Opcode_low -> "Opcode_low"

let render_operand : Isa_x86_table.operand -> string = function
  | Reg { cls; field } ->
      Printf.sprintf "Reg { cls = %s; field = %s }" (render_class cls) (render_field field)
  | Mem { bits } -> Printf.sprintf "Mem { bits = %d }" bits
  | Imm { bytes } -> Printf.sprintf "Imm { bytes = %d }" bytes
  | Fixed_reg name -> Printf.sprintf "Fixed_reg %S" name
  | Rounding { sae_only } -> Printf.sprintf "Rounding { sae_only = %b }" sae_only
  | Vsib { cls } -> Printf.sprintf "Vsib { cls = %s }" (render_class cls)
  | One -> "One"
  | Dfv -> "Dfv"

let render_row (s : Isa_x86_table.spec) =
  Printf.sprintf
    "    {\n\
    \      mnemonic = %S;\n\
    \      space = %s;\n\
    \      map = %d;\n\
    \      opcode = 0x%02x;\n\
    \      prefix = 0x%02x;\n\
    \      osz = %b;\n\
    \      w = %d;\n\
    \      l = %d;\n\
    \      disp8n = %d;\n\
    \      digit = %d;\n\
    \      rm = %d;\n\
    \      operands = [ %s ];\n\
    \      mode = %d;\n\
    \      evex_p2 = 0x%02x;\n\
    \      bcst = %d;\n\
    \      bcst_elem = %d;\n\
    \      mask = %d;\n\
    \      pseudo = %S;\n\
    \      no_rex2 = %b;\n\
    \      no_acc = [ %s ];\n\
    \      feature = %S;\n\
    \      source = %S;\n\
    \    };\n"
    s.mnemonic
    (match s.space with `Vex -> "Vex" | `Evex -> "Evex" | `Xop -> "Xop" | `Legacy -> "Legacy")
    s.map s.opcode s.prefix s.osz s.w s.l s.disp8n s.digit s.rm
    (String.concat "; " (List.map render_operand s.operands))
    s.mode s.evex_p2 s.bcst s.bcst_elem s.mask s.pseudo s.no_rex2
    (String.concat "; " (List.map string_of_int s.no_acc))
    (String.lowercase_ascii s.isa_set)
    s.iform

(* {1 Row builders} One typed builder per operand shape of each spelling the hand-written forms do
   not own, for the rows a mode has; rows reached only through a pseudo-prefix get none. *)

let keywords =
  [
    "and";
    "as";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let ident m =
  let b = Buffer.create (String.length m) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' | '_' -> Buffer.add_char b c
      | 'A' .. 'Z' -> Buffer.add_char b (Char.lowercase_ascii c)
      | _ -> Buffer.add_char b '_')
    m;
  let s = Buffer.contents b in
  if List.mem s keywords then s ^ "_" else s

(* A positional argument: its type, the expression that makes it an operand of [x], its type as
   an expression builder ([_e]), and its letter in a shape suffix. *)
type arg = { ty : string; ty_e : string; conv : string -> string; tok : string; wtok : string }

let builder_operand (s : Isa_x86_table.spec) (o : Isa_x86_table.operand) =
  let gpr = function Isa_x86_table.Gpr8 | Gpr16 | Gpr32 | Gpr64 -> true | _ -> false in
  let upper cls field =
    (s.space = `Evex && (field = Isa_x86_table.Modrm_reg || field = Modrm_rm || field = Vvvv))
    || s.space = `Legacy && s.map <= 1 && gpr cls
       && (field = Isa_x86_table.Modrm_reg || field = Modrm_rm || field = Opcode_low)
  in
  let arg ?(ty_e = "") ty conv tok wtok =
    `Arg { ty; ty_e = (if ty_e = "" then ty else ty_e); conv; tok; wtok }
  in
  let call f x = Printf.sprintf "Rowkit.%s %s" f x in
  match o with
  | Reg { cls; field } -> (
      let range = if upper cls field then "[< `Lo | `Hi ]" else "[< `Lo ]" in
      let gty w = Printf.sprintf "(w%d, %s) greg" w range in
      let vty l = Printf.sprintf "(l%d, %s) vreg" l range in
      match cls with
      | Gpr8 -> arg (gty 8) (call "g") "r" "r8"
      | Gpr16 -> arg (gty 16) (call "g") "r" "r16"
      | Gpr32 -> arg (gty 32) (call "g") "r" "r32"
      | Gpr64 | Gprv -> arg (gty 64) (call "g") "r" "r64"
      | Xmm -> arg (vty 128) (call "v") "x" "x"
      | Ymm -> arg (vty 256) (call "v") "y" "y"
      | Zmm -> arg (vty 512) (call "v") "z" "z"
      | Mmx -> arg "mmx" (call "mm") "q" "q"
      | Kmask -> arg "kmask" (call "k") "k" "k"
      | St -> arg "st" (call "st") "t" "t"
      | Tmm -> arg "tmm" (call "tmm") "a" "a"
      | Cr -> arg "creg" (call "cr") "c" "c"
      | Dr -> arg "dreg" (call "dr") "d" "d")
  | Mem _ when s.bcst > 0 -> arg "Mem.t" (call (Printf.sprintf "b %d" s.bcst)) "b" "b"
  | Mem _ | Vsib _ -> arg "Mem.t" (call "m") "m" "m"
  | Imm { bytes } when bytes <= 2 ->
      arg ~ty_e:"Asm_core.Expr.t" "int" (call "i") "i" (Printf.sprintf "i%d" (8 * bytes))
  | Imm _ -> arg ~ty_e:"Asm_core.Expr.t" "int64" (call "i64") "i" "i32"
  | Fixed_reg name -> `Implicit (Printf.sprintf "Rowkit.fixed %S" name)
  | One -> `Implicit "Rowkit.one"
  | Dfv -> arg "int" (call "dfv") "f" "f"
  | Rounding _ -> arg "Evex.rounding" (call "rc") "e" "e"

(* The spellings and names of the hand-written builders, read off their definitions in X86_ops
   ([b ... "name" "spelling"], [%s] standing for a size letter): their rows get no builder. *)
let hand_spellings repo =
  let* text = Tool_fs.read Fpath.(Repo.path repo / "targets" / "x86_family" / "x86_ops.ml") in
  (* every quoted string, and whether only blanks separate it from the one before *)
  let rec strings i prev acc =
    match String.index_from_opt text i '"' with
    | None -> List.rev acc
    | Some a -> (
        match String.index_from_opt text (a + 1) '"' with
        | None -> List.rev acc
        | Some b ->
            let blank =
              prev >= 0
              && String.for_all (fun c -> c = ' ' || c = '\n') (String.sub text prev (a - prev))
            in
            strings (b + 1) (b + 1) ((String.sub text (a + 1) (b - a - 1), blank) :: acc))
  in
  let hand = Hashtbl.create 2048 in
  let add s = Hashtbl.replace hand s () in
  let rec pairs = function
    | (name, _) :: ((spelling, true) :: _ as rest) ->
        add name;
        add (match String.index_opt name '_' with Some k -> String.sub name 0 k | None -> name);
        (match String.index_opt spelling '%' with
        | Some k ->
            let stem = String.sub spelling 0 k in
            List.iter (fun c -> add (stem ^ c)) [ "b"; "w"; "l"; "q" ]
        | None -> add spelling);
        pairs rest
    | _ :: rest -> pairs rest
    | [] -> ()
  in
  pairs (strings 0 (-1) []);
  Ok hand

let render_builders ~hand ~bits (rows : Isa_x86_table.spec list) =
  let here (s : Isa_x86_table.spec) = (s.mode = 0 || s.mode = bits) && s.pseudo = "" in
  (* first row of each spelling and operand shape *)
  let shapes = Hashtbl.create 4096 and order = ref [] in
  List.iteri
    (fun i (s : Isa_x86_table.spec) ->
      if here s && not (Hashtbl.mem hand s.mnemonic) then
        let ops = List.map (builder_operand s) s.operands in
        (* the register range aside: a promoted form (EVEX, REX2) widens its twin's range, and
           the encoder tries every row of the spelling *)
        let key =
          ( s.mnemonic,
            s.mask,
            List.filter_map
              (function
                | `Arg a -> Some (a.wtok ^ if a.tok = "i" then a.ty else "")
                | `Implicit "Rowkit.one" -> None
                | `Implicit c -> Some c)
              ops,
            s.bcst )
        in
        match Hashtbl.find_opt shapes key with
        | None ->
            Hashtbl.replace shapes key (i, s, ops);
            order := key :: !order
        | Some (j, t, old) ->
            let wider o n =
              match (o, n) with
              | `Arg (a : arg), `Arg (b : arg) when String.length b.ty > String.length a.ty -> n
              | _ -> o
            in
            Hashtbl.replace shapes key (j, t, List.map2 wider old ops))
    rows;
  let keys = List.rev !order in
  let per_mnemonic = Hashtbl.create 2048 in
  List.iter
    (fun ((m, _, _, _) as k) ->
      Hashtbl.replace per_mnemonic m
        (k :: Option.value (Hashtbl.find_opt per_mnemonic m) ~default:[]))
    keys;
  let taken = Hashtbl.create 8192 in
  let args_of k =
    let _, _, ops = Hashtbl.find shapes k in
    List.filter_map (function `Arg a -> Some a | `Implicit _ -> None) ops
  in
  let tok f k = String.concat "" (List.map f (args_of k)) in
  (* shapes that differ only in their registers' widths all spell the widths *)
  let name_of ((m, _, _, _) as k) =
    let base = ident m in
    let siblings = Hashtbl.find per_mnemonic m in
    let short = tok (fun a -> a.tok) k in
    let clash = List.exists (fun k' -> k' <> k && tok (fun a -> a.tok) k' = short) siblings in
    let suffix = if clash then tok (fun a -> a.wtok) k else short in
    let cands =
      (if List.length siblings = 1 then [ base ] else [])
      @ [ base ^ "_" ^ suffix ]
      @ List.init 8 (fun j -> Printf.sprintf "%s_%s%d" base suffix (j + 2))
    in
    let n = List.find (fun c -> not (Hashtbl.mem taken c)) cands in
    Hashtbl.replace taken n ();
    n
  in
  let render k =
    let i, (s : Isa_x86_table.spec), ops = Hashtbl.find shapes k in
    let name = name_of k in
    let n = ref 0 in
    let named =
      List.map
        (function
          | `Arg a ->
              incr n;
              `Arg (a, Printf.sprintf "a%d" !n)
          | `Implicit c -> `Implicit c)
        ops
    in
    let last = List.length named - 1 in
    let one ~e =
      let params =
        List.filter_map
          (function
            | `Arg (a, x) -> Some (Printf.sprintf "(%s : %s)" x (if e then a.ty_e else a.ty))
            | `Implicit _ -> None)
          named
      in
      let mask_params, mask_args =
        match s.mask with
        | 1 -> ([ "?k"; "?z" ], "?k ?z")
        | 2 -> ([ "?k" ], "?k")
        | 3 -> ([ "~k" ], "~k")
        | _ -> ([], "")
      in
      let operand j = function
        | `Arg (a, x) ->
            let c = if e && a.tok = "i" then Printf.sprintf "Rowkit.e %s" x else a.conv x in
            if j = last && mask_args <> "" then Printf.sprintf "Rowkit.masked %s (%s)" mask_args c
            else c
        | `Implicit c ->
            if j = last && mask_args <> "" then Printf.sprintf "Rowkit.masked %s (%s)" mask_args c
            else c
      in
      let body =
        Printf.sprintf "Rowkit.row %d [ %s ]" i (String.concat "; " (List.mapi operand named))
      in
      let lhs = String.concat " " ((if e then name ^ "_e" else name) :: (mask_params @ params)) in
      Printf.sprintf "let %s = %s\n" lhs body
    in
    let has_imm =
      List.exists (function `Arg (a, _) -> a.tok = "i" | `Implicit _ -> false) named
    in
    one ~e:false ^ if has_imm then one ~e:true else ""
  in
  let body = List.map render keys in
  ( Hashtbl.length per_mnemonic,
    List.length keys,
    String.concat ""
      ([
         Printf.sprintf
           "(* Generated by [rivet_tools isa-table x86-emit] from the committed XED exports; do not\n\
           \   edit by hand. A typed builder per operand shape of each generated row the x86_%d mode\n\
           \   has whose spelling the hand-written forms do not own, named by the spelling and, \
            where it\n\
           \   has several shapes, the shape's operand letters. *)\n\n\
            [@@@ocaml.warning \"-32\"]\n\n\
            open X86_family_encode\n\
            open X86_asm\n\n"
           bits;
       ]
      @ body) )

let emit repo =
  let* x32 = all_specs repo Target.X86_32 in
  let* x64 = all_specs repo Target.X86_64 in
  let read target =
    Isa_source_record.read_file (Repo.isa_db_export repo ~source:"xed_resolved" target)
  in
  let* records32 = read Target.X86_32 in
  let* records64 = read Target.X86_64 in
  let guard records specs =
    List.map
      (fun (s : Isa_x86_table.spec) ->
        {
          s with
          no_acc = List.sort_uniq compare (s.no_acc @ Isa_x86_table.accumulator_positions records s);
        })
      specs
  in
  let x32 = List.concat_map Isa_x86_table.expand x32 |> guard records32 in
  let x64 = List.concat_map Isa_x86_table.expand x64 |> guard records64 in
  (* xchg is symmetric: GNU accepts the accumulator on either side of the short form *)
  let swapped specs =
    List.concat_map
      (fun (s : Isa_x86_table.spec) ->
        if
          String.length s.iform > 4
          && String.sub s.iform 0 4 = "XCHG"
          && List.exists (function Isa_x86_table.Fixed_reg _ -> true | _ -> false) s.operands
        then [ s; { s with operands = List.rev s.operands } ]
        else [ s ])
      specs
  in
  let x32 = swapped x32 and x64 = swapped x64 in
  (* a form in both exports serves both modes; one only in the 64-bit export is 64-bit only *)
  let rows =
    List.map
      (fun (s : Isa_x86_table.spec) ->
        if
          List.exists
            (fun (t : Isa_x86_table.spec) ->
              t.iform = s.iform && t.opcode = s.opcode && t.mnemonic = s.mnemonic)
            x32
        then s
        else { s with mode = 64 })
      x64
    (* a form only the 32-bit export has (MODE!=2: the short inc/dec) *)
    @ List.filter (fun (s : Isa_x86_table.spec) -> s.mode = 32) x32
    |> List.fold_left
         (fun acc (s : Isa_x86_table.spec) ->
           if
             List.exists
               (fun (t : Isa_x86_table.spec) ->
                 { t with record_id = ""; iform = "" } = { s with record_id = ""; iform = "" })
               acc
           then acc
           else s :: acc)
         []
    |> List.rev
    (* decode tries rows in order: a row with more fixed fields first *)
    |> List.stable_sort (fun (a : Isa_x86_table.spec) (b : Isa_x86_table.spec) ->
        let fixed (s : Isa_x86_table.spec) =
          (if s.digit >= 0 then 1 else 0) + (if s.w >= 0 then 1 else 0) + if s.l >= 0 then 1 else 0
        in
        compare (fixed b) (fixed a))
  in
  (* a twin's row goes after every reachable row, so the encoder reaches the primary *)
  let secondary = Isa_x86_table.twins x64 and secondary32 = Isa_x86_table.twins x32 in
  let is_secondary (s : Isa_x86_table.spec) =
    Hashtbl.mem secondary s.record_id || Hashtbl.mem secondary32 s.record_id
  in
  (* and in preference order, so a pseudo-prefix reaches the form GNU as picks *)
  let rank = Isa_x86_table.twin_rank x64 and rank32 = Isa_x86_table.twin_rank x32 in
  let rank_of (s : Isa_x86_table.spec) =
    match Hashtbl.find_opt rank s.record_id with
    | Some i -> i
    | None -> Option.value (Hashtbl.find_opt rank32 s.record_id) ~default:0
  in
  let rows =
    (* a $1 shift takes the D0/D1 form wherever the imm8 one would also fit *)
    let one_first rows =
      let one (s : Isa_x86_table.spec) =
        List.exists (function Isa_x86_table.One -> true | _ -> false) s.operands
      in
      List.filter one rows @ List.filter (fun s -> not (one s)) rows
    in
    one_first (List.filter (fun s -> not (is_secondary s)) rows)
    @ one_first
        (List.stable_sort
           (fun a b -> compare (rank_of a) (rank_of b))
           (List.filter is_secondary rows))
  in
  let text =
    String.concat ""
      ([
         "(* Generated by [rivet_tools isa-table x86-emit] from the committed XED exports\n\
         \   (isa-db/export/xed_resolved); do not edit by hand. The selection and operand rules are\n\
         \   Isa_x86_table's DEC-X86-TABLE rule. *)\n\n\
          open X86_table_row\n\n\
          let rows : row array =\n\
         \  [|\n";
       ]
      @ List.map render_row rows @ [ "  |]\n" ])
  in
  let* hand = hand_spellings repo in
  let _, _, b32 = render_builders ~hand ~bits:32 rows in
  let _, _, b64 = render_builders ~hand ~bits:64 rows in
  let files =
    [ (rows_path repo, text); (builders_path repo 32, b32); (builders_path repo 64, b64) ]
  in
  let* () = Emit_guard.check_files files in
  Ok files

let fatal op detail =
  Command.of_error (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail))

let run_emit repo =
  match
    let* files = emit repo in
    List.fold_left
      (fun acc (path, text) ->
        let* () = acc in
        Tool_fs.write path text)
      (Ok ()) files
    |> Result.map (fun () -> files)
  with
  | Ok files ->
      Command.ok
        (List.map
           (fun (path, _) -> Diagnostic.stdout ("isa-table: wrote " ^ Fpath.to_string path))
           files)
  | Error e -> Command.of_error e

let run_check repo =
  match
    let* files = emit repo in
    List.fold_left
      (fun acc (path, text) ->
        let* ok = acc in
        let* committed = Tool_fs.read path in
        Ok (ok && String.equal text committed))
      (Ok true) files
  with
  | Ok true ->
      Command.ok
        [ Diagnostic.stdout "isa-table: x86_table_rows.ml and x86_asm_rows{32,64}.ml are current" ]
  | Ok false ->
      fatal Tool_error.Validate
        "isa-table: x86_table_rows.ml or x86_asm_rows{32,64}.ml differ from a fresh emission - run \
         isa-table x86-emit"
  | Error e -> Command.of_error e
