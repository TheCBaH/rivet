let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

type entry = { mnemonic : string; extension : string }

let extensions_dir repo = Fpath.(Repo.isa_data_riscv_opcodes repo / "extensions")
let is_dir p = try Sys.is_directory (Fpath.to_string p) with Sys_error _ -> false

let list_extension_files repo =
  let dir = extensions_dir repo in
  match Sys.readdir (Fpath.to_string dir) with
  | exception Sys_error msg -> err ~path:dir Tool_error.Read_file msg
  | names ->
      Array.to_list names
      |> List.filter (fun name -> name <> "unratified" && not (is_dir Fpath.(dir / name)))
      |> List.sort String.compare |> Result.ok

(* riscv-opcodes' own tables use runs of spaces to align columns, not tabs, but
   both are tolerated - only the split matters, not which whitespace produced
   it. *)
let tokens line =
  String.split_on_char ' ' line
  |> List.concat_map (String.split_on_char '\t')
  |> List.filter (fun s -> s <> "")

let parse_line ~name ~line_no line =
  let trimmed = String.trim line in
  if trimmed = "" || trimmed.[0] = '#' then Ok None
  else
    match tokens trimmed with
    | [] -> Ok None
    (* A composite extension re-exporting another file's own mnemonic (e.g.
       rv32_zk importing rv64_zbb::rolw) - not a new definition, so recording
       it here would double-count the mnemonic under two extensions. See the
       .mli. *)
    | "$import" :: _ -> Ok None
    | "$pseudo_op" :: _ :: pseudo :: _ -> Ok (Some { mnemonic = pseudo; extension = name })
    | "$pseudo_op" :: _ ->
        err Tool_error.Parse
          (Printf.sprintf "%s:%d: malformed $pseudo_op line: %S" name line_no line)
    | mnemonic :: _ -> Ok (Some { mnemonic; extension = name })

let parse_extension_file ~name text =
  let lines = String.split_on_char '\n' text in
  let* _, entries =
    List.fold_left
      (fun acc line ->
        let* line_no, entries = acc in
        let* entry = parse_line ~name ~line_no line in
        Ok (line_no + 1, match entry with None -> entries | Some e -> e :: entries))
      (Ok (1, []))
      lines
  in
  Ok (List.rev entries)

let has_prefix prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let applies_to (target : Target.t) ~extension =
  match target with
  | Target.Riscv32 -> has_prefix "rv_" extension || has_prefix "rv32_" extension
  | Target.Riscv64 -> has_prefix "rv_" extension || has_prefix "rv64_" extension
  | Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64 -> false

let compare_entry a b =
  match String.compare a.mnemonic b.mnemonic with
  | 0 -> String.compare a.extension b.extension
  | c -> c

let entries_for_target repo target =
  let* files = list_extension_files repo in
  let applicable = List.filter (fun name -> applies_to target ~extension:name) files in
  let* entries =
    List.fold_left
      (fun acc name ->
        let* entries = acc in
        let* text = Tool_fs.read Fpath.(extensions_dir repo / name) in
        let* file_entries = parse_extension_file ~name text in
        Ok (List.rev_append file_entries entries))
      (Ok []) applicable
  in
  (* sort_uniq, not a duplicate-rejecting sort: the source table legitimately
     spells the same mnemonic more than once within one file for distinct
     operand-count forms (e.g. rv_i's two-operand `jal rd, offset` and its own
     one-operand `$pseudo_op rv_i::jal jal offset` alias for `rd=x1`) - real
     evidence that a form exists, not a data-integrity bug. Phase A's
     (mnemonic, extension) granularity does not distinguish operand forms (see
     the .mli), so both collapse to one record here rather than being
     rejected. *)
  Ok (List.sort_uniq compare_entry entries)

let render_manifest ~source_commit (target : Target.t) entries =
  let b = Buffer.create 4096 in
  Buffer.add_string b "schema-version:1\n";
  Buffer.add_string b (Printf.sprintf "target:%s\n" (Target.to_string target));
  Buffer.add_string b "source:riscv-opcodes\n";
  Buffer.add_string b (Printf.sprintf "source-commit:%s\n" source_commit);
  Buffer.add_string b "source-license:BSD-3-Clause\n";
  List.iter
    (fun e ->
      Buffer.add_string b
        (Printf.sprintf
           "mnemonic:%s\textension:%s\tcompcert-emitted:unknown\tgcc-emitted:unknown\tstate:unknown\tdeferral-reason:-\tpromotion-trigger:-\n"
           (Manifest.escape e.mnemonic) (Manifest.escape e.extension)))
    entries;
  Buffer.contents b

let render_summary (target : Target.t) entries =
  let b = Buffer.create 1024 in
  Buffer.add_string b (Printf.sprintf "target:%s\n" (Target.to_string target));
  Buffer.add_string b (Printf.sprintf "total:%d\n" (List.length entries));
  let by_extension = Hashtbl.create 64 in
  List.iter
    (fun e ->
      Hashtbl.replace by_extension e.extension
        (1 + Option.value (Hashtbl.find_opt by_extension e.extension) ~default:0))
    entries;
  Hashtbl.to_seq by_extension |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (ext, n) -> Buffer.add_string b (Printf.sprintf "by-extension:%s:%d\n" ext n));
  Buffer.add_string b (Printf.sprintf "by-state:unknown:%d\n" (List.length entries));
  Buffer.add_string b "compcert-emitted:0\n";
  Buffer.add_string b "gcc-emitted:0\n";
  Buffer.contents b
