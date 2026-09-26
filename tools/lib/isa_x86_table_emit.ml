let ( let* ) = Result.bind
let rows_path repo = Fpath.(Repo.path repo / "asm" / "targets" / "x86_family" / "x86_table_rows.ml")

let specs repo target =
  let* records =
    Isa_source_record.read_file (Repo.isa_db_export repo ~source:"xed_resolved" target)
  in
  Ok
    (List.filter_map
       (fun rec_ ->
         match Isa_norm_xed.normalize_hand_written rec_ with
         | Error { Isa_norm_model.rule = "unhandled-iform"; _ } -> Isa_x86_table.spec_of_record rec_
         | _ -> None)
       records)

(* Every table-expressible record of one export, including those the hand-written rules own:
   twins are decided over all of them, and hand-written forms get rows too, used only when the
   hand-written encoder declines a shape (a high VEX register). *)
let all_specs repo target =
  let* records =
    Isa_source_record.read_file (Repo.isa_db_export repo ~source:"xed_resolved" target)
  in
  Ok (List.filter_map Isa_x86_table.spec_of_record records)

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
    \      operands = [ %s ];\n\
    \      mode = %d;\n\
    \      no_acc = [ %s ];\n\
    \      feature = %S;\n\
    \      source = %S;\n\
    \    };\n"
    s.mnemonic
    (match s.space with `Vex -> "Vex" | `Evex -> "Evex" | `Legacy -> "Legacy")
    s.map s.opcode s.prefix s.osz s.w s.l s.disp8n s.digit
    (String.concat "; " (List.map render_operand s.operands))
    s.mode
    (String.concat "; " (List.map string_of_int s.no_acc))
    (String.lowercase_ascii s.isa_set)
    s.iform

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
  let rows = List.filter (fun s -> not (is_secondary s)) rows @ List.filter is_secondary rows in
  Ok
    (String.concat ""
       ([
          "(* Generated by [compcert_tools isa-table x86-emit] from the committed XED exports\n\
          \   (isa-db/export/xed_resolved); do not edit by hand. The selection and operand rules are\n\
          \   Isa_x86_table's DEC-X86-TABLE rule. *)\n\n\
           open X86_table_row\n\n\
           let rows : row array =\n\
          \  [|\n";
        ]
       @ List.map render_row rows @ [ "  |]\n" ]))

let fatal op detail =
  Command.of_error (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail))

let run_emit repo =
  match
    let* text = emit repo in
    Tool_fs.write (rows_path repo) text
  with
  | Ok () ->
      Command.ok [ Diagnostic.stdout ("isa-table: wrote " ^ Fpath.to_string (rows_path repo)) ]
  | Error e -> Command.of_error e

let run_check repo =
  match
    let* text = emit repo in
    let* committed = Tool_fs.read (rows_path repo) in
    Ok (String.equal text committed)
  with
  | Ok true -> Command.ok [ Diagnostic.stdout "isa-table: x86_table_rows.ml is current" ]
  | Ok false ->
      fatal Tool_error.Validate
        "isa-table: x86_table_rows.ml differs from a fresh emission - run isa-table x86-emit"
  | Error e -> Command.of_error e
