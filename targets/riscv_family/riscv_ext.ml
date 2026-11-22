(* Construction shared by the RISC-V extension components. A component owns the hand-written
   forms its module lists and every generated table row whose feature is its own. *)

let table_forms rows =
  Array.fold_left
    (fun acc (r : Riscv_table_row.row) ->
      if rows r && not (List.mem_assoc r.mnemonic acc) then (r.mnemonic, r.source) :: acc else acc)
    [] Riscv_table_rows.rows
  |> List.rev

let component ~id ~feature ?(requires = []) ?(conflicts = []) ?(implies = []) ?(conditional = [])
    ?(default_on = true) ?(xlens = []) ?rows ?(legal = fun _ -> None) ~summary handwritten =
  let rows =
    Option.value rows ~default:(fun (r : Riscv_table_row.row) -> String.equal r.feature feature)
  in
  let handwritten =
    List.map (fun (op, name) -> (Riscv_types.Opcode.mnemonic op, name)) handwritten
  in
  let forms =
    handwritten @ List.filter (fun (m, _) -> not (List.mem_assoc m handwritten)) (table_forms rows)
  in
  {
    Rivet_target_features.Component.id;
    feature;
    requires;
    conflicts;
    implies = List.map (fun f -> (Rivet_target_features.Component.Always, f)) implies @ conditional;
    default_on;
    xlens;
    not_applicable = None;
    summary;
    forms =
      List.map
        (fun (mnemonic, name) ->
          {
            Rivet_target_features.Component.label = mnemonic;
            mnemonics = [ mnemonic ];
            sources = [ { Rivet_target_features.Component.upstream = "riscv-opcodes"; name } ];
            legal = legal mnemonic;
          })
        forms;
  }
