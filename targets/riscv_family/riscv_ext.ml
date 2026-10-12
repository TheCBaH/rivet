(* Construction shared by the RISC-V extension components. A component owns the hand-written
   forms its module lists and every generated table row whose feature is its own. *)

let table_forms feature =
  Array.fold_left
    (fun acc (r : Riscv_table_row.row) ->
      if String.equal r.feature feature && not (List.mem_assoc r.mnemonic acc) then
        (r.mnemonic, r.source) :: acc
      else acc)
    [] Riscv_table_rows.rows
  |> List.rev

let component ~id ~feature ?(requires = []) ?(implies = []) ?(default_on = true)
    ?(legal = fun _ -> None) ~summary handwritten =
  let forms =
    handwritten
    @ List.filter (fun (m, _) -> not (List.mem_assoc m handwritten)) (table_forms feature)
  in
  {
    Target_component.id;
    feature;
    requires;
    conflicts = [];
    implies = List.map (fun f -> (Target_component.Always, f)) implies;
    default_on;
    xlens = [];
    not_applicable = None;
    summary;
    forms =
      List.map
        (fun (mnemonic, name) ->
          {
            Target_component.label = mnemonic;
            mnemonics = [ mnemonic ];
            sources = [ { Target_component.upstream = "riscv-opcodes"; name } ];
            legal = legal mnemonic;
          })
        forms;
  }
