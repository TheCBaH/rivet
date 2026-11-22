(* Prints a module and disassembles bytes for x86-64, AArch64 and RV32 through Portable. *)
let print instruction type_char =
  print_string
    (Rivet_asm_core.Gnu_module.to_string
       { Rivet_asm_core.Gnu_module.type_char }
       ~instruction
       { Rivet_asm_core.Normalized_ast.unit_name = "m"; items = [] })

let () =
  print X86_64_encode.Instruction.pp_gnu '@';
  print Aarch64_encode.Instruction.pp_gnu '%';
  print Riscv32_encode.Instruction.pp_gnu '@';
  List.iter
    (fun t ->
      match Driver.Portable.disassemble_records_for t ~address:0L "\x13\x05\x15\x00" with
      | Ok rs ->
          List.iter (fun (r : Rivet_target_intf.Target.disasm_record) -> print_endline r.text) rs
      | Error _ -> ())
    [ "x86_64"; "aarch64"; "riscv32" ]
