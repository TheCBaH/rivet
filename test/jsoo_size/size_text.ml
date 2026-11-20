(* The same, through the text pipeline applied to the three targets. *)
let print instruction type_char =
  print_string
    (Asm_core.Gnu_module.to_string { Asm_core.Gnu_module.type_char } ~instruction
       { Asm_core.Normalized_ast.unit_name = "m"; items = [] })

module X = Pipeline.Make (X86_64)
module A = Pipeline.Make (Aarch64)
module R = Pipeline.Make (Riscv32)

let show = function
  | Ok rs -> List.iter (fun (r : Target_encode.disasm_record) -> print_endline r.text) rs
  | Error _ -> ()

let () =
  print X86_64.Instruction.pp_gnu '@';
  print Aarch64.Instruction.pp_gnu '%';
  print Riscv32.Instruction.pp_gnu '@';
  let b = "\x13\x05\x15\x00" in
  show (X.disassemble_records ~address:0L b);
  show (A.disassemble_records ~address:0L b);
  show (R.disassemble_records ~address:0L b)
