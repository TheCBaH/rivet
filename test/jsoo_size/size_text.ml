(* The same, through the text pipeline applied to the three targets. *)
let print instruction type_char =
  print_string
    (Rivet_asm_core.Gnu_module.to_string
       { Rivet_asm_core.Gnu_module.type_char }
       ~instruction
       { Rivet_asm_core.Normalized_ast.unit_name = "m"; items = [] })

module X = Rivet_text_pipeline.Pipeline.Make (Rivet_x86_64)
module A = Rivet_text_pipeline.Pipeline.Make (Rivet_aarch64)
module R = Rivet_text_pipeline.Pipeline.Make (Rivet_riscv32)

let show = function
  | Ok rs -> List.iter (fun (r : Rivet_target_encode.disasm_record) -> print_endline r.text) rs
  | Error _ -> ()

let () =
  print Rivet_x86_64.Instruction.pp_gnu '@';
  print Rivet_aarch64.Instruction.pp_gnu '%';
  print Rivet_riscv32.Instruction.pp_gnu '@';
  let b = "\x13\x05\x15\x00" in
  show (X.disassemble_records ~address:0L b);
  show (A.disassemble_records ~address:0L b);
  show (R.disassemble_records ~address:0L b)
