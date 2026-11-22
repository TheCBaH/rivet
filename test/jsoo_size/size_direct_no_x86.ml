(* The direct pipeline over AArch64 and RV32 only. *)
let print instruction type_char =
  print_string
    (Rivet_asm_core.Gnu_module.to_string
       { Rivet_asm_core.Gnu_module.type_char }
       ~instruction
       { Rivet_asm_core.Normalized_ast.unit_name = "m"; items = [] })

module A = Rivet_driver_direct.Pipeline_direct.Make (Rivet_aarch64_encode)
module R = Rivet_driver_direct.Pipeline_direct.Make (Riscv32_encode)

let show = function
  | Ok rs -> List.iter (fun (r : Rivet_target_encode.disasm_record) -> print_endline r.text) rs
  | Error _ -> ()

let () =
  print Rivet_aarch64_encode.Instruction.pp_gnu '%';
  print Riscv32_encode.Instruction.pp_gnu '@';
  let b = "\x13\x05\x15\x00" in
  show (A.disassemble_records ~address:0L b);
  show (R.disassemble_records ~address:0L b)
