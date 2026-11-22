(* Assemble a normalized module built as values, through the direct stages over
   the AArch64 encoder alone: [mov x0, #42; ret] is two words. *)

open Rivet_foundation
open Rivet_asm_core
module A = Aarch64_encode
module P = Rivet_driver_direct.Pipeline_direct.Make (A)

let origin = Origin.synthesized ~pass:"direct" ()

let instruction op ops =
  match
    A.simplify_instruction A.default_state
      (Result.get_ok (A.make_surface_instruction ~mnemonic:(A.Opcode.name op) ~origin ops))
  with
  | Ok i -> i
  | Error _ -> failwith "simplify"

let () =
  let m =
    {
      Normalized_ast.unit_name = "direct";
      items =
        [
          Normalized_ast.Directive
            {
              directive = Directive.Section { name = ".text"; perms = Perms.rx; nobits = false };
              origin;
            };
          Normalized_ast.Directive { directive = Directive.Global { name = "f" }; origin };
          Normalized_ast.Label { name = "f"; origin };
          Normalized_ast.Instruction
            {
              insn =
                instruction A.Opcode.Movz
                  [
                    A.Operand.Reg { A.Reg.num = 0; width = 64; is_sp = false };
                    A.Operand.Imm (Bigint.of_int 42);
                  ];
              origin;
            };
          Normalized_ast.Instruction { insn = instruction A.Opcode.Ret []; origin };
        ];
    }
  in
  match P.lower ~state:A.default_state m with
  | Error _ -> print_endline "lower failed"
  | Ok lowered -> (
      match P.plan ~entry:"f" lowered with
      | Error _ -> print_endline "plan failed"
      | Ok laid -> (
          let addresses =
            List.map
              (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0x1000L))
              (Rivet_image.plan_of laid).Rivet_image.segments
          in
          match Rivet_image.bind_image laid ~addresses with
          | Error _ -> print_endline "bind failed"
          | Ok image ->
              List.iter
                (fun (s : Rivet_image.segment) ->
                  Printf.printf "%Lx:%s\n" s.Rivet_image.address
                    (String.concat ""
                       (List.init (String.length s.Rivet_image.bytes) (fun i ->
                            Printf.sprintf "%02x" (Char.code s.Rivet_image.bytes.[i])))))
                image.Rivet_image.segments))
