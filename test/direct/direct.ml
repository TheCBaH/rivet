(* Assemble a normalized module built as values, through the direct stages over
   the AArch64 encoder alone: [mov x0, #42; ret] is two words. *)

open Foundation
open Asm_core
module A = Aarch64_encode
module P = Driver_direct.Pipeline_direct.Make (A)

let origin = Origin.synthesized ~pass:"direct" ()

let instruction op ops =
  match
    A.simplify_instruction A.default_state
      (Result.get_ok
         (A.make_surface_instruction ~mnemonic:(A.Opcode.name op) ~origin ops))
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
              directive =
                Directive.Section
                  { name = ".text"; perms = Perms.rx; nobits = false };
              origin;
            };
          Normalized_ast.Directive
            { directive = Directive.Global { name = "f" }; origin };
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
          Normalized_ast.Instruction
            { insn = instruction A.Opcode.Ret []; origin };
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
              (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x1000L))
              (Image.plan_of laid).Image.segments
          in
          match Image.bind_image laid ~addresses with
          | Error _ -> print_endline "bind failed"
          | Ok image ->
              List.iter
                (fun (s : Image.segment) ->
                  Printf.printf "%Lx:%s\n" s.Image.address
                    (String.concat ""
                       (List.init (String.length s.Image.bytes) (fun i ->
                            Printf.sprintf "%02x" (Char.code s.Image.bytes.[i])))))
                image.Image.segments))
