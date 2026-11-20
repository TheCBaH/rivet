(* The path-coherence assertions over Test_coherence's fixtures. Kept out of
   that library so rivet.test_support carries no ppx and no inline tests. *)

open Test_coherence
open Asm_core

let%expect_test "simplify of the parsed text equals the direct normalized AST" =
  let parsed, _ = from_text () in
  let a = render_normalized parsed and b = render_normalized direct_normalized in
  if String.equal a b then print_endline "normalized ASTs are identical"
  else Printf.printf "NORMALIZED ASTS DIFFER\n--- from text\n%s\n--- direct\n%s\n" a b;
  print_endline b;
  [%expect
    {|
    normalized ASTs are identical
    normalized asm_test_entry
    section .text r-x
    align 4
    globl asm_test_entry
    label asm_test_entry
    mov x15, sp
    stp x15, x30, [sp, #-16]!
    movz w0, #42
    ldr x30, [sp, #8]
    add sp, sp, #16
    ret x30
    type asm_test_entry function
    size asm_test_entry (. - asm_test_entry)
    declared-section .note.GNU-stack
    |}]

let%expect_test "lowering the direct normalized AST equals the direct lowered module" =
  match P.lower ~state:T.default_state direct_normalized with
  | Error ds -> print_endline (Foundation.Diag.render ds)
  | Ok low ->
      let a = render_lowered low and b = render_lowered direct_lowered in
      if String.equal a b then print_endline "lowered modules are identical"
      else Printf.printf "LOWERED MODULES DIFFER\n--- lowered\n%s\n--- direct\n%s\n" a b;
      print_endline b;
      [%expect
        {|
        lowered modules are identical
        lowered asm_test_entry
        section .text r-x align=4
          align 4
          label asm_test_entry
          bytes ef 03 00 91              [aarch64.add-imm]
          bytes ef 7b bf a9              [aarch64.stp-pre]
          bytes 40 05 80 52              [aarch64.movz]
          bytes fe 07 40 f9              [aarch64.ldr64]
          bytes ff 43 00 91              [aarch64.add-imm]
          bytes c0 03 5f d6              [aarch64.ret]
          size asm_test_entry = (. - asm_test_entry)
        global asm_test_entry function in .text size=(. - asm_test_entry)
        declared .note.GNU-stack (not allocated)
        |}]

(* {1 One image from three entry points} *)

let image_of low =
  match P.plan low with
  | Error ds -> failwith (Foundation.Diag.render ds)
  | Ok l -> (
      let plan = Image.plan_of l in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
      in
      match Image.bind_image l ~addresses with
      | Error ds -> failwith (Foundation.Diag.render ds)
      | Ok img -> Fmt.to_to_string Image.pp img)

let%expect_test "text, direct normalized AST and direct lowered module give one image" =
  let parsed, state = from_text () in
  let from_text_image =
    match P.lower ~state parsed with
    | Error ds -> failwith (Foundation.Diag.render ds)
    | Ok low -> image_of low
  in
  let from_normalized =
    match P.lower ~state:T.default_state direct_normalized with
    | Error ds -> failwith (Foundation.Diag.render ds)
    | Ok low -> image_of low
  in
  let from_lowered = image_of direct_lowered in
  if String.equal from_text_image from_normalized && String.equal from_normalized from_lowered then
    print_endline "all three paths agree:"
  else print_endline "PATHS DISAGREE:";
  print_endline from_text_image;
  [%expect
    {|
    all three paths agree:
    section .text address=0x0 size=24 permissions=r-x
    entry 0x0
    export asm_test_entry = 0x0 size=24
    |}]

(* {1 Malformed direct ASTs fail at their own validator}

   A direct producer gets the same checks the text path gets, and gets them at
   the stage that owns them - not at the end, where the diagnostic could only
   say "the image is wrong". *)

let%expect_test "a direct normalized AST with an out-of-range immediate is rejected at lowering" =
  let bad : T.Instruction.t Normalized_ast.module_ =
    {
      Normalized_ast.unit_name = "bad";
      items =
        [
          Normalized_ast.Directive
            {
              directive = Directive.Section { name = ".text"; perms = Perms.rx; nobits = false };
              origin;
            };
          Normalized_ast.Instruction
            {
              insn =
                {
                  T.Instruction.op = T.Opcode.Movz;
                  ops = [ T.Operand.Reg (x "w0"); T.Operand.Imm (Foundation.Bigint.of_int 70000) ];
                };
              origin;
            };
        ];
    }
  in
  (match P.lower ~state:T.default_state bad with
  | Ok _ -> print_endline "accepted an out-of-range immediate"
  | Error ds ->
      List.iter
        (fun d -> print_endline (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds));
  [%expect {| movz immediate does not fit 16 bits |}]

let%expect_test "a direct normalized AST with no section is rejected at lowering" =
  let bad : T.Instruction.t Normalized_ast.module_ =
    {
      Normalized_ast.unit_name = "bad";
      items =
        [
          Normalized_ast.Instruction
            { insn = { T.Instruction.op = T.Opcode.Ret; ops = [] }; origin };
        ];
    }
  in
  (match P.lower ~state:T.default_state bad with
  | Ok _ -> print_endline "accepted a fragment with nowhere to go"
  | Error ds ->
      List.iter
        (fun d -> print_endline (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds));
  [%expect {| this appears before any section directive |}]

let%expect_test "a direct lowered module whose exported symbol is undefined is rejected at planning"
    =
  let bad : T.fixup_kind Lowered_ast.module_ =
    {
      direct_lowered with
      Lowered_ast.symbols =
        [
          {
            Lowered_ast.name = "absent";
            binding = Lowered_ast.Global;
            visibility = Lowered_ast.Default;
            kind = Directive.Function;
            size = None;
            section = ".text";
          };
        ];
    }
  in
  (match P.plan bad with
  | Ok _ -> print_endline "accepted an undefined export"
  | Error ds ->
      List.iter
        (fun d -> print_endline (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds));
  [%expect
    {|
    symbol absent is declared but never defined
    exported symbol absent is not defined
    entry symbol absent is not defined
    |}]
