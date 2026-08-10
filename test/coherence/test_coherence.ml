(* Path coherence (.ai/asm_plan.md M1.7).

   §4's claim is that each staged AST is a *public entry point*, not an internal
   step: a compiler that already has semantic instructions should hand over a
   normalized AST rather than print assembly for the parser to read back. That
   claim is only worth anything if the two paths meet, so this file builds the
   normalized and lowered modules for the aarch64 fixture directly, in OCaml,
   and asserts:

     simplify (parse text)  =  the direct normalized AST
     lower (that)           =  the direct lowered module
     and all three paths produce the same image bytes.

   These use the *statically selected* target module - [Driver.Registry.Aarch64]
   is [Pipeline.Make (Aarch64)], not the erased [DRIVER] - so every type below
   is Aarch64's own. That is what makes the cross-target guarantee a compile-time
   one: [P.lower] takes an [Aarch64.instruction Normalized_ast.module_], so

     P.lower ~state:Aarch64.default_state some_arm_normalized_ast

   does not typecheck, and no runtime check is needed or possible. There is
   deliberately no test asserting it, because a test that passed would only mean
   the assertion compiled - the guarantee is that this file compiles at all. *)

module P = Driver.Registry.Aarch64
module T = Aarch64
open Asm_core

let origin = Foundation.Origin.synthesized ~pass:"direct" ()

(* Through the parser's own name table, so this file and the text path resolve
   [sp] the same way. A typo cannot reach an encoding - it fails here, at module
   initialization, where nothing has printed a test name yet and the message is
   the only diagnosis available. *)
let x n =
  match T.Reg.find n with Some r -> r | None -> Fmt.failwith "%s: unknown register %S" T.name n

let nop length =
  match T.nop_bytes ~length with Ok b -> b | Error d -> failwith (Foundation.Diagnostic.message d)

(* {1 The direct normalized AST}

   Written as a compiler would produce it: directives are values, the aliases
   are already resolved ([mov x15, sp] is [Opcode.Mov] with two registers, which
   lowering turns into [add ..., #0]), and nothing here mentions a token, a span
   or a spelling. *)
let direct_normalized : T.Instruction.t Normalized_ast.module_ =
  let insn i = Normalized_ast.Instruction { insn = i; origin } in
  let dir d = Normalized_ast.Directive { directive = d; origin } in
  {
    Normalized_ast.unit_name = "asm_test_entry";
    items =
      [
        dir (Directive.Section { name = ".text"; perms = Perms.rx });
        dir (Directive.Align { boundary = 4 });
        dir (Directive.Global { name = "asm_test_entry" });
        Normalized_ast.Label { name = "asm_test_entry"; origin };
        insn
          {
            T.Instruction.op = T.Opcode.Mov;
            ops = [ T.Operand.Reg (x "x15"); T.Operand.Reg (x "sp") ];
          };
        insn
          {
            T.Instruction.op = T.Opcode.Stp;
            ops =
              [
                T.Operand.Reg (x "x15");
                T.Operand.Reg (x "x30");
                T.Operand.Mem { T.Mem.base = x "sp"; offset = -16L; writeback = true; pre = true };
              ];
          };
        insn
          {
            T.Instruction.op = T.Opcode.Movz;
            ops = [ T.Operand.Reg (x "w0"); T.Operand.Imm (Foundation.Bigint.of_int 42) ];
          };
        insn
          {
            T.Instruction.op = T.Opcode.Ldr;
            ops =
              [
                T.Operand.Reg (x "x30");
                T.Operand.Mem { T.Mem.base = x "sp"; offset = 8L; writeback = false; pre = true };
              ];
          };
        insn
          {
            T.Instruction.op = T.Opcode.Add;
            ops =
              [
                T.Operand.Reg (x "sp");
                T.Operand.Reg (x "sp");
                T.Operand.Imm (Foundation.Bigint.of_int 16);
              ];
          };
        insn { T.Instruction.op = T.Opcode.Ret; ops = [ T.Operand.Reg (x "x30") ] };
        dir (Directive.Sym_type { name = "asm_test_entry"; kind = Directive.Function });
        dir
          (Directive.Sym_size
             {
               name = "asm_test_entry";
               size = Expr.Binary (Expr.Sub, Expr.Current_location, Expr.Symbol "asm_test_entry");
             });
        dir (Directive.Declared_section { name = ".note.GNU-stack" });
      ];
  }

let text = Test_dump_inputs.aarch64

let from_text () =
  let source = Foundation.Span.source ~name:"aarch64.s" ~contents:text in
  match P.parse ~unit_name:"asm_test_entry" ~source with
  | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
  | Ok src -> (
      match P.simplify src with
      | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
      | Ok (n, state) -> (n, state))

let render_normalized = Fmt.to_to_string (Normalized_ast.pp T.Instruction.pp)

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

(* {1 The direct lowered module}

   The stage below, written directly. It carries bytes and a form id, and no
   addresses at all - which is the property this fixture is really pinning: a
   lowered module that contained an address could not be laid out twice. *)

let bytes_of l =
  match T.encode l with
  | Ok (`Fixed a) -> a.Lowered_ast.bytes
  | Ok (`Relax (a :: _)) -> a.Lowered_ast.bytes
  | Ok (`Relax []) -> failwith "empty relaxation ladder"
  | Error d -> failwith (Foundation.Diagnostic.message d)

let form_of l =
  match T.encode l with
  | Ok (`Fixed a) -> Some ("aarch64." ^ a.Lowered_ast.form)
  | Ok (`Relax (a :: _)) -> Some ("aarch64." ^ a.Lowered_ast.form)
  | Ok (`Relax []) -> failwith "empty relaxation ladder"
  | Error d -> failwith (Foundation.Diagnostic.message d)

let direct_lowered : T.fixup_kind Lowered_ast.module_ =
  let frag l = Lowered_ast.Bytes { bytes = bytes_of l; form = form_of l; fixups = []; origin } in
  {
    Lowered_ast.unit_name = "asm_test_entry";
    sections =
      [
        {
          Lowered_ast.sec = { Lowered_ast.sec_name = ".text"; perms = Perms.rx; alignment = 4 };
          fragments =
            [
              (* The fill comes from the target, not from a byte string quoted
                 here. A literal would be a second statement of what A64 pads
                 with, and the one place it can disagree with {!Aarch64.nop_bytes}
                 is the place no test would look. *)
              Lowered_ast.Align { boundary = 4; fill = nop 4; origin };
              Lowered_ast.Label_def { name = "asm_test_entry"; origin };
              frag (T.Lowered.Add_imm { rd = x "x15"; rn = x "sp"; imm = 0L; shift12 = false });
              frag (T.Lowered.Stp_pre { rt = x "x15"; rt2 = x "x30"; rn = x "sp"; offset = -16L });
              frag (T.Lowered.Movz { rd = x "w0"; imm16 = 42L; hw = 0 });
              frag (T.Lowered.Ldr_uoff { rt = x "x30"; rn = x "sp"; offset = 8L });
              frag (T.Lowered.Add_imm { rd = x "sp"; rn = x "sp"; imm = 16L; shift12 = false });
              frag (T.Lowered.Ret { rn = x "x30" });
              Lowered_ast.Set_size
                {
                  name = "asm_test_entry";
                  size = Expr.Binary (Expr.Sub, Expr.Current_location, Expr.Symbol "asm_test_entry");
                  origin;
                };
            ];
        };
      ];
    symbols =
      [
        {
          Lowered_ast.name = "asm_test_entry";
          global = true;
          visibility = Lowered_ast.Default;
          kind = Directive.Function;
          size = Some (Expr.Binary (Expr.Sub, Expr.Current_location, Expr.Symbol "asm_test_entry"));
          section = ".text";
        };
      ];
    declared_sections = [ ".note.GNU-stack" ];
  }

let render_lowered = Fmt.to_to_string Lowered_ast.pp

let%expect_test "lowering the direct normalized AST equals the direct lowered module" =
  match P.lower ~state:T.default_state direct_normalized with
  | Error ds -> print_endline (Foundation.Diagnostic.render_all ds)
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
          align 4 fill=1f 20 03 d5
          label asm_test_entry
          bytes ef 03 00 91              [aarch64.add-imm]
          bytes ef 7b bf a9              [aarch64.stp-pre]
          bytes 40 05 80 52              [aarch64.movz]
          bytes fe 07 40 f9              [aarch64.ldr-uoff]
          bytes ff 43 00 91              [aarch64.add-imm]
          bytes c0 03 5f d6              [aarch64.ret]
          size asm_test_entry = (. - asm_test_entry)
        global asm_test_entry function in .text size=(. - asm_test_entry)
        declared .note.GNU-stack (not allocated)
        |}]

(* {1 One image from three entry points} *)

let image_of low =
  match P.plan low with
  | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
  | Ok l -> (
      let plan = Image.plan_of l in
      let addresses =
        List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
      in
      match Image.bind_image l ~addresses with
      | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
      | Ok img -> Fmt.to_to_string Image.pp img)

let%expect_test "text, direct normalized AST and direct lowered module give one image" =
  let parsed, state = from_text () in
  let from_text_image =
    match P.lower ~state parsed with
    | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
    | Ok low -> image_of low
  in
  let from_normalized =
    match P.lower ~state:T.default_state direct_normalized with
    | Error ds -> failwith (Foundation.Diagnostic.render_all ds)
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
            { directive = Directive.Section { name = ".text"; perms = Perms.rx }; origin };
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
  | Error ds -> List.iter (fun d -> print_endline (Foundation.Diagnostic.message d)) ds);
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
  | Error ds -> List.iter (fun d -> print_endline (Foundation.Diagnostic.message d)) ds);
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
            global = true;
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
  | Error ds -> List.iter (fun d -> print_endline (Foundation.Diagnostic.message d)) ds);
  [%expect
    {|
    symbol absent is declared but never defined
    exported symbol absent is not defined
    entry symbol absent is not defined
    |}]
