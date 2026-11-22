(* RV32 GNU export round trip: every case of the isa-difficult corpus, printed from its typed
   module with [Instruction.pp_gnu], reads back through Rivet's text route as the bytes GNU as made
   from the original text. *)

module C = Gnu_text_corpus

let riscv32 = (module C.P : Rivet_target_intf.Target.DRIVER)

let%expect_test "every RV32 corpus case reads back from its GNU print" =
  let cases = C.load "../../fixtures/isa-difficult/cases.jsonl" in
  let bad = ref 0 in
  List.iter
    (fun (c : C.case) ->
      let outcome =
        match
          C.print ?features:c.features ~instruction:Rivet_riscv32.Instruction.pp_gnu c.source
        with
        | Error m -> Some ("print: " ^ m)
        | Ok text -> (
            match C.assemble ?features:c.features riscv32 text with
            | Error m -> Some ("assemble: " ^ m ^ "\n" ^ text)
            | Ok b when String.equal b c.bytes -> None
            | Ok b -> Some (Printf.sprintf "%s, GNU %s\n%s" (C.hex b) (C.hex c.bytes) text))
      in
      Option.iter
        (fun m ->
          incr bad;
          Printf.printf "%s: %s\n" c.id m)
        outcome)
    cases;
  Printf.printf "%d cases, %d bad\n" (List.length cases) !bad;
  [%expect {| 1028 cases, 0 bad |}]

(* A vendor composition's table row prints the same way: GNU as knows no such row, so its print
   is checked against Rivet alone. *)
module Ext = Rivet_riscv_family_encode.Extensions
module Row = Rivet_riscv_family_encode.Riscv_table_row

let vendor : Ext.Validated.t =
  let gpr lsb = Row.Gpr { lsb; nonzero = false } in
  let row : Row.row =
    {
      mnemonic = "xfoo";
      mask = 0xfe00707fL;
      match_ = 0x0bL;
      operands = [ gpr 7; gpr 15; gpr 20 ];
      xlen = 0;
      feature = "xfoo";
      source = "vendor/xfoo";
    }
  in
  let component : Rivet_target_features.Component.t =
    {
      id = "vendor.xfoo";
      feature = "xfoo";
      requires = [];
      conflicts = [];
      implies = [];
      default_on = false;
      xlens = [];
      not_applicable = None;
      summary = "";
      forms =
        [
          {
            Rivet_target_features.Component.label = "xfoo";
            mnemonics = [ "xfoo" ];
            sources = [ { upstream = "vendor"; name = "xfoo" } ];
            legal = None;
          };
        ];
    }
  in
  match Ext.make ~xlen:32 { components = [ component ]; rows = [| row |] } with
  | Ok v -> v
  | Error ps -> failwith (String.concat "; " ps)

module V =
  Rivet_riscv_family.Make_with
    (Rivet_riscv32_encode.Profile)
    (struct
      let definition = vendor
    end)

module VP = Rivet_text_pipeline.Pipeline.Make (V)

let%expect_test "a vendor row reads back from its GNU print" =
  let features =
    match Rivet_target_features.Config.parse_spec "+xfoo" with
    | Ok s -> Some s
    | Error m -> failwith m
  in
  let source =
    {|
      .text
    1:
      xfoo a0, a1, a2
      bnez a0, 1b
      lui a1, %hi(far + 4)
      lw a2, %lo(far + 4)(a1)
      call far
    far:
      ret
|}
  in
  let src = Rivet_foundation.Span.source ~name:"v.s" ~contents:source in
  let text =
    match (VP.initial_state features, VP.parse ~unit_name:"v" ~source:src) with
    | Ok state, Ok m -> (
        match VP.simplify ~state m with
        | Ok (n, _) ->
            Rivet_asm_core.Gnu_module.to_string C.gnu_syntax ~instruction:V.Instruction.pp_gnu n
        | Error ds -> C.render ds)
    | Error ds, _ | _, Error ds -> C.render ds
  in
  print_string text;
  let vp = (module VP : Rivet_target_intf.Target.DRIVER) in
  let bytes t = Result.map C.hex (C.assemble ?features vp t) in
  print_endline
    (match (bytes source, bytes text) with
    | Ok a, Ok b when String.equal a b -> "same bytes"
    | Ok a, Ok b -> a ^ " / " ^ b
    | Error m, _ | _, Error m -> m);
  [%expect
    {|
    	.section	.text,"ax",@progbits
    .Lrivet.1.0:
    	xfoo x10, x11, x12
    	bnez x10, .Lrivet.1.0
    	lui x11, %hi((far + 4))
    	lw x12, %lo((far + 4))(x11)
    	call far
    far:
    	ret
    same bytes |}]

(* A numeric local label reads back on every target with a GNU printer. *)
let%expect_test "local labels read back from the GNU print on x86-64 and AArch64" =
  let check (type i) (module D : Rivet_target_intf.Target.DRIVER) ~type_char
      ~(simplify :
         Rivet_foundation.Span.source -> (i Rivet_asm_core.Normalized_ast.module_, string) result)
      ~(instruction : i Fmt.t) source =
    let src = Rivet_foundation.Span.source ~name:"l.s" ~contents:source in
    match simplify src with
    | Error m -> print_endline m
    | Ok n -> (
        let text =
          Rivet_asm_core.Gnu_module.to_string { Rivet_asm_core.Gnu_module.type_char } ~instruction n
        in
        print_string text;
        match (C.assemble (module D) source, C.assemble (module D) text) with
        | Ok a, Ok b -> print_endline (if String.equal a b then "same bytes" else "DIFFERENT")
        | Error m, _ | _, Error m -> print_endline m)
  in
  let module X = Rivet_text_pipeline.Pipeline.Make (X86_64) in
  let module A = Rivet_text_pipeline.Pipeline.Make (Rivet_aarch64) in
  let simplify_with parse simplify state src =
    match parse ~unit_name:"l" ~source:src with
    | Error ds -> Error (C.render ds)
    | Ok m -> ( match simplify ~state m with Ok (n, _) -> Ok n | Error ds -> Error (C.render ds))
  in
  check
    (module X)
    ~type_char:'@'
    ~simplify:(simplify_with X.parse (fun ~state m -> X.simplify ~state m) X86_64.default_state)
    ~instruction:X86_64.Instruction.pp_gnu
    {|
      .text
    1:
      nop
      jmp 1b
      leaq 1f(%rip), %rax
    1:
      ret
|};
  check
    (module A)
    ~type_char:'%'
    ~simplify:
      (simplify_with A.parse (fun ~state m -> A.simplify ~state m) Rivet_aarch64.default_state)
    ~instruction:Rivet_aarch64.Instruction.pp_gnu
    {|
      .text
    1:
      add x0, x0, #1
      b 1b
      cbz x0, 1f
    1:
      ret
|};
  [%expect
    {|
    	.section	.text,"ax",@progbits
    .Lrivet.1.0:
    	nop
    	jmp .Lrivet.1.0
    	leaq .Lrivet.1.1(%rip), %rax
    .Lrivet.1.1:
    	ret
    same bytes
    	.section	.text,"ax",%progbits
    .Lrivet.1.0:
    	add x0, x0, #1
    	b .Lrivet.1.0
    	cbz x0, .Lrivet.1.1
    .Lrivet.1.1:
    	ret
    same bytes |}]

(* A symbol whose name starts with [$] (CompCert's spelling of the C identifier [$1]) is printed
   parenthesised where an operand would start, so the print reads back; the bytes are the ones
   x86_64-linux-gnu-as and i686-linux-gnu-as 2.44 produce for the same source bound at 0. *)
let%expect_test "$ names read back from the GNU print on x86-64 and x86-32" =
  let check (type i) (module D : Rivet_target_intf.Target.DRIVER) ~parse ~simplify ~state
      ~(instruction : i Fmt.t) source =
    let src = Rivet_foundation.Span.source ~name:"d.s" ~contents:source in
    match parse ~unit_name:"d" ~source:src with
    | Error ds -> print_endline (C.render ds)
    | Ok m -> (
        match simplify ~state m with
        | Error ds -> print_endline (C.render ds)
        | Ok (n, _) -> (
            let text = Rivet_asm_core.Gnu_module.to_string C.gnu_syntax ~instruction n in
            print_string text;
            match (C.assemble (module D) source, C.assemble (module D) text) with
            | Ok a, Ok b when String.equal a b -> print_endline (C.hex a)
            | Ok a, Ok b -> Printf.printf "%s / %s\n" (C.hex a) (C.hex b)
            | Error m, _ | _, Error m -> print_endline m))
  in
  let module X64 = Rivet_text_pipeline.Pipeline.Make (X86_64) in
  let module X32 = Rivet_text_pipeline.Pipeline.Make (X86_32) in
  check
    (module X64)
    ~parse:X64.parse
    ~simplify:(fun ~state m -> X64.simplify ~state m)
    ~state:X86_64.default_state ~instruction:X86_64.Instruction.pp_gnu
    {|
      .text
      .globl $3
    $3:
      movl %eax, ($1)(%rip)
      leaq ($1)+4(%rip), %rax
      movl $1+$1, %ecx
      call ($3)
      jmp ($3)
    $1:
      ret
|};
  check
    (module X32)
    ~parse:X32.parse
    ~simplify:(fun ~state m -> X32.simplify ~state m)
    ~state:X86_32.default_state ~instruction:X86_32.Instruction.pp_gnu
    {|
      .text
      .globl $3
    $3:
      movl %eax, ($1)
      movl ($1)+4, %eax
      movl $$1, %edx
      pushl $$3
      call ($3)
    $1:
      ret
|};
  [%expect
    {|
    	.section	.text,"ax",@progbits
    	.globl	$3
    $3:
    	movl %eax, ($1)(%rip)
    	leaq ($1 + 4)(%rip), %rax
    	movl $(1 + $1), %ecx
    	call ($3)
    	jmp ($3)
    $1:
    	ret
    89 05 13 00 00 00 48 8d 05 10 00 00 00 b9 1a 00 00 00 e8 e9 ff ff ff eb e7 c3
    	.section	.text,"ax",@progbits
    	.globl	$3
    $3:
    	movl %eax, ($1)
    	movl ($1 + 4), %eax
    	movl $$1, %edx
    	push $$3
    	call ($3)
    $1:
    	ret
    a3 19 00 00 00 a1 1d 00 00 00 ba 19 00 00 00 68 00 00 00 00 e8 e7 ff ff ff c3 |}]
