(* RV32 GNU export round trip: every case of the isa-difficult corpus, printed from its typed
   module with [Instruction.pp_gnu], reads back through Rivet's text route as the bytes GNU as made
   from the original text. *)

module C = Gnu_text_corpus

let riscv32 = (module C.P : Target_intf.Target.DRIVER)

let%expect_test "every RV32 corpus case reads back from its GNU print" =
  let cases = C.load "../../fixtures/isa-difficult/cases.jsonl" in
  let bad = ref 0 in
  List.iter
    (fun (c : C.case) ->
      let outcome =
        match C.print ?features:c.features ~instruction:Riscv32.Instruction.pp_gnu c.source with
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
module Ext = Riscv_family_encode.Extensions
module Row = Riscv_family_encode.Riscv_table_row

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
  let component : Target_component.t =
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
            Target_component.label = "xfoo";
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
  Riscv_family.Make_with
    (Riscv32_encode.Profile)
    (struct
      let definition = vendor
    end)

module VP = Pipeline.Make (V)

let%expect_test "a vendor row reads back from its GNU print" =
  let features =
    match Target_config.parse_spec "+xfoo" with Ok s -> Some s | Error m -> failwith m
  in
  let source =
    "\t.text\n\
     1:\txfoo a0, a1, a2\n\
     \tbnez a0, 1b\n\
     \tlui a1, %hi(far + 4)\n\
     \tlw a2, %lo(far + 4)(a1)\n\
     \tcall far\n\
     far:\tret\n"
  in
  let src = Foundation.Span.source ~name:"v.s" ~contents:source in
  let text =
    match (VP.initial_state features, VP.parse ~unit_name:"v" ~source:src) with
    | Ok state, Ok m -> (
        match VP.simplify ~state m with
        | Ok (n, _) ->
            Asm_core.Gnu_module.to_string C.gnu_syntax ~instruction:V.Instruction.pp_gnu n
        | Error ds -> C.render ds)
    | Error ds, _ | _, Error ds -> C.render ds
  in
  print_string text;
  let vp = (module VP : Target_intf.Target.DRIVER) in
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
  let check (type i) (module D : Target_intf.Target.DRIVER) ~type_char
      ~(simplify : Foundation.Span.source -> (i Asm_core.Normalized_ast.module_, string) result)
      ~(instruction : i Fmt.t) source =
    let src = Foundation.Span.source ~name:"l.s" ~contents:source in
    match simplify src with
    | Error m -> print_endline m
    | Ok n -> (
        let text = Asm_core.Gnu_module.to_string { Asm_core.Gnu_module.type_char } ~instruction n in
        print_string text;
        match (C.assemble (module D) source, C.assemble (module D) text) with
        | Ok a, Ok b -> print_endline (if String.equal a b then "same bytes" else "DIFFERENT")
        | Error m, _ | _, Error m -> print_endline m)
  in
  let module X = Pipeline.Make (X86_64) in
  let module A = Pipeline.Make (Aarch64) in
  let simplify_with parse simplify state src =
    match parse ~unit_name:"l" ~source:src with
    | Error ds -> Error (C.render ds)
    | Ok m -> ( match simplify ~state m with Ok (n, _) -> Ok n | Error ds -> Error (C.render ds))
  in
  check
    (module X)
    ~type_char:'@'
    ~simplify:(simplify_with X.parse X.simplify X86_64.default_state)
    ~instruction:X86_64.Instruction.pp_gnu
    "\t.text\n1:\tnop\n\tjmp 1b\n\tleaq 1f(%rip), %rax\n1:\tret\n";
  check
    (module A)
    ~type_char:'%'
    ~simplify:(simplify_with A.parse A.simplify Aarch64.default_state)
    ~instruction:Aarch64.Instruction.pp_gnu
    "\t.text\n1:\tadd x0, x0, #1\n\tb 1b\n\tcbz x0, 1f\n1:\tret\n";
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
