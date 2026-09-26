(* Characterization of the two component extraction candidates, RISC-V M and
   x86 x87, recorded before either is moved out of its family encoder.

   These transcripts pin the observable behavior an extraction must preserve:
   bytes, [form_id] paths, and rejection diagnostics through the text path.
   Codec trees are pinned separately by test/cram/asm_dump.t; this file adds
   what that dump cannot show - which mnemonics a component owns and how each
   one is refused when misused. *)

let driver target = match Driver.Registry.find target with Some d -> d | None -> failwith target

let diagnostics_text ds =
  String.concat ""
    (List.map
       (fun d ->
         Printf.sprintf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
       (Foundation.Diag.diagnostics ds))

(* The diagnostic disassembly of [text] bound at a fixed address, or the rendered
   diagnostics if it does not assemble. *)
let disasm_string target text =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ~unit_name:"t" ~source () with
  | Error ds -> diagnostics_text ds
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map
          (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x40000000L))
          plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Error ds -> diagnostics_text ds
      | Ok img -> (
          match img.Image.segments with
          | s :: _ -> (
              match D.dump_disasm_diagnostic ~address:s.Image.address s.Image.bytes with
              | Ok text -> text
              | Error ds -> diagnostics_text ds)
          | [] -> "(no segments)\n"))

let disasm target text = print_string (disasm_string target text)

let one target line =
  Printf.printf "-- %s: %s\n" target (String.trim line);
  disasm target ("\t.text\n\t" ^ line ^ "\n")

(* {1 RISC-V M}

   Exactly three M mnemonics are implemented: [mul], [remu] and the RV64-only
   [mulw]. The rest of the extension is captured but not admitted. *)

let%expect_test "RISC-V M: the implemented mnemonics on both profiles" =
  List.iter
    (fun t ->
      one t "mul x5, x6, x7";
      one t "remu x5, x6, x7";
      one t "mulw x5, x6, x7")
    [ "riscv32"; "riscv64" ];
  [%expect
    {|
    -- riscv32: mul x5, x6, x7
    40000000  b3 02 73 02  mul x5, x6, x7  [riscv32.mul]
    -- riscv32: remu x5, x6, x7
    40000000  b3 72 73 02  remu x5, x6, x7  [riscv32.remu]
    -- riscv32: mulw x5, x6, x7
    riscv32.lower: mulw is available only when XLEN is 64
    -- riscv64: mul x5, x6, x7
    40000000  b3 02 73 02  mul x5, x6, x7  [riscv64.mul]
    -- riscv64: remu x5, x6, x7
    40000000  b3 72 73 02  remu x5, x6, x7  [riscv64.remu]
    -- riscv64: mulw x5, x6, x7
    40000000  bb 02 73 02  mulw x5, x6, x7  [riscv64.mulw] |}]

let%expect_test "RISC-V M: mnemonics beyond the implemented subset are rejected" =
  List.iter
    (fun m -> one "riscv64" (m ^ " x5, x6, x7"))
    [ "div"; "divu"; "rem"; "mulh"; "mulhu"; "mulhsu"; "divw"; "remw" ];
  [%expect
    {|
    -- riscv64: div x5, x6, x7
    riscv64.simplify: unknown instruction div
    -- riscv64: divu x5, x6, x7
    riscv64.simplify: unknown instruction divu
    -- riscv64: rem x5, x6, x7
    riscv64.simplify: unknown instruction rem
    -- riscv64: mulh x5, x6, x7
    riscv64.simplify: unknown instruction mulh
    -- riscv64: mulhu x5, x6, x7
    riscv64.simplify: unknown instruction mulhu
    -- riscv64: mulhsu x5, x6, x7
    riscv64.simplify: unknown instruction mulhsu
    -- riscv64: divw x5, x6, x7
    riscv64.simplify: unknown instruction divw
    -- riscv64: remw x5, x6, x7
    riscv64.simplify: unknown instruction remw |}]

let%expect_test "RISC-V M: operand misuse" =
  one "riscv64" "mul x5, x6";
  one "riscv64" "mul x5, x6, 7";
  one "riscv64" "remu x5, x6, x7, x8";
  [%expect
    {|
    -- riscv64: mul x5, x6
    riscv64.lower: no mul form takes these operands
    -- riscv64: mul x5, x6, 7
    riscv64.lower: no mul form takes these operands
    -- riscv64: remu x5, x6, x7, x8
    riscv64.lower: no remu form takes these operands |}]

(* {1 x86 x87} *)

let x87_memory =
  [
    "fldl 8(%esp)";
    "fstpl 24(%esp)";
    "fstps 52(%esp)";
    "flds 4(%esp)";
    "fildll 4(%esp)";
    "fistpll 4(%esp)";
    "fadds 4(%esp)";
    "fsubs 4(%esp)";
    "fnstcw 4(%esp)";
    "fldcw 4(%esp)";
  ]

let x87_fixed = [ "fucomp"; "fnstsw %ax"; "fadd %st(1), %st"; "sahf" ]

let%expect_test "x87: the implemented forms on both profiles" =
  List.iter (fun t -> List.iter (one t) (x87_memory @ x87_fixed)) [ "x86_32"; "x86_64" ];
  [%expect
    {|
    -- x86_32: fldl 8(%esp)
    40000000  dd 44 24 08  fldl 8(%esp)  [x86_32.fldl.opsz-absent.sib-disp8]
    -- x86_32: fstpl 24(%esp)
    40000000  dd 5c 24 18  fstpl 24(%esp)  [x86_32.fstpl.opsz-absent.sib-disp8]
    -- x86_32: fstps 52(%esp)
    40000000  d9 5c 24 34  fstps 52(%esp)  [x86_32.fstps.opsz-absent.sib-disp8]
    -- x86_32: flds 4(%esp)
    40000000  d9 44 24 04  flds 4(%esp)  [x86_32.flds.opsz-absent.sib-disp8]
    -- x86_32: fildll 4(%esp)
    40000000  df 6c 24 04  fildll 4(%esp)  [x86_32.fildll.opsz-absent.sib-disp8]
    -- x86_32: fistpll 4(%esp)
    40000000  df 7c 24 04  fistpll 4(%esp)  [x86_32.fistpll.opsz-absent.sib-disp8]
    -- x86_32: fadds 4(%esp)
    x86.lower: no fadds form takes these operands
    -- x86_32: fsubs 4(%esp)
    x86.lower: no fsubs form takes these operands
    -- x86_32: fnstcw 4(%esp)
    40000000  d9 7c 24 04  fnstcw 4(%esp)  [x86_32.fnstcw.opsz-absent.sib-disp8]
    -- x86_32: fldcw 4(%esp)
    40000000  d9 6c 24 04  fldcw 4(%esp)  [x86_32.fldcw.opsz-absent.sib-disp8]
    -- x86_32: fucomp
    40000000  dd e9  fucomp  [x86_32.fucomp]
    -- x86_32: fnstsw %ax
    40000000  df e0  fnstsw %ax  [x86_32.fnstsw]
    -- x86_32: fadd %st(1), %st
    40000000  d8 c1  fadd %st(1), %st  [x86_32.fadd-st0-x87]
    -- x86_32: sahf
    40000000  9e  sahf  [x86_32.sahf]
    -- x86_64: fldl 8(%esp)
    40000000  67 dd 44 24 08  fldl 8(%esp)  [x86_64.fldl.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fstpl 24(%esp)
    40000000  67 dd 5c 24 18  fstpl 24(%esp)  [x86_64.fstpl.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fstps 52(%esp)
    40000000  67 d9 5c 24 34  fstps 52(%esp)  [x86_64.fstps.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: flds 4(%esp)
    40000000  67 d9 44 24 04  flds 4(%esp)  [x86_64.flds.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fildll 4(%esp)
    40000000  67 df 6c 24 04  fildll 4(%esp)  [x86_64.fildll.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fistpll 4(%esp)
    40000000  67 df 7c 24 04  fistpll 4(%esp)  [x86_64.fistpll.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fadds 4(%esp)
    x86.lower: no fadds form takes these operands
    -- x86_64: fsubs 4(%esp)
    x86.lower: no fsubs form takes these operands
    -- x86_64: fnstcw 4(%esp)
    40000000  67 d9 7c 24 04  fnstcw 4(%esp)  [x86_64.fnstcw.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fldcw 4(%esp)
    40000000  67 d9 6c 24 04  fldcw 4(%esp)  [x86_64.fldcw.asz-present.opsz-absent.rex-absent.sib-disp8]
    -- x86_64: fucomp
    40000000  dd e9  fucomp  [x86_64.fucomp]
    -- x86_64: fnstsw %ax
    40000000  df e0  fnstsw %ax  [x86_64.fnstsw]
    -- x86_64: fadd %st(1), %st
    40000000  d8 c1  fadd %st(1), %st  [x86_64.fadd-st0-x87]
    -- x86_64: sahf
    40000000  9e  sahf  [x86_64.sahf] |}]

let%expect_test "x87: bare-symbol sources" =
  List.iter
    (fun m ->
      Printf.printf "-- x86_32: %s f\n" m;
      disasm "x86_32" ("\t.text\nf:\n\t" ^ m ^ " f\n"))
    [ "flds"; "fadds"; "fsubs"; "fldl" ];
  [%expect
    {|
    -- x86_32: flds f
    40000000  d9 05 00 00 00 40  flds 1073741824  [x86_32.flds.opsz-absent.disp32-norm]
    -- x86_32: fadds f
    40000000  d8 05 00 00 00 40  fadds 1073741824  [x86_32.fadds.opsz-absent.disp32-norm]
    -- x86_32: fsubs f
    40000000  d8 25 00 00 00 40  fsubs 1073741824  [x86_32.fsubs.opsz-absent.disp32-norm]
    -- x86_32: fldl f
    x86.lower: no fldl form takes these operands |}]

let%expect_test "x87: operand misuse" =
  one "x86_32" "fldl %eax";
  one "x86_32" "fucomp %st(1)";
  one "x86_32" "fnstsw %bx";
  one "x86_32" "fnstsw";
  one "x86_32" "fadd %st, %st(1)";
  one "x86_32" "faddp";
  [%expect
    {|
    -- x86_32: fldl %eax
    x86.lower: no fldl form takes these operands
    -- x86_32: fucomp %st(1)
    x86.simplify: fucomp takes no operands in M5
    -- x86_32: fnstsw %bx
    x86.simplify: fnstsw is only supported as fnstsw %%ax in M5
    -- x86_32: fnstsw
    x86.simplify: fnstsw is only supported as fnstsw %%ax in M5
    -- x86_32: fadd %st, %st(1)
    x86.lower: no fadd form takes these operands
    -- x86_32: faddp
    x86.simplify: unknown instruction faddp |}]

(* {1 Descriptors}

   Each family publishes its components as data. These checks compose them the
   way the family does and tie the descriptors back to what the encoder
   actually does, so a descriptor cannot claim a form the codec lacks. *)

let%expect_test "component descriptors are structurally clean in every profile" =
  let report name components =
    Printf.printf "%s: %s\n" name
      (String.concat "; "
         (List.map
            (fun (c : Target_component.t) ->
              Printf.sprintf "%s feature=%s forms=%d" c.id c.feature (List.length c.forms))
            components));
    List.iter (Printf.printf "  problem: %s\n") (Target_component.check components)
  in
  report "riscv32" Riscv32_encode.components;
  report "riscv64" Riscv64_encode.components;
  report "x86_32" X86_32_encode.components;
  report "x86_64" X86_64_encode.components;
  [%expect
    {|
    riscv32: riscv.zmmul feature=zmmul forms=2; riscv.m feature=m forms=1
    riscv64: riscv.zmmul feature=zmmul forms=2; riscv.m feature=m forms=1
    x86_32: x86.x87 feature=x87 forms=13
    x86_64: x86.x87 feature=x87 forms=13 |}]

let%expect_test "Target_component.check reports collisions" =
  let form label mnemonic sources : Target_component.form =
    { label; mnemonics = [ mnemonic ]; sources }
  in
  let src = [ { Target_component.upstream = "test"; name = "n" } ] in
  let a : Target_component.t =
    {
      id = "t.a";
      feature = "a";
      requires = [ "ghost" ];
      conflicts = [];
      summary = "";
      forms = [ form "x" "op" src; form "x" "op2" [] ];
    }
  in
  let b : Target_component.t =
    {
      id = "t.a";
      feature = "b";
      requires = [];
      conflicts = [];
      summary = "";
      forms = [ form "y" "op" src ];
    }
  in
  let empty : Target_component.t =
    { id = "t.e"; feature = "e"; requires = []; conflicts = []; summary = ""; forms = [] }
  in
  List.iter print_endline (Target_component.check [ a; b; empty ]);
  [%expect
    {|
    t.a: form x has no source mapping
    t.e: no forms
    t.a: requires names unknown feature ghost
    duplicate component id t.a
    duplicate form label x
    duplicate mnemonic op |}]

(* The x86 codec dump lists each alternative as "[priority cost=n] label ...". *)
let dump_labels target =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  String.split_on_char '\n' (D.dump_codec ())
  |> List.filter_map (fun line ->
      match String.index_opt line ']' with
      | Some i -> (
          match
            String.split_on_char ' ' (String.sub line (i + 1) (String.length line - i - 1))
            |> List.filter (fun t -> t <> "")
          with
          | label :: _ -> Some label
          | [] -> None)
      | None -> None)

let%expect_test "x86 descriptor labels are all present in the codec dump" =
  List.iter
    (fun (target, components) ->
      let labels = dump_labels target in
      List.iter
        (fun (c : Target_component.t) ->
          List.iter
            (fun l ->
              if not (List.mem l labels) then
                Printf.printf "%s: %s lacks alternative %s\n" target c.id l)
            (Target_component.labels c))
        components;
      Printf.printf "%s: %d alternatives, all descriptor labels present\n" target
        (List.length labels))
    [ ("x86_32", X86_32_encode.components); ("x86_64", X86_64_encode.components) ];
  [%expect
    {|
    x86_32: 165 alternatives, all descriptor labels present
    x86_64: 166 alternatives, all descriptor labels present |}]

(* The word a descriptor predicts for [mnemonic x5, x6, x7]: rd=5, rs1=6, rs2=7. *)
let%expect_test "M descriptors predict the assembled instruction word" =
  List.iter
    (fun (target, xlen) ->
      List.iter
        (fun (f : Riscv_family_encode.Riscv_ext_m.form) ->
          if xlen = 64 || not f.rv64_only then
            let predicted =
              f.opcode lor (5 lsl 7) lor (f.funct3 lsl 12) lor (6 lsl 15) lor (7 lsl 20)
              lor (f.funct7 lsl 25)
            in
            let text = disasm_string target ("\t.text\n\t" ^ f.mnemonic ^ " x5, x6, x7\n") in
            let bytes =
              Scanf.sscanf text "%_x %2x %2x %2x %2x" (fun a b c d ->
                  (d lsl 24) lor (c lsl 16) lor (b lsl 8) lor a)
            in
            Printf.printf "%s %s: %s\n" target f.mnemonic
              (if bytes = predicted then "matches"
               else Printf.sprintf "%08x <> %08x" bytes predicted))
        Riscv_family_encode.Riscv_ext_m.forms)
    [ ("riscv32", 32); ("riscv64", 64) ];
  [%expect
    {|
    riscv32 mul: matches
    riscv32 remu: matches
    riscv64 mul: matches
    riscv64 mulw: matches
    riscv64 remu: matches |}]
