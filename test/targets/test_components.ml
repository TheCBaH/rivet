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

let%expect_test "RISC-V M: the division and high-multiply forms assemble and decode" =
  List.iter
    (fun m -> one "riscv64" (m ^ " x5, x6, x7"))
    [ "div"; "divu"; "rem"; "mulh"; "mulhu"; "mulhsu"; "divw"; "divuw"; "remw"; "remuw" ];
  [%expect
    {|
    -- riscv64: div x5, x6, x7
    40000000  b3 42 73 02  div x5, x6, x7  [riscv64.div]
    -- riscv64: divu x5, x6, x7
    40000000  b3 52 73 02  divu x5, x6, x7  [riscv64.divu]
    -- riscv64: rem x5, x6, x7
    40000000  b3 62 73 02  rem x5, x6, x7  [riscv64.rem]
    -- riscv64: mulh x5, x6, x7
    40000000  b3 12 73 02  mulh x5, x6, x7  [riscv64.mulh]
    -- riscv64: mulhu x5, x6, x7
    40000000  b3 32 73 02  mulhu x5, x6, x7  [riscv64.mulhu]
    -- riscv64: mulhsu x5, x6, x7
    40000000  b3 22 73 02  mulhsu x5, x6, x7  [riscv64.mulhsu]
    -- riscv64: divw x5, x6, x7
    40000000  bb 42 73 02  divw x5, x6, x7  [riscv64.divw]
    -- riscv64: divuw x5, x6, x7
    40000000  bb 52 73 02  divuw x5, x6, x7  [riscv64.divuw]
    -- riscv64: remw x5, x6, x7
    40000000  bb 62 73 02  remw x5, x6, x7  [riscv64.remw]
    -- riscv64: remuw x5, x6, x7
    40000000  bb 72 73 02  remuw x5, x6, x7  [riscv64.remuw] |}]

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
    riscv32: riscv.zmmul feature=zmmul forms=5; riscv.m feature=m forms=8
    riscv64: riscv.zmmul feature=zmmul forms=5; riscv.m feature=m forms=8
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
    riscv32 mulh: matches
    riscv32 mulhsu: matches
    riscv32 mulhu: matches
    riscv32 div: matches
    riscv32 divu: matches
    riscv32 rem: matches
    riscv32 remu: matches
    riscv64 mul: matches
    riscv64 mulw: matches
    riscv64 mulh: matches
    riscv64 mulhsu: matches
    riscv64 mulhu: matches
    riscv64 div: matches
    riscv64 divu: matches
    riscv64 rem: matches
    riscv64 remu: matches
    riscv64 divw: matches
    riscv64 divuw: matches
    riscv64 remw: matches
    riscv64 remuw: matches |}]

(* The fence family against real riscv64-linux-gnu-as 2.44: bare [fence] is [fence iorw,iorw]
   (0ff0000f), not [fence rw,w] (0310000f); [fence.tso] is 8330000f and Zihintpause's [pause]
   0100000f. *)
let%expect_test "RISC-V fence spellings match GNU as" =
  List.iter (one "riscv64")
    [ "fence"; "fence rw,w"; "fence r,rw"; "fence iorw,o"; "fence.tso"; "pause"; "fence rx,w" ];
  [%expect
    {|
    -- riscv64: fence
    40000000  0f 00 f0 0f  fence  [riscv64.fence]
    -- riscv64: fence rw,w
    40000000  0f 00 10 03  fence rw, w  [riscv64.fence rw,w]
    -- riscv64: fence r,rw
    40000000  0f 00 30 02  fence r, rw  [riscv64.fence r,rw]
    -- riscv64: fence iorw,o
    40000000  0f 00 40 0f  fence iorw, o  [riscv64.fence iorw,o]
    -- riscv64: fence.tso
    40000000  0f 00 30 83  fence.tso  [riscv64.fence.tso]
    -- riscv64: pause
    40000000  0f 00 00 01  pause  [riscv64.pause]
    -- riscv64: fence rx,w
    riscv64.lower: no fence form takes these operands |}]

(* DEC-RV-TABLE priority and collision rules for the generated rows: a row never shares a
   mnemonic with a hand-written form (the hand-written one would silently win), and the row's
   own word decodes back to that row, not to a hand-written form or an earlier row. Operand fields are filled with distinct
   non-zero values, so a general row is not mistaken for a pseudo that fixes one of them to
   zero ([add.uw] with rs2 = x0 is [zext.w]). A HINT row ([ntl.*], [prefetch.*], [lpad]) is a
   base instruction with rd = x0, which the hand-written decoder claims first by design; those
   are listed. *)
let%expect_test
    "generated RISC-V table rows neither collide with nor are shadowed by hand-written forms" =
  let module Rows = Riscv_family_encode.Riscv_table_rows in
  let module Row = Riscv_family_encode.Riscv_table_row in
  (* the instruction bit holding a scattered immediate's lowest value bit: a
     valid, aligned, non-zero value *)
  let lowest (sc : Row.scatter) =
    let ibit, _ =
      List.fold_left
        (fun (bi, bv) (i, v) -> if v < bv then (i, v) else (bi, bv))
        (0, max_int) sc.bits
    in
    Int64.shift_left 1L ibit
  in
  let check (type o) target xlen (of_mnemonic : string -> o option) (is_table : o -> int option)
      (decode_index : string -> [ `Row of int | `Hand_written | `Nothing ]) =
    let problems = ref [] and hints = ref [] and aliases = ref [] in
    Array.iteri
      (fun i (r : Row.row) ->
        if r.xlen = 0 || r.xlen = xlen then (
          (match Option.bind (of_mnemonic r.mnemonic) is_table with
          | Some _ -> ()
          | None ->
              problems :=
                Printf.sprintf "%s: shadowed by a hand-written form" r.mnemonic :: !problems);
          let word, _ =
            List.fold_left
              (fun (w, prev) (k, (o : Row.operand)) ->
                let v = Int64.of_int (k + 1) in
                let put lsb = Int64.logor w (Int64.shift_left v lsb) in
                match o with
                | Gpr { lsb; _ } | Fpr { lsb } -> (put lsb, v)
                | Uimm { lsb; _ } | Simm { lsb; _ } | Fli { lsb } -> (put lsb, prev)
                | Mem_i { base } | Mem_s { base } | Mem_zero { base } | Mem_hi { base } ->
                    (put base, prev)
                | Creg { lsb } | Cfreg { lsb } -> (put lsb, prev)
                | Gpr_except { lsb; _ } -> (put lsb, v)
                | Scatter sc | Spmem { offset = sc } -> (Int64.logor w (lowest sc), prev)
                | Cmem { base; offset } -> (Int64.logor (put base) (lowest offset), prev)
                | Cui { lo; _ } -> (Int64.logor w (Int64.shift_left 1L lo), prev)
                | Sreg { lsb } -> (put lsb, prev)
                | Rlist { lsb } -> (Int64.logor w (Int64.shift_left 4L lsb), prev)
                | Stack_adj _ -> (w, prev)
                | Uimm_min { lsb; min; _ } ->
                    (Int64.logor w (Int64.shift_left (Int64.of_int min) lsb), prev)
                | Gpr_pair { lsb; _ } ->
                    (Int64.logor w (Int64.shift_left (Int64.of_int (2 * (k + 1))) lsb), prev)
                | Tied { lsb } -> (Int64.logor w (Int64.shift_left prev lsb), prev)
                | Fixed_gpr _ | Rm _ | Keyword _ -> (w, prev))
              (r.match_, 0L)
              (List.mapi (fun k o -> (k, o)) r.operands)
          in
          let bytes =
            String.init 4 (fun k ->
                Char.chr (Int64.to_int (Int64.shift_right_logical word (8 * k)) land 0xff))
          in
          match decode_index bytes with
          | `Row j when j = i -> ()
          | `Row j
            when Int64.equal Rows.rows.(j).mask r.mask && Int64.equal Rows.rows.(j).match_ r.match_
            ->
              aliases := Printf.sprintf "%s=%s" r.source Rows.rows.(j).source :: !aliases
          | `Row j ->
              problems :=
                Printf.sprintf "%s: decodes as row %s" r.source Rows.rows.(j).source :: !problems
          | `Hand_written -> hints := r.source :: !hints
          | `Nothing -> problems := Printf.sprintf "%s: does not decode" r.source :: !problems))
      Rows.rows;
    Printf.printf
      "%s: %s\n\
      \  decoded as the hand-written form they are a hint of: %s\n\
      \  identical encodings: %s\n"
      target
      (match !problems with [] -> "ok" | ps -> String.concat "; " (List.rev ps))
      (String.concat " " (List.rev !hints))
      (String.concat " " (List.rev !aliases))
  in
  check "riscv32" 32 Riscv32_encode.Opcode.of_mnemonic
    (function Riscv32_encode.Opcode.Table i -> Some i | _ -> None)
    (fun bytes ->
      match
        Riscv32_encode.decode_ungated
          { state = Riscv32_encode.default_state; address = 0L }
          bytes ~pos:0
      with
      | Ok ({ op = Riscv32_encode.Opcode.Table j; _ }, _, _) -> `Row j
      | Ok _ -> `Hand_written
      | Error _ -> `Nothing);
  check "riscv64" 64 Riscv64_encode.Opcode.of_mnemonic
    (function Riscv64_encode.Opcode.Table i -> Some i | _ -> None)
    (fun bytes ->
      match
        Riscv64_encode.decode_ungated
          { state = Riscv64_encode.default_state; address = 0L }
          bytes ~pos:0
      with
      | Ok ({ op = Riscv64_encode.Opcode.Table j; _ }, _, _) -> `Row j
      | Ok _ -> `Hand_written
      | Error _ -> `Nothing);
  [%expect
    {|
    riscv32: ok
      decoded as the hand-written form they are a hint of: rv_zihintntl/ntl.all rv_zihintntl/ntl.p1 rv_zihintntl/ntl.pall rv_zihintntl/ntl.s1 rv_zicbo/prefetch.i rv_zicbo/prefetch.r rv_zicbo/prefetch.w rv_c_zihintntl/c.ntl.all rv_c_zihintntl/c.ntl.p1 rv_c_zihintntl/c.ntl.pall rv_c_zihintntl/c.ntl.s1 rv_zicfilp/lpad
      identical encodings: rv_zcmop/c.mop.1=rv_c_zicfiss/c.sspush.x1 rv_zcmop/c.mop.5=rv_c_zicfiss/c.sspopchk.x5
    riscv64: ok
      decoded as the hand-written form they are a hint of: rv_zihintntl/ntl.all rv_zihintntl/ntl.p1 rv_zihintntl/ntl.pall rv_zihintntl/ntl.s1 rv_zicbo/prefetch.i rv_zicbo/prefetch.r rv_zicbo/prefetch.w rv_c_zihintntl/c.ntl.all rv_c_zihintntl/c.ntl.p1 rv_c_zihintntl/c.ntl.pall rv_c_zihintntl/c.ntl.s1 rv_zicfilp/lpad
      identical encodings: rv_zcmop/c.mop.1=rv_c_zicfiss/c.sspush.x1 rv_zcmop/c.mop.5=rv_c_zicfiss/c.sspopchk.x5 |}]

(* Table rows the generated differential cases cannot spell yet, pinned to real GNU as 2.44
   bytes: an AMO's ordering suffixes ([amoadd.b] 0x00c5852f, [.aq] 0x04c5852f, [.rl]
   0x02c5852f, [.aqrl] 0x06c5852f), and Zacas's register pairs - [amocas.d] on RV32 takes an
   even rd/rs2 (GNU as: "illegal operands" for a1). *)
let%expect_test "RISC-V table: AMO ordering suffixes and Zacas register pairs" =
  List.iter (one "riscv64")
    [
      "amoadd.b a0, a2, (a1)";
      "amoadd.b.aq a0, a2, (a1)";
      "amoadd.b.rl a0, a2, (a1)";
      "amoadd.b.aqrl a0, a2, (a1)";
      "amocas.w.aqrl a0, a2, (a1)";
    ];
  List.iter (one "riscv32") [ "amocas.d a0, a2, (a1)"; "amocas.d a1, a2, (a1)" ];
  [%expect
    {|
    -- riscv64: amoadd.b a0, a2, (a1)
    40000000  2f 85 c5 00  amoadd.b x10, x12, 0(x11)  [riscv64.amoadd.b]
    -- riscv64: amoadd.b.aq a0, a2, (a1)
    40000000  2f 85 c5 04  amoadd.b.aq x10, x12, 0(x11)  [riscv64.amoadd.b.aq]
    -- riscv64: amoadd.b.rl a0, a2, (a1)
    40000000  2f 85 c5 02  amoadd.b.rl x10, x12, 0(x11)  [riscv64.amoadd.b.rl]
    -- riscv64: amoadd.b.aqrl a0, a2, (a1)
    40000000  2f 85 c5 06  amoadd.b.aqrl x10, x12, 0(x11)  [riscv64.amoadd.b.aqrl]
    -- riscv64: amocas.w.aqrl a0, a2, (a1)
    40000000  2f a5 c5 2e  amocas.w.aqrl x10, x12, 0(x11)  [riscv64.amocas.w.aqrl]
    -- riscv32: amocas.d a0, a2, (a1)
    40000000  2f b5 c5 28  amocas.d x10, x12, 0(x11)  [riscv32.amocas.d]
    -- riscv32: amocas.d a1, a2, (a1)
    riscv32.lower: no amocas.d form takes these operands |}]

(* DEC-X86-TABLE: every generated row's encoding of a representative operand list decodes back to
   a form of the same length (the codec or a row), in each mode the row applies to. Rows are only
   reached when the hand-written forms decline, so this pins that nothing they emit is
   undecodable. *)
let%expect_test "generated x86 table rows round-trip through the decoder" =
  let module Row = X86_family_encode.X86_table_row in
  let check (type d) target ~mode64 ~(reg : width:int -> int -> X86_family_encode.Reg.t)
      ~(encode : Row.row -> X86_family_encode.Operand.t list -> string option)
      ~(decode : string -> (int, d) result) =
    let failures = ref [] and count = ref 0 in
    Array.iter
      (fun (r : Row.row) ->
        if r.mode = 0 || mode64 then
          let ops =
            List.mapi
              (fun k (o : Row.operand) ->
                match o with
                | Reg { cls; _ } ->
                    X86_family_encode.Operand.Reg (reg ~width:(Row.class_width cls) (k + 1))
                | Fixed_reg name ->
                    X86_family_encode.Operand.Reg
                      (match name with
                      | "cl" -> reg ~width:8 1
                      | "al" -> reg ~width:8 0
                      | "ax" -> reg ~width:16 0
                      | "eax" -> reg ~width:32 0
                      | "dx" -> reg ~width:16 2
                      | _ -> reg ~width:64 0)
                | Mem _ ->
                    X86_family_encode.Operand.Mem
                      (X86_family_encode.Mem.of_base ~disp:(X86_family_encode.Disp.Const 16L)
                         (reg ~width:(if mode64 then 64 else 32) 3))
                | Imm _ -> X86_family_encode.Operand.Imm (Foundation.Bigint.of_int 1))
              r.operands
          in
          match encode r ops with
          | None -> ()
          | Some bytes -> (
              incr count;
              match decode bytes with
              | Ok len when len = String.length bytes -> ()
              | _ -> failures := r.source :: !failures))
      X86_family_encode.X86_table_rows.rows;
    Printf.printf "%s: %s\n" target
      (match !failures with [] -> "ok" | fs -> String.concat " " (List.rev fs))
  in
  check "x86_32" ~mode64:false ~reg:X86_32_encode.reg_at ~encode:X86_32_encode.table_encode_row
    ~decode:(fun b ->
      Result.map
        (fun (_, _, len) -> len)
        (X86_32_encode.decode_ungated
           { state = X86_32_encode.default_state; address = 0L }
           b ~pos:0));
  check "x86_64" ~mode64:true ~reg:X86_64_encode.reg_at ~encode:X86_64_encode.table_encode_row
    ~decode:(fun b ->
      Result.map
        (fun (_, _, len) -> len)
        (X86_64_encode.decode_ungated
           { state = X86_64_encode.default_state; address = 0L }
           b ~pos:0));
  [%expect {|
    x86_32: ok
    x86_64: ok |}]
