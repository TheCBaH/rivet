(* Target-level tests: the codec checks, the two escape-hatch relations, and
   the M1.4 rejections.

   Split from test/differential because the questions are different. That file
   asks "do we agree with GNU as"; this one asks "is each target's own
   description internally consistent, and does the restricted linker refuse
   what it says it refuses". Neither answer implies the other: a codec can be
   self-consistent and wrong, and a linker can reject everything. *)

let show_list label = function
  | [] -> Printf.printf "%s: none\n" label
  | xs -> List.iter (fun x -> Printf.printf "%s: %s\n" label x) xs

(* {1 What Codec.check can decide}

   Syntactic properties only, and the boundary matters: an empty problem list
   is not "this codec is correct", it is "field widths, table injectivity,
   duplicate priorities and *visible* fixed-bit ambiguity are all clean". The
   overlaps that priority resolves would show here if the patterns were
   comparable; several are not, because an [Alt] whose branches differ in width
   has no single pattern - which is the ordinary x86 situation and is why the
   x86 forms carry explicit priorities anyway. *)

let%expect_test "every target's codec passes Codec.check" =
  List.iter
    (fun (module D : Target_intf.Target.DRIVER) -> show_list D.name (D.check_codec ()))
    Driver.Registry.drivers;
  [%expect
    {|
    x86_32: none
    x86_64: none
    arm: alt arm: movw and dp-imm have overlapping fixed bits; priority 1 before 3 decides
    arm: alt arm: movt and dp-imm have overlapping fixed bits; priority 2 before 3 decides
    arm: alt arm: vmov-reg-d and v3-d have overlapping fixed bits; priority 12 before 20 decides
    arm: alt arm: vmov-reg-s and v3-s have overlapping fixed bits; priority 13 before 21 decides
    arm: alt arm: vmov-imm-d and v3-d have overlapping fixed bits; priority 14 before 20 decides
    arm: alt arm: vmov-imm-s and v3-s have overlapping fixed bits; priority 15 before 21 decides
    arm: alt arm: v3-d and vneg-d have overlapping fixed bits; priority 20 before 22 decides
    arm: alt arm: v3-d and vcmp-reg-d have overlapping fixed bits; priority 20 before 24 decides
    arm: alt arm: v3-d and vcmp-zero-d have overlapping fixed bits; priority 20 before 26 decides
    arm: alt arm: v3-d and vcvt-f32-f64 have overlapping fixed bits; priority 20 before 29 decides
    arm: alt arm: v3-d and vcvt-f64-s32 have overlapping fixed bits; priority 20 before 32 decides
    arm: alt arm: v3-d and vcvt-f64-u32 have overlapping fixed bits; priority 20 before 33 decides
    arm: alt arm: v3-d and vcvt-s32-f64 have overlapping fixed bits; priority 20 before 34 decides
    arm: alt arm: v3-s and vneg-s have overlapping fixed bits; priority 21 before 23 decides
    arm: alt arm: v3-s and vcmp-reg-s have overlapping fixed bits; priority 21 before 25 decides
    arm: alt arm: v3-s and vcmp-zero-s have overlapping fixed bits; priority 21 before 27 decides
    arm: alt arm: v3-s and vcvt-f64-f32 have overlapping fixed bits; priority 21 before 30 decides
    arm: alt arm: v3-s and vcvt-f32-s32 have overlapping fixed bits; priority 21 before 31 decides
    aarch64: none
    riscv32: none
    riscv64: none
    |}]

let%expect_test "RISC-V codec round-trips a word and an atomic call pair" =
  let open Riscv32_encode in
  let zero = Asm_core.Expr.Const Foundation.Bigint.zero in
  let values =
    [
      Lowered.I
        {
          name = "addi";
          opcode = 0x13;
          funct3 = 0;
          funct_hi = 0;
          shamt_bits = None;
          rd = 10;
          rs1 = 0;
          imm = Asm_core.Expr.Const (Foundation.Bigint.of_int 42);
        };
      Lowered.Pair { name = "call"; rd = 1; tmp = 1; target = zero; addi = false };
    ]
  in
  List.iter
    (fun value ->
      match Codec.encode codec value with
      | Error _ -> print_endline "encode failed"
      | Ok encoded -> (
          Printf.printf "%s %d bits %s\n"
            (String.concat "." encoded.Codec.form)
            (Codec.Bits.width encoded.bits)
            (Codec.Bits.to_string encoded.bits);
          match Codec.decode_bits codec encoded.bits with
          | Some decoded ->
              Printf.printf "decoded %d bits, equal=%b\n" decoded.Codec.consumed
                (Lowered.equal value decoded.value)
          | None -> print_endline "decode failed"))
    values;
  [%expect
    {|
    word 32 bits 00000010101000000000010100010011
    decoded 32 bits, equal=true
    pair 64 bits 0000000000000000000000001001011100000000000000001000000011100111
    decoded 64 bits, equal=true
    |}]

(* {1 Declared modifier lexing} *)

let driver name =
  match Driver.Registry.find name with Some d -> d | None -> failwith ("missing target " ^ name)

let source text = Foundation.Span.source ~name:"<test>" ~contents:text

let show_tokens target text =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  match D.dump_tokens ~source:(source text) with
  | Ok dump -> print_string dump
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

let%expect_test "modifier spellings are declared by the lexical profile" =
  show_tokens "riscv32" "%pcrel_hi(symbol) %undeclared(symbol)\n";
  show_tokens "arm" ":lower16:symbol\n";
  [%expect
    {|
    0 9 modifier %pcrel_hi
    9 1 lparen (
    10 6 ident symbol
    16 1 rparen )
    18 1 percent %
    19 10 ident undeclared
    29 1 lparen (
    30 6 ident symbol
    36 1 rparen )
    37 1 eol \n
    38 0 eof
    0 9 modifier :lower16:
    9 6 ident symbol
    15 1 eol \n
    16 0 eof
    |}]

let%expect_test "target state is replayed in source order across push and pop" =
  let (module D : Target_intf.Target.DRIVER) = driver "riscv64" in
  let text =
    {|
.text
.option nopic
la x5, target
.option push
.option pic
la x6, target
.option pop
la x7, target
target:
addi x0, x0, 0
|}
  in
  (match D.dump_lowered_ast ~unit_name:"state" ~source:(source text) with
  | Ok dump -> print_string dump
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds));
  [%expect
    {|
    lowered state
    section .text r-x align=1
      bytes b7 02 00 00              [riscv64.lui]
        @0/4B pc+0 hi abs-hi20 b20 [12+20@0] = target
      bytes 93 82 02 00              [riscv64.addi]
        @0/4B pc+0 lo abs-lo12-i s12 [20+12@0] = target
      bytes 17 03 00 00 13 03 03 00  [riscv64.la]
        @0/4B pc+0 hi pcrel-hi20 b20 [12+20@0] = target head(pair)
        @4/4B pc+0 lo pcrel-lo12-i s12 [20+12@0] = target tail(sibling:pair)
      bytes b7 03 00 00              [riscv64.lui]
        @0/4B pc+0 hi abs-hi20 b20 [12+20@0] = target
      bytes 93 83 03 00              [riscv64.addi]
        @0/4B pc+0 lo abs-lo12-i s12 [20+12@0] = target
      label target
      bytes 13 00 00 00              [riscv64.addi]
    local target notype in .text
    |}]

(* {1 ARM modified immediates}

   The project's motivating [Iso_fun]: neither injective nor total, so no
   [Iso_table] and no static check. What proves it is the exhaustive round trip
   over all 4096 (rot, imm8) pairs, plus the four canonical-selection cases the
   plan pins. *)

let%expect_test "the modified-immediate relation round-trips exhaustively" =
  let dom = Arm.modimm_domain in
  Printf.printf "%s\n" (Fmt.to_to_string Codec.Finite.pp_domain dom);
  let failures =
    Codec.Finite.round_trip Arm.modimm_codec dom ~equal:Int64.equal ~show:(Printf.sprintf "0x%Lx")
  in
  show_list "round-trip failures" failures;
  [%expect
    {|
    arm.modimm: 4096 values, exhaustive
      why: the field is 4 bits of rotation and 8 bits of value, so the domain is exactly 4096 pairs and enumerating it is a table scan rather than a sample
    round-trip failures: none
    |}]

(* Canonical selection is the *smallest rotation*, which is what GNU as picks.
   The representation counts are printed too, because "two representations,
   we chose the first" and "one representation" are different facts and only
   the first is evidence that a selection rule exists. *)
let%expect_test "canonical modified-immediate selection matches GNU as" =
  let representations v =
    List.length
      (List.filter
         (fun rot -> Int64.compare (Arm.rotl32 v (2 * rot)) 256L < 0)
         (List.init 16 (fun r -> r)))
  in
  List.iter
    (fun v ->
      match Arm.encode_modimm v with
      | Some (rot, imm8) ->
          Printf.printf "0x%-10Lx %d representations, chose rot=%d imm8=0x%02x\n" v
            (representations v) rot imm8
      | None ->
          Printf.printf "0x%-10Lx %d representations, rejected by the relation\n" v
            (representations v))
    [ 42L; 0x3f000000L; 0x100L; 0x12345678L ];
  [%expect
    {|
    0x2a         2 representations, chose rot=0 imm8=0x2a
    0x3f000000   2 representations, chose rot=4 imm8=0x3f
    0x100        4 representations, chose rot=12 imm8=0x01
    0x12345678   0 representations, rejected by the relation
    |}]

(* {1 AArch64 scaled offsets}

   Weaker evidence than ARM's, and recorded as weaker: these domains are
   declared boundary samples rather than exhaustive enumerations, so a bug
   between the sampled points would survive. Both [why] strings are printed so
   that the difference is in the transcript rather than only in the source. *)

let%expect_test "the scaled-offset relations round-trip over their declared domains" =
  let check dom codec =
    Printf.printf "%s\n" (Fmt.to_to_string Codec.Finite.pp_domain dom);
    show_list "  failures"
      (Codec.Finite.round_trip codec dom ~equal:Int64.equal ~show:(Printf.sprintf "%Ld"))
  in
  check Aarch64.uoff12_domain (Aarch64.scaled ~shift:3 ~width:12 ~signedness:Codec.Unsigned "imm12");
  check Aarch64.stp_imm7_domain (Aarch64.scaled ~shift:3 ~width:7 ~signedness:Codec.Signed "imm7");
  [%expect
    {|
    aarch64.ldr-uoff12: 5 values, deterministic sample
      why: the field is 12 unsigned bits scaled by eight, so the domain is 4096 values; enumerating it would test the same arithmetic 4096 times, so this is the declared boundary set - zero, both extremes, one past each, and one misaligned value
      failures: none
    aarch64.stp-imm7: 7 values, deterministic sample
      why: the field is 7 signed bits scaled by eight, so the domain is 128 values; the declared boundary set is zero, both extremes and the two steps adjacent to them
      failures: none
    |}]

(* A64's logical immediates are the second [Iso_fun] with no statically
   checkable law, and unlike the scaled offsets their domain *is* exhaustible:
   the encoding is 8192 (N, immr, imms) triples, so every value the relation can
   express is enumerated and round-tripped. The value counts are printed because
   "how many constants can a logical immediate express" is the fact that decides
   whether lowering may reach for this form at all. *)
let%expect_test "the bitmask-immediate relations round-trip exhaustively" =
  List.iter
    (fun datasize ->
      let dom = Aarch64.bitmask_domain ~datasize in
      Printf.printf "%s\n" (Fmt.to_to_string Codec.Finite.pp_domain dom);
      show_list "  failures"
        (Codec.Finite.round_trip (Aarch64.bitmask_codec ~datasize) dom ~equal:Int64.equal
           ~show:(Printf.sprintf "0x%Lx")))
    [ 32; 64 ];
  [%expect
    {|
    aarch64.bitmask32: 1302 values, exhaustive
      why: the field is N:immr:imms, so the domain is 8192 triples for 64-bit and 4096 for 32-bit; enumerating the values they decode to is a table scan rather than a sample
      failures: none
    aarch64.bitmask64: 5334 values, exhaustive
      why: the field is N:immr:imms, so the domain is 8192 triples for 64-bit and 4096 for 32-bit; enumerating the values they decode to is a table scan rather than a sample
      failures: none |}]

(* The two values CompCert actually asks for, and the one it cannot: a bitmask
   immediate is a rotated run of ones, so 7 and 6 are expressible and 5 - which
   is not a contiguous run - is not. *)
let%expect_test "which small constants are bitmask immediates" =
  List.iter
    (fun v ->
      match Aarch64.encode_bitmask ~datasize:32 v with
      | Some (n, immr, imms) -> Printf.printf "%Ld  n=%d immr=%d imms=%d\n" v n immr imms
      | None -> Printf.printf "%Ld  not expressible\n" v)
    (* 0 and all-ones are the two the architecture reserves: a run of ones as
       long as the element would be four spellings of one value, so the encoding
       excludes it, and there is no run of length zero. *)
    [ 5L; 6L; 7L; 1L; 0L; 0xFFFFFFFFL ];
  [%expect
    {|
    5  not expressible
    6  n=0 immr=31 imms=1
    7  n=0 immr=0 imms=2
    1  n=0 immr=0 imms=0
    0  not expressible
    4294967295  not expressible |}]

(* {1 The restricted linker}

   One module, one strong exported symbol, no imports. Every unsupported
   condition is an explicit rejection with its own message, because
   "unsupported" and "wrong" must not produce the same diagnostic: a caller
   that hits a limit needs to know it is a limit.

   The one-allocatable-section limit was M1's and is gone: the global fixture
   puts its object in .data and its code in .text, so M2 lays out several
   sections in one module. Merging sections *across inputs*, and linking more
   than one module, remain M3. *)

let attempt target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ~unit_name:"t" ~source () with
  | Ok _ -> print_endline "accepted"
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

let%expect_test "RISC-V option failures are never ignored" =
  attempt "riscv32" ".text\n.option pop\naddi x0, x0, 0\n";
  attempt "riscv32" ".text\n.option rvc\naddi x0, x0, 0\n";
  attempt "riscv32" ".text\n.option relax\naddi x0, x0, 0\n";
  attempt "riscv32" ".text\n.option mystery\naddi x0, x0, 0\n";
  [%expect
    {|
    riscv32.directive: .option pop has no matching push
    riscv32.directive: .option rvc is not supported
    riscv32.directive: .option relax is not supported
    riscv32.directive: unknown .option mystery
    |}]

let show_fixup label result =
  match result with
  | Ok value -> Printf.printf "%-18s %Ld\n" label value
  | Error error ->
      let diagnostic = Riscv32_encode.error_diagnostic (Err.Error.kind error) in
      Printf.printf "%-18s %s\n" label (Foundation.Diagnostic.message diagnostic)

let%expect_test "RISC-V branch, jump, and rounded split boundaries are exact" =
  let branch target = Riscv32_encode.evaluate_fixup Riscv32_encode.Branch13 ~place:0L ~target in
  let jump target = Riscv32_encode.evaluate_fixup Riscv32_encode.Jal21 ~place:0L ~target in
  let hi target = Riscv32_encode.evaluate_fixup Riscv32_encode.Pcrel_hi20 ~place:0L ~target in
  let lo target = Riscv32_encode.evaluate_fixup Riscv32_encode.Pcrel_lo12_i ~place:0L ~target in
  List.iter
    (fun (label, value) -> show_fixup label (branch value))
    [ ("b min", -4096L); ("b below", -4098L); ("b max", 4094L); ("b above", 4096L) ];
  List.iter
    (fun (label, value) -> show_fixup label (jump value))
    [
      ("jal min", -1048576L);
      ("jal below", -1048578L);
      ("jal max", 1048574L);
      ("jal above", 1048576L);
    ];
  show_fixup "hi 0x7ff" (hi 0x7ffL);
  show_fixup "lo 0x7ff" (lo 0x7ffL);
  show_fixup "hi 0x800" (hi 0x800L);
  show_fixup "lo 0x800" (lo 0x800L);
  show_fixup "auipc max" (hi 0x7fffffffL);
  show_fixup "auipc overflow" (hi 0x80000000L);
  show_fixup "rv32 max hi"
    (Riscv32_encode.evaluate_fixup Riscv32_encode.Abs_hi20 ~place:0L ~target:0xffffffffL);
  show_fixup "rv32 max lo"
    (Riscv32_encode.evaluate_fixup Riscv32_encode.Abs_lo12_i ~place:0L ~target:0xffffffffL);
  show_fixup "rv32 overflow"
    (Riscv32_encode.evaluate_fixup Riscv32_encode.Abs_hi20 ~place:0L ~target:0x100000000L);
  [%expect
    {|
    b min              -4096
    b below            branch immediate is out of range
    b max              4094
    b above            branch immediate is out of range
    jal min            -1048576
    jal below          jal immediate is out of range
    jal max            1048574
    jal above          jal immediate is out of range
    hi 0x7ff           0
    lo 0x7ff           2047
    hi 0x800           1
    lo 0x800           -2048
    auipc max          524288
    auipc overflow     auipc immediate is out of range
    rv32 max hi        0
    rv32 max lo        -1
    rv32 overflow      RV32 address immediate is out of range
    |}]

let show_decode target bytes =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  match D.dump_disasm_canonical ~address:0L bytes with
  | Ok text -> Printf.printf "%s: %s" target text
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

let%expect_test "RISC-V decode rejects malformed or profile-incompatible words" =
  show_decode "riscv32" "\x13\x00\x00";
  show_decode "riscv32" "\xff\xff\xff\xff";
  show_decode "riscv32" "\x1b\x00\x00\x00";
  show_decode "riscv32" "\x01\x00\x00\x00";
  [%expect
    {|
    riscv32.decode: fewer than four bytes remain
    riscv32.decode: no form matches this word
    riscv32.decode: no form matches this word
    riscv32.decode: RISC-V instructions must be four bytes
    |}]

let%expect_test "RISC-V I/S and shift widths reject exactly one past their limits" =
  attempt "riscv32" ".text\naddi x1, x2, -2048\nsw x1, 2047(x2)\nslli x1, x2, 31\n";
  attempt "riscv32" ".text\naddi x1, x2, 2048\n";
  attempt "riscv32" ".text\nsw x1, -2049(x2)\n";
  attempt "riscv32" ".text\nslli x1, x2, 32\n";
  attempt "riscv64" ".text\nslli x1, x2, 63\n";
  attempt "riscv64" ".text\nslli x1, x2, 64\n";
  attempt "riscv32" ".text\naddw x1, x2, x3\n";
  [%expect
    {|
    accepted
    riscv32.fixup: addi 2048 (2048) immediate is out of range
    riscv32.fixup: sw immediate is out of range
    riscv32.fixup: slli 32 (32) immediate is out of range
    accepted
    riscv64.fixup: slli 64 (64) immediate is out of range
    riscv32.lower: addw is available only when XLEN is 64
    |}]

let%expect_test "a second allocatable section is accepted, as is a declared one" =
  attempt "x86_64" "\t.text\n\tret\n\t.section .note.GNU-stack,\"\",@progbits\n";
  attempt "x86_64" "\t.text\n\tret\n\t.data\n";
  [%expect {|
    accepted
    accepted
    |}]

let%expect_test "a RIP-relative pc_bias is the realized length, not a constant" =
  (* The one place a per-mnemonic constant would look right and be wrong. x86
     measures a RIP-relative displacement from the *end* of the instruction, so
     the bias is six bytes for a plain `movl g(%rip), %eax` and seven for the
     REX-prefixed `movq`. Hard-coding six would corrupt the displacement of
     every 64-bit load by one byte - and also the ELF addend the oracle derives
     from it, since that is `byte_offset - pc_bias`.

     Both loads name the same symbol from the same section, so the difference in
     the bound bytes is the difference in the bias and nothing else. *)
  let text =
    "\t.text\nasm_test_entry:\n\tmovq g(%rip), %rax\n\tmovl g(%rip), %eax\n\tret\ng:\n\t.long 7\n"
  in
  (match Driver.Portable.dump "x86_64" ~which:`Lowered_ast ~unit_name:"t" ~text with
  | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
  | Ok s ->
      List.iter
        (fun l -> if String.length l > 0 && l.[0] = ' ' then print_endline (String.trim l))
        (String.split_on_char '\n' s));
  (match Driver.Portable.assemble "x86_64" ~unit_name:"t" ~text with
  | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
  | Ok p -> (
      match Driver.Portable.bind_at p ~base:0x400000L with
      | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
      | Ok b -> (
          match Driver.Portable.section_bytes b ".text" with
          | None -> print_endline "no .text"
          | Some s ->
              print_endline
                (String.concat " "
                   (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i]))))
          )));
  [%expect
    {|
    label asm_test_entry
    bytes 48 8b 05 00 00 00 00     [x86_64.mov-r-rm.asz-absent.rex-present.disp32-norm]
    @3/4B pc+7 disp pcrel32-data s32 [0+32@0] = g
    bytes 8b 05 00 00 00 00        [x86_64.mov-r-rm.asz-absent.rex-absent.disp32-norm]
    @2/4B pc+6 disp pcrel32-data s32 [0+32@0] = g
    bytes c3                       [x86_64.ret]
    label g
    bytes 07 00 00 00
    48 8b 05 07 00 00 00 8b 05 01 00 00 00 c3 07 00 00 00
    |}]

let%expect_test "one base places one segment, and refuses to guess at two" =
  (* [bind_at] is the convenience for the caller who has a single section and
     one address. With two there is no single answer, and the guess it used to
     make - every segment at [base] - would be caught by [bind.overlap] only
     when the segments happen to have nonzero size, and then reported as a bad
     address rather than as a question that has none.

     [bind_sequential] is the documented deterministic alternative, so a caller
     who needs *some* valid placement does not invent one - two callers that
     each invented one would produce two images from the same plan. *)
  let text = "\t.text\n\tret\n\t.data\n\t.long 1\n" in
  let show label r =
    match r with
    | Error e -> Printf.printf "%-12s %s\n" label (Driver.Portable.render_error (Err.Error.kind e))
    | Ok b ->
        Printf.printf "%-12s %s\n" label
          (String.concat " "
             (List.map
                (fun (n, _, _, _) ->
                  match Driver.Portable.section_bytes b n with
                  | Some s -> Printf.sprintf "%s:%d" n (String.length s)
                  | None -> n ^ ":?")
                [ (".text", 0, 0, 0); (".data", 0, 0, 0) ]))
  in
  (match Driver.Portable.assemble "x86_64" ~unit_name:"t" ~text:"\t.text\n\tret\n" with
  | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
  | Ok p -> show "one" (Driver.Portable.bind_at p ~base:0x400000L));
  match Driver.Portable.assemble "x86_64" ~unit_name:"t" ~text with
  | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
  | Ok p ->
      show "two" (Driver.Portable.bind_at p ~base:0x400000L);
      show "sequential" (Driver.Portable.bind_sequential p ~base:0x400000L ~gap:0x1000);
      [%expect
        {|
        one          .text:1 .data:?
        two          bind.multi-segment: this plan has 2 allocatable segments (.text, .data), so one base does not place it; use bind_sequential or pass an address per segment
        sequential   .text:1 .data:4
        |}]

let%expect_test "a NOBITS section is accepted, in both spellings of the type" =
  (* M3 §6: real support, not a rejection - [.bss] and an explicit
     [.section name,"aw",@nobits]/[%nobits] both name a real, allocatable
     NOBITS section. Both spellings, for the same reason [@function] has two:
     [@] introduces a comment on ARM, so GAS writes [%nobits] there and a
     table that knew only one of them would accept on x86 and refuse on
     ARM. *)
  attempt "x86_64" "\t.text\n\tret\n\t.bss\n";
  attempt "x86_64" "\t.text\n\tret\n\t.section .bss,\"aw\",@nobits\n";
  attempt "arm" "\t.text\n\tbx lr\n\t.section .bss,\"aw\",%nobits\n";
  (* A PROGBITS section with the type spelled out is still accepted, exactly
     as before: the type is read, not assumed. *)
  attempt "x86_64" "\t.text\n\tret\n\t.section .data,\"aw\",@progbits\n";
  [%expect {|
    accepted
    accepted
    accepted
    accepted
    |}]

let%expect_test "a NOBITS section holds labels and .zero reservations, never initialized bytes" =
  (* This is what makes "real support" true rather than aspirational: a label
     lands at the right offset, [.zero] reserves without writing, and actual
     content - a [.byte] here, equally an instruction - is a validation
     error naming the section, not a silent drop into the image. *)
  attempt "x86_64" "\t.text\n\tret\n\t.bss\n\tcounter:\n\t.zero 4\n\tflag:\n\t.zero 1\n";
  attempt "x86_64" "\t.text\n\tret\n\t.bss\n\t.byte 1\n";
  attempt "x86_64" "\t.text\n\tret\n\t.bss\n\tret\n";
  [%expect
    {|
    accepted
    image.nobits-content: section .bss is NOBITS and cannot hold initialized content
    image.nobits-content: section .bss is NOBITS and cannot hold initialized content
    |}]

let%expect_test "binding-directive transitions match the measured GNU as table" =
  (* Real GNU `as` probes (M3 §7), not the ELF spec: [.weak] is sticky
     against a later [.local]/[.globl], but a [.weak] directive itself always
     applies. [.local]/[.globl] stay plain last-wins against each other
     whenever [.weak] has not applied - all six ordered pairs, through the
     real directive table and the real transition function, not called
     directly. *)
  let (module D : Target_intf.Target.DRIVER) = driver "x86_64" in
  let show first second =
    let text = Printf.sprintf ".%s x\n.%s x\n" first second in
    match D.dump_lowered_ast ~unit_name:"t" ~source:(source text) with
    | Ok dump ->
        let symbol_line =
          List.find (fun l -> l <> "" && l <> "lowered t") (String.split_on_char '\n' dump)
        in
        Printf.printf ".%s then .%s -> %s\n" first second symbol_line
    | Error ds ->
        List.iter
          (fun d ->
            Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d)
              (Foundation.Diagnostic.message d))
          (Foundation.Diag.diagnostics ds)
  in
  List.iter
    (fun (a, b) -> show a b)
    [
      ("local", "globl");
      ("globl", "local");
      ("local", "weak");
      ("weak", "local");
      ("globl", "weak");
      ("weak", "globl");
    ];
  [%expect
    {|
    .local then .globl -> global x notype in
    .globl then .local -> local x notype in
    .local then .weak -> weak x notype in
    .weak then .local -> weak x notype in
    .globl then .weak -> weak x notype in
    .weak then .globl -> weak x notype in
    |}]

(* {1 The entry symbol across the erased boundary}

   Cardinality named the entry while every fixture had exactly one global. Three
   of the M2 fixtures declare two - a callee, or a data object, beside the entry
   - and then it names nothing, so the caller has to be able to say. The caller
   here is architecture-erased, which is the whole point: an entry that only the
   concrete pipeline accepted would leave the CLI, the differential gate and
   both browser adapters stuck with the inference. *)

let entry_of ?entry target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ?entry ~unit_name:"t" ~source () with
  | Error ds ->
      List.iter
        (fun d -> Printf.printf "%s\n" (Foundation.Diagnostic.code d))
        (Foundation.Diag.diagnostics ds);
      None
  | Ok laid_out -> (
      match Image.bind_image laid_out ~addresses:[ (".text", 0x400000L) ] with
      | Error ds ->
          List.iter
            (fun d -> Printf.printf "%s\n" (Foundation.Diagnostic.code d))
            (Foundation.Diag.diagnostics ds);
          None
      | Ok img -> Some img.Image.entry)

let two_globals =
  "\t.text\n\
   \t.globl callee\n\
   \t.type callee, @function\n\
   callee:\n\
   \tret\n\
   \t.globl asm_test_entry\n\
   \t.type asm_test_entry, @function\n\
   asm_test_entry:\n\
   \tret\n"

let%expect_test "an explicit entry survives erasure; without one, two globals name nothing" =
  let show label e =
    Printf.printf "%-10s %s\n" label
      (match e with
      | None -> "(no image)"
      | Some None -> "no entry"
      | Some (Some a) -> Printf.sprintf "0x%Lx" a)
  in
  show "explicit" (entry_of ~entry:"asm_test_entry" "x86_64" two_globals);
  (* callee is one byte of `ret`, so the entry is one past the section base -
     which is what makes this a check on *which* symbol was chosen rather than
     on whether any address came out. *)
  show "other" (entry_of ~entry:"callee" "x86_64" two_globals);
  show "inferred" (entry_of "x86_64" two_globals);
  [%expect {|
    explicit   0x400001
    other      0x400000
    inferred   no entry |}]

let%expect_test "an unknown directive is a diagnostic, never a silent skip" =
  attempt "x86_64" "\t.text\n\t.frobnicate 1\n\tret\n";
  [%expect {| simplify.directive: unknown directive .frobnicate |}]

let%expect_test "an unknown instruction is a diagnostic" =
  attempt "x86_64" "\t.text\n\txchgq %rax, %rbx\n";
  [%expect {| x86.simplify: unknown instruction xchgq |}]

let%expect_test "x86 refuses to guess an operand size" =
  attempt "x86_64" "\t.text\n\tmov $42, %eax\n";
  [%expect {| x86.simplify: mov needs an operand-size suffix (b, w, l or q) |}]

let%expect_test "x86_32 has no 64-bit operand size" =
  attempt "x86_32" "\t.text\n\tmovq $42, %eax\n";
  [%expect {| x86.simplify: x86_32 has no 64-bit operand size |}]

let%expect_test "an ARM immediate outside the relation is rejected, not approximated" =
  attempt "arm" "\t.text\n\tmov r0, #0x12345678\n";
  [%expect
    {| arm.lower: #305419896 has no A32 modified-immediate representation; movw and mvn are M2 |}]

let%expect_test "an AArch64 ldr offset that is not a multiple of eight is rejected" =
  attempt "aarch64" "\t.text\n\tldr x30, [sp, #4]\n";
  [%expect {| aarch64.lower: offset must be a multiple of 8 for this access width |}]

let%expect_test "a declared but undefined symbol is rejected" =
  attempt "x86_64" "\t.text\n\tret\n\t.globl nowhere\n";
  [%expect
    {|
    image.undefined: symbol nowhere is declared but never defined
    image.export: exported symbol nowhere is not defined
    image.entry: entry symbol nowhere is not defined
    |}]

let%expect_test "code before any section directive has nowhere to go" =
  attempt "x86_64" "\tret\n";
  [%expect {| lower.no-section: this appears before any section directive |}]

(* {1 Binding}

   Address validation is [bind_image]'s, not the target's, and the constraint
   it enforces is the alignment the *plan* stated. A host that ignores the plan
   gets a rejection rather than a misaligned image. *)

let%expect_test "binding at a misaligned address is rejected" =
  let (module D : Target_intf.Target.DRIVER) = Option.get (Driver.Registry.find "aarch64") in
  let source = Foundation.Span.source ~name:"<test>" ~contents:"\t.text\n\t.balign 4\n\tret\n" in
  match D.assemble ~unit_name:"t" ~source () with
  | Error _ -> print_endline "unexpected: did not assemble"
  | Ok l ->
      (let plan = Image.plan_of l in
       let at base =
         List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, base)) plan.Image.segments
       in
       (match Image.bind_image l ~addresses:(at 0x40000002L) with
       | Ok _ -> print_endline "accepted a misaligned address"
       | Error ds ->
           List.iter
             (fun d -> print_endline (Foundation.Diagnostic.message d))
             (Foundation.Diag.diagnostics ds));
       match Image.bind_image l ~addresses:(at 0x40000000L) with
       | Ok img -> print_endline (Fmt.to_to_string Image.pp img)
       | Error _ -> print_endline "unexpected: aligned binding failed");
      [%expect
        {|
    segment .text needs 4-byte alignment, address is 0x40000002
    section .text address=0x40000000 size=4 permissions=r-x
    |}]

(* {1 Simplify's two expression rules (§4.3)}

   Constant folding happens at simplify, and only *fully absolute* subterms
   fold. Both halves are load-bearing: folding at bind instead would report a
   division by zero against a link map rather than against the line that wrote
   it, and folding a symbolic subterm early would mean inventing an address that
   does not exist yet. *)

let normalized target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.dump_normalized_ast ~unit_name:"t" ~source with
  | Ok s -> print_endline (String.trim s)
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

let%expect_test "absolute subterms fold, symbolic ones survive" =
  normalized "x86_64" "\t.text\nfoo:\n\tret\n\t.size foo, 2 + 3 * 4\n\t.size bar, . - foo\n";
  [%expect
    {|
    normalized t
    section .text r-x
    label foo
    ret
    size foo 14
    size bar (. - foo)
    |}]

let%expect_test "a folding error is reported against the line that wrote it" =
  normalized "x86_64" "\t.text\n\tret\n\t.size foo, 1 / 0\n";
  [%expect {| simplify.expression: division by zero |}]

(* A numeric local label is rejected rather than turned into a symbol named
   "1:". Nothing resolves [1b]/[1f] in M1, so such a symbol could never be
   referenced - it would be a file assembled without being understood. *)
let%expect_test "a numeric label with no reference is still a defined symbol" =
  (* It was rejected while there was no resolution pass, because a label
     nothing can reach is a file assembled without being understood. Now it
     resolves to a generated name and assembles. *)
  attempt "x86_64" "\t.text\n1:\n\tret\n";
  [%expect {| accepted |}]

(* {1 Numeric local labels}

   Resolved before the target sees an operand, so what reaches lowering is an
   ordinary symbol. These use the dumps rather than an encoding, because the
   pass is target-independent and the thing under test is which definition a
   reference picked. *)

let dump_norm target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.dump_normalized_ast ~unit_name:"t" ~source with
  | Ok s -> print_string s
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

(* [.size] is the vehicle because it is the only directive that takes an
   expression today; the data directives that will carry these references
   arrive with the rest of M2. What is under test is which definition a
   reference resolved to, and [.size]'s expression shows that exactly. *)

let%expect_test "a repeated numeric label is legal and each use picks the nearest" =
  (* The reason they are called local: [1:] may be defined many times, and a
     reference resolves against the closest one in the direction it names.
     Rejecting the repetition - or always resolving to the first definition -
     would be wrong in opposite ways. *)
  dump_norm "aarch64"
    "\t.text\nfoo:\n1:\n\tret\n\t.size foo, 1b\n1:\n\t.size foo, 1b\n\t.size foo, 1f\n1:\n";
  [%expect
    {|
    normalized t
    section .text r-x
    label foo
    label #L1#0
    ret
    size foo #L1#0
    label #L1#1
    size foo #L1#1
    size foo #L1#2
    label #L1#2
    |}]

let%expect_test "labels precede the statement on their own line" =
  (* [1: .size foo, 1f] must reach the *next* definition, not the one on its
     own line. Getting this backwards turns a forward reference into a self
     reference - which is how [1: jmp 1f] would silently become a loop. *)
  dump_norm "aarch64" "\t.text\nfoo:\n1:\n\t.size foo, 1f\n1:\n\t.size foo, 1b\n";
  [%expect
    {|
    normalized t
    section .text r-x
    label foo
    label #L1#0
    size foo #L1#1
    label #L1#1
    size foo #L1#1
    |}]

let%expect_test "a reference with no definition in its direction is a diagnostic" =
  dump_norm "aarch64" "\t.text\nfoo:\n\t.size foo, 1f\n";
  dump_norm "aarch64" "\t.text\nfoo:\n\t.size foo, 1b\n";
  [%expect
    {|
    parse.local-label: no 1f: after this reference
    parse.local-label: no 1b: before this reference
    |}]

(* {1 Direct calls}

   The first fixup that survives the whole pipeline: parse a symbolic operand,
   lower it to a symbolic branch, encode a placeholder plus a placement, lay
   out, bind, patch, and read the bytes back. Every earlier fixup test stopped
   at one of those boundaries. *)

let disasm target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ~unit_name:"t" ~source () with
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)
  | Ok laid_out -> (
      let plan = Image.plan_of laid_out in
      let addresses =
        List.map
          (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0x40000000L))
          plan.Image.segments
      in
      match Image.bind_image laid_out ~addresses with
      | Error ds ->
          List.iter
            (fun d ->
              Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d)
                (Foundation.Diagnostic.message d))
            (Foundation.Diag.diagnostics ds)
      | Ok img -> (
          match img.Image.segments with
          | s :: _ -> (
              match D.dump_disasm_diagnostic ~address:s.Image.address s.Image.bytes with
              | Ok text -> print_string text
              | Error ds ->
                  List.iter
                    (fun d -> Printf.printf "decode: %s\n" (Foundation.Diagnostic.message d))
                    (Foundation.Diag.diagnostics ds))
          | [] -> print_endline "(no segments)"))

let call_program = "\t.text\n\t.globl f\nf:\n\tret\n\t.globl g\ng:\n\tcall f\n\tret\n"

let%expect_test "a backward call is patched and reads back as its target" =
  (* The displacement is negative, which is the case that catches a fixup field
     decoded without sign extension - and, separately, one decoded big-endian:
     the codec lays bits down most-significant first while the linker patches
     the container little-endian, so the fixup needs the same [le] wrapper
     every other x86 displacement has. A placeholder of all zeroes hides both
     faults completely until something decodes patched bytes. *)
  disasm "x86_64" call_program;
  [%expect
    {|
    40000000  c3              ret              [x86_64.ret]
    40000001  e8 fa ff ff ff  call 1073741824  [x86_64.call-rel32]
    40000006  c3              ret              [x86_64.ret]
    |}]

let%expect_test "the same call on x86_32" =
  disasm "x86_32" call_program;
  [%expect
    {|
    40000000  c3              ret              [x86_32.ret]
    40000001  e8 fa ff ff ff  call 1073741824  [x86_32.call-rel32]
    40000006  c3              ret              [x86_32.ret]
    |}]

let%expect_test "a forward call reaches too" =
  disasm "x86_64" "\t.text\n\t.globl g\ng:\n\tcall f\n\tret\n\t.globl f\nf:\n\tret\n";
  [%expect
    {|
    40000000  e8 01 00 00 00  call 1073741830  [x86_64.call-rel32]
    40000005  c3              ret              [x86_64.ret]
    40000006  c3              ret              [x86_64.ret]
    |}]

let%expect_test "a call to an undefined symbol is rejected at plan time" =
  attempt "x86_64" "\t.text\n\t.globl g\ng:\n\tcall nowhere\n";
  [%expect {| image.undefined: fixup target references undefined symbol nowhere |}]

let%expect_test "the same call on the two fixed-width targets" =
  (* Both measure from somewhere other than the next byte - A64 from the
     instruction itself, A32 from the instruction plus eight - and both store a
     word count rather than a byte displacement. The bias lives in the fixup and
     the division in evaluate_fixup, so neither appears twice. *)
  disasm "aarch64" "\t.text\n\t.globl f\nf:\n\tret\n\t.globl g\ng:\n\tbl f\n\tret\n";
  disasm "arm" "\t.text\n\t.globl f\nf:\n\tbx lr\n\t.globl g\ng:\n\tbl f\n\tbx lr\n";
  [%expect
    {|
    40000000  c0 03 5f d6  ret            [aarch64.ret]
    40000004  ff ff ff 97  bl 1073741824  [aarch64.bl]
    40000008  c0 03 5f d6  ret            [aarch64.ret]
    40000000  1e ff 2f e1  bx lr          [arm.bx]
    40000004  fd ff ff eb  bl 1073741824  [arm.bl]
    40000008  1e ff 2f e1  bx lr          [arm.bx]
    |}]

(* {1 The x86-64 address-size prefix}

   32-bit registers used as *addresses* in 64-bit mode. Omitting the 0x67 does
   not produce a different-but-equal encoding - it addresses %rdi where the
   program said %edi - so this is a correctness property, not a byte-parity
   one. CompCert emits both shapes in the M2 fixtures. *)

let%expect_test "a 32-bit address in 64-bit mode carries 0x67, and decodes back" =
  disasm "x86_64"
    "\t.text\n\t.globl f\nf:\n\tleal 0(%edi,%edi,1), %eax\n\tleal 2(%eax), %eax\n\tret\n";
  [%expect
    {|
    40000000  67 8d 04 3f  leal (%edi,%edi,1), %eax  [x86_64.lea.asz-present.rex-absent.sib-disp0]
    40000004  67 8d 40 02  leal 2(%eax), %eax        [x86_64.lea.asz-present.rex-absent.base-disp8]
    40000008  c3           ret                       [x86_64.ret]
    |}]

let%expect_test "a 64-bit address carries no prefix" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tleaq 16(%rsp), %rax\n\tret\n";
  [%expect
    {|
    40000000  48 8d 44 24 10  leaq 16(%rsp), %rax  [x86_64.lea.asz-absent.rex-present.sib-disp8]
    40000005  c3              ret                  [x86_64.ret]
    |}]

(* {1 8-bit REX-extended sub-registers}

   [%r8b]-[%r15b] (M5 corpus evidence: asm/fixtures/corpus/c/x86_64/
   summary.txt - aes/knucleotide/sha1/sha3/siphash24/vmach all use one).
   x86_64_encode.ml's register list already had every other REX-extended
   width (64, 32, 16 is unused by any fixture and stays absent) but not this
   one. The register table addition is the whole fix - [reg_at]/ModR-M/REX.B
   selection are already width-and-number generic - so this is deliberately
   not a round-trip-through-bytes test: no 8-bit register operand of any kind
   (not even the pre-existing [%al]) lowers all the way to bytes yet, because
   [Opcode.Mov]'s width-8 case is scoped to [movb $imm,(mem)] only
   (`Mov8_only_movb`) - a pre-existing M1-scope limit this fix does not
   touch. What this proves is that [%r8b] now reaches that same, identical,
   already-existing boundary instead of failing earlier as an unknown
   register - i.e. the new register table entry is wired up correctly. *)
(* M1 rejected reg/reg and reg/mem [movb] entirely (only [movb $imm,(mem)]
   was in scope); M5 (asm/docs/corpus.md - [movb %sil, 7(%rdi)] and friends)
   lifted that, so both cases below now decode rather than error. [0x88],
   not [0x89] with an 8-bit width - the opcode byte itself carries the
   operand size for MOV, unlike every other form here where a shared opcode
   picks width from REX.W. *)
let%expect_test "%r8b-%r15b are recognized registers, and 8-bit reg/reg mov uses opcode 0x88" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tmovb %r8b, %al\n\tret\n";
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tmovb %al, %al\n\tret\n";
  [%expect
    {|
    40000000  44 88 c0  movb %r8b, %al  [x86_64.mov-rm-r8.asz-absent.rex-present.reg]
    40000003  c3        ret             [x86_64.ret]
    40000000  88 c0  movb %al, %al  [x86_64.mov-rm-r8.asz-absent.rex-absent.reg]
    40000002  c3     ret            [x86_64.ret]
    |}]

(* {1 M5 corpus-growth forms (asm/docs/corpus.md): actually assembling
   CompCert's [test/c/] corpus, not just parsing it}

   Every byte sequence and canonical spelling below was checked against the
   real, installed [x86_64-linux-gnu-as]/[objdump] (binutils) before being
   promoted here - not hand-typed. *)

let%expect_test
    "or/and/xor in the register-register and mem-source shapes CompCert's own codegen needs" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \torl $128, %r8d\n\
     \torq %r10, %r11\n\
     \tandq %rsi, %r14\n\
     \txorq %rsi, %r9\n\
     \txorq $255, %rcx\n\
     \tret\n";
  [%expect
    {|
    40000000  41 81 c8 80 00 00 00  orl $128, %r8d   [x86_64.alu-rm-imm32.asz-absent.rex-present.reg]
    40000007  4d 09 d3              orq %r10, %r11   [x86_64.alu-rm-r.asz-absent.rex-present.reg]
    4000000a  49 21 f6              andq %rsi, %r14  [x86_64.alu-rm-r.asz-absent.rex-present.reg]
    4000000d  49 31 f1              xorq %rsi, %r9   [x86_64.alu-rm-r.asz-absent.rex-present.reg]
    40000010  48 81 f1 ff 00 00 00  xorq $255, %rcx  [x86_64.alu-rm-imm32.asz-absent.rex-present.reg]
    40000017  c3                    ret              [x86_64.ret]
    |}]

let%expect_test "not (group-3, ext=2)" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tnotl %eax\n\tnotq %r14\n\tret\n";
  [%expect
    {|
    40000000  f7 d0     notl %eax  [x86_64.unary-rm.asz-absent.rex-absent.reg]
    40000002  49 f7 d6  notq %r14  [x86_64.unary-rm.asz-absent.rex-present.reg]
    40000005  c3        ret        [x86_64.ret]
    |}]

(* Group-2 shift/rotate beyond the M4-era shift-by-1-only [0xD1]: the general
   immediate count ([0xC1 ib]) and count-in-%cl ([0xD3]). A literal count of
   1 still picks the shorter [0xD1] form ([shift1-rm], unchanged from M4). *)
let%expect_test "the general group-2 shift/rotate forms" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \trorl $27, %eax\n\
     \tsall $16, %eax\n\
     \tsall %cl, %eax\n\
     \tsarl $2, %eax\n\
     \tshrq $63, %rax\n\
     \tshrl $1, %eax\n\
     \tret\n";
  [%expect
    {|
    40000000  c1 c8 1b     ror $27, %eax  [x86_64.shift-imm-rm.asz-absent.rex-absent.reg]
    40000003  c1 e0 10     shl $16, %eax  [x86_64.shift-imm-rm.asz-absent.rex-absent.reg]
    40000006  d3 e0        shl %cl, %eax  [x86_64.shift-cl-rm.asz-absent.rex-absent.reg]
    40000008  c1 f8 02     sar $2, %eax   [x86_64.shift-imm-rm.asz-absent.rex-absent.reg]
    4000000b  48 c1 e8 3f  shr $63, %rax  [x86_64.shift-imm-rm.asz-absent.rex-present.reg]
    4000000f  d1 e8        shr $1, %eax   [x86_64.shift1-rm.asz-absent.rex-absent.reg]
    40000011  c3           ret            [x86_64.ret]
    |}]

let%expect_test "setcc (0F 90+cc /0, ModR/M reg fixed at 0)" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tsete %al\n\tsetl %r8b\n\tret\n";
  [%expect
    {|
    40000000  0f 94 c0     sete %al   [x86_64.setcc-rm.asz-absent.rex-absent.reg]
    40000003  41 0f 9c c0  setl %r8b  [x86_64.setcc-rm.asz-absent.rex-present.reg]
    40000007  c3           ret        [x86_64.ret]
    |}]

(* Zero-/sign-extending move: the [0F B6/B7/BE/BF] family plus [movslq]'s
   own, structurally different [0x63]. *)
let%expect_test "movzx/movsx/movslq" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovzbl (%rdi), %ecx\n\
     \tmovsbl %bl, %edi\n\
     \tmovslq %eax, %r11\n\
     \tret\n";
  [%expect
    {|
    40000000  0f b6 0f  movzbl (%rdi), %ecx  [x86_64.movzx-b-r-rm.asz-absent.rex-absent.base-disp0]
    40000003  0f be fb  movsbl %bl, %edi     [x86_64.movsx-b-r-rm.asz-absent.rex-absent.reg]
    40000006  4c 63 d8  movslq %eax, %r11    [x86_64.movsxd-r-rm.asz-absent.rex-present.reg]
    40000009  c3        ret                  [x86_64.ret]
    |}]

let%expect_test "the two-operand imul $imm,reg form (0x69/0x6B, short-immediate-first)" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\timull $10000, %ebx\n\timulq $56, %rax\n\tret\n";
  [%expect
    {|
    40000000  69 db 10 27 00 00  imull $10000, %ebx  [x86_64.imul-r-rm-imm32.asz-absent.rex-absent.reg]
    40000006  48 6b c0 38        imulq $56, %rax     [x86_64.imul-r-rm-imm8.asz-absent.rex-present.reg]
    4000000a  c3                 ret                 [x86_64.ret]
    |}]

let%expect_test "TEST's own immediate form (0xF7 /0 id, always full-width, no imm8 alternative)" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\ttestl $1, %edi\n\tret\n";
  [%expect
    {|
    40000000  f7 c7 01 00 00 00  test $1, %edi  [x86_64.test-rm-imm.asz-absent.rex-absent.reg]
    40000006  c3                 ret            [x86_64.ret]
    |}]

(* {1 Base-less scaled-index (SIB) memory operands}

   [disp(,%index,scale)] - no base register, GCC/CompCert's array-index
   address idiom (M5 corpus evidence: asm/fixtures/corpus/c/x86_64/
   summary.txt). SIB.base=101 with mod=00 is reserved to mean "no base,
   disp32 always follows" - there is no shorter encoding, hence 8 bytes
   here where a real-base disp0/disp8 SIB form would take 3-4. *)

let%expect_test "a base-less SIB operand takes a mandatory disp32" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tleaq 0(,%rcx,8), %rdi\n\tret\n";
  [%expect
    {|
    40000000  48 8d 3c cd 00 00 00 00  leaq (,%rcx,8), %rdi  [x86_64.lea.asz-absent.rex-present.sib-nobase-disp32]
    40000008  c3                       ret                   [x86_64.ret]
    |}]

(* Also exercises the [sym_disp] sign-extension fix (x86_family_encode.ml):
   without it, the decoded disp32 redisplays as the unsigned magnitude
   4294967295 instead of -1, even though the encoded bytes (a two's-complement
   0xffffffff) are correct either way - [Fixup] carries no signedness of its
   own, unlike [Field]. The fix is shared with disp32-norm's RIP-relative
   form, which took the same bug; see "a negative RIP-relative displacement
   redisplays with its sign" below. *)
let%expect_test "a base-less SIB operand with a negative displacement" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tleaq -1(,%rbp,2), %rdi\n\tret\n";
  [%expect
    {|
    40000000  48 8d 3c 6d ff ff ff ff  leaq -1(,%rbp,2), %rdi  [x86_64.lea.asz-absent.rex-present.sib-nobase-disp32]
    40000008  c3                       ret                     [x86_64.ret]
    |}]

let%expect_test "a negative RIP-relative displacement redisplays with its sign" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tmovl -1(%rip), %eax\n\tret\n";
  [%expect
    {|
    40000000  8b 05 ff ff ff ff  movl -1(%rip), %eax  [x86_64.mov-r-rm.asz-absent.rex-absent.disp32-norm]
    40000006  c3                 ret                  [x86_64.ret]
    |}]

(* A *symbolic* base-less-SIB displacement - [Te4(,%eax,4)] and the
   parenthesized-expression sibling [(tbl + 4)(,%ecx,8)] (M5 corpus evidence:
   asm/fixtures/corpus/c/x86_32/summary.txt's aes.c/sha3.c). Unlike the
   numeric cases just above, the displacement here is a fixup against a
   symbol, not a literal; [split_nobase_sib] (x86_family.ml) is what makes
   the parenthesized-expression shape parse at all - [(tbl + 4)] is itself a
   grouped expression, not a bare identifier. Checked against i686-linux-gnu-as
   before being promoted here (see corpus.md's Follow-ups / this session). *)
(* [tbl] stays in [.text], like every other fixup test in this file
   ([disasm] binds every segment at the same address, so a second segment
   would overlap the first) and is a code label (a [ret], not a [.long]) so
   the trailing bytes still decode - [disasm] disassembles the whole segment
   linearly, and this test cares about the SIB fixup, not what [tbl] holds. *)
let%expect_test "a base-less SIB operand with a symbolic displacement" =
  disasm "x86_32"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovl tbl(,%eax,4), %eax\n\
     \tmovl (tbl + 4)(,%ecx,8), %ecx\n\
     \tret\n\
     tbl:\n\
     \tret\n\
     \tret\n";
  [%expect
    {|
    40000000  8b 04 85 0f 00 00 40  movl 1073741839(,%eax,4), %eax  [x86_32.mov-r-rm.sib-nobase-disp32]
    40000007  8b 0c cd 13 00 00 40  movl 1073741843(,%ecx,8), %ecx  [x86_32.mov-r-rm.sib-nobase-disp32]
    4000000e  c3                    ret                             [x86_32.ret]
    4000000f  c3                    ret                             [x86_32.ret]
    40000010  c3                    ret                             [x86_32.ret]
    |}]

(* [jmp *sym(,%reg,scale)] - an indirect jump through a jump-table entry
   (M5 corpus evidence: asm/fixtures/corpus/c/x86_32/summary.txt's
   siphash24.c/vmach.c switch-dispatch code). [Opcode.Jmp, [Operand.Mem m]]
   (x86_family_encode.ml) is the only new lowering this needed - [Jmp_rm] was
   already generic over [Rm.t]. *)
let%expect_test "an indirect jmp through a base-less SIB jump-table entry" =
  disasm "x86_32" "\t.text\n\t.globl f\nf:\n\tjmp *tbl(,%eax,4)\ntbl:\n\tret\n";
  [%expect
    {|
    40000000  ff 24 85 07 00 00 40  jmp *1073741831(,%eax,4)  [x86_32.jmp-rm.sib-nobase-disp32]
    40000007  c3                    ret                       [x86_32.ret]
    |}]

(* {1 Base-only memory operands with a symbolic displacement}

   [sym(%base)] and the parenthesized-expression sibling [(sym + N)(%base)] -
   no index, no scale, GCC/CompCert's plain struct/array-field address idiom
   (M5 corpus evidence: asm/fixtures/corpus/c/x86_32/summary.txt's
   almabench.c/nbody.c/sha3.c - "cannot parse operand a(%eax)",
   "cannot parse operand (bodies + 24)(%eax)", "cannot parse operand
   (testvec + 4)(%edx)"). Unlike the base-less-SIB case above, the encoder
   needed a change too: [base-disp32] (x86_family_encode.ml) declined
   [Disp.Sym] outright before this fix. Checked against i686-linux-gnu-as
   (binutils 2.44) before being promoted here: [movl tbl(%eax), %eax] and
   [movl (tbl + 24)(%edx), %edx] both encode as mod=10, no SIB, disp32 with
   an R_386_32 relocation against [.text] - `8b 80 0d 00 00 00` and
   `8b 92 25 00 00 00` respectively, matching this test's bytes exactly. *)
let%expect_test "a base-only memory operand with a symbolic displacement" =
  disasm "x86_32"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovl tbl(%eax), %eax\n\
     \tmovl (tbl + 24)(%edx), %edx\n\
     \tret\n\
     tbl:\n\
     \tret\n\
     \tret\n";
  [%expect
    {|
    40000000  8b 80 0d 00 00 40  movl 1073741837(%eax), %eax  [x86_32.mov-r-rm.base-disp32]
    40000006  8b 92 25 00 00 40  movl 1073741861(%edx), %edx  [x86_32.mov-r-rm.base-disp32]
    4000000c  c3                 ret                          [x86_32.ret]
    4000000d  c3                 ret                          [x86_32.ret]
    4000000e  c3                 ret                          [x86_32.ret]
    |}]

(* {1 SSE2 scalar float}

   M5 corpus evidence (asm/docs/corpus.md): the register class and
   instruction family CompCert's x86_64 double/float codegen needs.
   Byte-for-byte hand-verified against the real, installed
   x86_64-linux-gnu-as/objdump (binutils 2.44) before being written down
   here - the same discipline the base-less-SIB fix above used. *)

let%expect_test "one binop per mandatory-prefix group (f2, f3, 66, none)" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \taddsd %xmm0, %xmm1\n\
     \taddss %xmm0, %xmm1\n\
     \tcomisd %xmm0, %xmm1\n\
     \tcomiss %xmm2, %xmm3\n\
     \tret\n";
  [%expect
    {|
    40000000  f2 0f 58 c8  addsd %xmm0, %xmm1   [x86_64.sse-binop-f2.asz-absent.rex-absent.reg]
    40000004  f3 0f 58 c8  addss %xmm0, %xmm1   [x86_64.sse-binop-f3.asz-absent.rex-absent.reg]
    40000008  66 0f 2f c8  comisd %xmm0, %xmm1  [x86_64.sse-binop-66.asz-absent.rex-absent.reg]
    4000000c  0f 2f da     comiss %xmm2, %xmm3  [x86_64.sse-binop-none.asz-absent.rex-absent.reg]
    4000000f  c3           ret                  [x86_64.ret]
    |}]

(* An xmm8-15 operand needs REX, and the mandatory prefix has to precede it
   (M5 corpus.md's Fix C step 3): real GNU as emits [f2 45 0f 58 c8], not
   [45 f2 0f 58 c8]. *)
let%expect_test "a REX-extended xmm register keeps the mandatory prefix before REX" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\taddsd %xmm8, %xmm9\n\tret\n";
  [%expect
    {|
    40000000  f2 45 0f 58 c8  addsd %xmm8, %xmm9  [x86_64.sse-binop-f2.asz-absent.rex-present.reg]
    40000005  c3              ret                 [x86_64.ret]
    |}]

let%expect_test "movsd/movss both directions, including a SIB-addressed memory operand" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovsd (%rax,%rcx,8), %xmm2\n\
     \tmovsd %xmm2, (%rax,%rcx,8)\n\
     \tmovss (%rax), %xmm1\n\
     \tmovss %xmm1, (%rax)\n\
     \tret\n";
  [%expect
    {|
    40000000  f2 0f 10 14 c8  movsd (%rax,%rcx,8), %xmm2  [x86_64.sse-movsd-load.asz-absent.rex-absent.sib-disp0]
    40000005  f2 0f 11 14 c8  movsd %xmm2, (%rax,%rcx,8)  [x86_64.sse-movsd-store.asz-absent.rex-absent.sib-disp0]
    4000000a  f3 0f 10 08     movss (%rax), %xmm1         [x86_64.sse-movss-load.asz-absent.rex-absent.base-disp0]
    4000000e  f3 0f 11 08     movss %xmm1, (%rax)         [x86_64.sse-movss-store.asz-absent.rex-absent.base-disp0]
    40000012  c3              ret                         [x86_64.ret]
    |}]

(* The one case the SSE prefix design's review round specifically flagged:
   an [asz] (0x67) address-size override combined with a mandatory SSE
   prefix and REX all at once. Real GNU as: [67 f2 44 0f 10 00]. Without
   [asz_of ~rm] actually threaded into the encoder (Fix C step 3), this
   would silently encode as if 64-bit-addressed instead. *)
let%expect_test "movsd with a 32-bit address and an xmm8-15 destination" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\tmovsd (%eax), %xmm8\n\tret\n";
  [%expect
    {|
    40000000  67 f2 44 0f 10 00  movsd (%eax), %xmm8  [x86_64.sse-movsd-load.asz-present.rex-present.base-disp0]
    40000006  c3                 ret                  [x86_64.ret]
    |}]

let%expect_test "cvtsi2sdq sets REX.W; cvttsd2si's width comes from its GPR destination" =
  disasm "x86_64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tcvtsi2sd %eax, %xmm0\n\
     \tcvtsi2sdq %rax, %xmm0\n\
     \tcvttsd2si %xmm0, %eax\n\
     \tcvttsd2si %xmm0, %rax\n\
     \tret\n";
  [%expect
    {|
    40000000  f2 0f 2a c0     cvtsi2sd %eax, %xmm0   [x86_64.cvtsi2sd-r-rm.asz-absent.rex-absent.reg]
    40000004  f2 48 0f 2a c0  cvtsi2sd %rax, %xmm0   [x86_64.cvtsi2sd-r-rm.asz-absent.rex-present.reg]
    40000009  f2 0f 2c c0     cvttsd2si %xmm0, %eax  [x86_64.cvttsd2si-r-rm.asz-absent.rex-absent.reg]
    4000000d  f2 48 0f 2c c0  cvttsd2si %xmm0, %rax  [x86_64.cvttsd2si-r-rm.asz-absent.rex-present.reg]
    40000012  c3              ret                    [x86_64.ret]
    |}]

(* A RIP-relative binop memory operand ([xorpd __negd_mask(%rip), %xmmN] is
   the real corpus shape; a literal displacement stands in for the symbol
   here, matching how the base-less-SIB tests above and "a negative
   RIP-relative displacement..." already do it, so this stays a single-
   segment [.text]-only program. *)
let%expect_test "a RIP-relative binop memory operand (xorpd)" =
  disasm "x86_64" "\t.text\n\t.globl f\nf:\n\txorpd 8(%rip), %xmm3\n\tret\n";
  [%expect
    {|
    40000000  66 0f 57 1d 08 00 00 00  xorpd 8(%rip), %xmm3  [x86_64.sse-binop-66.asz-absent.rex-absent.disp32-norm]
    40000008  c3                       ret                   [x86_64.ret]
    |}]

(* x86_32 has xmm0-xmm7 (no REX byte, so no REX-extended xmm8-15 - GNU as
   rejects %xmm8 outright in 32-bit mode) with the identical opcode bytes as
   x86_64 (M5 corpus evidence: asm/fixtures/corpus/c/x86_32/summary.txt -
   "unknown register %xmmN" on every file with floating-point arithmetic).
   The SSE2 codec itself is already target-agnostic; this is a register-table
   addition only (x86_32_encode.ml). *)
let%expect_test "x86_32 has xmm0-xmm7 with the same SSE2 encoding as x86_64" =
  disasm "x86_32"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \taddsd %xmm2, %xmm3\n\
     \tcvtsi2sd %eax, %xmm4\n\
     \tmovsd (%eax), %xmm5\n\
     \tret\n";
  [%expect
    {|
    40000000  f2 0f 58 da  addsd %xmm2, %xmm3    [x86_32.sse-binop-f2.reg]
    40000004  f2 0f 2a e0  cvtsi2sd %eax, %xmm4  [x86_32.cvtsi2sd-r-rm.reg]
    40000008  f2 0f 10 28  movsd (%eax), %xmm5   [x86_32.sse-movsd-load.base-disp0]
    4000000c  c3           ret                   [x86_32.ret]
    |}]

(* {1 SSE operand-class validation}

   Reusing [Reg.t]/[Rm.t] for both GPR and xmm operands (width 128 is the
   only class marker) buys width-generic ModR/M machinery for free, but
   nothing else stops a cross-class operand from reaching the codec unless
   [lower_instruction]'s new arms check it explicitly (M5, corpus.md's Fix C
   step 5). *)

let%expect_test "a GPR where an xmm operand is required is rejected" =
  attempt "x86_64" "\t.text\n\t.globl f\nf:\n\taddsd %eax, %xmm0\n\tret\n";
  [%expect {| x86.lower: eax is 32-bit, expected an xmm register |}]

(* [cvtsi2sd]'s [rm] must be a GPR of exactly the mnemonic-selected width;
   an xmm register there reuses {!Register_width_mismatch} rather than a
   second class-mismatch case, since xmm's width (128) can never equal a
   GPR instruction's declared 32/64 - see [sse_operand_class_mismatch]'s own
   comment in x86_family_encode.ml. *)
let%expect_test "an xmm register where cvtsi2sd's GPR source is required is rejected" =
  attempt "x86_64" "\t.text\n\t.globl f\nf:\n\tcvtsi2sd %xmm1, %xmm0\n\tret\n";
  [%expect {| x86.lower: xmm1 is 128-bit but the instruction is 32-bit |}]

(* Caught at parse time, in the memory-operand grammar itself (x86_family.ml)
   - the earliest and cheapest point, and the one that protects every
   instruction with a memory operand, not only the new SSE ones. *)
let%expect_test "an xmm register used as a memory base is rejected at parse time" =
  attempt "x86_64" "\t.text\n\t.globl f\nf:\n\tmovsd (%xmm0), %xmm1\n\tret\n";
  [%expect {| x86.operand: %xmm0 cannot be used as a memory operand's base or index register |}]

(* {1 Alignment padding}

   The bytes below were measured by assembling a [.align 16] at every distance
   with each mode's own GNU as, not copied from a manual. That distinction cost
   something to learn: the published Intel table is right for 64-bit only up to
   nine bytes, and is the wrong table entirely for 32-bit, where GAS pads with
   [lea] forms because the long NOP is P6+ and the default target does not
   assume it.

   M1 never reached any of this - both x86 fixtures had their [.align] at
   offset 0, where the padding is empty - which is why the code carried an
   unverified table until the first fixture with two functions. *)

(* The target renders its own failure: this helper is generic over the four,
   so it takes the accessor rather than reaching for a shared printer that
   would stop working the moment a target grows its own tags. *)
let show_padding name ~to_diag f =
  for n = 1 to 12 do
    match f ~length:n with
    | Ok s -> Printf.printf "%s %2d: %s\n" name n (Foundation.Byte_cursor.to_hex s)
    | Error d ->
        Printf.printf "%s %2d: %s\n" name n
          (Foundation.Diagnostic.message (to_diag (Err.Error.kind d)))
  done

let%expect_test "x86-64 padding matches its own GNU as" =
  show_padding "x86_64" ~to_diag:X86_64.error_diagnostic X86_64.nop_bytes;
  [%expect
    {|
    x86_64  1: 90
    x86_64  2: 66 90
    x86_64  3: 0f 1f 00
    x86_64  4: 0f 1f 40 00
    x86_64  5: 0f 1f 44 00 00
    x86_64  6: 66 0f 1f 44 00 00
    x86_64  7: 0f 1f 80 00 00 00 00
    x86_64  8: 0f 1f 84 00 00 00 00 00
    x86_64  9: 66 0f 1f 84 00 00 00 00 00
    x86_64 10: 66 2e 0f 1f 84 00 00 00 00 00
    x86_64 11: 66 66 2e 0f 1f 84 00 00 00 00 00
    x86_64 12: 66 66 2e 0f 1f 84 00 00 00 00 00 90
    |}]

let%expect_test "x86-32 pads with lea forms, not long nops" =
  show_padding "x86_32" ~to_diag:X86_32.error_diagnostic X86_32.nop_bytes;
  [%expect
    {|
    x86_32  1: 90
    x86_32  2: 66 90
    x86_32  3: 8d 76 00
    x86_32  4: 8d 74 26 00
    x86_32  5: 2e 8d 74 26 00
    x86_32  6: 8d b6 00 00 00 00
    x86_32  7: 8d b4 26 00 00 00 00
    x86_32  8: 2e 8d b4 26 00 00 00 00
    x86_32  9: 2e 8d b4 26 00 00 00 00 90
    x86_32 10: 2e 8d b4 26 00 00 00 00 66 90
    x86_32 11: 2e 8d b4 26 00 00 00 00 8d 76 00
    x86_32 12: 2e 8d b4 26 00 00 00 00 8d 74 26 00
    |}]

(* {1 Alignment padding}

   Padding is the one thing in an executable section that is not code, and on
   x86 it is not even *spellable* as code: GNU's 64-bit table ends with
   [66 2e 0f 1f 84 ...] and [66 66 2e 0f 1f 84 ...], while GAS emits its
   prefixes in the order [2e 66], so no instruction source reassembles to those
   two rows. Both dumps therefore print the directive that produced the filler.

   M1 never reached any of this: both x86 fixtures had their alignment at offset
   zero, where the pad is empty. The first fixture with two functions is what
   made whole-image disassembly fail outright. *)

let pad_round_trip target ~insn ~size ~pad =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  (* [pad] bytes of filler, arranged by putting 16 - pad bytes of real
     instructions in front of a 16-byte alignment. Every row of the target's
     table is reached this way, which is the difference between testing the
     table and testing what the table is for. *)
  let body = String.concat "" (List.init ((16 - pad) / size) (fun _ -> "\t" ^ insn ^ "\n")) in
  let text = "\t.text\n" ^ body ^ "\t.balign 16\n\t" ^ insn ^ "\n" in
  let base = 0x400000L in
  (* Rendered on the way in, because this helper's own "no .text" failure is
     prose and not a Portable.error: one shape for the printf below. *)
  let render e = Driver.Portable.render_error (Err.Error.kind e) in
  let bytes_of t =
    match Driver.Portable.assemble target ~unit_name:"t" ~text:t with
    | Error e -> Error (render e)
    | Ok p -> (
        match Driver.Portable.bind_at p ~base with
        | Error e -> Error (render e)
        | Ok b -> (
            match Driver.Portable.section_bytes b ".text" with
            | Some s -> Ok (b, s)
            | None -> Error "no .text"))
  in
  match bytes_of text with
  | Error e -> Printf.printf "pad %2d: %s\n" pad e
  | Ok (b, bytes) -> (
      match Driver.Portable.disassemble b ~mode:`Canonical ~address:base bytes with
      | Error e -> Printf.printf "pad %2d: DISASM %s\n" pad (render e)
      | Ok canonical -> (
          let line =
            List.find_opt
              (fun l -> String.length l > 1 && l.[1] = '.')
              (String.split_on_char '\n' canonical)
          in
          match bytes_of ("\t.text\n" ^ canonical) with
          | Error e -> Printf.printf "pad %2d: REASSEMBLE %s\n" pad e
          | Ok (_, again) ->
              Printf.printf "pad %2d: %-12s %s\n" pad
                (match line with Some l -> String.trim l | None -> "(no directive)")
                (if String.equal bytes again then "round trips" else "DIFFERS")))

let%expect_test "every row of the x86-64 padding table decodes and round-trips" =
  List.iter
    (fun n -> pad_round_trip "x86_64" ~insn:"ret" ~size:1 ~pad:n)
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ];
  [%expect
    {|
    pad  1: .balign 2    round trips
    pad  2: .balign 4    round trips
    pad  3: .balign 4    round trips
    pad  4: .balign 8    round trips
    pad  5: .balign 8    round trips
    pad  6: .balign 8    round trips
    pad  7: .balign 8    round trips
    pad  8: .balign 16   round trips
    pad  9: .balign 16   round trips
    pad 10: .balign 16   round trips
    pad 11: .balign 16   round trips |}]

let%expect_test "every row of the x86-32 padding table decodes and round-trips" =
  (* Eight rows, not eleven, and lea-based rather than long-NOP: the long NOP is
     P6 and later, so 32-bit GNU as pads with [lea] forms instead. A table
     shared between the two modes was silently wrong in every padded 32-bit
     section. *)
  List.iter (fun n -> pad_round_trip "x86_32" ~insn:"ret" ~size:1 ~pad:n) [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  [%expect
    {|
    pad  1: .balign 2    round trips
    pad  2: .balign 4    round trips
    pad  3: .balign 4    round trips
    pad  4: .balign 8    round trips
    pad  5: .balign 8    round trips
    pad  6: .balign 8    round trips
    pad  7: .balign 8    round trips
    pad  8: .balign 16   round trips |}]

let%expect_test "the fixed-width targets pad with their architectural nop" =
  (* Nothing exotic to recognize: A32 and A64 both have a real [nop], so the
     filler is code and decodes as code. The directive still comes back, because
     recognition is by bytes and these bytes are what [nop_bytes] emits. *)
  List.iter (fun n -> pad_round_trip "arm" ~insn:"bx lr" ~size:4 ~pad:n) [ 4; 8; 12 ];
  List.iter (fun n -> pad_round_trip "aarch64" ~insn:"ret" ~size:4 ~pad:n) [ 4; 8; 12 ];
  [%expect
    {|
    pad  4: .balign 8    round trips
    pad  8: .balign 16   round trips
    pad 12: .balign 16   round trips
    pad  4: .balign 8    round trips
    pad  8: .balign 16   round trips
    pad 12: .balign 16   round trips |}]

(* {1 Branch relaxation and form preservation}

   M2.F0 and B10 on real x86 forms rather than on the synthetic ladder in
   test/image. Three things have to hold together and only the whole path shows
   it: layout picks the short rung when it reaches, promotes when it does not,
   and a decoded branch reassembles to the bytes it came from - including a near
   branch whose final displacement would also have fitted a short one. *)

let render_portable e = Driver.Portable.render_error (Err.Error.kind e)

let branch_bytes target ~gap ~mnemonic =
  let filler = String.concat "" (List.init gap (fun _ -> "\tret\n")) in
  let text = Printf.sprintf "\t.text\nasm_test_entry:\n\t%s far\n%sfar:\n\tret\n" mnemonic filler in
  match Driver.Portable.assemble ~entry:"asm_test_entry" target ~unit_name:"t" ~text with
  | Error e -> Error (render_portable e)
  | Ok p -> (
      match Driver.Portable.bind_at p ~base:0x400000L with
      | Error e -> Error (render_portable e)
      | Ok b -> (
          match Driver.Portable.section_bytes b ".text" with
          | Some s -> Ok (b, s)
          | None -> Error "no .text"))

let show_branch target ~gap ~mnemonic =
  match branch_bytes target ~gap ~mnemonic with
  | Error e -> Printf.printf "  gap %4d %s\n" gap e
  | Ok (b, bytes) -> (
      let head = String.sub bytes 0 (min 6 (String.length bytes)) in
      match Driver.Portable.disassemble b ~mode:`Canonical ~address:0x400000L bytes with
      | Error e -> Printf.printf "  gap %4d DISASM %s\n" gap (render_portable e)
      | Ok canon -> (
          let first =
            match List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' canon) with
            | l :: _ -> String.trim l
            | [] -> "(empty)"
          in
          (* Reassembling the canonical text is what proves the pin survived:
             without it a near branch whose displacement fits a byte comes back
             two bytes shorter and the image changes under a round trip. *)
          match Driver.Portable.assemble target ~unit_name:"r" ~text:("\t.text\n" ^ canon) with
          | Error e -> Printf.printf "  gap %4d REASSEMBLE %s\n" gap (render_portable e)
          | Ok p2 -> (
              match Driver.Portable.bind_at p2 ~base:0x400000L with
              | Error e -> Printf.printf "  gap %4d REBIND %s\n" gap (render_portable e)
              | Ok b2 ->
                  let again = Option.value ~default:"" (Driver.Portable.section_bytes b2 ".text") in
                  Printf.printf "  gap %4d %3d bytes  %-18s %-16s %s\n" gap (String.length bytes)
                    (Foundation.Byte_cursor.to_hex head)
                    first
                    (if String.equal again bytes then "round trips" else "REASSEMBLES DIFFERENTLY"))
          ))

let%expect_test "an x86 branch takes the short rung until it cannot" =
  (* 127 is what a signed byte reaches from the end of a two-byte branch, so the
     promotion has to happen at exactly 128 and not before. Each filler byte is
     one `ret`. *)
  List.iter (fun gap -> show_branch "x86_64" ~gap ~mnemonic:"jmp") [ 0; 127; 128 ];
  List.iter (fun gap -> show_branch "x86_64" ~gap ~mnemonic:"jl") [ 0; 127; 128 ];
  [%expect
    {|
    gap    0   3 bytes  eb 00 c3           jmp.d8 4194306   round trips
    gap  127 130 bytes  eb 7f c3 c3 c3 c3  jmp.d8 4194433   round trips
    gap  128 134 bytes  e9 80 00 00 00 c3  jmp.d32 4194437  round trips
    gap    0   3 bytes  7c 00 c3           jl.d8 4194306    round trips
    gap  127 130 bytes  7c 7f c3 c3 c3 c3  jl.d8 4194433    round trips
    gap  128 135 bytes  0f 8c 80 00 00 00  jl.d32 4194438   round trips |}]

let%expect_test "the two x86 profiles relax alike" =
  List.iter (fun gap -> show_branch "x86_32" ~gap ~mnemonic:"jge") [ 127; 128 ];
  [%expect
    {|
    gap  127 130 bytes  7d 7f c3 c3 c3 c3  jge.d8 4194433   round trips
    gap  128 135 bytes  0f 8d 80 00 00 00  jge.d32 4194438  round trips |}]

let%expect_test "a branch form suffix names a rung, and an unknown one is refused" =
  (* The pin is GAS's own spelling and is accepted on input, which is what makes
     canonical output re-parseable. A suffix naming no rung is rejected in this
     phase - which knows the mnemonic and has a span - so the codec's
     unknown-rung diagnostic stays reserved for a direct-lowered producer. *)
  List.iter (fun gap -> show_branch "x86_64" ~gap ~mnemonic:"jmp.d32") [ 0 ];
  List.iter (fun gap -> show_branch "x86_64" ~gap ~mnemonic:"jmp.d8") [ 0 ];
  attempt "x86_64" "\t.text\nfoo:\n\tjmp.d16 foo\n";
  attempt "x86_64" "\t.text\nfoo:\n\tjxx foo\n";
  [%expect
    {|
      gap    0   6 bytes  e9 00 00 00 00 c3  jmp.d32 4194309  round trips
      gap    0   3 bytes  eb 00 c3           jmp.d8 4194306   round trips
    x86.branch-suffix: jmp.d16: the branch form suffixes are .d8, .d32
    x86.simplify: unknown instruction jxx |}]

let%expect_test "a direct-lowered pin is checked by the codec, not by the parser" =
  (* The other half of the frozen split. There is no text here and no mnemonic
     to have a suffix: a producer that builds [Lowered.t] itself hands
     [T.encode] a rung name that no earlier phase saw, so the codec is the only
     layer that can reject it - and it must, because falling back to a rung the
     caller did not ask for is how a pinned near branch silently becomes short.

     The three cases are the three answers: a rung that exists and applies, one
     that does not exist, and - reachable only because [Resolved] is what decode
     produces - the same absence when the operand carries its own rung. *)
  let show rung =
    let target = Asm_core.Lowered_ast.Symbolic { value = Asm_core.Expr.Symbol "foo"; rung } in
    match X86_64.encode (X86_family_encode.Lowered.Jmp_rel { target }) with
    | Ok (`Fixed a) ->
        Printf.printf "%-8s %s  %s\n"
          (Option.value rung ~default:"(none)")
          a.Asm_core.Lowered_ast.form
          (String.concat " "
             (List.init (String.length a.Asm_core.Lowered_ast.bytes) (fun i ->
                  Printf.sprintf "%02x" (Char.code a.Asm_core.Lowered_ast.bytes.[i]))))
    | Ok (`Relax alts) ->
        Printf.printf "%-8s relax over %s\n"
          (Option.value rung ~default:"(none)")
          (String.concat ", " (List.map (fun a -> a.Asm_core.Lowered_ast.form) alts))
    | Error d ->
        let d = X86_64.error_diagnostic (Err.Error.kind d) in
        Printf.printf "%-8s %s: %s\n"
          (Option.value rung ~default:"(none)")
          (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d)
  in
  show None;
  show (Some "d32");
  show (Some "bogus");
  [%expect
    {|
    (none)   relax over jmp-rel.d8, jmp-rel.d32
    d32      jmp-rel.d32  e9 00 00 00 00
    bogus    codec.unknown-rung: encode_rung: no rung is named bogus; this description declares {d32, d8} |}]

(* {1 Relocation modifiers}

   [:lower16:] and [:upper16:] are the whole of §D3 as it reaches a target: the
   dialect declares the names, the lexer makes each one token, the grammar makes
   it a prefix, and lowering consumes it. That last step is what these tests are
   about. [Expr.fold] deliberately preserves a [Modifier], so a modifier that
   lowering forgot to unwrap does not fail here - it survives to [bind_image]
   and is reported as a residual, a diagnostic pointing at the wrong layer. *)

let%expect_test "a split-field modifier lowers to two relocatable halves" =
  disasm "arm"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovw r1, #:lower16:g\n\
     \tmovt r1, #:upper16:g\n\
     \tbx lr\n\
     \t.globl g\n\
     g:\n";
  (* And with a nonzero addend, which is the case a REL target has to get right:
     the addend is not in a record field but in the instruction, so it must
     survive the same split as the address it is added to. *)
  disasm "arm"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tmovw r1, #:lower16:g+5\n\
     \tmovt r1, #:upper16:g+5\n\
     \tbx lr\n\
     \t.globl g\n\
     g:\n";
  [%expect
    {|
    40000000  0c 10 00 e3  movw r1, #12     [arm.movw]
    40000004  00 10 44 e3  movt r1, #16384  [arm.movt]
    40000008  1e ff 2f e1  bx lr            [arm.bx]
    40000000  11 10 00 e3  movw r1, #17     [arm.movw]
    40000004  00 10 44 e3  movt r1, #16384  [arm.movt]
    40000008  1e ff 2f e1  bx lr            [arm.bx]
    |}]

let%expect_test "the modifier and the mnemonic must agree" =
  (* Not interchangeable: [movt] given the low half writes the wrong sixteen
     bits of every address, and nothing downstream can tell. *)
  attempt "arm" "\t.text\n\t.globl g\ng:\n\tmovt r1, #:lower16:g\n";
  attempt "arm" "\t.text\n\t.globl g\ng:\n\tmovw r1, #:upper16:g\n";
  attempt "arm" "\t.text\n\t.globl g\ng:\n\tmovw r1, g\n";
  [%expect
    {|
    arm.lower: movt takes #:upper16:, not #:lower16:
    arm.lower: movw takes #:lower16:, not #:upper16:
    arm.lower: movw needs a #:lower16: operand |}]

let%expect_test "a dialect that declares no modifier keeps its colons" =
  (* x86 has no modifier syntax, so [:] means only what it meant before - a
     label terminator - and a target parser never sees a [Modifier] token. *)
  attempt "x86_64" "\t.text\n\t.globl g\ng:\n\tmovl $:lo12:g, %eax\n";
  [%expect {| parse: unexpected token |}]

(* {1 assemble_many: the M3 exit criterion (§7-§10)}

   Two inputs into one resolved, fixed-address image, with no object files
   and no external linker - through the real DRIVER entry point, not
   Image.plan_image called directly. *)

let assemble_many target sources =
  let (module D : Target_intf.Target.DRIVER) = driver target in
  let sources = List.map (fun (unit_name, text) -> (unit_name, source text)) sources in
  match D.assemble_many sources () with
  | Ok _ -> print_endline "accepted"
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)

let%expect_test "two inputs assemble and link through assemble_many, plain cross-file case" =
  assemble_many "x86_64"
    [
      ("caller", "\t.text\n\t.globl entry\nentry:\n\tcall callee\n\tret\n");
      ("callee", "\t.text\n\t.globl callee\ncallee:\n\tret\n");
    ];
  [%expect {| accepted |}]

(* RISC-V is the mandatory case (M3 §9/§11a): it is the target family with
   real paired-fixup machinery to prove, both the intra-fragment [call]
   sequence (Call_hi20/Call_lo12_i, Sibling_key pairing) and the
   cross-fragment [%pcrel_hi]/[%pcrel_lo] sequence (Pcrel_hi20/
   Pcrel_lo12_i, Anchor_symbol pairing - the anchor is a numeric local
   label, resolved after the merge has already shifted its offset). Both
   referenced symbols are defined only in the *other* input, so this is a
   real cross-module resolution, not merely two inputs that happen not to
   collide. *)
let riscv_two_input_case target =
  assemble_many target
    [
      ( "caller",
        "\t.text\n\
         \t.balign 2\n\
         \t.globl entry\n\
         entry:\n\
         \tcall callee\n\
         1:\tauipc x31, %pcrel_hi(gvar)\n\
         \tlw x6, %pcrel_lo(1b)(x31)\n\
         \tjr x1\n" );
      ( "callee_mod",
        "\t.text\n\
         \t.balign 2\n\
         \t.globl callee\n\
         callee:\n\
         \tjr x1\n\
         \t.data\n\
         \t.balign 4\n\
         \t.globl gvar\n\
         gvar:\n\
         \t.long 99\n" );
    ]

let%expect_test "riscv32: cross-file call and pcrel-hi/lo data reference resolve after the merge" =
  riscv_two_input_case "riscv32";
  [%expect {| accepted |}]

let%expect_test "riscv64: cross-file call and pcrel-hi/lo data reference resolve after the merge" =
  riscv_two_input_case "riscv64";
  [%expect {| accepted |}]

(* M5 corpus evidence (asm/docs/corpus.md - test/regression/charlit.c's
   static [f1]): [%pcrel_hi(sym)]/[%pcrel_lo(sym)] must resolve [sym] as a
   symbol even when its spelling is also a legal register name - here the
   floating register [f1] - because CompCert names statics from the source
   program, register spellings included. Before the fix, [f1] the register
   won the ambiguity and the modifier's own token was left with no operand,
   producing "cannot parse operand %pcrel_hi" on a file that has nothing else
   wrong with it. Checked against a real riscv64-linux-gnu-as: this is valid,
   R_RISCV_PCREL_HI20/LO12_I-relocated syntax, not an invented shape. *)
let riscv_pcrel_register_name_collision target =
  attempt target
    "\t.text\n\
     \t.balign 2\n\
     \t.globl entry\n\
     entry:\n\
     1:\tauipc x31, %pcrel_hi(f1)\n\
     \tlw x6, %pcrel_lo(1b)(x31)\n\
     \tjr x1\n\
     \t.data\n\
     \t.balign 4\n\
     f1:\n\
     \t.long 99\n"

let%expect_test "riscv32: %pcrel_hi/%pcrel_lo resolve a symbol named like a register (f1)" =
  riscv_pcrel_register_name_collision "riscv32";
  [%expect {| accepted |}]

let%expect_test "riscv64: %pcrel_hi/%pcrel_lo resolve a symbol named like a register (f1)" =
  riscv_pcrel_register_name_collision "riscv64";
  [%expect {| accepted |}]

(* x86-only: ARM/AArch64/RISC-V are fixed-width and declare no relaxation
   ladder (contracts.md §5.5), so a cross-input forward-reference test does
   not generalize to them. This pins the short/long selection boundary
   where the branch target is defined in a *different* input than the
   branch - relaxation stays per-contribution (§9), so it can only ever
   pick the short rung when the target is undecidable... which a foreign
   target always is, making the long rung the only safe, and therefore the
   only correct, choice regardless of how close the two inputs end up. *)
let%expect_test "a branch to a same-section symbol in another input always takes the long rung" =
  let (module D : Target_intf.Target.DRIVER) = driver "x86_64" in
  let sources =
    [
      ("a", source "\t.text\n\t.globl entry\nentry:\n\tjmp far\n");
      ("b", source "\t.text\n\t.globl far\nfar:\n\tret\n");
    ]
  in
  (match D.assemble_many sources () with
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        (Foundation.Diag.diagnostics ds)
  | Ok laid_out -> (
      match Image.bind_image laid_out ~addresses:[ (".text", 0x400000L) ] with
      | Error ds ->
          List.iter
            (fun d ->
              Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d)
                (Foundation.Diagnostic.message d))
            (Foundation.Diag.diagnostics ds)
      | Ok img -> (
          match img.Image.segments with
          | s :: _ -> Printf.printf "%s\n" (Foundation.Byte_cursor.to_hex s.Image.bytes)
          | [] -> print_endline "(no segments)")));
  [%expect {| e9 00 00 00 00 c3 |}]

(* {1 Portable manifest (.ai/asm_plan.md M4 Phase 7)}

   [Driver.Portable.manifest] is a pure aggregation over [entry]/[exports]/
   [segments]/[section_bytes] - proven here rather than re-deriving each
   accessor's own coverage, which the tests above already give. What is worth
   a dedicated case is the one thing [manifest] does that no existing test
   does: materialize a NOBITS segment's [zero_fill] into real bytes
   ([section_bytes]'s documented convention, `driver/portable.ml:190-197`),
   over a real three-segment image - code, initialized data, and BSS - so a
   resource caveat stated in a doc comment is also a checked fact. *)
let%expect_test "portable manifest aggregates a multi-segment image, including BSS" =
  let text =
    "\t.text\n\
     \t.globl start\n\
     start:\n\
     \tmovl $1, %eax\n\
     \tret\n\
     \t.data\n\
     g:\n\
     \t.long 42\n\
     \t.bss\n\
     b:\n\
     \t.zero 8\n"
  in
  (match Driver.Portable.assemble ~entry:"start" "x86_64" ~unit_name:"m" ~text with
  | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
  | Ok p -> (
      match Driver.Portable.bind_sequential p ~base:0x400000L ~gap:0 with
      | Error e -> print_endline (Driver.Portable.render_error (Err.Error.kind e))
      | Ok b ->
          let m = Driver.Portable.manifest b in
          Printf.printf "target=%s entry=%s\n" m.Driver.Portable.target
            (match m.Driver.Portable.entry with
            | Some e -> Printf.sprintf "0x%Lx" e
            | None -> "none");
          List.iter
            (fun (s : Driver.Portable.manifest_segment) ->
              Printf.printf "%-6s address=0x%-10Lx bytes=%d %s\n" s.Driver.Portable.name
                s.Driver.Portable.address
                (String.length s.Driver.Portable.bytes)
                (Asm_core.Perms.to_string s.Driver.Portable.perms))
            m.Driver.Portable.segments));
  [%expect
    {|
    target=x86_64 entry=0x400000
    .text  address=0x400000     bytes=6 r-x
    .data  address=0x400006     bytes=4 rw-
    .bss   address=0x40000a     bytes=8 rw-
    |}]

(* {1 AArch64 register-offset addressing, lo12 add, and FP immediates}

   [Disp.t]'s third constructor ([Reg], register-offset addressing) and the
   scalar FP modified-immediate FMOV/FCMP forms. Byte-for-byte hand-verified
   against the real, installed aarch64-linux-gnu-as/objdump before being
   written down here - the same discipline the x86 SSE2 section above used.
   Real objdump output:
     b86a5924  ldr w4, [x9, w10, uxtw #2]
     b86a4924  ldr w4, [x9, w10, uxtw]
     f8616a82  ldr x2, [x20, x1]
     f8617a82  ldr x2, [x20, x1, lsl #3]
     b82ad920  str w0, [x9, w10, sxtw #2]
     38654ac2  ldrb w2, [x22, w5, uxtw]
     f861fa80  ldr x0, [x20, x1, sxtx #3] *)
let%expect_test "register-offset addressing: uxtw/sxtw/sxtx/lsl, scaled and unscaled" =
  disasm "aarch64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tldr w4, [x9, w10, uxtw #2]\n\
     \tldr w4, [x9, w10, uxtw]\n\
     \tldr x2, [x20, x1]\n\
     \tldr x2, [x20, x1, lsl #3]\n\
     \tstr w0, [x9, w10, sxtw #2]\n\
     \tldrb w2, [x22, w5, uxtw]\n\
     \tldr x0, [x20, x1, sxtx #3]\n\
     \tret\n";
  [%expect
    {|
    40000000  24 59 6a b8  ldr w4, [x9, w10, uxtw #2]  [aarch64.ldr32-roff]
    40000004  24 49 6a b8  ldr w4, [x9, w10, uxtw]     [aarch64.ldr32-roff]
    40000008  82 6a 61 f8  ldr x2, [x20, x1]           [aarch64.ldr64-roff]
    4000000c  82 7a 61 f8  ldr x2, [x20, x1, lsl #3]   [aarch64.ldr64-roff]
    40000010  20 d9 2a b8  str w0, [x9, w10, sxtw #2]  [aarch64.str32-roff]
    40000014  c2 4a 65 38  ldrb w2, [x22, w5, uxtw]    [aarch64.ldr8-roff]
    40000018  80 fa 61 f8  ldr x0, [x20, x1, sxtx #3]  [aarch64.ldr64-roff]
    4000001c  c0 03 5f d6  ret                         [aarch64.ret]
    |}]

(* {1 ADD/SUB (extended register)}

   Structurally distinct from ADD/SUB (shifted register) - a different fixed
   bit at 21 where the shifted form has a 2-bit shift kind, and [rd]/[rn] may
   be SP here where the shifted form forbids it. M5 corpus evidence
   (asm/docs/corpus.md): [test/regression/int64.c]'s
   [add x0, x0, x16, uxtx #0]. Byte-for-byte hand-verified against the real,
   installed aarch64-linux-gnu-as/objdump before being written down:
     8b306000  add x0, x0, x16, uxtx
     8b2263e1  add x1, sp, x2
     8b254883  add x3, x4, w5, uxtw #2
     0b2800e6  add w6, w7, w8, uxtb
     cb2bed49  sub x9, x10, x11, sxtx #3
   (GNU's own canonical printer omits an implied [uxtx]/[#0]; this project
   always prints the operand explicitly instead, matching what CompCert and
   this corpus fixture actually write - see {!Lowered.pp}'s [Addsub_extend]
   case.) *)
let%expect_test "ADD/SUB (extended register): uxtb/uxtw/uxtx/sxtx, and rd/rn = sp" =
  disasm "aarch64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tadd x0, x0, x16, uxtx #0\n\
     \tadd x1, sp, x2, uxtx #0\n\
     \tadd x3, x4, w5, uxtw #2\n\
     \tadd w6, w7, w8, uxtb #0\n\
     \tsub x9, x10, x11, sxtx #3\n\
     \tret\n";
  [%expect
    {|
    40000000  00 60 30 8b  add x0, x0, x16, uxtx #0   [aarch64.addsub-extend]
    40000004  e1 63 22 8b  add x1, sp, x2, uxtx #0    [aarch64.addsub-extend]
    40000008  83 48 25 8b  add x3, x4, w5, uxtw #2    [aarch64.addsub-extend]
    4000000c  e6 00 28 0b  add w6, w7, w8, uxtb #0    [aarch64.addsub-extend]
    40000010  49 ed 2b cb  sub x9, x10, x11, sxtx #3  [aarch64.addsub-extend]
    40000014  c0 03 5f d6  ret                        [aarch64.ret]
    |}]

(* [[x20, x1]] with no extend token at all and [[x20, x1, lsl #0]] with an
   explicit unscaled [lsl #0] assemble to the same word on real [as]
   ([f8616a82] both - verified directly, distinct from the [lsl #3] case
   above which is [f8617a82]). This test caught a real encoder bug during
   this pass: [ldst_roff_alt]'s [encode] set S=1 for *any* [Some _] amount,
   including an explicit [Some 0], so [lsl #0] on a doubleword access
   encoded identically to [lsl #3] instead of to the bare form. Fixed by
   treating [Some 0] as S=0 alongside [None] - only a nonzero explicit
   amount is S=1. *)
let%expect_test "a bare two-register offset and an explicit lsl #0 are the same encoding" =
  disasm "aarch64"
    "\t.text\n\t.globl f\nf:\n\tldr x2, [x20, x1]\n\tldr x2, [x20, x1, lsl #0]\n\tret\n";
  [%expect
    {|
    40000000  82 6a 61 f8  ldr x2, [x20, x1]  [aarch64.ldr64-roff]
    40000004  82 6a 61 f8  ldr x2, [x20, x1]  [aarch64.ldr64-roff]
    40000008  c0 03 5f d6  ret                [aarch64.ret]
    |}]

(* stp has no register-offset addressing form at all - real [as] rejects
   [stp x0, x1, [x9, x10]] outright ("error: invalid addressing mode"). Our
   own rejection reuses [`Stp_symbolic_offset] rather than adding a fourth
   error case, exactly as the [Disp.Sym] arm beside it already does. *)
let%expect_test "stp has no register-offset addressing form" =
  attempt "aarch64" "\t.text\n\tstp x0, x1, [x9, x10]\n";
  [%expect {| aarch64.lower: stp takes a numeric offset |}]

(* Real objdump output for the four immediates below:
     1e602008  fcmp d0, #0.0
     1e202028  fcmp s1, #0.0
     1e6e1000  fmov d0, #1.000000000000000000e+00
     1e2e1000  fmov s0, #1.000000000000000000e+00
     1e7e1002  fmov d2, #-1.000000000000000000e+00
     1e2c1003  fmov s3, #5.000000000000000000e-01 *)
let%expect_test "fcmp #0.0 and fmov modified-immediate forms, single and double" =
  disasm "aarch64"
    "\t.text\n\
     \t.globl f\n\
     f:\n\
     \tfcmp d0, #0.0\n\
     \tfcmp s1, #0.0\n\
     \tfmov d0, #1.0\n\
     \tfmov s0, #1.0\n\
     \tfmov d2, #-1.0\n\
     \tfmov s3, #0.5\n\
     \tret\n";
  [%expect
    {|
    40000000  08 20 60 1e  fcmp d0, #0.000000000000000000e+00   [aarch64.fcmp-imm0-d]
    40000004  28 20 20 1e  fcmp s1, #0.000000000000000000e+00   [aarch64.fcmp-imm0-s]
    40000008  00 10 6e 1e  fmov d0, #1.000000000000000000e+00   [aarch64.fmov-imm-d]
    4000000c  00 10 2e 1e  fmov s0, #1.000000000000000000e+00   [aarch64.fmov-imm-s]
    40000010  02 10 7e 1e  fmov d2, #-1.000000000000000000e+00  [aarch64.fmov-imm-d]
    40000014  03 10 2c 1e  fmov s3, #5.000000000000000000e-01   [aarch64.fmov-imm-s]
    40000018  c0 03 5f d6  ret                                  [aarch64.ret]
    |}]

(* fcmp's immediate operand must be exactly #0.0 - any other literal is a
   different, unimplemented instruction shape (verified against real [as]:
   [fcmp d0, #1.0] is rejected there too, "error: FP compare register expected"
   in a form gas does support elsewhere, but not as a modified-immediate). *)
let%expect_test "fcmp rejects a nonzero immediate" =
  attempt "aarch64" "\t.text\n\tfcmp d0, #1.0\n";
  [%expect
    {| aarch64.lower: fcmp's immediate operand must be #0.0, not #1.000000000000000000e+00 |}]

(* [add x9, x9, #:lo12:sym] - the ADD low-12 relocation form (M5 corpus
   evidence). Real [as]/[objdump] on the un-relocated instruction word alone
   (the low-12 field itself is R_AARCH64_ADD_ABS_LO12_NC, resolved by the
   linker, so it reads 0 in the object file):
     91000129  add x9, x9, #0x0
       0: R_AARCH64_ADD_ABS_LO12_NC .data
   [disasm] here (like the rest of this file) binds every segment at the same
   address, so a second, nonempty section would overlap [.text] rather than
   sit after it; [sym] is defined in [.text] itself to keep this a single-
   segment program (see "a base-less SIB..." and the RIP-relative test above
   for the same constraint on x86). [sym]'s bound address is 0x40000008, so
   the low-12 field the real relocation would leave as 0 instead comes out as
   8 (word-encoded as [imm12 = 8] at bits [21:10] - [0x91000129 + (8 << 10) =
   0x91002129], matching the fixed opcode/rd/rn bits real [as] produced
   above.) *)
let%expect_test "add ..., #:lo12:sym" =
  disasm "aarch64" "\t.text\n\t.globl f\nf:\n\tadd x9, x9, #:lo12:sym\n\tret\nsym:\n\t.word 0\n";
  [%expect
    {|
    40000000  29 21 00 91  add x9, x9, #8  [aarch64.add-imm]
    40000004  c0 03 5f d6  ret             [aarch64.ret]
    40000008  00 00 00 00  udf #0          [aarch64.udf]
    |}]
