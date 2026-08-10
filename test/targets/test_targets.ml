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
  [%expect {|
    x86_32: none
    x86_64: none
    arm: none
    aarch64: none
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

(* {1 M1.4 - the restricted linker}

   One module, one allocatable section, one strong exported symbol, no
   unresolved references, no relaxation. Every unsupported condition is an
   explicit rejection with its own message, because "unsupported" and "wrong"
   must not produce the same diagnostic: a caller that hits the one-section
   limit needs to know it is a limit. *)

let attempt target text =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with Some d -> d | None -> failwith target
  in
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match D.assemble ~unit_name:"t" ~source with
  | Ok _ -> print_endline "accepted"
  | Error ds ->
      List.iter
        (fun d ->
          Printf.printf "%s: %s\n" (Foundation.Diagnostic.code d) (Foundation.Diagnostic.message d))
        ds

let%expect_test "a second allocatable section is rejected, a declared one is not" =
  attempt "x86_64" "\t.text\n\tret\n\t.section .note.GNU-stack,\"\",@progbits\n";
  attempt "x86_64" "\t.text\n\tret\n\t.data\n";
  [%expect
    {|
    accepted
    image.multi-section: M1 links exactly one allocatable section; declared-but-unallocated sections such as .note.GNU-stack do not count and multiple real sections are M3
    |}]

let%expect_test "an unknown directive is a diagnostic, never a silent skip" =
  attempt "x86_64" "\t.text\n\t.quad 1\n\tret\n";
  [%expect {| simplify.directive: unknown directive .quad |}]

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
  [%expect {| aarch64.lower: ldr offset must be a multiple of eight for a 64-bit access |}]

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
  match D.assemble ~unit_name:"t" ~source with
  | Error _ -> print_endline "unexpected: did not assemble"
  | Ok l ->
      (let plan = Image.plan_of l in
       let at base =
         List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, base)) plan.Image.segments
       in
       (match Image.bind_image l ~addresses:(at 0x40000002L) with
       | Ok _ -> print_endline "accepted a misaligned address"
       | Error ds -> List.iter (fun d -> print_endline (Foundation.Diagnostic.message d)) ds);
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
        ds

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
let%expect_test "numeric local labels are rejected, not silently renamed" =
  attempt "x86_64" "\t.text\n1:\n\tret\n";
  [%expect
    {| parse.local-label: numeric local label 1: needs 1b/1f resolution, which is not in M1 scope |}]
