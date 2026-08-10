(* Expect tests for the codec EDSL, driven entirely by the synthetic ISA.

   No target is linked here, which is M0's exit criterion for this package
   stated as a build fact rather than as an intention. *)

open Codec
open Codec_corpus.Synthetic_isa

let show_bits b = Bits.to_string b
let hex b = Foundation.Byte_cursor.to_hex (Bits.to_bytes b)

let pp_placement ppf (p : _ placement) =
  Fmt.pf ppf "  fixup %s:%s %a" p.name (fixup_name p.kind)
    Fmt.(
      list ~sep:(any ",") (fun ppf (s : Codec.slice) ->
          Fmt.pf ppf "%d+%d@%d" s.Codec.bit_offset s.Codec.bit_width s.Codec.value_lsb))
    p.slices

let pp_encoded ppf (e : _ encoded) =
  Fmt.pf ppf "%s  %-6s form=%-10s%a" (show_bits e.bits) (hex e.bits) (form_id e)
    Fmt.(list ~sep:nop pp_placement)
    e.placements

(* Both outcomes print through one combinator, so a success and a failure can
   never drift into differently-shaped output. *)
let pp_encode_result = Fmt.result ~ok:pp_encoded ~error:Fmt.(any "ERROR " ++ pp_error)
let enc insn = Fmt.pr "%-18s %a@." (show_insn insn) pp_encode_result (encode isa insn)

let dec bits =
  match decode_bits isa bits with
  | None -> Fmt.pr "%-18s no decode@." (show_bits bits)
  | Some d ->
      Fmt.pr "%-18s %s (%d bits) form=%s@." (show_bits bits) (show_insn d.value) d.consumed
        (String.concat "." d.dform)

(* {1 Encode} *)

let%expect_test "encode" =
  enc Nop;
  enc (Add (R1, R6));
  (* The short form's guard admits this one and the long form does not get a
     chance; the priorities are what make that deterministic rather than a
     consequence of declaration order. *)
  enc (Li (R2, 5L));
  enc (Li (R2, 200L));
  enc (Jmp 0L);
  [%expect
    {|
    nop                0000000000000000  00 00  form=nop
    add r1, r6         0001001110000000  13 80  form=add
    li r2, 5           0100010010100000  44 a0  form=li.short
    li r2, 200         0010010110010000  25 90  form=li.long
    jmp 0              0011000000000000  30 00  form=jmp         fixup target:rel12 4+12@0 |}]

let%expect_test "encode out of range" =
  (* No alternative applies: the short form's guard rejects it and the long
     form's 8-bit field cannot hold it. The failure names the field, not the
     alternative, because that is the fact the author needs. *)
  enc (Li (R0, 300L));
  enc (Li (R0, -1L));
  [%expect
    {|
    li r0, 300         ERROR field imm: 300 does not fit 8 bits unsigned
    li r0, -1          ERROR field imm: -1 does not fit 8 bits unsigned |}]

(* {1 Decode - the same tree, read backwards} *)

let%expect_test "decode" =
  let round insn =
    match encode isa insn with
    | Error _ -> Fmt.pr "%-18s did not encode@." (show_insn insn)
    | Ok { bits; _ } -> dec bits
  in
  round Nop;
  round (Add (R1, R6));
  round (Li (R2, 5L));
  round (Li (R2, 200L));
  round (Jmp 0L);
  (* An opcode no form claims. *)
  dec (Bits.of_int64 ~width:16 0xF000L);
  [%expect
    {|
    0000000000000000   nop (16 bits) form=nop
    0001001110000000   add r1, r6 (16 bits) form=add
    0100010010100000   li r2, 5 (16 bits) form=li.short
    0010010110010000   li r2, 200 (16 bits) form=li.long
    0011000000000000   jmp 0 (16 bits) form=jmp
    1111000000000000   no decode |}]

(* A decoded [Li] cannot tell which encoding it came from, which is the point of
   §4.9's three levels: syntax, semantic instruction, exact form. The *bits*
   differ and the semantic instruction does not. *)
let%expect_test "two encodings, one semantic instruction" =
  let bits_of insn = match encode isa insn with Ok { bits; _ } -> bits | Error _ -> "" in
  let short = bits_of (Li (R2, 5L)) in
  let long = Bits.of_int64 ~width:16 0b0010_010_00000101_0L in
  Fmt.pr "short %s@." short;
  Fmt.pr "long  %s@." long;
  dec short;
  dec long;
  [%expect
    {|
    short 0100010010100000
    long  0010010000001010
    0100010010100000   li r2, 5 (16 bits) form=li.short
    0010010000001010   li r2, 5 (16 bits) form=li.long |}]

(* {1 Inspect} *)

let%expect_test "inspect" =
  Fmt.pr "@[<v>%a@]@." Shape.pp (inspect ~kind_name:fixup_name isa);
  [%expect
    {|
    alt synthetic
      [0 cost=0] nop              nop(){0000000000000000}
      [1 cost=1] add              add(){0001 reg reg 000000}
      [2 cost=1] li.short         li.short(){0100 reg imm:4u 00000}
      [3 cost=2] li.long          li.long(){0010 reg imm:8u 0}
      [4 cost=1] jmp              jmp(){0011 <target:12@0 rel12>}
    reg[8]{r:3u} |}]

let%expect_test "widths" =
  let w name node =
    Fmt.pr "%-10s %s@." name (match node with Some n -> string_of_int n | None -> "variable")
  in
  w "isa" (width_of isa);
  w "add" (width_of form_add);
  w "modimm" (width_of modimm);
  [%expect {|
    isa        16
    add        16
    modimm     6 |}]

(* {1 Check - and what it deliberately cannot say} *)

let%expect_test "check accepts the ISA" =
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name isa);
  Fmt.pr "problems: %d@." (List.length (check ~equal_kind:( = ) ~kind_name:fixup_name isa));
  [%expect {| problems: 0 |}]

let%expect_test "check rejects a broken table" =
  (* Injectivity in both directions and totality against the field width. A
     duplicate code would make decode pick arbitrarily between two registers. *)
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name broken_table);
  [%expect
    {|
    iso_table broken.dup: duplicate code 1
    iso_table broken.dup: code 99 for r3 does not fit 3 bits |}]

let%expect_test "check rejects ambiguity and duplicate priority" =
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name broken_alt);
  [%expect
    {|
    alt broken.ambiguous: duplicate priority 1
    alt broken.ambiguous: a and b have overlapping fixed bits; priority 0 before 1 decides |}]

(* {1 Generated finite-domain tests}

   The two things [check] cannot decide. [modimm] is an [Iso_fun], so its laws
   are not statically checkable at all - the round trip over an enumerated
   domain is the only evidence it ever gets - and an [Alt]'s guards are
   arbitrary predicates, so reachability is decided by running them. *)

let%expect_test "iso_fun round trip" =
  Fmt.pr "@[<v>%a@]@." Finite.pp_domain modimm_domain;
  let failures = Finite.round_trip modimm modimm_domain ~equal:Int64.equal ~show:Int64.to_string in
  Fmt.pr "failures: %d@." (List.length failures);
  List.iter (fun f -> Fmt.pr "  %s@." f) failures;
  [%expect
    {|
    modimm: 52 values, exhaustive
      why: the representable set is the image of a 2-bit rotation over a 4-bit immediate, so enumerating the 64 (rot, imm4) pairs enumerates every value there is
    failures: 0 |}]

let%expect_test "guard reachability" =
  (* Every alternative must be selected by something. A short-immediate form
     whose guard no value satisfies is dead code that reads exactly like a
     working optimisation. *)
  let domain =
    Finite.exhaustive ~name:"insn"
      ~why:
        "one instance of every constructor, with immediates chosen on both sides of the short \
         form's range boundary - the only thing any guard in this ISA branches on"
      [ Nop; Add (R1, R6); Li (R0, 0L); Li (R0, 15L); Li (R0, 16L); Li (R0, 255L); Jmp 0L ]
  in
  let dead = Finite.unreachable_alts isa domain in
  Fmt.pr "unreachable: %d@." (List.length dead);
  List.iter (fun d -> Fmt.pr "  %s@." d) dead;
  [%expect {|
    unreachable: 0 |}]

let%expect_test "guard reachability can fail" =
  (* The same check against a domain that never reaches the long form. This is
     the control: a reachability report that cannot report anything proves
     nothing about the report above. *)
  let domain =
    Finite.sample ~name:"insn.short-only"
      ~why:"deliberately omits any value the long form would encode"
      [ Nop; Li (R0, 1L) ]
  in
  List.iter (fun d -> Fmt.pr "  %s@." d) (Finite.unreachable_alts isa domain);
  [%expect
    {|
    alt synthetic: add is selected by no value in domain insn.short-only (sample)
    alt synthetic: li.long is selected by no value in domain insn.short-only (sample)
    alt synthetic: jmp is selected by no value in domain insn.short-only (sample) |}]

(* {1 Relaxation ladders}

   The three things a ladder has to do that an [Alt] cannot: hand back every
   rung at once, be told which rung to use, and let a decoded encoding say which
   rung produced it. *)

let show_ladder v =
  match encode_ladder ladder v with
  | Error e -> Fmt.pr "ERROR %a@." pp_error e
  | Ok forms ->
      List.iter (fun (e : _ encoded) -> Fmt.pr "  %-8s %s@." (form_id e) (hex e.bits)) forms

let%expect_test "a ladder yields every rung, not just the first" =
  (* This is the whole difference from [Alt], whose encoder returns on first
     success: both rungs apply to the same value, and layout - which knows where
     the target landed - is what chooses between them. *)
  show_ladder (Br 4L);
  [%expect {|
      rel8     eb 04
      rel16    e9 00 04 |}]

let%expect_test "a rung can be selected by name" =
  let one r =
    match encode_rung ladder ~rung:r (Br 4L) with
    | Ok e -> Fmt.pr "  %-8s %s@." (form_id e) (hex e.bits)
    | Error e -> Fmt.pr "  %-8s ERROR %a@." r pp_error e
  in
  one "rel8";
  one "rel16";
  (* An unknown pin is a diagnostic, never a quiet fall back to another rung:
     the entire point of a pin is that it is obeyed. *)
  one "rel99";
  [%expect
    {|
      rel8     eb 04
      rel16    e9 00 04
      rel99    ERROR encode_rung: rung rel99 is not among {rel8, rel16} |}]

let%expect_test "a decoded encoding reports its rung" =
  (* Which is only recoverable because the rungs have distinct fixed bits. If
     they overlapped, the first would always match and canonical disassembly
     would silently re-encode a near branch as short. *)
  let show bits =
    match decode_bits ladder bits with
    | None -> Fmt.pr "  no decode@."
    | Some d ->
        Fmt.pr "  %-8s Br %s@." (String.concat "." d.dform)
          (match d.value with Br v -> Int64.to_string v)
  in
  show (Bits.of_bytes "\xeb\x04");
  show (Bits.of_bytes "\xe9\x00\x04");
  [%expect {|
      rel8     Br 4
      rel16    Br 4 |}]

let%expect_test "a ladder has no single width, and check accepts it" =
  Fmt.pr "width: %s@." (match width_of ladder with Some n -> string_of_int n | None -> "variable");
  Fmt.pr "problems: %d@." (List.length (check ~equal_kind:( = ) ~kind_name:fixup_name ladder));
  [%expect {|
    width: variable
    problems: 0 |}]

let%expect_test "a buried ladder yields every rung, with its surroundings intact" =
  (* The top-level [ladder] above is the easy case. Here the rungs sit under an
     [Alt], an [Iso_fun] and two [Seq]s - which is where a real one lives - and
     each result has to be a *complete* encoding: the register operand ahead of
     the ladder, the trailing opcode bits behind it, the alternative's label at
     the head of the form path, and placements shifted to their real bit
     offsets rather than the rung's local ones. *)
  (match encode_ladder buried_ladder (Buried (R5, 4L)) with
  | Error e -> Fmt.pr "ERROR %a@." pp_error e
  | Ok forms ->
      List.iter
        (fun (e : _ encoded) ->
          Fmt.pr "  %-12s %s  %s@." (form_id e) (hex e.bits)
            (String.concat " "
               (List.map
                  (fun p ->
                    Printf.sprintf "%s@%s" p.name
                      (String.concat "+"
                         (List.map
                            (fun s -> Printf.sprintf "%d:%d" s.bit_offset s.bit_width)
                            p.slices)))
                  e.placements)))
        forms);
  [%expect
    {|
      br.rel8      bd 60 96  target@11:8
      br.rel16     bd 20 00 96  target@11:16 |}]

let%expect_test "check rejects two ladders on one realized path" =
  (* Distinct from the nesting rule: neither ladder is inside the other, so the
     rung-level check sees nothing wrong. What is wrong is that the form's rungs
     would be a product of two independent choices. *)
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name broken_two_ladders);
  [%expect
    {| seq: two relaxation ladders can be realized on one path; a form may contain at most one |}]

let%expect_test "check rejects rungs that cannot be told apart" =
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name broken_relax);
  [%expect
    {|
    relax broken.rungs: rung a (16 bits) is not shorter than b (16 bits)
    relax broken.rungs: rungs a and b have overlapping fixed bits, so a decoded form cannot say which one produced it |}]

(* {1 Split fixups}

   Two slices of one logical value at non-adjacent positions. The codec sees
   only pieces; the [Iso_fun] reassembles them. *)

let%expect_test "a split fixup merges into one placement" =
  (match encode split_hi_lo (Split 0xABCL) with
  | Error e -> Fmt.pr "ERROR %a@." pp_error e
  | Ok e ->
      Fmt.pr "%s  %s@." (show_bits e.bits) (hex e.bits);
      List.iter (fun p -> Fmt.pr "%a@." pp_placement p) e.placements);
  [%expect
    {|
    00001010101110101100000000000000  0a ba c0 00
      fixup sym:abs16 0+12@4,16+4@0 |}]

let%expect_test "a split fixup round-trips through its slices" =
  let round v =
    match encode split_hi_lo (Split v) with
    | Error e -> Fmt.pr "  %Ld: ERROR %a@." v pp_error e
    | Ok e -> (
        match decode_bits split_hi_lo e.bits with
        | None -> Fmt.pr "  %Ld: no decode@." v
        | Some d -> Fmt.pr "  %Ld -> %Ld@." v (match d.value with Split x -> x))
  in
  List.iter round [ 0L; 1L; 0xFL; 0x10L; 0xABCL; 0xFFFFL ];
  [%expect
    {|
      0 -> 0
      1 -> 1
      15 -> 15
      16 -> 16
      2748 -> 2748
      65535 -> 65535 |}]

let%expect_test "check rejects overlapping slices and mixed kinds" =
  (* Both faults in one description: the two slices claim value bits 0..3
     twice, and disagree about the kind. Comparing kinds by *equality* rather
     than by printed name is what makes the second detectable at all. *)
  Fmt.pr "@[<v>%a@]@."
    Fmt.(list ~sep:cut pp_problem)
    (check ~equal_kind:( = ) ~kind_name:fixup_name broken_fixup);
  [%expect
    {|
    seq: fixup s mixes kinds rel12 and abs16
    seq: fixup s has slices overlapping at value bit 0 |}]
