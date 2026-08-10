(* Expect tests for the codec EDSL, driven entirely by the synthetic ISA.

   No target is linked here, which is M0's exit criterion for this package
   stated as a build fact rather than as an intention. *)

open Codec
open Codec_corpus.Synthetic_isa

let show_bits b = Bits.to_string b
let hex b = Foundation.Byte_cursor.to_hex (Bits.to_bytes b)

let pp_placement ppf (p : _ placement) =
  Fmt.pf ppf "  fixup %s:%s@%d+%d" p.name (fixup_name p.kind) p.bit_offset p.bit_width

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
  enc Jmp;
  [%expect
    {|
    nop                0000000000000000  00 00  form=nop
    add r1, r6         0001001110000000  13 80  form=add
    li r2, 5           0100010010100000  44 a0  form=li.short
    li r2, 200         0010010110010000  25 90  form=li.long
    jmp <target>       0011000000000000  30 00  form=jmp         fixup target:rel12@4+12 |}]

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
  round Jmp;
  (* An opcode no form claims. *)
  dec (Bits.of_int64 ~width:16 0xF000L);
  [%expect
    {|
    0000000000000000   nop (16 bits) form=nop
    0001001110000000   add r1, r6 (16 bits) form=add
    0100010010100000   li r2, 5 (16 bits) form=li.short
    0010010110010000   li r2, 200 (16 bits) form=li.long
    0011000000000000   jmp <target> (16 bits) form=jmp
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
  Fmt.pr "@[<v>%a@]@." Shape.pp (inspect isa);
  [%expect
    {|
    alt synthetic
      [0 cost=0] nop              nop(){0000000000000000}
      [1 cost=1] add              add(){0001 reg reg 000000}
      [2 cost=1] li.short         li.short(){0100 reg imm:4u 00000}
      [3 cost=2] li.long          li.long(){0010 reg imm:8u 0}
      [4 cost=1] jmp              jmp(){0011 <target:12>}
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
  Fmt.pr "@[<v>%a@]@." Fmt.(list ~sep:cut pp_problem) (check isa);
  Fmt.pr "problems: %d@." (List.length (check isa));
  [%expect {| problems: 0 |}]

let%expect_test "check rejects a broken table" =
  (* Injectivity in both directions and totality against the field width. A
     duplicate code would make decode pick arbitrarily between two registers. *)
  Fmt.pr "@[<v>%a@]@." Fmt.(list ~sep:cut pp_problem) (check broken_table);
  [%expect
    {|
    iso_table broken.dup: duplicate code 1
    iso_table broken.dup: code 99 for r3 does not fit 3 bits |}]

let%expect_test "check rejects ambiguity and duplicate priority" =
  Fmt.pr "@[<v>%a@]@." Fmt.(list ~sep:cut pp_problem) (check broken_alt);
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
      [ Nop; Add (R1, R6); Li (R0, 0L); Li (R0, 15L); Li (R0, 16L); Li (R0, 255L); Jmp ]
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
