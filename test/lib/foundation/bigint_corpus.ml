(* A fixed Bigint corpus, printed rather than asserted.

   This module has two consumers and deliberately belongs to neither. The
   expect test in test/expect pins each section's output with [%expect]; the
   portable driver in test/unit prints all of them and is compiled under
   js_of_ocaml and Melange for the three-build equality gate. Keeping the corpus
   here means the two never drift — ppx_expect cannot run under Melange, so
   without a shared module the same values would have to be written twice.

   Consequently nothing in this file may use a ppx, Unix, the filesystem, or
   process state: it has to build under all three backends. It also includes
   values well beyond 2^53, where a JavaScript number stops holding integers
   exactly, since that is exactly what the equality gate is looking for. *)

open Foundation

let section title = Fmt.pr "# %s@." title
let repr label x = Fmt.pr "@[<v 2>%s =@,%a@]@." label Bigint.pp_repr x
let value label x = Fmt.pr "%s = %a (%a)@." label Bigint.pp x Bigint.pp_hex x
let b = Bigint.of_string_exn

(* Both result-returning entry points print through the same combinator, so an
   [Ok] and an [Error] can never drift into differently-shaped output.

   The error branch renders the typed payload with the domain's own printer, and
   deliberately not the Err wrapper: this corpus is the seed of the cram baseline
   and the three-backend equality gate, and asm/docs/errors.md §3 keeps
   provenance out of both. *)
let pp_error ppf e = Bigint.pp_error ppf (Err.Error.kind e)
let pp_result ok = Fmt.result ~ok ~error:Fmt.(any "error: " ++ pp_error)

(* {1 Representation invariants} *)

let representation () =
  section "representation";
  repr "zero" Bigint.zero;
  repr "one" Bigint.one;
  repr "minus_one" Bigint.minus_one;
  (* Exactly one limb wide, then one bit into the second limb: the base-2^14
     boundary is where a limb-carry bug would first show. *)
  repr "2^14 - 1" (Bigint.sub (Bigint.shift_left_exn Bigint.one 14) Bigint.one);
  repr "2^14" (Bigint.shift_left_exn Bigint.one 14);
  (* A subtraction whose result strips a most-significant limb back off. *)
  repr "2^14 - (2^14 - 1)"
    (Bigint.sub
       (Bigint.shift_left_exn Bigint.one 14)
       (Bigint.sub (Bigint.shift_left_exn Bigint.one 14) Bigint.one));
  (* x - x must land on canonical zero, sign and all, not on a signed empty
     magnitude or a magnitude of zero limbs. *)
  repr "big - big" (Bigint.sub (b "0xdeadbeefcafef00d") (b "0xdeadbeefcafef00d"));
  (* [of_int] on the most negative value it can be handed is the interesting
     case — it has no positive counterpart, so the conversion has to accumulate
     from the negative side. It cannot be spelled [min_int] here: that is
     host-dependent (-2^62 on 64-bit native OCaml, -2^30 on the 31-bit i386 and
     arm/v7 legs, -2^31 under js_of_ocaml and Melange), and this output is
     required to be byte-identical across all of them. -2^30 is representable
     everywhere, so it is the widest value this corpus can pin; the genuine
     [min_int] round-trip stays in bigint_laws.ml, where it is compared against
     [string_of_int] and is therefore correct on any host. *)
  repr "of_int (-2^30)" (Bigint.of_int (-1073741824));
  (* A magnitude several limbs wide, spelled as text so it is host-independent. *)
  repr "-2^62" (b "-4611686018427387904")

(* {1 Parsing}

   Every accepted spelling, and the exact text of every rejection — error
   messages are user-visible, so they are part of the contract. *)

let parsing () =
  section "parsing";
  let parse s = Fmt.pr "%-24S -> %a@." s (pp_result Bigint.pp) (Bigint.of_string s) in
  List.iter parse
    [
      "0";
      "-0";
      "42";
      "-42";
      "+42";
      "0x2a";
      "0X2A";
      "0b101010";
      "052";
      "12345678901234567890123456789012345678901234567890";
      "-12345678901234567890123456789012345678901234567890";
      "";
      "-";
      "0x";
      "0b12";
      "099";
      "12a";
      "1_000";
    ]

let rendering () =
  section "rendering";
  List.iter
    (fun s -> value s (b s))
    [ "0"; "42"; "-42"; "255"; "-255"; "65536"; "-1"; "0x123456789abcdef0123456789abcdef0" ]

(* {1 Values beyond 2^53}

   The three-build gate lives or dies here: a JavaScript number holds integers
   exactly only below 2^53, so any accidental reliance on float arithmetic in
   the jsoo or Melange backend shows up as a diff in this section. *)

let beyond_2_53 () =
  section "beyond 2^53";
  let p53 = Bigint.shift_left_exn Bigint.one 53 in
  value "2^53" p53;
  value "2^53 + 1" (Bigint.add p53 Bigint.one);
  value "2^64 - 1" (Bigint.sub (Bigint.shift_left_exn Bigint.one 64) Bigint.one);
  value "2^127" (Bigint.shift_left_exn Bigint.one 127);
  value "(2^64 - 1) * (2^64 - 1)"
    (let m = Bigint.sub (Bigint.shift_left_exn Bigint.one 64) Bigint.one in
     Bigint.mul m m);
  repr "2^53 + 1" (Bigint.add p53 Bigint.one)

(* [to_bits] is deliberately not here. Its cases are a width/value table whose
   whole interest is which entries fail and with what message, so it is written
   inline in test/expect/test_bigint_expect.ml next to its output. Nothing in it
   is arithmetic that a JavaScript backend could get wrong differently from the
   sections above, so the three-build gate loses nothing. *)

let all () =
  List.iteri
    (fun i f ->
      if i > 0 then Fmt.pr "@.";
      f ())
    [ representation; parsing; rendering; beyond_2_53 ]
