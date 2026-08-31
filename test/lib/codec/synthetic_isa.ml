(* A fictional 16-bit ISA, described entirely in the generic codec EDSL.

   M0's exit criteria require the codec library to pass tests "with no target
   linked", and this is what makes that a real constraint rather than a
   statement about the dune graph. If the EDSL had quietly acquired x86 or ARM
   vocabulary, a machine that resembles neither could not be described in it.

   It is also deliberately built the way a real target is - a top-level [Alt]
   over per-form branches, each an [Iso_fun] projecting one constructor of the
   instruction type, guarded by that constructor - so the shape the four M1
   targets use is exercised before any of them exist. *)

open Codec

(* {1 Fixup kinds}

   The tree is parameterized over this type. Having a synthetic ISA that
   actually uses a fixup is what keeps the parameter live: a type variable no
   description instantiates is indistinguishable from one that does not work.

   Declared before the first codec value, and not merely for tidiness: a codec
   built by function application gets a weakly polymorphic fixup-kind variable,
   and OCaml refuses to later unify one of those with a type declared after it
   was created ("would escape its scope"). *)

type fixup_kind = Rel12 | Abs16

let fixup_name = function Rel12 -> "rel12" | Abs16 -> "abs16"

(* {1 Registers} *)

type reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7

let all_regs = [ R0; R1; R2; R3; R4; R5; R6; R7 ]

let reg_name = function
  | R0 -> "r0"
  | R1 -> "r1"
  | R2 -> "r2"
  | R3 -> "r3"
  | R4 -> "r4"
  | R5 -> "r5"
  | R6 -> "r6"
  | R7 -> "r7"

(* A finite relation, so its injectivity and totality are decided by [check]
   rather than asserted by a comment. *)
let reg_codec =
  iso_table ~name:"reg" ~equal:( = ) ~show:reg_name
    ~entries:(List.mapi (fun i r -> (r, Int64.of_int i)) all_regs)
    (field ~width:3 "r")

(* {1 Instructions} *)

type insn =
  | Nop
  | Add of reg * reg
  | Li of reg * int64  (** load immediate; two encodings, chosen by range *)
  | Jmp of int64  (** the displacement: 0 while symbolic, whatever the bits held once decoded *)

let show_insn = function
  | Nop -> "nop"
  | Add (a, b) -> Printf.sprintf "add %s, %s" (reg_name a) (reg_name b)
  | Li (r, v) -> Printf.sprintf "li %s, %Ld" (reg_name r) v
  | Jmp d -> Printf.sprintf "jmp %Ld" d

(* Each form is described as its bit layout, then lifted to [insn] by an
   [Iso_fun] that projects the one constructor it encodes. The projection
   returning [None] for every other constructor is what makes the alternative's
   guard and its body agree by construction. *)

let form_nop =
  iso_fun ~name:"nop"
    ~encode:(function Nop -> Some () | _ -> None)
    ~decode:(fun () -> Some Nop)
    (const ~width:16 0L)

let form_add =
  iso_fun ~name:"add"
    ~encode:(function Add (d, s) -> Some ((), (d, (s, ()))) | _ -> None)
    ~decode:(fun ((), (d, (s, ()))) -> Some (Add (d, s)))
    (const ~width:4 0b0001L ** reg_codec ** reg_codec ** const ~width:6 0L)

let form_li_short =
  iso_fun ~name:"li.short"
    ~encode:(function Li (d, v) -> Some ((), (d, (v, ()))) | _ -> None)
    ~decode:(fun ((), (d, (v, ()))) -> Some (Li (d, v)))
    (const ~width:4 0b0100L ** reg_codec ** field ~width:4 "imm" ** const ~width:5 0L)

let form_li_long =
  iso_fun ~name:"li.long"
    ~encode:(function Li (d, v) -> Some ((), (d, (v, ()))) | _ -> None)
    ~decode:(fun ((), (d, (v, ()))) -> Some (Li (d, v)))
    (const ~width:4 0b0010L ** reg_codec ** field ~width:8 "imm" ** const ~width:1 0L)

(* [Jmp] carries its displacement now that a fixup decodes a value rather than
   [unit]: a symbolic jump writes the placeholder 0, and a decoded one reports
   whatever the bits held. *)
let form_jmp =
  iso_fun ~name:"jmp"
    ~encode:(function Jmp d -> Some ((), d) | _ -> None)
    ~decode:(fun ((), d) -> Some (Jmp d))
    (const ~width:4 0b0011L ** fixup ~width:12 ~kind:Rel12 "target")

(* The whole ISA. Priorities are what make selection deterministic on both
   sides: the short load-immediate is tried first on encode and matched first on
   decode, and its guard is the range that makes it applicable. *)
let isa : (insn, fixup_kind) t =
  choice ~name:"synthetic"
    [
      alt ~label:"nop" ~priority:0 form_nop;
      alt ~label:"add" ~priority:1 ~cost:1 form_add;
      alt ~label:"li.short" ~priority:2 ~cost:1
        ~guard:(function
          | Li (_, v) -> Int64.compare v 0L >= 0 && Int64.compare v 16L < 0 | _ -> false)
        form_li_short;
      alt ~label:"li.long" ~priority:3 ~cost:2 form_li_long;
      alt ~label:"jmp" ~priority:4 ~cost:1 form_jmp;
    ]

(* {1 A rotated immediate, for the escape hatch}

   Modelled on ARM's modified immediates, which are the M1 reason [Iso_fun]
   exists at all: the representable set is not a contiguous range and not a
   table anyone would want to write out, but it *is* finite and enumerable, so
   the round trip can be proved by enumeration rather than asserted.

   A value is [imm4 << (2 * rot)] with [rot] in 0..3. Several (rot, imm4) pairs
   can denote the same value - 0 is representable four ways - so the encoder
   picks the smallest rotation, exactly as GNU as does for ARM. *)

let modimm_encode v =
  let rec go rot =
    if rot > 3 then None
    else
      let shift = 2 * rot in
      if
        Int64.equal (Int64.shift_left (Int64.shift_right_logical v shift) shift) v
        && Int64.compare (Int64.shift_right_logical v shift) 16L < 0
      then Some (Int64.of_int rot, Int64.shift_right_logical v shift)
      else go (rot + 1)
  in
  go 0

let modimm_decode (rot, imm) = Some (Int64.shift_left imm (2 * Int64.to_int rot))

let modimm : (int64, fixup_kind) t =
  iso_fun ~name:"modimm" ~encode:modimm_encode ~decode:modimm_decode
    (field ~width:2 "rot" ** field ~width:4 "imm4")

(* Exhaustive because the domain is the image of a 2x4-bit space: 64 (rot,
   imm4) pairs, hence at most 64 distinct values. Enumerating the *values* by
   construction rather than guessing them is what makes "every representable
   value round-trips" a claim about all of them. *)
let modimm_domain =
  let values =
    List.concat_map
      (fun rot -> List.init 16 (fun imm -> Int64.shift_left (Int64.of_int imm) (2 * rot)))
      [ 0; 1; 2; 3 ]
  in
  let uniq = List.sort_uniq Int64.compare values in
  Finite.exhaustive ~name:"modimm"
    ~why:
      "the representable set is the image of a 2-bit rotation over a 4-bit immediate, so \
       enumerating the 64 (rot, imm4) pairs enumerates every value there is"
    uniq

(* {1 A relaxation ladder}

   Modelled on x86's short/near branch pair, which is the M2 reason [Relax]
   exists: GNU encodes a nearby branch in two bytes, so an assembler that always
   emitted the long form would be correct and would still fail a byte-for-byte
   comparison. The rung is not decidable at encode time - it depends on where the
   target lands - so the encoder hands back both and layout chooses.

   The two rungs have different opcodes, which is what makes a decoded form able
   to say which one produced it. [check] enforces that. *)

type branch = Br of int64

let br_short =
  iso_fun ~name:"br.short"
    ~encode:(function Br d -> Some ((), d))
    ~decode:(fun ((), d) -> Some (Br d))
    (const ~width:8 0b1110_1011L ** fixup ~width:8 ~kind:Rel12 "target")

let br_near =
  iso_fun ~name:"br.near"
    ~encode:(function Br d -> Some ((), d))
    ~decode:(fun ((), d) -> Some (Br d))
    (const ~width:8 0b1110_1001L ** fixup ~width:16 ~kind:Rel12 "target")

let ladder : (branch, fixup_kind) t =
  relax ~name:"br" [ rung ~label:"rel8" br_short; rung ~label:"rel16" br_near ]

(* The same ladder with a rung that can *decline*.

   [ladder] cannot: a [fixup] node is unsigned and truncating, because at encode
   time a real branch target is a placeholder zero and the range verdict belongs
   to bind. So a rung there always applies, and "you pinned a rung that does not
   fit this value" is not a state it can reach. A rung whose projection checks
   the range is what makes that second failure real, and it is the shape a target
   would use for a form whose applicability is decidable from the operand. *)
let checked_ladder : (branch, fixup_kind) t =
  relax ~name:"br.checked"
    [
      rung ~label:"rel8"
        (iso_fun ~name:"br.short-checked"
           ~encode:(function
             | Br d -> if Int64.compare (Int64.abs d) 128L < 0 then Some ((), d) else None)
           ~decode:(fun ((), d) -> Some (Br d))
           (const ~width:8 0b1110_1011L ** fixup ~width:8 ~kind:Rel12 "target"));
      rung ~label:"rel16" br_near;
    ]

(* {1 A ladder beside an unrelated, genuinely-failing alternative}

   Modelled on x86's real shape - a top-level [Alt] whose [jmp-rel] arm is
   itself a [Relax] node, each of whose *rungs* carries its own projecting
   [Iso_fun] (there is no single projection wrapping the whole ladder;
   asm/docs/corpus.md, x86_family_encode.ml's [jmp_rung]) - beside another
   arm whose value can genuinely fail a range check.

   This is what [attempt]'s [Relax] case has to get right: every rung of
   [RFixed]'s completely wrong-shaped value declining must produce a plain
   [Declined] for the whole ladder, not a manufactured [`No_rung]. Before
   that fix, the manufactured failure was recorded as the enclosing [Alt]'s
   [first_error] and permanently hid [fixed]'s own real,
   later [Field_does_not_fit] - the same masking gcc's `andl
   $0xfffffffc,%edx` (M5, asm/docs/corpus.md) hit for real, reported as
   `x86.encode: relax jmp: no rung applies` regardless of which alternative
   and value actually failed. *)
type relaxed = RBr of int64 | RFixed of int64

let show_relaxed = function RBr d -> Printf.sprintf "RBr %Ld" d | RFixed v -> Printf.sprintf "RFixed %Ld" v

let relaxed_ladder : (relaxed, fixup_kind) t =
  relax ~name:"jmp"
    [
      rung ~label:"short"
        (iso_fun ~name:"jmp.short"
           ~encode:(function RBr d -> Some ((), d) | RFixed _ -> None)
           ~decode:(fun ((), d) -> Some (RBr d))
           (const ~width:8 0b1110_1011L ** fixup ~width:8 ~kind:Rel12 "target"));
      rung ~label:"near"
        (iso_fun ~name:"jmp.near"
           ~encode:(function RBr d -> Some ((), d) | RFixed _ -> None)
           ~decode:(fun ((), d) -> Some (RBr d))
           (const ~width:8 0b1110_1001L ** fixup ~width:16 ~kind:Rel12 "target"));
    ]

let relaxed_isa : (relaxed, fixup_kind) t =
  choice ~name:"relaxed"
    [
      alt ~label:"jmp" ~priority:0 relaxed_ladder;
      alt ~label:"fixed" ~priority:1
        (iso_fun ~name:"fixed"
           ~encode:(function RFixed v -> Some v | RBr _ -> None)
           ~decode:(fun v -> Some (RFixed v))
           (field ~signedness:Signed ~width:8 "imm"));
    ]

(* The same ladder, buried the way a real one is.

   [ladder] above is the top-level node, which is the one shape where finding
   the rungs is trivial. A real ladder is never there: it sits under the
   [Iso_fun] that projected the instruction into fields, sequenced between a
   register operand and trailing opcode bits, reached through the target's
   top-level [Alt]. Each of those wrappers is a place a ladder walk can stop and
   hand back the single encoding [attempt] chose - which does not look like a
   bug, it looks like a form that does not relax.

   The nesting is [Seq (reg, Seq (ladder, const))], so the ladder is on the
   right of the outer [Seq] and the left of the inner one. Both orders matter:
   the interpreter has to vary whichever side carries the rungs and hold the
   other fixed, and a walk that only handled one order would still be wrong half
   the time. *)

type buried = Buried of reg * int64

let buried_body : (buried, fixup_kind) t =
  iso_fun ~name:"buried.body"
    ~encode:(function Buried (r, d) -> Some (r, (Br d, ())))
    ~decode:(fun (r, (Br d, ())) -> Some (Buried (r, d)))
    (reg_codec ** ladder ** const ~width:5 0b10110L)

let buried_ladder : (buried, fixup_kind) t =
  choice ~name:"buried" [ alt ~label:"br" ~priority:0 buried_body ]

(* {1 A split fixup}

   Two slices of one logical value at non-adjacent positions, as AArch64 [adrp]
   and ARM [movw]/[movt] have. The point is that the *codec* only ever sees
   pieces: reassembling them, and sign-extending the whole rather than each
   piece, is the [Iso_fun]'s job. Sign-extending per slice is the bug this shape
   exists to make visible. *)

type split = Split of int64

let split_hi_lo : (split, fixup_kind) t =
  iso_fun ~name:"split"
    ~encode:(function
      (* hi = value bits 4..15, lo = value bits 0..3 *)
      | Split v -> Some (Int64.shift_right_logical v 4, ((), (Int64.logand v 0xFL, ()))))
    ~decode:(fun (hi, ((), (lo, ()))) -> Some (Split (Int64.logor (Int64.shift_left hi 4) lo)))
    (fixup ~width:12 ~value_lsb:4 ~kind:Abs16 "sym"
    ** const ~width:4 0b1010L
    ** fixup ~width:4 ~value_lsb:0 ~kind:Abs16 "sym"
    ** const ~width:12 0L)

(* The same shape with a *signed* logical value, which is where per-slice sign
   extension stops being a style question. The value is 16 bits wide across two
   slices; extending the high slice alone would give the right answer for every
   non-negative value and the wrong one for every negative, so a test that only
   tried small positive numbers would never see it. *)
let sign_extend16 v = Int64.shift_right (Int64.shift_left v 48) 48

let split_signed : (split, fixup_kind) t =
  iso_fun ~name:"split.signed"
    ~encode:(function
      | Split v ->
          Some (Int64.logand (Int64.shift_right_logical v 4) 0xFFFL, ((), (Int64.logand v 0xFL, ()))))
    ~decode:(fun (hi, ((), (lo, ()))) ->
      Some (Split (sign_extend16 (Int64.logor (Int64.shift_left hi 4) lo))))
    (fixup ~width:12 ~value_lsb:4 ~kind:Abs16 "sym"
    ** const ~width:4 0b1010L
    ** fixup ~width:4 ~value_lsb:0 ~kind:Abs16 "sym"
    ** const ~width:12 0L)

(* {1 Deliberately broken descriptions}

   [check] is only evidence if it can fail, so these exist to make it fail.
   They are never part of [isa]. *)

let broken_table : (reg, fixup_kind) t =
  iso_table ~name:"broken.dup" ~equal:( = ) ~show:reg_name
    ~entries:[ (R0, 0L); (R1, 1L); (R2, 1L); (R3, 99L) ]
    (field ~width:3 "r")

let broken_alt : (unit * int64, fixup_kind) t =
  choice ~name:"broken.ambiguous"
    [
      alt ~label:"a" ~priority:0 (const ~width:4 0b0001L ** field ~width:12 "x");
      (* Same fixed bits as "a" wherever both are fixed, so no bit string can
         distinguish them and only the priority decides. *)
      alt ~label:"b" ~priority:1 (const ~width:4 0b0001L ** field ~width:12 "y");
      alt ~label:"c" ~priority:1 (const ~width:4 0b0101L ** field ~width:12 "z");
    ]

(* Two ladders sequenced on one realized path. "The rungs of this form" is then
   a product of two independent choices, and [encode_ladder] returns a list -
   so the shape is rejected rather than given an arbitrary reading. Note that
   neither ladder is nested inside the other, which is the check that already
   existed and does not catch this. *)
let broken_two_ladders : (branch * (branch * unit), fixup_kind) t =
  ladder ** ladder ** const ~width:4 0L

(* A ladder whose rungs cannot be told apart on decode. Same opcode bits, same
   width, so the first always matches and the rung that produced an encoding is
   unrecoverable - which silently breaks form preservation across the text
   boundary rather than failing anywhere obvious. *)
let broken_relax : (branch, fixup_kind) t =
  relax ~name:"broken.rungs"
    [
      rung ~label:"a"
        (iso_fun ~name:"a"
           ~encode:(function Br d -> Some ((), d))
           ~decode:(fun ((), d) -> Some (Br d))
           (const ~width:8 0b1111_0000L ** fixup ~width:8 ~kind:Rel12 "target"));
      rung ~label:"b"
        (iso_fun ~name:"b"
           ~encode:(function Br d -> Some ((), d))
           ~decode:(fun ((), d) -> Some (Br d))
           (const ~width:8 0b1111_0000L ** fixup ~width:8 ~kind:Rel12 "target"));
    ]

(* Two forms whose fixed bits differ, and one of them behind a fixup.

   [check] compares alternatives by their *fixed* bits, and a fixup's bits are
   not fixed: at encode time they hold a placeholder and at bind time whatever
   the linker computes. So they are don't-care, exactly as a field's are, and
   these two overlap even though writing the second one's low twelve bits as a
   constant would have told them apart. That is the honest answer - the pair
   really is ambiguous on decode - and it is worth pinning because the opposite
   reading would silently declare a whole family of branch forms distinguishable. *)
let fixup_is_dont_care : (branch, fixup_kind) t =
  choice ~name:"dont-care"
    [
      alt ~label:"const-low" ~priority:0
        (iso_fun ~name:"const-low"
           ~encode:(function Br _ -> Some ((), ()))
           ~decode:(fun ((), ()) -> Some (Br 0L))
           (const ~width:4 0b1100L ** const ~width:12 0xFFFL));
      alt ~label:"fixup-low" ~priority:1
        (iso_fun ~name:"fixup-low"
           ~encode:(function Br d -> Some ((), d))
           ~decode:(fun ((), d) -> Some (Br d))
           (const ~width:4 0b1100L ** fixup ~width:12 ~kind:Rel12 "target"));
    ]

(* A ladder inside a ladder. [encode_ladder] returns a list, so a rung that is
   itself a ladder would make one entry of that list stand for several
   encodings - and which one it stood for would depend on the order the walk
   happened to take. Rejected rather than defined, which is what keeps a ladder
   single-valued per rung. *)
let nested_relax : (branch, fixup_kind) t =
  relax ~name:"outer" [ rung ~label:"inner" ladder; rung ~label:"wide" br_near ]

(* Slices of one name that overlap in value space and disagree about the kind:
   the value bits 0..3 are claimed twice, and one slice says Rel12 where the
   other says Abs16. *)
let broken_fixup : (int64 * (unit * (int64 * unit)), fixup_kind) t =
  fixup ~width:8 ~value_lsb:0 ~kind:Rel12 "s"
  ** const ~width:4 0L
  ** fixup ~width:4 ~value_lsb:0 ~kind:Abs16 "s"
  ** const ~width:8 0L
