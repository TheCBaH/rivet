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
  | Jmp  (** target supplied by a fixup, not by an operand *)

let show_insn = function
  | Nop -> "nop"
  | Add (a, b) -> Printf.sprintf "add %s, %s" (reg_name a) (reg_name b)
  | Li (r, v) -> Printf.sprintf "li %s, %Ld" (reg_name r) v
  | Jmp -> "jmp <target>"

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

let form_jmp =
  iso_fun ~name:"jmp"
    ~encode:(function Jmp -> Some ((), ()) | _ -> None)
    ~decode:(fun ((), ()) -> Some Jmp)
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
