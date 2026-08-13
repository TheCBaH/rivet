(* The lowered module AST (.ai/asm_plan.md §4.4, §7).

   One input unit, whether parsed or constructed directly, in the form the image
   builder consumes: logical sections with permissions, ordered fragments,
   symbol declarations, and fixups. It has the same information an object file
   would carry because the internal linker needs it, and is never serialized as
   one.

   No addresses. A fragment knows its bytes and its alignment demand; where it
   lands is layout's answer, and a lowered module that already contained
   addresses could not be laid out twice.

   Parameterized over the target's fixup-kind type, so a fixup can be
   target-specific without this package knowing what a target is. The four
   return42 fixtures still carry no fixups - their .text is relocation-free once
   the .cfi_* directives, and therefore .eh_frame, are discarded - but the
   direct-call and global fixtures do. *)

open Foundation

(* What a reference is *for*. Not derivable from the kind by generic code - the
   kind is abstract above the target - and not recoverable by reading a name, so
   the target declares it and it is carried here. The oracle's classifier is
   indexed by it because the targets disagree per role about which references
   survive assembly. *)
type fixup_role = Call | Branch | Data_address

let fixup_role_name = function
  | Call -> "call"
  | Branch -> "branch"
  | Data_address -> "data-address"

(* The accepted values of a fixup, checked once against the whole logical value
   at bind. [Bitpattern n] is the union [-2^(n-1), 2^n): a 32-bit absolute
   address is neither signed nor unsigned in the field sense, and forcing it
   into one of those rejects half the values it must accept. *)
type range = Signed of int | Unsigned of int | Bitpattern of int

let range_width = function Signed n | Unsigned n | Bitpattern n -> n

let range_admits r v =
  let n = range_width r in
  if n >= 64 then true
  else
    let two_pow k = Int64.shift_left 1L k in
    match r with
    | Unsigned _ -> Int64.unsigned_compare v (two_pow n) < 0
    | Signed _ ->
        let half = two_pow (n - 1) in
        Int64.compare v (Int64.neg half) >= 0 && Int64.compare v half < 0
    | Bitpattern _ ->
        let half = two_pow (n - 1) in
        Int64.compare v (Int64.neg half) >= 0 && Int64.unsigned_compare v (two_pow n) < 0

let pp_range ppf r =
  let tag = match r with Signed _ -> "s" | Unsigned _ -> "u" | Bitpattern _ -> "b" in
  Fmt.pf ppf "%s%d" tag (range_width r)

(* One contiguous run of the logical value inside the patch container, in memory
   coordinates. [value_lsb] says which bit of the value it carries, so
   AArch64 adrp's immlo/immhi and ARM's movw/movt describe rather than
   approximate their split fields. *)
type slice = { bit_offset : int; bit_width : int; value_lsb : int }
type pair_ref = Sibling_key of string | Anchor_symbol of string
type pairing = Unpaired | Pair_head of string | Pair_tail of pair_ref

(* A fixup as the *linker* needs it: where to patch, what PC the architecture
   computes against, what values are acceptable, and the expression to evaluate.
   It is deliberately not Codec.placement - asm_core sits below codec in the
   dependency DAG (§5.1) and must stay there - and the shapes differ anyway,
   because a codec places bits inside one instruction while a linker patches
   bytes inside a section. The target converts.

   [byte_offset] and [pc_bias] are two different locations and both are needed:
   the first is where the bits go, the second is what the architecture measures
   a displacement from. On x86 they differ by the whole instruction length. *)
type 'k fixup = {
  kind : 'k;
  kind_name : string;  (** from the target; for dumps and the relocation oracle *)
  family : string;  (** ladder compatibility is checked on this, not on the kind *)
  role : fixup_role;
  name : string;
  slices : slice list;
  byte_offset : int;  (** the patch container, relative to the fragment start *)
  container : int;  (** its size in bytes; read and written little-endian *)
  pc_bias : int;  (** PC base = fragment address + this *)
  range : range;
  value : Expr.t;
  pairing : pairing;
  origin : Origin.t;
}

(* A branch or call target, in the three states it can actually be in.

   [Symbolic] with no rung is the ordinary case: lowering knows the expression
   and not the distance, so the encoder offers a ladder and layout chooses.

   [Symbolic] with a rung is a *pinned* symbolic target, and it is why two
   states are not enough. Canonical disassembly prints a form-forcing spelling
   so that a non-minimal encoding survives being read back; re-parsing that text
   yields a target that is still an address-dependent expression - so it still
   needs a fixup - but must not be relaxed again.

   [Resolved] only ever comes from decode, and carries the rung that matched so
   the same encoding can be reproduced. *)
type branch =
  | Symbolic of { value : Expr.t; rung : string option }
  | Resolved of { value : int64; rung : string }

let pp_branch ppf = function
  | Symbolic { value; rung = None } -> Fmt.string ppf (Expr.to_string value)
  | Symbolic { value; rung = Some r } -> Fmt.pf ppf "%s{%s}" (Expr.to_string value) r
  | Resolved { value; rung } -> Fmt.pf ppf "%Ld{%s}" value rung

let equal_branch a b =
  match (a, b) with
  | Symbolic x, Symbolic y ->
      String.equal (Expr.to_string x.value) (Expr.to_string y.value) && x.rung = y.rung
  | Resolved x, Resolved y -> Int64.equal x.value y.value && String.equal x.rung y.rung
  | _ -> false

(* One rung of a relaxation ladder: a complete encoding of the same operation,
   differing in size and in how far its fixup reaches. *)
type 'k encoded_form = { bytes : string; form : string; fixups : 'k fixup list }

type 'k fragment =
  | Bytes of { bytes : string; form : string option; fixups : 'k fixup list; origin : Origin.t }
      (** [form] is the codec's form identifier: the path of alternatives its encoding took. It is
          what the diagnostic disassembler prints and what a differential test names when an
          encoding is right by accident. *)
  | Align of { boundary : int; fills : string array; origin : Origin.t }
      (** [fills.(n-1)] is the target's padding for exactly [n] bytes - not one pattern to repeat.

          The distinction is the whole point on x86, where padding is a *single no-op instruction*
          chosen for the gap: GAS fills ten bytes with one ten-byte nop, and cycling a shorter
          pattern produces a different instruction stream that happens to be the same length. M1
          never saw it because both x86 fixtures aligned at offset 0, where the gap is empty.

          Still not zero-filled by default: a zero gap in [.text] decodes as an instruction nobody
          wrote. *)
  | Relax of { alts : 'k encoded_form list; origin : Origin.t }
      (** several encodings of one operation, shortest first, chosen at layout. It is a fragment
          rather than an already-encoded [Bytes] because the choice depends on where the target
          lands, which nothing knows before layout - and GNU picks the two-byte forms for the loop
          fixture's branches, so always emitting the long ones would be correct and would still
          fail the byte gate. A direct producer that wants a pinned encoding emits [Bytes]. *)
  | Zero of { length : int; origin : Origin.t }
  | Label_def of { name : string; origin : Origin.t }
  | Set_size of { name : string; size : Expr.t; origin : Origin.t }
      (** [.size s, . - s] as a zero-length fragment rather than a field of the symbol. The
          expression mentions the current location, so its value depends on *where the directive
          appeared*; a symbol-level field would have lost that position and had to guess. *)

type section = { sec_name : string; perms : Perms.t; alignment : int }
type 'k section_content = { sec : section; fragments : 'k fragment list }

(* ELF visibility. The directives that set it are not in M2's fixture set, so
   every symbol is [Default] today - but the field exists anyway, because the
   relocation classifier has to decide on visibility *this assembler parsed and
   preserved*. Reading it off the measured object instead would prove something
   about GNU rather than about us. *)
type visibility = Default | Hidden | Protected | Internal

let visibility_name = function
  | Default -> "default"
  | Hidden -> "hidden"
  | Protected -> "protected"
  | Internal -> "internal"

type symbol = {
  name : string;
  global : bool;
  visibility : visibility;
  kind : Directive.sym_kind;
  size : Expr.t option;
  section : string;
}

type 'k module_ = {
  unit_name : string;
  sections : 'k section_content list;
  symbols : symbol list;
  declared_sections : string list;
      (** sections named by the input that are never allocated - [.note.GNU-stack]. Recorded so a
          later pass can report them, and deliberately not section objects. *)
}

(* Printed from the fragment, so [--dump-lowered-ast] shows what will be
   patched. Still no addresses: a slice says where in the instruction, never
   where in memory. *)
let pp_slice ppf s = Fmt.pf ppf "%d+%d@%d" s.bit_offset s.bit_width s.value_lsb

let pp_pairing ppf = function
  | Unpaired -> ()
  | Pair_head key -> Fmt.pf ppf " head(%s)" key
  | Pair_tail (Sibling_key key) -> Fmt.pf ppf " tail(sibling:%s)" key
  | Pair_tail (Anchor_symbol symbol) -> Fmt.pf ppf " tail(anchor:%s)" symbol

let pp_fixup ppf f =
  Fmt.pf ppf "@%d/%dB pc+%d %s %s %a [%a] = %s%a" f.byte_offset f.container f.pc_bias f.name
    f.kind_name
    (fun ppf () -> pp_range ppf f.range)
    ()
    Fmt.(list ~sep:(any ",") pp_slice)
    f.slices (Expr.to_string f.value) pp_pairing f.pairing

let pp_fixups ppf = function
  | [] -> ()
  | fs -> Fmt.pf ppf "@,  @[<v>%a@]" Fmt.(list ~sep:cut pp_fixup) fs

let pp_fragment ppf = function
  | Bytes { bytes; form; fixups; _ } ->
      Fmt.pf ppf "bytes %-24s %s%a" (Byte_cursor.to_hex bytes)
        (match form with None -> "" | Some f -> "[" ^ f ^ "]")
        pp_fixups fixups
  | Relax { alts; _ } ->
      Fmt.pf ppf "@[<v>relax%a@]"
        Fmt.(
          list ~sep:nop (fun ppf a ->
              Fmt.pf ppf "@,  %-24s [%s]%a" (Byte_cursor.to_hex a.bytes) a.form pp_fixups a.fixups))
        alts
  | Align { boundary; fills; _ } ->
      (* The widest fill stands for the table: printing all of them would put a
         quadratic byte dump in every aligned section for no added fact, and the
         longest is the one that shows which family the target pads with. *)
      Fmt.pf ppf "align %d fill<=%s" boundary
        (if Array.length fills = 0 then "" else Byte_cursor.to_hex fills.(Array.length fills - 1))
  | Zero { length; _ } -> Fmt.pf ppf "zero %d" length
  | Label_def { name; _ } -> Fmt.pf ppf "label %s" name
  | Set_size { name; size; _ } -> Fmt.pf ppf "size %s = %a" name Expr.pp size

(* {1 Ladder well-formedness}

   Called from the pipeline *and* from the image, because a direct
   [Lowered_ast.module_] producer never passes through the pipeline and an
   ill-formed ladder there would reach layout unchecked.

   Compatibility is by fixup name, expression and *family* - not by kind or
   width. An x86 short/near pair necessarily changes both: rel8 and rel32 are
   different kinds with different widths and different pc_bias, and requiring
   them to match would reject the one case ladders exist for. *)
(* The annotations are load-bearing: [symbol] also declares a [name] field and
   is declared later, so without them OCaml resolves a fixup's [name] to it. *)
(* The ladder's error domain (asm/docs/errors.md). Every constructor carries the
   rungs it is talking about rather than their names spliced into a sentence,
   which is what lets a caller report the pair without re-parsing the message.
   Rendering is unchanged. *)
type relax_error =
  [ `Empty_ladder
  | `Rung_not_shorter of rung_pair
  | `Rung_fixups_differ of rung_pair
  | `Fixup_family_changes of fixup_family_changes
  | `Fixup_target_differs of string ]

and rung_pair = { rung : string; rung_bytes : int; other : string; other_bytes : int }
and fixup_family_changes = { fixup : string; from_family : string; to_family : string }

let pp_relax_error ppf : relax_error -> unit = function
  | `Empty_ladder -> Fmt.string ppf "a relaxation ladder needs at least one alternative"
  | `Rung_not_shorter { rung; rung_bytes; other; other_bytes } ->
      Fmt.pf ppf "rung %s (%d bytes) is not shorter than %s (%d bytes)" rung rung_bytes other
        other_bytes
  | `Rung_fixups_differ { rung; other; _ } ->
      Fmt.pf ppf "rung %s does not carry the same fixups as %s" rung other
  | `Fixup_family_changes { fixup; from_family; to_family } ->
      Fmt.pf ppf "fixup %s changes family between rungs (%s vs %s)" fixup from_family to_family
  | `Fixup_target_differs fixup ->
      Fmt.pf ppf "fixup %s targets a different expression between rungs" fixup

let relax_error_code : relax_error -> string = function
  | `Empty_ladder -> "relax.empty"
  | `Rung_not_shorter _ -> "relax.not-shorter"
  | `Rung_fixups_differ _ -> "relax.fixups-differ"
  | `Fixup_family_changes _ | `Fixup_target_differs _ -> "relax.fixup-differs"

let relax_fail ?pos (e : relax_error) = Err.fail ?pos ~pp_error:pp_relax_error e

let validate_relax (alts : 'k encoded_form list) =
  let names (f : 'k encoded_form) =
    List.sort_uniq compare (List.map (fun (x : 'k fixup) -> x.name) f.fixups)
  in
  match alts with
  | [] -> relax_fail ~pos:__POS__ `Empty_ladder
  | first :: _ -> (
      let want = names first in
      let rec ordered = function
        | a :: (b :: _ as rest) ->
            if String.length a.bytes >= String.length b.bytes then
              relax_fail ~pos:__POS__
                (`Rung_not_shorter
                   {
                     rung = a.form;
                     rung_bytes = String.length a.bytes;
                     other = b.form;
                     other_bytes = String.length b.bytes;
                   })
            else ordered rest
        | _ -> Ok ()
      in
      let compatible =
        List.fold_left
          (fun acc a ->
            match acc with
            | Error _ -> acc
            | Ok () ->
                if names a <> want then
                  relax_fail ~pos:__POS__
                    (`Rung_fixups_differ
                       {
                         rung = a.form;
                         rung_bytes = String.length a.bytes;
                         other = first.form;
                         other_bytes = String.length first.bytes;
                       })
                else
                  List.fold_left
                    (fun acc (fa : 'k fixup) ->
                      match acc with
                      | Error _ -> acc
                      | Ok () -> (
                          match
                            List.find_opt
                              (fun (fb : 'k fixup) -> String.equal fb.name fa.name)
                              first.fixups
                          with
                          | None -> Ok ()
                          | Some fb ->
                              if not (String.equal fa.family fb.family) then
                                relax_fail ~pos:__POS__
                                  (`Fixup_family_changes
                                     {
                                       fixup = fa.name;
                                       from_family = fb.family;
                                       to_family = fa.family;
                                     })
                              else if Expr.to_string fa.value <> Expr.to_string fb.value then
                                relax_fail ~pos:__POS__ (`Fixup_target_differs fa.name)
                              else Ok ()))
                    (Ok ()) a.fixups)
          (Ok ()) alts
      in
      match compatible with Error _ as e -> e | Ok () -> ordered alts)

let pp_symbol ppf s =
  Fmt.pf ppf "%s %s %s in %s%a"
    (if s.global then "global" else "local")
    s.name (Directive.sym_kind_name s.kind) s.section
    Fmt.(option (any " size=" ++ using Expr.to_string string))
    s.size

let pp ppf m =
  Fmt.pf ppf "@[<v>lowered %s@,%a@,%a%a@]" m.unit_name
    Fmt.(
      list ~sep:cut (fun ppf sc ->
          Fmt.pf ppf "@[<v2>section %s %a align=%d@,%a@]" sc.sec.sec_name Perms.pp sc.sec.perms
            sc.sec.alignment
            Fmt.(list ~sep:cut pp_fragment)
            sc.fragments))
    m.sections
    Fmt.(list ~sep:cut pp_symbol)
    m.symbols
    Fmt.(list ~sep:nop (fun ppf n -> Fmt.pf ppf "@,declared %s (not allocated)" n))
    m.declared_sections
