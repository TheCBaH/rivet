(* The lowered module AST (docs/design.md §4.4, §7).

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
        (* Either reading: [-2^(n-1), 2^n). The upper bound is a signed comparison, since an
           unsigned one would read every negative value as too large. *)
        let half = two_pow (n - 1) in
        Int64.compare v (Int64.neg half) >= 0 && (n = 63 || Int64.compare v (two_pow n) < 0)

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
  | Symbolic x, Symbolic y -> Expr.equal x.value y.value && x.rung = y.rung
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
  | Align of { boundary : int; fill : int option; max_skip : int option; origin : Origin.t }
      (** pad to [boundary] bytes. Layout asks for the padding of the one gap length it needs
          ([Image.plan_image]'s [align_fill]) - not a pattern to repeat, and not a table built here:
          on x86 the padding is a *single no-op instruction* chosen for the gap, GAS fills ten
          bytes with one ten-byte nop, and a table of every possible gap would cost
          [boundary^2 / 2] bytes per directive.

          Not zero-filled in an executable section: a zero gap in [.text] decodes as an
          instruction nobody wrote. *)
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

(* PROGBITS/NOBITS: a [Nobits] section reserves space and has no bytes. A
   same-name section fed both kinds by two different inputs is a dedicated rejection (the GNU probe in
   docs/m3-evidence found real `ld`, under this project's own linker-script
   shape, silently coerces that case into one PROGBITS section instead). *)
type section_kind = Progbits | Nobits

let section_kind_name = function Progbits -> "progbits" | Nobits -> "nobits"

type section = { sec_name : string; perms : Perms.t; alignment : int; kind : section_kind }
type 'k section_content = { sec : section; fragments : 'k fragment list }

(* ELF visibility. The directives that set it are not in M2's fixture set, so
   every symbol is [Default] today - but the field exists anyway, because the
   relocation classifier has to decide on visibility *this assembler parsed and
   preserved*. Reading it off the measured object instead would prove something
   about GNU rather than about us. *)
type visibility = Directive.visibility = Default | Hidden | Protected | Internal

let visibility_name = Directive.visibility_name

(* M3 (docs/design.md §12, "Milestone 3"). Replaces the bare [global : bool]
   M1/M2 carried: a linker with more than one input needs a third state, since
   GNU's [.weak] is neither "visible link-wide with duplicate-definition
   rejection" ([Global]) nor "invisible outside this input" ([Local]) - a weak
   definition is link-wide visible but loses to a strong one instead of
   conflicting with it (docs/contracts.md, the M3 resolver). *)
type binding = Local | Global | Weak

let binding_name = function Local -> "local" | Global -> "global" | Weak -> "weak"

type symbol = {
  name : string;
  binding : binding;
  visibility : visibility;
  kind : Directive.sym_kind;
  size : Expr.t option;
  section : string;
}

(* M3 §7: a [.comm] claim - size and alignment, no input-section offset. Kept
   apart from [symbol] because a common declaration is not yet a definition:
   whether it ever contributes storage, and how much, is decided link-wide by
   the resolver (strong beats it outright; several declarations of the same
   name fold into one reservation sized to the maximum requested size and
   aligned to the maximum requested alignment), never by this input alone. *)
type common = { comm_name : string; comm_size : int; comm_align : int }

type 'k module_ = {
  unit_name : string;
  target : string;
      (** the target the module was lowered for ([ENCODE.name]); a direct producer states it, and
          an image of one target refuses to run on a host of another *)
  sections : 'k section_content list;
  symbols : symbol list;
  commons : common list;
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
  | Align { boundary; fill; max_skip; _ } ->
      Fmt.pf ppf "align %d" boundary;
      Option.iter (Fmt.pf ppf " fill %d") fill;
      Option.iter (Fmt.pf ppf " max %d") max_skip
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
(* The ladder's error domain (docs/errors.md). Every constructor carries the
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
                              else if not (Expr.equal fa.value fb.value) then
                                relax_fail ~pos:__POS__ (`Fixup_target_differs fa.name)
                              else Ok ()))
                    (Ok ()) a.fixups)
          (Ok ()) alts
      in
      match compatible with Error _ as e -> e | Ok () -> ordered alts)

let pp_symbol ppf s =
  Fmt.pf ppf "%s %s %s in %s%s%a" (binding_name s.binding) s.name (Directive.sym_kind_name s.kind)
    s.section
    (match s.visibility with Default -> "" | v -> " visibility=" ^ visibility_name v)
    Fmt.(option (any " size=" ++ using Expr.to_string string))
    s.size

let pp_common ppf c = Fmt.pf ppf "@,comm %s size=%d align=%d" c.comm_name c.comm_size c.comm_align

let pp ppf m =
  Fmt.pf ppf "@[<v>lowered %s@,%a@,%a%a%a@]" m.unit_name
    Fmt.(
      list ~sep:cut (fun ppf sc ->
          Fmt.pf ppf "@[<v2>section %s %a align=%d@,%a@]" sc.sec.sec_name Perms.pp sc.sec.perms
            sc.sec.alignment
            Fmt.(list ~sep:cut pp_fragment)
            sc.fragments))
    m.sections
    Fmt.(list ~sep:cut pp_symbol)
    m.symbols
    Fmt.(list ~sep:nop pp_common)
    m.commons
    Fmt.(list ~sep:nop (fun ppf n -> Fmt.pf ppf "@,declared %s (not allocated)" n))
    m.declared_sections

type fixup_defect =
  | Offset_outside_fragment
  | Container_size of int
  | Slice_outside_container
  | Slice_outside_range
  | Range_width of int

type invalid =
  | Empty_name of [ `Unit | `Target | `Section ]
  | Duplicate_section of string
  | Bad_alignment of { where : string; value : int }
  | Negative_length of { section : string; length : int }
  | Bad_fixup of { section : string; fixup : string; defect : fixup_defect }
  | Duplicate_label of { section : string; name : string }

let pp_fixup_defect ppf = function
  | Offset_outside_fragment -> Fmt.string ppf "its container lies outside the fragment"
  | Container_size n -> Fmt.pf ppf "its container is %d bytes, not 1 to 8" n
  | Slice_outside_container -> Fmt.string ppf "a slice lies outside the container"
  | Slice_outside_range -> Fmt.string ppf "a slice carries bits beyond the range width"
  | Range_width n -> Fmt.pf ppf "its range is %d bits wide, not 1 to 64" n

let pp_invalid ppf = function
  | Empty_name `Unit -> Fmt.string ppf "the unit name is empty"
  | Empty_name `Target -> Fmt.string ppf "the target name is empty"
  | Empty_name `Section -> Fmt.string ppf "a section name is empty"
  | Duplicate_section n -> Fmt.pf ppf "section %s appears twice in one module" n
  | Bad_alignment { where; value } ->
      Fmt.pf ppf "%s: alignment %d is not a positive power of two" where value
  | Negative_length { section; length } ->
      Fmt.pf ppf "section %s: a zero-fill fragment has length %d" section length
  | Bad_fixup { section; fixup; defect } ->
      Fmt.pf ppf "section %s: fixup %s: %a" section fixup pp_fixup_defect defect
  | Duplicate_label { section; name } ->
      Fmt.pf ppf "section %s: label %s is defined at two different addresses" section name

let is_pow2 n = n > 0 && n land (n - 1) = 0

let fixup_defects ~len (f : 'k fixup) =
  let w = range_width f.range in
  let outside_container (s : slice) =
    s.bit_offset < 0 || s.bit_width < 1 || s.bit_offset + s.bit_width > f.container * 8
  in
  let outside_range (s : slice) = s.value_lsb < 0 || s.value_lsb + s.bit_width > w in
  if f.container < 1 || f.container > 8 then [ Container_size f.container ]
  else
    (if f.byte_offset < 0 || f.byte_offset + f.container > len then [ Offset_outside_fragment ]
     else [])
    @ (if w < 1 || w > 64 then [ Range_width w ] else [])
    @ (if List.exists outside_container f.slices then [ Slice_outside_container ] else [])
    @ if List.exists outside_range f.slices then [ Slice_outside_range ] else []

(* Label uniqueness cannot use final addresses, which relaxation has yet to settle. Two labels of
   one name are the same address when no fragment that can occupy space lies between them; this
   is how GAS tolerates a repeated label at the same value. *)
let validate (m : 'k module_) =
  let errs = ref [] in
  let add e = errs := e :: !errs in
  if m.unit_name = "" then add (Empty_name `Unit);
  if m.target = "" then add (Empty_name `Target);
  let seen = Hashtbl.create 8 in
  let labels = Hashtbl.create 16 in
  let epoch = ref 0 in
  List.iter
    (fun (sc : 'k section_content) ->
      let section = sc.sec.sec_name in
      if section = "" then add (Empty_name `Section)
      else if Hashtbl.mem seen section then add (Duplicate_section section)
      else Hashtbl.add seen section ();
      if not (is_pow2 sc.sec.alignment) then
        add (Bad_alignment { where = "section " ^ section; value = sc.sec.alignment });
      let check_fixups len fixups =
        List.iter
          (fun (f : 'k fixup) ->
            List.iter
              (fun defect -> add (Bad_fixup { section; fixup = f.name; defect }))
              (fixup_defects ~len f))
          fixups
      in
      incr epoch;
      List.iter
        (function
          | Bytes { bytes; fixups; _ } ->
              if bytes <> "" then incr epoch;
              check_fixups (String.length bytes) fixups
          | Relax { alts; _ } ->
              incr epoch;
              List.iter
                (fun (a : 'k encoded_form) -> check_fixups (String.length a.bytes) a.fixups)
                alts
          | Align { boundary; _ } ->
              incr epoch;
              if not (is_pow2 boundary) then
                add (Bad_alignment { where = "section " ^ section; value = boundary })
          | Zero { length; _ } ->
              if length < 0 then add (Negative_length { section; length });
              if length <> 0 then incr epoch
          | Label_def { name; _ } -> (
              let here = (section, !epoch) in
              match Hashtbl.find_opt labels name with
              | None -> Hashtbl.add labels name here
              | Some there when there = here -> ()
              | Some _ -> add (Duplicate_label { section; name }))
          | Set_size _ -> ())
        sc.fragments)
    m.sections;
  List.iter
    (fun c ->
      if not (is_pow2 c.comm_align) then
        add (Bad_alignment { where = "common " ^ c.comm_name; value = c.comm_align }))
    m.commons;
  match List.rev !errs with [] -> Ok () | es -> Error es

let group_parents = [ ".text"; ".rodata"; ".data"; ".bss" ]

let group_parent name =
  List.find_opt
    (fun p ->
      let n = String.length p in
      String.length name > n && String.equal (String.sub name 0 n) p && name.[n] = '.')
    group_parents

let group_sections (m : 'k module_) =
  let leader p =
    match List.find_opt (fun sc -> String.equal sc.sec.sec_name p) m.sections with
    | Some sc -> Some sc.sec
    | None ->
        List.find_map
          (fun sc ->
            match group_parent sc.sec.sec_name with
            | Some q when String.equal q p -> Some sc.sec
            | _ -> None)
          m.sections
  in
  let compatible p (s : section) =
    match leader p with Some l -> l.perms = s.perms && l.kind = s.kind | None -> false
  in
  let target_of (sc : 'k section_content) =
    match group_parent sc.sec.sec_name with
    | Some p when compatible p sc.sec -> p
    | _ -> sc.sec.sec_name
  in
  let grouped =
    List.exists (fun sc -> not (String.equal (target_of sc) sc.sec.sec_name)) m.sections
  in
  if not grouped then m
  else begin
    let order = ref [] in
    let table : (string, section * 'k fragment list ref) Hashtbl.t = Hashtbl.create 16 in
    List.iter
      (fun sc ->
        let t = target_of sc in
        match Hashtbl.find_opt table t with
        | None ->
            order := t :: !order;
            Hashtbl.replace table t ({ sc.sec with sec_name = t }, ref (List.rev sc.fragments))
        | Some (sec, frags) ->
            let sec = { sec with alignment = max sec.alignment sc.sec.alignment } in
            Hashtbl.replace table t (sec, frags);
            if sc.sec.alignment > 1 then
              frags :=
                Align
                  {
                    boundary = sc.sec.alignment;
                    fill = None;
                    max_skip = None;
                    origin = Foundation.Origin.synthesized ~pass:"group-sections" ();
                  }
                :: !frags;
            frags := List.rev_append sc.fragments !frags)
      m.sections;
    let sections =
      List.rev_map
        (fun t ->
          let sec, frags = Hashtbl.find table t in
          { sec; fragments = List.rev !frags })
        !order
    in
    let renamed = List.map (fun sc -> (sc.sec.sec_name, target_of sc)) m.sections in
    let rename n = Option.value (List.assoc_opt n renamed) ~default:n in
    {
      m with
      sections;
      symbols = List.map (fun s -> { s with section = rename s.section }) m.symbols;
      declared_sections = List.sort_uniq compare (List.map rename m.declared_sections);
    }
  end
