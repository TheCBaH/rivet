(* The typed bidirectional codec EDSL (.ai/asm_plan.md §4.9.1, §6).

   An instruction description is a *value*, not a function. That is the whole
   point: an opaque encoding function cannot be inverted to give a decoder, nor
   inspected to give a conflict checker, a field-placement dump, or generated
   boundary tests. Everything here is therefore an inspectable tree with four
   interpreters over it - encode, decode, inspect, check - and adding a fifth is
   a new traversal rather than a new obligation on every target.

   Two things this package must never acquire, both enforced by
   tools/asm-check-layers.sh rather than by convention:

   - no target vocabulary. There is no ModR/M here, no REX, no condition code.
     x86's family combinators are built *from* these primitives in
     targets/x86_family; a generic combinator named after one target is how a
     retargetable core stops being retargetable.

   - no dependency above foundation. The tree is parameterized over the
     target's fixup-kind type ([('a, 'k) t]) precisely so that fixups can be
     target-specific without this package knowing what a target is.

   Bit order. A codec describes bits most-significant first, and [Bits.to_bytes]
   packs them in that order. For x86 a form is authored as a sequence of 8-bit
   chunks in memory order, so the packing *is* memory order. For the two
   fixed-width targets a form is authored as one 32-bit word, so the packing is
   the big-endian rendering of that word and the target reverses it - both are
   little-endian in memory, and every committed artifact is in memory order. *)

(* {1 Bit strings}

   One character per bit. That is not the representation a production assembler
   would choose, but an instruction is at most 120 bits and the alternative -
   packing into int64 chunks with an explicit tail - puts shift arithmetic in
   the one place where a subtle error is hardest to see and where the 31-bit CI
   legs would differ from the 63-bit ones. Being able to print a codec's output
   as literal bits is also what makes the [inspect] dumps readable. *)

module Bits = struct
  type t = string (* each character is '0' or '1', most significant first *)

  let empty = ""
  let width = String.length
  let append = ( ^ )
  let concat = String.concat ""

  let of_int64 ~width v =
    String.init width (fun i ->
        if Int64.logand (Int64.shift_right_logical v (width - 1 - i)) 1L = 1L then '1' else '0')

  let to_int64 t =
    let v = ref 0L in
    String.iter (fun c -> v := Int64.logor (Int64.shift_left !v 1) (if c = '1' then 1L else 0L)) t;
    !v

  let sub t ~pos ~width = String.sub t pos width
  let zeros n = String.make n '0'

  let to_bytes t =
    if width t mod 8 <> 0 then invalid_arg "Codec.Bits.to_bytes: width is not a multiple of 8";
    String.init
      (width t / 8)
      (fun i -> Char.chr (Int64.to_int (to_int64 (sub t ~pos:(i * 8) ~width:8))))

  let of_bytes s =
    concat
      (List.init (String.length s) (fun i -> of_int64 ~width:8 (Int64.of_int (Char.code s.[i]))))

  let to_string t = t
end

(* {1 The tree} *)

type signedness = Signed | Unsigned

(* A fixup's placement in the encoded bits. The value is not known at encode
   time - that is what makes it a fixup - so the node's own type is [unit] and
   what encoding produces is this record, which the lowered fragment pairs with
   the expression to evaluate later. Distinguishing two fixups in one
   instruction is by [name]. *)
type 'k placement = { kind : 'k; name : string; bit_offset : int; bit_width : int }

type ('a, 'k) t =
  | Empty : (unit, 'k) t
      (** Zero width, always succeeds, consumes nothing. Not decoration: it is the *absent* branch
          of an optional prefix. x86 has three of them - the operand-size prefix, the REX byte, and
          the displacement - and each is an [Alt] between a byte and nothing at all. Without a
          zero-width node the alternative would have to be spelled as a width-0 [Const], which
          {!const} rejects for good reason, or as an [Iso_fun] over a phantom field, which would
          make [width_of] and [pattern] lie about the form. *)
  | Const : { width : int; value : int64 } -> (unit, 'k) t
  | Field : { name : string; width : int; signedness : signedness } -> (int64, 'k) t
  | Fixup : { name : string; width : int; kind : 'k } -> (unit, 'k) t
  | Seq : ('a, 'k) t * ('b, 'k) t -> ('a * 'b, 'k) t
  | Iso_table : {
      name : string;
      entries : ('a * int64) list;
      equal : 'a -> 'a -> bool;
      show : 'a -> string;
      inner : (int64, 'k) t;
    }
      -> ('a, 'k) t
      (** An inspectable finite relation. Injectivity, totality against the inner field's width,
          and invertibility are all decidable from [entries], so {!check} decides them. *)
  | Iso_fun : {
      name : string;
      encode : 'a -> 'b option;
      decode : 'b -> 'a option;
      inner : ('b, 'k) t;
    }
      -> ('a, 'k) t
      (** The escape hatch, with **no statically checkable laws**. Some encodings are genuinely
          irregular - ARM's modified immediates are the M1 example - and forcing them into a table
          would mean enumerating 2^32 values. What {!check} can say about one of these is nothing;
          what proves them correct is a generated finite-domain round-trip test, which is why
          {!Finite} exists. *)
  | Alt : { name : string; alts : ('a, 'k) alt list } -> ('a, 'k) t

and ('a, 'k) alt = {
  label : string;
  priority : int;  (** lower is tried first, on both the encode and the decode side *)
  guard : 'a -> bool;  (** encode-side applicability; an arbitrary predicate, so also opaque *)
  cost : int;  (** size or speed policy for relaxation; not used for selection in M1 *)
  body : ('a, 'k) t;
}

(* {1 Constructors}

   Thin, but they are where the invariants that must hold of *every* tree are
   established, so a malformed node cannot be built and then found by [check]
   only if someone runs it. *)

let empty = Empty

let const ~width value =
  if width < 1 || width > 64 then invalid_arg "Codec.const: width outside 1..64";
  Const { width; value }

let field ?(signedness = Unsigned) ~width name =
  if width < 1 || width > 64 then invalid_arg "Codec.field: width outside 1..64";
  Field { name; width; signedness }

let fixup ~width ~kind name =
  if width < 1 || width > 64 then invalid_arg "Codec.fixup: width outside 1..64";
  Fixup { name; width; kind }

let seq a b = Seq (a, b)
let ( ** ) = seq
let iso_table ~name ~equal ~show ~entries inner = Iso_table { name; entries; equal; show; inner }
let iso_fun ~name ~encode ~decode inner = Iso_fun { name; encode; decode; inner }

let alt ?(cost = 0) ?(guard = fun _ -> true) ~label ~priority body =
  { label; priority; guard; cost; body }

let choice ~name alts = Alt { name; alts }

(* {1 Field range checking}

   §4.3's rule, at the one place it belongs: narrowing an arbitrary-precision
   value to a fixed-width field. A field carries an [int64] because every M1
   field is at most 64 bits wide, and the conversion from [Bigint.t] happens in
   the target's operand handling, where the diagnostic can name the operand. *)

let fits ~width ~signedness v =
  if width >= 64 then true
  else
    match signedness with
    | Unsigned -> Int64.unsigned_compare v (Int64.shift_left 1L width) < 0
    | Signed ->
        let half = Int64.shift_left 1L (width - 1) in
        Int64.compare v (Int64.neg half) >= 0 && Int64.compare v half < 0

let truncate ~width v =
  if width >= 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L width) 1L)

let sign_extend ~width v =
  if width >= 64 then v
  else
    let shift = 64 - width in
    Int64.shift_right (Int64.shift_left v shift) shift

(* {1 Interpreter 1 - encode}

   [form] is the path of [Alt] labels the encoding took, outermost first. It is
   §4.9's "stable diagnostic form identifier", derived rather than declared: a
   form id that had to be written down separately from the tree that produced it
   would be one more thing that can disagree with the encoding it names. *)

type 'k encoded = { bits : Bits.t; placements : 'k placement list; form : string list }
type error = { where : string; detail : string }

let pp_error ppf e = Fmt.pf ppf "%s: %s" e.where e.detail
let form_id e = String.concat "." e.form

(* Three outcomes, not two, and the distinction is the whole of [Alt]'s
   semantics. An [Iso] projection returning [None] means *this value is not this
   form* - a top-level dispatch over an instruction type declines every branch
   but one - while a field that does not fit means *this form was the right one
   and the value is out of range*. Collapsing them, as a plain [result] does,
   makes the first alternative in a list swallow every instruction and report
   its own projection failure as the error, which is exactly what the synthetic
   ISA caught. *)
type 'k attempt = Encoded of 'k encoded | Declined | Failed of error

let rec attempt : type a k. (a, k) t -> a -> k attempt =
 fun node v ->
  match node with
  | Empty -> Encoded { bits = Bits.empty; placements = []; form = [] }
  | Const { width; value } ->
      Encoded { bits = Bits.of_int64 ~width value; placements = []; form = [] }
  | Field { name; width; signedness } ->
      if not (fits ~width ~signedness v) then
        Failed
          {
            where = "field " ^ name;
            detail =
              Printf.sprintf "%Ld does not fit %d bits %s" v width
                (match signedness with Signed -> "signed" | Unsigned -> "unsigned");
          }
      else Encoded { bits = Bits.of_int64 ~width (truncate ~width v); placements = []; form = [] }
  | Fixup { name; width; kind } ->
      Encoded
        {
          bits = Bits.zeros width;
          placements = [ { kind; name; bit_offset = 0; bit_width = width } ];
          form = [];
        }
  | Seq (a, b) -> (
      let x, y = v in
      match attempt a x with
      | (Declined | Failed _) as r -> r
      | Encoded ea -> (
          match attempt b y with
          | (Declined | Failed _) as r -> r
          | Encoded eb ->
              let shift = Bits.width ea.bits in
              Encoded
                {
                  bits = Bits.append ea.bits eb.bits;
                  placements =
                    ea.placements
                    @ List.map (fun p -> { p with bit_offset = p.bit_offset + shift }) eb.placements;
                  form = ea.form @ eb.form;
                }))
  | Iso_table { name; entries; equal; show; inner } -> (
      match List.find_opt (fun (a, _) -> equal a v) entries with
      (* A table is a *declared* finite relation, so a value outside it is a
         genuine error rather than a decline: the author enumerated the domain
         and this is not in it. *)
      | None -> Failed { where = "iso_table " ^ name; detail = show v ^ " is not in the relation" }
      | Some (_, code) -> attempt inner code)
  | Iso_fun { encode = f; inner; _ } -> (
      match f v with None -> Declined | Some b -> attempt inner b)
  | Alt { name; alts } ->
      let ordered = List.sort (fun a b -> compare a.priority b.priority) alts in
      (* Priority order, first success wins, and a failure does not stop the
         search: a short form whose immediate is out of range is precisely how a
         long form gets selected. The error reported when nothing works is the
         first *applicable* one, because "300 does not fit 8 bits" is the fact
         the author needs and "this is not a nop" is not. *)
      let rec go first_error = function
        | [] -> (
            match first_error with
            | Some e -> Failed e
            | None -> Failed { where = "alt " ^ name; detail = "no alternative applies" })
        | a :: rest -> (
            if not (a.guard v) then go first_error rest
            else
              match attempt a.body v with
              | Encoded e -> Encoded { e with form = a.label :: e.form }
              | Declined -> go first_error rest
              | Failed e -> go (match first_error with None -> Some e | some -> some) rest)
      in
      go None ordered

let encode node v =
  match attempt node v with
  | Encoded e -> Ok e
  | Failed e -> Error e
  | Declined -> Error { where = "encode"; detail = "no form of this description accepts the value" }

(* {1 Interpreter 2 - decode}

   The same tree read backwards. [Alt] tries its alternatives in priority order
   and the first that consumes a prefix wins, which is what makes "several legal
   binary forms, explicit selection priority" (§4.9) decidable rather than a
   matter of which branch the author happened to write first. *)

type 'a decoded = { value : 'a; consumed : int; dform : string list }

let rec decode : type a k. (a, k) t -> Bits.t -> pos:int -> a decoded option =
 fun node bits ~pos ->
  let plain value consumed = Some { value; consumed; dform = [] } in
  match node with
  | Empty -> plain () pos
  | Const { width; value } ->
      if Bits.width bits - pos < width then None
      else if Bits.to_int64 (Bits.sub bits ~pos ~width) = truncate ~width value then
        plain () (pos + width)
      else None
  | Field { width; signedness; _ } ->
      if Bits.width bits - pos < width then None
      else
        let raw = Bits.to_int64 (Bits.sub bits ~pos ~width) in
        plain
          (match signedness with Unsigned -> raw | Signed -> sign_extend ~width raw)
          (pos + width)
  | Fixup { width; _ } -> if Bits.width bits - pos < width then None else plain () (pos + width)
  | Seq (a, b) -> (
      match decode a bits ~pos with
      | None -> None
      | Some x -> (
          match decode b bits ~pos:x.consumed with
          | None -> None
          | Some y ->
              Some { value = (x.value, y.value); consumed = y.consumed; dform = x.dform @ y.dform })
      )
  | Iso_table { entries; inner; _ } -> (
      match decode inner bits ~pos with
      | None -> None
      | Some x -> (
          match List.find_opt (fun (_, c) -> Int64.equal c x.value) entries with
          | None -> None
          | Some (a, _) -> Some { x with value = a }))
  | Iso_fun { decode = g; inner; _ } -> (
      match decode inner bits ~pos with
      | None -> None
      | Some x -> ( match g x.value with None -> None | Some a -> Some { x with value = a }))
  | Alt { alts; _ } ->
      let ordered = List.sort (fun a b -> compare a.priority b.priority) alts in
      List.fold_left
        (fun acc a ->
          match acc with
          | Some _ -> acc
          | None -> (
              match decode a.body bits ~pos with
              | None -> None
              | Some x -> Some { x with dform = a.label :: x.dform }))
        None ordered

let decode_bits node bits = decode node bits ~pos:0

(* {1 Interpreter 3 - inspect}

   A structural description, which is what the diagnostic disassembler (§6) and
   the committed cram dumps read. It is deliberately not the same as [check]'s
   output: this says what a form *is*, that says what is wrong with it. *)

type shape =
  | S_empty
  | S_const of { width : int; value : int64 }
  | S_field of { name : string; width : int; signedness : signedness }
  | S_fixup of { name : string; width : int }
  | S_seq of shape list
  | S_iso_table of { name : string; entries : int; inner : shape }
  | S_iso_fun of { name : string; inner : shape }
  | S_alt of { name : string; alts : (string * int * int * shape) list }

let rec inspect : type a k. (a, k) t -> shape = function
  | Empty -> S_empty
  | Const { width; value } -> S_const { width; value }
  | Field { name; width; signedness } -> S_field { name; width; signedness }
  | Fixup { name; width; _ } -> S_fixup { name; width }
  | Seq (a, b) -> (
      (* Flattened, so that a right-nested chain of [**] prints as the sequence
         the author wrote rather than as a comb of pairs. *)
      let tail = match inspect b with S_seq xs -> xs | s -> [ s ] in
      match inspect a with S_seq xs -> S_seq (xs @ tail) | s -> S_seq (s :: tail))
  | Iso_table { name; entries; inner; _ } ->
      S_iso_table { name; entries = List.length entries; inner = inspect inner }
  | Iso_fun { name; inner; _ } -> S_iso_fun { name; inner = inspect inner }
  | Alt { name; alts } ->
      S_alt
        {
          name;
          alts =
            List.map
              (fun a -> (a.label, a.priority, a.cost, inspect a.body))
              (List.sort (fun a b -> compare a.priority b.priority) alts);
        }

let pp_signedness ppf = function Signed -> Fmt.string ppf "s" | Unsigned -> Fmt.string ppf "u"

let rec pp_shape ppf = function
  | S_empty -> Fmt.string ppf "()"
  | S_const { width; value } -> Fmt.pf ppf "%s" (Bits.to_string (Bits.of_int64 ~width value))
  | S_field { name; width; signedness } -> Fmt.pf ppf "%s:%d%a" name width pp_signedness signedness
  | S_fixup { name; width } -> Fmt.pf ppf "<%s:%d>" name width
  | S_seq xs -> Fmt.pf ppf "@[<h>%a@]" Fmt.(list ~sep:(any " ") pp_shape) xs
  | S_iso_table { name; entries; inner } -> Fmt.pf ppf "%s[%d]{%a}" name entries pp_shape inner
  | S_iso_fun { name; inner } -> Fmt.pf ppf "%s(){%a}" name pp_shape inner
  | S_alt { name; alts } ->
      Fmt.pf ppf "@[<v2>alt %s@,%a@]" name
        Fmt.(
          list ~sep:cut (fun ppf (label, prio, cost, s) ->
              Fmt.pf ppf "@[<h>[%d cost=%d] %-16s %a@]" prio cost label pp_shape s))
        alts

(* The width a form occupies, when that is a single number. An [Alt] whose
   branches disagree has none, which is not an error - x86 instructions are
   genuinely variable-length - so this is an option rather than a failure. *)
let rec width_of : type a k. (a, k) t -> int option = function
  | Empty -> Some 0
  | Const { width; _ } | Field { width; _ } | Fixup { width; _ } -> Some width
  | Seq (a, b) -> (
      match (width_of a, width_of b) with Some x, Some y -> Some (x + y) | _ -> None)
  | Iso_table { inner; _ } -> width_of inner
  | Iso_fun { inner; _ } -> width_of inner
  | Alt { alts; _ } -> (
      match List.map (fun a -> width_of a.body) alts with
      | [] -> None
      | w :: rest -> if List.for_all (fun x -> x = w) rest then w else None)

(* {1 Interpreter 4 - check}

   **Syntactic properties only**, and the boundary is stated rather than
   implied. What this decides: field width arithmetic, constant values that do
   not fit their declared width, [Iso_table] injectivity/totality/invertibility,
   duplicate priorities, and fixed-bit patterns that make two alternatives
   ambiguous.

   What it cannot decide, and deliberately does not pretend to: whether an
   [Alt]'s guards are reachable, and whether an [Iso_fun] round-trips. Both are
   arbitrary OCaml predicates, and an arbitrary predicate is exactly as opaque
   as the escape hatch it guards. Those are generated finite-domain tests
   ({!Finite}), and every declared domain has to say why enumerating it is
   tractable. *)

type problem = { node : string; issue : string }

let problem node issue = { node; issue }
let pp_problem ppf p = Fmt.pf ppf "%s: %s" p.node p.issue

(* The fixed-bit pattern of a form: for each bit position, '0', '1', or '?'.
   [None] where the form has no single width - a variable-length [Alt] nested
   inside another has no pattern to compare. *)
let rec pattern : type a k. (a, k) t -> string option = function
  | Empty -> Some ""
  | Const { width; value } -> Some (Bits.of_int64 ~width (truncate ~width value))
  | Field { width; _ } | Fixup { width; _ } -> Some (String.make width '?')
  | Seq (a, b) -> ( match (pattern a, pattern b) with Some x, Some y -> Some (x ^ y) | _ -> None)
  | Iso_table { inner; _ } -> pattern inner
  | Iso_fun { inner; _ } -> pattern inner
  | Alt { alts; _ } -> (
      (* A nested Alt has one pattern only if every branch agrees on it. *)
      match List.map (fun a -> pattern a.body) alts with
      | [] -> None
      | p :: rest -> if List.for_all (fun x -> x = p) rest then p else None)

(* Two patterns are ambiguous when some bit string matches both: they agree
   wherever both are fixed. Different lengths are not ambiguous here, because
   the shorter one is a prefix decode and priority decides - which is the
   ordinary x86 situation. *)
let patterns_overlap a b =
  String.length a = String.length b
  &&
  let n = String.length a in
  let rec go i = i >= n || ((a.[i] = '?' || b.[i] = '?' || a.[i] = b.[i]) && go (i + 1)) in
  go 0

let rec check : type a k. (a, k) t -> problem list = function
  | Empty -> []
  | Const { width; value } ->
      if fits ~width ~signedness:Unsigned value || fits ~width ~signedness:Signed value then []
      else [ problem "const" (Printf.sprintf "%Ld does not fit %d bits" value width) ]
  | Field _ | Fixup _ -> []
  | Seq (a, b) -> check a @ check b
  | Iso_table { name; entries; equal; show; inner } ->
      let where = "iso_table " ^ name in
      let inner_width = match inner with Field { width; _ } -> Some width | _ -> None in
      let sources = List.map fst entries in
      let dup_a =
        List.filteri
          (fun i a -> List.exists (equal a) (List.filteri (fun j _ -> j < i) sources))
          sources
      in
      let codes = List.map snd entries in
      let dup_code =
        List.filteri
          (fun i c -> List.exists (Int64.equal c) (List.filteri (fun j _ -> j < i) codes))
          codes
      in
      (* Injectivity in both directions is what makes the table an isomorphism
         rather than merely a function; without the second, decode would have to
         pick arbitrarily between two source values. *)
      List.map (fun a -> problem where ("duplicate source value " ^ show a)) dup_a
      @ List.map (fun c -> problem where (Printf.sprintf "duplicate code %Ld" c)) dup_code
      @ (match inner_width with
        | None -> [ problem where "inner codec is not a single field, so totality is undecidable" ]
        | Some w ->
            List.filter_map
              (fun (a, c) ->
                if fits ~width:w ~signedness:Unsigned c then None
                else
                  Some
                    (problem where
                       (Printf.sprintf "code %Ld for %s does not fit %d bits" c (show a) w)))
              entries)
      @ check inner
  | Iso_fun { inner; _ } ->
      (* Nothing to say: the functions are opaque. Recording that here rather
         than silently returning [] is the point - a reader must not read an
         empty problem list as "checked". *)
      check inner
  | Alt { name; alts } ->
      let where = "alt " ^ name in
      let prios = List.map (fun a -> a.priority) alts in
      let dup_prio =
        List.filteri
          (fun i p -> List.exists (( = ) p) (List.filteri (fun j _ -> j < i) prios))
          prios
      in
      let pairs =
        let rec go = function [] -> [] | x :: rest -> List.map (fun y -> (x, y)) rest @ go rest in
        go (List.sort (fun a b -> compare a.priority b.priority) alts)
      in
      let ambiguous =
        List.filter_map
          (fun (a, b) ->
            match (pattern a.body, pattern b.body) with
            | Some pa, Some pb when patterns_overlap pa pb ->
                Some
                  (problem where
                     (Printf.sprintf
                        "%s and %s have overlapping fixed bits; priority %d before %d decides"
                        a.label b.label a.priority b.priority))
            | _ -> None)
          pairs
      in
      List.map (fun p -> problem where (Printf.sprintf "duplicate priority %d" p)) dup_prio
      @ ambiguous
      @ List.concat_map (fun a -> check a.body) alts

(* {1 Generated finite-domain tests}

   §4.9.1's answer to the two things [check] cannot decide. A declared domain
   must record *why* enumerating it is tractable, because "we tested the
   interesting values" and "we tested all of them" are different claims and a
   test that silently became the first is worthless as evidence for the second.
   ARM's modified-immediate domain is 4096 pairs and genuinely exhaustive; a
   32-bit scaled offset is not, and gets a deterministic boundary sample
   instead. *)

module Finite = struct
  type 'a domain = { name : string; why : string; values : 'a list; exhaustive : bool }

  let exhaustive ~name ~why values = { name; why; values; exhaustive = true }
  let sample ~name ~why values = { name; why; values; exhaustive = false }

  (* Every value in the domain must encode, and decoding the result must give
     the value back. This is the only evidence an [Iso_fun] ever gets. *)
  let round_trip (type a k) (node : (a, k) t) (dom : a domain) ~equal ~show =
    List.filter_map
      (fun v ->
        match encode node v with
        | Error e ->
            Some (Printf.sprintf "%s: encode failed: %s" (show v) (Fmt.to_to_string pp_error e))
        | Ok { bits; _ } -> (
            match decode_bits node bits with
            | None -> Some (Printf.sprintf "%s: encoded to %s, which does not decode" (show v) bits)
            | Some d ->
                if d.consumed <> Bits.width bits then
                  Some
                    (Printf.sprintf "%s: decode consumed %d of %d bits" (show v) d.consumed
                       (Bits.width bits))
                else if not (equal v d.value) then
                  Some (Printf.sprintf "%s: decoded back as %s" (show v) (show d.value))
                else None))
      dom.values

  (* Alternative reachability: a form no value in the domain selects is dead.
     [check] cannot see this - selection depends on an arbitrary guard *and* on
     whether the body's projection declines and its fields fit - and a dead
     alternative is indistinguishable from a missing one, which is the defect
     class the reachability discipline exists to catch.

     Selection is read off [encode]'s form path rather than reimplemented here.
     Reimplementing it would mean this agreed with a *copy* of the rule instead
     of with the rule. *)
  let unreachable_alts (type a k) (node : (a, k) t) (dom : a domain) =
    match node with
    | Alt { name; alts } ->
        let hit =
          List.filter_map
            (fun v -> match encode node v with Ok { form = l :: _; _ } -> Some l | _ -> None)
            dom.values
        in
        List.filter_map
          (fun a ->
            if List.mem a.label hit then None
            else
              Some
                (Printf.sprintf "alt %s: %s is selected by no value in domain %s (%s)" name a.label
                   dom.name
                   (if dom.exhaustive then "exhaustive" else "sample")))
          (List.sort (fun a b -> compare a.priority b.priority) alts)
    | _ -> []

  let pp_domain ppf d =
    Fmt.pf ppf "%s: %d values, %s@,  why: %s" d.name (List.length d.values)
      (if d.exhaustive then "exhaustive" else "deterministic sample")
      d.why
end
