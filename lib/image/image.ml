(* The internal linker and the portable image contract (.ai/asm_plan.md §8, §9).

   The production API stops here, and the stopping point is the design rather
   than an omission. Everything below manipulates ordinary pure-OCaml values and
   records intended R/W/X permissions as *metadata*; nothing allocates a page,
   changes protection, flushes a cache, or calls generated code. That is what
   makes the same API work in a browser, where a target virtual address is a
   layout value and not a JavaScript pointer.

   Two steps, not one (§9):

   - [plan_image] lays fragments out, resolves symbols to section-relative
     offsets, and reports sizes, alignments, permissions, the entry identity and
     the exports - everything a host needs to decide *where* to put things.
   - [bind_image] takes the addresses the host chose, validates them against the
     constraints the plan stated, evaluates the expressions that could not be
     evaluated before addresses existed, and returns byte-exact segments.

   Collapsing them would force the assembler to choose addresses, which is
   precisely the decision an embedding host has to make.

   M1's linker is deliberately restricted - one module, one allocatable section,
   one strong exported symbol, no relaxation - and every unsupported condition
   is an explicit rejection rather than a silent partial result. *)

open Foundation
open Asm_core

(* {1 Layout policy (§8)} *)

type layout_policy = {
  entry_symbol : string option;
  exported : string list option;  (** [None] exports every global symbol *)
  max_size : int option;
}

let default_policy = { entry_symbol = None; exported = None; max_size = None }

(* {1 What a plan says}

   Sizes and constraints, not addresses: this is the half of the contract the
   host reads before it has decided anything. *)

type segment_plan = {
  seg_name : string;
  perms : Perms.t;
  alignment : int;
  init_size : int;
  zero_fill : int;
}

type plan = {
  segments : segment_plan list;
  entry : string option;
  exports : string list;
  unresolved : string list;
      (** placement-dependent fixups still outstanding. Empty for every M1 input, because .text in
          all four fixtures is relocation-free - the only relocations are the .eh_frame entries
          .cfi_* would generate, and those are discarded. *)
}

(* The laid-out content. Opaque to a caller: it is [plan_image]'s working state,
   handed back to [bind_image] so the two halves cannot be applied to different
   inputs. *)
type laid_out = {
  plan : plan;
  contents : (string * string) list;  (** section name -> initialized bytes *)
  offsets : (string * int) list;  (** symbol name -> section-relative offset *)
  sizes : (string * Expr.t * int) list;  (** symbol, size expression, offset where it was written *)
  globals : string list;
  section_of : (string * string) list;
}

(* {1 The bound image (§9)} *)

type segment = {
  name : string;
  perms : Perms.t;
  alignment : int;
  address : int64;
  bytes : string;
  zero_fill : int;
}

type t = {
  segments : segment list;
  entry : int64 option;
  exports : (string * int64) list;
  symbol_sizes : (string * int64) list;
}

let diag ?origin code message =
  Diagnostic.error ~code ~message
    ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"image" ())
    ()

(* {1 Laying fragments out}

   One pass, because M1 has no relaxation: every fragment's size is known before
   layout starts, so a second pass could not change anything. Alignment is the
   only thing that moves a fragment, and it moves it forward only. *)

let align_up n boundary = if boundary <= 1 then n else (n + boundary - 1) / boundary * boundary

let layout_section (sc : 'k Lowered_ast.section_content) =
  let buf = Buffer.create 256 in
  let offsets = ref [] in
  let sizes = ref [] in
  let errors = ref [] in
  List.iter
    (fun frag ->
      match frag with
      | Lowered_ast.Bytes { bytes; fixups; origin; _ } ->
          if fixups <> [] then
            errors :=
              diag ~origin "image.fixup" "M1 accepts no fixups: .text is relocation-free" :: !errors;
          Buffer.add_string buf bytes
      | Lowered_ast.Align { boundary; fill; _ } ->
          let target = align_up (Buffer.length buf) boundary in
          let pad = target - Buffer.length buf in
          (* The target chooses the fill. A zero-filled gap in an executable
             section is a decodable instruction, so padding code with zeroes is
             not a neutral default - it is a different program. *)
          let unit = max 1 (String.length fill) in
          for i = 0 to pad - 1 do
            Buffer.add_char buf fill.[i mod unit]
          done
      | Lowered_ast.Zero { length; _ } -> Buffer.add_string buf (String.make length '\000')
      | Lowered_ast.Label_def { name; _ } -> offsets := (name, Buffer.length buf) :: !offsets
      | Lowered_ast.Set_size { name; size; _ } -> sizes := (name, size, Buffer.length buf) :: !sizes)
    sc.Lowered_ast.fragments;
  (Buffer.contents buf, List.rev !offsets, List.rev !sizes, List.rev !errors)

(* {1 plan_image}

   M1.4's restrictions are checked here, each as its own rejection with its own
   message. "Unsupported" and "wrong" must not produce the same diagnostic: a
   caller that hits the one-section limit needs to know it is a limit. *)

let plan_image policy (modules : 'k Lowered_ast.module_ list) =
  let errors = ref [] in
  let fail ?origin code msg = errors := diag ?origin code msg :: !errors in
  (match modules with
  | [] -> fail "image.no-modules" "no lowered modules"
  | [ _ ] -> ()
  | _ -> fail "image.multi-module" "M1 links exactly one module; two-file linking is M3");
  match modules with
  | [] -> Error (List.rev !errors)
  | m :: _ ->
      let allocatable = m.Lowered_ast.sections in
      (match allocatable with
      | [] -> fail "image.no-section" "the module defines no allocatable section"
      | [ _ ] -> ()
      | _ ->
          fail "image.multi-section"
            "M1 links exactly one allocatable section; declared-but-unallocated sections such as \
             .note.GNU-stack do not count and multiple real sections are M3");
      let contents, offsets, sizes, layout_errors =
        match allocatable with
        | [] -> ([], [], [], [])
        | sc :: _ ->
            let bytes, offs, szs, errs = layout_section sc in
            ([ (sc.Lowered_ast.sec.Lowered_ast.sec_name, bytes) ], offs, szs, errs)
      in
      errors := List.rev_append layout_errors !errors;
      let globals =
        List.filter_map
          (fun s -> if s.Lowered_ast.global then Some s.Lowered_ast.name else None)
          m.Lowered_ast.symbols
      in
      (* One strong exported symbol, and a duplicate definition is a rejection
         rather than a last-wins. *)
      let dup =
        List.filter
          (fun n -> List.length (List.filter (String.equal n) (List.map fst offsets)) > 1)
          globals
      in
      List.iter (fun n -> fail "image.duplicate" ("duplicate definition of " ^ n)) dup;
      List.iter
        (fun s ->
          if not (List.mem_assoc s.Lowered_ast.name offsets) then
            fail "image.undefined"
              ("symbol " ^ s.Lowered_ast.name ^ " is declared but never defined"))
        m.Lowered_ast.symbols;
      let exports = match policy.exported with None -> globals | Some names -> names in
      List.iter
        (fun n ->
          if not (List.mem_assoc n offsets) then
            fail "image.export" ("exported symbol " ^ n ^ " is not defined"))
        exports;
      (match policy.entry_symbol with
      | Some e when not (List.mem_assoc e offsets) ->
          fail "image.entry" ("entry symbol " ^ e ^ " is not defined")
      | _ -> ());
      let segments =
        List.map
          (fun (sc : 'k Lowered_ast.section_content) ->
            let bytes =
              try List.assoc sc.Lowered_ast.sec.Lowered_ast.sec_name contents with Not_found -> ""
            in
            {
              seg_name = sc.Lowered_ast.sec.Lowered_ast.sec_name;
              perms = sc.Lowered_ast.sec.Lowered_ast.perms;
              alignment = sc.Lowered_ast.sec.Lowered_ast.alignment;
              init_size = String.length bytes;
              zero_fill = 0;
            })
          allocatable
      in
      (match policy.max_size with
      | Some limit ->
          let total = List.fold_left (fun a s -> a + s.init_size + s.zero_fill) 0 segments in
          if total > limit then
            fail "image.too-large" (Printf.sprintf "image is %d bytes, limit is %d" total limit)
      | None -> ());
      if !errors <> [] then Error (List.rev !errors)
      else
        Ok
          {
            plan = { segments; entry = policy.entry_symbol; exports; unresolved = [] };
            contents;
            offsets;
            sizes;
            globals;
            section_of =
              List.map
                (fun (n, _) ->
                  ( n,
                    match allocatable with
                    | sc :: _ -> sc.Lowered_ast.sec.Lowered_ast.sec_name
                    | [] -> "" ))
                offsets;
          }

(* {1 bind_image}

   The addresses arrive from outside. Everything that could not be decided
   without them is decided here: absolute symbol values, the entry, the exports,
   and the [.size] expressions - which is the whole reason [.size s, . - s] is
   not metadata. It exercises the current-location operator and symbol-relative
   subtraction, and its value is a number only once the section has an address. *)

let bind_image (l : laid_out) ~(addresses : (string * int64) list) =
  let errors = ref [] in
  let fail code msg = errors := diag code msg :: !errors in
  List.iter
    (fun s ->
      if not (List.mem_assoc s.seg_name addresses) then
        fail "bind.address" ("no address for segment " ^ s.seg_name))
    l.plan.segments;
  List.iter
    (fun s ->
      match List.assoc_opt s.seg_name addresses with
      | Some a when s.alignment > 1 && Int64.rem a (Int64.of_int s.alignment) <> 0L ->
          fail "bind.alignment"
            (Printf.sprintf "segment %s needs %d-byte alignment, address is 0x%Lx" s.seg_name
               s.alignment a)
      | _ -> ())
    l.plan.segments;
  if !errors <> [] then Error (List.rev !errors)
  else
    let address_of name =
      match List.assoc_opt name l.offsets with
      | None -> None
      | Some off -> (
          match List.assoc_opt name l.section_of with
          | None -> None
          | Some sect -> (
              match List.assoc_opt sect addresses with
              | None -> None
              | Some base -> Some (Int64.add base (Int64.of_int off))))
    in
    let env_at here_offset section =
      {
        Expr.lookup = (fun n -> Option.map Bigint.of_int64 (address_of n));
        here =
          (match List.assoc_opt section addresses with
          | None -> None
          | Some base -> Some (Bigint.of_int64 (Int64.add base (Int64.of_int here_offset))));
      }
    in
    let symbol_sizes =
      List.filter_map
        (fun (name, expr, at) ->
          let section = match List.assoc_opt name l.section_of with Some s -> s | None -> "" in
          match Expr.absolute (env_at at section) expr with
          | Ok v -> Some (name, Option.value ~default:0L (Bigint.to_int64_opt v))
          | Error m ->
              fail "bind.size" (Printf.sprintf "size of %s: %s" name m);
              None)
        l.sizes
    in
    let segments =
      List.map
        (fun s ->
          {
            name = s.seg_name;
            perms = s.perms;
            alignment = s.alignment;
            address = (match List.assoc_opt s.seg_name addresses with Some a -> a | None -> 0L);
            bytes = (try List.assoc s.seg_name l.contents with Not_found -> "");
            zero_fill = s.zero_fill;
          })
        l.plan.segments
    in
    let entry = match l.plan.entry with None -> None | Some e -> address_of e in
    let exports =
      List.filter_map (fun n -> Option.map (fun a -> (n, a)) (address_of n)) l.plan.exports
    in
    if !errors <> [] then Error (List.rev !errors)
    else Ok { segments; entry; exports; symbol_sizes }

(* {1 Rendering}

   The canonical dump for the [--dump-image] boundary. Addresses are the
   caller's, so this is only meaningful after binding. *)

(* The annotation is load-bearing: [t] declares a [segments] field too, and it
   is declared later, so without it OCaml resolves [p.segments] to [t]'s. *)
let pp_plan ppf (p : plan) =
  Fmt.pf ppf "@[<v>%a%a%a@]"
    Fmt.(
      list ~sep:cut (fun ppf (s : segment_plan) ->
          Fmt.pf ppf "segment %s size=%d zero=%d align=%d permissions=%a" s.seg_name s.init_size
            s.zero_fill s.alignment Perms.pp s.perms))
    p.segments
    Fmt.(option (any "@,entry " ++ string))
    p.entry
    Fmt.(list ~sep:nop (fun ppf e -> Fmt.pf ppf "@,export %s" e))
    p.exports

let pp ppf (t : t) =
  Fmt.pf ppf "@[<v>%a%a%a@]"
    Fmt.(
      list ~sep:cut (fun ppf (s : segment) ->
          Fmt.pf ppf "section %s address=0x%Lx size=%d permissions=%a" s.name s.address
            (String.length s.bytes) Perms.pp s.perms))
    t.segments
    Fmt.(option (fun ppf a -> Fmt.pf ppf "@,entry 0x%Lx" a))
    t.entry
    Fmt.(
      list ~sep:nop (fun ppf (n, a) ->
          (* The size is printed only when a [.size] directive gave the symbol
             one. Defaulting to zero would make "no .size directive" and
             ".size s, 0" print identically, and the first is the common case. *)
          match List.assoc_opt n t.symbol_sizes with
          | Some sz -> Fmt.pf ppf "@,export %s = 0x%Lx size=%Ld" n a sz
          | None -> Fmt.pf ppf "@,export %s = 0x%Lx" n a))
    t.exports

(* The plan a laid-out module carries. [laid_out] is otherwise opaque to a
   caller by convention: it is [plan_image]'s working state, handed straight
   back to [bind_image] so that the two halves of §9's contract cannot be
   applied to different inputs. The plan is the half a host is meant to read. *)
let plan_of (l : laid_out) = l.plan
