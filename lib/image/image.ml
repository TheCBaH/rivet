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

   The linker is still restricted - one module, one strong exported symbol, no
   imports - and every unsupported condition is an explicit rejection rather
   than a silent partial result. What M2 removed is the one-allocatable-section
   limit, because the global fixture puts its object in .data and its code in
   .text; merging sections across inputs and linking more than one module
   remain M3. *)

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
      (** placement-dependent fixups still outstanding: locally defined symbols whose absolute
          values await an address. Not "symbols we could not find" - a fixup naming a symbol this
          module never defines is rejected during planning, because [bind_image] takes section
          addresses and has no import resolver, and reporting it as bindable state would promise
          something the API cannot deliver. *)
}

(* The laid-out content. Opaque to a caller: it is [plan_image]'s working state,
   handed back to [bind_image] so the two halves cannot be applied to different
   inputs. *)
(* A fixup after layout, with the target's kind erased.

   Erasing is what keeps [laid_out] unparameterized, and therefore keeps
   [Portable]'s architecture-erased types the shape they already are. The
   evaluator is partially applied at plan time, so the one thing the kind was
   needed for travels with the record instead of the kind itself. The
   descriptive strings travel too, because the differential oracle enters
   through the erased boundary and has to classify what it finds there. *)
type resolved_fixup = {
  rf_section : string;
  rf_frag_offset : int;  (** the fragment start, section-relative: the PC base is measured here *)
  rf_byte_offset : int;  (** the patch container, section-relative *)
  rf_container : int;
  rf_pc_bias : int;
  rf_slices : Lowered_ast.slice list;
  rf_range : Lowered_ast.range;
  rf_value : Expr.t;
  rf_name : string;
  rf_kind_name : string;
  rf_family : string;
  rf_role : Lowered_ast.fixup_role;
  rf_origin : Origin.t;
  rf_evaluate : place:int64 -> target:int64 -> (int64, Diagnostic.t) result;
}

type laid_out = {
  plan : plan;
  contents : (string * string) list;  (** section name -> initialized bytes *)
  offsets : (string * int) list;  (** symbol name -> section-relative offset *)
  sizes : (string * Expr.t * int) list;  (** symbol, size expression, offset where it was written *)
  globals : string list;
  section_of : (string * string) list;
  fixups : resolved_fixup list;
  symbols : Lowered_ast.symbol list;
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

   Two phases now, because relaxation exists. Sizes are not all known up front:
   a branch encoded short or near depends on where its target lands, which
   depends on the sizes. Phase one reaches a fixpoint on that; phase two emits
   bytes for the selection it settled on.

   Alignment still only moves a fragment forward, but it is recomputed on every
   iteration rather than assumed, because a promotion upstream changes padding
   downstream. *)

let align_up n boundary = if boundary <= 1 then n else (n + boundary - 1) / boundary * boundary

(* The chosen alternative of each [Relax] fragment, by position in the section.
   Selections only ever move up their ladder, which is what makes the fixpoint
   terminate. *)
type selection = int array

let alt_bytes (a : 'k Lowered_ast.encoded_form) = String.length a.Lowered_ast.bytes

let fragment_size (sel : selection) i frag at =
  match frag with
  | Lowered_ast.Bytes { bytes; _ } -> String.length bytes
  | Lowered_ast.Relax { alts; _ } -> alt_bytes (List.nth alts sel.(i))
  | Lowered_ast.Zero { length; _ } -> length
  | Lowered_ast.Align { boundary; _ } -> align_up at boundary - at
  | Lowered_ast.Label_def _ | Lowered_ast.Set_size _ -> 0

(* Offsets of every fragment and every label under one selection. *)
let scan_offsets (sel : selection) frags =
  let at = ref 0 in
  let starts = Array.make (List.length frags) 0 in
  let labels = ref [] in
  List.iteri
    (fun i frag ->
      let size = fragment_size sel i frag !at in
      (match frag with
      | Lowered_ast.Align _ -> at := !at + size
      | Lowered_ast.Label_def { name; _ } ->
          starts.(i) <- !at;
          labels := (name, !at) :: !labels
      | _ ->
          starts.(i) <- !at;
          at := !at + size);
      if match frag with Lowered_ast.Align _ -> true | _ -> false then starts.(i) <- !at)
    frags;
  (starts, List.rev !labels, !at)

(* Can this alternative's fixups all reach, with the section laid out as it
   currently is?

   [None] means undecidable here, not "no": a reference to another section has
   no section-relative answer, and its displacement depends on a segment base
   nobody has chosen yet. Undecidable keeps the longest rung, which is always
   safe. Decidability is exactly the intra-section rule - PC-relative distances
   inside one section are independent of where the section is placed, which is
   why relaxation can run at plan time at all. *)
let alt_reaches ~evaluate ~labels ~frag_off (a : 'k Lowered_ast.encoded_form) =
  (* Being *evaluable* against this section's labels is not the same as being
     *in* this section, and the difference is the whole of the intra-section
     rule. [Expr.absolute] happily reduces [Const 0x400078] to a number, and
     comparing that number against a section-relative [place] silently assumes
     the section base is zero: the rung would be chosen for a displacement that
     no longer holds once [bind_image] picks a real base, and by then promotion
     is over.

     So decide it by measurement rather than by inspecting the expression's
     shape. Evaluate the target twice - once with the section at 0, once with it
     at a probe base, moving every label and [here] together - and keep the
     answer only if the target moved with the section. That is exactly "the
     target is a location in this section": a constant does not move (coefficient
     0), nor does a label difference, and a reference to a section we did not lay
     out here does not evaluate at all. Only a coefficient of 1 cancels against
     [place] to leave a base-independent displacement.

     The probe is arbitrary and only its difference is read, so no expression can
     collude with it; it is offset well past any plausible section size so that a
     coincidental match cannot come from small-integer arithmetic. *)
  let probe = 0x1000_0000 in
  (* [here] is the fragment address, matching [bind_image]: [.] is the
     instruction, not the PC base. Getting it wrong here would also make the
     probe misjudge which rung reaches. *)
  let env_for ~base (_ : 'k Lowered_ast.fixup) =
    {
      Expr.lookup =
        (fun n -> Option.map (fun o -> Bigint.of_int (o + base)) (List.assoc_opt n labels));
      here = Some (Bigint.of_int (frag_off + base));
    }
  in
  let at ~base f =
    match Expr.absolute (env_for ~base f) f.Lowered_ast.value with
    | Error _ -> None
    | Ok target -> Bigint.to_int64_opt target
  in
  List.fold_left
    (fun acc (f : 'k Lowered_ast.fixup) ->
      match acc with
      | Some false | None -> acc
      | Some true -> (
          match (at ~base:0 f, at ~base:probe f) with
          | Some t0, Some t1 when Int64.equal (Int64.sub t1 t0) (Int64.of_int probe) -> (
              let place = Int64.of_int (frag_off + f.Lowered_ast.pc_bias) in
              match evaluate f.Lowered_ast.kind ~place ~target:t0 with
              | Error _ -> Some false
              | Ok v -> Some (Lowered_ast.range_admits f.Lowered_ast.range v))
          | _ -> None))
    (Some true) a.Lowered_ast.fixups

(* Promotion-only fixpoint.

   Start every ladder at its shortest rung, lay the section out, and promote any
   rung that does not reach to the shortest one that does - or to the longest if
   none does. Repeat until nothing moves.

   Selections only travel one way along a finite ladder, so the state is
   monotone in a finite product order and this terminates in at most the sum of
   the ladder lengths. On exit every selection reaches, or [verify] reports the
   one that does not.

   It deliberately does not claim minimality. Alignment padding makes layout
   non-monotone in fragment size - promoting one fragment can shift others
   either way - so "smallest feasible selection" is not something this computes
   or that the plan asserts. What establishes that our choices match GNU is the
   byte-for-byte differential gate on each fixture. *)
let relax_fixpoint ~evaluate frags =
  let n = List.length frags in
  let sel = Array.make n 0 in
  let ladders =
    Array.of_list
      (List.map (function Lowered_ast.Relax { alts; _ } -> Array.of_list alts | _ -> [||]) frags)
  in
  let bound = Array.fold_left (fun acc l -> acc + max 0 (Array.length l - 1)) 0 ladders + 1 in
  let rec iterate steps =
    if steps > bound then ()
    else
      let starts, labels, _ = scan_offsets sel frags in
      let moved = ref false in
      Array.iteri
        (fun i ladder ->
          if Array.length ladder > 0 then
            let frag_off = starts.(i) in
            let reaches j = alt_reaches ~evaluate ~labels ~frag_off ladder.(j) in
            if reaches sel.(i) <> Some true then begin
              let target =
                let rec first j =
                  if j >= Array.length ladder then Array.length ladder - 1
                  else if reaches j = Some true then j
                  else first (j + 1)
                in
                first sel.(i)
              in
              if target > sel.(i) then begin
                sel.(i) <- target;
                moved := true
              end
            end)
        ladders;
      if !moved then iterate (steps + 1)
  in
  iterate 0;
  sel

let layout_section ~evaluate (sc : 'k Lowered_ast.section_content) =
  let frags = sc.Lowered_ast.fragments in
  let sel = relax_fixpoint ~evaluate frags in
  let starts, labels, _ = scan_offsets sel frags in
  let buf = Buffer.create 256 in
  let offsets = ref [] in
  let sizes = ref [] in
  let fixups = ref [] in
  let errors = ref [] in
  let emit_bytes ~frag_off ~form:_ bytes (fs : 'k Lowered_ast.fixup list) origin =
    Buffer.add_string buf bytes;
    List.iter
      (fun (f : 'k Lowered_ast.fixup) ->
        ignore origin;
        fixups := (frag_off, f) :: !fixups)
      fs
  in
  List.iteri
    (fun i frag ->
      let frag_off = starts.(i) in
      match frag with
      | Lowered_ast.Bytes { bytes; fixups = fs; origin; form } ->
          emit_bytes ~frag_off ~form bytes fs origin
      | Lowered_ast.Relax { alts; origin } -> (
          match Lowered_ast.validate_relax alts with
          | Error m -> errors := diag ~origin "image.relax-ladder" m :: !errors
          | Ok () ->
              let a = List.nth alts sel.(i) in
              (* The fixpoint promotes; if even the longest rung cannot reach,
                 that is a program that cannot be laid out, and saying so is the
                 whole point of verifying after convergence. *)
              (match alt_reaches ~evaluate ~labels ~frag_off a with
              | Some false ->
                  errors :=
                    diag ~origin "image.relax-unreachable"
                      (Printf.sprintf
                         "no encoding of this instruction reaches its target: widest form %s still \
                          out of range at section offset %d"
                         a.Lowered_ast.form frag_off)
                    :: !errors
              | Some true | None -> ());
              emit_bytes ~frag_off ~form:(Some a.Lowered_ast.form) a.Lowered_ast.bytes
                a.Lowered_ast.fixups origin)
      | Lowered_ast.Align { boundary; fills; origin } ->
          let target = align_up (Buffer.length buf) boundary in
          let pad = target - Buffer.length buf in
          (* The target supplies one fill per gap size, and this picks the one
             that fits exactly. Repeating a shorter pattern would be the same
             number of bytes and a different program: x86 pads with a single
             no-op sized for the gap, so ten bytes is one ten-byte instruction
             and not two nops that add up. *)
          if pad > 0 then
            if pad > Array.length fills || String.length fills.(pad - 1) <> pad then
              errors :=
                diag ~origin "image.align-fill"
                  (Printf.sprintf "the target supplies no %d-byte padding for a %d-byte alignment"
                     pad boundary)
                :: !errors
            else Buffer.add_string buf fills.(pad - 1)
      | Lowered_ast.Zero { length; _ } -> Buffer.add_string buf (String.make length '\000')
      | Lowered_ast.Label_def { name; _ } -> offsets := (name, Buffer.length buf) :: !offsets
      | Lowered_ast.Set_size { name; size; _ } -> sizes := (name, size, Buffer.length buf) :: !sizes)
    frags;
  (Buffer.contents buf, List.rev !offsets, List.rev !sizes, List.rev !fixups, List.rev !errors)

(* {1 plan_image}

   M1.4's restrictions are checked here, each as its own rejection with its own
   message. "Unsupported" and "wrong" must not produce the same diagnostic: a
   caller that hits the one-section limit needs to know it is a limit. *)

let plan_image ~evaluate policy (modules : 'k Lowered_ast.module_ list) =
  let errors = ref [] in
  let fail ?origin code msg = errors := diag ?origin code msg :: !errors in
  (match modules with
  | [] -> fail "image.no-modules" "no lowered modules"
  | [ _ ] -> ()
  | _ -> fail "image.multi-module" "M2 links exactly one module; two-file linking is M3");
  match modules with
  | [] -> Error (List.rev !errors)
  | m :: _ ->
      (* Several allocatable sections in one module are M2: the global fixture
         puts its object in .data and its code in .text. Merging sections
         *across inputs*, and multi-module linking, remain M3. *)
      let allocatable = m.Lowered_ast.sections in
      (match allocatable with
      | [] -> fail "image.no-section" "the module defines no allocatable section"
      | _ -> ());
      let laid =
        List.map
          (fun (sc : 'k Lowered_ast.section_content) ->
            let name = sc.Lowered_ast.sec.Lowered_ast.sec_name in
            let bytes, offs, szs, fx, errs = layout_section ~evaluate sc in
            (name, bytes, offs, szs, fx, errs))
          allocatable
      in
      let contents = List.map (fun (n, b, _, _, _, _) -> (n, b)) laid in
      let offsets = List.concat_map (fun (_, _, o, _, _, _) -> o) laid in
      let sizes = List.concat_map (fun (_, _, _, s, _, _) -> s) laid in
      let layout_errors = List.concat_map (fun (_, _, _, _, _, e) -> e) laid in
      let fixups =
        List.concat_map
          (fun (name, _, _, _, fx, _) ->
            List.map
              (fun (frag_off, (f : 'k Lowered_ast.fixup)) ->
                {
                  rf_section = name;
                  rf_frag_offset = frag_off;
                  rf_byte_offset = frag_off + f.Lowered_ast.byte_offset;
                  rf_container = f.Lowered_ast.container;
                  rf_pc_bias = f.Lowered_ast.pc_bias;
                  rf_slices = f.Lowered_ast.slices;
                  rf_range = f.Lowered_ast.range;
                  rf_value = f.Lowered_ast.value;
                  rf_name = f.Lowered_ast.name;
                  rf_kind_name = f.Lowered_ast.kind_name;
                  rf_family = f.Lowered_ast.family;
                  rf_role = f.Lowered_ast.role;
                  rf_origin = f.Lowered_ast.origin;
                  rf_evaluate = evaluate f.Lowered_ast.kind;
                })
              fx)
          laid
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
      (* A fixup naming a symbol this module never defines cannot become a bound
         image: [bind_image] takes section addresses and has no import
         resolver, so accepting it would be promising something the API cannot
         deliver. Imports and an external symbol map are M3. *)
      List.iter
        (fun f ->
          List.iter
            (fun s ->
              if not (List.mem_assoc s offsets) then
                fail ~origin:f.rf_origin "image.undefined"
                  ("fixup " ^ f.rf_name ^ " references undefined symbol " ^ s))
            (Expr.symbols f.rf_value))
        fixups;
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
      (* Which section defined each label, taken from the section that laid it
         out rather than assumed to be the first: with .text and .data both
         present, assuming would put every data symbol at a code address. *)
      let section_of =
        List.concat_map
          (fun (name, _, offs, _, _, _) -> List.map (fun (n, _) -> (n, name)) offs)
          laid
      in
      if !errors <> [] then Error (List.rev !errors)
      else
        Ok
          {
            plan =
              {
                segments;
                entry = policy.entry_symbol;
                exports;
                (* Not "symbols we could not find" - those were just rejected -
                   but placement-dependent fixups still outstanding, which is
                   what §9 says a plan reports and what binding satisfies. *)
                unresolved = List.sort_uniq compare (List.map (fun f -> f.rf_name) fixups);
              };
            contents;
            offsets;
            sizes;
            globals;
            section_of;
            fixups;
            symbols = m.Lowered_ast.symbols;
          }

(* {1 bind_image}

   The addresses arrive from outside. Everything that could not be decided
   without them is decided here: absolute symbol values, the entry, the exports,
   and the [.size] expressions - which is the whole reason [.size s, . - s] is
   not metadata. It exercises the current-location operator and symbol-relative
   subtraction, and its value is a number only once the section has an address. *)

(* Insert a logical value into its slices inside one little-endian container.
   Every target here is little-endian (exec-abi-v1.md §1), so one routine
   covers x86's byte-aligned disp32, ARM's 24-bit field inside a word, and
   AArch64's split adrp immediate alike.

   It overwrites the slice bits rather than adding to them. Addends live in the
   expression, so applying a patch twice cannot double a displacement - which
   is what makes it safe for [bind_image] to be handed a fragment whose bytes
   already contain a resolved encoding. *)
let patch_container bytes ~at ~size ~slices ~value =
  let container = ref 0L in
  for i = size - 1 downto 0 do
    container :=
      Int64.logor (Int64.shift_left !container 8) (Int64.of_int (Char.code bytes.[at + i]))
  done;
  List.iter
    (fun (s : Lowered_ast.slice) ->
      let mask =
        if s.Lowered_ast.bit_width >= 64 then -1L
        else Int64.sub (Int64.shift_left 1L s.Lowered_ast.bit_width) 1L
      in
      let piece = Int64.logand (Int64.shift_right_logical value s.Lowered_ast.value_lsb) mask in
      let cleared =
        Int64.logand !container (Int64.lognot (Int64.shift_left mask s.Lowered_ast.bit_offset))
      in
      container := Int64.logor cleared (Int64.shift_left piece s.Lowered_ast.bit_offset))
    slices;
  let out = Bytes.of_string bytes in
  for i = 0 to size - 1 do
    Bytes.set out (at + i)
      (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical !container (8 * i)) 0xFFL)))
  done;
  Bytes.to_string out

let bind_image (l : laid_out) ~(addresses : (string * int64) list) =
  let errors = ref [] in
  let fail code msg = errors := diag code msg :: !errors in
  (* Two segments at one address was M1's silent behaviour, because there was
     only ever one segment. With .text and .data both present it would map one
     over the other and the byte comparison would be against a program that
     cannot run. *)
  let placed =
    List.filter_map
      (fun s ->
        Option.map
          (fun a -> (s.seg_name, a, s.init_size + s.zero_fill))
          (List.assoc_opt s.seg_name addresses))
      l.plan.segments
  in
  List.iter
    (fun (n, a, sz) ->
      if Int64.compare a 0L < 0 || Int64.compare (Int64.add a (Int64.of_int sz)) a < 0 then
        fail "bind.overflow" (Printf.sprintf "segment %s at 0x%Lx overflows with size %d" n a sz))
    placed;
  let rec overlaps = function
    | [] -> ()
    | (n, a, sz) :: rest ->
        List.iter
          (fun (n', a', sz') ->
            let e = Int64.add a (Int64.of_int sz) and e' = Int64.add a' (Int64.of_int sz') in
            if Int64.compare a e' < 0 && Int64.compare a' e < 0 then
              fail "bind.overlap"
                (Printf.sprintf "segments %s at 0x%Lx (%d bytes) and %s at 0x%Lx (%d bytes) overlap"
                   n a sz n' a' sz'))
          rest;
        overlaps rest
  in
  overlaps placed;
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
    (* {2 Applying fixups}

       Everything a fixup needed and could not have before now: the expression
       becomes an address, [place] becomes the PC the architecture measures
       from, the target turns that into a field value, and the bits go in. *)
    let patched =
      List.fold_left
        (fun acc f ->
          let base = List.assoc_opt f.rf_section addresses in
          match base with
          | None -> acc
          | Some base -> (
              let place = Int64.add base (Int64.of_int (f.rf_frag_offset + f.rf_pc_bias)) in
              (* [.] is the address of the instruction, not the PC the
                 architecture measures from. The two differ by [pc_bias] - two
                 bytes for a short x86 branch, eight on ARM - and GAS resolves
                 [jmp .] to a self-loop, so taking [place] here would encode a
                 displacement of zero and fall through instead. *)
              let env =
                {
                  Expr.lookup = (fun n -> Option.map Bigint.of_int64 (address_of n));
                  here = Some (Bigint.of_int64 (Int64.add base (Int64.of_int f.rf_frag_offset)));
                }
              in
              match Expr.absolute env f.rf_value with
              | Error m ->
                  fail "bind.fixup" (Printf.sprintf "fixup %s: %s" f.rf_name m);
                  acc
              | Ok target -> (
                  match Bigint.to_int64_opt target with
                  | None ->
                      fail "bind.fixup"
                        (Printf.sprintf "fixup %s: target does not fit 64 bits" f.rf_name);
                      acc
                  | Some t -> (
                      match f.rf_evaluate ~place ~target:t with
                      | Error d ->
                          errors := d :: !errors;
                          acc
                      | Ok v ->
                          if not (Lowered_ast.range_admits f.rf_range v) then begin
                            fail "bind.fixup-range"
                              (Printf.sprintf
                                 "fixup %s: value %Ld does not fit %s at section offset %d"
                                 f.rf_name v
                                 (Fmt.to_to_string Lowered_ast.pp_range f.rf_range)
                                 f.rf_byte_offset);
                            acc
                          end
                          else
                            let cur =
                              match List.assoc_opt f.rf_section acc with
                              | Some b -> b
                              | None -> (
                                  try List.assoc f.rf_section l.contents with Not_found -> "")
                            in
                            let updated =
                              patch_container cur ~at:f.rf_byte_offset ~size:f.rf_container
                                ~slices:f.rf_slices ~value:v
                            in
                            (f.rf_section, updated) :: List.remove_assoc f.rf_section acc))))
        [] l.fixups
    in
    let content_of name =
      match List.assoc_opt name patched with
      | Some b -> b
      | None -> ( try List.assoc name l.contents with Not_found -> "")
    in
    let segments =
      List.map
        (fun s ->
          {
            name = s.seg_name;
            perms = s.perms;
            alignment = s.alignment;
            address = (match List.assoc_opt s.seg_name addresses with Some a -> a | None -> 0L);
            bytes = content_of s.seg_name;
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
  Fmt.pf ppf "@[<v>%a%a%a%a@]"
    Fmt.(
      list ~sep:cut (fun ppf (s : segment_plan) ->
          Fmt.pf ppf "segment %s size=%d zero=%d align=%d permissions=%a" s.seg_name s.init_size
            s.zero_fill s.alignment Perms.pp s.perms))
    p.segments
    Fmt.(option (any "@,entry " ++ string))
    p.entry
    Fmt.(list ~sep:nop (fun ppf e -> Fmt.pf ppf "@,export %s" e))
    p.exports
    (* What binding still has to do, which is the other half of what a plan is
       for: §9 splits layout from address assignment, and a plan that showed only
       what it had already decided would not say why the second step exists. *)
    Fmt.(list ~sep:nop (fun ppf u -> Fmt.pf ppf "@,unresolved %s" u))
    p.unresolved

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

(* {1 Fixup observations}

   What the differential oracle sees. It enters through the architecture-erased
   driver, so without this it would have no way to ask what references the
   assembler produced, let alone classify them - and classification needs
   binding, visibility and section relation, none of which survive as anything
   the kind could carry.

   Two variants rather than optional fields: an expression naming two symbols
   has no single symbol, and therefore no binding or visibility either. Saying
   so is more useful than four [None]s that a caller has to interpret. *)

type site = {
  o_section : string;
  o_offset : int;  (** the patch container, section-relative: where an ELF record would point *)
  o_place : int;
      (** the PC base this fixup measures from, section-relative. Carried because the ELF addend of
          a PC-relative record is [expr_addend + o_offset - o_place] and neither term is recoverable
          from the other; on x86 [o_place] is the realized instruction length past the fragment
          start, so it is not a constant anyone could substitute. *)
  o_kind_name : string;
  o_family : string;
  o_role : Lowered_ast.fixup_role;
}

(* Named rather than inline, so a classifier can take the payload on its own.
   That is not a convenience: an observation with no single symbol has no
   binding, visibility or section relation either, and a [classify] handed the
   whole sum would have a case it cannot decide - which is an invitation to
   invent an answer for it. The caller matches first and routes the other
   variant to the post-link byte comparison. *)
type symbolic_ref = {
  site : site;
  symbol : string;
  addend : int64;
  binding : [ `Local | `Global ];
  visibility : Lowered_ast.visibility;
  defined : bool;
  same_section : bool;
}

type non_normalizable = { nn_site : site; nn_expr : string }
type fixup_observation = Symbolic_ref of symbolic_ref | Non_normalizable of non_normalizable

(* An ELF record names one symbol and one addend. Only these two shapes reduce
   to that; anything else is reported as itself and gated by the post-link byte
   comparison instead of pretending to a record-for-record match.

   Folding first is what makes that "shapes", plural, rather than "spellings":
   [g + (2 + 2)] and [g + 4] are the same reference and must produce the same
   record, and matching on the unfolded tree would call the first
   non-normalizable and quietly drop it out of the comparison. [Expr.fold]
   leaves symbols and modifiers alone, so a genuine two-symbol expression still
   arrives here unreduced and is still reported as itself. *)
let normalize_ref (e : Expr.t) =
  (* A folding failure - a division by zero in a subterm, say - is not a shape
     this can name, so it takes the same route as any other unreducible one. *)
  match match Expr.fold Expr.no_env e with Ok folded -> folded | Error _ -> e with
  | Expr.Symbol s -> Some (s, 0L)
  | Expr.Binary (Expr.Add, Expr.Symbol s, Expr.Const k)
  (* Addition commutes and GAS accepts both spellings, so [4+g] is the same
     reference as [g+4]. Subtraction does not, and [4-g] is deliberately absent
     below: it is not a symbol plus an addend at all. *)
  | Expr.Binary (Expr.Add, Expr.Const k, Expr.Symbol s) ->
      Option.map (fun v -> (s, v)) (Bigint.to_int64_opt k)
  | Expr.Binary (Expr.Sub, Expr.Symbol s, Expr.Const k) ->
      Option.map (fun v -> (s, Int64.neg v)) (Bigint.to_int64_opt k)
  | _ -> None

let fixup_observations (l : laid_out) =
  List.map
    (fun f ->
      let site =
        {
          o_section = f.rf_section;
          o_offset = f.rf_byte_offset;
          o_place = f.rf_frag_offset + f.rf_pc_bias;
          o_kind_name = f.rf_kind_name;
          o_family = f.rf_family;
          o_role = f.rf_role;
        }
      in
      match normalize_ref f.rf_value with
      | None -> Non_normalizable { nn_site = site; nn_expr = Expr.to_string f.rf_value }
      | Some (symbol, addend) ->
          let sym = List.find_opt (fun s -> String.equal s.Lowered_ast.name symbol) l.symbols in
          let defined = List.mem_assoc symbol l.offsets in
          Symbolic_ref
            {
              site;
              symbol;
              addend;
              binding = (match sym with Some s when s.Lowered_ast.global -> `Global | _ -> `Local);
              visibility =
                (match sym with Some s -> s.Lowered_ast.visibility | None -> Lowered_ast.Default);
              defined;
              same_section =
                (match List.assoc_opt symbol l.section_of with
                | Some s -> String.equal s f.rf_section
                | None -> false);
            })
    l.fixups

let pp_observation ppf = function
  | Symbolic_ref o ->
      Fmt.pf ppf "%s\t0x%08x\t%s\t%s\t%s\t%s\t%s\t%s\t%s" o.site.o_section o.site.o_offset
        o.site.o_kind_name
        (Lowered_ast.fixup_role_name o.site.o_role)
        o.symbol
        (Printf.sprintf "%s0x%Lx"
           (if Int64.compare o.addend 0L < 0 then "-" else "+")
           (Int64.abs o.addend))
        (match o.binding with `Local -> "local" | `Global -> "global")
        (Lowered_ast.visibility_name o.visibility)
        (if o.defined then if o.same_section then "same-section" else "other-section"
         else "undefined")
  | Non_normalizable o ->
      Fmt.pf ppf "%s\t0x%08x\t%s\t%s\tnon-normalizable\t%s" o.nn_site.o_section o.nn_site.o_offset
        o.nn_site.o_kind_name
        (Lowered_ast.fixup_role_name o.nn_site.o_role)
        o.nn_expr

(* The plan a laid-out module carries. [laid_out] is otherwise opaque to a
   caller by convention: it is [plan_image]'s working state, handed straight
   back to [bind_image] so that the two halves of §9's contract cannot be
   applied to different inputs. The plan is the half a host is meant to read. *)
let plan_of (l : laid_out) = l.plan
