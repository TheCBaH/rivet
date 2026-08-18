(* The portable in-memory API (.ai/asm_plan.md §11.6).

   Plain OCaml over strings and int64s: assembly text in, a dump or image bytes
   out. No file paths, no Unix, no JavaScript value, no pointer. It is what the
   two browser adapters expose, and what M4.5's real-browser gate will run over -
   the same API the native CLI uses, not a parallel one, which is what makes
   that gate evidence rather than a second implementation being tested.

   Lives in [driver] rather than in a package of its own so that the two browser
   adapters can be thin aliases of one implementation. They are separate
   packages because js_of_ocaml and Melange need different library stanzas, not
   because they need different code.

   §9 is visible in the shape of it. [assemble] stops at a plan; [bind] takes the
   address the *caller* chose. Nothing here allocates a page, changes
   protection, flushes a cache or calls generated code - in a browser it could
   not, since a target virtual address is a layout value and not a pointer. *)

(* This boundary's error domain (asm/docs/errors.md).

   It used to be [string], on the grounds that the caller is JavaScript and a
   [Diagnostic.t] would have to be marshalled anyway. That reasoning covers the
   *rendering*, not the type: a caller cannot tell "unknown target" from "symbol
   undefined" by matching on prose, and the two are answered completely
   differently. So the payload is typed and {!render_error} produces the same
   strings the boundary produced before.

   [`Diagnostics] is the erasure arm, and it is honest about being one: by the
   time a failure reaches here it has crossed {!Target_intf.Target.DRIVER},
   which erases the architecture, so a target's own domain cannot survive to
   this point. Wrapping the rendered list keeps the two boundary-local failures
   - which this module detects itself, and which are not diagnostics at all -
   distinguishable from everything the pipeline reports. *)
type unknown_target = { requested : string; known : string list }

type error =
  [ `Unknown_target of unknown_target
  | `Bind_multi_segment of string list
  | `Diagnostics of Foundation.Diagnostic.t list ]

let pp_error ppf : error -> unit = function
  | `Unknown_target { requested; known } ->
      Fmt.pf ppf "unknown target %s; known: %s" requested (String.concat ", " known)
  | `Bind_multi_segment names ->
      Fmt.pf ppf
        "bind.multi-segment: this plan has %d allocatable segments (%s), so one base does not \
         place it; use bind_sequential or pass an address per segment"
        (List.length names) (String.concat ", " names)
  | `Diagnostics ds -> Fmt.string ppf (Foundation.Diagnostic.render_all ds)

let error_code : error -> string = function
  | `Unknown_target _ -> "portable.unknown-target"
  | `Bind_multi_segment _ -> "bind.multi-segment"
  | `Diagnostics _ -> "portable.diagnostics"

let render_error (e : error) = Fmt.to_to_string pp_error e
let fail ?pos (e : error) : ('a, error) Err.t = Err.fail ?pos ~pp_error e

(* Wrapping a stage failure. The payload the pipeline hands over is already a
   diagnostic list, so this is the erasure arm rather than a conversion; the
   [Map] event records that the failure crossed into this domain. *)
let of_stage r = Err.map_error ~pos:__POS__ ~pp_error (fun ds -> `Diagnostics ds) r
let of_diagnostics ?pos ds = fail ?pos (`Diagnostics ds)

(* The embedder's initializer. An adapter has no [main] to run this from, and
   [Err_policy] deliberately does not apply itself at module initialization -
   linking a library should not mutate process state. So a host that cares calls
   this once at startup; a host that does not still gets deterministic output,
   because nothing here renders Err provenance (asm/docs/errors.md §3), and pays
   only for the callstack Err's default captures at each detection. *)
let set_trace_policy = Foundation.Err_policy.apply
let targets = Registry.names

let with_driver target f =
  match Registry.find target with
  | None -> fail ~pos:__POS__ (`Unknown_target { requested = target; known = targets })
  | Some (module D : Target_intf.Target.DRIVER) -> f (module D : Target_intf.Target.DRIVER)

let source ~unit_name ~text = Foundation.Span.source ~name:(unit_name ^ ".s") ~contents:text

(* {1 Dumps} *)

let dump target ~which ~unit_name ~text =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) ->
      let source = source ~unit_name ~text in
      let r =
        match which with
        | `Tokens -> D.dump_tokens ~source
        | `Source_ast -> D.dump_source_ast ~unit_name ~source
        | `Normalized_ast -> D.dump_normalized_ast ~unit_name ~source
        | `Lowered_ast -> D.dump_lowered_ast ~unit_name ~source
      in
      of_stage r)

let dump_codec target =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) -> Ok (D.dump_codec ()))

(* {1 Plan and bind}

   Two calls, because §9 has two steps. The intermediate is opaque on purpose:
   handing back the plan *and* a token that only [bind] accepts is what stops a
   caller from binding one module's addresses onto another module's layout. *)

type planned = { laid_out : Image.laid_out; target : string }

let assemble ?entry target ~unit_name ~text =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) ->
      match D.assemble ?entry ~unit_name ~source:(source ~unit_name ~text) () with
      | Error ds -> of_stage (Error ds)
      | Ok laid_out -> Ok { laid_out; target })

(* M3's multi-module entry point, returning the same [planned] type
   [assemble] does - [Image.laid_out] is already architecture-erased and
   already carries however many inputs went into it, so nothing downstream
   ([plan_text], [bind], [fixup_observations]) needs a second version of
   itself just because this one exists. *)
let assemble_many ?entry target (sources : (string * string) list) =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) ->
      let sources =
        List.map (fun (unit_name, text) -> (unit_name, source ~unit_name ~text)) sources
      in
      match D.assemble_many ?entry sources () with
      | Error ds -> of_stage (Error ds)
      | Ok laid_out -> Ok { laid_out; target })

let plan_text p = Fmt.to_to_string Image.pp_plan (Image.plan_of p.laid_out)

(* Rendered rather than structured, for the same reason diagnostics are: the
   caller on the far side of this boundary is JavaScript, and one line per
   observation in the oracle's own column order is what it would have to
   marshal [Image.fixup_observation] into anyway. *)
let fixup_observations p =
  List.map (fun o -> Fmt.to_to_string Image.pp_observation o) (Image.fixup_observations p.laid_out)

let segments p =
  List.map
    (fun (s : Image.segment_plan) ->
      (s.Image.seg_name, s.Image.init_size, s.Image.zero_fill, s.Image.alignment))
    (Image.plan_of p.laid_out).Image.segments

type bound = { image : Image.t; target : string }

let bind p ~addresses =
  match of_stage (Image.bind_image p.laid_out ~addresses) with
  | Error _ as e -> e
  | Ok image -> Ok { image; target = p.target }

let planned_segments p = (Image.plan_of p.laid_out).Image.segments

(* The common case, spelled out so a caller with one section does not have to
   build an association list to say the obvious thing.

   With several segments there is no obvious thing to say, so it refuses rather
   than guessing. Putting them all at [base] is the guess it used to make, and
   [bind_image] would have caught the resulting overlap - but only by accident
   of the segments having nonzero size, and with a message about addresses
   rather than about the caller having asked a question with no single answer.
   A caller that wants one is [bind_sequential]. *)
let bind_at p ~base =
  match planned_segments p with
  | [ s ] -> bind p ~addresses:[ (s.Image.seg_name, base) ]
  | segments ->
      fail ~pos:__POS__
        (`Bind_multi_segment (List.map (fun (s : Image.segment_plan) -> s.Image.seg_name) segments))

(* The documented deterministic multi-segment layout: segments in plan order,
   each aligned up from the end of the previous one plus [gap]. Deterministic is
   the point - a caller that only needs *some* valid placement should not have
   to invent one, and two callers that invent different ones would produce two
   images from one plan. A caller with real addresses still passes them to
   [bind]; this is for the ones that have none. *)
let bind_sequential p ~base ~gap =
  let align_up v a =
    if a <= 1 then v
    else
      Int64.mul (Int64.div (Int64.add v (Int64.of_int (a - 1))) (Int64.of_int a)) (Int64.of_int a)
  in
  let _, addresses =
    List.fold_left
      (fun (at, acc) (s : Image.segment_plan) ->
        let a = align_up at s.Image.alignment in
        ( Int64.add
            (Int64.add a (Int64.of_int (s.Image.init_size + s.Image.zero_fill)))
            (Int64.of_int gap),
          (s.Image.seg_name, a) :: acc ))
      (base, []) (planned_segments p)
  in
  bind p ~addresses:(List.rev addresses)

let image_text b = Fmt.to_to_string Image.pp b.image

let section_bytes b name =
  match
    List.find_opt (fun (s : Image.segment) -> String.equal s.Image.name name) b.image.Image.segments
  with
  (* A NOBITS segment's [bytes] holds only its real ([init_size]) prefix -
     always empty until common-symbol storage exists (§7) - so a caller that
     wants to materialize memory gets the full logical extent, real bytes
     followed by [zero_fill] zero bytes, rather than having to special-case
     the segment kind itself. A no-op for every PROGBITS segment, since
     [zero_fill] is 0 there. *)
  | Some s -> Some (s.Image.bytes ^ String.make s.Image.zero_fill '\000')
  | None -> None

let entry b = b.image.Image.entry
let exports b = b.image.Image.exports

(* {1 Manifest}

   M4 Phase 7 (.ai/asm_plan.md): a pure aggregation over the accessors above,
   for a caller that wants one call rather than [entry]/[exports]/[segments]/
   [section_bytes] stitched together by hand. Deliberately not the QEMU wire
   format ([test/oracle/manifest.ml] is test-only and reaches into [Abi]/
   [Abi_v2], which has no place in a production package per .ai/asm_plan.md
   §9) - this is a plain, self-contained record a browser embedder can read
   directly or marshal to whatever shape its own host wants.

   [bytes] reuses [section_bytes]'s exact zero-fill convention: a NOBITS
   segment's logical extent is the real prefix followed by [zero_fill] zero
   bytes, materialized as one string. A large BSS segment therefore costs a
   same-size string in memory - the same resource caveat [section_bytes]
   already carries, restated here because [manifest] is the entry point most
   callers will actually use. *)
type manifest_segment = { name : string; address : int64; perms : Asm_core.Perms.t; bytes : string }

type manifest = {
  target : string;
  entry : int64 option;
  exports : (string * int64) list;
  segments : manifest_segment list;
}

let manifest (b : bound) =
  {
    target = b.target;
    entry = b.image.Image.entry;
    exports = b.image.Image.exports;
    segments =
      List.map
        (fun (s : Image.segment) ->
          {
            name = s.Image.name;
            address = s.Image.address;
            perms = s.Image.perms;
            bytes = s.Image.bytes ^ String.make s.Image.zero_fill '\000';
          })
        b.image.Image.segments;
  }

(* {1 Disassembly} *)

let disassemble (b : bound) ~mode ~address bytes =
  with_driver b.target (fun (module D : Target_intf.Target.DRIVER) ->
      let r =
        match mode with
        | `Canonical -> D.dump_disasm_canonical ~address bytes
        | `Diagnostic -> D.dump_disasm_diagnostic ~address bytes
      in
      of_stage r)
