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

type error = string
(** Diagnostics arrive rendered, because the caller across this boundary is
    JavaScript and a [Diagnostic.t] would have to be marshalled into something
    it can read anyway. A caller that wants the structured form uses
    [Registry] directly - it is not hidden, it is just not the shape this
    boundary is for. *)

let render ds = Foundation.Diagnostic.render_all ds
let targets = Registry.names

let with_driver target f =
  match Registry.find target with
  | None -> Error ("unknown target " ^ target ^ "; known: " ^ String.concat ", " targets)
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
      match r with Ok s -> Ok s | Error ds -> Error (render ds))

let dump_codec target =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) -> Ok (D.dump_codec ()))

(* {1 Plan and bind}

   Two calls, because §9 has two steps. The intermediate is opaque on purpose:
   handing back the plan *and* a token that only [bind] accepts is what stops a
   caller from binding one module's addresses onto another module's layout. *)

type planned = { laid_out : Image.laid_out; target : string }

let assemble target ~unit_name ~text =
  with_driver target (fun (module D : Target_intf.Target.DRIVER) ->
      match D.assemble ~unit_name ~source:(source ~unit_name ~text) with
      | Error ds -> Error (render ds)
      | Ok laid_out -> Ok { laid_out; target })

let plan_text p = Fmt.to_to_string Image.pp_plan (Image.plan_of p.laid_out)

let segments p =
  List.map
    (fun (s : Image.segment_plan) ->
      (s.Image.seg_name, s.Image.init_size, s.Image.zero_fill, s.Image.alignment))
    (Image.plan_of p.laid_out).Image.segments

type bound = { image : Image.t; target : string }

let bind p ~addresses =
  match Image.bind_image p.laid_out ~addresses with
  | Error ds -> Error (render ds)
  | Ok image -> Ok { image; target = p.target }

(* The common case, spelled out so a caller with one section does not have to
   build an association list to say the obvious thing. *)
let bind_at p ~base =
  bind p
    ~addresses:
      (List.map
         (fun (s : Image.segment_plan) -> (s.Image.seg_name, base))
         (Image.plan_of p.laid_out).Image.segments)

let image_text b = Fmt.to_to_string Image.pp b.image

let section_bytes b name =
  match
    List.find_opt (fun (s : Image.segment) -> String.equal s.Image.name name) b.image.Image.segments
  with
  | Some s -> Some s.Image.bytes
  | None -> None

let entry b = b.image.Image.entry
let exports b = b.image.Image.exports

(* {1 Disassembly} *)

let disassemble b ~mode ~address bytes =
  with_driver b.target (fun (module D : Target_intf.Target.DRIVER) ->
      let r =
        match mode with
        | `Canonical -> D.dump_disasm_canonical ~address bytes
        | `Diagnostic -> D.dump_disasm_diagnostic ~address bytes
      in
      match r with Ok s -> Ok s | Error ds -> Error (render ds))
