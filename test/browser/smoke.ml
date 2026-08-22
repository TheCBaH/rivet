(* Pure combining logic for the real-browser smoke harness (.ai/asm_plan.md
   M5, the deferred browser-harness item). Reuses exactly the inputs and
   production entry points already proven three-way-identical by
   asm_dump.ml/manifest_dump.ml's own cram gates - Test_dump_inputs's x86_64
   snippet for parser+encoder coverage, Smoke_fixtures's multi-segment source
   for linker+image coverage - rather than inventing new ones. Deliberately a
   short deterministic summary, not a full transcript: this must not become a
   second baseline to keep in sync with asm_dump.t/manifest_dump.t.

   No side effects here: smoke_native.ml, smoke_jsoo.ml, and the Melange
   emit (asm/melange/test/browser/dune) are each a thin adapter around
   [run] below, which is why this module alone is safe to share across all
   three backends without duplication. *)

let ok_diag = function Ok v -> v | Error e -> failwith (Foundation.Diag.render e)

let parser_encoder_summary () =
  let name = "x86_64" in
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find name with
    | Some d -> d
    | None -> failwith ("smoke: no driver for " ^ name)
  in
  let source = Foundation.Span.source ~name:(name ^ ".s") ~contents:Test_dump_inputs.x86_64 in
  let laid_out = ok_diag (D.assemble ~unit_name:"asm_test_entry" ~source ()) in
  let plan = Image.plan_of laid_out in
  let addresses =
    List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
  in
  let img = ok_diag (Image.bind_image laid_out ~addresses) in
  let bytes = match img.Image.segments with s :: _ -> s.Image.bytes | [] -> "" in
  Printf.sprintf "parser+encoder(%s): %d bytes\n" name (String.length bytes)

let ok_portable = function
  | Ok v -> v
  | Error e -> failwith (Driver.Portable.render_error (Err.Error.kind e))

let linker_image_summary () =
  let p =
    ok_portable
      (Driver.Portable.assemble ~entry:"start" "x86_64" ~unit_name:"m"
         ~text:Smoke_fixtures.multi_segment_source)
  in
  let b = ok_portable (Driver.Portable.bind_sequential p ~base:0x400000L ~gap:0) in
  Smoke_fixtures.summarize (Driver.Portable.manifest b)

let run () = parser_encoder_summary () ^ linker_image_summary ()
