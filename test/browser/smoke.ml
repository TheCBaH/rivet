(* Pure combining logic for the real-browser smoke harness (docs/design.md
   M5, the deferred browser-harness item). Reuses exactly the inputs and
   production entry points already proven three-way-identical by
   asm_dump.ml/manifest_dump.ml's own cram gates - Test_dump_inputs's x86_64
   snippet for parser+encoder coverage, Smoke_fixtures's multi-segment source
   for linker+image coverage - rather than inventing new ones. Deliberately a
   short deterministic summary, not a full transcript: this must not become a
   second baseline to keep in sync with asm_dump.t/manifest_dump.t.

   No side effects here: smoke_native.ml, smoke_jsoo.ml, and the Melange
   emit (melange/test/browser/dune) are each a thin adapter around
   [run] below, which is why this module alone is safe to share across all
   three backends without duplication. *)

let ok_diag = function Ok v -> v | Error e -> failwith (Rivet_foundation.Diag.render e)

let parser_encoder_summary () =
  let name = "x86_64" in
  let (module D : Rivet_target_intf.Target.DRIVER) =
    match Rivet_driver.Registry.find name with
    | Some d -> d
    | None -> failwith ("smoke: no driver for " ^ name)
  in
  let source = Rivet_foundation.Span.source ~name:(name ^ ".s") ~contents:Test_dump_inputs.x86_64 in
  let laid_out = ok_diag (D.assemble ~unit_name:"asm_test_entry" ~source ()) in
  let plan = Rivet_image.plan_of laid_out in
  let addresses =
    List.map
      (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0L))
      plan.Rivet_image.segments
  in
  let img = ok_diag (Rivet_image.bind_image laid_out ~addresses) in
  let bytes = match img.Rivet_image.segments with s :: _ -> s.Rivet_image.bytes | [] -> "" in
  Printf.sprintf "parser+encoder(%s): %d bytes\n" name (String.length bytes)

let ok_portable = function
  | Ok v -> v
  | Error e -> failwith (Rivet_driver.Portable.render_error (Err.Error.kind e))

let linker_image_summary () =
  let p =
    ok_portable
      (Rivet_driver.Portable.assemble ~entry:"start" "x86_64" ~unit_name:"m"
         ~text:Smoke_fixtures.multi_segment_source)
  in
  let b = ok_portable (Rivet_driver.Portable.bind_sequential p ~base:0x400000L ~gap:0) in
  Smoke_fixtures.summarize (Rivet_driver.Portable.manifest b)

let run () = parser_encoder_summary () ^ linker_image_summary ()
