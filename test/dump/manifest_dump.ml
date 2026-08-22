(* The three-build equality driver for Driver.Portable.manifest (.ai/asm_plan.md
   M4 Phase 8). Same shape as asm_dump.ml - one source, compiled native,
   js_of_ocaml, and Melange, with one committed cram baseline the three builds
   are checked against.

   The multi-segment source and the rendering logic live in Smoke_fixtures
   (.ai/asm_plan.md M5's real-browser harness reuses both verbatim), so this
   file is now a thin adapter: assemble, bind, render, print. *)

let ok = function
  | Ok v -> v
  | Error e ->
      print_string (Driver.Portable.render_error (Err.Error.kind e));
      exit 1

let () =
  let p =
    ok
      (Driver.Portable.assemble ~entry:"start" "x86_64" ~unit_name:"m"
         ~text:Smoke_fixtures.multi_segment_source)
  in
  let b = ok (Driver.Portable.bind_sequential p ~base:0x400000L ~gap:0) in
  print_string (Smoke_fixtures.summarize (Driver.Portable.manifest b))
