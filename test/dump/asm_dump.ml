(* The three-build equality driver (docs/design.md M1.7, §11.6).

   Emits every dump of docs/contracts.md §1, for every target, from one
   source that is compiled three ways: native OCaml, js_of_ocaml under Node, and
   Melange under Node. The cram baseline is committed once, so running the same
   transcript under all three backends is literally an equality check.

   The inputs come from Test_dump_inputs, which explains why they are string
   literals rather than files. *)

(* [Rivet_foundation.Diag.render] rather than [Diagnostic.render_all]: the driver's
   failures are wrapped, and the rendering is deliberately of the payload alone -
   docs/errors.md §3 keeps Err provenance out of anything a baseline
   compares. *)
let ok = function
  | Ok v -> v
  | Error e ->
      print_string (Rivet_foundation.Diag.render e);
      exit 1

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let banner title = Printf.printf "########## %s\n" title

let run (i : Rivet_test_dump_inputs.input) =
  let (module D : Rivet_target_intf.Target.DRIVER) =
    match Rivet_driver.Registry.find i.target with Some d -> d | None -> exit 1
  in
  let name = i.target ^ " " ^ i.case in
  let source = Rivet_foundation.Span.source ~name:i.file ~contents:i.text in
  let unit_name = Filename.remove_extension (Filename.basename i.file) in
  banner (name ^ " tokens");
  print_string (ok (D.dump_tokens ~source));
  banner (name ^ " source ast");
  print_endline (ok (D.dump_source_ast ~unit_name ~source));
  banner (name ^ " normalized ast");
  print_endline (ok (D.dump_normalized_ast ~unit_name ~source ()));
  banner (name ^ " lowered ast");
  print_endline (ok (D.dump_lowered_ast ~unit_name ~source ()));
  let laid_out = ok (D.assemble ~unit_name ~source ()) in
  let plan = Rivet_image.plan_of laid_out in
  banner (name ^ " plan");
  print_endline (Fmt.to_to_string Rivet_image.pp_plan plan);
  let addresses =
    List.map
      (fun (s : Rivet_image.segment_plan) -> (s.Rivet_image.seg_name, 0L))
      plan.Rivet_image.segments
  in
  let img = ok (Rivet_image.bind_image laid_out ~addresses) in
  banner (name ^ " image");
  print_endline (Fmt.to_to_string Rivet_image.pp img);
  let bytes = match img.Rivet_image.segments with s :: _ -> s.Rivet_image.bytes | [] -> "" in
  banner (name ^ " bytes");
  print_endline (hex bytes);
  banner (name ^ " disasm canonical");
  print_string (ok (D.dump_disasm_canonical ~address:0L bytes));
  banner (name ^ " disasm diagnostic");
  print_string (ok (D.dump_disasm_diagnostic ~address:0L bytes));
  banner (name ^ " codec");
  print_string (D.dump_codec ())

let () = List.iter run Rivet_test_dump_inputs.inputs
