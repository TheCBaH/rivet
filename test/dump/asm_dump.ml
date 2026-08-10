(* The three-build equality driver (.ai/asm_plan.md M1.7, §11.6).

   Emits every dump of asm/docs/contracts.md §1, for every target, from one
   source that is compiled three ways: native OCaml, js_of_ocaml under Node, and
   Melange under Node. The cram baseline is committed once, so running the same
   transcript under all three backends is literally an equality check.

   The inputs come from Test_dump_inputs, which explains why they are string
   literals rather than files. *)

let ok = function
  | Ok v -> v
  | Error ds ->
      print_string (Foundation.Diagnostic.render_all ds);
      exit 1

let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let banner title = Printf.printf "########## %s\n" title

let run (name, text) =
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find name with Some d -> d | None -> exit 1
  in
  let source = Foundation.Span.source ~name:(name ^ ".s") ~contents:text in
  let unit_name = "asm_test_entry" in
  banner (name ^ " tokens");
  print_string (ok (D.dump_tokens ~source));
  banner (name ^ " source ast");
  print_endline (ok (D.dump_source_ast ~unit_name ~source));
  banner (name ^ " normalized ast");
  print_endline (ok (D.dump_normalized_ast ~unit_name ~source));
  banner (name ^ " lowered ast");
  print_endline (ok (D.dump_lowered_ast ~unit_name ~source));
  let laid_out = ok (D.assemble ~unit_name ~source ()) in
  let plan = Image.plan_of laid_out in
  banner (name ^ " plan");
  print_endline (Fmt.to_to_string Image.pp_plan plan);
  let addresses =
    List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
  in
  let img = ok (Image.bind_image laid_out ~addresses) in
  banner (name ^ " image");
  print_endline (Fmt.to_to_string Image.pp img);
  let bytes = match img.Image.segments with s :: _ -> s.Image.bytes | [] -> "" in
  banner (name ^ " bytes");
  print_endline (hex bytes);
  banner (name ^ " disasm canonical");
  print_string (ok (D.dump_disasm_canonical ~address:0L bytes));
  banner (name ^ " disasm diagnostic");
  print_string (ok (D.dump_disasm_diagnostic ~address:0L bytes));
  banner (name ^ " codec");
  print_string (D.dump_codec ())

let () = List.iter run Test_dump_inputs.all
