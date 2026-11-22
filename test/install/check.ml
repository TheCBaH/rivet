(* The installed libraries against test/coherence's fixtures, through the public
   rivet.test_support: text, direct normalized AST and direct lowered module must
   agree with each other and with the outputs frozen in test_coherence_paths.ml. *)

module C = Test_coherence
module P = C.P

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 1)
    fmt

let ok = function Ok x -> x | Error ds -> fail "%s" (Rivet_foundation.Diag.render ds)

let lines s =
  String.split_on_char '\n' s |> List.map String.trim |> List.filter (fun l -> l <> "")

let expect what got want =
  if lines got <> lines want then fail "%s:\n--- got\n%s\n--- want\n%s" what got want

let image_of low =
  let l = ok (P.plan low) in
  let plan = Image.plan_of l in
  let addresses =
    List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, 0L)) plan.Image.segments
  in
  Fmt.to_to_string Image.pp (ok (Image.bind_image l ~addresses))

let () =
  let parsed, state = C.from_text () in
  expect "normalized, text vs direct" (C.render_normalized parsed)
    (C.render_normalized C.direct_normalized);
  let lowered = ok (P.lower ~state:C.T.default_state C.direct_normalized) in
  expect "lowered, direct" (C.render_lowered lowered) (C.render_lowered C.direct_lowered);
  expect "lowered, frozen" (C.render_lowered lowered)
    {|lowered asm_test_entry
      section .text r-x align=4
        align 4
        label asm_test_entry
        label .LFB0
        bytes 40 05 80 52              [aarch64.movz]
        bytes c0 03 5f d6              [aarch64.ret]
        label .LFE0
        size asm_test_entry = (. - asm_test_entry)
      global asm_test_entry function in .text size=(. - asm_test_entry)
      local .LFB0 notype in .text
      local .LFE0 notype in .text
      declared .note.GNU-stack (not allocated)|};
  let image = image_of (ok (P.lower ~state parsed)) in
  expect "image, text vs normalized" image (image_of lowered);
  expect "image, text vs lowered" image (image_of C.direct_lowered);
  expect "image, frozen" image
    {|section .text address=0x0 size=8 permissions=r-x
      entry 0x0
      export asm_test_entry = 0x0 size=8|};
  (* One Err: Foundation's errors are values of the err_trace this program links. *)
  (match Rivet_foundation.Bigint.of_string "not a number" with
  | Ok _ -> fail "Bigint parsed a non-number"
  | Error e -> (
      let r : (Rivet_foundation.Bigint.t, Rivet_foundation.Bigint.error) Err.t = Error e in
      match Err.payload r with Error _ -> () | Ok _ -> fail "Err.payload lost the error"));
  print_string image
