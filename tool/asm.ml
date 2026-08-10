(* The `asm` CLI.

   Outside the production closure by construction: it is an executable, so
   nothing can depend on it, and tools/asm-check-purity.sh classifies tool/ as a
   non-production tree. This is where file I/O is allowed to live. §9's rule is
   that none of it leaks back into a library, which is why the whole of this file
   is argument handling, reading a file, and printing - every decision it makes
   is made in [Driver].

   Argument parsing is hand-written rather than cmdliner. The production closure
   must build under Melange and js_of_ocaml, and a dependency added "only for the
   CLI" is exactly the kind of edge the purity audit exists to catch; keeping the
   tool's dependencies to [driver] alone means there is nothing to audit. *)

let usage =
  String.concat "\n"
    [
      "usage: asm --target <t> [options] <file.s>";
      "";
      "  --target <t>            one of: " ^ String.concat ", " Driver.Registry.names;
      "  --fixed-base <addr>     bind the image at this address (hex with 0x, or decimal)";
      "";
      "  --dump-tokens           the lexer";
      "  --dump-source-ast       after parsing";
      "  --dump-normalized-ast   after simplify";
      "  --dump-lowered-ast      after lowering";
      "  --dump-image            the plan, or the bound image with --fixed-base";
      "  --dump-disasm=canonical exactly re-parseable disassembly";
      "  --dump-disasm=diagnostic address, bytes, spelling and form id";
      "  --dump-codec            the target's encoding tree";
      "  --check-codec           what Codec.check says about it";
      "";
      "With no --dump flag, prints the image and exits nonzero on any diagnostic.";
    ]

type options = {
  mutable target : string option;
  mutable base : int64 option;
  mutable dumps : string list;
  mutable file : string option;
}

let die msg =
  prerr_endline ("asm: " ^ msg);
  exit 2

let parse_base s =
  match Int64.of_string_opt s with Some v -> v | None -> die ("not an address: " ^ s)

let read_file path =
  let ic = try open_in_bin path with Sys_error m -> die m in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  let o = { target = None; base = None; dumps = []; file = None } in
  let args = Array.to_list Sys.argv in
  let rec go = function
    | [] -> ()
    | "--help" :: _ | "-h" :: _ ->
        print_endline usage;
        exit 0
    | "--target" :: t :: rest ->
        o.target <- Some t;
        go rest
    | "--fixed-base" :: b :: rest ->
        o.base <- Some (parse_base b);
        go rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "--" ->
        o.dumps <- o.dumps @ [ String.sub a 2 (String.length a - 2) ];
        go rest
    | f :: rest ->
        o.file <- Some f;
        go rest
  in
  go (List.tl args);
  let target = match o.target with Some t -> t | None -> die "no --target" in
  let (module D : Target_intf.Target.DRIVER) =
    match Driver.Registry.find target with
    | Some d -> d
    | None ->
        die ("unknown target " ^ target ^ "; known: " ^ String.concat ", " Driver.Registry.names)
  in
  let path = match o.file with Some f -> f | None -> die "no input file" in
  let source = Foundation.Span.source ~name:path ~contents:(read_file path) in
  let unit_name = Filename.remove_extension (Filename.basename path) in
  let report ds =
    prerr_string (Foundation.Diagnostic.render_all ds);
    prerr_newline ();
    exit 1
  in
  let ok = function Ok v -> v | Error ds -> report ds in
  let laid_out = lazy (ok (D.assemble ~unit_name ~source ())) in
  (* Binding is the *host's* step (§9), so the CLI is the host here: it chooses
     the address, and the assembler only says what constraints it must satisfy.
     With no --fixed-base there is no bound image and only the plan exists. *)
  let bound =
    lazy
      (match o.base with
      | None -> None
      | Some base ->
          let l = Lazy.force laid_out in
          let plan = Image.plan_of l in
          let addresses =
            List.map (fun (s : Image.segment_plan) -> (s.Image.seg_name, base)) plan.Image.segments
          in
          Some (ok (Image.bind_image l ~addresses)))
  in
  let text_of_image () =
    match Lazy.force bound with
    | Some img -> Fmt.to_to_string Image.pp img
    | None -> Fmt.to_to_string Image.pp_plan (Image.plan_of (Lazy.force laid_out))
  in
  let bytes_and_base () =
    match Lazy.force bound with
    | Some img -> (
        match img.Image.segments with s :: _ -> (s.Image.bytes, s.Image.address) | [] -> ("", 0L))
    | None -> die "disassembly needs --fixed-base (§9: the host chooses the address)"
  in
  let dumps = if o.dumps = [] then [ "dump-image" ] else o.dumps in
  List.iter
    (fun d ->
      match d with
      | "dump-tokens" -> print_string (ok (D.dump_tokens ~source))
      | "dump-source-ast" -> print_endline (ok (D.dump_source_ast ~unit_name ~source))
      | "dump-normalized-ast" -> print_endline (ok (D.dump_normalized_ast ~unit_name ~source))
      | "dump-lowered-ast" -> print_endline (ok (D.dump_lowered_ast ~unit_name ~source))
      | "dump-image" -> print_endline (text_of_image ())
      | "dump-disasm=canonical" ->
          let bytes, address = bytes_and_base () in
          print_string (ok (D.dump_disasm_canonical ~address bytes))
      | "dump-disasm=diagnostic" ->
          let bytes, address = bytes_and_base () in
          print_string (ok (D.dump_disasm_diagnostic ~address bytes))
      | "dump-codec" -> print_string (D.dump_codec ())
      | "check-codec" ->
          let ps = D.check_codec () in
          if ps = [] then print_endline "codec: no problems"
          else List.iter (fun p -> print_endline ("codec: " ^ p)) ps
      | other -> die ("unknown option --" ^ other))
    dumps
