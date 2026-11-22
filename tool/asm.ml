(* The `asm` CLI.

   Outside the production closure by construction: it is an executable, so
   nothing can depend on it, and scripts/asm-check-purity.sh classifies tool/ as a
   non-production tree. This is where file I/O is allowed to live. §9's rule is
   that none of it leaks back into a library, which is why the whole of this file
   is argument handling, reading a file, and printing - every decision it makes
   is made in [Rivet_driver].

   Argument parsing is hand-written rather than cmdliner. The production closure
   must build under Melange and js_of_ocaml, and a dependency added "only for the
   CLI" is exactly the kind of edge the purity audit exists to catch; keeping the
   tool's dependencies to [driver] alone means there is nothing to audit. *)

let usage =
  String.concat "\n"
    [
      "usage: asm --target <t> [options] <file.s>";
      "";
      "  --target <t>            one of: " ^ String.concat ", " Rivet_driver.Registry.names;
      "  --fixed-base <addr>     bind the image at this address (hex with 0x, or decimal)";
      "  --features <spec>       enable/disable optional components: none, +name, -name, \
       comma-separated, applied left to right (default: all implemented)";
      "  --max-features <spec>   the most a directive such as .option arch may enable; with no \
       --features, also the starting configuration";
      "  --entry <sym>           the entry symbol (default: the only global)";
      "  --extern <a,b>          symbols the input may leave undefined (repeatable)";
      "  --export <a,b>          the exported symbols (repeatable; default: every global)";
      "  --max-size <n>          refuse an image larger than n bytes";
      "  --no-section-grouping   keep .text.*, .rodata.*, .data.*, .bss.* as sections of their own";
      "  --allow-rwx             allow a segment that is writable and executable";
      "  --import <sym=addr>     the address of an extern (repeatable)";
      "  --overlay <a,b>         one overlay group; segments of different groups may overlap \
       (repeatable)";
      "  --load-address <seg=addr>  a segment's load address when it differs (repeatable)";
      "  --at <seg=addr>         a segment's address, overriding --fixed-base (repeatable)";
      "  several input files are assembled and linked together";
      "  --inspect-disabled      with --dump-disasm=diagnostic: decode disabled components too, \
       and mark them";
      "  --err_trace <spec>      off | boundaries | all; error provenance to stderr";
      "";
      "  --dump-tokens           the lexer";
      "  --dump-source-ast       after parsing";
      "  --dump-normalized-ast   after simplify";
      "  --dump-lowered-ast      after lowering";
      "  --dump-image            the plan, or the bound image with --fixed-base";
      "  --dump-disasm=canonical exactly re-parseable disassembly";
      "  --dump-disasm=diagnostic address, bytes, spelling and form id";
      "  --dump-bytes            assembled section bytes, in the project's committed hex-dump \
       format";
      "  --dump-features         the effective feature configuration and the target's components";
      "  --dump-codec            the target's encoding tree";
      "  --check-codec           what Codec.check says about it";
      "";
      "With no --dump flag, prints the image and exits nonzero on any diagnostic.";
    ]

type options = {
  mutable target : string option;
  mutable base : int64 option;
  mutable dumps : string list;
  mutable files : string list;
  mutable entry : string option;
  mutable externs : string list;
  mutable exported : string list option;
  mutable max_size : int option;
  mutable allow_rwx : bool;
  mutable group_sections : bool;
  mutable imports : (string * int64) list;
  mutable overlays : string list list;
  mutable load_addresses : (string * int64) list;
  mutable at : (string * int64) list;
  mutable features : Rivet_target_features.Config.spec option;
  mutable max_features : Rivet_target_features.Config.spec option;
  mutable inspect : bool;
}

let die msg =
  prerr_endline ("asm: " ^ msg);
  exit 2

let parse_base s =
  match Int64.of_string_opt s with Some v -> v | None -> die ("not an address: " ^ s)

let split_commas s = List.filter (fun x -> x <> "") (String.split_on_char ',' s)

let parse_pair s =
  match String.index_opt s '=' with
  | Some i -> (String.sub s 0 i, parse_base (String.sub s (i + 1) (String.length s - i - 1)))
  | None -> die ("expected name=address: " ^ s)

(* Isa_gen_ours (tools/lib) shells out to this executable rather than
   linking it, so this project's own hex-dump format has to be reproduced here
   rather than shared as a library call - scripts/asm-check-purity.sh forbids the
   dependency running the other way (rivet_tools must never require
   building the whole assembler, tools-boundary). Byte-for-byte the same
   algorithm as tools/lib/hex_dump.ml's [of_bytes]: lowercase, space
   separated, 16 bytes per line, a trailing newline after every line including
   the last, and [""] for zero bytes. *)
let hex_dump_of_bytes s =
  let n = String.length s in
  if n = 0 then ""
  else begin
    let per_line = 16 in
    let b = Buffer.create ((n * 3) + (n / per_line) + 1) in
    String.iteri
      (fun i c ->
        if i > 0 then Buffer.add_char b (if i mod per_line = 0 then '\n' else ' ');
        Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
      s;
    Buffer.add_char b '\n';
    Buffer.contents b
  end

let read_file path =
  let ic = try open_in_bin path with Sys_error m -> die m in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  (* First, before anything can fail. Err's own default captures a Printexc
     callstack at every detection; docs/errors.md §3 explains why that must
     be off here, and why the policy is not simply Err.Config.fast. *)
  Rivet_foundation.Err_policy.apply ();
  let o =
    {
      target = None;
      base = None;
      dumps = [];
      files = [];
      entry = None;
      externs = [];
      exported = None;
      max_size = None;
      allow_rwx = false;
      group_sections = true;
      imports = [];
      overlays = [];
      load_addresses = [];
      at = [];
      features = None;
      max_features = None;
      inspect = false;
    }
  in
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
    | "--features" :: spec :: rest ->
        (match Rivet_target_features.Config.parse_spec spec with
        | Ok s -> o.features <- Some s
        | Error m -> die ("invalid --features " ^ spec ^ ": " ^ m));
        go rest
    | "--max-features" :: spec :: rest ->
        (match Rivet_target_features.Config.parse_spec spec with
        | Ok s -> o.max_features <- Some s
        | Error m -> die ("invalid --max-features " ^ spec ^ ": " ^ m));
        go rest
    | "--entry" :: e :: rest ->
        o.entry <- Some e;
        go rest
    | "--extern" :: l :: rest ->
        o.externs <- o.externs @ split_commas l;
        go rest
    | "--export" :: l :: rest ->
        o.exported <- Some (Option.value o.exported ~default:[] @ split_commas l);
        go rest
    | "--max-size" :: n :: rest ->
        (match int_of_string_opt n with
        | Some n when n >= 0 -> o.max_size <- Some n
        | _ -> die ("not a size: " ^ n));
        go rest
    | "--no-section-grouping" :: rest ->
        o.group_sections <- false;
        go rest
    | "--allow-rwx" :: rest ->
        o.allow_rwx <- true;
        go rest
    | "--import" :: p :: rest ->
        o.imports <- o.imports @ [ parse_pair p ];
        go rest
    | "--overlay" :: l :: rest ->
        o.overlays <- o.overlays @ [ split_commas l ];
        go rest
    | "--load-address" :: p :: rest ->
        o.load_addresses <- o.load_addresses @ [ parse_pair p ];
        go rest
    | "--at" :: p :: rest ->
        o.at <- o.at @ [ parse_pair p ];
        go rest
    | "--inspect-disabled" :: rest ->
        o.inspect <- true;
        go rest
    (* Error tracing, off unless asked for. The monitor writes to stderr and
       never to stdout: docs/errors.md §3 keeps Err provenance out of
       anything a cram baseline compares, and every dump above goes to stdout.
       The spec is parsed rather than read from the environment, so a build's
       behaviour is a property of its command line. *)
    | "--err_trace" :: spec :: rest ->
        (* [backtrace] is pinned to [off] rather than left to the spec. What
           this switch is for is the explicit ~pos trail, which is a compile-time
           constant and reads the same on every backend; a captured native stack
           would add host paths and frame counts that differ between native,
           js_of_ocaml and Melange, and say less. *)
        (match
           Rivet_foundation.Err_policy.of_strings ~trace:(Some spec) ~backtrace:(Some "off")
             ~max_events:None ~max_frames:None ~max_external_bytes:None
         with
        | Ok config ->
            Err.Config.set config;
            ignore
              (Err.Monitor.install (fun observation ->
                   Fmt.epr "@[<v>[err] %a@]@." Err.Observation.pp observation))
        | Error e ->
            Fmt.epr "invalid --err_trace %s: %a@." spec
              (Err.Error.pp Rivet_foundation.Err_policy.pp_of_strings_error)
              e;
            exit 2);
        go rest
    | a :: rest when String.length a > 2 && String.sub a 0 2 = "--" ->
        o.dumps <- o.dumps @ [ String.sub a 2 (String.length a - 2) ];
        go rest
    | f :: rest ->
        o.files <- o.files @ [ f ];
        go rest
  in
  go (List.tl args);
  let target = match o.target with Some t -> t | None -> die "no --target" in
  let (module D : Rivet_target_intf.Target.DRIVER) =
    match Rivet_driver.Registry.find target with
    | Some d -> d
    | None ->
        die
          ("unknown target " ^ target ^ "; known: " ^ String.concat ", " Rivet_driver.Registry.names)
  in
  let paths = if o.files = [] then die "no input file" else o.files in
  let sources =
    List.map
      (fun path ->
        ( Filename.remove_extension (Filename.basename path),
          Rivet_foundation.Span.source ~name:path ~contents:(read_file path) ))
      paths
  in
  let unit_name, source = List.hd sources in
  let report ds =
    prerr_string (Rivet_foundation.Diagnostic.render_all ds);
    prerr_newline ();
    exit 1
  in
  (* The payload only. A wrapped failure also carries a detection origin and the
     phase trail, and docs/errors.md §3 keeps both out of what a cram
     baseline compares; the tracing switch is how they are asked for. *)
  let ok = function Ok v -> v | Error e -> report (Rivet_foundation.Diag.diagnostics e) in
  let features = o.features in
  let request =
    {
      Rivet_target_intf.Target.entry = o.entry;
      features;
      max_features = o.max_features;
      externs = o.externs;
      exported = o.exported;
      max_size = o.max_size;
      allow_rwx = o.allow_rwx;
      group_sections = o.group_sections;
    }
  in
  let laid_out =
    lazy
      (match sources with
      | [ _ ] -> ok (D.assemble_request request ~unit_name ~source ())
      | _ -> ok (D.assemble_many_request request sources ()))
  in
  (* Binding is the *host's* step (§9), so the CLI is the host here: it chooses
     the address, and the assembler only says what constraints it must satisfy.
     With no --fixed-base there is no bound image and only the plan exists. *)
  let bound =
    lazy
      (match o.base with
      | None -> None
      | Some base ->
          let l = Lazy.force laid_out in
          let plan = Rivet_image.plan_of l in
          let addresses =
            List.map
              (fun (s : Rivet_image.segment_plan) ->
                ( s.Rivet_image.seg_name,
                  Option.value (List.assoc_opt s.Rivet_image.seg_name o.at) ~default:base ))
              plan.Rivet_image.segments
          in
          Some
            (ok
               (Rivet_image.bind_image ~imports:o.imports ~overlays:o.overlays
                  ~load_addresses:o.load_addresses l ~addresses)))
  in
  let text_of_image () =
    match Lazy.force bound with
    | Some img -> Fmt.to_to_string Rivet_image.pp img
    | None -> Fmt.to_to_string Rivet_image.pp_plan (Rivet_image.plan_of (Lazy.force laid_out))
  in
  let bytes_and_base () =
    match Lazy.force bound with
    | Some img -> (
        match img.Rivet_image.segments with
        | s :: _ -> (s.Rivet_image.bytes, s.Rivet_image.address)
        | [] -> ("", 0L))
    | None -> die "disassembly needs --fixed-base (§9: the host chooses the address)"
  in
  let dumps = if o.dumps = [] then [ "dump-image" ] else o.dumps in
  List.iter
    (fun d ->
      match d with
      | "dump-tokens" -> print_string (ok (D.dump_tokens ~source))
      | "dump-source-ast" -> print_endline (ok (D.dump_source_ast ~unit_name ~source))
      | "dump-normalized-ast" ->
          print_endline (ok (D.dump_normalized_ast ?features ~unit_name ~source ()))
      | "dump-lowered-ast" ->
          print_endline (ok (D.dump_lowered_ast ?features ~unit_name ~source ()))
      | "dump-image" -> print_endline (text_of_image ())
      | "dump-disasm=canonical" ->
          let bytes, address = bytes_and_base () in
          print_string (ok (D.dump_disasm_canonical ?features ~address bytes))
      | "dump-disasm=diagnostic" ->
          let bytes, address = bytes_and_base () in
          print_string (ok (D.dump_disasm_diagnostic ?features ~inspect:o.inspect ~address bytes))
      | "dump-bytes" ->
          let bytes, (_ : int64) = bytes_and_base () in
          print_string (hex_dump_of_bytes bytes)
      | "dump-features" -> (
          match
            D.configure (Option.value features ~default:Rivet_target_features.Config.default_spec)
          with
          | Error m -> die ("invalid --features: " ^ m)
          | Ok config ->
              Printf.printf "target: %s\n" D.name;
              Printf.printf "enabled: %s\n"
                (match Rivet_target_features.Config.features config with
                | [] -> "(none)"
                | fs -> String.concat "," fs);
              List.iter
                (fun (c : Rivet_target_features.Component.t) ->
                  Printf.printf "component %s feature=%s%s: %s\n" c.id c.feature
                    (if c.requires = [] then "" else " requires=" ^ String.concat "," c.requires)
                    c.summary)
                D.components)
      | "dump-codec" -> print_string (D.dump_codec ())
      | "check-codec" ->
          let ps = D.check_codec () in
          if ps = [] then print_endline "codec: no problems"
          else List.iter (fun p -> print_endline ("codec: " ^ p)) ps
      | other -> die ("unknown option --" ^ other))
    dumps
