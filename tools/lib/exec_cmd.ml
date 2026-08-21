let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

let timeout_s = 10

(* The artifact root is a work root, so it is env-selectable and validated by
   the same capability machinery every other root uses. *)
let artifacts repo = Tool_workspace.exec_artifacts repo ~env:Sys.getenv_opt
let hex_name section = String.sub section 1 (String.length section - 1)

(* The oracle's own linked manifest (v2, M3 §11, .ai/asm_plan.md §12) is what
   says which sections this case's oracle covers at all - not a static list -
   since a NOBITS section (.bss) legitimately has no .hex artifact to check
   for, and "no .hex file" used to be exactly how a section was excluded from
   the compared set. [[]] for a case with no oracle run yet, same as an absent
   file always meant here. *)
type linked_entry = { le_section : string; le_kind : [ `Progbits | `Nobits ]; le_size : int }

let read_linked_manifest oracle =
  let path = Fpath.(oracle / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string path)) then Ok []
  else
    let* text = Tool_fs.read path in
    Ok
      (String.split_on_char '\n' text
      |> List.filter_map (fun line ->
          match String.split_on_char '\t' (String.trim line) with
          | [ sec; kind; _addr; size; _artifact ] when sec <> "" -> (
              match int_of_string_opt size with
              | None -> None
              | Some size ->
                  Some
                    {
                      le_section = sec;
                      le_kind = (if String.equal kind "nobits" then `Nobits else `Progbits);
                      le_size = size;
                    })
          | _ -> None))

(* One object name per unit: the historical "fixture.o" for a single-source
   case, so its scratch layout is unchanged, and "<unit>.o" per unit for a
   multi-source one. *)
let obj_name units unit_name = match units with [ _ ] -> "fixture.o" | _ -> unit_name ^ ".o"

let run_one repo ~target ~case_name =
  let corpus = Repo.fixture_corpus repo in
  let tools = Gnu_tools.for_target target in
  let c = Target.config target in
  let target_s = Target.to_string target in
  let* case = Corpus.resolve corpus case_name in
  let* supported = Corpus.supports_target case target in
  if not supported then
    Ok
      ( Printf.sprintf "# skipped: %s does not support %s" case_name target_s,
        Printf.sprintf "qemu: %s/%s: skipped (unsupported target)" case_name target_s )
  else
    let* units = Corpus.unit_stems case in
    let* want =
      Expected_status.read ~case:case_name Fpath.(case.Corpus.root / "expected-status.txt")
    in
    let unit_sources =
      List.map
        (fun (unit_name, stem) ->
          (unit_name, obj_name units unit_name, Fpath.(case.Corpus.root / target_s / (stem ^ ".s"))))
        units
    in
    let oracle = Fpath.(case.Corpus.root / target_s / "oracle" / "linked") in
    let missing =
      List.filter (fun (_, _, p) -> not (Sys.file_exists (Fpath.to_string p))) unit_sources
    in
    match missing with
    | (_, _, p) :: _ ->
        err Tool_error.Read_file (Printf.sprintf "missing fixture %s" (Fpath.to_string p))
    | [] -> (
        let* root = artifacts repo in
        let work = Fpath.(Tool_workspace.read_path root / target_s / case_name) in
        (* Recreated, not reused: a stale image from an earlier run must never be
       what gets executed and compared. *)
        let* () = Tool_fs.remove_tree work in
        let* () = Tool_fs.mkdir_p work in

        let objs = List.map (fun (_, o, _) -> o) unit_sources in
        let start_s = Fpath.(work / "start.s") in
        let* () = Tool_fs.write start_s (Startup.source target) in
        let* () = Tool_fs.write Fpath.(work / "link.ld") (Link_script.exec target ~objs) in
        let start_o = Fpath.(work / "start.o") in
        let* () = Gnu_tools.assemble tools ~cwd:work ~src:start_s ~obj:start_o in
        let* () =
          let rec go = function
            | [] -> Ok ()
            | (_, o, src) :: rest ->
                let* () = Gnu_tools.assemble tools ~cwd:work ~src ~obj:Fpath.(work / o) in
                go rest
          in
          go unit_sources
        in
        let elf = Fpath.(work / "fixture.elf") in
        let* () =
          Gnu_tools.link tools ~cwd:work
            ~script:Fpath.(work / "link.ld")
            ~out:elf ~objs:("start.o" :: objs)
        in

        let* class_, machine = Gnu_tools.elf_identity tools elf in
        let* readelf = Gnu_tools.readelf_exec tools elf in
        let* () = Tool_fs.write Fpath.(work / "readelf.txt") readelf in
        let* disasm =
          Gnu_tools.objdump_disasm tools elf ~scrub:work ~drop_banner:false ~riscv_numeric:true
        in
        let* () = Tool_fs.write Fpath.(work / "objdump.txt") disasm in
        if class_ <> c.Target.elf_class then
          err Tool_error.Validate
            (Printf.sprintf "%s/%s: expected %s, got %s" case_name target_s c.Target.elf_class
               class_)
        else if machine <> c.Target.readelf_machine then
          err Tool_error.Validate
            (Printf.sprintf "%s/%s: expected %s, got %s" case_name target_s c.Target.readelf_machine
               machine)
        else
          (* {1 The bytes about to execute are the bytes the oracle accepted}

         The compared set has to be CLOSED, not merely non-empty. ld places an
         allocated input section the script never mentions as an orphan output
         section, so a fixture carrying, say, .sdata would have those bytes
         mapped and executable while matching no oracle artifact at all - and a
         loop driven by which oracle files exist would never look at them. The
         oracle's OWN manifest is the source of truth for what it covers, not
         a static list - a NOBITS section legitimately has no .hex file. *)
          let* linked = read_linked_manifest oracle in
          let compared = ".start" :: List.map (fun e -> e.le_section) linked in
          let* headers = Gnu_tools.objdump_headers tools elf in
          let* allocated = Gnu_output.alloc_sections headers in
          let unexpected = List.filter (fun s -> not (List.mem s compared)) allocated in
          if unexpected <> [] then
            err Tool_error.Validate
              (Printf.sprintf
                 "%s/%s: allocated section(s) in the executed image with no oracle entry: %s"
                 case_name target_s (String.concat " " unexpected))
          else
            (* .start is the one allocated output section with no oracle
           counterpart, so it is the one place bytes can hide. Trusting it by
           name is not enough: ld merges an orphan input section into an
           existing output section of the SAME NAME, so a fixture carrying its
           own .start would be appended to our stub and executed even though the
           script selects only start.o(.text.startup). Size equality against the
           stub establishes provenance. *)
            let* start_headers = Gnu_tools.objdump_headers tools start_o in
            match
              ( Gnu_output.section_size start_headers ~name:".text.startup",
                Gnu_output.section_size headers ~name:".start" )
            with
            | None, _ ->
                err Tool_error.Validate
                  (Printf.sprintf "%s/%s: startup stub has no .text.startup" case_name target_s)
            | Some expected, actual when Some expected <> actual ->
                err Tool_error.Validate
                  (Printf.sprintf
                     "%s/%s: .start is 0x%s bytes, expected 0x%s from the startup stub - the \
                      fixture contributed to it"
                     case_name target_s
                     (Option.value ~default:"<absent>" actual)
                     expected)
            | Some _, _ ->
                (* A NOBITS section has no bytes to extract from the EXECUTED
                 image either - the same objcopy-reports-empty signal a
                 genuinely absent section gives - so it is checked by its
                 logical size instead, read off the executed ELF's own
                 section headers and compared against the oracle's recorded
                 size (M3 §11, .ai/asm_plan.md §12). *)
                let rec compare_sections = function
                  | [] -> Ok ()
                  | { le_section = section; le_kind = `Nobits; le_size } :: rest ->
                      let got = Gnu_output.section_size headers ~name:section in
                      let got_size = Option.map (fun s -> int_of_string ("0x" ^ s)) got in
                      if got_size <> Some le_size then
                        err Tool_error.Validate
                          (Printf.sprintf "%s/%s: executed %s is %s bytes, oracle recorded 0x%x"
                             case_name target_s section
                             (Option.value ~default:"<absent>" got)
                             le_size)
                      else compare_sections rest
                  | { le_section = section; le_kind = `Progbits; _ } :: rest ->
                      let name = hex_name section in
                      let committed = Fpath.(oracle / (name ^ ".hex")) in
                      let bin = Fpath.(work / (name ^ ".bin")) in
                      let* _ = Gnu_tools.objcopy_section tools ~src:elf ~section ~out:bin in
                      let* bytes = Tool_fs.read bin in
                      let dumped = Hex_dump.of_bytes bytes in
                      let* () = Tool_fs.write Fpath.(work / (name ^ ".hex")) dumped in
                      let* want_hex = Tool_fs.read committed in
                      if dumped <> want_hex then
                        err Tool_error.Validate
                          (Printf.sprintf "%s/%s: executed %s differs from controlled-link oracle"
                             case_name target_s section)
                      else compare_sections rest
                in
                let* () = compare_sections linked in
                let* status, _out, errout = Gnu_tools.qemu_run tools ~exe:elf ~timeout_s in
                let code =
                  match status with
                  | Process_status.Exited n -> n
                  | Process_status.Signaled n -> 128 + n
                  | Process_status.Timed_out -> 124
                in
                let* () =
                  Tool_fs.write Fpath.(work / "exit-status.txt") (string_of_int code ^ "\n")
                in
                if code = 124 then
                  err Tool_error.Exec (Printf.sprintf "%s/%s: QEMU timed out" case_name target_s)
                else if code <> want then (
                  prerr_string errout;
                  flush stderr;
                  err Tool_error.Validate
                    (Printf.sprintf "%s/%s: expected exit status %d, got %d" case_name target_s want
                       code))
                else
                  Ok
                    ( Printf.sprintf "+ timeout %ds %s %s" timeout_s c.Target.qemu_bin
                        (Fpath.to_string elf),
                      Printf.sprintf "qemu: %s/%s: %s %s, exit %d" case_name target_s class_ machine
                        code ))

let provenance repo ~target =
  let tools = Gnu_tools.for_target target in
  let* root = artifacts repo in
  let dir = Fpath.(Tool_workspace.read_path root / Target.to_string target) in
  let* () = Tool_fs.mkdir_p dir in
  let* as_v = Gnu_tools.version_line tools `As in
  let* ld_v = Gnu_tools.version_line tools `Ld in
  let* qemu_v = Gnu_tools.qemu_version tools in
  Tool_fs.write
    Fpath.(dir / "tool-versions.txt")
    (Printf.sprintf "as\t%s\nld\t%s\nqemu\t%s\n" as_v ld_v qemu_v)

let run repo ~targets ~cases =
  let corpus = Repo.fixture_corpus repo in
  let names =
    match cases with
    | [] -> Result.map (List.map (fun c -> c.Corpus.name)) (Corpus.discover corpus)
    | given -> Ok given
  in
  match names with
  | Error e -> Command.of_error e
  | Ok names ->
      let rec targets_loop acc = function
        | [] -> Command.ok (List.rev acc)
        | t :: rest -> (
            let step =
              let* () = Gnu_tools.require (Gnu_tools.for_target t) ~qemu:true in
              provenance repo ~target:t
            in
            match step with
            | Error e -> Command.fail_after (List.rev acc) e
            | Ok () ->
                let rec cases_loop acc = function
                  | [] -> targets_loop acc rest
                  | c :: more -> (
                      match run_one repo ~target:t ~case_name:c with
                      | Error e -> Command.fail_after (List.rev acc) e
                      | Ok (invocation, result) ->
                          cases_loop
                            (Diagnostic.stdout result :: Diagnostic.stdout invocation :: acc)
                            more)
                in
                cases_loop acc names)
      in
      targets_loop [] targets
