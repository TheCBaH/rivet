let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

let timeout_s = 10

(* The artifact root is a work root, so it is env-selectable and validated by
   the same capability machinery every other root uses. *)
let artifacts repo = Tool_workspace.exec_artifacts repo ~env:Sys.getenv_opt
let hex_name section = String.sub section 1 (String.length section - 1)
let oracle_sections = [ ".text"; ".rodata"; ".data" ]

let run_one repo ~target ~case_name =
  let corpus = Repo.fixture_corpus repo in
  let tools = Gnu_tools.for_target target in
  let c = Target.config target in
  let target_s = Target.to_string target in
  let* case = Corpus.resolve corpus case_name in
  let* _prev, source_rel = Corpus.previous_and_source case in
  let* stem = Corpus.stem_of_source source_rel in
  let* want =
    Expected_status.read ~case:case_name Fpath.(case.Corpus.root / "expected-status.txt")
  in
  let fixture = Fpath.(case.Corpus.root / target_s / (stem ^ ".s")) in
  let oracle = Fpath.(case.Corpus.root / target_s / "oracle" / "linked") in
  if not (Sys.file_exists (Fpath.to_string fixture)) then
    err Tool_error.Read_file (Printf.sprintf "missing fixture %s" (Fpath.to_string fixture))
  else
    let* root = artifacts repo in
    let work = Fpath.(Tool_workspace.read_path root / target_s / case_name) in
    (* Recreated, not reused: a stale image from an earlier run must never be
       what gets executed and compared. *)
    let* () = Tool_fs.remove_tree work in
    let* () = Tool_fs.mkdir_p work in

    let start_s = Fpath.(work / "start.s") in
    let* () = Tool_fs.write start_s (Startup.source target) in
    let* () = Tool_fs.write Fpath.(work / "link.ld") (Link_script.exec target) in
    let start_o = Fpath.(work / "start.o") in
    let fixture_o = Fpath.(work / "fixture.o") in
    let* () = Gnu_tools.assemble tools ~cwd:work ~src:start_s ~obj:start_o in
    let* () = Gnu_tools.assemble tools ~cwd:work ~src:fixture ~obj:fixture_o in
    let elf = Fpath.(work / "fixture.elf") in
    let* () =
      Gnu_tools.link tools ~cwd:work
        ~script:Fpath.(work / "link.ld")
        ~out:elf ~objs:[ "start.o"; "fixture.o" ]
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
        (Printf.sprintf "%s/%s: expected %s, got %s" case_name target_s c.Target.elf_class class_)
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
         loop driven by which oracle files exist would never look at them. *)
      let compared =
        ".start"
        :: List.filter
             (fun s -> Sys.file_exists (Fpath.to_string Fpath.(oracle / (hex_name s ^ ".hex"))))
             oracle_sections
      in
      let* headers = Gnu_tools.objdump_headers tools elf in
      let* allocated = Gnu_output.alloc_sections headers in
      let unexpected = List.filter (fun s -> not (List.mem s compared)) allocated in
      if unexpected <> [] then
        err Tool_error.Validate
          (Printf.sprintf
             "%s/%s: allocated section(s) in the executed image with no oracle entry: %s" case_name
             target_s (String.concat " " unexpected))
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
                 "%s/%s: .start is 0x%s bytes, expected 0x%s from the startup stub - the fixture \
                  contributed to it"
                 case_name target_s
                 (Option.value ~default:"<absent>" actual)
                 expected)
        | Some _, _ ->
            let rec compare_sections = function
              | [] -> Ok ()
              | section :: rest ->
                  let name = hex_name section in
                  let committed = Fpath.(oracle / (name ^ ".hex")) in
                  if not (Sys.file_exists (Fpath.to_string committed)) then compare_sections rest
                  else
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
            let* () = compare_sections oracle_sections in
            let* status, _out, errout = Gnu_tools.qemu_run tools ~exe:elf ~timeout_s in
            let code =
              match status with
              | Process_status.Exited n -> n
              | Process_status.Signaled n -> 128 + n
              | Process_status.Timed_out -> 124
            in
            let* () = Tool_fs.write Fpath.(work / "exit-status.txt") (string_of_int code ^ "\n") in
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
                    code )

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
