let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

(* The allocatable sections the layout owns, in the order the artifacts list
   them. The set is CLOSED: an allocated section outside it is a hard failure,
   because ld places an input section the script never names as an ORPHAN
   output section - it would be mapped into every executed image while being
   recorded in no artifact at all. *)
let alloc_sections = [ ".text"; ".rodata"; ".data" ]
let hex_name section = String.sub section 1 (String.length section - 1)

let link_addr target section =
  let c = (Target.config target).Target.link in
  match section with
  | ".text" -> Some c.Target.text
  | ".rodata" -> Some c.Target.rodata
  | ".data" -> Some c.Target.data
  | _ -> None

(* {1 One (target, case) pair} *)

let run_one repo ~target ~case_name =
  let corpus = Repo.fixture_corpus repo in
  let tools = Gnu_tools.for_target target in
  let* case = Corpus.resolve corpus case_name in
  let* _prev, source_rel = Corpus.previous_and_source case in
  let* stem = Corpus.stem_of_source source_rel in
  let target_s = Target.to_string target in
  let src = Fpath.(case.Corpus.root / target_s / (stem ^ ".s")) in
  if not (Sys.file_exists (Fpath.to_string src)) then
    err Tool_error.Read_file
      (Printf.sprintf "no fixture for %s/%s (run make asm-fixtures-regen)" case_name target_s)
  else
    let* () = Gnu_tools.require tools ~qemu:false in
    let out = Fpath.(case.Corpus.root / target_s / "oracle") in
    let* () = Tool_fs.mkdir_p out in
    Tool_workspace.with_scratch ~label:"oracle" (fun work ->
        let obj = Fpath.(work / "obj.o") in
        (* cwd = work and the object named plainly, so neither the object name
           nor its path can reach the recorded output. *)
        let* () = Gnu_tools.assemble tools ~cwd:work ~src ~obj in

        let* readelf = Gnu_tools.readelf_oracle tools obj ~scrub:work in
        let* () = Tool_fs.write Fpath.(out / "readelf.txt") readelf in

        let* disasm =
          Gnu_tools.objdump_disasm tools obj ~scrub:work ~drop_banner:true ~riscv_numeric:true
        in
        let* () = Tool_fs.write Fpath.(out / "objdump.txt") disasm in

        let text_bin = Fpath.(work / "text.bin") in
        let* _ = Gnu_tools.objcopy_section tools ~src:obj ~section:".text" ~out:text_bin in
        let* text_bytes = Tool_fs.read text_bin in
        let* () = Tool_fs.write Fpath.(out / "text.hex") (Hex_dump.of_bytes text_bytes) in

        let* headers = Gnu_tools.readelf_headers tools obj in
        let* objdump_r = Gnu_tools.objdump_relocs tools obj in
        let* relocs = Gnu_output.relocations ~objdump_r ~rela:(Gnu_output.has_rela headers) in
        let* () = Tool_fs.write Fpath.(out / "reloc.txt") relocs in

        let* readelf_s = Gnu_tools.readelf_symbols tools obj in
        let* symbols = Gnu_output.symbols ~readelf_s in
        let* () = Tool_fs.write Fpath.(out / "symbols.txt") symbols in

        (* {2 The controlled reference link}

           Unresolved placeholder bytes are not encodings, so a fixture whose
           .text carries relocations cannot be byte-compared as an object. Both
           sides are resolved at the same addresses instead. *)
        let script = Fpath.(work / "link.ld") in
        let* () = Tool_fs.write script (Link_script.oracle target) in
        let linked = Fpath.(work / "linked.elf") in
        let* () = Gnu_tools.link tools ~cwd:work ~script ~out:linked ~objs:[ "obj.o" ] in

        (* Checked on the LINKED IMAGE, not the object: .bss and .eh_frame are
           allocated in every object and are legitimately absent here, one
           because it is empty and the other because the script discards it. *)
        let* linked_headers = Gnu_tools.objdump_headers tools linked in
        let* allocated = Gnu_output.alloc_sections linked_headers in
        let unexpected = List.filter (fun s -> not (List.mem s alloc_sections)) allocated in
        if unexpected <> [] then
          err Tool_error.Validate
            (Printf.sprintf "%s/%s: allocated section(s) outside the oracle's scope: %s" case_name
               target_s (String.concat " " unexpected))
        else
          let linked_dir = Fpath.(out / "linked") in
          let* () = Tool_fs.mkdir_p linked_dir in
          (* Stale .hex files are removed, not overwritten: a section that
             stopped being emitted must not leave its previous dump behind,
             where the manifest would no longer name it but the traversal-based
             corpus check still would. *)
          let* () = Tool_fs.remove_matching linked_dir ~suffix:".hex" in
          let* present =
            List.fold_left
              (fun acc section ->
                let* acc = acc in
                let name = hex_name section in
                let bin = Fpath.(work / (name ^ ".bin")) in
                let* got = Gnu_tools.objcopy_section tools ~src:linked ~section ~out:bin in
                if not got then Ok acc
                else
                  let* bytes = Tool_fs.read bin in
                  let* () =
                    Tool_fs.write Fpath.(linked_dir / (name ^ ".hex")) (Hex_dump.of_bytes bytes)
                  in
                  Ok (section :: acc))
              (Ok []) alloc_sections
          in
          let present = List.rev present in
          let manifest =
            String.concat ""
              (List.map
                 (fun section ->
                   let addr = Option.get (link_addr target section) in
                   Printf.sprintf "%s\t%s\tlinked/%s.hex\n" section (Target.hex addr)
                     (hex_name section))
                 present)
          in
          let* () = Tool_fs.write Fpath.(linked_dir / "manifest.txt") manifest in

          let* versions =
            List.fold_left
              (fun acc (label, which) ->
                let* acc = acc in
                let* v = Gnu_tools.version_line tools which in
                Ok (Printf.sprintf "%s\t%s\n" label v :: acc))
              (Ok [])
              [
                ("as", `As);
                ("objdump", `Objdump);
                ("readelf", `Readelf);
                ("objcopy", `Objcopy);
                ("ld", `Ld);
              ]
          in
          let* () =
            Tool_fs.write Fpath.(out / "tool-versions.txt") (String.concat "" (List.rev versions))
          in
          let reloc_count =
            List.length (List.filter (fun l -> l <> "") (String.split_on_char '\n' relocs))
          in
          Ok
            (Printf.sprintf "%s/%s: .text %d bytes, %d relocation(s), sections %s" case_name
               target_s (String.length text_bytes) reloc_count (String.concat " " present)))

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
      (* Target-major, and STOPPING at the first failure - the shell calls
         run_one directly rather than through an `|| rc=1` aggregator, so this
         is not the accumulating shape verify/check use. *)
      let rec go acc = function
        | [] -> Command.ok (List.rev acc)
        | (t, c) :: rest -> (
            match run_one repo ~target:t ~case_name:c with
            | Error e ->
                {
                  Command.events =
                    List.rev_map (fun d -> Command.Output d) acc @ [ Command.Fatal e ];
                  exit = `Failure;
                }
            | Ok line -> go (Diagnostic.stdout line :: acc) rest)
      in
      go [] (List.concat_map (fun t -> List.map (fun c -> (t, c)) names) targets)
