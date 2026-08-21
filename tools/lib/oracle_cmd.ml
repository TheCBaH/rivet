let ( let* ) = Result.bind

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

(* The allocatable sections the layout owns, in the order the artifacts list
   them. The set is CLOSED: an allocated section outside it is a hard failure,
   because ld places an input section the script never names as an ORPHAN
   output section - it would be mapped into every executed image while being
   recorded in no artifact at all. .bss is real M3 scope (.ai/asm_plan.md
   §12), not a placeholder: it is NOBITS rather than PROGBITS, so it is
   handled separately below wherever that distinction matters. *)
let alloc_sections = [ ".text"; ".rodata"; ".data"; ".bss" ]
let hex_name section = String.sub section 1 (String.length section - 1)

let link_addr target section =
  let c = (Target.config target).Target.link in
  match section with
  | ".text" -> Some c.Target.text
  | ".rodata" -> Some c.Target.rodata
  | ".data" -> Some c.Target.data
  | ".bss" -> Some c.Target.bss
  | _ -> None

(* One object name per unit: the historical "obj.o" for a single-source case,
   so its scratch layout and recorded output are unchanged, and "<unit>.o" per
   unit for a multi-source one. *)
let obj_name units unit_name = match units with [ _ ] -> "obj.o" | _ -> unit_name ^ ".o"

(* {1 One (target, case) pair} *)

let run_one repo ~target ~case_name =
  let corpus = Repo.fixture_corpus repo in
  let tools = Gnu_tools.for_target target in
  let* case = Corpus.resolve corpus case_name in
  let target_s = Target.to_string target in
  let* supported = Corpus.supports_target case target in
  if not supported then Ok (Printf.sprintf "%s/%s: skipped (unsupported target)" case_name target_s)
  else
    let* units = Corpus.unit_stems case in
    let unit_sources =
      List.map
        (fun (unit_name, stem) ->
          (unit_name, obj_name units unit_name, Fpath.(case.Corpus.root / target_s / (stem ^ ".s"))))
        units
    in
    let missing =
      List.filter (fun (_, _, p) -> not (Sys.file_exists (Fpath.to_string p))) unit_sources
    in
    match missing with
    | (unit_name, _, _) :: _ ->
        err Tool_error.Read_file
          (Printf.sprintf "no fixture for %s/%s/%s (run make asm-fixtures-regen)" case_name
             unit_name target_s)
    | [] ->
        let* () = Gnu_tools.require tools ~qemu:false in
        let out = Fpath.(case.Corpus.root / target_s / "oracle") in
        let* () = Tool_fs.mkdir_p out in
        (* A single-source case's own evidence still lands directly in [out], byte-
         for-byte where every already-committed fixture keeps it; a multi-source
         case's per-unit evidence gets its own subdirectory, named after the
         unit, per the oracle-evidence model (.ai/asm_plan.md §12/M3 §11). *)
        let unit_dir unit_name = match units with [ _ ] -> out | _ -> Fpath.(out / unit_name) in
        Tool_workspace.with_scratch ~label:"oracle" (fun work ->
            let objs = List.map (fun (_, o, _) -> o) unit_sources in
            let per_unit acc (unit_name, obj_rel, src) =
              let* acc = acc in
              (* cwd = work and the object named plainly, so neither the object
               name nor its path can reach the recorded output. *)
              let obj = Fpath.(work / obj_rel) in
              let* () = Gnu_tools.assemble tools ~cwd:work ~src ~obj in
              let dir = unit_dir unit_name in
              let* () = Tool_fs.mkdir_p dir in

              let* readelf = Gnu_tools.readelf_oracle tools obj ~scrub:work in
              let* () = Tool_fs.write Fpath.(dir / "readelf.txt") readelf in

              let* disasm =
                Gnu_tools.objdump_disasm tools obj ~scrub:work ~drop_banner:true ~riscv_numeric:true
              in
              let* () = Tool_fs.write Fpath.(dir / "objdump.txt") disasm in

              let text_bin = Fpath.(work / (unit_name ^ ".text.bin")) in
              let* _ = Gnu_tools.objcopy_section tools ~src:obj ~section:".text" ~out:text_bin in
              let* text_bytes = Tool_fs.read text_bin in
              let* () = Tool_fs.write Fpath.(dir / "text.hex") (Hex_dump.of_bytes text_bytes) in

              let* headers = Gnu_tools.readelf_headers tools obj in
              let* objdump_r = Gnu_tools.objdump_relocs tools obj in
              let* relocs = Gnu_output.relocations ~objdump_r ~rela:(Gnu_output.has_rela headers) in
              let* () = Tool_fs.write Fpath.(dir / "reloc.txt") relocs in

              let* readelf_s = Gnu_tools.readelf_symbols tools obj in
              let* symbols = Gnu_output.symbols ~readelf_s in
              let* () = Tool_fs.write Fpath.(dir / "symbols.txt") symbols in

              let reloc_count =
                List.length (List.filter (fun l -> l <> "") (String.split_on_char '\n' relocs))
              in
              Ok
                (Printf.sprintf "%s/%s/%s: .text %d bytes, %d relocation(s)" case_name unit_name
                   target_s (String.length text_bytes) reloc_count
                :: acc)
            in
            let* unit_lines = List.fold_left per_unit (Ok []) unit_sources in
            let unit_lines = List.rev unit_lines in

            (* {2 The controlled reference link}

           Unresolved placeholder bytes are not encodings, so a fixture whose
           .text carries relocations cannot be byte-compared as an object. Both
           sides are resolved at the same addresses instead. Every unit's own
           object joins this one link - the oracle wildcard-matches by
           SECTION, not by object, so an N-object link needs no script change
           of its own. *)
            let script = Fpath.(work / "link.ld") in
            let* () = Tool_fs.write script (Link_script.oracle target) in
            let linked = Fpath.(work / "linked.elf") in
            let* () = Gnu_tools.link tools ~cwd:work ~script ~out:linked ~objs in

            (* Checked on the LINKED IMAGE, not the object: .bss and .eh_frame are
           allocated in every object and are legitimately absent here, one
           because it is empty and the other because the script discards it. *)
            let* linked_headers = Gnu_tools.objdump_headers tools linked in
            let* allocated = Gnu_output.alloc_sections linked_headers in
            let unexpected = List.filter (fun s -> not (List.mem s alloc_sections)) allocated in
            if unexpected <> [] then
              err Tool_error.Validate
                (Printf.sprintf "%s/%s: allocated section(s) outside the oracle's scope: %s"
                   case_name target_s (String.concat " " unexpected))
            else
              let linked_dir = Fpath.(out / "linked") in
              let* () = Tool_fs.mkdir_p linked_dir in
              (* Stale .hex files are removed, not overwritten: a section that
             stopped being emitted must not leave its previous dump behind,
             where the manifest would no longer name it but the traversal-based
             corpus check still would. *)
              let* () = Tool_fs.remove_matching linked_dir ~suffix:".hex" in
              (* A NOBITS section (.bss) has no bytes to extract - objcopy always
               reports it as absent-or-empty, the same signal a genuinely
               unallocated section gives - so its evidence is its LOGICAL size
               from the header table instead, and there is no .hex artifact for
               it at all (M3 §11, .ai/asm_plan.md §12). *)
              let* present =
                List.fold_left
                  (fun acc section ->
                    let* acc = acc in
                    if Gnu_output.section_is_nobits linked_headers ~name:section then
                      match Gnu_output.section_size linked_headers ~name:section with
                      | None -> Ok acc
                      | Some size -> Ok ((section, `Nobits, int_of_string ("0x" ^ size)) :: acc)
                    else
                      let name = hex_name section in
                      let bin = Fpath.(work / (name ^ ".bin")) in
                      let* got = Gnu_tools.objcopy_section tools ~src:linked ~section ~out:bin in
                      if not got then Ok acc
                      else
                        let* bytes = Tool_fs.read bin in
                        let* () =
                          Tool_fs.write
                            Fpath.(linked_dir / (name ^ ".hex"))
                            (Hex_dump.of_bytes bytes)
                        in
                        Ok ((section, `Progbits, String.length bytes) :: acc))
                  (Ok []) alloc_sections
              in
              let present = List.rev present in
              (* Manifest v2 (M3 §11): section, kind, address, size, artifact -
               versioned because v1 had no kind column and no way to say "this
               section has no byte artifact". kind is "progbits" or "nobits";
               the artifact column is "-" for nobits, since there is nothing to
               objcopy out of it. *)
              let manifest =
                String.concat ""
                  (List.map
                     (fun (section, kind, size) ->
                       let addr = Option.get (link_addr target section) in
                       let kind_s =
                         match kind with `Progbits -> "progbits" | `Nobits -> "nobits"
                       in
                       let artifact =
                         match kind with
                         | `Progbits -> "linked/" ^ hex_name section ^ ".hex"
                         | `Nobits -> "-"
                       in
                       Printf.sprintf "%s\t%s\t%s\t%s\t%s\n" section kind_s (Target.hex addr)
                         (Target.hex size) artifact)
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
                Tool_fs.write
                  Fpath.(out / "tool-versions.txt")
                  (String.concat "" (List.rev versions))
              in
              Ok
                (String.concat "\n"
                   (unit_lines
                   @ [
                       Printf.sprintf "%s/%s: linked sections %s" case_name target_s
                         (String.concat " " (List.map (fun (s, _, _) -> s) present));
                     ])))

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
