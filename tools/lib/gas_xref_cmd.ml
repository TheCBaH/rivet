let ( let* ) = Result.bind
let err op detail = Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail)

(* {1 The frontier sources}

   Three groups answering different questions: `fixture` are positive controls
   this assembler already handles; `helper` are our own hand-written ABI
   helpers; `runtime` is CompCert's runtime library, the largest body of real
   assembly available here. The runtime group covers all six targets: both
   RISC-V profiles share CompCert's arch-named `runtime/riscV` directory (see
   runtime_dir below), rather than each having its own target-named one. *)

type group = Fixture | Helper | Runtime

let group_name = function Fixture -> "fixture" | Helper -> "helper" | Runtime -> "runtime"

(* CompCert's runtime tree is arch-named, not target-named: both RISC-V
   profiles share runtime/riscV, and it is the MODEL_ define below - not the
   directory - that selects the 32- or 64-bit half of vararg.S. Every other
   target's arch name and target name coincide. *)
let runtime_dir = function Target.Riscv32 | Target.Riscv64 -> "riscV" | t -> Target.to_string t

(* CompCert compiles its runtime with -DMODEL_/-DABI_/-DENDIANNESS_/-DSYS_
   (modules/CompCert/runtime/Makefile:62) and the values are the ones its own
   configure picks per target. Without them FUNCTION is left undefined and every
   file preprocesses to text no assembler can read - which would have been
   recorded as a frontier finding and would have been an artifact of how we
   invoked cpp. *)
let runtime_defines = function
  | Target.Arm -> [ "-DMODEL_armv7a"; "-DABI_hardfloat"; "-DENDIANNESS_little"; "-DSYS_linux" ]
  | Target.X86_32 -> [ "-DMODEL_32sse2"; "-DABI_standard"; "-DENDIANNESS_little"; "-DSYS_linux" ]
  | Target.X86_64 -> [ "-DMODEL_64"; "-DABI_standard"; "-DENDIANNESS_little"; "-DSYS_linux" ]
  | Target.Aarch64 -> [ "-DMODEL_default"; "-DABI_standard"; "-DENDIANNESS_little"; "-DSYS_linux" ]
  | Target.Riscv32 -> [ "-DMODEL_32"; "-DABI_standard"; "-DENDIANNESS_little"; "-DSYS_linux" ]
  | Target.Riscv64 -> [ "-DMODEL_64"; "-DABI_standard"; "-DENDIANNESS_little"; "-DSYS_linux" ]

let frontier_sources repo target =
  let root = Repo.path repo in
  let t = Target.to_string target in
  let fixed =
    [
      (Fixture, "asm_test_entry", "asm/fixtures/compcert-3.17/return42/" ^ t ^ "/asm_test_entry.s");
      (Helper, "helper", "asm/helpers/" ^ t ^ ".s");
    ]
  in
  let dir = "modules/CompCert/runtime/" ^ runtime_dir target in
  let runtime =
    (* Sys.is_directory RAISES on a missing path, where the shell's `[ -d … ]`
       is merely false. Every target now maps to a directory that exists; this
       guard's remaining job is to fail soft if the submodule is uninitialized,
       rather than to skip RISC-V. *)
    let is_dir p = try Sys.is_directory p with Sys_error _ -> false in
    if not (is_dir (Fpath.to_string Fpath.(root // v dir))) then []
    else
      Sys.readdir (Fpath.to_string Fpath.(root // v dir))
      |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".S")
      |> List.sort String.compare
      |> List.map (fun f -> (Runtime, Filename.chop_suffix f ".S", dir ^ "/" ^ f))
  in
  fixed @ runtime

(* {2 Recording one case}

   Both outcomes are written, and the artifact set differs between them: an
   assembled case gets objdump.txt and text.hex alongside `ok`, a rejected one
   gets only the diagnostic. *)
let record_gas tools ~dir ~src ~include_dir =
  Tool_workspace.with_scratch ~label:"gas" (fun work ->
      let obj = Fpath.(work / "obj.o") in
      let* outcome = Gnu_tools.try_assemble tools ~src ~obj ~include_dir in
      match outcome with
      | Gnu_tools.Rejected body ->
          let* () = Tool_fs.write Fpath.(dir / "gas.txt") ("error\n" ^ body) in
          Ok false
      | Gnu_tools.Assembled ->
          let* () = Tool_fs.write Fpath.(dir / "gas.txt") "ok\n" in
          (* No riscv_numeric here, unlike the fixture oracle: this corpus
             records the ALIAS spelling. *)
          let* disasm =
            Gnu_tools.objdump_disasm tools obj ~scrub:work ~drop_banner:true ~riscv_numeric:false
          in
          let* () = Tool_fs.write Fpath.(dir / "objdump.txt") disasm in
          let bin = Fpath.(work / "text.bin") in
          let* _ = Gnu_tools.objcopy_section tools ~src:obj ~section:".text" ~out:bin in
          let* bytes = Tool_fs.read bin in
          let* () = Tool_fs.write Fpath.(dir / "text.hex") (Hex_dump.of_bytes bytes) in
          Ok true)

(* {3 Generated cases}

   The emitter is this project's own build, so a corpus can never be
   regenerated from an assembler that does not compile. *)
let emit_generated repo ~into =
  Tool_process.exec
    (Tool_process.spec
       ~cwd:Fpath.(Repo.path repo / "asm")
       ~stdout:Tool_process.Out_null ~stderr:Tool_process.Err_inherit
       ~accepted:Process_status.Zero_only ~label:"snippet_emit" "opam"
       [
         "exec"; "--"; "dune"; "exec"; "test/snippets/snippet_emit.exe"; "--"; Fpath.to_string into;
       ])
  |> Result.map ignore

let target_of_dir name = match Target.of_string name with Ok t -> Some t | Error _ -> None

let regen repo =
  let step =
    let corpus_root = Tool_workspace.gas_xref_corpus repo in
    let* () =
      (* Every target's `as`, up front: a missing tool half way through would
         leave a partially rebuilt corpus behind. *)
      List.fold_left
        (fun acc t ->
          let* () = acc in
          Gnu_tools.require (Gnu_tools.for_target t) ~qemu:false)
        (Ok ()) Target.all
    in
    let root = Tool_workspace.recreatable_path corpus_root in
    let* () = Tool_workspace.recreate_root corpus_root in
    let* () = Tool_fs.mkdir_p Fpath.(root / "generated") in
    let* () = Tool_fs.mkdir_p Fpath.(root / "frontier") in
    let* () = emit_generated repo ~into:Fpath.(root / "generated") in

    (* Generated cases: every input.s the emitter wrote, in bytewise order. *)
    let* inputs = Tool_fs.files ~root:Fpath.(root / "generated") ~exclude:(fun _ -> false) in
    let inputs = List.filter (fun p -> Filename.basename p = "input.s") inputs in
    let* generated =
      List.fold_left
        (fun acc rel ->
          let* n = acc in
          let dir = Fpath.(root / "generated" // v (Filename.dirname rel)) in
          (* .../generated/<target>/<case>/input.s *)
          let target_name = Filename.basename (Filename.dirname (Filename.dirname rel)) in
          match target_of_dir target_name with
          | None ->
              err Tool_error.Validate
                (Printf.sprintf "emitter produced an unknown target directory %S" target_name)
          | Some t ->
              let tools = Gnu_tools.for_target t in
              let* ok = record_gas tools ~dir ~src:Fpath.(dir / "input.s") ~include_dir:None in
              (* A generated case GNU as refuses is a bug in our canonical
                 printer, not a finding about the corpus: we produced that
                 text. This is the asymmetry with the frontier loop below. *)
              if not ok then
                err Tool_error.Validate
                  (Printf.sprintf "GNU as rejected generated case %s - see %s/gas.txt"
                     (Fpath.to_string dir) (Fpath.to_string dir))
              else Ok (n + 1))
        (Ok 0) inputs
    in
    if generated = 0 then err Tool_error.Validate "the emitter produced no cases"
    else
      let* frontier =
        List.fold_left
          (fun acc t ->
            let* acc = acc in
            let tools = Gnu_tools.for_target t in
            List.fold_left
              (fun acc (group, id, rel) ->
                let* m = acc in
                let src = Fpath.(Repo.path repo // v rel) in
                if not (Sys.file_exists (Fpath.to_string src)) then Ok m
                else
                  let dir =
                    Fpath.(root / "frontier" / Target.to_string t / (group_name group ^ "-" ^ id))
                  in
                  let* () = Tool_fs.mkdir_p dir in
                  let input = Fpath.(dir / "input.s") in
                  let* staged =
                    if Filename.check_suffix rel ".S" then
                      Gnu_tools.preprocess tools ~src ~out:input ~defines:(runtime_defines t)
                        ~include_dir:Fpath.(Repo.path repo // v (Filename.dirname rel))
                    else
                      let* () = Tool_fs.copy ~src ~dst:input in
                      Ok true
                  in
                  if not staged then
                    let* () = Tool_fs.remove_tree dir in
                    Ok m
                  else
                    let* () = Tool_fs.write Fpath.(dir / "origin.txt") (rel ^ "\n") in
                    (* The helpers .include a shared file, which gas resolves
                       relative to -I. *)
                    let include_dir =
                      match group with
                      | Helper -> Some Fpath.(Repo.path repo / "asm" / "helpers")
                      | _ -> None
                    in
                    (* Unlike the generated loop, a rejection here is RECORDED
                       EVIDENCE and the run continues. *)
                    let* _ = record_gas tools ~dir ~src:input ~include_dir in
                    Ok (m + 1))
              (Ok acc) (frontier_sources repo t))
          (Ok 0) Target.all
      in
      let* versions =
        List.fold_left
          (fun acc t ->
            let* lines = acc in
            let tools = Gnu_tools.for_target t in
            let name = Target.to_string t in
            let* a = Gnu_tools.version_line tools `As in
            let* o = Gnu_tools.version_line tools `Objdump in
            Ok
              (Printf.sprintf "objdump:%s\t%s\n" name o
              :: Printf.sprintf "as:%s\t%s\n" name a
              :: lines))
          (Ok []) Target.all
      in
      let* () =
        Tool_fs.write Fpath.(root / "tool-versions.txt") (String.concat "" (List.rev versions))
      in
      (* Through Manifest, not a private writer: serialize sorts rendered lines
         bytewise, which is what `LC_ALL=C sort` did, and the corpus traversal
         excludes manifest.txt* exactly as the shell's find did. *)
      let* files = Corpus.files root in
      let* records =
        List.fold_left
          (fun acc rel ->
            let* rs = acc in
            let* h = Tool_fs.sha256 Fpath.(root // v rel) in
            Ok ({ Manifest.key = Manifest.Sha256 rel; value = Some h } :: rs))
          (Ok
             [
               { Manifest.key = Manifest.Generator; value = Some "compcert-tools gas-xref regen" };
               {
                 Manifest.key = Manifest.Inputs;
                 value = Some "generated by asm/test/snippets/snippet_emit.ml from the AST corpus";
               };
             ])
          files
      in
      let* () =
        Tool_fs.write
          Fpath.(root / "manifest.txt")
          (Manifest.serialize (Manifest.of_records (List.rev records)))
      in
      Ok (Printf.sprintf "gas-xref: %d generated cases, %d frontier cases" generated frontier)
  in
  match step with Error e -> Command.of_error e | Ok line -> Command.ok [ Diagnostic.stdout line ]
