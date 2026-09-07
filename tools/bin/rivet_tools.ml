(* The single entry point for the migrated tools.

   {1 What Cmdliner must NOT be allowed to decide}

   The --legacy-prog machinery retired with the launcher in Phase 8, along with
   the $0-interpolated usage text it existed to reproduce. One distinction it
   was built around OUTLIVES it, because it is about values rather than
   compatibility:

   - no target at all exits 2 - a malformed command line;
   - a target that is not one of the six exits 1 - a rejected VALUE.

   That is why the positional is OPTIONAL at the Cmdliner layer, so "no target"
   reaches the term rather than becoming a parse error that never runs it, and
   why the target is validated by Target.of_string INSIDE the term - a Cmdliner
   converter failure would be a parse error and would exit 2 for both. *)

open Compcert_tools

let err_trace_flag =
  let doc =
    "Print the full error trail - detection origin and boundary events - instead of the historical \
     one-line diagnostic. Native only; the compatibility launchers never pass it."
  in
  Cmdliner.Arg.(value & flag & info [ "err-trace" ] ~doc)

let repo_root_flag =
  let doc = "Repository root. Overrides COMPCERT_REPO_ROOT and the upward search." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "repo-root" ] ~docv:"DIR" ~doc)

let resolve_repo cli =
  Repo.resolve ~cli:(Option.map Fpath.v cli) ~env:Sys.getenv_opt ~cwd:(Fpath.v (Sys.getcwd ()))

let with_repo cli f =
  match resolve_repo cli with Error e -> Command.of_error e | Ok repo -> f repo

let cases_arg = Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"CASE")

let capability_arg =
  let doc =
    "Which target set to print: fixture, assembler or libc. `emit` instead renders \
     tools/target-matrix.sh."
  in
  Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"SET" ~doc)

let common = Cmdliner.Term.(const (fun e r -> (e, r)) $ err_trace_flag $ repo_root_flag)

let fixture_check_cmd =
  let run (err_trace, root) cases =
    (err_trace, with_repo root (fun repo -> Check_cmd.fixture_check repo ~cases))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check" ~doc:"Verify committed fixture bytes against the manifests")
    Cmdliner.Term.(const run $ common $ cases_arg)

(* Positional, and OPTIONAL at the Cmdliner layer. A `required` positional would
   make "no target" a PARSE error, which exits 2 without the term ever running -
   and the shell's no-target case also exits 2, so that would look right while
   being wrong for the badtarget case, which must exit 1. Both go through the
   term instead. *)
let verify_target_arg = Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"TARGET")
let verify_cases_arg = Cmdliner.Arg.(value & pos_right 0 string [] & info [] ~docv:"CASE")

let fixture_verify_cmd =
  let run (err_trace, root) target cases =
    let command =
      match target with
      (* Optional at the Cmdliner layer so "no target" reaches the term rather
         than becoming a parse error that never runs it. Cmdliner's own help
         covers the message; what matters here is that this exits 2 while a
         REJECTED target below exits 1. *)
      | None -> { Command.events = []; exit = `Usage }
      | Some name -> (
          (* Validated INSIDE the term, never by a Cmdliner converter: a
             converter failure is a parse error and exits 2, where this must
             exit 1 after printing usage AND a fatal. *)
          match Target.of_string name with
          | Error e -> Fixture_cmd.usage_error (Err.Error.kind e).Tool_error.detail
          | Ok target -> with_repo root (fun repo -> Fixture_cmd.verify repo ~target ~cases))
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "verify" ~doc:"Regenerate in scratch and byte-compare")
    Cmdliner.Term.(const run $ common $ verify_target_arg $ verify_cases_arg)

let fixture_regen_cmd =
  let run (err_trace, root) cases =
    (err_trace, with_repo root (fun repo -> Fixture_cmd.regen repo ~cases))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen" ~doc:"Regenerate every fixture; requires all six compilers")
    Cmdliner.Term.(const run $ common $ cases_arg)

let fixture_rehash_cmd =
  let run (err_trace, root) cases =
    (err_trace, with_repo root (fun repo -> Fixture_cmd.rehash repo ~cases))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "rehash" ~doc:"Recompute the manifests without recompiling")
    Cmdliner.Term.(const run $ common $ cases_arg)

(* The oracle takes `all` or one target, matching the shell's positional. It is
   NOT the verify shape: `all` is a legal value here and the target is the
   first positional rather than a mode selector. *)
let oracle_target_arg = Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"TARGET|all")
let oracle_cases_arg = Cmdliner.Arg.(value & pos_right 0 string [] & info [] ~docv:"CASE")

let fixture_oracle_cmd =
  let run (err_trace, root) target cases =
    let command =
      match target with
      | None -> Fixture_cmd.usage_error "no target given"
      | Some "all" -> with_repo root (fun repo -> Oracle_cmd.run repo ~targets:Target.all ~cases)
      | Some name -> (
          match Target.of_string name with
          | Error e -> Fixture_cmd.usage_error (Err.Error.kind e).Tool_error.detail
          | Ok t -> with_repo root (fun repo -> Oracle_cmd.run repo ~targets:[ t ] ~cases))
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "oracle" ~doc:"Record the GNU reference-assembler artifacts")
    Cmdliner.Term.(const run $ common $ oracle_target_arg $ oracle_cases_arg)

let fixture_exec_cmd =
  let run (err_trace, root) target cases =
    let command =
      match target with
      | None -> Fixture_cmd.usage_error "no target given"
      | Some "all" ->
          with_repo root (fun repo -> Exec_cmd.run repo ~targets:(Target.set Target.Fixture) ~cases)
      | Some name -> (
          match Target.of_string name with
          | Error e -> Fixture_cmd.usage_error (Err.Error.kind e).Tool_error.detail
          | Ok t -> with_repo root (fun repo -> Exec_cmd.run repo ~targets:[ t ] ~cases))
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "exec" ~doc:"Execute every fixture under qemu-user")
    Cmdliner.Term.(const run $ common $ oracle_target_arg $ oracle_cases_arg)

let fixture_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "fixture" ~doc:"The CompCert fixture corpus")
    [
      fixture_check_cmd;
      fixture_verify_cmd;
      fixture_regen_cmd;
      fixture_rehash_cmd;
      fixture_oracle_cmd;
      fixture_exec_cmd;
    ]

let gas_xref_check_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Check_cmd.gas_xref_check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check" ~doc:"Verify committed gas cross-reference bytes")
    Cmdliner.Term.(const run $ common)

let gas_xref_regen_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Gas_xref_cmd.regen) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen" ~doc:"Rebuild the gas cross-reference corpus")
    Cmdliner.Term.(const run $ common)

let gas_xref_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "gas-xref" ~doc:"The GNU as cross-reference corpus")
    [ gas_xref_check_cmd; gas_xref_regen_cmd ]

let isa_inventory_regen_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_inventory_cmd.regen) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen"
       ~doc:"Rebuild the whole-ISA instruction inventory (RISC-V only so far)")
    Cmdliner.Term.(const run $ common)

let isa_inventory_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "isa-inventory" ~doc:"The whole-ISA instruction/extension inventory")
    [ isa_inventory_regen_cmd ]

let corpus_check_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Corpus_classify_cmd.check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check"
       ~doc:"Verify every already-published CompCert c/ classification, no toolchain needed")
    Cmdliner.Term.(const run $ common)

(* One explicit subcommand per target, never a `--target` flag with
   unimplemented values (asm/docs/corpus.md's own Follow-ups) - `Target.of_string`
   stays internal to Target.all's own literals here, not exposed to argv. *)
let corpus_classify_c_cmd (target : Target.t) =
  let name = "classify-c-" ^ Target.to_string target in
  let run (err_trace, root) =
    (err_trace, with_repo root (fun repo -> Corpus_classify_cmd.classify_c repo target))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info name
       ~doc:
         (Printf.sprintf
            "Compile CompCert's test/c/ suite for %s and classify each file against the parser"
            (Target.to_string target)))
    Cmdliner.Term.(const run $ common)

let corpus_check_assemble_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Corpus_assemble_cmd.check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check-assemble"
       ~doc:
         "Verify every already-published CompCert c/ assemble classification, no toolchain needed")
    Cmdliner.Term.(const run $ common)

(* Same one-subcommand-per-target discipline as corpus_classify_c_cmd. *)
let corpus_assemble_c_cmd (target : Target.t) =
  let name = "assemble-c-" ^ Target.to_string target in
  let run (err_trace, root) =
    (err_trace, with_repo root (fun repo -> Corpus_assemble_cmd.assemble_c repo target))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info name
       ~doc:
         (Printf.sprintf
            "Compile CompCert's test/c/ suite for %s and run each generated .s through the full \
             parse/simplify/lower/encode/plan_image pipeline"
            (Target.to_string target)))
    Cmdliner.Term.(const run $ common)

(* One explicit subcommand per (suite, target) pair, same discipline as
   corpus_classify_c_cmd - never a --suite/--target flag accepting an
   unimplemented value. *)
let corpus_classify_suite_cmd ~(spec : Corpus_classify_cmd.suite_spec) (target : Target.t) =
  let name = "classify-" ^ spec.suite_tag ^ "-" ^ Target.to_string target in
  let run (err_trace, root) =
    (err_trace, with_repo root (fun repo -> Corpus_classify_cmd.classify_suite spec repo target))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info name
       ~doc:
         (Printf.sprintf
            "Compile CompCert's test/%s/ suite for %s and classify each file against the parser"
            spec.test_subdir (Target.to_string target)))
    Cmdliner.Term.(const run $ common)

let corpus_check_suite_cmd ~(spec : Corpus_classify_cmd.suite_spec) =
  let name = "check-" ^ spec.suite_tag in
  let run (err_trace, root) =
    (err_trace, with_repo root (fun repo -> Corpus_classify_cmd.check_suite spec repo))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info name
       ~doc:
         (Printf.sprintf
            "Verify every already-published CompCert %s/ classification, no toolchain needed"
            spec.test_subdir))
    Cmdliner.Term.(const run $ common)

let corpus_classify_regression_cmd =
  corpus_classify_suite_cmd ~spec:Corpus_classify_cmd.regression_spec

let corpus_classify_compression_cmd =
  corpus_classify_suite_cmd ~spec:Corpus_classify_cmd.compression_spec

(* classify-c-gcc: same test/c/ corpus as corpus_classify_c_cmd, compiled with
   the system cross gcc (Gcc) instead of ccomp - a second, independent-compiler
   classification, published under its own asm/fixtures/corpus/c-gcc/<target>/
   destination (Corpus_classify_gcc_cmd). Same one-explicit-subcommand-per-
   target discipline as every other corpus subcommand. *)
let corpus_check_c_gcc_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Corpus_classify_gcc_cmd.check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check-c-gcc"
       ~doc:"Verify every already-published CompCert c-gcc/ classification, no toolchain needed")
    Cmdliner.Term.(const run $ common)

let corpus_classify_c_gcc_cmd (target : Target.t) =
  let name = "classify-c-gcc-" ^ Target.to_string target in
  let run (err_trace, root) =
    (err_trace, with_repo root (fun repo -> Corpus_classify_gcc_cmd.classify_c_gcc repo target))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info name
       ~doc:
         (Printf.sprintf
            "Compile CompCert's test/c/ suite for %s with gcc and classify each file against the \
             parser"
            (Target.to_string target)))
    Cmdliner.Term.(const run $ common)

let corpus_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "corpus" ~doc:"CompCert's own test suites, classified against the parser")
    ((corpus_check_cmd :: List.map corpus_classify_c_cmd Target.all)
    @ (corpus_check_assemble_cmd :: List.map corpus_assemble_c_cmd Target.all)
    @ corpus_check_suite_cmd ~spec:Corpus_classify_cmd.regression_spec
      :: List.map corpus_classify_regression_cmd Target.all
    @ corpus_check_suite_cmd ~spec:Corpus_classify_cmd.compression_spec
      :: List.map corpus_classify_compression_cmd Target.all
    @ (corpus_check_c_gcc_cmd :: List.map corpus_classify_c_gcc_cmd Target.all))

let targets_cmd =
  let run (err_trace, _root) set =
    let command =
      match set with
      | "fixture" -> Check_cmd.targets Target.Fixture
      | "assembler" -> Check_cmd.targets Target.Assembler
      | "libc" -> Check_cmd.targets Target.Libc_smoke
      (* Not a target set, but it belongs on this command: it is the same
         Target.config data rendered for the other language. *)
      | "emit" -> Target_emit.emit ()
      | other ->
          Command.of_error
            (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
               (Tool_error.v Tool_error.Usage
                  (Printf.sprintf
                     "unknown target set '%s': expected fixture, assembler, libc or emit" other)))
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "targets" ~doc:"Print a named target set, one per line")
    Cmdliner.Term.(const run $ common $ capability_arg)

(* The mode defaults to `all`, as the shell's `${1:-all}` does, so "no argument"
   is a legal invocation rather than a usage error - which is why this positional
   is optional and validated inside the term. *)
let gate_mode_arg =
  Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"all|user|system|gdb")

let tool_gate_cmd =
  let run (err_trace, root) mode =
    let command =
      let requested = Option.value ~default:"all" mode in
      match Tool_gate_cmd.mode_of_string requested with
      | None -> Tool_gate_cmd.usage_error requested
      | Some mode -> with_repo root (fun repo -> Tool_gate_cmd.run repo ~mode ~env:Sys.getenv_opt)
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "tool-gate" ~doc:"Ask the container's tools to do what the project needs")
    Cmdliner.Term.(const run $ common $ gate_mode_arg)

let main_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "compcert-tools" ~doc:"CompCert assembler repository tooling")
    [ fixture_cmd; gas_xref_cmd; corpus_cmd; isa_inventory_cmd; targets_cmd; tool_gate_cmd ]

let () =
  (* At the entry point, not at module initialization: this is a process-wide
     policy and it belongs where the process starts. `deterministic` keeps the
     explicit ~pos:__POS__ origins and the semantic boundary events while
     disabling nondeterministic automatic stack capture - the same policy the
     assembler already uses. *)
  Err.Config.set Err.Config.deterministic;
  (* ~catch:false because Cmdliner's documented default writes the exception AND
     its stack trace to stderr before returning `Exn, which would contradict the
     one-diagnostic promise. The handler below produces the single deterministic
     internal-error line instead. *)
  let code =
    match Cmdliner.Cmd.eval_value ~catch:false main_cmd with
    | Ok (`Ok (err_trace, command)) -> Command.render ~err_trace command
    | Ok (`Help | `Version) -> 0
    | Error (`Parse | `Term) -> 2
    | Error `Exn -> 1 (* unreachable with ~catch:false *)
    | exception e ->
        prerr_endline
          (Tool_error.to_fatal_line
             (Tool_error.v Tool_error.Exec ("internal error: " ^ Printexc.to_string e)));
        1
  in
  exit code
