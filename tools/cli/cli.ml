(* The command line of the repository tools, as a library so that a consumer
   can run it with its own compiler and add subcommands of its own.

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

open Rivet_tools

let err_trace_flag =
  let doc =
    "Print the full error trail - detection origin and boundary events - instead of the historical \
     one-line diagnostic. Native only; the compatibility launchers never pass it."
  in
  Cmdliner.Arg.(value & flag & info [ "err-trace" ] ~doc)

let repo_root_flag =
  let doc = "Repository root. Overrides RIVET_ROOT and the upward search." in
  Cmdliner.Arg.(value & opt (some string) None & info [ "repo-root" ] ~docv:"DIR" ~doc)

let resolve_repo cli =
  Repo.resolve ~cli:(Option.map Fpath.v cli) ~env:Sys.getenv_opt ~cwd:(Fpath.v (Sys.getcwd ()))

(* Set once by [main] before any term runs. *)
let fixtures_layout : (string * string) option ref = ref None

let with_repo cli f =
  match resolve_repo cli with
  | Error e -> Command.of_error e
  | Ok repo ->
      let repo =
        match !fixtures_layout with
        | None -> repo
        | Some (sources, outputs) -> Repo.with_fixtures repo ~sources ~outputs
      in
      f repo

let cases_arg = Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"CASE")

let capability_arg =
  let doc =
    "Which target set to print: fixture, assembler or libc. `emit` instead renders \
     scripts/target-matrix.sh."
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

let fixture_verify_cmd ~compiler ~preexisting =
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
          | Ok target ->
              with_repo root (fun repo ->
                  Fixture_cmd.verify ?preexisting repo ~compiler ~target ~cases))
    in
    (err_trace, command)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "verify" ~doc:"Regenerate in scratch and byte-compare")
    Cmdliner.Term.(const run $ common $ verify_target_arg $ verify_cases_arg)

let fixture_regen_cmd ~compiler ~preexisting =
  let run (err_trace, root) cases =
    (err_trace, with_repo root (fun repo -> Fixture_cmd.regen ?preexisting repo ~compiler ~cases))
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen" ~doc:"Regenerate every fixture; requires every target's compiler")
    Cmdliner.Term.(const run $ common $ cases_arg)

let fixture_rehash_cmd ~compiler =
  let run (err_trace, root) cases =
    (err_trace, with_repo root (fun repo -> Fixture_cmd.rehash repo ~compiler ~cases))
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

let fixture_cmd ~compiler ~preexisting =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "fixture" ~doc:"The fixture corpus")
    [
      fixture_check_cmd;
      fixture_verify_cmd ~compiler ~preexisting;
      fixture_regen_cmd ~compiler ~preexisting;
      fixture_rehash_cmd ~compiler;
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

let isa_db_cross_validate_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_db_cross_validate.check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "cross-validate"
       ~doc:"Check every isa-inventory manifest row against the checked-in isa-db/ JSONL export")
    Cmdliner.Term.(const run $ common)

let isa_norm_accounting_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_norm_accounting.run) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "norm-accounting"
       ~doc:
         "Report, per checked-in isa-db export, how many records Isa_norm_riscv/Isa_norm_xed \
          normalize versus which diagnostic rule turns each remaining one away")
    Cmdliner.Term.(const run $ common)

let isa_family_admission_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_family_admission.run) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "family-admission"
       ~doc:
         "Classify every checked-in ISA-source record by source-native family as normalized-only, \
          GAS-generatable, promoted-support, oracle-unavailable, or its specific blocker")
    Cmdliner.Term.(const run $ common)

let isa_family_records_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_family_admission.record_lines) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "family-records"
       ~doc:
         "List every checked-in ISA-source record with its family, admission state, lookup key, \
          normalized form id and record id")
    Cmdliner.Term.(const run $ common)

let isa_table_cmd =
  let emit =
    let run (err_trace, root) = (err_trace, with_repo root Isa_riscv_table.run_emit) in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "riscv-emit"
         ~doc:"Regenerate targets/riscv_family/riscv_table_rows.ml from the riscv-opcodes exports")
      Cmdliner.Term.(const run $ common)
  in
  let check =
    let run (err_trace, root) = (err_trace, with_repo root Isa_riscv_table.run_check) in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "riscv-check"
         ~doc:"Fail unless the checked-in RISC-V table rows equal a fresh emission")
      Cmdliner.Term.(const run $ common)
  in
  let x86_emit =
    let run (err_trace, root) = (err_trace, with_repo root Isa_x86_table_emit.run_emit) in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "x86-emit"
         ~doc:"Regenerate targets/x86_family/x86_table_rows.ml from the XED exports")
      Cmdliner.Term.(const run $ common)
  in
  let x86_check =
    let run (err_trace, root) = (err_trace, with_repo root Isa_x86_table_emit.run_check) in
    Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "x86-check"
         ~doc:"Fail unless the checked-in x86 table rows equal a fresh emission")
      Cmdliner.Term.(const run $ common)
  in
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "isa-table" ~doc:"Generated ISA form tables (DEC-RV-TABLE, DEC-X86-TABLE)")
    [ emit; check; x86_emit; x86_check ]

let isa_residual_ledger_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_residual_ledger.run) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "residual-ledger"
       ~doc:
         "Print every source family that still has blocked records with the row that owns it \
          (missing capability, evidence, task, reopening gate) and fail if any blocked family is \
          unowned, doubly owned, or a row is stale")
    Cmdliner.Term.(const run $ common)

let isa_inventory_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "isa-inventory" ~doc:"The whole-ISA instruction/extension inventory")
    [
      isa_inventory_regen_cmd;
      isa_db_cross_validate_cmd;
      isa_norm_accounting_cmd;
      isa_family_admission_cmd;
      isa_family_records_cmd;
      isa_residual_ledger_cmd;
    ]

(* Isa_generated_case.cli_group_name/make_target freeze this group's own name
   and its Make targets' names; "check" replays the corpus "regen" commits,
   offline and without a toolchain. *)
let isa_generated_check_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Check_cmd.isa_generated_check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check"
       ~doc:
         "Replay the committed isa-generated pilot corpus offline, without a toolchain, and reject \
          any pilot manifest entry missing from it")
    Cmdliner.Term.(const run $ common)

let isa_generated_regen_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_generated_cmd.regen) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen"
       ~doc:
         "Build and run the frozen S3 pilot manifest's canonical cases against the real cross GNU \
          binutils, reporting exact bytes and observed-form checks, and commit the corpus \
          isa-generated check replays")
    Cmdliner.Term.(const run $ common)

let isa_generated_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "isa-generated" ~doc:"The pilot GAS differential generator")
    [ isa_generated_check_cmd; isa_generated_regen_cmd ]

(* Isa_gen_difficult.cli_group_name/check_make_target/regen_make_target freeze
   this group's own name and Make targets, mirroring isa-generated's own
   naming above but for the separate, non-frozen difficult-form corpus. *)
let isa_difficult_check_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Check_cmd.isa_difficult_check) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check"
       ~doc:
         "Replay the committed isa-difficult corpus offline, without a toolchain, and reject any \
          difficult manifest entry missing from it")
    Cmdliner.Term.(const run $ common)

let isa_difficult_regen_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_difficult_cmd.regen) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "regen"
       ~doc:
         "Build and run the difficult-form manifest's cases (RISC-V sw/beq legal offset and \
          branch-label domains) against the real cross GNU binutils, and commit the corpus \
          isa-difficult check replays")
    Cmdliner.Term.(const run $ common)

let isa_difficult_coverage_cmd =
  let run (err_trace, root) = (err_trace, with_repo root Isa_coverage_class.run) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "coverage"
       ~doc:
         "Report the committed corpora by coverage class (canonical, alias, pseudo, negative) per \
          profile, with each class's per-architecture obligation, and fail if a satisfied \
          obligation has no case or an unsatisfied one is stale")
    Cmdliner.Term.(const run $ common)

let isa_difficult_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "isa-difficult" ~doc:"The difficult-form GAS differential generator")
    [ isa_difficult_check_cmd; isa_difficult_regen_cmd; isa_difficult_coverage_cmd ]

let targets_cmd =
  let run (err_trace, _root) set =
    let command =
      match set with
      | "fixture" -> Check_cmd.targets Target.Fixture
      | "assembler" -> Check_cmd.targets Target.Assembler
      (* Not a target set, but it belongs on this command: it is the same
         Target.config data rendered for the other language. *)
      | "emit" -> Target_emit.emit ()
      | other ->
          Command.of_error
            (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
               (Tool_error.v Tool_error.Usage
                  (Printf.sprintf "unknown target set '%s': expected fixture, assembler or emit"
                     other)))
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

(* The command tree, with the compiler the fixture corpus is generated with.
   [extra] are appended, so a consumer adds its own subcommands to this
   executable's without forking it. *)
let main_cmd ~compiler ~preexisting ~name ~doc ~extra =
  Cmdliner.Cmd.group (Cmdliner.Cmd.info name ~doc)
    ([
       fixture_cmd ~compiler ~preexisting;
       gas_xref_cmd;
       isa_inventory_cmd;
       isa_generated_cmd;
       isa_difficult_cmd;
       isa_table_cmd;
       targets_cmd;
       tool_gate_cmd;
     ]
    @ extra)

let main ?preexisting ?(name = "rivet-tools") ?(doc = "Assembler repository tooling") ?(extra = [])
    ?fixtures ~compiler () =
  fixtures_layout := fixtures;
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
  match
    Cmdliner.Cmd.eval_value ~catch:false (main_cmd ~compiler ~preexisting ~name ~doc ~extra)
  with
  | Ok (`Ok (err_trace, command)) -> Command.render ~err_trace command
  | Ok (`Help | `Version) -> 0
  | Error (`Parse | `Term) -> 2
  | Error `Exn -> 1 (* unreachable with ~catch:false *)
  | exception e ->
      prerr_endline
        (Tool_error.to_fatal_line
           (Tool_error.v Tool_error.Exec ("internal error: " ^ Printexc.to_string e)));
      1
