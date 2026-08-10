(* The QEMU user-mode host runner (asm/docs/exec-abi-v1.md §15, and §11.5.2 of
   .ai/asm_plan.md's architecture).

   Serializes a manifest, runs the profile's helper under its emulator, enforces
   a wall-clock timeout, recovers the result record and normalizes the outcome.

   Two things here are not conveniences and must not be simplified away.

   The first is that attribution is by *phase*, not by signal number. The same
   QEMU process runs the helper and the guest, so a helper fault during manifest
   handling, mprotect, cache maintenance or result publication produces an
   identical wait status to a deliberate guest trap. Only the committed
   record_state/status pair distinguishes them, and the same table also decides
   what our own timeout kill means - otherwise a helper looping in manifest
   processing would produce the same artifact as the deliberately looping guest
   used to establish E4, letting a runner defect masquerade as a working
   timeout control.

   The second is that the subcode is read only from a record that validates.
   The class comes from the exit code, because open/ftruncate/control-alias and
   reservation failures can leave no record at all; a subcode invented for those
   would be a guess presented as an observation.

   Unix lives here and only here on the host side. §3.7 bans it from every
   production and browser package, and tools/asm-check-purity.sh enforces that
   over the resolved dependency DAG rather than by convention. *)

open Asm_oracle

type termination =
  | Completed
  | Fault  (** a guest fault: a trap signal arriving with the guest phase committed *)
  | Signal of int
  | Timeout  (** the guest itself ran too long *)
  | Transport_timeout  (** our kill landed before the guest was entered *)
  | Post_return_timeout  (** our kill landed after the guest returned *)
  | Runner_error of Abi.error_class
  | Unexpected_exit of int

type t = {
  termination : termination;
  record : Record.verdict option;  (** [None] when no result file exists at all *)
  wall_ms : int;  (** not part of the normalized artifact; kept for diagnosis only *)
}

(* {1 Locating a profile's helper}

   tools/asm-helpers.sh writes both the ELF and the emulator name, so the
   matrix in tools/target-matrix.sh stays the single definition of which
   emulator runs which profile. *)

let helpers_dir () = try Sys.getenv "ASM_HELPERS_DIR" with Not_found -> ".asm-helpers"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc contents)

type helper = { elf : string; qemu : string }

let helper_for profile =
  let dir = Filename.concat (helpers_dir ()) (Abi.profile_name profile) in
  let elf = Filename.concat dir "helper" in
  let qemu = Filename.concat dir "qemu" in
  if Sys.file_exists elf && Sys.file_exists qemu then
    Some { elf; qemu = String.trim (read_file qemu) }
  else None

let available_profiles () = List.filter (fun p -> helper_for p <> None) Abi.all_profiles

(* M0.3 requires helper hashes and tool versions in provenance;
   tools/asm-helpers.sh records them beside the ELF and the conformance run
   reproduces them, so a result is attributable to the exact sources and
   emulator that produced it. The APT packages are range-checked rather than
   digest-pinned, so this is how a base-image update that moved QEMU or the
   assembler under us shows up in the artifact instead of silently. *)
let provenance profile =
  let path =
    Filename.concat (Filename.concat (helpers_dir ()) (Abi.profile_name profile)) "provenance.txt"
  in
  if Sys.file_exists path then String.split_on_char '\n' (String.trim (read_file path)) else []

(* {1 The private temporary directory}

   §15.3: the result path is created O_EXCL by the helper inside a directory
   only we can write, so freshness is atomic rather than depending on an earlier
   existence check surviving a race. We create the directory; we never create
   the file. *)

let with_temp_dir f =
  let rec make n =
    if n > 20 then failwith "qemu_user: could not create a private temporary directory"
    else
      let path =
        Filename.concat (Filename.get_temp_dir_name ())
          (Printf.sprintf "asm-exec-%d-%d" (Unix.getpid ()) (Random.bits ()))
      in
      match Unix.mkdir path 0o700 with
      | () -> path
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> make (n + 1)
  in
  let dir = make 0 in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun e -> try Unix.unlink (Filename.concat dir e) with Unix.Unix_error _ -> ())
        (try Sys.readdir dir with Sys_error _ -> [||]);
      try Unix.rmdir dir with Unix.Unix_error _ -> ())
    (fun () -> f dir)

(* {1 Running}

   The timeout is the host's, not the manifest's: timeout_ms on the wire is
   advisory (§8) and a helper that hangs cannot be trusted to enforce its own
   deadline. SIGKILL rather than SIGTERM, because a helper has no signal
   handlers and a catchable signal would be one more thing that could hang. *)

let wait_with_timeout pid ~timeout_s =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec poll () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          let _, status = Unix.waitpid [] pid in
          (status, true))
        else (
          Unix.sleepf 0.002;
          poll ())
    | _, status -> (status, false)
  in
  poll ()

(* {1 Phase attribution (§15.1)}

   [Guest] is committed only by [validated] + [running]: the helper has switched
   to the guest stack and published that it is about to call. Everything else -
   including [returned], where control has come back - is the helper's own
   execution, and a signal there is transport failure rather than evidence about
   guest code. *)

type phase = Guest | Transport

let phase_of = function
  | Some (Record.Valid r) -> (
      match (r.Record.record_state, r.Record.status) with
      | Abi.Validated, Abi.Running -> Guest
      | _ -> Transport)
  | _ -> Transport

let fault_signals = [ Sys.sigill; Sys.sigsegv; Sys.sigbus; Sys.sigfpe; Sys.sigtrap ]

let classify ~status ~killed ~record =
  let phase = phase_of record in
  if killed then
    match (phase, record) with
    | Guest, _ -> Timeout
    | Transport, Some (Record.Valid r) when r.Record.record_state = Abi.Returned ->
        Post_return_timeout
    | Transport, _ -> Transport_timeout
  else
    match status with
    | Unix.WEXITED 0 -> Completed
    | Unix.WEXITED code when code >= Abi.exit_code_min && code <= Abi.exit_code_max -> (
        match Abi.class_of_code code with Some c -> Runner_error c | None -> Unexpected_exit code)
    | Unix.WEXITED code -> Unexpected_exit code
    | Unix.WSIGNALED s when List.mem s fault_signals && phase = Guest -> Fault
    | Unix.WSIGNALED s -> Signal s
    | Unix.WSTOPPED s -> Signal s

(* How the input path is presented to the helper. The two non-file forms exist
   because io subcodes 3 and 5 are otherwise unobservable: a manifest that is
   never created exercises the input open, and a directory opens and fstats
   fine but cannot be mapped. Inventing them as unit tests of the host would
   prove nothing about the helper. *)
type input = File  (** write the manifest and pass its path *) | Missing | Directory

let run ?(timeout_s = 10.0) ?(input = File) ?(extra_args = []) ~profile ~manifest ~expected_case_id
    () =
  match helper_for profile with
  | None -> invalid_arg ("qemu_user: no helper built for " ^ Abi.profile_name profile)
  | Some h ->
      with_temp_dir (fun dir ->
          let result_path = Filename.concat dir "result.bin" in
          let manifest_path =
            match input with
            | File ->
                let p = Filename.concat dir "manifest.bin" in
                write_file p manifest;
                p
            | Missing -> Filename.concat dir "absent.bin"
            | Directory -> dir
          in
          let t0 = Unix.gettimeofday () in
          let argv =
            Array.of_list (h.qemu :: h.elf :: manifest_path :: result_path :: extra_args)
          in
          (* fork/chdir/exec rather than create_process: qemu-user writes a
             qemu_<name>_<stamp>_<pid>.core file into the *current* directory
             whenever the emulated process takes a fatal signal, and the fault
             and guard-page controls take one deliberately. Running the child
             inside the private temporary directory puts those where the
             cleanup already removes them, instead of scattering them through
             whatever tree the suite happened to be run from. *)
          let pid =
            match Unix.fork () with
            | 0 ->
                (try
                   Unix.chdir dir;
                   (* execvp, not execv: the emulator is named by the matrix as
                      a bare command and is found on PATH. *)
                   Unix.execvp h.qemu argv
                 with _ -> ());
                (* execv only returns on failure, and this is a forked child:
                   _exit, never exit, so no at_exit handler or buffered output
                   of the parent runs twice. 127 is what a shell reports for an
                   unrunnable command, and the host normalizes it as an
                   unexpected exit rather than a guest result. *)
                Unix._exit 127
            | pid -> pid
          in
          let status, killed = wait_with_timeout pid ~timeout_s in
          let wall_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.) in
          let record =
            if Sys.file_exists result_path then
              Some (Record.validate ~expected_case_id (read_file result_path))
            else None
          in
          { termination = classify ~status ~killed ~record; record; wall_ms })

(* {1 The normalized artifact}

   Host paths, PIDs and timings are deliberately absent: this is what a test
   pins, and a rendering that carried any of them would differ between two
   correct runs. [wall_ms] stays on the record for a human debugging a
   failure and never appears here. *)

let pp_termination ppf = function
  | Completed -> Fmt.pf ppf "completed"
  | Fault -> Fmt.pf ppf "fault"
  | Signal s -> Fmt.pf ppf "signal %d" s
  | Timeout -> Fmt.pf ppf "timeout"
  | Transport_timeout -> Fmt.pf ppf "runner_error/transport_timeout"
  | Post_return_timeout -> Fmt.pf ppf "runner_error/post_return_timeout"
  | Runner_error c -> Fmt.pf ppf "runner_error/%s" (Abi.class_name c)
  | Unexpected_exit code -> Fmt.pf ppf "runner_error/unexpected_exit %d" code

let pp ppf t =
  Fmt.pf ppf "termination=%a result=" pp_termination t.termination;
  match t.record with
  | None -> Fmt.pf ppf "unavailable (no record)"
  | Some Record.Pending_pre_run_error -> Fmt.pf ppf "unavailable (pending_pre_run_error)"
  | Some (Record.Invalid why) -> Fmt.pf ppf "invalid (%s)" why
  | Some (Record.Valid r) -> Fmt.pf ppf "recovered %a" Record.pp r

(* The observed (class, subcode) pair, or [None] when no complete runner-error
   record validated. This is the accessor a conformance case uses, and the
   reason it is not simply "the subcode field" is §15.2: staged bytes are not
   evidence until their commit field changed. *)
let observed_error t =
  match t.record with
  | Some (Record.Valid r) -> (
      match r.Record.runner_error with Some c -> Some (c, r.Record.error_subcode) | None -> None)
  | _ -> None
