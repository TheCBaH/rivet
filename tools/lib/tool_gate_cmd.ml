let ( let* ) = Result.bind

type mode = All | User | System | Gdb

(* {1 The transcript}

   Column widths and wording are the shell's, and they are load-bearing rather
   than cosmetic: the gate's output IS its artifact, read by whoever is asking
   what a base-image update moved. tools/dev/compare-tool-gate.sh holds the two
   implementations to the same bytes. *)

let note label detail = Diagnostic.stdout (Printf.sprintf "  %-34s %s" label detail)
let failure label detail = Diagnostic.stdout (Printf.sprintf "  FAIL %-29s %s" label detail)

(* One run's accumulated state. Notes and failures are ONE ordered stream - they
   interleave in the shell's output and a split would reorder them - while
   provenance is a second stream, written to a file and dumped at the end. *)
type run = {
  mutable events : Diagnostic.t list; (* reversed *)
  mutable provenance : string list; (* reversed; each already "key\tvalue" *)
  mutable failures : int;
  work : Fpath.t;
}

let emit r d = r.events <- d :: r.events
let say r label detail = emit r (note label detail)

let bad r label detail =
  r.failures <- r.failures + 1;
  emit r (failure label detail)

let record r key value = r.provenance <- Printf.sprintf "%s\t%s" key value :: r.provenance

(* {2 Programs with no target prefix}

   qemu-system and gdb are not per-target binutils, so they do not belong to
   Gnu_tools. They are named literally, exactly as the shell names them. *)

let present prog =
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_null ~stderr:Tool_process.Err_null
         ~accepted:Process_status.Any_exit ~label:"probe" prog [ "--version" ])
  with
  | Ok _ -> true
  | Error _ -> false

let first_line s = match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* `"$bin" --version | head -1`. The status is deliberately unchecked, matching
   the shell: this is provenance, and a tool that answers oddly should be
   RECORDED oddly rather than turned into a gate failure of its own. *)
let version_of prog =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_null
         ~parsed_output:true ~accepted:Process_status.Any_exit ~label:(prog ^ " --version") prog
         [ "--version" ])
  in
  Ok (first_line (Option.value ~default:"" r.Tool_process.stdout))

(* {3 Check 1 - the qemu-user reference snippet}

   Assembled, linked, identified and executed, per target. Every step is a
   SEPARATE recorded failure with its own message, and every one of them
   `continue`s to the next target rather than ending the run: the gate exists to
   report which tools moved, and stopping at the first would hide the rest. *)

let user_gate r =
  let step target =
    let c = Target.config target in
    let t = Target.to_string target in
    let tools = Gnu_tools.for_target target in
    let dir = Fpath.(r.work / t) in
    let* () = Tool_fs.mkdir_p dir in
    let src = Fpath.(dir / "ref.s") in
    let* () = Tool_fs.write src (Gate_snippet.source target) in
    if not (Gnu_tools.installed tools `As) then (
      bad r t (Printf.sprintf "no %sas" c.Target.toolprefix);
      Ok ())
    else
      let* assembled =
        Gnu_tools.gate_assemble tools ~src ~obj:Fpath.(dir / "ref.o") ~log:Fpath.(dir / "as.log")
      in
      match assembled with
      | Gnu_tools.Step_failed msg ->
          bad r (t ^ " assemble") msg;
          Ok ()
      | Gnu_tools.Step_ok -> (
          let exe = Fpath.(dir / "ref") in
          let* linked =
            Gnu_tools.gate_link tools ~entry:"_start"
              ~obj:Fpath.(dir / "ref.o")
              ~out:exe
              ~log:Fpath.(dir / "ld.log")
          in
          match linked with
          | Gnu_tools.Step_failed msg ->
              bad r (t ^ " link") msg;
              Ok ()
          | Gnu_tools.Step_ok ->
              let* as_v = Gnu_tools.version_line tools `As in
              let* ld_v = Gnu_tools.version_line tools `Ld in
              record r (t ^ ".as") as_v;
              record r (t ^ ".ld") ld_v;
              let* class_, machine = Gnu_tools.elf_identity tools exe in
              if class_ <> c.Target.elf_class || machine <> c.Target.readelf_machine then (
                bad r (t ^ " ELF")
                  (Printf.sprintf "%s/%s, expected %s/%s" class_ machine c.Target.elf_class
                     c.Target.readelf_machine);
                Ok ())
              else if not (Gnu_tools.installed tools `Qemu) then (
                bad r t (Printf.sprintf "no %s" c.Target.qemu_bin);
                Ok ())
              else
                let* qemu_v = Gnu_tools.qemu_version tools in
                record r (t ^ ".qemu") qemu_v;
                let want = Gate_snippet.expected_status target in
                let* status, _out, _errout = Gnu_tools.qemu_run tools ~exe ~timeout_s:10 in
                let code =
                  match status with
                  | Process_status.Exited n -> n
                  | Process_status.Signaled n -> 128 + n
                  | Process_status.Timed_out -> 124
                in
                if code = 124 then (
                  bad r (t ^ " run") "timed out after 10s";
                  Ok ())
                else if code <> want then (
                  bad r (t ^ " run") (Printf.sprintf "exit status %d, expected %d" code want);
                  Ok ())
                else (
                  say r t
                    (Printf.sprintf "assembled, linked, exit %d under %s" code c.Target.qemu_bin);
                  Ok ()))
  in
  emit r (Diagnostic.stdout "== qemu-user: reference snippet, direct exit_group, no ABI v1 ==");
  List.fold_left
    (fun acc target ->
      let* () = acc in
      step target)
    (Ok ()) Target.all

(* {4 Check 3 - qemu-system machine aliases}

   The ALIAS is asserted rather than the versioned name, deliberately: the alias
   is what a command line would name, and it is the alias that disappears when a
   machine is retired. qemu-system-x86_64 appears twice, so its version is
   recorded twice - the shell does the same, and provenance is a log rather than
   a set. *)

let system_machines =
  [
    ("qemu-system-x86_64", "q35");
    ("qemu-system-x86_64", "pc");
    ("qemu-system-arm", "virt");
    ("qemu-system-aarch64", "virt");
  ]

(* `awk '$1 == m'` - the first whitespace-separated field, on every matching
   line. awk splits on runs of spaces AND tabs and ignores leading ones, so this
   does too. *)
let first_field line =
  let is_ws c = c = ' ' || c = '\t' in
  let n = String.length line in
  let rec start i = if i < n && is_ws line.[i] then start (i + 1) else i in
  let b = start 0 in
  let rec stop i = if i < n && not (is_ws line.[i]) then stop (i + 1) else i in
  String.sub line b (stop b - b)

(* `sed 's/  */ /g'`: runs of SPACES collapse to one. Tabs are untouched, which
   is what the sed does too. *)
let squeeze_spaces s =
  let b = Buffer.create (String.length s) in
  let prev_space = ref false in
  String.iter
    (fun c ->
      if c = ' ' then (
        if not !prev_space then Buffer.add_char b c;
        prev_space := true)
      else (
        Buffer.add_char b c;
        prev_space := false))
    s;
  Buffer.contents b

let split_lines text =
  let l = String.split_on_char '\n' text in
  match List.rev l with "" :: rest -> List.rev rest | _ -> l

let system_gate r =
  emit r (Diagnostic.stdout "== qemu-system: machine aliases present ==");
  List.fold_left
    (fun acc (bin, machine) ->
      let* () = acc in
      if not (present bin) then (
        bad r bin "not installed";
        Ok ())
      else
        let* v = version_of bin in
        record r bin v;
        (* stderr to /dev/null, as the shell has it: some builds print a warning
           here and it is not part of the machine list. *)
        let* out =
          Tool_process.exec
            (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_null
               ~parsed_output:true ~accepted:Process_status.Any_exit ~label:(bin ^ " -M help") bin
               [ "-M"; "help" ])
          |> Result.map (fun (r : Tool_process.result) ->
              Option.value ~default:"" r.Tool_process.stdout)
        in
        let matching = List.filter (fun l -> first_field l = machine) (split_lines out) in
        if matching = [] then (
          bad r (Printf.sprintf "%s -M %s" bin machine) "alias absent from -M help";
          Ok ())
        else (
          say r
            (Printf.sprintf "%s -M %s" bin machine)
            (squeeze_spaces (String.concat "\n" matching));
          Ok ()))
    (Ok ()) system_machines

(* {5 Check 4 - the GDB/RSP handshake}

   The one place the parent must GUARANTEE cleanup, and the reason Supervisor
   exists. The shell ends gdb_gate with `kill "$qpid" || true`, which under
   errexit is never reached when anything above it exits non-zero - so a failing
   gate leaks a qemu holding the TCP port, and the next run fails to bind with no
   indication why. with_process terminates on every exit path including an
   exception.

   gdb-multiarch, not plain gdb: Debian's gdb is built --target=x86_64-linux-gnu
   only, so it can never complete an RSP handshake with qemu-system-aarch64 -
   every attempt would fail, indistinguishably from a socket that never
   accepted. *)

let gdb_port ~env = match env "ASM_TOOL_GATE_PORT" with Some s when s <> "" -> s | _ -> "14730"

let gdb_batch ~port ~log extra =
  let args =
    [ "--batch"; "-ex"; "set confirm off"; "-ex"; Printf.sprintf "target remote :%s" port ]
    @ extra @ [ "-ex"; "detach" ]
  in
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_to_stdout
         ~parsed_output:true ~accepted:Process_status.Any_exit ~label:"gdb-multiarch"
         "gdb-multiarch" args)
  in
  let text = Option.value ~default:"" r.Tool_process.stdout in
  let* () = Tool_fs.write log text in
  Ok (r.Tool_process.status = Process_status.Exited 0, text)

(* `tr '\n' ' '`, the same transcription Gnu_tools makes for the as/ld logs -
   every newline including the final one, so a one-line message ends in a space.
   Spelled out again rather than shared across the module boundary: it is one
   String.map, and it belongs beside the shell line it reproduces. *)
let flatten s = String.map (function '\n' -> ' ' | c -> c) s

let last_bytes n s =
  let len = String.length s in
  if len <= n then s else String.sub s (len - n) n

let gdb_gate r ~env =
  emit r (Diagnostic.stdout "== gdb: batch/RSP handshake against a paused qemu-system ==");
  if not (present "gdb-multiarch") then (
    bad r "gdb-multiarch" "not installed";
    Ok ())
  else
    let* v = version_of "gdb-multiarch" in
    record r "gdb-multiarch" v;
    let port = gdb_port ~env in
    let log = Fpath.(r.work / "gdb.log") in
    Supervisor.with_process
      ~stdout:Fpath.(r.work / "qemu-system.log")
      ~label:"qemu-system-aarch64" "qemu-system-aarch64"
      [
        "-M";
        "virt";
        "-cpu";
        "cortex-a57";
        "-m";
        "128";
        "-nographic";
        "-S";
        "-gdb";
        "tcp::" ^ port;
        "-monitor";
        "none";
        "-serial";
        "none";
      ]
      (fun q ->
        (* The probe is a real connection attempt, not a fixed sleep: a fixed
           sleep is either flaky or slow, and on a loaded CI runner it is both.
           await additionally ends the wait if qemu EXITED, which is the case the
           shell's 50-attempt loop reports as a connection timeout. *)
        let ready () = match gdb_batch ~port ~log [] with Ok (true, _) -> true | _ -> false in
        let* connected = Supervisor.await q ~ready ~timeout:10.0 ~poll:0.2 in
        if not connected then (
          (* A divergence from the shell, on a failure path it cannot reach: a
             qemu that died during startup is reported as having died, not as a
             socket that never accepted. *)
          let* exited = Supervisor.exit_status q in
          (match exited with
          | Some st ->
              bad r "gdb handshake"
                (Printf.sprintf "qemu-system-aarch64 exited (%s) before accepting on :%s - see %s"
                   (Format.asprintf "%a" Process_status.pp st)
                   port
                   (Fpath.to_string Fpath.(r.work / "qemu-system.log")))
          | None -> bad r "gdb handshake" (Printf.sprintf "no connection to :%s after 10s" port));
          Ok ())
        else
          (* Connected once; now do the REAL handshake and read state back, so
             the check is "the two speak RSP" and not merely "a socket
             accepted". *)
          let* ok, text = gdb_batch ~port ~log [ "-ex"; "info registers pc" ] in
          let read_pc =
            ok
            && List.exists
                 (fun l -> String.length l >= 2 && String.sub l 0 2 = "pc")
                 (split_lines text)
          in
          if read_pc then (
            say r "gdb <-> qemu-system-aarch64" "connected, read pc, detached";
            Ok ())
          else (
            bad r "gdb handshake"
              (Printf.sprintf "connected but could not read pc: %s" (last_bytes 200 (flatten text)));
            Ok ()))

(* {6 Provenance}

   Written whatever the outcome, so a base-image update that moved a tool under
   us is visible in the artifact even when the gate failed for another reason. *)

let write_provenance r path =
  Tool_fs.write path (String.concat "" (List.rev_map (fun l -> l ^ "\n") r.provenance))

let run repo ~mode ~env =
  let step =
    let* root = Tool_workspace.tool_gate_work repo ~env in
    let work = Tool_workspace.recreatable_path root in
    let* () = Tool_workspace.recreate_root root in
    let r = { events = []; provenance = []; failures = 0; work } in
    let provenance = Fpath.(work / "provenance.txt") in
    let* () = write_provenance r provenance in
    let* () =
      match mode with
      | All ->
          let* () = user_gate r in
          let* () = system_gate r in
          gdb_gate r ~env
      | User -> user_gate r
      | System -> system_gate r
      | Gdb -> gdb_gate r ~env
    in
    let* () = write_provenance r provenance in
    emit r (Diagnostic.stdout (Printf.sprintf "== provenance (%s) ==" (Fpath.to_string provenance)));
    List.iter (fun l -> emit r (Diagnostic.stdout ("  " ^ l))) (List.rev r.provenance);
    if r.failures = 0 then emit r (Diagnostic.stdout "tool-gate: ok")
    else emit r (Diagnostic.stdout (Printf.sprintf "tool-gate: %d failure(s)" r.failures));
    Ok (List.rev r.events, r.failures)
  in
  match step with
  | Error e -> Command.of_error e
  | Ok (events, 0) -> Command.ok events
  | Ok (events, _) -> Command.fail events

let usage_error mode =
  {
    Command.events =
      [
        Command.Output
          (Diagnostic.stderr
             (Printf.sprintf
                "compcert-tools tool-gate: unknown mode '%s': expected all, user, system or gdb"
                mode));
      ];
    exit = `Usage;
  }

let mode_of_string = function
  | "all" -> Some All
  | "user" -> Some User
  | "system" -> Some System
  | "gdb" -> Some Gdb
  | _ -> None
