type outcome = Assembled of { bytes_hex : string } | Rejected of string

let ( let* ) = Result.bind

(* The fixed basename this function always uses inside its own private scratch
   directory - mirrors Isa_gen_oracle.normalized_argv exactly, and for the
   same reason: removing temporary paths and timestamps from committed
   identities. *)
let normalized_argv (case : Isa_generated_case.case) =
  [ "--target"; Target.to_string case.target; "--fixed-base"; "0x0"; "--dump-bytes"; "case.s" ]

let first_line s = match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s

(* Mirrors Gnu_tools.ml's own private [replace_all] (used there for exactly
   the same reason: [gas_outcome_of_result] scrubs the scratch [src] path out
   of GAS's diagnostic before it becomes a committed [Rejected] finding). Not
   shared - that function is not exposed by gnu_tools.mli, and this is short
   enough that duplicating it beats widening that module's public surface for
   one caller. *)
let replace_all ~sub ~by s =
  let sub_len = String.length sub in
  if sub_len = 0 then s
  else
    let buf = Buffer.create (String.length s) in
    let i = ref 0 in
    while !i <= String.length s - sub_len do
      if String.sub s !i sub_len = sub then (
        Buffer.add_string buf by;
        i := !i + sub_len)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.add_string buf (String.sub s !i (String.length s - !i));
    Buffer.contents buf

let git repo args ~label =
  Tool_process.exec
    (Tool_process.spec ~cwd:(Repo.path repo) ~stdout:Tool_process.Out_capture
       ~stderr:Tool_process.Err_capture ~accepted:Process_status.Zero_only ~label "git" args)

(* "ours" has no fixed release version the way RV32 2.43.1 / RV64 2.44 does -
   it is whatever this checkout's asm/ tree currently is, which is the entire
   point of comparing against it. The git revision is the label; "-dirty" is
   appended when asm/ itself has uncommitted changes, so a corpus regenerated
   against work-in-progress code is visibly distinguishable from one against a
   clean commit ([tool_label] must be resolved, never assumed from a prior
   measurement, extended to the one tool with no release to pin). *)
let tool_label repo =
  let* head = git repo [ "rev-parse"; "HEAD" ] ~label:"isa-generated-ours git rev-parse" in
  let* status =
    git repo [ "status"; "--porcelain"; "--"; "asm" ] ~label:"isa-generated-ours git status"
  in
  let rev = String.trim (Option.value head.Tool_process.stdout ~default:"") in
  let dirty = String.trim (Option.value status.Tool_process.stdout ~default:"") <> "" in
  Ok (Printf.sprintf "ours-%s%s" rev (if dirty then "-dirty" else ""))

let run repo (case : Isa_generated_case.case) =
  let* tool_label = tool_label repo in
  Tool_workspace.with_scratch ~label:"isa-generated-ours" (fun work ->
      let src = Fpath.(work / "case.s") in
      let* () = Tool_fs.write src case.rendered_source in
      let argv =
        [
          "--target";
          Target.to_string case.target;
          "--fixed-base";
          "0x0";
          "--dump-bytes";
          Fpath.to_string src;
        ]
      in
      (* Never in-process (Isa_gen_ours.mli): tool/asm.exe is a separately
         built executable, exactly the boundary gas_xref_cmd.ml's
         emit_generated already relies on. `dune exec` propagates the child's
         own exit code unchanged (verified: 0 on success, 1 on a rejected
         case's `exit 1`, 2 on a `die` usage error) - accepting exactly {0; 1}
         means any other status (2, a signal, a timeout) fails this call
         rather than being misread as a case rejection. *)
      let* result =
        Tool_process.exec
          (Tool_process.spec
             ~cwd:Fpath.(Repo.path repo / "asm")
             ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture
             ~accepted:(Process_status.Statuses [ 0; 1 ])
             ~label:"isa-generated-ours asm.exe" "opam"
             ([ "exec"; "--"; "dune"; "exec"; "tool/asm.exe"; "--" ] @ argv))
      in
      (* The scratch case.s path is machine-local and nondeterministic
         (Tool_workspace.with_scratch mints a fresh directory every call) -
         scrubbed out of the diagnostic before it reaches either the returned
         [outcome] or the committed [artifact], exactly like
         Gnu_tools.gas_outcome_of_result does for GAS's own diagnostics (plan
         §5.4: "removing temporary paths ... from committed identities"). *)
      let stderr_scrubbed =
        replace_all ~sub:(Fpath.to_string src) ~by:"case.s"
          (Option.value ~default:"" result.Tool_process.stderr)
      in
      let artifact_of bytes =
        Isa_generated_case.
          {
            tool_label;
            argv = normalized_argv case;
            exit_status = result.Tool_process.status;
            stdout = Option.value ~default:"" result.Tool_process.stdout;
            stderr = stderr_scrubbed;
            bytes;
            relocations = [];
          }
      in
      match result.Tool_process.status with
      | Process_status.Exited 0 ->
          let hex = Option.value ~default:"" result.Tool_process.stdout in
          Ok (Assembled { bytes_hex = hex }, artifact_of (Some hex))
      | _ ->
          let diag = first_line (String.trim stderr_scrubbed) in
          Ok (Rejected diag, artifact_of None))
