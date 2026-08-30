let tool_name (target : Target.t) = (Target.config target).Target.toolprefix ^ "gcc"

let args (target : Target.t) =
  let c = Target.config target in
  match target with
  | Target.Riscv32 -> c.Target.ccomp_args @ [ "-march=rv32imafd"; "-mabi=ilp32d"; "-mno-relax" ]
  | Target.Riscv64 -> c.Target.ccomp_args @ [ "-march=rv64imafd"; "-mabi=lp64d"; "-mno-relax" ]
  | Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64 -> c.Target.ccomp_args

(* Resolution goes through the same PATH search the runner uses -
   Gnu_tools.present's own convention, applied to gcc instead of as/ld. *)
let present prog =
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_null ~stderr:Tool_process.Err_null
         ~accepted:Process_status.Any_exit ~label:"probe" prog [ "--version" ])
  with
  | Ok _ -> true
  | Error _ -> false

let installed target = present (tool_name target)

let require_all targets =
  let missing = List.filter (fun t -> not (installed t)) targets in
  if missing = [] then Ok ()
  else
    Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
      (Tool_error.v Tool_error.Spawn
         (Printf.sprintf "no installed gcc for: %s (expected on PATH, e.g. %s)"
            (String.concat " " (List.map Target.to_string missing))
            (String.concat ", " (List.map tool_name missing))))

let first_line s = match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

let version target =
  let prog = tool_name target in
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_to_stdout
         ~accepted:Process_status.Zero_only ~label:"gcc --version" prog [ "--version" ])
  with
  | Error e -> Error e
  | Ok { Tool_process.stdout = Some s; _ } -> Ok (first_line s)
  | Ok _ ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Exec (Printf.sprintf "%s --version produced no output" prog))
