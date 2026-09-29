let tool_name (target : Target.t) = (Target.config target).Target.toolprefix ^ "gcc"

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

let compile_s target ~cwd ~out_rel ~source_rel ~case =
  let args = (Target.config target).Target.gcc_fixture_args in
  match
    Tool_process.exec
      (Tool_process.spec ~cwd ~stderr:Tool_process.Err_capture ~accepted:Process_status.Zero_only
         ~label:"gcc" (tool_name target)
         (("-S" :: args) @ [ "-o"; out_rel; source_rel ]))
  with
  | Ok _ -> Ok ()
  | Error e ->
      Err.map_error ~pos:__POS__ ~pp_error:Tool_error.pp
        (fun payload ->
          {
            payload with
            Tool_error.detail =
              Printf.sprintf "fixture compilation failed for %s/%s" case (Target.to_string target);
          })
        (Error e)

let compiler =
  {
    Compiler.name = "gcc";
    generator = "rivet-tools fixture regen";
    installed;
    require_all;
    version;
    provenance =
      (fun target ->
        let args = (Target.config target).Target.gcc_fixture_args in
        [ (Manifest.Compiler_args ("gcc", target), Some (String.concat " " args)) ]);
    compile_s;
  }
