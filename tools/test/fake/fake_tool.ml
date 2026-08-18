(* A program that does exactly what FAKE_TOOL_SPEC says, and records how it was
   called.

   The log is written with NUL separators because the whole point is to observe
   argv faithfully: a space- or newline-separated log could not distinguish one
   argument containing a space from two arguments. *)

let spec = try Sys.getenv "FAKE_TOOL_SPEC" with Not_found -> "exit:0"

let log_argv () =
  match Sys.getenv_opt "FAKE_TOOL_LOG" with
  | None -> ()
  | Some path ->
      let oc = open_out_gen [ Open_append; Open_creat; Open_binary ] 0o600 path in
      Array.iter (fun a -> output_string oc (a ^ "\000")) Sys.argv;
      output_string oc ("cwd=" ^ Sys.getcwd () ^ "\000");
      output_string oc ("LC_ALL=" ^ Option.value ~default:"" (Sys.getenv_opt "LC_ALL") ^ "\000");
      output_string oc "\n";
      close_out oc

let field name =
  let prefix = name ^ ":" in
  let n = String.length prefix in
  if String.length spec >= n && String.sub spec 0 n = prefix then
    Some (String.sub spec n (String.length spec - n))
  else None

let () =
  log_argv ();
  match spec with
  (* argv[0] is dropped: the interesting content is what the caller passed. *)
  | "argv" ->
      Array.iteri (fun i a -> if i > 0 then print_endline a) Sys.argv;
      exit 0
  | "cwd" ->
      print_endline (Sys.getcwd ());
      exit 0
  | _ -> (
      match field "env" with
      | Some name ->
          print_endline (Option.value ~default:"" (Sys.getenv_opt name));
          exit 0
      | None -> (
          match field "exit" with
          | Some code -> exit (int_of_string code)
          | None -> (
              match field "out" with
              | Some s ->
                  print_string s;
                  exit 0
              | None -> (
                  match field "err" with
                  | Some s ->
                      prerr_string s;
                      exit 0
                  | None -> (
                      match field "both" with
                      | Some s ->
                          print_string ("out:" ^ s);
                          prerr_string ("err:" ^ s);
                          exit 0
                      | None -> (
                          match field "fail_with" with
                          | Some s ->
                              prerr_endline s;
                              exit 1
                          | None -> (
                              (* Enough output to wedge any pipe-based parent
                                 that waits before draining. *)
                              match field "big" with
                              | Some n ->
                                  print_string (String.make (int_of_string n) 'x');
                                  exit 0
                              | None -> (
                                  match field "signal" with
                                  (* Named, not numbered: Unix.kill takes
                                     OCaml's INTERNAL signal numbering, in which
                                     Sys.sigterm is -11, so a literal 15 here
                                     would not be SIGTERM. *)
                                  | Some "term" ->
                                      Unix.kill (Unix.getpid ()) Sys.sigterm;
                                      exit 0
                                  | Some "kill" ->
                                      Unix.kill (Unix.getpid ()) Sys.sigkill;
                                      exit 0
                                  | Some other ->
                                      prerr_endline ("fake_tool: unknown signal " ^ other);
                                      exit 2
                                  | None -> (
                                      match field "sleep" with
                                      | Some s ->
                                          Unix.sleepf (float_of_string s);
                                          exit 0
                                      | None ->
                                          prerr_endline ("fake_tool: unknown spec " ^ spec);
                                          exit 2)))))))))
