let ( let* ) = Result.bind

type t = { prefix : string; as_args : string list; ld_args : string list; qemu : string }

let for_target target =
  let c = Target.config target in
  {
    prefix = c.Target.toolprefix;
    as_args = c.Target.as_args;
    ld_args = c.Target.ld_args;
    qemu = c.Target.qemu_bin;
  }

let tool t name = t.prefix ^ name

let run ?cwd ?(accepted = Process_status.Zero_only) ?(stdout = Tool_process.Out_null) ~label prog
    args =
  Result.map ignore
    (Tool_process.exec
       (Tool_process.spec ?cwd ~stdout ~stderr:Tool_process.Err_inherit ~accepted ~label prog args))

let capture ?cwd ~label prog args =
  let* r =
    Tool_process.exec
      (Tool_process.spec ?cwd ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_inherit
         ~parsed_output:true ~accepted:Process_status.Zero_only ~label prog args)
  in
  Ok (Option.value ~default:"" r.Tool_process.stdout)

(* Resolution goes through the same PATH search the runner uses, so "installed"
   here means exactly "executable by a child" and not "a file exists". *)
let present prog =
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_null ~stderr:Tool_process.Err_null
         ~accepted:Process_status.Any_exit ~label:"probe" prog [ "--version" ])
  with
  | Ok _ -> true
  | Error _ -> false

let require t ~qemu =
  let needed =
    List.map (tool t) [ "as"; "ld"; "readelf"; "objcopy"; "objdump" ]
    @ if qemu then [ t.qemu ] else []
  in
  match List.find_opt (fun p -> not (present p)) needed with
  | None -> Ok ()
  | Some missing ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Spawn
           (Printf.sprintf "%s is required - the oracle needs the cross binutils" missing))

let installed t which =
  present (match which with `As -> tool t "as" | `Ld -> tool t "ld" | `Qemu -> t.qemu)

let assemble t ~cwd ~src ~obj =
  run ~cwd ~label:"as" (tool t "as") (t.as_args @ [ "-o"; Fpath.to_string obj; Fpath.to_string src ])

let link t ~cwd ~script ~out ~objs =
  run ~cwd ~label:"ld" (tool t "ld")
    (t.ld_args @ [ "-static"; "-T"; Fpath.to_string script; "-o"; Fpath.to_string out ] @ objs)

(* Absent-or-empty is "not present", matching the shell's
   `objcopy … 2>/dev/null && [ -s … ]`. A nonzero status is therefore an
   ordinary answer here, not a failure - hence Any_exit and a bool. *)
let objcopy_section t ~src ~section ~out =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_null ~stderr:Tool_process.Err_null
         ~accepted:Process_status.Any_exit ~label:"objcopy" (tool t "objcopy")
         [ "--only-section=" ^ section; "-O"; "binary"; Fpath.to_string src; Fpath.to_string out ])
  in
  match r.Tool_process.status with
  | Process_status.Exited 0 -> (
      match Unix.stat (Fpath.to_string out) with
      | st -> Ok (st.Unix.st_size > 0)
      | exception Unix.Unix_error _ -> Ok false)
  | _ -> Ok false

let readelf_headers t p =
  capture ~label:"readelf -SW" (tool t "readelf") [ "-SW"; Fpath.to_string p ]

let objdump_headers t p = capture ~label:"objdump -h" (tool t "objdump") [ "-h"; Fpath.to_string p ]
let objdump_relocs t p = capture ~label:"objdump -r" (tool t "objdump") [ "-r"; Fpath.to_string p ]

let readelf_symbols t p =
  capture ~label:"readelf -sW" (tool t "readelf") [ "-sW"; Fpath.to_string p ]

(* {1 Commit normalization}

   These three transforms are what keep the artifacts free of this checkout's
   location and of the object's scratch name. They are transcribed from the
   shell's sed/awk pipeline rather than reinvented:

     sed -e "s|$work/||g" -e 's/^File: .*$//' -e 's/[[:space:]]*$//'
     awk 'NF || p { print } { p = NF }'

   The awk is a blank-line SQUEEZE, not a blank-line drop: it prints a line if
   it is non-empty OR the previous line was non-empty, so runs collapse to one
   and a single blank line survives. *)

let replace_all ~sub ~by s =
  if sub = "" then s
  else
    let b = Buffer.create (String.length s) in
    let n = String.length s and m = String.length sub in
    let i = ref 0 in
    while !i <= n - m do
      if String.sub s !i m = sub then (
        Buffer.add_string b by;
        i := !i + m)
      else (
        Buffer.add_char b s.[!i];
        incr i)
    done;
    Buffer.add_string b (String.sub s !i (n - !i));
    Buffer.contents b

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let split_lines text =
  let l = String.split_on_char '\n' text in
  match List.rev l with "" :: rest -> List.rev rest | _ -> l

let squeeze_blank lines =
  let out, _ =
    List.fold_left
      (fun (acc, prev_nonempty) l ->
        let nonempty = l <> "" in
        if nonempty || prev_nonempty then (l :: acc, nonempty) else (acc, nonempty))
      ([], false) lines
  in
  List.rev out

let render lines = String.concat "" (List.map (fun l -> l ^ "\n") lines)

let readelf_oracle t p ~scrub =
  let* out = capture ~label:"readelf -SWsWr" (tool t "readelf") [ "-SWsWr"; Fpath.to_string p ] in
  let scrub_s = Fpath.to_string scrub ^ "/" in
  let lines =
    split_lines out
    |> List.map (fun l ->
        let l = replace_all ~sub:scrub_s ~by:"" l in
        let l = if String.length l >= 6 && String.sub l 0 6 = "File: " then "" else l in
        rstrip l)
  in
  Ok (render (squeeze_blank lines))

let objdump_disasm t p ~scrub ~drop_banner ~riscv_numeric =
  let flags =
    (* Only when the CALLER asks. With it, RISC-V is disassembled without
       pseudo-instruction aliases and with numeric register names, so the
       artifact records what was encoded rather than one of several spellings
       of it - which is what the fixture oracle wants and what the gas
       cross-reference deliberately does not. *)
    match t.prefix with
    | pre when riscv_numeric && String.length pre >= 7 && String.sub pre 0 7 = "riscv64" ->
        [ "-M"; "no-aliases,numeric" ]
    | _ -> []
  in
  let* out =
    capture ~label:"objdump -dr" (tool t "objdump") (flags @ [ "-dr"; Fpath.to_string p ])
  in
  let scrub_s = Fpath.to_string scrub ^ "/" in
  let lines = split_lines out |> List.map (replace_all ~sub:scrub_s ~by:"") in
  let lines = if drop_banner then match lines with _ :: _ :: rest -> rest | l -> l else lines in
  Ok (render lines)

let version_line t which =
  let name =
    match which with
    | `As -> "as"
    | `Ld -> "ld"
    | `Objdump -> "objdump"
    | `Readelf -> "readelf"
    | `Objcopy -> "objcopy"
  in
  let* out = capture ~label:(name ^ " --version") (tool t name) [ "--version" ] in
  match split_lines out with
  | first :: _ -> Ok first
  | [] ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse (Printf.sprintf "%s --version printed nothing" name))

(* External `timeout`, deliberately retained: its 124-127 contract is what the
   expected-status range stops at 123 to preserve. Every status is accepted and
   returned for the caller to classify - swallowing 124 here would make a
   timeout indistinguishable from a guest exit. *)
let qemu_run t ~exe ~timeout_s =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture
         ~accepted:Process_status.Any_exit ~label:"qemu" "timeout"
         [ Printf.sprintf "%ds" timeout_s; t.qemu; Fpath.to_string exe ])
  in
  Ok
    ( r.Tool_process.status,
      Option.value ~default:"" r.Tool_process.stdout,
      Option.value ~default:"" r.Tool_process.stderr )

let readelf_exec t p =
  capture ~label:"readelf -hSWsr" (tool t "readelf") [ "-hSWsr"; Fpath.to_string p ]

let elf_identity t p =
  let* out = capture ~label:"readelf -h" (tool t "readelf") [ "-h"; Fpath.to_string p ] in
  (* The shell reads these with `sed -n 's/^ *Class: *//p'`, i.e. everything
     after the label, trailing spaces included - so the same trailing strip
     applies here or the comparison against the declared value would never
     match. *)
  let field label =
    List.find_map
      (fun l ->
        let l' = String.trim l in
        let n = String.length label in
        if String.length l' > n && String.sub l' 0 n = label then
          Some (String.trim (String.sub l' n (String.length l' - n)))
        else None)
      (split_lines out)
  in
  match (field "Class:", field "Machine:") with
  | Some c, Some m -> Ok (c, m)
  | _ ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "readelf -h printed no Class/Machine")

let qemu_version t =
  let* out = capture ~label:"qemu --version" t.qemu [ "--version" ] in
  match split_lines out with
  | first :: _ -> Ok first
  | [] ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "qemu --version printed nothing")

type gas_outcome = Assembled | Rejected of string

let try_assemble t ~src ~obj ~include_dir =
  let inc = match include_dir with Some d -> [ "-I"; Fpath.to_string d ] | None -> [] in
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_to_stdout
         ~parsed_output:true ~accepted:Process_status.Any_exit ~label:"as" (tool t "as")
         (t.as_args @ inc @ [ "-o"; Fpath.to_string obj; Fpath.to_string src ]))
  in
  match r.Tool_process.status with
  | Process_status.Exited 0 -> Ok Assembled
  | _ ->
      let text = Option.value ~default:"" r.Tool_process.stdout in
      (* The message only, with the input's path stripped: the path is the
         case's own identity and repeating it inside the record would make
         every line depend on where this checkout lives. *)
      let body =
        split_lines (replace_all ~sub:(Fpath.to_string src) ~by:"" text)
        |> List.filter (fun l ->
            let has sub =
              let n = String.length l and m = String.length sub in
              let rec go i = i + m <= n && (String.sub l i m = sub || go (i + 1)) in
              go 0
            in
            has "Error" || has "Warning")
        |> List.filteri (fun i _ -> i < 3)
      in
      Ok (Rejected (render body))

(* {1 The tool gate's two invocations}

   Neither can be `assemble`/`link`. Those treat a nonzero status as a run-ending
   error, and the gate's whole shape is the opposite: a target that cannot
   assemble is ONE recorded failure and the remaining five targets still run.

   They are not `try_assemble` either, which trims to the first three
   Error/Warning lines and strips the source path. Here the log is an artifact
   under the work root, kept verbatim for someone diagnosing a base-image change,
   and the printed form is the whole thing with newlines turned into spaces.

   STDERR only is captured, matching the shell's `2>"$dir/as.log"`: anything
   these tools write to stdout goes to the terminal there, so capturing both
   would move bytes out of the transcript. *)

type gate_step = Step_ok | Step_failed of string

(* `tr '\n' ' '`, exactly - including the final newline, so a one-line
   diagnostic renders with a trailing space. That space is in the shell's output
   too, and the transcript comparison is byte-exact. *)
let flatten s = String.map (function '\n' -> ' ' | c -> c) s

let gate_step ~log (r : Tool_process.result) =
  let text = Option.value ~default:"" r.Tool_process.stderr in
  let* () = Tool_fs.write log text in
  match r.Tool_process.status with
  | Process_status.Exited 0 -> Ok Step_ok
  | _ -> Ok (Step_failed (flatten text))

let gate_assemble t ~src ~obj ~log =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_inherit ~stderr:Tool_process.Err_capture
         ~parsed_output:true ~accepted:Process_status.Any_exit ~label:"as" (tool t "as")
         (t.as_args @ [ "-o"; Fpath.to_string obj; Fpath.to_string src ]))
  in
  gate_step ~log r

(* No linker script and an explicit entry symbol: this is a freestanding image
   with no libc, which is a different link from the oracle's placed one. Stating
   it as its own call is also what makes "assembling worked but linking did not"
   a distinguishable outcome rather than a side effect of one step. *)
let gate_link t ~entry ~obj ~out ~log =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_inherit ~stderr:Tool_process.Err_capture
         ~parsed_output:true ~accepted:Process_status.Any_exit ~label:"ld" (tool t "ld")
         (t.ld_args @ [ "-static"; "-e"; entry; "-o"; Fpath.to_string out; Fpath.to_string obj ]))
  in
  gate_step ~log r

let preprocess t ~src ~out ~defines ~include_dir =
  let* r =
    Tool_process.exec
      (Tool_process.spec ~stdout:(Tool_process.Out_file out) ~stderr:Tool_process.Err_null
         ~accepted:Process_status.Any_exit ~label:"cpp" (tool t "gcc")
         ([ "-E"; "-P" ] @ defines @ [ "-I"; Fpath.to_string include_dir; Fpath.to_string src ]))
  in
  Ok (r.Tool_process.status = Process_status.Exited 0)
