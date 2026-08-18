type event = Output of Diagnostic.t | Fatal of Tool_error.t Err.Error.t
type exit_class = [ `Success | `Failure | `Usage ]
type t = { events : event list; exit : exit_class }

let exit_code = function `Success -> 0 | `Failure -> 1 | `Usage -> 2
let outputs ds = List.map (fun d -> Output d) ds
let ok ds = { events = outputs ds; exit = `Success }
let fail ds = { events = outputs ds; exit = `Failure }

(* The exit class comes from the payload's own op, so a Usage error exits 2
   without the caller having to remember to say so. *)
let class_of_error e =
  match Tool_error.exit_code (Err.Error.kind e) with 2 -> `Usage | _ -> `Failure

let of_error e = { events = [ Fatal e ]; exit = class_of_error e }
let of_result r ~ok = match r with Ok v -> ok v | Error e -> of_error e

(* One Fatal per failure, in input order. Each keeps its own Error.t, so each
   keeps its own detection origin - which is the whole reason fold_errors is not
   used here. *)
let of_errors errs =
  let exit =
    List.fold_left
      (fun acc e ->
        match (acc, class_of_error e) with `Usage, _ | _, `Usage -> `Usage | _ -> `Failure)
      `Failure errs
  in
  { events = List.map (fun e -> Fatal e) errs; exit }

let of_accum_result r ~ok = match r with Ok v -> ok v | Error e -> of_errors (Err.Error.kind e)

let worse a b =
  match (a, b) with
  | `Usage, _ | _, `Usage -> `Usage
  | `Failure, _ | _, `Failure -> `Failure
  | `Success, `Success -> `Success

let accumulate xs ~f =
  List.fold_left
    (fun acc x ->
      let c = f x in
      { events = acc.events @ c.events; exit = worse acc.exit c.exit })
    { events = []; exit = `Success } xs

(* Rendering is owned here and nowhere else. A finding is Output and prints
   verbatim; only Fatal gets the "FATAL:" prefix, and only from the payload -
   never from Err.Error.pp, which would add a source location to bytes the
   equivalence gates compare. *)
let render ~err_trace t =
  (* Each event is flushed as it is written, so the INTERLEAVING of the two
     streams matches the shell's - every echo there is its own write syscall. A
     buffered stdout would emit the whole run's output after the first fatal,
     which is a visibly different transcript even though each stream's own bytes
     are identical. *)
  List.iter
    (fun ev ->
      match ev with
      | Output d ->
          let oc =
            match d.Diagnostic.stream with
            | Diagnostic.Stdout -> stdout
            | Diagnostic.Stderr -> stderr
          in
          output_string oc d.Diagnostic.text;
          output_char oc '\n';
          flush oc
      | Fatal e ->
          flush stdout;
          if err_trace then Format.eprintf "%a@." (Err.Error.pp Tool_error.pp) e
          else prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e)))
    t.events;
  flush stdout;
  flush stderr;
  exit_code t.exit

let fail_after outputs e =
  { events = List.map (fun d -> Output d) outputs @ [ Fatal e ]; exit = `Failure }
