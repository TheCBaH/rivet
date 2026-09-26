type klass = Canonical | Alias | Pseudo | Negative

let klass_to_string = function
  | Canonical -> "canonical"
  | Alias -> "alias"
  | Pseudo -> "pseudo"
  | Negative -> "negative"

let klass_of (case : Isa_generated_case.case) =
  if case.negative then Negative
  else if List.mem "alias-spelling" case.rule_ids then Alias
  else if List.mem "pseudo-expansion" case.rule_ids then Pseudo
  else Canonical

type status = Satisfied | Unsatisfied of { reason : string; task : string }
type obligation = { target : Target.t; klass : klass; status : status }

let riscv = [ Target.Riscv32; Target.Riscv64 ]
let x86 = [ Target.X86_32; Target.X86_64 ]
let each targets klass status = List.map (fun target -> { target; klass; status }) targets

let obligations =
  each riscv Alias Satisfied
  @ each riscv Pseudo
      (Unsatisfied
         {
           reason =
             "multi-instruction pseudo-ops (li, la, call, tail) expand to sequences with \
              relocations; the linked-image comparison exists but is not wired to generated cases";
           task = "GEN-06-PSEUDO";
         })
  @ each riscv Negative Satisfied
  @ each x86 Alias
      (Unsatisfied
         {
           reason =
             "XED has no alias records; GAS's own alternative spellings (for example sal for shl) \
              need a reviewed list before a case can be sourced";
           task = "GEN-06-X86-ALIAS";
         })
  @ each x86 Pseudo
      (Unsatisfied
         {
           reason =
             "XED has no pseudo-op concept; the nearest analogue, suffix-less spellings, is the \
              recorded frontier gap of the pilot x86 forms";
           task = "GEN-06-X86-ALIAS";
         })
  @ each x86 Negative Satisfied

let count cases target klass =
  List.length
    (List.filter
       (fun (c : Isa_generated_case.case) -> c.target = target && klass_of c = klass)
       cases)

let problems cases =
  List.filter_map
    (fun o ->
      let n = count cases o.target o.klass in
      let what = Printf.sprintf "%s/%s" (Target.to_string o.target) (klass_to_string o.klass) in
      match o.status with
      | Satisfied when n = 0 ->
          Some (Printf.sprintf "%s is declared satisfied but the corpus has no such case" what)
      | Unsatisfied { task; _ } when n > 0 ->
          Some
            (Printf.sprintf "%s is declared unsatisfied (%s) but the corpus has %d such case(s)"
               what task n)
      | _ -> None)
    obligations

let targets = riscv @ x86
let klasses = [ Canonical; Alias; Pseudo; Negative ]

let report_lines cases =
  let policy =
    "isa-coverage: policy: enumerated manifest, no sampling (seed: none); budget: one case per \
     (form, target, obligation)"
  in
  let counts =
    List.map
      (fun target ->
        Printf.sprintf "isa-coverage: %s: %s" (Target.to_string target)
          (String.concat " "
             (List.map
                (fun k -> Printf.sprintf "%s=%d" (klass_to_string k) (count cases target k))
                klasses)))
      targets
  in
  let obligation_lines =
    List.map
      (fun o ->
        let what = Printf.sprintf "%s/%s" (Target.to_string o.target) (klass_to_string o.klass) in
        match o.status with
        | Satisfied ->
            Printf.sprintf "isa-obligation: %s: satisfied (%d cases)" what
              (count cases o.target o.klass)
        | Unsatisfied { reason; task } ->
            Printf.sprintf "isa-obligation: %s: UNSATISFIED task=%s: %s" what task reason)
      obligations
  in
  (policy :: counts) @ obligation_lines

let run repo =
  let ( let* ) = Result.bind in
  let load path =
    if Sys.file_exists (Fpath.to_string path) then
      Result.map
        (List.map (fun (r : Isa_generated_corpus.record) -> r.case))
        (Isa_generated_corpus.load path)
    else Ok []
  in
  match
    let* pilot = load Fpath.(Repo.isa_generated_corpus repo / "cases.jsonl") in
    let* difficult = load Fpath.(Repo.isa_difficult_corpus repo / "cases.jsonl") in
    Ok (pilot @ difficult)
  with
  | Error e -> Command.of_error e
  | Ok cases -> (
      let report = List.map Diagnostic.stdout (report_lines cases) in
      match problems cases with
      | [] -> Command.ok report
      | ps ->
          Command.accumulate
            [
              Command.ok report;
              Command.of_error
                (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                   (Tool_error.v Tool_error.Validate ("isa-coverage: " ^ String.concat "; " ps)));
            ]
            ~f:Fun.id)
