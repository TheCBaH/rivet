module SMap = Map.Make (String)

type state =
  | Normalized_only
  | Gas_generatable
  | Promoted_support
  | Oracle_unavailable of string
  | Blocked of string

type tally = {
  normalized_only : int;
  gas_generatable : int;
  promoted_support : int;
  oracle_unavailable : int;
  blocked : (string * int) list;
}

type family = { name : string; total : int; tally : tally }
type construct_count = { construct : string; needed : int; sole : int }

type summary = {
  total : int;
  families : family list;
  unruled : int;
  known_only : int;
  constructs : construct_count list;
}

type mutable_tally = {
  mutable normalized_only : int;
  mutable gas_generatable : int;
  mutable promoted_support : int;
  mutable oracle_unavailable : int;
  mutable blocked : int SMap.t;
}

let empty () =
  {
    normalized_only = 0;
    gas_generatable = 0;
    promoted_support = 0;
    oracle_unavailable = 0;
    blocked = SMap.empty;
  }

let bump map key = SMap.update key (function None -> Some 1 | Some n -> Some (n + 1)) map

let record tally = function
  | Normalized_only -> tally.normalized_only <- tally.normalized_only + 1
  | Gas_generatable -> tally.gas_generatable <- tally.gas_generatable + 1
  | Promoted_support -> tally.promoted_support <- tally.promoted_support + 1
  | Oracle_unavailable _ -> tally.oracle_unavailable <- tally.oracle_unavailable + 1
  | Blocked rule -> tally.blocked <- bump tally.blocked rule

let tally_of (t : mutable_tally) : tally =
  {
    normalized_only = t.normalized_only;
    gas_generatable = t.gas_generatable;
    promoted_support = t.promoted_support;
    oracle_unavailable = t.oracle_unavailable;
    blocked = SMap.bindings t.blocked;
  }

let tally_total (t : tally) =
  t.normalized_only + t.gas_generatable + t.promoted_support + t.oracle_unavailable
  + List.fold_left (fun n (_, count) -> n + count) 0 t.blocked

let family_of (rec_ : Isa_source_record.t) =
  match rec_.provenance with
  | Isa_source_record.Riscv_provenance { extension = Some extension; _ } -> extension
  | Isa_source_record.Xed_provenance { isa_set = Some isa_set; _ } -> isa_set
  | Isa_source_record.Riscv_provenance _ -> "<missing-riscv-extension>"
  | Isa_source_record.Xed_provenance _ -> "<missing-xed-isa-set>"
  | Isa_source_record.Other -> "<unexpected-source-provenance>"

let normalize source rec_ =
  match source with
  | "riscv_opcodes" -> Isa_norm_riscv.normalize rec_
  | "xed_resolved" -> Isa_norm_xed.normalize rec_
  | other -> Error { Isa_norm_model.rule = "unhandled-source"; message = other }

let lookup_key source (rec_ : Isa_source_record.t) =
  match (source, rec_.provenance) with
  | "riscv_opcodes", _ -> rec_.native_name
  | "xed_resolved", Isa_source_record.Xed_provenance { iform = Some iform; _ } -> iform
  | _ -> ""

(* Support credit is read from the committed differential corpora, never
   restated by hand: a record is promoted exactly when some creditable
   positive case (GAS and this assembler both accepted it and emitted the
   same bytes) names it, and GAS-generatable when positive cases name it but
   none is creditable. A case names a record through its [source_record_ids]
   and its [form_id]; both must match what the record normalizes to today, so
   a neighbouring form that encodes the same operation gains no credit.

   Only the observed form is credited. The frozen pilot records forms GAS
   does not pick for their canonical spelling (ADD_GPRv_GPRv_03 assembles as
   the 01 form), so a pilot case credits only when its own observed-form check
   matched. Difficult-corpus entries are reviewed to name the form GAS emits;
   their Different_observed_form findings come from that check's
   single-instruction, leading-byte assumption (mandatory prefixes, a branch
   plus its label filler), so a byte-identical [Pass] credits there. *)
type credit = {
  promoted : (string * string, unit) Hashtbl.t;
  attempted : (string * string, unit) Hashtbl.t;
}

let corpus_cases repo =
  let ( let* ) = Result.bind in
  let load path =
    if Sys.file_exists (Fpath.to_string path) then Isa_generated_corpus.load path else Ok []
  in
  let* pilot = load Fpath.(Repo.isa_generated_corpus repo / "cases.jsonl") in
  let* difficult = load Fpath.(Repo.isa_difficult_corpus repo / "cases.jsonl") in
  let creditable ~pilot (r : Isa_generated_corpus.record) =
    r.verdict = Isa_generated_case.Pass
    && ((not pilot) || r.finding = Isa_generated_corpus.Matches_normalized_encoding)
  in
  Ok
    (List.map (fun r -> (r, creditable ~pilot:true r)) pilot
    @ List.map (fun r -> (r, creditable ~pilot:false r)) difficult)

let credit_of ~source ~target (records : Isa_source_record.t list)
    (cases : (Isa_generated_corpus.record * bool) list) =
  let key_of_id = Hashtbl.create (List.length records) in
  List.iter
    (fun (rec_ : Isa_source_record.t) ->
      Hashtbl.replace key_of_id rec_.record_id (lookup_key source rec_))
    records;
  let credit = { promoted = Hashtbl.create 1024; attempted = Hashtbl.create 1024 } in
  List.iter
    (fun ((r : Isa_generated_corpus.record), creditable) ->
      if r.case.target = target && not r.case.negative then
        List.iter
          (fun id ->
            match Hashtbl.find_opt key_of_id id with
            | None -> ()
            | Some key ->
                let k = (r.case.form_id, key) in
                Hashtbl.replace credit.attempted k ();
                if creditable then Hashtbl.replace credit.promoted k ())
          r.case.source_record_ids)
    cases;
  credit

let state_of ~source ~target credit ~known (rec_ : Isa_source_record.t) normalized =
  let extension = family_of rec_ in
  let unavailable () =
    match
      Isa_oracle_unavailable.find_record ~source target ~extension ~native_name:rec_.native_name
    with
    | Some _ as u -> u
    | None -> Isa_oracle_unavailable.find ~source target ~extension
  in
  match normalized with
  | _ when target = Target.X86_32 && Isa_x86_table.not_in_32bit_mode rec_ ->
      Oracle_unavailable "not-encodable-in-32-bit-mode"
  | Ok (form : Isa_norm_model.form)
    when Hashtbl.mem credit.promoted (form.form_id, lookup_key source rec_) ->
      Promoted_support
  | _ -> (
      match unavailable () with
      | Some u -> Oracle_unavailable u.reason
      | None -> (
          match normalized with
          | Error (diagnostic : Isa_norm_model.diagnostic) ->
              if Isa_construct.is_catch_all diagnostic.rule then
                Blocked (Isa_construct.blocker known rec_)
              else Blocked diagnostic.rule
          | Ok (form : Isa_norm_model.form) ->
              if Hashtbl.mem credit.attempted (form.form_id, lookup_key source rec_) then
                Gas_generatable
              else Normalized_only))

(* Per missing construct over the catch-all-blocked records: how many need it,
   and how many need nothing else (admitting it alone would unblock them). *)
let construct_histogram known unruled =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun rec_ ->
      let missing = Isa_construct.missing known rec_ in
      let sole = match missing with [ _ ] -> 1 | _ -> 0 in
      List.iter
        (fun c ->
          let key = Isa_construct.to_string c in
          let needed, only = Option.value (Hashtbl.find_opt tbl key) ~default:(0, 0) in
          Hashtbl.replace tbl key (needed + 1, only + sole))
        missing)
    unruled;
  Hashtbl.to_seq tbl |> List.of_seq
  |> List.map (fun (c, (needed, sole)) -> { construct = c; needed; sole })
  |> List.sort (fun a b ->
      match compare (b.sole, b.needed) (a.sole, a.needed) with
      | 0 -> String.compare a.construct b.construct
      | n -> n)

let targets_and_sources =
  [
    ("riscv_opcodes", Target.Riscv32);
    ("riscv_opcodes", Target.Riscv64);
    ("xed_resolved", Target.X86_32);
    ("xed_resolved", Target.X86_64);
  ]

type classified = {
  record : Isa_source_record.t;
  family : string;
  state : state;
  form_id : string option;
}

let classify repo ~source target =
  let ( let* ) = Result.bind in
  let* records = Isa_source_record.read_file (Repo.isa_db_export repo ~source target) in
  let* cases = corpus_cases repo in
  let credit = credit_of ~source ~target records cases in
  let normalized = List.map (fun rec_ -> (rec_, normalize source rec_)) records in
  let known = Isa_construct.known_of (List.map (fun (r, n) -> (r, Result.is_ok n)) normalized) in
  let classified =
    List.map
      (fun ((rec_ : Isa_source_record.t), n) ->
        {
          record = rec_;
          family = family_of rec_;
          state = state_of ~source ~target credit ~known rec_ n;
          form_id =
            (match n with Ok (form : Isa_norm_model.form) -> Some form.form_id | Error _ -> None);
        })
      normalized
  in
  (* Rule-less records still blocked: an oracle-unavailable spelling is not. *)
  let unruled =
    List.filter_map
      (fun (c, (_, n)) ->
        match (c.state, n) with
        | Blocked _, Error (d : Isa_norm_model.diagnostic) when Isa_construct.is_catch_all d.rule ->
            Some c.record
        | _ -> None)
      (List.combine classified normalized)
  in
  (* an x86 table twin with no credit is blocked on the pseudo-prefix that would reach it *)
  let classified =
    if source <> "xed_resolved" then classified
    else
      let specs = List.filter_map (fun (rec_, _) -> Isa_x86_table.spec_of_record rec_) normalized in
      let secondary = Isa_x86_table.twins specs in
      List.map
        (fun c ->
          match c.state with
          (* even with credit: a twin shares its primary's iform key, but GNU as never emits it *)
          | (Normalized_only | Gas_generatable | Promoted_support)
            when Hashtbl.mem secondary c.record.Isa_source_record.record_id ->
              { c with state = Blocked "needs-pseudo-prefix" }
          | _ -> c)
        classified
  in
  Ok (classified, known, unruled)

let summarize repo ~source target =
  let ( let* ) = Result.bind in
  let* classified, known, unruled = classify repo ~source target in
  let tallies = Hashtbl.create 64 in
  List.iter
    (fun c ->
      let tally =
        match Hashtbl.find_opt tallies c.family with
        | Some tally -> tally
        | None ->
            let tally = empty () in
            Hashtbl.add tallies c.family tally;
            tally
      in
      record tally c.state)
    classified;
  let families =
    Hashtbl.to_seq tallies |> List.of_seq
    |> List.map (fun (name, tally) ->
        { name; total = tally_total (tally_of tally); tally = tally_of tally })
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  Ok
    {
      total = List.length classified;
      families;
      unruled = List.length unruled;
      known_only = List.length (List.filter (fun r -> Isa_construct.missing known r = []) unruled);
      constructs = construct_histogram known unruled;
    }

let state_label = function
  | Normalized_only -> "normalized-only"
  | Gas_generatable -> "gas-generatable"
  | Promoted_support -> "promoted-support"
  | Oracle_unavailable reason -> "oracle-unavailable:" ^ reason
  | Blocked rule -> "blocked:" ^ rule

let record_lines repo =
  Command.accumulate targets_and_sources ~f:(fun (source, target) ->
      match classify repo ~source target with
      | Error e -> Command.of_error e
      | Ok (classified, _, _) ->
          let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
          Command.ok
            (List.map
               (fun c ->
                 Diagnostic.stdout
                   (Printf.sprintf "%s %s %s %s %s %s" label c.family (state_label c.state)
                      (lookup_key source c.record)
                      (Option.value c.form_id ~default:"-")
                      c.record.record_id))
               classified))

let report_lines ~label (summary : summary) =
  let header =
    Printf.sprintf "isa-family-admission: %s: %d records, %d families" label summary.total
      (List.length summary.families)
  in
  let line family =
    let t = family.tally in
    let blockers =
      match t.blocked with
      | [] -> "-"
      | items ->
          String.concat ","
            (List.map (fun (rule, count) -> Printf.sprintf "%s=%d" rule count) items)
    in
    Printf.sprintf
      "  %-28s total=%-5d normalized-only=%-4d gas-generatable=%-4d promoted-support=%-4d \
       oracle-unavailable=%-4d blocker=%s"
      family.name family.total t.normalized_only t.gas_generatable t.promoted_support
      t.oracle_unavailable blockers
  in
  let construct_lines =
    Printf.sprintf
      "isa-construct: %s: %d records without a normalizer rule, %d need only known constructs" label
      summary.unruled summary.known_only
    :: List.map
         (fun c -> Printf.sprintf "  %-52s needed=%-5d sole=%d" c.construct c.needed c.sole)
         summary.constructs
  in
  (header :: List.map line summary.families) @ construct_lines

let run repo =
  Command.accumulate targets_and_sources ~f:(fun (source, target) ->
      match summarize repo ~source target with
      | Error e -> Command.of_error e
      | Ok summary ->
          let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
          Command.ok (List.map Diagnostic.stdout (report_lines ~label summary)))
