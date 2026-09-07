(* case.rendered_source is the full multi-line GAS source (".text\n<line>\n");
   flatten it to one line for a single-line diagnostic. *)
let flatten_source src = String.trim (String.map (function '\n' -> ' ' | c -> c) src)

type result = { command : Command.t; record : Isa_generated_corpus.record option }

let verdict_tag : Isa_generated_case.verdict -> string = function
  | Pass -> "PASS"
  | Byte_mismatch -> "BYTE-MISMATCH"
  | Regression -> "REGRESSION"
  | Frontier_gap -> "FRONTIER-GAP"
  | Gas_rejected_valid_case -> "GAS-REJECTED"
  | Oracle_unavailable _ -> "ORACLE-UNAVAILABLE"
  | Blocked_unknown_requirement _ -> "BLOCKED"
  | Negative_case_accepted -> "NEGATIVE-CASE-ACCEPTED"

(* The "ours" half: only ever invoked when GAS itself accepted the case -
   there is nothing to compare against otherwise (Isa_generated_corpus.mli's
   own [ours = None iff gas.bytes = None] invariant). Returns the report
   suffix and the record fields together, since both are derived from the
   same Isa_gen_ours.run/Isa_gen_verdict.classify call. *)
let ours_and_verdict repo (case : Isa_generated_case.case) ~gas_hex =
  let ( let* ) = Result.bind in
  let* ours_outcome, ours_artifact = Isa_gen_ours.run repo case in
  let ours_result =
    match ours_outcome with
    | Isa_gen_ours.Assembled { bytes_hex } -> Isa_gen_verdict.Ours_assembled bytes_hex
    | Isa_gen_ours.Rejected diagnostic -> Isa_gen_verdict.Ours_rejected diagnostic
  in
  let known_gap =
    match ours_result with
    | Isa_gen_verdict.Ours_rejected diagnostic ->
        Isa_gen_verdict.known_syntax_gap ~case_id:case.case_id ~diagnostic
    | Isa_gen_verdict.Ours_assembled _ -> false
  in
  let verdict =
    Isa_gen_verdict.classify ~known_syntax_gap:known_gap
      (Isa_gen_verdict.Both_ran { gas_hex; ours = ours_result })
  in
  let suffix =
    match ours_result with
    | Isa_gen_verdict.Ours_assembled hex ->
        Printf.sprintf "ours=%s(bytes=%s)" (verdict_tag verdict) (String.trim hex)
    | Isa_gen_verdict.Ours_rejected diagnostic ->
        Printf.sprintf "ours=%s(%s)" (verdict_tag verdict) diagnostic
  in
  Ok (verdict, Some ours_artifact, suffix)

let run_case ~prefix ~label repo (case : Isa_generated_case.case)
    (encoding : Isa_norm_model.encoding) =
  match Isa_gen_oracle.run case encoding with
  | Error e -> { command = Command.of_error e; record = None }
  | Ok (outcome, artifact) -> (
      let finding = Isa_generated_corpus.finding_of_outcome outcome in
      let gas_line =
        match outcome with
        | Isa_gen_oracle.Assembled_matching { bytes_hex } ->
            Printf.sprintf "PASS %s bytes=%s"
              (flatten_source case.rendered_source)
              (String.trim bytes_hex)
        | Isa_gen_oracle.Assembled_mismatched { bytes_hex; detail } ->
            Printf.sprintf "DIFFERENT-FORM %s bytes=%s (%s)"
              (flatten_source case.rendered_source)
              (String.trim bytes_hex) detail
        | Isa_gen_oracle.Unexpected_relocation { bytes_hex; relocations } ->
            Printf.sprintf "UNEXPECTED-RELOCATION %s bytes=%s (%s)"
              (flatten_source case.rendered_source)
              (String.trim bytes_hex) (String.concat " | " relocations)
        | Isa_gen_oracle.Rejected body -> Printf.sprintf "GAS-REJECTED %s" body
      in
      let line suffix = Printf.sprintf "%s: %s: %s%s" prefix label gas_line suffix in
      match outcome with
      | Isa_gen_oracle.Rejected _ ->
          let verdict =
            Isa_gen_verdict.classify ~known_syntax_gap:false
              (Isa_gen_verdict.Gas_only_rejected artifact.stdout)
          in
          {
            command = Command.ok [ Diagnostic.stdout (line "") ];
            record =
              Some Isa_generated_corpus.{ case; gas = artifact; ours = None; finding; verdict };
          }
      | Isa_gen_oracle.Unexpected_relocation { relocations; _ } ->
          (* Plan §5.4: never compare unresolved object placeholders - "ours"
             is not invoked at all here, matching Rejected's own shape
             (Isa_gen_verdict.Gas_unexpected_relocation). *)
          let verdict =
            Isa_gen_verdict.classify ~known_syntax_gap:false
              (Isa_gen_verdict.Gas_unexpected_relocation relocations)
          in
          {
            command = Command.ok [ Diagnostic.stdout (line "") ];
            record =
              Some Isa_generated_corpus.{ case; gas = artifact; ours = None; finding; verdict };
          }
      | Isa_gen_oracle.Assembled_matching { bytes_hex }
      | Isa_gen_oracle.Assembled_mismatched { bytes_hex; detail = _ } -> (
          match ours_and_verdict repo case ~gas_hex:bytes_hex with
          | Error e -> { command = Command.of_error e; record = None }
          | Ok (verdict, ours, suffix) ->
              {
                command = Command.ok [ Diagnostic.stdout (line (" " ^ suffix)) ];
                record = Some Isa_generated_corpus.{ case; gas = artifact; ours; finding; verdict };
              }))
