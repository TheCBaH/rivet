type gas_finding =
  | Matches_normalized_encoding
  | Different_observed_form of { detail : string }
  | Unexpected_relocation of { relocations : string list }
  | Gas_rejected of { diagnostic : string }

let finding_of_outcome : Isa_gen_oracle.outcome -> gas_finding = function
  | Assembled_matching _ -> Matches_normalized_encoding
  | Assembled_mismatched { detail; _ } -> Different_observed_form { detail }
  | Isa_gen_oracle.Unexpected_relocation { relocations; _ } -> Unexpected_relocation { relocations }
  | Rejected body -> Gas_rejected { diagnostic = body }

let finding_description = function
  | Matches_normalized_encoding -> "PASS"
  | Different_observed_form { detail } -> Printf.sprintf "DIFFERENT-FORM (%s)" detail
  | Unexpected_relocation { relocations } ->
      Printf.sprintf "UNEXPECTED-RELOCATION (%s)" (String.concat " | " relocations)
  | Gas_rejected { diagnostic } -> Printf.sprintf "GAS-REJECTED %s" diagnostic

type record = {
  case : Isa_generated_case.case;
  gas : Isa_generated_case.artifact;
  ours : Isa_generated_case.artifact option;
  finding : gas_finding;
  verdict : Isa_generated_case.verdict;
}

let ( let* ) = Result.bind
let err fmt = Printf.ksprintf (fun s -> Error s) fmt

(* Generic JSON tree builders/accessors - the same small, hand-written style
   as Isa_norm_jsonl and Isa_source_record, not shared with either since
   neither exports these as a reusable combinator library. *)

let str s = Jsont.Json.string s
let int_ i = Jsont.Json.int i
let bool_ b = Jsont.Json.bool b
let list_ f xs = Jsont.Json.list (List.map f xs)

let obj fields =
  Jsont.Json.object' (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

let as_string = function Jsont.String (s, _) -> Ok s | _ -> err "expected a JSON string"

let as_int = function
  | Jsont.Number (n, _) -> Ok (int_of_float n)
  | _ -> err "expected a JSON number"

let as_bool = function Jsont.Bool (b, _) -> Ok b | _ -> err "expected a JSON boolean"
let as_list = function Jsont.Array (xs, _) -> Ok xs | _ -> err "expected a JSON array"
let mems_of : Jsont.json -> Jsont.object' = function Jsont.Object (mems, _) -> mems | _ -> []
let find name json = Option.map snd (Jsont.Json.find_mem name (mems_of json))

let require name json =
  match find name json with Some v -> Ok v | None -> err "missing member %S" name

let mem_json name json = require name json

let mem_string name json =
  let* v = require name json in
  as_string v

let mem_int name json =
  let* v = require name json in
  as_int v

let mem_bool name json =
  let* v = require name json in
  as_bool v

let mem_list name json =
  let* v = require name json in
  as_list v

let mem_opt_string name json =
  let* v = require name json in
  match v with Jsont.Null _ -> Ok None | v -> Result.map Option.some (as_string v)

let rec result_map f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = result_map f xs in
      Ok (y :: ys)

let target_to_json t = str (Target.to_string t)

let target_of_json json =
  let* s = as_string json in
  match Target.of_string s with
  | Ok t -> Ok t
  | Error e -> err "unknown target %S: %s" s (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e)

let process_status_to_json : Process_status.t -> Jsont.json = function
  | Exited n -> obj [ ("kind", str "exited"); ("code", int_ n) ]
  | Signaled n -> obj [ ("kind", str "signaled"); ("signal", int_ n) ]
  | Timed_out -> obj [ ("kind", str "timed_out") ]

let process_status_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "exited" ->
      let* code = mem_int "code" json in
      Ok (Process_status.Exited code)
  | "signaled" ->
      let* signal = mem_int "signal" json in
      Ok (Process_status.Signaled signal)
  | "timed_out" -> Ok Process_status.Timed_out
  | other -> err "unknown process_status kind: %s" other

let operand_pair_to_json (name, value) = obj [ ("name", str name); ("value", str value) ]

let operand_pair_of_json json =
  let* name = mem_string "name" json in
  let* value = mem_string "value" json in
  Ok (name, value)

let case_to_json (c : Isa_generated_case.case) =
  obj
    [
      ("case_id", str c.case_id);
      ("target", target_to_json c.target);
      ("form_id", str c.form_id);
      ("source_record_ids", list_ str c.source_record_ids);
      ("rule_ids", list_ str c.rule_ids);
      ("operands", list_ operand_pair_to_json c.operands);
      ("rendered_source", str c.rendered_source);
      ("configuration", list_ str c.configuration);
      ("negative", bool_ c.negative);
    ]

let case_of_json json =
  let* case_id = mem_string "case_id" json in
  let* target_json = mem_json "target" json in
  let* target = target_of_json target_json in
  let* form_id = mem_string "form_id" json in
  let* source_record_ids_json = mem_list "source_record_ids" json in
  let* source_record_ids = result_map as_string source_record_ids_json in
  let* rule_ids_json = mem_list "rule_ids" json in
  let* rule_ids = result_map as_string rule_ids_json in
  let* operands_json = mem_list "operands" json in
  let* operands = result_map operand_pair_of_json operands_json in
  let* rendered_source = mem_string "rendered_source" json in
  let* configuration_json = mem_list "configuration" json in
  let* configuration = result_map as_string configuration_json in
  let* negative = mem_bool "negative" json in
  Ok
    Isa_generated_case.
      {
        case_id;
        target;
        form_id;
        source_record_ids;
        rule_ids;
        operands;
        rendered_source;
        configuration;
        negative;
      }

let artifact_to_json (a : Isa_generated_case.artifact) =
  obj
    [
      ("tool_label", str a.tool_label);
      ("argv", list_ str a.argv);
      ("exit_status", process_status_to_json a.exit_status);
      ("stdout", str a.stdout);
      ("stderr", str a.stderr);
      ("bytes", match a.bytes with Some s -> str s | None -> Jsont.Json.null ());
      ("relocations", list_ str a.relocations);
    ]

let artifact_of_json json =
  let* tool_label = mem_string "tool_label" json in
  let* argv_json = mem_list "argv" json in
  let* argv = result_map as_string argv_json in
  let* exit_status_json = mem_json "exit_status" json in
  let* exit_status = process_status_of_json exit_status_json in
  let* stdout = mem_string "stdout" json in
  let* stderr = mem_string "stderr" json in
  let* bytes = mem_opt_string "bytes" json in
  let* relocations_json = mem_list "relocations" json in
  let* relocations = result_map as_string relocations_json in
  Ok Isa_generated_case.{ tool_label; argv; exit_status; stdout; stderr; bytes; relocations }

let gas_finding_to_json : gas_finding -> Jsont.json = function
  | Matches_normalized_encoding -> obj [ ("kind", str "matches_normalized_encoding") ]
  | Different_observed_form { detail } ->
      obj [ ("kind", str "different_observed_form"); ("detail", str detail) ]
  | Unexpected_relocation { relocations } ->
      obj [ ("kind", str "unexpected_relocation"); ("relocations", list_ str relocations) ]
  | Gas_rejected { diagnostic } ->
      obj [ ("kind", str "gas_rejected"); ("diagnostic", str diagnostic) ]

let verdict_to_json : Isa_generated_case.verdict -> Jsont.json = function
  | Pass -> obj [ ("kind", str "pass") ]
  | Byte_mismatch -> obj [ ("kind", str "byte_mismatch") ]
  | Regression -> obj [ ("kind", str "regression") ]
  | Frontier_gap -> obj [ ("kind", str "frontier_gap") ]
  | Gas_rejected_valid_case -> obj [ ("kind", str "gas_rejected_valid_case") ]
  | Oracle_unavailable { probe } -> obj [ ("kind", str "oracle_unavailable"); ("probe", str probe) ]
  | Blocked_unknown_requirement { rule } ->
      obj [ ("kind", str "blocked_unknown_requirement"); ("rule", str rule) ]
  | Negative_case_accepted -> obj [ ("kind", str "negative_case_accepted") ]

let verdict_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "pass" -> Ok Isa_generated_case.Pass
  | "byte_mismatch" -> Ok Isa_generated_case.Byte_mismatch
  | "regression" -> Ok Isa_generated_case.Regression
  | "frontier_gap" -> Ok Isa_generated_case.Frontier_gap
  | "gas_rejected_valid_case" -> Ok Isa_generated_case.Gas_rejected_valid_case
  | "oracle_unavailable" ->
      let* probe = mem_string "probe" json in
      Ok (Isa_generated_case.Oracle_unavailable { probe })
  | "blocked_unknown_requirement" ->
      let* rule = mem_string "rule" json in
      Ok (Isa_generated_case.Blocked_unknown_requirement { rule })
  | "negative_case_accepted" -> Ok Isa_generated_case.Negative_case_accepted
  | other -> err "unknown verdict kind: %s" other

let mem_opt name ~decode json =
  let* v = require name json in
  match v with Jsont.Null _ -> Ok None | v -> Result.map Option.some (decode v)

let gas_finding_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "matches_normalized_encoding" -> Ok Matches_normalized_encoding
  | "different_observed_form" ->
      let* detail = mem_string "detail" json in
      Ok (Different_observed_form { detail })
  | "unexpected_relocation" ->
      let* relocations_json = mem_list "relocations" json in
      let* relocations = result_map as_string relocations_json in
      Ok (Unexpected_relocation { relocations })
  | "gas_rejected" ->
      let* diagnostic = mem_string "diagnostic" json in
      Ok (Gas_rejected { diagnostic })
  | other -> err "unknown gas_finding kind: %s" other

let schema_version = 2

let to_json (r : record) =
  obj
    [
      ("schema_version", int_ schema_version);
      ("case", case_to_json r.case);
      ("gas", artifact_to_json r.gas);
      ("ours", match r.ours with Some a -> artifact_to_json a | None -> Jsont.Json.null ());
      ("finding", gas_finding_to_json r.finding);
      ("verdict", verdict_to_json r.verdict);
    ]

let of_json json =
  let* schema_version_read = mem_int "schema_version" json in
  let* () =
    if schema_version_read = schema_version then Ok ()
    else
      err "unsupported isa-generated corpus schema_version: %d (expected %d)" schema_version_read
        schema_version
  in
  let* case_json = mem_json "case" json in
  let* case = case_of_json case_json in
  let* gas_json = mem_json "gas" json in
  let* gas = artifact_of_json gas_json in
  let* ours = mem_opt "ours" ~decode:artifact_of_json json in
  let* finding_json = mem_json "finding" json in
  let* finding = gas_finding_of_json finding_json in
  let* verdict_json = mem_json "verdict" json in
  let* verdict = verdict_of_json verdict_json in
  Ok { case; gas; ours; finding; verdict }

let fail detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Parse detail)

let encode_line r =
  match Jsont_bytesrw.encode_string Jsont.json (to_json r) with
  | Ok line -> Err.return line
  | Error msg -> fail msg

let decode_line line =
  match Jsont_bytesrw.decode_string Jsont.json line with
  | Error msg -> fail msg
  | Ok json -> ( match of_json json with Ok r -> Err.return r | Error msg -> fail msg)

let write path records =
  let sorted =
    List.sort (fun (a : record) b -> String.compare a.case.case_id b.case.case_id) records
  in
  let* lines =
    List.fold_left
      (fun acc r ->
        let* acc = acc in
        let* line = encode_line r in
        Ok (line :: acc))
      (Ok []) sorted
  in
  Tool_fs.write path (String.concat "\n" (List.rev lines) ^ "\n")

let load path =
  let* text = Tool_fs.read path in
  let lines = String.split_on_char '\n' text |> List.filter (fun l -> String.trim l <> "") in
  let* records =
    List.fold_left
      (fun acc line ->
        let* acc = acc in
        let* r = decode_line line in
        Ok (r :: acc))
      (Ok []) lines
  in
  let records = List.rev records in
  let ids = List.map (fun (r : record) -> r.case.case_id) records in
  match List.find_opt (fun id -> List.length (List.filter (String.equal id) ids) > 1) ids with
  | Some dup -> fail (Printf.sprintf "duplicate case_id %S in %s" dup (Fpath.to_string path))
  | None -> Ok records

let replay_gas_side (r : record) (encoding : Isa_norm_model.encoding) =
  let expected_argv = Isa_gen_oracle.normalized_argv r.case in
  if r.gas.argv <> expected_argv then
    err "committed gas.argv [%s] does not match the recomputed [%s]" (String.concat " " r.gas.argv)
      (String.concat " " expected_argv)
  else
    match r.gas.bytes with
    | None -> (
        match r.finding with
        | Gas_rejected _ -> Ok ()
        | _ -> err "gas.bytes is None but the committed finding is not Gas_rejected")
    | Some hex -> (
        match Hex_dump.parse hex with
        | Error e ->
            err "cannot decode committed hex bytes: %s"
              (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e)
        | Ok raw -> (
            match r.finding with
            | Unexpected_relocation { relocations } ->
                (* Isa_gen_oracle.run never calls observed_form_check once it
                   has seen a .text relocation - raw is an unresolved
                   placeholder there, not a real encoding, so replay does not
                   call it either - never comparing unresolved
                   object placeholders. All this can check offline is that
                   the committed evidence is actually non-empty. *)
                if relocations = [] then
                  err "finding is Unexpected_relocation but the committed relocations list is empty"
                else Ok ()
            | _ -> (
                match Isa_gen_oracle.observed_form_check encoding raw with
                | Ok () -> (
                    match r.finding with
                    | Matches_normalized_encoding -> Ok ()
                    | _ ->
                        err
                          "observed_form_check now matches the normalized encoding, but the \
                           committed finding says otherwise - run --regen")
                | Error detail -> (
                    match r.finding with
                    | Different_observed_form { detail = recorded }
                      when String.equal detail recorded ->
                        Ok ()
                    | Different_observed_form { detail = recorded } ->
                        err
                          "observed_form_check now reports %S, committed finding recorded %S - run \
                           --regen"
                          detail recorded
                    | _ ->
                        err
                          "observed_form_check now reports a mismatch (%s), but the committed \
                           finding says otherwise - run --regen"
                          detail))))

(* Recompute the {!Isa_gen_verdict.outcome} from committed
   data alone (no tool invocation on either side) and check it reproduces
   [r.verdict] exactly - the same "recompute, never re-invoke" discipline
   [replay_gas_side] already applies to [r.finding]. *)
let check_recomputed_verdict (r : record) (outcome : Isa_gen_verdict.outcome) ~known_syntax_gap =
  let recomputed = Isa_gen_verdict.classify ~known_syntax_gap outcome in
  if recomputed = r.verdict then Ok ()
  else
    err "recomputed verdict %s does not match committed %s - run --regen"
      (Isa_generated_case.verdict_description recomputed)
      (Isa_generated_case.verdict_description r.verdict)

let replay_ours_side (r : record) =
  match r.finding with
  | Unexpected_relocation { relocations } -> (
      match r.ours with
      | Some _ -> err "finding is Unexpected_relocation but a committed ours artifact exists"
      | None ->
          check_recomputed_verdict r (Isa_gen_verdict.Gas_unexpected_relocation relocations)
            ~known_syntax_gap:false)
  | Matches_normalized_encoding | Different_observed_form _ | Gas_rejected _ -> (
      match (r.gas.bytes, r.ours) with
      | None, Some _ -> err "gas.bytes is None (GAS rejected) but a committed ours artifact exists"
      | Some _, None -> err "gas.bytes is Some (GAS accepted) but no committed ours artifact exists"
      | None, None ->
          check_recomputed_verdict r (Isa_gen_verdict.Gas_only_rejected r.gas.stdout)
            ~known_syntax_gap:false
      | Some gas_hex, Some (ours : Isa_generated_case.artifact) ->
          let expected_argv = Isa_gen_ours.normalized_argv r.case in
          if ours.argv <> expected_argv then
            err "committed ours.argv [%s] does not match the recomputed [%s]"
              (String.concat " " ours.argv) (String.concat " " expected_argv)
          else
            let ours_result =
              match ours.bytes with
              | Some hex -> Isa_gen_verdict.Ours_assembled hex
              | None -> Isa_gen_verdict.Ours_rejected ours.stderr
            in
            let known_gap =
              match ours_result with
              | Isa_gen_verdict.Ours_rejected diagnostic ->
                  Isa_gen_verdict.known_syntax_gap ~case_id:r.case.case_id ~diagnostic
              | Isa_gen_verdict.Ours_assembled _ -> false
            in
            check_recomputed_verdict r
              (Isa_gen_verdict.Both_ran { gas_hex; ours = ours_result })
              ~known_syntax_gap:known_gap)

let replay (r : record) (encoding : Isa_norm_model.encoding) =
  let* () = replay_gas_side r encoding in
  replay_ours_side r
