type case = {
  case_id : string;
  target : Target.t;
  form_id : string;
  source_record_ids : string list;
  rule_ids : string list;
  operands : (string * string) list;
  rendered_source : string;
  configuration : string list;
  negative : bool;
}

type artifact = {
  tool_label : string;
  argv : string list;
  exit_status : Process_status.t;
  stdout : string;
  stderr : string;
  bytes : string option;
  relocations : string list;
}

type verdict =
  | Pass
  | Byte_mismatch
  | Regression
  | Frontier_gap
  | Gas_rejected_valid_case
  | Oracle_unavailable of { probe : string }
  | Blocked_unknown_requirement of { rule : string }
  | Negative_case_accepted

let verdict_description = function
  | Pass -> "both accept and bytes agree: pass, crediting only the observed form/obligations"
  | Byte_mismatch -> "both accept but bytes differ: hard failure, including for a frontier case"
  | Regression ->
      "our assembler rejects a case already promoted to our support contract: regression"
  | Frontier_gap ->
      "our assembler rejects an explicitly unimplemented form: tracked frontier gap, no positive \
       credit"
  | Gas_rejected_valid_case ->
      "GAS rejects a supposedly valid case: investigate the recipe, constraints, flags, or tool \
       capability - never blanket-skip"
  | Oracle_unavailable { probe } ->
      Printf.sprintf
        "the form requires a known-unavailable GAS capability (probe: %s): explicit \
         oracle-unavailable"
        probe
  | Blocked_unknown_requirement { rule } ->
      Printf.sprintf "an unknown normalization constraint (%s) blocks positive generation" rule
  | Negative_case_accepted ->
      "a negative case was accepted unexpectedly: failure per that case's own assertion"

type observation = { case : case; gas : artifact option; ours : artifact option; verdict : verdict }
type tier = Offline_consumer | Gnu_regeneration | Producer_update

let fixture_dir_name = "isa-generated"
let cli_group_name = "isa-generated"

let make_target = function
  | Offline_consumer -> Some "asm-isa-generated-check"
  | Gnu_regeneration -> Some "asm-isa-generated-regen"
  | Producer_update -> None

let joins_prerequisite_of = function
  | Offline_consumer -> Some "asm-test"
  | Gnu_regeneration -> None
  | Producer_update -> None
