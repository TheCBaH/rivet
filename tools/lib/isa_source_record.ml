type field = { field_name : string; lsb : int; width : int }

type x86_operand = {
  op_name : string;
  op_type : string;
  lookupfn_name : string option;
  oc2 : string option;
  bits : string option;
  rw : string;
  visibility : string;
}

type encoding =
  | Fixed_bits of { width_bits : int; mask : string; value : string; fields : field list }
  | X86_encoding of {
      space : string;
      opcode_map : int;
      opcode : string;
      pattern : string;
      operands : x86_operand list;
    }
  | Opaque of { note : string }
  | Unknown_encoding of string

type applicability =
  | App_all of applicability list
  | App_any of applicability list
  | App_mode of { mode : string; equals : bool }
  | App_unknown of string

type relationship_status = Exact | Ambiguous | Missing | Unknown_status of string
type relationship = { rel_kind : string; status : relationship_status; candidates : string list }

type riscv_provenance = {
  extension : string option;
  raw_tokens : string list;
  resolved_mask : string option;
  resolved_match : string option;
  variable_fields : string list;
  relationships : relationship list;
}

type xed_provenance = {
  category : string option;
  extension : string option;
  iform : string option;
  isa_set : string option;
  mode_restriction : string option;
}

type provenance = Riscv_provenance of riscv_provenance | Xed_provenance of xed_provenance | Other
type origin = { path : string; line : int option }

type t = {
  record_id : string;
  source : string;
  kind : string;
  native_name : string;
  snapshot : string;
  origin : origin;
  encoding : encoding;
  provenance : provenance;
  applicability : applicability;
  unresolved : string list;
}

(* --- generic-JSON projection helpers, since [encoding]/[provenance]/[origin]
   shape depends on [encoding.kind]/[source] rather than being fixed once for
   the whole schema (see this module's .mli). *)

let mems_of : Jsont.json -> Jsont.object' = function Jsont.Object (mems, _) -> mems | _ -> []
let find name json = Option.map snd (Jsont.Json.find_mem name (mems_of json))

let find_str name json =
  match find name json with Some (Jsont.String (s, _)) -> Some s | _ -> None

let find_int name json =
  match find name json with Some (Jsont.Number (n, _)) -> Some (int_of_float n) | _ -> None

let find_list name json = match find name json with Some (Jsont.Array (l, _)) -> l | _ -> []
let as_str = function Jsont.String (s, _) -> Some s | _ -> None
let str_or_empty o = Option.value o ~default:""
let origin_of json = { path = str_or_empty (find_str "path" json); line = find_int "line" json }

let field_of json =
  match (find_str "name" json, find_int "lsb" json, find_int "width" json) with
  | Some field_name, Some lsb, Some width -> Some { field_name; lsb; width }
  | _ -> None

let x86_operand_of json =
  match (find_str "name" json, find_str "type" json, find_str "rw" json) with
  | Some op_name, Some op_type, Some rw ->
      Some
        {
          op_name;
          op_type;
          lookupfn_name = find_str "lookupfn_name" json;
          oc2 = find_str "oc2" json;
          bits = find_str "bits" json;
          rw;
          visibility = str_or_empty (find_str "visibility" json);
        }
  | _ -> None

let encoding_of json =
  match find_str "kind" json with
  | Some "fixed_bits" ->
      Fixed_bits
        {
          width_bits = Option.value (find_int "width_bits" json) ~default:0;
          mask = str_or_empty (find_str "mask" json);
          value = str_or_empty (find_str "value" json);
          fields = List.filter_map field_of (find_list "fields" json);
        }
  | Some "x86_encoding" ->
      X86_encoding
        {
          space = str_or_empty (find_str "space" json);
          opcode_map = Option.value (find_int "opcode_map" json) ~default:0;
          opcode = str_or_empty (find_str "opcode" json);
          pattern = str_or_empty (find_str "pattern" json);
          operands = List.filter_map x86_operand_of (find_list "operands" json);
        }
  | Some "opaque" -> Opaque { note = str_or_empty (find_str "note" json) }
  | Some other -> Unknown_encoding other
  | None -> Unknown_encoding "<missing encoding.kind>"

let rec applicability_of json =
  match find_str "kind" json with
  | Some "all" -> App_all (List.map applicability_of (find_list "of" json))
  | Some "any" -> App_any (List.map applicability_of (find_list "of" json))
  | Some "mode" -> (
      match (find_str "equals" json, find_str "not_equals" json) with
      | Some mode, _ -> App_mode { mode; equals = true }
      | None, Some mode -> App_mode { mode; equals = false }
      | None, None -> App_unknown "applicability.kind=mode has neither equals nor not_equals")
  | Some other -> App_unknown (Printf.sprintf "unrecognized applicability.kind: %s" other)
  | None -> App_unknown "<missing applicability.kind>"

let relationship_status_of = function
  | "exact" -> Exact
  | "ambiguous" -> Ambiguous
  | "missing" -> Missing
  | other -> Unknown_status other

let relationship_of json =
  match (find_str "kind" json, find_str "status" json) with
  | Some rel_kind, Some status ->
      Some
        {
          rel_kind;
          status = relationship_status_of status;
          candidates = List.filter_map as_str (find_list "candidates" json);
        }
  | _ -> None

let riscv_provenance_of json =
  let raw = find "raw" json in
  let resolved = find "upstream-resolved" json in
  {
    extension = find_str "extension" json;
    raw_tokens =
      (match raw with Some raw -> List.filter_map as_str (find_list "tokens" raw) | None -> []);
    resolved_mask = (match resolved with Some r -> find_str "mask" r | None -> None);
    resolved_match = (match resolved with Some r -> find_str "match" r | None -> None);
    variable_fields =
      (match resolved with
      | Some r -> List.filter_map as_str (find_list "variable_fields" r)
      | None -> []);
    relationships = List.filter_map relationship_of (find_list "relationship-resolution" json);
  }

let xed_provenance_of json =
  {
    category = find_str "category" json;
    extension = find_str "extension" json;
    iform = find_str "iform" json;
    isa_set = find_str "isa_set" json;
    mode_restriction = find_str "mode_restriction" json;
  }

let provenance_of ~source json =
  match source with
  | "riscv_opcodes" -> Riscv_provenance (riscv_provenance_of json)
  | "xed" -> Xed_provenance (xed_provenance_of json)
  | _ -> Other

(* --- the fixed top-level shape every record has, per schema §required. *)

type raw = {
  r_record_id : string;
  r_source : string;
  r_kind : string;
  r_native_name : string;
  r_snapshot : string;
  r_origin : Jsont.json;
  r_encoding : Jsont.json;
  r_provenance : Jsont.json;
  r_applicability : Jsont.json;
  r_unresolved : string list;
}

let empty_json = Jsont.Json.object' []

let raw_jsont : raw Jsont.t =
  Jsont.Object.map
    (fun
      r_record_id
      r_source
      r_kind
      r_native_name
      r_snapshot
      r_origin
      r_encoding
      r_provenance
      r_applicability
      r_unresolved
    ->
      {
        r_record_id;
        r_source;
        r_kind;
        r_native_name;
        r_snapshot;
        r_origin;
        r_encoding;
        r_provenance;
        r_applicability;
        r_unresolved;
      })
  |> Jsont.Object.mem "record_id" Jsont.string
  |> Jsont.Object.mem "source" Jsont.string
  |> Jsont.Object.mem "kind" Jsont.string
  |> Jsont.Object.mem "native_name" Jsont.string
  |> Jsont.Object.mem "snapshot" Jsont.string
  |> Jsont.Object.mem "origin" Jsont.json ~dec_absent:empty_json
  |> Jsont.Object.mem "encoding" Jsont.json ~dec_absent:empty_json
  |> Jsont.Object.mem "provenance" Jsont.json ~dec_absent:empty_json
  |> Jsont.Object.mem "applicability" Jsont.json ~dec_absent:empty_json
  |> Jsont.Object.mem "unresolved" (Jsont.list Jsont.string) ~dec_absent:[]
  |> Jsont.Object.finish

let of_raw (r : raw) : t =
  {
    record_id = r.r_record_id;
    source = r.r_source;
    kind = r.r_kind;
    native_name = r.r_native_name;
    snapshot = r.r_snapshot;
    origin = origin_of r.r_origin;
    encoding = encoding_of r.r_encoding;
    provenance = provenance_of ~source:r.r_source r.r_provenance;
    applicability = applicability_of r.r_applicability;
    unresolved = r.r_unresolved;
  }

let of_line line = Result.map of_raw (Jsont_bytesrw.decode_string raw_jsont line)

let fail ?path detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path Tool_error.Parse detail)

(* export/writer.py always ends the file with a newline after its last line,
   same as {!Isa_db_jsonl}, so splitting on '\n' leaves one trailing empty
   element to drop, not a record to decode. *)
let lines_of_text text =
  match String.split_on_char '\n' text with
  | parts -> ( match List.rev parts with "" :: rest -> List.rev rest | _ -> parts)

let read_file path =
  let ( let* ) = Result.bind in
  let* text = Tool_fs.read path in
  let rec go acc lineno = function
    | [] -> Ok (List.rev acc)
    | line :: rest -> (
        match of_line line with
        | Ok r -> go (r :: acc) (lineno + 1) rest
        | Error msg -> fail ~path (Printf.sprintf "line %d: %s" lineno msg))
  in
  go [] 1 (lines_of_text text)
