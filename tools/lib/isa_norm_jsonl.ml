let ( let* ) = Result.bind
let err fmt = Printf.ksprintf (fun s -> Error s) fmt

(* Generic JSON tree builders/accessors, same style as Isa_source_record's
   mems_of/find - this module never needs jsont's Object/Case combinators
   because several Isa_norm_model types are recursive sum types, which those
   combinators do not express as directly as a hand-written tagged tree. *)

let str s = Jsont.Json.string s
let int_ i = Jsont.Json.int i
let bool_ b = Jsont.Json.bool b
let list_ f xs = Jsont.Json.list (List.map f xs)

let obj fields =
  Jsont.Json.object' (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

let as_string = function Jsont.String (s, _) -> Ok s | _ -> err "expected a JSON string"
let as_bool = function Jsont.Bool (b, _) -> Ok b | _ -> err "expected a JSON boolean"

let as_int = function
  | Jsont.Number (n, _) -> Ok (int_of_float n)
  | _ -> err "expected a JSON number"

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

let rec result_map f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = result_map f xs in
      Ok (y :: ys)

let arch_to_json : Isa_norm_model.arch -> Jsont.json = function
  | Riscv -> str "riscv"
  | X86 -> str "x86"

let arch_of_json json =
  let* s = as_string json in
  match s with
  | "riscv" -> Ok Isa_norm_model.Riscv
  | "x86" -> Ok Isa_norm_model.X86
  | other -> err "unknown arch: %s" other

let register_class_to_json : Isa_norm_model.register_class -> Jsont.json = function
  | Riscv_gpr -> str "riscv_gpr"
  | Riscv_fpr -> str "riscv_fpr"
  | Riscv_vec -> str "riscv_vec"
  | X86_gpr -> str "x86_gpr"
  | X87_st -> str "x87_st"

let register_class_of_json json =
  let* s = as_string json in
  match s with
  | "riscv_gpr" -> Ok Isa_norm_model.Riscv_gpr
  | "riscv_fpr" -> Ok Isa_norm_model.Riscv_fpr
  | "riscv_vec" -> Ok Isa_norm_model.Riscv_vec
  | "x86_gpr" -> Ok Isa_norm_model.X86_gpr
  | "x87_st" -> Ok Isa_norm_model.X87_st
  | other -> err "unknown register_class: %s" other

let bit_run_to_json (r : Isa_norm_model.bit_run) =
  obj
    [
      ("field_name", str r.field_name);
      ("field_hi", int_ r.field_hi);
      ("field_lo", int_ r.field_lo);
      ("dest_hi", int_ r.dest_hi);
      ("dest_lo", int_ r.dest_lo);
    ]

let bit_run_of_json json =
  let* field_name = mem_string "field_name" json in
  let* field_hi = mem_int "field_hi" json in
  let* field_lo = mem_int "field_lo" json in
  let* dest_hi = mem_int "dest_hi" json in
  let* dest_lo = mem_int "dest_lo" json in
  Ok Isa_norm_model.{ field_name; field_hi; field_lo; dest_hi; dest_lo }

let immediate_to_json (i : Isa_norm_model.immediate) =
  obj
    [
      ("width_bits", int_ i.width_bits);
      ("signed", bool_ i.signed);
      ("implicit_low_zero_bits", int_ i.implicit_low_zero_bits);
      ("nonzero", bool_ i.nonzero);
      ("runs", list_ bit_run_to_json i.runs);
    ]

let immediate_of_json json =
  let* width_bits = mem_int "width_bits" json in
  let* signed = mem_bool "signed" json in
  let* implicit_low_zero_bits = mem_int "implicit_low_zero_bits" json in
  let* nonzero = mem_bool "nonzero" json in
  let* runs_json = mem_list "runs" json in
  let* runs = result_map bit_run_of_json runs_json in
  Ok Isa_norm_model.{ width_bits; signed; implicit_low_zero_bits; nonzero; runs }

let operand_kind_to_json : Isa_norm_model.operand_kind -> Jsont.json = function
  | Register { class_; excluded } ->
      obj
        [
          ("kind", str "register");
          ("class", register_class_to_json class_);
          ("excluded", list_ str excluded);
        ]
  | Immediate imm -> obj [ ("kind", str "immediate"); ("immediate", immediate_to_json imm) ]
  | Memory { width_bits } ->
      obj
        [
          ("kind", str "memory");
          ("width_bits", match width_bits with None -> Jsont.Json.null () | Some n -> int_ n);
        ]
  | Rounding_mode -> obj [ ("kind", str "rounding_mode") ]
  | Implicit_register { class_; native_name } ->
      obj
        [
          ("kind", str "implicit_register");
          ("class", register_class_to_json class_);
          ("native_name", str native_name);
        ]

let operand_kind_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "register" ->
      let* class_json = mem_json "class" json in
      let* class_ = register_class_of_json class_json in
      let* excluded_json = mem_list "excluded" json in
      let* excluded = result_map as_string excluded_json in
      Ok (Isa_norm_model.Register { class_; excluded })
  | "immediate" ->
      let* imm_json = mem_json "immediate" json in
      let* imm = immediate_of_json imm_json in
      Ok (Isa_norm_model.Immediate imm)
  | "memory" ->
      let* width_json = mem_json "width_bits" json in
      let width_bits =
        match width_json with
        | Jsont.Null _ -> Ok None
        | _ -> Result.map Option.some (as_int width_json)
      in
      let* width_bits = width_bits in
      Ok (Isa_norm_model.Memory { width_bits })
  | "rounding_mode" -> Ok Isa_norm_model.Rounding_mode
  | "implicit_register" ->
      let* class_json = mem_json "class" json in
      let* class_ = register_class_of_json class_json in
      let* native_name = mem_string "native_name" json in
      Ok (Isa_norm_model.Implicit_register { class_; native_name })
  | other -> err "unknown operand_kind: %s" other

let role_to_json : Isa_norm_model.role -> Jsont.json = function
  | In -> str "in"
  | Out -> str "out"
  | In_out -> str "in_out"

let role_of_json json =
  let* s = as_string json in
  match s with
  | "in" -> Ok Isa_norm_model.In
  | "out" -> Ok Isa_norm_model.Out
  | "in_out" -> Ok Isa_norm_model.In_out
  | other -> err "unknown role: %s" other

let operand_to_json (o : Isa_norm_model.operand) =
  obj
    [
      ("op_name", str o.op_name);
      ("op_kind", operand_kind_to_json o.op_kind);
      ("role", role_to_json o.role);
      ("explicit", bool_ o.explicit);
    ]

let operand_of_json json =
  let* op_name = mem_string "op_name" json in
  let* op_kind_json = mem_json "op_kind" json in
  let* op_kind = operand_kind_of_json op_kind_json in
  let* role_json = mem_json "role" json in
  let* role = role_of_json role_json in
  let* explicit = mem_bool "explicit" json in
  Ok Isa_norm_model.{ op_name; op_kind; role; explicit }

let encoding_to_json : Isa_norm_model.encoding -> Jsont.json = function
  | Riscv_encoding { width_bits; mask; value } ->
      obj
        [
          ("kind", str "riscv");
          ("width_bits", int_ width_bits);
          ("mask", str mask);
          ("value", str value);
        ]
  | X86_encoding { space; opcode_map; opcode; pattern } ->
      obj
        [
          ("kind", str "x86");
          ("space", str space);
          ("opcode_map", int_ opcode_map);
          ("opcode", str opcode);
          ("pattern", str pattern);
        ]

let encoding_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "riscv" ->
      let* width_bits = mem_int "width_bits" json in
      let* mask = mem_string "mask" json in
      let* value = mem_string "value" json in
      Ok (Isa_norm_model.Riscv_encoding { width_bits; mask; value })
  | "x86" ->
      let* space = mem_string "space" json in
      let* opcode_map = mem_int "opcode_map" json in
      let* opcode = mem_string "opcode" json in
      let* pattern = mem_string "pattern" json in
      Ok (Isa_norm_model.X86_encoding { space; opcode_map; opcode; pattern })
  | other -> err "unknown encoding kind: %s" other

let rec requirement_to_json : Isa_norm_model.requirement -> Jsont.json = function
  | Req_all rs -> obj [ ("kind", str "all"); ("of", list_ requirement_to_json rs) ]
  | Req_any rs -> obj [ ("kind", str "any"); ("of", list_ requirement_to_json rs) ]
  | Req_not r -> obj [ ("kind", str "not"); ("of", requirement_to_json r) ]
  | Req_feature name -> obj [ ("kind", str "feature"); ("name", str name) ]
  | Req_mode { mode; equals } ->
      obj [ ("kind", str "mode"); ("mode", str mode); ("equals", bool_ equals) ]
  | Req_xlen xlen -> obj [ ("kind", str "xlen"); ("xlen", int_ xlen) ]
  | Req_unknown name -> obj [ ("kind", str "unknown"); ("name", str name) ]

let rec requirement_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "all" ->
      let* xs = mem_list "of" json in
      let* rs = result_map requirement_of_json xs in
      Ok (Isa_norm_model.Req_all rs)
  | "any" ->
      let* xs = mem_list "of" json in
      let* rs = result_map requirement_of_json xs in
      Ok (Isa_norm_model.Req_any rs)
  | "not" ->
      let* r_json = mem_json "of" json in
      let* r = requirement_of_json r_json in
      Ok (Isa_norm_model.Req_not r)
  | "feature" ->
      let* name = mem_string "name" json in
      Ok (Isa_norm_model.Req_feature name)
  | "mode" ->
      let* mode = mem_string "mode" json in
      let* equals = mem_bool "equals" json in
      Ok (Isa_norm_model.Req_mode { mode; equals })
  | "xlen" ->
      let* xlen = mem_int "xlen" json in
      Ok (Isa_norm_model.Req_xlen xlen)
  | "unknown" ->
      let* name = mem_string "name" json in
      Ok (Isa_norm_model.Req_unknown name)
  | other -> err "unknown requirement kind: %s" other

let concreteness_to_json : Isa_norm_model.concreteness -> Jsont.json = function
  | Concrete -> obj [ ("kind", str "concrete") ]
  | Alias_of name -> obj [ ("kind", str "alias_of"); ("of", str name) ]
  | Expansion_of name -> obj [ ("kind", str "expansion_of"); ("of", str name) ]

let concreteness_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "concrete" -> Ok Isa_norm_model.Concrete
  | "alias_of" ->
      let* name = mem_string "of" json in
      Ok (Isa_norm_model.Alias_of name)
  | "expansion_of" ->
      let* name = mem_string "of" json in
      Ok (Isa_norm_model.Expansion_of name)
  | other -> err "unknown concreteness kind: %s" other

let rec syntax_token_to_json : Isa_norm_model.syntax_token -> Jsont.json = function
  | Syn_operand name -> obj [ ("kind", str "operand"); ("name", str name) ]
  | Syn_literal text -> obj [ ("kind", str "literal"); ("text", str text) ]
  | Syn_group toks -> obj [ ("kind", str "group"); ("tokens", list_ syntax_token_to_json toks) ]
  | Syn_decorated (prefix, tok) ->
      obj [ ("kind", str "decorated"); ("prefix", str prefix); ("token", syntax_token_to_json tok) ]

let rec syntax_token_of_json json =
  let* kind = mem_string "kind" json in
  match kind with
  | "operand" ->
      let* name = mem_string "name" json in
      Ok (Isa_norm_model.Syn_operand name)
  | "literal" ->
      let* text = mem_string "text" json in
      Ok (Isa_norm_model.Syn_literal text)
  | "group" ->
      let* xs = mem_list "tokens" json in
      let* toks = result_map syntax_token_of_json xs in
      Ok (Isa_norm_model.Syn_group toks)
  | "decorated" ->
      let* prefix = mem_string "prefix" json in
      let* tok_json = mem_json "token" json in
      let* tok = syntax_token_of_json tok_json in
      Ok (Isa_norm_model.Syn_decorated (prefix, tok))
  | other -> err "unknown syntax_token kind: %s" other

let syntax_recipe_to_json (s : Isa_norm_model.syntax_recipe) =
  obj
    [
      ("dialect", str s.dialect);
      ("mnemonic", str s.mnemonic);
      ("operands", list_ syntax_token_to_json s.operands);
    ]

let syntax_recipe_of_json json =
  let* dialect = mem_string "dialect" json in
  let* mnemonic = mem_string "mnemonic" json in
  let* xs = mem_list "operands" json in
  let* operands = result_map syntax_token_of_json xs in
  Ok Isa_norm_model.{ dialect; mnemonic; operands }

let provenance_label_to_json : Isa_norm_model.provenance_label -> Jsont.json = function
  | Upstream -> str "upstream"
  | Inferred -> str "inferred"

let provenance_label_of_json json =
  let* s = as_string json in
  match s with
  | "upstream" -> Ok Isa_norm_model.Upstream
  | "inferred" -> Ok Isa_norm_model.Inferred
  | other -> err "unknown provenance_label: %s" other

let fact_to_json (f : Isa_norm_model.fact) =
  obj [ ("label", provenance_label_to_json f.label); ("note", str f.note) ]

let fact_of_json json =
  let* label_json = mem_json "label" json in
  let* label = provenance_label_of_json label_json in
  let* note = mem_string "note" json in
  Ok Isa_norm_model.{ label; note }

let diagnostic_to_json (d : Isa_norm_model.diagnostic) =
  obj [ ("rule", str d.rule); ("message", str d.message) ]

let diagnostic_of_json json =
  let* rule = mem_string "rule" json in
  let* message = mem_string "message" json in
  Ok Isa_norm_model.{ rule; message }

let schema_version = 1

let to_json (f : Isa_norm_model.form) =
  obj
    [
      ("schema_version", int_ schema_version);
      ("form_id", str f.form_id);
      ("arch", arch_to_json f.arch);
      ("native_name", str f.native_name);
      ("source_record_ids", list_ str f.source_record_ids);
      ("requirement", requirement_to_json f.requirement);
      ("encoding", encoding_to_json f.encoding);
      ("operands", list_ operand_to_json f.operands);
      ("syntax", syntax_recipe_to_json f.syntax);
      ("concreteness", concreteness_to_json f.concreteness);
      ("facts", list_ fact_to_json f.facts);
      ("diagnostics", list_ diagnostic_to_json f.diagnostics);
    ]

let of_json json =
  let* schema_version_read = mem_int "schema_version" json in
  let* () =
    if schema_version_read = schema_version then Ok ()
    else
      err "unsupported normalized-form schema_version: %d (expected %d)" schema_version_read
        schema_version
  in
  let* form_id = mem_string "form_id" json in
  let* arch_json = mem_json "arch" json in
  let* arch = arch_of_json arch_json in
  let* native_name = mem_string "native_name" json in
  let* source_record_ids_json = mem_list "source_record_ids" json in
  let* source_record_ids = result_map as_string source_record_ids_json in
  let* requirement_json = mem_json "requirement" json in
  let* requirement = requirement_of_json requirement_json in
  let* encoding_json = mem_json "encoding" json in
  let* encoding = encoding_of_json encoding_json in
  let* operands_json = mem_list "operands" json in
  let* operands = result_map operand_of_json operands_json in
  let* syntax_json = mem_json "syntax" json in
  let* syntax = syntax_recipe_of_json syntax_json in
  let* concreteness_json = mem_json "concreteness" json in
  let* concreteness = concreteness_of_json concreteness_json in
  let* facts_json = mem_list "facts" json in
  let* facts = result_map fact_of_json facts_json in
  let* diagnostics_json = mem_list "diagnostics" json in
  let* diagnostics = result_map diagnostic_of_json diagnostics_json in
  Ok
    Isa_norm_model.
      {
        form_id;
        arch;
        native_name;
        source_record_ids;
        requirement;
        encoding;
        operands;
        syntax;
        concreteness;
        facts;
        diagnostics;
      }

let fail detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Parse detail)

let encode_line form =
  match Jsont_bytesrw.encode_string Jsont.json (to_json form) with
  | Ok line -> Err.return line
  | Error msg -> fail msg

let decode_line line =
  match Jsont_bytesrw.decode_string Jsont.json line with
  | Error msg -> fail msg
  | Ok json -> ( match of_json json with Ok form -> Err.return form | Error msg -> fail msg)
