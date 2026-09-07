(* Isa_norm_jsonl round-trip: every
   constructor of every Isa_norm_model sum type, encoded and decoded back to
   an equal value, plus a determinism check (encoding twice yields identical
   bytes) and a schema_version mismatch rejection. Synthetic forms, not real
   captured data - Isa_norm_riscv/Isa_norm_xed's own tests already ground
   normalization itself against real records; this suite is about the codec,
   which is a separate concern from any one source's normalization rules. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let base_form : Isa_norm_model.form =
  {
    form_id = "riscv:sw:rv_i";
    arch = Isa_norm_model.Riscv;
    native_name = "sw";
    source_record_ids = [ "riscv-opcodes:rv_i:sw@L20" ];
    requirement = Isa_norm_model.Req_all [];
    encoding = Isa_norm_model.Riscv_encoding { width_bits = 32; mask = "0x707f"; value = "0x2023" };
    operands =
      [
        {
          op_name = "value";
          op_kind = Isa_norm_model.Register { class_ = Isa_norm_model.Riscv_gpr; excluded = [] };
          role = Isa_norm_model.In;
          explicit = true;
        };
      ];
    syntax = { dialect = "gas"; mnemonic = "sw"; operands = [ Isa_norm_model.Syn_operand "value" ] };
    concreteness = Isa_norm_model.Concrete;
    facts = [ { label = Isa_norm_model.Upstream; note = "verbatim mask/value" } ];
    diagnostics = [];
  }

(* Exercise every constructor at least once, several nested inside one form
   so the recursive requirement/syntax_token codecs are actually recursed
   through rather than only hit at depth one. *)
let kitchen_sink_form : Isa_norm_model.form =
  {
    form_id = "x86:kitchen-sink";
    arch = Isa_norm_model.X86;
    native_name = "KITCHEN_SINK";
    source_record_ids = [ "xed:base:kitchen_sink@1"; "xed:base:kitchen_sink@2" ];
    requirement =
      Isa_norm_model.Req_all
        [
          Isa_norm_model.Req_any
            [
              Isa_norm_model.Req_feature "BASE";
              Isa_norm_model.Req_mode { mode = "mode64"; equals = true };
            ];
          Isa_norm_model.Req_not (Isa_norm_model.Req_xlen 32);
          Isa_norm_model.Req_unknown "unmapped XED extension: FOO";
        ];
    encoding =
      Isa_norm_model.X86_encoding
        { space = "legacy"; opcode_map = 0; opcode = "0x01"; pattern = "0x01 /r" };
    operands =
      [
        {
          op_name = "dest";
          op_kind =
            Isa_norm_model.Register { class_ = Isa_norm_model.X86_gpr; excluded = [ "esp" ] };
          role = Isa_norm_model.In_out;
          explicit = true;
        };
        {
          op_name = "imm";
          op_kind =
            Isa_norm_model.Immediate
              {
                width_bits = 32;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    { field_name = "SIMMz"; field_hi = 31; field_lo = 0; dest_hi = 31; dest_lo = 0 };
                  ];
              };
          role = Isa_norm_model.In;
          explicit = true;
        };
        {
          op_name = "mem";
          op_kind = Isa_norm_model.Memory { width_bits = None };
          role = Isa_norm_model.In;
          explicit = true;
        };
        {
          op_name = "rounding";
          op_kind = Isa_norm_model.Rounding_mode;
          role = Isa_norm_model.In;
          explicit = false;
        };
        {
          op_name = "st0";
          op_kind =
            Isa_norm_model.Implicit_register { class_ = Isa_norm_model.X87_st; native_name = "ST0" };
          role = Isa_norm_model.Out;
          explicit = false;
        };
      ];
    syntax =
      {
        dialect = "att";
        mnemonic = "kitchen_sink";
        operands =
          [
            Isa_norm_model.Syn_decorated ("$", Isa_norm_model.Syn_operand "imm");
            Isa_norm_model.Syn_literal ", ";
            Isa_norm_model.Syn_group
              [
                Isa_norm_model.Syn_literal "(";
                Isa_norm_model.Syn_decorated ("%", Isa_norm_model.Syn_operand "dest");
                Isa_norm_model.Syn_literal ")";
              ];
          ];
      };
    concreteness = Isa_norm_model.Alias_of "KITCHEN_SINK_LONG";
    facts =
      [
        { label = Isa_norm_model.Upstream; note = "encoding.pattern verbatim" };
        { label = Isa_norm_model.Inferred; note = "AT&T operand order" };
      ];
    diagnostics =
      [ { rule = "example-rule"; message = "kept for codec coverage, not a real diagnostic" } ];
  }

let expansion_form : Isa_norm_model.form =
  { kitchen_sink_form with concreteness = Isa_norm_model.Expansion_of "PSEUDO" }

let roundtrip name (form : Isa_norm_model.form) =
  match Isa_norm_jsonl.of_json (Isa_norm_jsonl.to_json form) with
  | Ok decoded -> check (Printf.sprintf "%s: to_json/of_json round-trips" name) (decoded = form)
  | Error msg ->
      check (Printf.sprintf "%s: to_json/of_json round-trips (decode failed: %s)" name msg) false

let roundtrip_line name (form : Isa_norm_model.form) =
  match Isa_norm_jsonl.encode_line form with
  | Error _ ->
      check (Printf.sprintf "%s: encode_line/decode_line round-trips (encode failed)" name) false
  | Ok line -> (
      match Isa_norm_jsonl.decode_line line with
      | Ok decoded ->
          check (Printf.sprintf "%s: encode_line/decode_line round-trips" name) (decoded = form)
      | Error _ ->
          check
            (Printf.sprintf "%s: encode_line/decode_line round-trips (decode failed)" name)
            false)

let test_determinism () =
  match
    (Isa_norm_jsonl.encode_line kitchen_sink_form, Isa_norm_jsonl.encode_line kitchen_sink_form)
  with
  | Ok l1, Ok l2 -> check "encode_line is deterministic (same bytes twice)" (String.equal l1 l2)
  | _ -> check "encode_line is deterministic (same bytes twice)" false

(* Replace the first occurrence of [sub] in [s] with [by], or return [s]
   unchanged if [sub] is not found. *)
let replace_first sub by s =
  let slen = String.length s and sublen = String.length sub in
  let rec find i =
    if i + sublen > slen then None else if String.sub s i sublen = sub then Some i else find (i + 1)
  in
  match find 0 with
  | None -> s
  | Some i -> String.sub s 0 i ^ by ^ String.sub s (i + sublen) (slen - i - sublen)

let contains sub s =
  let slen = String.length s and sublen = String.length sub in
  let rec find i = i + sublen <= slen && (String.sub s i sublen = sub || find (i + 1)) in
  find 0

let test_schema_version_mismatch () =
  let needle = Printf.sprintf "\"schema_version\":%d" Isa_norm_jsonl.schema_version in
  let bumped = Printf.sprintf "\"schema_version\":%d" (Isa_norm_jsonl.schema_version + 1) in
  match Isa_norm_jsonl.encode_line base_form with
  | Error _ -> check "schema_version mismatch is rejected (setup: encode)" false
  | Ok line -> (
      check "encoded line carries the expected schema_version member" (contains needle line);
      let mutated = replace_first needle bumped line in
      match Isa_norm_jsonl.decode_line mutated with
      | Error _ -> check "schema_version mismatch is rejected" true
      | Ok _ -> check "schema_version mismatch is rejected" false)

let () =
  print_endline "isa-norm-jsonl:";
  roundtrip "sw (base form)" base_form;
  roundtrip "kitchen sink (every constructor)" kitchen_sink_form;
  roundtrip "expansion_of concreteness" expansion_form;
  roundtrip_line "sw (base form)" base_form;
  roundtrip_line "kitchen sink (every constructor)" kitchen_sink_form;
  test_determinism ();
  test_schema_version_mismatch ();
  if !failures > 0 then (
    Printf.printf "isa-norm-jsonl: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-jsonl: all %d checks passed\n" !checks
