module R = Isa_source_record

type kind = Encoding | Operand
type construct = { kind : kind; name : string }

let to_string c =
  match c.kind with
  | Encoding -> "unsupported-encoding:" ^ c.name
  | Operand -> "unknown-operand:" ^ c.name

let enc name = { kind = Encoding; name }
let opd name = { kind = Operand; name }

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* XED lookup functions name a register class plus the ModRM/VEX field that
   selects it (GPRv_R, XMM_N3, ...); only the class matters here. The [3]
   suffix (32 EVEX registers) is a register-range obligation of the same
   class, not a separate construct. *)
let register_class lookup =
  let classes =
    [
      ("GPR8_", "gpr8");
      ("GPR16_", "gpr16");
      ("GPRv_", "gpr");
      ("GPRz_", "gpr");
      ("GPRy_", "gpr");
      ("GPR32_", "gpr");
      ("GPR64_", "gpr");
      ("VGPR", "gpr");
      ("A_GPR_", "gpr");
      ("XMM_", "xmm");
      ("YMM_", "ymm");
      ("ZMM_", "zmm");
      ("MASK_", "k");
      ("MASKNOT0", "k");
      ("MMX_", "mmx");
      ("X87", "st");
      ("TMM_", "tmm");
      ("BND_", "bnd");
      ("CR_", "cr");
      ("DR_", "dr");
      ("SEG", "seg");
    ]
  in
  match List.find_opt (fun (prefix, _) -> starts_with ~prefix lookup) classes with
  | Some (_, cls) -> "reg:" ^ cls
  | None -> "reg-lookup:" ^ lookup

let strip_xed_reg s =
  let p = "XED_REG_" in
  if starts_with ~prefix:p s then String.sub s (String.length p) (String.length s - String.length p)
  else s

let x86_operand (o : R.x86_operand) =
  let oc2 = Option.value o.oc2 ~default:"-" in
  if o.visibility = "SUPPRESSED" then []
  else
    match o.op_type with
    | "nt_lookup_fn" -> (
        match o.lookupfn_name with
        | Some "MASK1" -> [ enc "evex-opmask" ]
        | Some ("XMM_SE" | "YMM_SE") -> [ enc "is4" ]
        | Some lookup when o.visibility = "IMPLICIT" -> [ opd ("implicit:" ^ lookup) ]
        | Some lookup -> [ opd (register_class lookup) ]
        | None -> [ opd ("lookup:" ^ o.op_name) ])
    | "reg" ->
        let reg = strip_xed_reg (Option.value o.bits ~default:"?") in
        [ opd ("implicit:" ^ reg) ]
    | "imm_const" ->
        let name = o.op_name in
        if starts_with ~prefix:"MEM" name then [ opd ("mem:" ^ oc2) ]
        else if starts_with ~prefix:"AGEN" name then [ opd "agen" ]
        else if starts_with ~prefix:"RELBR" name || starts_with ~prefix:"ABSBR" name then
          [ enc "branch-displacement" ]
        else if starts_with ~prefix:"BCAST" name then []
        else if starts_with ~prefix:"IMM" name then
          if o.visibility = "IMPLICIT" then [ opd "implicit:imm-one" ] else [ opd ("imm:" ^ oc2) ]
        else if starts_with ~prefix:"PTR" name then [ opd "far-pointer" ]
        else [ opd ("operand:" ^ name) ]
    | other -> [ opd ("operand-type:" ^ other) ]

let pattern_constructs ~space pattern =
  let tokens = String.split_on_char ' ' pattern |> List.filter (fun t -> t <> "") in
  let has t = List.mem t tokens in
  let vector = space <> "legacy" in
  let vl =
    List.filter_map
      (fun (t, bits) ->
        if vector && has t then Some (enc (Printf.sprintf "%s-vl%d" space bits)) else None)
      [ ("VL=0", 128); ("VL=1", 256); ("VL=2", 512) ]
  in
  let flag t name = if has t then [ enc name ] else [] in
  let rounding =
    if has "BCRC=1" then
      if has "AVX512_ROUND()" then [ enc "evex-rounding" ]
      else if has "SAE()" then [ enc "evex-sae" ]
      else [ enc "evex-broadcast" ]
    else []
  in
  let vsib =
    if
      List.exists
        (fun t ->
          List.mem t
            [
              "UISA_VMODRM_XMM()";
              "UISA_VMODRM_YMM()";
              "UISA_VMODRM_ZMM()";
              "VMODRM_XMM()";
              "VMODRM_YMM()";
            ])
        tokens
    then [ enc "vsib" ]
    else []
  in
  vl
  @ (if vector && has "REXW=1" then [ enc (space ^ "-w1") ] else [])
  @ flag "ZEROING=1" "evex-zeroing" @ rounding @ vsib @ flag "REX2=1" "rex2"
  @ flag "EVAPX()" "apx-evex" @ flag "ND=1" "apx-ndd" @ flag "NF=1" "apx-nf"
  @ flag "EVAPX_SCC()" "apx-scc" @ flag "LOCK=1" "lock"

let dedup_sorted l =
  let rank c = match c.kind with Encoding -> 0 | Operand -> 1 in
  List.sort_uniq
    (fun a b -> match compare (rank a) (rank b) with 0 -> String.compare a.name b.name | n -> n)
    l

let of_record (rec_ : R.t) =
  match rec_.encoding with
  | R.X86_encoding { space; opcode_map; pattern; operands; _ } ->
      let space_map = enc (Printf.sprintf "%s-map%d" space opcode_map) in
      dedup_sorted
        ((space_map :: pattern_constructs ~space pattern) @ List.concat_map x86_operand operands)
  | R.Fixed_bits { width_bits; _ } ->
      let fields =
        match rec_.provenance with
        | R.Riscv_provenance { variable_fields; _ } ->
            List.map (fun f -> opd ("field:" ^ f)) variable_fields
        | _ -> []
      in
      dedup_sorted ((if width_bits = 16 then [ enc "len16" ] else []) @ fields)
  | R.Opaque _ -> [ enc "opaque-encoding" ]
  | R.Unknown_encoding kind -> [ enc ("encoding-kind:" ^ kind) ]

let is_catch_all = function "unhandled-iform" | "unhandled-native-name" -> true | _ -> false
let no_rule = "no-rule:known-constructs"

module S = Set.Make (String)

type known = S.t

let known_of pairs =
  List.fold_left
    (fun acc (rec_, ok) ->
      if ok then List.fold_left (fun acc c -> S.add (to_string c) acc) acc (of_record rec_) else acc)
    S.empty pairs

let missing known rec_ = List.filter (fun c -> not (S.mem (to_string c) known)) (of_record rec_)

let blocker known rec_ =
  match missing known rec_ with [] -> no_rule | first :: _ -> to_string first
