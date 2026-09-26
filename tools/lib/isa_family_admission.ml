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
type summary = { total : int; families : family list }

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
  | Oracle_unavailable reason ->
      tally.oracle_unavailable <- tally.oracle_unavailable + 1;
      tally.blocked <- bump tally.blocked ("oracle-unavailable:" ^ reason)
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

(* [promoted_case] is split into several smaller functions: one huge string
   match makes the 32-bit ARM OCaml 4.14 backend emit an out-of-range 16-bit
   field that the assembler rejects. *)

let promoted_case_part1 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | (Target.Riscv32 | Target.Riscv64), ("riscv:add" | "riscv:sub" | "riscv:mul" | "riscv:addi"), _
    ->
      true
  | Target.Riscv64, "riscv:addw", _ -> true
  | (Target.X86_32 | Target.X86_64), "x86:ADD_GPRv_GPRv_01", "ADD_GPRv_GPRv_01" -> true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:SUB_GPRv_GPRv_29" | "x86:AND_GPRv_GPRv_21" | "x86:OR_GPRv_GPRv_09"
      | "x86:XOR_GPRv_GPRv_31" | "x86:ADC_GPRv_GPRv_11" | "x86:SBB_GPRv_GPRv_19"
      | "x86:CMP_GPRv_GPRv_39" | "x86:TEST_GPRv_GPRv" ),
      ( "SUB_GPRv_GPRv_29" | "AND_GPRv_GPRv_21" | "OR_GPRv_GPRv_09" | "XOR_GPRv_GPRv_31"
      | "ADC_GPRv_GPRv_11" | "SBB_GPRv_GPRv_19" | "CMP_GPRv_GPRv_39" | "TEST_GPRv_GPRv" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_GPRv_MEMv" | "x86:ADC_GPRv_MEMv" | "x86:XOR_GPRv_MEMv" | "x86:SUB_GPRv_MEMv"
      | "x86:AND_GPRv_MEMv" | "x86:OR_GPRv_MEMv" | "x86:SBB_GPRv_MEMv" | "x86:CMP_GPRv_MEMv" ),
      ( "ADD_GPRv_MEMv" | "ADC_GPRv_MEMv" | "XOR_GPRv_MEMv" | "SUB_GPRv_MEMv" | "AND_GPRv_MEMv"
      | "OR_GPRv_MEMv" | "SBB_GPRv_MEMv" | "CMP_GPRv_MEMv" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_MEMv_GPRv" | "x86:OR_MEMv_GPRv" | "x86:ADC_MEMv_GPRv" | "x86:SBB_MEMv_GPRv"
      | "x86:AND_MEMv_GPRv" | "x86:SUB_MEMv_GPRv" | "x86:XOR_MEMv_GPRv" | "x86:CMP_MEMv_GPRv"
      | "x86:TEST_MEMv_GPRv" ),
      ( "ADD_MEMv_GPRv" | "OR_MEMv_GPRv" | "ADC_MEMv_GPRv" | "SBB_MEMv_GPRv" | "AND_MEMv_GPRv"
      | "SUB_MEMv_GPRv" | "XOR_MEMv_GPRv" | "CMP_MEMv_GPRv" | "TEST_MEMv_GPRv" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:OR_GPRv_IMMz" | "x86:ADC_GPRv_IMMz" | "x86:SBB_GPRv_IMMz" | "x86:AND_GPRv_IMMz"
      | "x86:SUB_GPRv_IMMz" | "x86:XOR_GPRv_IMMz" | "x86:CMP_GPRv_IMMz" ),
      ( "OR_GPRv_IMMz" | "ADC_GPRv_IMMz" | "SBB_GPRv_IMMz" | "AND_GPRv_IMMz" | "SUB_GPRv_IMMz"
      | "XOR_GPRv_IMMz" | "CMP_GPRv_IMMz" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_GPRv_IMMb" | "x86:OR_GPRv_IMMb" | "x86:ADC_GPRv_IMMb" | "x86:SBB_GPRv_IMMb"
      | "x86:AND_GPRv_IMMb" | "x86:SUB_GPRv_IMMb" | "x86:XOR_GPRv_IMMb" | "x86:CMP_GPRv_IMMb" ),
      ( "ADD_GPRv_IMMb" | "OR_GPRv_IMMb" | "ADC_GPRv_IMMb" | "SBB_GPRv_IMMb" | "AND_GPRv_IMMb"
      | "SUB_GPRv_IMMb" | "XOR_GPRv_IMMb" | "CMP_GPRv_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_MEMv_IMMb" | "x86:OR_MEMv_IMMb" | "x86:ADC_MEMv_IMMb" | "x86:SBB_MEMv_IMMb"
      | "x86:AND_MEMv_IMMb" | "x86:SUB_MEMv_IMMb" | "x86:XOR_MEMv_IMMb" | "x86:CMP_MEMv_IMMb" ),
      ( "ADD_MEMv_IMMb" | "OR_MEMv_IMMb" | "ADC_MEMv_IMMb" | "SBB_MEMv_IMMb" | "AND_MEMv_IMMb"
      | "SUB_MEMv_IMMb" | "XOR_MEMv_IMMb" | "CMP_MEMv_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_MEMv_IMMz" | "x86:OR_MEMv_IMMz" | "x86:ADC_MEMv_IMMz" | "x86:SBB_MEMv_IMMz"
      | "x86:AND_MEMv_IMMz" | "x86:SUB_MEMv_IMMz" | "x86:XOR_MEMv_IMMz" | "x86:CMP_MEMv_IMMz" ),
      ( "ADD_MEMv_IMMz" | "OR_MEMv_IMMz" | "ADC_MEMv_IMMz" | "SBB_MEMv_IMMz" | "AND_MEMv_IMMz"
      | "SUB_MEMv_IMMz" | "XOR_MEMv_IMMz" | "CMP_MEMv_IMMz" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_GPR8_IMMb_80r0" | "x86:OR_GPR8_IMMb_80r1" | "x86:ADC_GPR8_IMMb_80r2"
      | "x86:SBB_GPR8_IMMb_80r3" | "x86:AND_GPR8_IMMb_80r4" | "x86:SUB_GPR8_IMMb_80r5"
      | "x86:XOR_GPR8_IMMb_80r6" | "x86:CMP_GPR8_IMMb_80r7" ),
      ( "ADD_GPR8_IMMb_80r0" | "OR_GPR8_IMMb_80r1" | "ADC_GPR8_IMMb_80r2" | "SBB_GPR8_IMMb_80r3"
      | "AND_GPR8_IMMb_80r4" | "SUB_GPR8_IMMb_80r5" | "XOR_GPR8_IMMb_80r6" | "CMP_GPR8_IMMb_80r7" )
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_MEMb_IMMb_80r0" | "x86:OR_MEMb_IMMb_80r1" | "x86:ADC_MEMb_IMMb_80r2"
      | "x86:SBB_MEMb_IMMb_80r3" | "x86:AND_MEMb_IMMb_80r4" | "x86:SUB_MEMb_IMMb_80r5"
      | "x86:XOR_MEMb_IMMb_80r6" | "x86:CMP_MEMb_IMMb_80r7" ),
      ( "ADD_MEMb_IMMb_80r0" | "OR_MEMb_IMMb_80r1" | "ADC_MEMb_IMMb_80r2" | "SBB_MEMb_IMMb_80r3"
      | "AND_MEMb_IMMb_80r4" | "SUB_MEMb_IMMb_80r5" | "XOR_MEMb_IMMb_80r6" | "CMP_MEMb_IMMb_80r7" )
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADD_AL_IMMb" | "x86:OR_AL_IMMb" | "x86:ADC_AL_IMMb" | "x86:SBB_AL_IMMb"
      | "x86:AND_AL_IMMb" | "x86:SUB_AL_IMMb" | "x86:XOR_AL_IMMb" | "x86:CMP_AL_IMMb" ),
      ( "ADD_AL_IMMb" | "OR_AL_IMMb" | "ADC_AL_IMMb" | "SBB_AL_IMMb" | "AND_AL_IMMb" | "SUB_AL_IMMb"
      | "XOR_AL_IMMb" | "CMP_AL_IMMb" ) ) ->
      true
  | (Target.Riscv32 | Target.Riscv64), "riscv:sw", "sw" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:beq", "beq" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:c.addi", "c.addi" -> true
  | (Target.X86_32 | Target.X86_64), "x86:MOV_GPRv_MEMv", "MOV_GPRv_MEMv" -> true
  | (Target.X86_32 | Target.X86_64), "x86:MOV_MEMv_GPRv", "MOV_MEMv_GPRv" -> true
  | (Target.X86_32 | Target.X86_64), "x86:FADD_ST0_X87", "FADD_ST0_X87" -> true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADDSD_XMMsd_XMMsd" | "x86:SUBSD_XMMsd_XMMsd" | "x86:MULSD_XMMsd_XMMsd"
      | "x86:DIVSD_XMMsd_XMMsd" | "x86:MULSS_XMMss_XMMss" | "x86:DIVSS_XMMss_XMMss"
      | "x86:COMISD_XMMsd_XMMsd" | "x86:UCOMISD_XMMsd_XMMsd" | "x86:COMISS_XMMss_XMMss"
      | "x86:UCOMISS_XMMss_XMMss" | "x86:XORPD_XMMxuq_XMMxuq" | "x86:PXOR_XMMdq_XMMdq"
      | "x86:MOVAPD_XMMpd_XMMpd_0F28" | "x86:CVTSD2SS_XMMss_XMMsd" | "x86:CVTSS2SD_XMMsd_XMMss" ),
      ( "ADDSD_XMMsd_XMMsd" | "SUBSD_XMMsd_XMMsd" | "MULSD_XMMsd_XMMsd" | "DIVSD_XMMsd_XMMsd"
      | "MULSS_XMMss_XMMss" | "DIVSS_XMMss_XMMss" | "COMISD_XMMsd_XMMsd" | "UCOMISD_XMMsd_XMMsd"
      | "COMISS_XMMss_XMMss" | "UCOMISS_XMMss_XMMss" | "XORPD_XMMxuq_XMMxuq" | "PXOR_XMMdq_XMMdq"
      | "MOVAPD_XMMpd_XMMpd_0F28" | "CVTSD2SS_XMMss_XMMsd" | "CVTSS2SD_XMMsd_XMMss" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADDSD_XMMsd_MEMsd" | "x86:SUBSD_XMMsd_MEMsd" | "x86:MULSD_XMMsd_MEMsd"
      | "x86:DIVSD_XMMsd_MEMsd" | "x86:MULSS_XMMss_MEMss" | "x86:DIVSS_XMMss_MEMss"
      | "x86:COMISD_XMMsd_MEMsd" | "x86:UCOMISD_XMMsd_MEMsd" | "x86:COMISS_XMMss_MEMss"
      | "x86:UCOMISS_XMMss_MEMss" | "x86:XORPD_XMMxuq_MEMxuq" | "x86:PXOR_XMMdq_MEMdq"
      | "x86:MOVAPD_XMMpd_MEMpd" | "x86:CVTSD2SS_XMMss_MEMsd" | "x86:CVTSS2SD_XMMsd_MEMss" ),
      ( "ADDSD_XMMsd_MEMsd" | "SUBSD_XMMsd_MEMsd" | "MULSD_XMMsd_MEMsd" | "DIVSD_XMMsd_MEMsd"
      | "MULSS_XMMss_MEMss" | "DIVSS_XMMss_MEMss" | "COMISD_XMMsd_MEMsd" | "UCOMISD_XMMsd_MEMsd"
      | "COMISS_XMMss_MEMss" | "UCOMISS_XMMss_MEMss" | "XORPD_XMMxuq_MEMxuq" | "PXOR_XMMdq_MEMdq"
      | "MOVAPD_XMMpd_MEMpd" | "CVTSD2SS_XMMss_MEMsd" | "CVTSS2SD_XMMsd_MEMss" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:MOVSD_XMM_XMMdq_MEMsd" | "x86:MOVSD_XMM_MEMsd_XMMsd" | "x86:MOVSS_XMMdq_MEMss"
      | "x86:MOVSS_MEMss_XMMss" ),
      ("MOVSD_XMM_XMMdq_MEMsd" | "MOVSD_XMM_MEMsd_XMMsd" | "MOVSS_XMMdq_MEMss" | "MOVSS_MEMss_XMMss")
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:CVTSI2SD_XMMsd_GPR32d" | "x86:CVTSI2SS_XMMss_GPR32d" | "x86:CVTSI2SD_XMMsd_MEMd"
      | "x86:CVTSI2SS_XMMss_MEMd" | "x86:CVTTSD2SI_GPR32d_XMMsd" | "x86:CVTTSD2SI_GPR32d_MEMsd" ),
      ( "CVTSI2SD_XMMsd_GPR32d" | "CVTSI2SS_XMMss_GPR32d" | "CVTSI2SD_XMMsd_MEMd"
      | "CVTSI2SS_XMMss_MEMd" | "CVTTSD2SI_GPR32d_XMMsd" | "CVTTSD2SI_GPR32d_MEMsd" ) ) ->
      true
  | ( Target.X86_64,
      ( "x86:CVTSI2SD_XMMsd_GPR64q" | "x86:CVTSI2SS_XMMss_GPR64q" | "x86:CVTSI2SD_XMMsd_MEMq"
      | "x86:CVTSI2SS_XMMss_MEMq" | "x86:CVTTSD2SI_GPR64q_XMMsd" | "x86:CVTTSD2SI_GPR64q_MEMsd" ),
      ( "CVTSI2SD_XMMsd_GPR64q" | "CVTSI2SS_XMMss_GPR64q" | "CVTSI2SD_XMMsd_MEMq"
      | "CVTSI2SS_XMMss_MEMq" | "CVTTSD2SI_GPR64q_XMMsd" | "CVTTSD2SI_GPR64q_MEMsd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:MOVD_XMMdq_GPR32" | "x86:MOVD_GPR32_XMMd" | "x86:MOVD_XMMdq_MEMd" | "x86:MOVD_MEMd_XMMd"),
      ("MOVD_XMMdq_GPR32" | "MOVD_GPR32_XMMd" | "MOVD_XMMdq_MEMd" | "MOVD_MEMd_XMMd") ) ->
      true
  | ( Target.X86_64,
      ("x86:MOVQ_XMMdq_GPR64" | "x86:MOVQ_GPR64_XMMq"),
      ("MOVQ_XMMdq_GPR64" | "MOVQ_GPR64_XMMq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMOVD_XMMdq_GPR32d" | "x86:VMOVD_GPR32d_XMMd" | "x86:VMOVD_XMMdq_MEMd"
      | "x86:VMOVD_MEMd_XMMd" ),
      ("VMOVD_XMMdq_GPR32d" | "VMOVD_GPR32d_XMMd" | "VMOVD_XMMdq_MEMd" | "VMOVD_MEMd_XMMd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PINSRW_XMMdq_GPR32_IMMb" | "x86:PINSRW_XMMdq_MEMw_IMMb" | "x86:PEXTRW_GPR32_XMMdq_IMMb"
      | "x86:VPINSRW_XMMdq_XMMdq_GPR32d_IMMb" | "x86:VPINSRW_XMMdq_XMMdq_MEMw_IMMb"
      | "x86:VPEXTRW_GPR32d_XMMdq_IMMb_C5" ),
      ( "PINSRW_XMMdq_GPR32_IMMb" | "PINSRW_XMMdq_MEMw_IMMb" | "PEXTRW_GPR32_XMMdq_IMMb"
      | "VPINSRW_XMMdq_XMMdq_GPR32d_IMMb" | "VPINSRW_XMMdq_XMMdq_MEMw_IMMb"
      | "VPEXTRW_GPR32d_XMMdq_IMMb_C5" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:MOVMSKPS_GPR32_XMMps" | "x86:MOVMSKPD_GPR32_XMMpd" | "x86:PMOVMSKB_GPR32_XMMdq"
      | "x86:VMOVMSKPS_GPR32d_XMMdq" | "x86:VMOVMSKPD_GPR32d_XMMdq" | "x86:VPMOVMSKB_GPR32d_XMMdq"
        ),
      ( "MOVMSKPS_GPR32_XMMps" | "MOVMSKPD_GPR32_XMMpd" | "PMOVMSKB_GPR32_XMMdq"
      | "VMOVMSKPS_GPR32d_XMMdq" | "VMOVMSKPD_GPR32d_XMMdq" | "VPMOVMSKB_GPR32d_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ANDPS_XMMxud_XMMxud" | "x86:ANDNPS_XMMxud_XMMxud" | "x86:ORPS_XMMxud_XMMxud"
      | "x86:XORPS_XMMxud_XMMxud" | "x86:ANDPD_XMMxuq_XMMxuq" | "x86:ANDNPD_XMMxuq_XMMxuq"
      | "x86:ORPD_XMMxuq_XMMxuq" ),
      ( "ANDPS_XMMxud_XMMxud" | "ANDNPS_XMMxud_XMMxud" | "ORPS_XMMxud_XMMxud"
      | "XORPS_XMMxud_XMMxud" | "ANDPD_XMMxuq_XMMxuq" | "ANDNPD_XMMxuq_XMMxuq"
      | "ORPD_XMMxuq_XMMxuq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ANDPS_XMMxud_MEMxud" | "x86:ANDNPS_XMMxud_MEMxud" | "x86:ORPS_XMMxud_MEMxud"
      | "x86:XORPS_XMMxud_MEMxud" | "x86:ANDPD_XMMxuq_MEMxuq" | "x86:ANDNPD_XMMxuq_MEMxuq"
      | "x86:ORPD_XMMxuq_MEMxuq" ),
      ( "ANDPS_XMMxud_MEMxud" | "ANDNPS_XMMxud_MEMxud" | "ORPS_XMMxud_MEMxud"
      | "XORPS_XMMxud_MEMxud" | "ANDPD_XMMxuq_MEMxuq" | "ANDNPD_XMMxuq_MEMxuq"
      | "ORPD_XMMxuq_MEMxuq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:MOVAPS_XMMps_XMMps_0F28" | "x86:MOVUPS_XMMps_XMMps_0F10" | "x86:MOVUPD_XMMpd_XMMpd_0F10"),
      ("MOVAPS_XMMps_XMMps_0F28" | "MOVUPS_XMMps_XMMps_0F10" | "MOVUPD_XMMpd_XMMpd_0F10") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:MOVAPS_XMMps_MEMps" | "x86:MOVUPS_XMMps_MEMps" | "x86:MOVUPD_XMMpd_MEMpd"),
      ("MOVAPS_XMMps_MEMps" | "MOVUPS_XMMps_MEMps" | "MOVUPD_XMMpd_MEMpd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADDPS_XMMps_XMMps" | "x86:SUBPS_XMMps_XMMps" | "x86:MULPS_XMMps_XMMps"
      | "x86:DIVPS_XMMps_XMMps" | "x86:ADDPD_XMMpd_XMMpd" | "x86:SUBPD_XMMpd_XMMpd"
      | "x86:MULPD_XMMpd_XMMpd" | "x86:DIVPD_XMMpd_XMMpd" ),
      ( "ADDPS_XMMps_XMMps" | "SUBPS_XMMps_XMMps" | "MULPS_XMMps_XMMps" | "DIVPS_XMMps_XMMps"
      | "ADDPD_XMMpd_XMMpd" | "SUBPD_XMMpd_XMMpd" | "MULPD_XMMpd_XMMpd" | "DIVPD_XMMpd_XMMpd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ADDPS_XMMps_MEMps" | "x86:SUBPS_XMMps_MEMps" | "x86:MULPS_XMMps_MEMps"
      | "x86:DIVPS_XMMps_MEMps" | "x86:ADDPD_XMMpd_MEMpd" | "x86:SUBPD_XMMpd_MEMpd"
      | "x86:MULPD_XMMpd_MEMpd" | "x86:DIVPD_XMMpd_MEMpd" ),
      ( "ADDPS_XMMps_MEMps" | "SUBPS_XMMps_MEMps" | "MULPS_XMMps_MEMps" | "DIVPS_XMMps_MEMps"
      | "ADDPD_XMMpd_MEMpd" | "SUBPD_XMMpd_MEMpd" | "MULPD_XMMpd_MEMpd" | "DIVPD_XMMpd_MEMpd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:MAXSS_XMMss_XMMss" | "x86:MINSS_XMMss_XMMss" | "x86:MAXSD_XMMsd_XMMsd"
      | "x86:MINSD_XMMsd_XMMsd" | "x86:MAXPS_XMMps_XMMps" | "x86:MINPS_XMMps_XMMps"
      | "x86:MAXPD_XMMpd_XMMpd" | "x86:MINPD_XMMpd_XMMpd" ),
      ( "MAXSS_XMMss_XMMss" | "MINSS_XMMss_XMMss" | "MAXSD_XMMsd_XMMsd" | "MINSD_XMMsd_XMMsd"
      | "MAXPS_XMMps_XMMps" | "MINPS_XMMps_XMMps" | "MAXPD_XMMpd_XMMpd" | "MINPD_XMMpd_XMMpd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:MAXSS_XMMss_MEMss" | "x86:MINSS_XMMss_MEMss" | "x86:MAXSD_XMMsd_MEMsd"
      | "x86:MINSD_XMMsd_MEMsd" | "x86:MAXPS_XMMps_MEMps" | "x86:MINPS_XMMps_MEMps"
      | "x86:MAXPD_XMMpd_MEMpd" | "x86:MINPD_XMMpd_MEMpd" ),
      ( "MAXSS_XMMss_MEMss" | "MINSS_XMMss_MEMss" | "MAXSD_XMMsd_MEMsd" | "MINSD_XMMsd_MEMsd"
      | "MAXPS_XMMps_MEMps" | "MINPS_XMMps_MEMps" | "MAXPD_XMMpd_MEMpd" | "MINPD_XMMpd_MEMpd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:SQRTSS_XMMss_XMMss" | "x86:SQRTSD_XMMsd_XMMsd" | "x86:SQRTPS_XMMps_XMMps"
      | "x86:SQRTPD_XMMpd_XMMpd" ),
      ("SQRTSS_XMMss_XMMss" | "SQRTSD_XMMsd_XMMsd" | "SQRTPS_XMMps_XMMps" | "SQRTPD_XMMpd_XMMpd") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:SQRTSS_XMMss_MEMss" | "x86:SQRTSD_XMMsd_MEMsd" | "x86:SQRTPS_XMMps_MEMps"
      | "x86:SQRTPD_XMMpd_MEMpd" ),
      ("SQRTSS_XMMss_MEMss" | "SQRTSD_XMMsd_MEMsd" | "SQRTPS_XMMps_MEMps" | "SQRTPD_XMMpd_MEMpd") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:CVTPS2PD_XMMpd_XMMq" | "x86:CVTPD2PS_XMMps_XMMpd"),
      ("CVTPS2PD_XMMpd_XMMq" | "CVTPD2PS_XMMps_XMMpd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:CVTPS2PD_XMMpd_MEMq" | "x86:CVTPD2PS_XMMps_MEMpd"),
      ("CVTPS2PD_XMMpd_MEMq" | "CVTPD2PS_XMMps_MEMpd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:CVTDQ2PS_XMMps_XMMdq" | "x86:CVTPS2DQ_XMMdq_XMMps" | "x86:CVTTPS2DQ_XMMdq_XMMps"),
      ("CVTDQ2PS_XMMps_XMMdq" | "CVTPS2DQ_XMMdq_XMMps" | "CVTTPS2DQ_XMMdq_XMMps") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:CVTDQ2PS_XMMps_MEMdq" | "x86:CVTPS2DQ_XMMdq_MEMps" | "x86:CVTTPS2DQ_XMMdq_MEMps"),
      ("CVTDQ2PS_XMMps_MEMdq" | "CVTPS2DQ_XMMdq_MEMps" | "CVTTPS2DQ_XMMdq_MEMps") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:UNPCKLPS_XMMps_XMMq" | "x86:UNPCKHPS_XMMps_XMMdq" | "x86:UNPCKLPD_XMMpd_XMMq"
      | "x86:UNPCKHPD_XMMpd_XMMq" ),
      ( "UNPCKLPS_XMMps_XMMq" | "UNPCKHPS_XMMps_XMMdq" | "UNPCKLPD_XMMpd_XMMq"
      | "UNPCKHPD_XMMpd_XMMq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:UNPCKLPS_XMMps_MEMdq" | "x86:UNPCKHPS_XMMps_MEMdq" | "x86:UNPCKLPD_XMMpd_MEMdq"
      | "x86:UNPCKHPD_XMMpd_MEMdq" ),
      ( "UNPCKLPS_XMMps_MEMdq" | "UNPCKHPS_XMMps_MEMdq" | "UNPCKLPD_XMMpd_MEMdq"
      | "UNPCKHPD_XMMpd_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PUNPCKLQDQ_XMMdq_XMMq" | "x86:PUNPCKHQDQ_XMMdq_XMMq"),
      ("PUNPCKLQDQ_XMMdq_XMMq" | "PUNPCKHQDQ_XMMdq_XMMq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PUNPCKLQDQ_XMMdq_MEMdq" | "x86:PUNPCKHQDQ_XMMdq_MEMdq"),
      ("PUNPCKLQDQ_XMMdq_MEMdq" | "PUNPCKHQDQ_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PUNPCKLBW_XMMdq_XMMq" | "x86:PUNPCKHBW_XMMdq_XMMq" | "x86:PUNPCKLWD_XMMdq_XMMq"
      | "x86:PUNPCKHWD_XMMdq_XMMq" | "x86:PUNPCKLDQ_XMMdq_XMMq" | "x86:PUNPCKHDQ_XMMdq_XMMq" ),
      ( "PUNPCKLBW_XMMdq_XMMq" | "PUNPCKHBW_XMMdq_XMMq" | "PUNPCKLWD_XMMdq_XMMq"
      | "PUNPCKHWD_XMMdq_XMMq" | "PUNPCKLDQ_XMMdq_XMMq" | "PUNPCKHDQ_XMMdq_XMMq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PUNPCKLBW_XMMdq_MEMdq" | "x86:PUNPCKHBW_XMMdq_MEMdq" | "x86:PUNPCKLWD_XMMdq_MEMdq"
      | "x86:PUNPCKHWD_XMMdq_MEMdq" | "x86:PUNPCKLDQ_XMMdq_MEMdq" | "x86:PUNPCKHDQ_XMMdq_MEMdq" ),
      ( "PUNPCKLBW_XMMdq_MEMdq" | "PUNPCKHBW_XMMdq_MEMdq" | "PUNPCKLWD_XMMdq_MEMdq"
      | "PUNPCKHWD_XMMdq_MEMdq" | "PUNPCKLDQ_XMMdq_MEMdq" | "PUNPCKHDQ_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PADDB_XMMdq_XMMdq" | "x86:PADDW_XMMdq_XMMdq" | "x86:PADDD_XMMdq_XMMdq"
      | "x86:PADDQ_XMMdq_XMMdq" | "x86:PSUBB_XMMdq_XMMdq" | "x86:PSUBW_XMMdq_XMMdq"
      | "x86:PSUBD_XMMdq_XMMdq" | "x86:PSUBQ_XMMdq_XMMdq" ),
      ( "PADDB_XMMdq_XMMdq" | "PADDW_XMMdq_XMMdq" | "PADDD_XMMdq_XMMdq" | "PADDQ_XMMdq_XMMdq"
      | "PSUBB_XMMdq_XMMdq" | "PSUBW_XMMdq_XMMdq" | "PSUBD_XMMdq_XMMdq" | "PSUBQ_XMMdq_XMMdq" ) ) ->
      true
  | _ -> false

let promoted_case_part2 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PADDB_XMMdq_MEMdq" | "x86:PADDW_XMMdq_MEMdq" | "x86:PADDD_XMMdq_MEMdq"
      | "x86:PADDQ_XMMdq_MEMdq" | "x86:PSUBB_XMMdq_MEMdq" | "x86:PSUBW_XMMdq_MEMdq"
      | "x86:PSUBD_XMMdq_MEMdq" | "x86:PSUBQ_XMMdq_MEMdq" ),
      ( "PADDB_XMMdq_MEMdq" | "PADDW_XMMdq_MEMdq" | "PADDD_XMMdq_MEMdq" | "PADDQ_XMMdq_MEMdq"
      | "PSUBB_XMMdq_MEMdq" | "PSUBW_XMMdq_MEMdq" | "PSUBD_XMMdq_MEMdq" | "PSUBQ_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PCMPEQB_XMMdq_XMMdq" | "x86:PCMPEQW_XMMdq_XMMdq" | "x86:PCMPEQD_XMMdq_XMMdq"
      | "x86:PCMPGTB_XMMdq_XMMdq" | "x86:PCMPGTW_XMMdq_XMMdq" | "x86:PCMPGTD_XMMdq_XMMdq" ),
      ( "PCMPEQB_XMMdq_XMMdq" | "PCMPEQW_XMMdq_XMMdq" | "PCMPEQD_XMMdq_XMMdq"
      | "PCMPGTB_XMMdq_XMMdq" | "PCMPGTW_XMMdq_XMMdq" | "PCMPGTD_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PCMPEQB_XMMdq_MEMdq" | "x86:PCMPEQW_XMMdq_MEMdq" | "x86:PCMPEQD_XMMdq_MEMdq"
      | "x86:PCMPGTB_XMMdq_MEMdq" | "x86:PCMPGTW_XMMdq_MEMdq" | "x86:PCMPGTD_XMMdq_MEMdq" ),
      ( "PCMPEQB_XMMdq_MEMdq" | "PCMPEQW_XMMdq_MEMdq" | "PCMPEQD_XMMdq_MEMdq"
      | "PCMPGTB_XMMdq_MEMdq" | "PCMPGTW_XMMdq_MEMdq" | "PCMPGTD_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PACKSSWB_XMMdq_XMMdq" | "x86:PACKSSDW_XMMdq_XMMdq" | "x86:PACKUSWB_XMMdq_XMMdq"),
      ("PACKSSWB_XMMdq_XMMdq" | "PACKSSDW_XMMdq_XMMdq" | "PACKUSWB_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PACKSSWB_XMMdq_MEMdq" | "x86:PACKSSDW_XMMdq_MEMdq" | "x86:PACKUSWB_XMMdq_MEMdq"),
      ("PACKSSWB_XMMdq_MEMdq" | "PACKSSDW_XMMdq_MEMdq" | "PACKUSWB_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PAND_XMMdq_XMMdq" | "x86:PANDN_XMMdq_XMMdq" | "x86:POR_XMMdq_XMMdq"),
      ("PAND_XMMdq_XMMdq" | "PANDN_XMMdq_XMMdq" | "POR_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PAND_XMMdq_MEMdq" | "x86:PANDN_XMMdq_MEMdq" | "x86:POR_XMMdq_MEMdq"),
      ("PAND_XMMdq_MEMdq" | "PANDN_XMMdq_MEMdq" | "POR_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PMINUB_XMMdq_XMMdq" | "x86:PMAXUB_XMMdq_XMMdq" | "x86:PMINSW_XMMdq_XMMdq"
      | "x86:PMAXSW_XMMdq_XMMdq" ),
      ("PMINUB_XMMdq_XMMdq" | "PMAXUB_XMMdq_XMMdq" | "PMINSW_XMMdq_XMMdq" | "PMAXSW_XMMdq_XMMdq") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PMINUB_XMMdq_MEMdq" | "x86:PMAXUB_XMMdq_MEMdq" | "x86:PMINSW_XMMdq_MEMdq"
      | "x86:PMAXSW_XMMdq_MEMdq" ),
      ("PMINUB_XMMdq_MEMdq" | "PMAXUB_XMMdq_MEMdq" | "PMINSW_XMMdq_MEMdq" | "PMAXSW_XMMdq_MEMdq") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PMULLW_XMMdq_XMMdq" | "x86:PMULHW_XMMdq_XMMdq" | "x86:PMULHUW_XMMdq_XMMdq"
      | "x86:PAVGB_XMMdq_XMMdq" | "x86:PAVGW_XMMdq_XMMdq" | "x86:PSADBW_XMMdq_XMMdq" ),
      ( "PMULLW_XMMdq_XMMdq" | "PMULHW_XMMdq_XMMdq" | "PMULHUW_XMMdq_XMMdq" | "PAVGB_XMMdq_XMMdq"
      | "PAVGW_XMMdq_XMMdq" | "PSADBW_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PMULLW_XMMdq_MEMdq" | "x86:PMULHW_XMMdq_MEMdq" | "x86:PMULHUW_XMMdq_MEMdq"
      | "x86:PAVGB_XMMdq_MEMdq" | "x86:PAVGW_XMMdq_MEMdq" | "x86:PSADBW_XMMdq_MEMdq" ),
      ( "PMULLW_XMMdq_MEMdq" | "PMULHW_XMMdq_MEMdq" | "PMULHUW_XMMdq_MEMdq" | "PAVGB_XMMdq_MEMdq"
      | "PAVGW_XMMdq_MEMdq" | "PSADBW_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PSLLW_XMMdq_XMMdq" | "x86:PSLLD_XMMdq_XMMdq" | "x86:PSLLQ_XMMdq_XMMdq"
      | "x86:PSRLW_XMMdq_XMMdq" | "x86:PSRLD_XMMdq_XMMdq" | "x86:PSRLQ_XMMdq_XMMdq"
      | "x86:PSRAW_XMMdq_XMMdq" | "x86:PSRAD_XMMdq_XMMdq" ),
      ( "PSLLW_XMMdq_XMMdq" | "PSLLD_XMMdq_XMMdq" | "PSLLQ_XMMdq_XMMdq" | "PSRLW_XMMdq_XMMdq"
      | "PSRLD_XMMdq_XMMdq" | "PSRLQ_XMMdq_XMMdq" | "PSRAW_XMMdq_XMMdq" | "PSRAD_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PSLLW_XMMdq_MEMdq" | "x86:PSLLD_XMMdq_MEMdq" | "x86:PSLLQ_XMMdq_MEMdq"
      | "x86:PSRLW_XMMdq_MEMdq" | "x86:PSRLD_XMMdq_MEMdq" | "x86:PSRLQ_XMMdq_MEMdq"
      | "x86:PSRAW_XMMdq_MEMdq" | "x86:PSRAD_XMMdq_MEMdq" ),
      ( "PSLLW_XMMdq_MEMdq" | "PSLLD_XMMdq_MEMdq" | "PSLLQ_XMMdq_MEMdq" | "PSRLW_XMMdq_MEMdq"
      | "PSRLD_XMMdq_MEMdq" | "PSRLQ_XMMdq_MEMdq" | "PSRAW_XMMdq_MEMdq" | "PSRAD_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PSLLW_XMMdq_IMMb" | "x86:PSLLD_XMMdq_IMMb" | "x86:PSLLQ_XMMdq_IMMb"
      | "x86:PSRLW_XMMdq_IMMb" | "x86:PSRLD_XMMdq_IMMb" | "x86:PSRLQ_XMMdq_IMMb"
      | "x86:PSRAW_XMMdq_IMMb" | "x86:PSRAD_XMMdq_IMMb" ),
      ( "PSLLW_XMMdq_IMMb" | "PSLLD_XMMdq_IMMb" | "PSLLQ_XMMdq_IMMb" | "PSRLW_XMMdq_IMMb"
      | "PSRLD_XMMdq_IMMb" | "PSRLQ_XMMdq_IMMb" | "PSRAW_XMMdq_IMMb" | "PSRAD_XMMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PSLLDQ_XMMdq_IMMb" | "x86:PSRLDQ_XMMdq_IMMb"),
      ("PSLLDQ_XMMdq_IMMb" | "PSRLDQ_XMMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PSHUFB_XMMdq_XMMdq" | "x86:PSHUFB_XMMdq_MEMdq"),
      ("PSHUFB_XMMdq_XMMdq" | "PSHUFB_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PHADDW_XMMdq_XMMdq" | "x86:PHADDD_XMMdq_XMMdq" | "x86:PHSUBW_XMMdq_XMMdq"
      | "x86:PHSUBD_XMMdq_XMMdq" | "x86:PSIGNB_XMMdq_XMMdq" | "x86:PSIGNW_XMMdq_XMMdq"
      | "x86:PSIGND_XMMdq_XMMdq" | "x86:PMADDUBSW_XMMdq_XMMdq" | "x86:PMULHRSW_XMMdq_XMMdq" ),
      ( "PHADDW_XMMdq_XMMdq" | "PHADDD_XMMdq_XMMdq" | "PHSUBW_XMMdq_XMMdq" | "PHSUBD_XMMdq_XMMdq"
      | "PSIGNB_XMMdq_XMMdq" | "PSIGNW_XMMdq_XMMdq" | "PSIGND_XMMdq_XMMdq" | "PMADDUBSW_XMMdq_XMMdq"
      | "PMULHRSW_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PHADDSW_XMMdq_XMMdq" | "x86:PHSUBSW_XMMdq_XMMdq"),
      ("PHADDSW_XMMdq_XMMdq" | "PHSUBSW_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PHADDSW_XMMdq_MEMdq" | "x86:PHSUBSW_XMMdq_MEMdq"),
      ("PHADDSW_XMMdq_MEMdq" | "PHSUBSW_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PABSB_XMMdq_XMMdq" | "x86:PABSW_XMMdq_XMMdq" | "x86:PABSD_XMMdq_XMMdq"),
      ("PABSB_XMMdq_XMMdq" | "PABSW_XMMdq_XMMdq" | "PABSD_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PABSB_XMMdq_MEMdq" | "x86:PABSW_XMMdq_MEMdq" | "x86:PABSD_XMMdq_MEMdq"),
      ("PABSB_XMMdq_MEMdq" | "PABSW_XMMdq_MEMdq" | "PABSD_XMMdq_MEMdq") ) ->
      true
  | (Target.X86_32 | Target.X86_64), "x86:PALIGNR_XMMdq_XMMdq_IMMb", "PALIGNR_XMMdq_XMMdq_IMMb" ->
      true
  | (Target.X86_32 | Target.X86_64), "x86:PALIGNR_XMMdq_MEMdq_IMMb", "PALIGNR_XMMdq_MEMdq_IMMb" ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ROUNDPS_XMMps_XMMps_IMMb" | "x86:ROUNDPD_XMMpd_XMMpd_IMMb"
      | "x86:ROUNDSS_XMMd_XMMd_IMMb" | "x86:ROUNDSD_XMMq_XMMq_IMMb" ),
      ( "ROUNDPS_XMMps_XMMps_IMMb" | "ROUNDPD_XMMpd_XMMpd_IMMb" | "ROUNDSS_XMMd_XMMd_IMMb"
      | "ROUNDSD_XMMq_XMMq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:ROUNDPS_XMMps_MEMps_IMMb" | "x86:ROUNDPD_XMMpd_MEMpd_IMMb"
      | "x86:ROUNDSS_XMMd_MEMd_IMMb" | "x86:ROUNDSD_XMMq_MEMq_IMMb" ),
      ( "ROUNDPS_XMMps_MEMps_IMMb" | "ROUNDPD_XMMpd_MEMpd_IMMb" | "ROUNDSS_XMMd_MEMd_IMMb"
      | "ROUNDSD_XMMq_MEMq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PCMPEQQ_XMMdq_XMMdq" | "x86:PCMPGTQ_XMMdq_XMMdq" | "x86:PACKUSDW_XMMdq_XMMdq"
      | "x86:PMAXSB_XMMdq_XMMdq" | "x86:PMAXSD_XMMdq_XMMdq" | "x86:PMAXUD_XMMdq_XMMdq"
      | "x86:PMAXUW_XMMdq_XMMdq" | "x86:PMINSB_XMMdq_XMMdq" | "x86:PMINSD_XMMdq_XMMdq"
      | "x86:PMINUD_XMMdq_XMMdq" | "x86:PMINUW_XMMdq_XMMdq" | "x86:PMULDQ_XMMdq_XMMdq"
      | "x86:PMULLD_XMMdq_XMMdq" | "x86:PHMINPOSUW_XMMdq_XMMdq" ),
      ( "PCMPEQQ_XMMdq_XMMdq" | "PCMPGTQ_XMMdq_XMMdq" | "PACKUSDW_XMMdq_XMMdq"
      | "PMAXSB_XMMdq_XMMdq" | "PMAXSD_XMMdq_XMMdq" | "PMAXUD_XMMdq_XMMdq" | "PMAXUW_XMMdq_XMMdq"
      | "PMINSB_XMMdq_XMMdq" | "PMINSD_XMMdq_XMMdq" | "PMINUD_XMMdq_XMMdq" | "PMINUW_XMMdq_XMMdq"
      | "PMULDQ_XMMdq_XMMdq" | "PMULLD_XMMdq_XMMdq" | "PHMINPOSUW_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PCMPEQQ_XMMdq_MEMdq" | "x86:PCMPGTQ_XMMdq_MEMdq" | "x86:PACKUSDW_XMMdq_MEMdq"
      | "x86:PMAXSB_XMMdq_MEMdq" | "x86:PMAXSD_XMMdq_MEMdq" | "x86:PMAXUD_XMMdq_MEMdq"
      | "x86:PMAXUW_XMMdq_MEMdq" | "x86:PMINSB_XMMdq_MEMdq" | "x86:PMINSD_XMMdq_MEMdq"
      | "x86:PMINUD_XMMdq_MEMdq" | "x86:PMINUW_XMMdq_MEMdq" | "x86:PMULDQ_XMMdq_MEMdq"
      | "x86:PMULLD_XMMdq_MEMdq" | "x86:PHMINPOSUW_XMMdq_MEMdq" ),
      ( "PCMPEQQ_XMMdq_MEMdq" | "PCMPGTQ_XMMdq_MEMdq" | "PACKUSDW_XMMdq_MEMdq"
      | "PMAXSB_XMMdq_MEMdq" | "PMAXSD_XMMdq_MEMdq" | "PMAXUD_XMMdq_MEMdq" | "PMAXUW_XMMdq_MEMdq"
      | "PMINSB_XMMdq_MEMdq" | "PMINSD_XMMdq_MEMdq" | "PMINUD_XMMdq_MEMdq" | "PMINUW_XMMdq_MEMdq"
      | "PMULDQ_XMMdq_MEMdq" | "PMULLD_XMMdq_MEMdq" | "PHMINPOSUW_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PTEST_XMMdq_XMMdq" | "x86:PMOVSXBW_XMMdq_XMMq" | "x86:PMOVSXBD_XMMdq_XMMd"
      | "x86:PMOVSXBQ_XMMdq_XMMw" | "x86:PMOVSXWD_XMMdq_XMMq" | "x86:PMOVSXWQ_XMMdq_XMMd"
      | "x86:PMOVSXDQ_XMMdq_XMMq" | "x86:PMOVZXBW_XMMdq_XMMq" | "x86:PMOVZXBD_XMMdq_XMMd"
      | "x86:PMOVZXBQ_XMMdq_XMMw" | "x86:PMOVZXWD_XMMdq_XMMq" | "x86:PMOVZXWQ_XMMdq_XMMd"
      | "x86:PMOVZXDQ_XMMdq_XMMq" ),
      ( "PTEST_XMMdq_XMMdq" | "PMOVSXBW_XMMdq_XMMq" | "PMOVSXBD_XMMdq_XMMd" | "PMOVSXBQ_XMMdq_XMMw"
      | "PMOVSXWD_XMMdq_XMMq" | "PMOVSXWQ_XMMdq_XMMd" | "PMOVSXDQ_XMMdq_XMMq"
      | "PMOVZXBW_XMMdq_XMMq" | "PMOVZXBD_XMMdq_XMMd" | "PMOVZXBQ_XMMdq_XMMw"
      | "PMOVZXWD_XMMdq_XMMq" | "PMOVZXWQ_XMMdq_XMMd" | "PMOVZXDQ_XMMdq_XMMq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PTEST_XMMdq_MEMdq" | "x86:PMOVSXBW_XMMdq_MEMq" | "x86:PMOVSXBD_XMMdq_MEMd"
      | "x86:PMOVSXBQ_XMMdq_MEMw" | "x86:PMOVSXWD_XMMdq_MEMq" | "x86:PMOVSXWQ_XMMdq_MEMd"
      | "x86:PMOVSXDQ_XMMdq_MEMq" | "x86:PMOVZXBW_XMMdq_MEMq" | "x86:PMOVZXBD_XMMdq_MEMd"
      | "x86:PMOVZXBQ_XMMdq_MEMw" | "x86:PMOVZXWD_XMMdq_MEMq" | "x86:PMOVZXWQ_XMMdq_MEMd"
      | "x86:PMOVZXDQ_XMMdq_MEMq" | "x86:MOVNTDQA_XMMdq_MEMdq" ),
      ( "PTEST_XMMdq_MEMdq" | "PMOVSXBW_XMMdq_MEMq" | "PMOVSXBD_XMMdq_MEMd" | "PMOVSXBQ_XMMdq_MEMw"
      | "PMOVSXWD_XMMdq_MEMq" | "PMOVSXWQ_XMMdq_MEMd" | "PMOVSXDQ_XMMdq_MEMq"
      | "PMOVZXBW_XMMdq_MEMq" | "PMOVZXBD_XMMdq_MEMd" | "PMOVZXBQ_XMMdq_MEMw"
      | "PMOVZXWD_XMMdq_MEMq" | "PMOVZXWQ_XMMdq_MEMd" | "PMOVZXDQ_XMMdq_MEMq"
      | "MOVNTDQA_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:BLENDPS_XMMdq_XMMdq_IMMb" | "x86:BLENDPD_XMMdq_XMMdq_IMMb"
      | "x86:DPPS_XMMdq_XMMdq_IMMb" | "x86:DPPD_XMMdq_XMMdq_IMMb" | "x86:MPSADBW_XMMdq_XMMdq_IMMb"
      | "x86:PBLENDW_XMMdq_XMMdq_IMMb" ),
      ( "BLENDPS_XMMdq_XMMdq_IMMb" | "BLENDPD_XMMdq_XMMdq_IMMb" | "DPPS_XMMdq_XMMdq_IMMb"
      | "DPPD_XMMdq_XMMdq_IMMb" | "MPSADBW_XMMdq_XMMdq_IMMb" | "PBLENDW_XMMdq_XMMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:BLENDVPS_XMMdq_XMMdq" | "x86:BLENDVPS_XMMdq_MEMdq" | "x86:BLENDVPD_XMMdq_XMMdq"
      | "x86:BLENDVPD_XMMdq_MEMdq" | "x86:PBLENDVB_XMMdq_XMMdq" | "x86:PBLENDVB_XMMdq_MEMdq" ),
      ( "BLENDVPS_XMMdq_XMMdq" | "BLENDVPS_XMMdq_MEMdq" | "BLENDVPD_XMMdq_XMMdq"
      | "BLENDVPD_XMMdq_MEMdq" | "PBLENDVB_XMMdq_XMMdq" | "PBLENDVB_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:PEXTRB_MEMb_XMMdq_IMMb" | "x86:PEXTRD_MEMd_XMMdq_IMMb" | "x86:EXTRACTPS_MEMd_XMMps_IMMb"),
      ("PEXTRB_MEMb_XMMdq_IMMb" | "PEXTRD_MEMd_XMMdq_IMMb" | "EXTRACTPS_MEMd_XMMps_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PINSRB_XMMdq_GPR32d_IMMb" | "x86:PINSRD_XMMdq_GPR32d_IMMb"
      | "x86:PINSRB_XMMdq_MEMb_IMMb" | "x86:PINSRD_XMMdq_MEMd_IMMb" | "x86:PEXTRB_GPR32d_XMMdq_IMMb"
      | "x86:PEXTRD_GPR32d_XMMdq_IMMb" | "x86:EXTRACTPS_GPR32d_XMMdq_IMMb" ),
      ( "PINSRB_XMMdq_GPR32d_IMMb" | "PINSRD_XMMdq_GPR32d_IMMb" | "PINSRB_XMMdq_MEMb_IMMb"
      | "PINSRD_XMMdq_MEMd_IMMb" | "PEXTRB_GPR32d_XMMdq_IMMb" | "PEXTRD_GPR32d_XMMdq_IMMb"
      | "EXTRACTPS_GPR32d_XMMdq_IMMb" ) ) ->
      true
  | (Target.X86_32 | Target.X86_64), "x86:INSERTPS_XMMps_XMMps_IMMb", "INSERTPS_XMMps_XMMps_IMMb" ->
      true
  | (Target.X86_32 | Target.X86_64), "x86:INSERTPS_XMMps_MEMd_IMMb", "INSERTPS_XMMps_MEMd_IMMb" ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:BLENDPS_XMMdq_MEMdq_IMMb" | "x86:BLENDPD_XMMdq_MEMdq_IMMb"
      | "x86:DPPS_XMMdq_MEMdq_IMMb" | "x86:DPPD_XMMdq_MEMdq_IMMb" | "x86:MPSADBW_XMMdq_MEMdq_IMMb"
      | "x86:PBLENDW_XMMdq_MEMdq_IMMb" ),
      ( "BLENDPS_XMMdq_MEMdq_IMMb" | "BLENDPD_XMMdq_MEMdq_IMMb" | "DPPS_XMMdq_MEMdq_IMMb"
      | "DPPD_XMMdq_MEMdq_IMMb" | "MPSADBW_XMMdq_MEMdq_IMMb" | "PBLENDW_XMMdq_MEMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PHADDW_XMMdq_MEMdq" | "x86:PHADDD_XMMdq_MEMdq" | "x86:PHSUBW_XMMdq_MEMdq"
      | "x86:PHSUBD_XMMdq_MEMdq" | "x86:PSIGNB_XMMdq_MEMdq" | "x86:PSIGNW_XMMdq_MEMdq"
      | "x86:PSIGND_XMMdq_MEMdq" | "x86:PMADDUBSW_XMMdq_MEMdq" | "x86:PMULHRSW_XMMdq_MEMdq" ),
      ( "PHADDW_XMMdq_MEMdq" | "PHADDD_XMMdq_MEMdq" | "PHSUBW_XMMdq_MEMdq" | "PHSUBD_XMMdq_MEMdq"
      | "PSIGNB_XMMdq_MEMdq" | "PSIGNW_XMMdq_MEMdq" | "PSIGND_XMMdq_MEMdq" | "PMADDUBSW_XMMdq_MEMdq"
      | "PMULHRSW_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:MOVDQA_XMMdq_XMMdq_0F6F" | "x86:MOVDQU_XMMdq_XMMdq_0F6F" | "x86:MOVDQA_XMMdq_MEMdq"
      | "x86:MOVDQU_XMMdq_MEMdq" | "x86:MOVDQA_MEMdq_XMMdq" | "x86:MOVDQU_MEMdq_XMMdq" ),
      ( "MOVDQA_XMMdq_XMMdq_0F6F" | "MOVDQU_XMMdq_XMMdq_0F6F" | "MOVDQA_XMMdq_MEMdq"
      | "MOVDQU_XMMdq_MEMdq" | "MOVDQA_MEMdq_XMMdq" | "MOVDQU_MEMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:SHUFPS_XMMps_XMMps_IMMb" | "x86:SHUFPD_XMMpd_XMMpd_IMMb"),
      ("SHUFPS_XMMps_XMMps_IMMb" | "SHUFPD_XMMpd_XMMpd_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:SHUFPS_XMMps_MEMps_IMMb" | "x86:SHUFPD_XMMpd_MEMpd_IMMb"),
      ("SHUFPS_XMMps_MEMps_IMMb" | "SHUFPD_XMMpd_MEMpd_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:CMPSS_XMMss_XMMss_IMMb" | "x86:CMPSD_XMM_XMMsd_XMMsd_IMMb"
      | "x86:CMPPS_XMMps_XMMps_IMMb" | "x86:CMPPD_XMMpd_XMMpd_IMMb" ),
      ( "CMPSS_XMMss_XMMss_IMMb" | "CMPSD_XMM_XMMsd_XMMsd_IMMb" | "CMPPS_XMMps_XMMps_IMMb"
      | "CMPPD_XMMpd_XMMpd_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:CMPSS_XMMss_MEMss_IMMb" | "x86:CMPSD_XMM_XMMsd_MEMsd_IMMb"
      | "x86:CMPPS_XMMps_MEMps_IMMb" | "x86:CMPPD_XMMpd_MEMpd_IMMb" ),
      ( "CMPSS_XMMss_MEMss_IMMb" | "CMPSD_XMM_XMMsd_MEMsd_IMMb" | "CMPPS_XMMps_MEMps_IMMb"
      | "CMPPD_XMMpd_MEMpd_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PSHUFD_XMMdq_XMMdq_IMMb" | "x86:PSHUFLW_XMMdq_XMMdq_IMMb"
      | "x86:PSHUFHW_XMMdq_XMMdq_IMMb" ),
      ("PSHUFD_XMMdq_XMMdq_IMMb" | "PSHUFLW_XMMdq_XMMdq_IMMb" | "PSHUFHW_XMMdq_XMMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:PSHUFD_XMMdq_MEMdq_IMMb" | "x86:PSHUFLW_XMMdq_MEMdq_IMMb"
      | "x86:PSHUFHW_XMMdq_MEMdq_IMMb" ),
      ("PSHUFD_XMMdq_MEMdq_IMMb" | "PSHUFLW_XMMdq_MEMdq_IMMb" | "PSHUFHW_XMMdq_MEMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDSD_XMMdq_XMMdq_XMMq" | "x86:VSUBSD_XMMdq_XMMdq_XMMq"
      | "x86:VMULSD_XMMdq_XMMdq_XMMq" | "x86:VDIVSD_XMMdq_XMMdq_XMMq"
      | "x86:VADDSS_XMMdq_XMMdq_XMMd" | "x86:VSUBSS_XMMdq_XMMdq_XMMd"
      | "x86:VMULSS_XMMdq_XMMdq_XMMd" | "x86:VDIVSS_XMMdq_XMMdq_XMMd" ),
      ( "VADDSD_XMMdq_XMMdq_XMMq" | "VSUBSD_XMMdq_XMMdq_XMMq" | "VMULSD_XMMdq_XMMdq_XMMq"
      | "VDIVSD_XMMdq_XMMdq_XMMq" | "VADDSS_XMMdq_XMMdq_XMMd" | "VSUBSS_XMMdq_XMMdq_XMMd"
      | "VMULSS_XMMdq_XMMdq_XMMd" | "VDIVSS_XMMdq_XMMdq_XMMd" ) ) ->
      true
  | _ -> false

let promoted_case_part3 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDSD_XMMdq_XMMdq_MEMq" | "x86:VSUBSD_XMMdq_XMMdq_MEMq"
      | "x86:VMULSD_XMMdq_XMMdq_MEMq" | "x86:VDIVSD_XMMdq_XMMdq_MEMq"
      | "x86:VADDSS_XMMdq_XMMdq_MEMd" | "x86:VSUBSS_XMMdq_XMMdq_MEMd"
      | "x86:VMULSS_XMMdq_XMMdq_MEMd" | "x86:VDIVSS_XMMdq_XMMdq_MEMd" ),
      ( "VADDSD_XMMdq_XMMdq_MEMq" | "VSUBSD_XMMdq_XMMdq_MEMq" | "VMULSD_XMMdq_XMMdq_MEMq"
      | "VDIVSD_XMMdq_XMMdq_MEMq" | "VADDSS_XMMdq_XMMdq_MEMd" | "VSUBSS_XMMdq_XMMdq_MEMd"
      | "VMULSS_XMMdq_XMMdq_MEMd" | "VDIVSS_XMMdq_XMMdq_MEMd" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDPS_XMMdq_XMMdq_XMMdq" | "x86:VSUBPS_XMMdq_XMMdq_XMMdq"
      | "x86:VMULPS_XMMdq_XMMdq_XMMdq" | "x86:VDIVPS_XMMdq_XMMdq_XMMdq"
      | "x86:VADDPD_XMMdq_XMMdq_XMMdq" | "x86:VSUBPD_XMMdq_XMMdq_XMMdq"
      | "x86:VMULPD_XMMdq_XMMdq_XMMdq" | "x86:VDIVPD_XMMdq_XMMdq_XMMdq" ),
      ( "VADDPS_XMMdq_XMMdq_XMMdq" | "VSUBPS_XMMdq_XMMdq_XMMdq" | "VMULPS_XMMdq_XMMdq_XMMdq"
      | "VDIVPS_XMMdq_XMMdq_XMMdq" | "VADDPD_XMMdq_XMMdq_XMMdq" | "VSUBPD_XMMdq_XMMdq_XMMdq"
      | "VMULPD_XMMdq_XMMdq_XMMdq" | "VDIVPD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDPS_XMMdq_XMMdq_MEMdq" | "x86:VSUBPS_XMMdq_XMMdq_MEMdq"
      | "x86:VMULPS_XMMdq_XMMdq_MEMdq" | "x86:VDIVPS_XMMdq_XMMdq_MEMdq"
      | "x86:VADDPD_XMMdq_XMMdq_MEMdq" | "x86:VSUBPD_XMMdq_XMMdq_MEMdq"
      | "x86:VMULPD_XMMdq_XMMdq_MEMdq" | "x86:VDIVPD_XMMdq_XMMdq_MEMdq" ),
      ( "VADDPS_XMMdq_XMMdq_MEMdq" | "VSUBPS_XMMdq_XMMdq_MEMdq" | "VMULPS_XMMdq_XMMdq_MEMdq"
      | "VDIVPS_XMMdq_XMMdq_MEMdq" | "VADDPD_XMMdq_XMMdq_MEMdq" | "VSUBPD_XMMdq_XMMdq_MEMdq"
      | "VMULPD_XMMdq_XMMdq_MEMdq" | "VDIVPD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VANDPS_XMMdq_XMMdq_XMMdq" | "x86:VANDNPS_XMMdq_XMMdq_XMMdq"
      | "x86:VORPS_XMMdq_XMMdq_XMMdq" | "x86:VXORPS_XMMdq_XMMdq_XMMdq"
      | "x86:VANDPD_XMMdq_XMMdq_XMMdq" | "x86:VANDNPD_XMMdq_XMMdq_XMMdq"
      | "x86:VORPD_XMMdq_XMMdq_XMMdq" | "x86:VXORPD_XMMdq_XMMdq_XMMdq" ),
      ( "VANDPS_XMMdq_XMMdq_XMMdq" | "VANDNPS_XMMdq_XMMdq_XMMdq" | "VORPS_XMMdq_XMMdq_XMMdq"
      | "VXORPS_XMMdq_XMMdq_XMMdq" | "VANDPD_XMMdq_XMMdq_XMMdq" | "VANDNPD_XMMdq_XMMdq_XMMdq"
      | "VORPD_XMMdq_XMMdq_XMMdq" | "VXORPD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VANDPS_XMMdq_XMMdq_MEMdq" | "x86:VANDNPS_XMMdq_XMMdq_MEMdq"
      | "x86:VORPS_XMMdq_XMMdq_MEMdq" | "x86:VXORPS_XMMdq_XMMdq_MEMdq"
      | "x86:VANDPD_XMMdq_XMMdq_MEMdq" | "x86:VANDNPD_XMMdq_XMMdq_MEMdq"
      | "x86:VORPD_XMMdq_XMMdq_MEMdq" | "x86:VXORPD_XMMdq_XMMdq_MEMdq" ),
      ( "VANDPS_XMMdq_XMMdq_MEMdq" | "VANDNPS_XMMdq_XMMdq_MEMdq" | "VORPS_XMMdq_XMMdq_MEMdq"
      | "VXORPS_XMMdq_XMMdq_MEMdq" | "VANDPD_XMMdq_XMMdq_MEMdq" | "VANDNPD_XMMdq_XMMdq_MEMdq"
      | "VORPD_XMMdq_XMMdq_MEMdq" | "VXORPD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMAXSD_XMMdq_XMMdq_XMMq" | "x86:VMINSD_XMMdq_XMMdq_XMMq"
      | "x86:VMAXSS_XMMdq_XMMdq_XMMd" | "x86:VMINSS_XMMdq_XMMdq_XMMd"
      | "x86:VMAXPS_XMMdq_XMMdq_XMMdq" | "x86:VMINPS_XMMdq_XMMdq_XMMdq"
      | "x86:VMAXPD_XMMdq_XMMdq_XMMdq" | "x86:VMINPD_XMMdq_XMMdq_XMMdq" ),
      ( "VMAXSD_XMMdq_XMMdq_XMMq" | "VMINSD_XMMdq_XMMdq_XMMq" | "VMAXSS_XMMdq_XMMdq_XMMd"
      | "VMINSS_XMMdq_XMMdq_XMMd" | "VMAXPS_XMMdq_XMMdq_XMMdq" | "VMINPS_XMMdq_XMMdq_XMMdq"
      | "VMAXPD_XMMdq_XMMdq_XMMdq" | "VMINPD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMAXSD_XMMdq_XMMdq_MEMq" | "x86:VMINSD_XMMdq_XMMdq_MEMq"
      | "x86:VMAXSS_XMMdq_XMMdq_MEMd" | "x86:VMINSS_XMMdq_XMMdq_MEMd"
      | "x86:VMAXPS_XMMdq_XMMdq_MEMdq" | "x86:VMINPS_XMMdq_XMMdq_MEMdq"
      | "x86:VMAXPD_XMMdq_XMMdq_MEMdq" | "x86:VMINPD_XMMdq_XMMdq_MEMdq" ),
      ( "VMAXSD_XMMdq_XMMdq_MEMq" | "VMINSD_XMMdq_XMMdq_MEMq" | "VMAXSS_XMMdq_XMMdq_MEMd"
      | "VMINSS_XMMdq_XMMdq_MEMd" | "VMAXPS_XMMdq_XMMdq_MEMdq" | "VMINPS_XMMdq_XMMdq_MEMdq"
      | "VMAXPD_XMMdq_XMMdq_MEMdq" | "VMINPD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSQRTSD_XMMdq_XMMdq_XMMq" | "x86:VSQRTSS_XMMdq_XMMdq_XMMd"),
      ("VSQRTSD_XMMdq_XMMdq_XMMq" | "VSQRTSS_XMMdq_XMMdq_XMMd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSQRTSD_XMMdq_XMMdq_MEMq" | "x86:VSQRTSS_XMMdq_XMMdq_MEMd"),
      ("VSQRTSD_XMMdq_XMMdq_MEMq" | "VSQRTSS_XMMdq_XMMdq_MEMd") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSQRTPS_XMMdq_XMMdq" | "x86:VSQRTPD_XMMdq_XMMdq"),
      ("VSQRTPS_XMMdq_XMMdq" | "VSQRTPD_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSQRTPS_XMMdq_MEMdq" | "x86:VSQRTPD_XMMdq_MEMdq"),
      ("VSQRTPS_XMMdq_MEMdq" | "VSQRTPD_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMOVAPS_XMMdq_XMMdq_28" | "x86:VMOVUPS_XMMdq_XMMdq_10" | "x86:VMOVAPD_XMMdq_XMMdq_28"
      | "x86:VMOVUPD_XMMdq_XMMdq_10" ),
      ( "VMOVAPS_XMMdq_XMMdq_28" | "VMOVUPS_XMMdq_XMMdq_10" | "VMOVAPD_XMMdq_XMMdq_28"
      | "VMOVUPD_XMMdq_XMMdq_10" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMOVAPS_XMMdq_MEMdq" | "x86:VMOVUPS_XMMdq_MEMdq" | "x86:VMOVAPD_XMMdq_MEMdq"
      | "x86:VMOVUPD_XMMdq_MEMdq" ),
      ("VMOVAPS_XMMdq_MEMdq" | "VMOVUPS_XMMdq_MEMdq" | "VMOVAPD_XMMdq_MEMdq" | "VMOVUPD_XMMdq_MEMdq")
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VCOMISD_XMMq_XMMq" | "x86:VUCOMISD_XMMdq_XMMq" | "x86:VCOMISS_XMMd_XMMd"
      | "x86:VUCOMISS_XMMdq_XMMd" ),
      ("VCOMISD_XMMq_XMMq" | "VUCOMISD_XMMdq_XMMq" | "VCOMISS_XMMd_XMMd" | "VUCOMISS_XMMdq_XMMd") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VCOMISD_XMMq_MEMq" | "x86:VUCOMISD_XMMdq_MEMq" | "x86:VCOMISS_XMMd_MEMd"
      | "x86:VUCOMISS_XMMdq_MEMd" ),
      ("VCOMISD_XMMq_MEMq" | "VUCOMISD_XMMdq_MEMq" | "VCOMISS_XMMd_MEMd" | "VUCOMISS_XMMdq_MEMd") )
    ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VCVTPS2PD_XMMdq_XMMq" | "x86:VCVTPD2PS_XMMdq_XMMdq"),
      ("VCVTPS2PD_XMMdq_XMMq" | "VCVTPD2PS_XMMdq_XMMdq") ) ->
      true
  | (Target.X86_32 | Target.X86_64), "x86:VCVTPS2PD_XMMdq_MEMq", "VCVTPS2PD_XMMdq_MEMq" -> true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VCVTDQ2PS_XMMdq_XMMdq" | "x86:VCVTPS2DQ_XMMdq_XMMdq" | "x86:VCVTTPS2DQ_XMMdq_XMMdq"),
      ("VCVTDQ2PS_XMMdq_XMMdq" | "VCVTPS2DQ_XMMdq_XMMdq" | "VCVTTPS2DQ_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VCVTDQ2PS_XMMdq_MEMdq" | "x86:VCVTPS2DQ_XMMdq_MEMdq" | "x86:VCVTTPS2DQ_XMMdq_MEMdq"),
      ("VCVTDQ2PS_XMMdq_MEMdq" | "VCVTPS2DQ_XMMdq_MEMdq" | "VCVTTPS2DQ_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VUNPCKLPS_XMMdq_XMMdq_XMMdq" | "x86:VUNPCKHPS_XMMdq_XMMdq_XMMdq"
      | "x86:VUNPCKLPD_XMMdq_XMMdq_XMMdq" | "x86:VUNPCKHPD_XMMdq_XMMdq_XMMdq" ),
      ( "VUNPCKLPS_XMMdq_XMMdq_XMMdq" | "VUNPCKHPS_XMMdq_XMMdq_XMMdq"
      | "VUNPCKLPD_XMMdq_XMMdq_XMMdq" | "VUNPCKHPD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VUNPCKLPS_XMMdq_XMMdq_MEMdq" | "x86:VUNPCKHPS_XMMdq_XMMdq_MEMdq"
      | "x86:VUNPCKLPD_XMMdq_XMMdq_MEMdq" | "x86:VUNPCKHPD_XMMdq_XMMdq_MEMdq" ),
      ( "VUNPCKLPS_XMMdq_XMMdq_MEMdq" | "VUNPCKHPS_XMMdq_XMMdq_MEMdq"
      | "VUNPCKLPD_XMMdq_XMMdq_MEMdq" | "VUNPCKHPD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VPUNPCKLQDQ_XMMdq_XMMdq_XMMdq" | "x86:VPUNPCKHQDQ_XMMdq_XMMdq_XMMdq"),
      ("VPUNPCKLQDQ_XMMdq_XMMdq_XMMdq" | "VPUNPCKHQDQ_XMMdq_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VPUNPCKLQDQ_XMMdq_XMMdq_MEMdq" | "x86:VPUNPCKHQDQ_XMMdq_XMMdq_MEMdq"),
      ("VPUNPCKLQDQ_XMMdq_XMMdq_MEMdq" | "VPUNPCKHQDQ_XMMdq_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPUNPCKLBW_XMMdq_XMMdq_XMMdq" | "x86:VPUNPCKHBW_XMMdq_XMMdq_XMMdq"
      | "x86:VPUNPCKLWD_XMMdq_XMMdq_XMMdq" | "x86:VPUNPCKHWD_XMMdq_XMMdq_XMMdq"
      | "x86:VPUNPCKLDQ_XMMdq_XMMdq_XMMdq" | "x86:VPUNPCKHDQ_XMMdq_XMMdq_XMMdq" ),
      ( "VPUNPCKLBW_XMMdq_XMMdq_XMMdq" | "VPUNPCKHBW_XMMdq_XMMdq_XMMdq"
      | "VPUNPCKLWD_XMMdq_XMMdq_XMMdq" | "VPUNPCKHWD_XMMdq_XMMdq_XMMdq"
      | "VPUNPCKLDQ_XMMdq_XMMdq_XMMdq" | "VPUNPCKHDQ_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPUNPCKLBW_XMMdq_XMMdq_MEMdq" | "x86:VPUNPCKHBW_XMMdq_XMMdq_MEMdq"
      | "x86:VPUNPCKLWD_XMMdq_XMMdq_MEMdq" | "x86:VPUNPCKHWD_XMMdq_XMMdq_MEMdq"
      | "x86:VPUNPCKLDQ_XMMdq_XMMdq_MEMdq" | "x86:VPUNPCKHDQ_XMMdq_XMMdq_MEMdq" ),
      ( "VPUNPCKLBW_XMMdq_XMMdq_MEMdq" | "VPUNPCKHBW_XMMdq_XMMdq_MEMdq"
      | "VPUNPCKLWD_XMMdq_XMMdq_MEMdq" | "VPUNPCKHWD_XMMdq_XMMdq_MEMdq"
      | "VPUNPCKLDQ_XMMdq_XMMdq_MEMdq" | "VPUNPCKHDQ_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSHUFB_XMMdq_XMMdq_XMMdq" | "x86:VPHADDW_XMMdq_XMMdq_XMMdq"
      | "x86:VPHADDD_XMMdq_XMMdq_XMMdq" | "x86:VPHADDSW_XMMdq_XMMdq_XMMdq"
      | "x86:VPMADDUBSW_XMMdq_XMMdq_XMMdq" | "x86:VPHSUBW_XMMdq_XMMdq_XMMdq"
      | "x86:VPHSUBD_XMMdq_XMMdq_XMMdq" | "x86:VPHSUBSW_XMMdq_XMMdq_XMMdq"
      | "x86:VPSIGNB_XMMdq_XMMdq_XMMdq" | "x86:VPSIGNW_XMMdq_XMMdq_XMMdq"
      | "x86:VPSIGND_XMMdq_XMMdq_XMMdq" | "x86:VPMULHRSW_XMMdq_XMMdq_XMMdq"
      | "x86:VPMULDQ_XMMdq_XMMdq_XMMdq" | "x86:VPCMPEQQ_XMMdq_XMMdq_XMMdq"
      | "x86:VPACKUSDW_XMMdq_XMMdq_XMMdq" | "x86:VPCMPGTQ_XMMdq_XMMdq_XMMdq"
      | "x86:VPMINSB_XMMdq_XMMdq_XMMdq" | "x86:VPMINSD_XMMdq_XMMdq_XMMdq"
      | "x86:VPMINUW_XMMdq_XMMdq_XMMdq" | "x86:VPMINUD_XMMdq_XMMdq_XMMdq"
      | "x86:VPMAXSB_XMMdq_XMMdq_XMMdq" | "x86:VPMAXSD_XMMdq_XMMdq_XMMdq"
      | "x86:VPMAXUW_XMMdq_XMMdq_XMMdq" | "x86:VPMAXUD_XMMdq_XMMdq_XMMdq"
      | "x86:VPMULLD_XMMdq_XMMdq_XMMdq" ),
      ( "VPSHUFB_XMMdq_XMMdq_XMMdq" | "VPHADDW_XMMdq_XMMdq_XMMdq" | "VPHADDD_XMMdq_XMMdq_XMMdq"
      | "VPHADDSW_XMMdq_XMMdq_XMMdq" | "VPMADDUBSW_XMMdq_XMMdq_XMMdq" | "VPHSUBW_XMMdq_XMMdq_XMMdq"
      | "VPHSUBD_XMMdq_XMMdq_XMMdq" | "VPHSUBSW_XMMdq_XMMdq_XMMdq" | "VPSIGNB_XMMdq_XMMdq_XMMdq"
      | "VPSIGNW_XMMdq_XMMdq_XMMdq" | "VPSIGND_XMMdq_XMMdq_XMMdq" | "VPMULHRSW_XMMdq_XMMdq_XMMdq"
      | "VPMULDQ_XMMdq_XMMdq_XMMdq" | "VPCMPEQQ_XMMdq_XMMdq_XMMdq" | "VPACKUSDW_XMMdq_XMMdq_XMMdq"
      | "VPCMPGTQ_XMMdq_XMMdq_XMMdq" | "VPMINSB_XMMdq_XMMdq_XMMdq" | "VPMINSD_XMMdq_XMMdq_XMMdq"
      | "VPMINUW_XMMdq_XMMdq_XMMdq" | "VPMINUD_XMMdq_XMMdq_XMMdq" | "VPMAXSB_XMMdq_XMMdq_XMMdq"
      | "VPMAXSD_XMMdq_XMMdq_XMMdq" | "VPMAXUW_XMMdq_XMMdq_XMMdq" | "VPMAXUD_XMMdq_XMMdq_XMMdq"
      | "VPMULLD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSHUFB_XMMdq_XMMdq_MEMdq" | "x86:VPHADDW_XMMdq_XMMdq_MEMdq"
      | "x86:VPHADDD_XMMdq_XMMdq_MEMdq" | "x86:VPHADDSW_XMMdq_XMMdq_MEMdq"
      | "x86:VPMADDUBSW_XMMdq_XMMdq_MEMdq" | "x86:VPHSUBW_XMMdq_XMMdq_MEMdq"
      | "x86:VPHSUBD_XMMdq_XMMdq_MEMdq" | "x86:VPHSUBSW_XMMdq_XMMdq_MEMdq"
      | "x86:VPSIGNB_XMMdq_XMMdq_MEMdq" | "x86:VPSIGNW_XMMdq_XMMdq_MEMdq"
      | "x86:VPSIGND_XMMdq_XMMdq_MEMdq" | "x86:VPMULHRSW_XMMdq_XMMdq_MEMdq"
      | "x86:VPMULDQ_XMMdq_XMMdq_MEMdq" | "x86:VPCMPEQQ_XMMdq_XMMdq_MEMdq"
      | "x86:VPACKUSDW_XMMdq_XMMdq_MEMdq" | "x86:VPCMPGTQ_XMMdq_XMMdq_MEMdq"
      | "x86:VPMINSB_XMMdq_XMMdq_MEMdq" | "x86:VPMINSD_XMMdq_XMMdq_MEMdq"
      | "x86:VPMINUW_XMMdq_XMMdq_MEMdq" | "x86:VPMINUD_XMMdq_XMMdq_MEMdq"
      | "x86:VPMAXSB_XMMdq_XMMdq_MEMdq" | "x86:VPMAXSD_XMMdq_XMMdq_MEMdq"
      | "x86:VPMAXUW_XMMdq_XMMdq_MEMdq" | "x86:VPMAXUD_XMMdq_XMMdq_MEMdq"
      | "x86:VPMULLD_XMMdq_XMMdq_MEMdq" ),
      ( "VPSHUFB_XMMdq_XMMdq_MEMdq" | "VPHADDW_XMMdq_XMMdq_MEMdq" | "VPHADDD_XMMdq_XMMdq_MEMdq"
      | "VPHADDSW_XMMdq_XMMdq_MEMdq" | "VPMADDUBSW_XMMdq_XMMdq_MEMdq" | "VPHSUBW_XMMdq_XMMdq_MEMdq"
      | "VPHSUBD_XMMdq_XMMdq_MEMdq" | "VPHSUBSW_XMMdq_XMMdq_MEMdq" | "VPSIGNB_XMMdq_XMMdq_MEMdq"
      | "VPSIGNW_XMMdq_XMMdq_MEMdq" | "VPSIGND_XMMdq_XMMdq_MEMdq" | "VPMULHRSW_XMMdq_XMMdq_MEMdq"
      | "VPMULDQ_XMMdq_XMMdq_MEMdq" | "VPCMPEQQ_XMMdq_XMMdq_MEMdq" | "VPACKUSDW_XMMdq_XMMdq_MEMdq"
      | "VPCMPGTQ_XMMdq_XMMdq_MEMdq" | "VPMINSB_XMMdq_XMMdq_MEMdq" | "VPMINSD_XMMdq_XMMdq_MEMdq"
      | "VPMINUW_XMMdq_XMMdq_MEMdq" | "VPMINUD_XMMdq_XMMdq_MEMdq" | "VPMAXSB_XMMdq_XMMdq_MEMdq"
      | "VPMAXSD_XMMdq_XMMdq_MEMdq" | "VPMAXUW_XMMdq_XMMdq_MEMdq" | "VPMAXUD_XMMdq_XMMdq_MEMdq"
      | "VPMULLD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPALIGNR_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VBLENDPS_XMMdq_XMMdq_XMMdq_IMMb"
      | "x86:VBLENDPD_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VPBLENDW_XMMdq_XMMdq_XMMdq_IMMb"
      | "x86:VROUNDSS_XMMdq_XMMdq_XMMd_IMMb" | "x86:VROUNDSD_XMMdq_XMMdq_XMMq_IMMb"
      | "x86:VDPPS_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VDPPD_XMMdq_XMMdq_XMMdq_IMMb"
      | "x86:VMPSADBW_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VINSERTPS_XMMdq_XMMdq_XMMdq_IMMb" ),
      ( "VPALIGNR_XMMdq_XMMdq_XMMdq_IMMb" | "VBLENDPS_XMMdq_XMMdq_XMMdq_IMMb"
      | "VBLENDPD_XMMdq_XMMdq_XMMdq_IMMb" | "VPBLENDW_XMMdq_XMMdq_XMMdq_IMMb"
      | "VROUNDSS_XMMdq_XMMdq_XMMd_IMMb" | "VROUNDSD_XMMdq_XMMdq_XMMq_IMMb"
      | "VDPPS_XMMdq_XMMdq_XMMdq_IMMb" | "VDPPD_XMMdq_XMMdq_XMMdq_IMMb"
      | "VMPSADBW_XMMdq_XMMdq_XMMdq_IMMb" | "VINSERTPS_XMMdq_XMMdq_XMMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPALIGNR_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VBLENDPS_XMMdq_XMMdq_MEMdq_IMMb"
      | "x86:VBLENDPD_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VPBLENDW_XMMdq_XMMdq_MEMdq_IMMb"
      | "x86:VROUNDSS_XMMdq_XMMdq_MEMd_IMMb" | "x86:VROUNDSD_XMMdq_XMMdq_MEMq_IMMb"
      | "x86:VDPPS_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VDPPD_XMMdq_XMMdq_MEMdq_IMMb"
      | "x86:VMPSADBW_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VINSERTPS_XMMdq_XMMdq_MEMd_IMMb" ),
      ( "VPALIGNR_XMMdq_XMMdq_MEMdq_IMMb" | "VBLENDPS_XMMdq_XMMdq_MEMdq_IMMb"
      | "VBLENDPD_XMMdq_XMMdq_MEMdq_IMMb" | "VPBLENDW_XMMdq_XMMdq_MEMdq_IMMb"
      | "VROUNDSS_XMMdq_XMMdq_MEMd_IMMb" | "VROUNDSD_XMMdq_XMMdq_MEMq_IMMb"
      | "VDPPS_XMMdq_XMMdq_MEMdq_IMMb" | "VDPPD_XMMdq_XMMdq_MEMdq_IMMb"
      | "VMPSADBW_XMMdq_XMMdq_MEMdq_IMMb" | "VINSERTPS_XMMdq_XMMdq_MEMd_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPABSB_XMMdq_XMMdq" | "x86:VPABSW_XMMdq_XMMdq" | "x86:VPABSD_XMMdq_XMMdq"
      | "x86:VPHMINPOSUW_XMMdq_XMMdq" | "x86:VPTEST_XMMdq_XMMdq" | "x86:VPMOVSXBW_XMMdq_XMMq"
      | "x86:VPMOVSXBD_XMMdq_XMMd" | "x86:VPMOVSXBQ_XMMdq_XMMw" | "x86:VPMOVSXWD_XMMdq_XMMq"
      | "x86:VPMOVSXWQ_XMMdq_XMMd" | "x86:VPMOVSXDQ_XMMdq_XMMq" | "x86:VPMOVZXBW_XMMdq_XMMq"
      | "x86:VPMOVZXBD_XMMdq_XMMd" | "x86:VPMOVZXBQ_XMMdq_XMMw" | "x86:VPMOVZXWD_XMMdq_XMMq"
      | "x86:VPMOVZXWQ_XMMdq_XMMd" | "x86:VPMOVZXDQ_XMMdq_XMMq" ),
      ( "VPABSB_XMMdq_XMMdq" | "VPABSW_XMMdq_XMMdq" | "VPABSD_XMMdq_XMMdq"
      | "VPHMINPOSUW_XMMdq_XMMdq" | "VPTEST_XMMdq_XMMdq" | "VPMOVSXBW_XMMdq_XMMq"
      | "VPMOVSXBD_XMMdq_XMMd" | "VPMOVSXBQ_XMMdq_XMMw" | "VPMOVSXWD_XMMdq_XMMq"
      | "VPMOVSXWQ_XMMdq_XMMd" | "VPMOVSXDQ_XMMdq_XMMq" | "VPMOVZXBW_XMMdq_XMMq"
      | "VPMOVZXBD_XMMdq_XMMd" | "VPMOVZXBQ_XMMdq_XMMw" | "VPMOVZXWD_XMMdq_XMMq"
      | "VPMOVZXWQ_XMMdq_XMMd" | "VPMOVZXDQ_XMMdq_XMMq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPABSB_XMMdq_MEMdq" | "x86:VPABSW_XMMdq_MEMdq" | "x86:VPABSD_XMMdq_MEMdq"
      | "x86:VPHMINPOSUW_XMMdq_MEMdq" | "x86:VPTEST_XMMdq_MEMdq" | "x86:VPMOVSXBW_XMMdq_MEMq"
      | "x86:VPMOVSXBD_XMMdq_MEMd" | "x86:VPMOVSXBQ_XMMdq_MEMw" | "x86:VPMOVSXWD_XMMdq_MEMq"
      | "x86:VPMOVSXWQ_XMMdq_MEMd" | "x86:VPMOVSXDQ_XMMdq_MEMq" | "x86:VPMOVZXBW_XMMdq_MEMq"
      | "x86:VPMOVZXBD_XMMdq_MEMd" | "x86:VPMOVZXBQ_XMMdq_MEMw" | "x86:VPMOVZXWD_XMMdq_MEMq"
      | "x86:VPMOVZXWQ_XMMdq_MEMd" | "x86:VPMOVZXDQ_XMMdq_MEMq" | "x86:VMOVNTDQA_XMMdq_MEMdq" ),
      ( "VPABSB_XMMdq_MEMdq" | "VPABSW_XMMdq_MEMdq" | "VPABSD_XMMdq_MEMdq"
      | "VPHMINPOSUW_XMMdq_MEMdq" | "VPTEST_XMMdq_MEMdq" | "VPMOVSXBW_XMMdq_MEMq"
      | "VPMOVSXBD_XMMdq_MEMd" | "VPMOVSXBQ_XMMdq_MEMw" | "VPMOVSXWD_XMMdq_MEMq"
      | "VPMOVSXWQ_XMMdq_MEMd" | "VPMOVSXDQ_XMMdq_MEMq" | "VPMOVZXBW_XMMdq_MEMq"
      | "VPMOVZXBD_XMMdq_MEMd" | "VPMOVZXBQ_XMMdq_MEMw" | "VPMOVZXWD_XMMdq_MEMq"
      | "VPMOVZXWQ_XMMdq_MEMd" | "VPMOVZXDQ_XMMdq_MEMq" | "VMOVNTDQA_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDPS_YMMqq_YMMqq_YMMqq" | "x86:VSUBPS_YMMqq_YMMqq_YMMqq"
      | "x86:VMULPS_YMMqq_YMMqq_YMMqq" | "x86:VDIVPS_YMMqq_YMMqq_YMMqq"
      | "x86:VANDPS_YMMqq_YMMqq_YMMqq" | "x86:VANDNPS_YMMqq_YMMqq_YMMqq"
      | "x86:VORPS_YMMqq_YMMqq_YMMqq" | "x86:VXORPS_YMMqq_YMMqq_YMMqq"
      | "x86:VMAXPS_YMMqq_YMMqq_YMMqq" | "x86:VMINPS_YMMqq_YMMqq_YMMqq"
      | "x86:VUNPCKLPS_YMMqq_YMMqq_YMMqq" | "x86:VUNPCKHPS_YMMqq_YMMqq_YMMqq"
      | "x86:VADDPD_YMMqq_YMMqq_YMMqq" | "x86:VSUBPD_YMMqq_YMMqq_YMMqq"
      | "x86:VMULPD_YMMqq_YMMqq_YMMqq" | "x86:VDIVPD_YMMqq_YMMqq_YMMqq"
      | "x86:VANDPD_YMMqq_YMMqq_YMMqq" | "x86:VANDNPD_YMMqq_YMMqq_YMMqq"
      | "x86:VORPD_YMMqq_YMMqq_YMMqq" | "x86:VXORPD_YMMqq_YMMqq_YMMqq"
      | "x86:VMAXPD_YMMqq_YMMqq_YMMqq" | "x86:VMINPD_YMMqq_YMMqq_YMMqq"
      | "x86:VUNPCKLPD_YMMqq_YMMqq_YMMqq" | "x86:VUNPCKHPD_YMMqq_YMMqq_YMMqq"
      | "x86:VADDPS_YMMqq_YMMqq_MEMqq" | "x86:VSUBPS_YMMqq_YMMqq_MEMqq"
      | "x86:VMULPS_YMMqq_YMMqq_MEMqq" | "x86:VDIVPS_YMMqq_YMMqq_MEMqq"
      | "x86:VANDPS_YMMqq_YMMqq_MEMqq" | "x86:VANDNPS_YMMqq_YMMqq_MEMqq"
      | "x86:VORPS_YMMqq_YMMqq_MEMqq" | "x86:VXORPS_YMMqq_YMMqq_MEMqq"
      | "x86:VMAXPS_YMMqq_YMMqq_MEMqq" | "x86:VMINPS_YMMqq_YMMqq_MEMqq"
      | "x86:VUNPCKLPS_YMMqq_YMMqq_MEMqq" | "x86:VUNPCKHPS_YMMqq_YMMqq_MEMqq"
      | "x86:VADDPD_YMMqq_YMMqq_MEMqq" | "x86:VSUBPD_YMMqq_YMMqq_MEMqq"
      | "x86:VMULPD_YMMqq_YMMqq_MEMqq" | "x86:VDIVPD_YMMqq_YMMqq_MEMqq"
      | "x86:VANDPD_YMMqq_YMMqq_MEMqq" | "x86:VANDNPD_YMMqq_YMMqq_MEMqq"
      | "x86:VORPD_YMMqq_YMMqq_MEMqq" | "x86:VXORPD_YMMqq_YMMqq_MEMqq"
      | "x86:VMAXPD_YMMqq_YMMqq_MEMqq" | "x86:VMINPD_YMMqq_YMMqq_MEMqq"
      | "x86:VUNPCKLPD_YMMqq_YMMqq_MEMqq" | "x86:VUNPCKHPD_YMMqq_YMMqq_MEMqq"
      | "x86:VSQRTPS_YMMqq_YMMqq" | "x86:VSQRTPD_YMMqq_YMMqq" | "x86:VMOVAPS_YMMqq_YMMqq_28"
      | "x86:VMOVUPS_YMMqq_YMMqq_10" | "x86:VMOVAPD_YMMqq_YMMqq_28" | "x86:VMOVUPD_YMMqq_YMMqq_10"
      | "x86:VMOVDQA_YMMqq_YMMqq_6F" | "x86:VMOVDQU_YMMqq_YMMqq_6F" | "x86:VSQRTPS_YMMqq_MEMqq"
      | "x86:VSQRTPD_YMMqq_MEMqq" | "x86:VMOVAPS_YMMqq_MEMqq" | "x86:VMOVUPS_YMMqq_MEMqq"
      | "x86:VMOVAPD_YMMqq_MEMqq" | "x86:VMOVUPD_YMMqq_MEMqq" | "x86:VMOVDQA_YMMqq_MEMqq"
      | "x86:VMOVDQU_YMMqq_MEMqq" ),
      ( "VADDPS_YMMqq_YMMqq_YMMqq" | "VSUBPS_YMMqq_YMMqq_YMMqq" | "VMULPS_YMMqq_YMMqq_YMMqq"
      | "VDIVPS_YMMqq_YMMqq_YMMqq" | "VANDPS_YMMqq_YMMqq_YMMqq" | "VANDNPS_YMMqq_YMMqq_YMMqq"
      | "VORPS_YMMqq_YMMqq_YMMqq" | "VXORPS_YMMqq_YMMqq_YMMqq" | "VMAXPS_YMMqq_YMMqq_YMMqq"
      | "VMINPS_YMMqq_YMMqq_YMMqq" | "VUNPCKLPS_YMMqq_YMMqq_YMMqq" | "VUNPCKHPS_YMMqq_YMMqq_YMMqq"
      | "VADDPD_YMMqq_YMMqq_YMMqq" | "VSUBPD_YMMqq_YMMqq_YMMqq" | "VMULPD_YMMqq_YMMqq_YMMqq"
      | "VDIVPD_YMMqq_YMMqq_YMMqq" | "VANDPD_YMMqq_YMMqq_YMMqq" | "VANDNPD_YMMqq_YMMqq_YMMqq"
      | "VORPD_YMMqq_YMMqq_YMMqq" | "VXORPD_YMMqq_YMMqq_YMMqq" | "VMAXPD_YMMqq_YMMqq_YMMqq"
      | "VMINPD_YMMqq_YMMqq_YMMqq" | "VUNPCKLPD_YMMqq_YMMqq_YMMqq" | "VUNPCKHPD_YMMqq_YMMqq_YMMqq"
      | "VADDPS_YMMqq_YMMqq_MEMqq" | "VSUBPS_YMMqq_YMMqq_MEMqq" | "VMULPS_YMMqq_YMMqq_MEMqq"
      | "VDIVPS_YMMqq_YMMqq_MEMqq" | "VANDPS_YMMqq_YMMqq_MEMqq" | "VANDNPS_YMMqq_YMMqq_MEMqq"
      | "VORPS_YMMqq_YMMqq_MEMqq" | "VXORPS_YMMqq_YMMqq_MEMqq" | "VMAXPS_YMMqq_YMMqq_MEMqq"
      | "VMINPS_YMMqq_YMMqq_MEMqq" | "VUNPCKLPS_YMMqq_YMMqq_MEMqq" | "VUNPCKHPS_YMMqq_YMMqq_MEMqq"
      | "VADDPD_YMMqq_YMMqq_MEMqq" | "VSUBPD_YMMqq_YMMqq_MEMqq" | "VMULPD_YMMqq_YMMqq_MEMqq"
      | "VDIVPD_YMMqq_YMMqq_MEMqq" | "VANDPD_YMMqq_YMMqq_MEMqq" | "VANDNPD_YMMqq_YMMqq_MEMqq"
      | "VORPD_YMMqq_YMMqq_MEMqq" | "VXORPD_YMMqq_YMMqq_MEMqq" | "VMAXPD_YMMqq_YMMqq_MEMqq"
      | "VMINPD_YMMqq_YMMqq_MEMqq" | "VUNPCKLPD_YMMqq_YMMqq_MEMqq" | "VUNPCKHPD_YMMqq_YMMqq_MEMqq"
      | "VSQRTPS_YMMqq_YMMqq" | "VSQRTPD_YMMqq_YMMqq" | "VMOVAPS_YMMqq_YMMqq_28"
      | "VMOVUPS_YMMqq_YMMqq_10" | "VMOVAPD_YMMqq_YMMqq_28" | "VMOVUPD_YMMqq_YMMqq_10"
      | "VMOVDQA_YMMqq_YMMqq_6F" | "VMOVDQU_YMMqq_YMMqq_6F" | "VSQRTPS_YMMqq_MEMqq"
      | "VSQRTPD_YMMqq_MEMqq" | "VMOVAPS_YMMqq_MEMqq" | "VMOVUPS_YMMqq_MEMqq"
      | "VMOVAPD_YMMqq_MEMqq" | "VMOVUPD_YMMqq_MEMqq" | "VMOVDQA_YMMqq_MEMqq"
      | "VMOVDQU_YMMqq_MEMqq" ) ) ->
      true
  | _ -> false

let promoted_case_part4 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPUNPCKLQDQ_YMMqq_YMMqq_YMMqq" | "x86:VPUNPCKHQDQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPUNPCKLBW_YMMqq_YMMqq_YMMqq" | "x86:VPUNPCKHBW_YMMqq_YMMqq_YMMqq"
      | "x86:VPUNPCKLWD_YMMqq_YMMqq_YMMqq" | "x86:VPUNPCKHWD_YMMqq_YMMqq_YMMqq"
      | "x86:VPUNPCKLDQ_YMMqq_YMMqq_YMMqq" | "x86:VPUNPCKHDQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPADDB_YMMqq_YMMqq_YMMqq" | "x86:VPADDW_YMMqq_YMMqq_YMMqq"
      | "x86:VPADDD_YMMqq_YMMqq_YMMqq" | "x86:VPADDQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPSUBB_YMMqq_YMMqq_YMMqq" | "x86:VPSUBW_YMMqq_YMMqq_YMMqq"
      | "x86:VPSUBD_YMMqq_YMMqq_YMMqq" | "x86:VPSUBQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPCMPEQB_YMMqq_YMMqq_YMMqq" | "x86:VPCMPEQW_YMMqq_YMMqq_YMMqq"
      | "x86:VPCMPEQD_YMMqq_YMMqq_YMMqq" | "x86:VPCMPGTB_YMMqq_YMMqq_YMMqq"
      | "x86:VPCMPGTW_YMMqq_YMMqq_YMMqq" | "x86:VPCMPGTD_YMMqq_YMMqq_YMMqq"
      | "x86:VPACKSSWB_YMMqq_YMMqq_YMMqq" | "x86:VPACKSSDW_YMMqq_YMMqq_YMMqq"
      | "x86:VPACKUSWB_YMMqq_YMMqq_YMMqq" | "x86:VPAND_YMMqq_YMMqq_YMMqq"
      | "x86:VPANDN_YMMqq_YMMqq_YMMqq" | "x86:VPOR_YMMqq_YMMqq_YMMqq"
      | "x86:VPMINUB_YMMqq_YMMqq_YMMqq" | "x86:VPMAXUB_YMMqq_YMMqq_YMMqq"
      | "x86:VPMINSW_YMMqq_YMMqq_YMMqq" | "x86:VPMAXSW_YMMqq_YMMqq_YMMqq"
      | "x86:VPMULLW_YMMqq_YMMqq_YMMqq" | "x86:VPMULHW_YMMqq_YMMqq_YMMqq"
      | "x86:VPMULHUW_YMMqq_YMMqq_YMMqq" | "x86:VPAVGB_YMMqq_YMMqq_YMMqq"
      | "x86:VPAVGW_YMMqq_YMMqq_YMMqq" | "x86:VPSADBW_YMMqq_YMMqq_YMMqq"
      | "x86:VPSHUFB_YMMqq_YMMqq_YMMqq" | "x86:VPHADDW_YMMqq_YMMqq_YMMqq"
      | "x86:VPHADDD_YMMqq_YMMqq_YMMqq" | "x86:VPHADDSW_YMMqq_YMMqq_YMMqq"
      | "x86:VPMADDUBSW_YMMqq_YMMqq_YMMqq" | "x86:VPHSUBW_YMMqq_YMMqq_YMMqq"
      | "x86:VPHSUBD_YMMqq_YMMqq_YMMqq" | "x86:VPHSUBSW_YMMqq_YMMqq_YMMqq"
      | "x86:VPSIGNB_YMMqq_YMMqq_YMMqq" | "x86:VPSIGNW_YMMqq_YMMqq_YMMqq"
      | "x86:VPSIGND_YMMqq_YMMqq_YMMqq" | "x86:VPMULHRSW_YMMqq_YMMqq_YMMqq"
      | "x86:VPMULDQ_YMMqq_YMMqq_YMMqq" | "x86:VPCMPEQQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPACKUSDW_YMMqq_YMMqq_YMMqq" | "x86:VPCMPGTQ_YMMqq_YMMqq_YMMqq"
      | "x86:VPMINSB_YMMqq_YMMqq_YMMqq" | "x86:VPMINSD_YMMqq_YMMqq_YMMqq"
      | "x86:VPMINUW_YMMqq_YMMqq_YMMqq" | "x86:VPMINUD_YMMqq_YMMqq_YMMqq"
      | "x86:VPMAXSB_YMMqq_YMMqq_YMMqq" | "x86:VPMAXSD_YMMqq_YMMqq_YMMqq"
      | "x86:VPMAXUW_YMMqq_YMMqq_YMMqq" | "x86:VPMAXUD_YMMqq_YMMqq_YMMqq"
      | "x86:VPMULLD_YMMqq_YMMqq_YMMqq" | "x86:VPUNPCKLQDQ_YMMqq_YMMqq_MEMqq"
      | "x86:VPUNPCKHQDQ_YMMqq_YMMqq_MEMqq" | "x86:VPUNPCKLBW_YMMqq_YMMqq_MEMqq"
      | "x86:VPUNPCKHBW_YMMqq_YMMqq_MEMqq" | "x86:VPUNPCKLWD_YMMqq_YMMqq_MEMqq"
      | "x86:VPUNPCKHWD_YMMqq_YMMqq_MEMqq" | "x86:VPUNPCKLDQ_YMMqq_YMMqq_MEMqq"
      | "x86:VPUNPCKHDQ_YMMqq_YMMqq_MEMqq" | "x86:VPADDB_YMMqq_YMMqq_MEMqq"
      | "x86:VPADDW_YMMqq_YMMqq_MEMqq" | "x86:VPADDD_YMMqq_YMMqq_MEMqq"
      | "x86:VPADDQ_YMMqq_YMMqq_MEMqq" | "x86:VPSUBB_YMMqq_YMMqq_MEMqq"
      | "x86:VPSUBW_YMMqq_YMMqq_MEMqq" | "x86:VPSUBD_YMMqq_YMMqq_MEMqq"
      | "x86:VPSUBQ_YMMqq_YMMqq_MEMqq" | "x86:VPCMPEQB_YMMqq_YMMqq_MEMqq"
      | "x86:VPCMPEQW_YMMqq_YMMqq_MEMqq" | "x86:VPCMPEQD_YMMqq_YMMqq_MEMqq"
      | "x86:VPCMPGTB_YMMqq_YMMqq_MEMqq" | "x86:VPCMPGTW_YMMqq_YMMqq_MEMqq"
      | "x86:VPCMPGTD_YMMqq_YMMqq_MEMqq" | "x86:VPACKSSWB_YMMqq_YMMqq_MEMqq"
      | "x86:VPACKSSDW_YMMqq_YMMqq_MEMqq" | "x86:VPACKUSWB_YMMqq_YMMqq_MEMqq"
      | "x86:VPAND_YMMqq_YMMqq_MEMqq" | "x86:VPANDN_YMMqq_YMMqq_MEMqq"
      | "x86:VPOR_YMMqq_YMMqq_MEMqq" | "x86:VPMINUB_YMMqq_YMMqq_MEMqq"
      | "x86:VPMAXUB_YMMqq_YMMqq_MEMqq" | "x86:VPMINSW_YMMqq_YMMqq_MEMqq"
      | "x86:VPMAXSW_YMMqq_YMMqq_MEMqq" | "x86:VPMULLW_YMMqq_YMMqq_MEMqq"
      | "x86:VPMULHW_YMMqq_YMMqq_MEMqq" | "x86:VPMULHUW_YMMqq_YMMqq_MEMqq"
      | "x86:VPAVGB_YMMqq_YMMqq_MEMqq" | "x86:VPAVGW_YMMqq_YMMqq_MEMqq"
      | "x86:VPSADBW_YMMqq_YMMqq_MEMqq" | "x86:VPSHUFB_YMMqq_YMMqq_MEMqq"
      | "x86:VPHADDW_YMMqq_YMMqq_MEMqq" | "x86:VPHADDD_YMMqq_YMMqq_MEMqq"
      | "x86:VPHADDSW_YMMqq_YMMqq_MEMqq" | "x86:VPMADDUBSW_YMMqq_YMMqq_MEMqq"
      | "x86:VPHSUBW_YMMqq_YMMqq_MEMqq" | "x86:VPHSUBD_YMMqq_YMMqq_MEMqq"
      | "x86:VPHSUBSW_YMMqq_YMMqq_MEMqq" | "x86:VPSIGNB_YMMqq_YMMqq_MEMqq"
      | "x86:VPSIGNW_YMMqq_YMMqq_MEMqq" | "x86:VPSIGND_YMMqq_YMMqq_MEMqq"
      | "x86:VPMULHRSW_YMMqq_YMMqq_MEMqq" | "x86:VPMULDQ_YMMqq_YMMqq_MEMqq"
      | "x86:VPCMPEQQ_YMMqq_YMMqq_MEMqq" | "x86:VPACKUSDW_YMMqq_YMMqq_MEMqq"
      | "x86:VPCMPGTQ_YMMqq_YMMqq_MEMqq" | "x86:VPMINSB_YMMqq_YMMqq_MEMqq"
      | "x86:VPMINSD_YMMqq_YMMqq_MEMqq" | "x86:VPMINUW_YMMqq_YMMqq_MEMqq"
      | "x86:VPMINUD_YMMqq_YMMqq_MEMqq" | "x86:VPMAXSB_YMMqq_YMMqq_MEMqq"
      | "x86:VPMAXSD_YMMqq_YMMqq_MEMqq" | "x86:VPMAXUW_YMMqq_YMMqq_MEMqq"
      | "x86:VPMAXUD_YMMqq_YMMqq_MEMqq" | "x86:VPMULLD_YMMqq_YMMqq_MEMqq" | "x86:VPABSB_YMMqq_YMMqq"
      | "x86:VPABSW_YMMqq_YMMqq" | "x86:VPABSD_YMMqq_YMMqq" | "x86:VPTEST_YMMqq_YMMqq"
      | "x86:VPABSB_YMMqq_MEMqq" | "x86:VPABSW_YMMqq_MEMqq" | "x86:VPABSD_YMMqq_MEMqq"
      | "x86:VPTEST_YMMqq_MEMqq" | "x86:VMOVNTDQA_YMMqq_MEMqq" ),
      ( "VPUNPCKLQDQ_YMMqq_YMMqq_YMMqq" | "VPUNPCKHQDQ_YMMqq_YMMqq_YMMqq"
      | "VPUNPCKLBW_YMMqq_YMMqq_YMMqq" | "VPUNPCKHBW_YMMqq_YMMqq_YMMqq"
      | "VPUNPCKLWD_YMMqq_YMMqq_YMMqq" | "VPUNPCKHWD_YMMqq_YMMqq_YMMqq"
      | "VPUNPCKLDQ_YMMqq_YMMqq_YMMqq" | "VPUNPCKHDQ_YMMqq_YMMqq_YMMqq" | "VPADDB_YMMqq_YMMqq_YMMqq"
      | "VPADDW_YMMqq_YMMqq_YMMqq" | "VPADDD_YMMqq_YMMqq_YMMqq" | "VPADDQ_YMMqq_YMMqq_YMMqq"
      | "VPSUBB_YMMqq_YMMqq_YMMqq" | "VPSUBW_YMMqq_YMMqq_YMMqq" | "VPSUBD_YMMqq_YMMqq_YMMqq"
      | "VPSUBQ_YMMqq_YMMqq_YMMqq" | "VPCMPEQB_YMMqq_YMMqq_YMMqq" | "VPCMPEQW_YMMqq_YMMqq_YMMqq"
      | "VPCMPEQD_YMMqq_YMMqq_YMMqq" | "VPCMPGTB_YMMqq_YMMqq_YMMqq" | "VPCMPGTW_YMMqq_YMMqq_YMMqq"
      | "VPCMPGTD_YMMqq_YMMqq_YMMqq" | "VPACKSSWB_YMMqq_YMMqq_YMMqq" | "VPACKSSDW_YMMqq_YMMqq_YMMqq"
      | "VPACKUSWB_YMMqq_YMMqq_YMMqq" | "VPAND_YMMqq_YMMqq_YMMqq" | "VPANDN_YMMqq_YMMqq_YMMqq"
      | "VPOR_YMMqq_YMMqq_YMMqq" | "VPMINUB_YMMqq_YMMqq_YMMqq" | "VPMAXUB_YMMqq_YMMqq_YMMqq"
      | "VPMINSW_YMMqq_YMMqq_YMMqq" | "VPMAXSW_YMMqq_YMMqq_YMMqq" | "VPMULLW_YMMqq_YMMqq_YMMqq"
      | "VPMULHW_YMMqq_YMMqq_YMMqq" | "VPMULHUW_YMMqq_YMMqq_YMMqq" | "VPAVGB_YMMqq_YMMqq_YMMqq"
      | "VPAVGW_YMMqq_YMMqq_YMMqq" | "VPSADBW_YMMqq_YMMqq_YMMqq" | "VPSHUFB_YMMqq_YMMqq_YMMqq"
      | "VPHADDW_YMMqq_YMMqq_YMMqq" | "VPHADDD_YMMqq_YMMqq_YMMqq" | "VPHADDSW_YMMqq_YMMqq_YMMqq"
      | "VPMADDUBSW_YMMqq_YMMqq_YMMqq" | "VPHSUBW_YMMqq_YMMqq_YMMqq" | "VPHSUBD_YMMqq_YMMqq_YMMqq"
      | "VPHSUBSW_YMMqq_YMMqq_YMMqq" | "VPSIGNB_YMMqq_YMMqq_YMMqq" | "VPSIGNW_YMMqq_YMMqq_YMMqq"
      | "VPSIGND_YMMqq_YMMqq_YMMqq" | "VPMULHRSW_YMMqq_YMMqq_YMMqq" | "VPMULDQ_YMMqq_YMMqq_YMMqq"
      | "VPCMPEQQ_YMMqq_YMMqq_YMMqq" | "VPACKUSDW_YMMqq_YMMqq_YMMqq" | "VPCMPGTQ_YMMqq_YMMqq_YMMqq"
      | "VPMINSB_YMMqq_YMMqq_YMMqq" | "VPMINSD_YMMqq_YMMqq_YMMqq" | "VPMINUW_YMMqq_YMMqq_YMMqq"
      | "VPMINUD_YMMqq_YMMqq_YMMqq" | "VPMAXSB_YMMqq_YMMqq_YMMqq" | "VPMAXSD_YMMqq_YMMqq_YMMqq"
      | "VPMAXUW_YMMqq_YMMqq_YMMqq" | "VPMAXUD_YMMqq_YMMqq_YMMqq" | "VPMULLD_YMMqq_YMMqq_YMMqq"
      | "VPUNPCKLQDQ_YMMqq_YMMqq_MEMqq" | "VPUNPCKHQDQ_YMMqq_YMMqq_MEMqq"
      | "VPUNPCKLBW_YMMqq_YMMqq_MEMqq" | "VPUNPCKHBW_YMMqq_YMMqq_MEMqq"
      | "VPUNPCKLWD_YMMqq_YMMqq_MEMqq" | "VPUNPCKHWD_YMMqq_YMMqq_MEMqq"
      | "VPUNPCKLDQ_YMMqq_YMMqq_MEMqq" | "VPUNPCKHDQ_YMMqq_YMMqq_MEMqq" | "VPADDB_YMMqq_YMMqq_MEMqq"
      | "VPADDW_YMMqq_YMMqq_MEMqq" | "VPADDD_YMMqq_YMMqq_MEMqq" | "VPADDQ_YMMqq_YMMqq_MEMqq"
      | "VPSUBB_YMMqq_YMMqq_MEMqq" | "VPSUBW_YMMqq_YMMqq_MEMqq" | "VPSUBD_YMMqq_YMMqq_MEMqq"
      | "VPSUBQ_YMMqq_YMMqq_MEMqq" | "VPCMPEQB_YMMqq_YMMqq_MEMqq" | "VPCMPEQW_YMMqq_YMMqq_MEMqq"
      | "VPCMPEQD_YMMqq_YMMqq_MEMqq" | "VPCMPGTB_YMMqq_YMMqq_MEMqq" | "VPCMPGTW_YMMqq_YMMqq_MEMqq"
      | "VPCMPGTD_YMMqq_YMMqq_MEMqq" | "VPACKSSWB_YMMqq_YMMqq_MEMqq" | "VPACKSSDW_YMMqq_YMMqq_MEMqq"
      | "VPACKUSWB_YMMqq_YMMqq_MEMqq" | "VPAND_YMMqq_YMMqq_MEMqq" | "VPANDN_YMMqq_YMMqq_MEMqq"
      | "VPOR_YMMqq_YMMqq_MEMqq" | "VPMINUB_YMMqq_YMMqq_MEMqq" | "VPMAXUB_YMMqq_YMMqq_MEMqq"
      | "VPMINSW_YMMqq_YMMqq_MEMqq" | "VPMAXSW_YMMqq_YMMqq_MEMqq" | "VPMULLW_YMMqq_YMMqq_MEMqq"
      | "VPMULHW_YMMqq_YMMqq_MEMqq" | "VPMULHUW_YMMqq_YMMqq_MEMqq" | "VPAVGB_YMMqq_YMMqq_MEMqq"
      | "VPAVGW_YMMqq_YMMqq_MEMqq" | "VPSADBW_YMMqq_YMMqq_MEMqq" | "VPSHUFB_YMMqq_YMMqq_MEMqq"
      | "VPHADDW_YMMqq_YMMqq_MEMqq" | "VPHADDD_YMMqq_YMMqq_MEMqq" | "VPHADDSW_YMMqq_YMMqq_MEMqq"
      | "VPMADDUBSW_YMMqq_YMMqq_MEMqq" | "VPHSUBW_YMMqq_YMMqq_MEMqq" | "VPHSUBD_YMMqq_YMMqq_MEMqq"
      | "VPHSUBSW_YMMqq_YMMqq_MEMqq" | "VPSIGNB_YMMqq_YMMqq_MEMqq" | "VPSIGNW_YMMqq_YMMqq_MEMqq"
      | "VPSIGND_YMMqq_YMMqq_MEMqq" | "VPMULHRSW_YMMqq_YMMqq_MEMqq" | "VPMULDQ_YMMqq_YMMqq_MEMqq"
      | "VPCMPEQQ_YMMqq_YMMqq_MEMqq" | "VPACKUSDW_YMMqq_YMMqq_MEMqq" | "VPCMPGTQ_YMMqq_YMMqq_MEMqq"
      | "VPMINSB_YMMqq_YMMqq_MEMqq" | "VPMINSD_YMMqq_YMMqq_MEMqq" | "VPMINUW_YMMqq_YMMqq_MEMqq"
      | "VPMINUD_YMMqq_YMMqq_MEMqq" | "VPMAXSB_YMMqq_YMMqq_MEMqq" | "VPMAXSD_YMMqq_YMMqq_MEMqq"
      | "VPMAXUW_YMMqq_YMMqq_MEMqq" | "VPMAXUD_YMMqq_YMMqq_MEMqq" | "VPMULLD_YMMqq_YMMqq_MEMqq"
      | "VPABSB_YMMqq_YMMqq" | "VPABSW_YMMqq_YMMqq" | "VPABSD_YMMqq_YMMqq" | "VPTEST_YMMqq_YMMqq"
      | "VPABSB_YMMqq_MEMqq" | "VPABSW_YMMqq_MEMqq" | "VPABSD_YMMqq_MEMqq" | "VPTEST_YMMqq_MEMqq"
      | "VMOVNTDQA_YMMqq_MEMqq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VADDPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VSUBPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VMULPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VDIVPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VMAXPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VMINPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VUNPCKLPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VUNPCKHPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "x86:VADDPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VSUBPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VMULPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VDIVPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VMAXPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VMINPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VUNPCKLPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "x86:VUNPCKHPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512" ),
      ( "VADDPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VSUBPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VMULPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VDIVPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VMAXPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VMINPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VUNPCKLPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VUNPCKHPS_ZMMf32_MASKmskw_ZMMf32_ZMMf32_AVX512"
      | "VADDPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VSUBPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VMULPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VDIVPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VMAXPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VMINPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VUNPCKLPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512"
      | "VUNPCKHPD_ZMMf64_MASKmskw_ZMMf64_ZMMf64_AVX512" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPADDB_XMMdq_XMMdq_XMMdq" | "x86:VPADDW_XMMdq_XMMdq_XMMdq"
      | "x86:VPADDD_XMMdq_XMMdq_XMMdq" | "x86:VPADDQ_XMMdq_XMMdq_XMMdq"
      | "x86:VPSUBB_XMMdq_XMMdq_XMMdq" | "x86:VPSUBW_XMMdq_XMMdq_XMMdq"
      | "x86:VPSUBD_XMMdq_XMMdq_XMMdq" | "x86:VPSUBQ_XMMdq_XMMdq_XMMdq" ),
      ( "VPADDB_XMMdq_XMMdq_XMMdq" | "VPADDW_XMMdq_XMMdq_XMMdq" | "VPADDD_XMMdq_XMMdq_XMMdq"
      | "VPADDQ_XMMdq_XMMdq_XMMdq" | "VPSUBB_XMMdq_XMMdq_XMMdq" | "VPSUBW_XMMdq_XMMdq_XMMdq"
      | "VPSUBD_XMMdq_XMMdq_XMMdq" | "VPSUBQ_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPADDB_XMMdq_XMMdq_MEMdq" | "x86:VPADDW_XMMdq_XMMdq_MEMdq"
      | "x86:VPADDD_XMMdq_XMMdq_MEMdq" | "x86:VPADDQ_XMMdq_XMMdq_MEMdq"
      | "x86:VPSUBB_XMMdq_XMMdq_MEMdq" | "x86:VPSUBW_XMMdq_XMMdq_MEMdq"
      | "x86:VPSUBD_XMMdq_XMMdq_MEMdq" | "x86:VPSUBQ_XMMdq_XMMdq_MEMdq" ),
      ( "VPADDB_XMMdq_XMMdq_MEMdq" | "VPADDW_XMMdq_XMMdq_MEMdq" | "VPADDD_XMMdq_XMMdq_MEMdq"
      | "VPADDQ_XMMdq_XMMdq_MEMdq" | "VPSUBB_XMMdq_XMMdq_MEMdq" | "VPSUBW_XMMdq_XMMdq_MEMdq"
      | "VPSUBD_XMMdq_XMMdq_MEMdq" | "VPSUBQ_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPCMPEQB_XMMdq_XMMdq_XMMdq" | "x86:VPCMPEQW_XMMdq_XMMdq_XMMdq"
      | "x86:VPCMPEQD_XMMdq_XMMdq_XMMdq" | "x86:VPCMPGTB_XMMdq_XMMdq_XMMdq"
      | "x86:VPCMPGTW_XMMdq_XMMdq_XMMdq" | "x86:VPCMPGTD_XMMdq_XMMdq_XMMdq" ),
      ( "VPCMPEQB_XMMdq_XMMdq_XMMdq" | "VPCMPEQW_XMMdq_XMMdq_XMMdq" | "VPCMPEQD_XMMdq_XMMdq_XMMdq"
      | "VPCMPGTB_XMMdq_XMMdq_XMMdq" | "VPCMPGTW_XMMdq_XMMdq_XMMdq" | "VPCMPGTD_XMMdq_XMMdq_XMMdq"
        ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPCMPEQB_XMMdq_XMMdq_MEMdq" | "x86:VPCMPEQW_XMMdq_XMMdq_MEMdq"
      | "x86:VPCMPEQD_XMMdq_XMMdq_MEMdq" | "x86:VPCMPGTB_XMMdq_XMMdq_MEMdq"
      | "x86:VPCMPGTW_XMMdq_XMMdq_MEMdq" | "x86:VPCMPGTD_XMMdq_XMMdq_MEMdq" ),
      ( "VPCMPEQB_XMMdq_XMMdq_MEMdq" | "VPCMPEQW_XMMdq_XMMdq_MEMdq" | "VPCMPEQD_XMMdq_XMMdq_MEMdq"
      | "VPCMPGTB_XMMdq_XMMdq_MEMdq" | "VPCMPGTW_XMMdq_XMMdq_MEMdq" | "VPCMPGTD_XMMdq_XMMdq_MEMdq"
        ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPACKSSWB_XMMdq_XMMdq_XMMdq" | "x86:VPACKSSDW_XMMdq_XMMdq_XMMdq"
      | "x86:VPACKUSWB_XMMdq_XMMdq_XMMdq" ),
      ("VPACKSSWB_XMMdq_XMMdq_XMMdq" | "VPACKSSDW_XMMdq_XMMdq_XMMdq" | "VPACKUSWB_XMMdq_XMMdq_XMMdq")
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPACKSSWB_XMMdq_XMMdq_MEMdq" | "x86:VPACKSSDW_XMMdq_XMMdq_MEMdq"
      | "x86:VPACKUSWB_XMMdq_XMMdq_MEMdq" ),
      ("VPACKSSWB_XMMdq_XMMdq_MEMdq" | "VPACKSSDW_XMMdq_XMMdq_MEMdq" | "VPACKUSWB_XMMdq_XMMdq_MEMdq")
    ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VPAND_XMMdq_XMMdq_XMMdq" | "x86:VPANDN_XMMdq_XMMdq_XMMdq" | "x86:VPOR_XMMdq_XMMdq_XMMdq"),
      ("VPAND_XMMdq_XMMdq_XMMdq" | "VPANDN_XMMdq_XMMdq_XMMdq" | "VPOR_XMMdq_XMMdq_XMMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VPAND_XMMdq_XMMdq_MEMdq" | "x86:VPANDN_XMMdq_XMMdq_MEMdq" | "x86:VPOR_XMMdq_XMMdq_MEMdq"),
      ("VPAND_XMMdq_XMMdq_MEMdq" | "VPANDN_XMMdq_XMMdq_MEMdq" | "VPOR_XMMdq_XMMdq_MEMdq") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPMINUB_XMMdq_XMMdq_XMMdq" | "x86:VPMAXUB_XMMdq_XMMdq_XMMdq"
      | "x86:VPMINSW_XMMdq_XMMdq_XMMdq" | "x86:VPMAXSW_XMMdq_XMMdq_XMMdq" ),
      ( "VPMINUB_XMMdq_XMMdq_XMMdq" | "VPMAXUB_XMMdq_XMMdq_XMMdq" | "VPMINSW_XMMdq_XMMdq_XMMdq"
      | "VPMAXSW_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPMINUB_XMMdq_XMMdq_MEMdq" | "x86:VPMAXUB_XMMdq_XMMdq_MEMdq"
      | "x86:VPMINSW_XMMdq_XMMdq_MEMdq" | "x86:VPMAXSW_XMMdq_XMMdq_MEMdq" ),
      ( "VPMINUB_XMMdq_XMMdq_MEMdq" | "VPMAXUB_XMMdq_XMMdq_MEMdq" | "VPMINSW_XMMdq_XMMdq_MEMdq"
      | "VPMAXSW_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPMULLW_XMMdq_XMMdq_XMMdq" | "x86:VPMULHW_XMMdq_XMMdq_XMMdq"
      | "x86:VPMULHUW_XMMdq_XMMdq_XMMdq" | "x86:VPAVGB_XMMdq_XMMdq_XMMdq"
      | "x86:VPAVGW_XMMdq_XMMdq_XMMdq" | "x86:VPSADBW_XMMdq_XMMdq_XMMdq" ),
      ( "VPMULLW_XMMdq_XMMdq_XMMdq" | "VPMULHW_XMMdq_XMMdq_XMMdq" | "VPMULHUW_XMMdq_XMMdq_XMMdq"
      | "VPAVGB_XMMdq_XMMdq_XMMdq" | "VPAVGW_XMMdq_XMMdq_XMMdq" | "VPSADBW_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPMULLW_XMMdq_XMMdq_MEMdq" | "x86:VPMULHW_XMMdq_XMMdq_MEMdq"
      | "x86:VPMULHUW_XMMdq_XMMdq_MEMdq" | "x86:VPAVGB_XMMdq_XMMdq_MEMdq"
      | "x86:VPAVGW_XMMdq_XMMdq_MEMdq" | "x86:VPSADBW_XMMdq_XMMdq_MEMdq" ),
      ( "VPMULLW_XMMdq_XMMdq_MEMdq" | "VPMULHW_XMMdq_XMMdq_MEMdq" | "VPMULHUW_XMMdq_XMMdq_MEMdq"
      | "VPAVGB_XMMdq_XMMdq_MEMdq" | "VPAVGW_XMMdq_XMMdq_MEMdq" | "VPSADBW_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSLLW_XMMdq_XMMdq_XMMdq" | "x86:VPSLLD_XMMdq_XMMdq_XMMdq"
      | "x86:VPSLLQ_XMMdq_XMMdq_XMMdq" | "x86:VPSRLW_XMMdq_XMMdq_XMMdq"
      | "x86:VPSRLD_XMMdq_XMMdq_XMMdq" | "x86:VPSRLQ_XMMdq_XMMdq_XMMdq"
      | "x86:VPSRAW_XMMdq_XMMdq_XMMdq" | "x86:VPSRAD_XMMdq_XMMdq_XMMdq" ),
      ( "VPSLLW_XMMdq_XMMdq_XMMdq" | "VPSLLD_XMMdq_XMMdq_XMMdq" | "VPSLLQ_XMMdq_XMMdq_XMMdq"
      | "VPSRLW_XMMdq_XMMdq_XMMdq" | "VPSRLD_XMMdq_XMMdq_XMMdq" | "VPSRLQ_XMMdq_XMMdq_XMMdq"
      | "VPSRAW_XMMdq_XMMdq_XMMdq" | "VPSRAD_XMMdq_XMMdq_XMMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSLLW_XMMdq_XMMdq_MEMdq" | "x86:VPSLLD_XMMdq_XMMdq_MEMdq"
      | "x86:VPSLLQ_XMMdq_XMMdq_MEMdq" | "x86:VPSRLW_XMMdq_XMMdq_MEMdq"
      | "x86:VPSRLD_XMMdq_XMMdq_MEMdq" | "x86:VPSRLQ_XMMdq_XMMdq_MEMdq"
      | "x86:VPSRAW_XMMdq_XMMdq_MEMdq" | "x86:VPSRAD_XMMdq_XMMdq_MEMdq" ),
      ( "VPSLLW_XMMdq_XMMdq_MEMdq" | "VPSLLD_XMMdq_XMMdq_MEMdq" | "VPSLLQ_XMMdq_XMMdq_MEMdq"
      | "VPSRLW_XMMdq_XMMdq_MEMdq" | "VPSRLD_XMMdq_XMMdq_MEMdq" | "VPSRLQ_XMMdq_XMMdq_MEMdq"
      | "VPSRAW_XMMdq_XMMdq_MEMdq" | "VPSRAD_XMMdq_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSLLW_XMMdq_XMMdq_IMMb" | "x86:VPSLLD_XMMdq_XMMdq_IMMb"
      | "x86:VPSLLQ_XMMdq_XMMdq_IMMb" | "x86:VPSRLW_XMMdq_XMMdq_IMMb"
      | "x86:VPSRLD_XMMdq_XMMdq_IMMb" | "x86:VPSRLQ_XMMdq_XMMdq_IMMb"
      | "x86:VPSRAW_XMMdq_XMMdq_IMMb" | "x86:VPSRAD_XMMdq_XMMdq_IMMb" ),
      ( "VPSLLW_XMMdq_XMMdq_IMMb" | "VPSLLD_XMMdq_XMMdq_IMMb" | "VPSLLQ_XMMdq_XMMdq_IMMb"
      | "VPSRLW_XMMdq_XMMdq_IMMb" | "VPSRLD_XMMdq_XMMdq_IMMb" | "VPSRLQ_XMMdq_XMMdq_IMMb"
      | "VPSRAW_XMMdq_XMMdq_IMMb" | "VPSRAD_XMMdq_XMMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VPSLLDQ_XMMdq_XMMdq_IMMb" | "x86:VPSRLDQ_XMMdq_XMMdq_IMMb"),
      ("VPSLLDQ_XMMdq_XMMdq_IMMb" | "VPSRLDQ_XMMdq_XMMdq_IMMb") ) ->
      true
  | _ -> false

let promoted_case_part5 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VMOVDQA_XMMdq_XMMdq_6F" | "x86:VMOVDQU_XMMdq_XMMdq_6F" | "x86:VMOVDQA_XMMdq_MEMdq"
      | "x86:VMOVDQU_XMMdq_MEMdq" ),
      ( "VMOVDQA_XMMdq_XMMdq_6F" | "VMOVDQU_XMMdq_XMMdq_6F" | "VMOVDQA_XMMdq_MEMdq"
      | "VMOVDQU_XMMdq_MEMdq" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSHUFPS_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VSHUFPD_XMMdq_XMMdq_XMMdq_IMMb"),
      ("VSHUFPS_XMMdq_XMMdq_XMMdq_IMMb" | "VSHUFPD_XMMdq_XMMdq_XMMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ("x86:VSHUFPS_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VSHUFPD_XMMdq_XMMdq_MEMdq_IMMb"),
      ("VSHUFPS_XMMdq_XMMdq_MEMdq_IMMb" | "VSHUFPD_XMMdq_XMMdq_MEMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VCMPSS_XMMdq_XMMdq_XMMd_IMMb" | "x86:VCMPSD_XMMdq_XMMdq_XMMq_IMMb"
      | "x86:VCMPPS_XMMdq_XMMdq_XMMdq_IMMb" | "x86:VCMPPD_XMMdq_XMMdq_XMMdq_IMMb" ),
      ( "VCMPSS_XMMdq_XMMdq_XMMd_IMMb" | "VCMPSD_XMMdq_XMMdq_XMMq_IMMb"
      | "VCMPPS_XMMdq_XMMdq_XMMdq_IMMb" | "VCMPPD_XMMdq_XMMdq_XMMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VCMPSS_XMMdq_XMMdq_MEMd_IMMb" | "x86:VCMPSD_XMMdq_XMMdq_MEMq_IMMb"
      | "x86:VCMPPS_XMMdq_XMMdq_MEMdq_IMMb" | "x86:VCMPPD_XMMdq_XMMdq_MEMdq_IMMb" ),
      ( "VCMPSS_XMMdq_XMMdq_MEMd_IMMb" | "VCMPSD_XMMdq_XMMdq_MEMq_IMMb"
      | "VCMPPS_XMMdq_XMMdq_MEMdq_IMMb" | "VCMPPD_XMMdq_XMMdq_MEMdq_IMMb" ) ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSHUFD_XMMdq_XMMdq_IMMb" | "x86:VPSHUFLW_XMMdq_XMMdq_IMMb"
      | "x86:VPSHUFHW_XMMdq_XMMdq_IMMb" ),
      ("VPSHUFD_XMMdq_XMMdq_IMMb" | "VPSHUFLW_XMMdq_XMMdq_IMMb" | "VPSHUFHW_XMMdq_XMMdq_IMMb") ) ->
      true
  | ( (Target.X86_32 | Target.X86_64),
      ( "x86:VPSHUFD_XMMdq_MEMdq_IMMb" | "x86:VPSHUFLW_XMMdq_MEMdq_IMMb"
      | "x86:VPSHUFHW_XMMdq_MEMdq_IMMb" ),
      ("VPSHUFD_XMMdq_MEMdq_IMMb" | "VPSHUFLW_XMMdq_MEMdq_IMMb" | "VPSHUFHW_XMMdq_MEMdq_IMMb") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:fadd.s" | "riscv:fsub.s" | "riscv:fmul.s" | "riscv:fdiv.s" | "riscv:fadd.d"
      | "riscv:fsub.d" | "riscv:fmul.d" | "riscv:fdiv.d" ),
      ("fadd.s" | "fsub.s" | "fmul.s" | "fdiv.s" | "fadd.d" | "fsub.d" | "fmul.d" | "fdiv.d") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:flw" | "riscv:fld" | "riscv:fsw" | "riscv:fsd"),
      ("flw" | "fld" | "fsw" | "fsd") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:fsgnj.s" | "riscv:fsgnjn.s" | "riscv:fsgnjx.s" | "riscv:fsgnj.d" | "riscv:fsgnjn.d"
      | "riscv:fsgnjx.d" ),
      ("fsgnj.s" | "fsgnjn.s" | "fsgnjx.s" | "fsgnj.d" | "fsgnjn.d" | "fsgnjx.d") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:fmin.s" | "riscv:fmax.s" | "riscv:fmin.d" | "riscv:fmax.d"),
      ("fmin.s" | "fmax.s" | "fmin.d" | "fmax.d") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:fsqrt.s" | "riscv:fsqrt.d" | "riscv:fclass.s" | "riscv:fclass.d"),
      ("fsqrt.s" | "fsqrt.d" | "fclass.s" | "fclass.d") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:fmadd.s" | "riscv:fmsub.s" | "riscv:fnmsub.s" | "riscv:fnmadd.s" | "riscv:fmadd.d"
      | "riscv:fmsub.d" | "riscv:fnmsub.d" | "riscv:fnmadd.d" ),
      ( "fmadd.s" | "fmsub.s" | "fnmsub.s" | "fnmadd.s" | "fmadd.d" | "fmsub.d" | "fnmsub.d"
      | "fnmadd.d" ) ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:feq.s" | "riscv:fle.s" | "riscv:flt.s" | "riscv:feq.d" | "riscv:fle.d" | "riscv:flt.d"),
      ("feq.s" | "fle.s" | "flt.s" | "feq.d" | "fle.d" | "flt.d") ) ->
      true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fmv.x.w", "fmv.x.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fmv.w.x", "fmv.w.x" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.w.s", "fcvt.w.s" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.wu.s", "fcvt.wu.s" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.s.w", "fcvt.s.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.s.wu", "fcvt.s.wu" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.w.d", "fcvt.w.d" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.wu.d", "fcvt.wu.d" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.d.w", "fcvt.d.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.d.wu", "fcvt.d.wu" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.s.d", "fcvt.s.d" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:fcvt.d.s", "fcvt.d.s" -> true
  | Target.Riscv64, "riscv:fcvt.l.d", "fcvt.l.d" -> true
  | Target.Riscv64, "riscv:fcvt.lu.d", "fcvt.lu.d" -> true
  | Target.Riscv64, "riscv:fcvt.d.l", "fcvt.d.l" -> true
  | Target.Riscv64, "riscv:fcvt.d.lu", "fcvt.d.lu" -> true
  | Target.Riscv64, "riscv:fcvt.l.s", "fcvt.l.s" -> true
  | Target.Riscv64, "riscv:fcvt.lu.s", "fcvt.lu.s" -> true
  | Target.Riscv64, "riscv:fcvt.s.l", "fcvt.s.l" -> true
  | Target.Riscv64, "riscv:fcvt.s.lu", "fcvt.s.lu" -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:sh1add" | "riscv:sh2add" | "riscv:sh3add"),
      ("sh1add" | "sh2add" | "sh3add") ) ->
      true
  | ( Target.Riscv64,
      ("riscv:sh1add.uw" | "riscv:sh2add.uw" | "riscv:sh3add.uw"),
      ("sh1add.uw" | "sh2add.uw" | "sh3add.uw") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:min" | "riscv:minu" | "riscv:max" | "riscv:maxu"),
      ("min" | "minu" | "max" | "maxu") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:andn" | "riscv:orn" | "riscv:xnor" | "riscv:rol" | "riscv:ror"),
      ("andn" | "orn" | "xnor" | "rol" | "ror") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:clz" | "riscv:ctz" | "riscv:cpop" | "riscv:sext.b" | "riscv:sext.h" | "riscv:orc.b"),
      ("clz" | "ctz" | "cpop" | "sext.b" | "sext.h" | "orc.b") ) ->
      true
  | Target.Riscv64, ("riscv:clzw" | "riscv:ctzw" | "riscv:cpopw"), ("clzw" | "ctzw" | "cpopw") ->
      true
  | (Target.Riscv32 | Target.Riscv64), "riscv:brev8", "brev8" -> true
  | Target.Riscv64, "riscv:rev8", "rev8" -> true
  | Target.Riscv32, "riscv:rev8", "rev8.rv32" -> true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:pack" | "riscv:packh"), ("pack" | "packh") -> true
  | Target.Riscv64, "riscv:packw", "packw" -> true
  | Target.Riscv32, ("riscv:zip" | "riscv:unzip"), ("zip" | "unzip") -> true
  | Target.Riscv64, ("riscv:rolw" | "riscv:rorw"), ("rolw" | "rorw") -> true
  | Target.Riscv64, "riscv:rori", "rori" -> true
  | Target.Riscv32, "riscv:rori", "rori.rv32" -> true
  | Target.Riscv64, "riscv:roriw", "roriw" -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:bclr" | "riscv:bext" | "riscv:binv" | "riscv:bset"),
      ("bclr" | "bext" | "binv" | "bset") ) ->
      true
  | ( Target.Riscv64,
      ("riscv:bclri" | "riscv:bexti" | "riscv:binvi" | "riscv:bseti"),
      ("bclri" | "bexti" | "binvi" | "bseti") ) ->
      true
  | ( Target.Riscv32,
      ("riscv:bclri" | "riscv:bexti" | "riscv:binvi" | "riscv:bseti"),
      ("bclri.rv32" | "bexti.rv32" | "binvi.rv32" | "bseti.rv32") ) ->
      true
  | Target.Riscv64, "riscv:zext.h", "zext.h" -> true
  | Target.Riscv32, "riscv:zext.h", "zext.h.rv32" -> true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:clmul" | "riscv:clmulh"), ("clmul" | "clmulh") ->
      true
  | (Target.Riscv32 | Target.Riscv64), "riscv:clmulr", "clmulr" -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:czero.eqz" | "riscv:czero.nez"),
      ("czero.eqz" | "czero.nez") ) ->
      true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:sm3p0" | "riscv:sm3p1"), ("sm3p0" | "sm3p1") -> true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:sm4ed" | "riscv:sm4ks"), ("sm4ed" | "sm4ks") -> true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:xperm4" | "riscv:xperm8"), ("xperm4" | "xperm8") ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:sha256sum0" | "riscv:sha256sum1" | "riscv:sha256sig0" | "riscv:sha256sig1"),
      ("sha256sum0" | "sha256sum1" | "sha256sig0" | "sha256sig1") ) ->
      true
  | ( Target.Riscv64,
      ("riscv:sha512sum0" | "riscv:sha512sum1" | "riscv:sha512sig0" | "riscv:sha512sig1"),
      ("sha512sum0" | "sha512sum1" | "sha512sig0" | "sha512sig1") ) ->
      true
  | ( Target.Riscv32,
      ( "riscv:sha512sum0r" | "riscv:sha512sum1r" | "riscv:sha512sig0l" | "riscv:sha512sig1l"
      | "riscv:sha512sig0h" | "riscv:sha512sig1h" ),
      ("sha512sum0r" | "sha512sum1r" | "sha512sig0l" | "sha512sig1l" | "sha512sig0h" | "sha512sig1h")
    ) ->
      true
  | ( Target.Riscv64,
      ( "riscv:aes64ds" | "riscv:aes64dsm" | "riscv:aes64es" | "riscv:aes64esm" | "riscv:aes64ks2"
      | "riscv:aes64im" ),
      ("aes64ds" | "aes64dsm" | "aes64es" | "aes64esm" | "aes64ks2" | "aes64im") ) ->
      true
  | Target.Riscv64, "riscv:aes64ks1i", "aes64ks1i" -> true
  | ( Target.Riscv32,
      ("riscv:aes32dsi" | "riscv:aes32dsmi" | "riscv:aes32esi" | "riscv:aes32esmi"),
      ("aes32dsi" | "aes32dsmi" | "aes32esi" | "aes32esmi") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:csrrw" | "riscv:csrrs" | "riscv:csrrc" | "riscv:csrrwi" | "riscv:csrrsi"
      | "riscv:csrrci" ),
      ("csrrw" | "csrrs" | "csrrc" | "csrrwi" | "csrrsi" | "csrrci") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:csrr" | "riscv:csrw" | "riscv:csrs" | "riscv:csrc" | "riscv:csrwi" | "riscv:csrsi"
      | "riscv:csrci" ),
      ("csrr" | "csrw" | "csrs" | "csrc" | "csrwi" | "csrsi" | "csrci") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:amoswap.w" | "riscv:amoadd.w" | "riscv:amoxor.w" | "riscv:amoand.w" | "riscv:amoor.w"
      | "riscv:amomin.w" | "riscv:amomax.w" | "riscv:amominu.w" | "riscv:amomaxu.w" | "riscv:sc.w"
      | "riscv:lr.w" ),
      ( "amoswap.w" | "amoadd.w" | "amoxor.w" | "amoand.w" | "amoor.w" | "amomin.w" | "amomax.w"
      | "amominu.w" | "amomaxu.w" | "sc.w" | "lr.w" ) ) ->
      true
  | ( Target.Riscv64,
      ( "riscv:amoswap.d" | "riscv:amoadd.d" | "riscv:amoxor.d" | "riscv:amoand.d" | "riscv:amoor.d"
      | "riscv:amomin.d" | "riscv:amomax.d" | "riscv:amominu.d" | "riscv:amomaxu.d" | "riscv:sc.d"
      | "riscv:lr.d" ),
      ( "amoswap.d" | "amoadd.d" | "amoxor.d" | "amoand.d" | "amoor.d" | "amomin.d" | "amomax.d"
      | "amominu.d" | "amomaxu.d" | "sc.d" | "lr.d" ) ) ->
      true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsetvl", "vsetvl" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsetvli", "vsetvli" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsetivli", "vsetivli" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadd.vv", "vadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadd.vx", "vadd.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadd.vi", "vadd.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsub.vv", "vsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsub.vx", "vsub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrsub.vx", "vrsub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrsub.vi", "vrsub.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vand.vv", "vand.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vand.vx", "vand.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vand.vi", "vand.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vor.vv", "vor.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vor.vx", "vor.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vor.vi", "vor.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vxor.vv", "vxor.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vxor.vx", "vxor.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vxor.vi", "vxor.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsll.vv", "vsll.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsll.vx", "vsll.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsll.vi", "vsll.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsrl.vv", "vsrl.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsrl.vx", "vsrl.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsrl.vi", "vsrl.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsra.vv", "vsra.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsra.vx", "vsra.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsra.vi", "vsra.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vminu.vv", "vminu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vminu.vx", "vminu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmin.vv", "vmin.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmin.vx", "vmin.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmaxu.vv", "vmaxu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmaxu.vx", "vmaxu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmax.vv", "vmax.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmax.vx", "vmax.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmul.vv", "vmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmul.vx", "vmul.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulh.vv", "vmulh.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulh.vx", "vmulh.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulhu.vv", "vmulhu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulhu.vx", "vmulhu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulhsu.vv", "vmulhsu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmulhsu.vx", "vmulhsu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vdivu.vv", "vdivu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vdivu.vx", "vdivu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vdiv.vv", "vdiv.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vdiv.vx", "vdiv.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vremu.vv", "vremu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vremu.vx", "vremu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrem.vv", "vrem.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrem.vx", "vrem.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsaddu.vv", "vsaddu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsaddu.vx", "vsaddu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsaddu.vi", "vsaddu.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsadd.vv", "vsadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsadd.vx", "vsadd.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsadd.vi", "vsadd.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssubu.vv", "vssubu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssubu.vx", "vssubu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssub.vv", "vssub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssub.vx", "vssub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaaddu.vv", "vaaddu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaaddu.vx", "vaaddu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaadd.vv", "vaadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaadd.vx", "vaadd.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vasubu.vv", "vasubu.vv" -> true
  | _ -> false

let promoted_case_part6 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | (Target.Riscv32 | Target.Riscv64), "riscv:vasubu.vx", "vasubu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vasub.vv", "vasub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vasub.vx", "vasub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsrl.wv", "vnsrl.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsrl.wx", "vnsrl.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsrl.wi", "vnsrl.wi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsra.wv", "vnsra.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsra.wx", "vnsra.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnsra.wi", "vnsra.wi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclipu.wv", "vnclipu.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclipu.wx", "vnclipu.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclipu.wi", "vnclipu.wi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclip.wv", "vnclip.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclip.wx", "vnclip.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnclip.wi", "vnclip.wi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssrl.vv", "vssrl.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssrl.vx", "vssrl.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssrl.vi", "vssrl.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssra.vv", "vssra.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssra.vx", "vssra.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vssra.vi", "vssra.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrgather.vv", "vrgather.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrgather.vx", "vrgather.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrgather.vi", "vrgather.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrgatherei16.vv", "vrgatherei16.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwaddu.vv", "vwaddu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwaddu.vx", "vwaddu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwadd.vv", "vwadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwadd.vx", "vwadd.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsubu.vv", "vwsubu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsubu.vx", "vwsubu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsub.vv", "vwsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsub.vx", "vwsub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwaddu.wv", "vwaddu.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwaddu.wx", "vwaddu.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwadd.wv", "vwadd.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwadd.wx", "vwadd.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsubu.wv", "vwsubu.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsubu.wx", "vwsubu.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsub.wv", "vwsub.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsub.wx", "vwsub.wx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmulu.vv", "vwmulu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmulu.vx", "vwmulu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmulsu.vv", "vwmulsu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmulsu.vx", "vwmulsu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmul.vv", "vwmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmul.vx", "vwmul.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsext.vf2", "vsext.vf2" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsext.vf4", "vsext.vf4" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsext.vf8", "vsext.vf8" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vzext.vf2", "vzext.vf2" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vzext.vf4", "vzext.vf4" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vzext.vf8", "vzext.vf8" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmand.mm", "vmand.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmandn.mm", "vmandn.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmor.mm", "vmor.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmxor.mm", "vmxor.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmorn.mm", "vmorn.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmnand.mm", "vmnand.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmnor.mm", "vmnor.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmxnor.mm", "vmxnor.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredsum.vs", "vredsum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredand.vs", "vredand.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredor.vs", "vredor.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredxor.vs", "vredxor.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredminu.vs", "vredminu.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredmin.vs", "vredmin.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredmaxu.vs", "vredmaxu.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vredmax.vs", "vredmax.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwredsumu.vs", "vwredsumu.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwredsum.vs", "vwredsum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmseq.vv", "vmseq.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmseq.vx", "vmseq.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmseq.vi", "vmseq.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsne.vv", "vmsne.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsne.vx", "vmsne.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsne.vi", "vmsne.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsltu.vv", "vmsltu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsltu.vx", "vmsltu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmslt.vv", "vmslt.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmslt.vx", "vmslt.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsleu.vv", "vmsleu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsleu.vx", "vmsleu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsleu.vi", "vmsleu.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsle.vv", "vmsle.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsle.vx", "vmsle.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsle.vi", "vmsle.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsgtu.vx", "vmsgtu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsgtu.vi", "vmsgtu.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsgt.vx", "vmsgt.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsgt.vi", "vmsgt.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslideup.vx", "vslideup.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslideup.vi", "vslideup.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslidedown.vx", "vslidedown.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslidedown.vi", "vslidedown.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslide1up.vx", "vslide1up.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vslide1down.vx", "vslide1down.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmacc.vv", "vmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmacc.vx", "vmacc.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnmsac.vv", "vnmsac.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnmsac.vx", "vnmsac.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadd.vv", "vmadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadd.vx", "vmadd.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnmsub.vv", "vnmsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vnmsub.vx", "vnmsub.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmaccu.vv", "vwmaccu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmaccu.vx", "vwmaccu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmacc.vv", "vwmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmacc.vx", "vwmacc.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmaccsu.vv", "vwmaccsu.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmaccsu.vx", "vwmaccsu.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwmaccus.vx", "vwmaccus.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vid.v", "vid.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:viota.m", "viota.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vcompress.vm", "vcompress.vm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsbf.m", "vmsbf.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsif.m", "vmsif.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsof.m", "vmsof.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vcpop.m", "vcpop.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfirst.m", "vfirst.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadc.vvm", "vadc.vvm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadc.vxm", "vadc.vxm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vadc.vim", "vadc.vim" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vvm", "vmadc.vvm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vxm", "vmadc.vxm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vim", "vmadc.vim" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vv", "vmadc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vx", "vmadc.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmadc.vi", "vmadc.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsbc.vvm", "vsbc.vvm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsbc.vxm", "vsbc.vxm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsbc.vvm", "vmsbc.vvm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsbc.vxm", "vmsbc.vxm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsbc.vv", "vmsbc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmsbc.vx", "vmsbc.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmerge.vvm", "vmerge.vvm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmerge.vxm", "vmerge.vxm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmerge.vim", "vmerge.vim" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv.x.s", "vmv.x.s" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv.s.x", "vmv.s.x" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv.v.v", "vmv.v.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv.v.x", "vmv.v.x" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv.v.i", "vmv.v.i" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv1r.v", "vmv1r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv2r.v", "vmv2r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv4r.v", "vmv4r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmv8r.v", "vmv8r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsmul.vv", "vsmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsmul.vx", "vsmul.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfadd.vv", "vfadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfadd.vf", "vfadd.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsub.vv", "vfsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsub.vf", "vfsub.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfrsub.vf", "vfrsub.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmul.vv", "vfmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmul.vf", "vfmul.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfdiv.vv", "vfdiv.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfdiv.vf", "vfdiv.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfrdiv.vf", "vfrdiv.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmin.vv", "vfmin.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmin.vf", "vfmin.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmax.vv", "vfmax.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmax.vf", "vfmax.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnj.vv", "vfsgnj.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnj.vf", "vfsgnj.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnjn.vv", "vfsgnjn.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnjn.vf", "vfsgnjn.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnjx.vv", "vfsgnjx.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsgnjx.vf", "vfsgnjx.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfsqrt.v", "vfsqrt.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfrsqrt7.v", "vfrsqrt7.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfrec7.v", "vfrec7.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfclass.v", "vfclass.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfredosum.vs", "vfredosum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfredusum.vs", "vfredusum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfredmin.vs", "vfredmin.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfredmax.vs", "vfredmax.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfeq.vv", "vmfeq.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfeq.vf", "vmfeq.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfle.vv", "vmfle.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfle.vf", "vmfle.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmflt.vv", "vmflt.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmflt.vf", "vmflt.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfne.vv", "vmfne.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfne.vf", "vmfne.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfgt.vf", "vmfgt.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmfge.vf", "vmfge.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmv.f.s", "vfmv.f.s" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmv.s.f", "vfmv.s.f" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmv.v.f", "vfmv.v.f" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmerge.vfm", "vfmerge.vfm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.xu.f.v", "vfcvt.xu.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.x.f.v", "vfcvt.x.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.f.xu.v", "vfcvt.f.xu.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.f.x.v", "vfcvt.f.x.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.rtz.xu.f.v", "vfcvt.rtz.xu.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfcvt.rtz.x.f.v", "vfcvt.rtz.x.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmadd.vv", "vfmadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmadd.vf", "vfmadd.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmadd.vv", "vfnmadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmadd.vf", "vfnmadd.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmsub.vv", "vfmsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmsub.vf", "vfmsub.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmsub.vv", "vfnmsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmsub.vf", "vfnmsub.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmacc.vv", "vfmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmacc.vf", "vfmacc.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmacc.vv", "vfnmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmacc.vf", "vfnmacc.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmsac.vv", "vfmsac.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfmsac.vf", "vfmsac.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmsac.vv", "vfnmsac.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfnmsac.vf", "vfnmsac.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfslide1up.vf", "vfslide1up.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfslide1down.vf", "vfslide1down.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwadd.vv", "vfwadd.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwadd.vf", "vfwadd.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwadd.wv", "vfwadd.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwadd.wf", "vfwadd.wf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwsub.vv", "vfwsub.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwsub.vf", "vfwsub.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwsub.wv", "vfwsub.wv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwsub.wf", "vfwsub.wf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmul.vv", "vfwmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmul.vf", "vfwmul.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwredosum.vs", "vfwredosum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwredusum.vs", "vfwredusum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.xu.f.v", "vfwcvt.xu.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.x.f.v", "vfwcvt.x.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.f.xu.v", "vfwcvt.f.xu.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.f.x.v", "vfwcvt.f.x.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.f.f.v", "vfwcvt.f.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.rtz.xu.f.v", "vfwcvt.rtz.xu.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvt.rtz.x.f.v", "vfwcvt.rtz.x.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.xu.f.w", "vfncvt.xu.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.x.f.w", "vfncvt.x.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.f.xu.w", "vfncvt.f.xu.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.f.x.w", "vfncvt.f.x.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.f.f.w", "vfncvt.f.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.rod.f.f.w", "vfncvt.rod.f.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.rtz.xu.f.w", "vfncvt.rtz.xu.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvt.rtz.x.f.w", "vfncvt.rtz.x.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmacc.vv", "vfwmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmacc.vf", "vfwmacc.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwnmacc.vv", "vfwnmacc.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwnmacc.vf", "vfwnmacc.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmsac.vv", "vfwmsac.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmsac.vf", "vfwmsac.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwnmsac.vv", "vfwnmsac.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwnmsac.vf", "vfwnmsac.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle8.v", "vle8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle16.v", "vle16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle32.v", "vle32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle64.v", "vle64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vse8.v", "vse8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vse16.v", "vse16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vse32.v", "vse32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vse64.v", "vse64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vlm.v", "vlm.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm.v", "vsm.v" -> true
  | _ -> false

let promoted_case_part7 ~target ~form_id ~lookup_key =
  match (target, form_id, lookup_key) with
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle8ff.v", "vle8ff.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle16ff.v", "vle16ff.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle32ff.v", "vle32ff.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle64ff.v", "vle64ff.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vlse8.v", "vlse8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vlse16.v", "vlse16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vlse32.v", "vlse32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vlse64.v", "vlse64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsse8.v", "vsse8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsse16.v", "vsse16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsse32.v", "vsse32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsse64.v", "vsse64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vluxei8.v", "vluxei8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vluxei16.v", "vluxei16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vluxei32.v", "vluxei32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vluxei64.v", "vluxei64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vloxei8.v", "vloxei8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vloxei16.v", "vloxei16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vloxei32.v", "vloxei32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vloxei64.v", "vloxei64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsuxei8.v", "vsuxei8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsuxei16.v", "vsuxei16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsuxei32.v", "vsuxei32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsuxei64.v", "vsuxei64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsoxei8.v", "vsoxei8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsoxei16.v", "vsoxei16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsoxei32.v", "vsoxei32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsoxei64.v", "vsoxei64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl1re8.v", "vl1re8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl1re16.v", "vl1re16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl1re32.v", "vl1re32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl1re64.v", "vl1re64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl2re8.v", "vl2re8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl2re16.v", "vl2re16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl2re32.v", "vl2re32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl2re64.v", "vl2re64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl4re8.v", "vl4re8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl4re16.v", "vl4re16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl4re32.v", "vl4re32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl4re64.v", "vl4re64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl8re8.v", "vl8re8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl8re16.v", "vl8re16.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl8re32.v", "vl8re32.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl8re64.v", "vl8re64.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vs1r.v", "vs1r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vs2r.v", "vs2r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vs4r.v", "vs4r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vs8r.v", "vs8r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vclmul.vv", "vclmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vclmul.vx", "vclmul.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vclmulh.vv", "vclmulh.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vclmulh.vx", "vclmulh.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vghsh.vv", "vghsh.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vgmul.vv", "vgmul.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsha2ms.vv", "vsha2ms.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsha2ch.vv", "vsha2ch.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsha2cl.vv", "vsha2cl.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm4k.vi", "vsm4k.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm4r.vv", "vsm4r.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm4r.vs", "vsm4r.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm3c.vi", "vsm3c.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vsm3me.vv", "vsm3me.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vandn.vv", "vandn.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vandn.vx", "vandn.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vbrev.v", "vbrev.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vbrev8.v", "vbrev8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vclz.v", "vclz.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vcpop.v", "vcpop.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vctz.v", "vctz.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrev8.v", "vrev8.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrol.vv", "vrol.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vrol.vx", "vrol.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vror.vv", "vror.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vror.vx", "vror.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vror.vi", "vror.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsll.vv", "vwsll.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsll.vx", "vwsll.vx" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vwsll.vi", "vwsll.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesdf.vv", "vaesdf.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesdf.vs", "vaesdf.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesdm.vv", "vaesdm.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesdm.vs", "vaesdm.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesef.vv", "vaesef.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesef.vs", "vaesef.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesem.vv", "vaesem.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesem.vs", "vaesem.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaesz.vs", "vaesz.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaeskf1.vi", "vaeskf1.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vaeskf2.vi", "vaeskf2.vi" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwcvtbf16.f.f.v", "vfwcvtbf16.f.f.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfncvtbf16.f.f.w", "vfncvtbf16.f.f.w" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmaccbf16.vv", "vfwmaccbf16.vv" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwmaccbf16.vf", "vfwmaccbf16.vf" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vpopc.m", "vpopc.m" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmandnot.mm", "vmandnot.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vmornot.mm", "vmornot.mm" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfredsum.vs", "vfredsum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vfwredsum.vs", "vfwredsum.vs" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl1r.v", "vl1r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl2r.v", "vl2r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl4r.v", "vl4r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vl8r.v", "vl8r.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vle1.v", "vle1.v" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:vse1.v", "vse1.v" -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:mv" | "riscv:snez" | "riscv:nop" | "riscv:ret" | "riscv:neg" | "riscv:seqz"
      | "riscv:sltz" | "riscv:sgtz" | "riscv:zext.b" ),
      ("mv" | "snez" | "nop" | "ret" | "neg" | "seqz" | "sltz" | "sgtz" | "zext.b") ) ->
      true
  | Target.Riscv64, "riscv:sext.w", "sext.w" -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ( "riscv:fneg.s" | "riscv:fneg.d" | "riscv:fabs.s" | "riscv:fabs.d" | "riscv:fmv.s"
      | "riscv:fmv.d" | "riscv:fmv.x.s" | "riscv:fmv.s.x" ),
      ("fneg.s" | "fneg.d" | "fabs.s" | "fabs.d" | "fmv.s" | "fmv.d" | "fmv.x.s" | "fmv.s.x") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:c.and" | "riscv:c.or" | "riscv:c.xor" | "riscv:c.sub"),
      ("c.and" | "c.or" | "c.xor" | "c.sub") ) ->
      true
  | Target.Riscv64, ("riscv:c.addw" | "riscv:c.subw"), ("c.addw" | "c.subw") -> true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:c.jr" | "riscv:c.jalr" | "riscv:c.mv" | "riscv:c.add" | "riscv:c.ebreak"),
      ("c.jr" | "c.jalr" | "c.mv" | "c.add" | "c.ebreak") ) ->
      true
  | ( (Target.Riscv32 | Target.Riscv64),
      ("riscv:c.lw" | "riscv:c.sw" | "riscv:c.lwsp" | "riscv:c.swsp"),
      ("c.lw" | "c.sw" | "c.lwsp" | "c.swsp") ) ->
      true
  | ( Target.Riscv64,
      ("riscv:c.ld" | "riscv:c.sd" | "riscv:c.ldsp" | "riscv:c.sdsp"),
      ("c.ld" | "c.sd" | "c.ldsp" | "c.sdsp") ) ->
      true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:c.beqz" | "riscv:c.bnez"), ("c.beqz" | "c.bnez") ->
      true
  | _ -> false

(* These are the exact credit-bearing rows of the committed S3 pilot
   corpus, plus the isa-difficult corpus (sw/beq/c.addi - every one of
   whose committed records carries verdict=Pass, checked in
   asm/fixtures/isa-difficult/cases.jsonl: beq's own per-instruction
   Isa_gen_oracle finding reads Different_observed_form only because its
   two-instruction rendered source is longer than that single-form check's
   fixed-width assumption, not because GAS or "ours" picked a different form -
   the credit-bearing signal is Isa_gen_verdict's direct GAS-vs-ours byte
   comparison, which is Pass for all twenty committed cases; see
   Isa_gen_difficult.mli's own comment on beq_entries).  A pilot/difficult
   spelling that reaches a different GNU encoding or that our parser rejects
   is deliberately only GAS-generatable; it cannot gain support credit merely
   because a neighboring form happens to encode the same operation. *)
let promoted_case ~target ~form_id ~lookup_key =
  promoted_case_part1 ~target ~form_id ~lookup_key
  || promoted_case_part2 ~target ~form_id ~lookup_key
  || promoted_case_part3 ~target ~form_id ~lookup_key
  || promoted_case_part4 ~target ~form_id ~lookup_key
  || promoted_case_part5 ~target ~form_id ~lookup_key
  || promoted_case_part6 ~target ~form_id ~lookup_key
  || promoted_case_part7 ~target ~form_id ~lookup_key

let pilot_case ~target ~form_id ~lookup_key =
  List.exists
    (fun (entry : Isa_gen_pilot.pilot_entry) ->
      entry.target = target
      && String.equal entry.form_id form_id
      && String.equal entry.lookup_key lookup_key)
    Isa_gen_pilot.all
  || List.exists
       (fun (entry : Isa_gen_difficult.entry) ->
         entry.target = target
         && String.equal entry.form_id form_id
         && String.equal entry.lookup_key lookup_key)
       Isa_gen_difficult.all

let lookup_key source (rec_ : Isa_source_record.t) =
  match (source, rec_.provenance) with
  | "riscv_opcodes", _ -> rec_.native_name
  | "xed_resolved", Isa_source_record.Xed_provenance { iform = Some iform; _ } -> iform
  | _ -> ""

let state_of ~source ~target rec_ =
  match normalize source rec_ with
  | Error diagnostic -> Blocked diagnostic.rule
  | Ok (form : Isa_norm_model.form) ->
      let key = lookup_key source rec_ in
      if promoted_case ~target ~form_id:form.form_id ~lookup_key:key then Promoted_support
      else if pilot_case ~target ~form_id:form.form_id ~lookup_key:key then Gas_generatable
      else Normalized_only

let summarize repo ~source target =
  let ( let* ) = Result.bind in
  let* records = Isa_source_record.read_file (Repo.isa_db_export repo ~source target) in
  let tallies = Hashtbl.create 64 in
  List.iter
    (fun (rec_ : Isa_source_record.t) ->
      let name = family_of rec_ in
      let tally =
        match Hashtbl.find_opt tallies name with
        | Some tally -> tally
        | None ->
            let tally = empty () in
            Hashtbl.add tallies name tally;
            tally
      in
      record tally (state_of ~source ~target rec_))
    records;
  let families =
    Hashtbl.to_seq tallies |> List.of_seq
    |> List.map (fun (name, tally) ->
        { name; total = tally_total (tally_of tally); tally = tally_of tally })
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  Ok { total = List.length records; families }

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
  header :: List.map line summary.families

let targets_and_sources =
  [
    ("riscv_opcodes", Target.Riscv32);
    ("riscv_opcodes", Target.Riscv64);
    ("xed_resolved", Target.X86_32);
    ("xed_resolved", Target.X86_64);
  ]

let run repo =
  Command.accumulate targets_and_sources ~f:(fun (source, target) ->
      match summarize repo ~source target with
      | Error e -> Command.of_error e
      | Ok summary ->
          let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
          Command.ok (List.map Diagnostic.stdout (report_lines ~label summary)))
