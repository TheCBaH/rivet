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
  match (target, form_id, lookup_key) with
  | (Target.Riscv32 | Target.Riscv64), ("riscv:add" | "riscv:sub" | "riscv:mul" | "riscv:addi"), _
    ->
      true
  | Target.Riscv64, "riscv:addw", _ -> true
  | (Target.X86_32 | Target.X86_64), "x86:ADD_GPRv_GPRv_01", "ADD_GPRv_GPRv_01" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:sw", "sw" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:beq", "beq" -> true
  | (Target.Riscv32 | Target.Riscv64), "riscv:c.addi", "c.addi" -> true
  | (Target.X86_32 | Target.X86_64), "x86:MOV_GPRv_MEMv", "MOV_GPRv_MEMv" -> true
  | (Target.X86_32 | Target.X86_64), "x86:MOV_MEMv_GPRv", "MOV_MEMv_GPRv" -> true
  | (Target.X86_32 | Target.X86_64), "x86:FADD_ST0_X87", "FADD_ST0_X87" -> true
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
  | Target.Riscv64, "riscv:zext.h", "zext.h" -> true
  | Target.Riscv32, "riscv:zext.h", "zext.h.rv32" -> true
  | (Target.Riscv32 | Target.Riscv64), ("riscv:clmul" | "riscv:clmulh"), ("clmul" | "clmulh") ->
      true
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
  | _ -> false

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
