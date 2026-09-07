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
