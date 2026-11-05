(* The arm (A32) target description (docs/design.md §5.1): the encoder; its instruction types are in {!Arm_types}, the per-opcode table in {!Arm_ops}.

   Not in a family functor with AArch64, per §5.1's explicit instruction: they
   share a vendor and nothing else. A32 is a fixed 32-bit word with a condition
   field on every instruction and a rotate-based immediate relation; A64 has no
   condition field, no rotation, and a different register file. Forcing them
   together would produce a functor whose parameter had to supply the entire
   encoding.

   Byte order. A form here is authored as one 32-bit word, most significant bit
   first, so [Bits.to_bytes] gives the big-endian rendering and {!encode}
   reverses it. Both directions are little-endian in memory, and every committed
   artifact is in memory order. *)

open Foundation
include Arm_types
module Arm_ops = Arm_ops

(* What the types leave to the table: spellings, parsing, the data-processing opcode field, the
   extra loads' bits, the [rd, rm] pairs and the VFP selectors all come from {!Arm_ops}. *)
module V3 = struct
  include V3

  let name = Arm_ops.v3_name
  let selector = Arm_ops.v3_selector
  let of_selector = Arm_ops.v3_of_selector
end

module Opcode = struct
  include Opcode

  let name = Arm_ops.name
  let of_mnemonic = Arm_ops.of_mnemonic
  let split_vfp_mnemonic = Arm_ops.split_vfp_mnemonic
  let vfp_opcode_of = Arm_ops.vfp_opcode_of
  let to_dp = Arm_ops.to_dp
  let of_dp = Arm_ops.of_dp
  let is_compare = Arm_ops.is_compare
  let all = Arm_ops.opcodes
end

let x_name = Arm_ops.x_name
let x_bits = Arm_ops.x_bits
let x_kind_of_bits = Arm_ops.x_kind_of_bits

module Instruction = struct
  include Instruction

  (* The condition is a mnemonic suffix on the way out as well as in, and [al]
     is spelled by its absence. For a VFP opcode, [Opcode.name] already
     carries its own [.f64]/[.f32]/[.f32.f64]-shaped type suffix, and the
     condition has to land *before* that first dot ([vmoveq.f64], not
     [vmov.f64eq] - the latter is not an instruction GAS accepts), so it is
     spliced in there rather than appended at the end. *)
  let pp ppf i =
    let name = Opcode.name i.op and c = Cond.suffix i.cond in
    let m =
      match String.index_opt name '.' with
      | Some dot -> String.sub name 0 dot ^ c ^ String.sub name dot (String.length name - dot)
      | None -> name ^ c
    in
    match i.ops with
    | [] -> Fmt.string ppf m
    | ops -> Fmt.pf ppf "%s %a" m Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

module Lowered = struct
  include Lowered

  let pp ppf = function
    | Dp_imm { cond; dp; rd; rn; imm; _ } -> (
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
            Fmt.pf ppf "%s%s %a, #%Ld" (Opcode.name o) c Reg.pp rd imm
        (* A compare writes no destination, so printing [Rd] would print the
           zero the encoding requires as though it were an operand. *)
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, #%Ld" (Opcode.name o) c Reg.pp rn imm
        | Some o -> Fmt.pf ppf "%s%s %a, %a, #%Ld" (Opcode.name o) c Reg.pp rd Reg.pp rn imm
        | None -> Fmt.pf ppf "dp%d%s %a, %a, #%Ld" dp c Reg.pp rd Reg.pp rn imm)
    | Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } -> (
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
            Fmt.pf ppf "%s%s %a, %a%s" (Opcode.name o) c Reg.pp rd Reg.pp rm sh
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, %a%s" (Opcode.name o) c Reg.pp rn Reg.pp rm sh
        (* [adds] - {!Add}'s own flag-setting form. [dp] alone cannot distinguish it from plain
           [add]; only [s] can, exactly as {!Opcode.Adds}'s own doc explains. *)
        | Some Opcode.Add when s ->
            Fmt.pf ppf "adds%s %a, %a, %a%s" c Reg.pp rd Reg.pp rn Reg.pp rm sh
        | Some o ->
            Fmt.pf ppf "%s%s %a, %a, %a%s" (Opcode.name o) c Reg.pp rd Reg.pp rn Reg.pp rm sh
        | None -> Fmt.pf ppf "dp%d%s %a, %a, %a%s" dp c Reg.pp rd Reg.pp rn Reg.pp rm sh)
    | Nop { cond } -> Fmt.pf ppf "nop%s" (Cond.suffix cond)
    | Shift_reg { cond; kind; rd; rm; rs } ->
        Fmt.pf ppf "%s%s %a, %a, %a" (shift_name kind) (Cond.suffix cond) Reg.pp rd Reg.pp rm Reg.pp
          rs
    | Ldst_imm { cond; load; byte; rt; rn; offset; writeback; post } ->
        if post then
          Fmt.pf ppf "%s%s%s %a, [%a], #%Ld"
            (if load then "ldr" else "str")
            (if byte then "b" else "")
            (Cond.suffix cond) Reg.pp rt Reg.pp rn offset
        else
          Fmt.pf ppf "%s%s%s %a, [%a, #%Ld]%s"
            (if load then "ldr" else "str")
            (if byte then "b" else "")
            (Cond.suffix cond) Reg.pp rt Reg.pp rn offset
            (if writeback then "!" else "")
    | Ldst_x { cond; kind; rt; rn; offset; post } ->
        let off =
          match offset with
          | X_imm v -> Printf.sprintf "#%Ld" v
          | X_reg { rm; negate } -> (if negate then "-" else "") ^ Reg.name rm
        in
        if post then
          Fmt.pf ppf "%s%s %a, [%a], %s" (x_name kind) (Cond.suffix cond) Reg.pp rt Reg.pp rn off
        else Fmt.pf ppf "%s%s %a, [%a, %s]" (x_name kind) (Cond.suffix cond) Reg.pp rt Reg.pp rn off
    | Sbfx { cond; rd; rn; lsb; width } ->
        Fmt.pf ppf "sbfx%s %a, %a, #%d, #%d" (Cond.suffix cond) Reg.pp rd Reg.pp rn lsb width
    | Rd_rm { cond; op; rd; rm } ->
        Fmt.pf ppf "%s%s %a, %a" (Opcode.name op) (Cond.suffix cond) Reg.pp rd Reg.pp rm
    | Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } ->
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        Fmt.pf ppf "%s%s%s %a, [%a, %s%a%s]"
          (if load then "ldr" else "str")
          (if byte then "b" else "")
          (Cond.suffix cond) Reg.pp rt Reg.pp rn
          (if negate then "-" else "")
          Reg.pp rm sh
    | Bx { cond; rm } -> Fmt.pf ppf "bx%s %a" (Cond.suffix cond) Reg.pp rm
    | Push { cond; regs } ->
        let members =
          List.filter_map
            (fun n -> if regs land (1 lsl n) <> 0 then Some (Reg.of_num n) else None)
            (List.init 16 Fun.id)
        in
        Fmt.pf ppf "push%s {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Reg.pp) members
    | Ldm { cond; load; rn; regs } ->
        let members =
          List.filter_map
            (fun n -> if regs land (1 lsl n) <> 0 then Some (Reg.of_num n) else None)
            (List.init 16 Fun.id)
        in
        if load && Reg.equal rn Reg.sp && List.length members >= 2 then
          Fmt.pf ppf "pop%s {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Reg.pp) members
        else
          Fmt.pf ppf "%s%s %a!, {%a}"
            (if load then "ldmia" else "stmia")
            (Cond.suffix cond) Reg.pp rn
            Fmt.(list ~sep:(any ", ") Reg.pp)
            members
    | Udf { imm16 } -> Fmt.pf ppf "udf #%Ld" imm16
    | Bl { cond; target } ->
        Fmt.pf ppf "bl%s %a" (Cond.suffix cond) Asm_core.Lowered_ast.pp_branch target
    | B { cond; target } ->
        Fmt.pf ppf "b%s %a" (Cond.suffix cond) Asm_core.Lowered_ast.pp_branch target
    | Mul { cond; rd; rn; rm; _ } ->
        Fmt.pf ppf "mul%s %a, %a, %a" (Cond.suffix cond) Reg.pp rd Reg.pp rn Reg.pp rm
    | Mla { cond; rd; rn; rm; ra; _ } ->
        Fmt.pf ppf "mla%s %a, %a, %a, %a" (Cond.suffix cond) Reg.pp rd Reg.pp rn Reg.pp rm Reg.pp ra
    | Umull { cond; rdlo; rdhi; rn; rm; _ } ->
        Fmt.pf ppf "umull%s %a, %a, %a, %a" (Cond.suffix cond) Reg.pp rdlo Reg.pp rdhi Reg.pp rn
          Reg.pp rm
    | Movw_movt { cond; top; rd; imm } ->
        Fmt.pf ppf "%s%s %a, #%a"
          (if top then "movt" else "movw")
          (Cond.suffix cond) Reg.pp rd Asm_core.Lowered_ast.pp_branch imm
    | Vmov_reg_d { cond; vd; vm } ->
        Fmt.pf ppf "vmov%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vmov_reg_s { cond; vd; vm } ->
        Fmt.pf ppf "vmov%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vmov_imm_d { cond; vd; imm8 } ->
        Fmt.pf ppf "vmov%s.f64 %a, #%s" (Cond.suffix cond) Dreg.pp vd
          (decimal_of_float (vfp_expand imm8))
    | Vmov_imm_s { cond; vd; imm8 } ->
        Fmt.pf ppf "vmov%s.f32 %a, #%s" (Cond.suffix cond) Sreg.pp vd
          (decimal_of_float (vfp_expand imm8))
    | Vmov_to_single { cond; sn; rt } ->
        Fmt.pf ppf "vmov%s %a, %a" (Cond.suffix cond) Sreg.pp sn Reg.pp rt
    | Vmov_from_single { cond; rt; sn } ->
        Fmt.pf ppf "vmov%s %a, %a" (Cond.suffix cond) Reg.pp rt Sreg.pp sn
    | Vmov_to_double { cond; dm; rt; rt2 } ->
        Fmt.pf ppf "vmov%s %a, %a, %a" (Cond.suffix cond) Dreg.pp dm Reg.pp rt Reg.pp rt2
    | Vmov_from_double { cond; rt; rt2; dm } ->
        Fmt.pf ppf "vmov%s %a, %a, %a" (Cond.suffix cond) Reg.pp rt Reg.pp rt2 Dreg.pp dm
    | V3_d { cond; op; vd; vn; vm } ->
        Fmt.pf ppf "%s%s.f64 %a, %a, %a" (V3.name op) (Cond.suffix cond) Dreg.pp vd Dreg.pp vn
          Dreg.pp vm
    | V3_s { cond; op; vd; vn; vm } ->
        Fmt.pf ppf "%s%s.f32 %a, %a, %a" (V3.name op) (Cond.suffix cond) Sreg.pp vd Sreg.pp vn
          Sreg.pp vm
    | Vneg_d { cond; vd; vm } ->
        Fmt.pf ppf "vneg%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vneg_s { cond; vd; vm } ->
        Fmt.pf ppf "vneg%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vcmp_reg_d { cond; vd; vm } ->
        Fmt.pf ppf "vcmp%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vcmp_reg_s { cond; vd; vm } ->
        Fmt.pf ppf "vcmp%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vcmp_zero_d { cond; vd } -> Fmt.pf ppf "vcmp%s.f64 %a, #0" (Cond.suffix cond) Dreg.pp vd
    | Vcmp_zero_s { cond; vd } -> Fmt.pf ppf "vcmp%s.f32 %a, #0" (Cond.suffix cond) Sreg.pp vd
    | Vmrs { cond } -> Fmt.pf ppf "vmrs%s APSR_nzcv, FPSCR" (Cond.suffix cond)
    | Vcvt_f32_f64 { cond; sd; dm } ->
        Fmt.pf ppf "vcvt%s.f32.f64 %a, %a" (Cond.suffix cond) Sreg.pp sd Dreg.pp dm
    | Vcvt_f64_f32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.f32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_f32_s32 { cond; sd; sm } ->
        Fmt.pf ppf "vcvt%s.f32.s32 %a, %a" (Cond.suffix cond) Sreg.pp sd Sreg.pp sm
    | Vcvt_f64_s32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.s32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_f64_u32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.u32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_s32_f64 { cond; sd; dm } ->
        Fmt.pf ppf "vcvt%s.s32.f64 %a, %a" (Cond.suffix cond) Sreg.pp sd Dreg.pp dm
    | Vmem_d { cond; load; vd; rn; offset } ->
        Fmt.pf ppf "%s%s %a, [%a, #%Ld]"
          (if load then "vldr" else "vstr")
          (Cond.suffix cond) Dreg.pp vd Reg.pp rn offset
    | Vmem_s { cond; load; vd; rn; offset } ->
        Fmt.pf ppf "%s%s %a, [%a, #%Ld]"
          (if load then "vldr" else "vstr")
          (Cond.suffix cond) Sreg.pp vd Reg.pp rn offset
    | Vldr_lit_d { cond; vd; target } ->
        Fmt.pf ppf "vldr%s %a, %a" (Cond.suffix cond) Dreg.pp vd Asm_core.Lowered_ast.pp_branch
          target
    | Vldr_lit_s { cond; vd; target } ->
        Fmt.pf ppf "vldr%s %a, %a" (Cond.suffix cond) Sreg.pp vd Asm_core.Lowered_ast.pp_branch
          target
    | Vpush { cond; vd; count } ->
        let members = List.init count (fun i -> Dreg.of_num (vd.Dreg.num + i)) in
        Fmt.pf ppf "vpush%s.64 {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Dreg.pp) members

  let equal a b =
    match (a, b) with
    | Dp_imm x, Dp_imm y ->
        Cond.equal x.cond y.cond && x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd
        && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
    | Dp_reg x, Dp_reg y ->
        Cond.equal x.cond y.cond && x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd
        && Reg.equal x.rn y.rn && Reg.equal x.rm y.rm && x.sh_kind = y.sh_kind
        && x.sh_amt = y.sh_amt
    | Ldst_imm x, Ldst_imm y ->
        Cond.equal x.cond y.cond && x.load = y.load && x.byte = y.byte && Reg.equal x.rt y.rt
        && Reg.equal x.rn y.rn && Int64.equal x.offset y.offset && x.writeback = y.writeback
        && x.post = y.post
    | Ldst_x x, Ldst_x y -> (
        Cond.equal x.cond y.cond && x.kind = y.kind && Reg.equal x.rt y.rt && Reg.equal x.rn y.rn
        && x.post = y.post
        &&
        match (x.offset, y.offset) with
        | X_imm a, X_imm b -> Int64.equal a b
        | X_reg a, X_reg b -> Reg.equal a.rm b.rm && a.negate = b.negate
        | _ -> false)
    | Rd_rm x, Rd_rm y ->
        Cond.equal x.cond y.cond && x.op = y.op && Reg.equal x.rd y.rd && Reg.equal x.rm y.rm
    | Sbfx x, Sbfx y ->
        Cond.equal x.cond y.cond && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && x.lsb = y.lsb
        && x.width = y.width
    | Ldst_reg x, Ldst_reg y ->
        Cond.equal x.cond y.cond && x.load = y.load && x.byte = y.byte && Reg.equal x.rt y.rt
        && Reg.equal x.rn y.rn && Reg.equal x.rm y.rm && x.negate = y.negate
        && x.sh_kind = y.sh_kind && x.sh_amt = y.sh_amt
    | Bx x, Bx y -> Cond.equal x.cond y.cond && Reg.equal x.rm y.rm
    | Push x, Push y -> Cond.equal x.cond y.cond && x.regs = y.regs
    | Ldm x, Ldm y ->
        Cond.equal x.cond y.cond && x.load = y.load && Reg.equal x.rn y.rn && x.regs = y.regs
    | Udf x, Udf y -> Int64.equal x.imm16 y.imm16
    | Bl x, Bl y -> Cond.equal x.cond y.cond && Asm_core.Lowered_ast.equal_branch x.target y.target
    | B x, B y -> Cond.equal x.cond y.cond && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Mul x, Mul y ->
        Cond.equal x.cond y.cond && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm
    | Mla x, Mla y ->
        Cond.equal x.cond y.cond && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm && Reg.equal x.ra y.ra
    | Movw_movt x, Movw_movt y ->
        Cond.equal x.cond y.cond && x.top = y.top && Reg.equal x.rd y.rd
        && Asm_core.Lowered_ast.equal_branch x.imm y.imm
    | Vmov_reg_d x, Vmov_reg_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vmov_reg_s x, Vmov_reg_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vmov_imm_d x, Vmov_imm_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && x.imm8 = y.imm8
    | Vmov_imm_s x, Vmov_imm_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && x.imm8 = y.imm8
    | Vmov_to_single x, Vmov_to_single y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sn y.sn && Reg.equal x.rt y.rt
    | Vmov_from_single x, Vmov_from_single y ->
        Cond.equal x.cond y.cond && Reg.equal x.rt y.rt && Sreg.equal x.sn y.sn
    | Vmov_to_double x, Vmov_to_double y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dm y.dm && Reg.equal x.rt y.rt
        && Reg.equal x.rt2 y.rt2
    | Vmov_from_double x, Vmov_from_double y ->
        Cond.equal x.cond y.cond && Reg.equal x.rt y.rt && Reg.equal x.rt2 y.rt2
        && Dreg.equal x.dm y.dm
    | V3_d x, V3_d y ->
        Cond.equal x.cond y.cond && x.op = y.op && Dreg.equal x.vd y.vd && Dreg.equal x.vn y.vn
        && Dreg.equal x.vm y.vm
    | V3_s x, V3_s y ->
        Cond.equal x.cond y.cond && x.op = y.op && Sreg.equal x.vd y.vd && Sreg.equal x.vn y.vn
        && Sreg.equal x.vm y.vm
    | Vneg_d x, Vneg_d y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vneg_s x, Vneg_s y -> Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vcmp_reg_d x, Vcmp_reg_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vcmp_reg_s x, Vcmp_reg_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vcmp_zero_d x, Vcmp_zero_d y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd
    | Vcmp_zero_s x, Vcmp_zero_s y -> Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd
    | Vmrs x, Vmrs y -> Cond.equal x.cond y.cond
    | Vcvt_f32_f64 x, Vcvt_f32_f64 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Dreg.equal x.dm y.dm
    | Vcvt_f64_f32 x, Vcvt_f64_f32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_f32_s32 x, Vcvt_f32_s32 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Sreg.equal x.sm y.sm
    | Vcvt_f64_s32 x, Vcvt_f64_s32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_f64_u32 x, Vcvt_f64_u32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_s32_f64 x, Vcvt_s32_f64 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Dreg.equal x.dm y.dm
    | Vmem_d x, Vmem_d y ->
        Cond.equal x.cond y.cond && x.load = y.load && Dreg.equal x.vd y.vd && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Vmem_s x, Vmem_s y ->
        Cond.equal x.cond y.cond && x.load = y.load && Sreg.equal x.vd y.vd && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Vldr_lit_d x, Vldr_lit_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd
        && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Vldr_lit_s x, Vldr_lit_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd
        && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Vpush x, Vpush y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && x.count = y.count
    | Nop x, Nop y -> Cond.equal x.cond y.cond
    | Shift_reg x, Shift_reg y ->
        Cond.equal x.cond y.cond && x.kind = y.kind && Reg.equal x.rd y.rd && Reg.equal x.rm y.rm
        && Reg.equal x.rs y.rs
    | _ -> false
end

(* Measured on the M2 fixtures: a same-file call is R_ARM_CALL, a global address
   arrives as the R_ARM_MOVW_ABS_NC / R_ARM_MOVT_ABS pair - one relocation per
   instruction, each patching a field split across two non-adjacent runs - and a
   branch to a local label keeps no record at all. A32 is fixed-width, so there
   is no ladder and no short rung. *)
type fixup_kind = Abs32 | Pcrel_b26 | Pcrel_call | Movw_abs_nc | Movt_abs | Pcrel_vldr8

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Pcrel_b26 -> "pcrel-b26"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "movw-abs-nc"
  | Movt_abs -> "movt-abs"
  | Pcrel_vldr8 -> "pcrel-vldr8"

let equal_fixup_kind a b = a = b

let fixup_family = function
  | Abs32 -> "abs"
  | Pcrel_b26 -> "pcrel-branch"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "abs-lo16"
  | Movt_abs -> "abs-hi16"
  | Pcrel_vldr8 -> "pcrel-vldr-literal"

let fixup_role = function
  | Abs32 | Movw_abs_nc | Movt_abs | Pcrel_vldr8 -> Asm_core.Lowered_ast.Data_address
  | Pcrel_b26 -> Asm_core.Lowered_ast.Branch
  | Pcrel_call -> Asm_core.Lowered_ast.Call

(* The [.syntax] modes; the instructions this target takes read the same in both. *)
module Syntax = struct
  type t = Unified | Divided

  let of_name = function "unified" -> Some Unified | "divided" -> Some Divided | _ -> None
  let name = function Unified -> "unified" | Divided -> "divided"
end

type target_state = { syntax : Syntax.t; thumb : bool }

let default_state = { syntax = Syntax.Divided; thumb = false }

(* No separable components: this target's whole instruction set is the always-available base, so
   there is nothing to enable or disable and every configuration is the empty one; an opcode's
   requirement is what its table entry names, which is none. *)
let components : Target_component.t list = []
let default_config = Target_config.default components
let initial_state (_ : Target_config.t) = default_state
let state_config (_ : target_state) = default_config

let required (i : Instruction.t) : Target_component.Req.t option =
  Arm_ops.requirement i.Instruction.op

(* {1 The modified-immediate relation (§ M1.3)}

   A32's immediate field is 4 bits of rotation and 8 bits of value, denoting
   [ror32(imm8, 2 * rot)]. This is the project's motivating [Iso_fun]: the
   relation is neither injective (0x2a has two representations) nor total
   (0x12345678 has none), so it is not an [Iso_table] and no table of 2^32
   entries would make it one. What proves it correct is the exhaustive
   round-trip over all 4096 (rot, imm8) pairs in the test suite.

   Canonical selection is *the smallest rotation*, which is what GNU as picks
   and what the three committed cases pin: [#42] -> rot 0, [#0x3f000000] ->
   rot 4, [#0x100] -> rot 12. Choosing "the first representation found" without
   fixing the scan direction would be a coin flip that happened to agree on the
   one value in the fixture. *)

let rotr32 v n =
  let v = Int64.logand v 0xFFFFFFFFL in
  let n = n land 31 in
  if n = 0 then v
  else
    Int64.logand
      (Int64.logor (Int64.shift_right_logical v n) (Int64.shift_left v (32 - n)))
      0xFFFFFFFFL

let rotl32 v n = rotr32 v (32 - (n land 31))

let encode_modimm v =
  let v = Int64.logand v 0xFFFFFFFFL in
  let rec go rot =
    if rot > 15 then None
    else
      let cand = rotl32 v (2 * rot) in
      if Int64.compare cand 256L < 0 then Some (rot, Int64.to_int cand) else go (rot + 1)
  in
  go 0

let decode_modimm ~rot ~imm8 = rotr32 (Int64.of_int imm8) (2 * rot)

(* Every (rot, imm8) pair. 4096 is small enough to enumerate outright, which is
   the whole justification the domain has to record. *)
let modimm_domain =
  C.Finite.exhaustive ~name:"arm.modimm"
    ~why:
      "the field is 4 bits of rotation and 8 bits of value, so the domain is exactly 4096 pairs \
       and enumerating it is a table scan rather than a sample"
    (List.concat_map
       (fun rot -> List.init 256 (fun imm8 -> decode_modimm ~rot ~imm8))
       (List.init 16 (fun r -> r)))

let modimm_codec : (int64, fixup_kind) C.t =
  C.iso_fun ~name:"modimm"
    ~encode:(fun v ->
      match encode_modimm v with
      | None -> None
      | Some (rot, imm8) -> Some (Int64.of_int rot, Int64.of_int imm8))
    ~decode:(fun (rot, imm8) ->
      Some (decode_modimm ~rot:(Int64.to_int rot) ~imm8:(Int64.to_int imm8)))
    C.(field ~width:4 "rot" ** field ~width:8 "imm8")

(* {1 The codec}

   Every M1 instruction is unconditional. The condition field is therefore a
   constant rather than a field: making it a field would mean the codec accepted
   conditional forms that lowering cannot produce and that no test covers, and
   [Finite.unreachable_alts] would not catch it because the condition is not an
   alternative. Conditional execution arrives with M2, as a field and a
   suffix-parsing rule together. *)

(* The condition is a real field now, on every form. M1 encoded it as the
   constant [al] and said in this comment that conditional execution would
   arrive with M2; it has.

   A declared finite relation rather than a bare four-bit field, so [check]
   decides that all sixteen codes decode and that no two share one. *)
let cond_codec =
  C.iso_table ~name:"cond" ~equal:Cond.equal ~show:Cond.name
    ~entries:(List.map (fun c -> (c, Int64.of_int (Cond.code c))) Cond.all)
    (C.field ~width:4 "cond")

let reg_field name =
  C.iso_fun ~name
    ~encode:(fun (r : Reg.t) -> Some (Int64.of_int r.num))
    ~decode:(fun v -> Some (Reg.of_num (Int64.to_int v)))
    (C.field ~width:4 name)

(* {1 VFP register field splitting}

   Every VFP encoding splits a D/S register's number across a 1-bit
   "extension" field (D/N/M in the ARM ARM's own names, depending which
   operand slot it belongs to) and an adjacent 4-bit field - never the same
   two word positions twice, so the split itself, not its placement, is what
   belongs here. [Dreg.hilo]/[Sreg.hilo] already compute the two components;
   what differs between the register classes is *which* component goes in
   the 1-bit slot and which in the 4-bit slot - {!Dreg.hilo}'s own doc
   comment spells out the inversion. These wrappers normalize both classes
   to one shape, [(ext, field4)], so every VFP [Alt] below can place the two
   without having to remember which class inverts. *)
let dreg_parts (r : Dreg.t) : int64 * int64 = Dreg.hilo r
let dreg_unparts ext field4 = Dreg.of_hilo ext field4

let sreg_parts (r : Sreg.t) : int64 * int64 =
  let field4, ext = Sreg.hilo r in
  (ext, field4)

let sreg_unparts ext field4 = Sreg.of_hilo field4 ext

(* [movw]/[movt]. The 16-bit immediate is split across two non-adjacent fields -
   [imm4] at bits 19:16 and [imm12] at bits 11:0 - which is exactly the case
   §B2 of the contract exists for: two [fixup] nodes carrying the same name and
   kind, each tagged with the bit of the *logical* value it starts at, merged by
   [encode] into one placement with two slices. The split lives here, at the
   [iso_fun] boundary, so the sequential bit walk never has to interleave one
   node across two positions.

   [top] selects which half of the address the field holds, and with it the
   relocation kind, so it is a parameter of the form rather than a field in it:
   [Movw_abs_nc] and [Movt_abs] are different relocations, not one relocation
   with a bit. *)
let movw_alt ~top =
  let kind = if top then Movt_abs else Movw_abs_nc in
  let name = if top then "movt" else "movw" in
  let op = if top then 0b00110100L else 0b00110000L in
  C.iso_fun ~name
    ~encode:(function
      | Lowered.Movw_movt { cond; top = t; rd; imm } when t = top ->
          let v =
            match imm with
            | Asm_core.Lowered_ast.Symbolic _ -> 0L
            | Asm_core.Lowered_ast.Resolved { value; _ } -> value
          in
          Some
            ( cond,
              ((), (Int64.logand (Int64.shift_right_logical v 12) 0xFL, (rd, Int64.logand v 0xFFFL)))
            )
      | _ -> None)
    ~decode:(fun (cond, ((), (imm4, (rd, imm12)))) ->
      let value = Int64.logor (Int64.shift_left imm4 12) imm12 in
      Some
        (Lowered.Movw_movt
           { cond; top; rd; imm = Asm_core.Lowered_ast.Resolved { value; rung = name } }))
    C.(
      cond_codec ** const ~width:8 op
      ** fixup ~width:4 ~value_lsb:12 ~kind "imm"
      ** reg_field "rd"
      ** fixup ~width:12 ~value_lsb:0 ~kind "imm")

let rd_rm_alt rd_op =
  let hi, lo = Option.get (Arm_ops.rd_rm rd_op) in
  C.iso_fun ~name:(Opcode.name rd_op)
    ~encode:(function
      | Lowered.Rd_rm { cond; op; rd; rm } when op = rd_op ->
          Some (cond, ((), ((), (rd, ((), ((), rm))))))
      | _ -> None)
    ~decode:(fun (cond, ((), ((), (rd, ((), ((), rm)))))) ->
      Some (Lowered.Rd_rm { cond; op = rd_op; rd; rm }))
    C.(
      cond_codec ** const ~width:8 hi ** const ~width:4 0xFL ** reg_field "rd"
      ** const ~width:4 0xFL ** const ~width:4 lo ** reg_field "rm")

let codec : (Lowered.t, fixup_kind) C.t =
  C.choice ~name:"arm"
    [
      (* [bx] first. Its pattern [0001 0010 1111 1111 1111 0001 Rm] reads like a
         specialization of the data-processing register format, and putting it
         first is what keeps a [bx] from decoding as a plausible [teq]. It is
         only a near miss, though: [dp-reg] fixes bit 4 to zero and [bx] sets
         it, so [check] reports no overlap between the two. *)
      C.alt ~label:"bx" ~priority:0
        (C.iso_fun ~name:"bx"
           ~encode:(function Lowered.Bx { cond; rm } -> Some (cond, ((), rm)) | _ -> None)
           ~decode:(fun (cond, ((), rm)) -> Some (Lowered.Bx { cond; rm }))
           C.(cond_codec ** const ~width:24 0x12FFF1L ** reg_field "rm"));
      (* [push {reglist}], GNU's [stmdb sp!, {reglist}] alias (M5,
         the corpus notes). [100 1 0 0 1 0] is STM's P=1/U=0/S=0/W=1/L=0
         (pre-indexed, decrement, no S-bit, writeback, store) - verified
         against real [arm-linux-gnueabihf-as]/[objdump]:
         [e92d000f] for [push {r0, r1, r2, r3}]. *)
      C.alt ~label:"push" ~priority:37
        (C.iso_fun ~name:"push"
           ~encode:(function
             | Lowered.Push { cond; regs } -> Some (cond, ((), Int64.of_int regs)) | _ -> None)
           ~decode:(fun (cond, ((), regs)) ->
             Some (Lowered.Push { cond; regs = Int64.to_int regs }))
           C.(cond_codec ** const ~width:12 0b100100101101L ** field ~width:16 "reglist"));
      (* [stmia]/[ldmia Rn!, {reglist}], and [pop]'s own alias for the load
         direction with [Rn = sp] (M5). Increment-after
         STM/LDM: [100 0 1 0 1] is P=0/U=1/S=0/W=1 (post-indexed, increment,
         no S-bit, writeback forced - the only evidenced shape), leaving L
         (store/load) and Rn variable, unlike [push] above which bakes both
         P/U *and* Rn=sp into its fixed field. Verified against real
         arm-linux-gnueabihf-as/objdump: [e8a30003] for [stmia r3!, {r0,
         r1}], [e8b30003] for [ldmia r3!, {r0, r1}], and [e8bd0003] for
         [pop {r0, r1}] (identical to [ldmia sp!, {r0, r1}] - {!Opcode.Pop}
         is a pure printing alias, not a separate bit pattern). *)
      C.alt ~label:"ldm" ~priority:38
        (C.iso_fun ~name:"ldm"
           ~encode:(function
             | Lowered.Ldm { cond; load; rn; regs } ->
                 Some (cond, ((), ((if load then 1L else 0L), (rn, Int64.of_int regs))))
             | _ -> None)
           ~decode:(fun (cond, ((), (l, (rn, regs)))) ->
             Some (Lowered.Ldm { cond; load = Int64.equal l 1L; rn; regs = Int64.to_int regs }))
           C.(
             cond_codec ** const ~width:7 0b1000101L ** field ~width:1 "l" ** reg_field "rn"
             ** field ~width:16 "reglist"));
      (* [vpush.64 {dN, ...}], GNU's [vstmdb sp!, {dN, ...}] alias (M5) -
         the D-register cousin of [push] above, but VSTM-class rather than STMDB-class: no
         bitmask, only a base register and a count. [1101 0] is P=1/U=0 (pre-indexed,
         decrement), [D] is the base D-register's extension bit, [10] is W=1/L=0 (writeback,
         store), [1101] fixes Rn=sp, and [1011] is VSTM's own coprocessor tag ([101]) with
         the doubleword size bit ([1]) set - a single D register needs no separate
         one-register alias the way GPR [push] does, since this encoding already covers it
         (imm8 = 2). Verified against real arm-linux-gnueabihf-as/objdump (-march=armv7-a
         -mfpu=vfpv3): [ed2d8b04] for [vpush.64 {d8, d9}] (2 registers), [ed2d8b06] for
         [vpush.64 {d8-d10}] (3), [ed2d8b02] for [vpush.64 {d8}] (1), and [ed6d0b04] for
         [vpush.64 {d16-d17}] (base >= D16, exercising the D:Vd split's high half). *)
      C.alt ~label:"vpush" ~priority:39
        (C.iso_fun ~name:"vpush"
           ~encode:(function
             | Lowered.Vpush { cond; vd; count } ->
                 let d, vdf = dreg_parts vd in
                 let imm8 = Int64.of_int (count * 2) in
                 if Int64.compare imm8 256L >= 0 then None
                 else Some (cond, ((), (d, ((), ((), (vdf, ((), imm8)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), imm8))))))) ->
             Some (Lowered.Vpush { cond; vd = dreg_unparts d vdf; count = Int64.to_int imm8 / 2 }))
           C.(
             cond_codec ** const ~width:5 0b11010L ** field ~width:1 "D" ** const ~width:2 0b10L
             ** const ~width:4 0b1101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** field ~width:8 "imm8"));
      (* [movw]/[movt] must be tried *before* [dp-imm], not after. Their
         encodings sit inside the data-processing immediate space - [movw] is
         [dp = 8, S = 0] and [movt] is [dp = 10, S = 0], the [tst] and [cmp]
         slots with the flag-setting bit clear - so a [movw] word decodes
         perfectly well as a nonsensical [tst] if [dp-imm] is asked first.
         Lowering can never *produce* those two combinations (a comparison
         always sets S, and there is no [tst]), so the overlap costs nothing on
         the encode side; it is only decode that has to be told which reading is
         the real one. [check] reports both pairs, naming the priorities that
         decide them - the report is the record that this was chosen rather than
         missed. *)
      (* [nop] - the dedicated ARMv7 hint-NOP encoding, [e320f000] for [cond = Al] (the same
         bytes {!nop_bytes} already pads with). Its [dp] reading is 9 (the [teq] slot, S clear -
         unmapped, since {!Opcode.of_dp} names no [Teq]), but [dp-imm]'s own fields are all
         variable, not fixed, so it would still decode - wrongly, as a nonsensical [dp9] word -
         unless tried first, the identical overlap [movw]/[movt] already document just below. *)
      C.alt ~label:"nop" ~priority:(-1)
        (C.iso_fun ~name:"nop"
           ~encode:(function Lowered.Nop { cond } -> Some (cond, ()) | _ -> None)
           ~decode:(fun (cond, ()) -> Some (Lowered.Nop { cond }))
           C.(cond_codec ** const ~width:28 0x320F000L));
      C.alt ~label:"movw" ~priority:1 (movw_alt ~top:false);
      C.alt ~label:"movt" ~priority:2 (movw_alt ~top:true);
      C.alt ~label:"dp-imm" ~priority:3
        (C.iso_fun ~name:"dp-imm"
           ~encode:(function
             | Lowered.Dp_imm { cond; dp; s; rd; rn; imm } ->
                 Some (cond, ((), (Int64.of_int dp, ((if s then 1L else 0L), (rn, (rd, imm))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (dp, (s, (rn, (rd, imm)))))) ->
             Some (Lowered.Dp_imm { cond; dp = Int64.to_int dp; s = Int64.equal s 1L; rd; rn; imm }))
           C.(
             cond_codec ** const ~width:3 1L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** modimm_codec));
      C.alt ~label:"dp-reg" ~priority:4
        (C.iso_fun ~name:"dp-reg"
           ~encode:(function
             | Lowered.Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } ->
                 Some
                   ( cond,
                     ( (),
                       ( Int64.of_int dp,
                         ( (if s then 1L else 0L),
                           (rn, (rd, (Int64.of_int sh_amt, (Int64.of_int sh_kind, ((), rm))))) ) )
                     ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (dp, (s, (rn, (rd, (sh_amt, (sh_kind, ((), rm))))))))) ->
             Some
               (Lowered.Dp_reg
                  {
                    cond;
                    dp = Int64.to_int dp;
                    s = Int64.equal s 1L;
                    rd;
                    rn;
                    rm;
                    sh_kind = Int64.to_int sh_kind;
                    sh_amt = Int64.to_int sh_amt;
                  }))
           C.(
             cond_codec ** const ~width:3 0L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** field ~width:5 "shift-amount"
             ** field ~width:2 "shift-kind" ** const ~width:1 0L ** reg_field "rm"));
      (* Load and store, immediate offset. [U] is the sign of the offset and the
         12-bit field is its magnitude - A32 has no negative displacement field,
         so a signed field here would encode -4 as 0xFFC and address the wrong
         word. *)
      C.alt ~label:"ldst-imm" ~priority:5
        (C.iso_fun ~name:"ldst-imm"
           ~encode:(function
             | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset; writeback; post } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if Int64.compare mag 4096L >= 0 || (post && writeback) then None
                 else
                   Some
                     ( cond,
                       ( (),
                         ( (if post then 0L else 1L),
                           ( (if up then 1L else 0L),
                             ( (if byte then 1L else 0L),
                               ( (if writeback then 1L else 0L),
                                 ((if load then 1L else 0L), (rn, (rt, mag))) ) ) ) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (p, (up, (byte, (w, (load, (rn, (rt, mag))))))))) ->
             let post = Int64.equal p 0L in
             (* P = 0 with W = 1 is LDRT/STRT, the unprivileged access, not a writeback *)
             if post && Int64.equal w 1L then None
             else
               Some
                 (Lowered.Ldst_imm
                    {
                      cond;
                      load = Int64.equal load 1L;
                      byte = Int64.equal byte 1L;
                      rt;
                      rn;
                      offset = (if Int64.equal up 1L then mag else Int64.neg mag);
                      writeback = Int64.equal w 1L;
                      post;
                    }))
           C.(
             cond_codec ** const ~width:3 2L
             (* P: 1 for offset and pre-indexed addressing ([\[rn, #imm\]], [\[rn, #imm\]!]),
                0 for post-indexed ([\[rn\], #imm]) *)
             ** field ~width:1 "p"
             ** field ~width:1 "u" ** field ~width:1 "b" ** field ~width:1 "w" ** field ~width:1 "l"
             ** reg_field "rn" ** reg_field "rt" ** field ~width:12 "imm12"));
      (* Load and store, register offset: [ldr r2, [r1, r2, lsl #2]] / [ldrb
         r2, [r7, r0]]. Same word shape as ldst-imm one level up, but bit 25
         is set (the [I] bit ARM's own manual uses for this distinction) and
         the 12-bit magnitude is replaced by imm5 + shift-kind + a fixed 0 +
         Rm. [U] carries the index register's sign here exactly as it carries
         the immediate's sign above - there is no separate "negative register"
         field. Verified byte-for-byte against arm-linux-gnueabihf-as/objdump:
         [ldr r2, [r1, r2, lsl #2]] is [e7912102], [ldr r2, [r1, -r2]] is
         [e7112002], [ldr r2, [r1, r2, ror #7]] is [e79123e2]. *)
      (* The extra loads and stores, 8-bit immediate: [cond 000 P U 1 W L Rn Rt imm4H 1 S H 1
         imm4L], W = 0 (offset or post-indexed). Verified against real arm-linux-gnueabihf-as
         2.44: [ldrh r0, [r1, #6]] -> [e1d100b6], [strh r2, [r3, #-2]] -> [e14320b2],
         [ldrsh r4, [r5, #254]] -> [e1d54ffe], [ldrsb r6, [r7]] -> [e1d760d0],
         [ldrh ip, [r2], #2] -> [e0d2c0b2], [strh ip, [r3], #2] -> [e0c3c0b2]. *)
      C.alt ~label:"ldst-x-imm" ~priority:44
        (C.iso_fun ~name:"ldst-x-imm"
           ~encode:(function
             | Lowered.Ldst_x { cond; kind; rt; rn; offset = X_imm v; post } ->
                 let l, sb, h = x_bits kind in
                 let mag = Int64.abs v in
                 if Int64.compare mag 256L >= 0 then None
                 else
                   Some
                     ( cond,
                       ( (),
                         ( (if post then 0L else 1L),
                           ( (if Int64.compare v 0L >= 0 then 1L else 0L),
                             ( (),
                               ( (),
                                 ( l,
                                   ( rn,
                                     ( rt,
                                       ( Int64.shift_right mag 4,
                                         ((), (sb, (h, ((), Int64.logand mag 15L)))) ) ) ) ) ) ) )
                         ) ) )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (p, (u, ((), ((), (l, (rn, (rt, (hi, ((), (sb, (h, ((), lo))))))))))))))
             ->
             match x_kind_of_bits (l, sb, h) with
             | None -> None
             | Some kind ->
                 let mag = Int64.logor (Int64.shift_left hi 4) lo in
                 Some
                   (Lowered.Ldst_x
                      {
                        cond;
                        kind;
                        rt;
                        rn;
                        offset = X_imm (if Int64.equal u 1L then mag else Int64.neg mag);
                        post = Int64.equal p 0L;
                      }))
           C.(
             cond_codec ** const ~width:3 0L ** field ~width:1 "p" ** field ~width:1 "u"
             ** const ~width:1 1L (* I *) ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt" ** field ~width:4 "imm4h"
             ** const ~width:1 1L ** field ~width:1 "s" ** field ~width:1 "h" ** const ~width:1 1L
             ** field ~width:4 "imm4l"));
      (* The same, register offset: [imm4H] is 0000 and [imm4L] is [Rm]. Verified against real
         arm-linux-gnueabihf-as 2.44: [ldrsb r0, [r1, r2]] -> [e19100d2], [ldrh r3, [r4, -r5]]
         -> [e11430b5], [strh r6, [r7, r8]] -> [e18760b8]. *)
      C.alt ~label:"ldst-x-reg" ~priority:45
        (C.iso_fun ~name:"ldst-x-reg"
           ~encode:(function
             | Lowered.Ldst_x { cond; kind; rt; rn; offset = X_reg { rm; negate }; post } ->
                 let l, sb, h = x_bits kind in
                 Some
                   ( cond,
                     ( (),
                       ( (if post then 0L else 1L),
                         ( (if negate then 0L else 1L),
                           ((), ((), (l, (rn, (rt, ((), ((), (sb, (h, ((), rm)))))))))) ) ) ) )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (p, (u, ((), ((), (l, (rn, (rt, ((), ((), (sb, (h, ((), rm))))))))))))))
             ->
             match x_kind_of_bits (l, sb, h) with
             | None -> None
             | Some kind ->
                 Some
                   (Lowered.Ldst_x
                      {
                        cond;
                        kind;
                        rt;
                        rn;
                        offset = X_reg { rm; negate = Int64.equal u 0L };
                        post = Int64.equal p 0L;
                      }))
           C.(
             cond_codec ** const ~width:3 0L ** field ~width:1 "p" ** field ~width:1 "u"
             ** const ~width:1 0L (* I *) ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt" ** const ~width:4 0L
             ** const ~width:1 1L ** field ~width:1 "s" ** field ~width:1 "h" ** const ~width:1 1L
             ** reg_field "rm"));
      (* [sbfx rd, rn, #lsb, #width]: [cond 0111101 widthm1 Rd lsb 101 Rn]. Verified against
         real arm-linux-gnueabihf-as 2.44: [sbfx r0, r1, #0, #16] -> [e7af0051],
         [sbfx r2, r3, #3, #5] -> [e7a421d3]. *)
      C.alt ~label:"sbfx" ~priority:46
        (C.iso_fun ~name:"sbfx"
           ~encode:(function
             | Lowered.Sbfx { cond; rd; rn; lsb; width } ->
                 Some (cond, ((), (Int64.of_int (width - 1), (rd, (Int64.of_int lsb, ((), rn))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (w, (rd, (lsb, ((), rn)))))) ->
             let lsb = Int64.to_int lsb and width = Int64.to_int w + 1 in
             if lsb + width > 32 then None else Some (Lowered.Sbfx { cond; rd; rn; lsb; width }))
           C.(
             cond_codec ** const ~width:7 0b0111101L ** field ~width:5 "widthm1" ** reg_field "rd"
             ** field ~width:5 "lsb" ** const ~width:3 0b101L ** reg_field "rn"));
      (* Verified against real arm-linux-gnueabihf-as 2.44: [clz r0, r1] -> [e16f0f11],
         [rev r2, r3] -> [e6bf2f33], [rev16 r4, r5] -> [e6bf4fb5], [clzne ip, lr] -> [116fcf1e]. *)
      C.alt ~label:"clz" ~priority:47 (rd_rm_alt Opcode.Clz);
      C.alt ~label:"rev" ~priority:48 (rd_rm_alt Opcode.Rev);
      C.alt ~label:"rev16" ~priority:49 (rd_rm_alt Opcode.Rev16);
      C.alt ~label:"ldst-reg" ~priority:6
        (C.iso_fun ~name:"ldst-reg"
           ~encode:(function
             | Lowered.Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } ->
                 Some
                   ( cond,
                     ( (),
                       ( (),
                         ( (if negate then 0L else 1L),
                           ( (if byte then 1L else 0L),
                             ( (),
                               ( (if load then 1L else 0L),
                                 (rn, (rt, (Int64.of_int sh_amt, (Int64.of_int sh_kind, ((), rm)))))
                               ) ) ) ) ) ) )
             | _ -> None)
           ~decode:(fun
               ( cond,
                 ((), ((), (u, (byte, ((), (load, (rn, (rt, (sh_amt, (sh_kind, ((), rm))))))))))) )
             ->
             Some
               (Lowered.Ldst_reg
                  {
                    cond;
                    load = Int64.equal load 1L;
                    byte = Int64.equal byte 1L;
                    rt;
                    rn;
                    rm;
                    negate = Int64.equal u 0L;
                    sh_kind = Int64.to_int sh_kind;
                    sh_amt = Int64.to_int sh_amt;
                  }))
           C.(
             cond_codec ** const ~width:3 3L
             ** const ~width:1 1L (* P: offset addressing, no writeback *)
             ** field ~width:1 "u" ** field ~width:1 "b" ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt"
             ** field ~width:5 "shift-amount" ** field ~width:2 "shift-kind" ** const ~width:1 0L
             ** reg_field "rm"));
      (* [udf], the permanently-undefined encoding. Its fixed top nibble is
         literally [cond_codec]'s bit pattern - not a coincidence to route around,
         but where ARM parked this encoding. *)
      C.alt ~label:"udf" ~priority:7
        (C.iso_fun ~name:"udf"
           ~encode:(function
             | Lowered.Udf { imm16 } ->
                 if Int64.compare imm16 0L < 0 || Int64.compare imm16 65536L >= 0 then None
                 else
                   Some
                     ((), ((), (Int64.shift_right_logical imm16 4, ((), Int64.logand imm16 0xFL))))
             | _ -> None)
           ~decode:(fun ((), ((), (imm12, ((), imm4)))) ->
             Some (Lowered.Udf { imm16 = Int64.logor (Int64.shift_left imm12 4) imm4 }))
           C.(
             (* Not a condition field: UDF is unconditional, and 0xE is simply
                where ARM parked this encoding. Reading it as [al] would let a
                decoder accept fifteen more words that are not [udf]. *)
             const ~width:4 0xEL ** const ~width:8 0b01111111L ** field ~width:12 "imm12"
             ** const ~width:4 0b1111L ** field ~width:4 "imm4"));
      (* [bl] - R_ARM_CALL. The 24-bit field is a word count measured from the
         instruction plus eight, the A32 pipeline offset; both the division and
         the bias live outside this node - the division in [evaluate_fixup] and
         the bias in the fixup, so the evaluator receives a PC rather than
         something it has to correct. *)
      C.alt ~label:"bl" ~priority:8
        (C.iso_fun ~name:"bl"
           ~encode:(function
             | Lowered.Bl { cond; target } -> (
                 match target with
                 | Asm_core.Lowered_ast.Symbolic _ -> Some (cond, ((), 0L))
                 | Asm_core.Lowered_ast.Resolved { value; _ } -> Some (cond, ((), value)))
             | _ -> None)
           ~decode:(fun (cond, ((), d)) ->
             let v = Int64.shift_right (Int64.shift_left d 40) 40 in
             Some
               (Lowered.Bl
                  { cond; target = Asm_core.Lowered_ast.Resolved { value = v; rung = "bl" } }))
           C.(cond_codec ** const ~width:4 0b1011L ** fixup ~width:24 ~kind:Pcrel_call "target"));
      (* [b] - the same word-counted 24-bit field as [bl], one opcode bit apart,
         and the reason A32 needs no relaxation ladder: 24 words of signed reach
         is +/-32 MiB from a single fixed-width form. *)
      C.alt ~label:"b" ~priority:9
        (C.iso_fun ~name:"b"
           ~encode:(function
             | Lowered.B { cond; target } -> (
                 match target with
                 | Asm_core.Lowered_ast.Symbolic _ -> Some (cond, ((), 0L))
                 | Asm_core.Lowered_ast.Resolved { value; _ } -> Some (cond, ((), value)))
             | _ -> None)
           ~decode:(fun (cond, ((), d)) ->
             let v = Int64.shift_right (Int64.shift_left d 40) 40 in
             Some
               (Lowered.B { cond; target = Asm_core.Lowered_ast.Resolved { value = v; rung = "b" } }))
           C.(cond_codec ** const ~width:4 0b1010L ** fixup ~width:24 ~kind:Pcrel_b26 "target"));
      (* [mul] and [mla]. Bits 7:4 are [1001], and the data-processing register
         format requires bit 4 to be zero, so these do not overlap [dp-reg] and
         their position in this list is free. The operand order is the trap:
         [mul Rd, Rn, Rm] puts Rd at 19:16 where [dp-reg] keeps Rn, and Rn at
         3:0 where [dp-reg] keeps Rm. *)
      C.alt ~label:"mul" ~priority:10
        (C.iso_fun ~name:"mul"
           ~encode:(function
             | Lowered.Mul { cond; s; rd; rn; rm } ->
                 Some (cond, ((), ((if s then 1L else 0L), (rd, ((), (rm, ((), rn)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (s, (rd, ((), (rm, ((), rn))))))) ->
             Some (Lowered.Mul { cond; s = Int64.equal s 1L; rd; rn; rm }))
           C.(
             cond_codec ** const ~width:7 0L ** field ~width:1 "s" ** reg_field "rd"
             ** const ~width:4 0L ** reg_field "rm" ** const ~width:4 0b1001L ** reg_field "rn"));
      C.alt ~label:"mla" ~priority:11
        (C.iso_fun ~name:"mla"
           ~encode:(function
             | Lowered.Mla { cond; s; rd; rn; rm; ra } ->
                 Some (cond, ((), ((if s then 1L else 0L), (rd, (ra, (rm, ((), rn)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (s, (rd, (ra, (rm, ((), rn))))))) ->
             Some (Lowered.Mla { cond; s = Int64.equal s 1L; rd; rn; rm; ra }))
           C.(
             cond_codec ** const ~width:7 1L ** field ~width:1 "s" ** reg_field "rd"
             ** reg_field "ra" ** reg_field "rm" ** const ~width:4 0b1001L ** reg_field "rn"));
      (* [umull RdLo, RdHi, Rn, Rm] - multiply long, a genuinely different word shape from [mul]/
         [mla] just above (two destination registers instead of one, and a wider fixed prefix:
         bits 27:23 = [00001] rather than [mul]/[mla]'s bits 27:21 = 0), not a variant of either.
         Verified against real arm-linux-gnueabihf-as/objdump: [umull r4, r6, r0, r2] ->
         [e0864290]; bit 22 (0 here) selects unsigned vs. signed ([smull]) and bit 21 (0 here)
         selects multiply vs. multiply-accumulate ([umlal]) - both fixed at their {!Umull}-only
         value, since only plain [umull] is evidenced (M5 - gas_frontier.t's
         runtime-i64_smulh.S/i64_umulh.S). [s] stays a real field, as [mul]/[mla]'s own [s] does,
         even though nothing here ever lowers a [true] value ([umulls] is not evidenced either). *)
      C.alt ~label:"umull" ~priority:43
        (C.iso_fun ~name:"umull"
           ~encode:(function
             | Lowered.Umull { cond; s; rdlo; rdhi; rn; rm } ->
                 Some (cond, ((), ((), ((if s then 1L else 0L), (rdhi, (rdlo, (rm, ((), rn))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (s, (rdhi, (rdlo, (rm, ((), rn)))))))) ->
             Some (Lowered.Umull { cond; s = Int64.equal s 1L; rdlo; rdhi; rn; rm }))
           C.(
             cond_codec ** const ~width:5 0b00001L ** const ~width:2 0b00L ** field ~width:1 "s"
             ** reg_field "rdhi" ** reg_field "rdlo" ** reg_field "rm" ** const ~width:4 0b1001L
             ** reg_field "rn"));
      (* {2 VFP}

         Every form below was measured byte-for-byte against the real,
         installed arm-linux-gnueabihf-as/objdump (vfpv3-d16): the register
         moves, the immediate move (including the negative case, since VFP's
         modified immediate has a sign bit like A32's integer one), the
         three-register arithmetic family, negate, compare, the system
         register transfer, every VCVT direction this corpus needs, and
         VLDR/VSTR. *)
      C.alt ~label:"vmov-reg-d" ~priority:12
        (C.iso_fun ~name:"vmov-reg-d"
           ~encode:(function
             | Lowered.Vmov_reg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vmov_reg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-reg-s" ~priority:13
        (C.iso_fun ~name:"vmov-reg-s"
           ~encode:(function
             | Lowered.Vmov_reg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vmov_reg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-imm-d" ~priority:14
        (C.iso_fun ~name:"vmov-imm-d"
           ~encode:(function
             | Lowered.Vmov_imm_d { cond; vd; imm8 } ->
                 let d, vdf = dreg_parts vd in
                 let imm4h = Int64.of_int (imm8 lsr 4) and imm4l = Int64.of_int (imm8 land 0xF) in
                 Some (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l)))))))) ->
             let imm8 = (Int64.to_int imm4h lsl 4) lor Int64.to_int imm4l in
             Some (Lowered.Vmov_imm_d { cond; vd = dreg_unparts d vdf; imm8 }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** field ~width:4 "imm4H" ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:4 0L ** field ~width:4 "imm4L"));
      C.alt ~label:"vmov-imm-s" ~priority:15
        (C.iso_fun ~name:"vmov-imm-s"
           ~encode:(function
             | Lowered.Vmov_imm_s { cond; vd; imm8 } ->
                 let d, vdf = sreg_parts vd in
                 let imm4h = Int64.of_int (imm8 lsr 4) and imm4l = Int64.of_int (imm8 land 0xF) in
                 Some (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l)))))))) ->
             let imm8 = (Int64.to_int imm4h lsl 4) lor Int64.to_int imm4l in
             Some (Lowered.Vmov_imm_s { cond; vd = sreg_unparts d vdf; imm8 }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** field ~width:4 "imm4H" ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:4 0L ** field ~width:4 "imm4L"));
      C.alt ~label:"vmov-to-single" ~priority:16
        (C.iso_fun ~name:"vmov-to-single"
           ~encode:(function
             | Lowered.Vmov_to_single { cond; sn; rt } ->
                 let n, vnf = sreg_parts sn in
                 Some (cond, ((), ((), (vnf, (rt, ((), (n, ())))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (vnf, (rt, ((), (n, ()))))))) ->
             Some (Lowered.Vmov_to_single { cond; sn = sreg_unparts n vnf; rt }))
           C.(
             cond_codec ** const ~width:7 0b1110000L ** const ~width:1 0L ** field ~width:4 "Vn"
             ** reg_field "rt" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** const ~width:7 0b0010000L));
      C.alt ~label:"vmov-from-single" ~priority:17
        (C.iso_fun ~name:"vmov-from-single"
           ~encode:(function
             | Lowered.Vmov_from_single { cond; rt; sn } ->
                 let n, vnf = sreg_parts sn in
                 Some (cond, ((), ((), (vnf, (rt, ((), (n, ())))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (vnf, (rt, ((), (n, ()))))))) ->
             Some (Lowered.Vmov_from_single { cond; rt; sn = sreg_unparts n vnf }))
           C.(
             cond_codec ** const ~width:7 0b1110000L ** const ~width:1 1L ** field ~width:4 "Vn"
             ** reg_field "rt" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** const ~width:7 0b0010000L));
      C.alt ~label:"vmov-to-double" ~priority:18
        (C.iso_fun ~name:"vmov-to-double"
           ~encode:(function
             | Lowered.Vmov_to_double { cond; dm; rt; rt2 } ->
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf))))))))) ->
             Some (Lowered.Vmov_to_double { cond; dm = dreg_unparts m vmf; rt; rt2 }))
           C.(
             cond_codec ** const ~width:7 0b1100010L ** const ~width:1 0L ** reg_field "rt2"
             ** reg_field "rt" ** const ~width:4 0b1011L ** const ~width:2 0b00L
             ** field ~width:1 "D" ** const ~width:1 1L ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-from-double" ~priority:19
        (C.iso_fun ~name:"vmov-from-double"
           ~encode:(function
             | Lowered.Vmov_from_double { cond; rt; rt2; dm } ->
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf))))))))) ->
             Some (Lowered.Vmov_from_double { cond; rt; rt2; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:7 0b1100010L ** const ~width:1 1L ** reg_field "rt2"
             ** reg_field "rt" ** const ~width:4 0b1011L ** const ~width:2 0b00L
             ** field ~width:1 "D" ** const ~width:1 1L ** field ~width:4 "Vm"));
      (* [v3-d]/[v3-s] declare [b23]/[b21]/[b20]/[b6] as plain fields rather
         than four separate consts per op, because {!V3.selector} is the one
         place that relation lives and a const per alternative would restate
         it. The cost is exactly the situation {!movw_alt} already documents:
         [check] sees a field where the two-register "extension" family
         (vneg/vcmp/vcvt/vmov, priorities 12-34) sees a literal, so it reports
         overlap wherever a wildcarded field could reach one of those literal
         patterns. It is harmless on decode: {!V3.of_selector} accepts only
         the four combinations lowering ever produces, declines everything
         else, and [Alt]'s decoder falls through to the next priority on a
         decline - which is exactly why [v3-d]/[v3-s] are given the *lower*
         priority number of every pair [check] names, so a real three-register
         word is claimed here first and everything else falls through to its
         own, correct alternative. *)
      C.alt ~label:"v3-d" ~priority:20
        (C.iso_fun ~name:"v3-d"
           ~encode:(function
             | Lowered.V3_d { cond; op; vd; vn; vm } ->
                 let b23, b21, b20, b6 = V3.selector op in
                 let d, vdf = dreg_parts vd in
                 let n, vnf = dreg_parts vn in
                 let m, vmf = dreg_parts vm in
                 Some
                   ( cond,
                     ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf))))))))))))
                   )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf)))))))))))))
             ->
             match V3.of_selector (b23, b21, b20, b6) with
             | None -> None
             | Some op ->
                 Some
                   (Lowered.V3_d
                      {
                        cond;
                        op;
                        vd = dreg_unparts d vdf;
                        vn = dreg_unparts n vnf;
                        vm = dreg_unparts m vmf;
                      }))
           C.(
             cond_codec ** const ~width:4 0b1110L ** field ~width:1 "b23" ** field ~width:1 "D"
             ** field ~width:1 "b21" ** field ~width:1 "b20" ** field ~width:4 "Vn"
             ** field ~width:4 "Vd" ** const ~width:4 0b1011L ** field ~width:1 "N"
             ** field ~width:1 "b6" ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"v3-s" ~priority:21
        (C.iso_fun ~name:"v3-s"
           ~encode:(function
             | Lowered.V3_s { cond; op; vd; vn; vm } ->
                 let b23, b21, b20, b6 = V3.selector op in
                 let d, vdf = sreg_parts vd in
                 let n, vnf = sreg_parts vn in
                 let m, vmf = sreg_parts vm in
                 Some
                   ( cond,
                     ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf))))))))))))
                   )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf)))))))))))))
             ->
             match V3.of_selector (b23, b21, b20, b6) with
             | None -> None
             | Some op ->
                 Some
                   (Lowered.V3_s
                      {
                        cond;
                        op;
                        vd = sreg_unparts d vdf;
                        vn = sreg_unparts n vnf;
                        vm = sreg_unparts m vmf;
                      }))
           C.(
             cond_codec ** const ~width:4 0b1110L ** field ~width:1 "b23" ** field ~width:1 "D"
             ** field ~width:1 "b21" ** field ~width:1 "b20" ** field ~width:4 "Vn"
             ** field ~width:4 "Vd" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** field ~width:1 "b6" ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vneg-d" ~priority:22
        (C.iso_fun ~name:"vneg-d"
           ~encode:(function
             | Lowered.Vneg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vneg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0001L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vneg-s" ~priority:23
        (C.iso_fun ~name:"vneg-s"
           ~encode:(function
             | Lowered.Vneg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vneg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0001L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-reg-d" ~priority:24
        (C.iso_fun ~name:"vcmp-reg-d"
           ~encode:(function
             | Lowered.Vcmp_reg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcmp_reg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0100L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-reg-s" ~priority:25
        (C.iso_fun ~name:"vcmp-reg-s"
           ~encode:(function
             | Lowered.Vcmp_reg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcmp_reg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0100L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-zero-d" ~priority:26
        (C.iso_fun ~name:"vcmp-zero-d"
           ~encode:(function
             | Lowered.Vcmp_zero_d { cond; vd } ->
                 let d, vdf = dreg_parts vd in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ()))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ())))))))) ->
             Some (Lowered.Vcmp_zero_d { cond; vd = dreg_unparts d vdf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** const ~width:6 0L));
      C.alt ~label:"vcmp-zero-s" ~priority:27
        (C.iso_fun ~name:"vcmp-zero-s"
           ~encode:(function
             | Lowered.Vcmp_zero_s { cond; vd } ->
                 let d, vdf = sreg_parts vd in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ()))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ())))))))) ->
             Some (Lowered.Vcmp_zero_s { cond; vd = sreg_unparts d vdf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0101L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** const ~width:6 0L));
      (* [vmrs APSR_nzcv, FPSCR] - the only form this corpus needs, so the
         entire word past [cond] is one constant. *)
      C.alt ~label:"vmrs" ~priority:28
        (C.iso_fun ~name:"vmrs"
           ~encode:(function Lowered.Vmrs { cond } -> Some (cond, ()) | _ -> None)
           ~decode:(fun (cond, ()) -> Some (Lowered.Vmrs { cond }))
           C.(cond_codec ** const ~width:28 0xEF1FA10L));
      C.alt ~label:"vcvt-f32-f64" ~priority:29
        (C.iso_fun ~name:"vcvt-f32-f64"
           ~encode:(function
             | Lowered.Vcvt_f32_f64 { cond; sd; dm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_f32_f64 { cond; sd = sreg_unparts d vdf; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0111L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-f32" ~priority:30
        (C.iso_fun ~name:"vcvt-f64-f32"
           ~encode:(function
             | Lowered.Vcvt_f64_f32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_f64_f32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0111L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f32-s32" ~priority:31
        (C.iso_fun ~name:"vcvt-f32-s32"
           ~encode:(function
             | Lowered.Vcvt_f32_s32 { cond; sd; sm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f32_s32 { cond; sd = sreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:1 1L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-s32" ~priority:32
        (C.iso_fun ~name:"vcvt-f64-s32"
           ~encode:(function
             | Lowered.Vcvt_f64_s32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f64_s32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:1 1L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-u32" ~priority:33
        (C.iso_fun ~name:"vcvt-f64-u32"
           ~encode:(function
             | Lowered.Vcvt_f64_u32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f64_u32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:1 0L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-s32-f64" ~priority:34
        (C.iso_fun ~name:"vcvt-s32-f64"
           ~encode:(function
             | Lowered.Vcvt_s32_f64 { cond; sd; dm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_s32_f64 { cond; sd = sreg_unparts d vdf; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmem-d" ~priority:35
        (C.iso_fun ~name:"vmem-d"
           ~encode:(function
             | Lowered.Vmem_d { cond; load; vd; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if not (Int64.equal (Int64.rem mag 4L) 0L) then None
                 else
                   let imm8 = Int64.div mag 4L in
                   if Int64.compare imm8 256L >= 0 then None
                   else
                     let d, vdf = dreg_parts vd in
                     Some
                       ( cond,
                         ( (),
                           ( (if up then 1L else 0L),
                             (d, ((), ((if load then 1L else 0L), (rn, (vdf, ((), imm8)))))) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (up, (d, ((), (load, (rn, (vdf, ((), imm8))))))))) ->
             let mag = Int64.mul imm8 4L in
             let offset = if Int64.equal up 1L then mag else Int64.neg mag in
             Some
               (Lowered.Vmem_d
                  { cond; load = Int64.equal load 1L; vd = dreg_unparts d vdf; rn; offset }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** field ~width:1 "U" ** field ~width:1 "D"
             ** const ~width:1 0L ** field ~width:1 "L" ** reg_field "rn" ** field ~width:4 "Vd"
             ** const ~width:4 0b1011L ** field ~width:8 "imm8"));
      C.alt ~label:"vmem-s" ~priority:36
        (C.iso_fun ~name:"vmem-s"
           ~encode:(function
             | Lowered.Vmem_s { cond; load; vd; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if not (Int64.equal (Int64.rem mag 4L) 0L) then None
                 else
                   let imm8 = Int64.div mag 4L in
                   if Int64.compare imm8 256L >= 0 then None
                   else
                     let d, vdf = sreg_parts vd in
                     Some
                       ( cond,
                         ( (),
                           ( (if up then 1L else 0L),
                             (d, ((), ((if load then 1L else 0L), (rn, (vdf, ((), imm8)))))) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (up, (d, ((), (load, (rn, (vdf, ((), imm8))))))))) ->
             let mag = Int64.mul imm8 4L in
             let offset = if Int64.equal up 1L then mag else Int64.neg mag in
             Some
               (Lowered.Vmem_s
                  { cond; load = Int64.equal load 1L; vd = sreg_unparts d vdf; rn; offset }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** field ~width:1 "U" ** field ~width:1 "D"
             ** const ~width:1 0L ** field ~width:1 "L" ** reg_field "rn" ** field ~width:4 "Vd"
             ** const ~width:4 0b1010L ** field ~width:8 "imm8"));
      (* [vldr Dd/Sd, label] - the literal-pool form of {!Vmem_d}/{!Vmem_s}'s own
         word, with [Rn] fixed to [pc] (0b1111) rather than a runtime register,
         [L] fixed to 1 (load-only, the corpus notes), and [U] fixed to 1
         (forward-only: every corpus occurrence is a trailing same-function
         literal pool, and {!evaluate_fixup} rejects a negative distance rather
         than silently mis-encoding one). [imm8] is a fixup, not a plain field -
         the only difference from {!Vmem_d}/{!Vmem_s}'s own [imm8] is *where its
         value comes from* (a resolved link-time distance, not an operand
         already known at lowering time). Necessarily overlaps [vmem-d]/[vmem-s]
         on fixed bits (their [Rn]/[U]/[L] are wildcards, so an all-1111/1/1
         instance of either already matches them) - given a lower priority so
         they win on decode, reproducing real objdump's own [pc, #imm]
         disassembly rather than a bare-label spelling nothing re-parses back
         to the identical word (Codec.check's own tolerated-overlap list, the
         same shape [nop]/[movw]/[movt] already appear in). *)
      C.alt ~label:"vldr-lit-d" ~priority:41
        (C.iso_fun ~name:"vldr-lit-d"
           ~encode:(function
             | Lowered.Vldr_lit_d { cond; vd; target } ->
                 let imm8 =
                   match target with
                   | Asm_core.Lowered_ast.Symbolic _ -> 0L
                   | Asm_core.Lowered_ast.Resolved { value; _ } -> value
                 in
                 let d, vdf = dreg_parts vd in
                 Some (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8))))))))) ->
             Some
               (Lowered.Vldr_lit_d
                  {
                    cond;
                    vd = dreg_unparts d vdf;
                    target = Asm_core.Lowered_ast.Resolved { value = imm8; rung = "vldr-lit-d" };
                  }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** const ~width:1 1L (* U *) ** field ~width:1 "D"
             ** const ~width:1 0L ** const ~width:1 1L (* L *) ** const ~width:4 0b1111L
             (* Rn = pc *) ** field ~width:4 "Vd"
             ** const ~width:4 0b1011L
             ** fixup ~width:8 ~kind:Pcrel_vldr8 "target"));
      C.alt ~label:"vldr-lit-s" ~priority:42
        (C.iso_fun ~name:"vldr-lit-s"
           ~encode:(function
             | Lowered.Vldr_lit_s { cond; vd; target } ->
                 let imm8 =
                   match target with
                   | Asm_core.Lowered_ast.Symbolic _ -> 0L
                   | Asm_core.Lowered_ast.Resolved { value; _ } -> value
                 in
                 let d, vdf = sreg_parts vd in
                 Some (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8))))))))) ->
             Some
               (Lowered.Vldr_lit_s
                  {
                    cond;
                    vd = sreg_unparts d vdf;
                    target = Asm_core.Lowered_ast.Resolved { value = imm8; rung = "vldr-lit-s" };
                  }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** const ~width:1 1L (* U *) ** field ~width:1 "D"
             ** const ~width:1 0L ** const ~width:1 1L (* L *) ** const ~width:4 0b1111L
             (* Rn = pc *) ** field ~width:4 "Vd"
             ** const ~width:4 0b1010L
             ** fixup ~width:8 ~kind:Pcrel_vldr8 "target"));
      (* [lsl]/[lsr]/[asr]/[ror rd, rm, rs] - {!Opcode.Shift}'s register-shift-amount form
         (gcc's own [nsieve.c]/[nsievebits.c]). A [mov]-family word (dp = 13,
         S = 0, Rn = 0 - the only evidenced shape) with Rs at bits 11:8 in place of {!Dp_reg}'s
         5-bit immediate shift amount, and bit 4 set rather than clear - the ARM manual's own
         register/immediate-shift discriminator, so this never overlaps {!Dp_reg}'s own fixed
         bit 4 = 0. Verified against real arm-linux-gnueabihf-as/objdump: [e1a00211] for
         [lsl r0, r1, r2], [e1a00231]/[e1a00251]/[e1a00271] for [lsr]/[asr]/[ror] with the same
         operands (only the 2-bit shift-kind field differs). *)
      C.alt ~label:"shift-reg" ~priority:40
        (C.iso_fun ~name:"shift-reg"
           ~encode:(function
             | Lowered.Shift_reg { cond; kind; rd; rm; rs } ->
                 Some (cond, ((), ((), ((), ((), (rd, (rs, ((), (Int64.of_int kind, ((), rm))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), ((), ((), (rd, (rs, ((), (kind, ((), rm)))))))))) ->
             Some (Lowered.Shift_reg { cond; kind = Int64.to_int kind; rd; rm; rs }))
           C.(
             cond_codec ** const ~width:3 0L ** const ~width:4 13L ** const ~width:1 0L
             ** const ~width:4 0L ** reg_field "rd" ** reg_field "rs" ** const ~width:1 0L
             ** field ~width:2 "shift-kind" ** const ~width:1 1L ** reg_field "rm"));
    ]

let name = "arm"
let triple = "arm-linux-gnueabihf"

(* The A32 encoder's error domain (docs/errors.md). Below source text, so it
   names no token; the front end's operand failures are a separate domain in
   arm.ml, for the reason {!Target_intf.Target.TARGET} gives.

   [`No_modified_immediate] carries the value that has no representation, which
   is the whole content of the failure: an A32 immediate is a rotation of an
   eight-bit field, so *which constant* is what a caller needs. *)
type error_kind =
  [ Target_error.shared
  | `Thumb_out_of_scope
  | `Expected_register_or_shifted
  | `No_modified_immediate of int64
  | `No_modified_immediate_2op of no_modified_immediate_2op
  | `Writeback_out_of_scope
  | `Offset_not_12bit
  | `Offset_not_8bit
  | `Bitfield_out_of_range
  | `Udf_imm16_overflow
  | `Wrong_relocation_modifier of wrong_relocation_modifier
  | `Missing_relocation_modifier of missing_relocation_modifier
  | `Imm16_overflow of Opcode.t
  | `Codec of Codec.error
  | `Decode_short
  | `Decode_no_match
  | `Branch_not_word_aligned
  | `Branch_out_of_range
  | `Vldr_literal_backward
  | `Vldr_literal_not_word_aligned
  | `Vldr_literal_out_of_range
  | `No_data_relocation of int
  | `Padding_not_word_multiple
  | `No_vfp_immediate of float
  | `Vfp_offset_out_of_range of int64
  | `Vmrs_unsupported_operands
  | `Push_needs_two_or_more_registers
  | `Pop_needs_two_or_more_registers
  | `Vpush_needs_contiguous_registers
  | `Shift_amount_out_of_range ]

and wrong_relocation_modifier = { opcode : Opcode.t; want : string; got : string }
and missing_relocation_modifier = { opcode : Opcode.t; want : string }
and no_modified_immediate_2op = { opcode : Opcode.t; value : int64 }

type error = error_kind Target_error.t

let pp_error_kind ppf : error_kind -> unit = function
  | #Target_error.shared as e -> Target_error.pp_shared ppf e
  | `Thumb_out_of_scope -> Fmt.string ppf "Thumb is not in M1 scope"
  | `Expected_register_or_shifted -> Fmt.string ppf "expected a register or a shifted register"
  | `No_modified_immediate v -> Fmt.pf ppf "#%Ld has no A32 modified-immediate representation" v
  | `No_modified_immediate_2op { opcode; value } ->
      Fmt.pf ppf "%s #%Ld has no A32 modified-immediate representation; movw is M2"
        (Opcode.name opcode) value
  | `Writeback_out_of_scope -> Fmt.string ppf "writeback addressing is not in M1 scope"
  | `Offset_not_12bit -> Fmt.string ppf "offset does not fit the 12-bit immediate"
  | `Offset_not_8bit -> Fmt.string ppf "offset does not fit the 8-bit immediate"
  | `Bitfield_out_of_range -> Fmt.string ppf "bitfield lsb/width do not fit 32 bits"
  | `Udf_imm16_overflow -> Fmt.string ppf "udf immediate does not fit 16 bits"
  | `Wrong_relocation_modifier { opcode; want; got } ->
      Fmt.pf ppf "%s takes #:%s:, not #:%s:" (Opcode.name opcode) want got
  | `Missing_relocation_modifier { opcode; want } ->
      Fmt.pf ppf "%s needs a #:%s: operand" (Opcode.name opcode) want
  | `Imm16_overflow op -> Fmt.pf ppf "%s immediate does not fit 16 bits" (Opcode.name op)
  | `Codec e -> Codec.pp_error ppf e
  | `Decode_short -> Fmt.string ppf "fewer than four bytes remain"
  | `Decode_no_match -> Fmt.string ppf "no form matches this word"
  | `Branch_not_word_aligned -> Fmt.string ppf "branch target is not word-aligned"
  | `Branch_out_of_range -> Fmt.string ppf "branch target is out of range"
  | `Vldr_literal_backward ->
      Fmt.string ppf
        "vldr literal-pool target must follow the instruction; a backward one is not in scope"
  | `Vldr_literal_not_word_aligned -> Fmt.string ppf "vldr literal-pool target is not word-aligned"
  | `Vldr_literal_out_of_range -> Fmt.string ppf "vldr literal-pool target is out of range"
  | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte data initializer" w
  | `Padding_not_word_multiple ->
      Fmt.string ppf "A32 padding must be a whole number of four-byte instructions"
  | `No_vfp_immediate v ->
      Fmt.pf ppf "#%s has no VFP modified-immediate representation" (decimal_of_float v)
  | `Vfp_offset_out_of_range off ->
      Fmt.pf ppf "vldr/vstr offset %Ld does not fit a word-aligned 10-bit range" off
  | `Vmrs_unsupported_operands -> Fmt.string ppf "vmrs only supports APSR_nzcv, FPSCR in M1 scope"
  | `Push_needs_two_or_more_registers ->
      Fmt.string ppf
        "push needs two or more registers; a single register is str rN, [sp, #-4]!, not in scope"
  | `Pop_needs_two_or_more_registers ->
      Fmt.string ppf
        "pop needs two or more registers; a single register is ldr rN, [sp], #4, not in scope"
  | `Vpush_needs_contiguous_registers ->
      Fmt.string ppf
        "vpush.64 needs a non-empty, ascending, contiguous D-register list, e.g. {d8, d9}"
  | `Shift_amount_out_of_range -> Fmt.string ppf "shift amount does not fit the 5-bit immediate"

let error_kind_code : error_kind -> string = function
  | `Unknown_instruction _ -> "arm.simplify"
  | `Thumb_out_of_scope | `Immediate_too_wide | `Expected_register_or_shifted
  | `No_modified_immediate _ | `No_modified_immediate_2op _ | `Writeback_out_of_scope
  | `Offset_not_12bit | `Offset_not_8bit | `Bitfield_out_of_range | `Udf_imm16_overflow
  | `Wrong_relocation_modifier _ | `Missing_relocation_modifier _ | `Imm16_overflow _ | `No_form _
  | `No_vfp_immediate _ | `Vfp_offset_out_of_range _ | `Vmrs_unsupported_operands
  | `Push_needs_two_or_more_registers | `Pop_needs_two_or_more_registers
  | `Vpush_needs_contiguous_registers | `Shift_amount_out_of_range ->
      "arm.lower"
  | `Codec e -> Option.value (Codec.code e) ~default:"arm.encode"
  | `Decode_short | `Decode_no_match | `Decode_no_normalized -> "arm.decode"
  | `Branch_not_word_aligned | `Branch_out_of_range | `Vldr_literal_backward
  | `Vldr_literal_not_word_aligned | `Vldr_literal_out_of_range ->
      "arm.fixup"
  | `No_data_relocation _ -> "arm.data-fixup"
  | `Padding_not_word_multiple -> "arm.nop"
  | `Feature_disabled _ -> "arm.feature"

let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
let error_code e = error_kind_code (Target_error.kind e)
let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

(* Returns the wrapped error rather than a bare one, so every [Error (diag ...)]
   site keeps its shape: [Err.t] is a [result] whose error branch is an
   [Err.Error.t]. *)
let diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error
    (Target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"arm" ())
       kind)

let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

(* {1 Simplify} *)

let simplify_instruction (_ : target_state) s =
  let bad kind = Error (diag ~pos:__POS__ ~origin:s.Surface.origin kind) in
  let m = s.Surface.mnemonic in
  match Opcode.of_mnemonic m with
  (* The unsuffixed spelling first, so [b] is the branch rather than [b] with a
     stripped condition, and [bl] is not read as [b] plus [l]. Only when no
     opcode claims the whole mnemonic is a condition suffix looked for. *)
  | Some op -> Ok (Instruction.mk op s.Surface.ops)
  | None -> (
      match Opcode.split_vfp_mnemonic m with
      | Some (stem, cond, rest) -> (
          match Opcode.vfp_opcode_of stem rest with
          | Some op -> Ok (Instruction.mk ~cond op s.Surface.ops)
          | None -> bad (`Unknown_instruction m))
      | None -> (
          let stem, cond = Cond.split m in
          match Opcode.of_mnemonic stem with
          | Some op -> Ok (Instruction.mk ~cond op s.Surface.ops)
          | None -> bad (`Unknown_instruction m)))

(* {1 Lower} *)

(* Shared by [ldr]/[str]/[strb]: the mnemonic and byte-width vary, but which
   [Lowered] form a memory operand becomes - [Ldst_imm] or [Ldst_reg] - and
   the 12-bit magnitude check that only the immediate form needs, do not. *)
let lower_ldst ~bad ~cond ~load ~byte ~rt ~(m : Mem.t) =
  match m.offset with
  | Mem.Imm offset ->
      if Int64.compare (Int64.abs offset) 4096L >= 0 then bad `Offset_not_12bit
      else
        Ok
          [
            Lowered.Ldst_imm
              { cond; load; byte; rt; rn = m.base; offset; writeback = m.writeback; post = false };
          ]
  | Mem.Reg_offset { reg = rm; negate; kind = sh_kind; amount = sh_amt } ->
      (* Register-offset writeback ([str r0, [r1, r2]!]) is real ARM syntax
         but corpus-unevidenced (M5 classify-c-gcc's one finding, [sp, #-N]!,
         is always an immediate offset - the corpus notes) - stays out of
         scope here even though the immediate sibling just above is now in. *)
      if m.writeback then bad `Writeback_out_of_scope
      else
        Ok [ Lowered.Ldst_reg { cond; load; byte; rt; rn = m.base; rm; negate; sh_kind; sh_amt } ]

(* The operand lists an entry takes, and the lowered form its encoding class makes of them: the
   regular opcodes lower here, from their table entry; the rest keep their code below. *)
let lower_regular (e : Arm_ops.entry) cond ops =
  let open Arm_ops in
  let bad kind = Error kind in
  let imm_of v =
    match Bigint.to_int64_opt v with Some x -> Ok x | None -> Error `Immediate_too_wide
  in
  let kinds = List.map Kind.of_operand ops in
  if not (List.exists (fun (s : shape) -> s.kinds = kinds) e.shapes || List.mem kinds e.also) then
    bad (`No_form e.name)
  else
    let ( let* ) = Result.bind in
    let r0 = Reg.of_num 0 in
    (* a register, optionally shifted, as a [Dp_reg]'s [rm] and shift *)
    let rm_shift = function
      | Operand.Reg rm -> (rm, 0, 0)
      | Operand.Shifted { reg; kind; amount } -> (reg, kind, amount)
      | _ -> (r0, 0, 0)
    in
    let modimm err v =
      let* imm = imm_of v in
      if encode_modimm imm = None then bad (err imm) else Ok imm
    in
    let dp_reg ~dp ~s ~rd ~rn x =
      let rm, sh_kind, sh_amt = rm_shift x in
      Ok [ Lowered.Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } ]
    in
    match (e.enc, ops) with
    | Enc.Dp { dp; s; form = Enc.Move }, [ Operand.Reg rd; Operand.Imm v ] ->
        let* imm = modimm (fun value -> `No_modified_immediate_2op { opcode = e.op; value }) v in
        Ok [ Lowered.Dp_imm { cond; dp; s; rd; rn = r0; imm } ]
    | Enc.Dp { dp; s; form = Enc.Move }, [ Operand.Reg rd; x ] -> dp_reg ~dp ~s ~rd ~rn:r0 x
    | Enc.Dp { dp; s; form = Enc.Move }, [ Operand.Reg rd; Operand.Reg rn; x ] ->
        dp_reg ~dp ~s ~rd ~rn x
    | Enc.Dp { dp; s; form = Enc.Normal }, [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] ->
        let* imm = modimm (fun v -> `No_modified_immediate v) v in
        Ok [ Lowered.Dp_imm { cond; dp; s; rd; rn; imm } ]
    | Enc.Dp { dp; s; form = Enc.Normal }, [ Operand.Reg rd; x ] -> dp_reg ~dp ~s ~rd ~rn:r0 x
    | Enc.Dp { dp; s; form = Enc.Normal }, [ Operand.Reg rd; Operand.Reg rn; x ] ->
        dp_reg ~dp ~s ~rd ~rn x
    | Enc.Dp { dp; s; form = Enc.Compare }, [ Operand.Reg rn; Operand.Imm v ] ->
        let* imm = modimm (fun v -> `No_modified_immediate v) v in
        Ok [ Lowered.Dp_imm { cond; dp; s; rd = r0; rn; imm } ]
    | Enc.Dp { dp; s; form = Enc.Compare }, [ Operand.Reg rn; x ] -> dp_reg ~dp ~s ~rd:r0 ~rn x
    | Enc.Shift_op { kind; s }, [ Operand.Reg rd; Operand.Reg rm; Operand.Reg rs ] when not s ->
        Ok [ Lowered.Shift_reg { cond; kind; rd; rm; rs } ]
    | Enc.Shift_op { kind; s }, [ Operand.Reg rd; Operand.Reg rm; Operand.Imm v ] ->
        let* amount = imm_of v in
        if Int64.compare amount 0L < 0 || Int64.compare amount 32L >= 0 then
          bad `Shift_amount_out_of_range
        else
          Ok
            [
              Lowered.Dp_reg
                {
                  cond;
                  dp = Opcode.to_dp Opcode.Mov;
                  s;
                  rd;
                  rn = r0;
                  rm;
                  sh_kind = kind;
                  sh_amt = Int64.to_int amount;
                };
            ]
    | Enc.Rrx { s }, [ Operand.Reg rd; Operand.Reg rm ] ->
        Ok
          [
            Lowered.Dp_reg
              { cond; dp = Opcode.to_dp Opcode.Mov; s; rd; rn = r0; rm; sh_kind = 3; sh_amt = 0 };
          ]
    | Enc.Mul, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] ->
        Ok [ Lowered.Mul { cond; s = false; rd; rn; rm } ]
    | Enc.Mla, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ] ->
        Ok [ Lowered.Mla { cond; s = false; rd; rn; rm; ra } ]
    | Enc.Umull, [ Operand.Reg rdlo; Operand.Reg rdhi; Operand.Reg rn; Operand.Reg rm ] ->
        Ok [ Lowered.Umull { cond; s = false; rdlo; rdhi; rn; rm } ]
    | Enc.Rd_rm _, [ Operand.Reg rd; Operand.Reg rm ] ->
        Ok [ Lowered.Rd_rm { cond; op = e.op; rd; rm } ]
    | Enc.Sbfx, [ Operand.Reg rd; Operand.Reg rn; Operand.Imm lsb; Operand.Imm width ] -> (
        match (Bigint.to_int_opt lsb, Bigint.to_int_opt width) with
        | Some lsb, Some width when lsb >= 0 && lsb < 32 && width >= 1 && lsb + width <= 32 ->
            Ok [ Lowered.Sbfx { cond; rd; rn; lsb; width } ]
        | _ -> bad `Bitfield_out_of_range)
    | Enc.Bx, [ Operand.Reg rm ] -> Ok [ Lowered.Bx { cond; rm } ]
    | Enc.Bl, [ Operand.Sym x ] ->
        Ok
          [ Lowered.Bl { cond; target = Asm_core.Lowered_ast.Symbolic { value = x; rung = None } } ]
    (* A32 branches are fixed-width: one form, no ladder, so no rung to pin. The 24-bit field
       reaches 32 MiB either way, which every fixture is far inside. *)
    | Enc.B, [ Operand.Sym x ] ->
        Ok [ Lowered.B { cond; target = Asm_core.Lowered_ast.Symbolic { value = x; rung = None } } ]
    | Enc.Nop, [] -> Ok [ Lowered.Nop { cond } ]
    | Enc.V3 { dbl = true; _ }, [ Operand.Dreg vd; Operand.Dreg vn; Operand.Dreg vm ] -> (
        match e.op with
        | Opcode.V3d op -> Ok [ Lowered.V3_d { cond; op; vd; vn; vm } ]
        | _ -> bad (`No_form e.name))
    | Enc.V3 { dbl = false; _ }, [ Operand.Sreg vd; Operand.Sreg vn; Operand.Sreg vm ] -> (
        match e.op with
        | Opcode.V3s op -> Ok [ Lowered.V3_s { cond; op; vd; vn; vm } ]
        | _ -> bad (`No_form e.name))
    | Enc.Vmov { dbl = true }, [ Operand.Dreg vd; Operand.Dreg vm ] ->
        Ok [ Lowered.Vmov_reg_d { cond; vd; vm } ]
    | Enc.Vmov { dbl = true }, [ Operand.Dreg vd; Operand.FImm v ] -> (
        match vfp_compress v with
        | Some imm8 -> Ok [ Lowered.Vmov_imm_d { cond; vd; imm8 } ]
        | None -> bad (`No_vfp_immediate v))
    | Enc.Vmov { dbl = false }, [ Operand.Sreg vd; Operand.Sreg vm ] ->
        Ok [ Lowered.Vmov_reg_s { cond; vd; vm } ]
    | Enc.Vmov { dbl = false }, [ Operand.Sreg vd; Operand.FImm v ] -> (
        match vfp_compress v with
        | Some imm8 -> Ok [ Lowered.Vmov_imm_s { cond; vd; imm8 } ]
        | None -> bad (`No_vfp_immediate v))
    | Enc.Vmov_core, [ Operand.Sreg sn; Operand.Reg rt ] ->
        Ok [ Lowered.Vmov_to_single { cond; sn; rt } ]
    | Enc.Vmov_core, [ Operand.Reg rt; Operand.Sreg sn ] ->
        Ok [ Lowered.Vmov_from_single { cond; rt; sn } ]
    | Enc.Vmov_core, [ Operand.Dreg dm; Operand.Reg rt; Operand.Reg rt2 ] ->
        Ok [ Lowered.Vmov_to_double { cond; dm; rt; rt2 } ]
    | Enc.Vmov_core, [ Operand.Reg rt; Operand.Reg rt2; Operand.Dreg dm ] ->
        Ok [ Lowered.Vmov_from_double { cond; rt; rt2; dm } ]
    | Enc.Vneg { dbl = true }, [ Operand.Dreg vd; Operand.Dreg vm ] ->
        Ok [ Lowered.Vneg_d { cond; vd; vm } ]
    | Enc.Vneg { dbl = false }, [ Operand.Sreg vd; Operand.Sreg vm ] ->
        Ok [ Lowered.Vneg_s { cond; vd; vm } ]
    | Enc.Vcmp { dbl = true }, [ Operand.Dreg vd; Operand.Dreg vm ] ->
        Ok [ Lowered.Vcmp_reg_d { cond; vd; vm } ]
    | Enc.Vcmp { dbl = true }, [ Operand.Dreg vd; Operand.Imm v ] when Bigint.is_zero v ->
        Ok [ Lowered.Vcmp_zero_d { cond; vd } ]
    | Enc.Vcmp { dbl = false }, [ Operand.Sreg vd; Operand.Sreg vm ] ->
        Ok [ Lowered.Vcmp_reg_s { cond; vd; vm } ]
    | Enc.Vcmp { dbl = false }, [ Operand.Sreg vd; Operand.Imm v ] when Bigint.is_zero v ->
        Ok [ Lowered.Vcmp_zero_s { cond; vd } ]
    | Enc.Vcvt Enc.F32_f64, [ Operand.Sreg sd; Operand.Dreg dm ] ->
        Ok [ Lowered.Vcvt_f32_f64 { cond; sd; dm } ]
    | Enc.Vcvt Enc.F64_f32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_f32 { cond; dd; sm } ]
    | Enc.Vcvt Enc.F32_s32, [ Operand.Sreg sd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f32_s32 { cond; sd; sm } ]
    | Enc.Vcvt Enc.F64_s32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_s32 { cond; dd; sm } ]
    | Enc.Vcvt Enc.F64_u32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_u32 { cond; dd; sm } ]
    | Enc.Vcvt Enc.S32_f64, [ Operand.Sreg sd; Operand.Dreg dm ] ->
        Ok [ Lowered.Vcvt_s32_f64 { cond; sd; dm } ]
    | _ -> bad (`No_form e.name)

let lower_instruction state i =
  let bad kind = Error (diag ~pos:__POS__ kind) in
  if state.thumb then bad `Thumb_out_of_scope
  else
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag ~pos:__POS__ `Immediate_too_wide)
    in
    let cond = i.Instruction.cond in
    match Arm_ops.find_opt i.Instruction.op with
    | Some e when not e.Arm_ops.lower ->
        Result.map_error (fun k -> diag ~pos:__POS__ k) (lower_regular e cond i.Instruction.ops)
    | _ -> (
        match (i.Instruction.op, i.Instruction.ops) with
        (* str/ldr writeback ([str fp, [sp, #-4]!]) is M5 classify-c-gcc's one
       corpus-evidenced writeback form (gcc's own
       frame-pointer prologue idiom) - lower_ldst now honors m.writeback for
       an immediate offset instead of rejecting it outright. strb/ldrb below
       keep the blanket rejection: no byte-access writeback is evidenced, and
       nothing about the word-vs-byte bit makes it a safe default to widen
       for free. *)
        | ((Opcode.Str | Opcode.Ldr) as op), [ Operand.Reg rt; Operand.Mem m ] ->
            lower_ldst ~bad ~cond ~load:(op = Opcode.Ldr) ~byte:false ~rt ~m
        (* Post-indexed [ldr rt, \[rn\], #imm]: the operand parser reads the bracket and the
       offset after it as two operands. the compiler's inline [memcpy] loop is the evidence. *)
        | ( ((Opcode.Str | Opcode.Ldr | Opcode.Strb | Opcode.Ldrb) as op),
            [
              Operand.Reg rt;
              Operand.Mem { base; offset = Mem.Imm 0L; writeback = false; pre = true };
              Operand.Imm v;
            ] ) -> (
            match Bigint.to_int64_opt v with
            | Some offset when Int64.compare (Int64.abs offset) 4096L < 0 ->
                Ok
                  [
                    Lowered.Ldst_imm
                      {
                        cond;
                        load = op = Opcode.Ldr || op = Opcode.Ldrb;
                        byte = op = Opcode.Strb || op = Opcode.Ldrb;
                        rt;
                        rn = base;
                        offset;
                        writeback = false;
                        post = true;
                      };
                  ]
            | _ -> bad `Offset_not_12bit)
        | Opcode.Strb, [ Operand.Reg rt; Operand.Mem m ] ->
            if m.writeback then bad `Writeback_out_of_scope
            else lower_ldst ~bad ~cond ~load:false ~byte:true ~rt ~m
        | Opcode.Ldrb, [ Operand.Reg rt; Operand.Mem m ] ->
            if m.writeback then bad `Writeback_out_of_scope
            else lower_ldst ~bad ~cond ~load:true ~byte:true ~rt ~m
        (* The extra loads and stores ({!Lowered.Ldst_x}): an 8-bit immediate or a plain register
       offset, and the post-indexed immediate form. *)
        | ( ((Opcode.Strh | Opcode.Ldrh | Opcode.Ldrsb | Opcode.Ldrsh) as op),
            Operand.Reg rt :: Operand.Mem m :: post_off ) -> (
            let kind =
              match op with Opcode.Strh -> 0 | Opcode.Ldrh -> 1 | Opcode.Ldrsb -> 2 | _ -> 3
            in
            let imm v =
              if Int64.compare (Int64.abs v) 256L >= 0 then bad `Offset_not_8bit else Ok (X_imm v)
            in
            let mk offset post =
              Ok [ Lowered.Ldst_x { cond; kind; rt; rn = m.base; offset; post } ]
            in
            if m.writeback then bad `Writeback_out_of_scope
            else
              match (m.offset, post_off) with
              | Mem.Imm v, [] -> Result.bind (imm v) (fun o -> mk o false)
              | Mem.Reg_offset { reg; negate; kind = 0; amount = 0 }, [] ->
                  mk (X_reg { rm = reg; negate }) false
              | Mem.Imm 0L, [ Operand.Imm v ] -> (
                  match Bigint.to_int64_opt v with
                  | Some v -> Result.bind (imm v) (fun o -> mk o true)
                  | None -> bad `Offset_not_8bit)
              | _ -> bad (`No_form (Opcode.name op)))
        | Opcode.Push, [ Operand.Reglist regs ] ->
            if List.length regs < 2 then bad `Push_needs_two_or_more_registers
            else
              Ok
                [
                  Lowered.Push
                    { cond; regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs };
                ]
        | (Opcode.Stmia | Opcode.Ldmia), [ Operand.Reg_writeback rn; Operand.Reglist regs ] ->
            Ok
              [
                Lowered.Ldm
                  {
                    cond;
                    load = i.Instruction.op = Opcode.Ldmia;
                    rn;
                    regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs;
                  };
              ]
        | Opcode.Pop, [ Operand.Reglist regs ] ->
            if List.length regs < 2 then bad `Pop_needs_two_or_more_registers
            else
              Ok
                [
                  Lowered.Ldm
                    {
                      cond;
                      load = true;
                      rn = Reg.sp;
                      regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs;
                    };
                ]
        | Opcode.Vpush, [ Operand.Dreglist regs ] -> (
            let sorted = List.sort compare regs in
            match sorted with
            | [] -> bad `Vpush_needs_contiguous_registers
            | base :: _ as members ->
                let count = List.length members in
                if List.for_all2 (fun n i -> n = base + i) members (List.init count Fun.id) then
                  Ok [ Lowered.Vpush { cond; vd = Dreg.of_num base; count } ]
                else bad `Vpush_needs_contiguous_registers)
        | Opcode.Udf, [ Operand.Imm v ] -> (
            match imm_of v with
            | Error e -> Error e
            | Ok imm ->
                if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
                  bad `Udf_imm16_overflow
                else Ok [ Lowered.Udf { imm16 = imm } ])
        (* [movw]/[movt]. Per the contract's §B7 the modifier is *consumed* here -
       it selects the form, and what is stored is the expression underneath -
       because [Expr.fold] deliberately preserves a [Modifier], so one left in
       place would reach [bind_image] as a residual and be reported there
       instead of being understood here.
       The pairing is checked rather than assumed: [movw] takes [:lower16:] and
       [movt] takes [:upper16:], and the two are not interchangeable - a [movt]
       given the low half would silently write the wrong sixteen bits of every
       address. *)
        | ((Opcode.Movw | Opcode.Movt) as op), [ Operand.Reg rd; Operand.Sym e ] -> (
            let top = op = Opcode.Movt in
            let want = if top then "upper16" else "lower16" in
            match e with
            | Asm_core.Expr.Modifier (m, inner) when String.equal m want ->
                Ok
                  [
                    Lowered.Movw_movt
                      {
                        cond;
                        top;
                        rd;
                        imm = Asm_core.Lowered_ast.Symbolic { value = inner; rung = None };
                      };
                  ]
            | Asm_core.Expr.Modifier (m, _) ->
                bad (`Wrong_relocation_modifier { opcode = op; want; got = m })
            | _ -> bad (`Missing_relocation_modifier { opcode = op; want }))
        | ((Opcode.Movw | Opcode.Movt) as op), [ Operand.Reg rd; Operand.Imm v ] -> (
            match imm_of v with
            | Error e -> Error e
            | Ok imm ->
                if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
                  bad (`Imm16_overflow op)
                else
                  Ok
                    [
                      Lowered.Movw_movt
                        {
                          cond;
                          top = op = Opcode.Movt;
                          rd;
                          imm = Asm_core.Lowered_ast.Resolved { value = imm; rung = Opcode.name op };
                        };
                    ])
        | ( Opcode.Vmrs,
            [ Operand.Sym (Asm_core.Expr.Symbol a); Operand.Sym (Asm_core.Expr.Symbol f) ] )
          when String.equal a "APSR_nzcv" && String.equal f "FPSCR" ->
            Ok [ Lowered.Vmrs { cond } ]
        | Opcode.Vmrs, _ -> bad `Vmrs_unsupported_operands
        (* [vldr]/[vstr] offset-only addressing: the 8-bit field is scaled by 4,
       so the reach is a word-aligned +/-1020 - {!Mem.Reg_offset} (register
       offset) has no VFP encoding at all and falls to [No_form]. *)
        | ( (Opcode.Vldr | Opcode.Vstr),
            [ Operand.Dreg vd; Operand.Mem { offset = Mem.Imm offset; base; _ } ] ) ->
            if Int64.rem offset 4L <> 0L || Int64.compare (Int64.abs offset) 1024L >= 0 then
              bad (`Vfp_offset_out_of_range offset)
            else
              Ok
                [
                  Lowered.Vmem_d
                    { cond; load = i.Instruction.op = Opcode.Vldr; vd; rn = base; offset };
                ]
        | ( (Opcode.Vldr | Opcode.Vstr),
            [ Operand.Sreg vd; Operand.Mem { offset = Mem.Imm offset; base; _ } ] ) ->
            if Int64.rem offset 4L <> 0L || Int64.compare (Int64.abs offset) 1024L >= 0 then
              bad (`Vfp_offset_out_of_range offset)
            else
              Ok
                [
                  Lowered.Vmem_s
                    { cond; load = i.Instruction.op = Opcode.Vldr; vd; rn = base; offset };
                ]
        (* [vldr Dd/Sd, label] - a PC-relative literal-pool load (docs/corpus.md:
       fftsp.c's/knucleotide.c's own [.f32] float constants). Load-only:
       {!Opcode.Vstr} never reaches here, since real hardware defines a
       store's [Rn = 1111] as UNPREDICTABLE and no fixture spells it. *)
        | Opcode.Vldr, [ Operand.Dreg vd; Operand.Sym e ] ->
            Ok
              [
                Lowered.Vldr_lit_d
                  { cond; vd; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } };
              ]
        | Opcode.Vldr, [ Operand.Sreg vd; Operand.Sym e ] ->
            Ok
              [
                Lowered.Vldr_lit_s
                  { cond; vd; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } };
              ]
        (* [ite] is a Thumb construct. GAS accepts it in A32 source and emits
       nothing, because conditional execution there is native to every
       instruction - the suffixed [moveq]/[movne] that follow already say what
       the [ite] would have said. Measured: the fixture's offsets step straight
       from the [cmp] to the [moveq]. Accepting and dropping it is therefore
       matching GAS, not ignoring an instruction. *)
        | Opcode.Ite, _ -> Ok []
        | _ -> bad (`No_form (Opcode.name i.Instruction.op)))

(* {1 Encode and decode}

   A32 words are stored little-endian, and the codec produced them most
   significant bit first, so the four bytes are reversed exactly once, here. *)

let word_to_memory s = if String.length s <> 4 then s else String.init 4 (fun i -> s.[3 - i])

(* The same reversal reaches the fixups: a codec bit at position [p] of the
   most-significant-first word lands at bit [32 - p - w] of the little-endian
   container. movw/movt's imm4 and imm12 are two slices of one value under this,
   with no special case. [pc_bias] is 8 - the A32 pipeline offset - and it lives
   here rather than inside [evaluate_fixup], so that the evaluator receives a PC
   and not a thing it has to correct. *)
let fixup_of_placement (p : fixup_kind C.placement) =
  let kind = p.C.kind in
  {
    Asm_core.Lowered_ast.kind;
    kind_name = fixup_kind_name kind;
    family = fixup_family kind;
    role = fixup_role kind;
    name = p.C.name;
    slices =
      List.map
        (fun (s : C.slice) ->
          {
            Asm_core.Lowered_ast.bit_offset = 32 - s.C.bit_offset - s.C.bit_width;
            bit_width = s.C.bit_width;
            value_lsb = s.C.value_lsb;
          })
        p.C.slices;
    byte_offset = 0;
    container = 4;
    pc_bias = (match kind with Pcrel_b26 | Pcrel_call | Pcrel_vldr8 -> 8 | _ -> 0);
    range =
      (match kind with
      | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
      | Pcrel_b26 | Pcrel_call -> Asm_core.Lowered_ast.Signed 24
      | Movw_abs_nc | Movt_abs -> Asm_core.Lowered_ast.Unsigned 16
      (* Already reduced to the final imm8 field's own value by
         [evaluate_fixup] (word-shifted, forward-only) - this is a defensive
         re-check, the same role [Unsigned 16] plays for [Movw_abs_nc]/
         [Movt_abs] above, not a second computation. *)
      | Pcrel_vldr8 -> Asm_core.Lowered_ast.Unsigned 8);
    value = Asm_core.Expr.Const (Bigint.of_int 0);
    pairing = Asm_core.Lowered_ast.Unpaired;
    origin = Origin.synthesized ~pass:"arm.encode" ();
  }

(* The expression lives in the lowered value - Codec sits below asm_core and
   cannot mention Expr - and is paired with the placement by name. A placement
   with no expression came from a Resolved operand whose bits are already
   final, so it yields no linker fixup. *)
let expr_of_lowered : Lowered.t -> (string * Asm_core.Expr.t) list = function
  | Lowered.Bl { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.B { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.Vldr_lit_d { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.Vldr_lit_s { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } ->
      [ ("target", value) ]
  (* The name is the codec placement's, not a description: [form_of] pairs the
     two by name, and a mismatch here does not fail - it silently produces an
     encoding with no fixup, which then binds to the placeholder. *)
  | Lowered.Movw_movt { imm = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } -> [ ("imm", value) ]
  | _ -> []

let form_of l enc =
  let exprs = expr_of_lowered l in
  {
    Asm_core.Lowered_ast.bytes = word_to_memory (C.Bits.to_bytes enc.C.bits);
    form = C.form_id enc;
    fixups =
      List.filter_map
        (fun (p : fixup_kind C.placement) ->
          match List.assoc_opt p.C.name exprs with
          | None -> None
          | Some value -> Some { (fixup_of_placement p) with Asm_core.Lowered_ast.value })
        enc.C.placements;
  }

(* A32 is fixed-width: no ladder, every form `Fixed. *)
let encode l =
  match C.encode codec l with
  (* A32 is fixed-width and declares no ladder, so [e.code] is always [None]
     here; reading it anyway keeps one rule across the three targets rather than
     a special case waiting to be forgotten. *)
  | Error (e : C.error) -> Error (diag ~pos:__POS__ (`Codec e))
  | Ok enc -> Ok (`Fixed (form_of l enc))

let encode_in (_ : target_state) l = encode l

type decode_context = { state : target_state; address : int64 }

(* The opcode the table names for an encoding class. *)
let op_of enc = Option.get (Arm_ops.of_enc enc)

let instruction_of_lowered ?(at = 0L) =
  (* A branch field is a word count measured from the instruction plus eight,
     the A32 pipeline offset, so the address it names is [at + 8 + 4 * imm24].
     Printing the absolute target rather than the count is what objdump does and
     the only spelling re-parsing can mean. *)
  let absolute_target = function
    | Asm_core.Lowered_ast.Resolved { value; _ } -> Int64.add at (Int64.add 8L (Int64.mul value 4L))
    | Asm_core.Lowered_ast.Symbolic _ -> at
  in
  function
  | Lowered.Dp_imm { cond; dp; s; rd; rn; imm } -> (
      let imm = Operand.Imm (Bigint.of_int64 imm) in
      match Opcode.of_dp dp with
      | None -> None
      | Some o -> (
          match (Arm_ops.find o).Arm_ops.enc with
          | Arm_ops.Enc.Dp { form = Arm_ops.Enc.Move; _ } ->
              Some (Instruction.mk ~cond o [ Operand.Reg rd; imm ])
          | Arm_ops.Enc.Dp { form = Arm_ops.Enc.Compare; _ } ->
              Some (Instruction.mk ~cond o [ Operand.Reg rn; imm ])
          | _ ->
              (* a set S bit names the flag-setting opcode where it has an immediate form;
                 without it [subs rd, rn, #imm] would decode as plain [sub], which does not
                 reassemble to the original bytes *)
              let o =
                match if s then Arm_ops.s_form dp Arm_ops.Kind.[ R; R; Imm ] else None with
                | Some s_op -> s_op
                | None -> o
              in
              Some (Instruction.mk ~cond o [ Operand.Reg rd; Operand.Reg rn; imm ])))
  | Lowered.Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } -> (
      let shifted =
        if sh_kind = 0 && sh_amt = 0 then Operand.Reg rm
        else Operand.Shifted { reg = rm; kind = sh_kind; amount = sh_amt }
      in
      match Opcode.of_dp dp with
      | None -> None
      | Some o -> (
          match (Arm_ops.find o).Arm_ops.enc with
          | Arm_ops.Enc.Dp { form = Arm_ops.Enc.Move; _ } when s && o = Opcode.Mov -> (
              (* [movs rd, rm, <shift> #imm] is a flag-setting shift, not a plain move that drops
                 [s]; no shift (a bare [movs rd, rm]) is not reconstructed *)
              match shifted with
              | Operand.Shifted { kind; amount; _ } ->
                  Option.map
                    (fun o ->
                      Instruction.mk ~cond o
                        [ Operand.Reg rd; Operand.Reg rm; Operand.Imm (Bigint.of_int amount) ])
                    (Arm_ops.of_enc (Arm_ops.Enc.Shift_op { kind; s = true }))
              | _ -> None)
          | Arm_ops.Enc.Dp { form = Arm_ops.Enc.Move; _ } ->
              Some (Instruction.mk ~cond o [ Operand.Reg rd; shifted ])
          | Arm_ops.Enc.Dp { form = Arm_ops.Enc.Compare; _ } ->
              Some (Instruction.mk ~cond o [ Operand.Reg rn; shifted ])
          | _ ->
              let o =
                match if s then Arm_ops.s_form dp Arm_ops.Kind.[ R; R; R ] else None with
                | Some s_op -> s_op
                | None -> o
              in
              Some (Instruction.mk ~cond o [ Operand.Reg rd; Operand.Reg rn; shifted ])))
  | Lowered.Nop { cond } -> Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Nop) [])
  | Lowered.Shift_reg { cond; kind; rd; rm; rs } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Shift_op { kind; s = false }))
           [ Operand.Reg rd; Operand.Reg rm; Operand.Reg rs ])
  | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset; writeback; post } ->
      let ops =
        if post then
          [
            Operand.Reg rt;
            Operand.Mem { base = rn; offset = Mem.Imm 0L; writeback = false; pre = true };
            Operand.Imm (Bigint.of_int64 offset);
          ]
        else
          [
            Operand.Reg rt;
            Operand.Mem { base = rn; offset = Mem.Imm offset; writeback; pre = true };
          ]
      in
      Some (Instruction.mk ~cond (op_of (Arm_ops.Enc.Ldst { load; byte })) ops)
  | Lowered.Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } ->
      let mem_op =
        Operand.Mem
          {
            base = rn;
            offset = Mem.Reg_offset { reg = rm; negate; kind = sh_kind; amount = sh_amt };
            writeback = false;
            pre = true;
          }
      in
      Some
        (Instruction.mk ~cond (op_of (Arm_ops.Enc.Ldst { load; byte })) [ Operand.Reg rt; mem_op ])
  | Lowered.Ldst_x { cond; kind; rt; rn; offset; post } ->
      let op = op_of (Arm_ops.Enc.Ldst_x { kind; bits = x_bits kind }) in
      let mem off = Operand.Mem { base = rn; offset = off; writeback = false; pre = true } in
      let ops =
        match (offset, post) with
        | X_imm v, false -> [ Operand.Reg rt; mem (Mem.Imm v) ]
        | X_imm v, true -> [ Operand.Reg rt; mem (Mem.Imm 0L); Operand.Imm (Bigint.of_int64 v) ]
        | X_reg { rm; negate }, false ->
            [ Operand.Reg rt; mem (Mem.Reg_offset { reg = rm; negate; kind = 0; amount = 0 }) ]
        | X_reg _, true -> []
      in
      if ops = [] then None else Some (Instruction.mk ~cond op ops)
  | Lowered.Rd_rm { cond; op; rd; rm } ->
      Some (Instruction.mk ~cond op [ Operand.Reg rd; Operand.Reg rm ])
  | Lowered.Sbfx { cond; rd; rn; lsb; width } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Sbfx)
           [
             Operand.Reg rd;
             Operand.Reg rn;
             Operand.Imm (Bigint.of_int lsb);
             Operand.Imm (Bigint.of_int width);
           ])
  | Lowered.Bx { cond; rm } -> Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Bx) [ Operand.Reg rm ])
  | Lowered.Push { cond; regs } ->
      let members = List.filter (fun n -> regs land (1 lsl n) <> 0) (List.init 16 Fun.id) in
      Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Push) [ Operand.Reglist members ])
  | Lowered.Ldm { cond; load; rn; regs } ->
      let members = List.filter (fun n -> regs land (1 lsl n) <> 0) (List.init 16 Fun.id) in
      if load && Reg.equal rn Reg.sp && List.length members >= 2 then
        Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Pop) [ Operand.Reglist members ])
      else
        Some
          (Instruction.mk ~cond
             (op_of (Arm_ops.Enc.Ldm { load }))
             [ Operand.Reg_writeback rn; Operand.Reglist members ])
  | Lowered.Udf { imm16 } ->
      Some (Instruction.mk (op_of Arm_ops.Enc.Udf) [ Operand.Imm (Bigint.of_int64 imm16) ])
  | Lowered.Bl { cond; target } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Bl)
           [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
  | Lowered.B { cond; target } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.B)
           [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
  | Lowered.Mul { cond; rd; rn; rm; _ } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Mul)
           [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ])
  | Lowered.Mla { cond; rd; rn; rm; ra; _ } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Mla)
           [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ])
  | Lowered.Umull { cond; rdlo; rdhi; rn; rm; _ } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Umull)
           [ Operand.Reg rdlo; Operand.Reg rdhi; Operand.Reg rn; Operand.Reg rm ])
  | Lowered.Movw_movt { cond; top; rd; imm } ->
      (* Decoded, the immediate is the raw sixteen bits; symbolic, it is still
         the modifier expression. Either way the operand is an immediate, which
         is what re-parsing needs to see. *)
      let v =
        match imm with
        | Asm_core.Lowered_ast.Resolved { value; _ } -> Operand.Imm (Bigint.of_int64 value)
        | Asm_core.Lowered_ast.Symbolic { value; _ } -> Operand.Sym value
      in
      Some
        (Instruction.mk ~cond
           (op_of (if top then Arm_ops.Enc.Movt else Arm_ops.Enc.Movw))
           [ Operand.Reg rd; v ])
  | Lowered.Vmov_reg_d { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmov { dbl = true }))
           [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vmov_reg_s { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmov { dbl = false }))
           [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vmov_imm_d { cond; vd; imm8 } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmov { dbl = true }))
           [ Operand.Dreg vd; Operand.FImm (vfp_expand imm8) ])
  | Lowered.Vmov_imm_s { cond; vd; imm8 } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmov { dbl = false }))
           [ Operand.Sreg vd; Operand.FImm (vfp_expand imm8) ])
  | Lowered.Vmov_to_single { cond; sn; rt } ->
      Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Vmov_core) [ Operand.Sreg sn; Operand.Reg rt ])
  | Lowered.Vmov_from_single { cond; rt; sn } ->
      Some (Instruction.mk ~cond (op_of Arm_ops.Enc.Vmov_core) [ Operand.Reg rt; Operand.Sreg sn ])
  | Lowered.Vmov_to_double { cond; dm; rt; rt2 } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Vmov_core)
           [ Operand.Dreg dm; Operand.Reg rt; Operand.Reg rt2 ])
  | Lowered.Vmov_from_double { cond; rt; rt2; dm } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Vmov_core)
           [ Operand.Reg rt; Operand.Reg rt2; Operand.Dreg dm ])
  | Lowered.V3_d { cond; op; vd; vn; vm } ->
      Some
        (Instruction.mk ~cond (Opcode.V3d op) [ Operand.Dreg vd; Operand.Dreg vn; Operand.Dreg vm ])
  | Lowered.V3_s { cond; op; vd; vn; vm } ->
      Some
        (Instruction.mk ~cond (Opcode.V3s op) [ Operand.Sreg vd; Operand.Sreg vn; Operand.Sreg vm ])
  | Lowered.Vneg_d { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vneg { dbl = true }))
           [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vneg_s { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vneg { dbl = false }))
           [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vcmp_reg_d { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcmp { dbl = true }))
           [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vcmp_reg_s { cond; vd; vm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcmp { dbl = false }))
           [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vcmp_zero_d { cond; vd } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcmp { dbl = true }))
           [ Operand.Dreg vd; Operand.Imm Bigint.zero ])
  | Lowered.Vcmp_zero_s { cond; vd } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcmp { dbl = false }))
           [ Operand.Sreg vd; Operand.Imm Bigint.zero ])
  | Lowered.Vmrs { cond } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Vmrs)
           [
             Operand.Sym (Asm_core.Expr.Symbol "APSR_nzcv");
             Operand.Sym (Asm_core.Expr.Symbol "FPSCR");
           ])
  | Lowered.Vcvt_f32_f64 { cond; sd; dm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.F32_f64))
           [ Operand.Sreg sd; Operand.Dreg dm ])
  | Lowered.Vcvt_f64_f32 { cond; dd; sm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.F64_f32))
           [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_f32_s32 { cond; sd; sm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.F32_s32))
           [ Operand.Sreg sd; Operand.Sreg sm ])
  | Lowered.Vcvt_f64_s32 { cond; dd; sm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.F64_s32))
           [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_f64_u32 { cond; dd; sm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.F64_u32))
           [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_s32_f64 { cond; sd; dm } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vcvt Arm_ops.Enc.S32_f64))
           [ Operand.Sreg sd; Operand.Dreg dm ])
  | Lowered.Vmem_d { cond; load; vd; rn; offset } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmem { load }))
           [
             Operand.Dreg vd;
             Operand.Mem { base = rn; offset = Mem.Imm offset; writeback = false; pre = true };
           ])
  | Lowered.Vmem_s { cond; load; vd; rn; offset } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmem { load }))
           [
             Operand.Sreg vd;
             Operand.Mem { base = rn; offset = Mem.Imm offset; writeback = false; pre = true };
           ])
  | Lowered.Vldr_lit_d { cond; vd; target } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmem { load = true }))
           [
             Operand.Dreg vd;
             Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target)));
           ])
  | Lowered.Vldr_lit_s { cond; vd; target } ->
      Some
        (Instruction.mk ~cond
           (op_of (Arm_ops.Enc.Vmem { load = true }))
           [
             Operand.Sreg vd;
             Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target)));
           ])
  | Lowered.Vpush { cond; vd; count } ->
      Some
        (Instruction.mk ~cond (op_of Arm_ops.Enc.Vpush)
           [ Operand.Dreglist (List.init count (fun i -> vd.Dreg.num + i)) ])

let decode ctx bytes ~pos =
  if String.length bytes - pos < 4 then Error (diag ~pos:__POS__ `Decode_short)
  else
    let word = word_to_memory (String.sub bytes pos 4) in
    match C.decode_bits codec (C.Bits.of_bytes word) with
    | None -> Error (diag ~pos:__POS__ `Decode_no_match)
    | Some d -> (
        match instruction_of_lowered ~at:ctx.address d.C.value with
        | None -> Error (diag ~pos:__POS__ `Decode_no_normalized)
        | Some i -> Ok (i, String.concat "." d.C.dform, 4))

(* {1 Fixups, padding, directives} *)

let evaluate_fixup kind ~place ~target =
  (* [place] already carries the A32 pipeline offset: the caller adds the
     fixup's [pc_bias], which is 8 here. Adding it again - as this did while it
     was the only caller's convention - would double it now that x86 needs a
     per-form bias and the bias therefore lives in the fixup. *)
  match kind with
  | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)
  | Pcrel_b26 | Pcrel_call ->
      let d = Int64.sub target place in
      if Int64.rem d 4L <> 0L then Error (diag ~pos:__POS__ `Branch_not_word_aligned)
      else
        let words = Int64.div d 4L in
        (* +-32MB. Checked here rather than left to field truncation: a branch
           that does not reach is a diagnostic, not a wrong address. *)
        if Int64.compare words (-0x800000L) >= 0 && Int64.compare words 0x800000L < 0 then Ok words
        else Error (diag ~pos:__POS__ `Branch_out_of_range)
  | Movw_abs_nc -> Ok (Int64.logand target 0xFFFFL)
  | Movt_abs -> Ok (Int64.logand (Int64.shift_right_logical target 16) 0xFFFFL)
  (* [vldr]'s literal form always adds ([U] is fixed to 1 in the codec, not a
     field this fixup carries) - every corpus occurrence is a trailing
     same-function literal pool, ahead of the instruction, so a backward
     target is a scope decision, not a truncation. *)
  | Pcrel_vldr8 ->
      let d = Int64.sub target place in
      if Int64.compare d 0L < 0 then Error (diag ~pos:__POS__ `Vldr_literal_backward)
      else if Int64.rem d 4L <> 0L then Error (diag ~pos:__POS__ `Vldr_literal_not_word_aligned)
      else
        let imm8 = Int64.div d 4L in
        if Int64.compare imm8 256L < 0 then Ok imm8
        else Error (diag ~pos:__POS__ `Vldr_literal_out_of_range)

(* A32 has an architectural no-op, so padding an executable section needs no
   table and no policy. A boundary that is not a multiple of four is rejected
   rather than padded with bytes, because a partial instruction is not a no-op. *)
(* On ARM [.word] is four bytes, not two, and the compiler writes [.4byte] for a
   32-bit address - both spellings mean the same width here. *)
let data_widths =
  [
    (".byte", 1);
    (".short", 2);
    (".hword", 2);
    (".word", 4);
    (".4byte", 4);
    (".long", 4);
    (".quad", 8);
  ]

let data_fixup ~width =
  match width with 4 -> Ok Abs32 | _ -> Error (diag ~pos:__POS__ (`No_data_relocation width))

let nop_bytes ~length =
  if length mod 4 <> 0 then Error (diag ~pos:__POS__ `Padding_not_word_multiple)
  else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x00\xf0\x20\xe3")))

(* Measured (M3 §3/§5, docs/design.md §12): a linker-inserted merge gap in an
   executable section is plain zero fill on ARM, not NOP fill. *)
let merge_fill = None

(* Measured: ARM GAS records a section's alignment without rounding its size
   up to it. *)
let pad_section_to_alignment = false

let decode_candidates ctx bytes ~pos =
  match decode ctx bytes ~pos with Ok r -> [ r ] | Error _ -> []
