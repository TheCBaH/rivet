open Foundation
module C = Codec

(* Re-exported: the library's main module hides its siblings otherwise. *)
module Riscv_ext_m = Riscv_ext_m
module Riscv_csr = Riscv_csr
module Riscv_table_row = Riscv_table_row
module Riscv_table_rows = Riscv_table_rows
module Riscv_overlap = Riscv_overlap
module Riscv_extensions = Riscv_extensions
module Riscv_isa = Riscv_isa
module Riscv_types = Riscv_types

module type PROFILE = sig
  val name : string
  val triple : string
  val xlen : int
end

module type EXTENSIONS = sig
  val definition : Riscv_extensions.Validated.t
end

module Make_with (P : PROFILE) (E : EXTENSIONS) = struct
  open Asm_core.Lowered_ast

  let () =
    if Riscv_extensions.Validated.xlen E.definition <> P.xlen then
      invalid_arg
        (Printf.sprintf "%s: extensions validated for XLEN %d, profile is XLEN %d" P.name
           (Riscv_extensions.Validated.xlen E.definition)
           P.xlen)

  let table_rows = Riscv_extensions.Validated.rows E.definition
  let name = P.name
  let triple = P.triple
  let xlen = P.xlen

  module Reg = Riscv_types.Reg
  module Mem = Riscv_types.Mem
  module Operand = Riscv_types.Operand
  module Rm = Riscv_types.Rm
  module Xreg = Riscv_types.Xreg
  module Freg = Riscv_types.Freg
  module Vreg = Riscv_types.Vreg
  module Creg = Riscv_types.Creg
  module Cfreg = Riscv_types.Cfreg

  module Opcode = struct
    include Riscv_types.Opcode

    let name = function
      | Table i -> table_rows.(i).Riscv_table_row.mnemonic
      | op -> Riscv_types.Opcode.mnemonic op

    (* rv_v_aliases: riscv-opcodes' own deprecated pseudo-op spellings for
       already-admitted rv_v mnemonics. Each one's mask/value is identical to
       the canonical mnemonic it specializes (hand-verified against real GNU
       as: riscv32-linux-gnu-as/riscv64-linux-gnu-as 2.43.1/2.44 both accept
       every deprecated spelling under the same [-march=rv32imv]/[-march=
       rv64imv] the canonical spelling needs, producing byte-identical
       output), so resolving the deprecated spelling to the canonical
       opcode here - rather than a distinct [Opcode.t] variant - is exact,
       not an approximation; {!name}'s decode direction still only ever
       renders the canonical spelling, matching real GNU as's own
       disassembly. *)
    let deprecated_mnemonic_aliases =
      [
        ("vpopc.m", "vcpop.m");
        ("scall", "ecall");
        ("sbreak", "ebreak");
        ("vmandnot.mm", "vmandn.mm");
        ("vmornot.mm", "vmorn.mm");
        ("vfredsum.vs", "vfredusum.vs");
        ("vfwredsum.vs", "vfwredusum.vs");
        ("vl1r.v", "vl1re8.v");
        ("vl2r.v", "vl2re8.v");
        ("vl4r.v", "vl4re8.v");
        ("vl8r.v", "vl8re8.v");
        ("vle1.v", "vlm.v");
        ("vse1.v", "vsm.v");
      ]

    (* The first generated row spelled [s] for this XLEN; lowering tries every
       row with that spelling, so the index only names the mnemonic. *)
    let table_index s =
      let rows = table_rows in
      let rec go i =
        if i >= Array.length rows then None
        else
          let r = rows.(i) in
          if String.equal r.Riscv_table_row.mnemonic s && (r.xlen = 0 || r.xlen = xlen) then
            Some (Table i)
          else go (i + 1)
      in
      go 0

    let of_mnemonic s =
      let s = Option.value (List.assoc_opt s deprecated_mnemonic_aliases) ~default:s in
      match List.find_opt (fun op -> String.equal (name op) s) all with
      | Some _ as op -> op
      | None -> table_index s
  end

  open Opcode

  module Surface = struct
    type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

    let pp ppf s =
      if s.ops = [] then Fmt.string ppf s.mnemonic
      else Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) s.ops
  end

  module Instruction = struct
    type t = { op : Opcode.t; ops : Operand.t list }

    let mk op ops = { op; ops }

    (* Where a Zicsr form's CSR is; a number there prints in hex, as GNU's does. *)
    let csr_position = function
      | Opcode.Csrw | Csrs | Csrc | Csrwi | Csrsi | Csrci -> Some 0
      | Csrrw | Csrrs | Csrrc | Csrrwi | Csrrsi | Csrrci | Csrr -> Some 1
      | _ -> None

    let pp ppf i =
      let csr = csr_position i.op in
      let operand ppf (k, o) =
        match o with
        | Operand.Imm n when csr = Some k -> Bigint.pp_hex ppf n
        | o -> Operand.pp ppf o
      in
      if i.ops = [] then Fmt.string ppf (Opcode.name i.op)
      else
        Fmt.pf ppf "%s %a" (Opcode.name i.op)
          Fmt.(list ~sep:(any ", ") operand)
          (List.mapi (fun k o -> (k, o)) i.ops)
  end

  (* Which of an F/D instruction's [R]-shape operand positions is a scalar FP register
     rather than a GPR, and how many of the three fields printed. The generic [R]/[I]/[S]
     shapes carry plain register numbers with no class tag - correct for encoding (an FP
     register field is bit-for-bit the same 5-bit slot a GPR one is), but [pp] and [decode]
     both need to know which is which to print/reconstruct the right operand text, and this
     one table is what both consult rather than duplicating the classification. [arity] = 2
     covers the pseudo-mnemonics ([fneg.*]/[fmv.d], real hardware's [fsgnjn]/[fsgnj] with
     [rs2] forced equal to [rs1] - the shared register is stored once and never printed
     twice) and every convert/move between one FP and one GPR; [arity] = 3 covers the
     three-FP-register arithmetic family and the FP-operand compares (whose own result is a
     GPR).

     [rm] is [Some default_funct3] exactly when this mnemonic's own [funct3] field is a
     rounding mode rather than a fixed part of the encoding (real hardware defines a
     five-name enumeration - [rne]/[rtz]/[rdn]/[rup]/[rmm] - plus [dyn] for "read it from
     the fcsr", GAS's own default whenever none is written); [default_funct3] is exactly
     the value GAS's bare mnemonic - no explicit rounding-mode operand - picks, per
     mnemonic (almost always [dyn] = [7], except the two conversions real hardware defines
     as always exact, which pick [rne] = [0] instead). A decoded [funct3] equal to the
     default prints no trailing operand at all; any other value round-trips through an
     explicit one (M5 - perlin.c/binarytrees.c's own [(int)]/[(long)]
     cast idiom, which truncates and so must override the default with [rtz], not read
     it). [feq.d]/[fle.d]/[flt.d]/[flt.s]'s own [funct3] is a real comparison-kind selector,
     not a rounding mode, so [rm] is [None] there even though the field sits at the same
     bit position. *)
  type freg_shape = { rd_f : bool; rs1_f : bool; rs2_f : bool; arity : int; rm : Rm.t option }

  let f_shape : Opcode.t -> freg_shape option = function
    | Fadd_d | Fadd_s | Fsub_d | Fsub_s | Fmul_d | Fmul_s | Fdiv_d | Fdiv_s ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 3; rm = Some Rm.Dyn }
    | Fneg_d | Fneg_s | Fmv_d ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 2; rm = None }
    | Fsgnj_s | Fsgnj_d | Fsgnjn_s | Fsgnjn_d | Fsgnjx_s | Fsgnjx_d | Fmin_s | Fmax_s | Fmin_d
    | Fmax_d ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 3; rm = None }
    | Feq_d | Fle_d | Flt_d | Flt_s | Feq_s | Fle_s ->
        Some { rd_f = false; rs1_f = true; rs2_f = true; arity = 3; rm = None }
    | Fcvt_w_d | Fcvt_wu_d | Fcvt_l_d | Fcvt_lu_d | Fcvt_w_s | Fcvt_wu_s | Fcvt_l_s | Fcvt_lu_s ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = Some Rm.Dyn }
    | Fmv_x_d | Fmv_x_w -> Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = None }
    | Fmv_w_x -> Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = None }
    | Fclass_s | Fclass_d ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = None }
    | Fsqrt_s | Fsqrt_d ->
        Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some Rm.Dyn }
    | Fcvt_d_w | Fcvt_d_wu ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some Rm.Rne }
    | Fcvt_s_w | Fcvt_s_wu | Fcvt_s_l | Fcvt_s_lu | Fcvt_d_l | Fcvt_d_lu ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some Rm.Dyn }
    | Fcvt_s_d -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some Rm.Dyn }
    | Fcvt_d_s -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some Rm.Rne }
    | _ -> None

  let fence_set_name v =
    String.concat "" (List.filteri (fun j _ -> v land (1 lsl (3 - j)) <> 0) [ "i"; "o"; "r"; "w" ])

  module Lowered = struct
    (* The second instruction of an [auipc]-based pair: [Addi] for [la]/[lla]
       (pic), [Jalr] for [call]/[tail], [Load funct3] for a load pseudo like
       [ld rd, symbol] (GAS's own auipc+load expansion, confirmed against real
       riscv64-linux-gnu-as: `ld t6, sym` decodes back as
       `auipc t6, ...; ld t6, 0(t6)`). *)
    type pair_kind =
      | Addi
      | Jalr
      | Load of int
      | Fload of int
          (** [fld]/[flw]'s own literal-pool pseudo (M5 - almabench.c/
            fftsp.c/knucleotide.c/...): the same anchored [auipc]+load pairing as [Load],
            but the scratch register can never be [rd] itself the way [ld rd, symbol]
            reuses [rd] as its own [auipc] target - [rd] is a scalar FP register here, and
            [auipc] can only write a GPR. GAS's own three-operand pseudo spells the GPR
            scratch out explicitly (`fld rd, symbol, xtmp`) for exactly that reason, so
            unlike every other {!Pair} kind, [tmp] is a real, distinct, always-printed
            operand rather than a repeat of [rd]. Confirmed against real
            riscv64-linux-gnu-as: `fld fa1, .L100, x31` expands to `auipc t6,
            %pcrel_hi(.L100); fld fa1, %pcrel_lo(...)(t6)`. *)

    type t =
      | R of { op : Opcode.t; rd : Reg.t; rs1 : Reg.t; rs2 : Reg.t option; rm : Rm.t option }
          (** [rs2] is [None] where the field holds a fixed selector rather than a register;
              [rm] is the rounding mode of the forms that have one. The fixed fields come from
              {!Desc.r}. *)
      | Rbs of { op : Opcode.t; rd : Reg.t; rs1 : Reg.t; rs2 : Reg.t; bs : int }
          (** AES-32 and SM4's byte select, the top two bits of [funct7]. *)
      | R4 of { op : Opcode.t; rd : Reg.t; rs1 : Reg.t; rs2 : Reg.t; rs3 : Reg.t; rm : Rm.t }
          (** [fmadd]/[fmsub]/[fnmsub]/[fnmadd] - RISC-V's only R4-type instructions,
              with a fourth real register operand ([rs3]) {!R} has no field for; see
              {!f_fma_desc}. *)
      | I of { op : Opcode.t; rd : Reg.t; rs1 : Reg.t; imm : Asm_core.Expr.t }
      | S of { op : Opcode.t; rs1 : Reg.t; rs2 : Reg.t; imm : Asm_core.Expr.t }
      | B of { op : Opcode.t; rs1 : Xreg.t; rs2 : Xreg.t; target : Asm_core.Expr.t }
      | U of { op : Opcode.t; rd : Xreg.t; imm : Asm_core.Expr.t }
      | J of { rd : Xreg.t; target : Asm_core.Expr.t }
      | Pair of {
          op : Opcode.t;
          rd : Reg.t;
          tmp : Xreg.t;
          target : Asm_core.Expr.t;
          kind : pair_kind;
        }
      | Fixed of { op : Opcode.t; word : int64 }
      | Fixed_half of { op : Opcode.t; word : int64 }
          (** a 16-bit compressed word with no fixup: a generated table row *)
      | Caddi of { rd : Xreg.t; imm : Asm_core.Expr.t }
      | Cr2 of { op : Opcode.t; rd_rs1 : Creg.t; rs2 : Creg.t }
          (** the CA-format compressed register-register ops ([c.and]/[c.or]/[c.xor]/[c.sub]/
              [c.addw]/[c.subw]): [rd_rs1]/[rs2] are the registers (x8..x15); {!Creg.field} gives
              the 3-bit compressed field. *)
      | Cr of { op : Opcode.t; rd_rs1 : Xreg.t; rs2 : Xreg.t }
          (** the CR-format compressed register-register cluster ([c.jr]/[c.jalr]/[c.mv]/
              [c.add]/[c.ebreak]): unlike {!Cr2}, [rd_rs1]/[rs2] range over the full 0..31 GPR
              space (no compressed-subset restriction, no -8 bias) - [c.jr]/[c.jalr] set
              [rs2 = 0], [c.ebreak] sets both fields to 0 with [funct4 = 9]. *)
      | Cl of { op : Opcode.t; rd : Creg.t; rs1 : Creg.t; imm : Asm_core.Expr.t }
          (** CL-format compressed loads ([c.lw]/[c.ld]): [rd]/[rs1] are real register
              numbers (8..15) for the compressed 3-bit fields, like {!Cr2} - the packer
              subtracts 8. [c.lw]'s 5 raw offset bits scatter as [inst5|hi3|inst6] (RISC-V's
              own word-offset swap); [c.ld]'s instead concatenate straight as [lo2|hi3]
              (doubleword alignment leaves no swap to do) - {!word_c_lw}/{!word_c_ld} differ
              only in that shape, selected by [name] at the {!encode_ungated} call site. *)
      | Cs of { op : Opcode.t; rs1 : Creg.t; rs2 : Creg.t; imm : Asm_core.Expr.t }
          (** CS-format compressed stores ([c.sw]/[c.sd]): the store-side sibling of {!Cl}. *)
      | Clsp of { op : Opcode.t; rd : Xreg.t; imm : Asm_core.Expr.t }
          (** CI-format compressed SP-relative loads ([c.lwsp]/[c.ldsp]): the base register
              is architecturally fixed to [x2] (sp) - CI-format has no rs1 field at all, so
              only [rd] and [imm] are ever assembled/decoded; confirmed against real
              riscv64-linux-gnu-as ([c.lwsp a0,8(x1)] is rejected as illegal operands). *)
      | Css of { op : Opcode.t; rs2 : Xreg.t; imm : Asm_core.Expr.t }
          (** CSS-format compressed SP-relative stores ([c.swsp]/[c.sdsp]): the store-side
              sibling of {!Clsp}, same fixed-[x2]-base story. *)
      | Cj of { op : Opcode.t; target : Asm_core.Expr.t }
          (** CJ-format compressed jumps ([c.j], RV32's [c.jal]): a 12-bit signed, 2-byte-aligned
              PC-relative target resolved via a {!fixup_kind.Jump12c} fixup. *)
      | Cb of { op : Opcode.t; rs1 : Creg.t; target : Asm_core.Expr.t }
          (** CB-format compressed conditional branches ([c.beqz]/[c.bnez]): [rs1] is a real
              register number (8..15) for the compressed 3-bit field, like {!Cl}. [target] is a
              PC-relative branch target resolved via a {!fixup_kind.Branch9c} fixup, the
              compressed sibling of {!B}'s [Branch13] - a 9-bit signed, 2-byte-aligned offset
              instead of [beq]/[bne]'s 13-bit one. *)

    let pp_expr ppf e = Asm_core.Expr.pp ppf e

    let rm_suffix default rm =
      match (default, rm) with Some d, Some r when d <> r -> ", " ^ Rm.name r | _ -> ""

    (* A [fence] word names its predecessor and successor sets unless both are [iorw]. *)
    let fixed_name op word =
      match op with
      | Opcode.Fence ->
          let set shift = Int64.to_int (Int64.shift_right_logical word shift) land 0xf in
          let p = set 24 and q = set 20 in
          if p = 0xf && q = 0xf then "fence"
          else Printf.sprintf "fence %s,%s" (fence_set_name p) (fence_set_name q)
      | op -> Opcode.name op

    let pp ppf = function
      | R x -> (
          let name = Opcode.name x.op in
          let rm = Option.bind (f_shape x.op) (fun s -> s.rm) in
          match (f_shape x.op, x.rs2) with
          | Some { arity = 2; _ }, _ | _, None ->
              Fmt.pf ppf "%s %a, %a%s" name Reg.pp x.rd Reg.pp x.rs1 (rm_suffix rm x.rm)
          | _, Some rs2 ->
              Fmt.pf ppf "%s %a, %a, %a%s" name Reg.pp x.rd Reg.pp x.rs1 Reg.pp rs2
                (rm_suffix rm x.rm))
      | Rbs x ->
          Fmt.pf ppf "%s %a, %a, %a, %d" (Opcode.name x.op) Reg.pp x.rd Reg.pp x.rs1 Reg.pp x.rs2
            x.bs
      | R4 x ->
          Fmt.pf ppf "%s %a, %a, %a, %a%s" (Opcode.name x.op) Reg.pp x.rd Reg.pp x.rs1 Reg.pp x.rs2
            Reg.pp x.rs3
            (rm_suffix (Some Rm.Dyn) (Some x.rm))
      | I ({ op = Lb | Lh | Lw | Ld | Lbu | Lhu | Lwu | Flw | Fld | Jalr; _ } as x) ->
          Fmt.pf ppf "%s %a, %a(%a)" (Opcode.name x.op) Reg.pp x.rd pp_expr x.imm Reg.pp x.rs1
      | I x -> Fmt.pf ppf "%s %a, %a, %a" (Opcode.name x.op) Reg.pp x.rd Reg.pp x.rs1 pp_expr x.imm
      | S x -> Fmt.pf ppf "%s %a, %a(%a)" (Opcode.name x.op) Reg.pp x.rs2 pp_expr x.imm Reg.pp x.rs1
      | B x ->
          Fmt.pf ppf "%s %a, %a, %a" (Opcode.name x.op) Xreg.pp x.rs1 Xreg.pp x.rs2 pp_expr x.target
      | U x -> Fmt.pf ppf "%s %a, %a" (Opcode.name x.op) Xreg.pp x.rd pp_expr x.imm
      | J x -> Fmt.pf ppf "jal %a, %a" Xreg.pp x.rd pp_expr x.target
      | Pair ({ kind = Fload _; _ } as x) ->
          Fmt.pf ppf "%s %a, %a, %a" (Opcode.name x.op) Reg.pp x.rd pp_expr x.target Xreg.pp x.tmp
      | Pair x -> Fmt.pf ppf "%s %a, %a" (Opcode.name x.op) Reg.pp x.rd pp_expr x.target
      | Fixed x -> Fmt.string ppf (fixed_name x.op x.word)
      | Fixed_half x -> Fmt.string ppf (Opcode.name x.op)
      | Caddi x -> Fmt.pf ppf "c.addi %a, %a" Xreg.pp x.rd pp_expr x.imm
      | Cr2 x -> Fmt.pf ppf "%s %a, %a" (Opcode.name x.op) Creg.pp x.rd_rs1 Creg.pp x.rs2
      | Cr ({ op = C_ebreak; _ } as x) -> Fmt.string ppf (Opcode.name x.op)
      | Cr ({ op = C_jr | C_jalr; _ } as x) ->
          Fmt.pf ppf "%s %a" (Opcode.name x.op) Xreg.pp x.rd_rs1
      | Cr x -> Fmt.pf ppf "%s %a, %a" (Opcode.name x.op) Xreg.pp x.rd_rs1 Xreg.pp x.rs2
      | Cl x ->
          Fmt.pf ppf "%s %a, %a(%a)" (Opcode.name x.op) Creg.pp x.rd pp_expr x.imm Creg.pp x.rs1
      | Cs x ->
          Fmt.pf ppf "%s %a, %a(%a)" (Opcode.name x.op) Creg.pp x.rs2 pp_expr x.imm Creg.pp x.rs1
      | Clsp x -> Fmt.pf ppf "%s %a, %a(x2)" (Opcode.name x.op) Xreg.pp x.rd pp_expr x.imm
      | Css x -> Fmt.pf ppf "%s %a, %a(x2)" (Opcode.name x.op) Xreg.pp x.rs2 pp_expr x.imm
      | Cb x -> Fmt.pf ppf "%s %a, %a" (Opcode.name x.op) Creg.pp x.rs1 pp_expr x.target
      | Cj x -> Fmt.pf ppf "%s %a" (Opcode.name x.op) pp_expr x.target

    let equal (a : t) b = a = b
  end

  type fixup_kind =
    | Abs32
    | Abs64
    | Branch13
    | Branch9c
    | Jump12c
    | Jal21
    | Pcrel_hi20
    | Pcrel_lo12_i
    | Pcrel_lo12_s
    | Call_hi20
    | Call_lo12_i
    | Abs_hi20
    | Abs_lo12_i
    | Abs_lo12_s

  let fixup_kind_name = function
    | Abs32 -> "abs32"
    | Abs64 -> "abs64"
    | Branch13 -> "pcrel-b13"
    | Branch9c -> "pcrel-b9c"
    | Jump12c -> "pcrel-j12c"
    | Jal21 -> "pcrel-j21"
    | Pcrel_hi20 -> "pcrel-hi20"
    | Pcrel_lo12_i -> "pcrel-lo12-i"
    | Pcrel_lo12_s -> "pcrel-lo12-s"
    | Call_hi20 -> "call-hi20"
    | Call_lo12_i -> "call-lo12-i"
    | Abs_hi20 -> "abs-hi20"
    | Abs_lo12_i -> "abs-lo12-i"
    | Abs_lo12_s -> "abs-lo12-s"

  let equal_fixup_kind a b = a = b

  let fixup_family = function
    | Abs32 -> "abs32"
    | Abs64 -> "abs64"
    | Branch13 | Branch9c | Jump12c | Jal21 -> "pcrel-branch"
    | Pcrel_hi20 | Pcrel_lo12_i | Pcrel_lo12_s -> "pcrel-address"
    | Call_hi20 | Call_lo12_i -> "pcrel-call"
    | Abs_hi20 | Abs_lo12_i | Abs_lo12_s -> "absolute-address"

  let fixup_role = function
    | Branch13 | Branch9c | Jump12c | Jal21 -> Asm_core.Lowered_ast.Branch
    | Call_hi20 | Call_lo12_i -> Asm_core.Lowered_ast.Call
    | _ -> Asm_core.Lowered_ast.Data_address

  (* The separable instruction components this family publishes (one per gating feature). The
     rest of the family is the always-available base. *)
  let components = Riscv_extensions.Validated.components E.definition

  (* [config] is the validated feature set. It lives in the state, not beside it, so it rides
     everywhere the state already goes: each unit starts from the caller's initial state, and a
     lowered fragment replays under the state it was written in. [.option arch] changes it, so
     [option_stack] saves it with the flags. *)
  type target_state = {
    pic : bool;
    relax : bool;
    rvc : bool;
    option_stack : (bool * bool * bool * Target_config.t) list;
    config : Target_config.t;
  }

  let default_config = Target_config.default components

  let default_state =
    { pic = false; relax = false; rvc = false; option_stack = []; config = default_config }

  let initial_state config = { default_state with config }
  let state_config s = s.config

  type error_kind =
    [ Target_error.shared
    | `Wrong_operands of string
    | `Rv64_only of string
    | `Rv32_only of string
    | `Immediate_range of string
    | `Immediate_alignment of string
    | `Bad_modifier of string
    | `Compressed_disabled of string
    | `Format_mismatch of Opcode.t * string
    | `Decode_short
    | `Decode_length
    | `Decode_no_match
    | `No_data_relocation of int
    | `Padding_not_word_multiple ]

  type error = error_kind Target_error.t

  let pp_error_kind ppf : error_kind -> unit = function
    | #Target_error.shared as e -> Target_error.pp_shared ppf e
    | `Wrong_operands op -> Fmt.pf ppf "no %s form takes these operands" op
    | `Rv64_only op -> Fmt.pf ppf "%s is available only when XLEN is 64" op
    | `Rv32_only op -> Fmt.pf ppf "%s is available only when XLEN is 32" op
    | `Immediate_range what -> Fmt.pf ppf "%s immediate is out of range" what
    | `Immediate_alignment what -> Fmt.pf ppf "%s target is not two-byte aligned" what
    | `Bad_modifier m -> Fmt.pf ppf "unsupported relocation modifier %s" m
    | `Compressed_disabled op -> Fmt.pf ppf "%s requires .option rvc" op
    | `Format_mismatch (op, format) ->
        Fmt.pf ppf "internal: %s has no %s-format encoding" (Opcode.name op) format
    | `Decode_short -> Fmt.string ppf "fewer than four bytes remain"
    | `Decode_length -> Fmt.string ppf "RISC-V instructions must be four bytes"
    | `Decode_no_match -> Fmt.string ppf "no form matches this word"
    | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte initializer" w
    | `Padding_not_word_multiple -> Fmt.string ppf "RISC-V padding must be a multiple of four bytes"

  let error_kind_code : error_kind -> string = function
    | `Unknown_instruction _ -> P.name ^ ".simplify"
    | `Decode_short | `Decode_length | `Decode_no_match | `Decode_no_normalized ->
        P.name ^ ".decode"
    | `No_data_relocation _ -> P.name ^ ".data-fixup"
    | `Padding_not_word_multiple -> P.name ^ ".nop"
    | `Feature_disabled _ -> P.name ^ ".feature"
    | `Compressed_disabled _ -> P.name ^ ".lower"
    | `Format_mismatch _ -> P.name ^ ".internal.format"
    | `Immediate_alignment _ | `Immediate_range _ -> P.name ^ ".fixup"
    | _ -> P.name ^ ".lower"

  let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
  let error_code e = error_kind_code (Target_error.kind e)
  let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

  let diag ?pos ?origin kind =
    Err.Error.make ?pos ~pp_error
      (Target_error.make
         ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:P.name ())
         kind)

  let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

  (* The one place a form's component is checked against the configuration. Every stage that can
     see an opcode goes through it, so a disabled form is refused whichever path reached it. *)
  let legal_of_mnemonic = Target_component.legal components

  (* Each opcode's requirement, looked up once from the components that publish its spelling. *)
  let legal_of_op =
    let hand = Hashtbl.create 512 in
    List.iter (fun op -> Hashtbl.replace hand op (legal_of_mnemonic (Opcode.name op))) Opcode.all;
    let rows = Array.map (fun r -> legal_of_mnemonic r.Riscv_table_row.mnemonic) table_rows in
    function Opcode.Table i -> rows.(i) | op -> Option.join (Hashtbl.find_opt hand op)

  let feature_gate ?origin state op =
    match legal_of_op op with
    | None -> Ok ()
    | Some req -> (
        match Target_component.Req.unmet (Target_config.enabled state.config) req with
        | None -> Ok ()
        | Some unmet ->
            Error
              (diag ~pos:__POS__ ?origin
                 (`Feature_disabled (Opcode.name op, Fmt.str "%a" Target_component.Req.pp unmet))))

  let required (i : Instruction.t) = legal_of_op i.op

  let simplify_instruction state s =
    match Opcode.of_mnemonic s.Surface.mnemonic with
    | None -> Error (diag ~pos:__POS__ ~origin:s.origin (`Unknown_instruction s.mnemonic))
    | Some op -> (
        match feature_gate ~origin:s.origin state op with
        | Error _ as e -> e
        | Ok () -> Ok { Instruction.op; ops = s.ops })

  let const n = Asm_core.Expr.Const (Bigint.of_int n)

  let expr_of = function
    | Operand.Imm n -> Some (Asm_core.Expr.Const n)
    | Operand.Sym e -> Some e
    | _ -> None

  let xreg = function Operand.Reg (Reg.X r) -> Some r | _ -> None
  let freg = function Operand.Reg (Reg.F r) -> Some r | _ -> None
  let vreg = function Operand.Reg (Reg.V r) -> Some r | _ -> None

  (* The same operands as the class-tagged registers {!Lowered}'s R/I/S forms hold. *)
  let gx op = Option.map (fun r -> Reg.X r) (xreg op)
  let gf op = Option.map (fun r -> Reg.F r) (freg op)
  let creg op = Option.bind (xreg op) Creg.of_xreg
  let base (m : Mem.t) = Reg.to_x m.base
  let gbase m = Option.map (fun r -> Reg.X r) (base m)
  let cbase m = Option.bind (base m) Creg.of_xreg

  (* Register numbers, for generated rows, which place fields by number. *)
  let xnum op = Option.map Xreg.to_int (xreg op)
  let fnum op = Option.map Freg.to_int (freg op)
  let vnum op = Option.map Vreg.to_int (vreg op)
  let base_num m = Option.map Xreg.to_int (base m)

  (* V's masked-operand marker: a bare trailing [v0.t] (never a real register
     by itself - [Reg.find] never returns a [V] for a name with a [.t] suffix,
     so it falls through {!parse_one}'s generic bare-identifier case to
     [Operand.Sym (Symbol "v0.t")] with no frontend change needed, the same
     way vsetvli/vsetivli's keyword tokens do). *)
  let is_v0t = function Operand.Sym (Asm_core.Expr.Symbol "v0.t") -> true | _ -> false
  let rv64 op = if xlen = 64 then Ok () else Error (diag ~pos:__POS__ (`Rv64_only op))
  let wrong op = Error (diag ~pos:__POS__ (`Wrong_operands op))

  (* The M component's forms come from {!Riscv_ext_m}: one table owns the encoding
     fields, the RV64-only gate and the descriptor, so they cannot drift apart. *)
  let m_desc op =
    Option.map (fun (f : Riscv_ext_m.form) -> (f.opcode, f.funct3, f.funct7)) (Riscv_ext_m.find op)

  let m_rv64_only op = match Riscv_ext_m.find op with Some f -> f.rv64_only | None -> false

  (* CA-format compressed register-register ops (quadrant 1, funct3=100):
     funct6 selects word- vs *w-width, funct2 selects the operation.
     Hand-verified against real riscv64-linux-gnu-as/riscv32-linux-gnu-as
     2.44/2.43.1 (`c.and a0,a1` -> 8d6d, `c.addw a0,a1` -> 9d2d). *)
  let ca_desc = function
    | Opcode.C_sub -> Some (0x23, 0)
    | C_xor -> Some (0x23, 1)
    | C_or -> Some (0x23, 2)
    | C_and -> Some (0x23, 3)
    | C_subw -> Some (0x27, 0)
    | C_addw -> Some (0x27, 1)
    | _ -> None

  let r_desc = function
    | Opcode.Add -> Some (0x33, 0, 0x00)
    | Sub -> Some (0x33, 0, 0x20)
    | Sll -> Some (0x33, 1, 0x00)
    | Slt -> Some (0x33, 2, 0x00)
    | Sltu -> Some (0x33, 3, 0x00)
    | Xor -> Some (0x33, 4, 0x00)
    | Srl -> Some (0x33, 5, 0x00)
    | Sra -> Some (0x33, 5, 0x20)
    | Or -> Some (0x33, 6, 0x00)
    | And -> Some (0x33, 7, 0x00)
    | (Mul | Remu | Mulh | Mulhsu | Mulhu | Div | Divu | Rem | Mulw | Divw | Divuw | Remw | Remuw)
      as op ->
        m_desc op
    | Sh1add -> Some (0x33, 2, 0x10)
    | Sh2add -> Some (0x33, 4, 0x10)
    | Sh3add -> Some (0x33, 6, 0x10)
    | Min -> Some (0x33, 4, 0x05)
    | Minu -> Some (0x33, 5, 0x05)
    | Max -> Some (0x33, 6, 0x05)
    | Maxu -> Some (0x33, 7, 0x05)
    | Andn -> Some (0x33, 7, 0x20)
    | Orn -> Some (0x33, 6, 0x20)
    | Xnor -> Some (0x33, 4, 0x20)
    | Rol -> Some (0x33, 1, 0x30)
    | Ror -> Some (0x33, 5, 0x30)
    | Bclr -> Some (0x33, 1, 0x24)
    | Bext -> Some (0x33, 5, 0x24)
    | Binv -> Some (0x33, 1, 0x34)
    | Bset -> Some (0x33, 1, 0x14)
    | Clmul -> Some (0x33, 1, 0x05)
    | Clmulh -> Some (0x33, 3, 0x05)
    | Clmulr -> Some (0x33, 2, 0x05)
    | Czero_eqz -> Some (0x33, 5, 0x07)
    | Czero_nez -> Some (0x33, 7, 0x07)
    | Xperm4 -> Some (0x33, 2, 0x14)
    | Xperm8 -> Some (0x33, 4, 0x14)
    (* SHA-512's RV32-only 32-bit-word-pair-split helpers (gated below;
       riscv-opcodes has no RV64 record at all for these six - RV64 instead
       gets the plain, non-split {!Sha512sum0}/etc. two-GPR unary family
       above). *)
    | Sha512sum0r -> Some (0x33, 0, 0x28)
    | Sha512sum1r -> Some (0x33, 0, 0x29)
    | Sha512sig0l -> Some (0x33, 0, 0x2a)
    | Sha512sig1l -> Some (0x33, 0, 0x2b)
    | Sha512sig0h -> Some (0x33, 0, 0x2e)
    | Sha512sig1h -> Some (0x33, 0, 0x2f)
    (* AES-64's plain three-GPR round functions (aes64im's two-GPR unary
       sibling is in unary_imm_desc below, not here). *)
    | Aes64ds -> Some (0x33, 0, 0x1d)
    | Aes64dsm -> Some (0x33, 0, 0x1f)
    | Aes64es -> Some (0x33, 0, 0x19)
    | Aes64esm -> Some (0x33, 0, 0x1b)
    | Aes64ks2 -> Some (0x33, 0, 0x3f)
    | Sh1adduw -> Some (0x3b, 2, 0x10)
    | Sh2adduw -> Some (0x3b, 4, 0x10)
    | Sh3adduw -> Some (0x3b, 6, 0x10)
    | Addw -> Some (0x3b, 0, 0x00)
    | Subw -> Some (0x3b, 0, 0x20)
    | Sllw -> Some (0x3b, 1, 0x00)
    | Srlw -> Some (0x3b, 5, 0x00)
    | Sraw -> Some (0x3b, 5, 0x20)
    | Pack -> Some (0x33, 4, 0x04)
    | Packh -> Some (0x33, 7, 0x04)
    | Packw -> Some (0x3b, 4, 0x04)
    | Rolw -> Some (0x3b, 1, 0x30)
    | Rorw -> Some (0x3b, 5, 0x30)
    | _ -> None

  (* Zaamo's [amoOP rd, rs2, (rs1)] / [scOP rd, rs2, (rs1)] three-GPR-plus-
     memory shape (opcode 0x2f, aq/rl bits zeroed - GAS's own bare-mnemonic
     canonical spelling; the `.aq`/`.rl`/`.aqrl` suffix decorators GAS also
     accepts are not modeled here since they add a textual decorator on top
     of these same 22 records, not a new source record - a
     canonical-spelling-first policy). funct7's low 2 bits are aq/rl (both
     0), its high 5 bits are the funct5 that actually selects the operation;
     {!r_desc}'s reused [Lowered.R]/[word_r] path (already used for
     aes32dsi/etc.'s own composed funct7) needs no change. Values confirmed
     against real GNU as: `amoadd.w a0,a1,(a2)` -> `00b6252f`, `amoswap.w`
     -> `08b6252f` (funct5 0x01), `amoxor.w` -> `20b6252f` (0x04), `amoor.w`
     -> `40b6252f` (0x08), `amoand.w` -> `60b6252f` (0x0c), `amomin.w` ->
     `80b6252f` (0x10), `amomax.w` -> `a0b6252f` (0x14), `amominu.w` ->
     `c0b6252f` (0x18), `amomaxu.w` -> `e0b6252f` (0x1c), `sc.w` ->
     `18b6252f` (0x03); funct3 3 (not 2) for every `.d` sibling, e.g.
     `amoswap.d` -> `08b6352f`. *)
  let amo3_desc = function
    | Opcode.Amoadd_w -> Some (0x2f, 2, 0x00)
    | Amoswap_w -> Some (0x2f, 2, 0x04)
    | Amoxor_w -> Some (0x2f, 2, 0x10)
    | Amoor_w -> Some (0x2f, 2, 0x20)
    | Amoand_w -> Some (0x2f, 2, 0x30)
    | Amomin_w -> Some (0x2f, 2, 0x40)
    | Amomax_w -> Some (0x2f, 2, 0x50)
    | Amominu_w -> Some (0x2f, 2, 0x60)
    | Amomaxu_w -> Some (0x2f, 2, 0x70)
    | Sc_w -> Some (0x2f, 2, 0x0c)
    | Amoadd_d -> Some (0x2f, 3, 0x00)
    | Amoswap_d -> Some (0x2f, 3, 0x04)
    | Amoxor_d -> Some (0x2f, 3, 0x10)
    | Amoor_d -> Some (0x2f, 3, 0x20)
    | Amoand_d -> Some (0x2f, 3, 0x30)
    | Amomin_d -> Some (0x2f, 3, 0x40)
    | Amomax_d -> Some (0x2f, 3, 0x50)
    | Amominu_d -> Some (0x2f, 3, 0x60)
    | Amomaxu_d -> Some (0x2f, 3, 0x70)
    | Sc_d -> Some (0x2f, 3, 0x0c)
    | _ -> None

  (* [lr.w/lr.d rd, (rs1)] - the same opcode-0x2f family's only two-operand
     member (rs2's field is architecturally fixed to 0, never
     syntax-visible), funct5 0x02: `lr.w a0,(a2)` -> `1006252f`,
     `lr.d a0,(a2)` -> `1006352f`. *)
  let lr_desc = function
    | Opcode.Lr_w -> Some (0x2f, 2, 0x08)
    | Lr_d -> Some (0x2f, 3, 0x08)
    | _ -> None

  (* OP-IMM/OP-IMM-32 (opcodes 0x13/0x1b) "pseudo-unary" forms: Zbb's
     population-count/sign-extend/byte family (clz, ctz, cpop, sext.b,
     sext.h, orc.b and their RV64-only *w siblings). Unlike {!i_desc}, the
     remaining bits are a fully fixed funct12 that selects the mnemonic, not
     a genuine immediate operand - GAS syntax is "mnemonic rd, rs1" with no
     immediate written at all. Hand-verified against the checked-in
     riscv32.jsonl/riscv64.jsonl mask/value fields before writing this table
     (e.g. clz 0x60001013, orc.b 0x28705013), then confirmed against real
     riscv32-linux-gnu-as 2.43.1 / riscv64-linux-gnu-as 2.44. *)
  let unary_imm_desc = function
    | Opcode.Clz -> Some (0x13, 1, 0x600)
    | Ctz -> Some (0x13, 1, 0x601)
    | Cpop -> Some (0x13, 1, 0x602)
    | Sextb -> Some (0x13, 1, 0x604)
    | Sexth -> Some (0x13, 1, 0x605)
    | Orcb -> Some (0x13, 5, 0x287)
    | Clzw -> Some (0x1b, 1, 0x600)
    | Ctzw -> Some (0x1b, 1, 0x601)
    | Cpopw -> Some (0x1b, 1, 0x602)
    | Brev8 -> Some (0x13, 5, 0x687)
    (* [rev8] (byte-reverse) is the one form in this family whose funct12
       genuinely differs by XLEN - 0x698 on RV32, 0x6b8 on RV64 - rather than
       being a distinct RV64-only mnemonic (contrast {!Sh1adduw} etc). Real
       GNU as accepts the bare mnemonic ["rev8"] on BOTH profiles (confirmed:
       riscv32-linux-gnu-as REJECTS the source's own internal disambiguation
       label "rev8.rv32" as an unrecognized opcode; only ["rev8"] assembles).
       riscv-opcodes' own two native names for this - "rev8" (rv64_zbb
       import group) and "rev8.rv32" (rv32_zbb import group) - are therefore
       a source-internal table-key distinction, not two different
       user-facing mnemonics; {!Isa_norm_riscv} maps both to this one
       [Opcode.t] and rendered mnemonic. *)
    | Rev8 -> Some (0x13, 5, if xlen = 64 then 0x6b8 else 0x698)
    (* [zip]/[unzip] (Zbkb's RV32-only bit-interleave/de-interleave) - no
       riscv-opcodes RV64 counterpart at all (confirmed: real
       riscv64-linux-gnu-as rejects ["zip"] as an unrecognized opcode), so
       these need an explicit RV32-only gate below, the mirror image of
       every RV64-only sibling in this family. *)
    | Zip -> Some (0x13, 1, 0x08f)
    | Unzip -> Some (0x13, 5, 0x08f)
    (* Zknh's SHA-256 message-schedule helpers - the same two-GPR unary
       shape as clz/ctz/cpop above, XLEN-independent (identical mnemonic and
       encoding on both profiles), just a different funct12 per mnemonic.
       Hand-verified against the checked-in riscv32.jsonl/riscv64.jsonl
       mask/value fields (e.g. sha256sum0 0x10001013, sha256sig1
       0x10301013), then confirmed against real riscv32-linux-gnu-as
       2.43.1 / riscv64-linux-gnu-as 2.44. *)
    | Sha256sum0 -> Some (0x13, 1, 0x100)
    | Sha256sum1 -> Some (0x13, 1, 0x101)
    | Sha256sig0 -> Some (0x13, 1, 0x102)
    | Sha256sig1 -> Some (0x13, 1, 0x103)
    (* Zksh's SM3 message-schedule helpers - the same two-GPR unary shape,
       XLEN-independent, identical mnemonic/encoding on both profiles.
       Hand-verified against the checked-in riscv32.jsonl/riscv64.jsonl
       mask/value (sm3p0 0x10801013, sm3p1 0x10901013), then confirmed
       against real riscv32-linux-gnu-as 2.43.1 / riscv64-linux-gnu-as
       2.44. *)
    | Sm3p0 -> Some (0x13, 1, 0x108)
    | Sm3p1 -> Some (0x13, 1, 0x109)
    (* SHA-512's own message-schedule helpers - the same shape as SHA-256's
       above, RV64-only (gated below; riscv-opcodes has no RV32 record at
       all for these four - RV32 instead gets a genuinely different,
       32-bit-word-pair-split family, sha512sig0h/l etc., out of this
       slice's scope). *)
    | Sha512sum0 -> Some (0x13, 1, 0x104)
    | Sha512sum1 -> Some (0x13, 1, 0x105)
    | Sha512sig0 -> Some (0x13, 1, 0x106)
    | Sha512sig1 -> Some (0x13, 1, 0x107)
    (* AES-64's inverse-mix-columns helper - the same two-GPR unary shape as
       the SHA helpers above, RV64-only (gated below). *)
    | Aes64im -> Some (0x13, 1, 0x300)
    | _ -> None

  (* An R-type mnemonic whose third operand is an immediate is GAS's alias for
     the matching I-type form, wherever one exists - one rule, not a shift
     special case. Probed against riscv64-linux-gnu-as 2.44, `<op> t5, t5, -8`:
     and/or/xor/add/slt/sltu/addw all assemble as andi/ori/xori/addi/slti/
     sltiu/addiw, while sub/subw/mul are rejected, exactly the ops with no
     I-type counterpart to name here. The shifts are the same rule: `sll x5,
     x11, 2` decodes back as `slli t0, a1, 0x2`. `ror`/`rorw` follow suit
     (confirmed: `ror a0, a1, 5` assembles as `rori`) - but `rol`/`rolw` do
     NOT (confirmed: `rol a0, a1, 5` is rejected as "illegal operands"),
     since there is no `roli`/`roliw` - a left-rotate-by-immediate is
     redundant with `rori`'s own complementary shift amount, so Zbb never
     defined one. *)
  let imm_alias = function
    | Opcode.Add -> Some Opcode.Addi
    | Slt -> Some Slti
    | Sltu -> Some Sltiu
    | Xor -> Some Xori
    | Or -> Some Ori
    | And -> Some Andi
    | Addw -> Some Addiw
    | Sll -> Some Slli
    | Srl -> Some Srli
    | Sra -> Some Srai
    | Sllw -> Some Slliw
    | Srlw -> Some Srliw
    | Sraw -> Some Sraiw
    | Ror -> Some Rori
    | Rorw -> Some Roriw
    | Bclr -> Some Bclri
    | Bext -> Some Bexti
    | Binv -> Some Binvi
    | Bset -> Some Bseti
    | _ -> None

  let i_desc = function
    | Opcode.Addi -> Some (0x13, 0, 0, None)
    | Slti -> Some (0x13, 2, 0, None)
    | Sltiu -> Some (0x13, 3, 0, None)
    | Xori -> Some (0x13, 4, 0, None)
    | Ori -> Some (0x13, 6, 0, None)
    | Andi -> Some (0x13, 7, 0, None)
    | Slli -> Some (0x13, 1, 0, Some xlen)
    | Srli -> Some (0x13, 5, 0, Some xlen)
    | Srai -> Some (0x13, 5, (if xlen = 64 then 0x10 else 0x20), Some xlen)
    | Addiw -> Some (0x1b, 0, 0, None)
    | Slliw -> Some (0x1b, 1, 0, Some 32)
    | Srliw -> Some (0x1b, 5, 0, Some 32)
    | Sraiw -> Some (0x1b, 5, 0x20, Some 32)
    | Rori -> Some (0x13, 5, (if xlen = 64 then 0x18 else 0x30), Some xlen)
    | Roriw -> Some (0x1b, 5, 0x30, Some 32)
    | Bclri -> Some (0x13, 1, (if xlen = 64 then 0x12 else 0x24), Some xlen)
    | Bexti -> Some (0x13, 5, (if xlen = 64 then 0x12 else 0x24), Some xlen)
    | Binvi -> Some (0x13, 1, (if xlen = 64 then 0x1a else 0x34), Some xlen)
    | Bseti -> Some (0x13, 1, (if xlen = 64 then 0x0a else 0x14), Some xlen)
    | _ -> None

  let load_desc = function
    | Opcode.Lb -> Some 0
    | Lh -> Some 1
    | Lw -> Some 2
    | Ld -> Some 3
    | Lbu -> Some 4
    | Lhu -> Some 5
    | Lwu -> Some 6
    | _ -> None

  let store_desc = function
    | Opcode.Sb -> Some 0
    | Sh -> Some 1
    | Sw -> Some 2
    | Sd -> Some 3
    | _ -> None

  let branch_desc = function
    | Opcode.Beq -> Some 0
    | Bne -> Some 1
    | Blt -> Some 4
    | Bge -> Some 5
    | Bltu -> Some 6
    | Bgeu -> Some 7
    | _ -> None

  (* OP-FP (opcode [0x53]) descriptors, one table per operand shape (M5 corpus
     evidence, the corpus notes - almabench.c/bisect.c/fftsp.c/knucleotide.c/...).
     [funct3] here is the rounding-mode field GAS's own bare mnemonic spelling
     always picks - `7` (dynamic) for the arithmetic family and every convert *to* a
     narrower or differently-rounded type, `0` (round-to-nearest-even) for a
     conversion real hardware defines as always exact - not an operand this corpus
     ever spells explicitly, so no rounding-mode operand is read. *)
  let f_arith_desc = function
    | Opcode.Fadd_s -> Some (7, 0x00)
    | Fadd_d -> Some (7, 0x01)
    | Fsub_s -> Some (7, 0x04)
    | Fsub_d -> Some (7, 0x05)
    | Fmul_s -> Some (7, 0x08)
    | Fmul_d -> Some (7, 0x09)
    | Fdiv_s -> Some (7, 0x0c)
    | Fdiv_d -> Some (7, 0x0d)
    | _ -> None

  (* [fneg.d]/[fneg.s]/[fmv.d]/[fabs.d]/[fabs.s]/[fmv.s] - FSGNJN/FSGNJ/FSGNJX with
     [rs2] forced equal to [rs1], real hardware's own alias (verified against real
     riscv64-linux-gnu-as/objdump: `fneg.d fs0, fs1` -> `22949453`, decoding back
     with [rs1] = [rs2] = 9; `fabs.s fa0, fa1` -> `20b5a553`, `fabs.d fa0, fa1` ->
     `22b5a553`, `fmv.s fa0, fa1` -> `20b58553`, funct3/funct7 unchanged from the
     general [fsgnjx]/[fsgnj] forms - only the forced-equal operand makes these the
     pseudo spelling). The general two-different-register [fsgnj]/[fsgnjn]/[fsgnjx]
     this shares a word with is {!f_sgnj3_desc}. *)
  let f_sgnj_desc = function
    | Opcode.Fneg_s -> Some (1, 0x10)
    | Fneg_d -> Some (1, 0x11)
    | Fmv_d -> Some (0, 0x11)
    | Fabs_s -> Some (2, 0x10)
    | Fabs_d -> Some (2, 0x11)
    | Fmv_s -> Some (0, 0x10)
    | _ -> None

  (* General [fsgnj]/[fsgnjn]/[fsgnjx] with [rs1]/[rs2] distinct real registers
     (verified against real riscv64-linux-gnu-as/objdump: `fsgnj.s fa0, fa1, fa2` ->
     `20c58553`, `fsgnjn.d fa0, fa1, fa2` -> `22c59553`). Shares its word with
     {!f_sgnj_desc}'s [rs1] = [rs2] aliases; the lowering/decode direction each
     favor the pseudo name only when the registers actually coincide. *)
  let f_sgnj3_desc = function
    | Opcode.Fsgnj_s -> Some (0, 0x10)
    | Fsgnjn_s -> Some (1, 0x10)
    | Fsgnjx_s -> Some (2, 0x10)
    | Fsgnj_d -> Some (0, 0x11)
    | Fsgnjn_d -> Some (1, 0x11)
    | Fsgnjx_d -> Some (2, 0x11)
    | _ -> None

  (* [fmin]/[fmax]: like {!f_sgnj3_desc}, funct3 is a real per-mnemonic
     selector (0 = min, 1 = max), not a rounding mode (verified against real
     riscv64-linux-gnu-as/objdump: `fmin.s fa0, fa1, fa2` -> `28c58553`,
     `fmax.d fa0, fa1, fa2` -> `2ac59553`). *)
  let f_minmax_desc = function
    | Opcode.Fmin_s -> Some (0, 0x14)
    | Fmax_s -> Some (1, 0x14)
    | Fmin_d -> Some (0, 0x15)
    | Fmax_d -> Some (1, 0x15)
    | _ -> None

  (* [fsqrt]: one FP source, [rs2] a fixed selector (00000) rather than a real
     second operand - the same shape as {!f_to_f_desc}'s precision converts,
     with [funct3] a genuine rounding mode like the arithmetic family
     (verified against real riscv64-linux-gnu-as/objdump: `fsqrt.s fa0, fa1`
     -> `5805f553`, `fsqrt.d fa0, fa1` -> `5a05f553`, funct3 = 7 = dyn). *)
  let f_sqrt_desc = function
    | Opcode.Fsqrt_s -> Some (7, 0x2c, 0)
    | Fsqrt_d -> Some (7, 0x2d, 0)
    | _ -> None

  (* [fclass]: [rd] is a GPR (the classification bitmask), [rs1] is FP,
     [rs2] a fixed selector (00000); [funct3] = 1 is a fixed per-mnemonic
     identity bit, not a rounding mode, at the same (funct7, rs2=0) group
     {!f_to_i_desc}'s [Fmv_x_d] shares with funct3 = 0 - kept in its own
     table (not {!f_to_i_desc}) so it is never reachable through that
     table's explicit-rounding-mode-override lowering arm, which real GNU
     as itself rejects for any fixed-funct3 mnemonic in this group (verified:
     `fmv.x.d a0, fa1, rtz` -> "illegal operands"; this project's own encoder
     pre-dates this pass and does not enforce that rejection - a
     latent gap, not one this slice's own new mnemonics reproduce). Verified
     against real riscv64-linux-gnu-as/objdump: `fclass.s a0, fa1` ->
     `e0059553`, `fclass.d a0, fa1` -> `e2059553`. *)
  let f_class_desc = function
    | Opcode.Fclass_s -> Some (1, 0x70, 0)
    | Fclass_d -> Some (1, 0x71, 0)
    | _ -> None

  (* [fmadd]/[fmsub]/[fnmsub]/[fnmadd]: RISC-V's only R4-type instructions -
     four real FP register operands ([rd], [rs1], [rs2], [rs3]) rather than
     three, so unlike every other OP-FP form above this cannot reuse
     {!Lowered.R}/[word_r]: the top 7 bits of the word split into [rs3] (5
     bits) and a 2-bit [fmt] selector (00 = S, 01 = D) instead of one fixed
     [funct7], and each mnemonic gets its own base opcode rather than sharing
     OP-FP's [0x53] (verified against real riscv64-linux-gnu-as/objdump:
     `fmadd.s fa0, fa1, fa2, fa3` -> `68c5f543`, `fmsub.d fa0, fa1, fa2, fa3`
     -> `6ac5f547`, `fnmsub.s ...` -> `68c5f54b`, `fnmadd.d ...` ->
     `6ac5f54f`; [rs3] = 13 = [fa3] in each case). [funct3] is a genuine
     rounding mode like the arithmetic family (verified: `fmadd.s fa0, fa1,
     fa2, fa3, rtz` -> `68c59543`, funct3 = 1); this slice, like
     {!f_arith_desc}, only claims the bare dynamic-rounding spelling. *)
  let f_fma_desc = function
    | Opcode.Fmadd_s -> Some (0x43, 0)
    | Fmadd_d -> Some (0x43, 1)
    | Fmsub_s -> Some (0x47, 0)
    | Fmsub_d -> Some (0x47, 1)
    | Fnmsub_s -> Some (0x4b, 0)
    | Fnmsub_d -> Some (0x4b, 1)
    | Fnmadd_s -> Some (0x4f, 0)
    | Fnmadd_d -> Some (0x4f, 1)
    | _ -> None

  (* Compares: [rd] is a GPR (the boolean result), [rs1]/[rs2] are FP. Verified
     against real riscv64-linux-gnu-as/objdump: `feq.s a0, fa1, fa2` ->
     `a0c5a553`, `fle.s a0, fa1, fa2` -> `a0c58553` (funct7 = 0x50, matching
     [flt.s]'s own group). *)
  let f_cmp_desc = function
    | Opcode.Feq_d -> Some (2, 0x51)
    | Fle_d -> Some (0, 0x51)
    | Flt_d -> Some (1, 0x51)
    | Flt_s -> Some (1, 0x50)
    | Feq_s -> Some (2, 0x50)
    | Fle_s -> Some (0, 0x50)
    | _ -> None

  (* Float-to-integer converts, and [fmv.x.d] (bit-for-bit move, not a conversion) -
     [rd] is a GPR, [rs1] is FP, [rs2] is a fixed selector rather than a second
     operand. *)
  let f_to_i_desc = function
    | Opcode.Fcvt_w_d -> Some (7, 0x61, 0)
    | Fcvt_wu_d -> Some (7, 0x61, 1)
    | Fcvt_l_d -> Some (7, 0x61, 2)
    | Fcvt_lu_d -> Some (7, 0x61, 3)
    | Fcvt_w_s -> Some (7, 0x60, 0)
    | Fcvt_wu_s -> Some (7, 0x60, 1)
    | Fcvt_l_s -> Some (7, 0x60, 2)
    | Fcvt_lu_s -> Some (7, 0x60, 3)
    | Fmv_x_d -> Some (0, 0x71, 0)
    | _ -> None

  (* [fmv.x.w]: [fmv.x.d]'s single-precision sibling (bit-for-bit move, not a
     conversion), at the same (funct7, rs2 = 0) group {!f_class_desc}'s
     [Fclass_s] shares with funct3 = 1 - kept in its own table (not
     {!f_to_i_desc}, where [fmv.x.d] itself lives) for the same reason
     {!f_class_desc} is: so it is never reachable through {!f_to_i_desc}'s
     explicit-rounding-mode-override lowering arm and its documented latent
     bug (see {!f_class_desc}'s own comment). Verified against real
     riscv64-linux-gnu-as/objdump: `fmv.x.w a0, fa1` -> `e0058553` (funct7 =
     0x70, funct3 = 0, matching [fclass.s]'s own group). [fmv.x.s] is the exact
     same encoding under the ISA manual's pseudo-op spelling (verified against
     real riscv64-linux-gnu-as/objdump: `fmv.x.s a0, fa1` -> `e0058553`,
     byte-identical to [fmv.x.w]; GAS's own disassembly favors the [.w] name
     regardless of which spelling was assembled). *)
  let f_mv_x_w_desc = function
    | Opcode.Fmv_x_w -> Some (0, 0x70, 0)
    | Fmv_x_s -> Some (0, 0x70, 0)
    | _ -> None

  (* Integer-to-float converts - [rd] is FP, [rs1] is a GPR, [rs2] fixed.
     [fmv.w.x] (bit-for-bit move, not a conversion) shares this shape and has
     no explicit-rounding-mode-override lowering arm to worry about - only
     {!f_to_i_desc} has one - so it is safe to add directly here rather than
     needing its own table the way [fmv.x.w] does. Verified against real
     riscv64-linux-gnu-as/objdump: `fmv.w.x fa0, a1` -> `f0058553` (funct7 =
     0x78, a group no other mnemonic in this table shares). *)
  let i_to_f_desc = function
    | Opcode.Fcvt_d_w -> Some (0, 0x69, 0)
    | Fcvt_d_wu -> Some (0, 0x69, 1)
    | Fcvt_d_l -> Some (7, 0x69, 2)
    | Fcvt_d_lu -> Some (7, 0x69, 3)
    | Fcvt_s_w -> Some (7, 0x68, 0)
    | Fcvt_s_wu -> Some (7, 0x68, 1)
    | Fcvt_s_l -> Some (7, 0x68, 2)
    | Fcvt_s_lu -> Some (7, 0x68, 3)
    | Fmv_w_x -> Some (0, 0x78, 0)
    (* [fmv.s.x] - [fmv.w.x]'s ISA-manual pseudo-op spelling, byte-identical
       (verified against real riscv64-linux-gnu-as/objdump: `fmv.s.x fa0, a1` ->
       `f0058553`, matching [fmv.w.x] exactly; same disassembly-favors-the-other-
       name caveat as {!f_mv_x_w_desc}'s [fmv.x.s]). *)
    | Fmv_s_x -> Some (0, 0x78, 0)
    | _ -> None

  (* Float-to-float precision converts - [rd]/[rs1] both FP, [rs2] fixed (the source
     format, per the ISA's own encoding: [1] = double, [0] = single). *)
  let f_to_f_desc = function
    | Opcode.Fcvt_s_d -> Some (7, 0x20, 1)
    | Fcvt_d_s -> Some (0, 0x21, 0)
    | _ -> None

  (* LOAD-FP/STORE-FP (opcodes [0x07]/[0x27]) - the identical addressing mode
     integer [lw]/[sw] use, into/from an FP register instead of a GPR; only
     [funct3] (word width) differs between the two, exactly as it does for the
     integer loads/stores this shares a byte-count convention with. *)
  let f_load_desc = function Opcode.Flw -> Some 2 | Fld -> Some 3 | _ -> None
  let f_store_desc = function Opcode.Fsw -> Some 2 | Fsd -> Some 3 | _ -> None

  (* The fixed fields of each hand-written form, by opcode: the one place they are read when a
     {!Lowered} form is encoded. An opcode with no fields for a format is
     [`Format_mismatch]. *)
  module Desc = struct
    type r = { opcode : int; funct3 : int; funct7 : int; rs2 : int option }

    let r op =
      let reg (opcode, funct3, funct7) = Some { opcode; funct3; funct7; rs2 = None } in
      let fp (funct3, funct7) = Some { opcode = 0x53; funct3; funct7; rs2 = None } in
      let fp_sel (funct3, funct7, rs2) = Some { opcode = 0x53; funct3; funct7; rs2 = Some rs2 } in
      let ( |? ) a b = match a with Some _ -> a | None -> b () in
      ( ( ( ( ( ( ( ( ( ( ( (Option.bind (r_desc op) reg |? fun () -> Option.bind (amo3_desc op) reg)
                          |? fun () ->
                            Option.map
                              (fun (opcode, funct3, funct7) ->
                                { opcode; funct3; funct7; rs2 = Some 0 })
                              (lr_desc op) )
                        |? fun () -> Option.bind (f_arith_desc op) fp )
                      |? fun () -> Option.bind (f_sgnj3_desc op) fp )
                    |? fun () -> Option.bind (f_minmax_desc op) fp )
                  |? fun () -> Option.bind (f_sgnj_desc op) fp )
                |? fun () -> Option.bind (f_cmp_desc op) fp )
              |? fun () -> Option.bind (f_sqrt_desc op) fp_sel )
            |? fun () -> Option.bind (f_class_desc op) fp_sel )
          |? fun () -> Option.bind (f_mv_x_w_desc op) fp_sel )
        |? fun () -> Option.bind (f_to_i_desc op) fp_sel )
      |? fun () -> Option.bind (i_to_f_desc op) fp_sel )
      |? fun () -> Option.bind (f_to_f_desc op) fp_sel

    (* AES-32/SM4: [funct7] is the byte select over these five bits. *)
    let rbs = function
      | Opcode.Aes32dsi -> Some 0x15
      | Aes32dsmi -> Some 0x17
      | Aes32esi -> Some 0x11
      | Aes32esmi -> Some 0x13
      | Sm4ed -> Some 0x18
      | Sm4ks -> Some 0x1a
      | _ -> None

    let r4 = f_fma_desc

    type i = { opcode : int; funct3 : int; funct_hi : int; shamt_bits : int option }

    let i op =
      let plain opcode funct3 = Some { opcode; funct3; funct_hi = 0; shamt_bits = None } in
      match i_desc op with
      | Some (opcode, funct3, funct_hi, shamt_bits) -> Some { opcode; funct3; funct_hi; shamt_bits }
      | None -> (
          match (unary_imm_desc op, load_desc op, f_load_desc op) with
          | Some (opcode, funct3, _), _, _ -> plain opcode funct3
          | None, Some funct3, _ -> plain 0x03 funct3
          | None, None, Some funct3 -> plain 0x07 funct3
          | None, None, None -> (
              match op with
              | Opcode.Jalr -> plain 0x67 0
              | Csrrw | Csrw -> plain 0x73 1
              | Csrrs | Csrr | Csrs -> plain 0x73 2
              | Csrrc | Csrc -> plain 0x73 3
              | Csrrwi | Csrwi -> plain 0x73 5
              | Csrrsi | Csrsi -> plain 0x73 6
              | Csrrci | Csrci -> plain 0x73 7
              | Aes64ks1i -> plain 0x13 1
              | _ -> None))

    let s op =
      match (store_desc op, f_store_desc op) with
      | Some funct3, _ -> Some (0x23, funct3)
      | None, Some funct3 -> Some (0x27, funct3)
      | None, None -> None

    let b = branch_desc
    let u = function Opcode.Lui -> Some 0x37 | Auipc -> Some 0x17 | _ -> None
    let ca = ca_desc

    let cr = function
      | Opcode.C_jr | C_mv -> Some 8
      | C_jalr | C_add | C_ebreak -> Some 9
      | _ -> None

    (* [funct3] of the CL, CS, CI (SP-relative), CSS, CJ and CB forms. *)
    let c_funct3 = function
      | Opcode.C_lw | C_lwsp -> Some 2
      | C_ld | C_ldsp -> Some 3
      | C_sw | C_swsp -> Some 6
      | C_sd | C_sdsp -> Some 7
      | C_j -> Some 5
      | C_jal -> Some 1
      | C_beqz -> Some 6
      | C_bnez -> Some 7
      | _ -> None
  end

  (* The rounding mode a form takes when none is written, for the forms that have one. *)
  let default_rm op = Option.bind (f_shape op) (fun s -> s.rm)

  let fits_signed bits v =
    let lim = Int64.shift_left 1L (bits - 1) in
    Int64.compare v (Int64.neg lim) >= 0 && Int64.compare v lim < 0

  let fits_unsigned bits v =
    let lim = Int64.shift_left 1L bits in
    Int64.compare v 0L >= 0 && Int64.compare v lim < 0

  (* Every compressed load/store offset is an unsigned, alignment-scaled
     immediate ([bits] total width including the implicit-zero low bits;
     [scale] is 4 for word forms, 8 for doubleword forms) - e.g. [c.lw]/[c.sw]
     are [bits = 7, scale = 4] (0..124), [c.ld]/[c.sd] are [bits = 8, scale = 8]
     (0..248). *)
  let fits_scaled_unsigned bits scale v = Int64.rem v scale = 0L && fits_unsigned bits v

  let int64_expr e =
    match Asm_core.Expr.fold Asm_core.Expr.no_env e with
    | Ok (Asm_core.Expr.Const n) -> Bigint.to_int64_opt n
    | Ok _ | Error _ -> None

  (* A CSR address is a real 12-bit UNSIGNED field (0-4095), unlike every
     other I-type immediate this project encodes (all signed). Rather than
     adding new bit-masking machinery, every Csrr*/Csr{r,w,s,c}* match arm
     below writes the address's own SIGNED 12-bit two's-complement
     equivalent into the existing [Lowered.I]/[word_i] path unchanged -
     that path's own field-masking already operates on raw two's-
     complement bit patterns, so the two are bit-identical once masked back
     to 12 bits (confirmed against real GNU as: `csrrw a0, 0xf14, a1` ->
     `f1459573`, mhartid = 0xf14 = 3860 encoded via signed_csr = -236). *)
  let signed_csr csr = if Int64.compare csr 2048L >= 0 then Int64.sub csr 4096L else csr

  (* The address a CSR operand stands for: a standard name (as GNU as takes it, ahead of any
     symbol of that name) or an absolute expression. *)
  let csr_number e =
    match e with
    | Asm_core.Expr.Symbol s -> (
        match Riscv_csr.number s with Some n -> Some (Int64.of_int n) | None -> int64_expr e)
    | _ -> int64_expr e

  (* Zaamo's [(rs1)] memory operand carries no real offset field - GAS
     accepts the [offset(base)] syntax generically, so this rejects any
     concrete nonzero fold rather than silently dropping it, matching real
     GNU as's own "illegal operands" rejection of `amoadd.w a0, a1, 4(a2)`. *)
  let zero_offset e = match int64_expr e with Some 0L -> true | _ -> false
  let symbol_of = function Operand.Sym (Asm_core.Expr.Symbol s) -> Some s | _ -> None

  let symbols_of ops =
    let rec go acc = function
      | [] -> Some (List.rev acc)
      | o :: rest -> ( match symbol_of o with Some s -> go (s :: acc) rest | None -> None)
    in
    go [] ops

  (* An (i,o,r,w) fence flag set: a non-empty subsequence of "iorw", as a
     4-bit field with i at bit 3. *)
  let fence_set s =
    let rec go acc pos k =
      if k = String.length s then if acc = 0 then None else Some acc
      else
        match String.index_from_opt "iorw" pos s.[k] with
        | Some j -> go (acc lor (1 lsl (3 - j))) (j + 1) (k + 1)
        | None -> None
    in
    go 0 0 0

  (* DEC-RV-TABLE: a generated row's operand, checked against its field and
     placed at its bit position; [None] when the operand does not fit. *)
  (* A scattered immediate: range, alignment and non-zero checks, then each
     value bit to its instruction bit. *)
  let table_scatter (sc : Riscv_table_row.scatter) v =
    let aligned = Int64.rem v (Int64.shift_left 1L sc.scale) = 0L in
    let in_range = if sc.signed then fits_signed sc.width v else fits_unsigned sc.width v in
    if (not aligned) || (not in_range) || (sc.nonzero && v = 0L) then None
    else
      Some
        (List.fold_left
           (fun acc (ibit, vbit) ->
             Int64.logor acc
               (Int64.shift_left (Int64.logand (Int64.shift_right_logical v vbit) 1L) ibit))
           0L sc.bits)

  let table_field ?(acc = 0L) ~prev op (operand : Riscv_table_row.operand) =
    let place lsb v = Some (Int64.shift_left v lsb) in
    let low width v = Int64.logand v (Int64.sub (Int64.shift_left 1L width) 1L) in
    let value () = Option.bind (expr_of op) int64_expr in
    let mem_offset () =
      match op with
      | Operand.Mem m -> (
          match (base_num m, int64_expr m.offset) with
          | Some base, Some v when fits_signed 12 v -> Some (base, low 12 v)
          | _ -> None)
      | _ -> None
    in
    match operand with
    | Gpr { lsb; nonzero } -> (
        match xnum op with
        | Some n when (not nonzero) || n <> 0 -> place lsb (Int64.of_int n)
        | _ -> None)
    | Fpr { lsb } -> Option.bind (fnum op) (fun n -> place lsb (Int64.of_int n))
    | Uimm { lsb; width } -> (
        match value () with Some v when fits_unsigned width v -> place lsb v | _ -> None)
    | Simm { lsb; width } -> (
        match value () with Some v when fits_signed width v -> place lsb (low width v) | _ -> None)
    | Fixed_gpr n -> ( match xnum op with Some m when m = n -> Some 0L | _ -> None)
    | Rm { lsb; _ } -> (
        match op with
        | Operand.Sym (Asm_core.Expr.Symbol name) ->
            Option.bind (Rm.of_name name) (fun m -> place lsb (Int64.of_int (Rm.to_funct3 m)))
        | _ -> None)
    | Tied { lsb } -> Option.map (fun r -> Int64.shift_left (Int64.of_int r) lsb) prev
    | Keyword k -> (
        match op with
        | Operand.Sym (Asm_core.Expr.Symbol s) when String.equal s k -> Some 0L
        | _ -> None)
    | Fli { lsb } -> (
        match op with
        | Operand.Sym (Asm_core.Expr.Symbol text) ->
            Option.bind (Riscv_table_row.fli_index text) (fun i -> place lsb (Int64.of_int i))
        | _ -> None)
    | Mem_zero { base } -> (
        match op with
        | Operand.Mem m -> (
            match (base_num m, int64_expr m.offset) with
            | Some b, Some 0L -> place base (Int64.of_int b)
            | _ -> None)
        | _ -> None)
    | Mem_hi { base } -> (
        match op with
        | Operand.Mem m -> (
            match (base_num m, int64_expr m.offset) with
            | Some b, Some v when fits_signed 12 v && Int64.rem v 32L = 0L ->
                Some
                  (Int64.logor
                     (Int64.shift_left (Int64.of_int b) base)
                     (Int64.shift_left (Int64.logand (Int64.shift_right v 5) 0x7fL) 25))
            | _ -> None)
        | _ -> None)
    | Gpr_pair { lsb; below } -> (
        match xnum op with
        | Some n when xlen >= below || n land 1 = 0 -> place lsb (Int64.of_int n)
        | _ -> None)
    | Creg { lsb } -> (
        match xnum op with
        | Some n when n >= 8 && n <= 15 -> place lsb (Int64.of_int (n - 8))
        | _ -> None)
    | Cfreg { lsb } -> (
        match fnum op with
        | Some n when n >= 8 && n <= 15 -> place lsb (Int64.of_int (n - 8))
        | _ -> None)
    | Gpr_except { lsb; except } -> (
        match xnum op with
        | Some n when not (List.mem n except) -> place lsb (Int64.of_int n)
        | _ -> None)
    | Scatter sc -> Option.bind (value ()) (table_scatter sc)
    | Cmem { base; offset } -> (
        match op with
        | Operand.Mem m -> (
            match (base_num m, int64_expr m.offset) with
            | Some b, Some v when b >= 8 && b <= 15 ->
                Option.map
                  (fun f -> Int64.logor f (Int64.shift_left (Int64.of_int (b - 8)) base))
                  (table_scatter offset v)
            | _ -> None)
        | _ -> None)
    | Spmem { offset } -> (
        match op with
        | Operand.Mem m -> (
            match (base_num m, int64_expr m.offset) with
            | Some 2, Some v -> table_scatter offset v
            | _ -> None)
        | _ -> None)
    | Cui { hi; lo } -> (
        match value () with
        | Some v when (v >= 1L && v <= 31L) || (v >= 0xfffe0L && v <= 0xfffffL) ->
            Some
              (Int64.logor
                 (Int64.shift_left (Int64.logand (Int64.shift_right v 5) 1L) hi)
                 (Int64.shift_left (Int64.logand v 0x1fL) lo))
        | _ -> None)
    | Sreg { lsb } ->
        Option.bind (xnum op) (fun n ->
            Option.bind (Riscv_table_row.sreg_code n) (fun c -> place lsb (Int64.of_int c)))
    | Rlist { lsb } -> (
        match op with
        | Operand.Sym (Asm_core.Expr.Symbol text) ->
            let compact = String.concat "" (String.split_on_char ' ' text) in
            Option.bind (Riscv_table_row.rlist_code compact) (fun c -> place lsb (Int64.of_int c))
        | _ -> None)
    | Stack_adj { rlist; spimm; push } -> (
        let code = Int64.to_int (Int64.logand (Int64.shift_right_logical acc rlist) 0xfL) in
        let base = Riscv_table_row.rlist_base ~xlen code in
        match value () with
        | Some v ->
            let magnitude = Int64.to_int (if push then Int64.neg v else v) in
            let extra = magnitude - base in
            if extra >= 0 && extra <= 48 && extra mod 16 = 0 then
              place spimm (Int64.of_int (extra / 16))
            else None
        | None -> None)
    | Uimm_min { lsb; width; min } -> (
        match value () with
        | Some v when fits_unsigned width v && Int64.to_int v >= min -> place lsb v
        | _ -> None)
    | Mem_i { base } ->
        Option.map
          (fun (b, v) ->
            Int64.logor (Int64.shift_left (Int64.of_int b) base) (Int64.shift_left v 20))
          (mem_offset ())
    | Mem_s { base } ->
        Option.map
          (fun (b, v) ->
            Int64.logor
              (Int64.shift_left (Int64.of_int b) base)
              (Int64.logor
                 (Int64.shift_left (Int64.shift_right_logical v 5) 25)
                 (Int64.shift_left (Int64.logand v 0x1fL) 7)))
          (mem_offset ())
    | Vreg { lsb } -> Option.bind (vnum op) (fun n -> place lsb (Int64.of_int n))
    | Vm _ -> if is_v0t op then Some 0L else None
    | Fixed_vreg n -> ( match vnum op with Some m when m = n -> Some 0L | _ -> None)
    (* the keyword list is the rest of the operands: {!table_lower} encodes it *)
    | Vtype _ -> None
    | Indexed { prefix; lsb; width; scale } -> (
        match op with
        | Operand.Sym (Asm_core.Expr.Symbol text) -> (
            match Riscv_table_row.indexed_number ~prefix text with
            | Some n when n mod scale = 0 && fits_unsigned width (Int64.of_int (n / scale)) ->
                place lsb (Int64.of_int (n / scale))
            | _ -> None)
        | _ -> None)

  (* The register number of a register operand, for a following [Tied] field. *)
  let table_reg op = match op with Operand.Reg (Reg.F r) -> Some (Freg.to_int r) | _ -> xnum op
  let table_row_applies (r : Riscv_table_row.row) = r.xlen = 0 || r.xlen = xlen

  (* Every row spelled like row [i] is tried in table order; the first whose
     operands all fit is the encoding. *)
  let table_lower i ops =
    let mnemonic = table_rows.(i).Riscv_table_row.mnemonic in
    let encode_row (r : Riscv_table_row.row) =
      (* [Tied] fields are not spelled; a trailing [Rm] or [Vm] may be omitted, and a trailing
         [Vtype] takes every remaining operand. *)
      let spelled =
        List.filter (function Riscv_table_row.Tied _ -> false | _ -> true) r.operands
      in
      let n_ops = List.length ops and n_spelled = List.length spelled in
      let with_default =
        match List.rev spelled with
        | Riscv_table_row.Rm { lsb; default } :: _ when n_ops = n_spelled - 1 ->
            Some (Int64.shift_left (Int64.of_int default) lsb)
        | Riscv_table_row.Vm { lsb } :: _ when n_ops = n_spelled - 1 ->
            Some (Int64.shift_left 1L lsb)
        | Riscv_table_row.Vtype _ :: _ when n_ops >= n_spelled -> Some 0L
        | _ -> if n_ops = n_spelled then Some 0L else None
      in
      match with_default with
      | None -> None
      | Some extra ->
          let rec go acc prev operands ops =
            match (operands, ops) with
            | [], [] -> Some acc
            | (Riscv_table_row.Tied _ as t) :: rest, _ -> (
                match table_field ~prev (Operand.Imm Bigint.zero) t with
                | Some f -> go (Int64.logor acc f) prev rest ops
                | None -> None)
            | [ Riscv_table_row.Rm _ ], [] | [ Riscv_table_row.Vm _ ], [] -> Some acc
            | [ Riscv_table_row.Vtype { lsb; width } ], _ :: _ -> (
                match Option.bind (symbols_of ops) Riscv_table_row.vtype_of_keywords with
                | Some v when v < 1 lsl width ->
                    Some (Int64.logor acc (Int64.shift_left (Int64.of_int v) lsb))
                | _ -> None)
            | o :: rest, op :: ops -> (
                match table_field ~acc ~prev op o with
                | Some f -> go (Int64.logor acc f) (table_reg op) rest ops
                | None -> None)
            | _ -> None
          in
          go (Int64.logor r.match_ extra) None r.operands ops
    in
    let found =
      Array.fold_left
        (fun acc (r : Riscv_table_row.row) ->
          match acc with
          | Some _ -> acc
          | None ->
              if String.equal r.mnemonic mnemonic && table_row_applies r then encode_row r else None)
        None table_rows
    in
    match found with
    | Some word when Int64.logand word 3L <> 3L ->
        Ok [ Lowered.Fixed_half { op = Opcode.Table i; word } ]
    | Some word -> Ok [ Lowered.Fixed { op = Opcode.Table i; word } ]
    | None -> wrong mnemonic

  let lower_instruction_ungated state i =
    let opn = Opcode.name i.Instruction.op and iop = i.Instruction.op in
    match (i.op, i.ops) with
    | Opcode.Table row, _
      when Int64.logand table_rows.(row).Riscv_table_row.match_ 3L <> 3L && not state.rvc ->
        Error (diag ~pos:__POS__ (`Compressed_disabled opn))
    | Opcode.Table row, ops -> table_lower row ops
    | ( ( Opcode.Mul | Remu | Mulh | Mulhsu | Mulhu | Div | Divu | Rem | Mulw | Divw | Divuw | Remw
        | Remuw ),
        _ )
      when xlen <> 64 && m_rv64_only i.Instruction.op ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | ( ( Opcode.Addw | Subw | Sllw | Srlw | Sraw | Sh1adduw | Sh2adduw | Sh3adduw | Clzw | Ctzw
        | Cpopw | Packw | Rolw | Rorw | Sha512sum0 | Sha512sum1 | Sha512sig0 | Sha512sig1 | Aes64ds
        | Aes64dsm | Aes64es | Aes64esm | Aes64ks2 | Aes64im | Aes64ks1i | C_addw | C_subw ),
        _ )
      when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | (Opcode.Addiw | Slliw | Srliw | Sraiw | Roriw | Sext_w | Ld | Lwu | Sd), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | (Opcode.C_ld | C_sd | C_ldsp | C_sdsp), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | ( ( Opcode.Zip | Unzip | Sha512sum0r | Sha512sum1r | Sha512sig0l | Sha512sig1l | Sha512sig0h
        | Sha512sig1h | Aes32dsi | Aes32dsmi | Aes32esi | Aes32esmi ),
        _ )
      when xlen <> 32 ->
        Error (diag ~pos:__POS__ (`Rv32_only opn))
    | ( ( Opcode.C_addi | C_and | C_or | C_xor | C_sub | C_addw | C_subw | C_jr | C_jalr | C_mv
        | C_add | C_ebreak | C_lw | C_sw | C_lwsp | C_swsp | C_ld | C_sd | C_ldsp | C_sdsp | C_beqz
        | C_j | C_jal | C_bnez ),
        _ )
      when not state.rvc ->
        Error (diag ~pos:__POS__ (`Compressed_disabled opn))
    | Opcode.C_addi, [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm when rd <> Xreg.zero -> Ok [ Lowered.Caddi { rd; imm } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (ca_desc op) -> (
        match (creg a, creg b, ca_desc op) with
        | Some rd_rs1, Some rs2, Some _ -> Ok [ Lowered.Cr2 { op = iop; rd_rs1; rs2 } ]
        | _ -> wrong opn)
    | (Opcode.C_jr | C_jalr), [ a ] -> (
        match xreg a with
        | Some rd_rs1 when rd_rs1 <> Xreg.zero ->
            Ok [ Lowered.Cr { op = iop; rd_rs1; rs2 = Xreg.zero } ]
        | _ -> wrong opn)
    | (Opcode.C_mv | C_add), [ a; b ] -> (
        match (xreg a, xreg b) with
        | Some rd_rs1, Some rs2 when rs2 <> Xreg.zero -> Ok [ Lowered.Cr { op = iop; rd_rs1; rs2 } ]
        | _ -> wrong opn)
    | Opcode.C_ebreak, [] -> Ok [ Lowered.Cr { op = iop; rd_rs1 = Xreg.zero; rs2 = Xreg.zero } ]
    | (Opcode.C_lw | C_ld), [ a; Operand.Mem m ] -> (
        match (creg a, cbase m) with
        | Some rd, Some rs1 -> Ok [ Lowered.Cl { op = iop; rd; rs1; imm = m.offset } ]
        | _ -> wrong opn)
    | (Opcode.C_sw | C_sd), [ a; Operand.Mem m ] -> (
        match (creg a, cbase m) with
        | Some rs2, Some rs1 -> Ok [ Lowered.Cs { op = iop; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | (Opcode.C_lwsp | C_ldsp), [ a; Operand.Mem m ] -> (
        match (xreg a, base m) with
        | Some rd, Some sp when sp = Xreg.sp && rd <> Xreg.zero ->
            Ok [ Lowered.Clsp { op = iop; rd; imm = m.offset } ]
        | _ -> wrong opn)
    | (Opcode.C_swsp | C_sdsp), [ a; Operand.Mem m ] -> (
        match (xreg a, base m) with
        | Some rs2, Some sp when sp = Xreg.sp ->
            Ok [ Lowered.Css { op = iop; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | Opcode.C_jal, _ when xlen <> 32 -> Error (diag ~pos:__POS__ (`Rv32_only opn))
    | (Opcode.C_j | C_jal), [ target ] -> (
        (* CJ format: quadrant 01, funct3 101 ([c.j]) or 001 ([c.jal], RV32 only - RV64 reuses
           the encoding for [c.addiw]). *)
        match expr_of target with
        | Some target -> Ok [ Lowered.Cj { op = iop; target } ]
        | None -> wrong opn)
    | (Opcode.C_beqz | C_bnez), [ a; target ] -> (
        match (creg a, expr_of target) with
        | Some rs1, Some target -> Ok [ Lowered.Cb { op = iop; rs1; target } ]
        | _ -> wrong opn)
    | ( ( Opcode.Fcvt_l_d | Fmv_x_d | Fcvt_s_l | Fcvt_lu_d | Fcvt_d_l | Fcvt_d_lu | Fcvt_l_s
        | Fcvt_lu_s | Fcvt_s_lu ),
        _ )
      when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | ( ( Opcode.Amoswap_d | Amoadd_d | Amoxor_d | Amoand_d | Amoor_d | Amomin_d | Amomax_d
        | Amominu_d | Amomaxu_d | Lr_d | Sc_d ),
        _ )
      when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | op, [ a; b; Operand.Mem m ] when Option.is_some (amo3_desc op) -> (
        (* [amoOP rd, rs2, (rs1)] / [scOP rd, rs2, (rs1)] - the memory
           operand's offset is never syntax-visible (real GNU as rejects
           `amoadd.w a0, a1, 4(a2)`: "illegal operands"), so requiring a
           literal zero here is what enforces that shape instead of
           silently discarding a real one. *)
        match (gx a, gx b, gbase m, amo3_desc op) with
        | Some rd, Some rs2, Some rs1, Some _ when zero_offset m.offset ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = None } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (lr_desc op) -> (
        match (gx a, gbase m, lr_desc op) with
        | Some rd, Some rs1, Some _ when zero_offset m.offset ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = None } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (r_desc op) -> (
        match (gx a, gx b, gx c, r_desc op) with
        | Some rd, Some rs1, Some rs2, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = None } ]
        | Some rd, Some rs1, None, _ -> (
            (* A third register-typed operand is the R-type form above; a third
               immediate-typed one is GAS's I-type alias, per imm_alias. An op
               with no I-type counterpart (sub, subw, mul) falls through to
               [wrong], which is what GAS does too. *)
            match (imm_alias op, expr_of c) with
            | Some ialias, Some imm -> (
                match i_desc ialias with
                | Some _ -> Ok [ Lowered.I { op = ialias; rd; rs1; imm } ]
                | None -> wrong opn)
            | _ -> wrong opn)
        | _ -> wrong opn)
    | Opcode.Aes64ks1i, [ a; b; imm ] -> (
        (* [aes64ks1i rd, rs1, rnum] - AES-64's first key-schedule helper.
           Unlike every shift-immediate form the generic [i_desc]/[shamt_bits]
           path below serves, [rnum]'s valid range (0-10) is narrower than
           its 4-bit encoding field (0-15) - rounds 11-15 are architecturally
           reserved, and real GNU as rejects them outright ("Improper rnum
           immediate"), confirmed against riscv64-linux-gnu-as 2.44: rnum
           0/1/8/9/10 assemble, 11/15/16/-1 are all rejected. [i_desc] only
           checks a field's raw bit width, not this narrower semantic range,
           so this needs its own validated match arm instead. [rnum] is
           folded to a concrete value here (not left symbolic like a
           relocatable immediate) since GAS itself requires it foldable and
           a reserved round number is a real assembly-time error. The
           composed imm12 is the fixed 8-bit prefix 0x31 (bits[11:4]) with
           [rnum] in bits[3:0]. *)
        match (gx a, gx b, expr_of imm) with
        | Some rd, Some rs1, Some e -> (
            match int64_expr e with
            | Some rnum when Int64.compare rnum 0L >= 0 && Int64.compare rnum 10L <= 0 ->
                Ok [ Lowered.I { op = iop; rd; rs1; imm = const (0x310 lor Int64.to_int rnum) } ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Aes32dsi | Aes32dsmi | Aes32esi | Aes32esmi | Sm4ed | Sm4ks), [ a; b; c; bs ] -> (
        (* [aes32dsi/aes32dsmi/aes32esi/aes32esmi rd, rs1, rs2, bs] - AES-32's
           byte-select-parameterized round functions. [bs] is a real,
           syntax-visible 2-bit immediate, but unlike [aes64ks1i]'s [rnum] it
           uses its FULL field range (0-3, confirmed against real
           riscv32-linux-gnu-as 2.43.1: all four values assemble, bs=4 is
           rejected as "Improper bs immediate" purely because it overflows
           the 2-bit field, not because of any narrower semantic
           restriction) - so no extra range check beyond the field width is
           needed here, unlike [aes64ks1i]'s dedicated arm. [bs] does not sit
           in a spare high bit of an existing field the way [aes64ks1i]'s
           [rnum] does either: it occupies bits[31:30], directly ABOVE the
           5-bit fixed selector that would otherwise be [word_r]'s own
           7-bit funct7 (bits[31:25]) for a plain three-register form -
           since [word_r] already encodes its [~funct7] argument across
           exactly that same 7-bit span, this reuses [Lowered.R]/[word_r]
           completely unchanged by composing [bs] and the fixed 5-bit
           selector into one 7-bit value at lowering time
           ([(bs lsl 5) lor base]), rather than adding a new lowered
           instruction shape. Confirmed against real riscv32-linux-gnu-as
           2.43.1 for all four mnemonics x all four bs values (16 cases):
           `aes32dsi a0,a1,a2,1` -> `6ac58533`, matching
           `(1 lsl 5) lor 0x15 = 0x35` as the composed funct7. Real GNU as
           rejects all four AES-32 mnemonics outright on RV64 (genuinely
           absent, not merely extension-gated).

           [sm4ed]/[sm4ks] (Zksed's own SM4 round/key-schedule functions)
           share this identical shape and [bs]-composition rule, but - unlike
           the AES-32 four - real GNU as accepts BOTH on BOTH profiles
           (confirmed: riscv32-linux-gnu-as 2.43.1 and riscv64-linux-gnu-as
           2.44 both assemble `sm4ed a0,a1,a2,1` -> `70c58533`, matching
           `(1 lsl 5) lor 0x18 = 0x38`). *)
        match (gx a, gx b, gx c, expr_of bs) with
        | Some rd, Some rs1, Some rs2, Some e -> (
            match int64_expr e with
            | Some bs when Int64.compare bs 0L >= 0 && Int64.compare bs 3L <= 0 ->
                Ok [ Lowered.Rbs { op = iop; rd; rs1; rs2; bs = Int64.to_int bs } ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Csrrw | Csrrs | Csrrc), [ a; csr; b ] -> (
        (* [csrrw/csrrs/csrrc rd, csr, rs1] - Zicsr's register-source CSR
           forms. GAS's own operand text order is [rd, csr, rs1] - the CSR
           address comes SECOND, not third, unlike every plain I-type form
           this project has built (confirmed against real
           riscv32-linux-gnu-as: `csrrw a0, mstatus, a1` -> `30059573`,
           matching riscv-opcodes' own encoding.fields order [rd, rs1, csr]
           bit-for-bit even though the GAS text order differs). [csr] is a
           real, syntax-visible 12-bit UNSIGNED address (0-4095 - real GNU
           as rejects 4096+ as "improper CSR address"); see {!signed_csr}
           for how that is encoded. Both profiles accept identical syntax
           (Zicsr is XLEN-independent). *)
        match (gx a, expr_of csr, gx b) with
        | Some rd, Some csr_e, Some rs1 -> (
            match csr_number csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                Ok [ Lowered.I { op = iop; rd; rs1; imm = const (Int64.to_int (signed_csr csr)) } ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Csrrwi | Csrrsi | Csrrci), [ a; csr; zimm ] -> (
        (* [csrrwi/csrrsi/csrrci rd, csr, zimm] - Zicsr's immediate-source
           CSR forms. [zimm] is a real, syntax-visible 5-bit unsigned
           immediate (0-31 - real GNU as rejects 32+ as "improper CSRxI
           immediate"), occupying the SAME bit position (bits[19:15]) a GPR
           number would in [rs1] - since that position's own encoding is
           bit-identical either way, this reuses {!word_i}/[Lowered.I]'s
           existing [rs1] field unchanged to carry [zimm] rather than a
           real register, confirmed against real GNU as: `csrrwi a0,
           mstatus, 31` -> `300fd573`. *)
        match (gx a, expr_of csr, expr_of zimm) with
        | Some rd, Some csr_e, Some zimm_e -> (
            match (csr_number csr_e, int64_expr zimm_e) with
            | Some csr, Some zimm
              when Int64.compare csr 0L >= 0
                   && Int64.compare csr 4095L <= 0
                   && Int64.compare zimm 0L >= 0
                   && Int64.compare zimm 31L <= 0 ->
                Ok
                  [
                    Lowered.I
                      {
                        op = iop;
                        rd;
                        rs1 = Reg.x (Int64.to_int zimm);
                        imm = const (Int64.to_int (signed_csr csr));
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | Opcode.Csrr, [ a; csr ] -> (
        (* [csrr rd, csr] - GAS's read-only alias for [csrrs rd, csr, x0]
           (real hardware reads without ever setting a bit, since ORing
           with x0's value 0 is a no-op). Confirmed against real GNU as:
           `csrr a0, mstatus` -> `30002573`, matching `csrrs a0, mstatus,
           zero` bit-for-bit. *)
        match (gx a, expr_of csr) with
        | Some rd, Some csr_e -> (
            match csr_number csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                Ok
                  [
                    Lowered.I
                      { op = iop; rd; rs1 = Reg.zero; imm = const (Int64.to_int (signed_csr csr)) };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Csrw | Csrs | Csrc), [ csr; b ] -> (
        (* [csrw/csrs/csrc csr, rs1] - GAS's write/set/clear-only aliases
           for [csrrw/csrrs/csrrc x0, csr, rs1] (rd = x0, discarding the
           old value). Unlike the register-source base forms above, GAS's
           own text order here is [csr, rs1] - csr FIRST, not second -
           confirmed against real GNU as: `csrw mstatus, a1` -> `30059073`,
           matching `csrrw zero, mstatus, a1` bit-for-bit; riscv-opcodes'
           own declared operands order, [rs1, csr], matches neither this
           project's base-form order nor this one, so neither can be
           inferred from the other. *)
        match (expr_of csr, gx b) with
        | Some csr_e, Some rs1 -> (
            match csr_number csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                Ok
                  [
                    Lowered.I
                      { op = iop; rd = Reg.zero; rs1; imm = const (Int64.to_int (signed_csr csr)) };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Csrwi | Csrsi | Csrci), [ csr; zimm ] -> (
        (* [csrwi/csrsi/csrci csr, zimm] - GAS's write/set/clear-only
           aliases for [csrrwi/csrrsi/csrrci x0, csr, zimm] (rd = x0).
           Confirmed against real GNU as: `csrwi mstatus, 5` -> `3002d073`,
           matching `csrrwi zero, mstatus, 5` bit-for-bit. *)
        match (expr_of csr, expr_of zimm) with
        | Some csr_e, Some zimm_e -> (
            match (csr_number csr_e, int64_expr zimm_e) with
            | Some csr, Some zimm
              when Int64.compare csr 0L >= 0
                   && Int64.compare csr 4095L <= 0
                   && Int64.compare zimm 0L >= 0
                   && Int64.compare zimm 31L <= 0 ->
                Ok
                  [
                    Lowered.I
                      {
                        op = iop;
                        rd = Reg.zero;
                        rs1 = Reg.x (Int64.to_int zimm);
                        imm = const (Int64.to_int (signed_csr csr));
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (i_desc op) -> (
        match (gx a, gx b, expr_of c, i_desc op) with
        | Some rd, Some rs1, Some imm, Some _ -> Ok [ Lowered.I { op = iop; rd; rs1; imm } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (unary_imm_desc op) -> (
        match (gx a, gx b, unary_imm_desc op) with
        | Some rd, Some rs1, Some (_, _, funct12) ->
            Ok [ Lowered.I { op = iop; rd; rs1; imm = const funct12 } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (load_desc op) -> (
        match (gx a, gbase m, load_desc op) with
        | Some rd, Some rs1, Some _ -> Ok [ Lowered.I { op = iop; rd; rs1; imm = m.offset } ]
        | _ -> wrong opn)
    | Opcode.Ld, [ a; (Operand.Sym _ as target) ] -> (
        (* GAS's own `ld rd, symbol` pseudo, a literal-pool load: auipc rd,
           %pcrel_hi(symbol); ld rd, %pcrel_lo(...)(rd) - the same anchored
           hi/lo pairing `la`/`call` already use, with the second word an
           opcode-0x03 load rather than addi/jalr. Confirmed against real
           riscv64-linux-gnu-as. Scoped to `ld` alone since that is the only
           load pseudo this corpus evidences (a 64-bit constant pulled from a
           `.rodata.cst8` literal, the compiler's own float/double materialization
           idiom on riscv64). *)
        match (xreg a, expr_of target, load_desc Opcode.Ld) with
        | Some rd, Some target, Some funct3 ->
            Ok [ Lowered.Pair { op = iop; rd = Reg.X rd; tmp = rd; target; kind = Load funct3 } ]
        | _ -> wrong opn)
    | op, [ a; (Operand.Sym _ as target); b ] when Option.is_some (f_load_desc op) -> (
        (* [fld rd, symbol, xtmp] / [flw rd, symbol, xtmp] - the identical anchored
           [auipc]+load pseudo as [ld] above, except the scratch register can't be [rd]
           itself ([auipc] only ever writes a GPR, and [rd] here is a scalar FP register),
           so GAS's own three-operand spelling names it explicitly (M5,
           the corpus notes - almabench.c/fftsp.c/knucleotide.c/...). *)
        match (gf a, expr_of target, xreg b, f_load_desc op) with
        | Some rd, Some target, Some tmp, Some funct3 ->
            Ok [ Lowered.Pair { op = iop; rd; tmp; target; kind = Fload funct3 } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (store_desc op) -> (
        match (gx a, gbase m, store_desc op) with
        | Some rs2, Some rs1, Some _ -> Ok [ Lowered.S { op = iop; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_load_desc op) -> (
        match (gf a, gbase m, f_load_desc op) with
        | Some rd, Some rs1, Some _ -> Ok [ Lowered.I { op = iop; rd; rs1; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_store_desc op) -> (
        match (gf a, gbase m, f_store_desc op) with
        | Some rs2, Some rs1, Some _ -> Ok [ Lowered.S { op = iop; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_arith_desc op) -> (
        match (gf a, gf b, gf c, f_arith_desc op) with
        | Some rd, Some rs1, Some rs2, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_sgnj3_desc op) -> (
        match (gf a, gf b, gf c, f_sgnj3_desc op) with
        | Some rd, Some rs1, Some rs2, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_minmax_desc op) -> (
        match (gf a, gf b, gf c, f_minmax_desc op) with
        | Some rd, Some rs1, Some rs2, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_sgnj_desc op) -> (
        match (gf a, gf b, f_sgnj_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs1; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_sqrt_desc op) -> (
        match (gf a, gf b, f_sqrt_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_class_desc op) -> (
        match (gx a, gf b, f_class_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_mv_x_w_desc op) -> (
        match (gx a, gf b, f_mv_x_w_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b; c; d ] when Option.is_some (f_fma_desc op) -> (
        match (gf a, gf b, gf c, gf d, f_fma_desc op) with
        | Some rd, Some rs1, Some rs2, Some rs3, Some _ ->
            Ok [ Lowered.R4 { op = iop; rd; rs1; rs2; rs3; rm = Rm.Dyn } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_cmp_desc op) -> (
        match (gx a, gf b, gf c, f_cmp_desc op) with
        | Some rd, Some rs1, Some rs2, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = Some rs2; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_i_desc op) -> (
        match (gx a, gf b, f_to_i_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b; Operand.Sym (Asm_core.Expr.Symbol rm) ] when Option.is_some (f_to_i_desc op) -> (
        (* [fcvt.w.d rd, rs1, rtz] - an explicit rounding-mode operand overriding the
           bare mnemonic's own default (M5 - perlin.c/binarytrees.c's
           own [(int)]/[(long)] C cast idiom, which truncates and so needs [rtz] rather
           than the default dynamic mode). Checked against real riscv64-linux-gnu-as:
           `fcvt.l.d x22, f10, rtz` -> `c2251b53` (funct3 = 1, matching {!Rm}'s
           [Rtz] = 1). *)
        match (gx a, gf b, f_to_i_desc op, Rm.of_name rm) with
        | Some rd, Some rs1, Some _, Some rm ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = Some rm } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (i_to_f_desc op) -> (
        match (gf a, gx b, i_to_f_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_f_desc op) -> (
        match (gf a, gf b, f_to_f_desc op) with
        | Some rd, Some rs1, Some _ ->
            Ok [ Lowered.R { op = iop; rd; rs1; rs2 = None; rm = default_rm op } ]
        | _ -> wrong opn)
    | op, [ a; b; target ] when Option.is_some (branch_desc op) -> (
        match (xreg a, xreg b, expr_of target, branch_desc op) with
        | Some rs1, Some rs2, Some target, Some _ -> Ok [ Lowered.B { op = iop; rs1; rs2; target } ]
        | _ -> wrong opn)
    | (Opcode.Bgtu | Bleu), [ a; b; target ] -> (
        (* [bgtu rs,rt,label]/[bleu rs,rt,label] - GAS's own greater-than/
           less-or-equal-unsigned pseudos, [bltu]/[bgeu] with the two register
           operands swapped (real hardware defines no distinct opcode; only
           [bltu rt,rs,label]/[bgeu rt,rs,label] exist). Real objdump always
           disassembles the swapped-operand word back as the real underlying
           mnemonic, never the pseudo - this project's canonical printer follows
           that choice (the same alias-preference precedent as AArch64's
           [ubfx]/[ubfiz], Capability ladder item 4), reusing
           [Lowered.B]'s existing shape rather than adding a name-only variant.
           Checked against real riscv64-linux-gnu-as/objdump: `bgtu a0, a1, .`
           -> `00a5e063`, decoding as `bltu a1, a0, .`; `bleu a0, a1, .` ->
           `fea5fee3`, decoding as `bgeu a1, a0, .`. *)
        let real_op = if i.op = Bgtu then Opcode.Bltu else Opcode.Bgeu in
        match (xreg a, xreg b, expr_of target, branch_desc real_op) with
        | Some ra, Some rb, Some target, Some _ ->
            Ok [ Lowered.B { op = real_op; rs1 = rb; rs2 = ra; target } ]
        | _ -> wrong opn)
    | (Opcode.Bgt | Ble), [ a; b; target ] -> (
        (* The signed siblings of [bgtu]/[bleu]: [blt]/[bge] with the register
           operands swapped. Checked against real riscv64-linux-gnu-as:
           `bgt a0, a1, L` -> `blt a1, a0, L`; `ble a0, a1, L` -> `bge a1, a0, L`. *)
        let real_op = if i.op = Bgt then Opcode.Blt else Opcode.Bge in
        match (xreg a, xreg b, expr_of target, branch_desc real_op) with
        | Some ra, Some rb, Some target, Some _ ->
            Ok [ Lowered.B { op = real_op; rs1 = rb; rs2 = ra; target } ]
        | _ -> wrong opn)
    | (Opcode.Beqz | Bnez | Bgez | Bltz | Blez | Bgtz), [ a; target ] -> (
        (* Compare-with-x0 pseudos. [beqz]/[bnez]/[bgez]/[bltz] put the register in
           rs1 and x0 in rs2; [blez]/[bgtz] are [bge x0, rs]/[blt x0, rs]. Checked
           against real riscv64-linux-gnu-as: `beqz a0` -> `beq a0,zero`, `blez a0`
           -> `bge zero,a0`, `bgtz a0` -> `blt zero,a0`. *)
        let real_op, reg_in_rs1 =
          match i.op with
          | Beqz -> (Opcode.Beq, true)
          | Bnez -> (Opcode.Bne, true)
          | Bgez -> (Opcode.Bge, true)
          | Bltz -> (Opcode.Blt, true)
          | Blez -> (Opcode.Bge, false)
          | _ -> (Opcode.Blt, false)
        in
        match (xreg a, expr_of target, branch_desc real_op) with
        | Some r, Some target, Some _ ->
            let rs1, rs2 = if reg_in_rs1 then (r, Xreg.zero) else (Xreg.zero, r) in
            Ok [ Lowered.B { op = real_op; rs1; rs2; target } ]
        | _ -> wrong opn)
    | (Opcode.Lui | Auipc), [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm -> Ok [ Lowered.U { op = iop; rd; imm } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target -> Ok [ Lowered.J { rd; target } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = Xreg.ra; target } ]
        | None -> wrong opn)
    | Opcode.Jalr, [ a; Operand.Mem m ] -> (
        match (gx a, gbase m) with
        | Some rd, Some rs1 -> Ok [ Lowered.I { op = Opcode.Jalr; rd; rs1; imm = m.offset } ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a; b; imm ] -> (
        match (gx a, gx b, expr_of imm) with
        | Some rd, Some rs1, Some imm -> Ok [ Lowered.I { op = Opcode.Jalr; rd; rs1; imm } ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a ] | Opcode.Jr, [ a ] -> (
        match gx a with
        | Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    op = Opcode.Jalr;
                    rd = (if i.op = Jalr then Reg.x 1 else Reg.zero);
                    rs1;
                    imm = const 0;
                  };
              ]
        | None -> wrong opn)
    | Opcode.Snez, [ a; b ] -> (
        (* [snez rd, rs] - GAS's own set-not-equal-zero pseudo, [sltu rd, zero, rs]
           under a different spelling (only ever reachable through [Sltu]'s own
           three-register form otherwise, which has no all-zero-[rs1] entry point
           of its own). Real objdump prefers the alias text ([snez a2, a3]), but
           this project's canonical printer reuses the real underlying mnemonic
           the same way {!Mv}/{!Nop} above already reuse [addi]'s, rather than
           adding a second, decode-only spelling nothing here compares against
           (M5 - helpers/riscv.c's own zero-test idiom).
           Checked against real riscv64-linux-gnu-as/objdump: `snez a2, a3` ->
           `00d03633`, matching `sltu a2, zero, a3` bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs2 ->
            Ok [ Lowered.R { op = Opcode.Sltu; rd; rs1 = Reg.zero; rs2 = Some rs2; rm = None } ]
        | _ -> wrong opn)
    | Opcode.Neg, [ a; b ] -> (
        (* [neg rd, rs] - the base ISA's own negate pseudo, [sub rd, zero, rs] under a
           different spelling (only ever reachable through [Sub]'s own three-register
           form otherwise, which has no all-zero-[rs1] entry point of its own, the same
           gap {!Snez} above fills for [sltu]). Checked against real
           riscv64-linux-gnu-as/riscv32-linux-gnu-as/objdump: `neg a2, a3` -> `40d00633`
           on both profiles, matching `sub a2, zero, a3` bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs2 ->
            Ok [ Lowered.R { op = Opcode.Sub; rd; rs1 = Reg.zero; rs2 = Some rs2; rm = None } ]
        | _ -> wrong opn)
    | Opcode.Seqz, [ a; b ] -> (
        (* [seqz rd, rs] - set-equal-zero, [sltiu rd, rs, 1] under a different spelling
           (unsigned less-than-1 is exactly the zero test). Checked against real
           riscv64-linux-gnu-as/riscv32-linux-gnu-as/objdump: `seqz a2, a3` -> `0016b613`
           on both profiles, matching `sltiu a2, a3, 1` bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs1 -> Ok [ Lowered.I { op = Opcode.Sltiu; rd; rs1; imm = const 1 } ]
        | _ -> wrong opn)
    | Opcode.Sltz, [ a; b ] -> (
        (* [sltz rd, rs] - set-less-than-zero, [slt rd, rs, zero] under a different
           spelling (only ever reachable through [Slt]'s own three-register form
           otherwise, which has no all-zero-[rs2] entry point of its own). Checked
           against real riscv64-linux-gnu-as/riscv32-linux-gnu-as/objdump:
           `sltz a2, a3` -> `0006a633` on both profiles, matching `slt a2, a3, zero`
           bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs1 ->
            Ok [ Lowered.R { op = Opcode.Slt; rd; rs1; rs2 = Some Reg.zero; rm = None } ]
        | _ -> wrong opn)
    | Opcode.Sgtz, [ a; b ] -> (
        (* [sgtz rd, rs] - set-greater-than-zero, [slt rd, zero, rs] under a different
           spelling (the [Sltz] arm's mirror image: [Slt]'s all-zero-[rs1] entry point).
           Checked against real riscv64-linux-gnu-as/riscv32-linux-gnu-as/objdump:
           `sgtz a2, a3` -> `00d02633` on both profiles, matching `slt a2, zero, a3`
           bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs2 ->
            Ok [ Lowered.R { op = Opcode.Slt; rd; rs1 = Reg.zero; rs2 = Some rs2; rm = None } ]
        | _ -> wrong opn)
    | Opcode.Zext_b, [ a; b ] -> (
        (* [zext.b rd, rs] - the base ISA's own zero-extend-byte pseudo, [andi rd, rs,
           0xff] under a different spelling; unlike {!Zext_h} below this needs no Zbb
           dependency (it exists in [rv_i], not a bit-manipulation extension). Checked
           against real riscv64-linux-gnu-as/riscv32-linux-gnu-as/objdump: `zext.b a2,
           a3` -> `0ff6f613` on both profiles, matching `andi a2, a3, 0xff`
           bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs1 -> Ok [ Lowered.I { op = Opcode.Andi; rd; rs1; imm = const 255 } ]
        | _ -> wrong opn)
    | Opcode.Zext_h, [ a; b ] -> (
        (* [zext.h rd, rs] - Zbb's zero-extend-halfword pseudo, [pack rd, rs, zero] (RV32)
           or [packw rd, rs, zero] (RV64) under a different spelling; riscv-opcodes exports
           the RV32 case as a separate "zext.h.rv32" pseudo-op record specializing [pack],
           but real GAS accepts the literal text `zext.h` (never `zext.h.rv32`) on BOTH
           profiles - confirmed against real riscv32-linux-gnu-as (2.43.1) and
           riscv64-linux-gnu-as (2.44): `zext.h a0, a1` assembles to `0805c533`/`0805c53b`
           respectively, matching `pack`/`packw a0, a1, zero` bit-for-bit; both real
           assemblers also reject a three- or one-operand `zext.h` outright ("illegal
           operands"), which is why this is its own two-operand match arm rather than a
           reuse of the generic three-register [r_desc] path {!Pack}/{!Packw} share. *)
        match (gx a, gx b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.R
                  {
                    op = (if xlen = 64 then Opcode.Packw else Opcode.Pack);
                    rd;
                    rs1;
                    rs2 = Some Reg.zero;
                    rm = None;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Sext_w, [ a; b ] -> (
        (* [sext.w rd, rs] - the sign-extend-word pseudo, [addiw rd, rs, 0] under a
           different spelling (RV64 only: {!Opcode.t}'s early [Rv64_only] gate
           above already rejects it on RV32). Checked against real
           riscv64-linux-gnu-as/objdump: `sext.w a1, a2` -> `0006059b`, matching
           `addiw a1, a2, 0` bit-for-bit. *)
        match (gx a, gx b) with
        | Some rd, Some rs1 -> Ok [ Lowered.I { op = Opcode.Addiw; rd; rs1; imm = const 0 } ]
        | _ -> wrong opn)
    | Opcode.Mv, [ a; b ] -> (
        match (gx a, gx b) with
        | Some rd, Some rs1 -> Ok [ Lowered.I { op = Opcode.Addi; rd; rs1; imm = const 0 } ]
        | _ -> wrong opn)
    | Opcode.Nop, [] ->
        Ok [ Lowered.I { op = Opcode.Addi; rd = Reg.zero; rs1 = Reg.zero; imm = const 0 } ]
    | Opcode.J, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = Xreg.zero; target } ]
        | None -> wrong opn)
    | Opcode.Call, [ target ] | Opcode.Tail, [ target ] -> (
        match expr_of target with
        | Some target ->
            Ok
              [
                Lowered.Pair
                  {
                    op = iop;
                    rd = (if i.op = Call then Reg.x 1 else Reg.zero);
                    tmp = Xreg.of_int_exn (if i.op = Call then 1 else 6);
                    target;
                    kind = Jalr;
                  };
              ]
        | None -> wrong opn)
    | ((Opcode.La | Lla) as op), [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target when op = Lla || state.pic ->
            Ok [ Lowered.Pair { op = iop; rd = Reg.X rd; tmp = rd; target; kind = Addi } ]
        | Some rd, Some target ->
            (* In non-PIC state GAS's [la] is the absolute LUI/ADDI pair.
               Keeping this decision in lowering makes source-ordered option
               replay observable and prevents a final-file state from leaking
               backwards across an earlier instruction. *)
            Ok
              [
                Lowered.U { op = Opcode.Lui; rd; imm = Asm_core.Expr.Modifier ("%hi", target) };
                Lowered.I
                  {
                    op = Opcode.Addi;
                    rd = Reg.X rd;
                    rs1 = Reg.X rd;
                    imm = Asm_core.Expr.Modifier ("%lo", target);
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Li, [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm -> (
            let addi imm =
              Ok [ Lowered.I { op = Opcode.Addi; rd = Reg.X rd; rs1 = Reg.zero; imm } ]
            in
            match int64_expr imm with
            | Some v when fits_signed 12 v -> addi imm
            | Some v when Int64.logand v 0xfffL = 0L && fits_signed 32 v ->
                (* GAS's own [li] expansion for a constant whose low 12 bits are
                   exactly zero collapses to a bare [lui] - no [addi], the
                   redundant "+0" GAS itself never emits - the only large-constant
                   [li] shape this corpus evidences (helpers/riscv.c's
                   page/window-aligned address constants, e.g. `li a0,
                   0x30000000`). Checked against real riscv64-linux-gnu-as/
                   objdump: `li a0, 0x30000000` -> `lui a0, 0x30000`; `li a2,
                   -4096` -> `lui a2, 0xfffff`. A constant needing a genuine
                   [lui]+[addi] pair, or one wider than 32 bits, is not evidenced
                   and still falls through to the single-[addi] form below, which
                   then reports the same out-of-range diagnostic it always has. *)
                Ok
                  [
                    Lowered.U
                      {
                        op = Opcode.Lui;
                        rd;
                        imm = Asm_core.Expr.Const (Bigint.of_int64 (Int64.shift_right v 12));
                      };
                  ]
            | _ -> addi imm)
        | _ -> wrong opn)
    | Opcode.Ret, [] ->
        Ok [ Lowered.I { op = Opcode.Jalr; rd = Reg.zero; rs1 = Reg.x 1; imm = const 0 } ]
    | Opcode.Ecall, [] -> Ok [ Lowered.Fixed { op = Opcode.Ecall; word = 0x00000073L } ]
    | Opcode.Ebreak, [] -> Ok [ Lowered.Fixed { op = Opcode.Ebreak; word = 0x00100073L } ]
    | Opcode.Unimp, [] -> Ok [ Lowered.Fixed { op = Opcode.Unimp; word = 0xc0001073L } ]
    | Opcode.Fence_i, [] -> Ok [ Lowered.Fixed { op = Opcode.Fence_i; word = 0x0000100fL } ]
    | Opcode.Fence, [] ->
        (* Bare [fence] is GNU as's [fence iorw,iorw]: `0ff0000f`. *)
        Ok [ Lowered.Fixed { op = Opcode.Fence; word = 0x0ff0000fL } ]
    | ( Opcode.Fence,
        [ Operand.Sym (Asm_core.Expr.Symbol pred); Operand.Sym (Asm_core.Expr.Symbol succ) ] ) -> (
        (* [fence pred,succ]: opcode 0x0f/funct3 0 with rd = rs1 = x0 and fm = 0;
           [pred]/[succ] are (i,o,r,w) flag sets at bits 27:24/23:20, each a
           non-empty subset of "iorw" spelled in that order. Checked against real
           riscv64-linux-gnu-as/objdump: `fence rw,w` -> `0310000f`, bare
           [fence] -> `0ff0000f`. *)
        match (fence_set pred, fence_set succ) with
        | Some p, Some q ->
            let word = Int64.of_int ((p lsl 24) lor (q lsl 20) lor 0x0f) in
            Ok [ Lowered.Fixed { op = Opcode.Fence; word } ]
        | _ -> wrong opn)
    | Opcode.Fence_tso, [] ->
        (* [fence.tso]: fm = 1000, pred = succ = rw. Checked against real
           riscv64-linux-gnu-as: `8330000f`. *)
        Ok [ Lowered.Fixed { op = Opcode.Fence_tso; word = 0x8330000fL } ]
    | Opcode.Pause, [] ->
        (* Zihintpause [pause]: [fence w,0]. Checked against real
           riscv64-linux-gnu-as (-march=..._zihintpause): `0100000f`. *)
        Ok [ Lowered.Fixed { op = Opcode.Pause; word = 0x0100000fL } ]
    | _ -> wrong opn

  let lower_instruction state i =
    match feature_gate state i.Instruction.op with
    | Error _ as e -> e
    | Ok () -> lower_instruction_ungated state i

  let mask bits v =
    if bits = 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L bits) 1L)

  let field shift bits v = Int64.shift_left (mask bits v) shift

  let word_r ~opcode ~funct3 ~funct7 ~rd ~rs1 ~rs2 =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int (Reg.index rd)))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int (Reg.index rs1)))
               (Int64.logor (field 20 5 (Int64.of_int rs2)) (field 25 7 (Int64.of_int funct7))))))

  let word_r4 ~opcode ~fmt ~funct3 ~rd ~rs1 ~rs2 ~rs3 =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int (Reg.index rd)))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int (Reg.index rs1)))
               (Int64.logor
                  (field 20 5 (Int64.of_int (Reg.index rs2)))
                  (Int64.logor
                     (field 25 2 (Int64.of_int fmt))
                     (field 27 5 (Int64.of_int (Reg.index rs3))))))))

  let word_i ~opcode ~funct3 ~rd ~rs1 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int (Reg.index rd)))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor (field 15 5 (Int64.of_int (Reg.index rs1))) (field 20 12 imm))))

  let word_s ~opcode ~funct3 ~rs1 ~rs2 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor (field 7 5 imm)
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int (Reg.index rs1)))
               (Int64.logor
                  (field 20 5 (Int64.of_int (Reg.index rs2)))
                  (field 25 7 (Int64.shift_right_logical imm 5))))))

  let word_b ~funct3 ~rs1 ~rs2 imm =
    Int64.logor 0x63L
      (Int64.logor
         (field 7 1 (Int64.shift_right_logical imm 11))
         (Int64.logor
            (field 8 4 (Int64.shift_right_logical imm 1))
            (Int64.logor
               (field 12 3 (Int64.of_int funct3))
               (Int64.logor
                  (field 15 5 (Int64.of_int (Xreg.to_int rs1)))
                  (Int64.logor
                     (field 20 5 (Int64.of_int (Xreg.to_int rs2)))
                     (Int64.logor
                        (field 25 6 (Int64.shift_right_logical imm 5))
                        (field 31 1 (Int64.shift_right_logical imm 12))))))))

  let word_u ~opcode ~rd imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor (field 7 5 (Int64.of_int (Xreg.to_int rd))) (field 12 20 imm))

  let word_j ~rd imm =
    Int64.logor 0x6fL
      (Int64.logor
         (field 7 5 (Int64.of_int (Xreg.to_int rd)))
         (Int64.logor
            (field 12 8 (Int64.shift_right_logical imm 12))
            (Int64.logor
               (field 20 1 (Int64.shift_right_logical imm 11))
               (Int64.logor
                  (field 21 10 (Int64.shift_right_logical imm 1))
                  (field 31 1 (Int64.shift_right_logical imm 20))))))

  (* C.ADDI: quadrant 1 / funct3 000.  The six-bit signed immediate is split
     as imm[5] at bit 12 and imm[4:0] at bits 6:2; rd is also rs1 and x0 is
     excluded.  Keeping this as its own constructor, rather than shrinking an
     ADDI opportunistically, makes compression an explicit source/state choice
     and avoids silently changing a requested 32-bit instruction's bytes. *)
  let word_caddi ~rd imm =
    Int64.logor 0x1L
      (Int64.logor (field 2 5 imm)
         (Int64.logor
            (field 7 5 (Int64.of_int (Xreg.to_int rd)))
            (field 12 1 (Int64.shift_right_logical imm 5))))

  (* CA-format: quadrant(01) | rs2'[4:2] | funct2[6:5] | rd_rs1'[9:7] |
     funct6[15:10], where rd_rs1'/rs2' are the {!Creg.field}s of [rd_rs1]/[rs2] (the
     RVC compressed-register field). Confirmed against real
     riscv64-linux-gnu-as: `c.and a0,a1` -> word 0x8d6d. *)
  let word_cr2 ~funct6 ~funct2 ~rd_rs1 ~rs2 =
    Int64.logor 0x1L
      (Int64.logor
         (field 2 3 (Int64.of_int (Creg.field rs2)))
         (Int64.logor
            (field 5 2 (Int64.of_int funct2))
            (Int64.logor
               (field 7 3 (Int64.of_int (Creg.field rd_rs1)))
               (field 10 6 (Int64.of_int funct6)))))

  (* CR-format: quadrant(10) | rs2[6:2] | rd_rs1[11:7] | funct4[15:12] -
     the full-width sibling of {!word_cr2} ([c.jr]/[c.jalr]/[c.mv]/
     [c.add]/[c.ebreak]): no -8 bias, [rd_rs1]/[rs2] are real 0..31 GPR
     numbers. Confirmed against real riscv64-linux-gnu-as: `c.jr ra` ->
     0x8082, `c.mv ra,t6` -> 0x80fe, `c.ebreak` -> 0x9002. *)
  let word_cr ~funct4 ~rd_rs1 ~rs2 =
    Int64.logor 0x2L
      (Int64.logor
         (field 2 5 (Int64.of_int (Xreg.to_int rs2)))
         (Int64.logor
            (field 7 5 (Int64.of_int (Xreg.to_int rd_rs1)))
            (field 12 4 (Int64.of_int funct4))))

  (* CB-format compressed branches: quadrant(01) | rs1'[9:7] | funct3[15:13], where
     rs1' is [rs1] minus 8 like {!word_cr2}; the offset bits are left zero here and
     patched in later by the {!fixup_kind.Branch9c} fixup via {!cb_slices}, the same
     split {!word_b} uses for [beq]/[bne]'s own [Branch13] fixup. *)
  let word_cb ~funct3 ~rs1 =
    Int64.logor 0x1L
      (Int64.logor (field 7 3 (Int64.of_int (Creg.field rs1))) (field 13 3 (Int64.of_int funct3)))

  (* CL-format compressed loads: quadrant(00) | rd'[4:2] | offset bits | rs1'[9:7] |
     offset bits | funct3[15:13], where rd'/rs1' are [rd]/[rs1] minus 8 like
     {!word_cr2}. [c.lw]'s 5 raw offset bits scatter as [inst5|hi3|inst6] (RISC-V's
     own word-offset swap, confirmed against real riscv64-linux-gnu-as: `c.lw
     a0,4(a1)` -> word 0x41c8). *)
  let word_c_lw ~funct3 ~rd ~rs1 offset =
    Int64.logor
      (field 2 3 (Int64.of_int (Creg.field rd)))
      (Int64.logor
         (field 5 1 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 6 1 (Int64.shift_right_logical offset 2))
            (Int64.logor
               (field 7 3 (Int64.of_int (Creg.field rs1)))
               (Int64.logor
                  (field 10 3 (Int64.shift_right_logical offset 3))
                  (field 13 3 (Int64.of_int funct3))))))

  (* [c.ld]'s 5 raw offset bits instead concatenate straight as [lo2|hi3] - the
     extra doubleword-alignment zero bit leaves no swap to do, unlike {!word_c_lw}.
     Confirmed: `c.ld a0,8(a1)` -> word 0x6588. *)
  let word_c_ld ~funct3 ~rd ~rs1 offset =
    Int64.logor
      (field 2 3 (Int64.of_int (Creg.field rd)))
      (Int64.logor
         (field 5 2 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 7 3 (Int64.of_int (Creg.field rs1)))
            (Int64.logor
               (field 10 3 (Int64.shift_right_logical offset 3))
               (field 13 3 (Int64.of_int funct3)))))

  (* CS-format compressed stores: the store-side siblings of {!word_c_lw}/
     {!word_c_ld} - same offset scatter, [rs2'] takes rd's field position.
     Confirmed: `c.sw a0,4(a1)` -> word 0xc1c8, `c.sd a0,8(a1)` -> word 0xe588. *)
  let word_c_sw ~funct3 ~rs1 ~rs2 offset =
    Int64.logor
      (field 2 3 (Int64.of_int (Creg.field rs2)))
      (Int64.logor
         (field 5 1 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 6 1 (Int64.shift_right_logical offset 2))
            (Int64.logor
               (field 7 3 (Int64.of_int (Creg.field rs1)))
               (Int64.logor
                  (field 10 3 (Int64.shift_right_logical offset 3))
                  (field 13 3 (Int64.of_int funct3))))))

  let word_c_sd ~funct3 ~rs1 ~rs2 offset =
    Int64.logor
      (field 2 3 (Int64.of_int (Creg.field rs2)))
      (Int64.logor
         (field 5 2 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 7 3 (Int64.of_int (Creg.field rs1)))
            (Int64.logor
               (field 10 3 (Int64.shift_right_logical offset 3))
               (field 13 3 (Int64.of_int funct3)))))

  (* CI-format compressed SP-relative loads: quadrant(10) | offset bits | rd[11:7]
     (a real, un-biased 0..31 register - the base is fixed to x2/sp, which has no
     encoding field at all) | offset bit | funct3[15:13]. [c.lwsp]'s 6 raw offset
     bits scatter as [inst[3:2]|inst12|inst[6:4]] (confirmed against real
     riscv64-linux-gnu-as: `c.lwsp a0,4(sp)` -> word 0x4512). *)
  let word_c_lwsp ~funct3 ~rd offset =
    Int64.logor 0x2L
      (Int64.logor
         (field 2 1 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 3 1 (Int64.shift_right_logical offset 7))
            (Int64.logor
               (field 4 1 (Int64.shift_right_logical offset 2))
               (Int64.logor
                  (field 5 1 (Int64.shift_right_logical offset 3))
                  (Int64.logor
                     (field 6 1 (Int64.shift_right_logical offset 4))
                     (Int64.logor
                        (field 7 5 (Int64.of_int (Xreg.to_int rd)))
                        (Int64.logor
                           (field 12 1 (Int64.shift_right_logical offset 5))
                           (field 13 3 (Int64.of_int funct3)))))))))

  (* [c.ldsp] scatters its own 6 raw offset bits as [inst[4:2]|inst12|inst[6:5]] -
     a different swap from {!word_c_lwsp} (doubleword alignment shifts which raw
     bits are free). Confirmed: `c.ldsp a0,8(sp)` -> word 0x6522. *)
  let word_c_ldsp ~funct3 ~rd offset =
    Int64.logor 0x2L
      (Int64.logor
         (field 2 1 (Int64.shift_right_logical offset 6))
         (Int64.logor
            (field 3 1 (Int64.shift_right_logical offset 7))
            (Int64.logor
               (field 4 1 (Int64.shift_right_logical offset 8))
               (Int64.logor
                  (field 5 1 (Int64.shift_right_logical offset 3))
                  (Int64.logor
                     (field 6 1 (Int64.shift_right_logical offset 4))
                     (Int64.logor
                        (field 7 5 (Int64.of_int (Xreg.to_int rd)))
                        (Int64.logor
                           (field 12 1 (Int64.shift_right_logical offset 5))
                           (field 13 3 (Int64.of_int funct3)))))))))

  (* CSS-format compressed SP-relative stores: the store-side siblings of
     {!word_c_lwsp}/{!word_c_ldsp} - [rs2] (a real, un-biased register - stores
     never write back, so unlike [c.lwsp]'s [rd] there is no reserved x0 case)
     takes the 5-bit field at bits[6:2] instead of [11:7]. Confirmed: `c.swsp
     a0,4(sp)` -> word 0xc22a, `c.sdsp a0,8(sp)` -> word 0xe42a. *)
  let word_c_swsp ~funct3 ~rs2 offset =
    Int64.logor 0x2L
      (Int64.logor
         (field 2 5 (Int64.of_int (Xreg.to_int rs2)))
         (Int64.logor
            (field 7 1 (Int64.shift_right_logical offset 6))
            (Int64.logor
               (field 8 1 (Int64.shift_right_logical offset 7))
               (Int64.logor
                  (field 9 1 (Int64.shift_right_logical offset 2))
                  (Int64.logor
                     (field 10 1 (Int64.shift_right_logical offset 3))
                     (Int64.logor
                        (field 11 1 (Int64.shift_right_logical offset 4))
                        (Int64.logor
                           (field 12 1 (Int64.shift_right_logical offset 5))
                           (field 13 3 (Int64.of_int funct3)))))))))

  let word_c_sdsp ~funct3 ~rs2 offset =
    Int64.logor 0x2L
      (Int64.logor
         (field 2 5 (Int64.of_int (Xreg.to_int rs2)))
         (Int64.logor
            (field 7 1 (Int64.shift_right_logical offset 6))
            (Int64.logor
               (field 8 1 (Int64.shift_right_logical offset 7))
               (Int64.logor
                  (field 9 1 (Int64.shift_right_logical offset 8))
                  (Int64.logor
                     (field 10 1 (Int64.shift_right_logical offset 3))
                     (Int64.logor
                        (field 11 1 (Int64.shift_right_logical offset 4))
                        (Int64.logor
                           (field 12 1 (Int64.shift_right_logical offset 5))
                           (field 13 3 (Int64.of_int funct3)))))))))

  let bytes_of_word w =
    String.init 4 (fun i ->
        Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical w (8 * i)) 0xffL)))

  let bytes_of_half w =
    String.init 2 (fun i ->
        Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical w (8 * i)) 0xffL)))

  let mk_fixup ~kind ~name:fxname ~slices ~byte_offset ~container ~range ~value ~pairing =
    {
      Asm_core.Lowered_ast.kind;
      kind_name = fixup_kind_name kind;
      family = fixup_family kind;
      role = fixup_role kind;
      name = fxname;
      slices;
      byte_offset;
      container;
      pc_bias = 0;
      range;
      value;
      pairing;
      origin = Origin.synthesized ~pass:(P.name ^ ".encode") ();
    }

  let i_slices = [ { Asm_core.Lowered_ast.bit_offset = 20; bit_width = 12; value_lsb = 0 } ]

  let s_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 7; bit_width = 5; value_lsb = 0 };
      { bit_offset = 25; bit_width = 7; value_lsb = 5 };
    ]

  let b_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 7; bit_width = 1; value_lsb = 11 };
      { bit_offset = 8; bit_width = 4; value_lsb = 1 };
      { bit_offset = 25; bit_width = 6; value_lsb = 5 };
      { bit_offset = 31; bit_width = 1; value_lsb = 12 };
    ]

  let j_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 8; value_lsb = 12 };
      { bit_offset = 20; bit_width = 1; value_lsb = 11 };
      { bit_offset = 21; bit_width = 10; value_lsb = 1 };
      { bit_offset = 31; bit_width = 1; value_lsb = 20 };
    ]

  (* c.beqz/c.bnez's CB-format branch offset, {!b_slices}'s compressed sibling: a genuine
     bit scatter (not a plain shift), hand-verified against real riscv64-linux-gnu-as/objdump
     before writing any encoder code - see the GEN-05-RV-C follow-up 4 tracker entry. *)
  let cb_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 3; bit_width = 2; value_lsb = 1 };
      { bit_offset = 10; bit_width = 2; value_lsb = 3 };
      { bit_offset = 2; bit_width = 1; value_lsb = 5 };
      { bit_offset = 5; bit_width = 2; value_lsb = 6 };
      { bit_offset = 12; bit_width = 1; value_lsb = 8 };
    ]

  (* c.j/c.jal's CJ-format offset, imm[11|4|9:8|10|6|7|3:1|5] in bits 12..2 (RISC-V ISA manual;
     checked against real riscv32-linux-gnu-as: `c.j` +4 -> 0xa011, -2 -> 0xbffd). *)
  let cj_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 1; value_lsb = 11 };
      { bit_offset = 11; bit_width = 1; value_lsb = 4 };
      { bit_offset = 9; bit_width = 2; value_lsb = 8 };
      { bit_offset = 8; bit_width = 1; value_lsb = 10 };
      { bit_offset = 7; bit_width = 1; value_lsb = 6 };
      { bit_offset = 6; bit_width = 1; value_lsb = 7 };
      { bit_offset = 3; bit_width = 3; value_lsb = 1 };
      { bit_offset = 2; bit_width = 1; value_lsb = 5 };
    ]

  let u_slices = [ { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 20; value_lsb = 0 } ]
  let form bytes form fixups = { Asm_core.Lowered_ast.bytes; form; fixups }
  let bad_encode kind = Error (diag ~pos:__POS__ kind)

  let encode_ungated l =
    let fixed w f = Ok (`Fixed (form (bytes_of_word w) f [])) in
    let fields format lookup op k =
      match lookup op with Some d -> k d | None -> bad_encode (`Format_mismatch (op, format))
    in
    match l with
    | Lowered.Caddi x -> (
        match int64_expr x.imm with
        | Some imm when x.rd <> Xreg.zero && imm <> 0L && fits_signed 6 imm ->
            Ok (`Fixed (form (bytes_of_half (word_caddi ~rd:x.rd imm)) "c.addi" []))
        | Some _ -> bad_encode (`Immediate_range "c.addi")
        | None -> bad_encode (`Immediate_range ("c.addi " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.Cr2 x ->
        fields "CA" Desc.ca x.op @@ fun (funct6, funct2) ->
        Ok
          (`Fixed
             (form
                (bytes_of_half (word_cr2 ~funct6 ~funct2 ~rd_rs1:x.rd_rs1 ~rs2:x.rs2))
                (Opcode.name x.op) []))
    | Lowered.Cr x ->
        fields "CR" Desc.cr x.op @@ fun funct4 ->
        Ok
          (`Fixed
             (form
                (bytes_of_half (word_cr ~funct4 ~rd_rs1:x.rd_rs1 ~rs2:x.rs2))
                (Opcode.name x.op) []))
    | Lowered.Cl x -> (
        fields "CL" Desc.c_funct3 x.op @@ fun funct3 ->
        let bits, scale = if x.op = Opcode.C_lw then (7, 4L) else (8, 8L) in
        match int64_expr x.imm with
        | Some offset when fits_scaled_unsigned bits scale offset ->
            let word =
              if x.op = Opcode.C_lw then word_c_lw ~funct3 ~rd:x.rd ~rs1:x.rs1 offset
              else word_c_ld ~funct3 ~rd:x.rd ~rs1:x.rs1 offset
            in
            Ok (`Fixed (form (bytes_of_half word) (Opcode.name x.op) []))
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None ->
            bad_encode (`Immediate_range (Opcode.name x.op ^ " " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.Cs x -> (
        fields "CS" Desc.c_funct3 x.op @@ fun funct3 ->
        let bits, scale = if x.op = Opcode.C_sw then (7, 4L) else (8, 8L) in
        match int64_expr x.imm with
        | Some offset when fits_scaled_unsigned bits scale offset ->
            let word =
              if x.op = Opcode.C_sw then word_c_sw ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 offset
              else word_c_sd ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 offset
            in
            Ok (`Fixed (form (bytes_of_half word) (Opcode.name x.op) []))
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None ->
            bad_encode (`Immediate_range (Opcode.name x.op ^ " " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.Clsp x -> (
        fields "CI" Desc.c_funct3 x.op @@ fun funct3 ->
        let bits, scale = if x.op = Opcode.C_lwsp then (8, 4L) else (9, 8L) in
        match int64_expr x.imm with
        | Some offset when fits_scaled_unsigned bits scale offset ->
            let word =
              if x.op = Opcode.C_lwsp then word_c_lwsp ~funct3 ~rd:x.rd offset
              else word_c_ldsp ~funct3 ~rd:x.rd offset
            in
            Ok (`Fixed (form (bytes_of_half word) (Opcode.name x.op) []))
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None ->
            bad_encode (`Immediate_range (Opcode.name x.op ^ " " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.Css x -> (
        fields "CSS" Desc.c_funct3 x.op @@ fun funct3 ->
        let bits, scale = if x.op = Opcode.C_swsp then (8, 4L) else (9, 8L) in
        match int64_expr x.imm with
        | Some offset when fits_scaled_unsigned bits scale offset ->
            let word =
              if x.op = Opcode.C_swsp then word_c_swsp ~funct3 ~rs2:x.rs2 offset
              else word_c_sdsp ~funct3 ~rs2:x.rs2 offset
            in
            Ok (`Fixed (form (bytes_of_half word) (Opcode.name x.op) []))
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None ->
            bad_encode (`Immediate_range (Opcode.name x.op ^ " " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.Cj x ->
        fields "CJ" Desc.c_funct3 x.op @@ fun funct3 ->
        let fx =
          mk_fixup ~kind:Jump12c ~name:"target" ~slices:cj_slices ~byte_offset:0 ~container:2
            ~range:(Asm_core.Lowered_ast.Signed 12) ~value:x.target ~pairing:Unpaired
        in
        Ok
          (`Fixed
             (form
                (bytes_of_half (Int64.logor 0x1L (Int64.shift_left (Int64.of_int funct3) 13)))
                (Opcode.name x.op) [ fx ]))
    | Lowered.Cb x ->
        fields "CB" Desc.c_funct3 x.op @@ fun funct3 ->
        let fx =
          mk_fixup ~kind:Branch9c ~name:"target" ~slices:cb_slices ~byte_offset:0 ~container:2
            ~range:(Asm_core.Lowered_ast.Signed 9) ~value:x.target ~pairing:Unpaired
        in
        Ok (`Fixed (form (bytes_of_half (word_cb ~funct3 ~rs1:x.rs1)) (Opcode.name x.op) [ fx ]))
    | Lowered.R x -> (
        fields "R" Desc.r x.op @@ fun d ->
        let funct3 = match x.rm with Some rm -> Rm.to_funct3 rm | None -> d.funct3 in
        let rs2 =
          match (d.rs2, x.rs2) with
          | Some n, None -> Some n
          | None, Some r -> Some (Reg.index r)
          | Some _, Some _ | None, None -> None
        in
        match rs2 with
        | Some rs2 ->
            fixed
              (word_r ~opcode:d.opcode ~funct3 ~funct7:d.funct7 ~rd:x.rd ~rs1:x.rs1 ~rs2)
              (Opcode.name x.op)
        | None -> bad_encode (`Format_mismatch (x.op, "R")))
    | Lowered.Rbs x ->
        fields "R" Desc.rbs x.op @@ fun base ->
        fixed
          (word_r ~opcode:0x33 ~funct3:0
             ~funct7:((x.bs lsl 5) lor base)
             ~rd:x.rd ~rs1:x.rs1 ~rs2:(Reg.index x.rs2))
          (Opcode.name x.op)
    | Lowered.R4 x ->
        fields "R4" Desc.r4 x.op @@ fun (opcode, fmt) ->
        fixed
          (word_r4 ~opcode ~fmt ~funct3:(Rm.to_funct3 x.rm) ~rd:x.rd ~rs1:x.rs1 ~rs2:x.rs2
             ~rs3:x.rs3)
          (Opcode.name x.op)
    | I x -> (
        fields "I" Desc.i x.op @@ fun d ->
        match int64_expr x.imm with
        | Some imm -> (
            let encoded_imm =
              match d.shamt_bits with
              | None -> if fits_signed 12 imm then Some imm else None
              | Some width ->
                  let bits = if width = 64 then 6 else 5 in
                  if Int64.compare imm 0L >= 0 && Int64.compare imm (Int64.shift_left 1L bits) < 0
                  then Some (Int64.logor imm (Int64.shift_left (Int64.of_int d.funct_hi) bits))
                  else None
            in
            match encoded_imm with
            | Some imm ->
                fixed
                  (word_i ~opcode:d.opcode ~funct3:d.funct3 ~rd:x.rd ~rs1:x.rs1 imm)
                  (Opcode.name x.op)
            | None ->
                bad_encode
                  (`Immediate_range
                     (Printf.sprintf "%s %s (%Ld)" (Opcode.name x.op)
                        (Asm_core.Expr.to_string x.imm) imm)))
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_lo", Asm_core.Expr.Symbol anchor) ->
                let kind = Pcrel_lo12_i in
                let fx =
                  mk_fixup ~kind ~name:"lo" ~slices:i_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:(Asm_core.Expr.Symbol anchor)
                    ~pairing:(Asm_core.Lowered_ast.Pair_tail (Anchor_symbol anchor))
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_i ~opcode:d.opcode ~funct3:d.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                let fx =
                  mk_fixup ~kind:Abs_lo12_i ~name:"lo" ~slices:i_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_i ~opcode:d.opcode ~funct3:d.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ ->
                bad_encode
                  (`Immediate_range (Opcode.name x.op ^ " " ^ Asm_core.Expr.to_string x.imm))))
    | S x -> (
        fields "S" Desc.s x.op @@ fun (opcode, funct3) ->
        match int64_expr x.imm with
        | Some imm when fits_signed 12 imm ->
            fixed (word_s ~opcode ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 imm) (Opcode.name x.op)
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_lo", Asm_core.Expr.Symbol anchor) ->
                let fx =
                  mk_fixup ~kind:Pcrel_lo12_s ~name:"lo" ~slices:s_slices ~byte_offset:0
                    ~container:4 ~range:(Asm_core.Lowered_ast.Signed 12)
                    ~value:(Asm_core.Expr.Symbol anchor)
                    ~pairing:(Asm_core.Lowered_ast.Pair_tail (Anchor_symbol anchor))
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word (word_s ~opcode ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                (* [sw a0,%lo(sym)(s6)] - the store-side sibling of [I]'s own
                   [%lo] case just above (`la`'s non-PIC absolute [lui]+[addi]
                   pair, with [s6] already holding [%hi(sym)] from an earlier
                   instruction this project's own single-pass model resolves
                   independently): a real gap, not a dead branch - {!Abs_lo12_s}
                   already existed in {!fixup_kind} and in [evaluate_fixup]
                   below, but [S]'s own [encode] case never produced one (M5,
                   the corpus notes - helpers/riscv.c's own absolute
                   store to a page-local static, the first fixture to spell an
                   [S]-type absolute [%lo]). Checked against real
                   riscv64-linux-gnu-as: `sw a0,%lo(control_alias)(s6)` still
                   assembles to a placeholder-zero S-type word plus an
                   `R_RISCV_LO12_S` relocation, the identical shape [I]'s own
                   [Abs_lo12_i] already produces for a load/[addi]. *)
                let fx =
                  mk_fixup ~kind:Abs_lo12_s ~name:"lo" ~slices:s_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word (word_s ~opcode ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range (Opcode.name x.op))))
    | B x ->
        fields "B" Desc.b x.op @@ fun funct3 ->
        let fx =
          mk_fixup ~kind:Branch13 ~name:"target" ~slices:b_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 13) ~value:x.target ~pairing:Unpaired
        in
        Ok
          (`Fixed
             (form
                (bytes_of_word (word_b ~funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                (Opcode.name x.op) [ fx ]))
    | J x ->
        let fx =
          mk_fixup ~kind:Jal21 ~name:"target" ~slices:j_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 21) ~value:x.target ~pairing:Unpaired
        in
        Ok (`Fixed (form (bytes_of_word (word_j ~rd:x.rd 0L)) "jal" [ fx ]))
    | U x -> (
        fields "U" Desc.u x.op @@ fun opcode ->
        match int64_expr x.imm with
        | Some imm when fits_signed 20 imm || Int64.compare imm 0xfffffL <= 0 ->
            fixed (word_u ~opcode ~rd:x.rd imm) (Opcode.name x.op)
        | Some _ -> bad_encode (`Immediate_range (Opcode.name x.op))
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_hi", target) ->
                let fx =
                  mk_fixup ~kind:Pcrel_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target
                    ~pairing:(Pair_head "pcrel")
                in
                Ok
                  (`Fixed
                     (form (bytes_of_word (word_u ~opcode ~rd:x.rd 0L)) (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier ("%hi", target) ->
                let fx =
                  mk_fixup ~kind:Abs_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form (bytes_of_word (word_u ~opcode ~rd:x.rd 0L)) (Opcode.name x.op) [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range (Opcode.name x.op))))
    | Pair x ->
        let lo_opcode, lo_funct3 =
          match x.kind with
          | Addi -> (0x13, 0)
          | Jalr -> (0x67, 0)
          | Load funct3 -> (0x03, funct3)
          | Fload funct3 -> (0x07, funct3)
        in
        let hiword = word_u ~opcode:0x17 ~rd:x.tmp 0L in
        let lowword = word_i ~opcode:lo_opcode ~funct3:lo_funct3 ~rd:x.rd ~rs1:(Reg.X x.tmp) 0L in
        let hi_kind, lo_kind =
          match x.kind with
          | Addi | Load _ | Fload _ -> (Pcrel_hi20, Pcrel_lo12_i)
          | Jalr -> (Call_hi20, Call_lo12_i)
        in
        let hi =
          mk_fixup ~kind:hi_kind ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:x.target ~pairing:(Pair_head "pair")
        in
        let lo =
          mk_fixup ~kind:lo_kind ~name:"lo" ~slices:i_slices ~byte_offset:4 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 12) ~value:x.target
            ~pairing:(Pair_tail (Sibling_key "pair"))
        in
        Ok
          (`Fixed
             (form (bytes_of_word hiword ^ bytes_of_word lowword) (Opcode.name x.op) [ hi; lo ]))
    | Fixed x -> fixed x.word (Lowered.fixed_name x.op x.word)
    | Fixed_half x -> Ok (`Fixed (form (bytes_of_half x.word) (Opcode.name x.op) []))

  (* The opcode a lowered form is gated by. Every constructor is listed, so a new one has to
     say which instruction it is. *)
  let lowered_op = function
    | Lowered.R { op; _ }
    | Rbs { op; _ }
    | R4 { op; _ }
    | I { op; _ }
    | S { op; _ }
    | B { op; _ }
    | U { op; _ }
    | Pair { op; _ }
    | Fixed { op; _ }
    | Fixed_half { op; _ }
    | Cr2 { op; _ }
    | Cr { op; _ }
    | Cl { op; _ }
    | Cs { op; _ }
    | Clsp { op; _ }
    | Css { op; _ }
    | Cj { op; _ }
    | Cb { op; _ } ->
        op
    | J _ -> Opcode.Jal
    | Caddi _ -> Opcode.C_addi

  (* Encoding under a configuration, so a caller that builds lowered forms directly cannot
     emit a disabled component's instruction. [encode] is the default-configuration wrapper. *)
  (* Without Zve64x ELEN is 32: SEW 64 sets vill and LMUL 1/8 is reserved. Only the immediate
     vtype forms can be checked. *)
  let vtype_gate state = function
    | Lowered.Fixed { op = Table _ as op; word }
      when (match Opcode.name op with "vsetvli" | "vsetivli" -> true | _ -> false)
           && not (Target_config.enabled state.config "zve64x") -> (
        let name = Opcode.name op in
        let vtype = Int64.to_int (Int64.shift_right_logical word 20) land 0xff in
        let refuse setting =
          Error (diag ~pos:__POS__ (`Feature_disabled (name ^ " " ^ setting, "zve64x")))
        in
        match ((vtype lsr 3) land 7, vtype land 7) with
        | 3, _ -> refuse "e64"
        | _, 5 -> refuse "mf8"
        | _ -> Ok ())
    | _ -> Ok ()

  let encode_in state l =
    match Result.bind (feature_gate state (lowered_op l)) (fun () -> vtype_gate state l) with
    | Error _ as e -> e
    | Ok () -> encode_ungated l

  let encode l = encode_in default_state l

  let sign_extend bits v =
    let s = 64 - bits in
    Int64.shift_right (Int64.shift_left v s) s

  let bits w shift width = mask width (Int64.shift_right_logical w shift)

  let read_word s pos =
    let w = ref 0L in
    for i = 0 to 3 do
      w := Int64.logor !w (Int64.shift_left (Int64.of_int (Char.code s.[pos + i])) (8 * i))
    done;
    !w

  let read_half s pos =
    Int64.logor
      (Int64.of_int (Char.code s.[pos]))
      (Int64.shift_left (Int64.of_int (Char.code s.[pos + 1])) 8)

  let reg n = Operand.Reg (Reg.x (n land 31))
  let f_operand n = Operand.Reg (Reg.f (n land 31))
  let imm n = Operand.Imm (Bigint.of_int64 n)
  let sym n = Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 n))

  let mem rs1 n =
    Operand.Mem { Mem.base = Reg.x (rs1 land 31); offset = Asm_core.Expr.Const (Bigint.of_int64 n) }

  let op_exn s = Option.get (Opcode.of_mnemonic s)
  let instruction op ops = { Instruction.op; ops }

  (* The M component's rows decode from the same table they encode from. *)
  let m_name opcode f3 f7 =
    List.find_map
      (fun (f : Riscv_ext_m.form) ->
        if f.opcode = opcode && f.funct3 = f3 && f.funct7 = f7 && ((not f.rv64_only) || xlen = 64)
        then Some f.mnemonic
        else None)
      Riscv_ext_m.forms

  let r_name opcode f3 f7 =
    match m_name opcode f3 f7 with
    | Some _ as m -> m
    | None -> (
        match (opcode, f3, f7) with
        | 0x33, 0, 0 -> Some "add"
        | 0x33, 0, 0x20 -> Some "sub"
        | 0x33, 1, 0 -> Some "sll"
        | 0x33, 2, 0 -> Some "slt"
        | 0x33, 3, 0 -> Some "sltu"
        | 0x33, 4, 0 -> Some "xor"
        | 0x33, 5, 0 -> Some "srl"
        | 0x33, 5, 0x20 -> Some "sra"
        | 0x33, 6, 0 -> Some "or"
        | 0x33, 7, 0 -> Some "and"
        | 0x33, 2, 0x10 -> Some "sh1add"
        | 0x33, 4, 0x10 -> Some "sh2add"
        | 0x33, 6, 0x10 -> Some "sh3add"
        | 0x33, 4, 0x05 -> Some "min"
        | 0x33, 5, 0x05 -> Some "minu"
        | 0x33, 6, 0x05 -> Some "max"
        | 0x33, 7, 0x05 -> Some "maxu"
        | 0x33, 7, 0x20 -> Some "andn"
        | 0x33, 6, 0x20 -> Some "orn"
        | 0x33, 4, 0x20 -> Some "xnor"
        | 0x33, 1, 0x30 -> Some "rol"
        | 0x33, 5, 0x30 -> Some "ror"
        | 0x3b, 2, 0x10 -> Some "sh1add.uw"
        | 0x3b, 4, 0x10 -> Some "sh2add.uw"
        | 0x3b, 6, 0x10 -> Some "sh3add.uw"
        | 0x3b, 0, 0 -> Some "addw"
        | 0x3b, 0, 0x20 -> Some "subw"
        | 0x3b, 1, 0 -> Some "sllw"
        | 0x3b, 5, 0 -> Some "srlw"
        | 0x3b, 5, 0x20 -> Some "sraw"
        | 0x33, 4, 0x04 -> Some "pack"
        | 0x33, 7, 0x04 -> Some "packh"
        | 0x3b, 4, 0x04 -> Some "packw"
        | 0x3b, 1, 0x30 -> Some "rolw"
        | 0x3b, 5, 0x30 -> Some "rorw"
        | _ -> None)

  (* OP-FP (opcode [0x53]): unlike every integer R-type above, several of these
     mnemonics also need [rs2] to disambiguate (a convert or move fixes its second
     "operand" to a selector rather than reading a real register - see
     {!f_shape}, which is what actually decides whether [rs2] is printed at
     all). This always resolves the sign-injection group to its general
     [fsgnj]/[fsgnjn]/[fsgnjx] name; the caller downgrades to the [fneg.d]/[fneg.s]/
     [fmv.d] pseudo names when [rs1 = rs2], matching real hardware's own alias. *)
  let f_r_name f3 f7 rs2 =
    (* The arithmetic and convert families' [f3] is a rounding mode, not part of a
       mnemonic's own identity (an explicit non-default one is a real, separate operand -
       {!freg_shape}'s [rm] and [rm_suffix] print it, not this lookup) - so these match on
       [f7]/[rs2] alone, before the [f3]-sensitive compares/pseudo-moves below ever see
       them. The two groups' [f7] values never overlap. *)
    match (f7, rs2) with
    | 0x00, _ -> Some "fadd.s"
    | 0x01, _ -> Some "fadd.d"
    | 0x04, _ -> Some "fsub.s"
    | 0x05, _ -> Some "fsub.d"
    | 0x08, _ -> Some "fmul.s"
    | 0x09, _ -> Some "fmul.d"
    | 0x0c, _ -> Some "fdiv.s"
    | 0x0d, _ -> Some "fdiv.d"
    | 0x61, 0 -> Some "fcvt.w.d"
    | 0x61, 1 -> Some "fcvt.wu.d"
    | 0x61, 2 -> Some "fcvt.l.d"
    | 0x61, 3 -> Some "fcvt.lu.d"
    | 0x69, 0 -> Some "fcvt.d.w"
    | 0x69, 1 -> Some "fcvt.d.wu"
    | 0x69, 2 -> Some "fcvt.d.l"
    | 0x69, 3 -> Some "fcvt.d.lu"
    | 0x68, 0 -> Some "fcvt.s.w"
    | 0x68, 1 -> Some "fcvt.s.wu"
    | 0x68, 2 -> Some "fcvt.s.l"
    | 0x68, 3 -> Some "fcvt.s.lu"
    | 0x60, 0 -> Some "fcvt.w.s"
    | 0x60, 1 -> Some "fcvt.wu.s"
    | 0x60, 2 -> Some "fcvt.l.s"
    | 0x60, 3 -> Some "fcvt.lu.s"
    | 0x78, 0 -> Some "fmv.w.x"
    | 0x20, 1 -> Some "fcvt.s.d"
    | 0x21, 0 -> Some "fcvt.d.s"
    | 0x2c, _ -> Some "fsqrt.s"
    | 0x2d, _ -> Some "fsqrt.d"
    | _ -> (
        match (f3, f7, rs2) with
        | 0, 0x10, _ -> Some "fsgnj.s"
        | 1, 0x10, _ -> Some "fsgnjn.s"
        | 2, 0x10, _ -> Some "fsgnjx.s"
        | 0, 0x11, _ -> Some "fsgnj.d"
        | 1, 0x11, _ -> Some "fsgnjn.d"
        | 2, 0x11, _ -> Some "fsgnjx.d"
        | 0, 0x14, _ -> Some "fmin.s"
        | 1, 0x14, _ -> Some "fmax.s"
        | 0, 0x15, _ -> Some "fmin.d"
        | 1, 0x15, _ -> Some "fmax.d"
        | 0, 0x51, _ -> Some "fle.d"
        | 2, 0x51, _ -> Some "feq.d"
        | 1, 0x51, _ -> Some "flt.d"
        | 1, 0x50, _ -> Some "flt.s"
        | 2, 0x50, _ -> Some "feq.s"
        | 0, 0x50, _ -> Some "fle.s"
        | 0, 0x71, 0 -> Some "fmv.x.d"
        | 0, 0x70, 0 -> Some "fmv.x.w"
        | 1, 0x70, 0 -> Some "fclass.s"
        | 1, 0x71, 0 -> Some "fclass.d"
        | _ -> None)

  (* CA-format decode: the inverse of {!ca_desc}, keyed by (funct6, funct2). *)
  let ca_name = function
    | 0x23, 0 -> Some "c.sub"
    | 0x23, 1 -> Some "c.xor"
    | 0x23, 2 -> Some "c.or"
    | 0x23, 3 -> Some "c.and"
    | 0x27, 0 -> Some "c.subw"
    | 0x27, 1 -> Some "c.addw"
    | _ -> None

  let f_load_name = function 2 -> Some "flw" | 3 -> Some "fld" | _ -> None
  let f_store_name = function 2 -> Some "fsw" | 3 -> Some "fsd" | _ -> None

  (* [fmadd]/[fmsub]/[fnmsub]/[fnmadd] - R4-type, its own base opcode per
     mnemonic rather than OP-FP's shared [0x53]; see {!f_fma_desc}. *)
  let f_r4_name opcode fmt =
    match (opcode, fmt) with
    | 0x43, 0 -> Some "fmadd.s"
    | 0x43, 1 -> Some "fmadd.d"
    | 0x47, 0 -> Some "fmsub.s"
    | 0x47, 1 -> Some "fmsub.d"
    | 0x4b, 0 -> Some "fnmsub.s"
    | 0x4b, 1 -> Some "fnmsub.d"
    | 0x4f, 0 -> Some "fnmadd.s"
    | 0x4f, 1 -> Some "fnmadd.d"
    | _ -> None

  type decode_context = { state : target_state; address : int64 }

  (* DEC-RV-TABLE: tried only after the hand-written decoder finds nothing, so a
     generated row never shadows a hand-written form. *)
  let table_matches ?(half = false) ?(legal = fun _ -> true) ~first_only w =
    let rows = table_rows in
    let field lsb width = bits w lsb width in
    let unscatter (sc : Riscv_table_row.scatter) =
      let v =
        List.fold_left
          (fun acc (ibit, vbit) -> Int64.logor acc (Int64.shift_left (field ibit 1) vbit))
          0L sc.bits
      in
      let v = if sc.signed then sign_extend sc.width v else v in
      if sc.nonzero && v = 0L then None else Some v
    in
    let operand (o : Riscv_table_row.operand) =
      match o with
      | Gpr { lsb; nonzero } ->
          let n = Int64.to_int (field lsb 5) in
          if nonzero && n = 0 then None else Some (Some (reg n))
      | Fpr { lsb } -> Some (Some (f_operand (Int64.to_int (field lsb 5))))
      | Uimm { lsb; width } -> Some (Some (imm (field lsb width)))
      | Simm { lsb; width } -> Some (Some (imm (sign_extend width (field lsb width))))
      | Fixed_gpr n -> Some (Some (reg n))
      | Rm { lsb; default } ->
          let m = Int64.to_int (field lsb 3) in
          if m = default then Some None
          else
            Option.map
              (fun r -> Some (Operand.Sym (Asm_core.Expr.Symbol (Rm.name r))))
              (Rm.of_funct3 m)
      | Tied _ -> Some None
      | Keyword k -> Some (Some (Operand.Sym (Asm_core.Expr.Symbol k)))
      | Fli { lsb } ->
          Some
            (Some
               (Operand.Sym
                  (Asm_core.Expr.Symbol Riscv_table_row.fli_constants.(Int64.to_int (field lsb 5)))))
      | Mem_i { base } ->
          Some (Some (mem (Int64.to_int (field base 5)) (sign_extend 12 (field 20 12))))
      | Mem_zero { base } -> Some (Some (mem (Int64.to_int (field base 5)) 0L))
      | Mem_hi { base } ->
          Some
            (Some
               (mem
                  (Int64.to_int (field base 5))
                  (sign_extend 12 (Int64.shift_left (field 25 7) 5))))
      | Gpr_pair { lsb; below } ->
          let n = Int64.to_int (field lsb 5) in
          if xlen < below && n land 1 = 1 then None else Some (Some (reg n))
      | Creg { lsb } -> Some (Some (reg (8 + Int64.to_int (field lsb 3))))
      | Cfreg { lsb } -> Some (Some (f_operand (8 + Int64.to_int (field lsb 3))))
      | Gpr_except { lsb; except } ->
          let n = Int64.to_int (field lsb 5) in
          if List.mem n except then None else Some (Some (reg n))
      | Scatter sc -> Option.map (fun v -> Some (imm v)) (unscatter sc)
      | Cmem { base; offset } ->
          Option.map (fun v -> Some (mem (8 + Int64.to_int (field base 3)) v)) (unscatter offset)
      | Spmem { offset } -> Option.map (fun v -> Some (mem 2 v)) (unscatter offset)
      | Cui { hi; lo } ->
          let v6 = Int64.logor (Int64.shift_left (field hi 1) 5) (field lo 5) in
          if v6 = 0L then None
          else Some (Some (imm (if v6 >= 32L then Int64.logor 0xfffc0L v6 else v6)))
      | Sreg { lsb } ->
          Some (Some (reg (Riscv_table_row.sreg_register (Int64.to_int (field lsb 3)))))
      | Rlist { lsb } ->
          Option.map
            (fun t -> Some (Operand.Sym (Asm_core.Expr.Symbol t)))
            (Riscv_table_row.rlist_text (Int64.to_int (field lsb 4)))
      | Stack_adj { rlist; spimm; push } ->
          let code = Int64.to_int (field rlist 4) in
          if code < 4 then None
          else
            let v = Riscv_table_row.rlist_base ~xlen code + (16 * Int64.to_int (field spimm 2)) in
            Some (Some (imm (Int64.of_int (if push then -v else v))))
      | Uimm_min { lsb; width; min } ->
          let v = field lsb width in
          if Int64.to_int v < min then None else Some (Some (imm v))
      | Mem_s { base } ->
          let v = Int64.logor (Int64.shift_left (field 25 7) 5) (field 7 5) in
          Some (Some (mem (Int64.to_int (field base 5)) (sign_extend 12 v)))
      | Vreg { lsb } -> Some (Some (Operand.Reg (Reg.v (Int64.to_int (field lsb 5)))))
      | Vm { lsb } ->
          if field lsb 1 = 1L then Some None
          else Some (Some (Operand.Sym (Asm_core.Expr.Symbol "v0.t")))
      | Fixed_vreg n -> Some (Some (Operand.Reg (Reg.v n)))
      | Vtype _ -> None
      | Indexed { prefix; lsb; width; scale } ->
          let n = Int64.mul (field lsb width) (Int64.of_int scale) in
          Some (Some (Operand.Sym (Asm_core.Expr.Symbol (prefix ^ Int64.to_string n))))
    in
    (* Every operand's spelling; a vtype is its four keywords, and a reserved one has none. *)
    let spelling (o : Riscv_table_row.operand) =
      match o with
      | Vtype { lsb; width } ->
          Option.map
            (List.map (fun w -> Operand.Sym (Asm_core.Expr.Symbol w)))
            (Riscv_table_row.keywords_of_vtype (Int64.to_int (field lsb width)))
      | o -> Option.map Option.to_list (operand o)
    in
    (* A [Tied] field must repeat the register before it. *)
    let tied_ok (r : Riscv_table_row.row) =
      let rec go prev = function
        | [] -> true
        | Riscv_table_row.Tied { lsb } :: rest -> (
            match prev with
            | Some p -> Int64.equal (field lsb 5) (field p 5) && go prev rest
            | None -> false)
        | (Riscv_table_row.Gpr { lsb; _ } | Fpr { lsb }) :: rest -> go (Some lsb) rest
        | _ :: rest -> go prev rest
      in
      go None r.operands
    in
    let rec go i found =
      if i >= Array.length rows then List.rev found
      else
        let r = rows.(i) in
        if
          table_row_applies r && legal (Opcode.Table i)
          && Int64.logand r.match_ 3L <> 3L = half
          && Int64.equal (Int64.logand w r.mask) r.match_
          && tied_ok r
        then
          let ops = List.map spelling r.operands in
          if List.mem None ops then go (i + 1) found
          else
            let m =
              (instruction (Opcode.Table i) (List.concat (List.filter_map Fun.id ops)), r.mnemonic)
            in
            if first_only then [ m ] else go (i + 1) (m :: found)
        else go (i + 1) found
    in
    go 0 []

  let table_decode ?half ?legal w =
    match table_matches ?half ?legal ~first_only:true w with m :: _ -> Some m | [] -> None

  let decode_hand_written ctx bytes ~pos =
    if String.length bytes - pos < 2 then Error (diag ~pos:__POS__ `Decode_short)
    else if Char.code bytes.[pos] land 3 <> 3 then
      let half = read_half bytes pos in
      let quadrant = Int64.to_int (bits half 0 2) in
      let funct3 = Int64.to_int (bits half 13 3) in
      let rd = Int64.to_int (bits half 7 5) in
      let imm_value =
        sign_extend 6 (Int64.logor (bits half 2 5) (Int64.shift_left (bits half 12 1) 5))
      in
      let funct6 = Int64.to_int (bits half 10 6) in
      let funct2 = Int64.to_int (bits half 5 2) in
      let rd_rs1_c = Int64.to_int (bits half 7 3) + 8 in
      let rs2_c = Int64.to_int (bits half 2 3) + 8 in
      let b12 = Int64.to_int (bits half 12 1) in
      let rs2_full = Int64.to_int (bits half 2 5) in
      let c_lw_sw_offset () =
        Int64.logor
          (Int64.shift_left (bits half 5 1) 6)
          (Int64.logor (Int64.shift_left (bits half 10 3) 3) (Int64.shift_left (bits half 6 1) 2))
      in
      let c_ld_sd_offset () =
        Int64.logor (Int64.shift_left (bits half 5 2) 6) (Int64.shift_left (bits half 10 3) 3)
      in
      let c_lwsp_ldsp_offset ~hi3_shift =
        Int64.logor
          (Int64.shift_left (bits half 2 1) 6)
          (Int64.logor
             (Int64.shift_left (bits half 3 1) 7)
             (Int64.logor
                (Int64.shift_left (bits half 4 1) hi3_shift)
                (Int64.logor
                   (Int64.shift_left (bits half 5 1) 3)
                   (Int64.logor
                      (Int64.shift_left (bits half 6 1) 4)
                      (Int64.shift_left (bits half 12 1) 5)))))
      in
      let c_swsp_sdsp_offset ~hi3_shift =
        Int64.logor
          (Int64.shift_left (bits half 7 1) 6)
          (Int64.logor
             (Int64.shift_left (bits half 8 1) 7)
             (Int64.logor
                (Int64.shift_left (bits half 9 1) hi3_shift)
                (Int64.logor
                   (Int64.shift_left (bits half 10 1) 3)
                   (Int64.logor
                      (Int64.shift_left (bits half 11 1) 4)
                      (Int64.shift_left (bits half 12 1) 5)))))
      in
      let c_j_offset () =
        let b pos = bits half pos 1 in
        sign_extend 12
          (List.fold_left Int64.logor 0L
             [
               Int64.shift_left (b 12) 11;
               Int64.shift_left (b 11) 4;
               Int64.shift_left (bits half 9 2) 8;
               Int64.shift_left (b 8) 10;
               Int64.shift_left (b 7) 6;
               Int64.shift_left (b 6) 7;
               Int64.shift_left (bits half 3 3) 1;
               Int64.shift_left (b 2) 5;
             ])
      in
      let c_beqz_bnez_offset () =
        sign_extend 9
          (Int64.logor
             (Int64.shift_left (bits half 12 1) 8)
             (Int64.logor
                (Int64.shift_left (bits half 5 2) 6)
                (Int64.logor
                   (Int64.shift_left (bits half 2 1) 5)
                   (Int64.logor
                      (Int64.shift_left (bits half 10 2) 3)
                      (Int64.shift_left (bits half 3 2) 1)))))
      in
      match (quadrant, funct3, rd, imm_value) with
      | 1, 0, rd, imm_value when rd <> 0 && imm_value <> 0L ->
          Ok (instruction Opcode.C_addi [ reg rd; imm imm_value ], "c.addi", 2)
      | 1, 4, _, _ -> (
          match ca_name (funct6, funct2) with
          | Some n -> Ok (instruction (op_exn n) [ reg rd_rs1_c; reg rs2_c ], n, 2)
          | None -> Error (diag ~pos:__POS__ `Decode_no_match))
      | 1, 5, _, _ ->
          Ok (instruction Opcode.C_j [ sym (Int64.add ctx.address (c_j_offset ())) ], "c.j", 2)
      | 1, 1, _, _ when xlen = 32 ->
          Ok (instruction Opcode.C_jal [ sym (Int64.add ctx.address (c_j_offset ())) ], "c.jal", 2)
      | 1, 6, _, _ ->
          Ok
            ( instruction Opcode.C_beqz
                [ reg rd_rs1_c; sym (Int64.add ctx.address (c_beqz_bnez_offset ())) ],
              "c.beqz",
              2 )
      | 1, 7, _, _ ->
          Ok
            ( instruction Opcode.C_bnez
                [ reg rd_rs1_c; sym (Int64.add ctx.address (c_beqz_bnez_offset ())) ],
              "c.bnez",
              2 )
      | 2, 4, rd, _ -> (
          match (b12, rs2_full, rd) with
          | 0, 0, 0 -> Error (diag ~pos:__POS__ `Decode_no_match)
          | 0, 0, rd -> Ok (instruction Opcode.C_jr [ reg rd ], "c.jr", 2)
          | 0, rs2, rd -> Ok (instruction Opcode.C_mv [ reg rd; reg rs2 ], "c.mv", 2)
          | 1, 0, 0 -> Ok (instruction Opcode.C_ebreak [], "c.ebreak", 2)
          | 1, 0, rd -> Ok (instruction Opcode.C_jalr [ reg rd ], "c.jalr", 2)
          | _, rs2, rd -> Ok (instruction Opcode.C_add [ reg rd; reg rs2 ], "c.add", 2))
      | 0, 2, _, _ ->
          Ok (instruction Opcode.C_lw [ reg rs2_c; mem rd_rs1_c (c_lw_sw_offset ()) ], "c.lw", 2)
      | 0, 3, _, _ when xlen = 64 ->
          Ok (instruction Opcode.C_ld [ reg rs2_c; mem rd_rs1_c (c_ld_sd_offset ()) ], "c.ld", 2)
      | 0, 6, _, _ ->
          Ok (instruction Opcode.C_sw [ reg rs2_c; mem rd_rs1_c (c_lw_sw_offset ()) ], "c.sw", 2)
      | 0, 7, _, _ when xlen = 64 ->
          Ok (instruction Opcode.C_sd [ reg rs2_c; mem rd_rs1_c (c_ld_sd_offset ()) ], "c.sd", 2)
      | 2, 2, rd, _ when rd <> 0 ->
          Ok
            ( instruction Opcode.C_lwsp [ reg rd; mem 2 (c_lwsp_ldsp_offset ~hi3_shift:2) ],
              "c.lwsp",
              2 )
      | 2, 3, rd, _ when xlen = 64 && rd <> 0 ->
          Ok
            ( instruction Opcode.C_ldsp [ reg rd; mem 2 (c_lwsp_ldsp_offset ~hi3_shift:8) ],
              "c.ldsp",
              2 )
      | 2, 6, _, _ ->
          Ok
            ( instruction Opcode.C_swsp [ reg rs2_full; mem 2 (c_swsp_sdsp_offset ~hi3_shift:2) ],
              "c.swsp",
              2 )
      | 2, 7, _, _ when xlen = 64 ->
          Ok
            ( instruction Opcode.C_sdsp [ reg rs2_full; mem 2 (c_swsp_sdsp_offset ~hi3_shift:8) ],
              "c.sdsp",
              2 )
      | _ -> (
          match table_decode ~half:true half with
          | Some (i, f) -> Ok (i, f, 2)
          | None -> Error (diag ~pos:__POS__ `Decode_no_match))
    else if String.length bytes - pos < 4 then Error (diag ~pos:__POS__ `Decode_short)
    else
      let w = read_word bytes pos in
      let opc = Int64.to_int (bits w 0 7) in
      let rd = Int64.to_int (bits w 7 5) and f3 = Int64.to_int (bits w 12 3) in
      let rs1 = Int64.to_int (bits w 15 5) and rs2 = Int64.to_int (bits w 20 5) in
      let f7 = Int64.to_int (bits w 25 7) in
      let result =
        match r_name opc f3 f7 with
        | Some n when opc <> 0x3b || xlen = 64 ->
            Some (instruction (op_exn n) [ reg rd; reg rs1; reg rs2 ], n)
        | _ -> (
            match opc with
            | (0x13 | 0x1b) when opc = 0x13 || xlen = 64 ->
                let raw = bits w 20 12 in
                let funct_shift =
                  if opc = 0x13 && xlen = 64 then Int64.to_int (bits w 26 6)
                  else Int64.to_int (bits w 25 7)
                in
                let n =
                  match (opc, f3, funct_shift) with
                  | 0x13, 0, _ -> Some "addi"
                  | 0x13, 2, _ -> Some "slti"
                  | 0x13, 3, _ -> Some "sltiu"
                  | 0x13, 4, _ -> Some "xori"
                  | 0x13, 6, _ -> Some "ori"
                  | 0x13, 7, _ -> Some "andi"
                  | 0x13, 1, 0 -> Some "slli"
                  | 0x13, 5, 0 -> Some "srli"
                  | 0x13, 5, x when x = if xlen = 64 then 0x10 else 0x20 -> Some "srai"
                  | 0x13, 5, x when x = if xlen = 64 then 0x18 else 0x30 -> Some "rori"
                  | 0x1b, 0, _ -> Some "addiw"
                  | 0x1b, 1, 0 -> Some "slliw"
                  | 0x1b, 5, 0 -> Some "srliw"
                  | 0x1b, 5, 0x20 -> Some "sraiw"
                  | 0x1b, 5, 0x30 -> Some "roriw"
                  | 0x13, 1, _ when Int64.equal raw 0x600L -> Some "clz"
                  | 0x13, 1, _ when Int64.equal raw 0x601L -> Some "ctz"
                  | 0x13, 1, _ when Int64.equal raw 0x602L -> Some "cpop"
                  | 0x13, 1, _ when Int64.equal raw 0x604L -> Some "sext.b"
                  | 0x13, 1, _ when Int64.equal raw 0x605L -> Some "sext.h"
                  | 0x13, 5, _ when Int64.equal raw 0x287L -> Some "orc.b"
                  | 0x1b, 1, _ when Int64.equal raw 0x600L -> Some "clzw"
                  | 0x1b, 1, _ when Int64.equal raw 0x601L -> Some "ctzw"
                  | 0x1b, 1, _ when Int64.equal raw 0x602L -> Some "cpopw"
                  | 0x13, 5, _ when Int64.equal raw 0x687L -> Some "brev8"
                  | 0x13, 5, _ when Int64.equal raw (if xlen = 64 then 0x6b8L else 0x698L) ->
                      Some "rev8"
                  | 0x13, 1, _ when xlen = 32 && Int64.equal raw 0x08fL -> Some "zip"
                  | 0x13, 5, _ when xlen = 32 && Int64.equal raw 0x08fL -> Some "unzip"
                  | _ -> None
                in
                Option.map
                  (fun n ->
                    match n with
                    | "clz" | "ctz" | "cpop" | "sext.b" | "sext.h" | "orc.b" | "clzw" | "ctzw"
                    | "cpopw" | "brev8" | "rev8" | "zip" | "unzip" ->
                        (instruction (op_exn n) [ reg rd; reg rs1 ], n)
                    | _ ->
                        let shift = f3 = 1 || f3 = 5 in
                        let width = if opc = 0x1b then 5 else if xlen = 64 then 6 else 5 in
                        let v = if shift then bits w 20 width else sign_extend 12 raw in
                        (instruction (op_exn n) [ reg rd; reg rs1; imm v ], n))
                  n
            | 0x03 ->
                let n =
                  match f3 with
                  | 0 -> Some "lb"
                  | 1 -> Some "lh"
                  | 2 -> Some "lw"
                  | 3 when xlen = 64 -> Some "ld"
                  | 4 -> Some "lbu"
                  | 5 -> Some "lhu"
                  | 6 when xlen = 64 -> Some "lwu"
                  | _ -> None
                in
                Option.map
                  (fun n ->
                    (instruction (op_exn n) [ reg rd; mem rs1 (sign_extend 12 (bits w 20 12)) ], n))
                  n
            | 0x23 ->
                let n =
                  match f3 with
                  | 0 -> Some "sb"
                  | 1 -> Some "sh"
                  | 2 -> Some "sw"
                  | 3 when xlen = 64 -> Some "sd"
                  | _ -> None
                in
                let v =
                  sign_extend 12 (Int64.logor (bits w 7 5) (Int64.shift_left (bits w 25 7) 5))
                in
                Option.map (fun n -> (instruction (op_exn n) [ reg rs2; mem rs1 v ], n)) n
            | 0x07 ->
                Option.map
                  (fun n ->
                    ( instruction (op_exn n)
                        [ f_operand rd; mem rs1 (sign_extend 12 (bits w 20 12)) ],
                      n ))
                  (f_load_name f3)
            | 0x27 ->
                let v =
                  sign_extend 12 (Int64.logor (bits w 7 5) (Int64.shift_left (bits w 25 7) 5))
                in
                Option.map
                  (fun n -> (instruction (op_exn n) [ f_operand rs2; mem rs1 v ], n))
                  (f_store_name f3)
            | 0x53 -> (
                let n =
                  match f_r_name f3 f7 rs2 with
                  | Some "fsgnjn.s" when rs1 = rs2 -> Some "fneg.s"
                  | Some "fsgnjn.d" when rs1 = rs2 -> Some "fneg.d"
                  | Some "fsgnj.d" when rs1 = rs2 -> Some "fmv.d"
                  | other -> other
                in
                match n with
                | None -> None
                | Some n -> (
                    match f_shape (op_exn n) with
                    | None -> None
                    | Some shape ->
                        let r isf v = if isf then f_operand v else reg v in
                        let base =
                          if shape.arity = 2 then [ r shape.rd_f rd; r shape.rs1_f rs1 ]
                          else [ r shape.rd_f rd; r shape.rs1_f rs1; r shape.rs2_f rs2 ]
                        in
                        (* An explicit, non-default rounding mode is a real trailing
                           operand a re-parse must see too, not only text {!Lowered.pp}
                           prints - {!rm_suffix}'s own condition, read back here from the
                           other end. *)
                        let ops =
                          match shape.rm with
                          | Some default when Rm.to_funct3 default <> f3 -> (
                              match Rm.of_funct3 f3 with
                              | Some rm ->
                                  base @ [ Operand.Sym (Asm_core.Expr.Symbol (Rm.name rm)) ]
                              | None -> base)
                          | Some _ | None -> base
                        in
                        Some (instruction (op_exn n) ops, n)))
            | 0x43 | 0x47 | 0x4b | 0x4f ->
                let rs3 = Int64.to_int (bits w 27 5) in
                let fmt = Int64.to_int (bits w 25 2) in
                Option.map
                  (fun n ->
                    ( instruction (op_exn n)
                        [ f_operand rd; f_operand rs1; f_operand rs2; f_operand rs3 ],
                      n ))
                  (f_r4_name opc fmt)
            | 0x63 ->
                let n =
                  match f3 with
                  | 0 -> Some "beq"
                  | 1 -> Some "bne"
                  | 4 -> Some "blt"
                  | 5 -> Some "bge"
                  | 6 -> Some "bltu"
                  | 7 -> Some "bgeu"
                  | _ -> None
                in
                let v =
                  sign_extend 13
                    (Int64.logor
                       (Int64.shift_left (bits w 31 1) 12)
                       (Int64.logor
                          (Int64.shift_left (bits w 7 1) 11)
                          (Int64.logor
                             (Int64.shift_left (bits w 25 6) 5)
                             (Int64.shift_left (bits w 8 4) 1))))
                in
                Option.map
                  (fun n ->
                    (instruction (op_exn n) [ reg rs1; reg rs2; sym (Int64.add ctx.address v) ], n))
                  n
            | 0x37 | 0x17 ->
                let n = if opc = 0x37 then "lui" else "auipc" in
                Some (instruction (op_exn n) [ reg rd; imm (sign_extend 20 (bits w 12 20)) ], n)
            | 0x6f ->
                let v =
                  sign_extend 21
                    (Int64.logor
                       (Int64.shift_left (bits w 31 1) 20)
                       (Int64.logor
                          (Int64.shift_left (bits w 12 8) 12)
                          (Int64.logor
                             (Int64.shift_left (bits w 20 1) 11)
                             (Int64.shift_left (bits w 21 10) 1))))
                in
                Some (instruction Opcode.Jal [ reg rd; sym (Int64.add ctx.address v) ], "jal")
            | 0x67 when f3 = 0 ->
                Some
                  ( instruction Opcode.Jalr [ reg rd; mem rs1 (sign_extend 12 (bits w 20 12)) ],
                    "jalr" )
            | 0x73 when Int64.equal w 0x73L -> Some (instruction Opcode.Ecall [], "ecall")
            | 0x73 when Int64.equal w 0x00100073L -> Some (instruction Opcode.Ebreak [], "ebreak")
            | 0x73 when Int64.equal w 0xc0001073L -> Some (instruction Opcode.Unimp [], "unimp")
            | 0x73 when f3 <> 0 && f3 <> 4 ->
                (* [unimp] above is itself a [csrrw] word and must stay ahead of this arm. The
                   aliases are the ones the encoder takes back to the same word. *)
                let csr =
                  let n = Int64.to_int (bits w 20 12) in
                  match Riscv_csr.name n with
                  | Some name -> Operand.Sym (Asm_core.Expr.Symbol name)
                  | None -> imm (Int64.of_int n)
                in
                let name, ops =
                  match (f3, rd, rs1) with
                  | 2, _, 0 when rd <> 0 -> ("csrr", [ reg rd; csr ])
                  | 2, 0, 0 -> ("csrrs", [ reg rd; csr; reg rs1 ])
                  | (1 | 2 | 3), 0, _ ->
                      ((match f3 with 1 -> "csrw" | 2 -> "csrs" | _ -> "csrc"), [ csr; reg rs1 ])
                  | (5 | 6 | 7), 0, _ ->
                      ( (match f3 with 5 -> "csrwi" | 6 -> "csrsi" | _ -> "csrci"),
                        [ csr; imm (Int64.of_int rs1) ] )
                  | (1 | 2 | 3), _, _ ->
                      ( (match f3 with 1 -> "csrrw" | 2 -> "csrrs" | _ -> "csrrc"),
                        [ reg rd; csr; reg rs1 ] )
                  | _ ->
                      ( (match f3 with 5 -> "csrrwi" | 6 -> "csrrsi" | _ -> "csrrci"),
                        [ reg rd; csr; imm (Int64.of_int rs1) ] )
                in
                Some (instruction (op_exn name) ops, name)
            | 0x0f when Int64.equal w 0x0000100fL -> Some (instruction Opcode.Fence_i [], "fence.i")
            | 0x0f when Int64.equal w 0x8330000fL ->
                Some (instruction Opcode.Fence_tso [], "fence.tso")
            | 0x0f when Int64.equal w 0x0ff0000fL -> Some (instruction Opcode.Fence [], "fence")
            | 0x0f when Int64.equal w 0x0100000fL -> Some (instruction Opcode.Pause [], "pause")
            | 0x0f
              when Int64.logand w 0xf00fffffL = 0x0000000fL
                   && bits w 24 4 <> 0L
                   && bits w 20 4 <> 0L ->
                let p = fence_set_name (Int64.to_int (bits w 24 4))
                and q = fence_set_name (Int64.to_int (bits w 20 4)) in
                Some
                  ( instruction Opcode.Fence
                      [ Operand.Sym (Asm_core.Expr.Symbol p); Operand.Sym (Asm_core.Expr.Symbol q) ],
                    Printf.sprintf "fence %s,%s" p q )
            | 0x2f when f7 land 0x3 = 0 -> (
                (* Zaamo, bare-mnemonic spelling only (aq=rl=0 - see
                   {!amo3_desc}); aq/rl set is left undecoded rather than
                   silently dropped, since this project's own encoder never
                   emits that bit pattern for any mnemonic yet. *)
                let funct5 = f7 asr 2 in
                let suffix =
                  if f3 = 2 then Some ".w" else if f3 = 3 && xlen = 64 then Some ".d" else None
                in
                match suffix with
                | None -> None
                | Some suffix -> (
                    let base =
                      match funct5 with
                      | 0x00 -> Some "amoadd"
                      | 0x01 -> Some "amoswap"
                      | 0x02 -> Some "lr"
                      | 0x03 -> Some "sc"
                      | 0x04 -> Some "amoxor"
                      | 0x08 -> Some "amoor"
                      | 0x0c -> Some "amoand"
                      | 0x10 -> Some "amomin"
                      | 0x14 -> Some "amomax"
                      | 0x18 -> Some "amominu"
                      | 0x1c -> Some "amomaxu"
                      | _ -> None
                    in
                    match base with
                    | None -> None
                    | Some "lr" ->
                        let n = "lr" ^ suffix in
                        Some (instruction (op_exn n) [ reg rd; mem rs1 0L ], n)
                    | Some base ->
                        let n = base ^ suffix in
                        Some (instruction (op_exn n) [ reg rd; reg rs2; mem rs1 0L ], n)))
            | _ -> None)
      in
      match result with
      | None -> (
          match table_decode w with
          | Some (i, f) -> Ok (i, f, 4)
          | None -> Error (diag ~pos:__POS__ `Decode_no_match))
      | Some (i, f) -> Ok (i, f, 4)

  let table_fallback ?legal bytes ~pos =
    if String.length bytes - pos < 2 then None
    else if Char.code bytes.[pos] land 3 <> 3 then
      Option.map (fun (i, f) -> (i, f, 2)) (table_decode ~half:true ?legal (read_half bytes pos))
    else if String.length bytes - pos < 4 then None
    else Option.map (fun (i, f) -> (i, f, 4)) (table_decode ?legal (read_word bytes pos))

  (* Generated rows are tried whenever the hand-written decoder declines a word, whichever of its
     arms declined it. *)
  let decode_ungated ctx bytes ~pos =
    match decode_hand_written ctx bytes ~pos with
    | Ok _ as ok -> ok
    | Error _ as e -> ( match table_fallback bytes ~pos with Some r -> Ok r | None -> e)

  (* Every reading of the word, hand-written before generated and generated rows in table order,
     whether or not the state enables it. *)
  let decode_candidates ctx bytes ~pos =
    let hand = match decode_hand_written ctx bytes ~pos with Ok r -> [ r ] | Error _ -> [] in
    let rows =
      if String.length bytes - pos < 2 then []
      else if Char.code bytes.[pos] land 3 <> 3 then
        List.map
          (fun (i, f) -> (i, f, 2))
          (table_matches ~half:true ~first_only:false (read_half bytes pos))
      else if String.length bytes - pos < 4 then []
      else
        List.map (fun (i, f) -> (i, f, 4)) (table_matches ~first_only:false (read_word bytes pos))
    in
    hand
    @ List.filter (fun (_, f, _) -> not (List.exists (fun (_, g, _) -> String.equal f g) hand)) rows

  (* The first candidate whose mnemonic satisfies [legal], hand-written before generated, so a
     disabled specialization falls back to an enabled general form. *)
  let decode_where ~legal ctx bytes ~pos =
    let hand = decode_hand_written ctx bytes ~pos in
    match hand with
    | Ok (insn, _, _) when legal insn.Instruction.op -> hand
    | _ -> (
        match table_fallback ~legal bytes ~pos with
        | Some r -> Ok r
        | None -> (
            match hand with Error _ -> hand | Ok _ -> Error (diag ~pos:__POS__ `Decode_no_match)))

  (* Strict decoding: a word whose only readings are disabled is refused rather than printed as if
     it were assemblable under this configuration. *)
  let decode ctx bytes ~pos =
    let legal op = Result.is_ok (feature_gate ctx.state op) in
    match decode_where ~legal ctx bytes ~pos with
    | Ok _ as ok -> ok
    | Error _ as e -> (
        match decode_ungated ctx bytes ~pos with
        | Ok (insn, _, _) -> (
            match feature_gate ctx.state insn.Instruction.op with Error _ as g -> g | Ok () -> e)
        | Error _ -> e)

  (* [codec_in state] is the codec of the forms [state]'s configuration enables; [codec] is the
     default-configuration one, which leaves out the opt-in extensions.

     The public codec describes the same real words as the explicit encoder
     and decoder above.  Symbol expressions and PC context live outside a raw
     word relation, so decoding canonicalizes them to a zero displacement; the
     fixup-bearing [encode] result remains the authority for layout.  The
     64-bit branch is one atomic AUIPC+I-format pair, never two independently
     selectable instructions. *)
  let codec_in state : (Lowered.t, fixup_kind) C.t =
    let encoded_bits wanted l =
      match encode_in state l with
      | Ok (`Fixed f) when String.length f.bytes = wanted ->
          if wanted = 2 then Some (read_half f.bytes 0)
          else if wanted = 4 then Some (read_word f.bytes 0)
          else
            Some
              (Int64.logor
                 (Int64.shift_left (read_word f.bytes 0) 32)
                 (Int64.logand (read_word f.bytes 4) 0xffffffffL))
      | Ok (`Fixed _) | Ok (`Ladder _) | Error _ -> None
    in
    let decode_word w =
      match decode { state; address = 0L } (bytes_of_word w) ~pos:0 with
      | Ok (instruction, _, 4) -> (
          match lower_instruction state instruction with
          | Ok [ lowered ] -> Some lowered
          | _ -> None)
      | Ok _ | Error _ -> None
    in
    let decode_half half =
      match decode { state; address = 0L } (bytes_of_half half) ~pos:0 with
      | Ok (instruction, _, 2) -> (
          match lower_instruction { state with rvc = true } instruction with
          | Ok [ lowered ] -> Some lowered
          | _ -> None)
      | Ok _ | Error _ -> None
    in
    let decode_pair w =
      let high = Int64.shift_right_logical w 32 in
      let low = Int64.logand w 0xffffffffL in
      let high_opcode = Int64.to_int (bits high 0 7) in
      let tmp = Int64.to_int (bits high 7 5) in
      let low_opcode = Int64.to_int (bits low 0 7) in
      let low_funct3 = Int64.to_int (bits low 12 3) in
      let rd = Int64.to_int (bits low 7 5) in
      let rs1 = Int64.to_int (bits low 15 5) in
      let high_imm = sign_extend 20 (bits high 12 20) in
      let low_imm = sign_extend 12 (bits low 20 12) in
      if high_opcode <> 0x17 || rs1 <> tmp then None
      else
        let target =
          Asm_core.Expr.Const (Bigint.of_int64 (Int64.add (Int64.shift_left high_imm 12) low_imm))
        in
        match low_opcode with
        | 0x13 ->
            Some
              (Lowered.Pair
                 { op = Opcode.La; rd = Reg.x rd; tmp = Xreg.of_int_exn tmp; target; kind = Addi })
        | 0x67 when rd = 1 && tmp = 1 ->
            Some
              (Lowered.Pair
                 { op = Opcode.Call; rd = Reg.x rd; tmp = Xreg.of_int_exn tmp; target; kind = Jalr })
        | 0x67 when rd = 0 && tmp = 6 ->
            Some
              (Lowered.Pair
                 { op = Opcode.Tail; rd = Reg.x rd; tmp = Xreg.of_int_exn tmp; target; kind = Jalr })
        | 0x03 when rd = tmp && low_funct3 = 3 ->
            Some
              (Lowered.Pair
                 {
                   op = Opcode.Ld;
                   rd = Reg.x rd;
                   tmp = Xreg.of_int_exn tmp;
                   target;
                   kind = Load low_funct3;
                 })
        | 0x07 ->
            Option.map
              (fun op ->
                Lowered.Pair
                  { op; rd = Reg.f rd; tmp = Xreg.of_int_exn tmp; target; kind = Fload low_funct3 })
              (match low_funct3 with 2 -> Some Opcode.Flw | 3 -> Some Opcode.Fld | _ -> None)
        | _ -> None
    in
    C.choice ~name:P.name
      [
        C.alt ~label:"compressed" ~priority:2
          (C.iso_fun ~name:(P.name ^ "-compressed") ~encode:(encoded_bits 2) ~decode:decode_half
             (C.field ~width:16 "compressed-instruction"));
        C.alt ~label:"word" ~priority:1
          (C.iso_fun ~name:(P.name ^ "-word") ~encode:(encoded_bits 4) ~decode:decode_word
             (C.field ~width:32 "instruction"));
        C.alt ~label:"pair" ~priority:0
          (C.iso_fun ~name:(P.name ^ "-pair") ~encode:(encoded_bits 8) ~decode:decode_pair
             (C.field ~width:64 "auipc-i-pair"));
      ]

  let codec = codec_in default_state

  let evaluate_fixup kind ~place ~target =
    let d = Int64.sub target place in
    let aligned what bits =
      if Int64.logand d 1L <> 0L then Error (diag ~pos:__POS__ (`Immediate_alignment what))
      else if not (fits_signed bits d) then Error (diag ~pos:__POS__ (`Immediate_range what))
      else Ok d
    in
    let hi v = Int64.shift_right (Int64.add v 0x800L) 12 in
    let lo v = Int64.sub v (Int64.shift_left (hi v) 12) in
    let rv32_absolute () =
      if xlen <> 32 then Ok target
      else if Int64.compare target (-0x80000000L) < 0 || Int64.compare target 0xffffffffL > 0 then
        Error (diag ~pos:__POS__ (`Immediate_range "RV32 address"))
      else if Int64.compare target 0x7fffffffL > 0 then Ok (Int64.sub target 0x100000000L)
      else Ok target
    in
    match kind with
    | Abs32 ->
        if Int64.compare target (-0x80000000L) >= 0 && Int64.compare target 0xffffffffL <= 0 then
          Ok target
        else Error (diag ~pos:__POS__ (`Immediate_range "32-bit address"))
    | Abs64 -> Ok target
    | Branch13 -> aligned "branch" 13
    | Branch9c -> aligned "branch-c" 9
    | Jump12c -> aligned "jump-c" 12
    | Jal21 -> aligned "jal" 21
    | Pcrel_hi20 | Call_hi20 ->
        if fits_signed 32 d then Ok (hi d) else Error (diag ~pos:__POS__ (`Immediate_range "auipc"))
    | Pcrel_lo12_i | Pcrel_lo12_s | Call_lo12_i ->
        if fits_signed 32 d then Ok (lo d) else Error (diag ~pos:__POS__ (`Immediate_range "pcrel"))
    | Abs_hi20 -> Result.map hi (rv32_absolute ())
    | Abs_lo12_i | Abs_lo12_s -> Result.map lo (rv32_absolute ())

  let data_widths =
    [
      (".byte", 1);
      (".short", 2);
      (".hword", 2);
      (".word", 4);
      (".4byte", 4);
      (".long", 4);
      (".quad", 8);
      (".dword", 8);
    ]

  let data_fixup ~width =
    match width with
    | 4 -> Ok Abs32
    | 8 when xlen = 64 -> Ok Abs64
    | _ -> Error (diag ~pos:__POS__ (`No_data_relocation width))

  let nop_bytes ~length =
    if length mod 4 <> 0 then Error (diag ~pos:__POS__ `Padding_not_word_multiple)
    else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x13\x00\x00\x00")))

  (* Measured (M3 §3/§5, docs/design.md §12): a linker-inserted merge gap in an
     executable section is plain zero fill on both riscv32 and riscv64, not NOP
     fill - unlike the *assembler's own* end-of-section padding, which uses
     real [c.nop]/[nop] via [nop_bytes] above. *)
  let merge_fill = None

  (* Measured: riscv64/riscv32 GAS both round a section's own final size up to
     its recorded alignment (e.g. `.text` / `.balign 16` / `nop` records a
     16-byte-padded size), unlike every other target here. The real corpus
     agrees: GNU's `runtime-vararg` `.text` is 112 bytes on both RISC-V
     profiles - a multiple of its own 16-byte alignment - but 68/100/180 bytes
     (none a multiple of 16) on x86_32/arm/aarch64. *)
  let pad_section_to_alignment = true
end

(* The standard extensions, as a named functor: [Riscv_family.Make] applies [Make_with] to the
   same path, so the encoder and the text frontend of one profile share their types. An anonymous
   structure here would make every type of the two instances distinct. *)
module Standard (P : PROFILE) = struct
  let definition = Riscv_extensions.standard ~xlen:P.xlen
end

module Make (P : PROFILE) = Make_with (P) (Standard (P))

module Extensions = struct
  type definition = Riscv_extensions.definition = {
    components : Target_component.t list;
    rows : Riscv_table_row.row array;
  }

  module Validated = Riscv_extensions.Validated

  let standard = Riscv_extensions.standard

  (* The forms the encoder spells out in code do not depend on the profile. *)
  let handwritten = lazy (List.map Riscv_types.Opcode.mnemonic Riscv_types.Opcode.all)

  (* The mnemonic the standard decoder reads a 32-bit word as, per XLEN. *)
  let standard_reader xlen =
    let module Probe = Make (struct
      let name = "probe"
      let triple = "probe"
      let xlen = xlen
    end) in
    fun w ->
      match
        Probe.decode_hand_written
          { Probe.state = Probe.default_state; address = 0L }
          (Probe.bytes_of_word w) ~pos:0
      with
      | Ok (_, name, _) -> Some name
      | Error _ -> None

  let standard_readers = [ (32, lazy (standard_reader 32)); (64, lazy (standard_reader 64)) ]

  let make ~xlen def =
    let standard_reads =
      match List.assoc_opt xlen standard_readers with
      | Some r -> Lazy.force r
      | None -> standard_reader xlen
    in
    Riscv_extensions.make ~handwritten:(Lazy.force handwritten) ~standard_reads ~xlen def
end
