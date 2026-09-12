open Foundation
module C = Codec

module type PROFILE = sig
  val name : string
  val triple : string
  val xlen : int
end

module Make (P : PROFILE) = struct
  open Asm_core.Lowered_ast

  let name = P.name
  let triple = P.triple
  let xlen = P.xlen

  module Reg = struct
    type t = X of int | F of int | V of int

    let equal a b = a = b

    let name = function
      | X n -> Printf.sprintf "x%d" n
      | F n -> Printf.sprintf "f%d" n
      | V n -> Printf.sprintf "v%d" n

    let pp ppf r = Fmt.string ppf (name r)

    let aliases =
      [
        ("zero", 0);
        ("ra", 1);
        ("sp", 2);
        ("gp", 3);
        ("tp", 4);
        ("t0", 5);
        ("t1", 6);
        ("t2", 7);
        ("s0", 8);
        ("fp", 8);
        ("s1", 9);
        ("a0", 10);
        ("a1", 11);
        ("a2", 12);
        ("a3", 13);
        ("a4", 14);
        ("a5", 15);
        ("a6", 16);
        ("a7", 17);
        ("s2", 18);
        ("s3", 19);
        ("s4", 20);
        ("s5", 21);
        ("s6", 22);
        ("s7", 23);
        ("s8", 24);
        ("s9", 25);
        ("s10", 26);
        ("s11", 27);
        ("t3", 28);
        ("t4", 29);
        ("t5", 30);
        ("t6", 31);
      ]

    (* The FP calling-convention register names (RISC-V ABI). Not generated
       from a formula: the three runs are not contiguous. Read out of
       riscv64-linux-gnu-as 2.44 by assembling `fld <name>, 0(x1)` for each
       name and decoding the [rd] field. There is no FP counterpart of the
       integer [fp] alias. *)
    let f_aliases =
      [
        ("ft0", 0);
        ("ft1", 1);
        ("ft2", 2);
        ("ft3", 3);
        ("ft4", 4);
        ("ft5", 5);
        ("ft6", 6);
        ("ft7", 7);
        ("fs0", 8);
        ("fs1", 9);
        ("fa0", 10);
        ("fa1", 11);
        ("fa2", 12);
        ("fa3", 13);
        ("fa4", 14);
        ("fa5", 15);
        ("fa6", 16);
        ("fa7", 17);
        ("fs2", 18);
        ("fs3", 19);
        ("fs4", 20);
        ("fs5", 21);
        ("fs6", 22);
        ("fs7", 23);
        ("fs8", 24);
        ("fs9", 25);
        ("fs10", 26);
        ("fs11", 27);
        ("ft8", 28);
        ("ft9", 29);
        ("ft10", 30);
        ("ft11", 31);
      ]

    let numbered prefix ctor s =
      let n = String.length prefix in
      if String.length s > n && String.sub s 0 n = prefix then
        match int_of_string_opt (String.sub s n (String.length s - n)) with
        | Some i when i >= 0 && i < 32 -> Some (ctor i)
        | _ -> None
      else None

    let find s =
      match numbered "x" (fun n -> X n) s with
      | Some _ as r -> r
      | None -> (
          match numbered "f" (fun n -> F n) s with
          | Some _ as r -> r
          | None -> (
              match numbered "v" (fun n -> V n) s with
              | Some _ as r -> r
              | None -> (
                  (* The two alias tables cannot collide: every f_aliases name
                     starts with 'f' followed by a non-digit, so [numbered "f"]
                     has already declined it, and no integer alias is spelled that
                     way either - the integer [fp] is its own entry below. V has
                     no ABI alias names at all - every RVV mnemonic spells vector
                     registers as bare [v0]-[v31]. *)
                  match List.assoc_opt s f_aliases with
                  | Some n -> Some (F n)
                  | None -> Option.map (fun n -> X n) (List.assoc_opt s aliases))))

    let x = function X n -> Some n | F _ | V _ -> None
    let x_exn = function X n -> n | F _ | V _ -> invalid_arg "RISC-V integer register required"
    let of_x n = X (n land 31)
  end

  module Mem = struct
    type t = { offset : Asm_core.Expr.t; base : Reg.t }

    let pp ppf m = Fmt.pf ppf "%a(%a)" Asm_core.Expr.pp m.offset Reg.pp m.base
  end

  module Operand = struct
    type t = Reg of Reg.t | Imm of Bigint.t | Sym of Asm_core.Expr.t | Mem of Mem.t

    let pp ppf = function
      | Reg r -> Reg.pp ppf r
      | Imm n -> Bigint.pp ppf n
      | Sym e -> Asm_core.Expr.pp ppf e
      | Mem m -> Mem.pp ppf m
  end

  module Opcode = struct
    type t =
      | Add
      | Sub
      | Sll
      | Slt
      | Sltu
      | Snez
      | Xor
      | Srl
      | Sra
      | Or
      | And
      | Mul
      | Remu
      | Sh1add
      | Sh2add
      | Sh3add
      | Min
      | Minu
      | Max
      | Maxu
      | Andn
      | Orn
      | Xnor
      | Rol
      | Ror
      | Sh1adduw
      | Sh2adduw
      | Sh3adduw
      | Clz
      | Ctz
      | Cpop
      | Sextb
      | Sexth
      | Orcb
      | Clzw
      | Ctzw
      | Cpopw
      | Brev8
      | Rev8
      | Pack
      | Packh
      | Packw
      | Zip
      | Unzip
      | Rolw
      | Rorw
      | Rori
      | Roriw
      | Zext_h
      | Clmul
      | Clmulh
      | Xperm4
      | Xperm8
      | Sha256sum0
      | Sha256sum1
      | Sha256sig0
      | Sha256sig1
      | Sha512sum0
      | Sha512sum1
      | Sha512sig0
      | Sha512sig1
      | Sha512sum0r
      | Sha512sum1r
      | Sha512sig0l
      | Sha512sig1l
      | Sha512sig0h
      | Sha512sig1h
      | Aes64ds
      | Aes64dsm
      | Aes64es
      | Aes64esm
      | Aes64ks2
      | Aes64im
      | Aes64ks1i
      | Aes32dsi
      | Aes32dsmi
      | Aes32esi
      | Aes32esmi
      | Csrrw
      | Csrrs
      | Csrrc
      | Csrrwi
      | Csrrsi
      | Csrrci
      | Csrr
      | Csrw
      | Csrs
      | Csrc
      | Csrwi
      | Csrsi
      | Csrci
      | Amoswap_w
      | Amoadd_w
      | Amoxor_w
      | Amoand_w
      | Amoor_w
      | Amomin_w
      | Amomax_w
      | Amominu_w
      | Amomaxu_w
      | Lr_w
      | Sc_w
      | Amoswap_d
      | Amoadd_d
      | Amoxor_d
      | Amoand_d
      | Amoor_d
      | Amomin_d
      | Amomax_d
      | Amominu_d
      | Amomaxu_d
      | Lr_d
      | Sc_d
      | Addw
      | Subw
      | Sllw
      | Srlw
      | Sraw
      | Mulw
      | Addi
      | C_addi
      | Slti
      | Sltiu
      | Xori
      | Ori
      | Andi
      | Slli
      | Srli
      | Srai
      | Addiw
      | Slliw
      | Srliw
      | Sraiw
      | Sext_w
      | Lb
      | Lh
      | Lw
      | Lbu
      | Lhu
      | Lwu
      | Ld
      | Sb
      | Sh
      | Sw
      | Sd
      | Beq
      | Bne
      | Blt
      | Bge
      | Bltu
      | Bgeu
      | Bgtu
      | Bleu
      | Lui
      | Auipc
      | Jal
      | Jalr
      | Mv
      | Nop
      | J
      | Jr
      | Call
      | Tail
      | La
      | Lla
      | Li
      | Ret
      | Ecall
      | Ebreak
      | Unimp
      | Fence_i
      | Fence
      | Fld
      | Flw
      | Fsd
      | Fsw
      | Fadd_d
      | Fadd_s
      | Fsub_d
      | Fsub_s
      | Fmul_d
      | Fmul_s
      | Fdiv_d
      | Fdiv_s
      | Fneg_d
      | Fneg_s
      | Fsgnj_s
      | Fsgnj_d
      | Fsgnjn_s
      | Fsgnjn_d
      | Fsgnjx_s
      | Fsgnjx_d
      | Fmin_s
      | Fmax_s
      | Fmin_d
      | Fmax_d
      | Fsqrt_s
      | Fsqrt_d
      | Fclass_s
      | Fclass_d
      | Fmadd_s
      | Fmadd_d
      | Fmsub_s
      | Fmsub_d
      | Fnmsub_s
      | Fnmsub_d
      | Fnmadd_s
      | Fnmadd_d
      | Fmv_d
      | Fmv_x_d
      | Feq_d
      | Fle_d
      | Flt_d
      | Flt_s
      | Feq_s
      | Fle_s
      | Fmv_x_w
      | Fmv_w_x
      | Fcvt_w_d
      | Fcvt_wu_d
      | Fcvt_l_d
      | Fcvt_d_w
      | Fcvt_d_wu
      | Fcvt_s_w
      | Fcvt_s_wu
      | Fcvt_w_s
      | Fcvt_wu_s
      | Fcvt_s_d
      | Fcvt_d_s
      | Fcvt_s_l
      | Fcvt_lu_d
      | Fcvt_d_l
      | Fcvt_d_lu
      | Fcvt_l_s
      | Fcvt_lu_s
      | Fcvt_s_lu
      | Vsetvl
      | Vsetvli
      | Vsetivli
      | Vadd_vv
      | Vadd_vx
      | Vadd_vi
      | Vsub_vv
      | Vsub_vx
      | Vrsub_vx
      | Vrsub_vi
      | Vand_vv
      | Vand_vx
      | Vand_vi
      | Vor_vv
      | Vor_vx
      | Vor_vi
      | Vxor_vv
      | Vxor_vx
      | Vxor_vi
      | Vsll_vv
      | Vsll_vx
      | Vsll_vi
      | Vsrl_vv
      | Vsrl_vx
      | Vsrl_vi
      | Vsra_vv
      | Vsra_vx
      | Vsra_vi
      | Vminu_vv
      | Vminu_vx
      | Vmin_vv
      | Vmin_vx
      | Vmaxu_vv
      | Vmaxu_vx
      | Vmax_vv
      | Vmax_vx
      | Vmul_vv
      | Vmul_vx
      | Vmulh_vv
      | Vmulh_vx
      | Vmulhu_vv
      | Vmulhu_vx
      | Vmulhsu_vv
      | Vmulhsu_vx
      | Vdivu_vv
      | Vdivu_vx
      | Vdiv_vv
      | Vdiv_vx
      | Vremu_vv
      | Vremu_vx
      | Vrem_vv
      | Vrem_vx
      | Vaadd_vv
      | Vaadd_vx
      | Vaaddu_vv
      | Vaaddu_vx
      | Vasub_vv
      | Vasub_vx
      | Vasubu_vv
      | Vasubu_vx
      | Vnclip_wv
      | Vnclip_wx
      | Vnclip_wi
      | Vnclipu_wv
      | Vnclipu_wx
      | Vnclipu_wi
      | Vnsra_wv
      | Vnsra_wx
      | Vnsra_wi
      | Vnsrl_wv
      | Vnsrl_wx
      | Vnsrl_wi
      | Vssrl_vv
      | Vssrl_vx
      | Vssrl_vi
      | Vssra_vv
      | Vssra_vx
      | Vssra_vi
      | Vrgather_vv
      | Vrgather_vx
      | Vrgather_vi
      | Vrgatherei16_vv
      | Vwaddu_vv
      | Vwaddu_vx
      | Vwadd_vv
      | Vwadd_vx
      | Vwsubu_vv
      | Vwsubu_vx
      | Vwsub_vv
      | Vwsub_vx
      | Vwaddu_wv
      | Vwaddu_wx
      | Vwadd_wv
      | Vwadd_wx
      | Vwsubu_wv
      | Vwsubu_wx
      | Vwsub_wv
      | Vwsub_wx
      | Vwmulu_vv
      | Vwmulu_vx
      | Vwmulsu_vv
      | Vwmulsu_vx
      | Vwmul_vv
      | Vwmul_vx
      | Vsext_vf2
      | Vsext_vf4
      | Vsext_vf8
      | Vzext_vf2
      | Vzext_vf4
      | Vzext_vf8
      | Vsadd_vv
      | Vsadd_vx
      | Vsadd_vi
      | Vsaddu_vv
      | Vsaddu_vx
      | Vsaddu_vi
      | Vssub_vv
      | Vssub_vx
      | Vssubu_vv
      | Vssubu_vx
      | Vmand_mm
      | Vmandn_mm
      | Vmor_mm
      | Vmxor_mm
      | Vmorn_mm
      | Vmnand_mm
      | Vmnor_mm
      | Vmxnor_mm
      | Vredsum_vs
      | Vredand_vs
      | Vredor_vs
      | Vredxor_vs
      | Vredminu_vs
      | Vredmin_vs
      | Vredmaxu_vs
      | Vredmax_vs
      | Vwredsumu_vs
      | Vwredsum_vs
      | Vmseq_vv
      | Vmseq_vx
      | Vmseq_vi
      | Vmsne_vv
      | Vmsne_vx
      | Vmsne_vi
      | Vmsltu_vv
      | Vmsltu_vx
      | Vmslt_vv
      | Vmslt_vx
      | Vmsleu_vv
      | Vmsleu_vx
      | Vmsleu_vi
      | Vmsle_vv
      | Vmsle_vx
      | Vmsle_vi
      | Vmsgtu_vx
      | Vmsgtu_vi
      | Vmsgt_vx
      | Vmsgt_vi
      | Vslideup_vx
      | Vslideup_vi
      | Vslidedown_vx
      | Vslidedown_vi
      | Vslide1up_vx
      | Vslide1down_vx
      | Vmacc_vv
      | Vmacc_vx
      | Vnmsac_vv
      | Vnmsac_vx
      | Vmadd_vv
      | Vmadd_vx
      | Vnmsub_vv
      | Vnmsub_vx
      | Vwmaccu_vv
      | Vwmaccu_vx
      | Vwmacc_vv
      | Vwmacc_vx
      | Vwmaccsu_vv
      | Vwmaccsu_vx
      | Vwmaccus_vx
      | Vid_v
      | Viota_m
      | Vcompress_vm
      | Vcpop_m
      | Vfirst_m
      | Vmsbf_m
      | Vmsif_m
      | Vmsof_m
      | Vadc_vvm
      | Vadc_vxm
      | Vadc_vim
      | Vmadc_vvm
      | Vmadc_vxm
      | Vmadc_vim
      | Vmadc_vv
      | Vmadc_vx
      | Vmadc_vi
      | Vsbc_vvm
      | Vsbc_vxm
      | Vmsbc_vvm
      | Vmsbc_vxm
      | Vmsbc_vv
      | Vmsbc_vx
      | Vmerge_vvm
      | Vmerge_vxm
      | Vmerge_vim
      | Vmv_s_x
      | Vmv_x_s
      | Vmv_v_i
      | Vmv_v_v
      | Vmv_v_x
      | Vmv1r_v
      | Vmv2r_v
      | Vmv4r_v
      | Vmv8r_v
      | Vsmul_vv
      | Vsmul_vx

    let name = function
      | Add -> "add"
      | Sub -> "sub"
      | Sll -> "sll"
      | Slt -> "slt"
      | Sltu -> "sltu"
      | Snez -> "snez"
      | Xor -> "xor"
      | Srl -> "srl"
      | Sra -> "sra"
      | Or -> "or"
      | And -> "and"
      | Mul -> "mul"
      | Remu -> "remu"
      | Sh1add -> "sh1add"
      | Sh2add -> "sh2add"
      | Sh3add -> "sh3add"
      | Min -> "min"
      | Minu -> "minu"
      | Max -> "max"
      | Maxu -> "maxu"
      | Andn -> "andn"
      | Orn -> "orn"
      | Xnor -> "xnor"
      | Rol -> "rol"
      | Ror -> "ror"
      | Sh1adduw -> "sh1add.uw"
      | Sh2adduw -> "sh2add.uw"
      | Sh3adduw -> "sh3add.uw"
      | Clz -> "clz"
      | Ctz -> "ctz"
      | Cpop -> "cpop"
      | Sextb -> "sext.b"
      | Sexth -> "sext.h"
      | Orcb -> "orc.b"
      | Clzw -> "clzw"
      | Ctzw -> "ctzw"
      | Cpopw -> "cpopw"
      | Brev8 -> "brev8"
      | Rev8 -> "rev8"
      | Pack -> "pack"
      | Packh -> "packh"
      | Packw -> "packw"
      | Zip -> "zip"
      | Unzip -> "unzip"
      | Rolw -> "rolw"
      | Rorw -> "rorw"
      | Rori -> "rori"
      | Roriw -> "roriw"
      | Zext_h -> "zext.h"
      | Clmul -> "clmul"
      | Clmulh -> "clmulh"
      | Xperm4 -> "xperm4"
      | Xperm8 -> "xperm8"
      | Sha256sum0 -> "sha256sum0"
      | Sha256sum1 -> "sha256sum1"
      | Sha256sig0 -> "sha256sig0"
      | Sha256sig1 -> "sha256sig1"
      | Sha512sum0 -> "sha512sum0"
      | Sha512sum1 -> "sha512sum1"
      | Sha512sig0 -> "sha512sig0"
      | Sha512sig1 -> "sha512sig1"
      | Sha512sum0r -> "sha512sum0r"
      | Sha512sum1r -> "sha512sum1r"
      | Sha512sig0l -> "sha512sig0l"
      | Sha512sig1l -> "sha512sig1l"
      | Sha512sig0h -> "sha512sig0h"
      | Sha512sig1h -> "sha512sig1h"
      | Aes64ds -> "aes64ds"
      | Aes64dsm -> "aes64dsm"
      | Aes64es -> "aes64es"
      | Aes64esm -> "aes64esm"
      | Aes64ks2 -> "aes64ks2"
      | Aes64im -> "aes64im"
      | Aes64ks1i -> "aes64ks1i"
      | Aes32dsi -> "aes32dsi"
      | Aes32dsmi -> "aes32dsmi"
      | Aes32esi -> "aes32esi"
      | Aes32esmi -> "aes32esmi"
      | Csrrw -> "csrrw"
      | Csrrs -> "csrrs"
      | Csrrc -> "csrrc"
      | Csrrwi -> "csrrwi"
      | Csrrsi -> "csrrsi"
      | Csrrci -> "csrrci"
      | Csrr -> "csrr"
      | Csrw -> "csrw"
      | Csrs -> "csrs"
      | Csrc -> "csrc"
      | Csrwi -> "csrwi"
      | Csrsi -> "csrsi"
      | Csrci -> "csrci"
      | Amoswap_w -> "amoswap.w"
      | Amoadd_w -> "amoadd.w"
      | Amoxor_w -> "amoxor.w"
      | Amoand_w -> "amoand.w"
      | Amoor_w -> "amoor.w"
      | Amomin_w -> "amomin.w"
      | Amomax_w -> "amomax.w"
      | Amominu_w -> "amominu.w"
      | Amomaxu_w -> "amomaxu.w"
      | Lr_w -> "lr.w"
      | Sc_w -> "sc.w"
      | Amoswap_d -> "amoswap.d"
      | Amoadd_d -> "amoadd.d"
      | Amoxor_d -> "amoxor.d"
      | Amoand_d -> "amoand.d"
      | Amoor_d -> "amoor.d"
      | Amomin_d -> "amomin.d"
      | Amomax_d -> "amomax.d"
      | Amominu_d -> "amominu.d"
      | Amomaxu_d -> "amomaxu.d"
      | Lr_d -> "lr.d"
      | Sc_d -> "sc.d"
      | Addw -> "addw"
      | Subw -> "subw"
      | Sllw -> "sllw"
      | Srlw -> "srlw"
      | Sraw -> "sraw"
      | Mulw -> "mulw"
      | Addi -> "addi"
      | C_addi -> "c.addi"
      | Slti -> "slti"
      | Sltiu -> "sltiu"
      | Xori -> "xori"
      | Ori -> "ori"
      | Andi -> "andi"
      | Slli -> "slli"
      | Srli -> "srli"
      | Srai -> "srai"
      | Addiw -> "addiw"
      | Slliw -> "slliw"
      | Srliw -> "srliw"
      | Sraiw -> "sraiw"
      | Sext_w -> "sext.w"
      | Lb -> "lb"
      | Lh -> "lh"
      | Lw -> "lw"
      | Lbu -> "lbu"
      | Lhu -> "lhu"
      | Lwu -> "lwu"
      | Ld -> "ld"
      | Sb -> "sb"
      | Sh -> "sh"
      | Sw -> "sw"
      | Sd -> "sd"
      | Beq -> "beq"
      | Bne -> "bne"
      | Blt -> "blt"
      | Bge -> "bge"
      | Bltu -> "bltu"
      | Bgeu -> "bgeu"
      | Bgtu -> "bgtu"
      | Bleu -> "bleu"
      | Lui -> "lui"
      | Auipc -> "auipc"
      | Jal -> "jal"
      | Jalr -> "jalr"
      | Mv -> "mv"
      | Nop -> "nop"
      | J -> "j"
      | Jr -> "jr"
      | Call -> "call"
      | Tail -> "tail"
      | La -> "la"
      | Lla -> "lla"
      | Li -> "li"
      | Ret -> "ret"
      | Ecall -> "ecall"
      | Ebreak -> "ebreak"
      | Unimp -> "unimp"
      | Fence_i -> "fence.i"
      | Fence -> "fence"
      | Fld -> "fld"
      | Flw -> "flw"
      | Fsd -> "fsd"
      | Fsw -> "fsw"
      | Fadd_d -> "fadd.d"
      | Fadd_s -> "fadd.s"
      | Fsub_d -> "fsub.d"
      | Fsub_s -> "fsub.s"
      | Fmul_d -> "fmul.d"
      | Fmul_s -> "fmul.s"
      | Fdiv_d -> "fdiv.d"
      | Fdiv_s -> "fdiv.s"
      | Fneg_d -> "fneg.d"
      | Fneg_s -> "fneg.s"
      | Fsgnj_s -> "fsgnj.s"
      | Fsgnj_d -> "fsgnj.d"
      | Fsgnjn_s -> "fsgnjn.s"
      | Fsgnjn_d -> "fsgnjn.d"
      | Fsgnjx_s -> "fsgnjx.s"
      | Fsgnjx_d -> "fsgnjx.d"
      | Fmin_s -> "fmin.s"
      | Fmax_s -> "fmax.s"
      | Fmin_d -> "fmin.d"
      | Fmax_d -> "fmax.d"
      | Fsqrt_s -> "fsqrt.s"
      | Fsqrt_d -> "fsqrt.d"
      | Fclass_s -> "fclass.s"
      | Fclass_d -> "fclass.d"
      | Fmadd_s -> "fmadd.s"
      | Fmadd_d -> "fmadd.d"
      | Fmsub_s -> "fmsub.s"
      | Fmsub_d -> "fmsub.d"
      | Fnmsub_s -> "fnmsub.s"
      | Fnmsub_d -> "fnmsub.d"
      | Fnmadd_s -> "fnmadd.s"
      | Fnmadd_d -> "fnmadd.d"
      | Fmv_d -> "fmv.d"
      | Fmv_x_d -> "fmv.x.d"
      | Feq_d -> "feq.d"
      | Fle_d -> "fle.d"
      | Flt_d -> "flt.d"
      | Flt_s -> "flt.s"
      | Feq_s -> "feq.s"
      | Fle_s -> "fle.s"
      | Fmv_x_w -> "fmv.x.w"
      | Fmv_w_x -> "fmv.w.x"
      | Fcvt_w_d -> "fcvt.w.d"
      | Fcvt_wu_d -> "fcvt.wu.d"
      | Fcvt_l_d -> "fcvt.l.d"
      | Fcvt_d_w -> "fcvt.d.w"
      | Fcvt_d_wu -> "fcvt.d.wu"
      | Fcvt_s_w -> "fcvt.s.w"
      | Fcvt_s_wu -> "fcvt.s.wu"
      | Fcvt_w_s -> "fcvt.w.s"
      | Fcvt_wu_s -> "fcvt.wu.s"
      | Fcvt_s_d -> "fcvt.s.d"
      | Fcvt_d_s -> "fcvt.d.s"
      | Fcvt_s_l -> "fcvt.s.l"
      | Fcvt_lu_d -> "fcvt.lu.d"
      | Fcvt_d_l -> "fcvt.d.l"
      | Fcvt_d_lu -> "fcvt.d.lu"
      | Fcvt_l_s -> "fcvt.l.s"
      | Fcvt_lu_s -> "fcvt.lu.s"
      | Fcvt_s_lu -> "fcvt.s.lu"
      | Vsetvl -> "vsetvl"
      | Vsetvli -> "vsetvli"
      | Vsetivli -> "vsetivli"
      | Vadd_vv -> "vadd.vv"
      | Vadd_vx -> "vadd.vx"
      | Vadd_vi -> "vadd.vi"
      | Vsub_vv -> "vsub.vv"
      | Vsub_vx -> "vsub.vx"
      | Vrsub_vx -> "vrsub.vx"
      | Vrsub_vi -> "vrsub.vi"
      | Vand_vv -> "vand.vv"
      | Vand_vx -> "vand.vx"
      | Vand_vi -> "vand.vi"
      | Vor_vv -> "vor.vv"
      | Vor_vx -> "vor.vx"
      | Vor_vi -> "vor.vi"
      | Vxor_vv -> "vxor.vv"
      | Vxor_vx -> "vxor.vx"
      | Vxor_vi -> "vxor.vi"
      | Vsll_vv -> "vsll.vv"
      | Vsll_vx -> "vsll.vx"
      | Vsll_vi -> "vsll.vi"
      | Vsrl_vv -> "vsrl.vv"
      | Vsrl_vx -> "vsrl.vx"
      | Vsrl_vi -> "vsrl.vi"
      | Vsra_vv -> "vsra.vv"
      | Vsra_vx -> "vsra.vx"
      | Vsra_vi -> "vsra.vi"
      | Vminu_vv -> "vminu.vv"
      | Vminu_vx -> "vminu.vx"
      | Vmin_vv -> "vmin.vv"
      | Vmin_vx -> "vmin.vx"
      | Vmaxu_vv -> "vmaxu.vv"
      | Vmaxu_vx -> "vmaxu.vx"
      | Vmax_vv -> "vmax.vv"
      | Vmax_vx -> "vmax.vx"
      | Vmul_vv -> "vmul.vv"
      | Vmul_vx -> "vmul.vx"
      | Vmulh_vv -> "vmulh.vv"
      | Vmulh_vx -> "vmulh.vx"
      | Vmulhu_vv -> "vmulhu.vv"
      | Vmulhu_vx -> "vmulhu.vx"
      | Vmulhsu_vv -> "vmulhsu.vv"
      | Vmulhsu_vx -> "vmulhsu.vx"
      | Vdivu_vv -> "vdivu.vv"
      | Vdivu_vx -> "vdivu.vx"
      | Vdiv_vv -> "vdiv.vv"
      | Vdiv_vx -> "vdiv.vx"
      | Vremu_vv -> "vremu.vv"
      | Vremu_vx -> "vremu.vx"
      | Vrem_vv -> "vrem.vv"
      | Vrem_vx -> "vrem.vx"
      | Vaadd_vv -> "vaadd.vv"
      | Vaadd_vx -> "vaadd.vx"
      | Vaaddu_vv -> "vaaddu.vv"
      | Vaaddu_vx -> "vaaddu.vx"
      | Vasub_vv -> "vasub.vv"
      | Vasub_vx -> "vasub.vx"
      | Vasubu_vv -> "vasubu.vv"
      | Vasubu_vx -> "vasubu.vx"
      | Vnclip_wv -> "vnclip.wv"
      | Vnclip_wx -> "vnclip.wx"
      | Vnclip_wi -> "vnclip.wi"
      | Vnclipu_wv -> "vnclipu.wv"
      | Vnclipu_wx -> "vnclipu.wx"
      | Vnclipu_wi -> "vnclipu.wi"
      | Vnsra_wv -> "vnsra.wv"
      | Vnsra_wx -> "vnsra.wx"
      | Vnsra_wi -> "vnsra.wi"
      | Vnsrl_wv -> "vnsrl.wv"
      | Vnsrl_wx -> "vnsrl.wx"
      | Vnsrl_wi -> "vnsrl.wi"
      | Vssrl_vv -> "vssrl.vv"
      | Vssrl_vx -> "vssrl.vx"
      | Vssrl_vi -> "vssrl.vi"
      | Vssra_vv -> "vssra.vv"
      | Vssra_vx -> "vssra.vx"
      | Vssra_vi -> "vssra.vi"
      | Vrgather_vv -> "vrgather.vv"
      | Vrgather_vx -> "vrgather.vx"
      | Vrgather_vi -> "vrgather.vi"
      | Vrgatherei16_vv -> "vrgatherei16.vv"
      | Vwaddu_vv -> "vwaddu.vv"
      | Vwaddu_vx -> "vwaddu.vx"
      | Vwadd_vv -> "vwadd.vv"
      | Vwadd_vx -> "vwadd.vx"
      | Vwsubu_vv -> "vwsubu.vv"
      | Vwsubu_vx -> "vwsubu.vx"
      | Vwsub_vv -> "vwsub.vv"
      | Vwsub_vx -> "vwsub.vx"
      | Vwaddu_wv -> "vwaddu.wv"
      | Vwaddu_wx -> "vwaddu.wx"
      | Vwadd_wv -> "vwadd.wv"
      | Vwadd_wx -> "vwadd.wx"
      | Vwsubu_wv -> "vwsubu.wv"
      | Vwsubu_wx -> "vwsubu.wx"
      | Vwsub_wv -> "vwsub.wv"
      | Vwsub_wx -> "vwsub.wx"
      | Vwmulu_vv -> "vwmulu.vv"
      | Vwmulu_vx -> "vwmulu.vx"
      | Vwmulsu_vv -> "vwmulsu.vv"
      | Vwmulsu_vx -> "vwmulsu.vx"
      | Vwmul_vv -> "vwmul.vv"
      | Vwmul_vx -> "vwmul.vx"
      | Vsext_vf2 -> "vsext.vf2"
      | Vsext_vf4 -> "vsext.vf4"
      | Vsext_vf8 -> "vsext.vf8"
      | Vzext_vf2 -> "vzext.vf2"
      | Vzext_vf4 -> "vzext.vf4"
      | Vzext_vf8 -> "vzext.vf8"
      | Vsadd_vv -> "vsadd.vv"
      | Vsadd_vx -> "vsadd.vx"
      | Vsadd_vi -> "vsadd.vi"
      | Vsaddu_vv -> "vsaddu.vv"
      | Vsaddu_vx -> "vsaddu.vx"
      | Vsaddu_vi -> "vsaddu.vi"
      | Vssub_vv -> "vssub.vv"
      | Vssub_vx -> "vssub.vx"
      | Vssubu_vv -> "vssubu.vv"
      | Vssubu_vx -> "vssubu.vx"
      | Vmand_mm -> "vmand.mm"
      | Vmandn_mm -> "vmandn.mm"
      | Vmor_mm -> "vmor.mm"
      | Vmxor_mm -> "vmxor.mm"
      | Vmorn_mm -> "vmorn.mm"
      | Vmnand_mm -> "vmnand.mm"
      | Vmnor_mm -> "vmnor.mm"
      | Vmxnor_mm -> "vmxnor.mm"
      | Vredsum_vs -> "vredsum.vs"
      | Vredand_vs -> "vredand.vs"
      | Vredor_vs -> "vredor.vs"
      | Vredxor_vs -> "vredxor.vs"
      | Vredminu_vs -> "vredminu.vs"
      | Vredmin_vs -> "vredmin.vs"
      | Vredmaxu_vs -> "vredmaxu.vs"
      | Vredmax_vs -> "vredmax.vs"
      | Vwredsumu_vs -> "vwredsumu.vs"
      | Vwredsum_vs -> "vwredsum.vs"
      | Vmseq_vv -> "vmseq.vv"
      | Vmseq_vx -> "vmseq.vx"
      | Vmseq_vi -> "vmseq.vi"
      | Vmsne_vv -> "vmsne.vv"
      | Vmsne_vx -> "vmsne.vx"
      | Vmsne_vi -> "vmsne.vi"
      | Vmsltu_vv -> "vmsltu.vv"
      | Vmsltu_vx -> "vmsltu.vx"
      | Vmslt_vv -> "vmslt.vv"
      | Vmslt_vx -> "vmslt.vx"
      | Vmsleu_vv -> "vmsleu.vv"
      | Vmsleu_vx -> "vmsleu.vx"
      | Vmsleu_vi -> "vmsleu.vi"
      | Vmsle_vv -> "vmsle.vv"
      | Vmsle_vx -> "vmsle.vx"
      | Vmsle_vi -> "vmsle.vi"
      | Vmsgtu_vx -> "vmsgtu.vx"
      | Vmsgtu_vi -> "vmsgtu.vi"
      | Vmsgt_vx -> "vmsgt.vx"
      | Vmsgt_vi -> "vmsgt.vi"
      | Vslideup_vx -> "vslideup.vx"
      | Vslideup_vi -> "vslideup.vi"
      | Vslidedown_vx -> "vslidedown.vx"
      | Vslidedown_vi -> "vslidedown.vi"
      | Vslide1up_vx -> "vslide1up.vx"
      | Vslide1down_vx -> "vslide1down.vx"
      | Vmacc_vv -> "vmacc.vv"
      | Vmacc_vx -> "vmacc.vx"
      | Vnmsac_vv -> "vnmsac.vv"
      | Vnmsac_vx -> "vnmsac.vx"
      | Vmadd_vv -> "vmadd.vv"
      | Vmadd_vx -> "vmadd.vx"
      | Vnmsub_vv -> "vnmsub.vv"
      | Vnmsub_vx -> "vnmsub.vx"
      | Vwmaccu_vv -> "vwmaccu.vv"
      | Vwmaccu_vx -> "vwmaccu.vx"
      | Vwmacc_vv -> "vwmacc.vv"
      | Vwmacc_vx -> "vwmacc.vx"
      | Vwmaccsu_vv -> "vwmaccsu.vv"
      | Vwmaccsu_vx -> "vwmaccsu.vx"
      | Vwmaccus_vx -> "vwmaccus.vx"
      | Vid_v -> "vid.v"
      | Viota_m -> "viota.m"
      | Vcompress_vm -> "vcompress.vm"
      | Vcpop_m -> "vcpop.m"
      | Vfirst_m -> "vfirst.m"
      | Vmsbf_m -> "vmsbf.m"
      | Vmsif_m -> "vmsif.m"
      | Vmsof_m -> "vmsof.m"
      | Vadc_vvm -> "vadc.vvm"
      | Vadc_vxm -> "vadc.vxm"
      | Vadc_vim -> "vadc.vim"
      | Vmadc_vvm -> "vmadc.vvm"
      | Vmadc_vxm -> "vmadc.vxm"
      | Vmadc_vim -> "vmadc.vim"
      | Vmadc_vv -> "vmadc.vv"
      | Vmadc_vx -> "vmadc.vx"
      | Vmadc_vi -> "vmadc.vi"
      | Vsbc_vvm -> "vsbc.vvm"
      | Vsbc_vxm -> "vsbc.vxm"
      | Vmsbc_vvm -> "vmsbc.vvm"
      | Vmsbc_vxm -> "vmsbc.vxm"
      | Vmsbc_vv -> "vmsbc.vv"
      | Vmsbc_vx -> "vmsbc.vx"
      | Vmerge_vvm -> "vmerge.vvm"
      | Vmerge_vxm -> "vmerge.vxm"
      | Vmerge_vim -> "vmerge.vim"
      | Vmv_s_x -> "vmv.s.x"
      | Vmv_x_s -> "vmv.x.s"
      | Vmv_v_i -> "vmv.v.i"
      | Vmv_v_v -> "vmv.v.v"
      | Vmv_v_x -> "vmv.v.x"
      | Vmv1r_v -> "vmv1r.v"
      | Vmv2r_v -> "vmv2r.v"
      | Vmv4r_v -> "vmv4r.v"
      | Vmv8r_v -> "vmv8r.v"
      | Vsmul_vv -> "vsmul.vv"
      | Vsmul_vx -> "vsmul.vx"

    let all =
      [
        Add;
        Sub;
        Sll;
        Slt;
        Sltu;
        Snez;
        Xor;
        Srl;
        Sra;
        Or;
        And;
        Mul;
        Remu;
        Sh1add;
        Sh2add;
        Sh3add;
        Min;
        Minu;
        Max;
        Maxu;
        Andn;
        Orn;
        Xnor;
        Rol;
        Ror;
        Sh1adduw;
        Sh2adduw;
        Sh3adduw;
        Clz;
        Ctz;
        Cpop;
        Sextb;
        Sexth;
        Orcb;
        Clzw;
        Ctzw;
        Cpopw;
        Brev8;
        Rev8;
        Pack;
        Packh;
        Packw;
        Zip;
        Unzip;
        Rolw;
        Rorw;
        Rori;
        Roriw;
        Zext_h;
        Clmul;
        Clmulh;
        Xperm4;
        Xperm8;
        Sha256sum0;
        Sha256sum1;
        Sha256sig0;
        Sha256sig1;
        Sha512sum0;
        Sha512sum1;
        Sha512sig0;
        Sha512sig1;
        Sha512sum0r;
        Sha512sum1r;
        Sha512sig0l;
        Sha512sig1l;
        Sha512sig0h;
        Sha512sig1h;
        Aes64ds;
        Aes64dsm;
        Aes64es;
        Aes64esm;
        Aes64ks2;
        Aes64im;
        Aes64ks1i;
        Aes32dsi;
        Aes32dsmi;
        Aes32esi;
        Aes32esmi;
        Csrrw;
        Csrrs;
        Csrrc;
        Csrrwi;
        Csrrsi;
        Csrrci;
        Csrr;
        Csrw;
        Csrs;
        Csrc;
        Csrwi;
        Csrsi;
        Csrci;
        Amoswap_w;
        Amoadd_w;
        Amoxor_w;
        Amoand_w;
        Amoor_w;
        Amomin_w;
        Amomax_w;
        Amominu_w;
        Amomaxu_w;
        Lr_w;
        Sc_w;
        Amoswap_d;
        Amoadd_d;
        Amoxor_d;
        Amoand_d;
        Amoor_d;
        Amomin_d;
        Amomax_d;
        Amominu_d;
        Amomaxu_d;
        Lr_d;
        Sc_d;
        Addw;
        Subw;
        Sllw;
        Srlw;
        Sraw;
        Mulw;
        Addi;
        C_addi;
        Slti;
        Sltiu;
        Xori;
        Ori;
        Andi;
        Slli;
        Srli;
        Srai;
        Addiw;
        Slliw;
        Srliw;
        Sraiw;
        Sext_w;
        Lb;
        Lh;
        Lw;
        Lbu;
        Lhu;
        Lwu;
        Ld;
        Sb;
        Sh;
        Sw;
        Sd;
        Beq;
        Bne;
        Blt;
        Bge;
        Bltu;
        Bgeu;
        Bgtu;
        Bleu;
        Lui;
        Auipc;
        Jal;
        Jalr;
        Mv;
        Nop;
        J;
        Jr;
        Call;
        Tail;
        La;
        Lla;
        Li;
        Ret;
        Ecall;
        Ebreak;
        Unimp;
        Fence_i;
        Fence;
        Fld;
        Flw;
        Fsd;
        Fsw;
        Fadd_d;
        Fadd_s;
        Fsub_d;
        Fsub_s;
        Fmul_d;
        Fmul_s;
        Fdiv_d;
        Fdiv_s;
        Fneg_d;
        Fneg_s;
        Fsgnj_s;
        Fsgnj_d;
        Fsgnjn_s;
        Fsgnjn_d;
        Fsgnjx_s;
        Fsgnjx_d;
        Fmin_s;
        Fmax_s;
        Fmin_d;
        Fmax_d;
        Fsqrt_s;
        Fsqrt_d;
        Fclass_s;
        Fclass_d;
        Fmadd_s;
        Fmadd_d;
        Fmsub_s;
        Fmsub_d;
        Fnmsub_s;
        Fnmsub_d;
        Fnmadd_s;
        Fnmadd_d;
        Fmv_d;
        Fmv_x_d;
        Feq_d;
        Fle_d;
        Flt_d;
        Flt_s;
        Feq_s;
        Fle_s;
        Fmv_x_w;
        Fmv_w_x;
        Fcvt_w_d;
        Fcvt_wu_d;
        Fcvt_l_d;
        Fcvt_d_w;
        Fcvt_d_wu;
        Fcvt_s_w;
        Fcvt_s_wu;
        Fcvt_w_s;
        Fcvt_wu_s;
        Fcvt_s_d;
        Fcvt_d_s;
        Fcvt_s_l;
        Fcvt_lu_d;
        Fcvt_d_l;
        Fcvt_d_lu;
        Fcvt_l_s;
        Fcvt_lu_s;
        Fcvt_s_lu;
        Vsetvl;
        Vsetvli;
        Vsetivli;
        Vadd_vv;
        Vadd_vx;
        Vadd_vi;
        Vsub_vv;
        Vsub_vx;
        Vrsub_vx;
        Vrsub_vi;
        Vand_vv;
        Vand_vx;
        Vand_vi;
        Vor_vv;
        Vor_vx;
        Vor_vi;
        Vxor_vv;
        Vxor_vx;
        Vxor_vi;
        Vsll_vv;
        Vsll_vx;
        Vsll_vi;
        Vsrl_vv;
        Vsrl_vx;
        Vsrl_vi;
        Vsra_vv;
        Vsra_vx;
        Vsra_vi;
        Vminu_vv;
        Vminu_vx;
        Vmin_vv;
        Vmin_vx;
        Vmaxu_vv;
        Vmaxu_vx;
        Vmax_vv;
        Vmax_vx;
        Vmul_vv;
        Vmul_vx;
        Vmulh_vv;
        Vmulh_vx;
        Vmulhu_vv;
        Vmulhu_vx;
        Vmulhsu_vv;
        Vmulhsu_vx;
        Vdivu_vv;
        Vdivu_vx;
        Vdiv_vv;
        Vdiv_vx;
        Vremu_vv;
        Vremu_vx;
        Vrem_vv;
        Vrem_vx;
        Vaadd_vv;
        Vaadd_vx;
        Vaaddu_vv;
        Vaaddu_vx;
        Vasub_vv;
        Vasub_vx;
        Vasubu_vv;
        Vasubu_vx;
        Vnclip_wv;
        Vnclip_wx;
        Vnclip_wi;
        Vnclipu_wv;
        Vnclipu_wx;
        Vnclipu_wi;
        Vnsra_wv;
        Vnsra_wx;
        Vnsra_wi;
        Vnsrl_wv;
        Vnsrl_wx;
        Vnsrl_wi;
        Vssrl_vv;
        Vssrl_vx;
        Vssrl_vi;
        Vssra_vv;
        Vssra_vx;
        Vssra_vi;
        Vrgather_vv;
        Vrgather_vx;
        Vrgather_vi;
        Vrgatherei16_vv;
        Vwaddu_vv;
        Vwaddu_vx;
        Vwadd_vv;
        Vwadd_vx;
        Vwsubu_vv;
        Vwsubu_vx;
        Vwsub_vv;
        Vwsub_vx;
        Vwaddu_wv;
        Vwaddu_wx;
        Vwadd_wv;
        Vwadd_wx;
        Vwsubu_wv;
        Vwsubu_wx;
        Vwsub_wv;
        Vwsub_wx;
        Vwmulu_vv;
        Vwmulu_vx;
        Vwmulsu_vv;
        Vwmulsu_vx;
        Vwmul_vv;
        Vwmul_vx;
        Vsext_vf2;
        Vsext_vf4;
        Vsext_vf8;
        Vzext_vf2;
        Vzext_vf4;
        Vzext_vf8;
        Vsadd_vv;
        Vsadd_vx;
        Vsadd_vi;
        Vsaddu_vv;
        Vsaddu_vx;
        Vsaddu_vi;
        Vssub_vv;
        Vssub_vx;
        Vssubu_vv;
        Vssubu_vx;
        Vmand_mm;
        Vmandn_mm;
        Vmor_mm;
        Vmxor_mm;
        Vmorn_mm;
        Vmnand_mm;
        Vmnor_mm;
        Vmxnor_mm;
        Vredsum_vs;
        Vredand_vs;
        Vredor_vs;
        Vredxor_vs;
        Vredminu_vs;
        Vredmin_vs;
        Vredmaxu_vs;
        Vredmax_vs;
        Vwredsumu_vs;
        Vwredsum_vs;
        Vmseq_vv;
        Vmseq_vx;
        Vmseq_vi;
        Vmsne_vv;
        Vmsne_vx;
        Vmsne_vi;
        Vmsltu_vv;
        Vmsltu_vx;
        Vmslt_vv;
        Vmslt_vx;
        Vmsleu_vv;
        Vmsleu_vx;
        Vmsleu_vi;
        Vmsle_vv;
        Vmsle_vx;
        Vmsle_vi;
        Vmsgtu_vx;
        Vmsgtu_vi;
        Vmsgt_vx;
        Vmsgt_vi;
        Vslideup_vx;
        Vslideup_vi;
        Vslidedown_vx;
        Vslidedown_vi;
        Vslide1up_vx;
        Vslide1down_vx;
        Vmacc_vv;
        Vmacc_vx;
        Vnmsac_vv;
        Vnmsac_vx;
        Vmadd_vv;
        Vmadd_vx;
        Vnmsub_vv;
        Vnmsub_vx;
        Vwmaccu_vv;
        Vwmaccu_vx;
        Vwmacc_vv;
        Vwmacc_vx;
        Vwmaccsu_vv;
        Vwmaccsu_vx;
        Vwmaccus_vx;
        Vid_v;
        Viota_m;
        Vcompress_vm;
        Vcpop_m;
        Vfirst_m;
        Vmsbf_m;
        Vmsif_m;
        Vmsof_m;
        Vadc_vvm;
        Vadc_vxm;
        Vadc_vim;
        Vmadc_vvm;
        Vmadc_vxm;
        Vmadc_vim;
        Vmadc_vv;
        Vmadc_vx;
        Vmadc_vi;
        Vsbc_vvm;
        Vsbc_vxm;
        Vmsbc_vvm;
        Vmsbc_vxm;
        Vmsbc_vv;
        Vmsbc_vx;
        Vmerge_vvm;
        Vmerge_vxm;
        Vmerge_vim;
        Vmv_s_x;
        Vmv_x_s;
        Vmv_v_i;
        Vmv_v_v;
        Vmv_v_x;
        Vmv1r_v;
        Vmv2r_v;
        Vmv4r_v;
        Vmv8r_v;
        Vsmul_vv;
        Vsmul_vx;
      ]

    let of_mnemonic s = List.find_opt (fun op -> String.equal (name op) s) all
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

    let pp ppf i =
      if i.ops = [] then Fmt.string ppf (Opcode.name i.op)
      else Fmt.pf ppf "%s %a" (Opcode.name i.op) Fmt.(list ~sep:(any ", ") Operand.pp) i.ops
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
     explicit one (M5, asm/docs/corpus.md - perlin.c/binarytrees.c's own [(int)]/[(long)]
     cast idiom, which truncates and so must override the default with [rtz], not read
     it). [feq.d]/[fle.d]/[flt.d]/[flt.s]'s own [funct3] is a real comparison-kind selector,
     not a rounding mode, so [rm] is [None] there even though the field sits at the same
     bit position. *)
  type freg_shape = { rd_f : bool; rs1_f : bool; rs2_f : bool; arity : int; rm : int option }

  let f_shape_of_name = function
    | "fadd.d" | "fadd.s" | "fsub.d" | "fsub.s" | "fmul.d" | "fmul.s" | "fdiv.d" | "fdiv.s" ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 3; rm = Some 7 }
    | "fneg.d" | "fneg.s" | "fmv.d" ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 2; rm = None }
    | "fsgnj.s" | "fsgnj.d" | "fsgnjn.s" | "fsgnjn.d" | "fsgnjx.s" | "fsgnjx.d" | "fmin.s"
    | "fmax.s" | "fmin.d" | "fmax.d" ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 3; rm = None }
    | "feq.d" | "fle.d" | "flt.d" | "flt.s" | "feq.s" | "fle.s" ->
        Some { rd_f = false; rs1_f = true; rs2_f = true; arity = 3; rm = None }
    | "fcvt.w.d" | "fcvt.wu.d" | "fcvt.l.d" | "fcvt.lu.d" | "fcvt.w.s" | "fcvt.wu.s" | "fcvt.l.s"
    | "fcvt.lu.s" ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = Some 7 }
    | "fmv.x.d" | "fmv.x.w" ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = None }
    | "fmv.w.x" -> Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = None }
    | "fclass.s" | "fclass.d" ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = None }
    | "fsqrt.s" | "fsqrt.d" ->
        Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some 7 }
    | "fcvt.d.w" | "fcvt.d.wu" ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some 0 }
    | "fcvt.s.w" | "fcvt.s.wu" | "fcvt.s.l" | "fcvt.s.lu" | "fcvt.d.l" | "fcvt.d.lu" ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some 7 }
    | "fcvt.s.d" -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some 7 }
    | "fcvt.d.s" -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some 0 }
    | _ -> None

  (* The six standard rounding-mode names (RISC-V unprivileged spec table 11.2), shared by
     {!freg_shape}'s printing and by [lower_instruction]'s parsing of an explicit third
     operand on a convert. *)
  let rounding_modes = [ ("rne", 0); ("rtz", 1); ("rdn", 2); ("rup", 3); ("rmm", 4); ("dyn", 7) ]
  let rounding_mode_of_name s = List.assoc_opt s rounding_modes
  let rounding_name_of_mode m = List.find_opt (fun (_, v) -> v = m) rounding_modes |> Option.map fst

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
          (** [fld]/[flw]'s own literal-pool pseudo (M5, asm/docs/corpus.md - almabench.c/
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
      | R of {
          name : string;
          opcode : int;
          funct3 : int;
          funct7 : int;
          rd : int;
          rs1 : int;
          rs2 : int;
        }
      | R4 of {
          name : string;
          opcode : int;
          fmt : int;
          funct3 : int;
          rd : int;
          rs1 : int;
          rs2 : int;
          rs3 : int;
        }
          (** [fmadd]/[fmsub]/[fnmsub]/[fnmadd] - RISC-V's only R4-type instructions,
              with a fourth real register operand ([rs3]) {!R} has no field for; see
              {!f_fma_desc}. *)
      | I of {
          name : string;
          opcode : int;
          funct3 : int;
          funct_hi : int;
          shamt_bits : int option;
          rd : int;
          rs1 : int;
          imm : Asm_core.Expr.t;
        }
      | S of {
          name : string;
          opcode : int;
          funct3 : int;
          rs1 : int;
          rs2 : int;
          imm : Asm_core.Expr.t;
        }
          (** [opcode] defaults to STORE's [0x23] for every integer store; the F/D extension's
              [fsd]/[fsw] are the only other user, at STORE-FP's [0x27] (M5, asm/docs/corpus.md
              - almabench.c/bisect.c/... callee-saved double spills). Everything else about the
              S-type word - the split 12-bit immediate, [rs1]/[rs2] field positions - is
              identical between the two opcodes, so this is one field rather than a second
              constructor. *)
      | B of { name : string; funct3 : int; rs1 : int; rs2 : int; target : Asm_core.Expr.t }
      | U of { name : string; opcode : int; rd : int; imm : Asm_core.Expr.t }
      | J of { rd : int; target : Asm_core.Expr.t }
      | Pair of { name : string; rd : int; tmp : int; target : Asm_core.Expr.t; kind : pair_kind }
      | Fixed of { name : string; word : int64 }
      | Caddi of { rd : int; imm : Asm_core.Expr.t }

    let pp_expr ppf e = Asm_core.Expr.pp ppf e
    let reg_name isf n = Printf.sprintf "%s%d" (if isf then "f" else "x") n

    let rm_suffix rm funct3 =
      match rm with
      | Some default when default <> funct3 -> (
          match rounding_name_of_mode funct3 with Some n -> ", " ^ n | None -> "")
      | Some _ | None -> ""

    (* V's vm bit lives in [funct7]'s low bit ([funct7 = (funct6 lsl 1) lor
       vm]) - unmasked (vm = 1) prints no suffix, masked (vm = 0) prints the
       trailing [, v0.t] real GNU as's own disassembly always shows
       explicitly (there is no "default mask" elision the way [rm_suffix]
       elides a default rounding mode). *)
    let vm_suffix funct7 = if funct7 land 1 = 1 then "" else ", v0.t"

    (* Every OPIVV/OPIVX/OPIVI mnemonic admitted so far ([vadd]/[vsub]'s
       [.vv]/[.vx] siblings, [vrsub]'s [.vx]/[.vi] siblings, and
       [vand]/[vor]/[vxor]'s full [.vv]/[.vx]/[.vi] triples) shares exactly
       one of these three print shapes, keyed only by the GAS suffix on the
       mnemonic string rather than by opcode - so one table-free string
       check covers the whole admitted family instead of one match arm per
       mnemonic. *)
    let is_opivv_name name =
      String.length name >= 3 && String.sub name (String.length name - 3) 3 = ".vv"

    let is_opivx_name name =
      String.length name >= 3 && String.sub name (String.length name - 3) 3 = ".vx"

    let is_opivi_name name =
      String.length name >= 3 && String.sub name (String.length name - 3) 3 = ".vi"

    (* [vsll.vi]/[vsrl.vi]/[vsra.vi] carry an UNSIGNED 5-bit shift amount
       (riscv-opcodes' own field name is [zimm5], range 0..31 - real GNU as
       rejects both [-1] and [32]) rather than every other admitted [.vi]
       mnemonic's signed [simm5] (-16..15), so the shared suffix-keyed print
       shape above must not sign-extend these three. *)
    let is_opivi_uimm_name = function "vsll.vi" | "vsrl.vi" | "vsra.vi" -> true | _ -> false

    let pp ppf = function
      | R x when is_opivv_name x.name ->
          Fmt.pf ppf "%s v%d, v%d, v%d%s" x.name x.rd x.rs2 x.rs1 (vm_suffix x.funct7)
      | R x when is_opivx_name x.name ->
          Fmt.pf ppf "%s v%d, v%d, x%d%s" x.name x.rd x.rs2 x.rs1 (vm_suffix x.funct7)
      | R x when is_opivi_name x.name ->
          let imm =
            if is_opivi_uimm_name x.name then x.rs1
            else if x.rs1 land 0x10 <> 0 then x.rs1 - 32
            else x.rs1
          in
          Fmt.pf ppf "%s v%d, v%d, %d%s" x.name x.rd x.rs2 imm (vm_suffix x.funct7)
      | R x -> (
          match f_shape_of_name x.name with
          | Some { rd_f; rs1_f; rs2_f = _; arity = 2; rm } ->
              Fmt.pf ppf "%s %s, %s%s" x.name (reg_name rd_f x.rd) (reg_name rs1_f x.rs1)
                (rm_suffix rm x.funct3)
          | Some { rd_f; rs1_f; rs2_f; arity = _; rm } ->
              Fmt.pf ppf "%s %s, %s, %s%s" x.name (reg_name rd_f x.rd) (reg_name rs1_f x.rs1)
                (reg_name rs2_f x.rs2) (rm_suffix rm x.funct3)
          | None -> Fmt.pf ppf "%s x%d, x%d, x%d" x.name x.rd x.rs1 x.rs2)
      | R4 x ->
          Fmt.pf ppf "%s f%d, f%d, f%d, f%d%s" x.name x.rd x.rs1 x.rs2 x.rs3
            (rm_suffix (Some 7) x.funct3)
      | I x when x.opcode = 0x03 || x.opcode = 0x67 ->
          Fmt.pf ppf "%s x%d, %a(x%d)" x.name x.rd pp_expr x.imm x.rs1
      | I x when x.opcode = 0x07 -> Fmt.pf ppf "%s f%d, %a(x%d)" x.name x.rd pp_expr x.imm x.rs1
      | I x -> Fmt.pf ppf "%s x%d, x%d, %a" x.name x.rd x.rs1 pp_expr x.imm
      | S x when x.opcode = 0x27 -> Fmt.pf ppf "%s f%d, %a(x%d)" x.name x.rs2 pp_expr x.imm x.rs1
      | S x -> Fmt.pf ppf "%s x%d, %a(x%d)" x.name x.rs2 pp_expr x.imm x.rs1
      | B x -> Fmt.pf ppf "%s x%d, x%d, %a" x.name x.rs1 x.rs2 pp_expr x.target
      | U x -> Fmt.pf ppf "%s x%d, %a" x.name x.rd pp_expr x.imm
      | J x -> Fmt.pf ppf "jal x%d, %a" x.rd pp_expr x.target
      | Pair ({ kind = Fload _; _ } as x) ->
          Fmt.pf ppf "%s f%d, %a, x%d" x.name x.rd pp_expr x.target x.tmp
      | Pair x -> Fmt.pf ppf "%s x%d, %a" x.name x.rd pp_expr x.target
      | Fixed x -> Fmt.string ppf x.name
      | Caddi x -> Fmt.pf ppf "c.addi x%d, %a" x.rd pp_expr x.imm

    let equal (a : t) b = a = b
  end

  type fixup_kind =
    | Abs32
    | Abs64
    | Branch13
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
    | Branch13 | Jal21 -> "pcrel-branch"
    | Pcrel_hi20 | Pcrel_lo12_i | Pcrel_lo12_s -> "pcrel-address"
    | Call_hi20 | Call_lo12_i -> "pcrel-call"
    | Abs_hi20 | Abs_lo12_i | Abs_lo12_s -> "absolute-address"

  let fixup_role = function
    | Branch13 | Jal21 -> Asm_core.Lowered_ast.Branch
    | Call_hi20 | Call_lo12_i -> Asm_core.Lowered_ast.Call
    | _ -> Asm_core.Lowered_ast.Data_address

  type feature = No_features

  type target_state = {
    pic : bool;
    relax : bool;
    rvc : bool;
    option_stack : (bool * bool * bool) list;
  }

  let default_state = { pic = false; relax = false; rvc = false; option_stack = [] }
  let default_features = []

  type error_kind =
    [ Target_error.shared
    | `Wrong_operands of string
    | `Rv64_only of string
    | `Rv32_only of string
    | `Immediate_range of string
    | `Immediate_alignment of string
    | `Bad_modifier of string
    | `Compressed_disabled of string
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
    | `Compressed_disabled _ -> P.name ^ ".lower"
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

  let simplify_instruction ~features:_ s =
    match Opcode.of_mnemonic s.Surface.mnemonic with
    | None -> Error (diag ~pos:__POS__ ~origin:s.origin (`Unknown_instruction s.mnemonic))
    | Some op -> Ok { Instruction.op; ops = s.ops }

  let const n = Asm_core.Expr.Const (Bigint.of_int n)

  let expr_of = function
    | Operand.Imm n -> Some (Asm_core.Expr.Const n)
    | Operand.Sym e -> Some e
    | _ -> None

  let xreg = function Operand.Reg r -> Reg.x r | _ -> None
  let freg = function Operand.Reg (Reg.F n) -> Some n | _ -> None
  let vreg = function Operand.Reg (Reg.V n) -> Some n | _ -> None

  (* V's masked-operand marker: a bare trailing [v0.t] (never a real register
     by itself - [Reg.find] never returns a [V] for a name with a [.t] suffix,
     so it falls through {!parse_one}'s generic bare-identifier case to
     [Operand.Sym (Symbol "v0.t")] with no frontend change needed, the same
     way vsetvli/vsetivli's keyword tokens do). *)
  let is_v0t = function Operand.Sym (Asm_core.Expr.Symbol "v0.t") -> true | _ -> false

  (* The add-with-carry/subtract-with-borrow family's mandatory carry-in
     operand: a real, literal [v0] register (not the [v0.t] mask-toggle
     sigil above) - real GNU as accepts only [v0] there, rejecting both
     [v0.t] and any other vector register as "illegal operands". *)
  let is_v0 = function Operand.Reg (Reg.V 0) -> true | _ -> false
  let rv64 op = if xlen = 64 then Ok () else Error (diag ~pos:__POS__ (`Rv64_only op))
  let wrong op = Error (diag ~pos:__POS__ (`Wrong_operands op))

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
    | Mul -> Some (0x33, 0, 0x01)
    | Remu -> Some (0x33, 7, 0x01)
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
    | Clmul -> Some (0x33, 1, 0x05)
    | Clmulh -> Some (0x33, 3, 0x05)
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
    | Mulw -> Some (0x3b, 0, 0x01)
    | Pack -> Some (0x33, 4, 0x04)
    | Packh -> Some (0x33, 7, 0x04)
    | Packw -> Some (0x3b, 4, 0x04)
    | Rolw -> Some (0x3b, 1, 0x30)
    | Rorw -> Some (0x3b, 5, 0x30)
    (* Vector configuration-setting instruction with two GPR sources
       ([vsetvl rd, rs1, rs2] - the AVL requested in [rs1], the desired
       vtype encoded in [rs2] rather than an immediate; {!r_type_gpr_form}'s
       plain [rd, rs1, rs2] shape applies unchanged since no vector register
       is involved). OP-V's major opcode 0x57 is otherwise unused by this
       family (OP-FP is the distinct 0x53). Confirmed against real GNU as:
       `vsetvl a0, a1, a2` -> `80c5f557` on both riscv32-linux-gnu-as 2.43.1
       (-march=rv32iv) and riscv64-linux-gnu-as 2.44 (-march=rv64iv). *)
    | Vsetvl -> Some (0x57, 7, 0x40)
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
     evidence, asm/docs/corpus.md - almabench.c/bisect.c/fftsp.c/knucleotide.c/...).
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

  (* [fneg.d]/[fneg.s]/[fmv.d] - FSGNJN/FSGNJ with [rs2] forced equal to [rs1], real
     hardware's own alias (verified against real riscv64-linux-gnu-as/objdump:
     `fneg.d fs0, fs1` -> `22949453`, decoding back with [rs1] = [rs2] = 9). The
     general two-different-register [fsgnj]/[fsgnjn]/[fsgnjx] this shares a word
     with is {!f_sgnj3_desc}. *)
  let f_sgnj_desc = function
    | Opcode.Fneg_s -> Some (1, 0x10)
    | Fneg_d -> Some (1, 0x11)
    | Fmv_d -> Some (0, 0x11)
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
     0x70, funct3 = 0, matching [fclass.s]'s own group). *)
  let f_mv_x_w_desc = function Opcode.Fmv_x_w -> Some (0, 0x70, 0) | _ -> None

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

  let fits_signed bits v =
    let lim = Int64.shift_left 1L (bits - 1) in
    Int64.compare v (Int64.neg lim) >= 0 && Int64.compare v lim < 0

  let fits_unsigned bits v =
    let lim = Int64.shift_left 1L bits in
    Int64.compare v 0L >= 0 && Int64.compare v lim < 0

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

  (* Zaamo's [(rs1)] memory operand carries no real offset field - GAS
     accepts the [offset(base)] syntax generically, so this rejects any
     concrete nonzero fold rather than silently dropping it, matching real
     GNU as's own "illegal operands" rejection of `amoadd.w a0, a1, 4(a2)`. *)
  let zero_offset e = match int64_expr e with Some 0L -> true | _ -> false

  (* V's "vtype keyword list" - vsetvli/vsetivli's own trailing
     [e<SEW>][,m<LMUL>][,ta|tu][,ma|mu] syntax, not a register, plain
     immediate, or memory operand the generic text parser already has a
     name for; each bare identifier ([Operand.Sym (Symbol _)], produced by
     {!parse_one}'s already-generic bare-identifier fallback with no
     frontend changes needed) selects one of four independent categories.
     Real GNU as accepts any subset of the four - even a single trailing
     keyword, skipping every earlier category - but rejects them out of
     the fixed SEW < LMUL < tail-policy < mask-policy order, and rejects
     omitting every keyword. Confirmed against real GNU as: `vsetvli a0,
     a1, ta` alone (skipping SEW/LMUL) assembles as e8,m1,ta,mu; `vsetvli
     a0, a1, m1, e32` and `vsetvli a0, a1, ma, ta` are both "illegal
     operands"; `vsetvli a0, a1` (no keyword at all) is too. *)
  let vlmul_bits = function
    | "m1" -> Some 0
    | "m2" -> Some 1
    | "m4" -> Some 2
    | "m8" -> Some 3
    | "mf8" -> Some 5
    | "mf4" -> Some 6
    | "mf2" -> Some 7
    | _ -> None

  let vsew_bits = function
    | "e8" -> Some 0
    | "e16" -> Some 1
    | "e32" -> Some 2
    | "e64" -> Some 3
    | _ -> None

  let vtype_token = function
    | "ta" -> Some (2, 1 lsl 6)
    | "tu" -> Some (2, 0)
    | "ma" -> Some (3, 1 lsl 7)
    | "mu" -> Some (3, 0)
    | s -> (
        match vsew_bits s with
        | Some b -> Some (0, b lsl 3)
        | None -> ( match vlmul_bits s with Some b -> Some (1, b) | None -> None))

  let vtype_value_of tokens =
    let rec go last_category acc = function
      | [] -> Some acc
      | tok :: rest -> (
          match vtype_token tok with
          | Some (category, bits) when category > last_category -> go category (acc lor bits) rest
          | _ -> None)
    in
    go (-1) 0 tokens

  (* OP-V's funct6 (bits[31:26], the field alongside vm that together fill
     the R-type [funct7] span) for each admitted OPIVV/OPIVX/OPIVI mnemonic,
     one small table per operand shape rather than duplicating six lowering
     match arms per mnemonic - {!lower_instruction} composes
     [funct7 = (funct6 lsl 1) lor vm] generically from whichever table
     matches. Hand-verified against riscv-opcodes' own encoding and real
     GNU as (`riscv64-linux-gnu-as` 2.44, `-march=rv64gv`) for every entry:
     `vsub.vv v1,v2,v3` -> `0a2180d7`, `vsub.vx v1,v2,a0` -> `0a2540d7`,
     `vrsub.vx v1,v2,a0` -> `0e2540d7`, `vrsub.vi v1,v2,-5` -> `0e2db0d7`,
     `vand.vv/.vx/.vi` -> `262180d7`/`262540d7`/`262db0d7`,
     `vor.vv/.vx/.vi` -> `2a2180d7`/`2a2540d7`/`2a2db0d7`,
     `vxor.vv/.vx/.vi` -> `2e2180d7`/`2e2540d7`/`2e2db0d7` - all with vm=1
     (unmasked). Real GNU as also confirms `vsub`/`vrsub` are genuinely
     asymmetric ("vsub.vi"/"vrsub.vv" are both "unrecognized opcode", not
     "illegal operands"), matching riscv-opcodes' own export, which has no
     such records either. *)
  let opivv_funct6 = function
    | Opcode.Vadd_vv -> Some 0x00
    | Vsub_vv -> Some 0x02
    | Vand_vv -> Some 0x09
    | Vor_vv -> Some 0x0a
    | Vxor_vv -> Some 0x0b
    | Vsll_vv -> Some 0x25
    | Vsrl_vv -> Some 0x28
    | Vsra_vv -> Some 0x29
    | Vminu_vv -> Some 0x04
    | Vmin_vv -> Some 0x05
    | Vmaxu_vv -> Some 0x06
    | Vmax_vv -> Some 0x07
    | Vsaddu_vv -> Some 0x20
    | Vsadd_vv -> Some 0x21
    | Vssubu_vv -> Some 0x22
    | Vssub_vv -> Some 0x23
    | Vnsrl_wv -> Some 0x2c
    | Vnsra_wv -> Some 0x2d
    | Vnclipu_wv -> Some 0x2e
    | Vnclip_wv -> Some 0x2f
    | Vssrl_vv -> Some 0x2a
    | Vssra_vv -> Some 0x2b
    | Vrgather_vv -> Some 0x0c
    | Vrgatherei16_vv -> Some 0x0e
    (* [vwredsumu.vs]/[vwredsum.vs]: the widening-sum reduction pair - unlike
       every other reduction below, riscv-opcodes' own encoding puts these in
       OPIVV's funct3=0 space (confirmed against real GNU as: `vwredsumu.vs
       v1,v2,v3` -> `c22180d7`, funct3 bits are 0, not OPMVV's 2), so they
       belong in this table rather than {!opmvv_funct6}, even though 0x30/
       0x31 already label unrelated OPMVV entries there (funct3 keeps the
       two spaces disjoint). No [.vx]/[.vi] sibling exists for either. *)
    | Vwredsumu_vs -> Some 0x30
    | Vwredsum_vs -> Some 0x31
    (* [vmseq]/[vmsne]/[vmsltu]/[vmslt]/[vmsleu]/[vmsle]: the vector-vector
       forms of OP-V's mask-writing comparison family - the same OPIVV shape
       as [vadd.vv]/etc. ([vd] just holds a MASK result rather than a plain
       vector, invisible to this encoder layer). [vmsgtu]/[vmsgt] have no
       [.vv] sibling (real GNU as accepts `vmsgt.vv`/`vmsgtu.vv` only as a
       pseudo-instruction reversing `vmslt.vv`/`vmsltu.vv`'s own operands -
       e.g. `vmsgt.vv v1,v2,v3` assembles as `vmslt.vv v1,v3,v2` - a
       genuinely different alias-expansion feature, deliberately not
       admitted here). Confirmed against real GNU as, byte-identical on
       RV32/RV64: `vmseq.vv v1,v2,v3` -> `622180d7`, `vmsne.vv` ->
       `662180d7`, `vmsltu.vv` -> `6a2180d7`, `vmslt.vv` -> `6e2180d7`,
       `vmsleu.vv` -> `722180d7`, `vmsle.vv` -> `762180d7`. *)
    | Vmseq_vv -> Some 0x18
    | Vmsne_vv -> Some 0x19
    | Vmsltu_vv -> Some 0x1a
    | Vmslt_vv -> Some 0x1b
    | Vmsleu_vv -> Some 0x1c
    | Vmsle_vv -> Some 0x1d
    (* [vsmul]: the saturating fixed-point multiply pair - the same OPIVV
       (funct3 = 0)/OPIVX (funct3 = 4) shape as [vadd]/etc. above, not
       OPMVV/OPMVX despite being a multiply (confirmed against real GNU
       as: `vsmul.vv v1,v2,v3` -> `9e2180d7`, funct3 bits are 0). No
       [.vi] sibling (real GNU as rejects `vsmul.vi` as "unrecognized
       opcode"). Confirmed against real GNU as, byte-identical on
       RV32/RV64: `vsmul.vx v1,v2,a0` -> `9e2540d7`; masked, e.g.
       `vsmul.vv v1,v2,v3,v0.t` -> `9c2180d7`. *)
    | Vsmul_vv -> Some 0x27
    | _ -> None

  let opivx_funct6 = function
    | Opcode.Vadd_vx -> Some 0x00
    | Vsub_vx -> Some 0x02
    | Vrsub_vx -> Some 0x03
    | Vand_vx -> Some 0x09
    | Vor_vx -> Some 0x0a
    | Vxor_vx -> Some 0x0b
    | Vsll_vx -> Some 0x25
    | Vsrl_vx -> Some 0x28
    | Vsra_vx -> Some 0x29
    | Vminu_vx -> Some 0x04
    | Vmin_vx -> Some 0x05
    | Vmaxu_vx -> Some 0x06
    | Vmax_vx -> Some 0x07
    | Vsaddu_vx -> Some 0x20
    | Vsadd_vx -> Some 0x21
    | Vssubu_vx -> Some 0x22
    | Vssub_vx -> Some 0x23
    | Vnsrl_wx -> Some 0x2c
    | Vnsra_wx -> Some 0x2d
    | Vnclipu_wx -> Some 0x2e
    | Vnclip_wx -> Some 0x2f
    | Vssrl_vx -> Some 0x2a
    | Vssra_vx -> Some 0x2b
    | Vrgather_vx -> Some 0x0c
    (* [vmseq]/[vmsne]/[vmsltu]/[vmslt]/[vmsleu]/[vmsle]/[vmsgtu]/[vmsgt]:
       the vector-scalar forms of the mask-writing comparison family above -
       unlike the [.vv] shape, [vmsgtu]/[vmsgt] DO have a [.vx] sibling
       (real GNU as's own text-order convention keeps `gt` meaningful when
       comparing against a scalar broadcast, unlike vector-vector where it
       is redundant with a reversed `lt`). Confirmed against real GNU as,
       byte-identical on RV32/RV64: `vmseq.vx v1,v2,a0` -> `622540d7`,
       `vmsne.vx` -> `662540d7`, `vmsltu.vx` -> `6a2540d7`, `vmslt.vx` ->
       `6e2540d7`, `vmsleu.vx` -> `722540d7`, `vmsle.vx` -> `762540d7`,
       `vmsgtu.vx` -> `7a2540d7`, `vmsgt.vx` -> `7e2540d7`. *)
    | Vmseq_vx -> Some 0x18
    | Vmsne_vx -> Some 0x19
    | Vmsltu_vx -> Some 0x1a
    | Vmslt_vx -> Some 0x1b
    | Vmsleu_vx -> Some 0x1c
    | Vmsle_vx -> Some 0x1d
    | Vmsgtu_vx -> Some 0x1e
    | Vmsgt_vx -> Some 0x1f
    (* [vslideup]/[vslidedown]: OP-V's slide family - moves elements up/down
       by a scalar or immediate offset. Same OPIVX shape as every other
       [.vx] mnemonic (no [.vv] sibling - real GNU as rejects
       `vslideup.vv` as "unrecognized opcode", matching riscv-opcodes'
       own export). Confirmed against real GNU as, byte-identical on
       RV32/RV64: `vslideup.vx v1,v2,a0` -> `3a2540d7`, `vslidedown.vx` ->
       `3e2540d7`. *)
    | Vslideup_vx -> Some 0x0e
    | Vslidedown_vx -> Some 0x0f
    | Vsmul_vx -> Some 0x27
    | _ -> None

  let opivi_funct6 = function
    | Opcode.Vadd_vi -> Some 0x00
    | Vrsub_vi -> Some 0x03
    | Vand_vi -> Some 0x09
    | Vor_vi -> Some 0x0a
    | Vxor_vi -> Some 0x0b
    | Vsll_vi -> Some 0x25
    | Vsrl_vi -> Some 0x28
    | Vsra_vi -> Some 0x29
    | Vsaddu_vi -> Some 0x20
    | Vsadd_vi -> Some 0x21
    | Vnsrl_wi -> Some 0x2c
    | Vnsra_wi -> Some 0x2d
    | Vnclipu_wi -> Some 0x2e
    | Vnclip_wi -> Some 0x2f
    | Vssrl_vi -> Some 0x2a
    | Vssra_vi -> Some 0x2b
    | Vrgather_vi -> Some 0x0c
    (* [vmseq]/[vmsne]/[vmsleu]/[vmsle]/[vmsgtu]/[vmsgt]: the
       vector-immediate forms of the mask-writing comparison family above -
       [vmsltu]/[vmslt] have no [.vi] sibling (real GNU as rejects
       `vmslt.vi` as "unrecognized opcode": an immediate strict-less-than
       is redundant with `vmsleu`/`vmsle` against one-less, which riscv-
       opcodes' own export already omits accordingly). The immediate is
       SIGNED [simm5], like every other admitted [.vi] mnemonic except the
       shift-family UNSIGNED [zimm5] shapes. Confirmed against real GNU as,
       byte-identical on RV32/RV64: `vmseq.vi v1,v2,-5` -> `622db0d7`,
       `vmsne.vi` -> `662db0d7`, `vmsleu.vi` -> `722db0d7`, `vmsle.vi` ->
       `762db0d7`, `vmsgtu.vi` -> `7a2db0d7`, `vmsgt.vi` -> `7e2db0d7`. *)
    | Vmseq_vi -> Some 0x18
    | Vmsne_vi -> Some 0x19
    | Vmsleu_vi -> Some 0x1c
    | Vmsle_vi -> Some 0x1d
    | Vmsgtu_vi -> Some 0x1e
    | Vmsgt_vi -> Some 0x1f
    (* [vslideup.vi]/[vslidedown.vi]: {!Vslideup_vx}/[Vslidedown_vx]'s
       immediate siblings - riscv-opcodes' own field name is UNSIGNED
       [zimm5] (0..31, see {!opivi_unsigned}), not every other admitted
       [.vi] mnemonic's SIGNED [simm5]. Confirmed against real GNU as,
       byte-identical on RV32/RV64: `vslideup.vi v1,v2,5` -> `3a22b0d7`,
       `vslideup.vi v1,v2,31` -> `3a2fb0d7` (accepted), `,32` rejected with
       "bad value for vector immediate field, value must be 0...31";
       `vslidedown.vi v1,v2,5` -> `3e22b0d7`. *)
    | Vslideup_vi -> Some 0x0e
    | Vslidedown_vi -> Some 0x0f
    | _ -> None

  (* The saturating add/subtract family - `vsaddu`/`vsadd`/`vssubu`/`vssub` -
     shares the OPIVV/OPIVX/OPIVI shape verbatim (the assembler only encodes
     the instruction; saturation itself is execution-time behavior, invisible
     at this layer). `vsadd`/`vsaddu`'s [.vi] immediate is SIGNED [simm5],
     like every other admitted [.vi] mnemonic except the shift trio above;
     `vssub`/`vssubu` have no [.vi] sibling at all (real GNU as rejects
     `vssub.vi` as "unrecognized opcode", matching riscv-opcodes' own
     export). Confirmed against real GNU as, byte-identical on RV32/RV64:
     `vsaddu.vv/.vx/.vi` -> `822180d7`/`822540d7`/`822db0d7`, `vsadd.vv/.vx/
     .vi` -> `862180d7`/`862540d7`/`862db0d7`, `vssubu.vv/.vx` ->
     `8a2180d7`/`8a2540d7`, `vssub.vv/.vx` -> `8e2180d7`/`8e2540d7`. *)

  (* [vsll.vi]/[vsrl.vi]/[vsra.vi]'s shift-amount immediate is UNSIGNED
     (riscv-opcodes' own field name is [zimm5], range 0..31) unlike every
     other admitted [.vi] mnemonic's SIGNED [simm5] (-16..15). Confirmed
     against real GNU as: `vsll.vi v1,v2,31` -> `962fb0d7` (accepted),
     `vsll.vi v1,v2,32`/`,-1` both "bad value for vector immediate field,
     value must be 0...31" - the mirror image of `vand.vi v1,v2,16`/`,-17`'s
     own signed-range rejection message. The narrowing `.wi` family below -
     `vnsrl.wi`/`vnsra.wi`/`vnclipu.wi`/`vnclip.wi` - shares this same
     UNSIGNED [zimm5] shape (confirmed from riscv-opcodes' own
     `provenance.operands`, and against real GNU as: `vnclip.wi v1,v2,31`
     -> `be2fb0d7`, `vnclip.wi v1,v2,32` rejected with the identical
     "value must be 0...31" message). The scaling shift family below -
     `vssrl.vi`/`vssra.vi` - shares this same UNSIGNED [zimm5] shape too
     (confirmed from riscv-opcodes' own `provenance.operands`, and against
     real GNU as: `vssrl.vi v1,v2,31` -> `aa2fb0d7`). `vrgather.vi`'s index
     immediate is UNSIGNED too (confirmed: `vrgather.vi v1,v2,31` ->
     `322fb0d7`, `vrgather.vi v1,v2,32` rejected with the identical
     "value must be 0...31" message). *)
  let opivi_unsigned = function
    | Opcode.Vsll_vi | Vsrl_vi | Vsra_vi | Vnsrl_wi | Vnsra_wi | Vnclipu_wi | Vnclip_wi | Vssrl_vi
    | Vssra_vi | Vrgather_vi | Vslideup_vi | Vslidedown_vi ->
        true
    | _ -> false

  (* The add-with-carry/subtract-with-borrow family - [vadc]/[vmadc]/
     [vsbc]/[vmsbc] - is structurally the same OPIVV(funct3=0)/
     OPIVX(funct3=4)/OPIVI(funct3=3) shape as {!opivv_funct6}/
     {!opivx_funct6}/{!opivi_funct6} above, but [vm] is never a toggleable
     mask here: riscv-opcodes' own mask for every one of these 15 records
     covers bit 25 (0xfe00707f, not the usual 0xfc00707f), meaning [vm] is
     baked into each opcode's own fixed encoding rather than left free.
     The "m"-suffixed variants ([vadc.vvm]/[vmadc.vvm]/[vsbc.vvm]/
     [vmsbc.vvm] and their [.vxm]/[.vim] siblings) have [vm] fixed at 0
     and GAS syntax requires a literal, mandatory 4th operand spelled
     exactly [v0] (not the [v0.t] mask-toggle sigil {!is_v0t} recognizes,
     and not any other vector register) supplying the carry/borrow input;
     the bare (non-"m") siblings - only [vmadc.vv]/[.vx]/[.vi] and
     [vmsbc.vv]/[.vx] exist, since compare-with-carry/borrow can run with
     no carry-in but add/subtract cannot - have [vm] fixed at 1 and take
     no 4th operand at all, with no [, v0.t] masked form either.
     Confirmed against real GNU as (`riscv64-linux-gnu-as` 2.44
     `-march=rv64gv`, byte-identical on `riscv32-linux-gnu-as` 2.43.1
     `-march=rv32gv`): `vadc.vvm v1,v2,v3,v0` -> `402180d7`, `vadc.vxm
     v1,v2,a0,v0` -> `402540d7`, `vadc.vim v1,v2,5,v0` -> `4022b0d7`,
     `vmadc.vvm v1,v2,v3,v0` -> `442180d7`, `vmadc.vv v1,v2,v3` ->
     `462180d7`, `vsbc.vvm v1,v2,v3,v0` -> `482180d7`, `vmsbc.vvm
     v1,v2,v3,v0` -> `4c2180d7`, `vmsbc.vv v1,v2,v3` -> `4e2180d7`
     (funct6 0x10/0x11/0x12/0x13 for vadc/vmadc/vsbc/vmsbc respectively,
     identical whether "m"-suffixed or bare); `vadc.vvm v1,v2,v3` (no
     carry-in operand), `vmadc.vv v1,v2,v3,v0` (a carry-in operand on the
     bare form), `vadc.vvm v1,v2,v3,v0.t` (the mask-toggle sigil instead
     of a real [v0]), and `vadc.vvm v1,v2,v3,v1` (any other vector
     register) are all "illegal operands" on real GNU as. [vmerge]
     (funct6 0x17) shares this exact mandatory-[v0] shape too, with no
     bare (non-"m") sibling at all - real GNU as rejects `vmerge.vvm
     v1,v2,v3` (no carry-in operand) as "illegal operands". Confirmed
     against real GNU as, byte-identical on RV32/RV64: `vmerge.vvm
     v1,v2,v3,v0` -> `5c2180d7`, `vmerge.vxm v1,v2,a0,v0` -> `5c2540d7`,
     `vmerge.vim v1,v2,5,v0` -> `5c22b0d7`. *)
  let carry_m_vv_funct6 = function
    | Opcode.Vadc_vvm -> Some 0x10
    | Vmadc_vvm -> Some 0x11
    | Vsbc_vvm -> Some 0x12
    | Vmsbc_vvm -> Some 0x13
    | Vmerge_vvm -> Some 0x17
    | _ -> None

  let carry_m_vx_funct6 = function
    | Opcode.Vadc_vxm -> Some 0x10
    | Vmadc_vxm -> Some 0x11
    | Vsbc_vxm -> Some 0x12
    | Vmsbc_vxm -> Some 0x13
    | Vmerge_vxm -> Some 0x17
    | _ -> None

  let carry_m_vi_funct6 = function
    | Opcode.Vadc_vim -> Some 0x10
    | Vmadc_vim -> Some 0x11
    | Vmerge_vim -> Some 0x17
    | _ -> None

  let carry_vv_funct6 = function Opcode.Vmadc_vv -> Some 0x11 | Vmsbc_vv -> Some 0x13 | _ -> None
  let carry_vx_funct6 = function Opcode.Vmadc_vx -> Some 0x11 | Vmsbc_vx -> Some 0x13 | _ -> None
  let carry_vi_funct6 = function Opcode.Vmadc_vi -> Some 0x11 | _ -> None

  (* OP-V's second major-opcode-0x57 functional-unit group: OPMVV
     (funct3 = 2, all-vector-register) and OPMVX (funct3 = 6,
     scalar-broadcast) - the identical funct6-plus-vm-into-funct7
     composition as OPIVV/OPIVX above, just a different funct3 pair and no
     immediate ([.vi]) sibling (multiply has none in the base V extension).
     Hand-verified against real GNU as (`riscv64-linux-gnu-as` 2.44,
     `-march=rv64gv`, byte-identical on `riscv32-linux-gnu-as` 2.43.1):
     `vmul.vv v1,v2,v3` -> `9621a0d7`, `vmul.vx v1,v2,a0` -> `962560d7`,
     `vmulh.vv/.vx` -> `9e21a0d7`/`9e2560d7`, `vmulhu.vv/.vx` ->
     `9221a0d7`/`922560d7`, `vmulhsu.vv/.vx` -> `9a21a0d7`/`9a2560d7`
     (all vm=1, unmasked); `vminu.vi`/`vmul.vi` are both "unrecognized
     opcode" on real GNU as, confirming neither family has an OPIVI
     sibling. The divide/remainder family - `vdivu`/`vdiv`/`vremu`/`vrem`,
     also OPMVV/OPMVX-only with no [.vi] sibling (real GNU as rejects
     `vdiv.vi` as "unrecognized opcode") - shares the identical shape;
     confirmed against real GNU as, byte-identical on RV32/RV64:
     `vdivu.vv/.vx` -> `8221a0d7`/`822560d7`, `vdiv.vv/.vx` ->
     `8621a0d7`/`862560d7`, `vremu.vv/.vx` -> `8a21a0d7`/`8a2560d7`,
     `vrem.vv/.vx` -> `8e21a0d7`/`8e2560d7`. The averaging add/subtract
     family - `vaadd`/`vaaddu`/`vasub`/`vasubu`, also OPMVV/OPMVX-only with
     no [.vi] sibling (real GNU as rejects `vaadd.vi` as "unrecognized
     opcode") - shares the identical shape; confirmed against real GNU as,
     byte-identical on RV32/RV64: `vaaddu.vv/.vx` -> `2221a0d7`/`222560d7`,
     `vaadd.vv/.vx` -> `2621a0d7`/`262560d7`, `vasubu.vv/.vx` ->
     `2a21a0d7`/`2a2560d7`, `vasub.vv/.vx` -> `2e21a0d7`/`2e2560d7`. The
     widening add/subtract family - `vwaddu`/`vwadd`/`vwsubu`/`vwsub`, each
     with a `.vv`/`.vx` (both narrow operands) and `.wv`/`.wx` (`vs2` wide,
     `vs1`/`rs1` narrow) sibling pair, also OPMVV/OPMVX-only with no [.vi]
     sibling (real GNU as rejects `vwadd.vi` as "unrecognized opcode") -
     shares the identical shape (the assembler only encodes register
     field positions; operand *width* is an execution-time SEW/vtype
     concern, invisible here); confirmed against real GNU as, byte-
     identical on RV32/RV64: `vwaddu.vv/.vx` -> `c221a0d7`/`c22560d7`,
     `vwadd.vv/.vx` -> `c621a0d7`/`c62560d7`, `vwsubu.vv/.vx` ->
     `ca21a0d7`/`ca2560d7`, `vwsub.vv/.vx` -> `ce21a0d7`/`ce2560d7`,
     `vwaddu.wv/.wx` -> `d221a0d7`/`d22560d7`, `vwadd.wv/.wx` ->
     `d621a0d7`/`d62560d7`, `vwsubu.wv/.wx` -> `da21a0d7`/`da2560d7`,
     `vwsub.wv/.wx` -> `de21a0d7`/`de2560d7`. The widening multiply family
     - `vwmulu`/`vwmulsu`/`vwmul`, `.vv`/`.vx` only, no [.vi] sibling
     (real GNU as rejects `vwmul.vi` as "unrecognized opcode") - shares
     the identical shape and GAS text operand order (unlike the widening
     multiply-*accumulate* `vwmacc*` family, deliberately not admitted
     here: real GNU as swaps that family's last two text operands to
     `vd, vs1-or-rs1, vs2` rather than this shape's `vd, vs2, vs1-or-
     rs1`, a genuinely different shape not yet built); confirmed against
     real GNU as, byte-identical on RV32/RV64: `vwmulu.vv/.vx` ->
     `e221a0d7`/`e22560d7`, `vwmulsu.vv/.vx` -> `ea21a0d7`/`ea2560d7`,
     `vwmul.vv/.vx` -> `ee21a0d7`/`ee2560d7`. *)
  let opmvv_funct6 = function
    | Opcode.Vmul_vv -> Some 0x25
    | Vmulh_vv -> Some 0x27
    | Vmulhu_vv -> Some 0x24
    | Vmulhsu_vv -> Some 0x26
    | Vdivu_vv -> Some 0x20
    | Vdiv_vv -> Some 0x21
    | Vremu_vv -> Some 0x22
    | Vrem_vv -> Some 0x23
    | Vaaddu_vv -> Some 0x08
    | Vaadd_vv -> Some 0x09
    | Vasubu_vv -> Some 0x0a
    | Vasub_vv -> Some 0x0b
    | Vwaddu_vv -> Some 0x30
    | Vwadd_vv -> Some 0x31
    | Vwsubu_vv -> Some 0x32
    | Vwsub_vv -> Some 0x33
    | Vwaddu_wv -> Some 0x34
    | Vwadd_wv -> Some 0x35
    | Vwsubu_wv -> Some 0x36
    | Vwsub_wv -> Some 0x37
    | Vwmulu_vv -> Some 0x38
    | Vwmulsu_vv -> Some 0x3a
    | Vwmul_vv -> Some 0x3b
    (* [vredsum]/[vredand]/[vredor]/[vredxor]/[vredminu]/[vredmin]/
       [vredmaxu]/[vredmax.vs]: the plain (non-widening) vector-reduction
       family - the same [vd, vs2, vs1] shape as [vmul]/etc. above ([vs1]
       carries the scalar/initial value, [vs2] the vector to reduce,
       matching riscv-opcodes' own operand order), OPMVV (funct3 = 2), with
       a real, selectable [vm] like every other reduction (unlike the
       mask-register-logical family above). Confirmed against real GNU as,
       byte-identical on RV32/RV64: `vredsum.vs v1,v2,v3` -> `022180d7`,
       `vredand.vs` -> `062180d7`, `vredor.vs` -> `0a2180d7`, `vredxor.vs`
       -> `0e2180d7`, `vredminu.vs` -> `122180d7`, `vredmin.vs` ->
       `162180d7`, `vredmaxu.vs` -> `1a2180d7`, `vredmax.vs` -> `1e2180d7`;
       masked (`, v0.t`) drops the low funct7 bit, e.g. `vredsum.vs
       v1,v2,v3,v0.t` -> `002180d7`. No [.vx]/[.vi] sibling exists for any
       of these eight. *)
    | Vredsum_vs -> Some 0x00
    | Vredand_vs -> Some 0x01
    | Vredor_vs -> Some 0x02
    | Vredxor_vs -> Some 0x03
    | Vredminu_vs -> Some 0x04
    | Vredmin_vs -> Some 0x05
    | Vredmaxu_vs -> Some 0x06
    | Vredmax_vs -> Some 0x07
    | _ -> None

  let opmvx_funct6 = function
    | Opcode.Vmul_vx -> Some 0x25
    | Vmulh_vx -> Some 0x27
    | Vmulhu_vx -> Some 0x24
    | Vmulhsu_vx -> Some 0x26
    | Vdivu_vx -> Some 0x20
    | Vdiv_vx -> Some 0x21
    | Vremu_vx -> Some 0x22
    | Vrem_vx -> Some 0x23
    | Vaaddu_vx -> Some 0x08
    | Vaadd_vx -> Some 0x09
    | Vasubu_vx -> Some 0x0a
    | Vasub_vx -> Some 0x0b
    | Vwaddu_vx -> Some 0x30
    | Vwadd_vx -> Some 0x31
    | Vwsubu_vx -> Some 0x32
    | Vwsub_vx -> Some 0x33
    | Vwaddu_wx -> Some 0x34
    | Vwadd_wx -> Some 0x35
    | Vwsubu_wx -> Some 0x36
    | Vwsub_wx -> Some 0x37
    | Vwmulu_vx -> Some 0x38
    | Vwmulsu_vx -> Some 0x3a
    | Vwmul_vx -> Some 0x3b
    (* [vslide1up]/[vslide1down]: the slide family's own single-element
       (scalar-insert) siblings - OPMVX (funct3 = 6), same funct6 values
       as {!opivx_funct6}'s [vslideup.vx]/[vslidedown.vx] (funct3 keeps
       the two spaces disjoint); no [.vi] sibling (real GNU as rejects
       `vslide1up.vi` as "unrecognized opcode" - inserting one element
       needs a real scalar, not a 5-bit immediate). Confirmed against real
       GNU as, byte-identical on RV32/RV64: `vslide1up.vx v1,v2,a0` ->
       `3a2560d7`, `vslide1down.vx` -> `3e2560d7`. *)
    | Vslide1up_vx -> Some 0x0e
    | Vslide1down_vx -> Some 0x0f
    | _ -> None

  (* The multiply-accumulate family - [vmacc]/[vnmsac]/[vmadd]/[vnmsub] and
     their widening siblings [vwmaccu]/[vwmacc]/[vwmaccsu]/[vwmaccus] - uses
     the identical OPMVV (funct3 = 2)/OPMVX (funct3 = 6) funct6-plus-vm-into-
     funct7 composition as {!opmvv_funct6}/{!opmvx_funct6} above, but real
     GNU as swaps the last two text operands: [<mnemonic> vd, vs1-or-rs1,
     vs2] rather than every other OPMVV/OPMVX mnemonic's [vd, vs2, vs1-or-
     rs1] - confirmed by decoding the assembled word's own vs1/vs2 field
     bits, not just accepted/rejected status (`vmacc.vv v1,v2,v3` places
     `v2` in the vs1 field position and `v3` in the vs2 one). Kept in its
     own tables/lowering arms rather than folding into {!opmvv_funct6}/
     {!opmvx_funct6} for that reason. `vwmaccus` is the only member with no
     [.vv] sibling (real GNU as rejects `vwmaccus.vv` as "unrecognized
     opcode"). Confirmed against real GNU as (`riscv64-linux-gnu-as` 2.44
     `-march=rv64gv`, byte-identical on `riscv32-linux-gnu-as` 2.43.1
     `-march=rv32gv`): `vmacc.vv/.vx v1,v2,v3`/`v1,a0,v3` -> `b63120d7`/
     `b63560d7`, `vnmsac.vv/.vx` -> `be3120d7`/`be3560d7`, `vmadd.vv/.vx` ->
     `a63120d7`/`a63560d7`, `vnmsub.vv/.vx` -> `ae3120d7`/`ae3560d7`,
     `vwmaccu.vv/.vx` -> `f23120d7`/`f23560d7`, `vwmacc.vv/.vx` ->
     `f63120d7`/`f63560d7`, `vwmaccsu.vv/.vx` -> `fe3120d7`/`fe3560d7`,
     `vwmaccus.vx` -> `fa3560d7` (all vm=1, unmasked; masked forms also
     confirmed, e.g. `vmacc.vv v1,v2,v3,v0.t` -> `b43120d7`). *)
  let opmacc_funct6 = function
    | Opcode.Vmadd_vv -> Some 0x29
    | Vnmsub_vv -> Some 0x2b
    | Vmacc_vv -> Some 0x2d
    | Vnmsac_vv -> Some 0x2f
    | Vwmaccu_vv -> Some 0x3c
    | Vwmacc_vv -> Some 0x3d
    | Vwmaccsu_vv -> Some 0x3f
    | _ -> None

  let opmaccx_funct6 = function
    | Opcode.Vmadd_vx -> Some 0x29
    | Vnmsub_vx -> Some 0x2b
    | Vmacc_vx -> Some 0x2d
    | Vnmsac_vx -> Some 0x2f
    | Vwmaccu_vx -> Some 0x3c
    | Vwmacc_vx -> Some 0x3d
    | Vwmaccus_vx -> Some 0x3e
    | Vwmaccsu_vx -> Some 0x3f
    | _ -> None

  (* [vmand]/[vmandn]/[vmor]/[vmxor]/[vmorn]/[vmnand]/[vmnor]/[vmxnor]:
     OP-V's mask-register logical family ([.mm] - all three operands are
     mask registers, not general vector registers). Structurally this is
     the same all-vector-register OPMVV (funct3 = 2) shape as
     {!opmvv_funct6} above - [vd, vs2, vs1] with [funct7 = (funct6 lsl 1)
     lor vm] - but [vm] is architecturally fixed at 1 (unmasked): unlike
     every other OPMVV/OPIVV/OPIVX/OPIVI mnemonic, these have no masked
     sibling, so this project keeps their funct6 lookup in its own table
     ({!mm_funct6}) rather than adding them to {!opmvv_funct6}, and gives
     them a dedicated three-operand-only lowering arm below with no
     matching `, v0.t` arm. Confirmed against real GNU as
     (`riscv64-linux-gnu-as` 2.44, `-march=rv64gv`, byte-identical on
     `riscv32-linux-gnu-as` 2.43.1 `-march=rv32gv`): `vmand.mm v1,v2,v3` ->
     `6621a0d7`, `vmandn.mm` -> `6221a0d7`, `vmor.mm` -> `6a21a0d7`,
     `vmxor.mm` -> `6e21a0d7`, `vmorn.mm` -> `7221a0d7`, `vmnand.mm` ->
     `7621a0d7`, `vmnor.mm` -> `7a21a0d7`, `vmxnor.mm` -> `7e21a0d7`
     (`vd`/`vs2`/`vs1` = `v1`/`v2`/`v3`); `vmand.mm v1,v2,v3,v0.t`
     is "illegal operands" on real GNU as, confirming no masked form.
     [vcompress.vm] shares this exact fixed-vm-at-1, three-vector-register,
     no-masked-sibling shape (funct6 = 0x17) - confirmed against real GNU
     as: `vcompress.vm v1,v2,v3` -> `5e21a0d7`, `vcompress.vm
     v1,v2,v3,v0.t` rejected as "illegal operands" - so it is added to this
     same table and lowering arm rather than a dedicated one. *)
  let mm_funct6 = function
    | Opcode.Vmandn_mm -> Some 0x18
    | Vmand_mm -> Some 0x19
    | Vmor_mm -> Some 0x1a
    | Vmxor_mm -> Some 0x1b
    | Vmorn_mm -> Some 0x1c
    | Vmnand_mm -> Some 0x1d
    | Vmnor_mm -> Some 0x1e
    | Vmxnor_mm -> Some 0x1f
    | Vcompress_vm -> Some 0x17
    | _ -> None

  (* [vsext]/[vzext]: OP-V's integer sign-/zero-extend family - a genuinely
     new two-vector-register shape (`vd, vs2`, no third operand at all),
     under the same OPMVV major opcode/funct3 as [vmul]/etc. above
     (funct6 = 0x12 fixed for all six) but with the field position every
     other OPMVV/OPIVV mnemonic uses for [vs1]/[rs1] instead holding a
     fixed per-mnemonic constant (riscv-opcodes' own "19..15=<n>" encoding
     of the source-width divisor: vf8/vf4/vf2 -> 3/5/7 for sign-extend,
     2/4/6 for zero-extend) rather than a real operand. Confirmed against
     real GNU as, byte-identical on RV32/RV64: `vsext.vf2/.vf4/.vf8` ->
     `4a23a0d7`/`4a22a0d7`/`4a21a0d7`, `vzext.vf2/.vf4/.vf8` ->
     `4a2320d7`/`4a2220d7`/`4a2120d7` (unmasked; masked, e.g. `vsext.vf2
     v1,v2,v0.t` -> `4823a0d7`); a third operand is "illegal operands" on
     real GNU as, confirming no [.vx]/[.vi] sibling and no third operand
     of any kind. [viota.m] (funct6 = 0x14) shares this exact shape - GAS's
     `vd, vs2` with a different fixed rs1-position constant (0x10) - so the
     table carries funct6 alongside the constant rather than assuming
     vsext/vzext's own 0x12 for every entry. Confirmed against real GNU as,
     byte-identical on RV32/RV64: `viota.m v1,v2` -> `522820d7` (unmasked),
     `viota.m v1,v2,v0.t` -> `502820d7` (masked). [vmsbf.m]/[vmsif.m]/
     [vmsof.m] (mask-set-before/including/only-first) share this same
     shape too - funct6 = 0x14 like [viota.m], disambiguated purely by
     their own distinct rs1-position constants (0x1/0x3/0x2). Confirmed
     against real GNU as, byte-identical on RV32/RV64: `vmsbf.m v1,v2` ->
     `5220a0d7`, `vmsif.m v1,v2` -> `5221a0d7`, `vmsof.m v1,v2` ->
     `522120d7` (unmasked; masked, e.g. `vmsbf.m v1,v2,v0.t` ->
     `5020a0d7`). *)
  let opmvv_unary_const = function
    | Opcode.Vsext_vf8 -> Some (3, 0x12)
    | Vsext_vf4 -> Some (5, 0x12)
    | Vsext_vf2 -> Some (7, 0x12)
    | Vzext_vf8 -> Some (2, 0x12)
    | Vzext_vf4 -> Some (4, 0x12)
    | Vzext_vf2 -> Some (6, 0x12)
    | Viota_m -> Some (0x10, 0x14)
    | Vmsbf_m -> Some (0x1, 0x14)
    | Vmsof_m -> Some (0x2, 0x14)
    | Vmsif_m -> Some (0x3, 0x14)
    | _ -> None

  (* [vcpop.m]/[vfirst.m]: the mask-population-count/first-set-bit-index
     pair - the exact same OPMVV two-operand shape as {!opmvv_unary_const}
     above, but with a GPR destination rather than a vector-register one
     (riscv-opcodes' own field is literally named "rd", not "vd"), so this
     project keeps them in a separate table dispatched by a dedicated
     lowering arm matching {!xreg} on the destination rather than
     {!vreg}. Confirmed against real GNU as, byte-identical on RV32/RV64:
     `vcpop.m a0,v2` -> `42282557`, `vfirst.m a0,v2` -> `4228a557`
     (unmasked; masked, e.g. `vcpop.m a0,v2,v0.t` -> `40282557`); a third
     operand is "illegal operands" on real GNU as. *)
  let opmvv_gpr_unary_const = function
    | Opcode.Vcpop_m -> Some (0x10, 0x10)
    | Vfirst_m -> Some (0x11, 0x10)
    | _ -> None

  (* [vmv.x.s]: the same OPMVV two-operand GPR-destination shape as
     {!opmvv_gpr_unary_const} above (funct6 0x10, fixed rs1-position
     constant 0), but with no masked sibling at all - unlike [vcpop.m]/
     [vfirst.m], real GNU as rejects `vmv.x.s a0,v2,v0.t` as "illegal
     operands" - so this needs its own table/two-operand-only lowering
     arm rather than reuse of {!opmvv_gpr_unary_const}'s masked arm.
     Confirmed against real GNU as, byte-identical on RV32/RV64:
     `vmv.x.s a0,v2` -> `42202557`. *)
  let mv_x_s_const = function Opcode.Vmv_x_s -> Some (0, 0x10) | _ -> None

  (* [vmv.s.x]: the mirror-image shape of {!mv_x_s_const} - a vector
     destination and a GPR source, OPMVX (funct3 = 6), fixed vs2-position
     constant 0, funct6 0x10, no masked sibling (real GNU as rejects
     `vmv.s.x v1,a0,v0.t`). Confirmed against real GNU as, byte-identical
     on RV32/RV64: `vmv.s.x v1,a0` -> `420560d7`. *)
  let x_to_v_unary_const = function Opcode.Vmv_s_x -> Some (0, 0x10) | _ -> None

  (* [vmv.v.v]/[.v.x]/[.v.i]: OP-V's unconditional-move family - a
     genuinely new two-operand shape under OPIVV(funct3=0)/OPIVX(funct3=4)/
     OPIVI(funct3=3): unlike every other admitted OPIVV/OPIVX/OPIVI
     mnemonic, there is no [vs2] operand at all (its field position is a
     fixed constant 0), funct6 is fixed at 0x17 for all three, and [vm] is
     fixed at 1 with no masked sibling (real GNU as rejects `vmv.v.v
     v1,v2,v0.t`). Confirmed against real GNU as, byte-identical on
     RV32/RV64: `vmv.v.v v1,v2` -> `5e0100d7`, `vmv.v.x v1,a0` ->
     `5e0540d7`, `vmv.v.i v1,5` -> `5e02b0d7`; a third operand is "illegal
     operands" on real GNU as for every one of the three. *)

  (* [vmv1r.v]/[vmv2r.v]/[vmv4r.v]/[vmv8r.v]: OP-V's whole-register-group
     move family - the same two-vector-register shape {!vext_form}/
     {!opmvv_unary_const} model, but under OPIVI (funct3 = 3) rather than
     OPMVV, with a fixed funct6 (0x27) and a fixed per-mnemonic constant
     (0/1/3/7, one less than the register-group count) in the field
     position every OPIVI mnemonic uses for its immediate - real GNU as
     does not enforce the group-count register-number alignment (e.g.
     `vmv2r.v v1,v2` assembles even though `v1` is not 2-aligned) at
     assembly time, so this project's own encoder does not either. [vm] is
     fixed at 1 with no masked sibling (real GNU as rejects `vmv1r.v
     v1,v2,v0.t`). Confirmed against real GNU as, byte-identical on
     RV32/RV64: `vmv1r.v v1,v2` -> `9e2030d7`, `vmv2r.v v2,v4` ->
     `9e40b157`, `vmv4r.v v4,v8` -> `9e81b257`, `vmv8r.v v8,v16` ->
     `9f03b457`. *)
  let whole_reg_move_const = function
    | Opcode.Vmv1r_v -> Some 0
    | Vmv2r_v -> Some 1
    | Vmv4r_v -> Some 3
    | Vmv8r_v -> Some 7
    | _ -> None

  let symbol_of = function Operand.Sym (Asm_core.Expr.Symbol s) -> Some s | _ -> None

  let symbols_of ops =
    let rec go acc = function
      | [] -> Some (List.rev acc)
      | o :: rest -> ( match symbol_of o with Some s -> go (s :: acc) rest | None -> None)
    in
    go [] ops

  let lower_instruction state i =
    let opn = Opcode.name i.Instruction.op in
    match (i.op, i.ops) with
    | ( ( Opcode.Addw | Subw | Sllw | Srlw | Sraw | Mulw | Sh1adduw | Sh2adduw | Sh3adduw | Clzw
        | Ctzw | Cpopw | Packw | Rolw | Rorw | Sha512sum0 | Sha512sum1 | Sha512sig0 | Sha512sig1
        | Aes64ds | Aes64dsm | Aes64es | Aes64esm | Aes64ks2 | Aes64im | Aes64ks1i ),
        _ )
      when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | (Opcode.Addiw | Slliw | Srliw | Sraiw | Roriw | Sext_w | Ld | Lwu | Sd), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | ( ( Opcode.Zip | Unzip | Sha512sum0r | Sha512sum1r | Sha512sig0l | Sha512sig1l | Sha512sig0h
        | Sha512sig1h | Aes32dsi | Aes32dsmi | Aes32esi | Aes32esmi ),
        _ )
      when xlen <> 32 ->
        Error (diag ~pos:__POS__ (`Rv32_only opn))
    | Opcode.C_addi, _ when not state.rvc -> Error (diag ~pos:__POS__ (`Compressed_disabled opn))
    | Opcode.C_addi, [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm when rd <> 0 -> Ok [ Lowered.Caddi { rd; imm } ]
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
        match (xreg a, xreg b, Reg.x m.base, amo3_desc op) with
        | Some rd, Some rs2, Some rs1, Some (opcode, funct3, funct7) when zero_offset m.offset ->
            Ok [ Lowered.R { name = opn; opcode; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (lr_desc op) -> (
        match (xreg a, Reg.x m.base, lr_desc op) with
        | Some rd, Some rs1, Some (opcode, funct3, funct7) when zero_offset m.offset ->
            Ok [ Lowered.R { name = opn; opcode; funct3; funct7; rd; rs1; rs2 = 0 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (r_desc op) -> (
        match (xreg a, xreg b, xreg c, r_desc op) with
        | Some rd, Some rs1, Some rs2, Some (opcode, funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode; funct3; funct7; rd; rs1; rs2 } ]
        | Some rd, Some rs1, None, _ -> (
            (* A third register-typed operand is the R-type form above; a third
               immediate-typed one is GAS's I-type alias, per imm_alias. An op
               with no I-type counterpart (sub, subw, mul) falls through to
               [wrong], which is what GAS does too. *)
            match (imm_alias op, expr_of c) with
            | Some ialias, Some imm -> (
                match i_desc ialias with
                | Some (opcode, funct3, funct_hi, shamt) ->
                    Ok
                      [
                        Lowered.I
                          {
                            name = Opcode.name ialias;
                            opcode;
                            funct3;
                            funct_hi;
                            shamt_bits = shamt;
                            rd;
                            rs1;
                            imm;
                          };
                      ]
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
        match (xreg a, xreg b, expr_of imm) with
        | Some rd, Some rs1, Some e -> (
            match int64_expr e with
            | Some rnum when Int64.compare rnum 0L >= 0 && Int64.compare rnum 10L <= 0 ->
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x13;
                        funct3 = 1;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1;
                        imm = const (0x310 lor Int64.to_int rnum);
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | (Opcode.Aes32dsi | Aes32dsmi | Aes32esi | Aes32esmi), [ a; b; c; bs ] -> (
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
           rejects all four mnemonics outright on RV64 (genuinely absent,
           not merely extension-gated). *)
        let base = function
          | Opcode.Aes32dsi -> Some 0x15
          | Aes32dsmi -> Some 0x17
          | Aes32esi -> Some 0x11
          | Aes32esmi -> Some 0x13
          | _ -> None
        in
        match (xreg a, xreg b, xreg c, expr_of bs, base i.op) with
        | Some rd, Some rs1, Some rs2, Some e, Some base -> (
            match int64_expr e with
            | Some bs when Int64.compare bs 0L >= 0 && Int64.compare bs 3L <= 0 ->
                Ok
                  [
                    Lowered.R
                      {
                        name = opn;
                        opcode = 0x33;
                        funct3 = 0;
                        funct7 = (Int64.to_int bs lsl 5) lor base;
                        rd;
                        rs1;
                        rs2;
                      };
                  ]
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
        match (xreg a, expr_of csr, xreg b) with
        | Some rd, Some csr_e, Some rs1 -> (
            match int64_expr csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                let funct3 =
                  match i.op with Opcode.Csrrw -> 1 | Csrrs -> 2 | Csrrc -> 3 | _ -> assert false
                in
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x73;
                        funct3;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1;
                        imm = const (Int64.to_int (signed_csr csr));
                      };
                  ]
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
        match (xreg a, expr_of csr, expr_of zimm) with
        | Some rd, Some csr_e, Some zimm_e -> (
            match (int64_expr csr_e, int64_expr zimm_e) with
            | Some csr, Some zimm
              when Int64.compare csr 0L >= 0
                   && Int64.compare csr 4095L <= 0
                   && Int64.compare zimm 0L >= 0
                   && Int64.compare zimm 31L <= 0 ->
                let funct3 =
                  match i.op with
                  | Opcode.Csrrwi -> 5
                  | Csrrsi -> 6
                  | Csrrci -> 7
                  | _ -> assert false
                in
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x73;
                        funct3;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1 = Int64.to_int zimm;
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
        match (xreg a, expr_of csr) with
        | Some rd, Some csr_e -> (
            match int64_expr csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x73;
                        funct3 = 2;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1 = 0;
                        imm = const (Int64.to_int (signed_csr csr));
                      };
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
        match (expr_of csr, xreg b) with
        | Some csr_e, Some rs1 -> (
            match int64_expr csr_e with
            | Some csr when Int64.compare csr 0L >= 0 && Int64.compare csr 4095L <= 0 ->
                let funct3 =
                  match i.op with Opcode.Csrw -> 1 | Csrs -> 2 | Csrc -> 3 | _ -> assert false
                in
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x73;
                        funct3;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd = 0;
                        rs1;
                        imm = const (Int64.to_int (signed_csr csr));
                      };
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
            match (int64_expr csr_e, int64_expr zimm_e) with
            | Some csr, Some zimm
              when Int64.compare csr 0L >= 0
                   && Int64.compare csr 4095L <= 0
                   && Int64.compare zimm 0L >= 0
                   && Int64.compare zimm 31L <= 0 ->
                let funct3 =
                  match i.op with Opcode.Csrwi -> 5 | Csrsi -> 6 | Csrci -> 7 | _ -> assert false
                in
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x73;
                        funct3;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd = 0;
                        rs1 = Int64.to_int zimm;
                        imm = const (Int64.to_int (signed_csr csr));
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | Opcode.Vsetvli, a :: b :: (_ :: _ as tail) -> (
        (* [vsetvli rd, rs1, vtype...] - V's register-AVL configuration-
           setting instruction. [rd]/[rs1] are plain GPRs encoded through
           the same [Lowered.I]/[word_i] path {!i_desc}'s callers use; the
           "vtype..." tail is {!vtype_value_of}'s own keyword-list syntax,
           not a real immediate operand GAS lets you write directly.
           Confirmed against real GNU as: `vsetvli a0, a1, e32, m1, ta, ma`
           -> `0d05f557` (imm12 = 208; bit31 = 0 falls out automatically
           since 208 < 2048). *)
        match (xreg a, xreg b, symbols_of tail) with
        | Some rd, Some rs1, Some tokens -> (
            match vtype_value_of tokens with
            | Some vtype ->
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x57;
                        funct3 = 7;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1;
                        imm = const vtype;
                      };
                  ]
            | None -> wrong opn)
        | _ -> wrong opn)
    | Opcode.Vsetivli, a :: b :: (_ :: _ as tail) -> (
        (* [vsetivli rd, uimm, vtype...] - V's immediate-AVL sibling.
           [uimm] (0-31) occupies the same bit position {!Csrrwi}'s [zimm]
           does, reusing [Lowered.I]'s [rs1] field for a raw immediate
           rather than a register (same trick as that comment). The word's
           fixed bits[31:30] = 0b11 fall out of encoding [vtype - 1024] as
           a plain signed 12-bit two's-complement [Lowered.I] immediate -
           no new bit-masking machinery, the same way {!signed_csr} reuses
           this path for an unsigned field. Confirmed against real GNU as:
           `vsetivli a0, 5, e32, m1, ta, ma` -> `cd02f557` (vtype = 208,
           imm12 = 208 - 1024 = -816, two's complement 0xcd0 =
           0b11_0011010000, matching bits[31:20] exactly). *)
        match (xreg a, expr_of b, symbols_of tail) with
        | Some rd, Some uimm_e, Some tokens -> (
            match (int64_expr uimm_e, vtype_value_of tokens) with
            | Some uimm, Some vtype when Int64.compare uimm 0L >= 0 && Int64.compare uimm 31L <= 0
              ->
                Ok
                  [
                    Lowered.I
                      {
                        name = opn;
                        opcode = 0x57;
                        funct3 = 7;
                        funct_hi = 0;
                        shamt_bits = None;
                        rd;
                        rs1 = Int64.to_int uimm;
                        imm = const (vtype - 1024);
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op ] when Option.is_some (opivv_funct6 op) -> (
        (* [<mnemonic> vd, vs2, vs1] - the OPIVV shape (funct3 = 0), all
           three operands vector registers ([Reg.V], parsed with zero
           frontend changes the same way [vsetvli]'s keyword tokens were -
           [Reg.find] already resolves bare [v0]-[v31] generically). The
           word's top 7 bits split into a 6-bit funct6 ({!opivv_funct6}) and
           a 1-bit vm (1 = unmasked) - together exactly the same 7-bit span
           {!Lowered.R}/[word_r] already encodes any other R-type [funct7]
           across, so this reuses that path unchanged with
           [funct7 = (funct6 lsl 1) lor vm]. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, opivv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 0;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op; mask ] when is_v0t mask && Option.is_some (opivv_funct6 op) -> (
        (* Masked form: the same word with only the vm bit cleared. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, opivv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 0; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op ] when Option.is_some (opivx_funct6 op) -> (
        (* [<mnemonic> vd, vs2, rs1] - the scalar-broadcast OPIVX shape
           (funct3 = 4): [rs1] is a plain GPR, [vd]/[vs2] stay vector
           registers. *)
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, opivx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 4;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op; mask ] when is_v0t mask && Option.is_some (opivx_funct6 op) -> (
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, opivx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 4; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; imm_op ] when Option.is_some (opivi_funct6 op) -> (
        (* [<mnemonic> vd, vs2, simm5/zimm5] - the OPIVI shape (funct3 = 3):
           a 5-bit immediate occupies the [rs1] field position, the same
           "reuse the field for something that isn't a register" trick
           {!signed_csr}/[vsetivli]'s [uimm] already use. SIGNED (-16..15)
           for every mnemonic except {!opivi_unsigned}'s UNSIGNED (0..31)
           shift-amount trio - either way, masking with [0x1f] produces the
           correct 5-bit two's-complement field directly. *)
        match (vreg vd_op, vreg vs2_op, expr_of imm_op, opivi_funct6 op) with
        | Some rd, Some rs2, Some e, Some funct6 -> (
            let fits = if opivi_unsigned op then fits_unsigned 5 else fits_signed 5 in
            match int64_expr e with
            | Some v when fits v ->
                let rs1 = Int64.to_int (Int64.logand v 0x1fL) in
                Ok
                  [
                    Lowered.R
                      {
                        name = opn;
                        opcode = 0x57;
                        funct3 = 3;
                        funct7 = (funct6 lsl 1) lor 1;
                        rd;
                        rs1;
                        rs2;
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; imm_op; mask ] when is_v0t mask && Option.is_some (opivi_funct6 op) -> (
        match (vreg vd_op, vreg vs2_op, expr_of imm_op, opivi_funct6 op) with
        | Some rd, Some rs2, Some e, Some funct6 -> (
            let fits = if opivi_unsigned op then fits_unsigned 5 else fits_signed 5 in
            match int64_expr e with
            | Some v when fits v ->
                let rs1 = Int64.to_int (Int64.logand v 0x1fL) in
                Ok
                  [
                    Lowered.R
                      { name = opn; opcode = 0x57; funct3 = 3; funct7 = funct6 lsl 1; rd; rs1; rs2 };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op; carry ] when is_v0 carry && Option.is_some (carry_m_vv_funct6 op)
      -> (
        (* [<mnemonic> vd, vs2, vs1, v0] - OPIVV (funct3 = 0), [vm] fixed
           at 0, with a mandatory literal [v0] 4th operand; see
           {!carry_m_vv_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, carry_m_vv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 0; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op; carry ] when is_v0 carry && Option.is_some (carry_m_vx_funct6 op)
      -> (
        (* [<mnemonic> vd, vs2, rs1, v0] - OPIVX (funct3 = 4), [vm] fixed
           at 0; see {!carry_m_vx_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, carry_m_vx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 4; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; imm_op; carry ] when is_v0 carry && Option.is_some (carry_m_vi_funct6 op)
      -> (
        (* [<mnemonic> vd, vs2, simm5, v0] - OPIVI (funct3 = 3), [vm]
           fixed at 0; see {!carry_m_vi_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, expr_of imm_op, carry_m_vi_funct6 op) with
        | Some rd, Some rs2, Some e, Some funct6 -> (
            match int64_expr e with
            | Some v when fits_signed 5 v ->
                let rs1 = Int64.to_int (Int64.logand v 0x1fL) in
                Ok
                  [
                    Lowered.R
                      { name = opn; opcode = 0x57; funct3 = 3; funct7 = funct6 lsl 1; rd; rs1; rs2 };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op ] when Option.is_some (carry_vv_funct6 op) -> (
        (* [<mnemonic> vd, vs2, vs1] - OPIVV (funct3 = 0), [vm] fixed at
           1: no masked sibling and no carry-in operand exist for this
           bare (non-"m") form; see {!carry_vv_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, carry_vv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 0;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op ] when Option.is_some (carry_vx_funct6 op) -> (
        (* [<mnemonic> vd, vs2, rs1] - OPIVX (funct3 = 4), [vm] fixed at
           1; see {!carry_vx_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, carry_vx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 4;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; imm_op ] when Option.is_some (carry_vi_funct6 op) -> (
        (* [<mnemonic> vd, vs2, simm5] - OPIVI (funct3 = 3), [vm] fixed at
           1; see {!carry_vi_funct6} above. *)
        match (vreg vd_op, vreg vs2_op, expr_of imm_op, carry_vi_funct6 op) with
        | Some rd, Some rs2, Some e, Some funct6 -> (
            match int64_expr e with
            | Some v when fits_signed 5 v ->
                let rs1 = Int64.to_int (Int64.logand v 0x1fL) in
                Ok
                  [
                    Lowered.R
                      {
                        name = opn;
                        opcode = 0x57;
                        funct3 = 3;
                        funct7 = (funct6 lsl 1) lor 1;
                        rd;
                        rs1;
                        rs2;
                      };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op ] when Option.is_some (opmvv_funct6 op) -> (
        (* [<mnemonic> vd, vs2, vs1] - OPMVV (funct3 = 2): the same
           all-vector-register shape as OPIVV, just a different funct3. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, opmvv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op; mask ] when is_v0t mask && Option.is_some (opmvv_funct6 op) -> (
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, opmvv_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 2; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op ] when Option.is_some (opmvx_funct6 op) -> (
        (* [<mnemonic> vd, vs2, rs1] - OPMVX (funct3 = 6): the same
           scalar-broadcast shape as OPIVX, just a different funct3. *)
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, opmvx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 6;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; rs1_op; mask ] when is_v0t mask && Option.is_some (opmvx_funct6 op) -> (
        match (vreg vd_op, vreg vs2_op, xreg rs1_op, opmvx_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 6; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs1_op; vs2_op ] when Option.is_some (opmacc_funct6 op) -> (
        (* [<mnemonic> vd, vs1, vs2] - OPMVV (funct3 = 2), multiply-
           accumulate's own reordered text operand order (see
           {!opmacc_funct6} above): the last two text operands are swapped
           relative to every other OPMVV shape. *)
        match (vreg vd_op, vreg vs1_op, vreg vs2_op, opmacc_funct6 op) with
        | Some rd, Some rs1, Some rs2, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs1_op; vs2_op; mask ] when is_v0t mask && Option.is_some (opmacc_funct6 op) -> (
        match (vreg vd_op, vreg vs1_op, vreg vs2_op, opmacc_funct6 op) with
        | Some rd, Some rs1, Some rs2, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 2; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; rs1_op; vs2_op ] when Option.is_some (opmaccx_funct6 op) -> (
        (* [<mnemonic> vd, rs1, vs2] - OPMVX (funct3 = 6), the scalar-
           broadcast sibling of the arm above with the identical reordered
           text operand order. *)
        match (vreg vd_op, xreg rs1_op, vreg vs2_op, opmaccx_funct6 op) with
        | Some rd, Some rs1, Some rs2, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 6;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; rs1_op; vs2_op; mask ] when is_v0t mask && Option.is_some (opmaccx_funct6 op)
      -> (
        match (vreg vd_op, xreg rs1_op, vreg vs2_op, opmaccx_funct6 op) with
        | Some rd, Some rs1, Some rs2, Some funct6 ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 6; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; vs1_op ] when Option.is_some (mm_funct6 op) -> (
        (* [<mnemonic> vd, vs2, vs1] - OPMVV (funct3 = 2), [vm] fixed at 1:
           no masked sibling exists for this family (see {!mm_funct6}
           above), so unlike every other OPMVV shape there is no
           corresponding four-operand `, v0.t` arm. *)
        match (vreg vd_op, vreg vs2_op, vreg vs1_op, mm_funct6 op) with
        | Some rd, Some rs2, Some rs1, Some funct6 ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Vid_v, [ vd_op ] -> (
        (* [vid.v vd] - OP-V's element-index instruction, the first family
           surveyed with no [vs2]/[vs1]/[rs1] operand at all: funct6 = 0x14
           (OPMVV, funct3 = 2), and the field positions every other OPMVV
           mnemonic uses for [vs1]/[vs2] are both fixed constants (0x11/0x0
           respectively) rather than operands. Confirmed against real GNU
           as, byte-identical on RV32/RV64: `vid.v v1` -> `5208a0d7`
           (unmasked), `vid.v v1,v0.t` -> `5008a0d7` (masked). *)
        match vreg vd_op with
        | Some rd ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = 0x29;
                    rd;
                    rs1 = 0x11;
                    rs2 = 0x0;
                  };
              ]
        | None -> wrong opn)
    | Opcode.Vid_v, [ vd_op; mask ] when is_v0t mask -> (
        match vreg vd_op with
        | Some rd ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = 0x28;
                    rd;
                    rs1 = 0x11;
                    rs2 = 0x0;
                  };
              ]
        | None -> wrong opn)
    | op, [ vd_op; vs2_op ] when Option.is_some (opmvv_unary_const op) -> (
        (* [<mnemonic> vd, vs2] - OPMVV (funct3 = 2) with a per-mnemonic
           fixed funct6 and a fixed per-mnemonic constant in [vs1]'s field
           position; see {!opmvv_unary_const} above. *)
        match (vreg vd_op, vreg vs2_op, opmvv_unary_const op) with
        | Some rd, Some rs2, Some (rs1, funct6) ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; mask ] when is_v0t mask && Option.is_some (opmvv_unary_const op) -> (
        match (vreg vd_op, vreg vs2_op, opmvv_unary_const op) with
        | Some rd, Some rs2, Some (rs1, funct6) ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 2; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op ] when Option.is_some (opmvv_gpr_unary_const op) -> (
        (* [<mnemonic> rd, vs2] - the same shape as {!opmvv_unary_const}
           above but with a GPR destination; see {!opmvv_gpr_unary_const}. *)
        match (xreg vd_op, vreg vs2_op, opmvv_gpr_unary_const op) with
        | Some rd, Some rs2, Some (rs1, funct6) ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op; mask ] when is_v0t mask && Option.is_some (opmvv_gpr_unary_const op) -> (
        match (xreg vd_op, vreg vs2_op, opmvv_gpr_unary_const op) with
        | Some rd, Some rs2, Some (rs1, funct6) ->
            Ok
              [
                Lowered.R
                  { name = opn; opcode = 0x57; funct3 = 2; funct7 = funct6 lsl 1; rd; rs1; rs2 };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op ] when Option.is_some (mv_x_s_const op) -> (
        (* [vmv.x.s rd, vs2] - the same shape as {!opmvv_gpr_unary_const}
           but with no masked sibling; see {!mv_x_s_const} above. *)
        match (xreg vd_op, vreg vs2_op, mv_x_s_const op) with
        | Some rd, Some rs2, Some (rs1, funct6) ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 2;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | op, [ vd_op; rs1_op ] when Option.is_some (x_to_v_unary_const op) -> (
        (* [vmv.s.x vd, rs1] - OPMVX (funct3 = 6), vector destination and
           GPR source, fixed [vs2]-position constant; see
           {!x_to_v_unary_const} above. *)
        match (vreg vd_op, xreg rs1_op, x_to_v_unary_const op) with
        | Some rd, Some rs1, Some (rs2, funct6) ->
            Ok
              [
                Lowered.R
                  {
                    name = opn;
                    opcode = 0x57;
                    funct3 = 6;
                    funct7 = (funct6 lsl 1) lor 1;
                    rd;
                    rs1;
                    rs2;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Vmv_v_v, [ vd_op; vs1_op ] -> (
        (* [vmv.v.v vd, vs1] - OPIVV (funct3 = 0), no [vs2] operand at
           all (fixed constant 0), funct6 fixed at 0x17. *)
        match (vreg vd_op, vreg vs1_op) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.R { name = opn; opcode = 0x57; funct3 = 0; funct7 = 0x2f; rd; rs1; rs2 = 0 };
              ]
        | _ -> wrong opn)
    | Opcode.Vmv_v_x, [ vd_op; rs1_op ] -> (
        (* [vmv.v.x vd, rs1] - OPIVX (funct3 = 4), the scalar-broadcast
           sibling of [vmv.v.v]. *)
        match (vreg vd_op, xreg rs1_op) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.R { name = opn; opcode = 0x57; funct3 = 4; funct7 = 0x2f; rd; rs1; rs2 = 0 };
              ]
        | _ -> wrong opn)
    | Opcode.Vmv_v_i, [ vd_op; imm_op ] -> (
        (* [vmv.v.i vd, simm5] - OPIVI (funct3 = 3), the signed-immediate
           sibling of [vmv.v.v]. *)
        match (vreg vd_op, expr_of imm_op) with
        | Some rd, Some e -> (
            match int64_expr e with
            | Some v when fits_signed 5 v ->
                let rs1 = Int64.to_int (Int64.logand v 0x1fL) in
                Ok
                  [
                    Lowered.R
                      { name = opn; opcode = 0x57; funct3 = 3; funct7 = 0x2f; rd; rs1; rs2 = 0 };
                  ]
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ vd_op; vs2_op ] when Option.is_some (whole_reg_move_const op) -> (
        (* [<mnemonic> vd, vs2] - OPIVI (funct3 = 3), whole-register-group
           move; see {!whole_reg_move_const} above. *)
        match (vreg vd_op, vreg vs2_op, whole_reg_move_const op) with
        | Some rd, Some rs2, Some rs1 ->
            Ok [ Lowered.R { name = opn; opcode = 0x57; funct3 = 3; funct7 = 0x4f; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (i_desc op) -> (
        match (xreg a, xreg b, expr_of c, i_desc op) with
        | Some rd, Some rs1, Some imm, Some (opcode, funct3, funct_hi, shamt) ->
            Ok
              [
                Lowered.I { name = opn; opcode; funct3; funct_hi; shamt_bits = shamt; rd; rs1; imm };
              ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (unary_imm_desc op) -> (
        match (xreg a, xreg b, unary_imm_desc op) with
        | Some rd, Some rs1, Some (opcode, funct3, funct12) ->
            Ok
              [
                Lowered.I
                  {
                    name = opn;
                    opcode;
                    funct3;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = const funct12;
                  };
              ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (load_desc op) -> (
        match (xreg a, Reg.x m.base, load_desc op) with
        | Some rd, Some rs1, Some funct3 ->
            Ok
              [
                Lowered.I
                  {
                    name = opn;
                    opcode = 0x03;
                    funct3;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Ld, [ a; (Operand.Sym _ as target) ] -> (
        (* GAS's own `ld rd, symbol` pseudo, a literal-pool load: auipc rd,
           %pcrel_hi(symbol); ld rd, %pcrel_lo(...)(rd) - the same anchored
           hi/lo pairing `la`/`call` already use, with the second word an
           opcode-0x03 load rather than addi/jalr. Confirmed against real
           riscv64-linux-gnu-as. Scoped to `ld` alone since that is the only
           load pseudo this corpus evidences (a 64-bit constant pulled from a
           `.rodata.cst8` literal, CompCert's own float/double materialization
           idiom on riscv64). *)
        match (xreg a, expr_of target, load_desc Opcode.Ld) with
        | Some rd, Some target, Some funct3 ->
            Ok [ Lowered.Pair { name = opn; rd; tmp = rd; target; kind = Load funct3 } ]
        | _ -> wrong opn)
    | op, [ a; (Operand.Sym _ as target); b ] when Option.is_some (f_load_desc op) -> (
        (* [fld rd, symbol, xtmp] / [flw rd, symbol, xtmp] - the identical anchored
           [auipc]+load pseudo as [ld] above, except the scratch register can't be [rd]
           itself ([auipc] only ever writes a GPR, and [rd] here is a scalar FP register),
           so GAS's own three-operand spelling names it explicitly (M5,
           asm/docs/corpus.md - almabench.c/fftsp.c/knucleotide.c/...). *)
        match (freg a, expr_of target, xreg b, f_load_desc op) with
        | Some rd, Some target, Some tmp, Some funct3 ->
            Ok [ Lowered.Pair { name = opn; rd; tmp; target; kind = Fload funct3 } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (store_desc op) -> (
        match (xreg a, Reg.x m.base, store_desc op) with
        | Some rs2, Some rs1, Some funct3 ->
            Ok [ Lowered.S { name = opn; opcode = 0x23; funct3; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_load_desc op) -> (
        match (freg a, Reg.x m.base, f_load_desc op) with
        | Some rd, Some rs1, Some funct3 ->
            Ok
              [
                Lowered.I
                  {
                    name = opn;
                    opcode = 0x07;
                    funct3;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_store_desc op) -> (
        match (freg a, Reg.x m.base, f_store_desc op) with
        | Some rs2, Some rs1, Some funct3 ->
            Ok [ Lowered.S { name = opn; opcode = 0x27; funct3; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_arith_desc op) -> (
        match (freg a, freg b, freg c, f_arith_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_sgnj3_desc op) -> (
        match (freg a, freg b, freg c, f_sgnj3_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_minmax_desc op) -> (
        match (freg a, freg b, freg c, f_minmax_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_sgnj_desc op) -> (
        match (freg a, freg b, f_sgnj_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 = rs1 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_sqrt_desc op) -> (
        match (freg a, freg b, f_sqrt_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_class_desc op) -> (
        match (xreg a, freg b, f_class_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_mv_x_w_desc op) -> (
        match (xreg a, freg b, f_mv_x_w_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; c; d ] when Option.is_some (f_fma_desc op) -> (
        match (freg a, freg b, freg c, freg d, f_fma_desc op) with
        | Some rd, Some rs1, Some rs2, Some rs3, Some (opcode, fmt) ->
            Ok [ Lowered.R4 { name = opn; opcode; fmt; funct3 = 7; rd; rs1; rs2; rs3 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_cmp_desc op) -> (
        match (xreg a, freg b, freg c, f_cmp_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_i_desc op) -> (
        match (xreg a, freg b, f_to_i_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; Operand.Sym (Asm_core.Expr.Symbol rm) ] when Option.is_some (f_to_i_desc op) -> (
        (* [fcvt.w.d rd, rs1, rtz] - an explicit rounding-mode operand overriding the
           bare mnemonic's own default (M5, asm/docs/corpus.md - perlin.c/binarytrees.c's
           own [(int)]/[(long)] C cast idiom, which truncates and so needs [rtz] rather
           than the default dynamic mode). Checked against real riscv64-linux-gnu-as:
           `fcvt.l.d x22, f10, rtz` -> `c2251b53` (funct3 = 1, matching {!rounding_modes}'
           own [rtz] = 1). *)
        match (xreg a, freg b, f_to_i_desc op, rounding_mode_of_name rm) with
        | Some rd, Some rs1, Some (_, funct7, rs2), Some funct3 ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (i_to_f_desc op) -> (
        match (freg a, xreg b, i_to_f_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_f_desc op) -> (
        match (freg a, freg b, f_to_f_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; target ] when Option.is_some (branch_desc op) -> (
        match (xreg a, xreg b, expr_of target, branch_desc op) with
        | Some rs1, Some rs2, Some target, Some funct3 ->
            Ok [ Lowered.B { name = opn; funct3; rs1; rs2; target } ]
        | _ -> wrong opn)
    | (Opcode.Bgtu | Bleu), [ a; b; target ] -> (
        (* [bgtu rs,rt,label]/[bleu rs,rt,label] - GAS's own greater-than/
           less-or-equal-unsigned pseudos, [bltu]/[bgeu] with the two register
           operands swapped (real hardware defines no distinct opcode; only
           [bltu rt,rs,label]/[bgeu rt,rs,label] exist). Real objdump always
           disassembles the swapped-operand word back as the real underlying
           mnemonic, never the pseudo - this project's canonical printer follows
           that choice (the same alias-preference precedent as AArch64's
           [ubfx]/[ubfiz], asm/docs/corpus.md's Capability ladder item 4), reusing
           [Lowered.B]'s existing shape rather than adding a name-only variant.
           Checked against real riscv64-linux-gnu-as/objdump: `bgtu a0, a1, .`
           -> `00a5e063`, decoding as `bltu a1, a0, .`; `bleu a0, a1, .` ->
           `fea5fee3`, decoding as `bgeu a1, a0, .`. *)
        let real_op, real_name =
          if i.op = Bgtu then (Opcode.Bltu, "bltu") else (Opcode.Bgeu, "bgeu")
        in
        match (xreg a, xreg b, expr_of target, branch_desc real_op) with
        | Some ra, Some rb, Some target, Some funct3 ->
            Ok [ Lowered.B { name = real_name; funct3; rs1 = rb; rs2 = ra; target } ]
        | _ -> wrong opn)
    | ((Opcode.Lui | Auipc) as op), [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm ->
            Ok [ Lowered.U { name = opn; opcode = (if op = Lui then 0x37 else 0x17); rd; imm } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target -> Ok [ Lowered.J { rd; target } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = 1; target } ]
        | None -> wrong opn)
    | Opcode.Jalr, [ a; Operand.Mem m ] -> (
        match (xreg a, Reg.x m.base) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a; b; imm ] -> (
        match (xreg a, xreg b, expr_of imm) with
        | Some rd, Some rs1, Some imm ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a ] | Opcode.Jr, [ a ] -> (
        match xreg a with
        | Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd = (if i.op = Jalr then 1 else 0);
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
           (M5, asm/docs/corpus.md - asm/helpers/riscv.c's own zero-test idiom).
           Checked against real riscv64-linux-gnu-as/objdump: `snez a2, a3` ->
           `00d03633`, matching `sltu a2, zero, a3` bit-for-bit. *)
        match (xreg a, xreg b) with
        | Some rd, Some rs2 ->
            Ok
              [
                Lowered.R { name = "sltu"; opcode = 0x33; funct3 = 3; funct7 = 0; rd; rs1 = 0; rs2 };
              ]
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
        match (xreg a, xreg b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.R
                  {
                    name = (if xlen = 64 then "packw" else "pack");
                    opcode = (if xlen = 64 then 0x3b else 0x33);
                    funct3 = 4;
                    funct7 = 0x04;
                    rd;
                    rs1;
                    rs2 = 0;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Sext_w, [ a; b ] -> (
        (* [sext.w rd, rs] - the sign-extend-word pseudo, [addiw rd, rs, 0] under a
           different spelling (RV64 only: {!Opcode.t}'s early [Rv64_only] gate
           above already rejects it on RV32). Checked against real
           riscv64-linux-gnu-as/objdump: `sext.w a1, a2` -> `0006059b`, matching
           `addiw a1, a2, 0` bit-for-bit. *)
        match (xreg a, xreg b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "addiw";
                    opcode = 0x1b;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = const 0;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Mv, [ a; b ] -> (
        match (xreg a, xreg b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "addi";
                    opcode = 0x13;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = const 0;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Nop, [] ->
        Ok
          [
            Lowered.I
              {
                name = "addi";
                opcode = 0x13;
                funct3 = 0;
                funct_hi = 0;
                shamt_bits = None;
                rd = 0;
                rs1 = 0;
                imm = const 0;
              };
          ]
    | Opcode.J, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = 0; target } ]
        | None -> wrong opn)
    | Opcode.Call, [ target ] | Opcode.Tail, [ target ] -> (
        match expr_of target with
        | Some target ->
            Ok
              [
                Lowered.Pair
                  {
                    name = opn;
                    rd = (if i.op = Call then 1 else 0);
                    tmp = (if i.op = Call then 1 else 6);
                    target;
                    kind = Jalr;
                  };
              ]
        | None -> wrong opn)
    | ((Opcode.La | Lla) as op), [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target when op = Lla || state.pic ->
            Ok [ Lowered.Pair { name = opn; rd; tmp = rd; target; kind = Addi } ]
        | Some rd, Some target ->
            (* In non-PIC state GAS's [la] is the absolute LUI/ADDI pair.
               Keeping this decision in lowering makes source-ordered option
               replay observable and prevents a final-file state from leaking
               backwards across an earlier instruction. *)
            Ok
              [
                Lowered.U
                  { name = "lui"; opcode = 0x37; rd; imm = Asm_core.Expr.Modifier ("%hi", target) };
                Lowered.I
                  {
                    name = "addi";
                    opcode = 0x13;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1 = rd;
                    imm = Asm_core.Expr.Modifier ("%lo", target);
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Li, [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm -> (
            let addi imm =
              Ok
                [
                  Lowered.I
                    {
                      name = "addi";
                      opcode = 0x13;
                      funct3 = 0;
                      funct_hi = 0;
                      shamt_bits = None;
                      rd;
                      rs1 = 0;
                      imm;
                    };
                ]
            in
            match int64_expr imm with
            | Some v when fits_signed 12 v -> addi imm
            | Some v when Int64.logand v 0xfffL = 0L && fits_signed 32 v ->
                (* GAS's own [li] expansion for a constant whose low 12 bits are
                   exactly zero collapses to a bare [lui] - no [addi], the
                   redundant "+0" GAS itself never emits - the only large-constant
                   [li] shape this corpus evidences (asm/helpers/riscv.c's
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
                        name = "lui";
                        opcode = 0x37;
                        rd;
                        imm = Asm_core.Expr.Const (Bigint.of_int64 (Int64.shift_right v 12));
                      };
                  ]
            | _ -> addi imm)
        | _ -> wrong opn)
    | Opcode.Ret, [] ->
        Ok
          [
            Lowered.I
              {
                name = "jalr";
                opcode = 0x67;
                funct3 = 0;
                funct_hi = 0;
                shamt_bits = None;
                rd = 0;
                rs1 = 1;
                imm = const 0;
              };
          ]
    | Opcode.Ecall, [] -> Ok [ Lowered.Fixed { name = "ecall"; word = 0x00000073L } ]
    | Opcode.Ebreak, [] -> Ok [ Lowered.Fixed { name = "ebreak"; word = 0x00100073L } ]
    | Opcode.Unimp, [] -> Ok [ Lowered.Fixed { name = "unimp"; word = 0xc0001073L } ]
    | Opcode.Fence_i, [] -> Ok [ Lowered.Fixed { name = "fence.i"; word = 0x0000100fL } ]
    | ( Opcode.Fence,
        ([] | [ Operand.Sym (Asm_core.Expr.Symbol "rw"); Operand.Sym (Asm_core.Expr.Symbol "w") ]) )
      ->
        (* [fence rw,w] - a memory barrier, opcode 0x0f/funct3 0 with no ModR/M-like
           register fields ([rd]/[rs1] both fixed zero): [imm[11:0]] splits into a
           4-bit [pred]/[succ] pair, each an (i,o,r,w) flag set, at bits 27:24/23:20.
           Only this one predecessor/successor spelling is evidenced
           (asm/helpers/riscv.c's own release-store fence, via
           `__sync_synchronize`-style codegen), so - matching this project's own
           narrow-scope precedent for a single-spelling fixed word ([fucomp],
           [fence.i] above) - any other combination is a diagnostic rather than a
           silently-computed one; the bare no-operand form is decode's own
           round-trip spelling ({!Lowered.Fixed}'s [pp] prints no operand list, the
           same convention [ret]/[ecall] already use). Checked against real
           riscv64-linux-gnu-as/objdump: `fence rw,w` -> `0310000f`
           (pred = 0b0011 = r|w, succ = 0b0001 = w). *)
        Ok [ Lowered.Fixed { name = "fence"; word = 0x0310000fL } ]
    | _ -> wrong opn

  let mask bits v =
    if bits = 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L bits) 1L)

  let field shift bits v = Int64.shift_left (mask bits v) shift

  let word_r ~opcode ~funct3 ~funct7 ~rd ~rs1 ~rs2 =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int rs1))
               (Int64.logor (field 20 5 (Int64.of_int rs2)) (field 25 7 (Int64.of_int funct7))))))

  let word_r4 ~opcode ~fmt ~funct3 ~rd ~rs1 ~rs2 ~rs3 =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int rs1))
               (Int64.logor
                  (field 20 5 (Int64.of_int rs2))
                  (Int64.logor (field 25 2 (Int64.of_int fmt)) (field 27 5 (Int64.of_int rs3)))))))

  let word_i ~opcode ~funct3 ~rd ~rs1 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor (field 15 5 (Int64.of_int rs1)) (field 20 12 imm))))

  let word_s ~opcode ~funct3 ~rs1 ~rs2 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor (field 7 5 imm)
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int rs1))
               (Int64.logor
                  (field 20 5 (Int64.of_int rs2))
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
                  (field 15 5 (Int64.of_int rs1))
                  (Int64.logor
                     (field 20 5 (Int64.of_int rs2))
                     (Int64.logor
                        (field 25 6 (Int64.shift_right_logical imm 5))
                        (field 31 1 (Int64.shift_right_logical imm 12))))))))

  let word_u ~opcode ~rd imm =
    Int64.logor (Int64.of_int opcode) (Int64.logor (field 7 5 (Int64.of_int rd)) (field 12 20 imm))

  let word_j ~rd imm =
    Int64.logor 0x6fL
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
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
         (Int64.logor (field 7 5 (Int64.of_int rd)) (field 12 1 (Int64.shift_right_logical imm 5))))

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

  let u_slices = [ { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 20; value_lsb = 0 } ]
  let form bytes form fixups = { Asm_core.Lowered_ast.bytes; form; fixups }
  let bad_encode kind = Error (diag ~pos:__POS__ kind)

  let encode l =
    let fixed w f = Ok (`Fixed (form (bytes_of_word w) f [])) in
    match l with
    | Lowered.Caddi x -> (
        match int64_expr x.imm with
        | Some imm when x.rd <> 0 && imm <> 0L && fits_signed 6 imm ->
            Ok (`Fixed (form (bytes_of_half (word_caddi ~rd:x.rd imm)) "c.addi" []))
        | Some _ -> bad_encode (`Immediate_range "c.addi")
        | None -> bad_encode (`Immediate_range ("c.addi " ^ Asm_core.Expr.to_string x.imm)))
    | Lowered.R x ->
        fixed
          (word_r ~opcode:x.opcode ~funct3:x.funct3 ~funct7:x.funct7 ~rd:x.rd ~rs1:x.rs1 ~rs2:x.rs2)
          x.name
    | Lowered.R4 x ->
        fixed
          (word_r4 ~opcode:x.opcode ~fmt:x.fmt ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 ~rs2:x.rs2
             ~rs3:x.rs3)
          x.name
    | I x -> (
        match int64_expr x.imm with
        | Some imm -> (
            let encoded_imm =
              match x.shamt_bits with
              | None -> if fits_signed 12 imm then Some imm else None
              | Some width ->
                  let bits = if width = 64 then 6 else 5 in
                  if Int64.compare imm 0L >= 0 && Int64.compare imm (Int64.shift_left 1L bits) < 0
                  then Some (Int64.logor imm (Int64.shift_left (Int64.of_int x.funct_hi) bits))
                  else None
            in
            match encoded_imm with
            | Some imm ->
                fixed (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 imm) x.name
            | None ->
                bad_encode
                  (`Immediate_range
                     (Printf.sprintf "%s %s (%Ld)" x.name (Asm_core.Expr.to_string x.imm) imm)))
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
                           (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                let fx =
                  mk_fixup ~kind:Abs_lo12_i ~name:"lo" ~slices:i_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range (x.name ^ " " ^ Asm_core.Expr.to_string x.imm))))
    | S x -> (
        match int64_expr x.imm with
        | Some imm when fits_signed 12 imm ->
            fixed (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 imm) x.name
        | Some _ -> bad_encode (`Immediate_range x.name)
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
                        (bytes_of_word
                           (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                (* [sw a0,%lo(sym)(s6)] - the store-side sibling of [I]'s own
                   [%lo] case just above (`la`'s non-PIC absolute [lui]+[addi]
                   pair, with [s6] already holding [%hi(sym)] from an earlier
                   instruction this project's own single-pass model resolves
                   independently): a real gap, not a dead branch - {!Abs_lo12_s}
                   already existed in {!fixup_kind} and in [evaluate_fixup]
                   below, but [S]'s own [encode] case never produced one (M5,
                   asm/docs/corpus.md - asm/helpers/riscv.c's own absolute
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
                        (bytes_of_word
                           (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range x.name)))
    | B x ->
        let fx =
          mk_fixup ~kind:Branch13 ~name:"target" ~slices:b_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 13) ~value:x.target ~pairing:Unpaired
        in
        Ok
          (`Fixed
             (form (bytes_of_word (word_b ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L)) x.name [ fx ]))
    | J x ->
        let fx =
          mk_fixup ~kind:Jal21 ~name:"target" ~slices:j_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 21) ~value:x.target ~pairing:Unpaired
        in
        Ok (`Fixed (form (bytes_of_word (word_j ~rd:x.rd 0L)) "jal" [ fx ]))
    | U x -> (
        match int64_expr x.imm with
        | Some imm when fits_signed 20 imm || Int64.compare imm 0xfffffL <= 0 ->
            fixed (word_u ~opcode:x.opcode ~rd:x.rd imm) x.name
        | Some _ -> bad_encode (`Immediate_range x.name)
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_hi", target) ->
                let fx =
                  mk_fixup ~kind:Pcrel_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target
                    ~pairing:(Pair_head "pcrel")
                in
                Ok
                  (`Fixed (form (bytes_of_word (word_u ~opcode:x.opcode ~rd:x.rd 0L)) x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%hi", target) ->
                let fx =
                  mk_fixup ~kind:Abs_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed (form (bytes_of_word (word_u ~opcode:x.opcode ~rd:x.rd 0L)) x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range x.name)))
    | Pair x ->
        let lo_opcode, lo_funct3 =
          match x.kind with
          | Addi -> (0x13, 0)
          | Jalr -> (0x67, 0)
          | Load funct3 -> (0x03, funct3)
          | Fload funct3 -> (0x07, funct3)
        in
        let hiword = word_u ~opcode:0x17 ~rd:x.tmp 0L in
        let lowword = word_i ~opcode:lo_opcode ~funct3:lo_funct3 ~rd:x.rd ~rs1:x.tmp 0L in
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
        Ok (`Fixed (form (bytes_of_word hiword ^ bytes_of_word lowword) x.name [ hi; lo ]))
    | Fixed x -> fixed x.word x.name

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

  let reg n = Operand.Reg (Reg.of_x n)
  let f_operand n = Operand.Reg (Reg.F (n land 31))
  let imm n = Operand.Imm (Bigint.of_int64 n)
  let sym n = Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 n))

  let mem rs1 n =
    Operand.Mem { Mem.base = Reg.of_x rs1; offset = Asm_core.Expr.Const (Bigint.of_int64 n) }

  let op_exn s = Option.get (Opcode.of_mnemonic s)
  let instruction op ops = { Instruction.op; ops }

  let r_name opcode f3 f7 =
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
    | 0x33, 0, 1 -> Some "mul"
    | 0x33, 7, 1 -> Some "remu"
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
    | 0x3b, 0, 1 -> Some "mulw"
    | 0x33, 4, 0x04 -> Some "pack"
    | 0x33, 7, 0x04 -> Some "packh"
    | 0x3b, 4, 0x04 -> Some "packw"
    | 0x3b, 1, 0x30 -> Some "rolw"
    | 0x3b, 5, 0x30 -> Some "rorw"
    | 0x57, 7, 0x40 -> Some "vsetvl"
    | _ -> None

  (* OP-FP (opcode [0x53]): unlike every integer R-type above, several of these
     mnemonics also need [rs2] to disambiguate (a convert or move fixes its second
     "operand" to a selector rather than reading a real register - see
     {!f_shape_of_name}, which is what actually decides whether [rs2] is printed at
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

  let decode ctx bytes ~pos =
    if String.length bytes - pos < 2 then Error (diag ~pos:__POS__ `Decode_short)
    else if Char.code bytes.[pos] land 3 <> 3 then
      let half = read_half bytes pos in
      let quadrant = Int64.to_int (bits half 0 2) in
      let funct3 = Int64.to_int (bits half 13 3) in
      let rd = Int64.to_int (bits half 7 5) in
      let imm_value =
        sign_extend 6 (Int64.logor (bits half 2 5) (Int64.shift_left (bits half 12 1) 5))
      in
      match (quadrant, funct3, rd, imm_value) with
      | 1, 0, rd, imm_value when rd <> 0 && imm_value <> 0L ->
          Ok (instruction Opcode.C_addi [ reg rd; imm imm_value ], "c.addi", 2)
      | _ -> Error (diag ~pos:__POS__ `Decode_no_match)
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
                    match f_shape_of_name n with
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
                          | Some default when default <> f3 -> (
                              match rounding_name_of_mode f3 with
                              | Some rm -> base @ [ Operand.Sym (Asm_core.Expr.Symbol rm) ]
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
            | 0x0f when Int64.equal w 0x0000100fL -> Some (instruction Opcode.Fence_i [], "fence.i")
            | 0x0f when Int64.equal w 0x0310000fL -> Some (instruction Opcode.Fence [], "fence")
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
      | None -> Error (diag ~pos:__POS__ `Decode_no_match)
      | Some (i, f) -> Ok (i, f, 4)

  (* The public codec describes the same real words as the explicit encoder
     and decoder above.  Symbol expressions and PC context live outside a raw
     word relation, so decoding canonicalizes them to a zero displacement; the
     fixup-bearing [encode] result remains the authority for layout.  The
     64-bit branch is one atomic AUIPC+I-format pair, never two independently
     selectable instructions. *)
  let codec : (Lowered.t, fixup_kind) C.t =
    let encoded_bits wanted l =
      match encode l with
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
      match decode { state = default_state; address = 0L } (bytes_of_word w) ~pos:0 with
      | Ok (instruction, _, 4) -> (
          match lower_instruction default_state instruction with
          | Ok [ lowered ] -> Some lowered
          | _ -> None)
      | Ok _ | Error _ -> None
    in
    let decode_half half =
      match decode { state = default_state; address = 0L } (bytes_of_half half) ~pos:0 with
      | Ok (instruction, _, 2) -> (
          match lower_instruction { default_state with rvc = true } instruction with
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
        | 0x13 -> Some (Lowered.Pair { name = "la"; rd; tmp; target; kind = Addi })
        | 0x67 when rd = 1 && tmp = 1 ->
            Some (Lowered.Pair { name = "call"; rd; tmp; target; kind = Jalr })
        | 0x67 when rd = 0 && tmp = 6 ->
            Some (Lowered.Pair { name = "tail"; rd; tmp; target; kind = Jalr })
        | 0x03 when rd = tmp && low_funct3 = 3 ->
            Some (Lowered.Pair { name = "ld"; rd; tmp; target; kind = Load low_funct3 })
        | 0x07 ->
            Option.map
              (fun n -> Lowered.Pair { name = n; rd; tmp; target; kind = Fload low_funct3 })
              (f_load_name low_funct3)
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

  (* Measured (M3 §3/§5, .ai/asm_plan.md §12): a linker-inserted merge gap in an
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
