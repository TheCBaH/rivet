(* RISC-V types shared by every instance of {!Riscv_family_encode.Make_with}: riscv32, riscv64
   and vendor compositions agree on registers, operands and opcodes. *)

module type INDEX = sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
end

module Index : functor
  (_ : sig
     val prefix : string
     val lo : int
     val hi : int
   end)
  -> INDEX

module Xreg : sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val x0 : t
  val x1 : t
  val x2 : t
  val x3 : t
  val x4 : t
  val x5 : t
  val x6 : t
  val x7 : t
  val x8 : t
  val x9 : t
  val x10 : t
  val x11 : t
  val x12 : t
  val x13 : t
  val x14 : t
  val x15 : t
  val x16 : t
  val x17 : t
  val x18 : t
  val x19 : t
  val x20 : t
  val x21 : t
  val x22 : t
  val x23 : t
  val x24 : t
  val x25 : t
  val x26 : t
  val x27 : t
  val x28 : t
  val x29 : t
  val x30 : t
  val x31 : t
  val zero : t
  val ra : t
  val sp : t
  val gp : t
  val tp : t
  val t0 : t
  val t1 : t
  val t2 : t
  val s0 : t
  val fp : t
  val s1 : t
  val a0 : t
  val a1 : t
  val a2 : t
  val a3 : t
  val a4 : t
  val a5 : t
  val a6 : t
  val a7 : t
  val s2 : t
  val s3 : t
  val s4 : t
  val s5 : t
  val s6 : t
  val s7 : t
  val s8 : t
  val s9 : t
  val s10 : t
  val s11 : t
  val t3 : t
  val t4 : t
  val t5 : t
  val t6 : t
end

module Freg : sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val f0 : t
  val f1 : t
  val f2 : t
  val f3 : t
  val f4 : t
  val f5 : t
  val f6 : t
  val f7 : t
  val f8 : t
  val f9 : t
  val f10 : t
  val f11 : t
  val f12 : t
  val f13 : t
  val f14 : t
  val f15 : t
  val f16 : t
  val f17 : t
  val f18 : t
  val f19 : t
  val f20 : t
  val f21 : t
  val f22 : t
  val f23 : t
  val f24 : t
  val f25 : t
  val f26 : t
  val f27 : t
  val f28 : t
  val f29 : t
  val f30 : t
  val f31 : t
  val ft0 : t
  val ft1 : t
  val ft2 : t
  val ft3 : t
  val ft4 : t
  val ft5 : t
  val ft6 : t
  val ft7 : t
  val fs0 : t
  val fs1 : t
  val fa0 : t
  val fa1 : t
  val fa2 : t
  val fa3 : t
  val fa4 : t
  val fa5 : t
  val fa6 : t
  val fa7 : t
  val fs2 : t
  val fs3 : t
  val fs4 : t
  val fs5 : t
  val fs6 : t
  val fs7 : t
  val fs8 : t
  val fs9 : t
  val fs10 : t
  val fs11 : t
  val ft8 : t
  val ft9 : t
  val ft10 : t
  val ft11 : t
end

module Vreg : sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val v0 : t
  val v1 : t
  val v2 : t
  val v3 : t
  val v4 : t
  val v5 : t
  val v6 : t
  val v7 : t
  val v8 : t
  val v9 : t
  val v10 : t
  val v11 : t
  val v12 : t
  val v13 : t
  val v14 : t
  val v15 : t
  val v16 : t
  val v17 : t
  val v18 : t
  val v19 : t
  val v20 : t
  val v21 : t
  val v22 : t
  val v23 : t
  val v24 : t
  val v25 : t
  val v26 : t
  val v27 : t
  val v28 : t
  val v29 : t
  val v30 : t
  val v31 : t
end

module Creg : sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val of_xreg : Xreg.t -> t option
  val to_xreg : t -> Xreg.t
  val field : t -> int
  val x8 : t
  val x9 : t
  val x10 : t
  val x11 : t
  val x12 : t
  val x13 : t
  val x14 : t
  val x15 : t
  val s0 : t
  val fp : t
  val s1 : t
  val a0 : t
  val a1 : t
  val a2 : t
  val a3 : t
  val a4 : t
  val a5 : t
end

module Cfreg : sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val of_freg : Freg.t -> t option
  val to_freg : t -> Freg.t
  val field : t -> int
  val f8 : t
  val f9 : t
  val f10 : t
  val f11 : t
  val f12 : t
  val f13 : t
  val f14 : t
  val f15 : t
  val fs0 : t
  val fs1 : t
  val fa0 : t
  val fa1 : t
  val fa2 : t
  val fa3 : t
  val fa4 : t
  val fa5 : t
end

module Reg : sig
  type t = X of Xreg.t | F of Freg.t | V of Vreg.t

  val x : int -> t
  val f : int -> t
  val v : int -> t
  val zero : t
  val index : t -> int
  val equal : 'a -> 'a -> bool
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val aliases : (string * int) list
  val f_aliases : (string * int) list
  val numbered : string -> (int -> 'a) -> string -> 'a option
  val find : string -> t option
  val to_x : t -> Xreg.t option
  val to_f : t -> Freg.t option
  val to_v : t -> Vreg.t option
  val x0 : t
  val x1 : t
  val x2 : t
  val x3 : t
  val x4 : t
  val x5 : t
  val x6 : t
  val x7 : t
  val x8 : t
  val x9 : t
  val x10 : t
  val x11 : t
  val x12 : t
  val x13 : t
  val x14 : t
  val x15 : t
  val x16 : t
  val x17 : t
  val x18 : t
  val x19 : t
  val x20 : t
  val x21 : t
  val x22 : t
  val x23 : t
  val x24 : t
  val x25 : t
  val x26 : t
  val x27 : t
  val x28 : t
  val x29 : t
  val x30 : t
  val x31 : t
  val ra : t
  val sp : t
  val gp : t
  val tp : t
  val t0 : t
  val t1 : t
  val t2 : t
  val s0 : t
  val fp : t
  val s1 : t
  val a0 : t
  val a1 : t
  val a2 : t
  val a3 : t
  val a4 : t
  val a5 : t
  val a6 : t
  val a7 : t
  val s2 : t
  val s3 : t
  val s4 : t
  val s5 : t
  val s6 : t
  val s7 : t
  val s8 : t
  val s9 : t
  val s10 : t
  val s11 : t
  val t3 : t
  val t4 : t
  val t5 : t
  val t6 : t
  val f0 : t
  val f1 : t
  val f2 : t
  val f3 : t
  val f4 : t
  val f5 : t
  val f6 : t
  val f7 : t
  val f8 : t
  val f9 : t
  val f10 : t
  val f11 : t
  val f12 : t
  val f13 : t
  val f14 : t
  val f15 : t
  val f16 : t
  val f17 : t
  val f18 : t
  val f19 : t
  val f20 : t
  val f21 : t
  val f22 : t
  val f23 : t
  val f24 : t
  val f25 : t
  val f26 : t
  val f27 : t
  val f28 : t
  val f29 : t
  val f30 : t
  val f31 : t
  val ft0 : t
  val ft1 : t
  val ft2 : t
  val ft3 : t
  val ft4 : t
  val ft5 : t
  val ft6 : t
  val ft7 : t
  val fs0 : t
  val fs1 : t
  val fa0 : t
  val fa1 : t
  val fa2 : t
  val fa3 : t
  val fa4 : t
  val fa5 : t
  val fa6 : t
  val fa7 : t
  val fs2 : t
  val fs3 : t
  val fs4 : t
  val fs5 : t
  val fs6 : t
  val fs7 : t
  val fs8 : t
  val fs9 : t
  val fs10 : t
  val fs11 : t
  val ft8 : t
  val ft9 : t
  val ft10 : t
  val ft11 : t
  val v0 : t
  val v1 : t
  val v2 : t
  val v3 : t
  val v4 : t
  val v5 : t
  val v6 : t
  val v7 : t
  val v8 : t
  val v9 : t
  val v10 : t
  val v11 : t
  val v12 : t
  val v13 : t
  val v14 : t
  val v15 : t
  val v16 : t
  val v17 : t
  val v18 : t
  val v19 : t
  val v20 : t
  val v21 : t
  val v22 : t
  val v23 : t
  val v24 : t
  val v25 : t
  val v26 : t
  val v27 : t
  val v28 : t
  val v29 : t
  val v30 : t
  val v31 : t
end

module Mem : sig
  type t = { offset : Asm_core.Expr.t; base : Reg.t }

  val pp : Format.formatter -> t -> unit
end

module Operand : sig
  type t = Reg of Reg.t | Imm of Foundation.Bigint.t | Sym of Asm_core.Expr.t | Mem of Mem.t

  val pp : Format.formatter -> t -> unit
end

module Cls = Riscv_cls

module Row : sig
  type t = Riscv_row.t

  val index : t -> int
  val row : t -> Riscv_table_row.row
  val mnemonic : t -> string
  val feature : t -> string
  val source : t -> string
  val cls : t -> Riscv_cls.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Opcode : sig
  type t =
    | Add
    | Sub
    | Sll
    | Slt
    | Sltu
    | Snez
    | Neg
    | Seqz
    | Sltz
    | Sgtz
    | Zext_b
    | Xor
    | Srl
    | Sra
    | Or
    | And
    | Mul
    | Remu
    | Mulh
    | Mulhsu
    | Mulhu
    | Div
    | Divu
    | Rem
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
    | Bclr
    | Bext
    | Binv
    | Bset
    | Bclri
    | Bexti
    | Binvi
    | Bseti
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
    | Clmulr
    | Czero_eqz
    | Czero_nez
    | Sm3p0
    | Sm3p1
    | Sm4ed
    | Sm4ks
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
    | Divw
    | Divuw
    | Remw
    | Remuw
    | Addi
    | C_addi
    | C_and
    | C_or
    | C_xor
    | C_sub
    | C_addw
    | C_subw
    | C_jr
    | C_jalr
    | C_mv
    | C_add
    | C_ebreak
    | C_lw
    | C_sw
    | C_lwsp
    | C_swsp
    | C_ld
    | C_sd
    | C_ldsp
    | C_sdsp
    | C_beqz
    | C_j
    | C_jal
    | C_bnez
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
    | Bgt
    | Ble
    | Beqz
    | Bnez
    | Bgez
    | Bltz
    | Blez
    | Bgtz
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
    | Fence_tso
    | Pause
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
    | Fabs_d
    | Fabs_s
    | Fmv_s
    | Fmv_x_s
    | Fmv_s_x
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
    | Table of Row.t

  val mnemonic : t -> string
  val all : t list
end

module Rm : sig
  type t = Rne | Rtz | Rdn | Rup | Rmm | Dyn

  val all : t list
  val to_funct3 : t -> int
  val of_funct3 : int -> t option
  val name : t -> string
  val of_name : String.t -> t option
end

module Instruction : sig
  type t = { op : Opcode.t; ops : Operand.t list }

  val mk : Opcode.t -> Operand.t list -> t
  val name : Opcode.t -> string
  val csr_position : Opcode.t -> int option
  val pp : Format.formatter -> t -> unit
end
