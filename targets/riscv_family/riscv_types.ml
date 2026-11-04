(* RISC-V types shared by every instance of {!Riscv_family_encode.Make_with}: riscv32, riscv64
   and vendor compositions agree on registers, operands and opcodes. *)

open Foundation

(* Register numbers by class. Each is a [private int]: it reads as an int, but only the checked
   [of_int] makes one, so an out-of-range or wrong-class number does not type-check. *)
module type INDEX = sig
  type t = private int

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
end

module Index (B : sig
  val prefix : string
  val lo : int
  val hi : int
end) : INDEX = struct
  type t = int

  let of_int n = if n >= B.lo && n <= B.hi then Some n else None

  let of_int_exn n =
    match of_int n with
    | Some r -> r
    | None -> invalid_arg (Printf.sprintf "RISC-V register %s%d out of range" B.prefix n)

  let to_int r = r
  let equal = Int.equal
  let pp ppf r = Fmt.pf ppf "%s%d" B.prefix r
end

module Xreg = struct
  include Index (struct
    let prefix = "x"
    let lo = 0
    let hi = 31
  end)

  let zero = of_int_exn 0
  let ra = of_int_exn 1
  let sp = of_int_exn 2
end

module Freg = Index (struct
  let prefix = "f"
  let lo = 0
  let hi = 31
end)

module Vreg = Index (struct
  let prefix = "v"
  let lo = 0
  let hi = 31
end)

(* The eight registers a compressed 3-bit field names, x8..x15 (f8..f15 for [Cfreg]). *)
module Creg = struct
  include Index (struct
    let prefix = "x"
    let lo = 8
    let hi = 15
  end)

  let of_xreg r = of_int (Xreg.to_int r)
  let to_xreg r = Xreg.of_int_exn (to_int r)
  let field r = to_int r - 8
end

module Cfreg = struct
  include Index (struct
    let prefix = "f"
    let lo = 8
    let hi = 15
  end)

  let of_freg r = of_int (Freg.to_int r)
  let to_freg r = Freg.of_int_exn (to_int r)
  let field r = to_int r - 8
end

module Reg = struct
  type t = X of Xreg.t | F of Freg.t | V of Vreg.t

  let x n = X (Xreg.of_int_exn n)
  let f n = F (Freg.of_int_exn n)
  let v n = V (Vreg.of_int_exn n)
  let zero = X Xreg.zero
  let index = function X r -> Xreg.to_int r | F r -> Freg.to_int r | V r -> Vreg.to_int r
  let equal a b = a = b

  let name = function
    | X n -> Printf.sprintf "x%d" (Xreg.to_int n)
    | F n -> Printf.sprintf "f%d" (Freg.to_int n)
    | V n -> Printf.sprintf "v%d" (Vreg.to_int n)

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
    match numbered "x" x s with
    | Some _ as r -> r
    | None -> (
        match numbered "f" f s with
        | Some _ as r -> r
        | None -> (
            match numbered "v" v s with
            | Some _ as r -> r
            | None -> (
                (* The two alias tables cannot collide: every f_aliases name
                   starts with 'f' followed by a non-digit, so [numbered "f"]
                   has already declined it, and no integer alias is spelled that
                   way either - the integer [fp] is its own entry below. V has
                   no ABI alias names at all - every RVV mnemonic spells vector
                   registers as bare [v0]-[v31]. *)
                match List.assoc_opt s f_aliases with
                | Some n -> Some (f n)
                | None -> Option.map x (List.assoc_opt s aliases))))

  let to_x = function X r -> Some r | F _ | V _ -> None
  let to_f = function F r -> Some r | X _ | V _ -> None
  let to_v = function V r -> Some r | X _ | F _ -> None
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

module Cls = Riscv_cls

(* A table row's identity, minted only where an instance's rows are built. *)
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
end =
  Riscv_row

module Opcode = struct
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
    | Table of Row.t  (** a generated or vendor table row (DEC-RV-TABLE) *)

  (* The spelling of a hand-written opcode; a table row's is its row's, which only an instance of
     {!Riscv_family_encode.Make_with} knows. *)
  let mnemonic = function
    | Table _ -> invalid_arg "Riscv_types.Opcode.mnemonic: a table row"
    | Add -> "add"
    | Sub -> "sub"
    | Sll -> "sll"
    | Slt -> "slt"
    | Sltu -> "sltu"
    | Snez -> "snez"
    | Neg -> "neg"
    | Seqz -> "seqz"
    | Sltz -> "sltz"
    | Sgtz -> "sgtz"
    | Zext_b -> "zext.b"
    | Xor -> "xor"
    | Srl -> "srl"
    | Sra -> "sra"
    | Or -> "or"
    | And -> "and"
    | Mul -> "mul"
    | Remu -> "remu"
    | Mulh -> "mulh"
    | Mulhsu -> "mulhsu"
    | Mulhu -> "mulhu"
    | Div -> "div"
    | Divu -> "divu"
    | Rem -> "rem"
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
    | Bclr -> "bclr"
    | Bext -> "bext"
    | Binv -> "binv"
    | Bset -> "bset"
    | Bclri -> "bclri"
    | Bexti -> "bexti"
    | Binvi -> "binvi"
    | Bseti -> "bseti"
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
    | Clmulr -> "clmulr"
    | Czero_eqz -> "czero.eqz"
    | Czero_nez -> "czero.nez"
    | Sm3p0 -> "sm3p0"
    | Sm3p1 -> "sm3p1"
    | Sm4ed -> "sm4ed"
    | Sm4ks -> "sm4ks"
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
    | Divw -> "divw"
    | Divuw -> "divuw"
    | Remw -> "remw"
    | Remuw -> "remuw"
    | Addi -> "addi"
    | C_addi -> "c.addi"
    | C_and -> "c.and"
    | C_or -> "c.or"
    | C_xor -> "c.xor"
    | C_sub -> "c.sub"
    | C_addw -> "c.addw"
    | C_subw -> "c.subw"
    | C_jr -> "c.jr"
    | C_jalr -> "c.jalr"
    | C_mv -> "c.mv"
    | C_add -> "c.add"
    | C_ebreak -> "c.ebreak"
    | C_lw -> "c.lw"
    | C_sw -> "c.sw"
    | C_lwsp -> "c.lwsp"
    | C_swsp -> "c.swsp"
    | C_ld -> "c.ld"
    | C_sd -> "c.sd"
    | C_ldsp -> "c.ldsp"
    | C_sdsp -> "c.sdsp"
    | C_beqz -> "c.beqz"
    | C_j -> "c.j"
    | C_jal -> "c.jal"
    | C_bnez -> "c.bnez"
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
    | Bgt -> "bgt"
    | Ble -> "ble"
    | Beqz -> "beqz"
    | Bnez -> "bnez"
    | Bgez -> "bgez"
    | Bltz -> "bltz"
    | Blez -> "blez"
    | Bgtz -> "bgtz"
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
    | Fence_tso -> "fence.tso"
    | Pause -> "pause"
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
    | Fabs_d -> "fabs.d"
    | Fabs_s -> "fabs.s"
    | Fmv_s -> "fmv.s"
    | Fmv_x_s -> "fmv.x.s"
    | Fmv_s_x -> "fmv.s.x"
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

  (* Every hand-written opcode. *)
  let all =
    [
      Add;
      Sub;
      Sll;
      Slt;
      Sltu;
      Snez;
      Neg;
      Seqz;
      Sltz;
      Sgtz;
      Zext_b;
      Xor;
      Srl;
      Sra;
      Or;
      And;
      Mul;
      Remu;
      Mulh;
      Mulhsu;
      Mulhu;
      Div;
      Divu;
      Rem;
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
      Bclr;
      Bext;
      Binv;
      Bset;
      Bclri;
      Bexti;
      Binvi;
      Bseti;
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
      Clmulr;
      Czero_eqz;
      Czero_nez;
      Sm3p0;
      Sm3p1;
      Sm4ed;
      Sm4ks;
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
      Divw;
      Divuw;
      Remw;
      Remuw;
      Addi;
      C_addi;
      C_and;
      C_or;
      C_xor;
      C_sub;
      C_addw;
      C_subw;
      C_jr;
      C_jalr;
      C_mv;
      C_add;
      C_ebreak;
      C_lw;
      C_sw;
      C_lwsp;
      C_swsp;
      C_ld;
      C_sd;
      C_ldsp;
      C_sdsp;
      C_beqz;
      C_j;
      C_jal;
      C_bnez;
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
      Bgt;
      Ble;
      Beqz;
      Bnez;
      Bgez;
      Bltz;
      Blez;
      Bgtz;
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
      Fence_tso;
      Pause;
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
      Fabs_d;
      Fabs_s;
      Fmv_s;
      Fmv_x_s;
      Fmv_s_x;
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
    ]
end

(* The RISC-V rounding modes (unprivileged spec, table "Rounding mode encoding"); [Dyn] reads
   [frm]. Encodings 5 and 6 are reserved. *)
module Rm = struct
  type t = Rne | Rtz | Rdn | Rup | Rmm | Dyn

  let all = [ Rne; Rtz; Rdn; Rup; Rmm; Dyn ]
  let to_funct3 = function Rne -> 0 | Rtz -> 1 | Rdn -> 2 | Rup -> 3 | Rmm -> 4 | Dyn -> 7

  let of_funct3 = function
    | 0 -> Some Rne
    | 1 -> Some Rtz
    | 2 -> Some Rdn
    | 3 -> Some Rup
    | 4 -> Some Rmm
    | 7 -> Some Dyn
    | _ -> None

  let name = function
    | Rne -> "rne"
    | Rtz -> "rtz"
    | Rdn -> "rdn"
    | Rup -> "rup"
    | Rmm -> "rmm"
    | Dyn -> "dyn"

  let of_name s = List.find_opt (fun r -> String.equal (name r) s) all
end
