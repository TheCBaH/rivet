(* RISC-V types shared by every instance of {!Riscv_family_encode.Make_with}: riscv32, riscv64
   and vendor compositions agree on registers, operands and opcodes. *)

open Foundation

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
    | Table of int  (** a generated {!Riscv_table_rows} row, by index (DEC-RV-TABLE) *)
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
