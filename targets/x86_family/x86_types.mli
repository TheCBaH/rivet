(* The x86 vocabulary both modes share: register classes and registers, memory operands,
   operands, condition codes, opcodes and instructions. The per-opcode table
   ({!X86_ops}) is written over these, and the family encoder includes them. *)

module X86_table_row = X86_table_row
module X86_table_rows = X86_table_rows

module Rclass : sig
  type t =
    | Gpr8
    | Gpr8_high
    | Gpr16
    | Gpr32
    | Gpr64
    | Xmm
    | Ymm
    | Zmm
    | Mmx
    | Kmask
    | St
    | Tmm
    | Cr
    | Dr

  val width : t -> int option
  val of_width : int -> t option
  val is_vector : t -> bool
  val of_row : X86_table_row.rclass -> t
  val accepts : X86_table_row.rclass -> t -> bool
end

module Reg : sig
  type t = { cls : Rclass.t; num : int }

  val equal : t -> t -> bool
  val width : t -> int
  val names_32 : string array
  val names_64 : string array
  val names_16 : string array
  val names_8l : string array
  val rip : t
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val in_class : Rclass.t -> int list -> t list
  val range : int -> int -> int list
  val evex_upper : t list
  val mm_and_k : t list
  val base_regs : Rclass.t -> int -> t list
  val extended_regs : Rclass.t -> t list
  val apx_gprs : t list
  val find_in : t list -> String.t -> t option
end

module Disp : sig
  type t = Const of int64 | Sym of Asm_core.Expr.t

  val equal : t -> t -> bool
  val zero : t
  val is_zero : t -> bool
  val placeholder : t -> int64
  val to_string : t -> string
end

module Mem : sig
  type t = { base : Reg.t option; index : Reg.t option; scale : int; disp : Disp.t }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val of_base : ?disp:Disp.t -> Reg.t -> t
end

module Operand : sig
  type t =
    | Reg of Reg.t
    | Mem of Mem.t
    | Imm of Foundation.Bigint.t
    | Imm_sym of Asm_core.Expr.t
    | Sym of Asm_core.Expr.t
    | Dfv of int
    | Bcst of { mem : Mem.t; n : int }
    | Masked of { op : t; k : int; zero : bool }
    | Rc of int

  val rc_name : int -> string
  val pp : Format.formatter -> t -> unit
end

module Surface : sig
  type t = { mnemonic : string; ops : Operand.t list; origin : Foundation.Origin.t }

  val pp : Format.formatter -> t -> unit
end

module Cc : sig
  type t = O | No | B | Ae | E | Ne | Be | A | S | Ns | P | Np | L | Ge | Le | G

  val all : t list
  val name : t -> string
  val equal : t -> t -> bool
  val code : t -> int
  val of_code : int -> t option
  val synonyms : (string * t) list
  val of_name : String.t -> t option
  val split_after : string -> string -> t option
end

module Row : sig
  type t

  val of_index : int -> t
  val index : t -> int
  val row : t -> X86_table_row.row
  val mnemonic : t -> string
  val equal : t -> t -> bool
end

module Short : sig
  type t = Loop | Loope | Loopne | Jcxz | Jecxz | Jrcxz

  val name : t -> string
end

module Opcode : sig
  type t =
    | Add
    | Sub
    | Mov
    | Lea
    | Ret
    | Xor
    | And
    | Cmp
    | Imul
    | Cmov of Cc.t
    | Jcc of Cc.t
    | Short_branch of Short.t
    | Ud2
    | Pop
    | Bswap
    | Jmp
    | Call
    | Push
    | Neg
    | Test
    | Adc
    | Sbb
    | Mul
    | Div
    | Dec
    | Rcr
    | Shr
    | Or
    | Not
    | Ror
    | Shl
    | Sar
    | Shld
    | Setcc of Cc.t
    | Movzx of { src_width : int }
    | Movsx of { src_width : int }
    | Addsd
    | Subsd
    | Mulsd
    | Divsd
    | Addss
    | Subss
    | Mulss
    | Divss
    | Comisd
    | Ucomisd
    | Comiss
    | Ucomiss
    | Xorpd
    | Pxor
    | Movapd
    | Cvtsd2ss
    | Cvtss2sd
    | Cvtps2pd
    | Cvtpd2ps
    | Cvtdq2ps
    | Cvtps2dq
    | Cvttps2dq
    | Movsd
    | Movss
    | Cvtsi2sd
    | Cvtsi2ss
    | Cvttsd2si
    | Unpcklps
    | Unpckhps
    | Unpcklpd
    | Unpckhpd
    | Punpcklqdq
    | Punpckhqdq
    | Punpcklbw
    | Punpckhbw
    | Punpcklwd
    | Punpckhwd
    | Punpckldq
    | Punpckhdq
    | Paddb
    | Paddw
    | Paddd
    | Paddq
    | Psubb
    | Psubw
    | Psubd
    | Psubq
    | Pcmpeqb
    | Pcmpeqw
    | Pcmpeqd
    | Pcmpgtb
    | Pcmpgtw
    | Pcmpgtd
    | Packsswb
    | Packssdw
    | Packuswb
    | Pand
    | Pandn
    | Por
    | Pminub
    | Pmaxub
    | Pminsw
    | Pmaxsw
    | Pmullw
    | Pmulhw
    | Pmulhuw
    | Pavgb
    | Pavgw
    | Psadbw
    | Psllw
    | Pslld
    | Psllq
    | Psrlw
    | Psrld
    | Psrlq
    | Psraw
    | Psrad
    | Pslldq
    | Psrldq
    | Pshufb
    | Phaddw
    | Phaddd
    | Phsubw
    | Phsubd
    | Psignb
    | Psignw
    | Psignd
    | Pmaddubsw
    | Pmulhrsw
    | Phaddsw
    | Phsubsw
    | Pabsb
    | Pabsw
    | Pabsd
    | Palignr
    | Roundps
    | Roundpd
    | Roundss
    | Roundsd
    | Pcmpeqq
    | Pcmpgtq
    | Packusdw
    | Pmaxsb
    | Pmaxsd
    | Pmaxud
    | Pmaxuw
    | Pminsb
    | Pminsd
    | Pminud
    | Pminuw
    | Pmuldq
    | Pmulld
    | Phminposuw
    | Ptest
    | Pmovsxbw
    | Pmovsxbd
    | Pmovsxbq
    | Pmovsxwd
    | Pmovsxwq
    | Pmovsxdq
    | Pmovzxbw
    | Pmovzxbd
    | Pmovzxbq
    | Pmovzxwd
    | Pmovzxwq
    | Pmovzxdq
    | Movntdqa
    | Blendvps
    | Blendvpd
    | Pblendvb
    | Blendps
    | Blendpd
    | Dpps
    | Dppd
    | Mpsadbw
    | Pblendw
    | Insertps
    | Pinsrb
    | Pinsrd
    | Pextrb
    | Pextrd
    | Extractps
    | Vpshufb
    | Vphaddw
    | Vphaddd
    | Vphaddsw
    | Vpmaddubsw
    | Vphsubw
    | Vphsubd
    | Vphsubsw
    | Vpsignb
    | Vpsignw
    | Vpsignd
    | Vpmulhrsw
    | Vpmuldq
    | Vpcmpeqq
    | Vpackusdw
    | Vpcmpgtq
    | Vpminsb
    | Vpminsd
    | Vpminuw
    | Vpminud
    | Vpmaxsb
    | Vpmaxsd
    | Vpmaxuw
    | Vpmaxud
    | Vpmulld
    | Vpalignr
    | Vblendps
    | Vblendpd
    | Vpblendw
    | Vroundss
    | Vroundsd
    | Vdpps
    | Vdppd
    | Vmpsadbw
    | Vinsertps
    | Vpabsb
    | Vpabsw
    | Vpabsd
    | Vphminposuw
    | Vptest
    | Vpmovsxbw
    | Vpmovsxbd
    | Vpmovsxbq
    | Vpmovsxwd
    | Vpmovsxwq
    | Vpmovsxdq
    | Vpmovzxbw
    | Vpmovzxbd
    | Vpmovzxbq
    | Vpmovzxwd
    | Vpmovzxwq
    | Vpmovzxdq
    | Vmovntdqa
    | Movdqa
    | Movdqu
    | Andps
    | Andnps
    | Orps
    | Xorps
    | Andpd
    | Andnpd
    | Orpd
    | Movaps
    | Movups
    | Movupd
    | Addps
    | Subps
    | Mulps
    | Divps
    | Addpd
    | Subpd
    | Mulpd
    | Divpd
    | Maxss
    | Minss
    | Maxsd
    | Minsd
    | Maxps
    | Minps
    | Maxpd
    | Minpd
    | Sqrtss
    | Sqrtsd
    | Sqrtps
    | Sqrtpd
    | Shufps
    | Shufpd
    | Cmpss
    | Cmpsd
    | Cmpps
    | Cmppd
    | Pshufd
    | Pshuflw
    | Pshufhw
    | Movd
    | Vaddsd
    | Vsubsd
    | Vmulsd
    | Vdivsd
    | Vaddss
    | Vsubss
    | Vmulss
    | Vdivss
    | Vaddps
    | Vsubps
    | Vmulps
    | Vdivps
    | Vaddpd
    | Vsubpd
    | Vmulpd
    | Vdivpd
    | Vandps
    | Vandnps
    | Vorps
    | Vxorps
    | Vandpd
    | Vandnpd
    | Vorpd
    | Vxorpd
    | Vunpcklps
    | Vunpckhps
    | Vunpcklpd
    | Vunpckhpd
    | Vpunpcklqdq
    | Vpunpckhqdq
    | Vpunpcklbw
    | Vpunpckhbw
    | Vpunpcklwd
    | Vpunpckhwd
    | Vpunpckldq
    | Vpunpckhdq
    | Vpaddb
    | Vpaddw
    | Vpaddd
    | Vpaddq
    | Vpsubb
    | Vpsubw
    | Vpsubd
    | Vpsubq
    | Vpcmpeqb
    | Vpcmpeqw
    | Vpcmpeqd
    | Vpcmpgtb
    | Vpcmpgtw
    | Vpcmpgtd
    | Vpacksswb
    | Vpackssdw
    | Vpackuswb
    | Vpand
    | Vpandn
    | Vpor
    | Vpminub
    | Vpmaxub
    | Vpminsw
    | Vpmaxsw
    | Vmaxsd
    | Vminsd
    | Vmaxss
    | Vminss
    | Vmaxps
    | Vminps
    | Vmaxpd
    | Vminpd
    | Vsqrtsd
    | Vsqrtss
    | Vsqrtps
    | Vsqrtpd
    | Vmovaps
    | Vmovups
    | Vmovapd
    | Vmovupd
    | Vcomisd
    | Vucomisd
    | Vcomiss
    | Vucomiss
    | Vcvtps2pd
    | Vcvtpd2ps
    | Vcvtdq2ps
    | Vcvtps2dq
    | Vcvttps2dq
    | Vshufps
    | Vshufpd
    | Vcmpss
    | Vcmpsd
    | Vcmpps
    | Vcmppd
    | Vpshufd
    | Vpshuflw
    | Vpshufhw
    | Vmovd
    | Vpmullw
    | Vpmulhw
    | Vpmulhuw
    | Vpavgb
    | Vpavgw
    | Vpsadbw
    | Vpsllw
    | Vpslld
    | Vpsllq
    | Vpsrlw
    | Vpsrld
    | Vpsrlq
    | Vpsraw
    | Vpsrad
    | Vpslldq
    | Vpsrldq
    | Vmovdqa
    | Vmovdqu
    | Pinsrw
    | Pextrw
    | Vpinsrw
    | Vpextrw
    | Movmskps
    | Movmskpd
    | Pmovmskb
    | Vmovmskps
    | Vmovmskpd
    | Vpmovmskb
    | Fldl
    | Fstpl
    | Fstps
    | Flds
    | Fildll
    | Fadds
    | Fadd
    | Fucomp
    | Fnstcw
    | Fldcw
    | Fistpll
    | Fsubs
    | Fnstsw
    | Sahf
    | Table of Row.t

  val name : t -> string
  val all : t array
end

val suffix_of_width : int -> string

module Instruction : sig
  type t = { op : Opcode.t; width : int; ops : Operand.t list; form : string option }

  val mk : ?form:string -> Opcode.t -> int -> Operand.t list -> t
  val form_suffix : t -> string
  val table_spelling : int -> string
end

module Rm : sig
  type t = Reg of Reg.t | Mem of Mem.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type rm = Rm.t
