(* The x86-64 builders: the registers this mode has, by name and by class, and the instruction
   builders that exist in it. *)

include module type of struct
  include X86_asm
end

module Rows : module type of struct
  include X86_asm_rows64
end

include module type of struct
  include X86_asm_rows64
end

type st = View.st

module Gpr64 : sig
  type t = w64 gpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val rax : Gpr64.t
val rcx : Gpr64.t
val rdx : Gpr64.t
val rbx : Gpr64.t
val rsp : Gpr64.t
val rbp : Gpr64.t
val rsi : Gpr64.t
val rdi : Gpr64.t
val r8 : Gpr64.t
val r9 : Gpr64.t
val r10 : Gpr64.t
val r11 : Gpr64.t
val r12 : Gpr64.t
val r13 : Gpr64.t
val r14 : Gpr64.t
val r15 : Gpr64.t

module Gpr32 : sig
  type t = w32 gpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val eax : Gpr32.t
val ecx : Gpr32.t
val edx : Gpr32.t
val ebx : Gpr32.t
val esp : Gpr32.t
val ebp : Gpr32.t
val esi : Gpr32.t
val edi : Gpr32.t
val r8d : Gpr32.t
val r9d : Gpr32.t
val r10d : Gpr32.t
val r11d : Gpr32.t
val r12d : Gpr32.t
val r13d : Gpr32.t
val r14d : Gpr32.t
val r15d : Gpr32.t

module Gpr16 : sig
  type t = w16 gpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val ax : Gpr16.t
val cx : Gpr16.t
val dx : Gpr16.t
val bx : Gpr16.t
val sp : Gpr16.t
val bp : Gpr16.t
val si : Gpr16.t
val di : Gpr16.t
val r8w : Gpr16.t
val r9w : Gpr16.t
val r10w : Gpr16.t
val r11w : Gpr16.t
val r12w : Gpr16.t
val r13w : Gpr16.t
val r14w : Gpr16.t
val r15w : Gpr16.t

module Gpr8 : sig
  type t = w8 gpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val al : Gpr8.t
val cl : Gpr8.t
val dl : Gpr8.t
val bl : Gpr8.t
val spl : Gpr8.t
val bpl : Gpr8.t
val sil : Gpr8.t
val dil : Gpr8.t
val r8b : Gpr8.t
val r9b : Gpr8.t
val r10b : Gpr8.t
val r11b : Gpr8.t
val r12b : Gpr8.t
val r13b : Gpr8.t
val r14b : Gpr8.t
val r15b : Gpr8.t

module Xmm : sig
  type t = xmm

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val xmm0 : Xmm.t
val xmm1 : Xmm.t
val xmm2 : Xmm.t
val xmm3 : Xmm.t
val xmm4 : Xmm.t
val xmm5 : Xmm.t
val xmm6 : Xmm.t
val xmm7 : Xmm.t
val xmm8 : Xmm.t
val xmm9 : Xmm.t
val xmm10 : Xmm.t
val xmm11 : Xmm.t
val xmm12 : Xmm.t
val xmm13 : Xmm.t
val xmm14 : Xmm.t
val xmm15 : Xmm.t

module Ymm : sig
  type t = ymm

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val ymm0 : Ymm.t
val ymm1 : Ymm.t
val ymm2 : Ymm.t
val ymm3 : Ymm.t
val ymm4 : Ymm.t
val ymm5 : Ymm.t
val ymm6 : Ymm.t
val ymm7 : Ymm.t
val ymm8 : Ymm.t
val ymm9 : Ymm.t
val ymm10 : Ymm.t
val ymm11 : Ymm.t
val ymm12 : Ymm.t
val ymm13 : Ymm.t
val ymm14 : Ymm.t
val ymm15 : Ymm.t

module Zmm : sig
  type t = zmm

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val zmm0 : Zmm.t
val zmm1 : Zmm.t
val zmm2 : Zmm.t
val zmm3 : Zmm.t
val zmm4 : Zmm.t
val zmm5 : Zmm.t
val zmm6 : Zmm.t
val zmm7 : Zmm.t
val zmm8 : Zmm.t
val zmm9 : Zmm.t
val zmm10 : Zmm.t
val zmm11 : Zmm.t
val zmm12 : Zmm.t
val zmm13 : Zmm.t
val zmm14 : Zmm.t
val zmm15 : Zmm.t

module Gpr64_apx : sig
  type t = w64 agpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val r16 : Gpr64_apx.t
val r17 : Gpr64_apx.t
val r18 : Gpr64_apx.t
val r19 : Gpr64_apx.t
val r20 : Gpr64_apx.t
val r21 : Gpr64_apx.t
val r22 : Gpr64_apx.t
val r23 : Gpr64_apx.t
val r24 : Gpr64_apx.t
val r25 : Gpr64_apx.t
val r26 : Gpr64_apx.t
val r27 : Gpr64_apx.t
val r28 : Gpr64_apx.t
val r29 : Gpr64_apx.t
val r30 : Gpr64_apx.t
val r31 : Gpr64_apx.t

module Gpr32_apx : sig
  type t = w32 agpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val r16d : Gpr32_apx.t
val r17d : Gpr32_apx.t
val r18d : Gpr32_apx.t
val r19d : Gpr32_apx.t
val r20d : Gpr32_apx.t
val r21d : Gpr32_apx.t
val r22d : Gpr32_apx.t
val r23d : Gpr32_apx.t
val r24d : Gpr32_apx.t
val r25d : Gpr32_apx.t
val r26d : Gpr32_apx.t
val r27d : Gpr32_apx.t
val r28d : Gpr32_apx.t
val r29d : Gpr32_apx.t
val r30d : Gpr32_apx.t
val r31d : Gpr32_apx.t

module Gpr16_apx : sig
  type t = w16 agpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val r16w : Gpr16_apx.t
val r17w : Gpr16_apx.t
val r18w : Gpr16_apx.t
val r19w : Gpr16_apx.t
val r20w : Gpr16_apx.t
val r21w : Gpr16_apx.t
val r22w : Gpr16_apx.t
val r23w : Gpr16_apx.t
val r24w : Gpr16_apx.t
val r25w : Gpr16_apx.t
val r26w : Gpr16_apx.t
val r27w : Gpr16_apx.t
val r28w : Gpr16_apx.t
val r29w : Gpr16_apx.t
val r30w : Gpr16_apx.t
val r31w : Gpr16_apx.t

module Gpr8_apx : sig
  type t = w8 agpr

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val r16b : Gpr8_apx.t
val r17b : Gpr8_apx.t
val r18b : Gpr8_apx.t
val r19b : Gpr8_apx.t
val r20b : Gpr8_apx.t
val r21b : Gpr8_apx.t
val r22b : Gpr8_apx.t
val r23b : Gpr8_apx.t
val r24b : Gpr8_apx.t
val r25b : Gpr8_apx.t
val r26b : Gpr8_apx.t
val r27b : Gpr8_apx.t
val r28b : Gpr8_apx.t
val r29b : Gpr8_apx.t
val r30b : Gpr8_apx.t
val r31b : Gpr8_apx.t

module Xmm_evex : sig
  type t = l128 evec

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val xmm16 : Xmm_evex.t
val xmm17 : Xmm_evex.t
val xmm18 : Xmm_evex.t
val xmm19 : Xmm_evex.t
val xmm20 : Xmm_evex.t
val xmm21 : Xmm_evex.t
val xmm22 : Xmm_evex.t
val xmm23 : Xmm_evex.t
val xmm24 : Xmm_evex.t
val xmm25 : Xmm_evex.t
val xmm26 : Xmm_evex.t
val xmm27 : Xmm_evex.t
val xmm28 : Xmm_evex.t
val xmm29 : Xmm_evex.t
val xmm30 : Xmm_evex.t
val xmm31 : Xmm_evex.t

module Ymm_evex : sig
  type t = l256 evec

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val ymm16 : Ymm_evex.t
val ymm17 : Ymm_evex.t
val ymm18 : Ymm_evex.t
val ymm19 : Ymm_evex.t
val ymm20 : Ymm_evex.t
val ymm21 : Ymm_evex.t
val ymm22 : Ymm_evex.t
val ymm23 : Ymm_evex.t
val ymm24 : Ymm_evex.t
val ymm25 : Ymm_evex.t
val ymm26 : Ymm_evex.t
val ymm27 : Ymm_evex.t
val ymm28 : Ymm_evex.t
val ymm29 : Ymm_evex.t
val ymm30 : Ymm_evex.t
val ymm31 : Ymm_evex.t

module Zmm_evex : sig
  type t = l512 evec

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val zmm16 : Zmm_evex.t
val zmm17 : Zmm_evex.t
val zmm18 : Zmm_evex.t
val zmm19 : Zmm_evex.t
val zmm20 : Zmm_evex.t
val zmm21 : Zmm_evex.t
val zmm22 : Zmm_evex.t
val zmm23 : Zmm_evex.t
val zmm24 : Zmm_evex.t
val zmm25 : Zmm_evex.t
val zmm26 : Zmm_evex.t
val zmm27 : Zmm_evex.t
val zmm28 : Zmm_evex.t
val zmm29 : Zmm_evex.t
val zmm30 : Zmm_evex.t
val zmm31 : Zmm_evex.t

module Tmm : sig
  type t = tmm

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val tmm0 : Tmm.t
val tmm1 : Tmm.t
val tmm2 : Tmm.t
val tmm3 : Tmm.t
val tmm4 : Tmm.t
val tmm5 : Tmm.t
val tmm6 : Tmm.t
val tmm7 : Tmm.t

module Creg : sig
  type t = creg

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val cr0 : Creg.t
val cr1 : Creg.t
val cr2 : Creg.t
val cr3 : Creg.t
val cr4 : Creg.t
val cr5 : Creg.t
val cr6 : Creg.t
val cr7 : Creg.t
val cr8 : Creg.t
val cr9 : Creg.t
val cr10 : Creg.t
val cr11 : Creg.t
val cr12 : Creg.t
val cr13 : Creg.t
val cr14 : Creg.t
val cr15 : Creg.t

module Dreg : sig
  type t = dreg

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val dr0 : Dreg.t
val dr1 : Dreg.t
val dr2 : Dreg.t
val dr3 : Dreg.t
val dr4 : Dreg.t
val dr5 : Dreg.t
val dr6 : Dreg.t
val dr7 : Dreg.t

module Mmx : sig
  type t = mmx

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val mm0 : Mmx.t
val mm1 : Mmx.t
val mm2 : Mmx.t
val mm3 : Mmx.t
val mm4 : Mmx.t
val mm5 : Mmx.t
val mm6 : Mmx.t
val mm7 : Mmx.t

module Kmask : sig
  type t = kmask

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val k0 : Kmask.t
val k1 : Kmask.t
val k2 : Kmask.t
val k3 : Kmask.t
val k4 : Kmask.t
val k5 : Kmask.t
val k6 : Kmask.t
val k7 : Kmask.t

module St : sig
  type t = st

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val st : St.t
val st1 : St.t
val st2 : St.t
val st3 : St.t
val st4 : St.t
val st5 : St.t
val st6 : St.t
val st7 : St.t

val add :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val add_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val addb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val addb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val addl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val addl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val addq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val addq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val add_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val add_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val sub_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val subb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val subb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val subl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val subl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val subq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val subq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val sub_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sub_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val mov :
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val mov_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val mov_q :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val mov_rq :
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val mov_rm :
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val mov_rs :
  'a X86_family_encode.X86_ops.View.gpr -> Asm_core.Expr.t -> X86_family_encode.Instruction.t

val mov_i : int64 -> 'a X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val mov_i_e :
  Asm_core.Expr.t -> 'a X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val mov_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val mov_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val mov_mx :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val mov_qm :
  X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val mov_s :
  Asm_core.Expr.t -> 'a X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val mov_xm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movw_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movw_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val movq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val lea_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val lea_s :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ret : X86_family_encode.Instruction.t

val xor :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val xor_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xorb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val xorb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val xorl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val xorl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val xorq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val xorq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val xor_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val xor_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_ :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val and_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val andb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val andb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val andl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val andl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val andq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val andq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val and_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val and_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmp :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmp_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val cmp_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmp_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmp_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmpb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val cmpb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val cmpl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val cmpl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val cmpq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val cmpq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val imul : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val imul_rr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_rrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imull_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val imulq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val imul_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_s :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_i :
  int64 ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_i_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val imul_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ud2 : X86_family_encode.Instruction.t

val pop :
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pop_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val bswap : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val jmp : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val jmp_r :
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jmp_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val call : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val call_r :
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val call_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val push :
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val push_i : int64 -> X86_family_encode.Instruction.t
val push_i_e : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val neg : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val neg_rr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val negl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val negq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val neg_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val test :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val test_rm :
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val test_i :
  int64 -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val test_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val testl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val testl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val testq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val testq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val adc :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val adc_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adcb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val adcb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val adcl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val adcl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val adcq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val adcq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val adc_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val adc_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val sbb_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbbb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sbbb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sbbl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sbbl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sbbq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sbbq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val sbb_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sbb_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val mul : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val mull_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val mulq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val div : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val divl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val divq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcr : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val rcrl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcr_cl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val rcr_i :
  int -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val rcr_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcr_clrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcr_irr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcr_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcrl_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrq_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrl_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrq_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rcrq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val rcr_clm :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcr_im :
  int ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rcr_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val shrl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shr_cl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val shr_i :
  int -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val shr_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr_clrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr_irr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shrl_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrq_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrl_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrq_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shrq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val shr_clm :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr_im :
  int ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shr_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_ :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_rm :
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val or_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_irr :
  int64 ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_mrr :
  X86_family_encode.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val orb_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val orb_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val orl_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val orl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val orq_im : int64 -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val orq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val or_im :
  int64 ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val or_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val not : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val not_rr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val notl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val notq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val not_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val rorl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val ror_cl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val ror_i :
  int -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val ror_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror_clrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror_irr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rorl_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorq_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorl_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorq_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val rorq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val ror_clm :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror_im :
  int ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ror_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val shll_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shlq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shl_cl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val shl_i :
  int -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val shl_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl_clrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl_irr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shll_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shlq_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shll_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shll_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shlq_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val shlq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val shl_clm :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl_im :
  int ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shl_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t
val sarl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarq_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sar_cl : [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val sar_i :
  int -> [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr -> X86_family_encode.Instruction.t

val sar_i_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar_clrr :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar_irr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sarl_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarq_clm : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarl_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarl_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarq_im : int -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sarq_im_e : Asm_core.Expr.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val sar_clm :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar_im :
  int ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sar_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_cl :
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_i :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_i_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_irrr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_irrr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_clrm :
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val shld_irm :
  int ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val shld_irm_e :
  Asm_core.Expr.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val shld_irmr :
  int ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val shld_irmr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val addsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val addsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val subsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val subsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val mulsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mulsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val divsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val divsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val addss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val addss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val subss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val subss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val mulss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mulss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val divss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val divss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val comisd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val comisd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val ucomisd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val ucomisd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val comiss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val comiss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val ucomiss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val ucomiss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val xorpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val xorpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pxor_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pxor :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pxor_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pxor_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movapd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movapd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movapd_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cvtsd2ss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtsd2ss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtss2sd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtss2sd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtps2pd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtps2pd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtpd2ps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtpd2ps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtdq2ps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtdq2ps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtps2dq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtps2dq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvttps2dq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvttps2dq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movsd_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movss_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cvtsi2sd :
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtsi2sd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvtsi2ss :
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cvtsi2ss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val cvttsd2si_x :
  X86_family_encode.X86_ops.View.xmm ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cvttsd2si_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cvttsd2si_mr :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val unpcklps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val unpcklps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val unpckhps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val unpckhps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val unpcklpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val unpcklpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val unpckhpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val unpckhpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpcklqdq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpcklqdq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpckhqdq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpckhqdq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpcklbw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpcklbw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpcklbw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpcklbw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpckhbw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpckhbw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpckhbw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpckhbw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpcklwd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpcklwd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpcklwd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpcklwd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpckhwd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpckhwd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpckhwd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpckhwd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpckldq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpckldq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpckldq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpckldq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val punpckhdq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val punpckhdq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val punpckhdq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val punpckhdq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val paddb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val paddb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val paddb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val paddb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val paddw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val paddw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val paddw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val paddw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val paddd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val paddd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val paddd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val paddd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val paddq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val paddq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val paddq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val paddq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psubb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psubb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psubb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psubb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psubw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psubw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psubw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psubw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psubd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psubd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psubd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psubd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psubq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psubq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psubq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psubq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpeqb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpeqb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpeqb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpeqb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpeqw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpeqw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpeqw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpeqw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpeqd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpeqd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpeqd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpeqd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpgtb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpgtb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpgtb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpgtb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpgtw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpgtw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpgtw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpgtw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpgtd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pcmpgtd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpgtd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pcmpgtd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val packsswb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val packsswb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val packsswb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val packsswb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val packssdw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val packssdw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val packssdw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val packssdw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val packuswb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val packuswb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val packuswb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val packuswb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pand_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pand :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pand_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pand_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pandn_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pandn :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pandn_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pandn_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val por_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val por :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val por_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val por_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminub_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pminub :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminub_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pminub_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxub_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmaxub :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxub_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmaxub_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pminsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pminsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmaxsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmaxsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmullw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmullw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmullw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmullw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmulhw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmulhw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmulhw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmulhw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmulhuw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmulhuw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmulhuw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmulhuw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pavgb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pavgb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pavgb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pavgb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pavgw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pavgw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pavgw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pavgw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psadbw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psadbw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psadbw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psadbw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psllw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psllw_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllw_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllw_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllw_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pslld_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pslld :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pslld_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pslld_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pslld_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pslld_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pslld_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pslld_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psllq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psllq_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllq_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllq_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllq_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psllq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psllq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psrlw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psrlw_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlw_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlw_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlw_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrld_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psrld :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psrld_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrld_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrld_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrld_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrld_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrld_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlq_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psrlq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psrlq_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlq_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlq_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlq_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrlq_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrlq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psraw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psraw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psraw_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psraw_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psraw_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psraw_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psraw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psraw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrad_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psrad :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psrad_iq : int -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrad_iq_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrad_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrad_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrad_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psrad_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pslldq_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pslldq_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrldq_i : int -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psrldq_i_e :
  Asm_core.Expr.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pshufb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pshufb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pshufb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phaddw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phaddw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phaddw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phaddw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phaddd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phaddd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phaddd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phaddd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phsubw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phsubw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phsubw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phsubw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phsubd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phsubd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phsubd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phsubd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psignb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psignb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psignb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psignb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psignw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psignw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psignw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psignw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val psignd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val psignd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val psignd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val psignd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaddubsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmaddubsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaddubsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmaddubsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmulhrsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pmulhrsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmulhrsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pmulhrsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phaddsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phaddsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phaddsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phaddsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phsubsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val phsubsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phsubsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val phsubsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pabsb_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pabsb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pabsb_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pabsb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pabsw_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pabsw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pabsw_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pabsw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pabsd_qq :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pabsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pabsd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val pabsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val palignr_iqq :
  int ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val palignr_iqq_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val palignr_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val palignr_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val palignr_imq :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val palignr_imq_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val palignr_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val palignr_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundpd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundpd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundpd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundpd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundss_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundss_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundss_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundss_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundsd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundsd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundsd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val roundsd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpeqq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpeqq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pcmpgtq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pcmpgtq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val packusdw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val packusdw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxsb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxsb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxud :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxud_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmaxuw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmaxuw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminsb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminsb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminud :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminud_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pminuw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pminuw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmuldq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmuldq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmulld :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmulld_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val phminposuw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val phminposuw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val ptest :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val ptest_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxbw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxbw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxbd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxbd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxbq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxbq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxwd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxwd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxwq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxwq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovsxdq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovsxdq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxbw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxbw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxbd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxbd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxbq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxbq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxwd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxwd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxwq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxwq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pmovzxdq :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pmovzxdq_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movntdqa_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val blendvps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendvps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val blendvpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendvpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val pblendvb :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pblendvb_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val blendps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendpd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendpd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendpd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val blendpd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dpps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dpps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dpps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dpps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dppd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dppd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dppd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val dppd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mpsadbw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mpsadbw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mpsadbw_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mpsadbw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pblendw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pblendw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pblendw_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pblendw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val insertps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val insertps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val insertps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val insertps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrb_i :
  int ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrb_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrb_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrb_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrd_i :
  int ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pextrb_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrb_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrb_irm :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val pextrb_irm_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val pextrd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrd_irm :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val pextrd_irm_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val extractps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val extractps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val extractps_irm :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val extractps_irm_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vpshufb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddd :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddd_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddsw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphaddsw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaddubsw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaddubsw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubd :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubd_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubsw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphsubsw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignb :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignb_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignd :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsignd_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhrsw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhrsw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmuldq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmuldq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqq :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqq_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqq_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqq_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpackusdw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpackusdw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtq :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtq_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtq_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtq_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpminsb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminsb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminsd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminsd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminuw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminuw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminud :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminud_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxuw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxuw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxud :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxud_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulld :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulld_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpalignr_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpalignr_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpalignr_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpalignr_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendps_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendpd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendpd_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendpd_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vblendpd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpblendw_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpblendw_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpblendw_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpblendw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vroundss_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundss_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundss_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundss_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundsd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundsd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundsd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vroundsd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdpps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdpps_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdpps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdppd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdppd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdppd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdppd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmpsadbw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmpsadbw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmpsadbw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmpsadbw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vinsertps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vinsertps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vinsertps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vinsertps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vpabsb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpabsb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpabsw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpabsw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpabsd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpabsd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vphminposuw :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vphminposuw_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vptest :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vptest_m :
  X86_family_encode.Mem.t ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbw :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbw_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovsxbw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbd :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxbq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxwd :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxwd_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovsxwd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxwq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxwq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxdq :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovsxdq_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovsxdq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbw :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbw_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovzxbw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbd :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxbq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxwd :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxwd_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovzxwd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxwq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxwq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxdq :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmovzxdq_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vpmovzxdq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovntdqa_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val movdqa :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movdqa_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movdqa_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movdqu :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movdqu_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movdqu_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val andps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val andps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val andnps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val andnps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val orps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val orps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val xorps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val xorps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val andpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val andpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val andnpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val andnpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val orpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val orpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movaps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movaps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movaps_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movups :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movups_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movups_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movupd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movupd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movupd_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val addps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val addps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val subps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val subps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val mulps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mulps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val divps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val divps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val addpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val addpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val subpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val subpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val mulpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val mulpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val divpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val divpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val maxss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val maxss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val minss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val minss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val maxsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val maxsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val minsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val minsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val maxps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val maxps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val minps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val minps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val maxpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val maxpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val minpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val minpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val sqrtss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val sqrtss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val sqrtsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val sqrtsd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val sqrtps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val sqrtps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val sqrtpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val sqrtpd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val shufps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufpd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufpd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufpd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val shufpd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpss_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpss_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpss_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpss_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpsd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpsd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpsd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpsd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpps_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpps_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpps_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmpps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmppd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmppd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmppd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val cmppd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshuflw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshuflw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshuflw_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshuflw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufhw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufhw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufhw_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pshufhw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movd_q :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movd_rq :
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val movd_rx :
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val movd_x :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movd_mq :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Instruction.t

val movd_mx :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val movd_qm :
  X86_family_encode.X86_ops.View.mmx -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val movd_xm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val vaddsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vaddsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsubsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsubsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmulsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmulsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdivsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdivsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vaddss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vaddss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsubss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsubss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmulss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmulss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdivss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vdivss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vaddps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vaddps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsubps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsubps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmulps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmulps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdivps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdivps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vaddpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vaddpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsubpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsubpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmulpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmulpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdivpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vdivpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandnps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandnps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vorps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vorps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vxorps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vxorps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandnpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vandnpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vorpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vorpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vxorpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vxorpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpcklps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpcklps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpckhps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpckhps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpcklpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpcklpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpckhpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vunpckhpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklqdq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklqdq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhqdq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhqdq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklbw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklbw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhbw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhbw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklwd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpcklwd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhwd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhwd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckldq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckldq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhdq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpunpckhdq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpaddq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsubq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqb :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqb_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqb_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqb_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqw_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqw_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqd :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqd_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpeqd_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpeqd_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtb :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtb_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtb_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtb_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtw :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtw_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtw_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtw_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtd :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtd_rrk :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpcmpgtd_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpcmpgtd_mrk :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.kmask ->
  X86_family_encode.Instruction.t

val vpacksswb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpacksswb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpackssdw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpackssdw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpackuswb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpackuswb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpand :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpand_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpandn :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpandn_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpor :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpor_m :
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminub :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminub_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxub :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxub_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminsw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpminsw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmaxsw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmaxsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmaxsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vminsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vminsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmaxss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmaxss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vminss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vminss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmaxps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmaxps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vminps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vminps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmaxpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmaxpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vminpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vminpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsqrtsd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsqrtsd_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsqrtss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsqrtss_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vsqrtps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsqrtps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsqrtpd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vsqrtpd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovaps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovaps_rm :
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovaps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovups :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovups_rm :
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovups_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovapd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovapd_rm :
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovapd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovupd :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovupd_rm :
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovupd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcomisd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcomisd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vucomisd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vucomisd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vcomiss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcomiss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vucomiss :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vucomiss_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vcvtps2pd :
  X86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvtps2pd_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.Instruction.t

val vcvtps2pd_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvtpd2ps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcvtpd2ps_rr :
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcvtpd2ps_zy :
  X86_family_encode.X86_ops.View.zmm ->
  X86_family_encode.X86_ops.View.ymm ->
  X86_family_encode.Instruction.t

val vcvtpd2ps_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.ymm -> X86_family_encode.Instruction.t

val vcvtdq2ps :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvtdq2ps_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvtps2dq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvtps2dq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvttps2dq :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcvttps2dq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufps_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufps_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufps_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufpd_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufpd_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufpd_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vshufpd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmpss_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpss_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpss_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpss_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpsd_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpsd_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpsd_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpsd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vcmpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmpps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmpps_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmpps_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmppd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmppd_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmppd_im :
  int ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vcmppd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufd_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufd_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufd_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufd_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshuflw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshuflw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshuflw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshuflw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufhw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufhw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufhw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpshufhw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovd :
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmovd_rr :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vmovd_xx :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vmovd_m :
  X86_family_encode.Mem.t -> X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Instruction.t

val vmovd_rm :
  X86_family_encode.X86_ops.View.xmm -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val vpmullw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmullw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhuw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpmulhuw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpavgb :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpavgb_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpavgw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpavgw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsadbw :
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsadbw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslld_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsllq_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrld_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrlq_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsraw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad :
  X86_family_encode.X86_ops.View.xmm ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad_m :
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrad_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslldq_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslldq_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslldq_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpslldq_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrldq_i :
  int ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrldq_i_e :
  Asm_core.Expr.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrldq_im :
  int ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vpsrldq_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovdqa :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovdqa_rm :
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovdqa_m :
  X86_family_encode.Mem.t ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovdqu :
  ([< `L128 | `L256 ] as 'a) X86_family_encode.X86_ops.View.vec ->
  'a X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val vmovdqu_rm :
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vmovdqu_m :
  X86_family_encode.Mem.t ->
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.Instruction.t

val pinsrw_irq :
  int ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pinsrw_irq_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pinsrw_irx :
  int ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrw_irx_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrw_imq :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pinsrw_imq_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.Instruction.t

val pinsrw_imx :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pinsrw_imx_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val pextrw_iq :
  int ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrw_iq_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pextrw_irm :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val pextrw_irm_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vpinsrw_i :
  int ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vpinsrw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vpinsrw_im :
  int ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vpinsrw_im_e :
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Instruction.t

val vpextrw_i :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vpextrw_i_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vpextrw_irm :
  int ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val vpextrw_irm_e :
  Asm_core.Expr.t ->
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.Mem.t ->
  X86_family_encode.Instruction.t

val movmskps :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movmskpd :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pmovmskb_q :
  X86_family_encode.X86_ops.View.mmx ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val pmovmskb_x :
  X86_family_encode.X86_ops.View.xmm ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vmovmskps :
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vmovmskpd :
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val vpmovmskb :
  [< `L128 | `L256 ] X86_family_encode.X86_ops.View.vec ->
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val fldl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fstpl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fstps_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val flds : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val flds_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fildll_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fadds : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val fadds_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fadd : X86_family_encode.X86_ops.View.st -> X86_family_encode.Instruction.t
val fadd_r : X86_family_encode.X86_ops.View.st -> X86_family_encode.Instruction.t
val fnstcw_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fldcw_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fistpll_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fsubs : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val fsubs_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val fnstsw : X86_family_encode.Instruction.t
val fnstsw_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val sahf : X86_family_encode.Instruction.t

val cmovo :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovo_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jo : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val seto :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val seto_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovno :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovno_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jno : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setno :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setno_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovb :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovb_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jb : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setb :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setb_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovae :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovae_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jae : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setae :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setae_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmove :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmove_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val je : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val sete :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sete_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovne :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovne_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jne : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setne :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setne_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovbe :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovbe_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jbe : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setbe :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setbe_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmova :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmova_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val ja : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val seta :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val seta_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovs :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovs_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val js : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val sets :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val sets_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovns :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovns_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jns : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setns :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setns_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovp :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovp_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jp : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setp :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setp_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovnp :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnp_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jnp : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setnp :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnp_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovl :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovl_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jl : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setl :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovge :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovge_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jge : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setge :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setge_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovle :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovle_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jle : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setle :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setle_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovg :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovg_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jg : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setg :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setg_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t
val loop : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val loope : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val loopne : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jecxz : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jrcxz : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val movzb :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movzb_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movzw :
  X86_family_encode.X86_ops.View.w16 X86_family_encode.X86_ops.View.gpr ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movzw_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsb :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsb_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsw :
  X86_family_encode.X86_ops.View.w16 X86_family_encode.X86_ops.View.gpr ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsw_m :
  X86_family_encode.Mem.t ->
  [< `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsl :
  X86_family_encode.X86_ops.View.w32 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val movsl_m :
  X86_family_encode.Mem.t ->
  X86_family_encode.X86_ops.View.w64 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val jcc : X86_family_encode__X86_types.Cc.t -> Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jc : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnae : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnb : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnc : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jz : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnz : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jna : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnbe : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jpe : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jpo : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnge : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnl : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jng : Asm_core.Expr.t -> X86_family_encode.Instruction.t
val jnle : Asm_core.Expr.t -> X86_family_encode.Instruction.t

val setc :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setc_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnae :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnae_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnb :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnb_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnc :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnc_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setz :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setz_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnz :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnz_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setna :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setna_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnbe :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnbe_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setpe :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setpe_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setpo :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setpo_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnge :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnge_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnl :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnl_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setng :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setng_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val setnle :
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setnle_m : X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovc :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovc_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnae :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnae_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnb :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnb_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnc :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnc_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovz :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovz_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnz :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnz_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovna :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovna_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnbe :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnbe_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovpe :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovpe_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovpo :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovpo_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnge :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnge_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnl :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnl_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovng :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovng_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnle :
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovnle_m :
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val rip : ?disp:int64 -> unit -> X86_family_encode.Mem.t
val rip_e : Asm_core.Expr.t -> X86_family_encode.Mem.t
