(* The x86-32 builders: the registers this mode has, by name and by class, and the instruction
   builders that exist in it. *)

include module type of struct
  include Rivet_x86_asm
end

module Rows : module type of struct
  include X86_asm_rows32
end

include module type of struct
  include X86_asm_rows32
end

type st = View.st

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

module Gpr8_high : sig
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

val ah : Gpr8_high.t
val ch : Gpr8_high.t
val dh : Gpr8_high.t
val bh : Gpr8_high.t

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
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val add_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val add_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val add_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val add_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val addb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val addb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val addl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val addl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sub :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sub_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val sub_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sub_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sub_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val subb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val subb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val subl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val subl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val mov :
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val mov_rm :
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val mov_rs :
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Instruction.t

val mov_i :
  int64 -> 'a Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val mov_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val mov_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val mov_s :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val movb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val movw_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val movw_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val movl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val movl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val lea_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val lea_s :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val ret : Rivet_x86_family_encode.Instruction.t

val xor :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val xor_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val xor_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val xor_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val xor_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val xorb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val xorb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val xorl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val xorl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val and_ :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val and_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val and_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val and_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val and_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val andb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val andb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val andl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val andl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmp :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmp_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val cmp_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmp_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmp_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmpb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmpb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmpl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmpl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val imul :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val imul_rr :
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imull_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val imul_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imul_s :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imul_i :
  int64 ->
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imul_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imul_im :
  int64 ->
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val imul_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val ud2 : Rivet_x86_family_encode.Instruction.t

val pop :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pop_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val bswap :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val jmp : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val jmp_r :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jmp_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val call : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val call_r :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val call_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val push :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val push_i : int64 -> Rivet_x86_family_encode.Instruction.t
val push_i_e : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val neg :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val negl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val test :
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val test_rm :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val test_i :
  int64 ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val test_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val testl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val testl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val adc :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val adc_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val adc_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val adc_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val adc_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val adcb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val adcb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val adcl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val adcl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sbb :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sbb_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val sbb_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sbb_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sbb_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sbbb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sbbb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sbbl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sbbl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val mul :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val mull_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val div :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val divl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val dec :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val dec_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val rcr :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val rcrl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val rcr_cl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val rcr_i :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val rcr_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val rcrl_clm : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val rcrl_im : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val rcrl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shr :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val shrl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shr_cl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val shr_i :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shr_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shrl_clm : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val shrl_im : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shrl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val or_ :
  ([< `W32 | `W64 | `W8 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val or_rm :
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val or_i :
  int64 ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val or_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val or_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 | `W8 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val orb_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val orb_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val orl_im : int64 -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val orl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val not :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val notl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val ror :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val rorl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val ror_cl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val ror_i :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val ror_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val rorl_clm : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val rorl_im : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val rorl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val shll_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shl_cl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val shl_i :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shl_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shll_clm : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val shll_im : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shll_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sar :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val sarl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sar_cl :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr -> Rivet_x86_family_encode.Instruction.t

val sar_i :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sar_i_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sarl_clm : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val sarl_im : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val sarl_im_e :
  Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val shld_cl :
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shld_i :
  int ->
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shld_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val shld_clrm :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val shld_irm :
  int ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val shld_irm_e :
  Rivet_asm_core.Expr.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val addsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val comisd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val comisd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ucomisd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ucomisd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val comiss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val comiss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ucomiss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ucomiss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val xorpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val xorpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pxor_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pxor :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pxor_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pxor_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movapd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movapd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movapd_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val cvtsd2ss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtsd2ss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtss2sd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtss2sd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtps2pd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtps2pd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtpd2ps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtpd2ps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtdq2ps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtdq2ps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtps2dq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtps2dq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvttps2dq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvttps2dq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movsd_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movss_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val cvtsi2sd :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtsi2sd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtsi2ss :
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvtsi2ss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cvttsd2si_x :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cvttsd2si_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val unpcklps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpcklps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpckhps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpckhps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpcklpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpcklpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpckhpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val unpckhpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklqdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklqdq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhqdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhqdq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklbw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpcklbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklbw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpcklbw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhbw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhbw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhbw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklwd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpcklwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpcklwd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpcklwd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhwd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhwd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhwd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckldq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckldq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckldq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckldq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhdq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val punpckhdq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val punpckhdq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val paddq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val paddq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psubq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psubq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packsswb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packsswb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packsswb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packsswb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packssdw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packssdw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packssdw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packssdw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packuswb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packuswb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packuswb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val packuswb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pand_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pand :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pand_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pand_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pandn_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pandn :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pandn_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pandn_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val por_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val por :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val por_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val por_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminub_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pminub :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminub_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pminub_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxub_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaxub :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxub_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaxub_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pminsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pminsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmullw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmullw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmullw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmullw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhuw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhuw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhuw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhuw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pavgb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pavgb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pavgb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pavgb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pavgw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pavgw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pavgw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pavgw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psadbw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psadbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psadbw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psadbw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllw_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psllw_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllw_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psllw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pslld_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pslld :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pslld_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val pslld_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pslld_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val pslld_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pslld_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pslld_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllq_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psllq_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllq_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psllq_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psllq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psllq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlw_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psrlw_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlw_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psrlw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrld_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrld :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrld_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psrld_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrld_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psrld_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrld_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrld_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlq_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlq_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psrlq_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlq_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psrlq_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrlq_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrlq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psraw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psraw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psraw_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psraw_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psraw_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psraw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psraw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psraw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrad_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrad :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrad_iq :
  int -> Rivet_x86_family_encode.X86_ops.View.mmx -> Rivet_x86_family_encode.Instruction.t

val psrad_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrad_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psrad_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrad_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psrad_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pslldq_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val pslldq_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psrldq_i :
  int -> Rivet_x86_family_encode.X86_ops.View.xmm -> Rivet_x86_family_encode.Instruction.t

val psrldq_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pshufb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pshufb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val psignd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val psignd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaddubsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaddubsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaddubsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmaddubsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhrsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhrsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulhrsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pmulhrsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phaddsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phaddsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phsubsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val phsubsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsb_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsb_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsw_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsw_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsd_qq :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pabsd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pabsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val palignr_iqq :
  int ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val palignr_iqq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val palignr_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val palignr_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val palignr_imq :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val palignr_imq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val palignr_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val palignr_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundpd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundpd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundpd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundpd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundss_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundss_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundss_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundss_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundsd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundsd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundsd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val roundsd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpeqq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pcmpgtq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packusdw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val packusdw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxud :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxud_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxuw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmaxuw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminud :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminud_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminuw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pminuw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmuldq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmuldq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulld :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmulld_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phminposuw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val phminposuw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ptest :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val ptest_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxbq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxwd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxwq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxwq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovsxdq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxbq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxwd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxwq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxwq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pmovzxdq_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movntdqa_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendvps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendvps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendvpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendvpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendvb :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendvb_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendpd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendpd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendpd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val blendpd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dpps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dpps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dpps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dpps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dppd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dppd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dppd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val dppd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mpsadbw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mpsadbw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mpsadbw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mpsadbw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pblendw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val insertps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val insertps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val insertps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val insertps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrb_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrb_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrb_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrb_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pextrb_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrb_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrb_irm :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val pextrb_irm_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val pextrd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrd_irm :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val pextrd_irm_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val extractps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val extractps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val extractps_irm :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val extractps_irm_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vpshufb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddd :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddd_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddsw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphaddsw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaddubsw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaddubsw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubd :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubd_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubsw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphsubsw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignb :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignb_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignd :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsignd_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhrsw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhrsw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmuldq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmuldq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqq :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqq_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqq_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqq_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpackusdw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpackusdw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtq :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtq_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtq_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtq_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpminsb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminsb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminsd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminsd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminuw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminuw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminud :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminud_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxuw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxuw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxud :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxud_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulld :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulld_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpalignr_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpalignr_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpalignr_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpalignr_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendps_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendpd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendpd_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendpd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vblendpd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpblendw_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpblendw_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpblendw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpblendw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vroundss_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundss_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundss_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundss_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundsd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundsd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundsd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vroundsd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdpps_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdpps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdpps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdppd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdppd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdppd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdppd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmpsadbw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmpsadbw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmpsadbw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmpsadbw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vinsertps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vinsertps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vinsertps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vinsertps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vpabsb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpabsb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpabsw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpabsw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpabsd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpabsd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vphminposuw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vphminposuw_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vptest :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vptest_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbw_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxbq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxwd_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxwd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxwq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxwq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxdq_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovsxdq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbw_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxbq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxwd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxwd_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxwd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxwq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxwq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxdq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxdq_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vpmovzxdq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovntdqa_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val movdqa :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movdqa_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movdqa_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movdqu :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movdqu_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movdqu_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val andps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andnps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andnps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val orps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val orps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val xorps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val xorps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andnpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val andnpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val orpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val orpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movaps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movaps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movaps_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movups :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movups_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movups_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movupd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movupd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movupd_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val addps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val addpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val subpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val mulpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val divpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val maxpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val minpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val sqrtpd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufpd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufpd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufpd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val shufpd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpss_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpss_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpss_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpss_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpsd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpsd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpsd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpsd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpps_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpps_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmpps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmppd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmppd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmppd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val cmppd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshuflw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshuflw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshuflw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshuflw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufhw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufhw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufhw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pshufhw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movd_q :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movd_rq :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val movd_rx :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movd_x :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movd_mq :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val movd_mx :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val movd_qm :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movd_xm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vaddsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vaddsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsubsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsubsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmulsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmulsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdivsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdivsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vaddss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vaddss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsubss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsubss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmulss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmulss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdivss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vdivss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vaddps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vaddps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsubps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsubps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmulps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmulps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdivps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdivps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vaddpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vaddpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsubpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsubpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmulpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmulpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdivpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vdivpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandnps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandnps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vorps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vorps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vxorps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vxorps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandnpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vandnpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vorpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vorpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vxorpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vxorpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpcklps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpcklps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpckhps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpckhps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpcklpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpcklpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpckhpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vunpckhpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklqdq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklqdq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhqdq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhqdq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklbw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklbw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhbw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhbw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklwd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpcklwd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhwd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhwd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckldq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckldq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhdq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpunpckhdq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpaddq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsubq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqb :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqb_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqb_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqb_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqw_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqw_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqd :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqd_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqd_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpeqd_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtb :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtb_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtb_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtb_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtw :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtw_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtw_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtw_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtd :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtd_rrk :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtd_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpcmpgtd_mrk :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.kmask ->
  Rivet_x86_family_encode.Instruction.t

val vpacksswb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpacksswb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpackssdw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpackssdw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpackuswb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpackuswb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpand :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpand_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpandn :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpandn_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpor :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpor_m :
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminub :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminub_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxub :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxub_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminsw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpminsw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmaxsw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmaxsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmaxsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vminsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vminsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmaxss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmaxss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vminss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vminss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmaxps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmaxps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vminps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vminps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmaxpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmaxpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vminpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vminpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtsd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtsd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtpd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vsqrtpd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovaps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovaps_rm :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovaps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovups :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovups_rm :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovups_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovapd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovapd_rm :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovapd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovupd :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovupd_rm :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovupd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcomisd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcomisd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vucomisd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vucomisd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcomiss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcomiss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vucomiss :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vucomiss_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtps2pd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvtps2pd_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtps2pd_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvtpd2ps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtpd2ps_rr :
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtpd2ps_zy :
  Rivet_x86_family_encode.X86_ops.View.zmm ->
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtpd2ps_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.ymm ->
  Rivet_x86_family_encode.Instruction.t

val vcvtdq2ps :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvtdq2ps_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvtps2dq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvtps2dq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvttps2dq :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcvttps2dq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufps_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufps_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufpd_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufpd_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufpd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vshufpd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmpss_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpss_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpss_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpss_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpsd_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpsd_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpsd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpsd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vcmpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmpps_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmpps_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmpps_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmppd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmppd_i_e :
  Rivet_asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmppd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vcmppd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufd_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufd_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufd_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufd_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshuflw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshuflw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshuflw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshuflw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufhw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufhw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufhw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpshufhw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovd :
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmovd_rr :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vmovd_xx :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmovd_m :
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vmovd_rm :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vpmullw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmullw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhuw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpmulhuw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpavgb :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpavgb_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpavgw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpavgw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsadbw :
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsadbw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslld_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsllq_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrld_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrlq_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsraw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad_m :
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrad_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslldq_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslldq_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslldq_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpslldq_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrldq_i :
  int ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrldq_i_e :
  Rivet_asm_core.Expr.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrldq_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vpsrldq_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqa :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqa_rm :
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqa_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqu :
  ([< `L128 | `L256 ] as 'a) Rivet_x86_family_encode.X86_ops.View.vec ->
  'a Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqu_rm :
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vmovdqu_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_irq :
  int ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_irq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_irx :
  int ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_irx_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_imq :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_imq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_imx :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pinsrw_imx_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_iq :
  int ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_iq_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_irm :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val pextrw_irm_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vpinsrw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vpinsrw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vpinsrw_im :
  int ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vpinsrw_im_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Instruction.t

val vpextrw_i :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vpextrw_i_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vpextrw_irm :
  int ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val vpextrw_irm_e :
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val movmskps :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movmskpd :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pmovmskb_q :
  Rivet_x86_family_encode.X86_ops.View.mmx ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val pmovmskb_x :
  Rivet_x86_family_encode.X86_ops.View.xmm ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vmovmskps :
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vmovmskpd :
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val vpmovmskb :
  [< `L128 | `L256 ] Rivet_x86_family_encode.X86_ops.View.vec ->
  Rivet_x86_family_encode.X86_ops.View.w32 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val fldl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fstpl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fstps_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val flds : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val flds_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fildll_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fadds : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val fadds_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fadd : Rivet_x86_family_encode.X86_ops.View.st -> Rivet_x86_family_encode.Instruction.t
val fadd_r : Rivet_x86_family_encode.X86_ops.View.st -> Rivet_x86_family_encode.Instruction.t
val fnstcw_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fldcw_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fistpll_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fsubs : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val fsubs_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val fnstsw : Rivet_x86_family_encode.Instruction.t
val fnstsw_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val sahf : Rivet_x86_family_encode.Instruction.t

val cmovo :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovo_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jo : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val seto :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val seto_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovno :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovno_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jno : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setno :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setno_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovb :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovb_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jb : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setb :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setb_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovae :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovae_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jae : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setae :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setae_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmove :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmove_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val je : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val sete :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sete_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovne :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovne_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jne : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setne :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setne_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovbe :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovbe_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jbe : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setbe :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setbe_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmova :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmova_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val ja : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val seta :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val seta_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovs :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovs_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val js : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val sets :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val sets_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovns :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovns_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jns : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setns :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setns_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovp :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovp_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jp : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setp :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setp_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovnp :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnp_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jnp : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setnp :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnp_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovl :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovl_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jl : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setl :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovge :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovge_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jge : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setge :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setge_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovle :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovle_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jle : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setle :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setle_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovg :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovg_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jg : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setg :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setg_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t
val loop : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val loope : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val loopne : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jcxz : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jecxz : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val movzb :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movzb_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movzw :
  Rivet_x86_family_encode.X86_ops.View.w16 Rivet_x86_family_encode.X86_ops.View.gpr ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movzw_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movsb :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movsb_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movsw :
  Rivet_x86_family_encode.X86_ops.View.w16 Rivet_x86_family_encode.X86_ops.View.gpr ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val movsw_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val jcc :
  Rivet_x86_family_encode__X86_types.Cc.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Instruction.t

val jc : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnae : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnb : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnc : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jz : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnz : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jna : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnbe : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jpe : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jpo : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnge : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnl : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jng : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t
val jnle : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Instruction.t

val setc :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setc_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnae :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnae_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnb :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnb_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnc :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnc_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setz :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setz_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnz :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnz_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setna :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setna_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnbe :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnbe_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setpe :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setpe_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setpo :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setpo_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnge :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnge_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnl :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnl_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setng :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setng_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val setnle :
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setnle_m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Instruction.t

val cmovc :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovc_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnae :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnae_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnb :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnb_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnc :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnc_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovz :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovz_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnz :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnz_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovna :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovna_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnbe :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnbe_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovpe :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovpe_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovpo :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovpo_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnge :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnge_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnl :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnl_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovng :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovng_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnle :
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovnle_m :
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t
