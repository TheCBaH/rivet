(* One definition per hand-written x86 opcode: the encoder's constants for each of its forms, the
   component that publishes it, the mode it exists in, its flag effects, and whether its lowering
   stays code. Its typed builders follow, one per operand shape, named by the mnemonic with a
   suffix for the shape ([add rax rbx], [add_i 5L rax], [add_m (mem rax) rbx]); a shape whose
   operands do not fix the operand size carries the AT&T size letter ([addl_im]). *)

module View : sig
  type w8 = [ `W8 ]
  type w16 = [ `W16 ]
  type w32 = [ `W32 ]
  type w64 = [ `W64 ]
  type l128 = [ `L128 ]
  type l256 = [ `L256 ]
  type l512 = [ `L512 ]
  type lo = [ `Lo ]
  type hi = [ `Hi ]
  type (+'w, +'r) greg = private X86_types.Reg.t
  type (+'l, +'r) vreg = private X86_types.Reg.t
  type 'w gpr = ('w, lo) greg
  type 'l vec = ('l, lo) vreg
  type 'w agpr = ('w, hi) greg
  type 'l evec = ('l, hi) vreg
  type xmm = l128 vec
  type ymm = l256 vec
  type zmm = l512 vec
  type mmx = private X86_types.Reg.t
  type kmask = private X86_types.Reg.t
  type st = private X86_types.Reg.t
  type tmm = private X86_types.Reg.t
  type creg = private X86_types.Reg.t
  type dreg = private X86_types.Reg.t

  module Unsafe : sig
    val gpr : X86_types.Reg.t -> ('w, 'r) greg
    val vec : X86_types.Reg.t -> ('l, 'r) vreg
    val mmx : X86_types.Reg.t -> mmx
    val kmask : X86_types.Reg.t -> kmask
    val st : X86_types.Reg.t -> st
    val tmm : X86_types.Reg.t -> tmm
    val creg : X86_types.Reg.t -> creg
    val dreg : X86_types.Reg.t -> dreg
  end
end

module Kind : sig
  type t =
    | R
    | R8
    | R16
    | R32
    | R64
    | V
    | X
    | Y
    | Z
    | Mm
    | K
    | St0
    | Sti
    | M
    | I
    | I32
    | I64
    | S
    | Cl
    | Ax
    | Dx

  val name : t -> string
end

module Shape : sig
  type ('w, 'l, 'f, 'g, 'r) t =
    | Done : ('w, 'l, 'r, 'r, 'r) t
    | R : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'w View.gpr -> 'f, 'w View.gpr -> 'g, 'r) t
    | R8 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.w8 View.gpr -> 'f, View.w8 View.gpr -> 'g, 'r) t
    | R16 :
        ('w, 'l, 'f, 'g, 'r) t
        -> ('w, 'l, View.w16 View.gpr -> 'f, View.w16 View.gpr -> 'g, 'r) t
    | R32 :
        ('w, 'l, 'f, 'g, 'r) t
        -> ('w, 'l, View.w32 View.gpr -> 'f, View.w32 View.gpr -> 'g, 'r) t
    | R64 :
        ('w, 'l, 'f, 'g, 'r) t
        -> ('w, 'l, View.w64 View.gpr -> 'f, View.w64 View.gpr -> 'g, 'r) t
    | V : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'l View.vec -> 'f, 'l View.vec -> 'g, 'r) t
    | X : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.xmm -> 'f, View.xmm -> 'g, 'r) t
    | Y : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.ymm -> 'f, View.ymm -> 'g, 'r) t
    | Z : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.zmm -> 'f, View.zmm -> 'g, 'r) t
    | Mm : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.mmx -> 'f, View.mmx -> 'g, 'r) t
    | K : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.kmask -> 'f, View.kmask -> 'g, 'r) t
    | St0 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Sti : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, View.st -> 'f, View.st -> 'g, 'r) t
    | M : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, X86_types.Mem.t -> 'f, X86_types.Mem.t -> 'g, 'r) t
    | I : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | I32 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | I64 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | S : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, Asm_core.Expr.t -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Cl : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Ax : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Dx : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t

  val kinds : ('w, 'l, 'f, 'g, 'r) t -> Kind.t list
  val g : 'w View.gpr -> X86_types.Operand.t
  val gw : 'w View.gpr -> int
  val v : 'l View.vec -> X86_types.Operand.t
  val mm : View.mmx -> X86_types.Operand.t
  val km : View.kmask -> X86_types.Operand.t
  val sti : View.st -> X86_types.Operand.t
  val imm : int -> X86_types.Operand.t
  val imm64 : int64 -> X86_types.Operand.t
  val expr : Asm_core.Expr.t -> X86_types.Operand.t
  val st0 : X86_types.Operand.t
  val cl : X86_types.Operand.t
  val ax : X86_types.Operand.t
  val dx : X86_types.Operand.t
  val build : ('w, 'l, 'f, 'g, 'r) t -> (int option -> X86_types.Operand.t list -> 'r) -> 'f
  val build_e : ('w, 'l, 'f, 'g, 'r) t -> (int option -> X86_types.Operand.t list -> 'r) -> 'g
end

module Sample : sig
  type regs = { gpr : X86_types.Rclass.t -> int -> X86_types.Reg.t; base : X86_types.Reg.t }

  val apply : regs -> int -> int -> ('w, 'l, 'f, 'g, 'r) Shape.t -> 'f -> 'r
end

module Form : sig
  type t =
    | Alu_rm_r
    | Alu_r_rm
    | Alu_ext
    | Unary_ext
    | Shift_ext
    | Sse_binop_f2
    | Sse_binop_f3
    | Sse_binop_66
    | Sse_binop_none
    | Xmm_shift_ext
    | Xmm_shift_opcode
    | Sse_binop_0f38
    | Sse_binop_imm_0f3a
    | Sse_gpr_imm_0f3a
    | Vex3_map2_66
    | Vex3_map3_66
    | Vex3_map2_unop
    | Sse_binop_imm_none
    | Sse_binop_imm_66
    | Sse_binop_imm_f3
    | Sse_binop_imm_f2
    | Vex_scalar_f2
    | Vex_scalar_f3
    | Vex_scalar_none
    | Evex_ps
    | Vex_scalar_66
    | Evex_pd
    | Vex_unop_none
    | Vex_unop_66
    | Vex_unop_f3
    | Vex_binop_imm_none
    | Vex_binop_imm_66
    | Vex_binop_imm_f3
    | Vex_binop_imm_f2
    | Vex_unop_imm_66
    | Vex_unop_imm_f2
    | Vex_unop_imm_f3
    | Vex_shift_ext
    | Vex_shift_opcode
    | Sse_movmsk_none
    | Sse_movmsk_66
    | Vex_movmsk_none
    | Vex_movmsk_66

  val name : t -> string
end

type mode = Both | Only32 | Only64

type builder = {
  name : string;
  spelling : string;
  kinds : Kind.t list;
  widths : int list;
  lengths : int list;
  bmode : mode;
  sample : Sample.regs -> address:int -> width:int -> length:int -> X86_types.Instruction.t;
}

type entry = {
  op : X86_types.Opcode.t;
  forms : (Form.t * int) list;
  feature : string option;
  mode : mode;
  reads_flags : bool;
  writes_flags : bool;
  lower : bool;
  builders : builder list;
}

val registry : entry list ref

val def :
  ?forms:(Form.t * int) list ->
  ?feature:string ->
  ?mode:mode ->
  ?reads_flags:bool ->
  ?writes_flags:bool ->
  X86_types.Opcode.t ->
  builder list ->
  unit

val mk :
  ?width:int ->
  X86_family_encode__X86_types.Opcode.t ->
  ('a, 'b, 'c, 'd, X86_types.Instruction.t) Shape.t ->
  'c

val mk_e :
  ?width:int ->
  X86_family_encode__X86_types.Opcode.t ->
  ('a, 'b, 'c, 'd, X86_types.Instruction.t) Shape.t ->
  'd

val b :
  ?width:int ->
  ?widths:int list ->
  ?lengths:int list ->
  ?mode:mode ->
  string ->
  string ->
  X86_family_encode__X86_types.Opcode.t ->
  ('a, 'b, 'c, 'd, X86_types.Instruction.t) Shape.t ->
  builder

module S : sig
  val s_add :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_add_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_add_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_addb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_addl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_addq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_add_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_sub_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sub_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_subb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_subl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_subq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sub_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_mov :
    ('a, 'b, 'a View.gpr -> 'a View.gpr -> 'c, 'a View.gpr -> 'a View.gpr -> 'c, 'c) Shape.t

  val s_mov_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t

  val s_mov_q :
    ('a, 'b, View.mmx -> View.w64 View.gpr -> 'c, View.mmx -> View.w64 View.gpr -> 'c, 'c) Shape.t

  val s_mov_rq :
    ('a, 'b, View.w64 View.gpr -> View.mmx -> 'c, View.w64 View.gpr -> View.mmx -> 'c, 'c) Shape.t

  val s_mov_rm :
    ('a, 'b, 'a View.gpr -> X86_types.Mem.t -> 'c, 'a View.gpr -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_mov_rs :
    ('a, 'b, 'a View.gpr -> Asm_core.Expr.t -> 'c, 'a View.gpr -> Asm_core.Expr.t -> 'c, 'c) Shape.t

  val s_mov_i :
    ('a, 'b, int64 -> 'a View.gpr -> 'c, Asm_core.Expr.t -> 'a View.gpr -> 'c, 'c) Shape.t

  val s_mov_m :
    ('a, 'b, X86_types.Mem.t -> 'a View.gpr -> 'c, X86_types.Mem.t -> 'a View.gpr -> 'c, 'c) Shape.t

  val s_mov_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_mov_mx :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_mov_qm :
    ('a, 'b, View.mmx -> X86_types.Mem.t -> 'c, View.mmx -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_mov_s :
    ('a, 'b, Asm_core.Expr.t -> 'a View.gpr -> 'c, Asm_core.Expr.t -> 'a View.gpr -> 'c, 'c) Shape.t

  val s_mov_xm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movw_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_lea_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_lea_s :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ret_m32 : ('a, 'b, 'c, 'c, 'c) Shape.t
  val s_ret_m64 : ('a, 'b, 'c, 'c, 'c) Shape.t

  val s_xor :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_xor_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xor_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_xorb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_xorl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_xorq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_xor_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_ :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_and_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_and_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_andb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_andl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_andq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_and_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmp :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmp_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_cmp_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmp_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmpb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmpl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmpq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_imul : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_imul_rr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imul_rrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imull_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_imulq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_imul_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imul_s :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imul_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imul_mrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_imul_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ud2_m32 : ('a, 'b, 'c, 'c, 'c) Shape.t
  val s_ud2_m64 : ('a, 'b, 'c, 'c, 'c) Shape.t
  val s_pop_m32 : ('a, 'b, View.w32 View.gpr -> 'c, View.w32 View.gpr -> 'c, 'c) Shape.t
  val s_pop_m64 : ('a, 'b, View.w64 View.gpr -> 'c, View.w64 View.gpr -> 'c, 'c) Shape.t
  val s_pop_m_m32 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_pop_m_m64 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_bswap : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_jmp_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jmp_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jmp_r_m32 : ('a, 'b, View.w32 View.gpr -> 'c, View.w32 View.gpr -> 'c, 'c) Shape.t
  val s_jmp_r_m64 : ('a, 'b, View.w64 View.gpr -> 'c, View.w64 View.gpr -> 'c, 'c) Shape.t
  val s_jmp_m_m32 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_jmp_m_m64 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_call_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_call_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_call_r_m32 : ('a, 'b, View.w32 View.gpr -> 'c, View.w32 View.gpr -> 'c, 'c) Shape.t
  val s_call_r_m64 : ('a, 'b, View.w64 View.gpr -> 'c, View.w64 View.gpr -> 'c, 'c) Shape.t
  val s_call_m_m32 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_call_m_m64 : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_push_m32 : ('a, 'b, View.w32 View.gpr -> 'c, View.w32 View.gpr -> 'c, 'c) Shape.t
  val s_push_m64 : ('a, 'b, View.w64 View.gpr -> 'c, View.w64 View.gpr -> 'c, 'c) Shape.t
  val s_push_i_m32 : ('a, 'b, int64 -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_push_i_m64 : ('a, 'b, int64 -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_neg : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_neg_rr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_negl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_negq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_neg_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_test :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_test_rm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_test_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_testl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_testq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_adc :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_adc_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adc_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_adcb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_adcl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_adcq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_adc_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_sbb_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbb_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sbbb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sbbl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sbbq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sbb_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_mul : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_mull_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_mulq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_div : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_divl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_divq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_dec : ('a, 'b, View.w32 View.gpr -> 'c, View.w32 View.gpr -> 'c, 'c) Shape.t
  val s_dec_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rcr : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_rcrl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rcrq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rcr_cl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_rcr_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_rcr_clrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_rcr_irr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_rcrl_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rcrq_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_rcrl_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_rcrq_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_rcr_clm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_rcr_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shr : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_shrl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shrq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shr_cl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_shr_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shr_clrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shr_irr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shrl_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shrq_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shrl_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shrq_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shr_clm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shr_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_ :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_rrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_rm :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_or_i :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_m :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_rmr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_irr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_or_mrr :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_orb_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_orl_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_orq_im :
    ('a, 'b, int64 -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_or_im :
    ( ([< `W32 | `W64 | `W8 ] as 'a),
      'b,
      int64 -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_not : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_not_rr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_notl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_notq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_not_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ror : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_rorl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rorq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_ror_cl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_ror_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ror_clrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ror_irr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_rorl_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_rorq_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_rorl_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_rorq_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_ror_clm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ror_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_shll_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shlq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shl_cl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_shl_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shl_clrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shl_irr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shll_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_shlq_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shll_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shlq_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_shl_clm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shl_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sar : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t
  val s_sarl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_sarq_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_sar_cl : (([< `W32 | `W64 ] as 'a), 'b, 'a View.gpr -> 'c, 'a View.gpr -> 'c, 'c) Shape.t

  val s_sar_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sar_clrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sar_irr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sarl_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_sarq_clm : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sarl_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sarq_im :
    ('a, 'b, int -> X86_types.Mem.t -> 'c, Asm_core.Expr.t -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_sar_clm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_sar_im :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shld_cl :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shld_i :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shld_irrr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> 'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_shld_clrm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_shld_irm :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_shld_irmr :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      int -> 'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      Asm_core.Expr.t -> 'a View.gpr -> X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_addsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_addsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_subsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_subsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_mulsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_mulsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_divsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_divsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_addss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_addss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_subss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_subss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_mulss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_mulss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_divss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_divss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_comisd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_comisd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_ucomisd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_ucomisd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_comiss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_comiss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_ucomiss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_ucomiss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_xorpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_xorpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pxor_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pxor : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pxor_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pxor_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movapd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movapd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movapd_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cvtsd2ss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtsd2ss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtss2sd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtss2sd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtps2pd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtps2pd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtpd2ps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtpd2ps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtdq2ps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtdq2ps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtps2dq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtps2dq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvttps2dq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_cvttps2dq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movsd_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movss_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cvtsi2sd :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> View.xmm -> 'c,
      'a View.gpr -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cvtsi2sd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvtsi2ss :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> View.xmm -> 'c,
      'a View.gpr -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cvtsi2ss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_cvttsd2si_x :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      View.xmm -> 'a View.gpr -> 'c,
      View.xmm -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cvttsd2si_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cvttsd2si_mr :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.w64 View.gpr -> 'c,
      X86_types.Mem.t -> View.w64 View.gpr -> 'c,
      'c )
    Shape.t

  val s_unpcklps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_unpcklps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_unpckhps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_unpckhps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_unpcklpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_unpcklpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_unpckhpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_unpckhpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklqdq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklqdq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhqdq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhqdq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklbw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpcklbw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklbw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpcklbw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhbw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpckhbw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhbw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpckhbw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklwd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpcklwd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpcklwd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpcklwd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhwd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpckhwd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhwd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpckhwd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckldq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpckldq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckldq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpckldq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhdq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_punpckhdq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_punpckhdq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_punpckhdq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_paddb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_paddb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_paddb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_paddb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_paddw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_paddw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_paddw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_paddw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_paddd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_paddd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_paddd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_paddd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_paddq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_paddq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_paddq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_paddq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psubb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psubb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psubb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psubb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psubw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psubw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psubw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psubw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psubd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psubd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psubd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psubd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psubq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psubq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psubq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psubq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpeqb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpeqb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpeqw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpeqw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpeqd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpeqd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpgtb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpgtb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpgtw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpgtw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pcmpgtd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pcmpgtd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_packsswb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_packsswb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_packsswb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_packsswb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_packssdw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_packssdw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_packssdw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_packssdw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_packuswb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_packuswb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_packuswb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_packuswb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pand_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pand : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pand_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pand_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pandn_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pandn : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pandn_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pandn_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_por_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_por : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_por_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_por_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminub_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pminub : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminub_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pminub_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxub_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmaxub : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxub_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmaxub_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pminsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pminsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmaxsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmaxsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmullw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmullw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmullw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmullw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmulhw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmulhw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhuw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmulhuw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhuw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmulhuw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pavgb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pavgb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pavgb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pavgb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pavgw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pavgw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pavgw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pavgw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psadbw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psadbw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psadbw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psadbw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psllw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psllw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psllw_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psllw_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psllw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psllw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pslld_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pslld : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_pslld_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_pslld_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pslld_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pslld_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psllq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psllq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psllq_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psllq_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psllq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psllq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrlw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psrlw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psrlw_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psrlw_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrlw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psrlw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrld_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psrld : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psrld_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psrld_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrld_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psrld_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrlq_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psrlq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psrlq_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psrlq_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrlq_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psrlq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psraw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psraw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psraw_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psraw_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psraw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psraw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrad_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psrad : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_psrad_iq : ('a, 'b, int -> View.mmx -> 'c, Asm_core.Expr.t -> View.mmx -> 'c, 'c) Shape.t
  val s_psrad_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psrad_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psrad_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pslldq_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t
  val s_psrldq_i : ('a, 'b, int -> View.xmm -> 'c, Asm_core.Expr.t -> View.xmm -> 'c, 'c) Shape.t
  val s_pshufb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pshufb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pshufb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pshufb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phaddw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phaddw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phaddd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phaddd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phsubw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phsubw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phsubd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phsubd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psignb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psignb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psignb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psignb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psignw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psignw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psignw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psignw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_psignd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_psignd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_psignd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_psignd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaddubsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmaddubsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaddubsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmaddubsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhrsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pmulhrsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulhrsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pmulhrsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phaddsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phaddsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phaddsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_phsubsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phsubsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_phsubsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsb_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pabsb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsb_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pabsb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsw_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pabsw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsw_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pabsw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsd_qq : ('a, 'b, View.mmx -> View.mmx -> 'c, View.mmx -> View.mmx -> 'c, 'c) Shape.t
  val s_pabsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pabsd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_pabsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_palignr_iqq :
    ( 'a,
      'b,
      int -> View.mmx -> View.mmx -> 'c,
      Asm_core.Expr.t -> View.mmx -> View.mmx -> 'c,
      'c )
    Shape.t

  val s_palignr_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_palignr_imq :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.mmx -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.mmx -> 'c,
      'c )
    Shape.t

  val s_palignr_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundpd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundpd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundss_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundss_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundsd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_roundsd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pcmpeqq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpeqq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pcmpgtq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_packusdw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_packusdw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxud : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxud_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxuw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmaxuw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminud : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminud_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pminuw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pminuw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmuldq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmuldq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulld : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmulld_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_phminposuw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_phminposuw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_ptest : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_ptest_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxbq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxwd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxwd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxwq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxwq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxdq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovsxdq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxbq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxwd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxwd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxwq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxwq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxdq : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pmovzxdq_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movntdqa_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_blendvps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_blendvps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_blendvpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_blendvpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_pblendvb : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_pblendvb_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_blendps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_blendps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_blendpd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_blendpd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_dpps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_dpps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_dppd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_dppd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_mpsadbw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_mpsadbw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pblendw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pblendw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_insertps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_insertps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pinsrb_i :
    ( 'a,
      'b,
      int -> View.w32 View.gpr -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pinsrb_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pinsrd_i :
    ( 'a,
      'b,
      int -> View.w32 View.gpr -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pinsrd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pextrb_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_pextrb_irm :
    ( 'a,
      'b,
      int -> View.xmm -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_pextrd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_pextrd_irm :
    ( 'a,
      'b,
      int -> View.xmm -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_extractps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_extractps_irm :
    ( 'a,
      'b,
      int -> View.xmm -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_vpshufb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshufb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddd_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddsw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphaddsw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaddubsw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaddubsw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubd_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubsw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vphsubsw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignb :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignb_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsignd_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhrsw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhrsw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmuldq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmuldq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqq :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqq_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqq_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqq_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpackusdw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpackusdw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtq :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtq_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtq_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtq_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpminsb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminsb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminsd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminuw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminuw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminud :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminud_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxuw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxuw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxud :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxud_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulld :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulld_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpalignr_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpalignr_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vblendps_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vblendps_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vblendpd_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vblendpd_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpblendw_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpblendw_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vroundss_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vroundss_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vroundsd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vroundsd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdpps_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdpps_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdppd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdppd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmpsadbw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmpsadbw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vinsertps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vinsertps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vpabsb :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpabsb_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpabsw :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpabsw_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpabsd :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpabsd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vphminposuw : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vphminposuw_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vptest :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vptest_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovsxbw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovsxbw_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovsxbw_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxbd :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxbd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxbq :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxbq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxwd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovsxwd_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovsxwd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxwq :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxwq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovsxdq :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovsxdq_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovsxdq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxbw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovzxbw_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovzxbw_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxbd :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxbd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxbq :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxbq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxwd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovzxwd_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovzxwd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxwq :
    ('a, 'b, View.xmm -> 'b View.vec -> 'c, View.xmm -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxwq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vpmovzxdq :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmovzxdq_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vpmovzxdq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovntdqa_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_movdqa : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movdqa_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movdqa_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movdqu : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movdqu_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movdqu_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_andps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_andps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_andnps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_andnps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_orps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_orps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_xorps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_xorps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_andpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_andpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_andnpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_andnpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_orpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_orpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movaps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movaps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movaps_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movups : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movups_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movups_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movupd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_movupd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movupd_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_addps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_addps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_subps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_subps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_mulps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_mulps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_divps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_divps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_addpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_addpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_subpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_subpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_mulpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_mulpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_divpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_divpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_maxss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_maxss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_minss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_minss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_maxsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_maxsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_minsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_minsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_maxps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_maxps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_minps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_minps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_maxpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_maxpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_minpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_minpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtsd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtsd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtps_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtpd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_sqrtpd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_shufps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_shufps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_shufpd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_shufpd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpss_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpss_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpsd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpsd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpps_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmpps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmppd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_cmppd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshufd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshufd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshuflw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshuflw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshufhw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pshufhw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_movd_q :
    ('a, 'b, View.mmx -> View.w32 View.gpr -> 'c, View.mmx -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_movd_rq :
    ('a, 'b, View.w32 View.gpr -> View.mmx -> 'c, View.w32 View.gpr -> View.mmx -> 'c, 'c) Shape.t

  val s_movd_rx :
    ('a, 'b, View.w32 View.gpr -> View.xmm -> 'c, View.w32 View.gpr -> View.xmm -> 'c, 'c) Shape.t

  val s_movd_x :
    ('a, 'b, View.xmm -> View.w32 View.gpr -> 'c, View.xmm -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_movd_mq :
    ('a, 'b, X86_types.Mem.t -> View.mmx -> 'c, X86_types.Mem.t -> View.mmx -> 'c, 'c) Shape.t

  val s_movd_mx :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_movd_qm :
    ('a, 'b, View.mmx -> X86_types.Mem.t -> 'c, View.mmx -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_movd_xm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vaddsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vaddsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsubsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsubsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmulsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmulsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdivsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdivsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vaddss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vaddss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsubss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsubss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmulss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmulss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdivss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vdivss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vaddps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vaddps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vsubps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vsubps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmulps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmulps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdivps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdivps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vaddpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vaddpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vsubpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vsubpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmulpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmulpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdivpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vdivpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandnps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandnps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vorps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vorps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vxorps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vxorps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandnpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vandnpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vorpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vorpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vxorpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vxorpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpcklps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpcklps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpckhps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpckhps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpcklpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpcklpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpckhpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vunpckhpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklqdq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklqdq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhqdq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhqdq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklbw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklbw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhbw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhbw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklwd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpcklwd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhwd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhwd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckldq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckldq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhdq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpunpckhdq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpaddq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubq :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsubq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqb :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqb_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqb_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqb_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqw_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqw_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqd_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqd_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpeqd_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtb :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtb_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtb_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtb_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtw :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtw_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtw_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtw_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtd_rrk :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'b View.vec -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtd_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpcmpgtd_mrk :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      X86_types.Mem.t -> 'b View.vec -> View.kmask -> 'c,
      'c )
    Shape.t

  val s_vpacksswb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpacksswb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpackssdw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpackssdw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpackuswb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpackuswb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpand :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpand_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpandn :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpandn_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpor :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpor_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminub :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminub_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxub :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxub_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminsw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpminsw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmaxsw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmaxsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmaxsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vminsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vminsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmaxss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmaxss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vminss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vminss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vmaxps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmaxps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vminps :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vminps_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmaxpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmaxpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vminpd :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vminpd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vsqrtsd :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsqrtsd_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsqrtss :
    ( 'a,
      'b,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsqrtss_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vsqrtps :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vsqrtps_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vsqrtpd :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vsqrtpd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovaps :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovaps_rm :
    ('a, 'b, 'b View.vec -> X86_types.Mem.t -> 'c, 'b View.vec -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vmovaps_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovups :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovups_rm :
    ('a, 'b, 'b View.vec -> X86_types.Mem.t -> 'c, 'b View.vec -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vmovups_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovapd :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovapd_rm :
    ('a, 'b, 'b View.vec -> X86_types.Mem.t -> 'c, 'b View.vec -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vmovapd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovupd :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vmovupd_rm :
    ('a, 'b, 'b View.vec -> X86_types.Mem.t -> 'c, 'b View.vec -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vmovupd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcomisd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vcomisd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vucomisd : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vucomisd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vcomiss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vcomiss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vucomiss : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vucomiss_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vcvtps2pd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      View.xmm -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vcvtps2pd_rr : ('a, 'b, View.ymm -> View.zmm -> 'c, View.ymm -> View.zmm -> 'c, 'c) Shape.t

  val s_vcvtps2pd_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvtpd2ps : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t
  val s_vcvtpd2ps_rr : ('a, 'b, View.ymm -> View.xmm -> 'c, View.ymm -> View.xmm -> 'c, 'c) Shape.t
  val s_vcvtpd2ps_zy : ('a, 'b, View.zmm -> View.ymm -> 'c, View.zmm -> View.ymm -> 'c, 'c) Shape.t

  val s_vcvtpd2ps_m :
    ('a, 'b, X86_types.Mem.t -> View.ymm -> 'c, X86_types.Mem.t -> View.ymm -> 'c, 'c) Shape.t

  val s_vcvtdq2ps :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvtdq2ps_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvtps2dq :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvtps2dq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvttps2dq :
    ('a, 'b, 'b View.vec -> 'b View.vec -> 'c, 'b View.vec -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vcvttps2dq_m :
    ('a, 'b, X86_types.Mem.t -> 'b View.vec -> 'c, X86_types.Mem.t -> 'b View.vec -> 'c, 'c) Shape.t

  val s_vshufps_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vshufps_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vshufpd_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vshufpd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vcmpss_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vcmpss_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vcmpsd_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vcmpsd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vcmpps_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vcmpps_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vcmppd_i :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vcmppd_im :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      int -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshufd_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshufd_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshuflw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshuflw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshufhw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpshufhw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmovd :
    ('a, 'b, View.w32 View.gpr -> View.xmm -> 'c, View.w32 View.gpr -> View.xmm -> 'c, 'c) Shape.t

  val s_vmovd_rr :
    ('a, 'b, View.xmm -> View.w32 View.gpr -> 'c, View.xmm -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_vmovd_xx : ('a, 'b, View.xmm -> View.xmm -> 'c, View.xmm -> View.xmm -> 'c, 'c) Shape.t

  val s_vmovd_m :
    ('a, 'b, X86_types.Mem.t -> View.xmm -> 'c, X86_types.Mem.t -> View.xmm -> 'c, 'c) Shape.t

  val s_vmovd_rm :
    ('a, 'b, View.xmm -> X86_types.Mem.t -> 'c, View.xmm -> X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_vpmullw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmullw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhuw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpmulhuw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpavgb :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpavgb_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpavgw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpavgw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsadbw :
    ( 'a,
      'b,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsadbw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllw :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslld :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslld_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslld_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslld_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllq :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllq_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsllq_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlw :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrld :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrld_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrld_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrld_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlq :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlq_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlq_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrlq_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsraw :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsraw_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsraw_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsraw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrad :
    ( 'a,
      'b,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      View.xmm -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrad_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrad_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrad_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslldq_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpslldq_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrldq_i :
    ( 'a,
      'b,
      int -> 'b View.vec -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> 'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vpsrldq_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> 'b View.vec -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmovdqa :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmovdqa_rm :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> X86_types.Mem.t -> 'c,
      'b View.vec -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_vmovdqa_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmovdqu :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> 'b View.vec -> 'c,
      'b View.vec -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_vmovdqu_rm :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> X86_types.Mem.t -> 'c,
      'b View.vec -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_vmovdqu_m :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      X86_types.Mem.t -> 'b View.vec -> 'c,
      X86_types.Mem.t -> 'b View.vec -> 'c,
      'c )
    Shape.t

  val s_pinsrw_irq :
    ( 'a,
      'b,
      int -> View.w32 View.gpr -> View.mmx -> 'c,
      Asm_core.Expr.t -> View.w32 View.gpr -> View.mmx -> 'c,
      'c )
    Shape.t

  val s_pinsrw_irx :
    ( 'a,
      'b,
      int -> View.w32 View.gpr -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pinsrw_imq :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.mmx -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.mmx -> 'c,
      'c )
    Shape.t

  val s_pinsrw_imx :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_pextrw_iq :
    ( 'a,
      'b,
      int -> View.mmx -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.mmx -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_pextrw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_pextrw_irm :
    ( 'a,
      'b,
      int -> View.xmm -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_vpinsrw_i :
    ( 'a,
      'b,
      int -> View.w32 View.gpr -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vpinsrw_im :
    ( 'a,
      'b,
      int -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> 'c,
      'c )
    Shape.t

  val s_vpextrw_i :
    ( 'a,
      'b,
      int -> View.xmm -> View.w32 View.gpr -> 'c,
      Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_vpextrw_irm :
    ( 'a,
      'b,
      int -> View.xmm -> X86_types.Mem.t -> 'c,
      Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> 'c,
      'c )
    Shape.t

  val s_movmskps :
    ('a, 'b, View.xmm -> View.w32 View.gpr -> 'c, View.xmm -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_movmskpd :
    ('a, 'b, View.xmm -> View.w32 View.gpr -> 'c, View.xmm -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_pmovmskb_q :
    ('a, 'b, View.mmx -> View.w32 View.gpr -> 'c, View.mmx -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_pmovmskb_x :
    ('a, 'b, View.xmm -> View.w32 View.gpr -> 'c, View.xmm -> View.w32 View.gpr -> 'c, 'c) Shape.t

  val s_vmovmskps :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> View.w32 View.gpr -> 'c,
      'b View.vec -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_vmovmskpd :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> View.w32 View.gpr -> 'c,
      'b View.vec -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_vpmovmskb :
    ( 'a,
      ([< `L128 | `L256 ] as 'b),
      'b View.vec -> View.w32 View.gpr -> 'c,
      'b View.vec -> View.w32 View.gpr -> 'c,
      'c )
    Shape.t

  val s_fldl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fstpl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fstps_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_flds : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_flds_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fildll_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fadds : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_fadds_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fadd : ('a, 'b, View.st -> 'c, View.st -> 'c, 'c) Shape.t
  val s_fadd_r : ('a, 'b, View.st -> 'c, View.st -> 'c, 'c) Shape.t
  val s_fnstcw_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fldcw_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fistpll_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fsubs : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_fsubs_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_fnstsw : ('a, 'b, 'c, 'c, 'c) Shape.t
  val s_fnstsw_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_sahf : ('a, 'b, 'c, 'c, 'c) Shape.t

  val s_cmovo :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovo_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jo_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jo_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_seto : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_seto_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovno :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovno_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jno_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jno_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setno : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setno_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovb :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovb_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jb_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jb_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setb : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setb_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovae :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovae_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jae_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jae_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setae : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setae_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmove :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmove_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_je_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_je_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_sete : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_sete_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovne :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovne_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jne_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jne_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setne : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setne_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovbe :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovbe_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jbe_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jbe_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setbe : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setbe_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmova :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmova_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_ja_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_ja_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_seta : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_seta_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovs :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovs_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_js_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_js_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_sets : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_sets_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovns :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovns_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jns_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jns_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setns : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setns_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovp :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovp_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jp_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jp_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setp : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setp_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovnp :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovnp_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jnp_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jnp_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setnp : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setnp_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovl :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovl_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jl_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jl_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setl : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setl_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovge :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovge_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jge_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jge_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setge : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setge_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovle :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovle_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jle_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jle_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setle : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setle_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t

  val s_cmovg :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      'a View.gpr -> 'a View.gpr -> 'c,
      'a View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_cmovg_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_jg_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jg_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_setg : ('a, 'b, View.w8 View.gpr -> 'c, View.w8 View.gpr -> 'c, 'c) Shape.t
  val s_setg_m : ('a, 'b, X86_types.Mem.t -> 'c, X86_types.Mem.t -> 'c, 'c) Shape.t
  val s_loop_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_loop_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_loope_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_loope_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_loopne_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_loopne_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jcxz : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jecxz_m32 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jecxz_m64 : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t
  val s_jrcxz : ('a, 'b, Asm_core.Expr.t -> 'c, Asm_core.Expr.t -> 'c, 'c) Shape.t

  val s_movzb :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      View.w8 View.gpr -> 'a View.gpr -> 'c,
      View.w8 View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movzb_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movzw :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      View.w16 View.gpr -> 'a View.gpr -> 'c,
      View.w16 View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movzw_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsb :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      View.w8 View.gpr -> 'a View.gpr -> 'c,
      View.w8 View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsb_m :
    ( ([< `W16 | `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsw :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      View.w16 View.gpr -> 'a View.gpr -> 'c,
      View.w16 View.gpr -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsw_m :
    ( ([< `W32 | `W64 ] as 'a),
      'b,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      X86_types.Mem.t -> 'a View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsl :
    ( 'a,
      'b,
      View.w32 View.gpr -> View.w64 View.gpr -> 'c,
      View.w32 View.gpr -> View.w64 View.gpr -> 'c,
      'c )
    Shape.t

  val s_movsl_m :
    ( 'a,
      'b,
      X86_types.Mem.t -> View.w64 View.gpr -> 'c,
      X86_types.Mem.t -> View.w64 View.gpr -> 'c,
      'c )
    Shape.t
end

val all : entry array
val index : (X86_types.Opcode.t, entry) Hashtbl.t lazy_t
val entry : X86_types.Opcode.t -> entry option
val code : Form.t -> X86_types.Opcode.t -> int option
val code_or_none : Form.t -> X86_types.Opcode.t -> int
val of_code : Form.t -> int -> X86_types.Opcode.t option
val table : Form.t -> (X86_types.Opcode.t * int64) list
val shift_ext_opcode : X86_types.Opcode.t -> (int * int) option
val of_shift : ext:Form.t * Form.t -> opcode:int -> int -> X86_types.Opcode.t option
val of_xmm_shift : opcode:int -> int -> X86_types.Opcode.t option
val of_vex_shift : opcode:int -> int -> X86_types.Opcode.t option
val feature : X86_types.Opcode.t -> string option
val plain : X86_types.Opcode.t -> bool
val vec_len : X86_types.Reg.t -> int
val fits_loosely : builder -> X86_types.Operand.t list -> bool
val length_of : builder -> int -> int
val read_back : X86_types.Opcode.t -> X86_types.Operand.t list -> X86_types.Operand.t list
val in_forms : X86_types.Opcode.t -> Form.t list -> bool
val of_feature : string -> X86_types.Opcode.t list
val in_mode : bits:int -> X86_types.Opcode.t -> bool
val add : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val add_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val add_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val add_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val add_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val add_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val add_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val add_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val add_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val add_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val addb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val addb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val addl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val addl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val addq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val addq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val add_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val add_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sub : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sub_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sub_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val sub_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val sub_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val sub_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sub_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val sub_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sub_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val sub_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val subb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val subb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val subl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val subl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val subq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val subq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sub_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sub_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val mov : 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val mov_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val mov_q : View.mmx -> View.w64 View.gpr -> X86_types.Instruction.t
val mov_rq : View.w64 View.gpr -> View.mmx -> X86_types.Instruction.t
val mov_rm : 'a View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val mov_rs : 'a View.gpr -> Asm_core.Expr.t -> X86_types.Instruction.t
val mov_i : int64 -> 'a View.gpr -> X86_types.Instruction.t
val mov_i_e : Asm_core.Expr.t -> 'a View.gpr -> X86_types.Instruction.t
val mov_m : X86_types.Mem.t -> 'a View.gpr -> X86_types.Instruction.t
val mov_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val mov_mx : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mov_qm : View.mmx -> X86_types.Mem.t -> X86_types.Instruction.t
val mov_s : Asm_core.Expr.t -> 'a View.gpr -> X86_types.Instruction.t
val mov_xm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val movb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val movw_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val movw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val movl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val movl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val movq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val movq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val lea_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val lea_s : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ret_m32 : X86_types.Instruction.t
val ret_m64 : X86_types.Instruction.t
val xor : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val xor_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val xor_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val xor_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val xor_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val xor_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val xor_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val xor_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val xor_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val xor_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val xorb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val xorb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val xorl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val xorl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val xorq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val xorq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val xor_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val xor_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val and_ : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val and_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val and_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val and_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val and_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val and_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val and_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val and_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val and_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val and_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val andb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val andb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val andl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val andl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val andq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val andq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val and_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val and_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val cmp : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmp_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val cmp_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val cmp_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val cmp_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val cmpb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val cmpb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val cmpl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val cmpl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val cmpq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val cmpq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val imul : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val imul_rr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val imul_rrr :
  ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val imull_m : X86_types.Mem.t -> X86_types.Instruction.t
val imulq_m : X86_types.Mem.t -> X86_types.Instruction.t
val imul_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val imul_s : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val imul_i : int64 -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val imul_i_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val imul_mrr :
  X86_types.Mem.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val imul_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val imul_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val ud2_m32 : X86_types.Instruction.t
val ud2_m64 : X86_types.Instruction.t
val pop_m32 : View.w32 View.gpr -> X86_types.Instruction.t
val pop_m64 : View.w64 View.gpr -> X86_types.Instruction.t
val pop_m_m32 : X86_types.Mem.t -> X86_types.Instruction.t
val pop_m_m64 : X86_types.Mem.t -> X86_types.Instruction.t
val bswap : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jmp_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jmp_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val jmp_r_m32 : View.w32 View.gpr -> X86_types.Instruction.t
val jmp_r_m64 : View.w64 View.gpr -> X86_types.Instruction.t
val jmp_m_m32 : X86_types.Mem.t -> X86_types.Instruction.t
val jmp_m_m64 : X86_types.Mem.t -> X86_types.Instruction.t
val call_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val call_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val call_r_m32 : View.w32 View.gpr -> X86_types.Instruction.t
val call_r_m64 : View.w64 View.gpr -> X86_types.Instruction.t
val call_m_m32 : X86_types.Mem.t -> X86_types.Instruction.t
val call_m_m64 : X86_types.Mem.t -> X86_types.Instruction.t
val push_m32 : View.w32 View.gpr -> X86_types.Instruction.t
val push_m64 : View.w64 View.gpr -> X86_types.Instruction.t
val push_i_m32 : int64 -> X86_types.Instruction.t
val push_i_m32_e : Asm_core.Expr.t -> X86_types.Instruction.t
val push_i_m64 : int64 -> X86_types.Instruction.t
val push_i_m64_e : Asm_core.Expr.t -> X86_types.Instruction.t
val neg : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val neg_rr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val negl_m : X86_types.Mem.t -> X86_types.Instruction.t
val negq_m : X86_types.Mem.t -> X86_types.Instruction.t
val neg_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val test : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val test_rm : [< `W32 | `W64 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val test_i : int64 -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val test_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val testl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val testl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val testq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val testq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val adc : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val adc_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val adc_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val adc_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val adc_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val adc_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val adc_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val adc_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val adc_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val adc_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val adcb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val adcb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val adcl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val adcl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val adcq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val adcq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val adc_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val adc_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sbb : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sbb_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sbb_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val sbb_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val sbb_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val sbb_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sbb_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val sbb_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sbb_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val sbb_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val sbbb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val sbbb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sbbl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val sbbl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sbbq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val sbbq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sbb_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val sbb_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val mul : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val mull_m : X86_types.Mem.t -> X86_types.Instruction.t
val mulq_m : X86_types.Mem.t -> X86_types.Instruction.t
val div : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val divl_m : X86_types.Mem.t -> X86_types.Instruction.t
val divq_m : X86_types.Mem.t -> X86_types.Instruction.t
val dec : View.w32 View.gpr -> X86_types.Instruction.t
val dec_m : X86_types.Mem.t -> X86_types.Instruction.t
val rcr : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rcrl_m : X86_types.Mem.t -> X86_types.Instruction.t
val rcrq_m : X86_types.Mem.t -> X86_types.Instruction.t
val rcr_cl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rcr_i : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rcr_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rcr_clrr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val rcr_irr : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val rcr_irr_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val rcrl_clm : X86_types.Mem.t -> X86_types.Instruction.t
val rcrq_clm : X86_types.Mem.t -> X86_types.Instruction.t
val rcrl_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val rcrl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val rcrq_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val rcrq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val rcr_clm : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rcr_im : int -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val rcr_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val shr : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shrl_m : X86_types.Mem.t -> X86_types.Instruction.t
val shrq_m : X86_types.Mem.t -> X86_types.Instruction.t
val shr_cl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shr_i : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shr_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shr_clrr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val shr_irr : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shr_irr_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shrl_clm : X86_types.Mem.t -> X86_types.Instruction.t
val shrq_clm : X86_types.Mem.t -> X86_types.Instruction.t
val shrl_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val shrl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val shrq_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val shrq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val shr_clm : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shr_im : int -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val shr_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val or_ : ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val or_rrr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val or_rm : [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val or_i : int64 -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val or_i_e : Asm_core.Expr.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t
val or_m : X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val or_rmr :
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val or_irr :
  int64 -> ([< `W32 | `W64 | `W8 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val or_irr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val or_mrr :
  X86_types.Mem.t ->
  ([< `W32 | `W64 | `W8 ] as 'a) View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val orb_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val orb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val orl_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val orl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val orq_im : int64 -> X86_types.Mem.t -> X86_types.Instruction.t
val orq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val or_im : int64 -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val or_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 | `W8 ] View.gpr -> X86_types.Instruction.t

val not : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val not_rr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val notl_m : X86_types.Mem.t -> X86_types.Instruction.t
val notq_m : X86_types.Mem.t -> X86_types.Instruction.t
val not_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ror : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val rorl_m : X86_types.Mem.t -> X86_types.Instruction.t
val rorq_m : X86_types.Mem.t -> X86_types.Instruction.t
val ror_cl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ror_i : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ror_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ror_clrr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val ror_irr : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val ror_irr_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val rorl_clm : X86_types.Mem.t -> X86_types.Instruction.t
val rorq_clm : X86_types.Mem.t -> X86_types.Instruction.t
val rorl_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val rorl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val rorq_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val rorq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val ror_clm : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ror_im : int -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val ror_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val shl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shll_m : X86_types.Mem.t -> X86_types.Instruction.t
val shlq_m : X86_types.Mem.t -> X86_types.Instruction.t
val shl_cl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shl_i : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shl_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shl_clrr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val shl_irr : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shl_irr_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shll_clm : X86_types.Mem.t -> X86_types.Instruction.t
val shlq_clm : X86_types.Mem.t -> X86_types.Instruction.t
val shll_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val shll_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val shlq_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val shlq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val shl_clm : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val shl_im : int -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val shl_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val sar : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val sarl_m : X86_types.Mem.t -> X86_types.Instruction.t
val sarq_m : X86_types.Mem.t -> X86_types.Instruction.t
val sar_cl : [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val sar_i : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val sar_i_e : Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val sar_clrr : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val sar_irr : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sar_irr_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val sarl_clm : X86_types.Mem.t -> X86_types.Instruction.t
val sarq_clm : X86_types.Mem.t -> X86_types.Instruction.t
val sarl_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val sarl_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sarq_im : int -> X86_types.Mem.t -> X86_types.Instruction.t
val sarq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> X86_types.Instruction.t
val sar_clm : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val sar_im : int -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val sar_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t

val shld_cl : ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val shld_i : int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shld_i_e :
  Asm_core.Expr.t -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shld_irrr :
  int -> ([< `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> 'a View.gpr -> X86_types.Instruction.t

val shld_irrr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) View.gpr ->
  'a View.gpr ->
  'a View.gpr ->
  X86_types.Instruction.t

val shld_clrm : [< `W32 | `W64 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t
val shld_irm : int -> [< `W32 | `W64 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t

val shld_irm_e :
  Asm_core.Expr.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Mem.t -> X86_types.Instruction.t

val shld_irmr :
  int ->
  ([< `W32 | `W64 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val shld_irmr_e :
  Asm_core.Expr.t ->
  ([< `W32 | `W64 ] as 'a) View.gpr ->
  X86_types.Mem.t ->
  'a View.gpr ->
  X86_types.Instruction.t

val addsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val addsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val subsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val subsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mulsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val mulsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val divsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val divsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val addss : View.xmm -> View.xmm -> X86_types.Instruction.t
val addss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val subss : View.xmm -> View.xmm -> X86_types.Instruction.t
val subss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mulss : View.xmm -> View.xmm -> X86_types.Instruction.t
val mulss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val divss : View.xmm -> View.xmm -> X86_types.Instruction.t
val divss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val comisd : View.xmm -> View.xmm -> X86_types.Instruction.t
val comisd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val ucomisd : View.xmm -> View.xmm -> X86_types.Instruction.t
val ucomisd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val comiss : View.xmm -> View.xmm -> X86_types.Instruction.t
val comiss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val ucomiss : View.xmm -> View.xmm -> X86_types.Instruction.t
val ucomiss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val xorpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val xorpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pxor_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pxor : View.xmm -> View.xmm -> X86_types.Instruction.t
val pxor_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pxor_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movapd : View.xmm -> View.xmm -> X86_types.Instruction.t
val movapd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movapd_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val cvtsd2ss : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtsd2ss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtss2sd : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtss2sd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtps2pd : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtps2pd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtpd2ps : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtpd2ps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtdq2ps : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtdq2ps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtps2dq : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvtps2dq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvttps2dq : View.xmm -> View.xmm -> X86_types.Instruction.t
val cvttps2dq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val movsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movsd_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movss : View.xmm -> View.xmm -> X86_types.Instruction.t
val movss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movss_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val cvtsi2sd : [< `W32 | `W64 ] View.gpr -> View.xmm -> X86_types.Instruction.t
val cvtsi2sd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvtsi2ss : [< `W32 | `W64 ] View.gpr -> View.xmm -> X86_types.Instruction.t
val cvtsi2ss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cvttsd2si_x : View.xmm -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val cvttsd2si_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val cvttsd2si_mr : X86_types.Mem.t -> View.w64 View.gpr -> X86_types.Instruction.t
val unpcklps : View.xmm -> View.xmm -> X86_types.Instruction.t
val unpcklps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val unpckhps : View.xmm -> View.xmm -> X86_types.Instruction.t
val unpckhps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val unpcklpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val unpcklpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val unpckhpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val unpckhpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpcklqdq : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpcklqdq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpckhqdq : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpckhqdq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpcklbw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpcklbw : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpcklbw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpcklbw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpckhbw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpckhbw : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpckhbw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpckhbw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpcklwd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpcklwd : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpcklwd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpcklwd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpckhwd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpckhwd : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpckhwd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpckhwd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpckldq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpckldq : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpckldq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpckldq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val punpckhdq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val punpckhdq : View.xmm -> View.xmm -> X86_types.Instruction.t
val punpckhdq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val punpckhdq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val paddb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val paddb : View.xmm -> View.xmm -> X86_types.Instruction.t
val paddb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val paddb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val paddw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val paddw : View.xmm -> View.xmm -> X86_types.Instruction.t
val paddw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val paddw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val paddd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val paddd : View.xmm -> View.xmm -> X86_types.Instruction.t
val paddd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val paddd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val paddq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val paddq : View.xmm -> View.xmm -> X86_types.Instruction.t
val paddq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val paddq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psubb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psubb : View.xmm -> View.xmm -> X86_types.Instruction.t
val psubb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psubb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psubw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psubw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psubw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psubw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psubd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psubd : View.xmm -> View.xmm -> X86_types.Instruction.t
val psubd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psubd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psubq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psubq : View.xmm -> View.xmm -> X86_types.Instruction.t
val psubq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psubq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpeqb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpeqb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpeqb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpeqb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpeqw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpeqw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpeqw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpeqw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpeqd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpeqd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpeqd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpeqd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpgtb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpgtb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpgtb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpgtb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpgtw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpgtw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpgtw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpgtw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpgtd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pcmpgtd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpgtd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pcmpgtd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val packsswb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val packsswb : View.xmm -> View.xmm -> X86_types.Instruction.t
val packsswb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val packsswb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val packssdw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val packssdw : View.xmm -> View.xmm -> X86_types.Instruction.t
val packssdw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val packssdw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val packuswb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val packuswb : View.xmm -> View.xmm -> X86_types.Instruction.t
val packuswb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val packuswb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pand_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pand : View.xmm -> View.xmm -> X86_types.Instruction.t
val pand_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pand_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pandn_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pandn : View.xmm -> View.xmm -> X86_types.Instruction.t
val pandn_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pandn_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val por_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val por : View.xmm -> View.xmm -> X86_types.Instruction.t
val por_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val por_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminub_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pminub : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminub_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pminub_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxub_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmaxub : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxub_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmaxub_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pminsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pminsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmaxsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmaxsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmullw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmullw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmullw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmullw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmulhw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmulhw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmulhw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmulhw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmulhuw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmulhuw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmulhuw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmulhuw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pavgb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pavgb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pavgb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pavgb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pavgw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pavgw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pavgw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pavgw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psadbw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psadbw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psadbw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psadbw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psllw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psllw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psllw_iq : int -> View.mmx -> X86_types.Instruction.t
val psllw_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psllw_i : int -> View.xmm -> X86_types.Instruction.t
val psllw_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psllw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psllw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pslld_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pslld : View.xmm -> View.xmm -> X86_types.Instruction.t
val pslld_iq : int -> View.mmx -> X86_types.Instruction.t
val pslld_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val pslld_i : int -> View.xmm -> X86_types.Instruction.t
val pslld_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val pslld_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pslld_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psllq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psllq : View.xmm -> View.xmm -> X86_types.Instruction.t
val psllq_iq : int -> View.mmx -> X86_types.Instruction.t
val psllq_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psllq_i : int -> View.xmm -> X86_types.Instruction.t
val psllq_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psllq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psllq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psrlw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psrlw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psrlw_iq : int -> View.mmx -> X86_types.Instruction.t
val psrlw_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psrlw_i : int -> View.xmm -> X86_types.Instruction.t
val psrlw_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psrlw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psrlw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psrld_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psrld : View.xmm -> View.xmm -> X86_types.Instruction.t
val psrld_iq : int -> View.mmx -> X86_types.Instruction.t
val psrld_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psrld_i : int -> View.xmm -> X86_types.Instruction.t
val psrld_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psrld_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psrld_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psrlq_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psrlq : View.xmm -> View.xmm -> X86_types.Instruction.t
val psrlq_iq : int -> View.mmx -> X86_types.Instruction.t
val psrlq_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psrlq_i : int -> View.xmm -> X86_types.Instruction.t
val psrlq_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psrlq_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psrlq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psraw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psraw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psraw_iq : int -> View.mmx -> X86_types.Instruction.t
val psraw_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psraw_i : int -> View.xmm -> X86_types.Instruction.t
val psraw_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psraw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psraw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psrad_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psrad : View.xmm -> View.xmm -> X86_types.Instruction.t
val psrad_iq : int -> View.mmx -> X86_types.Instruction.t
val psrad_iq_e : Asm_core.Expr.t -> View.mmx -> X86_types.Instruction.t
val psrad_i : int -> View.xmm -> X86_types.Instruction.t
val psrad_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psrad_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psrad_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pslldq_i : int -> View.xmm -> X86_types.Instruction.t
val pslldq_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val psrldq_i : int -> View.xmm -> X86_types.Instruction.t
val psrldq_i_e : Asm_core.Expr.t -> View.xmm -> X86_types.Instruction.t
val pshufb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pshufb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pshufb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pshufb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phaddw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phaddw : View.xmm -> View.xmm -> X86_types.Instruction.t
val phaddw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phaddw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phaddd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phaddd : View.xmm -> View.xmm -> X86_types.Instruction.t
val phaddd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phaddd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phsubw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phsubw : View.xmm -> View.xmm -> X86_types.Instruction.t
val phsubw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phsubw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phsubd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phsubd : View.xmm -> View.xmm -> X86_types.Instruction.t
val phsubd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phsubd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psignb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psignb : View.xmm -> View.xmm -> X86_types.Instruction.t
val psignb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psignb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psignw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psignw : View.xmm -> View.xmm -> X86_types.Instruction.t
val psignw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psignw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val psignd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val psignd : View.xmm -> View.xmm -> X86_types.Instruction.t
val psignd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val psignd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaddubsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmaddubsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaddubsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmaddubsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmulhrsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pmulhrsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmulhrsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pmulhrsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phaddsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phaddsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val phaddsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phaddsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phsubsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val phsubsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val phsubsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val phsubsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pabsb_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pabsb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pabsb_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pabsb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pabsw_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pabsw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pabsw_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pabsw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pabsd_qq : View.mmx -> View.mmx -> X86_types.Instruction.t
val pabsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pabsd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pabsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val palignr_iqq : int -> View.mmx -> View.mmx -> X86_types.Instruction.t
val palignr_iqq_e : Asm_core.Expr.t -> View.mmx -> View.mmx -> X86_types.Instruction.t
val palignr_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val palignr_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val palignr_imq : int -> X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val palignr_imq_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val palignr_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val palignr_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundpd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundpd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundpd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundpd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundss_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundss_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundss_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundss_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundsd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundsd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val roundsd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val roundsd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpeqq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpeqq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pcmpgtq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pcmpgtq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val packusdw : View.xmm -> View.xmm -> X86_types.Instruction.t
val packusdw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxsb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxsb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxud : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxud_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmaxuw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmaxuw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminsb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminsb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminud : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminud_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pminuw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pminuw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmuldq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmuldq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmulld : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmulld_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val phminposuw : View.xmm -> View.xmm -> X86_types.Instruction.t
val phminposuw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val ptest : View.xmm -> View.xmm -> X86_types.Instruction.t
val ptest_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxbw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxbw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxbd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxbd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxbq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxbq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxwd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxwd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxwq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxwq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovsxdq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovsxdq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxbw : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxbw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxbd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxbd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxbq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxbq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxwd : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxwd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxwq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxwq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pmovzxdq : View.xmm -> View.xmm -> X86_types.Instruction.t
val pmovzxdq_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movntdqa_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendvps : View.xmm -> View.xmm -> X86_types.Instruction.t
val blendvps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendvpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val blendvpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pblendvb : View.xmm -> View.xmm -> X86_types.Instruction.t
val pblendvb_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val blendps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val blendps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendpd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val blendpd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val blendpd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val blendpd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val dpps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val dpps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val dpps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val dpps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val dppd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val dppd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val dppd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val dppd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mpsadbw_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val mpsadbw_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val mpsadbw_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mpsadbw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pblendw_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pblendw_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pblendw_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pblendw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val insertps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val insertps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val insertps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val insertps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pinsrb_i : int -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrb_i_e : Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrb_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pinsrb_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pinsrd_i : int -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrd_i_e : Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pinsrd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pextrb_i : int -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrb_i_e : Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrb_irm : int -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val pextrb_irm_e : Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val pextrd_i : int -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrd_i_e : Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrd_irm : int -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val pextrd_irm_e : Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val extractps_i : int -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val extractps_i_e : Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val extractps_irm : int -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val extractps_irm_e : Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val vpshufb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshufb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddd :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddd_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddsw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphaddsw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpmaddubsw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaddubsw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubd :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubd_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubsw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vphsubsw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignb :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignb_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignd :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpsignd_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpmulhrsw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulhrsw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmuldq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmuldq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqq :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqq_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqq_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqq_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t
val vpackusdw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpackusdw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtq :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtq_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtq_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtq_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t
val vpminsb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminsb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminsd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminsd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminuw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminuw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminud : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminud_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxuw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxuw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxud : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxud_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulld : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulld_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpalignr_i : int -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpalignr_i_e :
  Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpalignr_im : int -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpalignr_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vblendps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendps_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendps_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendpd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendpd_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendpd_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vblendpd_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vpblendw_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vpblendw_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vpblendw_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vpblendw_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vroundss_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vroundss_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vroundss_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vroundss_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vroundsd_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vroundsd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vroundsd_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vroundsd_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vdpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vdpps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vdpps_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vdpps_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vdppd_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdppd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdppd_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vdppd_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vmpsadbw_i : int -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vmpsadbw_i_e :
  Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vmpsadbw_im : int -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vmpsadbw_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vinsertps_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vinsertps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vinsertps_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vinsertps_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vpabsb : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpabsb_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpabsw : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpabsw_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpabsd : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpabsd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vphminposuw : View.xmm -> View.xmm -> X86_types.Instruction.t
val vphminposuw_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vptest : ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t
val vptest_m : X86_types.Mem.t -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovsxbw : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovsxbw_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovsxbw_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxbd : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxbd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxbq : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxbq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxwd : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovsxwd_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovsxwd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxwq : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxwq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovsxdq : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovsxdq_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovsxdq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxbw : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovzxbw_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovzxbw_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxbd : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxbd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxbq : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxbq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxwd : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovzxwd_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovzxwd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxwq : View.xmm -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxwq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpmovzxdq : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vpmovzxdq_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vpmovzxdq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovntdqa_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val movdqa : View.xmm -> View.xmm -> X86_types.Instruction.t
val movdqa_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movdqa_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movdqu : View.xmm -> View.xmm -> X86_types.Instruction.t
val movdqu_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movdqu_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val andps : View.xmm -> View.xmm -> X86_types.Instruction.t
val andps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val andnps : View.xmm -> View.xmm -> X86_types.Instruction.t
val andnps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val orps : View.xmm -> View.xmm -> X86_types.Instruction.t
val orps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val xorps : View.xmm -> View.xmm -> X86_types.Instruction.t
val xorps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val andpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val andpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val andnpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val andnpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val orpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val orpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movaps : View.xmm -> View.xmm -> X86_types.Instruction.t
val movaps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movaps_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movups : View.xmm -> View.xmm -> X86_types.Instruction.t
val movups_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movups_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movupd : View.xmm -> View.xmm -> X86_types.Instruction.t
val movupd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movupd_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val addps : View.xmm -> View.xmm -> X86_types.Instruction.t
val addps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val subps : View.xmm -> View.xmm -> X86_types.Instruction.t
val subps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mulps : View.xmm -> View.xmm -> X86_types.Instruction.t
val mulps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val divps : View.xmm -> View.xmm -> X86_types.Instruction.t
val divps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val addpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val addpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val subpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val subpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val mulpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val mulpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val divpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val divpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val maxss : View.xmm -> View.xmm -> X86_types.Instruction.t
val maxss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val minss : View.xmm -> View.xmm -> X86_types.Instruction.t
val minss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val maxsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val maxsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val minsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val minsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val maxps : View.xmm -> View.xmm -> X86_types.Instruction.t
val maxps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val minps : View.xmm -> View.xmm -> X86_types.Instruction.t
val minps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val maxpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val maxpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val minpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val minpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val sqrtss : View.xmm -> View.xmm -> X86_types.Instruction.t
val sqrtss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val sqrtsd : View.xmm -> View.xmm -> X86_types.Instruction.t
val sqrtsd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val sqrtps : View.xmm -> View.xmm -> X86_types.Instruction.t
val sqrtps_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val sqrtpd : View.xmm -> View.xmm -> X86_types.Instruction.t
val sqrtpd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val shufps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val shufps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val shufps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val shufps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val shufpd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val shufpd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val shufpd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val shufpd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpss_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpss_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpss_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpss_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpsd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpsd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpsd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpsd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpps_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpps_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmpps_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmpps_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmppd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmppd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val cmppd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val cmppd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshufd_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshufd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshufd_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshufd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshuflw_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshuflw_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshuflw_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshuflw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshufhw_i : int -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshufhw_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val pshufhw_im : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pshufhw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movd_q : View.mmx -> View.w32 View.gpr -> X86_types.Instruction.t
val movd_rq : View.w32 View.gpr -> View.mmx -> X86_types.Instruction.t
val movd_rx : View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val movd_x : View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val movd_mq : X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val movd_mx : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val movd_qm : View.mmx -> X86_types.Mem.t -> X86_types.Instruction.t
val movd_xm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val vaddsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vaddsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsubsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsubsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmulsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmulsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdivsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdivsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vaddss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vaddss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsubss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsubss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmulss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmulss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdivss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vdivss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vaddps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vaddps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsubps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsubps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmulps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmulps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vdivps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vdivps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vaddpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vaddpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsubpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsubpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmulpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmulpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vdivpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vdivpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandnps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandnps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vorps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vorps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vxorps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vxorps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandnpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vandnpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vorpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vorpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vxorpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vxorpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpcklps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpcklps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpckhps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpckhps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpcklpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpcklpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpckhpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vunpckhpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklqdq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklqdq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhqdq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhqdq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklbw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklbw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhbw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhbw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklwd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpcklwd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhwd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhwd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckldq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckldq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhdq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpunpckhdq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpaddq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubq : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsubq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqb :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqb_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqb_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqb_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqw_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqw_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqd :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqd_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpeqd_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpeqd_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtb :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtb_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtb_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtb_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtw :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtw_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtw_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtw_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtd :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtd_rrk : 'a View.vec -> 'a View.vec -> View.kmask -> X86_types.Instruction.t

val vpcmpgtd_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpcmpgtd_mrk : X86_types.Mem.t -> 'a View.vec -> View.kmask -> X86_types.Instruction.t
val vpacksswb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpacksswb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpackssdw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpackssdw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpackuswb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpackuswb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpand :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpand_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpandn :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpandn_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpor :
  ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpor_m :
  X86_types.Mem.t -> ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t

val vpminub : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminub_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxub : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxub_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminsw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpminsw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmaxsw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmaxsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmaxsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vminsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vminsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmaxss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmaxss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vminss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vminss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vmaxps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmaxps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vminps : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vminps_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmaxpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmaxpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vminpd : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vminpd_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsqrtsd : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsqrtsd_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsqrtss : View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsqrtss_m : X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vsqrtps : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsqrtps_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vsqrtpd : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vsqrtpd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovaps : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovaps_rm : 'a View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovaps_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovups : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovups_rm : 'a View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovups_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovapd : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovapd_rm : 'a View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovapd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovupd : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovupd_rm : 'a View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovupd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vcomisd : View.xmm -> View.xmm -> X86_types.Instruction.t
val vcomisd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vucomisd : View.xmm -> View.xmm -> X86_types.Instruction.t
val vucomisd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vcomiss : View.xmm -> View.xmm -> X86_types.Instruction.t
val vcomiss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vucomiss : View.xmm -> View.xmm -> X86_types.Instruction.t
val vucomiss_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vcvtps2pd : View.xmm -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vcvtps2pd_rr : View.ymm -> View.zmm -> X86_types.Instruction.t
val vcvtps2pd_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vcvtpd2ps : View.xmm -> View.xmm -> X86_types.Instruction.t
val vcvtpd2ps_rr : View.ymm -> View.xmm -> X86_types.Instruction.t
val vcvtpd2ps_zy : View.zmm -> View.ymm -> X86_types.Instruction.t
val vcvtpd2ps_m : X86_types.Mem.t -> View.ymm -> X86_types.Instruction.t
val vcvtdq2ps : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vcvtdq2ps_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vcvtps2dq : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vcvtps2dq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vcvttps2dq : 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vcvttps2dq_m : X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vshufps_i : int -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufps_i_e :
  Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufps_im : int -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufps_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufpd_i : int -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufpd_i_e :
  Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufpd_im : int -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vshufpd_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t

val vcmpss_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vcmpss_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vcmpss_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vcmpss_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vcmpsd_i : int -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vcmpsd_i_e : Asm_core.Expr.t -> View.xmm -> View.xmm -> View.xmm -> X86_types.Instruction.t
val vcmpsd_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vcmpsd_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vcmpps_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmpps_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmpps_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmpps_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmppd_i :
  int ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmppd_i_e :
  Asm_core.Expr.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmppd_im :
  int ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vcmppd_im_e :
  Asm_core.Expr.t ->
  X86_types.Mem.t ->
  ([< `L128 | `L256 ] as 'a) View.vec ->
  'a View.vec ->
  X86_types.Instruction.t

val vpshufd_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshufd_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshufd_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpshufd_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpshuflw_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshuflw_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshuflw_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpshuflw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpshufhw_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshufhw_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpshufhw_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpshufhw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovd : View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val vmovd_rr : View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val vmovd_xx : View.xmm -> View.xmm -> X86_types.Instruction.t
val vmovd_m : X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val vmovd_rm : View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val vpmullw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmullw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulhw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulhw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulhuw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpmulhuw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpavgb : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpavgb_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpavgw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpavgw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsadbw : 'a View.vec -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsadbw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllw : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllw_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllw_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllw_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsllw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpslld : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslld_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslld_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslld_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslld_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpslld_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsllq : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllq_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllq_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsllq_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsllq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrlw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrld : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrld_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrld_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrld_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrld_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrld_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrlq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsraw : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsraw_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsraw_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsraw_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsraw_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsraw_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrad : View.xmm -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrad_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrad_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrad_m : X86_types.Mem.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrad_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrad_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpslldq_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslldq_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpslldq_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpslldq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrldq_i : int -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrldq_i_e : Asm_core.Expr.t -> 'a View.vec -> 'a View.vec -> X86_types.Instruction.t
val vpsrldq_im : int -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vpsrldq_im_e : Asm_core.Expr.t -> X86_types.Mem.t -> 'a View.vec -> X86_types.Instruction.t
val vmovdqa : ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovdqa_rm : [< `L128 | `L256 ] View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovdqa_m : X86_types.Mem.t -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val vmovdqu : ([< `L128 | `L256 ] as 'a) View.vec -> 'a View.vec -> X86_types.Instruction.t
val vmovdqu_rm : [< `L128 | `L256 ] View.vec -> X86_types.Mem.t -> X86_types.Instruction.t
val vmovdqu_m : X86_types.Mem.t -> [< `L128 | `L256 ] View.vec -> X86_types.Instruction.t
val pinsrw_irq : int -> View.w32 View.gpr -> View.mmx -> X86_types.Instruction.t
val pinsrw_irq_e : Asm_core.Expr.t -> View.w32 View.gpr -> View.mmx -> X86_types.Instruction.t
val pinsrw_irx : int -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrw_irx_e : Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> X86_types.Instruction.t
val pinsrw_imq : int -> X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pinsrw_imq_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.mmx -> X86_types.Instruction.t
val pinsrw_imx : int -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pinsrw_imx_e : Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> X86_types.Instruction.t
val pextrw_iq : int -> View.mmx -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrw_iq_e : Asm_core.Expr.t -> View.mmx -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrw_i : int -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrw_i_e : Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pextrw_irm : int -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val pextrw_irm_e : Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val vpinsrw_i : int -> View.w32 View.gpr -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vpinsrw_i_e :
  Asm_core.Expr.t -> View.w32 View.gpr -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vpinsrw_im : int -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vpinsrw_im_e :
  Asm_core.Expr.t -> X86_types.Mem.t -> View.xmm -> View.xmm -> X86_types.Instruction.t

val vpextrw_i : int -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val vpextrw_i_e : Asm_core.Expr.t -> View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val vpextrw_irm : int -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val vpextrw_irm_e : Asm_core.Expr.t -> View.xmm -> X86_types.Mem.t -> X86_types.Instruction.t
val movmskps : View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val movmskpd : View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val pmovmskb_q : View.mmx -> View.w32 View.gpr -> X86_types.Instruction.t
val pmovmskb_x : View.xmm -> View.w32 View.gpr -> X86_types.Instruction.t
val vmovmskps : [< `L128 | `L256 ] View.vec -> View.w32 View.gpr -> X86_types.Instruction.t
val vmovmskpd : [< `L128 | `L256 ] View.vec -> View.w32 View.gpr -> X86_types.Instruction.t
val vpmovmskb : [< `L128 | `L256 ] View.vec -> View.w32 View.gpr -> X86_types.Instruction.t
val fldl_m : X86_types.Mem.t -> X86_types.Instruction.t
val fstpl_m : X86_types.Mem.t -> X86_types.Instruction.t
val fstps_m : X86_types.Mem.t -> X86_types.Instruction.t
val flds : Asm_core.Expr.t -> X86_types.Instruction.t
val flds_m : X86_types.Mem.t -> X86_types.Instruction.t
val fildll_m : X86_types.Mem.t -> X86_types.Instruction.t
val fadds : Asm_core.Expr.t -> X86_types.Instruction.t
val fadds_m : X86_types.Mem.t -> X86_types.Instruction.t
val fadd : View.st -> X86_types.Instruction.t
val fadd_r : View.st -> X86_types.Instruction.t
val fnstcw_m : X86_types.Mem.t -> X86_types.Instruction.t
val fldcw_m : X86_types.Mem.t -> X86_types.Instruction.t
val fistpll_m : X86_types.Mem.t -> X86_types.Instruction.t
val fsubs : Asm_core.Expr.t -> X86_types.Instruction.t
val fsubs_m : X86_types.Mem.t -> X86_types.Instruction.t
val fnstsw : X86_types.Instruction.t
val fnstsw_m : X86_types.Mem.t -> X86_types.Instruction.t
val sahf : X86_types.Instruction.t
val cmovo : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovo_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jo_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jo_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val seto : View.w8 View.gpr -> X86_types.Instruction.t
val seto_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovno : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovno_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jno_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jno_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setno : View.w8 View.gpr -> X86_types.Instruction.t
val setno_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovb : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovb_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jb_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jb_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setb : View.w8 View.gpr -> X86_types.Instruction.t
val setb_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovae : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovae_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jae_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jae_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setae : View.w8 View.gpr -> X86_types.Instruction.t
val setae_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmove : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmove_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val je_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val je_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val sete : View.w8 View.gpr -> X86_types.Instruction.t
val sete_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovne : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovne_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jne_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jne_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setne : View.w8 View.gpr -> X86_types.Instruction.t
val setne_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovbe : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovbe_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jbe_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jbe_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setbe : View.w8 View.gpr -> X86_types.Instruction.t
val setbe_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmova : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmova_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val ja_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val ja_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val seta : View.w8 View.gpr -> X86_types.Instruction.t
val seta_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovs : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovs_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val js_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val js_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val sets : View.w8 View.gpr -> X86_types.Instruction.t
val sets_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovns : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovns_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jns_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jns_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setns : View.w8 View.gpr -> X86_types.Instruction.t
val setns_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovp : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovp_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jp_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jp_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setp : View.w8 View.gpr -> X86_types.Instruction.t
val setp_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovnp : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovnp_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jnp_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jnp_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setnp : View.w8 View.gpr -> X86_types.Instruction.t
val setnp_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovl : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovl_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jl_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jl_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setl : View.w8 View.gpr -> X86_types.Instruction.t
val setl_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovge : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovge_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jge_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jge_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setge : View.w8 View.gpr -> X86_types.Instruction.t
val setge_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovle : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovle_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jle_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jle_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setle : View.w8 View.gpr -> X86_types.Instruction.t
val setle_m : X86_types.Mem.t -> X86_types.Instruction.t
val cmovg : ([< `W16 | `W32 | `W64 ] as 'a) View.gpr -> 'a View.gpr -> X86_types.Instruction.t
val cmovg_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val jg_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jg_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val setg : View.w8 View.gpr -> X86_types.Instruction.t
val setg_m : X86_types.Mem.t -> X86_types.Instruction.t
val loop_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val loop_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val loope_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val loope_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val loopne_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val loopne_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val jcxz : Asm_core.Expr.t -> X86_types.Instruction.t
val jecxz_m32 : Asm_core.Expr.t -> X86_types.Instruction.t
val jecxz_m64 : Asm_core.Expr.t -> X86_types.Instruction.t
val jrcxz : Asm_core.Expr.t -> X86_types.Instruction.t
val movzb : View.w8 View.gpr -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movzb_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movzw : View.w16 View.gpr -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movzw_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movsb : View.w8 View.gpr -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movsb_m : X86_types.Mem.t -> [< `W16 | `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movsw : View.w16 View.gpr -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movsw_m : X86_types.Mem.t -> [< `W32 | `W64 ] View.gpr -> X86_types.Instruction.t
val movsl : View.w32 View.gpr -> View.w64 View.gpr -> X86_types.Instruction.t
val movsl_m : X86_types.Mem.t -> View.w64 View.gpr -> X86_types.Instruction.t
