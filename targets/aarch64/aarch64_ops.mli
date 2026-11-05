(* One definition per hand-written AArch64 (A64) opcode: its spelling, the fields of the encoding
   classes it belongs to, whether its lowering stays code, and the operand lists it takes. The
   same operand lists are the typed builders ({!Builders}): one per operand shape, named by the
   mnemonic with a suffix for the shape ([add x0 x1 x2], [add_i sp sp 16],
   [add_sh x0 x1 x2 (lsl_ 3)], [ldr_post x0 sp 8]). Register positions are typed by what they
   accept: a width ([x] or [w], the same for every operand that shares it), and whether register
   31 is the stack pointer or the zero register there ({!Aarch64_types.View}). A scalar
   floating-point operand is a [d] or an [s] register and a vector one carries its arrangement, so
   [add x0 w1 x2], [fadd d0 s1 d2] and [fadd_v v0.4s v1.2d v2.4s] do not compile. *)

module Kind : sig
  type t = X | W | D | S | Q | Imm | Fimm | Mem | Sym | Sh | Vec | Vlane | Vlist | Vlist_lane

  val name : t -> string
end

module Sx : sig
  type sop = private Aarch64_types.Shift.t
  type eop = private Aarch64_types.Shift.t

  val by_lsl : int -> sop
  val by_lsr : int -> sop
  val by_asr : int -> sop
  val by_ror : int -> sop
  val extend : Aarch64_types.Extend.t -> int -> eop
end

type sample = {
  width : int;
  double : bool;
  arr : Aarch64_types.Varr.t;
  lane : Aarch64_types.Lane.t;
  imm : int;
}

module Shape : sig
  type ('w, 's, 'a, 'f, 'r) t =
    | Done : ('w, 's, 'a, 'r, 'r) t
    | Unit : ('w, 's, 'a, unit -> 'r, 'r) t
    | Z :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, ('w, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Sp :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, ('w, [< `Gp | `Sp ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Any :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, ('w, [< `Gp | `Sp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Xz :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Xsp :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Wz :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Zv :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, ('v, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Zw :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, ('v, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'f, 'r) t
    | Fr : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 's Aarch64_types.View.freg -> 'f, 'r) t
    | D :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, Aarch64_types.View.d Aarch64_types.View.freg -> 'f, 'r) t
    | S :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, 'a, Aarch64_types.View.s Aarch64_types.View.freg -> 'f, 'r) t
    | Q : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.View.qreg -> 'f, 'r) t
    | Vfp :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, ([< `D2 | `S2 | `S4 ] as 'a), 'a Aarch64_types.View.vec -> 'f, 'r) t
    | Vb :
        ('w, 's, 'a, 'f, 'r) t
        -> ('w, 's, ([< `B16 | `B8 ] as 'a), 'a Aarch64_types.View.vec -> 'f, 'r) t
    | Vd2 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, [ `D2 ] Aarch64_types.View.vec -> 'f, 'r) t
    | Vs2 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, [ `S2 ] Aarch64_types.View.vec -> 'f, 'r) t
    | Lane : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.View.lane -> 'f, 'r) t
    | Vlist : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.View.vlist -> 'f, 'r) t
    | Vlist_lane : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.View.vlist_lane -> 'f, 'r) t
    | Imm : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int -> 'f, 'r) t
    | Imm64 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int64 -> 'f, 'r) t
    | Lsl_amt : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int -> 'f, 'r) t
    | Lsl12 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Fimm : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, float -> 'f, 'r) t
    | Fzero : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Cond : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.Cond.t -> 'f, 'r) t
    | Fix_sym : string * ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Sym : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Asm_core.Expr.t -> 'f, 'r) t
    | Lo12 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Asm_core.Expr.t -> 'f, 'r) t
    | Mem : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Aarch64_types.Mem.t -> 'f, 'r) t
    | Post :
        ('w, 's, 'a, 'f, 'r) t
        -> ( 'w,
             's,
             'a,
             (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> 'f,
             'r )
           t
    | Sop : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Sx.sop -> 'f, 'r) t
    | Eop : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Sx.eop -> 'f, 'r) t

  type packed = P : ('w, 's, 'a, 'f, Aarch64_types.Instruction.t) t -> packed

  type node = {
    reads_w : bool;
    reads_d : bool;
    arrs : Aarch64_types.Varr.t list option;
    lanes : bool;
    kinds : sample -> Kind.t list;
    next : packed option;
  }

  val leaf : node
  val reg : sample -> Kind.t

  val step :
    ?w:bool ->
    ?d:bool ->
    ?arrs:Aarch64_types.Varr.t list ->
    ?lanes:bool ->
    (sample -> Kind.t list) ->
    ('a, 'b, 'c, 'd, Aarch64_types.Instruction.t) t ->
    node

  val one : 'a -> 'b -> 'a list
  val node : ('w, 's, 'a, 'f, Aarch64_types.Instruction.t) t -> node

  val fold :
    packed -> bool * bool * Aarch64_types.Varr.t list option * bool * (sample -> Kind.t list)

  val int : int -> Aarch64_types.Operand.t
  val reg_op : ('a, 'b) Aarch64_types.View.greg -> Aarch64_types.Operand.t
  val mem0 : (Aarch64_types.View.x, 'a) Aarch64_types.View.greg -> Aarch64_types.Operand.t
  val vec_op : 'a Aarch64_types.View.vec -> Aarch64_types.Operand.t
  val build : ('w, 's, 'a, 'f, 'r) t -> (Aarch64_types.Operand.t list -> 'r) -> 'f
  val sample : sample -> ('w, 's, 'a, 'f, 'r) t -> 'f -> 'r
end

module Enc : sig
  type t =
    | Addsub of { sub : bool; s : bool }
    | Mov
    | Wide of { opc : int }
    | Cbz of { nz : bool }
    | Sxtw
    | Uxtw
    | Div of { signed : bool }
    | Madd of { sub : bool }
    | Mul
    | Cset
    | Csel
    | Shiftv of { code : int }
    | Shift_imm
    | Bitfield of { signed : bool; lsl_alias : bool }
    | Dp1 of { w32 : int option; w64 : int option }
    | Ldst_pair of { load : bool }
    | Ldst of { load : bool; size : int option }
    | Ldst_f of { load : bool }
    | Ldst_q of { load : bool }
    | Ldst_sx of { size : int }
    | Ret
    | Br
    | Mrs
    | Msr
    | Logical of { opc : int; n : bool }
    | Adrp
    | Adr
    | B
    | Bcond
    | Udf
    | Bl
    | Fmov
    | Fcmp
    | Fcsel
    | Fbinop of { opc : int }
    | Fp1 of { opcode : int }
    | Vbin of { u : int; a : int; opc : int }
    | Vun of { u : int; a : int; opc : int }
    | Vlogic of { u : int; sel : int }
    | Vnot
    | Cvtf of { signed : bool }
    | Fmadd
    | Fcvt
    | Fcvtz of { signed : bool }
    | Vcvt of { widen : bool }
    | Dup
    | Ins
    | Vld1 of { load : bool }
    | Vld1r
end

type variant = { kinds : Kind.t list; sample : imm:int -> Aarch64_types.Instruction.t }

type entry = {
  op : Aarch64_types.Opcode.t;
  name : string;
  enc : Enc.t list;
  ext : string list;
  lower : bool;
  mutable shapes : variant list list;
  also : Kind.t list list;
}

val registry : entry list ref
val variants : Aarch64_encode__Aarch64_types.Opcode.t -> Shape.packed -> variant list

val entry :
  ?ext:string list ->
  ?lower:bool ->
  ?also:Kind.t list list ->
  name:string ->
  enc:Enc.t list ->
  Aarch64_types.Opcode.t ->
  Shape.packed list ->
  unit

val bld :
  Aarch64_encode__Aarch64_types.Opcode.t ->
  ('a, 'b, 'c, 'd, Aarch64_types.Instruction.t) Shape.t ->
  'd

module Builders : sig
  module Sh : sig
    val zz :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val zzz :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val zzzz :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val zz_sh :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Sx.sop ->
        'd,
        'd )
      Shape.t

    val zzz_sh :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Sx.sop ->
        'd,
        'd )
      Shape.t

    val zzz_cond :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Aarch64_types.Cond.t ->
        'd,
        'd )
      Shape.t

    val z_cond :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Cond.t -> 'd,
        'd )
      Shape.t

    val spsp_ext :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('d, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Sx.eop ->
        'e,
        'e )
      Shape.t

    val spsp_i :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val spsp_i12 :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val spsp_lo12 :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        Asm_core.Expr.t ->
        'd,
        'd )
      Shape.t

    val sp_i : ('a, 'b, 'c, ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> 'd, 'd) Shape.t
    val sp_i12 : ('a, 'b, 'c, ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> 'd, 'd) Shape.t

    val anyany :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Sp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val z_i64 :
      ('a, 'b, 'c, ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int64 -> 'd, 'd) Shape.t

    val z_i : ('a, 'b, 'c, ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> 'd, 'd) Shape.t

    val z_i_lsl :
      ('a, 'b, 'c, ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> int -> 'd, 'd) Shape.t

    val z_sym :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Asm_core.Expr.t -> 'd,
        'd )
      Shape.t

    val zz_i :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val zzii :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        int ->
        int ->
        'd,
        'd )
      Shape.t

    val spz_i64 :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        int64 ->
        'd,
        'd )
      Shape.t

    val z_mem :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Mem.t -> 'd,
        'd )
      Shape.t

    val z_post :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val wz_mem :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Mem.t -> 'd,
        'd )
      Shape.t

    val wz_post :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val xz_mem :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Mem.t -> 'd,
        'd )
      Shape.t

    val zz_mem :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Aarch64_types.Mem.t ->
        'd,
        'd )
      Shape.t

    val zz_post :
      ( 'a,
        'b,
        'c,
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
        int ->
        'd,
        'd )
      Shape.t

    val fr_mem : ('a, 'b, 'c, 'b Aarch64_types.View.freg -> Aarch64_types.Mem.t -> 'd, 'd) Shape.t
    val q_mem : ('a, 'b, 'c, Aarch64_types.View.qreg -> Aarch64_types.Mem.t -> 'd, 'd) Shape.t

    val frfr :
      ('a, 'b, 'c, 'b Aarch64_types.View.freg -> 'b Aarch64_types.View.freg -> 'd, 'd) Shape.t

    val frfrfr :
      ( 'a,
        'b,
        'c,
        'b Aarch64_types.View.freg -> 'b Aarch64_types.View.freg -> 'b Aarch64_types.View.freg -> 'd,
        'd )
      Shape.t

    val frfrfrfr :
      ( 'a,
        'b,
        'c,
        'b Aarch64_types.View.freg ->
        'b Aarch64_types.View.freg ->
        'b Aarch64_types.View.freg ->
        'b Aarch64_types.View.freg ->
        'd,
        'd )
      Shape.t

    val frfrfr_cond :
      ( 'a,
        'b,
        'c,
        'b Aarch64_types.View.freg ->
        'b Aarch64_types.View.freg ->
        'b Aarch64_types.View.freg ->
        Aarch64_types.Cond.t ->
        'd,
        'd )
      Shape.t

    val fr_fimm : ('a, 'b, 'c, 'b Aarch64_types.View.freg -> float -> 'd, 'd) Shape.t
    val fr_fzero : ('a, 'b, 'c, 'b Aarch64_types.View.freg -> 'd, 'd) Shape.t

    val fr_zv :
      ( 'a,
        'b,
        'c,
        'b Aarch64_types.View.freg -> ('d, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'e,
        'e )
      Shape.t

    val zv_fr :
      ( 'a,
        'b,
        'c,
        ('d, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'b Aarch64_types.View.freg -> 'e,
        'e )
      Shape.t

    val vvv_fp :
      ( 'a,
        'b,
        ([< `D2 | `S2 | `S4 ] as 'c),
        'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val vv_fp :
      ( 'a,
        'b,
        ([< `D2 | `S2 | `S4 ] as 'c),
        'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val vvv_b :
      ( 'a,
        'b,
        ([< `B16 | `B8 ] as 'c),
        'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val vv_b :
      ( 'a,
        'b,
        ([< `B16 | `B8 ] as 'c),
        'c Aarch64_types.View.vec -> 'c Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val vfp_lane :
      ( 'a,
        'b,
        ([< `D2 | `S2 | `S4 ] as 'a0),
        'a0 Aarch64_types.View.vec -> Aarch64_types.View.lane -> 'c,
        'c )
      Shape.t

    val vfp_zv :
      ( 'a,
        'b,
        ([< `D2 | `S2 | `S4 ] as 'a0),
        'a0 Aarch64_types.View.vec -> ('c, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'd,
        'd )
      Shape.t

    val fr_lane :
      ('a, 'b, 'c, 'b Aarch64_types.View.freg -> Aarch64_types.View.lane -> 'd, 'd) Shape.t

    val lane_lane :
      ('a, 'b, 'c, Aarch64_types.View.lane -> Aarch64_types.View.lane -> 'd, 'd) Shape.t

    val vl_mem :
      ('a, 'b, 'c, Aarch64_types.View.vlist_lane -> Aarch64_types.Mem.t -> 'd, 'd) Shape.t

    val vlist_mem : ('a, 'b, 'c, Aarch64_types.View.vlist -> Aarch64_types.Mem.t -> 'd, 'd) Shape.t

    val xz_sym :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Asm_core.Expr.t -> 'd,
        'd )
      Shape.t

    val xz_xz :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val xz_wz :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val xz :
      ('a, 'b, 'c, (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'd, 'd) Shape.t

    val sym : ('a, 'b, 'c, Asm_core.Expr.t -> 'd, 'd) Shape.t
    val imm : ('a, 'b, 'c, int -> 'd, 'd) Shape.t

    val d_xz :
      ( 'a,
        'b,
        'c,
        Aarch64_types.View.d Aarch64_types.View.freg ->
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val s_wz :
      ( 'a,
        'b,
        'c,
        Aarch64_types.View.s Aarch64_types.View.freg ->
        (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        'd,
        'd )
      Shape.t

    val xz_d :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Aarch64_types.View.d Aarch64_types.View.freg ->
        'd,
        'd )
      Shape.t

    val wz_s :
      ( 'a,
        'b,
        'c,
        (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
        Aarch64_types.View.s Aarch64_types.View.freg ->
        'd,
        'd )
      Shape.t

    val s_d :
      ( 'a,
        'b,
        'c,
        Aarch64_types.View.s Aarch64_types.View.freg ->
        Aarch64_types.View.d Aarch64_types.View.freg ->
        'd,
        'd )
      Shape.t

    val d_s :
      ( 'a,
        'b,
        'c,
        Aarch64_types.View.d Aarch64_types.View.freg ->
        Aarch64_types.View.s Aarch64_types.View.freg ->
        'd,
        'd )
      Shape.t

    val vd2_vs2 :
      ( 'a,
        'b,
        'c,
        [ `D2 ] Aarch64_types.View.vec -> [ `S2 ] Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val vs2_vd2 :
      ( 'a,
        'b,
        'c,
        [ `S2 ] Aarch64_types.View.vec -> [ `D2 ] Aarch64_types.View.vec -> 'd,
        'd )
      Shape.t

    val xz_sysreg :
      string ->
      ('a, 'b, 'c, (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'd, 'd) Shape.t

    val sysreg_xz :
      string ->
      ('a, 'b, 'c, (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> 'd, 'd) Shape.t
  end

  val add :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val add_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val add_ext :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('b, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.eop ->
    Aarch64_types.Instruction.t

  val add_i :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val add_i_lsl12 :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val add_lo12 :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    Asm_core.Expr.t ->
    Aarch64_types.Instruction.t

  val sub :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val sub_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val sub_ext :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('b, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.eop ->
    Aarch64_types.Instruction.t

  val sub_i :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val sub_i_lsl12 :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val cmp :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val cmp_i : ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val cmp_i_lsl12 :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val cmn_i : ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val cmn_i_lsl12 :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val mov :
    ('a, [< `Gp | `Sp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Sp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val mov_i : ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int64 -> Aarch64_types.Instruction.t
  val movz : ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val movz_lsl :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> int -> Aarch64_types.Instruction.t

  val movn : ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val movn_lsl :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> int -> Aarch64_types.Instruction.t

  val movk : ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> Aarch64_types.Instruction.t

  val movk_lsl :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int -> int -> Aarch64_types.Instruction.t

  val sxtw :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val uxtw :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val udiv :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val sdiv :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val madd :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val msub :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val mul :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val cset :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Cond.t ->
    Aarch64_types.Instruction.t

  val csel :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Cond.t ->
    Aarch64_types.Instruction.t

  val lsl_ :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val lsl_i :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val lsr_ :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val lsr_i :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val asr_ :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val asr_i :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val ror_ :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val ubfx :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    int ->
    Aarch64_types.Instruction.t

  val ubfiz :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    int ->
    Aarch64_types.Instruction.t

  val sbfx :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    int ->
    Aarch64_types.Instruction.t

  val sbfiz :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int ->
    int ->
    Aarch64_types.Instruction.t

  val rbit :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val rev16 :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val rev32 :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val rev :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val clz :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val cls :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val and_ :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val and_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val and_i :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int64 ->
    Aarch64_types.Instruction.t

  val orr :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val orr_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val orr_i :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int64 ->
    Aarch64_types.Instruction.t

  val eor :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val eor_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val eor_i :
    ('a, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    int64 ->
    Aarch64_types.Instruction.t

  val bic :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val bic_sh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Sx.sop ->
    Aarch64_types.Instruction.t

  val tst_i : ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> int64 -> Aarch64_types.Instruction.t

  val ldr :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldr_post :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val ldr_f : 'a Aarch64_types.View.freg -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t
  val ldr_q : Aarch64_types.View.qreg -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t

  val str :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val str_post :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val str_f : 'a Aarch64_types.View.freg -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t
  val str_q : Aarch64_types.View.qreg -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t

  val ldrb :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldrb_post :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val ldrh :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldrh_post :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val strb :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val strb_post :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val strh :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val strh_post :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val ldrsb :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldrsh :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldrsw :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldp :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val ldp_post :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val stp :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Mem.t ->
    Aarch64_types.Instruction.t

  val stp_post :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    (Aarch64_types.View.x, [< `Gp | `Sp ]) Aarch64_types.View.greg ->
    int ->
    Aarch64_types.Instruction.t

  val b : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val bl : Asm_core.Expr.t -> Aarch64_types.Instruction.t

  val b_cond :
    Aarch64_encode__Aarch64_types.Cond.t -> Asm_core.Expr.t -> Aarch64_types.Instruction.t

  val ret : unit -> Aarch64_types.Instruction.t

  val ret_r :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val br :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val cbz :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Asm_core.Expr.t -> Aarch64_types.Instruction.t

  val cbnz :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Asm_core.Expr.t -> Aarch64_types.Instruction.t

  val adrp :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Asm_core.Expr.t ->
    Aarch64_types.Instruction.t

  val adr :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Asm_core.Expr.t ->
    Aarch64_types.Instruction.t

  val udf : int -> Aarch64_types.Instruction.t

  val mrs_fpcr :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val mrs_nzcv :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val msr_fpcr :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val msr_nzcv :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg -> Aarch64_types.Instruction.t

  val fmov : 'a Aarch64_types.View.freg -> 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t
  val fmov_i : 'a Aarch64_types.View.freg -> float -> Aarch64_types.Instruction.t

  val fmov_d_x :
    Aarch64_types.View.d Aarch64_types.View.freg ->
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val fmov_s_w :
    Aarch64_types.View.s Aarch64_types.View.freg ->
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val fmov_x_d :
    (Aarch64_types.View.x, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.View.d Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fmov_w_s :
    (Aarch64_types.View.w, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.View.s Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcmp : 'a Aarch64_types.View.freg -> 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t
  val fcmp_z : 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t

  val fadd :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fsub :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fmul :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fdiv :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fmax :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fadd_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fsub_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fmul_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fdiv_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fmax_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fmla_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fcmeq_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fcmge_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fcmgt_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fcsel :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Cond.t ->
    Aarch64_types.Instruction.t

  val scvtf :
    'a Aarch64_types.View.freg ->
    ('b, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val ucvtf :
    'a Aarch64_types.View.freg ->
    ('b, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val fneg : 'a Aarch64_types.View.freg -> 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t

  val fsqrt :
    'a Aarch64_types.View.freg -> 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t

  val frintz :
    'a Aarch64_types.View.freg -> 'a Aarch64_types.View.freg -> Aarch64_types.Instruction.t

  val fneg_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fsqrt_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val frintz_v :
    ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val fmadd :
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    'a Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcvt_s_d :
    Aarch64_types.View.s Aarch64_types.View.freg ->
    Aarch64_types.View.d Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcvt_d_s :
    Aarch64_types.View.d Aarch64_types.View.freg ->
    Aarch64_types.View.s Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcvtzs :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    'b Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcvtzu :
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    'b Aarch64_types.View.freg ->
    Aarch64_types.Instruction.t

  val fcvtl :
    [ `D2 ] Aarch64_types.View.vec -> [ `S2 ] Aarch64_types.View.vec -> Aarch64_types.Instruction.t

  val fcvtn :
    [ `S2 ] Aarch64_types.View.vec -> [ `D2 ] Aarch64_types.View.vec -> Aarch64_types.Instruction.t

  val and_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val orr_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val eor_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val bsl_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val bit_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val not_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val mov_v :
    ([< `B16 | `B8 ] as 'a) Aarch64_types.View.vec ->
    'a Aarch64_types.View.vec ->
    Aarch64_types.Instruction.t

  val mov_lane :
    'a Aarch64_types.View.freg -> Aarch64_types.View.lane -> Aarch64_types.Instruction.t

  val mov_lane_lane :
    Aarch64_types.View.lane -> Aarch64_types.View.lane -> Aarch64_types.Instruction.t

  val ins : Aarch64_types.View.lane -> Aarch64_types.View.lane -> Aarch64_types.Instruction.t

  val dup_lane :
    [< `D2 | `S2 | `S4 ] Aarch64_types.View.vec ->
    Aarch64_types.View.lane ->
    Aarch64_types.Instruction.t

  val dup_gpr :
    [< `D2 | `S2 | `S4 ] Aarch64_types.View.vec ->
    ('a, [< `Gp | `Zr ]) Aarch64_types.View.greg ->
    Aarch64_types.Instruction.t

  val dup_scalar :
    'a Aarch64_types.View.freg -> Aarch64_types.View.lane -> Aarch64_types.Instruction.t

  val ld1_lane : Aarch64_types.View.vlist_lane -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t
  val st1_lane : Aarch64_types.View.vlist_lane -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t
  val ld1r : Aarch64_types.View.vlist -> Aarch64_types.Mem.t -> Aarch64_types.Instruction.t
  val b_eq : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_ne : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_cs : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_cc : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_mi : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_pl : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_vs : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_vc : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_hi : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_ls : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_ge : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_lt : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_gt : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_le : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_al : Asm_core.Expr.t -> Aarch64_types.Instruction.t
  val b_nv : Asm_core.Expr.t -> Aarch64_types.Instruction.t
end

val all : entry list
val find_table : (Aarch64_types.Opcode.t, entry) Hashtbl.t
val find : Aarch64_types.Opcode.t -> entry
val find_opt : Aarch64_types.Opcode.t -> entry option
val opcodes : Aarch64_types.Opcode.t list
val name : Aarch64_types.Opcode.t -> string
val spellings : (string, Aarch64_types.Opcode.t) Hashtbl.t
val of_mnemonic : string -> Aarch64_types.Opcode.t option
val enc_find : Aarch64_types.Opcode.t -> (Enc.t -> 'a option) -> 'a option
val name_where : (Enc.t -> bool) -> string option
val shift_name : int -> string
val logical_name : int -> string
val fbinop_opcode : Aarch64_types.Opcode.t -> int option
val fbinop_name : int -> string
val dp1_opcode : Aarch64_types.Opcode.t -> width:int -> int option
val vbin_bits : Aarch64_types.Opcode.t -> int * int * int
val vun_bits : Aarch64_types.Opcode.t -> int * int * int
val vlogic_bits : Aarch64_types.Opcode.t -> int * int
val requirement : Aarch64_types.Opcode.t -> Target_component.Req.t option
val of_enc : Enc.t -> Aarch64_types.Opcode.t option
