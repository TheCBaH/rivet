(* One definition per hand-written x86 opcode: the encoder's constants for each of its forms, the
   component that publishes it, the mode it exists in, its flag effects, and whether its lowering
   stays code. Its typed builders follow, one per operand shape, named by the mnemonic with a
   suffix for the shape ([add rax rbx], [add_i 5L rax], [add_m (mem rax) rbx]); a shape whose
   operands do not fix the operand size carries the AT&T size letter ([addl_im]). *)

open Foundation
open X86_types

(* Typed views of {!Reg.t}. A general register's width and a vector register's length are phantom
   tags, so [add eax rbx] does not compile. The per-mode libraries name the registers that exist. *)
module View : sig
  type w8 = [ `W8 ]
  type w16 = [ `W16 ]
  type w32 = [ `W32 ]
  type w64 = [ `W64 ]
  type l128 = [ `L128 ]
  type l256 = [ `L256 ]
  type l512 = [ `L512 ]
  type +'w gpr = private Reg.t
  type +'l vec = private Reg.t
  type xmm = l128 vec
  type ymm = l256 vec
  type zmm = l512 vec
  type mmx = private Reg.t
  type kmask = private Reg.t
  type st = private Reg.t

  (* Unchecked: for the register tables, which only build values of the class the tag names. *)
  module Unsafe : sig
    val gpr : Reg.t -> 'w gpr
    val vec : Reg.t -> 'l vec
    val mmx : Reg.t -> mmx
    val kmask : Reg.t -> kmask
    val st : Reg.t -> st
  end
end = struct
  type w8 = [ `W8 ]
  type w16 = [ `W16 ]
  type w32 = [ `W32 ]
  type w64 = [ `W64 ]
  type l128 = [ `L128 ]
  type l256 = [ `L256 ]
  type l512 = [ `L512 ]
  type 'w gpr = Reg.t
  type 'l vec = Reg.t
  type xmm = l128 vec
  type ymm = l256 vec
  type zmm = l512 vec
  type mmx = Reg.t
  type kmask = Reg.t
  type st = Reg.t

  module Unsafe = struct
    let gpr r = r
    let vec r = r
    let mmx r = r
    let kmask r = r
    let st r = r
  end
end

open View

module Kind = struct
  (* [R]/[V]: a general/vector register of the shape's own width/length; [St0], [Cl], [Ax], [Dx]:
     a register the spelling names but the builder supplies. *)
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

  let name = function
    | R -> "r"
    | R8 -> "r8"
    | R16 -> "r16"
    | R32 -> "r32"
    | R64 -> "r64"
    | V -> "v"
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | Mm -> "mm"
    | K -> "k"
    | St0 -> "st"
    | Sti -> "sti"
    | M -> "m"
    | I -> "i"
    | I32 -> "i32"
    | I64 -> "i64"
    | S -> "s"
    | Cl -> "cl"
    | Ax -> "ax"
    | Dx -> "dx"
end

(* A builder's operands in AT&T order. ['w]/['l] are the width/length its [R]/[V] operands share,
   ['f] the builder's type with immediates as [int] ([int64] for 32- and 64-bit immediates), ['g]
   the same with immediates as {!Asm_core.Expr.t} (the [_e] builder), ['r] their result. *)
module Shape = struct
  type ('w, 'l, 'f, 'g, 'r) t =
    | Done : ('w, 'l, 'r, 'r, 'r) t
    | R : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'w gpr -> 'f, 'w gpr -> 'g, 'r) t
    | R8 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, w8 gpr -> 'f, w8 gpr -> 'g, 'r) t
    | R16 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, w16 gpr -> 'f, w16 gpr -> 'g, 'r) t
    | R32 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, w32 gpr -> 'f, w32 gpr -> 'g, 'r) t
    | R64 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, w64 gpr -> 'f, w64 gpr -> 'g, 'r) t
    | V : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'l vec -> 'f, 'l vec -> 'g, 'r) t
    | X : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, xmm -> 'f, xmm -> 'g, 'r) t
    | Y : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, ymm -> 'f, ymm -> 'g, 'r) t
    | Z : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, zmm -> 'f, zmm -> 'g, 'r) t
    | Mm : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, mmx -> 'f, mmx -> 'g, 'r) t
    | K : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, kmask -> 'f, kmask -> 'g, 'r) t
    | St0 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Sti : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, st -> 'f, st -> 'g, 'r) t
    | M : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, Mem.t -> 'f, Mem.t -> 'g, 'r) t
    | I : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | I32 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | I64 : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | S : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, Asm_core.Expr.t -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Cl : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Ax : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t
    | Dx : ('w, 'l, 'f, 'g, 'r) t -> ('w, 'l, 'f, 'g, 'r) t

  let rec kinds : type w l f g r. (w, l, f, g, r) t -> Kind.t list = function
    | Done -> []
    | R s -> Kind.R :: kinds s
    | R8 s -> Kind.R8 :: kinds s
    | R16 s -> Kind.R16 :: kinds s
    | R32 s -> Kind.R32 :: kinds s
    | R64 s -> Kind.R64 :: kinds s
    | V s -> Kind.V :: kinds s
    | X s -> Kind.X :: kinds s
    | Y s -> Kind.Y :: kinds s
    | Z s -> Kind.Z :: kinds s
    | Mm s -> Kind.Mm :: kinds s
    | K s -> Kind.K :: kinds s
    | St0 s -> Kind.St0 :: kinds s
    | Sti s -> Kind.Sti :: kinds s
    | M s -> Kind.M :: kinds s
    | I s -> Kind.I :: kinds s
    | I32 s -> Kind.I32 :: kinds s
    | I64 s -> Kind.I64 :: kinds s
    | S s -> Kind.S :: kinds s
    | Cl s -> Kind.Cl :: kinds s
    | Ax s -> Kind.Ax :: kinds s
    | Dx s -> Kind.Dx :: kinds s

  let g (type w) (r : w gpr) = Operand.Reg (r :> Reg.t)
  let gw (type w) (r : w gpr) = Reg.width (r :> Reg.t)
  let v (type l) (r : l vec) = Operand.Reg (r :> Reg.t)
  let mm (r : mmx) = Operand.Reg (r :> Reg.t)
  let km (r : kmask) = Operand.Reg (r :> Reg.t)
  let sti (r : st) = Operand.Reg (r :> Reg.t)
  let imm n = Operand.Imm (Bigint.of_int n)
  let imm64 n = Operand.Imm (Bigint.of_int64 n)
  let expr = function Asm_core.Expr.Const n -> Operand.Imm n | e -> Operand.Imm_sym e
  let st0 = Operand.Reg { Reg.cls = St; num = 0 }
  let cl = Operand.Reg { Reg.cls = Gpr8; num = 1 }
  let ax = Operand.Reg { Reg.cls = Gpr16; num = 0 }
  let dx = Operand.Reg { Reg.cls = Gpr16; num = 2 }

  (* [k] receives the operands and the width of the first [R] operand, if any. *)
  let rec build : type w l f g r. (w, l, f, g, r) t -> (int option -> Operand.t list -> r) -> f =
   fun s k ->
    let next s o = build s (fun w ops -> k w (o :: ops)) in
    let next_r s r =
      build s (fun w ops -> k (match w with None -> Some (gw r) | w -> w) (g r :: ops))
    in
    match s with
    | Done -> k None []
    | R s -> fun r -> next_r s r
    | R8 s -> fun r -> next s (g r)
    | R16 s -> fun r -> next s (g r)
    | R32 s -> fun r -> next s (g r)
    | R64 s -> fun r -> next s (g r)
    | V s -> fun r -> next s (v r)
    | X s -> fun r -> next s (v r)
    | Y s -> fun r -> next s (v r)
    | Z s -> fun r -> next s (v r)
    | Mm s -> fun r -> next s (mm r)
    | K s -> fun r -> next s (km r)
    | St0 s -> next s st0
    | Sti s -> fun r -> next s (sti r)
    | M s -> fun m -> next s (Operand.Mem m)
    | I s -> fun n -> next s (imm n)
    | I32 s -> fun n -> next s (imm64 n)
    | I64 s -> fun n -> next s (imm64 n)
    | S s -> fun e -> next s (Operand.Sym e)
    | Cl s -> next s cl
    | Ax s -> next s ax
    | Dx s -> next s dx

  let rec build_e : type w l f g r. (w, l, f, g, r) t -> (int option -> Operand.t list -> r) -> g =
   fun s k ->
    let next s o = build_e s (fun w ops -> k w (o :: ops)) in
    let next_r s r =
      build_e s (fun w ops -> k (match w with None -> Some (gw r) | w -> w) (g r :: ops))
    in
    match s with
    | Done -> k None []
    | R s -> fun r -> next_r s r
    | R8 s -> fun r -> next s (g r)
    | R16 s -> fun r -> next s (g r)
    | R32 s -> fun r -> next s (g r)
    | R64 s -> fun r -> next s (g r)
    | V s -> fun r -> next s (v r)
    | X s -> fun r -> next s (v r)
    | Y s -> fun r -> next s (v r)
    | Z s -> fun r -> next s (v r)
    | Mm s -> fun r -> next s (mm r)
    | K s -> fun r -> next s (km r)
    | St0 s -> next s st0
    | Sti s -> fun r -> next s (sti r)
    | M s -> fun m -> next s (Operand.Mem m)
    | I s -> fun e -> next s (expr e)
    | I32 s -> fun e -> next s (expr e)
    | I64 s -> fun e -> next s (expr e)
    | S s -> fun e -> next s (Operand.Sym e)
    | Cl s -> next s cl
    | Ax s -> next s ax
    | Dx s -> next s dx
end

(* The sample operands a test builds and spells: the register at position [i] is [gpr]'s for
   that position (the [St] index and the vector, mask and MMX number are [1 + i]), an [R] has
   width [w], a [V] length [l], a memory operand is [(base)], an immediate is 1 and a target the
   symbol [l]. *)
module Sample = struct
  type regs = { gpr : Rclass.t -> int -> Reg.t; base : Reg.t }

  (* Applies a builder to those operands. *)
  let apply : type w l f g r. regs -> int -> int -> (w, l, f, g, r) Shape.t -> f -> r =
   fun rs w l s f ->
    let vec cls i = { Reg.cls; num = 1 + i } in
    let vcls = match l with 256 -> Rclass.Ymm | 512 -> Zmm | _ -> Xmm in
    let wcls () = Option.get (Rclass.of_width w) in
    let rec go : type w l f g r. int -> (w, l, f, g, r) Shape.t -> f -> r =
     fun i s f ->
      match s with
      | Done -> f
      | R s -> go (i + 1) s (f (Unsafe.gpr (rs.gpr (wcls ()) i)))
      | R8 s -> go (i + 1) s (f (Unsafe.gpr (rs.gpr Gpr8 i)))
      | R16 s -> go (i + 1) s (f (Unsafe.gpr (rs.gpr Gpr16 i)))
      | R32 s -> go (i + 1) s (f (Unsafe.gpr (rs.gpr Gpr32 i)))
      | R64 s -> go (i + 1) s (f (Unsafe.gpr (rs.gpr Gpr64 i)))
      | V s -> go (i + 1) s (f (Unsafe.vec (vec vcls i)))
      | X s -> go (i + 1) s (f (Unsafe.vec (vec Xmm i)))
      | Y s -> go (i + 1) s (f (Unsafe.vec (vec Ymm i)))
      | Z s -> go (i + 1) s (f (Unsafe.vec (vec Zmm i)))
      | Mm s -> go (i + 1) s (f (Unsafe.mmx (vec Mmx i)))
      | K s -> go (i + 1) s (f (Unsafe.kmask (vec Kmask i)))
      | St0 s -> go (i + 1) s f
      | Sti s -> go (i + 1) s (f (Unsafe.st (vec St i)))
      | M s -> go (i + 1) s (f (Mem.of_base rs.base))
      | I s -> go (i + 1) s (f 1)
      | I32 s -> go (i + 1) s (f 1L)
      | I64 s -> go (i + 1) s (f 1L)
      | S s -> go (i + 1) s (f (Asm_core.Expr.Symbol "l"))
      | Cl s -> go (i + 1) s f
      | Ax s -> go (i + 1) s f
      | Dx s -> go (i + 1) s f
    in
    go 0 s f
end

(* The encoder's constant tables, one constructor per table: an opcode byte (or a ModR/M
   extension) in that table's form. *)
module Form = struct
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

  let name = function
    | Alu_rm_r -> "alu-rm-r-op"
    | Alu_r_rm -> "alu-r-rm-op"
    | Alu_ext -> "alu-ext"
    | Unary_ext -> "unary-ext"
    | Shift_ext -> "shift-ext"
    | Sse_binop_f2 -> "sse-binop-f2-op"
    | Sse_binop_f3 -> "sse-binop-f3-op"
    | Sse_binop_66 -> "sse-binop-66-op"
    | Sse_binop_none -> "sse-binop-none-op"
    | Xmm_shift_ext -> "xmm-shift-ext"
    | Xmm_shift_opcode -> "xmm-shift-opcode"
    | Sse_binop_0f38 -> "sse-binop-0f38-op"
    | Sse_binop_imm_0f3a -> "sse-binop-imm-0f3a-op"
    | Sse_gpr_imm_0f3a -> "sse-gpr-imm-0f3a-op"
    | Vex3_map2_66 -> "vex3-map2-66-op"
    | Vex3_map3_66 -> "vex3-map3-66-op"
    | Vex3_map2_unop -> "vex3-map2-unop-op"
    | Sse_binop_imm_none -> "sse-binop-imm-none-op"
    | Sse_binop_imm_66 -> "sse-binop-imm-66-op"
    | Sse_binop_imm_f3 -> "sse-binop-imm-f3-op"
    | Sse_binop_imm_f2 -> "sse-binop-imm-f2-op"
    | Vex_scalar_f2 -> "vex-scalar-f2-op"
    | Vex_scalar_f3 -> "vex-scalar-f3-op"
    | Vex_scalar_none -> "vex-scalar-none-op"
    | Evex_ps -> "evex-ps-op"
    | Vex_scalar_66 -> "vex-scalar-66-op"
    | Evex_pd -> "evex-pd-op"
    | Vex_unop_none -> "vex-unop-none-op"
    | Vex_unop_66 -> "vex-unop-66-op"
    | Vex_unop_f3 -> "vex-unop-f3-op"
    | Vex_binop_imm_none -> "vex-binop-imm-none-op"
    | Vex_binop_imm_66 -> "vex-binop-imm-66-op"
    | Vex_binop_imm_f3 -> "vex-binop-imm-f3-op"
    | Vex_binop_imm_f2 -> "vex-binop-imm-f2-op"
    | Vex_unop_imm_66 -> "vex-unop-imm-66-op"
    | Vex_unop_imm_f2 -> "vex-unop-imm-f2-op"
    | Vex_unop_imm_f3 -> "vex-unop-imm-f3-op"
    | Vex_shift_ext -> "vex-shift-ext"
    | Vex_shift_opcode -> "vex-shift-opcode"
    | Sse_movmsk_none -> "sse-movmsk-none-op"
    | Sse_movmsk_66 -> "sse-movmsk-66-op"
    | Vex_movmsk_none -> "vex-movmsk-none-op"
    | Vex_movmsk_66 -> "vex-movmsk-66-op"
end

type mode = Both | Only32 | Only64

type builder = {
  name : string;  (** the OCaml name *)
  spelling : string;  (** the text mnemonic; [%s] stands for the AT&T size letter of [width] *)
  kinds : Kind.t list;
  widths : int list;  (** the widths an [R] operand takes *)
  lengths : int list;  (** the lengths a [V] operand takes *)
  bmode : mode;
  sample : Sample.regs -> address:int -> width:int -> length:int -> Instruction.t;
      (** the builder applied to {!Sample}'s operands; [address] is the mode's address width *)
}

type entry = {
  op : Opcode.t;
  forms : (Form.t * int) list;
  feature : string option;  (** the component that publishes it; none for the base *)
  mode : mode;
  reads_flags : bool;
  writes_flags : bool;
  lower : bool;  (** no constant table: lowering stays code *)
  builders : builder list;
}

let registry = ref []

let def ?(forms = []) ?feature ?(mode = Both) ?(reads_flags = false) ?(writes_flags = false) op
    builders =
  registry :=
    { op; forms; feature; mode; reads_flags; writes_flags; lower = forms = []; builders }
    :: !registry

(* The instruction width: the first [R] operand's, else [width]; [0] stands for the mode's address
   width, which the per-mode libraries supply. *)
let mk ?(width = 32) op s =
  Shape.build s (fun w ops -> Instruction.mk op (Option.value w ~default:width) ops)

let mk_e ?(width = 32) op s =
  Shape.build_e s (fun w ops -> Instruction.mk op (Option.value w ~default:width) ops)

let b ?(width = 32) ?(widths = []) ?(lengths = []) ?(mode = Both) name spelling op s =
  {
    name;
    spelling;
    kinds = Shape.kinds s;
    widths;
    lengths;
    bmode = mode;
    sample =
      (fun rs ~address ~width:w ~length:l ->
        Sample.apply rs w l s (mk ~width:(if width = 0 then address else width) op s));
  }

(* {1 Shapes} *)

module S = struct
  let s_add = Shape.(R (R Done))
  let s_add_rrr = Shape.(R (R (R Done)))
  let s_add_rm = Shape.(R (M Done))
  let s_add_i = Shape.(I32 (R Done))
  let s_add_m = Shape.(M (R Done))
  let s_add_rmr = Shape.(R (M (R Done)))
  let s_add_irr = Shape.(I32 (R (R Done)))
  let s_add_mrr = Shape.(M (R (R Done)))
  let s_addb_im = Shape.(I32 (M Done))
  let s_addl_im = Shape.(I32 (M Done))
  let s_addq_im = Shape.(I32 (M Done))
  let s_add_im = Shape.(I32 (M (R Done)))
  let s_sub = Shape.(R (R Done))
  let s_sub_rrr = Shape.(R (R (R Done)))
  let s_sub_rm = Shape.(R (M Done))
  let s_sub_i = Shape.(I32 (R Done))
  let s_sub_m = Shape.(M (R Done))
  let s_sub_rmr = Shape.(R (M (R Done)))
  let s_sub_irr = Shape.(I32 (R (R Done)))
  let s_sub_mrr = Shape.(M (R (R Done)))
  let s_subb_im = Shape.(I32 (M Done))
  let s_subl_im = Shape.(I32 (M Done))
  let s_subq_im = Shape.(I32 (M Done))
  let s_sub_im = Shape.(I32 (M (R Done)))
  let s_mov = Shape.(R (R Done))
  let s_mov_qq = Shape.(Mm (Mm Done))
  let s_mov_q = Shape.(Mm (R64 Done))
  let s_mov_rq = Shape.(R64 (Mm Done))
  let s_mov_rm = Shape.(R (M Done))
  let s_mov_rs = Shape.(R (S Done))
  let s_mov_i = Shape.(I64 (R Done))
  let s_mov_m = Shape.(M (R Done))
  let s_mov_mq = Shape.(M (Mm Done))
  let s_mov_mx = Shape.(M (X Done))
  let s_mov_qm = Shape.(Mm (M Done))
  let s_mov_s = Shape.(S (R Done))
  let s_mov_xm = Shape.(X (M Done))
  let s_movb_im = Shape.(I32 (M Done))
  let s_movw_im = Shape.(I32 (M Done))
  let s_movl_im = Shape.(I32 (M Done))
  let s_movq_im = Shape.(I32 (M Done))
  let s_lea_m = Shape.(M (R Done))
  let s_lea_s = Shape.(S (R Done))
  let s_ret_m32 = Shape.(Done)
  let s_ret_m64 = Shape.(Done)
  let s_xor = Shape.(R (R Done))
  let s_xor_rrr = Shape.(R (R (R Done)))
  let s_xor_rm = Shape.(R (M Done))
  let s_xor_i = Shape.(I32 (R Done))
  let s_xor_m = Shape.(M (R Done))
  let s_xor_rmr = Shape.(R (M (R Done)))
  let s_xor_irr = Shape.(I32 (R (R Done)))
  let s_xor_mrr = Shape.(M (R (R Done)))
  let s_xorb_im = Shape.(I32 (M Done))
  let s_xorl_im = Shape.(I32 (M Done))
  let s_xorq_im = Shape.(I32 (M Done))
  let s_xor_im = Shape.(I32 (M (R Done)))
  let s_and_ = Shape.(R (R Done))
  let s_and_rrr = Shape.(R (R (R Done)))
  let s_and_rm = Shape.(R (M Done))
  let s_and_i = Shape.(I32 (R Done))
  let s_and_m = Shape.(M (R Done))
  let s_and_rmr = Shape.(R (M (R Done)))
  let s_and_irr = Shape.(I32 (R (R Done)))
  let s_and_mrr = Shape.(M (R (R Done)))
  let s_andb_im = Shape.(I32 (M Done))
  let s_andl_im = Shape.(I32 (M Done))
  let s_andq_im = Shape.(I32 (M Done))
  let s_and_im = Shape.(I32 (M (R Done)))
  let s_cmp = Shape.(R (R Done))
  let s_cmp_rm = Shape.(R (M Done))
  let s_cmp_i = Shape.(I32 (R Done))
  let s_cmp_m = Shape.(M (R Done))
  let s_cmpb_im = Shape.(I32 (M Done))
  let s_cmpl_im = Shape.(I32 (M Done))
  let s_cmpq_im = Shape.(I32 (M Done))
  let s_imul = Shape.(R Done)
  let s_imul_rr = Shape.(R (R Done))
  let s_imul_rrr = Shape.(R (R (R Done)))
  let s_imull_m = Shape.(M Done)
  let s_imulq_m = Shape.(M Done)
  let s_imul_m = Shape.(M (R Done))
  let s_imul_s = Shape.(S (R Done))
  let s_imul_i = Shape.(I32 (R (R Done)))
  let s_imul_mrr = Shape.(M (R (R Done)))
  let s_imul_im = Shape.(I32 (M (R Done)))
  let s_ud2_m32 = Shape.(Done)
  let s_ud2_m64 = Shape.(Done)
  let s_pop_m32 = Shape.(R32 Done)
  let s_pop_m64 = Shape.(R64 Done)
  let s_pop_m_m32 = Shape.(M Done)
  let s_pop_m_m64 = Shape.(M Done)
  let s_bswap = Shape.(R Done)
  let s_jmp_m32 = Shape.(S Done)
  let s_jmp_m64 = Shape.(S Done)
  let s_jmp_r_m32 = Shape.(R32 Done)
  let s_jmp_r_m64 = Shape.(R64 Done)
  let s_jmp_m_m32 = Shape.(M Done)
  let s_jmp_m_m64 = Shape.(M Done)
  let s_call_m32 = Shape.(S Done)
  let s_call_m64 = Shape.(S Done)
  let s_call_r_m32 = Shape.(R32 Done)
  let s_call_r_m64 = Shape.(R64 Done)
  let s_call_m_m32 = Shape.(M Done)
  let s_call_m_m64 = Shape.(M Done)
  let s_push_m32 = Shape.(R32 Done)
  let s_push_m64 = Shape.(R64 Done)
  let s_push_i_m32 = Shape.(I32 Done)
  let s_push_i_m64 = Shape.(I32 Done)
  let s_neg = Shape.(R Done)
  let s_neg_rr = Shape.(R (R Done))
  let s_negl_m = Shape.(M Done)
  let s_negq_m = Shape.(M Done)
  let s_neg_m = Shape.(M (R Done))
  let s_test = Shape.(R (R Done))
  let s_test_rm = Shape.(R (M Done))
  let s_test_i = Shape.(I32 (R Done))
  let s_testl_im = Shape.(I32 (M Done))
  let s_testq_im = Shape.(I32 (M Done))
  let s_adc = Shape.(R (R Done))
  let s_adc_rrr = Shape.(R (R (R Done)))
  let s_adc_rm = Shape.(R (M Done))
  let s_adc_i = Shape.(I32 (R Done))
  let s_adc_m = Shape.(M (R Done))
  let s_adc_rmr = Shape.(R (M (R Done)))
  let s_adc_irr = Shape.(I32 (R (R Done)))
  let s_adc_mrr = Shape.(M (R (R Done)))
  let s_adcb_im = Shape.(I32 (M Done))
  let s_adcl_im = Shape.(I32 (M Done))
  let s_adcq_im = Shape.(I32 (M Done))
  let s_adc_im = Shape.(I32 (M (R Done)))
  let s_sbb = Shape.(R (R Done))
  let s_sbb_rrr = Shape.(R (R (R Done)))
  let s_sbb_rm = Shape.(R (M Done))
  let s_sbb_i = Shape.(I32 (R Done))
  let s_sbb_m = Shape.(M (R Done))
  let s_sbb_rmr = Shape.(R (M (R Done)))
  let s_sbb_irr = Shape.(I32 (R (R Done)))
  let s_sbb_mrr = Shape.(M (R (R Done)))
  let s_sbbb_im = Shape.(I32 (M Done))
  let s_sbbl_im = Shape.(I32 (M Done))
  let s_sbbq_im = Shape.(I32 (M Done))
  let s_sbb_im = Shape.(I32 (M (R Done)))
  let s_mul = Shape.(R Done)
  let s_mull_m = Shape.(M Done)
  let s_mulq_m = Shape.(M Done)
  let s_div = Shape.(R Done)
  let s_divl_m = Shape.(M Done)
  let s_divq_m = Shape.(M Done)
  let s_dec = Shape.(R32 Done)
  let s_dec_m = Shape.(M Done)
  let s_rcr = Shape.(R Done)
  let s_rcrl_m = Shape.(M Done)
  let s_rcrq_m = Shape.(M Done)
  let s_rcr_cl = Shape.(Cl (R Done))
  let s_rcr_i = Shape.(I (R Done))
  let s_rcr_clrr = Shape.(Cl (R (R Done)))
  let s_rcr_irr = Shape.(I (R (R Done)))
  let s_rcrl_clm = Shape.(Cl (M Done))
  let s_rcrq_clm = Shape.(Cl (M Done))
  let s_rcrl_im = Shape.(I (M Done))
  let s_rcrq_im = Shape.(I (M Done))
  let s_rcr_clm = Shape.(Cl (M (R Done)))
  let s_rcr_im = Shape.(I (M (R Done)))
  let s_shr = Shape.(R Done)
  let s_shrl_m = Shape.(M Done)
  let s_shrq_m = Shape.(M Done)
  let s_shr_cl = Shape.(Cl (R Done))
  let s_shr_i = Shape.(I (R Done))
  let s_shr_clrr = Shape.(Cl (R (R Done)))
  let s_shr_irr = Shape.(I (R (R Done)))
  let s_shrl_clm = Shape.(Cl (M Done))
  let s_shrq_clm = Shape.(Cl (M Done))
  let s_shrl_im = Shape.(I (M Done))
  let s_shrq_im = Shape.(I (M Done))
  let s_shr_clm = Shape.(Cl (M (R Done)))
  let s_shr_im = Shape.(I (M (R Done)))
  let s_or_ = Shape.(R (R Done))
  let s_or_rrr = Shape.(R (R (R Done)))
  let s_or_rm = Shape.(R (M Done))
  let s_or_i = Shape.(I32 (R Done))
  let s_or_m = Shape.(M (R Done))
  let s_or_rmr = Shape.(R (M (R Done)))
  let s_or_irr = Shape.(I32 (R (R Done)))
  let s_or_mrr = Shape.(M (R (R Done)))
  let s_orb_im = Shape.(I32 (M Done))
  let s_orl_im = Shape.(I32 (M Done))
  let s_orq_im = Shape.(I32 (M Done))
  let s_or_im = Shape.(I32 (M (R Done)))
  let s_not = Shape.(R Done)
  let s_not_rr = Shape.(R (R Done))
  let s_notl_m = Shape.(M Done)
  let s_notq_m = Shape.(M Done)
  let s_not_m = Shape.(M (R Done))
  let s_ror = Shape.(R Done)
  let s_rorl_m = Shape.(M Done)
  let s_rorq_m = Shape.(M Done)
  let s_ror_cl = Shape.(Cl (R Done))
  let s_ror_i = Shape.(I (R Done))
  let s_ror_clrr = Shape.(Cl (R (R Done)))
  let s_ror_irr = Shape.(I (R (R Done)))
  let s_rorl_clm = Shape.(Cl (M Done))
  let s_rorq_clm = Shape.(Cl (M Done))
  let s_rorl_im = Shape.(I (M Done))
  let s_rorq_im = Shape.(I (M Done))
  let s_ror_clm = Shape.(Cl (M (R Done)))
  let s_ror_im = Shape.(I (M (R Done)))
  let s_shl = Shape.(R Done)
  let s_shll_m = Shape.(M Done)
  let s_shlq_m = Shape.(M Done)
  let s_shl_cl = Shape.(Cl (R Done))
  let s_shl_i = Shape.(I (R Done))
  let s_shl_clrr = Shape.(Cl (R (R Done)))
  let s_shl_irr = Shape.(I (R (R Done)))
  let s_shll_clm = Shape.(Cl (M Done))
  let s_shlq_clm = Shape.(Cl (M Done))
  let s_shll_im = Shape.(I (M Done))
  let s_shlq_im = Shape.(I (M Done))
  let s_shl_clm = Shape.(Cl (M (R Done)))
  let s_shl_im = Shape.(I (M (R Done)))
  let s_sar = Shape.(R Done)
  let s_sarl_m = Shape.(M Done)
  let s_sarq_m = Shape.(M Done)
  let s_sar_cl = Shape.(Cl (R Done))
  let s_sar_i = Shape.(I (R Done))
  let s_sar_clrr = Shape.(Cl (R (R Done)))
  let s_sar_irr = Shape.(I (R (R Done)))
  let s_sarl_clm = Shape.(Cl (M Done))
  let s_sarq_clm = Shape.(Cl (M Done))
  let s_sarl_im = Shape.(I (M Done))
  let s_sarq_im = Shape.(I (M Done))
  let s_sar_clm = Shape.(Cl (M (R Done)))
  let s_sar_im = Shape.(I (M (R Done)))
  let s_shld_cl = Shape.(Cl (R (R Done)))
  let s_shld_i = Shape.(I (R (R Done)))
  let s_shld_irrr = Shape.(I (R (R (R Done))))
  let s_shld_clrm = Shape.(Cl (R (M Done)))
  let s_shld_irm = Shape.(I (R (M Done)))
  let s_shld_irmr = Shape.(I (R (M (R Done))))
  let s_addsd = Shape.(X (X Done))
  let s_addsd_m = Shape.(M (X Done))
  let s_subsd = Shape.(X (X Done))
  let s_subsd_m = Shape.(M (X Done))
  let s_mulsd = Shape.(X (X Done))
  let s_mulsd_m = Shape.(M (X Done))
  let s_divsd = Shape.(X (X Done))
  let s_divsd_m = Shape.(M (X Done))
  let s_addss = Shape.(X (X Done))
  let s_addss_m = Shape.(M (X Done))
  let s_subss = Shape.(X (X Done))
  let s_subss_m = Shape.(M (X Done))
  let s_mulss = Shape.(X (X Done))
  let s_mulss_m = Shape.(M (X Done))
  let s_divss = Shape.(X (X Done))
  let s_divss_m = Shape.(M (X Done))
  let s_comisd = Shape.(X (X Done))
  let s_comisd_m = Shape.(M (X Done))
  let s_ucomisd = Shape.(X (X Done))
  let s_ucomisd_m = Shape.(M (X Done))
  let s_comiss = Shape.(X (X Done))
  let s_comiss_m = Shape.(M (X Done))
  let s_ucomiss = Shape.(X (X Done))
  let s_ucomiss_m = Shape.(M (X Done))
  let s_xorpd = Shape.(X (X Done))
  let s_xorpd_m = Shape.(M (X Done))
  let s_pxor_qq = Shape.(Mm (Mm Done))
  let s_pxor = Shape.(X (X Done))
  let s_pxor_mq = Shape.(M (Mm Done))
  let s_pxor_m = Shape.(M (X Done))
  let s_movapd = Shape.(X (X Done))
  let s_movapd_m = Shape.(M (X Done))
  let s_movapd_rm = Shape.(X (M Done))
  let s_cvtsd2ss = Shape.(X (X Done))
  let s_cvtsd2ss_m = Shape.(M (X Done))
  let s_cvtss2sd = Shape.(X (X Done))
  let s_cvtss2sd_m = Shape.(M (X Done))
  let s_cvtps2pd = Shape.(X (X Done))
  let s_cvtps2pd_m = Shape.(M (X Done))
  let s_cvtpd2ps = Shape.(X (X Done))
  let s_cvtpd2ps_m = Shape.(M (X Done))
  let s_cvtdq2ps = Shape.(X (X Done))
  let s_cvtdq2ps_m = Shape.(M (X Done))
  let s_cvtps2dq = Shape.(X (X Done))
  let s_cvtps2dq_m = Shape.(M (X Done))
  let s_cvttps2dq = Shape.(X (X Done))
  let s_cvttps2dq_m = Shape.(M (X Done))
  let s_movsd = Shape.(X (X Done))
  let s_movsd_m = Shape.(M (X Done))
  let s_movsd_rm = Shape.(X (M Done))
  let s_movss = Shape.(X (X Done))
  let s_movss_m = Shape.(M (X Done))
  let s_movss_rm = Shape.(X (M Done))
  let s_cvtsi2sd = Shape.(R (X Done))
  let s_cvtsi2sd_m = Shape.(M (X Done))
  let s_cvtsi2ss = Shape.(R (X Done))
  let s_cvtsi2ss_m = Shape.(M (X Done))
  let s_cvttsd2si_x = Shape.(X (R Done))
  let s_cvttsd2si_m = Shape.(M (R Done))
  let s_cvttsd2si_mr = Shape.(M (R64 Done))
  let s_unpcklps = Shape.(X (X Done))
  let s_unpcklps_m = Shape.(M (X Done))
  let s_unpckhps = Shape.(X (X Done))
  let s_unpckhps_m = Shape.(M (X Done))
  let s_unpcklpd = Shape.(X (X Done))
  let s_unpcklpd_m = Shape.(M (X Done))
  let s_unpckhpd = Shape.(X (X Done))
  let s_unpckhpd_m = Shape.(M (X Done))
  let s_punpcklqdq = Shape.(X (X Done))
  let s_punpcklqdq_m = Shape.(M (X Done))
  let s_punpckhqdq = Shape.(X (X Done))
  let s_punpckhqdq_m = Shape.(M (X Done))
  let s_punpcklbw_qq = Shape.(Mm (Mm Done))
  let s_punpcklbw = Shape.(X (X Done))
  let s_punpcklbw_mq = Shape.(M (Mm Done))
  let s_punpcklbw_m = Shape.(M (X Done))
  let s_punpckhbw_qq = Shape.(Mm (Mm Done))
  let s_punpckhbw = Shape.(X (X Done))
  let s_punpckhbw_mq = Shape.(M (Mm Done))
  let s_punpckhbw_m = Shape.(M (X Done))
  let s_punpcklwd_qq = Shape.(Mm (Mm Done))
  let s_punpcklwd = Shape.(X (X Done))
  let s_punpcklwd_mq = Shape.(M (Mm Done))
  let s_punpcklwd_m = Shape.(M (X Done))
  let s_punpckhwd_qq = Shape.(Mm (Mm Done))
  let s_punpckhwd = Shape.(X (X Done))
  let s_punpckhwd_mq = Shape.(M (Mm Done))
  let s_punpckhwd_m = Shape.(M (X Done))
  let s_punpckldq_qq = Shape.(Mm (Mm Done))
  let s_punpckldq = Shape.(X (X Done))
  let s_punpckldq_mq = Shape.(M (Mm Done))
  let s_punpckldq_m = Shape.(M (X Done))
  let s_punpckhdq_qq = Shape.(Mm (Mm Done))
  let s_punpckhdq = Shape.(X (X Done))
  let s_punpckhdq_mq = Shape.(M (Mm Done))
  let s_punpckhdq_m = Shape.(M (X Done))
  let s_paddb_qq = Shape.(Mm (Mm Done))
  let s_paddb = Shape.(X (X Done))
  let s_paddb_mq = Shape.(M (Mm Done))
  let s_paddb_m = Shape.(M (X Done))
  let s_paddw_qq = Shape.(Mm (Mm Done))
  let s_paddw = Shape.(X (X Done))
  let s_paddw_mq = Shape.(M (Mm Done))
  let s_paddw_m = Shape.(M (X Done))
  let s_paddd_qq = Shape.(Mm (Mm Done))
  let s_paddd = Shape.(X (X Done))
  let s_paddd_mq = Shape.(M (Mm Done))
  let s_paddd_m = Shape.(M (X Done))
  let s_paddq_qq = Shape.(Mm (Mm Done))
  let s_paddq = Shape.(X (X Done))
  let s_paddq_mq = Shape.(M (Mm Done))
  let s_paddq_m = Shape.(M (X Done))
  let s_psubb_qq = Shape.(Mm (Mm Done))
  let s_psubb = Shape.(X (X Done))
  let s_psubb_mq = Shape.(M (Mm Done))
  let s_psubb_m = Shape.(M (X Done))
  let s_psubw_qq = Shape.(Mm (Mm Done))
  let s_psubw = Shape.(X (X Done))
  let s_psubw_mq = Shape.(M (Mm Done))
  let s_psubw_m = Shape.(M (X Done))
  let s_psubd_qq = Shape.(Mm (Mm Done))
  let s_psubd = Shape.(X (X Done))
  let s_psubd_mq = Shape.(M (Mm Done))
  let s_psubd_m = Shape.(M (X Done))
  let s_psubq_qq = Shape.(Mm (Mm Done))
  let s_psubq = Shape.(X (X Done))
  let s_psubq_mq = Shape.(M (Mm Done))
  let s_psubq_m = Shape.(M (X Done))
  let s_pcmpeqb_qq = Shape.(Mm (Mm Done))
  let s_pcmpeqb = Shape.(X (X Done))
  let s_pcmpeqb_mq = Shape.(M (Mm Done))
  let s_pcmpeqb_m = Shape.(M (X Done))
  let s_pcmpeqw_qq = Shape.(Mm (Mm Done))
  let s_pcmpeqw = Shape.(X (X Done))
  let s_pcmpeqw_mq = Shape.(M (Mm Done))
  let s_pcmpeqw_m = Shape.(M (X Done))
  let s_pcmpeqd_qq = Shape.(Mm (Mm Done))
  let s_pcmpeqd = Shape.(X (X Done))
  let s_pcmpeqd_mq = Shape.(M (Mm Done))
  let s_pcmpeqd_m = Shape.(M (X Done))
  let s_pcmpgtb_qq = Shape.(Mm (Mm Done))
  let s_pcmpgtb = Shape.(X (X Done))
  let s_pcmpgtb_mq = Shape.(M (Mm Done))
  let s_pcmpgtb_m = Shape.(M (X Done))
  let s_pcmpgtw_qq = Shape.(Mm (Mm Done))
  let s_pcmpgtw = Shape.(X (X Done))
  let s_pcmpgtw_mq = Shape.(M (Mm Done))
  let s_pcmpgtw_m = Shape.(M (X Done))
  let s_pcmpgtd_qq = Shape.(Mm (Mm Done))
  let s_pcmpgtd = Shape.(X (X Done))
  let s_pcmpgtd_mq = Shape.(M (Mm Done))
  let s_pcmpgtd_m = Shape.(M (X Done))
  let s_packsswb_qq = Shape.(Mm (Mm Done))
  let s_packsswb = Shape.(X (X Done))
  let s_packsswb_mq = Shape.(M (Mm Done))
  let s_packsswb_m = Shape.(M (X Done))
  let s_packssdw_qq = Shape.(Mm (Mm Done))
  let s_packssdw = Shape.(X (X Done))
  let s_packssdw_mq = Shape.(M (Mm Done))
  let s_packssdw_m = Shape.(M (X Done))
  let s_packuswb_qq = Shape.(Mm (Mm Done))
  let s_packuswb = Shape.(X (X Done))
  let s_packuswb_mq = Shape.(M (Mm Done))
  let s_packuswb_m = Shape.(M (X Done))
  let s_pand_qq = Shape.(Mm (Mm Done))
  let s_pand = Shape.(X (X Done))
  let s_pand_mq = Shape.(M (Mm Done))
  let s_pand_m = Shape.(M (X Done))
  let s_pandn_qq = Shape.(Mm (Mm Done))
  let s_pandn = Shape.(X (X Done))
  let s_pandn_mq = Shape.(M (Mm Done))
  let s_pandn_m = Shape.(M (X Done))
  let s_por_qq = Shape.(Mm (Mm Done))
  let s_por = Shape.(X (X Done))
  let s_por_mq = Shape.(M (Mm Done))
  let s_por_m = Shape.(M (X Done))
  let s_pminub_qq = Shape.(Mm (Mm Done))
  let s_pminub = Shape.(X (X Done))
  let s_pminub_mq = Shape.(M (Mm Done))
  let s_pminub_m = Shape.(M (X Done))
  let s_pmaxub_qq = Shape.(Mm (Mm Done))
  let s_pmaxub = Shape.(X (X Done))
  let s_pmaxub_mq = Shape.(M (Mm Done))
  let s_pmaxub_m = Shape.(M (X Done))
  let s_pminsw_qq = Shape.(Mm (Mm Done))
  let s_pminsw = Shape.(X (X Done))
  let s_pminsw_mq = Shape.(M (Mm Done))
  let s_pminsw_m = Shape.(M (X Done))
  let s_pmaxsw_qq = Shape.(Mm (Mm Done))
  let s_pmaxsw = Shape.(X (X Done))
  let s_pmaxsw_mq = Shape.(M (Mm Done))
  let s_pmaxsw_m = Shape.(M (X Done))
  let s_pmullw_qq = Shape.(Mm (Mm Done))
  let s_pmullw = Shape.(X (X Done))
  let s_pmullw_mq = Shape.(M (Mm Done))
  let s_pmullw_m = Shape.(M (X Done))
  let s_pmulhw_qq = Shape.(Mm (Mm Done))
  let s_pmulhw = Shape.(X (X Done))
  let s_pmulhw_mq = Shape.(M (Mm Done))
  let s_pmulhw_m = Shape.(M (X Done))
  let s_pmulhuw_qq = Shape.(Mm (Mm Done))
  let s_pmulhuw = Shape.(X (X Done))
  let s_pmulhuw_mq = Shape.(M (Mm Done))
  let s_pmulhuw_m = Shape.(M (X Done))
  let s_pavgb_qq = Shape.(Mm (Mm Done))
  let s_pavgb = Shape.(X (X Done))
  let s_pavgb_mq = Shape.(M (Mm Done))
  let s_pavgb_m = Shape.(M (X Done))
  let s_pavgw_qq = Shape.(Mm (Mm Done))
  let s_pavgw = Shape.(X (X Done))
  let s_pavgw_mq = Shape.(M (Mm Done))
  let s_pavgw_m = Shape.(M (X Done))
  let s_psadbw_qq = Shape.(Mm (Mm Done))
  let s_psadbw = Shape.(X (X Done))
  let s_psadbw_mq = Shape.(M (Mm Done))
  let s_psadbw_m = Shape.(M (X Done))
  let s_psllw_qq = Shape.(Mm (Mm Done))
  let s_psllw = Shape.(X (X Done))
  let s_psllw_iq = Shape.(I (Mm Done))
  let s_psllw_i = Shape.(I (X Done))
  let s_psllw_mq = Shape.(M (Mm Done))
  let s_psllw_m = Shape.(M (X Done))
  let s_pslld_qq = Shape.(Mm (Mm Done))
  let s_pslld = Shape.(X (X Done))
  let s_pslld_iq = Shape.(I (Mm Done))
  let s_pslld_i = Shape.(I (X Done))
  let s_pslld_mq = Shape.(M (Mm Done))
  let s_pslld_m = Shape.(M (X Done))
  let s_psllq_qq = Shape.(Mm (Mm Done))
  let s_psllq = Shape.(X (X Done))
  let s_psllq_iq = Shape.(I (Mm Done))
  let s_psllq_i = Shape.(I (X Done))
  let s_psllq_mq = Shape.(M (Mm Done))
  let s_psllq_m = Shape.(M (X Done))
  let s_psrlw_qq = Shape.(Mm (Mm Done))
  let s_psrlw = Shape.(X (X Done))
  let s_psrlw_iq = Shape.(I (Mm Done))
  let s_psrlw_i = Shape.(I (X Done))
  let s_psrlw_mq = Shape.(M (Mm Done))
  let s_psrlw_m = Shape.(M (X Done))
  let s_psrld_qq = Shape.(Mm (Mm Done))
  let s_psrld = Shape.(X (X Done))
  let s_psrld_iq = Shape.(I (Mm Done))
  let s_psrld_i = Shape.(I (X Done))
  let s_psrld_mq = Shape.(M (Mm Done))
  let s_psrld_m = Shape.(M (X Done))
  let s_psrlq_qq = Shape.(Mm (Mm Done))
  let s_psrlq = Shape.(X (X Done))
  let s_psrlq_iq = Shape.(I (Mm Done))
  let s_psrlq_i = Shape.(I (X Done))
  let s_psrlq_mq = Shape.(M (Mm Done))
  let s_psrlq_m = Shape.(M (X Done))
  let s_psraw_qq = Shape.(Mm (Mm Done))
  let s_psraw = Shape.(X (X Done))
  let s_psraw_iq = Shape.(I (Mm Done))
  let s_psraw_i = Shape.(I (X Done))
  let s_psraw_mq = Shape.(M (Mm Done))
  let s_psraw_m = Shape.(M (X Done))
  let s_psrad_qq = Shape.(Mm (Mm Done))
  let s_psrad = Shape.(X (X Done))
  let s_psrad_iq = Shape.(I (Mm Done))
  let s_psrad_i = Shape.(I (X Done))
  let s_psrad_mq = Shape.(M (Mm Done))
  let s_psrad_m = Shape.(M (X Done))
  let s_pslldq_i = Shape.(I (X Done))
  let s_psrldq_i = Shape.(I (X Done))
  let s_pshufb_qq = Shape.(Mm (Mm Done))
  let s_pshufb = Shape.(X (X Done))
  let s_pshufb_mq = Shape.(M (Mm Done))
  let s_pshufb_m = Shape.(M (X Done))
  let s_phaddw_qq = Shape.(Mm (Mm Done))
  let s_phaddw = Shape.(X (X Done))
  let s_phaddw_mq = Shape.(M (Mm Done))
  let s_phaddw_m = Shape.(M (X Done))
  let s_phaddd_qq = Shape.(Mm (Mm Done))
  let s_phaddd = Shape.(X (X Done))
  let s_phaddd_mq = Shape.(M (Mm Done))
  let s_phaddd_m = Shape.(M (X Done))
  let s_phsubw_qq = Shape.(Mm (Mm Done))
  let s_phsubw = Shape.(X (X Done))
  let s_phsubw_mq = Shape.(M (Mm Done))
  let s_phsubw_m = Shape.(M (X Done))
  let s_phsubd_qq = Shape.(Mm (Mm Done))
  let s_phsubd = Shape.(X (X Done))
  let s_phsubd_mq = Shape.(M (Mm Done))
  let s_phsubd_m = Shape.(M (X Done))
  let s_psignb_qq = Shape.(Mm (Mm Done))
  let s_psignb = Shape.(X (X Done))
  let s_psignb_mq = Shape.(M (Mm Done))
  let s_psignb_m = Shape.(M (X Done))
  let s_psignw_qq = Shape.(Mm (Mm Done))
  let s_psignw = Shape.(X (X Done))
  let s_psignw_mq = Shape.(M (Mm Done))
  let s_psignw_m = Shape.(M (X Done))
  let s_psignd_qq = Shape.(Mm (Mm Done))
  let s_psignd = Shape.(X (X Done))
  let s_psignd_mq = Shape.(M (Mm Done))
  let s_psignd_m = Shape.(M (X Done))
  let s_pmaddubsw_qq = Shape.(Mm (Mm Done))
  let s_pmaddubsw = Shape.(X (X Done))
  let s_pmaddubsw_mq = Shape.(M (Mm Done))
  let s_pmaddubsw_m = Shape.(M (X Done))
  let s_pmulhrsw_qq = Shape.(Mm (Mm Done))
  let s_pmulhrsw = Shape.(X (X Done))
  let s_pmulhrsw_mq = Shape.(M (Mm Done))
  let s_pmulhrsw_m = Shape.(M (X Done))
  let s_phaddsw_qq = Shape.(Mm (Mm Done))
  let s_phaddsw = Shape.(X (X Done))
  let s_phaddsw_mq = Shape.(M (Mm Done))
  let s_phaddsw_m = Shape.(M (X Done))
  let s_phsubsw_qq = Shape.(Mm (Mm Done))
  let s_phsubsw = Shape.(X (X Done))
  let s_phsubsw_mq = Shape.(M (Mm Done))
  let s_phsubsw_m = Shape.(M (X Done))
  let s_pabsb_qq = Shape.(Mm (Mm Done))
  let s_pabsb = Shape.(X (X Done))
  let s_pabsb_mq = Shape.(M (Mm Done))
  let s_pabsb_m = Shape.(M (X Done))
  let s_pabsw_qq = Shape.(Mm (Mm Done))
  let s_pabsw = Shape.(X (X Done))
  let s_pabsw_mq = Shape.(M (Mm Done))
  let s_pabsw_m = Shape.(M (X Done))
  let s_pabsd_qq = Shape.(Mm (Mm Done))
  let s_pabsd = Shape.(X (X Done))
  let s_pabsd_mq = Shape.(M (Mm Done))
  let s_pabsd_m = Shape.(M (X Done))
  let s_palignr_iqq = Shape.(I (Mm (Mm Done)))
  let s_palignr_i = Shape.(I (X (X Done)))
  let s_palignr_imq = Shape.(I (M (Mm Done)))
  let s_palignr_im = Shape.(I (M (X Done)))
  let s_roundps_i = Shape.(I (X (X Done)))
  let s_roundps_im = Shape.(I (M (X Done)))
  let s_roundpd_i = Shape.(I (X (X Done)))
  let s_roundpd_im = Shape.(I (M (X Done)))
  let s_roundss_i = Shape.(I (X (X Done)))
  let s_roundss_im = Shape.(I (M (X Done)))
  let s_roundsd_i = Shape.(I (X (X Done)))
  let s_roundsd_im = Shape.(I (M (X Done)))
  let s_pcmpeqq = Shape.(X (X Done))
  let s_pcmpeqq_m = Shape.(M (X Done))
  let s_pcmpgtq = Shape.(X (X Done))
  let s_pcmpgtq_m = Shape.(M (X Done))
  let s_packusdw = Shape.(X (X Done))
  let s_packusdw_m = Shape.(M (X Done))
  let s_pmaxsb = Shape.(X (X Done))
  let s_pmaxsb_m = Shape.(M (X Done))
  let s_pmaxsd = Shape.(X (X Done))
  let s_pmaxsd_m = Shape.(M (X Done))
  let s_pmaxud = Shape.(X (X Done))
  let s_pmaxud_m = Shape.(M (X Done))
  let s_pmaxuw = Shape.(X (X Done))
  let s_pmaxuw_m = Shape.(M (X Done))
  let s_pminsb = Shape.(X (X Done))
  let s_pminsb_m = Shape.(M (X Done))
  let s_pminsd = Shape.(X (X Done))
  let s_pminsd_m = Shape.(M (X Done))
  let s_pminud = Shape.(X (X Done))
  let s_pminud_m = Shape.(M (X Done))
  let s_pminuw = Shape.(X (X Done))
  let s_pminuw_m = Shape.(M (X Done))
  let s_pmuldq = Shape.(X (X Done))
  let s_pmuldq_m = Shape.(M (X Done))
  let s_pmulld = Shape.(X (X Done))
  let s_pmulld_m = Shape.(M (X Done))
  let s_phminposuw = Shape.(X (X Done))
  let s_phminposuw_m = Shape.(M (X Done))
  let s_ptest = Shape.(X (X Done))
  let s_ptest_m = Shape.(M (X Done))
  let s_pmovsxbw = Shape.(X (X Done))
  let s_pmovsxbw_m = Shape.(M (X Done))
  let s_pmovsxbd = Shape.(X (X Done))
  let s_pmovsxbd_m = Shape.(M (X Done))
  let s_pmovsxbq = Shape.(X (X Done))
  let s_pmovsxbq_m = Shape.(M (X Done))
  let s_pmovsxwd = Shape.(X (X Done))
  let s_pmovsxwd_m = Shape.(M (X Done))
  let s_pmovsxwq = Shape.(X (X Done))
  let s_pmovsxwq_m = Shape.(M (X Done))
  let s_pmovsxdq = Shape.(X (X Done))
  let s_pmovsxdq_m = Shape.(M (X Done))
  let s_pmovzxbw = Shape.(X (X Done))
  let s_pmovzxbw_m = Shape.(M (X Done))
  let s_pmovzxbd = Shape.(X (X Done))
  let s_pmovzxbd_m = Shape.(M (X Done))
  let s_pmovzxbq = Shape.(X (X Done))
  let s_pmovzxbq_m = Shape.(M (X Done))
  let s_pmovzxwd = Shape.(X (X Done))
  let s_pmovzxwd_m = Shape.(M (X Done))
  let s_pmovzxwq = Shape.(X (X Done))
  let s_pmovzxwq_m = Shape.(M (X Done))
  let s_pmovzxdq = Shape.(X (X Done))
  let s_pmovzxdq_m = Shape.(M (X Done))
  let s_movntdqa_m = Shape.(M (X Done))
  let s_blendvps = Shape.(X (X Done))
  let s_blendvps_m = Shape.(M (X Done))
  let s_blendvpd = Shape.(X (X Done))
  let s_blendvpd_m = Shape.(M (X Done))
  let s_pblendvb = Shape.(X (X Done))
  let s_pblendvb_m = Shape.(M (X Done))
  let s_blendps_i = Shape.(I (X (X Done)))
  let s_blendps_im = Shape.(I (M (X Done)))
  let s_blendpd_i = Shape.(I (X (X Done)))
  let s_blendpd_im = Shape.(I (M (X Done)))
  let s_dpps_i = Shape.(I (X (X Done)))
  let s_dpps_im = Shape.(I (M (X Done)))
  let s_dppd_i = Shape.(I (X (X Done)))
  let s_dppd_im = Shape.(I (M (X Done)))
  let s_mpsadbw_i = Shape.(I (X (X Done)))
  let s_mpsadbw_im = Shape.(I (M (X Done)))
  let s_pblendw_i = Shape.(I (X (X Done)))
  let s_pblendw_im = Shape.(I (M (X Done)))
  let s_insertps_i = Shape.(I (X (X Done)))
  let s_insertps_im = Shape.(I (M (X Done)))
  let s_pinsrb_i = Shape.(I (R32 (X Done)))
  let s_pinsrb_im = Shape.(I (M (X Done)))
  let s_pinsrd_i = Shape.(I (R32 (X Done)))
  let s_pinsrd_im = Shape.(I (M (X Done)))
  let s_pextrb_i = Shape.(I (X (R32 Done)))
  let s_pextrb_irm = Shape.(I (X (M Done)))
  let s_pextrd_i = Shape.(I (X (R32 Done)))
  let s_pextrd_irm = Shape.(I (X (M Done)))
  let s_extractps_i = Shape.(I (X (R32 Done)))
  let s_extractps_irm = Shape.(I (X (M Done)))
  let s_vpshufb = Shape.(V (V (V Done)))
  let s_vpshufb_m = Shape.(M (V (V Done)))
  let s_vphaddw = Shape.(V (V (V Done)))
  let s_vphaddw_m = Shape.(M (V (V Done)))
  let s_vphaddd = Shape.(V (V (V Done)))
  let s_vphaddd_m = Shape.(M (V (V Done)))
  let s_vphaddsw = Shape.(V (V (V Done)))
  let s_vphaddsw_m = Shape.(M (V (V Done)))
  let s_vpmaddubsw = Shape.(V (V (V Done)))
  let s_vpmaddubsw_m = Shape.(M (V (V Done)))
  let s_vphsubw = Shape.(V (V (V Done)))
  let s_vphsubw_m = Shape.(M (V (V Done)))
  let s_vphsubd = Shape.(V (V (V Done)))
  let s_vphsubd_m = Shape.(M (V (V Done)))
  let s_vphsubsw = Shape.(V (V (V Done)))
  let s_vphsubsw_m = Shape.(M (V (V Done)))
  let s_vpsignb = Shape.(V (V (V Done)))
  let s_vpsignb_m = Shape.(M (V (V Done)))
  let s_vpsignw = Shape.(V (V (V Done)))
  let s_vpsignw_m = Shape.(M (V (V Done)))
  let s_vpsignd = Shape.(V (V (V Done)))
  let s_vpsignd_m = Shape.(M (V (V Done)))
  let s_vpmulhrsw = Shape.(V (V (V Done)))
  let s_vpmulhrsw_m = Shape.(M (V (V Done)))
  let s_vpmuldq = Shape.(V (V (V Done)))
  let s_vpmuldq_m = Shape.(M (V (V Done)))
  let s_vpcmpeqq = Shape.(V (V (V Done)))
  let s_vpcmpeqq_rrk = Shape.(V (V (K Done)))
  let s_vpcmpeqq_m = Shape.(M (V (V Done)))
  let s_vpcmpeqq_mrk = Shape.(M (V (K Done)))
  let s_vpackusdw = Shape.(V (V (V Done)))
  let s_vpackusdw_m = Shape.(M (V (V Done)))
  let s_vpcmpgtq = Shape.(V (V (V Done)))
  let s_vpcmpgtq_rrk = Shape.(V (V (K Done)))
  let s_vpcmpgtq_m = Shape.(M (V (V Done)))
  let s_vpcmpgtq_mrk = Shape.(M (V (K Done)))
  let s_vpminsb = Shape.(V (V (V Done)))
  let s_vpminsb_m = Shape.(M (V (V Done)))
  let s_vpminsd = Shape.(V (V (V Done)))
  let s_vpminsd_m = Shape.(M (V (V Done)))
  let s_vpminuw = Shape.(V (V (V Done)))
  let s_vpminuw_m = Shape.(M (V (V Done)))
  let s_vpminud = Shape.(V (V (V Done)))
  let s_vpminud_m = Shape.(M (V (V Done)))
  let s_vpmaxsb = Shape.(V (V (V Done)))
  let s_vpmaxsb_m = Shape.(M (V (V Done)))
  let s_vpmaxsd = Shape.(V (V (V Done)))
  let s_vpmaxsd_m = Shape.(M (V (V Done)))
  let s_vpmaxuw = Shape.(V (V (V Done)))
  let s_vpmaxuw_m = Shape.(M (V (V Done)))
  let s_vpmaxud = Shape.(V (V (V Done)))
  let s_vpmaxud_m = Shape.(M (V (V Done)))
  let s_vpmulld = Shape.(V (V (V Done)))
  let s_vpmulld_m = Shape.(M (V (V Done)))
  let s_vpalignr_i = Shape.(I (V (V (V Done))))
  let s_vpalignr_im = Shape.(I (M (V (V Done))))
  let s_vblendps_i = Shape.(I (V (V (V Done))))
  let s_vblendps_im = Shape.(I (M (V (V Done))))
  let s_vblendpd_i = Shape.(I (V (V (V Done))))
  let s_vblendpd_im = Shape.(I (M (V (V Done))))
  let s_vpblendw_i = Shape.(I (V (V (V Done))))
  let s_vpblendw_im = Shape.(I (M (V (V Done))))
  let s_vroundss_i = Shape.(I (X (X (X Done))))
  let s_vroundss_im = Shape.(I (M (X (X Done))))
  let s_vroundsd_i = Shape.(I (X (X (X Done))))
  let s_vroundsd_im = Shape.(I (M (X (X Done))))
  let s_vdpps_i = Shape.(I (V (V (V Done))))
  let s_vdpps_im = Shape.(I (M (V (V Done))))
  let s_vdppd_i = Shape.(I (X (X (X Done))))
  let s_vdppd_im = Shape.(I (M (X (X Done))))
  let s_vmpsadbw_i = Shape.(I (V (V (V Done))))
  let s_vmpsadbw_im = Shape.(I (M (V (V Done))))
  let s_vinsertps_i = Shape.(I (X (X (X Done))))
  let s_vinsertps_im = Shape.(I (M (X (X Done))))
  let s_vpabsb = Shape.(V (V Done))
  let s_vpabsb_m = Shape.(M (V Done))
  let s_vpabsw = Shape.(V (V Done))
  let s_vpabsw_m = Shape.(M (V Done))
  let s_vpabsd = Shape.(V (V Done))
  let s_vpabsd_m = Shape.(M (V Done))
  let s_vphminposuw = Shape.(X (X Done))
  let s_vphminposuw_m = Shape.(M (X Done))
  let s_vptest = Shape.(V (V Done))
  let s_vptest_m = Shape.(M (V Done))
  let s_vpmovsxbw = Shape.(X (V Done))
  let s_vpmovsxbw_rr = Shape.(Y (Z Done))
  let s_vpmovsxbw_m = Shape.(M (V Done))
  let s_vpmovsxbd = Shape.(X (V Done))
  let s_vpmovsxbd_m = Shape.(M (V Done))
  let s_vpmovsxbq = Shape.(X (V Done))
  let s_vpmovsxbq_m = Shape.(M (V Done))
  let s_vpmovsxwd = Shape.(X (V Done))
  let s_vpmovsxwd_rr = Shape.(Y (Z Done))
  let s_vpmovsxwd_m = Shape.(M (V Done))
  let s_vpmovsxwq = Shape.(X (V Done))
  let s_vpmovsxwq_m = Shape.(M (V Done))
  let s_vpmovsxdq = Shape.(X (V Done))
  let s_vpmovsxdq_rr = Shape.(Y (Z Done))
  let s_vpmovsxdq_m = Shape.(M (V Done))
  let s_vpmovzxbw = Shape.(X (V Done))
  let s_vpmovzxbw_rr = Shape.(Y (Z Done))
  let s_vpmovzxbw_m = Shape.(M (V Done))
  let s_vpmovzxbd = Shape.(X (V Done))
  let s_vpmovzxbd_m = Shape.(M (V Done))
  let s_vpmovzxbq = Shape.(X (V Done))
  let s_vpmovzxbq_m = Shape.(M (V Done))
  let s_vpmovzxwd = Shape.(X (V Done))
  let s_vpmovzxwd_rr = Shape.(Y (Z Done))
  let s_vpmovzxwd_m = Shape.(M (V Done))
  let s_vpmovzxwq = Shape.(X (V Done))
  let s_vpmovzxwq_m = Shape.(M (V Done))
  let s_vpmovzxdq = Shape.(X (V Done))
  let s_vpmovzxdq_rr = Shape.(Y (Z Done))
  let s_vpmovzxdq_m = Shape.(M (V Done))
  let s_vmovntdqa_m = Shape.(M (V Done))
  let s_movdqa = Shape.(X (X Done))
  let s_movdqa_m = Shape.(M (X Done))
  let s_movdqa_rm = Shape.(X (M Done))
  let s_movdqu = Shape.(X (X Done))
  let s_movdqu_m = Shape.(M (X Done))
  let s_movdqu_rm = Shape.(X (M Done))
  let s_andps = Shape.(X (X Done))
  let s_andps_m = Shape.(M (X Done))
  let s_andnps = Shape.(X (X Done))
  let s_andnps_m = Shape.(M (X Done))
  let s_orps = Shape.(X (X Done))
  let s_orps_m = Shape.(M (X Done))
  let s_xorps = Shape.(X (X Done))
  let s_xorps_m = Shape.(M (X Done))
  let s_andpd = Shape.(X (X Done))
  let s_andpd_m = Shape.(M (X Done))
  let s_andnpd = Shape.(X (X Done))
  let s_andnpd_m = Shape.(M (X Done))
  let s_orpd = Shape.(X (X Done))
  let s_orpd_m = Shape.(M (X Done))
  let s_movaps = Shape.(X (X Done))
  let s_movaps_m = Shape.(M (X Done))
  let s_movaps_rm = Shape.(X (M Done))
  let s_movups = Shape.(X (X Done))
  let s_movups_m = Shape.(M (X Done))
  let s_movups_rm = Shape.(X (M Done))
  let s_movupd = Shape.(X (X Done))
  let s_movupd_m = Shape.(M (X Done))
  let s_movupd_rm = Shape.(X (M Done))
  let s_addps = Shape.(X (X Done))
  let s_addps_m = Shape.(M (X Done))
  let s_subps = Shape.(X (X Done))
  let s_subps_m = Shape.(M (X Done))
  let s_mulps = Shape.(X (X Done))
  let s_mulps_m = Shape.(M (X Done))
  let s_divps = Shape.(X (X Done))
  let s_divps_m = Shape.(M (X Done))
  let s_addpd = Shape.(X (X Done))
  let s_addpd_m = Shape.(M (X Done))
  let s_subpd = Shape.(X (X Done))
  let s_subpd_m = Shape.(M (X Done))
  let s_mulpd = Shape.(X (X Done))
  let s_mulpd_m = Shape.(M (X Done))
  let s_divpd = Shape.(X (X Done))
  let s_divpd_m = Shape.(M (X Done))
  let s_maxss = Shape.(X (X Done))
  let s_maxss_m = Shape.(M (X Done))
  let s_minss = Shape.(X (X Done))
  let s_minss_m = Shape.(M (X Done))
  let s_maxsd = Shape.(X (X Done))
  let s_maxsd_m = Shape.(M (X Done))
  let s_minsd = Shape.(X (X Done))
  let s_minsd_m = Shape.(M (X Done))
  let s_maxps = Shape.(X (X Done))
  let s_maxps_m = Shape.(M (X Done))
  let s_minps = Shape.(X (X Done))
  let s_minps_m = Shape.(M (X Done))
  let s_maxpd = Shape.(X (X Done))
  let s_maxpd_m = Shape.(M (X Done))
  let s_minpd = Shape.(X (X Done))
  let s_minpd_m = Shape.(M (X Done))
  let s_sqrtss = Shape.(X (X Done))
  let s_sqrtss_m = Shape.(M (X Done))
  let s_sqrtsd = Shape.(X (X Done))
  let s_sqrtsd_m = Shape.(M (X Done))
  let s_sqrtps = Shape.(X (X Done))
  let s_sqrtps_m = Shape.(M (X Done))
  let s_sqrtpd = Shape.(X (X Done))
  let s_sqrtpd_m = Shape.(M (X Done))
  let s_shufps_i = Shape.(I (X (X Done)))
  let s_shufps_im = Shape.(I (M (X Done)))
  let s_shufpd_i = Shape.(I (X (X Done)))
  let s_shufpd_im = Shape.(I (M (X Done)))
  let s_cmpss_i = Shape.(I (X (X Done)))
  let s_cmpss_im = Shape.(I (M (X Done)))
  let s_cmpsd_i = Shape.(I (X (X Done)))
  let s_cmpsd_im = Shape.(I (M (X Done)))
  let s_cmpps_i = Shape.(I (X (X Done)))
  let s_cmpps_im = Shape.(I (M (X Done)))
  let s_cmppd_i = Shape.(I (X (X Done)))
  let s_cmppd_im = Shape.(I (M (X Done)))
  let s_pshufd_i = Shape.(I (X (X Done)))
  let s_pshufd_im = Shape.(I (M (X Done)))
  let s_pshuflw_i = Shape.(I (X (X Done)))
  let s_pshuflw_im = Shape.(I (M (X Done)))
  let s_pshufhw_i = Shape.(I (X (X Done)))
  let s_pshufhw_im = Shape.(I (M (X Done)))
  let s_movd_q = Shape.(Mm (R32 Done))
  let s_movd_rq = Shape.(R32 (Mm Done))
  let s_movd_rx = Shape.(R32 (X Done))
  let s_movd_x = Shape.(X (R32 Done))
  let s_movd_mq = Shape.(M (Mm Done))
  let s_movd_mx = Shape.(M (X Done))
  let s_movd_qm = Shape.(Mm (M Done))
  let s_movd_xm = Shape.(X (M Done))
  let s_vaddsd = Shape.(X (X (X Done)))
  let s_vaddsd_m = Shape.(M (X (X Done)))
  let s_vsubsd = Shape.(X (X (X Done)))
  let s_vsubsd_m = Shape.(M (X (X Done)))
  let s_vmulsd = Shape.(X (X (X Done)))
  let s_vmulsd_m = Shape.(M (X (X Done)))
  let s_vdivsd = Shape.(X (X (X Done)))
  let s_vdivsd_m = Shape.(M (X (X Done)))
  let s_vaddss = Shape.(X (X (X Done)))
  let s_vaddss_m = Shape.(M (X (X Done)))
  let s_vsubss = Shape.(X (X (X Done)))
  let s_vsubss_m = Shape.(M (X (X Done)))
  let s_vmulss = Shape.(X (X (X Done)))
  let s_vmulss_m = Shape.(M (X (X Done)))
  let s_vdivss = Shape.(X (X (X Done)))
  let s_vdivss_m = Shape.(M (X (X Done)))
  let s_vaddps = Shape.(V (V (V Done)))
  let s_vaddps_m = Shape.(M (V (V Done)))
  let s_vsubps = Shape.(V (V (V Done)))
  let s_vsubps_m = Shape.(M (V (V Done)))
  let s_vmulps = Shape.(V (V (V Done)))
  let s_vmulps_m = Shape.(M (V (V Done)))
  let s_vdivps = Shape.(V (V (V Done)))
  let s_vdivps_m = Shape.(M (V (V Done)))
  let s_vaddpd = Shape.(V (V (V Done)))
  let s_vaddpd_m = Shape.(M (V (V Done)))
  let s_vsubpd = Shape.(V (V (V Done)))
  let s_vsubpd_m = Shape.(M (V (V Done)))
  let s_vmulpd = Shape.(V (V (V Done)))
  let s_vmulpd_m = Shape.(M (V (V Done)))
  let s_vdivpd = Shape.(V (V (V Done)))
  let s_vdivpd_m = Shape.(M (V (V Done)))
  let s_vandps = Shape.(V (V (V Done)))
  let s_vandps_m = Shape.(M (V (V Done)))
  let s_vandnps = Shape.(V (V (V Done)))
  let s_vandnps_m = Shape.(M (V (V Done)))
  let s_vorps = Shape.(V (V (V Done)))
  let s_vorps_m = Shape.(M (V (V Done)))
  let s_vxorps = Shape.(V (V (V Done)))
  let s_vxorps_m = Shape.(M (V (V Done)))
  let s_vandpd = Shape.(V (V (V Done)))
  let s_vandpd_m = Shape.(M (V (V Done)))
  let s_vandnpd = Shape.(V (V (V Done)))
  let s_vandnpd_m = Shape.(M (V (V Done)))
  let s_vorpd = Shape.(V (V (V Done)))
  let s_vorpd_m = Shape.(M (V (V Done)))
  let s_vxorpd = Shape.(V (V (V Done)))
  let s_vxorpd_m = Shape.(M (V (V Done)))
  let s_vunpcklps = Shape.(V (V (V Done)))
  let s_vunpcklps_m = Shape.(M (V (V Done)))
  let s_vunpckhps = Shape.(V (V (V Done)))
  let s_vunpckhps_m = Shape.(M (V (V Done)))
  let s_vunpcklpd = Shape.(V (V (V Done)))
  let s_vunpcklpd_m = Shape.(M (V (V Done)))
  let s_vunpckhpd = Shape.(V (V (V Done)))
  let s_vunpckhpd_m = Shape.(M (V (V Done)))
  let s_vpunpcklqdq = Shape.(V (V (V Done)))
  let s_vpunpcklqdq_m = Shape.(M (V (V Done)))
  let s_vpunpckhqdq = Shape.(V (V (V Done)))
  let s_vpunpckhqdq_m = Shape.(M (V (V Done)))
  let s_vpunpcklbw = Shape.(V (V (V Done)))
  let s_vpunpcklbw_m = Shape.(M (V (V Done)))
  let s_vpunpckhbw = Shape.(V (V (V Done)))
  let s_vpunpckhbw_m = Shape.(M (V (V Done)))
  let s_vpunpcklwd = Shape.(V (V (V Done)))
  let s_vpunpcklwd_m = Shape.(M (V (V Done)))
  let s_vpunpckhwd = Shape.(V (V (V Done)))
  let s_vpunpckhwd_m = Shape.(M (V (V Done)))
  let s_vpunpckldq = Shape.(V (V (V Done)))
  let s_vpunpckldq_m = Shape.(M (V (V Done)))
  let s_vpunpckhdq = Shape.(V (V (V Done)))
  let s_vpunpckhdq_m = Shape.(M (V (V Done)))
  let s_vpaddb = Shape.(V (V (V Done)))
  let s_vpaddb_m = Shape.(M (V (V Done)))
  let s_vpaddw = Shape.(V (V (V Done)))
  let s_vpaddw_m = Shape.(M (V (V Done)))
  let s_vpaddd = Shape.(V (V (V Done)))
  let s_vpaddd_m = Shape.(M (V (V Done)))
  let s_vpaddq = Shape.(V (V (V Done)))
  let s_vpaddq_m = Shape.(M (V (V Done)))
  let s_vpsubb = Shape.(V (V (V Done)))
  let s_vpsubb_m = Shape.(M (V (V Done)))
  let s_vpsubw = Shape.(V (V (V Done)))
  let s_vpsubw_m = Shape.(M (V (V Done)))
  let s_vpsubd = Shape.(V (V (V Done)))
  let s_vpsubd_m = Shape.(M (V (V Done)))
  let s_vpsubq = Shape.(V (V (V Done)))
  let s_vpsubq_m = Shape.(M (V (V Done)))
  let s_vpcmpeqb = Shape.(V (V (V Done)))
  let s_vpcmpeqb_rrk = Shape.(V (V (K Done)))
  let s_vpcmpeqb_m = Shape.(M (V (V Done)))
  let s_vpcmpeqb_mrk = Shape.(M (V (K Done)))
  let s_vpcmpeqw = Shape.(V (V (V Done)))
  let s_vpcmpeqw_rrk = Shape.(V (V (K Done)))
  let s_vpcmpeqw_m = Shape.(M (V (V Done)))
  let s_vpcmpeqw_mrk = Shape.(M (V (K Done)))
  let s_vpcmpeqd = Shape.(V (V (V Done)))
  let s_vpcmpeqd_rrk = Shape.(V (V (K Done)))
  let s_vpcmpeqd_m = Shape.(M (V (V Done)))
  let s_vpcmpeqd_mrk = Shape.(M (V (K Done)))
  let s_vpcmpgtb = Shape.(V (V (V Done)))
  let s_vpcmpgtb_rrk = Shape.(V (V (K Done)))
  let s_vpcmpgtb_m = Shape.(M (V (V Done)))
  let s_vpcmpgtb_mrk = Shape.(M (V (K Done)))
  let s_vpcmpgtw = Shape.(V (V (V Done)))
  let s_vpcmpgtw_rrk = Shape.(V (V (K Done)))
  let s_vpcmpgtw_m = Shape.(M (V (V Done)))
  let s_vpcmpgtw_mrk = Shape.(M (V (K Done)))
  let s_vpcmpgtd = Shape.(V (V (V Done)))
  let s_vpcmpgtd_rrk = Shape.(V (V (K Done)))
  let s_vpcmpgtd_m = Shape.(M (V (V Done)))
  let s_vpcmpgtd_mrk = Shape.(M (V (K Done)))
  let s_vpacksswb = Shape.(V (V (V Done)))
  let s_vpacksswb_m = Shape.(M (V (V Done)))
  let s_vpackssdw = Shape.(V (V (V Done)))
  let s_vpackssdw_m = Shape.(M (V (V Done)))
  let s_vpackuswb = Shape.(V (V (V Done)))
  let s_vpackuswb_m = Shape.(M (V (V Done)))
  let s_vpand = Shape.(V (V (V Done)))
  let s_vpand_m = Shape.(M (V (V Done)))
  let s_vpandn = Shape.(V (V (V Done)))
  let s_vpandn_m = Shape.(M (V (V Done)))
  let s_vpor = Shape.(V (V (V Done)))
  let s_vpor_m = Shape.(M (V (V Done)))
  let s_vpminub = Shape.(V (V (V Done)))
  let s_vpminub_m = Shape.(M (V (V Done)))
  let s_vpmaxub = Shape.(V (V (V Done)))
  let s_vpmaxub_m = Shape.(M (V (V Done)))
  let s_vpminsw = Shape.(V (V (V Done)))
  let s_vpminsw_m = Shape.(M (V (V Done)))
  let s_vpmaxsw = Shape.(V (V (V Done)))
  let s_vpmaxsw_m = Shape.(M (V (V Done)))
  let s_vmaxsd = Shape.(X (X (X Done)))
  let s_vmaxsd_m = Shape.(M (X (X Done)))
  let s_vminsd = Shape.(X (X (X Done)))
  let s_vminsd_m = Shape.(M (X (X Done)))
  let s_vmaxss = Shape.(X (X (X Done)))
  let s_vmaxss_m = Shape.(M (X (X Done)))
  let s_vminss = Shape.(X (X (X Done)))
  let s_vminss_m = Shape.(M (X (X Done)))
  let s_vmaxps = Shape.(V (V (V Done)))
  let s_vmaxps_m = Shape.(M (V (V Done)))
  let s_vminps = Shape.(V (V (V Done)))
  let s_vminps_m = Shape.(M (V (V Done)))
  let s_vmaxpd = Shape.(V (V (V Done)))
  let s_vmaxpd_m = Shape.(M (V (V Done)))
  let s_vminpd = Shape.(V (V (V Done)))
  let s_vminpd_m = Shape.(M (V (V Done)))
  let s_vsqrtsd = Shape.(X (X (X Done)))
  let s_vsqrtsd_m = Shape.(M (X (X Done)))
  let s_vsqrtss = Shape.(X (X (X Done)))
  let s_vsqrtss_m = Shape.(M (X (X Done)))
  let s_vsqrtps = Shape.(V (V Done))
  let s_vsqrtps_m = Shape.(M (V Done))
  let s_vsqrtpd = Shape.(V (V Done))
  let s_vsqrtpd_m = Shape.(M (V Done))
  let s_vmovaps = Shape.(V (V Done))
  let s_vmovaps_rm = Shape.(V (M Done))
  let s_vmovaps_m = Shape.(M (V Done))
  let s_vmovups = Shape.(V (V Done))
  let s_vmovups_rm = Shape.(V (M Done))
  let s_vmovups_m = Shape.(M (V Done))
  let s_vmovapd = Shape.(V (V Done))
  let s_vmovapd_rm = Shape.(V (M Done))
  let s_vmovapd_m = Shape.(M (V Done))
  let s_vmovupd = Shape.(V (V Done))
  let s_vmovupd_rm = Shape.(V (M Done))
  let s_vmovupd_m = Shape.(M (V Done))
  let s_vcomisd = Shape.(X (X Done))
  let s_vcomisd_m = Shape.(M (X Done))
  let s_vucomisd = Shape.(X (X Done))
  let s_vucomisd_m = Shape.(M (X Done))
  let s_vcomiss = Shape.(X (X Done))
  let s_vcomiss_m = Shape.(M (X Done))
  let s_vucomiss = Shape.(X (X Done))
  let s_vucomiss_m = Shape.(M (X Done))
  let s_vcvtps2pd = Shape.(X (V Done))
  let s_vcvtps2pd_rr = Shape.(Y (Z Done))
  let s_vcvtps2pd_m = Shape.(M (V Done))
  let s_vcvtpd2ps = Shape.(X (X Done))
  let s_vcvtpd2ps_rr = Shape.(Y (X Done))
  let s_vcvtpd2ps_zy = Shape.(Z (Y Done))
  let s_vcvtpd2ps_m = Shape.(M (Y Done))
  let s_vcvtdq2ps = Shape.(V (V Done))
  let s_vcvtdq2ps_m = Shape.(M (V Done))
  let s_vcvtps2dq = Shape.(V (V Done))
  let s_vcvtps2dq_m = Shape.(M (V Done))
  let s_vcvttps2dq = Shape.(V (V Done))
  let s_vcvttps2dq_m = Shape.(M (V Done))
  let s_vshufps_i = Shape.(I (V (V (V Done))))
  let s_vshufps_im = Shape.(I (M (V (V Done))))
  let s_vshufpd_i = Shape.(I (V (V (V Done))))
  let s_vshufpd_im = Shape.(I (M (V (V Done))))
  let s_vcmpss_i = Shape.(I (X (X (X Done))))
  let s_vcmpss_im = Shape.(I (M (X (X Done))))
  let s_vcmpsd_i = Shape.(I (X (X (X Done))))
  let s_vcmpsd_im = Shape.(I (M (X (X Done))))
  let s_vcmpps_i = Shape.(I (V (V (V Done))))
  let s_vcmpps_im = Shape.(I (M (V (V Done))))
  let s_vcmppd_i = Shape.(I (V (V (V Done))))
  let s_vcmppd_im = Shape.(I (M (V (V Done))))
  let s_vpshufd_i = Shape.(I (V (V Done)))
  let s_vpshufd_im = Shape.(I (M (V Done)))
  let s_vpshuflw_i = Shape.(I (V (V Done)))
  let s_vpshuflw_im = Shape.(I (M (V Done)))
  let s_vpshufhw_i = Shape.(I (V (V Done)))
  let s_vpshufhw_im = Shape.(I (M (V Done)))
  let s_vmovd = Shape.(R32 (X Done))
  let s_vmovd_rr = Shape.(X (R32 Done))
  let s_vmovd_xx = Shape.(X (X Done))
  let s_vmovd_m = Shape.(M (X Done))
  let s_vmovd_rm = Shape.(X (M Done))
  let s_vpmullw = Shape.(V (V (V Done)))
  let s_vpmullw_m = Shape.(M (V (V Done)))
  let s_vpmulhw = Shape.(V (V (V Done)))
  let s_vpmulhw_m = Shape.(M (V (V Done)))
  let s_vpmulhuw = Shape.(V (V (V Done)))
  let s_vpmulhuw_m = Shape.(M (V (V Done)))
  let s_vpavgb = Shape.(V (V (V Done)))
  let s_vpavgb_m = Shape.(M (V (V Done)))
  let s_vpavgw = Shape.(V (V (V Done)))
  let s_vpavgw_m = Shape.(M (V (V Done)))
  let s_vpsadbw = Shape.(V (V (V Done)))
  let s_vpsadbw_m = Shape.(M (V (V Done)))
  let s_vpsllw = Shape.(X (V (V Done)))
  let s_vpsllw_i = Shape.(I (V (V Done)))
  let s_vpsllw_m = Shape.(M (V (V Done)))
  let s_vpsllw_im = Shape.(I (M (V Done)))
  let s_vpslld = Shape.(X (V (V Done)))
  let s_vpslld_i = Shape.(I (V (V Done)))
  let s_vpslld_m = Shape.(M (V (V Done)))
  let s_vpslld_im = Shape.(I (M (V Done)))
  let s_vpsllq = Shape.(X (V (V Done)))
  let s_vpsllq_i = Shape.(I (V (V Done)))
  let s_vpsllq_m = Shape.(M (V (V Done)))
  let s_vpsllq_im = Shape.(I (M (V Done)))
  let s_vpsrlw = Shape.(X (V (V Done)))
  let s_vpsrlw_i = Shape.(I (V (V Done)))
  let s_vpsrlw_m = Shape.(M (V (V Done)))
  let s_vpsrlw_im = Shape.(I (M (V Done)))
  let s_vpsrld = Shape.(X (V (V Done)))
  let s_vpsrld_i = Shape.(I (V (V Done)))
  let s_vpsrld_m = Shape.(M (V (V Done)))
  let s_vpsrld_im = Shape.(I (M (V Done)))
  let s_vpsrlq = Shape.(X (V (V Done)))
  let s_vpsrlq_i = Shape.(I (V (V Done)))
  let s_vpsrlq_m = Shape.(M (V (V Done)))
  let s_vpsrlq_im = Shape.(I (M (V Done)))
  let s_vpsraw = Shape.(X (V (V Done)))
  let s_vpsraw_i = Shape.(I (V (V Done)))
  let s_vpsraw_m = Shape.(M (V (V Done)))
  let s_vpsraw_im = Shape.(I (M (V Done)))
  let s_vpsrad = Shape.(X (V (V Done)))
  let s_vpsrad_i = Shape.(I (V (V Done)))
  let s_vpsrad_m = Shape.(M (V (V Done)))
  let s_vpsrad_im = Shape.(I (M (V Done)))
  let s_vpslldq_i = Shape.(I (V (V Done)))
  let s_vpslldq_im = Shape.(I (M (V Done)))
  let s_vpsrldq_i = Shape.(I (V (V Done)))
  let s_vpsrldq_im = Shape.(I (M (V Done)))
  let s_vmovdqa = Shape.(V (V Done))
  let s_vmovdqa_rm = Shape.(V (M Done))
  let s_vmovdqa_m = Shape.(M (V Done))
  let s_vmovdqu = Shape.(V (V Done))
  let s_vmovdqu_rm = Shape.(V (M Done))
  let s_vmovdqu_m = Shape.(M (V Done))
  let s_pinsrw_irq = Shape.(I (R32 (Mm Done)))
  let s_pinsrw_irx = Shape.(I (R32 (X Done)))
  let s_pinsrw_imq = Shape.(I (M (Mm Done)))
  let s_pinsrw_imx = Shape.(I (M (X Done)))
  let s_pextrw_iq = Shape.(I (Mm (R32 Done)))
  let s_pextrw_i = Shape.(I (X (R32 Done)))
  let s_pextrw_irm = Shape.(I (X (M Done)))
  let s_vpinsrw_i = Shape.(I (R32 (X (X Done))))
  let s_vpinsrw_im = Shape.(I (M (X (X Done))))
  let s_vpextrw_i = Shape.(I (X (R32 Done)))
  let s_vpextrw_irm = Shape.(I (X (M Done)))
  let s_movmskps = Shape.(X (R32 Done))
  let s_movmskpd = Shape.(X (R32 Done))
  let s_pmovmskb_q = Shape.(Mm (R32 Done))
  let s_pmovmskb_x = Shape.(X (R32 Done))
  let s_vmovmskps = Shape.(V (R32 Done))
  let s_vmovmskpd = Shape.(V (R32 Done))
  let s_vpmovmskb = Shape.(V (R32 Done))
  let s_fldl_m = Shape.(M Done)
  let s_fstpl_m = Shape.(M Done)
  let s_fstps_m = Shape.(M Done)
  let s_flds = Shape.(S Done)
  let s_flds_m = Shape.(M Done)
  let s_fildll_m = Shape.(M Done)
  let s_fadds = Shape.(S Done)
  let s_fadds_m = Shape.(M Done)
  let s_fadd = Shape.(St0 (Sti Done))
  let s_fadd_r = Shape.(Sti (St0 Done))
  let s_fnstcw_m = Shape.(M Done)
  let s_fldcw_m = Shape.(M Done)
  let s_fistpll_m = Shape.(M Done)
  let s_fsubs = Shape.(S Done)
  let s_fsubs_m = Shape.(M Done)
  let s_fnstsw = Shape.(Ax Done)
  let s_fnstsw_m = Shape.(M Done)
  let s_sahf = Shape.(Done)
  let s_cmovo = Shape.(R (R Done))
  let s_cmovo_m = Shape.(M (R Done))
  let s_jo_m32 = Shape.(S Done)
  let s_jo_m64 = Shape.(S Done)
  let s_seto = Shape.(R8 Done)
  let s_seto_m = Shape.(M Done)
  let s_cmovno = Shape.(R (R Done))
  let s_cmovno_m = Shape.(M (R Done))
  let s_jno_m32 = Shape.(S Done)
  let s_jno_m64 = Shape.(S Done)
  let s_setno = Shape.(R8 Done)
  let s_setno_m = Shape.(M Done)
  let s_cmovb = Shape.(R (R Done))
  let s_cmovb_m = Shape.(M (R Done))
  let s_jb_m32 = Shape.(S Done)
  let s_jb_m64 = Shape.(S Done)
  let s_setb = Shape.(R8 Done)
  let s_setb_m = Shape.(M Done)
  let s_cmovae = Shape.(R (R Done))
  let s_cmovae_m = Shape.(M (R Done))
  let s_jae_m32 = Shape.(S Done)
  let s_jae_m64 = Shape.(S Done)
  let s_setae = Shape.(R8 Done)
  let s_setae_m = Shape.(M Done)
  let s_cmove = Shape.(R (R Done))
  let s_cmove_m = Shape.(M (R Done))
  let s_je_m32 = Shape.(S Done)
  let s_je_m64 = Shape.(S Done)
  let s_sete = Shape.(R8 Done)
  let s_sete_m = Shape.(M Done)
  let s_cmovne = Shape.(R (R Done))
  let s_cmovne_m = Shape.(M (R Done))
  let s_jne_m32 = Shape.(S Done)
  let s_jne_m64 = Shape.(S Done)
  let s_setne = Shape.(R8 Done)
  let s_setne_m = Shape.(M Done)
  let s_cmovbe = Shape.(R (R Done))
  let s_cmovbe_m = Shape.(M (R Done))
  let s_jbe_m32 = Shape.(S Done)
  let s_jbe_m64 = Shape.(S Done)
  let s_setbe = Shape.(R8 Done)
  let s_setbe_m = Shape.(M Done)
  let s_cmova = Shape.(R (R Done))
  let s_cmova_m = Shape.(M (R Done))
  let s_ja_m32 = Shape.(S Done)
  let s_ja_m64 = Shape.(S Done)
  let s_seta = Shape.(R8 Done)
  let s_seta_m = Shape.(M Done)
  let s_cmovs = Shape.(R (R Done))
  let s_cmovs_m = Shape.(M (R Done))
  let s_js_m32 = Shape.(S Done)
  let s_js_m64 = Shape.(S Done)
  let s_sets = Shape.(R8 Done)
  let s_sets_m = Shape.(M Done)
  let s_cmovns = Shape.(R (R Done))
  let s_cmovns_m = Shape.(M (R Done))
  let s_jns_m32 = Shape.(S Done)
  let s_jns_m64 = Shape.(S Done)
  let s_setns = Shape.(R8 Done)
  let s_setns_m = Shape.(M Done)
  let s_cmovp = Shape.(R (R Done))
  let s_cmovp_m = Shape.(M (R Done))
  let s_jp_m32 = Shape.(S Done)
  let s_jp_m64 = Shape.(S Done)
  let s_setp = Shape.(R8 Done)
  let s_setp_m = Shape.(M Done)
  let s_cmovnp = Shape.(R (R Done))
  let s_cmovnp_m = Shape.(M (R Done))
  let s_jnp_m32 = Shape.(S Done)
  let s_jnp_m64 = Shape.(S Done)
  let s_setnp = Shape.(R8 Done)
  let s_setnp_m = Shape.(M Done)
  let s_cmovl = Shape.(R (R Done))
  let s_cmovl_m = Shape.(M (R Done))
  let s_jl_m32 = Shape.(S Done)
  let s_jl_m64 = Shape.(S Done)
  let s_setl = Shape.(R8 Done)
  let s_setl_m = Shape.(M Done)
  let s_cmovge = Shape.(R (R Done))
  let s_cmovge_m = Shape.(M (R Done))
  let s_jge_m32 = Shape.(S Done)
  let s_jge_m64 = Shape.(S Done)
  let s_setge = Shape.(R8 Done)
  let s_setge_m = Shape.(M Done)
  let s_cmovle = Shape.(R (R Done))
  let s_cmovle_m = Shape.(M (R Done))
  let s_jle_m32 = Shape.(S Done)
  let s_jle_m64 = Shape.(S Done)
  let s_setle = Shape.(R8 Done)
  let s_setle_m = Shape.(M Done)
  let s_cmovg = Shape.(R (R Done))
  let s_cmovg_m = Shape.(M (R Done))
  let s_jg_m32 = Shape.(S Done)
  let s_jg_m64 = Shape.(S Done)
  let s_setg = Shape.(R8 Done)
  let s_setg_m = Shape.(M Done)
  let s_loop_m32 = Shape.(S Done)
  let s_loop_m64 = Shape.(S Done)
  let s_loope_m32 = Shape.(S Done)
  let s_loope_m64 = Shape.(S Done)
  let s_loopne_m32 = Shape.(S Done)
  let s_loopne_m64 = Shape.(S Done)
  let s_jcxz = Shape.(S Done)
  let s_jecxz_m32 = Shape.(S Done)
  let s_jecxz_m64 = Shape.(S Done)
  let s_jrcxz = Shape.(S Done)
  let s_movzb = Shape.(R8 (R Done))
  let s_movzb_m = Shape.(M (R Done))
  let s_movzw = Shape.(R16 (R Done))
  let s_movzw_m = Shape.(M (R Done))
  let s_movsb = Shape.(R8 (R Done))
  let s_movsb_m = Shape.(M (R Done))
  let s_movsw = Shape.(R16 (R Done))
  let s_movsw_m = Shape.(M (R Done))
  let s_movsl = Shape.(R32 (R64 Done))
  let s_movsl_m = Shape.(M (R64 Done))
end

(* {1 Definitions} *)

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x01); (Form.Alu_r_rm, 0x03); (Form.Alu_ext, 0x00) ]
    ~writes_flags:true Opcode.Add
    [
      b ~widths:[ 8; 32; 64 ] "add" "add%s" Opcode.Add S.s_add;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "add_rrr" "add%s" Opcode.Add S.s_add_rrr;
      b ~widths:[ 8; 32; 64 ] "add_rm" "add%s" Opcode.Add S.s_add_rm;
      b ~widths:[ 8; 32; 64 ] "add_i" "add%s" Opcode.Add S.s_add_i;
      b ~widths:[ 8; 32; 64 ] "add_m" "add%s" Opcode.Add S.s_add_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "add_rmr" "add%s" Opcode.Add S.s_add_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "add_irr" "add%s" Opcode.Add S.s_add_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "add_mrr" "add%s" Opcode.Add S.s_add_mrr;
      b ~width:8 "addb_im" "addb" Opcode.Add S.s_addb_im;
      b ~width:32 "addl_im" "addl" Opcode.Add S.s_addl_im;
      b ~width:64 ~mode:Only64 "addq_im" "addq" Opcode.Add S.s_addq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "add_im" "add%s" Opcode.Add S.s_add_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x29); (Form.Alu_r_rm, 0x2b); (Form.Alu_ext, 0x05) ]
    ~writes_flags:true Opcode.Sub
    [
      b ~widths:[ 8; 32; 64 ] "sub" "sub%s" Opcode.Sub S.s_sub;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sub_rrr" "sub%s" Opcode.Sub S.s_sub_rrr;
      b ~widths:[ 8; 32; 64 ] "sub_rm" "sub%s" Opcode.Sub S.s_sub_rm;
      b ~widths:[ 8; 32; 64 ] "sub_i" "sub%s" Opcode.Sub S.s_sub_i;
      b ~widths:[ 8; 32; 64 ] "sub_m" "sub%s" Opcode.Sub S.s_sub_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sub_rmr" "sub%s" Opcode.Sub S.s_sub_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sub_irr" "sub%s" Opcode.Sub S.s_sub_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sub_mrr" "sub%s" Opcode.Sub S.s_sub_mrr;
      b ~width:8 "subb_im" "subb" Opcode.Sub S.s_subb_im;
      b ~width:32 "subl_im" "subl" Opcode.Sub S.s_subl_im;
      b ~width:64 ~mode:Only64 "subq_im" "subq" Opcode.Sub S.s_subq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sub_im" "sub%s" Opcode.Sub S.s_sub_im;
    ]

let () =
  def Opcode.Mov
    [
      b ~widths:[ 8; 16; 32; 64 ] "mov" "mov%s" Opcode.Mov S.s_mov;
      b ~width:64 ~mode:Only64 "mov_qq" "mov%s" Opcode.Mov S.s_mov_qq;
      b ~width:64 ~mode:Only64 "mov_q" "movq" Opcode.Mov S.s_mov_q;
      b ~width:64 ~mode:Only64 "mov_rq" "movq" Opcode.Mov S.s_mov_rq;
      b ~widths:[ 8; 16; 32; 64 ] "mov_rm" "mov%s" Opcode.Mov S.s_mov_rm;
      b ~widths:[ 8; 16; 32; 64 ] "mov_rs" "mov%s" Opcode.Mov S.s_mov_rs;
      b ~widths:[ 8; 16; 32; 64 ] "mov_i" "mov%s" Opcode.Mov S.s_mov_i;
      b ~widths:[ 8; 16; 32; 64 ] "mov_m" "mov%s" Opcode.Mov S.s_mov_m;
      b ~width:64 ~mode:Only64 "mov_mq" "mov%s" Opcode.Mov S.s_mov_mq;
      b ~width:64 ~mode:Only64 "mov_mx" "mov%s" Opcode.Mov S.s_mov_mx;
      b ~width:64 ~mode:Only64 "mov_qm" "mov%s" Opcode.Mov S.s_mov_qm;
      b ~widths:[ 8; 16; 32; 64 ] "mov_s" "mov%s" Opcode.Mov S.s_mov_s;
      b ~width:64 ~mode:Only64 "mov_xm" "mov%s" Opcode.Mov S.s_mov_xm;
      b ~width:8 "movb_im" "movb" Opcode.Mov S.s_movb_im;
      b ~width:16 "movw_im" "movw" Opcode.Mov S.s_movw_im;
      b ~width:32 "movl_im" "movl" Opcode.Mov S.s_movl_im;
      b ~width:64 ~mode:Only64 "movq_im" "movq" Opcode.Mov S.s_movq_im;
    ]

let () =
  def Opcode.Lea
    [
      b ~widths:[ 32; 64 ] "lea_m" "lea%s" Opcode.Lea S.s_lea_m;
      b ~widths:[ 32; 64 ] "lea_s" "lea%s" Opcode.Lea S.s_lea_s;
    ]

let () =
  def Opcode.Ret
    [
      b ~width:32 ~mode:Only32 "ret_m32" "ret" Opcode.Ret S.s_ret_m32;
      b ~width:64 ~mode:Only64 "ret_m64" "ret" Opcode.Ret S.s_ret_m64;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x31); (Form.Alu_r_rm, 0x33); (Form.Alu_ext, 0x06) ]
    ~writes_flags:true Opcode.Xor
    [
      b ~widths:[ 8; 32; 64 ] "xor" "xor%s" Opcode.Xor S.s_xor;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "xor_rrr" "xor%s" Opcode.Xor S.s_xor_rrr;
      b ~widths:[ 8; 32; 64 ] "xor_rm" "xor%s" Opcode.Xor S.s_xor_rm;
      b ~widths:[ 8; 32; 64 ] "xor_i" "xor%s" Opcode.Xor S.s_xor_i;
      b ~widths:[ 8; 32; 64 ] "xor_m" "xor%s" Opcode.Xor S.s_xor_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "xor_rmr" "xor%s" Opcode.Xor S.s_xor_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "xor_irr" "xor%s" Opcode.Xor S.s_xor_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "xor_mrr" "xor%s" Opcode.Xor S.s_xor_mrr;
      b ~width:8 "xorb_im" "xorb" Opcode.Xor S.s_xorb_im;
      b ~width:32 "xorl_im" "xorl" Opcode.Xor S.s_xorl_im;
      b ~width:64 ~mode:Only64 "xorq_im" "xorq" Opcode.Xor S.s_xorq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "xor_im" "xor%s" Opcode.Xor S.s_xor_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x21); (Form.Alu_r_rm, 0x23); (Form.Alu_ext, 0x04) ]
    ~writes_flags:true Opcode.And
    [
      b ~widths:[ 8; 32; 64 ] "and_" "and%s" Opcode.And S.s_and_;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "and_rrr" "and%s" Opcode.And S.s_and_rrr;
      b ~widths:[ 8; 32; 64 ] "and_rm" "and%s" Opcode.And S.s_and_rm;
      b ~widths:[ 8; 32; 64 ] "and_i" "and%s" Opcode.And S.s_and_i;
      b ~widths:[ 8; 32; 64 ] "and_m" "and%s" Opcode.And S.s_and_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "and_rmr" "and%s" Opcode.And S.s_and_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "and_irr" "and%s" Opcode.And S.s_and_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "and_mrr" "and%s" Opcode.And S.s_and_mrr;
      b ~width:8 "andb_im" "andb" Opcode.And S.s_andb_im;
      b ~width:32 "andl_im" "andl" Opcode.And S.s_andl_im;
      b ~width:64 ~mode:Only64 "andq_im" "andq" Opcode.And S.s_andq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "and_im" "and%s" Opcode.And S.s_and_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x39); (Form.Alu_r_rm, 0x3b); (Form.Alu_ext, 0x07) ]
    ~writes_flags:true Opcode.Cmp
    [
      b ~widths:[ 8; 32; 64 ] "cmp" "cmp%s" Opcode.Cmp S.s_cmp;
      b ~widths:[ 8; 32; 64 ] "cmp_rm" "cmp%s" Opcode.Cmp S.s_cmp_rm;
      b ~widths:[ 8; 32; 64 ] "cmp_i" "cmp%s" Opcode.Cmp S.s_cmp_i;
      b ~widths:[ 8; 32; 64 ] "cmp_m" "cmp%s" Opcode.Cmp S.s_cmp_m;
      b ~width:8 "cmpb_im" "cmpb" Opcode.Cmp S.s_cmpb_im;
      b ~width:32 "cmpl_im" "cmpl" Opcode.Cmp S.s_cmpl_im;
      b ~width:64 ~mode:Only64 "cmpq_im" "cmpq" Opcode.Cmp S.s_cmpq_im;
    ]

let () =
  def ~writes_flags:true Opcode.Imul
    [
      b ~widths:[ 32; 64 ] "imul" "imul%s" Opcode.Imul S.s_imul;
      b ~widths:[ 32; 64 ] "imul_rr" "imul%s" Opcode.Imul S.s_imul_rr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "imul_rrr" "imul%s" Opcode.Imul S.s_imul_rrr;
      b ~width:32 "imull_m" "imull" Opcode.Imul S.s_imull_m;
      b ~width:64 ~mode:Only64 "imulq_m" "imulq" Opcode.Imul S.s_imulq_m;
      b ~widths:[ 32; 64 ] "imul_m" "imul%s" Opcode.Imul S.s_imul_m;
      b ~widths:[ 32; 64 ] "imul_s" "imul%s" Opcode.Imul S.s_imul_s;
      b ~widths:[ 32; 64 ] "imul_i" "imul%s" Opcode.Imul S.s_imul_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "imul_mrr" "imul%s" Opcode.Imul S.s_imul_mrr;
      b ~widths:[ 32; 64 ] "imul_im" "imul%s" Opcode.Imul S.s_imul_im;
    ]

let () =
  def Opcode.Ud2
    [
      b ~width:32 ~mode:Only32 "ud2_m32" "ud2" Opcode.Ud2 S.s_ud2_m32;
      b ~width:64 ~mode:Only64 "ud2_m64" "ud2" Opcode.Ud2 S.s_ud2_m64;
    ]

let () =
  def Opcode.Pop
    [
      b ~width:32 ~mode:Only32 "pop_m32" "popl" Opcode.Pop S.s_pop_m32;
      b ~width:64 ~mode:Only64 "pop_m64" "popq" Opcode.Pop S.s_pop_m64;
      b ~width:32 ~mode:Only32 "pop_m_m32" "pop%s" Opcode.Pop S.s_pop_m_m32;
      b ~width:64 ~mode:Only64 "pop_m_m64" "pop%s" Opcode.Pop S.s_pop_m_m64;
    ]

let () = def Opcode.Bswap [ b ~widths:[ 32; 64 ] "bswap" "bswap" Opcode.Bswap S.s_bswap ]

let () =
  def Opcode.Jmp
    [
      b ~width:32 ~mode:Only32 "jmp_m32" "jmp" Opcode.Jmp S.s_jmp_m32;
      b ~width:64 ~mode:Only64 "jmp_m64" "jmp" Opcode.Jmp S.s_jmp_m64;
      b ~width:32 ~mode:Only32 "jmp_r_m32" "jmp" Opcode.Jmp S.s_jmp_r_m32;
      b ~width:64 ~mode:Only64 "jmp_r_m64" "jmp" Opcode.Jmp S.s_jmp_r_m64;
      b ~width:32 ~mode:Only32 "jmp_m_m32" "jmp" Opcode.Jmp S.s_jmp_m_m32;
      b ~width:64 ~mode:Only64 "jmp_m_m64" "jmp" Opcode.Jmp S.s_jmp_m_m64;
    ]

let () =
  def Opcode.Call
    [
      b ~width:32 ~mode:Only32 "call_m32" "call" Opcode.Call S.s_call_m32;
      b ~width:64 ~mode:Only64 "call_m64" "call" Opcode.Call S.s_call_m64;
      b ~width:32 ~mode:Only32 "call_r_m32" "call" Opcode.Call S.s_call_r_m32;
      b ~width:64 ~mode:Only64 "call_r_m64" "call" Opcode.Call S.s_call_r_m64;
      b ~width:32 ~mode:Only32 "call_m_m32" "call" Opcode.Call S.s_call_m_m32;
      b ~width:64 ~mode:Only64 "call_m_m64" "call" Opcode.Call S.s_call_m_m64;
    ]

let () =
  def Opcode.Push
    [
      b ~width:32 ~mode:Only32 "push_m32" "pushl" Opcode.Push S.s_push_m32;
      b ~width:64 ~mode:Only64 "push_m64" "pushq" Opcode.Push S.s_push_m64;
      b ~width:32 ~mode:Only32 "push_i_m32" "push%s" Opcode.Push S.s_push_i_m32;
      b ~width:64 ~mode:Only64 "push_i_m64" "push%s" Opcode.Push S.s_push_i_m64;
    ]

let () =
  def
    ~forms:[ (Form.Unary_ext, 0x03) ]
    ~writes_flags:true Opcode.Neg
    [
      b ~widths:[ 32; 64 ] "neg" "neg%s" Opcode.Neg S.s_neg;
      b ~widths:[ 32; 64 ] ~mode:Only64 "neg_rr" "neg%s" Opcode.Neg S.s_neg_rr;
      b ~width:32 "negl_m" "negl" Opcode.Neg S.s_negl_m;
      b ~width:64 ~mode:Only64 "negq_m" "negq" Opcode.Neg S.s_negq_m;
      b ~widths:[ 32; 64 ] ~mode:Only64 "neg_m" "neg%s" Opcode.Neg S.s_neg_m;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x85) ]
    ~writes_flags:true Opcode.Test
    [
      b ~widths:[ 32; 64 ] "test" "test%s" Opcode.Test S.s_test;
      b ~widths:[ 32; 64 ] "test_rm" "test%s" Opcode.Test S.s_test_rm;
      b ~widths:[ 32; 64 ] "test_i" "test%s" Opcode.Test S.s_test_i;
      b ~width:32 "testl_im" "testl" Opcode.Test S.s_testl_im;
      b ~width:64 ~mode:Only64 "testq_im" "testq" Opcode.Test S.s_testq_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x11); (Form.Alu_r_rm, 0x13); (Form.Alu_ext, 0x02) ]
    ~reads_flags:true ~writes_flags:true Opcode.Adc
    [
      b ~widths:[ 8; 32; 64 ] "adc" "adc%s" Opcode.Adc S.s_adc;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "adc_rrr" "adc%s" Opcode.Adc S.s_adc_rrr;
      b ~widths:[ 8; 32; 64 ] "adc_rm" "adc%s" Opcode.Adc S.s_adc_rm;
      b ~widths:[ 8; 32; 64 ] "adc_i" "adc%s" Opcode.Adc S.s_adc_i;
      b ~widths:[ 8; 32; 64 ] "adc_m" "adc%s" Opcode.Adc S.s_adc_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "adc_rmr" "adc%s" Opcode.Adc S.s_adc_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "adc_irr" "adc%s" Opcode.Adc S.s_adc_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "adc_mrr" "adc%s" Opcode.Adc S.s_adc_mrr;
      b ~width:8 "adcb_im" "adcb" Opcode.Adc S.s_adcb_im;
      b ~width:32 "adcl_im" "adcl" Opcode.Adc S.s_adcl_im;
      b ~width:64 ~mode:Only64 "adcq_im" "adcq" Opcode.Adc S.s_adcq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "adc_im" "adc%s" Opcode.Adc S.s_adc_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x19); (Form.Alu_r_rm, 0x1b); (Form.Alu_ext, 0x03) ]
    ~reads_flags:true ~writes_flags:true Opcode.Sbb
    [
      b ~widths:[ 8; 32; 64 ] "sbb" "sbb%s" Opcode.Sbb S.s_sbb;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sbb_rrr" "sbb%s" Opcode.Sbb S.s_sbb_rrr;
      b ~widths:[ 8; 32; 64 ] "sbb_rm" "sbb%s" Opcode.Sbb S.s_sbb_rm;
      b ~widths:[ 8; 32; 64 ] "sbb_i" "sbb%s" Opcode.Sbb S.s_sbb_i;
      b ~widths:[ 8; 32; 64 ] "sbb_m" "sbb%s" Opcode.Sbb S.s_sbb_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sbb_rmr" "sbb%s" Opcode.Sbb S.s_sbb_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sbb_irr" "sbb%s" Opcode.Sbb S.s_sbb_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sbb_mrr" "sbb%s" Opcode.Sbb S.s_sbb_mrr;
      b ~width:8 "sbbb_im" "sbbb" Opcode.Sbb S.s_sbbb_im;
      b ~width:32 "sbbl_im" "sbbl" Opcode.Sbb S.s_sbbl_im;
      b ~width:64 ~mode:Only64 "sbbq_im" "sbbq" Opcode.Sbb S.s_sbbq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "sbb_im" "sbb%s" Opcode.Sbb S.s_sbb_im;
    ]

let () =
  def
    ~forms:[ (Form.Unary_ext, 0x04) ]
    ~writes_flags:true Opcode.Mul
    [
      b ~widths:[ 32; 64 ] "mul" "mul%s" Opcode.Mul S.s_mul;
      b ~width:32 "mull_m" "mull" Opcode.Mul S.s_mull_m;
      b ~width:64 ~mode:Only64 "mulq_m" "mulq" Opcode.Mul S.s_mulq_m;
    ]

let () =
  def
    ~forms:[ (Form.Unary_ext, 0x06) ]
    ~writes_flags:true Opcode.Div
    [
      b ~widths:[ 32; 64 ] "div" "div%s" Opcode.Div S.s_div;
      b ~width:32 "divl_m" "divl" Opcode.Div S.s_divl_m;
      b ~width:64 ~mode:Only64 "divq_m" "divq" Opcode.Div S.s_divq_m;
    ]

let () =
  def ~mode:Only32 ~writes_flags:true Opcode.Dec
    [
      b ~width:32 ~mode:Only32 "dec" "dec" Opcode.Dec S.s_dec;
      b ~width:32 ~mode:Only32 "dec_m" "dec%s" Opcode.Dec S.s_dec_m;
    ]

let () =
  def
    ~forms:[ (Form.Shift_ext, 0x03) ]
    ~reads_flags:true ~writes_flags:true Opcode.Rcr
    [
      b ~widths:[ 32; 64 ] "rcr" "rcr%s" Opcode.Rcr S.s_rcr;
      b ~width:32 "rcrl_m" "rcrl" Opcode.Rcr S.s_rcrl_m;
      b ~width:64 ~mode:Only64 "rcrq_m" "rcrq" Opcode.Rcr S.s_rcrq_m;
      b ~widths:[ 32; 64 ] "rcr_cl" "rcr%s" Opcode.Rcr S.s_rcr_cl;
      b ~widths:[ 32; 64 ] "rcr_i" "rcr%s" Opcode.Rcr S.s_rcr_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "rcr_clrr" "rcr%s" Opcode.Rcr S.s_rcr_clrr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "rcr_irr" "rcr%s" Opcode.Rcr S.s_rcr_irr;
      b ~width:32 "rcrl_clm" "rcrl" Opcode.Rcr S.s_rcrl_clm;
      b ~width:64 ~mode:Only64 "rcrq_clm" "rcrq" Opcode.Rcr S.s_rcrq_clm;
      b ~width:32 "rcrl_im" "rcrl" Opcode.Rcr S.s_rcrl_im;
      b ~width:64 ~mode:Only64 "rcrq_im" "rcrq" Opcode.Rcr S.s_rcrq_im;
      b ~widths:[ 32; 64 ] ~mode:Only64 "rcr_clm" "rcr%s" Opcode.Rcr S.s_rcr_clm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "rcr_im" "rcr%s" Opcode.Rcr S.s_rcr_im;
    ]

let () =
  def
    ~forms:[ (Form.Shift_ext, 0x05) ]
    ~writes_flags:true Opcode.Shr
    [
      b ~widths:[ 32; 64 ] "shr" "shr%s" Opcode.Shr S.s_shr;
      b ~width:32 "shrl_m" "shrl" Opcode.Shr S.s_shrl_m;
      b ~width:64 ~mode:Only64 "shrq_m" "shrq" Opcode.Shr S.s_shrq_m;
      b ~widths:[ 32; 64 ] "shr_cl" "shr%s" Opcode.Shr S.s_shr_cl;
      b ~widths:[ 32; 64 ] "shr_i" "shr%s" Opcode.Shr S.s_shr_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shr_clrr" "shr%s" Opcode.Shr S.s_shr_clrr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shr_irr" "shr%s" Opcode.Shr S.s_shr_irr;
      b ~width:32 "shrl_clm" "shrl" Opcode.Shr S.s_shrl_clm;
      b ~width:64 ~mode:Only64 "shrq_clm" "shrq" Opcode.Shr S.s_shrq_clm;
      b ~width:32 "shrl_im" "shrl" Opcode.Shr S.s_shrl_im;
      b ~width:64 ~mode:Only64 "shrq_im" "shrq" Opcode.Shr S.s_shrq_im;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shr_clm" "shr%s" Opcode.Shr S.s_shr_clm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shr_im" "shr%s" Opcode.Shr S.s_shr_im;
    ]

let () =
  def
    ~forms:[ (Form.Alu_rm_r, 0x09); (Form.Alu_r_rm, 0x0b); (Form.Alu_ext, 0x01) ]
    ~writes_flags:true Opcode.Or
    [
      b ~widths:[ 8; 32; 64 ] "or_" "or%s" Opcode.Or S.s_or_;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "or_rrr" "or%s" Opcode.Or S.s_or_rrr;
      b ~widths:[ 8; 32; 64 ] "or_rm" "or%s" Opcode.Or S.s_or_rm;
      b ~widths:[ 8; 32; 64 ] "or_i" "or%s" Opcode.Or S.s_or_i;
      b ~widths:[ 8; 32; 64 ] "or_m" "or%s" Opcode.Or S.s_or_m;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "or_rmr" "or%s" Opcode.Or S.s_or_rmr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "or_irr" "or%s" Opcode.Or S.s_or_irr;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "or_mrr" "or%s" Opcode.Or S.s_or_mrr;
      b ~width:8 "orb_im" "orb" Opcode.Or S.s_orb_im;
      b ~width:32 "orl_im" "orl" Opcode.Or S.s_orl_im;
      b ~width:64 ~mode:Only64 "orq_im" "orq" Opcode.Or S.s_orq_im;
      b ~widths:[ 8; 32; 64 ] ~mode:Only64 "or_im" "or%s" Opcode.Or S.s_or_im;
    ]

let () =
  def
    ~forms:[ (Form.Unary_ext, 0x02) ]
    Opcode.Not
    [
      b ~widths:[ 32; 64 ] "not" "not%s" Opcode.Not S.s_not;
      b ~widths:[ 32; 64 ] ~mode:Only64 "not_rr" "not%s" Opcode.Not S.s_not_rr;
      b ~width:32 "notl_m" "notl" Opcode.Not S.s_notl_m;
      b ~width:64 ~mode:Only64 "notq_m" "notq" Opcode.Not S.s_notq_m;
      b ~widths:[ 32; 64 ] ~mode:Only64 "not_m" "not%s" Opcode.Not S.s_not_m;
    ]

let () =
  def
    ~forms:[ (Form.Shift_ext, 0x01) ]
    ~writes_flags:true Opcode.Ror
    [
      b ~widths:[ 32; 64 ] "ror" "ror%s" Opcode.Ror S.s_ror;
      b ~width:32 "rorl_m" "rorl" Opcode.Ror S.s_rorl_m;
      b ~width:64 ~mode:Only64 "rorq_m" "rorq" Opcode.Ror S.s_rorq_m;
      b ~widths:[ 32; 64 ] "ror_cl" "ror%s" Opcode.Ror S.s_ror_cl;
      b ~widths:[ 32; 64 ] "ror_i" "ror%s" Opcode.Ror S.s_ror_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "ror_clrr" "ror%s" Opcode.Ror S.s_ror_clrr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "ror_irr" "ror%s" Opcode.Ror S.s_ror_irr;
      b ~width:32 "rorl_clm" "rorl" Opcode.Ror S.s_rorl_clm;
      b ~width:64 ~mode:Only64 "rorq_clm" "rorq" Opcode.Ror S.s_rorq_clm;
      b ~width:32 "rorl_im" "rorl" Opcode.Ror S.s_rorl_im;
      b ~width:64 ~mode:Only64 "rorq_im" "rorq" Opcode.Ror S.s_rorq_im;
      b ~widths:[ 32; 64 ] ~mode:Only64 "ror_clm" "ror%s" Opcode.Ror S.s_ror_clm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "ror_im" "ror%s" Opcode.Ror S.s_ror_im;
    ]

let () =
  def
    ~forms:[ (Form.Shift_ext, 0x04) ]
    ~writes_flags:true Opcode.Shl
    [
      b ~widths:[ 32; 64 ] "shl" "sal%s" Opcode.Shl S.s_shl;
      b ~width:32 "shll_m" "sall" Opcode.Shl S.s_shll_m;
      b ~width:64 ~mode:Only64 "shlq_m" "salq" Opcode.Shl S.s_shlq_m;
      b ~widths:[ 32; 64 ] "shl_cl" "sal%s" Opcode.Shl S.s_shl_cl;
      b ~widths:[ 32; 64 ] "shl_i" "sal%s" Opcode.Shl S.s_shl_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shl_clrr" "sal%s" Opcode.Shl S.s_shl_clrr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shl_irr" "sal%s" Opcode.Shl S.s_shl_irr;
      b ~width:32 "shll_clm" "sall" Opcode.Shl S.s_shll_clm;
      b ~width:64 ~mode:Only64 "shlq_clm" "salq" Opcode.Shl S.s_shlq_clm;
      b ~width:32 "shll_im" "sall" Opcode.Shl S.s_shll_im;
      b ~width:64 ~mode:Only64 "shlq_im" "salq" Opcode.Shl S.s_shlq_im;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shl_clm" "sal%s" Opcode.Shl S.s_shl_clm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shl_im" "sal%s" Opcode.Shl S.s_shl_im;
    ]

let () =
  def
    ~forms:[ (Form.Shift_ext, 0x07) ]
    ~writes_flags:true Opcode.Sar
    [
      b ~widths:[ 32; 64 ] "sar" "sar%s" Opcode.Sar S.s_sar;
      b ~width:32 "sarl_m" "sarl" Opcode.Sar S.s_sarl_m;
      b ~width:64 ~mode:Only64 "sarq_m" "sarq" Opcode.Sar S.s_sarq_m;
      b ~widths:[ 32; 64 ] "sar_cl" "sar%s" Opcode.Sar S.s_sar_cl;
      b ~widths:[ 32; 64 ] "sar_i" "sar%s" Opcode.Sar S.s_sar_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "sar_clrr" "sar%s" Opcode.Sar S.s_sar_clrr;
      b ~widths:[ 32; 64 ] ~mode:Only64 "sar_irr" "sar%s" Opcode.Sar S.s_sar_irr;
      b ~width:32 "sarl_clm" "sarl" Opcode.Sar S.s_sarl_clm;
      b ~width:64 ~mode:Only64 "sarq_clm" "sarq" Opcode.Sar S.s_sarq_clm;
      b ~width:32 "sarl_im" "sarl" Opcode.Sar S.s_sarl_im;
      b ~width:64 ~mode:Only64 "sarq_im" "sarq" Opcode.Sar S.s_sarq_im;
      b ~widths:[ 32; 64 ] ~mode:Only64 "sar_clm" "sar%s" Opcode.Sar S.s_sar_clm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "sar_im" "sar%s" Opcode.Sar S.s_sar_im;
    ]

let () =
  def ~writes_flags:true Opcode.Shld
    [
      b ~widths:[ 32; 64 ] "shld_cl" "shld%s" Opcode.Shld S.s_shld_cl;
      b ~widths:[ 32; 64 ] "shld_i" "shld%s" Opcode.Shld S.s_shld_i;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shld_irrr" "shld%s" Opcode.Shld S.s_shld_irrr;
      b ~widths:[ 32; 64 ] "shld_clrm" "shld%s" Opcode.Shld S.s_shld_clrm;
      b ~widths:[ 32; 64 ] "shld_irm" "shld%s" Opcode.Shld S.s_shld_irm;
      b ~widths:[ 32; 64 ] ~mode:Only64 "shld_irmr" "shld%s" Opcode.Shld S.s_shld_irmr;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x58) ]
    Opcode.Addsd
    [
      b ~width:32 "addsd" "addsd" Opcode.Addsd S.s_addsd;
      b ~width:32 "addsd_m" "addsd" Opcode.Addsd S.s_addsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x5c) ]
    Opcode.Subsd
    [
      b ~width:32 "subsd" "subsd" Opcode.Subsd S.s_subsd;
      b ~width:32 "subsd_m" "subsd" Opcode.Subsd S.s_subsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x59) ]
    Opcode.Mulsd
    [
      b ~width:32 "mulsd" "mulsd" Opcode.Mulsd S.s_mulsd;
      b ~width:32 "mulsd_m" "mulsd" Opcode.Mulsd S.s_mulsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x5e) ]
    Opcode.Divsd
    [
      b ~width:32 "divsd" "divsd" Opcode.Divsd S.s_divsd;
      b ~width:32 "divsd_m" "divsd" Opcode.Divsd S.s_divsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x58) ]
    Opcode.Addss
    [
      b ~width:32 "addss" "addss" Opcode.Addss S.s_addss;
      b ~width:32 "addss_m" "addss" Opcode.Addss S.s_addss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5c) ]
    Opcode.Subss
    [
      b ~width:32 "subss" "subss" Opcode.Subss S.s_subss;
      b ~width:32 "subss_m" "subss" Opcode.Subss S.s_subss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x59) ]
    Opcode.Mulss
    [
      b ~width:32 "mulss" "mulss" Opcode.Mulss S.s_mulss;
      b ~width:32 "mulss_m" "mulss" Opcode.Mulss S.s_mulss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5e) ]
    Opcode.Divss
    [
      b ~width:32 "divss" "divss" Opcode.Divss S.s_divss;
      b ~width:32 "divss_m" "divss" Opcode.Divss S.s_divss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x2f) ]
    ~writes_flags:true Opcode.Comisd
    [
      b ~width:32 "comisd" "comisd" Opcode.Comisd S.s_comisd;
      b ~width:32 "comisd_m" "comisd" Opcode.Comisd S.s_comisd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x2e) ]
    ~writes_flags:true Opcode.Ucomisd
    [
      b ~width:32 "ucomisd" "ucomisd" Opcode.Ucomisd S.s_ucomisd;
      b ~width:32 "ucomisd_m" "ucomisd" Opcode.Ucomisd S.s_ucomisd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x2f) ]
    ~writes_flags:true Opcode.Comiss
    [
      b ~width:32 "comiss" "comiss" Opcode.Comiss S.s_comiss;
      b ~width:32 "comiss_m" "comiss" Opcode.Comiss S.s_comiss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x2e) ]
    ~writes_flags:true Opcode.Ucomiss
    [
      b ~width:32 "ucomiss" "ucomiss" Opcode.Ucomiss S.s_ucomiss;
      b ~width:32 "ucomiss_m" "ucomiss" Opcode.Ucomiss S.s_ucomiss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x57) ]
    Opcode.Xorpd
    [
      b ~width:32 "xorpd" "xorpd" Opcode.Xorpd S.s_xorpd;
      b ~width:32 "xorpd_m" "xorpd" Opcode.Xorpd S.s_xorpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xef) ]
    Opcode.Pxor
    [
      b ~width:32 "pxor_qq" "pxor" Opcode.Pxor S.s_pxor_qq;
      b ~width:32 "pxor" "pxor" Opcode.Pxor S.s_pxor;
      b ~width:32 "pxor_mq" "pxor" Opcode.Pxor S.s_pxor_mq;
      b ~width:32 "pxor_m" "pxor" Opcode.Pxor S.s_pxor_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x28) ]
    Opcode.Movapd
    [
      b ~width:32 "movapd" "movapd" Opcode.Movapd S.s_movapd;
      b ~width:32 "movapd_m" "movapd" Opcode.Movapd S.s_movapd_m;
      b ~width:32 "movapd_rm" "movapd" Opcode.Movapd S.s_movapd_rm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x5a) ]
    Opcode.Cvtsd2ss
    [
      b ~width:32 "cvtsd2ss" "cvtsd2ss" Opcode.Cvtsd2ss S.s_cvtsd2ss;
      b ~width:32 "cvtsd2ss_m" "cvtsd2ss" Opcode.Cvtsd2ss S.s_cvtsd2ss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5a) ]
    Opcode.Cvtss2sd
    [
      b ~width:32 "cvtss2sd" "cvtss2sd" Opcode.Cvtss2sd S.s_cvtss2sd;
      b ~width:32 "cvtss2sd_m" "cvtss2sd" Opcode.Cvtss2sd S.s_cvtss2sd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5a) ]
    Opcode.Cvtps2pd
    [
      b ~width:32 "cvtps2pd" "cvtps2pd" Opcode.Cvtps2pd S.s_cvtps2pd;
      b ~width:32 "cvtps2pd_m" "cvtps2pd" Opcode.Cvtps2pd S.s_cvtps2pd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5a) ]
    Opcode.Cvtpd2ps
    [
      b ~width:32 "cvtpd2ps" "cvtpd2ps" Opcode.Cvtpd2ps S.s_cvtpd2ps;
      b ~width:32 "cvtpd2ps_m" "cvtpd2ps" Opcode.Cvtpd2ps S.s_cvtpd2ps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5b) ]
    Opcode.Cvtdq2ps
    [
      b ~width:32 "cvtdq2ps" "cvtdq2ps" Opcode.Cvtdq2ps S.s_cvtdq2ps;
      b ~width:32 "cvtdq2ps_m" "cvtdq2ps" Opcode.Cvtdq2ps S.s_cvtdq2ps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5b) ]
    Opcode.Cvtps2dq
    [
      b ~width:32 "cvtps2dq" "cvtps2dq" Opcode.Cvtps2dq S.s_cvtps2dq;
      b ~width:32 "cvtps2dq_m" "cvtps2dq" Opcode.Cvtps2dq S.s_cvtps2dq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5b) ]
    Opcode.Cvttps2dq
    [
      b ~width:32 "cvttps2dq" "cvttps2dq" Opcode.Cvttps2dq S.s_cvttps2dq;
      b ~width:32 "cvttps2dq_m" "cvttps2dq" Opcode.Cvttps2dq S.s_cvttps2dq_m;
    ]

let () =
  def Opcode.Movsd
    [
      b ~width:32 "movsd" "movsd" Opcode.Movsd S.s_movsd;
      b ~width:32 "movsd_m" "movsd" Opcode.Movsd S.s_movsd_m;
      b ~width:32 "movsd_rm" "movsd" Opcode.Movsd S.s_movsd_rm;
    ]

let () =
  def Opcode.Movss
    [
      b ~width:32 "movss" "movss" Opcode.Movss S.s_movss;
      b ~width:32 "movss_m" "movss" Opcode.Movss S.s_movss_m;
      b ~width:32 "movss_rm" "movss" Opcode.Movss S.s_movss_rm;
    ]

let () =
  def Opcode.Cvtsi2sd
    [
      b ~width:32 ~widths:[ 32; 64 ] "cvtsi2sd" "cvtsi2sd" Opcode.Cvtsi2sd S.s_cvtsi2sd;
      b ~width:32 "cvtsi2sd_m" "cvtsi2sd" Opcode.Cvtsi2sd S.s_cvtsi2sd_m;
    ]

let () =
  def Opcode.Cvtsi2ss
    [
      b ~width:32 ~widths:[ 32; 64 ] "cvtsi2ss" "cvtsi2ss" Opcode.Cvtsi2ss S.s_cvtsi2ss;
      b ~width:32 "cvtsi2ss_m" "cvtsi2ss" Opcode.Cvtsi2ss S.s_cvtsi2ss_m;
    ]

let () =
  def Opcode.Cvttsd2si
    [
      b ~widths:[ 32; 64 ] "cvttsd2si_x" "cvttsd2si" Opcode.Cvttsd2si S.s_cvttsd2si_x;
      b ~widths:[ 32; 64 ] "cvttsd2si_m" "cvttsd2si" Opcode.Cvttsd2si S.s_cvttsd2si_m;
      b ~width:64 ~mode:Only64 "cvttsd2si_mr" "cvttsd2siq" Opcode.Cvttsd2si S.s_cvttsd2si_mr;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x14) ]
    Opcode.Unpcklps
    [
      b ~width:32 "unpcklps" "unpcklps" Opcode.Unpcklps S.s_unpcklps;
      b ~width:32 "unpcklps_m" "unpcklps" Opcode.Unpcklps S.s_unpcklps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x15) ]
    Opcode.Unpckhps
    [
      b ~width:32 "unpckhps" "unpckhps" Opcode.Unpckhps S.s_unpckhps;
      b ~width:32 "unpckhps_m" "unpckhps" Opcode.Unpckhps S.s_unpckhps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x14) ]
    Opcode.Unpcklpd
    [
      b ~width:32 "unpcklpd" "unpcklpd" Opcode.Unpcklpd S.s_unpcklpd;
      b ~width:32 "unpcklpd_m" "unpcklpd" Opcode.Unpcklpd S.s_unpcklpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x15) ]
    Opcode.Unpckhpd
    [
      b ~width:32 "unpckhpd" "unpckhpd" Opcode.Unpckhpd S.s_unpckhpd;
      b ~width:32 "unpckhpd_m" "unpckhpd" Opcode.Unpckhpd S.s_unpckhpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x6c) ]
    Opcode.Punpcklqdq
    [
      b ~width:32 "punpcklqdq" "punpcklqdq" Opcode.Punpcklqdq S.s_punpcklqdq;
      b ~width:32 "punpcklqdq_m" "punpcklqdq" Opcode.Punpcklqdq S.s_punpcklqdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x6d) ]
    Opcode.Punpckhqdq
    [
      b ~width:32 "punpckhqdq" "punpckhqdq" Opcode.Punpckhqdq S.s_punpckhqdq;
      b ~width:32 "punpckhqdq_m" "punpckhqdq" Opcode.Punpckhqdq S.s_punpckhqdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x60) ]
    Opcode.Punpcklbw
    [
      b ~width:32 "punpcklbw_qq" "punpcklbw" Opcode.Punpcklbw S.s_punpcklbw_qq;
      b ~width:32 "punpcklbw" "punpcklbw" Opcode.Punpcklbw S.s_punpcklbw;
      b ~width:32 "punpcklbw_mq" "punpcklbw" Opcode.Punpcklbw S.s_punpcklbw_mq;
      b ~width:32 "punpcklbw_m" "punpcklbw" Opcode.Punpcklbw S.s_punpcklbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x68) ]
    Opcode.Punpckhbw
    [
      b ~width:32 "punpckhbw_qq" "punpckhbw" Opcode.Punpckhbw S.s_punpckhbw_qq;
      b ~width:32 "punpckhbw" "punpckhbw" Opcode.Punpckhbw S.s_punpckhbw;
      b ~width:32 "punpckhbw_mq" "punpckhbw" Opcode.Punpckhbw S.s_punpckhbw_mq;
      b ~width:32 "punpckhbw_m" "punpckhbw" Opcode.Punpckhbw S.s_punpckhbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x61) ]
    Opcode.Punpcklwd
    [
      b ~width:32 "punpcklwd_qq" "punpcklwd" Opcode.Punpcklwd S.s_punpcklwd_qq;
      b ~width:32 "punpcklwd" "punpcklwd" Opcode.Punpcklwd S.s_punpcklwd;
      b ~width:32 "punpcklwd_mq" "punpcklwd" Opcode.Punpcklwd S.s_punpcklwd_mq;
      b ~width:32 "punpcklwd_m" "punpcklwd" Opcode.Punpcklwd S.s_punpcklwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x69) ]
    Opcode.Punpckhwd
    [
      b ~width:32 "punpckhwd_qq" "punpckhwd" Opcode.Punpckhwd S.s_punpckhwd_qq;
      b ~width:32 "punpckhwd" "punpckhwd" Opcode.Punpckhwd S.s_punpckhwd;
      b ~width:32 "punpckhwd_mq" "punpckhwd" Opcode.Punpckhwd S.s_punpckhwd_mq;
      b ~width:32 "punpckhwd_m" "punpckhwd" Opcode.Punpckhwd S.s_punpckhwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x62) ]
    Opcode.Punpckldq
    [
      b ~width:32 "punpckldq_qq" "punpckldq" Opcode.Punpckldq S.s_punpckldq_qq;
      b ~width:32 "punpckldq" "punpckldq" Opcode.Punpckldq S.s_punpckldq;
      b ~width:32 "punpckldq_mq" "punpckldq" Opcode.Punpckldq S.s_punpckldq_mq;
      b ~width:32 "punpckldq_m" "punpckldq" Opcode.Punpckldq S.s_punpckldq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x6a) ]
    Opcode.Punpckhdq
    [
      b ~width:32 "punpckhdq_qq" "punpckhdq" Opcode.Punpckhdq S.s_punpckhdq_qq;
      b ~width:32 "punpckhdq" "punpckhdq" Opcode.Punpckhdq S.s_punpckhdq;
      b ~width:32 "punpckhdq_mq" "punpckhdq" Opcode.Punpckhdq S.s_punpckhdq_mq;
      b ~width:32 "punpckhdq_m" "punpckhdq" Opcode.Punpckhdq S.s_punpckhdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xfc) ]
    Opcode.Paddb
    [
      b ~width:32 "paddb_qq" "paddb" Opcode.Paddb S.s_paddb_qq;
      b ~width:32 "paddb" "paddb" Opcode.Paddb S.s_paddb;
      b ~width:32 "paddb_mq" "paddb" Opcode.Paddb S.s_paddb_mq;
      b ~width:32 "paddb_m" "paddb" Opcode.Paddb S.s_paddb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xfd) ]
    Opcode.Paddw
    [
      b ~width:32 "paddw_qq" "paddw" Opcode.Paddw S.s_paddw_qq;
      b ~width:32 "paddw" "paddw" Opcode.Paddw S.s_paddw;
      b ~width:32 "paddw_mq" "paddw" Opcode.Paddw S.s_paddw_mq;
      b ~width:32 "paddw_m" "paddw" Opcode.Paddw S.s_paddw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xfe) ]
    Opcode.Paddd
    [
      b ~width:32 "paddd_qq" "paddd" Opcode.Paddd S.s_paddd_qq;
      b ~width:32 "paddd" "paddd" Opcode.Paddd S.s_paddd;
      b ~width:32 "paddd_mq" "paddd" Opcode.Paddd S.s_paddd_mq;
      b ~width:32 "paddd_m" "paddd" Opcode.Paddd S.s_paddd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xd4) ]
    Opcode.Paddq
    [
      b ~width:32 "paddq_qq" "paddq" Opcode.Paddq S.s_paddq_qq;
      b ~width:32 "paddq" "paddq" Opcode.Paddq S.s_paddq;
      b ~width:32 "paddq_mq" "paddq" Opcode.Paddq S.s_paddq_mq;
      b ~width:32 "paddq_m" "paddq" Opcode.Paddq S.s_paddq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf8) ]
    Opcode.Psubb
    [
      b ~width:32 "psubb_qq" "psubb" Opcode.Psubb S.s_psubb_qq;
      b ~width:32 "psubb" "psubb" Opcode.Psubb S.s_psubb;
      b ~width:32 "psubb_mq" "psubb" Opcode.Psubb S.s_psubb_mq;
      b ~width:32 "psubb_m" "psubb" Opcode.Psubb S.s_psubb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf9) ]
    Opcode.Psubw
    [
      b ~width:32 "psubw_qq" "psubw" Opcode.Psubw S.s_psubw_qq;
      b ~width:32 "psubw" "psubw" Opcode.Psubw S.s_psubw;
      b ~width:32 "psubw_mq" "psubw" Opcode.Psubw S.s_psubw_mq;
      b ~width:32 "psubw_m" "psubw" Opcode.Psubw S.s_psubw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xfa) ]
    Opcode.Psubd
    [
      b ~width:32 "psubd_qq" "psubd" Opcode.Psubd S.s_psubd_qq;
      b ~width:32 "psubd" "psubd" Opcode.Psubd S.s_psubd;
      b ~width:32 "psubd_mq" "psubd" Opcode.Psubd S.s_psubd_mq;
      b ~width:32 "psubd_m" "psubd" Opcode.Psubd S.s_psubd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xfb) ]
    Opcode.Psubq
    [
      b ~width:32 "psubq_qq" "psubq" Opcode.Psubq S.s_psubq_qq;
      b ~width:32 "psubq" "psubq" Opcode.Psubq S.s_psubq;
      b ~width:32 "psubq_mq" "psubq" Opcode.Psubq S.s_psubq_mq;
      b ~width:32 "psubq_m" "psubq" Opcode.Psubq S.s_psubq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x74) ]
    Opcode.Pcmpeqb
    [
      b ~width:32 "pcmpeqb_qq" "pcmpeqb" Opcode.Pcmpeqb S.s_pcmpeqb_qq;
      b ~width:32 "pcmpeqb" "pcmpeqb" Opcode.Pcmpeqb S.s_pcmpeqb;
      b ~width:32 "pcmpeqb_mq" "pcmpeqb" Opcode.Pcmpeqb S.s_pcmpeqb_mq;
      b ~width:32 "pcmpeqb_m" "pcmpeqb" Opcode.Pcmpeqb S.s_pcmpeqb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x75) ]
    Opcode.Pcmpeqw
    [
      b ~width:32 "pcmpeqw_qq" "pcmpeqw" Opcode.Pcmpeqw S.s_pcmpeqw_qq;
      b ~width:32 "pcmpeqw" "pcmpeqw" Opcode.Pcmpeqw S.s_pcmpeqw;
      b ~width:32 "pcmpeqw_mq" "pcmpeqw" Opcode.Pcmpeqw S.s_pcmpeqw_mq;
      b ~width:32 "pcmpeqw_m" "pcmpeqw" Opcode.Pcmpeqw S.s_pcmpeqw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x76) ]
    Opcode.Pcmpeqd
    [
      b ~width:32 "pcmpeqd_qq" "pcmpeqd" Opcode.Pcmpeqd S.s_pcmpeqd_qq;
      b ~width:32 "pcmpeqd" "pcmpeqd" Opcode.Pcmpeqd S.s_pcmpeqd;
      b ~width:32 "pcmpeqd_mq" "pcmpeqd" Opcode.Pcmpeqd S.s_pcmpeqd_mq;
      b ~width:32 "pcmpeqd_m" "pcmpeqd" Opcode.Pcmpeqd S.s_pcmpeqd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x64) ]
    Opcode.Pcmpgtb
    [
      b ~width:32 "pcmpgtb_qq" "pcmpgtb" Opcode.Pcmpgtb S.s_pcmpgtb_qq;
      b ~width:32 "pcmpgtb" "pcmpgtb" Opcode.Pcmpgtb S.s_pcmpgtb;
      b ~width:32 "pcmpgtb_mq" "pcmpgtb" Opcode.Pcmpgtb S.s_pcmpgtb_mq;
      b ~width:32 "pcmpgtb_m" "pcmpgtb" Opcode.Pcmpgtb S.s_pcmpgtb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x65) ]
    Opcode.Pcmpgtw
    [
      b ~width:32 "pcmpgtw_qq" "pcmpgtw" Opcode.Pcmpgtw S.s_pcmpgtw_qq;
      b ~width:32 "pcmpgtw" "pcmpgtw" Opcode.Pcmpgtw S.s_pcmpgtw;
      b ~width:32 "pcmpgtw_mq" "pcmpgtw" Opcode.Pcmpgtw S.s_pcmpgtw_mq;
      b ~width:32 "pcmpgtw_m" "pcmpgtw" Opcode.Pcmpgtw S.s_pcmpgtw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x66) ]
    Opcode.Pcmpgtd
    [
      b ~width:32 "pcmpgtd_qq" "pcmpgtd" Opcode.Pcmpgtd S.s_pcmpgtd_qq;
      b ~width:32 "pcmpgtd" "pcmpgtd" Opcode.Pcmpgtd S.s_pcmpgtd;
      b ~width:32 "pcmpgtd_mq" "pcmpgtd" Opcode.Pcmpgtd S.s_pcmpgtd_mq;
      b ~width:32 "pcmpgtd_m" "pcmpgtd" Opcode.Pcmpgtd S.s_pcmpgtd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x63) ]
    Opcode.Packsswb
    [
      b ~width:32 "packsswb_qq" "packsswb" Opcode.Packsswb S.s_packsswb_qq;
      b ~width:32 "packsswb" "packsswb" Opcode.Packsswb S.s_packsswb;
      b ~width:32 "packsswb_mq" "packsswb" Opcode.Packsswb S.s_packsswb_mq;
      b ~width:32 "packsswb_m" "packsswb" Opcode.Packsswb S.s_packsswb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x6b) ]
    Opcode.Packssdw
    [
      b ~width:32 "packssdw_qq" "packssdw" Opcode.Packssdw S.s_packssdw_qq;
      b ~width:32 "packssdw" "packssdw" Opcode.Packssdw S.s_packssdw;
      b ~width:32 "packssdw_mq" "packssdw" Opcode.Packssdw S.s_packssdw_mq;
      b ~width:32 "packssdw_m" "packssdw" Opcode.Packssdw S.s_packssdw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x67) ]
    Opcode.Packuswb
    [
      b ~width:32 "packuswb_qq" "packuswb" Opcode.Packuswb S.s_packuswb_qq;
      b ~width:32 "packuswb" "packuswb" Opcode.Packuswb S.s_packuswb;
      b ~width:32 "packuswb_mq" "packuswb" Opcode.Packuswb S.s_packuswb_mq;
      b ~width:32 "packuswb_m" "packuswb" Opcode.Packuswb S.s_packuswb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xdb) ]
    Opcode.Pand
    [
      b ~width:32 "pand_qq" "pand" Opcode.Pand S.s_pand_qq;
      b ~width:32 "pand" "pand" Opcode.Pand S.s_pand;
      b ~width:32 "pand_mq" "pand" Opcode.Pand S.s_pand_mq;
      b ~width:32 "pand_m" "pand" Opcode.Pand S.s_pand_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xdf) ]
    Opcode.Pandn
    [
      b ~width:32 "pandn_qq" "pandn" Opcode.Pandn S.s_pandn_qq;
      b ~width:32 "pandn" "pandn" Opcode.Pandn S.s_pandn;
      b ~width:32 "pandn_mq" "pandn" Opcode.Pandn S.s_pandn_mq;
      b ~width:32 "pandn_m" "pandn" Opcode.Pandn S.s_pandn_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xeb) ]
    Opcode.Por
    [
      b ~width:32 "por_qq" "por" Opcode.Por S.s_por_qq;
      b ~width:32 "por" "por" Opcode.Por S.s_por;
      b ~width:32 "por_mq" "por" Opcode.Por S.s_por_mq;
      b ~width:32 "por_m" "por" Opcode.Por S.s_por_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xda) ]
    Opcode.Pminub
    [
      b ~width:32 "pminub_qq" "pminub" Opcode.Pminub S.s_pminub_qq;
      b ~width:32 "pminub" "pminub" Opcode.Pminub S.s_pminub;
      b ~width:32 "pminub_mq" "pminub" Opcode.Pminub S.s_pminub_mq;
      b ~width:32 "pminub_m" "pminub" Opcode.Pminub S.s_pminub_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xde) ]
    Opcode.Pmaxub
    [
      b ~width:32 "pmaxub_qq" "pmaxub" Opcode.Pmaxub S.s_pmaxub_qq;
      b ~width:32 "pmaxub" "pmaxub" Opcode.Pmaxub S.s_pmaxub;
      b ~width:32 "pmaxub_mq" "pmaxub" Opcode.Pmaxub S.s_pmaxub_mq;
      b ~width:32 "pmaxub_m" "pmaxub" Opcode.Pmaxub S.s_pmaxub_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xea) ]
    Opcode.Pminsw
    [
      b ~width:32 "pminsw_qq" "pminsw" Opcode.Pminsw S.s_pminsw_qq;
      b ~width:32 "pminsw" "pminsw" Opcode.Pminsw S.s_pminsw;
      b ~width:32 "pminsw_mq" "pminsw" Opcode.Pminsw S.s_pminsw_mq;
      b ~width:32 "pminsw_m" "pminsw" Opcode.Pminsw S.s_pminsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xee) ]
    Opcode.Pmaxsw
    [
      b ~width:32 "pmaxsw_qq" "pmaxsw" Opcode.Pmaxsw S.s_pmaxsw_qq;
      b ~width:32 "pmaxsw" "pmaxsw" Opcode.Pmaxsw S.s_pmaxsw;
      b ~width:32 "pmaxsw_mq" "pmaxsw" Opcode.Pmaxsw S.s_pmaxsw_mq;
      b ~width:32 "pmaxsw_m" "pmaxsw" Opcode.Pmaxsw S.s_pmaxsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xd5) ]
    Opcode.Pmullw
    [
      b ~width:32 "pmullw_qq" "pmullw" Opcode.Pmullw S.s_pmullw_qq;
      b ~width:32 "pmullw" "pmullw" Opcode.Pmullw S.s_pmullw;
      b ~width:32 "pmullw_mq" "pmullw" Opcode.Pmullw S.s_pmullw_mq;
      b ~width:32 "pmullw_m" "pmullw" Opcode.Pmullw S.s_pmullw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe5) ]
    Opcode.Pmulhw
    [
      b ~width:32 "pmulhw_qq" "pmulhw" Opcode.Pmulhw S.s_pmulhw_qq;
      b ~width:32 "pmulhw" "pmulhw" Opcode.Pmulhw S.s_pmulhw;
      b ~width:32 "pmulhw_mq" "pmulhw" Opcode.Pmulhw S.s_pmulhw_mq;
      b ~width:32 "pmulhw_m" "pmulhw" Opcode.Pmulhw S.s_pmulhw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe4) ]
    Opcode.Pmulhuw
    [
      b ~width:32 "pmulhuw_qq" "pmulhuw" Opcode.Pmulhuw S.s_pmulhuw_qq;
      b ~width:32 "pmulhuw" "pmulhuw" Opcode.Pmulhuw S.s_pmulhuw;
      b ~width:32 "pmulhuw_mq" "pmulhuw" Opcode.Pmulhuw S.s_pmulhuw_mq;
      b ~width:32 "pmulhuw_m" "pmulhuw" Opcode.Pmulhuw S.s_pmulhuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe0) ]
    Opcode.Pavgb
    [
      b ~width:32 "pavgb_qq" "pavgb" Opcode.Pavgb S.s_pavgb_qq;
      b ~width:32 "pavgb" "pavgb" Opcode.Pavgb S.s_pavgb;
      b ~width:32 "pavgb_mq" "pavgb" Opcode.Pavgb S.s_pavgb_mq;
      b ~width:32 "pavgb_m" "pavgb" Opcode.Pavgb S.s_pavgb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe3) ]
    Opcode.Pavgw
    [
      b ~width:32 "pavgw_qq" "pavgw" Opcode.Pavgw S.s_pavgw_qq;
      b ~width:32 "pavgw" "pavgw" Opcode.Pavgw S.s_pavgw;
      b ~width:32 "pavgw_mq" "pavgw" Opcode.Pavgw S.s_pavgw_mq;
      b ~width:32 "pavgw_m" "pavgw" Opcode.Pavgw S.s_pavgw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf6) ]
    Opcode.Psadbw
    [
      b ~width:32 "psadbw_qq" "psadbw" Opcode.Psadbw S.s_psadbw_qq;
      b ~width:32 "psadbw" "psadbw" Opcode.Psadbw S.s_psadbw;
      b ~width:32 "psadbw_mq" "psadbw" Opcode.Psadbw S.s_psadbw_mq;
      b ~width:32 "psadbw_m" "psadbw" Opcode.Psadbw S.s_psadbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf1); (Form.Xmm_shift_ext, 0x06); (Form.Xmm_shift_opcode, 0x71) ]
    Opcode.Psllw
    [
      b ~width:32 "psllw_qq" "psllw" Opcode.Psllw S.s_psllw_qq;
      b ~width:32 "psllw" "psllw" Opcode.Psllw S.s_psllw;
      b ~width:32 "psllw_iq" "psllw" Opcode.Psllw S.s_psllw_iq;
      b ~width:32 "psllw_i" "psllw" Opcode.Psllw S.s_psllw_i;
      b ~width:32 "psllw_mq" "psllw" Opcode.Psllw S.s_psllw_mq;
      b ~width:32 "psllw_m" "psllw" Opcode.Psllw S.s_psllw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf2); (Form.Xmm_shift_ext, 0x06); (Form.Xmm_shift_opcode, 0x72) ]
    Opcode.Pslld
    [
      b ~width:32 "pslld_qq" "pslld" Opcode.Pslld S.s_pslld_qq;
      b ~width:32 "pslld" "pslld" Opcode.Pslld S.s_pslld;
      b ~width:32 "pslld_iq" "pslld" Opcode.Pslld S.s_pslld_iq;
      b ~width:32 "pslld_i" "pslld" Opcode.Pslld S.s_pslld_i;
      b ~width:32 "pslld_mq" "pslld" Opcode.Pslld S.s_pslld_mq;
      b ~width:32 "pslld_m" "pslld" Opcode.Pslld S.s_pslld_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xf3); (Form.Xmm_shift_ext, 0x06); (Form.Xmm_shift_opcode, 0x73) ]
    Opcode.Psllq
    [
      b ~width:32 "psllq_qq" "psllq" Opcode.Psllq S.s_psllq_qq;
      b ~width:32 "psllq" "psllq" Opcode.Psllq S.s_psllq;
      b ~width:32 "psllq_iq" "psllq" Opcode.Psllq S.s_psllq_iq;
      b ~width:32 "psllq_i" "psllq" Opcode.Psllq S.s_psllq_i;
      b ~width:32 "psllq_mq" "psllq" Opcode.Psllq S.s_psllq_mq;
      b ~width:32 "psllq_m" "psllq" Opcode.Psllq S.s_psllq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xd1); (Form.Xmm_shift_ext, 0x02); (Form.Xmm_shift_opcode, 0x71) ]
    Opcode.Psrlw
    [
      b ~width:32 "psrlw_qq" "psrlw" Opcode.Psrlw S.s_psrlw_qq;
      b ~width:32 "psrlw" "psrlw" Opcode.Psrlw S.s_psrlw;
      b ~width:32 "psrlw_iq" "psrlw" Opcode.Psrlw S.s_psrlw_iq;
      b ~width:32 "psrlw_i" "psrlw" Opcode.Psrlw S.s_psrlw_i;
      b ~width:32 "psrlw_mq" "psrlw" Opcode.Psrlw S.s_psrlw_mq;
      b ~width:32 "psrlw_m" "psrlw" Opcode.Psrlw S.s_psrlw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xd2); (Form.Xmm_shift_ext, 0x02); (Form.Xmm_shift_opcode, 0x72) ]
    Opcode.Psrld
    [
      b ~width:32 "psrld_qq" "psrld" Opcode.Psrld S.s_psrld_qq;
      b ~width:32 "psrld" "psrld" Opcode.Psrld S.s_psrld;
      b ~width:32 "psrld_iq" "psrld" Opcode.Psrld S.s_psrld_iq;
      b ~width:32 "psrld_i" "psrld" Opcode.Psrld S.s_psrld_i;
      b ~width:32 "psrld_mq" "psrld" Opcode.Psrld S.s_psrld_mq;
      b ~width:32 "psrld_m" "psrld" Opcode.Psrld S.s_psrld_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xd3); (Form.Xmm_shift_ext, 0x02); (Form.Xmm_shift_opcode, 0x73) ]
    Opcode.Psrlq
    [
      b ~width:32 "psrlq_qq" "psrlq" Opcode.Psrlq S.s_psrlq_qq;
      b ~width:32 "psrlq" "psrlq" Opcode.Psrlq S.s_psrlq;
      b ~width:32 "psrlq_iq" "psrlq" Opcode.Psrlq S.s_psrlq_iq;
      b ~width:32 "psrlq_i" "psrlq" Opcode.Psrlq S.s_psrlq_i;
      b ~width:32 "psrlq_mq" "psrlq" Opcode.Psrlq S.s_psrlq_mq;
      b ~width:32 "psrlq_m" "psrlq" Opcode.Psrlq S.s_psrlq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe1); (Form.Xmm_shift_ext, 0x04); (Form.Xmm_shift_opcode, 0x71) ]
    Opcode.Psraw
    [
      b ~width:32 "psraw_qq" "psraw" Opcode.Psraw S.s_psraw_qq;
      b ~width:32 "psraw" "psraw" Opcode.Psraw S.s_psraw;
      b ~width:32 "psraw_iq" "psraw" Opcode.Psraw S.s_psraw_iq;
      b ~width:32 "psraw_i" "psraw" Opcode.Psraw S.s_psraw_i;
      b ~width:32 "psraw_mq" "psraw" Opcode.Psraw S.s_psraw_mq;
      b ~width:32 "psraw_m" "psraw" Opcode.Psraw S.s_psraw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0xe2); (Form.Xmm_shift_ext, 0x04); (Form.Xmm_shift_opcode, 0x72) ]
    Opcode.Psrad
    [
      b ~width:32 "psrad_qq" "psrad" Opcode.Psrad S.s_psrad_qq;
      b ~width:32 "psrad" "psrad" Opcode.Psrad S.s_psrad;
      b ~width:32 "psrad_iq" "psrad" Opcode.Psrad S.s_psrad_iq;
      b ~width:32 "psrad_i" "psrad" Opcode.Psrad S.s_psrad_i;
      b ~width:32 "psrad_mq" "psrad" Opcode.Psrad S.s_psrad_mq;
      b ~width:32 "psrad_m" "psrad" Opcode.Psrad S.s_psrad_m;
    ]

let () =
  def
    ~forms:[ (Form.Xmm_shift_ext, 0x07); (Form.Xmm_shift_opcode, 0x73) ]
    Opcode.Pslldq
    [ b ~width:32 "pslldq_i" "pslldq" Opcode.Pslldq S.s_pslldq_i ]

let () =
  def
    ~forms:[ (Form.Xmm_shift_ext, 0x03); (Form.Xmm_shift_opcode, 0x73) ]
    Opcode.Psrldq
    [ b ~width:32 "psrldq_i" "psrldq" Opcode.Psrldq S.s_psrldq_i ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x00) ]
    Opcode.Pshufb
    [
      b ~width:32 "pshufb_qq" "pshufb" Opcode.Pshufb S.s_pshufb_qq;
      b ~width:32 "pshufb" "pshufb" Opcode.Pshufb S.s_pshufb;
      b ~width:32 "pshufb_mq" "pshufb" Opcode.Pshufb S.s_pshufb_mq;
      b ~width:32 "pshufb_m" "pshufb" Opcode.Pshufb S.s_pshufb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x01) ]
    Opcode.Phaddw
    [
      b ~width:32 "phaddw_qq" "phaddw" Opcode.Phaddw S.s_phaddw_qq;
      b ~width:32 "phaddw" "phaddw" Opcode.Phaddw S.s_phaddw;
      b ~width:32 "phaddw_mq" "phaddw" Opcode.Phaddw S.s_phaddw_mq;
      b ~width:32 "phaddw_m" "phaddw" Opcode.Phaddw S.s_phaddw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x02) ]
    Opcode.Phaddd
    [
      b ~width:32 "phaddd_qq" "phaddd" Opcode.Phaddd S.s_phaddd_qq;
      b ~width:32 "phaddd" "phaddd" Opcode.Phaddd S.s_phaddd;
      b ~width:32 "phaddd_mq" "phaddd" Opcode.Phaddd S.s_phaddd_mq;
      b ~width:32 "phaddd_m" "phaddd" Opcode.Phaddd S.s_phaddd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x05) ]
    Opcode.Phsubw
    [
      b ~width:32 "phsubw_qq" "phsubw" Opcode.Phsubw S.s_phsubw_qq;
      b ~width:32 "phsubw" "phsubw" Opcode.Phsubw S.s_phsubw;
      b ~width:32 "phsubw_mq" "phsubw" Opcode.Phsubw S.s_phsubw_mq;
      b ~width:32 "phsubw_m" "phsubw" Opcode.Phsubw S.s_phsubw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x06) ]
    Opcode.Phsubd
    [
      b ~width:32 "phsubd_qq" "phsubd" Opcode.Phsubd S.s_phsubd_qq;
      b ~width:32 "phsubd" "phsubd" Opcode.Phsubd S.s_phsubd;
      b ~width:32 "phsubd_mq" "phsubd" Opcode.Phsubd S.s_phsubd_mq;
      b ~width:32 "phsubd_m" "phsubd" Opcode.Phsubd S.s_phsubd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x08) ]
    Opcode.Psignb
    [
      b ~width:32 "psignb_qq" "psignb" Opcode.Psignb S.s_psignb_qq;
      b ~width:32 "psignb" "psignb" Opcode.Psignb S.s_psignb;
      b ~width:32 "psignb_mq" "psignb" Opcode.Psignb S.s_psignb_mq;
      b ~width:32 "psignb_m" "psignb" Opcode.Psignb S.s_psignb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x09) ]
    Opcode.Psignw
    [
      b ~width:32 "psignw_qq" "psignw" Opcode.Psignw S.s_psignw_qq;
      b ~width:32 "psignw" "psignw" Opcode.Psignw S.s_psignw;
      b ~width:32 "psignw_mq" "psignw" Opcode.Psignw S.s_psignw_mq;
      b ~width:32 "psignw_m" "psignw" Opcode.Psignw S.s_psignw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x0a) ]
    Opcode.Psignd
    [
      b ~width:32 "psignd_qq" "psignd" Opcode.Psignd S.s_psignd_qq;
      b ~width:32 "psignd" "psignd" Opcode.Psignd S.s_psignd;
      b ~width:32 "psignd_mq" "psignd" Opcode.Psignd S.s_psignd_mq;
      b ~width:32 "psignd_m" "psignd" Opcode.Psignd S.s_psignd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x04) ]
    Opcode.Pmaddubsw
    [
      b ~width:32 "pmaddubsw_qq" "pmaddubsw" Opcode.Pmaddubsw S.s_pmaddubsw_qq;
      b ~width:32 "pmaddubsw" "pmaddubsw" Opcode.Pmaddubsw S.s_pmaddubsw;
      b ~width:32 "pmaddubsw_mq" "pmaddubsw" Opcode.Pmaddubsw S.s_pmaddubsw_mq;
      b ~width:32 "pmaddubsw_m" "pmaddubsw" Opcode.Pmaddubsw S.s_pmaddubsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x0b) ]
    Opcode.Pmulhrsw
    [
      b ~width:32 "pmulhrsw_qq" "pmulhrsw" Opcode.Pmulhrsw S.s_pmulhrsw_qq;
      b ~width:32 "pmulhrsw" "pmulhrsw" Opcode.Pmulhrsw S.s_pmulhrsw;
      b ~width:32 "pmulhrsw_mq" "pmulhrsw" Opcode.Pmulhrsw S.s_pmulhrsw_mq;
      b ~width:32 "pmulhrsw_m" "pmulhrsw" Opcode.Pmulhrsw S.s_pmulhrsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x03) ]
    Opcode.Phaddsw
    [
      b ~width:32 "phaddsw_qq" "phaddsw" Opcode.Phaddsw S.s_phaddsw_qq;
      b ~width:32 "phaddsw" "phaddsw" Opcode.Phaddsw S.s_phaddsw;
      b ~width:32 "phaddsw_mq" "phaddsw" Opcode.Phaddsw S.s_phaddsw_mq;
      b ~width:32 "phaddsw_m" "phaddsw" Opcode.Phaddsw S.s_phaddsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x07) ]
    Opcode.Phsubsw
    [
      b ~width:32 "phsubsw_qq" "phsubsw" Opcode.Phsubsw S.s_phsubsw_qq;
      b ~width:32 "phsubsw" "phsubsw" Opcode.Phsubsw S.s_phsubsw;
      b ~width:32 "phsubsw_mq" "phsubsw" Opcode.Phsubsw S.s_phsubsw_mq;
      b ~width:32 "phsubsw_m" "phsubsw" Opcode.Phsubsw S.s_phsubsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x1c) ]
    Opcode.Pabsb
    [
      b ~width:32 "pabsb_qq" "pabsb" Opcode.Pabsb S.s_pabsb_qq;
      b ~width:32 "pabsb" "pabsb" Opcode.Pabsb S.s_pabsb;
      b ~width:32 "pabsb_mq" "pabsb" Opcode.Pabsb S.s_pabsb_mq;
      b ~width:32 "pabsb_m" "pabsb" Opcode.Pabsb S.s_pabsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x1d) ]
    Opcode.Pabsw
    [
      b ~width:32 "pabsw_qq" "pabsw" Opcode.Pabsw S.s_pabsw_qq;
      b ~width:32 "pabsw" "pabsw" Opcode.Pabsw S.s_pabsw;
      b ~width:32 "pabsw_mq" "pabsw" Opcode.Pabsw S.s_pabsw_mq;
      b ~width:32 "pabsw_m" "pabsw" Opcode.Pabsw S.s_pabsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x1e) ]
    Opcode.Pabsd
    [
      b ~width:32 "pabsd_qq" "pabsd" Opcode.Pabsd S.s_pabsd_qq;
      b ~width:32 "pabsd" "pabsd" Opcode.Pabsd S.s_pabsd;
      b ~width:32 "pabsd_mq" "pabsd" Opcode.Pabsd S.s_pabsd_mq;
      b ~width:32 "pabsd_m" "pabsd" Opcode.Pabsd S.s_pabsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0f) ]
    Opcode.Palignr
    [
      b ~width:32 "palignr_iqq" "palignr" Opcode.Palignr S.s_palignr_iqq;
      b ~width:32 "palignr_i" "palignr" Opcode.Palignr S.s_palignr_i;
      b ~width:32 "palignr_imq" "palignr" Opcode.Palignr S.s_palignr_imq;
      b ~width:32 "palignr_im" "palignr" Opcode.Palignr S.s_palignr_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x08) ]
    Opcode.Roundps
    [
      b ~width:32 "roundps_i" "roundps" Opcode.Roundps S.s_roundps_i;
      b ~width:32 "roundps_im" "roundps" Opcode.Roundps S.s_roundps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x09) ]
    Opcode.Roundpd
    [
      b ~width:32 "roundpd_i" "roundpd" Opcode.Roundpd S.s_roundpd_i;
      b ~width:32 "roundpd_im" "roundpd" Opcode.Roundpd S.s_roundpd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0a) ]
    Opcode.Roundss
    [
      b ~width:32 "roundss_i" "roundss" Opcode.Roundss S.s_roundss_i;
      b ~width:32 "roundss_im" "roundss" Opcode.Roundss S.s_roundss_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0b) ]
    Opcode.Roundsd
    [
      b ~width:32 "roundsd_i" "roundsd" Opcode.Roundsd S.s_roundsd_i;
      b ~width:32 "roundsd_im" "roundsd" Opcode.Roundsd S.s_roundsd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x29) ]
    Opcode.Pcmpeqq
    [
      b ~width:32 "pcmpeqq" "pcmpeqq" Opcode.Pcmpeqq S.s_pcmpeqq;
      b ~width:32 "pcmpeqq_m" "pcmpeqq" Opcode.Pcmpeqq S.s_pcmpeqq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x37) ]
    Opcode.Pcmpgtq
    [
      b ~width:32 "pcmpgtq" "pcmpgtq" Opcode.Pcmpgtq S.s_pcmpgtq;
      b ~width:32 "pcmpgtq_m" "pcmpgtq" Opcode.Pcmpgtq S.s_pcmpgtq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x2b) ]
    Opcode.Packusdw
    [
      b ~width:32 "packusdw" "packusdw" Opcode.Packusdw S.s_packusdw;
      b ~width:32 "packusdw_m" "packusdw" Opcode.Packusdw S.s_packusdw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3c) ]
    Opcode.Pmaxsb
    [
      b ~width:32 "pmaxsb" "pmaxsb" Opcode.Pmaxsb S.s_pmaxsb;
      b ~width:32 "pmaxsb_m" "pmaxsb" Opcode.Pmaxsb S.s_pmaxsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3d) ]
    Opcode.Pmaxsd
    [
      b ~width:32 "pmaxsd" "pmaxsd" Opcode.Pmaxsd S.s_pmaxsd;
      b ~width:32 "pmaxsd_m" "pmaxsd" Opcode.Pmaxsd S.s_pmaxsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3f) ]
    Opcode.Pmaxud
    [
      b ~width:32 "pmaxud" "pmaxud" Opcode.Pmaxud S.s_pmaxud;
      b ~width:32 "pmaxud_m" "pmaxud" Opcode.Pmaxud S.s_pmaxud_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3e) ]
    Opcode.Pmaxuw
    [
      b ~width:32 "pmaxuw" "pmaxuw" Opcode.Pmaxuw S.s_pmaxuw;
      b ~width:32 "pmaxuw_m" "pmaxuw" Opcode.Pmaxuw S.s_pmaxuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x38) ]
    Opcode.Pminsb
    [
      b ~width:32 "pminsb" "pminsb" Opcode.Pminsb S.s_pminsb;
      b ~width:32 "pminsb_m" "pminsb" Opcode.Pminsb S.s_pminsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x39) ]
    Opcode.Pminsd
    [
      b ~width:32 "pminsd" "pminsd" Opcode.Pminsd S.s_pminsd;
      b ~width:32 "pminsd_m" "pminsd" Opcode.Pminsd S.s_pminsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3b) ]
    Opcode.Pminud
    [
      b ~width:32 "pminud" "pminud" Opcode.Pminud S.s_pminud;
      b ~width:32 "pminud_m" "pminud" Opcode.Pminud S.s_pminud_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x3a) ]
    Opcode.Pminuw
    [
      b ~width:32 "pminuw" "pminuw" Opcode.Pminuw S.s_pminuw;
      b ~width:32 "pminuw_m" "pminuw" Opcode.Pminuw S.s_pminuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x28) ]
    Opcode.Pmuldq
    [
      b ~width:32 "pmuldq" "pmuldq" Opcode.Pmuldq S.s_pmuldq;
      b ~width:32 "pmuldq_m" "pmuldq" Opcode.Pmuldq S.s_pmuldq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x40) ]
    Opcode.Pmulld
    [
      b ~width:32 "pmulld" "pmulld" Opcode.Pmulld S.s_pmulld;
      b ~width:32 "pmulld_m" "pmulld" Opcode.Pmulld S.s_pmulld_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x41) ]
    Opcode.Phminposuw
    [
      b ~width:32 "phminposuw" "phminposuw" Opcode.Phminposuw S.s_phminposuw;
      b ~width:32 "phminposuw_m" "phminposuw" Opcode.Phminposuw S.s_phminposuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x17) ]
    ~writes_flags:true Opcode.Ptest
    [
      b ~width:32 "ptest" "ptest" Opcode.Ptest S.s_ptest;
      b ~width:32 "ptest_m" "ptest" Opcode.Ptest S.s_ptest_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x20) ]
    Opcode.Pmovsxbw
    [
      b ~width:32 "pmovsxbw" "pmovsxbw" Opcode.Pmovsxbw S.s_pmovsxbw;
      b ~width:32 "pmovsxbw_m" "pmovsxbw" Opcode.Pmovsxbw S.s_pmovsxbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x21) ]
    Opcode.Pmovsxbd
    [
      b ~width:32 "pmovsxbd" "pmovsxbd" Opcode.Pmovsxbd S.s_pmovsxbd;
      b ~width:32 "pmovsxbd_m" "pmovsxbd" Opcode.Pmovsxbd S.s_pmovsxbd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x22) ]
    Opcode.Pmovsxbq
    [
      b ~width:32 "pmovsxbq" "pmovsxbq" Opcode.Pmovsxbq S.s_pmovsxbq;
      b ~width:32 "pmovsxbq_m" "pmovsxbq" Opcode.Pmovsxbq S.s_pmovsxbq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x23) ]
    Opcode.Pmovsxwd
    [
      b ~width:32 "pmovsxwd" "pmovsxwd" Opcode.Pmovsxwd S.s_pmovsxwd;
      b ~width:32 "pmovsxwd_m" "pmovsxwd" Opcode.Pmovsxwd S.s_pmovsxwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x24) ]
    Opcode.Pmovsxwq
    [
      b ~width:32 "pmovsxwq" "pmovsxwq" Opcode.Pmovsxwq S.s_pmovsxwq;
      b ~width:32 "pmovsxwq_m" "pmovsxwq" Opcode.Pmovsxwq S.s_pmovsxwq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x25) ]
    Opcode.Pmovsxdq
    [
      b ~width:32 "pmovsxdq" "pmovsxdq" Opcode.Pmovsxdq S.s_pmovsxdq;
      b ~width:32 "pmovsxdq_m" "pmovsxdq" Opcode.Pmovsxdq S.s_pmovsxdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x30) ]
    Opcode.Pmovzxbw
    [
      b ~width:32 "pmovzxbw" "pmovzxbw" Opcode.Pmovzxbw S.s_pmovzxbw;
      b ~width:32 "pmovzxbw_m" "pmovzxbw" Opcode.Pmovzxbw S.s_pmovzxbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x31) ]
    Opcode.Pmovzxbd
    [
      b ~width:32 "pmovzxbd" "pmovzxbd" Opcode.Pmovzxbd S.s_pmovzxbd;
      b ~width:32 "pmovzxbd_m" "pmovzxbd" Opcode.Pmovzxbd S.s_pmovzxbd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x32) ]
    Opcode.Pmovzxbq
    [
      b ~width:32 "pmovzxbq" "pmovzxbq" Opcode.Pmovzxbq S.s_pmovzxbq;
      b ~width:32 "pmovzxbq_m" "pmovzxbq" Opcode.Pmovzxbq S.s_pmovzxbq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x33) ]
    Opcode.Pmovzxwd
    [
      b ~width:32 "pmovzxwd" "pmovzxwd" Opcode.Pmovzxwd S.s_pmovzxwd;
      b ~width:32 "pmovzxwd_m" "pmovzxwd" Opcode.Pmovzxwd S.s_pmovzxwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x34) ]
    Opcode.Pmovzxwq
    [
      b ~width:32 "pmovzxwq" "pmovzxwq" Opcode.Pmovzxwq S.s_pmovzxwq;
      b ~width:32 "pmovzxwq_m" "pmovzxwq" Opcode.Pmovzxwq S.s_pmovzxwq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x35) ]
    Opcode.Pmovzxdq
    [
      b ~width:32 "pmovzxdq" "pmovzxdq" Opcode.Pmovzxdq S.s_pmovzxdq;
      b ~width:32 "pmovzxdq_m" "pmovzxdq" Opcode.Pmovzxdq S.s_pmovzxdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x2a) ]
    Opcode.Movntdqa
    [ b ~width:32 "movntdqa_m" "movntdqa" Opcode.Movntdqa S.s_movntdqa_m ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x14) ]
    Opcode.Blendvps
    [
      b ~width:32 "blendvps" "blendvps" Opcode.Blendvps S.s_blendvps;
      b ~width:32 "blendvps_m" "blendvps" Opcode.Blendvps S.s_blendvps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x15) ]
    Opcode.Blendvpd
    [
      b ~width:32 "blendvpd" "blendvpd" Opcode.Blendvpd S.s_blendvpd;
      b ~width:32 "blendvpd_m" "blendvpd" Opcode.Blendvpd S.s_blendvpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_0f38, 0x10) ]
    Opcode.Pblendvb
    [
      b ~width:32 "pblendvb" "pblendvb" Opcode.Pblendvb S.s_pblendvb;
      b ~width:32 "pblendvb_m" "pblendvb" Opcode.Pblendvb S.s_pblendvb_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0c) ]
    Opcode.Blendps
    [
      b ~width:32 "blendps_i" "blendps" Opcode.Blendps S.s_blendps_i;
      b ~width:32 "blendps_im" "blendps" Opcode.Blendps S.s_blendps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0d) ]
    Opcode.Blendpd
    [
      b ~width:32 "blendpd_i" "blendpd" Opcode.Blendpd S.s_blendpd_i;
      b ~width:32 "blendpd_im" "blendpd" Opcode.Blendpd S.s_blendpd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x40) ]
    Opcode.Dpps
    [
      b ~width:32 "dpps_i" "dpps" Opcode.Dpps S.s_dpps_i;
      b ~width:32 "dpps_im" "dpps" Opcode.Dpps S.s_dpps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x41) ]
    Opcode.Dppd
    [
      b ~width:32 "dppd_i" "dppd" Opcode.Dppd S.s_dppd_i;
      b ~width:32 "dppd_im" "dppd" Opcode.Dppd S.s_dppd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x42) ]
    Opcode.Mpsadbw
    [
      b ~width:32 "mpsadbw_i" "mpsadbw" Opcode.Mpsadbw S.s_mpsadbw_i;
      b ~width:32 "mpsadbw_im" "mpsadbw" Opcode.Mpsadbw S.s_mpsadbw_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x0e) ]
    Opcode.Pblendw
    [
      b ~width:32 "pblendw_i" "pblendw" Opcode.Pblendw S.s_pblendw_i;
      b ~width:32 "pblendw_im" "pblendw" Opcode.Pblendw S.s_pblendw_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_0f3a, 0x21) ]
    Opcode.Insertps
    [
      b ~width:32 "insertps_i" "insertps" Opcode.Insertps S.s_insertps_i;
      b ~width:32 "insertps_im" "insertps" Opcode.Insertps S.s_insertps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_gpr_imm_0f3a, 0x20) ]
    Opcode.Pinsrb
    [
      b ~width:32 "pinsrb_i" "pinsrb" Opcode.Pinsrb S.s_pinsrb_i;
      b ~width:32 "pinsrb_im" "pinsrb" Opcode.Pinsrb S.s_pinsrb_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_gpr_imm_0f3a, 0x22) ]
    Opcode.Pinsrd
    [
      b ~width:32 "pinsrd_i" "pinsrd" Opcode.Pinsrd S.s_pinsrd_i;
      b ~width:32 "pinsrd_im" "pinsrd" Opcode.Pinsrd S.s_pinsrd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_gpr_imm_0f3a, 0x14) ]
    Opcode.Pextrb
    [
      b ~width:32 "pextrb_i" "pextrb" Opcode.Pextrb S.s_pextrb_i;
      b ~width:32 "pextrb_irm" "pextrb" Opcode.Pextrb S.s_pextrb_irm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_gpr_imm_0f3a, 0x16) ]
    Opcode.Pextrd
    [
      b ~width:32 "pextrd_i" "pextrd" Opcode.Pextrd S.s_pextrd_i;
      b ~width:32 "pextrd_irm" "pextrd" Opcode.Pextrd S.s_pextrd_irm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_gpr_imm_0f3a, 0x17) ]
    Opcode.Extractps
    [
      b ~width:32 "extractps_i" "extractps" Opcode.Extractps S.s_extractps_i;
      b ~width:32 "extractps_irm" "extractps" Opcode.Extractps S.s_extractps_irm;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x00) ]
    Opcode.Vpshufb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufb" "vpshufb" Opcode.Vpshufb S.s_vpshufb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufb_m" "vpshufb" Opcode.Vpshufb S.s_vpshufb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x01) ]
    Opcode.Vphaddw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddw" "vphaddw" Opcode.Vphaddw S.s_vphaddw;
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddw_m" "vphaddw" Opcode.Vphaddw S.s_vphaddw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x02) ]
    Opcode.Vphaddd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddd" "vphaddd" Opcode.Vphaddd S.s_vphaddd;
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddd_m" "vphaddd" Opcode.Vphaddd S.s_vphaddd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x03) ]
    Opcode.Vphaddsw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddsw" "vphaddsw" Opcode.Vphaddsw S.s_vphaddsw;
      b ~width:32 ~lengths:[ 128; 256 ] "vphaddsw_m" "vphaddsw" Opcode.Vphaddsw S.s_vphaddsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x04) ]
    Opcode.Vpmaddubsw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaddubsw" "vpmaddubsw" Opcode.Vpmaddubsw
        S.s_vpmaddubsw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaddubsw_m" "vpmaddubsw" Opcode.Vpmaddubsw
        S.s_vpmaddubsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x05) ]
    Opcode.Vphsubw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubw" "vphsubw" Opcode.Vphsubw S.s_vphsubw;
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubw_m" "vphsubw" Opcode.Vphsubw S.s_vphsubw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x06) ]
    Opcode.Vphsubd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubd" "vphsubd" Opcode.Vphsubd S.s_vphsubd;
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubd_m" "vphsubd" Opcode.Vphsubd S.s_vphsubd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x07) ]
    Opcode.Vphsubsw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubsw" "vphsubsw" Opcode.Vphsubsw S.s_vphsubsw;
      b ~width:32 ~lengths:[ 128; 256 ] "vphsubsw_m" "vphsubsw" Opcode.Vphsubsw S.s_vphsubsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x08) ]
    Opcode.Vpsignb
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignb" "vpsignb" Opcode.Vpsignb S.s_vpsignb;
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignb_m" "vpsignb" Opcode.Vpsignb S.s_vpsignb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x09) ]
    Opcode.Vpsignw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignw" "vpsignw" Opcode.Vpsignw S.s_vpsignw;
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignw_m" "vpsignw" Opcode.Vpsignw S.s_vpsignw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x0a) ]
    Opcode.Vpsignd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignd" "vpsignd" Opcode.Vpsignd S.s_vpsignd;
      b ~width:32 ~lengths:[ 128; 256 ] "vpsignd_m" "vpsignd" Opcode.Vpsignd S.s_vpsignd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x0b) ]
    Opcode.Vpmulhrsw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhrsw" "vpmulhrsw" Opcode.Vpmulhrsw S.s_vpmulhrsw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhrsw_m" "vpmulhrsw" Opcode.Vpmulhrsw
        S.s_vpmulhrsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x28) ]
    Opcode.Vpmuldq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmuldq" "vpmuldq" Opcode.Vpmuldq S.s_vpmuldq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmuldq_m" "vpmuldq" Opcode.Vpmuldq S.s_vpmuldq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x29) ]
    Opcode.Vpcmpeqq
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqq" "vpcmpeqq" Opcode.Vpcmpeqq S.s_vpcmpeqq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqq_rrk" "vpcmpeqq" Opcode.Vpcmpeqq
        S.s_vpcmpeqq_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqq_m" "vpcmpeqq" Opcode.Vpcmpeqq S.s_vpcmpeqq_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqq_mrk" "vpcmpeqq" Opcode.Vpcmpeqq
        S.s_vpcmpeqq_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x2b) ]
    Opcode.Vpackusdw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackusdw" "vpackusdw" Opcode.Vpackusdw S.s_vpackusdw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackusdw_m" "vpackusdw" Opcode.Vpackusdw
        S.s_vpackusdw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x37) ]
    Opcode.Vpcmpgtq
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtq" "vpcmpgtq" Opcode.Vpcmpgtq S.s_vpcmpgtq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtq_rrk" "vpcmpgtq" Opcode.Vpcmpgtq
        S.s_vpcmpgtq_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtq_m" "vpcmpgtq" Opcode.Vpcmpgtq S.s_vpcmpgtq_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtq_mrk" "vpcmpgtq" Opcode.Vpcmpgtq
        S.s_vpcmpgtq_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x38) ]
    Opcode.Vpminsb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsb" "vpminsb" Opcode.Vpminsb S.s_vpminsb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsb_m" "vpminsb" Opcode.Vpminsb S.s_vpminsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x39) ]
    Opcode.Vpminsd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsd" "vpminsd" Opcode.Vpminsd S.s_vpminsd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsd_m" "vpminsd" Opcode.Vpminsd S.s_vpminsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3a) ]
    Opcode.Vpminuw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminuw" "vpminuw" Opcode.Vpminuw S.s_vpminuw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminuw_m" "vpminuw" Opcode.Vpminuw S.s_vpminuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3b) ]
    Opcode.Vpminud
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminud" "vpminud" Opcode.Vpminud S.s_vpminud;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminud_m" "vpminud" Opcode.Vpminud S.s_vpminud_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3c) ]
    Opcode.Vpmaxsb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsb" "vpmaxsb" Opcode.Vpmaxsb S.s_vpmaxsb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsb_m" "vpmaxsb" Opcode.Vpmaxsb S.s_vpmaxsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3d) ]
    Opcode.Vpmaxsd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsd" "vpmaxsd" Opcode.Vpmaxsd S.s_vpmaxsd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsd_m" "vpmaxsd" Opcode.Vpmaxsd S.s_vpmaxsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3e) ]
    Opcode.Vpmaxuw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxuw" "vpmaxuw" Opcode.Vpmaxuw S.s_vpmaxuw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxuw_m" "vpmaxuw" Opcode.Vpmaxuw S.s_vpmaxuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x3f) ]
    Opcode.Vpmaxud
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxud" "vpmaxud" Opcode.Vpmaxud S.s_vpmaxud;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxud_m" "vpmaxud" Opcode.Vpmaxud S.s_vpmaxud_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_66, 0x40) ]
    Opcode.Vpmulld
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulld" "vpmulld" Opcode.Vpmulld S.s_vpmulld;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulld_m" "vpmulld" Opcode.Vpmulld S.s_vpmulld_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0f) ]
    Opcode.Vpalignr
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpalignr_i" "vpalignr" Opcode.Vpalignr S.s_vpalignr_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpalignr_im" "vpalignr" Opcode.Vpalignr
        S.s_vpalignr_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0c) ]
    Opcode.Vblendps
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vblendps_i" "vblendps" Opcode.Vblendps S.s_vblendps_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vblendps_im" "vblendps" Opcode.Vblendps S.s_vblendps_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0d) ]
    Opcode.Vblendpd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vblendpd_i" "vblendpd" Opcode.Vblendpd S.s_vblendpd_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vblendpd_im" "vblendpd" Opcode.Vblendpd S.s_vblendpd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0e) ]
    Opcode.Vpblendw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpblendw_i" "vpblendw" Opcode.Vpblendw S.s_vpblendw_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vpblendw_im" "vpblendw" Opcode.Vpblendw S.s_vpblendw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0a) ]
    Opcode.Vroundss
    [
      b ~width:32 "vroundss_i" "vroundss" Opcode.Vroundss S.s_vroundss_i;
      b ~width:32 "vroundss_im" "vroundss" Opcode.Vroundss S.s_vroundss_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x0b) ]
    Opcode.Vroundsd
    [
      b ~width:32 "vroundsd_i" "vroundsd" Opcode.Vroundsd S.s_vroundsd_i;
      b ~width:32 "vroundsd_im" "vroundsd" Opcode.Vroundsd S.s_vroundsd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x40) ]
    Opcode.Vdpps
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vdpps_i" "vdpps" Opcode.Vdpps S.s_vdpps_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vdpps_im" "vdpps" Opcode.Vdpps S.s_vdpps_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x41) ]
    Opcode.Vdppd
    [
      b ~width:32 "vdppd_i" "vdppd" Opcode.Vdppd S.s_vdppd_i;
      b ~width:32 "vdppd_im" "vdppd" Opcode.Vdppd S.s_vdppd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x42) ]
    Opcode.Vmpsadbw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmpsadbw_i" "vmpsadbw" Opcode.Vmpsadbw S.s_vmpsadbw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmpsadbw_im" "vmpsadbw" Opcode.Vmpsadbw
        S.s_vmpsadbw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map3_66, 0x21) ]
    Opcode.Vinsertps
    [
      b ~width:32 "vinsertps_i" "vinsertps" Opcode.Vinsertps S.s_vinsertps_i;
      b ~width:32 "vinsertps_im" "vinsertps" Opcode.Vinsertps S.s_vinsertps_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x1c) ]
    Opcode.Vpabsb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsb" "vpabsb" Opcode.Vpabsb S.s_vpabsb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsb_m" "vpabsb" Opcode.Vpabsb S.s_vpabsb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x1d) ]
    Opcode.Vpabsw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsw" "vpabsw" Opcode.Vpabsw S.s_vpabsw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsw_m" "vpabsw" Opcode.Vpabsw S.s_vpabsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x1e) ]
    Opcode.Vpabsd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsd" "vpabsd" Opcode.Vpabsd S.s_vpabsd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpabsd_m" "vpabsd" Opcode.Vpabsd S.s_vpabsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x41) ]
    Opcode.Vphminposuw
    [
      b ~width:32 "vphminposuw" "vphminposuw" Opcode.Vphminposuw S.s_vphminposuw;
      b ~width:32 "vphminposuw_m" "vphminposuw" Opcode.Vphminposuw S.s_vphminposuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x17) ]
    ~writes_flags:true Opcode.Vptest
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vptest" "vptest" Opcode.Vptest S.s_vptest;
      b ~width:32 ~lengths:[ 128; 256 ] "vptest_m" "vptest" Opcode.Vptest S.s_vptest_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x20) ]
    Opcode.Vpmovsxbw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovsxbw" "vpmovsxbw" Opcode.Vpmovsxbw S.s_vpmovsxbw;
      b ~width:32 "vpmovsxbw_rr" "vpmovsxbw" Opcode.Vpmovsxbw S.s_vpmovsxbw_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxbw_m" "vpmovsxbw" Opcode.Vpmovsxbw
        S.s_vpmovsxbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x21) ]
    Opcode.Vpmovsxbd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxbd" "vpmovsxbd" Opcode.Vpmovsxbd S.s_vpmovsxbd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxbd_m" "vpmovsxbd" Opcode.Vpmovsxbd
        S.s_vpmovsxbd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x22) ]
    Opcode.Vpmovsxbq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxbq" "vpmovsxbq" Opcode.Vpmovsxbq S.s_vpmovsxbq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxbq_m" "vpmovsxbq" Opcode.Vpmovsxbq
        S.s_vpmovsxbq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x23) ]
    Opcode.Vpmovsxwd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovsxwd" "vpmovsxwd" Opcode.Vpmovsxwd S.s_vpmovsxwd;
      b ~width:32 "vpmovsxwd_rr" "vpmovsxwd" Opcode.Vpmovsxwd S.s_vpmovsxwd_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxwd_m" "vpmovsxwd" Opcode.Vpmovsxwd
        S.s_vpmovsxwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x24) ]
    Opcode.Vpmovsxwq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxwq" "vpmovsxwq" Opcode.Vpmovsxwq S.s_vpmovsxwq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxwq_m" "vpmovsxwq" Opcode.Vpmovsxwq
        S.s_vpmovsxwq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x25) ]
    Opcode.Vpmovsxdq
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovsxdq" "vpmovsxdq" Opcode.Vpmovsxdq S.s_vpmovsxdq;
      b ~width:32 "vpmovsxdq_rr" "vpmovsxdq" Opcode.Vpmovsxdq S.s_vpmovsxdq_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovsxdq_m" "vpmovsxdq" Opcode.Vpmovsxdq
        S.s_vpmovsxdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x30) ]
    Opcode.Vpmovzxbw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovzxbw" "vpmovzxbw" Opcode.Vpmovzxbw S.s_vpmovzxbw;
      b ~width:32 "vpmovzxbw_rr" "vpmovzxbw" Opcode.Vpmovzxbw S.s_vpmovzxbw_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxbw_m" "vpmovzxbw" Opcode.Vpmovzxbw
        S.s_vpmovzxbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x31) ]
    Opcode.Vpmovzxbd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxbd" "vpmovzxbd" Opcode.Vpmovzxbd S.s_vpmovzxbd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxbd_m" "vpmovzxbd" Opcode.Vpmovzxbd
        S.s_vpmovzxbd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x32) ]
    Opcode.Vpmovzxbq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxbq" "vpmovzxbq" Opcode.Vpmovzxbq S.s_vpmovzxbq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxbq_m" "vpmovzxbq" Opcode.Vpmovzxbq
        S.s_vpmovzxbq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x33) ]
    Opcode.Vpmovzxwd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovzxwd" "vpmovzxwd" Opcode.Vpmovzxwd S.s_vpmovzxwd;
      b ~width:32 "vpmovzxwd_rr" "vpmovzxwd" Opcode.Vpmovzxwd S.s_vpmovzxwd_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxwd_m" "vpmovzxwd" Opcode.Vpmovzxwd
        S.s_vpmovzxwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x34) ]
    Opcode.Vpmovzxwq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxwq" "vpmovzxwq" Opcode.Vpmovzxwq S.s_vpmovzxwq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxwq_m" "vpmovzxwq" Opcode.Vpmovzxwq
        S.s_vpmovzxwq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x35) ]
    Opcode.Vpmovzxdq
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpmovzxdq" "vpmovzxdq" Opcode.Vpmovzxdq S.s_vpmovzxdq;
      b ~width:32 "vpmovzxdq_rr" "vpmovzxdq" Opcode.Vpmovzxdq S.s_vpmovzxdq_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmovzxdq_m" "vpmovzxdq" Opcode.Vpmovzxdq
        S.s_vpmovzxdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex3_map2_unop, 0x2a) ]
    Opcode.Vmovntdqa
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovntdqa_m" "vmovntdqa" Opcode.Vmovntdqa
        S.s_vmovntdqa_m;
    ]

let () =
  def Opcode.Movdqa
    [
      b ~width:32 "movdqa" "movdqa" Opcode.Movdqa S.s_movdqa;
      b ~width:32 "movdqa_m" "movdqa" Opcode.Movdqa S.s_movdqa_m;
      b ~width:32 "movdqa_rm" "movdqa" Opcode.Movdqa S.s_movdqa_rm;
    ]

let () =
  def Opcode.Movdqu
    [
      b ~width:32 "movdqu" "movdqu" Opcode.Movdqu S.s_movdqu;
      b ~width:32 "movdqu_m" "movdqu" Opcode.Movdqu S.s_movdqu_m;
      b ~width:32 "movdqu_rm" "movdqu" Opcode.Movdqu S.s_movdqu_rm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x54) ]
    Opcode.Andps
    [
      b ~width:32 "andps" "andps" Opcode.Andps S.s_andps;
      b ~width:32 "andps_m" "andps" Opcode.Andps S.s_andps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x55) ]
    Opcode.Andnps
    [
      b ~width:32 "andnps" "andnps" Opcode.Andnps S.s_andnps;
      b ~width:32 "andnps_m" "andnps" Opcode.Andnps S.s_andnps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x56) ]
    Opcode.Orps
    [
      b ~width:32 "orps" "orps" Opcode.Orps S.s_orps;
      b ~width:32 "orps_m" "orps" Opcode.Orps S.s_orps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x57) ]
    Opcode.Xorps
    [
      b ~width:32 "xorps" "xorps" Opcode.Xorps S.s_xorps;
      b ~width:32 "xorps_m" "xorps" Opcode.Xorps S.s_xorps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x54) ]
    Opcode.Andpd
    [
      b ~width:32 "andpd" "andpd" Opcode.Andpd S.s_andpd;
      b ~width:32 "andpd_m" "andpd" Opcode.Andpd S.s_andpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x55) ]
    Opcode.Andnpd
    [
      b ~width:32 "andnpd" "andnpd" Opcode.Andnpd S.s_andnpd;
      b ~width:32 "andnpd_m" "andnpd" Opcode.Andnpd S.s_andnpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x56) ]
    Opcode.Orpd
    [
      b ~width:32 "orpd" "orpd" Opcode.Orpd S.s_orpd;
      b ~width:32 "orpd_m" "orpd" Opcode.Orpd S.s_orpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x28) ]
    Opcode.Movaps
    [
      b ~width:32 "movaps" "movaps" Opcode.Movaps S.s_movaps;
      b ~width:32 "movaps_m" "movaps" Opcode.Movaps S.s_movaps_m;
      b ~width:32 "movaps_rm" "movaps" Opcode.Movaps S.s_movaps_rm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x10) ]
    Opcode.Movups
    [
      b ~width:32 "movups" "movups" Opcode.Movups S.s_movups;
      b ~width:32 "movups_m" "movups" Opcode.Movups S.s_movups_m;
      b ~width:32 "movups_rm" "movups" Opcode.Movups S.s_movups_rm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x10) ]
    Opcode.Movupd
    [
      b ~width:32 "movupd" "movupd" Opcode.Movupd S.s_movupd;
      b ~width:32 "movupd_m" "movupd" Opcode.Movupd S.s_movupd_m;
      b ~width:32 "movupd_rm" "movupd" Opcode.Movupd S.s_movupd_rm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x58) ]
    Opcode.Addps
    [
      b ~width:32 "addps" "addps" Opcode.Addps S.s_addps;
      b ~width:32 "addps_m" "addps" Opcode.Addps S.s_addps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5c) ]
    Opcode.Subps
    [
      b ~width:32 "subps" "subps" Opcode.Subps S.s_subps;
      b ~width:32 "subps_m" "subps" Opcode.Subps S.s_subps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x59) ]
    Opcode.Mulps
    [
      b ~width:32 "mulps" "mulps" Opcode.Mulps S.s_mulps;
      b ~width:32 "mulps_m" "mulps" Opcode.Mulps S.s_mulps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5e) ]
    Opcode.Divps
    [
      b ~width:32 "divps" "divps" Opcode.Divps S.s_divps;
      b ~width:32 "divps_m" "divps" Opcode.Divps S.s_divps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x58) ]
    Opcode.Addpd
    [
      b ~width:32 "addpd" "addpd" Opcode.Addpd S.s_addpd;
      b ~width:32 "addpd_m" "addpd" Opcode.Addpd S.s_addpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5c) ]
    Opcode.Subpd
    [
      b ~width:32 "subpd" "subpd" Opcode.Subpd S.s_subpd;
      b ~width:32 "subpd_m" "subpd" Opcode.Subpd S.s_subpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x59) ]
    Opcode.Mulpd
    [
      b ~width:32 "mulpd" "mulpd" Opcode.Mulpd S.s_mulpd;
      b ~width:32 "mulpd_m" "mulpd" Opcode.Mulpd S.s_mulpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5e) ]
    Opcode.Divpd
    [
      b ~width:32 "divpd" "divpd" Opcode.Divpd S.s_divpd;
      b ~width:32 "divpd_m" "divpd" Opcode.Divpd S.s_divpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5f) ]
    Opcode.Maxss
    [
      b ~width:32 "maxss" "maxss" Opcode.Maxss S.s_maxss;
      b ~width:32 "maxss_m" "maxss" Opcode.Maxss S.s_maxss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x5d) ]
    Opcode.Minss
    [
      b ~width:32 "minss" "minss" Opcode.Minss S.s_minss;
      b ~width:32 "minss_m" "minss" Opcode.Minss S.s_minss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x5f) ]
    Opcode.Maxsd
    [
      b ~width:32 "maxsd" "maxsd" Opcode.Maxsd S.s_maxsd;
      b ~width:32 "maxsd_m" "maxsd" Opcode.Maxsd S.s_maxsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x5d) ]
    Opcode.Minsd
    [
      b ~width:32 "minsd" "minsd" Opcode.Minsd S.s_minsd;
      b ~width:32 "minsd_m" "minsd" Opcode.Minsd S.s_minsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5f) ]
    Opcode.Maxps
    [
      b ~width:32 "maxps" "maxps" Opcode.Maxps S.s_maxps;
      b ~width:32 "maxps_m" "maxps" Opcode.Maxps S.s_maxps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x5d) ]
    Opcode.Minps
    [
      b ~width:32 "minps" "minps" Opcode.Minps S.s_minps;
      b ~width:32 "minps_m" "minps" Opcode.Minps S.s_minps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5f) ]
    Opcode.Maxpd
    [
      b ~width:32 "maxpd" "maxpd" Opcode.Maxpd S.s_maxpd;
      b ~width:32 "maxpd_m" "maxpd" Opcode.Maxpd S.s_maxpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x5d) ]
    Opcode.Minpd
    [
      b ~width:32 "minpd" "minpd" Opcode.Minpd S.s_minpd;
      b ~width:32 "minpd_m" "minpd" Opcode.Minpd S.s_minpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f3, 0x51) ]
    Opcode.Sqrtss
    [
      b ~width:32 "sqrtss" "sqrtss" Opcode.Sqrtss S.s_sqrtss;
      b ~width:32 "sqrtss_m" "sqrtss" Opcode.Sqrtss S.s_sqrtss_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_f2, 0x51) ]
    Opcode.Sqrtsd
    [
      b ~width:32 "sqrtsd" "sqrtsd" Opcode.Sqrtsd S.s_sqrtsd;
      b ~width:32 "sqrtsd_m" "sqrtsd" Opcode.Sqrtsd S.s_sqrtsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_none, 0x51) ]
    Opcode.Sqrtps
    [
      b ~width:32 "sqrtps" "sqrtps" Opcode.Sqrtps S.s_sqrtps;
      b ~width:32 "sqrtps_m" "sqrtps" Opcode.Sqrtps S.s_sqrtps_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_66, 0x51) ]
    Opcode.Sqrtpd
    [
      b ~width:32 "sqrtpd" "sqrtpd" Opcode.Sqrtpd S.s_sqrtpd;
      b ~width:32 "sqrtpd_m" "sqrtpd" Opcode.Sqrtpd S.s_sqrtpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_none, 0xc6) ]
    Opcode.Shufps
    [
      b ~width:32 "shufps_i" "shufps" Opcode.Shufps S.s_shufps_i;
      b ~width:32 "shufps_im" "shufps" Opcode.Shufps S.s_shufps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_66, 0xc6) ]
    Opcode.Shufpd
    [
      b ~width:32 "shufpd_i" "shufpd" Opcode.Shufpd S.s_shufpd_i;
      b ~width:32 "shufpd_im" "shufpd" Opcode.Shufpd S.s_shufpd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_f3, 0xc2) ]
    Opcode.Cmpss
    [
      b ~width:32 "cmpss_i" "cmpss" Opcode.Cmpss S.s_cmpss_i;
      b ~width:32 "cmpss_im" "cmpss" Opcode.Cmpss S.s_cmpss_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_f2, 0xc2) ]
    Opcode.Cmpsd
    [
      b ~width:32 "cmpsd_i" "cmpsd" Opcode.Cmpsd S.s_cmpsd_i;
      b ~width:32 "cmpsd_im" "cmpsd" Opcode.Cmpsd S.s_cmpsd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_none, 0xc2) ]
    Opcode.Cmpps
    [
      b ~width:32 "cmpps_i" "cmpps" Opcode.Cmpps S.s_cmpps_i;
      b ~width:32 "cmpps_im" "cmpps" Opcode.Cmpps S.s_cmpps_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_66, 0xc2) ]
    Opcode.Cmppd
    [
      b ~width:32 "cmppd_i" "cmppd" Opcode.Cmppd S.s_cmppd_i;
      b ~width:32 "cmppd_im" "cmppd" Opcode.Cmppd S.s_cmppd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_66, 0x70) ]
    Opcode.Pshufd
    [
      b ~width:32 "pshufd_i" "pshufd" Opcode.Pshufd S.s_pshufd_i;
      b ~width:32 "pshufd_im" "pshufd" Opcode.Pshufd S.s_pshufd_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_f2, 0x70) ]
    Opcode.Pshuflw
    [
      b ~width:32 "pshuflw_i" "pshuflw" Opcode.Pshuflw S.s_pshuflw_i;
      b ~width:32 "pshuflw_im" "pshuflw" Opcode.Pshuflw S.s_pshuflw_im;
    ]

let () =
  def
    ~forms:[ (Form.Sse_binop_imm_f3, 0x70) ]
    Opcode.Pshufhw
    [
      b ~width:32 "pshufhw_i" "pshufhw" Opcode.Pshufhw S.s_pshufhw_i;
      b ~width:32 "pshufhw_im" "pshufhw" Opcode.Pshufhw S.s_pshufhw_im;
    ]

let () =
  def Opcode.Movd
    [
      b ~width:32 "movd_q" "movd" Opcode.Movd S.s_movd_q;
      b ~width:32 "movd_rq" "movd" Opcode.Movd S.s_movd_rq;
      b ~width:32 "movd_rx" "movd" Opcode.Movd S.s_movd_rx;
      b ~width:32 "movd_x" "movd" Opcode.Movd S.s_movd_x;
      b ~width:32 "movd_mq" "movd" Opcode.Movd S.s_movd_mq;
      b ~width:32 "movd_mx" "movd" Opcode.Movd S.s_movd_mx;
      b ~width:32 "movd_qm" "movd" Opcode.Movd S.s_movd_qm;
      b ~width:32 "movd_xm" "movd" Opcode.Movd S.s_movd_xm;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x58) ]
    Opcode.Vaddsd
    [
      b ~width:32 "vaddsd" "vaddsd" Opcode.Vaddsd S.s_vaddsd;
      b ~width:32 "vaddsd_m" "vaddsd" Opcode.Vaddsd S.s_vaddsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x5c) ]
    Opcode.Vsubsd
    [
      b ~width:32 "vsubsd" "vsubsd" Opcode.Vsubsd S.s_vsubsd;
      b ~width:32 "vsubsd_m" "vsubsd" Opcode.Vsubsd S.s_vsubsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x59) ]
    Opcode.Vmulsd
    [
      b ~width:32 "vmulsd" "vmulsd" Opcode.Vmulsd S.s_vmulsd;
      b ~width:32 "vmulsd_m" "vmulsd" Opcode.Vmulsd S.s_vmulsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x5e) ]
    Opcode.Vdivsd
    [
      b ~width:32 "vdivsd" "vdivsd" Opcode.Vdivsd S.s_vdivsd;
      b ~width:32 "vdivsd_m" "vdivsd" Opcode.Vdivsd S.s_vdivsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x58) ]
    Opcode.Vaddss
    [
      b ~width:32 "vaddss" "vaddss" Opcode.Vaddss S.s_vaddss;
      b ~width:32 "vaddss_m" "vaddss" Opcode.Vaddss S.s_vaddss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x5c) ]
    Opcode.Vsubss
    [
      b ~width:32 "vsubss" "vsubss" Opcode.Vsubss S.s_vsubss;
      b ~width:32 "vsubss_m" "vsubss" Opcode.Vsubss S.s_vsubss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x59) ]
    Opcode.Vmulss
    [
      b ~width:32 "vmulss" "vmulss" Opcode.Vmulss S.s_vmulss;
      b ~width:32 "vmulss_m" "vmulss" Opcode.Vmulss S.s_vmulss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x5e) ]
    Opcode.Vdivss
    [
      b ~width:32 "vdivss" "vdivss" Opcode.Vdivss S.s_vdivss;
      b ~width:32 "vdivss_m" "vdivss" Opcode.Vdivss S.s_vdivss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x58); (Form.Evex_ps, 0x58) ]
    Opcode.Vaddps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vaddps" "vaddps" Opcode.Vaddps S.s_vaddps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vaddps_m" "vaddps" Opcode.Vaddps S.s_vaddps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x5c); (Form.Evex_ps, 0x5c) ]
    Opcode.Vsubps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsubps" "vsubps" Opcode.Vsubps S.s_vsubps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsubps_m" "vsubps" Opcode.Vsubps S.s_vsubps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x59); (Form.Evex_ps, 0x59) ]
    Opcode.Vmulps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmulps" "vmulps" Opcode.Vmulps S.s_vmulps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmulps_m" "vmulps" Opcode.Vmulps S.s_vmulps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x5e); (Form.Evex_ps, 0x5e) ]
    Opcode.Vdivps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vdivps" "vdivps" Opcode.Vdivps S.s_vdivps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vdivps_m" "vdivps" Opcode.Vdivps S.s_vdivps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x58); (Form.Evex_pd, 0x58) ]
    Opcode.Vaddpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vaddpd" "vaddpd" Opcode.Vaddpd S.s_vaddpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vaddpd_m" "vaddpd" Opcode.Vaddpd S.s_vaddpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x5c); (Form.Evex_pd, 0x5c) ]
    Opcode.Vsubpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsubpd" "vsubpd" Opcode.Vsubpd S.s_vsubpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsubpd_m" "vsubpd" Opcode.Vsubpd S.s_vsubpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x59); (Form.Evex_pd, 0x59) ]
    Opcode.Vmulpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmulpd" "vmulpd" Opcode.Vmulpd S.s_vmulpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmulpd_m" "vmulpd" Opcode.Vmulpd S.s_vmulpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x5e); (Form.Evex_pd, 0x5e) ]
    Opcode.Vdivpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vdivpd" "vdivpd" Opcode.Vdivpd S.s_vdivpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vdivpd_m" "vdivpd" Opcode.Vdivpd S.s_vdivpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x54) ]
    Opcode.Vandps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandps" "vandps" Opcode.Vandps S.s_vandps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandps_m" "vandps" Opcode.Vandps S.s_vandps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x55) ]
    Opcode.Vandnps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandnps" "vandnps" Opcode.Vandnps S.s_vandnps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandnps_m" "vandnps" Opcode.Vandnps S.s_vandnps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x56) ]
    Opcode.Vorps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vorps" "vorps" Opcode.Vorps S.s_vorps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vorps_m" "vorps" Opcode.Vorps S.s_vorps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x57) ]
    Opcode.Vxorps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vxorps" "vxorps" Opcode.Vxorps S.s_vxorps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vxorps_m" "vxorps" Opcode.Vxorps S.s_vxorps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x54) ]
    Opcode.Vandpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandpd" "vandpd" Opcode.Vandpd S.s_vandpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandpd_m" "vandpd" Opcode.Vandpd S.s_vandpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x55) ]
    Opcode.Vandnpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandnpd" "vandnpd" Opcode.Vandnpd S.s_vandnpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vandnpd_m" "vandnpd" Opcode.Vandnpd S.s_vandnpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x56) ]
    Opcode.Vorpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vorpd" "vorpd" Opcode.Vorpd S.s_vorpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vorpd_m" "vorpd" Opcode.Vorpd S.s_vorpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x57) ]
    Opcode.Vxorpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vxorpd" "vxorpd" Opcode.Vxorpd S.s_vxorpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vxorpd_m" "vxorpd" Opcode.Vxorpd S.s_vxorpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x14); (Form.Evex_ps, 0x14) ]
    Opcode.Vunpcklps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpcklps" "vunpcklps" Opcode.Vunpcklps S.s_vunpcklps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpcklps_m" "vunpcklps" Opcode.Vunpcklps
        S.s_vunpcklps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x15); (Form.Evex_ps, 0x15) ]
    Opcode.Vunpckhps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpckhps" "vunpckhps" Opcode.Vunpckhps S.s_vunpckhps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpckhps_m" "vunpckhps" Opcode.Vunpckhps
        S.s_vunpckhps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x14); (Form.Evex_pd, 0x14) ]
    Opcode.Vunpcklpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpcklpd" "vunpcklpd" Opcode.Vunpcklpd S.s_vunpcklpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpcklpd_m" "vunpcklpd" Opcode.Vunpcklpd
        S.s_vunpcklpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x15); (Form.Evex_pd, 0x15) ]
    Opcode.Vunpckhpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpckhpd" "vunpckhpd" Opcode.Vunpckhpd S.s_vunpckhpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vunpckhpd_m" "vunpckhpd" Opcode.Vunpckhpd
        S.s_vunpckhpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x6c) ]
    Opcode.Vpunpcklqdq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklqdq" "vpunpcklqdq" Opcode.Vpunpcklqdq
        S.s_vpunpcklqdq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklqdq_m" "vpunpcklqdq" Opcode.Vpunpcklqdq
        S.s_vpunpcklqdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x6d) ]
    Opcode.Vpunpckhqdq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhqdq" "vpunpckhqdq" Opcode.Vpunpckhqdq
        S.s_vpunpckhqdq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhqdq_m" "vpunpckhqdq" Opcode.Vpunpckhqdq
        S.s_vpunpckhqdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x60) ]
    Opcode.Vpunpcklbw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklbw" "vpunpcklbw" Opcode.Vpunpcklbw
        S.s_vpunpcklbw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklbw_m" "vpunpcklbw" Opcode.Vpunpcklbw
        S.s_vpunpcklbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x68) ]
    Opcode.Vpunpckhbw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhbw" "vpunpckhbw" Opcode.Vpunpckhbw
        S.s_vpunpckhbw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhbw_m" "vpunpckhbw" Opcode.Vpunpckhbw
        S.s_vpunpckhbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x61) ]
    Opcode.Vpunpcklwd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklwd" "vpunpcklwd" Opcode.Vpunpcklwd
        S.s_vpunpcklwd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpcklwd_m" "vpunpcklwd" Opcode.Vpunpcklwd
        S.s_vpunpcklwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x69) ]
    Opcode.Vpunpckhwd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhwd" "vpunpckhwd" Opcode.Vpunpckhwd
        S.s_vpunpckhwd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhwd_m" "vpunpckhwd" Opcode.Vpunpckhwd
        S.s_vpunpckhwd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x62) ]
    Opcode.Vpunpckldq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckldq" "vpunpckldq" Opcode.Vpunpckldq
        S.s_vpunpckldq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckldq_m" "vpunpckldq" Opcode.Vpunpckldq
        S.s_vpunpckldq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x6a) ]
    Opcode.Vpunpckhdq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhdq" "vpunpckhdq" Opcode.Vpunpckhdq
        S.s_vpunpckhdq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpunpckhdq_m" "vpunpckhdq" Opcode.Vpunpckhdq
        S.s_vpunpckhdq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xfc) ]
    Opcode.Vpaddb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddb" "vpaddb" Opcode.Vpaddb S.s_vpaddb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddb_m" "vpaddb" Opcode.Vpaddb S.s_vpaddb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xfd) ]
    Opcode.Vpaddw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddw" "vpaddw" Opcode.Vpaddw S.s_vpaddw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddw_m" "vpaddw" Opcode.Vpaddw S.s_vpaddw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xfe) ]
    Opcode.Vpaddd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddd" "vpaddd" Opcode.Vpaddd S.s_vpaddd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddd_m" "vpaddd" Opcode.Vpaddd S.s_vpaddd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xd4) ]
    Opcode.Vpaddq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddq" "vpaddq" Opcode.Vpaddq S.s_vpaddq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpaddq_m" "vpaddq" Opcode.Vpaddq S.s_vpaddq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf8) ]
    Opcode.Vpsubb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubb" "vpsubb" Opcode.Vpsubb S.s_vpsubb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubb_m" "vpsubb" Opcode.Vpsubb S.s_vpsubb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf9) ]
    Opcode.Vpsubw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubw" "vpsubw" Opcode.Vpsubw S.s_vpsubw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubw_m" "vpsubw" Opcode.Vpsubw S.s_vpsubw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xfa) ]
    Opcode.Vpsubd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubd" "vpsubd" Opcode.Vpsubd S.s_vpsubd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubd_m" "vpsubd" Opcode.Vpsubd S.s_vpsubd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xfb) ]
    Opcode.Vpsubq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubq" "vpsubq" Opcode.Vpsubq S.s_vpsubq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsubq_m" "vpsubq" Opcode.Vpsubq S.s_vpsubq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x74) ]
    Opcode.Vpcmpeqb
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqb" "vpcmpeqb" Opcode.Vpcmpeqb S.s_vpcmpeqb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqb_rrk" "vpcmpeqb" Opcode.Vpcmpeqb
        S.s_vpcmpeqb_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqb_m" "vpcmpeqb" Opcode.Vpcmpeqb S.s_vpcmpeqb_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqb_mrk" "vpcmpeqb" Opcode.Vpcmpeqb
        S.s_vpcmpeqb_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x75) ]
    Opcode.Vpcmpeqw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqw" "vpcmpeqw" Opcode.Vpcmpeqw S.s_vpcmpeqw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqw_rrk" "vpcmpeqw" Opcode.Vpcmpeqw
        S.s_vpcmpeqw_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqw_m" "vpcmpeqw" Opcode.Vpcmpeqw S.s_vpcmpeqw_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqw_mrk" "vpcmpeqw" Opcode.Vpcmpeqw
        S.s_vpcmpeqw_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x76) ]
    Opcode.Vpcmpeqd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqd" "vpcmpeqd" Opcode.Vpcmpeqd S.s_vpcmpeqd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqd_rrk" "vpcmpeqd" Opcode.Vpcmpeqd
        S.s_vpcmpeqd_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpeqd_m" "vpcmpeqd" Opcode.Vpcmpeqd S.s_vpcmpeqd_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpeqd_mrk" "vpcmpeqd" Opcode.Vpcmpeqd
        S.s_vpcmpeqd_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x64) ]
    Opcode.Vpcmpgtb
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtb" "vpcmpgtb" Opcode.Vpcmpgtb S.s_vpcmpgtb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtb_rrk" "vpcmpgtb" Opcode.Vpcmpgtb
        S.s_vpcmpgtb_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtb_m" "vpcmpgtb" Opcode.Vpcmpgtb S.s_vpcmpgtb_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtb_mrk" "vpcmpgtb" Opcode.Vpcmpgtb
        S.s_vpcmpgtb_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x65) ]
    Opcode.Vpcmpgtw
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtw" "vpcmpgtw" Opcode.Vpcmpgtw S.s_vpcmpgtw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtw_rrk" "vpcmpgtw" Opcode.Vpcmpgtw
        S.s_vpcmpgtw_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtw_m" "vpcmpgtw" Opcode.Vpcmpgtw S.s_vpcmpgtw_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtw_mrk" "vpcmpgtw" Opcode.Vpcmpgtw
        S.s_vpcmpgtw_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x66) ]
    Opcode.Vpcmpgtd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtd" "vpcmpgtd" Opcode.Vpcmpgtd S.s_vpcmpgtd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtd_rrk" "vpcmpgtd" Opcode.Vpcmpgtd
        S.s_vpcmpgtd_rrk;
      b ~width:32 ~lengths:[ 128; 256 ] "vpcmpgtd_m" "vpcmpgtd" Opcode.Vpcmpgtd S.s_vpcmpgtd_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpcmpgtd_mrk" "vpcmpgtd" Opcode.Vpcmpgtd
        S.s_vpcmpgtd_mrk;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x63) ]
    Opcode.Vpacksswb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpacksswb" "vpacksswb" Opcode.Vpacksswb S.s_vpacksswb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpacksswb_m" "vpacksswb" Opcode.Vpacksswb
        S.s_vpacksswb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x6b) ]
    Opcode.Vpackssdw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackssdw" "vpackssdw" Opcode.Vpackssdw S.s_vpackssdw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackssdw_m" "vpackssdw" Opcode.Vpackssdw
        S.s_vpackssdw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x67) ]
    Opcode.Vpackuswb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackuswb" "vpackuswb" Opcode.Vpackuswb S.s_vpackuswb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpackuswb_m" "vpackuswb" Opcode.Vpackuswb
        S.s_vpackuswb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xdb) ]
    Opcode.Vpand
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpand" "vpand" Opcode.Vpand S.s_vpand;
      b ~width:32 ~lengths:[ 128; 256 ] "vpand_m" "vpand" Opcode.Vpand S.s_vpand_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xdf) ]
    Opcode.Vpandn
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpandn" "vpandn" Opcode.Vpandn S.s_vpandn;
      b ~width:32 ~lengths:[ 128; 256 ] "vpandn_m" "vpandn" Opcode.Vpandn S.s_vpandn_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xeb) ]
    Opcode.Vpor
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vpor" "vpor" Opcode.Vpor S.s_vpor;
      b ~width:32 ~lengths:[ 128; 256 ] "vpor_m" "vpor" Opcode.Vpor S.s_vpor_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xda) ]
    Opcode.Vpminub
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminub" "vpminub" Opcode.Vpminub S.s_vpminub;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminub_m" "vpminub" Opcode.Vpminub S.s_vpminub_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xde) ]
    Opcode.Vpmaxub
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxub" "vpmaxub" Opcode.Vpmaxub S.s_vpmaxub;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxub_m" "vpmaxub" Opcode.Vpmaxub S.s_vpmaxub_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xea) ]
    Opcode.Vpminsw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsw" "vpminsw" Opcode.Vpminsw S.s_vpminsw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpminsw_m" "vpminsw" Opcode.Vpminsw S.s_vpminsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xee) ]
    Opcode.Vpmaxsw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsw" "vpmaxsw" Opcode.Vpmaxsw S.s_vpmaxsw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmaxsw_m" "vpmaxsw" Opcode.Vpmaxsw S.s_vpmaxsw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x5f) ]
    Opcode.Vmaxsd
    [
      b ~width:32 "vmaxsd" "vmaxsd" Opcode.Vmaxsd S.s_vmaxsd;
      b ~width:32 "vmaxsd_m" "vmaxsd" Opcode.Vmaxsd S.s_vmaxsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x5d) ]
    Opcode.Vminsd
    [
      b ~width:32 "vminsd" "vminsd" Opcode.Vminsd S.s_vminsd;
      b ~width:32 "vminsd_m" "vminsd" Opcode.Vminsd S.s_vminsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x5f) ]
    Opcode.Vmaxss
    [
      b ~width:32 "vmaxss" "vmaxss" Opcode.Vmaxss S.s_vmaxss;
      b ~width:32 "vmaxss_m" "vmaxss" Opcode.Vmaxss S.s_vmaxss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x5d) ]
    Opcode.Vminss
    [
      b ~width:32 "vminss" "vminss" Opcode.Vminss S.s_vminss;
      b ~width:32 "vminss_m" "vminss" Opcode.Vminss S.s_vminss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x5f); (Form.Evex_ps, 0x5f) ]
    Opcode.Vmaxps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmaxps" "vmaxps" Opcode.Vmaxps S.s_vmaxps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmaxps_m" "vmaxps" Opcode.Vmaxps S.s_vmaxps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_none, 0x5d); (Form.Evex_ps, 0x5d) ]
    Opcode.Vminps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vminps" "vminps" Opcode.Vminps S.s_vminps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vminps_m" "vminps" Opcode.Vminps S.s_vminps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x5f); (Form.Evex_pd, 0x5f) ]
    Opcode.Vmaxpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmaxpd" "vmaxpd" Opcode.Vmaxpd S.s_vmaxpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmaxpd_m" "vmaxpd" Opcode.Vmaxpd S.s_vmaxpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0x5d); (Form.Evex_pd, 0x5d) ]
    Opcode.Vminpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vminpd" "vminpd" Opcode.Vminpd S.s_vminpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vminpd_m" "vminpd" Opcode.Vminpd S.s_vminpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f2, 0x51) ]
    Opcode.Vsqrtsd
    [
      b ~width:32 "vsqrtsd" "vsqrtsd" Opcode.Vsqrtsd S.s_vsqrtsd;
      b ~width:32 "vsqrtsd_m" "vsqrtsd" Opcode.Vsqrtsd S.s_vsqrtsd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_f3, 0x51) ]
    Opcode.Vsqrtss
    [
      b ~width:32 "vsqrtss" "vsqrtss" Opcode.Vsqrtss S.s_vsqrtss;
      b ~width:32 "vsqrtss_m" "vsqrtss" Opcode.Vsqrtss S.s_vsqrtss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x51) ]
    Opcode.Vsqrtps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsqrtps" "vsqrtps" Opcode.Vsqrtps S.s_vsqrtps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsqrtps_m" "vsqrtps" Opcode.Vsqrtps S.s_vsqrtps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x51) ]
    Opcode.Vsqrtpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsqrtpd" "vsqrtpd" Opcode.Vsqrtpd S.s_vsqrtpd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vsqrtpd_m" "vsqrtpd" Opcode.Vsqrtpd S.s_vsqrtpd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x28) ]
    Opcode.Vmovaps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovaps" "vmovaps" Opcode.Vmovaps S.s_vmovaps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovaps_rm" "vmovaps" Opcode.Vmovaps S.s_vmovaps_rm;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovaps_m" "vmovaps" Opcode.Vmovaps S.s_vmovaps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x10) ]
    Opcode.Vmovups
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovups" "vmovups" Opcode.Vmovups S.s_vmovups;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovups_rm" "vmovups" Opcode.Vmovups S.s_vmovups_rm;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovups_m" "vmovups" Opcode.Vmovups S.s_vmovups_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x28) ]
    Opcode.Vmovapd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovapd" "vmovapd" Opcode.Vmovapd S.s_vmovapd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovapd_rm" "vmovapd" Opcode.Vmovapd S.s_vmovapd_rm;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovapd_m" "vmovapd" Opcode.Vmovapd S.s_vmovapd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x10) ]
    Opcode.Vmovupd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovupd" "vmovupd" Opcode.Vmovupd S.s_vmovupd;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovupd_rm" "vmovupd" Opcode.Vmovupd S.s_vmovupd_rm;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vmovupd_m" "vmovupd" Opcode.Vmovupd S.s_vmovupd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x2f) ]
    ~writes_flags:true Opcode.Vcomisd
    [
      b ~width:32 "vcomisd" "vcomisd" Opcode.Vcomisd S.s_vcomisd;
      b ~width:32 "vcomisd_m" "vcomisd" Opcode.Vcomisd S.s_vcomisd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x2e) ]
    ~writes_flags:true Opcode.Vucomisd
    [
      b ~width:32 "vucomisd" "vucomisd" Opcode.Vucomisd S.s_vucomisd;
      b ~width:32 "vucomisd_m" "vucomisd" Opcode.Vucomisd S.s_vucomisd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x2f) ]
    ~writes_flags:true Opcode.Vcomiss
    [
      b ~width:32 "vcomiss" "vcomiss" Opcode.Vcomiss S.s_vcomiss;
      b ~width:32 "vcomiss_m" "vcomiss" Opcode.Vcomiss S.s_vcomiss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x2e) ]
    ~writes_flags:true Opcode.Vucomiss
    [
      b ~width:32 "vucomiss" "vucomiss" Opcode.Vucomiss S.s_vucomiss;
      b ~width:32 "vucomiss_m" "vucomiss" Opcode.Vucomiss S.s_vucomiss_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x5a) ]
    Opcode.Vcvtps2pd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vcvtps2pd" "vcvtps2pd" Opcode.Vcvtps2pd S.s_vcvtps2pd;
      b ~width:32 "vcvtps2pd_rr" "vcvtps2pd" Opcode.Vcvtps2pd S.s_vcvtps2pd_rr;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvtps2pd_m" "vcvtps2pd" Opcode.Vcvtps2pd
        S.s_vcvtps2pd_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x5a) ]
    Opcode.Vcvtpd2ps
    [
      b ~width:32 "vcvtpd2ps" "vcvtpd2ps" Opcode.Vcvtpd2ps S.s_vcvtpd2ps;
      b ~width:32 "vcvtpd2ps_rr" "vcvtpd2ps" Opcode.Vcvtpd2ps S.s_vcvtpd2ps_rr;
      b ~width:32 "vcvtpd2ps_zy" "vcvtpd2ps" Opcode.Vcvtpd2ps S.s_vcvtpd2ps_zy;
      b ~width:32 "vcvtpd2ps_m" "vcvtpd2ps" Opcode.Vcvtpd2ps S.s_vcvtpd2ps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_none, 0x5b) ]
    Opcode.Vcvtdq2ps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvtdq2ps" "vcvtdq2ps" Opcode.Vcvtdq2ps S.s_vcvtdq2ps;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvtdq2ps_m" "vcvtdq2ps" Opcode.Vcvtdq2ps
        S.s_vcvtdq2ps_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x5b) ]
    Opcode.Vcvtps2dq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvtps2dq" "vcvtps2dq" Opcode.Vcvtps2dq S.s_vcvtps2dq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvtps2dq_m" "vcvtps2dq" Opcode.Vcvtps2dq
        S.s_vcvtps2dq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_f3, 0x5b) ]
    Opcode.Vcvttps2dq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvttps2dq" "vcvttps2dq" Opcode.Vcvttps2dq
        S.s_vcvttps2dq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vcvttps2dq_m" "vcvttps2dq" Opcode.Vcvttps2dq
        S.s_vcvttps2dq_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_none, 0xc6) ]
    Opcode.Vshufps
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vshufps_i" "vshufps" Opcode.Vshufps S.s_vshufps_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vshufps_im" "vshufps" Opcode.Vshufps S.s_vshufps_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_66, 0xc6) ]
    Opcode.Vshufpd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vshufpd_i" "vshufpd" Opcode.Vshufpd S.s_vshufpd_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vshufpd_im" "vshufpd" Opcode.Vshufpd S.s_vshufpd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_f3, 0xc2) ]
    Opcode.Vcmpss
    [
      b ~width:32 "vcmpss_i" "vcmpss" Opcode.Vcmpss S.s_vcmpss_i;
      b ~width:32 "vcmpss_im" "vcmpss" Opcode.Vcmpss S.s_vcmpss_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_f2, 0xc2) ]
    Opcode.Vcmpsd
    [
      b ~width:32 "vcmpsd_i" "vcmpsd" Opcode.Vcmpsd S.s_vcmpsd_i;
      b ~width:32 "vcmpsd_im" "vcmpsd" Opcode.Vcmpsd S.s_vcmpsd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_none, 0xc2) ]
    Opcode.Vcmpps
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vcmpps_i" "vcmpps" Opcode.Vcmpps S.s_vcmpps_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vcmpps_im" "vcmpps" Opcode.Vcmpps S.s_vcmpps_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_binop_imm_66, 0xc2) ]
    Opcode.Vcmppd
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vcmppd_i" "vcmppd" Opcode.Vcmppd S.s_vcmppd_i;
      b ~width:32 ~lengths:[ 128; 256 ] "vcmppd_im" "vcmppd" Opcode.Vcmppd S.s_vcmppd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_imm_66, 0x70) ]
    Opcode.Vpshufd
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufd_i" "vpshufd" Opcode.Vpshufd S.s_vpshufd_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufd_im" "vpshufd" Opcode.Vpshufd S.s_vpshufd_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_imm_f2, 0x70) ]
    Opcode.Vpshuflw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshuflw_i" "vpshuflw" Opcode.Vpshuflw S.s_vpshuflw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshuflw_im" "vpshuflw" Opcode.Vpshuflw
        S.s_vpshuflw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_imm_f3, 0x70) ]
    Opcode.Vpshufhw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufhw_i" "vpshufhw" Opcode.Vpshufhw S.s_vpshufhw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpshufhw_im" "vpshufhw" Opcode.Vpshufhw
        S.s_vpshufhw_im;
    ]

let () =
  def Opcode.Vmovd
    [
      b ~width:32 "vmovd" "vmovd" Opcode.Vmovd S.s_vmovd;
      b ~width:32 "vmovd_rr" "vmovd" Opcode.Vmovd S.s_vmovd_rr;
      b ~width:32 "vmovd_xx" "vmovd" Opcode.Vmovd S.s_vmovd_xx;
      b ~width:32 "vmovd_m" "vmovd" Opcode.Vmovd S.s_vmovd_m;
      b ~width:32 "vmovd_rm" "vmovd" Opcode.Vmovd S.s_vmovd_rm;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xd5) ]
    Opcode.Vpmullw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmullw" "vpmullw" Opcode.Vpmullw S.s_vpmullw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmullw_m" "vpmullw" Opcode.Vpmullw S.s_vpmullw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe5) ]
    Opcode.Vpmulhw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhw" "vpmulhw" Opcode.Vpmulhw S.s_vpmulhw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhw_m" "vpmulhw" Opcode.Vpmulhw S.s_vpmulhw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe4) ]
    Opcode.Vpmulhuw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhuw" "vpmulhuw" Opcode.Vpmulhuw S.s_vpmulhuw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpmulhuw_m" "vpmulhuw" Opcode.Vpmulhuw S.s_vpmulhuw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe0) ]
    Opcode.Vpavgb
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpavgb" "vpavgb" Opcode.Vpavgb S.s_vpavgb;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpavgb_m" "vpavgb" Opcode.Vpavgb S.s_vpavgb_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe3) ]
    Opcode.Vpavgw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpavgw" "vpavgw" Opcode.Vpavgw S.s_vpavgw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpavgw_m" "vpavgw" Opcode.Vpavgw S.s_vpavgw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf6) ]
    Opcode.Vpsadbw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsadbw" "vpsadbw" Opcode.Vpsadbw S.s_vpsadbw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsadbw_m" "vpsadbw" Opcode.Vpsadbw S.s_vpsadbw_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf1); (Form.Vex_shift_ext, 0x06); (Form.Vex_shift_opcode, 0x71) ]
    Opcode.Vpsllw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllw" "vpsllw" Opcode.Vpsllw S.s_vpsllw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllw_i" "vpsllw" Opcode.Vpsllw S.s_vpsllw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllw_m" "vpsllw" Opcode.Vpsllw S.s_vpsllw_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllw_im" "vpsllw" Opcode.Vpsllw S.s_vpsllw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf2); (Form.Vex_shift_ext, 0x06); (Form.Vex_shift_opcode, 0x72) ]
    Opcode.Vpslld
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslld" "vpslld" Opcode.Vpslld S.s_vpslld;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslld_i" "vpslld" Opcode.Vpslld S.s_vpslld_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslld_m" "vpslld" Opcode.Vpslld S.s_vpslld_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslld_im" "vpslld" Opcode.Vpslld S.s_vpslld_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xf3); (Form.Vex_shift_ext, 0x06); (Form.Vex_shift_opcode, 0x73) ]
    Opcode.Vpsllq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllq" "vpsllq" Opcode.Vpsllq S.s_vpsllq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllq_i" "vpsllq" Opcode.Vpsllq S.s_vpsllq_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllq_m" "vpsllq" Opcode.Vpsllq S.s_vpsllq_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsllq_im" "vpsllq" Opcode.Vpsllq S.s_vpsllq_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xd1); (Form.Vex_shift_ext, 0x02); (Form.Vex_shift_opcode, 0x71) ]
    Opcode.Vpsrlw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlw" "vpsrlw" Opcode.Vpsrlw S.s_vpsrlw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlw_i" "vpsrlw" Opcode.Vpsrlw S.s_vpsrlw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlw_m" "vpsrlw" Opcode.Vpsrlw S.s_vpsrlw_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlw_im" "vpsrlw" Opcode.Vpsrlw S.s_vpsrlw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xd2); (Form.Vex_shift_ext, 0x02); (Form.Vex_shift_opcode, 0x72) ]
    Opcode.Vpsrld
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrld" "vpsrld" Opcode.Vpsrld S.s_vpsrld;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrld_i" "vpsrld" Opcode.Vpsrld S.s_vpsrld_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrld_m" "vpsrld" Opcode.Vpsrld S.s_vpsrld_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrld_im" "vpsrld" Opcode.Vpsrld S.s_vpsrld_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xd3); (Form.Vex_shift_ext, 0x02); (Form.Vex_shift_opcode, 0x73) ]
    Opcode.Vpsrlq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlq" "vpsrlq" Opcode.Vpsrlq S.s_vpsrlq;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlq_i" "vpsrlq" Opcode.Vpsrlq S.s_vpsrlq_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlq_m" "vpsrlq" Opcode.Vpsrlq S.s_vpsrlq_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrlq_im" "vpsrlq" Opcode.Vpsrlq S.s_vpsrlq_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe1); (Form.Vex_shift_ext, 0x04); (Form.Vex_shift_opcode, 0x71) ]
    Opcode.Vpsraw
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsraw" "vpsraw" Opcode.Vpsraw S.s_vpsraw;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsraw_i" "vpsraw" Opcode.Vpsraw S.s_vpsraw_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsraw_m" "vpsraw" Opcode.Vpsraw S.s_vpsraw_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsraw_im" "vpsraw" Opcode.Vpsraw S.s_vpsraw_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_scalar_66, 0xe2); (Form.Vex_shift_ext, 0x04); (Form.Vex_shift_opcode, 0x72) ]
    Opcode.Vpsrad
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrad" "vpsrad" Opcode.Vpsrad S.s_vpsrad;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrad_i" "vpsrad" Opcode.Vpsrad S.s_vpsrad_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrad_m" "vpsrad" Opcode.Vpsrad S.s_vpsrad_m;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrad_im" "vpsrad" Opcode.Vpsrad S.s_vpsrad_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_shift_ext, 0x07); (Form.Vex_shift_opcode, 0x73) ]
    Opcode.Vpslldq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslldq_i" "vpslldq" Opcode.Vpslldq S.s_vpslldq_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpslldq_im" "vpslldq" Opcode.Vpslldq S.s_vpslldq_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_shift_ext, 0x03); (Form.Vex_shift_opcode, 0x73) ]
    Opcode.Vpsrldq
    [
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrldq_i" "vpsrldq" Opcode.Vpsrldq S.s_vpsrldq_i;
      b ~width:32 ~lengths:[ 128; 256; 512 ] "vpsrldq_im" "vpsrldq" Opcode.Vpsrldq S.s_vpsrldq_im;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_66, 0x6f) ]
    Opcode.Vmovdqa
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqa" "vmovdqa" Opcode.Vmovdqa S.s_vmovdqa;
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqa_rm" "vmovdqa" Opcode.Vmovdqa S.s_vmovdqa_rm;
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqa_m" "vmovdqa" Opcode.Vmovdqa S.s_vmovdqa_m;
    ]

let () =
  def
    ~forms:[ (Form.Vex_unop_f3, 0x6f) ]
    Opcode.Vmovdqu
    [
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqu" "vmovdqu" Opcode.Vmovdqu S.s_vmovdqu;
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqu_rm" "vmovdqu" Opcode.Vmovdqu S.s_vmovdqu_rm;
      b ~width:32 ~lengths:[ 128; 256 ] "vmovdqu_m" "vmovdqu" Opcode.Vmovdqu S.s_vmovdqu_m;
    ]

let () =
  def Opcode.Pinsrw
    [
      b ~width:32 "pinsrw_irq" "pinsrw" Opcode.Pinsrw S.s_pinsrw_irq;
      b ~width:32 "pinsrw_irx" "pinsrw" Opcode.Pinsrw S.s_pinsrw_irx;
      b ~width:32 "pinsrw_imq" "pinsrw" Opcode.Pinsrw S.s_pinsrw_imq;
      b ~width:32 "pinsrw_imx" "pinsrw" Opcode.Pinsrw S.s_pinsrw_imx;
    ]

let () =
  def Opcode.Pextrw
    [
      b ~width:32 "pextrw_iq" "pextrw" Opcode.Pextrw S.s_pextrw_iq;
      b ~width:32 "pextrw_i" "pextrw" Opcode.Pextrw S.s_pextrw_i;
      b ~width:32 "pextrw_irm" "pextrw" Opcode.Pextrw S.s_pextrw_irm;
    ]

let () =
  def Opcode.Vpinsrw
    [
      b ~width:32 "vpinsrw_i" "vpinsrw" Opcode.Vpinsrw S.s_vpinsrw_i;
      b ~width:32 "vpinsrw_im" "vpinsrw" Opcode.Vpinsrw S.s_vpinsrw_im;
    ]

let () =
  def Opcode.Vpextrw
    [
      b ~width:32 "vpextrw_i" "vpextrw" Opcode.Vpextrw S.s_vpextrw_i;
      b ~width:32 "vpextrw_irm" "vpextrw" Opcode.Vpextrw S.s_vpextrw_irm;
    ]

let () =
  def
    ~forms:[ (Form.Sse_movmsk_none, 0x50) ]
    Opcode.Movmskps
    [ b ~width:32 "movmskps" "movmskps" Opcode.Movmskps S.s_movmskps ]

let () =
  def
    ~forms:[ (Form.Sse_movmsk_66, 0x50) ]
    Opcode.Movmskpd
    [ b ~width:32 "movmskpd" "movmskpd" Opcode.Movmskpd S.s_movmskpd ]

let () =
  def
    ~forms:[ (Form.Sse_movmsk_66, 0xd7) ]
    Opcode.Pmovmskb
    [
      b ~width:32 "pmovmskb_q" "pmovmskb" Opcode.Pmovmskb S.s_pmovmskb_q;
      b ~width:32 "pmovmskb_x" "pmovmskb" Opcode.Pmovmskb S.s_pmovmskb_x;
    ]

let () =
  def
    ~forms:[ (Form.Vex_movmsk_none, 0x50) ]
    Opcode.Vmovmskps
    [ b ~width:32 ~lengths:[ 128; 256 ] "vmovmskps" "vmovmskps" Opcode.Vmovmskps S.s_vmovmskps ]

let () =
  def
    ~forms:[ (Form.Vex_movmsk_66, 0x50) ]
    Opcode.Vmovmskpd
    [ b ~width:32 ~lengths:[ 128; 256 ] "vmovmskpd" "vmovmskpd" Opcode.Vmovmskpd S.s_vmovmskpd ]

let () =
  def
    ~forms:[ (Form.Vex_movmsk_66, 0xd7) ]
    Opcode.Vpmovmskb
    [ b ~width:32 ~lengths:[ 128; 256 ] "vpmovmskb" "vpmovmskb" Opcode.Vpmovmskb S.s_vpmovmskb ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fldl
    [ b ~width:32 "fldl_m" "fldl" Opcode.Fldl S.s_fldl_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fstpl
    [ b ~width:32 "fstpl_m" "fstpl" Opcode.Fstpl S.s_fstpl_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fstps
    [ b ~width:32 "fstps_m" "fstps" Opcode.Fstps S.s_fstps_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Flds
    [
      b ~width:32 "flds" "flds" Opcode.Flds S.s_flds;
      b ~width:32 "flds_m" "flds" Opcode.Flds S.s_flds_m;
    ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fildll
    [ b ~width:32 "fildll_m" "fildll" Opcode.Fildll S.s_fildll_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fadds
    [
      b ~width:32 "fadds" "fadds" Opcode.Fadds S.s_fadds;
      b ~width:32 "fadds_m" "fadds" Opcode.Fadds S.s_fadds_m;
    ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fadd
    [
      b ~width:32 "fadd" "fadd" Opcode.Fadd S.s_fadd;
      b ~width:32 "fadd_r" "fadd" Opcode.Fadd S.s_fadd_r;
    ]

let () = def ~feature:X86_x87.component.feature Opcode.Fucomp []

let () =
  def ~feature:X86_x87.component.feature Opcode.Fnstcw
    [ b ~width:32 "fnstcw_m" "fnstcw" Opcode.Fnstcw S.s_fnstcw_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fldcw
    [ b ~width:32 "fldcw_m" "fldcw" Opcode.Fldcw S.s_fldcw_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fistpll
    [ b ~width:32 "fistpll_m" "fistpll" Opcode.Fistpll S.s_fistpll_m ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fsubs
    [
      b ~width:32 "fsubs" "fsubs" Opcode.Fsubs S.s_fsubs;
      b ~width:32 "fsubs_m" "fsubs" Opcode.Fsubs S.s_fsubs_m;
    ]

let () =
  def ~feature:X86_x87.component.feature Opcode.Fnstsw
    [
      b ~width:32 "fnstsw" "fnstsw" Opcode.Fnstsw S.s_fnstsw;
      b ~width:32 "fnstsw_m" "fnstsw" Opcode.Fnstsw S.s_fnstsw_m;
    ]

let () = def ~writes_flags:true Opcode.Sahf [ b ~width:32 "sahf" "sahf" Opcode.Sahf S.s_sahf ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.O)
    [
      b ~widths:[ 16; 32; 64 ] "cmovo" "cmovo" (Opcode.Cmov Cc.O) S.s_cmovo;
      b ~widths:[ 16; 32; 64 ] "cmovo_m" "cmovo" (Opcode.Cmov Cc.O) S.s_cmovo_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.O)
    [
      b ~width:32 ~mode:Only32 "jo_m32" "jo" (Opcode.Jcc Cc.O) S.s_jo_m32;
      b ~width:64 ~mode:Only64 "jo_m64" "jo" (Opcode.Jcc Cc.O) S.s_jo_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.O)
    [
      b ~width:8 "seto" "seto" (Opcode.Setcc Cc.O) S.s_seto;
      b ~width:8 "seto_m" "seto" (Opcode.Setcc Cc.O) S.s_seto_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.No)
    [
      b ~widths:[ 16; 32; 64 ] "cmovno" "cmovno" (Opcode.Cmov Cc.No) S.s_cmovno;
      b ~widths:[ 16; 32; 64 ] "cmovno_m" "cmovno" (Opcode.Cmov Cc.No) S.s_cmovno_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.No)
    [
      b ~width:32 ~mode:Only32 "jno_m32" "jno" (Opcode.Jcc Cc.No) S.s_jno_m32;
      b ~width:64 ~mode:Only64 "jno_m64" "jno" (Opcode.Jcc Cc.No) S.s_jno_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.No)
    [
      b ~width:8 "setno" "setno" (Opcode.Setcc Cc.No) S.s_setno;
      b ~width:8 "setno_m" "setno" (Opcode.Setcc Cc.No) S.s_setno_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.B)
    [
      b ~widths:[ 16; 32; 64 ] "cmovb" "cmovb" (Opcode.Cmov Cc.B) S.s_cmovb;
      b ~widths:[ 16; 32; 64 ] "cmovb_m" "cmovb" (Opcode.Cmov Cc.B) S.s_cmovb_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.B)
    [
      b ~width:32 ~mode:Only32 "jb_m32" "jb" (Opcode.Jcc Cc.B) S.s_jb_m32;
      b ~width:64 ~mode:Only64 "jb_m64" "jb" (Opcode.Jcc Cc.B) S.s_jb_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.B)
    [
      b ~width:8 "setb" "setb" (Opcode.Setcc Cc.B) S.s_setb;
      b ~width:8 "setb_m" "setb" (Opcode.Setcc Cc.B) S.s_setb_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Ae)
    [
      b ~widths:[ 16; 32; 64 ] "cmovae" "cmovae" (Opcode.Cmov Cc.Ae) S.s_cmovae;
      b ~widths:[ 16; 32; 64 ] "cmovae_m" "cmovae" (Opcode.Cmov Cc.Ae) S.s_cmovae_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Ae)
    [
      b ~width:32 ~mode:Only32 "jae_m32" "jae" (Opcode.Jcc Cc.Ae) S.s_jae_m32;
      b ~width:64 ~mode:Only64 "jae_m64" "jae" (Opcode.Jcc Cc.Ae) S.s_jae_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Ae)
    [
      b ~width:8 "setae" "setae" (Opcode.Setcc Cc.Ae) S.s_setae;
      b ~width:8 "setae_m" "setae" (Opcode.Setcc Cc.Ae) S.s_setae_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.E)
    [
      b ~widths:[ 16; 32; 64 ] "cmove" "cmove" (Opcode.Cmov Cc.E) S.s_cmove;
      b ~widths:[ 16; 32; 64 ] "cmove_m" "cmove" (Opcode.Cmov Cc.E) S.s_cmove_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.E)
    [
      b ~width:32 ~mode:Only32 "je_m32" "je" (Opcode.Jcc Cc.E) S.s_je_m32;
      b ~width:64 ~mode:Only64 "je_m64" "je" (Opcode.Jcc Cc.E) S.s_je_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.E)
    [
      b ~width:8 "sete" "sete" (Opcode.Setcc Cc.E) S.s_sete;
      b ~width:8 "sete_m" "sete" (Opcode.Setcc Cc.E) S.s_sete_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Ne)
    [
      b ~widths:[ 16; 32; 64 ] "cmovne" "cmovne" (Opcode.Cmov Cc.Ne) S.s_cmovne;
      b ~widths:[ 16; 32; 64 ] "cmovne_m" "cmovne" (Opcode.Cmov Cc.Ne) S.s_cmovne_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Ne)
    [
      b ~width:32 ~mode:Only32 "jne_m32" "jne" (Opcode.Jcc Cc.Ne) S.s_jne_m32;
      b ~width:64 ~mode:Only64 "jne_m64" "jne" (Opcode.Jcc Cc.Ne) S.s_jne_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Ne)
    [
      b ~width:8 "setne" "setne" (Opcode.Setcc Cc.Ne) S.s_setne;
      b ~width:8 "setne_m" "setne" (Opcode.Setcc Cc.Ne) S.s_setne_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Be)
    [
      b ~widths:[ 16; 32; 64 ] "cmovbe" "cmovbe" (Opcode.Cmov Cc.Be) S.s_cmovbe;
      b ~widths:[ 16; 32; 64 ] "cmovbe_m" "cmovbe" (Opcode.Cmov Cc.Be) S.s_cmovbe_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Be)
    [
      b ~width:32 ~mode:Only32 "jbe_m32" "jbe" (Opcode.Jcc Cc.Be) S.s_jbe_m32;
      b ~width:64 ~mode:Only64 "jbe_m64" "jbe" (Opcode.Jcc Cc.Be) S.s_jbe_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Be)
    [
      b ~width:8 "setbe" "setbe" (Opcode.Setcc Cc.Be) S.s_setbe;
      b ~width:8 "setbe_m" "setbe" (Opcode.Setcc Cc.Be) S.s_setbe_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.A)
    [
      b ~widths:[ 16; 32; 64 ] "cmova" "cmova" (Opcode.Cmov Cc.A) S.s_cmova;
      b ~widths:[ 16; 32; 64 ] "cmova_m" "cmova" (Opcode.Cmov Cc.A) S.s_cmova_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.A)
    [
      b ~width:32 ~mode:Only32 "ja_m32" "ja" (Opcode.Jcc Cc.A) S.s_ja_m32;
      b ~width:64 ~mode:Only64 "ja_m64" "ja" (Opcode.Jcc Cc.A) S.s_ja_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.A)
    [
      b ~width:8 "seta" "seta" (Opcode.Setcc Cc.A) S.s_seta;
      b ~width:8 "seta_m" "seta" (Opcode.Setcc Cc.A) S.s_seta_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.S)
    [
      b ~widths:[ 16; 32; 64 ] "cmovs" "cmovs" (Opcode.Cmov Cc.S) S.s_cmovs;
      b ~widths:[ 16; 32; 64 ] "cmovs_m" "cmovs" (Opcode.Cmov Cc.S) S.s_cmovs_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.S)
    [
      b ~width:32 ~mode:Only32 "js_m32" "js" (Opcode.Jcc Cc.S) S.s_js_m32;
      b ~width:64 ~mode:Only64 "js_m64" "js" (Opcode.Jcc Cc.S) S.s_js_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.S)
    [
      b ~width:8 "sets" "sets" (Opcode.Setcc Cc.S) S.s_sets;
      b ~width:8 "sets_m" "sets" (Opcode.Setcc Cc.S) S.s_sets_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Ns)
    [
      b ~widths:[ 16; 32; 64 ] "cmovns" "cmovns" (Opcode.Cmov Cc.Ns) S.s_cmovns;
      b ~widths:[ 16; 32; 64 ] "cmovns_m" "cmovns" (Opcode.Cmov Cc.Ns) S.s_cmovns_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Ns)
    [
      b ~width:32 ~mode:Only32 "jns_m32" "jns" (Opcode.Jcc Cc.Ns) S.s_jns_m32;
      b ~width:64 ~mode:Only64 "jns_m64" "jns" (Opcode.Jcc Cc.Ns) S.s_jns_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Ns)
    [
      b ~width:8 "setns" "setns" (Opcode.Setcc Cc.Ns) S.s_setns;
      b ~width:8 "setns_m" "setns" (Opcode.Setcc Cc.Ns) S.s_setns_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.P)
    [
      b ~widths:[ 16; 32; 64 ] "cmovp" "cmovp" (Opcode.Cmov Cc.P) S.s_cmovp;
      b ~widths:[ 16; 32; 64 ] "cmovp_m" "cmovp" (Opcode.Cmov Cc.P) S.s_cmovp_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.P)
    [
      b ~width:32 ~mode:Only32 "jp_m32" "jp" (Opcode.Jcc Cc.P) S.s_jp_m32;
      b ~width:64 ~mode:Only64 "jp_m64" "jp" (Opcode.Jcc Cc.P) S.s_jp_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.P)
    [
      b ~width:8 "setp" "setp" (Opcode.Setcc Cc.P) S.s_setp;
      b ~width:8 "setp_m" "setp" (Opcode.Setcc Cc.P) S.s_setp_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Np)
    [
      b ~widths:[ 16; 32; 64 ] "cmovnp" "cmovnp" (Opcode.Cmov Cc.Np) S.s_cmovnp;
      b ~widths:[ 16; 32; 64 ] "cmovnp_m" "cmovnp" (Opcode.Cmov Cc.Np) S.s_cmovnp_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Np)
    [
      b ~width:32 ~mode:Only32 "jnp_m32" "jnp" (Opcode.Jcc Cc.Np) S.s_jnp_m32;
      b ~width:64 ~mode:Only64 "jnp_m64" "jnp" (Opcode.Jcc Cc.Np) S.s_jnp_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Np)
    [
      b ~width:8 "setnp" "setnp" (Opcode.Setcc Cc.Np) S.s_setnp;
      b ~width:8 "setnp_m" "setnp" (Opcode.Setcc Cc.Np) S.s_setnp_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.L)
    [
      b ~widths:[ 16; 32; 64 ] "cmovl" "cmovl" (Opcode.Cmov Cc.L) S.s_cmovl;
      b ~widths:[ 16; 32; 64 ] "cmovl_m" "cmovl" (Opcode.Cmov Cc.L) S.s_cmovl_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.L)
    [
      b ~width:32 ~mode:Only32 "jl_m32" "jl" (Opcode.Jcc Cc.L) S.s_jl_m32;
      b ~width:64 ~mode:Only64 "jl_m64" "jl" (Opcode.Jcc Cc.L) S.s_jl_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.L)
    [
      b ~width:8 "setl" "setl" (Opcode.Setcc Cc.L) S.s_setl;
      b ~width:8 "setl_m" "setl" (Opcode.Setcc Cc.L) S.s_setl_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Ge)
    [
      b ~widths:[ 16; 32; 64 ] "cmovge" "cmovge" (Opcode.Cmov Cc.Ge) S.s_cmovge;
      b ~widths:[ 16; 32; 64 ] "cmovge_m" "cmovge" (Opcode.Cmov Cc.Ge) S.s_cmovge_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Ge)
    [
      b ~width:32 ~mode:Only32 "jge_m32" "jge" (Opcode.Jcc Cc.Ge) S.s_jge_m32;
      b ~width:64 ~mode:Only64 "jge_m64" "jge" (Opcode.Jcc Cc.Ge) S.s_jge_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Ge)
    [
      b ~width:8 "setge" "setge" (Opcode.Setcc Cc.Ge) S.s_setge;
      b ~width:8 "setge_m" "setge" (Opcode.Setcc Cc.Ge) S.s_setge_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.Le)
    [
      b ~widths:[ 16; 32; 64 ] "cmovle" "cmovle" (Opcode.Cmov Cc.Le) S.s_cmovle;
      b ~widths:[ 16; 32; 64 ] "cmovle_m" "cmovle" (Opcode.Cmov Cc.Le) S.s_cmovle_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.Le)
    [
      b ~width:32 ~mode:Only32 "jle_m32" "jle" (Opcode.Jcc Cc.Le) S.s_jle_m32;
      b ~width:64 ~mode:Only64 "jle_m64" "jle" (Opcode.Jcc Cc.Le) S.s_jle_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.Le)
    [
      b ~width:8 "setle" "setle" (Opcode.Setcc Cc.Le) S.s_setle;
      b ~width:8 "setle_m" "setle" (Opcode.Setcc Cc.Le) S.s_setle_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Cmov Cc.G)
    [
      b ~widths:[ 16; 32; 64 ] "cmovg" "cmovg" (Opcode.Cmov Cc.G) S.s_cmovg;
      b ~widths:[ 16; 32; 64 ] "cmovg_m" "cmovg" (Opcode.Cmov Cc.G) S.s_cmovg_m;
    ]

let () =
  def ~reads_flags:true (Opcode.Jcc Cc.G)
    [
      b ~width:32 ~mode:Only32 "jg_m32" "jg" (Opcode.Jcc Cc.G) S.s_jg_m32;
      b ~width:64 ~mode:Only64 "jg_m64" "jg" (Opcode.Jcc Cc.G) S.s_jg_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Setcc Cc.G)
    [
      b ~width:8 "setg" "setg" (Opcode.Setcc Cc.G) S.s_setg;
      b ~width:8 "setg_m" "setg" (Opcode.Setcc Cc.G) S.s_setg_m;
    ]

let () =
  def (Opcode.Short_branch Short.Loop)
    [
      b ~width:32 ~mode:Only32 "loop_m32" "loop" (Opcode.Short_branch Short.Loop) S.s_loop_m32;
      b ~width:64 ~mode:Only64 "loop_m64" "loop" (Opcode.Short_branch Short.Loop) S.s_loop_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Short_branch Short.Loope)
    [
      b ~width:32 ~mode:Only32 "loope_m32" "loope" (Opcode.Short_branch Short.Loope) S.s_loope_m32;
      b ~width:64 ~mode:Only64 "loope_m64" "loope" (Opcode.Short_branch Short.Loope) S.s_loope_m64;
    ]

let () =
  def ~reads_flags:true (Opcode.Short_branch Short.Loopne)
    [
      b ~width:32 ~mode:Only32 "loopne_m32" "loopne" (Opcode.Short_branch Short.Loopne)
        S.s_loopne_m32;
      b ~width:64 ~mode:Only64 "loopne_m64" "loopne" (Opcode.Short_branch Short.Loopne)
        S.s_loopne_m64;
    ]

let () =
  def ~mode:Only32 (Opcode.Short_branch Short.Jcxz)
    [ b ~width:32 ~mode:Only32 "jcxz" "jcxz" (Opcode.Short_branch Short.Jcxz) S.s_jcxz ]

let () =
  def (Opcode.Short_branch Short.Jecxz)
    [
      b ~width:32 ~mode:Only32 "jecxz_m32" "jecxz" (Opcode.Short_branch Short.Jecxz) S.s_jecxz_m32;
      b ~width:64 ~mode:Only64 "jecxz_m64" "jecxz" (Opcode.Short_branch Short.Jecxz) S.s_jecxz_m64;
    ]

let () =
  def ~mode:Only64 (Opcode.Short_branch Short.Jrcxz)
    [ b ~width:64 ~mode:Only64 "jrcxz" "jrcxz" (Opcode.Short_branch Short.Jrcxz) S.s_jrcxz ]

let () =
  def
    (Opcode.Movzx { src_width = 8 })
    [
      b ~widths:[ 16; 32; 64 ] "movzb" "movzb%s" (Opcode.Movzx { src_width = 8 }) S.s_movzb;
      b ~widths:[ 16; 32; 64 ] "movzb_m" "movzb%s" (Opcode.Movzx { src_width = 8 }) S.s_movzb_m;
    ]

let () =
  def
    (Opcode.Movzx { src_width = 16 })
    [
      b ~widths:[ 32; 64 ] "movzw" "movzw%s" (Opcode.Movzx { src_width = 16 }) S.s_movzw;
      b ~widths:[ 32; 64 ] "movzw_m" "movzw%s" (Opcode.Movzx { src_width = 16 }) S.s_movzw_m;
    ]

let () =
  def
    (Opcode.Movsx { src_width = 8 })
    [
      b ~widths:[ 16; 32; 64 ] "movsb" "movsb%s" (Opcode.Movsx { src_width = 8 }) S.s_movsb;
      b ~widths:[ 16; 32; 64 ] "movsb_m" "movsb%s" (Opcode.Movsx { src_width = 8 }) S.s_movsb_m;
    ]

let () =
  def
    (Opcode.Movsx { src_width = 16 })
    [
      b ~widths:[ 32; 64 ] "movsw" "movsw%s" (Opcode.Movsx { src_width = 16 }) S.s_movsw;
      b ~widths:[ 32; 64 ] "movsw_m" "movsw%s" (Opcode.Movsx { src_width = 16 }) S.s_movsw_m;
    ]

let () =
  def ~mode:Only64
    (Opcode.Movsx { src_width = 32 })
    [
      b ~width:64 ~mode:Only64 "movsl" "movslq" (Opcode.Movsx { src_width = 32 }) S.s_movsl;
      b ~width:64 ~mode:Only64 "movsl_m" "movslq" (Opcode.Movsx { src_width = 32 }) S.s_movsl_m;
    ]

let all = Array.of_list (List.rev !registry)

(* {1 Builders} *)
let add a = mk Opcode.Add S.s_add a
let add_rrr a = mk Opcode.Add S.s_add_rrr a
let add_rm a = mk Opcode.Add S.s_add_rm a
let add_i a = mk Opcode.Add S.s_add_i a
let add_i_e a = mk_e Opcode.Add S.s_add_i a
let add_m a = mk Opcode.Add S.s_add_m a
let add_rmr a = mk Opcode.Add S.s_add_rmr a
let add_irr a = mk Opcode.Add S.s_add_irr a
let add_irr_e a = mk_e Opcode.Add S.s_add_irr a
let add_mrr a = mk Opcode.Add S.s_add_mrr a
let addb_im a = mk ~width:8 Opcode.Add S.s_addb_im a
let addb_im_e a = mk_e ~width:8 Opcode.Add S.s_addb_im a
let addl_im a = mk ~width:32 Opcode.Add S.s_addl_im a
let addl_im_e a = mk_e ~width:32 Opcode.Add S.s_addl_im a
let addq_im a = mk ~width:64 Opcode.Add S.s_addq_im a
let addq_im_e a = mk_e ~width:64 Opcode.Add S.s_addq_im a
let add_im a = mk Opcode.Add S.s_add_im a
let add_im_e a = mk_e Opcode.Add S.s_add_im a
let sub a = mk Opcode.Sub S.s_sub a
let sub_rrr a = mk Opcode.Sub S.s_sub_rrr a
let sub_rm a = mk Opcode.Sub S.s_sub_rm a
let sub_i a = mk Opcode.Sub S.s_sub_i a
let sub_i_e a = mk_e Opcode.Sub S.s_sub_i a
let sub_m a = mk Opcode.Sub S.s_sub_m a
let sub_rmr a = mk Opcode.Sub S.s_sub_rmr a
let sub_irr a = mk Opcode.Sub S.s_sub_irr a
let sub_irr_e a = mk_e Opcode.Sub S.s_sub_irr a
let sub_mrr a = mk Opcode.Sub S.s_sub_mrr a
let subb_im a = mk ~width:8 Opcode.Sub S.s_subb_im a
let subb_im_e a = mk_e ~width:8 Opcode.Sub S.s_subb_im a
let subl_im a = mk ~width:32 Opcode.Sub S.s_subl_im a
let subl_im_e a = mk_e ~width:32 Opcode.Sub S.s_subl_im a
let subq_im a = mk ~width:64 Opcode.Sub S.s_subq_im a
let subq_im_e a = mk_e ~width:64 Opcode.Sub S.s_subq_im a
let sub_im a = mk Opcode.Sub S.s_sub_im a
let sub_im_e a = mk_e Opcode.Sub S.s_sub_im a
let mov a = mk Opcode.Mov S.s_mov a
let mov_qq a = mk ~width:64 Opcode.Mov S.s_mov_qq a
let mov_q a = mk ~width:64 Opcode.Mov S.s_mov_q a
let mov_rq a = mk ~width:64 Opcode.Mov S.s_mov_rq a
let mov_rm a = mk Opcode.Mov S.s_mov_rm a
let mov_rs a = mk Opcode.Mov S.s_mov_rs a
let mov_i a = mk Opcode.Mov S.s_mov_i a
let mov_i_e a = mk_e Opcode.Mov S.s_mov_i a
let mov_m a = mk Opcode.Mov S.s_mov_m a
let mov_mq a = mk ~width:64 Opcode.Mov S.s_mov_mq a
let mov_mx a = mk ~width:64 Opcode.Mov S.s_mov_mx a
let mov_qm a = mk ~width:64 Opcode.Mov S.s_mov_qm a
let mov_s a = mk Opcode.Mov S.s_mov_s a
let mov_xm a = mk ~width:64 Opcode.Mov S.s_mov_xm a
let movb_im a = mk ~width:8 Opcode.Mov S.s_movb_im a
let movb_im_e a = mk_e ~width:8 Opcode.Mov S.s_movb_im a
let movw_im a = mk ~width:16 Opcode.Mov S.s_movw_im a
let movw_im_e a = mk_e ~width:16 Opcode.Mov S.s_movw_im a
let movl_im a = mk ~width:32 Opcode.Mov S.s_movl_im a
let movl_im_e a = mk_e ~width:32 Opcode.Mov S.s_movl_im a
let movq_im a = mk ~width:64 Opcode.Mov S.s_movq_im a
let movq_im_e a = mk_e ~width:64 Opcode.Mov S.s_movq_im a
let lea_m a = mk Opcode.Lea S.s_lea_m a
let lea_s a = mk Opcode.Lea S.s_lea_s a
let ret_m32 = mk ~width:32 Opcode.Ret S.s_ret_m32
let ret_m64 = mk ~width:64 Opcode.Ret S.s_ret_m64
let xor a = mk Opcode.Xor S.s_xor a
let xor_rrr a = mk Opcode.Xor S.s_xor_rrr a
let xor_rm a = mk Opcode.Xor S.s_xor_rm a
let xor_i a = mk Opcode.Xor S.s_xor_i a
let xor_i_e a = mk_e Opcode.Xor S.s_xor_i a
let xor_m a = mk Opcode.Xor S.s_xor_m a
let xor_rmr a = mk Opcode.Xor S.s_xor_rmr a
let xor_irr a = mk Opcode.Xor S.s_xor_irr a
let xor_irr_e a = mk_e Opcode.Xor S.s_xor_irr a
let xor_mrr a = mk Opcode.Xor S.s_xor_mrr a
let xorb_im a = mk ~width:8 Opcode.Xor S.s_xorb_im a
let xorb_im_e a = mk_e ~width:8 Opcode.Xor S.s_xorb_im a
let xorl_im a = mk ~width:32 Opcode.Xor S.s_xorl_im a
let xorl_im_e a = mk_e ~width:32 Opcode.Xor S.s_xorl_im a
let xorq_im a = mk ~width:64 Opcode.Xor S.s_xorq_im a
let xorq_im_e a = mk_e ~width:64 Opcode.Xor S.s_xorq_im a
let xor_im a = mk Opcode.Xor S.s_xor_im a
let xor_im_e a = mk_e Opcode.Xor S.s_xor_im a
let and_ a = mk Opcode.And S.s_and_ a
let and_rrr a = mk Opcode.And S.s_and_rrr a
let and_rm a = mk Opcode.And S.s_and_rm a
let and_i a = mk Opcode.And S.s_and_i a
let and_i_e a = mk_e Opcode.And S.s_and_i a
let and_m a = mk Opcode.And S.s_and_m a
let and_rmr a = mk Opcode.And S.s_and_rmr a
let and_irr a = mk Opcode.And S.s_and_irr a
let and_irr_e a = mk_e Opcode.And S.s_and_irr a
let and_mrr a = mk Opcode.And S.s_and_mrr a
let andb_im a = mk ~width:8 Opcode.And S.s_andb_im a
let andb_im_e a = mk_e ~width:8 Opcode.And S.s_andb_im a
let andl_im a = mk ~width:32 Opcode.And S.s_andl_im a
let andl_im_e a = mk_e ~width:32 Opcode.And S.s_andl_im a
let andq_im a = mk ~width:64 Opcode.And S.s_andq_im a
let andq_im_e a = mk_e ~width:64 Opcode.And S.s_andq_im a
let and_im a = mk Opcode.And S.s_and_im a
let and_im_e a = mk_e Opcode.And S.s_and_im a
let cmp a = mk Opcode.Cmp S.s_cmp a
let cmp_rm a = mk Opcode.Cmp S.s_cmp_rm a
let cmp_i a = mk Opcode.Cmp S.s_cmp_i a
let cmp_i_e a = mk_e Opcode.Cmp S.s_cmp_i a
let cmp_m a = mk Opcode.Cmp S.s_cmp_m a
let cmpb_im a = mk ~width:8 Opcode.Cmp S.s_cmpb_im a
let cmpb_im_e a = mk_e ~width:8 Opcode.Cmp S.s_cmpb_im a
let cmpl_im a = mk ~width:32 Opcode.Cmp S.s_cmpl_im a
let cmpl_im_e a = mk_e ~width:32 Opcode.Cmp S.s_cmpl_im a
let cmpq_im a = mk ~width:64 Opcode.Cmp S.s_cmpq_im a
let cmpq_im_e a = mk_e ~width:64 Opcode.Cmp S.s_cmpq_im a
let imul a = mk Opcode.Imul S.s_imul a
let imul_rr a = mk Opcode.Imul S.s_imul_rr a
let imul_rrr a = mk Opcode.Imul S.s_imul_rrr a
let imull_m a = mk ~width:32 Opcode.Imul S.s_imull_m a
let imulq_m a = mk ~width:64 Opcode.Imul S.s_imulq_m a
let imul_m a = mk Opcode.Imul S.s_imul_m a
let imul_s a = mk Opcode.Imul S.s_imul_s a
let imul_i a = mk Opcode.Imul S.s_imul_i a
let imul_i_e a = mk_e Opcode.Imul S.s_imul_i a
let imul_mrr a = mk Opcode.Imul S.s_imul_mrr a
let imul_im a = mk Opcode.Imul S.s_imul_im a
let imul_im_e a = mk_e Opcode.Imul S.s_imul_im a
let ud2_m32 = mk ~width:32 Opcode.Ud2 S.s_ud2_m32
let ud2_m64 = mk ~width:64 Opcode.Ud2 S.s_ud2_m64
let pop_m32 a = mk ~width:32 Opcode.Pop S.s_pop_m32 a
let pop_m64 a = mk ~width:64 Opcode.Pop S.s_pop_m64 a
let pop_m_m32 a = mk ~width:32 Opcode.Pop S.s_pop_m_m32 a
let pop_m_m64 a = mk ~width:64 Opcode.Pop S.s_pop_m_m64 a
let bswap a = mk Opcode.Bswap S.s_bswap a
let jmp_m32 a = mk ~width:32 Opcode.Jmp S.s_jmp_m32 a
let jmp_m64 a = mk ~width:64 Opcode.Jmp S.s_jmp_m64 a
let jmp_r_m32 a = mk ~width:32 Opcode.Jmp S.s_jmp_r_m32 a
let jmp_r_m64 a = mk ~width:64 Opcode.Jmp S.s_jmp_r_m64 a
let jmp_m_m32 a = mk ~width:32 Opcode.Jmp S.s_jmp_m_m32 a
let jmp_m_m64 a = mk ~width:64 Opcode.Jmp S.s_jmp_m_m64 a
let call_m32 a = mk ~width:32 Opcode.Call S.s_call_m32 a
let call_m64 a = mk ~width:64 Opcode.Call S.s_call_m64 a
let call_r_m32 a = mk ~width:32 Opcode.Call S.s_call_r_m32 a
let call_r_m64 a = mk ~width:64 Opcode.Call S.s_call_r_m64 a
let call_m_m32 a = mk ~width:32 Opcode.Call S.s_call_m_m32 a
let call_m_m64 a = mk ~width:64 Opcode.Call S.s_call_m_m64 a
let push_m32 a = mk ~width:32 Opcode.Push S.s_push_m32 a
let push_m64 a = mk ~width:64 Opcode.Push S.s_push_m64 a
let push_i_m32 a = mk ~width:32 Opcode.Push S.s_push_i_m32 a
let push_i_m32_e a = mk_e ~width:32 Opcode.Push S.s_push_i_m32 a
let push_i_m64 a = mk ~width:64 Opcode.Push S.s_push_i_m64 a
let push_i_m64_e a = mk_e ~width:64 Opcode.Push S.s_push_i_m64 a
let neg a = mk Opcode.Neg S.s_neg a
let neg_rr a = mk Opcode.Neg S.s_neg_rr a
let negl_m a = mk ~width:32 Opcode.Neg S.s_negl_m a
let negq_m a = mk ~width:64 Opcode.Neg S.s_negq_m a
let neg_m a = mk Opcode.Neg S.s_neg_m a
let test a = mk Opcode.Test S.s_test a
let test_rm a = mk Opcode.Test S.s_test_rm a
let test_i a = mk Opcode.Test S.s_test_i a
let test_i_e a = mk_e Opcode.Test S.s_test_i a
let testl_im a = mk ~width:32 Opcode.Test S.s_testl_im a
let testl_im_e a = mk_e ~width:32 Opcode.Test S.s_testl_im a
let testq_im a = mk ~width:64 Opcode.Test S.s_testq_im a
let testq_im_e a = mk_e ~width:64 Opcode.Test S.s_testq_im a
let adc a = mk Opcode.Adc S.s_adc a
let adc_rrr a = mk Opcode.Adc S.s_adc_rrr a
let adc_rm a = mk Opcode.Adc S.s_adc_rm a
let adc_i a = mk Opcode.Adc S.s_adc_i a
let adc_i_e a = mk_e Opcode.Adc S.s_adc_i a
let adc_m a = mk Opcode.Adc S.s_adc_m a
let adc_rmr a = mk Opcode.Adc S.s_adc_rmr a
let adc_irr a = mk Opcode.Adc S.s_adc_irr a
let adc_irr_e a = mk_e Opcode.Adc S.s_adc_irr a
let adc_mrr a = mk Opcode.Adc S.s_adc_mrr a
let adcb_im a = mk ~width:8 Opcode.Adc S.s_adcb_im a
let adcb_im_e a = mk_e ~width:8 Opcode.Adc S.s_adcb_im a
let adcl_im a = mk ~width:32 Opcode.Adc S.s_adcl_im a
let adcl_im_e a = mk_e ~width:32 Opcode.Adc S.s_adcl_im a
let adcq_im a = mk ~width:64 Opcode.Adc S.s_adcq_im a
let adcq_im_e a = mk_e ~width:64 Opcode.Adc S.s_adcq_im a
let adc_im a = mk Opcode.Adc S.s_adc_im a
let adc_im_e a = mk_e Opcode.Adc S.s_adc_im a
let sbb a = mk Opcode.Sbb S.s_sbb a
let sbb_rrr a = mk Opcode.Sbb S.s_sbb_rrr a
let sbb_rm a = mk Opcode.Sbb S.s_sbb_rm a
let sbb_i a = mk Opcode.Sbb S.s_sbb_i a
let sbb_i_e a = mk_e Opcode.Sbb S.s_sbb_i a
let sbb_m a = mk Opcode.Sbb S.s_sbb_m a
let sbb_rmr a = mk Opcode.Sbb S.s_sbb_rmr a
let sbb_irr a = mk Opcode.Sbb S.s_sbb_irr a
let sbb_irr_e a = mk_e Opcode.Sbb S.s_sbb_irr a
let sbb_mrr a = mk Opcode.Sbb S.s_sbb_mrr a
let sbbb_im a = mk ~width:8 Opcode.Sbb S.s_sbbb_im a
let sbbb_im_e a = mk_e ~width:8 Opcode.Sbb S.s_sbbb_im a
let sbbl_im a = mk ~width:32 Opcode.Sbb S.s_sbbl_im a
let sbbl_im_e a = mk_e ~width:32 Opcode.Sbb S.s_sbbl_im a
let sbbq_im a = mk ~width:64 Opcode.Sbb S.s_sbbq_im a
let sbbq_im_e a = mk_e ~width:64 Opcode.Sbb S.s_sbbq_im a
let sbb_im a = mk Opcode.Sbb S.s_sbb_im a
let sbb_im_e a = mk_e Opcode.Sbb S.s_sbb_im a
let mul a = mk Opcode.Mul S.s_mul a
let mull_m a = mk ~width:32 Opcode.Mul S.s_mull_m a
let mulq_m a = mk ~width:64 Opcode.Mul S.s_mulq_m a
let div a = mk Opcode.Div S.s_div a
let divl_m a = mk ~width:32 Opcode.Div S.s_divl_m a
let divq_m a = mk ~width:64 Opcode.Div S.s_divq_m a
let dec a = mk ~width:32 Opcode.Dec S.s_dec a
let dec_m a = mk ~width:32 Opcode.Dec S.s_dec_m a
let rcr a = mk Opcode.Rcr S.s_rcr a
let rcrl_m a = mk ~width:32 Opcode.Rcr S.s_rcrl_m a
let rcrq_m a = mk ~width:64 Opcode.Rcr S.s_rcrq_m a
let rcr_cl a = mk Opcode.Rcr S.s_rcr_cl a
let rcr_i a = mk Opcode.Rcr S.s_rcr_i a
let rcr_i_e a = mk_e Opcode.Rcr S.s_rcr_i a
let rcr_clrr a = mk Opcode.Rcr S.s_rcr_clrr a
let rcr_irr a = mk Opcode.Rcr S.s_rcr_irr a
let rcr_irr_e a = mk_e Opcode.Rcr S.s_rcr_irr a
let rcrl_clm a = mk ~width:32 Opcode.Rcr S.s_rcrl_clm a
let rcrq_clm a = mk ~width:64 Opcode.Rcr S.s_rcrq_clm a
let rcrl_im a = mk ~width:32 Opcode.Rcr S.s_rcrl_im a
let rcrl_im_e a = mk_e ~width:32 Opcode.Rcr S.s_rcrl_im a
let rcrq_im a = mk ~width:64 Opcode.Rcr S.s_rcrq_im a
let rcrq_im_e a = mk_e ~width:64 Opcode.Rcr S.s_rcrq_im a
let rcr_clm a = mk Opcode.Rcr S.s_rcr_clm a
let rcr_im a = mk Opcode.Rcr S.s_rcr_im a
let rcr_im_e a = mk_e Opcode.Rcr S.s_rcr_im a
let shr a = mk Opcode.Shr S.s_shr a
let shrl_m a = mk ~width:32 Opcode.Shr S.s_shrl_m a
let shrq_m a = mk ~width:64 Opcode.Shr S.s_shrq_m a
let shr_cl a = mk Opcode.Shr S.s_shr_cl a
let shr_i a = mk Opcode.Shr S.s_shr_i a
let shr_i_e a = mk_e Opcode.Shr S.s_shr_i a
let shr_clrr a = mk Opcode.Shr S.s_shr_clrr a
let shr_irr a = mk Opcode.Shr S.s_shr_irr a
let shr_irr_e a = mk_e Opcode.Shr S.s_shr_irr a
let shrl_clm a = mk ~width:32 Opcode.Shr S.s_shrl_clm a
let shrq_clm a = mk ~width:64 Opcode.Shr S.s_shrq_clm a
let shrl_im a = mk ~width:32 Opcode.Shr S.s_shrl_im a
let shrl_im_e a = mk_e ~width:32 Opcode.Shr S.s_shrl_im a
let shrq_im a = mk ~width:64 Opcode.Shr S.s_shrq_im a
let shrq_im_e a = mk_e ~width:64 Opcode.Shr S.s_shrq_im a
let shr_clm a = mk Opcode.Shr S.s_shr_clm a
let shr_im a = mk Opcode.Shr S.s_shr_im a
let shr_im_e a = mk_e Opcode.Shr S.s_shr_im a
let or_ a = mk Opcode.Or S.s_or_ a
let or_rrr a = mk Opcode.Or S.s_or_rrr a
let or_rm a = mk Opcode.Or S.s_or_rm a
let or_i a = mk Opcode.Or S.s_or_i a
let or_i_e a = mk_e Opcode.Or S.s_or_i a
let or_m a = mk Opcode.Or S.s_or_m a
let or_rmr a = mk Opcode.Or S.s_or_rmr a
let or_irr a = mk Opcode.Or S.s_or_irr a
let or_irr_e a = mk_e Opcode.Or S.s_or_irr a
let or_mrr a = mk Opcode.Or S.s_or_mrr a
let orb_im a = mk ~width:8 Opcode.Or S.s_orb_im a
let orb_im_e a = mk_e ~width:8 Opcode.Or S.s_orb_im a
let orl_im a = mk ~width:32 Opcode.Or S.s_orl_im a
let orl_im_e a = mk_e ~width:32 Opcode.Or S.s_orl_im a
let orq_im a = mk ~width:64 Opcode.Or S.s_orq_im a
let orq_im_e a = mk_e ~width:64 Opcode.Or S.s_orq_im a
let or_im a = mk Opcode.Or S.s_or_im a
let or_im_e a = mk_e Opcode.Or S.s_or_im a
let not a = mk Opcode.Not S.s_not a
let not_rr a = mk Opcode.Not S.s_not_rr a
let notl_m a = mk ~width:32 Opcode.Not S.s_notl_m a
let notq_m a = mk ~width:64 Opcode.Not S.s_notq_m a
let not_m a = mk Opcode.Not S.s_not_m a
let ror a = mk Opcode.Ror S.s_ror a
let rorl_m a = mk ~width:32 Opcode.Ror S.s_rorl_m a
let rorq_m a = mk ~width:64 Opcode.Ror S.s_rorq_m a
let ror_cl a = mk Opcode.Ror S.s_ror_cl a
let ror_i a = mk Opcode.Ror S.s_ror_i a
let ror_i_e a = mk_e Opcode.Ror S.s_ror_i a
let ror_clrr a = mk Opcode.Ror S.s_ror_clrr a
let ror_irr a = mk Opcode.Ror S.s_ror_irr a
let ror_irr_e a = mk_e Opcode.Ror S.s_ror_irr a
let rorl_clm a = mk ~width:32 Opcode.Ror S.s_rorl_clm a
let rorq_clm a = mk ~width:64 Opcode.Ror S.s_rorq_clm a
let rorl_im a = mk ~width:32 Opcode.Ror S.s_rorl_im a
let rorl_im_e a = mk_e ~width:32 Opcode.Ror S.s_rorl_im a
let rorq_im a = mk ~width:64 Opcode.Ror S.s_rorq_im a
let rorq_im_e a = mk_e ~width:64 Opcode.Ror S.s_rorq_im a
let ror_clm a = mk Opcode.Ror S.s_ror_clm a
let ror_im a = mk Opcode.Ror S.s_ror_im a
let ror_im_e a = mk_e Opcode.Ror S.s_ror_im a
let shl a = mk Opcode.Shl S.s_shl a
let shll_m a = mk ~width:32 Opcode.Shl S.s_shll_m a
let shlq_m a = mk ~width:64 Opcode.Shl S.s_shlq_m a
let shl_cl a = mk Opcode.Shl S.s_shl_cl a
let shl_i a = mk Opcode.Shl S.s_shl_i a
let shl_i_e a = mk_e Opcode.Shl S.s_shl_i a
let shl_clrr a = mk Opcode.Shl S.s_shl_clrr a
let shl_irr a = mk Opcode.Shl S.s_shl_irr a
let shl_irr_e a = mk_e Opcode.Shl S.s_shl_irr a
let shll_clm a = mk ~width:32 Opcode.Shl S.s_shll_clm a
let shlq_clm a = mk ~width:64 Opcode.Shl S.s_shlq_clm a
let shll_im a = mk ~width:32 Opcode.Shl S.s_shll_im a
let shll_im_e a = mk_e ~width:32 Opcode.Shl S.s_shll_im a
let shlq_im a = mk ~width:64 Opcode.Shl S.s_shlq_im a
let shlq_im_e a = mk_e ~width:64 Opcode.Shl S.s_shlq_im a
let shl_clm a = mk Opcode.Shl S.s_shl_clm a
let shl_im a = mk Opcode.Shl S.s_shl_im a
let shl_im_e a = mk_e Opcode.Shl S.s_shl_im a
let sar a = mk Opcode.Sar S.s_sar a
let sarl_m a = mk ~width:32 Opcode.Sar S.s_sarl_m a
let sarq_m a = mk ~width:64 Opcode.Sar S.s_sarq_m a
let sar_cl a = mk Opcode.Sar S.s_sar_cl a
let sar_i a = mk Opcode.Sar S.s_sar_i a
let sar_i_e a = mk_e Opcode.Sar S.s_sar_i a
let sar_clrr a = mk Opcode.Sar S.s_sar_clrr a
let sar_irr a = mk Opcode.Sar S.s_sar_irr a
let sar_irr_e a = mk_e Opcode.Sar S.s_sar_irr a
let sarl_clm a = mk ~width:32 Opcode.Sar S.s_sarl_clm a
let sarq_clm a = mk ~width:64 Opcode.Sar S.s_sarq_clm a
let sarl_im a = mk ~width:32 Opcode.Sar S.s_sarl_im a
let sarl_im_e a = mk_e ~width:32 Opcode.Sar S.s_sarl_im a
let sarq_im a = mk ~width:64 Opcode.Sar S.s_sarq_im a
let sarq_im_e a = mk_e ~width:64 Opcode.Sar S.s_sarq_im a
let sar_clm a = mk Opcode.Sar S.s_sar_clm a
let sar_im a = mk Opcode.Sar S.s_sar_im a
let sar_im_e a = mk_e Opcode.Sar S.s_sar_im a
let shld_cl a = mk Opcode.Shld S.s_shld_cl a
let shld_i a = mk Opcode.Shld S.s_shld_i a
let shld_i_e a = mk_e Opcode.Shld S.s_shld_i a
let shld_irrr a = mk Opcode.Shld S.s_shld_irrr a
let shld_irrr_e a = mk_e Opcode.Shld S.s_shld_irrr a
let shld_clrm a = mk Opcode.Shld S.s_shld_clrm a
let shld_irm a = mk Opcode.Shld S.s_shld_irm a
let shld_irm_e a = mk_e Opcode.Shld S.s_shld_irm a
let shld_irmr a = mk Opcode.Shld S.s_shld_irmr a
let shld_irmr_e a = mk_e Opcode.Shld S.s_shld_irmr a
let addsd a = mk ~width:32 Opcode.Addsd S.s_addsd a
let addsd_m a = mk ~width:32 Opcode.Addsd S.s_addsd_m a
let subsd a = mk ~width:32 Opcode.Subsd S.s_subsd a
let subsd_m a = mk ~width:32 Opcode.Subsd S.s_subsd_m a
let mulsd a = mk ~width:32 Opcode.Mulsd S.s_mulsd a
let mulsd_m a = mk ~width:32 Opcode.Mulsd S.s_mulsd_m a
let divsd a = mk ~width:32 Opcode.Divsd S.s_divsd a
let divsd_m a = mk ~width:32 Opcode.Divsd S.s_divsd_m a
let addss a = mk ~width:32 Opcode.Addss S.s_addss a
let addss_m a = mk ~width:32 Opcode.Addss S.s_addss_m a
let subss a = mk ~width:32 Opcode.Subss S.s_subss a
let subss_m a = mk ~width:32 Opcode.Subss S.s_subss_m a
let mulss a = mk ~width:32 Opcode.Mulss S.s_mulss a
let mulss_m a = mk ~width:32 Opcode.Mulss S.s_mulss_m a
let divss a = mk ~width:32 Opcode.Divss S.s_divss a
let divss_m a = mk ~width:32 Opcode.Divss S.s_divss_m a
let comisd a = mk ~width:32 Opcode.Comisd S.s_comisd a
let comisd_m a = mk ~width:32 Opcode.Comisd S.s_comisd_m a
let ucomisd a = mk ~width:32 Opcode.Ucomisd S.s_ucomisd a
let ucomisd_m a = mk ~width:32 Opcode.Ucomisd S.s_ucomisd_m a
let comiss a = mk ~width:32 Opcode.Comiss S.s_comiss a
let comiss_m a = mk ~width:32 Opcode.Comiss S.s_comiss_m a
let ucomiss a = mk ~width:32 Opcode.Ucomiss S.s_ucomiss a
let ucomiss_m a = mk ~width:32 Opcode.Ucomiss S.s_ucomiss_m a
let xorpd a = mk ~width:32 Opcode.Xorpd S.s_xorpd a
let xorpd_m a = mk ~width:32 Opcode.Xorpd S.s_xorpd_m a
let pxor_qq a = mk ~width:32 Opcode.Pxor S.s_pxor_qq a
let pxor a = mk ~width:32 Opcode.Pxor S.s_pxor a
let pxor_mq a = mk ~width:32 Opcode.Pxor S.s_pxor_mq a
let pxor_m a = mk ~width:32 Opcode.Pxor S.s_pxor_m a
let movapd a = mk ~width:32 Opcode.Movapd S.s_movapd a
let movapd_m a = mk ~width:32 Opcode.Movapd S.s_movapd_m a
let movapd_rm a = mk ~width:32 Opcode.Movapd S.s_movapd_rm a
let cvtsd2ss a = mk ~width:32 Opcode.Cvtsd2ss S.s_cvtsd2ss a
let cvtsd2ss_m a = mk ~width:32 Opcode.Cvtsd2ss S.s_cvtsd2ss_m a
let cvtss2sd a = mk ~width:32 Opcode.Cvtss2sd S.s_cvtss2sd a
let cvtss2sd_m a = mk ~width:32 Opcode.Cvtss2sd S.s_cvtss2sd_m a
let cvtps2pd a = mk ~width:32 Opcode.Cvtps2pd S.s_cvtps2pd a
let cvtps2pd_m a = mk ~width:32 Opcode.Cvtps2pd S.s_cvtps2pd_m a
let cvtpd2ps a = mk ~width:32 Opcode.Cvtpd2ps S.s_cvtpd2ps a
let cvtpd2ps_m a = mk ~width:32 Opcode.Cvtpd2ps S.s_cvtpd2ps_m a
let cvtdq2ps a = mk ~width:32 Opcode.Cvtdq2ps S.s_cvtdq2ps a
let cvtdq2ps_m a = mk ~width:32 Opcode.Cvtdq2ps S.s_cvtdq2ps_m a
let cvtps2dq a = mk ~width:32 Opcode.Cvtps2dq S.s_cvtps2dq a
let cvtps2dq_m a = mk ~width:32 Opcode.Cvtps2dq S.s_cvtps2dq_m a
let cvttps2dq a = mk ~width:32 Opcode.Cvttps2dq S.s_cvttps2dq a
let cvttps2dq_m a = mk ~width:32 Opcode.Cvttps2dq S.s_cvttps2dq_m a
let movsd a = mk ~width:32 Opcode.Movsd S.s_movsd a
let movsd_m a = mk ~width:32 Opcode.Movsd S.s_movsd_m a
let movsd_rm a = mk ~width:32 Opcode.Movsd S.s_movsd_rm a
let movss a = mk ~width:32 Opcode.Movss S.s_movss a
let movss_m a = mk ~width:32 Opcode.Movss S.s_movss_m a
let movss_rm a = mk ~width:32 Opcode.Movss S.s_movss_rm a
let cvtsi2sd a = mk ~width:32 Opcode.Cvtsi2sd S.s_cvtsi2sd a
let cvtsi2sd_m a = mk ~width:32 Opcode.Cvtsi2sd S.s_cvtsi2sd_m a
let cvtsi2ss a = mk ~width:32 Opcode.Cvtsi2ss S.s_cvtsi2ss a
let cvtsi2ss_m a = mk ~width:32 Opcode.Cvtsi2ss S.s_cvtsi2ss_m a
let cvttsd2si_x a = mk Opcode.Cvttsd2si S.s_cvttsd2si_x a
let cvttsd2si_m a = mk Opcode.Cvttsd2si S.s_cvttsd2si_m a
let cvttsd2si_mr a = mk ~width:64 Opcode.Cvttsd2si S.s_cvttsd2si_mr a
let unpcklps a = mk ~width:32 Opcode.Unpcklps S.s_unpcklps a
let unpcklps_m a = mk ~width:32 Opcode.Unpcklps S.s_unpcklps_m a
let unpckhps a = mk ~width:32 Opcode.Unpckhps S.s_unpckhps a
let unpckhps_m a = mk ~width:32 Opcode.Unpckhps S.s_unpckhps_m a
let unpcklpd a = mk ~width:32 Opcode.Unpcklpd S.s_unpcklpd a
let unpcklpd_m a = mk ~width:32 Opcode.Unpcklpd S.s_unpcklpd_m a
let unpckhpd a = mk ~width:32 Opcode.Unpckhpd S.s_unpckhpd a
let unpckhpd_m a = mk ~width:32 Opcode.Unpckhpd S.s_unpckhpd_m a
let punpcklqdq a = mk ~width:32 Opcode.Punpcklqdq S.s_punpcklqdq a
let punpcklqdq_m a = mk ~width:32 Opcode.Punpcklqdq S.s_punpcklqdq_m a
let punpckhqdq a = mk ~width:32 Opcode.Punpckhqdq S.s_punpckhqdq a
let punpckhqdq_m a = mk ~width:32 Opcode.Punpckhqdq S.s_punpckhqdq_m a
let punpcklbw_qq a = mk ~width:32 Opcode.Punpcklbw S.s_punpcklbw_qq a
let punpcklbw a = mk ~width:32 Opcode.Punpcklbw S.s_punpcklbw a
let punpcklbw_mq a = mk ~width:32 Opcode.Punpcklbw S.s_punpcklbw_mq a
let punpcklbw_m a = mk ~width:32 Opcode.Punpcklbw S.s_punpcklbw_m a
let punpckhbw_qq a = mk ~width:32 Opcode.Punpckhbw S.s_punpckhbw_qq a
let punpckhbw a = mk ~width:32 Opcode.Punpckhbw S.s_punpckhbw a
let punpckhbw_mq a = mk ~width:32 Opcode.Punpckhbw S.s_punpckhbw_mq a
let punpckhbw_m a = mk ~width:32 Opcode.Punpckhbw S.s_punpckhbw_m a
let punpcklwd_qq a = mk ~width:32 Opcode.Punpcklwd S.s_punpcklwd_qq a
let punpcklwd a = mk ~width:32 Opcode.Punpcklwd S.s_punpcklwd a
let punpcklwd_mq a = mk ~width:32 Opcode.Punpcklwd S.s_punpcklwd_mq a
let punpcklwd_m a = mk ~width:32 Opcode.Punpcklwd S.s_punpcklwd_m a
let punpckhwd_qq a = mk ~width:32 Opcode.Punpckhwd S.s_punpckhwd_qq a
let punpckhwd a = mk ~width:32 Opcode.Punpckhwd S.s_punpckhwd a
let punpckhwd_mq a = mk ~width:32 Opcode.Punpckhwd S.s_punpckhwd_mq a
let punpckhwd_m a = mk ~width:32 Opcode.Punpckhwd S.s_punpckhwd_m a
let punpckldq_qq a = mk ~width:32 Opcode.Punpckldq S.s_punpckldq_qq a
let punpckldq a = mk ~width:32 Opcode.Punpckldq S.s_punpckldq a
let punpckldq_mq a = mk ~width:32 Opcode.Punpckldq S.s_punpckldq_mq a
let punpckldq_m a = mk ~width:32 Opcode.Punpckldq S.s_punpckldq_m a
let punpckhdq_qq a = mk ~width:32 Opcode.Punpckhdq S.s_punpckhdq_qq a
let punpckhdq a = mk ~width:32 Opcode.Punpckhdq S.s_punpckhdq a
let punpckhdq_mq a = mk ~width:32 Opcode.Punpckhdq S.s_punpckhdq_mq a
let punpckhdq_m a = mk ~width:32 Opcode.Punpckhdq S.s_punpckhdq_m a
let paddb_qq a = mk ~width:32 Opcode.Paddb S.s_paddb_qq a
let paddb a = mk ~width:32 Opcode.Paddb S.s_paddb a
let paddb_mq a = mk ~width:32 Opcode.Paddb S.s_paddb_mq a
let paddb_m a = mk ~width:32 Opcode.Paddb S.s_paddb_m a
let paddw_qq a = mk ~width:32 Opcode.Paddw S.s_paddw_qq a
let paddw a = mk ~width:32 Opcode.Paddw S.s_paddw a
let paddw_mq a = mk ~width:32 Opcode.Paddw S.s_paddw_mq a
let paddw_m a = mk ~width:32 Opcode.Paddw S.s_paddw_m a
let paddd_qq a = mk ~width:32 Opcode.Paddd S.s_paddd_qq a
let paddd a = mk ~width:32 Opcode.Paddd S.s_paddd a
let paddd_mq a = mk ~width:32 Opcode.Paddd S.s_paddd_mq a
let paddd_m a = mk ~width:32 Opcode.Paddd S.s_paddd_m a
let paddq_qq a = mk ~width:32 Opcode.Paddq S.s_paddq_qq a
let paddq a = mk ~width:32 Opcode.Paddq S.s_paddq a
let paddq_mq a = mk ~width:32 Opcode.Paddq S.s_paddq_mq a
let paddq_m a = mk ~width:32 Opcode.Paddq S.s_paddq_m a
let psubb_qq a = mk ~width:32 Opcode.Psubb S.s_psubb_qq a
let psubb a = mk ~width:32 Opcode.Psubb S.s_psubb a
let psubb_mq a = mk ~width:32 Opcode.Psubb S.s_psubb_mq a
let psubb_m a = mk ~width:32 Opcode.Psubb S.s_psubb_m a
let psubw_qq a = mk ~width:32 Opcode.Psubw S.s_psubw_qq a
let psubw a = mk ~width:32 Opcode.Psubw S.s_psubw a
let psubw_mq a = mk ~width:32 Opcode.Psubw S.s_psubw_mq a
let psubw_m a = mk ~width:32 Opcode.Psubw S.s_psubw_m a
let psubd_qq a = mk ~width:32 Opcode.Psubd S.s_psubd_qq a
let psubd a = mk ~width:32 Opcode.Psubd S.s_psubd a
let psubd_mq a = mk ~width:32 Opcode.Psubd S.s_psubd_mq a
let psubd_m a = mk ~width:32 Opcode.Psubd S.s_psubd_m a
let psubq_qq a = mk ~width:32 Opcode.Psubq S.s_psubq_qq a
let psubq a = mk ~width:32 Opcode.Psubq S.s_psubq a
let psubq_mq a = mk ~width:32 Opcode.Psubq S.s_psubq_mq a
let psubq_m a = mk ~width:32 Opcode.Psubq S.s_psubq_m a
let pcmpeqb_qq a = mk ~width:32 Opcode.Pcmpeqb S.s_pcmpeqb_qq a
let pcmpeqb a = mk ~width:32 Opcode.Pcmpeqb S.s_pcmpeqb a
let pcmpeqb_mq a = mk ~width:32 Opcode.Pcmpeqb S.s_pcmpeqb_mq a
let pcmpeqb_m a = mk ~width:32 Opcode.Pcmpeqb S.s_pcmpeqb_m a
let pcmpeqw_qq a = mk ~width:32 Opcode.Pcmpeqw S.s_pcmpeqw_qq a
let pcmpeqw a = mk ~width:32 Opcode.Pcmpeqw S.s_pcmpeqw a
let pcmpeqw_mq a = mk ~width:32 Opcode.Pcmpeqw S.s_pcmpeqw_mq a
let pcmpeqw_m a = mk ~width:32 Opcode.Pcmpeqw S.s_pcmpeqw_m a
let pcmpeqd_qq a = mk ~width:32 Opcode.Pcmpeqd S.s_pcmpeqd_qq a
let pcmpeqd a = mk ~width:32 Opcode.Pcmpeqd S.s_pcmpeqd a
let pcmpeqd_mq a = mk ~width:32 Opcode.Pcmpeqd S.s_pcmpeqd_mq a
let pcmpeqd_m a = mk ~width:32 Opcode.Pcmpeqd S.s_pcmpeqd_m a
let pcmpgtb_qq a = mk ~width:32 Opcode.Pcmpgtb S.s_pcmpgtb_qq a
let pcmpgtb a = mk ~width:32 Opcode.Pcmpgtb S.s_pcmpgtb a
let pcmpgtb_mq a = mk ~width:32 Opcode.Pcmpgtb S.s_pcmpgtb_mq a
let pcmpgtb_m a = mk ~width:32 Opcode.Pcmpgtb S.s_pcmpgtb_m a
let pcmpgtw_qq a = mk ~width:32 Opcode.Pcmpgtw S.s_pcmpgtw_qq a
let pcmpgtw a = mk ~width:32 Opcode.Pcmpgtw S.s_pcmpgtw a
let pcmpgtw_mq a = mk ~width:32 Opcode.Pcmpgtw S.s_pcmpgtw_mq a
let pcmpgtw_m a = mk ~width:32 Opcode.Pcmpgtw S.s_pcmpgtw_m a
let pcmpgtd_qq a = mk ~width:32 Opcode.Pcmpgtd S.s_pcmpgtd_qq a
let pcmpgtd a = mk ~width:32 Opcode.Pcmpgtd S.s_pcmpgtd a
let pcmpgtd_mq a = mk ~width:32 Opcode.Pcmpgtd S.s_pcmpgtd_mq a
let pcmpgtd_m a = mk ~width:32 Opcode.Pcmpgtd S.s_pcmpgtd_m a
let packsswb_qq a = mk ~width:32 Opcode.Packsswb S.s_packsswb_qq a
let packsswb a = mk ~width:32 Opcode.Packsswb S.s_packsswb a
let packsswb_mq a = mk ~width:32 Opcode.Packsswb S.s_packsswb_mq a
let packsswb_m a = mk ~width:32 Opcode.Packsswb S.s_packsswb_m a
let packssdw_qq a = mk ~width:32 Opcode.Packssdw S.s_packssdw_qq a
let packssdw a = mk ~width:32 Opcode.Packssdw S.s_packssdw a
let packssdw_mq a = mk ~width:32 Opcode.Packssdw S.s_packssdw_mq a
let packssdw_m a = mk ~width:32 Opcode.Packssdw S.s_packssdw_m a
let packuswb_qq a = mk ~width:32 Opcode.Packuswb S.s_packuswb_qq a
let packuswb a = mk ~width:32 Opcode.Packuswb S.s_packuswb a
let packuswb_mq a = mk ~width:32 Opcode.Packuswb S.s_packuswb_mq a
let packuswb_m a = mk ~width:32 Opcode.Packuswb S.s_packuswb_m a
let pand_qq a = mk ~width:32 Opcode.Pand S.s_pand_qq a
let pand a = mk ~width:32 Opcode.Pand S.s_pand a
let pand_mq a = mk ~width:32 Opcode.Pand S.s_pand_mq a
let pand_m a = mk ~width:32 Opcode.Pand S.s_pand_m a
let pandn_qq a = mk ~width:32 Opcode.Pandn S.s_pandn_qq a
let pandn a = mk ~width:32 Opcode.Pandn S.s_pandn a
let pandn_mq a = mk ~width:32 Opcode.Pandn S.s_pandn_mq a
let pandn_m a = mk ~width:32 Opcode.Pandn S.s_pandn_m a
let por_qq a = mk ~width:32 Opcode.Por S.s_por_qq a
let por a = mk ~width:32 Opcode.Por S.s_por a
let por_mq a = mk ~width:32 Opcode.Por S.s_por_mq a
let por_m a = mk ~width:32 Opcode.Por S.s_por_m a
let pminub_qq a = mk ~width:32 Opcode.Pminub S.s_pminub_qq a
let pminub a = mk ~width:32 Opcode.Pminub S.s_pminub a
let pminub_mq a = mk ~width:32 Opcode.Pminub S.s_pminub_mq a
let pminub_m a = mk ~width:32 Opcode.Pminub S.s_pminub_m a
let pmaxub_qq a = mk ~width:32 Opcode.Pmaxub S.s_pmaxub_qq a
let pmaxub a = mk ~width:32 Opcode.Pmaxub S.s_pmaxub a
let pmaxub_mq a = mk ~width:32 Opcode.Pmaxub S.s_pmaxub_mq a
let pmaxub_m a = mk ~width:32 Opcode.Pmaxub S.s_pmaxub_m a
let pminsw_qq a = mk ~width:32 Opcode.Pminsw S.s_pminsw_qq a
let pminsw a = mk ~width:32 Opcode.Pminsw S.s_pminsw a
let pminsw_mq a = mk ~width:32 Opcode.Pminsw S.s_pminsw_mq a
let pminsw_m a = mk ~width:32 Opcode.Pminsw S.s_pminsw_m a
let pmaxsw_qq a = mk ~width:32 Opcode.Pmaxsw S.s_pmaxsw_qq a
let pmaxsw a = mk ~width:32 Opcode.Pmaxsw S.s_pmaxsw a
let pmaxsw_mq a = mk ~width:32 Opcode.Pmaxsw S.s_pmaxsw_mq a
let pmaxsw_m a = mk ~width:32 Opcode.Pmaxsw S.s_pmaxsw_m a
let pmullw_qq a = mk ~width:32 Opcode.Pmullw S.s_pmullw_qq a
let pmullw a = mk ~width:32 Opcode.Pmullw S.s_pmullw a
let pmullw_mq a = mk ~width:32 Opcode.Pmullw S.s_pmullw_mq a
let pmullw_m a = mk ~width:32 Opcode.Pmullw S.s_pmullw_m a
let pmulhw_qq a = mk ~width:32 Opcode.Pmulhw S.s_pmulhw_qq a
let pmulhw a = mk ~width:32 Opcode.Pmulhw S.s_pmulhw a
let pmulhw_mq a = mk ~width:32 Opcode.Pmulhw S.s_pmulhw_mq a
let pmulhw_m a = mk ~width:32 Opcode.Pmulhw S.s_pmulhw_m a
let pmulhuw_qq a = mk ~width:32 Opcode.Pmulhuw S.s_pmulhuw_qq a
let pmulhuw a = mk ~width:32 Opcode.Pmulhuw S.s_pmulhuw a
let pmulhuw_mq a = mk ~width:32 Opcode.Pmulhuw S.s_pmulhuw_mq a
let pmulhuw_m a = mk ~width:32 Opcode.Pmulhuw S.s_pmulhuw_m a
let pavgb_qq a = mk ~width:32 Opcode.Pavgb S.s_pavgb_qq a
let pavgb a = mk ~width:32 Opcode.Pavgb S.s_pavgb a
let pavgb_mq a = mk ~width:32 Opcode.Pavgb S.s_pavgb_mq a
let pavgb_m a = mk ~width:32 Opcode.Pavgb S.s_pavgb_m a
let pavgw_qq a = mk ~width:32 Opcode.Pavgw S.s_pavgw_qq a
let pavgw a = mk ~width:32 Opcode.Pavgw S.s_pavgw a
let pavgw_mq a = mk ~width:32 Opcode.Pavgw S.s_pavgw_mq a
let pavgw_m a = mk ~width:32 Opcode.Pavgw S.s_pavgw_m a
let psadbw_qq a = mk ~width:32 Opcode.Psadbw S.s_psadbw_qq a
let psadbw a = mk ~width:32 Opcode.Psadbw S.s_psadbw a
let psadbw_mq a = mk ~width:32 Opcode.Psadbw S.s_psadbw_mq a
let psadbw_m a = mk ~width:32 Opcode.Psadbw S.s_psadbw_m a
let psllw_qq a = mk ~width:32 Opcode.Psllw S.s_psllw_qq a
let psllw a = mk ~width:32 Opcode.Psllw S.s_psllw a
let psllw_iq a = mk ~width:32 Opcode.Psllw S.s_psllw_iq a
let psllw_iq_e a = mk_e ~width:32 Opcode.Psllw S.s_psllw_iq a
let psllw_i a = mk ~width:32 Opcode.Psllw S.s_psllw_i a
let psllw_i_e a = mk_e ~width:32 Opcode.Psllw S.s_psllw_i a
let psllw_mq a = mk ~width:32 Opcode.Psllw S.s_psllw_mq a
let psllw_m a = mk ~width:32 Opcode.Psllw S.s_psllw_m a
let pslld_qq a = mk ~width:32 Opcode.Pslld S.s_pslld_qq a
let pslld a = mk ~width:32 Opcode.Pslld S.s_pslld a
let pslld_iq a = mk ~width:32 Opcode.Pslld S.s_pslld_iq a
let pslld_iq_e a = mk_e ~width:32 Opcode.Pslld S.s_pslld_iq a
let pslld_i a = mk ~width:32 Opcode.Pslld S.s_pslld_i a
let pslld_i_e a = mk_e ~width:32 Opcode.Pslld S.s_pslld_i a
let pslld_mq a = mk ~width:32 Opcode.Pslld S.s_pslld_mq a
let pslld_m a = mk ~width:32 Opcode.Pslld S.s_pslld_m a
let psllq_qq a = mk ~width:32 Opcode.Psllq S.s_psllq_qq a
let psllq a = mk ~width:32 Opcode.Psllq S.s_psllq a
let psllq_iq a = mk ~width:32 Opcode.Psllq S.s_psllq_iq a
let psllq_iq_e a = mk_e ~width:32 Opcode.Psllq S.s_psllq_iq a
let psllq_i a = mk ~width:32 Opcode.Psllq S.s_psllq_i a
let psllq_i_e a = mk_e ~width:32 Opcode.Psllq S.s_psllq_i a
let psllq_mq a = mk ~width:32 Opcode.Psllq S.s_psllq_mq a
let psllq_m a = mk ~width:32 Opcode.Psllq S.s_psllq_m a
let psrlw_qq a = mk ~width:32 Opcode.Psrlw S.s_psrlw_qq a
let psrlw a = mk ~width:32 Opcode.Psrlw S.s_psrlw a
let psrlw_iq a = mk ~width:32 Opcode.Psrlw S.s_psrlw_iq a
let psrlw_iq_e a = mk_e ~width:32 Opcode.Psrlw S.s_psrlw_iq a
let psrlw_i a = mk ~width:32 Opcode.Psrlw S.s_psrlw_i a
let psrlw_i_e a = mk_e ~width:32 Opcode.Psrlw S.s_psrlw_i a
let psrlw_mq a = mk ~width:32 Opcode.Psrlw S.s_psrlw_mq a
let psrlw_m a = mk ~width:32 Opcode.Psrlw S.s_psrlw_m a
let psrld_qq a = mk ~width:32 Opcode.Psrld S.s_psrld_qq a
let psrld a = mk ~width:32 Opcode.Psrld S.s_psrld a
let psrld_iq a = mk ~width:32 Opcode.Psrld S.s_psrld_iq a
let psrld_iq_e a = mk_e ~width:32 Opcode.Psrld S.s_psrld_iq a
let psrld_i a = mk ~width:32 Opcode.Psrld S.s_psrld_i a
let psrld_i_e a = mk_e ~width:32 Opcode.Psrld S.s_psrld_i a
let psrld_mq a = mk ~width:32 Opcode.Psrld S.s_psrld_mq a
let psrld_m a = mk ~width:32 Opcode.Psrld S.s_psrld_m a
let psrlq_qq a = mk ~width:32 Opcode.Psrlq S.s_psrlq_qq a
let psrlq a = mk ~width:32 Opcode.Psrlq S.s_psrlq a
let psrlq_iq a = mk ~width:32 Opcode.Psrlq S.s_psrlq_iq a
let psrlq_iq_e a = mk_e ~width:32 Opcode.Psrlq S.s_psrlq_iq a
let psrlq_i a = mk ~width:32 Opcode.Psrlq S.s_psrlq_i a
let psrlq_i_e a = mk_e ~width:32 Opcode.Psrlq S.s_psrlq_i a
let psrlq_mq a = mk ~width:32 Opcode.Psrlq S.s_psrlq_mq a
let psrlq_m a = mk ~width:32 Opcode.Psrlq S.s_psrlq_m a
let psraw_qq a = mk ~width:32 Opcode.Psraw S.s_psraw_qq a
let psraw a = mk ~width:32 Opcode.Psraw S.s_psraw a
let psraw_iq a = mk ~width:32 Opcode.Psraw S.s_psraw_iq a
let psraw_iq_e a = mk_e ~width:32 Opcode.Psraw S.s_psraw_iq a
let psraw_i a = mk ~width:32 Opcode.Psraw S.s_psraw_i a
let psraw_i_e a = mk_e ~width:32 Opcode.Psraw S.s_psraw_i a
let psraw_mq a = mk ~width:32 Opcode.Psraw S.s_psraw_mq a
let psraw_m a = mk ~width:32 Opcode.Psraw S.s_psraw_m a
let psrad_qq a = mk ~width:32 Opcode.Psrad S.s_psrad_qq a
let psrad a = mk ~width:32 Opcode.Psrad S.s_psrad a
let psrad_iq a = mk ~width:32 Opcode.Psrad S.s_psrad_iq a
let psrad_iq_e a = mk_e ~width:32 Opcode.Psrad S.s_psrad_iq a
let psrad_i a = mk ~width:32 Opcode.Psrad S.s_psrad_i a
let psrad_i_e a = mk_e ~width:32 Opcode.Psrad S.s_psrad_i a
let psrad_mq a = mk ~width:32 Opcode.Psrad S.s_psrad_mq a
let psrad_m a = mk ~width:32 Opcode.Psrad S.s_psrad_m a
let pslldq_i a = mk ~width:32 Opcode.Pslldq S.s_pslldq_i a
let pslldq_i_e a = mk_e ~width:32 Opcode.Pslldq S.s_pslldq_i a
let psrldq_i a = mk ~width:32 Opcode.Psrldq S.s_psrldq_i a
let psrldq_i_e a = mk_e ~width:32 Opcode.Psrldq S.s_psrldq_i a
let pshufb_qq a = mk ~width:32 Opcode.Pshufb S.s_pshufb_qq a
let pshufb a = mk ~width:32 Opcode.Pshufb S.s_pshufb a
let pshufb_mq a = mk ~width:32 Opcode.Pshufb S.s_pshufb_mq a
let pshufb_m a = mk ~width:32 Opcode.Pshufb S.s_pshufb_m a
let phaddw_qq a = mk ~width:32 Opcode.Phaddw S.s_phaddw_qq a
let phaddw a = mk ~width:32 Opcode.Phaddw S.s_phaddw a
let phaddw_mq a = mk ~width:32 Opcode.Phaddw S.s_phaddw_mq a
let phaddw_m a = mk ~width:32 Opcode.Phaddw S.s_phaddw_m a
let phaddd_qq a = mk ~width:32 Opcode.Phaddd S.s_phaddd_qq a
let phaddd a = mk ~width:32 Opcode.Phaddd S.s_phaddd a
let phaddd_mq a = mk ~width:32 Opcode.Phaddd S.s_phaddd_mq a
let phaddd_m a = mk ~width:32 Opcode.Phaddd S.s_phaddd_m a
let phsubw_qq a = mk ~width:32 Opcode.Phsubw S.s_phsubw_qq a
let phsubw a = mk ~width:32 Opcode.Phsubw S.s_phsubw a
let phsubw_mq a = mk ~width:32 Opcode.Phsubw S.s_phsubw_mq a
let phsubw_m a = mk ~width:32 Opcode.Phsubw S.s_phsubw_m a
let phsubd_qq a = mk ~width:32 Opcode.Phsubd S.s_phsubd_qq a
let phsubd a = mk ~width:32 Opcode.Phsubd S.s_phsubd a
let phsubd_mq a = mk ~width:32 Opcode.Phsubd S.s_phsubd_mq a
let phsubd_m a = mk ~width:32 Opcode.Phsubd S.s_phsubd_m a
let psignb_qq a = mk ~width:32 Opcode.Psignb S.s_psignb_qq a
let psignb a = mk ~width:32 Opcode.Psignb S.s_psignb a
let psignb_mq a = mk ~width:32 Opcode.Psignb S.s_psignb_mq a
let psignb_m a = mk ~width:32 Opcode.Psignb S.s_psignb_m a
let psignw_qq a = mk ~width:32 Opcode.Psignw S.s_psignw_qq a
let psignw a = mk ~width:32 Opcode.Psignw S.s_psignw a
let psignw_mq a = mk ~width:32 Opcode.Psignw S.s_psignw_mq a
let psignw_m a = mk ~width:32 Opcode.Psignw S.s_psignw_m a
let psignd_qq a = mk ~width:32 Opcode.Psignd S.s_psignd_qq a
let psignd a = mk ~width:32 Opcode.Psignd S.s_psignd a
let psignd_mq a = mk ~width:32 Opcode.Psignd S.s_psignd_mq a
let psignd_m a = mk ~width:32 Opcode.Psignd S.s_psignd_m a
let pmaddubsw_qq a = mk ~width:32 Opcode.Pmaddubsw S.s_pmaddubsw_qq a
let pmaddubsw a = mk ~width:32 Opcode.Pmaddubsw S.s_pmaddubsw a
let pmaddubsw_mq a = mk ~width:32 Opcode.Pmaddubsw S.s_pmaddubsw_mq a
let pmaddubsw_m a = mk ~width:32 Opcode.Pmaddubsw S.s_pmaddubsw_m a
let pmulhrsw_qq a = mk ~width:32 Opcode.Pmulhrsw S.s_pmulhrsw_qq a
let pmulhrsw a = mk ~width:32 Opcode.Pmulhrsw S.s_pmulhrsw a
let pmulhrsw_mq a = mk ~width:32 Opcode.Pmulhrsw S.s_pmulhrsw_mq a
let pmulhrsw_m a = mk ~width:32 Opcode.Pmulhrsw S.s_pmulhrsw_m a
let phaddsw_qq a = mk ~width:32 Opcode.Phaddsw S.s_phaddsw_qq a
let phaddsw a = mk ~width:32 Opcode.Phaddsw S.s_phaddsw a
let phaddsw_mq a = mk ~width:32 Opcode.Phaddsw S.s_phaddsw_mq a
let phaddsw_m a = mk ~width:32 Opcode.Phaddsw S.s_phaddsw_m a
let phsubsw_qq a = mk ~width:32 Opcode.Phsubsw S.s_phsubsw_qq a
let phsubsw a = mk ~width:32 Opcode.Phsubsw S.s_phsubsw a
let phsubsw_mq a = mk ~width:32 Opcode.Phsubsw S.s_phsubsw_mq a
let phsubsw_m a = mk ~width:32 Opcode.Phsubsw S.s_phsubsw_m a
let pabsb_qq a = mk ~width:32 Opcode.Pabsb S.s_pabsb_qq a
let pabsb a = mk ~width:32 Opcode.Pabsb S.s_pabsb a
let pabsb_mq a = mk ~width:32 Opcode.Pabsb S.s_pabsb_mq a
let pabsb_m a = mk ~width:32 Opcode.Pabsb S.s_pabsb_m a
let pabsw_qq a = mk ~width:32 Opcode.Pabsw S.s_pabsw_qq a
let pabsw a = mk ~width:32 Opcode.Pabsw S.s_pabsw a
let pabsw_mq a = mk ~width:32 Opcode.Pabsw S.s_pabsw_mq a
let pabsw_m a = mk ~width:32 Opcode.Pabsw S.s_pabsw_m a
let pabsd_qq a = mk ~width:32 Opcode.Pabsd S.s_pabsd_qq a
let pabsd a = mk ~width:32 Opcode.Pabsd S.s_pabsd a
let pabsd_mq a = mk ~width:32 Opcode.Pabsd S.s_pabsd_mq a
let pabsd_m a = mk ~width:32 Opcode.Pabsd S.s_pabsd_m a
let palignr_iqq a = mk ~width:32 Opcode.Palignr S.s_palignr_iqq a
let palignr_iqq_e a = mk_e ~width:32 Opcode.Palignr S.s_palignr_iqq a
let palignr_i a = mk ~width:32 Opcode.Palignr S.s_palignr_i a
let palignr_i_e a = mk_e ~width:32 Opcode.Palignr S.s_palignr_i a
let palignr_imq a = mk ~width:32 Opcode.Palignr S.s_palignr_imq a
let palignr_imq_e a = mk_e ~width:32 Opcode.Palignr S.s_palignr_imq a
let palignr_im a = mk ~width:32 Opcode.Palignr S.s_palignr_im a
let palignr_im_e a = mk_e ~width:32 Opcode.Palignr S.s_palignr_im a
let roundps_i a = mk ~width:32 Opcode.Roundps S.s_roundps_i a
let roundps_i_e a = mk_e ~width:32 Opcode.Roundps S.s_roundps_i a
let roundps_im a = mk ~width:32 Opcode.Roundps S.s_roundps_im a
let roundps_im_e a = mk_e ~width:32 Opcode.Roundps S.s_roundps_im a
let roundpd_i a = mk ~width:32 Opcode.Roundpd S.s_roundpd_i a
let roundpd_i_e a = mk_e ~width:32 Opcode.Roundpd S.s_roundpd_i a
let roundpd_im a = mk ~width:32 Opcode.Roundpd S.s_roundpd_im a
let roundpd_im_e a = mk_e ~width:32 Opcode.Roundpd S.s_roundpd_im a
let roundss_i a = mk ~width:32 Opcode.Roundss S.s_roundss_i a
let roundss_i_e a = mk_e ~width:32 Opcode.Roundss S.s_roundss_i a
let roundss_im a = mk ~width:32 Opcode.Roundss S.s_roundss_im a
let roundss_im_e a = mk_e ~width:32 Opcode.Roundss S.s_roundss_im a
let roundsd_i a = mk ~width:32 Opcode.Roundsd S.s_roundsd_i a
let roundsd_i_e a = mk_e ~width:32 Opcode.Roundsd S.s_roundsd_i a
let roundsd_im a = mk ~width:32 Opcode.Roundsd S.s_roundsd_im a
let roundsd_im_e a = mk_e ~width:32 Opcode.Roundsd S.s_roundsd_im a
let pcmpeqq a = mk ~width:32 Opcode.Pcmpeqq S.s_pcmpeqq a
let pcmpeqq_m a = mk ~width:32 Opcode.Pcmpeqq S.s_pcmpeqq_m a
let pcmpgtq a = mk ~width:32 Opcode.Pcmpgtq S.s_pcmpgtq a
let pcmpgtq_m a = mk ~width:32 Opcode.Pcmpgtq S.s_pcmpgtq_m a
let packusdw a = mk ~width:32 Opcode.Packusdw S.s_packusdw a
let packusdw_m a = mk ~width:32 Opcode.Packusdw S.s_packusdw_m a
let pmaxsb a = mk ~width:32 Opcode.Pmaxsb S.s_pmaxsb a
let pmaxsb_m a = mk ~width:32 Opcode.Pmaxsb S.s_pmaxsb_m a
let pmaxsd a = mk ~width:32 Opcode.Pmaxsd S.s_pmaxsd a
let pmaxsd_m a = mk ~width:32 Opcode.Pmaxsd S.s_pmaxsd_m a
let pmaxud a = mk ~width:32 Opcode.Pmaxud S.s_pmaxud a
let pmaxud_m a = mk ~width:32 Opcode.Pmaxud S.s_pmaxud_m a
let pmaxuw a = mk ~width:32 Opcode.Pmaxuw S.s_pmaxuw a
let pmaxuw_m a = mk ~width:32 Opcode.Pmaxuw S.s_pmaxuw_m a
let pminsb a = mk ~width:32 Opcode.Pminsb S.s_pminsb a
let pminsb_m a = mk ~width:32 Opcode.Pminsb S.s_pminsb_m a
let pminsd a = mk ~width:32 Opcode.Pminsd S.s_pminsd a
let pminsd_m a = mk ~width:32 Opcode.Pminsd S.s_pminsd_m a
let pminud a = mk ~width:32 Opcode.Pminud S.s_pminud a
let pminud_m a = mk ~width:32 Opcode.Pminud S.s_pminud_m a
let pminuw a = mk ~width:32 Opcode.Pminuw S.s_pminuw a
let pminuw_m a = mk ~width:32 Opcode.Pminuw S.s_pminuw_m a
let pmuldq a = mk ~width:32 Opcode.Pmuldq S.s_pmuldq a
let pmuldq_m a = mk ~width:32 Opcode.Pmuldq S.s_pmuldq_m a
let pmulld a = mk ~width:32 Opcode.Pmulld S.s_pmulld a
let pmulld_m a = mk ~width:32 Opcode.Pmulld S.s_pmulld_m a
let phminposuw a = mk ~width:32 Opcode.Phminposuw S.s_phminposuw a
let phminposuw_m a = mk ~width:32 Opcode.Phminposuw S.s_phminposuw_m a
let ptest a = mk ~width:32 Opcode.Ptest S.s_ptest a
let ptest_m a = mk ~width:32 Opcode.Ptest S.s_ptest_m a
let pmovsxbw a = mk ~width:32 Opcode.Pmovsxbw S.s_pmovsxbw a
let pmovsxbw_m a = mk ~width:32 Opcode.Pmovsxbw S.s_pmovsxbw_m a
let pmovsxbd a = mk ~width:32 Opcode.Pmovsxbd S.s_pmovsxbd a
let pmovsxbd_m a = mk ~width:32 Opcode.Pmovsxbd S.s_pmovsxbd_m a
let pmovsxbq a = mk ~width:32 Opcode.Pmovsxbq S.s_pmovsxbq a
let pmovsxbq_m a = mk ~width:32 Opcode.Pmovsxbq S.s_pmovsxbq_m a
let pmovsxwd a = mk ~width:32 Opcode.Pmovsxwd S.s_pmovsxwd a
let pmovsxwd_m a = mk ~width:32 Opcode.Pmovsxwd S.s_pmovsxwd_m a
let pmovsxwq a = mk ~width:32 Opcode.Pmovsxwq S.s_pmovsxwq a
let pmovsxwq_m a = mk ~width:32 Opcode.Pmovsxwq S.s_pmovsxwq_m a
let pmovsxdq a = mk ~width:32 Opcode.Pmovsxdq S.s_pmovsxdq a
let pmovsxdq_m a = mk ~width:32 Opcode.Pmovsxdq S.s_pmovsxdq_m a
let pmovzxbw a = mk ~width:32 Opcode.Pmovzxbw S.s_pmovzxbw a
let pmovzxbw_m a = mk ~width:32 Opcode.Pmovzxbw S.s_pmovzxbw_m a
let pmovzxbd a = mk ~width:32 Opcode.Pmovzxbd S.s_pmovzxbd a
let pmovzxbd_m a = mk ~width:32 Opcode.Pmovzxbd S.s_pmovzxbd_m a
let pmovzxbq a = mk ~width:32 Opcode.Pmovzxbq S.s_pmovzxbq a
let pmovzxbq_m a = mk ~width:32 Opcode.Pmovzxbq S.s_pmovzxbq_m a
let pmovzxwd a = mk ~width:32 Opcode.Pmovzxwd S.s_pmovzxwd a
let pmovzxwd_m a = mk ~width:32 Opcode.Pmovzxwd S.s_pmovzxwd_m a
let pmovzxwq a = mk ~width:32 Opcode.Pmovzxwq S.s_pmovzxwq a
let pmovzxwq_m a = mk ~width:32 Opcode.Pmovzxwq S.s_pmovzxwq_m a
let pmovzxdq a = mk ~width:32 Opcode.Pmovzxdq S.s_pmovzxdq a
let pmovzxdq_m a = mk ~width:32 Opcode.Pmovzxdq S.s_pmovzxdq_m a
let movntdqa_m a = mk ~width:32 Opcode.Movntdqa S.s_movntdqa_m a
let blendvps a = mk ~width:32 Opcode.Blendvps S.s_blendvps a
let blendvps_m a = mk ~width:32 Opcode.Blendvps S.s_blendvps_m a
let blendvpd a = mk ~width:32 Opcode.Blendvpd S.s_blendvpd a
let blendvpd_m a = mk ~width:32 Opcode.Blendvpd S.s_blendvpd_m a
let pblendvb a = mk ~width:32 Opcode.Pblendvb S.s_pblendvb a
let pblendvb_m a = mk ~width:32 Opcode.Pblendvb S.s_pblendvb_m a
let blendps_i a = mk ~width:32 Opcode.Blendps S.s_blendps_i a
let blendps_i_e a = mk_e ~width:32 Opcode.Blendps S.s_blendps_i a
let blendps_im a = mk ~width:32 Opcode.Blendps S.s_blendps_im a
let blendps_im_e a = mk_e ~width:32 Opcode.Blendps S.s_blendps_im a
let blendpd_i a = mk ~width:32 Opcode.Blendpd S.s_blendpd_i a
let blendpd_i_e a = mk_e ~width:32 Opcode.Blendpd S.s_blendpd_i a
let blendpd_im a = mk ~width:32 Opcode.Blendpd S.s_blendpd_im a
let blendpd_im_e a = mk_e ~width:32 Opcode.Blendpd S.s_blendpd_im a
let dpps_i a = mk ~width:32 Opcode.Dpps S.s_dpps_i a
let dpps_i_e a = mk_e ~width:32 Opcode.Dpps S.s_dpps_i a
let dpps_im a = mk ~width:32 Opcode.Dpps S.s_dpps_im a
let dpps_im_e a = mk_e ~width:32 Opcode.Dpps S.s_dpps_im a
let dppd_i a = mk ~width:32 Opcode.Dppd S.s_dppd_i a
let dppd_i_e a = mk_e ~width:32 Opcode.Dppd S.s_dppd_i a
let dppd_im a = mk ~width:32 Opcode.Dppd S.s_dppd_im a
let dppd_im_e a = mk_e ~width:32 Opcode.Dppd S.s_dppd_im a
let mpsadbw_i a = mk ~width:32 Opcode.Mpsadbw S.s_mpsadbw_i a
let mpsadbw_i_e a = mk_e ~width:32 Opcode.Mpsadbw S.s_mpsadbw_i a
let mpsadbw_im a = mk ~width:32 Opcode.Mpsadbw S.s_mpsadbw_im a
let mpsadbw_im_e a = mk_e ~width:32 Opcode.Mpsadbw S.s_mpsadbw_im a
let pblendw_i a = mk ~width:32 Opcode.Pblendw S.s_pblendw_i a
let pblendw_i_e a = mk_e ~width:32 Opcode.Pblendw S.s_pblendw_i a
let pblendw_im a = mk ~width:32 Opcode.Pblendw S.s_pblendw_im a
let pblendw_im_e a = mk_e ~width:32 Opcode.Pblendw S.s_pblendw_im a
let insertps_i a = mk ~width:32 Opcode.Insertps S.s_insertps_i a
let insertps_i_e a = mk_e ~width:32 Opcode.Insertps S.s_insertps_i a
let insertps_im a = mk ~width:32 Opcode.Insertps S.s_insertps_im a
let insertps_im_e a = mk_e ~width:32 Opcode.Insertps S.s_insertps_im a
let pinsrb_i a = mk ~width:32 Opcode.Pinsrb S.s_pinsrb_i a
let pinsrb_i_e a = mk_e ~width:32 Opcode.Pinsrb S.s_pinsrb_i a
let pinsrb_im a = mk ~width:32 Opcode.Pinsrb S.s_pinsrb_im a
let pinsrb_im_e a = mk_e ~width:32 Opcode.Pinsrb S.s_pinsrb_im a
let pinsrd_i a = mk ~width:32 Opcode.Pinsrd S.s_pinsrd_i a
let pinsrd_i_e a = mk_e ~width:32 Opcode.Pinsrd S.s_pinsrd_i a
let pinsrd_im a = mk ~width:32 Opcode.Pinsrd S.s_pinsrd_im a
let pinsrd_im_e a = mk_e ~width:32 Opcode.Pinsrd S.s_pinsrd_im a
let pextrb_i a = mk ~width:32 Opcode.Pextrb S.s_pextrb_i a
let pextrb_i_e a = mk_e ~width:32 Opcode.Pextrb S.s_pextrb_i a
let pextrb_irm a = mk ~width:32 Opcode.Pextrb S.s_pextrb_irm a
let pextrb_irm_e a = mk_e ~width:32 Opcode.Pextrb S.s_pextrb_irm a
let pextrd_i a = mk ~width:32 Opcode.Pextrd S.s_pextrd_i a
let pextrd_i_e a = mk_e ~width:32 Opcode.Pextrd S.s_pextrd_i a
let pextrd_irm a = mk ~width:32 Opcode.Pextrd S.s_pextrd_irm a
let pextrd_irm_e a = mk_e ~width:32 Opcode.Pextrd S.s_pextrd_irm a
let extractps_i a = mk ~width:32 Opcode.Extractps S.s_extractps_i a
let extractps_i_e a = mk_e ~width:32 Opcode.Extractps S.s_extractps_i a
let extractps_irm a = mk ~width:32 Opcode.Extractps S.s_extractps_irm a
let extractps_irm_e a = mk_e ~width:32 Opcode.Extractps S.s_extractps_irm a
let vpshufb a = mk ~width:32 Opcode.Vpshufb S.s_vpshufb a
let vpshufb_m a = mk ~width:32 Opcode.Vpshufb S.s_vpshufb_m a
let vphaddw a = mk ~width:32 Opcode.Vphaddw S.s_vphaddw a
let vphaddw_m a = mk ~width:32 Opcode.Vphaddw S.s_vphaddw_m a
let vphaddd a = mk ~width:32 Opcode.Vphaddd S.s_vphaddd a
let vphaddd_m a = mk ~width:32 Opcode.Vphaddd S.s_vphaddd_m a
let vphaddsw a = mk ~width:32 Opcode.Vphaddsw S.s_vphaddsw a
let vphaddsw_m a = mk ~width:32 Opcode.Vphaddsw S.s_vphaddsw_m a
let vpmaddubsw a = mk ~width:32 Opcode.Vpmaddubsw S.s_vpmaddubsw a
let vpmaddubsw_m a = mk ~width:32 Opcode.Vpmaddubsw S.s_vpmaddubsw_m a
let vphsubw a = mk ~width:32 Opcode.Vphsubw S.s_vphsubw a
let vphsubw_m a = mk ~width:32 Opcode.Vphsubw S.s_vphsubw_m a
let vphsubd a = mk ~width:32 Opcode.Vphsubd S.s_vphsubd a
let vphsubd_m a = mk ~width:32 Opcode.Vphsubd S.s_vphsubd_m a
let vphsubsw a = mk ~width:32 Opcode.Vphsubsw S.s_vphsubsw a
let vphsubsw_m a = mk ~width:32 Opcode.Vphsubsw S.s_vphsubsw_m a
let vpsignb a = mk ~width:32 Opcode.Vpsignb S.s_vpsignb a
let vpsignb_m a = mk ~width:32 Opcode.Vpsignb S.s_vpsignb_m a
let vpsignw a = mk ~width:32 Opcode.Vpsignw S.s_vpsignw a
let vpsignw_m a = mk ~width:32 Opcode.Vpsignw S.s_vpsignw_m a
let vpsignd a = mk ~width:32 Opcode.Vpsignd S.s_vpsignd a
let vpsignd_m a = mk ~width:32 Opcode.Vpsignd S.s_vpsignd_m a
let vpmulhrsw a = mk ~width:32 Opcode.Vpmulhrsw S.s_vpmulhrsw a
let vpmulhrsw_m a = mk ~width:32 Opcode.Vpmulhrsw S.s_vpmulhrsw_m a
let vpmuldq a = mk ~width:32 Opcode.Vpmuldq S.s_vpmuldq a
let vpmuldq_m a = mk ~width:32 Opcode.Vpmuldq S.s_vpmuldq_m a
let vpcmpeqq a = mk ~width:32 Opcode.Vpcmpeqq S.s_vpcmpeqq a
let vpcmpeqq_rrk a = mk ~width:32 Opcode.Vpcmpeqq S.s_vpcmpeqq_rrk a
let vpcmpeqq_m a = mk ~width:32 Opcode.Vpcmpeqq S.s_vpcmpeqq_m a
let vpcmpeqq_mrk a = mk ~width:32 Opcode.Vpcmpeqq S.s_vpcmpeqq_mrk a
let vpackusdw a = mk ~width:32 Opcode.Vpackusdw S.s_vpackusdw a
let vpackusdw_m a = mk ~width:32 Opcode.Vpackusdw S.s_vpackusdw_m a
let vpcmpgtq a = mk ~width:32 Opcode.Vpcmpgtq S.s_vpcmpgtq a
let vpcmpgtq_rrk a = mk ~width:32 Opcode.Vpcmpgtq S.s_vpcmpgtq_rrk a
let vpcmpgtq_m a = mk ~width:32 Opcode.Vpcmpgtq S.s_vpcmpgtq_m a
let vpcmpgtq_mrk a = mk ~width:32 Opcode.Vpcmpgtq S.s_vpcmpgtq_mrk a
let vpminsb a = mk ~width:32 Opcode.Vpminsb S.s_vpminsb a
let vpminsb_m a = mk ~width:32 Opcode.Vpminsb S.s_vpminsb_m a
let vpminsd a = mk ~width:32 Opcode.Vpminsd S.s_vpminsd a
let vpminsd_m a = mk ~width:32 Opcode.Vpminsd S.s_vpminsd_m a
let vpminuw a = mk ~width:32 Opcode.Vpminuw S.s_vpminuw a
let vpminuw_m a = mk ~width:32 Opcode.Vpminuw S.s_vpminuw_m a
let vpminud a = mk ~width:32 Opcode.Vpminud S.s_vpminud a
let vpminud_m a = mk ~width:32 Opcode.Vpminud S.s_vpminud_m a
let vpmaxsb a = mk ~width:32 Opcode.Vpmaxsb S.s_vpmaxsb a
let vpmaxsb_m a = mk ~width:32 Opcode.Vpmaxsb S.s_vpmaxsb_m a
let vpmaxsd a = mk ~width:32 Opcode.Vpmaxsd S.s_vpmaxsd a
let vpmaxsd_m a = mk ~width:32 Opcode.Vpmaxsd S.s_vpmaxsd_m a
let vpmaxuw a = mk ~width:32 Opcode.Vpmaxuw S.s_vpmaxuw a
let vpmaxuw_m a = mk ~width:32 Opcode.Vpmaxuw S.s_vpmaxuw_m a
let vpmaxud a = mk ~width:32 Opcode.Vpmaxud S.s_vpmaxud a
let vpmaxud_m a = mk ~width:32 Opcode.Vpmaxud S.s_vpmaxud_m a
let vpmulld a = mk ~width:32 Opcode.Vpmulld S.s_vpmulld a
let vpmulld_m a = mk ~width:32 Opcode.Vpmulld S.s_vpmulld_m a
let vpalignr_i a = mk ~width:32 Opcode.Vpalignr S.s_vpalignr_i a
let vpalignr_i_e a = mk_e ~width:32 Opcode.Vpalignr S.s_vpalignr_i a
let vpalignr_im a = mk ~width:32 Opcode.Vpalignr S.s_vpalignr_im a
let vpalignr_im_e a = mk_e ~width:32 Opcode.Vpalignr S.s_vpalignr_im a
let vblendps_i a = mk ~width:32 Opcode.Vblendps S.s_vblendps_i a
let vblendps_i_e a = mk_e ~width:32 Opcode.Vblendps S.s_vblendps_i a
let vblendps_im a = mk ~width:32 Opcode.Vblendps S.s_vblendps_im a
let vblendps_im_e a = mk_e ~width:32 Opcode.Vblendps S.s_vblendps_im a
let vblendpd_i a = mk ~width:32 Opcode.Vblendpd S.s_vblendpd_i a
let vblendpd_i_e a = mk_e ~width:32 Opcode.Vblendpd S.s_vblendpd_i a
let vblendpd_im a = mk ~width:32 Opcode.Vblendpd S.s_vblendpd_im a
let vblendpd_im_e a = mk_e ~width:32 Opcode.Vblendpd S.s_vblendpd_im a
let vpblendw_i a = mk ~width:32 Opcode.Vpblendw S.s_vpblendw_i a
let vpblendw_i_e a = mk_e ~width:32 Opcode.Vpblendw S.s_vpblendw_i a
let vpblendw_im a = mk ~width:32 Opcode.Vpblendw S.s_vpblendw_im a
let vpblendw_im_e a = mk_e ~width:32 Opcode.Vpblendw S.s_vpblendw_im a
let vroundss_i a = mk ~width:32 Opcode.Vroundss S.s_vroundss_i a
let vroundss_i_e a = mk_e ~width:32 Opcode.Vroundss S.s_vroundss_i a
let vroundss_im a = mk ~width:32 Opcode.Vroundss S.s_vroundss_im a
let vroundss_im_e a = mk_e ~width:32 Opcode.Vroundss S.s_vroundss_im a
let vroundsd_i a = mk ~width:32 Opcode.Vroundsd S.s_vroundsd_i a
let vroundsd_i_e a = mk_e ~width:32 Opcode.Vroundsd S.s_vroundsd_i a
let vroundsd_im a = mk ~width:32 Opcode.Vroundsd S.s_vroundsd_im a
let vroundsd_im_e a = mk_e ~width:32 Opcode.Vroundsd S.s_vroundsd_im a
let vdpps_i a = mk ~width:32 Opcode.Vdpps S.s_vdpps_i a
let vdpps_i_e a = mk_e ~width:32 Opcode.Vdpps S.s_vdpps_i a
let vdpps_im a = mk ~width:32 Opcode.Vdpps S.s_vdpps_im a
let vdpps_im_e a = mk_e ~width:32 Opcode.Vdpps S.s_vdpps_im a
let vdppd_i a = mk ~width:32 Opcode.Vdppd S.s_vdppd_i a
let vdppd_i_e a = mk_e ~width:32 Opcode.Vdppd S.s_vdppd_i a
let vdppd_im a = mk ~width:32 Opcode.Vdppd S.s_vdppd_im a
let vdppd_im_e a = mk_e ~width:32 Opcode.Vdppd S.s_vdppd_im a
let vmpsadbw_i a = mk ~width:32 Opcode.Vmpsadbw S.s_vmpsadbw_i a
let vmpsadbw_i_e a = mk_e ~width:32 Opcode.Vmpsadbw S.s_vmpsadbw_i a
let vmpsadbw_im a = mk ~width:32 Opcode.Vmpsadbw S.s_vmpsadbw_im a
let vmpsadbw_im_e a = mk_e ~width:32 Opcode.Vmpsadbw S.s_vmpsadbw_im a
let vinsertps_i a = mk ~width:32 Opcode.Vinsertps S.s_vinsertps_i a
let vinsertps_i_e a = mk_e ~width:32 Opcode.Vinsertps S.s_vinsertps_i a
let vinsertps_im a = mk ~width:32 Opcode.Vinsertps S.s_vinsertps_im a
let vinsertps_im_e a = mk_e ~width:32 Opcode.Vinsertps S.s_vinsertps_im a
let vpabsb a = mk ~width:32 Opcode.Vpabsb S.s_vpabsb a
let vpabsb_m a = mk ~width:32 Opcode.Vpabsb S.s_vpabsb_m a
let vpabsw a = mk ~width:32 Opcode.Vpabsw S.s_vpabsw a
let vpabsw_m a = mk ~width:32 Opcode.Vpabsw S.s_vpabsw_m a
let vpabsd a = mk ~width:32 Opcode.Vpabsd S.s_vpabsd a
let vpabsd_m a = mk ~width:32 Opcode.Vpabsd S.s_vpabsd_m a
let vphminposuw a = mk ~width:32 Opcode.Vphminposuw S.s_vphminposuw a
let vphminposuw_m a = mk ~width:32 Opcode.Vphminposuw S.s_vphminposuw_m a
let vptest a = mk ~width:32 Opcode.Vptest S.s_vptest a
let vptest_m a = mk ~width:32 Opcode.Vptest S.s_vptest_m a
let vpmovsxbw a = mk ~width:32 Opcode.Vpmovsxbw S.s_vpmovsxbw a
let vpmovsxbw_rr a = mk ~width:32 Opcode.Vpmovsxbw S.s_vpmovsxbw_rr a
let vpmovsxbw_m a = mk ~width:32 Opcode.Vpmovsxbw S.s_vpmovsxbw_m a
let vpmovsxbd a = mk ~width:32 Opcode.Vpmovsxbd S.s_vpmovsxbd a
let vpmovsxbd_m a = mk ~width:32 Opcode.Vpmovsxbd S.s_vpmovsxbd_m a
let vpmovsxbq a = mk ~width:32 Opcode.Vpmovsxbq S.s_vpmovsxbq a
let vpmovsxbq_m a = mk ~width:32 Opcode.Vpmovsxbq S.s_vpmovsxbq_m a
let vpmovsxwd a = mk ~width:32 Opcode.Vpmovsxwd S.s_vpmovsxwd a
let vpmovsxwd_rr a = mk ~width:32 Opcode.Vpmovsxwd S.s_vpmovsxwd_rr a
let vpmovsxwd_m a = mk ~width:32 Opcode.Vpmovsxwd S.s_vpmovsxwd_m a
let vpmovsxwq a = mk ~width:32 Opcode.Vpmovsxwq S.s_vpmovsxwq a
let vpmovsxwq_m a = mk ~width:32 Opcode.Vpmovsxwq S.s_vpmovsxwq_m a
let vpmovsxdq a = mk ~width:32 Opcode.Vpmovsxdq S.s_vpmovsxdq a
let vpmovsxdq_rr a = mk ~width:32 Opcode.Vpmovsxdq S.s_vpmovsxdq_rr a
let vpmovsxdq_m a = mk ~width:32 Opcode.Vpmovsxdq S.s_vpmovsxdq_m a
let vpmovzxbw a = mk ~width:32 Opcode.Vpmovzxbw S.s_vpmovzxbw a
let vpmovzxbw_rr a = mk ~width:32 Opcode.Vpmovzxbw S.s_vpmovzxbw_rr a
let vpmovzxbw_m a = mk ~width:32 Opcode.Vpmovzxbw S.s_vpmovzxbw_m a
let vpmovzxbd a = mk ~width:32 Opcode.Vpmovzxbd S.s_vpmovzxbd a
let vpmovzxbd_m a = mk ~width:32 Opcode.Vpmovzxbd S.s_vpmovzxbd_m a
let vpmovzxbq a = mk ~width:32 Opcode.Vpmovzxbq S.s_vpmovzxbq a
let vpmovzxbq_m a = mk ~width:32 Opcode.Vpmovzxbq S.s_vpmovzxbq_m a
let vpmovzxwd a = mk ~width:32 Opcode.Vpmovzxwd S.s_vpmovzxwd a
let vpmovzxwd_rr a = mk ~width:32 Opcode.Vpmovzxwd S.s_vpmovzxwd_rr a
let vpmovzxwd_m a = mk ~width:32 Opcode.Vpmovzxwd S.s_vpmovzxwd_m a
let vpmovzxwq a = mk ~width:32 Opcode.Vpmovzxwq S.s_vpmovzxwq a
let vpmovzxwq_m a = mk ~width:32 Opcode.Vpmovzxwq S.s_vpmovzxwq_m a
let vpmovzxdq a = mk ~width:32 Opcode.Vpmovzxdq S.s_vpmovzxdq a
let vpmovzxdq_rr a = mk ~width:32 Opcode.Vpmovzxdq S.s_vpmovzxdq_rr a
let vpmovzxdq_m a = mk ~width:32 Opcode.Vpmovzxdq S.s_vpmovzxdq_m a
let vmovntdqa_m a = mk ~width:32 Opcode.Vmovntdqa S.s_vmovntdqa_m a
let movdqa a = mk ~width:32 Opcode.Movdqa S.s_movdqa a
let movdqa_m a = mk ~width:32 Opcode.Movdqa S.s_movdqa_m a
let movdqa_rm a = mk ~width:32 Opcode.Movdqa S.s_movdqa_rm a
let movdqu a = mk ~width:32 Opcode.Movdqu S.s_movdqu a
let movdqu_m a = mk ~width:32 Opcode.Movdqu S.s_movdqu_m a
let movdqu_rm a = mk ~width:32 Opcode.Movdqu S.s_movdqu_rm a
let andps a = mk ~width:32 Opcode.Andps S.s_andps a
let andps_m a = mk ~width:32 Opcode.Andps S.s_andps_m a
let andnps a = mk ~width:32 Opcode.Andnps S.s_andnps a
let andnps_m a = mk ~width:32 Opcode.Andnps S.s_andnps_m a
let orps a = mk ~width:32 Opcode.Orps S.s_orps a
let orps_m a = mk ~width:32 Opcode.Orps S.s_orps_m a
let xorps a = mk ~width:32 Opcode.Xorps S.s_xorps a
let xorps_m a = mk ~width:32 Opcode.Xorps S.s_xorps_m a
let andpd a = mk ~width:32 Opcode.Andpd S.s_andpd a
let andpd_m a = mk ~width:32 Opcode.Andpd S.s_andpd_m a
let andnpd a = mk ~width:32 Opcode.Andnpd S.s_andnpd a
let andnpd_m a = mk ~width:32 Opcode.Andnpd S.s_andnpd_m a
let orpd a = mk ~width:32 Opcode.Orpd S.s_orpd a
let orpd_m a = mk ~width:32 Opcode.Orpd S.s_orpd_m a
let movaps a = mk ~width:32 Opcode.Movaps S.s_movaps a
let movaps_m a = mk ~width:32 Opcode.Movaps S.s_movaps_m a
let movaps_rm a = mk ~width:32 Opcode.Movaps S.s_movaps_rm a
let movups a = mk ~width:32 Opcode.Movups S.s_movups a
let movups_m a = mk ~width:32 Opcode.Movups S.s_movups_m a
let movups_rm a = mk ~width:32 Opcode.Movups S.s_movups_rm a
let movupd a = mk ~width:32 Opcode.Movupd S.s_movupd a
let movupd_m a = mk ~width:32 Opcode.Movupd S.s_movupd_m a
let movupd_rm a = mk ~width:32 Opcode.Movupd S.s_movupd_rm a
let addps a = mk ~width:32 Opcode.Addps S.s_addps a
let addps_m a = mk ~width:32 Opcode.Addps S.s_addps_m a
let subps a = mk ~width:32 Opcode.Subps S.s_subps a
let subps_m a = mk ~width:32 Opcode.Subps S.s_subps_m a
let mulps a = mk ~width:32 Opcode.Mulps S.s_mulps a
let mulps_m a = mk ~width:32 Opcode.Mulps S.s_mulps_m a
let divps a = mk ~width:32 Opcode.Divps S.s_divps a
let divps_m a = mk ~width:32 Opcode.Divps S.s_divps_m a
let addpd a = mk ~width:32 Opcode.Addpd S.s_addpd a
let addpd_m a = mk ~width:32 Opcode.Addpd S.s_addpd_m a
let subpd a = mk ~width:32 Opcode.Subpd S.s_subpd a
let subpd_m a = mk ~width:32 Opcode.Subpd S.s_subpd_m a
let mulpd a = mk ~width:32 Opcode.Mulpd S.s_mulpd a
let mulpd_m a = mk ~width:32 Opcode.Mulpd S.s_mulpd_m a
let divpd a = mk ~width:32 Opcode.Divpd S.s_divpd a
let divpd_m a = mk ~width:32 Opcode.Divpd S.s_divpd_m a
let maxss a = mk ~width:32 Opcode.Maxss S.s_maxss a
let maxss_m a = mk ~width:32 Opcode.Maxss S.s_maxss_m a
let minss a = mk ~width:32 Opcode.Minss S.s_minss a
let minss_m a = mk ~width:32 Opcode.Minss S.s_minss_m a
let maxsd a = mk ~width:32 Opcode.Maxsd S.s_maxsd a
let maxsd_m a = mk ~width:32 Opcode.Maxsd S.s_maxsd_m a
let minsd a = mk ~width:32 Opcode.Minsd S.s_minsd a
let minsd_m a = mk ~width:32 Opcode.Minsd S.s_minsd_m a
let maxps a = mk ~width:32 Opcode.Maxps S.s_maxps a
let maxps_m a = mk ~width:32 Opcode.Maxps S.s_maxps_m a
let minps a = mk ~width:32 Opcode.Minps S.s_minps a
let minps_m a = mk ~width:32 Opcode.Minps S.s_minps_m a
let maxpd a = mk ~width:32 Opcode.Maxpd S.s_maxpd a
let maxpd_m a = mk ~width:32 Opcode.Maxpd S.s_maxpd_m a
let minpd a = mk ~width:32 Opcode.Minpd S.s_minpd a
let minpd_m a = mk ~width:32 Opcode.Minpd S.s_minpd_m a
let sqrtss a = mk ~width:32 Opcode.Sqrtss S.s_sqrtss a
let sqrtss_m a = mk ~width:32 Opcode.Sqrtss S.s_sqrtss_m a
let sqrtsd a = mk ~width:32 Opcode.Sqrtsd S.s_sqrtsd a
let sqrtsd_m a = mk ~width:32 Opcode.Sqrtsd S.s_sqrtsd_m a
let sqrtps a = mk ~width:32 Opcode.Sqrtps S.s_sqrtps a
let sqrtps_m a = mk ~width:32 Opcode.Sqrtps S.s_sqrtps_m a
let sqrtpd a = mk ~width:32 Opcode.Sqrtpd S.s_sqrtpd a
let sqrtpd_m a = mk ~width:32 Opcode.Sqrtpd S.s_sqrtpd_m a
let shufps_i a = mk ~width:32 Opcode.Shufps S.s_shufps_i a
let shufps_i_e a = mk_e ~width:32 Opcode.Shufps S.s_shufps_i a
let shufps_im a = mk ~width:32 Opcode.Shufps S.s_shufps_im a
let shufps_im_e a = mk_e ~width:32 Opcode.Shufps S.s_shufps_im a
let shufpd_i a = mk ~width:32 Opcode.Shufpd S.s_shufpd_i a
let shufpd_i_e a = mk_e ~width:32 Opcode.Shufpd S.s_shufpd_i a
let shufpd_im a = mk ~width:32 Opcode.Shufpd S.s_shufpd_im a
let shufpd_im_e a = mk_e ~width:32 Opcode.Shufpd S.s_shufpd_im a
let cmpss_i a = mk ~width:32 Opcode.Cmpss S.s_cmpss_i a
let cmpss_i_e a = mk_e ~width:32 Opcode.Cmpss S.s_cmpss_i a
let cmpss_im a = mk ~width:32 Opcode.Cmpss S.s_cmpss_im a
let cmpss_im_e a = mk_e ~width:32 Opcode.Cmpss S.s_cmpss_im a
let cmpsd_i a = mk ~width:32 Opcode.Cmpsd S.s_cmpsd_i a
let cmpsd_i_e a = mk_e ~width:32 Opcode.Cmpsd S.s_cmpsd_i a
let cmpsd_im a = mk ~width:32 Opcode.Cmpsd S.s_cmpsd_im a
let cmpsd_im_e a = mk_e ~width:32 Opcode.Cmpsd S.s_cmpsd_im a
let cmpps_i a = mk ~width:32 Opcode.Cmpps S.s_cmpps_i a
let cmpps_i_e a = mk_e ~width:32 Opcode.Cmpps S.s_cmpps_i a
let cmpps_im a = mk ~width:32 Opcode.Cmpps S.s_cmpps_im a
let cmpps_im_e a = mk_e ~width:32 Opcode.Cmpps S.s_cmpps_im a
let cmppd_i a = mk ~width:32 Opcode.Cmppd S.s_cmppd_i a
let cmppd_i_e a = mk_e ~width:32 Opcode.Cmppd S.s_cmppd_i a
let cmppd_im a = mk ~width:32 Opcode.Cmppd S.s_cmppd_im a
let cmppd_im_e a = mk_e ~width:32 Opcode.Cmppd S.s_cmppd_im a
let pshufd_i a = mk ~width:32 Opcode.Pshufd S.s_pshufd_i a
let pshufd_i_e a = mk_e ~width:32 Opcode.Pshufd S.s_pshufd_i a
let pshufd_im a = mk ~width:32 Opcode.Pshufd S.s_pshufd_im a
let pshufd_im_e a = mk_e ~width:32 Opcode.Pshufd S.s_pshufd_im a
let pshuflw_i a = mk ~width:32 Opcode.Pshuflw S.s_pshuflw_i a
let pshuflw_i_e a = mk_e ~width:32 Opcode.Pshuflw S.s_pshuflw_i a
let pshuflw_im a = mk ~width:32 Opcode.Pshuflw S.s_pshuflw_im a
let pshuflw_im_e a = mk_e ~width:32 Opcode.Pshuflw S.s_pshuflw_im a
let pshufhw_i a = mk ~width:32 Opcode.Pshufhw S.s_pshufhw_i a
let pshufhw_i_e a = mk_e ~width:32 Opcode.Pshufhw S.s_pshufhw_i a
let pshufhw_im a = mk ~width:32 Opcode.Pshufhw S.s_pshufhw_im a
let pshufhw_im_e a = mk_e ~width:32 Opcode.Pshufhw S.s_pshufhw_im a
let movd_q a = mk ~width:32 Opcode.Movd S.s_movd_q a
let movd_rq a = mk ~width:32 Opcode.Movd S.s_movd_rq a
let movd_rx a = mk ~width:32 Opcode.Movd S.s_movd_rx a
let movd_x a = mk ~width:32 Opcode.Movd S.s_movd_x a
let movd_mq a = mk ~width:32 Opcode.Movd S.s_movd_mq a
let movd_mx a = mk ~width:32 Opcode.Movd S.s_movd_mx a
let movd_qm a = mk ~width:32 Opcode.Movd S.s_movd_qm a
let movd_xm a = mk ~width:32 Opcode.Movd S.s_movd_xm a
let vaddsd a = mk ~width:32 Opcode.Vaddsd S.s_vaddsd a
let vaddsd_m a = mk ~width:32 Opcode.Vaddsd S.s_vaddsd_m a
let vsubsd a = mk ~width:32 Opcode.Vsubsd S.s_vsubsd a
let vsubsd_m a = mk ~width:32 Opcode.Vsubsd S.s_vsubsd_m a
let vmulsd a = mk ~width:32 Opcode.Vmulsd S.s_vmulsd a
let vmulsd_m a = mk ~width:32 Opcode.Vmulsd S.s_vmulsd_m a
let vdivsd a = mk ~width:32 Opcode.Vdivsd S.s_vdivsd a
let vdivsd_m a = mk ~width:32 Opcode.Vdivsd S.s_vdivsd_m a
let vaddss a = mk ~width:32 Opcode.Vaddss S.s_vaddss a
let vaddss_m a = mk ~width:32 Opcode.Vaddss S.s_vaddss_m a
let vsubss a = mk ~width:32 Opcode.Vsubss S.s_vsubss a
let vsubss_m a = mk ~width:32 Opcode.Vsubss S.s_vsubss_m a
let vmulss a = mk ~width:32 Opcode.Vmulss S.s_vmulss a
let vmulss_m a = mk ~width:32 Opcode.Vmulss S.s_vmulss_m a
let vdivss a = mk ~width:32 Opcode.Vdivss S.s_vdivss a
let vdivss_m a = mk ~width:32 Opcode.Vdivss S.s_vdivss_m a
let vaddps a = mk ~width:32 Opcode.Vaddps S.s_vaddps a
let vaddps_m a = mk ~width:32 Opcode.Vaddps S.s_vaddps_m a
let vsubps a = mk ~width:32 Opcode.Vsubps S.s_vsubps a
let vsubps_m a = mk ~width:32 Opcode.Vsubps S.s_vsubps_m a
let vmulps a = mk ~width:32 Opcode.Vmulps S.s_vmulps a
let vmulps_m a = mk ~width:32 Opcode.Vmulps S.s_vmulps_m a
let vdivps a = mk ~width:32 Opcode.Vdivps S.s_vdivps a
let vdivps_m a = mk ~width:32 Opcode.Vdivps S.s_vdivps_m a
let vaddpd a = mk ~width:32 Opcode.Vaddpd S.s_vaddpd a
let vaddpd_m a = mk ~width:32 Opcode.Vaddpd S.s_vaddpd_m a
let vsubpd a = mk ~width:32 Opcode.Vsubpd S.s_vsubpd a
let vsubpd_m a = mk ~width:32 Opcode.Vsubpd S.s_vsubpd_m a
let vmulpd a = mk ~width:32 Opcode.Vmulpd S.s_vmulpd a
let vmulpd_m a = mk ~width:32 Opcode.Vmulpd S.s_vmulpd_m a
let vdivpd a = mk ~width:32 Opcode.Vdivpd S.s_vdivpd a
let vdivpd_m a = mk ~width:32 Opcode.Vdivpd S.s_vdivpd_m a
let vandps a = mk ~width:32 Opcode.Vandps S.s_vandps a
let vandps_m a = mk ~width:32 Opcode.Vandps S.s_vandps_m a
let vandnps a = mk ~width:32 Opcode.Vandnps S.s_vandnps a
let vandnps_m a = mk ~width:32 Opcode.Vandnps S.s_vandnps_m a
let vorps a = mk ~width:32 Opcode.Vorps S.s_vorps a
let vorps_m a = mk ~width:32 Opcode.Vorps S.s_vorps_m a
let vxorps a = mk ~width:32 Opcode.Vxorps S.s_vxorps a
let vxorps_m a = mk ~width:32 Opcode.Vxorps S.s_vxorps_m a
let vandpd a = mk ~width:32 Opcode.Vandpd S.s_vandpd a
let vandpd_m a = mk ~width:32 Opcode.Vandpd S.s_vandpd_m a
let vandnpd a = mk ~width:32 Opcode.Vandnpd S.s_vandnpd a
let vandnpd_m a = mk ~width:32 Opcode.Vandnpd S.s_vandnpd_m a
let vorpd a = mk ~width:32 Opcode.Vorpd S.s_vorpd a
let vorpd_m a = mk ~width:32 Opcode.Vorpd S.s_vorpd_m a
let vxorpd a = mk ~width:32 Opcode.Vxorpd S.s_vxorpd a
let vxorpd_m a = mk ~width:32 Opcode.Vxorpd S.s_vxorpd_m a
let vunpcklps a = mk ~width:32 Opcode.Vunpcklps S.s_vunpcklps a
let vunpcklps_m a = mk ~width:32 Opcode.Vunpcklps S.s_vunpcklps_m a
let vunpckhps a = mk ~width:32 Opcode.Vunpckhps S.s_vunpckhps a
let vunpckhps_m a = mk ~width:32 Opcode.Vunpckhps S.s_vunpckhps_m a
let vunpcklpd a = mk ~width:32 Opcode.Vunpcklpd S.s_vunpcklpd a
let vunpcklpd_m a = mk ~width:32 Opcode.Vunpcklpd S.s_vunpcklpd_m a
let vunpckhpd a = mk ~width:32 Opcode.Vunpckhpd S.s_vunpckhpd a
let vunpckhpd_m a = mk ~width:32 Opcode.Vunpckhpd S.s_vunpckhpd_m a
let vpunpcklqdq a = mk ~width:32 Opcode.Vpunpcklqdq S.s_vpunpcklqdq a
let vpunpcklqdq_m a = mk ~width:32 Opcode.Vpunpcklqdq S.s_vpunpcklqdq_m a
let vpunpckhqdq a = mk ~width:32 Opcode.Vpunpckhqdq S.s_vpunpckhqdq a
let vpunpckhqdq_m a = mk ~width:32 Opcode.Vpunpckhqdq S.s_vpunpckhqdq_m a
let vpunpcklbw a = mk ~width:32 Opcode.Vpunpcklbw S.s_vpunpcklbw a
let vpunpcklbw_m a = mk ~width:32 Opcode.Vpunpcklbw S.s_vpunpcklbw_m a
let vpunpckhbw a = mk ~width:32 Opcode.Vpunpckhbw S.s_vpunpckhbw a
let vpunpckhbw_m a = mk ~width:32 Opcode.Vpunpckhbw S.s_vpunpckhbw_m a
let vpunpcklwd a = mk ~width:32 Opcode.Vpunpcklwd S.s_vpunpcklwd a
let vpunpcklwd_m a = mk ~width:32 Opcode.Vpunpcklwd S.s_vpunpcklwd_m a
let vpunpckhwd a = mk ~width:32 Opcode.Vpunpckhwd S.s_vpunpckhwd a
let vpunpckhwd_m a = mk ~width:32 Opcode.Vpunpckhwd S.s_vpunpckhwd_m a
let vpunpckldq a = mk ~width:32 Opcode.Vpunpckldq S.s_vpunpckldq a
let vpunpckldq_m a = mk ~width:32 Opcode.Vpunpckldq S.s_vpunpckldq_m a
let vpunpckhdq a = mk ~width:32 Opcode.Vpunpckhdq S.s_vpunpckhdq a
let vpunpckhdq_m a = mk ~width:32 Opcode.Vpunpckhdq S.s_vpunpckhdq_m a
let vpaddb a = mk ~width:32 Opcode.Vpaddb S.s_vpaddb a
let vpaddb_m a = mk ~width:32 Opcode.Vpaddb S.s_vpaddb_m a
let vpaddw a = mk ~width:32 Opcode.Vpaddw S.s_vpaddw a
let vpaddw_m a = mk ~width:32 Opcode.Vpaddw S.s_vpaddw_m a
let vpaddd a = mk ~width:32 Opcode.Vpaddd S.s_vpaddd a
let vpaddd_m a = mk ~width:32 Opcode.Vpaddd S.s_vpaddd_m a
let vpaddq a = mk ~width:32 Opcode.Vpaddq S.s_vpaddq a
let vpaddq_m a = mk ~width:32 Opcode.Vpaddq S.s_vpaddq_m a
let vpsubb a = mk ~width:32 Opcode.Vpsubb S.s_vpsubb a
let vpsubb_m a = mk ~width:32 Opcode.Vpsubb S.s_vpsubb_m a
let vpsubw a = mk ~width:32 Opcode.Vpsubw S.s_vpsubw a
let vpsubw_m a = mk ~width:32 Opcode.Vpsubw S.s_vpsubw_m a
let vpsubd a = mk ~width:32 Opcode.Vpsubd S.s_vpsubd a
let vpsubd_m a = mk ~width:32 Opcode.Vpsubd S.s_vpsubd_m a
let vpsubq a = mk ~width:32 Opcode.Vpsubq S.s_vpsubq a
let vpsubq_m a = mk ~width:32 Opcode.Vpsubq S.s_vpsubq_m a
let vpcmpeqb a = mk ~width:32 Opcode.Vpcmpeqb S.s_vpcmpeqb a
let vpcmpeqb_rrk a = mk ~width:32 Opcode.Vpcmpeqb S.s_vpcmpeqb_rrk a
let vpcmpeqb_m a = mk ~width:32 Opcode.Vpcmpeqb S.s_vpcmpeqb_m a
let vpcmpeqb_mrk a = mk ~width:32 Opcode.Vpcmpeqb S.s_vpcmpeqb_mrk a
let vpcmpeqw a = mk ~width:32 Opcode.Vpcmpeqw S.s_vpcmpeqw a
let vpcmpeqw_rrk a = mk ~width:32 Opcode.Vpcmpeqw S.s_vpcmpeqw_rrk a
let vpcmpeqw_m a = mk ~width:32 Opcode.Vpcmpeqw S.s_vpcmpeqw_m a
let vpcmpeqw_mrk a = mk ~width:32 Opcode.Vpcmpeqw S.s_vpcmpeqw_mrk a
let vpcmpeqd a = mk ~width:32 Opcode.Vpcmpeqd S.s_vpcmpeqd a
let vpcmpeqd_rrk a = mk ~width:32 Opcode.Vpcmpeqd S.s_vpcmpeqd_rrk a
let vpcmpeqd_m a = mk ~width:32 Opcode.Vpcmpeqd S.s_vpcmpeqd_m a
let vpcmpeqd_mrk a = mk ~width:32 Opcode.Vpcmpeqd S.s_vpcmpeqd_mrk a
let vpcmpgtb a = mk ~width:32 Opcode.Vpcmpgtb S.s_vpcmpgtb a
let vpcmpgtb_rrk a = mk ~width:32 Opcode.Vpcmpgtb S.s_vpcmpgtb_rrk a
let vpcmpgtb_m a = mk ~width:32 Opcode.Vpcmpgtb S.s_vpcmpgtb_m a
let vpcmpgtb_mrk a = mk ~width:32 Opcode.Vpcmpgtb S.s_vpcmpgtb_mrk a
let vpcmpgtw a = mk ~width:32 Opcode.Vpcmpgtw S.s_vpcmpgtw a
let vpcmpgtw_rrk a = mk ~width:32 Opcode.Vpcmpgtw S.s_vpcmpgtw_rrk a
let vpcmpgtw_m a = mk ~width:32 Opcode.Vpcmpgtw S.s_vpcmpgtw_m a
let vpcmpgtw_mrk a = mk ~width:32 Opcode.Vpcmpgtw S.s_vpcmpgtw_mrk a
let vpcmpgtd a = mk ~width:32 Opcode.Vpcmpgtd S.s_vpcmpgtd a
let vpcmpgtd_rrk a = mk ~width:32 Opcode.Vpcmpgtd S.s_vpcmpgtd_rrk a
let vpcmpgtd_m a = mk ~width:32 Opcode.Vpcmpgtd S.s_vpcmpgtd_m a
let vpcmpgtd_mrk a = mk ~width:32 Opcode.Vpcmpgtd S.s_vpcmpgtd_mrk a
let vpacksswb a = mk ~width:32 Opcode.Vpacksswb S.s_vpacksswb a
let vpacksswb_m a = mk ~width:32 Opcode.Vpacksswb S.s_vpacksswb_m a
let vpackssdw a = mk ~width:32 Opcode.Vpackssdw S.s_vpackssdw a
let vpackssdw_m a = mk ~width:32 Opcode.Vpackssdw S.s_vpackssdw_m a
let vpackuswb a = mk ~width:32 Opcode.Vpackuswb S.s_vpackuswb a
let vpackuswb_m a = mk ~width:32 Opcode.Vpackuswb S.s_vpackuswb_m a
let vpand a = mk ~width:32 Opcode.Vpand S.s_vpand a
let vpand_m a = mk ~width:32 Opcode.Vpand S.s_vpand_m a
let vpandn a = mk ~width:32 Opcode.Vpandn S.s_vpandn a
let vpandn_m a = mk ~width:32 Opcode.Vpandn S.s_vpandn_m a
let vpor a = mk ~width:32 Opcode.Vpor S.s_vpor a
let vpor_m a = mk ~width:32 Opcode.Vpor S.s_vpor_m a
let vpminub a = mk ~width:32 Opcode.Vpminub S.s_vpminub a
let vpminub_m a = mk ~width:32 Opcode.Vpminub S.s_vpminub_m a
let vpmaxub a = mk ~width:32 Opcode.Vpmaxub S.s_vpmaxub a
let vpmaxub_m a = mk ~width:32 Opcode.Vpmaxub S.s_vpmaxub_m a
let vpminsw a = mk ~width:32 Opcode.Vpminsw S.s_vpminsw a
let vpminsw_m a = mk ~width:32 Opcode.Vpminsw S.s_vpminsw_m a
let vpmaxsw a = mk ~width:32 Opcode.Vpmaxsw S.s_vpmaxsw a
let vpmaxsw_m a = mk ~width:32 Opcode.Vpmaxsw S.s_vpmaxsw_m a
let vmaxsd a = mk ~width:32 Opcode.Vmaxsd S.s_vmaxsd a
let vmaxsd_m a = mk ~width:32 Opcode.Vmaxsd S.s_vmaxsd_m a
let vminsd a = mk ~width:32 Opcode.Vminsd S.s_vminsd a
let vminsd_m a = mk ~width:32 Opcode.Vminsd S.s_vminsd_m a
let vmaxss a = mk ~width:32 Opcode.Vmaxss S.s_vmaxss a
let vmaxss_m a = mk ~width:32 Opcode.Vmaxss S.s_vmaxss_m a
let vminss a = mk ~width:32 Opcode.Vminss S.s_vminss a
let vminss_m a = mk ~width:32 Opcode.Vminss S.s_vminss_m a
let vmaxps a = mk ~width:32 Opcode.Vmaxps S.s_vmaxps a
let vmaxps_m a = mk ~width:32 Opcode.Vmaxps S.s_vmaxps_m a
let vminps a = mk ~width:32 Opcode.Vminps S.s_vminps a
let vminps_m a = mk ~width:32 Opcode.Vminps S.s_vminps_m a
let vmaxpd a = mk ~width:32 Opcode.Vmaxpd S.s_vmaxpd a
let vmaxpd_m a = mk ~width:32 Opcode.Vmaxpd S.s_vmaxpd_m a
let vminpd a = mk ~width:32 Opcode.Vminpd S.s_vminpd a
let vminpd_m a = mk ~width:32 Opcode.Vminpd S.s_vminpd_m a
let vsqrtsd a = mk ~width:32 Opcode.Vsqrtsd S.s_vsqrtsd a
let vsqrtsd_m a = mk ~width:32 Opcode.Vsqrtsd S.s_vsqrtsd_m a
let vsqrtss a = mk ~width:32 Opcode.Vsqrtss S.s_vsqrtss a
let vsqrtss_m a = mk ~width:32 Opcode.Vsqrtss S.s_vsqrtss_m a
let vsqrtps a = mk ~width:32 Opcode.Vsqrtps S.s_vsqrtps a
let vsqrtps_m a = mk ~width:32 Opcode.Vsqrtps S.s_vsqrtps_m a
let vsqrtpd a = mk ~width:32 Opcode.Vsqrtpd S.s_vsqrtpd a
let vsqrtpd_m a = mk ~width:32 Opcode.Vsqrtpd S.s_vsqrtpd_m a
let vmovaps a = mk ~width:32 Opcode.Vmovaps S.s_vmovaps a
let vmovaps_rm a = mk ~width:32 Opcode.Vmovaps S.s_vmovaps_rm a
let vmovaps_m a = mk ~width:32 Opcode.Vmovaps S.s_vmovaps_m a
let vmovups a = mk ~width:32 Opcode.Vmovups S.s_vmovups a
let vmovups_rm a = mk ~width:32 Opcode.Vmovups S.s_vmovups_rm a
let vmovups_m a = mk ~width:32 Opcode.Vmovups S.s_vmovups_m a
let vmovapd a = mk ~width:32 Opcode.Vmovapd S.s_vmovapd a
let vmovapd_rm a = mk ~width:32 Opcode.Vmovapd S.s_vmovapd_rm a
let vmovapd_m a = mk ~width:32 Opcode.Vmovapd S.s_vmovapd_m a
let vmovupd a = mk ~width:32 Opcode.Vmovupd S.s_vmovupd a
let vmovupd_rm a = mk ~width:32 Opcode.Vmovupd S.s_vmovupd_rm a
let vmovupd_m a = mk ~width:32 Opcode.Vmovupd S.s_vmovupd_m a
let vcomisd a = mk ~width:32 Opcode.Vcomisd S.s_vcomisd a
let vcomisd_m a = mk ~width:32 Opcode.Vcomisd S.s_vcomisd_m a
let vucomisd a = mk ~width:32 Opcode.Vucomisd S.s_vucomisd a
let vucomisd_m a = mk ~width:32 Opcode.Vucomisd S.s_vucomisd_m a
let vcomiss a = mk ~width:32 Opcode.Vcomiss S.s_vcomiss a
let vcomiss_m a = mk ~width:32 Opcode.Vcomiss S.s_vcomiss_m a
let vucomiss a = mk ~width:32 Opcode.Vucomiss S.s_vucomiss a
let vucomiss_m a = mk ~width:32 Opcode.Vucomiss S.s_vucomiss_m a
let vcvtps2pd a = mk ~width:32 Opcode.Vcvtps2pd S.s_vcvtps2pd a
let vcvtps2pd_rr a = mk ~width:32 Opcode.Vcvtps2pd S.s_vcvtps2pd_rr a
let vcvtps2pd_m a = mk ~width:32 Opcode.Vcvtps2pd S.s_vcvtps2pd_m a
let vcvtpd2ps a = mk ~width:32 Opcode.Vcvtpd2ps S.s_vcvtpd2ps a
let vcvtpd2ps_rr a = mk ~width:32 Opcode.Vcvtpd2ps S.s_vcvtpd2ps_rr a
let vcvtpd2ps_zy a = mk ~width:32 Opcode.Vcvtpd2ps S.s_vcvtpd2ps_zy a
let vcvtpd2ps_m a = mk ~width:32 Opcode.Vcvtpd2ps S.s_vcvtpd2ps_m a
let vcvtdq2ps a = mk ~width:32 Opcode.Vcvtdq2ps S.s_vcvtdq2ps a
let vcvtdq2ps_m a = mk ~width:32 Opcode.Vcvtdq2ps S.s_vcvtdq2ps_m a
let vcvtps2dq a = mk ~width:32 Opcode.Vcvtps2dq S.s_vcvtps2dq a
let vcvtps2dq_m a = mk ~width:32 Opcode.Vcvtps2dq S.s_vcvtps2dq_m a
let vcvttps2dq a = mk ~width:32 Opcode.Vcvttps2dq S.s_vcvttps2dq a
let vcvttps2dq_m a = mk ~width:32 Opcode.Vcvttps2dq S.s_vcvttps2dq_m a
let vshufps_i a = mk ~width:32 Opcode.Vshufps S.s_vshufps_i a
let vshufps_i_e a = mk_e ~width:32 Opcode.Vshufps S.s_vshufps_i a
let vshufps_im a = mk ~width:32 Opcode.Vshufps S.s_vshufps_im a
let vshufps_im_e a = mk_e ~width:32 Opcode.Vshufps S.s_vshufps_im a
let vshufpd_i a = mk ~width:32 Opcode.Vshufpd S.s_vshufpd_i a
let vshufpd_i_e a = mk_e ~width:32 Opcode.Vshufpd S.s_vshufpd_i a
let vshufpd_im a = mk ~width:32 Opcode.Vshufpd S.s_vshufpd_im a
let vshufpd_im_e a = mk_e ~width:32 Opcode.Vshufpd S.s_vshufpd_im a
let vcmpss_i a = mk ~width:32 Opcode.Vcmpss S.s_vcmpss_i a
let vcmpss_i_e a = mk_e ~width:32 Opcode.Vcmpss S.s_vcmpss_i a
let vcmpss_im a = mk ~width:32 Opcode.Vcmpss S.s_vcmpss_im a
let vcmpss_im_e a = mk_e ~width:32 Opcode.Vcmpss S.s_vcmpss_im a
let vcmpsd_i a = mk ~width:32 Opcode.Vcmpsd S.s_vcmpsd_i a
let vcmpsd_i_e a = mk_e ~width:32 Opcode.Vcmpsd S.s_vcmpsd_i a
let vcmpsd_im a = mk ~width:32 Opcode.Vcmpsd S.s_vcmpsd_im a
let vcmpsd_im_e a = mk_e ~width:32 Opcode.Vcmpsd S.s_vcmpsd_im a
let vcmpps_i a = mk ~width:32 Opcode.Vcmpps S.s_vcmpps_i a
let vcmpps_i_e a = mk_e ~width:32 Opcode.Vcmpps S.s_vcmpps_i a
let vcmpps_im a = mk ~width:32 Opcode.Vcmpps S.s_vcmpps_im a
let vcmpps_im_e a = mk_e ~width:32 Opcode.Vcmpps S.s_vcmpps_im a
let vcmppd_i a = mk ~width:32 Opcode.Vcmppd S.s_vcmppd_i a
let vcmppd_i_e a = mk_e ~width:32 Opcode.Vcmppd S.s_vcmppd_i a
let vcmppd_im a = mk ~width:32 Opcode.Vcmppd S.s_vcmppd_im a
let vcmppd_im_e a = mk_e ~width:32 Opcode.Vcmppd S.s_vcmppd_im a
let vpshufd_i a = mk ~width:32 Opcode.Vpshufd S.s_vpshufd_i a
let vpshufd_i_e a = mk_e ~width:32 Opcode.Vpshufd S.s_vpshufd_i a
let vpshufd_im a = mk ~width:32 Opcode.Vpshufd S.s_vpshufd_im a
let vpshufd_im_e a = mk_e ~width:32 Opcode.Vpshufd S.s_vpshufd_im a
let vpshuflw_i a = mk ~width:32 Opcode.Vpshuflw S.s_vpshuflw_i a
let vpshuflw_i_e a = mk_e ~width:32 Opcode.Vpshuflw S.s_vpshuflw_i a
let vpshuflw_im a = mk ~width:32 Opcode.Vpshuflw S.s_vpshuflw_im a
let vpshuflw_im_e a = mk_e ~width:32 Opcode.Vpshuflw S.s_vpshuflw_im a
let vpshufhw_i a = mk ~width:32 Opcode.Vpshufhw S.s_vpshufhw_i a
let vpshufhw_i_e a = mk_e ~width:32 Opcode.Vpshufhw S.s_vpshufhw_i a
let vpshufhw_im a = mk ~width:32 Opcode.Vpshufhw S.s_vpshufhw_im a
let vpshufhw_im_e a = mk_e ~width:32 Opcode.Vpshufhw S.s_vpshufhw_im a
let vmovd a = mk ~width:32 Opcode.Vmovd S.s_vmovd a
let vmovd_rr a = mk ~width:32 Opcode.Vmovd S.s_vmovd_rr a
let vmovd_xx a = mk ~width:32 Opcode.Vmovd S.s_vmovd_xx a
let vmovd_m a = mk ~width:32 Opcode.Vmovd S.s_vmovd_m a
let vmovd_rm a = mk ~width:32 Opcode.Vmovd S.s_vmovd_rm a
let vpmullw a = mk ~width:32 Opcode.Vpmullw S.s_vpmullw a
let vpmullw_m a = mk ~width:32 Opcode.Vpmullw S.s_vpmullw_m a
let vpmulhw a = mk ~width:32 Opcode.Vpmulhw S.s_vpmulhw a
let vpmulhw_m a = mk ~width:32 Opcode.Vpmulhw S.s_vpmulhw_m a
let vpmulhuw a = mk ~width:32 Opcode.Vpmulhuw S.s_vpmulhuw a
let vpmulhuw_m a = mk ~width:32 Opcode.Vpmulhuw S.s_vpmulhuw_m a
let vpavgb a = mk ~width:32 Opcode.Vpavgb S.s_vpavgb a
let vpavgb_m a = mk ~width:32 Opcode.Vpavgb S.s_vpavgb_m a
let vpavgw a = mk ~width:32 Opcode.Vpavgw S.s_vpavgw a
let vpavgw_m a = mk ~width:32 Opcode.Vpavgw S.s_vpavgw_m a
let vpsadbw a = mk ~width:32 Opcode.Vpsadbw S.s_vpsadbw a
let vpsadbw_m a = mk ~width:32 Opcode.Vpsadbw S.s_vpsadbw_m a
let vpsllw a = mk ~width:32 Opcode.Vpsllw S.s_vpsllw a
let vpsllw_i a = mk ~width:32 Opcode.Vpsllw S.s_vpsllw_i a
let vpsllw_i_e a = mk_e ~width:32 Opcode.Vpsllw S.s_vpsllw_i a
let vpsllw_m a = mk ~width:32 Opcode.Vpsllw S.s_vpsllw_m a
let vpsllw_im a = mk ~width:32 Opcode.Vpsllw S.s_vpsllw_im a
let vpsllw_im_e a = mk_e ~width:32 Opcode.Vpsllw S.s_vpsllw_im a
let vpslld a = mk ~width:32 Opcode.Vpslld S.s_vpslld a
let vpslld_i a = mk ~width:32 Opcode.Vpslld S.s_vpslld_i a
let vpslld_i_e a = mk_e ~width:32 Opcode.Vpslld S.s_vpslld_i a
let vpslld_m a = mk ~width:32 Opcode.Vpslld S.s_vpslld_m a
let vpslld_im a = mk ~width:32 Opcode.Vpslld S.s_vpslld_im a
let vpslld_im_e a = mk_e ~width:32 Opcode.Vpslld S.s_vpslld_im a
let vpsllq a = mk ~width:32 Opcode.Vpsllq S.s_vpsllq a
let vpsllq_i a = mk ~width:32 Opcode.Vpsllq S.s_vpsllq_i a
let vpsllq_i_e a = mk_e ~width:32 Opcode.Vpsllq S.s_vpsllq_i a
let vpsllq_m a = mk ~width:32 Opcode.Vpsllq S.s_vpsllq_m a
let vpsllq_im a = mk ~width:32 Opcode.Vpsllq S.s_vpsllq_im a
let vpsllq_im_e a = mk_e ~width:32 Opcode.Vpsllq S.s_vpsllq_im a
let vpsrlw a = mk ~width:32 Opcode.Vpsrlw S.s_vpsrlw a
let vpsrlw_i a = mk ~width:32 Opcode.Vpsrlw S.s_vpsrlw_i a
let vpsrlw_i_e a = mk_e ~width:32 Opcode.Vpsrlw S.s_vpsrlw_i a
let vpsrlw_m a = mk ~width:32 Opcode.Vpsrlw S.s_vpsrlw_m a
let vpsrlw_im a = mk ~width:32 Opcode.Vpsrlw S.s_vpsrlw_im a
let vpsrlw_im_e a = mk_e ~width:32 Opcode.Vpsrlw S.s_vpsrlw_im a
let vpsrld a = mk ~width:32 Opcode.Vpsrld S.s_vpsrld a
let vpsrld_i a = mk ~width:32 Opcode.Vpsrld S.s_vpsrld_i a
let vpsrld_i_e a = mk_e ~width:32 Opcode.Vpsrld S.s_vpsrld_i a
let vpsrld_m a = mk ~width:32 Opcode.Vpsrld S.s_vpsrld_m a
let vpsrld_im a = mk ~width:32 Opcode.Vpsrld S.s_vpsrld_im a
let vpsrld_im_e a = mk_e ~width:32 Opcode.Vpsrld S.s_vpsrld_im a
let vpsrlq a = mk ~width:32 Opcode.Vpsrlq S.s_vpsrlq a
let vpsrlq_i a = mk ~width:32 Opcode.Vpsrlq S.s_vpsrlq_i a
let vpsrlq_i_e a = mk_e ~width:32 Opcode.Vpsrlq S.s_vpsrlq_i a
let vpsrlq_m a = mk ~width:32 Opcode.Vpsrlq S.s_vpsrlq_m a
let vpsrlq_im a = mk ~width:32 Opcode.Vpsrlq S.s_vpsrlq_im a
let vpsrlq_im_e a = mk_e ~width:32 Opcode.Vpsrlq S.s_vpsrlq_im a
let vpsraw a = mk ~width:32 Opcode.Vpsraw S.s_vpsraw a
let vpsraw_i a = mk ~width:32 Opcode.Vpsraw S.s_vpsraw_i a
let vpsraw_i_e a = mk_e ~width:32 Opcode.Vpsraw S.s_vpsraw_i a
let vpsraw_m a = mk ~width:32 Opcode.Vpsraw S.s_vpsraw_m a
let vpsraw_im a = mk ~width:32 Opcode.Vpsraw S.s_vpsraw_im a
let vpsraw_im_e a = mk_e ~width:32 Opcode.Vpsraw S.s_vpsraw_im a
let vpsrad a = mk ~width:32 Opcode.Vpsrad S.s_vpsrad a
let vpsrad_i a = mk ~width:32 Opcode.Vpsrad S.s_vpsrad_i a
let vpsrad_i_e a = mk_e ~width:32 Opcode.Vpsrad S.s_vpsrad_i a
let vpsrad_m a = mk ~width:32 Opcode.Vpsrad S.s_vpsrad_m a
let vpsrad_im a = mk ~width:32 Opcode.Vpsrad S.s_vpsrad_im a
let vpsrad_im_e a = mk_e ~width:32 Opcode.Vpsrad S.s_vpsrad_im a
let vpslldq_i a = mk ~width:32 Opcode.Vpslldq S.s_vpslldq_i a
let vpslldq_i_e a = mk_e ~width:32 Opcode.Vpslldq S.s_vpslldq_i a
let vpslldq_im a = mk ~width:32 Opcode.Vpslldq S.s_vpslldq_im a
let vpslldq_im_e a = mk_e ~width:32 Opcode.Vpslldq S.s_vpslldq_im a
let vpsrldq_i a = mk ~width:32 Opcode.Vpsrldq S.s_vpsrldq_i a
let vpsrldq_i_e a = mk_e ~width:32 Opcode.Vpsrldq S.s_vpsrldq_i a
let vpsrldq_im a = mk ~width:32 Opcode.Vpsrldq S.s_vpsrldq_im a
let vpsrldq_im_e a = mk_e ~width:32 Opcode.Vpsrldq S.s_vpsrldq_im a
let vmovdqa a = mk ~width:32 Opcode.Vmovdqa S.s_vmovdqa a
let vmovdqa_rm a = mk ~width:32 Opcode.Vmovdqa S.s_vmovdqa_rm a
let vmovdqa_m a = mk ~width:32 Opcode.Vmovdqa S.s_vmovdqa_m a
let vmovdqu a = mk ~width:32 Opcode.Vmovdqu S.s_vmovdqu a
let vmovdqu_rm a = mk ~width:32 Opcode.Vmovdqu S.s_vmovdqu_rm a
let vmovdqu_m a = mk ~width:32 Opcode.Vmovdqu S.s_vmovdqu_m a
let pinsrw_irq a = mk ~width:32 Opcode.Pinsrw S.s_pinsrw_irq a
let pinsrw_irq_e a = mk_e ~width:32 Opcode.Pinsrw S.s_pinsrw_irq a
let pinsrw_irx a = mk ~width:32 Opcode.Pinsrw S.s_pinsrw_irx a
let pinsrw_irx_e a = mk_e ~width:32 Opcode.Pinsrw S.s_pinsrw_irx a
let pinsrw_imq a = mk ~width:32 Opcode.Pinsrw S.s_pinsrw_imq a
let pinsrw_imq_e a = mk_e ~width:32 Opcode.Pinsrw S.s_pinsrw_imq a
let pinsrw_imx a = mk ~width:32 Opcode.Pinsrw S.s_pinsrw_imx a
let pinsrw_imx_e a = mk_e ~width:32 Opcode.Pinsrw S.s_pinsrw_imx a
let pextrw_iq a = mk ~width:32 Opcode.Pextrw S.s_pextrw_iq a
let pextrw_iq_e a = mk_e ~width:32 Opcode.Pextrw S.s_pextrw_iq a
let pextrw_i a = mk ~width:32 Opcode.Pextrw S.s_pextrw_i a
let pextrw_i_e a = mk_e ~width:32 Opcode.Pextrw S.s_pextrw_i a
let pextrw_irm a = mk ~width:32 Opcode.Pextrw S.s_pextrw_irm a
let pextrw_irm_e a = mk_e ~width:32 Opcode.Pextrw S.s_pextrw_irm a
let vpinsrw_i a = mk ~width:32 Opcode.Vpinsrw S.s_vpinsrw_i a
let vpinsrw_i_e a = mk_e ~width:32 Opcode.Vpinsrw S.s_vpinsrw_i a
let vpinsrw_im a = mk ~width:32 Opcode.Vpinsrw S.s_vpinsrw_im a
let vpinsrw_im_e a = mk_e ~width:32 Opcode.Vpinsrw S.s_vpinsrw_im a
let vpextrw_i a = mk ~width:32 Opcode.Vpextrw S.s_vpextrw_i a
let vpextrw_i_e a = mk_e ~width:32 Opcode.Vpextrw S.s_vpextrw_i a
let vpextrw_irm a = mk ~width:32 Opcode.Vpextrw S.s_vpextrw_irm a
let vpextrw_irm_e a = mk_e ~width:32 Opcode.Vpextrw S.s_vpextrw_irm a
let movmskps a = mk ~width:32 Opcode.Movmskps S.s_movmskps a
let movmskpd a = mk ~width:32 Opcode.Movmskpd S.s_movmskpd a
let pmovmskb_q a = mk ~width:32 Opcode.Pmovmskb S.s_pmovmskb_q a
let pmovmskb_x a = mk ~width:32 Opcode.Pmovmskb S.s_pmovmskb_x a
let vmovmskps a = mk ~width:32 Opcode.Vmovmskps S.s_vmovmskps a
let vmovmskpd a = mk ~width:32 Opcode.Vmovmskpd S.s_vmovmskpd a
let vpmovmskb a = mk ~width:32 Opcode.Vpmovmskb S.s_vpmovmskb a
let fldl_m a = mk ~width:32 Opcode.Fldl S.s_fldl_m a
let fstpl_m a = mk ~width:32 Opcode.Fstpl S.s_fstpl_m a
let fstps_m a = mk ~width:32 Opcode.Fstps S.s_fstps_m a
let flds a = mk ~width:32 Opcode.Flds S.s_flds a
let flds_m a = mk ~width:32 Opcode.Flds S.s_flds_m a
let fildll_m a = mk ~width:32 Opcode.Fildll S.s_fildll_m a
let fadds a = mk ~width:32 Opcode.Fadds S.s_fadds a
let fadds_m a = mk ~width:32 Opcode.Fadds S.s_fadds_m a
let fadd a = mk ~width:32 Opcode.Fadd S.s_fadd a
let fadd_r a = mk ~width:32 Opcode.Fadd S.s_fadd_r a
let fnstcw_m a = mk ~width:32 Opcode.Fnstcw S.s_fnstcw_m a
let fldcw_m a = mk ~width:32 Opcode.Fldcw S.s_fldcw_m a
let fistpll_m a = mk ~width:32 Opcode.Fistpll S.s_fistpll_m a
let fsubs a = mk ~width:32 Opcode.Fsubs S.s_fsubs a
let fsubs_m a = mk ~width:32 Opcode.Fsubs S.s_fsubs_m a
let fnstsw = mk ~width:32 Opcode.Fnstsw S.s_fnstsw
let fnstsw_m a = mk ~width:32 Opcode.Fnstsw S.s_fnstsw_m a
let sahf = mk ~width:32 Opcode.Sahf S.s_sahf
let cmovo a = mk (Opcode.Cmov Cc.O) S.s_cmovo a
let cmovo_m a = mk (Opcode.Cmov Cc.O) S.s_cmovo_m a
let jo_m32 a = mk ~width:32 (Opcode.Jcc Cc.O) S.s_jo_m32 a
let jo_m64 a = mk ~width:64 (Opcode.Jcc Cc.O) S.s_jo_m64 a
let seto a = mk ~width:8 (Opcode.Setcc Cc.O) S.s_seto a
let seto_m a = mk ~width:8 (Opcode.Setcc Cc.O) S.s_seto_m a
let cmovno a = mk (Opcode.Cmov Cc.No) S.s_cmovno a
let cmovno_m a = mk (Opcode.Cmov Cc.No) S.s_cmovno_m a
let jno_m32 a = mk ~width:32 (Opcode.Jcc Cc.No) S.s_jno_m32 a
let jno_m64 a = mk ~width:64 (Opcode.Jcc Cc.No) S.s_jno_m64 a
let setno a = mk ~width:8 (Opcode.Setcc Cc.No) S.s_setno a
let setno_m a = mk ~width:8 (Opcode.Setcc Cc.No) S.s_setno_m a
let cmovb a = mk (Opcode.Cmov Cc.B) S.s_cmovb a
let cmovb_m a = mk (Opcode.Cmov Cc.B) S.s_cmovb_m a
let jb_m32 a = mk ~width:32 (Opcode.Jcc Cc.B) S.s_jb_m32 a
let jb_m64 a = mk ~width:64 (Opcode.Jcc Cc.B) S.s_jb_m64 a
let setb a = mk ~width:8 (Opcode.Setcc Cc.B) S.s_setb a
let setb_m a = mk ~width:8 (Opcode.Setcc Cc.B) S.s_setb_m a
let cmovae a = mk (Opcode.Cmov Cc.Ae) S.s_cmovae a
let cmovae_m a = mk (Opcode.Cmov Cc.Ae) S.s_cmovae_m a
let jae_m32 a = mk ~width:32 (Opcode.Jcc Cc.Ae) S.s_jae_m32 a
let jae_m64 a = mk ~width:64 (Opcode.Jcc Cc.Ae) S.s_jae_m64 a
let setae a = mk ~width:8 (Opcode.Setcc Cc.Ae) S.s_setae a
let setae_m a = mk ~width:8 (Opcode.Setcc Cc.Ae) S.s_setae_m a
let cmove a = mk (Opcode.Cmov Cc.E) S.s_cmove a
let cmove_m a = mk (Opcode.Cmov Cc.E) S.s_cmove_m a
let je_m32 a = mk ~width:32 (Opcode.Jcc Cc.E) S.s_je_m32 a
let je_m64 a = mk ~width:64 (Opcode.Jcc Cc.E) S.s_je_m64 a
let sete a = mk ~width:8 (Opcode.Setcc Cc.E) S.s_sete a
let sete_m a = mk ~width:8 (Opcode.Setcc Cc.E) S.s_sete_m a
let cmovne a = mk (Opcode.Cmov Cc.Ne) S.s_cmovne a
let cmovne_m a = mk (Opcode.Cmov Cc.Ne) S.s_cmovne_m a
let jne_m32 a = mk ~width:32 (Opcode.Jcc Cc.Ne) S.s_jne_m32 a
let jne_m64 a = mk ~width:64 (Opcode.Jcc Cc.Ne) S.s_jne_m64 a
let setne a = mk ~width:8 (Opcode.Setcc Cc.Ne) S.s_setne a
let setne_m a = mk ~width:8 (Opcode.Setcc Cc.Ne) S.s_setne_m a
let cmovbe a = mk (Opcode.Cmov Cc.Be) S.s_cmovbe a
let cmovbe_m a = mk (Opcode.Cmov Cc.Be) S.s_cmovbe_m a
let jbe_m32 a = mk ~width:32 (Opcode.Jcc Cc.Be) S.s_jbe_m32 a
let jbe_m64 a = mk ~width:64 (Opcode.Jcc Cc.Be) S.s_jbe_m64 a
let setbe a = mk ~width:8 (Opcode.Setcc Cc.Be) S.s_setbe a
let setbe_m a = mk ~width:8 (Opcode.Setcc Cc.Be) S.s_setbe_m a
let cmova a = mk (Opcode.Cmov Cc.A) S.s_cmova a
let cmova_m a = mk (Opcode.Cmov Cc.A) S.s_cmova_m a
let ja_m32 a = mk ~width:32 (Opcode.Jcc Cc.A) S.s_ja_m32 a
let ja_m64 a = mk ~width:64 (Opcode.Jcc Cc.A) S.s_ja_m64 a
let seta a = mk ~width:8 (Opcode.Setcc Cc.A) S.s_seta a
let seta_m a = mk ~width:8 (Opcode.Setcc Cc.A) S.s_seta_m a
let cmovs a = mk (Opcode.Cmov Cc.S) S.s_cmovs a
let cmovs_m a = mk (Opcode.Cmov Cc.S) S.s_cmovs_m a
let js_m32 a = mk ~width:32 (Opcode.Jcc Cc.S) S.s_js_m32 a
let js_m64 a = mk ~width:64 (Opcode.Jcc Cc.S) S.s_js_m64 a
let sets a = mk ~width:8 (Opcode.Setcc Cc.S) S.s_sets a
let sets_m a = mk ~width:8 (Opcode.Setcc Cc.S) S.s_sets_m a
let cmovns a = mk (Opcode.Cmov Cc.Ns) S.s_cmovns a
let cmovns_m a = mk (Opcode.Cmov Cc.Ns) S.s_cmovns_m a
let jns_m32 a = mk ~width:32 (Opcode.Jcc Cc.Ns) S.s_jns_m32 a
let jns_m64 a = mk ~width:64 (Opcode.Jcc Cc.Ns) S.s_jns_m64 a
let setns a = mk ~width:8 (Opcode.Setcc Cc.Ns) S.s_setns a
let setns_m a = mk ~width:8 (Opcode.Setcc Cc.Ns) S.s_setns_m a
let cmovp a = mk (Opcode.Cmov Cc.P) S.s_cmovp a
let cmovp_m a = mk (Opcode.Cmov Cc.P) S.s_cmovp_m a
let jp_m32 a = mk ~width:32 (Opcode.Jcc Cc.P) S.s_jp_m32 a
let jp_m64 a = mk ~width:64 (Opcode.Jcc Cc.P) S.s_jp_m64 a
let setp a = mk ~width:8 (Opcode.Setcc Cc.P) S.s_setp a
let setp_m a = mk ~width:8 (Opcode.Setcc Cc.P) S.s_setp_m a
let cmovnp a = mk (Opcode.Cmov Cc.Np) S.s_cmovnp a
let cmovnp_m a = mk (Opcode.Cmov Cc.Np) S.s_cmovnp_m a
let jnp_m32 a = mk ~width:32 (Opcode.Jcc Cc.Np) S.s_jnp_m32 a
let jnp_m64 a = mk ~width:64 (Opcode.Jcc Cc.Np) S.s_jnp_m64 a
let setnp a = mk ~width:8 (Opcode.Setcc Cc.Np) S.s_setnp a
let setnp_m a = mk ~width:8 (Opcode.Setcc Cc.Np) S.s_setnp_m a
let cmovl a = mk (Opcode.Cmov Cc.L) S.s_cmovl a
let cmovl_m a = mk (Opcode.Cmov Cc.L) S.s_cmovl_m a
let jl_m32 a = mk ~width:32 (Opcode.Jcc Cc.L) S.s_jl_m32 a
let jl_m64 a = mk ~width:64 (Opcode.Jcc Cc.L) S.s_jl_m64 a
let setl a = mk ~width:8 (Opcode.Setcc Cc.L) S.s_setl a
let setl_m a = mk ~width:8 (Opcode.Setcc Cc.L) S.s_setl_m a
let cmovge a = mk (Opcode.Cmov Cc.Ge) S.s_cmovge a
let cmovge_m a = mk (Opcode.Cmov Cc.Ge) S.s_cmovge_m a
let jge_m32 a = mk ~width:32 (Opcode.Jcc Cc.Ge) S.s_jge_m32 a
let jge_m64 a = mk ~width:64 (Opcode.Jcc Cc.Ge) S.s_jge_m64 a
let setge a = mk ~width:8 (Opcode.Setcc Cc.Ge) S.s_setge a
let setge_m a = mk ~width:8 (Opcode.Setcc Cc.Ge) S.s_setge_m a
let cmovle a = mk (Opcode.Cmov Cc.Le) S.s_cmovle a
let cmovle_m a = mk (Opcode.Cmov Cc.Le) S.s_cmovle_m a
let jle_m32 a = mk ~width:32 (Opcode.Jcc Cc.Le) S.s_jle_m32 a
let jle_m64 a = mk ~width:64 (Opcode.Jcc Cc.Le) S.s_jle_m64 a
let setle a = mk ~width:8 (Opcode.Setcc Cc.Le) S.s_setle a
let setle_m a = mk ~width:8 (Opcode.Setcc Cc.Le) S.s_setle_m a
let cmovg a = mk (Opcode.Cmov Cc.G) S.s_cmovg a
let cmovg_m a = mk (Opcode.Cmov Cc.G) S.s_cmovg_m a
let jg_m32 a = mk ~width:32 (Opcode.Jcc Cc.G) S.s_jg_m32 a
let jg_m64 a = mk ~width:64 (Opcode.Jcc Cc.G) S.s_jg_m64 a
let setg a = mk ~width:8 (Opcode.Setcc Cc.G) S.s_setg a
let setg_m a = mk ~width:8 (Opcode.Setcc Cc.G) S.s_setg_m a
let loop_m32 a = mk ~width:32 (Opcode.Short_branch Short.Loop) S.s_loop_m32 a
let loop_m64 a = mk ~width:64 (Opcode.Short_branch Short.Loop) S.s_loop_m64 a
let loope_m32 a = mk ~width:32 (Opcode.Short_branch Short.Loope) S.s_loope_m32 a
let loope_m64 a = mk ~width:64 (Opcode.Short_branch Short.Loope) S.s_loope_m64 a
let loopne_m32 a = mk ~width:32 (Opcode.Short_branch Short.Loopne) S.s_loopne_m32 a
let loopne_m64 a = mk ~width:64 (Opcode.Short_branch Short.Loopne) S.s_loopne_m64 a
let jcxz a = mk ~width:32 (Opcode.Short_branch Short.Jcxz) S.s_jcxz a
let jecxz_m32 a = mk ~width:32 (Opcode.Short_branch Short.Jecxz) S.s_jecxz_m32 a
let jecxz_m64 a = mk ~width:64 (Opcode.Short_branch Short.Jecxz) S.s_jecxz_m64 a
let jrcxz a = mk ~width:64 (Opcode.Short_branch Short.Jrcxz) S.s_jrcxz a
let movzb a = mk (Opcode.Movzx { src_width = 8 }) S.s_movzb a
let movzb_m a = mk (Opcode.Movzx { src_width = 8 }) S.s_movzb_m a
let movzw a = mk (Opcode.Movzx { src_width = 16 }) S.s_movzw a
let movzw_m a = mk (Opcode.Movzx { src_width = 16 }) S.s_movzw_m a
let movsb a = mk (Opcode.Movsx { src_width = 8 }) S.s_movsb a
let movsb_m a = mk (Opcode.Movsx { src_width = 8 }) S.s_movsb_m a
let movsw a = mk (Opcode.Movsx { src_width = 16 }) S.s_movsw a
let movsw_m a = mk (Opcode.Movsx { src_width = 16 }) S.s_movsw_m a
let movsl a = mk ~width:64 (Opcode.Movsx { src_width = 32 }) S.s_movsl a
let movsl_m a = mk ~width:64 (Opcode.Movsx { src_width = 32 }) S.s_movsl_m a
