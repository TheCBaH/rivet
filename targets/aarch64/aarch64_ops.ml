(* One definition per hand-written AArch64 (A64) opcode: its spelling, the fields of the encoding
   classes it belongs to, whether its lowering stays code, and the operand lists it takes. The
   same operand lists are the typed builders ({!Builders}): one per operand shape, named by the
   mnemonic with a suffix for the shape ([add x0 x1 x2], [add_i sp sp 16],
   [add_sh x0 x1 x2 (lsl_ 3)], [ldr_post x0 sp 8]). Register positions are typed by what they
   accept: a width ([x] or [w], the same for every operand that shares it), and whether register
   31 is the stack pointer or the zero register there ({!Aarch64_types.View}). A scalar
   floating-point operand is a [d] or an [s] register and a vector one carries its arrangement, so
   [add x0 w1 x2], [fadd d0 s1 d2] and [fadd_v v0.4s v1.2d v2.4s] do not compile. *)

open Foundation
open Aarch64_types
open View

module Kind = struct
  type t = X | W | D | S | Q | Imm | Fimm | Mem | Sym | Sh | Vec | Vlane | Vlist | Vlist_lane

  let name = function
    | X -> "x"
    | W -> "w"
    | D -> "d"
    | S -> "s"
    | Q -> "q"
    | Imm -> "imm"
    | Fimm -> "fimm"
    | Mem -> "mem"
    | Sym -> "sym"
    | Sh -> "sh"
    | Vec -> "vec"
    | Vlane -> "vlane"
    | Vlist -> "vlist"
    | Vlist_lane -> "vlist_lane"
end

(* A shift ([lsl #3]) or extend ([uxtw #2]) operand. *)
module Sx : sig
  type sop = private Shift.t
  type eop = private Shift.t

  val by_lsl : int -> sop
  val by_lsr : int -> sop
  val by_asr : int -> sop
  val by_ror : int -> sop
  val extend : Extend.t -> int -> eop
end = struct
  type sop = Shift.t
  type eop = Shift.t

  let by_lsl amount = { Shift.kind = Shift.Lsl; amount }
  let by_lsr amount = { Shift.kind = Shift.Lsr; amount }
  let by_asr amount = { Shift.kind = Shift.Asr; amount }
  let by_ror amount = { Shift.kind = Shift.Ror; amount }
  let extend e amount = { Shift.kind = Shift.Ext e; amount }
end

open Sx

(* The values a sample takes: the width of the registers that share one, the size of the
   floating-point ones, the arrangement of the vector ones and the lane type that goes with
   them, and the number every integer operand is. *)
type sample = { width : int; double : bool; arr : Varr.t; lane : Lane.t; imm : int }

(* A builder's operands, in the text's order. ['w] is the width the register operands that share
   it have, ['s] the size of the floating-point ones, ['a] the arrangement of the vector ones;
   ['f] is the builder's type and ['r] its result. *)
module Shape = struct
  type ('w, 's, 'a, 'f, 'r) t =
    | Done : ('w, 's, 'a, 'r, 'r) t
    | Unit : ('w, 's, 'a, unit -> 'r, 'r) t
    | Z : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, ('w, [< gp | zr ]) greg -> 'f, 'r) t
    | Sp : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, ('w, [< gp | sp ]) greg -> 'f, 'r) t
    | Any : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, ('w, [< gp | zr | sp ]) greg -> 'f, 'r) t
    | Xz : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, (x, [< gp | zr ]) greg -> 'f, 'r) t
    | Xsp : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, (x, [< gp | sp ]) greg -> 'f, 'r) t
    | Wz : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, (w, [< gp | zr ]) greg -> 'f, 'r) t
    | Zv : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, ('v, [< gp | zr ]) greg -> 'f, 'r) t
    | Zw : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, ('v, [< gp | zr ]) greg -> 'f, 'r) t
    | Fr : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 's freg -> 'f, 'r) t
    | D : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, d freg -> 'f, 'r) t
    | S : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, s freg -> 'f, 'r) t
    | Q : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, qreg -> 'f, 'r) t
    | Vfp : ('w, 's, ([< `S2 | `S4 | `D2 ] as 'a), 'f, 'r) t -> ('w, 's, 'a, 'a vec -> 'f, 'r) t
    | Vb : ('w, 's, ([< `B8 | `B16 ] as 'a), 'f, 'r) t -> ('w, 's, 'a, 'a vec -> 'f, 'r) t
    | Vd2 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, [ `D2 ] vec -> 'f, 'r) t
    | Vs2 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, [ `S2 ] vec -> 'f, 'r) t
    | Lane : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, lane -> 'f, 'r) t
    | Vlist : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, vlist -> 'f, 'r) t
    | Vlist_lane : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, vlist_lane -> 'f, 'r) t
    | Imm : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int -> 'f, 'r) t
    | Imm64 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int64 -> 'f, 'r) t
    | Lsl_amt : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, int -> 'f, 'r) t
    | Lsl12 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Fimm : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, float -> 'f, 'r) t
    | Fzero : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Cond : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Cond.t -> 'f, 'r) t
    | Fix_sym : string * ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, 'f, 'r) t
    | Sym : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Asm_core.Expr.t -> 'f, 'r) t
    | Lo12 : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Asm_core.Expr.t -> 'f, 'r) t
    | Mem : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, Mem.t -> 'f, 'r) t
    | Post : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, (x, [< gp | sp ]) greg -> int -> 'f, 'r) t
    | Sop : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, sop -> 'f, 'r) t
    | Eop : ('w, 's, 'a, 'f, 'r) t -> ('w, 's, 'a, eop -> 'f, 'r) t

  type packed = P : ('w, 's, 'a, 'f, Instruction.t) t -> packed

  (* What one position of a shape contributes: the dimensions of the sample it reads, the
     arrangements it admits, its kinds and the rest of the shape. *)
  type node = {
    reads_w : bool;
    reads_d : bool;
    arrs : Varr.t list option;
    lanes : bool;
    kinds : sample -> Kind.t list;
    next : packed option;
  }

  let leaf =
    {
      reads_w = false;
      reads_d = false;
      arrs = None;
      lanes = false;
      kinds = (fun _ -> []);
      next = None;
    }

  let reg sm = if sm.width = 64 then Kind.X else Kind.W

  let step ?(w = false) ?(d = false) ?arrs ?(lanes = false) kinds s =
    { reads_w = w; reads_d = d; arrs; lanes; kinds; next = Some (P s) }

  let one k _ = [ k ]

  let node : type w s a f. (w, s, a, f, Instruction.t) t -> node = function
    | Done | Unit -> leaf
    | Z s -> step ~w:true (fun sm -> [ reg sm ]) s
    | Sp s -> step ~w:true (fun sm -> [ reg sm ]) s
    | Any s -> step ~w:true (fun sm -> [ reg sm ]) s
    | Zv s -> step ~w:true (fun sm -> [ reg sm ]) s
    | Xz s -> step (one Kind.X) s
    | Xsp s -> step (one Kind.X) s
    | Wz s -> step (one Kind.W) s
    | Zw s -> step (one Kind.W) s
    | Fr s -> step ~d:true (fun sm -> [ (if sm.double then Kind.D else Kind.S) ]) s
    | D s -> step (one Kind.D) s
    | S s -> step (one Kind.S) s
    | Q s -> step (one Kind.Q) s
    | Vfp s -> step ~arrs:Varr.[ S4; S2; D2 ] (one Kind.Vec) s
    | Vb s -> step ~arrs:Varr.[ B16; B8 ] (one Kind.Vec) s
    | Vd2 s -> step (one Kind.Vec) s
    | Vs2 s -> step (one Kind.Vec) s
    | Lane s -> step ~lanes:true (one Kind.Vlane) s
    | Vlist s -> step (one Kind.Vlist) s
    | Vlist_lane s -> step (one Kind.Vlist_lane) s
    | Imm s -> step (one Kind.Imm) s
    | Imm64 s -> step (one Kind.Imm) s
    | Lsl_amt s -> step (one Kind.Sh) s
    | Lsl12 s -> step (one Kind.Sh) s
    | Sop s -> step (one Kind.Sh) s
    | Eop s -> step (one Kind.Sh) s
    | Fimm s -> step (one Kind.Fimm) s
    | Fzero s -> step (one Kind.Fimm) s
    | Cond s -> step (one Kind.Sym) s
    | Sym s -> step (one Kind.Sym) s
    | Lo12 s -> step (one Kind.Sym) s
    | Fix_sym (_, s) -> step (one Kind.Sym) s
    | Mem s -> step (one Kind.Mem) s
    | Post s -> step (fun _ -> [ Kind.Mem; Kind.Imm ]) s

  let rec fold (P s) =
    let n = node s in
    match n.next with
    | None -> (n.reads_w, n.reads_d, n.arrs, n.lanes, n.kinds)
    | Some rest ->
        let w, d, a, l, k = fold rest in
        ( n.reads_w || w,
          n.reads_d || d,
          (match n.arrs with Some _ as x -> x | None -> a),
          n.lanes || l,
          fun sm -> n.kinds sm @ k sm )

  let int n = Operand.Imm (Bigint.of_int n)
  let reg_op r = Operand.Reg (r : (_, _) greg :> Reg.t)

  let mem0 base =
    Operand.Mem
      { Mem.base : (x, _) greg :> Reg.t; offset = Disp.zero; writeback = false; pre = true }

  let vec_op (v : _ vec) =
    let n, a = (v : _ vec :> int * Varr.t) in
    Operand.Vec (n, a)

  let rec build : type w s a f r. (w, s, a, f, r) t -> (Operand.t list -> r) -> f =
   fun s k ->
    let next s o = build s (fun ops -> k (o :: ops)) in
    match s with
    | Done -> k []
    | Unit -> fun () -> k []
    | Z s -> fun r -> next s (reg_op r)
    | Sp s -> fun r -> next s (reg_op r)
    | Any s -> fun r -> next s (reg_op r)
    | Xz s -> fun r -> next s (reg_op r)
    | Xsp s -> fun r -> next s (reg_op r)
    | Wz s -> fun r -> next s (reg_op r)
    | Zv s -> fun r -> next s (reg_op r)
    | Zw s -> fun r -> next s (reg_op r)
    | Fr s -> fun r -> next s (Operand.Freg (r : _ freg :> Freg.t))
    | D s -> fun r -> next s (Operand.Freg (r : _ freg :> Freg.t))
    | S s -> fun r -> next s (Operand.Freg (r : _ freg :> Freg.t))
    | Q s -> fun r -> next s (Operand.Qreg (r : qreg :> int))
    | Vfp s -> fun v -> next s (vec_op v)
    | Vb s -> fun v -> next s (vec_op v)
    | Vd2 s -> fun v -> next s (vec_op v)
    | Vs2 s -> fun v -> next s (vec_op v)
    | Lane s ->
        fun l ->
          next s
            (let n, ln, i = (l : lane :> int * Lane.t * int) in
             Operand.Vlane (n, ln, i))
    | Vlist s ->
        fun l ->
          next s
            (let n, a = (l : vlist :> int * Varr.t) in
             Operand.Vlist (n, a))
    | Vlist_lane s ->
        fun l ->
          next s
            (let n, ln, i = (l : vlist_lane :> int * Lane.t * int) in
             Operand.Vlist_lane (n, ln, i))
    | Imm s -> fun n -> next s (int n)
    | Imm64 s -> fun n -> next s (Operand.Imm (Bigint.of_int64 n))
    | Lsl_amt s -> fun n -> next s (Operand.Shift { Shift.kind = Shift.Lsl; amount = n })
    | Lsl12 s -> next s (Operand.Shift { Shift.kind = Shift.Lsl; amount = 12 })
    | Fimm s -> fun v -> next s (Operand.Fimm v)
    | Fzero s -> next s (Operand.Fimm 0.0)
    | Cond s -> fun c -> next s (Operand.Sym (Asm_core.Expr.Symbol (Cond.name c)))
    | Fix_sym (n, s) -> next s (Operand.Sym (Asm_core.Expr.Symbol n))
    | Sym s -> fun e -> next s (Operand.Sym e)
    | Lo12 s -> fun e -> next s (Operand.Sym (Asm_core.Expr.Modifier ("lo12", e)))
    | Mem s -> fun m -> next s (Operand.Mem m)
    | Post s -> fun base n -> build s (fun ops -> k (mem0 base :: int n :: ops))
    | Sop s -> fun o -> next s (Operand.Shift (o :> Shift.t))
    | Eop s -> fun o -> next s (Operand.Shift (o :> Shift.t))

  (* [f] applied to sample operands: the register at position [i] is number [1 + i], every
     integer is [sm.imm] (a [movz]'s shift is 16), a memory operand is [\[x(1+i)\]], a symbol is
     [l], a shift is [lsl #1] and an extend [uxtw #1]. *)
  let sample : type w s a f r. sample -> (w, s, a, f, r) t -> f -> r =
   fun sm s f ->
    let rec go : type w s a f r. int -> (w, s, a, f, r) t -> f -> r =
     fun i s f ->
      let n = 1 + i in
      let g width = { Reg.num = n; width; is_sp = false } in
      match s with
      | Done -> f
      | Unit -> f ()
      | Z s -> go (i + 1) s (f (Unsafe.greg (g sm.width)))
      | Sp s -> go (i + 1) s (f (Unsafe.greg (g sm.width)))
      | Any s -> go (i + 1) s (f (Unsafe.greg (g sm.width)))
      | Zv s -> go (i + 1) s (f (Unsafe.greg (g sm.width)))
      | Xz s -> go (i + 1) s (f (Unsafe.greg (g 64)))
      | Xsp s -> go (i + 1) s (f (Unsafe.greg (g 64)))
      | Wz s -> go (i + 1) s (f (Unsafe.greg (g 32)))
      | Zw s -> go (i + 1) s (f (Unsafe.greg (g 32)))
      | Fr s -> go (i + 1) s (f (Unsafe.freg { Freg.num = n; double = sm.double }))
      | D s -> go (i + 1) s (f (Unsafe.freg { Freg.num = n; double = true }))
      | S s -> go (i + 1) s (f (Unsafe.freg { Freg.num = n; double = false }))
      | Q s -> go (i + 1) s (f (Unsafe.qreg n))
      | Vfp s -> go (i + 1) s (f (Unsafe.vec n sm.arr))
      | Vb s -> go (i + 1) s (f (Unsafe.vec n sm.arr))
      | Vd2 s -> go (i + 1) s (f (Unsafe.vec n Varr.D2))
      | Vs2 s -> go (i + 1) s (f (Unsafe.vec n Varr.S2))
      | Lane s -> go (i + 1) s (f (Unsafe.lane n sm.lane 1))
      | Vlist s -> go (i + 1) s (f (Unsafe.vlist n sm.arr))
      | Vlist_lane s -> go (i + 1) s (f (Unsafe.vlist_lane n sm.lane 1))
      | Imm s -> go (i + 1) s (f sm.imm)
      | Imm64 s -> go (i + 1) s (f (Int64.of_int sm.imm))
      | Lsl_amt s -> go (i + 1) s (f 16)
      | Lsl12 s -> go (i + 1) s f
      | Fimm s -> go (i + 1) s (f 1.0)
      | Fzero s -> go (i + 1) s f
      | Cond s -> go (i + 1) s (f Cond.Ne)
      | Fix_sym (_, s) -> go (i + 1) s f
      | Sym s -> go (i + 1) s (f (Asm_core.Expr.Symbol "l"))
      | Lo12 s -> go (i + 1) s (f (Asm_core.Expr.Symbol "l"))
      | Mem s ->
          go (i + 1) s (f { Mem.base = g 64; offset = Disp.zero; writeback = false; pre = true })
      | Post s -> go (i + 1) s (f (Unsafe.greg (g 64)) sm.imm)
      | Sop s -> go (i + 1) s (f (by_lsl 1))
      | Eop s -> go (i + 1) s (f (extend Extend.Uxtw 1))
    in
    go 0 s f
end

(* The fields an encoding class fixes. An opcode belongs to the class of each of its forms, so one
   with both a scalar and a vector form ([fadd], [and]) lists two. *)
module Enc = struct
  type t =
    | Addsub of { sub : bool; s : bool }  (** add, sub, cmp, cmn *)
    | Mov
    | Wide of { opc : int }  (** movn 0, movz 2, movk 3 *)
    | Cbz of { nz : bool }
    | Sxtw
    | Uxtw
    | Div of { signed : bool }
    | Madd of { sub : bool }
    | Mul
    | Cset
    | Csel
    | Shiftv of { code : int }  (** lsl, lsr, asr, ror by a register: the [op2] field *)
    | Shift_imm
    | Bitfield of { signed : bool; lsl_alias : bool }  (** sbfx, ubfx, sbfiz, ubfiz *)
    | Dp1 of { w32 : int option; w64 : int option }  (** the opcode field by register width *)
    | Ldst_pair of { load : bool }
    | Ldst of { load : bool; size : int option }  (** [None]: the register's width *)
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
    | Fp1 of { opcode : int }  (** fneg, fsqrt, frintz *)
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

type variant = { kinds : Kind.t list; sample : imm:int -> Instruction.t }

type entry = {
  op : Opcode.t;
  name : string;
  enc : Enc.t list;
  ext : string list;  (** the components that publish it: none, the whole target is the base *)
  lower : bool;  (** lowering stays code *)
  mutable shapes : variant list list;  (** per builder shape, a variant for each sample dimension *)
  also : Kind.t list list;  (** other operand lists lowering takes, without a builder *)
}

let registry : entry list ref = ref []

let variants op p =
  let w, d, arrs, lanes, kinds = Shape.fold p in
  let widths = if w then [ 64; 32 ] else [ 64 ] in
  let doubles = if d then [ true; false ] else [ false ] in
  let vector = arrs <> None in
  let arrs = match arrs with Some l -> l | None -> [ Varr.S4 ] in
  let lane arr double =
    if not lanes then Lane.S
    else if vector then match arr with Varr.D2 -> Lane.D | _ -> Lane.S
    else if double then Lane.D
    else Lane.S
  in
  let (Shape.P s) = p in
  List.concat_map
    (fun width ->
      List.concat_map
        (fun double ->
          List.filter_map
            (fun arr ->
              (* a vector built from a general register takes the lane's width: [dup v0.2d, x1] *)
              let width' =
                if vector && w then match arr with Varr.D2 -> 64 | _ -> 32 else width
              in
              if vector && w && width' <> width then None
              else
                let sm0 = { width = width'; double; arr; lane = lane arr double; imm = 8 } in
                Some
                  {
                    kinds = kinds sm0;
                    sample =
                      (fun ~imm ->
                        let sm = { sm0 with imm } in
                        Shape.sample sm s (Shape.build s (Instruction.mk op)));
                  })
            arrs)
        doubles)
    widths

(* Registers the opcode and the operand lists of its builders. *)
let entry ?(ext = []) ?(lower = false) ?(also = []) ~name ~enc op shapes =
  let e = { op; name; enc; ext; lower; shapes = List.map (variants op) shapes; also } in
  registry := e :: !registry

let bld op s = Shape.build s (Instruction.mk op)

(* {1 The builders}

   Operand lists are values (so that their width, size and arrangement stay polymorphic), a
   builder is the function that applies one; the entries below list the same shapes. *)
module Builders = struct
  open Shape

  module Sh = struct
    let zz = Z (Z Done)
    let zzz = Z (Z (Z Done))
    let zzzz = Z (Z (Z (Z Done)))
    let zz_sh = Z (Z (Sop Done))
    let zzz_sh = Z (Z (Z (Sop Done)))
    let zzz_cond = Z (Z (Z (Cond Done)))
    let z_cond = Z (Cond Done)
    let spsp_ext = Sp (Sp (Zw (Eop Done)))
    let spsp_i = Sp (Sp (Imm Done))
    let spsp_i12 = Sp (Sp (Imm (Lsl12 Done)))
    let spsp_lo12 = Sp (Sp (Lo12 Done))
    let sp_i = Sp (Imm Done)
    let sp_i12 = Sp (Imm (Lsl12 Done))
    let anyany = Any (Any Done)
    let z_i64 = Z (Imm64 Done)
    let z_i = Z (Imm Done)
    let z_i_lsl = Z (Imm (Lsl_amt Done))
    let z_sym = Z (Sym Done)
    let zz_i = Z (Z (Imm Done))
    let zzii = Z (Z (Imm (Imm Done)))
    let spz_i64 = Sp (Z (Imm64 Done))
    let z_mem = Z (Mem Done)
    let z_post = Z (Post Done)
    let wz_mem = Wz (Mem Done)
    let wz_post = Wz (Post Done)
    let xz_mem = Xz (Mem Done)
    let zz_mem = Z (Z (Mem Done))
    let zz_post = Z (Z (Post Done))
    let fr_mem = Fr (Mem Done)
    let q_mem = Q (Mem Done)
    let frfr = Fr (Fr Done)
    let frfrfr = Fr (Fr (Fr Done))
    let frfrfrfr = Fr (Fr (Fr (Fr Done)))
    let frfrfr_cond = Fr (Fr (Fr (Cond Done)))
    let fr_fimm = Fr (Fimm Done)
    let fr_fzero = Fr (Fzero Done)
    let fr_zv = Fr (Zv Done)
    let zv_fr = Zv (Fr Done)
    let vvv_fp = Vfp (Vfp (Vfp Done))
    let vv_fp = Vfp (Vfp Done)
    let vvv_b = Vb (Vb (Vb Done))
    let vv_b = Vb (Vb Done)
    let vfp_lane = Vfp (Lane Done)
    let vfp_zv = Vfp (Zv Done)
    let fr_lane = Fr (Lane Done)
    let lane_lane = Lane (Lane Done)
    let vl_mem = Vlist_lane (Mem Done)
    let vlist_mem = Vlist (Mem Done)
    let xz_sym = Xz (Sym Done)
    let xz_xz = Xz (Xz Done)
    let xz_wz = Xz (Wz Done)
    let xz = Xz Done
    let sym = Sym Done
    let imm = Imm Done
    let d_xz = D (Xz Done)
    let s_wz = S (Wz Done)
    let xz_d = Xz (D Done)
    let wz_s = Wz (S Done)
    let s_d = S (D Done)
    let d_s = D (S Done)
    let vd2_vs2 = Vd2 (Vs2 Done)
    let vs2_vd2 = Vs2 (Vd2 Done)
    let xz_sysreg n = Xz (Fix_sym (n, Done))
    let sysreg_xz n = Fix_sym (n, Xz Done)
  end

  open Sh

  (* {2 Arithmetic} *)

  let add a = bld Opcode.Add zzz a
  let add_sh a = bld Opcode.Add zzz_sh a
  let add_ext a = bld Opcode.Add spsp_ext a
  let add_i a = bld Opcode.Add spsp_i a
  let add_i_lsl12 a = bld Opcode.Add spsp_i12 a
  let add_lo12 a = bld Opcode.Add spsp_lo12 a
  let sub a = bld Opcode.Sub zzz a
  let sub_sh a = bld Opcode.Sub zzz_sh a
  let sub_ext a = bld Opcode.Sub spsp_ext a
  let sub_i a = bld Opcode.Sub spsp_i a
  let sub_i_lsl12 a = bld Opcode.Sub spsp_i12 a
  let cmp a = bld Opcode.Cmp zz a
  let cmp_i a = bld Opcode.Cmp sp_i a
  let cmp_i_lsl12 a = bld Opcode.Cmp sp_i12 a
  let cmn_i a = bld Opcode.Cmn sp_i a
  let cmn_i_lsl12 a = bld Opcode.Cmn sp_i12 a
  let mov a = bld Opcode.Mov anyany a
  let mov_i a = bld Opcode.Mov z_i64 a
  let movz a = bld Opcode.Movz z_i a
  let movz_lsl a = bld Opcode.Movz z_i_lsl a
  let movn a = bld Opcode.Movn z_i a
  let movn_lsl a = bld Opcode.Movn z_i_lsl a
  let movk a = bld Opcode.Movk z_i a
  let movk_lsl a = bld Opcode.Movk z_i_lsl a
  let sxtw a = bld Opcode.Sxtw xz_wz a
  let uxtw a = bld Opcode.Uxtw xz_wz a
  let udiv a = bld Opcode.Udiv zzz a
  let sdiv a = bld Opcode.Sdiv zzz a
  let madd a = bld Opcode.Madd zzzz a
  let msub a = bld Opcode.Msub zzzz a
  let mul a = bld Opcode.Mul zzz a
  let cset a = bld Opcode.Cset z_cond a
  let csel a = bld Opcode.Csel zzz_cond a
  let lsl_ a = bld Opcode.Lsl zzz a
  let lsl_i a = bld Opcode.Lsl zz_i a
  let lsr_ a = bld Opcode.Lsr zzz a
  let lsr_i a = bld Opcode.Lsr zz_i a
  let asr_ a = bld Opcode.Asr zzz a
  let asr_i a = bld Opcode.Asr zz_i a
  let ror_ a = bld Opcode.Ror zzz a
  let ubfx a = bld Opcode.Ubfx zzii a
  let ubfiz a = bld Opcode.Ubfiz zzii a
  let sbfx a = bld Opcode.Sbfx zzii a
  let sbfiz a = bld Opcode.Sbfiz zzii a
  let rbit a = bld Opcode.Rbit zz a
  let rev16 a = bld Opcode.Rev16 zz a
  let rev32 a = bld Opcode.Rev32 xz_xz a
  let rev a = bld Opcode.Rev zz a
  let clz a = bld Opcode.Clz zz a
  let cls a = bld Opcode.Cls zz a
  let and_ a = bld Opcode.And zzz a
  let and_sh a = bld Opcode.And zzz_sh a
  let and_i a = bld Opcode.And spz_i64 a
  let orr a = bld Opcode.Orr zzz a
  let orr_sh a = bld Opcode.Orr zzz_sh a
  let orr_i a = bld Opcode.Orr spz_i64 a
  let eor a = bld Opcode.Eor zzz a
  let eor_sh a = bld Opcode.Eor zzz_sh a
  let eor_i a = bld Opcode.Eor spz_i64 a
  let bic a = bld Opcode.Bic zzz a
  let bic_sh a = bld Opcode.Bic zzz_sh a
  let tst_i a = bld Opcode.Tst z_i64 a

  (* {2 Loads and stores} *)

  let ldr a = bld Opcode.Ldr z_mem a
  let ldr_post a = bld Opcode.Ldr z_post a
  let ldr_f a = bld Opcode.Ldr fr_mem a
  let ldr_q a = bld Opcode.Ldr q_mem a
  let str a = bld Opcode.Str z_mem a
  let str_post a = bld Opcode.Str z_post a
  let str_f a = bld Opcode.Str fr_mem a
  let str_q a = bld Opcode.Str q_mem a
  let ldrb a = bld Opcode.Ldrb wz_mem a
  let ldrb_post a = bld Opcode.Ldrb wz_post a
  let ldrh a = bld Opcode.Ldrh wz_mem a
  let ldrh_post a = bld Opcode.Ldrh wz_post a
  let strb a = bld Opcode.Strb wz_mem a
  let strb_post a = bld Opcode.Strb wz_post a
  let strh a = bld Opcode.Strh wz_mem a
  let strh_post a = bld Opcode.Strh wz_post a
  let ldrsb a = bld Opcode.Ldrsb z_mem a
  let ldrsh a = bld Opcode.Ldrsh z_mem a
  let ldrsw a = bld Opcode.Ldrsw xz_mem a
  let ldp a = bld Opcode.Ldp zz_mem a
  let ldp_post a = bld Opcode.Ldp zz_post a
  let stp a = bld Opcode.Stp zz_mem a
  let stp_post a = bld Opcode.Stp zz_post a

  (* {2 Branches and system} *)

  let b a = bld Opcode.B sym a
  let bl a = bld Opcode.Bl sym a
  let b_cond c a = bld (Opcode.Bcond c) sym a
  let ret a = bld Opcode.Ret Unit a
  let ret_r a = bld Opcode.Ret xz a
  let br a = bld Opcode.Br xz a
  let cbz a = bld Opcode.Cbz z_sym a
  let cbnz a = bld Opcode.Cbnz z_sym a
  let adrp a = bld Opcode.Adrp xz_sym a
  let adr a = bld Opcode.Adr xz_sym a
  let udf a = bld Opcode.Udf imm a
  let mrs_fpcr a = bld Opcode.Mrs (xz_sysreg "fpcr") a
  let mrs_nzcv a = bld Opcode.Mrs (xz_sysreg "nzcv") a
  let msr_fpcr a = bld Opcode.Msr (sysreg_xz "fpcr") a
  let msr_nzcv a = bld Opcode.Msr (sysreg_xz "nzcv") a

  (* {2 Floating point} A scalar operand is a {!View.d} or {!View.s} register; [_v] is the
     vector form, whose operands share an arrangement. *)

  let fmov a = bld Opcode.Fmov frfr a
  let fmov_i a = bld Opcode.Fmov fr_fimm a
  let fmov_d_x a = bld Opcode.Fmov d_xz a
  let fmov_s_w a = bld Opcode.Fmov s_wz a
  let fmov_x_d a = bld Opcode.Fmov xz_d a
  let fmov_w_s a = bld Opcode.Fmov wz_s a
  let fcmp a = bld Opcode.Fcmp frfr a
  let fcmp_z a = bld Opcode.Fcmp fr_fzero a
  let fadd a = bld Opcode.Fadd frfrfr a
  let fsub a = bld Opcode.Fsub frfrfr a
  let fmul a = bld Opcode.Fmul frfrfr a
  let fdiv a = bld Opcode.Fdiv frfrfr a
  let fmax a = bld Opcode.Fmax frfrfr a
  let fadd_v a = bld Opcode.Fadd vvv_fp a
  let fsub_v a = bld Opcode.Fsub vvv_fp a
  let fmul_v a = bld Opcode.Fmul vvv_fp a
  let fdiv_v a = bld Opcode.Fdiv vvv_fp a
  let fmax_v a = bld Opcode.Fmax vvv_fp a
  let fmla_v a = bld Opcode.Fmla vvv_fp a
  let fcmeq_v a = bld Opcode.Fcmeq vvv_fp a
  let fcmge_v a = bld Opcode.Fcmge vvv_fp a
  let fcmgt_v a = bld Opcode.Fcmgt vvv_fp a
  let fcsel a = bld Opcode.Fcsel frfrfr_cond a
  let scvtf a = bld Opcode.Scvtf fr_zv a
  let ucvtf a = bld Opcode.Ucvtf fr_zv a
  let fneg a = bld Opcode.Fneg frfr a
  let fsqrt a = bld Opcode.Fsqrt frfr a
  let frintz a = bld Opcode.Frintz frfr a
  let fneg_v a = bld Opcode.Fneg vv_fp a
  let fsqrt_v a = bld Opcode.Fsqrt vv_fp a
  let frintz_v a = bld Opcode.Frintz vv_fp a
  let fmadd a = bld Opcode.Fmadd frfrfrfr a
  let fcvt_s_d a = bld Opcode.Fcvt s_d a
  let fcvt_d_s a = bld Opcode.Fcvt d_s a
  let fcvtzs a = bld Opcode.Fcvtzs zv_fr a
  let fcvtzu a = bld Opcode.Fcvtzu zv_fr a

  (* {2 Advanced SIMD} *)

  let fcvtl a = bld Opcode.Fcvtl vd2_vs2 a
  let fcvtn a = bld Opcode.Fcvtn vs2_vd2 a
  let and_v a = bld Opcode.And vvv_b a
  let orr_v a = bld Opcode.Orr vvv_b a
  let eor_v a = bld Opcode.Eor vvv_b a
  let bsl_v a = bld Opcode.Bsl vvv_b a
  let bit_v a = bld Opcode.Bit vvv_b a
  let not_v a = bld Opcode.Not vv_b a
  let mov_v a = bld Opcode.Mov vv_b a
  let mov_lane a = bld Opcode.Mov fr_lane a
  let mov_lane_lane a = bld Opcode.Mov lane_lane a
  let ins a = bld Opcode.Ins lane_lane a
  let dup_lane a = bld Opcode.Dup vfp_lane a
  let dup_gpr a = bld Opcode.Dup vfp_zv a
  let dup_scalar a = bld Opcode.Dup fr_lane a
  let ld1_lane a = bld Opcode.Ld1 vl_mem a
  let st1_lane a = bld Opcode.St1 vl_mem a
  let ld1r a = bld Opcode.Ld1r vlist_mem a

  (* the conditional branches, from the conditions *)
  let b_eq a = b_cond Cond.Eq a
  let b_ne a = b_cond Cond.Ne a
  let b_cs a = b_cond Cond.Cs a
  let b_cc a = b_cond Cond.Cc a
  let b_mi a = b_cond Cond.Mi a
  let b_pl a = b_cond Cond.Pl a
  let b_vs a = b_cond Cond.Vs a
  let b_vc a = b_cond Cond.Vc a
  let b_hi a = b_cond Cond.Hi a
  let b_ls a = b_cond Cond.Ls a
  let b_ge a = b_cond Cond.Ge a
  let b_lt a = b_cond Cond.Lt a
  let b_gt a = b_cond Cond.Gt a
  let b_le a = b_cond Cond.Le a
  let b_al a = b_cond Cond.Al a
  let b_nv a = b_cond Cond.Nv a
end

(* {1 The table} *)
let () =
  let open Builders in
  let open Sh in
  let open Shape in
  let open Enc in
  let p x = P x in
  let k = Kind.[ X; W ] in
  ignore k;
  let addsub sub s = Addsub { sub; s } in
  entry Opcode.Add ~name:"add"
    ~enc:[ addsub false false ]
    ~lower:true
    [ p zzz; p zzz_sh; p spsp_ext; p spsp_i; p spsp_i12; p spsp_lo12 ];
  entry Opcode.Mov ~name:"mov" ~enc:[ Mov ] ~lower:true
    [ p anyany; p z_i64; p vv_b; p fr_lane; p lane_lane ];
  entry Opcode.Movz ~name:"movz" ~enc:[ Wide { opc = 2 } ] [ p z_i; p z_i_lsl ];
  entry Opcode.Movn ~name:"movn" ~enc:[ Wide { opc = 0 } ] [ p z_i; p z_i_lsl ];
  entry Opcode.Movk ~name:"movk" ~enc:[ Wide { opc = 3 } ] [ p z_i; p z_i_lsl ];
  entry Opcode.Cbz ~name:"cbz" ~enc:[ Cbz { nz = false } ] [ p z_sym ];
  entry Opcode.Cbnz ~name:"cbnz" ~enc:[ Cbz { nz = true } ] [ p z_sym ];
  entry Opcode.Sxtw ~name:"sxtw" ~enc:[ Sxtw ] [ p xz_wz ];
  entry Opcode.Uxtw ~name:"uxtw" ~enc:[ Uxtw ] [ p xz_wz ];
  entry Opcode.Udiv ~name:"udiv" ~enc:[ Div { signed = false } ] [ p zzz ];
  entry Opcode.Sdiv ~name:"sdiv" ~enc:[ Div { signed = true } ] [ p zzz ];
  entry Opcode.Cset ~name:"cset" ~enc:[ Cset ] [ p z_cond ];
  entry Opcode.Lsl ~name:"lsl" ~enc:[ Shiftv { code = 0 }; Shift_imm ] ~lower:true [ p zzz; p zz_i ];
  entry Opcode.Lsr ~name:"lsr" ~enc:[ Shiftv { code = 1 }; Shift_imm ] ~lower:true [ p zzz; p zz_i ];
  entry Opcode.Asr ~name:"asr" ~enc:[ Shiftv { code = 2 }; Shift_imm ] ~lower:true [ p zzz; p zz_i ];
  entry Opcode.Ror ~name:"ror" ~enc:[ Shiftv { code = 3 } ] [ p zzz ];
  entry Opcode.Ubfx ~name:"ubfx" ~enc:[ Bitfield { signed = false; lsl_alias = false } ] [ p zzii ];
  entry Opcode.Ubfiz ~name:"ubfiz" ~enc:[ Bitfield { signed = false; lsl_alias = true } ] [ p zzii ];
  entry Opcode.Sbfx ~name:"sbfx" ~enc:[ Bitfield { signed = true; lsl_alias = false } ] [ p zzii ];
  entry Opcode.Sbfiz ~name:"sbfiz" ~enc:[ Bitfield { signed = true; lsl_alias = true } ] [ p zzii ];
  entry Opcode.Rbit ~name:"rbit" ~enc:[ Dp1 { w32 = Some 0; w64 = Some 0 } ] [ p zz ];
  entry Opcode.Rev16 ~name:"rev16" ~enc:[ Dp1 { w32 = Some 1; w64 = Some 1 } ] [ p zz ];
  entry Opcode.Rev32 ~name:"rev32" ~enc:[ Dp1 { w32 = None; w64 = Some 2 } ] [ p xz_xz ];
  entry Opcode.Rev ~name:"rev" ~enc:[ Dp1 { w32 = Some 2; w64 = Some 3 } ] [ p zz ];
  entry Opcode.Clz ~name:"clz" ~enc:[ Dp1 { w32 = Some 4; w64 = Some 4 } ] [ p zz ];
  entry Opcode.Cls ~name:"cls" ~enc:[ Dp1 { w32 = Some 5; w64 = Some 5 } ] [ p zz ];
  entry Opcode.Stp ~name:"stp"
    ~enc:[ Ldst_pair { load = false } ]
    ~lower:true
    [ p zz_mem; p zz_post ];
  entry Opcode.Ldp ~name:"ldp"
    ~enc:[ Ldst_pair { load = true } ]
    ~lower:true
    [ p zz_mem; p zz_post ];
  entry Opcode.Ldr ~name:"ldr"
    ~enc:[ Ldst { load = true; size = None }; Ldst_f { load = true }; Ldst_q { load = true } ]
    ~lower:true
    [ p z_mem; p z_post; p fr_mem; p q_mem ];
  entry Opcode.Ldrb ~name:"ldrb"
    ~enc:[ Ldst { load = true; size = Some 0 } ]
    ~lower:true
    [ p wz_mem; p wz_post ];
  entry Opcode.Ldrh ~name:"ldrh"
    ~enc:[ Ldst { load = true; size = Some 1 } ]
    ~lower:true
    [ p wz_mem; p wz_post ];
  entry Opcode.Ldrsb ~name:"ldrsb" ~enc:[ Ldst_sx { size = 0 } ] ~lower:true [ p z_mem ];
  entry Opcode.Ldrsh ~name:"ldrsh" ~enc:[ Ldst_sx { size = 1 } ] ~lower:true [ p z_mem ];
  entry Opcode.Ldrsw ~name:"ldrsw" ~enc:[ Ldst_sx { size = 2 } ] ~lower:true [ p xz_mem ];
  entry Opcode.Str ~name:"str"
    ~enc:[ Ldst { load = false; size = None }; Ldst_f { load = false }; Ldst_q { load = false } ]
    ~lower:true
    [ p z_mem; p z_post; p fr_mem; p q_mem ];
  entry Opcode.Strb ~name:"strb"
    ~enc:[ Ldst { load = false; size = Some 0 } ]
    ~lower:true
    [ p wz_mem; p wz_post ];
  entry Opcode.Strh ~name:"strh"
    ~enc:[ Ldst { load = false; size = Some 1 } ]
    ~lower:true
    [ p wz_mem; p wz_post ];
  entry Opcode.Ret ~name:"ret" ~enc:[ Ret ] [ p Unit; p xz ];
  entry Opcode.Br ~name:"br" ~enc:[ Br ] [ p xz ];
  entry Opcode.Mrs ~name:"mrs" ~enc:[ Mrs ] [ p (xz_sysreg "fpcr"); p (xz_sysreg "nzcv") ];
  entry Opcode.Msr ~name:"msr" ~enc:[ Msr ] [ p (sysreg_xz "fpcr"); p (sysreg_xz "nzcv") ];
  entry Opcode.Sub ~name:"sub"
    ~enc:[ addsub true false ]
    ~lower:true
    [ p zzz; p zzz_sh; p spsp_ext; p spsp_i; p spsp_i12 ];
  entry Opcode.Cmp ~name:"cmp" ~enc:[ addsub true true ] ~lower:true [ p zz; p sp_i; p sp_i12 ];
  entry Opcode.Cmn ~name:"cmn" ~enc:[ addsub false true ] ~lower:true [ p sp_i; p sp_i12 ];
  entry Opcode.And ~name:"and"
    ~enc:[ Logical { opc = 0; n = false }; Vlogic { u = 0; sel = 0b00 } ]
    ~lower:true
    [ p zzz; p zzz_sh; p spz_i64; p vvv_b ];
  entry Opcode.Orr ~name:"orr"
    ~enc:[ Logical { opc = 1; n = false }; Vlogic { u = 0; sel = 0b10 } ]
    ~lower:true
    [ p zzz; p zzz_sh; p spz_i64; p vvv_b ];
  entry Opcode.Eor ~name:"eor"
    ~enc:[ Logical { opc = 2; n = false }; Vlogic { u = 1; sel = 0b00 } ]
    ~lower:true
    [ p zzz; p zzz_sh; p spz_i64; p vvv_b ];
  entry Opcode.Bic ~name:"bic"
    ~enc:[ Logical { opc = 0; n = true } ]
    ~lower:true
    [ p zzz; p zzz_sh ];
  entry Opcode.Madd ~name:"madd" ~enc:[ Madd { sub = false } ] [ p zzzz ];
  entry Opcode.Msub ~name:"msub" ~enc:[ Madd { sub = true } ] [ p zzzz ];
  entry Opcode.Mul ~name:"mul" ~enc:[ Mul ] [ p zzz ];
  entry Opcode.Csel ~name:"csel" ~enc:[ Csel ] [ p zzz_cond ];
  entry Opcode.Adrp ~name:"adrp" ~enc:[ Adrp ] [ p xz_sym ];
  entry Opcode.Adr ~name:"adr" ~enc:[ Adr ] [ p xz_sym ];
  entry Opcode.B ~name:"b" ~enc:[ B ] [ p sym ];
  List.iter
    (fun c -> entry (Opcode.Bcond c) ~name:("b." ^ Cond.name c) ~enc:[ Bcond ] [ p sym ])
    Cond.all;
  entry Opcode.Udf ~name:"udf" ~enc:[ Udf ] [ p imm ];
  entry Opcode.Bl ~name:"bl" ~enc:[ Bl ] [ p sym ];
  entry Opcode.Fmov ~name:"fmov" ~enc:[ Fmov ] ~lower:true
    [ p frfr; p fr_fimm; p d_xz; p s_wz; p xz_d; p wz_s ];
  entry Opcode.Fcmp ~name:"fcmp" ~enc:[ Fcmp ] [ p frfr; p fr_fzero ];
  entry Opcode.Fadd ~name:"fadd"
    ~enc:[ Fbinop { opc = 2 }; Vbin { u = 0; a = 0; opc = 0b11010 } ]
    [ p frfrfr; p vvv_fp ];
  entry Opcode.Fsub ~name:"fsub"
    ~enc:[ Fbinop { opc = 3 }; Vbin { u = 0; a = 1; opc = 0b11010 } ]
    [ p frfrfr; p vvv_fp ];
  entry Opcode.Fmul ~name:"fmul"
    ~enc:[ Fbinop { opc = 0 }; Vbin { u = 1; a = 0; opc = 0b11011 } ]
    [ p frfrfr; p vvv_fp ];
  entry Opcode.Fdiv ~name:"fdiv"
    ~enc:[ Fbinop { opc = 1 }; Vbin { u = 1; a = 0; opc = 0b11111 } ]
    [ p frfrfr; p vvv_fp ];
  entry Opcode.Fcsel ~name:"fcsel" ~enc:[ Fcsel ] [ p frfrfr_cond ];
  entry Opcode.Scvtf ~name:"scvtf" ~enc:[ Cvtf { signed = true } ] [ p fr_zv ];
  entry Opcode.Ucvtf ~name:"ucvtf" ~enc:[ Cvtf { signed = false } ] [ p fr_zv ];
  entry Opcode.Fneg ~name:"fneg"
    ~enc:[ Fp1 { opcode = 0b000010 }; Vun { u = 1; a = 1; opc = 0b01111 } ]
    [ p frfr; p vv_fp ];
  entry Opcode.Fmax ~name:"fmax"
    ~enc:[ Fbinop { opc = 4 }; Vbin { u = 0; a = 0; opc = 0b11110 } ]
    [ p frfrfr; p vvv_fp ];
  entry Opcode.Fmla ~name:"fmla" ~enc:[ Vbin { u = 0; a = 0; opc = 0b11001 } ] [ p vvv_fp ];
  entry Opcode.Fcmeq ~name:"fcmeq" ~enc:[ Vbin { u = 0; a = 0; opc = 0b11100 } ] [ p vvv_fp ];
  entry Opcode.Fcmge ~name:"fcmge" ~enc:[ Vbin { u = 1; a = 0; opc = 0b11100 } ] [ p vvv_fp ];
  entry Opcode.Fcmgt ~name:"fcmgt" ~enc:[ Vbin { u = 1; a = 1; opc = 0b11100 } ] [ p vvv_fp ];
  entry Opcode.Bsl ~name:"bsl" ~enc:[ Vlogic { u = 1; sel = 0b01 } ] [ p vvv_b ];
  entry Opcode.Bit ~name:"bit" ~enc:[ Vlogic { u = 1; sel = 0b10 } ] [ p vvv_b ];
  entry Opcode.Not ~name:"not" ~enc:[ Vnot ] [ p vv_b ];
  entry Opcode.Fmadd ~name:"fmadd" ~enc:[ Fmadd ] [ p frfrfrfr ];
  entry Opcode.Dup ~name:"dup" ~enc:[ Dup ] ~lower:true [ p vfp_lane; p vfp_zv; p fr_lane ];
  entry Opcode.Ins ~name:"ins" ~enc:[ Ins ] ~lower:true [ p lane_lane ];
  entry Opcode.Ld1 ~name:"ld1" ~enc:[ Vld1 { load = true } ] ~lower:true [ p vl_mem ];
  entry Opcode.Ld1r ~name:"ld1r" ~enc:[ Vld1r ] ~lower:true [ p vlist_mem ];
  entry Opcode.St1 ~name:"st1" ~enc:[ Vld1 { load = false } ] ~lower:true [ p vl_mem ];
  entry Opcode.Fcvtl ~name:"fcvtl" ~enc:[ Vcvt { widen = true } ] [ p vd2_vs2 ];
  entry Opcode.Fcvtn ~name:"fcvtn" ~enc:[ Vcvt { widen = false } ] [ p vs2_vd2 ];
  entry Opcode.Fsqrt ~name:"fsqrt"
    ~enc:[ Fp1 { opcode = 0b000011 }; Vun { u = 1; a = 1; opc = 0b11111 } ]
    [ p frfr; p vv_fp ];
  entry Opcode.Frintz ~name:"frintz"
    ~enc:[ Fp1 { opcode = 0b001011 }; Vun { u = 0; a = 1; opc = 0b11001 } ]
    [ p frfr; p vv_fp ];
  entry Opcode.Fcvt ~name:"fcvt" ~enc:[ Fcvt ] [ p s_d; p d_s ];
  entry Opcode.Fcvtzs ~name:"fcvtzs" ~enc:[ Fcvtz { signed = true } ] [ p zv_fr ];
  entry Opcode.Fcvtzu ~name:"fcvtzu" ~enc:[ Fcvtz { signed = false } ] [ p zv_fr ];
  entry Opcode.Tst ~name:"tst" ~enc:[ Logical { opc = 3; n = false } ] ~lower:true [ p z_i64 ]

(* {1 Views} What the encoder and the parser read. *)

let all = List.rev !registry
let find_table = Hashtbl.create 128
let () = List.iter (fun e -> Hashtbl.replace find_table e.op e) all
let find op = Hashtbl.find find_table op
let find_opt op = Hashtbl.find_opt find_table op
let opcodes = List.map (fun e -> e.op) all
let name op = (find op).name

(* The spellings the parser takes: every entry's own, [mvn] for [not], and [b.<cc>] with the
   condition split off ([bge] is read as [b.ge] too). *)
let spellings = Hashtbl.create 128

let () =
  List.iter
    (fun e -> match e.op with Opcode.Bcond _ -> () | _ -> Hashtbl.replace spellings e.name e.op)
    all;
  Hashtbl.replace spellings "mvn" Opcode.Not

let of_mnemonic m =
  match Hashtbl.find_opt spellings m with
  | Some _ as r -> r
  | None ->
      if String.length m > 2 && String.sub m 0 2 = "b." then
        Option.map (fun c -> Opcode.Bcond c) (Cond.of_name (String.sub m 2 (String.length m - 2)))
      else if String.length m = 3 && m.[0] = 'b' then
        (* gcc prints [bge]/[ble]/[bne]..., which GNU as reads as [b.ge]/[b.le]/[b.ne]. Every real
           three-letter mnemonic beginning with [b] is an entry above. *)
        Option.map (fun c -> Opcode.Bcond c) (Cond.of_name (String.sub m 1 2))
      else None

(* {2 Encoding fields} *)

let enc_find op f = Option.bind (find_opt op) (fun e -> List.find_map f e.enc)
let name_where f = List.find_map (fun e -> if List.exists f e.enc then Some e.name else None) all

(* The shift a register-specified shift selects ([op2]), and the name of each: lsl, lsr, asr, ror. *)
let shift_name code =
  Option.value ~default:"?"
    (name_where (function Enc.Shiftv { code = c } -> c = code | _ -> false))

(* The [opc] of AND, ORR, EOR; [ands], the flag-setting one, is only the spelling of [tst]'s word. *)
let logical_name opc =
  match opc with
  | 3 -> "ands"
  | _ ->
      Option.value ~default:"?"
        (name_where (function Enc.Logical { opc = o; n = false } -> o = opc | _ -> false))

(* "Floating-point data-processing (2 source)"'s 4-bit opcode field. *)
let fbinop_opcode op = enc_find op (function Enc.Fbinop { opc } -> Some opc | _ -> None)

let fbinop_name opc =
  Option.value ~default:"?" (name_where (function Enc.Fbinop { opc = o } -> o = opc | _ -> false))

(* The 6-bit opcode of a "data-processing (1 source)" operation at a register width. *)
let dp1_opcode op ~width =
  match enc_find op (function Enc.Dp1 { w32; w64 } -> Some (w32, w64) | _ -> None) with
  | Some (w32, w64) -> if width = 64 then w64 else w32
  | None -> None

(* [U], [a] (bit 23) and the five-bit opcode of a vector three-register form. *)
let vbin_bits op =
  Option.get (enc_find op (function Enc.Vbin { u; a; opc } -> Some (u, a, opc) | _ -> None))

(* the same for a vector two-register form *)
let vun_bits op =
  Option.get (enc_find op (function Enc.Vun { u; a; opc } -> Some (u, a, opc) | _ -> None))

(* [U] and the two selector bits (23:22) of a bitwise vector operation. *)
let vlogic_bits op =
  Option.get (enc_find op (function Enc.Vlogic { u; sel } -> Some (u, sel) | _ -> None))

(* What an opcode needs enabled: nothing, for this target's whole instruction set is the
   always-available base; an entry with components would name them in [ext]. *)
let requirement op =
  match (find op).ext with
  | [] -> None
  | [ f ] -> Some (Target_component.Req.Feature f)
  | l -> Some (Target_component.Req.All (List.map (fun f -> Target_component.Req.Feature f) l))

(* The opcode of an encoding class: what decoding names a word it read. *)
let of_enc enc = List.find_map (fun e -> if List.mem enc e.enc then Some e.op else None) all
