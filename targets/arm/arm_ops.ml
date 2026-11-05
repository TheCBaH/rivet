(* One definition per hand-written ARM (A32) opcode: its spelling, the operand lists it takes, the
   fields of its encoding class, whether it carries a condition and sets the flags, and whether its
   lowering stays code. Each definition is also the opcode's typed builder: one per operand shape,
   named by the mnemonic with a suffix for the shape ([add r0 r1 r2], [add_i r0 r1 4L],
   [add_sh r0 r1 (shifted Lsl r2 2)], [ldr r0 (mem r1)], [ldr_post r0 r1 4]); a conditional one
   takes [?cond]. A flag-setting form is an opcode of its own ([adds]). An instruction that
   lowering reads as VFP is told apart from the integer ones by its register classes: {!Dreg.t} and
   {!Sreg.t} are different types, so a [d] register where an [s] one is expected does not compile. *)

open Foundation
open Arm_types

module Shift = struct
  type kind = Lsl | Lsr | Asr | Ror

  let code = function Lsl -> 0 | Lsr -> 1 | Asr -> 2 | Ror -> 3
  let of_code = function 0 -> Lsl | 1 -> Lsr | 2 -> Asr | _ -> Ror

  (* a register shifted by a constant: the second operand of a data-processing instruction *)
  type t = { reg : Reg.t; kind : kind; amount : int }
end

module Kind = struct
  type t = R | D | S | Imm | Fimm | Mem | Sym | Sh | Reglist | Dreglist | Wb

  let name = function
    | R -> "r"
    | D -> "d"
    | S -> "s"
    | Imm -> "imm"
    | Fimm -> "fimm"
    | Mem -> "mem"
    | Sym -> "sym"
    | Sh -> "sh"
    | Reglist -> "reglist"
    | Dreglist -> "dreglist"
    | Wb -> "wb"

  let of_operand = function
    | Operand.Reg _ -> R
    | Operand.Dreg _ -> D
    | Operand.Sreg _ -> S
    | Operand.Imm _ -> Imm
    | Operand.FImm _ -> Fimm
    | Operand.Mem _ -> Mem
    | Operand.Sym _ -> Sym
    | Operand.Shifted _ -> Sh
    | Operand.Reglist _ -> Reglist
    | Operand.Dreglist _ -> Dreglist
    | Operand.Reg_writeback _ -> Wb
end

(* A builder's operands, in the text's order; ['f] is the builder's type (without [?cond]), ['r]
   its result. [Zero] and [Apsr_fpscr] are operands the spelling fixes. [Post] is a base register
   and an offset: [\[rn\], #off]. [Lo16]/[Hi16] wrap an expression in [:lower16:]/[:upper16:]. *)
module Shape = struct
  type ('f, 'r) t =
    | Done : ('r, 'r) t
    | Unit : (unit -> 'r, 'r) t
    | Vmrs : (unit -> 'r, 'r) t
    | R : ('f, 'r) t -> (Reg.t -> 'f, 'r) t
    | D : ('f, 'r) t -> (Dreg.t -> 'f, 'r) t
    | S : ('f, 'r) t -> (Sreg.t -> 'f, 'r) t
    | Imm : ('f, 'r) t -> (int -> 'f, 'r) t
    | Imm64 : ('f, 'r) t -> (int64 -> 'f, 'r) t
    | Zero : ('f, 'r) t -> ('f, 'r) t
    | Fimm : ('f, 'r) t -> (float -> 'f, 'r) t
    | Mem : ('f, 'r) t -> (Mem.t -> 'f, 'r) t
    | Post : ('f, 'r) t -> (Reg.t -> int -> 'f, 'r) t
    | Sym : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Lo16 : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Hi16 : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Sh : ('f, 'r) t -> (Shift.t -> 'f, 'r) t
    | Reglist : ('f, 'r) t -> (Reg.t list -> 'f, 'r) t
    | Dreglist : ('f, 'r) t -> (Dreg.t list -> 'f, 'r) t
    | Wb : ('f, 'r) t -> (Reg.t -> 'f, 'r) t

  let rec kinds : type f r. (f, r) t -> Kind.t list = function
    | Done | Unit -> []
    | Vmrs -> Kind.[ Sym; Sym ]
    | R s -> Kind.R :: kinds s
    | D s -> Kind.D :: kinds s
    | S s -> Kind.S :: kinds s
    | Imm s -> Kind.Imm :: kinds s
    | Imm64 s -> Kind.Imm :: kinds s
    | Zero s -> Kind.Imm :: kinds s
    | Fimm s -> Kind.Fimm :: kinds s
    | Mem s -> Kind.Mem :: kinds s
    | Post s -> Kind.Mem :: Kind.Imm :: kinds s
    | Sym s -> Kind.Sym :: kinds s
    | Lo16 s -> Kind.Sym :: kinds s
    | Hi16 s -> Kind.Sym :: kinds s
    | Sh s -> Kind.Sh :: kinds s
    | Reglist s -> Kind.Reglist :: kinds s
    | Dreglist s -> Kind.Dreglist :: kinds s
    | Wb s -> Kind.Wb :: kinds s

  let int n = Operand.Imm (Bigint.of_int n)
  let mem0 base = Operand.Mem { Mem.base; offset = Mem.Imm 0L; writeback = false; pre = true }
  let numbers l = List.map (fun (r : Reg.t) -> r.Reg.num) l
  let dnumbers l = List.map (fun (r : Dreg.t) -> r.Dreg.num) l

  let rec build : type f r. (f, r) t -> (Operand.t list -> r) -> f =
   fun s k ->
    let next s o = build s (fun ops -> k (o :: ops)) in
    match s with
    | Done -> k []
    | Unit -> fun () -> k []
    | Vmrs ->
        fun () ->
          k
            [
              Operand.Sym (Asm_core.Expr.Symbol "APSR_nzcv");
              Operand.Sym (Asm_core.Expr.Symbol "FPSCR");
            ]
    | R s -> fun r -> next s (Operand.Reg r)
    | D s -> fun r -> next s (Operand.Dreg r)
    | S s -> fun r -> next s (Operand.Sreg r)
    | Imm s -> fun n -> next s (int n)
    | Imm64 s -> fun n -> next s (Operand.Imm (Bigint.of_int64 n))
    | Zero s -> next s (int 0)
    | Fimm s -> fun v -> next s (Operand.FImm v)
    | Mem s -> fun m -> next s (Operand.Mem m)
    | Post s -> fun base n -> build s (fun ops -> k (mem0 base :: int n :: ops))
    | Sym s -> fun e -> next s (Operand.Sym e)
    | Lo16 s -> fun e -> next s (Operand.Sym (Asm_core.Expr.Modifier ("lower16", e)))
    | Hi16 s -> fun e -> next s (Operand.Sym (Asm_core.Expr.Modifier ("upper16", e)))
    | Sh s ->
        fun { Shift.reg; kind; amount } ->
          next s (Operand.Shifted { reg; kind = Shift.code kind; amount })
    | Reglist s -> fun l -> next s (Operand.Reglist (numbers l))
    | Dreglist s -> fun l -> next s (Operand.Dreglist (dnumbers l))
    | Wb s -> fun r -> next s (Operand.Reg_writeback r)

  (* [f] applied to sample operands: the register at position [i] is number [1 + i], every
     integer is [imm], a memory operand has offset 0, a symbol is [l], a list holds two registers
     and a shifted register is [lsl #2]. *)
  let rec sample : type f r. (f, r) t -> int -> int -> f -> r =
   fun s i imm f ->
    match s with
    | Done -> f
    | Unit -> f ()
    | Vmrs -> f ()
    | R s -> sample s (i + 1) imm (f (Reg.of_num (1 + i)))
    | D s -> sample s (i + 1) imm (f (Dreg.of_num (1 + i)))
    | S s -> sample s (i + 1) imm (f (Sreg.of_num (1 + i)))
    | Imm s -> sample s (i + 1) imm (f imm)
    | Imm64 s -> sample s (i + 1) imm (f (Int64.of_int imm))
    | Zero s -> sample s i imm f
    | Fimm s -> sample s (i + 1) imm (f 1.0)
    | Mem s ->
        sample s (i + 1) imm
          (f { Mem.base = Reg.of_num (1 + i); offset = Mem.Imm 0L; writeback = false; pre = true })
    | Post s -> sample s (i + 1) imm (f (Reg.of_num (1 + i)) imm)
    | Sym s -> sample s (i + 1) imm (f (Asm_core.Expr.Symbol "l"))
    | Lo16 s -> sample s (i + 1) imm (f (Asm_core.Expr.Symbol "l"))
    | Hi16 s -> sample s (i + 1) imm (f (Asm_core.Expr.Symbol "l"))
    | Sh s ->
        sample s (i + 1) imm (f { Shift.reg = Reg.of_num (1 + i); kind = Shift.Lsl; amount = 2 })
    | Reglist s -> sample s (i + 1) imm (f [ Reg.of_num (1 + i); Reg.of_num (2 + i) ])
    | Dreglist s -> sample s (i + 1) imm (f [ Dreg.of_num (1 + i); Dreg.of_num (2 + i) ])
    | Wb s -> sample s (i + 1) imm (f (Reg.of_num (1 + i)))
end

(* The fields an encoding class fixes. A [Dp] is a data-processing instruction: [dp] is its 4-bit
   opcode field and [s] the flag-setting bit; a [Move] has no first operand ([mov], [mvn]) and a
   [Compare] no destination ([cmp], [cmn]). *)
module Enc = struct
  type dp_form = Normal | Move | Compare
  type cvt = F32_f64 | F64_f32 | F32_s32 | F64_s32 | F64_u32 | S32_f64

  type t =
    | Dp of { dp : int; s : bool; form : dp_form }
    | Shift_op of { kind : int; s : bool }
    | Rrx of { s : bool }
    | Mul
    | Mla
    | Umull
    | Rd_rm of { hi : int64; lo : int64 }
    | Sbfx
    | Bx
    | B
    | Bl
    | Nop
    | Udf
    | Movw
    | Movt
    | It
    | Ldst of { load : bool; byte : bool }
    | Ldst_x of { kind : int; bits : int64 * int64 * int64 }
    | Push
    | Pop
    | Ldm of { load : bool }
    | V3 of { dbl : bool; sel : int64 * int64 * int64 * int64 }
    | Vmov of { dbl : bool }
    | Vmov_core
    | Vneg of { dbl : bool }
    | Vcmp of { dbl : bool }
    | Vmrs
    | Vcvt of cvt
    | Vmem of { load : bool }
    | Vpush
end

type group = Core | Vfp
type shape = { kinds : Kind.t list; sample : cond:Cond.t -> int -> Instruction.t }

type entry = {
  op : Opcode.t;
  name : string;  (** the spelling, with a VFP type suffix where it has one *)
  group : group;
  enc : Enc.t;
  conditional : bool;  (** the instruction word carries a condition *)
  sets_flags : bool;
  ext : string list;  (** the components that publish it: none, the whole target is the base *)
  lower : bool;  (** lowering stays code *)
  mutable shapes : shape list;  (** the builders' operand lists, in definition order *)
  also : Kind.t list list;  (** other operand lists lowering takes, without a builder *)
}

let registry : entry list ref = ref []

let builder op shape =
 fun ?(cond = Cond.Al) -> Shape.build shape (fun ops -> Instruction.mk ~cond op ops)

let shape_of op s =
  { kinds = Shape.kinds s; sample = (fun ~cond imm -> Shape.sample s 0 imm (builder op s ~cond)) }

(* The builder of an opcode's first shape, which also registers the opcode. *)
let def ?(group = Core) ?(conditional = true) ?(sets_flags = false) ?(lower = false) ?(also = [])
    ~name ~enc op s =
  registry :=
    {
      op;
      name;
      group;
      enc;
      conditional;
      sets_flags;
      ext = [];
      lower;
      shapes = [ shape_of op s ];
      also;
    }
    :: !registry;
  builder op s

(* A further shape of an opcode already defined. *)
let alt op s =
  match List.find_opt (fun e -> e.op = op) !registry with
  | Some e ->
      e.shapes <- e.shapes @ [ shape_of op s ];
      builder op s
  | None -> invalid_arg "Arm_ops.alt"

(* {1 The table and its builders} *)
module Builders = struct
  (* An opcode whose word has no condition field: its builder takes no [?cond]. *)
  let uncond ~name ~enc op s =
    registry :=
      {
        op;
        name;
        group = Core;
        enc;
        conditional = false;
        sets_flags = false;
        ext = [];
        lower = true;
        shapes = [ shape_of op s ];
        also = [];
      }
      :: !registry;
    Shape.build s (Instruction.mk op)

  open Shape
  open Enc

  let dp_ ?(s = false) ?(form = Normal) dp = Dp { dp; s; form }
  let rrr = R (R (R Done))
  let rri = R (R (Imm64 Done))
  let rrsh = R (R (Sh Done))
  let rr = R (R Done)
  let shorthand = Kind.[ [ R; R ]; [ R; Sh ] ]
  let three_move = Kind.[ [ R; R; R ]; [ R; R; Sh ] ]

  (* {2 Data processing} The two-operand register forms of the three-operand opcodes ([add r0, r1]
   lowers with [rn] = r0) and the three-operand [mov] are accepted, without builders. *)

  let mov = def ~name:"mov" ~enc:(dp_ ~form:Move 13) ~also:three_move Opcode.Mov rr
  let mov_i = alt Opcode.Mov (R (Imm64 Done))
  let mov_sh = alt Opcode.Mov (R (Sh Done))
  let mvn = def ~name:"mvn" ~enc:(dp_ ~form:Move 15) ~also:three_move Opcode.Mvn rr
  let mvn_i = alt Opcode.Mvn (R (Imm64 Done))
  let mvn_sh = alt Opcode.Mvn (R (Sh Done))
  let add = def ~name:"add" ~enc:(dp_ 4) ~also:shorthand Opcode.Add rrr
  let add_sh = alt Opcode.Add rrsh
  let add_i = alt Opcode.Add rri
  let sub = def ~name:"sub" ~enc:(dp_ 2) ~also:shorthand Opcode.Sub rrr
  let sub_sh = alt Opcode.Sub rrsh
  let sub_i = alt Opcode.Sub rri
  let and_ = def ~name:"and" ~enc:(dp_ 0) ~also:shorthand Opcode.And rrr
  let and_sh = alt Opcode.And rrsh
  let and_i = alt Opcode.And rri
  let eor = def ~name:"eor" ~enc:(dp_ 1) ~also:shorthand Opcode.Eor rrr
  let eor_sh = alt Opcode.Eor rrsh
  let eor_i = alt Opcode.Eor rri
  let rsb = def ~name:"rsb" ~enc:(dp_ 3) ~also:shorthand Opcode.Rsb rrr
  let rsb_sh = alt Opcode.Rsb rrsh
  let rsb_i = alt Opcode.Rsb rri
  let orr = def ~name:"orr" ~enc:(dp_ 12) ~also:shorthand Opcode.Orr rrr
  let orr_sh = alt Opcode.Orr rrsh
  let orr_i = alt Opcode.Orr rri
  let bic = def ~name:"bic" ~enc:(dp_ 14) ~also:shorthand Opcode.Bic rrr
  let bic_sh = alt Opcode.Bic rrsh
  let bic_i = alt Opcode.Bic rri
  let adc = def ~name:"adc" ~enc:(dp_ 5) ~also:shorthand Opcode.Adc rrr
  let adc_sh = alt Opcode.Adc rrsh
  let adc_i = alt Opcode.Adc rri
  let sbc = def ~name:"sbc" ~enc:(dp_ 6) ~also:shorthand Opcode.Sbc rrr
  let sbc_sh = alt Opcode.Sbc rrsh
  let cmp = def ~name:"cmp" ~sets_flags:true ~enc:(dp_ ~s:true ~form:Compare 10) Opcode.Cmp rr
  let cmp_sh = alt Opcode.Cmp (R (Sh Done))
  let cmp_i = alt Opcode.Cmp (R (Imm64 Done))
  let cmn = def ~name:"cmn" ~sets_flags:true ~enc:(dp_ ~s:true ~form:Compare 11) Opcode.Cmn rr
  let cmn_sh = alt Opcode.Cmn (R (Sh Done))
  let cmn_i = alt Opcode.Cmn (R (Imm64 Done))

  (* the flag-setting forms are opcodes of their own *)
  let adds = def ~name:"adds" ~sets_flags:true ~enc:(dp_ ~s:true 4) Opcode.Adds rrr
  let adds_i = alt Opcode.Adds rri
  let subs = def ~name:"subs" ~sets_flags:true ~enc:(dp_ ~s:true 2) Opcode.Subs rrr
  let subs_i = alt Opcode.Subs rri
  let rsbs_i = def ~name:"rsbs" ~sets_flags:true ~enc:(dp_ ~s:true 3) Opcode.Rsbs rri
  let orrs = def ~name:"orrs" ~sets_flags:true ~enc:(dp_ ~s:true 12) Opcode.Orrs rrr
  let adcs = def ~name:"adcs" ~sets_flags:true ~enc:(dp_ ~s:true 5) Opcode.Adcs rrr
  let sbcs = def ~name:"sbcs" ~sets_flags:true ~enc:(dp_ ~s:true 6) Opcode.Sbcs rrr

  (* {2 Shifts} [lsl rd, rm, rs] or [lsl rd, rm, #n]; [lsls] takes the constant only. *)

  let shift_reg = R (R (R Done))
  let shift_imm = R (R (Imm Done))
  let shift ~name k = def ~name ~enc:(Shift_op { kind = k; s = false }) (Opcode.Shift k) shift_reg
  let shift_i k = alt (Opcode.Shift k) shift_imm

  let shifts ~name k =
    def ~name ~sets_flags:true ~enc:(Shift_op { kind = k; s = true }) (Opcode.Shifts k) shift_imm

  let lsl_ = shift ~name:"lsl" 0
  let lsl_i = shift_i 0
  let lsr_ = shift ~name:"lsr" 1
  let lsr_i = shift_i 1
  let asr_ = shift ~name:"asr" 2
  let asr_i = shift_i 2
  let ror = shift ~name:"ror" 3
  let ror_i = shift_i 3
  let lsls_i = shifts ~name:"lsls" 0
  let lsrs_i = shifts ~name:"lsrs" 1
  let asrs_i = shifts ~name:"asrs" 2
  let rors_i = shifts ~name:"rors" 3
  let rrx = def ~name:"rrx" ~enc:(Rrx { s = false }) Opcode.Rrx rr
  let rrxs = def ~name:"rrxs" ~sets_flags:true ~enc:(Rrx { s = true }) Opcode.Rrxs rr

  (* {2 Multiply} *)

  let mul = def ~name:"mul" ~enc:Mul Opcode.Mul rrr
  let mla = def ~name:"mla" ~enc:Mla Opcode.Mla (R (R (R (R Done))))
  let umull = def ~name:"umull" ~enc:Umull Opcode.Umull (R (R (R (R Done))))

  (* {2 Loads and stores} The memory operand is [\[rn, #off\]] (with [!] for write-back) or a
   register offset; [_post] is [\[rn\], #off]. *)

  let ldst ~name ~load ~byte op =
    let b = def ~name ~lower:true ~enc:(Ldst { load; byte }) op (R (Mem Done)) in
    (b, alt op (R (Post Done)))

  let str, str_post = ldst ~name:"str" ~load:false ~byte:false Opcode.Str
  let ldr, ldr_post = ldst ~name:"ldr" ~load:true ~byte:false Opcode.Ldr
  let strb, strb_post = ldst ~name:"strb" ~load:false ~byte:true Opcode.Strb
  let ldrb, ldrb_post = ldst ~name:"ldrb" ~load:true ~byte:true Opcode.Ldrb

  (* the extra loads and stores, by their [L], [S], [H] bits: [S] = [H] = 0 is the multiply space,
     and [L] = 0 with [S] = 1 is LDRD/STRD, neither of which is one of these *)
  let ldst_x ~name kind bits op =
    let b = def ~name ~lower:true ~enc:(Ldst_x { kind; bits }) op (R (Mem Done)) in
    (b, alt op (R (Post Done)))

  let strh, strh_post = ldst_x ~name:"strh" 0 (0L, 0L, 1L) Opcode.Strh
  let ldrh, ldrh_post = ldst_x ~name:"ldrh" 1 (1L, 0L, 1L) Opcode.Ldrh
  let ldrsb, ldrsb_post = ldst_x ~name:"ldrsb" 2 (1L, 1L, 0L) Opcode.Ldrsb
  let ldrsh, ldrsh_post = ldst_x ~name:"ldrsh" 3 (1L, 1L, 1L) Opcode.Ldrsh

  (* {2 Register and bit-field operations} *)

  (* [cond | hi | 1111 | Rd | 1111 | lo | Rm]: one fixed pair of fields per operation *)
  let rd_rm ~name hi lo op = def ~name ~enc:(Rd_rm { hi; lo }) op rr
  let clz = rd_rm ~name:"clz" 0x16L 0b0001L Opcode.Clz
  let rev = rd_rm ~name:"rev" 0x6BL 0b0011L Opcode.Rev
  let rev16 = rd_rm ~name:"rev16" 0x6BL 0b1011L Opcode.Rev16
  let sbfx = def ~name:"sbfx" ~enc:Sbfx Opcode.Sbfx (R (R (Imm (Imm Done))))
  let bx = def ~name:"bx" ~enc:Bx Opcode.Bx (R Done)

  (* {2 Branches, immediates, hints} *)

  let b = def ~name:"b" ~enc:B Opcode.B (Sym Done)
  let bl = def ~name:"bl" ~enc:Bl Opcode.Bl (Sym Done)
  let movw = def ~name:"movw" ~lower:true ~enc:Movw Opcode.Movw (R (Imm Done))
  let movw_s = alt Opcode.Movw (R (Lo16 Done))
  let movt = def ~name:"movt" ~lower:true ~enc:Movt Opcode.Movt (R (Imm Done))
  let movt_s = alt Opcode.Movt (R (Hi16 Done))
  let nop = def ~name:"nop" ~enc:Nop Opcode.Nop Unit
  let udf = uncond ~name:"udf" ~enc:Udf Opcode.Udf (Imm Done)

  (* GAS accepts [it]/[ite] and emits nothing: conditional execution is native to every instruction *)
  let ite = uncond ~name:"ite" ~enc:It Opcode.Ite Unit

  (* {2 Register lists} *)

  let push = def ~name:"push" ~lower:true ~enc:Push Opcode.Push (Reglist Done)
  let pop = def ~name:"pop" ~lower:true ~enc:Pop Opcode.Pop (Reglist Done)

  let stmia =
    def ~name:"stmia" ~lower:true ~enc:(Ldm { load = false }) Opcode.Stmia (Wb (Reglist Done))

  let ldmia =
    def ~name:"ldmia" ~lower:true ~enc:(Ldm { load = true }) Opcode.Ldmia (Wb (Reglist Done))

  (* {2 VFP} A double-precision operand is a {!Dreg.t}, a single-precision one a {!Sreg.t}. *)

  let vfp ?(lower = false) ~name ~enc op s = def ~group:Vfp ~lower ~name ~enc op s
  let ddd = D (D (D Done))
  let sss = S (S (S Done))
  let dd = D (D Done)
  let ss = S (S Done)

  let vadd_f64 =
    vfp ~name:"vadd.f64" ~enc:(V3 { dbl = true; sel = (0L, 1L, 1L, 0L) }) (Opcode.V3d V3.Vadd) ddd

  let vsub_f64 =
    vfp ~name:"vsub.f64" ~enc:(V3 { dbl = true; sel = (0L, 1L, 1L, 1L) }) (Opcode.V3d V3.Vsub) ddd

  let vmul_f64 =
    vfp ~name:"vmul.f64" ~enc:(V3 { dbl = true; sel = (0L, 1L, 0L, 0L) }) (Opcode.V3d V3.Vmul) ddd

  let vdiv_f64 =
    vfp ~name:"vdiv.f64" ~enc:(V3 { dbl = true; sel = (1L, 0L, 0L, 0L) }) (Opcode.V3d V3.Vdiv) ddd

  let vmla_f64 =
    vfp ~name:"vmla.f64" ~enc:(V3 { dbl = true; sel = (0L, 0L, 0L, 0L) }) (Opcode.V3d V3.Vmla) ddd

  let vadd_f32 =
    vfp ~name:"vadd.f32" ~enc:(V3 { dbl = false; sel = (0L, 1L, 1L, 0L) }) (Opcode.V3s V3.Vadd) sss

  let vsub_f32 =
    vfp ~name:"vsub.f32" ~enc:(V3 { dbl = false; sel = (0L, 1L, 1L, 1L) }) (Opcode.V3s V3.Vsub) sss

  let vmul_f32 =
    vfp ~name:"vmul.f32" ~enc:(V3 { dbl = false; sel = (0L, 1L, 0L, 0L) }) (Opcode.V3s V3.Vmul) sss

  let vdiv_f32 =
    vfp ~name:"vdiv.f32" ~enc:(V3 { dbl = false; sel = (1L, 0L, 0L, 0L) }) (Opcode.V3s V3.Vdiv) sss

  let vmla_f32 =
    vfp ~name:"vmla.f32" ~enc:(V3 { dbl = false; sel = (0L, 0L, 0L, 0L) }) (Opcode.V3s V3.Vmla) sss

  let vmov_f64 = vfp ~name:"vmov.f64" ~enc:(Vmov { dbl = true }) Opcode.Vmov_d dd
  let vmov_f64_i = alt Opcode.Vmov_d (D (Fimm Done))
  let vmov_f32 = vfp ~name:"vmov.f32" ~enc:(Vmov { dbl = false }) Opcode.Vmov_s ss
  let vmov_f32_i = alt Opcode.Vmov_s (S (Fimm Done))

  (* [vmov] between core and VFP registers, by direction *)
  let vmov_to_s = vfp ~name:"vmov" ~enc:Vmov_core Opcode.Vmov_core (S (R Done))
  let vmov_from_s = alt Opcode.Vmov_core (R (S Done))
  let vmov_to_d = alt Opcode.Vmov_core (D (R (R Done)))
  let vmov_from_d = alt Opcode.Vmov_core (R (R (D Done)))
  let vneg_f64 = vfp ~name:"vneg.f64" ~enc:(Vneg { dbl = true }) Opcode.Vneg_d dd
  let vneg_f32 = vfp ~name:"vneg.f32" ~enc:(Vneg { dbl = false }) Opcode.Vneg_s ss
  let vcmp_f64 = vfp ~name:"vcmp.f64" ~enc:(Vcmp { dbl = true }) Opcode.Vcmp_d dd
  let vcmp_f64_z = alt Opcode.Vcmp_d (D (Zero Done))
  let vcmp_f32 = vfp ~name:"vcmp.f32" ~enc:(Vcmp { dbl = false }) Opcode.Vcmp_s ss
  let vcmp_f32_z = alt Opcode.Vcmp_s (S (Zero Done))
  let vmrs = vfp ~lower:true ~name:"vmrs" ~enc:Vmrs Opcode.Vmrs Vmrs
  let vcvt_f32_f64 = vfp ~name:"vcvt.f32.f64" ~enc:(Vcvt F32_f64) Opcode.Vcvt_f32_f64 (S (D Done))
  let vcvt_f64_f32 = vfp ~name:"vcvt.f64.f32" ~enc:(Vcvt F64_f32) Opcode.Vcvt_f64_f32 (D (S Done))
  let vcvt_f32_s32 = vfp ~name:"vcvt.f32.s32" ~enc:(Vcvt F32_s32) Opcode.Vcvt_f32_s32 ss
  let vcvt_f64_s32 = vfp ~name:"vcvt.f64.s32" ~enc:(Vcvt F64_s32) Opcode.Vcvt_f64_s32 (D (S Done))
  let vcvt_f64_u32 = vfp ~name:"vcvt.f64.u32" ~enc:(Vcvt F64_u32) Opcode.Vcvt_f64_u32 (D (S Done))
  let vcvt_s32_f64 = vfp ~name:"vcvt.s32.f64" ~enc:(Vcvt S32_f64) Opcode.Vcvt_s32_f64 (S (D Done))

  (* [vldr]/[vstr]: an offset from a core register; [vldr] also loads a literal at a label *)
  let vldr_d = vfp ~lower:true ~name:"vldr" ~enc:(Vmem { load = true }) Opcode.Vldr (D (Mem Done))
  let vldr_s = alt Opcode.Vldr (S (Mem Done))
  let vldr_d_l = alt Opcode.Vldr (D (Sym Done))
  let vldr_s_l = alt Opcode.Vldr (S (Sym Done))
  let vstr_d = vfp ~lower:true ~name:"vstr" ~enc:(Vmem { load = false }) Opcode.Vstr (D (Mem Done))
  let vstr_s = alt Opcode.Vstr (S (Mem Done))
  let vpush_64 = vfp ~lower:true ~name:"vpush.64" ~enc:Vpush Opcode.Vpush (Dreglist Done)
end

(* {1 Views} What the encoder and the parser read. *)

let all = List.rev !registry
let find_table = Hashtbl.create 128
let () = List.iter (fun e -> Hashtbl.replace find_table e.op e) all
let find op = Hashtbl.find find_table op
let find_opt op = Hashtbl.find_opt find_table op
let opcodes = List.map (fun e -> e.op) all
let name op = (find op).name

(* The spellings the parser takes: the core ones whole, [it] too for the IT-block prefix. *)
let core_spellings = Hashtbl.create 128

let () =
  List.iter (fun e -> if e.group = Core then Hashtbl.replace core_spellings e.name e.op) all;
  Hashtbl.replace core_spellings "it" Opcode.Ite

let of_mnemonic m = Hashtbl.find_opt core_spellings m

(* A VFP spelling is a stem and its type suffixes: [vcvt.f64.s32] is [vcvt] and [f64; s32]. *)
let vfp_spellings = Hashtbl.create 64

let split_name n =
  match String.split_on_char '.' n with stem :: rest -> (stem, rest) | [] -> (n, [])

let () =
  List.iter
    (fun e -> if e.group = Vfp then Hashtbl.replace vfp_spellings (split_name e.name) e.op)
    all

let vfp_bases =
  List.sort_uniq compare
    (List.filter_map (fun e -> if e.group = Vfp then Some (fst (split_name e.name)) else None) all)

let vfp_opcode_of stem rest = Hashtbl.find_opt vfp_spellings (stem, rest)

(* [vmoveq.f64] splits as base [vmov], condition [eq], type suffix [f64]: the condition sits between
   the base and the first dot, not at the very end the way {!Cond.split} expects. *)
let split_vfp_mnemonic m =
  match String.split_on_char '.' m with
  | [] -> None
  | base_cond :: rest ->
      if List.mem base_cond vfp_bases then Some (base_cond, Cond.Al, rest)
      else
        let stem, cond = Cond.split base_cond in
        if List.mem stem vfp_bases then Some (stem, cond, rest) else None

(* A32 encodes a data-processing opcode as one format distinguished by a 4-bit field; [-1] is "not a
   data-processing opcode". *)
let to_dp op = match find_opt op with Some { enc = Enc.Dp { dp; _ }; _ } -> dp | _ -> -1

(* The opcode of a [dp] value, without the S bit: the compares are the only entries with it set
   whose [dp] is not shared. *)
let of_dp dp =
  List.find_map
    (fun e ->
      match e.enc with
      | Enc.Dp { dp = d; s; form } when d = dp && ((not s) || form = Enc.Compare) -> Some e.op
      | _ -> None)
    all

(* The opcodes that write no destination register and set the flags instead. *)
let is_compare op =
  match find_opt op with Some { enc = Enc.Dp { form = Enc.Compare; _ }; _ } -> true | _ -> false

let x_entries =
  List.filter_map
    (fun e -> match e.enc with Enc.Ldst_x { kind; bits } -> Some (kind, e.name, bits) | _ -> None)
    all

let x_name k =
  match List.find_opt (fun (n, _, _) -> n = k) x_entries with Some (_, m, _) -> m | None -> "?"

let x_bits k =
  match List.find_opt (fun (n, _, _) -> n = k) x_entries with
  | Some (_, _, b) -> b
  | None -> (0L, 0L, 0L)

let x_kind_of_bits b = List.find_map (fun (n, _, b') -> if b = b' then Some n else None) x_entries

(* [clz]/[rev]/[rev16]: the fixed pair of fields. *)
let rd_rm op =
  match find_opt op with Some { enc = Enc.Rd_rm { hi; lo }; _ } -> Some (hi, lo) | _ -> None

(* A VFP three-operand opcode's selector bits (b23, b21, b20, b6), verified against the GNU
   assembler for [vadd.f64] ([ee310b02]), [vsub.f64] ([ee310b42]), [vmul.f64] ([ee210b02]),
   [vdiv.f64] ([ee810b02]) and [vmla.f64] ([ee010b02]). *)
let v3_selector (op : V3.op) =
  let opcode = Opcode.V3d op in
  match (find opcode).enc with Enc.V3 { sel; _ } -> sel | _ -> invalid_arg "Arm_ops.v3_selector"

let v3_of_selector sel =
  List.find_map
    (fun e ->
      match (e.op, e.enc) with
      | Opcode.V3d op, Enc.V3 { sel = s; _ } when s = sel -> Some op
      | _ -> None)
    all

let v3_name (op : V3.op) = fst (split_name (find (Opcode.V3d op)).name)

(* What an opcode needs enabled: nothing, for this target's whole instruction set is the
   always-available base; an entry with components would name them in [ext]. *)
let requirement op =
  match (find op).ext with
  | [] -> None
  | [ f ] -> Some (Target_component.Req.Feature f)
  | l -> Some (Target_component.Req.All (List.map (fun f -> Target_component.Req.Feature f) l))

(* The opcode of an encoding class: what decoding names a word it read. *)
let of_enc enc = List.find_map (fun e -> if e.enc = enc then Some e.op else None) all

(* The flag-setting opcode of a data-processing [dp] with a builder over [kinds], if there is one:
   [adds] has a register and an immediate shape, [orrs] a register shape only. *)
let s_form dp kinds =
  List.find_map
    (fun e ->
      match e.enc with
      | Enc.Dp { dp = d; s = true; form = Enc.Normal }
        when d = dp && List.exists (fun (sh : shape) -> sh.kinds = kinds) e.shapes ->
          Some e.op
      | _ -> None)
    all
