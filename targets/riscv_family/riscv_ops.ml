(* One definition per hand-written RISC-V opcode: the operands it takes, its encoding, the
   components that publish it, the XLEN it exists for, its class, the rounding mode it takes when
   none is written, and whether its lowering stays code (pseudo expansions and irregular forms).
   Each definition is also the opcode's typed builder. The encoder does not read it yet: a test
   holds it to the encoder's own tables until each consumer switches to it. *)

open Foundation
open Riscv_types

module Kind = struct
  type t = X | F | V | Creg | Imm | Mem | Cmem | Spmem | Sym | Rm

  let name = function
    | X -> "x"
    | F -> "f"
    | V -> "v"
    | Creg -> "creg"
    | Imm -> "imm"
    | Mem -> "mem"
    | Cmem -> "cmem"
    | Spmem -> "spmem"
    | Sym -> "sym"
    | Rm -> "rm"
end

(* A compressed load or store's memory operand: a base among x8..x15 and an offset. *)
module Cmem = struct
  type t = { base : Creg.t; offset : int }
end

(* A builder's operands, in the text's order. ['f] is the builder's type with immediates as [int]
   ([int64] for [li]), ['g] the same with immediates as {!Asm_core.Expr.t} (the [_e] builder), ['r]
   their result. *)
module Shape = struct
  type ('f, 'g, 'r) t =
    | Done : ('r, 'r, 'r) t
    | X : ('f, 'g, 'r) t -> (Xreg.t -> 'f, Xreg.t -> 'g, 'r) t
    | F : ('f, 'g, 'r) t -> (Freg.t -> 'f, Freg.t -> 'g, 'r) t
    | V : ('f, 'g, 'r) t -> (Vreg.t -> 'f, Vreg.t -> 'g, 'r) t
    | Creg : ('f, 'g, 'r) t -> (Creg.t -> 'f, Creg.t -> 'g, 'r) t
    | Imm : ('f, 'g, 'r) t -> (int -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Imm64 : ('f, 'g, 'r) t -> (int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Mem : ('f, 'g, 'r) t -> (Mem.t -> 'f, Mem.t -> 'g, 'r) t
    | Cmem : ('f, 'g, 'r) t -> (Cmem.t -> 'f, Cmem.t -> 'g, 'r) t
    | Spmem : ('f, 'g, 'r) t -> (int -> 'f, int -> 'g, 'r) t
    | Sym : ('f, 'g, 'r) t -> (Asm_core.Expr.t -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Rm : ('f, 'g, 'r) t -> (Rm.t -> 'f, Rm.t -> 'g, 'r) t

  let rec kinds : type f g r. (f, g, r) t -> Kind.t list = function
    | Done -> []
    | X s -> Kind.X :: kinds s
    | F s -> Kind.F :: kinds s
    | V s -> Kind.V :: kinds s
    | Creg s -> Kind.Creg :: kinds s
    | Imm s -> Kind.Imm :: kinds s
    | Imm64 s -> Kind.Imm :: kinds s
    | Mem s -> Kind.Mem :: kinds s
    | Cmem s -> Kind.Cmem :: kinds s
    | Spmem s -> Kind.Spmem :: kinds s
    | Sym s -> Kind.Sym :: kinds s
    | Rm s -> Kind.Rm :: kinds s

  let int n = Operand.Imm (Bigint.of_int n)
  let expr = function Asm_core.Expr.Const n -> Operand.Imm n | e -> Operand.Sym e
  let x r = Operand.Reg (Reg.X r)

  let mem ~base offset =
    Operand.Mem { Mem.offset = Asm_core.Expr.Const (Bigint.of_int offset); base = Reg.X base }

  let cmem (m : Cmem.t) = mem ~base:(Creg.to_xreg m.base) m.offset
  let rm r = Operand.Sym (Asm_core.Expr.Symbol (Rm.name r))

  let rec build : type f g r. (f, g, r) t -> (Operand.t list -> r) -> f =
   fun s k ->
    let next s o = build s (fun ops -> k (o :: ops)) in
    match s with
    | Done -> k []
    | X s -> fun r -> next s (x r)
    | F s -> fun r -> next s (Operand.Reg (Reg.F r))
    | V s -> fun r -> next s (Operand.Reg (Reg.V r))
    | Creg s -> fun r -> next s (x (Creg.to_xreg r))
    | Imm s -> fun n -> next s (int n)
    | Imm64 s -> fun n -> next s (Operand.Imm (Bigint.of_int64 n))
    | Mem s -> fun m -> next s (Operand.Mem m)
    | Cmem s -> fun m -> next s (cmem m)
    | Spmem s -> fun n -> next s (mem ~base:Xreg.sp n)
    | Sym s -> fun e -> next s (Operand.Sym e)
    | Rm s -> fun r -> next s (rm r)

  let rec build_e : type f g r. (f, g, r) t -> (Operand.t list -> r) -> g =
   fun s k ->
    let next s o = build_e s (fun ops -> k (o :: ops)) in
    match s with
    | Done -> k []
    | X s -> fun r -> next s (x r)
    | F s -> fun r -> next s (Operand.Reg (Reg.F r))
    | V s -> fun r -> next s (Operand.Reg (Reg.V r))
    | Creg s -> fun r -> next s (x (Creg.to_xreg r))
    | Imm s -> fun e -> next s (expr e)
    | Imm64 s -> fun e -> next s (expr e)
    | Mem s -> fun m -> next s (Operand.Mem m)
    | Cmem s -> fun m -> next s (cmem m)
    | Spmem s -> fun n -> next s (mem ~base:Xreg.sp n)
    | Sym s -> fun e -> next s (Operand.Sym e)
    | Rm s -> fun r -> next s (rm r)

  (* [f] applied to the sample operands: the register at position [i] is number [8 + i] (a
     compressed register too), every immediate is [imm], a memory operand has offset 0, a target
     is the symbol [l] and a rounding mode is [rtz]. *)
  let rec sample : type f g r. (f, g, r) t -> int -> int -> f -> r =
   fun s i imm f ->
    match s with
    | Done -> f
    | X s -> sample s (i + 1) imm (f (Xreg.of_int_exn (8 + i)))
    | F s -> sample s (i + 1) imm (f (Freg.of_int_exn (8 + i)))
    | V s -> sample s (i + 1) imm (f (Vreg.of_int_exn (8 + i)))
    | Creg s -> sample s (i + 1) imm (f (Creg.of_int_exn (8 + i)))
    | Imm s -> sample s (i + 1) imm (f imm)
    | Imm64 s -> sample s (i + 1) imm (f (Int64.of_int imm))
    | Mem s ->
        sample s (i + 1) imm
          (f
             {
               Mem.offset = Asm_core.Expr.Const Bigint.zero;
               base = Reg.X (Xreg.of_int_exn (8 + i));
             })
    | Cmem s -> sample s (i + 1) imm (f { Cmem.base = Creg.of_int_exn (8 + i); offset = 0 })
    | Spmem s -> sample s (i + 1) imm (f 0)
    | Sym s -> sample s (i + 1) imm (f (Asm_core.Expr.Symbol "l"))
    | Rm s -> sample s (i + 1) imm (f Rm.Rtz)
end

(* The fields an encoding fixes. A shift's RV32 [funct7] is its RV64 [funct6] shifted left once. *)
module Enc = struct
  type t =
    | R of { opcode : int; funct3 : int; funct7 : int; rs2 : int option }
    | Rbs of { funct7 : int }
    | R4 of { opcode : int; fmt : int }
    | Unary of { opcode : int; funct3 : int; funct12 : int }
    | I of { opcode : int; funct3 : int }
    | Shift of { funct3 : int; funct6 : int }
    | Shiftw of { funct3 : int; funct7 : int }
    | S of { opcode : int; funct3 : int }
    | B of { funct3 : int }
    | U of { opcode : int }
    | Ca of { funct6 : int; funct2 : int }
    | Cr of { funct4 : int }
    | C of { funct3 : int }
end

type xlen = Any | Rv32 | Rv64

type entry = {
  op : Opcode.t;
  shape : Kind.t list;
  also : Kind.t list list;  (** other operand lists lowering takes *)
  enc : Enc.t option;  (** [None]: no single regular encoding *)
  enc64 : Enc.t option;  (** RV64's, where it differs other than as a shift *)
  ext : string list;  (** the components that publish the spelling; none for the base *)
  src : string option;  (** the riscv-opcodes record of the form *)
  alt : string list;  (** when several components provide it independently: any of these *)
  xlen : xlen;
  cls : Cls.t;
  rm : Rm.t option;
  lower : bool;  (** lowering stays code *)
  sample : int -> Instruction.t;
      (** the builder applied to {!Shape.sample}'s operands, with this immediate *)
}

let registry = ref []

(* The builder over [int] immediates and the one over expressions. *)
let def ?(also = []) ?enc ?enc64 ?(ext = []) ?src ?(alt = []) ?(xlen = Any) ?rm ?(lower = false)
    ~cls op shape =
  let b = Shape.build shape (Instruction.mk op) in
  let sample imm = Shape.sample shape 0 imm b in
  registry :=
    { op; shape = Shape.kinds shape; also; enc; enc64; ext; src; alt; xlen; cls; rm; lower; sample }
    :: !registry;
  (b, Shape.build_e shape (Instruction.mk op))

let add, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Add
    Shape.(X (X (X Done)))

let sub, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x20; rs2 = None })
    Opcode.Sub
    Shape.(X (X (X Done)))

let sll, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sll
    Shape.(X (X (X Done)))

let slt, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Slt
    Shape.(X (X (X Done)))

let sltu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sltu
    Shape.(X (X (X Done)))

let snez, _ = def ~cls:Alu ~lower:true Opcode.Snez Shape.(X (X Done))
let neg, _ = def ~cls:Alu ~lower:true Opcode.Neg Shape.(X (X Done))
let seqz, _ = def ~cls:Alu ~lower:true Opcode.Seqz Shape.(X (X Done))
let sltz, _ = def ~cls:Alu ~lower:true Opcode.Sltz Shape.(X (X Done))
let sgtz, _ = def ~cls:Alu ~lower:true Opcode.Sgtz Shape.(X (X Done))
let zext_b, _ = def ~cls:Alu ~lower:true Opcode.Zext_b Shape.(X (X Done))

let xor, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Xor
    Shape.(X (X (X Done)))

let srl, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Srl
    Shape.(X (X (X Done)))

let sra, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x20; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sra
    Shape.(X (X (X Done)))

let or_, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Or
    Shape.(X (X (X Done)))

let and_, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.And
    Shape.(X (X (X Done)))

let mul, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~src:"rv_m/mul" Opcode.Mul
    Shape.(X (X (X Done)))

let remu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv_m/remu" Opcode.Remu
    Shape.(X (X (X Done)))

let mulh, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~src:"rv_m/mulh" Opcode.Mulh
    Shape.(X (X (X Done)))

let mulhsu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~src:"rv_m/mulhsu" Opcode.Mulhsu
    Shape.(X (X (X Done)))

let mulhu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~src:"rv_m/mulhu" Opcode.Mulhu
    Shape.(X (X (X Done)))

let div, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv_m/div" Opcode.Div
    Shape.(X (X (X Done)))

let divu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv_m/divu" Opcode.Divu
    Shape.(X (X (X Done)))

let rem, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv_m/rem" Opcode.Rem
    Shape.(X (X (X Done)))

let sh1add, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv_zba/sh1add" Opcode.Sh1add
    Shape.(X (X (X Done)))

let sh2add, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv_zba/sh2add" Opcode.Sh2add
    Shape.(X (X (X Done)))

let sh3add, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv_zba/sh3add" Opcode.Sh3add
    Shape.(X (X (X Done)))

let min, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/min" Opcode.Min
    Shape.(X (X (X Done)))

let minu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/minu" Opcode.Minu
    Shape.(X (X (X Done)))

let max, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/max" Opcode.Max
    Shape.(X (X (X Done)))

let maxu, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/maxu" Opcode.Maxu
    Shape.(X (X (X Done)))

let andn, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/andn" ~alt:[ "zbb"; "zbkb" ] Opcode.Andn
    Shape.(X (X (X Done)))

let orn, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/orn" ~alt:[ "zbb"; "zbkb" ] Opcode.Orn
    Shape.(X (X (X Done)))

let xnor, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/xnor" ~alt:[ "zbb"; "zbkb" ] Opcode.Xnor
    Shape.(X (X (X Done)))

let rol, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/rol" ~alt:[ "zbb"; "zbkb" ] Opcode.Rol
    Shape.(X (X (X Done)))

let ror, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv_zbb/ror" ~alt:[ "zbb"; "zbkb" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Ror
    Shape.(X (X (X Done)))

let bclr, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x24; rs2 = None })
    ~ext:[ "zbs" ] ~src:"rv_zbs/bclr"
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bclr
    Shape.(X (X (X Done)))

let bext, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x24; rs2 = None })
    ~ext:[ "zbs" ] ~src:"rv_zbs/bext"
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bext
    Shape.(X (X (X Done)))

let binv, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x34; rs2 = None })
    ~ext:[ "zbs" ] ~src:"rv_zbs/binv"
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Binv
    Shape.(X (X (X Done)))

let bset, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbs" ] ~src:"rv_zbs/bset"
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bset
    Shape.(X (X (X Done)))

let bclri, bclri_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x12 })
    ~ext:[ "zbs" ] ~src:"rv64_zbs/bclri"
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bclri
    Shape.(X (X (Imm Done)))

let bexti, bexti_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x12 })
    ~ext:[ "zbs" ] ~src:"rv64_zbs/bexti"
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bexti
    Shape.(X (X (Imm Done)))

let binvi, binvi_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x1a })
    ~ext:[ "zbs" ] ~src:"rv64_zbs/binvi"
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Binvi
    Shape.(X (X (Imm Done)))

let bseti, bseti_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x0a })
    ~ext:[ "zbs" ] ~src:"rv64_zbs/bseti"
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bseti
    Shape.(X (X (Imm Done)))

let sh1add_uw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv64_zba/sh1add.uw" ~xlen:Rv64 Opcode.Sh1adduw
    Shape.(X (X (X Done)))

let sh2add_uw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv64_zba/sh2add.uw" ~xlen:Rv64 Opcode.Sh2adduw
    Shape.(X (X (X Done)))

let sh3add_uw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 6; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~src:"rv64_zba/sh3add.uw" ~xlen:Rv64 Opcode.Sh3adduw
    Shape.(X (X (X Done)))

let clz, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x600 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/clz" Opcode.Clz
    Shape.(X (X Done))

let ctz, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x601 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/ctz" Opcode.Ctz
    Shape.(X (X Done))

let cpop, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x602 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/cpop" Opcode.Cpop
    Shape.(X (X Done))

let sext_b, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x604 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/sext.b" Opcode.Sextb
    Shape.(X (X Done))

let sext_h, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x605 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/sext.h" Opcode.Sexth
    Shape.(X (X Done))

let orc_b, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x287 })
    ~ext:[ "zbb" ] ~src:"rv_zbb/orc.b" Opcode.Orcb
    Shape.(X (X Done))

let clzw, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x600 })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/clzw" ~xlen:Rv64 Opcode.Clzw
    Shape.(X (X Done))

let ctzw, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x601 })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/ctzw" ~xlen:Rv64 Opcode.Ctzw
    Shape.(X (X Done))

let cpopw, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x602 })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/cpopw" ~xlen:Rv64 Opcode.Cpopw
    Shape.(X (X Done))

let brev8, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x687 })
    ~ext:[ "zbkb" ] ~src:"rv_zbkb/brev8" Opcode.Brev8
    Shape.(X (X Done))

let rev8, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x698 })
    ~enc64:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x6b8 })
    ~ext:[ "zbb" ] ~src:"rv32_zbkb/rev8" ~alt:[ "zbb"; "zbkb" ] Opcode.Rev8
    Shape.(X (X Done))

let pack, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] ~src:"rv_zbkb/pack" Opcode.Pack
    Shape.(X (X (X Done)))

let packh, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] ~src:"rv_zbkb/packh" Opcode.Packh
    Shape.(X (X (X Done)))

let packw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] ~src:"rv64_zbkb/packw" ~xlen:Rv64 Opcode.Packw
    Shape.(X (X (X Done)))

let zip, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x08f })
    ~ext:[ "zbkb" ] ~src:"rv32_zbkb/zip" ~xlen:Rv32 Opcode.Zip
    Shape.(X (X Done))

let unzip, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x08f })
    ~ext:[ "zbkb" ] ~src:"rv32_zbkb/unzip" ~xlen:Rv32 Opcode.Unzip
    Shape.(X (X Done))

let rolw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 1; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/rolw" ~alt:[ "zbb"; "zbkb" ] ~xlen:Rv64 Opcode.Rolw
    Shape.(X (X (X Done)))

let rorw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/rorw" ~alt:[ "zbb"; "zbkb" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Rorw
    Shape.(X (X (X Done)))

let rori, rori_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x18 })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/rori" ~alt:[ "zbb"; "zbkb" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Rori
    Shape.(X (X (Imm Done)))

let roriw, roriw_e =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x30 })
    ~ext:[ "zbb" ] ~src:"rv64_zbb/roriw" ~alt:[ "zbb"; "zbkb" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Roriw
    Shape.(X (X (Imm Done)))

let zext_h, _ =
  def ~cls:Alu ~ext:[ "zbb" ] ~src:"rv64_zbb/zext.h" ~lower:true Opcode.Zext_h Shape.(X (X Done))

let clmul, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] ~src:"rv_zbc/clmul" ~alt:[ "zbc"; "zbkc" ] Opcode.Clmul
    Shape.(X (X (X Done)))

let clmulh, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] ~src:"rv_zbc/clmulh" ~alt:[ "zbc"; "zbkc" ] Opcode.Clmulh
    Shape.(X (X (X Done)))

let clmulr, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] ~src:"rv_zbc/clmulr" Opcode.Clmulr
    Shape.(X (X (X Done)))

let czero_eqz, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x07; rs2 = None })
    ~ext:[ "zicond" ] ~src:"rv_zicond/czero.eqz" Opcode.Czero_eqz
    Shape.(X (X (X Done)))

let czero_nez, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x07; rs2 = None })
    ~ext:[ "zicond" ] ~src:"rv_zicond/czero.nez" Opcode.Czero_nez
    Shape.(X (X (X Done)))

let sm3p0, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x108 })
    ~ext:[ "zksh" ] ~src:"rv_zksh/sm3p0" Opcode.Sm3p0
    Shape.(X (X Done))

let sm3p1, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x109 })
    ~ext:[ "zksh" ] ~src:"rv_zksh/sm3p1" Opcode.Sm3p1
    Shape.(X (X Done))

let sm4ed, sm4ed_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x18 })
    ~ext:[ "zksed" ] ~src:"rv_zksed/sm4ed" Opcode.Sm4ed
    Shape.(X (X (X (Imm Done))))

let sm4ks, sm4ks_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x1a })
    ~ext:[ "zksed" ] ~src:"rv_zksed/sm4ks" Opcode.Sm4ks
    Shape.(X (X (X (Imm Done))))

let xperm4, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbkx" ] ~src:"rv_zbkx/xperm4" Opcode.Xperm4
    Shape.(X (X (X Done)))

let xperm8, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbkx" ] ~src:"rv_zbkx/xperm8" Opcode.Xperm8
    Shape.(X (X (X Done)))

let sha256sum0, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x100 })
    ~ext:[ "zknh" ] ~src:"rv_zknh/sha256sum0" Opcode.Sha256sum0
    Shape.(X (X Done))

let sha256sum1, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x101 })
    ~ext:[ "zknh" ] ~src:"rv_zknh/sha256sum1" Opcode.Sha256sum1
    Shape.(X (X Done))

let sha256sig0, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x102 })
    ~ext:[ "zknh" ] ~src:"rv_zknh/sha256sig0" Opcode.Sha256sig0
    Shape.(X (X Done))

let sha256sig1, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x103 })
    ~ext:[ "zknh" ] ~src:"rv_zknh/sha256sig1" Opcode.Sha256sig1
    Shape.(X (X Done))

let sha512sum0, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x104 })
    ~ext:[ "zknh" ] ~src:"rv64_zknh/sha512sum0" ~xlen:Rv64 Opcode.Sha512sum0
    Shape.(X (X Done))

let sha512sum1, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x105 })
    ~ext:[ "zknh" ] ~src:"rv64_zknh/sha512sum1" ~xlen:Rv64 Opcode.Sha512sum1
    Shape.(X (X Done))

let sha512sig0, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x106 })
    ~ext:[ "zknh" ] ~src:"rv64_zknh/sha512sig0" ~xlen:Rv64 Opcode.Sha512sig0
    Shape.(X (X Done))

let sha512sig1, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x107 })
    ~ext:[ "zknh" ] ~src:"rv64_zknh/sha512sig1" ~xlen:Rv64 Opcode.Sha512sig1
    Shape.(X (X Done))

let sha512sum0r, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x28; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sum0r" ~xlen:Rv32 Opcode.Sha512sum0r
    Shape.(X (X (X Done)))

let sha512sum1r, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x29; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sum1r" ~xlen:Rv32 Opcode.Sha512sum1r
    Shape.(X (X (X Done)))

let sha512sig0l, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2a; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sig0l" ~xlen:Rv32 Opcode.Sha512sig0l
    Shape.(X (X (X Done)))

let sha512sig1l, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2b; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sig1l" ~xlen:Rv32 Opcode.Sha512sig1l
    Shape.(X (X (X Done)))

let sha512sig0h, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2e; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sig0h" ~xlen:Rv32 Opcode.Sha512sig0h
    Shape.(X (X (X Done)))

let sha512sig1h, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2f; rs2 = None })
    ~ext:[ "zknh" ] ~src:"rv32_zknh/sha512sig1h" ~xlen:Rv32 Opcode.Sha512sig1h
    Shape.(X (X (X Done)))

let aes64ds, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1d; rs2 = None })
    ~ext:[ "zknd" ] ~src:"rv64_zknd/aes64ds" ~xlen:Rv64 Opcode.Aes64ds
    Shape.(X (X (X Done)))

let aes64dsm, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1f; rs2 = None })
    ~ext:[ "zknd" ] ~src:"rv64_zknd/aes64dsm" ~xlen:Rv64 Opcode.Aes64dsm
    Shape.(X (X (X Done)))

let aes64es, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x19; rs2 = None })
    ~ext:[ "zkne" ] ~src:"rv64_zkne/aes64es" ~xlen:Rv64 Opcode.Aes64es
    Shape.(X (X (X Done)))

let aes64esm, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1b; rs2 = None })
    ~ext:[ "zkne" ] ~src:"rv64_zkne/aes64esm" ~xlen:Rv64 Opcode.Aes64esm
    Shape.(X (X (X Done)))

let aes64ks2, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x3f; rs2 = None })
    ~ext:[ "zknd" ] ~src:"rv64_zknd/aes64ks2" ~alt:[ "zknd"; "zkne" ] ~xlen:Rv64 Opcode.Aes64ks2
    Shape.(X (X (X Done)))

let aes64im, _ =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x300 })
    ~ext:[ "zknd" ] ~src:"rv64_zknd/aes64im" ~xlen:Rv64 Opcode.Aes64im
    Shape.(X (X Done))

let aes64ks1i, aes64ks1i_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 1 })
    ~ext:[ "zknd" ] ~src:"rv64_zknd/aes64ks1i" ~alt:[ "zknd"; "zkne" ] ~xlen:Rv64 ~lower:true
    Opcode.Aes64ks1i
    Shape.(X (X (Imm Done)))

let aes32dsi, aes32dsi_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x15 })
    ~ext:[ "zknd" ] ~src:"rv32_zknd/aes32dsi" ~xlen:Rv32 Opcode.Aes32dsi
    Shape.(X (X (X (Imm Done))))

let aes32dsmi, aes32dsmi_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x17 })
    ~ext:[ "zknd" ] ~src:"rv32_zknd/aes32dsmi" ~xlen:Rv32 Opcode.Aes32dsmi
    Shape.(X (X (X (Imm Done))))

let aes32esi, aes32esi_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x11 })
    ~ext:[ "zkne" ] ~src:"rv32_zkne/aes32esi" ~xlen:Rv32 Opcode.Aes32esi
    Shape.(X (X (X (Imm Done))))

let aes32esmi, aes32esmi_e =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x13 })
    ~ext:[ "zkne" ] ~src:"rv32_zkne/aes32esmi" ~xlen:Rv32 Opcode.Aes32esmi
    Shape.(X (X (X (Imm Done))))

let csrrw, csrrw_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 1 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrw" Opcode.Csrrw
    Shape.(X (Imm (X Done)))

let csrrs, csrrs_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrs" Opcode.Csrrs
    Shape.(X (Imm (X Done)))

let csrrc, csrrc_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 3 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrc" Opcode.Csrrc
    Shape.(X (Imm (X Done)))

let csrrwi, csrrwi_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 5 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrwi" Opcode.Csrrwi
    Shape.(X (Imm (Imm Done)))

let csrrsi, csrrsi_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 6 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrsi" Opcode.Csrrsi
    Shape.(X (Imm (Imm Done)))

let csrrci, csrrci_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 7 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrrci" Opcode.Csrrci
    Shape.(X (Imm (Imm Done)))

let csrr, csrr_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrr" Opcode.Csrr
    Shape.(X (Imm Done))

let csrw, csrw_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 1 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrw" Opcode.Csrw
    Shape.(Imm (X Done))

let csrs, csrs_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrs" Opcode.Csrs
    Shape.(Imm (X Done))

let csrc, csrc_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 3 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrc" Opcode.Csrc
    Shape.(Imm (X Done))

let csrwi, csrwi_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 5 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrwi" Opcode.Csrwi
    Shape.(Imm (Imm Done))

let csrsi, csrsi_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 6 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrsi" Opcode.Csrsi
    Shape.(Imm (Imm Done))

let csrci, csrci_e =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 7 })
    ~ext:[ "zicsr" ] ~src:"rv_zicsr/csrci" Opcode.Csrci
    Shape.(Imm (Imm Done))

let amoswap_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x04; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amoswap.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoswap_w
    Shape.(X (X (Mem Done)))

let amoadd_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x00; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amoadd.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoadd_w
    Shape.(X (X (Mem Done)))

let amoxor_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amoxor.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoxor_w
    Shape.(X (X (Mem Done)))

let amoand_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x30; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amoand.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoand_w
    Shape.(X (X (Mem Done)))

let amoor_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x20; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amoor.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoor_w
    Shape.(X (X (Mem Done)))

let amomin_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x40; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amomin.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomin_w
    Shape.(X (X (Mem Done)))

let amomax_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x50; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amomax.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomax_w
    Shape.(X (X (Mem Done)))

let amominu_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x60; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amominu.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amominu_w
    Shape.(X (X (Mem Done)))

let amomaxu_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x70; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/amomaxu.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomaxu_w
    Shape.(X (X (Mem Done)))

let lr_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x08; rs2 = Some 0 })
    ~ext:[ "a" ] ~src:"rv_a/lr.w"
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lr_w
    Shape.(X (Mem Done))

let sc_w, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x0c; rs2 = None })
    ~ext:[ "a" ] ~src:"rv_a/sc.w"
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Sc_w
    Shape.(X (X (Mem Done)))

let amoswap_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x04; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amoswap.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoswap_d
    Shape.(X (X (Mem Done)))

let amoadd_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x00; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amoadd.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoadd_d
    Shape.(X (X (Mem Done)))

let amoxor_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x10; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amoxor.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoxor_d
    Shape.(X (X (Mem Done)))

let amoand_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x30; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amoand.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoand_d
    Shape.(X (X (Mem Done)))

let amoor_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x20; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amoor.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoor_d
    Shape.(X (X (Mem Done)))

let amomin_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x40; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amomin.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomin_d
    Shape.(X (X (Mem Done)))

let amomax_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x50; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amomax.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomax_d
    Shape.(X (X (Mem Done)))

let amominu_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x60; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amominu.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amominu_d
    Shape.(X (X (Mem Done)))

let amomaxu_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x70; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/amomaxu.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomaxu_d
    Shape.(X (X (Mem Done)))

let lr_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x08; rs2 = Some 0 })
    ~ext:[ "a" ] ~src:"rv64_a/lr.d" ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lr_d
    Shape.(X (Mem Done))

let sc_d, _ =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x0c; rs2 = None })
    ~ext:[ "a" ] ~src:"rv64_a/sc.d" ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Sc_d
    Shape.(X (X (Mem Done)))

let addw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Addw
    Shape.(X (X (X Done)))

let subw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x20; rs2 = None })
    ~xlen:Rv64 Opcode.Subw
    Shape.(X (X (X Done)))

let sllw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 1; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sllw
    Shape.(X (X (X Done)))

let srlw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Srlw
    Shape.(X (X (X Done)))

let sraw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x20; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sraw
    Shape.(X (X (X Done)))

let mulw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~src:"rv64_m/mulw" ~xlen:Rv64 Opcode.Mulw
    Shape.(X (X (X Done)))

let divw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv64_m/divw" ~xlen:Rv64 Opcode.Divw
    Shape.(X (X (X Done)))

let divuw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv64_m/divuw" ~xlen:Rv64 Opcode.Divuw
    Shape.(X (X (X Done)))

let remw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 6; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv64_m/remw" ~xlen:Rv64 Opcode.Remw
    Shape.(X (X (X Done)))

let remuw, _ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~src:"rv64_m/remuw" ~xlen:Rv64 Opcode.Remuw
    Shape.(X (X (X Done)))

let addi, addi_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 0 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Addi
    Shape.(X (X (Imm Done)))

let c_addi, c_addi_e =
  def ~cls:Alu ~ext:[ "zca" ] ~src:"rv_c/c.addi" ~lower:true
    ~also:Kind.[ [ X; Sym ] ]
    Opcode.C_addi
    Shape.(X (Imm Done))

let c_and, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 3 })
    ~ext:[ "zca" ] ~src:"rv_c/c.and" Opcode.C_and
    Shape.(Creg (Creg Done))

let c_or, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 2 })
    ~ext:[ "zca" ] ~src:"rv_c/c.or" Opcode.C_or
    Shape.(Creg (Creg Done))

let c_xor, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 1 })
    ~ext:[ "zca" ] ~src:"rv_c/c.xor" Opcode.C_xor
    Shape.(Creg (Creg Done))

let c_sub, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 0 })
    ~ext:[ "zca" ] ~src:"rv_c/c.sub" Opcode.C_sub
    Shape.(Creg (Creg Done))

let c_addw, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x27; funct2 = 1 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.addw" ~xlen:Rv64 Opcode.C_addw
    Shape.(Creg (Creg Done))

let c_subw, _ =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x27; funct2 = 0 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.subw" ~xlen:Rv64 Opcode.C_subw
    Shape.(Creg (Creg Done))

let c_jr, _ =
  def ~cls:Jump ~enc:(Cr { funct4 = 8 }) ~ext:[ "zca" ] ~src:"rv_c/c.jr" Opcode.C_jr Shape.(X Done)

let c_jalr, _ =
  def ~cls:Jump
    ~enc:(Cr { funct4 = 9 })
    ~ext:[ "zca" ] ~src:"rv_c/c.jalr" Opcode.C_jalr
    Shape.(X Done)

let c_mv, _ =
  def ~cls:Alu
    ~enc:(Cr { funct4 = 8 })
    ~ext:[ "zca" ] ~src:"rv_c/c.mv" Opcode.C_mv
    Shape.(X (X Done))

let c_add, _ =
  def ~cls:Alu
    ~enc:(Cr { funct4 = 9 })
    ~ext:[ "zca" ] ~src:"rv_c/c.add" Opcode.C_add
    Shape.(X (X Done))

let c_ebreak, _ =
  def ~cls:System
    ~enc:(Cr { funct4 = 9 })
    ~ext:[ "zca" ] ~src:"rv_c/c.ebreak" Opcode.C_ebreak Shape.Done

let c_lw, _ =
  def ~cls:Load
    ~enc:(C { funct3 = 2 })
    ~ext:[ "zca" ] ~src:"rv_c/c.lw" Opcode.C_lw
    Shape.(Creg (Cmem Done))

let c_sw, _ =
  def ~cls:Store
    ~enc:(C { funct3 = 6 })
    ~ext:[ "zca" ] ~src:"rv_c/c.sw" Opcode.C_sw
    Shape.(Creg (Cmem Done))

let c_lwsp, _ =
  def ~cls:Load
    ~enc:(C { funct3 = 2 })
    ~ext:[ "zca" ] ~src:"rv_c/c.lwsp" Opcode.C_lwsp
    Shape.(X (Spmem Done))

let c_swsp, _ =
  def ~cls:Store
    ~enc:(C { funct3 = 6 })
    ~ext:[ "zca" ] ~src:"rv_c/c.swsp" Opcode.C_swsp
    Shape.(X (Spmem Done))

let c_ld, _ =
  def ~cls:Load
    ~enc:(C { funct3 = 3 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.ld" ~xlen:Rv64 Opcode.C_ld
    Shape.(Creg (Cmem Done))

let c_sd, _ =
  def ~cls:Store
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.sd" ~xlen:Rv64 Opcode.C_sd
    Shape.(Creg (Cmem Done))

let c_ldsp, _ =
  def ~cls:Load
    ~enc:(C { funct3 = 3 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.ldsp" ~xlen:Rv64 Opcode.C_ldsp
    Shape.(X (Spmem Done))

let c_sdsp, _ =
  def ~cls:Store
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ] ~src:"rv64_c/c.sdsp" ~xlen:Rv64 Opcode.C_sdsp
    Shape.(X (Spmem Done))

let c_beqz, _ =
  def ~cls:Branch
    ~enc:(C { funct3 = 6 })
    ~ext:[ "zca" ] ~src:"rv_c/c.beqz"
    ~also:Kind.[ [ Creg; Imm ] ]
    Opcode.C_beqz
    Shape.(Creg (Sym Done))

let c_j, _ =
  def ~cls:Jump
    ~enc:(C { funct3 = 5 })
    ~ext:[ "zca" ] ~src:"rv_c/c.j"
    ~also:Kind.[ [ Imm ] ]
    Opcode.C_j
    Shape.(Sym Done)

let c_jal, _ =
  def ~cls:Jump
    ~enc:(C { funct3 = 1 })
    ~ext:[ "zca" ] ~src:"rv32_c/c.jal" ~xlen:Rv32
    ~also:Kind.[ [ Imm ] ]
    Opcode.C_jal
    Shape.(Sym Done)

let c_bnez, _ =
  def ~cls:Branch
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ] ~src:"rv_c/c.bnez"
    ~also:Kind.[ [ Creg; Imm ] ]
    Opcode.C_bnez
    Shape.(Creg (Sym Done))

let slti, slti_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 2 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slti
    Shape.(X (X (Imm Done)))

let sltiu, sltiu_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 3 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Sltiu
    Shape.(X (X (Imm Done)))

let xori, xori_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 4 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Xori
    Shape.(X (X (Imm Done)))

let ori, ori_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 6 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Ori
    Shape.(X (X (Imm Done)))

let andi, andi_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 7 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Andi
    Shape.(X (X (Imm Done)))

let slli, slli_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x00 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slli
    Shape.(X (X (Imm Done)))

let srli, srli_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x00 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srli
    Shape.(X (X (Imm Done)))

let srai, srai_e =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x10 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srai
    Shape.(X (X (Imm Done)))

let addiw, addiw_e =
  def ~cls:Alu
    ~enc:(I { opcode = 0x1b; funct3 = 0 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Addiw
    Shape.(X (X (Imm Done)))

let slliw, slliw_e =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 1; funct7 = 0x00 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slliw
    Shape.(X (X (Imm Done)))

let srliw, srliw_e =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x00 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srliw
    Shape.(X (X (Imm Done)))

let sraiw, sraiw_e =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x20 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Sraiw
    Shape.(X (X (Imm Done)))

let sext_w, _ = def ~cls:Alu ~xlen:Rv64 ~lower:true Opcode.Sext_w Shape.(X (X Done))

let lb, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 0 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lb
    Shape.(X (Mem Done))

let lh, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 1 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lh
    Shape.(X (Mem Done))

let lw, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 2 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lw
    Shape.(X (Mem Done))

let lbu, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 4 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lbu
    Shape.(X (Mem Done))

let lhu, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 5 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lhu
    Shape.(X (Mem Done))

let lwu, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 6 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lwu
    Shape.(X (Mem Done))

let ld, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 3 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ]; [ X; Sym ] ]
    Opcode.Ld
    Shape.(X (Mem Done))

let sb, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 0 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sb
    Shape.(X (Mem Done))

let sh, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 1 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sh
    Shape.(X (Mem Done))

let sw, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 2 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sw
    Shape.(X (Mem Done))

let sd, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 3 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sd
    Shape.(X (Mem Done))

let beq, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 0 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Beq
    Shape.(X (X (Sym Done)))

let bne, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 1 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Bne
    Shape.(X (X (Sym Done)))

let blt, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 4 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Blt
    Shape.(X (X (Sym Done)))

let bge, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 5 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Bge
    Shape.(X (X (Sym Done)))

let bltu, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 6 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Bltu
    Shape.(X (X (Sym Done)))

let bgeu, _ =
  def ~cls:Branch
    ~enc:(B { funct3 = 7 })
    ~also:Kind.[ [ X; X; Imm ] ]
    Opcode.Bgeu
    Shape.(X (X (Sym Done)))

let bgtu, _ =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Imm ] ] Opcode.Bgtu Shape.(X (X (Sym Done)))

let bleu, _ =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Imm ] ] Opcode.Bleu Shape.(X (X (Sym Done)))

let bgt, _ =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Imm ] ] Opcode.Bgt Shape.(X (X (Sym Done)))

let ble, _ =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Imm ] ] Opcode.Ble Shape.(X (X (Sym Done)))

let beqz, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Beqz Shape.(X (Sym Done))
let bnez, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Bnez Shape.(X (Sym Done))
let bgez, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Bgez Shape.(X (Sym Done))
let bltz, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Bltz Shape.(X (Sym Done))
let blez, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Blez Shape.(X (Sym Done))
let bgtz, _ = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Bgtz Shape.(X (Sym Done))

let lui, lui_e =
  def ~cls:Alu ~enc:(U { opcode = 0x37 }) ~also:Kind.[ [ X; Sym ] ] Opcode.Lui Shape.(X (Imm Done))

let auipc, auipc_e =
  def ~cls:Alu
    ~enc:(U { opcode = 0x17 })
    ~also:Kind.[ [ X; Sym ] ]
    Opcode.Auipc
    Shape.(X (Imm Done))

let jal, _ =
  def ~cls:Jump ~lower:true
    ~also:Kind.[ [ X; Imm ]; [ X; Sym ]; [ Imm ] ]
    Opcode.Jal
    Shape.(Sym Done)

let jalr, _ =
  def ~cls:Jump
    ~enc:(I { opcode = 0x67; funct3 = 0 })
    ~also:Kind.[ [ X; Mem ]; [ X; Spmem ]; [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Jalr
    Shape.(X Done)

let mv, _ = def ~cls:Alu ~lower:true Opcode.Mv Shape.(X (X Done))
let nop, _ = def ~cls:Alu ~lower:true Opcode.Nop Shape.Done
let j, _ = def ~cls:Jump ~lower:true ~also:Kind.[ [ Imm ] ] Opcode.J Shape.(Sym Done)
let jr, _ = def ~cls:Jump ~lower:true Opcode.Jr Shape.(X Done)
let call, _ = def ~cls:Jump ~lower:true ~also:Kind.[ [ Imm ] ] Opcode.Call Shape.(Sym Done)
let tail, _ = def ~cls:Jump ~lower:true ~also:Kind.[ [ Imm ] ] Opcode.Tail Shape.(Sym Done)
let la, _ = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.La Shape.(X (Sym Done))
let lla, _ = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Imm ] ] Opcode.Lla Shape.(X (Sym Done))
let li, li_e = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Li Shape.(X (Imm64 Done))
let ret, _ = def ~cls:Jump ~lower:true Opcode.Ret Shape.Done
let ecall, _ = def ~cls:System ~lower:true Opcode.Ecall Shape.Done
let ebreak, _ = def ~cls:System ~lower:true Opcode.Ebreak Shape.Done
let unimp, _ = def ~cls:Csr ~lower:true Opcode.Unimp Shape.Done

let fence_i, _ =
  def ~cls:System ~ext:[ "zifencei" ] ~src:"rv_zifencei/fence.i" ~lower:true Opcode.Fence_i
    Shape.Done

let fence, _ = def ~cls:System ~lower:true Opcode.Fence Shape.Done
let fence_tso, _ = def ~cls:System ~lower:true Opcode.Fence_tso Shape.Done
let pause, _ = def ~cls:System ~lower:true Opcode.Pause Shape.Done

let fld, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x07; funct3 = 3 })
    ~ext:[ "d" ] ~src:"rv_d/fld"
    ~also:Kind.[ [ F; Spmem ]; [ F; Sym; X ] ]
    Opcode.Fld
    Shape.(F (Mem Done))

let flw, _ =
  def ~cls:Load
    ~enc:(I { opcode = 0x07; funct3 = 2 })
    ~ext:[ "f" ] ~src:"rv_f/flw"
    ~also:Kind.[ [ F; Spmem ]; [ F; Sym; X ] ]
    Opcode.Flw
    Shape.(F (Mem Done))

let fsd, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x27; funct3 = 3 })
    ~ext:[ "d" ] ~src:"rv_d/fsd"
    ~also:Kind.[ [ F; Spmem ] ]
    Opcode.Fsd
    Shape.(F (Mem Done))

let fsw, _ =
  def ~cls:Store
    ~enc:(S { opcode = 0x27; funct3 = 2 })
    ~ext:[ "f" ] ~src:"rv_f/fsw"
    ~also:Kind.[ [ F; Spmem ] ]
    Opcode.Fsw
    Shape.(F (Mem Done))

let fadd_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fadd.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fadd_d
    Shape.(F (F (F Done)))

let fadd_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x00; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fadd.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fadd_s
    Shape.(F (F (F Done)))

let fsub_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x05; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsub.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fsub_d
    Shape.(F (F (F Done)))

let fsub_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x04; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsub.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fsub_s
    Shape.(F (F (F Done)))

let fmul_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x09; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fmul.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fmul_d
    Shape.(F (F (F Done)))

let fmul_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x08; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fmul.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fmul_s
    Shape.(F (F (F Done)))

let fdiv_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x0d; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fdiv.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fdiv_d
    Shape.(F (F (F Done)))

let fdiv_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x0c; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fdiv.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fdiv_s
    Shape.(F (F (F Done)))

let fneg_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnjn.d" Opcode.Fneg_d
    Shape.(F (F Done))

let fneg_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnjn.s" Opcode.Fneg_s
    Shape.(F (F Done))

let fabs_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnjx.d" Opcode.Fabs_d
    Shape.(F (F Done))

let fabs_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnjx.s" Opcode.Fabs_s
    Shape.(F (F Done))

let fmv_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnj.s" Opcode.Fmv_s
    Shape.(F (F Done))

let fmv_x_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmv.x.w" Opcode.Fmv_x_s
    Shape.(X (F Done))

let fmv_s_x, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x78; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmv.w.x" Opcode.Fmv_s_x
    Shape.(F (X Done))

let fsgnj_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnj.s" Opcode.Fsgnj_s
    Shape.(F (F (F Done)))

let fsgnj_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnj.d" Opcode.Fsgnj_d
    Shape.(F (F (F Done)))

let fsgnjn_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnjn.s" Opcode.Fsgnjn_s
    Shape.(F (F (F Done)))

let fsgnjn_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnjn.d" Opcode.Fsgnjn_d
    Shape.(F (F (F Done)))

let fsgnjx_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fsgnjx.s" Opcode.Fsgnjx_s
    Shape.(F (F (F Done)))

let fsgnjx_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnjx.d" Opcode.Fsgnjx_d
    Shape.(F (F (F Done)))

let fmin_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x14; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fmin.s" Opcode.Fmin_s
    Shape.(F (F (F Done)))

let fmax_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x14; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fmax.s" Opcode.Fmax_s
    Shape.(F (F (F Done)))

let fmin_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x15; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fmin.d" Opcode.Fmin_d
    Shape.(F (F (F Done)))

let fmax_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x15; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fmax.d" Opcode.Fmax_d
    Shape.(F (F (F Done)))

let fsqrt_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x2c; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fsqrt.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fsqrt_s
    Shape.(F (F Done))

let fsqrt_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x2d; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fsqrt.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fsqrt_d
    Shape.(F (F Done))

let fclass_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fclass.s" Opcode.Fclass_s
    Shape.(X (F Done))

let fclass_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x71; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fclass.d" Opcode.Fclass_d
    Shape.(X (F Done))

let fmadd_s, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x43; fmt = 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmadd.s" Opcode.Fmadd_s
    Shape.(F (F (F (F Done))))

let fmadd_d, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x43; fmt = 1 })
    ~ext:[ "d" ] ~src:"rv_d/fmadd.d" Opcode.Fmadd_d
    Shape.(F (F (F (F Done))))

let fmsub_s, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x47; fmt = 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmsub.s" Opcode.Fmsub_s
    Shape.(F (F (F (F Done))))

let fmsub_d, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x47; fmt = 1 })
    ~ext:[ "d" ] ~src:"rv_d/fmsub.d" Opcode.Fmsub_d
    Shape.(F (F (F (F Done))))

let fnmsub_s, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4b; fmt = 0 })
    ~ext:[ "f" ] ~src:"rv_f/fnmsub.s" Opcode.Fnmsub_s
    Shape.(F (F (F (F Done))))

let fnmsub_d, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4b; fmt = 1 })
    ~ext:[ "d" ] ~src:"rv_d/fnmsub.d" Opcode.Fnmsub_d
    Shape.(F (F (F (F Done))))

let fnmadd_s, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4f; fmt = 0 })
    ~ext:[ "f" ] ~src:"rv_f/fnmadd.s" Opcode.Fnmadd_s
    Shape.(F (F (F (F Done))))

let fnmadd_d, _ =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4f; fmt = 1 })
    ~ext:[ "d" ] ~src:"rv_d/fnmadd.d" Opcode.Fnmadd_d
    Shape.(F (F (F (F Done))))

let fmv_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fsgnj.d" Opcode.Fmv_d
    Shape.(F (F Done))

let fmv_x_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x71; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fmv.x.d" ~xlen:Rv64 Opcode.Fmv_x_d
    Shape.(X (F Done))

let feq_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/feq.d" Opcode.Feq_d
    Shape.(X (F (F Done)))

let fle_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/fle.d" Opcode.Fle_d
    Shape.(X (F (F Done)))

let flt_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] ~src:"rv_d/flt.d" Opcode.Flt_d
    Shape.(X (F (F Done)))

let flt_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/flt.s" Opcode.Flt_s
    Shape.(X (F (F Done)))

let feq_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/feq.s" Opcode.Feq_s
    Shape.(X (F (F Done)))

let fle_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] ~src:"rv_f/fle.s" Opcode.Fle_s
    Shape.(X (F (F Done)))

let fmv_x_w, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmv.x.w" Opcode.Fmv_x_w
    Shape.(X (F Done))

let fmv_w_x, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x78; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fmv.w.x" Opcode.Fmv_w_x
    Shape.(F (X Done))

let fcvt_w_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.w.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_w_d
    Shape.(X (F Done))

let fcvt_wu_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 1 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.wu.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_wu_d
    Shape.(X (F Done))

let fcvt_l_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 2 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.l.d" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_l_d
    Shape.(X (F Done))

let fcvt_d_w, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x69; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.d.w" ~rm:Rm.Rne
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_w
    Shape.(F (X Done))

let fcvt_d_wu, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x69; rs2 = Some 1 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.d.wu" ~rm:Rm.Rne
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_wu
    Shape.(F (X Done))

let fcvt_s_w, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.s.w" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_w
    Shape.(F (X Done))

let fcvt_s_wu, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 1 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.s.wu" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_wu
    Shape.(F (X Done))

let fcvt_w_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 0 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.w.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_w_s
    Shape.(X (F Done))

let fcvt_wu_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 1 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.wu.s" ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_wu_s
    Shape.(X (F Done))

let fcvt_s_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x20; rs2 = Some 1 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.s.d" ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fcvt_s_d
    Shape.(F (F Done))

let fcvt_d_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x21; rs2 = Some 0 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.d.s" ~rm:Rm.Rne
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fcvt_d_s
    Shape.(F (F Done))

let fcvt_s_l, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 2 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.s.l" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_l
    Shape.(F (X Done))

let fcvt_lu_d, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 3 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.lu.d" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_lu_d
    Shape.(X (F Done))

let fcvt_d_l, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x69; rs2 = Some 2 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.d.l" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_l
    Shape.(F (X Done))

let fcvt_d_lu, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x69; rs2 = Some 3 })
    ~ext:[ "d" ] ~src:"rv_d/fcvt.d.lu" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_lu
    Shape.(F (X Done))

let fcvt_l_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 2 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.l.s" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_l_s
    Shape.(X (F Done))

let fcvt_lu_s, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 3 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.lu.s" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_lu_s
    Shape.(X (F Done))

let fcvt_s_lu, _ =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 3 })
    ~ext:[ "f" ] ~src:"rv_f/fcvt.s.lu" ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_lu
    Shape.(F (X Done))

(* Every definition, in [Opcode.all]'s order. *)
let all = Array.of_list (List.rev !registry)

let by_op =
  let t = Hashtbl.create 512 in
  Array.iter (fun e -> Hashtbl.replace t e.op e) all;
  t

let find op = Hashtbl.find_opt by_op op

(* The hand-written forms a component owns, with their riscv-opcodes records, in table order. *)
let forms feature =
  List.filter_map
    (fun e -> match e.src with Some s when List.mem feature e.ext -> Some (e.op, s) | _ -> None)
    (Array.to_list all)

(* The spellings several components provide, and those components. *)
let alternatives =
  List.filter_map
    (fun e -> if e.alt = [] then None else Some (Opcode.mnemonic e.op, e.alt))
    (Array.to_list all)
