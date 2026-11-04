(* One definition per hand-written RISC-V opcode: the operands it takes, its encoding, the
   components that publish it, the XLEN it exists for, its class, the rounding mode it takes when
   none is written, and whether its lowering stays code (pseudo expansions and irregular forms).
   Each definition is also the opcode's typed builder. The encoder does not read it yet: a test
   holds it to the encoder's own tables until each consumer switches to it. *)

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

(* A builder's operands, in the text's order: ['f] is the builder's type, ['r] its result. *)
module Shape = struct
  type ('f, 'r) t =
    | Done : ('r, 'r) t
    | X : ('f, 'r) t -> (Xreg.t -> 'f, 'r) t
    | F : ('f, 'r) t -> (Freg.t -> 'f, 'r) t
    | V : ('f, 'r) t -> (Vreg.t -> 'f, 'r) t
    | Creg : ('f, 'r) t -> (Creg.t -> 'f, 'r) t
    | Imm : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Mem : ('f, 'r) t -> (Mem.t -> 'f, 'r) t
    | Cmem : ('f, 'r) t -> (Mem.t -> 'f, 'r) t
    | Spmem : ('f, 'r) t -> (Mem.t -> 'f, 'r) t
    | Sym : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Rm : ('f, 'r) t -> (Rm.t -> 'f, 'r) t

  let rec kinds : type f r. (f, r) t -> Kind.t list = function
    | Done -> []
    | X s -> Kind.X :: kinds s
    | F s -> Kind.F :: kinds s
    | V s -> Kind.V :: kinds s
    | Creg s -> Kind.Creg :: kinds s
    | Imm s -> Kind.Imm :: kinds s
    | Mem s -> Kind.Mem :: kinds s
    | Cmem s -> Kind.Cmem :: kinds s
    | Spmem s -> Kind.Spmem :: kinds s
    | Sym s -> Kind.Sym :: kinds s
    | Rm s -> Kind.Rm :: kinds s

  let imm = function Asm_core.Expr.Const n -> Operand.Imm n | e -> Operand.Sym e

  let rec build : type f r. (f, r) t -> (Operand.t list -> r) -> f =
   fun s k ->
    let next s o = build s (fun ops -> k (o :: ops)) in
    match s with
    | Done -> k []
    | X s -> fun r -> next s (Operand.Reg (Reg.X r))
    | F s -> fun r -> next s (Operand.Reg (Reg.F r))
    | V s -> fun r -> next s (Operand.Reg (Reg.V r))
    | Creg s -> fun r -> next s (Operand.Reg (Reg.X (Riscv_types.Creg.to_xreg r)))
    | Imm s -> fun e -> next s (imm e)
    | Mem s -> fun m -> next s (Operand.Mem m)
    | Cmem s -> fun m -> next s (Operand.Mem m)
    | Spmem s -> fun m -> next s (Operand.Mem m)
    | Sym s -> fun e -> next s (Operand.Sym e)
    | Rm s -> fun r -> next s (Operand.Sym (Asm_core.Expr.Symbol (Riscv_types.Rm.name r)))
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
  xlen : xlen;
  cls : Cls.t;
  rm : Rm.t option;
  lower : bool;  (** lowering stays code *)
}

let registry = ref []

let def ?(also = []) ?enc ?enc64 ?(ext = []) ?(xlen = Any) ?rm ?(lower = false) ~cls op shape =
  registry :=
    { op; shape = Shape.kinds shape; also; enc; enc64; ext; xlen; cls; rm; lower } :: !registry;
  Shape.build shape (fun ops -> (op, ops))

let add =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Add
    Shape.(X (X (X Done)))

let sub =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x20; rs2 = None })
    Opcode.Sub
    Shape.(X (X (X Done)))

let sll =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sll
    Shape.(X (X (X Done)))

let slt =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Slt
    Shape.(X (X (X Done)))

let sltu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sltu
    Shape.(X (X (X Done)))

let snez = def ~cls:Alu ~lower:true Opcode.Snez Shape.(X (X Done))
let neg = def ~cls:Alu ~lower:true Opcode.Neg Shape.(X (X Done))
let seqz = def ~cls:Alu ~lower:true Opcode.Seqz Shape.(X (X Done))
let sltz = def ~cls:Alu ~lower:true Opcode.Sltz Shape.(X (X Done))
let sgtz = def ~cls:Alu ~lower:true Opcode.Sgtz Shape.(X (X Done))
let zext_b = def ~cls:Alu ~lower:true Opcode.Zext_b Shape.(X (X Done))

let xor =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Xor
    Shape.(X (X (X Done)))

let srl =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Srl
    Shape.(X (X (X Done)))

let sra =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x20; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sra
    Shape.(X (X (X Done)))

let or_ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Or
    Shape.(X (X (X Done)))

let and_ =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x00; rs2 = None })
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.And
    Shape.(X (X (X Done)))

let mul =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] Opcode.Mul
    Shape.(X (X (X Done)))

let remu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] Opcode.Remu
    Shape.(X (X (X Done)))

let mulh =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] Opcode.Mulh
    Shape.(X (X (X Done)))

let mulhsu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] Opcode.Mulhsu
    Shape.(X (X (X Done)))

let mulhu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] Opcode.Mulhu
    Shape.(X (X (X Done)))

let div =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] Opcode.Div
    Shape.(X (X (X Done)))

let divu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] Opcode.Divu
    Shape.(X (X (X Done)))

let rem =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] Opcode.Rem
    Shape.(X (X (X Done)))

let sh1add =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] Opcode.Sh1add
    Shape.(X (X (X Done)))

let sh2add =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] Opcode.Sh2add
    Shape.(X (X (X Done)))

let sh3add =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] Opcode.Sh3add
    Shape.(X (X (X Done)))

let min =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Min
    Shape.(X (X (X Done)))

let minu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Minu
    Shape.(X (X (X Done)))

let max =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Max
    Shape.(X (X (X Done)))

let maxu =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Maxu
    Shape.(X (X (X Done)))

let andn =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Andn
    Shape.(X (X (X Done)))

let orn =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 6; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Orn
    Shape.(X (X (X Done)))

let xnor =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x20; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Xnor
    Shape.(X (X (X Done)))

let rol =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] Opcode.Rol
    Shape.(X (X (X Done)))

let ror =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Ror
    Shape.(X (X (X Done)))

let bclr =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x24; rs2 = None })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bclr
    Shape.(X (X (X Done)))

let bext =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x24; rs2 = None })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bext
    Shape.(X (X (X Done)))

let binv =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x34; rs2 = None })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Binv
    Shape.(X (X (X Done)))

let bset =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Bset
    Shape.(X (X (X Done)))

let bclri =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x12 })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bclri
    Shape.(X (X (Imm Done)))

let bexti =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x12 })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bexti
    Shape.(X (X (Imm Done)))

let binvi =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x1a })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Binvi
    Shape.(X (X (Imm Done)))

let bseti =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x0a })
    ~ext:[ "zbs" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bseti
    Shape.(X (X (Imm Done)))

let sh1add_uw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~xlen:Rv64 Opcode.Sh1adduw
    Shape.(X (X (X Done)))

let sh2add_uw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~xlen:Rv64 Opcode.Sh2adduw
    Shape.(X (X (X Done)))

let sh3add_uw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 6; funct7 = 0x10; rs2 = None })
    ~ext:[ "zba" ] ~xlen:Rv64 Opcode.Sh3adduw
    Shape.(X (X (X Done)))

let clz =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x600 })
    ~ext:[ "zbb" ] Opcode.Clz
    Shape.(X (X Done))

let ctz =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x601 })
    ~ext:[ "zbb" ] Opcode.Ctz
    Shape.(X (X Done))

let cpop =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x602 })
    ~ext:[ "zbb" ] Opcode.Cpop
    Shape.(X (X Done))

let sext_b =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x604 })
    ~ext:[ "zbb" ] Opcode.Sextb
    Shape.(X (X Done))

let sext_h =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x605 })
    ~ext:[ "zbb" ] Opcode.Sexth
    Shape.(X (X Done))

let orc_b =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x287 })
    ~ext:[ "zbb" ] Opcode.Orcb
    Shape.(X (X Done))

let clzw =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x600 })
    ~ext:[ "zbb" ] ~xlen:Rv64 Opcode.Clzw
    Shape.(X (X Done))

let ctzw =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x601 })
    ~ext:[ "zbb" ] ~xlen:Rv64 Opcode.Ctzw
    Shape.(X (X Done))

let cpopw =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x1b; funct3 = 1; funct12 = 0x602 })
    ~ext:[ "zbb" ] ~xlen:Rv64 Opcode.Cpopw
    Shape.(X (X Done))

let brev8 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x687 })
    ~ext:[ "zbkb" ] Opcode.Brev8
    Shape.(X (X Done))

let rev8 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x698 })
    ~enc64:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x6b8 })
    ~ext:[ "zbb" ] Opcode.Rev8
    Shape.(X (X Done))

let pack =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] Opcode.Pack
    Shape.(X (X (X Done)))

let packh =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] Opcode.Packh
    Shape.(X (X (X Done)))

let packw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x04; rs2 = None })
    ~ext:[ "zbkb" ] ~xlen:Rv64 Opcode.Packw
    Shape.(X (X (X Done)))

let zip =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x08f })
    ~ext:[ "zbkb" ] ~xlen:Rv32 Opcode.Zip
    Shape.(X (X Done))

let unzip =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 5; funct12 = 0x08f })
    ~ext:[ "zbkb" ] ~xlen:Rv32 Opcode.Unzip
    Shape.(X (X Done))

let rolw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 1; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~xlen:Rv64 Opcode.Rolw
    Shape.(X (X (X Done)))

let rorw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x30; rs2 = None })
    ~ext:[ "zbb" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Rorw
    Shape.(X (X (X Done)))

let rori =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x18 })
    ~ext:[ "zbb" ]
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Rori
    Shape.(X (X (Imm Done)))

let roriw =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x30 })
    ~ext:[ "zbb" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Roriw
    Shape.(X (X (Imm Done)))

let zext_h = def ~cls:Alu ~ext:[ "zbb" ] ~lower:true Opcode.Zext_h Shape.(X (X Done))

let clmul =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 1; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] Opcode.Clmul
    Shape.(X (X (X Done)))

let clmulh =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 3; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] Opcode.Clmulh
    Shape.(X (X (X Done)))

let clmulr =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x05; rs2 = None })
    ~ext:[ "zbc" ] Opcode.Clmulr
    Shape.(X (X (X Done)))

let czero_eqz =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 5; funct7 = 0x07; rs2 = None })
    ~ext:[ "zicond" ] Opcode.Czero_eqz
    Shape.(X (X (X Done)))

let czero_nez =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 7; funct7 = 0x07; rs2 = None })
    ~ext:[ "zicond" ] Opcode.Czero_nez
    Shape.(X (X (X Done)))

let sm3p0 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x108 })
    ~ext:[ "zksh" ] Opcode.Sm3p0
    Shape.(X (X Done))

let sm3p1 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x109 })
    ~ext:[ "zksh" ] Opcode.Sm3p1
    Shape.(X (X Done))

let sm4ed =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x18 })
    ~ext:[ "zksed" ] Opcode.Sm4ed
    Shape.(X (X (X (Imm Done))))

let sm4ks =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x1a })
    ~ext:[ "zksed" ] Opcode.Sm4ks
    Shape.(X (X (X (Imm Done))))

let xperm4 =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 2; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbkx" ] Opcode.Xperm4
    Shape.(X (X (X Done)))

let xperm8 =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 4; funct7 = 0x14; rs2 = None })
    ~ext:[ "zbkx" ] Opcode.Xperm8
    Shape.(X (X (X Done)))

let sha256sum0 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x100 })
    ~ext:[ "zknh" ] Opcode.Sha256sum0
    Shape.(X (X Done))

let sha256sum1 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x101 })
    ~ext:[ "zknh" ] Opcode.Sha256sum1
    Shape.(X (X Done))

let sha256sig0 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x102 })
    ~ext:[ "zknh" ] Opcode.Sha256sig0
    Shape.(X (X Done))

let sha256sig1 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x103 })
    ~ext:[ "zknh" ] Opcode.Sha256sig1
    Shape.(X (X Done))

let sha512sum0 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x104 })
    ~ext:[ "zknh" ] ~xlen:Rv64 Opcode.Sha512sum0
    Shape.(X (X Done))

let sha512sum1 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x105 })
    ~ext:[ "zknh" ] ~xlen:Rv64 Opcode.Sha512sum1
    Shape.(X (X Done))

let sha512sig0 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x106 })
    ~ext:[ "zknh" ] ~xlen:Rv64 Opcode.Sha512sig0
    Shape.(X (X Done))

let sha512sig1 =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x107 })
    ~ext:[ "zknh" ] ~xlen:Rv64 Opcode.Sha512sig1
    Shape.(X (X Done))

let sha512sum0r =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x28; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sum0r
    Shape.(X (X (X Done)))

let sha512sum1r =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x29; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sum1r
    Shape.(X (X (X Done)))

let sha512sig0l =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2a; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sig0l
    Shape.(X (X (X Done)))

let sha512sig1l =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2b; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sig1l
    Shape.(X (X (X Done)))

let sha512sig0h =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2e; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sig0h
    Shape.(X (X (X Done)))

let sha512sig1h =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x2f; rs2 = None })
    ~ext:[ "zknh" ] ~xlen:Rv32 Opcode.Sha512sig1h
    Shape.(X (X (X Done)))

let aes64ds =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1d; rs2 = None })
    ~ext:[ "zknd" ] ~xlen:Rv64 Opcode.Aes64ds
    Shape.(X (X (X Done)))

let aes64dsm =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1f; rs2 = None })
    ~ext:[ "zknd" ] ~xlen:Rv64 Opcode.Aes64dsm
    Shape.(X (X (X Done)))

let aes64es =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x19; rs2 = None })
    ~ext:[ "zkne" ] ~xlen:Rv64 Opcode.Aes64es
    Shape.(X (X (X Done)))

let aes64esm =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x1b; rs2 = None })
    ~ext:[ "zkne" ] ~xlen:Rv64 Opcode.Aes64esm
    Shape.(X (X (X Done)))

let aes64ks2 =
  def ~cls:Alu
    ~enc:(R { opcode = 0x33; funct3 = 0; funct7 = 0x3f; rs2 = None })
    ~ext:[ "zknd" ] ~xlen:Rv64 Opcode.Aes64ks2
    Shape.(X (X (X Done)))

let aes64im =
  def ~cls:Alu
    ~enc:(Unary { opcode = 0x13; funct3 = 1; funct12 = 0x300 })
    ~ext:[ "zknd" ] ~xlen:Rv64 Opcode.Aes64im
    Shape.(X (X Done))

let aes64ks1i =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 1 })
    ~ext:[ "zknd" ] ~xlen:Rv64 ~lower:true Opcode.Aes64ks1i
    Shape.(X (X (Imm Done)))

let aes32dsi =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x15 })
    ~ext:[ "zknd" ] ~xlen:Rv32 Opcode.Aes32dsi
    Shape.(X (X (X (Imm Done))))

let aes32dsmi =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x17 })
    ~ext:[ "zknd" ] ~xlen:Rv32 Opcode.Aes32dsmi
    Shape.(X (X (X (Imm Done))))

let aes32esi =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x11 })
    ~ext:[ "zkne" ] ~xlen:Rv32 Opcode.Aes32esi
    Shape.(X (X (X (Imm Done))))

let aes32esmi =
  def ~cls:Alu
    ~enc:(Rbs { funct7 = 0x13 })
    ~ext:[ "zkne" ] ~xlen:Rv32 Opcode.Aes32esmi
    Shape.(X (X (X (Imm Done))))

let csrrw =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 1 })
    ~ext:[ "zicsr" ] Opcode.Csrrw
    Shape.(X (Imm (X Done)))

let csrrs =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] Opcode.Csrrs
    Shape.(X (Imm (X Done)))

let csrrc =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 3 })
    ~ext:[ "zicsr" ] Opcode.Csrrc
    Shape.(X (Imm (X Done)))

let csrrwi =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 5 })
    ~ext:[ "zicsr" ] Opcode.Csrrwi
    Shape.(X (Imm (Imm Done)))

let csrrsi =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 6 })
    ~ext:[ "zicsr" ] Opcode.Csrrsi
    Shape.(X (Imm (Imm Done)))

let csrrci =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 7 })
    ~ext:[ "zicsr" ] Opcode.Csrrci
    Shape.(X (Imm (Imm Done)))

let csrr =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] Opcode.Csrr
    Shape.(X (Imm Done))

let csrw =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 1 })
    ~ext:[ "zicsr" ] Opcode.Csrw
    Shape.(Imm (X Done))

let csrs =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 2 })
    ~ext:[ "zicsr" ] Opcode.Csrs
    Shape.(Imm (X Done))

let csrc =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 3 })
    ~ext:[ "zicsr" ] Opcode.Csrc
    Shape.(Imm (X Done))

let csrwi =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 5 })
    ~ext:[ "zicsr" ] Opcode.Csrwi
    Shape.(Imm (Imm Done))

let csrsi =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 6 })
    ~ext:[ "zicsr" ] Opcode.Csrsi
    Shape.(Imm (Imm Done))

let csrci =
  def ~cls:Csr
    ~enc:(I { opcode = 0x73; funct3 = 7 })
    ~ext:[ "zicsr" ] Opcode.Csrci
    Shape.(Imm (Imm Done))

let amoswap_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x04; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoswap_w
    Shape.(X (X (Mem Done)))

let amoadd_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x00; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoadd_w
    Shape.(X (X (Mem Done)))

let amoxor_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoxor_w
    Shape.(X (X (Mem Done)))

let amoand_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x30; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoand_w
    Shape.(X (X (Mem Done)))

let amoor_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x20; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoor_w
    Shape.(X (X (Mem Done)))

let amomin_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x40; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomin_w
    Shape.(X (X (Mem Done)))

let amomax_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x50; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomax_w
    Shape.(X (X (Mem Done)))

let amominu_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x60; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amominu_w
    Shape.(X (X (Mem Done)))

let amomaxu_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x70; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomaxu_w
    Shape.(X (X (Mem Done)))

let lr_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x08; rs2 = Some 0 })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lr_w
    Shape.(X (Mem Done))

let sc_w =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 2; funct7 = 0x0c; rs2 = None })
    ~ext:[ "a" ]
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Sc_w
    Shape.(X (X (Mem Done)))

let amoswap_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x04; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoswap_d
    Shape.(X (X (Mem Done)))

let amoadd_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x00; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoadd_d
    Shape.(X (X (Mem Done)))

let amoxor_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x10; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoxor_d
    Shape.(X (X (Mem Done)))

let amoand_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x30; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoand_d
    Shape.(X (X (Mem Done)))

let amoor_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x20; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amoor_d
    Shape.(X (X (Mem Done)))

let amomin_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x40; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomin_d
    Shape.(X (X (Mem Done)))

let amomax_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x50; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomax_d
    Shape.(X (X (Mem Done)))

let amominu_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x60; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amominu_d
    Shape.(X (X (Mem Done)))

let amomaxu_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x70; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Amomaxu_d
    Shape.(X (X (Mem Done)))

let lr_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x08; rs2 = Some 0 })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lr_d
    Shape.(X (Mem Done))

let sc_d =
  def ~cls:Atomic
    ~enc:(R { opcode = 0x2f; funct3 = 3; funct7 = 0x0c; rs2 = None })
    ~ext:[ "a" ] ~xlen:Rv64
    ~also:Kind.[ [ X; X; Spmem ] ]
    Opcode.Sc_d
    Shape.(X (X (Mem Done)))

let addw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Addw
    Shape.(X (X (X Done)))

let subw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x20; rs2 = None })
    ~xlen:Rv64 Opcode.Subw
    Shape.(X (X (X Done)))

let sllw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 1; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sllw
    Shape.(X (X (X Done)))

let srlw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x00; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Srlw
    Shape.(X (X (X Done)))

let sraw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x20; rs2 = None })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Sraw
    Shape.(X (X (X Done)))

let mulw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 0; funct7 = 0x01; rs2 = None })
    ~ext:[ "zmmul" ] ~xlen:Rv64 Opcode.Mulw
    Shape.(X (X (X Done)))

let divw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 4; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~xlen:Rv64 Opcode.Divw
    Shape.(X (X (X Done)))

let divuw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 5; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~xlen:Rv64 Opcode.Divuw
    Shape.(X (X (X Done)))

let remw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 6; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~xlen:Rv64 Opcode.Remw
    Shape.(X (X (X Done)))

let remuw =
  def ~cls:Alu
    ~enc:(R { opcode = 0x3b; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "m" ] ~xlen:Rv64 Opcode.Remuw
    Shape.(X (X (X Done)))

let addi =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 0 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Addi
    Shape.(X (X (Imm Done)))

let c_addi =
  def ~cls:Alu ~ext:[ "zca" ] ~lower:true
    ~also:Kind.[ [ X; Sym ] ]
    Opcode.C_addi
    Shape.(X (Imm Done))

let c_and =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 3 })
    ~ext:[ "zca" ] Opcode.C_and
    Shape.(Creg (Creg Done))

let c_or =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 2 })
    ~ext:[ "zca" ] Opcode.C_or
    Shape.(Creg (Creg Done))

let c_xor =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 1 })
    ~ext:[ "zca" ] Opcode.C_xor
    Shape.(Creg (Creg Done))

let c_sub =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x23; funct2 = 0 })
    ~ext:[ "zca" ] Opcode.C_sub
    Shape.(Creg (Creg Done))

let c_addw =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x27; funct2 = 1 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_addw
    Shape.(Creg (Creg Done))

let c_subw =
  def ~cls:Alu
    ~enc:(Ca { funct6 = 0x27; funct2 = 0 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_subw
    Shape.(Creg (Creg Done))

let c_jr = def ~cls:Jump ~enc:(Cr { funct4 = 8 }) ~ext:[ "zca" ] Opcode.C_jr Shape.(X Done)
let c_jalr = def ~cls:Jump ~enc:(Cr { funct4 = 9 }) ~ext:[ "zca" ] Opcode.C_jalr Shape.(X Done)
let c_mv = def ~cls:Alu ~enc:(Cr { funct4 = 8 }) ~ext:[ "zca" ] Opcode.C_mv Shape.(X (X Done))
let c_add = def ~cls:Alu ~enc:(Cr { funct4 = 9 }) ~ext:[ "zca" ] Opcode.C_add Shape.(X (X Done))
let c_ebreak = def ~cls:System ~enc:(Cr { funct4 = 9 }) ~ext:[ "zca" ] Opcode.C_ebreak Shape.Done
let c_lw = def ~cls:Load ~enc:(C { funct3 = 2 }) ~ext:[ "zca" ] Opcode.C_lw Shape.(Creg (Cmem Done))

let c_sw =
  def ~cls:Store ~enc:(C { funct3 = 6 }) ~ext:[ "zca" ] Opcode.C_sw Shape.(Creg (Cmem Done))

let c_lwsp =
  def ~cls:Load ~enc:(C { funct3 = 2 }) ~ext:[ "zca" ] Opcode.C_lwsp Shape.(X (Spmem Done))

let c_swsp =
  def ~cls:Store ~enc:(C { funct3 = 6 }) ~ext:[ "zca" ] Opcode.C_swsp Shape.(X (Spmem Done))

let c_ld =
  def ~cls:Load
    ~enc:(C { funct3 = 3 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_ld
    Shape.(Creg (Cmem Done))

let c_sd =
  def ~cls:Store
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_sd
    Shape.(Creg (Cmem Done))

let c_ldsp =
  def ~cls:Load
    ~enc:(C { funct3 = 3 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_ldsp
    Shape.(X (Spmem Done))

let c_sdsp =
  def ~cls:Store
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ] ~xlen:Rv64 Opcode.C_sdsp
    Shape.(X (Spmem Done))

let c_beqz =
  def ~cls:Branch
    ~enc:(C { funct3 = 6 })
    ~ext:[ "zca" ]
    ~also:Kind.[ [ Creg; Sym ] ]
    Opcode.C_beqz
    Shape.(Creg (Imm Done))

let c_j =
  def ~cls:Jump
    ~enc:(C { funct3 = 5 })
    ~ext:[ "zca" ]
    ~also:Kind.[ [ Sym ] ]
    Opcode.C_j
    Shape.(Imm Done)

let c_jal =
  def ~cls:Jump
    ~enc:(C { funct3 = 1 })
    ~ext:[ "zca" ] ~xlen:Rv32
    ~also:Kind.[ [ Sym ] ]
    Opcode.C_jal
    Shape.(Imm Done)

let c_bnez =
  def ~cls:Branch
    ~enc:(C { funct3 = 7 })
    ~ext:[ "zca" ]
    ~also:Kind.[ [ Creg; Sym ] ]
    Opcode.C_bnez
    Shape.(Creg (Imm Done))

let slti =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 2 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slti
    Shape.(X (X (Imm Done)))

let sltiu =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 3 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Sltiu
    Shape.(X (X (Imm Done)))

let xori =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 4 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Xori
    Shape.(X (X (Imm Done)))

let ori =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 6 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Ori
    Shape.(X (X (Imm Done)))

let andi =
  def ~cls:Alu
    ~enc:(I { opcode = 0x13; funct3 = 7 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Andi
    Shape.(X (X (Imm Done)))

let slli =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 1; funct6 = 0x00 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slli
    Shape.(X (X (Imm Done)))

let srli =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x00 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srli
    Shape.(X (X (Imm Done)))

let srai =
  def ~cls:Alu
    ~enc:(Shift { funct3 = 5; funct6 = 0x10 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srai
    Shape.(X (X (Imm Done)))

let addiw =
  def ~cls:Alu
    ~enc:(I { opcode = 0x1b; funct3 = 0 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Addiw
    Shape.(X (X (Imm Done)))

let slliw =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 1; funct7 = 0x00 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Slliw
    Shape.(X (X (Imm Done)))

let srliw =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x00 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Srliw
    Shape.(X (X (Imm Done)))

let sraiw =
  def ~cls:Alu
    ~enc:(Shiftw { funct3 = 5; funct7 = 0x20 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Sraiw
    Shape.(X (X (Imm Done)))

let sext_w = def ~cls:Alu ~xlen:Rv64 ~lower:true Opcode.Sext_w Shape.(X (X Done))

let lb =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 0 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lb
    Shape.(X (Mem Done))

let lh =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 1 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lh
    Shape.(X (Mem Done))

let lw =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 2 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lw
    Shape.(X (Mem Done))

let lbu =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 4 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lbu
    Shape.(X (Mem Done))

let lhu =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 5 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lhu
    Shape.(X (Mem Done))

let lwu =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 6 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Lwu
    Shape.(X (Mem Done))

let ld =
  def ~cls:Load
    ~enc:(I { opcode = 0x03; funct3 = 3 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ]; [ X; Sym ] ]
    Opcode.Ld
    Shape.(X (Mem Done))

let sb =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 0 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sb
    Shape.(X (Mem Done))

let sh =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 1 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sh
    Shape.(X (Mem Done))

let sw =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 2 })
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sw
    Shape.(X (Mem Done))

let sd =
  def ~cls:Store
    ~enc:(S { opcode = 0x23; funct3 = 3 })
    ~xlen:Rv64
    ~also:Kind.[ [ X; Spmem ] ]
    Opcode.Sd
    Shape.(X (Mem Done))

let beq =
  def ~cls:Branch
    ~enc:(B { funct3 = 0 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Beq
    Shape.(X (X (Imm Done)))

let bne =
  def ~cls:Branch
    ~enc:(B { funct3 = 1 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bne
    Shape.(X (X (Imm Done)))

let blt =
  def ~cls:Branch
    ~enc:(B { funct3 = 4 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Blt
    Shape.(X (X (Imm Done)))

let bge =
  def ~cls:Branch
    ~enc:(B { funct3 = 5 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bge
    Shape.(X (X (Imm Done)))

let bltu =
  def ~cls:Branch
    ~enc:(B { funct3 = 6 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bltu
    Shape.(X (X (Imm Done)))

let bgeu =
  def ~cls:Branch
    ~enc:(B { funct3 = 7 })
    ~also:Kind.[ [ X; X; Sym ] ]
    Opcode.Bgeu
    Shape.(X (X (Imm Done)))

let bgtu =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Sym ] ] Opcode.Bgtu Shape.(X (X (Imm Done)))

let bleu =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Sym ] ] Opcode.Bleu Shape.(X (X (Imm Done)))

let bgt =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Sym ] ] Opcode.Bgt Shape.(X (X (Imm Done)))

let ble =
  def ~cls:Branch ~lower:true ~also:Kind.[ [ X; X; Sym ] ] Opcode.Ble Shape.(X (X (Imm Done)))

let beqz = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Beqz Shape.(X (Imm Done))
let bnez = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Bnez Shape.(X (Imm Done))
let bgez = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Bgez Shape.(X (Imm Done))
let bltz = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Bltz Shape.(X (Imm Done))
let blez = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Blez Shape.(X (Imm Done))
let bgtz = def ~cls:Branch ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Bgtz Shape.(X (Imm Done))

let lui =
  def ~cls:Alu ~enc:(U { opcode = 0x37 }) ~also:Kind.[ [ X; Sym ] ] Opcode.Lui Shape.(X (Imm Done))

let auipc =
  def ~cls:Alu
    ~enc:(U { opcode = 0x17 })
    ~also:Kind.[ [ X; Sym ] ]
    Opcode.Auipc
    Shape.(X (Imm Done))

let jal =
  def ~cls:Jump ~lower:true
    ~also:Kind.[ [ Sym ]; [ X; Imm ]; [ X; Sym ] ]
    Opcode.Jal
    Shape.(Imm Done)

let jalr =
  def ~cls:Jump
    ~enc:(I { opcode = 0x67; funct3 = 0 })
    ~also:Kind.[ [ X; Mem ]; [ X; Spmem ]; [ X; X; Imm ]; [ X; X; Sym ] ]
    Opcode.Jalr
    Shape.(X Done)

let mv = def ~cls:Alu ~lower:true Opcode.Mv Shape.(X (X Done))
let nop = def ~cls:Alu ~lower:true Opcode.Nop Shape.Done
let j = def ~cls:Jump ~lower:true ~also:Kind.[ [ Sym ] ] Opcode.J Shape.(Imm Done)
let jr = def ~cls:Jump ~lower:true Opcode.Jr Shape.(X Done)
let call = def ~cls:Jump ~lower:true ~also:Kind.[ [ Sym ] ] Opcode.Call Shape.(Imm Done)
let tail = def ~cls:Jump ~lower:true ~also:Kind.[ [ Sym ] ] Opcode.Tail Shape.(Imm Done)
let la = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.La Shape.(X (Imm Done))
let lla = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Lla Shape.(X (Imm Done))
let li = def ~cls:Alu ~lower:true ~also:Kind.[ [ X; Sym ] ] Opcode.Li Shape.(X (Imm Done))
let ret = def ~cls:Jump ~lower:true Opcode.Ret Shape.Done
let ecall = def ~cls:System ~lower:true Opcode.Ecall Shape.Done
let ebreak = def ~cls:System ~lower:true Opcode.Ebreak Shape.Done
let unimp = def ~cls:Csr ~lower:true Opcode.Unimp Shape.Done
let fence_i = def ~cls:System ~ext:[ "zifencei" ] ~lower:true Opcode.Fence_i Shape.Done
let fence = def ~cls:System ~lower:true Opcode.Fence Shape.Done
let fence_tso = def ~cls:System ~lower:true Opcode.Fence_tso Shape.Done
let pause = def ~cls:System ~lower:true Opcode.Pause Shape.Done

let fld =
  def ~cls:Load
    ~enc:(I { opcode = 0x07; funct3 = 3 })
    ~ext:[ "d" ]
    ~also:Kind.[ [ F; Spmem ]; [ F; Sym; X ] ]
    Opcode.Fld
    Shape.(F (Mem Done))

let flw =
  def ~cls:Load
    ~enc:(I { opcode = 0x07; funct3 = 2 })
    ~ext:[ "f" ]
    ~also:Kind.[ [ F; Spmem ]; [ F; Sym; X ] ]
    Opcode.Flw
    Shape.(F (Mem Done))

let fsd =
  def ~cls:Store
    ~enc:(S { opcode = 0x27; funct3 = 3 })
    ~ext:[ "d" ]
    ~also:Kind.[ [ F; Spmem ] ]
    Opcode.Fsd
    Shape.(F (Mem Done))

let fsw =
  def ~cls:Store
    ~enc:(S { opcode = 0x27; funct3 = 2 })
    ~ext:[ "f" ]
    ~also:Kind.[ [ F; Spmem ] ]
    Opcode.Fsw
    Shape.(F (Mem Done))

let fadd_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x01; rs2 = None })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fadd_d
    Shape.(F (F (F Done)))

let fadd_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x00; rs2 = None })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fadd_s
    Shape.(F (F (F Done)))

let fsub_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x05; rs2 = None })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fsub_d
    Shape.(F (F (F Done)))

let fsub_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x04; rs2 = None })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fsub_s
    Shape.(F (F (F Done)))

let fmul_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x09; rs2 = None })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fmul_d
    Shape.(F (F (F Done)))

let fmul_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x08; rs2 = None })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fmul_s
    Shape.(F (F (F Done)))

let fdiv_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x0d; rs2 = None })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fdiv_d
    Shape.(F (F (F Done)))

let fdiv_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x0c; rs2 = None })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; F; Rm ] ]
    Opcode.Fdiv_s
    Shape.(F (F (F Done)))

let fneg_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fneg_d
    Shape.(F (F Done))

let fneg_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fneg_s
    Shape.(F (F Done))

let fabs_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fabs_d
    Shape.(F (F Done))

let fabs_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fabs_s
    Shape.(F (F Done))

let fmv_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fmv_s
    Shape.(F (F Done))

let fmv_x_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] Opcode.Fmv_x_s
    Shape.(X (F Done))

let fmv_s_x =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x78; rs2 = Some 0 })
    ~ext:[ "f" ] Opcode.Fmv_s_x
    Shape.(F (X Done))

let fsgnj_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fsgnj_s
    Shape.(F (F (F Done)))

let fsgnj_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fsgnj_d
    Shape.(F (F (F Done)))

let fsgnjn_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fsgnjn_s
    Shape.(F (F (F Done)))

let fsgnjn_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fsgnjn_d
    Shape.(F (F (F Done)))

let fsgnjx_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x10; rs2 = None })
    ~ext:[ "f" ] Opcode.Fsgnjx_s
    Shape.(F (F (F Done)))

let fsgnjx_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fsgnjx_d
    Shape.(F (F (F Done)))

let fmin_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x14; rs2 = None })
    ~ext:[ "f" ] Opcode.Fmin_s
    Shape.(F (F (F Done)))

let fmax_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x14; rs2 = None })
    ~ext:[ "f" ] Opcode.Fmax_s
    Shape.(F (F (F Done)))

let fmin_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x15; rs2 = None })
    ~ext:[ "d" ] Opcode.Fmin_d
    Shape.(F (F (F Done)))

let fmax_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x15; rs2 = None })
    ~ext:[ "d" ] Opcode.Fmax_d
    Shape.(F (F (F Done)))

let fsqrt_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x2c; rs2 = Some 0 })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fsqrt_s
    Shape.(F (F Done))

let fsqrt_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x2d; rs2 = Some 0 })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fsqrt_d
    Shape.(F (F Done))

let fclass_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] Opcode.Fclass_s
    Shape.(X (F Done))

let fclass_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x71; rs2 = Some 0 })
    ~ext:[ "d" ] Opcode.Fclass_d
    Shape.(X (F Done))

let fmadd_s =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x43; fmt = 0 })
    ~ext:[ "f" ] Opcode.Fmadd_s
    Shape.(F (F (F (F Done))))

let fmadd_d =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x43; fmt = 1 })
    ~ext:[ "d" ] Opcode.Fmadd_d
    Shape.(F (F (F (F Done))))

let fmsub_s =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x47; fmt = 0 })
    ~ext:[ "f" ] Opcode.Fmsub_s
    Shape.(F (F (F (F Done))))

let fmsub_d =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x47; fmt = 1 })
    ~ext:[ "d" ] Opcode.Fmsub_d
    Shape.(F (F (F (F Done))))

let fnmsub_s =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4b; fmt = 0 })
    ~ext:[ "f" ] Opcode.Fnmsub_s
    Shape.(F (F (F (F Done))))

let fnmsub_d =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4b; fmt = 1 })
    ~ext:[ "d" ] Opcode.Fnmsub_d
    Shape.(F (F (F (F Done))))

let fnmadd_s =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4f; fmt = 0 })
    ~ext:[ "f" ] Opcode.Fnmadd_s
    Shape.(F (F (F (F Done))))

let fnmadd_d =
  def ~cls:Fp
    ~enc:(R4 { opcode = 0x4f; fmt = 1 })
    ~ext:[ "d" ] Opcode.Fnmadd_d
    Shape.(F (F (F (F Done))))

let fmv_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x11; rs2 = None })
    ~ext:[ "d" ] Opcode.Fmv_d
    Shape.(F (F Done))

let fmv_x_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x71; rs2 = Some 0 })
    ~ext:[ "d" ] ~xlen:Rv64 Opcode.Fmv_x_d
    Shape.(X (F Done))

let feq_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] Opcode.Feq_d
    Shape.(X (F (F Done)))

let fle_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] Opcode.Fle_d
    Shape.(X (F (F Done)))

let flt_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x51; rs2 = None })
    ~ext:[ "d" ] Opcode.Flt_d
    Shape.(X (F (F Done)))

let flt_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 1; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] Opcode.Flt_s
    Shape.(X (F (F Done)))

let feq_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 2; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] Opcode.Feq_s
    Shape.(X (F (F Done)))

let fle_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x50; rs2 = None })
    ~ext:[ "f" ] Opcode.Fle_s
    Shape.(X (F (F Done)))

let fmv_x_w =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x70; rs2 = Some 0 })
    ~ext:[ "f" ] Opcode.Fmv_x_w
    Shape.(X (F Done))

let fmv_w_x =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x78; rs2 = Some 0 })
    ~ext:[ "f" ] Opcode.Fmv_w_x
    Shape.(F (X Done))

let fcvt_w_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 0 })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_w_d
    Shape.(X (F Done))

let fcvt_wu_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 1 })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_wu_d
    Shape.(X (F Done))

let fcvt_l_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 2 })
    ~ext:[ "d" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_l_d
    Shape.(X (F Done))

let fcvt_d_w =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x69; rs2 = Some 0 })
    ~ext:[ "d" ] ~rm:Rm.Rne
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_w
    Shape.(F (X Done))

let fcvt_d_wu =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x69; rs2 = Some 1 })
    ~ext:[ "d" ] ~rm:Rm.Rne
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_wu
    Shape.(F (X Done))

let fcvt_s_w =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 0 })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_w
    Shape.(F (X Done))

let fcvt_s_wu =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 1 })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_wu
    Shape.(F (X Done))

let fcvt_w_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 0 })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_w_s
    Shape.(X (F Done))

let fcvt_wu_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 1 })
    ~ext:[ "f" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_wu_s
    Shape.(X (F Done))

let fcvt_s_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x20; rs2 = Some 1 })
    ~ext:[ "d" ] ~rm:Rm.Dyn
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fcvt_s_d
    Shape.(F (F Done))

let fcvt_d_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 0; funct7 = 0x21; rs2 = Some 0 })
    ~ext:[ "d" ] ~rm:Rm.Rne
    ~also:Kind.[ [ F; F; Rm ] ]
    Opcode.Fcvt_d_s
    Shape.(F (F Done))

let fcvt_s_l =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 2 })
    ~ext:[ "f" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_l
    Shape.(F (X Done))

let fcvt_lu_d =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x61; rs2 = Some 3 })
    ~ext:[ "d" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_lu_d
    Shape.(X (F Done))

let fcvt_d_l =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x69; rs2 = Some 2 })
    ~ext:[ "d" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_l
    Shape.(F (X Done))

let fcvt_d_lu =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x69; rs2 = Some 3 })
    ~ext:[ "d" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_d_lu
    Shape.(F (X Done))

let fcvt_l_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 2 })
    ~ext:[ "f" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_l_s
    Shape.(X (F Done))

let fcvt_lu_s =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x60; rs2 = Some 3 })
    ~ext:[ "f" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ X; F; Rm ] ]
    Opcode.Fcvt_lu_s
    Shape.(X (F Done))

let fcvt_s_lu =
  def ~cls:Fp
    ~enc:(R { opcode = 0x53; funct3 = 7; funct7 = 0x68; rs2 = Some 3 })
    ~ext:[ "f" ] ~xlen:Rv64 ~rm:Rm.Dyn
    ~also:Kind.[ [ F; X; Rm ] ]
    Opcode.Fcvt_s_lu
    Shape.(F (X Done))

(* Every definition, in [Opcode.all]'s order. *)
let all = Array.of_list (List.rev !registry)
