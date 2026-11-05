(* The aarch64 (A64) target's instruction types: registers, operands, conditions, opcodes and
   instructions. The encoder ({!Aarch64_encode}) builds on them, and so does the per-opcode table
   ({!Aarch64_ops}), which sits between the two. *)

open Foundation

(* {1 Registers}

   Register 31 is [sp] in some instruction positions and [xzr] in others, and
   nothing in the encoding says which - the *instruction* says. So the flag is
   part of the register value and is checked at lowering, where the diagnostic
   can name the operand, rather than being inferred at encode time where it
   could only be guessed. *)

module Reg = struct
  type t = { num : int; width : int; is_sp : bool }

  let equal a b = a.num = b.num && a.width = b.width && a.is_sp = b.is_sp

  let name r =
    let p = if r.width = 64 then "x" else "w" in
    if r.num = 31 then if r.is_sp then if r.width = 64 then "sp" else "wsp" else p ^ "zr"
    else Printf.sprintf "%s%d" p r.num

  let pp ppf r = Fmt.string ppf (name r)

  let find name =
    let numbered prefix width =
      let n = String.length prefix in
      if String.length name > n && String.sub name 0 n = prefix then
        match int_of_string_opt (String.sub name n (String.length name - n)) with
        | Some i when i >= 0 && i <= 30 -> Some { num = i; width; is_sp = false }
        | _ -> None
      else None
    in
    match name with
    | "sp" -> Some { num = 31; width = 64; is_sp = true }
    | "wsp" -> Some { num = 31; width = 32; is_sp = true }
    | "xzr" -> Some { num = 31; width = 64; is_sp = false }
    | "wzr" -> Some { num = 31; width = 32; is_sp = false }
    | _ -> ( match numbered "x" 64 with Some r -> Some r | None -> numbered "w" 32)

  (* Decoding cannot recover [is_sp] from the bits, so it is supplied by the
     position: the caller says whether this field is an SP-accepting one. *)
  let of_num ~width ~sp n = { num = n land 31; width; is_sp = sp && n land 31 = 31 }

  (* The link register, which [ret] defaults to and which the ABI helpers name
     often enough that spelling the record out each time obscured the intent. *)
  let x30 = { num = 30; width = 64; is_sp = false }
end

(* The scalar FP register file - [d0]-[d31]/[s0]-[s31] - kept separate from
   [Reg] rather than folded into it with a "kind" tag: A64 has no register
   that is sometimes general-purpose and sometimes floating-point the way
   [Reg]'s own SP/zero-register ambiguity is resolved by instruction
   position, so a shared type would carry a distinction every consumer has
   to re-check for no case that needs it. Only [d]/[s] are here (M5's FMOV/
   FCMP-immediate scope, the corpus notes); [q]/[v] (NEON) are not. *)
module Freg = struct
  type t = { num : int; double : bool }

  let equal a b = a.num = b.num && a.double = b.double
  let name r = Printf.sprintf "%s%d" (if r.double then "d" else "s") r.num
  let pp ppf r = Fmt.string ppf (name r)

  let find name =
    let numbered prefix double =
      let n = String.length prefix in
      if String.length name > n && String.sub name 0 n = prefix then
        match int_of_string_opt (String.sub name n (String.length name - n)) with
        | Some i when i >= 0 && i <= 31 -> Some { num = i; double }
        | _ -> None
      else None
    in
    match numbered "d" true with Some r -> Some r | None -> numbered "s" false

  let of_num ~double n = { num = n land 31; double }
end

(* {1 Operands} *)

(* A memory offset is a number or an expression (§D2). [ldr w0, [x16, #:lo12:g]]
   is the reason: the offset is not known until the linker places [g], so a
   numeric field cannot hold it and a separate "symbolic load" form would be a
   second spelling of the same addressing mode. *)
(* The index extends of a register offset and of ADD/SUB (extended register); [Lsl_ext] is the
   [lsl] an index takes. *)
module Extend = struct
  type t = Uxtb | Uxth | Uxtw | Uxtx | Sxtb | Sxth | Sxtw | Sxtx | Lsl_ext

  let all = [ Uxtb; Uxth; Uxtw; Uxtx; Sxtb; Sxth; Sxtw; Sxtx; Lsl_ext ]

  let name = function
    | Uxtb -> "uxtb"
    | Uxth -> "uxth"
    | Uxtw -> "uxtw"
    | Uxtx -> "uxtx"
    | Sxtb -> "sxtb"
    | Sxth -> "sxth"
    | Sxtw -> "sxtw"
    | Sxtx -> "sxtx"
    | Lsl_ext -> "lsl"

  let of_name s = List.find_opt (fun e -> String.equal (name e) s) all
end

module Disp = struct
  (* [Reg] is the register-offset addressing form - [ldr w4, [x9, w10, uxtw
     #2]] - not a fourth [Mem] shape but a third [Disp] one, because base
     plus offset is the whole addressing relation and this is a third kind
     of offset, exactly as [Sym] already is. [amount] is what the source
     wrote: [None] when the extend name carried no [#N] at all ([uxtw]
     alone, or the two-register form with no extend token at all - GAS
     accepts that too and it means [lsl] unshifted), [Some n] when it did,
     including [Some 0] - which is a real, distinct encoding (S=1) from
     [None] (S=0) even though both add the index unscaled; verified against
     real [as]/[objdump]: [[x20, x1]] and [[x20, x1, lsl #0]] assemble to
     the same word and objdump prints both as the bare two-register form,
     which is why decode reconstructs [amount = None] whenever the encoded
     amount is unscaled, regardless of which spelling produced it. *)
  type t =
    | Const of int64
    | Sym of Asm_core.Expr.t
    | Reg of { index : Reg.t; extend : Extend.t; amount : int option }

  let zero = Const 0L
  let is_zero = function Const v -> Int64.equal v 0L | Sym _ | Reg _ -> false
  let equal a b = match (a, b) with Const x, Const y -> Int64.equal x y | _ -> a = b

  let to_string = function
    | Const v -> Printf.sprintf "#%Ld" v
    | Sym e -> Asm_core.Expr.to_string e
    | Reg { index; extend; amount } ->
        let tail =
          if extend = Extend.Lsl_ext && amount = None then ""
          else
            ", " ^ Extend.name extend
            ^ match amount with Some n -> Printf.sprintf " #%d" n | None -> ""
        in
        Reg.name index ^ tail
end

module Mem = struct
  type t = { base : Reg.t; offset : Disp.t; writeback : bool; pre : bool }

  let pp ppf m =
    let off = if Disp.is_zero m.offset then "" else ", " ^ Disp.to_string m.offset in
    Fmt.pf ppf "[%a%s]%s" Reg.pp m.base off (if m.writeback then "!" else "")
end

(* A shift or extend operand, [<keyword> #<amount>]: the shifted-register forms take a shift,
   ADD/SUB (extended register) an extend. *)
module Shift = struct
  type kind = Lsl | Lsr | Asr | Ror | Ext of Extend.t
  type t = { kind : kind; amount : int }

  let name = function
    | Lsl -> "lsl"
    | Lsr -> "lsr"
    | Asr -> "asr"
    | Ror -> "ror"
    | Ext e -> Extend.name e

  let of_name = function
    | "lsl" -> Some Lsl
    | "lsr" -> Some Lsr
    | "asr" -> Some Asr
    | "ror" -> Some Ror
    | s -> Option.map (fun e -> Ext e) (Extend.of_name s)
end

(* {1 Advanced SIMD}

   The vector forms a compiler's selected code uses: arithmetic on two or four binary32 or two
   binary64 lanes, lane moves, single-lane and replicating loads and stores, and the Q-register
   transfers. A vector register is its number alone; the arrangement or lane belongs to the
   operand that names it, as it does in the assembler's own syntax. *)

(* The arrangement of a vector register: a 64-bit half or the whole 128 bits. *)
module Varr = struct
  type t = B8 | B16 | S2 | S4 | D2

  let name = function B8 -> "8b" | B16 -> "16b" | S2 -> "2s" | S4 -> "4s" | D2 -> "2d"

  let of_name = function
    | "8b" -> Some B8
    | "16b" -> Some B16
    | "2s" -> Some S2
    | "4s" -> Some S4
    | "2d" -> Some D2
    | _ -> None

  let equal (a : t) b = a = b
  let q = function B8 | S2 -> 0 | B16 | S4 | D2 -> 1
  let sz = function D2 -> 1 | B8 | B16 | S2 | S4 -> 0
end

(* One lane's size: a binary32 or binary64 element. *)
module Lane = struct
  type t = S | D

  let name = function S -> "s" | D -> "d"
  let of_name = function "s" -> Some S | "d" -> Some D | _ -> None
  let lanes = function S -> 4 | D -> 2

  (* DUP and INS's [imm5]: the lane size's marker bit with the index above it. *)
  let imm5 t i = match t with S -> (i lsl 3) lor 0b100 | D -> (i lsl 4) lor 0b1000

  let of_imm5 v =
    if v land 0b1111 = 0b1000 then Some (D, v lsr 4)
    else if v land 0b111 = 0b100 then Some (S, v lsr 3)
    else None
end

(* {1 Register views}

   The same register values, typed by what an instruction position accepts. A general register is
   [('w, 'k) greg]: [w] its width ([x] or [w]) and [k] what register 31 is - [gp] for x0-x30, [zr]
   for the zero register, [sp] for the stack pointer. A builder that takes the zero register or a
   stack pointer asks for [\[< gp | zr\]] or [\[< gp | sp\]], so [add x0 w1 x2] and [add x0 sp x2]
   do not compile. *)
module View : sig
  type x = [ `X ]
  type w = [ `W ]
  type gp = [ `Gp ]
  type zr = [ `Zr ]
  type sp = [ `Sp ]
  type d = [ `D ]
  type s = [ `S ]

  type (+'w, +'k) greg = private Reg.t
  (** a general-purpose register *)

  type +'s freg = private Freg.t
  (** a scalar floating-point register of size [d] or [s] *)

  type qreg = private int

  type +'a vec = private int * Varr.t
  (** a vector register read in the arrangement ['a] ([`B8 | `B16 | `S2 | `S4 | `D2]) *)

  type lane = private int * Lane.t * int
  type vlist = private int * Varr.t
  type vlist_lane = private int * Lane.t * int

  (* Unchecked: for the register tables, which only build values of the class the tag names. *)
  module Unsafe : sig
    val greg : Reg.t -> ('w, 'k) greg
    val freg : Freg.t -> 's freg
    val qreg : int -> qreg
    val vec : int -> Varr.t -> 'a vec
    val lane : int -> Lane.t -> int -> lane
    val vlist : int -> Varr.t -> vlist
    val vlist_lane : int -> Lane.t -> int -> vlist_lane
  end
end = struct
  type x = [ `X ]
  type w = [ `W ]
  type gp = [ `Gp ]
  type zr = [ `Zr ]
  type sp = [ `Sp ]
  type d = [ `D ]
  type s = [ `S ]
  type ('w, 'k) greg = Reg.t
  type 's freg = Freg.t
  type qreg = int
  type 'a vec = int * Varr.t
  type lane = int * Lane.t * int
  type vlist = int * Varr.t
  type vlist_lane = int * Lane.t * int

  module Unsafe = struct
    let greg r = r
    let freg r = r
    let qreg n = n
    let vec n a = (n, a)
    let lane n l i = (n, l, i)
    let vlist n a = (n, a)
    let vlist_lane n l i = (n, l, i)
  end
end

(* One register class: the REGS laws over a list of numbers, with the class's spelling. *)
module Class (P : sig
  type t

  val prefix : string
  val numbers : int list
  val make : int -> t
  val to_int : t -> int
end) =
struct
  type t = P.t

  let numbers = P.numbers
  let count = List.length numbers
  let mem n = List.mem n numbers
  let of_int n = if mem n then Some (P.make n) else None

  let of_int_exn n =
    match of_int n with Some r -> r | None -> invalid_arg (P.prefix ^ ": no such register")

  let to_int = P.to_int
  let all = List.map P.make numbers
  let range = Some (List.hd numbers, List.nth numbers (count - 1))
  let name r = Printf.sprintf "%s%d" P.prefix (P.to_int r)
  let pp ppf r = Fmt.string ppf (name r)

  let find s =
    let n = String.length P.prefix in
    if String.length s > n && String.sub s 0 n = P.prefix then
      match int_of_string_opt (String.sub s n (String.length s - n)) with
      | Some i -> of_int i
      | None -> None
    else None

  let equal a b = P.to_int a = P.to_int b
end

let upto n = List.init (n + 1) Fun.id

(* x0-x30 and w0-w30: register 31 is a register of its own, [xzr]/[wzr] or [sp]/[wsp] by position *)
module Xreg = Class (struct
  type t = (View.x, View.gp) View.greg

  let prefix = "x"
  let numbers = upto 30
  let make n = View.Unsafe.greg { Reg.num = n; width = 64; is_sp = false }
  let to_int (r : t) = (r :> Reg.t).Reg.num
end)

module Wreg = Class (struct
  type t = (View.w, View.gp) View.greg

  let prefix = "w"
  let numbers = upto 30
  let make n = View.Unsafe.greg { Reg.num = n; width = 32; is_sp = false }
  let to_int (r : t) = (r :> Reg.t).Reg.num
end)

(* the stack pointer and the zero registers *)
let sp : (View.x, View.sp) View.greg = View.Unsafe.greg { Reg.num = 31; width = 64; is_sp = true }
let wsp : (View.w, View.sp) View.greg = View.Unsafe.greg { Reg.num = 31; width = 32; is_sp = true }
let xzr : (View.x, View.zr) View.greg = View.Unsafe.greg { Reg.num = 31; width = 64; is_sp = false }
let wzr : (View.w, View.zr) View.greg = View.Unsafe.greg { Reg.num = 31; width = 32; is_sp = false }

module Dreg = Class (struct
  type t = View.d View.freg

  let prefix = "d"
  let numbers = upto 31
  let make n = View.Unsafe.freg { Freg.num = n; double = true }
  let to_int (r : t) = (r :> Freg.t).Freg.num
end)

module Sreg = Class (struct
  type t = View.s View.freg

  let prefix = "s"
  let numbers = upto 31
  let make n = View.Unsafe.freg { Freg.num = n; double = false }
  let to_int (r : t) = (r :> Freg.t).Freg.num
end)

module Qreg = Class (struct
  type t = View.qreg

  let prefix = "q"
  let numbers = upto 31
  let make = View.Unsafe.qreg
  let to_int (r : t) = (r :> int)
end)

(* v0-v31, the number a vector operand names; the arrangement is given where it is used *)
module Vreg = Class (struct
  type t = int

  let prefix = "v"
  let numbers = upto 31
  let make n = n
  let to_int n = n
end)

module Operand = struct
  type t =
    | Reg of Reg.t
    | Freg of Freg.t
    | Imm of Bigint.t
    | Fimm of float
        (** [#1.0000000] in [fmov d0, #1.0000000] - the scalar FP modified immediate FMOV and
            FCMP(#0.0) take. A distinct constructor from [Imm] rather than a reinterpreted bit
            pattern: the value is a *value*, and [Fimm]'s domain (what VFPExpandImm can express) is
            not [Imm]'s (any integer that fits). *)
    | Mem of Mem.t
    | Shift of Shift.t
        (** [lsl #0] in [movz w0, #42, lsl #0]. Not an operand in any real sense - it modifies the
            immediate before it - but the common parser hands over comma-separated slices and this
            is one of them. Simplify folds it into the operand it belongs to; a surface AST that
            had already folded it could not report a shift applied to nothing. *)
    | Sym of Asm_core.Expr.t
        (** a branch or call target. Written without a [#], unlike an immediate: it is an address,
            not a value, and the two are spelled differently for that reason. *)
    | Vec of int * Varr.t  (** [v0.4s] *)
    | Vlane of int * Lane.t * int  (** [v0.s[1]] *)
    | Vlist of int * Varr.t  (** [{v0.4s}] *)
    | Vlist_lane of int * Lane.t * int  (** [{v0.s}[1]] *)
    | Qreg of int  (** [q0] *)

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Freg r -> Freg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    (* Objdump's own spelling ([%.18e], verified against real
       [aarch64-linux-gnu-objdump]: [#1.000000000000000000e+00], not
       the compiler's source spelling [#1.0000000] - same rule as [Reg]'s
       canonical names ([sl]/[fp] rather than the [r10]/[r11] a source file
       might have written), because this printer is also what the decoded
       canonical dump uses and that dump is compared against objdump's. *)
    | Fimm f -> Fmt.pf ppf "#%s" (Printf.sprintf "%.18e" f)
    | Mem m -> Mem.pp ppf m
    | Shift s -> Fmt.pf ppf "%s #%d" (Shift.name s.Shift.kind) s.Shift.amount
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
    | Vec (r, a) -> Fmt.pf ppf "v%d.%s" r (Varr.name a)
    | Vlane (r, l, i) -> Fmt.pf ppf "v%d.%s[%d]" r (Lane.name l) i
    | Vlist (r, a) -> Fmt.pf ppf "{v%d.%s}" r (Varr.name a)
    | Vlist_lane (r, l, i) -> Fmt.pf ppf "{v%d.%s}[%d]" r (Lane.name l) i
    | Qreg r -> Fmt.pf ppf "q%d" r
end

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Condition codes}

   Unlike A32 a condition is not on every instruction: A64 has three consumers -
   [b.<cc>], [csel] and the [cset] family - and this file needs the first two.
   Declared in encoding order, so the code is the position, and all sixteen are
   named because [decode] must be total over the field. *)
module Cond = struct
  type t = Eq | Ne | Cs | Cc | Mi | Pl | Vs | Vc | Hi | Ls | Ge | Lt | Gt | Le | Al | Nv

  let all = [ Eq; Ne; Cs; Cc; Mi; Pl; Vs; Vc; Hi; Ls; Ge; Lt; Gt; Le; Al; Nv ]

  let name = function
    | Eq -> "eq"
    | Ne -> "ne"
    | Cs -> "cs"
    | Cc -> "cc"
    | Mi -> "mi"
    | Pl -> "pl"
    | Vs -> "vs"
    | Vc -> "vc"
    | Hi -> "hi"
    | Ls -> "ls"
    | Ge -> "ge"
    | Lt -> "lt"
    | Gt -> "gt"
    | Le -> "le"
    | Al -> "al"
    | Nv -> "nv"

  let equal (a : t) b = a = b

  let code c =
    let rec go i = function [] -> 14 | x :: rest -> if equal x c then i else go (i + 1) rest in
    go 0 all

  (* Input-only spellings, as on A32: objdump prints [cs] and [cc]. *)
  let synonyms = [ ("hs", Cs); ("lo", Cc) ]

  let of_name s =
    match List.find_opt (fun c -> String.equal (name c) s) all with
    | Some c -> Some c
    | None -> List.assoc_opt s synonyms

  (* [cset rd, cc] is [csinc rd, zr, zr, invert(cc)] - the condition that must hold for the
     *complement* selection (real hardware's own encoding of the alias, not a semantic negation
     this project computes at runtime). Every A64 condition but AL/NV pairs with the one flipping
     its least-significant bit, and [cset] never evidences either of those two. *)
  let invert = function
    | Eq -> Ne
    | Ne -> Eq
    | Cs -> Cc
    | Cc -> Cs
    | Mi -> Pl
    | Pl -> Mi
    | Vs -> Vc
    | Vc -> Vs
    | Hi -> Ls
    | Ls -> Hi
    | Ge -> Lt
    | Lt -> Ge
    | Gt -> Le
    | Le -> Gt
    | Al -> Nv
    | Nv -> Al
end

module Opcode = struct
  type t =
    | Add
    | Mov
    | Movz
    | Movn
    | Movk
    | Cbz
    | Cbnz
    | Sxtw
    | Uxtw
    | Udiv
    | Sdiv
    | Cset
    | Lsl
    | Lsr
    | Asr
    | Ror
    | Ubfx
    | Ubfiz
    | Sbfx
    | Sbfiz
    | Rbit
    | Rev16
    | Rev32
    | Rev
    | Clz
    | Cls
    | Stp
    | Ldp
    | Ldr
    | Ldrb
    | Ldrh
    | Ldrsb
    | Ldrsh
    | Ldrsw
    | Str
    | Strb
    | Strh
    | Ret
    | Br
    | Mrs
    | Msr
    | Sub
    | Cmp
    | Cmn
    | And
    | Orr
    | Eor
    | Bic
    | Madd
    | Msub
    | Mul
    | Csel
    | Adrp
    | Adr
    | B
    | Bcond of Cond.t
    | Udf
    | Bl
    | Fmov
    | Fcmp
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Fcsel
    | Scvtf
    | Ucvtf
    | Fneg
    | Fmax
    | Fmla
    | Fcmeq
    | Fcmge
    | Fcmgt
    | Bsl
    | Bit
    | Not
    | Fmadd
    | Dup
    | Ins
    | Ld1
    | Ld1r
    | St1
    | Fcvtl
    | Fcvtn
    | Fsqrt
    | Frintz
    | Fcvt
    | Fcvtzs
    | Fcvtzu
    | Tst
end

module Instruction = struct
  type t = { op : Opcode.t; ops : Operand.t list }

  let mk op ops = { op; ops }
end
