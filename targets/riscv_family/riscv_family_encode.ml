open Foundation
module C = Codec

module type PROFILE = sig
  val name : string
  val triple : string
  val xlen : int
end

module Make (P : PROFILE) = struct
  open Asm_core.Lowered_ast

  let name = P.name
  let triple = P.triple
  let xlen = P.xlen

  module Reg = struct
    type t = X of int | F of int

    let equal a b = a = b
    let name = function X n -> Printf.sprintf "x%d" n | F n -> Printf.sprintf "f%d" n
    let pp ppf r = Fmt.string ppf (name r)

    let aliases =
      [
        ("zero", 0);
        ("ra", 1);
        ("sp", 2);
        ("gp", 3);
        ("tp", 4);
        ("t0", 5);
        ("t1", 6);
        ("t2", 7);
        ("s0", 8);
        ("fp", 8);
        ("s1", 9);
        ("a0", 10);
        ("a1", 11);
        ("a2", 12);
        ("a3", 13);
        ("a4", 14);
        ("a5", 15);
        ("a6", 16);
        ("a7", 17);
        ("s2", 18);
        ("s3", 19);
        ("s4", 20);
        ("s5", 21);
        ("s6", 22);
        ("s7", 23);
        ("s8", 24);
        ("s9", 25);
        ("s10", 26);
        ("s11", 27);
        ("t3", 28);
        ("t4", 29);
        ("t5", 30);
        ("t6", 31);
      ]

    let numbered prefix ctor s =
      let n = String.length prefix in
      if String.length s > n && String.sub s 0 n = prefix then
        match int_of_string_opt (String.sub s n (String.length s - n)) with
        | Some i when i >= 0 && i < 32 -> Some (ctor i)
        | _ -> None
      else None

    let find s =
      match numbered "x" (fun n -> X n) s with
      | Some _ as r -> r
      | None -> (
          match numbered "f" (fun n -> F n) s with
          | Some _ as r -> r
          | None -> Option.map (fun n -> X n) (List.assoc_opt s aliases))

    let x = function X n -> Some n | F _ -> None
    let x_exn = function X n -> n | F _ -> invalid_arg "RISC-V integer register required"
    let of_x n = X (n land 31)
  end

  module Mem = struct
    type t = { offset : Asm_core.Expr.t; base : Reg.t }

    let pp ppf m = Fmt.pf ppf "%a(%a)" Asm_core.Expr.pp m.offset Reg.pp m.base
  end

  module Operand = struct
    type t = Reg of Reg.t | Imm of Bigint.t | Sym of Asm_core.Expr.t | Mem of Mem.t

    let pp ppf = function
      | Reg r -> Reg.pp ppf r
      | Imm n -> Bigint.pp ppf n
      | Sym e -> Asm_core.Expr.pp ppf e
      | Mem m -> Mem.pp ppf m
  end

  module Opcode = struct
    type t =
      | Add
      | Sub
      | Sll
      | Slt
      | Sltu
      | Snez
      | Xor
      | Srl
      | Sra
      | Or
      | And
      | Mul
      | Remu
      | Addw
      | Subw
      | Sllw
      | Srlw
      | Sraw
      | Mulw
      | Addi
      | Slti
      | Sltiu
      | Xori
      | Ori
      | Andi
      | Slli
      | Srli
      | Srai
      | Addiw
      | Slliw
      | Srliw
      | Sraiw
      | Sext_w
      | Lb
      | Lh
      | Lw
      | Lbu
      | Lhu
      | Lwu
      | Ld
      | Sb
      | Sh
      | Sw
      | Sd
      | Beq
      | Bne
      | Blt
      | Bge
      | Bltu
      | Bgeu
      | Bgtu
      | Bleu
      | Lui
      | Auipc
      | Jal
      | Jalr
      | Mv
      | Nop
      | J
      | Jr
      | Call
      | Tail
      | La
      | Lla
      | Li
      | Ret
      | Ecall
      | Ebreak
      | Unimp
      | Fence_i
      | Fence
      | Fld
      | Flw
      | Fsd
      | Fsw
      | Fadd_d
      | Fadd_s
      | Fsub_d
      | Fsub_s
      | Fmul_d
      | Fmul_s
      | Fdiv_d
      | Fdiv_s
      | Fneg_d
      | Fneg_s
      | Fmv_d
      | Fmv_x_d
      | Feq_d
      | Fle_d
      | Flt_d
      | Flt_s
      | Fcvt_w_d
      | Fcvt_wu_d
      | Fcvt_l_d
      | Fcvt_d_w
      | Fcvt_d_wu
      | Fcvt_s_w
      | Fcvt_s_d
      | Fcvt_d_s
      | Fcvt_s_l

    let name = function
      | Add -> "add"
      | Sub -> "sub"
      | Sll -> "sll"
      | Slt -> "slt"
      | Sltu -> "sltu"
      | Snez -> "snez"
      | Xor -> "xor"
      | Srl -> "srl"
      | Sra -> "sra"
      | Or -> "or"
      | And -> "and"
      | Mul -> "mul"
      | Remu -> "remu"
      | Addw -> "addw"
      | Subw -> "subw"
      | Sllw -> "sllw"
      | Srlw -> "srlw"
      | Sraw -> "sraw"
      | Mulw -> "mulw"
      | Addi -> "addi"
      | Slti -> "slti"
      | Sltiu -> "sltiu"
      | Xori -> "xori"
      | Ori -> "ori"
      | Andi -> "andi"
      | Slli -> "slli"
      | Srli -> "srli"
      | Srai -> "srai"
      | Addiw -> "addiw"
      | Slliw -> "slliw"
      | Srliw -> "srliw"
      | Sraiw -> "sraiw"
      | Sext_w -> "sext.w"
      | Lb -> "lb"
      | Lh -> "lh"
      | Lw -> "lw"
      | Lbu -> "lbu"
      | Lhu -> "lhu"
      | Lwu -> "lwu"
      | Ld -> "ld"
      | Sb -> "sb"
      | Sh -> "sh"
      | Sw -> "sw"
      | Sd -> "sd"
      | Beq -> "beq"
      | Bne -> "bne"
      | Blt -> "blt"
      | Bge -> "bge"
      | Bltu -> "bltu"
      | Bgeu -> "bgeu"
      | Bgtu -> "bgtu"
      | Bleu -> "bleu"
      | Lui -> "lui"
      | Auipc -> "auipc"
      | Jal -> "jal"
      | Jalr -> "jalr"
      | Mv -> "mv"
      | Nop -> "nop"
      | J -> "j"
      | Jr -> "jr"
      | Call -> "call"
      | Tail -> "tail"
      | La -> "la"
      | Lla -> "lla"
      | Li -> "li"
      | Ret -> "ret"
      | Ecall -> "ecall"
      | Ebreak -> "ebreak"
      | Unimp -> "unimp"
      | Fence_i -> "fence.i"
      | Fence -> "fence"
      | Fld -> "fld"
      | Flw -> "flw"
      | Fsd -> "fsd"
      | Fsw -> "fsw"
      | Fadd_d -> "fadd.d"
      | Fadd_s -> "fadd.s"
      | Fsub_d -> "fsub.d"
      | Fsub_s -> "fsub.s"
      | Fmul_d -> "fmul.d"
      | Fmul_s -> "fmul.s"
      | Fdiv_d -> "fdiv.d"
      | Fdiv_s -> "fdiv.s"
      | Fneg_d -> "fneg.d"
      | Fneg_s -> "fneg.s"
      | Fmv_d -> "fmv.d"
      | Fmv_x_d -> "fmv.x.d"
      | Feq_d -> "feq.d"
      | Fle_d -> "fle.d"
      | Flt_d -> "flt.d"
      | Flt_s -> "flt.s"
      | Fcvt_w_d -> "fcvt.w.d"
      | Fcvt_wu_d -> "fcvt.wu.d"
      | Fcvt_l_d -> "fcvt.l.d"
      | Fcvt_d_w -> "fcvt.d.w"
      | Fcvt_d_wu -> "fcvt.d.wu"
      | Fcvt_s_w -> "fcvt.s.w"
      | Fcvt_s_d -> "fcvt.s.d"
      | Fcvt_d_s -> "fcvt.d.s"
      | Fcvt_s_l -> "fcvt.s.l"

    let all =
      [
        Add;
        Sub;
        Sll;
        Slt;
        Sltu;
        Snez;
        Xor;
        Srl;
        Sra;
        Or;
        And;
        Mul;
        Remu;
        Addw;
        Subw;
        Sllw;
        Srlw;
        Sraw;
        Mulw;
        Addi;
        Slti;
        Sltiu;
        Xori;
        Ori;
        Andi;
        Slli;
        Srli;
        Srai;
        Addiw;
        Slliw;
        Srliw;
        Sraiw;
        Sext_w;
        Lb;
        Lh;
        Lw;
        Lbu;
        Lhu;
        Lwu;
        Ld;
        Sb;
        Sh;
        Sw;
        Sd;
        Beq;
        Bne;
        Blt;
        Bge;
        Bltu;
        Bgeu;
        Bgtu;
        Bleu;
        Lui;
        Auipc;
        Jal;
        Jalr;
        Mv;
        Nop;
        J;
        Jr;
        Call;
        Tail;
        La;
        Lla;
        Li;
        Ret;
        Ecall;
        Ebreak;
        Unimp;
        Fence_i;
        Fence;
        Fld;
        Flw;
        Fsd;
        Fsw;
        Fadd_d;
        Fadd_s;
        Fsub_d;
        Fsub_s;
        Fmul_d;
        Fmul_s;
        Fdiv_d;
        Fdiv_s;
        Fneg_d;
        Fneg_s;
        Fmv_d;
        Fmv_x_d;
        Feq_d;
        Fle_d;
        Flt_d;
        Flt_s;
        Fcvt_w_d;
        Fcvt_wu_d;
        Fcvt_l_d;
        Fcvt_d_w;
        Fcvt_d_wu;
        Fcvt_s_w;
        Fcvt_s_d;
        Fcvt_d_s;
        Fcvt_s_l;
      ]

    let of_mnemonic s = List.find_opt (fun op -> String.equal (name op) s) all
  end

  open Opcode

  module Surface = struct
    type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

    let pp ppf s =
      if s.ops = [] then Fmt.string ppf s.mnemonic
      else Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) s.ops
  end

  module Instruction = struct
    type t = { op : Opcode.t; ops : Operand.t list }

    let mk op ops = { op; ops }

    let pp ppf i =
      if i.ops = [] then Fmt.string ppf (Opcode.name i.op)
      else Fmt.pf ppf "%s %a" (Opcode.name i.op) Fmt.(list ~sep:(any ", ") Operand.pp) i.ops
  end

  (* Which of an F/D instruction's [R]-shape operand positions is a scalar FP register
     rather than a GPR, and how many of the three fields printed. The generic [R]/[I]/[S]
     shapes carry plain register numbers with no class tag - correct for encoding (an FP
     register field is bit-for-bit the same 5-bit slot a GPR one is), but [pp] and [decode]
     both need to know which is which to print/reconstruct the right operand text, and this
     one table is what both consult rather than duplicating the classification. [arity] = 2
     covers the pseudo-mnemonics ([fneg.*]/[fmv.d], real hardware's [fsgnjn]/[fsgnj] with
     [rs2] forced equal to [rs1] - the shared register is stored once and never printed
     twice) and every convert/move between one FP and one GPR; [arity] = 3 covers the
     three-FP-register arithmetic family and the FP-operand compares (whose own result is a
     GPR).

     [rm] is [Some default_funct3] exactly when this mnemonic's own [funct3] field is a
     rounding mode rather than a fixed part of the encoding (real hardware defines a
     five-name enumeration - [rne]/[rtz]/[rdn]/[rup]/[rmm] - plus [dyn] for "read it from
     the fcsr", GAS's own default whenever none is written); [default_funct3] is exactly
     the value GAS's bare mnemonic - no explicit rounding-mode operand - picks, per
     mnemonic (almost always [dyn] = [7], except the two conversions real hardware defines
     as always exact, which pick [rne] = [0] instead). A decoded [funct3] equal to the
     default prints no trailing operand at all; any other value round-trips through an
     explicit one (M5, asm/docs/corpus.md - perlin.c/binarytrees.c's own [(int)]/[(long)]
     cast idiom, which truncates and so must override the default with [rtz], not read
     it). [feq.d]/[fle.d]/[flt.d]/[flt.s]'s own [funct3] is a real comparison-kind selector,
     not a rounding mode, so [rm] is [None] there even though the field sits at the same
     bit position. *)
  type freg_shape = { rd_f : bool; rs1_f : bool; rs2_f : bool; arity : int; rm : int option }

  let f_shape_of_name = function
    | "fadd.d" | "fadd.s" | "fsub.d" | "fsub.s" | "fmul.d" | "fmul.s" | "fdiv.d" | "fdiv.s" ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 3; rm = Some 7 }
    | "fneg.d" | "fneg.s" | "fmv.d" ->
        Some { rd_f = true; rs1_f = true; rs2_f = true; arity = 2; rm = None }
    | "feq.d" | "fle.d" | "flt.d" | "flt.s" ->
        Some { rd_f = false; rs1_f = true; rs2_f = true; arity = 3; rm = None }
    | "fcvt.w.d" | "fcvt.wu.d" | "fcvt.l.d" ->
        Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = Some 7 }
    | "fmv.x.d" -> Some { rd_f = false; rs1_f = true; rs2_f = false; arity = 2; rm = None }
    | "fcvt.d.w" | "fcvt.d.wu" ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some 0 }
    | "fcvt.s.w" | "fcvt.s.l" ->
        Some { rd_f = true; rs1_f = false; rs2_f = false; arity = 2; rm = Some 7 }
    | "fcvt.s.d" -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some 7 }
    | "fcvt.d.s" -> Some { rd_f = true; rs1_f = true; rs2_f = false; arity = 2; rm = Some 0 }
    | _ -> None

  (* The six standard rounding-mode names (RISC-V unprivileged spec table 11.2), shared by
     {!freg_shape}'s printing and by [lower_instruction]'s parsing of an explicit third
     operand on a convert. *)
  let rounding_modes = [ ("rne", 0); ("rtz", 1); ("rdn", 2); ("rup", 3); ("rmm", 4); ("dyn", 7) ]
  let rounding_mode_of_name s = List.assoc_opt s rounding_modes
  let rounding_name_of_mode m = List.find_opt (fun (_, v) -> v = m) rounding_modes |> Option.map fst

  module Lowered = struct
    (* The second instruction of an [auipc]-based pair: [Addi] for [la]/[lla]
       (pic), [Jalr] for [call]/[tail], [Load funct3] for a load pseudo like
       [ld rd, symbol] (GAS's own auipc+load expansion, confirmed against real
       riscv64-linux-gnu-as: `ld t6, sym` decodes back as
       `auipc t6, ...; ld t6, 0(t6)`). *)
    type pair_kind =
      | Addi
      | Jalr
      | Load of int
      | Fload of int
          (** [fld]/[flw]'s own literal-pool pseudo (M5, asm/docs/corpus.md - almabench.c/
            fftsp.c/knucleotide.c/...): the same anchored [auipc]+load pairing as [Load],
            but the scratch register can never be [rd] itself the way [ld rd, symbol]
            reuses [rd] as its own [auipc] target - [rd] is a scalar FP register here, and
            [auipc] can only write a GPR. GAS's own three-operand pseudo spells the GPR
            scratch out explicitly (`fld rd, symbol, xtmp`) for exactly that reason, so
            unlike every other {!Pair} kind, [tmp] is a real, distinct, always-printed
            operand rather than a repeat of [rd]. Confirmed against real
            riscv64-linux-gnu-as: `fld fa1, .L100, x31` expands to `auipc t6,
            %pcrel_hi(.L100); fld fa1, %pcrel_lo(...)(t6)`. *)

    type t =
      | R of {
          name : string;
          opcode : int;
          funct3 : int;
          funct7 : int;
          rd : int;
          rs1 : int;
          rs2 : int;
        }
      | I of {
          name : string;
          opcode : int;
          funct3 : int;
          funct_hi : int;
          shamt_bits : int option;
          rd : int;
          rs1 : int;
          imm : Asm_core.Expr.t;
        }
      | S of {
          name : string;
          opcode : int;
          funct3 : int;
          rs1 : int;
          rs2 : int;
          imm : Asm_core.Expr.t;
        }
          (** [opcode] defaults to STORE's [0x23] for every integer store; the F/D extension's
              [fsd]/[fsw] are the only other user, at STORE-FP's [0x27] (M5, asm/docs/corpus.md
              - almabench.c/bisect.c/... callee-saved double spills). Everything else about the
              S-type word - the split 12-bit immediate, [rs1]/[rs2] field positions - is
              identical between the two opcodes, so this is one field rather than a second
              constructor. *)
      | B of { name : string; funct3 : int; rs1 : int; rs2 : int; target : Asm_core.Expr.t }
      | U of { name : string; opcode : int; rd : int; imm : Asm_core.Expr.t }
      | J of { rd : int; target : Asm_core.Expr.t }
      | Pair of { name : string; rd : int; tmp : int; target : Asm_core.Expr.t; kind : pair_kind }
      | Fixed of { name : string; word : int64 }

    let pp_expr ppf e = Asm_core.Expr.pp ppf e
    let reg_name isf n = Printf.sprintf "%s%d" (if isf then "f" else "x") n

    let rm_suffix rm funct3 =
      match rm with
      | Some default when default <> funct3 -> (
          match rounding_name_of_mode funct3 with Some n -> ", " ^ n | None -> "")
      | Some _ | None -> ""

    let pp ppf = function
      | R x -> (
          match f_shape_of_name x.name with
          | Some { rd_f; rs1_f; rs2_f = _; arity = 2; rm } ->
              Fmt.pf ppf "%s %s, %s%s" x.name (reg_name rd_f x.rd) (reg_name rs1_f x.rs1)
                (rm_suffix rm x.funct3)
          | Some { rd_f; rs1_f; rs2_f; arity = _; rm } ->
              Fmt.pf ppf "%s %s, %s, %s%s" x.name (reg_name rd_f x.rd) (reg_name rs1_f x.rs1)
                (reg_name rs2_f x.rs2) (rm_suffix rm x.funct3)
          | None -> Fmt.pf ppf "%s x%d, x%d, x%d" x.name x.rd x.rs1 x.rs2)
      | I x when x.opcode = 0x03 || x.opcode = 0x67 ->
          Fmt.pf ppf "%s x%d, %a(x%d)" x.name x.rd pp_expr x.imm x.rs1
      | I x when x.opcode = 0x07 -> Fmt.pf ppf "%s f%d, %a(x%d)" x.name x.rd pp_expr x.imm x.rs1
      | I x -> Fmt.pf ppf "%s x%d, x%d, %a" x.name x.rd x.rs1 pp_expr x.imm
      | S x when x.opcode = 0x27 -> Fmt.pf ppf "%s f%d, %a(x%d)" x.name x.rs2 pp_expr x.imm x.rs1
      | S x -> Fmt.pf ppf "%s x%d, %a(x%d)" x.name x.rs2 pp_expr x.imm x.rs1
      | B x -> Fmt.pf ppf "%s x%d, x%d, %a" x.name x.rs1 x.rs2 pp_expr x.target
      | U x -> Fmt.pf ppf "%s x%d, %a" x.name x.rd pp_expr x.imm
      | J x -> Fmt.pf ppf "jal x%d, %a" x.rd pp_expr x.target
      | Pair ({ kind = Fload _; _ } as x) ->
          Fmt.pf ppf "%s f%d, %a, x%d" x.name x.rd pp_expr x.target x.tmp
      | Pair x -> Fmt.pf ppf "%s x%d, %a" x.name x.rd pp_expr x.target
      | Fixed x -> Fmt.string ppf x.name

    let equal (a : t) b = a = b
  end

  type fixup_kind =
    | Abs32
    | Abs64
    | Branch13
    | Jal21
    | Pcrel_hi20
    | Pcrel_lo12_i
    | Pcrel_lo12_s
    | Call_hi20
    | Call_lo12_i
    | Abs_hi20
    | Abs_lo12_i
    | Abs_lo12_s

  let fixup_kind_name = function
    | Abs32 -> "abs32"
    | Abs64 -> "abs64"
    | Branch13 -> "pcrel-b13"
    | Jal21 -> "pcrel-j21"
    | Pcrel_hi20 -> "pcrel-hi20"
    | Pcrel_lo12_i -> "pcrel-lo12-i"
    | Pcrel_lo12_s -> "pcrel-lo12-s"
    | Call_hi20 -> "call-hi20"
    | Call_lo12_i -> "call-lo12-i"
    | Abs_hi20 -> "abs-hi20"
    | Abs_lo12_i -> "abs-lo12-i"
    | Abs_lo12_s -> "abs-lo12-s"

  let equal_fixup_kind a b = a = b

  let fixup_family = function
    | Abs32 -> "abs32"
    | Abs64 -> "abs64"
    | Branch13 | Jal21 -> "pcrel-branch"
    | Pcrel_hi20 | Pcrel_lo12_i | Pcrel_lo12_s -> "pcrel-address"
    | Call_hi20 | Call_lo12_i -> "pcrel-call"
    | Abs_hi20 | Abs_lo12_i | Abs_lo12_s -> "absolute-address"

  let fixup_role = function
    | Branch13 | Jal21 -> Asm_core.Lowered_ast.Branch
    | Call_hi20 | Call_lo12_i -> Asm_core.Lowered_ast.Call
    | _ -> Asm_core.Lowered_ast.Data_address

  type feature = No_features

  type target_state = {
    pic : bool;
    relax : bool;
    rvc : bool;
    option_stack : (bool * bool * bool) list;
  }

  let default_state = { pic = false; relax = false; rvc = false; option_stack = [] }
  let default_features = []

  type error_kind =
    [ Target_error.shared
    | `Wrong_operands of string
    | `Rv64_only of string
    | `Immediate_range of string
    | `Immediate_alignment of string
    | `Bad_modifier of string
    | `Decode_short
    | `Decode_length
    | `Decode_no_match
    | `No_data_relocation of int
    | `Padding_not_word_multiple ]

  type error = error_kind Target_error.t

  let pp_error_kind ppf : error_kind -> unit = function
    | #Target_error.shared as e -> Target_error.pp_shared ppf e
    | `Wrong_operands op -> Fmt.pf ppf "no %s form takes these operands" op
    | `Rv64_only op -> Fmt.pf ppf "%s is available only when XLEN is 64" op
    | `Immediate_range what -> Fmt.pf ppf "%s immediate is out of range" what
    | `Immediate_alignment what -> Fmt.pf ppf "%s target is not two-byte aligned" what
    | `Bad_modifier m -> Fmt.pf ppf "unsupported relocation modifier %s" m
    | `Decode_short -> Fmt.string ppf "fewer than four bytes remain"
    | `Decode_length -> Fmt.string ppf "RISC-V instructions must be four bytes"
    | `Decode_no_match -> Fmt.string ppf "no form matches this word"
    | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte initializer" w
    | `Padding_not_word_multiple -> Fmt.string ppf "RISC-V padding must be a multiple of four bytes"

  let error_kind_code : error_kind -> string = function
    | `Unknown_instruction _ -> P.name ^ ".simplify"
    | `Decode_short | `Decode_length | `Decode_no_match | `Decode_no_normalized ->
        P.name ^ ".decode"
    | `No_data_relocation _ -> P.name ^ ".data-fixup"
    | `Padding_not_word_multiple -> P.name ^ ".nop"
    | `Immediate_alignment _ | `Immediate_range _ -> P.name ^ ".fixup"
    | _ -> P.name ^ ".lower"

  let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
  let error_code e = error_kind_code (Target_error.kind e)
  let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

  let diag ?pos ?origin kind =
    Err.Error.make ?pos ~pp_error
      (Target_error.make
         ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:P.name ())
         kind)

  let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

  let simplify_instruction ~features:_ s =
    match Opcode.of_mnemonic s.Surface.mnemonic with
    | None -> Error (diag ~pos:__POS__ ~origin:s.origin (`Unknown_instruction s.mnemonic))
    | Some op -> Ok { Instruction.op; ops = s.ops }

  let const n = Asm_core.Expr.Const (Bigint.of_int n)

  let expr_of = function
    | Operand.Imm n -> Some (Asm_core.Expr.Const n)
    | Operand.Sym e -> Some e
    | _ -> None

  let xreg = function Operand.Reg r -> Reg.x r | _ -> None
  let freg = function Operand.Reg (Reg.F n) -> Some n | _ -> None
  let rv64 op = if xlen = 64 then Ok () else Error (diag ~pos:__POS__ (`Rv64_only op))
  let wrong op = Error (diag ~pos:__POS__ (`Wrong_operands op))

  let r_desc = function
    | Opcode.Add -> Some (0x33, 0, 0x00)
    | Sub -> Some (0x33, 0, 0x20)
    | Sll -> Some (0x33, 1, 0x00)
    | Slt -> Some (0x33, 2, 0x00)
    | Sltu -> Some (0x33, 3, 0x00)
    | Xor -> Some (0x33, 4, 0x00)
    | Srl -> Some (0x33, 5, 0x00)
    | Sra -> Some (0x33, 5, 0x20)
    | Or -> Some (0x33, 6, 0x00)
    | And -> Some (0x33, 7, 0x00)
    | Mul -> Some (0x33, 0, 0x01)
    | Remu -> Some (0x33, 7, 0x01)
    | Addw -> Some (0x3b, 0, 0x00)
    | Subw -> Some (0x3b, 0, 0x20)
    | Sllw -> Some (0x3b, 1, 0x00)
    | Srlw -> Some (0x3b, 5, 0x00)
    | Sraw -> Some (0x3b, 5, 0x20)
    | Mulw -> Some (0x3b, 0, 0x01)
    | _ -> None

  let shift_i_alias = function
    | Opcode.Sll -> Some Opcode.Slli
    | Srl -> Some Srli
    | Sra -> Some Srai
    | Sllw -> Some Slliw
    | Srlw -> Some Srliw
    | Sraw -> Some Sraiw
    | _ -> None

  let i_desc = function
    | Opcode.Addi -> Some (0x13, 0, 0, None)
    | Slti -> Some (0x13, 2, 0, None)
    | Sltiu -> Some (0x13, 3, 0, None)
    | Xori -> Some (0x13, 4, 0, None)
    | Ori -> Some (0x13, 6, 0, None)
    | Andi -> Some (0x13, 7, 0, None)
    | Slli -> Some (0x13, 1, 0, Some xlen)
    | Srli -> Some (0x13, 5, 0, Some xlen)
    | Srai -> Some (0x13, 5, (if xlen = 64 then 0x10 else 0x20), Some xlen)
    | Addiw -> Some (0x1b, 0, 0, None)
    | Slliw -> Some (0x1b, 1, 0, Some 32)
    | Srliw -> Some (0x1b, 5, 0, Some 32)
    | Sraiw -> Some (0x1b, 5, 0x20, Some 32)
    | _ -> None

  let load_desc = function
    | Opcode.Lb -> Some 0
    | Lh -> Some 1
    | Lw -> Some 2
    | Ld -> Some 3
    | Lbu -> Some 4
    | Lhu -> Some 5
    | Lwu -> Some 6
    | _ -> None

  let store_desc = function
    | Opcode.Sb -> Some 0
    | Sh -> Some 1
    | Sw -> Some 2
    | Sd -> Some 3
    | _ -> None

  let branch_desc = function
    | Opcode.Beq -> Some 0
    | Bne -> Some 1
    | Blt -> Some 4
    | Bge -> Some 5
    | Bltu -> Some 6
    | Bgeu -> Some 7
    | _ -> None

  (* OP-FP (opcode [0x53]) descriptors, one table per operand shape (M5 corpus
     evidence, asm/docs/corpus.md - almabench.c/bisect.c/fftsp.c/knucleotide.c/...).
     [funct3] here is the rounding-mode field GAS's own bare mnemonic spelling
     always picks - `7` (dynamic) for the arithmetic family and every convert *to* a
     narrower or differently-rounded type, `0` (round-to-nearest-even) for a
     conversion real hardware defines as always exact - not an operand this corpus
     ever spells explicitly, so no rounding-mode operand is read. *)
  let f_arith_desc = function
    | Opcode.Fadd_s -> Some (7, 0x00)
    | Fadd_d -> Some (7, 0x01)
    | Fsub_s -> Some (7, 0x04)
    | Fsub_d -> Some (7, 0x05)
    | Fmul_s -> Some (7, 0x08)
    | Fmul_d -> Some (7, 0x09)
    | Fdiv_s -> Some (7, 0x0c)
    | Fdiv_d -> Some (7, 0x0d)
    | _ -> None

  (* [fneg.d]/[fneg.s]/[fmv.d] - FSGNJN/FSGNJ with [rs2] forced equal to [rs1], real
     hardware's own alias (verified against real riscv64-linux-gnu-as/objdump:
     `fneg.d fs0, fs1` -> `22949453`, decoding back with [rs1] = [rs2] = 9). The
     general two-different-register [fsgnj]/[fsgnjn]/[fsgnjx] this shares a word with
     is not evidenced and not implemented. *)
  let f_sgnj_desc = function
    | Opcode.Fneg_s -> Some (1, 0x10)
    | Fneg_d -> Some (1, 0x11)
    | Fmv_d -> Some (0, 0x11)
    | _ -> None

  (* Compares: [rd] is a GPR (the boolean result), [rs1]/[rs2] are FP. *)
  let f_cmp_desc = function
    | Opcode.Feq_d -> Some (2, 0x51)
    | Fle_d -> Some (0, 0x51)
    | Flt_d -> Some (1, 0x51)
    | Flt_s -> Some (1, 0x50)
    | _ -> None

  (* Float-to-integer converts, and [fmv.x.d] (bit-for-bit move, not a conversion) -
     [rd] is a GPR, [rs1] is FP, [rs2] is a fixed selector rather than a second
     operand. *)
  let f_to_i_desc = function
    | Opcode.Fcvt_w_d -> Some (7, 0x61, 0)
    | Fcvt_wu_d -> Some (7, 0x61, 1)
    | Fcvt_l_d -> Some (7, 0x61, 2)
    | Fmv_x_d -> Some (0, 0x71, 0)
    | _ -> None

  (* Integer-to-float converts - [rd] is FP, [rs1] is a GPR, [rs2] fixed. *)
  let i_to_f_desc = function
    | Opcode.Fcvt_d_w -> Some (0, 0x69, 0)
    | Fcvt_d_wu -> Some (0, 0x69, 1)
    | Fcvt_s_w -> Some (7, 0x68, 0)
    | Fcvt_s_l -> Some (7, 0x68, 2)
    | _ -> None

  (* Float-to-float precision converts - [rd]/[rs1] both FP, [rs2] fixed (the source
     format, per the ISA's own encoding: [1] = double, [0] = single). *)
  let f_to_f_desc = function
    | Opcode.Fcvt_s_d -> Some (7, 0x20, 1)
    | Fcvt_d_s -> Some (0, 0x21, 0)
    | _ -> None

  (* LOAD-FP/STORE-FP (opcodes [0x07]/[0x27]) - the identical addressing mode
     integer [lw]/[sw] use, into/from an FP register instead of a GPR; only
     [funct3] (word width) differs between the two, exactly as it does for the
     integer loads/stores this shares a byte-count convention with. *)
  let f_load_desc = function Opcode.Flw -> Some 2 | Fld -> Some 3 | _ -> None
  let f_store_desc = function Opcode.Fsw -> Some 2 | Fsd -> Some 3 | _ -> None

  let fits_signed bits v =
    let lim = Int64.shift_left 1L (bits - 1) in
    Int64.compare v (Int64.neg lim) >= 0 && Int64.compare v lim < 0

  let int64_expr e =
    match Asm_core.Expr.fold Asm_core.Expr.no_env e with
    | Ok (Asm_core.Expr.Const n) -> Bigint.to_int64_opt n
    | Ok _ | Error _ -> None

  let lower_instruction state i =
    let opn = Opcode.name i.Instruction.op in
    match (i.op, i.ops) with
    | (Opcode.Addw | Subw | Sllw | Srlw | Sraw | Mulw), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | (Opcode.Addiw | Slliw | Srliw | Sraiw | Sext_w | Ld | Lwu | Sd), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | (Opcode.Fcvt_l_d | Fmv_x_d | Fcvt_s_l), _ when xlen <> 64 ->
        Error (diag ~pos:__POS__ (`Rv64_only opn))
    | op, [ a; b; c ] when Option.is_some (r_desc op) -> (
        match (xreg a, xreg b, xreg c, r_desc op) with
        | Some rd, Some rs1, Some rs2, Some (opcode, funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode; funct3; funct7; rd; rs1; rs2 } ]
        | Some rd, Some rs1, None, _ -> (
            (* GAS overloads sll/srl/sra(w) with a third register-typed operand
               as the R-type register-shift-amount form above, and with a third
               immediate-typed operand as an alias for the corresponding
               slli/srli/srai(w) form - confirmed against real
               riscv64-linux-gnu-as: `sll x5, x11, 2` decodes back as
               `slli t0, a1, 0x2`. *)
            match (shift_i_alias op, expr_of c) with
            | Some ialias, Some imm -> (
                match i_desc ialias with
                | Some (opcode, funct3, funct_hi, shamt) ->
                    Ok
                      [
                        Lowered.I
                          {
                            name = Opcode.name ialias;
                            opcode;
                            funct3;
                            funct_hi;
                            shamt_bits = shamt;
                            rd;
                            rs1;
                            imm;
                          };
                      ]
                | None -> wrong opn)
            | _ -> wrong opn)
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (i_desc op) -> (
        match (xreg a, xreg b, expr_of c, i_desc op) with
        | Some rd, Some rs1, Some imm, Some (opcode, funct3, funct_hi, shamt) ->
            Ok
              [
                Lowered.I { name = opn; opcode; funct3; funct_hi; shamt_bits = shamt; rd; rs1; imm };
              ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (load_desc op) -> (
        match (xreg a, Reg.x m.base, load_desc op) with
        | Some rd, Some rs1, Some funct3 ->
            Ok
              [
                Lowered.I
                  {
                    name = opn;
                    opcode = 0x03;
                    funct3;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Ld, [ a; (Operand.Sym _ as target) ] -> (
        (* GAS's own `ld rd, symbol` pseudo, a literal-pool load: auipc rd,
           %pcrel_hi(symbol); ld rd, %pcrel_lo(...)(rd) - the same anchored
           hi/lo pairing `la`/`call` already use, with the second word an
           opcode-0x03 load rather than addi/jalr. Confirmed against real
           riscv64-linux-gnu-as. Scoped to `ld` alone since that is the only
           load pseudo this corpus evidences (a 64-bit constant pulled from a
           `.rodata.cst8` literal, CompCert's own float/double materialization
           idiom on riscv64). *)
        match (xreg a, expr_of target, load_desc Opcode.Ld) with
        | Some rd, Some target, Some funct3 ->
            Ok [ Lowered.Pair { name = opn; rd; tmp = rd; target; kind = Load funct3 } ]
        | _ -> wrong opn)
    | op, [ a; (Operand.Sym _ as target); b ] when Option.is_some (f_load_desc op) -> (
        (* [fld rd, symbol, xtmp] / [flw rd, symbol, xtmp] - the identical anchored
           [auipc]+load pseudo as [ld] above, except the scratch register can't be [rd]
           itself ([auipc] only ever writes a GPR, and [rd] here is a scalar FP register),
           so GAS's own three-operand spelling names it explicitly (M5,
           asm/docs/corpus.md - almabench.c/fftsp.c/knucleotide.c/...). *)
        match (freg a, expr_of target, xreg b, f_load_desc op) with
        | Some rd, Some target, Some tmp, Some funct3 ->
            Ok [ Lowered.Pair { name = opn; rd; tmp; target; kind = Fload funct3 } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (store_desc op) -> (
        match (xreg a, Reg.x m.base, store_desc op) with
        | Some rs2, Some rs1, Some funct3 ->
            Ok [ Lowered.S { name = opn; opcode = 0x23; funct3; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_load_desc op) -> (
        match (freg a, Reg.x m.base, f_load_desc op) with
        | Some rd, Some rs1, Some funct3 ->
            Ok
              [
                Lowered.I
                  {
                    name = opn;
                    opcode = 0x07;
                    funct3;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | op, [ a; Operand.Mem m ] when Option.is_some (f_store_desc op) -> (
        match (freg a, Reg.x m.base, f_store_desc op) with
        | Some rs2, Some rs1, Some funct3 ->
            Ok [ Lowered.S { name = opn; opcode = 0x27; funct3; rs1; rs2; imm = m.offset } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_arith_desc op) -> (
        match (freg a, freg b, freg c, f_arith_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_sgnj_desc op) -> (
        match (freg a, freg b, f_sgnj_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 = rs1 } ]
        | _ -> wrong opn)
    | op, [ a; b; c ] when Option.is_some (f_cmp_desc op) -> (
        match (xreg a, freg b, freg c, f_cmp_desc op) with
        | Some rd, Some rs1, Some rs2, Some (funct3, funct7) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_i_desc op) -> (
        match (xreg a, freg b, f_to_i_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; Operand.Sym (Asm_core.Expr.Symbol rm) ] when Option.is_some (f_to_i_desc op) -> (
        (* [fcvt.w.d rd, rs1, rtz] - an explicit rounding-mode operand overriding the
           bare mnemonic's own default (M5, asm/docs/corpus.md - perlin.c/binarytrees.c's
           own [(int)]/[(long)] C cast idiom, which truncates and so needs [rtz] rather
           than the default dynamic mode). Checked against real riscv64-linux-gnu-as:
           `fcvt.l.d x22, f10, rtz` -> `c2251b53` (funct3 = 1, matching {!rounding_modes}'
           own [rtz] = 1). *)
        match (xreg a, freg b, f_to_i_desc op, rounding_mode_of_name rm) with
        | Some rd, Some rs1, Some (_, funct7, rs2), Some funct3 ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (i_to_f_desc op) -> (
        match (freg a, xreg b, i_to_f_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b ] when Option.is_some (f_to_f_desc op) -> (
        match (freg a, freg b, f_to_f_desc op) with
        | Some rd, Some rs1, Some (funct3, funct7, rs2) ->
            Ok [ Lowered.R { name = opn; opcode = 0x53; funct3; funct7; rd; rs1; rs2 } ]
        | _ -> wrong opn)
    | op, [ a; b; target ] when Option.is_some (branch_desc op) -> (
        match (xreg a, xreg b, expr_of target, branch_desc op) with
        | Some rs1, Some rs2, Some target, Some funct3 ->
            Ok [ Lowered.B { name = opn; funct3; rs1; rs2; target } ]
        | _ -> wrong opn)
    | (Opcode.Bgtu | Bleu), [ a; b; target ] -> (
        (* [bgtu rs,rt,label]/[bleu rs,rt,label] - GAS's own greater-than/
           less-or-equal-unsigned pseudos, [bltu]/[bgeu] with the two register
           operands swapped (real hardware defines no distinct opcode; only
           [bltu rt,rs,label]/[bgeu rt,rs,label] exist). Real objdump always
           disassembles the swapped-operand word back as the real underlying
           mnemonic, never the pseudo - this project's canonical printer follows
           that choice (the same alias-preference precedent as AArch64's
           [ubfx]/[ubfiz], asm/docs/corpus.md's Capability ladder item 4), reusing
           [Lowered.B]'s existing shape rather than adding a name-only variant.
           Checked against real riscv64-linux-gnu-as/objdump: `bgtu a0, a1, .`
           -> `00a5e063`, decoding as `bltu a1, a0, .`; `bleu a0, a1, .` ->
           `fea5fee3`, decoding as `bgeu a1, a0, .`. *)
        let real_op, real_name =
          if i.op = Bgtu then (Opcode.Bltu, "bltu") else (Opcode.Bgeu, "bgeu")
        in
        match (xreg a, xreg b, expr_of target, branch_desc real_op) with
        | Some ra, Some rb, Some target, Some funct3 ->
            Ok [ Lowered.B { name = real_name; funct3; rs1 = rb; rs2 = ra; target } ]
        | _ -> wrong opn)
    | ((Opcode.Lui | Auipc) as op), [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm ->
            Ok [ Lowered.U { name = opn; opcode = (if op = Lui then 0x37 else 0x17); rd; imm } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target -> Ok [ Lowered.J { rd; target } ]
        | _ -> wrong opn)
    | Opcode.Jal, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = 1; target } ]
        | None -> wrong opn)
    | Opcode.Jalr, [ a; Operand.Mem m ] -> (
        match (xreg a, Reg.x m.base) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = m.offset;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a; b; imm ] -> (
        match (xreg a, xreg b, expr_of imm) with
        | Some rd, Some rs1, Some imm ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Jalr, [ a ] | Opcode.Jr, [ a ] -> (
        match xreg a with
        | Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "jalr";
                    opcode = 0x67;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd = (if i.op = Jalr then 1 else 0);
                    rs1;
                    imm = const 0;
                  };
              ]
        | None -> wrong opn)
    | Opcode.Snez, [ a; b ] -> (
        (* [snez rd, rs] - GAS's own set-not-equal-zero pseudo, [sltu rd, zero, rs]
           under a different spelling (only ever reachable through [Sltu]'s own
           three-register form otherwise, which has no all-zero-[rs1] entry point
           of its own). Real objdump prefers the alias text ([snez a2, a3]), but
           this project's canonical printer reuses the real underlying mnemonic
           the same way {!Mv}/{!Nop} above already reuse [addi]'s, rather than
           adding a second, decode-only spelling nothing here compares against
           (M5, asm/docs/corpus.md - asm/helpers/riscv.c's own zero-test idiom).
           Checked against real riscv64-linux-gnu-as/objdump: `snez a2, a3` ->
           `00d03633`, matching `sltu a2, zero, a3` bit-for-bit. *)
        match (xreg a, xreg b) with
        | Some rd, Some rs2 ->
            Ok
              [
                Lowered.R { name = "sltu"; opcode = 0x33; funct3 = 3; funct7 = 0; rd; rs1 = 0; rs2 };
              ]
        | _ -> wrong opn)
    | Opcode.Sext_w, [ a; b ] -> (
        (* [sext.w rd, rs] - the sign-extend-word pseudo, [addiw rd, rs, 0] under a
           different spelling (RV64 only: {!Opcode.t}'s early [Rv64_only] gate
           above already rejects it on RV32). Checked against real
           riscv64-linux-gnu-as/objdump: `sext.w a1, a2` -> `0006059b`, matching
           `addiw a1, a2, 0` bit-for-bit. *)
        match (xreg a, xreg b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "addiw";
                    opcode = 0x1b;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = const 0;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Mv, [ a; b ] -> (
        match (xreg a, xreg b) with
        | Some rd, Some rs1 ->
            Ok
              [
                Lowered.I
                  {
                    name = "addi";
                    opcode = 0x13;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1;
                    imm = const 0;
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Nop, [] ->
        Ok
          [
            Lowered.I
              {
                name = "addi";
                opcode = 0x13;
                funct3 = 0;
                funct_hi = 0;
                shamt_bits = None;
                rd = 0;
                rs1 = 0;
                imm = const 0;
              };
          ]
    | Opcode.J, [ target ] -> (
        match expr_of target with
        | Some target -> Ok [ Lowered.J { rd = 0; target } ]
        | None -> wrong opn)
    | Opcode.Call, [ target ] | Opcode.Tail, [ target ] -> (
        match expr_of target with
        | Some target ->
            Ok
              [
                Lowered.Pair
                  {
                    name = opn;
                    rd = (if i.op = Call then 1 else 0);
                    tmp = (if i.op = Call then 1 else 6);
                    target;
                    kind = Jalr;
                  };
              ]
        | None -> wrong opn)
    | ((Opcode.La | Lla) as op), [ a; target ] -> (
        match (xreg a, expr_of target) with
        | Some rd, Some target when op = Lla || state.pic ->
            Ok [ Lowered.Pair { name = opn; rd; tmp = rd; target; kind = Addi } ]
        | Some rd, Some target ->
            (* In non-PIC state GAS's [la] is the absolute LUI/ADDI pair.
               Keeping this decision in lowering makes source-ordered option
               replay observable and prevents a final-file state from leaking
               backwards across an earlier instruction. *)
            Ok
              [
                Lowered.U
                  { name = "lui"; opcode = 0x37; rd; imm = Asm_core.Expr.Modifier ("%hi", target) };
                Lowered.I
                  {
                    name = "addi";
                    opcode = 0x13;
                    funct3 = 0;
                    funct_hi = 0;
                    shamt_bits = None;
                    rd;
                    rs1 = rd;
                    imm = Asm_core.Expr.Modifier ("%lo", target);
                  };
              ]
        | _ -> wrong opn)
    | Opcode.Li, [ a; imm ] -> (
        match (xreg a, expr_of imm) with
        | Some rd, Some imm -> (
            let addi imm =
              Ok
                [
                  Lowered.I
                    {
                      name = "addi";
                      opcode = 0x13;
                      funct3 = 0;
                      funct_hi = 0;
                      shamt_bits = None;
                      rd;
                      rs1 = 0;
                      imm;
                    };
                ]
            in
            match int64_expr imm with
            | Some v when fits_signed 12 v -> addi imm
            | Some v when Int64.logand v 0xfffL = 0L && fits_signed 32 v ->
                (* GAS's own [li] expansion for a constant whose low 12 bits are
                   exactly zero collapses to a bare [lui] - no [addi], the
                   redundant "+0" GAS itself never emits - the only large-constant
                   [li] shape this corpus evidences (asm/helpers/riscv.c's
                   page/window-aligned address constants, e.g. `li a0,
                   0x30000000`). Checked against real riscv64-linux-gnu-as/
                   objdump: `li a0, 0x30000000` -> `lui a0, 0x30000`; `li a2,
                   -4096` -> `lui a2, 0xfffff`. A constant needing a genuine
                   [lui]+[addi] pair, or one wider than 32 bits, is not evidenced
                   and still falls through to the single-[addi] form below, which
                   then reports the same out-of-range diagnostic it always has. *)
                Ok
                  [
                    Lowered.U
                      {
                        name = "lui";
                        opcode = 0x37;
                        rd;
                        imm = Asm_core.Expr.Const (Bigint.of_int64 (Int64.shift_right v 12));
                      };
                  ]
            | _ -> addi imm)
        | _ -> wrong opn)
    | Opcode.Ret, [] ->
        Ok
          [
            Lowered.I
              {
                name = "jalr";
                opcode = 0x67;
                funct3 = 0;
                funct_hi = 0;
                shamt_bits = None;
                rd = 0;
                rs1 = 1;
                imm = const 0;
              };
          ]
    | Opcode.Ecall, [] -> Ok [ Lowered.Fixed { name = "ecall"; word = 0x00000073L } ]
    | Opcode.Ebreak, [] -> Ok [ Lowered.Fixed { name = "ebreak"; word = 0x00100073L } ]
    | Opcode.Unimp, [] -> Ok [ Lowered.Fixed { name = "unimp"; word = 0xc0001073L } ]
    | Opcode.Fence_i, [] -> Ok [ Lowered.Fixed { name = "fence.i"; word = 0x0000100fL } ]
    | ( Opcode.Fence,
        ([] | [ Operand.Sym (Asm_core.Expr.Symbol "rw"); Operand.Sym (Asm_core.Expr.Symbol "w") ]) )
      ->
        (* [fence rw,w] - a memory barrier, opcode 0x0f/funct3 0 with no ModR/M-like
           register fields ([rd]/[rs1] both fixed zero): [imm[11:0]] splits into a
           4-bit [pred]/[succ] pair, each an (i,o,r,w) flag set, at bits 27:24/23:20.
           Only this one predecessor/successor spelling is evidenced
           (asm/helpers/riscv.c's own release-store fence, via
           `__sync_synchronize`-style codegen), so - matching this project's own
           narrow-scope precedent for a single-spelling fixed word ([fucomp],
           [fence.i] above) - any other combination is a diagnostic rather than a
           silently-computed one; the bare no-operand form is decode's own
           round-trip spelling ({!Lowered.Fixed}'s [pp] prints no operand list, the
           same convention [ret]/[ecall] already use). Checked against real
           riscv64-linux-gnu-as/objdump: `fence rw,w` -> `0310000f`
           (pred = 0b0011 = r|w, succ = 0b0001 = w). *)
        Ok [ Lowered.Fixed { name = "fence"; word = 0x0310000fL } ]
    | _ -> wrong opn

  let mask bits v =
    if bits = 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L bits) 1L)

  let field shift bits v = Int64.shift_left (mask bits v) shift

  let word_r ~opcode ~funct3 ~funct7 ~rd ~rs1 ~rs2 =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int rs1))
               (Int64.logor (field 20 5 (Int64.of_int rs2)) (field 25 7 (Int64.of_int funct7))))))

  let word_i ~opcode ~funct3 ~rd ~rs1 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor (field 15 5 (Int64.of_int rs1)) (field 20 12 imm))))

  let word_s ~opcode ~funct3 ~rs1 ~rs2 imm =
    Int64.logor (Int64.of_int opcode)
      (Int64.logor (field 7 5 imm)
         (Int64.logor
            (field 12 3 (Int64.of_int funct3))
            (Int64.logor
               (field 15 5 (Int64.of_int rs1))
               (Int64.logor
                  (field 20 5 (Int64.of_int rs2))
                  (field 25 7 (Int64.shift_right_logical imm 5))))))

  let word_b ~funct3 ~rs1 ~rs2 imm =
    Int64.logor 0x63L
      (Int64.logor
         (field 7 1 (Int64.shift_right_logical imm 11))
         (Int64.logor
            (field 8 4 (Int64.shift_right_logical imm 1))
            (Int64.logor
               (field 12 3 (Int64.of_int funct3))
               (Int64.logor
                  (field 15 5 (Int64.of_int rs1))
                  (Int64.logor
                     (field 20 5 (Int64.of_int rs2))
                     (Int64.logor
                        (field 25 6 (Int64.shift_right_logical imm 5))
                        (field 31 1 (Int64.shift_right_logical imm 12))))))))

  let word_u ~opcode ~rd imm =
    Int64.logor (Int64.of_int opcode) (Int64.logor (field 7 5 (Int64.of_int rd)) (field 12 20 imm))

  let word_j ~rd imm =
    Int64.logor 0x6fL
      (Int64.logor
         (field 7 5 (Int64.of_int rd))
         (Int64.logor
            (field 12 8 (Int64.shift_right_logical imm 12))
            (Int64.logor
               (field 20 1 (Int64.shift_right_logical imm 11))
               (Int64.logor
                  (field 21 10 (Int64.shift_right_logical imm 1))
                  (field 31 1 (Int64.shift_right_logical imm 20))))))

  let bytes_of_word w =
    String.init 4 (fun i ->
        Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical w (8 * i)) 0xffL)))

  let mk_fixup ~kind ~name:fxname ~slices ~byte_offset ~container ~range ~value ~pairing =
    {
      Asm_core.Lowered_ast.kind;
      kind_name = fixup_kind_name kind;
      family = fixup_family kind;
      role = fixup_role kind;
      name = fxname;
      slices;
      byte_offset;
      container;
      pc_bias = 0;
      range;
      value;
      pairing;
      origin = Origin.synthesized ~pass:(P.name ^ ".encode") ();
    }

  let i_slices = [ { Asm_core.Lowered_ast.bit_offset = 20; bit_width = 12; value_lsb = 0 } ]

  let s_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 7; bit_width = 5; value_lsb = 0 };
      { bit_offset = 25; bit_width = 7; value_lsb = 5 };
    ]

  let b_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 7; bit_width = 1; value_lsb = 11 };
      { bit_offset = 8; bit_width = 4; value_lsb = 1 };
      { bit_offset = 25; bit_width = 6; value_lsb = 5 };
      { bit_offset = 31; bit_width = 1; value_lsb = 12 };
    ]

  let j_slices =
    [
      { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 8; value_lsb = 12 };
      { bit_offset = 20; bit_width = 1; value_lsb = 11 };
      { bit_offset = 21; bit_width = 10; value_lsb = 1 };
      { bit_offset = 31; bit_width = 1; value_lsb = 20 };
    ]

  let u_slices = [ { Asm_core.Lowered_ast.bit_offset = 12; bit_width = 20; value_lsb = 0 } ]
  let form bytes form fixups = { Asm_core.Lowered_ast.bytes; form; fixups }
  let bad_encode kind = Error (diag ~pos:__POS__ kind)

  let encode l =
    let fixed w f = Ok (`Fixed (form (bytes_of_word w) f [])) in
    match l with
    | Lowered.R x ->
        fixed
          (word_r ~opcode:x.opcode ~funct3:x.funct3 ~funct7:x.funct7 ~rd:x.rd ~rs1:x.rs1 ~rs2:x.rs2)
          x.name
    | I x -> (
        match int64_expr x.imm with
        | Some imm -> (
            let encoded_imm =
              match x.shamt_bits with
              | None -> if fits_signed 12 imm then Some imm else None
              | Some width ->
                  let bits = if width = 64 then 6 else 5 in
                  if Int64.compare imm 0L >= 0 && Int64.compare imm (Int64.shift_left 1L bits) < 0
                  then Some (Int64.logor imm (Int64.shift_left (Int64.of_int x.funct_hi) bits))
                  else None
            in
            match encoded_imm with
            | Some imm ->
                fixed (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 imm) x.name
            | None ->
                bad_encode
                  (`Immediate_range
                     (Printf.sprintf "%s %s (%Ld)" x.name (Asm_core.Expr.to_string x.imm) imm)))
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_lo", Asm_core.Expr.Symbol anchor) ->
                let kind = Pcrel_lo12_i in
                let fx =
                  mk_fixup ~kind ~name:"lo" ~slices:i_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:(Asm_core.Expr.Symbol anchor)
                    ~pairing:(Asm_core.Lowered_ast.Pair_tail (Anchor_symbol anchor))
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                let fx =
                  mk_fixup ~kind:Abs_lo12_i ~name:"lo" ~slices:i_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_i ~opcode:x.opcode ~funct3:x.funct3 ~rd:x.rd ~rs1:x.rs1 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range (x.name ^ " " ^ Asm_core.Expr.to_string x.imm))))
    | S x -> (
        match int64_expr x.imm with
        | Some imm when fits_signed 12 imm ->
            fixed (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 imm) x.name
        | Some _ -> bad_encode (`Immediate_range x.name)
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_lo", Asm_core.Expr.Symbol anchor) ->
                let fx =
                  mk_fixup ~kind:Pcrel_lo12_s ~name:"lo" ~slices:s_slices ~byte_offset:0
                    ~container:4 ~range:(Asm_core.Lowered_ast.Signed 12)
                    ~value:(Asm_core.Expr.Symbol anchor)
                    ~pairing:(Asm_core.Lowered_ast.Pair_tail (Anchor_symbol anchor))
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%lo", target) ->
                (* [sw a0,%lo(sym)(s6)] - the store-side sibling of [I]'s own
                   [%lo] case just above (`la`'s non-PIC absolute [lui]+[addi]
                   pair, with [s6] already holding [%hi(sym)] from an earlier
                   instruction this project's own single-pass model resolves
                   independently): a real gap, not a dead branch - {!Abs_lo12_s}
                   already existed in {!fixup_kind} and in [evaluate_fixup]
                   below, but [S]'s own [encode] case never produced one (M5,
                   asm/docs/corpus.md - asm/helpers/riscv.c's own absolute
                   store to a page-local static, the first fixture to spell an
                   [S]-type absolute [%lo]). Checked against real
                   riscv64-linux-gnu-as: `sw a0,%lo(control_alias)(s6)` still
                   assembles to a placeholder-zero S-type word plus an
                   `R_RISCV_LO12_S` relocation, the identical shape [I]'s own
                   [Abs_lo12_i] already produces for a load/[addi]. *)
                let fx =
                  mk_fixup ~kind:Abs_lo12_s ~name:"lo" ~slices:s_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Signed 12) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed
                     (form
                        (bytes_of_word
                           (word_s ~opcode:x.opcode ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L))
                        x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range x.name)))
    | B x ->
        let fx =
          mk_fixup ~kind:Branch13 ~name:"target" ~slices:b_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 13) ~value:x.target ~pairing:Unpaired
        in
        Ok
          (`Fixed
             (form (bytes_of_word (word_b ~funct3:x.funct3 ~rs1:x.rs1 ~rs2:x.rs2 0L)) x.name [ fx ]))
    | J x ->
        let fx =
          mk_fixup ~kind:Jal21 ~name:"target" ~slices:j_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 21) ~value:x.target ~pairing:Unpaired
        in
        Ok (`Fixed (form (bytes_of_word (word_j ~rd:x.rd 0L)) "jal" [ fx ]))
    | U x -> (
        match int64_expr x.imm with
        | Some imm when fits_signed 20 imm || Int64.compare imm 0xfffffL <= 0 ->
            fixed (word_u ~opcode:x.opcode ~rd:x.rd imm) x.name
        | Some _ -> bad_encode (`Immediate_range x.name)
        | None -> (
            match x.imm with
            | Asm_core.Expr.Modifier ("%pcrel_hi", target) ->
                let fx =
                  mk_fixup ~kind:Pcrel_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target
                    ~pairing:(Pair_head "pcrel")
                in
                Ok
                  (`Fixed (form (bytes_of_word (word_u ~opcode:x.opcode ~rd:x.rd 0L)) x.name [ fx ]))
            | Asm_core.Expr.Modifier ("%hi", target) ->
                let fx =
                  mk_fixup ~kind:Abs_hi20 ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
                    ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:target ~pairing:Unpaired
                in
                Ok
                  (`Fixed (form (bytes_of_word (word_u ~opcode:x.opcode ~rd:x.rd 0L)) x.name [ fx ]))
            | Asm_core.Expr.Modifier (m, _) -> bad_encode (`Bad_modifier m)
            | _ -> bad_encode (`Immediate_range x.name)))
    | Pair x ->
        let lo_opcode, lo_funct3 =
          match x.kind with
          | Addi -> (0x13, 0)
          | Jalr -> (0x67, 0)
          | Load funct3 -> (0x03, funct3)
          | Fload funct3 -> (0x07, funct3)
        in
        let hiword = word_u ~opcode:0x17 ~rd:x.tmp 0L in
        let lowword = word_i ~opcode:lo_opcode ~funct3:lo_funct3 ~rd:x.rd ~rs1:x.tmp 0L in
        let hi_kind, lo_kind =
          match x.kind with
          | Addi | Load _ | Fload _ -> (Pcrel_hi20, Pcrel_lo12_i)
          | Jalr -> (Call_hi20, Call_lo12_i)
        in
        let hi =
          mk_fixup ~kind:hi_kind ~name:"hi" ~slices:u_slices ~byte_offset:0 ~container:4
            ~range:(Asm_core.Lowered_ast.Bitpattern 20) ~value:x.target ~pairing:(Pair_head "pair")
        in
        let lo =
          mk_fixup ~kind:lo_kind ~name:"lo" ~slices:i_slices ~byte_offset:4 ~container:4
            ~range:(Asm_core.Lowered_ast.Signed 12) ~value:x.target
            ~pairing:(Pair_tail (Sibling_key "pair"))
        in
        Ok (`Fixed (form (bytes_of_word hiword ^ bytes_of_word lowword) x.name [ hi; lo ]))
    | Fixed x -> fixed x.word x.name

  let sign_extend bits v =
    let s = 64 - bits in
    Int64.shift_right (Int64.shift_left v s) s

  let bits w shift width = mask width (Int64.shift_right_logical w shift)

  let read_word s pos =
    let w = ref 0L in
    for i = 0 to 3 do
      w := Int64.logor !w (Int64.shift_left (Int64.of_int (Char.code s.[pos + i])) (8 * i))
    done;
    !w

  let reg n = Operand.Reg (Reg.of_x n)
  let f_operand n = Operand.Reg (Reg.F (n land 31))
  let imm n = Operand.Imm (Bigint.of_int64 n)
  let sym n = Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 n))

  let mem rs1 n =
    Operand.Mem { Mem.base = Reg.of_x rs1; offset = Asm_core.Expr.Const (Bigint.of_int64 n) }

  let op_exn s = Option.get (Opcode.of_mnemonic s)
  let instruction op ops = { Instruction.op; ops }

  let r_name opcode f3 f7 =
    match (opcode, f3, f7) with
    | 0x33, 0, 0 -> Some "add"
    | 0x33, 0, 0x20 -> Some "sub"
    | 0x33, 1, 0 -> Some "sll"
    | 0x33, 2, 0 -> Some "slt"
    | 0x33, 3, 0 -> Some "sltu"
    | 0x33, 4, 0 -> Some "xor"
    | 0x33, 5, 0 -> Some "srl"
    | 0x33, 5, 0x20 -> Some "sra"
    | 0x33, 6, 0 -> Some "or"
    | 0x33, 7, 0 -> Some "and"
    | 0x33, 0, 1 -> Some "mul"
    | 0x33, 7, 1 -> Some "remu"
    | 0x3b, 0, 0 -> Some "addw"
    | 0x3b, 0, 0x20 -> Some "subw"
    | 0x3b, 1, 0 -> Some "sllw"
    | 0x3b, 5, 0 -> Some "srlw"
    | 0x3b, 5, 0x20 -> Some "sraw"
    | 0x3b, 0, 1 -> Some "mulw"
    | _ -> None

  (* OP-FP (opcode [0x53]): unlike every integer R-type above, several of these
     mnemonics also need [rs2] to disambiguate (a convert or move fixes its second
     "operand" to a selector rather than reading a real register - see
     {!f_shape_of_name}, which is what actually decides whether [rs2] is printed at
     all). [fneg.d]/[fneg.s]/[fmv.d] are real hardware's [fsgnjn]/[fsgnj] with [rs1] =
     [rs2] - this table alone cannot tell that apart from the general two-different-
     operand form (unimplemented, unevidenced), so the caller checks [rs1 = rs2]
     before accepting one of these three names. *)
  let f_r_name f3 f7 rs2 =
    (* The arithmetic and convert families' [f3] is a rounding mode, not part of a
       mnemonic's own identity (an explicit non-default one is a real, separate operand -
       {!freg_shape}'s [rm] and [rm_suffix] print it, not this lookup) - so these match on
       [f7]/[rs2] alone, before the [f3]-sensitive compares/pseudo-moves below ever see
       them. The two groups' [f7] values never overlap. *)
    match (f7, rs2) with
    | 0x00, _ -> Some "fadd.s"
    | 0x01, _ -> Some "fadd.d"
    | 0x04, _ -> Some "fsub.s"
    | 0x05, _ -> Some "fsub.d"
    | 0x08, _ -> Some "fmul.s"
    | 0x09, _ -> Some "fmul.d"
    | 0x0c, _ -> Some "fdiv.s"
    | 0x0d, _ -> Some "fdiv.d"
    | 0x61, 0 -> Some "fcvt.w.d"
    | 0x61, 1 -> Some "fcvt.wu.d"
    | 0x61, 2 -> Some "fcvt.l.d"
    | 0x69, 0 -> Some "fcvt.d.w"
    | 0x69, 1 -> Some "fcvt.d.wu"
    | 0x68, 0 -> Some "fcvt.s.w"
    | 0x68, 2 -> Some "fcvt.s.l"
    | 0x20, 1 -> Some "fcvt.s.d"
    | 0x21, 0 -> Some "fcvt.d.s"
    | _ -> (
        match (f3, f7, rs2) with
        | 1, 0x10, _ -> Some "fneg.s"
        | 1, 0x11, _ -> Some "fneg.d"
        | 0, 0x11, _ -> Some "fmv.d"
        | 0, 0x51, _ -> Some "fle.d"
        | 2, 0x51, _ -> Some "feq.d"
        | 1, 0x51, _ -> Some "flt.d"
        | 1, 0x50, _ -> Some "flt.s"
        | 0, 0x71, 0 -> Some "fmv.x.d"
        | _ -> None)

  let f_load_name = function 2 -> Some "flw" | 3 -> Some "fld" | _ -> None
  let f_store_name = function 2 -> Some "fsw" | 3 -> Some "fsd" | _ -> None

  type decode_context = { state : target_state; address : int64 }

  let decode ctx bytes ~pos =
    if String.length bytes - pos < 4 then Error (diag ~pos:__POS__ `Decode_short)
    else if Char.code bytes.[pos] land 3 <> 3 then Error (diag ~pos:__POS__ `Decode_length)
    else
      let w = read_word bytes pos in
      let opc = Int64.to_int (bits w 0 7) in
      let rd = Int64.to_int (bits w 7 5) and f3 = Int64.to_int (bits w 12 3) in
      let rs1 = Int64.to_int (bits w 15 5) and rs2 = Int64.to_int (bits w 20 5) in
      let f7 = Int64.to_int (bits w 25 7) in
      let result =
        match r_name opc f3 f7 with
        | Some n when opc <> 0x3b || xlen = 64 ->
            Some (instruction (op_exn n) [ reg rd; reg rs1; reg rs2 ], n)
        | _ -> (
            match opc with
            | (0x13 | 0x1b) when opc = 0x13 || xlen = 64 ->
                let raw = bits w 20 12 in
                let funct_shift =
                  if opc = 0x13 && xlen = 64 then Int64.to_int (bits w 26 6)
                  else Int64.to_int (bits w 25 7)
                in
                let n =
                  match (opc, f3, funct_shift) with
                  | 0x13, 0, _ -> Some "addi"
                  | 0x13, 2, _ -> Some "slti"
                  | 0x13, 3, _ -> Some "sltiu"
                  | 0x13, 4, _ -> Some "xori"
                  | 0x13, 6, _ -> Some "ori"
                  | 0x13, 7, _ -> Some "andi"
                  | 0x13, 1, 0 -> Some "slli"
                  | 0x13, 5, 0 -> Some "srli"
                  | 0x13, 5, x when x = if xlen = 64 then 0x10 else 0x20 -> Some "srai"
                  | 0x1b, 0, _ -> Some "addiw"
                  | 0x1b, 1, 0 -> Some "slliw"
                  | 0x1b, 5, 0 -> Some "srliw"
                  | 0x1b, 5, 0x20 -> Some "sraiw"
                  | _ -> None
                in
                Option.map
                  (fun n ->
                    let shift = f3 = 1 || f3 = 5 in
                    let width = if opc = 0x1b then 5 else if xlen = 64 then 6 else 5 in
                    let v = if shift then bits w 20 width else sign_extend 12 raw in
                    (instruction (op_exn n) [ reg rd; reg rs1; imm v ], n))
                  n
            | 0x03 ->
                let n =
                  match f3 with
                  | 0 -> Some "lb"
                  | 1 -> Some "lh"
                  | 2 -> Some "lw"
                  | 3 when xlen = 64 -> Some "ld"
                  | 4 -> Some "lbu"
                  | 5 -> Some "lhu"
                  | 6 when xlen = 64 -> Some "lwu"
                  | _ -> None
                in
                Option.map
                  (fun n ->
                    (instruction (op_exn n) [ reg rd; mem rs1 (sign_extend 12 (bits w 20 12)) ], n))
                  n
            | 0x23 ->
                let n =
                  match f3 with
                  | 0 -> Some "sb"
                  | 1 -> Some "sh"
                  | 2 -> Some "sw"
                  | 3 when xlen = 64 -> Some "sd"
                  | _ -> None
                in
                let v =
                  sign_extend 12 (Int64.logor (bits w 7 5) (Int64.shift_left (bits w 25 7) 5))
                in
                Option.map (fun n -> (instruction (op_exn n) [ reg rs2; mem rs1 v ], n)) n
            | 0x07 ->
                Option.map
                  (fun n ->
                    ( instruction (op_exn n)
                        [ f_operand rd; mem rs1 (sign_extend 12 (bits w 20 12)) ],
                      n ))
                  (f_load_name f3)
            | 0x27 ->
                let v =
                  sign_extend 12 (Int64.logor (bits w 7 5) (Int64.shift_left (bits w 25 7) 5))
                in
                Option.map
                  (fun n -> (instruction (op_exn n) [ f_operand rs2; mem rs1 v ], n))
                  (f_store_name f3)
            | 0x53 -> (
                let n =
                  match f_r_name f3 f7 rs2 with
                  | Some (("fneg.s" | "fneg.d" | "fmv.d") as n) when rs1 = rs2 -> Some n
                  | Some ("fneg.s" | "fneg.d" | "fmv.d") -> None
                  | other -> other
                in
                match n with
                | None -> None
                | Some n -> (
                    match f_shape_of_name n with
                    | None -> None
                    | Some shape ->
                        let r isf v = if isf then f_operand v else reg v in
                        let base =
                          if shape.arity = 2 then [ r shape.rd_f rd; r shape.rs1_f rs1 ]
                          else [ r shape.rd_f rd; r shape.rs1_f rs1; r shape.rs2_f rs2 ]
                        in
                        (* An explicit, non-default rounding mode is a real trailing
                           operand a re-parse must see too, not only text {!Lowered.pp}
                           prints - {!rm_suffix}'s own condition, read back here from the
                           other end. *)
                        let ops =
                          match shape.rm with
                          | Some default when default <> f3 -> (
                              match rounding_name_of_mode f3 with
                              | Some rm -> base @ [ Operand.Sym (Asm_core.Expr.Symbol rm) ]
                              | None -> base)
                          | Some _ | None -> base
                        in
                        Some (instruction (op_exn n) ops, n)))
            | 0x63 ->
                let n =
                  match f3 with
                  | 0 -> Some "beq"
                  | 1 -> Some "bne"
                  | 4 -> Some "blt"
                  | 5 -> Some "bge"
                  | 6 -> Some "bltu"
                  | 7 -> Some "bgeu"
                  | _ -> None
                in
                let v =
                  sign_extend 13
                    (Int64.logor
                       (Int64.shift_left (bits w 31 1) 12)
                       (Int64.logor
                          (Int64.shift_left (bits w 7 1) 11)
                          (Int64.logor
                             (Int64.shift_left (bits w 25 6) 5)
                             (Int64.shift_left (bits w 8 4) 1))))
                in
                Option.map
                  (fun n ->
                    (instruction (op_exn n) [ reg rs1; reg rs2; sym (Int64.add ctx.address v) ], n))
                  n
            | 0x37 | 0x17 ->
                let n = if opc = 0x37 then "lui" else "auipc" in
                Some (instruction (op_exn n) [ reg rd; imm (sign_extend 20 (bits w 12 20)) ], n)
            | 0x6f ->
                let v =
                  sign_extend 21
                    (Int64.logor
                       (Int64.shift_left (bits w 31 1) 20)
                       (Int64.logor
                          (Int64.shift_left (bits w 12 8) 12)
                          (Int64.logor
                             (Int64.shift_left (bits w 20 1) 11)
                             (Int64.shift_left (bits w 21 10) 1))))
                in
                Some (instruction Opcode.Jal [ reg rd; sym (Int64.add ctx.address v) ], "jal")
            | 0x67 when f3 = 0 ->
                Some
                  ( instruction Opcode.Jalr [ reg rd; mem rs1 (sign_extend 12 (bits w 20 12)) ],
                    "jalr" )
            | 0x73 when Int64.equal w 0x73L -> Some (instruction Opcode.Ecall [], "ecall")
            | 0x73 when Int64.equal w 0x00100073L -> Some (instruction Opcode.Ebreak [], "ebreak")
            | 0x73 when Int64.equal w 0xc0001073L -> Some (instruction Opcode.Unimp [], "unimp")
            | 0x0f when Int64.equal w 0x0000100fL -> Some (instruction Opcode.Fence_i [], "fence.i")
            | 0x0f when Int64.equal w 0x0310000fL -> Some (instruction Opcode.Fence [], "fence")
            | _ -> None)
      in
      match result with
      | None -> Error (diag ~pos:__POS__ `Decode_no_match)
      | Some (i, f) -> Ok (i, f, 4)

  (* The public codec describes the same real words as the explicit encoder
     and decoder above.  Symbol expressions and PC context live outside a raw
     word relation, so decoding canonicalizes them to a zero displacement; the
     fixup-bearing [encode] result remains the authority for layout.  The
     64-bit branch is one atomic AUIPC+I-format pair, never two independently
     selectable instructions. *)
  let codec : (Lowered.t, fixup_kind) C.t =
    let encoded_bits wanted l =
      match encode l with
      | Ok (`Fixed f) when String.length f.bytes = wanted ->
          if wanted = 4 then Some (read_word f.bytes 0)
          else
            Some
              (Int64.logor
                 (Int64.shift_left (read_word f.bytes 0) 32)
                 (Int64.logand (read_word f.bytes 4) 0xffffffffL))
      | Ok (`Fixed _) | Ok (`Ladder _) | Error _ -> None
    in
    let decode_word w =
      match decode { state = default_state; address = 0L } (bytes_of_word w) ~pos:0 with
      | Ok (instruction, _, 4) -> (
          match lower_instruction default_state instruction with
          | Ok [ lowered ] -> Some lowered
          | _ -> None)
      | Ok _ | Error _ -> None
    in
    let decode_pair w =
      let high = Int64.shift_right_logical w 32 in
      let low = Int64.logand w 0xffffffffL in
      let high_opcode = Int64.to_int (bits high 0 7) in
      let tmp = Int64.to_int (bits high 7 5) in
      let low_opcode = Int64.to_int (bits low 0 7) in
      let low_funct3 = Int64.to_int (bits low 12 3) in
      let rd = Int64.to_int (bits low 7 5) in
      let rs1 = Int64.to_int (bits low 15 5) in
      let high_imm = sign_extend 20 (bits high 12 20) in
      let low_imm = sign_extend 12 (bits low 20 12) in
      if high_opcode <> 0x17 || rs1 <> tmp then None
      else
        let target =
          Asm_core.Expr.Const (Bigint.of_int64 (Int64.add (Int64.shift_left high_imm 12) low_imm))
        in
        match low_opcode with
        | 0x13 -> Some (Lowered.Pair { name = "la"; rd; tmp; target; kind = Addi })
        | 0x67 when rd = 1 && tmp = 1 ->
            Some (Lowered.Pair { name = "call"; rd; tmp; target; kind = Jalr })
        | 0x67 when rd = 0 && tmp = 6 ->
            Some (Lowered.Pair { name = "tail"; rd; tmp; target; kind = Jalr })
        | 0x03 when rd = tmp && low_funct3 = 3 ->
            Some (Lowered.Pair { name = "ld"; rd; tmp; target; kind = Load low_funct3 })
        | 0x07 ->
            Option.map
              (fun n -> Lowered.Pair { name = n; rd; tmp; target; kind = Fload low_funct3 })
              (f_load_name low_funct3)
        | _ -> None
    in
    C.choice ~name:P.name
      [
        C.alt ~label:"word" ~priority:1
          (C.iso_fun ~name:(P.name ^ "-word") ~encode:(encoded_bits 4) ~decode:decode_word
             (C.field ~width:32 "instruction"));
        C.alt ~label:"pair" ~priority:0
          (C.iso_fun ~name:(P.name ^ "-pair") ~encode:(encoded_bits 8) ~decode:decode_pair
             (C.field ~width:64 "auipc-i-pair"));
      ]

  let evaluate_fixup kind ~place ~target =
    let d = Int64.sub target place in
    let aligned what bits =
      if Int64.logand d 1L <> 0L then Error (diag ~pos:__POS__ (`Immediate_alignment what))
      else if not (fits_signed bits d) then Error (diag ~pos:__POS__ (`Immediate_range what))
      else Ok d
    in
    let hi v = Int64.shift_right (Int64.add v 0x800L) 12 in
    let lo v = Int64.sub v (Int64.shift_left (hi v) 12) in
    let rv32_absolute () =
      if xlen <> 32 then Ok target
      else if Int64.compare target (-0x80000000L) < 0 || Int64.compare target 0xffffffffL > 0 then
        Error (diag ~pos:__POS__ (`Immediate_range "RV32 address"))
      else if Int64.compare target 0x7fffffffL > 0 then Ok (Int64.sub target 0x100000000L)
      else Ok target
    in
    match kind with
    | Abs32 ->
        if Int64.compare target (-0x80000000L) >= 0 && Int64.compare target 0xffffffffL <= 0 then
          Ok target
        else Error (diag ~pos:__POS__ (`Immediate_range "32-bit address"))
    | Abs64 -> Ok target
    | Branch13 -> aligned "branch" 13
    | Jal21 -> aligned "jal" 21
    | Pcrel_hi20 | Call_hi20 ->
        if fits_signed 32 d then Ok (hi d) else Error (diag ~pos:__POS__ (`Immediate_range "auipc"))
    | Pcrel_lo12_i | Pcrel_lo12_s | Call_lo12_i ->
        if fits_signed 32 d then Ok (lo d) else Error (diag ~pos:__POS__ (`Immediate_range "pcrel"))
    | Abs_hi20 -> Result.map hi (rv32_absolute ())
    | Abs_lo12_i | Abs_lo12_s -> Result.map lo (rv32_absolute ())

  let data_widths =
    [
      (".byte", 1);
      (".short", 2);
      (".hword", 2);
      (".word", 4);
      (".4byte", 4);
      (".long", 4);
      (".quad", 8);
      (".dword", 8);
    ]

  let data_fixup ~width =
    match width with
    | 4 -> Ok Abs32
    | 8 when xlen = 64 -> Ok Abs64
    | _ -> Error (diag ~pos:__POS__ (`No_data_relocation width))

  let nop_bytes ~length =
    if length mod 4 <> 0 then Error (diag ~pos:__POS__ `Padding_not_word_multiple)
    else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x13\x00\x00\x00")))

  (* Measured (M3 §3/§5, .ai/asm_plan.md §12): a linker-inserted merge gap in an
     executable section is plain zero fill on both riscv32 and riscv64, not NOP
     fill - unlike the *assembler's own* end-of-section padding, which uses
     real [c.nop]/[nop] via [nop_bytes] above. *)
  let merge_fill = None
end
