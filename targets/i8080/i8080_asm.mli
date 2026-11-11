(** Typed builders for the Intel 8080 instructions, and programs of them.

    A builder is named by its mnemonic and returns the {!I8080_encode.Instruction.t} the
    encoder lowers: [let open I8080_asm in [ lxi sp 0xf000; mvi a 42; sta 0x8000; hlt () ]]. A
    builder only selects the descriptor-table entry and wraps its operands; no opcode byte is
    listed here. Immediates and addresses are [int] and are range-checked when the program is
    assembled; the [_e] variant of a builder with an immediate takes an {!Asm_core.Expr.t}, for
    labels and other symbols. The OCaml keyword [in] is [in_].

    Byte registers and register pairs are different types, and each pair class is closed, so
    [mov bc a], [push sp] and [ldax hl] do not compile. [mov m m] (the slot of [hlt]) and the
    ranges of immediates are diagnostics of assembly. *)

type t = I8080_encode.Instruction.t

(** {1 Operands} *)

type reg = I8080_encode.Reg.t
(** [b c d e h l m a]; [m] is the memory byte at [hl]. *)

type pair = [ `Bc | `De | `Hl | `Sp ]
(** [bc de hl sp]: [lxi], [inx], [dcx], [dad]. *)

type stack_pair = [ `Bc | `De | `Hl | `Psw ]
(** [bc de hl psw]: [push], [pop]. *)

type ind_pair = [ `Bc | `De ]
(** [bc de]: [ldax], [stax]. *)

val b : reg
val c : reg
val d : reg
val e : reg
val h : reg
val l : reg
val m : reg
val a : reg
val bc : [> `Bc ]
val de : [> `De ]
val hl : [> `Hl ]
val sp : [> `Sp ]
val psw : [> `Psw ]

(** {1 Expressions} *)

val symbol : string -> Asm_core.Expr.t
val const : int -> Asm_core.Expr.t
val plus : Asm_core.Expr.t -> Asm_core.Expr.t -> Asm_core.Expr.t
val minus : Asm_core.Expr.t -> Asm_core.Expr.t -> Asm_core.Expr.t

val low : Asm_core.Expr.t -> Asm_core.Expr.t
(** The low byte of an address expression. *)

val high : Asm_core.Expr.t -> Asm_core.Expr.t
(** The high byte of an address expression. *)

(** {1 Instructions} *)

val nop : unit -> t
val hlt : unit -> t
val ret : unit -> t
val rnz : unit -> t
val rz : unit -> t
val rnc : unit -> t
val rc : unit -> t
val rpo : unit -> t
val rpe : unit -> t
val rp : unit -> t
val rm : unit -> t
val xchg : unit -> t
val xthl : unit -> t
val sphl : unit -> t
val pchl : unit -> t
val ei : unit -> t
val di : unit -> t
val daa : unit -> t
val cma : unit -> t
val stc : unit -> t
val cmc : unit -> t
val rlc : unit -> t
val rrc : unit -> t
val ral : unit -> t
val rar : unit -> t
val mov : reg -> reg -> t
val inr : reg -> t
val dcr : reg -> t
val add : reg -> t
val adc : reg -> t
val sub : reg -> t
val sbb : reg -> t
val ana : reg -> t
val xra : reg -> t
val ora : reg -> t
val cmp : reg -> t
val mvi : reg -> int -> t
val mvi_e : reg -> Asm_core.Expr.t -> t
val lxi : pair -> int -> t
val lxi_e : pair -> Asm_core.Expr.t -> t
val inx : pair -> t
val dcx : pair -> t
val dad : pair -> t
val ldax : ind_pair -> t
val stax : ind_pair -> t
val push : stack_pair -> t
val pop : stack_pair -> t

val rst : int -> t
(** The restart vector, 0 to 7. *)

val adi : int -> t
val adi_e : Asm_core.Expr.t -> t
val aci : int -> t
val aci_e : Asm_core.Expr.t -> t
val sui : int -> t
val sui_e : Asm_core.Expr.t -> t
val sbi : int -> t
val sbi_e : Asm_core.Expr.t -> t
val ani : int -> t
val ani_e : Asm_core.Expr.t -> t
val xri : int -> t
val xri_e : Asm_core.Expr.t -> t
val ori : int -> t
val ori_e : Asm_core.Expr.t -> t
val cpi : int -> t
val cpi_e : Asm_core.Expr.t -> t
val in_ : int -> t
val in_e : Asm_core.Expr.t -> t
val out : int -> t
val out_e : Asm_core.Expr.t -> t
val jmp : int -> t
val jmp_e : Asm_core.Expr.t -> t
val jnz : int -> t
val jnz_e : Asm_core.Expr.t -> t
val jz : int -> t
val jz_e : Asm_core.Expr.t -> t
val jnc : int -> t
val jnc_e : Asm_core.Expr.t -> t
val jc : int -> t
val jc_e : Asm_core.Expr.t -> t
val jpo : int -> t
val jpo_e : Asm_core.Expr.t -> t
val jpe : int -> t
val jpe_e : Asm_core.Expr.t -> t
val jp : int -> t
val jp_e : Asm_core.Expr.t -> t
val jm : int -> t
val jm_e : Asm_core.Expr.t -> t
val call : int -> t
val call_e : Asm_core.Expr.t -> t
val cnz : int -> t
val cnz_e : Asm_core.Expr.t -> t
val cz : int -> t
val cz_e : Asm_core.Expr.t -> t
val cnc : int -> t
val cnc_e : Asm_core.Expr.t -> t
val cc : int -> t
val cc_e : Asm_core.Expr.t -> t
val cpo : int -> t
val cpo_e : Asm_core.Expr.t -> t
val cpe : int -> t
val cpe_e : Asm_core.Expr.t -> t
val cp : int -> t
val cp_e : Asm_core.Expr.t -> t
val cm : int -> t
val cm_e : Asm_core.Expr.t -> t
val sta : int -> t
val sta_e : Asm_core.Expr.t -> t
val lda : int -> t
val lda_e : Asm_core.Expr.t -> t
val shld : int -> t
val shld_e : Asm_core.Expr.t -> t
val lhld : int -> t
val lhld_e : Asm_core.Expr.t -> t

(** {1 Program items} *)

type item
(** An instruction, a label or data, in program order. *)

val insn : t -> item
(** Any instruction value, including one built from the generic {!I8080_encode.Instruction.Insn};
    a malformed one is a diagnostic of assembly. *)

val label : string -> item
(** Defines a symbol at the current address; every label is visible in
    {!Program.image}[.symbols]. *)

val db : int list -> item
val db_e : Asm_core.Expr.t list -> item

val dw : int list -> item
(** Little-endian words. *)

val dw_e : Asm_core.Expr.t list -> item

val ascii : string -> item
(** The bytes of the string. *)

val space : int -> item
(** That many zero bytes. *)

module Items : sig
  (** The same builders returning {!item}, so a program is a plain list and needs no {!insn}:
      [let open I8080_asm.Items in [ lxi sp 0xf000; label "loop"; dcr b; jnz_e (symbol "loop"); hlt () ]].
      The operands, the expression helpers and the data items are those above. *)

  val b : reg
  val c : reg
  val d : reg
  val e : reg
  val h : reg
  val l : reg
  val m : reg
  val a : reg
  val bc : [> `Bc ]
  val de : [> `De ]
  val hl : [> `Hl ]
  val sp : [> `Sp ]
  val psw : [> `Psw ]
  val symbol : string -> Asm_core.Expr.t
  val const : int -> Asm_core.Expr.t
  val plus : Asm_core.Expr.t -> Asm_core.Expr.t -> Asm_core.Expr.t
  val minus : Asm_core.Expr.t -> Asm_core.Expr.t -> Asm_core.Expr.t
  val low : Asm_core.Expr.t -> Asm_core.Expr.t
  val high : Asm_core.Expr.t -> Asm_core.Expr.t
  val nop : unit -> item
  val hlt : unit -> item
  val ret : unit -> item
  val rnz : unit -> item
  val rz : unit -> item
  val rnc : unit -> item
  val rc : unit -> item
  val rpo : unit -> item
  val rpe : unit -> item
  val rp : unit -> item
  val rm : unit -> item
  val xchg : unit -> item
  val xthl : unit -> item
  val sphl : unit -> item
  val pchl : unit -> item
  val ei : unit -> item
  val di : unit -> item
  val daa : unit -> item
  val cma : unit -> item
  val stc : unit -> item
  val cmc : unit -> item
  val rlc : unit -> item
  val rrc : unit -> item
  val ral : unit -> item
  val rar : unit -> item
  val mov : reg -> reg -> item
  val inr : reg -> item
  val dcr : reg -> item
  val add : reg -> item
  val adc : reg -> item
  val sub : reg -> item
  val sbb : reg -> item
  val ana : reg -> item
  val xra : reg -> item
  val ora : reg -> item
  val cmp : reg -> item
  val mvi : reg -> int -> item
  val mvi_e : reg -> Asm_core.Expr.t -> item
  val lxi : pair -> int -> item
  val lxi_e : pair -> Asm_core.Expr.t -> item
  val inx : pair -> item
  val dcx : pair -> item
  val dad : pair -> item
  val ldax : ind_pair -> item
  val stax : ind_pair -> item
  val push : stack_pair -> item
  val pop : stack_pair -> item
  val rst : int -> item
  val adi : int -> item
  val adi_e : Asm_core.Expr.t -> item
  val aci : int -> item
  val aci_e : Asm_core.Expr.t -> item
  val sui : int -> item
  val sui_e : Asm_core.Expr.t -> item
  val sbi : int -> item
  val sbi_e : Asm_core.Expr.t -> item
  val ani : int -> item
  val ani_e : Asm_core.Expr.t -> item
  val xri : int -> item
  val xri_e : Asm_core.Expr.t -> item
  val ori : int -> item
  val ori_e : Asm_core.Expr.t -> item
  val cpi : int -> item
  val cpi_e : Asm_core.Expr.t -> item
  val in_ : int -> item
  val in_e : Asm_core.Expr.t -> item
  val out : int -> item
  val out_e : Asm_core.Expr.t -> item
  val jmp : int -> item
  val jmp_e : Asm_core.Expr.t -> item
  val jnz : int -> item
  val jnz_e : Asm_core.Expr.t -> item
  val jz : int -> item
  val jz_e : Asm_core.Expr.t -> item
  val jnc : int -> item
  val jnc_e : Asm_core.Expr.t -> item
  val jc : int -> item
  val jc_e : Asm_core.Expr.t -> item
  val jpo : int -> item
  val jpo_e : Asm_core.Expr.t -> item
  val jpe : int -> item
  val jpe_e : Asm_core.Expr.t -> item
  val jp : int -> item
  val jp_e : Asm_core.Expr.t -> item
  val jm : int -> item
  val jm_e : Asm_core.Expr.t -> item
  val call : int -> item
  val call_e : Asm_core.Expr.t -> item
  val cnz : int -> item
  val cnz_e : Asm_core.Expr.t -> item
  val cz : int -> item
  val cz_e : Asm_core.Expr.t -> item
  val cnc : int -> item
  val cnc_e : Asm_core.Expr.t -> item
  val cc : int -> item
  val cc_e : Asm_core.Expr.t -> item
  val cpo : int -> item
  val cpo_e : Asm_core.Expr.t -> item
  val cpe : int -> item
  val cpe_e : Asm_core.Expr.t -> item
  val cp : int -> item
  val cp_e : Asm_core.Expr.t -> item
  val cm : int -> item
  val cm_e : Asm_core.Expr.t -> item
  val sta : int -> item
  val sta_e : Asm_core.Expr.t -> item
  val lda : int -> item
  val lda_e : Asm_core.Expr.t -> item
  val shld : int -> item
  val shld_e : Asm_core.Expr.t -> item
  val lhld : int -> item
  val lhld_e : Asm_core.Expr.t -> item
  val insn : t -> item
  val label : string -> item
  val db : int list -> item
  val db_e : Asm_core.Expr.t list -> item
  val dw : int list -> item
  val dw_e : Asm_core.Expr.t list -> item
  val ascii : string -> item
  val space : int -> item
end

module Program : sig
  type image = {
    origin : int;
    bytes : string;
    symbols : (string * int) list;  (** every label, by address then name *)
  }

  val assemble : ?origin:int -> ?unit_name:string -> item list -> image Foundation.Diag.t
  (** Lowers, lays out and binds the items at [origin] (default 0) through the shared direct
      pipeline stages. Diagnostics: duplicate and undefined labels, operand shapes, ranges,
      an origin or image outside the 64 KiB address space. *)

  val symbol : image -> string -> int option

  val bind : origin:int -> Image.laid_out -> image Foundation.Diag.t
  (** The last step of {!assemble}, for a caller that planned its own module: one segment bound at
      [origin], with its exported symbols. *)
end
