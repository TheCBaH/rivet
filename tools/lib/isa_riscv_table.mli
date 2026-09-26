(** The table-driven RISC-V forms (DEC-RV-TABLE): which captured
    riscv-opcodes records become rows of the assembler's generated
    [asm/targets/riscv_family/riscv_table_rows.ml], and the reviewed rule that
    gives each of their fields an operand domain.

    One rule serves three consumers, so they cannot disagree: the emitter
    that writes the checked-in rows, the normalizer ({!Isa_norm_riscv}) that
    turns the same records into {!Isa_norm_model.form}s, and the case
    generator ({!Isa_gen_difficult.table_entries}). A record is a row only
    when its extension (and, where listed, its native name) is on the
    allowlist and every variable field is a contiguous field with a known
    domain; everything else stays with the hand-written encoder or blocked. *)

type operand =
  | Gpr of { field : string; lsb : int; nonzero : bool }
  | Fpr of { field : string; lsb : int }
  | Uimm of { field : string; lsb : int; width : int }
  | Fixed_gpr of int  (** spelled in the source but fixed by the encoding, e.g. [sspopchk x1] *)
  | Rm of { lsb : int; default : int }  (** optional trailing rounding mode *)
  | Tied of { lsb : int }  (** repeats the previous register, unspelled ([rs2=rs1]) *)
  | Mem_i of { base : int }  (** [imm12(base)], offset in bits 31:20 *)
  | Mem_s of { base : int }  (** [imm12(base)], offset split into 31:25 and 11:7 *)
  | Keyword of string  (** a word the spelling requires but the encoding fixes *)
  | Fli of { lsb : int }  (** Zfa's fli constant, spelled by name or value, encoded as its index *)
  | Mem_zero of { base : int }  (** [(base)]: an address with no offset field *)
  | Mem_hi of { base : int }  (** [imm12(base)], offset a multiple of 32 in bits 31:25 *)
  | Gpr_pair of { field : string; lsb : int; below : int }
      (** an even/odd register pair when XLEN is below [below] *)
  | Fence_set of { field : string; lsb : int }  (** [fence]'s i/o/r/w set (hand-encoded) *)
  | Creg of { field : string; lsb : int }  (** compressed x8..x15 *)
  | Cfreg of { field : string; lsb : int }  (** compressed f8..f15 *)
  | Gpr_except of { field : string; lsb : int; except : int list }
  | Scatter of scatter  (** an immediate spread over the instruction *)
  | Cmem of { base : int; offset : scatter }  (** [offset(rs1')] *)
  | Spmem of { offset : scatter }  (** [offset(sp)] *)
  | Cui of { hi : int; lo : int }  (** [c.lui]'s upper immediate *)
  | Sreg of { field : string; lsb : int }  (** Zcmp s0..s7 *)
  | Rlist of { lsb : int }  (** Zcmp register list *)
  | Stack_adj of { rlist : int; spimm : int; push : bool }  (** Zcmp stack adjustment *)
  | Uimm_min of { field : string; lsb : int; width : int; min : int }

and scatter = {
  signed : bool;
  nonzero : bool;
  scale : int;  (** implicit low zero bits *)
  width : int;
  bits : (int * int) list;  (** (instruction bit, value bit) *)
}

type spec = {
  record_id : string;
  native_name : string;
  mnemonic : string;  (** the GNU spelling: the native name without a [.xN] register suffix *)
  extension : string;
  width_bits : int;
  mask : string;
  match_ : string;
  operands : operand list;  (** GNU syntax order *)
  xlen : int;  (** 0 unless the extension file is [rv32_*]/[rv64_*] *)
  feature : string;  (** the enabling extension, e.g. ["zimop"], ["zba"], ["f"] *)
  isa : string;  (** the [-march] ISA string after [rv32]/[rv64], e.g. ["imf_zfh"] *)
  ordering : bool;  (** aq/rl fields, spelled as [.aq]/[.rl]/[.aqrl] mnemonic suffixes *)
}

val spec_of_record : Isa_source_record.t -> spec option
(** [Some] exactly for allowlisted, table-expressible records. *)

val form :
  requirement:Isa_norm_model.requirement -> Isa_source_record.t -> spec -> Isa_norm_model.form
(** The normalized form of a table record: operands named after their
    fields, GNU syntax in field order, the fixed register as a literal. *)

val march : Target.t -> spec -> string list
(** The GAS configuration a case of this row needs, e.g.
    [["-march=rv64im_zimop"; "-mabi=lp64"; "-mno-relax"]]. *)

val rows_path : Repo.t -> Fpath.t
(** [asm/targets/riscv_family/riscv_table_rows.ml]. *)

val emit : Repo.t -> (string, Tool_error.t) Err.t
(** The generated OCaml source for {!rows_path}, from both committed
    riscv-opcodes exports: a record present in both profiles becomes one row
    with [xlen = 0], otherwise the row carries its profile's XLEN. *)

val run_emit : Repo.t -> Command.t
(** Write {!emit} to {!rows_path}. *)

val run_check : Repo.t -> Command.t
(** Fail unless {!rows_path} equals a fresh {!emit}. Toolchain-free. *)
