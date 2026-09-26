(* One table-driven fixed-format RISC-V form (DEC-RV-TABLE). The rows
   themselves are generated from the captured riscv-opcodes export into
   Riscv_table_rows by [compcert_tools isa-table riscv-emit]; nothing here
   reads the capture at run time. *)

(* An immediate whose value bits are scattered over the instruction:
   [bits] maps instruction bit positions to value bit indices. The value must
   fit [width] bits (two's complement when [signed]), be a multiple of
   [2 ^ scale], and be non-zero when [nonzero]. *)
type scatter = { signed : bool; nonzero : bool; scale : int; width : int; bits : (int * int) list }

type operand =
  | Gpr of { lsb : int; nonzero : bool }  (** a 5-bit integer register field *)
  | Fpr of { lsb : int }  (** a 5-bit floating-point register field *)
  | Uimm of { lsb : int; width : int }  (** a contiguous unsigned immediate field *)
  | Simm of { lsb : int; width : int }  (** a contiguous two's-complement immediate field *)
  | Fixed_gpr of int
      (** a register the spelling names but the encoding fixes, e.g. [sspopchk x1] *)
  | Rm of { lsb : int; default : int }
      (** a 3-bit rounding mode; the GNU spelling may omit it (always last), and
          then [default] is encoded *)
  | Tied of { lsb : int }
      (** a register field that repeats the previous operand's register, e.g.
          [fmv.h rd, rs] is [fsgnj.h rd, rs, rs]; not spelled *)
  | Mem_i of { base : int }  (** [imm12(base)] with the signed offset in bits 31:20 *)
  | Keyword of string
      (** a word the spelling requires but the encoding fixes, e.g. the [rtz] of
          [fcvtmod.w.d rd, rs, rtz] *)
  | Fli of { lsb : int }
      (** Zfa's [fli.*] constant, spelled by name or value, encoded as its
          5-bit index into {!fli_constants} *)
  | Mem_zero of { base : int }  (** [(base)] or [0(base)]: an address with no offset field *)
  | Mem_hi of { base : int }
      (** [imm12(base)] whose offset is a multiple of 32, stored as bits 11:5 in 31:25
          (Zicbop's prefetch hints) *)
  | Gpr_pair of { lsb : int; below : int }
      (** an integer register that names an even/odd pair when XLEN is below [below]
          (Zacas's [amocas.d] on RV32, [amocas.q] on RV64), so must then be even *)
  | Creg of { lsb : int }  (** a compressed 3-bit integer register field, x8..x15 *)
  | Cfreg of { lsb : int }  (** a compressed 3-bit floating-point register field, f8..f15 *)
  | Gpr_except of { lsb : int; except : int list }
      (** an integer register field that excludes some registers ([c.lui]: x0, x2) *)
  | Scatter of scatter  (** an immediate spread over the instruction *)
  | Cmem of { base : int; offset : scatter }  (** [offset(rs1')] with a compressed base *)
  | Spmem of { offset : scatter }  (** [offset(sp)] *)
  | Cui of { hi : int; lo : int }
      (** [c.lui]'s immediate: 1..31 or 0xfffe0..0xfffff, bit 5 at [hi], bits 4..0 from [lo] *)
  | Sreg of { lsb : int }  (** Zcmp's 3-bit s-register field: s0, s1, s2..s7 *)
  | Rlist of { lsb : int }  (** Zcmp's register list, [{ra}], [{ra, s0}], [{ra, s0-sN}] *)
  | Stack_adj of { rlist : int; spimm : int; push : bool }
      (** Zcmp's stack adjustment, which depends on the register list at [rlist] and on XLEN *)
  | Uimm_min of { lsb : int; width : int; min : int }  (** an unsigned field with a lower bound *)
  | Mem_s of { base : int }
      (** [imm12(base)] with the signed offset split into bits 31:25 and 11:7 *)

type row = {
  mnemonic : string;
  mask : int64;
  match_ : int64;
  operands : operand list;  (** in GNU assembler syntax order *)
  xlen : int;  (** 0 when the form exists on both RV32 and RV64 *)
  feature : string;  (** the GNU [-march] extension that enables it *)
  source : string;  (** the riscv-opcodes record, [<extension>/<native name>] *)
}

(* Zfa's [fli] constant table, index order (RISC-V Zfa specification); the
   spelling printed for each. Index 1 is the format's smallest normal number,
   which has no format-independent value, so it is only ever spelled [min]. *)
let fli_constants =
  [|
    "-1.0";
    "min";
    "1.52587890625e-05";
    "3.0517578125e-05";
    "0.00390625";
    "0.0078125";
    "0.0625";
    "0.125";
    "0.25";
    "0.3125";
    "0.375";
    "0.4375";
    "0.5";
    "0.625";
    "0.75";
    "0.875";
    "1.0";
    "1.25";
    "1.5";
    "1.75";
    "2.0";
    "2.5";
    "3.0";
    "4.0";
    "8.0";
    "16.0";
    "128.0";
    "256.0";
    "32768.0";
    "65536.0";
    "inf";
    "nan";
  |]

let fli_index text =
  let named = function "min" -> Some 1 | "inf" -> Some 30 | "nan" -> Some 31 | _ -> None in
  match named text with
  | Some _ as i -> i
  | None -> (
      match float_of_string_opt text with
      | None -> None
      | Some v ->
          let rec go i =
            if i >= Array.length fli_constants then None
            else if i <> 1 && i < 30 && float_of_string fli_constants.(i) = v then Some i
            else go (i + 1)
          in
          go 0)

(* Zcmp register lists: the 4-bit [rlist] code for each spelling (whitespace
   removed), and the number of registers it saves. *)
let rlist_code text =
  match text with
  | "{ra}" -> Some 4
  | "{ra,s0}" -> Some 5
  | _ ->
      let prefix = "{ra,s0-s" in
      let n = String.length text and k = String.length prefix in
      if n > k + 1 && String.sub text 0 k = prefix && text.[n - 1] = '}' then
        match int_of_string_opt (String.sub text k (n - k - 1)) with
        | Some last when last >= 1 && last <= 9 -> Some (last + 5)
        | Some 11 -> Some 15
        | _ -> None
      else None

let rlist_text code =
  match code with
  | 4 -> Some "{ra}"
  | 5 -> Some "{ra, s0}"
  | c when c >= 6 && c <= 14 -> Some (Printf.sprintf "{ra, s0-s%d}" (c - 5))
  | 15 -> Some "{ra, s0-s11}"
  | _ -> None

(* The stack a push/pop moves at spimm = 0: the saved registers rounded up to 16 bytes. *)
let rlist_base ~xlen code =
  let registers = if code = 15 then 13 else code - 3 in
  ((registers * (xlen / 8)) + 15) / 16 * 16

let sreg_code n =
  if n = 8 || n = 9 then Some (n - 8) else if n >= 18 && n <= 23 then Some (n - 16) else None

let sreg_register c = if c < 2 then c + 8 else c + 16
