(* The reference snippets of execution ABI v1 §16.3, written as ASTs.

   {!Asm_oracle.Snippet_bytes} states each snippet as a byte string with a
   comment naming the assembly it encodes. Nothing checked that the comment and
   the bytes agreed: a transposed nibble would have been invisible, and the
   comment is the only thing that says what the snippet is *for*.

   This file writes the same snippets the other way round - as normalized ASTs -
   and pushes each through this project's own [lower_instruction] and [encode].
   The test beside it asserts the result equals the frozen constant. The comment
   becomes executable, and the assembler is dogfooded on the one corpus whose
   correct answer was established independently of it.

   The direction of evidence matters and is deliberately one-way. The frozen
   table stays the ground truth: it is what {!Conform} runs under QEMU, it was
   never produced by this assembler, and this file cannot change it. What is
   under test here is the assembler.

   {1 Why some entries are [Needs]}

   asm/docs/contracts.md §2.3 freezes M1 at the 24 encoding forms the four
   return-42 fixtures emit; §4.3 lists what is deliberately absent, PC-relative
   expressions and fixups among it. Most §16.3 snippets are outside that set -
   they exist to trap, to spin, to test SP alignment - so they cannot be
   assembled today and are recorded as [Needs] with the form they want.

   That is the point of listing them rather than dropping them. §4.3's list of
   deliberate absences is prose; this corpus is the same claim as data, checked
   on every run, and each M2 form moves an entry from [Needs] to a hexdump that
   has to match. An entry that vanished would take its scope statement with it. *)

module Sb = Asm_oracle.Snippet_bytes

type built =
  | Assembled of { bytes : string; canonical : string list }
      (** [canonical] is one line per *lowered* instruction, which is not always one line per
          normalized one: lowering is one-to-many. It is what Part C hands to GNU as, so the same
          AST produces our bytes and the text the reference toolchain assembles. *)
  | Refused of string
      (** the assembler rejected an entry this corpus expected to assemble - always a failure, never
          a scope statement *)
  | Needs of string
      (** declared out of M1 scope, naming the form. Not a failure; the inventory reports it. *)

type entry = { name : string; frozen : Sb.t -> string; built : built }

(* {1 The staged assembler}

   Over TARGET, so the four corpora below share one path and none can quietly
   take a shortcut. Entries are built at the *normalized* stage and pushed
   through both remaining stages, so this exercises lowering as well as
   encoding - an encoder fed lowered forms directly would never notice that
   [simplify] and [lower] disagree about which operands a mnemonic takes. *)

module Make (T : Target_intf.Target.TARGET) = struct
  exception Refuse of string

  let assemble (insns : T.Instruction.t list) =
    let one i =
      match T.lower_instruction T.default_state i with
      | Error d -> raise (Refuse (Foundation.Diagnostic.message d))
      | Ok lowered ->
          List.map
            (fun l ->
              match T.encode l with
              | Error d -> raise (Refuse (Foundation.Diagnostic.message d))
              | Ok (bytes, _form, _placements) -> (bytes, Fmt.to_to_string T.Lowered.pp l))
            lowered
    in
    match List.concat_map one insns with
    | parts ->
        Assembled { bytes = String.concat "" (List.map fst parts); canonical = List.map snd parts }
    | exception Refuse m -> Refused m

  (* Padding is the target's answer too, and the [.align] fill is the one blob
     that reaches an image without passing through an instruction. *)
  let nop ~length =
    match T.nop_bytes ~length with
    | Ok b -> Ok b
    | Error d -> Error (Foundation.Diagnostic.message d)
end

(* {1 aarch64} *)

module Aarch64_corpus = struct
  module T = Aarch64
  module A = Make (Aarch64)

  let r n = Option.get (T.Reg.find n)
  let insn op ops : T.Instruction.t = { T.Instruction.op; ops }
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)
  let reg n = T.Operand.Reg (r n)

  let mem ?(writeback = false) base offset =
    T.Operand.Mem { T.Mem.base = r base; offset; writeback; pre = true }

  (* The CompCert 3.17 prologue/epilogue, which is exactly the M1 fixture. *)
  let return_n n =
    A.assemble
      [
        insn T.Opcode.Mov [ reg "x15"; reg "sp" ];
        insn T.Opcode.Stp [ reg "x15"; reg "x30"; mem ~writeback:true "sp" (-16L) ];
        insn T.Opcode.Movz [ reg "w0"; imm n ];
        insn T.Opcode.Ldr [ reg "x30"; mem "sp" 8L ];
        insn T.Opcode.Add [ reg "sp"; reg "sp"; imm 16 ];
        insn T.Opcode.Ret [ reg "x30" ];
      ]

  let entries =
    [
      { name = "return42"; frozen = (fun s -> s.Sb.return42); built = return_n 42 };
      { name = "return41"; frozen = (fun s -> s.Sb.return41); built = return_n 41 };
      {
        (* [mov x19, #0] is [movz x19, #0]: the same form as the fixture's
           [movz w0, #42] with sf set, which the codec already carries. So this
           one needs no new encoding form, only the 64-bit register. *)
        name = "callee_clobber";
        frozen = (fun s -> s.Sb.callee_clobber);
        built =
          A.assemble
            [
              insn T.Opcode.Movz [ reg "x19"; imm 0 ];
              insn T.Opcode.Movz [ reg "w0"; imm 42 ];
              insn T.Opcode.Ret [ reg "x30" ];
            ];
      };
      { name = "trap"; frozen = (fun s -> s.Sb.trap); built = Needs "udf - no trap form" };
      {
        name = "spin";
        frozen = (fun s -> s.Sb.spin);
        built = Needs "b . - PC-relative branch, §4.3 defers fixups to M2";
      };
      {
        name = "sp_align";
        frozen = (fun s -> s.Sb.sp_align);
        built = Needs "and/cmp/b.eq - three forms plus a PC-relative branch";
      };
      {
        name = "stack_full";
        frozen = (fun s -> s.Sb.stack_full);
        built = Needs "sub-imm with lsl #12, and strb";
      };
      {
        name = "stack_over";
        frozen = (fun s -> s.Sb.stack_over);
        built = Needs "sub-imm with lsl #12, and strb";
      };
      {
        name = "sp_corrupt";
        frozen = (fun s -> s.Sb.sp_corrupt);
        built = Needs "sub-imm - A64 encodes it separately from add-imm";
      };
    ]

  let nops = A.nop
end

(* {1 arm} *)

module Arm_corpus = struct
  module T = Arm
  module A = Make (Arm)

  let r n = Option.get (T.Reg.find n)
  let insn op ops : T.Instruction.t = { T.Instruction.op; ops }
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)
  let reg n = T.Operand.Reg (r n)
  let mem base offset = T.Operand.Mem { T.Mem.base = r base; offset; writeback = false; pre = true }

  let return_n n =
    A.assemble
      [
        insn T.Opcode.Mov [ reg "r12"; reg "sp" ];
        insn T.Opcode.Sub [ reg "sp"; reg "sp"; imm 8 ];
        insn T.Opcode.Str [ reg "r12"; mem "sp" 0L ];
        insn T.Opcode.Str [ reg "lr"; mem "sp" 4L ];
        insn T.Opcode.Mov [ reg "r0"; imm n ];
        insn T.Opcode.Ldr [ reg "lr"; mem "sp" 4L ];
        insn T.Opcode.Add [ reg "sp"; reg "sp"; imm 8 ];
        insn T.Opcode.Bx [ reg "lr" ];
      ]

  let entries =
    [
      { name = "return42"; frozen = (fun s -> s.Sb.return42); built = return_n 42 };
      { name = "return41"; frozen = (fun s -> s.Sb.return41); built = return_n 41 };
      {
        (* [mov r4, #0] and [mov r0, #42] are both the fixture's
           modified-immediate form, and [bx lr] is the fixture's. *)
        name = "callee_clobber";
        frozen = (fun s -> s.Sb.callee_clobber);
        built =
          A.assemble
            [
              insn T.Opcode.Mov [ reg "r4"; imm 0 ];
              insn T.Opcode.Mov [ reg "r0"; imm 42 ];
              insn T.Opcode.Bx [ reg "lr" ];
            ];
      };
      {
        (* [sub sp, sp, #16] is the fixture's sub-immediate. *)
        name = "sp_corrupt";
        frozen = (fun s -> s.Sb.sp_corrupt);
        built =
          A.assemble
            [
              insn T.Opcode.Sub [ reg "sp"; reg "sp"; imm 16 ];
              insn T.Opcode.Mov [ reg "r0"; imm 42 ];
              insn T.Opcode.Bx [ reg "lr" ];
            ];
      };
      { name = "trap"; frozen = (fun s -> s.Sb.trap); built = Needs "udf - no trap form" };
      {
        name = "spin";
        frozen = (fun s -> s.Sb.spin);
        built = Needs "b . - PC-relative branch, §4.3 defers fixups to M2";
      };
      {
        name = "sp_align";
        frozen = (fun s -> s.Sb.sp_align);
        built = Needs "and/cmp plus the conditional-execution suffixes moveq/movne";
      };
      { name = "stack_full"; frozen = (fun s -> s.Sb.stack_full); built = Needs "strb" };
      { name = "stack_over"; frozen = (fun s -> s.Sb.stack_over); built = Needs "strb" };
    ]

  let nops = A.nop
end

(* {1 x86}

   The prologue is written once for both modes. That is possible because
   [X86_family.Make] re-exports the family's constructor modules rather than
   redeclaring them, so [X86_32.Instruction.t] and [X86_64.Instruction.t] are
   the *same* type and one builder produces values both assemblers accept. §11
   gives the modes different frame adjustments and different operand widths;
   that is all the parameterization the shape needs.

   The x86 entries below are shared for the same reason: the two modes need the
   same missing forms, and stating that twice would let the two lists drift. *)

module X86_shared = struct
  open X86_family

  let insn op width ops : Instruction.t = { Instruction.op; width; ops }
  let imm n = Operand.Imm (Foundation.Bigint.of_int n)

  (* [frame] is the §11 stack adjustment: 8 on x86-64 and 12 on x86-32, because
     the return address differs in width and the ABI wants the stack 16-byte
     aligned at the call. Getting it wrong is what the sp_align case exists to
     catch, so it is a parameter here rather than a constant. *)
  let return_n ~sp ~ax ~eax ~width ~frame n =
    [
      insn Opcode.Sub width [ imm frame; Operand.Reg sp ];
      insn Opcode.Lea width [ Operand.Mem (Mem.of_base ~disp:16L sp); Operand.Reg ax ];
      insn Opcode.Mov width [ Operand.Reg ax; Operand.Mem (Mem.of_base sp) ];
      (* The one line where the two modes agree exactly: [movl $42, %eax] is
         byte-identical in 32- and 64-bit mode, which is why the fixtures use it
         to pin that fact. So it is 32-bit wide in both, and takes [%eax] rather
         than the mode's own accumulator. *)
      insn Opcode.Mov 32 [ imm n; Operand.Reg eax ];
      insn Opcode.Add width [ imm frame; Operand.Reg sp ];
      insn Opcode.Ret width [];
    ]

  let entries return_n =
    [
      { name = "return42"; frozen = (fun s -> s.Sb.return42); built = return_n 42 };
      { name = "return41"; frozen = (fun s -> s.Sb.return41); built = return_n 41 };
      { name = "trap"; frozen = (fun s -> s.Sb.trap); built = Needs "ud2" };
      {
        name = "spin";
        frozen = (fun s -> s.Sb.spin);
        built = Needs "jmp . - PC-relative branch, §4.3 defers fixups to M2";
      };
      {
        name = "sp_align";
        frozen = (fun s -> s.Sb.sp_align);
        built = Needs "and/cmp/jcc - two ALU extensions plus a PC-relative branch";
      };
      {
        name = "stack_full";
        frozen = (fun s -> s.Sb.stack_full);
        built = Needs "movb $0,(%reg) - byte-width store to memory";
      };
      {
        name = "stack_over";
        frozen = (fun s -> s.Sb.stack_over);
        built = Needs "movb $0,(%reg) - byte-width store to memory";
      };
      {
        name = "sp_corrupt";
        frozen = (fun s -> s.Sb.sp_corrupt);
        built = Needs "pop, and the indirect jmp *%reg";
      };
      { name = "callee_clobber"; frozen = (fun s -> s.Sb.callee_clobber); built = Needs "xor" };
    ]
end

module X86_64_corpus = struct
  module A = Make (X86_64)

  let r n = Option.get (X86_64.find_reg n)

  let entries =
    X86_shared.entries (fun n ->
        A.assemble
          (X86_shared.return_n ~sp:(r "rsp") ~ax:(r "rax") ~eax:(r "eax") ~width:64 ~frame:8 n))

  let nops = A.nop
end

module X86_32_corpus = struct
  module A = Make (X86_32)

  let r n = Option.get (X86_32.find_reg n)

  let entries =
    X86_shared.entries (fun n ->
        A.assemble
          (X86_shared.return_n ~sp:(r "esp") ~ax:(r "eax") ~eax:(r "eax") ~width:32 ~frame:12 n))

  let nops = A.nop
end

(* {1 The corpus}

   In the canonical target order every generated artifact and manifest in this
   project uses (tools/target-matrix.sh). *)

type target = {
  target : string;
  snippets : Sb.t;
  entries : entry list;
  nop : length:int -> (string, string) result;
  preamble : string list;
      (** what has to precede the instructions for both this assembler and GNU as to read the same
          file. Only ARM needs more than a section and a symbol, and it needs all of it: without
          [.arm] the reference toolchain may assemble Thumb, and comparing A32 bytes against T32
          bytes would report a difference that is not one. *)
}

let all =
  [
    {
      target = "x86_32";
      snippets = Sb.x86_32;
      entries = X86_32_corpus.entries;
      nop = X86_32_corpus.nops;
      preamble = [ ".text" ];
    };
    {
      target = "x86_64";
      snippets = Sb.x86_64;
      entries = X86_64_corpus.entries;
      nop = X86_64_corpus.nops;
      preamble = [ ".text" ];
    };
    {
      target = "arm";
      snippets = Sb.arm;
      entries = Arm_corpus.entries;
      nop = Arm_corpus.nops;
      preamble = [ ".syntax unified"; ".arch armv7-a"; ".fpu vfpv3-d16"; ".arm"; ".text" ];
    };
    {
      target = "aarch64";
      snippets = Sb.aarch64;
      entries = Aarch64_corpus.entries;
      nop = Aarch64_corpus.nops;
      preamble = [ ".text" ];
    };
  ]

(* {1 Assembly text}

   The third rendering of one AST. The corpus already produces our bytes and a
   hexdump; this produces the file GNU as reads, from the same canonical
   printer, so [tools/asm-gas-xref.sh] compares two assemblers on an input
   neither of them chose.

   The symbol is [asm_snippet] in every file: the M1 restricted linker wants one
   strong exported symbol, and a name derived from the case would make the
   readelf output differ case by case for no reason connected to encoding. *)

let symbol = "asm_snippet"

let source_of t canonical =
  String.concat ""
    (List.map (fun d -> "\t" ^ d ^ "\n") t.preamble
    @ [ "\t.globl " ^ symbol ^ "\n"; symbol ^ ":\n" ]
    @ List.map (fun line -> "\t" ^ line ^ "\n") canonical)

(* The one hex renderer. [test/dump/asm_dump.ml] and
   [test/differential/test_differential.ml] each carry a copy of this three-line
   function; a third would be the point at which they start disagreeing about
   whether the separator is a space. *)
let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))
