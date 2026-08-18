(* The reference snippets of execution ABI v1 §16.3, written as ASTs.

   §16.3 used to be stated twice: once as frozen byte strings in
   {!Asm_oracle.Snippet_bytes}, which is what {!Conform} ran under QEMU, and
   once here as ASTs that this project's own [lower_instruction] and [encode]
   turned back into those same bytes. The second statement had to reproduce the
   first exactly, and a test asserted it did.

   That two-sided arrangement was bootstrapping. It bought independence at a
   time when the assembler could not yet build six of the nine snippets: a
   table no part of the assembler could influence was the only thing that could
   say what the right answer was. M2 closed that gap - all 36 assembled and all
   36 matched - and what the duplicate table buys now is smaller than what it
   costs, because two statements of one fact are two places to edit and one
   opportunity to disagree.

   So the ASTs are the statement, and the bytes are derived from them. This
   file is the single source of §16.3: the expect baselines beside it hold the
   assembled hexdump of every snippet and are what a regression has to get
   past, and {!for_profile} hands the same bytes to the conformance suite, so
   what QEMU executes is what this assembler produced.

   What still checks the assembler, now that nothing compares it to a frozen
   table:

   - the baselines, reviewed as a diff. An encoding that changes prints its
     snippet's hexdump next to the instruction sequence that produced it.
   - the helpers, under QEMU. [sp_align] returns 42 only if the entry SP
     alignment of §11 really holds, [trap] must fault at its instruction,
     [stack_over] must touch the guard page. Those are semantic assertions
     about the bytes, and no baseline promotion can satisfy them.
   - the differential gate, which compares this assembler against GNU as on the
     fixture corpus, and [tools/asm-gas-xref.sh], which does the same for these
     snippets from the canonical text {!source_of} emits.

   {1 [Needs], and why it is still here}

   M1 froze the assembler at the 24 encoding forms the four return-42 fixtures
   emit, and §4.3 listed what was deliberately absent - PC-relative expressions
   and fixups among it. Six §16.3 snippets were outside that set, so they could
   not be assembled and were recorded as [Needs] naming the form they wanted:
   they exist to spin and to check SP alignment, which is exactly what a fixup
   is for.

   M2 supplied those forms and all six became hexdumps that have to match, so
   nothing is [Needs] today. The constructor stays because that is how the
   corpus states a scope boundary - as data checked on every run rather than as
   prose - and the next milestone will have one of its own. An entry that
   vanished instead would take its scope statement with it. It now also carries
   a second obligation: a snippet that is [Needs] or [Refused] is one the
   conformance suite cannot run, and {!for_profile} says so rather than
   handing QEMU a hole. *)

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

(* {1 The registry}

   One field per §16.3 snippet, so a corpus is a record with named members
   rather than a list a test has to search. That is what lets a test say
   [X86_64_corpus.cases.spin] and be exactly the test of x86_64's [spin]; it
   also makes a target that forgets a snippet a type error rather than a row
   that quietly goes missing from the inventory, and it is what {!for_profile}
   walks to hand {!Sb.t}'s nine fields to the conformance suite. *)

type cases = {
  return42 : built;
  return41 : built;
  trap : built;
  spin : built;
  sp_align : built;
  stack_full : built;
  stack_over : built;
  sp_corrupt : built;
  callee_clobber : built;
}

(* Field to §16.3 name, in the canonical inventory order. The one place a
   snippet's name is spelled, so the inventory, the emitted .s directories and
   the record cannot disagree about what a case is called. Accessors rather
   than a list of entries, so a caller that wants one snippet across all four
   targets - the inventory does - reaches it by the same field the per-snippet
   tests use, and not by looking a string up in a list. *)
let fields : (string * (cases -> built)) list =
  [
    ("return42", fun c -> c.return42);
    ("return41", fun c -> c.return41);
    ("trap", fun c -> c.trap);
    ("spin", fun c -> c.spin);
    ("sp_align", fun c -> c.sp_align);
    ("stack_full", fun c -> c.stack_full);
    ("stack_over", fun c -> c.stack_over);
    ("sp_corrupt", fun c -> c.sp_corrupt);
    ("callee_clobber", fun c -> c.callee_clobber);
  ]

let named c = List.map (fun (name, get) -> (name, get c)) fields

(* {1 The staged assembler}

   Over {!Target_encode.ENCODE} rather than TARGET, so the four corpora below
   share one path and none can quietly take a shortcut. Entries are built at
   the *normalized* stage and pushed through both remaining stages, so this
   exercises lowering as well as encoding - an encoder fed lowered forms
   directly would never notice that [simplify] and [lower] disagree about which
   operands a mnemonic takes.

   ENCODE is the half of a target that exists below source text, and naming it
   here rather than TARGET is what keeps the lexer, the grammar and the token
   type out of this library and out of everything that links it. A snippet is
   an instruction *value*; nothing about building one should require the
   machinery for reading one, and until that signature was split, naming any
   part of a target required all of it. *)

module Make (T : Target_encode.ENCODE) = struct
  exception Refuse of string

  (* This harness reports refusals as prose, so a target failure is rendered on
     the way in. [Make] is generic over [T] and cannot hold a [T.error], which is
     the same erasure the pipeline performs (asm/docs/errors.md §2). *)
  let message_of_error e = Foundation.Diagnostic.message (T.error_diagnostic (Err.Error.kind e))

  let evaluate kind ~place ~target =
    Result.map_error
      (fun e -> T.error_diagnostic (Err.Error.kind e))
      (T.evaluate_fixup kind ~place ~target)

  (* A snippet is laid out and bound, not merely encoded. §16.3's [spin] is
     [b .] and [sp_align] branches over three instructions, and a branch's bytes
     are not decided by [encode] - it writes a placeholder and hands back a
     fixup. Running the corpus through [plan_image] and [bind_image] is what
     turns those into bytes, and it uses the *production* patcher and the
     production relaxation fixpoint rather than a second copy that could agree
     with itself.

     The base is zero because §16.3's snippets are position-independent: every
     branch in them is internal, so the bound bytes do not depend on where they
     land, which is also why the conformance suite may map them at whatever
     address the profile asks for. *)
  let base = 0L

  let assemble (insns : T.Instruction.t list) =
    let fragments_of i =
      match T.lower_instruction T.default_state i with
      | Error d -> raise (Refuse (message_of_error d))
      | Ok lowered ->
          List.map
            (fun l ->
              let origin = Foundation.Origin.synthesized ~pass:"snippet" () in
              let frag =
                match T.encode l with
                | Error d -> raise (Refuse (message_of_error d))
                | Ok (`Fixed a) ->
                    Asm_core.Lowered_ast.Bytes
                      {
                        bytes = a.Asm_core.Lowered_ast.bytes;
                        form = Some a.Asm_core.Lowered_ast.form;
                        fixups = a.Asm_core.Lowered_ast.fixups;
                        origin;
                      }
                | Ok (`Relax alts) -> Asm_core.Lowered_ast.Relax { alts; origin }
              in
              (frag, Fmt.to_to_string T.Lowered.pp l))
            lowered
    in
    match List.concat_map fragments_of insns with
    | parts -> (
        let section =
          {
            Asm_core.Lowered_ast.sec =
              {
                Asm_core.Lowered_ast.sec_name = ".text";
                perms = Asm_core.Perms.rx;
                alignment = 4;
                kind = Asm_core.Lowered_ast.Progbits;
              };
            fragments = List.map fst parts;
          }
        in
        let m =
          {
            Asm_core.Lowered_ast.unit_name = "snippet";
            sections = [ section ];
            symbols = [];
            commons = [];
            declared_sections = [];
          }
        in
        let fail e =
          Refused
            (String.concat "; "
               (List.map (fun d -> Foundation.Diagnostic.message d) (Foundation.Diag.diagnostics e)))
        in
        match Image.plan_image ~evaluate Image.default_policy [ m ] with
        | Error ds -> fail ds
        | Ok laid_out -> (
            match Image.bind_image laid_out ~addresses:[ (".text", base) ] with
            | Error ds -> fail ds
            | Ok img -> (
                match
                  List.find_opt
                    (fun (s : Image.segment) -> String.equal s.Image.name ".text")
                    img.Image.segments
                with
                | None -> Refused "the bound image has no .text"
                | Some s -> Assembled { bytes = s.Image.bytes; canonical = List.map snd parts })))
    | exception Refuse m -> Refused m

  (* Padding is the target's answer too, and the [.align] fill is the one blob
     that reaches an image without passing through an instruction. *)
  let nop ~length =
    match T.nop_bytes ~length with Ok b -> Ok b | Error d -> Error (message_of_error d)
end

(* Registers are named by the spelling the *parser* accepts, so the corpus goes
   through the same name table as source text does - aliases included - rather
   than around it. A misspelling therefore cannot assemble to the wrong
   register; it can only fail. It fails at module initialization, before any
   test name is printed, so the message has to carry the whole diagnosis
   itself: [Option.get]'s "option is None" named neither the register nor the
   target it was looked up in. *)
let reg_exn ~target find n =
  match find n with Some r -> r | None -> Fmt.failwith "%s: unknown register %S" target n

(* {1 aarch64} *)

module Aarch64_corpus = struct
  module T = Aarch64_encode
  module A = Make (Aarch64_encode)

  let r n = reg_exn ~target:T.name T.Reg.find n
  let insn op ops : T.Instruction.t = T.Instruction.mk op ops
  let reg n = T.Operand.Reg (r n)
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)

  let mem ?(writeback = false) base offset =
    T.Operand.Mem { T.Mem.base = r base; offset = T.Disp.Const offset; writeback; pre = true }

  (* The CompCert 3.17 prologue/epilogue, which is exactly the M1 fixture. *)
  let return_n n =
    A.assemble
      T.Opcode.
        [
          insn Mov [ reg "x15"; reg "sp" ];
          insn Stp [ reg "x15"; reg "x30"; mem ~writeback:true "sp" (-16L) ];
          insn Movz [ reg "w0"; imm n ];
          insn Ldr [ reg "x30"; mem "sp" 8L ];
          insn Add [ reg "sp"; reg "sp"; imm 16 ];
          insn Ret [ reg "x30" ];
        ]

  let cases =
    {
      return42 = return_n 42;
      return41 = return_n 41;
      (* §16.3: [udf #0] is genuinely the zero word here. [brk #0] (d4200000,
         SIGTRAP) is a verified alternative; [udf] is preferred because it
         keeps all four profiles on SIGILL. *)
      trap = A.assemble T.Opcode.[ insn Udf [ imm 0 ] ];
      spin = A.assemble T.Opcode.[ insn B [ T.Operand.Sym Asm_core.Expr.Current_location ] ];
      (* A64 has no predication outside [csel], so unlike A32 this one really
         does branch: [b.eq] jumps over the five-byte-equivalent single word
         that loads 41, which is [. + 8]. *)
      sp_align =
        A.assemble
          T.Opcode.
            [
              insn Mov [ reg "x0"; reg "sp" ];
              insn And [ reg "x0"; reg "x0"; imm 15 ];
              insn Cmp [ reg "x0"; imm 0 ];
              insn Movz [ reg "w0"; imm 42 ];
              insn (Bcond T.Cond.Eq)
                [
                  T.Operand.Sym
                    Asm_core.Expr.(
                      Binary (Add, Current_location, Const (Foundation.Bigint.of_int 8)));
                ];
              insn Movz [ reg "w0"; imm 41 ];
              insn Ret [];
            ];
      (* [sub x1, sp, #4, lsl #12] carves a 16384-byte gap below sp, then
         [strb wzr, [x1]] touches its first byte. *)
      stack_full =
        A.assemble
          T.Opcode.
            [
              insn Sub
                T.Operand.[ reg "x1"; reg "sp"; imm 4; Shift { T.Shift.kind = "lsl"; amount = 12 } ];
              insn Strb [ reg "wzr"; mem "x1" 0L ];
              insn Movz [ reg "w0"; imm 42 ];
              insn Ret [];
            ];
      (* One byte past [stack_full]'s gap, the first byte the guard page misses. *)
      stack_over =
        A.assemble
          T.Opcode.
            [
              insn Sub
                T.Operand.[ reg "x1"; reg "sp"; imm 4; Shift { T.Shift.kind = "lsl"; amount = 12 } ];
              insn Sub [ reg "x1"; reg "x1"; imm 1 ];
              insn Strb [ reg "wzr"; mem "x1" 0L ];
              insn Movz [ reg "w0"; imm 42 ];
              insn Ret [];
            ];
      (* [sub sp, sp, #16] is A64's separate sub-immediate encoding. *)
      sp_corrupt =
        A.assemble
          T.Opcode.
            [ insn Sub [ reg "sp"; reg "sp"; imm 16 ]; insn Movz [ reg "w0"; imm 42 ]; insn Ret [] ];
      (* [mov x19, #0] is [movz x19, #0]: the same form as the fixture's
         [movz w0, #42] with sf set, which the codec already carries. So this
         one needs no new encoding form, only the 64-bit register. *)
      callee_clobber =
        A.assemble
          T.Opcode.
            [
              insn Movz [ reg "x19"; imm 0 ]; insn Movz [ reg "w0"; imm 42 ]; insn Ret [ reg "x30" ];
            ];
    }

  let nops = A.nop
end

(* {1 arm} *)

module Arm_corpus = struct
  module T = Arm_encode
  module A = Make (Arm_encode)

  let r n = reg_exn ~target:T.name T.Reg.find n
  let insn op ops : T.Instruction.t = T.Instruction.mk op ops
  let reg n = T.Operand.Reg (r n)
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)
  let mem base offset = T.Operand.Mem { T.Mem.base = r base; offset; writeback = false; pre = true }

  let return_n n =
    A.assemble
      T.Opcode.
        [
          insn Mov [ reg "r12"; reg "sp" ];
          insn Sub [ reg "sp"; reg "sp"; imm 8 ];
          insn Str [ reg "r12"; mem "sp" 0L ];
          insn Str [ reg "lr"; mem "sp" 4L ];
          insn Mov [ reg "r0"; imm n ];
          insn Ldr [ reg "lr"; mem "sp" 4L ];
          insn Add [ reg "sp"; reg "sp"; imm 8 ];
          insn Bx [ reg "lr" ];
        ]

  let cases =
    {
      return42 = return_n 42;
      return41 = return_n 41;
      (* §16.3: [udf #0] is e7f000f0, not the zero word. On A32 that word
         decodes as [andeq r0, r0, r0], a conditional no-op, and a snippet that
         fell through into whatever followed would report success while proving
         nothing. *)
      trap = A.assemble T.Opcode.[ insn Udf [ imm 0 ] ];
      spin = A.assemble T.Opcode.[ insn B [ T.Operand.Sym Asm_core.Expr.Current_location ] ];
      (* A32 needs no branch here: the two results are written by predicated
         moves, which is what the condition field is for and why this snippet
         is six words with no jump in it. The residue is 7 rather
         than x86's 8 or 12 because the A32 prologue pushes nothing. *)
      sp_align =
        A.assemble
          T.Opcode.
            [
              insn Mov [ reg "r0"; reg "sp" ];
              insn And [ reg "r0"; reg "r0"; imm 7 ];
              insn Cmp [ reg "r0"; imm 0 ];
              T.Instruction.mk ~cond:T.Cond.Eq Mov [ reg "r0"; imm 42 ];
              T.Instruction.mk ~cond:T.Cond.Ne Mov [ reg "r0"; imm 41 ];
              insn Bx [ reg "lr" ];
            ];
      (* [sub r1, sp, #0x4000] carves a 16384-byte gap below sp, then
         [strb r0, [r1]] (with r0 zeroed) touches its first byte. Nothing is
         pushed by the call here - the return address goes in lr - so entry SP
         is exactly the top of the stack and that byte is its lowest. *)
      stack_full =
        A.assemble
          T.Opcode.
            [
              insn Sub [ reg "r1"; reg "sp"; imm 0x4000 ];
              insn Mov [ reg "r0"; imm 0 ];
              insn Strb [ reg "r0"; mem "r1" 0L ];
              insn Mov [ reg "r0"; imm 42 ];
              insn Bx [ reg "lr" ];
            ];
      (* One byte past [stack_full]'s gap. *)
      stack_over =
        A.assemble
          T.Opcode.
            [
              insn Sub [ reg "r1"; reg "sp"; imm 0x4000 ];
              insn Sub [ reg "r1"; reg "r1"; imm 1 ];
              insn Mov [ reg "r0"; imm 0 ];
              insn Strb [ reg "r0"; mem "r1" 0L ];
              insn Mov [ reg "r0"; imm 42 ];
              insn Bx [ reg "lr" ];
            ];
      (* [sub sp, sp, #16] is the fixture's sub-immediate. *)
      sp_corrupt =
        A.assemble
          T.Opcode.
            [
              insn Sub [ reg "sp"; reg "sp"; imm 16 ];
              insn Mov [ reg "r0"; imm 42 ];
              insn Bx [ reg "lr" ];
            ];
      (* [mov r4, #0] and [mov r0, #42] are both the fixture's
         modified-immediate form, and [bx lr] is the fixture's. *)
      callee_clobber =
        A.assemble
          T.Opcode.
            [ insn Mov [ reg "r4"; imm 0 ]; insn Mov [ reg "r0"; imm 42 ]; insn Bx [ reg "lr" ] ];
    }

  let nops = A.nop
end

(* {1 RISC-V}

   The two XLEN profiles deliberately spell the same nine programs.  Their
   target modules have generative operand types, so the small constructors are
   instantiated twice while the semantic shape stays visibly identical. *)

module Riscv32_corpus = struct
  module T = Riscv32_encode
  module A = Make (T)

  let r n = reg_exn ~target:T.name T.Reg.find n
  let reg n = T.Operand.Reg (r n)
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)

  let mem base offset =
    T.Operand.Mem
      { T.Mem.offset = Asm_core.Expr.Const (Foundation.Bigint.of_int64 offset); base = r base }

  let insn op ops : T.Instruction.t = T.Instruction.mk op ops
  let here = T.Operand.Sym Asm_core.Expr.Current_location

  let ahead n =
    T.Operand.Sym Asm_core.Expr.(Binary (Add, Current_location, Const (Foundation.Bigint.of_int n)))

  let return_n n = A.assemble T.Opcode.[ insn Addi [ reg "a0"; reg "zero"; imm n ]; insn Ret [] ]

  let cases =
    let open T.Opcode in
    {
      return42 = return_n 42;
      return41 = return_n 41;
      trap = A.assemble [ insn Unimp [] ];
      spin = A.assemble [ insn J [ here ] ];
      sp_align =
        A.assemble
          [
            insn Andi [ reg "t0"; reg "sp"; imm 15 ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Beq [ reg "t0"; reg "zero"; ahead 8 ];
            insn Addi [ reg "a0"; reg "zero"; imm 41 ];
            insn Ret [];
          ];
      stack_full =
        A.assemble
          [
            insn Lui [ reg "t0"; imm 4 ];
            insn Sub [ reg "t0"; reg "sp"; reg "t0" ];
            insn Sb [ reg "zero"; mem "t0" 0L ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
      stack_over =
        A.assemble
          [
            insn Lui [ reg "t0"; imm 4 ];
            insn Sub [ reg "t0"; reg "sp"; reg "t0" ];
            insn Addi [ reg "t0"; reg "t0"; imm (-1) ];
            insn Sb [ reg "zero"; mem "t0" 0L ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
      sp_corrupt =
        A.assemble
          [
            insn Mv [ reg "t0"; reg "ra" ];
            insn Addi [ reg "sp"; reg "sp"; imm (-16) ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Jr [ reg "t0" ];
          ];
      callee_clobber =
        A.assemble
          [
            insn Addi [ reg "s0"; reg "zero"; imm 0 ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
    }

  let nops = A.nop
end

module Riscv64_corpus = struct
  module T = Riscv64_encode
  module A = Make (T)

  let r n = reg_exn ~target:T.name T.Reg.find n
  let reg n = T.Operand.Reg (r n)
  let imm n = T.Operand.Imm (Foundation.Bigint.of_int n)

  let mem base offset =
    T.Operand.Mem
      { T.Mem.offset = Asm_core.Expr.Const (Foundation.Bigint.of_int64 offset); base = r base }

  let insn op ops : T.Instruction.t = T.Instruction.mk op ops
  let here = T.Operand.Sym Asm_core.Expr.Current_location

  let ahead n =
    T.Operand.Sym Asm_core.Expr.(Binary (Add, Current_location, Const (Foundation.Bigint.of_int n)))

  let return_n n = A.assemble T.Opcode.[ insn Addi [ reg "a0"; reg "zero"; imm n ]; insn Ret [] ]

  let cases =
    let open T.Opcode in
    {
      return42 = return_n 42;
      return41 = return_n 41;
      trap = A.assemble [ insn Unimp [] ];
      spin = A.assemble [ insn J [ here ] ];
      sp_align =
        A.assemble
          [
            insn Andi [ reg "t0"; reg "sp"; imm 15 ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Beq [ reg "t0"; reg "zero"; ahead 8 ];
            insn Addi [ reg "a0"; reg "zero"; imm 41 ];
            insn Ret [];
          ];
      stack_full =
        A.assemble
          [
            insn Lui [ reg "t0"; imm 4 ];
            insn Sub [ reg "t0"; reg "sp"; reg "t0" ];
            insn Sb [ reg "zero"; mem "t0" 0L ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
      stack_over =
        A.assemble
          [
            insn Lui [ reg "t0"; imm 4 ];
            insn Sub [ reg "t0"; reg "sp"; reg "t0" ];
            insn Addi [ reg "t0"; reg "t0"; imm (-1) ];
            insn Sb [ reg "zero"; mem "t0" 0L ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
      sp_corrupt =
        A.assemble
          [
            insn Mv [ reg "t0"; reg "ra" ];
            insn Addi [ reg "sp"; reg "sp"; imm (-16) ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Jr [ reg "t0" ];
          ];
      callee_clobber =
        A.assemble
          [
            insn Addi [ reg "s0"; reg "zero"; imm 0 ];
            insn Addi [ reg "a0"; reg "zero"; imm 42 ];
            insn Ret [];
          ];
    }

  let nops = A.nop
end

(* {1 x86}

   The prologue is written once for both modes. That is possible because
   [X86_family_encode.Make] re-exports the family's constructor modules rather
   than redeclaring them, so [X86_32_encode.Instruction.t] and
   [X86_64_encode.Instruction.t] are the *same* type and one builder produces
   values both assemblers accept. §11 gives the modes different frame
   adjustments and different operand widths; that is all the parameterization
   the shape needs.

   The x86 entries below are shared for the same reason: the two modes need the
   same missing forms, and stating that twice would let the two corpora drift
   apart in which snippets they even claim to cover. *)

module X86_shared = struct
  open X86_family_encode

  let insn op width ops : Instruction.t = Instruction.mk op width ops
  let imm n = Operand.Imm (Foundation.Bigint.of_int n)

  (* [.] and [. + k], the two branch targets §16.3 needs. A snippet has no
     labels, so a self-loop and a forward jump over three instructions are both
     written against the current location - which is exactly how GAS reads
     [jmp .], and why {!Image} resolves [.] to the instruction address rather
     than to the PC the architecture measures from. *)
  let here = Operand.Sym Asm_core.Expr.Current_location

  let here_plus k =
    Operand.Sym Asm_core.Expr.(Binary (Add, Current_location, Const (Foundation.Bigint.of_int k)))

  (* [frame] is the §11 stack adjustment: 8 on x86-64 and 12 on x86-32, because
     the return address differs in width and the ABI wants the stack 16-byte
     aligned at the call. Getting it wrong is what the sp_align case exists to
     catch, so it is a parameter here rather than a constant. *)
  let return_n ~sp ~ax ~eax ~width ~frame n =
    [
      insn Opcode.Sub width [ imm frame; Operand.Reg sp ];
      insn Opcode.Lea width [ Operand.Mem (Mem.of_base ~disp:(Disp.Const 16L) sp); Operand.Reg ax ];
      insn Opcode.Mov width [ Operand.Reg ax; Operand.Mem (Mem.of_base sp) ];
      (* The one line where the two modes agree exactly: [movl $42, %eax] is
         byte-identical in 32- and 64-bit mode, which is why the fixtures use it
         to pin that fact. So it is 32-bit wide in both, and takes [%eax] rather
         than the mode's own accumulator. *)
      insn Opcode.Mov 32 [ imm n; Operand.Reg eax ];
      insn Opcode.Add width [ imm frame; Operand.Reg sp ];
      insn Opcode.Ret width [];
    ]

  let trap_insns = [ insn Opcode.Ud2 32 [] ]
  let spin_insns ~width = [ insn Opcode.Jmp width [ here ] ]

  (* §16.3's alignment probe: it returns 42 when the incoming stack pointer is
     where the ABI says it should be and 41 when it is not. [frame] is the same
     §11 return-address width [return_n] adjusts by, because that *is* the
     residue - the ABI wants sp 16-byte aligned at the call site, so on entry it
     is short by exactly the pushed return address.

     The [je] jumps over one five-byte [mov], so its target is [. + 7]: two for
     the branch itself and five for what it skips. Written against [.] rather
     than a label because a snippet has none. *)
  let sp_align_insns ~sp ~ax ~eax ~width ~frame =
    [
      insn Opcode.Mov width [ Operand.Reg sp; Operand.Reg ax ];
      insn Opcode.And width [ imm 15; Operand.Reg ax ];
      insn Opcode.Cmp width [ imm frame; Operand.Reg ax ];
      insn Opcode.Mov 32 [ imm 42; Operand.Reg eax ];
      insn (Opcode.Jcc Cc.E) width [ here_plus 7 ];
      insn Opcode.Mov 32 [ imm 41; Operand.Reg eax ];
      insn Opcode.Ret width [];
    ]

  let callee_clobber_insns ~bx ~eax ~width =
    [
      insn Opcode.Xor width [ Operand.Reg bx; Operand.Reg bx ];
      insn Opcode.Mov 32 [ imm 42; Operand.Reg eax ];
      insn Opcode.Ret width [];
    ]

  (* [entry_gap] is the same §11 return-address width as [return_n]'s [frame].
     The two [stack_*] cases probe one byte on either side of a 16384-byte gap
     below sp: [stack_full] touches its first byte, [stack_over] the byte just
     past it. *)
  let stack_full_insns ~sp ~ax ~eax ~width ~entry_gap =
    [
      insn Opcode.Lea width
        [
          Operand.Mem (Mem.of_base ~disp:(Disp.Const (Int64.neg (Int64.sub 16384L entry_gap))) sp);
          Operand.Reg ax;
        ];
      insn Opcode.Mov 8 [ imm 0; Operand.Mem (Mem.of_base ax) ];
      insn Opcode.Mov 32 [ imm 42; Operand.Reg eax ];
      insn Opcode.Ret width [];
    ]

  let stack_over_insns ~sp ~ax ~eax ~width ~entry_gap =
    [
      insn Opcode.Lea width
        [
          Operand.Mem (Mem.of_base ~disp:(Disp.Const (Int64.neg (Int64.sub 16385L entry_gap))) sp);
          Operand.Reg ax;
        ];
      insn Opcode.Mov 8 [ imm 0; Operand.Mem (Mem.of_base ax) ];
      insn Opcode.Mov 32 [ imm 42; Operand.Reg eax ];
      insn Opcode.Ret width [];
    ]

  let sp_corrupt_insns ~sp ~dx ~eax ~width =
    [
      insn Opcode.Pop width [ Operand.Reg dx ];
      insn Opcode.Sub width [ imm 16; Operand.Reg sp ];
      insn Opcode.Mov 32 [ imm 42; Operand.Reg eax ];
      insn Opcode.Jmp width [ Operand.Reg dx ];
    ]

  (* The mode supplies its nine [built]s and this fixes which field each one
     lands in, so a mode cannot file [stack_over]'s bytes under [stack_full] on
     its own. Labelled arguments rather than a list, so a mode that leaves one
     out does not compile. *)
  let cases ~return_n ~trap ~callee_clobber ~stack_full ~stack_over ~sp_corrupt ~spin ~sp_align =
    {
      return42 = return_n 42;
      return41 = return_n 41;
      trap;
      spin;
      sp_align;
      stack_full;
      stack_over;
      sp_corrupt;
      callee_clobber;
    }
end

module X86_64_corpus = struct
  module A = Make (X86_64_encode)

  let r n = reg_exn ~target:X86_64_encode.name X86_64_encode.find_reg n

  let cases =
    X86_shared.cases
      ~return_n:(fun n ->
        A.assemble
          (X86_shared.return_n ~sp:(r "rsp") ~ax:(r "rax") ~eax:(r "eax") ~width:64 ~frame:8 n))
      ~trap:(A.assemble X86_shared.trap_insns)
      ~callee_clobber:
        (A.assemble (X86_shared.callee_clobber_insns ~bx:(r "rbx") ~eax:(r "eax") ~width:64))
      ~stack_full:
        (A.assemble
           (X86_shared.stack_full_insns ~sp:(r "rsp") ~ax:(r "rax") ~eax:(r "eax") ~width:64
              ~entry_gap:8L))
      ~stack_over:
        (A.assemble
           (X86_shared.stack_over_insns ~sp:(r "rsp") ~ax:(r "rax") ~eax:(r "eax") ~width:64
              ~entry_gap:8L))
      ~sp_corrupt:
        (A.assemble
           (X86_shared.sp_corrupt_insns ~sp:(r "rsp") ~dx:(r "rdx") ~eax:(r "eax") ~width:64))
      ~spin:(A.assemble (X86_shared.spin_insns ~width:64))
      ~sp_align:
        (A.assemble
           (X86_shared.sp_align_insns ~sp:(r "rsp") ~ax:(r "rax") ~eax:(r "eax") ~width:64 ~frame:8))

  let nops = A.nop
end

module X86_32_corpus = struct
  module A = Make (X86_32_encode)

  let r n = reg_exn ~target:X86_32_encode.name X86_32_encode.find_reg n

  let cases =
    X86_shared.cases
      ~return_n:(fun n ->
        A.assemble
          (X86_shared.return_n ~sp:(r "esp") ~ax:(r "eax") ~eax:(r "eax") ~width:32 ~frame:12 n))
      ~trap:(A.assemble X86_shared.trap_insns)
      ~callee_clobber:
        (A.assemble (X86_shared.callee_clobber_insns ~bx:(r "ebx") ~eax:(r "eax") ~width:32))
      ~stack_full:
        (A.assemble
           (X86_shared.stack_full_insns ~sp:(r "esp") ~ax:(r "eax") ~eax:(r "eax") ~width:32
              ~entry_gap:4L))
      ~stack_over:
        (A.assemble
           (X86_shared.stack_over_insns ~sp:(r "esp") ~ax:(r "eax") ~eax:(r "eax") ~width:32
              ~entry_gap:4L))
      ~sp_corrupt:
        (A.assemble
           (X86_shared.sp_corrupt_insns ~sp:(r "esp") ~dx:(r "edx") ~eax:(r "eax") ~width:32))
      ~spin:(A.assemble (X86_shared.spin_insns ~width:32))
      ~sp_align:
        (A.assemble
           (X86_shared.sp_align_insns ~sp:(r "esp") ~ax:(r "eax") ~eax:(r "eax") ~width:32 ~frame:12))

  let nops = A.nop
end

(* {1 The corpus}

   In the canonical target order every generated artifact and manifest in this
   project uses (tools/target-matrix.sh). *)

type target = {
  target : string;
  cases : cases;
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
      cases = X86_32_corpus.cases;
      nop = X86_32_corpus.nops;
      preamble = [ ".text" ];
    };
    {
      target = "x86_64";
      cases = X86_64_corpus.cases;
      nop = X86_64_corpus.nops;
      preamble = [ ".text" ];
    };
    {
      target = "arm";
      cases = Arm_corpus.cases;
      nop = Arm_corpus.nops;
      preamble = [ ".syntax unified"; ".arch armv7-a"; ".fpu vfpv3-d16"; ".arm"; ".text" ];
    };
    {
      target = "aarch64";
      cases = Aarch64_corpus.cases;
      nop = Aarch64_corpus.nops;
      preamble = [ ".text" ];
    };
    {
      target = "riscv32";
      cases = Riscv32_corpus.cases;
      nop = Riscv32_corpus.nops;
      preamble = [ ".option norelax"; ".option norvc"; ".text" ];
    };
    {
      target = "riscv64";
      cases = Riscv64_corpus.cases;
      nop = Riscv64_corpus.nops;
      preamble = [ ".option norelax"; ".option norvc"; ".text" ];
    };
  ]

(* {1 The runtime view}

   What {!Conform} loads into a guest. The conformance suite wants nine byte
   strings per profile and nothing else - it knows nothing about ASTs, lowering
   or canonical text - so this is where the corpus stops being a corpus and
   becomes {!Sb.t}.

   A snippet that did not assemble is reported rather than papered over. The
   suite's obligation is to run §16.3 in full; handing it eight snippets and a
   silent hole would turn a missing encoding form into a conformance result,
   which is the one confusion this direction of dependency could introduce. *)

let bytes_of name = function
  | Assembled { bytes; _ } -> Ok bytes
  | Refused msg -> Error (name ^ ": refused by the assembler: " ^ msg)
  | Needs what -> Error (name ^ ": needs " ^ what)

let snippet_bytes (c : cases) : (Sb.t, string) result =
  let ( let* ) = Result.bind in
  let* return42 = bytes_of "return42" c.return42 in
  let* return41 = bytes_of "return41" c.return41 in
  let* trap = bytes_of "trap" c.trap in
  let* spin = bytes_of "spin" c.spin in
  let* sp_align = bytes_of "sp_align" c.sp_align in
  let* stack_full = bytes_of "stack_full" c.stack_full in
  let* stack_over = bytes_of "stack_over" c.stack_over in
  let* sp_corrupt = bytes_of "sp_corrupt" c.sp_corrupt in
  let* callee_clobber = bytes_of "callee_clobber" c.callee_clobber in
  Ok
    {
      Sb.return42;
      return41;
      trap;
      spin;
      sp_align;
      stack_full;
      stack_over;
      sp_corrupt;
      callee_clobber;
    }

(* Matched exhaustively rather than looked up in {!all} by name, so a profile
   added to {!Asm_oracle.Abi} is a compile error here - the conformance suite
   would otherwise report a new profile as having no snippets, which reads like
   a helper problem rather than a corpus that has not been extended. *)
let for_profile : Asm_oracle.Abi.profile -> (Sb.t, string) result = function
  | Asm_oracle.Abi.X86_32 -> snippet_bytes X86_32_corpus.cases
  | Asm_oracle.Abi.X86_64 -> snippet_bytes X86_64_corpus.cases
  | Asm_oracle.Abi.Arm -> snippet_bytes Arm_corpus.cases
  | Asm_oracle.Abi.Aarch64 -> snippet_bytes Aarch64_corpus.cases
  | Asm_oracle.Abi.Riscv32 -> snippet_bytes Riscv32_corpus.cases
  | Asm_oracle.Abi.Riscv64 -> snippet_bytes Riscv64_corpus.cases

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
