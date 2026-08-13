(* What every §16.3 snippet assembles to, as a reviewable artifact.

   Each block is one snippet on one target: the lowered instruction sequence,
   and the bytes it encodes to. That is the whole assertion. These baselines
   used to carry a third line comparing the result against a frozen table in
   {!Asm_oracle.Snippet_bytes}; the corpus is now the single statement of
   §16.3, the table is gone, and the diff a promotion produces is what a
   regression has to get past. So read a change to these hexdumps as a change
   to the assembler's output, and promote one only after deciding the new
   encoding is right.

   Two things beyond these baselines still test the same bytes, which is what
   makes promotion a decision rather than a rubber stamp: {!Conform} executes
   them under QEMU, where [sp_align] must return 42 only when §11's entry
   alignment really holds and [trap] must fault at its instruction, and
   [tools/asm-gas-xref.sh] assembles the same sequences with GNU as.

   {1 One block per snippet}

   Each block names the one registry entry it checks -
   [X86_64_corpus.cases.spin] under a test called "x86_64 spin" - so the
   mapping from test to subject is the line itself, not a lookup by string
   through a list that happens to hold it. The cost is 36 blocks; what it buys
   is that a changed encoding prints one snippet's diff on one target, and that
   no test can silently check something other than what it is named after. *)

open Snippet_corpus.Snippet_ast

(* The whole of what a snippet has to show: the sequence, then its bytes.
   Everything a block prints goes through here, so 36 baselines cannot drift
   into 36 shapes. *)
let check = function
  | Needs what -> Printf.printf "not built: needs %s\n" what
  | Refused msg -> Printf.printf "REFUSED by the assembler: %s\n" msg
  | Assembled { bytes; canonical } ->
      List.iter (fun line -> Printf.printf "| %s\n" line) canonical;
      print_endline (hex bytes)

(* {1 x86_64} *)

let%expect_test "x86_64 return42" =
  check X86_64_corpus.cases.return42;
  [%expect
    {|
    | subq $8, %rsp
    | leaq 16(%rsp), %rax
    | movq %rax, (%rsp)
    | movl $42, %eax
    | addq $8, %rsp
    | ret
    48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 2a 00 00 00 48 83 c4 08 c3 |}]

let%expect_test "x86_64 return41" =
  check X86_64_corpus.cases.return41;
  [%expect
    {|
    | subq $8, %rsp
    | leaq 16(%rsp), %rax
    | movq %rax, (%rsp)
    | movl $41, %eax
    | addq $8, %rsp
    | ret
    48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 29 00 00 00 48 83 c4 08 c3 |}]

let%expect_test "x86_64 trap" =
  check X86_64_corpus.cases.trap;
  [%expect {|
    | ud2
    0f 0b |}]

let%expect_test "x86_64 spin" =
  check X86_64_corpus.cases.spin;
  [%expect {|
    | jmp .
    eb fe |}]

let%expect_test "x86_64 sp_align" =
  check X86_64_corpus.cases.sp_align;
  [%expect
    {|
    | movq %rsp, %rax
    | andq $15, %rax
    | cmpq $8, %rax
    | movl $42, %eax
    | je (. + 7)
    | movl $41, %eax
    | ret
    48 89 e0 48 83 e0 0f 48 83 f8 08 b8 2a 00 00 00 74 05 b8 29 00 00 00 c3 |}]

let%expect_test "x86_64 stack_full" =
  check X86_64_corpus.cases.stack_full;
  [%expect
    {|
    | leaq -16376(%rsp), %rax
    | movb $0, (%rax)
    | movl $42, %eax
    | ret
    48 8d 84 24 08 c0 ff ff c6 00 00 b8 2a 00 00 00 c3 |}]

let%expect_test "x86_64 stack_over" =
  check X86_64_corpus.cases.stack_over;
  [%expect
    {|
    | leaq -16377(%rsp), %rax
    | movb $0, (%rax)
    | movl $42, %eax
    | ret
    48 8d 84 24 07 c0 ff ff c6 00 00 b8 2a 00 00 00 c3 |}]

let%expect_test "x86_64 sp_corrupt" =
  check X86_64_corpus.cases.sp_corrupt;
  [%expect
    {|
    | pop %rdx
    | subq $16, %rsp
    | movl $42, %eax
    | jmp *%rdx
    5a 48 83 ec 10 b8 2a 00 00 00 ff e2 |}]

let%expect_test "x86_64 callee_clobber" =
  check X86_64_corpus.cases.callee_clobber;
  [%expect
    {|
    | xorq %rbx, %rbx
    | movl $42, %eax
    | ret
    48 31 db b8 2a 00 00 00 c3 |}]

(* {1 x86_32} *)

let%expect_test "x86_32 return42" =
  check X86_32_corpus.cases.return42;
  [%expect
    {|
    | subl $12, %esp
    | leal 16(%esp), %eax
    | movl %eax, (%esp)
    | movl $42, %eax
    | addl $12, %esp
    | ret
    83 ec 0c 8d 44 24 10 89 04 24 b8 2a 00 00 00 83 c4 0c c3 |}]

let%expect_test "x86_32 return41" =
  check X86_32_corpus.cases.return41;
  [%expect
    {|
    | subl $12, %esp
    | leal 16(%esp), %eax
    | movl %eax, (%esp)
    | movl $41, %eax
    | addl $12, %esp
    | ret
    83 ec 0c 8d 44 24 10 89 04 24 b8 29 00 00 00 83 c4 0c c3 |}]

let%expect_test "x86_32 trap" =
  check X86_32_corpus.cases.trap;
  [%expect {|
    | ud2
    0f 0b |}]

let%expect_test "x86_32 spin" =
  check X86_32_corpus.cases.spin;
  [%expect {|
    | jmp .
    eb fe |}]

let%expect_test "x86_32 sp_align" =
  check X86_32_corpus.cases.sp_align;
  [%expect
    {|
    | movl %esp, %eax
    | andl $15, %eax
    | cmpl $12, %eax
    | movl $42, %eax
    | je (. + 7)
    | movl $41, %eax
    | ret
    89 e0 83 e0 0f 83 f8 0c b8 2a 00 00 00 74 05 b8 29 00 00 00 c3 |}]

let%expect_test "x86_32 stack_full" =
  check X86_32_corpus.cases.stack_full;
  [%expect
    {|
    | leal -16380(%esp), %eax
    | movb $0, (%eax)
    | movl $42, %eax
    | ret
    8d 84 24 04 c0 ff ff c6 00 00 b8 2a 00 00 00 c3 |}]

let%expect_test "x86_32 stack_over" =
  check X86_32_corpus.cases.stack_over;
  [%expect
    {|
    | leal -16381(%esp), %eax
    | movb $0, (%eax)
    | movl $42, %eax
    | ret
    8d 84 24 03 c0 ff ff c6 00 00 b8 2a 00 00 00 c3 |}]

let%expect_test "x86_32 sp_corrupt" =
  check X86_32_corpus.cases.sp_corrupt;
  [%expect
    {|
    | pop %edx
    | subl $16, %esp
    | movl $42, %eax
    | jmp *%edx
    5a 83 ec 10 b8 2a 00 00 00 ff e2 |}]

let%expect_test "x86_32 callee_clobber" =
  check X86_32_corpus.cases.callee_clobber;
  [%expect {|
    | xorl %ebx, %ebx
    | movl $42, %eax
    | ret
    31 db b8 2a 00 00 00 c3 |}]

(* {1 arm} *)

let%expect_test "arm return42" =
  check Arm_corpus.cases.return42;
  [%expect
    {|
    | mov ip, sp
    | sub sp, sp, #8
    | str ip, [sp, #0]
    | str lr, [sp, #4]
    | mov r0, #42
    | ldr lr, [sp, #4]
    | add sp, sp, #8
    | bx lr
    0d c0 a0 e1 08 d0 4d e2 00 c0 8d e5 04 e0 8d e5 2a 00 a0 e3 04 e0 9d e5 08 d0 8d e2 1e ff 2f e1 |}]

let%expect_test "arm return41" =
  check Arm_corpus.cases.return41;
  [%expect
    {|
    | mov ip, sp
    | sub sp, sp, #8
    | str ip, [sp, #0]
    | str lr, [sp, #4]
    | mov r0, #41
    | ldr lr, [sp, #4]
    | add sp, sp, #8
    | bx lr
    0d c0 a0 e1 08 d0 4d e2 00 c0 8d e5 04 e0 8d e5 29 00 a0 e3 04 e0 9d e5 08 d0 8d e2 1e ff 2f e1 |}]

let%expect_test "arm trap" =
  check Arm_corpus.cases.trap;
  [%expect {|
    | udf #0
    f0 00 f0 e7 |}]

let%expect_test "arm spin" =
  check Arm_corpus.cases.spin;
  [%expect {|
    | b .
    fe ff ff ea |}]

let%expect_test "arm sp_align" =
  check Arm_corpus.cases.sp_align;
  [%expect
    {|
    | mov r0, sp
    | and r0, r0, #7
    | cmp r0, #0
    | moveq r0, #42
    | movne r0, #41
    | bx lr
    0d 00 a0 e1 07 00 00 e2 00 00 50 e3 2a 00 a0 03 29 00 a0 13 1e ff 2f e1 |}]

let%expect_test "arm stack_full" =
  check Arm_corpus.cases.stack_full;
  [%expect
    {|
    | sub r1, sp, #16384
    | mov r0, #0
    | strb r0, [r1, #0]
    | mov r0, #42
    | bx lr
    01 19 4d e2 00 00 a0 e3 00 00 c1 e5 2a 00 a0 e3 1e ff 2f e1 |}]

let%expect_test "arm stack_over" =
  check Arm_corpus.cases.stack_over;
  [%expect
    {|
    | sub r1, sp, #16384
    | sub r1, r1, #1
    | mov r0, #0
    | strb r0, [r1, #0]
    | mov r0, #42
    | bx lr
    01 19 4d e2 01 10 41 e2 00 00 a0 e3 00 00 c1 e5 2a 00 a0 e3 1e ff 2f e1 |}]

let%expect_test "arm sp_corrupt" =
  check Arm_corpus.cases.sp_corrupt;
  [%expect
    {|
    | sub sp, sp, #16
    | mov r0, #42
    | bx lr
    10 d0 4d e2 2a 00 a0 e3 1e ff 2f e1 |}]

let%expect_test "arm callee_clobber" =
  check Arm_corpus.cases.callee_clobber;
  [%expect
    {|
    | mov r4, #0
    | mov r0, #42
    | bx lr
    00 40 a0 e3 2a 00 a0 e3 1e ff 2f e1 |}]

(* {1 aarch64} *)

let%expect_test "aarch64 return42" =
  check Aarch64_corpus.cases.return42;
  [%expect
    {|
    | mov x15, sp
    | stp x15, x30, [sp, #-16]!
    | movz w0, #42
    | ldr x30, [sp, #8]
    | add sp, sp, #16
    | ret
    ef 03 00 91 ef 7b bf a9 40 05 80 52 fe 07 40 f9 ff 43 00 91 c0 03 5f d6 |}]

let%expect_test "aarch64 return41" =
  check Aarch64_corpus.cases.return41;
  [%expect
    {|
    | mov x15, sp
    | stp x15, x30, [sp, #-16]!
    | movz w0, #41
    | ldr x30, [sp, #8]
    | add sp, sp, #16
    | ret
    ef 03 00 91 ef 7b bf a9 20 05 80 52 fe 07 40 f9 ff 43 00 91 c0 03 5f d6 |}]

let%expect_test "aarch64 trap" =
  check Aarch64_corpus.cases.trap;
  [%expect {|
    | udf #0
    00 00 00 00 |}]

let%expect_test "aarch64 spin" =
  check Aarch64_corpus.cases.spin;
  [%expect {|
    | b .
    00 00 00 14 |}]

let%expect_test "aarch64 sp_align" =
  check Aarch64_corpus.cases.sp_align;
  [%expect
    {|
    | mov x0, sp
    | and x0, x0, #15
    | cmp x0, #0
    | movz w0, #42
    | b.eq (. + 8)
    | movz w0, #41
    | ret
    e0 03 00 91 00 0c 40 92 1f 00 00 f1 40 05 80 52 40 00 00 54 20 05 80 52 c0 03 5f d6 |}]

let%expect_test "aarch64 stack_full" =
  check Aarch64_corpus.cases.stack_full;
  [%expect
    {|
    | sub x1, sp, #4, lsl #12
    | strb wzr, [x1]
    | movz w0, #42
    | ret
    e1 13 40 d1 3f 00 00 39 40 05 80 52 c0 03 5f d6 |}]

let%expect_test "aarch64 stack_over" =
  check Aarch64_corpus.cases.stack_over;
  [%expect
    {|
    | sub x1, sp, #4, lsl #12
    | sub x1, x1, #1
    | strb wzr, [x1]
    | movz w0, #42
    | ret
    e1 13 40 d1 21 04 00 d1 3f 00 00 39 40 05 80 52 c0 03 5f d6 |}]

let%expect_test "aarch64 sp_corrupt" =
  check Aarch64_corpus.cases.sp_corrupt;
  [%expect
    {|
    | sub sp, sp, #16
    | movz w0, #42
    | ret
    ff 43 00 d1 40 05 80 52 c0 03 5f d6 |}]

let%expect_test "aarch64 callee_clobber" =
  check Aarch64_corpus.cases.callee_clobber;
  [%expect
    {|
    | movz x19, #0
    | movz w0, #42
    | ret
    13 00 80 d2 40 05 80 52 c0 03 5f d6 |}]

(* {1 riscv32} *)

let%expect_test "riscv32 return42" =
  check Riscv32_corpus.cases.return42;
  [%expect {|
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv32 return41" =
  check Riscv32_corpus.cases.return41;
  [%expect {|
  | addi x10, x0, 41
  | jalr x0, 0(x1)
  13 05 90 02 67 80 00 00 |}]

let%expect_test "riscv32 trap" =
  check Riscv32_corpus.cases.trap;
  [%expect {|
  | unimp
  73 10 00 c0 |}]

let%expect_test "riscv32 spin" =
  check Riscv32_corpus.cases.spin;
  [%expect {|
  | jal x0, .
  6f 00 00 00 |}]

let%expect_test "riscv32 sp_align" =
  check Riscv32_corpus.cases.sp_align;
  [%expect
    {|
  | andi x5, x2, 15
  | addi x10, x0, 42
  | beq x5, x0, (. + 8)
  | addi x10, x0, 41
  | jalr x0, 0(x1)
  93 72 f1 00 13 05 a0 02 63 84 02 00 13 05 90 02 67 80 00 00 |}]

let%expect_test "riscv32 stack_full" =
  check Riscv32_corpus.cases.stack_full;
  [%expect
    {|
  | lui x5, 4
  | sub x5, x2, x5
  | sb x0, 0(x5)
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  b7 42 00 00 b3 02 51 40 23 80 02 00 13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv32 stack_over" =
  check Riscv32_corpus.cases.stack_over;
  [%expect
    {|
  | lui x5, 4
  | sub x5, x2, x5
  | addi x5, x5, -1
  | sb x0, 0(x5)
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  b7 42 00 00 b3 02 51 40 93 82 f2 ff 23 80 02 00 13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv32 sp_corrupt" =
  check Riscv32_corpus.cases.sp_corrupt;
  [%expect
    {|
  | addi x5, x1, 0
  | addi x2, x2, -16
  | addi x10, x0, 42
  | jalr x0, 0(x5)
  93 82 00 00 13 01 01 ff 13 05 a0 02 67 80 02 00 |}]

let%expect_test "riscv32 callee_clobber" =
  check Riscv32_corpus.cases.callee_clobber;
  [%expect
    {|
  | addi x8, x0, 0
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  13 04 00 00 13 05 a0 02 67 80 00 00 |}]

(* {1 riscv64} *)

let%expect_test "riscv64 return42" =
  check Riscv64_corpus.cases.return42;
  [%expect {|
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv64 return41" =
  check Riscv64_corpus.cases.return41;
  [%expect {|
  | addi x10, x0, 41
  | jalr x0, 0(x1)
  13 05 90 02 67 80 00 00 |}]

let%expect_test "riscv64 trap" =
  check Riscv64_corpus.cases.trap;
  [%expect {|
  | unimp
  73 10 00 c0 |}]

let%expect_test "riscv64 spin" =
  check Riscv64_corpus.cases.spin;
  [%expect {|
  | jal x0, .
  6f 00 00 00 |}]

let%expect_test "riscv64 sp_align" =
  check Riscv64_corpus.cases.sp_align;
  [%expect
    {|
  | andi x5, x2, 15
  | addi x10, x0, 42
  | beq x5, x0, (. + 8)
  | addi x10, x0, 41
  | jalr x0, 0(x1)
  93 72 f1 00 13 05 a0 02 63 84 02 00 13 05 90 02 67 80 00 00 |}]

let%expect_test "riscv64 stack_full" =
  check Riscv64_corpus.cases.stack_full;
  [%expect
    {|
  | lui x5, 4
  | sub x5, x2, x5
  | sb x0, 0(x5)
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  b7 42 00 00 b3 02 51 40 23 80 02 00 13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv64 stack_over" =
  check Riscv64_corpus.cases.stack_over;
  [%expect
    {|
  | lui x5, 4
  | sub x5, x2, x5
  | addi x5, x5, -1
  | sb x0, 0(x5)
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  b7 42 00 00 b3 02 51 40 93 82 f2 ff 23 80 02 00 13 05 a0 02 67 80 00 00 |}]

let%expect_test "riscv64 sp_corrupt" =
  check Riscv64_corpus.cases.sp_corrupt;
  [%expect
    {|
  | addi x5, x1, 0
  | addi x2, x2, -16
  | addi x10, x0, 42
  | jalr x0, 0(x5)
  93 82 00 00 13 01 01 ff 13 05 a0 02 67 80 02 00 |}]

let%expect_test "riscv64 callee_clobber" =
  check Riscv64_corpus.cases.callee_clobber;
  [%expect
    {|
  | addi x8, x0, 0
  | addi x10, x0, 42
  | jalr x0, 0(x1)
  13 04 00 00 13 05 a0 02 67 80 00 00 |}]

(* {1 The inventory}

   What is dogfooded and what is not, in one table - the one test that is over
   the whole registry rather than one entry of it, because a frontier is only
   readable as a table. The [needs] column is that frontier: it is the work M2
   has to do before that snippet can be built here, stated per profile because
   the instruction sets do not run out of forms at the same place - AArch64
   encodes [movz x19, #0] with the fixture's own form and so reaches
   [callee_clobber] today, while x86 needs [xor] for the same snippet. *)

let%expect_test "the M1 scope frontier, per snippet and profile" =
  let status = function
    | Assembled _ -> "ok"
    | Refused _ -> "REFUSED"
    | Needs what -> "needs: " ^ what
  in
  List.iter
    (fun (name, get) ->
      Printf.printf "%s\n" name;
      List.iter (fun t -> Printf.printf "  %-8s %s\n" t.target (status (get t.cases))) all)
    fields;
  let entries = List.concat_map (fun t -> named t.cases) all in
  let built =
    List.length
      (List.filter (fun (_, b) -> match b with Assembled _ -> true | _ -> false) entries)
  in
  Printf.printf "\ndogfooded %d of %d\n" built (List.length entries);
  [%expect
    {|
    return42
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    return41
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    trap
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    spin
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    sp_align
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    stack_full
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    stack_over
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    sp_corrupt
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok
    callee_clobber
      x86_32   ok
      x86_64   ok
      arm      ok
      aarch64  ok
      riscv32  ok
      riscv64  ok

    dogfooded 54 of 54 |}]

(* {1 Padding}

   [nop_bytes] is the one blob that reaches an image without passing through an
   instruction, so no snippet covers it. Pinned per length here instead.

   The x86 table is a genuine encoding table - nine hand-written multi-byte
   no-ops, one per length - and stays a literal in the target, because it *is*
   the encoding rather than a restatement of one. What it did not have was
   anything asserting the lengths come out right. *)

(* Accepted, accepted at the wrong length, and rejected all render through one
   combinator, so the three cannot drift into differently-shaped output. *)
let show_nop ~length =
  let ok ppf b =
    if String.length b = length then Fmt.string ppf (hex b)
    else Fmt.pf ppf "WRONG LENGTH %d: %s" (String.length b) (hex b)
  in
  Fmt.str "%a" (Fmt.result ~ok ~error:Fmt.(any "rejected: " ++ string))

let%expect_test "nop_bytes, every length a target accepts" =
  List.iter
    (fun t ->
      Printf.printf "%s\n" t.target;
      List.iter
        (fun length -> Printf.printf "  %2d  %s\n" length (show_nop ~length (t.nop ~length)))
        (List.init 16 (fun i -> i + 1)))
    all;
  [%expect
    {|
    x86_32
       1  90
       2  66 90
       3  8d 76 00
       4  8d 74 26 00
       5  2e 8d 74 26 00
       6  8d b6 00 00 00 00
       7  8d b4 26 00 00 00 00
       8  2e 8d b4 26 00 00 00 00
       9  2e 8d b4 26 00 00 00 00 90
      10  2e 8d b4 26 00 00 00 00 66 90
      11  2e 8d b4 26 00 00 00 00 8d 76 00
      12  2e 8d b4 26 00 00 00 00 8d 74 26 00
      13  2e 8d b4 26 00 00 00 00 2e 8d 74 26 00
      14  2e 8d b4 26 00 00 00 00 8d b6 00 00 00 00
      15  2e 8d b4 26 00 00 00 00 8d b4 26 00 00 00 00
      16  2e 8d b4 26 00 00 00 00 2e 8d b4 26 00 00 00 00
    x86_64
       1  90
       2  66 90
       3  0f 1f 00
       4  0f 1f 40 00
       5  0f 1f 44 00 00
       6  66 0f 1f 44 00 00
       7  0f 1f 80 00 00 00 00
       8  0f 1f 84 00 00 00 00 00
       9  66 0f 1f 84 00 00 00 00 00
      10  66 2e 0f 1f 84 00 00 00 00 00
      11  66 66 2e 0f 1f 84 00 00 00 00 00
      12  66 66 2e 0f 1f 84 00 00 00 00 00 90
      13  66 66 2e 0f 1f 84 00 00 00 00 00 66 90
      14  66 66 2e 0f 1f 84 00 00 00 00 00 0f 1f 00
      15  66 66 2e 0f 1f 84 00 00 00 00 00 0f 1f 40 00
      16  66 66 2e 0f 1f 84 00 00 00 00 00 0f 1f 44 00 00
    arm
       1  rejected: A32 padding must be a whole number of four-byte instructions
       2  rejected: A32 padding must be a whole number of four-byte instructions
       3  rejected: A32 padding must be a whole number of four-byte instructions
       4  00 f0 20 e3
       5  rejected: A32 padding must be a whole number of four-byte instructions
       6  rejected: A32 padding must be a whole number of four-byte instructions
       7  rejected: A32 padding must be a whole number of four-byte instructions
       8  00 f0 20 e3 00 f0 20 e3
       9  rejected: A32 padding must be a whole number of four-byte instructions
      10  rejected: A32 padding must be a whole number of four-byte instructions
      11  rejected: A32 padding must be a whole number of four-byte instructions
      12  00 f0 20 e3 00 f0 20 e3 00 f0 20 e3
      13  rejected: A32 padding must be a whole number of four-byte instructions
      14  rejected: A32 padding must be a whole number of four-byte instructions
      15  rejected: A32 padding must be a whole number of four-byte instructions
      16  00 f0 20 e3 00 f0 20 e3 00 f0 20 e3 00 f0 20 e3
    aarch64
       1  rejected: A64 padding must be a whole number of four-byte instructions
       2  rejected: A64 padding must be a whole number of four-byte instructions
       3  rejected: A64 padding must be a whole number of four-byte instructions
       4  1f 20 03 d5
       5  rejected: A64 padding must be a whole number of four-byte instructions
       6  rejected: A64 padding must be a whole number of four-byte instructions
       7  rejected: A64 padding must be a whole number of four-byte instructions
       8  1f 20 03 d5 1f 20 03 d5
       9  rejected: A64 padding must be a whole number of four-byte instructions
      10  rejected: A64 padding must be a whole number of four-byte instructions
      11  rejected: A64 padding must be a whole number of four-byte instructions
      12  1f 20 03 d5 1f 20 03 d5 1f 20 03 d5
      13  rejected: A64 padding must be a whole number of four-byte instructions
      14  rejected: A64 padding must be a whole number of four-byte instructions
      15  rejected: A64 padding must be a whole number of four-byte instructions
      16  1f 20 03 d5 1f 20 03 d5 1f 20 03 d5 1f 20 03 d5
    riscv32
       1  rejected: RISC-V padding must be a multiple of four bytes
       2  rejected: RISC-V padding must be a multiple of four bytes
       3  rejected: RISC-V padding must be a multiple of four bytes
       4  13 00 00 00
       5  rejected: RISC-V padding must be a multiple of four bytes
       6  rejected: RISC-V padding must be a multiple of four bytes
       7  rejected: RISC-V padding must be a multiple of four bytes
       8  13 00 00 00 13 00 00 00
       9  rejected: RISC-V padding must be a multiple of four bytes
      10  rejected: RISC-V padding must be a multiple of four bytes
      11  rejected: RISC-V padding must be a multiple of four bytes
      12  13 00 00 00 13 00 00 00 13 00 00 00
      13  rejected: RISC-V padding must be a multiple of four bytes
      14  rejected: RISC-V padding must be a multiple of four bytes
      15  rejected: RISC-V padding must be a multiple of four bytes
      16  13 00 00 00 13 00 00 00 13 00 00 00 13 00 00 00
    riscv64
       1  rejected: RISC-V padding must be a multiple of four bytes
       2  rejected: RISC-V padding must be a multiple of four bytes
       3  rejected: RISC-V padding must be a multiple of four bytes
       4  13 00 00 00
       5  rejected: RISC-V padding must be a multiple of four bytes
       6  rejected: RISC-V padding must be a multiple of four bytes
       7  rejected: RISC-V padding must be a multiple of four bytes
       8  13 00 00 00 13 00 00 00
       9  rejected: RISC-V padding must be a multiple of four bytes
      10  rejected: RISC-V padding must be a multiple of four bytes
      11  rejected: RISC-V padding must be a multiple of four bytes
      12  13 00 00 00 13 00 00 00 13 00 00 00
      13  rejected: RISC-V padding must be a multiple of four bytes
      14  rejected: RISC-V padding must be a multiple of four bytes
      15  rejected: RISC-V padding must be a multiple of four bytes
      16  13 00 00 00 13 00 00 00 13 00 00 00 13 00 00 00 |}]
