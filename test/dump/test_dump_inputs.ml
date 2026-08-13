(* The six fixture sources, embedded as string literals.

   Embedded rather than read from disk, and that is not a convenience: a Melange
   build shares no filesystem with the native one, so a three-build driver that
   read files would be comparing three programs with three different inputs
   instead of one program three times.

   These are byte-identical copies of the committed fixtures, and
   test/differential asserts that they still are - which is what stops the copy
   from quietly becoming a second, easier input. *)

let x86_32 =
  "\t.text\n\
   \t.align\t16\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tsubl\t$12, %esp\n\
   \t.cfi_adjust_cfa_offset\t12\n\
   \tleal\t16(%esp), %eax\n\
   \tmovl\t%eax, 0(%esp)\n\
   \tmovl\t$42, %eax\n\
   \taddl\t$12, %esp\n\
   \tret\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, @function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let x86_64 =
  "\t.text\n\
   \t.align\t16\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tsubq\t$8, %rsp\n\
   \t.cfi_adjust_cfa_offset\t8\n\
   \tleaq\t16(%rsp), %rax\n\
   \tmovq\t%rax, 0(%rsp)\n\
   \tmovl\t$42, %eax\n\
   \taddq\t$8, %rsp\n\
   \tret\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, @function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let arm =
  "\t.syntax\tunified\n\
   \t.arch\tarmv7-a\n\
   \t.fpu\tvfpv3-d16\n\
   \t.eabi_attribute Tag_ABI_VFP_args, 1\n\
   \t.arm\n\
   \t.text\n\
   \t.balign 4\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tmov\tr12, sp\n\
   \tsub\tsp, sp, #8\n\
   \t.cfi_adjust_cfa_offset\t8\n\
   \tstr\tr12, [sp, #0]\n\
   \tstr\tlr, [sp, #4]\n\
   \t.cfi_rel_offset\tlr, 4\n\
   \tmov\tr0, #42\n\
   \tldr\tlr, [sp, #4]\n\
   \tadd\tsp, sp, #8\n\
   \tbx\tlr\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, %function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let aarch64 =
  "\t.text\n\
   \t.balign 4\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tmov\tx15, sp\n\
   \tstp\tx15, x30, [sp, #-16]!\n\
   \t.cfi_adjust_cfa_offset\t16\n\
   \t.cfi_rel_offset\tx30, 8\n\
   \tmovz\tw0, #42, lsl #0\n\
   \tldr\tx30, [sp, #8]\n\
   \tadd\tsp, sp, #16\n\
   \tret\tx30\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, @function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let riscv32 =
  "\t.option pic\n\
   \t.text\n\
   \t.balign 2\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tmv\tx30, x2\n\
   \taddi\tx2, x2, -16\n\
   \t.cfi_adjust_cfa_offset\t16\n\
   \tsw\tx30, 0(x2)\n\
   \tsw\tx1, 4(x2)\n\
   \t.cfi_rel_offset\tx1, 4\n\
   \taddi\tx10, x0, 42\n\
   \tlw\tx1, 4(x2)\n\
   \taddi\tx2, x2, 16\n\
   \tjr\tx1\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, @function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let riscv64 =
  "\t.option pic\n\
   \t.text\n\
   \t.balign 2\n\
   \t.globl asm_test_entry\n\
   asm_test_entry:\n\
   \t.cfi_startproc\n\
   \tmv\tx30, x2\n\
   \taddi\tx2, x2, -16\n\
   \t.cfi_adjust_cfa_offset\t16\n\
   \tsd\tx30, 0(x2)\n\
   \tsd\tx1, 8(x2)\n\
   \t.cfi_rel_offset\tx1, 8\n\
   \taddiw\tx10, x0, 42\n\
   \tld\tx1, 8(x2)\n\
   \taddi\tx2, x2, 16\n\
   \tjr\tx1\n\
   \t.cfi_endproc\n\
   \t.type\tasm_test_entry, @function\n\
   \t.size\tasm_test_entry, . - asm_test_entry\n\n\
   \t.section .note.GNU-stack,\"\",%progbits\n"

let all =
  [
    ("x86_32", x86_32);
    ("x86_64", x86_64);
    ("arm", arm);
    ("aarch64", aarch64);
    ("riscv32", riscv32);
    ("riscv64", riscv64);
  ]
