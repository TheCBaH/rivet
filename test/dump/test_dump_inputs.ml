(* Fixture sources, embedded as string literals: the gcc-14 [return42] and [loop] cases of
   fixtures/gcc-14 for each target.

   Embedded rather than read from disk, and that is not a convenience: a Melange
   build shares no filesystem with the native one, so a three-build driver that
   read files would be comparing three programs with three different inputs
   instead of one program three times.

   These are byte-identical copies of the committed fixtures, and
   test/differential asserts that they still are - which is what stops the copy
   from quietly becoming a second, easier input. *)

type input = {
  case : string;
  target : string;
  file : string;  (** the fixture this copies, relative to fixtures/gcc-14 *)
  text : string;
}

let return42_x86_32 =
  {|	.file	"asm_test_entry.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$42, %eax
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let return42_x86_64 =
  {|	.file	"asm_test_entry.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$42, %eax
	ret
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let return42_arm =
  {|	.arch armv7-a
	.fpu vfpv3-d16
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 1
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"asm_test_entry.c"
	.text
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #42
	bx	lr
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
|}

let return42_aarch64 =
  {|	.arch armv8-a
	.file	"asm_test_entry.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	mov	w0, 42
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let return42_riscv32 =
  {|	.file	"asm_test_entry.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB0:
	.cfi_startproc
	li	a0,42
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let return42_riscv64 =
  {|	.file	"asm_test_entry.c"
	.option nopic
	.option norelax
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB0:
	.cfi_startproc
	li	a0,42
	ret
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let loop_x86_32 =
  {|	.file	"loop.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	subl	$16, %esp
	movl	$6, 12(%esp)
	movl	12(%esp), %eax
	testl	%eax, %eax
	jle	.L4
	movl	$0, %eax
	movl	$0, %edx
	.p2align 4
.L3:
	addl	$7, %edx
	addl	$1, %eax
	movl	12(%esp), %ecx
	cmpl	%eax, %ecx
	jg	.L3
.L1:
	movl	%edx, %eax
	addl	$16, %esp
	ret
.L4:
	movl	$0, %edx
	jmp	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let loop_x86_64 =
  {|	.file	"loop.c"
	.text
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
	movl	$6, -4(%rsp)
	movl	-4(%rsp), %eax
	testl	%eax, %eax
	jle	.L4
	movl	$0, %eax
	movl	$0, %edx
	.p2align 4
.L3:
	addl	$7, %edx
	addl	$1, %eax
	movl	-4(%rsp), %ecx
	cmpl	%eax, %ecx
	jg	.L3
.L1:
	movl	%edx, %eax
	ret
.L4:
	movl	$0, %edx
	jmp	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let loop_arm =
  {|	.arch armv7-a
	.fpu vfpv3-d16
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 1
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"loop.c"
	.text
	.align	2
	.global	asm_test_entry
	.syntax unified
	.arm
	.type	asm_test_entry, %function
asm_test_entry:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	sub	sp, sp, #8
	mov	r3, #6
	str	r3, [sp, #4]
	ldr	r3, [sp, #4]
	cmp	r3, #0
	ble	.L4
	mov	r3, #0
	mov	r0, r3
.L3:
	add	r0, r0, #7
	add	r3, r3, #1
	ldr	r2, [sp, #4]
	cmp	r2, r3
	bgt	.L3
.L1:
	add	sp, sp, #8
	@ sp needed
	bx	lr
.L4:
	mov	r0, #0
	b	.L1
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",%progbits
|}

let loop_aarch64 =
  {|	.arch armv8-a
	.file	"loop.c"
	.text
	.align	2
	.global	asm_test_entry
	.type	asm_test_entry, %function
asm_test_entry:
.LFB0:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	w0, 6
	str	w0, [sp, 12]
	ldr	w0, [sp, 12]
	cmp	w0, 0
	ble	.L4
	mov	w1, 0
	mov	w0, 0
.L3:
	add	w0, w0, 7
	add	w1, w1, 1
	ldr	w2, [sp, 12]
	cmp	w2, w1
	bgt	.L3
.L1:
	add	sp, sp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	ret
.L4:
	.cfi_restore_state
	mov	w0, 0
	b	.L1
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let loop_riscv32 =
  {|	.file	"loop.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB0:
	.cfi_startproc
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	li	a5,6
	sw	a5,12(sp)
	lw	a5,12(sp)
	ble	a5,zero,.L4
	li	a5,0
	li	a0,0
.L3:
	addi	a0,a0,7
	addi	a5,a5,1
	lw	a4,12(sp)
	bgt	a4,a5,.L3
.L1:
	addi	sp,sp,16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	jr	ra
.L4:
	.cfi_restore_state
	li	a0,0
	j	.L1
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let loop_riscv64 =
  {|	.file	"loop.c"
	.option nopic
	.option norelax
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	asm_test_entry
	.type	asm_test_entry, @function
asm_test_entry:
.LFB0:
	.cfi_startproc
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	li	a5,6
	sw	a5,12(sp)
	lw	a5,12(sp)
	sext.w	a5,a5
	ble	a5,zero,.L4
	li	a4,0
	li	a0,0
.L3:
	addiw	a0,a0,7
	addiw	a4,a4,1
	lw	a5,12(sp)
	sext.w	a5,a5
	bgt	a5,a4,.L3
.L2:
	addi	sp,sp,16
	.cfi_remember_state
	.cfi_def_cfa_offset 0
	jr	ra
.L4:
	.cfi_restore_state
	li	a0,0
	j	.L2
	.cfi_endproc
.LFE0:
	.size	asm_test_entry, .-asm_test_entry
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
|}

let input case target file text =
  { case; target; file = Printf.sprintf "%s/%s/%s" case target file; text }

let inputs =
  List.concat_map
    (fun (case, file, texts) -> List.map (fun (target, text) -> input case target file text) texts)
    [
      ( "return42",
        "asm_test_entry.s",
        [
          ("x86_32", return42_x86_32);
          ("x86_64", return42_x86_64);
          ("arm", return42_arm);
          ("aarch64", return42_aarch64);
          ("riscv32", return42_riscv32);
          ("riscv64", return42_riscv64);
        ] );
      ( "loop",
        "loop.s",
        [
          ("x86_32", loop_x86_32);
          ("x86_64", loop_x86_64);
          ("arm", loop_arm);
          ("aarch64", loop_aarch64);
          ("riscv32", loop_riscv32);
          ("riscv64", loop_riscv64);
        ] );
    ]

(* The [return42] text of each target, by target name. *)
let x86_32 = return42_x86_32
let x86_64 = return42_x86_64
let arm = return42_arm
let aarch64 = return42_aarch64
let riscv32 = return42_riscv32
let riscv64 = return42_riscv64

let all =
  List.filter_map (fun i -> if i.case = "return42" then Some (i.target, i.text) else None) inputs
