# GCC -S snapshot of asm/helpers/riscv.c (ABI_VERSION=2), riscv32.
#
# NOT a build input: tools/asm-helpers.sh's own riscv32 leg (build_riscv)
# compiles riscv.c directly with -c, never through this file. This snapshot
# exists only so asm/tools' gas-xref frontier corpus - which reads
# asm/helpers/<target>.s unconditionally, the same way it already does for
# the four legacy profiles' own hand-written helper sources - has real,
# substantial RISC-V assembly to compare this project's own parser/lowering
# against (M5, asm/docs/corpus.md).
#
# Regenerate only if riscv.c itself changes, with:
#
#   riscv32-linux-gnu-gcc -march=rv32imafd -mabi=ilp32d -mno-relax \
#     -O2 -ffreestanding -fno-builtin -fno-stack-protector -fno-pic \
#     -fomit-frame-pointer -nostdlib -S -o asm/helpers/riscv32.s \
#     asm/helpers/riscv.c
#
# An incidental gcc-version drift would regenerate byte-identical output only
# if the toolchain matches; this file's own bytes are what a gas-xref regen
# hashes, not riscv.c's.
	.file	"riscv.c"
	.option nopic
	.option norelax
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
#APP
	.text
.align 2
.globl riscv_run_guest
.type riscv_run_guest,@function
riscv_run_guest:
  la t0,guest_out
  sw a3,0(t0)
  la t0,saved_helper_sp
  sw sp,0(t0)
  la t0,saved_ra
  sw ra,0(t0)
  la t0,saved_s
  sw s0,0(t0)
  sw s1,4(t0)
  sw s2,8(t0)
  sw s3,12(t0)
  sw s4,16(t0)
  sw s5,20(t0)
  sw s6,24(t0)
  sw s7,28(t0)
  sw s8,32(t0)
  sw s9,36(t0)
  sw s10,40(t0)
  sw s11,44(t0)
  la t0,guest_sp_expected
  sw a1,0(t0)
  li s0,0x181
  li s1,0x192
  li s2,0x1a3
  li s3,0x1b4
  li s4,0x1c5
  li s5,0x1d6
  li s6,0x1e7
  li s7,0x1f8
  li s8,0x209
  li s9,0x21a
  li s10,0x22b
  li s11,0x23c
  mv t3,a0
  mv sp,a1
  li t0,1
  sh t0,10(a2)
  fence rw,w
  jalr ra,t3,0
  mv t3,a0
  mv t4,sp
  la t0,control_alias
  lw t1,0(t0)
  li t0,3
  sw t0,220(t1)
  fence rw,w
  li t5,0
  li t0,0x181
  bne s0,t0,1f
  li t0,0x192
  bne s1,t0,1f
  li t0,0x1a3
  bne s2,t0,1f
  li t0,0x1b4
  bne s3,t0,1f
  li t0,0x1c5
  bne s4,t0,1f
  li t0,0x1d6
  bne s5,t0,1f
  li t0,0x1e7
  bne s6,t0,1f
  li t0,0x1f8
  bne s7,t0,1f
  li t0,0x209
  bne s8,t0,1f
  li t0,0x21a
  bne s9,t0,1f
  li t0,0x22b
  bne s10,t0,1f
  li t0,0x23c
  beq s11,t0,2f
1: li t5,1
2: la t0,saved_helper_sp
  lw sp,0(t0)
  la t0,saved_s
  lw s0,0(t0)
  lw s1,4(t0)
  lw s2,8(t0)
  lw s3,12(t0)
  lw s4,16(t0)
  lw s5,20(t0)
  lw s6,24(t0)
  lw s7,28(t0)
  lw s8,32(t0)
  lw s9,36(t0)
  lw s10,40(t0)
  lw s11,44(t0)
  la t0,saved_ra
  lw ra,0(t0)
  la t0,guest_out
  lw t0,0(t0)
  sw t3,0(t0)
  sw t4,4(t0)
  sw t5,8(t0)
  ret
.size riscv_run_guest,.-riscv_run_guest

#NO_APP
	.align	2
	.type	rd64, @function
rd64:
.LFB2:
	.cfi_startproc
	lbu	a2,1(a0)
	lbu	a3,5(a0)
	lbu	t1,0(a0)
	lbu	a4,2(a0)
	lbu	a7,4(a0)
	lbu	a5,6(a0)
	lbu	a1,7(a0)
	lbu	a6,3(a0)
	slli	a2,a2,8
	slli	a3,a3,8
	or	a2,a2,t1
	or	a3,a3,a7
	slli	a4,a4,16
	slli	a5,a5,16
	or	a4,a4,a2
	slli	a0,a6,24
	or	a5,a5,a3
	slli	a1,a1,24
	or	a0,a0,a4
	or	a1,a1,a5
	ret
	.cfi_endproc
.LFE2:
	.size	rd64, .-rd64
	.align	2
	.type	exit_class, @function
exit_class:
.LFB12:
	.cfi_startproc
	li	a1,0
	li	a2,0
	li	a3,0
	li	a4,0
	li	a5,0
	li	a7,94
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
.L4:
#APP
# 166 "asm/helpers/riscv.c" 1
	ebreak
# 0 "" 2
# 166 "asm/helpers/riscv.c" 1
	ebreak
# 0 "" 2
#NO_APP
	j	.L4
	.cfi_endproc
.LFE12:
	.size	exit_class, .-exit_class
	.align	2
	.type	fail_pre, @function
fail_pre:
.LFB14:
	.cfi_startproc
	lui	a5,%hi(control_alias)
	lw	a5,%lo(control_alias)(a5)
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	sw	ra,12(sp)
	.cfi_offset 1, -4
	li	a4,1
	sb	a1,218(a5)
	sb	zero,219(a5)
	sb	zero,221(a5)
	sb	zero,222(a5)
	sb	zero,223(a5)
	sb	a4,220(a5)
#APP
# 173 "asm/helpers/riscv.c" 1
	fence rw,w
# 0 "" 2
#NO_APP
	sb	a0,216(a5)
	sb	zero,217(a5)
	call	exit_class
	.cfi_endproc
.LFE14:
	.size	fail_pre, .-fail_pre
	.section	.rodata.str1.4,"aMS",@progbits,1
	.align	2
.LC0:
	.string	"pgsz-bad"
	.align	2
.LC1:
	.string	"pgsz-abs"
	.text
	.align	2
	.type	helper_main, @function
helper_main:
.LFB19:
	.cfi_startproc
	lw	a3,0(a0)
	addi	sp,sp,-752
	.cfi_def_cfa_offset 752
	sw	ra,748(sp)
	sw	s0,744(sp)
	li	a5,2
	.cfi_offset 1, -4
	.cfi_offset 8, -8
	bleu	a3,a5,.L8
	slli	a5,a3,2
	addi	a5,a5,8
	add	a5,a0,a5
	lw	a4,0(a5)
	mv	t1,a0
	beq	a4,zero,.L10
.L9:
	lw	a4,4(a5)
	addi	a5,a5,4
	bne	a4,zero,.L9
.L10:
	lw	a6,4(a5)
	li	t3,0
	addi	a5,a5,4
	beq	a6,zero,.L11
	li	a2,0
	li	a4,6
.L13:
	bne	a6,a4,.L12
	lw	a2,4(a5)
	li	t3,1
.L12:
	lw	a6,8(a5)
	addi	a5,a5,8
	bne	a6,zero,.L13
	mv	a6,a2
.L11:
	li	a5,3
	beq	a3,a5,.L14
	lw	a2,16(t1)
	lui	a5,%hi(.LC0)
	addi	a5,a5,%lo(.LC0)
	sub	a1,a2,a5
	lui	a0,%hi(.LC0+8)
.L16:
	add	a4,a5,a1
	lbu	a4,0(a4)
	lbu	a3,0(a5)
	addi	a5,a5,1
	bne	a3,a4,.L15
	addi	a4,a0,%lo(.LC0+8)
	bne	a5,a4,.L16
	li	a6,8192
.L15:
	lui	a5,%hi(.LC1)
	addi	a5,a5,%lo(.LC1)
	sub	a2,a2,a5
	lui	a1,%hi(.LC1+8)
.L18:
	add	a4,a5,a2
	lbu	a4,0(a4)
	lbu	a3,0(a5)
	addi	a5,a5,1
	bne	a3,a4,.L14
	addi	a4,a1,%lo(.LC1+8)
	bne	a5,a4,.L18
	li	t3,0
.L14:
	lw	a1,12(t1)
	li	a0,-100
	li	a2,194
	li	a3,384
	li	a4,0
	li	a5,0
	li	a7,56
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	t4,-4096
	mv	s0,a0
	bgtu	a0,t4,.L8
	li	a1,4096
	li	a2,0
	li	a3,0
	li	a7,46
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t4,.L8
	li	a3,1048576
	sw	s1,740(sp)
	addi	a3,a3,34
	li	a0,805306368
	li	a1,1048576
	li	a4,-1
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	a4,805306368
	.cfi_offset 9, -12
	beq	a0,a4,.L19
.L20:
	li	a0,66
	sw	s2,736(sp)
	sw	s3,732(sp)
	sw	s4,728(sp)
	sw	s5,724(sp)
	sw	s6,720(sp)
	sw	s7,716(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_offset 18, -16
	.cfi_offset 19, -20
	.cfi_offset 20, -24
	.cfi_offset 21, -28
	.cfi_offset 22, -32
	.cfi_offset 23, -36
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	exit_class
.L8:
	.cfi_restore 9
	.cfi_restore 18
	.cfi_restore 19
	.cfi_restore 20
	.cfi_restore 21
	.cfi_restore 22
	.cfi_restore 23
	.cfi_restore 24
	.cfi_restore 25
	.cfi_restore 26
	.cfi_restore 27
	li	a0,65
	sw	s1,740(sp)
	sw	s2,736(sp)
	sw	s3,732(sp)
	sw	s4,728(sp)
	sw	s5,724(sp)
	sw	s6,720(sp)
	sw	s7,716(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_offset 9, -12
	.cfi_offset 18, -16
	.cfi_offset 19, -20
	.cfi_offset 20, -24
	.cfi_offset 21, -28
	.cfi_offset 22, -32
	.cfi_offset 23, -36
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	exit_class
.L19:
	.cfi_restore 18
	.cfi_restore 19
	.cfi_restore 20
	.cfi_restore 21
	.cfi_restore 22
	.cfi_restore 23
	.cfi_restore 24
	.cfi_restore 25
	.cfi_restore 26
	.cfi_restore 27
	li	a0,0
	li	a1,4096
	li	a2,3
	li	a3,1
	mv	a4,s0
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	mv	s1,a0
	bgtu	a0,t4,.L20
	sw	s6,720(sp)
	lui	a2,%hi(rmagic.2)
	.cfi_offset 22, -32
	lui	s6,%hi(control_alias)
	sw	s2,736(sp)
	sw	a0,%lo(control_alias)(s6)
	addi	a2,a2,%lo(rmagic.2)
	li	a5,0
	li	a1,8
	.cfi_offset 18, -16
.L21:
	add	a4,a2,a5
	lbu	a3,0(a4)
	add	a4,s1,a5
	addi	a5,a5,1
	sb	a3,0(a4)
	bne	a5,a1,.L21
	li	s2,2
	sb	zero,9(s1)
	sb	s2,8(s1)
	sw	s3,732(sp)
	sw	s4,728(sp)
	.cfi_offset 19, -20
	.cfi_offset 20, -24
	beq	t3,zero,.L254
	sw	s7,716(sp)
	li	a5,4096
	.cfi_offset 23, -36
	beq	a6,a5,.L23
	li	a1,1
	li	a0,69
	sw	s5,724(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_offset 21, -28
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	fail_pre
.L23:
	.cfi_restore 21
	.cfi_restore 24
	.cfi_restore 25
	.cfi_restore 26
	.cfi_restore 27
	lw	a1,8(t1)
	li	a0,-100
	li	a2,0
	li	a3,0
	li	a4,0
	li	a5,0
	li	a7,56
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	s4,-4096
	mv	s3,a0
	bgtu	a0,s4,.L255
	lui	a1,%hi(empty_path.1)
	mv	a2,a6
	addi	a1,a1,%lo(empty_path.1)
	li	a3,512
	addi	a4,sp,48
	li	a7,291
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,s4,.L256
	addi	a0,sp,88
	call	rd64
	sw	a1,8(sp)
	sw	s5,724(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	mv	s7,a0
	.cfi_offset 21, -28
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	beq	a1,zero,.L257
.L166:
	li	a1,2
	li	a0,64
	call	fail_pre
.L254:
	.cfi_restore 21
	.cfi_restore 23
	.cfi_restore 24
	.cfi_restore 25
	.cfi_restore 26
	.cfi_restore 27
	mv	a1,s2
	li	a0,69
	sw	s5,724(sp)
	sw	s7,716(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_offset 21, -28
	.cfi_offset 23, -36
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	fail_pre
.L255:
	.cfi_restore 21
	.cfi_restore 24
	.cfi_restore 25
	.cfi_restore 26
	.cfi_restore 27
	li	a1,3
	li	a0,65
	sw	s5,724(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_remember_state
	.cfi_offset 21, -28
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	fail_pre
.L256:
	.cfi_restore_state
	li	a1,4
	li	a0,65
	sw	s5,724(sp)
	sw	s8,712(sp)
	sw	s9,708(sp)
	sw	s10,704(sp)
	sw	s11,700(sp)
	.cfi_offset 21, -28
	.cfi_offset 24, -40
	.cfi_offset 25, -44
	.cfi_offset 26, -48
	.cfi_offset 27, -52
	call	fail_pre
.L257:
	li	a5,119
	bleu	a0,a5,.L258
	li	a5,1048576
	bgtu	a0,a5,.L166
	mv	a3,s2
	mv	a4,s3
	li	a0,0
	mv	a1,s7
	li	a2,1
	li	a5,0
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	mv	s2,a0
	bgtu	a0,s4,.L259
	lui	a2,%hi(mmagic.0)
	addi	a2,a2,%lo(mmagic.0)
	li	a5,0
	li	a1,8
.L30:
	add	a3,s2,a5
	add	a4,a2,a5
	lbu	a3,0(a3)
	lbu	a4,0(a4)
	bne	a3,a4,.L31
	addi	a5,a5,1
	bne	a5,a1,.L30
	lbu	s8,9(s2)
	lbu	a3,8(s2)
	li	a4,2
	slli	s8,s8,8
	or	s8,s8,a3
	beq	s8,a4,.L247
	li	a1,4
	li	a0,64
	call	fail_pre
.L258:
	li	a1,1
	li	a0,64
	call	fail_pre
.L259:
	li	a1,5
	li	a0,65
	call	fail_pre
.L31:
	li	a1,3
.L251:
	li	a0,64
	call	fail_pre
.L247:
	lbu	a4,11(s2)
	lbu	a3,10(s2)
	li	a1,5
	slli	a4,a4,8
	or	a4,a4,a3
	bne	a4,a1,.L251
	lbu	a2,13(s2)
	lbu	a1,12(s2)
	lbu	a3,14(s2)
	lbu	a4,15(s2)
	slli	a2,a2,8
	or	a2,a2,a1
	slli	a3,a3,16
	or	a3,a3,a2
	slli	a4,a4,24
	or	a4,a4,a3
	li	a1,6
	bne	a4,s7,.L251
	lbu	s4,21(s2)
	lbu	a4,20(s2)
	li	a1,7
	slli	s4,s4,8
	or	s4,s4,a4
	addi	a4,s4,-1
	bgtu	a4,a1,.L251
	lbu	a4,23(s2)
	lbu	a2,22(s2)
	li	a3,1
	slli	a4,a4,8
	or	a4,a4,a2
	beq	a4,a3,.L37
	mv	a1,a5
	li	a0,64
	call	fail_pre
.L37:
	lbu	a3,41(s2)
	lbu	a2,40(s2)
	lbu	a4,42(s2)
	lbu	a5,43(s2)
	slli	a3,a3,8
	or	a3,a3,a2
	slli	a4,a4,16
	or	a4,a4,a3
	slli	a5,a5,24
	or	a5,a5,a4
	li	a4,256
	beq	a5,a4,.L38
	li	a1,9
	li	a0,64
	call	fail_pre
.L38:
	lbu	a3,49(s2)
	lbu	a2,48(s2)
	lbu	a4,50(s2)
	lbu	a5,51(s2)
	slli	a3,a3,8
	or	a3,a3,a2
	slli	a4,a4,16
	or	a4,a4,a3
	slli	a5,a5,24
	or	a5,a5,a4
	li	a4,61440
	addi	a5,a5,-100
	addi	a4,a4,-1540
	bgtu	a5,a4,.L260
	lbu	a4,53(s2)
	lbu	a3,52(s2)
	lbu	a5,54(s2)
	lbu	s11,55(s2)
	slli	a4,a4,8
	or	a4,a4,a3
	slli	a5,a5,16
	or	a5,a5,a4
	slli	s11,s11,24
	or	s11,s11,a5
	bne	s11,zero,.L261
	addi	a5,s2,64
	addi	s9,s2,120
	li	a3,0
.L41:
	lbu	a4,0(a5)
	addi	a5,a5,1
	or	a3,a3,a4
	bne	a5,s9,.L41
	bne	a3,zero,.L262
	li	s3,40
	mul	s3,s4,s3
	li	a2,0
	addi	s3,s3,120
	bgtu	s3,s7,.L263
	addi	s5,sp,304
	mv	s10,s5
	li	t3,0
.L49:
	mv	a0,s9
	sw	a2,28(sp)
	sw	a3,24(sp)
	sw	t3,20(sp)
	call	rd64
	mv	a4,a0
	sw	a4,0(s10)
	addi	a0,s9,8
	sw	a1,4(s10)
	call	rd64
	mv	a7,a0
	sw	a7,8(s10)
	addi	a0,s9,16
	sw	a1,12(s10)
	sw	a7,12(sp)
	sw	a1,16(sp)
	call	rd64
	mv	a4,a0
	sw	a4,16(s10)
	addi	a0,s9,24
	sw	a1,20(s10)
	call	rd64
	lbu	a5,33(s9)
	lbu	t5,32(s9)
	lw	a7,12(sp)
	slli	a5,a5,8
	lbu	t4,34(s9)
	mv	a4,a0
	or	a0,a5,t5
	lbu	a5,35(s9)
	add	t5,a7,a4
	lw	t1,16(sp)
	slli	t4,t4,16
	or	t4,t4,a0
	slli	a0,a5,24
	sltu	a5,t5,a7
	lbu	a7,36(s9)
	add	t1,t1,a1
	or	a0,a0,t4
	sw	a1,28(s10)
	sw	a4,24(s10)
	add	a5,a5,t1
	sw	a0,32(s10)
	sb	a7,36(s10)
	bne	a5,zero,.L45
	bgtu	t5,s7,.L45
	bne	a1,zero,.L45
	lw	t3,20(sp)
	lw	a3,24(sp)
	lw	a2,28(sp)
	bgtu	a4,t5,.L45
	addi	t3,t3,1
	addi	s9,s9,40
	addi	s10,s10,48
	bgtu	s4,t3,.L49
	mv	a5,s5
	li	a0,0
.L56:
	lw	a4,24(a5)
	lw	a6,28(a5)
	andi	a1,a4,7
	bne	a1,zero,.L264
	or	a1,a4,a6
	beq	a1,zero,.L52
	beq	a2,a6,.L265
.L52:
	lw	a4,8(a5)
	lw	a6,12(a5)
	snez	a1,a1
	or	a4,a4,a6
	snez	a4,a4
	bne	a1,a4,.L266
	addi	a0,a0,1
	addi	a5,a5,48
	bgtu	s4,a0,.L56
	mv	a1,s5
	li	a6,0
	j	.L65
.L57:
	addi	a1,a1,48
	bleu	s4,a6,.L267
.L65:
	lw	a5,8(a1)
	lw	a4,12(a1)
	addi	a6,a6,1
	or	a0,a5,a4
	beq	a0,zero,.L57
	lw	t3,24(a1)
	lw	a7,28(a1)
	mv	a0,a1
	add	t1,a5,t3
	sltu	a5,t1,a5
	add	a4,a4,a7
	add	a4,a5,a4
	mv	t4,a6
.L59:
	bgeu	t4,s4,.L57
	lw	a5,56(a0)
	lw	t2,60(a0)
	or	t5,a5,t2
	beq	t5,zero,.L60
	lw	t6,72(a0)
	lw	t5,76(a0)
	add	t0,a5,t6
	sltu	a5,t0,a5
	add	t2,t2,t5
	add	a5,a5,t2
	bgtu	a5,a7,.L171
	bne	a5,a7,.L60
	bleu	t0,t3,.L60
.L171:
	bgtu	a4,t5,.L172
	beq	a4,t5,.L268
.L60:
	addi	t4,t4,1
	addi	a0,a0,48
	j	.L59
.L260:
	li	a1,10
	li	a0,64
	call	fail_pre
.L261:
	li	a1,11
	li	a0,64
	call	fail_pre
.L264:
	li	a1,15
	li	a0,64
	call	fail_pre
.L266:
	li	a1,17
	li	a0,64
	call	fail_pre
.L267:
	mv	a4,s5
	li	a7,0
	j	.L73
.L66:
	addi	a7,a7,1
	addi	a4,a4,48
	bleu	s4,a7,.L269
.L73:
	lw	a1,8(a4)
	lw	a0,12(a4)
	or	a5,a1,a0
	beq	a5,zero,.L66
	lw	t1,24(a4)
	addi	a5,s3,7
	sltu	a6,a5,s3
	andi	a5,a5,-8
	add	a2,a6,a2
	bne	t1,a5,.L173
	lw	a6,28(a4)
	bne	a6,a2,.L173
	add	s3,s2,s3
	add	t1,s2,a5
	li	a6,0
.L70:
	beq	s3,t1,.L270
	lbu	t3,0(s3)
	addi	s3,s3,1
	or	a6,a6,t3
	j	.L70
.L269:
	addi	a5,s3,7
	sltu	a4,a5,s3
	andi	a5,a5,-8
	add	a4,a4,a2
	bne	s7,a5,.L174
	lw	a5,8(sp)
	beq	a5,a4,.L240
.L174:
	li	a1,21
	li	a0,64
	call	fail_pre
.L240:
	add	s3,s2,s3
	add	s7,s2,s7
.L76:
	beq	s3,s7,.L271
	lbu	a5,0(s3)
	addi	s3,s3,1
	or	a3,a3,a5
	j	.L76
.L268:
	bleu	t1,t6,.L60
.L172:
	li	a1,18
	li	a0,64
	call	fail_pre
.L271:
	bne	a3,zero,.L78
	mv	a5,s5
	li	a4,0
	li	a2,262144
	li	a3,1048576
.L83:
	lw	a1,12(a5)
	bne	a1,zero,.L175
	lw	a1,8(a5)
	bgtu	a1,a2,.L175
	lw	a1,20(a5)
	bne	a1,zero,.L176
	lw	a1,16(a5)
	bgtu	a1,a3,.L176
	addi	a4,a4,1
	addi	a5,a5,48
	bgtu	s4,a4,.L83
	li	t1,4096
	addi	t1,t1,-1
	mv	a5,s5
	li	t4,7
	li	t3,6
.L103:
	lw	a3,8(a5)
	lw	a4,16(a5)
	lw	a1,12(a5)
	lw	a2,20(a5)
	or	a0,a3,a4
	or	a6,a1,a2
	or	a0,a0,a6
	beq	a0,zero,.L272
	lw	s3,32(a5)
	beq	s3,zero,.L85
	addi	a0,s3,-1
	and	s3,s3,a0
	bne	s3,zero,.L85
	lw	a6,0(a5)
	lw	a7,4(a5)
	and	a0,a0,a6
	bne	a0,zero,.L273
	lbu	a0,36(a5)
	bgtu	a0,t4,.L274
	andi	a0,a0,6
	beq	a0,t3,.L275
	add	t5,a3,a6
	add	a1,a1,a7
	sltu	a3,t5,a3
	add	a0,a4,t5
	add	a3,a3,a1
	add	a1,a2,a3
	sltu	a4,a0,a4
	add	a2,a0,t1
	add	a4,a4,a1
	sltu	a1,a2,a0
	add	a1,a1,a4
	bgtu	a4,a1,.L91
	bne	a4,a1,.L177
	bgtu	a0,a2,.L91
.L177:
	li	t6,-4096
	and	a2,a2,t6
	bgtu	a7,a3,.L91
	beq	a7,a3,.L276
.L96:
	bgtu	a3,a4,.L91
	beq	a3,a4,.L277
.L98:
	li	a4,-4096
	and	a4,a6,a4
	beq	a7,zero,.L278
.L178:
	bne	a1,zero,.L99
	li	a3,806354944
	bgtu	a2,a3,.L99
	sw	a4,40(a5)
	sw	a2,44(a5)
	addi	s11,s11,1
	addi	a5,a5,48
	bgtu	s4,s11,.L103
	addi	a0,s2,32
	call	rd64
	lbu	a3,45(s2)
	lbu	a6,44(s2)
	lbu	a4,46(s2)
	mv	s9,a0
	li	a2,-4096
	lbu	a0,47(s2)
	and	a2,s9,a2
	slli	a3,a3,8
	li	a5,4096
	or	a3,a3,a6
	slli	a4,a4,16
	add	a5,a2,a5
	or	a4,a4,a3
	slli	a0,a0,24
	sltu	a3,a5,a2
	or	s7,a0,a4
	add	a3,a3,a1
	li	a4,805568512
	mv	s10,a1
	add	a4,s7,a4
	bgtu	a1,a3,.L160
	bne	a1,a3,.L104
	bgtu	a2,a5,.L160
.L104:
	mv	a3,s5
	li	a1,0
	li	a7,805568512
	li	t1,805564416
.L114:
	lw	a6,40(a3)
	lw	a0,44(a3)
	bgeu	a6,a5,.L106
	bgtu	a0,a2,.L107
.L106:
	bgeu	a6,a4,.L109
	bgtu	a0,a7,.L110
.L109:
	bgeu	a6,a7,.L112
	bgtu	a0,t1,.L113
.L112:
	addi	a1,a1,1
	addi	a3,a3,48
	bgtu	s4,a1,.L114
	bleu	a4,a2,.L162
	li	a4,805568512
	bleu	a5,a4,.L162
	li	a1,34
	li	a0,64
	call	fail_pre
.L263:
	li	a1,13
	li	a0,64
	call	fail_pre
.L262:
	li	a1,12
	li	a0,64
	call	fail_pre
.L173:
	li	a1,19
	li	a0,64
	call	fail_pre
.L270:
	bne	a6,zero,.L78
	add	a5,a1,a5
	sltu	a1,a5,a1
	add	a0,a0,a2
	mv	s3,a5
	add	a2,a1,a0
	j	.L66
.L45:
	li	a1,14
	li	a0,64
	call	fail_pre
.L265:
	bleu	s3,a4,.L52
	li	a1,16
	li	a0,64
	call	fail_pre
.L277:
	bleu	t5,a0,.L98
.L91:
	li	a1,29
	li	a0,64
	call	fail_pre
.L276:
	bgtu	a6,t5,.L91
	j	.L96
.L275:
	li	a1,28
	li	a0,64
	call	fail_pre
.L273:
	li	a1,26
	li	a0,64
	call	fail_pre
.L85:
	li	a1,25
	li	a0,64
	call	fail_pre
.L272:
	li	a1,24
	li	a0,64
	call	fail_pre
.L176:
	li	a1,23
	li	a0,64
	call	fail_pre
.L175:
	li	a1,22
	li	a0,64
	call	fail_pre
.L78:
	li	a1,20
	li	a0,64
	call	fail_pre
.L274:
	li	a1,27
	li	a0,64
	call	fail_pre
.L278:
	li	a3,805306368
	bgeu	a4,a3,.L178
.L99:
	li	a1,30
	li	a0,64
	call	fail_pre
.L160:
	li	a5,-1
	j	.L104
.L107:
	li	a1,31
	li	a0,64
	call	fail_pre
.L110:
	li	a1,32
	li	a0,64
	call	fail_pre
.L113:
	li	a1,33
	li	a0,64
	call	fail_pre
.L162:
	mv	a5,s5
.L121:
	addi	s3,s3,1
	mv	a3,s3
	mv	a4,a5
.L117:
	bgeu	a3,s4,.L279
	lw	a6,40(a5)
	lw	a0,92(a4)
	lw	a1,44(a5)
	lw	a2,88(a4)
	bgeu	a6,a0,.L118
	bgtu	a1,a2,.L119
.L118:
	addi	a3,a3,1
	addi	a4,a4,48
	j	.L117
.L279:
	addi	a5,a5,48
	bgtu	s4,s3,.L121
	lbu	a3,29(s2)
	lbu	a2,28(s2)
	lbu	a4,30(s2)
	lbu	a5,31(s2)
	slli	a3,a3,8
	or	a3,a3,a2
	slli	a4,a4,16
	or	a4,a4,a3
	slli	a5,a5,24
	or	a5,a5,a4
	bne	a5,zero,.L122
	lbu	a4,37(s2)
	lbu	a3,36(s2)
	lbu	a5,38(s2)
	lbu	s11,39(s2)
	slli	a4,a4,8
	or	a4,a4,a3
	slli	a5,a5,16
	or	a5,a5,a4
	slli	s11,s11,24
	or	s11,s11,a5
	beq	s11,zero,.L123
.L122:
	li	a1,42
	li	a0,64
	call	fail_pre
.L119:
	li	a1,35
	li	a0,64
	call	fail_pre
.L123:
	addi	s3,s2,24
	mv	a0,s3
	call	rd64
	mv	a4,a0
	mv	a3,s5
	li	a6,0
.L128:
	lbu	a5,36(a3)
	andi	a5,a5,4
	beq	a5,zero,.L124
	lw	a2,4(a3)
	lw	a5,0(a3)
	bgtu	a2,a1,.L124
	bne	a2,a1,.L179
	bgtu	a5,a4,.L124
.L179:
	lw	a0,8(a3)
	lw	t1,12(a3)
	li	a7,1
	add	a0,a5,a0
	sltu	a5,a0,a5
	add	a2,a2,t1
	add	a5,a5,a2
	bgtu	a5,a1,.L126
	beq	a5,a1,.L280
.L127:
	li	a7,0
.L126:
	or	a6,a6,a7
.L124:
	addi	s11,s11,1
	addi	a3,a3,48
	bgtu	s4,s11,.L128
	beq	a6,zero,.L281
	andi	a4,a4,3
	bne	a4,zero,.L282
	li	a1,4096
	addi	a6,a1,-1
	and	a5,s9,a6
	bne	a5,zero,.L283
	li	a5,805502976
	bne	s9,a5,.L180
	beq	s10,zero,.L246
.L180:
	li	a1,39
	li	a0,64
	call	fail_pre
.L283:
	li	a1,38
	li	a0,64
	call	fail_pre
.L282:
	li	a1,37
	li	a0,64
	call	fail_pre
.L281:
	li	a1,36
	li	a0,64
	call	fail_pre
.L280:
	bleu	a0,a4,.L127
	or	a6,a6,a7
	j	.L124
.L246:
	addi	a5,s7,-1
	li	a4,262144
	bgeu	a5,a4,.L136
	and	a6,s7,a6
	beq	a6,zero,.L137
.L136:
	li	a1,40
	li	a0,64
	call	fail_pre
.L137:
	mv	a0,s9
	mv	a4,s0
	li	a2,3
	li	a3,17
	li	a5,0
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	t1,-4096
	bgtu	a0,t1,.L284
	lbu	a3,45(s2)
	lbu	a0,44(s2)
	lbu	a4,46(s2)
	lbu	a1,47(s2)
	slli	a3,a3,8
	or	a3,a3,a0
	slli	a4,a4,16
	or	a4,a4,a3
	slli	a1,a1,24
	or	a1,a1,a4
	li	a0,805568512
	li	a3,50
	li	a4,-1
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t1,.L285
	li	a0,805564416
	li	a1,4096
	li	a2,0
	li	a3,50
	li	a4,-1
	li	a5,0
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	a5,-4096
	bgtu	a0,a5,.L286
	mv	t3,a5
	mv	t1,s5
	li	t4,0
.L140:
	lw	a1,44(t1)
	lw	a0,40(t1)
	li	a2,3
	li	a3,50
	sub	a1,a1,a0
	li	a4,-1
	li	a5,0
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t3,.L287
	addi	t4,t4,1
	addi	t1,t1,48
	bgtu	s4,t4,.L140
	mv	a5,s5
	li	a3,0
.L143:
	lw	a1,8(a5)
	lw	a4,12(a5)
	or	a4,a1,a4
	bne	a4,zero,.L142
.L147:
	addi	a3,a3,1
	addi	a5,a5,48
	bgtu	s4,a3,.L143
	mv	t1,s5
	li	t3,0
	li	t4,-4096
.L144:
	lbu	a5,36(t1)
	andi	a4,a5,2
	andi	a2,a5,1
	beq	a4,zero,.L148
	ori	a2,a2,2
.L148:
	andi	a5,a5,4
	beq	a5,zero,.L149
	ori	a2,a2,4
.L149:
	lw	a1,44(t1)
	lw	a0,40(t1)
	li	a3,0
	li	a4,0
	sub	a1,a1,a0
	li	a5,0
	li	a7,226
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t4,.L288
	addi	t3,t3,1
	addi	t1,t1,48
	bgtu	s4,t3,.L144
	li	t1,-4096
.L152:
	lbu	a5,36(s5)
	andi	a5,a5,4
	beq	a5,zero,.L151
	lw	a0,40(s5)
	lw	a1,44(s5)
	li	a2,0
	li	a3,0
	li	a4,0
	li	a5,0
	li	a7,259
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t1,.L289
.L151:
	addi	a6,a6,1
	addi	s5,s5,48
	bgtu	s4,a6,.L152
	lbu	a3,17(s2)
	lbu	a2,16(s2)
	lbu	a4,18(s2)
	lbu	a5,19(s2)
	slli	a3,a3,8
	or	a3,a3,a2
	slli	a4,a4,16
	or	a4,a4,a3
	slli	a5,a5,24
	or	a5,a5,a4
	srli	a2,a5,8
	srli	a3,a5,16
	srli	a4,a5,24
	li	s4,1
	addi	s5,s2,56
	sb	a5,12(s1)
	sb	a2,13(s1)
	sb	a3,14(s1)
	sb	a4,15(s1)
	sb	zero,17(s1)
	sb	s4,16(s1)
	mv	a0,s5
	call	rd64
	mv	a4,a1
	mv	a5,a0
	srli	a7,a0,8
	srli	a6,a0,16
	srli	a1,a1,8
	srli	a0,a0,24
	srli	a2,a4,16
	srli	a3,a4,24
	sb	a4,28(s1)
	sb	zero,18(s1)
	sb	zero,19(s1)
	sb	a5,24(s1)
	sb	a7,25(s1)
	sb	a6,26(s1)
	sb	a0,27(s1)
	sb	a1,29(s1)
	sb	a2,30(s1)
	sb	a3,31(s1)
#APP
# 532 "asm/helpers/riscv.c" 1
	fence rw,w
# 0 "" 2
#NO_APP
	li	s7,2
	sb	zero,221(s1)
	sb	zero,222(s1)
	sb	zero,223(s1)
	mv	a0,s3
	sb	s7,220(s1)
	call	rd64
	lbu	a3,45(s2)
	lbu	a2,44(s2)
	lbu	a4,46(s2)
	lbu	a5,47(s2)
	slli	a3,a3,8
	or	a3,a3,a2
	slli	a4,a4,16
	or	a4,a4,a3
	lw	a2,%lo(control_alias)(s6)
	slli	a5,a5,24
	or	a5,a5,a4
	li	a1,805568512
	add	a1,a5,a1
	addi	a3,sp,36
	call	riscv_run_guest
	lui	a5,%hi(guest_sp_expected)
	lw	s0,36(sp)
	lw	a4,40(sp)
	lw	a5,%lo(guest_sp_expected)(a5)
	srai	s2,s0,31
	beq	a4,a5,.L153
	lw	a5,%lo(control_alias)(s6)
	sb	s4,218(a5)
.L252:
	sb	zero,219(a5)
#APP
# 180 "asm/helpers/riscv.c" 1
	fence rw,w
# 0 "" 2
#NO_APP
	li	a0,68
	sb	zero,217(a5)
	sb	a0,216(a5)
	call	exit_class
.L284:
	mv	a1,a2
	li	a0,66
	call	fail_pre
.L287:
	li	a1,6
	li	a0,66
	call	fail_pre
.L142:
	lw	a7,0(a5)
	lw	a0,24(a5)
	li	a4,0
.L145:
	add	t1,a4,a7
	beq	a1,a4,.L147
	add	a2,a4,a0
	add	a2,s2,a2
	lbu	a2,0(a2)
	addi	a4,a4,1
	sb	a2,0(t1)
	j	.L145
.L286:
	li	a1,5
	li	a0,66
	call	fail_pre
.L288:
	li	a1,7
	li	a0,66
	call	fail_pre
.L289:
	li	a1,3
	li	a0,67
	call	fail_pre
.L285:
	li	a1,4
	li	a0,66
	call	fail_pre
.L153:
	lw	a5,44(sp)
	beq	a5,zero,.L154
	lw	a5,%lo(control_alias)(s6)
	sb	s7,218(a5)
	j	.L252
.L154:
	srli	a0,s0,8
	srli	a1,s0,16
	srli	a2,s0,24
	srli	a3,s2,8
	srli	a4,s2,16
	srli	a5,s2,24
	sb	s0,88(s1)
	sb	s2,92(s1)
	sb	s4,18(s1)
	sb	zero,19(s1)
	sb	zero,20(s1)
	sb	zero,21(s1)
	sb	zero,22(s1)
	sb	zero,23(s1)
	sb	a0,89(s1)
	sb	a1,90(s1)
	sb	a2,91(s1)
	sb	a3,93(s1)
	sb	a4,94(s1)
	sb	a5,95(s1)
#APP
# 573 "asm/helpers/riscv.c" 1
	fence rw,w
# 0 "" 2
#NO_APP
	mv	a0,s5
	call	rd64
	beq	a0,s0,.L290
.L164:
	li	s8,3
.L155:
	sb	s8,10(s1)
	sb	zero,11(s1)
	li	a0,0
	call	exit_class
.L290:
	bne	a1,s2,.L164
	j	.L155
	.cfi_endproc
.LFE19:
	.size	helper_main, .-helper_main
	.align	2
	.globl	_start
	.type	_start, @function
_start:
.LFB20:
	.cfi_startproc
#APP
# 593 "asm/helpers/riscv.c" 1
	mv a0,sp
 tail helper_main
# 0 "" 2
#NO_APP
	.cfi_endproc
.LFE20:
	.size	_start, .-_start
	.bss
	.align	2
	.type	saved_s, @object
	.size	saved_s, 48
saved_s:
	.zero	48
	.section	.sbss,"aw",@nobits
	.align	2
	.type	guest_out, @object
	.size	guest_out, 4
guest_out:
	.zero	4
	.type	guest_sp_expected, @object
	.size	guest_sp_expected, 4
guest_sp_expected:
	.zero	4
	.type	saved_ra, @object
	.size	saved_ra, 4
saved_ra:
	.zero	4
	.type	saved_helper_sp, @object
	.size	saved_helper_sp, 4
saved_helper_sp:
	.zero	4
	.type	control_alias, @object
	.size	control_alias, 4
control_alias:
	.zero	4
	.section	.srodata,"a"
	.align	2
	.type	mmagic.0, @object
	.size	mmagic.0, 8
mmagic.0:
	.string	"ASMEXE"
	.ascii	"\002"
	.type	empty_path.1, @object
	.size	empty_path.1, 1
empty_path.1:
	.zero	1
	.zero	3
	.type	rmagic.2, @object
	.size	rmagic.2, 8
rmagic.2:
	.string	"ASMRES"
	.ascii	"\002"
	.ident	"GCC: (crosstool-NG 1.27.0) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
