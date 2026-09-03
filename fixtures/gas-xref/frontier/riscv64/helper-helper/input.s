# GCC -S snapshot of asm/helpers/riscv.c (ABI_VERSION=2), riscv64.
#
# NOT a build input: tools/asm-helpers.sh's own riscv64 leg (build_riscv)
# compiles riscv.c directly with -c, never through this file. This snapshot
# exists only so asm/tools' gas-xref frontier corpus - which reads
# asm/helpers/<target>.s unconditionally, the same way it already does for
# the four legacy profiles' own hand-written helper sources - has real,
# substantial RISC-V assembly to compare this project's own parser/lowering
# against (M5, asm/docs/corpus.md).
#
# Regenerate only if riscv.c itself changes, with:
#
#   riscv64-linux-gnu-gcc -march=rv64imafd -mabi=lp64d -mno-relax \
#     -O2 -ffreestanding -fno-builtin -fno-stack-protector -fno-pic \
#     -fomit-frame-pointer -nostdlib -S -o asm/helpers/riscv64.s \
#     asm/helpers/riscv.c
#
# An incidental gcc-version drift would regenerate byte-identical output only
# if the toolchain matches; this file's own bytes are what a gas-xref regen
# hashes, not riscv.c's.
	.file	"riscv.c"
	.option nopic
	.option norelax
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
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
  sd a3,0(t0)
  la t0,saved_helper_sp
  sd sp,0(t0)
  la t0,saved_ra
  sd ra,0(t0)
  la t0,saved_s
  sd s0,0(t0)
  sd s1,8(t0)
  sd s2,16(t0)
  sd s3,24(t0)
  sd s4,32(t0)
  sd s5,40(t0)
  sd s6,48(t0)
  sd s7,56(t0)
  sd s8,64(t0)
  sd s9,72(t0)
  sd s10,80(t0)
  sd s11,88(t0)
  la t0,guest_sp_expected
  sd a1,0(t0)
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
  ld t1,0(t0)
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
  ld sp,0(t0)
  la t0,saved_s
  ld s0,0(t0)
  ld s1,8(t0)
  ld s2,16(t0)
  ld s3,24(t0)
  ld s4,32(t0)
  ld s5,40(t0)
  ld s6,48(t0)
  ld s7,56(t0)
  ld s8,64(t0)
  ld s9,72(t0)
  ld s10,80(t0)
  ld s11,88(t0)
  la t0,saved_ra
  ld ra,0(t0)
  la t0,guest_out
  ld t0,0(t0)
  sd t3,0(t0)
  sd t4,8(t0)
  sd t5,16(t0)
  ret
.size riscv_run_guest,.-riscv_run_guest

#NO_APP
	.align	2
	.type	rd64, @function
rd64:
.LFB2:
	.cfi_startproc
	lbu	a5,1(a0)
	lbu	a4,0(a0)
	lbu	a1,2(a0)
	lbu	a2,3(a0)
	lbu	a3,4(a0)
	slli	a5,a5,8
	or	a5,a5,a4
	slli	a1,a1,16
	lbu	a4,5(a0)
	or	a1,a1,a5
	slli	a2,a2,24
	lbu	a5,6(a0)
	or	a2,a2,a1
	lbu	a0,7(a0)
	slli	a3,a3,32
	or	a3,a3,a2
	slli	a4,a4,40
	or	a4,a4,a3
	slli	a5,a5,48
	or	a5,a5,a4
	slli	a0,a0,56
	or	a0,a0,a5
	ret
	.cfi_endproc
.LFE2:
	.size	rd64, .-rd64
	.align	2
	.type	exit_class, @function
exit_class:
.LFB12:
	.cfi_startproc
	slli	a0,a0,32
	srli	a0,a0,32
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
	ld	a5,%lo(control_alias)(a5)
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	sd	ra,8(sp)
	.cfi_offset 1, -8
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
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align	3
.LC0:
	.string	"pgsz-bad"
	.align	3
.LC1:
	.string	"pgsz-abs"
	.text
	.align	2
	.type	helper_main, @function
helper_main:
.LFB19:
	.cfi_startproc
	ld	a3,0(a0)
	addi	sp,sp,-880
	.cfi_def_cfa_offset 880
	sd	ra,872(sp)
	sd	s0,864(sp)
	li	a5,2
	.cfi_offset 1, -8
	.cfi_offset 8, -16
	bleu	a3,a5,.L8
	slli	a5,a3,3
	addi	a5,a5,16
	add	a5,a0,a5
	ld	a4,0(a5)
	mv	t1,a0
	beq	a4,zero,.L10
.L9:
	ld	a4,8(a5)
	addi	a5,a5,8
	bne	a4,zero,.L9
.L10:
	ld	a6,8(a5)
	li	t3,0
	addi	a5,a5,8
	beq	a6,zero,.L11
	li	a2,0
	li	a4,6
.L13:
	bne	a6,a4,.L12
	ld	a2,8(a5)
	li	t3,1
.L12:
	ld	a6,16(a5)
	addi	a5,a5,16
	bne	a6,zero,.L13
	mv	a6,a2
.L11:
	li	a5,3
	beq	a3,a5,.L14
	ld	a2,32(t1)
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
	ld	a1,24(t1)
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
	sd	s1,856(sp)
	addi	a3,a3,34
	li	a0,1073741824
	li	a1,1048576
	li	a4,-1
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	li	a4,1073741824
	.cfi_offset 9, -24
	beq	a0,a4,.L19
.L20:
	li	a0,66
	sd	s2,848(sp)
	sd	s3,840(sp)
	sd	s4,832(sp)
	sd	s5,824(sp)
	sd	s6,816(sp)
	sd	s7,808(sp)
	sd	s8,800(sp)
	sd	s9,792(sp)
	sd	s10,784(sp)
	sd	s11,776(sp)
	.cfi_offset 18, -32
	.cfi_offset 19, -40
	.cfi_offset 20, -48
	.cfi_offset 21, -56
	.cfi_offset 22, -64
	.cfi_offset 23, -72
	.cfi_offset 24, -80
	.cfi_offset 25, -88
	.cfi_offset 26, -96
	.cfi_offset 27, -104
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
	sd	s1,856(sp)
	sd	s2,848(sp)
	sd	s3,840(sp)
	sd	s4,832(sp)
	sd	s5,824(sp)
	sd	s6,816(sp)
	sd	s7,808(sp)
	sd	s8,800(sp)
	sd	s9,792(sp)
	sd	s10,784(sp)
	sd	s11,776(sp)
	.cfi_offset 9, -24
	.cfi_offset 18, -32
	.cfi_offset 19, -40
	.cfi_offset 20, -48
	.cfi_offset 21, -56
	.cfi_offset 22, -64
	.cfi_offset 23, -72
	.cfi_offset 24, -80
	.cfi_offset 25, -88
	.cfi_offset 26, -96
	.cfi_offset 27, -104
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
	sd	s6,816(sp)
	lui	a2,%hi(rmagic.2)
	.cfi_offset 22, -64
	lui	s6,%hi(control_alias)
	sd	s2,848(sp)
	sd	a0,%lo(control_alias)(s6)
	addi	a2,a2,%lo(rmagic.2)
	li	a5,0
	li	a1,8
	.cfi_offset 18, -32
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
	sd	s3,840(sp)
	sd	s4,832(sp)
	sd	s5,824(sp)
	sd	s7,808(sp)
	sd	s8,800(sp)
	sd	s9,792(sp)
	sd	s10,784(sp)
	sd	s11,776(sp)
	.cfi_offset 19, -40
	.cfi_offset 20, -48
	.cfi_offset 21, -56
	.cfi_offset 23, -72
	.cfi_offset 24, -80
	.cfi_offset 25, -88
	.cfi_offset 26, -96
	.cfi_offset 27, -104
	beq	t3,zero,.L195
	li	a5,4096
	beq	a6,a5,.L23
	li	a1,1
	li	a0,69
	call	fail_pre
.L23:
	ld	a1,16(t1)
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
	bgtu	a0,s4,.L196
	lui	a1,%hi(empty_path.1)
	mv	a2,a6
	addi	a1,a1,%lo(empty_path.1)
	li	a3,512
	addi	a4,sp,64
	li	a7,291
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,s4,.L197
	addi	a0,sp,104
	call	rd64
	li	a5,119
	mv	s9,a0
	bleu	a0,a5,.L198
	li	a5,1048576
	bgtu	a0,a5,.L199
	mv	a3,s2
	mv	a4,s3
	li	a0,0
	mv	a1,s9
	li	a2,1
	li	a5,0
	li	a7,222
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	mv	s2,a0
	bgtu	a0,s4,.L200
	lui	a2,%hi(mmagic.0)
	addi	a2,a2,%lo(mmagic.0)
	li	a3,0
	li	a1,8
.L28:
	add	a4,s2,a3
	add	a5,a2,a3
	lbu	a4,0(a4)
	lbu	a5,0(a5)
	bne	a4,a5,.L29
	addi	a3,a3,1
	bne	a3,a1,.L28
	lbu	s8,9(s2)
	lbu	a5,8(s2)
	li	a4,2
	slli	s8,s8,8
	or	a2,s8,a5
	mv	s8,a2
	beq	a2,a4,.L190
	li	a1,4
	li	a0,64
	call	fail_pre
.L195:
	mv	a1,s2
	li	a0,69
	call	fail_pre
.L196:
	li	a1,3
	li	a0,65
	call	fail_pre
.L197:
	li	a1,4
	li	a0,65
	call	fail_pre
.L198:
	li	a1,1
	li	a0,64
	call	fail_pre
.L199:
	mv	a1,s2
	li	a0,64
	call	fail_pre
.L200:
	li	a1,5
	li	a0,65
	call	fail_pre
.L29:
	li	a1,3
.L193:
	li	a0,64
	call	fail_pre
.L190:
	lbu	a1,11(s2)
	lbu	a4,10(s2)
	li	a5,6
	slli	a1,a1,8
	or	a1,a1,a4
	beq	a1,a5,.L31
	li	a1,5
	li	a0,64
	call	fail_pre
.L31:
	lbu	a2,13(s2)
	lbu	a0,12(s2)
	lbu	a4,14(s2)
	lbu	a5,15(s2)
	slli	a2,a2,8
	or	a2,a2,a0
	slli	a4,a4,16
	or	a4,a4,a2
	slli	a5,a5,24
	or	a5,a5,a4
	bne	a5,s9,.L193
	lbu	s4,21(s2)
	lbu	a4,20(s2)
	li	a1,7
	slli	s4,s4,8
	or	a5,s4,a4
	addiw	a2,a5,-1
	mv	s4,a5
	bgtu	a2,a1,.L193
	lbu	a4,23(s2)
	lbu	a1,22(s2)
	li	a2,1
	slli	a4,a4,8
	or	a4,a4,a1
	beq	a4,a2,.L34
	mv	a1,a3
	li	a0,64
	call	fail_pre
.L34:
	lbu	a2,41(s2)
	lbu	a1,40(s2)
	lbu	a3,42(s2)
	lbu	a4,43(s2)
	slli	a2,a2,8
	or	a2,a2,a1
	slli	a3,a3,16
	or	a3,a3,a2
	slli	a4,a4,24
	or	a4,a4,a3
	sext.w	a4,a4
	li	a3,256
	beq	a4,a3,.L35
	li	a1,9
	li	a0,64
	call	fail_pre
.L35:
	lbu	a2,49(s2)
	lbu	a1,48(s2)
	lbu	a3,50(s2)
	lbu	a4,51(s2)
	slli	a2,a2,8
	or	a2,a2,a1
	slli	a3,a3,16
	or	a3,a3,a2
	slli	a4,a4,24
	or	a4,a4,a3
	li	a3,61440
	addiw	a4,a4,-100
	addi	a3,a3,-1540
	bgtu	a4,a3,.L201
	lbu	a2,53(s2)
	lbu	a1,52(s2)
	lbu	a3,54(s2)
	lbu	a4,55(s2)
	slli	a2,a2,8
	or	a2,a2,a1
	slli	a3,a3,16
	or	a3,a3,a2
	slli	a4,a4,24
	or	a4,a4,a3
	sext.w	a2,a4
	bne	a4,zero,.L202
	addi	a3,s2,64
	addi	s7,s2,120
	li	a4,0
.L38:
	lbu	a1,0(a3)
	addi	a3,a3,1
	or	a4,a4,a1
	bne	a3,s7,.L38
	bne	a4,zero,.L203
	li	a3,40
	mulw	a5,a5,a3
	addi	s3,a5,120
	bgtu	s3,s9,.L204
	addi	s5,sp,320
	mv	s10,s5
	li	a6,0
.L45:
	mv	a0,s7
	sd	a4,24(sp)
	sd	a6,16(sp)
	sd	a2,8(sp)
	call	rd64
	mv	a5,a0
	sd	a5,0(s10)
	addi	a0,s7,8
	call	rd64
	mv	s11,a0
	sd	s11,8(s10)
	addi	a0,s7,16
	call	rd64
	mv	a5,a0
	sd	a5,16(s10)
	addi	a0,s7,24
	call	rd64
	mv	a5,a0
	lbu	a0,33(s7)
	lbu	a3,32(s7)
	lbu	a1,34(s7)
	slli	a0,a0,8
	or	a0,a0,a3
	lbu	a3,35(s7)
	slli	a1,a1,16
	or	a1,a1,a0
	lbu	a0,36(s7)
	slli	a3,a3,24
	or	a3,a3,a1
	sd	a5,24(s10)
	sw	a3,32(s10)
	add	a5,s11,a5
	sb	a0,36(s10)
	sltu	s11,a5,s11
	bltu	s9,a5,.L43
	ld	a2,8(sp)
	ld	a6,16(sp)
	ld	a4,24(sp)
	bne	s11,zero,.L43
	addiw	a6,a6,1
	addi	s7,s7,40
	addi	s10,s10,56
	bgtu	s4,a6,.L45
	mv	a3,s5
	li	a1,0
.L49:
	ld	a5,24(a3)
	andi	a0,a5,7
	bne	a0,zero,.L205
	beq	a5,zero,.L47
	bgtu	s3,a5,.L206
.L47:
	ld	a0,8(a3)
	snez	a5,a5
	snez	a0,a0
	bne	a5,a0,.L207
	addiw	a1,a1,1
	addi	a3,a3,56
	bgtu	s4,a1,.L49
	mv	a5,s5
	li	a3,0
	j	.L54
.L50:
	addi	a5,a5,56
	bleu	s4,a3,.L208
.L54:
	ld	a1,8(a5)
	addiw	a3,a3,1
	beq	a1,zero,.L50
	ld	t1,24(a5)
	mv	a0,a5
	mv	a7,a3
	add	a1,a1,t1
.L51:
	bgeu	a7,s4,.L50
	ld	a6,64(a0)
	beq	a6,zero,.L52
	ld	t3,80(a0)
	add	a6,a6,t3
	bgeu	t1,a6,.L52
	bltu	t3,a1,.L209
.L52:
	addiw	a7,a7,1
	addi	a0,a0,56
	j	.L51
.L201:
	li	a1,10
	li	a0,64
	call	fail_pre
.L202:
	li	a1,11
	li	a0,64
	call	fail_pre
.L205:
	li	a1,15
	li	a0,64
	call	fail_pre
.L207:
	li	a1,17
	li	a0,64
	call	fail_pre
.L208:
	mv	a3,s5
	li	a1,0
	j	.L60
.L55:
	addiw	a1,a1,1
	addi	a3,a3,56
	bleu	s4,a1,.L210
.L60:
	ld	a6,8(a3)
	beq	a6,zero,.L55
	ld	a0,24(a3)
	addi	a5,s3,7
	andi	a5,a5,-8
	bne	a0,a5,.L211
	add	s3,s2,s3
	li	a0,0
	addw	a7,a5,s2
	j	.L57
.L58:
	lbu	t1,0(s3)
	addi	s3,s3,1
	or	a0,a0,t1
.L57:
	sext.w	t1,s3
	bne	t1,a7,.L58
	bne	a0,zero,.L64
	add	s3,a6,a5
	j	.L55
.L210:
	addi	a5,s3,7
	andi	a5,a5,-8
	beq	s9,a5,.L61
	li	a1,21
	li	a0,64
	call	fail_pre
.L204:
	li	a1,13
	li	a0,64
	call	fail_pre
.L203:
	li	a1,12
	li	a0,64
	call	fail_pre
.L43:
	li	a1,14
	li	a0,64
	call	fail_pre
.L206:
	li	a1,16
	li	a0,64
	call	fail_pre
.L211:
	li	a1,19
	li	a0,64
	call	fail_pre
.L64:
	li	a1,20
	li	a0,64
	call	fail_pre
.L61:
	add	s3,s2,s3
	addw	s9,s9,s2
	j	.L62
.L63:
	lbu	a5,0(s3)
	addi	s3,s3,1
	or	a4,a4,a5
.L62:
	sext.w	a5,s3
	bne	a5,s9,.L63
	bne	a4,zero,.L64
	mv	a5,s5
	li	a1,262144
	li	a3,1048576
.L67:
	ld	a0,8(a5)
	bgtu	a0,a1,.L212
	ld	a0,16(a5)
	bgtu	a0,a3,.L213
	addiw	a4,a4,1
	addi	a5,a5,56
	bgtu	s4,a4,.L67
	li	a6,4096
	addi	a6,a6,-1
	mv	a5,s5
	li	t1,7
	li	a7,6
.L85:
	ld	t4,8(a5)
	ld	a1,16(a5)
	or	a4,t4,a1
	beq	a4,zero,.L214
	lw	s10,32(a5)
	beq	s10,zero,.L69
	addiw	a4,s10,-1
	and	s10,s10,a4
	bne	s10,zero,.L69
	slli	a3,a4,32
	ld	a4,0(a5)
	srli	a3,a3,32
	and	a3,a3,a4
	bne	a3,zero,.L215
	lbu	a3,36(a5)
	bgtu	a3,t1,.L216
	andi	a3,a3,6
	beq	a3,a7,.L217
	add	a0,t4,a4
	add	t3,a1,a0
	add	a3,t3,a6
	sltu	a0,a0,t4
	sltu	a1,t3,a1
	bltu	a3,t3,.L80
	li	s3,-4096
	and	a3,a3,s3
	bne	a0,zero,.L80
	bne	a1,zero,.L80
	and	a4,a4,s3
	li	a1,1073741824
	bltu	a4,a1,.L83
	li	a1,1074790400
	bgtu	a3,a1,.L83
	sd	a4,40(a5)
	sd	a3,48(a5)
	addiw	a2,a2,1
	addi	a5,a5,56
	bgtu	s4,a2,.L85
	addi	a0,s2,32
	call	rd64
	lbu	a3,45(s2)
	lbu	a2,44(s2)
	lbu	a4,46(s2)
	lbu	a5,47(s2)
	slli	a3,a3,8
	slli	a4,a4,16
	or	a3,a3,a2
	or	a3,a4,a3
	slli	a5,a5,24
	or	a6,a5,a3
	li	a3,1074003968
	and	a4,a0,s3
	li	a5,4096
	addw	a3,a3,a6
	slli	a3,a3,32
	add	a5,a4,a5
	mv	s9,a0
	srli	a3,a3,32
	sext.w	s7,a6
	bgeu	a5,a4,.L88
	li	a5,-1
.L88:
	mv	a2,s5
	li	a1,0
	li	a7,1074003968
	li	t1,1073999872
.L97:
	ld	a6,40(a2)
	ld	a0,48(a2)
	bgeu	a6,a5,.L89
	bgtu	a0,a4,.L90
.L89:
	bgeu	a6,a3,.L92
	bgtu	a0,a7,.L93
.L92:
	bgeu	a6,a7,.L95
	bgtu	a0,t1,.L96
.L95:
	addiw	a1,a1,1
	addi	a2,a2,56
	bgtu	s4,a1,.L97
	bgeu	a4,a3,.L137
	li	a4,1074003968
	bleu	a5,a4,.L137
	li	a1,34
	li	a0,64
	call	fail_pre
.L209:
	li	a1,18
	li	a0,64
	call	fail_pre
.L212:
	li	a1,22
	li	a0,64
	call	fail_pre
.L213:
	li	a1,23
	li	a0,64
	call	fail_pre
.L83:
	li	a1,30
	li	a0,64
	call	fail_pre
.L90:
	li	a1,31
	li	a0,64
	call	fail_pre
.L80:
	li	a1,29
	li	a0,64
	call	fail_pre
.L217:
	li	a1,28
	li	a0,64
	call	fail_pre
.L93:
	li	a1,32
	li	a0,64
	call	fail_pre
.L96:
	li	a1,33
	li	a0,64
	call	fail_pre
.L137:
	mv	a5,s5
	li	a4,0
.L104:
	addiw	a4,a4,1
	mv	a2,a4
	mv	a3,a5
.L100:
	bgeu	a2,s4,.L218
	ld	a7,40(a5)
	ld	a6,104(a3)
	ld	a0,48(a5)
	ld	a1,96(a3)
	bgeu	a7,a6,.L101
	bgtu	a0,a1,.L102
.L101:
	addiw	a2,a2,1
	addi	a3,a3,56
	j	.L100
.L216:
	li	a1,27
	li	a0,64
	call	fail_pre
.L69:
	li	a1,25
	li	a0,64
	call	fail_pre
.L214:
	li	a1,24
	li	a0,64
	call	fail_pre
.L215:
	li	a1,26
	li	a0,64
	call	fail_pre
.L218:
	addi	a5,a5,56
	bgtu	s4,a4,.L104
	addi	s3,s2,24
	mv	a0,s3
	call	rd64
	mv	a5,a0
	mv	a2,s5
	li	a3,0
.L106:
	lbu	a4,36(a2)
	andi	a4,a4,4
	beq	a4,zero,.L105
	ld	a4,0(a2)
	bltu	a5,a4,.L105
	ld	a1,8(a2)
	add	a4,a4,a1
	sltu	a4,a5,a4
	or	a3,a3,a4
	sext.w	a3,a3
.L105:
	addiw	s10,s10,1
	addi	a2,a2,56
	bgtu	s4,s10,.L106
	beq	a3,zero,.L219
	andi	a5,a5,3
	bne	a5,zero,.L220
	li	t3,4096
	addi	a4,t3,-1
	and	a5,s9,a4
	bne	a5,zero,.L221
	li	a5,1073938432
	beq	s9,a5,.L110
	li	a1,39
	li	a0,64
	call	fail_pre
.L102:
	li	a1,35
	li	a0,64
	call	fail_pre
.L219:
	li	a1,36
	li	a0,64
	call	fail_pre
.L110:
	addiw	a5,s7,-1
	li	a3,262144
	bgeu	a5,a3,.L111
	and	a6,s7,a4
	beq	a6,zero,.L112
.L111:
	li	a1,40
	li	a0,64
	call	fail_pre
.L112:
	mv	a0,s9
	mv	a4,s0
	mv	a1,t3
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
	bgtu	a0,t1,.L222
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
	li	a0,1074003968
	li	a3,50
	li	a4,-1
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t1,.L223
	mv	a1,t3
	li	a0,1073999872
	li	a2,0
#APP
# 157 "asm/helpers/riscv.c" 1
	ecall
# 0 "" 2
#NO_APP
	bgtu	a0,t1,.L224
	mv	t1,s5
	li	t3,0
	li	t4,-4096
.L115:
	ld	a1,48(t1)
	ld	a0,40(t1)
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
	bgtu	a0,t4,.L225
	addiw	t3,t3,1
	addi	t1,t1,56
	bgtu	s4,t3,.L115
	mv	a5,s5
	li	a2,0
.L118:
	ld	a4,8(a5)
	bne	a4,zero,.L117
.L122:
	addiw	a2,a2,1
	addi	a5,a5,56
	bgtu	s4,a2,.L118
	mv	t1,s5
	li	t3,0
	li	t4,-4096
.L119:
	lbu	a5,36(t1)
	andi	a4,a5,2
	andi	a2,a5,1
	beq	a4,zero,.L123
	ori	a2,a2,2
.L123:
	andi	a5,a5,4
	beq	a5,zero,.L124
	ori	a2,a2,4
.L124:
	ld	a1,48(t1)
	ld	a0,40(t1)
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
	bgtu	a0,t4,.L226
	addiw	t3,t3,1
	addi	t1,t1,56
	bgtu	s4,t3,.L119
	li	t1,-4096
.L127:
	lbu	a5,36(s5)
	andi	a5,a5,4
	beq	a5,zero,.L126
	ld	a0,40(s5)
	ld	a1,48(s5)
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
	bgtu	a0,t1,.L227
.L126:
	addiw	a6,a6,1
	addi	s5,s5,56
	bgtu	s4,a6,.L127
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
	sext.w	a5,a5
	srliw	a2,a5,8
	srliw	a3,a5,16
	srliw	a4,a5,24
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
	mv	a5,a0
	srliw	a7,a0,8
	srliw	a6,a0,16
	srli	a1,a5,32
	srliw	a0,a0,24
	srli	a2,a5,40
	srli	a3,a5,48
	srli	a4,a5,56
	sb	zero,18(s1)
	sb	zero,19(s1)
	sb	a5,24(s1)
	sb	a7,25(s1)
	sb	a6,26(s1)
	sb	a0,27(s1)
	sb	a1,28(s1)
	sb	a2,29(s1)
	sb	a3,30(s1)
	sb	a4,31(s1)
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
	lbu	a4,45(s2)
	lbu	a3,44(s2)
	lbu	a5,46(s2)
	lbu	a1,47(s2)
	slli	a4,a4,8
	or	a4,a4,a3
	slli	a5,a5,16
	or	a5,a5,a4
	slli	a1,a1,24
	or	a1,a1,a5
	li	a5,1074003968
	addw	a1,a1,a5
	ld	a2,%lo(control_alias)(s6)
	slli	a1,a1,32
	addi	a3,sp,40
	srli	a1,a1,32
	call	riscv_run_guest
	lui	a5,%hi(guest_sp_expected)
	ld	a4,40(sp)
	ld	a3,48(sp)
	ld	a5,%lo(guest_sp_expected)(a5)
	sext.w	s0,a4
	beq	a3,a5,.L128
	ld	a5,%lo(control_alias)(s6)
	sb	s4,218(a5)
.L194:
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
.L221:
	li	a1,38
	li	a0,64
	call	fail_pre
.L220:
	li	a1,37
	li	a0,64
	call	fail_pre
.L225:
	li	a1,6
	li	a0,66
	call	fail_pre
.L224:
	li	a1,5
	li	a0,66
	call	fail_pre
.L223:
	li	a1,4
	li	a0,66
	call	fail_pre
.L222:
	mv	a1,a2
	li	a0,66
	call	fail_pre
.L117:
	ld	a7,0(a5)
	ld	a0,24(a5)
	sext.w	a3,a4
	li	a4,0
.L120:
	sext.w	a1,a4
	add	t1,a7,a4
	beq	a3,a1,.L122
	add	a1,a0,a4
	add	a1,s2,a1
	lbu	a1,0(a1)
	addi	a4,a4,1
	sb	a1,0(t1)
	j	.L120
.L226:
	li	a1,7
	li	a0,66
	call	fail_pre
.L227:
	li	a1,3
	li	a0,67
	call	fail_pre
.L128:
	ld	a5,56(sp)
	beq	a5,zero,.L129
	ld	a5,%lo(control_alias)(s6)
	sb	s7,218(a5)
	j	.L194
.L129:
	srai	a5,s0,63
	srliw	a7,a5,8
	srliw	a6,a5,16
	srliw	a0,s0,8
	srliw	a5,a5,24
	srliw	a1,s0,16
	srliw	a2,s0,24
	srli	a3,s0,32
	sb	a4,88(s1)
	sb	s4,18(s1)
	sb	zero,19(s1)
	sb	zero,20(s1)
	sb	zero,21(s1)
	sb	zero,22(s1)
	sb	zero,23(s1)
	sb	a7,93(s1)
	sb	a6,94(s1)
	sb	a5,95(s1)
	sb	a0,89(s1)
	sb	a1,90(s1)
	sb	a2,91(s1)
	sb	a3,92(s1)
#APP
# 573 "asm/helpers/riscv.c" 1
	fence rw,w
# 0 "" 2
#NO_APP
	mv	a0,s5
	call	rd64
	beq	a0,s0,.L130
	li	s8,3
.L130:
	sb	s8,10(s1)
	sb	zero,11(s1)
	li	a0,0
	call	exit_class
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
	.align	3
	.type	saved_s, @object
	.size	saved_s, 96
saved_s:
	.zero	96
	.section	.sbss,"aw",@nobits
	.align	3
	.type	guest_out, @object
	.size	guest_out, 8
guest_out:
	.zero	8
	.type	guest_sp_expected, @object
	.size	guest_sp_expected, 8
guest_sp_expected:
	.zero	8
	.type	saved_ra, @object
	.size	saved_ra, 8
saved_ra:
	.zero	8
	.type	saved_helper_sp, @object
	.size	saved_helper_sp, 8
saved_helper_sp:
	.zero	8
	.type	control_alias, @object
	.size	control_alias, 8
control_alias:
	.zero	8
	.section	.srodata,"a"
	.align	3
	.type	mmagic.0, @object
	.size	mmagic.0, 8
mmagic.0:
	.string	"ASMEXE"
	.ascii	"\002"
	.type	empty_path.1, @object
	.size	empty_path.1, 1
empty_path.1:
	.zero	1
	.zero	7
	.type	rmagic.2, @object
	.size	rmagic.2, 8
rmagic.2:
	.string	"ASMRES"
	.ascii	"\002"
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
