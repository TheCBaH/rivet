	.option norelax
	.option norvc
	.text
	.globl asm_snippet
asm_snippet:
	addi x5, x1, 0
	addi x2, x2, -16
	addi x10, x0, 42
	jalr x0, 0(x5)
