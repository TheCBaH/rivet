	.option norelax
	.option norvc
	.text
	.globl asm_snippet
asm_snippet:
	addi x8, x0, 0
	addi x10, x0, 42
	jalr x0, 0(x1)
