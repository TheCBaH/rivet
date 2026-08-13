	.option norelax
	.option norvc
	.text
	.globl asm_snippet
asm_snippet:
	lui x5, 4
	sub x5, x2, x5
	addi x5, x5, -1
	sb x0, 0(x5)
	addi x10, x0, 42
	jalr x0, 0(x1)
