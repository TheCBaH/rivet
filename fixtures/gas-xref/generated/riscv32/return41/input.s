	.option norelax
	.option norvc
	.text
	.globl asm_snippet
asm_snippet:
	addi x10, x0, 41
	jalr x0, 0(x1)
