	.option norelax
	.option norvc
	.text
	.globl asm_snippet
asm_snippet:
	andi x5, x2, 15
	addi x10, x0, 42
	beq x5, x0, (. + 8)
	addi x10, x0, 41
	jalr x0, 0(x1)
