	.text
	.globl asm_snippet
asm_snippet:
	sub x1, sp, #4, lsl #12
	sub x1, x1, #1
	strb wzr, [x1]
	movz w0, #42
	ret
