	.text
	.globl asm_snippet
asm_snippet:
	sub x1, sp, #4, lsl #12
	strb wzr, [x1, #0]
	movz w0, #42
	ret
