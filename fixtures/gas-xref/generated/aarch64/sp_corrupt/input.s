	.text
	.globl asm_snippet
asm_snippet:
	sub sp, sp, #16
	movz w0, #42
	ret
