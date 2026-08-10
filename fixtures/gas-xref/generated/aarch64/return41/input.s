	.text
	.globl asm_snippet
asm_snippet:
	mov x15, sp
	stp x15, x30, [sp, #-16]!
	movz w0, #41
	ldr x30, [sp, #8]
	add sp, sp, #16
	ret
