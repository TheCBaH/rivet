	.text
	.globl asm_snippet
asm_snippet:
	mov x0, sp
	and x0, x0, #15
	cmp x0, #0
	movz w0, #42
	b.eq (. + 8)
	movz w0, #41
	ret
