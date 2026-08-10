	.text
	.globl asm_snippet
asm_snippet:
	xorq %rbx, %rbx
	movl $42, %eax
	ret
