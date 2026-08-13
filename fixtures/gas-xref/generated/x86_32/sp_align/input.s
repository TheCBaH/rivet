	.text
	.globl asm_snippet
asm_snippet:
	movl %esp, %eax
	andl $15, %eax
	cmpl $12, %eax
	movl $42, %eax
	je (. + 7)
	movl $41, %eax
	ret
