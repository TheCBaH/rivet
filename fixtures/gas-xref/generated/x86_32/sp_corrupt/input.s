	.text
	.globl asm_snippet
asm_snippet:
	pop %edx
	subl $16, %esp
	movl $42, %eax
	jmp *%edx
