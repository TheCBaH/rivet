	.text
	.globl asm_snippet
asm_snippet:
	subl $12, %esp
	leal 16(%esp), %eax
	movl %eax, (%esp)
	movl $42, %eax
	addl $12, %esp
	ret
