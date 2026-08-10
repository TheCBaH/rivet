	.text
	.globl asm_snippet
asm_snippet:
	leal -16380(%esp), %eax
	movb $0, (%eax)
	movl $42, %eax
	ret
