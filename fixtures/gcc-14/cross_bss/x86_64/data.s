	.file	"data.c"
	.text
	.globl	shared_value
	.bss
	.align 4
	.type	shared_value, @object
	.size	shared_value, 4
shared_value:
	.zero	4
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
