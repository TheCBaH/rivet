	.arch armv8-a
	.file	"data.c"
	.text
	.global	shared_value
	.data
	.align	2
	.type	shared_value, %object
	.size	shared_value, 4
shared_value:
	.word	20
	.ident	"GCC: (Debian 14.2.0-19) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
