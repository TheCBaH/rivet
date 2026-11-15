	.text
	.comm c, 8, 8
	.local d
	.comm d, 4, 4
	.weak w
	.globl g
	.weak h
	.globl h
g:
	.long 1
w:
	.long 2
h:
	.long 3
