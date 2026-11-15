	.text
	.byte 1
	.pushsection .data,"aw"
	.byte 2
	.popsection
	.byte 3
	.section .rodata,"a"
	.byte 9
	.previous
	.byte 4
	.pushsection .rodata,"a"
	.byte 8
	.pushsection .data,"aw"
	.byte 7
	.popsection
	.byte 6
	.popsection
	.byte 5
	.text
	.balign 4,0
