	.text
	.set A, 3
	.equ B, A*4
C = B+1
	.byte A, B, C
	.set A, 9
	.byte A
	.long B+C
	.zero A-7
	.byte B-A, 0
