Sections named .text.*, .rodata.*, .data.* and .bss.* (what -ffunction-sections
and -fdata-sections emit) join their parent output section in input order,
each at its own alignment. A section whose permissions differ from the parent's
keeps its own name.

  $ asm() { ../../tool/asm.exe "$@"; }
  $ cat > a.s <<'EOF'
  > 	.text
  > 	.globl f
  > f:
  > 	ret
  > 	.section .text.g,"ax",@progbits
  > 	.p2align 4
  > 	.globl g
  > g:
  > 	call f
  > 	ret
  > 	.section .rodata.str1.1,"aMS",@progbits,1
  > .LC0:
  > 	.string "hi"
  > 	.section .data.x,"aw",@progbits
  > 	.long 7
  > 	.section .bss.y,"aw",@nobits
  > 	.zero 4
  > 	.section .text.w,"aw",@progbits
  > 	.byte 1
  > EOF
  $ cat > b.s <<'EOF'
  > 	.section .text.h,"ax",@progbits
  > 	.globl h
  > h:
  > 	call g
  > 	ret
  > EOF
  $ asm --target x86_64 a.s b.s
  segment .text size=28 zero=0 align=16 permissions=r-x
  segment .rodata size=3 zero=0 align=1 permissions=r--
  segment .data size=4 zero=0 align=1 permissions=rw-
  segment .bss size=0 zero=4 align=1 permissions=rw-
  segment .text.w size=1 zero=0 align=1 permissions=rw-
  export f
  export g
  export h
  unresolved target

  $ asm --target x86_64 --fixed-base 0x1000 --at .rodata=0x2000 --at .data=0x3000 --at .bss=0x4000 --at .text.w=0x5000 a.s b.s
  section .text address=0x1000 size=28 permissions=r-x
  section .rodata address=0x2000 size=3 permissions=r--
  section .data address=0x3000 size=4 permissions=rw-
  section .bss address=0x4000 size=4 permissions=rw-
  section .text.w address=0x5000 size=1 permissions=rw-
  export f = 0x1000
  export g = 0x1010
  export h = 0x1016

  $ asm --target x86_64 --no-section-grouping a.s | head -3
  segment .text size=1 zero=0 align=1 permissions=r-x
  segment .text.g size=6 zero=0 align=16 permissions=r-x
  segment .rodata.str1.1 size=3 zero=0 align=1 permissions=r--
