  $ sh run_asm.sh
  ########## x86_32 tokens
  1 5 directive .text
  6 1 eol \n
  8 6 directive .align
  15 2 int 16
  17 1 eol \n
  19 6 directive .globl
  26 14 ident asm_test_entry
  40 1 eol \n
  41 14 ident asm_test_entry
  55 1 colon :
  56 1 eol \n
  58 14 directive .cfi_startproc
  72 1 eol \n
  74 4 ident subl
  79 1 immsigil $
  80 2 int 12
  82 1 comma ,
  84 4 register %esp
  88 1 eol \n
  90 22 directive .cfi_adjust_cfa_offset
  113 2 int 12
  115 1 eol \n
  117 4 ident leal
  122 2 int 16
  124 1 lparen (
  125 4 register %esp
  129 1 rparen )
  130 1 comma ,
  132 4 register %eax
  136 1 eol \n
  138 4 ident movl
  143 4 register %eax
  147 1 comma ,
  149 1 int 0
  150 1 lparen (
  151 4 register %esp
  155 1 rparen )
  156 1 eol \n
  158 4 ident movl
  163 1 immsigil $
  164 2 int 42
  166 1 comma ,
  168 4 register %eax
  172 1 eol \n
  174 4 ident addl
  179 1 immsigil $
  180 2 int 12
  182 1 comma ,
  184 4 register %esp
  188 1 eol \n
  190 3 ident ret
  193 1 eol \n
  195 12 directive .cfi_endproc
  207 1 eol \n
  209 5 directive .type
  215 14 ident asm_test_entry
  229 1 comma ,
  231 1 at @
  232 8 ident function
  240 1 eol \n
  242 5 directive .size
  248 14 ident asm_test_entry
  262 1 comma ,
  264 1 dot .
  266 1 minus -
  268 14 ident asm_test_entry
  282 1 eol \n
  283 1 eol \n
  285 8 directive .section
  294 9 directive .note.GNU
  303 1 minus -
  304 5 ident stack
  309 1 comma ,
  310 2 string ""
  312 1 comma ,
  313 9 register %progbits
  322 1 eol \n
  323 0 eof 
  ########## x86_32 source ast
  source asm_test_entry
  directive .text
  directive .align [16]
  directive .globl [asm_test_entry]
  label asm_test_entry
  directive .cfi_startproc
  insn subl $12, %esp
  directive .cfi_adjust_cfa_offset [12]
  insn leal 16(%esp), %eax
  insn movl %eax, (%esp)
  insn movl $42, %eax
  insn addl $12, %esp
  insn ret
  directive .cfi_endproc
  directive .type [asm_test_entry] [@function]
  directive .size [asm_test_entry] [. - asm_test_entry]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## x86_32 normalized ast
  normalized asm_test_entry
  section .text r-x
  align 16
  globl asm_test_entry
  label asm_test_entry
  subl $12, %esp
  leal 16(%esp), %eax
  movl %eax, (%esp)
  movl $42, %eax
  addl $12, %esp
  ret
  type asm_test_entry function
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_32 lowered ast
  lowered asm_test_entry
  section .text r-x align=16
    align 16 fill=66 0f 1f 84 00 00 00 00 00 0f 1f 80 00 00 00 00
    label asm_test_entry
    bytes 83 ec 0c                 [x86_32.alu-rm-imm8.reg]
    bytes 8d 44 24 10              [x86_32.lea.sib-disp8]
    bytes 89 04 24                 [x86_32.mov-rm-r.sib-disp0]
    bytes b8 2a 00 00 00           [x86_32.mov-r-imm]
    bytes 83 c4 0c                 [x86_32.alu-rm-imm8.reg]
    bytes c3                       [x86_32.ret]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## x86_32 plan
  segment .text size=19 zero=0 align=16 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## x86_32 image
  section .text address=0x0 size=19 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=19
  ########## x86_32 bytes
  83 ec 0c 8d 44 24 10 89 04 24 b8 2a 00 00 00 83 c4 0c c3
  ########## x86_32 disasm canonical
  	subl $12, %esp
  	leal 16(%esp), %eax
  	movl %eax, (%esp)
  	movl $42, %eax
  	addl $12, %esp
  	ret
  ########## x86_32 disasm diagnostic
  00000000  83 ec 0c        subl $12, %esp       [x86_32.alu-rm-imm8.reg]
  00000003  8d 44 24 10     leal 16(%esp), %eax  [x86_32.lea.sib-disp8]
  00000007  89 04 24        movl %eax, (%esp)    [x86_32.mov-rm-r.sib-disp0]
  0000000a  b8 2a 00 00 00  movl $42, %eax       [x86_32.mov-r-imm]
  0000000f  83 c4 0c        addl $12, %esp       [x86_32.alu-rm-imm8.reg]
  00000012  c3              ret                  [x86_32.ret]
  ########## x86_32 codec
  alt x86_32
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){no-rex 10000011 modrm imm:8s}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){no-rex 10000001 modrm le32}
    [2 cost=0] mov-r-imm        mov-r-imm(){no-rex 10111 reg:3u le32}
    [3 cost=0] mov-rm-r         mov-rm-r(){no-rex 10001001 modrm}
    [4 cost=0] mov-r-rm         mov-r-rm(){no-rex 10001011 modrm}
    [5 cost=0] lea              lea(){no-rex 10001101 modrm}
    [6 cost=0] ret              ret(){11000011}
    [7 cost=0] mov-rm-imm8      mov-rm-imm8(){no-rex 11000110 modrm imm8:8s}
    [8 cost=0] xor-rm-r         xor-rm-r(){no-rex 00110001 modrm}
    [9 cost=0] pop-r            pop-r(){no-rex 01011 reg:3u}
    [10 cost=0] jmp-rm           jmp-rm(){no-rex 11111111 modrm}
    [11 cost=0] ud2              ud2(){0000111100001011}
  no-rex(){()}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib no-disp}
    [2 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp8:8s}
    [3 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib le32}
    [4 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u no-disp}
    [5 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp8:8s}
    [6 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u le32}
  le32(){imm:32u}
  sib(){scale:2u index:3u base:3u}
  no-disp(){()}
  ########## x86_64 tokens
  1 5 directive .text
  6 1 eol \n
  8 6 directive .align
  15 2 int 16
  17 1 eol \n
  19 6 directive .globl
  26 14 ident asm_test_entry
  40 1 eol \n
  41 14 ident asm_test_entry
  55 1 colon :
  56 1 eol \n
  58 14 directive .cfi_startproc
  72 1 eol \n
  74 4 ident subq
  79 1 immsigil $
  80 1 int 8
  81 1 comma ,
  83 4 register %rsp
  87 1 eol \n
  89 22 directive .cfi_adjust_cfa_offset
  112 1 int 8
  113 1 eol \n
  115 4 ident leaq
  120 2 int 16
  122 1 lparen (
  123 4 register %rsp
  127 1 rparen )
  128 1 comma ,
  130 4 register %rax
  134 1 eol \n
  136 4 ident movq
  141 4 register %rax
  145 1 comma ,
  147 1 int 0
  148 1 lparen (
  149 4 register %rsp
  153 1 rparen )
  154 1 eol \n
  156 4 ident movl
  161 1 immsigil $
  162 2 int 42
  164 1 comma ,
  166 4 register %eax
  170 1 eol \n
  172 4 ident addq
  177 1 immsigil $
  178 1 int 8
  179 1 comma ,
  181 4 register %rsp
  185 1 eol \n
  187 3 ident ret
  190 1 eol \n
  192 12 directive .cfi_endproc
  204 1 eol \n
  206 5 directive .type
  212 14 ident asm_test_entry
  226 1 comma ,
  228 1 at @
  229 8 ident function
  237 1 eol \n
  239 5 directive .size
  245 14 ident asm_test_entry
  259 1 comma ,
  261 1 dot .
  263 1 minus -
  265 14 ident asm_test_entry
  279 1 eol \n
  280 1 eol \n
  282 8 directive .section
  291 9 directive .note.GNU
  300 1 minus -
  301 5 ident stack
  306 1 comma ,
  307 2 string ""
  309 1 comma ,
  310 9 register %progbits
  319 1 eol \n
  320 0 eof 
  ########## x86_64 source ast
  source asm_test_entry
  directive .text
  directive .align [16]
  directive .globl [asm_test_entry]
  label asm_test_entry
  directive .cfi_startproc
  insn subq $8, %rsp
  directive .cfi_adjust_cfa_offset [8]
  insn leaq 16(%rsp), %rax
  insn movq %rax, (%rsp)
  insn movl $42, %eax
  insn addq $8, %rsp
  insn ret
  directive .cfi_endproc
  directive .type [asm_test_entry] [@function]
  directive .size [asm_test_entry] [. - asm_test_entry]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## x86_64 normalized ast
  normalized asm_test_entry
  section .text r-x
  align 16
  globl asm_test_entry
  label asm_test_entry
  subq $8, %rsp
  leaq 16(%rsp), %rax
  movq %rax, (%rsp)
  movl $42, %eax
  addq $8, %rsp
  ret
  type asm_test_entry function
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_64 lowered ast
  lowered asm_test_entry
  section .text r-x align=16
    align 16 fill=66 0f 1f 84 00 00 00 00 00 0f 1f 80 00 00 00 00
    label asm_test_entry
    bytes 48 83 ec 08              [x86_64.alu-rm-imm8.rex-present.reg]
    bytes 48 8d 44 24 10           [x86_64.lea.rex-present.sib-disp8]
    bytes 48 89 04 24              [x86_64.mov-rm-r.rex-present.sib-disp0]
    bytes b8 2a 00 00 00           [x86_64.mov-r-imm.rex-absent]
    bytes 48 83 c4 08              [x86_64.alu-rm-imm8.rex-present.reg]
    bytes c3                       [x86_64.ret]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## x86_64 plan
  segment .text size=23 zero=0 align=16 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## x86_64 image
  section .text address=0x0 size=23 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=23
  ########## x86_64 bytes
  48 83 ec 08 48 8d 44 24 10 48 89 04 24 b8 2a 00 00 00 48 83 c4 08 c3
  ########## x86_64 disasm canonical
  	subq $8, %rsp
  	leaq 16(%rsp), %rax
  	movq %rax, (%rsp)
  	movl $42, %eax
  	addq $8, %rsp
  	ret
  ########## x86_64 disasm diagnostic
  00000000  48 83 ec 08     subq $8, %rsp        [x86_64.alu-rm-imm8.rex-present.reg]
  00000004  48 8d 44 24 10  leaq 16(%rsp), %rax  [x86_64.lea.rex-present.sib-disp8]
  00000009  48 89 04 24     movq %rax, (%rsp)    [x86_64.mov-rm-r.rex-present.sib-disp0]
  0000000d  b8 2a 00 00 00  movl $42, %eax       [x86_64.mov-r-imm.rex-absent]
  00000012  48 83 c4 08     addq $8, %rsp        [x86_64.alu-rm-imm8.rex-present.reg]
  00000016  c3              ret                  [x86_64.ret]
  ########## x86_64 codec
  alt x86_64
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){rex 10000011 modrm imm:8s}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){rex 10000001 modrm le32}
    [2 cost=0] mov-r-imm        mov-r-imm(){rex 10111 reg:3u le32}
    [3 cost=0] mov-rm-r         mov-rm-r(){rex 10001001 modrm}
    [4 cost=0] mov-r-rm         mov-r-rm(){rex 10001011 modrm}
    [5 cost=0] lea              lea(){rex 10001101 modrm}
    [6 cost=0] ret              ret(){11000011}
    [7 cost=0] mov-rm-imm8      mov-rm-imm8(){rex 11000110 modrm imm8:8s}
    [8 cost=0] xor-rm-r         xor-rm-r(){rex 00110001 modrm}
    [9 cost=0] pop-r            pop-r(){rex 01011 reg:3u}
    [10 cost=0] jmp-rm           jmp-rm(){rex 11111111 modrm}
    [11 cost=0] ud2              ud2(){0000111100001011}
  alt rex
    [0 cost=0] rex-present      rex-present(){0100 wrxb:4u}
    [1 cost=0] rex-absent       rex-absent(){()}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib no-disp}
    [2 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp8:8s}
    [3 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib le32}
    [4 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u no-disp}
    [5 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp8:8s}
    [6 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u le32}
  le32(){imm:32u}
  sib(){scale:2u index:3u base:3u}
  no-disp(){()}
  ########## arm tokens
  1 7 directive .syntax
  9 7 ident unified
  16 1 eol \n
  18 5 directive .arch
  24 5 ident armv7
  29 1 minus -
  30 1 ident a
  31 1 eol \n
  33 4 directive .fpu
  38 5 ident vfpv3
  43 1 minus -
  44 3 ident d16
  47 1 eol \n
  49 15 directive .eabi_attribute
  65 16 ident Tag_ABI_VFP_args
  81 1 comma ,
  83 1 int 1
  84 1 eol \n
  86 4 directive .arm
  90 1 eol \n
  92 5 directive .text
  97 1 eol \n
  99 7 directive .balign
  107 1 int 4
  108 1 eol \n
  110 6 directive .globl
  117 14 ident asm_test_entry
  131 1 eol \n
  132 14 ident asm_test_entry
  146 1 colon :
  147 1 eol \n
  149 14 directive .cfi_startproc
  163 1 eol \n
  165 3 ident mov
  169 3 ident r12
  172 1 comma ,
  174 2 ident sp
  176 1 eol \n
  178 3 ident sub
  182 2 ident sp
  184 1 comma ,
  186 2 ident sp
  188 1 comma ,
  190 1 immsigil #
  191 1 int 8
  192 1 eol \n
  194 22 directive .cfi_adjust_cfa_offset
  217 1 int 8
  218 1 eol \n
  220 3 ident str
  224 3 ident r12
  227 1 comma ,
  229 1 lbracket [
  230 2 ident sp
  232 1 comma ,
  234 1 immsigil #
  235 1 int 0
  236 1 rbracket ]
  237 1 eol \n
  239 3 ident str
  243 2 ident lr
  245 1 comma ,
  247 1 lbracket [
  248 2 ident sp
  250 1 comma ,
  252 1 immsigil #
  253 1 int 4
  254 1 rbracket ]
  255 1 eol \n
  257 15 directive .cfi_rel_offset
  273 2 ident lr
  275 1 comma ,
  277 1 int 4
  278 1 eol \n
  280 3 ident mov
  284 2 ident r0
  286 1 comma ,
  288 1 immsigil #
  289 2 int 42
  291 1 eol \n
  293 3 ident ldr
  297 2 ident lr
  299 1 comma ,
  301 1 lbracket [
  302 2 ident sp
  304 1 comma ,
  306 1 immsigil #
  307 1 int 4
  308 1 rbracket ]
  309 1 eol \n
  311 3 ident add
  315 2 ident sp
  317 1 comma ,
  319 2 ident sp
  321 1 comma ,
  323 1 immsigil #
  324 1 int 8
  325 1 eol \n
  327 2 ident bx
  330 2 ident lr
  332 1 eol \n
  334 12 directive .cfi_endproc
  346 1 eol \n
  348 5 directive .type
  354 14 ident asm_test_entry
  368 1 comma ,
  370 1 percent %
  371 8 ident function
  379 1 eol \n
  381 5 directive .size
  387 14 ident asm_test_entry
  401 1 comma ,
  403 1 dot .
  405 1 minus -
  407 14 ident asm_test_entry
  421 1 eol \n
  422 1 eol \n
  424 8 directive .section
  433 9 directive .note.GNU
  442 1 minus -
  443 5 ident stack
  448 1 comma ,
  449 2 string ""
  451 1 comma ,
  452 1 percent %
  453 8 ident progbits
  461 1 eol \n
  462 0 eof 
  ########## arm source ast
  source asm_test_entry
  directive .syntax [unified]
  directive .arch [armv7-a]
  directive .fpu [vfpv3-d16]
  directive .eabi_attribute [Tag_ABI_VFP_args] [1]
  directive .arm
  directive .text
  directive .balign [4]
  directive .globl [asm_test_entry]
  label asm_test_entry
  directive .cfi_startproc
  insn mov ip, sp
  insn sub sp, sp, #8
  directive .cfi_adjust_cfa_offset [8]
  insn str ip, [sp]
  insn str lr, [sp, #4]
  directive .cfi_rel_offset [lr] [4]
  insn mov r0, #42
  insn ldr lr, [sp, #4]
  insn add sp, sp, #8
  insn bx lr
  directive .cfi_endproc
  directive .type [asm_test_entry] [%function]
  directive .size [asm_test_entry] [. - asm_test_entry]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## arm normalized ast
  normalized asm_test_entry
  section .text r-x
  align 4
  globl asm_test_entry
  label asm_test_entry
  mov ip, sp
  sub sp, sp, #8
  str ip, [sp]
  str lr, [sp, #4]
  mov r0, #42
  ldr lr, [sp, #4]
  add sp, sp, #8
  bx lr
  type asm_test_entry function
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## arm lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4 fill=00 f0 20 e3
    label asm_test_entry
    bytes 0d c0 a0 e1              [arm.dp-reg]
    bytes 08 d0 4d e2              [arm.dp-imm]
    bytes 00 c0 8d e5              [arm.ldst-imm]
    bytes 04 e0 8d e5              [arm.ldst-imm]
    bytes 2a 00 a0 e3              [arm.dp-imm]
    bytes 04 e0 9d e5              [arm.ldst-imm]
    bytes 08 d0 8d e2              [arm.dp-imm]
    bytes 1e ff 2f e1              [arm.bx]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## arm plan
  segment .text size=32 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## arm image
  section .text address=0x0 size=32 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=32
  ########## arm bytes
  0d c0 a0 e1 08 d0 4d e2 00 c0 8d e5 04 e0 8d e5 2a 00 a0 e3 04 e0 9d e5 08 d0 8d e2 1e ff 2f e1
  ########## arm disasm canonical
  	mov ip, sp
  	sub sp, sp, #8
  	str ip, [sp]
  	str lr, [sp, #4]
  	mov r0, #42
  	ldr lr, [sp, #4]
  	add sp, sp, #8
  	bx lr
  ########## arm disasm diagnostic
  00000000  0d c0 a0 e1  mov ip, sp        [arm.dp-reg]
  00000004  08 d0 4d e2  sub sp, sp, #8    [arm.dp-imm]
  00000008  00 c0 8d e5  str ip, [sp]      [arm.ldst-imm]
  0000000c  04 e0 8d e5  str lr, [sp, #4]  [arm.ldst-imm]
  00000010  2a 00 a0 e3  mov r0, #42       [arm.dp-imm]
  00000014  04 e0 9d e5  ldr lr, [sp, #4]  [arm.ldst-imm]
  00000018  08 d0 8d e2  add sp, sp, #8    [arm.dp-imm]
  0000001c  1e ff 2f e1  bx lr             [arm.bx]
  ########## arm codec
  alt arm
    [0 cost=0] bx               bx(){1110 000100101111111111110001 rm}
    [1 cost=0] dp-imm           dp-imm(){1110 001 dp:4u s:1u rn rd modimm}
    [2 cost=0] dp-reg           dp-reg(){1110 000 dp:4u s:1u rn rd shift-amount:5u shift-kind:2u 0 rm}
    [3 cost=0] ldst-imm         ldst-imm(){1110 010 1 u:1u b:1u 0 l:1u rn rt imm12:12u}
    [4 cost=0] udf              udf(){1110 01111111 imm12:12u 1111 imm4:4u}
  rm(){rm:4u}
  rn(){rn:4u}
  rd(){rd:4u}
  modimm(){rot:4u imm8:8u}
  rt(){rt:4u}
  ########## aarch64 tokens
  1 5 directive .text
  6 1 eol \n
  8 7 directive .balign
  16 1 int 4
  17 1 eol \n
  19 6 directive .globl
  26 14 ident asm_test_entry
  40 1 eol \n
  41 14 ident asm_test_entry
  55 1 colon :
  56 1 eol \n
  58 14 directive .cfi_startproc
  72 1 eol \n
  74 3 ident mov
  78 3 ident x15
  81 1 comma ,
  83 2 ident sp
  85 1 eol \n
  87 3 ident stp
  91 3 ident x15
  94 1 comma ,
  96 3 ident x30
  99 1 comma ,
  101 1 lbracket [
  102 2 ident sp
  104 1 comma ,
  106 1 immsigil #
  107 1 minus -
  108 2 int 16
  110 1 rbracket ]
  111 1 bang !
  112 1 eol \n
  114 22 directive .cfi_adjust_cfa_offset
  137 2 int 16
  139 1 eol \n
  141 15 directive .cfi_rel_offset
  157 3 ident x30
  160 1 comma ,
  162 1 int 8
  163 1 eol \n
  165 4 ident movz
  170 2 ident w0
  172 1 comma ,
  174 1 immsigil #
  175 2 int 42
  177 1 comma ,
  179 3 ident lsl
  183 1 immsigil #
  184 1 int 0
  185 1 eol \n
  187 3 ident ldr
  191 3 ident x30
  194 1 comma ,
  196 1 lbracket [
  197 2 ident sp
  199 1 comma ,
  201 1 immsigil #
  202 1 int 8
  203 1 rbracket ]
  204 1 eol \n
  206 3 ident add
  210 2 ident sp
  212 1 comma ,
  214 2 ident sp
  216 1 comma ,
  218 1 immsigil #
  219 2 int 16
  221 1 eol \n
  223 3 ident ret
  227 3 ident x30
  230 1 eol \n
  232 12 directive .cfi_endproc
  244 1 eol \n
  246 5 directive .type
  252 14 ident asm_test_entry
  266 1 comma ,
  268 1 at @
  269 8 ident function
  277 1 eol \n
  279 5 directive .size
  285 14 ident asm_test_entry
  299 1 comma ,
  301 1 dot .
  303 1 minus -
  305 14 ident asm_test_entry
  319 1 eol \n
  320 1 eol \n
  322 8 directive .section
  331 9 directive .note.GNU
  340 1 minus -
  341 5 ident stack
  346 1 comma ,
  347 2 string ""
  349 1 comma ,
  350 1 percent %
  351 8 ident progbits
  359 1 eol \n
  360 0 eof 
  ########## aarch64 source ast
  source asm_test_entry
  directive .text
  directive .balign [4]
  directive .globl [asm_test_entry]
  label asm_test_entry
  directive .cfi_startproc
  insn mov x15, sp
  insn stp x15, x30, [sp, #-16]!
  directive .cfi_adjust_cfa_offset [16]
  directive .cfi_rel_offset [x30] [8]
  insn movz w0, #42, lsl #0
  insn ldr x30, [sp, #8]
  insn add sp, sp, #16
  insn ret x30
  directive .cfi_endproc
  directive .type [asm_test_entry] [@function]
  directive .size [asm_test_entry] [. - asm_test_entry]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## aarch64 normalized ast
  normalized asm_test_entry
  section .text r-x
  align 4
  globl asm_test_entry
  label asm_test_entry
  mov x15, sp
  stp x15, x30, [sp, #-16]!
  movz w0, #42
  ldr x30, [sp, #8]
  add sp, sp, #16
  ret x30
  type asm_test_entry function
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## aarch64 lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4 fill=1f 20 03 d5
    label asm_test_entry
    bytes ef 03 00 91              [aarch64.add-imm]
    bytes ef 7b bf a9              [aarch64.stp-pre]
    bytes 40 05 80 52              [aarch64.movz]
    bytes fe 07 40 f9              [aarch64.ldr-uoff]
    bytes ff 43 00 91              [aarch64.add-imm]
    bytes c0 03 5f d6              [aarch64.ret]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## aarch64 plan
  segment .text size=24 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## aarch64 image
  section .text address=0x0 size=24 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=24
  ########## aarch64 bytes
  ef 03 00 91 ef 7b bf a9 40 05 80 52 fe 07 40 f9 ff 43 00 91 c0 03 5f d6
  ########## aarch64 disasm canonical
  	mov x15, sp
  	stp x15, x30, [sp, #-16]!
  	movz w0, #42
  	ldr x30, [sp, #8]
  	add sp, sp, #16
  	ret
  ########## aarch64 disasm diagnostic
  00000000  ef 03 00 91  mov x15, sp                [aarch64.add-imm]
  00000004  ef 7b bf a9  stp x15, x30, [sp, #-16]!  [aarch64.stp-pre]
  00000008  40 05 80 52  movz w0, #42               [aarch64.movz]
  0000000c  fe 07 40 f9  ldr x30, [sp, #8]          [aarch64.ldr-uoff]
  00000010  ff 43 00 91  add sp, sp, #16            [aarch64.add-imm]
  00000014  c0 03 5f d6  ret                        [aarch64.ret]
  ########## aarch64 codec
  alt aarch64
    [0 cost=0] add-imm          add-imm(){100100010 sh:1u imm12:12u rn rd}
    [1 cost=0] stp-pre          stp-pre(){1010100110 imm7-scaled8 rt2 rn rt}
    [2 cost=0] movz             movz(){sf:1u 10100101 hw:2u imm16:16u rd}
    [3 cost=0] ldr-uoff         ldr-uoff(){1111100101 imm12-scaled8 rn rt}
    [4 cost=0] ret              ret(){1101011001011111000000 rn 00000}
    [5 cost=0] sub-imm          sub-imm(){110100010 sh:1u imm12:12u rn rd}
    [6 cost=0] strb-uoff        strb-uoff(){0011100100 imm12:12u rn rt}
    [7 cost=0] udf              udf(){0000000000000000 imm16:16u}
  rn(){rn:5u}
  rd(){rd:5u}
  imm7-scaled8(){imm7:7s}
  rt2(){rt2:5u}
  rt(){rt:5u}
  imm12-scaled8(){imm12:12u}
