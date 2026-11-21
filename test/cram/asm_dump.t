  $ sh run_asm.sh
  ########## x86_32 return42 tokens
  1 5 directive .file
  7 18 string "asm_test_entry.c"
  25 1 eol \n
  27 5 directive .text
  32 1 eol \n
  34 6 directive .globl
  41 14 ident asm_test_entry
  55 1 eol \n
  57 5 directive .type
  63 14 ident asm_test_entry
  77 1 comma ,
  79 1 at @
  80 8 ident function
  88 1 eol \n
  89 14 ident asm_test_entry
  103 1 colon :
  104 1 eol \n
  106 4 ident movl
  111 1 immsigil $
  112 2 int 42
  114 1 comma ,
  116 4 register %eax
  120 1 eol \n
  122 3 ident ret
  125 1 eol \n
  127 5 directive .size
  133 14 ident asm_test_entry
  147 1 comma ,
  149 1 dot .
  150 1 minus -
  151 14 ident asm_test_entry
  165 1 eol \n
  167 6 directive .ident
  174 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  206 1 eol \n
  208 8 directive .section
  217 9 directive .note.GNU
  226 1 minus -
  227 5 ident stack
  232 1 comma ,
  233 2 string ""
  235 1 comma ,
  236 1 at @
  237 8 ident progbits
  245 1 eol \n
  246 0 eof 
  ########## x86_32 return42 source ast
  source asm_test_entry
  directive .file ["asm_test_entry.c"]
  directive .text
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  insn movl $42, %eax
  insn ret
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## x86_32 return42 normalized ast
  normalized asm_test_entry
  section .text r-x
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  movl $42, %eax
  ret
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_32 return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=1
    label asm_test_entry
    bytes b8 2a 00 00 00           [x86_32.mov-r-imm.opsz-absent]
    bytes c3                       [x86_32.ret]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## x86_32 return42 plan
  segment .text size=6 zero=0 align=1 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## x86_32 return42 image
  section .text address=0x0 size=6 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=6
  ########## x86_32 return42 bytes
  b8 2a 00 00 00 c3
  ########## x86_32 return42 disasm canonical
  	movl $42, %eax
  	ret
  ########## x86_32 return42 disasm diagnostic
  00000000  b8 2a 00 00 00  movl $42, %eax  [x86_32.mov-r-imm.opsz-absent]
  00000005  c3              ret             [x86_32.ret]
  ########## x86_32 return42 codec
  alt x86_32
    [-2 cost=0] alu-acc-imm8     alu-acc-imm8(){prefixes alu-acc-imm8-opcode imm:8s}
    [-1 cost=0] alu-acc-imm      alu-acc-imm(){prefixes alu-acc-imm-opcode imm-sym32}
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){prefixes 10000011 modrm imm}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){prefixes 10000001 modrm imm-sym32}
    [2 cost=0] mov-r-imm        mov-r-imm(){prefixes 10111 reg:3u imm-sym32}
    [3 cost=0] mov-eax-moffs    mov-eax-moffs(){10100001 disp-sym}
    [4 cost=0] mov-moffs-eax    mov-moffs-eax(){10100011 disp-sym}
    [5 cost=0] mov-rm-r         mov-rm-r(){prefixes 10001001 modrm}
    [6 cost=0] mov-r-rm         mov-r-rm(){prefixes 10001011 modrm}
    [7 cost=0] lea              lea(){prefixes 10001101 modrm}
    [8 cost=0] ret              ret(){11000011}
    [9 cost=0] mov-rm-imm8      mov-rm-imm8(){prefixes 11000110 modrm imm8:8s}
    [10 cost=0] alu-rm-r         alu-rm-r(){prefixes alu-rm-r-op modrm}
    [11 cost=0] pop-r            pop-r(){prefixes 01011 reg:3u}
    [12 cost=0] jmp-rm           jmp-rm(){prefixes 11111111 modrm}
    [13 cost=0] ud2              ud2(){0000111100001011}
    [14 cost=0] call-rel32       call-rel32(){11101000 le32}
    [15 cost=0] imul-r-rm        imul-r-rm(){prefixes 0000111110101111 modrm}
    [16 cost=0] cmov-r-rm        cmov-r-rm(){prefixes 000011110100 cc modrm}
    [17 cost=0] jmp-rel          relax jmp
                                   d8               jmp.d8(){11101011 <target:8@0 pcrel8-branch>}
                                   d32              jmp.d32(){11101001 le32}
    [18 cost=0] jcc-rel          relax jcc
                                   d8               jcc.d8(){0111 cc <target:8@0 pcrel8-branch>}
                                   d32              jcc.d32(){000011111000 cc le32}
    [19 cost=0] push-r           push-r(){prefixes 01010 reg:3u}
    [20 cost=0] unary-rm         unary-rm(){prefixes 11110111 modrm}
    [21 cost=0] alu-r-rm         alu-r-rm(){prefixes alu-r-rm-op modrm}
    [22 cost=0] shift1-rm        shift1-rm(){prefixes 11010001 modrm}
    [23 cost=0] dec-r            dec-r(){01001 reg:3u}
    [24 cost=0] sse-binop-f2     sse-binop-f2(){no-asz 11110010 no-rex 00001111 sse-binop-f2-op modrm}
    [25 cost=0] sse-binop-f3     sse-binop-f3(){no-asz 11110011 no-rex 00001111 sse-binop-f3-op modrm}
    [26 cost=0] sse-binop-66     sse-binop-66(){no-asz 01100110 no-rex 00001111 sse-binop-66-op modrm}
    [27 cost=0] sse-binop-none   sse-binop-none(){prefixes 00001111 sse-binop-none-op modrm}
    [28 cost=0] sse-movsd-load   sse-movsd-load(){no-asz 11110010 no-rex 0000111100010000 modrm}
    [29 cost=0] sse-movss-load   sse-movss-load(){no-asz 11110011 no-rex 0000111100010000 modrm}
    [30 cost=0] sse-movsd-store  sse-movsd-store(){no-asz 11110010 no-rex 0000111100010001 modrm}
    [31 cost=0] sse-movss-store  sse-movss-store(){no-asz 11110011 no-rex 0000111100010001 modrm}
    [32 cost=0] cvtsi2sd-r-rm    cvtsi2sd-r-rm(){no-asz 11110010 no-rex 0000111100101010 modrm}
    [33 cost=0] cvtsi2ss-r-rm    cvtsi2ss-r-rm(){no-asz 11110011 no-rex 0000111100101010 modrm}
    [34 cost=0] cvttsd2si-r-rm   cvttsd2si-r-rm(){no-asz 11110010 no-rex 0000111100101100 modrm}
    [35 cost=0] shift-imm-rm     shift-imm-rm(){prefixes 11000001 modrm imm8:8u}
    [36 cost=0] shift-cl-rm      shift-cl-rm(){prefixes 11010011 modrm}
    [37 cost=0] setcc-rm         setcc-rm(){prefixes 000011111001 cc modrm}
    [38 cost=0] movzx-b-r-rm     movzx-b-r-rm(){prefixes 0000111110110110 modrm}
    [39 cost=0] movzx-w-r-rm     movzx-w-r-rm(){prefixes 0000111110110111 modrm}
    [40 cost=0] movsx-b-r-rm     movsx-b-r-rm(){prefixes 0000111110111110 modrm}
    [41 cost=0] movsx-w-r-rm     movsx-w-r-rm(){prefixes 0000111110111111 modrm}
    [42 cost=0] movsxd-r-rm      movsxd-r-rm(){prefixes 01100011 modrm}
    [43 cost=0] imul-r-rm-imm8   imul-r-rm-imm8(){prefixes 01101011 modrm imm:8s}
    [44 cost=0] imul-r-rm-imm32  imul-r-rm-imm32(){prefixes 01101001 modrm le32}
    [45 cost=0] test-rm-imm      test-rm-imm(){prefixes 11110111 modrm le32}
    [46 cost=0] mov-rm-r8        mov-rm-r8(){prefixes 10001000 modrm}
    [47 cost=0] mov-r-rm8        mov-r-rm8(){prefixes 10001010 modrm}
    [48 cost=0] push-imm8        push-imm8(){01101010 imm}
    [49 cost=0] push-imm32       push-imm32(){01101000 imm-sym32}
    [50 cost=0] fldl             fldl(){prefixes 11011101 modrm}
    [51 cost=0] fstpl            fstpl(){prefixes 11011101 modrm}
    [52 cost=0] fstps            fstps(){prefixes 11011001 modrm}
    [53 cost=0] shld-imm-rm      shld-imm-rm(){prefixes 0000111110100100 modrm imm8:8u}
    [54 cost=0] flds             flds(){prefixes 11011001 modrm}
    [55 cost=0] fucomp           fucomp(){1101110111101001}
    [56 cost=0] fildll           fildll(){prefixes 11011111 modrm}
    [57 cost=0] fadds            fadds(){prefixes 11011000 modrm}
    [58 cost=0] fnstcw           fnstcw(){prefixes 11011001 modrm}
    [59 cost=0] fldcw            fldcw(){prefixes 11011001 modrm}
    [60 cost=0] fistpll          fistpll(){prefixes 11011111 modrm}
    [61 cost=0] fsubs            fsubs(){prefixes 11011000 modrm}
    [62 cost=0] fnstsw           fnstsw(){1101111111100000}
    [63 cost=0] sahf             sahf(){10011110}
    [64 cost=0] mov-r-imm8       mov-r-imm8(){prefixes 10110 reg:3u imm:8u}
    [65 cost=0] fadd-st0-x87     fadd-st0-x87(){11011000 11 000 st:3u}
    [66 cost=0] alu-rm-imm8-byte alu-rm-imm8-byte(){prefixes 10000000 modrm imm:8s}
    [67 cost=0] vex-scalar-f2-rrr vex-scalar-f2-rrr(){11000101 vex-byte2:8u vex-scalar-f2-op modrm}
    [68 cost=0] vex-scalar-f3-rrr vex-scalar-f3-rrr(){11000101 vex-byte2:8u vex-scalar-f3-op modrm}
    [69 cost=0] vex-scalar-none-rrr vex-scalar-none-rrr(){11000101 vex-byte2:8u vex-scalar-none-op modrm}
    [70 cost=0] vex-scalar-66-rrr vex-scalar-66-rrr(){11000101 vex-byte2:8u vex-scalar-66-op modrm}
    [71 cost=0] vex-unop-none    vex-unop-none(){11000101 vex-byte2:8u vex-unop-none-op modrm}
    [72 cost=0] vex-unop-66      vex-unop-66(){11000101 vex-byte2:8u vex-unop-66-op modrm}
    [73 cost=0] sse-binop-imm-f2 sse-binop-imm-f2(){no-asz 11110010 no-rex 00001111 sse-binop-imm-f2-op modrm imm8:8u}
    [74 cost=0] sse-binop-imm-f3 sse-binop-imm-f3(){no-asz 11110011 no-rex 00001111 sse-binop-imm-f3-op modrm imm8:8u}
    [75 cost=0] sse-binop-imm-66 sse-binop-imm-66(){no-asz 01100110 no-rex 00001111 sse-binop-imm-66-op modrm imm8:8u}
    [76 cost=0] sse-binop-imm-none sse-binop-imm-none(){prefixes 00001111 sse-binop-imm-none-op modrm imm8:8u}
    [77 cost=0] vex-binop-imm-f2 vex-binop-imm-f2(){11000101 vex-byte2:8u vex-binop-imm-f2-op modrm imm8:8u}
    [78 cost=0] vex-binop-imm-f3 vex-binop-imm-f3(){11000101 vex-byte2:8u vex-binop-imm-f3-op modrm imm8:8u}
    [79 cost=0] vex-binop-imm-none vex-binop-imm-none(){11000101 vex-byte2:8u vex-binop-imm-none-op modrm imm8:8u}
    [80 cost=0] vex-binop-imm-66 vex-binop-imm-66(){11000101 vex-byte2:8u vex-binop-imm-66-op modrm imm8:8u}
    [81 cost=0] vex-unop-imm-f2  vex-unop-imm-f2(){11000101 vex-byte2:8u vex-unop-imm-f2-op modrm imm8:8u}
    [82 cost=0] vex-unop-imm-f3  vex-unop-imm-f3(){11000101 vex-byte2:8u vex-unop-imm-f3-op modrm imm8:8u}
    [83 cost=0] vex-unop-imm-66  vex-unop-imm-66(){11000101 vex-byte2:8u vex-unop-imm-66-op modrm imm8:8u}
    [84 cost=0] movd-load-r-rm   movd-load-r-rm(){no-asz 01100110 no-rex 0000111101101110 modrm}
    [85 cost=0] movd-store-r-rm  movd-store-r-rm(){no-asz 01100110 no-rex 0000111101111110 modrm}
    [86 cost=0] vex-movd-load-r-rm vex-movd-load-r-rm(){11000101 vex-byte2:8u 01101110 modrm}
    [87 cost=0] vex-movd-store-r-rm vex-movd-store-r-rm(){11000101 vex-byte2:8u 01111110 modrm}
    [88 cost=0] movdqa-load      movdqa-load(){no-asz 01100110 no-rex 0000111101101111 modrm}
    [89 cost=0] movdqu-load      movdqu-load(){no-asz 11110011 no-rex 0000111101101111 modrm}
    [90 cost=0] movdqa-store     movdqa-store(){no-asz 01100110 no-rex 0000111101111111 modrm}
    [91 cost=0] movdqu-store     movdqu-store(){no-asz 11110011 no-rex 0000111101111111 modrm}
    [92 cost=0] vex-unop-f3      vex-unop-f3(){11000101 vex-byte2:8u vex-unop-f3-op modrm}
    [93 cost=0] sse-pinsrw       sse-pinsrw(){no-asz 01100110 no-rex 0000111111000100 modrm imm8:8u}
    [94 cost=0] sse-pextrw       sse-pextrw(){no-asz 01100110 no-rex 0000111111000101 modrm imm8:8u}
    [95 cost=0] vex-pinsrw       vex-pinsrw(){11000101 vex-byte2:8u 11000100 modrm imm8:8u}
    [96 cost=0] vex-pextrw       vex-pextrw(){11000101 vex-byte2:8u 11000101 modrm imm8:8u}
    [97 cost=0] sse-movmsk-66    sse-movmsk-66(){no-asz 01100110 no-rex 00001111 sse-movmsk-66-op modrm}
    [98 cost=0] sse-movmsk-none  sse-movmsk-none(){prefixes 00001111 sse-movmsk-none-op modrm}
    [99 cost=0] vex-movmsk-66    vex-movmsk-66(){11000101 vex-byte2:8u vex-movmsk-66-op modrm}
    [100 cost=0] vex-movmsk-none  vex-movmsk-none(){11000101 vex-byte2:8u vex-movmsk-none-op modrm}
    [101 cost=0] xmm-shift-imm-w  xmm-shift-imm-w(){no-asz 01100110 no-rex 00001111 01110001 modrm imm8:8u}
    [102 cost=0] xmm-shift-imm-d  xmm-shift-imm-d(){no-asz 01100110 no-rex 00001111 01110010 modrm imm8:8u}
    [103 cost=0] xmm-shift-imm-q  xmm-shift-imm-q(){no-asz 01100110 no-rex 00001111 01110011 modrm imm8:8u}
    [104 cost=0] vex-shift-imm-w  vex-shift-imm-w(){11000101 vex-byte2:8u 01110001 modrm imm8:8u}
    [105 cost=0] vex-shift-imm-d  vex-shift-imm-d(){11000101 vex-byte2:8u 01110010 modrm imm8:8u}
    [106 cost=0] vex-shift-imm-q  vex-shift-imm-q(){11000101 vex-byte2:8u 01110011 modrm imm8:8u}
    [107 cost=0] sse-binop-0f38-66 sse-binop-0f38-66(){no-asz 01100110 no-rex 00001111 00111000 sse-binop-0f38-op modrm}
    [108 cost=0] sse-binop-imm-0f3a-66 sse-binop-imm-0f3a-66(){no-asz 01100110 no-rex 00001111 00111010 sse-binop-imm-0f3a-op modrm imm8:8u}
    [109 cost=0] sse-gpr-imm-0f3a-66 sse-gpr-imm-0f3a-66(){no-asz 01100110 no-rex 00001111 00111010 sse-gpr-imm-0f3a-op modrm imm8:8u}
    [110 cost=0] vex3-map2-66-rrr vex3-map2-66-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-66-op modrm}
    [111 cost=0] vex3-map3-66-imm-rrr vex3-map3-66-imm-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map3-66-op modrm imm8:8u}
    [112 cost=0] vex3-map2-66-unop vex3-map2-66-unop(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-unop-op modrm}
    [113 cost=0] evex-ps-rrr      evex-ps-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-ps-op modrm}
    [114 cost=0] evex-pd-rrr      evex-pd-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-pd-op modrm}
    [200 cost=0] short-loop       relax loop
                                    d8               loop.d8(){11100010 <target:8@0 pcrel8-branch>}
    [201 cost=0] short-loope      relax loope
                                    d8               loope.d8(){11100001 <target:8@0 pcrel8-branch>}
    [202 cost=0] short-loopne     relax loopne
                                    d8               loopne.d8(){11100000 <target:8@0 pcrel8-branch>}
    [203 cost=0] short-jecxz      relax jecxz
                                    d8               jecxz.d8(){11100011 <target:8@0 pcrel8-branch>}
    [204 cost=0] short-jcxz       relax jcxz
                                    d8               jcxz.d8(){0110011111100011 <target:8@0 pcrel8-branch>}
    [300 cost=0] mov-r64-simm32   mov-r64-simm32(){prefixes 11000111 modrm le32}
    [301 cost=0] mov-r64-imm64    mov-r64-imm64(){prefixes 10111 reg:3u le64}
    [302 cost=0] bswap-r          bswap-r(){prefixes 00001111 11001 reg:3u}
  prefixes(){no-asz opsz no-rex}
  alu-acc-imm8-opcode(){opcode:8u}
  alu-acc-imm-opcode(){opcode:8u}
  imm-sym32(){le32}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-nobase-disp32 modrm-sib-nobase-disp32(){00 reg:3u 100 scale:2u index:3u 101 disp-sym}
    [2 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib disp-none}
    [3 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp-c8}
    [4 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib disp-c32}
    [5 cost=0] disp32-norm      modrm-disp32-norm(){00 reg:3u 101 disp-sym}
    [6 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u disp-none}
    [7 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp-c8}
    [8 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u disp-sym}
  imm(){imm:8s}
  disp-sym(){le32}
  alu-rm-r-op[9]{opcode:8u}
  le32(){<target:32@0 pcrel32-call>}
  cc[16]{cc:4u}
  alu-r-rm-op[8]{opcode:8u}
  no-asz(){()}
  no-rex(){()}
  sse-binop-f2-op[8]{opcode:8u}
  sse-binop-f3-op[9]{opcode:8u}
  sse-binop-66-op[66]{opcode:8u}
  sse-binop-none-op[19]{opcode:8u}
  vex-scalar-f2-op[7]{opcode:8u}
  vex-scalar-f3-op[7]{opcode:8u}
  vex-scalar-none-op[12]{opcode:8u}
  vex-scalar-66-op[58]{opcode:8u}
  vex-unop-none-op[7]{opcode:8u}
  vex-unop-66-op[8]{opcode:8u}
  sse-binop-imm-f2-op[2]{opcode:8u}
  sse-binop-imm-f3-op[2]{opcode:8u}
  sse-binop-imm-66-op[3]{opcode:8u}
  sse-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-f2-op[1]{opcode:8u}
  vex-binop-imm-f3-op[1]{opcode:8u}
  vex-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-66-op[2]{opcode:8u}
  vex-unop-imm-f2-op[1]{opcode:8u}
  vex-unop-imm-f3-op[1]{opcode:8u}
  vex-unop-imm-66-op[1]{opcode:8u}
  vex-unop-f3-op[2]{opcode:8u}
  sse-movmsk-66-op[2]{opcode:8u}
  sse-movmsk-none-op[1]{opcode:8u}
  vex-movmsk-66-op[2]{opcode:8u}
  vex-movmsk-none-op[1]{opcode:8u}
  sse-binop-0f38-op[46]{opcode:8u}
  sse-binop-imm-0f3a-op[12]{opcode:8u}
  sse-gpr-imm-0f3a-op[5]{opcode:8u}
  vex3-map2-66-op[25]{opcode:8u}
  vex3-map3-66-op[10]{opcode:8u}
  vex3-map2-unop-op[18]{opcode:8u}
  evex-ps-op[8]{opcode:8u}
  evex-pd-op[8]{opcode:8u}
  le64(){imm64:64u}
  alt opsz
    [0 cost=0] opsz-present     opsz-present(){01100110}
    [1 cost=0] opsz-absent      opsz-absent(){()}
  sib(){scale:2u index:3u base:3u}
  disp-none(){no-disp}
  disp-c8(){disp8:8s}
  disp-c32(){le32}
  no-disp(){()}
  ########## x86_64 return42 tokens
  1 5 directive .file
  7 18 string "asm_test_entry.c"
  25 1 eol \n
  27 5 directive .text
  32 1 eol \n
  34 6 directive .globl
  41 14 ident asm_test_entry
  55 1 eol \n
  57 5 directive .type
  63 14 ident asm_test_entry
  77 1 comma ,
  79 1 at @
  80 8 ident function
  88 1 eol \n
  89 14 ident asm_test_entry
  103 1 colon :
  104 1 eol \n
  106 4 ident movl
  111 1 immsigil $
  112 2 int 42
  114 1 comma ,
  116 4 register %eax
  120 1 eol \n
  122 3 ident ret
  125 1 eol \n
  127 5 directive .size
  133 14 ident asm_test_entry
  147 1 comma ,
  149 1 dot .
  150 1 minus -
  151 14 ident asm_test_entry
  165 1 eol \n
  167 6 directive .ident
  174 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  206 1 eol \n
  208 8 directive .section
  217 9 directive .note.GNU
  226 1 minus -
  227 5 ident stack
  232 1 comma ,
  233 2 string ""
  235 1 comma ,
  236 1 at @
  237 8 ident progbits
  245 1 eol \n
  246 0 eof 
  ########## x86_64 return42 source ast
  source asm_test_entry
  directive .file ["asm_test_entry.c"]
  directive .text
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  insn movl $42, %eax
  insn ret
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## x86_64 return42 normalized ast
  normalized asm_test_entry
  section .text r-x
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  movl $42, %eax
  ret
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_64 return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=1
    label asm_test_entry
    bytes b8 2a 00 00 00           [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
    bytes c3                       [x86_64.ret]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## x86_64 return42 plan
  segment .text size=6 zero=0 align=1 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## x86_64 return42 image
  section .text address=0x0 size=6 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=6
  ########## x86_64 return42 bytes
  b8 2a 00 00 00 c3
  ########## x86_64 return42 disasm canonical
  	movl $42, %eax
  	ret
  ########## x86_64 return42 disasm diagnostic
  00000000  b8 2a 00 00 00  movl $42, %eax  [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
  00000005  c3              ret             [x86_64.ret]
  ########## x86_64 return42 codec
  alt x86_64
    [-2 cost=0] alu-acc-imm8     alu-acc-imm8(){prefixes alu-acc-imm8-opcode imm:8s}
    [-1 cost=0] alu-acc-imm      alu-acc-imm(){prefixes alu-acc-imm-opcode imm-sym32}
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){prefixes 10000011 modrm imm}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){prefixes 10000001 modrm imm-sym32}
    [2 cost=0] mov-r-imm        mov-r-imm(){prefixes 10111 reg:3u imm-sym32}
    [5 cost=0] mov-rm-r         mov-rm-r(){prefixes 10001001 modrm}
    [6 cost=0] mov-r-rm         mov-r-rm(){prefixes 10001011 modrm}
    [7 cost=0] lea              lea(){prefixes 10001101 modrm}
    [8 cost=0] ret              ret(){11000011}
    [9 cost=0] mov-rm-imm8      mov-rm-imm8(){prefixes 11000110 modrm imm8:8s}
    [10 cost=0] alu-rm-r         alu-rm-r(){prefixes alu-rm-r-op modrm}
    [11 cost=0] pop-r            pop-r(){prefixes 01011 reg:3u}
    [12 cost=0] jmp-rm           jmp-rm(){prefixes 11111111 modrm}
    [13 cost=0] ud2              ud2(){0000111100001011}
    [14 cost=0] call-rel32       call-rel32(){11101000 le32}
    [15 cost=0] imul-r-rm        imul-r-rm(){prefixes 0000111110101111 modrm}
    [16 cost=0] cmov-r-rm        cmov-r-rm(){prefixes 000011110100 cc modrm}
    [17 cost=0] jmp-rel          relax jmp
                                   d8               jmp.d8(){11101011 <target:8@0 pcrel8-branch>}
                                   d32              jmp.d32(){11101001 le32}
    [18 cost=0] jcc-rel          relax jcc
                                   d8               jcc.d8(){0111 cc <target:8@0 pcrel8-branch>}
                                   d32              jcc.d32(){000011111000 cc le32}
    [19 cost=0] push-r           push-r(){prefixes 01010 reg:3u}
    [20 cost=0] unary-rm         unary-rm(){prefixes 11110111 modrm}
    [21 cost=0] alu-r-rm         alu-r-rm(){prefixes alu-r-rm-op modrm}
    [22 cost=0] shift1-rm        shift1-rm(){prefixes 11010001 modrm}
    [24 cost=0] sse-binop-f2     sse-binop-f2(){asz 11110010 rex 00001111 sse-binop-f2-op modrm}
    [25 cost=0] sse-binop-f3     sse-binop-f3(){asz 11110011 rex 00001111 sse-binop-f3-op modrm}
    [26 cost=0] sse-binop-66     sse-binop-66(){asz 01100110 rex 00001111 sse-binop-66-op modrm}
    [27 cost=0] sse-binop-none   sse-binop-none(){prefixes 00001111 sse-binop-none-op modrm}
    [28 cost=0] sse-movsd-load   sse-movsd-load(){asz 11110010 rex 0000111100010000 modrm}
    [29 cost=0] sse-movss-load   sse-movss-load(){asz 11110011 rex 0000111100010000 modrm}
    [30 cost=0] sse-movsd-store  sse-movsd-store(){asz 11110010 rex 0000111100010001 modrm}
    [31 cost=0] sse-movss-store  sse-movss-store(){asz 11110011 rex 0000111100010001 modrm}
    [32 cost=0] cvtsi2sd-r-rm    cvtsi2sd-r-rm(){asz 11110010 rex 0000111100101010 modrm}
    [33 cost=0] cvtsi2ss-r-rm    cvtsi2ss-r-rm(){asz 11110011 rex 0000111100101010 modrm}
    [34 cost=0] cvttsd2si-r-rm   cvttsd2si-r-rm(){asz 11110010 rex 0000111100101100 modrm}
    [35 cost=0] shift-imm-rm     shift-imm-rm(){prefixes 11000001 modrm imm8:8u}
    [36 cost=0] shift-cl-rm      shift-cl-rm(){prefixes 11010011 modrm}
    [37 cost=0] setcc-rm         setcc-rm(){prefixes 000011111001 cc modrm}
    [38 cost=0] movzx-b-r-rm     movzx-b-r-rm(){prefixes 0000111110110110 modrm}
    [39 cost=0] movzx-w-r-rm     movzx-w-r-rm(){prefixes 0000111110110111 modrm}
    [40 cost=0] movsx-b-r-rm     movsx-b-r-rm(){prefixes 0000111110111110 modrm}
    [41 cost=0] movsx-w-r-rm     movsx-w-r-rm(){prefixes 0000111110111111 modrm}
    [42 cost=0] movsxd-r-rm      movsxd-r-rm(){prefixes 01100011 modrm}
    [43 cost=0] imul-r-rm-imm8   imul-r-rm-imm8(){prefixes 01101011 modrm imm:8s}
    [44 cost=0] imul-r-rm-imm32  imul-r-rm-imm32(){prefixes 01101001 modrm le32}
    [45 cost=0] test-rm-imm      test-rm-imm(){prefixes 11110111 modrm le32}
    [46 cost=0] mov-rm-r8        mov-rm-r8(){prefixes 10001000 modrm}
    [47 cost=0] mov-r-rm8        mov-r-rm8(){prefixes 10001010 modrm}
    [48 cost=0] push-imm8        push-imm8(){01101010 imm}
    [49 cost=0] push-imm32       push-imm32(){01101000 imm-sym32}
    [50 cost=0] fldl             fldl(){prefixes 11011101 modrm}
    [51 cost=0] fstpl            fstpl(){prefixes 11011101 modrm}
    [52 cost=0] fstps            fstps(){prefixes 11011001 modrm}
    [53 cost=0] shld-imm-rm      shld-imm-rm(){prefixes 0000111110100100 modrm imm8:8u}
    [54 cost=0] flds             flds(){prefixes 11011001 modrm}
    [55 cost=0] fucomp           fucomp(){1101110111101001}
    [56 cost=0] fildll           fildll(){prefixes 11011111 modrm}
    [57 cost=0] fadds            fadds(){prefixes 11011000 modrm}
    [58 cost=0] fnstcw           fnstcw(){prefixes 11011001 modrm}
    [59 cost=0] fldcw            fldcw(){prefixes 11011001 modrm}
    [60 cost=0] fistpll          fistpll(){prefixes 11011111 modrm}
    [61 cost=0] fsubs            fsubs(){prefixes 11011000 modrm}
    [62 cost=0] fnstsw           fnstsw(){1101111111100000}
    [63 cost=0] sahf             sahf(){10011110}
    [64 cost=0] mov-r-imm8       mov-r-imm8(){prefixes 10110 reg:3u imm:8u}
    [65 cost=0] fadd-st0-x87     fadd-st0-x87(){11011000 11 000 st:3u}
    [66 cost=0] alu-rm-imm8-byte alu-rm-imm8-byte(){prefixes 10000000 modrm imm:8s}
    [67 cost=0] vex-scalar-f2-rrr vex-scalar-f2-rrr(){11000101 vex-byte2:8u vex-scalar-f2-op modrm}
    [68 cost=0] vex-scalar-f3-rrr vex-scalar-f3-rrr(){11000101 vex-byte2:8u vex-scalar-f3-op modrm}
    [69 cost=0] vex-scalar-none-rrr vex-scalar-none-rrr(){11000101 vex-byte2:8u vex-scalar-none-op modrm}
    [70 cost=0] vex-scalar-66-rrr vex-scalar-66-rrr(){11000101 vex-byte2:8u vex-scalar-66-op modrm}
    [71 cost=0] vex-unop-none    vex-unop-none(){11000101 vex-byte2:8u vex-unop-none-op modrm}
    [72 cost=0] vex-unop-66      vex-unop-66(){11000101 vex-byte2:8u vex-unop-66-op modrm}
    [73 cost=0] sse-binop-imm-f2 sse-binop-imm-f2(){asz 11110010 rex 00001111 sse-binop-imm-f2-op modrm imm8:8u}
    [74 cost=0] sse-binop-imm-f3 sse-binop-imm-f3(){asz 11110011 rex 00001111 sse-binop-imm-f3-op modrm imm8:8u}
    [75 cost=0] sse-binop-imm-66 sse-binop-imm-66(){asz 01100110 rex 00001111 sse-binop-imm-66-op modrm imm8:8u}
    [76 cost=0] sse-binop-imm-none sse-binop-imm-none(){prefixes 00001111 sse-binop-imm-none-op modrm imm8:8u}
    [77 cost=0] vex-binop-imm-f2 vex-binop-imm-f2(){11000101 vex-byte2:8u vex-binop-imm-f2-op modrm imm8:8u}
    [78 cost=0] vex-binop-imm-f3 vex-binop-imm-f3(){11000101 vex-byte2:8u vex-binop-imm-f3-op modrm imm8:8u}
    [79 cost=0] vex-binop-imm-none vex-binop-imm-none(){11000101 vex-byte2:8u vex-binop-imm-none-op modrm imm8:8u}
    [80 cost=0] vex-binop-imm-66 vex-binop-imm-66(){11000101 vex-byte2:8u vex-binop-imm-66-op modrm imm8:8u}
    [81 cost=0] vex-unop-imm-f2  vex-unop-imm-f2(){11000101 vex-byte2:8u vex-unop-imm-f2-op modrm imm8:8u}
    [82 cost=0] vex-unop-imm-f3  vex-unop-imm-f3(){11000101 vex-byte2:8u vex-unop-imm-f3-op modrm imm8:8u}
    [83 cost=0] vex-unop-imm-66  vex-unop-imm-66(){11000101 vex-byte2:8u vex-unop-imm-66-op modrm imm8:8u}
    [84 cost=0] movd-load-r-rm   movd-load-r-rm(){asz 01100110 rex 0000111101101110 modrm}
    [85 cost=0] movd-store-r-rm  movd-store-r-rm(){asz 01100110 rex 0000111101111110 modrm}
    [86 cost=0] vex-movd-load-r-rm vex-movd-load-r-rm(){11000101 vex-byte2:8u 01101110 modrm}
    [87 cost=0] vex-movd-store-r-rm vex-movd-store-r-rm(){11000101 vex-byte2:8u 01111110 modrm}
    [88 cost=0] movdqa-load      movdqa-load(){asz 01100110 rex 0000111101101111 modrm}
    [89 cost=0] movdqu-load      movdqu-load(){asz 11110011 rex 0000111101101111 modrm}
    [90 cost=0] movdqa-store     movdqa-store(){asz 01100110 rex 0000111101111111 modrm}
    [91 cost=0] movdqu-store     movdqu-store(){asz 11110011 rex 0000111101111111 modrm}
    [92 cost=0] vex-unop-f3      vex-unop-f3(){11000101 vex-byte2:8u vex-unop-f3-op modrm}
    [93 cost=0] sse-pinsrw       sse-pinsrw(){asz 01100110 rex 0000111111000100 modrm imm8:8u}
    [94 cost=0] sse-pextrw       sse-pextrw(){asz 01100110 rex 0000111111000101 modrm imm8:8u}
    [95 cost=0] vex-pinsrw       vex-pinsrw(){11000101 vex-byte2:8u 11000100 modrm imm8:8u}
    [96 cost=0] vex-pextrw       vex-pextrw(){11000101 vex-byte2:8u 11000101 modrm imm8:8u}
    [97 cost=0] sse-movmsk-66    sse-movmsk-66(){asz 01100110 rex 00001111 sse-movmsk-66-op modrm}
    [98 cost=0] sse-movmsk-none  sse-movmsk-none(){prefixes 00001111 sse-movmsk-none-op modrm}
    [99 cost=0] vex-movmsk-66    vex-movmsk-66(){11000101 vex-byte2:8u vex-movmsk-66-op modrm}
    [100 cost=0] vex-movmsk-none  vex-movmsk-none(){11000101 vex-byte2:8u vex-movmsk-none-op modrm}
    [101 cost=0] xmm-shift-imm-w  xmm-shift-imm-w(){asz 01100110 rex 00001111 01110001 modrm imm8:8u}
    [102 cost=0] xmm-shift-imm-d  xmm-shift-imm-d(){asz 01100110 rex 00001111 01110010 modrm imm8:8u}
    [103 cost=0] xmm-shift-imm-q  xmm-shift-imm-q(){asz 01100110 rex 00001111 01110011 modrm imm8:8u}
    [104 cost=0] vex-shift-imm-w  vex-shift-imm-w(){11000101 vex-byte2:8u 01110001 modrm imm8:8u}
    [105 cost=0] vex-shift-imm-d  vex-shift-imm-d(){11000101 vex-byte2:8u 01110010 modrm imm8:8u}
    [106 cost=0] vex-shift-imm-q  vex-shift-imm-q(){11000101 vex-byte2:8u 01110011 modrm imm8:8u}
    [107 cost=0] sse-binop-0f38-66 sse-binop-0f38-66(){asz 01100110 rex 00001111 00111000 sse-binop-0f38-op modrm}
    [108 cost=0] sse-binop-imm-0f3a-66 sse-binop-imm-0f3a-66(){asz 01100110 rex 00001111 00111010 sse-binop-imm-0f3a-op modrm imm8:8u}
    [109 cost=0] sse-gpr-imm-0f3a-66 sse-gpr-imm-0f3a-66(){asz 01100110 rex 00001111 00111010 sse-gpr-imm-0f3a-op modrm imm8:8u}
    [110 cost=0] vex3-map2-66-rrr vex3-map2-66-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-66-op modrm}
    [111 cost=0] vex3-map3-66-imm-rrr vex3-map3-66-imm-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map3-66-op modrm imm8:8u}
    [112 cost=0] vex3-map2-66-unop vex3-map2-66-unop(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-unop-op modrm}
    [113 cost=0] evex-ps-rrr      evex-ps-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-ps-op modrm}
    [114 cost=0] evex-pd-rrr      evex-pd-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-pd-op modrm}
    [200 cost=0] short-loop       relax loop
                                    d8               loop.d8(){11100010 <target:8@0 pcrel8-branch>}
    [201 cost=0] short-loope      relax loope
                                    d8               loope.d8(){11100001 <target:8@0 pcrel8-branch>}
    [202 cost=0] short-loopne     relax loopne
                                    d8               loopne.d8(){11100000 <target:8@0 pcrel8-branch>}
    [203 cost=0] short-jrcxz      relax jrcxz
                                    d8               jrcxz.d8(){11100011 <target:8@0 pcrel8-branch>}
    [204 cost=0] short-jecxz      relax jecxz
                                    d8               jecxz.d8(){0110011111100011 <target:8@0 pcrel8-branch>}
    [300 cost=0] mov-r64-simm32   mov-r64-simm32(){prefixes 11000111 modrm le32}
    [301 cost=0] mov-r64-imm64    mov-r64-imm64(){prefixes 10111 reg:3u le64}
    [302 cost=0] bswap-r          bswap-r(){prefixes 00001111 11001 reg:3u}
  prefixes(){asz opsz rex}
  alu-acc-imm8-opcode(){opcode:8u}
  alu-acc-imm-opcode(){opcode:8u}
  imm-sym32(){le32}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-nobase-disp32 modrm-sib-nobase-disp32(){00 reg:3u 100 scale:2u index:3u 101 disp-sym}
    [2 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib disp-none}
    [3 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp-c8}
    [4 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib disp-c32}
    [5 cost=0] disp32-norm      modrm-disp32-norm(){00 reg:3u 101 disp-sym}
    [6 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u disp-none}
    [7 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp-c8}
    [8 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u disp-sym}
  imm(){imm:8s}
  alu-rm-r-op[9]{opcode:8u}
  le32(){<target:32@0 pcrel32-call>}
  cc[16]{cc:4u}
  alu-r-rm-op[8]{opcode:8u}
  alt asz
    [0 cost=0] asz-present      asz-present(){01100111}
    [1 cost=0] asz-absent       asz-absent(){()}
  alt rex
    [0 cost=0] rex-present      rex-present(){0100 wrxb:4u}
    [1 cost=0] rex-absent       rex-absent(){()}
  sse-binop-f2-op[8]{opcode:8u}
  sse-binop-f3-op[9]{opcode:8u}
  sse-binop-66-op[66]{opcode:8u}
  sse-binop-none-op[19]{opcode:8u}
  vex-scalar-f2-op[7]{opcode:8u}
  vex-scalar-f3-op[7]{opcode:8u}
  vex-scalar-none-op[12]{opcode:8u}
  vex-scalar-66-op[58]{opcode:8u}
  vex-unop-none-op[7]{opcode:8u}
  vex-unop-66-op[8]{opcode:8u}
  sse-binop-imm-f2-op[2]{opcode:8u}
  sse-binop-imm-f3-op[2]{opcode:8u}
  sse-binop-imm-66-op[3]{opcode:8u}
  sse-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-f2-op[1]{opcode:8u}
  vex-binop-imm-f3-op[1]{opcode:8u}
  vex-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-66-op[2]{opcode:8u}
  vex-unop-imm-f2-op[1]{opcode:8u}
  vex-unop-imm-f3-op[1]{opcode:8u}
  vex-unop-imm-66-op[1]{opcode:8u}
  vex-unop-f3-op[2]{opcode:8u}
  sse-movmsk-66-op[2]{opcode:8u}
  sse-movmsk-none-op[1]{opcode:8u}
  vex-movmsk-66-op[2]{opcode:8u}
  vex-movmsk-none-op[1]{opcode:8u}
  sse-binop-0f38-op[46]{opcode:8u}
  sse-binop-imm-0f3a-op[12]{opcode:8u}
  sse-gpr-imm-0f3a-op[5]{opcode:8u}
  vex3-map2-66-op[25]{opcode:8u}
  vex3-map3-66-op[10]{opcode:8u}
  vex3-map2-unop-op[18]{opcode:8u}
  evex-ps-op[8]{opcode:8u}
  evex-pd-op[8]{opcode:8u}
  le64(){imm64:64u}
  alt opsz
    [0 cost=0] opsz-present     opsz-present(){01100110}
    [1 cost=0] opsz-absent      opsz-absent(){()}
  disp-sym(){le32}
  sib(){scale:2u index:3u base:3u}
  disp-none(){no-disp}
  disp-c8(){disp8:8s}
  disp-c32(){le32}
  no-disp(){()}
  ########## arm return42 tokens
  1 5 directive .arch
  7 5 ident armv7
  12 1 minus -
  13 1 ident a
  14 1 eol \n
  16 4 directive .fpu
  21 5 ident vfpv3
  26 1 minus -
  27 3 ident d16
  30 1 eol \n
  32 15 directive .eabi_attribute
  48 2 int 28
  50 1 comma ,
  52 1 int 1
  53 1 eol \n
  55 15 directive .eabi_attribute
  71 2 int 20
  73 1 comma ,
  75 1 int 1
  76 1 eol \n
  78 15 directive .eabi_attribute
  94 2 int 21
  96 1 comma ,
  98 1 int 1
  99 1 eol \n
  101 15 directive .eabi_attribute
  117 2 int 23
  119 1 comma ,
  121 1 int 3
  122 1 eol \n
  124 15 directive .eabi_attribute
  140 2 int 24
  142 1 comma ,
  144 1 int 1
  145 1 eol \n
  147 15 directive .eabi_attribute
  163 2 int 25
  165 1 comma ,
  167 1 int 1
  168 1 eol \n
  170 15 directive .eabi_attribute
  186 2 int 26
  188 1 comma ,
  190 1 int 2
  191 1 eol \n
  193 15 directive .eabi_attribute
  209 2 int 30
  211 1 comma ,
  213 1 int 1
  214 1 eol \n
  216 15 directive .eabi_attribute
  232 2 int 34
  234 1 comma ,
  236 1 int 1
  237 1 eol \n
  239 15 directive .eabi_attribute
  255 2 int 18
  257 1 comma ,
  259 1 int 4
  260 1 eol \n
  262 5 directive .file
  268 18 string "asm_test_entry.c"
  286 1 eol \n
  288 5 directive .text
  293 1 eol \n
  295 6 directive .align
  302 1 int 2
  303 1 eol \n
  305 7 directive .global
  313 14 ident asm_test_entry
  327 1 eol \n
  329 7 directive .syntax
  337 7 ident unified
  344 1 eol \n
  346 4 directive .arm
  350 1 eol \n
  352 5 directive .type
  358 14 ident asm_test_entry
  372 1 comma ,
  374 1 percent %
  375 8 ident function
  383 1 eol \n
  384 14 ident asm_test_entry
  398 1 colon :
  399 1 eol \n
  435 1 eol \n
  480 1 eol \n
  514 1 eol \n
  516 3 ident mov
  520 2 ident r0
  522 1 comma ,
  524 1 immsigil #
  525 2 int 42
  527 1 eol \n
  529 2 ident bx
  532 2 ident lr
  534 1 eol \n
  536 5 directive .size
  542 14 ident asm_test_entry
  556 1 comma ,
  558 1 dot .
  559 1 minus -
  560 14 ident asm_test_entry
  574 1 eol \n
  576 6 directive .ident
  583 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  615 1 eol \n
  617 8 directive .section
  626 9 directive .note.GNU
  635 1 minus -
  636 5 ident stack
  641 1 comma ,
  642 2 string ""
  644 1 comma ,
  645 1 percent %
  646 8 ident progbits
  654 1 eol \n
  655 0 eof 
  ########## arm return42 source ast
  source asm_test_entry
  directive .arch [armv7-a]
  directive .fpu [vfpv3-d16]
  directive .eabi_attribute [28] [1]
  directive .eabi_attribute [20] [1]
  directive .eabi_attribute [21] [1]
  directive .eabi_attribute [23] [3]
  directive .eabi_attribute [24] [1]
  directive .eabi_attribute [25] [1]
  directive .eabi_attribute [26] [2]
  directive .eabi_attribute [30] [1]
  directive .eabi_attribute [34] [1]
  directive .eabi_attribute [18] [4]
  directive .file ["asm_test_entry.c"]
  directive .text
  directive .align [2]
  directive .global [asm_test_entry]
  directive .syntax [unified]
  directive .arm
  directive .type [asm_test_entry] [%function]
  label asm_test_entry
  insn mov r0, #42
  insn bx lr
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## arm return42 normalized ast
  normalized asm_test_entry
  target-state .arch armv7-a
  target-state .fpu vfpv3-d16
  target-state .eabi_attribute 28, 1
  target-state .eabi_attribute 20, 1
  target-state .eabi_attribute 21, 1
  target-state .eabi_attribute 23, 3
  target-state .eabi_attribute 24, 1
  target-state .eabi_attribute 25, 1
  target-state .eabi_attribute 26, 2
  target-state .eabi_attribute 30, 1
  target-state .eabi_attribute 34, 1
  target-state .eabi_attribute 18, 4
  section .text r-x
  align 4
  globl asm_test_entry
  target-state .syntax unified
  target-state .arm 
  type asm_test_entry function
  label asm_test_entry
  mov r0, #42
  bx lr
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## arm return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4
    label asm_test_entry
    bytes 2a 00 a0 e3              [arm.dp-imm]
    bytes 1e ff 2f e1              [arm.bx]
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  declared .note.GNU-stack (not allocated)
  ########## arm return42 plan
  segment .text size=8 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## arm return42 image
  section .text address=0x0 size=8 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=8
  ########## arm return42 bytes
  2a 00 a0 e3 1e ff 2f e1
  ########## arm return42 disasm canonical
  	mov r0, #42
  	bx lr
  ########## arm return42 disasm diagnostic
  00000000  2a 00 a0 e3  mov r0, #42  [arm.dp-imm]
  00000004  1e ff 2f e1  bx lr        [arm.bx]
  ########## arm return42 codec
  alt arm
    [-1 cost=0] nop              nop(){cond 0011001000001111000000000000}
    [0 cost=0] bx               bx(){cond 000100101111111111110001 rm}
    [1 cost=0] movw             movw(){cond 00110000 <imm:4@12 movw-abs-nc> rd <imm:12@0 movw-abs-nc>}
    [2 cost=0] movt             movt(){cond 00110100 <imm:4@12 movt-abs> rd <imm:12@0 movt-abs>}
    [3 cost=0] dp-imm           dp-imm(){cond 001 dp:4u s:1u rn rd modimm}
    [4 cost=0] dp-reg           dp-reg(){cond 000 dp:4u s:1u rn rd shift-amount:5u shift-kind:2u 0 rm}
    [5 cost=0] ldst-imm         ldst-imm(){cond 010 p:1u u:1u b:1u w:1u l:1u rn rt imm12:12u}
    [6 cost=0] ldst-reg         ldst-reg(){cond 011 1 u:1u b:1u 0 l:1u rn rt shift-amount:5u shift-kind:2u 0 rm}
    [7 cost=0] udf              udf(){1110 01111111 imm12:12u 1111 imm4:4u}
    [8 cost=0] bl               bl(){cond 1011 <target:24@0 pcrel-call>}
    [9 cost=0] b                b(){cond 1010 <target:24@0 pcrel-b26>}
    [10 cost=0] mul              mul(){cond 0000000 s:1u rd 0000 rm 1001 rn}
    [11 cost=0] mla              mla(){cond 0000001 s:1u rd ra rm 1001 rn}
    [12 cost=0] vmov-reg-d       vmov-reg-d(){cond 11101 D:1u 11 0000 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [13 cost=0] vmov-reg-s       vmov-reg-s(){cond 11101 D:1u 11 0000 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [14 cost=0] vmov-imm-d       vmov-imm-d(){cond 11101 D:1u 11 imm4H:4u Vd:4u 1011 0000 imm4L:4u}
    [15 cost=0] vmov-imm-s       vmov-imm-s(){cond 11101 D:1u 11 imm4H:4u Vd:4u 1010 0000 imm4L:4u}
    [16 cost=0] vmov-to-single   vmov-to-single(){cond 1110000 0 Vn:4u rt 1010 N:1u 0010000}
    [17 cost=0] vmov-from-single vmov-from-single(){cond 1110000 1 Vn:4u rt 1010 N:1u 0010000}
    [18 cost=0] vmov-to-double   vmov-to-double(){cond 1100010 0 rt2 rt 1011 00 D:1u 1 Vm:4u}
    [19 cost=0] vmov-from-double vmov-from-double(){cond 1100010 1 rt2 rt 1011 00 D:1u 1 Vm:4u}
    [20 cost=0] v3-d             v3-d(){cond 1110 b23:1u D:1u b21:1u b20:1u Vn:4u Vd:4u 1011 N:1u b6:1u M:1u 0 Vm:4u}
    [21 cost=0] v3-s             v3-s(){cond 1110 b23:1u D:1u b21:1u b20:1u Vn:4u Vd:4u 1010 N:1u b6:1u M:1u 0 Vm:4u}
    [22 cost=0] vneg-d           vneg-d(){cond 11101 D:1u 11 0001 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [23 cost=0] vneg-s           vneg-s(){cond 11101 D:1u 11 0001 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [24 cost=0] vcmp-reg-d       vcmp-reg-d(){cond 11101 D:1u 11 0100 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [25 cost=0] vcmp-reg-s       vcmp-reg-s(){cond 11101 D:1u 11 0100 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [26 cost=0] vcmp-zero-d      vcmp-zero-d(){cond 11101 D:1u 11 0101 Vd:4u 1011 01 000000}
    [27 cost=0] vcmp-zero-s      vcmp-zero-s(){cond 11101 D:1u 11 0101 Vd:4u 1010 01 000000}
    [28 cost=0] vmrs             vmrs(){cond 1110111100011111101000010000}
    [29 cost=0] vcvt-f32-f64     vcvt-f32-f64(){cond 11101 D:1u 11 0111 Vd:4u 1011 11 M:1u 0 Vm:4u}
    [30 cost=0] vcvt-f64-f32     vcvt-f64-f32(){cond 11101 D:1u 11 0111 Vd:4u 1010 11 M:1u 0 Vm:4u}
    [31 cost=0] vcvt-f32-s32     vcvt-f32-s32(){cond 11101 D:1u 11 1000 Vd:4u 1010 1 1 M:1u 0 Vm:4u}
    [32 cost=0] vcvt-f64-s32     vcvt-f64-s32(){cond 11101 D:1u 11 1000 Vd:4u 1011 1 1 M:1u 0 Vm:4u}
    [33 cost=0] vcvt-f64-u32     vcvt-f64-u32(){cond 11101 D:1u 11 1000 Vd:4u 1011 0 1 M:1u 0 Vm:4u}
    [34 cost=0] vcvt-s32-f64     vcvt-s32-f64(){cond 11101 D:1u 11 1101 Vd:4u 1011 11 M:1u 0 Vm:4u}
    [35 cost=0] vmem-d           vmem-d(){cond 1101 U:1u D:1u 0 L:1u rn Vd:4u 1011 imm8:8u}
    [36 cost=0] vmem-s           vmem-s(){cond 1101 U:1u D:1u 0 L:1u rn Vd:4u 1010 imm8:8u}
    [37 cost=0] push             push(){cond 100100101101 reglist:16u}
    [38 cost=0] ldm              ldm(){cond 1000101 l:1u rn reglist:16u}
    [39 cost=0] vpush            vpush(){cond 11010 D:1u 10 1101 Vd:4u 1011 imm8:8u}
    [40 cost=0] shift-reg        shift-reg(){cond 000 1101 0 0000 rd rs 0 shift-kind:2u 1 rm}
    [41 cost=0] vldr-lit-d       vldr-lit-d(){cond 1101 1 D:1u 0 1 1111 Vd:4u 1011 <target:8@0 pcrel-vldr8>}
    [42 cost=0] vldr-lit-s       vldr-lit-s(){cond 1101 1 D:1u 0 1 1111 Vd:4u 1010 <target:8@0 pcrel-vldr8>}
    [43 cost=0] umull            umull(){cond 00001 00 s:1u rdhi rdlo rm 1001 rn}
    [44 cost=0] ldst-x-imm       ldst-x-imm(){cond 000 p:1u u:1u 1 0 l:1u rn rt imm4h:4u 1 s:1u h:1u 1 imm4l:4u}
    [45 cost=0] ldst-x-reg       ldst-x-reg(){cond 000 p:1u u:1u 0 0 l:1u rn rt 0000 1 s:1u h:1u 1 rm}
    [46 cost=0] sbfx             sbfx(){cond 0111101 widthm1:5u rd lsb:5u 101 rn}
    [47 cost=0] clz              clz(){cond 00010110 1111 rd 1111 0001 rm}
    [48 cost=0] rev              rev(){cond 01101011 1111 rd 1111 0011 rm}
    [49 cost=0] rev16            rev16(){cond 01101011 1111 rd 1111 1011 rm}
  cond[16]{cond:4u}
  rm(){rm:4u}
  rd(){rd:4u}
  rn(){rn:4u}
  modimm(){rot:4u imm8:8u}
  rt(){rt:4u}
  ra(){ra:4u}
  rt2(){rt2:4u}
  rs(){rs:4u}
  rdhi(){rdhi:4u}
  rdlo(){rdlo:4u}
  ########## aarch64 return42 tokens
  1 5 directive .arch
  7 5 ident armv8
  12 1 minus -
  13 1 ident a
  14 1 eol \n
  16 5 directive .file
  22 18 string "asm_test_entry.c"
  40 1 eol \n
  42 5 directive .text
  47 1 eol \n
  49 6 directive .align
  56 1 int 2
  57 1 eol \n
  59 7 directive .global
  67 14 ident asm_test_entry
  81 1 eol \n
  83 5 directive .type
  89 14 ident asm_test_entry
  103 1 comma ,
  105 1 percent %
  106 8 ident function
  114 1 eol \n
  115 14 ident asm_test_entry
  129 1 colon :
  130 1 eol \n
  131 5 directive .LFB0
  136 1 colon :
  137 1 eol \n
  139 14 directive .cfi_startproc
  153 1 eol \n
  155 3 ident mov
  159 2 ident w0
  161 1 comma ,
  163 2 int 42
  165 1 eol \n
  167 3 ident ret
  170 1 eol \n
  172 12 directive .cfi_endproc
  184 1 eol \n
  185 5 directive .LFE0
  190 1 colon :
  191 1 eol \n
  193 5 directive .size
  199 14 ident asm_test_entry
  213 1 comma ,
  215 1 dot .
  216 1 minus -
  217 14 ident asm_test_entry
  231 1 eol \n
  233 6 directive .ident
  240 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  272 1 eol \n
  274 8 directive .section
  283 9 directive .note.GNU
  292 1 minus -
  293 5 ident stack
  298 1 comma ,
  299 2 string ""
  301 1 comma ,
  302 1 at @
  303 8 ident progbits
  311 1 eol \n
  312 0 eof 
  ########## aarch64 return42 source ast
  source asm_test_entry
  directive .arch [armv8-a]
  directive .file ["asm_test_entry.c"]
  directive .text
  directive .align [2]
  directive .global [asm_test_entry]
  directive .type [asm_test_entry] [%function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn mov w0, 42
  insn ret
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## aarch64 return42 normalized ast
  normalized asm_test_entry
  target-state .arch armv8-a
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  mov w0, #42
  ret
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## aarch64 return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes 40 05 80 52              [aarch64.movz]
    bytes c0 03 5f d6              [aarch64.ret]
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## aarch64 return42 plan
  segment .text size=8 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## aarch64 return42 image
  section .text address=0x0 size=8 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=8
  ########## aarch64 return42 bytes
  40 05 80 52 c0 03 5f d6
  ########## aarch64 return42 disasm canonical
  	movz w0, #42
  	ret
  ########## aarch64 return42 disasm diagnostic
  00000000  40 05 80 52  movz w0, #42  [aarch64.movz]
  00000004  c0 03 5f d6  ret           [aarch64.ret]
  ########## aarch64 return42 codec
  alt aarch64
    [0 cost=0] add-imm          add-imm(){sf:1u 00100010 sh:1u <imm:12@0 add-lo12> rn rd}
    [1 cost=0] sub-imm          sub-imm(){sf:1u 10100010 sh:1u imm12:12u rn rd}
    [2 cost=0] addsub-shift     addsub-shift(){sf:1u op:1u s:1u 01011 shift:2u 0 rm imm6:6u rn rd}
    [3 cost=0] logical-imm-32   logical-imm-32(){0 opc:2u 100100 bitmask32 rn rd}
    [4 cost=0] logical-imm-64   logical-imm-64(){1 opc:2u 100100 bitmask64 rn rd}
    [5 cost=0] madd             madd(){sf:1u 0011011000 rm 0 ra rn rd}
    [6 cost=0] csel             csel(){sf:1u 0011010100 rm cond 00 rn rd}
    [7 cost=0] stp-pre          stp-pre(){1010100110 imm7-scaled8 rt2 rn rt}
    [8 cost=0] movz             movz(){sf:1u 10100101 hw:2u imm16:16u rd}
    [9 cost=0] ldr8             ldr8(){00 111001 01 offset-scaled1 rn rt}
    [10 cost=0] str8             str8(){00 111001 00 offset-scaled1 rn rt}
    [11 cost=0] ldr16            ldr16(){01 111001 01 offset-scaled2 rn rt}
    [12 cost=0] str16            str16(){01 111001 00 offset-scaled2 rn rt}
    [13 cost=0] ldr32            ldr32(){10 111001 01 offset-scaled4 rn rt}
    [14 cost=0] str32            str32(){10 111001 00 offset-scaled4 rn rt}
    [15 cost=0] ldr64            ldr64(){11 111001 01 offset-scaled8 rn rt}
    [16 cost=0] str64            str64(){11 111001 00 offset-scaled8 rn rt}
    [17 cost=0] ret              ret(){1101011001011111000000 rn 00000}
    [18 cost=0] udf              udf(){0000000000000000 imm16:16u}
    [19 cost=0] adrp             adrp(){1 <page:2@0 adrp-page> 10000 <page:19@2 adrp-page> rd}
    [20 cost=0] b                b(){000101 <target:26@0 pcrel-b26>}
    [21 cost=0] b-cond           b-cond(){01010100 <target:19@0 pcrel-b19> 0 cond}
    [22 cost=0] bl               bl(){100101 <target:26@0 pcrel-call26>}
    [23 cost=0] subs-imm         subs-imm(){sf:1u 11100010 sh:1u imm12:12u rn rd}
    [24 cost=0] ldr8-roff        ldr8-roff(){00 111000 01 1 rm option:3u s:1u 10 rn rt}
    [25 cost=0] str8-roff        str8-roff(){00 111000 00 1 rm option:3u s:1u 10 rn rt}
    [26 cost=0] ldr16-roff       ldr16-roff(){01 111000 01 1 rm option:3u s:1u 10 rn rt}
    [27 cost=0] str16-roff       str16-roff(){01 111000 00 1 rm option:3u s:1u 10 rn rt}
    [28 cost=0] ldr32-roff       ldr32-roff(){10 111000 01 1 rm option:3u s:1u 10 rn rt}
    [29 cost=0] str32-roff       str32-roff(){10 111000 00 1 rm option:3u s:1u 10 rn rt}
    [30 cost=0] ldr64-roff       ldr64-roff(){11 111000 01 1 rm option:3u s:1u 10 rn rt}
    [31 cost=0] str64-roff       str64-roff(){11 111000 00 1 rm option:3u s:1u 10 rn rt}
    [32 cost=0] fmov-imm-d       fmov-imm-d(){000 11110 01 1 fpimm8-d 10000000 rd:5u}
    [33 cost=0] fmov-imm-s       fmov-imm-s(){000 11110 00 1 fpimm8-s 10000000 rd:5u}
    [34 cost=0] fcmp-imm0-d      fcmp-imm0-d(){000 11110 01 1 00000001000 rn:5u 01000}
    [35 cost=0] fcmp-imm0-s      fcmp-imm0-s(){000 11110 00 1 00000001000 rn:5u 01000}
    [36 cost=0] addsub-extend    addsub-extend(){sf:1u op:1u s:1u 01011 00 1 rm option:3u imm3:3u rn rd}
    [37 cost=0] movn             movn(){sf:1u 00100101 hw:2u imm16:16u rd}
    [38 cost=0] cbz              cbz(){sf:1u 011010 op:1u <target:19@0 pcrel-b19> rt}
    [39 cost=0] logical-shift    logical-shift(){sf:1u opc:2u 01010 shift:2u N:1u rm imm6:6u rn rd}
    [40 cost=0] sdiv             sdiv(){sf:1u 0011010110 rm 000011 rn rd}
    [41 cost=0] sxtw             sxtw(){100100110 1 000000 011111 rn rd}
    [42 cost=0] msub             msub(){sf:1u 0011011000 rm 1 ra rn rd}
    [43 cost=0] udiv             udiv(){sf:1u 0011010110 rm 000010 rn rd}
    [44 cost=0] cset             cset(){sf:1u 0011010100 rm cond 01 rn rd}
    [45 cost=0] movk             movk(){sf:1u 11100101 hw:2u imm16:16u rd}
    [46 cost=0] lslv             lslv(){sf:1u 0011010110 rm 001000 rn rd}
    [47 cost=0] ubfm             ubfm(){sf:1u 10100110 N:1u immr:6u imms:6u rn rd}
    [48 cost=0] adr              adr(){0 <offset:2@0 pcrel-adr21> 10000 <offset:19@2 pcrel-adr21> rd}
    [49 cost=0] br               br(){1101011000011111000000 rn 00000}
    [50 cost=0] fcmp-reg-d       fcmp-reg-d(){000 11110 01 1 rm:5u 00 1000 rn:5u 00000}
    [51 cost=0] fcmp-reg-s       fcmp-reg-s(){000 11110 00 1 rm:5u 00 1000 rn:5u 00000}
    [52 cost=0] fmul-d           fbinop-fmul-d(){000 11110 01 1 rm:5u 0000 10 rn:5u rd:5u}
    [53 cost=0] fmul-s           fbinop-fmul-s(){000 11110 00 1 rm:5u 0000 10 rn:5u rd:5u}
    [54 cost=0] fdiv-d           fbinop-fdiv-d(){000 11110 01 1 rm:5u 0001 10 rn:5u rd:5u}
    [55 cost=0] fdiv-s           fbinop-fdiv-s(){000 11110 00 1 rm:5u 0001 10 rn:5u rd:5u}
    [56 cost=0] fadd-d           fbinop-fadd-d(){000 11110 01 1 rm:5u 0010 10 rn:5u rd:5u}
    [57 cost=0] fadd-s           fbinop-fadd-s(){000 11110 00 1 rm:5u 0010 10 rn:5u rd:5u}
    [58 cost=0] fsub-d           fbinop-fsub-d(){000 11110 01 1 rm:5u 0011 10 rn:5u rd:5u}
    [59 cost=0] fsub-s           fbinop-fsub-s(){000 11110 00 1 rm:5u 0011 10 rn:5u rd:5u}
    [60 cost=0] fcsel-d          fcsel-d(){000 11110 01 1 rm:5u cond 11 rn:5u rd:5u}
    [61 cost=0] fcsel-s          fcsel-s(){000 11110 00 1 rm:5u cond 11 rn:5u rd:5u}
    [62 cost=0] scvtf-d          cvtf-s-d(){sf:1u 0011110 01 100010000000 rn:5u rd:5u}
    [63 cost=0] scvtf-s          cvtf-s-s(){sf:1u 0011110 00 100010000000 rn:5u rd:5u}
    [64 cost=0] ucvtf-d          cvtf-u-d(){sf:1u 0011110 01 100011000000 rn:5u rd:5u}
    [65 cost=0] ucvtf-s          cvtf-u-s(){sf:1u 0011110 00 100011000000 rn:5u rd:5u}
    [66 cost=0] fmov-from-gpr-d  fmov-from-gpr-d(){sf:1u 0011110 01 100111000000 rn:5u rd:5u}
    [67 cost=0] fmov-from-gpr-s  fmov-from-gpr-s(){sf:1u 0011110 00 100111000000 rn:5u rd:5u}
    [68 cost=0] fcvtzs-d         fcvtzs-d(){sf:1u 0011110 01 111000000000 rn:5u rd:5u}
    [69 cost=0] fcvtzs-s         fcvtzs-s(){sf:1u 0011110 00 111000000000 rn:5u rd:5u}
    [70 cost=0] fneg-d           fneg-d(){00011110 01 100001010000 rn:5u rd:5u}
    [71 cost=0] fneg-s           fneg-s(){00011110 00 100001010000 rn:5u rd:5u}
    [72 cost=0] fcvt-d-to-s      fcvt-d-to-s(){00011110 01 100010010000 rn:5u rd:5u}
    [73 cost=0] fcvt-s-to-d      fcvt-s-to-d(){00011110 00 100010110000 rn:5u rd:5u}
    [74 cost=0] ldr32-f          ldr32-f(){10 111101 01 offset-scaled4 rn rt:5u}
    [75 cost=0] ldr64-f          ldr64-f(){11 111101 01 offset-scaled8 rn rt:5u}
    [76 cost=0] str32-f          str32-f(){10 111101 00 offset-scaled4 rn rt:5u}
    [77 cost=0] str64-f          str64-f(){11 111101 00 offset-scaled8 rn rt:5u}
    [78 cost=0] ldr32-f-roff     ldr32-f-roff(){10 111100 01 1 rm option:3u s:1u 10 rn rt:5u}
    [79 cost=0] ldr64-f-roff     ldr64-f-roff(){11 111100 01 1 rm option:3u s:1u 10 rn rt:5u}
    [80 cost=0] str32-f-roff     str32-f-roff(){10 111100 00 1 rm option:3u s:1u 10 rn rt:5u}
    [81 cost=0] str64-f-roff     str64-f-roff(){11 111100 00 1 rm option:3u s:1u 10 rn rt:5u}
    [82 cost=0] fmov-reg-d       fmov-reg-d(){00011110 01 100000010000 rn:5u rd:5u}
    [83 cost=0] fmov-reg-s       fmov-reg-s(){00011110 00 100000010000 rn:5u rd:5u}
    [84 cost=0] ldp64-post       ldp64-post(){1010100011 imm7-scaled8 rt2 rn rt}
    [85 cost=0] ldp64-off        ldp64-off(){1010100101 imm7-scaled8 rt2 rn rt}
    [86 cost=0] stp64-post       stp64-post(){1010100010 imm7-scaled8 rt2 rn rt}
    [87 cost=0] stp64-off        stp64-off(){1010100100 imm7-scaled8 rt2 rn rt}
    [88 cost=0] ldp32-post       ldp32-post(){0010100011 imm7-scaled4 rt2 rn rt}
    [89 cost=0] ldp32-off        ldp32-off(){0010100101 imm7-scaled4 rt2 rn rt}
    [90 cost=0] stp32-post       stp32-post(){0010100010 imm7-scaled4 rt2 rn rt}
    [91 cost=0] stp32-off        stp32-off(){0010100100 imm7-scaled4 rt2 rn rt}
    [92 cost=0] adds-imm         adds-imm(){sf:1u 01100010 sh:1u imm12:12u rn rd}
    [93 cost=0] ldr8-post        ldr8-post(){00 111000 01 0 imm9-scaled1 01 rn rt}
    [94 cost=0] str8-post        str8-post(){00 111000 00 0 imm9-scaled1 01 rn rt}
    [95 cost=0] ldr16-post       ldr16-post(){01 111000 01 0 imm9-scaled1 01 rn rt}
    [96 cost=0] str16-post       str16-post(){01 111000 00 0 imm9-scaled1 01 rn rt}
    [97 cost=0] ldr32-post       ldr32-post(){10 111000 01 0 imm9-scaled1 01 rn rt}
    [98 cost=0] str32-post       str32-post(){10 111000 00 0 imm9-scaled1 01 rn rt}
    [99 cost=0] ldr64-post       ldr64-post(){11 111000 01 0 imm9-scaled1 01 rn rt}
    [100 cost=0] str64-post       str64-post(){11 111000 00 0 imm9-scaled1 01 rn rt}
    [101 cost=0] sbfm             sbfm(){sf:1u 00100110 N:1u immr:6u imms:6u rn rd}
    [102 cost=0] lsrv             lsrv(){sf:1u 0011010110 rm 001001 rn rd}
    [103 cost=0] asrv             asrv(){sf:1u 0011010110 rm 001010 rn rd}
    [104 cost=0] rorv             rorv(){sf:1u 0011010110 rm 001011 rn rd}
    [105 cost=0] fcvtzu-d         fcvtzu-d(){sf:1u 0011110 01 111001000000 rn:5u rd:5u}
    [106 cost=0] fcvtzu-s         fcvtzu-s(){sf:1u 0011110 00 111001000000 rn:5u rd:5u}
    [110 cost=0] ldrsb-w          ldrsb-w(){00 111001 11 offset-scaled1 rn rt}
    [111 cost=0] ldrsb-w-roff     ldrsb-w-roff(){00 111000 11 1 rm option:3u s:1u 10 rn rt}
    [112 cost=0] ldrsb-x          ldrsb-x(){00 111001 10 offset-scaled1 rn rt}
    [113 cost=0] ldrsb-x-roff     ldrsb-x-roff(){00 111000 10 1 rm option:3u s:1u 10 rn rt}
    [114 cost=0] ldrsh-w          ldrsh-w(){01 111001 11 offset-scaled2 rn rt}
    [115 cost=0] ldrsh-w-roff     ldrsh-w-roff(){01 111000 11 1 rm option:3u s:1u 10 rn rt}
    [116 cost=0] ldrsh-x          ldrsh-x(){01 111001 10 offset-scaled2 rn rt}
    [117 cost=0] ldrsh-x-roff     ldrsh-x-roff(){01 111000 10 1 rm option:3u s:1u 10 rn rt}
    [118 cost=0] ldrsw-x          ldrsw-x(){10 111001 10 offset-scaled4 rn rt}
    [119 cost=0] ldrsw-x-roff     ldrsw-x-roff(){10 111000 10 1 rm option:3u s:1u 10 rn rt}
    [130 cost=0] rbit-w           rbit-w(){0 101101011000000000000 rn rd}
    [131 cost=0] rbit-x           rbit-x(){1 101101011000000000000 rn rd}
    [132 cost=0] rev16-w          rev16-w(){0 101101011000000000001 rn rd}
    [133 cost=0] rev16-x          rev16-x(){1 101101011000000000001 rn rd}
    [134 cost=0] rev32-x          rev32-x(){1 101101011000000000010 rn rd}
    [135 cost=0] rev-w            rev-w(){0 101101011000000000010 rn rd}
    [136 cost=0] rev-x            rev-x(){1 101101011000000000011 rn rd}
    [137 cost=0] clz-w            clz-w(){0 101101011000000000100 rn rd}
    [138 cost=0] clz-x            clz-x(){1 101101011000000000100 rn rd}
    [139 cost=0] cls-w            cls-w(){0 101101011000000000101 rn rd}
    [140 cost=0] cls-x            cls-x(){1 101101011000000000101 rn rd}
    [141 cost=0] mrs-fpcr         mrs-fpcr(){110101010011101101000100000 rt}
    [142 cost=0] msr-fpcr         msr-fpcr(){110101010001101101000100000 rt}
    [143 cost=0] fsqrt-d          fsqrt-d(){00011110 01 100001110000 rn:5u rd:5u}
    [144 cost=0] fsqrt-s          fsqrt-s(){00011110 00 100001110000 rn:5u rd:5u}
    [145 cost=0] frintz-d         frintz-d(){00011110 01 100101110000 rn:5u rd:5u}
    [146 cost=0] frintz-s         frintz-s(){00011110 00 100101110000 rn:5u rd:5u}
    [147 cost=0] fmadd-d          fmadd-d(){00011111 01 0 rm:5u 0 ra:5u rn:5u rd:5u}
    [148 cost=0] fmadd-s          fmadd-s(){00011111 00 0 rm:5u 0 ra:5u rn:5u rd:5u}
    [149 cost=0] fmov-to-gpr-d    fmov-to-gpr-d(){sf:1u 0011110 01 100110000000 rn:5u rd:5u}
    [150 cost=0] fmov-to-gpr-s    fmov-to-gpr-s(){sf:1u 0011110 00 100110000000 rn:5u rd:5u}
    [151 cost=0] fmax-d           fbinop-fmax-d(){000 11110 01 1 rm:5u 0100 10 rn:5u rd:5u}
    [152 cost=0] fmax-s           fbinop-fmax-s(){000 11110 00 1 rm:5u 0100 10 rn:5u rd:5u}
    [153 cost=0] vand-16b         vand-16b(){01001110001 rm:5u 000111 rn:5u rd:5u}
    [154 cost=0] vand-8b          vand-8b(){00001110001 rm:5u 000111 rn:5u rd:5u}
    [155 cost=0] vfcvtl           vfcvtl(){0000111001100001011110 rn:5u rd:5u}
    [156 cost=0] vfcvtn           vfcvtn(){0000111001100001011010 rn:5u rd:5u}
    [157 cost=0] ldr-q            ldr-q(){0011110111 imm12:12u rn rt:5u}
    [158 cost=0] str-q            str-q(){0011110110 imm12:12u rn rt:5u}
    [159 cost=0] vfadd-s2         vfadd-2s(){00001110001 rm:5u 110101 rn:5u rd:5u}
    [160 cost=0] vfadd-s4         vfadd-4s(){01001110001 rm:5u 110101 rn:5u rd:5u}
    [161 cost=0] vfadd-d2         vfadd-2d(){01001110011 rm:5u 110101 rn:5u rd:5u}
    [162 cost=0] vfsub-s2         vfsub-2s(){00001110101 rm:5u 110101 rn:5u rd:5u}
    [163 cost=0] vfsub-s4         vfsub-4s(){01001110101 rm:5u 110101 rn:5u rd:5u}
    [164 cost=0] vfsub-d2         vfsub-2d(){01001110111 rm:5u 110101 rn:5u rd:5u}
    [165 cost=0] vfmul-s2         vfmul-2s(){00101110001 rm:5u 110111 rn:5u rd:5u}
    [166 cost=0] vfmul-s4         vfmul-4s(){01101110001 rm:5u 110111 rn:5u rd:5u}
    [167 cost=0] vfmul-d2         vfmul-2d(){01101110011 rm:5u 110111 rn:5u rd:5u}
    [168 cost=0] vfdiv-s2         vfdiv-2s(){00101110001 rm:5u 111111 rn:5u rd:5u}
    [169 cost=0] vfdiv-s4         vfdiv-4s(){01101110001 rm:5u 111111 rn:5u rd:5u}
    [170 cost=0] vfdiv-d2         vfdiv-2d(){01101110011 rm:5u 111111 rn:5u rd:5u}
    [171 cost=0] vfmax-s2         vfmax-2s(){00001110001 rm:5u 111101 rn:5u rd:5u}
    [172 cost=0] vfmax-s4         vfmax-4s(){01001110001 rm:5u 111101 rn:5u rd:5u}
    [173 cost=0] vfmax-d2         vfmax-2d(){01001110011 rm:5u 111101 rn:5u rd:5u}
    [174 cost=0] vfmla-s2         vfmla-2s(){00001110001 rm:5u 110011 rn:5u rd:5u}
    [175 cost=0] vfmla-s4         vfmla-4s(){01001110001 rm:5u 110011 rn:5u rd:5u}
    [176 cost=0] vfmla-d2         vfmla-2d(){01001110011 rm:5u 110011 rn:5u rd:5u}
    [177 cost=0] vfneg-s2         vfneg-2s(){0010111010100000111110 rn:5u rd:5u}
    [178 cost=0] vfneg-s4         vfneg-4s(){0110111010100000111110 rn:5u rd:5u}
    [179 cost=0] vfneg-d2         vfneg-2d(){0110111011100000111110 rn:5u rd:5u}
    [180 cost=0] vfsqrt-s2        vfsqrt-2s(){0010111010100001111110 rn:5u rd:5u}
    [181 cost=0] vfsqrt-s4        vfsqrt-4s(){0110111010100001111110 rn:5u rd:5u}
    [182 cost=0] vfsqrt-d2        vfsqrt-2d(){0110111011100001111110 rn:5u rd:5u}
    [183 cost=0] vfrintz-s2       vfrintz-2s(){0000111010100001100110 rn:5u rd:5u}
    [184 cost=0] vfrintz-s4       vfrintz-4s(){0100111010100001100110 rn:5u rd:5u}
    [185 cost=0] vfrintz-d2       vfrintz-2d(){0100111011100001100110 rn:5u rd:5u}
    [191 cost=0] vld1-s           vld1-s(){0 q:1u 00110101 000000100 s:1u 00 rn rt:5u}
    [192 cost=0] vst1-s           vst1-s(){0 q:1u 00110100 000000100 s:1u 00 rn rt:5u}
    [195 cost=0] vld1-d           vld1-d(){0 q:1u 00110101 000000100 s:1u 01 rn rt:5u}
    [196 cost=0] vst1-d           vst1-d(){0 q:1u 00110100 000000100 s:1u 01 rn rt:5u}
    [197 cost=0] vld1r-s2         vld1r-2s(){00 00110101 0000001100 10 rn rt:5u}
    [198 cost=0] vld1r-s4         vld1r-4s(){01 00110101 0000001100 10 rn rt:5u}
    [199 cost=0] vld1r-d2         vld1r-2d(){01 00110101 0000001100 11 rn rt:5u}
    [200 cost=0] vdup-q           vdup-q(){01001110000 imm5:5u 000001 rn:5u rd:5u}
    [201 cost=0] vdup-d           vdup-d(){00001110000 imm5:5u 000001 rn:5u rd:5u}
    [202 cost=0] vdup-scalar      vdup-scalar(){01011110000 imm5:5u 000001 rn:5u rd:5u}
    [203 cost=0] vins             vins(){01101110000 imm5:5u 0 imm4:4u 1 rn:5u rd:5u}
    [204 cost=0] vorr-16b         vorr-16b(){01001110101 rm:5u 000111 rn:5u rd:5u}
    [205 cost=0] vorr-8b          vorr-8b(){00001110101 rm:5u 000111 rn:5u rd:5u}
    [206 cost=0] veor-16b         veor-16b(){01101110001 rm:5u 000111 rn:5u rd:5u}
    [207 cost=0] veor-8b          veor-8b(){00101110001 rm:5u 000111 rn:5u rd:5u}
    [208 cost=0] vbsl-16b         vbsl-16b(){01101110011 rm:5u 000111 rn:5u rd:5u}
    [209 cost=0] vbsl-8b          vbsl-8b(){00101110011 rm:5u 000111 rn:5u rd:5u}
    [210 cost=0] vnot-16b         vnot-16b(){0110111000100000010110 rn:5u rd:5u}
    [211 cost=0] vnot-8b          vnot-8b(){0010111000100000010110 rn:5u rd:5u}
    [212 cost=0] vfcmeq-s2        vfcmeq-2s(){00001110001 rm:5u 111001 rn:5u rd:5u}
    [213 cost=0] vfcmeq-s4        vfcmeq-4s(){01001110001 rm:5u 111001 rn:5u rd:5u}
    [214 cost=0] vfcmeq-d2        vfcmeq-2d(){01001110011 rm:5u 111001 rn:5u rd:5u}
    [215 cost=0] vfcmge-s2        vfcmge-2s(){00101110001 rm:5u 111001 rn:5u rd:5u}
    [216 cost=0] vfcmge-s4        vfcmge-4s(){01101110001 rm:5u 111001 rn:5u rd:5u}
    [217 cost=0] vfcmge-d2        vfcmge-2d(){01101110011 rm:5u 111001 rn:5u rd:5u}
    [218 cost=0] vfcmgt-s2        vfcmgt-2s(){00101110101 rm:5u 111001 rn:5u rd:5u}
    [219 cost=0] vfcmgt-s4        vfcmgt-4s(){01101110101 rm:5u 111001 rn:5u rd:5u}
    [220 cost=0] vfcmgt-d2        vfcmgt-2d(){01101110111 rm:5u 111001 rn:5u rd:5u}
    [221 cost=0] vbit-16b         vbit-16b(){01101110101 rm:5u 000111 rn:5u rd:5u}
    [222 cost=0] vbit-8b          vbit-8b(){00101110101 rm:5u 000111 rn:5u rd:5u}
    [223 cost=0] vdup-gpr-q       vdup-gpr-q(){01001110000 imm5:5u 000011 rn rd:5u}
    [224 cost=0] vdup-gpr-d       vdup-gpr-d(){00001110000 imm5:5u 000011 rn rd:5u}
    [225 cost=0] mrs-nzcv         mrs-nzcv(){110101010011101101000010000 rt}
    [226 cost=0] msr-nzcv         msr-nzcv(){110101010001101101000010000 rt}
  rn(){rn:5u}
  rd(){rd:5u}
  rm(){rm:5u}
  bitmask32(){n:1u immr:6u imms:6u}
  bitmask64(){n:1u immr:6u imms:6u}
  ra(){ra:5u}
  cond[16]{cond:4u}
  imm7-scaled8(){imm7:7s}
  rt2(){rt2:5u}
  rt(){rt:5u}
  offset-scaled1(){<offset:12@0 ldst8-lo12>}
  offset-scaled2(){<offset:12@0 ldst16-lo12>}
  offset-scaled4(){<offset:12@0 ldst32-lo12>}
  offset-scaled8(){<offset:12@0 ldst64-lo12>}
  fpimm8-d(){imm8:8u}
  fpimm8-s(){imm8:8u}
  imm7-scaled4(){imm7:7s}
  imm9-scaled1(){imm9:9s}
  ########## riscv32 return42 tokens
  1 5 directive .file
  7 18 string "asm_test_entry.c"
  25 1 eol \n
  27 7 directive .option
  35 5 ident nopic
  40 1 eol \n
  42 7 directive .option
  50 7 ident norelax
  57 1 eol \n
  59 10 directive .attribute
  70 4 ident arch
  74 1 comma ,
  76 39 string "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  115 1 eol \n
  117 10 directive .attribute
  128 16 ident unaligned_access
  144 1 comma ,
  146 1 int 0
  147 1 eol \n
  149 10 directive .attribute
  160 11 ident stack_align
  171 1 comma ,
  173 2 int 16
  175 1 eol \n
  177 5 directive .text
  182 1 eol \n
  184 6 directive .align
  191 1 int 2
  192 1 eol \n
  194 6 directive .globl
  201 14 ident asm_test_entry
  215 1 eol \n
  217 5 directive .type
  223 14 ident asm_test_entry
  237 1 comma ,
  239 1 at @
  240 8 ident function
  248 1 eol \n
  249 14 ident asm_test_entry
  263 1 colon :
  264 1 eol \n
  265 5 directive .LFB0
  270 1 colon :
  271 1 eol \n
  273 14 directive .cfi_startproc
  287 1 eol \n
  289 2 ident li
  292 2 ident a0
  294 1 comma ,
  295 2 int 42
  297 1 eol \n
  299 3 ident ret
  302 1 eol \n
  304 12 directive .cfi_endproc
  316 1 eol \n
  317 5 directive .LFE0
  322 1 colon :
  323 1 eol \n
  325 5 directive .size
  331 14 ident asm_test_entry
  345 1 comma ,
  347 1 dot .
  348 1 minus -
  349 14 ident asm_test_entry
  363 1 eol \n
  365 6 directive .ident
  372 35 string "GCC: (crosstool-NG 1.27.0) 14.2.0"
  407 1 eol \n
  409 8 directive .section
  418 9 directive .note.GNU
  427 1 minus -
  428 5 ident stack
  433 1 comma ,
  434 2 string ""
  436 1 comma ,
  437 1 at @
  438 8 ident progbits
  446 1 eol \n
  447 0 eof 
  ########## riscv32 return42 source ast
  source asm_test_entry
  directive .file ["asm_test_entry.c"]
  directive .option [nopic]
  directive .option [norelax]
  directive .attribute [arch] ["rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"]
  directive .attribute [unaligned_access] [0]
  directive .attribute [stack_align] [16]
  directive .text
  directive .align [2]
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn li x10, 42
  insn ret
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (crosstool-NG 1.27.0) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## riscv32 return42 normalized ast
  normalized asm_test_entry
  target-state .option nopic
  target-state .option norelax
  target-state .attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  target-state .attribute unaligned_access, 0
  target-state .attribute stack_align, 16
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  li x10, 42
  ret
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## riscv32 return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes 13 05 a0 02              [riscv32.addi]
    bytes 67 80 00 00              [riscv32.jalr]
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## riscv32 return42 plan
  segment .text size=8 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## riscv32 return42 image
  section .text address=0x0 size=8 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=8
  ########## riscv32 return42 bytes
  13 05 a0 02 67 80 00 00
  ########## riscv32 return42 disasm canonical
  	addi x10, x0, 42
  	jalr x0, 0(x1)
  ########## riscv32 return42 disasm diagnostic
  00000000  13 05 a0 02  addi x10, x0, 42  [riscv32.addi]
  00000004  67 80 00 00  jalr x0, 0(x1)    [riscv32.jalr]
  ########## riscv32 return42 codec
  alt riscv32
    [0 cost=0] pair             riscv32-pair(){auipc-i-pair:64u}
    [1 cost=0] word             riscv32-word(){instruction:32u}
    [2 cost=0] compressed       riscv32-compressed(){compressed-instruction:16u}
  ########## riscv64 return42 tokens
  1 5 directive .file
  7 18 string "asm_test_entry.c"
  25 1 eol \n
  27 7 directive .option
  35 5 ident nopic
  40 1 eol \n
  42 7 directive .option
  50 7 ident norelax
  57 1 eol \n
  59 10 directive .attribute
  70 4 ident arch
  74 1 comma ,
  76 39 string "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  115 1 eol \n
  117 10 directive .attribute
  128 16 ident unaligned_access
  144 1 comma ,
  146 1 int 0
  147 1 eol \n
  149 10 directive .attribute
  160 11 ident stack_align
  171 1 comma ,
  173 2 int 16
  175 1 eol \n
  177 5 directive .text
  182 1 eol \n
  184 6 directive .align
  191 1 int 2
  192 1 eol \n
  194 6 directive .globl
  201 14 ident asm_test_entry
  215 1 eol \n
  217 5 directive .type
  223 14 ident asm_test_entry
  237 1 comma ,
  239 1 at @
  240 8 ident function
  248 1 eol \n
  249 14 ident asm_test_entry
  263 1 colon :
  264 1 eol \n
  265 5 directive .LFB0
  270 1 colon :
  271 1 eol \n
  273 14 directive .cfi_startproc
  287 1 eol \n
  289 2 ident li
  292 2 ident a0
  294 1 comma ,
  295 2 int 42
  297 1 eol \n
  299 3 ident ret
  302 1 eol \n
  304 12 directive .cfi_endproc
  316 1 eol \n
  317 5 directive .LFE0
  322 1 colon :
  323 1 eol \n
  325 5 directive .size
  331 14 ident asm_test_entry
  345 1 comma ,
  347 1 dot .
  348 1 minus -
  349 14 ident asm_test_entry
  363 1 eol \n
  365 6 directive .ident
  372 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  404 1 eol \n
  406 8 directive .section
  415 9 directive .note.GNU
  424 1 minus -
  425 5 ident stack
  430 1 comma ,
  431 2 string ""
  433 1 comma ,
  434 1 at @
  435 8 ident progbits
  443 1 eol \n
  444 0 eof 
  ########## riscv64 return42 source ast
  source asm_test_entry
  directive .file ["asm_test_entry.c"]
  directive .option [nopic]
  directive .option [norelax]
  directive .attribute [arch] ["rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"]
  directive .attribute [unaligned_access] [0]
  directive .attribute [stack_align] [16]
  directive .text
  directive .align [2]
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn li x10, 42
  insn ret
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## riscv64 return42 normalized ast
  normalized asm_test_entry
  target-state .option nopic
  target-state .option norelax
  target-state .attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  target-state .attribute unaligned_access, 0
  target-state .attribute stack_align, 16
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  li x10, 42
  ret
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## riscv64 return42 lowered ast
  lowered asm_test_entry
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes 13 05 a0 02              [riscv64.addi]
    bytes 67 80 00 00              [riscv64.jalr]
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## riscv64 return42 plan
  segment .text size=8 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  ########## riscv64 return42 image
  section .text address=0x0 size=8 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=8
  ########## riscv64 return42 bytes
  13 05 a0 02 67 80 00 00
  ########## riscv64 return42 disasm canonical
  	addi x10, x0, 42
  	jalr x0, 0(x1)
  ########## riscv64 return42 disasm diagnostic
  00000000  13 05 a0 02  addi x10, x0, 42  [riscv64.addi]
  00000004  67 80 00 00  jalr x0, 0(x1)    [riscv64.jalr]
  ########## riscv64 return42 codec
  alt riscv64
    [0 cost=0] pair             riscv64-pair(){auipc-i-pair:64u}
    [1 cost=0] word             riscv64-word(){instruction:32u}
    [2 cost=0] compressed       riscv64-compressed(){compressed-instruction:16u}
  ########## x86_32 loop tokens
  1 5 directive .file
  7 8 string "loop.c"
  15 1 eol \n
  17 5 directive .text
  22 1 eol \n
  24 6 directive .globl
  31 14 ident asm_test_entry
  45 1 eol \n
  47 5 directive .type
  53 14 ident asm_test_entry
  67 1 comma ,
  69 1 at @
  70 8 ident function
  78 1 eol \n
  79 14 ident asm_test_entry
  93 1 colon :
  94 1 eol \n
  96 4 ident subl
  101 1 immsigil $
  102 2 int 16
  104 1 comma ,
  106 4 register %esp
  110 1 eol \n
  112 4 ident movl
  117 1 immsigil $
  118 1 int 6
  119 1 comma ,
  121 2 int 12
  123 1 lparen (
  124 4 register %esp
  128 1 rparen )
  129 1 eol \n
  131 4 ident movl
  136 2 int 12
  138 1 lparen (
  139 4 register %esp
  143 1 rparen )
  144 1 comma ,
  146 4 register %eax
  150 1 eol \n
  152 5 ident testl
  158 4 register %eax
  162 1 comma ,
  164 4 register %eax
  168 1 eol \n
  170 3 ident jle
  174 3 directive .L4
  177 1 eol \n
  179 4 ident movl
  184 1 immsigil $
  185 1 int 0
  186 1 comma ,
  188 4 register %eax
  192 1 eol \n
  194 4 ident movl
  199 1 immsigil $
  200 1 int 0
  201 1 comma ,
  203 4 register %edx
  207 1 eol \n
  209 8 directive .p2align
  218 1 int 4
  219 1 eol \n
  220 3 directive .L3
  223 1 colon :
  224 1 eol \n
  226 4 ident addl
  231 1 immsigil $
  232 1 int 7
  233 1 comma ,
  235 4 register %edx
  239 1 eol \n
  241 4 ident addl
  246 1 immsigil $
  247 1 int 1
  248 1 comma ,
  250 4 register %eax
  254 1 eol \n
  256 4 ident movl
  261 2 int 12
  263 1 lparen (
  264 4 register %esp
  268 1 rparen )
  269 1 comma ,
  271 4 register %ecx
  275 1 eol \n
  277 4 ident cmpl
  282 4 register %eax
  286 1 comma ,
  288 4 register %ecx
  292 1 eol \n
  294 2 ident jg
  297 3 directive .L3
  300 1 eol \n
  301 3 directive .L1
  304 1 colon :
  305 1 eol \n
  307 4 ident movl
  312 4 register %edx
  316 1 comma ,
  318 4 register %eax
  322 1 eol \n
  324 4 ident addl
  329 1 immsigil $
  330 2 int 16
  332 1 comma ,
  334 4 register %esp
  338 1 eol \n
  340 3 ident ret
  343 1 eol \n
  344 3 directive .L4
  347 1 colon :
  348 1 eol \n
  350 4 ident movl
  355 1 immsigil $
  356 1 int 0
  357 1 comma ,
  359 4 register %edx
  363 1 eol \n
  365 3 ident jmp
  369 3 directive .L1
  372 1 eol \n
  374 5 directive .size
  380 14 ident asm_test_entry
  394 1 comma ,
  396 1 dot .
  397 1 minus -
  398 14 ident asm_test_entry
  412 1 eol \n
  414 6 directive .ident
  421 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  453 1 eol \n
  455 8 directive .section
  464 9 directive .note.GNU
  473 1 minus -
  474 5 ident stack
  479 1 comma ,
  480 2 string ""
  482 1 comma ,
  483 1 at @
  484 8 ident progbits
  492 1 eol \n
  493 0 eof 
  ########## x86_32 loop source ast
  source loop
  directive .file ["loop.c"]
  directive .text
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  insn subl $16, %esp
  insn movl $6, 12(%esp)
  insn movl 12(%esp), %eax
  insn testl %eax, %eax
  insn jle .L4
  insn movl $0, %eax
  insn movl $0, %edx
  directive .p2align [4]
  label .L3
  insn addl $7, %edx
  insn addl $1, %eax
  insn movl 12(%esp), %ecx
  insn cmpl %eax, %ecx
  insn jg .L3
  label .L1
  insn movl %edx, %eax
  insn addl $16, %esp
  insn ret
  label .L4
  insn movl $0, %edx
  insn jmp .L1
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## x86_32 loop normalized ast
  normalized loop
  section .text r-x
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  subl $16, %esp
  movl $6, 12(%esp)
  movl 12(%esp), %eax
  test %eax, %eax
  jle .L4
  movl $0, %eax
  movl $0, %edx
  align 16
  label .L3
  addl $7, %edx
  addl $1, %eax
  movl 12(%esp), %ecx
  cmpl %eax, %ecx
  jg .L3
  label .L1
  movl %edx, %eax
  addl $16, %esp
  ret
  label .L4
  movl $0, %edx
  jmp .L1
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_32 loop lowered ast
  lowered loop
  section .text r-x align=16
    label asm_test_entry
    bytes 83 ec 10                 [x86_32.alu-rm-imm8.opsz-absent.reg]
    bytes c7 44 24 0c 06 00 00 00  [x86_32.movl]
    bytes 8b 44 24 0c              [x86_32.mov-r-rm.opsz-absent.sib-disp8]
    bytes 85 c0                    [x86_32.alu-rm-r.opsz-absent.reg]
    relax
      7e 00                    [x86_32.jcc-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L4
      0f 8e 00 00 00 00        [x86_32.jcc-rel.d32]
      @2/4B pc+6 target pcrel32-branch s32 [0+32@0] = .L4
    bytes b8 00 00 00 00           [x86_32.mov-r-imm.opsz-absent]
    bytes ba 00 00 00 00           [x86_32.mov-r-imm.opsz-absent]
    align 16
    label .L3
    bytes 83 c2 07                 [x86_32.alu-rm-imm8.opsz-absent.reg]
    bytes 83 c0 01                 [x86_32.alu-rm-imm8.opsz-absent.reg]
    bytes 8b 4c 24 0c              [x86_32.mov-r-rm.opsz-absent.sib-disp8]
    bytes 39 c1                    [x86_32.alu-rm-r.opsz-absent.reg]
    relax
      7f 00                    [x86_32.jcc-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L3
      0f 8f 00 00 00 00        [x86_32.jcc-rel.d32]
      @2/4B pc+6 target pcrel32-branch s32 [0+32@0] = .L3
    label .L1
    bytes 89 d0                    [x86_32.mov-rm-r.opsz-absent.reg]
    bytes 83 c4 10                 [x86_32.alu-rm-imm8.opsz-absent.reg]
    bytes c3                       [x86_32.ret]
    label .L4
    bytes ba 00 00 00 00           [x86_32.mov-r-imm.opsz-absent]
    relax
      eb 00                    [x86_32.jmp-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L1
      e9 00 00 00 00           [x86_32.jmp-rel.d32]
      @1/4B pc+5 target pcrel32-branch s32 [0+32@0] = .L1
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .L3 notype in .text
  local .L1 notype in .text
  local .L4 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## x86_32 loop plan
  segment .text size=59 zero=0 align=16 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## x86_32 loop image
  section .text address=0x0 size=59 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=59
  ########## x86_32 loop bytes
  83 ec 10 c7 44 24 0c 06 00 00 00 8b 44 24 0c 85 c0 7e 21 b8 00 00 00 00 ba 00 00 00 00 8d 76 00 83 c2 07 83 c0 01 8b 4c 24 0c 39 c1 7f f2 89 d0 83 c4 10 c3 ba 00 00 00 00 eb f3
  ########## x86_32 loop disasm canonical
  	subl $16, %esp
  	movl $6, 12(%esp)
  	movl 12(%esp), %eax
  	test %eax, %eax
  	jle.d8 52
  	movl $0, %eax
  	movl $0, %edx
  	.balign 4
  	addl $7, %edx
  	addl $1, %eax
  	movl 12(%esp), %ecx
  	cmpl %eax, %ecx
  	jg.d8 32
  	movl %edx, %eax
  	addl $16, %esp
  	ret
  	movl $0, %edx
  	jmp.d8 46
  ########## x86_32 loop disasm diagnostic
  00000000  83 ec 10                 subl $16, %esp       [x86_32.alu-rm-imm8.opsz-absent.reg]
  00000003  c7 44 24 0c 06 00 00 00  movl $6, 12(%esp)    [x86_32.movl]
  0000000b  8b 44 24 0c              movl 12(%esp), %eax  [x86_32.mov-r-rm.opsz-absent.sib-disp8]
  0000000f  85 c0                    test %eax, %eax      [x86_32.alu-rm-r.opsz-absent.reg]
  00000011  7e 21                    jle.d8 52            [x86_32.jcc-rel.d8]
  00000013  b8 00 00 00 00           movl $0, %eax        [x86_32.mov-r-imm.opsz-absent]
  00000018  ba 00 00 00 00           movl $0, %edx        [x86_32.mov-r-imm.opsz-absent]
  0000001d  8d 76 00                 .balign 4            [padding]
  00000020  83 c2 07                 addl $7, %edx        [x86_32.alu-rm-imm8.opsz-absent.reg]
  00000023  83 c0 01                 addl $1, %eax        [x86_32.alu-rm-imm8.opsz-absent.reg]
  00000026  8b 4c 24 0c              movl 12(%esp), %ecx  [x86_32.mov-r-rm.opsz-absent.sib-disp8]
  0000002a  39 c1                    cmpl %eax, %ecx      [x86_32.alu-rm-r.opsz-absent.reg]
  0000002c  7f f2                    jg.d8 32             [x86_32.jcc-rel.d8]
  0000002e  89 d0                    movl %edx, %eax      [x86_32.mov-rm-r.opsz-absent.reg]
  00000030  83 c4 10                 addl $16, %esp       [x86_32.alu-rm-imm8.opsz-absent.reg]
  00000033  c3                       ret                  [x86_32.ret]
  00000034  ba 00 00 00 00           movl $0, %edx        [x86_32.mov-r-imm.opsz-absent]
  00000039  eb f3                    jmp.d8 46            [x86_32.jmp-rel.d8]
  ########## x86_32 loop codec
  alt x86_32
    [-2 cost=0] alu-acc-imm8     alu-acc-imm8(){prefixes alu-acc-imm8-opcode imm:8s}
    [-1 cost=0] alu-acc-imm      alu-acc-imm(){prefixes alu-acc-imm-opcode imm-sym32}
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){prefixes 10000011 modrm imm}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){prefixes 10000001 modrm imm-sym32}
    [2 cost=0] mov-r-imm        mov-r-imm(){prefixes 10111 reg:3u imm-sym32}
    [3 cost=0] mov-eax-moffs    mov-eax-moffs(){10100001 disp-sym}
    [4 cost=0] mov-moffs-eax    mov-moffs-eax(){10100011 disp-sym}
    [5 cost=0] mov-rm-r         mov-rm-r(){prefixes 10001001 modrm}
    [6 cost=0] mov-r-rm         mov-r-rm(){prefixes 10001011 modrm}
    [7 cost=0] lea              lea(){prefixes 10001101 modrm}
    [8 cost=0] ret              ret(){11000011}
    [9 cost=0] mov-rm-imm8      mov-rm-imm8(){prefixes 11000110 modrm imm8:8s}
    [10 cost=0] alu-rm-r         alu-rm-r(){prefixes alu-rm-r-op modrm}
    [11 cost=0] pop-r            pop-r(){prefixes 01011 reg:3u}
    [12 cost=0] jmp-rm           jmp-rm(){prefixes 11111111 modrm}
    [13 cost=0] ud2              ud2(){0000111100001011}
    [14 cost=0] call-rel32       call-rel32(){11101000 le32}
    [15 cost=0] imul-r-rm        imul-r-rm(){prefixes 0000111110101111 modrm}
    [16 cost=0] cmov-r-rm        cmov-r-rm(){prefixes 000011110100 cc modrm}
    [17 cost=0] jmp-rel          relax jmp
                                   d8               jmp.d8(){11101011 <target:8@0 pcrel8-branch>}
                                   d32              jmp.d32(){11101001 le32}
    [18 cost=0] jcc-rel          relax jcc
                                   d8               jcc.d8(){0111 cc <target:8@0 pcrel8-branch>}
                                   d32              jcc.d32(){000011111000 cc le32}
    [19 cost=0] push-r           push-r(){prefixes 01010 reg:3u}
    [20 cost=0] unary-rm         unary-rm(){prefixes 11110111 modrm}
    [21 cost=0] alu-r-rm         alu-r-rm(){prefixes alu-r-rm-op modrm}
    [22 cost=0] shift1-rm        shift1-rm(){prefixes 11010001 modrm}
    [23 cost=0] dec-r            dec-r(){01001 reg:3u}
    [24 cost=0] sse-binop-f2     sse-binop-f2(){no-asz 11110010 no-rex 00001111 sse-binop-f2-op modrm}
    [25 cost=0] sse-binop-f3     sse-binop-f3(){no-asz 11110011 no-rex 00001111 sse-binop-f3-op modrm}
    [26 cost=0] sse-binop-66     sse-binop-66(){no-asz 01100110 no-rex 00001111 sse-binop-66-op modrm}
    [27 cost=0] sse-binop-none   sse-binop-none(){prefixes 00001111 sse-binop-none-op modrm}
    [28 cost=0] sse-movsd-load   sse-movsd-load(){no-asz 11110010 no-rex 0000111100010000 modrm}
    [29 cost=0] sse-movss-load   sse-movss-load(){no-asz 11110011 no-rex 0000111100010000 modrm}
    [30 cost=0] sse-movsd-store  sse-movsd-store(){no-asz 11110010 no-rex 0000111100010001 modrm}
    [31 cost=0] sse-movss-store  sse-movss-store(){no-asz 11110011 no-rex 0000111100010001 modrm}
    [32 cost=0] cvtsi2sd-r-rm    cvtsi2sd-r-rm(){no-asz 11110010 no-rex 0000111100101010 modrm}
    [33 cost=0] cvtsi2ss-r-rm    cvtsi2ss-r-rm(){no-asz 11110011 no-rex 0000111100101010 modrm}
    [34 cost=0] cvttsd2si-r-rm   cvttsd2si-r-rm(){no-asz 11110010 no-rex 0000111100101100 modrm}
    [35 cost=0] shift-imm-rm     shift-imm-rm(){prefixes 11000001 modrm imm8:8u}
    [36 cost=0] shift-cl-rm      shift-cl-rm(){prefixes 11010011 modrm}
    [37 cost=0] setcc-rm         setcc-rm(){prefixes 000011111001 cc modrm}
    [38 cost=0] movzx-b-r-rm     movzx-b-r-rm(){prefixes 0000111110110110 modrm}
    [39 cost=0] movzx-w-r-rm     movzx-w-r-rm(){prefixes 0000111110110111 modrm}
    [40 cost=0] movsx-b-r-rm     movsx-b-r-rm(){prefixes 0000111110111110 modrm}
    [41 cost=0] movsx-w-r-rm     movsx-w-r-rm(){prefixes 0000111110111111 modrm}
    [42 cost=0] movsxd-r-rm      movsxd-r-rm(){prefixes 01100011 modrm}
    [43 cost=0] imul-r-rm-imm8   imul-r-rm-imm8(){prefixes 01101011 modrm imm:8s}
    [44 cost=0] imul-r-rm-imm32  imul-r-rm-imm32(){prefixes 01101001 modrm le32}
    [45 cost=0] test-rm-imm      test-rm-imm(){prefixes 11110111 modrm le32}
    [46 cost=0] mov-rm-r8        mov-rm-r8(){prefixes 10001000 modrm}
    [47 cost=0] mov-r-rm8        mov-r-rm8(){prefixes 10001010 modrm}
    [48 cost=0] push-imm8        push-imm8(){01101010 imm}
    [49 cost=0] push-imm32       push-imm32(){01101000 imm-sym32}
    [50 cost=0] fldl             fldl(){prefixes 11011101 modrm}
    [51 cost=0] fstpl            fstpl(){prefixes 11011101 modrm}
    [52 cost=0] fstps            fstps(){prefixes 11011001 modrm}
    [53 cost=0] shld-imm-rm      shld-imm-rm(){prefixes 0000111110100100 modrm imm8:8u}
    [54 cost=0] flds             flds(){prefixes 11011001 modrm}
    [55 cost=0] fucomp           fucomp(){1101110111101001}
    [56 cost=0] fildll           fildll(){prefixes 11011111 modrm}
    [57 cost=0] fadds            fadds(){prefixes 11011000 modrm}
    [58 cost=0] fnstcw           fnstcw(){prefixes 11011001 modrm}
    [59 cost=0] fldcw            fldcw(){prefixes 11011001 modrm}
    [60 cost=0] fistpll          fistpll(){prefixes 11011111 modrm}
    [61 cost=0] fsubs            fsubs(){prefixes 11011000 modrm}
    [62 cost=0] fnstsw           fnstsw(){1101111111100000}
    [63 cost=0] sahf             sahf(){10011110}
    [64 cost=0] mov-r-imm8       mov-r-imm8(){prefixes 10110 reg:3u imm:8u}
    [65 cost=0] fadd-st0-x87     fadd-st0-x87(){11011000 11 000 st:3u}
    [66 cost=0] alu-rm-imm8-byte alu-rm-imm8-byte(){prefixes 10000000 modrm imm:8s}
    [67 cost=0] vex-scalar-f2-rrr vex-scalar-f2-rrr(){11000101 vex-byte2:8u vex-scalar-f2-op modrm}
    [68 cost=0] vex-scalar-f3-rrr vex-scalar-f3-rrr(){11000101 vex-byte2:8u vex-scalar-f3-op modrm}
    [69 cost=0] vex-scalar-none-rrr vex-scalar-none-rrr(){11000101 vex-byte2:8u vex-scalar-none-op modrm}
    [70 cost=0] vex-scalar-66-rrr vex-scalar-66-rrr(){11000101 vex-byte2:8u vex-scalar-66-op modrm}
    [71 cost=0] vex-unop-none    vex-unop-none(){11000101 vex-byte2:8u vex-unop-none-op modrm}
    [72 cost=0] vex-unop-66      vex-unop-66(){11000101 vex-byte2:8u vex-unop-66-op modrm}
    [73 cost=0] sse-binop-imm-f2 sse-binop-imm-f2(){no-asz 11110010 no-rex 00001111 sse-binop-imm-f2-op modrm imm8:8u}
    [74 cost=0] sse-binop-imm-f3 sse-binop-imm-f3(){no-asz 11110011 no-rex 00001111 sse-binop-imm-f3-op modrm imm8:8u}
    [75 cost=0] sse-binop-imm-66 sse-binop-imm-66(){no-asz 01100110 no-rex 00001111 sse-binop-imm-66-op modrm imm8:8u}
    [76 cost=0] sse-binop-imm-none sse-binop-imm-none(){prefixes 00001111 sse-binop-imm-none-op modrm imm8:8u}
    [77 cost=0] vex-binop-imm-f2 vex-binop-imm-f2(){11000101 vex-byte2:8u vex-binop-imm-f2-op modrm imm8:8u}
    [78 cost=0] vex-binop-imm-f3 vex-binop-imm-f3(){11000101 vex-byte2:8u vex-binop-imm-f3-op modrm imm8:8u}
    [79 cost=0] vex-binop-imm-none vex-binop-imm-none(){11000101 vex-byte2:8u vex-binop-imm-none-op modrm imm8:8u}
    [80 cost=0] vex-binop-imm-66 vex-binop-imm-66(){11000101 vex-byte2:8u vex-binop-imm-66-op modrm imm8:8u}
    [81 cost=0] vex-unop-imm-f2  vex-unop-imm-f2(){11000101 vex-byte2:8u vex-unop-imm-f2-op modrm imm8:8u}
    [82 cost=0] vex-unop-imm-f3  vex-unop-imm-f3(){11000101 vex-byte2:8u vex-unop-imm-f3-op modrm imm8:8u}
    [83 cost=0] vex-unop-imm-66  vex-unop-imm-66(){11000101 vex-byte2:8u vex-unop-imm-66-op modrm imm8:8u}
    [84 cost=0] movd-load-r-rm   movd-load-r-rm(){no-asz 01100110 no-rex 0000111101101110 modrm}
    [85 cost=0] movd-store-r-rm  movd-store-r-rm(){no-asz 01100110 no-rex 0000111101111110 modrm}
    [86 cost=0] vex-movd-load-r-rm vex-movd-load-r-rm(){11000101 vex-byte2:8u 01101110 modrm}
    [87 cost=0] vex-movd-store-r-rm vex-movd-store-r-rm(){11000101 vex-byte2:8u 01111110 modrm}
    [88 cost=0] movdqa-load      movdqa-load(){no-asz 01100110 no-rex 0000111101101111 modrm}
    [89 cost=0] movdqu-load      movdqu-load(){no-asz 11110011 no-rex 0000111101101111 modrm}
    [90 cost=0] movdqa-store     movdqa-store(){no-asz 01100110 no-rex 0000111101111111 modrm}
    [91 cost=0] movdqu-store     movdqu-store(){no-asz 11110011 no-rex 0000111101111111 modrm}
    [92 cost=0] vex-unop-f3      vex-unop-f3(){11000101 vex-byte2:8u vex-unop-f3-op modrm}
    [93 cost=0] sse-pinsrw       sse-pinsrw(){no-asz 01100110 no-rex 0000111111000100 modrm imm8:8u}
    [94 cost=0] sse-pextrw       sse-pextrw(){no-asz 01100110 no-rex 0000111111000101 modrm imm8:8u}
    [95 cost=0] vex-pinsrw       vex-pinsrw(){11000101 vex-byte2:8u 11000100 modrm imm8:8u}
    [96 cost=0] vex-pextrw       vex-pextrw(){11000101 vex-byte2:8u 11000101 modrm imm8:8u}
    [97 cost=0] sse-movmsk-66    sse-movmsk-66(){no-asz 01100110 no-rex 00001111 sse-movmsk-66-op modrm}
    [98 cost=0] sse-movmsk-none  sse-movmsk-none(){prefixes 00001111 sse-movmsk-none-op modrm}
    [99 cost=0] vex-movmsk-66    vex-movmsk-66(){11000101 vex-byte2:8u vex-movmsk-66-op modrm}
    [100 cost=0] vex-movmsk-none  vex-movmsk-none(){11000101 vex-byte2:8u vex-movmsk-none-op modrm}
    [101 cost=0] xmm-shift-imm-w  xmm-shift-imm-w(){no-asz 01100110 no-rex 00001111 01110001 modrm imm8:8u}
    [102 cost=0] xmm-shift-imm-d  xmm-shift-imm-d(){no-asz 01100110 no-rex 00001111 01110010 modrm imm8:8u}
    [103 cost=0] xmm-shift-imm-q  xmm-shift-imm-q(){no-asz 01100110 no-rex 00001111 01110011 modrm imm8:8u}
    [104 cost=0] vex-shift-imm-w  vex-shift-imm-w(){11000101 vex-byte2:8u 01110001 modrm imm8:8u}
    [105 cost=0] vex-shift-imm-d  vex-shift-imm-d(){11000101 vex-byte2:8u 01110010 modrm imm8:8u}
    [106 cost=0] vex-shift-imm-q  vex-shift-imm-q(){11000101 vex-byte2:8u 01110011 modrm imm8:8u}
    [107 cost=0] sse-binop-0f38-66 sse-binop-0f38-66(){no-asz 01100110 no-rex 00001111 00111000 sse-binop-0f38-op modrm}
    [108 cost=0] sse-binop-imm-0f3a-66 sse-binop-imm-0f3a-66(){no-asz 01100110 no-rex 00001111 00111010 sse-binop-imm-0f3a-op modrm imm8:8u}
    [109 cost=0] sse-gpr-imm-0f3a-66 sse-gpr-imm-0f3a-66(){no-asz 01100110 no-rex 00001111 00111010 sse-gpr-imm-0f3a-op modrm imm8:8u}
    [110 cost=0] vex3-map2-66-rrr vex3-map2-66-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-66-op modrm}
    [111 cost=0] vex3-map3-66-imm-rrr vex3-map3-66-imm-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map3-66-op modrm imm8:8u}
    [112 cost=0] vex3-map2-66-unop vex3-map2-66-unop(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-unop-op modrm}
    [113 cost=0] evex-ps-rrr      evex-ps-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-ps-op modrm}
    [114 cost=0] evex-pd-rrr      evex-pd-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-pd-op modrm}
    [200 cost=0] short-loop       relax loop
                                    d8               loop.d8(){11100010 <target:8@0 pcrel8-branch>}
    [201 cost=0] short-loope      relax loope
                                    d8               loope.d8(){11100001 <target:8@0 pcrel8-branch>}
    [202 cost=0] short-loopne     relax loopne
                                    d8               loopne.d8(){11100000 <target:8@0 pcrel8-branch>}
    [203 cost=0] short-jecxz      relax jecxz
                                    d8               jecxz.d8(){11100011 <target:8@0 pcrel8-branch>}
    [204 cost=0] short-jcxz       relax jcxz
                                    d8               jcxz.d8(){0110011111100011 <target:8@0 pcrel8-branch>}
    [300 cost=0] mov-r64-simm32   mov-r64-simm32(){prefixes 11000111 modrm le32}
    [301 cost=0] mov-r64-imm64    mov-r64-imm64(){prefixes 10111 reg:3u le64}
    [302 cost=0] bswap-r          bswap-r(){prefixes 00001111 11001 reg:3u}
  prefixes(){no-asz opsz no-rex}
  alu-acc-imm8-opcode(){opcode:8u}
  alu-acc-imm-opcode(){opcode:8u}
  imm-sym32(){le32}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-nobase-disp32 modrm-sib-nobase-disp32(){00 reg:3u 100 scale:2u index:3u 101 disp-sym}
    [2 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib disp-none}
    [3 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp-c8}
    [4 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib disp-c32}
    [5 cost=0] disp32-norm      modrm-disp32-norm(){00 reg:3u 101 disp-sym}
    [6 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u disp-none}
    [7 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp-c8}
    [8 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u disp-sym}
  imm(){imm:8s}
  disp-sym(){le32}
  alu-rm-r-op[9]{opcode:8u}
  le32(){<target:32@0 pcrel32-call>}
  cc[16]{cc:4u}
  alu-r-rm-op[8]{opcode:8u}
  no-asz(){()}
  no-rex(){()}
  sse-binop-f2-op[8]{opcode:8u}
  sse-binop-f3-op[9]{opcode:8u}
  sse-binop-66-op[66]{opcode:8u}
  sse-binop-none-op[19]{opcode:8u}
  vex-scalar-f2-op[7]{opcode:8u}
  vex-scalar-f3-op[7]{opcode:8u}
  vex-scalar-none-op[12]{opcode:8u}
  vex-scalar-66-op[58]{opcode:8u}
  vex-unop-none-op[7]{opcode:8u}
  vex-unop-66-op[8]{opcode:8u}
  sse-binop-imm-f2-op[2]{opcode:8u}
  sse-binop-imm-f3-op[2]{opcode:8u}
  sse-binop-imm-66-op[3]{opcode:8u}
  sse-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-f2-op[1]{opcode:8u}
  vex-binop-imm-f3-op[1]{opcode:8u}
  vex-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-66-op[2]{opcode:8u}
  vex-unop-imm-f2-op[1]{opcode:8u}
  vex-unop-imm-f3-op[1]{opcode:8u}
  vex-unop-imm-66-op[1]{opcode:8u}
  vex-unop-f3-op[2]{opcode:8u}
  sse-movmsk-66-op[2]{opcode:8u}
  sse-movmsk-none-op[1]{opcode:8u}
  vex-movmsk-66-op[2]{opcode:8u}
  vex-movmsk-none-op[1]{opcode:8u}
  sse-binop-0f38-op[46]{opcode:8u}
  sse-binop-imm-0f3a-op[12]{opcode:8u}
  sse-gpr-imm-0f3a-op[5]{opcode:8u}
  vex3-map2-66-op[25]{opcode:8u}
  vex3-map3-66-op[10]{opcode:8u}
  vex3-map2-unop-op[18]{opcode:8u}
  evex-ps-op[8]{opcode:8u}
  evex-pd-op[8]{opcode:8u}
  le64(){imm64:64u}
  alt opsz
    [0 cost=0] opsz-present     opsz-present(){01100110}
    [1 cost=0] opsz-absent      opsz-absent(){()}
  sib(){scale:2u index:3u base:3u}
  disp-none(){no-disp}
  disp-c8(){disp8:8s}
  disp-c32(){le32}
  no-disp(){()}
  ########## x86_64 loop tokens
  1 5 directive .file
  7 8 string "loop.c"
  15 1 eol \n
  17 5 directive .text
  22 1 eol \n
  24 6 directive .globl
  31 14 ident asm_test_entry
  45 1 eol \n
  47 5 directive .type
  53 14 ident asm_test_entry
  67 1 comma ,
  69 1 at @
  70 8 ident function
  78 1 eol \n
  79 14 ident asm_test_entry
  93 1 colon :
  94 1 eol \n
  96 4 ident movl
  101 1 immsigil $
  102 1 int 6
  103 1 comma ,
  105 1 minus -
  106 1 int 4
  107 1 lparen (
  108 4 register %rsp
  112 1 rparen )
  113 1 eol \n
  115 4 ident movl
  120 1 minus -
  121 1 int 4
  122 1 lparen (
  123 4 register %rsp
  127 1 rparen )
  128 1 comma ,
  130 4 register %eax
  134 1 eol \n
  136 5 ident testl
  142 4 register %eax
  146 1 comma ,
  148 4 register %eax
  152 1 eol \n
  154 3 ident jle
  158 3 directive .L4
  161 1 eol \n
  163 4 ident movl
  168 1 immsigil $
  169 1 int 0
  170 1 comma ,
  172 4 register %eax
  176 1 eol \n
  178 4 ident movl
  183 1 immsigil $
  184 1 int 0
  185 1 comma ,
  187 4 register %edx
  191 1 eol \n
  193 8 directive .p2align
  202 1 int 4
  203 1 eol \n
  204 3 directive .L3
  207 1 colon :
  208 1 eol \n
  210 4 ident addl
  215 1 immsigil $
  216 1 int 7
  217 1 comma ,
  219 4 register %edx
  223 1 eol \n
  225 4 ident addl
  230 1 immsigil $
  231 1 int 1
  232 1 comma ,
  234 4 register %eax
  238 1 eol \n
  240 4 ident movl
  245 1 minus -
  246 1 int 4
  247 1 lparen (
  248 4 register %rsp
  252 1 rparen )
  253 1 comma ,
  255 4 register %ecx
  259 1 eol \n
  261 4 ident cmpl
  266 4 register %eax
  270 1 comma ,
  272 4 register %ecx
  276 1 eol \n
  278 2 ident jg
  281 3 directive .L3
  284 1 eol \n
  285 3 directive .L1
  288 1 colon :
  289 1 eol \n
  291 4 ident movl
  296 4 register %edx
  300 1 comma ,
  302 4 register %eax
  306 1 eol \n
  308 3 ident ret
  311 1 eol \n
  312 3 directive .L4
  315 1 colon :
  316 1 eol \n
  318 4 ident movl
  323 1 immsigil $
  324 1 int 0
  325 1 comma ,
  327 4 register %edx
  331 1 eol \n
  333 3 ident jmp
  337 3 directive .L1
  340 1 eol \n
  342 5 directive .size
  348 14 ident asm_test_entry
  362 1 comma ,
  364 1 dot .
  365 1 minus -
  366 14 ident asm_test_entry
  380 1 eol \n
  382 6 directive .ident
  389 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  421 1 eol \n
  423 8 directive .section
  432 9 directive .note.GNU
  441 1 minus -
  442 5 ident stack
  447 1 comma ,
  448 2 string ""
  450 1 comma ,
  451 1 at @
  452 8 ident progbits
  460 1 eol \n
  461 0 eof 
  ########## x86_64 loop source ast
  source loop
  directive .file ["loop.c"]
  directive .text
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  insn movl $6, -4(%rsp)
  insn movl -4(%rsp), %eax
  insn testl %eax, %eax
  insn jle .L4
  insn movl $0, %eax
  insn movl $0, %edx
  directive .p2align [4]
  label .L3
  insn addl $7, %edx
  insn addl $1, %eax
  insn movl -4(%rsp), %ecx
  insn cmpl %eax, %ecx
  insn jg .L3
  label .L1
  insn movl %edx, %eax
  insn ret
  label .L4
  insn movl $0, %edx
  insn jmp .L1
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## x86_64 loop normalized ast
  normalized loop
  section .text r-x
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  movl $6, -4(%rsp)
  movl -4(%rsp), %eax
  test %eax, %eax
  jle .L4
  movl $0, %eax
  movl $0, %edx
  align 16
  label .L3
  addl $7, %edx
  addl $1, %eax
  movl -4(%rsp), %ecx
  cmpl %eax, %ecx
  jg .L3
  label .L1
  movl %edx, %eax
  ret
  label .L4
  movl $0, %edx
  jmp .L1
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## x86_64 loop lowered ast
  lowered loop
  section .text r-x align=16
    label asm_test_entry
    bytes c7 44 24 fc 06 00 00 00  [x86_64.movl]
    bytes 8b 44 24 fc              [x86_64.mov-r-rm.asz-absent.opsz-absent.rex-absent.sib-disp8]
    bytes 85 c0                    [x86_64.alu-rm-r.asz-absent.opsz-absent.rex-absent.reg]
    relax
      7e 00                    [x86_64.jcc-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L4
      0f 8e 00 00 00 00        [x86_64.jcc-rel.d32]
      @2/4B pc+6 target pcrel32-branch s32 [0+32@0] = .L4
    bytes b8 00 00 00 00           [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
    bytes ba 00 00 00 00           [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
    align 16
    label .L3
    bytes 83 c2 07                 [x86_64.alu-rm-imm8.asz-absent.opsz-absent.rex-absent.reg]
    bytes 83 c0 01                 [x86_64.alu-rm-imm8.asz-absent.opsz-absent.rex-absent.reg]
    bytes 8b 4c 24 fc              [x86_64.mov-r-rm.asz-absent.opsz-absent.rex-absent.sib-disp8]
    bytes 39 c1                    [x86_64.alu-rm-r.asz-absent.opsz-absent.rex-absent.reg]
    relax
      7f 00                    [x86_64.jcc-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L3
      0f 8f 00 00 00 00        [x86_64.jcc-rel.d32]
      @2/4B pc+6 target pcrel32-branch s32 [0+32@0] = .L3
    label .L1
    bytes 89 d0                    [x86_64.mov-rm-r.asz-absent.opsz-absent.rex-absent.reg]
    bytes c3                       [x86_64.ret]
    label .L4
    bytes ba 00 00 00 00           [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
    relax
      eb 00                    [x86_64.jmp-rel.d8]
      @1/1B pc+2 target pcrel8-branch s8 [0+8@0] = .L1
      e9 00 00 00 00           [x86_64.jmp-rel.d32]
      @1/4B pc+5 target pcrel32-branch s32 [0+32@0] = .L1
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .L3 notype in .text
  local .L1 notype in .text
  local .L4 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## x86_64 loop plan
  segment .text size=56 zero=0 align=16 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## x86_64 loop image
  section .text address=0x0 size=56 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=56
  ########## x86_64 loop bytes
  c7 44 24 fc 06 00 00 00 8b 44 24 fc 85 c0 7e 21 b8 00 00 00 00 ba 00 00 00 00 66 0f 1f 44 00 00 83 c2 07 83 c0 01 8b 4c 24 fc 39 c1 7f f2 89 d0 c3 ba 00 00 00 00 eb f6
  ########## x86_64 loop disasm canonical
  	movl $6, -4(%rsp)
  	movl -4(%rsp), %eax
  	test %eax, %eax
  	jle.d8 49
  	movl $0, %eax
  	movl $0, %edx
  	.balign 8
  	addl $7, %edx
  	addl $1, %eax
  	movl -4(%rsp), %ecx
  	cmpl %eax, %ecx
  	jg.d8 32
  	movl %edx, %eax
  	ret
  	movl $0, %edx
  	jmp.d8 46
  ########## x86_64 loop disasm diagnostic
  00000000  c7 44 24 fc 06 00 00 00  movl $6, -4(%rsp)    [x86_64.movl]
  00000008  8b 44 24 fc              movl -4(%rsp), %eax  [x86_64.mov-r-rm.asz-absent.opsz-absent.rex-absent.sib-disp8]
  0000000c  85 c0                    test %eax, %eax      [x86_64.alu-rm-r.asz-absent.opsz-absent.rex-absent.reg]
  0000000e  7e 21                    jle.d8 49            [x86_64.jcc-rel.d8]
  00000010  b8 00 00 00 00           movl $0, %eax        [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
  00000015  ba 00 00 00 00           movl $0, %edx        [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
  0000001a  66 0f 1f 44 00 00        .balign 8            [padding]
  00000020  83 c2 07                 addl $7, %edx        [x86_64.alu-rm-imm8.asz-absent.opsz-absent.rex-absent.reg]
  00000023  83 c0 01                 addl $1, %eax        [x86_64.alu-rm-imm8.asz-absent.opsz-absent.rex-absent.reg]
  00000026  8b 4c 24 fc              movl -4(%rsp), %ecx  [x86_64.mov-r-rm.asz-absent.opsz-absent.rex-absent.sib-disp8]
  0000002a  39 c1                    cmpl %eax, %ecx      [x86_64.alu-rm-r.asz-absent.opsz-absent.rex-absent.reg]
  0000002c  7f f2                    jg.d8 32             [x86_64.jcc-rel.d8]
  0000002e  89 d0                    movl %edx, %eax      [x86_64.mov-rm-r.asz-absent.opsz-absent.rex-absent.reg]
  00000030  c3                       ret                  [x86_64.ret]
  00000031  ba 00 00 00 00           movl $0, %edx        [x86_64.mov-r-imm.asz-absent.opsz-absent.rex-absent]
  00000036  eb f6                    jmp.d8 46            [x86_64.jmp-rel.d8]
  ########## x86_64 loop codec
  alt x86_64
    [-2 cost=0] alu-acc-imm8     alu-acc-imm8(){prefixes alu-acc-imm8-opcode imm:8s}
    [-1 cost=0] alu-acc-imm      alu-acc-imm(){prefixes alu-acc-imm-opcode imm-sym32}
    [0 cost=0] alu-rm-imm8      alu-rm-imm8(){prefixes 10000011 modrm imm}
    [1 cost=0] alu-rm-imm32     alu-rm-imm32(){prefixes 10000001 modrm imm-sym32}
    [2 cost=0] mov-r-imm        mov-r-imm(){prefixes 10111 reg:3u imm-sym32}
    [5 cost=0] mov-rm-r         mov-rm-r(){prefixes 10001001 modrm}
    [6 cost=0] mov-r-rm         mov-r-rm(){prefixes 10001011 modrm}
    [7 cost=0] lea              lea(){prefixes 10001101 modrm}
    [8 cost=0] ret              ret(){11000011}
    [9 cost=0] mov-rm-imm8      mov-rm-imm8(){prefixes 11000110 modrm imm8:8s}
    [10 cost=0] alu-rm-r         alu-rm-r(){prefixes alu-rm-r-op modrm}
    [11 cost=0] pop-r            pop-r(){prefixes 01011 reg:3u}
    [12 cost=0] jmp-rm           jmp-rm(){prefixes 11111111 modrm}
    [13 cost=0] ud2              ud2(){0000111100001011}
    [14 cost=0] call-rel32       call-rel32(){11101000 le32}
    [15 cost=0] imul-r-rm        imul-r-rm(){prefixes 0000111110101111 modrm}
    [16 cost=0] cmov-r-rm        cmov-r-rm(){prefixes 000011110100 cc modrm}
    [17 cost=0] jmp-rel          relax jmp
                                   d8               jmp.d8(){11101011 <target:8@0 pcrel8-branch>}
                                   d32              jmp.d32(){11101001 le32}
    [18 cost=0] jcc-rel          relax jcc
                                   d8               jcc.d8(){0111 cc <target:8@0 pcrel8-branch>}
                                   d32              jcc.d32(){000011111000 cc le32}
    [19 cost=0] push-r           push-r(){prefixes 01010 reg:3u}
    [20 cost=0] unary-rm         unary-rm(){prefixes 11110111 modrm}
    [21 cost=0] alu-r-rm         alu-r-rm(){prefixes alu-r-rm-op modrm}
    [22 cost=0] shift1-rm        shift1-rm(){prefixes 11010001 modrm}
    [24 cost=0] sse-binop-f2     sse-binop-f2(){asz 11110010 rex 00001111 sse-binop-f2-op modrm}
    [25 cost=0] sse-binop-f3     sse-binop-f3(){asz 11110011 rex 00001111 sse-binop-f3-op modrm}
    [26 cost=0] sse-binop-66     sse-binop-66(){asz 01100110 rex 00001111 sse-binop-66-op modrm}
    [27 cost=0] sse-binop-none   sse-binop-none(){prefixes 00001111 sse-binop-none-op modrm}
    [28 cost=0] sse-movsd-load   sse-movsd-load(){asz 11110010 rex 0000111100010000 modrm}
    [29 cost=0] sse-movss-load   sse-movss-load(){asz 11110011 rex 0000111100010000 modrm}
    [30 cost=0] sse-movsd-store  sse-movsd-store(){asz 11110010 rex 0000111100010001 modrm}
    [31 cost=0] sse-movss-store  sse-movss-store(){asz 11110011 rex 0000111100010001 modrm}
    [32 cost=0] cvtsi2sd-r-rm    cvtsi2sd-r-rm(){asz 11110010 rex 0000111100101010 modrm}
    [33 cost=0] cvtsi2ss-r-rm    cvtsi2ss-r-rm(){asz 11110011 rex 0000111100101010 modrm}
    [34 cost=0] cvttsd2si-r-rm   cvttsd2si-r-rm(){asz 11110010 rex 0000111100101100 modrm}
    [35 cost=0] shift-imm-rm     shift-imm-rm(){prefixes 11000001 modrm imm8:8u}
    [36 cost=0] shift-cl-rm      shift-cl-rm(){prefixes 11010011 modrm}
    [37 cost=0] setcc-rm         setcc-rm(){prefixes 000011111001 cc modrm}
    [38 cost=0] movzx-b-r-rm     movzx-b-r-rm(){prefixes 0000111110110110 modrm}
    [39 cost=0] movzx-w-r-rm     movzx-w-r-rm(){prefixes 0000111110110111 modrm}
    [40 cost=0] movsx-b-r-rm     movsx-b-r-rm(){prefixes 0000111110111110 modrm}
    [41 cost=0] movsx-w-r-rm     movsx-w-r-rm(){prefixes 0000111110111111 modrm}
    [42 cost=0] movsxd-r-rm      movsxd-r-rm(){prefixes 01100011 modrm}
    [43 cost=0] imul-r-rm-imm8   imul-r-rm-imm8(){prefixes 01101011 modrm imm:8s}
    [44 cost=0] imul-r-rm-imm32  imul-r-rm-imm32(){prefixes 01101001 modrm le32}
    [45 cost=0] test-rm-imm      test-rm-imm(){prefixes 11110111 modrm le32}
    [46 cost=0] mov-rm-r8        mov-rm-r8(){prefixes 10001000 modrm}
    [47 cost=0] mov-r-rm8        mov-r-rm8(){prefixes 10001010 modrm}
    [48 cost=0] push-imm8        push-imm8(){01101010 imm}
    [49 cost=0] push-imm32       push-imm32(){01101000 imm-sym32}
    [50 cost=0] fldl             fldl(){prefixes 11011101 modrm}
    [51 cost=0] fstpl            fstpl(){prefixes 11011101 modrm}
    [52 cost=0] fstps            fstps(){prefixes 11011001 modrm}
    [53 cost=0] shld-imm-rm      shld-imm-rm(){prefixes 0000111110100100 modrm imm8:8u}
    [54 cost=0] flds             flds(){prefixes 11011001 modrm}
    [55 cost=0] fucomp           fucomp(){1101110111101001}
    [56 cost=0] fildll           fildll(){prefixes 11011111 modrm}
    [57 cost=0] fadds            fadds(){prefixes 11011000 modrm}
    [58 cost=0] fnstcw           fnstcw(){prefixes 11011001 modrm}
    [59 cost=0] fldcw            fldcw(){prefixes 11011001 modrm}
    [60 cost=0] fistpll          fistpll(){prefixes 11011111 modrm}
    [61 cost=0] fsubs            fsubs(){prefixes 11011000 modrm}
    [62 cost=0] fnstsw           fnstsw(){1101111111100000}
    [63 cost=0] sahf             sahf(){10011110}
    [64 cost=0] mov-r-imm8       mov-r-imm8(){prefixes 10110 reg:3u imm:8u}
    [65 cost=0] fadd-st0-x87     fadd-st0-x87(){11011000 11 000 st:3u}
    [66 cost=0] alu-rm-imm8-byte alu-rm-imm8-byte(){prefixes 10000000 modrm imm:8s}
    [67 cost=0] vex-scalar-f2-rrr vex-scalar-f2-rrr(){11000101 vex-byte2:8u vex-scalar-f2-op modrm}
    [68 cost=0] vex-scalar-f3-rrr vex-scalar-f3-rrr(){11000101 vex-byte2:8u vex-scalar-f3-op modrm}
    [69 cost=0] vex-scalar-none-rrr vex-scalar-none-rrr(){11000101 vex-byte2:8u vex-scalar-none-op modrm}
    [70 cost=0] vex-scalar-66-rrr vex-scalar-66-rrr(){11000101 vex-byte2:8u vex-scalar-66-op modrm}
    [71 cost=0] vex-unop-none    vex-unop-none(){11000101 vex-byte2:8u vex-unop-none-op modrm}
    [72 cost=0] vex-unop-66      vex-unop-66(){11000101 vex-byte2:8u vex-unop-66-op modrm}
    [73 cost=0] sse-binop-imm-f2 sse-binop-imm-f2(){asz 11110010 rex 00001111 sse-binop-imm-f2-op modrm imm8:8u}
    [74 cost=0] sse-binop-imm-f3 sse-binop-imm-f3(){asz 11110011 rex 00001111 sse-binop-imm-f3-op modrm imm8:8u}
    [75 cost=0] sse-binop-imm-66 sse-binop-imm-66(){asz 01100110 rex 00001111 sse-binop-imm-66-op modrm imm8:8u}
    [76 cost=0] sse-binop-imm-none sse-binop-imm-none(){prefixes 00001111 sse-binop-imm-none-op modrm imm8:8u}
    [77 cost=0] vex-binop-imm-f2 vex-binop-imm-f2(){11000101 vex-byte2:8u vex-binop-imm-f2-op modrm imm8:8u}
    [78 cost=0] vex-binop-imm-f3 vex-binop-imm-f3(){11000101 vex-byte2:8u vex-binop-imm-f3-op modrm imm8:8u}
    [79 cost=0] vex-binop-imm-none vex-binop-imm-none(){11000101 vex-byte2:8u vex-binop-imm-none-op modrm imm8:8u}
    [80 cost=0] vex-binop-imm-66 vex-binop-imm-66(){11000101 vex-byte2:8u vex-binop-imm-66-op modrm imm8:8u}
    [81 cost=0] vex-unop-imm-f2  vex-unop-imm-f2(){11000101 vex-byte2:8u vex-unop-imm-f2-op modrm imm8:8u}
    [82 cost=0] vex-unop-imm-f3  vex-unop-imm-f3(){11000101 vex-byte2:8u vex-unop-imm-f3-op modrm imm8:8u}
    [83 cost=0] vex-unop-imm-66  vex-unop-imm-66(){11000101 vex-byte2:8u vex-unop-imm-66-op modrm imm8:8u}
    [84 cost=0] movd-load-r-rm   movd-load-r-rm(){asz 01100110 rex 0000111101101110 modrm}
    [85 cost=0] movd-store-r-rm  movd-store-r-rm(){asz 01100110 rex 0000111101111110 modrm}
    [86 cost=0] vex-movd-load-r-rm vex-movd-load-r-rm(){11000101 vex-byte2:8u 01101110 modrm}
    [87 cost=0] vex-movd-store-r-rm vex-movd-store-r-rm(){11000101 vex-byte2:8u 01111110 modrm}
    [88 cost=0] movdqa-load      movdqa-load(){asz 01100110 rex 0000111101101111 modrm}
    [89 cost=0] movdqu-load      movdqu-load(){asz 11110011 rex 0000111101101111 modrm}
    [90 cost=0] movdqa-store     movdqa-store(){asz 01100110 rex 0000111101111111 modrm}
    [91 cost=0] movdqu-store     movdqu-store(){asz 11110011 rex 0000111101111111 modrm}
    [92 cost=0] vex-unop-f3      vex-unop-f3(){11000101 vex-byte2:8u vex-unop-f3-op modrm}
    [93 cost=0] sse-pinsrw       sse-pinsrw(){asz 01100110 rex 0000111111000100 modrm imm8:8u}
    [94 cost=0] sse-pextrw       sse-pextrw(){asz 01100110 rex 0000111111000101 modrm imm8:8u}
    [95 cost=0] vex-pinsrw       vex-pinsrw(){11000101 vex-byte2:8u 11000100 modrm imm8:8u}
    [96 cost=0] vex-pextrw       vex-pextrw(){11000101 vex-byte2:8u 11000101 modrm imm8:8u}
    [97 cost=0] sse-movmsk-66    sse-movmsk-66(){asz 01100110 rex 00001111 sse-movmsk-66-op modrm}
    [98 cost=0] sse-movmsk-none  sse-movmsk-none(){prefixes 00001111 sse-movmsk-none-op modrm}
    [99 cost=0] vex-movmsk-66    vex-movmsk-66(){11000101 vex-byte2:8u vex-movmsk-66-op modrm}
    [100 cost=0] vex-movmsk-none  vex-movmsk-none(){11000101 vex-byte2:8u vex-movmsk-none-op modrm}
    [101 cost=0] xmm-shift-imm-w  xmm-shift-imm-w(){asz 01100110 rex 00001111 01110001 modrm imm8:8u}
    [102 cost=0] xmm-shift-imm-d  xmm-shift-imm-d(){asz 01100110 rex 00001111 01110010 modrm imm8:8u}
    [103 cost=0] xmm-shift-imm-q  xmm-shift-imm-q(){asz 01100110 rex 00001111 01110011 modrm imm8:8u}
    [104 cost=0] vex-shift-imm-w  vex-shift-imm-w(){11000101 vex-byte2:8u 01110001 modrm imm8:8u}
    [105 cost=0] vex-shift-imm-d  vex-shift-imm-d(){11000101 vex-byte2:8u 01110010 modrm imm8:8u}
    [106 cost=0] vex-shift-imm-q  vex-shift-imm-q(){11000101 vex-byte2:8u 01110011 modrm imm8:8u}
    [107 cost=0] sse-binop-0f38-66 sse-binop-0f38-66(){asz 01100110 rex 00001111 00111000 sse-binop-0f38-op modrm}
    [108 cost=0] sse-binop-imm-0f3a-66 sse-binop-imm-0f3a-66(){asz 01100110 rex 00001111 00111010 sse-binop-imm-0f3a-op modrm imm8:8u}
    [109 cost=0] sse-gpr-imm-0f3a-66 sse-gpr-imm-0f3a-66(){asz 01100110 rex 00001111 00111010 sse-gpr-imm-0f3a-op modrm imm8:8u}
    [110 cost=0] vex3-map2-66-rrr vex3-map2-66-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-66-op modrm}
    [111 cost=0] vex3-map3-66-imm-rrr vex3-map3-66-imm-rrr(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map3-66-op modrm imm8:8u}
    [112 cost=0] vex3-map2-66-unop vex3-map2-66-unop(){11000100 vex3-byte1:8u vex3-byte2:8u vex3-map2-unop-op modrm}
    [113 cost=0] evex-ps-rrr      evex-ps-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-ps-op modrm}
    [114 cost=0] evex-pd-rrr      evex-pd-rrr(){01100010 evex-p0:8u evex-p1:8u evex-p2:8u evex-pd-op modrm}
    [200 cost=0] short-loop       relax loop
                                    d8               loop.d8(){11100010 <target:8@0 pcrel8-branch>}
    [201 cost=0] short-loope      relax loope
                                    d8               loope.d8(){11100001 <target:8@0 pcrel8-branch>}
    [202 cost=0] short-loopne     relax loopne
                                    d8               loopne.d8(){11100000 <target:8@0 pcrel8-branch>}
    [203 cost=0] short-jrcxz      relax jrcxz
                                    d8               jrcxz.d8(){11100011 <target:8@0 pcrel8-branch>}
    [204 cost=0] short-jecxz      relax jecxz
                                    d8               jecxz.d8(){0110011111100011 <target:8@0 pcrel8-branch>}
    [300 cost=0] mov-r64-simm32   mov-r64-simm32(){prefixes 11000111 modrm le32}
    [301 cost=0] mov-r64-imm64    mov-r64-imm64(){prefixes 10111 reg:3u le64}
    [302 cost=0] bswap-r          bswap-r(){prefixes 00001111 11001 reg:3u}
  prefixes(){asz opsz rex}
  alu-acc-imm8-opcode(){opcode:8u}
  alu-acc-imm-opcode(){opcode:8u}
  imm-sym32(){le32}
  alt modrm
    [0 cost=0] reg              modrm-reg(){11 reg:3u rm:3u}
    [1 cost=0] sib-nobase-disp32 modrm-sib-nobase-disp32(){00 reg:3u 100 scale:2u index:3u 101 disp-sym}
    [2 cost=0] sib-disp0        modrm-sib-disp0(){00 reg:3u 100 sib disp-none}
    [3 cost=0] sib-disp8        modrm-sib-disp8(){01 reg:3u 100 sib disp-c8}
    [4 cost=0] sib-disp32       modrm-sib-disp32(){10 reg:3u 100 sib disp-c32}
    [5 cost=0] disp32-norm      modrm-disp32-norm(){00 reg:3u 101 disp-sym}
    [6 cost=0] base-disp0       modrm-base-disp0(){00 reg:3u rm:3u disp-none}
    [7 cost=0] base-disp8       modrm-base-disp8(){01 reg:3u rm:3u disp-c8}
    [8 cost=0] base-disp32      modrm-base-disp32(){10 reg:3u rm:3u disp-sym}
  imm(){imm:8s}
  alu-rm-r-op[9]{opcode:8u}
  le32(){<target:32@0 pcrel32-call>}
  cc[16]{cc:4u}
  alu-r-rm-op[8]{opcode:8u}
  alt asz
    [0 cost=0] asz-present      asz-present(){01100111}
    [1 cost=0] asz-absent       asz-absent(){()}
  alt rex
    [0 cost=0] rex-present      rex-present(){0100 wrxb:4u}
    [1 cost=0] rex-absent       rex-absent(){()}
  sse-binop-f2-op[8]{opcode:8u}
  sse-binop-f3-op[9]{opcode:8u}
  sse-binop-66-op[66]{opcode:8u}
  sse-binop-none-op[19]{opcode:8u}
  vex-scalar-f2-op[7]{opcode:8u}
  vex-scalar-f3-op[7]{opcode:8u}
  vex-scalar-none-op[12]{opcode:8u}
  vex-scalar-66-op[58]{opcode:8u}
  vex-unop-none-op[7]{opcode:8u}
  vex-unop-66-op[8]{opcode:8u}
  sse-binop-imm-f2-op[2]{opcode:8u}
  sse-binop-imm-f3-op[2]{opcode:8u}
  sse-binop-imm-66-op[3]{opcode:8u}
  sse-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-f2-op[1]{opcode:8u}
  vex-binop-imm-f3-op[1]{opcode:8u}
  vex-binop-imm-none-op[2]{opcode:8u}
  vex-binop-imm-66-op[2]{opcode:8u}
  vex-unop-imm-f2-op[1]{opcode:8u}
  vex-unop-imm-f3-op[1]{opcode:8u}
  vex-unop-imm-66-op[1]{opcode:8u}
  vex-unop-f3-op[2]{opcode:8u}
  sse-movmsk-66-op[2]{opcode:8u}
  sse-movmsk-none-op[1]{opcode:8u}
  vex-movmsk-66-op[2]{opcode:8u}
  vex-movmsk-none-op[1]{opcode:8u}
  sse-binop-0f38-op[46]{opcode:8u}
  sse-binop-imm-0f3a-op[12]{opcode:8u}
  sse-gpr-imm-0f3a-op[5]{opcode:8u}
  vex3-map2-66-op[25]{opcode:8u}
  vex3-map3-66-op[10]{opcode:8u}
  vex3-map2-unop-op[18]{opcode:8u}
  evex-ps-op[8]{opcode:8u}
  evex-pd-op[8]{opcode:8u}
  le64(){imm64:64u}
  alt opsz
    [0 cost=0] opsz-present     opsz-present(){01100110}
    [1 cost=0] opsz-absent      opsz-absent(){()}
  disp-sym(){le32}
  sib(){scale:2u index:3u base:3u}
  disp-none(){no-disp}
  disp-c8(){disp8:8s}
  disp-c32(){le32}
  no-disp(){()}
  ########## arm loop tokens
  1 5 directive .arch
  7 5 ident armv7
  12 1 minus -
  13 1 ident a
  14 1 eol \n
  16 4 directive .fpu
  21 5 ident vfpv3
  26 1 minus -
  27 3 ident d16
  30 1 eol \n
  32 15 directive .eabi_attribute
  48 2 int 28
  50 1 comma ,
  52 1 int 1
  53 1 eol \n
  55 15 directive .eabi_attribute
  71 2 int 20
  73 1 comma ,
  75 1 int 1
  76 1 eol \n
  78 15 directive .eabi_attribute
  94 2 int 21
  96 1 comma ,
  98 1 int 1
  99 1 eol \n
  101 15 directive .eabi_attribute
  117 2 int 23
  119 1 comma ,
  121 1 int 3
  122 1 eol \n
  124 15 directive .eabi_attribute
  140 2 int 24
  142 1 comma ,
  144 1 int 1
  145 1 eol \n
  147 15 directive .eabi_attribute
  163 2 int 25
  165 1 comma ,
  167 1 int 1
  168 1 eol \n
  170 15 directive .eabi_attribute
  186 2 int 26
  188 1 comma ,
  190 1 int 2
  191 1 eol \n
  193 15 directive .eabi_attribute
  209 2 int 30
  211 1 comma ,
  213 1 int 1
  214 1 eol \n
  216 15 directive .eabi_attribute
  232 2 int 34
  234 1 comma ,
  236 1 int 1
  237 1 eol \n
  239 15 directive .eabi_attribute
  255 2 int 18
  257 1 comma ,
  259 1 int 4
  260 1 eol \n
  262 5 directive .file
  268 8 string "loop.c"
  276 1 eol \n
  278 5 directive .text
  283 1 eol \n
  285 6 directive .align
  292 1 int 2
  293 1 eol \n
  295 7 directive .global
  303 14 ident asm_test_entry
  317 1 eol \n
  319 7 directive .syntax
  327 7 ident unified
  334 1 eol \n
  336 4 directive .arm
  340 1 eol \n
  342 5 directive .type
  348 14 ident asm_test_entry
  362 1 comma ,
  364 1 percent %
  365 8 ident function
  373 1 eol \n
  374 14 ident asm_test_entry
  388 1 colon :
  389 1 eol \n
  425 1 eol \n
  470 1 eol \n
  504 1 eol \n
  506 3 ident sub
  510 2 ident sp
  512 1 comma ,
  514 2 ident sp
  516 1 comma ,
  518 1 immsigil #
  519 1 int 8
  520 1 eol \n
  522 3 ident mov
  526 2 ident r3
  528 1 comma ,
  530 1 immsigil #
  531 1 int 6
  532 1 eol \n
  534 3 ident str
  538 2 ident r3
  540 1 comma ,
  542 1 lbracket [
  543 2 ident sp
  545 1 comma ,
  547 1 immsigil #
  548 1 int 4
  549 1 rbracket ]
  550 1 eol \n
  552 3 ident ldr
  556 2 ident r3
  558 1 comma ,
  560 1 lbracket [
  561 2 ident sp
  563 1 comma ,
  565 1 immsigil #
  566 1 int 4
  567 1 rbracket ]
  568 1 eol \n
  570 3 ident cmp
  574 2 ident r3
  576 1 comma ,
  578 1 immsigil #
  579 1 int 0
  580 1 eol \n
  582 3 ident ble
  586 3 directive .L4
  589 1 eol \n
  591 3 ident mov
  595 2 ident r3
  597 1 comma ,
  599 1 immsigil #
  600 1 int 0
  601 1 eol \n
  603 3 ident mov
  607 2 ident r0
  609 1 comma ,
  611 2 ident r3
  613 1 eol \n
  614 3 directive .L3
  617 1 colon :
  618 1 eol \n
  620 3 ident add
  624 2 ident r0
  626 1 comma ,
  628 2 ident r0
  630 1 comma ,
  632 1 immsigil #
  633 1 int 7
  634 1 eol \n
  636 3 ident add
  640 2 ident r3
  642 1 comma ,
  644 2 ident r3
  646 1 comma ,
  648 1 immsigil #
  649 1 int 1
  650 1 eol \n
  652 3 ident ldr
  656 2 ident r2
  658 1 comma ,
  660 1 lbracket [
  661 2 ident sp
  663 1 comma ,
  665 1 immsigil #
  666 1 int 4
  667 1 rbracket ]
  668 1 eol \n
  670 3 ident cmp
  674 2 ident r2
  676 1 comma ,
  678 2 ident r3
  680 1 eol \n
  682 3 ident bgt
  686 3 directive .L3
  689 1 eol \n
  690 3 directive .L1
  693 1 colon :
  694 1 eol \n
  696 3 ident add
  700 2 ident sp
  702 1 comma ,
  704 2 ident sp
  706 1 comma ,
  708 1 immsigil #
  709 1 int 8
  710 1 eol \n
  723 1 eol \n
  725 2 ident bx
  728 2 ident lr
  730 1 eol \n
  731 3 directive .L4
  734 1 colon :
  735 1 eol \n
  737 3 ident mov
  741 2 ident r0
  743 1 comma ,
  745 1 immsigil #
  746 1 int 0
  747 1 eol \n
  749 1 ident b
  751 3 directive .L1
  754 1 eol \n
  756 5 directive .size
  762 14 ident asm_test_entry
  776 1 comma ,
  778 1 dot .
  779 1 minus -
  780 14 ident asm_test_entry
  794 1 eol \n
  796 6 directive .ident
  803 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  835 1 eol \n
  837 8 directive .section
  846 9 directive .note.GNU
  855 1 minus -
  856 5 ident stack
  861 1 comma ,
  862 2 string ""
  864 1 comma ,
  865 1 percent %
  866 8 ident progbits
  874 1 eol \n
  875 0 eof 
  ########## arm loop source ast
  source loop
  directive .arch [armv7-a]
  directive .fpu [vfpv3-d16]
  directive .eabi_attribute [28] [1]
  directive .eabi_attribute [20] [1]
  directive .eabi_attribute [21] [1]
  directive .eabi_attribute [23] [3]
  directive .eabi_attribute [24] [1]
  directive .eabi_attribute [25] [1]
  directive .eabi_attribute [26] [2]
  directive .eabi_attribute [30] [1]
  directive .eabi_attribute [34] [1]
  directive .eabi_attribute [18] [4]
  directive .file ["loop.c"]
  directive .text
  directive .align [2]
  directive .global [asm_test_entry]
  directive .syntax [unified]
  directive .arm
  directive .type [asm_test_entry] [%function]
  label asm_test_entry
  insn sub sp, sp, #8
  insn mov r3, #6
  insn str r3, [sp, #4]
  insn ldr r3, [sp, #4]
  insn cmp r3, #0
  insn ble .L4
  insn mov r3, #0
  insn mov r0, r3
  label .L3
  insn add r0, r0, #7
  insn add r3, r3, #1
  insn ldr r2, [sp, #4]
  insn cmp r2, r3
  insn bgt .L3
  label .L1
  insn add sp, sp, #8
  insn bx lr
  label .L4
  insn mov r0, #0
  insn b .L1
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [%progbits]
  ########## arm loop normalized ast
  normalized loop
  target-state .arch armv7-a
  target-state .fpu vfpv3-d16
  target-state .eabi_attribute 28, 1
  target-state .eabi_attribute 20, 1
  target-state .eabi_attribute 21, 1
  target-state .eabi_attribute 23, 3
  target-state .eabi_attribute 24, 1
  target-state .eabi_attribute 25, 1
  target-state .eabi_attribute 26, 2
  target-state .eabi_attribute 30, 1
  target-state .eabi_attribute 34, 1
  target-state .eabi_attribute 18, 4
  section .text r-x
  align 4
  globl asm_test_entry
  target-state .syntax unified
  target-state .arm 
  type asm_test_entry function
  label asm_test_entry
  sub sp, sp, #8
  mov r3, #6
  str r3, [sp, #4]
  ldr r3, [sp, #4]
  cmp r3, #0
  ble .L4
  mov r3, #0
  mov r0, r3
  label .L3
  add r0, r0, #7
  add r3, r3, #1
  ldr r2, [sp, #4]
  cmp r2, r3
  bgt .L3
  label .L1
  add sp, sp, #8
  bx lr
  label .L4
  mov r0, #0
  b .L1
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## arm loop lowered ast
  lowered loop
  section .text r-x align=4
    align 4
    label asm_test_entry
    bytes 08 d0 4d e2              [arm.dp-imm]
    bytes 06 30 a0 e3              [arm.dp-imm]
    bytes 04 30 8d e5              [arm.ldst-imm]
    bytes 04 30 9d e5              [arm.ldst-imm]
    bytes 00 00 53 e3              [arm.dp-imm]
    bytes 00 00 00 da              [arm.b]
      @0/4B pc+8 target pcrel-b26 s24 [0+24@0] = .L4
    bytes 00 30 a0 e3              [arm.dp-imm]
    bytes 03 00 a0 e1              [arm.dp-reg]
    label .L3
    bytes 07 00 80 e2              [arm.dp-imm]
    bytes 01 30 83 e2              [arm.dp-imm]
    bytes 04 20 9d e5              [arm.ldst-imm]
    bytes 03 00 52 e1              [arm.dp-reg]
    bytes 00 00 00 ca              [arm.b]
      @0/4B pc+8 target pcrel-b26 s24 [0+24@0] = .L3
    label .L1
    bytes 08 d0 8d e2              [arm.dp-imm]
    bytes 1e ff 2f e1              [arm.bx]
    label .L4
    bytes 00 00 a0 e3              [arm.dp-imm]
    bytes 00 00 00 ea              [arm.b]
      @0/4B pc+8 target pcrel-b26 s24 [0+24@0] = .L1
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .L3 notype in .text
  local .L1 notype in .text
  local .L4 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## arm loop plan
  segment .text size=68 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## arm loop image
  section .text address=0x0 size=68 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=68
  ########## arm loop bytes
  08 d0 4d e2 06 30 a0 e3 04 30 8d e5 04 30 9d e5 00 00 53 e3 08 00 00 da 00 30 a0 e3 03 00 a0 e1 07 00 80 e2 01 30 83 e2 04 20 9d e5 03 00 52 e1 fa ff ff ca 08 d0 8d e2 1e ff 2f e1 00 00 a0 e3 fb ff ff ea
  ########## arm loop disasm canonical
  	sub sp, sp, #8
  	mov r3, #6
  	str r3, [sp, #4]
  	ldr r3, [sp, #4]
  	cmp r3, #0
  	ble 60
  	mov r3, #0
  	mov r0, r3
  	add r0, r0, #7
  	add r3, r3, #1
  	ldr r2, [sp, #4]
  	cmp r2, r3
  	bgt 32
  	add sp, sp, #8
  	bx lr
  	mov r0, #0
  	b 52
  ########## arm loop disasm diagnostic
  00000000  08 d0 4d e2  sub sp, sp, #8    [arm.dp-imm]
  00000004  06 30 a0 e3  mov r3, #6        [arm.dp-imm]
  00000008  04 30 8d e5  str r3, [sp, #4]  [arm.ldst-imm]
  0000000c  04 30 9d e5  ldr r3, [sp, #4]  [arm.ldst-imm]
  00000010  00 00 53 e3  cmp r3, #0        [arm.dp-imm]
  00000014  08 00 00 da  ble 60            [arm.b]
  00000018  00 30 a0 e3  mov r3, #0        [arm.dp-imm]
  0000001c  03 00 a0 e1  mov r0, r3        [arm.dp-reg]
  00000020  07 00 80 e2  add r0, r0, #7    [arm.dp-imm]
  00000024  01 30 83 e2  add r3, r3, #1    [arm.dp-imm]
  00000028  04 20 9d e5  ldr r2, [sp, #4]  [arm.ldst-imm]
  0000002c  03 00 52 e1  cmp r2, r3        [arm.dp-reg]
  00000030  fa ff ff ca  bgt 32            [arm.b]
  00000034  08 d0 8d e2  add sp, sp, #8    [arm.dp-imm]
  00000038  1e ff 2f e1  bx lr             [arm.bx]
  0000003c  00 00 a0 e3  mov r0, #0        [arm.dp-imm]
  00000040  fb ff ff ea  b 52              [arm.b]
  ########## arm loop codec
  alt arm
    [-1 cost=0] nop              nop(){cond 0011001000001111000000000000}
    [0 cost=0] bx               bx(){cond 000100101111111111110001 rm}
    [1 cost=0] movw             movw(){cond 00110000 <imm:4@12 movw-abs-nc> rd <imm:12@0 movw-abs-nc>}
    [2 cost=0] movt             movt(){cond 00110100 <imm:4@12 movt-abs> rd <imm:12@0 movt-abs>}
    [3 cost=0] dp-imm           dp-imm(){cond 001 dp:4u s:1u rn rd modimm}
    [4 cost=0] dp-reg           dp-reg(){cond 000 dp:4u s:1u rn rd shift-amount:5u shift-kind:2u 0 rm}
    [5 cost=0] ldst-imm         ldst-imm(){cond 010 p:1u u:1u b:1u w:1u l:1u rn rt imm12:12u}
    [6 cost=0] ldst-reg         ldst-reg(){cond 011 1 u:1u b:1u 0 l:1u rn rt shift-amount:5u shift-kind:2u 0 rm}
    [7 cost=0] udf              udf(){1110 01111111 imm12:12u 1111 imm4:4u}
    [8 cost=0] bl               bl(){cond 1011 <target:24@0 pcrel-call>}
    [9 cost=0] b                b(){cond 1010 <target:24@0 pcrel-b26>}
    [10 cost=0] mul              mul(){cond 0000000 s:1u rd 0000 rm 1001 rn}
    [11 cost=0] mla              mla(){cond 0000001 s:1u rd ra rm 1001 rn}
    [12 cost=0] vmov-reg-d       vmov-reg-d(){cond 11101 D:1u 11 0000 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [13 cost=0] vmov-reg-s       vmov-reg-s(){cond 11101 D:1u 11 0000 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [14 cost=0] vmov-imm-d       vmov-imm-d(){cond 11101 D:1u 11 imm4H:4u Vd:4u 1011 0000 imm4L:4u}
    [15 cost=0] vmov-imm-s       vmov-imm-s(){cond 11101 D:1u 11 imm4H:4u Vd:4u 1010 0000 imm4L:4u}
    [16 cost=0] vmov-to-single   vmov-to-single(){cond 1110000 0 Vn:4u rt 1010 N:1u 0010000}
    [17 cost=0] vmov-from-single vmov-from-single(){cond 1110000 1 Vn:4u rt 1010 N:1u 0010000}
    [18 cost=0] vmov-to-double   vmov-to-double(){cond 1100010 0 rt2 rt 1011 00 D:1u 1 Vm:4u}
    [19 cost=0] vmov-from-double vmov-from-double(){cond 1100010 1 rt2 rt 1011 00 D:1u 1 Vm:4u}
    [20 cost=0] v3-d             v3-d(){cond 1110 b23:1u D:1u b21:1u b20:1u Vn:4u Vd:4u 1011 N:1u b6:1u M:1u 0 Vm:4u}
    [21 cost=0] v3-s             v3-s(){cond 1110 b23:1u D:1u b21:1u b20:1u Vn:4u Vd:4u 1010 N:1u b6:1u M:1u 0 Vm:4u}
    [22 cost=0] vneg-d           vneg-d(){cond 11101 D:1u 11 0001 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [23 cost=0] vneg-s           vneg-s(){cond 11101 D:1u 11 0001 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [24 cost=0] vcmp-reg-d       vcmp-reg-d(){cond 11101 D:1u 11 0100 Vd:4u 1011 01 M:1u 0 Vm:4u}
    [25 cost=0] vcmp-reg-s       vcmp-reg-s(){cond 11101 D:1u 11 0100 Vd:4u 1010 01 M:1u 0 Vm:4u}
    [26 cost=0] vcmp-zero-d      vcmp-zero-d(){cond 11101 D:1u 11 0101 Vd:4u 1011 01 000000}
    [27 cost=0] vcmp-zero-s      vcmp-zero-s(){cond 11101 D:1u 11 0101 Vd:4u 1010 01 000000}
    [28 cost=0] vmrs             vmrs(){cond 1110111100011111101000010000}
    [29 cost=0] vcvt-f32-f64     vcvt-f32-f64(){cond 11101 D:1u 11 0111 Vd:4u 1011 11 M:1u 0 Vm:4u}
    [30 cost=0] vcvt-f64-f32     vcvt-f64-f32(){cond 11101 D:1u 11 0111 Vd:4u 1010 11 M:1u 0 Vm:4u}
    [31 cost=0] vcvt-f32-s32     vcvt-f32-s32(){cond 11101 D:1u 11 1000 Vd:4u 1010 1 1 M:1u 0 Vm:4u}
    [32 cost=0] vcvt-f64-s32     vcvt-f64-s32(){cond 11101 D:1u 11 1000 Vd:4u 1011 1 1 M:1u 0 Vm:4u}
    [33 cost=0] vcvt-f64-u32     vcvt-f64-u32(){cond 11101 D:1u 11 1000 Vd:4u 1011 0 1 M:1u 0 Vm:4u}
    [34 cost=0] vcvt-s32-f64     vcvt-s32-f64(){cond 11101 D:1u 11 1101 Vd:4u 1011 11 M:1u 0 Vm:4u}
    [35 cost=0] vmem-d           vmem-d(){cond 1101 U:1u D:1u 0 L:1u rn Vd:4u 1011 imm8:8u}
    [36 cost=0] vmem-s           vmem-s(){cond 1101 U:1u D:1u 0 L:1u rn Vd:4u 1010 imm8:8u}
    [37 cost=0] push             push(){cond 100100101101 reglist:16u}
    [38 cost=0] ldm              ldm(){cond 1000101 l:1u rn reglist:16u}
    [39 cost=0] vpush            vpush(){cond 11010 D:1u 10 1101 Vd:4u 1011 imm8:8u}
    [40 cost=0] shift-reg        shift-reg(){cond 000 1101 0 0000 rd rs 0 shift-kind:2u 1 rm}
    [41 cost=0] vldr-lit-d       vldr-lit-d(){cond 1101 1 D:1u 0 1 1111 Vd:4u 1011 <target:8@0 pcrel-vldr8>}
    [42 cost=0] vldr-lit-s       vldr-lit-s(){cond 1101 1 D:1u 0 1 1111 Vd:4u 1010 <target:8@0 pcrel-vldr8>}
    [43 cost=0] umull            umull(){cond 00001 00 s:1u rdhi rdlo rm 1001 rn}
    [44 cost=0] ldst-x-imm       ldst-x-imm(){cond 000 p:1u u:1u 1 0 l:1u rn rt imm4h:4u 1 s:1u h:1u 1 imm4l:4u}
    [45 cost=0] ldst-x-reg       ldst-x-reg(){cond 000 p:1u u:1u 0 0 l:1u rn rt 0000 1 s:1u h:1u 1 rm}
    [46 cost=0] sbfx             sbfx(){cond 0111101 widthm1:5u rd lsb:5u 101 rn}
    [47 cost=0] clz              clz(){cond 00010110 1111 rd 1111 0001 rm}
    [48 cost=0] rev              rev(){cond 01101011 1111 rd 1111 0011 rm}
    [49 cost=0] rev16            rev16(){cond 01101011 1111 rd 1111 1011 rm}
  cond[16]{cond:4u}
  rm(){rm:4u}
  rd(){rd:4u}
  rn(){rn:4u}
  modimm(){rot:4u imm8:8u}
  rt(){rt:4u}
  ra(){ra:4u}
  rt2(){rt2:4u}
  rs(){rs:4u}
  rdhi(){rdhi:4u}
  rdlo(){rdlo:4u}
  ########## aarch64 loop tokens
  1 5 directive .arch
  7 5 ident armv8
  12 1 minus -
  13 1 ident a
  14 1 eol \n
  16 5 directive .file
  22 8 string "loop.c"
  30 1 eol \n
  32 5 directive .text
  37 1 eol \n
  39 6 directive .align
  46 1 int 2
  47 1 eol \n
  49 7 directive .global
  57 14 ident asm_test_entry
  71 1 eol \n
  73 5 directive .type
  79 14 ident asm_test_entry
  93 1 comma ,
  95 1 percent %
  96 8 ident function
  104 1 eol \n
  105 14 ident asm_test_entry
  119 1 colon :
  120 1 eol \n
  121 5 directive .LFB0
  126 1 colon :
  127 1 eol \n
  129 14 directive .cfi_startproc
  143 1 eol \n
  145 3 ident sub
  149 2 ident sp
  151 1 comma ,
  153 2 ident sp
  155 1 comma ,
  157 1 immsigil #
  158 2 int 16
  160 1 eol \n
  162 19 directive .cfi_def_cfa_offset
  182 2 int 16
  184 1 eol \n
  186 3 ident mov
  190 2 ident w0
  192 1 comma ,
  194 1 int 6
  195 1 eol \n
  197 3 ident str
  201 2 ident w0
  203 1 comma ,
  205 1 lbracket [
  206 2 ident sp
  208 1 comma ,
  210 2 int 12
  212 1 rbracket ]
  213 1 eol \n
  215 3 ident ldr
  219 2 ident w0
  221 1 comma ,
  223 1 lbracket [
  224 2 ident sp
  226 1 comma ,
  228 2 int 12
  230 1 rbracket ]
  231 1 eol \n
  233 3 ident cmp
  237 2 ident w0
  239 1 comma ,
  241 1 int 0
  242 1 eol \n
  244 3 ident ble
  248 3 directive .L4
  251 1 eol \n
  253 3 ident mov
  257 2 ident w1
  259 1 comma ,
  261 1 int 0
  262 1 eol \n
  264 3 ident mov
  268 2 ident w0
  270 1 comma ,
  272 1 int 0
  273 1 eol \n
  274 3 directive .L3
  277 1 colon :
  278 1 eol \n
  280 3 ident add
  284 2 ident w0
  286 1 comma ,
  288 2 ident w0
  290 1 comma ,
  292 1 int 7
  293 1 eol \n
  295 3 ident add
  299 2 ident w1
  301 1 comma ,
  303 2 ident w1
  305 1 comma ,
  307 1 int 1
  308 1 eol \n
  310 3 ident ldr
  314 2 ident w2
  316 1 comma ,
  318 1 lbracket [
  319 2 ident sp
  321 1 comma ,
  323 2 int 12
  325 1 rbracket ]
  326 1 eol \n
  328 3 ident cmp
  332 2 ident w2
  334 1 comma ,
  336 2 ident w1
  338 1 eol \n
  340 3 ident bgt
  344 3 directive .L3
  347 1 eol \n
  348 3 directive .L1
  351 1 colon :
  352 1 eol \n
  354 3 ident add
  358 2 ident sp
  360 1 comma ,
  362 2 ident sp
  364 1 comma ,
  366 2 int 16
  368 1 eol \n
  370 19 directive .cfi_remember_state
  389 1 eol \n
  391 19 directive .cfi_def_cfa_offset
  411 1 int 0
  412 1 eol \n
  414 3 ident ret
  417 1 eol \n
  418 3 directive .L4
  421 1 colon :
  422 1 eol \n
  424 18 directive .cfi_restore_state
  442 1 eol \n
  444 3 ident mov
  448 2 ident w0
  450 1 comma ,
  452 1 int 0
  453 1 eol \n
  455 1 ident b
  457 3 directive .L1
  460 1 eol \n
  462 12 directive .cfi_endproc
  474 1 eol \n
  475 5 directive .LFE0
  480 1 colon :
  481 1 eol \n
  483 5 directive .size
  489 14 ident asm_test_entry
  503 1 comma ,
  505 1 dot .
  506 1 minus -
  507 14 ident asm_test_entry
  521 1 eol \n
  523 6 directive .ident
  530 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  562 1 eol \n
  564 8 directive .section
  573 9 directive .note.GNU
  582 1 minus -
  583 5 ident stack
  588 1 comma ,
  589 2 string ""
  591 1 comma ,
  592 1 at @
  593 8 ident progbits
  601 1 eol \n
  602 0 eof 
  ########## aarch64 loop source ast
  source loop
  directive .arch [armv8-a]
  directive .file ["loop.c"]
  directive .text
  directive .align [2]
  directive .global [asm_test_entry]
  directive .type [asm_test_entry] [%function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn sub sp, sp, #16
  directive .cfi_def_cfa_offset [16]
  insn mov w0, 6
  insn str w0, [sp, #12]
  insn ldr w0, [sp, #12]
  insn cmp w0, 0
  insn ble .L4
  insn mov w1, 0
  insn mov w0, 0
  label .L3
  insn add w0, w0, 7
  insn add w1, w1, 1
  insn ldr w2, [sp, #12]
  insn cmp w2, w1
  insn bgt .L3
  label .L1
  insn add sp, sp, 16
  directive .cfi_remember_state
  directive .cfi_def_cfa_offset [0]
  insn ret
  label .L4
  directive .cfi_restore_state
  insn mov w0, 0
  insn b .L1
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## aarch64 loop normalized ast
  normalized loop
  target-state .arch armv8-a
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  sub sp, sp, #16
  mov w0, #6
  str w0, [sp, #12]
  ldr w0, [sp, #12]
  cmp w0, #0
  b.le .L4
  mov w1, #0
  mov w0, #0
  label .L3
  add w0, w0, #7
  add w1, w1, #1
  ldr w2, [sp, #12]
  cmp w2, w1
  b.gt .L3
  label .L1
  add sp, sp, #16
  ret
  label .L4
  mov w0, #0
  b .L1
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## aarch64 loop lowered ast
  lowered loop
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes ff 43 00 d1              [aarch64.sub-imm]
    bytes c0 00 80 52              [aarch64.movz]
    bytes e0 0f 00 b9              [aarch64.str32]
    bytes e0 0f 40 b9              [aarch64.ldr32]
    bytes 1f 00 00 71              [aarch64.subs-imm]
    bytes 0d 00 00 54              [aarch64.b-cond]
      @0/4B pc+0 target pcrel-b19 s19 [5+19@0] = .L4
    bytes 01 00 80 52              [aarch64.movz]
    bytes 00 00 80 52              [aarch64.movz]
    label .L3
    bytes 00 1c 00 11              [aarch64.add-imm]
    bytes 21 04 00 11              [aarch64.add-imm]
    bytes e2 0f 40 b9              [aarch64.ldr32]
    bytes 5f 00 01 6b              [aarch64.addsub-shift]
    bytes 0c 00 00 54              [aarch64.b-cond]
      @0/4B pc+0 target pcrel-b19 s19 [5+19@0] = .L3
    label .L1
    bytes ff 43 00 91              [aarch64.add-imm]
    bytes c0 03 5f d6              [aarch64.ret]
    label .L4
    bytes 00 00 80 52              [aarch64.movz]
    bytes 00 00 00 14              [aarch64.b]
      @0/4B pc+0 target pcrel-b26 s26 [0+26@0] = .L1
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .L3 notype in .text
  local .L1 notype in .text
  local .L4 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## aarch64 loop plan
  segment .text size=68 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## aarch64 loop image
  section .text address=0x0 size=68 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=68
  ########## aarch64 loop bytes
  ff 43 00 d1 c0 00 80 52 e0 0f 00 b9 e0 0f 40 b9 1f 00 00 71 4d 01 00 54 01 00 80 52 00 00 80 52 00 1c 00 11 21 04 00 11 e2 0f 40 b9 5f 00 01 6b 8c ff ff 54 ff 43 00 91 c0 03 5f d6 00 00 80 52 fd ff ff 17
  ########## aarch64 loop disasm canonical
  	sub sp, sp, #16
  	movz w0, #6
  	str w0, [sp, #12]
  	ldr w0, [sp, #12]
  	cmp w0, #0
  	b.le 60
  	movz w1, #0
  	movz w0, #0
  	add w0, w0, #7
  	add w1, w1, #1
  	ldr w2, [sp, #12]
  	cmp w2, w1
  	b.gt 32
  	add sp, sp, #16
  	ret
  	movz w0, #0
  	b 52
  ########## aarch64 loop disasm diagnostic
  00000000  ff 43 00 d1  sub sp, sp, #16    [aarch64.sub-imm]
  00000004  c0 00 80 52  movz w0, #6        [aarch64.movz]
  00000008  e0 0f 00 b9  str w0, [sp, #12]  [aarch64.str32]
  0000000c  e0 0f 40 b9  ldr w0, [sp, #12]  [aarch64.ldr32]
  00000010  1f 00 00 71  cmp w0, #0         [aarch64.subs-imm]
  00000014  4d 01 00 54  b.le 60            [aarch64.b-cond]
  00000018  01 00 80 52  movz w1, #0        [aarch64.movz]
  0000001c  00 00 80 52  movz w0, #0        [aarch64.movz]
  00000020  00 1c 00 11  add w0, w0, #7     [aarch64.add-imm]
  00000024  21 04 00 11  add w1, w1, #1     [aarch64.add-imm]
  00000028  e2 0f 40 b9  ldr w2, [sp, #12]  [aarch64.ldr32]
  0000002c  5f 00 01 6b  cmp w2, w1         [aarch64.addsub-shift]
  00000030  8c ff ff 54  b.gt 32            [aarch64.b-cond]
  00000034  ff 43 00 91  add sp, sp, #16    [aarch64.add-imm]
  00000038  c0 03 5f d6  ret                [aarch64.ret]
  0000003c  00 00 80 52  movz w0, #0        [aarch64.movz]
  00000040  fd ff ff 17  b 52               [aarch64.b]
  ########## aarch64 loop codec
  alt aarch64
    [0 cost=0] add-imm          add-imm(){sf:1u 00100010 sh:1u <imm:12@0 add-lo12> rn rd}
    [1 cost=0] sub-imm          sub-imm(){sf:1u 10100010 sh:1u imm12:12u rn rd}
    [2 cost=0] addsub-shift     addsub-shift(){sf:1u op:1u s:1u 01011 shift:2u 0 rm imm6:6u rn rd}
    [3 cost=0] logical-imm-32   logical-imm-32(){0 opc:2u 100100 bitmask32 rn rd}
    [4 cost=0] logical-imm-64   logical-imm-64(){1 opc:2u 100100 bitmask64 rn rd}
    [5 cost=0] madd             madd(){sf:1u 0011011000 rm 0 ra rn rd}
    [6 cost=0] csel             csel(){sf:1u 0011010100 rm cond 00 rn rd}
    [7 cost=0] stp-pre          stp-pre(){1010100110 imm7-scaled8 rt2 rn rt}
    [8 cost=0] movz             movz(){sf:1u 10100101 hw:2u imm16:16u rd}
    [9 cost=0] ldr8             ldr8(){00 111001 01 offset-scaled1 rn rt}
    [10 cost=0] str8             str8(){00 111001 00 offset-scaled1 rn rt}
    [11 cost=0] ldr16            ldr16(){01 111001 01 offset-scaled2 rn rt}
    [12 cost=0] str16            str16(){01 111001 00 offset-scaled2 rn rt}
    [13 cost=0] ldr32            ldr32(){10 111001 01 offset-scaled4 rn rt}
    [14 cost=0] str32            str32(){10 111001 00 offset-scaled4 rn rt}
    [15 cost=0] ldr64            ldr64(){11 111001 01 offset-scaled8 rn rt}
    [16 cost=0] str64            str64(){11 111001 00 offset-scaled8 rn rt}
    [17 cost=0] ret              ret(){1101011001011111000000 rn 00000}
    [18 cost=0] udf              udf(){0000000000000000 imm16:16u}
    [19 cost=0] adrp             adrp(){1 <page:2@0 adrp-page> 10000 <page:19@2 adrp-page> rd}
    [20 cost=0] b                b(){000101 <target:26@0 pcrel-b26>}
    [21 cost=0] b-cond           b-cond(){01010100 <target:19@0 pcrel-b19> 0 cond}
    [22 cost=0] bl               bl(){100101 <target:26@0 pcrel-call26>}
    [23 cost=0] subs-imm         subs-imm(){sf:1u 11100010 sh:1u imm12:12u rn rd}
    [24 cost=0] ldr8-roff        ldr8-roff(){00 111000 01 1 rm option:3u s:1u 10 rn rt}
    [25 cost=0] str8-roff        str8-roff(){00 111000 00 1 rm option:3u s:1u 10 rn rt}
    [26 cost=0] ldr16-roff       ldr16-roff(){01 111000 01 1 rm option:3u s:1u 10 rn rt}
    [27 cost=0] str16-roff       str16-roff(){01 111000 00 1 rm option:3u s:1u 10 rn rt}
    [28 cost=0] ldr32-roff       ldr32-roff(){10 111000 01 1 rm option:3u s:1u 10 rn rt}
    [29 cost=0] str32-roff       str32-roff(){10 111000 00 1 rm option:3u s:1u 10 rn rt}
    [30 cost=0] ldr64-roff       ldr64-roff(){11 111000 01 1 rm option:3u s:1u 10 rn rt}
    [31 cost=0] str64-roff       str64-roff(){11 111000 00 1 rm option:3u s:1u 10 rn rt}
    [32 cost=0] fmov-imm-d       fmov-imm-d(){000 11110 01 1 fpimm8-d 10000000 rd:5u}
    [33 cost=0] fmov-imm-s       fmov-imm-s(){000 11110 00 1 fpimm8-s 10000000 rd:5u}
    [34 cost=0] fcmp-imm0-d      fcmp-imm0-d(){000 11110 01 1 00000001000 rn:5u 01000}
    [35 cost=0] fcmp-imm0-s      fcmp-imm0-s(){000 11110 00 1 00000001000 rn:5u 01000}
    [36 cost=0] addsub-extend    addsub-extend(){sf:1u op:1u s:1u 01011 00 1 rm option:3u imm3:3u rn rd}
    [37 cost=0] movn             movn(){sf:1u 00100101 hw:2u imm16:16u rd}
    [38 cost=0] cbz              cbz(){sf:1u 011010 op:1u <target:19@0 pcrel-b19> rt}
    [39 cost=0] logical-shift    logical-shift(){sf:1u opc:2u 01010 shift:2u N:1u rm imm6:6u rn rd}
    [40 cost=0] sdiv             sdiv(){sf:1u 0011010110 rm 000011 rn rd}
    [41 cost=0] sxtw             sxtw(){100100110 1 000000 011111 rn rd}
    [42 cost=0] msub             msub(){sf:1u 0011011000 rm 1 ra rn rd}
    [43 cost=0] udiv             udiv(){sf:1u 0011010110 rm 000010 rn rd}
    [44 cost=0] cset             cset(){sf:1u 0011010100 rm cond 01 rn rd}
    [45 cost=0] movk             movk(){sf:1u 11100101 hw:2u imm16:16u rd}
    [46 cost=0] lslv             lslv(){sf:1u 0011010110 rm 001000 rn rd}
    [47 cost=0] ubfm             ubfm(){sf:1u 10100110 N:1u immr:6u imms:6u rn rd}
    [48 cost=0] adr              adr(){0 <offset:2@0 pcrel-adr21> 10000 <offset:19@2 pcrel-adr21> rd}
    [49 cost=0] br               br(){1101011000011111000000 rn 00000}
    [50 cost=0] fcmp-reg-d       fcmp-reg-d(){000 11110 01 1 rm:5u 00 1000 rn:5u 00000}
    [51 cost=0] fcmp-reg-s       fcmp-reg-s(){000 11110 00 1 rm:5u 00 1000 rn:5u 00000}
    [52 cost=0] fmul-d           fbinop-fmul-d(){000 11110 01 1 rm:5u 0000 10 rn:5u rd:5u}
    [53 cost=0] fmul-s           fbinop-fmul-s(){000 11110 00 1 rm:5u 0000 10 rn:5u rd:5u}
    [54 cost=0] fdiv-d           fbinop-fdiv-d(){000 11110 01 1 rm:5u 0001 10 rn:5u rd:5u}
    [55 cost=0] fdiv-s           fbinop-fdiv-s(){000 11110 00 1 rm:5u 0001 10 rn:5u rd:5u}
    [56 cost=0] fadd-d           fbinop-fadd-d(){000 11110 01 1 rm:5u 0010 10 rn:5u rd:5u}
    [57 cost=0] fadd-s           fbinop-fadd-s(){000 11110 00 1 rm:5u 0010 10 rn:5u rd:5u}
    [58 cost=0] fsub-d           fbinop-fsub-d(){000 11110 01 1 rm:5u 0011 10 rn:5u rd:5u}
    [59 cost=0] fsub-s           fbinop-fsub-s(){000 11110 00 1 rm:5u 0011 10 rn:5u rd:5u}
    [60 cost=0] fcsel-d          fcsel-d(){000 11110 01 1 rm:5u cond 11 rn:5u rd:5u}
    [61 cost=0] fcsel-s          fcsel-s(){000 11110 00 1 rm:5u cond 11 rn:5u rd:5u}
    [62 cost=0] scvtf-d          cvtf-s-d(){sf:1u 0011110 01 100010000000 rn:5u rd:5u}
    [63 cost=0] scvtf-s          cvtf-s-s(){sf:1u 0011110 00 100010000000 rn:5u rd:5u}
    [64 cost=0] ucvtf-d          cvtf-u-d(){sf:1u 0011110 01 100011000000 rn:5u rd:5u}
    [65 cost=0] ucvtf-s          cvtf-u-s(){sf:1u 0011110 00 100011000000 rn:5u rd:5u}
    [66 cost=0] fmov-from-gpr-d  fmov-from-gpr-d(){sf:1u 0011110 01 100111000000 rn:5u rd:5u}
    [67 cost=0] fmov-from-gpr-s  fmov-from-gpr-s(){sf:1u 0011110 00 100111000000 rn:5u rd:5u}
    [68 cost=0] fcvtzs-d         fcvtzs-d(){sf:1u 0011110 01 111000000000 rn:5u rd:5u}
    [69 cost=0] fcvtzs-s         fcvtzs-s(){sf:1u 0011110 00 111000000000 rn:5u rd:5u}
    [70 cost=0] fneg-d           fneg-d(){00011110 01 100001010000 rn:5u rd:5u}
    [71 cost=0] fneg-s           fneg-s(){00011110 00 100001010000 rn:5u rd:5u}
    [72 cost=0] fcvt-d-to-s      fcvt-d-to-s(){00011110 01 100010010000 rn:5u rd:5u}
    [73 cost=0] fcvt-s-to-d      fcvt-s-to-d(){00011110 00 100010110000 rn:5u rd:5u}
    [74 cost=0] ldr32-f          ldr32-f(){10 111101 01 offset-scaled4 rn rt:5u}
    [75 cost=0] ldr64-f          ldr64-f(){11 111101 01 offset-scaled8 rn rt:5u}
    [76 cost=0] str32-f          str32-f(){10 111101 00 offset-scaled4 rn rt:5u}
    [77 cost=0] str64-f          str64-f(){11 111101 00 offset-scaled8 rn rt:5u}
    [78 cost=0] ldr32-f-roff     ldr32-f-roff(){10 111100 01 1 rm option:3u s:1u 10 rn rt:5u}
    [79 cost=0] ldr64-f-roff     ldr64-f-roff(){11 111100 01 1 rm option:3u s:1u 10 rn rt:5u}
    [80 cost=0] str32-f-roff     str32-f-roff(){10 111100 00 1 rm option:3u s:1u 10 rn rt:5u}
    [81 cost=0] str64-f-roff     str64-f-roff(){11 111100 00 1 rm option:3u s:1u 10 rn rt:5u}
    [82 cost=0] fmov-reg-d       fmov-reg-d(){00011110 01 100000010000 rn:5u rd:5u}
    [83 cost=0] fmov-reg-s       fmov-reg-s(){00011110 00 100000010000 rn:5u rd:5u}
    [84 cost=0] ldp64-post       ldp64-post(){1010100011 imm7-scaled8 rt2 rn rt}
    [85 cost=0] ldp64-off        ldp64-off(){1010100101 imm7-scaled8 rt2 rn rt}
    [86 cost=0] stp64-post       stp64-post(){1010100010 imm7-scaled8 rt2 rn rt}
    [87 cost=0] stp64-off        stp64-off(){1010100100 imm7-scaled8 rt2 rn rt}
    [88 cost=0] ldp32-post       ldp32-post(){0010100011 imm7-scaled4 rt2 rn rt}
    [89 cost=0] ldp32-off        ldp32-off(){0010100101 imm7-scaled4 rt2 rn rt}
    [90 cost=0] stp32-post       stp32-post(){0010100010 imm7-scaled4 rt2 rn rt}
    [91 cost=0] stp32-off        stp32-off(){0010100100 imm7-scaled4 rt2 rn rt}
    [92 cost=0] adds-imm         adds-imm(){sf:1u 01100010 sh:1u imm12:12u rn rd}
    [93 cost=0] ldr8-post        ldr8-post(){00 111000 01 0 imm9-scaled1 01 rn rt}
    [94 cost=0] str8-post        str8-post(){00 111000 00 0 imm9-scaled1 01 rn rt}
    [95 cost=0] ldr16-post       ldr16-post(){01 111000 01 0 imm9-scaled1 01 rn rt}
    [96 cost=0] str16-post       str16-post(){01 111000 00 0 imm9-scaled1 01 rn rt}
    [97 cost=0] ldr32-post       ldr32-post(){10 111000 01 0 imm9-scaled1 01 rn rt}
    [98 cost=0] str32-post       str32-post(){10 111000 00 0 imm9-scaled1 01 rn rt}
    [99 cost=0] ldr64-post       ldr64-post(){11 111000 01 0 imm9-scaled1 01 rn rt}
    [100 cost=0] str64-post       str64-post(){11 111000 00 0 imm9-scaled1 01 rn rt}
    [101 cost=0] sbfm             sbfm(){sf:1u 00100110 N:1u immr:6u imms:6u rn rd}
    [102 cost=0] lsrv             lsrv(){sf:1u 0011010110 rm 001001 rn rd}
    [103 cost=0] asrv             asrv(){sf:1u 0011010110 rm 001010 rn rd}
    [104 cost=0] rorv             rorv(){sf:1u 0011010110 rm 001011 rn rd}
    [105 cost=0] fcvtzu-d         fcvtzu-d(){sf:1u 0011110 01 111001000000 rn:5u rd:5u}
    [106 cost=0] fcvtzu-s         fcvtzu-s(){sf:1u 0011110 00 111001000000 rn:5u rd:5u}
    [110 cost=0] ldrsb-w          ldrsb-w(){00 111001 11 offset-scaled1 rn rt}
    [111 cost=0] ldrsb-w-roff     ldrsb-w-roff(){00 111000 11 1 rm option:3u s:1u 10 rn rt}
    [112 cost=0] ldrsb-x          ldrsb-x(){00 111001 10 offset-scaled1 rn rt}
    [113 cost=0] ldrsb-x-roff     ldrsb-x-roff(){00 111000 10 1 rm option:3u s:1u 10 rn rt}
    [114 cost=0] ldrsh-w          ldrsh-w(){01 111001 11 offset-scaled2 rn rt}
    [115 cost=0] ldrsh-w-roff     ldrsh-w-roff(){01 111000 11 1 rm option:3u s:1u 10 rn rt}
    [116 cost=0] ldrsh-x          ldrsh-x(){01 111001 10 offset-scaled2 rn rt}
    [117 cost=0] ldrsh-x-roff     ldrsh-x-roff(){01 111000 10 1 rm option:3u s:1u 10 rn rt}
    [118 cost=0] ldrsw-x          ldrsw-x(){10 111001 10 offset-scaled4 rn rt}
    [119 cost=0] ldrsw-x-roff     ldrsw-x-roff(){10 111000 10 1 rm option:3u s:1u 10 rn rt}
    [130 cost=0] rbit-w           rbit-w(){0 101101011000000000000 rn rd}
    [131 cost=0] rbit-x           rbit-x(){1 101101011000000000000 rn rd}
    [132 cost=0] rev16-w          rev16-w(){0 101101011000000000001 rn rd}
    [133 cost=0] rev16-x          rev16-x(){1 101101011000000000001 rn rd}
    [134 cost=0] rev32-x          rev32-x(){1 101101011000000000010 rn rd}
    [135 cost=0] rev-w            rev-w(){0 101101011000000000010 rn rd}
    [136 cost=0] rev-x            rev-x(){1 101101011000000000011 rn rd}
    [137 cost=0] clz-w            clz-w(){0 101101011000000000100 rn rd}
    [138 cost=0] clz-x            clz-x(){1 101101011000000000100 rn rd}
    [139 cost=0] cls-w            cls-w(){0 101101011000000000101 rn rd}
    [140 cost=0] cls-x            cls-x(){1 101101011000000000101 rn rd}
    [141 cost=0] mrs-fpcr         mrs-fpcr(){110101010011101101000100000 rt}
    [142 cost=0] msr-fpcr         msr-fpcr(){110101010001101101000100000 rt}
    [143 cost=0] fsqrt-d          fsqrt-d(){00011110 01 100001110000 rn:5u rd:5u}
    [144 cost=0] fsqrt-s          fsqrt-s(){00011110 00 100001110000 rn:5u rd:5u}
    [145 cost=0] frintz-d         frintz-d(){00011110 01 100101110000 rn:5u rd:5u}
    [146 cost=0] frintz-s         frintz-s(){00011110 00 100101110000 rn:5u rd:5u}
    [147 cost=0] fmadd-d          fmadd-d(){00011111 01 0 rm:5u 0 ra:5u rn:5u rd:5u}
    [148 cost=0] fmadd-s          fmadd-s(){00011111 00 0 rm:5u 0 ra:5u rn:5u rd:5u}
    [149 cost=0] fmov-to-gpr-d    fmov-to-gpr-d(){sf:1u 0011110 01 100110000000 rn:5u rd:5u}
    [150 cost=0] fmov-to-gpr-s    fmov-to-gpr-s(){sf:1u 0011110 00 100110000000 rn:5u rd:5u}
    [151 cost=0] fmax-d           fbinop-fmax-d(){000 11110 01 1 rm:5u 0100 10 rn:5u rd:5u}
    [152 cost=0] fmax-s           fbinop-fmax-s(){000 11110 00 1 rm:5u 0100 10 rn:5u rd:5u}
    [153 cost=0] vand-16b         vand-16b(){01001110001 rm:5u 000111 rn:5u rd:5u}
    [154 cost=0] vand-8b          vand-8b(){00001110001 rm:5u 000111 rn:5u rd:5u}
    [155 cost=0] vfcvtl           vfcvtl(){0000111001100001011110 rn:5u rd:5u}
    [156 cost=0] vfcvtn           vfcvtn(){0000111001100001011010 rn:5u rd:5u}
    [157 cost=0] ldr-q            ldr-q(){0011110111 imm12:12u rn rt:5u}
    [158 cost=0] str-q            str-q(){0011110110 imm12:12u rn rt:5u}
    [159 cost=0] vfadd-s2         vfadd-2s(){00001110001 rm:5u 110101 rn:5u rd:5u}
    [160 cost=0] vfadd-s4         vfadd-4s(){01001110001 rm:5u 110101 rn:5u rd:5u}
    [161 cost=0] vfadd-d2         vfadd-2d(){01001110011 rm:5u 110101 rn:5u rd:5u}
    [162 cost=0] vfsub-s2         vfsub-2s(){00001110101 rm:5u 110101 rn:5u rd:5u}
    [163 cost=0] vfsub-s4         vfsub-4s(){01001110101 rm:5u 110101 rn:5u rd:5u}
    [164 cost=0] vfsub-d2         vfsub-2d(){01001110111 rm:5u 110101 rn:5u rd:5u}
    [165 cost=0] vfmul-s2         vfmul-2s(){00101110001 rm:5u 110111 rn:5u rd:5u}
    [166 cost=0] vfmul-s4         vfmul-4s(){01101110001 rm:5u 110111 rn:5u rd:5u}
    [167 cost=0] vfmul-d2         vfmul-2d(){01101110011 rm:5u 110111 rn:5u rd:5u}
    [168 cost=0] vfdiv-s2         vfdiv-2s(){00101110001 rm:5u 111111 rn:5u rd:5u}
    [169 cost=0] vfdiv-s4         vfdiv-4s(){01101110001 rm:5u 111111 rn:5u rd:5u}
    [170 cost=0] vfdiv-d2         vfdiv-2d(){01101110011 rm:5u 111111 rn:5u rd:5u}
    [171 cost=0] vfmax-s2         vfmax-2s(){00001110001 rm:5u 111101 rn:5u rd:5u}
    [172 cost=0] vfmax-s4         vfmax-4s(){01001110001 rm:5u 111101 rn:5u rd:5u}
    [173 cost=0] vfmax-d2         vfmax-2d(){01001110011 rm:5u 111101 rn:5u rd:5u}
    [174 cost=0] vfmla-s2         vfmla-2s(){00001110001 rm:5u 110011 rn:5u rd:5u}
    [175 cost=0] vfmla-s4         vfmla-4s(){01001110001 rm:5u 110011 rn:5u rd:5u}
    [176 cost=0] vfmla-d2         vfmla-2d(){01001110011 rm:5u 110011 rn:5u rd:5u}
    [177 cost=0] vfneg-s2         vfneg-2s(){0010111010100000111110 rn:5u rd:5u}
    [178 cost=0] vfneg-s4         vfneg-4s(){0110111010100000111110 rn:5u rd:5u}
    [179 cost=0] vfneg-d2         vfneg-2d(){0110111011100000111110 rn:5u rd:5u}
    [180 cost=0] vfsqrt-s2        vfsqrt-2s(){0010111010100001111110 rn:5u rd:5u}
    [181 cost=0] vfsqrt-s4        vfsqrt-4s(){0110111010100001111110 rn:5u rd:5u}
    [182 cost=0] vfsqrt-d2        vfsqrt-2d(){0110111011100001111110 rn:5u rd:5u}
    [183 cost=0] vfrintz-s2       vfrintz-2s(){0000111010100001100110 rn:5u rd:5u}
    [184 cost=0] vfrintz-s4       vfrintz-4s(){0100111010100001100110 rn:5u rd:5u}
    [185 cost=0] vfrintz-d2       vfrintz-2d(){0100111011100001100110 rn:5u rd:5u}
    [191 cost=0] vld1-s           vld1-s(){0 q:1u 00110101 000000100 s:1u 00 rn rt:5u}
    [192 cost=0] vst1-s           vst1-s(){0 q:1u 00110100 000000100 s:1u 00 rn rt:5u}
    [195 cost=0] vld1-d           vld1-d(){0 q:1u 00110101 000000100 s:1u 01 rn rt:5u}
    [196 cost=0] vst1-d           vst1-d(){0 q:1u 00110100 000000100 s:1u 01 rn rt:5u}
    [197 cost=0] vld1r-s2         vld1r-2s(){00 00110101 0000001100 10 rn rt:5u}
    [198 cost=0] vld1r-s4         vld1r-4s(){01 00110101 0000001100 10 rn rt:5u}
    [199 cost=0] vld1r-d2         vld1r-2d(){01 00110101 0000001100 11 rn rt:5u}
    [200 cost=0] vdup-q           vdup-q(){01001110000 imm5:5u 000001 rn:5u rd:5u}
    [201 cost=0] vdup-d           vdup-d(){00001110000 imm5:5u 000001 rn:5u rd:5u}
    [202 cost=0] vdup-scalar      vdup-scalar(){01011110000 imm5:5u 000001 rn:5u rd:5u}
    [203 cost=0] vins             vins(){01101110000 imm5:5u 0 imm4:4u 1 rn:5u rd:5u}
    [204 cost=0] vorr-16b         vorr-16b(){01001110101 rm:5u 000111 rn:5u rd:5u}
    [205 cost=0] vorr-8b          vorr-8b(){00001110101 rm:5u 000111 rn:5u rd:5u}
    [206 cost=0] veor-16b         veor-16b(){01101110001 rm:5u 000111 rn:5u rd:5u}
    [207 cost=0] veor-8b          veor-8b(){00101110001 rm:5u 000111 rn:5u rd:5u}
    [208 cost=0] vbsl-16b         vbsl-16b(){01101110011 rm:5u 000111 rn:5u rd:5u}
    [209 cost=0] vbsl-8b          vbsl-8b(){00101110011 rm:5u 000111 rn:5u rd:5u}
    [210 cost=0] vnot-16b         vnot-16b(){0110111000100000010110 rn:5u rd:5u}
    [211 cost=0] vnot-8b          vnot-8b(){0010111000100000010110 rn:5u rd:5u}
    [212 cost=0] vfcmeq-s2        vfcmeq-2s(){00001110001 rm:5u 111001 rn:5u rd:5u}
    [213 cost=0] vfcmeq-s4        vfcmeq-4s(){01001110001 rm:5u 111001 rn:5u rd:5u}
    [214 cost=0] vfcmeq-d2        vfcmeq-2d(){01001110011 rm:5u 111001 rn:5u rd:5u}
    [215 cost=0] vfcmge-s2        vfcmge-2s(){00101110001 rm:5u 111001 rn:5u rd:5u}
    [216 cost=0] vfcmge-s4        vfcmge-4s(){01101110001 rm:5u 111001 rn:5u rd:5u}
    [217 cost=0] vfcmge-d2        vfcmge-2d(){01101110011 rm:5u 111001 rn:5u rd:5u}
    [218 cost=0] vfcmgt-s2        vfcmgt-2s(){00101110101 rm:5u 111001 rn:5u rd:5u}
    [219 cost=0] vfcmgt-s4        vfcmgt-4s(){01101110101 rm:5u 111001 rn:5u rd:5u}
    [220 cost=0] vfcmgt-d2        vfcmgt-2d(){01101110111 rm:5u 111001 rn:5u rd:5u}
    [221 cost=0] vbit-16b         vbit-16b(){01101110101 rm:5u 000111 rn:5u rd:5u}
    [222 cost=0] vbit-8b          vbit-8b(){00101110101 rm:5u 000111 rn:5u rd:5u}
    [223 cost=0] vdup-gpr-q       vdup-gpr-q(){01001110000 imm5:5u 000011 rn rd:5u}
    [224 cost=0] vdup-gpr-d       vdup-gpr-d(){00001110000 imm5:5u 000011 rn rd:5u}
    [225 cost=0] mrs-nzcv         mrs-nzcv(){110101010011101101000010000 rt}
    [226 cost=0] msr-nzcv         msr-nzcv(){110101010001101101000010000 rt}
  rn(){rn:5u}
  rd(){rd:5u}
  rm(){rm:5u}
  bitmask32(){n:1u immr:6u imms:6u}
  bitmask64(){n:1u immr:6u imms:6u}
  ra(){ra:5u}
  cond[16]{cond:4u}
  imm7-scaled8(){imm7:7s}
  rt2(){rt2:5u}
  rt(){rt:5u}
  offset-scaled1(){<offset:12@0 ldst8-lo12>}
  offset-scaled2(){<offset:12@0 ldst16-lo12>}
  offset-scaled4(){<offset:12@0 ldst32-lo12>}
  offset-scaled8(){<offset:12@0 ldst64-lo12>}
  fpimm8-d(){imm8:8u}
  fpimm8-s(){imm8:8u}
  imm7-scaled4(){imm7:7s}
  imm9-scaled1(){imm9:9s}
  ########## riscv32 loop tokens
  1 5 directive .file
  7 8 string "loop.c"
  15 1 eol \n
  17 7 directive .option
  25 5 ident nopic
  30 1 eol \n
  32 7 directive .option
  40 7 ident norelax
  47 1 eol \n
  49 10 directive .attribute
  60 4 ident arch
  64 1 comma ,
  66 39 string "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  105 1 eol \n
  107 10 directive .attribute
  118 16 ident unaligned_access
  134 1 comma ,
  136 1 int 0
  137 1 eol \n
  139 10 directive .attribute
  150 11 ident stack_align
  161 1 comma ,
  163 2 int 16
  165 1 eol \n
  167 5 directive .text
  172 1 eol \n
  174 6 directive .align
  181 1 int 2
  182 1 eol \n
  184 6 directive .globl
  191 14 ident asm_test_entry
  205 1 eol \n
  207 5 directive .type
  213 14 ident asm_test_entry
  227 1 comma ,
  229 1 at @
  230 8 ident function
  238 1 eol \n
  239 14 ident asm_test_entry
  253 1 colon :
  254 1 eol \n
  255 5 directive .LFB0
  260 1 colon :
  261 1 eol \n
  263 14 directive .cfi_startproc
  277 1 eol \n
  279 4 ident addi
  284 2 ident sp
  286 1 comma ,
  287 2 ident sp
  289 1 comma ,
  290 1 minus -
  291 2 int 16
  293 1 eol \n
  295 19 directive .cfi_def_cfa_offset
  315 2 int 16
  317 1 eol \n
  319 2 ident li
  322 2 ident a5
  324 1 comma ,
  325 1 int 6
  326 1 eol \n
  328 2 ident sw
  331 2 ident a5
  333 1 comma ,
  334 2 int 12
  336 1 lparen (
  337 2 ident sp
  339 1 rparen )
  340 1 eol \n
  342 2 ident lw
  345 2 ident a5
  347 1 comma ,
  348 2 int 12
  350 1 lparen (
  351 2 ident sp
  353 1 rparen )
  354 1 eol \n
  356 3 ident ble
  360 2 ident a5
  362 1 comma ,
  363 4 ident zero
  367 1 comma ,
  368 3 directive .L4
  371 1 eol \n
  373 2 ident li
  376 2 ident a5
  378 1 comma ,
  379 1 int 0
  380 1 eol \n
  382 2 ident li
  385 2 ident a0
  387 1 comma ,
  388 1 int 0
  389 1 eol \n
  390 3 directive .L3
  393 1 colon :
  394 1 eol \n
  396 4 ident addi
  401 2 ident a0
  403 1 comma ,
  404 2 ident a0
  406 1 comma ,
  407 1 int 7
  408 1 eol \n
  410 4 ident addi
  415 2 ident a5
  417 1 comma ,
  418 2 ident a5
  420 1 comma ,
  421 1 int 1
  422 1 eol \n
  424 2 ident lw
  427 2 ident a4
  429 1 comma ,
  430 2 int 12
  432 1 lparen (
  433 2 ident sp
  435 1 rparen )
  436 1 eol \n
  438 3 ident bgt
  442 2 ident a4
  444 1 comma ,
  445 2 ident a5
  447 1 comma ,
  448 3 directive .L3
  451 1 eol \n
  452 3 directive .L1
  455 1 colon :
  456 1 eol \n
  458 4 ident addi
  463 2 ident sp
  465 1 comma ,
  466 2 ident sp
  468 1 comma ,
  469 2 int 16
  471 1 eol \n
  473 19 directive .cfi_remember_state
  492 1 eol \n
  494 19 directive .cfi_def_cfa_offset
  514 1 int 0
  515 1 eol \n
  517 2 ident jr
  520 2 ident ra
  522 1 eol \n
  523 3 directive .L4
  526 1 colon :
  527 1 eol \n
  529 18 directive .cfi_restore_state
  547 1 eol \n
  549 2 ident li
  552 2 ident a0
  554 1 comma ,
  555 1 int 0
  556 1 eol \n
  558 1 ident j
  560 3 directive .L1
  563 1 eol \n
  565 12 directive .cfi_endproc
  577 1 eol \n
  578 5 directive .LFE0
  583 1 colon :
  584 1 eol \n
  586 5 directive .size
  592 14 ident asm_test_entry
  606 1 comma ,
  608 1 dot .
  609 1 minus -
  610 14 ident asm_test_entry
  624 1 eol \n
  626 6 directive .ident
  633 35 string "GCC: (crosstool-NG 1.27.0) 14.2.0"
  668 1 eol \n
  670 8 directive .section
  679 9 directive .note.GNU
  688 1 minus -
  689 5 ident stack
  694 1 comma ,
  695 2 string ""
  697 1 comma ,
  698 1 at @
  699 8 ident progbits
  707 1 eol \n
  708 0 eof 
  ########## riscv32 loop source ast
  source loop
  directive .file ["loop.c"]
  directive .option [nopic]
  directive .option [norelax]
  directive .attribute [arch] ["rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"]
  directive .attribute [unaligned_access] [0]
  directive .attribute [stack_align] [16]
  directive .text
  directive .align [2]
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn addi x2, x2, -16
  directive .cfi_def_cfa_offset [16]
  insn li x15, 6
  insn sw x15, 12(x2)
  insn lw x15, 12(x2)
  insn ble x15, x0, .L4
  insn li x15, 0
  insn li x10, 0
  label .L3
  insn addi x10, x10, 7
  insn addi x15, x15, 1
  insn lw x14, 12(x2)
  insn bgt x14, x15, .L3
  label .L1
  insn addi x2, x2, 16
  directive .cfi_remember_state
  directive .cfi_def_cfa_offset [0]
  insn jr x1
  label .L4
  directive .cfi_restore_state
  insn li x10, 0
  insn j .L1
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (crosstool-NG 1.27.0) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## riscv32 loop normalized ast
  normalized loop
  target-state .option nopic
  target-state .option norelax
  target-state .attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  target-state .attribute unaligned_access, 0
  target-state .attribute stack_align, 16
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  addi x2, x2, -16
  li x15, 6
  sw x15, 12(x2)
  lw x15, 12(x2)
  ble x15, x0, .L4
  li x15, 0
  li x10, 0
  label .L3
  addi x10, x10, 7
  addi x15, x15, 1
  lw x14, 12(x2)
  bgt x14, x15, .L3
  label .L1
  addi x2, x2, 16
  jr x1
  label .L4
  li x10, 0
  j .L1
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## riscv32 loop lowered ast
  lowered loop
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes 13 01 01 ff              [riscv32.addi]
    bytes 93 07 60 00              [riscv32.addi]
    bytes 23 26 f1 00              [riscv32.sw]
    bytes 83 27 c1 00              [riscv32.lw]
    bytes 63 50 f0 00              [riscv32.bge]
      @0/4B pc+0 target pcrel-b13 s13 [7+1@11,8+4@1,25+6@5,31+1@12] = .L4
    bytes 93 07 00 00              [riscv32.addi]
    bytes 13 05 00 00              [riscv32.addi]
    label .L3
    bytes 13 05 75 00              [riscv32.addi]
    bytes 93 87 17 00              [riscv32.addi]
    bytes 03 27 c1 00              [riscv32.lw]
    bytes 63 c0 e7 00              [riscv32.blt]
      @0/4B pc+0 target pcrel-b13 s13 [7+1@11,8+4@1,25+6@5,31+1@12] = .L3
    label .L1
    bytes 13 01 01 01              [riscv32.addi]
    bytes 67 80 00 00              [riscv32.jalr]
    label .L4
    bytes 13 05 00 00              [riscv32.addi]
    bytes 6f 00 00 00              [riscv32.jal]
      @0/4B pc+0 target pcrel-j21 s21 [12+8@12,20+1@11,21+10@1,31+1@20] = .L1
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .L3 notype in .text
  local .L1 notype in .text
  local .L4 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## riscv32 loop plan
  segment .text size=60 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## riscv32 loop image
  section .text address=0x0 size=60 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=60
  ########## riscv32 loop bytes
  13 01 01 ff 93 07 60 00 23 26 f1 00 83 27 c1 00 63 52 f0 02 93 07 00 00 13 05 00 00 13 05 75 00 93 87 17 00 03 27 c1 00 e3 ca e7 fe 13 01 01 01 67 80 00 00 13 05 00 00 6f f0 5f ff
  ########## riscv32 loop disasm canonical
  	addi x2, x2, -16
  	addi x15, x0, 6
  	sw x15, 12(x2)
  	lw x15, 12(x2)
  	bge x0, x15, 52
  	addi x15, x0, 0
  	addi x10, x0, 0
  	addi x10, x10, 7
  	addi x15, x15, 1
  	lw x14, 12(x2)
  	blt x15, x14, 28
  	addi x2, x2, 16
  	jalr x0, 0(x1)
  	addi x10, x0, 0
  	jal x0, 44
  ########## riscv32 loop disasm diagnostic
  00000000  13 01 01 ff  addi x2, x2, -16  [riscv32.addi]
  00000004  93 07 60 00  addi x15, x0, 6   [riscv32.addi]
  00000008  23 26 f1 00  sw x15, 12(x2)    [riscv32.sw]
  0000000c  83 27 c1 00  lw x15, 12(x2)    [riscv32.lw]
  00000010  63 52 f0 02  bge x0, x15, 52   [riscv32.bge]
  00000014  93 07 00 00  addi x15, x0, 0   [riscv32.addi]
  00000018  13 05 00 00  addi x10, x0, 0   [riscv32.addi]
  0000001c  13 05 75 00  addi x10, x10, 7  [riscv32.addi]
  00000020  93 87 17 00  addi x15, x15, 1  [riscv32.addi]
  00000024  03 27 c1 00  lw x14, 12(x2)    [riscv32.lw]
  00000028  e3 ca e7 fe  blt x15, x14, 28  [riscv32.blt]
  0000002c  13 01 01 01  addi x2, x2, 16   [riscv32.addi]
  00000030  67 80 00 00  jalr x0, 0(x1)    [riscv32.jalr]
  00000034  13 05 00 00  addi x10, x0, 0   [riscv32.addi]
  00000038  6f f0 5f ff  jal x0, 44        [riscv32.jal]
  ########## riscv32 loop codec
  alt riscv32
    [0 cost=0] pair             riscv32-pair(){auipc-i-pair:64u}
    [1 cost=0] word             riscv32-word(){instruction:32u}
    [2 cost=0] compressed       riscv32-compressed(){compressed-instruction:16u}
  ########## riscv64 loop tokens
  1 5 directive .file
  7 8 string "loop.c"
  15 1 eol \n
  17 7 directive .option
  25 5 ident nopic
  30 1 eol \n
  32 7 directive .option
  40 7 ident norelax
  47 1 eol \n
  49 10 directive .attribute
  60 4 ident arch
  64 1 comma ,
  66 39 string "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  105 1 eol \n
  107 10 directive .attribute
  118 16 ident unaligned_access
  134 1 comma ,
  136 1 int 0
  137 1 eol \n
  139 10 directive .attribute
  150 11 ident stack_align
  161 1 comma ,
  163 2 int 16
  165 1 eol \n
  167 5 directive .text
  172 1 eol \n
  174 6 directive .align
  181 1 int 2
  182 1 eol \n
  184 6 directive .globl
  191 14 ident asm_test_entry
  205 1 eol \n
  207 5 directive .type
  213 14 ident asm_test_entry
  227 1 comma ,
  229 1 at @
  230 8 ident function
  238 1 eol \n
  239 14 ident asm_test_entry
  253 1 colon :
  254 1 eol \n
  255 5 directive .LFB0
  260 1 colon :
  261 1 eol \n
  263 14 directive .cfi_startproc
  277 1 eol \n
  279 4 ident addi
  284 2 ident sp
  286 1 comma ,
  287 2 ident sp
  289 1 comma ,
  290 1 minus -
  291 2 int 16
  293 1 eol \n
  295 19 directive .cfi_def_cfa_offset
  315 2 int 16
  317 1 eol \n
  319 2 ident li
  322 2 ident a5
  324 1 comma ,
  325 1 int 6
  326 1 eol \n
  328 2 ident sw
  331 2 ident a5
  333 1 comma ,
  334 2 int 12
  336 1 lparen (
  337 2 ident sp
  339 1 rparen )
  340 1 eol \n
  342 2 ident lw
  345 2 ident a5
  347 1 comma ,
  348 2 int 12
  350 1 lparen (
  351 2 ident sp
  353 1 rparen )
  354 1 eol \n
  356 6 ident sext.w
  363 2 ident a5
  365 1 comma ,
  366 2 ident a5
  368 1 eol \n
  370 3 ident ble
  374 2 ident a5
  376 1 comma ,
  377 4 ident zero
  381 1 comma ,
  382 3 directive .L4
  385 1 eol \n
  387 2 ident li
  390 2 ident a4
  392 1 comma ,
  393 1 int 0
  394 1 eol \n
  396 2 ident li
  399 2 ident a0
  401 1 comma ,
  402 1 int 0
  403 1 eol \n
  404 3 directive .L3
  407 1 colon :
  408 1 eol \n
  410 5 ident addiw
  416 2 ident a0
  418 1 comma ,
  419 2 ident a0
  421 1 comma ,
  422 1 int 7
  423 1 eol \n
  425 5 ident addiw
  431 2 ident a4
  433 1 comma ,
  434 2 ident a4
  436 1 comma ,
  437 1 int 1
  438 1 eol \n
  440 2 ident lw
  443 2 ident a5
  445 1 comma ,
  446 2 int 12
  448 1 lparen (
  449 2 ident sp
  451 1 rparen )
  452 1 eol \n
  454 6 ident sext.w
  461 2 ident a5
  463 1 comma ,
  464 2 ident a5
  466 1 eol \n
  468 3 ident bgt
  472 2 ident a5
  474 1 comma ,
  475 2 ident a4
  477 1 comma ,
  478 3 directive .L3
  481 1 eol \n
  482 3 directive .L2
  485 1 colon :
  486 1 eol \n
  488 4 ident addi
  493 2 ident sp
  495 1 comma ,
  496 2 ident sp
  498 1 comma ,
  499 2 int 16
  501 1 eol \n
  503 19 directive .cfi_remember_state
  522 1 eol \n
  524 19 directive .cfi_def_cfa_offset
  544 1 int 0
  545 1 eol \n
  547 2 ident jr
  550 2 ident ra
  552 1 eol \n
  553 3 directive .L4
  556 1 colon :
  557 1 eol \n
  559 18 directive .cfi_restore_state
  577 1 eol \n
  579 2 ident li
  582 2 ident a0
  584 1 comma ,
  585 1 int 0
  586 1 eol \n
  588 1 ident j
  590 3 directive .L2
  593 1 eol \n
  595 12 directive .cfi_endproc
  607 1 eol \n
  608 5 directive .LFE0
  613 1 colon :
  614 1 eol \n
  616 5 directive .size
  622 14 ident asm_test_entry
  636 1 comma ,
  638 1 dot .
  639 1 minus -
  640 14 ident asm_test_entry
  654 1 eol \n
  656 6 directive .ident
  663 32 string "GCC: (Debian 14.2.0-19) 14.2.0"
  695 1 eol \n
  697 8 directive .section
  706 9 directive .note.GNU
  715 1 minus -
  716 5 ident stack
  721 1 comma ,
  722 2 string ""
  724 1 comma ,
  725 1 at @
  726 8 ident progbits
  734 1 eol \n
  735 0 eof 
  ########## riscv64 loop source ast
  source loop
  directive .file ["loop.c"]
  directive .option [nopic]
  directive .option [norelax]
  directive .attribute [arch] ["rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"]
  directive .attribute [unaligned_access] [0]
  directive .attribute [stack_align] [16]
  directive .text
  directive .align [2]
  directive .globl [asm_test_entry]
  directive .type [asm_test_entry] [@function]
  label asm_test_entry
  label .LFB0
  directive .cfi_startproc
  insn addi x2, x2, -16
  directive .cfi_def_cfa_offset [16]
  insn li x15, 6
  insn sw x15, 12(x2)
  insn lw x15, 12(x2)
  insn sext.w x15, x15
  insn ble x15, x0, .L4
  insn li x14, 0
  insn li x10, 0
  label .L3
  insn addiw x10, x10, 7
  insn addiw x14, x14, 1
  insn lw x15, 12(x2)
  insn sext.w x15, x15
  insn bgt x15, x14, .L3
  label .L2
  insn addi x2, x2, 16
  directive .cfi_remember_state
  directive .cfi_def_cfa_offset [0]
  insn jr x1
  label .L4
  directive .cfi_restore_state
  insn li x10, 0
  insn j .L2
  directive .cfi_endproc
  label .LFE0
  directive .size [asm_test_entry] [.-asm_test_entry]
  directive .ident ["GCC: (Debian 14.2.0-19) 14.2.0"]
  directive .section [.note.GNU-stack] [""] [@progbits]
  ########## riscv64 loop normalized ast
  normalized loop
  target-state .option nopic
  target-state .option norelax
  target-state .attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_zicsr2p0"
  target-state .attribute unaligned_access, 0
  target-state .attribute stack_align, 16
  section .text r-x
  align 4
  globl asm_test_entry
  type asm_test_entry function
  label asm_test_entry
  label .LFB0
  addi x2, x2, -16
  li x15, 6
  sw x15, 12(x2)
  lw x15, 12(x2)
  sext.w x15, x15
  ble x15, x0, .L4
  li x14, 0
  li x10, 0
  label .L3
  addiw x10, x10, 7
  addiw x14, x14, 1
  lw x15, 12(x2)
  sext.w x15, x15
  bgt x15, x14, .L3
  label .L2
  addi x2, x2, 16
  jr x1
  label .L4
  li x10, 0
  j .L2
  label .LFE0
  size asm_test_entry (. - asm_test_entry)
  declared-section .note.GNU-stack
  ########## riscv64 loop lowered ast
  lowered loop
  section .text r-x align=4
    align 4
    label asm_test_entry
    label .LFB0
    bytes 13 01 01 ff              [riscv64.addi]
    bytes 93 07 60 00              [riscv64.addi]
    bytes 23 26 f1 00              [riscv64.sw]
    bytes 83 27 c1 00              [riscv64.lw]
    bytes 9b 87 07 00              [riscv64.addiw]
    bytes 63 50 f0 00              [riscv64.bge]
      @0/4B pc+0 target pcrel-b13 s13 [7+1@11,8+4@1,25+6@5,31+1@12] = .L4
    bytes 13 07 00 00              [riscv64.addi]
    bytes 13 05 00 00              [riscv64.addi]
    label .L3
    bytes 1b 05 75 00              [riscv64.addiw]
    bytes 1b 07 17 00              [riscv64.addiw]
    bytes 83 27 c1 00              [riscv64.lw]
    bytes 9b 87 07 00              [riscv64.addiw]
    bytes 63 40 f7 00              [riscv64.blt]
      @0/4B pc+0 target pcrel-b13 s13 [7+1@11,8+4@1,25+6@5,31+1@12] = .L3
    label .L2
    bytes 13 01 01 01              [riscv64.addi]
    bytes 67 80 00 00              [riscv64.jalr]
    label .L4
    bytes 13 05 00 00              [riscv64.addi]
    bytes 6f 00 00 00              [riscv64.jal]
      @0/4B pc+0 target pcrel-j21 s21 [12+8@12,20+1@11,21+10@1,31+1@20] = .L2
    label .LFE0
    size asm_test_entry = (. - asm_test_entry)
  global asm_test_entry function in .text size=(. - asm_test_entry)
  local .LFB0 notype in .text
  local .L3 notype in .text
  local .L2 notype in .text
  local .L4 notype in .text
  local .LFE0 notype in .text
  declared .note.GNU-stack (not allocated)
  ########## riscv64 loop plan
  segment .text size=68 zero=0 align=4 permissions=r-x
  entry asm_test_entry
  export asm_test_entry
  unresolved target
  ########## riscv64 loop image
  section .text address=0x0 size=68 permissions=r-x
  entry 0x0
  export asm_test_entry = 0x0 size=68
  ########## riscv64 loop bytes
  13 01 01 ff 93 07 60 00 23 26 f1 00 83 27 c1 00 9b 87 07 00 63 54 f0 02 13 07 00 00 13 05 00 00 1b 05 75 00 1b 07 17 00 83 27 c1 00 9b 87 07 00 e3 48 f7 fe 13 01 01 01 67 80 00 00 13 05 00 00 6f f0 5f ff
  ########## riscv64 loop disasm canonical
  	addi x2, x2, -16
  	addi x15, x0, 6
  	sw x15, 12(x2)
  	lw x15, 12(x2)
  	addiw x15, x15, 0
  	bge x0, x15, 60
  	addi x14, x0, 0
  	addi x10, x0, 0
  	addiw x10, x10, 7
  	addiw x14, x14, 1
  	lw x15, 12(x2)
  	addiw x15, x15, 0
  	blt x14, x15, 32
  	addi x2, x2, 16
  	jalr x0, 0(x1)
  	addi x10, x0, 0
  	jal x0, 52
  ########## riscv64 loop disasm diagnostic
  00000000  13 01 01 ff  addi x2, x2, -16   [riscv64.addi]
  00000004  93 07 60 00  addi x15, x0, 6    [riscv64.addi]
  00000008  23 26 f1 00  sw x15, 12(x2)     [riscv64.sw]
  0000000c  83 27 c1 00  lw x15, 12(x2)     [riscv64.lw]
  00000010  9b 87 07 00  addiw x15, x15, 0  [riscv64.addiw]
  00000014  63 54 f0 02  bge x0, x15, 60    [riscv64.bge]
  00000018  13 07 00 00  addi x14, x0, 0    [riscv64.addi]
  0000001c  13 05 00 00  addi x10, x0, 0    [riscv64.addi]
  00000020  1b 05 75 00  addiw x10, x10, 7  [riscv64.addiw]
  00000024  1b 07 17 00  addiw x14, x14, 1  [riscv64.addiw]
  00000028  83 27 c1 00  lw x15, 12(x2)     [riscv64.lw]
  0000002c  9b 87 07 00  addiw x15, x15, 0  [riscv64.addiw]
  00000030  e3 48 f7 fe  blt x14, x15, 32   [riscv64.blt]
  00000034  13 01 01 01  addi x2, x2, 16    [riscv64.addi]
  00000038  67 80 00 00  jalr x0, 0(x1)     [riscv64.jalr]
  0000003c  13 05 00 00  addi x10, x0, 0    [riscv64.addi]
  00000040  6f f0 5f ff  jal x0, 52         [riscv64.jal]
  ########## riscv64 loop codec
  alt riscv64
    [0 cost=0] pair             riscv64-pair(){auipc-i-pair:64u}
    [1 cost=0] word             riscv64-word(){instruction:32u}
    [2 cost=0] compressed       riscv64-compressed(){compressed-instruction:16u}
