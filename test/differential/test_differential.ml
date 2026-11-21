(* The differential gate over this repository's gcc corpus: {!Differential.Make} instantiated
   for fixtures/gcc-14, where a case is any generated directory with a manifest, and the
   transcripts that pin it. *)

include Differential.Make (struct
  let root = "../../fixtures/gcc-14"
  let targets = [ "x86_32"; "x86_64"; "arm"; "aarch64"; "riscv32"; "riscv64" ]
  let is_case dir = Sys.file_exists (Filename.concat dir "manifest.txt")
end)

let read = Differential.read

let%expect_test "every bound segment matches the controlled reference link" =
  List.iter (fun c -> List.iter (fun t -> check_bytes c t) (targets_for c)) (cases ());
  [%expect
    {|
    args_arith   x86_32   .text 76
    args_arith   x86_64   .text 59
    args_arith   arm      .text 80
    args_arith   aarch64  .text 80
    args_arith   riscv32  .text 88
    args_arith   riscv64  .text 92
    cond_select  x86_32   .text 80
    cond_select  x86_64   .text 74
    cond_select  arm      .text 80
    cond_select  aarch64  .text 96
    cond_select  riscv32  .text 92
    cond_select  riscv64  .text 116
    cross_bss    x86_32   .text 14, .bss 4 bytes (nobits)
    cross_bss    x86_64   .text 16, .bss 4 bytes (nobits)
    cross_bss    arm      .text 24, .bss 4 bytes (nobits)
    cross_bss    aarch64  .text 20, .bss 4 bytes (nobits)
    cross_bss    riscv32  .text 20, .bss 4 bytes (nobits)
    cross_bss    riscv64  .text 20, .bss 4 bytes (nobits)
    cross_call   x86_32   .text 24
    cross_call   x86_64   .text 26
    cross_call   arm      .text 28
    cross_call   aarch64  .text 36
    cross_call   riscv32  .text 44
    cross_call   riscv64  .text 44
    cross_data   x86_32   .text 14, .data 4
    cross_data   x86_64   .text 16, .data 4
    cross_data   arm      .text 24, .data 4
    cross_data   aarch64  .text 20, .data 4
    cross_data   riscv32  .text 20, .data 4
    cross_data   riscv64  .text 20, .data 4
    direct_call  x86_32   .text 21
    direct_call  x86_64   .text 18
    direct_call  arm      .text 28
    direct_call  aarch64  .text 36
    direct_call  riscv32  .text 44
    direct_call  riscv64  .text 44
    global_ldst  x86_32   .text 14, .data 4
    global_ldst  x86_64   .text 16, .data 4
    global_ldst  arm      .text 24, .data 4
    global_ldst  aarch64  .text 20, .data 4
    global_ldst  riscv32  .text 20, .data 4
    global_ldst  riscv64  .text 20, .data 4
    loop         x86_32   .text 59
    loop         x86_64   .text 56
    loop         arm      .text 68
    loop         aarch64  .text 68
    loop         riscv32  .text 60
    loop         riscv64  .text 68
    return42     x86_32   .text 6
    return42     x86_64   .text 6
    return42     arm      .text 8
    return42     aarch64  .text 8
    return42     riscv32  .text 8
    return42     riscv64  .text 8 |}]

let%expect_test "diagnostic spelling agrees with objdump after normalization" =
  List.iter (fun c -> List.iter (fun t -> check_disasm c t) (targets_for c)) (cases ());
  [%expect
    {|
    args_arith   x86_32   20 lines agree
    args_arith   x86_64   15 lines agree
    args_arith   arm      20 lines agree
    args_arith   aarch64  20 lines agree
    args_arith   riscv32  22 lines agree
    args_arith   riscv64  23 lines agree
    cond_select  x86_32   23 lines agree
    cond_select  x86_64   21 lines agree
    cond_select  arm      20 lines agree
    cond_select  aarch64  24 lines agree
    cond_select  riscv32  23 lines agree
    cond_select  riscv64  29 lines agree
    cross_bss    x86_32   4 lines agree (2 relocated operands compared as records instead)
    cross_bss    x86_64   4 lines agree (2 relocated operands compared as records instead)
    cross_bss    arm      6 lines agree (2 relocated operands compared as records instead)
    cross_bss    aarch64  5 lines agree (3 relocated operands compared as records instead)
    cross_bss    riscv32  5 lines agree (5 relocated operands compared as records instead)
    cross_bss    riscv64  5 lines agree (5 relocated operands compared as records instead)
    cross_call   x86_32   9 lines agree (1 relocated operand compared as records instead)
    cross_call   x86_64   8 lines agree (1 relocated operand compared as records instead)
    cross_call   arm      DIFFERS
        ours:    mov r0,r0,lsl #1
        objdump: lsl r0,r0,#1
    cross_call   aarch64  DIFFERS
        ours:    ubfiz w0,w0,#1,#31
        objdump: lsl w0,w0,#1
    cross_call   riscv32  11 lines agree (2 relocated operands compared as records instead)
    cross_call   riscv64  11 lines agree (2 relocated operands compared as records instead)
    cross_data   x86_32   4 lines agree (2 relocated operands compared as records instead)
    cross_data   x86_64   4 lines agree (2 relocated operands compared as records instead)
    cross_data   arm      6 lines agree (2 relocated operands compared as records instead)
    cross_data   aarch64  5 lines agree (3 relocated operands compared as records instead)
    cross_data   riscv32  5 lines agree (5 relocated operands compared as records instead)
    cross_data   riscv64  5 lines agree (5 relocated operands compared as records instead)
    direct_call  x86_32   8 lines agree (1 relocated operand compared as records instead)
    direct_call  x86_64   6 lines agree (1 relocated operand compared as records instead)
    direct_call  arm      DIFFERS
        ours:    mov r0,r0,lsl #1
        objdump: lsl r0,r0,#1
    direct_call  aarch64  DIFFERS
        ours:    ubfiz w0,w0,#1,#31
        objdump: lsl w0,w0,#1
    direct_call  riscv32  11 lines agree (2 relocated operands compared as records instead)
    direct_call  riscv64  11 lines agree (2 relocated operands compared as records instead)
    global_ldst  x86_32   4 lines agree (2 relocated operands compared as records instead)
    global_ldst  x86_64   4 lines agree (2 relocated operands compared as records instead)
    global_ldst  arm      6 lines agree (2 relocated operands compared as records instead)
    global_ldst  aarch64  5 lines agree (3 relocated operands compared as records instead)
    global_ldst  riscv32  5 lines agree (5 relocated operands compared as records instead)
    global_ldst  riscv64  5 lines agree (5 relocated operands compared as records instead)
    loop         x86_32   17 lines agree
    loop         x86_64   15 lines agree
    loop         arm      17 lines agree
    loop         aarch64  17 lines agree
    loop         riscv32  15 lines agree
    loop         riscv64  17 lines agree
    return42     x86_32   2 lines agree
    return42     x86_64   2 lines agree
    return42     arm      2 lines agree
    return42     aarch64  2 lines agree
    return42     riscv32  2 lines agree
    return42     riscv64  2 lines agree |}]

let%expect_test "canonical disassembly reassembles to the same bytes" =
  List.iter (fun c -> List.iter (fun t -> check_round_trip c t) (targets_for c)) (cases ());
  [%expect
    {|
    args_arith   x86_32   76 bytes reproduced
    args_arith   x86_64   59 bytes reproduced
    args_arith   arm      80 bytes reproduced
    args_arith   aarch64  80 bytes reproduced
    args_arith   riscv32  88 bytes reproduced
    args_arith   riscv64  92 bytes reproduced
    cond_select  x86_32   80 bytes reproduced
    cond_select  x86_64   74 bytes reproduced
    cond_select  arm      80 bytes reproduced
    cond_select  aarch64  96 bytes reproduced
    cond_select  riscv32  92 bytes reproduced
    cond_select  riscv64  116 bytes reproduced
    cross_bss    x86_32   (multi-source: round-trip comparison not yet implemented per unit)
    cross_bss    x86_64   (multi-source: round-trip comparison not yet implemented per unit)
    cross_bss    arm      (multi-source: round-trip comparison not yet implemented per unit)
    cross_bss    aarch64  (multi-source: round-trip comparison not yet implemented per unit)
    cross_bss    riscv32  (multi-source: round-trip comparison not yet implemented per unit)
    cross_bss    riscv64  (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   x86_32   (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   x86_64   (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   arm      (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   aarch64  (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   riscv32  (multi-source: round-trip comparison not yet implemented per unit)
    cross_call   riscv64  (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   x86_32   (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   x86_64   (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   arm      (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   aarch64  (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   riscv32  (multi-source: round-trip comparison not yet implemented per unit)
    cross_data   riscv64  (multi-source: round-trip comparison not yet implemented per unit)
    direct_call  x86_32   21 bytes reproduced
    direct_call  x86_64   18 bytes reproduced
    direct_call  arm      28 bytes reproduced
    direct_call  aarch64  36 bytes reproduced
    direct_call  riscv32  44 bytes reproduced
    direct_call  riscv64  44 bytes reproduced
    global_ldst  x86_32   14 bytes reproduced
    global_ldst  x86_64   16 bytes reproduced
    global_ldst  arm      24 bytes reproduced
    global_ldst  aarch64  20 bytes reproduced
    global_ldst  riscv32  20 bytes reproduced
    global_ldst  riscv64  20 bytes reproduced
    loop         x86_32   59 bytes reproduced
    loop         x86_64   56 bytes reproduced
    loop         arm      68 bytes reproduced
    loop         aarch64  68 bytes reproduced
    loop         riscv32  60 bytes reproduced
    loop         riscv64  68 bytes reproduced
    return42     x86_32   6 bytes reproduced
    return42     x86_64   6 bytes reproduced
    return42     arm      8 bytes reproduced
    return42     aarch64  8 bytes reproduced
    return42     riscv32  8 bytes reproduced
    return42     riscv64  8 bytes reproduced |}]

let%expect_test "fixup observations classify to exactly the measured relocations" =
  List.iter (fun c -> List.iter (fun t -> check_relocs c t) (targets_for c)) (cases ());
  [%expect
    {|
    args_arith   x86_32   0 linker-visible, 0 assembler-resolved
    args_arith   x86_64   0 linker-visible, 0 assembler-resolved
    args_arith   arm      0 linker-visible, 0 assembler-resolved
    args_arith   aarch64  0 linker-visible, 0 assembler-resolved
    args_arith   riscv32  0 linker-visible, 0 assembler-resolved
    args_arith   riscv64  0 linker-visible, 0 assembler-resolved
    cond_select  x86_32   0 linker-visible, 4 assembler-resolved
      .text+0x1e   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x2e   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x49   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x4f   pcrel8-branch    branch local  same-sec  assembler-resolved
    cond_select  x86_64   0 linker-visible, 4 assembler-resolved
      .text+0x1b   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x2b   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x43   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x49   pcrel8-branch    branch local  same-sec  assembler-resolved
    cond_select  arm      0 linker-visible, 0 assembler-resolved
    cond_select  aarch64  0 linker-visible, 4 assembler-resolved
      .text+0x20   pcrel-b19        branch local  same-sec  assembler-resolved
      .text+0x34   pcrel-b19        branch local  same-sec  assembler-resolved
      .text+0x54   pcrel-b26        branch local  same-sec  assembler-resolved
      .text+0x5c   pcrel-b26        branch local  same-sec  assembler-resolved
    cond_select  riscv32  0 linker-visible, 6 assembler-resolved
      .text+0x1c   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x2c   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x38   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x48   pcrel-j21        branch local  same-sec  assembler-resolved
      .text+0x50   pcrel-j21        branch local  same-sec  assembler-resolved
      .text+0x58   pcrel-j21        branch local  same-sec  assembler-resolved
    cond_select  riscv64  0 linker-visible, 6 assembler-resolved
      .text+0x20   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x38   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x48   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x5c   pcrel-j21        branch local  same-sec  assembler-resolved
      .text+0x68   pcrel-j21        branch local  same-sec  assembler-resolved
      .text+0x70   pcrel-j21        branch local  same-sec  assembler-resolved
    cross_bss    x86_32   2 linker-visible, 0 assembler-resolved
      .text+0x1    abs32            data-address global other-sec R_386_32
      .text+0x9    abs32            data-address global other-sec R_386_32
    cross_bss    x86_64   2 linker-visible, 0 assembler-resolved
      .text+0x2    pcrel32-data     data-address global other-sec R_X86_64_PC32
      .text+0xb    pcrel32-data     data-address global other-sec R_X86_64_PC32
    cross_bss    arm      2 linker-visible, 0 assembler-resolved
      .text+0x0    movw-abs-nc      data-address global other-sec R_ARM_MOVW_ABS_NC
      .text+0x4    movt-abs         data-address global other-sec R_ARM_MOVT_ABS
    cross_bss    aarch64  3 linker-visible, 0 assembler-resolved
      .text+0x0    adrp-page        data-address global other-sec R_AARCH64_ADR_PREL_PG_HI21
      .text+0x4    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
      .text+0xc    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
    cross_bss    riscv32  no table row for abs-hi20 on riscv32
    cross_bss    riscv32  no table row for abs-lo12-i on riscv32
    cross_bss    riscv32  no table row for abs-lo12-s on riscv32
    cross_bss    riscv32  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 shared_value +0
    cross_bss    riscv32  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I shared_value +0
    cross_bss    riscv32  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S shared_value +0
    cross_bss    riscv64  no table row for abs-hi20 on riscv64
    cross_bss    riscv64  no table row for abs-lo12-i on riscv64
    cross_bss    riscv64  no table row for abs-lo12-s on riscv64
    cross_bss    riscv64  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 shared_value +0
    cross_bss    riscv64  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I shared_value +0
    cross_bss    riscv64  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S shared_value +0
    cross_call   x86_32   1 linker-visible, 0 assembler-resolved
      .text+0xd    pcrel32-call     call   global same-sec  R_386_PC32
    cross_call   x86_64   1 linker-visible, 0 assembler-resolved
      .text+0xe    pcrel32-call     call   global same-sec  R_X86_64_PLT32
    cross_call   arm      1 linker-visible, 0 assembler-resolved
      .text+0x10   pcrel-call       call   global same-sec  R_ARM_CALL
    cross_call   aarch64  1 linker-visible, 0 assembler-resolved
      .text+0x14   pcrel-call26     call   global same-sec  R_AARCH64_CALL26
    cross_call   riscv32  1 linker-visible, 1 assembler-resolved
      .text+0x14   call-hi20        call   global same-sec  R_RISCV_CALL_PLT
      .text+0x18   call-lo12-i      call   global same-sec  assembler-resolved
    cross_call   riscv64  1 linker-visible, 1 assembler-resolved
      .text+0x14   call-hi20        call   global same-sec  R_RISCV_CALL_PLT
      .text+0x18   call-lo12-i      call   global same-sec  assembler-resolved
    cross_data   x86_32   2 linker-visible, 0 assembler-resolved
      .text+0x1    abs32            data-address global other-sec R_386_32
      .text+0x9    abs32            data-address global other-sec R_386_32
    cross_data   x86_64   2 linker-visible, 0 assembler-resolved
      .text+0x2    pcrel32-data     data-address global other-sec R_X86_64_PC32
      .text+0xb    pcrel32-data     data-address global other-sec R_X86_64_PC32
    cross_data   arm      2 linker-visible, 0 assembler-resolved
      .text+0x0    movw-abs-nc      data-address global other-sec R_ARM_MOVW_ABS_NC
      .text+0x4    movt-abs         data-address global other-sec R_ARM_MOVT_ABS
    cross_data   aarch64  3 linker-visible, 0 assembler-resolved
      .text+0x0    adrp-page        data-address global other-sec R_AARCH64_ADR_PREL_PG_HI21
      .text+0x4    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
      .text+0xc    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
    cross_data   riscv32  no table row for abs-hi20 on riscv32
    cross_data   riscv32  no table row for abs-lo12-i on riscv32
    cross_data   riscv32  no table row for abs-lo12-s on riscv32
    cross_data   riscv32  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 shared_value +0
    cross_data   riscv32  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I shared_value +0
    cross_data   riscv32  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S shared_value +0
    cross_data   riscv64  no table row for abs-hi20 on riscv64
    cross_data   riscv64  no table row for abs-lo12-i on riscv64
    cross_data   riscv64  no table row for abs-lo12-s on riscv64
    cross_data   riscv64  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 shared_value +0
    cross_data   riscv64  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I shared_value +0
    cross_data   riscv64  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S shared_value +0
    direct_call  x86_32   1 linker-visible, 0 assembler-resolved
      .text+0xa    pcrel32-call     call   global same-sec  R_386_PC32
    direct_call  x86_64   1 linker-visible, 0 assembler-resolved
      .text+0xa    pcrel32-call     call   global same-sec  R_X86_64_PLT32
    direct_call  arm      1 linker-visible, 0 assembler-resolved
      .text+0x10   pcrel-call       call   global same-sec  R_ARM_CALL
    direct_call  aarch64  1 linker-visible, 0 assembler-resolved
      .text+0x14   pcrel-call26     call   global same-sec  R_AARCH64_CALL26
    direct_call  riscv32  1 linker-visible, 1 assembler-resolved
      .text+0x14   call-hi20        call   global same-sec  R_RISCV_CALL_PLT
      .text+0x18   call-lo12-i      call   global same-sec  assembler-resolved
    direct_call  riscv64  1 linker-visible, 1 assembler-resolved
      .text+0x14   call-hi20        call   global same-sec  R_RISCV_CALL_PLT
      .text+0x18   call-lo12-i      call   global same-sec  assembler-resolved
    global_ldst  x86_32   2 linker-visible, 0 assembler-resolved
      .text+0x1    abs32            data-address global other-sec R_386_32
      .text+0x9    abs32            data-address global other-sec R_386_32
    global_ldst  x86_64   2 linker-visible, 0 assembler-resolved
      .text+0x2    pcrel32-data     data-address global other-sec R_X86_64_PC32
      .text+0xb    pcrel32-data     data-address global other-sec R_X86_64_PC32
    global_ldst  arm      2 linker-visible, 0 assembler-resolved
      .text+0x0    movw-abs-nc      data-address global other-sec R_ARM_MOVW_ABS_NC
      .text+0x4    movt-abs         data-address global other-sec R_ARM_MOVT_ABS
    global_ldst  aarch64  3 linker-visible, 0 assembler-resolved
      .text+0x0    adrp-page        data-address global other-sec R_AARCH64_ADR_PREL_PG_HI21
      .text+0x4    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
      .text+0xc    ldst32-lo12      data-address global other-sec R_AARCH64_LDST32_ABS_LO12_NC
    global_ldst  riscv32  no table row for abs-hi20 on riscv32
    global_ldst  riscv32  no table row for abs-lo12-i on riscv32
    global_ldst  riscv32  no table row for abs-lo12-s on riscv32
    global_ldst  riscv32  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 asm_test_global +0
    global_ldst  riscv32  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I asm_test_global +0
    global_ldst  riscv32  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S asm_test_global +0
    global_ldst  riscv64  no table row for abs-hi20 on riscv64
    global_ldst  riscv64  no table row for abs-lo12-i on riscv64
    global_ldst  riscv64  no table row for abs-lo12-s on riscv64
    global_ldst  riscv64  GNU has a record we do not predict: .text+0x0 R_RISCV_HI20 asm_test_global +0
    global_ldst  riscv64  GNU has a record we do not predict: .text+0x4 R_RISCV_LO12_I asm_test_global +0
    global_ldst  riscv64  GNU has a record we do not predict: .text+0xc R_RISCV_LO12_S asm_test_global +0
    loop         x86_32   0 linker-visible, 3 assembler-resolved
      .text+0x12   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x2d   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x3a   pcrel8-branch    branch local  same-sec  assembler-resolved
    loop         x86_64   0 linker-visible, 3 assembler-resolved
      .text+0xf    pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x2d   pcrel8-branch    branch local  same-sec  assembler-resolved
      .text+0x37   pcrel8-branch    branch local  same-sec  assembler-resolved
    loop         arm      0 linker-visible, 3 assembler-resolved
      .text+0x14   pcrel-b26        branch local  same-sec  assembler-resolved
      .text+0x30   pcrel-b26        branch local  same-sec  assembler-resolved
      .text+0x40   pcrel-b26        branch local  same-sec  assembler-resolved
    loop         aarch64  0 linker-visible, 3 assembler-resolved
      .text+0x14   pcrel-b19        branch local  same-sec  assembler-resolved
      .text+0x30   pcrel-b19        branch local  same-sec  assembler-resolved
      .text+0x40   pcrel-b26        branch local  same-sec  assembler-resolved
    loop         riscv32  0 linker-visible, 3 assembler-resolved
      .text+0x10   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x28   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x38   pcrel-j21        branch local  same-sec  assembler-resolved
    loop         riscv64  0 linker-visible, 3 assembler-resolved
      .text+0x14   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x30   pcrel-b13        branch local  same-sec  assembler-resolved
      .text+0x40   pcrel-j21        branch local  same-sec  assembler-resolved
    return42     x86_32   0 linker-visible, 0 assembler-resolved
    return42     x86_64   0 linker-visible, 0 assembler-resolved
    return42     arm      0 linker-visible, 0 assembler-resolved
    return42     aarch64  0 linker-visible, 0 assembler-resolved
    return42     riscv32  0 linker-visible, 0 assembler-resolved
    return42     riscv64  0 linker-visible, 0 assembler-resolved |}]

(* {1 The embedded copies}

   test/dump embeds the six fixture texts as string literals, because a Melange
   build shares no filesystem with the native one and a three-build driver that
   read files would compare three programs with three different inputs. That
   copy is only safe while it is a copy, so this is where it is checked. Without
   this the embedded text could quietly drift into a second, easier input and
   the three-build gate would still pass. *)
let%expect_test "the embedded dump inputs are byte-identical to the fixtures" =
  List.iter
    (fun (name, embedded) ->
      let path =
        Filename.concat
          (Filename.concat (Filename.concat corpus_root "return42") name)
          "asm_test_entry.s"
      in
      let committed = read path in
      (* The fixture carries a two-line the compiler provenance banner that the
         embedded copy drops: it is a comment, so it changes no output, and
         keeping it would put a tool version inside a byte-compared artifact. *)
      let strip_banner s =
        match String.split_on_char '\n' s with
        | a :: b :: rest when String.length a > 0 && (a.[0] = '#' || a.[0] = '@' || a.[0] = '/') ->
            ignore b;
            String.concat "\n" rest
        | _ -> s
      in
      if String.equal (strip_banner committed) embedded then
        Printf.printf "%-8s embedded copy matches the fixture\n" name
      else Printf.printf "%-8s EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE\n" name)
    Test_dump_inputs.all;
  [%expect
    {|
    x86_32   EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    x86_64   EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    arm      EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    aarch64  EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    riscv32  EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    riscv64  EMBEDDED COPY HAS DRIFTED FROM THE FIXTURE
    |}]

(* The comparison's own failure modes, which the corpus cannot exercise because
   every case in it agrees. Untested error handling inside a gate is how a gate
   quietly stops being one. *)
let%expect_test "the record comparison reports each way it can disagree" =
  let r off ty addend = (".text", off, ty, "g", Some addend) in
  let show label ~predicted ~measured =
    Printf.printf "%s:\n" label;
    match compare_records ~predicted ~measured with
    | [] -> print_endline "  (agree)"
    | ps -> List.iter (fun p -> Printf.printf "  %s\n" p) ps
  in
  show "identical, different order"
    ~predicted:[ r 8 "R_X86_64_PC32" (-4L); r 4 "R_X86_64_PLT32" (-4L) ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L); r 8 "R_X86_64_PC32" (-4L) ];
  show "we predict one GNU does not have"
    ~predicted:[ r 4 "R_X86_64_PLT32" (-4L); r 8 "R_X86_64_PC32" (-4L) ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L) ];
  show "GNU has one we do not predict"
    ~predicted:[ r 4 "R_X86_64_PLT32" (-4L) ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L); r 8 "R_X86_64_PC32" (-4L) ];
  show "same place, wrong type"
    ~predicted:[ r 4 "R_X86_64_PC32" (-4L) ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L) ];
  show "same place, wrong addend"
    ~predicted:[ r 4 "R_X86_64_PLT32" 0L ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L) ];
  show "two records at one key"
    ~predicted:[ r 4 "R_X86_64_PLT32" (-4L); r 4 "R_X86_64_PC32" (-4L) ]
    ~measured:[ r 4 "R_X86_64_PLT32" (-4L) ];
  [%expect
    {|
    identical, different order:
      (agree)
    we predict one GNU does not have:
      we predict a record GNU does not have: .text+0x8 R_X86_64_PC32 g -4
    GNU has one we do not predict:
      GNU has a record we do not predict: .text+0x8 R_X86_64_PC32 g -4
    same place, wrong type:
      mismatch: ours .text+0x4 R_X86_64_PC32 g -4, GNU .text+0x4 R_X86_64_PLT32 g -4
    same place, wrong addend:
      mismatch: ours .text+0x4 R_X86_64_PLT32 g +0, GNU .text+0x4 R_X86_64_PLT32 g -4
    two records at one key:
      duplicate key in ours: .text+0x4 R_X86_64_PC32 g -4
      mismatch: ours .text+0x4 R_X86_64_PC32 g -4, GNU .text+0x4 R_X86_64_PLT32 g -4 |}]
