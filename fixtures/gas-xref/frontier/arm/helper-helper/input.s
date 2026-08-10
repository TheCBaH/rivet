/* Execution ABI v1 helper - profile 3, ARM / A32 AAPCS.
 *
 * The same document and the same order as the two x86 helpers. Three things
 * are genuinely new here rather than a change of spelling:
 *
 *   - §12 cache synchronization. ARM's instruction cache is not coherent with
 *     stores, so the executable ranges go through the ARM-private cacheflush
 *     syscall after mprotect. The throwaway C probe that established the
 *     three-segment mapping used a compiler builtin this code does not share,
 *     so it is no evidence for this path; the behavioural smoke is the fact
 *     that a freshly copied return-42 payload executes at all.
 *
 *   - §10.3's entry rule. An ARM entry must be word-aligned, which also
 *     forces bit 0 clear and therefore ARM state rather than Thumb - matching
 *     the -marm fixtures. Subcode 37 has no case on x86 and does here.
 *
 *   - §5's release barrier. ARM is weakly ordered, so a dmb ish precedes
 *     *every* commit store, including both record_state commits and not only
 *     status and runner_error. Without it the host could recover a validated
 *     record whose expected[] had not landed, or a returned record without the
 *     captured value.
 *
 * 32-bit, so the u64 wire fields are low/high pairs with adds/adcs, exactly as
 * in x86_32.s, and the same proofs about which values have been shown to fit
 * in 32 bits apply at the same points.
 *
 * Invocation:  helper <manifest-path> <result-path> [test-entry-token] */

        .syntax unified
        .arch   armv7-a
        .arm

        .include "abi-v1.inc"

/* Linux ARM EABI syscall numbers; the number goes in r7 and the call is svc #0. */
        .equ    SYS_exit_group, 248
        .equ    SYS_open,       5
        .equ    SYS_ftruncate,  93
        .equ    SYS_mprotect,   125
        .equ    SYS_mmap2,      192
        .equ    SYS_fstat64,    197
/* __ARM_NR_cacheflush: an ARM-private syscall, not a generic Linux one. */
        .equ    SYS_cacheflush, 0x0f0002

/* struct stat64, ARM EABI. st_size is a long long and the ABI aligns it to
 * eight bytes, so it sits at 48 rather than at the 44 an i386 layout would
 * put it at - the one place where "the same 32-bit code" would be wrong. */
        .equ    STAT_ST_SIZE,   48
        .equ    STAT_SIZE,      104

        .equ    WINDOW_BASE,    WINDOW_BASE_32
        .equ    WINDOW_END,     WINDOW_BASE_32 + WINDOW_SIZE
        .equ    RESULT_NORM,    WINDOW_BASE_32 + OFF_RESULT
        .equ    GUARD_NORM,     WINDOW_BASE_32 + OFF_GUARD
        .equ    STACK_NORM,     WINDOW_BASE_32 + OFF_STACK

/* Constants and symbol addresses via movw/movt rather than a literal pool:
 * ldr's pc-relative range is 4 KiB and this function is longer than that, so
 * pools would have to be threaded through the control flow by hand. */
        .macro  LDC reg, val
        movw    \reg, #:lower16:\val
        movt    \reg, #:upper16:\val
        .endm

/* The value of a .bss word. */
        .macro  LDV reg, sym
        LDC     \reg, \sym
        ldr     \reg, [\reg]
        .endm

        .macro  STV reg, sym, tmp
        LDC     \tmp, \sym
        str     \reg, [\tmp]
        .endm

        .macro  SYSCALL nr
        LDC     r7, \nr
        svc     #0
        .endm

/* A syscall return in [-4095, -1] is an errno; anything else, including a
 * mapping address with the top bit set, is success. */
        .macro  SYSCALL_FAILED label
        cmn     r0, #(ERRNO_LIMIT+1)    /* r0 > -4096, unsigned: the errno band */
        bhi     \label
        .endm

        .macro  FAIL_NO_RECORD cls
        mov     r0, #\cls
        STV     r0, exit_code, r1
        b       exit_class
        .endm

        .macro  FAIL cls, sub
        mov     r0, #\cls
        STV     r0, exit_code, r1
        LDC     r0, \sub
        STV     r0, fail_subcode, r1
        b       fail_pre_run
        .endm

/* r11 <- the descriptor at index r9. */
        .macro  DESC
        mov     r11, #D_SIZE
        mul     r11, r9, r11
        add     r11, r11, r4
        add     r11, r11, #M_HEADER_SIZE
        .endm

        .section .rodata
        .align  2
manifest_magic:
        .ascii  "ASMEXE\0\1"
result_magic:
        .ascii  "ASMRES\0\1"
pagesz_wrong_token:
        .ascii  "pgsz-bad"
pagesz_absent_token:
        .ascii  "pgsz-abs"

        .bss
        .align  3
result_fd:      .skip   4
input_fd:       .skip   4
manifest_path:  .skip   4
result_path:    .skip   4
saved_pagesz:   .skip   4
pagesz_found:   .skip   4
control_alias:  .skip   4
exit_code:      .skip   4
fail_subcode:   .skip   4
cursor:         .skip   4
helper_sp:      .skip   4
guest_sp:       .skip   4
stack_size:     .skip   4
entry_addr:     .skip   4
result_lo:      .skip   4
expected_lo:    .skip   4
expected_hi:    .skip   4
res_start:      .skip   4
res_end:        .skip   4
stk_start:      .skip   4
stk_end:        .skip   4
seg_start:      .skip   4 * N_SEGMENTS_MAX
seg_end:        .skip   4 * N_SEGMENTS_MAX
/* mmap2 wants six registers including two this code keeps state in, so its
 * arguments are marshalled through here and the wrapper saves them. */
mm_addr:        .skip   4
mm_len:         .skip   4
mm_prot:        .skip   4
mm_flags:       .skip   4
mm_fd:          .skip   4
statbuf:        .skip   STAT_SIZE

        .text
        .globl  _start
        .type   _start, %function

/* Live registers through the validator:
 *   r4  manifest base      r5  st_size          r6  control alias
 *   r8  n_segments         r9  loop index       r10 meta_end
 *   r11 descriptor pointer r0-r3, r12 scratch
 * The mmap2 wrapper preserves r4-r6, so these survive every syscall. */

_start:
/* ---- process entry: argv and the auxiliary vector ---- */
        ldr     r0, [sp]                        /* argc */
        cmp     r0, #3
        bge     .Largs_ok
        FAIL_NO_RECORD CLS_IO
.Largs_ok:
        ldr     r1, [sp, #8]
        STV     r1, manifest_path, r2
        ldr     r1, [sp, #12]
        STV     r1, result_path, r2

        add     r1, sp, #4                      /* &argv[0] */
        add     r1, r1, r0, lsl #2
        add     r1, r1, #4                      /* &envp[0] = argv + argc + 1 */
.Lskip_env:
        ldr     r2, [r1], #4
        cmp     r2, #0
        bne     .Lskip_env
.Lauxv:
        ldr     r2, [r1]                        /* a_type */
        cmp     r2, #0
        beq     .Lauxv_done
        cmp     r2, #AT_PAGESZ
        bne     .Lauxv_next
        ldr     r2, [r1, #4]
        STV     r2, saved_pagesz, r3
        mov     r2, #1
        STV     r2, pagesz_found, r3
.Lauxv_next:
        add     r1, r1, #8
        b       .Lauxv
.Lauxv_done:

/* The §14.2 class 69 test entry: see x86_64.s for why it exists. */
        ldr     r0, [sp]
        cmp     r0, #4
        blt     .Lno_test_entry
        ldr     r1, [sp, #16]                   /* argv[3] */
        ldr     r2, [r1]
        ldr     r3, [r1, #4]
        LDC     r0, pagesz_wrong_token
        ldr     r12, [r0]
        cmp     r2, r12
        bne     .Ltest_absent
        ldr     r12, [r0, #4]
        cmp     r3, r12
        bne     .Ltest_absent
        mov     r2, #(2*PAGE_SIZE)
        STV     r2, saved_pagesz, r3
        b       .Lno_test_entry
.Ltest_absent:
        LDC     r0, pagesz_absent_token
        ldr     r12, [r0]
        cmp     r2, r12
        bne     .Lno_test_entry
        ldr     r12, [r0, #4]
        cmp     r3, r12
        bne     .Lno_test_entry
        mov     r2, #0
        STV     r2, pagesz_found, r3
.Lno_test_entry:

/* ---- §4 step 1 ---- */
        LDV     r0, result_path
        mov     r1, #(O_RDWR|O_CREAT|O_EXCL)
        mov     r2, #RESULT_MODE
        SYSCALL SYS_open
        SYSCALL_FAILED .Lresult_open_failed
        STV     r0, result_fd, r1

        mov     r1, #R_PAGE_SIZE
        SYSCALL SYS_ftruncate
        SYSCALL_FAILED .Lresult_ftruncate_failed

/* ---- §4 step 2: reserve the window ---- */
        LDC     r0, WINDOW_BASE
        STV     r0, mm_addr, r1
        LDC     r0, WINDOW_SIZE
        STV     r0, mm_len, r1
        mov     r0, #PROT_NONE
        STV     r0, mm_prot, r1
        LDC     r0, (MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE)
        STV     r0, mm_flags, r1
        mvn     r0, #0
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lwindow_failed
        LDC     r1, WINDOW_BASE
        cmp     r0, r1
        bne     .Lwindow_failed

/* ---- §4 step 3: the control alias ---- */
        mov     r0, #0
        STV     r0, mm_addr, r1
        mov     r0, #R_PAGE_SIZE
        STV     r0, mm_len, r1
        mov     r0, #(PROT_READ|PROT_WRITE)
        STV     r0, mm_prot, r1
        mov     r0, #MAP_SHARED
        STV     r0, mm_flags, r1
        LDV     r0, result_fd
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lcontrol_failed
        mov     r6, r0
        STV     r6, control_alias, r1

/* ---- §4 step 4: the bootstrap record ---- */
        LDC     r0, result_magic
        ldr     r1, [r0]
        str     r1, [r6, #R_MAGIC]
        ldr     r1, [r0, #4]
        str     r1, [r6, #R_MAGIC+4]
        mov     r1, #ABI_VERSION
        strh    r1, [r6, #R_ABI_VERSION]

/* ---- §4 step 4b ---- */
        LDV     r0, pagesz_found
        cmp     r0, #0
        bne     .Lpagesz_present
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_ABSENT
.Lpagesz_present:
        LDV     r0, saved_pagesz
        mov     r1, #PAGE_SIZE
        cmp     r0, r1
        beq     .Lpagesz_ok
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_WRONG
.Lpagesz_ok:

/* ---- §4 step 5 / §10 stage 1 ---- */
        LDV     r0, manifest_path
        mov     r1, #O_RDONLY
        mov     r2, #0
        SYSCALL SYS_open
        SYSCALL_FAILED .Linput_open_failed
        STV     r0, input_fd, r1

        LDC     r1, statbuf
        SYSCALL SYS_fstat64
        SYSCALL_FAILED .Linput_fstat_failed

        LDC     r0, statbuf
        ldr     r1, [r0, #STAT_ST_SIZE+4]
        cmp     r1, #0
        bne     .Lsize_too_big
        ldr     r5, [r0, #STAT_ST_SIZE]
        cmp     r5, #MANIFEST_MIN
        bhs     .Lsize_lower_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_BELOW_MINIMUM
.Lsize_lower_ok:
        LDC     r1, MANIFEST_MAX
        cmp     r5, r1
        bls     .Lsize_upper_ok
.Lsize_too_big:
        FAIL    CLS_MANIFEST, SUB_SIZE_ABOVE_MAXIMUM
.Lsize_upper_ok:

/* ---- §10 stage 2 ---- */
        mov     r0, #0
        STV     r0, mm_addr, r1
        STV     r5, mm_len, r1
        mov     r0, #PROT_READ
        STV     r0, mm_prot, r1
        mov     r0, #MAP_PRIVATE
        STV     r0, mm_flags, r1
        LDV     r0, input_fd
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Linput_mmap_failed
        mov     r4, r0

/* ---- §10 stage 3: the fixed header ---- */
        LDC     r0, manifest_magic
        ldr     r1, [r0]
        ldr     r2, [r4, #M_MAGIC]
        cmp     r1, r2
        bne     .Lbad_magic
        ldr     r1, [r0, #4]
        ldr     r2, [r4, #M_MAGIC+4]
        cmp     r1, r2
        beq     .Lmagic_ok
.Lbad_magic:
        FAIL    CLS_MANIFEST, SUB_BAD_MAGIC
.Lmagic_ok:
        ldrh    r0, [r4, #M_ABI_VERSION]
        cmp     r0, #ABI_VERSION
        beq     .Lversion_ok
        FAIL    CLS_MANIFEST, SUB_BAD_ABI_VERSION
.Lversion_ok:
        ldrh    r0, [r4, #M_PROFILE_ID]
        cmp     r0, #PROF_ARM
        beq     .Lprofile_ok
        FAIL    CLS_MANIFEST, SUB_BAD_PROFILE_ID
.Lprofile_ok:
        ldr     r0, [r4, #M_TOTAL_LEN]
        cmp     r0, r5
        beq     .Ltotal_ok
        FAIL    CLS_MANIFEST, SUB_TOTAL_LEN_MISMATCH
.Ltotal_ok:
        ldrh    r8, [r4, #M_N_SEGMENTS]
        cmp     r8, #0
        beq     .Lnseg_bad
        cmp     r8, #N_SEGMENTS_MAX
        bls     .Lnseg_ok
.Lnseg_bad:
        FAIL    CLS_MANIFEST, SUB_N_SEGMENTS_RANGE
.Lnseg_ok:
        ldrh    r0, [r4, #M_N_EXPECTED]
        cmp     r0, #1
        beq     .Lnexp_ok
        FAIL    CLS_MANIFEST, SUB_N_EXPECTED_RANGE
.Lnexp_ok:
        ldr     r0, [r4, #M_RESULT_SIZE]
        cmp     r0, #R_RECORD_SIZE
        beq     .Lressize_ok
        FAIL    CLS_MANIFEST, SUB_RESULT_SIZE_WRONG
.Lressize_ok:
        ldr     r0, [r4, #M_TIMEOUT_MS]
        LDC     r1, TIMEOUT_MIN
        cmp     r0, r1
        blo     .Ltimeout_bad
        LDC     r1, TIMEOUT_MAX
        cmp     r0, r1
        bls     .Ltimeout_ok
.Ltimeout_bad:
        FAIL    CLS_MANIFEST, SUB_TIMEOUT_RANGE
.Ltimeout_ok:
        ldr     r0, [r4, #M_RESERVED]
        cmp     r0, #0
        beq     .Lhdr_reserved_ok
        FAIL    CLS_MANIFEST, SUB_HEADER_RESERVED_NONZERO
.Lhdr_reserved_ok:
        add     r0, r4, #M_EXPECTED+8
        mov     r1, #56
        bl      or_bytes
        cmp     r0, #0
        beq     .Lexp_tail_ok
        FAIL    CLS_MANIFEST, SUB_EXPECTED_TAIL_NONZERO
.Lexp_tail_ok:

/* ---- §10 stage 4 ---- */
        mov     r0, #D_SIZE
        mul     r10, r8, r0
        add     r10, r10, #M_HEADER_SIZE
        cmp     r10, r5
        bls     .Lmeta_ok
        FAIL    CLS_MANIFEST, SUB_DESCRIPTOR_TABLE_OVERRUNS
.Lmeta_ok:

/* ---- §10 stage 5 ----
 *
 * A sum whose high word is zero and which produced no carry cannot come from
 * operands whose high words are not both zero, so payload_off and init_len are
 * 32-bit quantities everywhere below this point. */
        mov     r9, #0
.Lstage5:
        cmp     r9, r8
        bhs     .Lstage5_done
        DESC
        ldr     r0, [r11, #D_PAYLOAD_OFF]
        ldr     r1, [r11, #D_PAYLOAD_OFF+4]
        ldr     r2, [r11, #D_INIT_LEN]
        ldr     r3, [r11, #D_INIT_LEN+4]
        adds    r0, r0, r2
        adcs    r1, r1, r3
        bcs     .Lpayload_overruns
        cmp     r1, #0
        bne     .Lpayload_overruns
        cmp     r0, r5
        bls     .Lstage5_next
.Lpayload_overruns:
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERRUNS
.Lstage5_next:
        add     r9, r9, #1
        b       .Lstage5
.Lstage5_done:

/* ---- §10 stage 6 ---- */
        mov     r9, #0
.Lstage6:
        cmp     r9, r8
        bhs     .Lstage6_done
        DESC
        ldr     r0, [r11, #D_PAYLOAD_OFF]
        tst     r0, #7
        beq     .Lstage6_aligned
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_MISALIGNED
.Lstage6_aligned:
        cmp     r0, #0
        beq     .Lstage6_pairing
        cmp     r0, r10
        bhs     .Lstage6_pairing
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_IN_METADATA
.Lstage6_pairing:
        ldr     r1, [r11, #D_INIT_LEN]
        cmp     r0, #0
        movne   r2, #1
        moveq   r2, #0
        cmp     r1, #0
        movne   r3, #1
        moveq   r3, #0
        cmp     r2, r3
        beq     .Lstage6_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_PAIRING
.Lstage6_next:
        add     r9, r9, #1
        b       .Lstage6
.Lstage6_done:

/* ---- §10 stage 7: pairwise payload non-overlap ---- */
        mov     r9, #0
.Lstage7_i:
        cmp     r9, r8
        bhs     .Lstage7_done
        DESC
        ldr     r1, [r11, #D_INIT_LEN]
        cmp     r1, #0
        beq     .Lstage7_i_next
        ldr     r0, [r11, #D_PAYLOAD_OFF]       /* start_i */
        add     r1, r1, r0                      /* end_i */
        add     r12, r9, #1                     /* j */
.Lstage7_j:
        cmp     r12, r8
        bhs     .Lstage7_i_next
        mov     r2, #D_SIZE
        mul     r2, r12, r2
        add     r2, r2, r4
        add     r2, r2, #M_HEADER_SIZE
        ldr     r3, [r2, #D_INIT_LEN]
        cmp     r3, #0
        beq     .Lstage7_j_next
        ldr     r2, [r2, #D_PAYLOAD_OFF]        /* start_j */
        add     r3, r3, r2                      /* end_j */
        cmp     r0, r3                          /* start_i >= end_j ? */
        bhs     .Lstage7_j_next
        cmp     r2, r1                          /* start_j >= end_i ? */
        bhs     .Lstage7_j_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERLAP
.Lstage7_j_next:
        add     r12, r12, #1
        b       .Lstage7_j
.Lstage7_i_next:
        add     r9, r9, #1
        b       .Lstage7_i
.Lstage7_done:

/* ---- §10 stage 8: canonical packing ---- */
        STV     r10, cursor, r0
        mov     r9, #0
.Lstage8:
        cmp     r9, r8
        bhs     .Lstage8_done
        DESC
        ldr     r1, [r11, #D_INIT_LEN]
        cmp     r1, #0
        beq     .Lstage8_next
        LDV     r2, cursor
        add     r0, r2, #7
        bic     r0, r0, #7                      /* expected payload offset */
        ldr     r3, [r11, #D_PAYLOAD_OFF]
        cmp     r0, r3
        beq     .Lstage8_tight
        FAIL    CLS_MANIFEST, SUB_PACKING_NOT_TIGHT
.Lstage8_tight:
        subs    r1, r0, r2                      /* alignment gap */
        beq     .Lstage8_nogap
        add     r0, r4, r2
        bl      or_bytes
        cmp     r0, #0
        beq     .Lstage8_nogap
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_nogap:
        DESC
        ldr     r0, [r11, #D_PAYLOAD_OFF]
        ldr     r1, [r11, #D_INIT_LEN]
        add     r0, r0, r1
        STV     r0, cursor, r2
.Lstage8_next:
        add     r9, r9, #1
        b       .Lstage8
.Lstage8_done:
        /* Length before padding: a file short of the aligned end would fault
           the helper rather than produce a subcode. */
        LDV     r2, cursor
        add     r0, r2, #7
        bic     r0, r0, #7
        cmp     r0, r5
        beq     .Lstage8_len_ok
        FAIL    CLS_MANIFEST, SUB_TRAILING_BYTES
.Lstage8_len_ok:
        subs    r1, r0, r2
        beq     .Lstage8_pad_ok
        add     r0, r4, r2
        bl      or_bytes
        cmp     r0, #0
        beq     .Lstage8_pad_ok
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_pad_ok:

/* ---- §10 stage 9: resource caps ---- */
        mov     r9, #0
.Lstage9:
        cmp     r9, r8
        bhs     .Lstage9_done
        DESC
        ldr     r0, [r11, #D_INIT_LEN]
        LDC     r1, INIT_LEN_MAX
        cmp     r0, r1
        bls     .Lstage9_zero
        FAIL    CLS_MANIFEST, SUB_INIT_LEN_TOO_LARGE
.Lstage9_zero:
        ldr     r0, [r11, #D_ZERO_LEN+4]
        cmp     r0, #0
        bne     .Lzero_len_big
        ldr     r0, [r11, #D_ZERO_LEN]
        LDC     r1, ZERO_LEN_MAX
        cmp     r0, r1
        bls     .Lstage9_next
.Lzero_len_big:
        FAIL    CLS_MANIFEST, SUB_ZERO_LEN_TOO_LARGE
.Lstage9_next:
        add     r9, r9, #1
        b       .Lstage9
.Lstage9_done:

/* ---- §10.2 geometry ---- */
        mov     r9, #0
.Lgeom:
        cmp     r9, r8
        bhs     .Lgeom_done
        DESC
        ldr     r0, [r11, #D_INIT_LEN]
        ldr     r1, [r11, #D_ZERO_LEN]
        orrs    r2, r0, r1
        bne     .Lgeom_nonempty
        FAIL    CLS_MANIFEST, SUB_SEGMENT_EMPTY
.Lgeom_nonempty:
        ldr     r2, [r11, #D_ALIGN]
        cmp     r2, #0
        beq     .Lgeom_align_bad
        sub     r3, r2, #1
        tst     r2, r3
        beq     .Lgeom_align_ok
.Lgeom_align_bad:
        FAIL    CLS_MANIFEST, SUB_ALIGN_NOT_POWER_OF_TWO
.Lgeom_align_ok:
        /* align is a u32, so only vaddr's low word can violate it. */
        ldr     r2, [r11, #D_VADDR]
        tst     r2, r3
        beq     .Lgeom_vaddr_ok
        FAIL    CLS_MANIFEST, SUB_VADDR_MISALIGNED
.Lgeom_vaddr_ok:
        ldrb    r2, [r11, #D_PERMS]
        tst     r2, #0xf8
        beq     .Lgeom_perms_known
        FAIL    CLS_MANIFEST, SUB_PERMS_UNKNOWN_BITS
.Lgeom_perms_known:
        and     r3, r2, #(PERM_W|PERM_X)
        cmp     r3, #(PERM_W|PERM_X)
        bne     .Lgeom_perms_ok
        FAIL    CLS_MANIFEST, SUB_PERMS_WRITE_EXECUTE
.Lgeom_perms_ok:
        add     r2, r0, r1                      /* init_len + zero_len, <= 1.25 MiB */
        ldr     r0, [r11, #D_VADDR]
        ldr     r1, [r11, #D_VADDR+4]
        adds    r0, r0, r2
        adcs    r1, r1, #0
        bcs     .Lgeom_overflow
        mov    r12, #0xf00
        orr    r12, r12, #0xff
        adds    r0, r0, r12
        adcs    r1, r1, #0
        bcs     .Lgeom_overflow
        bic     r0, r0, #0xff
        bic     r0, r0, #0xf00          /* map_end low word */
        cmp     r1, #0
        bne     .Lgeom_outside                  /* map_end >= 2^32 */
        mov     r3, r0                          /* map_end */
        ldr     r1, [r11, #D_VADDR+4]
        cmp     r1, #0
        bne     .Lgeom_outside                  /* vaddr >= 2^32 */
        ldr     r0, [r11, #D_VADDR]
        bic     r0, r0, #0xff
        bic     r0, r0, #0xf00          /* map_start */
        LDC     r2, WINDOW_BASE
        cmp     r0, r2
        blo     .Lgeom_outside
        LDC     r2, WINDOW_END
        cmp     r3, r2
        bhi     .Lgeom_outside
        LDC     r2, seg_start
        str     r0, [r2, r9, lsl #2]
        LDC     r2, seg_end
        str     r3, [r2, r9, lsl #2]
        add     r9, r9, #1
        b       .Lgeom
.Lgeom_overflow:
        FAIL    CLS_MANIFEST, SUB_ADDRESS_OVERFLOW
.Lgeom_outside:
        FAIL    CLS_MANIFEST, SUB_OUTSIDE_WINDOW
.Lgeom_done:

/* ---- §10.2 collisions ----
 *
 * The result page and the stack come from unvalidated fields, so both ranges
 * saturate rather than wrap: a wrapped range would appear to sit at address
 * zero and could collide with a segment nowhere near it. */
        ldr     r1, [r4, #M_RESULT_ADDR+4]
        cmp     r1, #0
        beq     .Lres_in_range
        LDC     r0, 0xFFFFF000
        STV     r0, res_start, r2
        mvn     r0, #0
        STV     r0, res_end, r2
        b       .Lres_done
.Lres_in_range:
        ldr     r0, [r4, #M_RESULT_ADDR]
        STV     r0, result_lo, r2
        bic     r0, r0, #0xff
        bic     r0, r0, #0xf00
        STV     r0, res_start, r2
        adds    r0, r0, #PAGE_SIZE
        movcs   r0, #-1
        STV     r0, res_end, r2
.Lres_done:
        ldr     r0, [r4, #M_STACK_SIZE]
        STV     r0, stack_size, r2
        LDC     r1, STACK_NORM
        STV     r1, stk_start, r2
        mov    r12, #0xf00
        orr    r12, r12, #0xff
        adds    r0, r0, r12
        bcs     .Lstk_saturate
        bic     r0, r0, #0xff
        bic     r0, r0, #0xf00
        adds    r0, r0, r1
        bcc     .Lstk_store
.Lstk_saturate:
        mvn     r0, #0
.Lstk_store:
        STV     r0, stk_end, r2

        mov     r9, #0
.Lcoll:
        cmp     r9, r8
        bhs     .Lcoll_done
        LDC     r0, seg_start
        ldr     r0, [r0, r9, lsl #2]
        LDC     r1, seg_end
        ldr     r1, [r1, r9, lsl #2]
        LDV     r2, res_end
        cmp     r0, r2
        bhs     .Lcoll_stack
        LDV     r2, res_start
        cmp     r1, r2
        bls     .Lcoll_stack
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_RESULT
.Lcoll_stack:
        LDV     r2, stk_end
        cmp     r0, r2
        bhs     .Lcoll_guard
        LDV     r2, stk_start
        cmp     r1, r2
        bls     .Lcoll_guard
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_STACK
.Lcoll_guard:
        LDC     r2, (GUARD_NORM+PAGE_SIZE)
        cmp     r0, r2
        bhs     .Lcoll_next
        LDC     r2, GUARD_NORM
        cmp     r1, r2
        bls     .Lcoll_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_GUARD
.Lcoll_next:
        add     r9, r9, #1
        b       .Lcoll
.Lcoll_done:
        LDV     r0, res_start
        LDV     r2, stk_end
        cmp     r0, r2
        bhs     .Lcoll_rs_ok
        LDV     r0, res_end
        LDV     r2, stk_start
        cmp     r0, r2
        bls     .Lcoll_rs_ok
        FAIL    CLS_MANIFEST, SUB_COLLIDE_RESULT_STACK
.Lcoll_rs_ok:
        mov     r9, #0
.Lcoll_ii:
        cmp     r9, r8
        bhs     .Lcoll_ii_done
        LDC     r0, seg_start
        ldr     r0, [r0, r9, lsl #2]
        LDC     r1, seg_end
        ldr     r1, [r1, r9, lsl #2]
        add     r12, r9, #1
.Lcoll_jj:
        cmp     r12, r8
        bhs     .Lcoll_ii_next
        LDC     r2, seg_start
        ldr     r2, [r2, r12, lsl #2]
        LDC     r3, seg_end
        ldr     r3, [r3, r12, lsl #2]
        cmp     r0, r3
        bhs     .Lcoll_jj_next
        cmp     r2, r1
        bhs     .Lcoll_jj_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_SEGMENT
.Lcoll_jj_next:
        add     r12, r12, #1
        b       .Lcoll_jj
.Lcoll_ii_next:
        add     r9, r9, #1
        b       .Lcoll_ii
.Lcoll_ii_done:

/* ---- §10.2 cross-region well-formedness ----
 *
 * The order inside this group is the one §10.2 now states: the 32-bit address
 * rule first, so it is reachable at all. */
        ldr     r0, [r4, #M_ENTRY_ADDR+4]
        ldr     r1, [r4, #M_RESULT_ADDR+4]
        orrs    r0, r0, r1
        beq     .Lhigh_bits_ok
        FAIL    CLS_MANIFEST, SUB_ADDRESS_HIGH_BITS_SET
.Lhigh_bits_ok:
        ldr     r0, [r4, #M_ENTRY_ADDR]
        STV     r0, entry_addr, r1
        mov     r9, #0
.Lentry:
        cmp     r9, r8
        bhs     .Lentry_bad
        DESC
        ldrb    r0, [r11, #D_PERMS]
        tst     r0, #PERM_X
        beq     .Lentry_next
        LDV     r3, entry_addr
        ldr     r0, [r11, #D_VADDR]
        cmp     r3, r0
        blo     .Lentry_next
        ldr     r1, [r11, #D_INIT_LEN]
        add     r0, r0, r1
        cmp     r3, r0
        blo     .Lentry_found
.Lentry_next:
        add     r9, r9, #1
        b       .Lentry
.Lentry_bad:
        FAIL    CLS_MANIFEST, SUB_ENTRY_NOT_IN_X_SEGMENT
.Lentry_found:
        /* §10.3: an ARM entry is word-aligned, which also forces bit 0 clear
           and therefore ARM state rather than Thumb - matching the -marm
           fixtures. This is the profile rule x86 does not have. */
        LDV     r0, entry_addr
        tst     r0, #3
        beq     .Lentry_profile_ok
        FAIL    CLS_MANIFEST, SUB_ENTRY_PROFILE_RULE
.Lentry_profile_ok:
        LDV     r0, result_lo
        lsls     r12, r0, #20
        beq     .Lresult_aligned
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_MISALIGNED
.Lresult_aligned:
        LDC     r1, RESULT_NORM
        cmp     r0, r1
        beq     .Lresult_normative
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_NOT_NORMATIVE
.Lresult_normative:
        LDV     r0, stack_size
        cmp     r0, #0
        beq     .Lstack_bad
        lsls     r12, r0, #20
        bne     .Lstack_bad
        LDC     r1, STACK_SIZE_MIN
        cmp     r0, r1
        blo     .Lstack_bad
        LDC     r1, STACK_SIZE_MAX
        cmp     r0, r1
        bls     .Lstack_ok
.Lstack_bad:
        FAIL    CLS_MANIFEST, SUB_STACK_SIZE_INVALID
.Lstack_ok:
        /* Subcode 41: both facts it could still catch are fixed at assembly
           time, so they are asserted there. See x86_64.s. */
        .if     (GUARD_NORM + PAGE_SIZE) != STACK_NORM
        .error  "guard page is not immediately below the guest stack"
        .endif
        .if     (STACK_NORM + STACK_SIZE_MAX) > (WINDOW_BASE + WINDOW_SIZE)
        .error  "a maximum-size stack does not fit inside the reserved window"
        .endif

/* ---- §4 step 7: install the mappings ---- */
        LDC     r0, RESULT_NORM
        STV     r0, mm_addr, r1
        mov     r0, #R_PAGE_SIZE
        STV     r0, mm_len, r1
        mov     r0, #(PROT_READ|PROT_WRITE)
        STV     r0, mm_prot, r1
        mov     r0, #(MAP_SHARED|MAP_FIXED)
        STV     r0, mm_flags, r1
        LDV     r0, result_fd
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lresult_alias_failed

        LDC     r0, STACK_NORM
        STV     r0, mm_addr, r1
        LDV     r0, stack_size
        STV     r0, mm_len, r1
        mov     r0, #(PROT_READ|PROT_WRITE)
        STV     r0, mm_prot, r1
        mov     r0, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        STV     r0, mm_flags, r1
        mvn     r0, #0
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lstack_map_failed

        LDC     r0, GUARD_NORM
        STV     r0, mm_addr, r1
        mov     r0, #PAGE_SIZE
        STV     r0, mm_len, r1
        mov     r0, #PROT_NONE
        STV     r0, mm_prot, r1
        mov     r0, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        STV     r0, mm_flags, r1
        mvn     r0, #0
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lguard_map_failed

        mov     r9, #0
.Lmap_seg:
        cmp     r9, r8
        bhs     .Lmap_seg_done
        LDC     r0, seg_start
        ldr     r0, [r0, r9, lsl #2]
        STV     r0, mm_addr, r1
        LDC     r1, seg_end
        ldr     r1, [r1, r9, lsl #2]
        sub     r1, r1, r0
        STV     r1, mm_len, r2
        mov     r0, #(PROT_READ|PROT_WRITE)
        STV     r0, mm_prot, r1
        mov     r0, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        STV     r0, mm_flags, r1
        mvn     r0, #0
        STV     r0, mm_fd, r1
        bl      sys_mmap2
        SYSCALL_FAILED .Lseg_map_failed
        add     r9, r9, #1
        b       .Lmap_seg
.Lmap_seg_done:

/* ---- §4 step 8: copy, protect, synchronize ---- */
        mov     r9, #0
.Lcopy:
        cmp     r9, r8
        bhs     .Lcopy_done
        DESC
        ldr     r2, [r11, #D_INIT_LEN]
        cmp     r2, #0
        beq     .Lcopy_next
        ldr     r0, [r11, #D_VADDR]             /* destination */
        ldr     r1, [r11, #D_PAYLOAD_OFF]
        add     r1, r1, r4                      /* source */
.Lcopy_byte:
        ldrb    r3, [r1], #1
        strb    r3, [r0], #1
        subs    r2, r2, #1
        bne     .Lcopy_byte
.Lcopy_next:
        add     r9, r9, #1
        b       .Lcopy
.Lcopy_done:

        mov     r9, #0
.Lprot:
        cmp     r9, r8
        bhs     .Lprot_done
        DESC
        ldrb    r1, [r11, #D_PERMS]
        mov     r2, #0
        tst     r1, #PERM_R
        orrne   r2, r2, #PROT_READ
        tst     r1, #PERM_W
        orrne   r2, r2, #PROT_WRITE
        tst     r1, #PERM_X
        orrne   r2, r2, #PROT_EXEC
        LDC     r0, seg_start
        ldr     r0, [r0, r9, lsl #2]
        LDC     r1, seg_end
        ldr     r1, [r1, r9, lsl #2]
        sub     r1, r1, r0
        SYSCALL SYS_mprotect
        SYSCALL_FAILED .Lmprotect_failed
        add     r9, r9, #1
        b       .Lprot
.Lprot_done:

/* §12: the ARM instruction cache is not coherent with stores. Every executable
 * range goes through the ARM-private cacheflush syscall, after mprotect and
 * before any running commit, so a failure here is a transport failure with its
 * own class rather than something the guest could be blamed for. */
        mov     r9, #0
.Lcache:
        cmp     r9, r8
        bhs     .Lcache_done
        DESC
        ldrb    r0, [r11, #D_PERMS]
        tst     r0, #PERM_X
        beq     .Lcache_next
        LDC     r0, seg_start
        ldr     r0, [r0, r9, lsl #2]
        LDC     r1, seg_end
        ldr     r1, [r1, r9, lsl #2]
        mov     r2, #0
        SYSCALL SYS_cacheflush
        cmn     r0, #(ERRNO_LIMIT+1)
        bls     .Lcache_next
        FAIL    CLS_CACHE_SYNC, SUB_ARM_CACHEFLUSH
.Lcache_next:
        add     r9, r9, #1
        b       .Lcache
.Lcache_done:

/* ---- §4 step 9 ----
 *
 * ARM is weakly ordered, so the validated fields must be visible before the
 * record_state store that publishes them. §5 puts a release barrier before
 * *every* commit, and both record_state commits are commits. */
        ldr     r0, [r4, #M_CASE_ID]
        str     r0, [r6, #R_CASE_ID]
        ldr     r0, [r4, #M_EXPECTED]
        STV     r0, expected_lo, r2
        str     r0, [r6, #R_EXPECTED]
        ldr     r0, [r4, #M_EXPECTED+4]
        STV     r0, expected_hi, r2
        str     r0, [r6, #R_EXPECTED+4]
        mov     r0, #1
        strh    r0, [r6, #R_N_EXPECTED]
        mov     r0, #0
        strh    r0, [r6, #R_N_VALUES]
        dmb     ish
        mov     r0, #RS_VALIDATED
        str     r0, [r6, #R_RECORD_STATE]

/* ---- §4 steps 10-12 ----
 *
 * SP is 8-byte aligned at the public interface (§11). Nothing is pushed by the
 * call - the return address goes in lr - so the entry observes exactly the top
 * of the validated stack. */
        LDV     r3, entry_addr
        LDC     r0, STACK_NORM
        LDV     r1, stack_size
        add     r0, r0, r1
        STV     r0, guest_sp, r2
        mov     r1, sp
        STV     r1, helper_sp, r2
        mov     r4, r1                          /* §11's convenience copy */
        mov     sp, r0

        dmb     ish
        mov     r0, #ST_RUNNING
        strh    r0, [r6, #R_STATUS]
        blx     r3

        /* Capture first: the observation is r0 sign-extended to 64 bits. */
        mov     r2, r0
        asr      r3, r0, #31

        /* Commit returned before restoring the helper SP, reloading the
           control alias from .bss rather than trusting the guest to have
           preserved r6. */
        LDV     r6, control_alias
        dmb     ish
        mov     r0, #RS_RETURNED
        str     r0, [r6, #R_RECORD_STATE]

        mov     r1, sp                          /* the SP the guest handed back */
        LDV     r0, helper_sp
        mov     sp, r0

/* ---- §7 protocol checks, then §4 step 14 ---- */
        LDV     r0, guest_sp
        cmp     r1, r0
        beq     .Lsp_ok
        mov     r0, #CLS_PROTOCOL
        STV     r0, exit_code, r1
        mov     r0, #SUB_STACK_POINTER_CORRUPT
        STV     r0, fail_subcode, r1
        b       fail_late
.Lsp_ok:
        LDV     r0, helper_sp
        cmp     r4, r0
        beq     .Lcallee_ok
        mov     r0, #CLS_PROTOCOL
        STV     r0, exit_code, r1
        mov     r0, #SUB_CALLEE_SAVED_CLOBBERED
        STV     r0, fail_subcode, r1
        b       fail_late
.Lcallee_ok:
        str     r2, [r6, #R_VALUES]
        str     r3, [r6, #R_VALUES+4]
        mov     r0, #1
        strh    r0, [r6, #R_N_VALUES]
        mov     r0, #0
        str     r0, [r6, #R_DIAG_LEN]
        LDV     r0, expected_lo
        cmp     r2, r0
        bne     .Lfailed
        LDV     r0, expected_hi
        cmp     r3, r0
        bne     .Lfailed
        mov     r0, #ST_PASSED
        b       .Lstatus
.Lfailed:
        mov     r0, #ST_FAILED
.Lstatus:
        dmb     ish
        strh    r0, [r6, #R_STATUS]
        mov     r0, #0
        STV     r0, exit_code, r1
        b       exit_class

/* ---- subroutines ---- */

/* OR of [r0, r0+r1). Returns r0; clobbers r1, r2, r3. */
or_bytes:
        mov     r2, #0
        cmp     r1, #0
        beq     .Lor_done
.Lor_loop:
        ldrb    r3, [r0], #1
        orr     r2, r2, r3
        subs    r1, r1, #1
        bne     .Lor_loop
.Lor_done:
        mov     r0, r2
        bx      lr

/* mmap2 wants six registers, two of which hold validator state, so the
   arguments are marshalled through .bss and r4-r6 are saved here. */
sys_mmap2:
        push    {r4, r5, r6, lr}
        LDC     r12, mm_addr
        ldr     r0, [r12, #0]
        ldr     r1, [r12, #4]
        ldr     r2, [r12, #8]
        ldr     r3, [r12, #12]
        ldr     r4, [r12, #16]
        mov     r5, #0
        LDC     r7, SYS_mmap2
        svc     #0
        pop     {r4, r5, r6, pc}

/* Publish a pre-run error and exit. Staged first, committed last (§5), with
   the release barrier a weakly-ordered profile needs before the commit. */
fail_pre_run:
        LDV     r0, control_alias
        LDV     r1, fail_subcode
        strh    r1, [r0, #R_ERROR_SUBCODE]
        mov     r1, #RS_PRE_RUN_ERROR
        str     r1, [r0, #R_RECORD_STATE]
        dmb     ish
        LDV     r1, exit_code
        strh    r1, [r0, #R_RUNNER_ERROR]
        b       exit_class

/* A failure after the guest returned: record_state stays returned and status
   stays running, so only the subcode is staged. */
fail_late:
        LDV     r0, control_alias
        LDV     r1, fail_subcode
        strh    r1, [r0, #R_ERROR_SUBCODE]
        dmb     ish
        LDV     r1, exit_code
        strh    r1, [r0, #R_RUNNER_ERROR]
        b       exit_class

exit_class:
        LDV     r0, exit_code
        SYSCALL SYS_exit_group
        udf     #0

/* ---- syscall failure landing pads ---- */
.Lresult_open_failed:
        FAIL_NO_RECORD CLS_IO
.Lresult_ftruncate_failed:
        FAIL_NO_RECORD CLS_IO
.Lwindow_failed:
        FAIL_NO_RECORD CLS_MAPPING
.Lcontrol_failed:
        FAIL_NO_RECORD CLS_MAPPING
.Linput_open_failed:
        FAIL    CLS_IO, SUB_INPUT_OPEN
.Linput_fstat_failed:
        FAIL    CLS_IO, SUB_INPUT_FSTAT
.Linput_mmap_failed:
        FAIL    CLS_IO, SUB_INPUT_MMAP
.Lresult_alias_failed:
        FAIL    CLS_MAPPING, SUB_RESULT_ALIAS
.Lstack_map_failed:
        FAIL    CLS_MAPPING, SUB_STACK_MAPPING
.Lguard_map_failed:
        FAIL    CLS_MAPPING, SUB_GUARD_MAPPING
.Lseg_map_failed:
        FAIL    CLS_MAPPING, SUB_SEGMENT_MAPPING
.Lmprotect_failed:
        FAIL    CLS_MAPPING, SUB_MPROTECT

        .size   _start, .-_start
        .section .note.GNU-stack,"",%progbits
