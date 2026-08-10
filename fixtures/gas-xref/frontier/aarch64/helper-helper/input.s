/* Execution ABI v1 helper - profile 4, AArch64 PCS.
 *
 * The same document and the same order as the other three. What is specific
 * here:
 *
 *   - §12 cache maintenance is explicit rather than a syscall. The executable
 *     ranges are walked with DC CVAU / DSB ISH / IC IVAU / DSB ISH / ISB, with
 *     the data and instruction line sizes read from CTR_EL0. A line size the
 *     register reports as unusable is its own subcode, because walking a range
 *     in steps of zero would hang rather than fail.
 *
 *   - §5's release barriers, as on ARM: this is a weakly-ordered profile, so a
 *     dmb ish precedes every commit store including both record_state commits.
 *
 *   - §10.3's entry rule: 4-byte aligned.
 *
 * 64-bit, so the u64 wire fields are single registers and the arithmetic reads
 * like the x86-64 helper rather than like the two 32-bit ones. There is also no
 * register pressure worth managing, so validator state stays in registers.
 *
 * Invocation:  helper <manifest-path> <result-path> [test-entry-token] */

        .include "abi-v1.inc"

/* Linux generic (asm-generic) syscall numbers; x8 holds the number. There is no
 * open on this ABI - only openat - which is the one call shape that differs
 * from the other three helpers. */
        .equ    SYS_openat,     56
        .equ    SYS_ftruncate,  46
        .equ    SYS_fstat,      80
        .equ    SYS_exit_group, 94
        .equ    SYS_mmap,       222
        .equ    SYS_mprotect,   226
        .equ    AT_FDCWD,       -100

/* struct stat, asm-generic 64-bit: st_size at 48. */
        .equ    STAT_ST_SIZE,   48
        .equ    STAT_SIZE,      128

        .equ    WINDOW_BASE,    WINDOW_BASE_64
        .equ    WINDOW_END,     WINDOW_BASE_64 + WINDOW_SIZE
        .equ    RESULT_NORM,    WINDOW_BASE_64 + OFF_RESULT
        .equ    GUARD_NORM,     WINDOW_BASE_64 + OFF_GUARD
        .equ    STACK_NORM,     WINDOW_BASE_64 + OFF_STACK

/* A symbol's address. adrp/add reaches +-4 GiB, so unlike the ARM helper this
 * needs no movw/movt dance and no literal pools. */
        .macro  LDA reg, sym
        adrp    \reg, \sym
        add     \reg, \reg, #:lo12:\sym
        .endm

        .macro  LDV reg, sym, tmp
        LDA     \tmp, \sym
        ldr     \reg, [\tmp]
        .endm

        .macro  STV reg, sym, tmp
        LDA     \tmp, \sym
        str     \reg, [\tmp]
        .endm

/* A syscall return in [-4095, -1] is an errno. Comparing against -4096 as an
 * unsigned quantity is the idiom that keeps a legitimately high mapping
 * address from being read as a failure. */
        .macro  SYSCALL_FAILED label
        cmn     x0, #(ERRNO_LIMIT+1)
        b.hi    \label
        .endm

        .macro  FAIL_NO_RECORD cls
        mov     w0, #\cls
        STV     x0, exit_code, x1
        b       exit_class
        .endm

        .macro  FAIL cls, sub
        mov     w0, #\cls
        STV     x0, exit_code, x1
        mov     w0, #\sub
        STV     x0, fail_subcode, x1
        b       fail_pre_run
        .endm

/* x11 <- the descriptor at index x9. */
        .macro  RELOAD
        LDV     x4, mbase_save, x12
        LDV     x8, nseg_save, x12
        .endm

        .macro  DESC
        mov     x11, #D_SIZE
        mul     x11, x9, x11
        add     x11, x11, x4
        add     x11, x11, #M_HEADER_SIZE
        .endm

        .section .rodata
        .align  3
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
result_fd:      .skip   8
input_fd:       .skip   8
manifest_path:  .skip   8
result_path:    .skip   8
saved_pagesz:   .skip   8
pagesz_found:   .skip   8
control_alias:  .skip   8
exit_code:      .skip   8
fail_subcode:   .skip   8
cursor:         .skip   8
helper_sp:      .skip   8
guest_sp:       .skip   8
stack_size:     .skip   8
entry_addr:     .skip   8
result_addr:    .skip   8
expected0:      .skip   8
res_start:      .skip   8
res_end:        .skip   8
stk_start:      .skip   8
stk_end:        .skip   8
seg_start:      .skip   8 * N_SEGMENTS_MAX
seg_end:        .skip   8 * N_SEGMENTS_MAX
statbuf:        .skip   STAT_SIZE

        .text
        .globl  _start
        .type   _start, %function

/* Live registers through the validator:
 *   x4  manifest base    x5  st_size      x6  control alias
 *   x8  n_segments       x9  loop index   x10 meta_end
 *   x11 descriptor ptr   x0-x3, x12-x17 scratch
 * x8 is the syscall-number register, so it is reloaded from .bss around each
 * syscall rather than assumed to survive one. */

_start:
/* ---- process entry: argv and the auxiliary vector ---- */
        ldr     x0, [sp]                        /* argc */
        cmp     x0, #3
        b.ge    .Largs_ok
        FAIL_NO_RECORD CLS_IO
.Largs_ok:
        ldr     x1, [sp, #16]
        STV     x1, manifest_path, x2
        ldr     x1, [sp, #24]
        STV     x1, result_path, x2

        add     x1, sp, #8                      /* &argv[0] */
        add     x1, x1, x0, lsl #3
        add     x1, x1, #8                      /* &envp[0] = argv + argc + 1 */
.Lskip_env:
        ldr     x2, [x1], #8
        cbnz    x2, .Lskip_env
.Lauxv:
        ldr     x2, [x1]                        /* a_type */
        cbz     x2, .Lauxv_done
        cmp     x2, #AT_PAGESZ
        b.ne    .Lauxv_next
        ldr     x2, [x1, #8]
        STV     x2, saved_pagesz, x3
        mov     x2, #1
        STV     x2, pagesz_found, x3
.Lauxv_next:
        add     x1, x1, #16
        b       .Lauxv
.Lauxv_done:

/* The §14.2 class 69 test entry: see x86_64.s for why it exists. */
        ldr     x0, [sp]
        cmp     x0, #4
        b.lt    .Lno_test_entry
        ldr     x1, [sp, #32]                   /* argv[3] */
        ldr     x2, [x1]
        LDA     x0, pagesz_wrong_token
        ldr     x3, [x0]
        cmp     x2, x3
        b.ne    .Ltest_absent
        mov     x2, #(2*PAGE_SIZE)
        STV     x2, saved_pagesz, x3
        b       .Lno_test_entry
.Ltest_absent:
        LDA     x0, pagesz_absent_token
        ldr     x3, [x0]
        cmp     x2, x3
        b.ne    .Lno_test_entry
        mov     x2, xzr
        STV     x2, pagesz_found, x3
.Lno_test_entry:

/* ---- §4 step 1 ---- */
        mov     x0, #AT_FDCWD
        LDV     x1, result_path, x2
        mov     x2, #(O_RDWR|O_CREAT|O_EXCL)
        mov     x3, #RESULT_MODE
        mov     x8, #SYS_openat
        svc     #0
        SYSCALL_FAILED .Lresult_open_failed
        STV     x0, result_fd, x1

        mov     x1, #R_PAGE_SIZE
        mov     x8, #SYS_ftruncate
        svc     #0
        SYSCALL_FAILED .Lresult_ftruncate_failed

/* ---- §4 step 2: reserve the window ---- */
        mov     x0, #WINDOW_BASE
        mov     x1, #WINDOW_SIZE
        mov     x2, #PROT_NONE
        /* Not a single movz/movk immediate; MAP_FIXED_NOREPLACE is one bit, so
           it goes in with orr. */
        mov     x3, #(MAP_PRIVATE|MAP_ANONYMOUS)
        orr     x3, x3, #MAP_FIXED_NOREPLACE
        mov     x4, #-1
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        SYSCALL_FAILED .Lwindow_failed
        mov     x1, #WINDOW_BASE
        cmp     x0, x1
        b.ne    .Lwindow_failed

/* ---- §4 step 3: the control alias ---- */
        mov     x0, xzr
        mov     x1, #R_PAGE_SIZE
        mov     x2, #(PROT_READ|PROT_WRITE)
        mov     x3, #MAP_SHARED
        LDV     x4, result_fd, x6
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        SYSCALL_FAILED .Lcontrol_failed
        mov     x6, x0
        STV     x6, control_alias, x1

/* ---- §4 step 4: the bootstrap record ---- */
        LDA     x0, result_magic
        ldr     x1, [x0]
        str     x1, [x6, #R_MAGIC]
        mov     w1, #ABI_VERSION
        strh    w1, [x6, #R_ABI_VERSION]

/* ---- §4 step 4b ---- */
        LDV     x0, pagesz_found, x1
        cbnz    x0, .Lpagesz_present
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_ABSENT
.Lpagesz_present:
        LDV     x0, saved_pagesz, x1
        mov     x1, #PAGE_SIZE
        cmp     x0, x1
        b.eq    .Lpagesz_ok
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_WRONG
.Lpagesz_ok:

/* ---- §4 step 5 / §10 stage 1 ---- */
        mov     x0, #AT_FDCWD
        LDV     x1, manifest_path, x2
        mov     x2, #O_RDONLY
        mov     x3, xzr
        mov     x8, #SYS_openat
        svc     #0
        SYSCALL_FAILED .Linput_open_failed
        STV     x0, input_fd, x1

        LDA     x1, statbuf
        mov     x8, #SYS_fstat
        svc     #0
        SYSCALL_FAILED .Linput_fstat_failed

        LDA     x0, statbuf
        ldr     x5, [x0, #STAT_ST_SIZE]
        cmp     x5, #MANIFEST_MIN
        b.hs    .Lsize_lower_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_BELOW_MINIMUM
.Lsize_lower_ok:
        mov     x1, #MANIFEST_MAX
        cmp     x5, x1
        b.ls    .Lsize_upper_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_ABOVE_MAXIMUM
.Lsize_upper_ok:

/* ---- §10 stage 2 ---- */
        mov     x0, xzr
        mov     x1, x5
        mov     x2, #PROT_READ
        mov     x3, #MAP_PRIVATE
        LDV     x4, input_fd, x12
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        SYSCALL_FAILED .Linput_mmap_failed
        mov     x12, x0
        LDA     x0, statbuf
        ldr     x5, [x0, #STAT_ST_SIZE]         /* x5 was consumed as an argument */
        mov     x4, x12

/* ---- §10 stage 3: the fixed header ---- */
        LDA     x0, manifest_magic
        ldr     x1, [x0]
        ldr     x2, [x4, #M_MAGIC]
        cmp     x1, x2
        b.eq    .Lmagic_ok
        FAIL    CLS_MANIFEST, SUB_BAD_MAGIC
.Lmagic_ok:
        ldrh    w0, [x4, #M_ABI_VERSION]
        cmp     w0, #ABI_VERSION
        b.eq    .Lversion_ok
        FAIL    CLS_MANIFEST, SUB_BAD_ABI_VERSION
.Lversion_ok:
        ldrh    w0, [x4, #M_PROFILE_ID]
        cmp     w0, #PROF_AARCH64
        b.eq    .Lprofile_ok
        FAIL    CLS_MANIFEST, SUB_BAD_PROFILE_ID
.Lprofile_ok:
        ldr     w0, [x4, #M_TOTAL_LEN]          /* 32-bit load zero-extends */
        cmp     x0, x5
        b.eq    .Ltotal_ok
        FAIL    CLS_MANIFEST, SUB_TOTAL_LEN_MISMATCH
.Ltotal_ok:
        ldrh    w8, [x4, #M_N_SEGMENTS]
        cbz     x8, .Lnseg_bad
        cmp     x8, #N_SEGMENTS_MAX
        b.ls    .Lnseg_ok
.Lnseg_bad:
        FAIL    CLS_MANIFEST, SUB_N_SEGMENTS_RANGE
.Lnseg_ok:
        ldrh    w0, [x4, #M_N_EXPECTED]
        cmp     w0, #1
        b.eq    .Lnexp_ok
        FAIL    CLS_MANIFEST, SUB_N_EXPECTED_RANGE
.Lnexp_ok:
        ldr     w0, [x4, #M_RESULT_SIZE]
        cmp     w0, #R_RECORD_SIZE
        b.eq    .Lressize_ok
        FAIL    CLS_MANIFEST, SUB_RESULT_SIZE_WRONG
.Lressize_ok:
        ldr     w0, [x4, #M_TIMEOUT_MS]
        mov     w1, #TIMEOUT_MIN
        cmp     w0, w1
        b.lo    .Ltimeout_bad
        mov     w1, #TIMEOUT_MAX
        cmp     w0, w1
        b.ls    .Ltimeout_ok
.Ltimeout_bad:
        FAIL    CLS_MANIFEST, SUB_TIMEOUT_RANGE
.Ltimeout_ok:
        ldr     w0, [x4, #M_RESERVED]
        cbz     w0, .Lhdr_reserved_ok
        FAIL    CLS_MANIFEST, SUB_HEADER_RESERVED_NONZERO
.Lhdr_reserved_ok:
        add     x0, x4, #M_EXPECTED+8
        mov     x1, #56
        bl      or_bytes
        cbz     x0, .Lexp_tail_ok
        FAIL    CLS_MANIFEST, SUB_EXPECTED_TAIL_NONZERO
.Lexp_tail_ok:

/* ---- §10 stage 4 ---- */
        mov     x0, #D_SIZE
        mul     x10, x8, x0
        add     x10, x10, #M_HEADER_SIZE
        cmp     x10, x5
        b.ls    .Lmeta_ok
        FAIL    CLS_MANIFEST, SUB_DESCRIPTOR_TABLE_OVERRUNS
.Lmeta_ok:

/* ---- §10 stage 5 ---- */
        mov     x9, xzr
.Lstage5:
        cmp     x9, x8
        b.hs    .Lstage5_done
        DESC
        ldr     x0, [x11, #D_PAYLOAD_OFF]
        ldr     x1, [x11, #D_INIT_LEN]
        adds    x0, x0, x1
        b.cs    .Lpayload_overruns
        cmp     x0, x5
        b.ls    .Lstage5_next
.Lpayload_overruns:
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERRUNS
.Lstage5_next:
        add     x9, x9, #1
        b       .Lstage5
.Lstage5_done:

/* ---- §10 stage 6 ---- */
        mov     x9, xzr
.Lstage6:
        cmp     x9, x8
        b.hs    .Lstage6_done
        DESC
        ldr     x0, [x11, #D_PAYLOAD_OFF]
        tst     x0, #7
        b.eq    .Lstage6_aligned
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_MISALIGNED
.Lstage6_aligned:
        cbz     x0, .Lstage6_pairing
        cmp     x0, x10
        b.hs    .Lstage6_pairing
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_IN_METADATA
.Lstage6_pairing:
        ldr     x1, [x11, #D_INIT_LEN]
        cmp     x0, xzr
        cset    w2, ne
        cmp     x1, xzr
        cset    w3, ne
        cmp     w2, w3
        b.eq    .Lstage6_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_PAIRING
.Lstage6_next:
        add     x9, x9, #1
        b       .Lstage6
.Lstage6_done:

/* ---- §10 stage 7: pairwise payload non-overlap ---- */
        mov     x9, xzr
.Lstage7_i:
        cmp     x9, x8
        b.hs    .Lstage7_done
        DESC
        ldr     x1, [x11, #D_INIT_LEN]
        cbz     x1, .Lstage7_i_next
        ldr     x0, [x11, #D_PAYLOAD_OFF]       /* start_i */
        add     x1, x1, x0                      /* end_i */
        add     x12, x9, #1
.Lstage7_j:
        cmp     x12, x8
        b.hs    .Lstage7_i_next
        mov     x13, #D_SIZE
        mul     x13, x12, x13
        add     x13, x13, x4
        add     x13, x13, #M_HEADER_SIZE
        ldr     x3, [x13, #D_INIT_LEN]
        cbz     x3, .Lstage7_j_next
        ldr     x2, [x13, #D_PAYLOAD_OFF]       /* start_j */
        add     x3, x3, x2                      /* end_j */
        cmp     x0, x3
        b.hs    .Lstage7_j_next
        cmp     x2, x1
        b.hs    .Lstage7_j_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERLAP
.Lstage7_j_next:
        add     x12, x12, #1
        b       .Lstage7_j
.Lstage7_i_next:
        add     x9, x9, #1
        b       .Lstage7_i
.Lstage7_done:

/* ---- §10 stage 8: canonical packing ---- */
        STV     x10, cursor, x0
        mov     x9, xzr
.Lstage8:
        cmp     x9, x8
        b.hs    .Lstage8_done
        DESC
        ldr     x1, [x11, #D_INIT_LEN]
        cbz     x1, .Lstage8_next
        LDV     x2, cursor, x0
        add     x0, x2, #7
        and     x0, x0, #~7                     /* expected payload offset */
        ldr     x3, [x11, #D_PAYLOAD_OFF]
        cmp     x0, x3
        b.eq    .Lstage8_tight
        FAIL    CLS_MANIFEST, SUB_PACKING_NOT_TIGHT
.Lstage8_tight:
        subs    x1, x0, x2                      /* alignment gap */
        b.eq    .Lstage8_nogap
        add     x0, x4, x2
        bl      or_bytes
        cbz     x0, .Lstage8_nogap
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_nogap:
        DESC
        ldr     x0, [x11, #D_PAYLOAD_OFF]
        ldr     x1, [x11, #D_INIT_LEN]
        add     x0, x0, x1
        STV     x0, cursor, x2
.Lstage8_next:
        add     x9, x9, #1
        b       .Lstage8
.Lstage8_done:
        /* Length before padding: a file short of the aligned end would fault
           the helper rather than produce a subcode. */
        LDV     x2, cursor, x0
        add     x0, x2, #7
        and     x0, x0, #~7
        cmp     x0, x5
        b.eq    .Lstage8_len_ok
        FAIL    CLS_MANIFEST, SUB_TRAILING_BYTES
.Lstage8_len_ok:
        subs    x1, x0, x2
        b.eq    .Lstage8_pad_ok
        add     x0, x4, x2
        bl      or_bytes
        cbz     x0, .Lstage8_pad_ok
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_pad_ok:

/* ---- §10 stage 9: resource caps ---- */
        mov     x9, xzr
.Lstage9:
        cmp     x9, x8
        b.hs    .Lstage9_done
        DESC
        ldr     x0, [x11, #D_INIT_LEN]
        mov     x1, #INIT_LEN_MAX
        cmp     x0, x1
        b.ls    .Lstage9_zero
        FAIL    CLS_MANIFEST, SUB_INIT_LEN_TOO_LARGE
.Lstage9_zero:
        ldr     x0, [x11, #D_ZERO_LEN]
        mov     x1, #ZERO_LEN_MAX
        cmp     x0, x1
        b.ls    .Lstage9_next
        FAIL    CLS_MANIFEST, SUB_ZERO_LEN_TOO_LARGE
.Lstage9_next:
        add     x9, x9, #1
        b       .Lstage9
.Lstage9_done:

/* ---- §10.2 geometry ---- */
        mov     x9, xzr
.Lgeom:
        cmp     x9, x8
        b.hs    .Lgeom_done
        DESC
        ldr     x0, [x11, #D_INIT_LEN]
        ldr     x1, [x11, #D_ZERO_LEN]
        orr     x2, x0, x1
        cbnz    x2, .Lgeom_nonempty
        FAIL    CLS_MANIFEST, SUB_SEGMENT_EMPTY
.Lgeom_nonempty:
        ldr     w2, [x11, #D_ALIGN]
        cbz     w2, .Lgeom_align_bad
        sub     x3, x2, #1
        tst     x2, x3
        b.eq    .Lgeom_align_ok
.Lgeom_align_bad:
        FAIL    CLS_MANIFEST, SUB_ALIGN_NOT_POWER_OF_TWO
.Lgeom_align_ok:
        ldr     x2, [x11, #D_VADDR]
        tst     x2, x3
        b.eq    .Lgeom_vaddr_ok
        FAIL    CLS_MANIFEST, SUB_VADDR_MISALIGNED
.Lgeom_vaddr_ok:
        ldrb    w2, [x11, #D_PERMS]
        tst     w2, #0xf8
        b.eq    .Lgeom_perms_known
        FAIL    CLS_MANIFEST, SUB_PERMS_UNKNOWN_BITS
.Lgeom_perms_known:
        and     w3, w2, #(PERM_W|PERM_X)
        cmp     w3, #(PERM_W|PERM_X)
        b.ne    .Lgeom_perms_ok
        FAIL    CLS_MANIFEST, SUB_PERMS_WRITE_EXECUTE
.Lgeom_perms_ok:
        ldr     x2, [x11, #D_VADDR]
        adds    x12, x2, x0
        b.cs    .Lgeom_overflow
        adds    x12, x12, x1
        b.cs    .Lgeom_overflow
        add     x13, x12, #(PAGE_SIZE-1)
        cmp     x13, x12
        b.lo    .Lgeom_overflow
        and     x13, x13, #~(PAGE_SIZE-1)       /* map_end */
        and     x14, x2, #~(PAGE_SIZE-1)        /* map_start */
        mov     x15, #WINDOW_BASE
        cmp     x14, x15
        b.lo    .Lgeom_outside
        mov     x15, #WINDOW_END
        cmp     x13, x15
        b.hi    .Lgeom_outside
        LDA     x15, seg_start
        str     x14, [x15, x9, lsl #3]
        LDA     x15, seg_end
        str     x13, [x15, x9, lsl #3]
        add     x9, x9, #1
        b       .Lgeom
.Lgeom_overflow:
        FAIL    CLS_MANIFEST, SUB_ADDRESS_OVERFLOW
.Lgeom_outside:
        FAIL    CLS_MANIFEST, SUB_OUTSIDE_WINDOW
.Lgeom_done:

/* ---- §10.2 collisions ----
 *
 * The result page and the stack come from unvalidated fields, so both ranges
 * saturate rather than wrap. */
        ldr     x0, [x4, #M_RESULT_ADDR]
        STV     x0, result_addr, x1
        and     x0, x0, #~(PAGE_SIZE-1)
        STV     x0, res_start, x1
        adds    x1, x0, #PAGE_SIZE
        b.hs    .Lres_saturate          /* carry set is unsigned overflow */
        b       .Lres_store
.Lres_saturate:
        mov     x1, #-1
.Lres_store:
        STV     x1, res_end, x2

        ldr     w0, [x4, #M_STACK_SIZE]
        STV     x0, stack_size, x1
        mov     x1, #STACK_NORM
        STV     x1, stk_start, x2
        add     x0, x0, #(PAGE_SIZE-1)
        and     x0, x0, #~(PAGE_SIZE-1)
        adds    x0, x0, x1
        b.hs    .Lstk_saturate
        b       .Lstk_store
.Lstk_saturate:
        mov     x0, #-1
.Lstk_store:
        STV     x0, stk_end, x1

        mov     x9, xzr
.Lcoll:
        cmp     x9, x8
        b.hs    .Lcoll_done
        LDA     x12, seg_start
        ldr     x0, [x12, x9, lsl #3]
        LDA     x12, seg_end
        ldr     x1, [x12, x9, lsl #3]
        LDV     x2, res_end, x12
        cmp     x0, x2
        b.hs    .Lcoll_stack
        LDV     x2, res_start, x12
        cmp     x1, x2
        b.ls    .Lcoll_stack
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_RESULT
.Lcoll_stack:
        LDV     x2, stk_end, x12
        cmp     x0, x2
        b.hs    .Lcoll_guard
        LDV     x2, stk_start, x12
        cmp     x1, x2
        b.ls    .Lcoll_guard
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_STACK
.Lcoll_guard:
        mov     x2, #(GUARD_NORM+PAGE_SIZE)
        cmp     x0, x2
        b.hs    .Lcoll_next
        mov     x2, #(GUARD_NORM+PAGE_SIZE)
        sub     x2, x2, #PAGE_SIZE
        cmp     x1, x2
        b.ls    .Lcoll_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_GUARD
.Lcoll_next:
        add     x9, x9, #1
        b       .Lcoll
.Lcoll_done:
        LDV     x0, res_start, x12
        LDV     x2, stk_end, x12
        cmp     x0, x2
        b.hs    .Lcoll_rs_ok
        LDV     x0, res_end, x12
        LDV     x2, stk_start, x12
        cmp     x0, x2
        b.ls    .Lcoll_rs_ok
        FAIL    CLS_MANIFEST, SUB_COLLIDE_RESULT_STACK
.Lcoll_rs_ok:
        mov     x9, xzr
.Lcoll_ii:
        cmp     x9, x8
        b.hs    .Lcoll_ii_done
        LDA     x12, seg_start
        ldr     x0, [x12, x9, lsl #3]
        LDA     x12, seg_end
        ldr     x1, [x12, x9, lsl #3]
        add     x13, x9, #1
.Lcoll_jj:
        cmp     x13, x8
        b.hs    .Lcoll_ii_next
        LDA     x12, seg_start
        ldr     x2, [x12, x13, lsl #3]
        LDA     x12, seg_end
        ldr     x3, [x12, x13, lsl #3]
        cmp     x0, x3
        b.hs    .Lcoll_jj_next
        cmp     x2, x1
        b.hs    .Lcoll_jj_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_SEGMENT
.Lcoll_jj_next:
        add     x13, x13, #1
        b       .Lcoll_jj
.Lcoll_ii_next:
        add     x9, x9, #1
        b       .Lcoll_ii
.Lcoll_ii_done:

/* ---- §10.2 cross-region well-formedness ----
 *
 * Subcode 42 does not apply on a 64-bit profile: every address field is
 * legitimately 64 bits wide, so the order §10.2 states starts at rule 36. */
        ldr     x0, [x4, #M_ENTRY_ADDR]
        STV     x0, entry_addr, x1
        mov     x9, xzr
.Lentry:
        cmp     x9, x8
        b.hs    .Lentry_bad
        DESC
        ldrb    w0, [x11, #D_PERMS]
        tst     w0, #PERM_X
        b.eq    .Lentry_next
        LDV     x3, entry_addr, x12
        ldr     x0, [x11, #D_VADDR]
        cmp     x3, x0
        b.lo    .Lentry_next
        ldr     x1, [x11, #D_INIT_LEN]
        add     x0, x0, x1
        cmp     x3, x0
        b.lo    .Lentry_found
.Lentry_next:
        add     x9, x9, #1
        b       .Lentry
.Lentry_bad:
        FAIL    CLS_MANIFEST, SUB_ENTRY_NOT_IN_X_SEGMENT
.Lentry_found:
        /* §10.3: an AArch64 entry is 4-byte aligned. */
        LDV     x0, entry_addr, x12
        tst     x0, #3
        b.eq    .Lentry_profile_ok
        FAIL    CLS_MANIFEST, SUB_ENTRY_PROFILE_RULE
.Lentry_profile_ok:
        LDV     x0, result_addr, x12
        tst     x0, #(PAGE_SIZE-1)
        b.eq    .Lresult_aligned
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_MISALIGNED
.Lresult_aligned:
        mov     x1, #RESULT_NORM
        cmp     x0, x1
        b.eq    .Lresult_normative
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_NOT_NORMATIVE
.Lresult_normative:
        LDV     x0, stack_size, x12
        cbz     x0, .Lstack_bad
        tst     x0, #(PAGE_SIZE-1)
        b.ne    .Lstack_bad
        mov     x1, #STACK_SIZE_MIN
        cmp     x0, x1
        b.lo    .Lstack_bad
        mov     x1, #STACK_SIZE_MAX
        cmp     x0, x1
        b.ls    .Lstack_ok
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

/* ---- §4 step 7: install the mappings ----
 *
 * Validation is complete, and from here on every syscall clobbers two of the
 * registers the validator was keeping state in: mmap takes its fd in x4, which
 * holds the manifest base, and the syscall number goes in x8, which holds
 * n_segments. Both are parked in .bss and reloaded after every svc rather than
 * shuffled between scratch registers at each site - the mapping, copy, protect
 * and cache loops all need them and all issue syscalls. */
        STV     x4, mbase_save, x12
        STV     x8, nseg_save, x12
        mov     x0, #RESULT_NORM
        mov     x1, #R_PAGE_SIZE
        mov     x2, #(PROT_READ|PROT_WRITE)
        mov     x3, #(MAP_SHARED|MAP_FIXED)
        LDV     x4, result_fd, x12
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        RELOAD
        SYSCALL_FAILED .Lresult_alias_failed

        mov     x0, #STACK_NORM
        LDV     x1, stack_size, x12
        mov     x2, #(PROT_READ|PROT_WRITE)
        mov     x3, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        mov     x4, #-1
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        RELOAD
        SYSCALL_FAILED .Lstack_map_failed

        mov     x0, #(GUARD_NORM+PAGE_SIZE)
        sub     x0, x0, #PAGE_SIZE
        mov     x1, #PAGE_SIZE
        mov     x2, #PROT_NONE
        mov     x3, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        mov     x4, #-1
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        RELOAD
        SYSCALL_FAILED .Lguard_map_failed

        mov     x9, xzr
.Lmap_seg:
        cmp     x9, x8
        b.hs    .Lmap_seg_done
        LDA     x12, seg_start
        ldr     x0, [x12, x9, lsl #3]
        LDA     x12, seg_end
        ldr     x1, [x12, x9, lsl #3]
        sub     x1, x1, x0
        mov     x2, #(PROT_READ|PROT_WRITE)
        mov     x3, #(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED)
        mov     x4, #-1
        mov     x5, xzr
        mov     x8, #SYS_mmap
        svc     #0
        RELOAD
        SYSCALL_FAILED .Lseg_map_failed
        add     x9, x9, #1
        b       .Lmap_seg
.Lmap_seg_done:

/* ---- §4 step 8: copy, protect, synchronize ---- */
        mov     x9, xzr
.Lcopy:
        cmp     x9, x8
        b.hs    .Lcopy_done
        DESC
        ldr     x2, [x11, #D_INIT_LEN]
        cbz     x2, .Lcopy_next
        ldr     x0, [x11, #D_VADDR]
        ldr     x1, [x11, #D_PAYLOAD_OFF]
        add     x1, x1, x4
.Lcopy_byte:
        ldrb    w3, [x1], #1
        strb    w3, [x0], #1
        subs    x2, x2, #1
        b.ne    .Lcopy_byte
.Lcopy_next:
        add     x9, x9, #1
        b       .Lcopy
.Lcopy_done:

        mov     x9, xzr
.Lprot:
        cmp     x9, x8
        b.hs    .Lprot_done
        DESC
        ldrb    w1, [x11, #D_PERMS]
        mov     w2, wzr
        tst     w1, #PERM_R
        b.eq    1f
        orr     w2, w2, #PROT_READ
1:      tst     w1, #PERM_W
        b.eq    2f
        orr     w2, w2, #PROT_WRITE
2:      tst     w1, #PERM_X
        b.eq    3f
        orr     w2, w2, #PROT_EXEC
3:      LDA     x12, seg_start
        ldr     x0, [x12, x9, lsl #3]
        LDA     x12, seg_end
        ldr     x1, [x12, x9, lsl #3]
        sub     x1, x1, x0
        mov     x8, #SYS_mprotect
        svc     #0
        RELOAD
        SYSCALL_FAILED .Lmprotect_failed
        add     x9, x9, #1
        b       .Lprot
.Lprot_done:

/* §12: explicit cache maintenance, because AArch64 has no cacheflush syscall.
 * Clean the data cache to the point of unification over each executable range,
 * then invalidate the instruction cache over it, with a barrier between and an
 * ISB at the end. The line sizes come from CTR_EL0 rather than being assumed -
 * and a line size of zero is rejected rather than used, because walking a range
 * in steps of zero would hang rather than fail. */
        mrs     x14, ctr_el0
        ubfx    x15, x14, #16, #4               /* DminLine: log2 words */
        mov     x16, #4
        lsl     x15, x16, x15                   /* D-cache line size in bytes */
        and     x17, x14, #0xf                  /* IminLine: log2 words */
        lsl     x17, x16, x17                   /* I-cache line size in bytes */
        cbz     x15, .Lline_size_bad
        cbz     x17, .Lline_size_bad
        b       .Lcache
.Lline_size_bad:
        FAIL    CLS_CACHE_SYNC, SUB_AARCH64_LINE_SIZE
.Lcache:
        mov     x9, xzr
.Lcache_seg:
        cmp     x9, x8
        b.hs    .Lcache_done
        DESC
        ldrb    w0, [x11, #D_PERMS]
        tst     w0, #PERM_X
        b.eq    .Lcache_next
        LDA     x12, seg_start
        ldr     x0, [x12, x9, lsl #3]
        LDA     x12, seg_end
        ldr     x1, [x12, x9, lsl #3]
        mov     x2, x0
.Ldc_loop:
        dc      cvau, x2
        add     x2, x2, x15
        cmp     x2, x1
        b.lo    .Ldc_loop
        dsb     ish
        mov     x2, x0
.Lic_loop:
        ic      ivau, x2
        add     x2, x2, x17
        cmp     x2, x1
        b.lo    .Lic_loop
        dsb     ish
        isb
.Lcache_next:
        add     x9, x9, #1
        b       .Lcache_seg
.Lcache_done:

/* ---- §4 step 9 ---- */
        ldr     w0, [x4, #M_CASE_ID]
        str     w0, [x6, #R_CASE_ID]
        ldr     x0, [x4, #M_EXPECTED]
        STV     x0, expected0, x1
        str     x0, [x6, #R_EXPECTED]
        mov     w0, #1
        strh    w0, [x6, #R_N_EXPECTED]
        strh    wzr, [x6, #R_N_VALUES]
        dmb     ish
        mov     w0, #RS_VALIDATED
        str     w0, [x6, #R_RECORD_STATE]

/* ---- §4 steps 10-12 ----
 *
 * SP is 16-byte aligned (§11), and nothing is pushed by the call - the return
 * address goes in x30 - so the entry observes exactly the top of the validated
 * stack. */
        LDV     x3, entry_addr, x12
        mov     x0, #STACK_NORM
        LDV     x1, stack_size, x12
        add     x0, x0, x1
        STV     x0, guest_sp, x12
        mov     x1, sp
        STV     x1, helper_sp, x12
        mov     x19, x1                         /* §11's convenience copy */
        mov     sp, x0

        dmb     ish
        mov     w0, #ST_RUNNING
        strh    w0, [x6, #R_STATUS]
        blr     x3

        /* Capture first: the observation is w0 sign-extended to 64 bits. */
        sxtw    x20, w0

        /* Commit returned before restoring the helper SP, reloading the
           control alias from .bss rather than trusting the guest to have
           preserved x6. */
        LDV     x6, control_alias, x12
        dmb     ish
        mov     w0, #RS_RETURNED
        str     w0, [x6, #R_RECORD_STATE]

        mov     x1, sp                          /* the SP the guest handed back */
        LDV     x0, helper_sp, x12
        mov     sp, x0

/* ---- §7 protocol checks, then §4 step 14 ---- */
        LDV     x0, guest_sp, x12
        cmp     x1, x0
        b.eq    .Lsp_ok
        mov     w0, #CLS_PROTOCOL
        STV     x0, exit_code, x1
        mov     w0, #SUB_STACK_POINTER_CORRUPT
        STV     x0, fail_subcode, x1
        b       fail_late
.Lsp_ok:
        LDV     x0, helper_sp, x12
        cmp     x19, x0
        b.eq    .Lcallee_ok
        mov     w0, #CLS_PROTOCOL
        STV     x0, exit_code, x1
        mov     w0, #SUB_CALLEE_SAVED_CLOBBERED
        STV     x0, fail_subcode, x1
        b       fail_late
.Lcallee_ok:
        str     x20, [x6, #R_VALUES]
        mov     w0, #1
        strh    w0, [x6, #R_N_VALUES]
        str     wzr, [x6, #R_DIAG_LEN]
        LDV     x0, expected0, x12
        cmp     x20, x0
        b.ne    .Lfailed
        mov     w0, #ST_PASSED
        b       .Lstatus
.Lfailed:
        mov     w0, #ST_FAILED
.Lstatus:
        dmb     ish
        strh    w0, [x6, #R_STATUS]
        mov     x0, xzr
        STV     x0, exit_code, x1
        b       exit_class

/* ---- subroutines ---- */

/* OR of [x0, x0+x1). Returns x0; clobbers x1, x2, x3. */
or_bytes:
        mov     x2, xzr
        cbz     x1, .Lor_done
.Lor_loop:
        ldrb    w3, [x0], #1
        orr     x2, x2, x3
        subs    x1, x1, #1
        b.ne    .Lor_loop
.Lor_done:
        mov     x0, x2
        ret

/* Publish a pre-run error and exit. Staged first, committed last (§5), with the
   release barrier a weakly-ordered profile needs before the commit. */
fail_pre_run:
        LDV     x0, control_alias, x1
        LDV     x1, fail_subcode, x2
        strh    w1, [x0, #R_ERROR_SUBCODE]
        mov     w1, #RS_PRE_RUN_ERROR
        str     w1, [x0, #R_RECORD_STATE]
        dmb     ish
        LDV     x1, exit_code, x2
        strh    w1, [x0, #R_RUNNER_ERROR]
        b       exit_class

fail_late:
        LDV     x0, control_alias, x1
        LDV     x1, fail_subcode, x2
        strh    w1, [x0, #R_ERROR_SUBCODE]
        dmb     ish
        LDV     x1, exit_code, x2
        strh    w1, [x0, #R_RUNNER_ERROR]
        b       exit_class

exit_class:
        LDV     x0, exit_code, x1
        mov     x8, #SYS_exit_group
        svc     #0
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

        .bss
        .align  3
mbase_save:     .skip   8
nseg_save:      .skip   8

        .section .note.GNU-stack,"",%progbits
