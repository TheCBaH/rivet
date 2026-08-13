/* Execution ABI v1 helper - profile 1, x86-32 SysV.
 *
 * The same document as x86_64.s, in the same order, with two differences that
 * are inherent to a 32-bit profile rather than stylistic.
 *
 * First, the wire is 64 bits wide and the machine is not. Every u64 field is
 * handled as an explicit low/high pair with add/adc and a high-word test, and
 * the places where a value has already been *proved* to fit in 32 bits are
 * called out where that proof is used. Truncating a wire field to 32 bits and
 * comparing the remainder would turn "0x1_0000_0000 bytes" into "0 bytes",
 * which is precisely the class of defect subcode 42 exists to backstop.
 *
 * Second, i386 has eight registers and the syscall convention wants six of
 * them, so validator state lives in .bss rather than in registers and the
 * syscall wrappers take their arguments from there too. That is more verbose
 * than the x86-64 version and considerably harder to get wrong.
 *
 * Invocation:  helper <manifest-path> <result-path> [test-entry-token] */

        .include "abi-v1.inc"

/* Linux i386 syscall numbers, via int $0x80. */
        .equ    SYS_exit_group, 252
        .equ    SYS_open,       5
        .equ    SYS_close,      6
        .equ    SYS_ftruncate,  93
        .equ    SYS_mprotect,   125
        .equ    SYS_mmap2,      192
        .equ    SYS_fstat64,    197

/* struct stat64, i386: st_size is a long long at offset 44. */
        .equ    STAT_ST_SIZE,   44
        .equ    STAT_SIZE,      96

        .equ    WINDOW_BASE,    WINDOW_BASE_32
        .equ    WINDOW_END,     WINDOW_BASE_32 + WINDOW_SIZE
        .equ    RESULT_NORM,    WINDOW_BASE_32 + OFF_RESULT
        .equ    GUARD_NORM,     WINDOW_BASE_32 + OFF_GUARD
        .equ    STACK_NORM,     WINDOW_BASE_32 + OFF_STACK

        .macro  FAIL_NO_RECORD cls
        movl    $\cls, %eax
        movl    %eax, exit_code
        jmp     exit_class
        .endm

        .macro  FAIL cls, sub
        movl    $\cls, %eax
        movl    %eax, exit_code
        movl    $\sub, %eax
        movl    %eax, fail_subcode
        jmp     fail_pre_run
        .endm

/* esi <- the descriptor at index [idx]. Five instructions rather than an
 * addressing mode, because on i386 there is no spare register to keep the
 * table base in across a syscall. */
        .macro  DESC_ESI
        movl    idx, %eax
        imull   $D_SIZE, %eax, %eax
        addl    mbase, %eax
        addl    $M_HEADER_SIZE, %eax
        movl    %eax, %esi
        .endm

/* A syscall return in [-4095, -1] is an errno. */
        .macro  SYSCALL_FAILED label
        cmpl    $-ERRNO_LIMIT, %eax
        jae     \label
        .endm

        .section .rodata
manifest_magic:
        .ascii  "ASMEXE\0"
        .byte   ABI_VERSION
result_magic:
        .ascii  "ASMRES\0"
        .byte   ABI_VERSION
pagesz_wrong_token:
        .ascii  "pgsz-bad"
pagesz_absent_token:
        .ascii  "pgsz-abs"

        .bss
        .align  4
result_fd:      .skip   4
input_fd:       .skip   4
manifest_path:  .skip   4
result_path:    .skip   4
saved_pagesz:   .skip   4
pagesz_found:   .skip   4
control_alias:  .skip   4
exit_code:      .skip   4
fail_subcode:   .skip   4
mbase:          .skip   4       /* the manifest mapping */
msize:          .skip   4       /* st_size, proved <= 1 MiB before use */
nseg:           .skip   4
meta_end:       .skip   4       /* 120 + 40 * n_segments */
cursor:         .skip   4       /* stage 8's running packing cursor */
idx:            .skip   4
jdx:            .skip   4
helper_sp:      .skip   4
guest_sp:       .skip   4
stack_size:     .skip   4
entry_addr:     .skip   4
result_lo:      .skip   4
case_id:        .skip   4
expected_lo:    .skip   4
expected_hi:    .skip   4
retval_lo:      .skip   4
retval_hi:      .skip   4
res_start:      .skip   4       /* page-rounded, saturating */
res_end:        .skip   4
stk_start:      .skip   4
stk_end:        .skip   4
seg_start:      .skip   4 * N_SEGMENTS_MAX
seg_end:        .skip   4 * N_SEGMENTS_MAX
/* mmap2 arguments, because the syscall wants six registers and i386 has
 * eight. The page offset is always zero here, so it is not a field. */
mm_addr:        .skip   4
mm_len:         .skip   4
mm_prot:        .skip   4
mm_flags:       .skip   4
mm_fd:          .skip   4
statbuf:        .skip   STAT_SIZE

        .text
        .globl  _start
        .type   _start, @function

_start:
/* ---- process entry: argv and the auxiliary vector ---- */
        movl    (%esp), %eax                    /* argc */
        cmpl    $3, %eax
        jge     .Largs_ok
        FAIL_NO_RECORD CLS_IO
.Largs_ok:
        movl    8(%esp), %edx
        movl    %edx, manifest_path
        movl    12(%esp), %edx
        movl    %edx, result_path

        leal    4(%esp), %esi                   /* &argv[0] */
        leal    4(%esi,%eax,4), %esi            /* &envp[0] = argv + argc + 1 */
.Lskip_env:
        movl    (%esi), %edx
        addl    $4, %esi
        testl   %edx, %edx
        jnz     .Lskip_env
.Lauxv:
        movl    (%esi), %edx                    /* a_type */
        testl   %edx, %edx
        jz      .Lauxv_done
        cmpl    $AT_PAGESZ, %edx
        jne     .Lauxv_next
        movl    4(%esi), %edx
        movl    %edx, saved_pagesz
        movl    $1, pagesz_found
.Lauxv_next:
        addl    $8, %esi
        jmp     .Lauxv
.Lauxv_done:

/* The §14.2 class 69 test entry: see x86_64.s for why it exists. Two 32-bit
 * compares here rather than one 64-bit one. */
        movl    (%esp), %eax
        cmpl    $4, %eax
        jl      .Lno_test_entry
        movl    16(%esp), %esi                  /* argv[3] */
        movl    (%esi), %eax
        movl    4(%esi), %edx
        cmpl    pagesz_wrong_token, %eax
        jne     .Ltest_absent
        cmpl    pagesz_wrong_token+4, %edx
        jne     .Ltest_absent
        movl    $(2*PAGE_SIZE), %eax
        movl    %eax, saved_pagesz
        jmp     .Lno_test_entry
.Ltest_absent:
        cmpl    pagesz_absent_token, %eax
        jne     .Lno_test_entry
        cmpl    pagesz_absent_token+4, %edx
        jne     .Lno_test_entry
        movl    $0, pagesz_found
.Lno_test_entry:

/* ---- §4 step 1 ---- */
        call    sys_open_result
        SYSCALL_FAILED .Lresult_open_failed
        movl    %eax, result_fd
        call    sys_ftruncate_result
        SYSCALL_FAILED .Lresult_ftruncate_failed

/* ---- §4 step 2: reserve the window ---- */
        movl    $WINDOW_BASE, mm_addr
        movl    $WINDOW_SIZE, mm_len
        movl    $PROT_NONE, mm_prot
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE), mm_flags
        movl    $-1, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lwindow_failed
        cmpl    $WINDOW_BASE, %eax
        jne     .Lwindow_failed

/* ---- §4 step 3: the control alias ---- */
        movl    $0, mm_addr
        movl    $R_PAGE_SIZE, mm_len
        movl    $(PROT_READ|PROT_WRITE), mm_prot
        movl    $MAP_SHARED, mm_flags
        movl    result_fd, %eax
        movl    %eax, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lcontrol_failed
        movl    %eax, control_alias

/* ---- §4 step 4: the bootstrap record ---- */
        movl    %eax, %edi
        movl    result_magic, %eax
        movl    %eax, R_MAGIC(%edi)
        movl    result_magic+4, %eax
        movl    %eax, R_MAGIC+4(%edi)
        movw    $ABI_VERSION, R_ABI_VERSION(%edi)

/* ---- §4 step 4b ---- */
        cmpl    $0, pagesz_found
        jne     .Lpagesz_present
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_ABSENT
.Lpagesz_present:
        cmpl    $PAGE_SIZE, saved_pagesz
        je      .Lpagesz_ok
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_WRONG
.Lpagesz_ok:

/* ---- §4 step 5 / §10 stage 1 ---- */
        call    sys_open_input
        SYSCALL_FAILED .Linput_open_failed
        movl    %eax, input_fd
        call    sys_fstat_input
        SYSCALL_FAILED .Linput_fstat_failed
        /* st_size is a signed 64-bit value; anything with a nonzero high word
           is far past the 1 MiB cap, and treating it as the cap violation is
           the same answer the low word would give if it fit. */
        movl    statbuf+STAT_ST_SIZE+4, %eax
        testl   %eax, %eax
        jnz     .Lsize_too_big
        movl    statbuf+STAT_ST_SIZE, %eax
        cmpl    $MANIFEST_MIN, %eax
        jae     .Lsize_lower_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_BELOW_MINIMUM
.Lsize_lower_ok:
        cmpl    $MANIFEST_MAX, %eax
        jbe     .Lsize_upper_ok
.Lsize_too_big:
        FAIL    CLS_MANIFEST, SUB_SIZE_ABOVE_MAXIMUM
.Lsize_upper_ok:
        movl    %eax, msize

/* ---- §10 stage 2 ---- */
        movl    $0, mm_addr
        movl    %eax, mm_len
        movl    $PROT_READ, mm_prot
        movl    $MAP_PRIVATE, mm_flags
        movl    input_fd, %eax
        movl    %eax, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Linput_mmap_failed
        movl    %eax, mbase

/* ---- §10 stage 3: the fixed header ---- */
        movl    mbase, %ebx
        movl    manifest_magic, %eax
        cmpl    M_MAGIC(%ebx), %eax
        jne     .Lbad_magic
        movl    manifest_magic+4, %eax
        cmpl    M_MAGIC+4(%ebx), %eax
        je      .Lmagic_ok
.Lbad_magic:
        FAIL    CLS_MANIFEST, SUB_BAD_MAGIC
.Lmagic_ok:
        movzwl  M_ABI_VERSION(%ebx), %eax
        cmpl    $ABI_VERSION, %eax
        je      .Lversion_ok
        FAIL    CLS_MANIFEST, SUB_BAD_ABI_VERSION
.Lversion_ok:
        movzwl  M_PROFILE_ID(%ebx), %eax
        cmpl    $PROF_X86_32, %eax
        je      .Lprofile_ok
        FAIL    CLS_MANIFEST, SUB_BAD_PROFILE_ID
.Lprofile_ok:
        movl    M_TOTAL_LEN(%ebx), %eax
        cmpl    msize, %eax
        je      .Ltotal_ok
        FAIL    CLS_MANIFEST, SUB_TOTAL_LEN_MISMATCH
.Ltotal_ok:
        movzwl  M_N_SEGMENTS(%ebx), %eax
        testl   %eax, %eax
        jz      .Lnseg_bad
        cmpl    $N_SEGMENTS_MAX, %eax
        jbe     .Lnseg_ok
.Lnseg_bad:
        FAIL    CLS_MANIFEST, SUB_N_SEGMENTS_RANGE
.Lnseg_ok:
        movl    %eax, nseg
        movzwl  M_N_EXPECTED(%ebx), %eax
        cmpl    $1, %eax
        je      .Lnexp_ok
        FAIL    CLS_MANIFEST, SUB_N_EXPECTED_RANGE
.Lnexp_ok:
        movl    M_RESULT_SIZE(%ebx), %eax
        cmpl    $R_RECORD_SIZE, %eax
        je      .Lressize_ok
        FAIL    CLS_MANIFEST, SUB_RESULT_SIZE_WRONG
.Lressize_ok:
        movl    M_TIMEOUT_MS(%ebx), %eax
        cmpl    $TIMEOUT_MIN, %eax
        jb      .Ltimeout_bad
        cmpl    $TIMEOUT_MAX, %eax
        jbe     .Ltimeout_ok
.Ltimeout_bad:
        FAIL    CLS_MANIFEST, SUB_TIMEOUT_RANGE
.Ltimeout_ok:
        movl    M_RESERVED(%ebx), %eax
        testl   %eax, %eax
        jz      .Lhdr_reserved_ok
        FAIL    CLS_MANIFEST, SUB_HEADER_RESERVED_NONZERO
.Lhdr_reserved_ok:
        leal    M_EXPECTED+8(%ebx), %esi
        movl    $56, %ecx
        call    or_bytes
        testl   %eax, %eax
        jz      .Lexp_tail_ok
        FAIL    CLS_MANIFEST, SUB_EXPECTED_TAIL_NONZERO
.Lexp_tail_ok:

/* ---- §10 stage 4 ---- */
        movl    nseg, %eax
        imull   $D_SIZE, %eax, %eax
        addl    $M_HEADER_SIZE, %eax
        cmpl    msize, %eax
        jbe     .Lmeta_ok
        FAIL    CLS_MANIFEST, SUB_DESCRIPTOR_TABLE_OVERRUNS
.Lmeta_ok:
        movl    %eax, meta_end

/* ---- §10 stage 5 ----
 *
 * The 64-bit sum, in full. Note what this establishes for everything after it:
 * a sum whose high word is zero and which produced no carry can only come from
 * two operands whose high words are both zero, so payload_off and init_len are
 * 32-bit quantities from here on and later stages may treat them as such. */
        movl    $0, idx
.Lstage5:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lstage5_done
        DESC_ESI
        movl    D_PAYLOAD_OFF(%esi), %eax
        movl    D_PAYLOAD_OFF+4(%esi), %edx
        addl    D_INIT_LEN(%esi), %eax
        adcl    D_INIT_LEN+4(%esi), %edx
        jc      .Lpayload_overruns
        testl   %edx, %edx
        jnz     .Lpayload_overruns
        cmpl    msize, %eax
        jbe     .Lstage5_next
.Lpayload_overruns:
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERRUNS
.Lstage5_next:
        incl    idx
        jmp     .Lstage5
.Lstage5_done:

/* ---- §10 stage 6 ---- */
        movl    $0, idx
.Lstage6:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lstage6_done
        DESC_ESI
        movl    D_PAYLOAD_OFF(%esi), %eax
        testb   $7, %al
        jz      .Lstage6_aligned
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_MISALIGNED
.Lstage6_aligned:
        testl   %eax, %eax
        jz      .Lstage6_pairing
        cmpl    meta_end, %eax
        jae     .Lstage6_pairing
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_IN_METADATA
.Lstage6_pairing:
        movl    D_INIT_LEN(%esi), %edx
        testl   %eax, %eax
        setne   %cl
        testl   %edx, %edx
        setne   %ch
        cmpb    %cl, %ch
        je      .Lstage6_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_PAIRING
.Lstage6_next:
        incl    idx
        jmp     .Lstage6
.Lstage6_done:

/* ---- §10 stage 7: pairwise payload non-overlap ---- */
        movl    $0, idx
.Lstage7_i:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lstage7_done
        DESC_ESI
        movl    D_INIT_LEN(%esi), %ecx
        testl   %ecx, %ecx
        jz      .Lstage7_i_next
        movl    D_PAYLOAD_OFF(%esi), %ebx       /* start_i */
        addl    %ebx, %ecx                      /* end_i */
        movl    idx, %eax
        incl    %eax
        movl    %eax, jdx
.Lstage7_j:
        movl    jdx, %eax
        cmpl    nseg, %eax
        jae     .Lstage7_i_next
        imull   $D_SIZE, %eax, %eax
        addl    mbase, %eax
        addl    $M_HEADER_SIZE, %eax
        movl    %eax, %edi
        movl    D_INIT_LEN(%edi), %edx
        testl   %edx, %edx
        jz      .Lstage7_j_next
        movl    D_PAYLOAD_OFF(%edi), %eax       /* start_j */
        addl    %eax, %edx                      /* end_j */
        cmpl    %edx, %ebx                      /* start_i >= end_j ? */
        jae     .Lstage7_j_next
        cmpl    %ecx, %eax                      /* start_j >= end_i ? */
        jae     .Lstage7_j_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERLAP
.Lstage7_j_next:
        incl    jdx
        jmp     .Lstage7_j
.Lstage7_i_next:
        incl    idx
        jmp     .Lstage7_i
.Lstage7_done:

/* ---- §10 stage 8: canonical packing ---- */
        movl    meta_end, %eax
        movl    %eax, cursor
        movl    $0, idx
.Lstage8:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lstage8_done
        DESC_ESI
        movl    D_INIT_LEN(%esi), %edx
        testl   %edx, %edx
        jz      .Lstage8_next
        movl    cursor, %eax
        addl    $7, %eax
        andl    $-8, %eax                       /* expected payload offset */
        cmpl    D_PAYLOAD_OFF(%esi), %eax
        je      .Lstage8_tight
        FAIL    CLS_MANIFEST, SUB_PACKING_NOT_TIGHT
.Lstage8_tight:
        movl    %eax, %ebx                      /* expected offset */
        movl    %ebx, %ecx
        subl    cursor, %ecx                    /* alignment gap */
        jz      .Lstage8_nogap
        movl    cursor, %esi
        addl    mbase, %esi
        call    or_bytes
        testl   %eax, %eax
        jz      .Lstage8_nogap
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_nogap:
        DESC_ESI
        movl    %ebx, %eax
        addl    D_INIT_LEN(%esi), %eax
        movl    %eax, cursor
.Lstage8_next:
        incl    idx
        jmp     .Lstage8
.Lstage8_done:
        /* The length is settled before the padding is read: a file short of
           the aligned end would otherwise fault the helper here rather than
           produce a subcode. */
        movl    cursor, %eax
        addl    $7, %eax
        andl    $-8, %eax
        cmpl    msize, %eax
        je      .Lstage8_len_ok
        FAIL    CLS_MANIFEST, SUB_TRAILING_BYTES
.Lstage8_len_ok:
        movl    %eax, %ecx
        subl    cursor, %ecx
        jz      .Lstage8_pad_ok
        movl    cursor, %esi
        addl    mbase, %esi
        call    or_bytes
        testl   %eax, %eax
        jz      .Lstage8_pad_ok
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_pad_ok:

/* ---- §10 stage 9: resource caps ----
 *
 * init_len is 32-bit by stage 5; zero_len has been touched by nothing so far
 * and still needs its high word rejected. */
        movl    $0, idx
.Lstage9:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lstage9_done
        DESC_ESI
        movl    D_INIT_LEN(%esi), %eax
        cmpl    $INIT_LEN_MAX, %eax
        jbe     .Lstage9_zero
        FAIL    CLS_MANIFEST, SUB_INIT_LEN_TOO_LARGE
.Lstage9_zero:
        movl    D_ZERO_LEN+4(%esi), %eax
        testl   %eax, %eax
        jnz     .Lzero_len_big
        movl    D_ZERO_LEN(%esi), %eax
        cmpl    $ZERO_LEN_MAX, %eax
        jbe     .Lstage9_next
.Lzero_len_big:
        FAIL    CLS_MANIFEST, SUB_ZERO_LEN_TOO_LARGE
.Lstage9_next:
        incl    idx
        jmp     .Lstage9
.Lstage9_done:

/* ---- §10.2 geometry ----
 *
 * After stage 9 both lengths are 32-bit and their sum is at most 1.25 MiB, so
 * the only genuinely 64-bit operand left is vaddr. */
        movl    $0, idx
.Lgeom:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lgeom_done
        DESC_ESI
        movl    D_INIT_LEN(%esi), %eax
        orl     D_ZERO_LEN(%esi), %eax
        jnz     .Lgeom_nonempty
        FAIL    CLS_MANIFEST, SUB_SEGMENT_EMPTY
.Lgeom_nonempty:
        movl    D_ALIGN(%esi), %ecx
        testl   %ecx, %ecx
        jz      .Lgeom_align_bad
        leal    -1(%ecx), %ebx
        testl   %ebx, %ecx
        jz      .Lgeom_align_ok
.Lgeom_align_bad:
        FAIL    CLS_MANIFEST, SUB_ALIGN_NOT_POWER_OF_TWO
.Lgeom_align_ok:
        /* align is a u32, so only vaddr's low word can violate it. */
        movl    D_VADDR(%esi), %eax
        testl   %ebx, %eax
        jz      .Lgeom_vaddr_ok
        FAIL    CLS_MANIFEST, SUB_VADDR_MISALIGNED
.Lgeom_vaddr_ok:
        movzbl  D_PERMS(%esi), %eax
        testl   $~PERM_KNOWN, %eax
        jz      .Lgeom_perms_known
        FAIL    CLS_MANIFEST, SUB_PERMS_UNKNOWN_BITS
.Lgeom_perms_known:
        andl    $(PERM_W|PERM_X), %eax
        cmpl    $(PERM_W|PERM_X), %eax
        jne     .Lgeom_perms_ok
        FAIL    CLS_MANIFEST, SUB_PERMS_WRITE_EXECUTE
.Lgeom_perms_ok:
        movl    D_INIT_LEN(%esi), %ecx
        addl    D_ZERO_LEN(%esi), %ecx          /* <= 1.25 MiB, cannot carry */
        movl    D_VADDR(%esi), %eax
        movl    D_VADDR+4(%esi), %edx
        addl    %ecx, %eax
        adcl    $0, %edx
        jc      .Lgeom_overflow
        addl    $(PAGE_SIZE-1), %eax
        adcl    $0, %edx
        jc      .Lgeom_overflow
        andl    $-PAGE_SIZE, %eax               /* map_end, low word */
        testl   %edx, %edx
        jnz     .Lgeom_outside                  /* map_end >= 2^32 */
        movl    %eax, %ebx                      /* map_end */
        movl    D_VADDR+4(%esi), %edx
        testl   %edx, %edx
        jnz     .Lgeom_outside                  /* vaddr >= 2^32 */
        movl    D_VADDR(%esi), %eax
        andl    $-PAGE_SIZE, %eax               /* map_start */
        cmpl    $WINDOW_BASE, %eax
        jb      .Lgeom_outside
        cmpl    $WINDOW_END, %ebx
        ja      .Lgeom_outside
        movl    idx, %ecx
        movl    %eax, seg_start(,%ecx,4)
        movl    %ebx, seg_end(,%ecx,4)
        incl    idx
        jmp     .Lgeom
.Lgeom_overflow:
        FAIL    CLS_MANIFEST, SUB_ADDRESS_OVERFLOW
.Lgeom_outside:
        FAIL    CLS_MANIFEST, SUB_OUTSIDE_WINDOW
.Lgeom_done:

/* ---- §10.2 collisions ----
 *
 * The result page and the stack are derived from unvalidated manifest fields,
 * so both ranges saturate at 2^32-1 instead of wrapping. A wrapped range would
 * appear to sit at a low address and could collide with a segment that is
 * nowhere near it. */
        movl    mbase, %ebx
        movl    M_RESULT_ADDR+4(%ebx), %edx
        testl   %edx, %edx
        jz      .Lres_in_range
        movl    $-PAGE_SIZE, res_start
        movl    $-1, res_end
        jmp     .Lres_done
.Lres_in_range:
        movl    M_RESULT_ADDR(%ebx), %eax
        movl    %eax, result_lo
        andl    $-PAGE_SIZE, %eax
        movl    %eax, res_start
        addl    $PAGE_SIZE, %eax
        jnc     .Lres_store
        movl    $-1, %eax
.Lres_store:
        movl    %eax, res_end
.Lres_done:
        movl    M_STACK_SIZE(%ebx), %eax
        movl    %eax, stack_size
        movl    $STACK_NORM, stk_start
        addl    $(PAGE_SIZE-1), %eax
        jc      .Lstk_saturate
        andl    $-PAGE_SIZE, %eax
        addl    $STACK_NORM, %eax
        jnc     .Lstk_store
.Lstk_saturate:
        movl    $-1, %eax
.Lstk_store:
        movl    %eax, stk_end

        movl    $0, idx
.Lcoll:
        movl    idx, %ecx
        cmpl    nseg, %ecx
        jae     .Lcoll_done
        movl    seg_start(,%ecx,4), %eax
        movl    seg_end(,%ecx,4), %edx
        cmpl    res_end, %eax
        jae     .Lcoll_stack
        cmpl    res_start, %edx
        jbe     .Lcoll_stack
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_RESULT
.Lcoll_stack:
        cmpl    stk_end, %eax
        jae     .Lcoll_guard
        cmpl    stk_start, %edx
        jbe     .Lcoll_guard
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_STACK
.Lcoll_guard:
        cmpl    $(GUARD_NORM+PAGE_SIZE), %eax
        jae     .Lcoll_next
        cmpl    $GUARD_NORM, %edx
        jbe     .Lcoll_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_GUARD
.Lcoll_next:
        incl    idx
        jmp     .Lcoll
.Lcoll_done:
        movl    res_start, %eax
        cmpl    stk_end, %eax
        jae     .Lcoll_rs_ok
        movl    res_end, %eax
        cmpl    stk_start, %eax
        jbe     .Lcoll_rs_ok
        FAIL    CLS_MANIFEST, SUB_COLLIDE_RESULT_STACK
.Lcoll_rs_ok:
        movl    $0, idx
.Lcoll_ii:
        movl    idx, %ecx
        cmpl    nseg, %ecx
        jae     .Lcoll_ii_done
        movl    seg_start(,%ecx,4), %ebx
        movl    seg_end(,%ecx,4), %esi
        incl    %ecx
        movl    %ecx, jdx
.Lcoll_jj:
        movl    jdx, %ecx
        cmpl    nseg, %ecx
        jae     .Lcoll_ii_next
        movl    seg_start(,%ecx,4), %eax
        movl    seg_end(,%ecx,4), %edx
        cmpl    %edx, %ebx
        jae     .Lcoll_jj_next
        cmpl    %esi, %eax
        jae     .Lcoll_jj_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_SEGMENT
.Lcoll_jj_next:
        incl    jdx
        jmp     .Lcoll_jj
.Lcoll_ii_next:
        incl    idx
        jmp     .Lcoll_ii
.Lcoll_ii_done:

/* ---- §10.2 cross-region well-formedness ----
 *
 * §10.2 fixes the order of the first three groups - stages, then descriptor
 * index, then a fixed pair order - but says nothing about the order *within*
 * well-formedness. "First failure wins" needs a total order, so one is chosen
 * here and recorded: the 32-bit address rule runs first.
 *
 * That is not arbitrary. Every input with a high word set is also caught by a
 * later rule - an entry beyond 2^32 is in no segment, a result address beyond
 * 2^32 is not the normative one - so with subcode 42 last it would be a check
 * that exists and can never fire. Running it first shadows nothing, because
 * every other subcode here has inputs whose high words are zero. */
        movl    mbase, %ebx
        movl    M_ENTRY_ADDR+4(%ebx), %eax
        orl     M_RESULT_ADDR+4(%ebx), %eax
        jz      .Lhigh_bits_ok
        FAIL    CLS_MANIFEST, SUB_ADDRESS_HIGH_BITS_SET
.Lhigh_bits_ok:
        /* Segment vaddrs were already forced below 2^32 by the containment
           rule, so they cannot reach this test; checking them again would be
           the unreachable branch the rule above exists to avoid. */
        movl    M_ENTRY_ADDR(%ebx), %eax
        movl    %eax, entry_addr
        movl    $0, idx
.Lentry:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lentry_bad
        DESC_ESI
        movzbl  D_PERMS(%esi), %eax
        testl   $PERM_X, %eax
        jz      .Lentry_next
        movl    D_VADDR(%esi), %eax
        cmpl    %eax, entry_addr
        jb      .Lentry_next
        addl    D_INIT_LEN(%esi), %eax
        cmpl    %eax, entry_addr
        jb      .Lentry_found
.Lentry_next:
        incl    idx
        jmp     .Lentry
.Lentry_bad:
        FAIL    CLS_MANIFEST, SUB_ENTRY_NOT_IN_X_SEGMENT
.Lentry_found:
        /* §10.3 places no instruction-alignment restriction on x86, so subcode
           37 has no case on this profile. */
        movl    result_lo, %eax
        testl   $(PAGE_SIZE-1), %eax
        jz      .Lresult_aligned
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_MISALIGNED
.Lresult_aligned:
        cmpl    $RESULT_NORM, %eax
        je      .Lresult_normative
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_NOT_NORMATIVE
.Lresult_normative:
        movl    stack_size, %eax
        testl   %eax, %eax
        jz      .Lstack_bad
        testl   $(PAGE_SIZE-1), %eax
        jnz     .Lstack_bad
        cmpl    $STACK_SIZE_MIN, %eax
        jb      .Lstack_bad
        cmpl    $STACK_SIZE_MAX, %eax
        jbe     .Lstack_ok
.Lstack_bad:
        FAIL    CLS_MANIFEST, SUB_STACK_SIZE_INVALID
.Lstack_ok:
        /* Subcode 41: see x86_64.s. Both facts it could still catch are fixed
           at assembly time, so they are asserted there. */
        .if     (GUARD_NORM + PAGE_SIZE) != STACK_NORM
        .error  "guard page is not immediately below the guest stack"
        .endif
        .if     (STACK_NORM + STACK_SIZE_MAX) > (WINDOW_BASE + WINDOW_SIZE)
        .error  "a maximum-size stack does not fit inside the reserved window"
        .endif

/* ---- §4 step 7: install the mappings ---- */
        movl    $RESULT_NORM, mm_addr
        movl    $R_PAGE_SIZE, mm_len
        movl    $(PROT_READ|PROT_WRITE), mm_prot
        movl    $(MAP_SHARED|MAP_FIXED), mm_flags
        movl    result_fd, %eax
        movl    %eax, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lresult_alias_failed

        movl    $STACK_NORM, mm_addr
        movl    stack_size, %eax
        movl    %eax, mm_len
        movl    $(PROT_READ|PROT_WRITE), mm_prot
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), mm_flags
        movl    $-1, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lstack_map_failed

        movl    $GUARD_NORM, mm_addr
        movl    $PAGE_SIZE, mm_len
        movl    $PROT_NONE, mm_prot
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), mm_flags
        movl    $-1, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lguard_map_failed

        movl    $0, idx
.Lmap_seg:
        movl    idx, %ecx
        cmpl    nseg, %ecx
        jae     .Lmap_seg_done
        movl    seg_start(,%ecx,4), %eax
        movl    %eax, mm_addr
        movl    seg_end(,%ecx,4), %edx
        subl    %eax, %edx
        movl    %edx, mm_len
        movl    $(PROT_READ|PROT_WRITE), mm_prot
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), mm_flags
        movl    $-1, mm_fd
        call    sys_mmap2
        SYSCALL_FAILED .Lseg_map_failed
        incl    idx
        jmp     .Lmap_seg
.Lmap_seg_done:

/* ---- §4 step 8: copy payloads, then take away write permission ---- */
        movl    $0, idx
.Lcopy:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lcopy_done
        DESC_ESI
        movl    D_INIT_LEN(%esi), %ecx
        testl   %ecx, %ecx
        jz      .Lcopy_next
        movl    D_VADDR(%esi), %edi
        movl    D_PAYLOAD_OFF(%esi), %eax
        addl    mbase, %eax
        movl    %eax, %esi
        cld
        rep movsb
.Lcopy_next:
        incl    idx
        jmp     .Lcopy
.Lcopy_done:

        movl    $0, idx
.Lprot:
        movl    idx, %eax
        cmpl    nseg, %eax
        jae     .Lprot_done
        DESC_ESI
        movzbl  D_PERMS(%esi), %eax
        xorl    %edx, %edx
        testl   $PERM_R, %eax
        jz      .Lprot_w
        orl     $PROT_READ, %edx
.Lprot_w:
        testl   $PERM_W, %eax
        jz      .Lprot_x
        orl     $PROT_WRITE, %edx
.Lprot_x:
        testl   $PERM_X, %eax
        jz      .Lprot_call
        orl     $PROT_EXEC, %edx
.Lprot_call:
        movl    idx, %ecx
        movl    seg_start(,%ecx,4), %eax
        movl    %eax, mm_addr
        movl    seg_end(,%ecx,4), %ecx
        subl    %eax, %ecx
        movl    %ecx, mm_len
        movl    %edx, mm_prot
        call    sys_mprotect
        SYSCALL_FAILED .Lmprotect_failed
        incl    idx
        jmp     .Lprot
.Lprot_done:

/* §12: x86 needs no instruction-cache maintenance. */

/* ---- §4 step 9 ---- */
        movl    mbase, %ebx
        movl    control_alias, %edi
        movl    M_CASE_ID(%ebx), %eax
        movl    %eax, case_id
        movl    %eax, R_CASE_ID(%edi)
        movl    M_EXPECTED(%ebx), %eax
        movl    %eax, expected_lo
        movl    %eax, R_EXPECTED(%edi)
        movl    M_EXPECTED+4(%ebx), %eax
        movl    %eax, expected_hi
        movl    %eax, R_EXPECTED+4(%edi)
        movw    $1, R_N_EXPECTED(%edi)
        movw    $0, R_N_VALUES(%edi)
        movl    $RS_VALIDATED, R_RECORD_STATE(%edi)

/* ---- §4 steps 10-12 ----
 *
 * ESP is 16-byte aligned at the call, so the entry observes ESP = 12 (mod 16)
 * once the return address is pushed (§11). */
        movl    entry_addr, %edx
        movl    $STACK_NORM, %eax
        addl    stack_size, %eax
        movl    %eax, guest_sp
        movl    %esp, helper_sp
        movl    %esp, %ebx                      /* §11's convenience copy */
        movl    %eax, %esp

        movw    $ST_RUNNING, R_STATUS(%edi)
        cld
        call    *%edx

        /* Capture first: a C int result lives in eax, and the observation is
           that value sign-extended to 64 bits. */
        movl    %eax, retval_lo
        cltd
        movl    %edx, retval_hi

        /* Commit returned before restoring the helper SP, reloading the
           control alias from .bss rather than trusting the guest to have
           preserved edi. */
        movl    control_alias, %edi
        movl    $RS_RETURNED, R_RECORD_STATE(%edi)

        movl    %esp, %ecx                      /* the SP the guest handed back */
        movl    helper_sp, %esp

/* ---- §7 protocol checks, then §4 step 14 ---- */
        cmpl    guest_sp, %ecx
        je      .Lsp_ok
        movl    $CLS_PROTOCOL, exit_code
        movl    $SUB_STACK_POINTER_CORRUPT, fail_subcode
        jmp     fail_late
.Lsp_ok:
        cmpl    helper_sp, %ebx
        je      .Lcallee_ok
        movl    $CLS_PROTOCOL, exit_code
        movl    $SUB_CALLEE_SAVED_CLOBBERED, fail_subcode
        jmp     fail_late
.Lcallee_ok:
        movl    retval_lo, %eax
        movl    %eax, R_VALUES(%edi)
        movl    retval_hi, %edx
        movl    %edx, R_VALUES+4(%edi)
        movw    $1, R_N_VALUES(%edi)
        movl    $0, R_DIAG_LEN(%edi)
        movl    $ST_FAILED, %ecx
        cmpl    expected_lo, %eax
        jne     .Lstatus
        cmpl    expected_hi, %edx
        jne     .Lstatus
        movl    $ST_PASSED, %ecx
.Lstatus:
        movw    %cx, R_STATUS(%edi)
        movl    $0, exit_code
        jmp     exit_class

/* ---- subroutines ---- */

/* OR of [esi, esi+ecx). Returns eax; clobbers esi, ecx, edx. */
or_bytes:
        xorl    %eax, %eax
        testl   %ecx, %ecx
        jz      .Lor_done
.Lor_loop:
        movzbl  (%esi), %edx
        orl     %edx, %eax
        incl    %esi
        decl    %ecx
        jnz     .Lor_loop
.Lor_done:
        ret

sys_open_result:
        pushl   %ebx
        movl    result_path, %ebx
        movl    $(O_RDWR|O_CREAT|O_EXCL), %ecx
        movl    $RESULT_MODE, %edx
        movl    $SYS_open, %eax
        int     $0x80
        popl    %ebx
        ret

sys_ftruncate_result:
        pushl   %ebx
        movl    result_fd, %ebx
        movl    $R_PAGE_SIZE, %ecx
        movl    $SYS_ftruncate, %eax
        int     $0x80
        popl    %ebx
        ret

sys_open_input:
        pushl   %ebx
        movl    manifest_path, %ebx
        movl    $O_RDONLY, %ecx
        xorl    %edx, %edx
        movl    $SYS_open, %eax
        int     $0x80
        popl    %ebx
        ret

sys_fstat_input:
        pushl   %ebx
        movl    input_fd, %ebx
        movl    $statbuf, %ecx
        movl    $SYS_fstat64, %eax
        int     $0x80
        popl    %ebx
        ret

/* mmap2 wants six registers including ebp, which is more than i386 has spare;
   the arguments therefore come from .bss. The page offset is always zero. */
sys_mmap2:
        pushl   %ebx
        pushl   %ebp
        movl    mm_addr, %ebx
        movl    mm_len, %ecx
        movl    mm_prot, %edx
        movl    mm_flags, %esi
        movl    mm_fd, %edi
        xorl    %ebp, %ebp
        movl    $SYS_mmap2, %eax
        int     $0x80
        popl    %ebp
        popl    %ebx
        ret

sys_mprotect:
        pushl   %ebx
        movl    mm_addr, %ebx
        movl    mm_len, %ecx
        movl    mm_prot, %edx
        movl    $SYS_mprotect, %eax
        int     $0x80
        popl    %ebx
        ret

/* Publish a pre-run error and exit. Staged first, committed last (§5). */
fail_pre_run:
        movl    control_alias, %eax
        movl    fail_subcode, %edx
        movw    %dx, R_ERROR_SUBCODE(%eax)
        movl    $RS_PRE_RUN_ERROR, R_RECORD_STATE(%eax)
        movl    exit_code, %edx
        movw    %dx, R_RUNNER_ERROR(%eax)
        jmp     exit_class

/* A failure after the guest returned: record_state stays returned and status
   stays running, so only the subcode is staged. */
fail_late:
        movl    control_alias, %eax
        movl    fail_subcode, %edx
        movw    %dx, R_ERROR_SUBCODE(%eax)
        movl    exit_code, %edx
        movw    %dx, R_RUNNER_ERROR(%eax)
        jmp     exit_class

exit_class:
        movl    exit_code, %ebx
        movl    $SYS_exit_group, %eax
        int     $0x80
        hlt

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
        .section .note.GNU-stack,"",@progbits
