/* Execution ABI v1 helper - profile 2, x86-64 SysV.
 *
 * Implements asm/docs/exec-abi-v1.md §4 step by step. Direct Linux syscalls
 * only: no C, no libc, no dynamic loader. Assembled and linked by the reference
 * GNU as/ld (tools/asm-helpers.sh) and run under qemu-x86_64.
 *
 * Invocation:  helper <manifest-path> <result-path>
 *
 * The structure follows the document rather than what would be convenient in
 * assembly, and in two places that is the entire point:
 *
 *   - the result fd is mapped twice, once at a kernel-selected control address
 *     and once at the normative result_addr, so an error channel exists before
 *     anything can fail validation and no positioned-write path is needed;
 *
 *   - every mapping, protection change and cache operation happens *before* the
 *     running commit, so that a helper fault in those steps and a deliberate
 *     guest trap do not produce the same wait status (§15.1).
 *
 * Register conventions inside the validator (no syscalls run there, so the
 * syscall-clobbered rcx/r11 are free):
 *
 *   r15  manifest mapping base        r14  st_size (trusted, from fstat)
 *   r13  control alias (result page)  r12  n_segments
 *   rbx  loop index                   rcx  current descriptor pointer
 *
 * Anything that must survive the guest call is reloaded from .bss
 * PC-relatively afterwards. §11 keeps a copy of the helper SP in rbx as a
 * convenience, but recovery never depends on the guest having preserved it. */

        .include "abi-v1.inc"

/* Linux x86-64 syscall numbers. */
        .equ    SYS_open,       2
        .equ    SYS_close,      3
        .equ    SYS_fstat,      5
        .equ    SYS_mmap,       9
        .equ    SYS_mprotect,   10
        .equ    SYS_ftruncate,  77
        .equ    SYS_exit_group, 231

/* struct stat, x86-64. */
        .equ    STAT_ST_SIZE,   48
        .equ    STAT_SIZE,      144

/* This profile's slice of the §3 address map. */
        .equ    WINDOW_BASE,    WINDOW_BASE_64
        .equ    WINDOW_END,     WINDOW_BASE_64 + WINDOW_SIZE
        .equ    RESULT_NORM,    WINDOW_BASE_64 + OFF_RESULT
        .equ    GUARD_NORM,     WINDOW_BASE_64 + OFF_GUARD
        .equ    STACK_NORM,     WINDOW_BASE_64 + OFF_STACK

/* A sentinel the guest must hand back in rbx (§11): the saved helper SP. Any
 * other value is a detected callee-saved clobber, class 68 subcode 2. */

/* ---- failure macros ----
 *
 * FAIL_NO_RECORD is for the three failure points that precede the control
 * alias (§7): nothing is writable yet, so the class comes from the exit code
 * alone and the host reports the subcode unavailable.
 *
 * FAIL publishes a canonical pre_run_error: the staged subcode and the
 * record_state change are written first, and runner_error - the field whose
 * value actually changes, 0 -> class - is the commit store (§5). */

        .macro  FAIL_NO_RECORD cls
        movl    $\cls, %edi
        jmp     exit_class
        .endm

        .macro  FAIL cls, sub
        movl    $\cls, %edi
        movl    $\sub, %esi
        jmp     fail_pre_run
        .endm

/* A syscall return in [-4095, -1] is an errno. Testing that range rather than
 * "negative" matters here: mmap legitimately returns addresses with the top bit
 * set, and treating those as failures would be a bug that only appears on a
 * host with a different mmap layout. */
        .macro  SYSCALL_FAILED label
        cmpq    $-ERRNO_LIMIT, %rax
        jae     \label
        .endm

        .section .rodata
manifest_magic:
        .ascii  "ASMEXE\0"
        .byte   ABI_VERSION
result_magic:
        .ascii  "ASMRES\0"
        .byte   ABI_VERSION
/* Eight bytes each, so recognizing one is a single load and compare. */
pagesz_wrong_token:
        .ascii  "pgsz-bad"
pagesz_absent_token:
        .ascii  "pgsz-abs"

        .bss
        .align  8
result_fd:      .skip   8
input_fd:       .skip   8
manifest_path:  .skip   8
result_path:    .skip   8
saved_pagesz:   .skip   8
pagesz_found:   .skip   8
control_alias:  .skip   8       /* the result page, at a kernel-selected address */
meta_end:       .skip   8       /* 120 + 40 * n_segments */
helper_sp:      .skip   8
guest_sp:       .skip   8
case_id:        .skip   8
expected0:      .skip   8
entry_addr:     .skip   8
stack_size:     .skip   8
result_addr:    .skip   8
seg_start:      .skip   8 * N_SEGMENTS_MAX
seg_end:        .skip   8 * N_SEGMENTS_MAX
statbuf:        .skip   STAT_SIZE

        .text
        .globl  _start
        .type   _start, @function

_start:
/* ---- process entry: argv and the auxiliary vector ----
 *
 * The kernel hands us the initial stack unmodified: argc, argv[], NULL,
 * envp[], NULL, then the auxiliary vector. AT_PAGESZ is read here, at entry,
 * because §2 requires the page size to be verified rather than assumed - but it
 * is not *rejected* here, since at this point no record exists to publish the
 * rejection through. That happens at step 4b. */
        movq    (%rsp), %rax            /* argc */
        cmpq    $3, %rax
        jge     .Largs_ok
        /* A malformed invocation cannot produce a record and has no subcode of
           its own; the host sees io with the subcode unavailable, which is the
           same shape as a failed result open. */
        FAIL_NO_RECORD CLS_IO
.Largs_ok:
        movq    16(%rsp), %rdx
        movq    %rdx, manifest_path(%rip)
        movq    24(%rsp), %rdx
        movq    %rdx, result_path(%rip)

        leaq    8(%rsp), %rsi           /* &argv[0] */
        leaq    8(%rsi,%rax,8), %rsi    /* &envp[0] = argv + argc + 1 */
.Lskip_env:
        movq    (%rsi), %rdx
        addq    $8, %rsi
        testq   %rdx, %rdx
        jnz     .Lskip_env
.Lauxv:
        movq    (%rsi), %rdx            /* a_type */
        testq   %rdx, %rdx
        jz      .Lauxv_done
        cmpq    $AT_PAGESZ, %rdx
        jne     .Lauxv_next
        movq    8(%rsi), %rdx
        movq    %rdx, saved_pagesz(%rip)
        movq    $1, pagesz_found(%rip)
.Lauxv_next:
        addq    $16, %rsi
        jmp     .Lauxv
.Lauxv_done:

/* ---- the class 69 test entry ----
 *
 * §14.2: qemu-user cannot be made to report a non-4096 AT_PAGESZ on any of the
 * four provisioned profiles, so the environment subcodes would otherwise be
 * checks that exist and can never be observed. An optional third argument
 * fabricates the saved page size and lets the run continue into the *same*
 * step-4b publication branch, so what the test observes is the complete
 * published artifact - exit_group(69), a canonical pre_run_error record and the
 * exact subcode - rather than a predicate called in isolation.
 *
 * The tokens are exactly eight bytes so the comparison is a single load. This
 * is target-assembly test transport, like the helper itself, so the no-C
 * boundary is untouched. */
        movq    (%rsp), %rax            /* argc */
        cmpq    $4, %rax
        jl      .Lno_test_entry
        movq    32(%rsp), %rsi          /* argv[3] */
        movq    (%rsi), %rax
        cmpq    pagesz_wrong_token(%rip), %rax
        jne     .Ltest_absent
        movq    $(2*PAGE_SIZE), %rax
        movq    %rax, saved_pagesz(%rip)
        jmp     .Lno_test_entry
.Ltest_absent:
        cmpq    pagesz_absent_token(%rip), %rax
        jne     .Lno_test_entry
        movq    $0, pagesz_found(%rip)
.Lno_test_entry:

/* ---- §4 step 1: create the result file ----
 *
 * O_EXCL in the host's private temporary directory is what makes freshness
 * atomic; the host never relies on an earlier "does not exist" check having
 * survived a race. ftruncate zeroes all 4096 bytes, which is also what makes
 * the §13 page padding zero without the helper ever writing there. */
        movq    result_path(%rip), %rdi
        movl    $(O_RDWR|O_CREAT|O_EXCL), %esi
        movl    $RESULT_MODE, %edx
        movl    $SYS_open, %eax
        syscall
        SYSCALL_FAILED .Lresult_open_failed
        movq    %rax, result_fd(%rip)

        movq    %rax, %rdi
        movl    $R_PAGE_SIZE, %esi
        movl    $SYS_ftruncate, %eax
        syscall
        SYSCALL_FAILED .Lresult_ftruncate_failed

/* ---- §4 step 2: reserve the 1 MiB window ----
 *
 * MAP_FIXED_NOREPLACE, not MAP_FIXED: if something already owns this range the
 * reservation must fail loudly rather than silently unmapping it. This precedes
 * the control alias, so its failure is one of the three that can leave no
 * record at all. */
        movl    $WINDOW_BASE, %edi
        movl    $WINDOW_SIZE, %esi
        movl    $PROT_NONE, %edx
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE), %r10d
        movq    $-1, %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lwindow_failed
        cmpq    $WINDOW_BASE, %rax
        jne     .Lwindow_failed

/* ---- §4 step 3: the control alias ----
 *
 * A kernel-selected address, necessarily outside the reservation we just made.
 * This is the error channel, and it exists before anything can fail
 * validation. */
        xorl    %edi, %edi
        movl    $R_PAGE_SIZE, %esi
        movl    $(PROT_READ|PROT_WRITE), %edx
        movl    $MAP_SHARED, %r10d
        movq    result_fd(%rip), %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lcontrol_failed
        movq    %rax, %r13
        movq    %rax, control_alias(%rip)

/* ---- §4 step 4: the bootstrap record ----
 *
 * The page is already zero from ftruncate, so magic and version are the only
 * stores: status = not_started, record_state = bootstrap, everything else zero
 * is exactly what §6 requires of state 0. */
        movq    result_magic(%rip), %rax
        movq    %rax, R_MAGIC(%r13)
        movw    $ABI_VERSION, R_ABI_VERSION(%r13)

/* ---- §4 step 4b: reject a page-size mismatch ----
 *
 * Deliberately here and not earlier. Checking it literally first would put it
 * ahead of the control alias and make its subcode unrecoverable; checking it
 * later would put it after manifest stages whose arithmetic it invalidates. */
        cmpq    $0, pagesz_found(%rip)
        jne     .Lpagesz_present
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_ABSENT
.Lpagesz_present:
        cmpq    $PAGE_SIZE, saved_pagesz(%rip)
        je      .Lpagesz_ok
        FAIL    CLS_ENVIRONMENT, SUB_PAGE_SIZE_WRONG
.Lpagesz_ok:

/* ---- §4 step 5 / §10 stage 1: open and fstat the input, without mapping ----
 *
 * The size bound comes first so that an empty or truncated file yields the
 * promised manifest subcode instead of an incidental mapping error, and so
 * that every later bound can be measured against a *trusted* size rather than
 * against the manifest's own total_len. */
        movq    manifest_path(%rip), %rdi
        movl    $O_RDONLY, %esi
        xorl    %edx, %edx
        movl    $SYS_open, %eax
        syscall
        SYSCALL_FAILED .Linput_open_failed
        movq    %rax, input_fd(%rip)

        movq    %rax, %rdi
        leaq    statbuf(%rip), %rsi
        movl    $SYS_fstat, %eax
        syscall
        SYSCALL_FAILED .Linput_fstat_failed

        movq    statbuf+STAT_ST_SIZE(%rip), %r14
        cmpq    $MANIFEST_MIN, %r14
        jae     .Lsize_lower_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_BELOW_MINIMUM
.Lsize_lower_ok:
        cmpq    $MANIFEST_MAX, %r14
        jbe     .Lsize_upper_ok
        FAIL    CLS_MANIFEST, SUB_SIZE_ABOVE_MAXIMUM
.Lsize_upper_ok:

/* ---- §10 stage 2: map exactly st_size, read-only ---- */
        xorl    %edi, %edi
        movq    %r14, %rsi
        movl    $PROT_READ, %edx
        movl    $MAP_PRIVATE, %r10d
        movq    input_fd(%rip), %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Linput_mmap_failed
        movq    %rax, %r15

/* ---- §10 stage 3: the fixed header, the ABI discriminator ----
 *
 * Before any variable-length data is interpreted. Otherwise a file with bad
 * magic or an unsupported future ABI would be read as a v1 descriptor layout
 * and rejected with a packing or payload subcode rather than a format-identity
 * one. Bounds safety was never at risk - that comes from st_size - but the
 * versioning boundary has to be settled first. */
        movq    manifest_magic(%rip), %rax
        cmpq    M_MAGIC(%r15), %rax
        je      .Lmagic_ok
        FAIL    CLS_MANIFEST, SUB_BAD_MAGIC
.Lmagic_ok:
        movzwl  M_ABI_VERSION(%r15), %eax
        cmpl    $ABI_VERSION, %eax
        je      .Lversion_ok
        FAIL    CLS_MANIFEST, SUB_BAD_ABI_VERSION
.Lversion_ok:
        movzwl  M_PROFILE_ID(%r15), %eax
        cmpl    $PROF_X86_64, %eax
        je      .Lprofile_ok
        FAIL    CLS_MANIFEST, SUB_BAD_PROFILE_ID
.Lprofile_ok:
        movl    M_TOTAL_LEN(%r15), %eax         /* 32-bit load zero-extends */
        cmpq    %r14, %rax
        je      .Ltotal_ok
        FAIL    CLS_MANIFEST, SUB_TOTAL_LEN_MISMATCH
.Ltotal_ok:
        movzwl  M_N_SEGMENTS(%r15), %eax
        testl   %eax, %eax
        jz      .Lnseg_bad
        cmpl    $N_SEGMENTS_MAX, %eax
        jbe     .Lnseg_ok
.Lnseg_bad:
        FAIL    CLS_MANIFEST, SUB_N_SEGMENTS_RANGE
.Lnseg_ok:
        movq    %rax, %r12
        movzwl  M_N_EXPECTED(%r15), %eax
        cmpl    $1, %eax
        je      .Lnexp_ok
        FAIL    CLS_MANIFEST, SUB_N_EXPECTED_RANGE
.Lnexp_ok:
        movl    M_RESULT_SIZE(%r15), %eax
        cmpl    $R_RECORD_SIZE, %eax
        je      .Lressize_ok
        FAIL    CLS_MANIFEST, SUB_RESULT_SIZE_WRONG
.Lressize_ok:
        movl    M_TIMEOUT_MS(%r15), %eax
        cmpl    $TIMEOUT_MIN, %eax
        jb      .Ltimeout_bad
        cmpl    $TIMEOUT_MAX, %eax
        jbe     .Ltimeout_ok
.Ltimeout_bad:
        FAIL    CLS_MANIFEST, SUB_TIMEOUT_RANGE
.Ltimeout_ok:
        movl    M_RESERVED(%r15), %eax
        testl   %eax, %eax
        jz      .Lhdr_reserved_ok
        FAIL    CLS_MANIFEST, SUB_HEADER_RESERVED_NONZERO
.Lhdr_reserved_ok:
        /* expected[] entries beyond n_expected = 1 must be zero. */
        leaq    M_EXPECTED+8(%r15), %rsi
        movl    $56, %ecx
        call    or_bytes
        testq   %rax, %rax
        jz      .Lexp_tail_ok
        FAIL    CLS_MANIFEST, SUB_EXPECTED_TAIL_NONZERO
.Lexp_tail_ok:

/* ---- §10 stage 4: the descriptor table must be in-file ----
 *
 * Checked before any descriptor is read, and against st_size rather than the
 * manifest's own total_len: total_len is attacker-controlled, so a 120-byte
 * file claiming 1 MiB would otherwise pass this and send the helper reading
 * past the end of its mapping. n_segments <= 8, so the arithmetic cannot
 * overflow. */
        imulq   $D_SIZE, %r12, %rax
        addq    $M_HEADER_SIZE, %rax
        cmpq    %r14, %rax
        jbe     .Lmeta_ok
        FAIL    CLS_MANIFEST, SUB_DESCRIPTOR_TABLE_OVERRUNS
.Lmeta_ok:
        movq    %rax, meta_end(%rip)

/* ---- §10 stage 5: every payload must be in-file ----
 *
 * Before any payload byte is read, for the same reason. The carry out of the
 * addition is itself an overrun: a payload_off near 2^64 wraps to a small sum
 * that would otherwise pass. */
        xorl    %ebx, %ebx
.Lstage5:
        cmpq    %r12, %rbx
        jae     .Lstage5_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_PAYLOAD_OFF(%rcx), %rax
        addq    D_INIT_LEN(%rcx), %rax
        jc      .Lpayload_overruns
        cmpq    %r14, %rax
        jbe     .Lstage5_next
.Lpayload_overruns:
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERRUNS
.Lstage5_next:
        incq    %rbx
        jmp     .Lstage5
.Lstage5_done:

/* ---- §10 stage 6: per-descriptor payload semantics ----
 *
 * The metadata test is guarded on payload_off != 0, so that a descriptor
 * claiming init_len > 0 with payload_off = 0 reaches the pairing rule instead
 * of being absorbed by "payload starts inside the metadata region". Both
 * directions of the pairing subcode stay reachable that way. */
        xorl    %ebx, %ebx
.Lstage6:
        cmpq    %r12, %rbx
        jae     .Lstage6_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_PAYLOAD_OFF(%rcx), %rax
        testb   $7, %al
        jz      .Lstage6_aligned
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_MISALIGNED
.Lstage6_aligned:
        testq   %rax, %rax
        jz      .Lstage6_pairing
        cmpq    meta_end(%rip), %rax
        jae     .Lstage6_pairing
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_IN_METADATA
.Lstage6_pairing:
        movq    D_INIT_LEN(%rcx), %rdx
        testq   %rax, %rax
        setnz   %sil
        testq   %rdx, %rdx
        setnz   %dil
        cmpb    %sil, %dil
        je      .Lstage6_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_PAIRING
.Lstage6_next:
        incq    %rbx
        jmp     .Lstage6
.Lstage6_done:

/* ---- §10 stage 7: pairwise payload non-overlap ----
 *
 * Descriptors with no payload are skipped; the pair order is (i, j) with
 * i < j ascending, which is the "fixed pair order" §10.2 requires so that a
 * multi-violation input still names one subcode deterministically. */
        xorl    %ebx, %ebx
.Lstage7_i:
        cmpq    %r12, %rbx
        jae     .Lstage7_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %r9
        testq   %r9, %r9
        jz      .Lstage7_i_next
        movq    D_PAYLOAD_OFF(%rcx), %r8         /* start_i */
        addq    %r8, %r9                         /* end_i */
        leaq    1(%rbx), %r10
.Lstage7_j:
        cmpq    %r12, %r10
        jae     .Lstage7_i_next
        imulq   $D_SIZE, %r10, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %rdx
        testq   %rdx, %rdx
        jz      .Lstage7_j_next
        movq    D_PAYLOAD_OFF(%rcx), %rax        /* start_j */
        addq    %rax, %rdx                       /* end_j */
        cmpq    %rdx, %r8                        /* start_i < end_j ? */
        jae     .Lstage7_j_next
        cmpq    %r9, %rax                        /* start_j < end_i ? */
        jae     .Lstage7_j_next
        FAIL    CLS_MANIFEST, SUB_PAYLOAD_OVERLAP
.Lstage7_j_next:
        incq    %r10
        jmp     .Lstage7_j
.Lstage7_i_next:
        incq    %rbx
        jmp     .Lstage7_i
.Lstage7_done:

/* ---- §10 stage 8: canonical packing ----
 *
 * Deliberately after stages 6 and 7. Checking the exact packing recurrence
 * first would make every misaligned offset, payload-before-metadata, wrong
 * zero/nonzero pairing and payload overlap violate packing before reaching its
 * own rule, rendering all four of those subcodes unreachable.
 *
 * r11 is the running cursor: the byte after the previous payload, or the end of
 * the descriptor table before the first one. */
        movq    meta_end(%rip), %r11
        xorl    %ebx, %ebx
.Lstage8:
        cmpq    %r12, %rbx
        jae     .Lstage8_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %rdx
        testq   %rdx, %rdx
        jz      .Lstage8_next
        movq    %r11, %rax
        addq    $7, %rax
        andq    $-8, %rax                       /* expected payload offset */
        cmpq    D_PAYLOAD_OFF(%rcx), %rax
        je      .Lstage8_tight
        FAIL    CLS_MANIFEST, SUB_PACKING_NOT_TIGHT
.Lstage8_tight:
        /* The alignment gap between the previous payload and this one must be
           zero padding. or_bytes clobbers rax/rcx/rdx/rsi, so the two values
           still needed afterwards move to r8 and r9 first. */
        movq    %rax, %r9                       /* this payload's offset */
        movq    %rdx, %r8                       /* its init_len */
        movq    %r9, %rcx
        subq    %r11, %rcx                      /* gap length */
        jz      .Lstage8_nogap
        leaq    (%r15,%r11,1), %rsi
        call    or_bytes
        testq   %rax, %rax
        jz      .Lstage8_nogap
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_nogap:
        movq    %r9, %r11
        addq    %r8, %r11                       /* cursor = payload_off + init_len */
.Lstage8_next:
        incq    %rbx
        jmp     .Lstage8
.Lstage8_done:
        /* The canonical file end is the 8-byte-aligned end of the final
           payload, or of the descriptor table when nothing has one. Length is
           checked before the padding bytes are read: if the file were short of
           the aligned end, reading them would fault the helper instead of
           producing a subcode. */
        movq    %r11, %rax
        addq    $7, %rax
        andq    $-8, %rax
        cmpq    %r14, %rax
        je      .Lstage8_len_ok
        FAIL    CLS_MANIFEST, SUB_TRAILING_BYTES
.Lstage8_len_ok:
        movq    %rax, %rcx
        subq    %r11, %rcx
        jz      .Lstage8_pad_ok
        leaq    (%r15,%r11,1), %rsi
        call    or_bytes
        testq   %rax, %rax
        jz      .Lstage8_pad_ok
        FAIL    CLS_MANIFEST, SUB_PACKING_GAP_NONZERO
.Lstage8_pad_ok:

/* ---- §10 stage 9: per-descriptor resource caps ----
 *
 * Before address geometry, so that an oversized zero_len - which also violates
 * window containment - deterministically yields the resource subcode rather
 * than the geometry one it would otherwise shadow. */
        xorl    %ebx, %ebx
.Lstage9:
        cmpq    %r12, %rbx
        jae     .Lstage9_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %rax
        cmpq    $INIT_LEN_MAX, %rax
        jbe     .Lstage9_zero
        FAIL    CLS_MANIFEST, SUB_INIT_LEN_TOO_LARGE
.Lstage9_zero:
        movq    D_ZERO_LEN(%rcx), %rax
        cmpq    $ZERO_LEN_MAX, %rax
        jbe     .Lstage9_next
        FAIL    CLS_MANIFEST, SUB_ZERO_LEN_TOO_LARGE
.Lstage9_next:
        incq    %rbx
        jmp     .Lstage9
.Lstage9_done:

/* ---- §10.2 geometry, in descriptor-index order ----
 *
 * The page-rounded range of each segment is recorded as it is computed: the
 * collision phase compares rounded ranges because mprotect is page-granular,
 * and the mapping phase needs the same numbers. Computing them once is also
 * what makes "the range that was validated" and "the range that was mapped"
 * the same range by construction. */
        xorl    %ebx, %ebx
.Lgeom:
        cmpq    %r12, %rbx
        jae     .Lgeom_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %r8
        movq    D_ZERO_LEN(%rcx), %r9
        movq    %r8, %rax
        orq     %r9, %rax
        jnz     .Lgeom_nonempty
        FAIL    CLS_MANIFEST, SUB_SEGMENT_EMPTY
.Lgeom_nonempty:
        movl    D_ALIGN(%rcx), %eax             /* zero-extends */
        testq   %rax, %rax
        jz      .Lgeom_align_bad
        leaq    -1(%rax), %rdx
        testq   %rdx, %rax
        jz      .Lgeom_align_ok
.Lgeom_align_bad:
        FAIL    CLS_MANIFEST, SUB_ALIGN_NOT_POWER_OF_TWO
.Lgeom_align_ok:
        movq    D_VADDR(%rcx), %r10
        testq   %rdx, %r10
        jz      .Lgeom_vaddr_ok
        FAIL    CLS_MANIFEST, SUB_VADDR_MISALIGNED
.Lgeom_vaddr_ok:
        movzbl  D_PERMS(%rcx), %eax
        testl   $~PERM_KNOWN, %eax
        jz      .Lgeom_perms_known
        FAIL    CLS_MANIFEST, SUB_PERMS_UNKNOWN_BITS
.Lgeom_perms_known:
        andl    $(PERM_W|PERM_X), %eax
        cmpl    $(PERM_W|PERM_X), %eax
        jne     .Lgeom_perms_ok
        FAIL    CLS_MANIFEST, SUB_PERMS_WRITE_EXECUTE
.Lgeom_perms_ok:
        /* data_end = vaddr + init_len + zero_len, then rounded up to a page.
           Every step is checked: a wrapped sum would pass containment. */
        movq    %r10, %rax
        addq    %r8, %rax
        jc      .Lgeom_overflow
        addq    %r9, %rax
        jc      .Lgeom_overflow
        addq    $(PAGE_SIZE-1), %rax
        jc      .Lgeom_overflow
        andq    $-PAGE_SIZE, %rax               /* map_end */
        movq    %r10, %rdx
        andq    $-PAGE_SIZE, %rdx               /* map_start */
        cmpq    $WINDOW_BASE, %rdx
        jb      .Lgeom_outside
        cmpq    $WINDOW_END, %rax
        ja      .Lgeom_outside
        leaq    seg_start(%rip), %r8
        movq    %rdx, (%r8,%rbx,8)
        leaq    seg_end(%rip), %r8
        movq    %rax, (%r8,%rbx,8)
        incq    %rbx
        jmp     .Lgeom
.Lgeom_overflow:
        FAIL    CLS_MANIFEST, SUB_ADDRESS_OVERFLOW
.Lgeom_outside:
        FAIL    CLS_MANIFEST, SUB_OUTSIDE_WINDOW
.Lgeom_done:

/* ---- §10.2 collisions ----
 *
 * Before the normative-address rules, deliberately: otherwise a manifest moving
 * the result page onto the stack would always fail "result_addr equals its
 * normative address" first, and the result/stack collision subcode could never
 * be observed.
 *
 * The occupied set includes the guard page. That is not decoration - segments
 * are installed with MAP_FIXED *after* the guard, so a segment admitted over it
 * would silently replace PROT_NONE and void the stack-overflow test.
 *
 * r8/r9 hold the result page range, rdi/rsi the stack range, both page-rounded.
 * The result range saturates rather than wrapping, so an absurd result_addr
 * simply collides with nothing and falls through to the normative check. */
        movq    M_RESULT_ADDR(%r15), %rax
        movq    %rax, result_addr(%rip)
        movq    %rax, %r8
        andq    $-PAGE_SIZE, %r8
        movq    %r8, %r9
        addq    $PAGE_SIZE, %r9
        jnc     .Lcoll_result_ok
        movq    $-1, %r9
.Lcoll_result_ok:
        movl    M_STACK_SIZE(%r15), %eax
        movq    %rax, stack_size(%rip)
        movq    $STACK_NORM, %rdi
        movq    %rdi, %rsi
        addq    %rax, %rsi
        addq    $(PAGE_SIZE-1), %rsi
        andq    $-PAGE_SIZE, %rsi
        xorl    %ebx, %ebx
.Lcoll:
        cmpq    %r12, %rbx
        jae     .Lcoll_done
        leaq    seg_start(%rip), %rcx
        movq    (%rcx,%rbx,8), %r10
        leaq    seg_end(%rip), %rcx
        movq    (%rcx,%rbx,8), %r11
        /* Two ranges are disjoint when either starts at or after the other
           ends; anything else is an overlap. */
        cmpq    %r9, %r10
        jae     .Lcoll_stack
        cmpq    %r8, %r11
        jbe     .Lcoll_stack
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_RESULT
.Lcoll_stack:
        cmpq    %rsi, %r10
        jae     .Lcoll_guard
        cmpq    %rdi, %r11
        jbe     .Lcoll_guard
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_STACK
.Lcoll_guard:
        cmpq    $(GUARD_NORM+PAGE_SIZE), %r10
        jae     .Lcoll_next
        cmpq    $GUARD_NORM, %r11
        jbe     .Lcoll_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_GUARD
.Lcoll_next:
        incq    %rbx
        jmp     .Lcoll
.Lcoll_done:
        /* result page vs guest stack */
        cmpq    %rsi, %r8
        jae     .Lcoll_rs_ok
        cmpq    %r9, %rdi
        jae     .Lcoll_rs_ok
        FAIL    CLS_MANIFEST, SUB_COLLIDE_RESULT_STACK
.Lcoll_rs_ok:
        /* segment vs segment, pairs (i, j) with i < j */
        xorl    %ebx, %ebx
.Lcoll_ii:
        cmpq    %r12, %rbx
        jae     .Lcoll_ii_done
        leaq    seg_start(%rip), %rsi
        movq    (%rsi,%rbx,8), %r10
        leaq    seg_end(%rip), %rsi
        movq    (%rsi,%rbx,8), %r11
        leaq    1(%rbx), %rcx
.Lcoll_jj:
        cmpq    %r12, %rcx
        jae     .Lcoll_ii_next
        leaq    seg_start(%rip), %rsi
        movq    (%rsi,%rcx,8), %rax
        leaq    seg_end(%rip), %rsi
        movq    (%rsi,%rcx,8), %rdx
        cmpq    %rdx, %r10
        jae     .Lcoll_jj_next
        cmpq    %r11, %rax
        jae     .Lcoll_jj_next
        FAIL    CLS_MANIFEST, SUB_COLLIDE_SEGMENT_SEGMENT
.Lcoll_jj_next:
        incq    %rcx
        jmp     .Lcoll_jj
.Lcoll_ii_next:
        incq    %rbx
        jmp     .Lcoll_ii
.Lcoll_ii_done:

/* ---- §10.2 cross-region well-formedness ---- */
        /* entry_addr inside the *initialized* range of an executable segment.
           Initialized, not mapped: entering a zero-fill tail would execute
           zeroes, which on this profile is not even a reliable trap. */
        movq    M_ENTRY_ADDR(%r15), %r9
        movq    %r9, entry_addr(%rip)
        xorl    %ebx, %ebx
.Lentry:
        cmpq    %r12, %rbx
        jae     .Lentry_bad
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movzbl  D_PERMS(%rcx), %eax
        testl   $PERM_X, %eax
        jz      .Lentry_next
        movq    D_VADDR(%rcx), %rax
        cmpq    %rax, %r9
        jb      .Lentry_next
        addq    D_INIT_LEN(%rcx), %rax
        cmpq    %rax, %r9
        jb      .Lentry_found
.Lentry_next:
        incq    %rbx
        jmp     .Lentry
.Lentry_bad:
        FAIL    CLS_MANIFEST, SUB_ENTRY_NOT_IN_X_SEGMENT
.Lentry_found:
        /* §10.3: x86-64 places no instruction-alignment restriction on the
           entry, so subcode 37 has no reachable case on this profile. It is
           reachable on ARM and AArch64, and the conformance suite records the
           per-profile split rather than pretending otherwise. */

        movq    result_addr(%rip), %rax
        testq   $(PAGE_SIZE-1), %rax
        jz      .Lresult_aligned
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_MISALIGNED
.Lresult_aligned:
        cmpq    $RESULT_NORM, %rax
        je      .Lresult_normative
        FAIL    CLS_MANIFEST, SUB_RESULT_ADDR_NOT_NORMATIVE
.Lresult_normative:
        movq    stack_size(%rip), %rax
        testq   %rax, %rax
        jz      .Lstack_bad
        testq   $(PAGE_SIZE-1), %rax
        jnz     .Lstack_bad
        cmpq    $STACK_SIZE_MIN, %rax
        jb      .Lstack_bad
        cmpq    $STACK_SIZE_MAX, %rax
        jbe     .Lstack_ok
.Lstack_bad:
        FAIL    CLS_MANIFEST, SUB_STACK_SIZE_INVALID
.Lstack_ok:
        /* "Stack plus guard exactly matches the normative region" (subcode 41).
           The manifest's only input to the stack region is stack_size, and the
           rule above already constrains it to a nonzero page multiple in
           4 KiB..256 KiB; stack_start and guard_addr are normative constants.
           So the two things this rule could still catch are both decided at
           assembly time, and they are asserted there rather than compiled into
           a runtime branch that can never be taken. Subcode 41 is therefore
           structurally unreachable in ABI v1, and the conformance suite records
           it as such with this proof instead of shipping a case that cannot
           exist. */
        .if     (GUARD_NORM + PAGE_SIZE) != STACK_NORM
        .error  "guard page is not immediately below the guest stack"
        .endif
        .if     (STACK_NORM + STACK_SIZE_MAX) > (WINDOW_BASE + WINDOW_SIZE)
        .error  "a maximum-size stack does not fit inside the reserved window"
        .endif
        /* Subcode 42 - high 32 bits of a u64 address field set - applies to
           32-bit profiles only. On x86-64 every address field is legitimately
           64 bits wide, so there is no case here either. */

/* ---- §4 step 7: install the mappings ----
 *
 * The second result alias first, so the guest-visible result page exists before
 * anything else is placed; then the stack and its guard; then the segments with
 * MAP_FIXED, which is safe now because the overlap arithmetic has already run
 * and nothing we install can be sitting on anything else. */
        movq    $RESULT_NORM, %rdi
        movl    $R_PAGE_SIZE, %esi
        movl    $(PROT_READ|PROT_WRITE), %edx
        movl    $(MAP_SHARED|MAP_FIXED), %r10d
        movq    result_fd(%rip), %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lresult_alias_failed

        movq    $STACK_NORM, %rdi
        movq    stack_size(%rip), %rsi
        movl    $(PROT_READ|PROT_WRITE), %edx
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), %r10d
        movq    $-1, %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lstack_map_failed

        movq    $GUARD_NORM, %rdi
        movl    $PAGE_SIZE, %esi
        movl    $PROT_NONE, %edx
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), %r10d
        movq    $-1, %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lguard_map_failed

        xorl    %ebx, %ebx
.Lmap_seg:
        cmpq    %r12, %rbx
        jae     .Lmap_seg_done
        leaq    seg_start(%rip), %rcx
        movq    (%rcx,%rbx,8), %rdi
        leaq    seg_end(%rip), %rcx
        movq    (%rcx,%rbx,8), %rsi
        subq    %rdi, %rsi
        movl    $(PROT_READ|PROT_WRITE), %edx
        movl    $(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED), %r10d
        movq    $-1, %r8
        xorl    %r9d, %r9d
        movl    $SYS_mmap, %eax
        syscall
        SYSCALL_FAILED .Lseg_map_failed
        incq    %rbx
        jmp     .Lmap_seg
.Lmap_seg_done:

/* ---- §4 step 8: copy payloads, then take away write permission ----
 *
 * Anonymous mappings arrive zeroed, so the zero-fill tail needs no work; only
 * the initialized bytes are copied. Both this and the mprotect happen before
 * any running commit, which is what makes a fault here a transport failure
 * rather than something indistinguishable from a guest trap. */
        xorl    %ebx, %ebx
.Lcopy:
        cmpq    %r12, %rbx
        jae     .Lcopy_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movq    D_INIT_LEN(%rcx), %rdx
        testq   %rdx, %rdx
        jz      .Lcopy_next
        movq    D_VADDR(%rcx), %rdi
        movq    D_PAYLOAD_OFF(%rcx), %rsi
        addq    %r15, %rsi
        movq    %rdx, %rcx
        cld
        rep movsb
.Lcopy_next:
        incq    %rbx
        jmp     .Lcopy
.Lcopy_done:

        xorl    %ebx, %ebx
.Lprot:
        cmpq    %r12, %rbx
        jae     .Lprot_done
        imulq   $D_SIZE, %rbx, %rcx
        leaq    M_HEADER_SIZE(%r15,%rcx,1), %rcx
        movzbl  D_PERMS(%rcx), %eax
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
        leaq    seg_start(%rip), %rcx
        movq    (%rcx,%rbx,8), %rdi
        leaq    seg_end(%rip), %rcx
        movq    (%rcx,%rbx,8), %rsi
        subq    %rdi, %rsi
        movl    $SYS_mprotect, %eax
        syscall
        SYSCALL_FAILED .Lmprotect_failed
        incq    %rbx
        jmp     .Lprot
.Lprot_done:

/* §12: on x86 the instruction cache is coherent with stores, and the mprotect
 * syscall plus the indirect call through a memory operand is sufficient
 * serialization. No cache syscall exists to fail, so class 67 has no reachable
 * case on this profile. */

/* ---- §4 step 9: publish the validated fields ----
 *
 * record_state is the commit store for this transition (§5) because it is the
 * field whose value actually changes; status stays not_started, so re-storing
 * it would tell the host nothing. */
        movl    M_CASE_ID(%r15), %eax
        movq    %rax, case_id(%rip)
        movl    %eax, R_CASE_ID(%r13)
        movq    M_EXPECTED(%r15), %rax
        movq    %rax, expected0(%rip)
        movq    %rax, R_EXPECTED(%r13)
        movw    $1, R_N_EXPECTED(%r13)
        movw    $0, R_N_VALUES(%r13)
        movl    $RS_VALIDATED, R_RECORD_STATE(%r13)

/* ---- §4 steps 10-12: switch stacks, commit running, call ----
 *
 * The order is the contract. The stack switch happens before the running
 * commit and the return capture before the returned commit, which shrinks the
 * window attributed to the guest to the documented straight-line boundary of
 * §4.1: the stores below touch only the mapped result page, the validated guest
 * stack and registers, all of which are already proven mapped.
 *
 * The guest SP is the top of the validated stack, which is page-aligned and so
 * 16-byte aligned; the call pushes the return address, and the entry therefore
 * observes RSP = 8 (mod 16) as §11 requires. */
        movq    entry_addr(%rip), %r11
        movq    $STACK_NORM, %rax
        addq    stack_size(%rip), %rax
        movq    %rax, guest_sp(%rip)
        movq    %rsp, helper_sp(%rip)
        movq    %rsp, %rbx                      /* §11's convenience copy */
        movq    %rax, %rsp

        movw    $ST_RUNNING, R_STATUS(%r13)

        /* §10.3: SysV requires a clear direction flag at a function boundary,
           and CompCert-generated string operations must not inherit incidental
           helper state. */
        cld
        call    *%r11

        /* Capture first, before anything else can disturb eax. The observation
           is the 32-bit return register sign-extended to 64 bits: a C int lives
           in eax, and the upper half of rax is not part of the value, so a
           return of -1 must normalize to 0xffffffffffffffff rather than to a
           zero-extended raw rax. */
        movslq  %eax, %r10

        /* Commit returned before restoring the helper SP, and reload the
           control alias from our own .bss rather than trusting the guest to
           have preserved r13. */
        movq    control_alias(%rip), %r13
        movl    $RS_RETURNED, R_RECORD_STATE(%r13)

        movq    %rsp, %r9                       /* the SP the guest handed back */
        movq    helper_sp(%rip), %rsp

/* ---- §4 step 14, with the §7 protocol checks first ----
 *
 * A detected call-ABI violation stays in returned with status still running and
 * a committed runner_error: the guest did return, so the record must not be
 * rewound, but no result may be published from a run that broke the contract.
 * This is only a guarantee when the helper regains control at all - code that
 * corrupts the return path normalizes as a fault or a timeout instead. */
        cmpq    guest_sp(%rip), %r9
        je      .Lsp_ok
        movl    $CLS_PROTOCOL, %edi
        movl    $SUB_STACK_POINTER_CORRUPT, %esi
        jmp     fail_late
.Lsp_ok:
        cmpq    helper_sp(%rip), %rbx
        je      .Lcallee_ok
        movl    $CLS_PROTOCOL, %edi
        movl    $SUB_CALLEE_SAVED_CLOBBERED, %esi
        jmp     fail_late
.Lcallee_ok:
        movq    %r10, R_VALUES(%r13)
        movw    $1, R_N_VALUES(%r13)
        movl    $0, R_DIAG_LEN(%r13)
        movl    $ST_PASSED, %eax
        cmpq    expected0(%rip), %r10
        je      .Lstatus
        movl    $ST_FAILED, %eax
.Lstatus:
        movw    %ax, R_STATUS(%r13)
        xorl    %edi, %edi
        jmp     exit_class

/* ---- subroutines ---- */

/* OR of [rsi, rsi+rcx). Returns rax; clobbers rsi, rcx, rdx. Used for every
   "must be zero" range: reserved fields, the expected[] tail, and the
   alignment gaps canonical packing requires to be zero padding. */
or_bytes:
        xorl    %eax, %eax
        testq   %rcx, %rcx
        jz      .Lor_done
.Lor_loop:
        movzbl  (%rsi), %edx
        orl     %edx, %eax
        incq    %rsi
        decq    %rcx
        jnz     .Lor_loop
.Lor_done:
        ret

/* Publish a pre-run error through the control alias and exit with the class.
   edi = class, esi = subcode. Staged first, committed last (§5). */
fail_pre_run:
        movq    control_alias(%rip), %rax
        movw    %si, R_ERROR_SUBCODE(%rax)
        movl    $RS_PRE_RUN_ERROR, R_RECORD_STATE(%rax)
        movw    %di, R_RUNNER_ERROR(%rax)
        jmp     exit_class

/* The same, for a failure after the guest returned: record_state stays
   returned and status stays running, so only the subcode is staged. */
fail_late:
        movq    control_alias(%rip), %rax
        movw    %si, R_ERROR_SUBCODE(%rax)
        movw    %di, R_RUNNER_ERROR(%rax)
        jmp     exit_class

/* exit_group with the class in edi. */
exit_class:
        movl    $SYS_exit_group, %eax
        syscall
        hlt

/* ---- syscall failure landing pads ----
 *
 * Split out so the syscall sites stay readable, and so that the two failures
 * which cannot have a record are visibly different from the rest. */
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
