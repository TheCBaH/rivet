/* Execution ABI v1 constants, shared by all four helpers.
 *
 * asm/docs/exec-abi-v1.md is the specification and this is its transcription
 * for the assembler. It exists so that four assembly implementations cannot
 * quietly disagree about an offset or a subcode: guardrail 1 says the helpers
 * conform to one document, and sharing the numbers is the cheapest way to make
 * that structural rather than aspirational.
 *
 * C-style comments and .equ only, so the file is dialect-neutral: the same
 * bytes are .include'd by the x86, ARM and AArch64 sources. Nothing here may
 * name a register, an instruction or a syscall - those differ per profile and
 * live in the profile source.
 *
 * Every value below is quoted from a section of exec-abi-v1.md, and changing
 * one is an ABI v2 bump (§17), not an edit. */

/* ---- §8 manifest header ---- */

        .equ    M_MAGIC,        0       /* 8  "ASMEXE\0\1" */
        .equ    M_ABI_VERSION,  8       /* 2 */
        .equ    M_PROFILE_ID,   10      /* 2 */
        .equ    M_TOTAL_LEN,    12      /* 4 */
        .equ    M_CASE_ID,      16      /* 4 */
        .equ    M_N_SEGMENTS,   20      /* 2 */
        .equ    M_N_EXPECTED,   22      /* 2 */
        .equ    M_ENTRY_ADDR,   24      /* 8 */
        .equ    M_RESULT_ADDR,  32      /* 8 */
        .equ    M_RESULT_SIZE,  40      /* 4 */
        .equ    M_STACK_SIZE,   44      /* 4 */
        .equ    M_TIMEOUT_MS,   48      /* 4 */
        .equ    M_RESERVED,     52      /* 4, zero */
        .equ    M_EXPECTED,     56      /* 8 x 8 */
        .equ    M_HEADER_SIZE,  120

/* ---- §8 segment descriptor ---- */

        .equ    D_VADDR,        0       /* 8 */
        .equ    D_INIT_LEN,     8       /* 8 */
        .equ    D_ZERO_LEN,     16      /* 8 */
        .equ    D_PAYLOAD_OFF,  24      /* 8 */
        .equ    D_ALIGN,        32      /* 4 */
        .equ    D_PERMS,        36      /* 1, bit0 R bit1 W bit2 X */
        .equ    D_RESERVED,     37      /* 3, zero */
        .equ    D_SIZE,         40

        .equ    PERM_R,         1
        .equ    PERM_W,         2
        .equ    PERM_X,         4
        .equ    PERM_KNOWN,     7

/* ---- §13 result block ---- */

        .equ    R_MAGIC,        0       /* 8  "ASMRES\0\1" */
        .equ    R_ABI_VERSION,  8       /* 2 */
        .equ    R_STATUS,       10      /* 2 */
        .equ    R_CASE_ID,      12      /* 4 */
        .equ    R_N_EXPECTED,   16      /* 2 */
        .equ    R_N_VALUES,     18      /* 2 */
        .equ    R_DIAG_LEN,     20      /* 4 */
        .equ    R_EXPECTED,     24      /* 8 x 8 */
        .equ    R_VALUES,       88      /* 8 x 8 */
        .equ    R_DIAG,         152     /* 64 */
        .equ    R_RUNNER_ERROR, 216     /* 2 */
        .equ    R_ERROR_SUBCODE,218     /* 2 */
        .equ    R_RECORD_STATE, 220     /* 4 */
        .equ    R_RESERVED,     224     /* 32, zero */
        .equ    R_RECORD_SIZE,  256
        .equ    R_PAGE_SIZE,    4096

/* §13 status, §6 record state. Both are wire values. */

        .equ    ST_NOT_STARTED, 0
        .equ    ST_RUNNING,     1
        .equ    ST_PASSED,      2
        .equ    ST_FAILED,      3
        .equ    ST_FAULT,       4

        .equ    RS_BOOTSTRAP,   0
        .equ    RS_PRE_RUN_ERROR,1
        .equ    RS_VALIDATED,   2
        .equ    RS_RETURNED,    3

        .equ    ABI_VERSION,    1

/* ---- §1 profile ids ---- */

        .equ    PROF_X86_32,    1
        .equ    PROF_X86_64,    2
        .equ    PROF_ARM,       3
        .equ    PROF_AARCH64,   4

/* ---- §3 address map ----
 *
 * One base per word size, one set of offsets for all four profiles. The guard
 * page is deliberately the page immediately below the stack (0x3F000 + 0x1000
 * = 0x40000), which is what makes a stack overflow hit PROT_NONE. */

        .equ    WINDOW_BASE_32, 0x30000000
        .equ    WINDOW_BASE_64, 0x40000000
        .equ    WINDOW_SIZE,    0x100000
        .equ    OFF_CODE,       0x00000
        .equ    OFF_RODATA,     0x10000
        .equ    OFF_DATA,       0x20000
        .equ    OFF_RESULT,     0x30000
        .equ    OFF_GUARD,      0x3F000
        .equ    OFF_STACK,      0x40000

        .equ    PAGE_SIZE,      4096

/* ---- §10.4 resource limits ---- */

        .equ    MANIFEST_MIN,   120             /* the fixed header alone */
        .equ    MANIFEST_MAX,   0x100000        /* 1 MiB */
        .equ    N_SEGMENTS_MAX, 8
        .equ    TIMEOUT_MIN,    100
        .equ    TIMEOUT_MAX,    60000
        .equ    INIT_LEN_MAX,   0x40000         /* 256 KiB */
        .equ    ZERO_LEN_MAX,   0x100000        /* 1 MiB */
        .equ    STACK_SIZE_MIN, 0x1000          /* 4 KiB */
        .equ    STACK_SIZE_MAX, 0x40000         /* 256 KiB */

/* ---- §14.1 error classes, which are also the exit codes ---- */

        .equ    CLS_NONE,       0
        .equ    CLS_MANIFEST,   64
        .equ    CLS_IO,         65
        .equ    CLS_MAPPING,    66
        .equ    CLS_CACHE_SYNC, 67
        .equ    CLS_PROTOCOL,   68
        .equ    CLS_ENVIRONMENT,69

/* ---- §14.2 subcodes, scoped to their class ----
 *
 * The names match asm/test/oracle/abi.ml, which is how a conformance
 * expectation and the helper that produces it are checked against one table
 * rather than against each other. */

/* class 64 manifest - staged bounds (stages 1-5) */
        .equ    SUB_SIZE_BELOW_MINIMUM,         1
        .equ    SUB_SIZE_ABOVE_MAXIMUM,         2
/* class 64 manifest - fixed header (stage 3, the ABI discriminator) */
        .equ    SUB_BAD_MAGIC,                  3
        .equ    SUB_BAD_ABI_VERSION,            4
        .equ    SUB_BAD_PROFILE_ID,             5
        .equ    SUB_TOTAL_LEN_MISMATCH,         6
        .equ    SUB_N_SEGMENTS_RANGE,           7
        .equ    SUB_N_EXPECTED_RANGE,           8
        .equ    SUB_RESULT_SIZE_WRONG,          9
        .equ    SUB_TIMEOUT_RANGE,              10
        .equ    SUB_HEADER_RESERVED_NONZERO,    11
        .equ    SUB_EXPECTED_TAIL_NONZERO,      12
/* class 64 manifest - descriptor and payload bounds (stages 4-8) */
        .equ    SUB_DESCRIPTOR_TABLE_OVERRUNS,  13
        .equ    SUB_PAYLOAD_OVERRUNS,           14
        .equ    SUB_PAYLOAD_MISALIGNED,         15
        .equ    SUB_PAYLOAD_IN_METADATA,        16
        .equ    SUB_PAYLOAD_PAIRING,            17
        .equ    SUB_PAYLOAD_OVERLAP,            18
        .equ    SUB_PACKING_NOT_TIGHT,          19
        .equ    SUB_PACKING_GAP_NONZERO,        20
        .equ    SUB_TRAILING_BYTES,             21
/* class 64 manifest - resource caps (stage 9) */
        .equ    SUB_INIT_LEN_TOO_LARGE,         22
        .equ    SUB_ZERO_LEN_TOO_LARGE,         23
/* class 64 manifest - geometry */
        .equ    SUB_SEGMENT_EMPTY,              24
        .equ    SUB_ALIGN_NOT_POWER_OF_TWO,     25
        .equ    SUB_VADDR_MISALIGNED,           26
        .equ    SUB_PERMS_UNKNOWN_BITS,         27
        .equ    SUB_PERMS_WRITE_EXECUTE,        28
        .equ    SUB_ADDRESS_OVERFLOW,           29
        .equ    SUB_OUTSIDE_WINDOW,             30
/* class 64 manifest - collisions, in the fixed pair order of §10.2 */
        .equ    SUB_COLLIDE_SEGMENT_RESULT,     31
        .equ    SUB_COLLIDE_SEGMENT_STACK,      32
        .equ    SUB_COLLIDE_SEGMENT_GUARD,      33
        .equ    SUB_COLLIDE_RESULT_STACK,       34
        .equ    SUB_COLLIDE_SEGMENT_SEGMENT,    35
/* class 64 manifest - cross-region well-formedness */
        .equ    SUB_ENTRY_NOT_IN_X_SEGMENT,     36
        .equ    SUB_ENTRY_PROFILE_RULE,         37
        .equ    SUB_RESULT_ADDR_MISALIGNED,     38
        .equ    SUB_RESULT_ADDR_NOT_NORMATIVE,  39
        .equ    SUB_STACK_SIZE_INVALID,         40
        .equ    SUB_STACK_REGION_NOT_NORMATIVE, 41
        .equ    SUB_ADDRESS_HIGH_BITS_SET,      42

/* class 65 io */
        .equ    SUB_RESULT_OPEN,        1
        .equ    SUB_RESULT_FTRUNCATE,   2
        .equ    SUB_INPUT_OPEN,         3
        .equ    SUB_INPUT_FSTAT,        4
        .equ    SUB_INPUT_MMAP,         5

/* class 66 mapping */
        .equ    SUB_WINDOW_RESERVATION, 1
        .equ    SUB_CONTROL_ALIAS,      2
        .equ    SUB_RESULT_ALIAS,       3
        .equ    SUB_STACK_MAPPING,      4
        .equ    SUB_GUARD_MAPPING,      5
        .equ    SUB_SEGMENT_MAPPING,    6
        .equ    SUB_MPROTECT,           7

/* class 67 cache-sync */
        .equ    SUB_ARM_CACHEFLUSH,     1
        .equ    SUB_AARCH64_LINE_SIZE,  2

/* class 68 protocol */
        .equ    SUB_STACK_POINTER_CORRUPT,      1
        .equ    SUB_CALLEE_SAVED_CLOBBERED,     2

/* class 69 environment */
        .equ    SUB_PAGE_SIZE_WRONG,    1
        .equ    SUB_PAGE_SIZE_ABSENT,   2

/* ---- auxiliary vector ---- */

        .equ    AT_NULL,        0
        .equ    AT_PAGESZ,      6

/* ---- mmap/mprotect/open flags (Linux, identical on all four profiles) ---- */

        .equ    PROT_NONE,      0
        .equ    PROT_READ,      1
        .equ    PROT_WRITE,     2
        .equ    PROT_EXEC,      4

        .equ    MAP_SHARED,     0x01
        .equ    MAP_PRIVATE,    0x02
        .equ    MAP_FIXED,      0x10
        .equ    MAP_ANONYMOUS,  0x20
        .equ    MAP_FIXED_NOREPLACE, 0x100000

        .equ    O_RDONLY,       0
        .equ    O_RDWR,         2
        .equ    O_CREAT,        0100    /* octal, as in the kernel headers */
        .equ    O_EXCL,         0200
        .equ    RESULT_MODE,    0600

/* A syscall return in [-4095, -1] is an errno. Every helper tests against this
 * bound rather than "negative", so a legitimately large mmap address is never
 * mistaken for a failure. */
        .equ    ERRNO_LIMIT,    4095
