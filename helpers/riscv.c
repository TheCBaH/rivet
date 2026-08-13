/* Execution ABI v2 helper shared by RV32 and RV64.
 *
 * This is freestanding transport code: no headers, libc, compiler runtime, or
 * dynamic loader.  tools/asm-helpers.sh compiles it with the profile's explicit
 * -march/-mabi flags and links the object directly with GNU ld.  All operating
 * system interaction is through the RISC-V Linux syscall ABI below.
 *
 * Keeping the validator in one source is important here.  The wire format is
 * identical for RV32 and RV64; only pointer width, profile id, and the address
 * window differ.  A duplicated pair would make validation precedence an
 * accidental per-XLEN property.
 */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef unsigned long uptr;
typedef long sptr;

#if __riscv_xlen == 32
# define PROFILE_ID 5
# define WINDOW_BASE ((uptr)0x30000000UL)
#else
# define PROFILE_ID 6
# define WINDOW_BASE ((uptr)0x40000000UL)
#endif

enum {
  PAGE_SIZE = 4096,
  WINDOW_SIZE = 0x100000,
  RESULT_NORM = WINDOW_BASE + 0x30000,
  GUARD_NORM = WINDOW_BASE + 0x3f000,
  STACK_NORM = WINDOW_BASE + 0x40000,
  MANIFEST_MIN = 120,
  MANIFEST_MAX = 0x100000,
  NSEG_MAX = 8,
  INIT_MAX = 0x40000,
  ZERO_MAX = 0x100000,
  STACK_MIN = 0x1000,
  STACK_MAX = 0x40000,
  TIMEOUT_MIN = 100,
  TIMEOUT_MAX = 60000
};

enum { CLS_MANIFEST = 64, CLS_IO, CLS_MAPPING, CLS_CACHE, CLS_PROTOCOL, CLS_ENVIRONMENT };
enum { ST_NOT_STARTED, ST_RUNNING, ST_PASSED, ST_FAILED, ST_FAULT };
enum { RS_BOOTSTRAP, RS_PRE_RUN_ERROR, RS_VALIDATED, RS_RETURNED };
enum { PROT_NONE, PROT_READ = 1, PROT_WRITE = 2, PROT_EXEC = 4 };
enum {
  MAP_SHARED = 1,
  MAP_PRIVATE = 2,
  MAP_FIXED = 0x10,
  MAP_ANONYMOUS = 0x20,
  MAP_FIXED_NOREPLACE = 0x100000
};
enum { PERM_R = 1, PERM_W = 2, PERM_X = 4, PERM_KNOWN = 7 };

enum {
  SYS_ftruncate = 46,
  SYS_openat = 56,
  SYS_statx = 291,
  SYS_exit_group = 94,
  SYS_mmap = 222,
  SYS_mprotect = 226,
  SYS_riscv_flush_icache = 259
};

enum {
  M_MAGIC = 0, M_VERSION = 8, M_PROFILE = 10, M_TOTAL = 12, M_CASE = 16,
  M_NSEG = 20, M_NEXPECTED = 22, M_ENTRY = 24, M_RESULT = 32,
  M_RESULT_SIZE = 40, M_STACK_SIZE = 44, M_TIMEOUT = 48, M_RESERVED = 52,
  M_EXPECTED = 56, M_HEADER_SIZE = 120
};
enum {
  D_VADDR = 0, D_INIT = 8, D_ZERO = 16, D_PAYLOAD = 24, D_ALIGN = 32,
  D_PERMS = 36, D_RESERVED = 37, D_SIZE = 40
};
enum {
  R_MAGIC = 0, R_VERSION = 8, R_STATUS = 10, R_CASE = 12,
  R_NEXPECTED = 16, R_NVALUES = 18, R_DIAG_LEN = 20, R_EXPECTED = 24,
  R_VALUES = 88, R_RUNNER_ERROR = 216, R_SUBCODE = 218, R_STATE = 220
};

static uptr control_alias;
static uptr saved_helper_sp __attribute__((used));
static uptr saved_ra __attribute__((used));
static uptr saved_s[12] __attribute__((used));
static uptr guest_sp_expected __attribute__((used));
static uptr guest_out __attribute__((used));

static inline u16 rd16(const u8 *p) {
  return (u16)p[0] | ((u16)p[1] << 8);
}
static inline u32 rd32(const u8 *p) {
  return (u32)p[0] | ((u32)p[1] << 8) | ((u32)p[2] << 16) | ((u32)p[3] << 24);
}
static inline u64 rd64(const u8 *p) {
  return (u64)rd32(p) | ((u64)rd32(p + 4) << 32);
}
static inline void wr16(u8 *p, u16 x) {
  p[0] = (u8)x; p[1] = (u8)(x >> 8);
}
static inline void wr32(u8 *p, u32 x) {
  p[0] = (u8)x; p[1] = (u8)(x >> 8); p[2] = (u8)(x >> 16); p[3] = (u8)(x >> 24);
}
static inline void wr64(u8 *p, u64 x) {
  wr32(p, (u32)x); wr32(p + 4, (u32)(x >> 32));
}
static int bytes_nonzero(const u8 *p, u32 n) {
  u8 x = 0;
  while (n--) x |= *p++;
  return x != 0;
}
static void copy_bytes(u8 *d, const u8 *s, u32 n) {
  while (n--) *d++ = *s++;
}
static int bytes_equal(const u8 *a, const u8 *b, u32 n) {
  while (n--) if (*a++ != *b++) return 0;
  return 1;
}

static inline sptr syscall6(uptr n, uptr a0, uptr a1, uptr a2, uptr a3, uptr a4, uptr a5) {
  register uptr x10 __asm__("a0") = a0;
  register uptr x11 __asm__("a1") = a1;
  register uptr x12 __asm__("a2") = a2;
  register uptr x13 __asm__("a3") = a3;
  register uptr x14 __asm__("a4") = a4;
  register uptr x15 __asm__("a5") = a5;
  register uptr x17 __asm__("a7") = n;
  __asm__ volatile("ecall" : "+r"(x10) : "r"(x11), "r"(x12), "r"(x13), "r"(x14), "r"(x15), "r"(x17) : "memory");
  return (sptr)x10;
}
static inline sptr syscall3(uptr n, uptr a0, uptr a1, uptr a2) {
  return syscall6(n, a0, a1, a2, 0, 0, 0);
}
static int failed(sptr r) { return (uptr)r >= (uptr)-4095; }
static __attribute__((noreturn)) void exit_class(u32 c) {
  syscall3(SYS_exit_group, c, 0, 0);
  for (;;) __asm__ volatile("ebreak");
}
static __attribute__((noreturn)) void fail_no_record(u32 c) { exit_class(c); }
static __attribute__((noreturn)) void fail_pre(u32 c, u32 sub) {
  u8 *r = (u8 *)control_alias;
  wr16(r + R_SUBCODE, (u16)sub);
  wr32(r + R_STATE, RS_PRE_RUN_ERROR);
  __asm__ volatile("fence rw,w" ::: "memory");
  wr16(r + R_RUNNER_ERROR, (u16)c);
  exit_class(c);
}
static __attribute__((noreturn)) void fail_late(u32 c, u32 sub) {
  u8 *r = (u8 *)control_alias;
  wr16(r + R_SUBCODE, (u16)sub);
  __asm__ volatile("fence rw,w" ::: "memory");
  wr16(r + R_RUNNER_ERROR, (u16)c);
  exit_class(c);
}

/* This boundary owns the stack switch and all twelve integer callee-saved
 * checks.  Original helper values are restored before returning to C, so the
 * compiler's calling convention remains true even for the negative control. */
__asm__(
".text\n"
".align 2\n"
".globl riscv_run_guest\n"
".type riscv_run_guest,@function\n"
"riscv_run_guest:\n"
#if __riscv_xlen == 32
"  la t0,guest_out\n  sw a3,0(t0)\n"
"  la t0,saved_helper_sp\n  sw sp,0(t0)\n"
"  la t0,saved_ra\n  sw ra,0(t0)\n"
"  la t0,saved_s\n"
"  sw s0,0(t0)\n  sw s1,4(t0)\n  sw s2,8(t0)\n  sw s3,12(t0)\n"
"  sw s4,16(t0)\n  sw s5,20(t0)\n  sw s6,24(t0)\n  sw s7,28(t0)\n"
"  sw s8,32(t0)\n  sw s9,36(t0)\n  sw s10,40(t0)\n  sw s11,44(t0)\n"
"  la t0,guest_sp_expected\n  sw a1,0(t0)\n"
#else
"  la t0,guest_out\n  sd a3,0(t0)\n"
"  la t0,saved_helper_sp\n  sd sp,0(t0)\n"
"  la t0,saved_ra\n  sd ra,0(t0)\n"
"  la t0,saved_s\n"
"  sd s0,0(t0)\n  sd s1,8(t0)\n  sd s2,16(t0)\n  sd s3,24(t0)\n"
"  sd s4,32(t0)\n  sd s5,40(t0)\n  sd s6,48(t0)\n  sd s7,56(t0)\n"
"  sd s8,64(t0)\n  sd s9,72(t0)\n  sd s10,80(t0)\n  sd s11,88(t0)\n"
"  la t0,guest_sp_expected\n  sd a1,0(t0)\n"
#endif
"  li s0,0x181\n  li s1,0x192\n  li s2,0x1a3\n  li s3,0x1b4\n"
"  li s4,0x1c5\n  li s5,0x1d6\n  li s6,0x1e7\n  li s7,0x1f8\n"
"  li s8,0x209\n  li s9,0x21a\n  li s10,0x22b\n  li s11,0x23c\n"
"  mv t3,a0\n  mv sp,a1\n"
"  li t0,1\n  sh t0,10(a2)\n  fence rw,w\n"
"  jalr ra,t3,0\n"
"  mv t3,a0\n  mv t4,sp\n"
"  la t0,control_alias\n"
#if __riscv_xlen == 32
"  lw t1,0(t0)\n"
#else
"  ld t1,0(t0)\n"
#endif
"  li t0,3\n  sw t0,220(t1)\n  fence rw,w\n"
"  li t5,0\n"
"  li t0,0x181\n  bne s0,t0,1f\n  li t0,0x192\n  bne s1,t0,1f\n"
"  li t0,0x1a3\n  bne s2,t0,1f\n  li t0,0x1b4\n  bne s3,t0,1f\n"
"  li t0,0x1c5\n  bne s4,t0,1f\n  li t0,0x1d6\n  bne s5,t0,1f\n"
"  li t0,0x1e7\n  bne s6,t0,1f\n  li t0,0x1f8\n  bne s7,t0,1f\n"
"  li t0,0x209\n  bne s8,t0,1f\n  li t0,0x21a\n  bne s9,t0,1f\n"
"  li t0,0x22b\n  bne s10,t0,1f\n  li t0,0x23c\n  beq s11,t0,2f\n"
"1: li t5,1\n"
"2: la t0,saved_helper_sp\n"
#if __riscv_xlen == 32
"  lw sp,0(t0)\n  la t0,saved_s\n"
"  lw s0,0(t0)\n  lw s1,4(t0)\n  lw s2,8(t0)\n  lw s3,12(t0)\n"
"  lw s4,16(t0)\n  lw s5,20(t0)\n  lw s6,24(t0)\n  lw s7,28(t0)\n"
"  lw s8,32(t0)\n  lw s9,36(t0)\n  lw s10,40(t0)\n  lw s11,44(t0)\n"
"  la t0,saved_ra\n  lw ra,0(t0)\n"
#else
"  ld sp,0(t0)\n  la t0,saved_s\n"
"  ld s0,0(t0)\n  ld s1,8(t0)\n  ld s2,16(t0)\n  ld s3,24(t0)\n"
"  ld s4,32(t0)\n  ld s5,40(t0)\n  ld s6,48(t0)\n  ld s7,56(t0)\n"
"  ld s8,64(t0)\n  ld s9,72(t0)\n  ld s10,80(t0)\n  ld s11,88(t0)\n"
"  la t0,saved_ra\n  ld ra,0(t0)\n"
#endif
#if __riscv_xlen == 32
"  la t0,guest_out\n  lw t0,0(t0)\n  sw t3,0(t0)\n  sw t4,4(t0)\n  sw t5,8(t0)\n"
#else
"  la t0,guest_out\n  ld t0,0(t0)\n  sd t3,0(t0)\n  sd t4,8(t0)\n  sd t5,16(t0)\n"
#endif
"  ret\n"
".size riscv_run_guest,.-riscv_run_guest\n");

struct guest_result { uptr value; uptr sp_after; uptr callee_bad; };
extern void riscv_run_guest(uptr entry, uptr guest_sp, uptr result, struct guest_result *out);

struct segment { u64 vaddr, init, zero, payload; u32 align; u8 perms; uptr start, end; };

static u64 add64(u64 a, u64 b, int *overflow) {
  u64 c = a + b;
  if (c < a) *overflow = 1;
  return c;
}
static int overlaps(uptr a0, uptr a1, uptr b0, uptr b1) {
  return a0 < b1 && b0 < a1;
}
static uptr ptr_of(u64 x) { return (uptr)x; }

static __attribute__((noreturn, used, noinline)) void helper_main(uptr *initial_sp) {
  uptr argc = initial_sp[0];
  uptr *argv = initial_sp + 1;
  uptr *env = argv + argc + 1;
  uptr pagesz = 0, pagesz_found = 0;
  sptr fd, input_fd, rr;
  u8 statbuf[256];
  u8 *m, *result;
  u64 file_size;
  u32 nseg, i, j, meta_end;
  struct segment seg[NSEG_MAX];
  static const u8 mmagic[8] = { 'A','S','M','E','X','E',0,2 };
  static const u8 rmagic[8] = { 'A','S','M','R','E','S',0,2 };
  static const u8 empty_path[1] = { 0 };

  if (argc < 3) fail_no_record(CLS_IO);
  while (*env) ++env;
  ++env;
  while (env[0]) {
    if (env[0] == 6) { pagesz = env[1]; pagesz_found = 1; }
    env += 2;
  }
  if (argc >= 4 && bytes_equal((u8 *)argv[3], (const u8 *)"pgsz-bad", 8)) pagesz = PAGE_SIZE * 2;
  if (argc >= 4 && bytes_equal((u8 *)argv[3], (const u8 *)"pgsz-abs", 8)) pagesz_found = 0;

  fd = syscall6(SYS_openat, (uptr)-100, argv[2], 2 | 0100 | 0200, 0600, 0, 0);
  if (failed(fd)) fail_no_record(CLS_IO);
  if (failed(syscall3(SYS_ftruncate, (uptr)fd, PAGE_SIZE, 0))) fail_no_record(CLS_IO);

  rr = syscall6(SYS_mmap, WINDOW_BASE, WINDOW_SIZE, PROT_NONE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE, (uptr)-1, 0);
  if (failed(rr) || (uptr)rr != WINDOW_BASE) fail_no_record(CLS_MAPPING);
  rr = syscall6(SYS_mmap, 0, PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, (uptr)fd, 0);
  if (failed(rr)) fail_no_record(CLS_MAPPING);
  control_alias = (uptr)rr;
  result = (u8 *)control_alias;
  copy_bytes(result + R_MAGIC, rmagic, 8);
  wr16(result + R_VERSION, 2);
  if (!pagesz_found) fail_pre(CLS_ENVIRONMENT, 2);
  if (pagesz != PAGE_SIZE) fail_pre(CLS_ENVIRONMENT, 1);

  input_fd = syscall6(SYS_openat, (uptr)-100, argv[1], 0, 0, 0, 0);
  if (failed(input_fd)) fail_pre(CLS_IO, 3);
  if (failed(syscall6(SYS_statx, (uptr)input_fd, (uptr)empty_path,
                      0x1000, 0x200, (uptr)statbuf, 0))) fail_pre(CLS_IO, 4);
  file_size = rd64(statbuf + 40);
  if (file_size < MANIFEST_MIN) fail_pre(CLS_MANIFEST, 1);
  if (file_size > MANIFEST_MAX) fail_pre(CLS_MANIFEST, 2);
  rr = syscall6(SYS_mmap, 0, (uptr)file_size, PROT_READ, MAP_PRIVATE, (uptr)input_fd, 0);
  if (failed(rr)) fail_pre(CLS_IO, 5);
  m = (u8 *)(uptr)rr;

  if (!bytes_equal(m + M_MAGIC, mmagic, 8)) fail_pre(CLS_MANIFEST, 3);
  if (rd16(m + M_VERSION) != 2) fail_pre(CLS_MANIFEST, 4);
  if (rd16(m + M_PROFILE) != PROFILE_ID) fail_pre(CLS_MANIFEST, 5);
  if ((u64)rd32(m + M_TOTAL) != file_size) fail_pre(CLS_MANIFEST, 6);
  nseg = rd16(m + M_NSEG);
  if (nseg == 0 || nseg > NSEG_MAX) fail_pre(CLS_MANIFEST, 7);
  if (rd16(m + M_NEXPECTED) != 1) fail_pre(CLS_MANIFEST, 8);
  if (rd32(m + M_RESULT_SIZE) != 256) fail_pre(CLS_MANIFEST, 9);
  if (rd32(m + M_TIMEOUT) < TIMEOUT_MIN || rd32(m + M_TIMEOUT) > TIMEOUT_MAX)
    fail_pre(CLS_MANIFEST, 10);
  if (rd32(m + M_RESERVED) != 0) fail_pre(CLS_MANIFEST, 11);
  if (bytes_nonzero(m + M_EXPECTED + 8, 56)) fail_pre(CLS_MANIFEST, 12);

  meta_end = M_HEADER_SIZE + D_SIZE * nseg;
  if ((u64)meta_end > file_size) fail_pre(CLS_MANIFEST, 13);
  for (i = 0; i < nseg; ++i) {
    const u8 *d = m + M_HEADER_SIZE + D_SIZE * i;
    int ov = 0;
    seg[i].vaddr = rd64(d + D_VADDR); seg[i].init = rd64(d + D_INIT);
    seg[i].zero = rd64(d + D_ZERO); seg[i].payload = rd64(d + D_PAYLOAD);
    seg[i].align = rd32(d + D_ALIGN); seg[i].perms = d[D_PERMS];
    if (add64(seg[i].payload, seg[i].init, &ov) > file_size || ov)
      fail_pre(CLS_MANIFEST, 14);
  }
  for (i = 0; i < nseg; ++i) {
    if (seg[i].payload & 7) fail_pre(CLS_MANIFEST, 15);
    if (seg[i].payload && seg[i].payload < meta_end) fail_pre(CLS_MANIFEST, 16);
    if ((seg[i].payload != 0) != (seg[i].init != 0)) fail_pre(CLS_MANIFEST, 17);
  }
  for (i = 0; i < nseg; ++i) if (seg[i].init) {
    u64 ie = seg[i].payload + seg[i].init;
    for (j = i + 1; j < nseg; ++j) if (seg[j].init) {
      u64 je = seg[j].payload + seg[j].init;
      if (seg[i].payload < je && seg[j].payload < ie) fail_pre(CLS_MANIFEST, 18);
    }
  }
  {
    u64 cursor = meta_end;
    for (i = 0; i < nseg; ++i) if (seg[i].init) {
      u64 expected = (cursor + 7) & ~(u64)7;
      if (seg[i].payload != expected) fail_pre(CLS_MANIFEST, 19);
      if (bytes_nonzero(m + (uptr)cursor, (u32)(expected - cursor))) fail_pre(CLS_MANIFEST, 20);
      cursor = expected + seg[i].init;
    }
    {
      u64 end = (cursor + 7) & ~(u64)7;
      if (end != file_size) fail_pre(CLS_MANIFEST, 21);
      if (bytes_nonzero(m + (uptr)cursor, (u32)(end - cursor))) fail_pre(CLS_MANIFEST, 20);
    }
  }
  for (i = 0; i < nseg; ++i) {
    if (seg[i].init > INIT_MAX) fail_pre(CLS_MANIFEST, 22);
    if (seg[i].zero > ZERO_MAX) fail_pre(CLS_MANIFEST, 23);
  }
  for (i = 0; i < nseg; ++i) {
    u64 end, rounded;
    int ov = 0;
    if ((seg[i].init | seg[i].zero) == 0) fail_pre(CLS_MANIFEST, 24);
    if (!seg[i].align || (seg[i].align & (seg[i].align - 1))) fail_pre(CLS_MANIFEST, 25);
    if (seg[i].vaddr & (seg[i].align - 1)) fail_pre(CLS_MANIFEST, 26);
    if (seg[i].perms & ~PERM_KNOWN) fail_pre(CLS_MANIFEST, 27);
    if ((seg[i].perms & (PERM_W | PERM_X)) == (PERM_W | PERM_X)) fail_pre(CLS_MANIFEST, 28);
    end = add64(seg[i].vaddr, seg[i].init, &ov);
    end = add64(end, seg[i].zero, &ov);
    rounded = add64(end, PAGE_SIZE - 1, &ov) & ~(u64)(PAGE_SIZE - 1);
    if (ov) fail_pre(CLS_MANIFEST, 29);
    if ((seg[i].vaddr & ~(u64)(PAGE_SIZE - 1)) < WINDOW_BASE ||
        rounded > (u64)WINDOW_BASE + WINDOW_SIZE) fail_pre(CLS_MANIFEST, 30);
    seg[i].start = ptr_of(seg[i].vaddr & ~(u64)(PAGE_SIZE - 1));
    seg[i].end = ptr_of(rounded);
  }
  {
    u64 result64 = rd64(m + M_RESULT);
    u64 result_start64 = result64 & ~(u64)(PAGE_SIZE - 1);
    u64 result_end64 = result_start64 + PAGE_SIZE;
    uptr result_start = ptr_of(result_start64), result_end = ptr_of(result_end64);
    uptr stack_end = STACK_NORM + rd32(m + M_STACK_SIZE);
    if (result_end64 < result_start64) result_end = (uptr)-1;
    for (i = 0; i < nseg; ++i) {
      if (overlaps(seg[i].start, seg[i].end, result_start, result_end)) fail_pre(CLS_MANIFEST, 31);
      if (overlaps(seg[i].start, seg[i].end, STACK_NORM, stack_end)) fail_pre(CLS_MANIFEST, 32);
      if (overlaps(seg[i].start, seg[i].end, GUARD_NORM, GUARD_NORM + PAGE_SIZE)) fail_pre(CLS_MANIFEST, 33);
    }
    if (overlaps(result_start, result_end, STACK_NORM, stack_end)) fail_pre(CLS_MANIFEST, 34);
    for (i = 0; i < nseg; ++i) for (j = i + 1; j < nseg; ++j)
      if (overlaps(seg[i].start, seg[i].end, seg[j].start, seg[j].end)) fail_pre(CLS_MANIFEST, 35);
  }
#if __riscv_xlen == 32
  if (rd32(m + M_ENTRY + 4) || rd32(m + M_RESULT + 4)) fail_pre(CLS_MANIFEST, 42);
#endif
  {
    u64 entry = rd64(m + M_ENTRY);
    int found = 0;
    for (i = 0; i < nseg; ++i)
      if ((seg[i].perms & PERM_X) && entry >= seg[i].vaddr && entry < seg[i].vaddr + seg[i].init)
        found = 1;
    if (!found) fail_pre(CLS_MANIFEST, 36);
    if (entry & 3) fail_pre(CLS_MANIFEST, 37);
  }
  if (rd64(m + M_RESULT) & (PAGE_SIZE - 1)) fail_pre(CLS_MANIFEST, 38);
  if (rd64(m + M_RESULT) != RESULT_NORM) fail_pre(CLS_MANIFEST, 39);
  {
    u32 stack_size = rd32(m + M_STACK_SIZE);
    if (!stack_size || (stack_size & (PAGE_SIZE - 1)) || stack_size < STACK_MIN || stack_size > STACK_MAX)
      fail_pre(CLS_MANIFEST, 40);
  }

  rr = syscall6(SYS_mmap, RESULT_NORM, PAGE_SIZE, PROT_READ | PROT_WRITE,
                MAP_SHARED | MAP_FIXED, (uptr)fd, 0);
  if (failed(rr)) fail_pre(CLS_MAPPING, 3);
  rr = syscall6(SYS_mmap, STACK_NORM, rd32(m + M_STACK_SIZE), PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, (uptr)-1, 0);
  if (failed(rr)) fail_pre(CLS_MAPPING, 4);
  rr = syscall6(SYS_mmap, GUARD_NORM, PAGE_SIZE, PROT_NONE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, (uptr)-1, 0);
  if (failed(rr)) fail_pre(CLS_MAPPING, 5);
  for (i = 0; i < nseg; ++i) {
    rr = syscall6(SYS_mmap, seg[i].start, seg[i].end - seg[i].start,
                  PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, (uptr)-1, 0);
    if (failed(rr)) fail_pre(CLS_MAPPING, 6);
  }
  for (i = 0; i < nseg; ++i) if (seg[i].init)
    copy_bytes((u8 *)ptr_of(seg[i].vaddr), m + ptr_of(seg[i].payload), (u32)seg[i].init);
  for (i = 0; i < nseg; ++i) {
    u32 prot = 0;
    if (seg[i].perms & PERM_R) prot |= PROT_READ;
    if (seg[i].perms & PERM_W) prot |= PROT_WRITE;
    if (seg[i].perms & PERM_X) prot |= PROT_EXEC;
    if (failed(syscall3(SYS_mprotect, seg[i].start, seg[i].end - seg[i].start, prot)))
      fail_pre(CLS_MAPPING, 7);
  }
  for (i = 0; i < nseg; ++i) if (seg[i].perms & PERM_X) {
    if (failed(syscall3(SYS_riscv_flush_icache, seg[i].start, seg[i].end, 0)))
      fail_pre(CLS_CACHE, 3);
  }

  wr32(result + R_CASE, rd32(m + M_CASE));
  wr16(result + R_NEXPECTED, 1);
  wr16(result + R_NVALUES, 0);
  wr64(result + R_EXPECTED, rd64(m + M_EXPECTED));
  __asm__ volatile("fence rw,w" ::: "memory");
  wr32(result + R_STATE, RS_VALIDATED);
  {
    struct guest_result g;
    riscv_run_guest(ptr_of(rd64(m + M_ENTRY)), STACK_NORM + rd32(m + M_STACK_SIZE),
                    control_alias, &g);
    u64 actual = (u64)(sptr)(int)(u32)g.value;
    if (g.sp_after != guest_sp_expected) fail_late(CLS_PROTOCOL, 1);
    if (g.callee_bad) fail_late(CLS_PROTOCOL, 2);
    wr64(result + R_VALUES, actual);
    wr16(result + R_NVALUES, 1);
    wr32(result + R_DIAG_LEN, 0);
    __asm__ volatile("fence rw,w" ::: "memory");
    wr16(result + R_STATUS, actual == rd64(m + M_EXPECTED) ? ST_PASSED : ST_FAILED);
  }
  exit_class(0);
}

__attribute__((naked, noreturn, used)) void _start(void) {
  __asm__ volatile("mv a0,sp\n tail helper_main");
}
