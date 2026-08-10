# Execution ABI v1 — frozen

**Status: frozen.** Four assembly helpers implement this document; they do not
define it — writing this before any helper is the point (`.ai/asm_plan.md`
guardrail 1), so that four assembly implementations never become the de facto
specification. Any change to a wire layout, a state rule, a validation order, or
an existing subcode is an **ABI v2** bump rather than an edit; §17 states the one
narrow addition permitted within v1.

This is the contract between:

- the **target-assembly helper** (`asm/helpers/<profile>.s`), assembled and
  linked by the reference GNU `as`/`ld`, using direct Linux syscalls, **no C and
  no libc**, running under `qemu-<arch>` user mode; and
- the **host runner** (`asm/test/oracle/qemu_user.ml`), which serializes a bound
  image into a manifest, invokes the helper, and normalizes the outcome.

It is also the contract for the optional system-mode backend (§11.5.4 of
`.ai/asm_plan.md`), which shares the result-block format. Where the two differ,
the difference is stated, never implied.

Nothing in this document is a production API. Per `.ai/asm_plan.md` §9 the
production packages stop at address binding; everything here lives in test-only
code and checked-in target assembly.

## 1. Profiles

| `profile_id` | Profile | Emulator | Word size |
|---:|---|---|---:|
| 1 | x86-32 SysV | `qemu-i386` | 32 |
| 2 | x86-64 SysV | `qemu-x86_64` | 64 |
| 3 | ARM / A32 AAPCS | `qemu-arm` | 32 |
| 4 | AArch64 PCS | `qemu-aarch64` | 64 |

All integers on the wire are **little-endian**, at fixed offsets, unaligned
access never required. A big-endian profile (M6's PowerPC) byte-swaps into this
canonical form in its helper; the wire format does not change.

## 2. Page size

Fixed at **4096** for all four profiles, and **verified** against `AT_PAGESZ`
from the process auxiliary vector rather than assumed. The value is read at
process entry and checked at step 4b (§4). A mismatch is a rejection with class
**69 `environment`** and `exit_group(69)`.

It is *not* class 64 `manifest`: the mismatch comes from the auxiliary vector and
is rejected before any manifest is opened, so labelling it a manifest error would
blur the split between malformed input, I/O, mapping, cache, protocol and
environment failures.

The check cannot run literally first: that would put it ahead of the control
alias, making its subcode unobservable. Step 4b is the earliest point at which a
canonical `pre_run_error` record can actually be published.

## 3. Address map

The helper reserves a **1 MiB window** and every region lives inside it.

| Region | 32-bit (profiles 1, 3) | 64-bit (profiles 2, 4) |
|---|---|---|
| reserved window (`PROT_NONE`) | `0x30000000`–`0x30100000` | `0x40000000`–`0x40100000` |
| code (RX) | `0x30000000` | `0x40000000` |
| rodata (R) | `+0x10000` | `+0x10000` |
| data (RW, zero-fill) | `+0x20000` | `+0x20000` |
| result block (RW, shared alias) | `+0x30000` | `+0x30000` |
| guard page (`PROT_NONE`) | `+0x3F000` | `+0x3F000` |
| guest stack (RW, grows down) | `+0x40000` | `+0x40000` |

The `PROT_NONE` reservation is address space, not committed memory, and does not
count toward any budget. Containment plus pairwise non-overlap is the whole
budget rule; there is deliberately no separate aggregate mapped-memory check,
because it could never fail first and would have no reachable case.

Only the three-segment part of this map is verified today (`.ai/asm_plan.md`
records it at E2, established by a throwaway C probe that is not checked in). The
window reservation, the dual-alias result mapping and the guarded stack are new
mechanisms and stay unverified until the conformance suite reaches E3/E4. The
probe's cache synchronization came from a compiler builtin the helpers do not
share, so it is not evidence for §11 either.

## 4. Operation order

The result fd is mapped **twice**: once at a kernel-selected control address
outside the reservation, and once at the normative `result_addr` inside it. That
is what makes an error channel exist before anything can fail validation, and it
removes any need for a positioned-write path.

1. `open(argv[2], O_RDWR|O_CREAT|O_EXCL, 0600)`; `ftruncate(fd, 4096)`. `O_EXCL`
   in a **host-owned private temporary directory** makes freshness atomic rather
   than depending on an earlier "does not exist" check surviving a race.
2. Reserve the window:
   `mmap(base, 0x100000, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE)`.
3. Map the result fd `MAP_SHARED` at a **kernel-selected control address**, which
   is necessarily outside the reservation just made. This is the error channel.
4. Write the **bootstrap record** through the control alias.
   **4b.** Reject a page-size mismatch here — after the bootstrap record exists,
   so the failure is observable, and before the input is opened, so it precedes
   every manifest stage.
5. `open` and **`fstat`** the input — **without mapping it**. Reject
   `st_size < 120` or `st_size > 1 MiB` as *manifest size* errors, so an empty or
   truncated file yields the promised manifest subcode rather than an incidental
   mapping fault.
6. `mmap` **exactly `st_size`** read-only, then validate the manifest (§9). On
   failure write the runner-error record through the control alias and
   `exit_group(64)`.
7. Map a **second shared alias of the same fd** at `result_addr` inside the
   reservation; then the stack and guard; then the segments (`MAP_FIXED` inside
   our own reservation, safe because overlap arithmetic has already run). Both
   aliases address the same page, so guest-visible and helper-visible updates are
   automatically identical.
8. Copy payloads, zero tails, `mprotect` each segment to its final permissions,
   and perform per-profile instruction-cache synchronization over executable
   ranges. **All of this happens before any `running` commit**, so a fault here
   is unambiguously a transport failure rather than a guest fault.
9. Write the validated fields (`case_id`, `n_expected = 1`, `expected[]`,
   **`n_values = 0`**, zeroed actuals), then commit `record_state = validated`.
10. Save helper state and switch to the validated guest SP.
11. Commit `status = running` — **already on the guest stack**.
12. Call `entry_addr` **immediately** (the next instruction).
13. On return, **capture the 32-bit return register immediately**, then commit
    `record_state = returned` — **before** restoring the helper SP.
14. Restore helper state; write the actual value, **set `n_values = 1`**, write
    `diag`/`diag_len`; then commit `status = passed | failed` **last**;
    `exit_group(0)`.

### 4.1 What the `running` phase actually guarantees

A store and the following instruction cannot be made atomic, so "exactly the
call" would overstate it. The committed `running` phase is **the call plus a
fixed, documented entry/return boundary**: the few instructions between the
`status = running` store and the call, and between the return-register capture
and the `record_state = returned` store. Those instructions are enumerated per
profile in §11 and are **proven non-faulting under the already-validated mapping
invariants** — they touch only the mapped result page, the validated guest stack,
and registers.

**The residual window is conservatively attributed to the guest, and that is the
frozen contract.** At an injection point inside it the `returned` commit has by
definition not happened, so the only recoverable record is `validated/running`;
the wire carries no PC, marker, or handler report that could identify the failure
as helper-side. ABI v1 therefore does **not** claim every helper fault is
distinguishable. Conformance injects a failure there and **asserts exactly that
limitation** (§16.4). Distinguishing it would need a helper signal handler with a
separately committed phase marker — more assembly and signal-ABI work than the M1
walking skeleton justifies, and even then the commit could not be made atomic
with the guest return.

Helper-private saved state, including the saved SP, lives in the **helper ELF's
own `.bss`**, addressed PC-relatively or at its link-time address — never in the
result page, which the guest can see through the second alias.

## 5. Commit semantics

**There is no single commit word.** Both error paths leave `status` unchanged —
early errors keep `not_started`, late errors keep `running` — so re-storing the
same value could not tell the host whether the preceding stores completed. Each
transition has its own commit field, always one whose value actually changes:

| Transition | Commit field, written last |
|---|---|
| → `validated` | `record_state` |
| → `running` | `status` |
| → `returned` | `record_state` |
| → `passed` / `failed` | `status` |
| → `fault` (system-mode backends only) | `status` |
| → any runner error | `runner_error`, stored 0 → nonzero class |

- Staged data is written **before** its commit field: actuals and diagnostics
  before the `status` commit; `error_subcode` and any `record_state` change
  before the `runner_error` commit.
- `error_subcode` is ignored while `runner_error = 0`.
- The host ignores result staging (`values[]`, `n_values`, `diag`, `diag_len`)
  **only in the `running` cells enumerated in §15.2 step 4** — not "whenever status is
  non-terminal", which would wrongly excuse `bootstrap/not_started` and
  `validated/not_started` from their zero requirement.
- The **terminal set is `{passed, failed, fault}`**. `fault` is included so a
  system-mode backend committing it can also publish a diagnostic, which a
  `{passed, failed}`-only rule would silently discard.
- On weakly-ordered profiles (ARM, AArch64) a **release barrier precedes every
  commit store in the table above** — including *both* `record_state` commits,
  not only `status` and `runner_error` (`dmb ish`, or a store-release). x86 is
  TSO. Concretely: the validated fields must be visible before
  `record_state = validated`, and the captured return value before
  `record_state = returned`.
- A timeout before a commit legitimately yields `result = unavailable`. No
  atomically-updated multi-field record is required, and no generation or
  checksum scheme is needed.

## 6. Record states

Host validation **branches on `record_state`**.

| `record_state` | Written at | Valid contents |
|---|---|---|
| `0 bootstrap` | step 4 | magic, version, `status = not_started`, `case_id = 0`, `n_expected = n_values = 0`, every other field zero |
| `1 pre_run_error` | any failure before the guest is entered | `runner_error` committed nonzero with `error_subcode`; `status = not_started`; **no trusted manifest fields have been published**, so `case_id`/`expected[]` stay zero |
| `2 validated` | step 9 | trusted `case_id`, `n_expected = 1`, `expected[]` populated, unused entries zero; `status ∈ {not_started, running, fault}`; `n_values = 0` |
| `3 returned` | step 13 | as `validated`, plus the guest returned; `status ∈ {running, passed, failed}`; `n_values = 1` once terminal |

State 1 is named **`pre_run_error`**, not `runner_error`: it is used for
second-alias, segment, `mprotect` and cache-sync failures too, all of which occur
*after* manifest validation succeeded. Its accurate meaning is **"failed before
the guest ran, with no trusted manifest fields published"**. Whether validation
itself succeeded is carried by the subcode, never inferred from `record_state`.

**`fault` belongs to `validated`**, where a system-mode shim or debugger-backed
runner commits it because the guest never returned. **`returned + fault` is
invalid**: if control came back, the outcome is `passed` or `failed`. The
user-mode helper never publishes `fault` at all — an unhandled guest signal kills
the process, leaving `running` — but the matrix is shared by both backends.

### 6.1 Terminal-fault schema

`n_values` means *recorded observations*, not configured slots, so a fault
records none:

- `record_state = validated`, `status = fault`;
- `n_expected = 1` (the manifest asked for one) but **`n_values = 0`**; every
  actual slot stays zero and is ignored. Requiring `n_values = 1` here would let
  the initialized zero in `values[0]` masquerade as a real observation from a
  guest that never returned one;
- diagnostic bytes and `diag_len` may be staged, then `status = fault` is the
  release-ordered commit store;
- terminal validation therefore checks value completeness only for
  `passed`/`failed`, and diagnostic canonicality for all three terminal statuses.

Because the QEMU-user helper **cannot** publish this state, conformance includes
**synthetic result-record tests** for it. The optional system backend must not be
the first validator of a shared wire format.

**Late failures after the guest returns stay `returned`**, with a committed
nonzero `runner_error`; they do not revert to state 1.

Invariants: `runner_error ≠ 0 ⟹ status ∉ {passed, failed, fault}`, and trusted
fields (`case_id`, `expected[]`) are readable exactly when `record_state ≥ 2`.

## 7. Failure stages

| Failure point | Class | Record state after | Exit |
|---|---:|---|---:|
| result `open`/`ftruncate`/control `mmap` | 65 io | ***no record*** — nothing is writable yet | 65 |
| window reservation | 66 mapping | ***no record*** — precedes the control alias and the bootstrap write | 66 |
| `AT_PAGESZ ≠ 4096` (step 4b) | 69 environment | `bootstrap` → `pre_run_error` | 69 |
| input `open`/`fstat` | 65 io | `bootstrap` → `pre_run_error` | 65 |
| input size bounds or `mmap` | 64 manifest / 65 io | `bootstrap` → `pre_run_error` | 64 / 65 |
| manifest validation | 64 manifest | `bootstrap` → `pre_run_error` | 64 |
| second result alias, stack, guard, or segment mapping | 66 mapping | `bootstrap` → `pre_run_error` | 66 |
| `mprotect` (step 8, before any `running` commit) | 66 mapping | `bootstrap` → `pre_run_error` | 66 |
| cache synchronization (step 8) | 67 cache-sync | `bootstrap` → `pre_run_error` | 67 |
| detected call-ABI violation after the guest returns | 68 protocol | stays `returned`; `status` stays `running`; `runner_error` committed | 68 |

Host normalization accepts exits **64–69**. Because step 8 precedes the `running`
commit, the `mprotect` and cache-sync rows land in `bootstrap`/`pre_run_error`
rather than mid-run — which is precisely what makes helper faults distinguishable
from guest faults.

## 8. Manifest wire format

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `magic` = `ASMEXE\0\x01` |
| 8 | 2 | `abi_version` u16 = 1 |
| 10 | 2 | `profile_id` u16 (see §1) |
| 12 | 4 | `total_len` u32 — must equal the file size |
| 16 | 4 | `case_id` u32 |
| 20 | 2 | `n_segments` u16, 1…8 |
| 22 | 2 | `n_expected` u16, **= 1** |
| 24 | 8 | `entry_addr` u64 |
| 32 | 8 | `result_addr` u64 |
| 40 | 4 | `result_size` u32, **= 256** |
| 44 | 4 | `stack_size` u32 |
| 48 | 4 | `timeout_ms` u32 (advisory; the host enforces) |
| 52 | 4 | reserved, zero |
| 56 | 64 | `expected[8]`; entries ≥ `n_expected` must be zero |
| 120 | 40×n | segment descriptors |
| … | … | payloads, each 8-byte aligned |

Segment descriptor, 40 bytes:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `vaddr` u64 |
| 8 | 8 | `init_len` u64 |
| 16 | 8 | `zero_len` u64 |
| 24 | 8 | `payload_off` u64 |
| 32 | 4 | `align` u32 |
| 36 | 1 | `perms` u8 — bit0 R, bit1 W, bit2 X |
| 37 | 3 | reserved, zero |

## 9. Page arithmetic

Rounding `vaddr` down and `init+zero` up *separately* loses the intra-page offset
and under-maps a segment starting near a page end. The frozen computation is:

```text
map_start = floor_page(vaddr)
data_end  = checked_add(vaddr, checked_add(init_len, zero_len))
map_end   = ceil_page(data_end)
map_len   = checked_sub(map_end, map_start)
copy_at   = vaddr - map_start
```

## 10. Validation

A raw-assembly helper must never read variable-length data before proving it is
in-file; otherwise a truncated descriptor table or an out-of-file payload faults
the helper instead of producing its promised subcode.

**Every memory-safety bound uses the trusted `fstat` size, never an unvalidated
wire field.** Comparing descriptor and payload ends against the manifest's own
`total_len` would be comparing against attacker-controlled data: a 120-byte file
could claim `total_len = 1 MiB`, pass that check, and send the helper reading past
the end of the mapping before the structural `total_len = file size` test ever ran.

| Stage | Check |
|---:|---|
| 0 | **Environment.** `AT_PAGESZ = 4096`, read at process entry and rejected at step 4b — before the input is opened, so it precedes every stage below and its subcode is deterministic on a multi-violation input. Every later bound is page arithmetic, so a wrong page size invalidates everything after it. |
| 1 | `open` + **`fstat`** the input **without mapping it**; reject `st_size < 120` or `st_size > 1 MiB`. |
| 2 | `mmap` **exactly `st_size`**; read the fixed 120-byte header. |
| 3 | **Fixed header — the ABI discriminator.** `magic`, `abi_version`, `profile_id`, `total_len = st_size`, `n_segments` ∈ 1…8, `n_expected = 1`, `result_size = 256`, **`timeout_ms` ∈ 100…60000**, header reserved bytes and the unused `expected[]` tail zero. |
| 4 | **`120 + 40 × n_segments` ≤ `st_size`**, checked arithmetic, **before any descriptor is read**. |
| 5 | **Every `payload_off + init_len` ≤ `st_size`**, checked arithmetic, **before any payload byte is read**. |
| 6 | **Per-descriptor payload semantics**: `payload_off` 8-byte aligned; at or after the descriptor table (no payload inside metadata); **`payload_off ≠ 0` iff `init_len > 0`**. |
| 7 | **Pairwise payload non-overlap.** |
| 8 | **Canonical packing** (§10.1). |
| 9 | **Per-descriptor resource caps**, in descriptor-index order: `init_len` ≤ 256 KiB, `zero_len` ≤ 1 MiB. |

Stage 3 comes before stages 4–8 deliberately: otherwise a file with bad magic or
an unsupported future ABI would be interpreted as a v1 descriptor/payload layout
and rejected with a packing or payload subcode rather than a format-identity one.
Bounds safety was never at risk — that comes from `st_size` — but the versioning
boundary must be settled first.

Stage 9 comes before address geometry so that an oversized `zero_len` — which
also violates window containment — deterministically yields the *resource*
subcode rather than a geometry one.

### 10.1 Canonical packing

Descriptors with `init_len > 0` carry payloads **tightly packed in descriptor
order** at 8-byte alignment, so for those descriptors payload-offset order does
follow descriptor order and "the last payload" is unambiguous. Descriptors with
`init_len = 0` are **skipped** (`payload_off = 0`, no payload). What need *not*
follow descriptor order is **virtual-address order**: segments may be described
in any `vaddr` order.

Inter-payload alignment gaps must be **zero**. The canonical file end is the
8-byte-aligned end of the final payload — or, when no descriptor has a payload,
the 8-byte-aligned end of the descriptor table — and any bytes in that final
alignment are required **zero padding**. `st_size` must equal that end exactly;
anything beyond it is a trailing-bytes violation.

Packing runs **after** stages 6–7 deliberately. Checking the exact packing
recurrence first would make *every* misaligned offset, payload-before-metadata,
wrong zero/nonzero pairing and payload overlap violate packing before reaching
its own rule, rendering all four of those subcodes unreachable.

### 10.2 Precedence

Validation precedence is fixed, so "the exact subcode" is deterministic when an
input violates several rules at once:

1. stages 0–9 in order;
2. then per-descriptor **geometry** in descriptor-index order;
3. then **collisions** in a fixed pair order;
4. then **cross-region well-formedness**.

First failure wins.

Collisions deliberately precede the normative-address rules. Otherwise a manifest
moving the result page onto the stack would always fail the "`result_addr` equals
its normative address" check first, making the result/stack collision subcode
unreachable.

**Geometry**: zero-length segment; `align` zero or not a power of two; `vaddr`
misaligned against `align`; unknown permission bits; **final W+X**; address
overflow; any region outside the reserved window; and **overlap between any two
occupied regions on rounded page ranges** — because `mprotect` is page-granular.
The occupied set is explicitly **{each segment, the result page, the guest stack,
the guard page}**. Naming the guard is essential: segments are installed with
`MAP_FIXED` *after* the guard, so a segment admitted over it would silently
replace `PROT_NONE` and void the overflow test.

**Well-formedness**: `entry_addr` lies within the initialized byte range of a
segment carrying X *and* satisfies §10.3; `result_addr` is page-aligned and equals
the profile's normative result address; `stack_size` is nonzero, a page multiple,
within min/max, and stack plus guard exactly matches the normative region; on
32-bit profiles the **high 32 bits of every u64 address field are zero**.

### 10.3 Per-profile entry validity

| Profile | Entry rule |
|---|---|
| ARM / A32 | **word-aligned**, and **bit 0 clear** — ARM state, never Thumb, matching the `-marm` fixtures |
| AArch64 | **4-byte aligned** |
| x86-32 / x86-64 | no instruction-alignment restriction |

Additionally, the **x86 helpers execute `cld` before entry**: SysV requires a
clear direction flag at a function boundary, and later CompCert-generated string
operations must not inherit incidental helper state.

### 10.4 Resource limits

| Bound | Limit | Validated at |
|---|---|---|
| total file | `120 + 40 × n_segments` … 1 MiB | stage 1 (`fstat`) / stage 4 |
| page size | `AT_PAGESZ` = 4096 | stage 0, rejected at step 4b |
| `timeout_ms` | 100 … 60000 | stage 3, with the fixed header |
| `init_len` | ≤ 256 KiB | stage 9, per descriptor |
| `zero_len` | ≤ 1 MiB | stage 9, per descriptor |
| `stack_size` | 4 KiB … 256 KiB | cross-region well-formedness |

The minimum valid manifest is `120 + 40 × n_segments` bytes — 160 for a single
pure-zero-fill segment — before aligned payloads.

## 11. Per-profile call ABI

| Profile | Initial SP | Alignment at the call | Saved helper SP |
|---|---|---|---|
| x86-32 SysV | `stack_start + stack_size` | ESP 16-byte aligned *at the `call`*; entry observes ESP ≡ 12 (mod 16) | helper `.bss` + `ebx` |
| x86-64 SysV | same | RSP 16-byte aligned *at the `call`*; entry observes RSP ≡ 8 (mod 16) | helper `.bss` + `rbx` |
| ARM AAPCS | same | SP 8-byte aligned at the public interface | helper `.bss` + `r4` |
| AArch64 PCS | same | SP 16-byte aligned | helper `.bss` + `x19` |

The stack grows **downward** from `stack_start + stack_size`. The helper's SP is
saved in its own `.bss`, reachable **PC-relatively without depending on any
guest-preserved register**; the callee-saved register is a convenience, not the
recovery mechanism. The helper assumes the callee preserves the profile's
callee-saved set and **captures the integer return register immediately on
return, before any syscall**.

**What `runner_error` can and cannot promise.** It is guaranteed **only when the
helper regains control and detects an ABI violation**. Code that corrupts the
return path cannot be relied on to hand control back; that outcome normalizes as
`fault`, `signal` or `timeout` in the ordinary way.

## 12. Instruction-cache synchronization

| Profile | Mechanism |
|---|---|
| x86-32 / x86-64 | I-cache is coherent with stores; the `mprotect` syscall plus the indirect call is sufficient serialization. No cache syscall. |
| ARM | the ARM-specific Linux `cacheflush` syscall over each executable range |
| AArch64 | explicit maintenance — `DC CVAU` / `DSB ISH` / `IC IVAU` / `DSB ISH` / `ISB`, with D- and I-cache line sizes read from `CTR_EL0` |

Each gets a behavioral smoke test in M0.3. The throwaway C probe used a compiler
builtin the helpers do not share, so it is not evidence for any of these.

## 13. Result block

256 bytes of record inside a 4096-byte page.

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `magic` = `ASMRES\0\x01` |
| 8 | 2 | `abi_version` u16 = 1 |
| 10 | 2 | `status` u16 — 0 `not_started`, 1 `running`, 2 `passed`, 3 `failed`, 4 `fault` |
| 12 | 4 | `case_id` u32 |
| 16 | 2 | `n_expected` u16 |
| 18 | 2 | `n_values` u16 |
| 20 | 4 | `diag_len` u32 ≤ 64 |
| 24 | 64 | `expected[8]` |
| 88 | 64 | `values[8]` (actual) |
| 152 | 64 | `diag`, unused tail zero |
| 216 | 2 | `runner_error` u16 — class (§14) |
| 218 | 2 | `error_subcode` u16 (§14) |
| 220 | 4 | `record_state` u32 — 0 bootstrap, 1 pre_run_error, 2 validated, 3 returned |
| 224 | 32 | reserved, zero |
| 256 | 3840 | **page padding, reserved — zero-initialized and required zero on recovery** |

Bytes 256–4095 are frozen as reserved padding rather than left undefined:
`ftruncate` zeroes them at creation, the helper never writes there, and recovery
requires them zero — which also catches a stray guest store into the shared page.

Status `4 fault` is **admitted, not reserved**. §11.5.1 requires all five statuses
in the common result block and the same format serves both backends, so reserving
it would be a silent deviation and admitting it later under the same ABI version
would itself change the wire-validity rules.

**One configured observation source.** `n_expected = 1` always. **`n_values` is
determined by the committed state**, not fixed — 0 in `validated` (including
`validated + fault`), 1 only once `passed`/`failed` is committed in `returned`.

The value is the profile's **32-bit** integer return register — `eax` on **both**
x86 profiles, `r0` on ARM, `w0` on AArch64 — **sign-extended 32→64** and
reinterpreted as u64. Not `rax` on x86-64: a C `int` result lives in `eax` and the
upper half of `rax` is not part of the value, so a negative return of
`eax = 0xffffffff` must normalize to `0xffffffffffffffff`, not to a zero-extended
raw `rax`. The x86-64 helper performs an explicit 32-to-64 sign extension.

Additional observation sources require an **ABI v2** bump.

## 14. Error classes and subcodes

### 14.1 Classes

| Class | Name | Meaning |
|---:|---|---|
| 0 | none | no runner error |
| 64 | manifest | malformed or out-of-policy input |
| 65 | io | a syscall on a file failed |
| 66 | mapping | `mmap`/`mprotect` failed, or geometry is unusable |
| 67 | cache-sync | instruction-cache maintenance failed |
| 68 | protocol | a call-ABI violation detected after the guest returned |
| 69 | environment | the process environment is not what the ABI requires |

The class is also the process exit code. The host accepts 64–69; **any other exit
code** normalizes to `runner_error` with `error_subcode = unexpected_exit` and the
numeric code recorded — QEMU loader errors, malformed helper invocation and helper
bugs must never fall through or be mistaken for a guest result.

### 14.2 Subcodes

Subcodes are **scoped to their class**: ABI v1 does not require a globally unique
subcode namespace, because the recovery validator never consults a class-specific
table before the `runner_error` commit (§15.2). `0` is never a valid committed
subcode.

Every subcode below carries a conformance case proving it is **observable**, run
against the final precedence table of §10.2 — not merely that the check exists.

**Class 64 — manifest**

| Subcode | Stage | Condition |
|---:|---:|---|
| 1 | 1 | `st_size < 120` (empty or truncated header) |
| 2 | 1 | `st_size > 1 MiB` |
| 3 | 3 | bad `magic` |
| 4 | 3 | unsupported `abi_version` |
| 5 | 3 | unknown or wrong `profile_id` |
| 6 | 3 | `total_len ≠ st_size` |
| 7 | 3 | `n_segments` outside 1…8 |
| 8 | 3 | `n_expected ≠ 1` |
| 9 | 3 | `result_size ≠ 256` |
| 10 | 3 | `timeout_ms` outside 100…60000 |
| 11 | 3 | header reserved bytes nonzero |
| 12 | 3 | `expected[]` tail beyond `n_expected` nonzero |
| 13 | 4 | descriptor table end exceeds `st_size` |
| 14 | 5 | `payload_off + init_len` exceeds `st_size` |
| 15 | 6 | `payload_off` not 8-byte aligned |
| 16 | 6 | payload starts inside the metadata region |
| 17 | 6 | `payload_off ≠ 0` and `init_len = 0`, or the converse |
| 18 | 7 | two payloads overlap |
| 19 | 8 | payloads not tightly packed in descriptor order |
| 20 | 8 | inter-payload alignment gap is nonzero |
| 21 | 8 | trailing bytes past the canonical file end |
| 22 | 9 | `init_len > 256 KiB` |
| 23 | 9 | `zero_len > 1 MiB` |
| 24 | geometry | zero-length segment (`init_len = zero_len = 0`) |
| 25 | geometry | `align` zero or not a power of two |
| 26 | geometry | `vaddr` misaligned against `align` |
| 27 | geometry | unknown permission bits set |
| 28 | geometry | final permissions are W+X |
| 29 | geometry | address overflow computing `data_end` |
| 30 | geometry | region falls outside the reserved window |
| 31 | collision | segment overlaps the result page |
| 32 | collision | segment overlaps the guest stack |
| 33 | collision | **segment overlaps the guard page** |
| 34 | collision | result page overlaps the guest stack |
| 35 | collision | two segments overlap |
| 36 | well-formed | `entry_addr` outside the initialized range of an X segment |
| 37 | well-formed | `entry_addr` violates the profile rule of §10.3 |
| 38 | well-formed | `result_addr` not page-aligned |
| 39 | well-formed | `result_addr` ≠ the profile's normative address |
| 40 | well-formed | `stack_size` zero, not a page multiple, or outside 4 KiB…256 KiB |
| 41 | well-formed | stack plus guard does not match the normative region |
| 42 | well-formed | high 32 bits of a u64 address field nonzero on a 32-bit profile |

**Class 65 — io**

| Subcode | Condition |
|---:|---|
| 1 | result `open` failed |
| 2 | `ftruncate` failed |
| 3 | input `open` failed |
| 4 | input `fstat` failed |
| 5 | input `mmap` failed |

**Class 66 — mapping**

| Subcode | Condition |
|---:|---|
| 1 | window reservation failed (no record can exist) |
| 2 | control-alias `mmap` failed (no record can exist) |
| 3 | second result alias failed |
| 4 | guest stack mapping failed |
| 5 | guard page mapping failed |
| 6 | segment mapping failed |
| 7 | `mprotect` to final permissions failed |

**Class 67 — cache-sync**

| Subcode | Condition |
|---:|---|
| 1 | ARM `cacheflush` syscall failed |
| 2 | AArch64 `CTR_EL0` reported an unusable line size |

**Class 68 — protocol**

| Subcode | Condition |
|---:|---|
| 1 | guest returned with a corrupted stack pointer |
| 2 | guest returned with a callee-saved register clobbered |

**Class 69 — environment**

| Subcode | Condition |
|---:|---|
| 1 | `AT_PAGESZ ≠ 4096` |
| 2 | `AT_PAGESZ` absent from the auxiliary vector |

`qemu-user` cannot be made to report a non-4096 `AT_PAGESZ` on the four
provisioned profiles, so class 69 is exercised by an **assembly-only test entry
that fabricates the saved page size and enters the same step-4b publication
branch** — not merely by calling the predicate in isolation. It must assert the
**complete published artifact**: `exit_group(69)`, a canonical `pre_run_error`
record, and the exact subcode. Anything less would test the check without testing
its observability. The validator and its test entry are both target-assembly test
transport, so the no-C boundary is untouched.

## 15. Host-side recovery and normalization

Deterministic, from the wait status — never from QEMU's exit text.

### 15.1 Attribution by phase

The same QEMU process runs both the helper and the guest, so a helper fault
during manifest handling, `mprotect`, cache maintenance, stack switching or
result publication produces an *identical* wait status to a deliberate guest
trap. The committed `record_state`/`status` pair disambiguates it:

| Recovered state when the signal arrived | Attribution |
|---|---|
| no record, or `bootstrap` / `pre_run_error` | **transport failure** — `termination = runner_error`, never guest evidence |
| `validated` with `status = not_started` | **helper fault before entry** — transport failure |
| `validated` with `status = running` | **guest fault** — `termination = fault` |
| `returned` | **helper fault after the guest returned** — transport failure |

This is only sound because step 8 moved mapping, protection and cache
synchronization *before* the `running` commit.

Host timeouts use the same table:

| Recovered state at the kill | Attribution |
|---|---|
| `validated` with `status = running` | guest **`timeout`** |
| no record, `bootstrap`, `pre_run_error`, or `validated` with `status = not_started` | `runner_error` / **`transport_timeout`** |
| `returned` | `runner_error` / **`post_return_timeout`** |

Classifying the host's own kill as `timeout` unconditionally would let a helper
looping in manifest processing, cache maintenance or post-return publication
produce the *same* artifact as the deliberate looping guest used to establish E4
— letting a runner defect masquerade as a working guest-timeout control. The host
still records that *it* sent the kill, so none of these degrades into an ordinary
external `signal`.

Given that phase check: `WIFSIGNALED` with SIGILL, SIGSEGV, SIGBUS, SIGFPE or
SIGTRAP in the guest phase → **`fault`**; any other signal → **`signal`**;
`WIFEXITED` 0 → **`completed`**; 64–69 → **`runner_error`** with that class; any
other exit code → `runner_error` / `unexpected_exit`.

### 15.2 The recovery validator

Recovery is conditional on what exists: `open`, `ftruncate`, control-alias `mmap`
and reservation failures can leave no valid record at all. So the broad
`runner_error` **class comes from the exit code**; the **subcode is read only when
a complete runner-error record validates**; otherwise the artifact carries
`result = unavailable` and `error_subcode = unavailable`. The guarantee that
"every case has an exact observable subcode" is scoped to failures occurring
**after the control alias exists** — which includes every malformed-manifest
conformance case.

The validator runs in **four fixed steps**. The raw pending-error patterns must be
recognized *before* the canonical checks they intentionally bypass: the
undefined-field-zero rule would reject a staged `error_subcode`, and the
state-combination rule would reject `pre_run_error` with `runner_error = 0`, so
neither pattern could ever be reached if the order were swapped.

**Step 1 — unconditional framing.** Holds in every recoverable record regardless
of state, so it cannot pre-empt step 2:

- exact 4096-byte file; exact `magic` and `abi_version`;
- the unconditional reserved bytes and a **zero page tail** (offsets 256–4095).

**Step 2 — raw pending-error patterns.** Saying "read as the previous state" is
not implementable on its own: once raw `record_state` has been stored as
`pre_run_error`, the prior zero is gone from the file.

Because step 2 **returns immediately**, each accepted pattern must itself enforce
every step-3 and step-4 invariant it thereby bypasses. Both are therefore stated
as *complete* wire predicates:

| # | Raw pattern (all conditions required) | Interpretation |
|---|---|---|
| A | `record_state = bootstrap`, `status = not_started`, `runner_error = 0`, a **nonzero** staged `error_subcode`, and **every other bootstrap and result-staging field canonical zero** | valid **bootstrap**; ignore only `error_subcode` |
| B | `record_state = pre_run_error`, `status = not_started`, `runner_error = 0`, a **nonzero** staged `error_subcode`, and **every other bootstrap and result-staging field canonical zero** | **`pending_pre_run_error`** — an uncommitted transition; report `result = unavailable`, since the error class never committed and cannot be inferred |

A third shape, `record_state = pre_run_error` with `runner_error ≠ 0`, is not an
exception at all: it is the canonical **terminal early-error** state, validated
normally in step 3.

**"Nonzero" means exactly that — a nonzero u16, nothing more.** With
`runner_error = 0` the class commit has not happened, and pattern B says outright
that the class cannot be inferred, so step 2 **cannot** consult a class-specific
subcode table. An **unknown nonzero subcode** is a required raw-pattern test case.

Zero-subcode outcomes differ between the two patterns:

| Raw state | `error_subcode` | Outcome |
|---|---:|---|
| `bootstrap`, otherwise canonical | 0 | **canonical bootstrap** — succeeds in step 3 |
| `bootstrap`, otherwise canonical | nonzero | **pattern A** — bootstrap, staged subcode ignored |
| `pre_run_error`, `runner_error = 0`, otherwise bootstrap | 0 | **invalid** — neither a pending pattern nor a committed error; fails step 3 |
| `pre_run_error`, `runner_error = 0`, otherwise bootstrap | nonzero | **pattern B** — `result = unavailable` |

This follows from the writer's store order: `error_subcode` is written before the
`runner_error` commit, so an interruption *before* that store leaves a plain
canonical record. `error_subcode` and a staged `record_state` are *error* staging,
not result staging, and are the only nonzero values these otherwise-zero records
may contain.

**Step 3 — canonical committed state.** State-dependent, and therefore after raw
recognition:

- a **known `record_state`**, and a `status`/`runner_error` combination allowed
  for that state by §6;
- zero for every field the state does not define (e.g. `case_id` and `expected[]`
  in a `bootstrap` record);
- for `record_state ≥ 2`, the expected `case_id` and `n_expected = 1`.

A malformed near-miss of either step-2 pattern falls through to step 3 and fails
there, which is what keeps the two exceptions narrow.

**Step 4 — the result-staging matrix.** One explicit `(record_state, status)`
table, not a broad "terminal versus nonterminal" rule — three overlapping informal
statements ("ignored whenever status is not terminal", "checked only by terminal
rules", "ignored while `status = running`") give *different* answers for a
committed `validated/not_started` record, which is precisely the state §15.1 uses
to classify a helper failure between the `validated` and `running` commits.

| State | Result staging (`values[]`, `n_values`, `diag`, `diag_len`) |
|---|---|
| `bootstrap` / `not_started` | required **zero** |
| `pre_run_error` / `not_started` | required **zero**; committed runner-error schema applies |
| `validated` / `not_started` | required **zero** — `n_values = 0`, zero actuals, zero diagnostic fields; no staging has begun |
| `validated` / `running` | **ignored** — step 14 is interruptible |
| `returned` / `running` | **ignored** — same |
| `validated` / `fault` | `n_values = 0`, zero actuals, **canonical** terminal diagnostics |
| `returned` / `passed` \| `failed` | `n_values = 1`, one complete actual, **canonical** terminal diagnostics |

Canonical terminal diagnostics mean `diag_len ≤ 64` with a zero tail. Value
completeness applies **only** to `passed`/`failed`; `fault` records no
observation. **Any `(record_state, status)` pair absent from this matrix is
invalid.**

A record caught mid-update by a timeout therefore reports `termination = timeout`,
`status = running`, and its staged bytes are discarded rather than treated as a
validation failure.

### 15.3 Freshness

The result path is created `O_EXCL` in a host-owned private temporary directory,
so freshness is atomic rather than depending on an earlier existence check.

## 16. Conformance requirements

### 16.1 Malformed manifests

Every subcode in §14.2, each requiring its **exact** subcode: result/image,
result/stack, **segment/guard** and segment/stack collisions, misaligned
`result_addr`, wrong `result_size`, result-address overflow on both address
widths, entry outside an executable segment, payload overlap, 32-bit
high-bit-set addresses, the exact minimum/maximum resource boundaries and
max-plus-one, and the staged-bounds cases — **empty file, truncated header,
truncated descriptor table, out-of-file payload, `total_len` overstating the real
`st_size`, nonzero alignment gap, trailing bytes**.

Coverage is **reachability-based, not merely precedence-based**: every promised
subcode carries a test proving it can actually be observed, run against the
**final** precedence table. A **multi-violation input** additionally proves
precedence is deterministic.

### 16.2 Positive snippets

Entry SP alignment; a snippet that **consumes the entire declared stack without
touching the guard**; and a **one-byte overflow that reliably faults**.

### 16.3 Deliberate trap snippets

Frozen per profile, with verified bytes. This is the control that establishes
fault classification independently of assembler-produced code, so it must trap
*at that instruction*, not merely die eventually.

| Profile | Instruction | Word | Memory bytes | Expected signal |
|---|---|---|---|---|
| x86-32 | `ud2` | — | `0f 0b` | SIGILL |
| x86-64 | `ud2` | — | `0f 0b` | SIGILL |
| ARM / A32 | **`udf #0`** | `e7f000f0` | `f0 00 f0 e7` | SIGILL |
| AArch64 | `udf #0` (`.inst 0`) | `00000000` | `00 00 00 00` | SIGILL |

`.inst 0` is **not** an ARM trap: on A32 that word decodes as `andeq r0, r0, r0`,
a conditional no-op. A measured A32 `.inst 0` snippet still exited with SIGILL,
but only by falling through into whatever followed — the control would have
reported success while proving nothing. AArch64 `brk #0` (`d4200000`, memory
`00 00 20 d4`, SIGTRAP) is a verified alternative; `udf #0` is preferred because
it keeps all four profiles on SIGILL.

Each case asserts **both** the wait signal and the recovered `validated/running`
phase.

### 16.4 Record-state and injection coverage

- One case per `(record_state, status)` cell of the §15.2 step-4 matrix.
- Both raw pending patterns, plus **malformed near-misses for each field step 2
  bypasses** — corrupt `status`, `case_id`, `expected[]`, `n_values`, actuals and
  diagnostics — all of which must fall through to step 3 and fail there.
- Canonical bootstrap with a zero subcode (all four rows of the §15.2 table).
- **Synthetic result-record cases** for the terminal `fault` schema no user-mode
  helper can produce, plus **records interrupted on both sides of the step-14
  `n_values` store**.
- **Injected helper faults *and hangs* before entry and after the `returned`
  commit**, proving those are not attributed to guest code, and that only a
  genuinely running guest can satisfy the guest-fault and guest-timeout controls.
- A fault injected **inside the residual capture-to-commit window** (§4.1),
  asserting the *documented conservative* outcome — reported as a guest `fault` —
  so the known limitation cannot regress silently.
- A late failure after the guest returns; a timeout mid-update; an unexpected
  exit code; and a completed record.

### 16.5 Cache synchronization

A behavioral smoke per profile (§12), plus the induced known-wrong encoding of
M1.6.

## 17. Change control

Anything in §8, §9, §10, §11, §13 or §15 is the wire or the state machine:
changing it is **ABI v2**. Adding a *new* subcode to an existing class in §14.2 is
permitted within v1 **only if** it describes a condition that was previously
rejected with a different subcode's meaning left intact, and it arrives with its
reachability test. Removing or renumbering a subcode is v2.
