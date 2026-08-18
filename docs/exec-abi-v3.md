# Execution ABI v3

ABI v3 is an additive successor to the frozen ABI v1 and to ABI v2. Unless this
document overrides a rule, every v1 field offset, size, validation stage, result
state, error class, and subcode retains its exact value and meaning; v2's RISC-V
profiles, cache-sync addition, and address windows are likewise unchanged and
orthogonal to what follows.

The manifest and result magic values end in byte `3`, and `abi_version` is 3.
Profile IDs retain their v1/v2 meanings — v3 was proven against IDs 1–4 (the
four legacy profiles) first, then against RISC-V (IDs 5–6, both XLEN
profiles): nothing here is profile-specific.

Where v1 said "additional observation sources require an ABI v2 bump" (§13),
that bump is this document.

## 1. What changes: a result vector instead of a single scalar

v1/v2 fix `n_expected = 1` — one configured observation, the return register.
v3 permits `n_expected` in the existing `1…8` range the field's width (`u16`)
and the result block's existing `expected[8]`/`values[8]` arrays already
accommodated, but never used above 1. Slot 0 is always the return register, per
v1 §13, and needs no descriptor of its own. Observations 1…`n_expected − 1` are
each described by a new manifest-side **observation descriptor**, read after the
guest returns.

## 2. Manifest wire format

**Bytes 0–119: unchanged from v1 §8's fixed header**, distinguished by
`magic` (ending `\x03`) and `abi_version = 3`. `n_expected` is now valid in the
full `1…8` range rather than forced to `1` — no width change, the field was
already a `u16`.

**Bytes `120 … 120 + 40×n_segments`: unchanged from v1 §8** — segment
descriptors, identical shape and packing rule.

**New — the observation-descriptor table**, placed at a **derived offset**
rather than a stored one, computed from fields the header already carries:

```text
obs_table_off = 120 + 40 × n_segments
obs_table_len = 16 × (n_expected − 1)
```

Present (nonempty) only when `n_expected > 1`. There are exactly
`n_expected − 1` descriptors — one per observation beyond slot 0 — each 16
bytes:

| Offset | Size | Field |
|---:|---:|---|
| 0 | 8 | `vaddr` u64 |
| 8 | 1 | `width` u8 — must be one of `{1, 2, 4, 8}` |
| 9 | 1 | `flags` u8 — bit 0: sign-extend (1) vs. zero-extend (0) to 64 bits; bits 1–7 reserved, zero |
| 10 | 6 | reserved, zero |

A derived rather than stored offset was chosen because v1's only reserved
header space is the 4 bytes at offset 52 (`docs/exec-abi-v1.md` §8) — too
small to hold a safe offset+length pair over a 1 MiB manifest range, and
reinterpreting those 4 bytes would change what they mean for v1/v2 readers of
the same byte range. Computing the table's position from `n_segments` and
`n_expected` avoids both problems: no header growth, no reserved-byte
reinterpretation, and a v3 reader with `n_expected = 1` sees a manifest that is
**byte-shape-identical to v1/v2** (only `magic`/`abi_version` differ) — the
common single-value case costs v3 nothing extra.

**Segment payload bytes** follow immediately after the observation-descriptor
table (or immediately after the segment-descriptor table, unchanged, when
`n_expected = 1`), using v1 §8's/§10.1's existing packing, alignment, and
zero-gap rule, with the base offset shifted by `obs_table_len`.

**Minimum valid v3 manifest size**: `120 + 40×n_segments + 16×(n_expected − 1)`
bytes, checked arithmetic — a direct generalization of v1's `120 + 40×n_segments`
stage-4 check (`docs/exec-abi-v1.md` §10.4).

## 3. Validation: a new stage 4b, before any segment is mapped

v1 §10's stage table validates the whole manifest before any segment is
mapped, so that pre-run failures have deterministic attribution (v1 §4.1,
§10). Containment of an observation's `[vaddr, vaddr+width)` range inside a
segment the same manifest declares is fully decidable from the manifest's own
descriptors — it needs no mapped memory — so it belongs in that same
pre-mapping sweep, not deferred to after the guest returns.

**Stage 4b**, inserted immediately after v1's stage 4 (`120 + 40×n_segments ≤
st_size`) and before stage 5 (payload-in-file bounds):

1. `obs_table_off + obs_table_len ≤ st_size`, checked arithmetic — the
   table-bound check, generalizing stage 4's own form.
2. Per descriptor, in descriptor-index order:
   - `width ∈ {1, 2, 4, 8}` — else subcode **43**, "bad observation width";
   - reserved bits (`flags` bits 1–7, and the 6 reserved bytes) zero — else the
     existing-style class-64 "reserved nonzero" treatment, subcode **44**;
   - `[vaddr, vaddr+width)` is contained entirely within **one** declared
     segment's `[address, address+init_len+zero_len)` range, checked for
     overflow — else subcode **45**, "observation address not contained in a
     declared segment";
   - that containing segment's descriptor has its **read** permission bit set
     (`perms` bit 0, v1 §8's segment-descriptor `perms` field) — else subcode
     **46**, "observation address not in a readable segment". The wire format
     already exposes R/W/X per segment; without this check a v3 manifest could
     declare an observation over a non-readable segment and rely on host/kernel
     permission behavior rather than the protocol's own contract.

Only the actual memory **load** — reading the live byte value at `vaddr` into
`values[i]`, applying `width` and the sign/zero-extend `flags` bit — happens
**after** the guest returns, using the descriptor validated at stage 4b. Stage
4b never dereferences guest memory; it only checks the manifest's own declared
geometry against itself and against the segment table already validated by
stage 4.

Stage 4b runs before geometry/collision/well-formedness (v1 §10.2's groups 2–4)
for the same reason stage 4 does: an observation table that does not even fit
in the file should not be interpreted as containing valid descriptors.

## 4. Result block: existing capacity, generalized interpretation

No wire-layout change. v1 §13's result block already reserves 64 bytes each for
`expected[8]` (offset 24) and `values[8]` (offset 88) — room for 8 slots was
always there, unused above index 0. v3 changes only the **interpretation**:

- `n_expected` (offset 16) may be `1…8`, matching the manifest's own
  `n_expected`.
- `n_values` (offset 18) must equal `n_expected` **exactly** once a `returned`
  terminal record is committed (`status ∈ {passed, failed}`) — generalizing v1
  §13's "1 only once passed/failed is committed in returned." In every
  non-`returned` state `n_values = 0`, unchanged.
- **Publication order** (v1 §5's atomicity discipline, generalized): every
  entry of `values[0..n_values-1]` is written before `n_values`/`status`
  commits. A reader that observes a committed `returned` record with
  `status ∈ {passed, failed}` observes a fully populated vector, never a
  partial one.
- Slot 0 remains the profile's 32-bit integer return register, sign-extended
  to 64 bits exactly as v1 §13 specifies. Slots 1…`n_expected − 1` hold the
  loaded observation values, sign- or zero-extended per each descriptor's
  `flags` bit, in the same order as the manifest's observation-descriptor
  table.
- **The passed/failed decision, generalized element-wise.** v1 §13 decides
  `status` by comparing the single `values[0]` against `expected[0]`. v3
  compares **every** slot: `status = passed` iff `values[i] = expected[i]`
  for **all** `i` in `0..n_expected − 1`; a mismatch in any single slot
  (including a mismatch confined to an observation slot with slot 0 correct)
  commits `status = failed`. This was left unstated in the first draft of
  this document; it is the direct generalization of v1's single-slot rule to
  parallel `expected[]`/`values[]` arrays that were already sized for up to
  8 entries, and it is what makes a v3 fixture's "wrong" case (e.g. a correct
  quotient with a corrupted remainder) actually observable as `failed`
  rather than silently reported `passed` because only slot 0 was checked.

## 5. Error classes and subcodes

v3 adds four subcodes to the existing **class 64 — manifest** (v1 §14.1),
continuing that class's numbering from v1's highest (42) — the same pattern
v2 used to add class 67 subcode 3 (`docs/exec-abi-v2.md`) rather than opening
a new class:

| Subcode | Stage | Condition |
|---:|---:|---|
| 43 | 4b | `width ∉ {1, 2, 4, 8}` |
| 44 | 4b | observation descriptor reserved bits nonzero |
| 45 | 4b | `[vaddr, vaddr+width)` not contained in any one declared segment |
| 46 | 4b | the containing segment is not readable |

No other class gains a subcode. v3 introduces no new failure mode outside the
observation table: a v3 manifest with `n_expected = 1` is validated exactly as
a v1/v2 one is, byte for byte.

## 6. Implementation status

Implementation status: **all six profiles proven end to end.**
Generated helpers (`test/oracle/abi_gen_main.exe --profile <name>
--abi-version 3`, `tools/asm-helpers.sh`'s `build_generated_one`) for x86_64,
x86_32, ARM and AArch64 all pass the complete conformance corpus under real
`qemu-x86_64`/`qemu-i386`/`qemu-arm`/`qemu-aarch64`, including all four new
subcodes above and a real multi-value `passed`/`failed` result, and each
profile's v1/v2 output is separately proven byte-for-byte identical to its
frozen `helpers/<profile>.s`. x86_32's own generator work caught two real
bugs before either reached a committed helper: a hand-written conformance
case clobbering the profile's checked callee-saved register (`%ebx`), and an
off-by-one in the observation-store index that silently overwrote slot 0;
ARM's and AArch64's generators, written with both lessons already in hand,
each passed their first real run clean.

RISC-V (both `riscv32` and `riscv64`) is also proven end to end, sharing one
validator source (`asm/helpers/riscv.c`) between XLEN profiles exactly as its
v2 path already does — v3 is `ABI_VERSION`-guarded logic added to that same
checked-in C file rather than a second file or a generated one, since C has
no equivalent need for the legacy profiles' frozen/generated split. Because
the helper runs the guest in-process (`riscv_run_guest` is a plain call, not
a fork), the observation load after return reads guest memory directly
through the same host pointer the pre-run validation used, and `m`/`seg[]`/
`nexp`/`obs_table_off` survive the guest call as ordinary C locals — the
compiler's own callee-saved discipline, respected by `riscv_run_guest`'s
save/restore stub, already gives this guarantee, so no analogue of the
legacy profiles' `.bss`-based "never trust a register across the call" state
is needed. RISC-V's own conformance run passed clean on the first real QEMU
run, catching no new bugs.

See Phase 5/6 for the fixture work that puts this protocol behind a real
committed fixture rather than only a hand-built manifest.
