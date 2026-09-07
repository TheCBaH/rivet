"""riscv-opcodes extensions/ adapter: per-line source records plus a compat view.

Mirrors asm/tools/lib/isa_inventory_riscv.ml's selection policy exactly
(ratified only, `rv_`/`rv32_`/`rv64_`-prefixed files) so that
`compat_entries_for_profile` can be regression-tested directly against the
checked-in asm/fixtures/isa-inventory/{riscv32,riscv64} manifests. Source
records go further: `$import` and `$pseudo_op` lines become explicit typed
relationships (`imports` / `specializes`) instead of being folded into a
plain mnemonic ($import) or reduced to just their own name ($pseudo_op) -
those choices define its actual coverage boundary; they should be explicit
selection policy in a future database rather than disappear during
normalization.

`source_records_for_file` cross-checks every plain instruction's mask/value
against `upstream.create_inst_dict_for_file`, and uses that same upstream
dict as the actual encoding source for `$import`
and `$pseudo_op` lines, which this adapter's own line parser cannot resolve
on its own (an import has no local encoding at all to parse; a pseudo-op's
overload tokens are not the plain "hi..lo=val" syntax `parse_fields`
handles). `compat_entries`/`compat_entries_for_profile` are untouched by
any of this - they never needed an encoding in the first place.
"""

from __future__ import annotations

import re
from pathlib import Path

from adapters.riscv_opcodes import upstream
from normalize.model import SourceRecord, fixed_bits_encoding, opaque_encoding, unconditional

_FIELD_RE = re.compile(r"^(\d+)(?:\.\.(\d+))?=(0[xXbBoO][0-9a-fA-F]+|\d+)$")


def tokens(line: str) -> list[str]:
    parts: list[str] = []
    for chunk in line.split(" "):
        parts.extend(chunk.split("\t"))
    return [p for p in parts if p != ""]


def parse_fields(toks: list[str]) -> tuple[list[dict], list[str], int, int]:
    """Split trailing tokens into (bit-field specs, operand names, mask, value).

    Bit positions come straight from the source ("hi..lo=value" tokens), so
    mask/value are correct regardless of instruction width; the caller pairs
    this with `_instruction_width_bits` to label the encoding's width
    (16 for a compressed 'c' extension, 32 otherwise) rather than assuming
    32 outright. Values may be decimal, hex ("0x..."), or binary ("0b...",
    e.g. rv32_zknd's "29..25=0b10111") - `int(raw, 0)` matches upstream
    riscv-opcodes' own `int(entry, 0)` base-detection exactly.
    """
    fields = []
    operands = []
    mask = 0
    value = 0
    for tok in toks:
        m = _FIELD_RE.match(tok)
        if m is None:
            operands.append(tok)
            continue
        hi = int(m.group(1))
        lo = int(m.group(2)) if m.group(2) is not None else hi
        raw = m.group(3)
        v = int(raw, 0)
        width = hi - lo + 1
        field_mask = ((1 << width) - 1) << lo
        field_val = (v & ((1 << width) - 1)) << lo
        fields.append({"name": f"bits[{hi}:{lo}]", "lsb": lo, "width": width})
        mask |= field_mask
        value |= field_val
    return fields, operands, mask, value


def _list_extension_files(extensions_dir: Path) -> list[Path]:
    return sorted(
        p
        for p in extensions_dir.iterdir()
        if p.is_file() and p.name != "unratified"
    )


def _applies_to(profile: str, extension: str) -> bool:
    if profile == "riscv32":
        return extension.startswith("rv_") or extension.startswith("rv32_")
    if profile == "riscv64":
        return extension.startswith("rv_") or extension.startswith("rv64_")
    raise ValueError(f"unsupported profile: {profile!r}")


def _instruction_width_bits(extension: str) -> int:
    """16 for a compressed extension (its own 'c' name component, e.g.
    'rv_c', 'rv32_c_f', 'rv_c_zicfiss'), else 32.

    Checking components after splitting on '_', rather than a substring
    check for "_c", matters: a naive substring check either misses these
    (splitting removes the underscore, so "_c" never reappears as a
    substring - the bug this replaced) or, if fixed to search for a bare
    "c", would wrongly also match extensions like 'rv_zbkc' or 'rv_zbc'
    whose name merely contains the letter c.
    """
    return 16 if "c" in extension.split("_")[1:] else 32


def _resolve_variable_fields(names: list[str], arg_lut: dict[str, tuple[int, int]]) -> list[dict]:
    fields = []
    for name in names:
        msb, lsb = arg_lut[name]
        fields.append({"name": name, "lsb": lsb, "width": msb - lsb + 1})
    return fields


def _encoding_from_upstream(entry: dict, width_bits: int, arg_lut: dict[str, tuple[int, int]]) -> dict:
    mask = int(entry["mask"], 16)
    value = int(entry["match"], 16)
    fields = _resolve_variable_fields(entry["variable_fields"], arg_lut)
    return fixed_bits_encoding(width_bits, mask, value, fields)


def source_records_for_file(path: Path, snapshot: str) -> list[dict]:
    extension = path.name
    width_bits = _instruction_width_bits(extension)
    upstream_dict = upstream.create_inst_dict_for_file(extension)
    arg_lut = upstream.arg_lut()
    records = []
    for lineno, raw_line in enumerate(path.read_text(encoding="utf-8", errors="replace").split("\n"), start=1):
        trimmed = raw_line.strip()
        if trimmed == "" or trimmed.startswith("#"):
            continue
        toks = tokens(trimmed)
        if not toks:
            continue
        origin = {"path": f"extensions/{extension}", "line": lineno}

        if toks[0] == "$import":
            target_ext, _, target_name = toks[1].partition("::")
            unresolved = ["import target not resolved to a concrete record_id by this adapter"]
            entry = upstream_dict.get(target_name.replace(".", "_"))
            if entry is not None:
                encoding = _encoding_from_upstream(entry, width_bits, arg_lut)
            else:
                encoding = opaque_encoding("import - no local encoding, see relationships")
                unresolved.append("upstream create_inst_dict did not resolve this import to a concrete encoding")
            rec = SourceRecord(
                record_id=f"riscv-opcodes:{extension}:{target_name}@L{lineno}",
                source="riscv_opcodes",
                kind="import",
                native_name=target_name,
                snapshot=snapshot,
                origin=origin,
                encoding=encoding,
                applicability=unconditional(),
                relationships=[{"kind": "imports", "target": f"riscv-opcodes:{target_ext}:{target_name}"}],
                provenance={"extension": extension},
                unresolved=unresolved,
            )
            records.append(rec.to_dict())
            continue

        if toks[0] == "$pseudo_op":
            if len(toks) < 3:
                raise ValueError(f"{path}:{lineno}: malformed $pseudo_op line: {raw_line!r}")
            base_ref, mnemonic = toks[1], toks[2]
            base_ext, _, base_name = base_ref.partition("::")
            upstream_key = mnemonic.replace(".", "_")
            entry = upstream_dict.get(upstream_key) or upstream_dict.get(f"{upstream_key}_pseudo")
            if entry is not None:
                encoding = _encoding_from_upstream(entry, width_bits, arg_lut)
                operands = entry["variable_fields"]
            else:
                fields, operands, mask, value = parse_fields(toks[3:])
                encoding = fixed_bits_encoding(width_bits, mask, value, fields)
            rec = SourceRecord(
                record_id=f"riscv-opcodes:{extension}:{mnemonic}@L{lineno}",
                source="riscv_opcodes",
                kind="pseudo-op",
                native_name=mnemonic,
                snapshot=snapshot,
                origin=origin,
                encoding=encoding,
                applicability=unconditional(),
                relationships=[{"kind": "specializes", "target": f"riscv-opcodes:{base_ext}:{base_name}"}],
                provenance={"extension": extension, "operands": operands},
                unresolved=["specializes-target not resolved to a concrete record_id by this adapter"],
            )
            records.append(rec.to_dict())
            continue

        mnemonic = toks[0]
        fields, operands, mask, value = parse_fields(toks[1:])
        entry = upstream_dict.get(mnemonic.replace(".", "_"))
        if entry is not None:
            fields = fields + _resolve_variable_fields(entry["variable_fields"], arg_lut)
        rec = SourceRecord(
            record_id=f"riscv-opcodes:{extension}:{mnemonic}@L{lineno}",
            source="riscv_opcodes",
            kind="instruction-form",
            native_name=mnemonic,
            snapshot=snapshot,
            origin=origin,
            encoding=fixed_bits_encoding(width_bits, mask, value, fields),
            applicability=unconditional(),
            provenance={"extension": extension, "operands": operands},
            unresolved=[],
        )
        records.append(rec.to_dict())
    return records


def source_records(extensions_dir: Path, snapshot: str) -> list[dict]:
    records = []
    for f in _list_extension_files(extensions_dir):
        records.extend(source_records_for_file(f, snapshot))
    return records


def source_records_for_profile(extensions_dir: Path, profile: str, snapshot: str) -> list[dict]:
    """Like `source_records`, restricted to files `_applies_to` selects for
    `profile` - the same file-level selection `compat_entries_for_profile`
    already uses, so the two stay consistent by construction.
    """
    records = []
    for f in _list_extension_files(extensions_dir):
        if not _applies_to(profile, f.name):
            continue
        records.extend(source_records_for_file(f, snapshot))
    return records


def compat_entries_for_profile(extensions_dir: Path, profile: str) -> set[tuple[str, str]]:
    entries: set[tuple[str, str]] = set()
    for f in _list_extension_files(extensions_dir):
        if not _applies_to(profile, f.name):
            continue
        for raw_line in f.read_text(encoding="utf-8", errors="replace").split("\n"):
            trimmed = raw_line.strip()
            if trimmed == "" or trimmed.startswith("#"):
                continue
            toks = tokens(trimmed)
            if not toks:
                continue
            if toks[0] == "$import":
                continue
            if toks[0] == "$pseudo_op":
                if len(toks) < 3:
                    raise ValueError(f"{f}: malformed $pseudo_op line: {raw_line!r}")
                mnemonic = toks[2]
            else:
                mnemonic = toks[0]
            entries.add((mnemonic, f.name))
    return entries
