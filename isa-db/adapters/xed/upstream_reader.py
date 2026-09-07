"""Maps XED's own resolved `inst_t` records (`adapters/xed/upstream.py`)
into `SourceRecord`s, using the lean `x86_encoding` shape (2026-09-05 user
sign-off).

This is **additive**, not a replacement for `adapters/xed/reader.py`'s
coarse block-scan adapter: `export/xed/*.jsonl` (and the `compat_entries`/
`compat_entries_for_profile` regression-tested against the checked-in
`asm/fixtures/isa-inventory` manifests) stay exactly as they were. The
reason is `provenance.group` (the `datafiles/` directory an instruction
lives under) - `asm/tools`'s `Isa_db_cross_validate` keys XED matching on
`(native_name.lower(), provenance.group)`, and that directory identity is
not recoverable from the resolved reader path: it was verified in this
session that neither `iclass` alone (566 of 1995 iclasses span multiple
directory groups, e.g. `nop` under `base`/`mpx`/`cet`/`cldemote`/`ibhf`) nor
`(iclass, EXTENSION)` (still 307 ambiguous pairs - APX-F's directory
duplicates many BASE-extension instructions under the same `EXTENSION`
value with new encodings) gives a well-defined join back to `group`. So
these records carry `extension`/`isa_set`/`category` (upstream's own
fields) in `provenance`, not `group` - a real, structural gap, not an
oversight; see `unresolved` on each record.
"""

from __future__ import annotations

from adapters.xed import upstream
from adapters.xed.reader import applies_32, applies_64
from normalize.model import SourceRecord, mode_equals, mode_not_equals, unconditional, x86_encoding

# v.mode_restriction values, per read_xed_db.py's _find_opcodes/_set_eosz:
# 'unspecified' (no MODE constraint), 'not64' (MODE!=2), or 0/1/2 (16/32/64-
# bit only). Maps directly onto the same mode-expression vocabulary
# adapters/xed/reader.py's applicability already uses, so applies_32/
# applies_64 (imported above) evaluate these unchanged.
_MODE_RESTRICTION_EXPR = {
    "unspecified": unconditional(),
    "not64": mode_not_equals("mode64"),
    0: mode_equals("mode16"),
    1: mode_equals("mode32"),
    2: mode_equals("mode64"),
}


def applicability_for(v) -> dict:
    try:
        return _MODE_RESTRICTION_EXPR[v.mode_restriction]
    except KeyError as exc:
        raise ValueError(f"unhandled mode_restriction {v.mode_restriction!r} for iform {v.iform}") from exc


def _operand_dict(op) -> dict:
    bits = list(op.bits) if isinstance(op.bits, list) else op.bits
    return {
        "name": op.name,
        "type": op.type,
        "bits": bits,
        "lookupfn_name": op.lookupfn_name,
        "rw": op.rw,
        "visibility": op.visibility,
        "oc2": op.oc2,
    }


def source_records(snapshot: str) -> list[dict]:
    """One record per resolved `inst_t` (i.e. per instruction *form*, not
    per `iclass`) - `iform` is XED's own disambiguated per-form name but is
    not globally unique on its own (1411 of 9001 distinct iforms across the
    vendored checkout repeat, e.g. `FLDENV_MEMmem14` three times for
    variants that collapse to the same iform string), so `record_id`
    appends this function's own 0-based occurrence counter per iform,
    computed over `upstream.records()`'s stable iteration order.
    """
    occurrence: dict[str, int] = {}
    records = []
    for v in upstream.records():
        n = occurrence.get(v.iform, 0)
        occurrence[v.iform] = n + 1
        rec = SourceRecord(
            record_id=f"xed:{v.isa_set.lower()}:{v.iform}:{n}",
            source="xed",
            kind="instruction-form",
            native_name=v.iclass,
            snapshot=snapshot,
            origin={
                # The resolved reader path aggregates every datafiles/ file
                # into one obj/dgen/all-dec-instructions.txt before parsing
                # (see adapters/xed/upstream.py) and does not preserve the
                # original file:line - unlike reader.py's coarse adapter,
                # which reads datafiles/ directly. See `unresolved` below.
                "path": "obj/dgen/all-dec-instructions.txt",
                "line": None,
            },
            encoding=x86_encoding(
                space=v.space,
                opcode_map=v.map,
                opcode=v.opcode,
                pattern=v.pattern,
                operands=[_operand_dict(op) for op in v.parsed_operands],
            ),
            applicability=applicability_for(v),
            provenance={
                "extension": v.extension,
                "isa_set": v.isa_set,
                "category": v.category,
                "iform": v.iform,
            },
            unresolved=[
                "no original datafiles/ file:line (see origin)",
                "no directory group - see this module's docstring",
                "no byte-level decode - see encoding.pattern",
            ],
        )
        records.append(rec.to_dict())
    return records


def source_records_for_profile(profile: str, snapshot: str) -> list[dict]:
    if profile not in ("x86_32", "x86_64"):
        raise ValueError(f"unsupported profile: {profile!r}")
    check = applies_32 if profile == "x86_32" else applies_64
    return [r for r in source_records(snapshot) if check(r["applicability"])]
