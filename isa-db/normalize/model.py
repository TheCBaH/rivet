"""Source-agnostic pieces of the isa-db source-record layer.

See isa-db/README.md for this slice's scope (XED + riscv-opcodes only).
Nothing here is specific to either adapter; adapter-specific parsing
lives under isa-db/adapters/<name>/reader.py.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


# ---------------------------------------------------------------------------
# Encodings: a tagged union, per schema/source_record.schema.v1.json.


def opaque_encoding(note: str) -> dict:
    return {"kind": "opaque", "note": note}


def fixed_bits_encoding(width_bits: int, mask: int, value: int, fields: list[dict]) -> dict:
    return {
        "kind": "fixed_bits",
        "width_bits": width_bits,
        "mask": hex(mask),
        "value": hex(value),
        "fields": fields,
    }


def x86_encoding(*, space: str, opcode_map: int, opcode: str, pattern: str, operands: list[dict]) -> dict:
    """x86 has no single fixed-width mask (unlike RISC-V) - per the
    X86InstrFormats.td discussion, this keeps XED's own
    resolved-but-still-textual facts instead of a byte-level
    decode: which encoding space (`legacy`/`vex`/`evex`/`xop`), the opcode
    map/byte XED itself classified from the pattern, the full resolved
    PATTERN token string (state-bits already macro-expanded, unlike the
    coarse block-scan adapter's raw block dump), and a resolved operand list -
    the "lean" option, by explicit user decision."""
    return {
        "kind": "x86_encoding",
        "space": space,
        "opcode_map": opcode_map,
        "opcode": opcode,
        "pattern": pattern,
        "operands": operands,
    }


# ---------------------------------------------------------------------------
# Applicability: a small typed expression tree, evaluated three-valued.
#
# This slice only needs x86 execution-mode constraints (from XED PATTERN
# tokens): "mode" nodes compared against one of "mode16"/"mode32"/"mode64".
# `unconditional()` represents an empty conjunction (always true) - the
# "no PATTERN line, or a PATTERN with no mode token" case.


def unconditional() -> dict:
    return {"kind": "all", "of": []}


def mode_equals(mode: str) -> dict:
    return {"kind": "mode", "equals": mode}


def mode_not_equals(mode: str) -> dict:
    return {"kind": "mode", "not_equals": mode}


def all_of(exprs: list[dict]) -> dict:
    return {"kind": "all", "of": exprs}


def any_of(exprs: list[dict]) -> dict:
    return {"kind": "any", "of": exprs}


def eval_mode_expr(expr: dict, mode: str) -> bool:
    """Evaluate an applicability expression against one concrete x86 mode.

    Three-valued in principle (per the design doc); this slice's inputs are
    always fully resolved mode tokens, so this evaluator only ever returns
    True/False, never "unknown" - there is nothing yet that would produce an
    unknown mode constraint from XED's PATTERN tokens.
    """
    kind = expr["kind"]
    if kind == "all":
        return all(eval_mode_expr(sub, mode) for sub in expr["of"])
    if kind == "any":
        return any(eval_mode_expr(sub, mode) for sub in expr["of"])
    if kind == "mode":
        if "equals" in expr:
            return mode == expr["equals"]
        if "not_equals" in expr:
            return mode != expr["not_equals"]
        raise ValueError(f"mode node with neither 'equals' nor 'not_equals': {expr!r}")
    raise ValueError(f"unknown applicability node kind: {kind!r}")


# ---------------------------------------------------------------------------
# The source record itself.

REQUIRED_KEYS = (
    "record_id",
    "source",
    "kind",
    "native_name",
    "snapshot",
    "origin",
    "encoding",
    "applicability",
    "relationships",
    "unresolved",
)


@dataclass
class SourceRecord:
    record_id: str
    source: str
    kind: str
    native_name: str
    snapshot: str
    origin: dict
    encoding: dict
    applicability: dict
    relationships: list[dict] = field(default_factory=list)
    unresolved: list[str] = field(default_factory=list)
    provenance: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict:
        d = {
            "record_id": self.record_id,
            "source": self.source,
            "kind": self.kind,
            "native_name": self.native_name,
            "snapshot": self.snapshot,
            "origin": self.origin,
            "encoding": self.encoding,
            "applicability": self.applicability,
            "relationships": self.relationships,
            "unresolved": self.unresolved,
        }
        if self.provenance:
            d["provenance"] = self.provenance
        return d
