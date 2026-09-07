"""Minimal, stdlib-only structural check against source_record.schema.v1.json.

Not a JSON Schema engine: no `jsonschema` package was available in this
environment (see isa-db/README.md). This checks the schema file's top-level
`required` list plus the `encoding` tagged union's two branches - enough to
catch a record missing a required field or misshaping its encoding, not
full schema conformance (no pattern/enum/format checks).
"""

from __future__ import annotations

import json
from pathlib import Path

_SCHEMA_PATH = Path(__file__).resolve().parent.parent / "schema" / "source_record.schema.v1.json"


def _load_schema() -> dict:
    return json.loads(_SCHEMA_PATH.read_text(encoding="utf-8"))


def check_source_record(record: dict) -> list[str]:
    """Return a list of shape violations; empty means "structurally OK"."""
    schema = _load_schema()
    problems = []

    for key in schema["required"]:
        if key not in record:
            problems.append(f"missing required top-level key: {key!r}")

    encoding = record.get("encoding")
    if isinstance(encoding, dict):
        kind = encoding.get("kind")
        if kind == "fixed_bits":
            for key in ("width_bits", "mask", "value", "fields"):
                if key not in encoding:
                    problems.append(f"encoding.fixed_bits missing {key!r}")
        elif kind == "opaque":
            if "note" not in encoding:
                problems.append("encoding.opaque missing 'note'")
        elif kind == "x86_encoding":
            for key in ("space", "opcode_map", "opcode", "pattern", "operands"):
                if key not in encoding:
                    problems.append(f"encoding.x86_encoding missing {key!r}")
        else:
            problems.append(f"encoding.kind must be 'fixed_bits', 'opaque', or 'x86_encoding', got {kind!r}")
    else:
        problems.append("encoding must be an object")

    applicability = record.get("applicability")
    if not isinstance(applicability, dict) or "kind" not in applicability:
        problems.append("applicability must be an object with a 'kind'")

    for rel in record.get("relationships") or []:
        if "kind" not in rel or "target" not in rel:
            problems.append(f"relationship missing 'kind' or 'target': {rel!r}")

    return problems
