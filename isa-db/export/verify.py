"""Check the pinned native-capture inputs without rewriting any artifact."""

from __future__ import annotations

import json

from export.writer import CAPTURE_MANIFEST, capture_manifest, load_lock, verify_source_inputs


def main() -> None:
    lock = load_lock()
    verify_source_inputs(lock)
    recorded = json.loads(CAPTURE_MANIFEST.read_text(encoding="utf-8"))
    expected = capture_manifest(lock)
    if recorded != expected:
        raise SystemExit(f"{CAPTURE_MANIFEST} is stale; rerun python3 -m export.regen")
    print("isa-db capture inputs: pinned, clean, and hash-matched")


if __name__ == "__main__":
    main()
