"""Read a checked-in asm/fixtures/isa-inventory/<target>/manifest.txt as a
plain set of (mnemonic, extension) pairs, for regression-comparing an
isa-db adapter's compat view against the existing OCaml generator's output.

No manifest row in the four current fixtures uses Manifest.escape's
backslash escaping (verified with `grep -c '\\\\'` over all four files when
this was written), so this intentionally does not implement unescaping -
see isa-db/README.md's zero-dependency, minimal-scope note.
"""

from __future__ import annotations

from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]


def manifest_path(target: str) -> Path:
    return REPO_ROOT / "asm" / "fixtures" / "isa-inventory" / target / "manifest.txt"


def read_manifest_pairs(target: str) -> set[tuple[str, str]]:
    pairs = set()
    for line in manifest_path(target).read_text(encoding="utf-8").splitlines():
        if not line.startswith("mnemonic:"):
            continue
        fields = dict(part.split(":", 1) for part in line.split("\t"))
        pairs.add((fields["mnemonic"], fields["extension"]))
    return pairs
