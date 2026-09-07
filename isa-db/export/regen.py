"""Regenerate the checked-in export/ JSONL files.

Run from isa-db/: `python3 -m export.regen`
Run from the repo root: `python3 -m export.regen` with `-t isa-db` (see
isa-db/README.md's test-invocation convention for the `-t` flag).
"""

from __future__ import annotations

from export.writer import regenerate


def main() -> None:
    for path in regenerate():
        print(path)


if __name__ == "__main__":
    main()
