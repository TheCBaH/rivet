"""JSON Lines export: one file per (source, profile), e.g.
`isa-db/export/riscv_opcodes/riscv64.jsonl`, `isa-db/export/xed/x86_64.jsonl`.

Checked into git (by explicit user decision,
2026-09-05), mirroring today's asm/fixtures/isa-inventory/*
manifest.txt/summary.txt convention - works fully offline and is diffable,
at the cost of a diff on every upstream bump. Regenerate with
`python3 -m export.regen` from the isa-db/ directory (or `-t isa-db` from
the repo root; see isa-db/README.md).

Each line is one `SourceRecord.to_dict()` (from normalize/model.py),
serialized with `json.dumps(..., sort_keys=True)` for byte-stable diffs,
one line per record, sorted by `record_id` so the file's own line order is
also stable across re-runs regardless of the adapter's internal iteration
order.

Also writes one auxiliary reference file per source that has one -
`export/riscv_opcodes/arg-lut.jsonl` - which
isn't a per-instruction source record at all (no `record_id`), so it is
sorted by its own `name` field instead.

Also writes one "resolved-path" file per (source, profile) that has one -
currently just `export/xed_resolved/{x86_32,x86_64}.jsonl`, from
`adapters/xed/upstream_reader.py`'s stronger `gen_setup`/`read_xed_db`-based
reader. Additive, not a replacement for `export/xed/*.jsonl` above (still
the coarse block-scan adapter, still what `Isa_db_cross_validate` reads):
the resolved path can't recover `provenance.group`, which that OCaml check
keys XED matching on.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import subprocess

from adapters.riscv_opcodes import reader as riscv_opcodes_reader
from adapters.riscv_opcodes import reference as riscv_opcodes_reference
from adapters.xed import reader as xed_reader
from adapters.xed import upstream_reader as xed_upstream_reader

REPO_ROOT = Path(__file__).resolve().parents[2]
LOCK_PATH = REPO_ROOT / "isa-db" / "sources.lock.json"
EXPORT_DIR = Path(__file__).resolve().parent

PROFILES: dict[str, list[str]] = {
    "xed": ["x86_32", "x86_64"],
    "riscv_opcodes": ["riscv32", "riscv64"],
}


def load_lock() -> dict:
    return json.loads(LOCK_PATH.read_text(encoding="utf-8"))


def snapshot_for(source: str, lock: dict) -> str:
    return f"{source}@{lock['sources'][source]['commit']}"


_ADAPTER_SUBDIR = {
    "xed": "datafiles",
    "riscv_opcodes": "extensions",
}


def _source_dir(source: str, lock: dict) -> Path:
    return REPO_ROOT / lock["sources"][source]["path"] / _ADAPTER_SUBDIR[source]


def records_for(source: str, profile: str, lock: dict) -> list[dict]:
    snapshot = snapshot_for(source, lock)
    source_dir = _source_dir(source, lock)
    if source == "xed":
        return xed_reader.source_records_for_profile(source_dir, profile, snapshot)
    if source == "riscv_opcodes":
        return riscv_opcodes_reader.source_records_for_profile(source_dir, profile, snapshot)
    raise ValueError(f"unknown source: {source!r}")


# Sources with an auxiliary reference export beyond their per-profile source
# records - see reference_records_for.
REFERENCE_SOURCES: list[str] = ["riscv_opcodes"]

# Sources with an additional resolved-path export beyond their per-profile
# source records - see resolved_records_for. Written to
# export/<source>_resolved/<profile>.jsonl, deliberately not folded into
# PROFILES/records_for above (see this module's docstring).
RESOLVED_PROFILES: dict[str, list[str]] = {"xed": ["x86_32", "x86_64"]}

CAPTURE_MANIFEST = EXPORT_DIR / "capture-manifest.v1.json"
CAPTURE_MANIFEST_SCHEMA = "isa-db-capture-manifest-v1"


def _git(path: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(path), *args], text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
    )
    if result.returncode != 0:
        raise ValueError(f"cannot inspect pinned source {path}: {result.stderr.strip()}")
    return result.stdout.strip()


def _verify_checkout(name: str, path: Path, expected_commit: str) -> None:
    actual = _git(path, "rev-parse", "HEAD")
    if actual != expected_commit:
        raise ValueError(f"{name} checkout is {actual}, but sources.lock.json pins {expected_commit}")
    dirty = _git(path, "status", "--porcelain", "--untracked-files=no")
    if dirty:
        raise ValueError(f"{name} checkout is dirty; refuse to label a development tree as pinned capture")


def verify_source_inputs(lock: dict) -> None:
    """Refuse a release capture when its named source/build dependency is not
    exactly the clean lockfile checkout.  This is intentionally producer-side
    and stays outside the portable assembler test path."""
    for source, spec in lock["sources"].items():
        _verify_checkout(source, REPO_ROOT / spec["path"], spec["commit"])
        for dependency, dep_spec in spec.get("build-dependencies", {}).items():
            _verify_checkout(f"{source}/{dependency}", REPO_ROOT / dep_spec["path"], dep_spec["commit"])


def _selected_input_files(source: str, lock: dict) -> list[Path]:
    root = REPO_ROOT / lock["sources"][source]["path"]
    if source == "xed":
        return sorted((root / "datafiles").rglob("*.txt"))
    if source == "riscv_opcodes":
        return sorted(
            [p for p in (root / "extensions").iterdir() if p.is_file()]
            + [root / "arg_lut.csv"]
        )
    raise ValueError(f"unknown source: {source!r}")


def _tree_digest(root: Path, files: list[Path]) -> tuple[str, int]:
    digest = hashlib.sha256()
    for path in files:
        digest.update(path.relative_to(root).as_posix().encode("utf-8"))
        digest.update(b"\0")
        digest.update(hashlib.sha256(path.read_bytes()).digest())
    return digest.hexdigest(), len(files)


def _producer_revision() -> str:
    """Content identity avoids tying a checked-in capture to a future git
    commit made after regeneration while still identifying the exact producer
    sources that formed it."""
    files = [
        REPO_ROOT / "isa-db" / "export" / "writer.py",
        REPO_ROOT / "isa-db" / "adapters" / "riscv_opcodes" / "reader.py",
        REPO_ROOT / "isa-db" / "adapters" / "xed" / "reader.py",
        REPO_ROOT / "isa-db" / "adapters" / "xed" / "upstream_reader.py",
        REPO_ROOT / "isa-db" / "normalize" / "model.py",
    ]
    digest, _ = _tree_digest(REPO_ROOT, files)
    return f"sha256:{digest}"


def capture_manifest(lock: dict) -> dict:
    sources = {}
    for source, spec in sorted(lock["sources"].items()):
        root = REPO_ROOT / spec["path"]
        digest, count = _tree_digest(root, _selected_input_files(source, lock))
        sources[source] = {
            "commit": spec["commit"],
            "selected-input-file-count": count,
            "selected-input-sha256": digest,
            "build-dependencies": {
                name: dep["commit"] for name, dep in sorted(spec.get("build-dependencies", {}).items())
            },
        }
    return {
        "schema": CAPTURE_MANIFEST_SCHEMA,
        "source-record-schema": "source_record.schema.v1.json",
        "producer-revision": _producer_revision(),
        "dirty-source-policy": "refuse",
        "sources": sources,
    }


def resolved_records_for(source: str, profile: str, lock: dict) -> list[dict]:
    snapshot = snapshot_for(source, lock)
    if source == "xed":
        return xed_upstream_reader.source_records_for_profile(profile, snapshot)
    raise ValueError(f"no resolved-path export defined for source: {source!r}")


def reference_records_for(source: str, lock: dict) -> list[dict]:
    if source == "riscv_opcodes":
        snapshot = snapshot_for(source, lock)
        extensions_dir = _source_dir(source, lock)
        return [
            {"source": source, "snapshot": snapshot, **entry}
            for entry in riscv_opcodes_reference.operand_vocab(extensions_dir)
        ]
    raise ValueError(f"no reference export defined for source: {source!r}")


def write_jsonl(records: list[dict], path: Path, *, key=lambda r: r["record_id"]) -> None:
    ordered = sorted(records, key=key)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        for rec in ordered:
            f.write(json.dumps(rec, sort_keys=True))
            f.write("\n")


def regenerate(out_dir: Path = EXPORT_DIR) -> list[Path]:
    lock = load_lock()
    verify_source_inputs(lock)
    written = []
    for source, profiles in PROFILES.items():
        for profile in profiles:
            path = out_dir / source / f"{profile}.jsonl"
            write_jsonl(records_for(source, profile, lock), path)
            written.append(path)
    for source in REFERENCE_SOURCES:
        path = out_dir / source / "arg-lut.jsonl"
        write_jsonl(reference_records_for(source, lock), path, key=lambda r: r["name"])
        written.append(path)
    for source, profiles in RESOLVED_PROFILES.items():
        for profile in profiles:
            path = out_dir / f"{source}_resolved" / f"{profile}.jsonl"
            write_jsonl(resolved_records_for(source, profile, lock), path)
            written.append(path)
    manifest_path = out_dir / CAPTURE_MANIFEST.name
    manifest_path.write_text(json.dumps(capture_manifest(lock), indent=2, sort_keys=True) + "\n", encoding="utf-8")
    written.append(manifest_path)
    return written
