"""Regression tests for export/writer.py.

Cross-validates the exported JSONL against the same manifest fixtures the
adapter-level compat views are already tested against, so this doesn't just
re-test source_records_for_profile in isolation - it proves the exported
files agree with the checked-in asm/fixtures/isa-inventory manifests too.
"""

from __future__ import annotations

import json
import unittest

from adapters.riscv_opcodes import reader as riscv_opcodes_reader
from adapters.xed import reader as xed_reader
from export.writer import (
    EXPORT_DIR,
    PROFILES,
    REFERENCE_SOURCES,
    RESOLVED_PROFILES,
    load_lock,
    records_for,
    reference_records_for,
    resolved_records_for,
    write_jsonl,
)
from normalize.schema_check import check_source_record
from tests.manifest_fixture import read_manifest_pairs


class TestRecordsForProfile(unittest.TestCase):
    def test_riscv64_matches_manifest_pairs(self):
        """compat_entries_for_profile (and the manifest) skip `$import` lines
        outright - a source record's own "imports" relationship is the
        richer, explicit replacement (see reader.py's module docstring), so
        this compares only instruction-form/pseudo-op records, not the full
        exported set."""
        lock = load_lock()
        records = records_for("riscv_opcodes", "riscv64", lock)
        record_pairs = {
            (r["native_name"], r["provenance"]["extension"])
            for r in records
            if r["kind"] != "import"
        }
        self.assertEqual(record_pairs, riscv_opcodes_reader.compat_entries_for_profile(
            EXPORT_DIR.parent.parent / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream" / "extensions",
            "riscv64",
        ))
        self.assertEqual(record_pairs, read_manifest_pairs("riscv64"))

    def test_x86_64_matches_manifest_pairs(self):
        lock = load_lock()
        records = records_for("xed", "x86_64", lock)
        record_pairs = {(r["native_name"].lower(), r["provenance"]["group"]) for r in records}
        self.assertEqual(record_pairs, xed_reader.compat_entries_for_profile(
            EXPORT_DIR.parent.parent / "asm" / "vendor" / "isa-data" / "xed" / "upstream" / "datafiles",
            "x86_64",
        ))
        self.assertEqual(record_pairs, read_manifest_pairs("x86_64"))

    def test_all_profiles_schema_shaped(self):
        lock = load_lock()
        for source, profiles in PROFILES.items():
            for profile in profiles:
                for rec in records_for(source, profile, lock):
                    self.assertEqual(check_source_record(rec), [], f"{source}/{profile}: {rec['record_id']}")

    def test_resolved_profiles_schema_shaped(self):
        lock = load_lock()
        for source, profiles in RESOLVED_PROFILES.items():
            for profile in profiles:
                for rec in resolved_records_for(source, profile, lock):
                    self.assertEqual(
                        check_source_record(rec), [], f"{source}_resolved/{profile}: {rec['record_id']}"
                    )


class TestWriteJsonl(unittest.TestCase):
    def test_sorted_by_record_id_and_round_trips(self):
        records = [
            {"record_id": "b", "x": 1},
            {"record_id": "a", "x": 2},
        ]
        path = EXPORT_DIR / "_test_scratch.jsonl"
        try:
            write_jsonl(records, path)
            lines = path.read_text(encoding="utf-8").splitlines()
            self.assertEqual([json.loads(l)["record_id"] for l in lines], ["a", "b"])
        finally:
            path.unlink(missing_ok=True)


class TestCheckedInExportUpToDate(unittest.TestCase):
    """Pins that the checked-in export/*.jsonl files are exactly what
    `regenerate()` produces right now - a stale checked-in file (adapter
    changed, export/ not regenerated) fails this instead of silently
    diverging, matching tools-isa-inventory-diff's checked-in-output
    contract on the OCaml side.
    """

    def test_checked_in_files_match_regeneration(self):
        lock = load_lock()
        for source, profiles in PROFILES.items():
            for profile in profiles:
                path = EXPORT_DIR / source / f"{profile}.jsonl"
                expected = "".join(
                    json.dumps(r, sort_keys=True) + "\n"
                    for r in sorted(records_for(source, profile, lock), key=lambda r: r["record_id"])
                )
                self.assertEqual(
                    path.read_text(encoding="utf-8"),
                    expected,
                    f"{path} is stale - rerun `python3 -m export.regen`",
                )
        for source in REFERENCE_SOURCES:
            path = EXPORT_DIR / source / "arg-lut.jsonl"
            expected = "".join(
                json.dumps(r, sort_keys=True) + "\n"
                for r in sorted(reference_records_for(source, lock), key=lambda r: r["name"])
            )
            self.assertEqual(
                path.read_text(encoding="utf-8"),
                expected,
                f"{path} is stale - rerun `python3 -m export.regen`",
            )
        for source, profiles in RESOLVED_PROFILES.items():
            for profile in profiles:
                path = EXPORT_DIR / f"{source}_resolved" / f"{profile}.jsonl"
                expected = "".join(
                    json.dumps(r, sort_keys=True) + "\n"
                    for r in sorted(resolved_records_for(source, profile, lock), key=lambda r: r["record_id"])
                )
                self.assertEqual(
                    path.read_text(encoding="utf-8"),
                    expected,
                    f"{path} is stale - rerun `python3 -m export.regen`",
                )
