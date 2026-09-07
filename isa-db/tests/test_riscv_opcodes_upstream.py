"""Exhaustive cross-validation against riscv-opcodes' own create_inst_dict.

Where test_riscv_opcodes_reader.py spot-checks a handful of mnemonics
(sh1add, sh1add.uw, nop, clmul/clmulh) against known-good values written
into the test file by hand, this asserts, for *every* record this adapter
emits across the whole vendored extensions/ tree, that its fixed_bits
mask/value agree exactly with the project's own upstream producer.
"""

from __future__ import annotations

import unittest
from pathlib import Path

from adapters.riscv_opcodes import reader, upstream
from normalize.schema_check import check_source_record

REPO_ROOT = Path(__file__).resolve().parents[2]
EXTENSIONS_DIR = REPO_ROOT / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream" / "extensions"


def _upstream_key(native_name: str) -> str:
    return native_name.replace(".", "_")


class TestMaskValueMatchesUpstreamForEveryInstruction(unittest.TestCase):
    def test_every_extension_file(self):
        mismatches = []
        schema_violations = []
        checked = 0
        for path in reader._list_extension_files(EXTENSIONS_DIR):
            records = reader.source_records_for_file(path, "riscv-opcodes@test")
            upstream_dict = upstream.create_inst_dict_for_file(path.name)
            for rec in records:
                violations = check_source_record(rec)
                if violations:
                    schema_violations.append((rec["record_id"], violations))

                enc = rec["encoding"]
                if enc["kind"] != "fixed_bits":
                    continue

                key = _upstream_key(rec["native_name"])
                entry = upstream_dict.get(key) or upstream_dict.get(f"{key}_pseudo")
                if entry is None:
                    mismatches.append((rec["record_id"], "not found in upstream dict"))
                    continue

                checked += 1
                if enc["mask"] != entry["mask"] or enc["value"] != entry["match"]:
                    mismatches.append(
                        (
                            rec["record_id"],
                            f"ours mask={enc['mask']} value={enc['value']} "
                            f"vs upstream mask={entry['mask']} match={entry['match']}",
                        )
                    )

        self.assertEqual(schema_violations, [])
        self.assertEqual(mismatches, [])
        self.assertGreater(checked, 1000, "sanity check: this should have run across the whole corpus")


if __name__ == "__main__":
    unittest.main()
