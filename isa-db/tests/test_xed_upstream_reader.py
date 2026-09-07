"""Regression tests for adapters/xed/upstream_reader.py - the mapping from
XED's resolved inst_t records into SourceRecords with the new x86_encoding
shape.
"""

from __future__ import annotations

import unittest

from adapters.xed import upstream_reader
from normalize.schema_check import check_source_record

SNAPSHOT = "xed@test"


def _records_by_native_name(native_name: str) -> list[dict]:
    return [r for r in upstream_reader.source_records(SNAPSHOT) if r["native_name"] == native_name]


class TestAaa(unittest.TestCase):
    """AAA is a lone 0x37 byte, real-mode/32-bit only (mode_restriction
    'not64') - same ground truth test_xed_upstream.py's wrapper-level test
    already established directly against the inst_t object."""

    def setUp(self):
        (self.rec,) = _records_by_native_name("AAA")

    def test_schema_shaped(self):
        self.assertEqual(check_source_record(self.rec), [])

    def test_record_id_has_zero_occurrence_index(self):
        self.assertEqual(self.rec["record_id"], "xed:i86:AAA:0")

    def test_encoding(self):
        enc = self.rec["encoding"]
        self.assertEqual(enc["kind"], "x86_encoding")
        self.assertEqual(enc["space"], "legacy")
        self.assertEqual(enc["opcode_map"], 0)
        self.assertEqual(enc["opcode"], "0x37")
        # implicit AL/AH operands (SUPPRESSED - not written in assembly
        # syntax, but part of the instruction's real register footprint)
        self.assertEqual(
            [(op["name"], op["type"], op["bits"], op["visibility"]) for op in enc["operands"]],
            [("REG0", "reg", "XED_REG_AL", "SUPPRESSED"), ("REG1", "reg", "XED_REG_AH", "SUPPRESSED")],
        )

    def test_not64_applicability_excludes_mode64(self):
        applicability = self.rec["applicability"]
        self.assertFalse(upstream_reader.applies_64(applicability))
        self.assertTrue(upstream_reader.applies_32(applicability))

    def test_provenance(self):
        self.assertEqual(self.rec["provenance"]["extension"], "BASE")
        self.assertEqual(self.rec["provenance"]["isa_set"], "I86")
        self.assertNotIn("group", self.rec["provenance"])


class TestAddGprvImmz(unittest.TestCase):
    def setUp(self):
        matches = [r for r in _records_by_native_name("ADD") if r["provenance"]["iform"] == "ADD_GPRv_IMMz"]
        self.assertEqual(len(matches), 1)
        self.rec = matches[0]

    def test_encoding(self):
        enc = self.rec["encoding"]
        self.assertEqual(enc["opcode"], "0x81")
        self.assertEqual(enc["opcode_map"], 0)
        op_names = [op["name"] for op in enc["operands"]]
        self.assertEqual(op_names, ["REG0", "IMM0"])
        reg0 = enc["operands"][0]
        self.assertEqual(reg0["type"], "nt_lookup_fn")
        self.assertIsNone(reg0["bits"])
        imm0 = enc["operands"][1]
        self.assertEqual(imm0["type"], "imm_const")
        self.assertEqual(imm0["oc2"], "z")

    def test_unconditional_applicability(self):
        self.assertEqual(self.rec["applicability"], {"kind": "all", "of": []})


class TestOccurrenceIndexDisambiguatesRepeatedIforms(unittest.TestCase):
    """FLDENV's MEMmem14 iform repeats across multiple mode_restriction
    variants (verified directly against upstream.records() while writing
    this adapter), so record_id needs the occurrence counter, not iform
    alone, to stay unique."""

    def test_fldenv_memmem14_has_more_than_one_record_with_distinct_ids(self):
        matches = [
            r for r in _records_by_native_name("FLDENV") if r["provenance"]["iform"] == "FLDENV_MEMmem14"
        ]
        self.assertGreater(len(matches), 1)
        ids = [r["record_id"] for r in matches]
        self.assertEqual(len(ids), len(set(ids)))
        self.assertEqual(sorted(ids), [f"xed:x87:FLDENV_MEMmem14:{i}" for i in range(len(ids))])


class TestAllRecordsAreSchemaShaped(unittest.TestCase):
    def test_x86_64_profile(self):
        records = upstream_reader.source_records_for_profile("x86_64", SNAPSHOT)
        self.assertGreater(len(records), 5000)
        violations = [(r["record_id"], check_source_record(r)) for r in records if check_source_record(r)]
        self.assertEqual(violations, [])


if __name__ == "__main__":
    unittest.main()
