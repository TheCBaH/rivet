import unittest
from pathlib import Path

from adapters.xed import reader
from normalize.model import eval_mode_expr
from normalize.schema_check import check_source_record
from tests.manifest_fixture import read_manifest_pairs

REPO_ROOT = Path(__file__).resolve().parents[2]
DATAFILES_DIR = REPO_ROOT / "asm" / "vendor" / "isa-data" / "xed" / "upstream" / "datafiles"


def _records_by_native_name(name: str) -> list[dict]:
    snapshot = "xed@test"
    return [
        r
        for r in reader.source_records(DATAFILES_DIR, snapshot)
        if r["native_name"] == name
    ]


class TestFieldParsing(unittest.TestCase):
    def test_field_value_requires_word_boundary(self):
        self.assertEqual(reader.field_value("ICLASS    : PUSHA", "ICLASS"), "PUSHA")
        self.assertIsNone(reader.field_value("ICLASSX   : PUSHA", "ICLASS"))
        self.assertIsNone(reader.field_value("random text", "ICLASS"))

    def test_tokens_splits_space_and_tab(self):
        self.assertEqual(reader.tokens("0x60  mode16\tno66_prefix"), ["0x60", "mode16", "no66_prefix"])


class TestPusha(unittest.TestCase):
    """PUSHA has only mode16/mode32 PATTERN lines - no mode64, no not64 - so
    it must be excluded from x86_64 and included in x86_32 ("mode is a
    constraint, not absence of a token" - a regression this repo already
    fixed for it)."""

    def setUp(self):
        self.records = _records_by_native_name("PUSHA")

    def test_single_block_group_base(self):
        self.assertEqual(len(self.records), 1)
        self.assertEqual(self.records[0]["provenance"]["group"], "base")
        self.assertEqual(self.records[0]["provenance"]["isa_extension"], "BASE")
        self.assertIn("patterns", self.records[0]["provenance"]["raw"])

    def test_applicability_excludes_mode64(self):
        expr = self.records[0]["applicability"]
        self.assertTrue(eval_mode_expr(expr, "mode16"))
        self.assertTrue(eval_mode_expr(expr, "mode32"))
        self.assertFalse(eval_mode_expr(expr, "mode64"))

    def test_compat_view_agrees(self):
        merged = reader.compat_entries(DATAFILES_DIR)
        a32, a64 = merged[("pusha", "base")]
        self.assertTrue(a32)
        self.assertFalse(a64)

    def test_record_is_schema_shaped(self):
        self.assertEqual(check_source_record(self.records[0]), [])


class TestMovqMultiBlockMultiExtension(unittest.TestCase):
    """MOVQ is defined across five separate root-file blocks (four EXTENSION:
    SSE2, one EXTENSION: MMX) - a real example of source grouping not being
    an ISA feature: all five share the directory-derived group "base", but
    their own EXTENSION field disagrees with that group and with each
    other."""

    def setUp(self):
        self.records = _records_by_native_name("MOVQ")

    def test_five_blocks_two_isa_extensions(self):
        self.assertEqual(len(self.records), 5)
        groups = {r["provenance"]["group"] for r in self.records}
        self.assertEqual(groups, {"base"})
        isa_extensions = {r["provenance"]["isa_extension"] for r in self.records}
        self.assertEqual(isa_extensions, {"SSE2", "MMX"})

    def test_compat_view_merges_to_one_key_both_modes(self):
        merged = reader.compat_entries(DATAFILES_DIR)
        a32, a64 = merged[("movq", "base")]
        self.assertTrue(a32)
        self.assertTrue(a64)


class TestModeRestrictionVocabulary(unittest.TestCase):
    def test_jrcxz_longmode_is_not_x86_32_applicable(self):
        records = _records_by_native_name("JRCXZ")
        self.assertEqual(len(records), 1)
        expr = records[0]["applicability"]
        self.assertFalse(eval_mode_expr(expr, "mode16"))
        self.assertFalse(eval_mode_expr(expr, "mode32"))
        self.assertTrue(eval_mode_expr(expr, "mode64"))
        self.assertNotIn(("jrcxz", "base"), reader.compat_entries_for_profile(DATAFILES_DIR, "x86_32"))

    def test_unknown_mode_restriction_is_not_a_positive_match(self):
        block = reader.Block(iclass="FUTURE", patterns=["0x90 FORCE32()"])
        expr = reader.block_applicability(block)
        self.assertIsNone(eval_mode_expr(expr, "mode16"))
        self.assertIsNone(eval_mode_expr(expr, "mode32"))
        self.assertIsNone(eval_mode_expr(expr, "mode64"))
        self.assertFalse(reader.applies_32(expr))
        self.assertFalse(reader.applies_64(expr))


class TestCompatRegression(unittest.TestCase):
    """The isa-db compat view must exactly reproduce the checked-in
    asm/fixtures/isa-inventory manifests produced by
    asm/tools/lib/isa_inventory_xed.ml - this is this slice's Phase 2
    "regression check ... form counts" acceptance criterion."""

    def test_x86_32_matches_checked_in_manifest(self):
        got = reader.compat_entries_for_profile(DATAFILES_DIR, "x86_32")
        want = read_manifest_pairs("x86_32")
        self.assertEqual(got, want)

    def test_x86_64_matches_checked_in_manifest(self):
        got = reader.compat_entries_for_profile(DATAFILES_DIR, "x86_64")
        want = read_manifest_pairs("x86_64")
        self.assertEqual(got, want)


if __name__ == "__main__":
    unittest.main()
