import unittest
from pathlib import Path

from adapters.riscv_opcodes import reader
from normalize.schema_check import check_source_record
from tests.manifest_fixture import read_manifest_pairs

REPO_ROOT = Path(__file__).resolve().parents[2]
EXTENSIONS_DIR = REPO_ROOT / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream" / "extensions"


def _records_by_native_name(path: Path, name: str) -> list[dict]:
    return [r for r in reader.source_records_for_file(path, "riscv-opcodes@test") if r["native_name"] == name]


class TestSh1add(unittest.TestCase):
    """rv_zba's sh1add: cross-checked against both riscv-opcodes' own parser
    and QEMU's decodetree parser (mask 0xfe00707f, value 0x20002033)."""

    def test_fixed_bits_match_cross_source_ground_truth(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_zba", "sh1add")
        self.assertEqual(len(records), 1)
        enc = records[0]["encoding"]
        self.assertEqual(enc["kind"], "fixed_bits")
        self.assertEqual(enc["mask"], hex(0xFE00707F))
        self.assertEqual(enc["value"], hex(0x20002033))

    def test_record_is_schema_shaped(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_zba", "sh1add")
        self.assertEqual(check_source_record(records[0]), [])


class TestCompressedInstructionWidth(unittest.TestCase):
    """rv_c's c.addi4spn is a real 16-bit compressed encoding. A prior
    version of this adapter always declared width_bits=32 here (its
    "is this a compressed extension" check used a substring match that could
    never fire, since splitting on '_' removes the underscore it searched
    for) - this pins the fix: width_bits must reflect the actual 16-bit
    encoding for a 'c' extension component, and must not be fooled by an
    unrelated extension like rv_zbkc merely containing the letter c."""

    def test_c_addi4spn_is_16_bit(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_c", "c.addi4spn")
        self.assertEqual(len(records), 1)
        enc = records[0]["encoding"]
        self.assertEqual(enc["kind"], "fixed_bits")
        self.assertEqual(enc["width_bits"], 16)
        self.assertEqual(enc["mask"], hex(0xE003))
        self.assertEqual(enc["value"], hex(0x0))

    def test_non_compressed_extension_stays_32_bit(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_zba", "sh1add")
        self.assertEqual(records[0]["encoding"]["width_bits"], 32)

    def test_zbkc_is_not_mistaken_for_compressed(self):
        self.assertEqual(reader._instruction_width_bits("rv_zbkc"), 32)
        self.assertEqual(reader._instruction_width_bits("rv_c"), 16)
        self.assertEqual(reader._instruction_width_bits("rv32_c_f"), 16)


class TestBinaryLiteralFixedField(unittest.TestCase):
    """rv32_zknd's aes32dsmi writes its bs-selecting fixed field as a binary
    literal ("29..25=0b10111"), not hex/decimal. A prior version of
    `_FIELD_RE` only recognized "0x..." or plain decimal, so this token
    silently fell through to `operands` instead of contributing to
    mask/value - found by the exhaustive upstream cross-validation in
    test_riscv_opcodes_upstream.py, which caught the resulting mask/value
    mismatch against riscv-opcodes' own create_inst_dict."""

    def test_aes32dsmi_mask_value(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv32_zknd", "aes32dsmi")
        self.assertEqual(len(records), 1)
        enc = records[0]["encoding"]
        self.assertEqual(enc["kind"], "fixed_bits")
        self.assertEqual(enc["mask"], hex(0x3E00707F))
        self.assertEqual(enc["value"], hex(0x2E000033))


class TestSh1addUwIsRv64Only(unittest.TestCase):
    """sh1add.uw lives only in rv64_zba (design doc: "SH1ADD_UW additionally
    requires RV64"), so it must appear in the riscv64 compat view and not
    the riscv32 one."""

    def test_present_only_in_riscv64_profile(self):
        riscv64 = reader.compat_entries_for_profile(EXTENSIONS_DIR, "riscv64")
        riscv32 = reader.compat_entries_for_profile(EXTENSIONS_DIR, "riscv32")
        self.assertIn(("sh1add.uw", "rv64_zba"), riscv64)
        self.assertNotIn(("sh1add.uw", "rv64_zba"), riscv32)
        self.assertFalse(any(mnem == "sh1add.uw" for mnem, _ext in riscv32))


class TestNopPseudoOp(unittest.TestCase):
    """rv_i's nop is `$pseudo_op rv_i::addi nop ...` - a specialization of
    addi, not an independent instruction."""

    def test_specializes_addi(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_i", "nop")
        self.assertEqual(len(records), 1)
        rec = records[0]
        self.assertEqual(rec["kind"], "pseudo-op")
        self.assertEqual(rec["relationships"], [{"kind": "specializes", "target": "riscv-opcodes:rv_i:addi"}])

    def test_record_is_schema_shaped(self):
        records = _records_by_native_name(EXTENSIONS_DIR / "rv_i", "nop")
        self.assertEqual(check_source_record(records[0]), [])


class TestImportRelationship(unittest.TestCase):
    """rv_zbkc re-exposes rv_zbc's clmul/clmulh via $import, rather than
    redefining them - it must not silently gain its own new mnemonics."""

    def test_clmul_and_clmulh_import_from_rv_zbc(self):
        all_records = reader.source_records(EXTENSIONS_DIR, "riscv-opcodes@test")
        imports = [
            r
            for r in all_records
            if r["provenance"].get("extension") == "rv_zbkc" and r["kind"] == "import"
        ]
        self.assertEqual({r["native_name"] for r in imports}, {"clmul", "clmulh"})
        for r in imports:
            self.assertEqual(len(r["relationships"]), 1)
            self.assertTrue(r["relationships"][0]["target"].startswith("riscv-opcodes:rv_zbc:"))

    def test_import_does_not_appear_in_compat_view(self):
        # $import is intentionally excluded from the compat view (it would
        # otherwise double-count rv_zbc's own mnemonics under rv_zbkc too) -
        # matching isa_inventory_riscv.ml's own documented behavior.
        entries = reader.compat_entries_for_profile(EXTENSIONS_DIR, "riscv32")
        self.assertFalse(any(ext == "rv_zbkc" for _mnem, ext in entries))


class TestCompatRegression(unittest.TestCase):
    """Same acceptance criterion as the XED adapter's regression test: exact
    parity with the checked-in asm/fixtures/isa-inventory manifests."""

    def test_riscv32_matches_checked_in_manifest(self):
        got = reader.compat_entries_for_profile(EXTENSIONS_DIR, "riscv32")
        want = read_manifest_pairs("riscv32")
        self.assertEqual(got, want)

    def test_riscv64_matches_checked_in_manifest(self):
        got = reader.compat_entries_for_profile(EXTENSIONS_DIR, "riscv64")
        want = read_manifest_pairs("riscv64")
        self.assertEqual(got, want)


if __name__ == "__main__":
    unittest.main()
