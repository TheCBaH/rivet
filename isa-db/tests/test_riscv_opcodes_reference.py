"""Regression tests for adapters/riscv_opcodes/reference.py: the arg_lut
operand-vocabulary export.
"""

from __future__ import annotations

import csv
import unittest
from pathlib import Path

from adapters.riscv_opcodes import reference

REPO_ROOT = Path(__file__).resolve().parents[2]
UPSTREAM_DIR = REPO_ROOT / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream"
EXTENSIONS_DIR = UPSTREAM_DIR / "extensions"
ARG_LUT_CSV = UPSTREAM_DIR / "arg_lut.csv"


def _read_raw_csv() -> dict[str, tuple[int, int]]:
    """An independent, from-scratch re-parse of the raw CSV - deliberately
    not reusing anything from adapters/riscv_opcodes/ - used only to check
    that operand_vocab is a superset of the literal file with matching bit
    ranges, not to define expected behavior on its own (see reference.py's
    module docstring for why the raw file alone is not the full vocabulary).
    """
    with ARG_LUT_CSV.open(encoding="utf-8", newline="") as f:
        return {row[0]: (int(row[1]), int(row[2])) for row in csv.reader(f, skipinitialspace=True)}


class TestOperandVocab(unittest.TestCase):
    def test_sh1add_operands_match_known_bit_ranges(self):
        vocab = {e["name"]: e for e in reference.operand_vocab(EXTENSIONS_DIR)}
        self.assertEqual(vocab["rd"], {"name": "rd", "lsb": 7, "width": 5})
        self.assertEqual(vocab["rs1"], {"name": "rs1", "lsb": 15, "width": 5})
        self.assertEqual(vocab["rs2"], {"name": "rs2", "lsb": 20, "width": 5})

    def test_superset_of_raw_csv_with_matching_bit_ranges(self):
        vocab = {e["name"]: e for e in reference.operand_vocab(EXTENSIONS_DIR)}
        raw = _read_raw_csv()
        self.assertGreater(len(raw), 0)
        for name, (msb, lsb) in raw.items():
            self.assertIn(name, vocab, f"{name!r} from arg_lut.csv missing from operand_vocab")
            self.assertEqual(vocab[name], {"name": name, "lsb": lsb, "width": msb - lsb + 1})

    def test_includes_names_hardcoded_beyond_the_raw_csv(self):
        """constants.py registers a handful of "mop"/"c_mop_t" names that
        never appear in arg_lut.csv at all - operand_vocab must still surface
        them, since real instruction records resolve against them too."""
        raw = _read_raw_csv()
        self.assertNotIn("mop_r_t_30", raw)
        vocab = {e["name"]: e for e in reference.operand_vocab(EXTENSIONS_DIR)}
        self.assertEqual(vocab["mop_r_t_30"], {"name": "mop_r_t_30", "lsb": 30, "width": 1})

    def test_sorted_by_name_with_no_duplicates(self):
        vocab = reference.operand_vocab(EXTENSIONS_DIR)
        names = [e["name"] for e in vocab]
        self.assertEqual(names, sorted(names))
        self.assertEqual(len(names), len(set(names)))


if __name__ == "__main__":
    unittest.main()
