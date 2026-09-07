"""Regression-tests XED's own `gen_setup`/`read_xed_db.xed_reader_t` reader
path, wrapped by `adapters/xed/upstream.py`, made permanent instead of a
one-off REPL session.

Ground truth for AAA (ASCII adjust after addition) and one legacy-map ADD
form below was read directly off `inst_t` objects in that same session and
is easy to cross-check by hand against Intel's SDM (AAA is a lone 0x37 byte,
real-mode/32-bit only; ADD_GPRv_IMMz is opcode 0x81 /0).
"""

from __future__ import annotations

import contextlib
import io
import unittest

from adapters.xed import upstream


class TestUpstreamReader(unittest.TestCase):
    def test_loads_the_whole_corpus(self):
        recs = upstream.records()
        # 10,994 at the time this was written; assert a floor,
        # not an exact count, so a routine XED submodule bump doesn't break
        # this test over a harmless instruction-count change.
        self.assertGreater(len(recs), 10000)

    def test_udelete_and_version_delete_are_applied(self):
        # Re-derive the exact counts read_xed_db itself prints (suppressed
        # by upstream.read_db()) by re-running the same steps without the
        # dedup/delete passes, and diffing against the public records() list.
        gen_setup = upstream._gen_setup()
        upstream._ensure_dgen()

        class _Args:
            pass

        args = _Args()
        args.prefix = str(upstream.DGEN_DIR)
        gen_setup.make_paths(args)
        xeddb = upstream.read_db()
        with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            raw = xeddb._process_input_lines(args.instructions_filename)
            raw = xeddb._expand_compound_values(raw)
        self.assertGreater(len(raw), len(xeddb.recs))

    def _find(self, iclass: str, *, space: str | None = None):
        matches = [
            v for v in upstream.records() if v.iclass == iclass and (space is None or v.space == space)
        ]
        self.assertTrue(matches, f"no records for iclass={iclass!r} space={space!r}")
        return matches

    def test_aaa_known_encoding(self):
        (aaa,) = self._find("AAA")
        self.assertEqual(aaa.iform, "AAA")
        self.assertEqual(aaa.extension, "BASE")
        self.assertEqual(aaa.isa_set, "I86")
        self.assertEqual(aaa.category, "DECIMAL")
        self.assertEqual(aaa.space, "legacy")
        self.assertEqual(aaa.map, 0)
        self.assertEqual(aaa.opcode, "0x37")
        self.assertEqual(aaa.mode_restriction, "not64")
        self.assertEqual(aaa.implicit_operands, ["AL", "AH"])
        self.assertEqual(aaa.explicit_operands, ["none"])

    def test_add_gprv_immz_known_encoding(self):
        matches = self._find("ADD", space="legacy")
        by_iform = {v.iform: v for v in matches}
        add = by_iform["ADD_GPRv_IMMz"]
        self.assertEqual(add.extension, "BASE")
        self.assertEqual(add.isa_set, "I86")
        self.assertEqual(add.map, 0)
        self.assertEqual(add.opcode, "0x81")
        op_names = [op.name for op in add.parsed_operands]
        self.assertEqual(op_names, ["REG0", "IMM0"])


if __name__ == "__main__":
    unittest.main()
