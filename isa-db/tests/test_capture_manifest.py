import unittest
from unittest.mock import patch

from export.writer import CAPTURE_MANIFEST_SCHEMA, _verify_checkout, capture_manifest, load_lock


class TestPinnedCheckoutVerification(unittest.TestCase):
    def test_wrong_commit_is_rejected(self):
        with patch("export.writer._git", return_value="wrong"):
            with self.assertRaisesRegex(ValueError, "pins"):
                _verify_checkout("source", __import__("pathlib").Path("/source"), "expected")

    def test_dirty_checkout_is_rejected(self):
        with patch("export.writer._git", side_effect=["expected", " M input"]):
            with self.assertRaisesRegex(ValueError, "dirty"):
                _verify_checkout("source", __import__("pathlib").Path("/source"), "expected")


class TestCaptureManifest(unittest.TestCase):
    def test_has_versioned_contract_and_input_hashes(self):
        manifest = capture_manifest(load_lock())
        self.assertEqual(manifest["schema"], CAPTURE_MANIFEST_SCHEMA)
        self.assertEqual(manifest["dirty-source-policy"], "refuse")
        self.assertTrue(manifest["producer-revision"].startswith("sha256:"))
        for source in ("xed", "riscv_opcodes"):
            facts = manifest["sources"][source]
            self.assertGreater(facts["selected-input-file-count"], 0)
            self.assertEqual(len(facts["selected-input-sha256"]), 64)
