"""Thin wrapper around riscv-opcodes' own upstream producer.

`asm/vendor/isa-data/riscv-opcodes/upstream/src/riscv_opcodes/shared_utils.py`
is the *project's own* generator - the one that builds riscv-opcodes'
`instr_dict.json` and every HDL/C header it ships. Its `create_inst_dict`
already does real bit-range validation, `$import`/`$pseudo_op` resolution,
and (via `arg_lut`) knows the canonical bit range of every named operand -
all things `adapters/riscv_opcodes/reader.py`'s own line parser either
reimplements by hand or skips outright, verified against the vendored
checkout.

Deliberately does not import `riscv_opcodes.parse` (the `python3 -m
riscv_opcodes` entry point) - that module additionally imports
`c_utils`/`chisel_utils`/`go_utils`/`latex_utils`/`rust_utils`/
`sverilog_utils`/`svg_utils` at module scope, none of which this adapter
needs or has vetted for extra dependencies.
"""

from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
UPSTREAM_SRC = REPO_ROOT / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream" / "src"


def _shared_utils():
    if str(UPSTREAM_SRC) not in sys.path:
        sys.path.insert(0, str(UPSTREAM_SRC))
    from riscv_opcodes import shared_utils

    return shared_utils


def create_inst_dict_for_file(extension_filename: str, *, include_pseudo: bool = True) -> dict:
    """Run upstream's create_inst_dict scoped to exactly one extensions/ file.

    `file_filter` is matched with `fnmatch` against filenames under
    `extensions/` (and `extensions/unratified/`); an exact filename with no
    glob metacharacters selects just that one file, so this keeps today's
    per-file provenance/origin tracking intact rather than resolving the
    whole ISA at once. `include_pseudo=True` so a $pseudo_op mnemonic
    resolves to its own concrete entry even when its base instruction lives
    in the same file - upstream's own default (`include_pseudo=False`)
    would otherwise suppress it in exactly that case (e.g. rv_i's `nop`
    would vanish, since rv_i also defines `addi`).
    """
    shared_utils = _shared_utils()
    return shared_utils.create_inst_dict([extension_filename], include_pseudo=include_pseudo)


def arg_lut() -> dict[str, tuple[int, int]]:
    """The operand-name -> (msb, lsb) table `create_inst_dict` itself
    consults. Call `create_inst_dict_for_file` first if resolving a name
    that depends on an overloaded ("existing=new") alias registered as a
    side effect of processing a specific file - by the time that call
    returns successfully, every variable-field name it used is guaranteed
    present here (an unresolvable name makes upstream raise SystemExit
    instead of returning).
    """
    return _shared_utils().arg_lut
