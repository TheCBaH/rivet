"""Thin wrapper around XED's own stronger reader path (`gen_setup`/
`read_xed_db.xed_reader_t`), the one `pysrc/xed_to_db.py` itself is built on.

Unlike `adapters/xed/reader.py`'s hand-rolled block scan, this path is XED's
*own* code for turning `datafiles/` into fully resolved instruction records:
macro/state-bits expansion, UDELETE and version-delete handling, map/opcode/
space classification, and parsed operand objects, all done by upstream
itself instead of re-derived here, verified against the vendored checkout.

`mfile.py just-prep` (run as a subprocess, from `asm/vendor/isa-data/xed/
upstream`) is XED's *own* build step that turns `datafiles/` into the
`obj/dgen/*.txt` aggregate inputs `gen_setup.read_db` needs - it requires the
sibling `intelxed/mbuild` submodule (`genutil.find_dir('mbuild')`, resolved
by running with `cwd` set to the `upstream` checkout so the sibling
`xed/mbuild` directory is found), but does no C-compiler probing and takes
well under a second. `obj/dgen/` is untracked (the XED submodule's own
`.gitignore` has `obj*`), so this is regenerated on demand, the same way
`adapters/riscv_opcodes/upstream.py` calls `create_inst_dict` live rather
than caching its output - not checked in.

Deliberately does not import `mfile.py` itself (XED's own build-graph
driver, which needs `mbuild`) from this process - only shells out to it once
to produce `obj/dgen/`, then imports `gen_setup`/`read_xed_db` directly for
the read side, neither of which touches `mbuild` at all.
"""

from __future__ import annotations

import contextlib
import functools
import io
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
XED_UPSTREAM = REPO_ROOT / "asm" / "vendor" / "isa-data" / "xed" / "upstream"
XED_PYSRC = XED_UPSTREAM / "pysrc"
DGEN_DIR = XED_UPSTREAM / "obj" / "dgen"
MBUILD_PARENT = REPO_ROOT / "asm" / "vendor" / "isa-data" / "xed" / "mbuild"


def _ensure_dgen() -> None:
    proc = subprocess.run(
        [sys.executable, "mfile.py", "just-prep"],
        cwd=XED_UPSTREAM,
        capture_output=True,
        text=True,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"mfile.py just-prep failed (exit {proc.returncode}):\n"
            f"{proc.stdout}\n{proc.stderr}"
        )


def _gen_setup():
    # codegen.py (imported transitively via gen_setup -> read_xed_db ->
    # opnd_types -> enum_txt_writer -> codegen) calls
    # genutil.add_mbuild_to_path() at module scope, which first tries a bare
    # `import mbuild` and only falls back to genutil.find_dir('mbuild')
    # (a cwd-relative search) if that fails - so putting the sibling
    # `xed/mbuild` checkout on sys.path ourselves sidesteps the cwd
    # dependence entirely, unlike the `mfile.py just-prep` subprocess above
    # which relies on that cwd search (run with cwd=XED_UPSTREAM).
    if str(MBUILD_PARENT) not in sys.path:
        sys.path.insert(0, str(MBUILD_PARENT))
    if str(XED_PYSRC) not in sys.path:
        sys.path.insert(0, str(XED_PYSRC))
    import gen_setup

    return gen_setup


@functools.lru_cache(maxsize=None)
def read_db():
    """Return a `read_xed_db.xed_reader_t` for the vendored XED checkout.

    Regenerates `obj/dgen/` on this process's first call, then memoizes the
    parsed result (parsing all ~11k records is not free) - so always reflects
    the currently checked-out submodule commit rather than a stale cache from
    a previous run, without re-parsing on every call within one process.
    """
    _ensure_dgen()
    gen_setup = _gen_setup()

    class _Args:
        pass

    args = _Args()
    args.prefix = str(DGEN_DIR)
    gen_setup.make_paths(args)
    # xed_reader_t writes per-line progress dots to stderr as it parses;
    # swallow that (but let any exception propagate normally). Its
    # [UDELETES]/[VERSION DELETES] summary lines go through genutil.msgout,
    # a reference to sys.stdout captured at genutil's import time - earlier
    # than this redirect - so those two lines aren't swallowed and are left
    # to print; they're a useful one-line-per-run summary, not noise worth
    # monkeypatching upstream's module state to suppress.
    with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
        return gen_setup.read_db(args)


def records() -> list:
    """The `xed_reader_t.recs` list: one `inst_t` per resolved instruction
    form, UDELETE/version-delete already applied, `iclass`/`iform`/
    `extension`/`isa_set`/`space`/`map`/`opcode`/`pattern`/`parsed_operands`
    (among many other upstream-computed fields) all present as attributes.
    """
    return read_db().recs
