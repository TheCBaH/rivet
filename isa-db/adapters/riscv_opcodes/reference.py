"""Auxiliary reference data, beyond the per-instruction encodings reader.py
already extracts: ingests arg_lut.csv as a small shared "operand vocabulary"
table (name -> canonical bit range).

The raw CSV file alone would be an *incomplete* vocabulary: riscv-opcodes'
own constants.py adds a handful of names at import time that never appear in
arg_lut.csv at all (the "mop"/"c_mop_t" fields - see constants.py's six
`arg_lut["..."] = (msb, lsb)` lines right after `read_arg_lut_csv`), and
processing certain instructions registers further aliases at runtime via an
"existing=new" token (`handle_arg_lut_mapping` in shared_utils.py - the same
mechanism `upstream.arg_lut`'s own docstring already documents for
per-instruction resolution). Naively re-parsing just the CSV would therefore
silently omit names that this same export's own instruction-form records
resolve through - exactly the "re-derive from raw text instead of trusting
upstream's own computation" mistake Phase A already replaced elsewhere (see
reader.py's module docstring). So this reads `upstream.arg_lut()` - the same
live dict `create_inst_dict` consults - after processing every extension
file reader.py itself iterates over, capturing its complete closure rather
than the static CSV alone.

csrs.csv/csrs32.csv/causes.csv (CSR addresses/names, trap cause codes) are
deliberately not ingested here: they are unrelated reference tables, not
part of the operand-bit vocabulary - left for a future, separate pass if a
concrete consumer needs them.
"""

from __future__ import annotations

from pathlib import Path

from adapters.riscv_opcodes import reader, upstream


def operand_vocab(extensions_dir: Path) -> list[dict]:
    """`{name, lsb, width}` entries for every operand name `create_inst_dict`
    resolves against, sorted by name. Processes every file
    `reader._list_extension_files` selects (i.e. everything reader.py itself
    draws instruction-form records from) so this vocabulary's closure matches
    exactly what those records were resolved against - not a superset or
    subset of it.
    """
    for path in reader._list_extension_files(extensions_dir):
        upstream.create_inst_dict_for_file(path.name)
    table = upstream.arg_lut()
    return [
        {"name": name, "lsb": lsb, "width": msb - lsb + 1}
        for name, (msb, lsb) in sorted(table.items())
    ]
