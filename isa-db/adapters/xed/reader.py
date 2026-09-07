"""XED datafiles/ adapter: block-level source records plus a compat view.

Mirrors asm/tools/lib/isa_inventory_xed.ml's traversal and mode-applicability
logic exactly (see that file's own comments for the "why" of each rule this
re-derives) so that `compat_entries_for_profile` can be regression-tested
directly against the checked-in asm/fixtures/isa-inventory/{x86_32,x86_64}
manifests. Source records go further: unlike the OCaml importer, they keep
each block separately (not merged by (mnemonic, extension) yet), and they
preserve the block's own EXTENSION:/ISA_SET: fields as provenance instead of
discarding them in favor of the directory-derived group - source grouping is
not an ISA feature.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

from normalize.model import (
    SourceRecord,
    any_of,
    eval_mode_expr,
    mode_equals,
    mode_not_equals,
    opaque_encoding,
    unconditional,
)

_MODE_TOKENS = ("mode16", "mode32", "mode64", "not64")


def tokens(line: str) -> list[str]:
    parts: list[str] = []
    for chunk in line.split(" "):
        parts.extend(chunk.split("\t"))
    return [p for p in parts if p != ""]


def field_value(line: str, field_name: str) -> str | None:
    if not line.startswith(field_name):
        return None
    rest = line[len(field_name) :]
    if rest and rest[0] not in (" ", "\t", ":"):
        return None
    colon = line.find(":")
    if colon == -1:
        return None
    return line[colon + 1 :].strip()


@dataclass
class Block:
    iclass: str | None = None
    isa_extension: str | None = None
    isa_set: str | None = None
    line: int = 0
    patterns: list[str] = field(default_factory=list)

    @property
    def pattern_tokens(self) -> list[list[str]]:
        return [tokens(p) for p in self.patterns]


def parse_datafile_blocks(text: str) -> list[Block]:
    """Parse one XED datafile's text into its ICLASS-bearing blocks.

    Blocks without an ICLASS field are skipped, matching close_block in
    isa_inventory_xed.ml ("a block with no ICLASS field is not an
    instruction"). Text outside any '{'/'}' pair (section headers, stray
    comments) is ignored, not a parse error.
    """
    blocks: list[Block] = []
    in_block = False
    current = Block()
    for lineno, raw_line in enumerate(text.split("\n"), start=1):
        trimmed = raw_line.strip()
        if trimmed == "" or trimmed.startswith("#"):
            continue
        if trimmed == "{":
            if in_block:
                raise ValueError(f"line {lineno}: nested '{{'")
            in_block = True
            current = Block(line=lineno)
            continue
        if trimmed == "}":
            if not in_block:
                raise ValueError(f"line {lineno}: unmatched '}}'")
            in_block = False
            if current.iclass is not None:
                blocks.append(current)
            continue
        if not in_block:
            continue
        if current.iclass is None:
            v = field_value(raw_line, "ICLASS")
            if v is not None:
                current.iclass = v
        if current.isa_extension is None:
            v = field_value(raw_line, "EXTENSION")
            if v is not None:
                current.isa_extension = v
        if current.isa_set is None:
            v = field_value(raw_line, "ISA_SET")
            if v is not None:
                current.isa_set = v
        v = field_value(raw_line, "PATTERN")
        if v is not None:
            current.patterns.append(v)
    if in_block:
        raise ValueError("unterminated '{' at end of file")
    return blocks


def pattern_mode_expr(toks: list[str]) -> dict:
    has = lambda t: t in toks  # noqa: E731
    constraints = []
    if has("mode64"):
        constraints.append(mode_equals("mode64"))
    if has("mode32"):
        constraints.append(mode_equals("mode32"))
    if has("mode16"):
        constraints.append(mode_equals("mode16"))
    if has("not64"):
        constraints.append(mode_not_equals("mode64"))
    if not constraints:
        return unconditional()
    if len(constraints) == 1:
        return constraints[0]
    return {"kind": "all", "of": constraints}


def block_applicability(block: Block) -> dict:
    if not block.patterns:
        return unconditional()
    return any_of([pattern_mode_expr(t) for t in block.pattern_tokens])


def applies_32(applicability: dict) -> bool:
    return eval_mode_expr(applicability, "mode16") or eval_mode_expr(applicability, "mode32")


def applies_64(applicability: dict) -> bool:
    return eval_mode_expr(applicability, "mode64")


# ---------------------------------------------------------------------------
# Traversal: root .txt files as group "base", then each top-level directory
# recursively (nested subdirectories keep their top-level directory's group -
# e.g. amd/amdxop/* is group "amd") - see isa_inventory_xed.ml's own comments.


def _is_dir(p: Path) -> bool:
    return p.is_dir()


def _list_txt_files_rec(d: Path) -> list[Path]:
    out = []
    for child in sorted(d.iterdir()):
        if _is_dir(child):
            out.extend(_list_txt_files_rec(child))
        elif child.name.endswith(".txt"):
            out.append(child)
    return out


def iter_source_blocks(datafiles_dir: Path):
    """Yield (group, rel_path, Block) for every ICLASS block under datafiles_dir."""
    root_files = sorted(
        p for p in datafiles_dir.iterdir() if not _is_dir(p) and p.name.endswith(".txt")
    )
    for f in root_files:
        for b in parse_datafile_blocks(f.read_text(encoding="utf-8", errors="replace")):
            yield "base", f.relative_to(datafiles_dir).as_posix(), b

    top_dirs = sorted(p.name for p in datafiles_dir.iterdir() if _is_dir(p))
    for group in top_dirs:
        for f in _list_txt_files_rec(datafiles_dir / group):
            for b in parse_datafile_blocks(f.read_text(encoding="utf-8", errors="replace")):
                yield group, f.relative_to(datafiles_dir).as_posix(), b


def source_records(datafiles_dir: Path, snapshot: str) -> list[dict]:
    records = []
    for group, rel_path, block in iter_source_blocks(datafiles_dir):
        applicability = block_applicability(block)
        pattern_note = (
            "; ".join(block.patterns) if block.patterns else "no PATTERN line (applies to both modes)"
        )
        rec = SourceRecord(
            record_id=f"xed:{group}:{block.iclass.lower()}@{rel_path}:{block.line}",
            source="xed",
            kind="instruction-form",
            native_name=block.iclass,
            snapshot=snapshot,
            origin={"path": rel_path, "line": block.line},
            encoding=opaque_encoding(f"PATTERN: {pattern_note}"),
            applicability=applicability,
            provenance={
                "group": group,
                "isa_extension": block.isa_extension,
                "isa_set": block.isa_set,
            },
            unresolved=[
                "operand forms (OPERANDS field) are not extracted",
                "XED's UDELETE cross-file suppression directive is not evaluated",
            ],
        )
        records.append(rec.to_dict())
    return records


def compat_entries(datafiles_dir: Path) -> dict[tuple[str, str], tuple[bool, bool]]:
    """(mnemonic, group) -> (applies_32, applies_64), OR-merged across blocks.

    Reproduces isa_inventory_xed.ml's merge_entries exactly (same key, same
    OR-across-blocks semantics), computed from the same applicability
    expressions `source_records` builds rather than a second copy of the
    mode-token logic.
    """
    merged: dict[tuple[str, str], tuple[bool, bool]] = {}
    for group, _rel_path, block in iter_source_blocks(datafiles_dir):
        key = (block.iclass.lower(), group)
        applicability = block_applicability(block)
        a32, a64 = applies_32(applicability), applies_64(applicability)
        prev = merged.get(key)
        merged[key] = (a32, a64) if prev is None else (prev[0] or a32, prev[1] or a64)
    return merged


def compat_entries_for_profile(datafiles_dir: Path, profile: str) -> set[tuple[str, str]]:
    if profile not in ("x86_32", "x86_64"):
        raise ValueError(f"unsupported profile: {profile!r}")
    idx = 0 if profile == "x86_32" else 1
    return {key for key, flags in compat_entries(datafiles_dir).items() if flags[idx]}


def source_records_for_profile(datafiles_dir: Path, profile: str, snapshot: str) -> list[dict]:
    """Like `source_records`, restricted to records whose applicability
    evaluates true for `profile` - the same per-mode evaluation
    `compat_entries_for_profile` already does, applied record-by-record
    instead of merged by (mnemonic, group).
    """
    if profile not in ("x86_32", "x86_64"):
        raise ValueError(f"unsupported profile: {profile!r}")
    check = applies_32 if profile == "x86_32" else applies_64
    return [r for r in source_records(datafiles_dir, snapshot) if check(r["applicability"])]
