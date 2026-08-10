(lang dune 3.15)

(cram enable)

; 3.15, not 3.8: gating a library on %{env:...} needs it. Dune cannot make a
; library's `modes` conditional, and two same-named stanzas in one file crash
; it, so the Melange build is a parallel tree under melange/ that copy_files the
; same sources and is enabled by ASM_MELANGE. See melange/dune.

(using melange 0.1)

; Menhir owns the common statement and expression grammar (.ai/asm_plan.md
; §4.7). Only lib/asm_core uses it, and only for that grammar: individual
; instruction forms must never become Menhir productions.

(using menhir 3.0)
