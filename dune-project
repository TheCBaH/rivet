(lang dune 3.15)

(cram enable)

; One package, so that a consuming project can name every library it needs as
; rivet.<library>. Libraries without a public name are private to this project,
; and dune resolves a private library only from within it.

(package
 (name rivet)
 (synopsis "Retargetable assembler and linker in pure OCaml")
 (allow_empty))

; 3.15, not 3.8: gating a library on %{env:...} needs it. Dune cannot make a
; library's `modes` conditional, and two same-named stanzas in one file crash
; it, so the Melange build is a parallel tree under melange/ that copy_files the
; same sources and is enabled by RIVET_MELANGE. See melange/dune.

(using melange 0.1)

; Menhir owns the common statement and expression grammar (docs/design.md
; §4.7). Only lib/asm_core uses it, and only for that grammar: individual
; instruction forms must never become Menhir productions.

(using menhir 3.0)
