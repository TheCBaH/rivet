; The tool project's opam dependencies: one row per package, carrying its
; version floor, its scope, and the dune library names it supplies.
;
; This is the SOLE source of truth for those floors. There is no committed
; .opam file: an *.opam inside asm/ risks dune treating it as a project
; package, which affects @install and package inference in a project whose
; libraries deliberately have no public names. This is an audited declaration,
; not an installable package, so it does not pretend to be one.
;
; Checked by `asm_audit.ml tool-dependencies`, which joins it to the dune graph
; with four rules: every direct external library maps to exactly one declared
; package; every declared package supplies at least one used library; every
; floor is satisfied, evaluated BY OPAM rather than by string comparison; and
; the transitive closure stays inside a separate containment allowlist.
;
; Format: (package (>= VERSION) scope (library ...))
; Only (>= V) is accepted - the installed-set query renders exactly
; PACKAGE>=FLOOR and cannot evaluate a general opam formula. Scope is `runtime`
; or `test`; the field exists from day one so the grammar does not have to
; change when the first test-only package arrives.

(bos      (>= 0.2.1) runtime (bos))
(fpath    (>= 0.7.3) runtime (fpath))
(cmdliner (>= 1.2.0) runtime (cmdliner))
(digestif (>= 1.1.0) runtime (digestif.ocaml))
; Monotonic deadlines for Supervisor. Unix has no monotonic clock on 4.14, and
; a deadline computed from Unix.gettimeofday can be stepped backwards by NTP -
; on CI that is indistinguishable from a hang. mtime.clock.os, not bare mtime:
; the clock is the part with the OS implementation.
; All three names, because dune resolves mtime.clock.os into direct requires on
; mtime and mtime.clock as well - and rule 1 asks about the libraries actually
; used, not the one spelled in lib/dune. They all come from this one package.
(mtime     (>= 2.0.0) runtime (mtime.clock.os mtime.clock mtime))

; Deliberately absent, so these read as decisions rather than omissions:
;
;   err_trace  a LOCAL library of this project (tools/vendor/err_trace_local),
;              copied from the submodule. Not package-backed, so no row.
;   fmt        reached transitively through bos, and it is the INSTALLED opam
;              package, not asm/vendor/fmt, which this project cannot see.
;              Rule 4's business, never rule 1's.
;   rresult    a bos opam dependency that does NOT appear in the resolved dune
;              closure. Measured, not assumed.
;   unix       compiler-supplied, recognized by its directory under the
;              switch's OCaml library directory rather than by name.
;
