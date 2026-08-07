# Vendored `Fmt`

`fmt.ml` and `fmt.mli` are copied **verbatim** from `fmt.0.11.0`
(`src/fmt.ml`, `src/fmt.mli`), ISC licensed — see `LICENSE.md`.

| File | SHA-256 |
|---|---|
| `fmt.ml` | `e7a41d9a8c1f2f2b8a075cdb461bea6b8f8582092ae8ca717abcb762cf0d146f` |
| `fmt.mli` | `050a7b7448584928a21462d23ec0ba3abe3a985231535855b9be8e72595d9c38` |

## Why vendored rather than an opam dependency

The opam `fmt` package is built with topkg and installs a META *template*, which
Dune refuses to expose to Melange:

```text
Error: The library fmt was not compiled with Dune or it was compiled with Dune
but published with a META template. Such libraries are not compatible with
melange support
```

`.ai/asm_plan.md` §11.6 requires the whole production closure to build and run
under native OCaml, js_of_ocaml, and Melange, so an external `fmt` is not usable.
Vendoring the two core source files as a Dune library with
`(modes byte native melange)` fixes that, and was verified to produce
byte-identical output under all three backends.

## Why this does not weaken the purity closure

`.ai/asm_plan.md` §1/§2.2/§3.7 ban C sources, foreign stubs, and C-backed
transitive dependencies from the production closure. The vendored subset is pure
OCaml over `Stdlib` only — no `external`, no `Obj`, no `Unix`. The upstream
`fmt.tty` (terminal capability detection, needs `Unix`) and `fmt.cli` (needs
`cmdliner`) sub-libraries are **not** vendored.

## Upgrading

Replace both files from the upstream tarball, update the hashes above, and
re-run the three-build equality harness. Do not patch the sources in place: any
local change must be a separate module in `lib/foundation`, so this directory
stays a verifiable copy.
