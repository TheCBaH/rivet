# Vendored `Fmt`

`fmt.ml` and `fmt.mli` are built from the `fmt` git submodule checked out at
[`./upstream`](./upstream) from <https://github.com/dbuenzli/fmt>, pinned at
the `v0.11.0` tag (`src/fmt.ml`, `src/fmt.mli`), ISC licensed — see
`upstream/LICENSE.md`.

Nothing is copied into this directory ahead of time. `dune` here
`copy_files#`s the two sources out of `upstream/` at build time, so the only
record of *which* version is vendored is the submodule's gitlink — matching
[`../err_trace`](../err_trace/README.md). There is no SHA-256 table to keep in
step: `git submodule status` already answers that question.

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
`cmdliner`) sub-libraries are **not** vendored, and `upstream/`'s `dune` is
`data_only_dirs` so nothing else in the checkout can be read by dune either.

## Upgrading

```sh
git -C asm/vendor/fmt/upstream fetch origin --tags
git -C asm/vendor/fmt/upstream checkout <new-tag>
git add asm/vendor/fmt/upstream        # commits the new gitlink
make asm-ci asm-melange asm-js         # all three backends, plus the audits
```

Do not patch the submodule in place: any local change must be a separate module
in `lib/foundation`, so this directory stays a verifiable copy. If a change
belongs in the library, send it upstream and move the gitlink.
