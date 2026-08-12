# Vendored `err_trace`

`err.ml` and `err.mli` are built from the `err_trace` git submodule checked out
at [`../err_trace`](../err_trace) from
<https://github.com/TheCBaH/err_trace>, branch `main`, MIT licensed (see
`../err_trace/LICENSE`). The library's public
module is `Err`; the published API documentation is at
<https://thecbah.github.io/err_trace/err_trace/>.

Nothing is copied into this directory. The `dune` here `copy_files#`s the two
sources out of the submodule at build time, so the only record of *which*
version is vendored is the submodule's gitlink — which is exactly what a
submodule is for. Unlike [`../fmt`](../fmt/README.md), there is no SHA-256
table to keep in step: `git submodule status` already answers that question.

## What it is for

`err_trace` is the typed-error layer described in [`../../docs/errors.md`](../../docs/errors.md).
`Foundation.Origin` says where in the *assembly source* a problem is;
`Err` records where in the *assembler* it was detected, carries the typed
error payload across phase boundaries, and bounds the semantic event trail. It
is named by `lib/foundation/dune` rather than higher up because `foundation` is
the root of the production closure, so everything above it gets `Err`
transitively.

## Why vendored rather than an opam dependency

`.ai/asm_plan.md` §1/§2.2/§3.7 ban C sources, foreign stubs, and C-backed
transitive dependencies from the production closure, and `tools/asm_audit.ml`
enforces that with an `external_allowlist` that is **empty by design** — the
closure is meant to need nothing from opam at all. Vendoring keeps that
property literally true rather than nearly true.

It costs nothing here: `err_trace` is dependency-free pure OCaml over `Stdlib`
(`Atomic`, `Printexc`, `Format`), with no `external`, no `Obj`, no `Unix`, and
it already supports the three backends §11.6 requires — native OCaml,
js_of_ocaml, and Melange.

## Why the submodule is `data_only_dirs`

The submodule is a whole upstream repository, not just the two files we build.
Its own `test/dune` and `bench/dune` declare a test suite depending on
`ppx_expect`, `base`, `time_now` and `compiler-libs`. Left visible to dune,
those stanzas appear in `dune describe`, and `tools/asm_audit.ml` classifies
anything under `vendor/` as production (`production_dirs`), while `base`,
`time_now` and `ppx_expect` are on its `denied` list — so `make asm-purity`
fails, and on a CI image without `ppx_expect` the build fails outright.

[`../dune`](../dune) marks the checkout `data_only_dirs`, which leaves the
files reachable as ordinary sources while reading no dune file inside it.
`tools/asm-check-planted.sh` has a case that deletes that file and requires the
purity audit to reject the result, so the protection cannot be lost silently.

## Upgrading

```sh
git -C asm/vendor/err_trace fetch origin main
git -C asm/vendor/err_trace checkout origin/main
git add asm/vendor/err_trace          # commits the new gitlink
make asm-ci asm-melange asm-js        # all three backends, plus the audits
```

Do not patch the submodule in place: any local change must be a separate module
under `lib/foundation`, so this stays a plain checkout of upstream. If a change
belongs in the library, send it upstream and move the gitlink.
