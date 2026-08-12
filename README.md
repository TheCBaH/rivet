# Devcontainer for CompCert

[![CompCert devcontainer build](https://github.com/TheCBaH/devcontainer.CompCert/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/TheCBaH/devcontainer.CompCert/actions/workflows/build.yml)

Devcontainer to build and check [CompCert](https://compcert.org/), the
formally-verified C compiler, vendored here as the `modules/CompCert`
submodule, using [Rocq](https://rocq-prover.org/) (formerly Coq).

CompCert is not free software; this non-commercial distribution may only be used
for evaluation, research, educational and personal purposes. See
[modules/CompCert/LICENSE](modules/CompCert/LICENSE) for details. This repo's
own [LICENSE](LICENSE) covers only the devcontainer and build tooling.

## Get started
* [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=1323060576)
* Then, inside the devcontainer:
  * `make compcert` configure and build CompCert (proof + `ccomp`) for the
    native architecture
  * `make compcert-check-proof` recheck the proof with `coqchk`/`rocqchk`
  * `make compcert-test` run the CompCert test suite

List of OCaml/opam packages (including Rocq and Menhir) installed by the
devcontainer is located in
[.devcontainer/devcontainer.json](.devcontainer/devcontainer.json).

## Rocq-free split build
Rocq is only needed to check the proof and extract OCaml sources from it;
building `ccomp` from those sources is plain OCaml.

* `make compcert-extraction-archive` prove and extract, then archive the
  extracted sources into `compcert-extraction.tar.gz` - the only step that
  needs Rocq
* `make compcert-build-from-archive` build `ccomp` from that archive alone

## CompCert as a dune library
`compcert-lib/` is a standalone dune project that builds the same sources as a
regular OCaml library (`compcert`), so CompCert can be depended on like any
other library. `ccomp`'s CLI entry point, `driver/Driver.ml`, is kept out of the
library as `bin/main.ml`.

* `make compcert-lib-build` sync the sources into `compcert-lib/` (not
  committed) and build them with dune
* `make compcert-lib-run ARGS=-version` run the dune-built driver

## Redistributable package
For consumers who just want to `dune build` CompCert as a library, without
cloning this repo or touching Rocq:

* `make compcert-export-archive` package that dune project plus CompCert's own
  `LICENSE` into `compcert-export.tar.gz`, also published on `v*` tags
* `make compcert-export-run ARGS=-version` build and run the unpacked archive

## Retargetable assembler
`asm/` is a standalone dune project - independent of the CompCert build, so it
needs neither Rocq nor a cross toolchain - holding a retargetable assembler for
x86-32, x86-64, ARM and AArch64. See [.ai/asm_plan.md](.ai/asm_plan.md).

* `make asm-ci` what CI runs, and what to run before pushing
* `make asm-test` build and run the test suite

It vendors [err_trace](https://github.com/TheCBaH/err_trace) as the
`asm/vendor/err_trace` submodule, so a fresh clone needs
`git submodule update --init` before `make asm-build`. See
[asm/vendor/err_trace_local/README.md](asm/vendor/err_trace_local/README.md) for why it is
vendored rather than taken from opam, and
[asm/docs/errors.md](asm/docs/errors.md) for the error model it supports.
