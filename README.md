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
