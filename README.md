# rivet

A retargetable assembler and linker in pure OCaml, for x86-32, x86-64, ARM,
AArch64, RV32 and RV64. It reads GNU-syntax assembly, produces an in-memory
image, and can bind that image at chosen addresses. There is no C code in the
production closure: the same sources run natively, under js_of_ocaml, and
under Melange. See [docs/design.md](docs/design.md).

[![ci](https://github.com/TheCBaH/rivet/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/TheCBaH/rivet/actions/workflows/ci.yml)

## Get started
Inside the devcontainer (OCaml, dune, Menhir, the cross binutils and gcc, and
qemu-user):

* `git submodule update --init` fetch the vendored libraries
* `make build` build everything
* `make test` build and run the test suite
* `make ci` what CI runs, and what to run before pushing

`tool/asm.exe` is the command line: `asm --target aarch64 file.s`.

## Evidence
The assembler is checked against GNU as, not against its own opinion of itself.

* The **fixture oracle** compiles a small C corpus with each target's cross gcc,
  assembles the output with GNU binutils, links it, and runs the very bytes
  under QEMU. The assembler's own image is then compared with that reference
  link. It needs the cross toolchains and is not on the path of `make test`. See
  [docs/fixture-oracle.md](docs/fixture-oracle.md).
  * `make fixture-oracle-<target>` one target's complete leg
  * `make fixture-oracle` all six
* The **GAS differential** generators (`fixtures/isa-generated`,
  `fixtures/isa-difficult`) and the **cross-reference** (`fixtures/gas-xref`)
  hold the assembler to GNU as, instruction by instruction.
* The **execution ABI** suite (`make abi-conform`, `make exec`) runs generated
  helpers and the assembler's own image under qemu-user. See
  [docs/exec-abi-v1.md](docs/exec-abi-v1.md).

## Tooling
`tools/` is a separate dune project providing `rivet-tools`, the fixture,
manifest and oracle command line. It is kept apart from the assembler so that
toolchain-free checks never require an assembler build, and it can be extended
by another project: it is installed as the `rivet-tools` package, with the
compiler that generates fixtures as a parameter.

## Vendored libraries
[err_trace](https://github.com/TheCBaH/err_trace) is vendored as
`vendor/err_trace/upstream` and [Fmt](https://github.com/dbuenzli/fmt) as
`vendor/fmt/upstream`, so a fresh clone needs `git submodule update --init`
before `make build`. See [vendor/err_trace/README.md](vendor/err_trace/README.md)
and [vendor/fmt/README.md](vendor/fmt/README.md) for why they are vendored
rather than taken from opam, and [docs/errors.md](docs/errors.md) for the error
model err_trace supports.

## Using it from another project
Every library a consumer needs has a public name in the `rivet` package
(`rivet.driver`, `rivet.aarch64`, `rivet.foundation`, ...). Vendor this
repository as a subdirectory with `(vendored_dirs vendor)`, and name the
libraries as usual.

Or install it: `rivet.opam` builds the native libraries (with `Native_exec`)
against the opam `fmt` and `err_trace`, from a source archive with no
submodules, e.g. `opam pin add rivet <archive URL>`.

A consumer that already has its own Fmt and err_trace, and links them into the
same executable, builds with `RIVET_EXTERNAL_DEPS=true`. The vendored copies
are then not built; `rivet.fmt` and `rivet.err_trace` re-export the findlib
libraries `fmt` and `err_trace` (opam, or a workspace package with those public
names), so `Err.t` values cross the boundary without a second `Err` module.
Melange builds keep the vendored copies. `make external-deps` builds and tests
this configuration in a throwaway workspace; `test/consumer` is the example.

Embedding native code: `Native_exec.load` maps an image once, `call` runs it
any number of times, and `close` unmaps it after the last call. Host functions
are bound with `Native_exec.bind_host`, which generates a unit of ISA-specific
trampolines (aarch64, x86_64, riscv64) to assemble beside the image; the
addresses are process-specific and must not be cached.

## History
This repository began as a directory of a larger one, and its history was carried
over. Each such commit says `Split-from:` with the original repository and commit.

## License
[MIT](LICENSE).
