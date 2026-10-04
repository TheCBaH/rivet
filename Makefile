# The retargetable assembler: a dune project at the repository root, plus the
# tools/ project that generates and checks its fixtures. See docs/design.md.
#
# `asm-*` spellings of every goal below are kept as aliases for one release.

default: build

# fmt rewrites sources in place; fmt-check is the CI form, which fails with a
# diff instead. Both go through dune's @fmt alias, so the vendored sources under
# vendor/ (marked (vendored_dirs) in dune) are skipped and stay byte-identical
# to upstream.
#
# Both depend on fmt-ocamlformat rather than trusting whatever ocamlformat a
# devcontainer image happens to ship. The pin in .ocamlformat is otherwise only
# enforced by ocamlformat's own refusal to run against a mismatched binary - a
# check that gives no way to self-heal. Installing the pinned version
# explicitly, here, on every run, makes fmt/fmt-check work the same way on any
# machine regardless of what the image happened to ship.
OCAMLFORMAT_VERSION := $(shell awk -F' *= *' '/^version/{print $$2}' .ocamlformat)

fmt-ocamlformat:
	opam install -y ocamlformat.$(OCAMLFORMAT_VERSION)

# err_trace and Fmt are both vendored as submodules (vendor/err_trace/upstream,
# vendor/fmt/upstream) and their sources are copy_files'd into the build by the
# enclosing directory's dune, so an uninitialized submodule is a build failure -
# and an unhelpful one, since dune reports it as a missing rule for a path
# rather than as a missing checkout. Checked here rather than in ci so that a
# bare `make build` gets the same answer.
submodules:
	@test -f vendor/err_trace/upstream/src/err.ml || { \
	  echo "vendor/err_trace/upstream is not checked out; run:" >&2; \
	  echo "  git submodule update --init vendor/err_trace/upstream" >&2; \
	  exit 1; }
	@test -f vendor/fmt/upstream/src/fmt.ml || { \
	  echo "vendor/fmt/upstream is not checked out; run:" >&2; \
	  echo "  git submodule update --init vendor/fmt/upstream" >&2; \
	  exit 1; }

build: submodules
	opam exec -- dune build @all

test: build fixtures-check gas-xref-check isa-generated-check isa-difficult-check
	opam exec -- dune build @runtest

fmt: fmt-ocamlformat
	opam exec -- dune build @fmt --auto-promote

fmt-check: fmt-ocamlformat
	opam exec -- dune build @fmt

# The Melange configuration. Declaring melange mode on a library schedules melc
# in @all whether or not anything emits JavaScript, so it is gated on
# RIVET_MELANGE and the two configurations are built separately (melange/dune
# explains why dune leaves no better option). This target needs Melange
# installed and therefore an OCaml 4.14 switch; it is not part of ci.
melange:
	RIVET_MELANGE=true opam exec -- dune build @all

# Byte-identical output from native OCaml, js_of_ocaml/Node, and Melange/Node.
# All three run the *same* committed cram baseline, test/cram/bigint_dump.t,
# with RIVET_BACKEND choosing which artifact produces it - so equality is the
# cram diff itself rather than a comparison between two uncommitted outputs.
# Separate dune invocations because RIVET_MELANGE changes which library stanzas
# are enabled.
js: build js-portable
	RIVET_BACKEND=melange RIVET_MELANGE=true opam exec -- dune build @runtest

# The two legs that need no Melange, split out so the portable CI matrix can run
# them on every image. Melange requires `ocaml >= 4.14 & < 4.15`, so the third
# leg cannot run on the OCaml 5.x images the matrix also builds - but
# js_of_ocaml can, and leaving it out of the matrix entirely would mean the only
# evidence that the closure compiles to JavaScript came from one pinned leg.
js-portable: build
	RIVET_BACKEND=native opam exec -- dune build @runtest
	RIVET_BACKEND=jsoo opam exec -- dune build @runtest

# The real-browser smoke harness. A separate target, not a fourth RIVET_BACKEND
# cram leg: the cram mechanism is built around one synchronous process's stdout
# diffed against a committed transcript, while a real browser run is an async
# multi-step lifecycle with a genuine heavy external dependency (Chromium).
# Not reachable from ci.
#
# js is a prerequisite (proving three-build equality first is a cheap gate
# before spending Chromium-launch time), but melange deliberately is not: the
# wrapper script already runs the one RIVET_MELANGE=true dune build it needs.
js-browser: js
	scripts/asm-browser-harness.sh

# The in-process execution host: maps a laid-out image, binds it and calls its
# entry. It ships C stubs, so it is gated by RIVET_NATIVE_EXEC and outside every
# default build, and it runs code natively, so it is only meaningful on a host
# of the target's own ISA. Not part of ci.
native-exec: submodules
	RIVET_NATIVE_EXEC=true opam exec -- dune build @native_exec/all @native_exec/runtest

# The behavioral tool gate. APT tooling is range-checked rather than
# digest-pinned, and version numbers alone do not establish compatibility where
# behaviour matters - so this asks the tools to do the things the project
# depends on and records what answered. It needs the cross toolchains, all six
# qemu-user binaries, qemu-system and gdb, but no part of the assembler.
tool-gate: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) tool-gate all

# The Melange opt-in, verified rather than asserted. Checks both clean
# configurations: zero melc rules and a successful build with Melange
# unavailable, and the rules reappearing under RIVET_MELANGE=true. Needs nothing
# beyond dune, so it runs on every leg.
melange-optin:
	scripts/asm-melange-optin.sh

# The opt-in configuration for a consumer that supplies its own Fmt and
# err_trace (vendor/host/dune). test/consumer plays that consumer in a
# throwaway workspace with this tree vendored inside it.
external-deps: submodules
	scripts/external-deps.sh

# {1 Fixtures}
#
# Two modes. fixtures-check needs NO cross toolchain, so every ordinary test run
# and every portable CI leg consumes the checked-in bytes. Regenerating them
# needs the six cross gcc/binutils toolchains and QEMU, and stays off that path.
#
# fixtures/c holds each case's C source and expected status; fixtures/gcc-14
# holds what the cross gcc generated from it, with the GNU oracle artifacts.
#
# tools-build is TARGETED: `dune build @all` here would put an entire assembler
# build behind a toolchain-free check.
fixtures-check: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture check

# The fixture profiles come from scripts/target-matrix.sh rather than a copy of
# the list here. $(shell) does not fail the build when its command fails, so
# both the status and the cardinality are asserted: an empty or short set would
# otherwise make the aggregate goal below run zero targets and report success.
FIXTURE_TARGETS := $(shell scripts/target-matrix.sh fixture)
ifneq ($(.SHELLSTATUS),0)
$(error scripts/target-matrix.sh fixture failed)
endif
ifneq ($(words $(FIXTURE_TARGETS)),6)
$(error expected 6 fixture targets, got '$(FIXTURE_TARGETS)')
endif

FIXTURE_VERIFY_GOALS := $(addprefix fixtures-verify-,$(FIXTURE_TARGETS))
FIXTURE_EXEC_GOALS   := $(addprefix fixture-exec-,$(FIXTURE_TARGETS))
FIXTURE_ORACLE_GOALS := $(addprefix fixture-oracle-,$(FIXTURE_TARGETS))

# Static pattern rules, not `%` implicit rules. GNU Make skips implicit rule
# search for .PHONY targets, so an implicit pattern plus a phony expansion
# yields "Nothing to be done" and exit 0 - a silent no-op. Static pattern rules
# are explicit rules, so .PHONY does what it is meant to here.
.PHONY: $(FIXTURE_VERIFY_GOALS)
$(FIXTURE_VERIFY_GOALS): fixtures-verify-%: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture verify -- $*

.PHONY: $(FIXTURE_EXEC_GOALS)
$(FIXTURE_EXEC_GOALS): fixture-exec-%: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture exec -- $*

# One target's complete oracle leg, in the one order that makes the evidence
# chain hold: exact regeneration, GNU oracle, QEMU execution, manifest
# completeness, clean tree. CI runs this goal rather than restating the
# sequence, so local and CI ordering have a single definition.
#
# --check before git diff is load-bearing: git diff cannot see an untracked
# file, so a newly produced oracle artifact would leave the leg reporting clean.
# --check reports it as UNRECORDED. git diff stays because the manifest excludes
# itself from its own hashes.
.PHONY: $(FIXTURE_ORACLE_GOALS)
$(FIXTURE_ORACLE_GOALS): fixture-oracle-%: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture verify -- $*
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle -- $*
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture exec -- $*
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture check
	git diff --exit-code -- fixtures/gcc-14

fixture-oracle:
	@for t in $(FIXTURE_TARGETS); do $(MAKE) fixture-oracle-$$t || exit 1; done

# A regeneration difference is a reviewed failure, not a refresh: it can change
# accepted syntax, relocations, or instruction coverage.
fixtures-regen: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture regen

# The reference-assembler artifacts the differential gate compares our encoder
# against. rehash folds them into the same manifest, so fixtures-check covers
# them too.
oracle: tools-build fixtures-regen
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle -- all
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture rehash

# The GNU as cross-reference. Same two-mode policy as the fixtures above:
# check hashes the committed corpus and needs no toolchain, regen needs the six
# cross binutils. Its inputs are generated from this project's own AST corpus,
# so binutils alone is the whole requirement.
gas-xref-check: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref check

gas-xref-regen: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref regen

# The isa-generated pilot GAS differential generator. Same two-mode split as
# gas-xref: check replays the committed fixtures/isa-generated/cases.jsonl
# corpus offline and needs no toolchain, regen needs the six targets' cross GNU
# binutils (in particular the RV32 one, which needs
# /usr/local/riscv32-linux-gnu-toolchain/bin on PATH) and stays off that
# critical path. The "ours" half additionally shells out to this project's own
# tool/asm.exe, so regen also needs build: with it as a prerequisite, `dune exec
# tool/asm.exe` at regen time only ever runs an already-built binary, so its
# exit code is unambiguously the assembler's own (0 accepted, 1 rejected)
# rather than a dune build failure.
isa-generated-check: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-generated check

isa-generated-regen: tools-build build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-generated regen

# The isa-difficult non-frozen difficult-form GAS differential generator. Same
# two-mode split, tool/dependency shape and build requirement as isa-generated,
# but for bounded difficult-form families (currently RISC-V split-immediate and
# compressed cases and x86 addressing/x87) in its own corpus
# (fixtures/isa-difficult/), so growing it can never touch the frozen pilot.
isa-difficult-check: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-difficult check
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-difficult coverage

isa-difficult-regen: tools-build build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-difficult regen

# The transitive purity and layer audits, and the planted violations that prove
# they can fail. Run these before treating a successful JavaScript build as
# evidence of portability - a package shipping a JS runtime replacement for its
# C primitives compiles cleanly and still breaks the no-C rule.
purity: build
	scripts/asm-check-purity.sh
	scripts/asm-check-layers.sh

planted: build
	scripts/asm-check-planted.sh

# {1 The OCaml tool project}

TOOLS_EXE   := $(CURDIR)/_build/default/tools/bin/rivet_tools.exe
TOOLS_ITEST := $(CURDIR)/_build/default/tools/test/repo/repo_tests.exe

# TARGETED, never @all: `dune build @all` would drag the whole assembler in, so
# every check-only target that later gains a tools-build edge would
# transitively require an assembler build - which is what preserves the
# toolchain-free property of fixtures-check.
#
# submodules because the tool project copy_files its err_trace sources from the
# submodule, and without the guard dune reports a missing RULE for a path
# rather than a missing checkout (see the comment on submodules itself).
tools-build: submodules
	opam exec -- dune build tools/bin/rivet_tools.exe

tools-test: submodules
	opam exec -- dune build @tools/runtest

# The repository-level suite is RUN BY MAKE, not by a dune runtest rule: it
# reads Makefile, scripts/target-matrix.sh and scripts/dev/dump-target-config.sh,
# which the tools/ project's sandboxed actions would be depending on as
# undeclared inputs. The root is passed as argv.
tools-integration: submodules
	opam exec -- dune build tools/test/repo/repo_tests.exe
	$(TOOLS_ITEST) $(CURDIR)

# A cold targeted build into a private build directory, then the resolved
# library closure of exactly the rivet_tools executable. It is on the CI path
# because it is what keeps fixtures-check free of an assembler build. Its
# planted cases run in copied trees, so no ordering constraint against the
# ordinary _build is needed.
tools-boundary: submodules
	scripts/dev/check-tool-build-boundary.sh

# scripts/target-matrix.sh is a GENERATED file: Target owns the values and this
# renders them for the shell consumers. Generating rather than having those
# scripts query the executable is what keeps asm-helpers.sh free of a dune
# build - it is the first thing its CI job runs.
#
# Staged and moved rather than redirected in place: `> scripts/target-matrix.sh`
# truncates before the tool runs, so a generator failure would destroy the file
# it was asked to reproduce.
tools-matrix: tools-build
	@$(TOOLS_EXE) targets emit > scripts/target-matrix.sh.new
	@chmod 755 scripts/target-matrix.sh.new
	@mv scripts/target-matrix.sh.new scripts/target-matrix.sh

# The equivalence gate: regenerate and require the working tree to stay clean.
# An edit to target.ml that is not reflected in the shell fails here rather than
# at whichever consumer next disagreed - and an edit to the shell alone fails
# here too, which is what makes "do not edit" enforced rather than requested.
tools-matrix-diff: tools-matrix
	@test -z "$$(git status --porcelain -- scripts/target-matrix.sh)" || { \
	  git status --porcelain -- scripts/target-matrix.sh; \
	  echo "scripts/target-matrix.sh is generated - edit tools/lib/target.ml and run 'make tools-matrix'" >&2; \
	  exit 1; }

# Re-record every oracle artifact for all six targets and require the working
# tree to stay clean. It needs the cross BINUTILS but NOT the compiler - the
# oracle reads the committed .s files rather than regenerating them - which is
# why it is a cheap gate (about a second) rather than a toolchain build. `git
# status` is the assertion; the recipe leaves the corpus regenerated either way,
# so a failure can be inspected with an ordinary `git diff`.
#
# In CI this runs in the abi-conform job, NOT the portable matrix: the
# devcontainer Dockerfile installs only the ARM cross set on armhf/armel, so on
# linux/arm/v7 this would fail for a missing tool rather than a byte
# difference. ci below is a local aggregate and assumes a full toolchain.
tools-oracle-diff: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle all
	@test -z "$$(git status --porcelain -- fixtures/gcc-14)" || { \
	  git status --porcelain -- fixtures/gcc-14; \
	  echo "oracle artifacts changed - review the diff above" >&2; exit 1; }

# The same gate for the gas cross-reference, rebuilt from scratch (the command
# deletes the corpus root first) and required to come back identical.
#
# Unlike tools-oracle-diff this DOES need the assembler built, because the
# generated cases come from test/snippets/snippet_emit.exe - by design, so a
# corpus can never be regenerated from an assembler that does not compile.
tools-gasxref-diff: tools-build build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref regen
	@test -z "$$(git status --porcelain -- fixtures/gas-xref)" || { \
	  git status --porcelain -- fixtures/gas-xref; \
	  echo "gas-xref corpus changed - review the diff above" >&2; exit 1; }

# The whole-ISA instruction inventory (docs/isa-inventory.md), RISC-V and x86.
# Toolchain-free (it reads only the vendored vendor/isa-data submodules, never a
# compiler), so unlike tools-gasxref-diff this needs no build edge - only the
# tool itself. Guarded separately from submodules rather than folded into it:
# isa-data is needed only for this track, and submodules also gates plain build,
# which must not start requiring a submodule the assembler itself never reads.
tools-isa-inventory: tools-build
	@test -f vendor/isa-data/riscv-opcodes/upstream/extensions/rv_i || { \
	  echo "vendor/isa-data/riscv-opcodes/upstream is not checked out; run:" >&2; \
	  echo "  git submodule update --init vendor/isa-data/riscv-opcodes/upstream" >&2; \
	  exit 1; }
	@test -d vendor/isa-data/xed/upstream/datafiles/avx || { \
	  echo "vendor/isa-data/xed/upstream is not checked out; run:" >&2; \
	  echo "  git submodule update --init vendor/isa-data/xed/upstream" >&2; \
	  exit 1; }
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-inventory regen

tools-isa-inventory-diff: tools-isa-inventory
	@test -z "$$(git status --porcelain -- fixtures/isa-inventory)" || { \
	  git status --porcelain -- fixtures/isa-inventory; \
	  echo "isa-inventory changed - review the diff above" >&2; exit 1; }

# The isa-db cross-validate check `tools-integration` already runs inside
# repo_tests.exe, exposed standalone for discoverability. Toolchain-free like
# tools-isa-inventory-diff: it reads only checked-in files
# (fixtures/isa-inventory/, isa-db/export/), never the isa-data submodules regen
# needs, so unlike tools-isa-inventory it has no submodule guard.
tools-isa-db-cross-validate: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-inventory cross-validate

# The residual ledger (tools/lib/isa_residual_ledger.ml): prints every source
# family that still has blocked records with its owning row (missing capability,
# evidence, task, reopening gate) and fails if a blocked family is unowned or
# doubly owned, or a row is stale. Toolchain-free - it reads only the checked-in
# exports - and already exercised by tools-integration inside ci.
tools-isa-residual-ledger: tools-build
	RIVET_ROOT=$(CURDIR) $(TOOLS_EXE) isa-inventory residual-ledger

# Producer-only native-capture integrity check. It intentionally stays out of
# ci: it reads the vendored source trees and requires Python, whereas ci's
# portable artifact checks consume only the committed JSONL files.
isa-db-capture-check:
	cd isa-db && python3 -m export.verify

# Control-flow properties of regen, verify and rehash, asserted against the
# OCaml implementation with fake compilers. Needs the executable but no cross
# toolchain.
tools-fixture-modes: tools-build
	scripts/dev/test-fixture-modes.sh

# The execution ABI (docs/exec-abi-v1.md, -v2.md and -v3.md). The dependency
# edges are what enforce a policy: neither helpers nor abi-conform is reachable
# from test or ci, so `make test` can never acquire a hidden cross-toolchain or
# QEMU dependency.
#
# helpers builds the four legacy profiles in v1 and v2 modes, the RISC-V
# profiles in v2 mode only, and - for the legacy profiles the generator already
# supports (x86_64 today) - a v3 helper too. abi-conform executes the four-profile
# v1 regression, the complete six-profile v2 suite, and the v3 suite over
# whatever profiles have a generated v3 helper, all under qemu-user.
helpers:
	scripts/asm-helpers.sh all

runner: build
	opam exec -- dune build test/oracle/conform.exe

# RIVET_HELPERS_DIR is absolute because the driver runs from the build tree.
# run_control's v1 dependency stays independent of the v3 leg deliberately:
# build_one's v1 calls in asm-helpers.sh are unconditional and untouched by the
# v3 addition, so the first leg below never depends on the third having built
# anything.
abi-conform: runner helpers
	RIVET_HELPERS_DIR=$(CURDIR)/.asm-helpers RIVET_ABI_VERSION=1 \
	  opam exec -- dune exec test/oracle/conform.exe
	RIVET_HELPERS_DIR=$(CURDIR)/.asm-helpers RIVET_ABI_VERSION=2 \
	  opam exec -- dune exec test/oracle/conform.exe
	RIVET_HELPERS_DIR=$(CURDIR)/.asm-helpers RIVET_ABI_VERSION=3 \
	  opam exec -- dune exec test/oracle/conform.exe

# The assembler's own image bound at the ABI-v1 profile code base and run under
# the same helpers. Separate from abi-conform on purpose - conform depends on no
# part of the assembler, so a broken assembler cannot make the ABI suite pass,
# and this target is where the assembler is what is on trial. The prerequisites
# are the graph, not a convenience: this is not a claim worth making until the
# ABI holds, and the fixtures this binds are the checked-in ones. Spelling the
# edges out is also what makes the target self-sufficient - it never depends on
# another job having run first, which is what the fresh full-oracle job needs
# and what makes it safe under parallel make.
exec: runner helpers abi-conform fixtures-check
	RIVET_HELPERS_DIR=$(CURDIR)/.asm-helpers \
	  opam exec -- dune exec test/oracle/exec.exe

# What CI runs, and what to run locally before pushing. Formatting is checked
# first: an unformatted tree is the cheapest failure to diagnose. js is not
# here - it needs Melange, hence OCaml 4.14, so it is its own CI job.
ci: fmt-check build test tools-test tools-integration tools-boundary tools-matrix-diff tools-fixture-modes tools-oracle-diff tools-gasxref-diff tools-isa-inventory-diff purity planted melange-optin external-deps js-portable

# The old spellings, for one release.
ASM_ALIASES := build test fmt fmt-check ci melange js js-portable js-browser tool-gate \
  melange-optin fixtures-check fixtures-regen fixture-oracle oracle gas-xref-check \
  gas-xref-regen isa-generated-check isa-generated-regen isa-difficult-check \
  isa-difficult-regen purity planted helpers runner abi-conform exec submodules
ASM_ALIAS_GOALS := $(addprefix asm-,$(ASM_ALIASES)) \
  $(addprefix asm-fixture-oracle-,$(FIXTURE_TARGETS)) $(addprefix asm-fixture-exec-,$(FIXTURE_TARGETS)) \
  $(addprefix asm-fixtures-verify-,$(FIXTURE_TARGETS))

.PHONY: $(ASM_ALIAS_GOALS)
$(addprefix asm-,$(ASM_ALIASES)): asm-%: %
$(addprefix asm-fixture-oracle-,$(FIXTURE_TARGETS)): asm-fixture-oracle-%: fixture-oracle-%
$(addprefix asm-fixture-exec-,$(FIXTURE_TARGETS)): asm-fixture-exec-%: fixture-exec-%
$(addprefix asm-fixtures-verify-,$(FIXTURE_TARGETS)): asm-fixtures-verify-%: fixtures-verify-%

.PHONY: default fmt-ocamlformat submodules build test fmt fmt-check melange js js-portable \
  js-browser native-exec tool-gate melange-optin external-deps fixtures-check fixture-oracle fixtures-regen oracle \
  gas-xref-check gas-xref-regen isa-generated-check isa-generated-regen \
  isa-difficult-check isa-difficult-regen purity planted \
  tools-build tools-test tools-integration tools-boundary tools-fixture-modes \
  tools-oracle-diff tools-gasxref-diff tools-matrix tools-matrix-diff \
  tools-isa-inventory tools-isa-inventory-diff tools-isa-db-cross-validate \
  tools-isa-residual-ledger isa-db-capture-check helpers runner abi-conform exec ci
