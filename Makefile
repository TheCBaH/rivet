COMPCERT_DIR := modules/CompCert
COMPCERT_JOBS := $(shell nproc)
COMPCERT_EXTRACTION_ARCHIVE := compcert-extraction.tar.gz
COMPCERT_LIB_DIR := compcert-lib
COMPCERT_EXPORT_ARCHIVE := compcert-export.tar.gz
COMPCERT_EXPORT_DIR := compcert-export
ASM_DIR := asm

default: compcert

# PLATFORM (linux/amd64, linux/i386, linux/arm/v7, ...) is the devcontainer
# action's generic Docker-platform env var, forwarded into the container via
# `devcontainer exec --remote-env`; when set it's preferred over `uname -m`,
# which isn't reliable here: on a 32-bit container running on a 64-bit host
# (e.g. linux/i386 on an amd64 host, linux/arm/v7 on an arm64 host) it
# reports the *host's* 64-bit machine type, not the container's, since
# nothing switches the process's kernel personality just because the
# image/binaries are 32-bit. This has to stay a shell expression run from
# the recipe (as opposed to a $(shell ...)-computed make variable): the case
# statement's bare `pattern)` labels contain unbalanced parentheses, which
# confuses make's own paren matching when used inside $(shell ...).
# COMPCERT_TARGET overrides the PLATFORM/uname autodetection below with an
# explicit target - used by compcert-export-archive-all. Read as a plain
# shell env var (command-line make vars are auto-exported), not a make
# $(if ...): wrapping the case statement in a make function hits the same
# paren-matching confusion as the $(shell ...) gotcha above.
compcert-configure:
	cd $(COMPCERT_DIR) && opam exec -- ./configure $$( \
	  if [ -n "$${COMPCERT_TARGET:-}" ]; then echo "$$COMPCERT_TARGET"; else \
	  case "$$PLATFORM" in \
	  linux/aarch64|linux/arm64) echo aarch64-linux ;; \
	  linux/amd64|linux/x86_64) echo x86_64-linux ;; \
	  linux/i386|linux/386) echo x86_32-linux ;; \
	  linux/arm/v7*|linux/arm/v6*|linux/arm) echo arm-linux ;; \
	  *) m=$$(uname -m); case "$$m" in \
	       aarch64|arm64) echo aarch64-linux ;; \
	       x86_64|amd64) echo x86_64-linux ;; \
	       i686|i386) echo x86_32-linux ;; \
	       armv7l|armv6l|arm) echo arm-linux ;; \
	       *) echo x86_64-linux ;; \
	     esac ;; \
	  esac; fi)

compcert-build:
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) -j$(COMPCERT_JOBS) all

compcert-check-proof:
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) check-proof

compcert-test:
	cd $(COMPCERT_DIR)/test && opam exec -- $(MAKE) all
	cd $(COMPCERT_DIR)/test && opam exec -- $(MAKE) test

compcert: compcert-configure compcert-build

# Split build: run the Rocq-dependent proof + extraction once, archive the
# resulting sources, then let compcert-build-from-archive compile/link
# ccomp from that archive alone, without touching Rocq again (the CI
# workflow proves this by uninstalling rocq-prover before this runs).
# Makefile.config is included because CompCert's own ./configure refuses
# to run without Rocq present, even though its content (compiler paths,
# target arch, ...) has nothing to do with Rocq.
# `depend` has to run before `extraction`: CompCert's source lists (VLIB,
# COMMON, ...) name files without their subdirectory, and it's only the
# generated .depend file that tells make the .vo targets live under lib/,
# common/, etc. Without it, coqc gets invoked on bare filenames and fails to
# find them. CompCert's own `all`/`light` targets get this for free (they
# run `depend` before `proof`/`extraction`); calling `extraction` directly
# skips that, so it's done explicitly here.
compcert-extraction-archive: compcert-configure
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) -j$(COMPCERT_JOBS) depend
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) -j$(COMPCERT_JOBS) extraction
	cd $(COMPCERT_DIR) && tar -czf ../../$(COMPCERT_EXTRACTION_ARCHIVE) \
	  Makefile.config extraction/*.ml extraction/*.mli

compcert-extraction-unarchive:
	tar -xzf $(COMPCERT_EXTRACTION_ARCHIVE) -C $(COMPCERT_DIR)

# Everything ccomp's OCaml side needs to compile, besides the extraction
# archive itself: the runtime config (compcert.ini/driver/Version.ml, both
# cheap shell substitutions - no Rocq), tools/modorder, and the
# Menhir/ocamllex-generated parser files, pulled in as a side effect of
# computing .depend.extr.
# tools/modorder (plain OCaml, no Rocq) is what turns .depend.extr into
# ccomp's link order; Makefile.extr computes CCOMP_OBJS from it with
# $(shell ...), so when it is missing the object list comes out *empty* and
# `make -f Makefile.extr ccomp` cheerfully links an executable with no
# modules in it - which then exits 0 on any argument, so even a -version
# smoke test can't tell. CompCert's own `all` builds it via .depend.extr's
# prerequisites; the split build calls Makefile.extr directly and has to ask
# for it explicitly.
compcert-prepare-sources: compcert-extraction-unarchive
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) compcert.ini driver/Version.ml tools/modorder
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) -f Makefile.extr depend

compcert-build-from-archive: compcert-prepare-sources
	cd $(COMPCERT_DIR) && opam exec -- $(MAKE) -f Makefile.extr ccomp

# Alternative to compcert-build-from-archive: instead of compiling/linking
# ccomp with CompCert's own hand-rolled ocamlc/ocamlopt+modorder Makefile,
# copy the same hand-written + generated sources out into compcert-lib/ (a
# standalone dune project, see compcert-lib/src/dune) and let dune resolve
# module dependencies and link a library + our own bin/main.ml, so CompCert
# can be depended on like an ordinary OCaml library. driver/Driver.ml is
# excluded from the library and copied to bin/main.ml instead, since it's
# ccomp's CLI entry point, not library code (see it for the ~3-call
# sequence - Frontend.parse_c_file / Compiler.transf_c_program / print -
# a from-scratch main.ml would drive the library the same way).
# The module set comes from tools/modorder, i.e. exactly what CompCert links
# into ccomp, rather than everything found in the source directories: those
# also hold sources ccomp's own build never compiles (clightgen's export/*,
# cparser/GCC.ml, backend/Json*.ml, ...), and dune compiles every module in
# the directory, so some of them simply don't build (cparser/GCC.mli still
# refers to a long-gone Builtins.t). modorder emits .cmx paths in link
# order; the matching .ml (plus its .mli, when there is one) is what gets
# copied, minus driver/Driver.ml, which becomes bin/main.ml.
compcert-lib-sync: compcert-prepare-sources
	rm -f $(COMPCERT_LIB_DIR)/src/*.ml $(COMPCERT_LIB_DIR)/src/*.mli
	mkdir -p $(COMPCERT_LIB_DIR)/src $(COMPCERT_LIB_DIR)/bin
	cd $(COMPCERT_DIR) && objs=$$(tools/modorder .depend.extr driver/Driver.cmx) && \
	if [ -z "$$objs" ]; then echo "modorder listed no modules" >&2; exit 1; fi; \
	for o in $$objs; do \
	  src=$${o%.cmx}.ml; \
	  if [ "$$src" != driver/Driver.ml ]; then \
	    cp "$$src" $(CURDIR)/$(COMPCERT_LIB_DIR)/src/; \
	    if [ -f "$${src}i" ]; then cp "$${src}i" $(CURDIR)/$(COMPCERT_LIB_DIR)/src/; fi; \
	  fi; \
	done
	# Interface-only modules (cparser/C.mli, debug/D*Types.mli) have no .cmx,
	# so modorder never names them, but the modules above won't compile
	# without them - see also src/dune's modules_without_implementation.
	arch=$$(grep '^ARCH=' $(COMPCERT_DIR)/Makefile.config | cut -d= -f2); \
	for d in extraction lib common $$arch backend cfrontend cparser debug driver; do \
	  for f in $(COMPCERT_DIR)/$$d/*.mli; do \
	    [ -e "$$f" ] || continue; \
	    [ -e "$${f%.mli}.ml" ] || cp $$f $(COMPCERT_LIB_DIR)/src/; \
	  done; \
	done
	cp $(COMPCERT_DIR)/driver/Driver.ml $(COMPCERT_LIB_DIR)/bin/main.ml

compcert-lib-build: compcert-lib-sync
	cd $(COMPCERT_LIB_DIR) && opam exec -- dune build

# ccomp looks for compcert.ini next to its own executable (or via
# COMPCERT_CONFIG/-conf), which compcert-lib/_build isn't, so point it at
# the one compcert-prepare-sources generated in $(COMPCERT_DIR) instead of
# copying it into the dune tree.
compcert-lib-run: compcert-lib-build
	cd $(COMPCERT_LIB_DIR) && COMPCERT_CONFIG=$(CURDIR)/$(COMPCERT_DIR)/compcert.ini \
	  opam exec -- dune exec bin/main.exe -- $(ARGS)

# Redistributable package for consumers who just want to `dune build`
# CompCert as a library, without cloning this repo or touching Rocq: the
# same dune-project/src/bin tree compcert-lib-sync populates, plus
# CompCert's own LICENSE (this project's top-level LICENSE only covers the
# devcontainer/build tooling, not the CompCert sources being redistributed).
# Staged into a throwaway directory first since `tar --append` doesn't work
# on an already-gzipped archive.
compcert-export-archive: compcert-lib-sync
	rm -rf $(COMPCERT_EXPORT_DIR)
	mkdir -p $(COMPCERT_EXPORT_DIR)
	cp $(COMPCERT_LIB_DIR)/dune-project $(COMPCERT_EXPORT_DIR)/
	cp -r $(COMPCERT_LIB_DIR)/src $(COMPCERT_EXPORT_DIR)/src
	cp -r $(COMPCERT_LIB_DIR)/bin $(COMPCERT_EXPORT_DIR)/bin
	rm -f $(COMPCERT_EXPORT_DIR)/src/.gitignore $(COMPCERT_EXPORT_DIR)/bin/.gitignore
	cp $(COMPCERT_DIR)/LICENSE $(COMPCERT_EXPORT_DIR)/LICENSE
	tar -czf $(COMPCERT_EXPORT_ARCHIVE) -C $(COMPCERT_EXPORT_DIR) .
	rm -rf $(COMPCERT_EXPORT_DIR)

compcert-export-unarchive:
	rm -rf $(COMPCERT_EXPORT_DIR)
	mkdir -p $(COMPCERT_EXPORT_DIR)
	tar -xzf $(COMPCERT_EXPORT_ARCHIVE) -C $(COMPCERT_EXPORT_DIR)

# Test fixture for the exported package: builds straight from the unpacked
# archive's own dune-project/src/bin, instead of compcert-lib's live-synced
# tree, so CI proves the exact dune file being published is buildable.
compcert-export-build: compcert-export-unarchive
	cd $(COMPCERT_EXPORT_DIR) && opam exec -- dune build

compcert-export-run: compcert-export-build
	cd $(COMPCERT_EXPORT_DIR) && COMPCERT_CONFIG=$(CURDIR)/$(COMPCERT_DIR)/compcert.ini \
	  opam exec -- dune exec bin/main.exe -- $(ARGS)

# The retargetable assembler (asm/), a standalone dune project independent of
# the CompCert build above: it has its own dune-project, so `dune` run from
# asm/ never descends into compcert-lib/ and none of these targets need Rocq,
# a cross toolchain, or QEMU. See .ai/asm_plan.md.
#
# asm-fmt rewrites sources in place; asm-fmt-check is the CI form, which fails
# with a diff instead. Both go through dune's @fmt alias, so the vendored
# sources under asm/vendor (marked (vendored_dirs) in asm/dune) are skipped and
# stay byte-identical to upstream.

# err_trace is vendored as a submodule and its sources are copy_files'd into
# the build by asm/vendor/err_trace_local/dune, so an uninitialized submodule is a
# build failure - and an unhelpful one, since dune reports it as a missing rule
# for a path rather than as a missing checkout. Checked here rather than in
# asm-ci so that a bare `make asm-build` gets the same answer.
asm-submodules:
	@test -f $(ASM_DIR)/vendor/err_trace/src/err.ml || { \
	  echo "$(ASM_DIR)/vendor/err_trace is not checked out; run:" >&2; \
	  echo "  git submodule update --init $(ASM_DIR)/vendor/err_trace" >&2; \
	  exit 1; }

asm-build: asm-submodules
	cd $(ASM_DIR) && opam exec -- dune build @all

asm-test: asm-build asm-fixtures-check asm-gas-xref-check
	cd $(ASM_DIR) && opam exec -- dune build @runtest

asm-fmt:
	cd $(ASM_DIR) && opam exec -- dune build @fmt --auto-promote

asm-fmt-check:
	cd $(ASM_DIR) && opam exec -- dune build @fmt

# The Melange configuration. Declaring melange mode on a library schedules melc
# in @all whether or not anything emits JavaScript, so it is gated on
# ASM_MELANGE and the two configurations are built separately (asm/melange/dune
# explains why dune leaves no better option). This target needs Melange
# installed and therefore an OCaml 4.14 switch; it is not part of asm-ci.
asm-melange:
	cd $(ASM_DIR) && ASM_MELANGE=true opam exec -- dune build @all

# Byte-identical output from native OCaml, js_of_ocaml/Node, and Melange/Node
# (.ai/asm_plan.md §11.6). All three run the *same* committed cram baseline,
# asm/test/cram/bigint_dump.t, with ASM_BACKEND choosing which artifact produces
# it - so equality is the cram diff itself rather than a comparison between two
# uncommitted outputs. Separate dune invocations because ASM_MELANGE changes
# which library stanzas are enabled.
asm-js: asm-build asm-js-portable
	cd $(ASM_DIR) && ASM_BACKEND=melange ASM_MELANGE=true opam exec -- dune build @runtest

# The two legs that need no Melange, split out so the portable CI matrix can run
# them on every image. Melange 7.0.1-414 requires `ocaml >= 4.14 & < 4.15`, so
# the third leg cannot run on the OCaml 5.x images the matrix also builds - but
# js_of_ocaml can, and leaving it out of the matrix entirely would mean the only
# evidence that the closure compiles to JavaScript came from one pinned leg.
asm-js-portable: asm-build
	cd $(ASM_DIR) && ASM_BACKEND=native opam exec -- dune build @runtest
	cd $(ASM_DIR) && ASM_BACKEND=jsoo opam exec -- dune build @runtest

# The behavioral tool gate (§3.2, an M0 exit criterion). APT tooling is
# range-checked rather than digest-pinned, and version numbers alone do not
# establish compatibility where behaviour matters - so this asks the tools to do
# the things the project depends on and records what answered. It needs the
# cross toolchains, all six qemu-user binaries, qemu-system and gdb, but no
# CompCert and no part of the assembler: it establishes E2 tool compatibility
# only, and asm-abi-conform remains the distinct E4 transport proof.
asm-tool-gate: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) tool-gate all

# The Melange opt-in, verified rather than asserted (§3.2). Checks both clean
# configurations: zero melc rules and a successful build with Melange
# unavailable, and the rules reappearing under ASM_MELANGE=true. Needs nothing
# beyond dune, so it runs on every leg.
asm-melange-optin:
	tools/asm-melange-optin.sh

# The M1 fixtures. §12 has a two-mode policy and the dependency edges here are
# what enforce it: asm-fixtures-check needs NO cross toolchain, so every
# ordinary test run and every portable CI leg consumes the checked-in bytes.
# Only asm-fixtures-regen sits downstream of asm-cross-setup, which builds all
# six CompCert installations and is far too expensive to put on the path of
# `make asm-test`.
# Repointed at the native executable (tool.md §11 Phase 2). The SCRIPT is
# unchanged and remains the public shell implementation until Phase 3's
# cutover; only this Make edge moves.
#
# This target now needs a dune build where it previously needed nothing but
# bash and coreutils - divergence D2, and it lands on every platform leg
# including the emulated linux/i386 and linux/arm/v7. It is not one of gate 6's
# four forbidden dependencies, but it is real, and it is why tools-build is
# TARGETED: `dune build @all` here would put an entire assembler build behind a
# toolchain-free check.
asm-fixtures-check: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture check

# The fixture profiles come from tools/target-matrix.sh rather than a copy of
# the list here. $(shell) does not fail the build when its command fails, so
# both the status and the cardinality are asserted: an empty or short set would
# otherwise make the aggregate goal below run zero targets and report success.
FIXTURE_TARGETS := $(shell tools/target-matrix.sh fixture)
ifneq ($(.SHELLSTATUS),0)
$(error tools/target-matrix.sh fixture failed)
endif
ifneq ($(words $(FIXTURE_TARGETS)),6)
$(error expected 6 fixture targets, got '$(FIXTURE_TARGETS)')
endif

FIXTURE_SETUP_GOALS  := $(addprefix asm-fixture-setup-,$(FIXTURE_TARGETS))
FIXTURE_VERIFY_GOALS := $(addprefix asm-fixtures-verify-,$(FIXTURE_TARGETS))
FIXTURE_EXEC_GOALS   := $(addprefix asm-fixture-exec-,$(FIXTURE_TARGETS))
FIXTURE_ORACLE_GOALS := $(addprefix asm-fixture-oracle-,$(FIXTURE_TARGETS))

# Static pattern rules, not `%` implicit rules. GNU Make skips implicit rule
# search for .PHONY targets, so an implicit pattern plus a phony expansion
# yields "Nothing to be done" and exit 0 - a silent no-op. Static pattern rules
# are explicit rules, so .PHONY does what it is meant to here.
.PHONY: $(FIXTURE_SETUP_GOALS)
$(FIXTURE_SETUP_GOALS): asm-fixture-setup-%:
	tools/compcert-fixture-setup.sh $*

# tools-build on every goal that reaches $(TOOLS_EXE), which since Phase 8 is
# all of them: the shell implementations are gone and there is nothing else to
# run. Without the edge the recipe would invoke a path that does not exist.
#
# asm-fixture-oracle-% is the one that matters most: the fixture-oracle CI job
# runs it alone in a fresh checkout, with no prior job and no edge to
# asm-fixtures-check, so it cannot inherit the build from anywhere else.
.PHONY: $(FIXTURE_VERIFY_GOALS)
$(FIXTURE_VERIFY_GOALS): asm-fixtures-verify-%: tools-build asm-fixture-setup-%
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture verify -- $*

.PHONY: $(FIXTURE_EXEC_GOALS)
$(FIXTURE_EXEC_GOALS): asm-fixture-exec-%: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture exec -- $*

# One target's complete oracle leg, in the one order that makes the evidence
# chain hold: strict profile, exact regeneration, GNU oracle, QEMU execution,
# manifest completeness, clean tree. CI runs this goal rather than restating
# the sequence, so local and CI ordering have a single definition.
#
# --check before git diff is load-bearing: git diff cannot see an untracked
# file, so a newly produced oracle artifact would leave the leg reporting clean.
# --check reports it as UNRECORDED. git diff stays because the manifest excludes
# itself from its own hashes.
.PHONY: $(FIXTURE_ORACLE_GOALS)
$(FIXTURE_ORACLE_GOALS): asm-fixture-oracle-%: tools-build
	tools/compcert-fixture-setup.sh $*
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture verify -- $*
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle -- $*
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture exec -- $*
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture check
	git diff --exit-code -- asm/fixtures/compcert-3.17

asm-fixture-oracle:
	@for t in $(FIXTURE_TARGETS); do $(MAKE) asm-fixture-oracle-$$t || exit 1; done

# Six freestanding fixture compilers under .fixture-work. The libc cross-smoke
# suite owns .cross-smoke-work and is no longer on this path: fixture work is
# freestanding, so requiring a libc here would only import skips.
asm-cross-setup:
	tools/compcert-fixture-setup.sh all

asm-libc-cross-smoke:
	tools/compcert-cross-smoke.sh all

# The suite's OK/FAIL/SKIP result, asserted without building anything. SKIP is
# the state that matters and the one nothing tested: a target whose compiler or
# emulator is absent is skipped rather than failed, and the run still exits 0,
# which is what lets the suite pass on a 32-bit host. Both skip checks run
# before any build, so this takes well under a second.
#
# It needs the x86_32 cross gcc and qemu-i386 in order to reach the FAIL and
# emulator-absent branches, so - like tools-oracle-diff - it runs in a job with
# the full cross toolchain rather than in the portable matrix, where on
# linux/arm/v7 it would fail for a missing tool rather than for a defect.
asm-cross-smoke-selftest:
	tools/dev/test-cross-smoke-tristate.sh

# A regeneration difference is a reviewed failure, not a refresh: it can change
# accepted syntax, relocations, or instruction coverage, i.e. the M1 scope.
asm-fixtures-regen: tools-build asm-cross-setup
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture regen

# The reference-assembler artifacts M1.5 compares our encoder against. --rehash
# folds them into the same manifest, so asm-fixtures-check covers them too.
asm-oracle: tools-build asm-fixtures-regen
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle -- all
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture rehash

# The GNU as cross-reference. Same two-mode policy as the fixtures above and
# for the same reason, so the edges are the same shape: --check hashes the
# committed corpus and needs no toolchain, --regen needs the six cross
# binutils. It is NOT downstream of asm-cross-setup, unlike asm-fixtures-regen:
# the inputs are generated from this project's own AST corpus rather than
# compiled by CompCert, so binutils alone is the whole requirement.
asm-gas-xref-check: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref check

asm-gas-xref-regen: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref regen

# The transitive purity and layer audits (§1, §2.2, §3.7, §5.1), and the
# planted violations that prove they can fail. Guardrail 6: run these before
# treating a successful JavaScript build as evidence of portability - a package
# shipping a JS runtime replacement for its C primitives compiles cleanly and
# still breaks the no-C rule.
asm-purity: asm-build
	tools/asm-check-purity.sh
	tools/asm-check-layers.sh

asm-planted: asm-build
	tools/asm-check-planted.sh

# Control-flow tests for asm-fixture-gen.sh --regen, using fake compilers in a
# throwaway tree. No toolchain, no corpus and no build: this belongs on the
# cheap side of the two-mode policy, alongside asm-fixtures-check, which is why
# it takes no prerequisite.
# {1 The OCaml tool project (tool.md §5)}

TOOLS_EXE   := $(CURDIR)/$(ASM_DIR)/_build/default/tools/bin/compcert_tools.exe
TOOLS_ITEST := $(CURDIR)/$(ASM_DIR)/_build/default/tools/test/repo/repo_tests.exe

# TARGETED, never @all: `cd asm && dune build @all` would drag the whole
# assembler in, so every check-only target that later gains a tools-build edge
# would transitively require an assembler build - exactly what tool.md gate 6
# forbids and what preserves §5's third property. `make -n` asserts this.
#
# asm-submodules because the tool project copy_files# its err_trace sources from
# the submodule, and without the guard dune reports a missing RULE for a path
# rather than a missing checkout (see the comment on asm-submodules itself).
tools-build: asm-submodules
	cd $(ASM_DIR) && opam exec -- dune build tools/bin/compcert_tools.exe

tools-test: asm-submodules
	cd $(ASM_DIR) && opam exec -- dune build @tools/runtest

# The repository-level suite is RUN BY MAKE, not by a dune runtest rule: it
# reads Makefile, tools/target-matrix.sh and tools/dev/dump-target-config.sh,
# all of which are outside the asm/ dune workspace, so a sandboxed action would
# be depending on undeclared inputs. The root is passed as argv, which is also
# why %{workspace_root} - asm/, not the repository root - never enters.
tools-integration: asm-submodules
	cd $(ASM_DIR) && opam exec -- dune build tools/test/repo/repo_tests.exe
	$(TOOLS_ITEST) $(CURDIR)

# A cold targeted build into a private build directory, then the resolved
# library closure of exactly the compcert_tools executable. It is on the CI path
# because it is called load-bearing throughout: it is what keeps
# asm-fixtures-check free of an assembler build. Its planted cases run in copied
# trees, so no ordering constraint against the ordinary _build is needed.
tools-boundary: asm-submodules
	tools/dev/check-tool-build-boundary.sh

# tools/target-matrix.sh is a GENERATED file: Target owns the sixteen values and
# this renders them for the shell consumers that drive ./configure, make and
# linkhier. Generating rather than having those scripts query the executable is
# what keeps compcert-fixture-setup.sh and asm-helpers.sh free of a dune build -
# they are the first thing their CI jobs run.
#
# Staged and moved rather than redirected in place: `> tools/target-matrix.sh`
# truncates before the tool runs, so a generator failure would destroy the file
# it was asked to reproduce.
tools-matrix: tools-build
	@$(TOOLS_EXE) targets emit > tools/target-matrix.sh.new
	@chmod 755 tools/target-matrix.sh.new
	@mv tools/target-matrix.sh.new tools/target-matrix.sh

# The equivalence gate, in the pattern tools-oracle-diff established: regenerate
# and require the working tree to stay clean. An edit to target.ml that is not
# reflected in the shell fails here rather than at whichever consumer next
# disagreed - and an edit to the shell alone fails here too, which is what makes
# "do not edit" enforced rather than requested.
tools-matrix-diff: tools-matrix
	@test -z "$$(git status --porcelain -- tools/target-matrix.sh)" || { \
	  git status --porcelain -- tools/target-matrix.sh; \
	  echo "tools/target-matrix.sh is generated - edit asm/tools/lib/target.ml and run 'make tools-matrix'" >&2; \
	  exit 1; }

# Phase 4's artifact gate, and the strongest one in the migration: re-record
# every oracle artifact for all six targets with the OCaml implementation and
# require the working tree to stay clean. 294 committed files, produced by the
# AWK this phase replaces.
#
# It needs the cross BINUTILS but NOT CompCert - the oracle reads the committed
# .s files rather than regenerating them - which is why it is a cheap gate
# (about a second) rather than a toolchain build. `git diff --exit-code` is the
# assertion; the recipe leaves the corpus regenerated either way, so a failure
# can be inspected with an ordinary `git diff`.
#
# In CI this runs in the abi-conform job, NOT the portable asm matrix: the
# devcontainer Dockerfile installs only the ARM cross set on armhf/armel, so on
# linux/arm/v7 this would fail for a missing tool rather than a byte
# difference. asm-ci below is a local aggregate and assumes a full toolchain.
tools-oracle-diff: tools-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) fixture oracle all
	@test -z "$$(git status --porcelain -- asm/fixtures/compcert-3.17)" || { \
	  git status --porcelain -- asm/fixtures/compcert-3.17; \
	  echo "oracle artifacts changed - review the diff above" >&2; exit 1; }

# The same gate for the gas cross-reference: 463 committed files, rebuilt from
# scratch (the command deletes the corpus root first) and required to come back
# identical.
#
# Unlike tools-oracle-diff this DOES need the assembler built, because the
# generated cases come from asm/test/snippets/snippet_emit.exe - by design, so
# a corpus can never be regenerated from an assembler that does not compile.
# Hence the asm-build edge, which tools-oracle-diff deliberately lacks.
tools-gasxref-diff: tools-build asm-build
	COMPCERT_REPO_ROOT=$(CURDIR) $(TOOLS_EXE) gas-xref regen
	@test -z "$$(git status --porcelain -- asm/fixtures/gas-xref)" || { \
	  git status --porcelain -- asm/fixtures/gas-xref; \
	  echo "gas-xref corpus changed - review the diff above" >&2; exit 1; }

# The same control-flow properties, asserted against the OCaml implementation
# with fake compilers. Needs the executable but no cross toolchain.
tools-fixture-modes: tools-build
	tools/dev/test-fixture-modes.sh

# Re-run the recorded shell behavior and compare. This is the migration's
# reference: the OCaml replacements are held to these snapshots, so a change to
# a shell entry point has to show up here before it can be silently ported.
# Also toolchain-free, and it runs against a detached worktree of HEAD rather
# than the working checkout, so nothing it does can touch the real corpus.
asm-characterize-verify:
	tools/dev/characterize.sh verify

# The execution ABI (asm/docs/exec-abi-v1.md and exec-abi-v2.md). The
# dependency edges are again what enforce a policy: neither asm-helpers nor
# asm-abi-conform is reachable from asm-test or asm-ci, so `make asm-test` can
# never acquire a hidden cross-toolchain or QEMU dependency (guardrail 4).
#
# asm-helpers builds the four legacy profiles in v1 and v2 modes, the RISC-V
# profiles in v2 mode only, and - for the legacy profiles the generator
# already supports (.ai/asm_plan.md M4 Phase 3.4: x86_64 today) - a v3
# helper too. asm-abi-conform executes the four-profile v1 regression, the
# complete six-profile v2 suite, and the v3 suite over whatever profiles have
# a generated v3 helper, all under qemu-user.
asm-helpers:
	tools/asm-helpers.sh all

asm-runner: asm-build
	cd $(ASM_DIR) && opam exec -- dune build test/oracle/conform.exe

# ASM_HELPERS_DIR is absolute because the driver runs from the build tree.
# run_control's v1 dependency stays independent of the v3 leg deliberately:
# build_one's v1 calls in asm-helpers.sh are unconditional and untouched by
# the v3 addition, so the first leg below never depends on the third having
# built anything.
asm-abi-conform: asm-runner asm-helpers
	cd $(ASM_DIR) && ASM_HELPERS_DIR=$(CURDIR)/.asm-helpers ASM_ABI_VERSION=1 \
	  opam exec -- dune exec test/oracle/conform.exe
	cd $(ASM_DIR) && ASM_HELPERS_DIR=$(CURDIR)/.asm-helpers ASM_ABI_VERSION=2 \
	  opam exec -- dune exec test/oracle/conform.exe
	cd $(ASM_DIR) && ASM_HELPERS_DIR=$(CURDIR)/.asm-helpers ASM_ABI_VERSION=3 \
	  opam exec -- dune exec test/oracle/conform.exe

# M1.6, the E5 rung: the assembler's own image bound at the ABI-v1 profile code
# base and run under the same helpers. Separate from asm-abi-conform on purpose -
# conform depends on no part of the assembler, so a broken assembler cannot make
# the ABI suite pass, and this target is where the assembler is what is on trial.
# The prerequisites are the plan's graph, not a convenience: E5 is not a claim
# worth making until E4 holds, and the fixtures this binds are the checked-in
# ones. Spelling the edges out is also what makes the target self-sufficient -
# it never depends on another job having run first, which is what the fresh
# full-oracle job needs and what makes it safe under parallel make.
asm-exec: asm-runner asm-helpers asm-abi-conform asm-fixtures-check
	cd $(ASM_DIR) && ASM_HELPERS_DIR=$(CURDIR)/.asm-helpers \
	  opam exec -- dune exec test/oracle/exec.exe

# What CI runs, and what to run locally before pushing. Formatting is checked
# first: an unformatted tree is the cheapest failure to diagnose. asm-js is not
# here — it needs Melange, hence OCaml 4.14, so it is its own CI job.
asm-ci: asm-fmt-check asm-build asm-test tools-test tools-integration tools-boundary tools-matrix-diff tools-fixture-modes tools-oracle-diff tools-gasxref-diff asm-purity asm-planted asm-cross-smoke-selftest asm-characterize-verify asm-melange-optin asm-js-portable

# One archive per target: Rocq extraction differs per architecture, so
# compcert-export-archive alone only covers whichever target was last
# configured. See tools/compcert-export-archive-all.sh.
compcert-export-archive-all:
	tools/compcert-export-archive-all.sh

.PHONY: default \
  compcert compcert-configure compcert-build compcert-check-proof compcert-test \
  compcert-extraction-archive compcert-extraction-unarchive compcert-prepare-sources \
  compcert-build-from-archive compcert-lib-sync compcert-lib-build compcert-lib-run \
  compcert-export-archive compcert-export-unarchive compcert-export-build compcert-export-run \
  compcert-export-archive-all \
  asm-submodules asm-build asm-test asm-fmt asm-fmt-check asm-melange asm-js asm-purity asm-planted \
  tools-build tools-test tools-integration tools-boundary tools-fixture-modes tools-oracle-diff tools-gasxref-diff tools-matrix tools-matrix-diff \
  asm-fixtures-check asm-characterize-verify asm-cross-setup asm-libc-cross-smoke asm-cross-smoke-selftest asm-fixtures-regen \
  asm-oracle asm-fixture-oracle asm-ci \
  asm-gas-xref-check asm-gas-xref-regen \
  asm-helpers asm-runner asm-abi-conform asm-exec asm-tool-gate asm-melange-optin \
  asm-js-portable
