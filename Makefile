COMPCERT_DIR := modules/CompCert
COMPCERT_JOBS := $(shell nproc)
COMPCERT_EXTRACTION_ARCHIVE := compcert-extraction.tar.gz
COMPCERT_LIB_DIR := compcert-lib
COMPCERT_EXPORT_ARCHIVE := compcert-export.tar.gz
COMPCERT_EXPORT_DIR := compcert-export

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
  compcert-export-archive-all
