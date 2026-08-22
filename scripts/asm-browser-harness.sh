#!/usr/bin/env bash
# The real-browser smoke harness (.ai/asm_plan.md M5, the deferred
# browser-harness item deferred at M0 §11.6 "until the image APIs and
# broader assembler slices have stabilized" - M4's close is that
# stabilization point). Proves that the same parser+encoder and
# linker+multi-segment-image entry points asm_dump.ml/manifest_dump.ml
# already prove three-way-identical under Node also agree inside a real
# browser JS engine, with native OCaml's output as the one baseline
# authority every other three-build gate in this project already uses.
#
# Provisioning happens here, at run time, under whichever user invokes this
# script - never baked into .devcontainer/Dockerfile. The default devcontainer
# build context has no COPY, so asm/test/browser/package.json cannot exist
# inside a Dockerfile RUN step, and a root-built image would put Chromium in a
# cache path the runtime user (vscode locally, a different user in CI) cannot
# see. PLAYWRIGHT_BROWSERS_PATH is set to a repo-local, explicit directory so
# install time and launch time agree by construction regardless of who runs
# this.
#
# Explicit Dune build targets throughout, not alias closure: asm-js (this
# script's Makefile prerequisite) builds @runtest, and neither that nor
# asm-melange's @all is guaranteed to contain smoke_native.exe/
# smoke_jsoo.bc.js/the Melange smoke module as an incidental member.
set -euo pipefail
cd "$(dirname "$0")/.."
ASM_DIR=${ASM_DIR:-asm}
BROWSER_DIR="$ASM_DIR/test/browser"
REPO_ROOT="$(pwd)"

note() { printf '  %-46s %s\n' "$1" "$2"; }
fail() {
  printf '  FAIL %-41s %s\n' "$1" "$2"
  exit 1
}

# --- B.1: Playwright/Chromium provisioning, repo-local and run-time only ---

export PLAYWRIGHT_BROWSERS_PATH="$REPO_ROOT/.playwright-browsers"
note "PLAYWRIGHT_BROWSERS_PATH" "$PLAYWRIGHT_BROWSERS_PATH"

( cd "$BROWSER_DIR" && npm ci ) || fail "npm ci" "failed"
note "npm ci" "ok"

( cd "$BROWSER_DIR" && npx playwright install --with-deps chromium ) \
  || fail "playwright install chromium" "failed"
note "playwright install chromium" "ok"

# --- explicit build targets (not @all/@runtest alias closure) ---

( cd "$ASM_DIR" && opam exec -- dune build ./test/browser/smoke_native.exe ./test/browser/smoke_jsoo.bc.js ) \
  || fail "dune build (native+jsoo smoke)" "failed"
note "dune build (native+jsoo smoke)" "ok"

MELANGE_SMOKE_JS="$ASM_DIR/_build/default/melange/test/browser/js/melange/test/browser/smoke.js"
( cd "$ASM_DIR" && ASM_MELANGE=true opam exec -- dune build ./melange/test/browser/ ) \
  || fail "dune build (melange smoke)" "failed"
[ -f "$MELANGE_SMOKE_JS" ] || fail "melange smoke output" "not found at $MELANGE_SMOKE_JS"
note "dune build (melange smoke)" "ok"

# --- B.5: generated, gitignored work directory - the artifact hand-off ---
#
# The three build artifacts live in three different _build trees and cannot
# be referenced by relative <script src="..."> paths from a static file
# sitting in the source tree, so they are staged as siblings here before
# Playwright ever navigates to harness.html.

WORK_DIR="$BROWSER_DIR/_work"
rm -rf -- "$WORK_DIR"
mkdir -p "$WORK_DIR"

# The native baseline, captured rather than hand-embedded in HTML source (a
# hand-embedded value would silently drift from the real native output).
# JSON.stringify runs in Node, not the shell, so the captured stdout stays on
# stdin end-to-end instead of being shell-interpolated (which would corrupt
# quotes/newlines/backslashes in a future summary string).
"$ASM_DIR/_build/default/test/browser/smoke_native.exe" | node -e '
  const data = require("fs").readFileSync(0, "utf8");
  process.stdout.write("window.__ASM_SMOKE_BASELINE__ = " + JSON.stringify(data) + ";\n");
' > "$WORK_DIR/baseline.js"
note "native baseline" "captured to $WORK_DIR/baseline.js"

cp "$ASM_DIR/_build/default/test/browser/smoke_jsoo.bc.js" "$WORK_DIR/smoke_jsoo.bc.js"
note "jsoo bundle" "staged"

# --- B.4: Melange CommonJS -> browser-loadable bundle, done here (not a Dune
# rule: a Dune action's working directory is that rule's own _build
# counterpart, not the repo root, so a bundling rule declared inside
# asm/melange/test/browser/dune cannot resolve --prefix or name its input by
# a simple relative path). IIFE format so it loads via a plain
# <script src="..."> under a file:// page with no CORS/module-resolution
# friction.
( cd "$BROWSER_DIR" && npx esbuild "$REPO_ROOT/$MELANGE_SMOKE_JS" \
    --bundle --platform=browser --format=iife --global-name=AsmSmokeMelange \
    --outfile="$REPO_ROOT/$WORK_DIR/smoke_melange.bundle.js" ) \
  || fail "esbuild (melange bundle)" "failed"
note "melange bundle" "staged"

cp "$BROWSER_DIR/harness.html" "$WORK_DIR/harness.html"
note "harness.html" "staged"

# --- the driver: browser process lifecycle only; all assembler logic and the
# jsoo/melange-vs-native comparison run inside the real browser page ---

node "$BROWSER_DIR/run_harness.mjs" "$WORK_DIR/harness.html"
