#!/bin/sh
# Policy 9 (docs/namespacing.md): every unit Rivet installs, or builds for
# Melange, is Rivet_<x>. One configuration per run:
#
#   namespace-check-rivet.sh standalone|opam|native-exec|melange [DIR]
#
# Without DIR the configuration is built first (standalone: @install;
# opam: -p rivet with RIVET_EXTERNAL_DEPS in its own build directory;
# native-exec: @install with RIVET_NATIVE_EXEC; melange: the full mirror
# build) and its tree is checked; with DIR an installed tree is checked
# as it is (the opam workflow). The expected libraries and the exceptions
# are scripts/namespace/.
set -eu
cd "$(dirname "$0")/.."
cfg=${1:?configuration}
dir=${2:-}
rules=scripts/namespace

check() { # prefix inventories...
  prefix=$1
  shift
  sh scripts/namespace-check.sh --fail -q --prefix "$prefix" \
    --exceptions "$rules/exceptions" --expect-libs "$rules/expect" \
    --ocamlobjinfo "opam exec -- ocamlobjinfo" "$@"
}

case $cfg in
  standalone)
    [ -n "$dir" ] || { opam exec -- dune build @install; dir=_build/install/default/lib/rivet; }
    check Rivet "standalone=$dir" ;;
  opam)
    if [ -z "$dir" ]; then
      trap 'rm -rf _build-opam' EXIT
      RIVET_EXTERNAL_DEPS=true opam exec -- dune build -p rivet --build-dir _build-opam --promote-install-files=false @install
      dir=_build-opam/install/default/lib/rivet
    fi
    check Rivet "opam=$dir" ;;
  native-exec)
    if [ -z "$dir" ]; then
      RIVET_NATIVE_EXEC=true opam exec -- dune build @install
      dir=_build/install/default/lib/rivet-native-exec
    fi
    check Rivet_native_exec "native-exec=$dir" ;;
  melange)
    # Melange installs nothing: each mirror library's .objs/melange is its
    # inventory, staged under the library's name. The unpublished mirrors
    # (browser_jsoo, browser_melange, smoke_fixtures, foundation_corpus) are
    # linked only into Rivet's own programs and are not checked.
    [ -z "$dir" ] || { echo "melange takes no DIR" >&2; exit 2; }
    RIVET_MELANGE=true opam exec -- dune build @all
    stage=$(mktemp -d)
    trap 'rm -rf "$stage"' EXIT
    for objs in $(find _build/default/melange -type d -path '*.objs/melange'); do
      lib=${objs%.objs/melange}
      lib=${lib##*/.}
      case $lib in
        browser_jsoo | browser_melange | smoke_fixtures | foundation_corpus) continue ;;
      esac
      ln -s "$(pwd)/$objs" "$stage/$lib"
    done
    check Rivet "melange=$stage" ;;
  *) echo "unknown configuration: $cfg" >&2; exit 2 ;;
esac
