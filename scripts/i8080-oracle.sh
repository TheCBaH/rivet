#!/bin/sh
# The i8080 comparisons against the retrocpu oracles, in order.
# usage: i8080-oracle.sh <retrocpu root>      (root printed by retrocpu-fetch.sh)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$1
dune build targets/i8080/tool/i8080_tool.exe
tool=$PWD/_build/default/targets/i8080/tool/i8080_tool.exe
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
echo "== encoding corpus: asl + p2bin against Rivet"
sh "$here/i8080-corpus.sh" "$work/corpus"
sh "$here/i8080-oracle-compare.sh" "$root" "$work/corpus" "$tool" test/i8080/oracle-known.txt
echo "== release sources: asl against Rivet"
sh "$here/i8080-oracle-sources.sh" "$root" "$tool"
