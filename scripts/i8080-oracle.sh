#!/bin/sh
# The i8080 comparisons against the retrocpu oracles, in order.
# usage: i8080-oracle.sh <retrocpu root>      (root printed by retrocpu-fetch.sh)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$1
opam exec -- dune build targets/i8080/tool/i8080_tool.exe
tool=$PWD/_build/default/targets/i8080/tool/i8080_tool.exe
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
echo "== encoding corpus: asl + p2bin against Rivet"
sh "$here/i8080-corpus.sh" "$work/corpus"
sh "$here/i8080-oracle-compare.sh" "$root" "$work/corpus" "$tool" test/i8080/oracle-known.txt
echo "== every opcode byte: Rivet's decoder and printer, assembled by asl"
sh "$here/i8080-oracle-dis.sh" "$root" "$tool"
echo "== release sources: asl against Rivet"
sh "$here/i8080-oracle-sources.sh" "$root" "$tool"
echo "== model through the K5 shim: 8080PRE and 8080EX1 against altairz80"
sh "$here/i8080-oracle-model.sh" "$root" "$tool"
echo "== generated programs: model, altairz80, vm80a_sim"
seeds=${I8080_DIFF_SEEDS:-120}
sh "$here/i8080-oracle-diff.sh" "$root" "$tool" "$seeds" 150
echo "== planted model bugs must be caught"
for bug in ult aux carry; do
    sh "$here/i8080-oracle-diff.sh" "$root" "$tool" 60 150 "$bug" | tail -1
done
echo "== T-states per form: model against vm80a_sim"
mkdir -p "${I8080_ARTIFACTS:-_build/i8080-oracle}"
sh "$here/i8080-oracle-cycles.sh" "$root" "$tool" "${I8080_ARTIFACTS:-_build/i8080-oracle}/cycles.txt"
