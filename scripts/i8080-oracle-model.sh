#!/bin/sh
# 8080PRE and 8080EX1 in Rivet's behavioural model through the K5 shim, against
# altairz80: pass message, 25 of 25 groups, and the console text equal once
# line terminators are normalised.
# usage: i8080-oracle-model.sh <retrocpu root> <i8080_tool.exe> [programs...]   (default: 8080PRE 8080EX1)
set -eu
root=$1 tool=$2
shift 2
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
diag=$root/retrocpu-diagnostics
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
norm() { tr -d '\r' | grep -v '^$' || true; }
[ $# -gt 0 ] || set -- 8080PRE 8080EX1
for p in "$@"; do
    start=$(date +%s)
    "$tool" run "$diag/$p.COM" --shim "$diag" > "$work/$p.model" 2> "$work/$p.model.err" || { cat "$work/$p.model.err"; echo "$p: model run failed" >&2; exit 1; }
    t_model=$(($(date +%s) - start))
    cp "$diag"/*.COM "$diag"/shim-*.bin "$work/"
    { echo "set cpu 8080"; echo "set sio port=1"; echo "set sio port=1/0/0/0/0/F/0/T"
      echo "load shim-boot.bin 0"; echo "load shim-bdos.bin 5"; echo "load shim-handler.bin fe00"
      echo "load $p.COM 100"; echo "d pc 100"; echo "d sp fe00"; echo "go"; echo "exit"; } > "$work/$p.ini"
    start=$(date +%s)
    (cd "$work" && "$bin/altairz80" "$p.ini") > "$work/$p.simh.raw" 2>&1
    t_simh=$(($(date +%s) - start))
    grep -v -e '^Altair 8800' -e 'bytes \[' -e '^HALT instruction' -e '^Goodbye' -e '^$' "$work/$p.simh.raw" | norm > "$work/$p.simh"
    norm < "$work/$p.model" > "$work/$p.model.n"
    cmp "$work/$p.model.n" "$work/$p.simh" || { diff "$work/$p.model.n" "$work/$p.simh" | head; echo "$p: console differs from altairz80" >&2; exit 1; }
    case $p in
    8080PRE) grep -q "8080 Preliminary tests complete" "$work/$p.model.n" || { echo "$p: no pass message" >&2; exit 1; } ;;
    8080EX1)
        ok=$(grep -c '  OK$' "$work/$p.model.n")
        [ "$ok" = 25 ] && grep -q 'Tests complete' "$work/$p.model.n" || { echo "$p: $ok of 25 groups OK" >&2; exit 1; } ;;
    esac
    echo "$p: model $(cat "$work/$p.model.err") (${t_model}s); console equal to altairz80 (${t_simh}s)$([ "$p" = 8080EX1 ] && echo "; $ok of 25 groups OK")"
done
