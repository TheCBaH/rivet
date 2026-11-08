#!/bin/sh
# Assemble a corpus with asl + p2bin and with Rivet, compare the bytes.
# usage: i8080-oracle-compare.sh <retrocpu root> <corpus dir> <i8080_tool.exe> [known-differences file]
# Valid files must give the same bytes; invalid files must be refused by both.
# A difference is listed with a classification from the known file
# ("<relative path> <class>: <note>"); an unlisted one fails the run.
set -eu
root=$1 corpus=$2 tool=$3 known=${4:-/dev/null}
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
same=0 diff=0 both=0 split=0 unlisted=0
report() { # path kind
    c=$(grep "^$1 " "$known" | head -1 | cut -d' ' -f2- || true)
    if [ -n "$c" ]; then echo "difference $1 ($2): $c"; else echo "UNCLASSIFIED difference $1 ($2)"; unlisted=$((unlisted + 1)); fi
}
for f in "$corpus"/valid/*.asm; do
    n=$(basename "$f" .asm)
    if "$bin/asl" -q -cpu 8080 -o "$work/$n.p" "$f" > "$work/$n.asl.log" 2>&1 && "$bin/p2bin" -l 0 "$work/$n.p" "$work/$n.asl" > /dev/null 2>&1; then :; else
        echo "valid/$n: asl refused: $(head -2 "$work/$n.asl.log")"; report "valid/$n" asl-refused; diff=$((diff + 1)); continue
    fi
    if ! "$tool" asm "$f" -o "$work/$n.rivet" > "$work/$n.rivet.log" 2>&1; then
        echo "valid/$n: rivet refused: $(head -2 "$work/$n.rivet.log")"; report "valid/$n" rivet-refused; diff=$((diff + 1)); continue
    fi
    if cmp -s "$work/$n.asl" "$work/$n.rivet"; then same=$((same + 1)); else report "valid/$n" bytes; diff=$((diff + 1)); fi
done
for f in "$corpus"/invalid/*.asm; do
    n=$(basename "$f" .asm)
    a=ok r=ok
    "$bin/asl" -q -cpu 8080 -o "$work/i$n.p" "$f" > "$work/i$n.log" 2>&1 || a=refused
    grep -q 'error' "$work/i$n.log" && a=refused
    "$tool" asm "$f" -o "$work/i$n.rivet" > "$work/i$n.rlog" 2>&1 || r=refused
    if [ "$a" = refused ] && [ "$r" = refused ]; then both=$((both + 1))
    else echo "invalid/$n: asl $a, rivet $r: $(sed -n 3p "$f" | tr -d '\t')"; report "invalid/$n" "asl-$a-rivet-$r"; split=$((split + 1)); fi
done
# either: both assemblers must agree, whether they accept or refuse
agree=0
for f in "$corpus"/either/*.asm; do
    n=$(basename "$f" .asm)
    a=ok r=ok
    "$bin/asl" -q -cpu 8080 -o "$work/e$n.p" "$f" > "$work/e$n.log" 2>&1 && "$bin/p2bin" -l 0 "$work/e$n.p" "$work/e$n.asl" > /dev/null 2>&1 || a=refused
    grep -q 'error' "$work/e$n.log" && a=refused
    "$tool" asm "$f" -o "$work/e$n.rivet" > "$work/e$n.rlog" 2>&1 || r=refused
    if [ "$a" = ok ] && [ "$r" = ok ]; then
        if cmp -s "$work/e$n.asl" "$work/e$n.rivet"; then agree=$((agree + 1)); else report "either/$n" bytes; fi
    elif [ "$a" = "$r" ]; then agree=$((agree + 1))
    else echo "either/$n: asl $a, rivet $r: $(sed -n 3p "$f" | tr -d '\t')"; report "either/$n" "asl-$a-rivet-$r"; fi
done
echo "i8080-oracle-compare: valid identical=$same different=$diff; invalid refused-by-both=$both disagree=$split; either agree=$agree; unclassified=$unlisted"
[ "$unlisted" = 0 ]
