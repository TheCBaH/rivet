#!/bin/sh
# The release's AS-syntax 8080PRE, 8080EX1 and K5 shim sources assembled by asl
# and by Rivet must give the same bytes, and the M80 originals must be
# refused with source positions.
# usage: i8080-oracle-sources.sh <retrocpu root> <i8080_tool.exe>
set -eu
root=$1 tool=$2
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
src=$root/as-source
orig=$root/retrocpu-diagnostics/source
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
for n in 8080PRE.MAC 8080EX1.MAC shim.asm; do
    "$bin/asl" -q -cpu 8080 -o "$work/a.p" "$src/$n"
    "$bin/p2bin" -l 0 "$work/a.p" "$work/asl.bin" > /dev/null
    "$tool" asm "$src/$n" -o "$work/rivet.bin" -l "$work/$n.lst" -s "$work/$n.sym" > /dev/null
    cmp "$work/asl.bin" "$work/rivet.bin"
    echo "$n: $(wc -c < "$work/rivet.bin") bytes, sha256 $(sha256sum < "$work/rivet.bin" | cut -c1-16)..., equal to asl"
done
# the shipped images
"$tool" asm "$src/8080PRE.MAC" -o "$work/pre.com" > /dev/null
[ "$(sha256sum < "$work/pre.com" | cut -d' ' -f1)" = 18eb3c79cba42c0718f160be6a1853cb64cdce7aa47d65780189a57bdd98c4e0 ] ||
    { echo "8080PRE.COM hash differs from the pinned one" >&2; exit 1; }
"$tool" asm "$src/8080EX1.MAC" -o "$work/ex1.com" > /dev/null
[ "$(wc -c < "$work/ex1.com")" = 4535 ] && cmp "$work/ex1.com" "$root/retrocpu-diagnostics/8080EX1.COM"
for r in "0x0000-0x0002 boot" "0x0005-0x0007 bdos" "0xfe00-0xfe1b handler"; do
    set -- $r
    "$tool" asm "$src/shim.asm" -r "$1" -o "$work/shim-$2.bin" > /dev/null
    cmp "$work/shim-$2.bin" "$root/retrocpu-diagnostics/shim-$2.bin"
done
echo "8080PRE.COM sha256 18eb3c79...c4e0, 8080EX1.COM 4535 bytes, K5 shim ranges: equal to the release images"
# unsupported constructs are reported with positions (the M80 originals)
for n in 8080PRE.MAC 8080EX1.MAC; do
    if "$tool" asm "$orig/$n" -o "$work/o.bin" 2> "$work/err"; then echo "$n: M80 original accepted" >&2; exit 1; fi
    grep -q "^$n:4[0-9]: unsupported" "$work/err" ||
        { echo "$n: no positioned unsupported report"; cat "$work/err"; exit 1; }
    echo "$n (M80 original): $(grep -c unsupported "$work/err") unsupported constructs reported, first: $(grep unsupported "$work/err" | head -1)"
done
