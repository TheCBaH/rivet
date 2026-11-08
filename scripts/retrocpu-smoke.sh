#!/bin/sh
# The fetched oracles run, as a non-root user: asl + p2bin rebuild the shim
# bytes shipped in the release, altairz80 passes 8080PRE.
# usage: retrocpu-smoke.sh <dir printed by retrocpu-fetch.sh>
set -eu
[ "$(id -u)" != 0 ] || { echo "retrocpu-smoke: run as a non-root user" >&2; exit 2; }
root=$1
arch=$(basename "$root")
[ -x "$root/retrocpu-tools-$arch/bin/asl" ] || { echo "retrocpu-smoke: no tools in $root" >&2; exit 2; }
sh "$root/retrocpu-diagnostics/check-shim.sh" "$root" "$arch"
(cd "$root/retrocpu-diagnostics" && "$root/retrocpu-tools-$arch/bin/altairz80" 8080PRE.ini) |
    grep -q "8080 Preliminary tests complete" ||
    { echo "retrocpu-smoke: 8080PRE did not finish under altairz80" >&2; exit 1; }
echo "retrocpu-smoke: asl, p2bin and altairz80 run; 8080PRE passes"
