#!/bin/sh
# Differential runs: seeded generated programs executed by Rivet's model,
# altairz80 and vm80a_sim, with the .COM images built by asl and checked
# against Rivet's own assembler.
#
# usage: i8080-oracle-diff.sh <retrocpu root> <i8080_tool.exe> <seeds> <units> [plant]
#
# model vs vm80a_sim: every OUT (port, value) in order, and the whole 64 KiB
#   memory image after the run.
# model vs altairz80: the console text, and the whole memory image (altairz80
#   cannot show OUTs to other ports).
# A mismatch must be listed in test/i8080/diff-known.txt ("<seed> <runner>
# <class>: <note>"); with [plant] (a planted model bug) mismatches are
# required instead.
set -eu
root=$1 tool=$2 seeds=$3 units=$4 plant=${5:-}
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
diag=$root/retrocpu-diagnostics
vm=$root/retrocpu-vm80a-sim-$arch
known=${I8080_DIFF_KNOWN:-test/i8080/diff-known.txt}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$diag"/shim-*.bin "$work/"
hexlines() { od -An -v -tx1 -w1 "$1" | tr -d ' '; }
mismatch=0 unlisted=0 programs=0 asm_same=0 alias_skipped=0
: > "$work/opcodes"
note() { # seed runner what
    mismatch=$((mismatch + 1))
    c=$(grep "^$1 $2 " "$known" 2>/dev/null | head -1 | cut -d' ' -f3- || true)
    if [ -n "$c" ]; then echo "seed $1: $2 differs ($3): $c"; else echo "seed $1: $2 differs ($3): UNCLASSIFIED"; unlisted=$((unlisted + 1)); fi
}
s=1
while [ "$s" -le "$seeds" ]; do
    d=$work/p$s
    mkdir "$d"
    "$tool" gen "$s" "$units" > "$d/prog.asm"
    "$bin/asl" -q -cpu 8080 -o "$d/prog.p" "$d/prog.asm" 2> "$d/asl.err" || { cat "$d/asl.err"; echo "seed $s: asl refused the generated program" >&2; exit 1; }
    "$bin/p2bin" -l 0 "$d/prog.p" "$d/prog.com" > /dev/null
    "$tool" asm "$d/prog.asm" -o "$d/prog.rivet" > /dev/null || { echo "seed $s: rivet refused the generated program" >&2; exit 1; }
    if cmp -s "$d/prog.com" "$d/prog.rivet"; then asm_same=$((asm_same + 1)); else note "$s" assembler "asl and rivet bytes differ"; fi
    # the model
    "$tool" run "$d/prog.com" --shim "$work" --dump "$d/model.bin" --io-out "$d/model.io" --console "$d/model.console" --opcodes "$d/model.ops" ${plant:+--plant $plant} > /dev/null 2> "$d/model.err" || true
    grep -q 'status=\(exited\|halted\)' "$d/model.err" || { cat "$d/model.err"; echo "seed $s: model did not finish" >&2; exit 1; }
    cat "$d/model.ops" >> "$work/opcodes"
    hexlines "$d/model.bin" > "$d/model.mem"
    # vm80a_sim
    mkdir "$d/vm"
    python3 -I "$vm/vm80a.py" com "$d/prog.com" "$d/vm" -- "$vm/bin/vm80a_sim_fast" > "$d/vm.log" 2>&1 || { cat "$d/vm.log"; echo "seed $s: vm80a_sim failed" >&2; exit 1; }
    cmp -s "$d/model.io" "$d/vm/prog.ioout" || note "$s" vm80a_sim "io_out"
    cmp -s "$d/model.mem" "$d/vm/prog.mem" || note "$s" vm80a_sim "memory"
    # altairz80 executes the alias bytes 08..38 as Z80 instructions (JR, DJNZ, EX AF,AF'),
    # so programs that use aliases (seeds divisible by 3) are not compared there
    if [ $((s % 3)) = 0 ]; then alias_skipped=$((alias_skipped + 1)); programs=$((programs + 1)); s=$((s + 1)); continue; fi
    cp "$d/prog.com" "$d/p.com"
    { echo "set cpu 8080"; echo "set sio port=1"; echo "set sio port=1/0/0/0/0/F/0/T"
      echo "load $work/shim-boot.bin 0"; echo "load $work/shim-bdos.bin 5"; echo "load $work/shim-handler.bin fe00"
      echo "load $d/p.com 100"; echo "d pc 100"; echo "d sp fe00"; echo "go"; echo "ex 0-ffff"; echo "exit"; } > "$d/simh.ini"
    timeout 60 "$bin/altairz80" "$d/simh.ini" > "$d/simh.raw" 2>&1 || true
    awk -F'\t' '/^[0-9A-F]+:\t[0-9A-F][0-9A-F]$/ {print tolower($2)}' "$d/simh.raw" > "$d/simh.mem"
    grep -a -v -e '^[0-9A-F]*:	' -e '^Altair 8800' -e 'bytes \[' -e 'HALT instruction' -e '^Goodbye' "$d/simh.raw" | tr -d '\r' | tr -d '\n' > "$d/simh.console"
    tr -d '\r\n' < "$d/model.console" > "$d/model.console.n"
    cmp -s "$d/model.console.n" "$d/simh.console" || note "$s" altairz80 "console"
    # altairz80 has the Altair boot ROM at FF00..FFFF: an environment difference, not the CPU's
    head -n 65280 "$d/model.mem" > "$d/model.low"
    head -n 65280 "$d/simh.mem" > "$d/simh.low"
    cmp -s "$d/model.low" "$d/simh.low" || note "$s" altairz80 "memory below FF00"
    programs=$((programs + 1))
    s=$((s + 1))
done
sort -u "$work/opcodes" > "$work/covered"
doc=$("$tool" dis | awk -F'\t' '$4 == "doc" {print substr($1, 1, 2)}' | sort -u)
missing=$(echo "$doc" | grep -v -x -F -f "$work/covered" | tr '\n' ' ' || true)
echo "i8080-oracle-diff: $programs programs x $units units, assembler bytes equal in $asm_same; altairz80 skipped for $alias_skipped alias programs; mismatches=$mismatch unclassified=$unlisted; documented opcodes executed: $(( $(echo "$doc" | wc -l) - $(echo $missing | wc -w) )) of $(echo "$doc" | wc -l) (not executed: ${missing:-none})"
if [ -n "$plant" ]; then
    [ "$mismatch" -gt 0 ] || { echo "planted bug $plant was NOT caught" >&2; exit 1; }
    echo "planted model bug '$plant' caught: $mismatch mismatches"
else
    [ "$unlisted" = 0 ]
fi
