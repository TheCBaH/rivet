#!/bin/sh
# Decode and print every opcode byte, assemble the text with asl, compare the
# bytes: the decoder, the printer and every instruction length (and the twelve
# undocumented bytes as data) against a third-party assembler, with no table of
# ours in between. With the listing command `builders`, the same for the bytes
# the typed OCaml builders produce.
# usage: i8080-oracle-dis.sh <retrocpu root> <i8080_tool.exe> [dis|builders]
set -eu
root=$1 tool=$2 listing=${3:-dis}
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
"$tool" "$listing" > "$work/listing"
n=0 bad=0
while IFS='	' read -r bytes text len cls; do
    # asl reads the manual's NNh, not 0xNN
    asl_text=$(echo "$text" | sed -E 's/0x([0-9a-f]+)/0\1h/g')
    printf '\tcpu\t8080\n\torg\t0\n\t%s\n' "$asl_text" > "$work/t.asm"
    if "$bin/asl" -q -cpu 8080 -o "$work/t.p" "$work/t.asm" > "$work/asl.log" 2>&1 && "$bin/p2bin" -l 0 "$work/t.p" "$work/t.bin" > /dev/null 2>&1; then
        got=$(od -An -v -tx1 "$work/t.bin" | tr -s ' \n' ' ' | sed 's/^ //; s/ $//')
    else
        got="asl refused: $(head -1 "$work/asl.log")"
    fi
    n=$((n + 1))
    [ "$got" = "$bytes" ] || { bad=$((bad + 1)); echo "$bytes ($text): asl gives $got"; }
done < "$work/listing"
if [ "$listing" = builders ]; then
    echo "i8080-oracle-dis: $n builder instructions printed by Rivet and assembled by asl, $((n - bad)) give the same bytes, $bad differ"
else
    echo "i8080-oracle-dis: $n opcode bytes printed by Rivet and assembled by asl, $((n - bad)) give the same bytes, $bad differ"
fi
[ "$bad" = 0 ]
