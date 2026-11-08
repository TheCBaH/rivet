#!/bin/sh
# T-states per form: every documented instruction (and the alias bytes) alone in a
# small program, run in vm80a_sim and in the model; the cost of the instruction is
# the difference to the same program with a NOP, plus the NOP's 4. The model's
# count comes from the descriptor table; vm80a counts clock cycles.
# usage: i8080-oracle-cycles.sh <retrocpu root> <i8080_tool.exe> [table file]
set -eu
root=$1 tool=$2 out=${3:-/dev/null}
arch=$(basename "$root")
bin=$root/retrocpu-tools-$arch/bin
diag=$root/retrocpu-diagnostics
vm=$root/retrocpu-vm80a-sim-$arch
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$diag"/shim-*.bin "$work/"

prog() { # text flagbyte ret(0/1) hl-is-next(0/1) -> asm on stdout
    cat <<EOS
	cpu	8080
	org	0100h
	lxi	sp,3000h
	mvi	a,0c9h
	sta	8
	sta	10h
	sta	18h
	sta	20h
	sta	28h
	sta	30h
	sta	38h
	lxi	h,$2
	push	h
	pop	psw
	lxi	b,2100h
	lxi	d,2100h
EOS
    if [ "$3" = 1 ]; then printf '\tlxi\th,next\n\tpush\th\n'; fi
    printf '\tlxi\th,2100h\n'
    if [ "$4" = 1 ]; then printf '\tlxi\th,next\n'; fi
    printf '\t%s\nnext:\n\tlxi\tsp,3000h\n\tjmp\t0\n' "$1"
}
tstates_vm() { # asm -> tstates
    "$bin/asl" -q -cpu 8080 -o "$work/t.p" "$1" 2> "$work/asl.err" || { cat "$work/asl.err" >&2; return 1; }
    "$bin/p2bin" -l 0 "$work/t.p" "$work/t.com" > /dev/null
    rm -rf "$work/vm"; mkdir "$work/vm"
    python3 -I "$vm/vm80a.py" com "$work/t.com" "$work/vm" -- "$vm/bin/vm80a_sim_fast" > "$work/vm.log" 2>&1 || { cat "$work/vm.log" >&2; return 1; }
    sed -n 's/^tstates=//p' "$work/vm/t.result"
}
tstates_model() {
    "$tool" run "$work/t.com" --shim "$work" 2>&1 >/dev/null | sed -n 's/.*tstates=\([0-9]*\).*/\1/p'
}
printf "byte form flags k1_model vm80a\n" > "$work/table"
n=0 bad=0 skipped=0
"$tool" dis > "$work/listing"
while IFS='	' read -r bytes text len cls; do
    b=$(echo "$bytes" | cut -c1-2)
    byte=$((0x$b))
    case $b in 76 | c7) skipped=$((skipped + 1)); continue ;; esac   # HLT ends the run; RST 0 is the warm boot
    # operand spellings: control transfers go to NEXT, other addresses to 2100h
    ctl=0
    case $((byte & 0xc7)) in 194 | 196) ctl=1 ;; esac            # jcc, ccc
    [ $byte = 195 ] || [ $byte = 205 ] || [ $byte = 203 ] || [ $byte = 221 ] || [ $byte = 237 ] || [ $byte = 253 ] && ctl=1
    insn=$(echo "$text" | sed 's/, /,/')
    case $cls in alias*) insn="db 0${b}h"; case $b in cb | dd | ed | fd) insn="db 0${b}h
	dw next" ;; esac ;; esac
    if [ $ctl = 1 ] && [ "${cls%% *}" = doc ]; then insn=$(echo "$insn" | sed 's/0x2000/next/'); else insn=$(echo "$insn" | sed 's/0x2000/2100h/; s/,0x2a$/,2ah/; s/ 0x2a$/ 2ah/'); fi
    case $text in "in "*) insn="in 60h" ;; "out "*) insn="out 70h" ;; esac
    insn=$(echo "$insn" | sed 's/0x2a/2ah/')
    retnext=0 hlnext=0
    [ $b = e9 ] && hlnext=1
    case $b in c9 | d9) retnext=1 ;; esac
    [ $((byte & 0xc7)) = 192 ] && retnext=1                      # rcc
    # alias bytes that jump (cb), call (dd ed fd) or return (d9) behave as their twin
    for flag in 0002h 00d7h; do
        if [ $flag = 00d7h ]; then
            case $((byte & 0xc7)) in 192 | 194 | 196) ;; *) [ "$cls" = "alias of C3" ] || [ "$cls" = "alias of CD" ] || [ "$cls" = "alias of C9" ] || continue ;; esac
        fi
        prog "$insn" "$flag" $retnext $hlnext > "$work/i.asm"
        t_i=$(tstates_vm "$work/i.asm") || { echo "$b: vm80a failed" >&2; exit 1; }
        m_i=$(tstates_model)
        prog "nop" "$flag" $retnext $hlnext > "$work/n.asm"
        t_n=$(tstates_vm "$work/n.asm")
        m_n=$(tstates_model)
        dv=$((t_i - t_n + 4)); dm=$((m_i - m_n + 4))
        printf '%s %s %s %s %s\n' "$b" "$(echo "$insn" | tr ' ' '_' | tr '\n\t' '__')" "$flag" "$dm" "$dv" >> "$work/table"
        n=$((n + 1))
        [ "$dv" = "$dm" ] || { bad=$((bad + 1)); echo "$b $insn ($flag): model $dm, vm80a $dv"; }
    done
done < "$work/listing"
cp "$work/table" "$out" 2>/dev/null || true
echo "i8080-oracle-cycles: $n runs (HLT and RST 0 skipped: $skipped), model equals vm80a in $((n - bad)), differs in $bad"
[ "$bad" = 0 ]
