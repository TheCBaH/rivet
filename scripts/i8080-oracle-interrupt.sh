#!/bin/sh
# Interrupt timing: EI/DI sequences run with INT held high and an RST 0 supplied by
# the acknowledge cycle (the K5 shim's OUT FF at 0000 then ends the run), in the
# model and in vm80a_sim. Each program counts INR A after the sequence, so the
# final OUT FF shows how many instructions ran before the interrupt was taken.
# The model's OUT list must equal vm80a_sim's. The bytes of EI, DI and INR are
# checked against asl by i8080-oracle-dis.sh.
# usage: i8080-oracle-interrupt.sh <retrocpu root> <i8080_tool.exe>
set -eu
root=$1 tool=$2
arch=$(basename "$root")
diag=$root/retrocpu-diagnostics
vm=$root/retrocpu-vm80a-sim-$arch
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$diag"/shim-*.bin "$work/"
failed=0
while IFS=: read -r name body; do
    d=$work/$name
    mkdir "$d"
    {
        printf '\tcpu\t8080\n\torg\t0100h\n\tmvi\ta,0\n'
        for i in $body; do printf '\t%s\n' "$i"; done
        printf '\tinr\ta\n\tinr\ta\n\tinr\ta\n\tout\t32h\n\tout\t0ffh\n'
    } > "$d/prog.asm"
    "$tool" asm "$d/prog.asm" -o "$d/prog.com" > /dev/null
    "$tool" run "$d/prog.com" --shim "$work" --io-out "$d/model.io" --int c7 > /dev/null 2> "$d/model.err" || { cat "$d/model.err"; echo "$name: model run failed" >&2; exit 1; }
    python3 -I "$vm/vm80a.py" com-manifest "$d/prog.com" "$d/prog.json"
    printf '# i8080-stimulus v1\nreset 0 @4\ninta c7\nint 1 @m1+1\n' > "$d/prog.stim"
    sed -i 's/"stimulus": null/"stimulus": "prog.stim"/' "$d/prog.json"
    python3 -I "$vm/vm80a.py" run "$d/prog.json" "$d/vm" -- "$vm/bin/vm80a_sim_fast" > /dev/null
    if cmp -s "$d/model.io" "$d/vm/prog.ioout"; then
        echo "$name: model and vm80a_sim agree: $(tr '\n' ' ' < "$d/model.io")"
    else
        echo "$name: model $(tr '\n' ' ' < "$d/model.io")| vm80a_sim $(tr '\n' ' ' < "$d/vm/prog.ioout")" >&2
        failed=1
    fi
done <<'CASES'
ei_nop:ei nop
ei_ei:ei ei
ei_ei_ei:ei ei ei
ei_di:ei di
ei_di_ei:ei di ei
ei_ei_di:ei ei di
CASES
[ "$failed" = 0 ]
