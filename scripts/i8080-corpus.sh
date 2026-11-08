#!/bin/sh
# Generate the encoding corpus for the i8080 oracle comparison, in the dialect
# both asl and Rivet read. The 244 documented forms are listed here from the
# Intel manual, not taken from Rivet's table.
# usage: i8080-corpus.sh <outdir>   -> <outdir>/valid/*.asm, <outdir>/invalid/*.asm
set -eu
out=$1
mkdir -p "$out/valid" "$out/invalid"
forms=0

hx() { printf '0%xh' "$1"; }
head() { printf '\tcpu\t8080\n\torg\t0\n'; }

# operand sample sets (decimal, converted to the manual's hex spelling)
d8_values="0 1 2 63 127 128 129 170 255"
d16_values="0 1 255 256 4660 32767 32768 43690 65535"
r8="b c d e h l m a"
rp="b d h sp"
rpp="b d h psw"

file() { # name -> opens valid/name.asm
    f="$out/valid/$1.asm"
    head > "$f"
    echo "$f"
}

# register-only families
for m in inr dcr add adc sub sbb ana xra ora cmp; do
    f=$(file $m)
    for r in $r8; do printf '\t%s\t%s\n' $m $r >> "$f"; forms=$((forms + 1)); done
done
f=$(file mov)
for d in $r8; do for s in $r8; do
    [ "$d$s" = mm ] && continue
    printf '\tmov\t%s,%s\n' $d $s >> "$f"; forms=$((forms + 1))
done; done
f=$(file mvi)
for r in $r8; do for v in $d8_values; do printf '\tmvi\t%s,%s\n' $r "$(hx $v)" >> "$f"; done; forms=$((forms + 1)); done
# negative and spelling variants of an 8-bit immediate (same encoding forms)
printf '\tmvi\ta,-1\n\tmvi\ta,-128\n\tmvi\ta,255\n\tmvi\ta,0b3h\n\tmvi\ta,10110b\n\tmvi\ta,%s\n\tmvi\ta,%s\n\tmvi\ta,%s\n' "'A'" '"z"' "3+4*5" >> "$f"

for m in inx dcx dad; do
    f=$(file $m)
    for r in $rp; do printf '\t%s\t%s\n' $m $r >> "$f"; forms=$((forms + 1)); done
done
f=$(file lxi)
for r in $rp; do for v in $d16_values; do printf '\tlxi\t%s,%s\n' $r "$(hx $v)" >> "$f"; done; forms=$((forms + 1)); done
printf '\tlxi\th,-1\n\tlxi\th,-32768\n\tlxi\td,1234\n' >> "$f"
for m in push pop; do
    f=$(file $m)
    for r in $rpp; do printf '\t%s\t%s\n' $m $r >> "$f"; forms=$((forms + 1)); done
done
for m in ldax stax; do
    f=$(file $m)
    for r in b d; do printf '\t%s\t%s\n' $m $r >> "$f"; forms=$((forms + 1)); done
done

# 8-bit immediate and port families
for m in adi aci sui sbi ani xri ori cpi in out; do
    f=$(file $m)
    for v in $d8_values; do printf '\t%s\t%s\n' $m "$(hx $v)" >> "$f"; done
    forms=$((forms + 1))
done

# 16-bit address families
for m in sta lda shld lhld jmp jnz jz jnc jc jpo jpe jp jm call cnz cz cnc cc cpo cpe cp cm; do
    f=$(file $m)
    for v in $d16_values; do printf '\t%s\t%s\n' $m "$(hx $v)" >> "$f"; done
    forms=$((forms + 1))
done

# no-operand families
f=$(file implied)
for m in nop hlt xchg xthl sphl pchl ei di ret rnz rz rnc rc rpo rpe rp rm rlc rrc ral rar daa cma stc cmc; do
    printf '\t%s\n' $m >> "$f"; forms=$((forms + 1))
done
f=$(file rst)
for n in 0 1 2 3 4 5 6 7; do printf '\trst\t%s\n' $n >> "$f"; forms=$((forms + 1)); done

# symbols, forward and backward, and expressions over them
f=$(file symbols)
cat >> "$f" <<'EOS'
back:	nop
	jmp	back
	jmp	fwd
	lxi	h,fwd
	lxi	h,fwd+2
	mvi	a,(fwd>>8)
	mvi	a,(fwd&0ffh)
	mvi	b,(back+7)&0ffh
	call	$
	jnz	$+3
	lda	konst
	adi	konst2
	lxi	sp,konst+konst2
fwd:	hlt
konst	equ	0c000h
konst2	equ	konst>>10
	dw	back,fwd,konst
	db	back,fwd&0ffh
	db	'hello',0
	db	"a",'b',0cdh
	dw	0,65535,-1,-32768
	db	-128,255,0
	db	5 dup (0aah)
	db	2 dup (1,2)
	dw	3 dup (konst)
	org	0100h
	db	1
	org	0200h
	db	2
	ds	3
	db	3
EOS

# AS's operators and their priorities, every ordered pair on two operand sets
f=$(file expressions)
e=0
mkdir -p "$out/either"
for a in "3 2 2" "5 3 1" "2 1 2"; do
    set -- $a
    for x in '^' '*' '/' '#' '+' '-' '<<' '>>' '&' '!' '|' '=' '<>' '<' '>' '<=' '>=' '&&' '||'; do
        for y in '^' '*' '/' '#' '+' '-' '<<' '>>' '&' '!' '|' '=' '<>' '<' '>' '<=' '>=' '&&' '||'; do
            case "$x$y" in
            *[/#]*) # a zero divisor is an error in both: one file each
                e=$((e + 1)); { head; printf '\tdw\t%s%s%s%s%s\n' "$1" "$x" "$2" "$y" "$3"; } > "$out/either/$(printf %03d $e).asm" ;;
            *) printf '\tdw\t%s%s%s%s%s\n' "$1" "$x" "$2" "$y" "$3" >> "$f" ;;
            esac
        done
    done
done
printf '\tdw\t-2^2,-1>>1,~1+1,-(3+4),(1+2)*(3+4),10-2-3,100/10/5,7#4,0ah+0bh*2,1011b+0ffh,$+2\n' >> "$f"

# values outside the operand's range, each its own file: both assemblers must refuse
n=0
bad() { n=$((n + 1)); { head; printf '\t%s\n' "$1"; } > "$out/invalid/$(printf %03d $n).asm"; }
bad "mvi a,256"; bad "mvi a,-129"; bad "adi 256"; bad "cpi 1000h"; bad "in 256"; bad "out -129"
bad "lxi h,65536"; bad "lxi h,-32769"; bad "jmp 65536"; bad "sta -32769"; bad "call 10000h"
bad "rst 8"; bad "rst -1"; bad "mov m,m"; bad "mov a"; bad "mov a,b,c"; bad "mvi a"; bad "mvi 5,a"
bad "inr sp"; bad "inx a"; bad "push sp"; bad "pop sp"; bad "ldax h"; bad "stax psw"; bad "lxi psw,0"
bad "dad psw"; bad "nop 1"; bad "ret a"; bad "jmp"; bad "lxi h"; bad "xchg a"; bad "bogus"
bad "mvi a,undefinedsym"; bad "jmp undefinedsym"; bad "mvi a,1/0"; bad "db 256"; bad "db -129"
bad "dw 65536"; bad "dw -32769"; bad "mvi q,1"; bad "add 5"; bad "adi b"

echo "forms=$forms valid_files=$(ls "$out/valid" | wc -l) either_files=$e invalid_files=$n"
