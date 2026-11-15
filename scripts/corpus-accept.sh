#!/usr/bin/env bash
# Acceptance summary: how many gcc fixtures assemble at each optimisation level.
#
#   scripts/corpus-accept.sh [gen|run|all] [-O "O0 O1 O2 O3 Os"] [-d DIR]
#
# gen compiles fixtures/c/*/*.c to DIR/<level>/<target>/<case>-<file>.s with the
# cross gcc (flags as tools/lib/target.ml gcc_fixture_args, -O swapped); run
# assembles each with _build/default/tool/asm.exe and prints, per level, the
# accepted count and the first rejection line of every rejected fixture.
# Informational: always exits 0. The two halves are separate so they can run
# in different containers (gcc in the CI image, asm.exe where it was built).
set -u
mode=all
levels="O0 O1 O2 O3 Os"
dir=_build/corpus-accept
case "${1:-}" in gen | run | all) mode=$1; shift ;; esac
while [ $# -gt 0 ]; do
	case $1 in
	-O) levels=$2; shift 2 ;;
	-d) dir=$2; shift 2 ;;
	*) echo "usage: $0 [gen|run|all] [-O levels] [-d dir]" >&2; exit 2 ;;
	esac
done
cd "$(dirname "$0")/.." || exit 2
asm=_build/default/tool/asm.exe
export PATH="$PATH:/usr/local/riscv32-linux-gnu-toolchain/bin"
targets="x86_32 x86_64 arm aarch64 riscv32 riscv64"

gcc_args() {
	common="-fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables"
	case $1 in
	x86_32) prefix=i686-linux-gnu- args="" ;;
	x86_64) prefix=x86_64-linux-gnu- args="" ;;
	arm) prefix=arm-linux-gnueabihf- args="-marm" ;;
	aarch64) prefix=aarch64-linux-gnu- args="" ;;
	riscv32) prefix=riscv32-linux-gnu- args="-march=rv32imafd -mabi=ilp32d -mno-relax -msmall-data-limit=0" ;;
	riscv64) prefix=riscv64-linux-gnu- args="-march=rv64imafd -mabi=lp64d -mno-relax -msmall-data-limit=0" ;;
	esac
	args="$args $common"
}

gen() {
	for l in $levels; do
		for t in $targets; do
			gcc_args "$t"
			mkdir -p "$dir/$l/$t"
			for c in fixtures/c/*/*.c; do
				n=$(basename "$(dirname "$c")")-$(basename "$c" .c)
				"${prefix}gcc" -S -"$l" $args -o "$dir/$l/$t/$n.s" "$c" 2>"$dir/$l/$t/$n.gcc.err" ||
					echo "gen: gcc failed: $l $t $n" >&2
			done
		done
	done
}

run() {
	for l in $levels; do
		total=0 ok=0
		rej="$dir/$l/rejections.txt"
		: >"$rej"
		for t in $targets; do
			for s in "$dir/$l/$t"/*.s; do
				[ -f "$s" ] || continue
				n=$(basename "$s" .s)
				total=$((total + 1))
				if err=$("$asm" --target "$t" --dump-lowered-ast "$s" 2>&1 >/dev/null); then
					ok=$((ok + 1))
				else
					printf '%s/%s: %s\n' "$t" "$n" "$(printf '%s\n' "$err" | grep -m1 .)" >>"$rej"
				fi
			done
		done
		echo "== -$l: $ok/$total accepted"
		sed 's/^/   /' "$rej"
	done
}

[ "$mode" = run ] || gen
[ "$mode" = gen ] || run
exit 0
