#!/usr/bin/env bash
# Print target_config's seventeen values for ONE target as a simple TSV.
#
# This exists so Target can be compared against the shell matrix. Since Phase 7
# that matrix is GENERATED from Target, so the comparison is a round trip: it
# catches a rendering bug - bad quoting, a lost array element, a forgotten
# default - rather than a disagreement between two hand-written definitions.
# It takes a validated target argument and
# is invoked as an argv vector: no `declare -p`, no eval, no dynamically
# constructed shell. The comparison is a test's job, not this script's.
#
# Arrays are printed with a single space between elements, which is exactly what
# "${ARRAY[*]}" produces and exactly what write_manifest records - so the
# comparison is against the spelling that reaches the committed bytes.
set -euo pipefail
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)

usage() { echo "Usage: $0 <target>" >&2; }
Fatal() { echo "FATAL: $*" >&2; exit 1; }

# shellcheck source=../target-matrix.sh
. "$REPO_ROOT/tools/target-matrix.sh"

[ "$#" -eq 1 ] || { usage; exit 2; }

known=false
for t in "${ALL_TARGETS[@]}"; do [ "$t" = "$1" ] && known=true; done
"$known" || Fatal "unknown target '$1'"

target_config "$1"

printf 'CONFIGURE_TARGET\t%s\n'        "$CONFIGURE_TARGET"
printf 'TOOLPREFIX\t%s\n'              "$TOOLPREFIX"
printf 'QEMU_BIN\t%s\n'                "$QEMU_BIN"
printf 'QEMU_SYSROOT\t%s\n'            "$QEMU_SYSROOT"
printf 'CCOMP_EXTRA_ARGS\t%s\n'        "${CCOMP_EXTRA_ARGS[*]-}"
printf 'COMPCERT_CONFIGURE_ARGS\t%s\n' "${COMPCERT_CONFIGURE_ARGS[*]-}"
printf 'AS_FLAGS\t%s\n'                "${AS_FLAGS[*]-}"
printf 'LD_FLAGS\t%s\n'                "${LD_FLAGS[*]-}"
printf 'LINKER_EMULATION\t%s\n'        "$LINKER_EMULATION"
printf 'ELF_CLASS\t%s\n'               "$ELF_CLASS"
printf 'WORD_SIZE\t%s\n'               "$WORD_SIZE"
printf 'HAS_SYSROOT\t%s\n'             "$HAS_SYSROOT"
printf 'READELF_MACHINE\t%s\n'         "$READELF_MACHINE"
printf 'LINK_TEXT_ADDR\t%s\n'          "$LINK_TEXT_ADDR"
printf 'LINK_RODATA_ADDR\t%s\n'        "$LINK_RODATA_ADDR"
printf 'LINK_DATA_ADDR\t%s\n'          "$LINK_DATA_ADDR"
printf 'LINK_BSS_ADDR\t%s\n'           "$LINK_BSS_ADDR"
