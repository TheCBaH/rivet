#!/bin/sh
# Fetch the pinned retrocpu-tools release, verify it, unpack it for this arch.
# usage: retrocpu-fetch.sh [dest]        (default .retrocpu)
#   RETROCPU_ASSETS=<dir>  take the release assets from a local directory
#                          instead of downloading (still verified)
# Prints the unpack directory on success. Fails if any hash differs from
# scripts/retrocpu.pin; nothing is unpacked from an unverified file.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
. "$here/retrocpu.pin"
dest=${1:-.retrocpu}
case $(uname -m) in
x86_64 | amd64) arch=x86_64 ;;
aarch64 | arm64) arch=aarch64 ;;
*) echo "retrocpu-fetch: unsupported machine $(uname -m)" >&2; exit 2 ;;
esac
mkdir -p "$dest"; dest=$(cd "$dest" && pwd)
out=$dest/$RETROCPU_TAG/$arch
dl=$dest/$RETROCPU_TAG/download
mkdir -p "$dl" "$out"
assets="tools:retrocpu-tools-linux-$arch-musl diagnostics:retrocpu-diagnostics-$arch vm80a:retrocpu-vm80a-sim-linux-$arch-musl"
for a in $assets; do
    key=${a%%:*} name=${a#*:}.tar.xz
    want=$(eval echo "\$RETROCPU_SHA256_${key}_$arch")
    f=$dl/$name
    if [ -n "${RETROCPU_ASSETS:-}" ]; then
        cp "$RETROCPU_ASSETS/$name" "$f"
    elif [ ! -f "$f" ]; then
        curl -fsSL --retry 3 -o "$f" \
            "https://github.com/$RETROCPU_REPO/releases/download/$RETROCPU_TAG/$name"
    fi
    got=$(sha256sum "$f" | cut -d' ' -f1)
    if [ "$got" != "$want" ]; then
        rm -f "$f"
        echo "retrocpu-fetch: $name: SHA-256 $got, pinned $want" >&2
        exit 1
    fi
    if [ ! -f "$out/.$key.unpacked" ]; then
        tar -xJf "$f" -C "$out"
        : > "$out/.$key.unpacked"
    fi
done
# AS-syntax exercisers: the release ships the M80 originals and the patches
src=$out/as-source
if [ ! -f "$src/.done" ]; then
    mkdir -p "$src"
    d=$out/retrocpu-diagnostics/source
    cp "$d/8080PRE.MAC" "$d/8080EX1.MAC" "$d/shim.asm" "$src"
    for n in 8080PRE 8080EX1; do (cd "$src" && patch -p1 -s < "$d/patches/$n.patch"); done
    : > "$src/.done"
fi
echo "$out"
