#!/bin/sh
# A wrong hash must fail and unpack nothing; the right one must run.
# usage: retrocpu-fetch-test.sh <dir holding the release assets>
set -eu
here=$(cd "$(dirname "$0")" && pwd)
assets=$1
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/bad"
cp "$assets"/*.tar.xz "$tmp/bad/"
victim=$(ls "$tmp"/bad/retrocpu-tools-linux-*-musl.tar.xz)
printf x >> "$victim"
if RETROCPU_ASSETS=$tmp/bad "$here/retrocpu-fetch.sh" "$tmp/dest" 2> "$tmp/err"; then
    echo "retrocpu-fetch-test: tampered asset accepted" >&2
    exit 1
fi
grep -q 'SHA-256' "$tmp/err" || { echo "retrocpu-fetch-test: no hash message" >&2; exit 1; }
[ ! -e "$tmp"/dest/*/*/.tools.unpacked ] || { echo "retrocpu-fetch-test: tampered asset unpacked" >&2; exit 1; }
RETROCPU_ASSETS=$assets "$here/retrocpu-fetch.sh" "$tmp/dest" > /dev/null
echo "retrocpu-fetch-test: tampered asset rejected, pinned assets accepted"
