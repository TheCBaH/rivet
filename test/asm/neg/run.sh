#!/bin/sh
# Compiles each case against the built rivet package. A case names its packages on its first line,
# (* requires: rivet.x ... *), and on its second the text its error must mention,
# (* error: Xreg *); a case without that line must compile. Error wording differs between
# compilers, so only the mention is compared.
lib=$1
OCAMLPATH="$lib${OCAMLPATH:+:$OCAMLPATH}"
export OCAMLPATH
out_dir=$(mktemp -d)
for f in *.ml; do
  name=$(basename "$f" .ml)
  pkgs=$(sed -n '1s/^(\* requires: \(.*\) \*)$/\1/p' "$f" | tr ' ' ',')
  want=$(sed -n '2s/^(\* error: \(.*\) \*)$/\1/p' "$f")
  if msg=$(ocamlfind ocamlc -package "$pkgs" -c -o "$out_dir/$name" "$f" 2>&1); then
    if [ -n "$want" ]; then echo "$name: COMPILES"; else echo "$name: compiles"; fi
  else
    err=$(printf '%s\n' "$msg" | sed -n '/^Error/,$p' | tr -s ' \n' ' ')
    if [ -z "$want" ]; then echo "$name: REFUSED $err"
    elif printf '%s' "$err" | grep -qF "$want"; then echo "$name: refused, mentions $want"
    else echo "$name: REFUSED WITHOUT $want: $err"
    fi
  fi
done
rm -rf "$out_dir"
