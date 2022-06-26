#!/usr/bin/env bash

set -eu

flags=(
    -nolabels
    -strict-formats
    -strict-sequence
    -unboxed-types
    -warn-error "+a"
)

ocp-indent -i "$WD/src/"*.ml
cp "$WD/src/"*.ml "$WD/build/"

(
    cd "$WD/build/"
    ocamlc "${flags[@]}" "main.ml" -o "$WD/bin/com"
    "$WD/bin/com" "$WD/build/main.asm"
    fasm "$WD/build/main.asm" "$WD/build/main.o"
    ld -fuse-ld=mold -o "$WD/bin/run" -lc "$WD/build/main.o"
    "$WD/bin/run"
)
