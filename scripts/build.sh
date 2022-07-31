#!/usr/bin/env bash

set -eu

heap_cap=4096
flags_ocaml=(
    -g
    -nolabels
    -strict-formats
    -strict-sequence
    -unboxed-types
    -warn-error "+a"
)
flags_c=(
    -c
    "-DHEAP_CAP=${heap_cap}"
    "-ferror-limit=1"
    -fshort-enums
    "-march=native"
    -O3
    "-std=c99"
    -Werror
    -Weverything
    -Wno-c11-extensions
    -Wno-declaration-after-statement
    -Wno-disabled-macro-expansion
    -Wno-extra-semi-stmt
    -Wno-incompatible-library-redeclaration
)
flags_fasm=(
    -d "HEAP_CAP=${heap_cap}"
)

(
    ocp-indent -i "$WD/src/"*.ml &
    clang-format -i -verbose "$WD/src/"*.c &
    for _ in $(jobs -p); do
        wait -n
    done
)
(
    cp "$WD/src/"*.ml "$WD/build/"
    (
        cd "$WD/build/"
        ocamlc "${flags_ocaml[@]}" "io.ml" "parse.ml" "compile.ml" "main.ml" \
            -o "$WD/bin/com"
    ) &
    clang "${flags_c[@]}" -o "$WD/build/runtime_c.o" "$WD/src/runtime.c" &
    for _ in $(jobs -p); do
        wait -n
    done
    fasm "${flags_fasm[@]}" "$WD/src/runtime.asm" "$WD/build/runtime_asm.o"
)
