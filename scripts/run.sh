#!/usr/bin/env bash

set -eu

memory_cap=512
flags_ocaml=(
    -nolabels
    -strict-formats
    -strict-sequence
    -unboxed-types
    -warn-error "+a"
)
flags_c=(
    -c
    "-DMEMORY_CAP=${memory_cap}"
    "-ferror-limit=1"
    "-march=native"
    -O3
    "-std=c99"
    -Werror
    -Weverything
    -Wno-declaration-after-statement
    -Wno-disabled-macro-expansion
    -Wno-extra-semi-stmt
)

(
    ocp-indent -i "$WD/src/"*.ml &
    clang-format -i -verbose "$WD/src/"*.c &
    for _ in $(jobs -p); do
        wait -n
    done
    cp "$WD/src/"*.ml "$WD/build/"
)

(
    cd "$WD/build/"
    (
        ocamlc "${flags_ocaml[@]}" "types.ml" "parse.ml" "compile.ml" \
            "main.ml" -o "$WD/bin/com"
        "$WD/bin/com" "$WD/ex/fib.oc" "$WD/build/main.asm"
        fasm "$WD/build/main.asm" "$WD/build/main.o"
    ) &
    clang "${flags_c[@]}" -o "$WD/build/runtime_c.o" "$WD/src/runtime.c" &
    fasm -d "MEMORY_CAP=${memory_cap}" "$WD/src/runtime.asm" \
        "$WD/build/runtime_asm.o" &
    for _ in $(jobs -p); do
        wait -n
    done
    ld -fuse-ld=mold -o "$WD/bin/run" -lc "$WD/build/runtime_asm.o" \
        "$WD/build/runtime_c.o" "$WD/build/main.o"
    "$WD/bin/run"
)
