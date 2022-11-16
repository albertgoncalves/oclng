#!/usr/bin/env bash

set -eu

export OCAMLRUNPARAM="b"

"$WD/bin/com" "$1" "$WD/build/main.asm"
cat "$WD/build/main.asm" >&2
fasm "$WD/build/main.asm" "$WD/build/main.o" > /dev/null
mold -run clang -no-pie -o "$WD/bin/run" "$WD/build/runtime_asm.o" \
    "$WD/build/runtime_c.o" "$WD/build/main.o"
"$WD/bin/run"
