#!/usr/bin/env bash

set -eu

"$WD/bin/com" "$1" "$WD/build/main.asm"
fasm "$WD/build/main.asm" "$WD/build/main.o"
ld -fuse-ld=mold -o "$WD/bin/run" -lc "$WD/build/runtime_asm.o" \
    "$WD/build/runtime_c.o" "$WD/build/main.o"
"$WD/bin/run"
