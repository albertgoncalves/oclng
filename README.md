# oclng

Dependencies
---
 - [Clang](https://clang.llvm.org/)
```console
$ clang --version
clang version 14.0.6
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
```
 - [flat assembler](https://flatassembler.net/)
```console
$ fasm
flat assembler  version 1.73.30
```
 - [mold](https://github.com/rui314/mold)
```console
$ mold --version
mold 1.6.0 (323ad30e25c2c81efdb07ab76601a119335a40c8; compatible with GNU ld)
```
 - [Nix](https://nixos.org/download.html)
```console
$ nix --version
nix (Nix) 2.11.1
```

Quick start
---
```console
$ nix-shell
[nix-shell:path/to/oclng]$ ./scripts/build.sh
[nix-shell:path/to/oclng]$ ./scripts/test.py
[nix-shell:path/to/oclng]$ cat > program.oc << EOF
> fib n a b {
>     switch (= n 0) {
>         (fib (- n 1) b (+ a b))
>     } {
>         a
>     }
> }
>
> entry {
>     (printf "%ld\n" (fib 50 0 1));
>     0
> }
> EOF
[nix-shell:path/to/oclng]$ ./scripts/run.sh program.oc
12586269025
```
