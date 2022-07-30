format ELF64

public _start

extrn fflush
extrn stdout
extrn stderr

extrn print_heap

extrn entry_

section '.text' executable
    _start:
        mov rbp, rsp
        call entry_
        push rax
        call print_heap
        mov rdi, [stdout]
        call fflush
        mov rdi, [stderr]
        call fflush
        pop rdi
        mov eax, 60
        syscall

public HEAP_MEMORY

section '.bss' writeable
    HEAP_MEMORY rb HEAP_CAP
