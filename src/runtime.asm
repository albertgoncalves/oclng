format ELF64

public main

extrn fflush
extrn setlinebuf
extrn stderr
extrn stdout

extrn print_heap

extrn entry_

section '.text' executable
    main:
        push rbp
        mov rbp, rsp

        mov rdi, [stderr]
        call setlinebuf

        call entry_
        push rax

        call print_heap

        mov rdi, [stdout]
        call fflush
        mov rdi, [stderr]
        call fflush

        pop rax
        leave
        ret

public HEAP_MEMORY

section '.bss' writeable
    HEAP_MEMORY rb HEAP_CAP
