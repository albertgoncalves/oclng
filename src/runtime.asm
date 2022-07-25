format ELF64

public _start

extrn fflush
extrn stdout
extrn stderr

extrn print_memory

extrn _entry_

section '.text' executable
    _start:
        call _entry_
        push rax
        call print_memory
        mov rdi, [stdout]
        call fflush
        mov rdi, [stderr]
        call fflush
        pop rdi
        mov eax, 60
        syscall

public MEMORY
public MEMORY_LEN

section '.data' writeable
    MEMORY_LEN dq 0

section '.bss' writeable
    MEMORY rb MEMORY_CAP
