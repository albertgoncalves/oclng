format ELF64

public _start

extrn _entry_

section '.text' executable
    _start:
        call _entry_
        mov rdi, rax
        mov eax, 60
        syscall

public MEMORY
public MEMORY_LEN

section '.data' writeable
    MEMORY_LEN dq 0

section '.bss' writeable
    MEMORY rb MEMORY_CAP
