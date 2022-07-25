#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

typedef uint32_t u32;
typedef uint64_t u64;

#define ERROR 1

#define EXIT_IF(condition)             \
    {                                  \
        if (condition) {               \
            fflush(stdout);            \
            fprintf(stderr,            \
                    "%s:%s:%d `%s`\n", \
                    __FILE__,          \
                    __func__,          \
                    __LINE__,          \
                    #condition);       \
            _exit(ERROR);              \
        }                              \
    }

typedef union {
    struct {
        u32 size;
        u32 refs;
    } as_struct;
    u64 as_u64;
} Header;

extern u64 MEMORY[MEMORY_CAP / 8];
extern u64 MEMORY_LEN;

u64* alloc(u32);
u64* alloc(u32 size) {
    EXIT_IF((size == 0) || ((size % 8) != 0));
    size += 8;
    u64 len = MEMORY_LEN + size;
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = ((Header){.as_struct = {.size = size, .refs = 0}}).as_u64;
    MEMORY_LEN = len;
    return &block[1];
}

u32 ref_incr(u64*);
u32 ref_incr(u64* block) {
    return ++((Header*)&block[-1])->as_struct.refs;
}

u32 ref_decr(u64*);
u32 ref_decr(u64* block) {
    return --((Header*)&block[-1])->as_struct.refs;
}

void print_memory(void);
void print_memory(void) {
    u64 i = 0;
    for (u64 offset = 0; offset < MEMORY_LEN;) {
        Header* header = (Header*)&MEMORY[offset / 8];
        fprintf(stderr,
                "%4lu { size:%u, refs:%u }\n",
                i++,
                header->as_struct.size,
                header->as_struct.refs);
        offset += header->as_struct.size;
    }
}
