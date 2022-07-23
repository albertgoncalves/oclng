#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

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

extern u64 MEMORY[MEMORY_CAP / 8];
extern u64 MEMORY_LEN;

u64* alloc(u64);
u64* alloc(u64 size) {
    EXIT_IF((size == 0) || ((size % 8) != 0));
    u64 len = MEMORY_LEN + size;
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    MEMORY_LEN = len;
    return block;
}
