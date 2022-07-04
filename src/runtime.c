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

u64* pack_1(u64);
u64* pack_2(u64, u64);
u64* pack_3(u64, u64, u64);
u64* pack_4(u64, u64, u64, u64);
u64* pack_5(u64, u64, u64, u64, u64);
u64* pack_6(u64, u64, u64, u64, u64, u64);

u64* pack_1(u64 a) {
    u64 len = MEMORY_LEN + 8;
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    MEMORY_LEN = len;
    return block;
}

u64* pack_2(u64 a, u64 b) {
    u64 len = MEMORY_LEN + (8 * 2);
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    block[1] = b;
    MEMORY_LEN = len;
    return block;
}

u64* pack_3(u64 a, u64 b, u64 c) {
    u64 len = MEMORY_LEN + (8 * 3);
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    block[1] = b;
    block[2] = c;
    MEMORY_LEN = len;
    return block;
}

u64* pack_4(u64 a, u64 b, u64 c, u64 d) {
    u64 len = MEMORY_LEN + (8 * 4);
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    block[1] = b;
    block[2] = c;
    block[3] = d;
    MEMORY_LEN = len;
    return block;
}

u64* pack_5(u64 a, u64 b, u64 c, u64 d, u64 e) {
    u64 len = MEMORY_LEN + (8 * 5);
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    block[1] = b;
    block[2] = c;
    block[3] = d;
    block[4] = e;
    MEMORY_LEN = len;
    return block;
}

u64* pack_6(u64 a, u64 b, u64 c, u64 d, u64 e, u64 f) {
    u64 len = MEMORY_LEN + (8 * 6);
    EXIT_IF(MEMORY_CAP < len);
    u64* block = &MEMORY[MEMORY_LEN / 8];
    block[0] = a;
    block[1] = b;
    block[2] = c;
    block[3] = d;
    block[4] = e;
    block[5] = f;
    MEMORY_LEN = len;
    return block;
}
