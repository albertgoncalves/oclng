#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#define ERROR 1

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

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

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

STATIC_ASSERT(sizeof(Bool) == sizeof(u8));

typedef union {
    struct {
        u32  children;
        u16  size;
        Bool reachable;
        u8   magic;
    } as_struct;
    u64 as_u64;
} Header;

STATIC_ASSERT(sizeof(Header) == sizeof(u64));

extern u64 HEAP_MEMORY[HEAP_CAP / 8];
extern u64 HEAP_LEN;

u64* alloc(u16);
u64* alloc(u16 size) {
    EXIT_IF((size == 0) || ((size % 8) != 0));
    size += 8;
    u64 len = HEAP_LEN + size;
    EXIT_IF(HEAP_CAP < len);
    u64* block = &HEAP_MEMORY[HEAP_LEN / 8];
    block[0] =
        ((Header){
             .as_struct = {.magic = 0xFF, .reachable = TRUE, .size = size}})
            .as_u64;
    HEAP_LEN = len;
    return &block[1];
}

void set_child(u64*, u8);
void set_child(u64* block, u8 child) {
    EXIT_IF(31 < child);
    Header* header = (Header*)&block[-1];
    header->as_struct.children |= 1 << child;
}

static Header* get_header(u64* block) {
    Header* header = (Header*)block;
    if (header->as_struct.magic != 0xFF) {
        return NULL;
    }
    if ((u8)(~(1 << 0)) & header->as_struct.reachable) {
        return NULL;
    }
    return header;
}

void print_heap(void);
void print_heap(void) {
    fprintf(stderr, "\nHEAP\n");
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Header* header = get_header(&HEAP_MEMORY[offset / 8]);
        EXIT_IF(!header);
        fprintf(stderr,
                "    %p - { reachable: %hhu, size:%hu, children:0x%x }\n",
                (void*)header,
                header->as_struct.reachable,
                header->as_struct.size,
                header->as_struct.children);
        offset += header->as_struct.size;
    }
}

static Bool within_heap(u64* block) {
    return (HEAP_MEMORY <= block) && (block < (&HEAP_MEMORY[HEAP_CAP / 8]));
}

void print_stack(u64*, u64*);
void print_stack(u64* base, u64* top) {
    fprintf(stderr, "\nSTACK\n");
    --base;
    --top;
    for (; top < base; --base) {
        fprintf(stderr, "    %p - %p", (void*)base, *(void**)base);
        u64* block = *(u64**)base;
        if (within_heap(block)) {
            Header* header = get_header(&block[-1]);
            if (header) {
                fprintf(stderr, " - HEAP { header: %p }", *(void**)header);
            }
        }
        fputc('\n', stderr);
    }
}
