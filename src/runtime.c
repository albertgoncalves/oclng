#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

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

typedef struct {
    u32  children;
    u16  size;
    Bool reachable;
    u8   magic;
} Header;

STATIC_ASSERT(sizeof(Bool) == sizeof(u8));

typedef union Block Block;

union Block {
    Header as_header;
    Block* as_child;
};

STATIC_ASSERT(sizeof(Block) == sizeof(u64));

extern Block HEAP_MEMORY[HEAP_CAP / sizeof(Block)];
extern u64   HEAP_LEN;

static Bool within_heap(Block* block) {
    return (HEAP_MEMORY <= block) && (block < (&HEAP_MEMORY[HEAP_CAP / 8]));
}

static Bool valid(Header header) {
    return (header.magic == 0xFF) && (!((u8)(~(1 << 0)) & header.reachable));
}

Block* alloc(u16);
Block* alloc(u16 size) {
    EXIT_IF((size == 0) || ((size % sizeof(Block)) != 0));
    size += sizeof(Block);
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        EXIT_IF(!valid(block->as_header));
        if ((!block->as_header.reachable) && (size <= block->as_header.size)) {
            block->as_header.reachable = TRUE;
            block->as_header.children = 0;
            return &block[1];
        }
        offset += block->as_header.size;
    }
    u64 len = HEAP_LEN + size;
    EXIT_IF(HEAP_CAP < len);
    Block* block = &HEAP_MEMORY[HEAP_LEN / sizeof(Block)];
    block[0].as_header = (Header){
        .magic = 0xFF,
        .reachable = TRUE,
        .size = size,
        .children = 0,
    };
    HEAP_LEN = len;
    return &block[1];
}

void set_child(Block*, u8);
void set_child(Block* block, u8 child) {
    EXIT_IF(31 < child);
    block[-1].as_header.children |= 1 << child;
}

static void trace_children(Block* block) {
    for (u32 i = 0; i < 32; ++i) {
        u32 mask = 1 << i;
        if (block->as_header.children < mask) {
            break;
        }
        if (block->as_header.children & mask) {
            Block* child = &block[i + 1].as_child[-1];
            EXIT_IF(!(within_heap(child) && valid(child->as_header)));
            if (child->as_header.reachable) {
                continue;
            }
            child->as_header.reachable = TRUE;
            trace_children(child);
        }
    }
}

u64 free(u64*, u64*);
u64 free(u64* base, u64* top) {
    u64 before = 0;
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        EXIT_IF(!valid(block->as_header));
        if (block->as_header.reachable) {
            block->as_header.reachable = FALSE;
            before += block->as_header.size;
        }
        offset += block->as_header.size;
    }
    base -= 2;
    top -= 1;
    for (; top < base; --base) {
        Block* block = &(*(Block**)base)[-1];
        if (!within_heap(block)) {
            continue;
        }
        EXIT_IF(!valid(block->as_header));
        block->as_header.reachable = TRUE;
    }
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        EXIT_IF(!valid(block->as_header));
        if (block->as_header.reachable) {
            trace_children(block);
        }
        offset += block->as_header.size;
    }
    u64 after = 0;
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        EXIT_IF(!valid(block->as_header));
        if (block->as_header.reachable) {
            after += block->as_header.size;
        }
        offset += block->as_header.size;
    }
    return before - after;
}

void print_heap(void);
void print_heap(void) {
    fprintf(stderr, "\nHEAP\n");
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        EXIT_IF(!valid(block->as_header));
        fprintf(stderr,
                "    %p - { reachable: %hhu, size:%hu, children:0x%x }\n",
                (void*)block,
                block->as_header.reachable,
                block->as_header.size,
                block->as_header.children);
        offset += block->as_header.size;
    }
}

void print_stack(u64*, u64*);
void print_stack(u64* base, u64* top) {
    fprintf(stderr, "\nSTACK\n");
    base -= 2;
    top -= 1;
    for (; top < base; --base) {
        fprintf(stderr, "    %p - %p", (void*)base, *(void**)base);
        Block* block = &(*(Block**)base)[-1];
        if (within_heap(block) && valid(block->as_header)) {
            fprintf(stderr, " - HEAP { header: %p }", *(void**)block);
        }
        fputc('\n', stderr);
    }
}
