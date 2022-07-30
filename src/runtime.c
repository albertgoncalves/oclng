#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

STATIC_ASSERT(sizeof(void*) == sizeof(u64));

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
    Block* as_pointer;
    u64    as_u64;
};

STATIC_ASSERT(sizeof(Block) == sizeof(u64));
STATIC_ASSERT(sizeof(Block) == sizeof(Block*));

#define FREE_CAP 1024
#define HEAP_END (HEAP_CAP - FREE_CAP)

STATIC_ASSERT(FREE_CAP < HEAP_CAP);

extern Block HEAP_MEMORY[HEAP_CAP / sizeof(Block)];

static Block** FREE_MEMORY = &HEAP_MEMORY[HEAP_END / sizeof(Block)].as_pointer;

static u64 HEAP_LEN = 0;
static u64 FREE_LEN = 0;

Block* alloc(u16);
Block* alloc(u16 size) {
    EXIT_IF((size == 0) || ((size % sizeof(Block)) != 0));
    size += sizeof(Block);
    for (u64 offset = 0; offset < FREE_LEN; offset += sizeof(Block*)) {
        Block* block = FREE_MEMORY[offset / sizeof(Block*)];
        if ((!block->as_header.reachable) && (size <= block->as_header.size)) {
            block->as_header.reachable = TRUE;
            block->as_header.children = 0;
            return &block[1];
        }
    }
    u64 len = HEAP_LEN + size;
    EXIT_IF(HEAP_END < len);
    Block* block = &HEAP_MEMORY[HEAP_LEN / sizeof(Block)];
    block->as_header = (Header){
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

static Block* into_pointer(u64 address) {
    if ((address % 8) != 0) {
        return NULL;
    }
    address -= sizeof(Block*);
    Block* block = (Block*)address;
    if (block < HEAP_MEMORY) {
        return NULL;
    }
    if ((&HEAP_MEMORY[HEAP_END / sizeof(Block)]) <= block) {
        return NULL;
    }
    if (block->as_header.magic != 0xFF) {
        return NULL;
    }
    if (block->as_header.size == 0) {
        return NULL;
    }
    if ((block->as_header.size % sizeof(Block)) != 0) {
        return NULL;
    }
    if ((u8)(~(1 << 0)) & block->as_header.reachable) {
        return NULL;
    }
    return block;
}

static void trace(Block* block) {
    block->as_header.reachable = TRUE;
    if (!block->as_header.children) {
        return;
    }
    u32 depth = (u32)(32 - __builtin_clz(block->as_header.children));
    for (u32 i = 0; i < depth; ++i) {
        if (!(block->as_header.children & (1 << i))) {
            continue;
        }
        Block* child = into_pointer(block[i + 1].as_u64);
        EXIT_IF(!child);
        if (child->as_header.reachable) {
            continue;
        }
        trace(child);
    }
}

u64 free(u64*, u64*);
u64 free(u64* base, u64* top) {
    u64 before = 0;
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        offset += block->as_header.size;
        if (block->as_header.reachable) {
            block->as_header.reachable = FALSE;
            before += block->as_header.size;
        }
    }
    base -= 2;
    top -= 1;
    for (; top < base; --base) {
        Block* block = into_pointer(*base);
        if (!block) {
            continue;
        }
        trace(block);
    }
    u64 after = 0;
    {
        u64 len = 0;
        for (u64 offset = 0; offset < HEAP_LEN;) {
            Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
            offset += block->as_header.size;
            if (block->as_header.reachable) {
                after += block->as_header.size;
                len = offset;
            }
        }
        EXIT_IF(HEAP_END < len);
        HEAP_LEN = len;
    }
    FREE_LEN = 0;
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        offset += block->as_header.size;
        if (!block->as_header.reachable) {
            FREE_MEMORY[FREE_LEN / sizeof(Block*)] = block;
            FREE_LEN += sizeof(Block*);
            EXIT_IF(FREE_CAP < FREE_LEN);
        }
    }
    return before - after;
}

void print_heap(void);
void print_heap(void) {
    fprintf(stderr, "\nHEAP\n");
    for (u64 offset = 0; offset < HEAP_LEN;) {
        Block* block = &HEAP_MEMORY[offset / sizeof(Block)];
        fprintf(stderr,
                "    [ %p ] { reachable: %hhu, size: %hu, children: 0x%x }\n",
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
        u64 address = *base;
        fprintf(stderr, "    [ %p ] %-18p", (void*)base, (void*)address);
        Block* block = into_pointer(address);
        if (block) {
            fprintf(stderr, " -> { header: %p }", *(void**)block);
        }
        fputc('\n', stderr);
    }
}
