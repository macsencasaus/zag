#ifndef BUMPALLOC_H
#define BUMPALLOC_H

#include <stddef.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef BUMPALLOC_MAX_ALIGN
#include <stdalign.h>
#define BUMPALLOC_MAX_ALIGN sizeof(max_align_t)
#endif

#ifndef BUMPALLOC_THRESH
#define BUMPALLOC_THRESH 64
#endif

#ifndef BUMPALLOC_ASSERT
#include <assert.h>
#define BUMPALLOC_ASSERT assert
#endif

#define MIN_CHUNK_CAPACITY sysconf(_SC_PAGESIZE)

typedef struct Bump_Alloc_Chunk Bump_Alloc_Chunk;

struct Bump_Alloc_Chunk {
    Bump_Alloc_Chunk *next;
    size_t size;
    size_t capacity;
    uint8_t data[];
};

typedef struct {
    Bump_Alloc_Chunk *head;
    Bump_Alloc_Chunk *next_avail;
} Bump_Alloc;

void *ba_alloc(Bump_Alloc *, size_t size);
void *ba_alloc_aligned(Bump_Alloc *, size_t size, size_t alignment);
void ba_free(Bump_Alloc *);

#endif  // BUMPALLOC_H

#ifdef BUMPALLOC_IMPLEMENTATION

static Bump_Alloc_Chunk *bump_alloc_new_chunk(size_t initial_size) {
    size_t page_size = MIN_CHUNK_CAPACITY;
    size_t chunk_min_size = sizeof(Bump_Alloc_Chunk) + initial_size;
    size_t chunk_size = (chunk_min_size + page_size - 1) / page_size * page_size;

    Bump_Alloc_Chunk *chunk = mmap(NULL, chunk_size, PROT_READ | PROT_WRITE,
                                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    BUMPALLOC_ASSERT(chunk && "mmap failed to map chunk");

    chunk->capacity = chunk_size - sizeof(Bump_Alloc_Chunk);

    return chunk;
}

void *ba_alloc(Bump_Alloc *ba, size_t size) {
    return ba_alloc_aligned(ba, size, BUMPALLOC_MAX_ALIGN);
}

void *ba_alloc_aligned(Bump_Alloc *ba, size_t size, size_t alignment) {
    BUMPALLOC_ASSERT((alignment > 0) && ((alignment & (alignment - 1)) == 0));

    if (!ba->next_avail) {
        ba->next_avail = bump_alloc_new_chunk(size + alignment - 1);

        if (!ba->head)
            ba->head = ba->next_avail;
    }

    Bump_Alloc_Chunk *chunk = ba->next_avail;
    uintptr_t ptr = (uintptr_t)(chunk->data + chunk->size);
    uintptr_t aligned = (ptr + alignment - 1) & ~(alignment - 1);
    size_t padding = aligned - ptr;

    uintptr_t end = (uintptr_t)(chunk->data + chunk->capacity);
    if (aligned + size > end) {
        ba->next_avail = chunk->next = bump_alloc_new_chunk(size + alignment - 1);
        chunk = chunk->next;

        ptr = (uintptr_t)(chunk->data + chunk->size);
        aligned = (ptr + alignment - 1) & ~(alignment - 1);
        padding = aligned - ptr;
    }

    chunk->size += padding + size;

    return (void *)aligned;
}

void ba_free(Bump_Alloc *ba) {
    Bump_Alloc_Chunk *chunk = ba->head, *next;
    for (; chunk; chunk = next) {
        next = chunk->next;
        BUMPALLOC_ASSERT(munmap(chunk, sizeof(Bump_Alloc_Chunk) + chunk->capacity) == 0);
    }
}

#endif  // BUMPALLOC_IMPLEMENTATION
