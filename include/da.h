#ifndef DA_H
#define DA_H

#include <stdlib.h>
#include <string.h>

#ifndef DA_REALLOC
#include <stdlib.h>
#define DA_REALLOC realloc
#endif

#ifndef DA_FREE
#include <stdlib.h>
#define DA_FREE free
#endif

#ifndef DA_ASSERT
#include <assert.h>
#define DA_ASSERT assert
#endif

#define DYNAMIC_ARRAY_TEMPLATE(__name, __ty) \
    typedef struct {                         \
        size_t size;                         \
        size_t capacity;                     \
        __ty *store;                         \
    } __name

#define Dynamic_Array(__ty) \
    struct { \
        size_t size; \
        size_t capacity; \
        __ty *store; \
    }

#define da_init(da, init_size)                                            \
    do {                                                                  \
        (da)->size = (init_size);                                         \
        (da)->capacity = (init_size);                                     \
        (da)->store = DA_REALLOC(NULL, init_size * sizeof(*(da)->store)); \
        DA_ASSERT((da)->store != NULL);                                   \
    } while (0)

#define da_ensure_capacity(da, new_capacity)                                          \
    do {                                                                              \
        if ((da)->capacity >= new_capacity)                                           \
            break;                                                                    \
        if ((da)->capacity == 0)                                                      \
            (da)->capacity = 1;                                                       \
        else                                                                          \
            (da)->capacity <<= 1;                                                     \
        while ((da)->capacity < new_capacity)                                         \
            (da)->capacity <<= 1;                                                     \
        (da)->store = DA_REALLOC((da)->store, (da)->capacity * sizeof(*(da)->store)); \
        DA_ASSERT((da)->store != NULL);                                               \
    } while (0)

#define da_ensure_reduce_capacity(da, new_capacity)                                   \
    do {                                                                              \
        if (((da)->capacity >> 1) < new_capacity)                                     \
            break;                                                                    \
        (da)->capacity >>= 1;                                                         \
        while (((da)->capacity >> 1) >= new_capacity)                                 \
            (da)->capacity >>= 1;                                                     \
        (da)->store = DA_REALLOC((da)->store, (da)->capacity * sizeof(*(da)->store)); \
        DA_ASSERT((da)->store != NULL);                                               \
    } while (0)

#define da_append(da, value)                      \
    do {                                          \
        da_ensure_capacity((da), (da)->size + 1); \
        (da)->store[(da)->size++] = (value);      \
    } while (0)

#define da_append_buf(da, arr, n)                 \
    do {                                          \
        da_ensure_capacity((da), (da)->size + n); \
        memcpy((da)->store + (da)->size, arr, n); \
        (da)->size += n;                          \
    } while (0)

#define da_pop(da)           \
    do {                     \
        if ((da)->size != 0) \
            --(da)->size;    \
    } while (0)

#define da_clear(da)        \
    do {                    \
        (da)->size = 0;     \
        (da)->capacity = 0; \
    } while (0)

#define da_delete(da)         \
    do {                      \
        da_clear(da);         \
        DA_FREE((da)->store); \
        (da)->store = NULL;   \
    } while (0)

#endif
