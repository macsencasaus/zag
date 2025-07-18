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

#ifndef DA_INIT_CAPACITY
#define DA_INIT_CAPACITY 32
#endif

#define DYNAMIC_ARRAY_TEMPLATE(__name, __ty) \
    typedef struct {                         \
        size_t size;                         \
        size_t capacity;                     \
        __ty *store;                         \
    } __name

#define Dynamic_Array(__ty) \
    struct {                \
        size_t size;        \
        size_t capacity;    \
        __ty *store;        \
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
        if ((da)->capacity >= (new_capacity))                                         \
            break;                                                                    \
        if ((da)->capacity == 0)                                                      \
            (da)->capacity = DA_INIT_CAPACITY;                                        \
        else                                                                          \
            (da)->capacity <<= 1;                                                     \
        while ((da)->capacity < (new_capacity))                                       \
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

#define da_append_buf(da, arr, n)                                            \
    do {                                                                     \
        da_ensure_capacity((da), (da)->size + (n));                          \
        memcpy((da)->store + (da)->size, (arr), (n) * sizeof(*(da)->store)); \
        (da)->size += (n);                                                   \
    } while (0)

#define da_pop(da)           \
    do {                     \
        if ((da)->size != 0) \
            --(da)->size;    \
    } while (0)

#define da_at(da, idx) ((da)->store + (idx))

#define da_last(da) ((da)->store + (da)->size - 1)

#define da_remove(da, idx) ((da)->store[(idx)] = (da)->store[--(da)->size])

#define da_delete(da) \
    DA_FREE((da)->store);

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(*arr))

#define Array(__T, __Cap) \
    struct {              \
        __T store[__Cap]; \
        size_t len;       \
    }

#define Array_Template(__T, __Cap, __Name) \
    typedef Array(__T, __Cap) __Name

#define append(fa, v)                                   \
    do {                                                \
        DA_ASSERT((fa)->len < ARRAY_SIZE((fa)->store)); \
        (fa)->store[(fa)->len++] = v;                   \
    } while (0)

#define can_append(fa) ((fa)->len < ARRAY_SIZE((fa)->store))

#define at(fa, idx) ((fa)->store + (idx))

#define last(fa) ((fa)->store + (fa)->len - 1)

#define remove(fa, idx) ((fa)->store[(idx)] = (fa)->store[--(fa)->len])

#endif
