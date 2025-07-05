#ifndef SB_H
#define SB_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef SB_INIT_CAP
#define SB_INIT_CAP 512
#endif

#ifndef SB_REALLOC
#define SB_REALLOC realloc
#endif

#ifndef SB_FREE
#define SB_FREE free
#endif

#ifndef SB_ASSERT
#include <assert.h>
#define SB_ASSERT assert
#endif

typedef struct {
    char *store;
    size_t size;
    size_t capacity;
} String_Builder;

#define sb_init(__sb) *(__sb) = (String_Builder){0}

char *sb_append_buf(String_Builder *, const char *buf, size_t n);
void sb_reserve(String_Builder *, size_t additional_size);

static inline void sb_append(String_Builder *sb, char c) {
    sb_append_buf(sb, (char[]){c}, 1);
}

#define sb_append_sv(sb, sv) \
    sb_append_buf((sb), (sv)->store, (sv)->len)

int sb_appendf(String_Builder *, const char *fmt, ...);

#define sb_append_cstr(s, buf)        \
    do {                              \
        size_t n = strlen(buf);       \
        sb_append_buf((s), (buf), n); \
    } while (0)

#define sb_append_null(sb) sb_append_buf((sb), "", 1)

#define sb_pop_last(sb) --(sb)->size

#define sb_pop(sb, n) (sb)->size -= (n)

#define sb_free(s) SB_FREE((s)->store)

typedef struct {
    const char *store;
    size_t len;
} String_View;

bool sveq(String_View sv1, String_View sv2);
String_View sv_from_cstr(char *str);

#endif  // SB_H

#ifdef SB_IMPLEMENTATION

char *sb_append_buf(String_Builder *sb, const char *buf, size_t n) {
    sb_reserve(sb, n);
    char *res = sb->store + sb->size;
    memcpy(res, buf, n);
    sb->size += n;
    return res;
}

void sb_reserve(String_Builder *sb, size_t additional_size) {
    if (sb->size + additional_size <= sb->capacity)
        return;

    if (sb->size == 0)
        sb->capacity = SB_INIT_CAP;

    while (sb->size + additional_size > sb->capacity)
        sb->capacity <<= 1;

    sb->store = SB_REALLOC(sb->store, sb->capacity);
    SB_ASSERT(sb->store != NULL && "SB_REALLOC failed");
}

int sb_appendf(String_Builder *sb, const char *fmt, ...) {
    va_list vargs;
    va_start(vargs, fmt);
    int n = vsnprintf(NULL, 0, fmt, vargs);
    va_end(vargs);

    sb_reserve(sb, n + 1);

    va_start(vargs, fmt);
    vsnprintf(sb->store + sb->size, n + 1, fmt, vargs);
    va_end(vargs);

    sb->size += n;

    return n;
}

int sb_vappendf(String_Builder *sb, const char *fmt, va_list vargs) {
    va_list vargs_copy;
    va_copy(vargs_copy, vargs);

    int n = vsnprintf(NULL, 0, fmt, vargs_copy);
    va_end(vargs_copy);

    sb_reserve(sb, n + 1);

    va_copy(vargs_copy, vargs);
    vsnprintf(sb->store + sb->size, n + 1, fmt, vargs_copy);
    va_end(vargs_copy);

    sb->size += n;
    return n;
}

bool sveq(String_View sv1, String_View sv2) {
    if (sv1.len != sv2.len) {
        return false;
    }
    return strncmp(sv1.store, sv2.store, sv1.len) == 0;
}

String_View sv_from_cstr(char *str) {
    return (String_View) {
        .store = str,
        .len = strlen(str),
    };
}

#endif
