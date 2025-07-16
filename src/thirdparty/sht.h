#ifndef SHT_H
#define SHT_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef sht_REALLOC
#include <stdlib.h>
#define SHT_REALLOC realloc
#endif
//
#ifndef SHT_FREE
#include <stdlib.h>
#define SHT_FREE free
#endif

#ifndef SHT_ASSERT
#include <assert.h>
#define SHT_ASSERT assert
#endif

#ifndef SHT_STRNCMP
#include <string.h>
#define SHT_STRNCMP strncmp
#endif

#ifndef SHT_LOAD_FACTOR
#define SHT_LOAD_FACTOR 0.7
#endif
static_assert(SHT_LOAD_FACTOR >= 0 && SHT_LOAD_FACTOR <= 1, "Expected Load Factor between 0 and 1");

#ifndef SHT_INIT_B_CAP  // in bytes
#define SHT_INIT_B_CAP 1024 * 1024
#endif

typedef struct {
    size_t size;
    size_t b_size;
    size_t bin_count;
    size_t arena_b_capacity;

    int64_t *bins;
    void *arena;

    size_t value_t_size;

    size_t cur_prime_idx;
} String_Hash_Table;

void sht_init(String_Hash_Table *, size_t value_t_size,
              size_t suggested_bin_count);

void *sht_get(String_Hash_Table *sht,
              const char *key,
              size_t key_t_size);

void *sht_try_get(const String_Hash_Table *sht,
                  const char *key,
                  size_t key_t_size);

void sht_free(String_Hash_Table *);

#endif  // SHT_H

#ifdef SHT_IMPLEMENTATION

#define VOID_OFFSET(__a, __offset) (void *)((char *)(__a) + (__offset))
#define VOID_DIF(__a, __b) ((char *)__a - (char *)__b)

static const size_t sht_primes[] = {
    53, 97, 193, 389, 769, 1543, 3079, 6151,
    12289, 24593, 49157, 98317, 196613, 393241,
    786433, 1572869, 3145739, 6291469, 12582917,
    25165843, 50331653, 100663319, 201326611,
    402653189, 805306457, 1610612741};

#define DJB2_PRIME 5381ull

static uint64_t sht_djb2_hash(const char *key, size_t key_t_size) {
    uint64_t hash = DJB2_PRIME;
    for (size_t i = 0; i < key_t_size; ++i) {
        hash = ((hash << 5) + hash) ^ ((size_t)key[i]);
    }
    return hash;
}

void sht_init(String_Hash_Table *ht, size_t value_t_size,
              size_t suggested_bin_count) {
    *ht = (String_Hash_Table){
        .value_t_size = value_t_size,
    };

    while (sht_primes[ht->cur_prime_idx] < suggested_bin_count)
        ++ht->cur_prime_idx;

    ht->bin_count = sht_primes[ht->cur_prime_idx++];

    ht->bins = (int64_t *)SHT_REALLOC(NULL, ht->bin_count * sizeof(int64_t));
    SHT_ASSERT(ht->bins != NULL && "HT_REALLOC failed");
    memset(ht->bins, -1, ht->bin_count * sizeof(int64_t));
}

void *sht_get(String_Hash_Table *ht, const char *key, size_t key_t_size) {
    size_t item_size = sizeof(int64_t) + key_t_size + ht->value_t_size;

    if (ht->arena == NULL) {
        ht->arena_b_capacity = SHT_INIT_B_CAP;
        ht->arena = SHT_REALLOC(NULL, SHT_INIT_B_CAP);
        SHT_ASSERT(ht->arena != NULL && "HT_REALLOC failed");

        memset(ht->arena, -1, SHT_INIT_B_CAP);
    }

    uint64_t hash = sht_djb2_hash(key, key_t_size);
    uint64_t bin_idx = hash % ht->bin_count;

    int64_t *last_offset = ht->bins + bin_idx;
    int64_t next = *last_offset;

    void *item, *value;
    char *cur_key;

    while (next != -1) {
        item = VOID_OFFSET(ht->arena, next);
        cur_key = (char *)VOID_OFFSET(item, sizeof(int64_t));
        value = VOID_OFFSET(cur_key, key_t_size);

        if (SHT_STRNCMP(key, cur_key, key_t_size) == 0)
            return value;

        last_offset = (int64_t *)item;
        next = *(int64_t *)item;
    }

    if (ht->b_size == ht->arena_b_capacity) {
        void *old_arena = ht->arena;
        ht->arena_b_capacity <<= 1;
        ht->arena = SHT_REALLOC(ht->arena, ht->arena_b_capacity);

        if (ht->arena == NULL) {
            SHT_FREE(old_arena);
            SHT_ASSERT(ht->arena != NULL && "HT_REALLOC failed");
        }

        last_offset = (int64_t *)VOID_OFFSET(ht->arena, VOID_DIF(last_offset, old_arena));
    }

    // TODO: ensure load factor

    *last_offset = (int64_t)ht->b_size;
    item = VOID_OFFSET(ht->arena, ht->b_size);
    *(int64_t *)item = -1ll;
    ++ht->size;
    ht->b_size += item_size;

    cur_key = (char *)VOID_OFFSET(item, sizeof(int64_t));
    memcpy(cur_key, key, key_t_size);

    value = VOID_OFFSET(cur_key, key_t_size);
    return value;
}

void *sht_try_get(const String_Hash_Table *ht, const char *key, size_t key_t_size) {
    if (ht->arena == NULL)
        return NULL;

    uint64_t hash = sht_djb2_hash(key, key_t_size);
    uint64_t bin_idx = hash % ht->bin_count;

    int64_t next = ht->bins[bin_idx];

    void *item, *value;
    const char *cur_key;

    while (next != -1) {
        item = VOID_OFFSET(ht->arena, next);
        cur_key = (char *)VOID_OFFSET(item, sizeof(int64_t));
        value = VOID_OFFSET(cur_key, key_t_size);

        if (SHT_STRNCMP(key, cur_key, key_t_size) == 0)
            return value;

        next = *(int64_t *)item;
    }
    return NULL;
}

void sht_free(String_Hash_Table *ht) {
    SHT_FREE(ht->bins);
    SHT_FREE(ht->arena);
}

#endif
