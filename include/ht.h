#ifndef HT_H
#define HT_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifndef HT_REALLOC
#include <stdlib.h>
#define HT_REALLOC realloc
#endif
//
#ifndef HT_FREE
#include <stdlib.h>
#define HT_FREE free
#endif

#ifndef HT_ASSERT
#include <assert.h>
#define HT_ASSERT assert
#endif

#ifndef HT_LOAD_FACTOR
#define HT_LOAD_FACTOR 0.7
#endif
static_assert(HT_LOAD_FACTOR >= 0 && HT_LOAD_FACTOR <= 1, "Expected Load Factor between 0 and 1");

#ifndef HT_INIT_CAP
#define HT_INIT_CAP 256
#endif

typedef uint64_t ht_hash_fn_t(const void *key);

typedef int ht_eq_fn_t(const void *key1, const void *key2);

typedef struct {
    size_t size;
    size_t bin_count;
    size_t arena_capacity;

    int64_t *bins;
    void *arena;

    size_t key_t_size;
    size_t value_t_size;

    ht_hash_fn_t *hash;
    ht_eq_fn_t *eq;

    size_t cur_prime_idx;
} Hash_Table;

void ht_init(Hash_Table *, size_t key_t_size, size_t value_t_size,
             ht_hash_fn_t *hash, ht_eq_fn_t *eq, size_t suggested_bin_count);

void *ht_get(Hash_Table *, const void *key);
void *ht_try_get(const Hash_Table *, const void *key);

void ht_free(Hash_Table *);

#endif  // HT_H

#ifdef HT_IMPLEMENTATION

#define VOID_OFFSET(__a, __offset) (void *)((char *)(__a) + (__offset))
#define VOID_DIF(__a, __b) ((char *)__a - (char *)__b)

static const size_t ht_primes[] = {
    53, 97, 193, 389, 769, 1543, 3079, 6151,
    12289, 24593, 49157, 98317, 196613, 393241,
    786433, 1572869, 3145739, 6291469, 12582917,
    25165843, 50331653, 100663319, 201326611,
    402653189, 805306457, 1610612741};

void ht_init(Hash_Table *ht, size_t key_t_size, size_t value_t_size,
             ht_hash_fn_t *hash, ht_eq_fn_t *eq, size_t suggested_bin_count) {
    *ht = (Hash_Table){
        .key_t_size = key_t_size,
        .value_t_size = value_t_size,

        .hash = hash,
        .eq = eq,
    };

    while (ht_primes[ht->cur_prime_idx] < suggested_bin_count)
        ++ht->cur_prime_idx;

    ht->bin_count = ht_primes[ht->cur_prime_idx++];

    ht->bins = (int64_t *)HT_REALLOC(NULL, ht->bin_count * sizeof(int64_t));
    HT_ASSERT(ht->bins != NULL && "HT_REALLOC failed");
}

void *ht_get(Hash_Table *ht, const void *key) {
    size_t item_size = sizeof(int64_t) + ht->key_t_size + ht->value_t_size;

    if (ht->arena == NULL) {
        ht->arena_capacity = HT_INIT_CAP;
        ht->arena = HT_REALLOC(NULL, HT_INIT_CAP * item_size);
        HT_ASSERT(ht->arena != NULL && "HT_REALLOC failed");

        memset(ht->arena, -1, HT_INIT_CAP * item_size);
    }

    uint64_t hash = ht->hash(key);
    uint64_t bin_idx = hash % ht->bin_count;

    int64_t *last_offset = NULL;
    int64_t next = ht->bins[bin_idx];

    void *item, *cur_key, *value;

    while (next != -1) {
        item = VOID_OFFSET(ht->arena, next * item_size);
        cur_key = VOID_OFFSET(item, sizeof(int64_t));
        value = VOID_OFFSET(key, ht->key_t_size);

        if (ht->eq(key, cur_key))
            return value;

        last_offset = (int64_t *)item;
        next = *(int64_t *)item;
    }

    if (ht->size == ht->arena_capacity) {
        void *old_arena = ht->arena;
        ht->arena_capacity <<= 1;
        ht->arena = HT_REALLOC(ht->arena, ht->arena_capacity * item_size);

        if (ht->arena == NULL) {
            HT_FREE(old_arena);
            HT_ASSERT(ht->arena != NULL && "HT_REALLOC failed");
        }

        last_offset = (int64_t *)VOID_OFFSET(ht->arena, VOID_DIF(last_offset, old_arena));
    }

    // TODO: ensure load factor

    *last_offset = (int64_t)ht->size;
    item = VOID_OFFSET(ht->arena, ht->size * item_size);
    *(int64_t *)item = -1ll;
    ++ht->size;

    cur_key = VOID_OFFSET(item, sizeof(int64_t));
    memcpy(cur_key, key, ht->key_t_size);

    value = VOID_OFFSET(cur_key, ht->key_t_size);
    return value;
}

void *ht_try_get(const Hash_Table *ht, const void *key) {
    size_t item_size = sizeof(int64_t) + ht->key_t_size + ht->value_t_size;

    if (ht->arena == NULL)
        return NULL;

    uint64_t hash = ht->hash(key);
    uint64_t bin_idx = hash % ht->bin_count;

    int64_t next = ht->bins[bin_idx];

    void *item, *cur_key, *value;

    while (next != -1) {
        item = VOID_OFFSET(ht->arena, next * item_size);
        cur_key = VOID_OFFSET(item, sizeof(int64_t));
        value = VOID_OFFSET(cur_key, ht->key_t_size);

        if (ht->eq(key, cur_key))
            return value;

        next = *(int64_t *)item;
    }
    return NULL;
}

void ht_free(Hash_Table *ht) {
    HT_FREE(ht->bins);
    HT_FREE(ht->arena);
}

#endif
