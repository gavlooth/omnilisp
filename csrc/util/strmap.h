/*@semantic
id: file::csrc/util/strmap.h
kind: file
name: strmap.h
summary: String-keyed hash map header for compiler. Wrapper for runtime version.
responsibility:
  - Provide strmap API for compiler components
*/

#ifndef OMNI_CSRC_STRMAP_H
#define OMNI_CSRC_STRMAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* Include wyhash from runtime - csrc Makefile has -I../runtime/include */
#include <wyhash.h>

/*@semantic
id: struct::StrMapEntry
kind: struct
name: StrMapEntry
summary: Entry node in a string-keyed hash map bucket chain.
*/
typedef struct StrMapEntry {
    char* key;                    /* Owned copy of string key */
    void* value;                  /* Borrowed pointer to value */
    struct StrMapEntry* next;     /* Next entry in collision chain */
} StrMapEntry;

/*@semantic
id: struct::StrMap
kind: struct
name: StrMap
summary: Hash map with string keys for O(1) average-case lookup.
*/
typedef struct StrMap {
    StrMapEntry** buckets;        /* Array of bucket heads */
    size_t bucket_count;          /* Number of buckets */
    size_t entry_count;           /* Number of entries */
    float load_factor;            /* Resize threshold */
    int had_alloc_failure;        /* Flag for allocation failure */
} StrMap;

/* ========== Creation / Destruction ========== */

StrMap* strmap_new(void);
StrMap* strmap_with_capacity(size_t capacity);
void strmap_free(StrMap* map);

/* ========== Core Operations ========== */

static inline uint64_t strmap_hash(const char* key) {
    return wyhash(key, strlen(key), 0, _wyp);
}

static inline void* strmap_get(StrMap* map, const char* key) {
    if (!map || !key) return NULL;

    uint64_t hash = strmap_hash(key);
    size_t bucket = hash % map->bucket_count;

    StrMapEntry* entry = map->buckets[bucket];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return entry->value;
        }
        entry = entry->next;
    }

    return NULL;
}

static inline bool strmap_contains(StrMap* map, const char* key) {
    return strmap_get(map, key) != NULL;
}

void strmap_put(StrMap* map, const char* key, void* value);
void* strmap_remove(StrMap* map, const char* key);

/* ========== Iteration ========== */

typedef void (*StrMapIterFn)(const char* key, void* value, void* ctx);
void strmap_foreach(StrMap* map, StrMapIterFn fn, void* ctx);

/* ========== Utility ========== */

size_t strmap_size(StrMap* map);
void strmap_clear(StrMap* map);
int strmap_had_alloc_failure(StrMap* map);

#endif /* OMNI_CSRC_STRMAP_H */
