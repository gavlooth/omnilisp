/*@semantic
id: file::strmap.h
kind: file
name: strmap.h
summary: String-keyed hash map header providing O(1) string lookup operations.
responsibility:
  - Define StrMap data structure and API for string-keyed hash tables
  - Provide inline fast-path for get operations
inputs:
  - N/A
outputs:
  - N/A
side_effects:
  - none
tags:
  - data-structure
  - hashmap
  - string-keys
  - optimization
*/

#ifndef OMNI_STRMAP_H
#define OMNI_STRMAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* Forward declaration for wyhash - included via wyhash.h
 * Path works for both runtime/src/util and csrc/util via -I flags */
#ifndef wyhash_final_version_4_2
#ifdef __OMNI_CSRC__
#include <wyhash.h>
#else
#include "../../include/wyhash.h"
#endif
#endif

/*@semantic
id: struct::StrMapEntry
kind: struct
name: StrMapEntry
summary: Entry node in a string-keyed hash map bucket chain.
responsibility:
  - Store key-value pair with owned copy of string key
  - Link to next entry for collision chaining
data_reads:
  - key
  - value
  - next
data_writes:
  - key
  - value
  - next
lifetime:
  - key is owned (strdup'd), freed on entry removal
  - value is borrowed, not freed by strmap
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
responsibility:
  - Manage bucket array for hash distribution
  - Track entry count and load factor for resizing
  - Provide efficient string-keyed storage and retrieval
data_reads:
  - buckets
  - bucket_count
  - entry_count
data_writes:
  - buckets
  - bucket_count
  - entry_count
invariants:
  - entry_count <= bucket_count * load_factor triggers resize
  - bucket_count is always a power of 2 or close
thread_safety:
  - not thread-safe
*/
typedef struct StrMap {
    StrMapEntry** buckets;        /* Array of bucket heads */
    size_t bucket_count;          /* Number of buckets */
    size_t entry_count;           /* Number of entries */
    float load_factor;            /* Resize threshold */
    int had_alloc_failure;        /* Flag for allocation failure */
} StrMap;

/* ========== Creation / Destruction ========== */

/*@semantic
id: function::strmap_new
kind: function
name: strmap_new
summary: Create a new empty string map with default capacity.
inputs: none
outputs:
  - return: StrMap* — newly allocated map or NULL on failure
side_effects:
  - memory
lifetime:
  - caller owns returned StrMap, must call strmap_free
*/
StrMap* strmap_new(void);

/*@semantic
id: function::strmap_with_capacity
kind: function
name: strmap_with_capacity
summary: Create a new string map with specified initial capacity.
inputs:
  - capacity: size_t — initial number of buckets
outputs:
  - return: StrMap* — newly allocated map or NULL on failure
side_effects:
  - memory
lifetime:
  - caller owns returned StrMap, must call strmap_free
*/
StrMap* strmap_with_capacity(size_t capacity);

/*@semantic
id: function::strmap_free
kind: function
name: strmap_free
summary: Free a string map and all its entries (but not values).
inputs:
  - map: StrMap* — map to free
outputs: none
side_effects:
  - memory
lifetime:
  - map and all entries freed
  - values are NOT freed (caller responsibility)
*/
void strmap_free(StrMap* map);

/* ========== Core Operations ========== */

/*@semantic
id: function::strmap_hash
kind: function
name: strmap_hash
summary: Compute hash of a string key using wyhash.
inputs:
  - key: const char* — null-terminated string to hash
outputs:
  - return: uint64_t — 64-bit hash value
side_effects:
  - none
*/
static inline uint64_t strmap_hash(const char* key) {
    return wyhash(key, strlen(key), 0, _wyp);
}

/*@semantic
id: function::strmap_get
kind: function
name: strmap_get
summary: Look up a value by string key. Inlined for hot path optimization.
inputs:
  - map: StrMap* — map to search
  - key: const char* — key to look up
outputs:
  - return: void* — value if found, NULL otherwise
side_effects:
  - none
invariants:
  - map must not be NULL
  - key must be null-terminated
*/
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

/*@semantic
id: function::strmap_contains
kind: function
name: strmap_contains
summary: Check if a key exists in the map.
inputs:
  - map: StrMap* — map to search
  - key: const char* — key to check
outputs:
  - return: bool — true if key exists
side_effects:
  - none
*/
static inline bool strmap_contains(StrMap* map, const char* key) {
    return strmap_get(map, key) != NULL;
}

/*@semantic
id: function::strmap_put
kind: function
name: strmap_put
summary: Insert or update a key-value pair in the map.
inputs:
  - map: StrMap* — map to modify
  - key: const char* — key (will be copied)
  - value: void* — value to store (borrowed)
outputs: none
side_effects:
  - memory (may allocate entry and copy key)
lifetime:
  - key is copied into map-owned memory
  - value is borrowed, not owned
*/
void strmap_put(StrMap* map, const char* key, void* value);

/*@semantic
id: function::strmap_remove
kind: function
name: strmap_remove
summary: Remove a key from the map and return its value.
inputs:
  - map: StrMap* — map to modify
  - key: const char* — key to remove
outputs:
  - return: void* — removed value or NULL if not found
side_effects:
  - memory (frees entry and key copy)
lifetime:
  - entry and key copy freed
  - returned value still valid (was borrowed)
*/
void* strmap_remove(StrMap* map, const char* key);

/* ========== Iteration ========== */

/*@semantic
id: typedef::StrMapIterFn
kind: typedef
name: StrMapIterFn
summary: Callback function type for iterating over map entries.
*/
typedef void (*StrMapIterFn)(const char* key, void* value, void* ctx);

/*@semantic
id: function::strmap_foreach
kind: function
name: strmap_foreach
summary: Iterate over all entries calling a callback function.
inputs:
  - map: StrMap* — map to iterate
  - fn: StrMapIterFn — callback function
  - ctx: void* — user context passed to callback
outputs: none
side_effects:
  - depends on callback
*/
void strmap_foreach(StrMap* map, StrMapIterFn fn, void* ctx);

/* ========== Utility ========== */

/*@semantic
id: function::strmap_size
kind: function
name: strmap_size
summary: Return the number of entries in the map.
inputs:
  - map: StrMap* — map to query
outputs:
  - return: size_t — number of entries
side_effects:
  - none
*/
size_t strmap_size(StrMap* map);

/*@semantic
id: function::strmap_clear
kind: function
name: strmap_clear
summary: Remove all entries from the map.
inputs:
  - map: StrMap* — map to clear
outputs: none
side_effects:
  - memory (frees all entries)
*/
void strmap_clear(StrMap* map);

/*@semantic
id: function::strmap_had_alloc_failure
kind: function
name: strmap_had_alloc_failure
summary: Check if the map experienced an allocation failure.
inputs:
  - map: StrMap* — map to check
outputs:
  - return: int — 1 if allocation failed, 0 otherwise
side_effects:
  - none
*/
int strmap_had_alloc_failure(StrMap* map);

#endif /* OMNI_STRMAP_H */
