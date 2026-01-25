/*@semantic
id: file::strmap.c
kind: file
name: strmap.c
summary: String-keyed hash map implementation providing O(1) average-case operations.
responsibility:
  - Implement string map creation, insertion, lookup, and deletion
  - Handle dynamic resizing on load factor threshold
  - Manage memory for copied string keys
inputs:
  - N/A
outputs:
  - N/A
side_effects:
  - memory
tags:
  - data-structure
  - hashmap
  - string-keys
  - optimization
*/

#include "strmap.h"
#include <stdlib.h>
#include <string.h>

/*@semantic
id: macro::STRMAP_INITIAL_BUCKETS
kind: macro
name: STRMAP_INITIAL_BUCKETS
summary: Default number of buckets for new string maps.
*/
#define STRMAP_INITIAL_BUCKETS 64

/*@semantic
id: macro::STRMAP_MAX_LOAD_FACTOR
kind: macro
name: STRMAP_MAX_LOAD_FACTOR
summary: Load factor threshold triggering resize (0.75 = 75% full).
*/
#define STRMAP_MAX_LOAD_FACTOR 0.75f

/*@semantic
id: function::strmap_resize
kind: function
name: strmap_resize
summary: Double the bucket count and rehash all entries.
inputs:
  - map: StrMap* — map to resize
outputs: none
side_effects:
  - memory (allocates new bucket array)
data_reads:
  - map->buckets
  - map->bucket_count
data_writes:
  - map->buckets
  - map->bucket_count
  - map->had_alloc_failure
invariants:
  - bucket_count doubles on success
error_handling:
  - sets had_alloc_failure on OOM, keeps old buckets
*/
static void strmap_resize(StrMap* map) {
    /* Overflow check before doubling */
    if (map->bucket_count > SIZE_MAX / 2) {
        map->had_alloc_failure = 1;
        return;
    }

    size_t new_count = map->bucket_count * 2;
    StrMapEntry** new_buckets = calloc(new_count, sizeof(StrMapEntry*));
    if (!new_buckets) {
        map->had_alloc_failure = 1;
        return;
    }

    /*@semantic
    id: block::strmap_resize::rehash_loop
    kind: block
    summary: Rehash all existing entries into new bucket array.
    data_reads:
      - map->buckets
      - entry->key
    data_writes:
      - new_buckets
      - entry->next
    */
    for (size_t i = 0; i < map->bucket_count; i++) {
        StrMapEntry* entry = map->buckets[i];
        while (entry) {
            StrMapEntry* next = entry->next;
            uint64_t hash = strmap_hash(entry->key);
            size_t idx = hash % new_count;
            entry->next = new_buckets[idx];
            new_buckets[idx] = entry;
            entry = next;
        }
    }

    free(map->buckets);
    map->buckets = new_buckets;
    map->bucket_count = new_count;
}

/*@semantic
id: function::strmap_new
kind: function
name: strmap_new
summary: Create a new empty string map with default capacity (64 buckets).
*/
StrMap* strmap_new(void) {
    return strmap_with_capacity(STRMAP_INITIAL_BUCKETS);
}

/*@semantic
id: function::strmap_with_capacity
kind: function
name: strmap_with_capacity
summary: Create a new string map with specified initial bucket count.
*/
StrMap* strmap_with_capacity(size_t capacity) {
    StrMap* map = malloc(sizeof(StrMap));
    if (!map) return NULL;

    if (capacity < 16) capacity = 16;

    map->buckets = calloc(capacity, sizeof(StrMapEntry*));
    if (!map->buckets) {
        free(map);
        return NULL;
    }

    map->bucket_count = capacity;
    map->entry_count = 0;
    map->load_factor = STRMAP_MAX_LOAD_FACTOR;
    map->had_alloc_failure = 0;
    return map;
}

/*@semantic
id: function::strmap_free
kind: function
name: strmap_free
summary: Free map, all entries, and copied keys. Values are not freed.
*/
void strmap_free(StrMap* map) {
    if (!map) return;

    /*@semantic
    id: block::strmap_free::entry_cleanup
    kind: block
    summary: Free all entries and their copied keys.
    data_reads:
      - map->buckets
    data_writes:
      - entry->key (freed)
    */
    for (size_t i = 0; i < map->bucket_count; i++) {
        StrMapEntry* entry = map->buckets[i];
        while (entry) {
            StrMapEntry* next = entry->next;
            free(entry->key);
            free(entry);
            entry = next;
        }
    }

    free(map->buckets);
    free(map);
}

/*@semantic
id: function::strmap_put
kind: function
name: strmap_put
summary: Insert or update a key-value pair. Key is copied.
*/
void strmap_put(StrMap* map, const char* key, void* value) {
    if (!map || !key) return;

    /*@semantic
    id: block::strmap_put::load_check
    kind: block
    summary: Check load factor and resize if needed.
    */
    if ((float)map->entry_count / map->bucket_count > map->load_factor) {
        strmap_resize(map);
    }

    uint64_t hash = strmap_hash(key);
    size_t idx = hash % map->bucket_count;
    StrMapEntry* entry = map->buckets[idx];

    /*@semantic
    id: block::strmap_put::update_check
    kind: block
    summary: Search bucket for existing key to update.
    */
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }

    /*@semantic
    id: block::strmap_put::insert_new
    kind: block
    summary: Allocate new entry with copied key and insert at bucket head.
    */
    entry = malloc(sizeof(StrMapEntry));
    if (!entry) {
        map->had_alloc_failure = 1;
        return;
    }

    entry->key = strdup(key);
    if (!entry->key) {
        free(entry);
        map->had_alloc_failure = 1;
        return;
    }

    entry->value = value;
    entry->next = map->buckets[idx];
    map->buckets[idx] = entry;
    map->entry_count++;
}

/*@semantic
id: function::strmap_remove
kind: function
name: strmap_remove
summary: Remove key from map, returning its value. Frees entry and key copy.
*/
void* strmap_remove(StrMap* map, const char* key) {
    if (!map || !key) return NULL;

    uint64_t hash = strmap_hash(key);
    size_t idx = hash % map->bucket_count;
    StrMapEntry** prev = &map->buckets[idx];
    StrMapEntry* entry = *prev;

    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            void* value = entry->value;
            *prev = entry->next;
            free(entry->key);
            free(entry);
            map->entry_count--;
            return value;
        }
        prev = &entry->next;
        entry = entry->next;
    }

    return NULL;
}

/*@semantic
id: function::strmap_foreach
kind: function
name: strmap_foreach
summary: Iterate all entries, calling fn(key, value, ctx) for each.
*/
void strmap_foreach(StrMap* map, StrMapIterFn fn, void* ctx) {
    if (!map || !fn) return;

    for (size_t i = 0; i < map->bucket_count; i++) {
        StrMapEntry* entry = map->buckets[i];
        while (entry) {
            fn(entry->key, entry->value, ctx);
            entry = entry->next;
        }
    }
}

/*@semantic
id: function::strmap_size
kind: function
name: strmap_size
summary: Return the number of entries in the map.
*/
size_t strmap_size(StrMap* map) {
    return map ? map->entry_count : 0;
}

/*@semantic
id: function::strmap_clear
kind: function
name: strmap_clear
summary: Remove all entries from the map.
*/
void strmap_clear(StrMap* map) {
    if (!map) return;

    for (size_t i = 0; i < map->bucket_count; i++) {
        StrMapEntry* entry = map->buckets[i];
        while (entry) {
            StrMapEntry* next = entry->next;
            free(entry->key);
            free(entry);
            entry = next;
        }
        map->buckets[i] = NULL;
    }
    map->entry_count = 0;
}

/*@semantic
id: function::strmap_had_alloc_failure
kind: function
name: strmap_had_alloc_failure
summary: Check if map experienced allocation failure.
*/
int strmap_had_alloc_failure(StrMap* map) {
    return map ? map->had_alloc_failure : 0;
}
