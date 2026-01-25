/*@semantic
id: file::csrc/util/strmap.c
kind: file
name: strmap.c
summary: String-keyed hash map implementation for compiler.
*/

#include "strmap.h"
#include <stdlib.h>
#include <string.h>

#define STRMAP_INITIAL_BUCKETS 64
#define STRMAP_MAX_LOAD_FACTOR 0.75f

static void strmap_resize(StrMap* map) {
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

StrMap* strmap_new(void) {
    return strmap_with_capacity(STRMAP_INITIAL_BUCKETS);
}

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

void strmap_free(StrMap* map) {
    if (!map) return;

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

void strmap_put(StrMap* map, const char* key, void* value) {
    if (!map || !key) return;

    if ((float)map->entry_count / map->bucket_count > map->load_factor) {
        strmap_resize(map);
    }

    uint64_t hash = strmap_hash(key);
    size_t idx = hash % map->bucket_count;
    StrMapEntry* entry = map->buckets[idx];

    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }

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

size_t strmap_size(StrMap* map) {
    return map ? map->entry_count : 0;
}

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

int strmap_had_alloc_failure(StrMap* map) {
    return map ? map->had_alloc_failure : 0;
}
