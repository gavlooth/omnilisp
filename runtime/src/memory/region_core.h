#ifndef OMNI_REGION_CORE_H
#define OMNI_REGION_CORE_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "../../../third_party/arena/arena.h"

// Region Control Block (RCB)
// The logical owner of a memory region.
typedef struct Region {
    Arena arena;                // The physical storage (bump allocator)
    int external_rc;            // Strong refs from OTHER regions/stack (atomic)
    int tether_count;           // Temporary "borrows" by threads (atomic)
    bool scope_alive;           // True if the semantic scope is still active
} Region;

// Lifecycle
Region* region_create(void);
void region_destroy_if_dead(Region* r);

// Scope Management
void region_exit(Region* r);

// RC Management (Internal - use RegionRef for high level)
void region_retain_internal(Region* r);
void region_release_internal(Region* r);

// Tethering
void region_tether_start(Region* r);
void region_tether_end(Region* r);

// Allocation
void* region_alloc(Region* r, size_t size);

#endif // OMNI_REGION_CORE_H
