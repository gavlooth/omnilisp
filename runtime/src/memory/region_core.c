#include "region_core.h"
#include <stdio.h>

Region* region_create(void) {
    // We allocate the Region struct itself using malloc, 
    // but the contents are managed by the internal Arena.
    // Ideally, the Region struct could be *inside* the arena, 
    // but we need the RCB to persist until the LAST ref dies, 
    // whereas the Arena might be reset. 
    // For now, simple malloc for the RCB.
    Region* r = (Region*)malloc(sizeof(Region));
    if (!r) {
        fprintf(stderr, "Fatal: OOM in region_create\n");
        exit(1);
    }
    
    // Initialize Arena (zero-init is sufficient for tsoding/arena to start)
    r->arena.begin = NULL;
    r->arena.end = NULL;
    
    // Initialize Control Block
    r->external_rc = 0;
    r->tether_count = 0;
    r->scope_alive = true;
    
    return r;
}

void region_destroy_if_dead(Region* r) {
    if (!r) return;

    // Check liveness:
    // 1. Scope must be dead (region_exit called)
    // 2. No external references (external_rc == 0)
    // 3. No active tethers (tether_count == 0)
    
    // We use atomic loads for thread safety check
    int rc = __atomic_load_n(&r->external_rc, __ATOMIC_ACQUIRE);
    int tc = __atomic_load_n(&r->tether_count, __ATOMIC_ACQUIRE);
    
    if (!r->scope_alive && rc == 0 && tc == 0) {
        arena_free(&r->arena);
        free(r);
    }
}

void region_exit(Region* r) {
    if (r) {
        r->scope_alive = false;
        region_destroy_if_dead(r);
    }
}

void region_retain_internal(Region* r) {
    if (r) {
        __atomic_add_fetch(&r->external_rc, 1, __ATOMIC_SEQ_CST);
    }
}

void region_release_internal(Region* r) {
    if (r) {
        int new_rc = __atomic_sub_fetch(&r->external_rc, 1, __ATOMIC_SEQ_CST);
        if (new_rc == 0) {
            region_destroy_if_dead(r);
        }
    }
}

void region_tether_start(Region* r) {
    if (r) {
        __atomic_add_fetch(&r->tether_count, 1, __ATOMIC_SEQ_CST);
    }
}

void region_tether_end(Region* r) {
    if (r) {
        int new_tc = __atomic_sub_fetch(&r->tether_count, 1, __ATOMIC_SEQ_CST);
        if (new_tc == 0) {
            region_destroy_if_dead(r);
        }
    }
}

void* region_alloc(Region* r, size_t size) {
    return arena_alloc(&r->arena, size);
}

// -- RegionRef Implementation --

void region_retain(RegionRef ref) {
    region_retain_internal(ref.region);
}

void region_release(RegionRef ref) {
    region_release_internal(ref.region);
}