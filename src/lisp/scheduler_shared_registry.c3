module lisp;

import std::core::mem;

fn void scheduler_shared_registry_free_payload(SharedHandleBlob* payload) @inline {
    if (payload == null) return;
    if (payload.bytes != null) mem::free(payload.bytes);
    mem::free(payload);
}

fn bool scheduler_shared_registry_enqueue_retired_payload_locked(
    SharedRegistry* registry,
    usz slot,
    uint generation,
    SharedHandleBlob* payload
) @inline {
    if (registry == null || payload == null) return false;
    if (registry.retire_count >= SHARED_RETIRED_QUEUE_CAPACITY) {
        registry.retire_overflow_count++;
        return false;
    }
    SharedRetiredPayload* retired = &registry.retired[registry.retire_tail];
    retired.slot = slot;
    retired.generation = generation;
    retired.payload = payload;
    registry.retire_tail = (registry.retire_tail + 1) % SHARED_RETIRED_QUEUE_CAPACITY;
    registry.retire_count++;
    return true;
}

fn void scheduler_shared_registry_drain_retired(usz budget = 64) {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized) return;

    SharedHandleBlob*[SHARED_RETIRED_QUEUE_CAPACITY] to_free;
    usz to_free_count = 0;
    usz drain_limit = budget;
    if (drain_limit == 0) return;
    if (drain_limit > SHARED_RETIRED_QUEUE_CAPACITY) {
        drain_limit = SHARED_RETIRED_QUEUE_CAPACITY;
    }

    registry.mu.lock();
    while (registry.retire_count > 0 && to_free_count < drain_limit) {
        SharedRetiredPayload* retired = &registry.retired[registry.retire_head];
        if (retired.payload != null) {
            to_free[to_free_count++] = retired.payload;
        }
        retired.slot = SHARED_REGISTRY_CAPACITY;
        retired.generation = 0;
        retired.payload = null;
        registry.retire_head = (registry.retire_head + 1) % SHARED_RETIRED_QUEUE_CAPACITY;
        registry.retire_count--;
    }
    registry.mu.unlock();

    for (usz i = 0; i < to_free_count; i++) {
        scheduler_shared_registry_free_payload(to_free[i]);
    }
}

fn void scheduler_shared_registry_shutdown() {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized) return;

    SharedHandleBlob*[SHARED_REGISTRY_CAPACITY + SHARED_RETIRED_QUEUE_CAPACITY] to_free;
    usz to_free_count = 0;

    registry.mu.lock();
    for (usz i = 0; i < SHARED_REGISTRY_CAPACITY; i++) {
        SharedRegistryEntry* entry = &registry.entries[i];
        if (entry.active && entry.payload != null) {
            to_free[to_free_count++] = entry.payload;
        }
        entry.active = false;
        entry.kind = SHARED_HANDLE_BLOB;
        entry.generation = scheduler_shared_registry_next_generation(entry.generation);
        entry.payload = null;
    }
    while (registry.retire_count > 0) {
        SharedRetiredPayload* retired = &registry.retired[registry.retire_head];
        if (retired.payload != null) {
            to_free[to_free_count++] = retired.payload;
        }
        retired.slot = SHARED_REGISTRY_CAPACITY;
        retired.generation = 0;
        retired.payload = null;
        registry.retire_head = (registry.retire_head + 1) % SHARED_RETIRED_QUEUE_CAPACITY;
        registry.retire_count--;
    }
    registry.retire_head = 0;
    registry.retire_tail = 0;
    registry.retire_overflow_count = 0;
    registry.next_slot = 0;
    registry.initialized = false;
    registry.mu.unlock();
    (void)registry.mu.destroy();

    for (usz i = 0; i < to_free_count; i++) {
        scheduler_shared_registry_free_payload(to_free[i]);
    }
}

fn void scheduler_domain_safe_point(SchedulerDomain* domain) @inline {
    if (!scheduler_domain_is_local(domain)) return;
    scheduler_shared_registry_drain_retired(64);
}

fn uint scheduler_shared_registry_next_generation(uint current) @inline {
    if (current == uint.max) return 1;
    return current + 1;
}

fn bool scheduler_shared_registry_ensure_ready() {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (registry.initialized) return true;
    if (catch err = registry.mu.init()) return false;
    registry.initialized = true;
    registry.next_slot = 0;
    registry.retire_head = 0;
    registry.retire_tail = 0;
    registry.retire_count = 0;
    registry.retire_overflow_count = 0;
    return true;
}

fn bool scheduler_shared_registry_alloc(
    SharedHandleKind kind,
    SharedHandleBlob* payload,
    usz* out_slot,
    uint* out_generation
) {
    if (!scheduler_shared_registry_ensure_ready()) return false;
    SharedRegistry* registry = &g_scheduler.shared_registry;
    registry.mu.lock();
    defer registry.mu.unlock();

    for (usz i = 0; i < SHARED_REGISTRY_CAPACITY; i++) {
        usz slot = (registry.next_slot + i) % SHARED_REGISTRY_CAPACITY;
        SharedRegistryEntry* entry = &registry.entries[slot];
        if (entry.active) continue;

        uint generation = entry.generation;
        if (generation == 0) generation = 1;

        entry.active = true;
        entry.kind = kind;
        entry.generation = generation;
        entry.payload = payload;
        registry.next_slot = (slot + 1) % SHARED_REGISTRY_CAPACITY;

        if (out_slot != null) *out_slot = slot;
        if (out_generation != null) *out_generation = generation;
        return true;
    }
    return false;
}

fn void scheduler_shared_registry_retire_entry(SharedRegistryEntry* entry) @inline {
    entry.active = false;
    entry.payload = null;
    entry.generation = scheduler_shared_registry_next_generation(entry.generation);
}

fn bool scheduler_shared_registry_destroy_slot(usz slot, uint generation, SharedHandleBlob* expected_payload = null) {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized || slot >= SHARED_REGISTRY_CAPACITY) return false;

    registry.mu.lock();
    defer registry.mu.unlock();

    SharedRegistryEntry* entry = &registry.entries[slot];
    if (!entry.active || entry.generation != generation) return false;
    if (expected_payload != null && entry.payload != expected_payload) return false;

    SharedHandleBlob* payload = entry.payload;
    scheduler_shared_registry_retire_entry(entry);
    if (payload != null &&
        !scheduler_shared_registry_enqueue_retired_payload_locked(registry, slot, generation, payload)) {
        scheduler_shared_registry_free_payload(payload);
    }
    return true;
}

fn SharedHandleBlob* scheduler_shared_registry_resolve_blob(SharedHandle* handle) {
    if (handle == null || handle.kind != SHARED_HANDLE_BLOB) return null;
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized || handle.slot >= SHARED_REGISTRY_CAPACITY) return null;

    registry.mu.lock();
    defer registry.mu.unlock();

    SharedRegistryEntry* entry = &registry.entries[handle.slot];
    if (!entry.active || entry.generation != handle.generation || entry.kind != handle.kind) return null;
    if (handle.payload != null && entry.payload != handle.payload) return null;
    return entry.payload;
}

fn bool scheduler_shared_registry_copy_blob_bytes(SharedHandle* handle, char** out_bytes, usz* out_len) {
    if (out_bytes != null) *out_bytes = null;
    if (out_len != null) *out_len = 0;
    if (handle == null || handle.kind != SHARED_HANDLE_BLOB || out_bytes == null) return false;

    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized || handle.slot >= SHARED_REGISTRY_CAPACITY) return false;

    registry.mu.lock();
    defer registry.mu.unlock();

    SharedRegistryEntry* entry = &registry.entries[handle.slot];
    if (!entry.active || entry.generation != handle.generation || entry.kind != handle.kind) return false;
    if (handle.payload != null && entry.payload != handle.payload) return false;

    SharedHandleBlob* payload = entry.payload;
    if (payload == null || payload.bytes == null) return false;

    usz len = payload.len;
    if (len == usz.max) return false;
    char* owned = (char*)mem::malloc(len + 1);
    if (owned == null) return false;
    for (usz i = 0; i < len; i++) owned[i] = payload.bytes[i];
    owned[len] = 0;

    *out_bytes = owned;
    if (out_len != null) *out_len = len;
    return true;
}

fn bool scheduler_shared_destroy(SharedHandle* handle) {
    if (handle == null) return false;

    SharedRegistry* registry = &g_scheduler.shared_registry;
    bool released = false;
    if (registry.initialized && handle.slot < SHARED_REGISTRY_CAPACITY) {
        registry.mu.lock();
        SharedRegistryEntry* entry = &registry.entries[handle.slot];
        if (entry.active &&
            entry.generation == handle.generation &&
            entry.kind == handle.kind &&
            entry.payload != null &&
            (handle.payload == null || entry.payload == handle.payload)) {
            SharedHandleBlob* payload = entry.payload;
            long old = payload.refcount.add(-1);
            if (old <= 1) {
                scheduler_shared_registry_retire_entry(entry);
                if (!scheduler_shared_registry_enqueue_retired_payload_locked(
                    registry,
                    handle.slot,
                    handle.generation,
                    payload
                )) {
                    scheduler_shared_registry_free_payload(payload);
                }
            }
            released = true;
        }
        registry.mu.unlock();
    }

    if (!released && (!registry.initialized || handle.slot >= SHARED_REGISTRY_CAPACITY) && handle.payload != null) {
        long old = handle.payload.refcount.add(-1);
        if (old <= 1) {
            if (handle.payload.bytes != null) mem::free(handle.payload.bytes);
            mem::free(handle.payload);
        }
    }

    mem::free(handle);
    return released;
}

fn bool scheduler_shared_retire(SharedHandle* handle) {
    if (handle == null) return false;
    bool retired = scheduler_shared_registry_destroy_slot(handle.slot, handle.generation, handle.payload);
    mem::free(handle);
    return retired;
}
