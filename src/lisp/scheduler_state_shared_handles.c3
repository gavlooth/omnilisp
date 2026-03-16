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

    // Fallback safety in case a handle exists before registry availability.
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

fn SharedHandleBlob* shared_handle_as_blob(SharedHandle* handle) @inline {
    return scheduler_shared_registry_resolve_blob(handle);
}

fn SharedHandle* shared_handle_blob_new_owned(char* owned_bytes, usz len) {
    if (owned_bytes == null) return null;
    if (!scheduler_shared_registry_ensure_ready()) {
        mem::free(owned_bytes);
        return null;
    }

    SharedHandleBlob* payload = (SharedHandleBlob*)mem::malloc(SharedHandleBlob.sizeof);
    if (payload == null) {
        mem::free(owned_bytes);
        return null;
    }
    payload.refcount.store(1);
    payload.bytes = owned_bytes;
    payload.len = len;

    usz slot = SHARED_REGISTRY_CAPACITY;
    uint generation = 0;
    if (!scheduler_shared_registry_alloc(SHARED_HANDLE_BLOB, payload, &slot, &generation)) {
        mem::free(payload.bytes);
        mem::free(payload);
        return null;
    }

    SharedHandle* handle = (SharedHandle*)mem::malloc(SharedHandle.sizeof);
    if (handle == null) {
        (void)scheduler_shared_registry_destroy_slot(slot, generation, payload);
        return null;
    }
    handle.kind = SHARED_HANDLE_BLOB;
    handle.slot = slot;
    handle.generation = generation;
    handle.payload = payload;
    return handle;
}

fn SharedHandle* shared_handle_blob_new_copy(char[] bytes) {
    usz cap = bytes.len + 1;
    if (cap < 32) cap = 32;
    char* copy = (char*)mem::malloc(cap);
    if (copy == null) return null;
    for (usz i = 0; i < bytes.len; i++) copy[i] = bytes[i];
    copy[bytes.len] = 0;
    return shared_handle_blob_new_owned(copy, bytes.len);
}

fn char[] shared_handle_blob_bytes(SharedHandle* handle, usz* out_len) @inline {
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null) {
        if (out_len != null) *out_len = 0;
        return "";
    }
    if (out_len != null) *out_len = payload.len;
    return payload.bytes[:payload.len];
}

fn SharedHandle* scheduler_publish_shared_bytes_owned(char* owned_bytes, usz len) @inline {
    return shared_handle_blob_new_owned(owned_bytes, len);
}

fn SharedHandle* scheduler_publish_shared_bytes_copy(char[] bytes) @inline {
    return shared_handle_blob_new_copy(bytes);
}

fn SharedHandle* scheduler_publish_shared(char[] bytes) @inline {
    return scheduler_publish_shared_bytes_copy(bytes);
}

fn SharedHandle* scheduler_domain_publish_shared(SchedulerDomain* domain, char[] bytes) @inline {
    if (!scheduler_domain_is_local(domain)) return null;
    scheduler_domain_safe_point(domain);
    return scheduler_publish_shared(bytes);
}

fn SharedHandle* scheduler_domain_publish_shared_owned(SchedulerDomain* domain, char* owned_bytes, usz len) @inline {
    if (!scheduler_domain_is_local(domain)) {
        if (owned_bytes != null) mem::free(owned_bytes);
        return null;
    }
    scheduler_domain_safe_point(domain);
    return scheduler_publish_shared_bytes_owned(owned_bytes, len);
}

fn bool scheduler_project_shared_bytes_view(SharedHandle* handle, char[]* out_bytes) @inline {
    if (out_bytes != null) *out_bytes = "";
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null) return false;
    if (out_bytes != null) *out_bytes = payload.bytes[:payload.len];
    return true;
}

fn bool scheduler_project_shared(SharedHandle* handle, char[]* out_bytes) @inline {
    return scheduler_project_shared_bytes_view(handle, out_bytes);
}

fn bool scheduler_take_shared_bytes_owned(SharedHandle* handle, char** out_bytes, usz* out_len) @inline {
    if (out_bytes != null) *out_bytes = null;
    if (out_len != null) *out_len = 0;
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null || out_bytes == null) return false;

    usz len = payload.len;
    char* owned = (char*)mem::malloc(len + 1);
    if (owned == null) return false;
    for (usz i = 0; i < len; i++) owned[i] = payload.bytes[i];
    owned[len] = 0;
    *out_bytes = owned;
    if (out_len != null) *out_len = payload.len;
    return true;
}

fn void shared_handle_clear_blob(SharedHandle* handle) @inline {
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null) return;
    payload.bytes = null;
    payload.len = 0;
}

fn void shared_handle_release(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}

fn void scheduler_release_shared(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}

fn long scheduler_publish_sendable_int(long value) @inline {
    return value;
}

fn long scheduler_domain_publish_sendable_int(SchedulerDomain* domain, long value) @inline {
    if (!scheduler_domain_is_local(domain)) return value;
    scheduler_domain_safe_point(domain);
    return scheduler_publish_sendable_int(value);
}

fn Value* scheduler_consume_sendable_int(long value, Interp* interp) @inline {
    return make_int(interp, value);
}

fn Value* scheduler_domain_consume_sendable_int(SchedulerDomain* domain, long value, Interp* interp) @inline {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-sendable-consume", "scheduler: invalid domain sendable consume");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_consume_sendable_int(value, interp);
}

fn Value* scheduler_project_shared_to_local_value(SharedHandle** handle_ref, Interp* interp) {
    if (handle_ref == null || *handle_ref == null) {
        return make_string(interp, "");
    }

    SharedHandle* handle = *handle_ref;
    switch (handle.kind) {
        case SHARED_HANDLE_BLOB:
            char* owned = null;
            usz len = 0;
            if (!scheduler_take_shared_bytes_owned(handle, &owned, &len)) {
                scheduler_release_shared(handle);
                *handle_ref = null;
                return make_string(interp, "");
            }

            scheduler_release_shared(handle);
            *handle_ref = null;
            return make_string_owned(interp, owned, len);
    }
}

fn Value* scheduler_domain_project_shared_to_local_value(SchedulerDomain* domain, SharedHandle** handle_ref, Interp* interp) {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-shared-project", "scheduler: invalid domain shared projection");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_project_shared_to_local_value(handle_ref, interp);
}
