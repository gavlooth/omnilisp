module lisp;

import std::core::mem;

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
    if (bytes.len == usz.max) return null;
    usz cap = bytes.len + 1;
    if (cap < 32) cap = 32;
    char* copy = (char*)mem::malloc(cap);
    if (copy == null) return null;
    for (usz i = 0; i < bytes.len; i++) copy[i] = bytes[i];
    copy[bytes.len] = 0;
    return shared_handle_blob_new_owned(copy, bytes.len);
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

fn bool scheduler_copy_shared_bytes_owned(SharedHandle* handle, char** out_bytes, usz* out_len) @inline {
    return scheduler_shared_registry_copy_blob_bytes(handle, out_bytes, out_len);
}

fn bool scheduler_take_shared_bytes_owned(SharedHandle* handle, char** out_bytes, usz* out_len) @inline {
    return scheduler_copy_shared_bytes_owned(handle, out_bytes, out_len);
}

fn void shared_handle_release(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}

fn void scheduler_release_shared(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}
