module lisp;

import std::io;
import std::core::mem;
import std::thread;

const usz FFI_DLOPEN_REGISTRY_PATH_CAP = 256;

// =============================================================================
// SHARED DLOPEN REGISTRY (Item 10)
// Maps library path → shared dlopen handle with reference counting.
// Thread-safe via std::thread::Mutex.
// =============================================================================

struct DlopenRegistryEntry {
    char[256] path;
    void* handle;
    uint refcount;
}

DlopenRegistryEntry* g_dlopen_registry = null;
uint g_dlopen_registry_capacity = 0;
uint g_dlopen_registry_count = 0;
thread::Mutex g_dlopen_registry_mu;
thread::OnceFlag g_dlopen_registry_once;

fn void ffi_dlopen_registry_init_once() {
    if (catch err = g_dlopen_registry_mu.init()) {
        unreachable("dlopen registry mutex init failed");
    }
}

fn void ffi_dlopen_registry_lock() @inline {
    g_dlopen_registry_once.call(&ffi_dlopen_registry_init_once);
    g_dlopen_registry_mu.lock();
}

fn void ffi_dlopen_registry_unlock() @inline {
    g_dlopen_registry_mu.unlock();
}

fn bool ffi_dlopen_registry_copy_path(char[] path, char* out_path, usz out_cap) {
    if (path.len == 0 || out_path == null || out_cap == 0 || path.len >= out_cap) return false;
    for (usz i = 0; i < path.len; i++) out_path[i] = path[i];
    out_path[path.len] = 0;
    return true;
}

fn int ffi_dlopen_registry_find(char[] path) {
    if (path.len == 0 || path.len >= FFI_DLOPEN_REGISTRY_PATH_CAP) return -1;
    for (uint i = 0; i < g_dlopen_registry_count; i++) {
        char[] entry_path = g_dlopen_registry[i].path[:ffi_c_string_len(&g_dlopen_registry[i].path[0])];
        if (ffi_char_slice_equals(path, entry_path)) {
            return (int)i;
        }
    }
    return -1;
}

/**
 * Acquire a shared dlopen handle for the given library path.
 * If the path is already in the registry, increments refcount and returns
 * the existing handle. Otherwise dlopen's the library and adds it with
 * refcount = 1.
 *
 * Thread-safe.
 */
fn void* ffi_dlopen_registry_acquire(char[] path) {
    ffi_dlopen_registry_lock();
    defer ffi_dlopen_registry_unlock();

    int idx = ffi_dlopen_registry_find(path);
    if (idx >= 0) {
        g_dlopen_registry[idx].refcount++;
        return g_dlopen_registry[idx].handle;
    }
    char[FFI_DLOPEN_REGISTRY_PATH_CAP] c_path;
    if (!ffi_dlopen_registry_copy_path(path, &c_path[0], FFI_DLOPEN_REGISTRY_PATH_CAP)) return null;

    (void)dlerror();
    void* handle = dlopen((ZString)&c_path[0], RTLD_LAZY);
    if (handle == null) return null;

    if (g_dlopen_registry_count >= g_dlopen_registry_capacity) {
        if (g_dlopen_registry_capacity > uint.max / 2) return null;
        uint new_cap = g_dlopen_registry_capacity == 0 ? 4 : g_dlopen_registry_capacity * 2;
        DlopenRegistryEntry* new_entries = (DlopenRegistryEntry*)mem::realloc(
            g_dlopen_registry, DlopenRegistryEntry.sizeof * new_cap);
        if (new_entries == null) {
            dlclose(handle);
            return null;
        }
        g_dlopen_registry = new_entries;
        g_dlopen_registry_capacity = new_cap;
    }

    DlopenRegistryEntry* entry = &g_dlopen_registry[g_dlopen_registry_count];
    usz len = path.len;
    for (usz i = 0; i < len; i++) entry.path[i] = path[i];
    entry.path[len] = 0;
    entry.handle = handle;
    entry.refcount = 1;
    g_dlopen_registry_count++;

    return handle;
}

/**
 * Look up an existing registry entry without changing its refcount.
 * Returns null if the path is not registered.
 *
 * Thread-safe.
 */
fn void* ffi_dlopen_registry_lookup(char[] path) {
    ffi_dlopen_registry_lock();
    defer ffi_dlopen_registry_unlock();

    int idx = ffi_dlopen_registry_find(path);
    if (idx >= 0) return g_dlopen_registry[idx].handle;
    return null;
}

/**
 * Release a registry reference for the given library path.
 * Decrements refcount; when refcount hits 0, calls dlclose and removes
 * the entry.
 *
 * Thread-safe.
 */
fn void ffi_dlopen_registry_release(char[] path) {
    ffi_dlopen_registry_lock();
    defer ffi_dlopen_registry_unlock();

    int idx = ffi_dlopen_registry_find(path);
    if (idx < 0) return;

    DlopenRegistryEntry* entry = &g_dlopen_registry[idx];
    if (entry.refcount > 1) {
        entry.refcount--;
        return;
    }

    dlclose(entry.handle);
    for (uint i = (uint)idx; i + 1 < g_dlopen_registry_count; i++) {
        g_dlopen_registry[i] = g_dlopen_registry[i + 1];
    }
    g_dlopen_registry_count--;
}

/**
 * Release all remaining registry entries and free the registry array.
 * Called on interpreter teardown.
 *
 * Thread-safe.
 */
fn void ffi_dlopen_registry_release_all() {
    ffi_dlopen_registry_lock();
    defer ffi_dlopen_registry_unlock();

    for (uint i = 0; i < g_dlopen_registry_count; i++) {
        dlclose(g_dlopen_registry[i].handle);
    }
    mem::free(g_dlopen_registry);
    g_dlopen_registry = null;
    g_dlopen_registry_capacity = 0;
    g_dlopen_registry_count = 0;
}
