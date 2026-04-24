module lisp;

fn bool stable_escape_store_passport_snapshot(Interp* interp, StableEscapeHandle* handle, StableGraphPassport* out_passport) {
    if (handle == null || out_passport == null || handle.kind != STABLE_ESCAPE_HANDLE_ROOT_VALUE) return false;
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null || !store.initialized || handle.slot >= STABLE_ESCAPE_STORE_CAPACITY) return false;
    store.mu.lock();
    defer store.mu.unlock();
    StableEscapeStoreEntry* entry = &store.entries[handle.slot];
    if (!entry.active || entry.kind != handle.kind || entry.generation != handle.generation || entry.root != handle.root) return false;
    StableGraphPassportInvalidationReason reason = STABLE_GRAPH_PASSPORT_VALID;
    if (entry.owner_scope == null || entry.owner_scope.generation == 0) {
        reason = STABLE_GRAPH_PASSPORT_OWNER_SCOPE_INVALID;
    } else if (entry.owner_scope.escape_generation != entry.owner_escape_generation) {
        reason = STABLE_GRAPH_PASSPORT_OWNER_ESCAPE_CHANGED;
    } else if (!stable_escape_store_root_publishable_in_owner_scope(entry.root, entry.owner_scope)) {
        reason = STABLE_GRAPH_PASSPORT_ROOT_NOT_IN_OWNER_CHAIN;
    } else if (!stable_escape_store_prepared_nodes_alive(entry)) {
        reason = STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE;
    }
    entry.passport.invalidation_reason = reason;
    *out_passport = entry.passport;
    return true;
}
