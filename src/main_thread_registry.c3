module main;

// =============================================================================
// SECTION 13: THREAD-LOCAL REGISTRY
// =============================================================================

fn void thread_registry_init() {
    // Old region registry is retired from active runtime paths.
    // Keep init as an explicit no-op while entry flows are converged.
}

fn void thread_registry_shutdown() {
    // ScopeRegion structs are recycled in a global freelist.
    // Flush it at shutdown so sanitizer runs report real leaks only.
    scope_freelist_cleanup();
}
