module main;

import std::io;
import std::thread;

// =============================================================================
// Global freelist
// =============================================================================

ScopeRegion* g_scope_freelist = null;
usz g_scope_freelist_count = 0;
const usz SCOPE_FREELIST_MAX = 64;  // Don't hoard too many recycled scopes

// Global monotonic generation counter — ensures every scope gets a unique generation.
// Used by Value.scope_gen stamps for O(1) scope membership checks.
uint g_scope_generation_counter = 0;

thread::Mutex g_scope_global_mu;
thread::OnceFlag g_scope_global_once;
bool g_scope_global_mu_destroyed = false;
tlocal char g_scope_thread_marker;

fn void scope_global_mu_init_once() {
    assert(!g_scope_global_mu_destroyed, "scope global mutex used after shutdown cleanup");
    if (catch err = g_scope_global_mu.init()) {
        // The global scope lock guards shared region state; continuing without it
        // would make scope freelist/generation ownership invariants unsound.
        unreachable("scope global mutex init failed");
    }
}

fn void scope_global_lock() @inline {
    assert(!g_scope_global_mu_destroyed, "scope global mutex used after shutdown cleanup");
    g_scope_global_once.call(&scope_global_mu_init_once);
    g_scope_global_mu.lock();
}

fn void scope_global_unlock() @inline {
    g_scope_global_mu.unlock();
}

fn void scope_global_destroy_after_shutdown() {
    if (g_scope_global_mu_destroyed) return;
    g_scope_global_once.call(&scope_global_mu_init_once);
    assert(g_scope_global_mu.is_initialized(), "scope global mutex destroy requires initialized mutex");
    g_scope_global_mu.destroy();
    g_scope_global_mu_destroyed = true;
}

fn bool scope_freelist_recycle_enabled() @inline {
    // Under ASAN prefer direct free over freelist reuse to reduce stale-pointer
    // reuse noise and surface the first invalid writer.
    $if env::ADDRESS_SANITIZER:
        return false;
    $else
        return true;
    $endif
}

fn void* scope_current_thread_token() @inline {
    return (void*)&g_scope_thread_marker;
}

fn void scope_thread_violation(char[] op) @inline {
    io::eprintfn("scope-region thread-affinity violation in %s", op);
    unreachable("scope-region accessed from non-owner thread");
}

fn void scope_require_owner(ScopeRegion* scope, char[] op) @inline {
    if (scope == null) return;
    if (scope.owner_thread_token != scope_current_thread_token()) {
        scope_thread_violation(op);
    }
}

macro scope_guard_owner(scope, op) {
    scope_require_owner(scope, op);
}

macro scope_invariant_refcount_live(scope) {
    assert(scope.refcount > 0, "scope invariant: refcount must be > 0");
}

macro scope_invariant_distinct_scopes(parent, child) {
    assert(parent != child, "scope invariant: parent and child must differ");
}
