module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 5: ENVIRONMENT
// =============================================================================
// =============================================================================
// SECTION 5: ENVIRONMENT
// =============================================================================

// Initial capacity for dynamically-sized Env binding arrays
const usz ENV_INITIAL_CAPACITY = 4;

// Threshold: build hash table when binding_count exceeds this
const usz ENV_HASH_THRESHOLD = 16;

// Sentinel for empty hash table slots
const SymbolId ENV_HASH_EMPTY = 0xFFFFFFFF;

bool g_env_force_bindings_alloc_oom = false;
bool g_env_force_hash_alloc_oom = false;
bool g_env_force_barrier_promote_failure = false;

/**
 * Binding — Single name-value binding.
 */
struct Binding {
    SymbolId name;
    Value*   value;
}

/**
 * EnvHashEntry — Open-addressing hash table entry for Env.
 */
struct EnvHashEntry {
    SymbolId key;    // ENV_HASH_EMPTY = empty slot
    usz      index;  // Index into bindings array
}

// Per-boundary memo table entry used by promotion/copy routing.
struct PromotionMemoEntry {
    Value*              src;
    Value*              dst;
    PromotionMemoEntry* next;
}

// Promotion scope-chain cache entry (small fixed-size ownership-context cache).
// Used to avoid repeated chain scans for recurring membership probes with the same
// pinned generation and target scope pair while walking the same release chain.
const usz PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE = 8;

struct PromotionContextScopeChainCacheEntry {
    void*               ptr;
    main::ScopeRegion*  target_scope;
    uint                target_generation;
    uint                target_escape_generation;
    uint                pinned_generation;
    bool                in_scope_chain;
    bool                used;
}

// Shared context for one promotion/copy boundary.
// Supports nested boundaries via `prev`.
enum PromotionAbortReason : char {
    PROMOTION_ABORT_NONE,
    PROMOTION_ABORT_BUDGET_EXHAUSTED,
}

enum EnvLifetimeKind : uint {
    ENV_LIFETIME_LOCAL_SCOPE_FRAME,
    ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX,
}

struct PromotionContext {
    uint                epoch;
    usz                 budget;
    bool                aborted;
    PromotionAbortReason abort_reason;
    PromotionMemoEntry* memo_head;
    PromotionContext*   prev;
    usz scope_chain_cache_cursor;
    PromotionContextScopeChainCacheEntry[ PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE ] scope_chain_cache;
}

/**
 * Env — Linked list of environment frames.
 * Bindings array is heap-allocated and grows dynamically.
 * For frames with >ENV_HASH_THRESHOLD bindings, a hash table provides O(1) lookup.
 */
struct Env {
    Binding*              bindings;
    usz                   binding_count;
    usz                   capacity;
    Env*                  parent;
    EnvHashEntry*         hash_table;     // NULL if not using hash
    usz                   hash_capacity;  // Power of 2
    EnvLifetimeKind       lifetime_kind;  // Governs env copy semantics across scope boundaries
    uint                  scope_gen;      // Scope generation stamp for O(1) membership check
    bool                  is_inline;
    Binding[4]            inline_bindings;
}

fn bool Env.is_root_persistent_box(Env* self) @inline {
    return self != null && self.lifetime_kind == ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX;
}

fn bool Env.is_transient_scope_frame(Env* self) @inline {
    return self != null && self.lifetime_kind == ENV_LIFETIME_LOCAL_SCOPE_FRAME;
}

fn Env* make_env(Interp* interp, Env* parent) {
    // When enabled, env frames are allocated in the current scope ESCAPE lane
    // so they survive TEMP-lane resets across TCO bounces.
    bool use_escape_lane = interp.escape_env_mode;
    main::ScopeRegion* target = interp.current_scope;

    Env* env = use_escape_lane ? interp.alloc_env_escape() : interp.alloc_env();
    env.is_inline = true;
    env.bindings = &env.inline_bindings[0];
    env.binding_count = 0;
    env.capacity = 4;
    env.parent = parent;
    env.hash_table = null;
    env.hash_capacity = 0;
    env.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    env.scope_gen = use_escape_lane ? target.escape_generation : target.generation;
    if (use_escape_lane) {
        main::scope_register_dtor_escape(target, (void*)env, &scope_dtor_env);
    } else {
        main::scope_register_dtor(target, (void*)env, &scope_dtor_env);
    }
    return env;
}

/**
 * Build or rebuild the hash table for an Env frame.
 * Hash capacity is next power of 2 >= 2 * binding_count.
 */
fn bool Env.build_hash_table(Env* self) {
    // Determine capacity (next power of 2 >= 2 * count)
    usz cap = 32;
    while (cap < self.binding_count * 2) cap *= 2;

    EnvHashEntry* new_hash_table = g_env_force_hash_alloc_oom
        ? null
        : (EnvHashEntry*)mem::malloc(EnvHashEntry.sizeof * cap);
    if (new_hash_table == null) {
        if (self.hash_table != null) {
            mem::free(self.hash_table);
            self.hash_table = null;
            self.hash_capacity = 0;
        }
        return false;
    }

    // Initialize the replacement before swapping it in.
    for (usz i = 0; i < cap; i++) {
        new_hash_table[i].key = ENV_HASH_EMPTY;
    }

    // Insert all existing bindings
    usz mask = cap - 1;
    for (usz i = 0; i < self.binding_count; i++) {
        usz slot = (usz)self.bindings[i].name & mask;
        while (new_hash_table[slot].key != ENV_HASH_EMPTY) {
            slot = (slot + 1) & mask;
        }
        new_hash_table[slot].key = self.bindings[i].name;
        new_hash_table[slot].index = i;
    }

    // Swap the replacement in only after it is fully initialized.
    if (self.hash_table != null) {
        mem::free(self.hash_table);
    }
    self.hash_capacity = cap;
    self.hash_table = new_hash_table;
    return true;
}

/**
 * Look up a SymbolId in the hash table. Returns index into bindings or ~(usz)0 if not found.
 */
<* @require self.hash_table != null : "hash_lookup called without hash table" *>
fn usz Env.hash_lookup(Env* self, SymbolId name) {
    usz mask = self.hash_capacity - 1;
    usz slot = (usz)name & mask;
    for (;;) {
        SymbolId k = self.hash_table[slot].key;
        if (k == name) return self.hash_table[slot].index;
        if (k == ENV_HASH_EMPTY) return ~(usz)0;
        slot = (slot + 1) & mask;
    }
}

/**
 * Insert into the hash table (must have space — caller ensures load factor).
 */
<* @require self.hash_table != null : "hash_insert called without hash table" *>
fn void Env.hash_insert(Env* self, SymbolId name, usz index) {
    usz mask = self.hash_capacity - 1;
    usz slot = (usz)name & mask;
    while (self.hash_table[slot].key != ENV_HASH_EMPTY && self.hash_table[slot].key != name) {
        slot = (slot + 1) & mask;
    }
    self.hash_table[slot].key = name;
    self.hash_table[slot].index = index;
}

fn bool Env.define(Env* self, SymbolId name, Value* value) @nodiscard {
    // Fast path: use hash table if available
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            self.bindings[idx].value = value;
            return true;
        }
    } else {
        // Linear scan for small frames
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                self.bindings[i].value = value;
                return true;
            }
        }
    }
    // Grow if needed
    if (self.binding_count >= self.capacity) {
        usz new_cap = self.capacity * 2;
        Binding* new_bindings = g_env_force_bindings_alloc_oom
            ? null
            : (Binding*)mem::malloc(Binding.sizeof * new_cap);
        if (new_bindings == null) return false;
        for (usz i = 0; i < self.binding_count; i++) {
            new_bindings[i] = self.bindings[i];
        }
        if (!self.is_inline) {
            mem::free(self.bindings);
        }
        self.is_inline = false;
        self.bindings = new_bindings;
        self.capacity = new_cap;
    }
    usz new_idx = self.binding_count;
    self.bindings[new_idx].name = name;
    self.bindings[new_idx].value = value;
    self.binding_count++;

    // Build hash table if we crossed the threshold
    if (self.hash_table == null && self.binding_count > ENV_HASH_THRESHOLD) {
        (void)self.build_hash_table();
    } else if (self.hash_table != null) {
        // Check load factor: rebuild if > 70%
        if (self.binding_count * 10 > self.hash_capacity * 7) {
            (void)self.build_hash_table();
        } else {
            self.hash_insert(name, new_idx);
        }
    }
    return true;
}

// Barrier-aware env write helper for dual-lane rollout.
// If target env is in current scope's ESCAPE lane, promote TEMP values first.
fn Value* promote_for_env_target(Env* target, Value* value, Interp* interp) {
    if (target == null || interp == null || interp.current_scope == null) return value;
    if (value == null) return null;

    // Dual-lane intra-scope target: TEMP -> ESCAPE lane promotion.
    if (target.scope_gen == interp.current_scope.escape_generation) {
        return boundary_promote_to_escape(value, interp);
    }

    // Same TEMP-lane frame: direct store is safe.
    if (target.scope_gen == interp.current_scope.generation) {
        return value;
    }

    // Cross-scope env writes (for example child eval -> global/module env) must
    // copy into the target scope so child-scope values do not escape via env bindings.
    main::ScopeRegion* target_scope = interp.current_scope;
    while (target_scope != null &&
           target_scope.generation != target.scope_gen &&
           target_scope.escape_generation != target.scope_gen) {
        target_scope = target_scope.parent;
    }
    if (target_scope == null) return value;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = session.current_scope;
    if (COPY_SITE_ENV_PROMOTE_FOR_TARGET < COPY_SITE_COUNT) {
        g_copy_to_parent_stats.site_calls[COPY_SITE_ENV_PROMOTE_FOR_TARGET]++;
    }
    CopyParentRoute route = copy_parent_route_for_tag(value.tag);
    return copy_to_parent_by_route(value, interp, route);
}

fn bool env_barrier_promote_failed(Value* original, Value* stored) @inline {
    if (stored == null) return true;
    return original != null && original.tag != ERROR && stored.tag == ERROR;
}

fn Value* env_promote_for_target_result(Env* env, SymbolId _name, Value* value, Interp* interp, char[] failure_msg) {
    (void)_name;
    if (g_env_force_barrier_promote_failure) {
        return raise_error(interp, failure_msg);
    }

    Value* stored = promote_for_env_target(env, value, interp);
    if (env_barrier_promote_failed(value, stored)) {
        return stored != null ? stored : raise_error(interp, failure_msg);
    }
    return stored;
}

fn Value* env_define_with_barrier_result(Env* env, SymbolId name, Value* value, Interp* interp) {
    if (env == null) return raise_error(interp, "out of memory while defining binding");
    Value* stored = env_promote_for_target_result(env, name, value, interp, "out of memory while defining binding");
    if (stored != null && stored.tag == ERROR && value != stored) {
        return stored;
    }
    if (!env.define(name, stored)) {
        return raise_error(interp, "out of memory while defining binding");
    }
    return null;
}

fn bool env_define_with_barrier_checked(Env* env, SymbolId name, Value* value, Interp* interp) @nodiscard {
    return env_define_with_barrier_result(env, name, value, interp) == null;
}

fn void env_define_with_barrier(Env* env, SymbolId name, Value* value, Interp* interp) {
    assert(
        env_define_with_barrier_checked(env, name, value, interp),
        "env_define_with_barrier: binding define failed"
    );
}

// Barrier-aware env set helper for dual-lane rollout.
// Walks parent chain and applies TEMP->ESCAPE promotion at the actual write site.
fn Value* env_set_with_barrier_result(Env* env, SymbolId name, Value* value, Interp* interp, bool* found_out) {
    if (found_out != null) *found_out = false;
    Env* cur = env;
    while (cur != null) {
        if (cur.hash_table != null) {
            usz idx = cur.hash_lookup(name);
            if (idx != ~(usz)0) {
                if (found_out != null) *found_out = true;
                Value* stored = env_promote_for_target_result(cur, name, value, interp, "out of memory while mutating binding");
                if (stored != null && stored.tag == ERROR && value != stored) {
                    return stored;
                }
                cur.bindings[idx].value = stored;
                return null;
            }
        } else {
            for (usz i = 0; i < cur.binding_count; i++) {
                if (cur.bindings[i].name == name) {
                    if (found_out != null) *found_out = true;
                    Value* stored = env_promote_for_target_result(cur, name, value, interp, "out of memory while mutating binding");
                    if (stored != null && stored.tag == ERROR && value != stored) {
                        return stored;
                    }
                    cur.bindings[i].value = stored;
                    return null;
                }
            }
        }
        cur = cur.parent;
    }
    return null;
}

fn bool env_set_with_barrier_checked(Env* env, SymbolId name, Value* value, Interp* interp, bool* found_out = null) @nodiscard {
    return env_set_with_barrier_result(env, name, value, interp, found_out) == null;
}

fn void? env_set_with_barrier(Env* env, SymbolId name, Value* value, Interp* interp) {
    bool found = false;
    if (!env_set_with_barrier_checked(env, name, value, interp, &found)) {
        return BINDING_WRITE_FAILED~;
    }
    if (!found) return UNBOUND_VARIABLE~;
    return;
}

fn Value* Env.lookup(Env* self, SymbolId name) {
    // Fast path: use hash table if available
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            return self.bindings[idx].value;
        }
    } else {
        // Linear scan for small frames
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                return self.bindings[i].value;
            }
        }
    }
    // Search parent
    if (self.parent != null) {
        return self.parent.lookup(name);
    }
    return null;  // Not found
}

fn void? Env.set(Env* self, SymbolId name, Value* value) {
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            self.bindings[idx].value = value;
            return;
        }
    } else {
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                self.bindings[i].value = value;
                return;
            }
        }
    }
    if (self.parent != null) return self.parent.set(name, value);
    return UNBOUND_VARIABLE~;
}

fn Env* Env.extend(Env* self, Interp* interp, SymbolId name, Value* value) {
    Env* new_env = make_env(interp, self);
    if (!new_env.define(name, value)) return null;
    return new_env;
}

