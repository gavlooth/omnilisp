module lisp;

import main;

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
    main::ScopeRegion*    owner_scope;    // Owning scope authority for barrier-targeted promotion/copy
    bool                  owner_escape_lane; // True when frame was allocated in owner_scope ESCAPE lane
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
    env.owner_scope = target;
    env.owner_escape_lane = use_escape_lane;
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
