module lisp;

import std::collections::list;
import std::io;
import main;

// Test-only allocation fault injection. These globals are compiled out of
// normal builds and are intentionally driven by single-threaded test code.
bool g_interp_force_macro_table_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_macro_hash_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_grow_macro_table_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_grow_macro_hash_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_module_table_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_module_hash_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_grow_module_table_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_grow_module_hash_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_handler_stack_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;
bool g_interp_force_grow_handler_stack_alloc_fail @if($feature(OMNI_TEST_FAULT_INJECTION)) = false;

fn bool interp_fault_injection_enabled() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return true;
    $else
        return false;
    $endif
}

fn void interp_force_macro_table_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_macro_table_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_macro_hash_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_macro_hash_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_grow_macro_table_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_grow_macro_table_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_grow_macro_hash_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_grow_macro_hash_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_module_table_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_module_table_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_module_hash_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_module_hash_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_grow_module_table_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_grow_module_table_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_grow_module_hash_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_grow_module_hash_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_handler_stack_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_handler_stack_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn void interp_force_grow_handler_stack_alloc_fail_for_test(bool enabled) {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        g_interp_force_grow_handler_stack_alloc_fail = enabled;
    $else
        (void)enabled;
    $endif
}

fn bool interp_force_macro_table_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_macro_table_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_macro_hash_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_macro_hash_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_grow_macro_table_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_grow_macro_table_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_grow_macro_hash_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_grow_macro_hash_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_module_table_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_module_table_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_module_hash_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_module_hash_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_grow_module_table_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_grow_module_table_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_grow_module_hash_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_grow_module_hash_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_handler_stack_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_handler_stack_alloc_fail;
    $else
        return false;
    $endif
}

fn bool interp_force_grow_handler_stack_alloc_fail_active() @inline {
    $if $feature(OMNI_TEST_FAULT_INJECTION):
        return g_interp_force_grow_handler_stack_alloc_fail;
    $else
        return false;
    $endif
}

fn usz interp_capacity_for_need(usz current, usz initial, usz needed) @inline {
    usz cap = current > 0 ? current : initial;
    while (cap < needed) {
        if (cap > usz.max / 2) return 0;
        cap *= 2;
    }
    return cap;
}

fn bool Interp.ensure_macro_capacity(Interp* self, usz needed) {
    if (self == null) return false;
    if (needed == 0) return true;
    if (self.macro_table != null &&
        self.macro_hash_index != null &&
        self.macro_capacity >= needed &&
        self.macro_hash_capacity >= self.macro_capacity * 2) {
        return true;
    }

    usz new_cap = interp_capacity_for_need(self.macro_capacity, MACRO_INITIAL_CAPACITY, needed);
    if (new_cap == 0 || new_cap > usz.max / 2) return false;
    if (new_cap > usz.max / MacroDef.sizeof) return false;
    usz new_hash_cap = new_cap * 2;
    if (new_hash_cap > usz.max / usz.sizeof) return false;

    bool growing = self.macro_table != null || self.macro_hash_index != null || self.macro_capacity > 0;
    MacroDef* new_table =
        (growing ? interp_force_grow_macro_table_alloc_fail_active() : interp_force_macro_table_alloc_fail_active())
            ? null
            : (MacroDef*)mem::malloc(MacroDef.sizeof * new_cap);
    if (new_table == null) return false;

    usz* new_hash =
        (growing ? interp_force_grow_macro_hash_alloc_fail_active() : interp_force_macro_hash_alloc_fail_active())
            ? null
            : (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    if (new_hash == null) {
        mem::free(new_table);
        return false;
    }

    for (usz i = 0; i < self.macro_count; i++) new_table[i] = self.macro_table[i];
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    for (usz i = 0; i < self.macro_count; i++) {
        usz h = (usz)new_table[i].name;
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) % new_hash_cap;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }

    mem::free(self.macro_table);
    mem::free(self.macro_hash_index);
    self.macro_table = new_table;
    self.macro_hash_index = new_hash;
    self.macro_capacity = new_cap;
    self.macro_hash_capacity = new_hash_cap;
    return true;
}

fn bool Interp.ensure_module_capacity(Interp* self, usz needed) {
    if (self == null) return false;
    if (needed == 0) return true;
    if (self.modules != null &&
        self.module_hash_index != null &&
        self.module_capacity >= needed &&
        self.module_hash_capacity >= self.module_capacity * 2) {
        return true;
    }

    usz new_cap = interp_capacity_for_need(self.module_capacity, MODULE_INITIAL_CAPACITY, needed);
    if (new_cap == 0 || new_cap > usz.max / 2) return false;
    if (new_cap > usz.max / Module.sizeof) return false;
    usz new_hash_cap = new_cap * 2;
    if (new_hash_cap > usz.max / usz.sizeof) return false;

    bool growing = self.modules != null || self.module_hash_index != null || self.module_capacity > 0;
    Module* new_modules =
        (growing ? interp_force_grow_module_table_alloc_fail_active() : interp_force_module_table_alloc_fail_active())
            ? null
            : (Module*)mem::malloc(Module.sizeof * new_cap);
    if (new_modules == null) return false;

    usz* new_hash =
        (growing ? interp_force_grow_module_hash_alloc_fail_active() : interp_force_module_hash_alloc_fail_active())
            ? null
            : (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    if (new_hash == null) {
        mem::free(new_modules);
        return false;
    }

    for (usz i = 0; i < self.module_count; i++) new_modules[i] = self.modules[i];
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    for (usz i = 0; i < self.module_count; i++) {
        usz h = (usz)new_modules[i].name;
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) % new_hash_cap;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }

    mem::free(self.modules);
    mem::free(self.module_hash_index);
    self.modules = new_modules;
    self.module_hash_index = new_hash;
    self.module_capacity = new_cap;
    self.module_hash_capacity = new_hash_cap;
    return true;
}

fn bool Interp.ensure_handler_capacity(Interp* self, usz needed) {
    if (self == null) return false;
    if (needed == 0) return true;
    if (self.handler_stack != null && self.handler_capacity >= needed) return true;

    usz new_cap = interp_capacity_for_need(self.handler_capacity, HANDLER_INITIAL_CAPACITY, needed);
    if (new_cap == 0) return false;
    if (new_cap > usz.max / EffectHandler.sizeof) return false;

    bool growing = self.handler_stack != null || self.handler_capacity > 0;
    EffectHandler* new_stack =
        (growing ? interp_force_grow_handler_stack_alloc_fail_active() : interp_force_handler_stack_alloc_fail_active())
            ? null
            : (EffectHandler*)mem::malloc(EffectHandler.sizeof * new_cap);
    if (new_stack == null) return false;

    for (usz i = 0; i < self.handler_count; i++) new_stack[i] = self.handler_stack[i];

    mem::free(self.handler_stack);
    self.handler_stack = new_stack;
    self.handler_capacity = new_cap;
    return true;
}

fn bool Interp.grow_macro_table(Interp* self) {
    usz needed = self.macro_capacity > 0 ? self.macro_capacity * 2 : MACRO_INITIAL_CAPACITY;
    return self.ensure_macro_capacity(needed);
}

fn bool Interp.grow_module_table(Interp* self) {
    usz needed = self.module_capacity > 0 ? self.module_capacity * 2 : MODULE_INITIAL_CAPACITY;
    return self.ensure_module_capacity(needed);
}

fn bool Interp.grow_handler_stack(Interp* self) {
    usz needed = self.handler_capacity > 0 ? self.handler_capacity * 2 : HANDLER_INITIAL_CAPACITY;
    return self.ensure_handler_capacity(needed);
}

fn bool interp_handle_state_seen_before(Continuation* head, Continuation* stop, void* state) {
    Continuation* it = head;
    while (it != null && it != stop) {
        if (it.handle_state == state) return true;
        it = it.next_owned;
    }
    return false;
}

fn void interp_invalidate_live_continuations(Interp* self) {
    Continuation* head = self.continuation_head;
    Continuation* k = head;
    while (k != null) {
        void* hstate = k.handle_state;
        if (hstate != null && runtime_handle_state_is_live(hstate)) {
            runtime_handle_state_invalidate_continuation(hstate, k.handle_retained);
        }
        // StackCtx objects are reclaimed by stack_pool_shutdown().
        k.ctx = null;
        k.owner_interp = null;
        k.handle_retained = false;
        k = k.next_owned;
    }

    // Release each shared handle state once after all continuation decrements.
    k = head;
    while (k != null) {
        void* hstate = k.handle_state;
        if (hstate != null &&
            runtime_handle_state_is_live(hstate) &&
            !interp_handle_state_seen_before(head, k, hstate) &&
            runtime_handle_state_continuation_refcount(hstate) == 0) {
            runtime_handle_state_release(hstate);
        }
        k = k.next_owned;
    }

    k = head;
    while (k != null) {
        k.handle_state = null;
        k = k.next_owned;
    }
    self.continuation_head = null;
}

fn void Interp.destroy(Interp* self) {
    if (runtime_interp_on_current_stack(self)) {
        assert(!runtime_interp_has_active_exec(self), "Interp.destroy: cannot destroy interpreter with active JIT frames on current stack");
    }
    assert(
        !scheduler_has_interp_owned_fibers(self),
        "Interp.destroy: scheduler still owns fibers/results for this interpreter"
    );
    // Escaped continuations may retain handler snapshots beyond lexical frame
    // lifetime; invalidate and release those references before root-scope teardown.
    interp_invalidate_live_continuations(self);
    // JIT cache keys are Expr* identities from the AST arena. Clear cache
    // before arena teardown so stale pointers cannot survive destruction.
    runtime_cache_clear();
    ast_arena_destroy(&self.ast_arena);

    // Stack continuations/coroutines can retain scope guards through deferred
    // cleanup entries. Release those guards before root teardown so retained
    // child scopes still participate in normal region destruction.
    main::stack_pool_run_deferred_destroy_all(&self.stack_ctx_pool);

    // Release root scope last among value-owned structures so registered dtors run.
    // This is the primary deterministic cleanup point for scope-managed values/envs.
    if (self.root_scope != null) {
        self.current_scope = self.root_scope;
        self.releasing_scope = null;
        self.tco_recycle_scope = null;
        self.tco_scope_defer_slot = 0;
        self.tco_scope_defer_active = false;
        self.active_promotion_ctx = null;
        if (self.stable_escape_store != null) {
            stable_escape_store_reset(self);
        }
        main::scope_destroy_owned_root(self.root_scope);
        self.root_scope = null;
        self.current_scope = null;
    }

    self.symbols.destroy();
    self.types.destroy();
    if (self.macro_table != null) {
        for (usz i = 0; i < self.macro_count; i++) macro_def_destroy(&self.macro_table[i]);
        mem::free(self.macro_table);
        self.macro_table = null;
    }
    if (self.macro_hash_index != null) { mem::free(self.macro_hash_index); self.macro_hash_index = null; }
    if (self.modules != null) {
        for (usz i = 0; i < self.module_count; i++) {
            if (self.modules[i].exports != null) {
                mem::free(self.modules[i].exports);
            }
            if (self.modules[i].path != null) {
                mem::free(self.modules[i].path);
                self.modules[i].path = null;
            }
        }
        mem::free(self.modules);
        self.modules = null;
    }
    if (self.source_dirs != null) {
        for (usz i = 0; i < self.source_dir_count; i++) {
            if (self.source_dirs[i] != null) mem::free(self.source_dirs[i]);
        }
        mem::free(self.source_dirs);
        self.source_dirs = null;
    }
    if (self.console_capture_buf != null) {
        mem::free(self.console_capture_buf);
        self.console_capture_buf = null;
        self.console_capture_len = 0;
        self.console_capture_cap = 0;
        self.console_capture_active = false;
    }
    if (self.stable_escape_store != null) {
        mem::free(self.stable_escape_store);
        self.stable_escape_store = null;
    }
    self.input_state = null;
    self.sigint_requested = 0;
    self.interrupt_flag = null;
    self.signal_handles_head = null;
    if (self.module_hash_index != null) { mem::free(self.module_hash_index); self.module_hash_index = null; }
    if (self.handler_stack != null) { mem::free(self.handler_stack); self.handler_stack = null; }
    main::stack_pool_shutdown(&self.stack_ctx_pool);
    runtime_detach_interp(self);
    runtime_global_shutdown();
}
