module lisp;

import std::collections::list;
import std::io;
import main;

fn bool interp_checked_mul_usz(usz left, usz right, usz* out) @inline {
    if (out == null) return false;
    if (left == 0 || right == 0) {
        *out = 0;
        return true;
    }
    if (left > usz.max / right) return false;
    *out = left * right;
    return true;
}

fn bool Interp.grow_macro_table(Interp* self) {
    if (self.macro_capacity == 0 || self.macro_capacity > usz.max / 2) return false;
    usz new_cap = self.macro_capacity * 2;
    if (new_cap > usz.max / MacroDef.sizeof) return false;
    MacroDef* old_table = self.macro_table;
    usz* old_hash = self.macro_hash_index;
    usz table_bytes = 0;
    if (!interp_checked_mul_usz(MacroDef.sizeof, new_cap, &table_bytes)) return false;
    MacroDef* new_table = (MacroDef*)mem::malloc(table_bytes);
    if (new_table == null) return false;
    for (usz i = 0; i < self.macro_count; i++) new_table[i] = old_table[i];

    // Rebuild hash
    if (new_cap > usz.max / 2) {
        mem::free(new_table);
        return false;
    }
    usz new_hash_cap = new_cap * 2;
    usz hash_bytes = 0;
    if (!interp_checked_mul_usz(usz.sizeof, new_hash_cap, &hash_bytes)) {
        mem::free(new_table);
        return false;
    }
    usz* new_hash = (usz*)mem::malloc(hash_bytes);
    if (new_hash == null) {
        mem::free(new_table);
        return false;
    }
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    for (usz i = 0; i < self.macro_count; i++) {
        usz slot = (usz)new_table[i].name % new_hash_cap;
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
            slot = (slot + 1) % new_hash_cap;
        }
    }

    self.macro_table = new_table;
    self.macro_capacity = new_cap;
    self.macro_hash_index = new_hash;
    self.macro_hash_capacity = new_hash_cap;
    mem::free(old_table);
    mem::free(old_hash);
    return true;
}

fn bool Interp.grow_module_table(Interp* self) {
    if (self.module_capacity == 0 || self.module_capacity > usz.max / 2) return false;
    usz new_cap = self.module_capacity * 2;
    if (new_cap > usz.max / Module.sizeof) return false;
    Module* old_mods = self.modules;
    usz* old_hash = self.module_hash_index;
    usz table_bytes = 0;
    if (!interp_checked_mul_usz(Module.sizeof, new_cap, &table_bytes)) return false;
    Module* new_mods = (Module*)mem::malloc(table_bytes);
    if (new_mods == null) return false;
    for (usz i = 0; i < self.module_count; i++) new_mods[i] = old_mods[i];

    // Rebuild hash
    if (new_cap > usz.max / 2) {
        mem::free(new_mods);
        return false;
    }
    usz new_hash_cap = new_cap * 2;
    usz hash_bytes = 0;
    if (!interp_checked_mul_usz(usz.sizeof, new_hash_cap, &hash_bytes)) {
        mem::free(new_mods);
        return false;
    }
    usz* new_hash = (usz*)mem::malloc(hash_bytes);
    if (new_hash == null) {
        mem::free(new_mods);
        return false;
    }
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    for (usz i = 0; i < self.module_count; i++) {
        if (new_mods[i].name == (SymbolId)0 || new_mods[i].name == INVALID_SYMBOL_ID) continue;
        usz slot = (usz)new_mods[i].name % new_hash_cap;
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
            slot = (slot + 1) % new_hash_cap;
        }
    }

    self.modules = new_mods;
    self.module_capacity = new_cap;
    self.module_hash_index = new_hash;
    self.module_hash_capacity = new_hash_cap;
    mem::free(old_mods);
    mem::free(old_hash);
    return true;
}

fn bool Interp.grow_handler_stack(Interp* self) {
    if (self.handler_capacity == 0 || self.handler_capacity > usz.max / 2) return false;
    usz new_cap = self.handler_capacity * 2;
    EffectHandler* old_stack = self.handler_stack;
    usz stack_bytes = 0;
    if (!interp_checked_mul_usz(EffectHandler.sizeof, new_cap, &stack_bytes)) return false;
    EffectHandler* new_stack = (EffectHandler*)mem::malloc(stack_bytes);
    if (new_stack == null) return false;
    for (usz i = 0; i < self.handler_count; i++) new_stack[i] = old_stack[i];
    self.handler_stack = new_stack;
    self.handler_capacity = new_cap;
    mem::free(old_stack);
    return true;
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

    // Release root scope last among value-owned structures so registered dtors run.
    // This is the primary deterministic cleanup point for scope-managed values/envs.
    if (self.root_scope != null) {
        self.current_scope = self.root_scope;
        self.releasing_scope = null;
        self.tco_recycle_scope = null;
        self.tco_scope_defer_slot = 0;
        self.tco_scope_defer_active = false;
        self.active_promotion_ctx = null;
        main::scope_release(self.root_scope);
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
