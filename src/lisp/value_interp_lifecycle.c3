module lisp;

import std::collections::list;
import std::io;
import main;

fn void Interp.grow_macro_table(Interp* self) {
    usz new_cap = self.macro_capacity * 2;
    MacroDef* old_table = self.macro_table;
    usz* old_hash = self.macro_hash_index;
    MacroDef* new_table = (MacroDef*)mem::malloc(MacroDef.sizeof * new_cap);
    assert(new_table != null, "grow_macro_table: macro table alloc failed");
    for (usz i = 0; i < self.macro_count; i++) new_table[i] = old_table[i];

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    assert(new_hash != null, "grow_macro_table: macro hash alloc failed");
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.macro_count; i++) {
        char[] name_str = self.symbols.get_name(new_table[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }

    self.macro_table = new_table;
    self.macro_capacity = new_cap;
    self.macro_hash_index = new_hash;
    self.macro_hash_capacity = new_hash_cap;
    mem::free(old_table);
    mem::free(old_hash);
}

fn void Interp.grow_module_table(Interp* self) {
    usz new_cap = self.module_capacity * 2;
    Module* old_mods = self.modules;
    usz* old_hash = self.module_hash_index;
    Module* new_mods = (Module*)mem::malloc(Module.sizeof * new_cap);
    assert(new_mods != null, "grow_module_table: module table alloc failed");
    for (usz i = 0; i < self.module_count; i++) new_mods[i] = old_mods[i];

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    assert(new_hash != null, "grow_module_table: module hash alloc failed");
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.module_count; i++) {
        char[] name_str = self.symbols.get_name(new_mods[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }

    self.modules = new_mods;
    self.module_capacity = new_cap;
    self.module_hash_index = new_hash;
    self.module_hash_capacity = new_hash_cap;
    mem::free(old_mods);
    mem::free(old_hash);
}

fn void Interp.grow_handler_stack(Interp* self) {
    usz new_cap = self.handler_capacity * 2;
    EffectHandler* old_stack = self.handler_stack;
    EffectHandler* new_stack = (EffectHandler*)mem::malloc(EffectHandler.sizeof * new_cap);
    assert(new_stack != null, "grow_handler_stack: handler stack alloc failed");
    for (usz i = 0; i < self.handler_count; i++) new_stack[i] = old_stack[i];
    self.handler_stack = new_stack;
    self.handler_capacity = new_cap;
    mem::free(old_stack);
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
