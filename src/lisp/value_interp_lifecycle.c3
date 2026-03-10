module lisp;

import std::collections::list;
import std::io;
import main;

fn void Interp.grow_macro_table(Interp* self) {
    usz new_cap = self.macro_capacity * 2;
    MacroDef* new_table = (MacroDef*)mem::malloc(MacroDef.sizeof * new_cap);
    for (usz i = 0; i < self.macro_count; i++) new_table[i] = self.macro_table[i];
    mem::free(self.macro_table);
    self.macro_table = new_table;
    self.macro_capacity = new_cap;

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;
    mem::free(self.macro_hash_index);
    self.macro_hash_index = new_hash;
    self.macro_hash_capacity = new_hash_cap;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.macro_count; i++) {
        char[] name_str = self.symbols.get_name(self.macro_table[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }
}

fn void Interp.grow_module_table(Interp* self) {
    usz new_cap = self.module_capacity * 2;
    Module* new_mods = (Module*)mem::malloc(Module.sizeof * new_cap);
    for (usz i = 0; i < self.module_count; i++) new_mods[i] = self.modules[i];
    mem::free(self.modules);
    self.modules = new_mods;
    self.module_capacity = new_cap;

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;
    mem::free(self.module_hash_index);
    self.module_hash_index = new_hash;
    self.module_hash_capacity = new_hash_cap;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.module_count; i++) {
        char[] name_str = self.symbols.get_name(self.modules[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }
}

fn void Interp.grow_handler_stack(Interp* self) {
    usz new_cap = self.handler_capacity * 2;
    EffectHandler* new_stack = (EffectHandler*)mem::malloc(EffectHandler.sizeof * new_cap);
    for (usz i = 0; i < self.handler_count; i++) new_stack[i] = self.handler_stack[i];
    mem::free(self.handler_stack);
    self.handler_stack = new_stack;
    self.handler_capacity = new_cap;
}

fn void Interp.destroy(Interp* self) {
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
    if (self.module_hash_index != null) { mem::free(self.module_hash_index); self.module_hash_index = null; }
    if (self.handler_stack != null) { mem::free(self.handler_stack); self.handler_stack = null; }
    main::stack_pool_shutdown(&self.stack_ctx_pool);
    if (g_stack_ctx_pool == &self.stack_ctx_pool) g_stack_ctx_pool = null;
    jit_global_shutdown();
}
