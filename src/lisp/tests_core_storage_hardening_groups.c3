module lisp;

import std::io;

fn void run_basic_interp_storage_hardening_tests(Interp* interp, int* pass, int* fail) {
    if (interp_fault_injection_enabled()) {
        {
            Interp local = {};
            interp_force_macro_table_alloc_fail_for_test(true);
            local.init();
            interp_force_macro_table_alloc_fail_for_test(false);

            bool ok = local.macro_table == null
                && local.macro_hash_index == null
                && local.macro_capacity == 0
                && local.macro_hash_capacity == 0
                && local.ensure_macro_capacity(1)
                && local.macro_table != null
                && local.macro_hash_index != null;
            if (ok) {
                emit_pass_literal("[PASS] interp macro init allocation failure fails closed");
                (*pass)++;
            } else {
                io::printn("[FAIL] interp macro init allocation failure fails closed");
                (*fail)++;
            }
            local.destroy();
        }

        {
            Interp local = {};
            interp_force_module_table_alloc_fail_for_test(true);
            local.init();
            interp_force_module_table_alloc_fail_for_test(false);

            bool ok = local.modules == null
                && local.module_hash_index == null
                && local.module_capacity == 0
                && local.module_hash_capacity == 0
                && local.ensure_module_capacity(1)
                && local.modules != null
                && local.module_hash_index != null;
            if (ok) {
                emit_pass_literal("[PASS] interp module init allocation failure fails closed");
                (*pass)++;
            } else {
                io::printn("[FAIL] interp module init allocation failure fails closed");
                (*fail)++;
            }
            local.destroy();
        }

        {
            Interp local = {};
            interp_force_handler_stack_alloc_fail_for_test(true);
            local.init();
            interp_force_handler_stack_alloc_fail_for_test(false);

            bool ok = local.handler_stack == null
                && local.handler_capacity == 0
                && local.ensure_handler_capacity(1)
                && local.handler_stack != null
                && local.handler_capacity >= 1;
            if (ok) {
                emit_pass_literal("[PASS] interp handler stack init allocation failure fails closed");
                (*pass)++;
            } else {
                io::printn("[FAIL] interp handler stack init allocation failure fails closed");
                (*fail)++;
            }
            local.destroy();
        }
    } else {
        emit_pass_literal("[PASS] interp init fault-injection hardening skipped (enable OMNI_TEST_FAULT_INJECTION)");
        (*pass)++;
    }

    {
        Interp local = {};
        local.init();
        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;
        bool failed = !local.ensure_macro_capacity(usz.max);
        bool preserved = local.macro_table == before_table
            && local.macro_hash_index == before_hash
            && local.macro_capacity == before_cap
            && local.macro_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp macro capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    if (!interp_fault_injection_enabled()) {
        emit_pass_literal("[PASS] interp grow fault-injection hardening skipped (enable OMNI_TEST_FAULT_INJECTION)");
        (*pass)++;
        return;
    }

    {
        Interp local = {};
        local.init();
        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;
        usz max_macro_needed = usz.max / MacroDef.sizeof + 1;
        bool failed = !local.ensure_macro_capacity(max_macro_needed);
        bool preserved = local.macro_table == before_table
            && local.macro_hash_index == before_hash
            && local.macro_capacity == before_cap
            && local.macro_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp macro item-bytes overflow guards growth");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro item-bytes overflow guards growth");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        Module* before_modules = local.modules;
        usz* before_hash = local.module_hash_index;
        usz before_cap = local.module_capacity;
        usz before_hash_cap = local.module_hash_capacity;
        bool failed = !local.ensure_module_capacity(usz.max);
        bool preserved = local.modules == before_modules
            && local.module_hash_index == before_hash
            && local.module_capacity == before_cap
            && local.module_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp module capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp module capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        EffectHandler* before_stack = local.handler_stack;
        usz before_cap = local.handler_capacity;
        bool failed = !local.ensure_handler_capacity(usz.max);
        bool preserved = local.handler_stack == before_stack
            && local.handler_capacity == before_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp handler capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp handler capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        SymbolId name = local.symbols.intern("macro-grow-check");
        local.macro_table[0] = (MacroDef){
            .name = name,
            .clauses = null,
            .clause_count = 0,
            .captured_bindings = null,
            .captured_count = 0,
        };
        local.macro_count = 1;
        usz slot = (usz)name % local.macro_hash_capacity;
        while (local.macro_hash_index[slot] != usz.max) {
            slot = (slot + 1) % local.macro_hash_capacity;
        }
        local.macro_hash_index[slot] = 0;

        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;

        interp_force_grow_macro_table_alloc_fail_for_test(true);
        bool failed = !local.ensure_macro_capacity(local.macro_capacity + 1);
        interp_force_grow_macro_table_alloc_fail_for_test(false);
        bool preserved = local.macro_count == 1
            && before_table == local.macro_table
            && before_hash == local.macro_hash_index
            && before_cap == local.macro_capacity
            && before_hash_cap == local.macro_hash_capacity;
        bool recovered = local.ensure_macro_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && lookup_macro_index(name, &local) == 0;
        if (ok) {
            emit_pass_literal("[PASS] interp macro grow allocation failure preserves lookup");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro grow allocation failure preserves lookup");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        SymbolId name = local.symbols.intern("module-grow-check");
        local.modules[0] = (Module){
            .name = name,
            .env = null,
            .exports = null,
            .export_count = 0,
            .export_capacity = 0,
            .loaded = true,
            .path = null,
            .path_len = 0,
        };
        local.module_count = 1;
        usz slot = (usz)name % local.module_hash_capacity;
        while (local.module_hash_index[slot] != usz.max) {
            slot = (slot + 1) % local.module_hash_capacity;
        }
        local.module_hash_index[slot] = 0;

        Module* before_modules = local.modules;
        usz* before_hash = local.module_hash_index;
        usz before_cap = local.module_capacity;
        usz before_hash_cap = local.module_hash_capacity;

        interp_force_grow_module_table_alloc_fail_for_test(true);
        bool failed = !local.ensure_module_capacity(local.module_capacity + 1);
        interp_force_grow_module_table_alloc_fail_for_test(false);
        bool preserved = local.module_count == 1
            && before_modules == local.modules
            && before_hash == local.module_hash_index
            && before_cap == local.module_capacity
            && before_hash_cap == local.module_hash_capacity;
        bool recovered = local.ensure_module_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && find_module(name, &local) == &local.modules[0];
        if (ok) {
            emit_pass_literal("[PASS] interp module grow allocation failure preserves lookup");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp module grow allocation failure preserves lookup");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        local.handler_stack[0] = (EffectHandler){
            .tags = null,
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = true,
            .effect_state = null,
        };
        local.handler_count = 1;

        EffectHandler* before_stack = local.handler_stack;
        usz before_cap = local.handler_capacity;

        interp_force_grow_handler_stack_alloc_fail_for_test(true);
        bool failed = !local.ensure_handler_capacity(local.handler_capacity + 1);
        interp_force_grow_handler_stack_alloc_fail_for_test(false);
        bool preserved = before_stack == local.handler_stack
            && before_cap == local.handler_capacity
            && local.handler_count == 1;
        bool recovered = local.ensure_handler_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && local.handler_stack[0].clause_count == 1
            && local.handler_stack[0].strict_mode;
        if (ok) {
            emit_pass_literal("[PASS] interp handler stack grow allocation failure preserves handlers");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp handler stack grow allocation failure preserves handlers");
            (*fail)++;
        }
        local.destroy();
    }
}
