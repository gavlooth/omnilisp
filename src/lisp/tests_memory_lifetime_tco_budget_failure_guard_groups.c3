module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_tco_env_copy_failure_fail_closed_test(Interp* interp, int* pass, int* fail) {
    // Invariant: direct TCO env-chain copying rejects a boundary-copy failure
    // before committing a broken binding into the copied env frame.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: tco env copy failure fails closed");
            (*fail)++;
            return;
        }

        SymbolId sym = interp.symbols.intern("__tco_env_copy_opaque");

        interp.current_scope = source;
        Env* source_env = make_env(interp, interp.global_env);
        Value* opaque = make_tco_opaque_primitive_for_test(interp, source, 't', 7301);
        bool setup_ok = source_env != null &&
                        opaque != null &&
                        env_define_with_barrier_checked(source_env, sym, opaque, interp);

        interp.current_scope = target;
        Env* copied_env = setup_ok
            ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
            : null;

        Value* original_value = source_env != null ? source_env.lookup(sym) : null;
        bool fail_closed = setup_ok &&
            copied_env == null &&
            original_value == opaque &&
            original_value != null &&
            original_value.tag == PRIMITIVE &&
            original_value.prim_val != null &&
            original_value.prim_val.user_data != null;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (fail_closed) {
            emit_pass_literal("[PASS] lifetime: tco env copy failure fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tco env copy failure fails closed (setup_ok=%s copied_null=%s original_ok=%s)",
                setup_ok ? "yes" : "no",
                copied_env == null ? "yes" : "no",
                (original_value == opaque) ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_recycle_env_copy_failure_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle preserves the previous env and call scope when
    // env-chain copying fails while preparing the fresh recycle scope.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        usz saved_tco_slot = interp.tco_scope_defer_slot;
        bool saved_tco_active = interp.tco_scope_defer_active;
        Env* saved_jit_env = interp.jit_env;

        main::ScopeRegion* cs = main::scope_create(saved_scope);
        if (cs == null) {
            io::printn("[FAIL] lifetime: tco recycle env copy failure restores state");
            (*fail)++;
            return;
        }

        SymbolId sym = interp.symbols.intern("__tco_recycle_opaque");

        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        interp.tco_scope_defer_slot = 0;
        interp.tco_scope_defer_active = false;
        interp.jit_env = interp.global_env;

        Env* env = make_env(interp, interp.global_env);
        Value* opaque = make_tco_opaque_primitive_for_test(interp, cs, 'r', 7302);
        bool setup_ok = env != null &&
                        opaque != null &&
                        env_define_with_barrier_checked(env, sym, opaque, interp);
        Env* env_io = env;

        Value* err = setup_ok
            ? jit_prepare_tco_recycle(&env_io, interp.global_env, interp)
            : make_nil(interp);

        bool err_ok = err != null &&
                      err.tag == ERROR &&
                      err.str_chars != null &&
                      str_contains(
                          err.str_chars[:err.str_len],
                          "jit: failed to copy TCO recycle env"
                      );
        bool env_ok = env_io == env &&
                      env != null &&
                      env.lookup(sym) == opaque;
        bool state_ok = interp.current_scope == cs &&
                        interp.tco_recycle_scope == cs &&
                        interp.jit_env == interp.global_env;

        interp.current_scope = saved_scope;
        interp.tco_recycle_scope = saved_tco_recycle;
        interp.tco_scope_defer_slot = saved_tco_slot;
        interp.tco_scope_defer_active = saved_tco_active;
        interp.jit_env = saved_jit_env;
        main::scope_release(cs);

        bool ok = setup_ok && err_ok && env_ok && state_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco recycle env copy failure restores state");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tco recycle env copy failure restores state (setup_ok=%s err_ok=%s env_ok=%s state_ok=%s)",
                setup_ok ? "yes" : "no",
                err_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                state_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_nested_temp_graph_recycle_guard_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle must not fast-reset TEMP when an outer binding
    // wrapper still reaches a nested TEMP-owned child transitively.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        Env* saved_jit_env = interp.jit_env;

        main::ScopeRegion* owner = main::scope_create(saved_scope);
        if (owner == null) {
            io::printn("[FAIL] lifetime: tco nested temp graph blocks fast reset");
            (*fail)++;
            return;
        }

        main::ScopeRegion* recycle_scope = main::scope_create(owner);
        if (recycle_scope == null) {
            main::scope_release(owner);
            io::printn("[FAIL] lifetime: tco nested temp graph blocks fast reset");
            (*fail)++;
            return;
        }

        SymbolId array_sym = interp.symbols.intern("__tco_nested_temp_array");

        interp.current_scope = owner;
        Env* env = make_env(interp, interp.global_env);
        Value* array_value = interp.alloc_value();
        Array* array_payload = null;
        bool setup_ok = env != null && array_value != null;
        if (setup_ok) {
            array_value.tag = ARRAY;
            main::scope_register_dtor(owner, (void*)array_value, &scope_dtor_value);
            array_payload = (Array*)mem::malloc(Array.sizeof);
            setup_ok = array_payload != null;
        }
        if (setup_ok) {
            array_payload.capacity = 4;
            array_payload.length = 0;
            array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
            setup_ok = array_payload.items != null;
        }
        if (setup_ok) {
            for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
            array_value.array_val = array_payload;
        } else if (array_payload != null) {
            mem::free(array_payload);
            array_payload = null;
        }

        interp.current_scope = recycle_scope;
        Value* nested_cons = setup_ok ? make_cons(interp, make_int(interp, 77), make_nil(interp)) : null;
        if (setup_ok) {
            setup_ok = nested_cons != null && nested_cons.tag == CONS;
        }
        if (setup_ok) {
            array_payload.items[0] = nested_cons;
            array_payload.length = 1;
            env_define_with_barrier(env, array_sym, array_value, interp);
        }

        interp.current_scope = recycle_scope;
        interp.tco_recycle_scope = recycle_scope;
        interp.jit_env = interp.global_env;
        Env* env_io = env;

        bool shallow_frame_ok = !env_chain_has_frame_in_temp_scope(env_io, recycle_scope, interp.global_env);
        bool shallow_binding_ok = !env_chain_has_binding_in_temp_scope(env_io, recycle_scope, interp.global_env);
        bool nested_detected = env_chain_has_graph_binding_values_in_temp_scope(env_io, recycle_scope, interp.global_env);

        Value* err = setup_ok ? jit_prepare_tco_recycle(&env_io, interp.global_env, interp) : make_nil(interp);
        main::ScopeRegion* post_scope = interp.current_scope;
        Value* copied_array = env_io != null ? env_io.lookup(array_sym) : null;
        Value* copied_nested = copied_array != null &&
            copied_array.tag == ARRAY &&
            copied_array.array_val != null &&
            copied_array.array_val.length == 1
            ? copied_array.array_val.items[0]
            : null;

        bool copied_ok = err == null &&
            env_io != null &&
            env_io != env &&
            post_scope != null &&
            post_scope != recycle_scope &&
            interp.tco_recycle_scope == post_scope &&
            copied_array != null &&
            copied_array.tag == ARRAY &&
            copied_array != array_value &&
            copied_array.array_val != null &&
            copied_array.array_val != array_payload &&
            copied_nested != null &&
            copied_nested.tag == CONS &&
            copied_nested != nested_cons &&
            copied_nested.cons_val.car != null &&
            copied_nested.cons_val.car.tag == INT &&
            copied_nested.cons_val.car.int_val == 77;

        interp.current_scope = saved_scope;
        interp.tco_recycle_scope = saved_tco_recycle;
        interp.jit_env = saved_jit_env;

        if (post_scope != null && post_scope != recycle_scope && post_scope != owner && post_scope != saved_scope) {
            main::scope_release(post_scope);
        }
        if (post_scope == recycle_scope) {
            main::scope_release(recycle_scope);
        }
        main::scope_release(owner);

        bool ok = setup_ok &&
            shallow_frame_ok &&
            shallow_binding_ok &&
            nested_detected &&
            copied_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco nested temp graph blocks fast reset");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tco nested temp graph blocks fast reset (setup_ok=%s frame_ok=%s binding_ok=%s nested_detected=%s env_rewritten=%s copied_ok=%s err=%s)",
                setup_ok ? "yes" : "no",
                shallow_frame_ok ? "yes" : "no",
                shallow_binding_ok ? "yes" : "no",
                nested_detected ? "yes" : "no",
                (env_io != null && env_io != env) ? "yes" : "no",
                copied_ok ? "yes" : "no",
                err != null && err.tag == ERROR ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_closure_payload_temp_recycle_guard_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle must not fast-reset TEMP when a surviving closure
    // wrapper still points at a TEMP-owned closure payload.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        Env* saved_jit_env = interp.jit_env;

        main::ScopeRegion* owner = main::scope_create(saved_scope);
        if (owner == null) {
            io::printn("[FAIL] lifetime: tco closure payload temp blocks fast reset");
            (*fail)++;
            return;
        }

        main::ScopeRegion* recycle_scope = main::scope_create(owner);
        if (recycle_scope == null) {
            main::scope_release(owner);
            io::printn("[FAIL] lifetime: tco closure payload temp blocks fast reset");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__tco_temp_payload_closure");

        interp.current_scope = owner;
        Env* env = make_env(interp, interp.global_env);
        Value* closure_value = interp.alloc_value();
        bool setup_ok = env != null && closure_value != null;

        interp.current_scope = recycle_scope;
        Closure* payload = setup_ok ? (Closure*)recycle_scope.alloc(Closure.sizeof) : null;
        if (setup_ok) {
            setup_ok = payload != null;
        }
        if (setup_ok) {
            payload.scope_gen = recycle_scope.generation;
            payload.env_scope = null;
            payload.param = (SymbolId)0;
            payload.params = null;
            payload.param_count = 0;
            payload.has_rest = false;
            payload.rest_param = (SymbolId)0;
            payload.body = null;
            payload.env = interp.global_env;
            payload.has_param = false;
            payload.has_typed_params = false;
            payload.type_sig = null;
            payload.name = (SymbolId)0;
        }

        interp.current_scope = owner;
        if (setup_ok) {
            closure_value.tag = CLOSURE;
            closure_value.closure_val = payload;
            env_define_with_barrier(env, closure_sym, closure_value, interp);
        }

        interp.current_scope = recycle_scope;
        interp.tco_recycle_scope = recycle_scope;
        interp.jit_env = interp.global_env;
        Env* env_io = env;

        bool shallow_frame_ok = !env_chain_has_frame_in_temp_scope(env_io, recycle_scope, interp.global_env);
        bool shallow_binding_ok = !env_chain_has_binding_in_temp_scope(env_io, recycle_scope, interp.global_env);
        bool nested_detected = env_chain_has_graph_binding_values_in_temp_scope(env_io, recycle_scope, interp.global_env);

        Value* err = setup_ok ? jit_prepare_tco_recycle(&env_io, interp.global_env, interp) : make_nil(interp);
        main::ScopeRegion* post_scope = interp.current_scope;
        Value* copied_closure = env_io != null ? env_io.lookup(closure_sym) : null;

        bool copied_ok = err == null &&
            env_io != null &&
            env_io != env &&
            post_scope != null &&
            post_scope != recycle_scope &&
            interp.tco_recycle_scope == post_scope &&
            copied_closure != null &&
            copied_closure.tag == CLOSURE &&
            copied_closure != closure_value &&
            copied_closure.closure_val != null &&
            copied_closure.closure_val != payload &&
            copied_closure.closure_val.scope_gen == post_scope.generation &&
            copied_closure.closure_val.env == interp.global_env;

        interp.current_scope = saved_scope;
        interp.tco_recycle_scope = saved_tco_recycle;
        interp.jit_env = saved_jit_env;

        if (post_scope != null && post_scope != recycle_scope && post_scope != owner && post_scope != saved_scope) {
            main::scope_release(post_scope);
        }
        if (post_scope == recycle_scope) {
            main::scope_release(recycle_scope);
        }
        main::scope_release(owner);

        bool ok = setup_ok &&
            shallow_frame_ok &&
            shallow_binding_ok &&
            nested_detected &&
            copied_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco closure payload temp blocks fast reset");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tco closure payload temp blocks fast reset (setup_ok=%s frame_ok=%s binding_ok=%s nested_detected=%s env_rewritten=%s copied_ok=%s err=%s)",
                setup_ok ? "yes" : "no",
                shallow_frame_ok ? "yes" : "no",
                shallow_binding_ok ? "yes" : "no",
                nested_detected ? "yes" : "no",
                (env_io != null && env_io != env) ? "yes" : "no",
                copied_ok ? "yes" : "no",
                err != null && err.tag == ERROR ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_hot_budget_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_jit_copy_hot_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_frame_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_list_accumulator_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_string_argument_list_promotion_test(interp, pass, fail);
    run_memory_lifetime_tco_boundary_state_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_runtime_fields_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_recycle_error_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_env_copy_failure_fail_closed_test(interp, pass, fail);
    run_memory_lifetime_tco_recycle_env_copy_failure_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_nested_temp_graph_recycle_guard_test(interp, pass, fail);
    run_memory_lifetime_tco_closure_payload_temp_recycle_guard_test(interp, pass, fail);
}
