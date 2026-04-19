module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_env_copy_continuation_rollback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy rollback releases continuation handle retention when
    // the first copied continuation wrapper is abandoned by a later binding fault.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy continuation rollback");
            (*fail)++;
            return;
        }

        SymbolId cont_sym = interp.symbols.intern("__copy_env_continuation");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_continuation_fail");

        interp.current_scope = source;
        Continuation* k = interp.alloc_lisp_continuation();
        HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        Value* cont_wrapper = interp.alloc_value();
        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;
        Env* frame = make_env(interp, interp.global_env);

        bool setup_ok = k != null &&
                        shared_state != null &&
                        cont_wrapper != null &&
                        frame != null &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;
        if (setup_ok) {
            handle_effect_state_init(shared_state, interp);
            cont_wrapper.tag = CONTINUATION;
            cont_wrapper.cont_val = k;
            k.handle_state = shared_state;
            k.handle_retained = false;
            env_define_with_barrier(frame, cont_sym, cont_wrapper, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        int refcount_after_fail = shared_state != null ? (int)shared_state.continuation_refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = k != null &&
                           !k.handle_retained &&
                           refcount_after_fail == 0;

        if (shared_state != null) {
            shared_state.frame_active = false;
            if (shared_state.continuation_refcount == 0) {
                handle_effect_state_release(shared_state);
            }
            if (k != null) {
                k.handle_state = null;
                k.handle_retained = false;
            }
        }

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy continuation rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy continuation rollback (setup_ok=%s fault=%s retained=%s refcount=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                k != null && k.handle_retained ? "yes" : "no",
                refcount_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_nested_cons_rollback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy rollback unwinds nested copied cons children immediately,
    // not only when the abandoned target scope is later released.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* owner = main::scope_create(saved_scope);
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (owner == null || source == null || target == null) {
            if (owner != null) main::scope_release(owner);
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy nested cons rollback");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_nested_cons_payload");
        SymbolId cons_sym = interp.symbols.intern("__copy_env_nested_cons");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_nested_cons_fail");

        interp.current_scope = owner;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 901), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        interp.current_scope = source;
        Value* cons_wrapper = shared_closure != null ? make_cons(interp, shared_closure, make_nil(interp)) : null;
        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;
        Env* frame = make_env(interp, interp.global_env);

        bool setup_ok = frame != null &&
                        shared_closure != null &&
                        shared_closure.tag == CLOSURE &&
                        shared_closure.closure_val != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        cons_wrapper != null &&
                        cons_wrapper.tag == CONS &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;
        if (setup_ok) {
            env_define_with_barrier(frame, cons_sym, cons_wrapper, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = rc_after_fail == 1 &&
                           closure_env_lookup_int(shared_closure, payload_sym, 901);

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool teardown_ok = rc_after_target_release == 1;
        main::scope_release(owner);

        bool ok = setup_ok && fault_ok && rollback_ok && teardown_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy nested cons rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy nested cons rollback (setup_ok=%s fault=%s rc_after_fail=%d rc_after_target_release=%d rollback_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                rc_after_fail,
                rc_after_target_release,
                rollback_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_nested_array_rollback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy rollback unwinds nested copied shared-wrapper children
    // immediately, not only when the abandoned target scope is later released.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* owner = main::scope_create(saved_scope);
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (owner == null || source == null || target == null) {
            if (owner != null) main::scope_release(owner);
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy nested array rollback");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_nested_array_payload");
        SymbolId array_sym = interp.symbols.intern("__copy_env_nested_array");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_nested_array_fail");

        interp.current_scope = owner;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 902), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
            if (closure_env_scope != null) {
                main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                    int* counter = (int*)ptr;
                    if (counter != null) (*counter)++;
                });
            }
        }

        interp.current_scope = source;
        Value* array_src = interp.alloc_value();
        Array* array_payload = null;
        bool array_ok = array_src != null;
        if (array_ok) {
            array_src.tag = ARRAY;
            main::scope_register_dtor(source, (void*)array_src, &scope_dtor_value);
            array_payload = (Array*)mem::malloc(Array.sizeof);
            array_ok = array_payload != null;
        }
        if (array_ok) {
            array_payload.capacity = 4;
            array_payload.length = 1;
            array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
            array_ok = array_payload.items != null;
        }
        if (array_ok) {
            for (usz i = 0; i < array_payload.capacity; i++) {
                array_payload.items[i] = null;
            }
            array_payload.items[0] = shared_closure;
            array_src.array_val = array_payload;
        } else if (array_payload != null) {
            mem::free(array_payload);
            array_payload = null;
        }

        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;
        Env* frame = make_env(interp, interp.global_env);

        bool setup_ok = frame != null &&
                        array_ok &&
                        shared_closure != null &&
                        shared_closure.tag == CLOSURE &&
                        shared_closure.closure_val != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;
        if (setup_ok) {
            env_define_with_barrier(frame, array_sym, array_src, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = rc_after_fail == 1 &&
                           closure_env_lookup_int(shared_closure, payload_sym, 902);

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        bool teardown_ok = rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0;
        main::scope_release(owner);
        int destroy_count_after_owner_release = closure_env_destroy_count;
        bool owner_release_ok = destroy_count_after_owner_release == 1;

        bool ok = setup_ok && fault_ok && rollback_ok && teardown_ok && owner_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy nested array rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy nested array rollback (setup_ok=%s fault=%s rc_after_fail=%d rc_after_target_release=%d destroy_count_after_target_release=%d destroy_count_after_owner_release=%d rollback_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                rc_after_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_owner_release,
                rollback_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_nested_cons_wrapper_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy clones a target-chain outer CONS when a nested CONS
    // shell still belongs to the source/releasing scope, even if the nested
    // payload leaves are only scalars.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy nested cons wrapper alias");
            (*fail)++;
            return;
        }

        SymbolId cons_sym = interp.symbols.intern("__copy_env_nested_cons_wrapper");

        interp.current_scope = target;
        Value* outer_cons = make_cons(interp, make_int(interp, 31), make_nil(interp));

        interp.current_scope = source;
        Value* nested_cons = make_cons(interp, make_int(interp, 32), make_nil(interp));
        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = outer_cons != null && nested_cons != null && frame != null;
        if (setup_ok) {
            outer_cons.cons_val.cdr = nested_cons;
            setup_ok = frame.define(cons_sym, outer_cons);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        Value* copied_cons = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(cons_sym)
            : null;
        bool copied_ok = boundary_env_copy_result_ok(copied) &&
                         copied.env != null &&
                         copied_cons != null &&
                         copied_cons.tag == CONS &&
                         copied_cons != outer_cons &&
                         copied_cons.cons_val.car != null &&
                         copied_cons.cons_val.car.tag == INT &&
                         copied_cons.cons_val.car.int_val == 31 &&
                         copied_cons.cons_val.cdr != null &&
                         copied_cons.cons_val.cdr.tag == CONS &&
                         copied_cons.cons_val.cdr != nested_cons &&
                         copied_cons.cons_val.cdr.cons_val.car != null &&
                         copied_cons.cons_val.cdr.cons_val.car.tag == INT &&
                         copied_cons.cons_val.cdr.cons_val.car.int_val == 32;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        bool post_release_ok = copied_ok &&
                               copied_cons != null &&
                               copied_cons.cons_val.cdr != null &&
                               copied_cons.cons_val.cdr.tag == CONS &&
                               copied_cons.cons_val.cdr.cons_val.car != null &&
                               copied_cons.cons_val.cdr.cons_val.car.tag == INT &&
                               copied_cons.cons_val.cdr.cons_val.car.int_val == 32;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy nested cons wrapper alias");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy nested cons wrapper alias (setup_ok=%s copied_ok=%s post_release_ok=%s fault=%s)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code)
            );
            (*fail)++;
        }
    }
}
