module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_continuation_boundary_partial_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary partial cleanup releases continuation handle retention
    // when a later opaque child aborts a copied shared wrapper.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (foreign == null || target == null) {
            if (target != null) main::scope_release(target);
            if (foreign != null) main::scope_release(foreign);
            io::printn("[FAIL] lifetime: continuation boundary partial cleanup");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Continuation* k = interp.alloc_lisp_continuation();
        HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        Value* cont_wrapper = interp.alloc_value();
        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        Value* src_array = interp.alloc_value();
        Array* src_array_payload = (Array*)mem::malloc(Array.sizeof);
        Value* copied_value = null;
        bool setup_ok = k != null &&
                        shared_state != null &&
                        cont_wrapper != null &&
                        opaque != null &&
                        opaque_prim != null &&
                        opaque_payload != null &&
                        src_array != null &&
                        src_array_payload != null;

        if (setup_ok) {
            handle_effect_state_init(shared_state, interp);
            cont_wrapper.tag = CONTINUATION;
            cont_wrapper.cont_val = k;
            k.handle_state = shared_state;
            k.handle_retained = false;

            *opaque_prim = {};
            *opaque_payload = 913;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'x';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 1;
            opaque_prim.tag = 913;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;

            src_array.tag = ARRAY;
            main::scope_register_dtor(foreign, (void*)src_array, &scope_dtor_value);
            src_array_payload.capacity = 4;
            src_array_payload.length = 2;
            src_array_payload.items = (Value**)mem::malloc(Value*.sizeof * src_array_payload.capacity);
            setup_ok = src_array_payload.items != null;
            if (setup_ok) {
                for (usz i = 0; i < src_array_payload.capacity; i++) {
                    src_array_payload.items[i] = null;
                }
                src_array_payload.items[0] = cont_wrapper;
                src_array_payload.items[1] = opaque;
                src_array.array_val = src_array_payload;
            }
        }

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyResult copied =
            setup_ok ? boundary_copy_to_parent_site_ctx_checked(src_array, interp, COPY_SITE_GENERIC, null)
                     : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        copied_value = copied.value;
        int refcount_after_fail = shared_state != null ? (int)shared_state.continuation_refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;

        bool fault_ok = !boundary_copy_result_ok(copied) &&
                        copied_value == null &&
                        copied.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
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

        main::scope_release(target);
        main::scope_release(foreign);

        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: continuation boundary partial cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: continuation boundary partial cleanup (setup_ok=%s fault=%s retained=%s refcount=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copied.fault_code),
                k != null && k.handle_retained ? "yes" : "no",
                refcount_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_continuation_boundary_teardown_test(Interp* interp, int* pass, int* fail) {
    // Invariant: a successful boundary-owned continuation wrapper releases its
    // retained handle-state ref on normal target-scope teardown.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (foreign == null || target == null) {
            if (target != null) main::scope_release(target);
            if (foreign != null) main::scope_release(foreign);
            io::printn("[FAIL] lifetime: continuation boundary teardown");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Continuation* k = interp.alloc_lisp_continuation();
        HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        Value* cont_wrapper = interp.alloc_value();
        bool setup_ok = k != null && shared_state != null && cont_wrapper != null;
        if (setup_ok) {
            handle_effect_state_init(shared_state, interp);
            cont_wrapper.tag = CONTINUATION;
            cont_wrapper.cont_val = k;
            k.handle_state = shared_state;
            k.handle_retained = false;
        }

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyResult copied =
            setup_ok ? boundary_copy_to_parent_site_ctx_checked(cont_wrapper, interp, COPY_SITE_GENERIC, null)
                     : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        Value* copied_value = copied.value;
        bool copied_ok = boundary_copy_result_ok(copied) &&
                         copied_value != null &&
                         copied_value.tag == CONTINUATION &&
                         copied_value != cont_wrapper &&
                         copied_value.continuation_boundary_owned &&
                         k.handle_retained &&
                         shared_state.continuation_refcount == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);

        int refcount_after_target_release = shared_state != null ? (int)shared_state.continuation_refcount : -1;
        bool teardown_ok = k != null &&
                           !k.handle_retained &&
                           refcount_after_target_release == 0;

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

        main::scope_release(foreign);

        bool ok = setup_ok && copied_ok && teardown_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: continuation boundary teardown");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: continuation boundary teardown (setup_ok=%s copied_ok=%s retained=%s refcount=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                k != null && k.handle_retained ? "yes" : "no",
                refcount_after_target_release
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_continuation_boundary_alloc_failure_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: continuation boundary copy/promotion must release retained
    // handle-state refs immediately when wrapper allocation fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (foreign == null || target == null) {
            if (target != null) main::scope_release(target);
            if (foreign != null) main::scope_release(foreign);
            io::printn("[FAIL] lifetime: continuation boundary copy alloc failure rollback");
            (*fail)++;
        } else {
            interp.current_scope = foreign;
            Continuation* k = interp.alloc_lisp_continuation();
            HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
            Value* cont_wrapper = interp.alloc_value();
            bool setup_ok = k != null && shared_state != null && cont_wrapper != null;
            if (setup_ok) {
                handle_effect_state_init(shared_state, interp);
                cont_wrapper.tag = CONTINUATION;
                cont_wrapper.cont_val = k;
                k.handle_state = shared_state;
                k.handle_retained = false;
            }

            interp.current_scope = target;
            interp.releasing_scope = foreign;
            bool saved_force = g_boundary_force_copy_continuation_wrapper_alloc_fail;
            g_boundary_force_copy_continuation_wrapper_alloc_fail = true;
            BoundaryCopyResult copied =
                setup_ok ? boundary_copy_to_parent_site_ctx_checked(cont_wrapper, interp, COPY_SITE_GENERIC, null)
                         : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
            g_boundary_force_copy_continuation_wrapper_alloc_fail = saved_force;
            interp.releasing_scope = null;
            interp.current_scope = saved;

            int refcount_after_fail = shared_state != null ? (int)shared_state.continuation_refcount : -1;
            bool fault_ok = boundary_copy_result_ok(copied) &&
                            copied.value != null &&
                            copied.value.tag == ERROR &&
                            copied.value.str_chars != null &&
                            str_starts_with(copied.value.str_chars[:copied.value.str_len], "boundary: failed to allocate continuation copy value");
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

            main::scope_release(target);
            main::scope_release(foreign);

            bool ok = setup_ok && fault_ok && rollback_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: continuation boundary copy alloc failure rollback");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: continuation boundary copy alloc failure rollback (setup_ok=%s fault=%s retained=%s refcount=%d)",
                    setup_ok ? "yes" : "no",
                    copied.value != null && copied.value.tag == ERROR ? copied.value.str_chars : (ZString)boundary_copy_fault_name(copied.fault_code),
                    k != null && k.handle_retained ? "yes" : "no",
                    refcount_after_fail
                );
                (*fail)++;
            }
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        if (target == null) {
            io::printn("[FAIL] lifetime: continuation escape alloc failure rollback");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        Continuation* k = interp.alloc_lisp_continuation();
        HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        Value* cont_wrapper = interp.alloc_value();
        bool setup_ok = k != null && shared_state != null && cont_wrapper != null;
        if (setup_ok) {
            handle_effect_state_init(shared_state, interp);
            cont_wrapper.tag = CONTINUATION;
            cont_wrapper.cont_val = k;
            k.handle_state = shared_state;
            k.handle_retained = false;
        }

        PromotionContext ctx = {};
        boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
        bool saved_force = g_boundary_force_escape_continuation_wrapper_alloc_fail;
        g_boundary_force_escape_continuation_wrapper_alloc_fail = true;
        Value* promoted = setup_ok ? promote_to_escape(cont_wrapper, interp) : null;
        g_boundary_force_escape_continuation_wrapper_alloc_fail = saved_force;
        boundary_promotion_context_end(interp, &ctx);

        int refcount_after_fail = shared_state != null ? (int)shared_state.continuation_refcount : -1;
        bool fault_ok = promoted != null &&
                        promoted.tag == ERROR &&
                        promoted.str_chars != null &&
                        str_starts_with(promoted.str_chars[:promoted.str_len], "boundary: failed to allocate continuation escape copy");
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

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: continuation escape alloc failure rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: continuation escape alloc failure rollback (setup_ok=%s fault_ok=%s retained=%s refcount=%d)",
                setup_ok ? "yes" : "no",
                fault_ok ? "yes" : "no",
                k != null && k.handle_retained ? "yes" : "no",
                refcount_after_fail
            );
            (*fail)++;
        }
    }
}
