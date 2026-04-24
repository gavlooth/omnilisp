module lisp;


import std::io;
import main;

fn void run_memory_lifetime_promotion_context_memo_test(Interp* interp, int* pass, int* fail) {
    // Invariant: promotion context memoizes fallback copy within one boundary epoch.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 7), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 8);
        Value* a = test_copy_from_releasing_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        Value* b = test_copy_from_releasing_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        promotion_context_end(interp, &ctx);

        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        bool ok = a != null &&
                  b == a &&
                  delta_in_range(site_delta, 1, 4) &&
                  ctx.epoch != 0 &&
                  !ctx.aborted;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion context memo gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion context memo gate (site_delta=%d epoch=%d aborted=%s same=%s)",
                (long)site_delta,
                (int)ctx.epoch,
                ctx.aborted ? "yes" : "no",
                (a != null && b == a) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_scope_chain_cache_test(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated target-chain scans for the same ptr/context are cached after first miss.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: scope-chain cache (scope setup)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 101), make_nil(interp));

        interp.current_scope = parent;
        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, usz.max);
        boundary_reset_decision_stats();

        bool all_false = true;
        for (usz i = 0; i < 256; i++) {
            if (boundary_ptr_in_target_scope_chain_with_hint((void*)src, interp, src.scope_gen)) {
                all_false = false;
                break;
            }
        }

        BoundaryDecisionStats stats = boundary_decision_stats_snapshot();
        promotion_context_end(interp, &ctx);

        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        bool telemetry_on = boundary_telemetry_enabled();
        bool cache_stats_ok = !telemetry_on ||
                              (stats.scope_chain_scan_fallback == 1 &&
                               stats.scope_chain_scan_total >= 1);
        bool ok = all_false &&
                  interp.active_promotion_ctx == null &&
                  cache_stats_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: scope-chain cache");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: scope-chain cache (scope_chain_scan_total=%d scope_chain_scan_fallback=%d telemetry_on=%s all_false=%s active_ctx=%s)",
                (long)stats.scope_chain_scan_total,
                (long)stats.scope_chain_scan_fallback,
                telemetry_on ? "yes" : "no",
                all_false ? "yes" : "no",
                interp.active_promotion_ctx == null ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_abort_fallback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: budget abort triggers deterministic fallback in same epoch context.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 11), make_nil(interp));

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 0);
        Value* promoted = promote_to_escape(src, interp);

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        Value* fallback = test_copy_from_releasing_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        interp.releasing_scope = saved_releasing;
        promotion_context_end(interp, &ctx);

        bool ok = promoted == src &&
                  ctx.aborted &&
                  ctx.epoch != 0 &&
                  fallback != null &&
                  fallback.tag == CONS &&
                  delta_in_range(site_delta, 1, 4);

        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion abort fallback gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion abort fallback gate (aborted=%s epoch=%d promoted_same=%s site_delta=%d fallback_tag=%d)",
                ctx.aborted ? "yes" : "no",
                (int)ctx.epoch,
                (promoted == src) ? "yes" : "no",
                (long)site_delta,
                fallback != null ? (int)fallback.tag : -1);
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_abort_nested_releasing_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested boundary_copy_from_releasing_scope interleavings under
    // aborted promotion context restore boundary state and avoid memo aliasing.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);
        main::ScopeRegion* sibling = main::scope_create(parent);
        if (parent == null || child == null || sibling == null) {
            if (sibling != null) main::scope_release(sibling);
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: promotion abort nested releasing copy");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 21), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 0);

        Value* first = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        main::ScopeRegion* enter_saved = boundary_enter_scope(interp, sibling);
        Value* second = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        boundary_leave_scope(interp, enter_saved);

        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        bool state_ok = interp.current_scope == parent &&
                        interp.releasing_scope == saved_releasing;
        bool copies_ok = first != null &&
                         second != null &&
                         first != second &&
                         first.tag == CONS &&
                         second.tag == CONS &&
                         first.cons_val.car != null &&
                         second.cons_val.car != null &&
                         first.cons_val.car.tag == INT &&
                         second.cons_val.car.tag == INT &&
                         first.cons_val.car.int_val == 21 &&
                         second.cons_val.car.int_val == 21 &&
                         !main::is_in_scope((void*)first, child) &&
                         !main::is_in_scope((void*)second, child) &&
                         main::is_in_scope((void*)first, parent) &&
                         main::is_in_scope((void*)second, sibling);
        bool budget_ok = ctx.aborted && ctx.epoch != 0 && site_delta >= 2;
        promotion_context_end(interp, &ctx);

        bool post_ctx_ok = interp.active_promotion_ctx == null;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(sibling);
        main::scope_release(child);
        main::scope_release(parent);

        bool ok = state_ok && copies_ok && budget_ok && post_ctx_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion abort nested releasing copy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion abort nested releasing copy (state_ok=%s copies_ok=%s budget_ok=%s post_ctx_ok=%s site_delta=%d aborted=%s)",
                state_ok ? "yes" : "no",
                copies_ok ? "yes" : "no",
                budget_ok ? "yes" : "no",
                post_ctx_ok ? "yes" : "no",
                (long)site_delta,
                ctx.aborted ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_escape_promotion_fail_closed_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested CONS/PARTIAL_PRIM/ITERATOR escape promotion fails closed
    // when it encounters an opaque primitive payload transitively.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested escape promotion fail-closed");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: nested escape promotion fail-closed (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 801;
        opaque_prim.name[0] = 'e';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 2;
        opaque_prim.tag = 801;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* partial = interp.alloc_value();
        partial.tag = PARTIAL_PRIM;
        partial.partial_val.func = &prim_add;
        partial.partial_val.remaining = 1;
        partial.partial_val.first_arg = opaque;
        partial.partial_val.second_arg = make_int(interp, 9);

        Value* iter = make_iterator(interp, partial);
        Value* cons = make_cons(interp, iter, make_nil(interp));

        interp.current_scope = target;
        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 32);

        Value* cons_promoted = promote_to_escape(cons, interp);
        Value* partial_promoted = promote_to_escape(partial, interp);
        Value* iter_promoted = promote_to_escape(iter, interp);

        promotion_context_end(interp, &ctx);

        bool cons_ok = cons_promoted != null &&
                       cons_promoted.tag == ERROR;
        bool partial_ok = partial_promoted != null &&
                          partial_promoted.tag == ERROR;
        bool iter_ok = iter_promoted != null &&
                       iter_promoted.tag == ERROR;

        main::scope_release(foreign);

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = cons_ok && partial_ok && iter_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested escape promotion fail-closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: nested escape promotion fail-closed (cons_ok=%s partial_ok=%s iter_ok=%s)",
                cons_ok ? "yes" : "no",
                partial_ok ? "yes" : "no",
                iter_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_promotion_context_stack_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested promotion contexts preserve stack discipline and memo behavior.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: nested promotion context stack");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 55), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;

        PromotionContext outer = {};
        promotion_context_begin(interp, &outer, 0); // force aborted fallback in outer
        Value* outer_a = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);

        PromotionContext inner = {};
        promotion_context_begin(interp, &inner, 4);
        bool inner_prev_ok = inner.prev == &outer && interp.active_promotion_ctx == &inner;
        Value* inner_a = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        Value* inner_b = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        bool inner_memo_ok = inner_a != null && inner_b == inner_a && !inner.aborted;
        promotion_context_end(interp, &inner);
        bool outer_restored_ok = interp.active_promotion_ctx == &outer;

        Value* outer_b = test_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        promotion_context_end(interp, &outer);

        bool outer_abort_ok = outer.aborted &&
                              outer_a != null &&
                              outer_b != null &&
                              outer_a != outer_b;
        bool final_ctx_ok = interp.active_promotion_ctx == null;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        bool ok = inner_prev_ok && inner_memo_ok && outer_restored_ok &&
                  outer_abort_ok && final_ctx_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested promotion context stack");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: nested promotion context stack (inner_prev_ok=%s inner_memo_ok=%s outer_restored_ok=%s outer_abort_ok=%s final_ctx_ok=%s)",
                inner_prev_ok ? "yes" : "no",
                inner_memo_ok ? "yes" : "no",
                outer_restored_ok ? "yes" : "no",
                outer_abort_ok ? "yes" : "no",
                final_ctx_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_abort_copy_fallback_stress_test(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated zero-budget promote+copy fallback loops preserve state and deterministic abort behavior.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: promotion abort/copy fallback stress");
            (*fail)++;
            return;
        }

        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        bool ok = true;
        usz failed_iter = usz.max;
        usz failed_site_delta = 0;
        bool failed_aborted = false;
        bool failed_fallback_non_null = false;
        bool failed_state_ok = false;
        bool failed_ctx_clear = false;
        bool failed_promoted_same = false;
        for (usz i = 0; i < 192 && ok; i++) {
            interp.current_scope = child;
            Value* src = make_cons(interp, make_int(interp, (long)(700 + i)), make_nil(interp));

            interp.current_scope = parent;
            interp.releasing_scope = child;
            usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

            PromotionContext ctx = {};
            promotion_context_begin(interp, &ctx, 0);
            Value* promoted = promote_to_escape(src, interp);
            Value* fallback = test_copy_from_releasing_scope(
                src, interp, child, COPY_SITE_RUN_JIT_RESULT);
            promotion_context_end(interp, &ctx);

            usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
            bool fallback_non_null = fallback != null;
            bool state_ok = interp.current_scope == parent &&
                            interp.releasing_scope == child;
            bool ctx_clear = interp.active_promotion_ctx == null;
            bool promoted_same = promoted == src;
            bool iter_ok = src != null &&
                           ctx.aborted &&
                           ctx.epoch != 0 &&
                           fallback_non_null &&
                           site_delta >= 1 &&
                           state_ok &&
                           ctx_clear;
            if (!iter_ok) {
                ok = false;
                failed_iter = i;
                failed_site_delta = site_delta;
                failed_aborted = ctx.aborted;
                failed_fallback_non_null = fallback_non_null;
                failed_state_ok = state_ok;
                failed_ctx_clear = ctx_clear;
                failed_promoted_same = promoted_same;
            }
        }

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion abort/copy fallback stress");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion abort/copy fallback stress (failed_iter=%d site_delta=%d aborted=%s fallback_non_null=%s state_ok=%s ctx_clear=%s promoted_same=%s)",
                (long)failed_iter,
                (long)failed_site_delta,
                failed_aborted ? "yes" : "no",
                failed_fallback_non_null ? "yes" : "no",
                failed_state_ok ? "yes" : "no",
                failed_ctx_clear ? "yes" : "no",
                failed_promoted_same ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_context_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_promotion_context_memo_test(interp, pass, fail);
    run_memory_lifetime_scope_chain_cache_test(interp, pass, fail);
    run_memory_lifetime_promotion_abort_fallback_test(interp, pass, fail);
    run_memory_lifetime_promotion_abort_nested_releasing_copy_test(interp, pass, fail);
    run_memory_lifetime_nested_escape_promotion_fail_closed_test(interp, pass, fail);
    run_memory_lifetime_nested_promotion_context_stack_test(interp, pass, fail);
    run_memory_lifetime_promotion_abort_copy_fallback_stress_test(interp, pass, fail);
}
