module lisp;


import std::io;
import main;

struct FinalizePathOutcome {
    Value* value;
    bool scope_spliced;
    usz site_delta;
    bool scope_state_restored;
}

fn bool finalize_alloc_scope_pair(
    main::ScopeRegion* parent,
    main::ScopeRegion** saved_scope_out,
    main::ScopeRegion** active_scope_out
) {
    main::ScopeRegion* saved_scope = main::scope_create(parent);
    if (saved_scope == null) return false;

    main::ScopeRegion* active_scope = main::scope_create(saved_scope);
    if (active_scope == null) {
        main::scope_release(saved_scope);
        return false;
    }

    *saved_scope_out = saved_scope;
    *active_scope_out = active_scope;
    return true;
}

fn Value* finalize_make_temp_int_list(Interp* interp, usz len) {
    Value* acc = make_nil(interp);
    for (usz i = 0; i < len; i++) {
        acc = make_cons(interp, make_int(interp, (long)(i + 1)), acc);
    }
    return acc;
}

fn bool finalize_cons_head_matches(Value* value, long expected_head) @inline {
    return value != null &&
           value.tag == CONS &&
           value.cons_val.car != null &&
           value.cons_val.car.tag == INT &&
           value.cons_val.car.int_val == expected_head;
}

fn FinalizePathOutcome run_eval_finalize_path(
    Interp* interp,
    Value* result,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope
) {
    usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
    main::ScopeRegion* saved_releasing = interp.releasing_scope;

    interp.current_scope = active_scope;
    bool scope_spliced = false;
    Value* final = run_promote_result(result, interp, active_scope, saved_scope, &scope_spliced);
    boundary_pop_child_scope(interp, saved_scope, scope_spliced ? null : active_scope);

    return {
        .value = final,
        .scope_spliced = scope_spliced,
        .site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site,
        .scope_state_restored = interp.current_scope == saved_scope &&
                                interp.releasing_scope == saved_releasing,
    };
}

fn FinalizePathOutcome run_jit_finalize_path(
    Interp* interp,
    Value* result,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope,
    uint copy_site
) {
    usz base_site = copy_to_parent_site_calls(copy_site);
    main::ScopeRegion* saved_releasing = interp.releasing_scope;

    interp.current_scope = saved_scope;
    bool scope_spliced = false;
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope,
        &scope_spliced
    );

    return {
        .value = final,
        .scope_spliced = scope_spliced,
        .site_delta = copy_to_parent_site_calls(copy_site) - base_site,
        .scope_state_restored = interp.current_scope == saved_scope &&
                                interp.releasing_scope == saved_releasing,
    };
}

fn void run_memory_lifetime_finalize_splice_parity_test(Interp* interp, int* pass, int* fail) {
    // Invariant: eval and JIT scoped finalize paths both commit TEMP cons safely
    // (either splice or destination-aware ESCAPE build) without fallback copy.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* eval_saved = null;
        main::ScopeRegion* eval_active = null;
        main::ScopeRegion* jit_saved = null;
        main::ScopeRegion* jit_active = null;

        bool eval_alloc_ok = finalize_alloc_scope_pair(root_scope, &eval_saved, &eval_active);
        bool jit_alloc_ok = finalize_alloc_scope_pair(root_scope, &jit_saved, &jit_active);
        if (!eval_alloc_ok || !jit_alloc_ok) {
            if (jit_active != null) main::scope_release(jit_active);
            if (jit_saved != null) main::scope_release(jit_saved);
            if (eval_active != null) main::scope_release(eval_active);
            if (eval_saved != null) main::scope_release(eval_saved);
            io::printn("[FAIL] lifetime: finalize promote+splice parity");
            (*fail)++;
            return;
        }

        interp.current_scope = eval_active;
        Value* eval_src = make_cons(interp, make_int(interp, 41), make_nil(interp));

        interp.current_scope = jit_active;
        Value* jit_src = make_cons(interp, make_int(interp, 41), make_nil(interp));

        FinalizePathOutcome eval_out = run_eval_finalize_path(interp, eval_src, eval_saved, eval_active);
        FinalizePathOutcome jit_out = run_jit_finalize_path(
            interp,
            jit_src,
            jit_saved,
            jit_active,
            COPY_SITE_JIT_SINGLE_SCOPE_RETURN
        );

        bool eval_committed_ok = eval_out.scope_spliced ||
                                 (eval_out.value != null &&
                                  eval_out.value.scope_gen == eval_saved.escape_generation);
        bool jit_committed_ok = jit_out.scope_spliced ||
                                (jit_out.value != null &&
                                 jit_out.value.scope_gen == jit_saved.escape_generation);
        bool eval_ok = eval_committed_ok &&
                       eval_out.site_delta == 0 &&
                       eval_out.scope_state_restored &&
                       finalize_cons_head_matches(eval_out.value, 41) &&
                       main::is_in_scope((void*)eval_out.value, eval_saved);
        bool jit_ok = jit_committed_ok &&
                      jit_out.site_delta == 0 &&
                      jit_out.scope_state_restored &&
                      finalize_cons_head_matches(jit_out.value, 41) &&
                      main::is_in_scope((void*)jit_out.value, jit_saved);

        interp.current_scope = root_scope;
        main::scope_release(jit_saved);
        main::scope_release(eval_saved);

        bool ok = eval_ok && jit_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: finalize promote+splice parity");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: finalize promote+splice parity (eval_committed=%s eval_spliced=%s eval_site=%d eval_state=%s jit_committed=%s jit_spliced=%s jit_site=%d jit_state=%s)",
                eval_committed_ok ? "yes" : "no",
                eval_out.scope_spliced ? "yes" : "no",
                (long)eval_out.site_delta,
                eval_out.scope_state_restored ? "yes" : "no",
                jit_committed_ok ? "yes" : "no",
                jit_out.scope_spliced ? "yes" : "no",
                (long)jit_out.site_delta,
                jit_out.scope_state_restored ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_finalize_abort_fallback_parity_test(Interp* interp, int* pass, int* fail) {
    // Invariant: eval and JIT scoped finalize paths both commit large TEMP graphs
    // without copy-site fallback when return-finalize uses unbounded promotion context.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* eval_saved = null;
        main::ScopeRegion* eval_active = null;
        main::ScopeRegion* jit_saved = null;
        main::ScopeRegion* jit_active = null;

        bool eval_alloc_ok = finalize_alloc_scope_pair(root_scope, &eval_saved, &eval_active);
        bool jit_alloc_ok = finalize_alloc_scope_pair(root_scope, &jit_saved, &jit_active);
        if (!eval_alloc_ok || !jit_alloc_ok) {
            if (jit_active != null) main::scope_release(jit_active);
            if (jit_saved != null) main::scope_release(jit_saved);
            if (eval_active != null) main::scope_release(eval_active);
            if (eval_saved != null) main::scope_release(eval_saved);
            io::printn("[FAIL] lifetime: finalize abort fallback parity");
            (*fail)++;
            return;
        }

        usz list_len = PROMOTION_CTX_BUDGET_RETURN + 128;

        interp.current_scope = eval_active;
        Value* eval_src = finalize_make_temp_int_list(interp, list_len);

        interp.current_scope = jit_active;
        Value* jit_src = finalize_make_temp_int_list(interp, list_len);

        FinalizePathOutcome eval_out = run_eval_finalize_path(interp, eval_src, eval_saved, eval_active);
        FinalizePathOutcome jit_out = run_jit_finalize_path(
            interp,
            jit_src,
            jit_saved,
            jit_active,
            COPY_SITE_JIT_SINGLE_SCOPE_RETURN
        );

        bool eval_committed_ok = eval_out.scope_spliced ||
                                 (eval_out.value != null &&
                                  eval_out.value.scope_gen == eval_saved.escape_generation);
        bool jit_committed_ok = jit_out.scope_spliced ||
                                (jit_out.value != null &&
                                 jit_out.value.scope_gen == jit_saved.escape_generation);
        bool eval_ok = eval_committed_ok &&
                       eval_out.site_delta == 0 &&
                       eval_out.scope_state_restored &&
                       finalize_cons_head_matches(eval_out.value, (long)list_len) &&
                       main::is_in_scope((void*)eval_out.value, eval_saved);
        bool jit_ok = jit_committed_ok &&
                      jit_out.site_delta == 0 &&
                      jit_out.scope_state_restored &&
                      finalize_cons_head_matches(jit_out.value, (long)list_len) &&
                      main::is_in_scope((void*)jit_out.value, jit_saved);

        interp.current_scope = root_scope;
        main::scope_release(jit_saved);
        main::scope_release(eval_saved);

        bool ok = eval_ok && jit_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: finalize abort fallback parity");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: finalize abort fallback parity (eval_spliced=%s eval_site=%d eval_state=%s eval_same=%s jit_spliced=%s jit_site=%d jit_state=%s jit_same=%s)",
                eval_out.scope_spliced ? "yes" : "no",
                (long)eval_out.site_delta,
                eval_out.scope_state_restored ? "yes" : "no",
                (eval_out.value == eval_src) ? "yes" : "no",
                jit_out.scope_spliced ? "yes" : "no",
                (long)jit_out.site_delta,
                jit_out.scope_state_restored ? "yes" : "no",
                (jit_out.value == jit_src) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_finalize_foreign_fast_return_parity_test(Interp* interp, int* pass, int* fail) {
    // Invariant: eval and JIT scoped finalize paths both fast-return values already outside the releasing scope.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* eval_saved = null;
        main::ScopeRegion* eval_active = null;
        main::ScopeRegion* jit_saved = null;
        main::ScopeRegion* jit_active = null;

        bool eval_alloc_ok = finalize_alloc_scope_pair(root_scope, &eval_saved, &eval_active);
        bool jit_alloc_ok = finalize_alloc_scope_pair(root_scope, &jit_saved, &jit_active);
        if (!eval_alloc_ok || !jit_alloc_ok) {
            if (jit_active != null) main::scope_release(jit_active);
            if (jit_saved != null) main::scope_release(jit_saved);
            if (eval_active != null) main::scope_release(eval_active);
            if (eval_saved != null) main::scope_release(eval_saved);
            io::printn("[FAIL] lifetime: finalize foreign fast-return parity");
            (*fail)++;
            return;
        }

        interp.current_scope = eval_saved;
        Value* eval_src = make_cons(interp, make_int(interp, 73), make_nil(interp));

        interp.current_scope = jit_saved;
        Value* jit_src = make_cons(interp, make_int(interp, 73), make_nil(interp));

        FinalizePathOutcome eval_out = run_eval_finalize_path(interp, eval_src, eval_saved, eval_active);
        FinalizePathOutcome jit_out = run_jit_finalize_path(
            interp,
            jit_src,
            jit_saved,
            jit_active,
            COPY_SITE_JIT_SINGLE_SCOPE_RETURN
        );

        bool eval_ok = !eval_out.scope_spliced &&
                       eval_out.site_delta == 0 &&
                       eval_out.scope_state_restored &&
                       eval_out.value == eval_src &&
                       finalize_cons_head_matches(eval_out.value, 73) &&
                       main::is_in_scope((void*)eval_out.value, eval_saved);
        bool jit_ok = !jit_out.scope_spliced &&
                      jit_out.site_delta == 0 &&
                      jit_out.scope_state_restored &&
                      jit_out.value == jit_src &&
                      finalize_cons_head_matches(jit_out.value, 73) &&
                      main::is_in_scope((void*)jit_out.value, jit_saved);

        interp.current_scope = root_scope;
        main::scope_release(jit_saved);
        main::scope_release(eval_saved);

        bool ok = eval_ok && jit_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: finalize foreign fast-return parity");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: finalize foreign fast-return parity (eval_spliced=%s eval_site=%d eval_state=%s eval_same=%s jit_spliced=%s jit_site=%d jit_state=%s jit_same=%s)",
                eval_out.scope_spliced ? "yes" : "no",
                (long)eval_out.site_delta,
                eval_out.scope_state_restored ? "yes" : "no",
                (eval_out.value == eval_src) ? "yes" : "no",
                jit_out.scope_spliced ? "yes" : "no",
                (long)jit_out.site_delta,
                jit_out.scope_state_restored ? "yes" : "no",
                (jit_out.value == jit_src) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_finalize_parity_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_finalize_splice_parity_test(interp, pass, fail);
    run_memory_lifetime_finalize_abort_fallback_parity_test(interp, pass, fail);
    run_memory_lifetime_finalize_foreign_fast_return_parity_test(interp, pass, fail);
}

fn bool run_memory_lifetime_result_and_coroutine_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: run() must return a value that lives in caller scope.
    // For closure results, this catches stale child-scope pointers.
    {
        EvalResult r = run("((lambda (x) (lambda (y) x)) 10)", interp);
        bool ok = !r.error.has_error &&
                  r.value != null &&
                  main::is_in_scope((void*)r.value, interp.current_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run promotes closure result to caller scope");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: run promotes closure result to caller scope");
            (*fail)++;
        }
    }

    // Invariant: completed user coroutines should return their StackCtx to the pool.
    // This uses a fresh interpreter so pool accounting is deterministic.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        tmp.init();
        register_primitives(tmp);

        EvalResult r_thunk = run("(lambda () 42)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: completed coroutine returns StackCtx to pool (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;

        usz before = tmp.stack_ctx_pool.pool_size;
        Value* c = prim_coroutine(coro_args[..], null, tmp);

        Value*[1] resume_args;
        resume_args[0] = c;
        Value* r = prim_resume(resume_args[..], null, tmp);
        usz after = tmp.stack_ctx_pool.pool_size;

        bool ok = c != null &&
                  c.tag == COROUTINE &&
                  r != null &&
                  r.tag == INT &&
                  r.int_val == 42 &&
                  after == before + 1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: completed coroutine returns StackCtx to pool");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: completed coroutine returns StackCtx to pool (before=%d after=%d, coro_tag=%d, result_tag=%d)",
                (int)before, (int)after,
                c != null ? (int)c.tag : -1,
                r != null ? (int)r.tag : -1);
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    return true;
}

fn void run_memory_lifetime_cons_escape_promotion_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: dual-lane cons barrier promotes TEMP inputs when cdr is ESCAPE-lane.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Value* temp_car = make_int(interp, 7); // TEMP lane
        Value* escape_nil = interp.alloc_value_escape();
        escape_nil.tag = NIL;
        Value* pair = make_cons(interp, temp_car, escape_nil);

        bool ok = pair != null &&
                  pair.scope_gen == s.escape_generation &&
                  pair.cons_val.car != null &&
                  pair.cons_val.car.scope_gen == s.escape_generation &&
                  pair.cons_val.cdr != null &&
                  pair.cons_val.cdr.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: dual-lane make_cons promotes temp->escape");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: dual-lane make_cons promotes temp->escape");
            (*fail)++;
        }
    }

    // Invariant: promote_to_escape upgrades TEMP list wrappers to ESCAPE lane.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Value* tmp_nil = make_nil(interp);
        Value* tmp_list = make_cons(interp, make_int(interp, 1), tmp_nil);
        Value* esc_list = promote_to_escape(tmp_list, interp);

        bool ok = esc_list != null &&
                  esc_list.scope_gen == s.escape_generation &&
                  esc_list.tag == CONS &&
                  esc_list.cons_val.car != null &&
                  esc_list.cons_val.car.scope_gen == s.escape_generation &&
                  esc_list.cons_val.cdr != null &&
                  esc_list.cons_val.cdr.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promote_to_escape upgrades temp list");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: promote_to_escape upgrades temp list");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_lane_barrier_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env_define_with_barrier promotes temp payload for ESCAPE-lane env.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_define");
        Value* tmp = make_int(interp, 11);
        env_define_with_barrier(e, sid, tmp, interp);
        Value* got = e.lookup(sid);

        bool ok = got != null &&
                  got.tag == INT &&
                  got.int_val == 11 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_define_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_define_with_barrier promotes temp value");
            (*fail)++;
        }
    }

    // Invariant: env_set_with_barrier promotes temp payload at write target frame.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_set");
        env_define_with_barrier(e, sid, make_int(interp, 1), interp);
        Value* tmp_new = make_int(interp, 99);
        bool set_ok = true;
        if (catch err = env_set_with_barrier(e, sid, tmp_new, interp)) {
            set_ok = false;
        }
        Value* got = e.lookup(sid);

        bool ok = set_ok &&
                  got != null &&
                  got.tag == INT &&
                  got.int_val == 99 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_set_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_set_with_barrier promotes temp value");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_lane_barrier_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_cons_escape_promotion_tests(interp, pass, fail);
    run_memory_lifetime_env_lane_barrier_tests(interp, pass, fail);
}

fn void run_memory_lifetime_root_boundary_promotion_test(Interp* interp, int* pass, int* fail) {
    // Invariant: root-boundary promotion handles disjoint scope values through
    // explicit destination routing without fallback copy-site usage.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);

        interp.current_scope = scope_b;
        Value* foreign = make_cons(interp, make_int(interp, 21), make_nil(interp));

        interp.current_scope = scope_a;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(foreign, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted != foreign &&
                      promoted.tag == CONS &&
                      promoted.cons_val.car != null &&
                      promoted.cons_val.car.tag == INT &&
                      promoted.cons_val.car.int_val == 21 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)promoted, scope_b);

        // Releasing source scope must not invalidate promoted value.
        main::scope_release(scope_b);

        bool post_ok = promoted != null &&
                       promoted.tag == CONS &&
                       promoted.cons_val.car != null &&
                       promoted.cons_val.car.tag == INT &&
                       promoted.cons_val.car.int_val == 21;

        interp.current_scope = saved;
        main::scope_release(scope_a);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary disjoint destination route");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary disjoint destination route (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == foreign) ? "yes" : "no",
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_direct_promotion_test(Interp* interp, int* pass, int* fail) {
    // Invariant: releasing-owned root-store values use direct destination
    // promotion and do not increment copy-site fallback counters.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary direct destination promotion (setup)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* local = make_cons(interp, make_int(interp, 37), make_nil(interp));
        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted != local &&
                      promoted.tag == CONS &&
                      boundary_ptr_in_scope((void*)promoted, interp.root_scope) &&
                      promoted.cons_val.car != null &&
                      promoted.cons_val.car.tag == INT &&
                      promoted.cons_val.car.int_val == 37 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)promoted, child);

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == CONS &&
                       promoted.cons_val.car != null &&
                       promoted.cons_val.car.tag == INT &&
                       promoted.cons_val.car.int_val == 37;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary direct destination promotion");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary direct destination promotion (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == local) ? "yes" : "no",
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_array_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned ARRAY payload crossing root-store boundary is
    // cloned into root lifetime (wrapper + payload), and source wrapper remains
    // valid until source scope release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = ARRAY;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);

        Array* arr = (Array*)mem::malloc(Array.sizeof);
        if (arr == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup array)");
            (*fail)++;
            return;
        }
        arr.capacity = 4;
        arr.length = 1;
        arr.items = (Value**)mem::malloc(Value*.sizeof * arr.capacity);
        if (arr.items == null) {
            mem::free(arr);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup items)");
            (*fail)++;
            return;
        }
        arr.items[0] = make_int(interp, 73);
        local.array_val = arr;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == ARRAY &&
                      promoted != local &&
                      promoted.array_val != null &&
                      local.array_val != null &&
                      promoted.array_val != local.array_val &&
                      promoted.array_val.length == 1 &&
                      promoted.array_val.items[0] != null &&
                      promoted.array_val.items[0].tag == INT &&
                      promoted.array_val.items[0].int_val == 73 &&
                      local.array_val.length == 1 &&
                      local.array_val.items[0] != null &&
                      local.array_val.items[0].tag == INT &&
                      local.array_val.items[0].int_val == 73 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == ARRAY &&
                       promoted.array_val != null &&
                       promoted.array_val.length == 1 &&
                       promoted.array_val.items[0] != null &&
                       promoted.array_val.items[0].tag == INT &&
                       promoted.array_val.items[0].int_val == 73;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary ARRAY clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary ARRAY clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_hashmap_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned HASHMAP payload crossing root-store boundary is
    // cloned into root lifetime and source wrapper remains valid until release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = HASHMAP;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);

        HashMap* map = (HashMap*)mem::malloc(HashMap.sizeof);
        if (map == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup map)");
            (*fail)++;
            return;
        }

        map.capacity = 8;
        map.count = 1;
        map.mask = map.capacity - 1;
        map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * map.capacity);
        if (map.entries == null) {
            mem::free(map);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < map.capacity; i++) {
            map.entries[i].key = null;
            map.entries[i].value = null;
        }
        Value* key_one = make_int(interp, 1);
        Value* val_ninety_nine = make_int(interp, 99);
        uint key_idx = hash_value(key_one) & map.mask;
        map.entries[key_idx].key = key_one;
        map.entries[key_idx].value = val_ninety_nine;
        local.hashmap_val = map;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        Value* promoted_key = null;
        Value* promoted_val = null;
        if (promoted != null && promoted.hashmap_val != null && promoted.hashmap_val.entries != null) {
            for (uint i = 0; i < promoted.hashmap_val.capacity; i++) {
                if (promoted.hashmap_val.entries[i].key != null) {
                    promoted_key = promoted.hashmap_val.entries[i].key;
                    promoted_val = promoted.hashmap_val.entries[i].value;
                    break;
                }
            }
        }

        bool pre_ok = promoted != null &&
                      promoted.tag == HASHMAP &&
                      promoted != local &&
                      promoted.hashmap_val != null &&
                      local.hashmap_val != null &&
                      promoted.hashmap_val != local.hashmap_val &&
                      promoted.hashmap_val.count == 1 &&
                      promoted_key != null &&
                      promoted_val != null &&
                      boundary_ptr_in_scope((void*)promoted_key, interp.root_scope) &&
                      boundary_ptr_in_scope((void*)promoted_val, interp.root_scope) &&
                      local.hashmap_val.count == 1 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        Value* got = (promoted != null && promoted.hashmap_val != null)
            ? hashmap_get(promoted.hashmap_val, make_int(interp, 1))
            : null;
        bool post_ok = got != null && got.tag == INT && got.int_val == 99;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary HASHMAP clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary HASHMAP clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_method_table_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned METHOD_TABLE payload crossing root-store boundary is
    // cloned into root lifetime and source wrapper remains valid until release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = METHOD_TABLE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.method_table_val = null;

        MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup table)");
            (*fail)++;
            return;
        }

        mt.name = interp.symbols.intern("root-boundary-method-table");
        mt.capacity = 1;
        mt.entry_count = 1;
        mt.fallback = make_int(interp, 33);
        mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt.capacity);
        if (mt.entries == null) {
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup entries)");
            (*fail)++;
            return;
        }

        method_signature_init_empty(&mt.entries[0].sig);
        MethodSignature src_sig;
        if (!method_signature_alloc_arrays_in_scope(&src_sig, child, 1, 0)) {
            mem::free(mt.entries);
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup signature)");
            (*fail)++;
            return;
        }
        src_sig.param_types[0] = interp.tid_Int;
        src_sig.val_literals[0].tag = NIL;
        src_sig.val_literals[0].int_value = 0;
        src_sig.val_literals[0].sym_value = 0;
        src_sig.has_val_literal[0] = false;
        if (!method_signature_copy_to_heap(&mt.entries[0].sig, &src_sig)) {
            mem::free(mt.entries);
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (copy signature)");
            (*fail)++;
            return;
        }
        mt.entries[0].implementation = make_int(interp, 42);
        local.method_table_val = mt;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_DEFINE);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_DEFINE);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_DEFINE) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == METHOD_TABLE &&
                      promoted != local &&
                      promoted.method_table_val != null &&
                      local.method_table_val != null &&
                      promoted.method_table_val != local.method_table_val &&
                      promoted.method_table_val.entry_count == 1 &&
                      promoted.method_table_val.entries != null &&
                      promoted.method_table_val.entries[0].sig.param_count == 1 &&
                      promoted.method_table_val.entries[0].sig.param_types[0] == interp.tid_Int &&
                      promoted.method_table_val.entries[0].implementation != null &&
                      promoted.method_table_val.entries[0].implementation.tag == INT &&
                      promoted.method_table_val.entries[0].implementation.int_val == 42 &&
                      promoted.method_table_val.fallback != null &&
                      promoted.method_table_val.fallback.tag == INT &&
                      promoted.method_table_val.fallback.int_val == 33 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == METHOD_TABLE &&
                       promoted.method_table_val != null &&
                       promoted.method_table_val.entry_count == 1 &&
                       promoted.method_table_val.entries != null &&
                       promoted.method_table_val.entries[0].implementation != null &&
                       promoted.method_table_val.entries[0].implementation.tag == INT &&
                       promoted.method_table_val.entries[0].implementation.int_val == 42 &&
                       promoted.method_table_val.fallback != null &&
                       promoted.method_table_val.fallback.tag == INT &&
                       promoted.method_table_val.fallback.int_val == 33;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary METHOD_TABLE clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_primitive_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned PRIMITIVE wrapper crossing root-store boundary is
    // cloned (wrapper + Primitive payload) to avoid shared-ownership aliasing.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = PRIMITIVE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.prim_val = null;

        Primitive* prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (prim == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (setup primitive)");
            (*fail)++;
            return;
        }
        *prim = {};
        prim.name[0] = 'p';
        prim.name[1] = 0;
        prim.arity = 2;
        prim.tag = 123;
        local.prim_val = prim;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == PRIMITIVE &&
                      promoted != local &&
                      promoted.prim_val != null &&
                      local.prim_val != null &&
                      promoted.prim_val != local.prim_val &&
                      promoted.prim_val.arity == 2 &&
                      promoted.prim_val.tag == 123 &&
                      promoted.prim_val.name[0] == 'p' &&
                      promoted.prim_val.name[1] == 0 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == PRIMITIVE &&
                       promoted.prim_val != null &&
                       promoted.prim_val.arity == 2 &&
                       promoted.prim_val.tag == 123 &&
                       promoted.prim_val.name[0] == 'p' &&
                       promoted.prim_val.name[1] == 0;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary PRIMITIVE clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_cons_barrier_fallback_gate_test(Interp* interp, int* pass, int* fail) {
    // Invariant: cons-barrier fallback copy sites must remain unused.
    // If these counters rise, an old path was accidentally reintroduced.
    {
        usz base_total = copy_to_parent_total_calls();
        usz base_car = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR);
        usz base_cdr = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR);

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) (if (= n 2000) acc (loop (+ n 1) (cons n acc))))) (length (loop 0 nil)))",
            interp);

        usz total_delta = copy_to_parent_total_calls() - base_total;
        usz car_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR) - base_car;
        usz cdr_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR) - base_cdr;

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_budget = car_delta == 0 && cdr_delta == 0;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: cons-barrier fallback sites stay at zero");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: cons-barrier fallback sites stay at zero (total_delta=%d car_delta=%d cdr_delta=%d tag=%d int=%d err=%s)",
                (long)total_delta,
                (long)car_delta,
                (long)cdr_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_disjoint_fallback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: disjoint-scope values commit directly to destination ESCAPE
    // without detouring through generic copy fallback accounting.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);

        interp.current_scope = foreign;
        Value* src = make_cons(interp, make_int(interp, 33), make_nil(interp));

        interp.current_scope = target;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_GENERIC);
        Value* escaped = promote_to_escape(src, interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_site;

        bool pre_ok = escaped != null &&
                      escaped != src &&
                      escaped.tag == CONS &&
                      escaped.scope_gen == target.escape_generation &&
                      escaped.cons_val.car != null &&
                      escaped.cons_val.car.tag == INT &&
                      escaped.cons_val.car.int_val == 33 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)escaped, foreign);

        main::scope_release(foreign);

        bool post_ok = escaped != null &&
                       escaped.tag == CONS &&
                       escaped.cons_val.car != null &&
                       escaped.cons_val.car.tag == INT &&
                       escaped.cons_val.car.int_val == 33;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape disjoint direct destination gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: escape disjoint direct destination gate (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_resume_missing_yield_scope_gate_test(Interp* interp, int* pass, int* fail) {
    // Invariant: coroutine resume boundary requires explicit yield provenance.
    // Missing yield_scope must hard-error and must not re-enter copy fallback.
    {
        Value* saved_yield = interp.yield_value;
        main::ScopeRegion* saved_yield_scope = interp.yield_scope;
        defer {
            interp.yield_value = saved_yield;
            interp.yield_scope = saved_yield_scope;
        }

        usz base_site = copy_to_parent_site_calls(COPY_SITE_PRIM_RESUME);
        interp.yield_value = make_int(interp, 5150);
        interp.yield_scope = null;

        Value* result = prim_resume_yield_result(interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_PRIM_RESUME) - base_site;

        bool ok = result != null &&
                  result.tag == ERROR &&
                  str_starts_with(result.str_chars[:result.str_len], "resume: missing yield-scope provenance") &&
                  interp.yield_value == null &&
                  interp.yield_scope == null &&
                  site_delta == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: resume missing yield provenance gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: resume missing yield provenance gate (site_delta=%d tag=%d state_yield=%s state_scope=%s)",
                (long)site_delta,
                result != null ? (int)result.tag : -1,
                interp.yield_value == null ? "cleared" : "dirty",
                interp.yield_scope == null ? "cleared" : "dirty");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_ast_arena_validation_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: AstArena is chunked bump storage with deterministic growth and stable teardown.
    {
        bool interp_available = interp != null;
        AstArena arena = {};
        ast_arena_init(&arena);
        bool init_ok = ast_arena_validate(&arena) && arena.chunks == null && arena.count == 0 && arena.capacity == 0;

        Expr* first_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        Expr* second_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        Pattern* first_pattern = (Pattern*)ast_arena_alloc(&arena, Pattern.sizeof);
        void* first_block = ast_arena_alloc(&arena, AST_ARENA_INITIAL_CHUNK_BYTES / 2);
        void* second_block = ast_arena_alloc(&arena, AST_ARENA_INITIAL_CHUNK_BYTES / 2);
        void* overflow_block = ast_arena_alloc(&arena, Expr.sizeof);

        bool alloc_ok = first_expr != null &&
                        second_expr != null &&
                        first_pattern != null &&
                        first_block != null &&
                        second_block != null &&
                        overflow_block != null &&
                        arena.count >= 2 &&
                        ((usz)first_expr % AST_ARENA_ALIGNMENT == 0) &&
                        ((usz)first_pattern % AST_ARENA_ALIGNMENT == 0) &&
                        ((usz)overflow_block % AST_ARENA_ALIGNMENT == 0) &&
                        ast_arena_contains(&arena, first_expr) &&
                        ast_arena_contains(&arena, first_pattern) &&
                        ast_arena_contains(&arena, first_block) &&
                        ast_arena_contains(&arena, second_block) &&
                        ast_arena_contains(&arena, overflow_block);

        bool overflow_gate_ok = true;
        AstArenaChunk* chunk = arena.chunks;
        if (chunk != null) {
            usz chunk_used_save = chunk.used;
            chunk.used = chunk.capacity + 1;
            overflow_gate_ok = !ast_arena_validate(&arena);
            chunk.used = chunk_used_save;
        } else {
            overflow_gate_ok = false;
        }

        usz count_before_destroy = arena.count;
        usz capacity_before_destroy = arena.capacity;
        ast_arena_destroy(&arena);
        bool destroy_ok = ast_arena_validate(&arena) &&
                          arena.chunks == null &&
                          arena.count == 0 &&
                          arena.capacity == 0 &&
                          count_before_destroy > 0;

        ast_arena_init(&arena);
        usz expected_capacity = arena.capacity;
        Expr* reinit_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        bool reinit_ok = ast_arena_validate(&arena) &&
                         reinit_expr != null &&
                         ast_arena_contains(&arena, reinit_expr) &&
                         expected_capacity == 0;

        void* foreign = mem::malloc(16);
        bool contains_foreign_is_false = foreign != null &&
                                         !ast_arena_contains(&arena, foreign);
        if (foreign != null) mem::free(foreign);

        ast_arena_destroy(&arena);
        bool ok = interp_available &&
                  init_ok &&
                  alloc_ok &&
                  overflow_gate_ok &&
                  destroy_ok &&
                  reinit_ok &&
                  contains_foreign_is_false;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ast_arena chunked allocator validation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ast_arena chunked allocator validation (init=%s alloc=%s overflow=%s destroy=%s reinit=%s contains=%s)",
                init_ok ? "yes" : "no",
                alloc_ok ? "yes" : "no",
                overflow_gate_ok ? "yes" : "no",
                destroy_ok ? "yes" : "no",
                reinit_ok ? "yes" : "no",
                contains_foreign_is_false ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_fallback_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_root_boundary_promotion_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_direct_promotion_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_array_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_hashmap_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_method_table_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_primitive_clone_test(interp, pass, fail);
    run_memory_lifetime_cons_barrier_fallback_gate_test(interp, pass, fail);
    run_memory_lifetime_escape_disjoint_fallback_test(interp, pass, fail);
    run_memory_lifetime_resume_missing_yield_scope_gate_test(interp, pass, fail);
}

fn void run_memory_lifetime_regression_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Lifetime Regression Tests ---");

    if (!run_memory_lifetime_result_and_coroutine_tests(interp, pass, fail)) return;
    run_memory_lifetime_finalize_parity_tests(interp, pass, fail);
    run_memory_lifetime_lane_barrier_tests(interp, pass, fail);
    run_memory_lifetime_root_fallback_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_restore_tests(interp, pass, fail);
    run_memory_lifetime_boundary_copy_fault_tests(interp, pass, fail);
    run_memory_lifetime_boundary_invariant_policy_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_interleaving_test(interp, pass, fail);
    run_memory_lifetime_boundary_return_classification_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_tests(interp, pass, fail);
    run_memory_lifetime_boundary_policy_parse_tests(interp, pass, fail);
    run_memory_lifetime_boundary_graph_audit_tests(interp, pass, fail);
    run_memory_lifetime_boundary_txn_protocol_tests(interp, pass, fail);
    run_memory_lifetime_boundary_negative_stress_tests(interp, pass, fail);
    run_memory_lifetime_run_program_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_run_program_error_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(interp, pass, fail);
    run_memory_lifetime_env_copy_escape_mode_tests(interp, pass, fail);
    run_memory_lifetime_hot_budget_tests(interp, pass, fail);
    run_memory_lifetime_promotion_context_tests(interp, pass, fail);
    run_memory_lifetime_ast_arena_validation_tests(interp, pass, fail);
    run_memory_lifetime_boundary_benchmarks(interp);
    run_memory_lifetime_ast_arena_benchmark(interp);
}
