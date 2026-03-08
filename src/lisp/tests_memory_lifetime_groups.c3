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
        result,
        interp,
        saved_scope,
        active_scope,
        copy_site,
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
    // Invariant: root-boundary promotion keeps defensive-copy semantics for
    // disjoint scope values (value not owned by releasing scope and not rooted).
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
                      delta_in_range(site_delta, 1, 3) &&
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
            emit_pass_literal("[PASS] lifetime: root-boundary promotion defends disjoint scope");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary promotion defends disjoint scope (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == foreign) ? "yes" : "no",
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
    // Invariant: disjoint-scope values take defensive copy fallback before escape-lane promotion.
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
                      delta_in_range(site_delta, 1, 2) &&
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
            emit_pass_literal("[PASS] lifetime: escape disjoint fallback copy gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: escape disjoint fallback copy gate (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_fallback_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_root_boundary_promotion_test(interp, pass, fail);
    run_memory_lifetime_cons_barrier_fallback_gate_test(interp, pass, fail);
    run_memory_lifetime_escape_disjoint_fallback_test(interp, pass, fail);
}

fn void run_memory_lifetime_regression_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Lifetime Regression Tests ---");

    if (!run_memory_lifetime_result_and_coroutine_tests(interp, pass, fail)) return;
    run_memory_lifetime_finalize_parity_tests(interp, pass, fail);
    run_memory_lifetime_lane_barrier_tests(interp, pass, fail);
    run_memory_lifetime_root_fallback_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_restore_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_interleaving_test(interp, pass, fail);
    run_memory_lifetime_boundary_return_classification_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_tests(interp, pass, fail);
    run_memory_lifetime_boundary_graph_audit_tests(interp, pass, fail);
    run_memory_lifetime_boundary_txn_protocol_tests(interp, pass, fail);
    run_memory_lifetime_boundary_negative_stress_tests(interp, pass, fail);
    run_memory_lifetime_run_program_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_run_program_error_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(interp, pass, fail);
    run_memory_lifetime_env_copy_escape_mode_tests(interp, pass, fail);
    run_memory_lifetime_hot_budget_tests(interp, pass, fail);
    run_memory_lifetime_promotion_context_tests(interp, pass, fail);
    run_memory_lifetime_boundary_benchmarks(interp);
}
