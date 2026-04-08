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
