module lisp;


import std::io;
import main;

fn void run_memory_lifetime_jit_copy_hot_budget_test(Interp* interp, int* pass, int* fail) {
    // Invariant: core hot copy sites remain bounded on hot closure loop.
    {
        reset_memory_promotion_stats();

        EvalResult r = run(
            "(block "
            "  (define step (lambda (x) (+ x 1))) "
            "  (define mk (lambda (n) (lambda (x) (+ x n)))) "
            "  (let ^rec (loop (lambda (i acc) "
            "    (if (= i 600) "
            "        acc "
            "        (let (f (mk i)) "
            "          (loop (+ i 1) (step (+ acc (f 1)))))))) "
            "    (loop 0 0)))",
            interp);

        usz jit_single_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
        usz copy_env_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        usz run_jit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        usz tco_frame_delta = copy_to_parent_site_calls(COPY_SITE_JIT_TCO_FRAME_COPY);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 180900;
        bool ok_budget = jit_single_delta <= 32 &&
                         copy_env_delta <= 32 &&
                         run_jit_delta <= 8 &&
                         tco_frame_delta <= 64;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: jit/copy hot-site budget gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: jit/copy hot-site budget gate (jit_single=%d copy_env=%d run_jit=%d tco_frame=%d tag=%d int=%d err=%s)",
                (long)jit_single_delta,
                (long)copy_env_delta,
                (long)run_jit_delta,
                (long)tco_frame_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_frame_budget_test(Interp* interp, int* pass, int* fail) {
    // Invariant: named-let TCO fast path keeps frame-copy churn near-zero.
    {
        reset_memory_promotion_stats();

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 5000) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);

        usz tco_frame_delta = copy_to_parent_site_calls(COPY_SITE_JIT_TCO_FRAME_COPY);
        usz copy_env_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        usz run_jit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 5000;
        bool ok_budget = tco_frame_delta <= 16 &&
                         copy_env_delta <= 8 &&
                         run_jit_delta <= 4;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco frame-copy budget gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco frame-copy budget gate (tco_frame=%d copy_env=%d run_jit=%d tag=%d int=%d err=%s)",
                (long)tco_frame_delta,
                (long)copy_env_delta,
                (long)run_jit_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_boundary_state_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle/env-copy boundary paths restore interpreter scope state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 2000) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_state = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;
        bool ok = ok_eval && ok_state;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
                ok_eval ? "yes" : "no",
                ok_state ? "yes" : "no",
                interp.current_scope == saved_scope ? "yes" : "no",
                interp.releasing_scope == saved_releasing ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_list_accumulator_budget_test(Interp* interp, int* pass, int* fail) {
    // Invariant: recursive graph safety checks must not rescan already-safe cons tails
    // on every TCO bounce for list accumulators.
    {
        reset_memory_promotion_stats();

        EvalResult r = run(
            "(block "
            "  (define my-rev (lambda (lst) "
            "    (let loop (xs lst acc (quote ())) "
            "      (if (null? xs) "
            "          acc "
            "          (loop (cdr xs) (cons (car xs) acc)))))) "
            "  (length (my-rev (range 2000))))",
            interp);

        usz tco_frame_delta = copy_to_parent_site_calls(COPY_SITE_JIT_TCO_FRAME_COPY);
        usz copy_env_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        usz run_jit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_budget = tco_frame_delta <= 4096 &&
                         copy_env_delta <= 64 &&
                         run_jit_delta <= 8;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco list-accumulator budget gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco list-accumulator budget gate (tco_frame=%d copy_env=%d run_jit=%d tag=%d int=%d err=%s)",
                (long)tco_frame_delta,
                (long)copy_env_delta,
                (long)run_jit_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_runtime_fields_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO paths restore recycle/defer runtime fields on both success and error.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        usz saved_tco_slot = interp.tco_scope_defer_slot;
        bool saved_tco_active = interp.tco_scope_defer_active;
        bool saved_escape = interp.escape_env_mode;

        EvalResult ok_run = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 1024) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);
        bool ok_eval = !ok_run.error.has_error &&
                       ok_run.value != null &&
                       ok_run.value.tag == INT &&
                       ok_run.value.int_val == 1024;
        bool ok_state_after_ok =
            interp.current_scope == saved_scope &&
            interp.releasing_scope == saved_releasing &&
            interp.tco_recycle_scope == saved_tco_recycle &&
            interp.tco_scope_defer_slot == saved_tco_slot &&
            interp.tco_scope_defer_active == saved_tco_active &&
            interp.escape_env_mode == saved_escape;

        EvalResult err_run = run(
            "(let ^rec (loop (lambda (n) "
            "  (if (= n 256) "
            "      (car 1) "
            "      (loop (+ n 1))))) "
            "  (loop 0))",
            interp);
        bool err_eval = err_run.error.has_error;
        bool ok_state_after_err =
            interp.current_scope == saved_scope &&
            interp.releasing_scope == saved_releasing &&
            interp.tco_recycle_scope == saved_tco_recycle &&
            interp.tco_scope_defer_slot == saved_tco_slot &&
            interp.tco_scope_defer_active == saved_tco_active &&
            interp.escape_env_mode == saved_escape;

        bool ok = ok_eval && err_eval && ok_state_after_ok && ok_state_after_err;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco runtime fields restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco runtime fields restore (ok_eval=%s err_eval=%s state_ok=%s state_err=%s)",
                ok_eval ? "yes" : "no",
                err_eval ? "yes" : "no",
                ok_state_after_ok ? "yes" : "no",
                ok_state_after_err ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_recycle_error_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle error path restores state when defer retarget cannot run.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        usz saved_tco_slot = interp.tco_scope_defer_slot;
        bool saved_tco_active = interp.tco_scope_defer_active;
        Env* saved_jit_env = interp.jit_env;

        main::ScopeRegion* cs = main::scope_create(saved_scope);
        if (cs == null) {
            io::printn("[FAIL] lifetime: tco recycle error path state restore");
            (*fail)++;
            return;
        }

        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        interp.tco_scope_defer_slot = 0;
        interp.tco_scope_defer_active = true;
        interp.jit_env = interp.global_env;

        SymbolId sym = interp.symbols.intern("__tco_recycle_err");
        Env* env = make_env(interp, interp.global_env);
        env_define_with_barrier(env, sym, make_int(interp, 1), interp);
        Env* env_io = env;

        bool stack_ctx_missing = (main::g_current_stack_ctx == null);
        Value* err = jit_prepare_tco_recycle(&env_io, interp.global_env, interp);

        bool err_ok = err != null && err.tag == ERROR;
        bool env_ok = env_io == env;
        bool restored_ok = interp.current_scope == cs &&
                           interp.tco_recycle_scope == cs &&
                           interp.jit_env == interp.global_env;
        bool ok = stack_ctx_missing && err_ok && env_ok && restored_ok;

        interp.current_scope = saved_scope;
        interp.tco_recycle_scope = saved_tco_recycle;
        interp.tco_scope_defer_slot = saved_tco_slot;
        interp.tco_scope_defer_active = saved_tco_active;
        interp.jit_env = saved_jit_env;
        main::scope_release(cs);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco recycle error path state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco recycle error path state restore (stack_ctx_missing=%s err_ok=%s env_ok=%s restored_ok=%s)",
                stack_ctx_missing ? "yes" : "no",
                err_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                restored_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_hot_budget_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_jit_copy_hot_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_frame_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_list_accumulator_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_boundary_state_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_runtime_fields_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_recycle_error_restore_test(interp, pass, fail);
}
