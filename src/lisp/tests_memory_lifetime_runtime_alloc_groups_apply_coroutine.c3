module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_apply_coroutine_tests(Interp* interp, int* pass, int* fail) {

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* iter_raise = iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-map: expected iterator state");
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;
        bool ok = runtime_alloc_error_matches(iter_raise, "__iterator-map: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator ctor raise survives payload hashmap alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator ctor raise survives payload hashmap alloc failure (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(iter_raise));
            (*fail)++;
        }
    }

    {
        Value* invalid_iter = interp.alloc_value();
        bool setup_ok = invalid_iter != null;
        if (setup_ok) {
            invalid_iter.tag = ITERATOR;
            invalid_iter.iterator_val = make_nil(interp);
        }
        Value*[1] args = { invalid_iter };
        Value* ctor_err = setup_ok ? prim_ctor_iterator(args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* next_err = setup_ok ? prim_next(args[0:1], null, interp) : raise_error(interp, "setup failed");

        bool ok = setup_ok &&
            runtime_alloc_error_matches(ctor_err, "Iterator: expected valid iterator") &&
            runtime_alloc_error_matches(next_err, "next: expected iterator");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator wrapper fails constructor and terminal gates");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator wrapper fails constructor and terminal gates (setup_ok=%s ctor=%s next=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(ctor_err),
                runtime_alloc_error_text(next_err));
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value* pred = prim_coroutine_p(no_args[0:0], null, interp);
        Value* null_ctx = make_coroutine(interp, null);

        bool ok =
            pred != null &&
            pred.tag == NIL &&
            runtime_alloc_error_matches(null_ctx, "coroutine: missing context");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine predicate and constructor reject malformed inputs");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine predicate and constructor reject malformed inputs (pred_tag=%d msg=%s)",
                pred != null ? (int)pred.tag : -1,
                runtime_alloc_error_text(null_ctx));
            (*fail)++;
        }
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        if (setup_ok) {
            tmp.init();
            setup_ok = register_primitives(tmp);
        }

        main::ScopeRegion* saved = setup_ok ? tmp.current_scope : null;
        main::ScopeRegion* foreign = setup_ok ? main::scope_create(saved) : null;
        Value* thunk = null;
        usz before_all = 0;
        usz before_pool = 0;
        Value* result = null;

        if (setup_ok) {
            setup_ok = foreign != null;
        }
        if (setup_ok) {
            tmp.current_scope = foreign;
            thunk = make_closure_no_param(tmp, null, tmp.global_env);
            tmp.current_scope = saved;
            setup_ok = thunk != null && thunk.tag == CLOSURE;
        }
        if (setup_ok) {
            Value*[1] coro_args = { thunk };
            bool saved_force = g_boundary_force_copy_closure_wrapper_alloc_fail;
            before_all = tmp.stack_ctx_pool.all_count;
            before_pool = tmp.stack_ctx_pool.pool_size;
            tmp.current_scope = foreign;
            g_boundary_force_copy_closure_wrapper_alloc_fail = true;
            result = prim_coroutine(coro_args[0:1], null, tmp);
            g_boundary_force_copy_closure_wrapper_alloc_fail = saved_force;
            tmp.current_scope = saved;
        }

        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            tmp.stack_ctx_pool.all_count == before_all &&
            tmp.stack_ctx_pool.pool_size == before_pool;
        int after_all = tmp != null ? (int)tmp.stack_ctx_pool.all_count : -1;
        int after_pool = tmp != null ? (int)tmp.stack_ctx_pool.pool_size : -1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine thunk promotion failure aborts before context creation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine thunk promotion failure aborts before context creation (setup_ok=%s tag=%d all=%d pool=%d before_all=%d before_pool=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1,
                after_all,
                after_pool,
                (int)before_all,
                (int)before_pool);
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
        if (tmp != null) {
            tmp.destroy();
            mem::free(tmp);
        }
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        if (setup_ok) {
            tmp.init();
            setup_ok = register_primitives(tmp);
        }

        Value* thunk = null;
        usz before_all = 0;
        usz before_pool = 0;
        Value* result = null;

        if (setup_ok) {
            thunk = make_closure_no_param(tmp, null, tmp.global_env);
            setup_ok = thunk != null && thunk.tag == CLOSURE;
        }
        if (setup_ok) {
            Value*[1] coro_args = { thunk };
            int saved_force = g_boundary_force_alloc_value_fail_nth;
            before_all = tmp.stack_ctx_pool.all_count;
            before_pool = tmp.stack_ctx_pool.pool_size;
            g_boundary_force_alloc_value_fail_nth = 1;
            result = prim_coroutine(coro_args[0:1], null, tmp);
            g_boundary_force_alloc_value_fail_nth = saved_force;
        }

        bool asan_stack_pool = main::stack_runtime_asan_enabled();
        usz expected_all = before_all + (asan_stack_pool ? 0 : 1);
        usz expected_pool = before_pool + (asan_stack_pool ? 0 : 1);

        bool ok = setup_ok &&
            runtime_alloc_error_matches(result, "coroutine: failed to allocate coroutine value") &&
            tmp.stack_ctx_pool.all_count == expected_all &&
            tmp.stack_ctx_pool.pool_size == expected_pool;
        int after_all = tmp != null ? (int)tmp.stack_ctx_pool.all_count : -1;
        int after_pool = tmp != null ? (int)tmp.stack_ctx_pool.pool_size : -1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine wrapper allocation failure cleans up context");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine wrapper allocation failure cleans up context (setup_ok=%s msg=%s all=%d pool=%d before_all=%d before_pool=%d)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(result),
                after_all,
                after_pool,
                (int)before_all,
                (int)before_pool
            );
            (*fail)++;
        }

        if (tmp != null) {
            tmp.destroy();
            mem::free(tmp);
        }
    }

    {
        Value* partial = interp.alloc_value();
        Value* arg = make_int(interp, 42);
        bool setup_ok = partial != null && arg != null && arg.tag == INT;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.first_arg = make_int(interp, 1);
            partial.partial_val.second_arg = null;
            partial.partial_val.remaining = 2;
        }

        bool saved_force = g_eval_force_partial_alloc_fail;
        g_eval_force_partial_alloc_fail = true;
        EvalResult result = setup_ok
            ? apply_partial(partial, arg, interp)
            : eval_error("setup failed");
        g_eval_force_partial_alloc_fail = saved_force;

        bool ok = setup_ok &&
            result.error.has_error &&
            str_starts_with(result.error.message[:256], "out of memory while allocating partial application");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply_partial alloc failure returns eval error");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply_partial alloc failure returns eval error (setup_ok=%s error=%s)",
                setup_ok ? "yes" : "no",
                result.error.has_error ? (ZString)result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        Value* partial = interp.alloc_value();
        Value* arg = make_int(interp, 42);
        bool setup_ok = partial != null && arg != null && arg.tag == INT;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.first_arg = make_int(interp, 1);
            partial.partial_val.second_arg = make_int(interp, 2);
            partial.partial_val.remaining = 3;
        }

        EvalResult result = setup_ok
            ? apply_partial(partial, arg, interp)
            : eval_error("setup failed");

        bool ok = setup_ok &&
            result.error.has_error &&
            str_starts_with(result.error.message[:256], "partial application state exceeds supported captured-arg width");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply_partial oversized state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply_partial oversized state fails closed (setup_ok=%s error=%s)",
                setup_ok ? "yes" : "no",
                result.error.has_error ? (ZString)result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* escape_first = interp.alloc_value_escape();
            Value* escape_second = interp.alloc_value_escape();
            Value* partial = interp.alloc_value();
            setup_ok = escape_first != null && escape_second != null && partial != null;
            if (setup_ok) {
                escape_first.tag = INT;
                escape_first.int_val = 1;
                escape_second.tag = INT;
                escape_second.int_val = 2;
                partial.tag = PARTIAL_PRIM;
                partial.partial_val.func = &prim_add;
                partial.partial_val.first_arg = escape_first;
                partial.partial_val.second_arg = escape_second;
                partial.partial_val.remaining = 1;
                int saved_force = g_boundary_force_alloc_escape_value_fail_nth;
                g_boundary_force_alloc_escape_value_fail_nth = 1;
                promoted = promote_to_escape(partial, interp);
                g_boundary_force_alloc_escape_value_fail_nth = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(promoted, "boundary: failed to allocate partial primitive escape copy");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: partial primitive escape wrapper alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: partial primitive escape wrapper alloc faults fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(promoted));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        Value* arg = make_int(interp, 42);
        Value* negative_partial = interp.alloc_value();
        Value* null_func_partial = interp.alloc_value();
        Value* inconsistent_partial = interp.alloc_value();
        Value* null_first_arg_partial = interp.alloc_value();
        Value* invalid_prim = interp.alloc_value();
        bool setup_ok = arg != null && arg.tag == INT &&
            negative_partial != null &&
            null_func_partial != null &&
            inconsistent_partial != null &&
            null_first_arg_partial != null &&
            invalid_prim != null;
        if (setup_ok) {
            *negative_partial = {};
            negative_partial.tag = PARTIAL_PRIM;
            negative_partial.partial_val.func = &prim_add;
            negative_partial.partial_val.first_arg = make_int(interp, 1);
            negative_partial.partial_val.second_arg = null;
            negative_partial.partial_val.remaining = -1;

            *null_func_partial = {};
            null_func_partial.tag = PARTIAL_PRIM;
            null_func_partial.partial_val.func = null;
            null_func_partial.partial_val.first_arg = make_int(interp, 1);
            null_func_partial.partial_val.second_arg = null;
            null_func_partial.partial_val.remaining = 1;

            *inconsistent_partial = {};
            inconsistent_partial.tag = PARTIAL_PRIM;
            inconsistent_partial.partial_val.func = &prim_add;
            inconsistent_partial.partial_val.first_arg = make_int(interp, 1);
            inconsistent_partial.partial_val.second_arg = make_int(interp, 2);
            inconsistent_partial.partial_val.remaining = 2;

            *null_first_arg_partial = {};
            null_first_arg_partial.tag = PARTIAL_PRIM;
            null_first_arg_partial.partial_val.func = &prim_add;
            null_first_arg_partial.partial_val.first_arg = null;
            null_first_arg_partial.partial_val.second_arg = null;
            null_first_arg_partial.partial_val.remaining = 1;

            *invalid_prim = {};
            invalid_prim.tag = PRIMITIVE;
            invalid_prim.prim_val = null;

            setup_ok = negative_partial.partial_val.first_arg != null &&
                null_func_partial.partial_val.first_arg != null &&
                inconsistent_partial.partial_val.first_arg != null &&
                inconsistent_partial.partial_val.second_arg != null;
        }

        EvalResult negative_result = setup_ok
            ? apply_partial(negative_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult null_func_result = setup_ok
            ? apply_partial(null_func_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult inconsistent_result = setup_ok
            ? apply_partial(inconsistent_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult null_first_arg_result = setup_ok
            ? apply_partial(null_first_arg_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult invalid_prim_result = setup_ok
            ? apply_primitive(invalid_prim, arg, interp)
            : eval_error("setup failed");

        bool ok = setup_ok &&
            negative_result.error.has_error &&
            null_func_result.error.has_error &&
            inconsistent_result.error.has_error &&
            null_first_arg_result.error.has_error &&
            invalid_prim_result.error.has_error &&
            str_starts_with(negative_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(null_func_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(inconsistent_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(null_first_arg_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(invalid_prim_result.error.message[:256], "invalid primitive application state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply helpers invalid states fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply helpers invalid states fail closed (setup_ok=%s neg=%s null_fn=%s inconsistent=%s null_first=%s invalid_prim=%s)",
                setup_ok ? "yes" : "no",
                negative_result.error.has_error ? (ZString)negative_result.error.message[:256] : (ZString)"<none>",
                null_func_result.error.has_error ? (ZString)null_func_result.error.message[:256] : (ZString)"<none>",
                inconsistent_result.error.has_error ? (ZString)inconsistent_result.error.message[:256] : (ZString)"<none>",
                null_first_arg_result.error.has_error ? (ZString)null_first_arg_result.error.message[:256] : (ZString)"<none>",
                invalid_prim_result.error.has_error ? (ZString)invalid_prim_result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }
}
