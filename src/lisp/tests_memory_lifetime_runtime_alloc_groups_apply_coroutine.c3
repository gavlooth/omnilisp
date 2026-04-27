module lisp;

import std::io;
import std::core::mem;
import main;
import lisp::aot;

struct AotCaptureScopeTestData {
    Value* captured;
}

struct PrimitiveUserDataHookTestPayload {
    int marker;
    int* copy_count;
    int* finalizer_count;
}

bool g_primitive_user_data_hook_copy_fail = false;

fn Value* compiler_aot_test_return_captured_value(void* data, Value* arg) {
    AotCaptureScopeTestData* payload = (AotCaptureScopeTestData*)data;
    return payload != null && payload.captured != null ? payload.captured : aot::make_nil();
}

fn void* primitive_user_data_hook_copy(void* data) {
    if (g_primitive_user_data_hook_copy_fail) return null;
    PrimitiveUserDataHookTestPayload* src = (PrimitiveUserDataHookTestPayload*)data;
    if (src == null) return null;
    PrimitiveUserDataHookTestPayload* dst = (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof);
    if (dst == null) return null;
    *dst = *src;
    if (dst.copy_count != null) (*dst.copy_count)++;
    return (void*)dst;
}

fn void primitive_user_data_hook_finalizer(void* data) {
    PrimitiveUserDataHookTestPayload* payload = (PrimitiveUserDataHookTestPayload*)data;
    if (payload == null) return;
    if (payload.finalizer_count != null) (*payload.finalizer_count)++;
    mem::free(payload);
}

fn Value* primitive_user_data_hook_apply(Value*[] args, Env* env, Interp* interp) {
    PrimitiveUserDataHookTestPayload* payload = interp != null
        ? (PrimitiveUserDataHookTestPayload*)interp.prim_user_data
        : null;
    return payload != null ? make_int(interp, payload.marker) : make_error(interp, "test: missing primitive user_data");
}

fn void run_memory_lifetime_runtime_alloc_groups_apply_coroutine_tests(Interp* interp, int* pass, int* fail) {

    run_memory_lifetime_runtime_alloc_groups_apply_coroutine_user_data_tests(interp, pass, fail);

    {
        bool setup_ok = aot::aot_init();
        Interp* aot_interp = setup_ok ? aot::aot_interp() : null;
        main::ScopeRegion* saved_scope = null;
        main::ScopeRegion* child_scope = aot_interp != null ? boundary_push_child_scope(aot_interp, &saved_scope) : null;
        int child_destroy_count = 0;
        bool dtor_registered = false;
        bool closure_created = false;
        bool child_released_at_pop = false;
        if (child_scope != null) {
            dtor_registered = main::scope_register_dtor(child_scope, (void*)&child_destroy_count, fn void(void* ptr) {
                int* counter = (int*)ptr;
                if (counter != null) (*counter)++;
            });
            Value* captured = aot::cons(aot::make_string("captured"), aot::make_nil());
            aot::AotCaptureRetention* retention = aot::capture_retention_begin();
            Value* retain_error = retention != null ? aot::capture_retention_retain_value(retention, captured) : aot::make_error("test: missing capture retention");
            AotCaptureScopeTestData* payload = (AotCaptureScopeTestData*)mem::malloc(AotCaptureScopeTestData.sizeof);
            if (payload != null) payload.captured = captured;
            Value* closure = retain_error == null && payload != null
                ? aot::make_generated_closure_with_retention((void*)payload, &compiler_aot_test_return_captured_value, retention)
                : null;
            if (retain_error != null && retention != null) {
                aot::capture_retention_release(retention);
            }
            if ((closure == null || closure.tag == ValueTag.ERROR) && payload != null) {
                mem::free(payload);
            }
            closure_created = closure != null && closure.tag == ValueTag.PRIMITIVE;
            boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
            child_scope = null;
            child_released_at_pop = dtor_registered && closure_created && child_destroy_count == 1;
        }
        if (child_scope != null && aot_interp != null) {
            boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
        }
        if (setup_ok) aot::aot_shutdown();

        bool ok = setup_ok &&
            dtor_registered &&
            closure_created &&
            child_released_at_pop &&
            child_destroy_count == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: unreturned AOT closure capture releases child scope at local teardown");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: unreturned AOT closure capture releases child scope at local teardown (setup=%s dtor=%s closure=%s released=%s count=%d)",
                setup_ok ? "yes" : "no",
                dtor_registered ? "yes" : "no",
                closure_created ? "yes" : "no",
                child_released_at_pop ? "yes" : "no",
                child_destroy_count);
            (*fail)++;
        }
    }

    {
        bool setup_ok = aot::aot_init();
        Interp* aot_interp = setup_ok ? aot::aot_interp() : null;
        main::ScopeRegion* saved_parent_scope = null;
        main::ScopeRegion* parent_scope = aot_interp != null ? boundary_push_child_scope(aot_interp, &saved_parent_scope) : null;
        main::ScopeRegion* target_scope = aot_interp != null && aot_interp.root_scope != null ? main::scope_create(aot_interp.root_scope) : null;
        main::ScopeRegion* saved_child_scope = null;
        main::ScopeRegion* child_scope = null;
        int parent_destroy_count = 0;
        bool parent_dtor_registered = false;
        bool closure_created = false;
        bool copied_created = false;
        bool parent_retained_after_pop = false;
        bool copied_survives_pop = false;
        bool parent_released_at_target_teardown = false;
        bool not_released_twice = false;
        if (parent_scope != null) {
            parent_dtor_registered = main::scope_register_dtor(parent_scope, (void*)&parent_destroy_count, fn void(void* ptr) {
                int* counter = (int*)ptr;
                if (counter != null) (*counter)++;
            });
            Value* captured = aot::cons(aot::make_string("parent-captured"), aot::make_nil());
            if (target_scope != null) {
                child_scope = boundary_push_child_scope(aot_interp, &saved_child_scope);
            }
            if (target_scope != null && child_scope != null) {
                aot::AotCaptureRetention* retention = aot::capture_retention_begin();
                Value* retain_error = retention != null ? aot::capture_retention_retain_value(retention, captured) : aot::make_error("test: missing capture retention");
                AotCaptureScopeTestData* payload = (AotCaptureScopeTestData*)mem::malloc(AotCaptureScopeTestData.sizeof);
                if (payload != null) payload.captured = captured;
                Value* closure = retain_error == null && payload != null
                    ? aot::make_generated_closure_with_retention((void*)payload, &compiler_aot_test_return_captured_value, retention)
                    : null;
                if (retain_error != null && retention != null) {
                    aot::capture_retention_release(retention);
                }
                if ((closure == null || closure.tag == ValueTag.ERROR) && payload != null) {
                    mem::free(payload);
                }
                closure_created = closure != null && closure.tag == ValueTag.PRIMITIVE;
                BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
                aot_interp.current_scope = target_scope;
                aot_interp.releasing_scope = child_scope;
                Value* copied = closure_created ? copy_to_parent_with_fault(closure, aot_interp, &copy_fault) : null;
                aot_interp.releasing_scope = null;
                copied_created = copied != null && copied.tag == ValueTag.PRIMITIVE && copy_fault == BOUNDARY_COPY_FAULT_NONE;
                boundary_pop_child_scope(aot_interp, saved_child_scope, child_scope);
                child_scope = null;
                boundary_pop_child_scope(aot_interp, saved_parent_scope, parent_scope);
                parent_scope = null;
                parent_retained_after_pop = parent_dtor_registered && copied_created && parent_destroy_count == 0;
                if (parent_retained_after_pop) {
                    Value* result = aot::invoke(copied, aot::make_nil());
                    copied_survives_pop =
                        result != null &&
                        result.tag == ValueTag.CONS &&
                        result.cons_val.car != null &&
                        result.cons_val.car.tag == ValueTag.STRING &&
                        compiler_str_eq(result.cons_val.car.str_chars[:result.cons_val.car.str_len], "parent-captured");
                }
                main::scope_release(target_scope);
                target_scope = null;
                parent_released_at_target_teardown = parent_destroy_count == 1;
            }
        }
        if (child_scope != null && aot_interp != null) {
            boundary_pop_child_scope(aot_interp, saved_child_scope, child_scope);
        }
        if (target_scope != null && aot_interp != null) {
            main::scope_release(target_scope);
        }
        if (parent_scope != null && aot_interp != null) {
            boundary_pop_child_scope(aot_interp, saved_parent_scope, parent_scope);
        }
        if (setup_ok) {
            aot::aot_shutdown();
            not_released_twice = parent_destroy_count == 1;
        }

        bool ok = setup_ok &&
            parent_dtor_registered &&
            closure_created &&
            copied_created &&
            parent_retained_after_pop &&
            copied_survives_pop &&
            parent_released_at_target_teardown &&
            not_released_twice;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copied AOT closure capture releases ancestor scope at copied teardown");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: copied AOT closure capture releases ancestor scope at copied teardown (setup=%s dtor=%s closure=%s copied=%s retained=%s survives=%s released=%s no_double=%s count=%d)",
                setup_ok ? "yes" : "no",
                parent_dtor_registered ? "yes" : "no",
                closure_created ? "yes" : "no",
                copied_created ? "yes" : "no",
                parent_retained_after_pop ? "yes" : "no",
                copied_survives_pop ? "yes" : "no",
                parent_released_at_target_teardown ? "yes" : "no",
                not_released_twice ? "yes" : "no",
                parent_destroy_count);
            (*fail)++;
        }
    }

    {
        bool setup_ok = aot::aot_init();
        Interp* aot_interp = setup_ok ? aot::aot_interp() : null;
        main::ScopeRegion* saved_scope = null;
        main::ScopeRegion* child_scope = aot_interp != null ? boundary_push_child_scope(aot_interp, &saved_scope) : null;
        Value* init_error = null;
        aot::AotMutableCell* cell = null;
        bool init_promoted = false;
        bool init_survives_release = false;
        if (child_scope != null) {
            Value* initial = aot::cons(aot::make_string("initial"), aot::make_nil());
            cell = aot::make_mutable_cell_checked(initial, &init_error);
            Value* stored = cell != null ? aot::mutable_cell_get(cell) : null;
            init_promoted =
                init_error == null &&
                stored != null &&
                stored.tag == ValueTag.CONS &&
                boundary_ptr_in_scope((void*)stored, aot_interp.root_scope) &&
                !boundary_ptr_in_scope((void*)stored, child_scope) &&
                stored.cons_val.car != null &&
                stored.cons_val.car.tag == ValueTag.STRING &&
                !boundary_ptr_in_scope((void*)stored.cons_val.car, child_scope) &&
                !boundary_ptr_in_scope((void*)stored.cons_val.car.str_chars, child_scope);
            boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
            child_scope = null;
            if (init_promoted) {
                stored = cell != null ? aot::mutable_cell_get(cell) : null;
                init_survives_release =
                    stored != null &&
                    stored.tag == ValueTag.CONS &&
                    stored.cons_val.car != null &&
                    stored.cons_val.car.tag == ValueTag.STRING &&
                    compiler_str_eq(stored.cons_val.car.str_chars[:stored.cons_val.car.str_len], "initial");
            }
        }
        if (child_scope != null && aot_interp != null) {
            boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
        }
        if (setup_ok) aot::aot_shutdown();

        bool ok = cell != null && init_promoted && init_survives_release;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: AOT mutable cell init promotes child value to root");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: AOT mutable cell init promotes child value to root (setup=%s cell=%s promoted=%s survives=%s)",
                setup_ok ? "yes" : "no",
                cell != null ? "yes" : "no",
                init_promoted ? "yes" : "no",
                init_survives_release ? "yes" : "no");
            (*fail)++;
        }
    }

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
        Value* ctor_err = setup_ok ? prim_ctor_iterator(args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* next_err = setup_ok ? prim_next(args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

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
