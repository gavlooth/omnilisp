module lisp;

import std::io;
import std::core::mem;
import main;

const usz COLLECTION_MATERIALIZATION_REGRESSION_COUNT = 65537;
const usz ITERATOR_MATERIALIZATION_REGRESSION_COUNT = 4097;

fn Value* runtime_alloc_make_integer_list(Interp* interp, usz count) {
    Value* result = make_nil(interp);
    for (usz i = count; i > 0; i--) {
        Value* next = make_cons(interp, make_int(interp, (long)(i - 1)), result);
        if (next == null || next.tag == ERROR) return next;
        result = next;
    }
    return result;
}

fn Value* runtime_alloc_make_integer_array(Interp* interp, usz count) {
    Value* arr = make_array_checked(interp, count, "test: failed to allocate integer array");
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return arr;
    for (usz i = 0; i < count; i++) {
        arr.array_val.items[i] = make_int(interp, (long)i);
    }
    arr.array_val.length = count;
    return arr;
}

fn Value* runtime_alloc_list_ref(Value* list, usz index) {
    Value* cur = list;
    for (usz i = 0; i < index; i++) {
        if (!is_cons(cur)) return null;
        cur = cdr(cur);
    }
    return is_cons(cur) ? car(cur) : null;
}

fn void run_memory_lifetime_runtime_alloc_groups_array_tensor_ctor_tests(Interp* interp, int* pass, int* fail) {

    run_memory_lifetime_runtime_alloc_groups_array_tensor_constructor_tests(interp, pass, fail);

    {
        EvalResult tensor_eval = run("(Tensor Float64 [2 2] [1 2 3 4])", interp);
        bool setup_ok = !tensor_eval.error.has_error &&
            tensor_eval.value != null &&
            tensor_eval.value.tag == TENSOR;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value*[1] transpose_args = { tensor_eval.value };
        Value* transpose_fail = setup_ok ? prim_matrix_transpose_view(transpose_args[0:1], null, interp) : null;
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            runtime_alloc_error_matches(transpose_fail, "matrix/transpose-view: failed to register Tensor view destructor") &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor transpose-view dtor OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor transpose-view dtor OOM fails closed (setup=%s result=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(transpose_fail),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult guard = run("(let (cuda-backend (ref (tensor-backends) 1)) (and (ref cuda-backend 'available) (ref cuda-backend 'float64)))", interp);
        bool backend_ok = !guard.error.has_error &&
            guard.value != null &&
            is_symbol(guard.value) &&
            guard.value.sym_val == interp.sym_true;
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult tensor_eval = guard;

        main::g_scope_force_dtor_alloc_oom = true;
        if (backend_ok) {
            tensor_eval = run("(to-device (Tensor Float64 [2] [1 2]) 'cuda)", interp);
        }
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = !backend_ok ||
            (tensor_eval.error.has_error &&
                runtime_alloc_error_matches(tensor_eval.value, "to-device: failed to register CUDA tensor destructor") &&
                active_tensor_dtors_after == active_tensor_dtors_before);
        if (ok) {
            emit_pass_literal("[PASS] lifetime: CUDA tensor to-device destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: CUDA tensor to-device destructor registration OOM fails closed (backend=%s result=%s before=%d after=%d)",
                backend_ok ? "yes" : "no",
                runtime_alloc_error_text(tensor_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool backend_ok = !guard.error.has_error &&
            guard.value != null &&
            is_symbol(guard.value) &&
            guard.value.sym_val == interp.sym_true;
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult tensor_eval = guard;

        main::g_scope_force_dtor_alloc_oom = true;
        if (backend_ok) {
            tensor_eval = run("(ml/layer-normalization (to-device (Tensor Float64 [2 3] [1 2 3 2 4 6]) 'vulkan) 1)", interp);
        }
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = !backend_ok ||
            (tensor_eval.error.has_error &&
                runtime_alloc_error_matches(tensor_eval.value, "ml/layer-normalization: failed to register Vulkan result tensor destructor") &&
                active_tensor_dtors_after == active_tensor_dtors_before);
        if (ok) {
            emit_pass_literal("[PASS] lifetime: Vulkan layer-normalization destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: Vulkan layer-normalization destructor registration OOM fails closed (backend=%s result=%s before=%d after=%d)",
                backend_ok ? "yes" : "no",
                runtime_alloc_error_text(tensor_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) 3)", interp);
        EvalResult index_eval = run("[1]", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !index_eval.error.has_error &&
            index_eval.value != null;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* ref_result = setup_ok ? tensor_ref_value(expr_eval.value, index_eval.value, interp) : null;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            ref_result != null &&
            ref_result.tag == DOUBLE &&
            ref_result.double_val == 5.0 &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor ref cleans temporary realized tensor");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor ref cleans temporary realized tensor (setup=%s result=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                ref_result != null && ref_result.tag == DOUBLE ? (ZString)"double" : runtime_alloc_error_text(ref_result),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (map + (Tensor Float64 [2] [1 2]) 1) 2)", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 4.0 && data[1] == 5.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run(
            "(contract (contract (Tensor Float64 [2 2] [1 2 3 4]) (Tensor Float64 [2 2] [1 0 0 1]) [1] [0]) (Tensor Float64 [2] [10 20]) [1] [0])",
            interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 50.0 && data[1] == 110.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor contract realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor contract realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* copied = null;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(saved);
        usz active_tensor_dtors_after = active_tensor_dtors_before;

        int saved_force = g_tensor_force_expr_edge_copy_fail_nth;
        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            g_tensor_force_expr_edge_copy_fail_nth = 3;
            copied = setup_ok ? copy_to_parent_with_fault(expr_eval.value, interp, &copy_fault) : null;
            g_tensor_force_expr_edge_copy_fail_nth = saved_force;
            interp.releasing_scope = null;
            active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(saved);
        }
        g_tensor_force_expr_edge_copy_fail_nth = saved_force;
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor edge copy rollback cleans partial child copies");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor edge copy rollback cleans partial child copies (setup=%s copied=%s fault=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                copied == null ? (ZString)"null" : runtime_alloc_error_text(copied),
                (ZString)boundary_copy_fault_name(copy_fault),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* tensor = null;
        Value* copied = null;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;

        if (setup_ok) {
            usz[1] tensor_shape = { 2 };
            interp.current_scope = child;
            tensor = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
            setup_ok = tensor != null && tensor.tag == TENSOR;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            copied = setup_ok ? copy_to_parent_with_fault(tensor, interp, &copy_fault) : null;
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            interp.releasing_scope = null;
        }
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor parent-copy dtor OOM reports dtor fault");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor parent-copy dtor OOM reports dtor fault (setup=%s copied=%s fault=%s)",
                setup_ok ? "yes" : "no",
                copied == null ? (ZString)"null" : runtime_alloc_error_text(copied),
                (ZString)boundary_copy_fault_name(copy_fault));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* copied = null;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(saved);
        usz active_tensor_dtors_after = active_tensor_dtors_before;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            copied = setup_ok ? copy_to_parent_with_fault(expr_eval.value, interp, &copy_fault) : null;
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            interp.releasing_scope = null;
            active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(saved);
        }
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor parent-copy dtor OOM cleans copied edges");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor parent-copy dtor OOM cleans copied edges (setup=%s copied=%s fault=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                copied == null ? (ZString)"null" : runtime_alloc_error_text(copied),
                (ZString)boundary_copy_fault_name(copy_fault),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        bool setup_ok = child != null && target != null;
        Value* tensor = null;
        Value* cloned = null;

        if (setup_ok) {
            usz[1] tensor_shape = { 2 };
            interp.current_scope = child;
            tensor = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
            setup_ok = tensor != null && tensor.tag == TENSOR;

            interp.current_scope = saved;
            bool saved_escape_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
            main::g_scope_force_escape_dtor_alloc_oom = true;
            cloned = setup_ok ? root_store_clone_tensor_to_scope(tensor, interp, target, child) : null;
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
        }
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(cloned, "root-store: failed to register tensor clone destructor");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor root-store dtor OOM preserves dtor error");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor root-store dtor OOM preserves dtor error (setup=%s result=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(cloned));
            (*fail)++;
        }

        if (target != null) main::scope_release(target);
        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        bool map_left_in_child = true;
        bool map_right_in_child = true;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;
            promoted = setup_ok ? promote_to_root(expr_eval.value, interp) : null;
            if (promoted != null &&
                promoted.tag == TENSOR &&
                promoted.tensor_val != null &&
                promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP) {
                map_left_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_left, child);
                map_right_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_right, child);
            }
            interp.current_scope = saved;
            interp.releasing_scope = null;
        }

        bool ok = setup_ok &&
            promoted != null &&
            promoted.tag == TENSOR &&
            promoted.scope_gen == saved.escape_generation &&
            promoted.tensor_val != null &&
            promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !map_left_in_child &&
            !map_right_in_child;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor root promotion keeps expression edges out of child scope");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor root promotion keeps expression edges out of child scope (setup=%s promoted=%s left_child=%s right_child=%s)",
                setup_ok ? "yes" : "no",
                promoted != null && promoted.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(promoted),
                map_left_in_child ? "yes" : "no",
                map_right_in_child ? "yes" : "no");
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }
}
