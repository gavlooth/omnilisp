module lisp;

import std::io;
import std::core::mem;
import main;
import lisp::aot;

fn void run_memory_lifetime_runtime_alloc_groups_apply_coroutine_user_data_tests(Interp* interp, int* pass, int* fail) {
    {
        bool setup_ok = aot::aot_init();
        Value* init_error = null;
        aot::AotMutableCell* cell = setup_ok ? aot::make_mutable_cell_checked(aot::make_nil(), &init_error) : null;
        Value* closure = cell != null
            ? aot::make_closure((void*)cell, &compiler_aot_test_store_fresh_cons_in_cell)
            : null;
        Interp* aot_interp = setup_ok ? aot::aot_interp() : null;
        main::ScopeRegion* saved_scope = null;
        main::ScopeRegion* child_scope = aot_interp != null ? boundary_push_child_scope(aot_interp, &saved_scope) : null;
        bool stored_promoted = false;
        bool stored_survives_release = false;
        if (child_scope != null && closure != null && closure.tag == PRIMITIVE) {
            Value* result = aot::invoke(closure, aot::make_nil());
            Value* stored = aot::mutable_cell_get(cell);
            stored_promoted =
                result != null &&
                result.tag == ValueTag.CONS &&
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
            if (stored_promoted) {
                stored = aot::mutable_cell_get(cell);
                stored_survives_release =
                    stored != null &&
                    stored.tag == ValueTag.CONS &&
                    stored.cons_val.car != null &&
                    stored.cons_val.car.tag == ValueTag.STRING &&
                    compiler_str_eq(stored.cons_val.car.str_chars[:stored.cons_val.car.str_len], "fresh");
            }
        }
        if (child_scope != null && aot_interp != null) {
            boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
        }
        if (setup_ok) aot::aot_shutdown();

        bool ok = cell != null && init_error == null && stored_promoted && stored_survives_release;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: AOT mutable cell set promotes child value to root");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: AOT mutable cell set promotes child value to root (setup=%s cell=%s promoted=%s survives=%s)",
                setup_ok ? "yes" : "no",
                cell != null ? "yes" : "no",
                stored_promoted ? "yes" : "no",
                stored_survives_release ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_target_scope = null;
        main::ScopeRegion* target_scope = boundary_push_child_scope(interp, &saved_target_scope);
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = target_scope != null ? boundary_push_child_scope(interp, &saved_source_scope) : null;
        int copy_count = 0;
        int finalizer_count = 0;
        bool primitive_created = false;
        bool copied_created = false;
        bool source_released = false;
        bool copied_works = false;
        bool copied_released = false;

        PrimitiveUserDataHookTestPayload* payload = source_scope != null
            ? (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof)
            : null;
        if (payload != null) {
            payload.marker = 246;
            payload.copy_count = &copy_count;
            payload.finalizer_count = &finalizer_count;
        }
        Value* prim = payload != null
            ? make_primitive_with_user_data_in_scope(
                interp,
                source_scope,
                "hook-test",
                &primitive_user_data_hook_apply,
                1,
                (void*)payload,
                &primitive_user_data_hook_copy,
                &primitive_user_data_hook_finalizer)
            : null;
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE;

        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = null;
        if (primitive_created && target_scope != null && source_scope != null) {
            interp.current_scope = target_scope;
            interp.releasing_scope = source_scope;
            copied = copy_to_parent_with_fault(prim, interp, &copy_fault);
            interp.releasing_scope = null;
        }
        copied_created = copied != null && copied.tag == ValueTag.PRIMITIVE && copy_fault == BOUNDARY_COPY_FAULT_NONE && copy_count == 1;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        source_released = finalizer_count == 1;
        if (copied_created) {
            EvalResult applied = apply_primitive(copied, make_nil(interp), interp);
            copied_works =
                !applied.error.has_error &&
                applied.value != null &&
                applied.value.tag == ValueTag.INT &&
                applied.value.int_val == 246;
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
            target_scope = null;
        }
        copied_released = finalizer_count == 2;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
        }

        bool ok = primitive_created &&
            copied_created &&
            source_released &&
            copied_works &&
            copied_released;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive user_data hooks copy and finalize across parent boundary");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive user_data hooks copy and finalize across parent boundary (primitive=%s copied=%s source_released=%s works=%s copied_released=%s copies=%d finalizers=%d)",
                primitive_created ? "yes" : "no",
                copied_created ? "yes" : "no",
                source_released ? "yes" : "no",
                copied_works ? "yes" : "no",
                copied_released ? "yes" : "no",
                copy_count,
                finalizer_count);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_target_scope = null;
        main::ScopeRegion* target_scope = boundary_push_child_scope(interp, &saved_target_scope);
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = target_scope != null ? boundary_push_child_scope(interp, &saved_source_scope) : null;
        int copy_count = 0;
        int finalizer_count = 0;
        bool primitive_created = false;
        bool copy_failed_before_hook = false;
        bool source_released = false;

        PrimitiveUserDataHookTestPayload* payload = source_scope != null
            ? (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof)
            : null;
        if (payload != null) {
            payload.marker = 247;
            payload.copy_count = &copy_count;
            payload.finalizer_count = &finalizer_count;
        }
        Value* prim = payload != null
            ? make_primitive_with_user_data_in_scope(
                interp,
                source_scope,
                "hook-test-wrapper-oom",
                &primitive_user_data_hook_apply,
                1,
                (void*)payload,
                &primitive_user_data_hook_copy,
                &primitive_user_data_hook_finalizer)
            : null;
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE;

        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = null;
        bool saved_force = g_boundary_force_copy_primitive_wrapper_alloc_fail;
        if (primitive_created && target_scope != null && source_scope != null) {
            interp.current_scope = target_scope;
            interp.releasing_scope = source_scope;
            g_boundary_force_copy_primitive_wrapper_alloc_fail = true;
            copied = copy_to_parent_with_fault(prim, interp, &copy_fault);
            g_boundary_force_copy_primitive_wrapper_alloc_fail = saved_force;
            interp.releasing_scope = null;
        } else {
            g_boundary_force_copy_primitive_wrapper_alloc_fail = saved_force;
        }
        copy_failed_before_hook =
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            copy_count == 0;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        source_released = finalizer_count == 1;
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
            target_scope = null;
        }

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
        }

        bool ok = primitive_created &&
            copy_failed_before_hook &&
            source_released &&
            finalizer_count == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive user_data copy skips hook on wrapper allocation failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive user_data copy skips hook on wrapper allocation failure (primitive=%s failed=%s source_released=%s copies=%d finalizers=%d fault=%d)",
                primitive_created ? "yes" : "no",
                copy_failed_before_hook ? "yes" : "no",
                source_released ? "yes" : "no",
                copy_count,
                finalizer_count,
                (int)copy_fault);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_target_scope = null;
        main::ScopeRegion* target_scope = boundary_push_child_scope(interp, &saved_target_scope);
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = target_scope != null ? boundary_push_child_scope(interp, &saved_source_scope) : null;
        int copy_count = 0;
        int finalizer_count = 0;
        bool primitive_created = false;
        bool copy_failed_before_hook = false;
        bool source_released = false;

        PrimitiveUserDataHookTestPayload* payload = source_scope != null
            ? (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof)
            : null;
        if (payload != null) {
            payload.marker = 248;
            payload.copy_count = &copy_count;
            payload.finalizer_count = &finalizer_count;
        }
        Value* prim = payload != null
            ? make_primitive_with_user_data_in_scope(
                interp,
                source_scope,
                "hook-test-dtor-oom",
                &primitive_user_data_hook_apply,
                1,
                (void*)payload,
                &primitive_user_data_hook_copy,
                &primitive_user_data_hook_finalizer)
            : null;
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE;

        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = null;
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        if (primitive_created && target_scope != null && source_scope != null) {
            interp.current_scope = target_scope;
            interp.releasing_scope = source_scope;
            main::g_scope_force_dtor_alloc_oom = true;
            copied = copy_to_parent_with_fault(prim, interp, &copy_fault);
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            interp.releasing_scope = null;
        } else {
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        }
        copy_failed_before_hook =
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION &&
            copy_count == 0;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        source_released = finalizer_count == 1;
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
            target_scope = null;
        }

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
        }

        bool ok = primitive_created &&
            copy_failed_before_hook &&
            source_released &&
            finalizer_count == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive user_data copy skips hook on dtor registration failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive user_data copy skips hook on dtor registration failure (primitive=%s failed=%s source_released=%s copies=%d finalizers=%d fault=%d)",
                primitive_created ? "yes" : "no",
                copy_failed_before_hook ? "yes" : "no",
                source_released ? "yes" : "no",
                copy_count,
                finalizer_count,
                (int)copy_fault);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_target_scope = null;
        main::ScopeRegion* target_scope = boundary_push_child_scope(interp, &saved_target_scope);
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = target_scope != null ? boundary_push_child_scope(interp, &saved_source_scope) : null;
        bool primitive_created = false;
        bool copy_failed_closed = false;

        Value* prim = source_scope != null
            ? boundary_alloc_value_in_scope(interp, source_scope, false)
            : null;
        if (prim != null) {
            prim.tag = ValueTag.PRIMITIVE;
            prim.prim_val = null;
        }
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE && prim.prim_val == null;

        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = null;
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        if (primitive_created && target_scope != null && source_scope != null) {
            interp.current_scope = target_scope;
            interp.releasing_scope = source_scope;
            main::g_scope_force_dtor_alloc_oom = true;
            copied = copy_to_parent_with_fault(prim, interp, &copy_fault);
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            interp.releasing_scope = null;
        } else {
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        }
        copy_failed_closed =
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
            target_scope = null;
        }

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
        }

        bool ok = primitive_created && copy_failed_closed;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: null primitive copy reports dtor registration failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: null primitive copy reports dtor registration failure (primitive=%s failed=%s fault=%d)",
                primitive_created ? "yes" : "no",
                copy_failed_closed ? "yes" : "no",
                (int)copy_fault);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_target_scope = null;
        main::ScopeRegion* target_scope = boundary_push_child_scope(interp, &saved_target_scope);
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = target_scope != null ? boundary_push_child_scope(interp, &saved_source_scope) : null;
        int copy_count = 0;
        int finalizer_count = 0;
        bool primitive_created = false;
        bool escape_failed_before_hook = false;
        bool source_released = false;

        PrimitiveUserDataHookTestPayload* payload = source_scope != null
            ? (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof)
            : null;
        if (payload != null) {
            payload.marker = 249;
            payload.copy_count = &copy_count;
            payload.finalizer_count = &finalizer_count;
        }
        Value* prim = payload != null
            ? make_primitive_with_user_data_in_scope(
                interp,
                source_scope,
                "hook-test-escape-dtor-oom",
                &primitive_user_data_hook_apply,
                1,
                (void*)payload,
                &primitive_user_data_hook_copy,
                &primitive_user_data_hook_finalizer)
            : null;
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE;

        Value* escaped = null;
        bool saved_escape_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
        if (primitive_created && target_scope != null && source_scope != null) {
            interp.current_scope = target_scope;
            interp.releasing_scope = source_scope;
            main::g_scope_force_escape_dtor_alloc_oom = true;
            escaped = promote_to_escape(prim, interp);
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
            interp.releasing_scope = null;
        } else {
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
        }
        escape_failed_before_hook =
            escaped != null &&
            escaped.tag == ValueTag.ERROR &&
            copy_count == 0;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        source_released = finalizer_count == 1;
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
            target_scope = null;
        }

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            boundary_pop_child_scope(interp, saved_target_scope, target_scope);
        }

        bool ok = primitive_created &&
            escape_failed_before_hook &&
            source_released &&
            finalizer_count == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive user_data escape skips hook on dtor registration failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive user_data escape skips hook on dtor registration failure (primitive=%s failed=%s source_released=%s copies=%d finalizers=%d)",
                primitive_created ? "yes" : "no",
                escape_failed_before_hook ? "yes" : "no",
                source_released ? "yes" : "no",
                copy_count,
                finalizer_count);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_source_scope = null;
        main::ScopeRegion* source_scope = boundary_push_child_scope(interp, &saved_source_scope);
        main::ScopeRegion* target_scope = interp.root_scope != null ? main::scope_create(interp.root_scope) : null;
        int copy_count = 0;
        int finalizer_count = 0;
        bool primitive_created = false;
        bool root_store_failed_before_hook = false;
        bool source_released = false;

        PrimitiveUserDataHookTestPayload* payload = source_scope != null
            ? (PrimitiveUserDataHookTestPayload*)mem::malloc(PrimitiveUserDataHookTestPayload.sizeof)
            : null;
        if (payload != null) {
            payload.marker = 250;
            payload.copy_count = &copy_count;
            payload.finalizer_count = &finalizer_count;
        }
        Value* prim = payload != null
            ? make_primitive_with_user_data_in_scope(
                interp,
                source_scope,
                "hook-test-root-store-dtor-oom",
                &primitive_user_data_hook_apply,
                1,
                (void*)payload,
                &primitive_user_data_hook_copy,
                &primitive_user_data_hook_finalizer)
            : null;
        primitive_created = prim != null && prim.tag == ValueTag.PRIMITIVE;

        Value* cloned = null;
        bool saved_escape_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
        if (primitive_created && target_scope != null && source_scope != null) {
            main::g_scope_force_escape_dtor_alloc_oom = true;
            cloned = root_store_clone_primitive_to_scope(prim, interp, target_scope, source_scope);
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
        } else {
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
        }
        root_store_failed_before_hook =
            cloned != null &&
            cloned.tag == ValueTag.ERROR &&
            copy_count == 0;

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
            source_scope = null;
        }
        source_released = finalizer_count == 1;
        if (target_scope != null) {
            main::scope_release(target_scope);
            target_scope = null;
        }

        if (source_scope != null) {
            boundary_pop_child_scope(interp, saved_source_scope, source_scope);
        }
        if (target_scope != null) {
            main::scope_release(target_scope);
        }

        bool ok = primitive_created &&
            root_store_failed_before_hook &&
            source_released &&
            finalizer_count == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive user_data root-store skips hook on dtor registration failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive user_data root-store skips hook on dtor registration failure (primitive=%s failed=%s source_released=%s copies=%d finalizers=%d)",
                primitive_created ? "yes" : "no",
                root_store_failed_before_hook ? "yes" : "no",
                source_released ? "yes" : "no",
                copy_count,
                finalizer_count);
            (*fail)++;
        }
    }
}
