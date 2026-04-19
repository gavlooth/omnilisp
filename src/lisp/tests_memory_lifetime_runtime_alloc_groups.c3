module lisp;

import std::io;
import std::core::mem;
import main;

fn bool runtime_alloc_error_matches(Value* value, char[] expected) @inline {
    return value != null &&
        value.tag == ERROR &&
        value.str_chars != null &&
        str_starts_with(value.str_chars[:value.str_len], expected);
}

fn ZString runtime_alloc_error_text(Value* value) @inline {
    return value != null && value.tag == ERROR && value.str_chars != null
        ? (ZString)value.str_chars[:value.str_len]
        : (ZString)"<none>";
}

fn Value* runtime_alloc_null_iterator_next(Value*[] args, Env* env, Interp* interp) {
    return null;
}

int g_runtime_alloc_ffi_finalizer_calls = 0;
int g_runtime_alloc_tensor_device_finalizer_calls = 0;
int g_runtime_alloc_tensor_cuda_free_finalizer_calls = 0;

fn void runtime_alloc_ffi_finalizer(void* payload) {
    g_runtime_alloc_ffi_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn void runtime_alloc_tensor_device_finalizer(void* payload) {
    g_runtime_alloc_tensor_device_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn void runtime_alloc_tensor_cuda_free_finalizer(void* payload) {
    g_runtime_alloc_tensor_cuda_free_finalizer_calls += 1;
    if (payload != null) {
        omni_tensor_backend_cuda_free(payload);
    }
}

fn usz runtime_alloc_active_tensor_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) {
            Value* v = (Value*)dtor.ptr;
            if (v.tag == TENSOR && v.tensor_val != null) count++;
        }
        dtor = dtor.next;
    }
    return count;
}

fn usz runtime_alloc_active_hashmap_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) {
            Value* v = (Value*)dtor.ptr;
            if ((v.tag == HASHMAP || v.tag == SET) && v.hashmap_val != null) count++;
        }
        dtor = dtor.next;
    }
    return count;
}

fn usz runtime_alloc_active_value_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) count++;
        dtor = dtor.next;
    }
    return count;
}

fn bool runtime_alloc_process_spawn_cleanup_case(
    Interp* interp,
    int* pass,
    int* fail,
    char[] label,
    int forced_fail_nth,
    int expected_stage,
    bool expect_stdin_wrapper_created,
    bool expect_stdout_wrapper_created,
    bool expect_stderr_wrapper_created,
    bool expect_stdin_cleanup_called,
    bool expect_stdout_cleanup_called,
    bool expect_stderr_cleanup_called
) {
    Value* proc_argv = make_cons(interp,
        make_string(interp, "-c"),
        make_cons(interp, make_string(interp, "sleep 0.05"), make_nil(interp)));
    Value*[3] spawn_args = {
        make_string(interp, "/bin/sh"),
        proc_argv,
        make_nil(interp)
    };

    ProcessSpawnCleanupTrace trace = {};
    int saved_force = g_value_force_make_ffi_box_alloc_fail_nth;
    g_value_force_make_ffi_box_alloc_fail_nth = forced_fail_nth;
    Value* spawn_fail = process_spawn_impl(spawn_args[0:3], null, interp, &trace);
    g_value_force_make_ffi_box_alloc_fail_nth = saved_force;

    bool ok = runtime_alloc_error_matches(spawn_fail, "fs-open: out of memory") &&
        trace.spawn_succeeded &&
        trace.process_handle_created &&
        trace.failed_stage == expected_stage &&
        trace.stdin_wrapper_created == expect_stdin_wrapper_created &&
        trace.stdout_wrapper_created == expect_stdout_wrapper_created &&
        trace.stderr_wrapper_created == expect_stderr_wrapper_created &&
        trace.stdin_cleanup_called == expect_stdin_cleanup_called &&
        trace.stdout_cleanup_called == expect_stdout_cleanup_called &&
        trace.stderr_cleanup_called == expect_stderr_cleanup_called &&
        trace.process_closed;

    if (ok) {
        io::printfn("[PASS] %s", (ZString)label);
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] %s (error=%s spawned=%s handle=%s stage=%d stdin_created=%s stdout_created=%s stderr_created=%s stdin_cleanup=%s stdout_cleanup=%s stderr_cleanup=%s process_closed=%s)",
            (ZString)label,
            runtime_alloc_error_text(spawn_fail),
            trace.spawn_succeeded ? "yes" : "no",
            trace.process_handle_created ? "yes" : "no",
            trace.failed_stage,
            trace.stdin_wrapper_created ? "yes" : "no",
            trace.stdout_wrapper_created ? "yes" : "no",
            trace.stderr_wrapper_created ? "yes" : "no",
            trace.stdin_cleanup_called ? "yes" : "no",
            trace.stdout_cleanup_called ? "yes" : "no",
            trace.stderr_cleanup_called ? "yes" : "no",
            trace.process_closed ? "yes" : "no");
        (*fail)++;
    }

    return ok;
}

fn void run_memory_lifetime_runtime_helper_alloc_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_runtime_alloc_groups_core_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_schema_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_apply_coroutine_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_array_tensor_ctor_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_tensor_promotion_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_checked_collections_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_data_parsing_tests(interp, pass, fail);
    run_memory_lifetime_runtime_alloc_groups_iterators_strings_tests(interp, pass, fail);
}
