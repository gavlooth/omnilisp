module lisp;

import std::io;
import std::core::mem;

fn void run_advanced_ffi_system_surface_tests(Interp* interp, int* pass, int* fail) {
    // === FFI TESTS (define [ffi lib] / define [ffi λ]) ===
    // Library declaration
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    test_tag(interp, "ffi lib handle", "libc", FFI_HANDLE, pass, fail);
    test_truthy(interp, "foreign-describe uses array capabilities metadata",
        "(let (m (foreign-describe libc)) "
        "  (and (= (ref m 'type) 'ForeignHandle) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'library) "
        "       (= (ref m 'name) \"libc.so.6\") "
        "       (array? (ref m 'capabilities))"
        "       (= (length (ref m 'capabilities)) 3)"
        "       (= (ref (ref m 'capabilities) 0) 'load)"
        "       (= (ref (ref m 'capabilities) 1) 'resolve)"
        "       (= (ref (ref m 'capabilities) 2) 'reflect)))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects non-releasable library handle",
        "(foreign-release libc)",
        "handle is not releasable",
        pass, fail);
    {
        bool saved_force = g_env_force_barrier_promote_failure;
        g_env_force_barrier_promote_failure = true;
        test_error_contains(interp, "ffi lib registration failure releases constructed wrapper",
            "(define [ffi lib] ffi-lib-fail \"libc.so.6\")",
            "ffi lib: out of memory",
            pass, fail);
        g_env_force_barrier_promote_failure = saved_force;
    }
    setup(interp, "(define [ffi lib] libc-recover \"libc.so.6\")");
    test_tag(interp, "ffi lib registration still works after failed wrapper release",
        "libc-recover", FFI_HANDLE, pass, fail);

    // Function bindings — integer functions
    setup(interp, "(define [ffi λ libc] (strlen (^String s)) ^Integer)");
    test_eq(interp, "ffi λ strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi λ strlen empty", "(strlen \"\")", 0, pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata after call",
        "(block "
        "(strlen \"hello\") "
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect))))",
        pass, fail);

    setup(interp,
        "(define [ffi module] libc-grouped \"libc.so.6\" "
        "  (strlen (^String s)) ^Integer "
        "  (abs (^Integer n)) ^Integer "
        "  (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose} "
        "  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)");
    test_eq(interp, "ffi module grouped strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi module grouped abs", "(abs -7)", 7, pass, fail);
    test_error_contains(interp, "ffi module grouped fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle",
        pass, fail);
    test_truthy(interp, "ffi module grouped foreign-describe callable metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "ffi module grouped ForeignHandle return metadata",
        "(let (r (ref (foreign-describe fopen) 'returns)) "
        "  (and (= (ref r 'type) 'ForeignHandle) "
        "       (= (ref r 'name) 'File) "
        "       (= (ref r 'ownership) 'owned) "
        "       (= (ref r 'finalizer) 'fclose)))",
        pass, fail);

    setup(interp, "(define [ffi λ libc] (abs (^Integer n)) ^Integer)");
    test_eq(interp, "ffi λ abs", "(abs -42)", 42, pass, fail);
    test_eq(interp, "ffi λ abs zero", "(abs 0)", 0, pass, fail);

    setup(interp, "(define [ffi λ libc] (atoi (^String s)) ^Integer)");
    test_eq(interp, "ffi λ atoi", "(atoi \"12345\")", 12345, pass, fail);
    test_eq(interp, "ffi λ atoi neg", "(atoi \"-99\")", -99, pass, fail);

    // Zero-arg function
    setup(interp, "(define [ffi λ libc] (getpid) ^Integer)");
    test_gt(interp, "ffi λ getpid", "(getpid)", 0, pass, fail);

    // Math library — Float64 functions
    setup(interp, "(define [ffi lib] libm \"libm.so.6\")");
    setup(interp, "(define [ffi λ libm] (sqrt (^Float64 x)) ^Float64)");
    test_eq_double(interp, "ffi λ sqrt", "(sqrt 4.0)", 2.0, pass, fail);
    test_eq_double(interp, "ffi λ sqrt 9", "(sqrt 9.0)", 3.0, pass, fail);

    setup(interp, "(define [ffi λ libm] (pow (^Float64 x) (^Float64 y)) ^Float64)");
    test_eq_double(interp, "ffi λ pow", "(pow 2.0 10.0)", 1024.0, pass, fail);

    // Mixed int/Float64 (libffi enables this — old FFI couldn't)
    setup(interp, "(define [ffi λ libm] (ldexp (^Float64 x) (^Integer exp)) ^Float64)");
    test_eq_double(interp, "ffi λ ldexp", "(ldexp 1.0 10)", 1024.0, pass, fail);

    setup(interp, "(define [ffi λ libc] (strtol (^String s) (^ForeignHandle endptr) (^Integer base)) ^Integer)");
    test_eq(interp, "ffi λ strtol mixed scalar foreign handle", "(strtol \"12345\" nil 10)", 12345, pass, fail);

    setup(interp, "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^String)");
    test_str_val(interp, "ffi λ string return copies C string", "(strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi λ string return null maps to nil", "(strchr \"hello\" 120)", pass, fail);

    // Void return (canonical ForeignHandle annotation)
    setup(interp, "(define [ffi λ libc] (free (^ForeignHandle p)))");
    test_truthy(interp, "ffi λ free null (^ForeignHandle)", "(= (type-of (free nil)) 'Void)", pass, fail);
    test_error_contains(interp, "ffi λ free rejects integer address (^ForeignHandle)",
        "(free 0)",
        "argument 1 expected ForeignHandle, got Integer", pass, fail);

    // Error cases
    test_error_contains(interp, "ffi λ strlen rejects non-string arg",
        "(strlen 123)",
        "argument 1 expected String, got Integer", pass, fail);
    test_error_contains(interp, "ffi λ abs rejects boolean arg",
        "(abs false)",
        "argument 1 expected Integer, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ sqrt rejects boolean arg",
        "(sqrt false)",
        "argument 1 expected Float64, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ free rejects string arg",
        "(free \"hello\")",
        "argument 1 expected ForeignHandle, got String", pass, fail);
    test_error_contains(interp, "ffi λ exact arity rejects extra args",
        "(getpid 1)",
        "arity mismatch: expected 0 argument(s), got 1", pass, fail);
    test_error_contains(interp, "ffi lib bad",
        "(define [ffi lib] bad-lib \"nonexistent_xyz.so\")",
        "dlopen failed for", pass, fail);
    test_error_contains(interp, "ffi lib bad preserves path cause",
        "(define [ffi lib] bad-lib2 \"nonexistent_xyz.so\")",
        "nonexistent_xyz.so", pass, fail);
    setup(interp, "(define [ffi λ libc] (definitely_missing_ffi_symbol (^Integer n)) ^Integer)");
    test_error_contains(interp, "ffi λ missing symbol cause",
        "(definitely_missing_ffi_symbol 1)",
        "dlsym failed for", pass, fail);
    test_error_contains(interp, "ffi λ missing symbol preserves loader cause",
        "(definitely_missing_ffi_symbol 1)",
        "undefined symbol", pass, fail);
    test_error_contains(interp, "ffi λ unsupported parameter annotation",
        "(block (define [ffi lib] bad-ffi-lib \"libc.so.6\") (define [ffi λ bad-ffi-lib] (strlen (^Symbol s)) ^Integer))",
        "unsupported parameter type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ legacy Pointer annotation is rejected",
        "(block (define [ffi lib] bad-ffi-lib3 \"libc.so.6\") (define [ffi λ bad-ffi-lib3] (free (^Pointer p))))",
        "unsupported parameter type annotation '^Pointer'", pass, fail);
    test_error_contains(interp, "ffi λ unsupported return annotation",
        "(block (define [ffi lib] bad-ffi-lib2 \"libc.so.6\") (define [ffi λ bad-ffi-lib2] (strlen (^String s)) ^Symbol))",
        "unsupported return type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ variadic declarations are rejected",
        "(block (define [ffi lib] libc-var \"libc.so.6\") (define [ffi λ libc-var variadic] (printf (^String fmt)) ^Integer))",
        "variadic bindings are not supported yet", pass, fail);
    test_error_contains(interp, "ffi λ Boolean requires real booleans",
        "(block (define [ffi λ libc] (abs (^Boolean flag)) ^Integer) (abs 1))",
        "argument 1 expected Boolean, got Integer", pass, fail);
    test_error_contains(interp, "ffi module grouped rejects missing return annotation",
        "(define [ffi module] bad-grouped \"libc.so.6\" (strlen (^String s)))",
        "expected return type annotation after [ffi module] function signature", pass, fail);

    // === SYSTEM PRIMITIVES TESTS ===
    // random — returns Float64 in [0,1)
    test_truthy(interp, "random range", "(let (r (random)) (and (>= r 0.0) (< r 1.0)))", pass, fail);
    test_truthy(interp, "random is Float64", "(float64? (random))", pass, fail);
    // random-int — returns int in [0,n)
    test_truthy(interp, "random-int range", "(let (r (random-int 10)) (and (>= r 0) (< r 10)))", pass, fail);
    test_truthy(interp, "random-int is int", "(int? (random-int 100))", pass, fail);
    // time — returns epoch seconds (should be > 2020-01-01)
    test_gt(interp, "time epoch", "(time)", 1577836800, pass, fail);
    // time-ms — returns epoch milliseconds (> time * 1000)
    test_gt(interp, "time-ms epoch", "(time-ms)", 1577836800000, pass, fail);
    // getenv — HOME should exist, nonexistent returns nil
    test_str(interp, "getenv HOME", "(getenv \"HOME\")", pass, fail);
    test_nil(interp, "getenv missing", "(getenv \"OMNI_NONEXISTENT_VAR_XYZ\")", pass, fail);
    // shell — captures stdout
    test_str_val(interp, "shell echo", "(shell \"echo hello\")", "hello", pass, fail);
    // sleep — returns void (tiny sleep to not slow tests)
    test_tag_interp(interp, "sleep tiny", "(sleep 0.001)", VOID, pass, fail);

    run_advanced_uv_timer_callback_register_invoke_test(interp, pass, fail);
    run_advanced_uv_timer_callback_unhandle_idempotence_test(interp, pass, fail);
    run_advanced_uv_timer_callback_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_handle_isolation_test(interp, pass, fail);
}
