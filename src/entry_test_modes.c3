module main;

import std::io;

fn bool test_suite_is_all(char* suite_name) @inline {
    return suite_name == null || str_eq(suite_name, "all");
}

fn bool test_suite_matches(char* suite_name, char[] expected) @inline {
    return suite_name != null && str_eq(suite_name, expected);
}

fn bool test_suite_is_valid(char* suite_name) @inline {
    return test_suite_is_all(suite_name) ||
        test_suite_matches(suite_name, "stack") ||
        test_suite_matches(suite_name, "scope") ||
        test_suite_matches(suite_name, "lisp");
}

fn int run_test_mode_for_suite(char* suite_name) {
    if (!test_suite_is_valid(suite_name)) {
        io::printfn("Unknown test suite '%s'. Expected one of: all, stack, scope, lisp.",
            suite_name);
        return 2;
    }

    io::printfn("Runtime memory architecture initialized");

    // Run stack engine tests (mmap stacks, context switch, coroutine lifecycle).
    // The stack engine test runner handles ASAN-specific overflow-test skipping.
    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "stack")) {
        run_stack_engine_tests();
    }

    // Run scope region tests (bump allocator, RC, freelist recycling)
    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "scope")) {
        run_scope_region_tests();
    }

    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "lisp")) {
        run_lisp_test_suite_with_registry();
    }
    return 0;
}

fn int run_test_mode() {
    return run_test_mode_for_suite(null);
}

fn int run_test_mode_with_self_for_suite(char* self_exe, char* suite_name) {
    int rc = run_test_mode_for_suite(suite_name);
    if (rc != 0) return rc;

    if (!test_suite_is_all(suite_name)) return 0;

    if (c_getenv("OMNI_STACK_AFFINITY_HARNESS") == null) return 0;

    io::printn("");
    io::printn("=== Stack Affinity Harness ===");
    bool ok = run_stack_affinity_harness(self_exe);
    if (ok) {
        io::printn("PASS: stack affinity misuse probe failed fast as expected");
        if (c_getenv("OMNI_TEST_SUMMARY") != null) {
            io::printfn("OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0");
        }
        return 0;
    }

    io::printn("FAIL: stack affinity misuse probe did not show expected fail-fast signal");
    if (c_getenv("OMNI_TEST_SUMMARY") != null) {
        io::printfn("OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=0 fail=1");
    }
    return 1;
}

fn int run_test_mode_with_self(char* self_exe) {
    return run_test_mode_with_self_for_suite(self_exe, null);
}
