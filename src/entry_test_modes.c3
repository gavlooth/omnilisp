module main;

import std::io;
import libc;
import lisp;

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

fn char* find_test_suite_arg(int argc, char** argv, int test_idx) {
    for (int i = test_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        return argv[i];
    }
    return null;
}

fn char[] test_suite_display_name(char* suite_name) {
    if (suite_name == null) return "all";
    usz len = 0;
    if (!cli_cstr_len(suite_name, &len)) return "<overlong>";
    return suite_name[:len];
}

fn void print_test_suite_invalid_json(char* suite_name) {
    io::print("{\"ok\":false,\"code\":\"cli/invalid-test-suite\",\"severity\":\"error\",\"message\":\"Unknown test suite\",\"suite\":");
    print_json_escaped_string(test_suite_display_name(suite_name));
    io::print(",\"expected\":[\"all\",\"stack\",\"scope\",\"lisp\"]}\n");
}

fn void print_test_suite_unexpected_arg_json(char* arg) {
    io::print("{\"ok\":false,\"code\":\"cli/usage\",\"severity\":\"error\",\"message\":");
    char[256] msg_buf;
    if (try msg = io::bprintf(&msg_buf, "unexpected argument after --test-suite: %s", (ZString)arg)) {
        print_json_escaped_string(msg);
    } else {
        print_json_escaped_string("unexpected argument after --test-suite");
    }
    io::print("}\n");
}

fn void print_test_suite_json_entry(char[] suite_name, int pass, int fail, int code) {
    io::print("{\"name\":");
    print_json_escaped_string(suite_name);
    io::print(",\"pass\":");
    io::printf("%d", pass);
    io::print(",\"fail\":");
    io::printf("%d", fail);
    io::print(",\"code\":");
    io::printf("%d", code);
    io::print(",\"ok\":");
    io::print(fail == 0 && code == 0 ? "true" : "false");
    io::print("}");
}

fn int run_test_mode_for_suite(char* suite_name) {
    if (!test_suite_is_valid(suite_name)) {
        io::printfn("Unknown test suite '%s'. Expected one of: all, stack, scope, lisp.",
            (ZString)suite_name);
        return 2;
    }

    if (!lisp::test_quiet_requested()) {
        io::printfn("Runtime memory architecture initialized");
    }

    // Run stack engine tests (mmap stacks, context switch, coroutine lifecycle).
    // The stack engine test runner handles ASAN-specific overflow-test skipping.
    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "stack")) {
        run_stack_engine_tests(null, null);
    }

    // Run scope region tests (bump allocator, RC, freelist recycling)
    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "scope")) {
        run_scope_region_tests(null, null);
    }

    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "lisp")) {
        int lisp_rc = run_lisp_test_suite_with_registry(null, null);
        if (lisp_rc != 0) return lisp_rc;
    }
    return 0;
}

fn int run_test_mode_for_suite_json(char* suite_name, char* self_exe) {
    if (!test_suite_is_valid(suite_name)) {
        print_test_suite_invalid_json(suite_name);
        return 2;
    }

    char* summary_env = c_getenv("OMNI_TEST_SUMMARY");
    if (summary_env != null) (void)libc::unsetenv("OMNI_TEST_SUMMARY");
    (void)libc::setenv("OMNI_TEST_QUIET", "1", 1);

    bool include_affinity = test_suite_is_all(suite_name) &&
        c_getenv("OMNI_STACK_AFFINITY_HARNESS") != null;

    int stack_pass = 0;
    int stack_fail = 0;
    int scope_pass = 0;
    int scope_fail = 0;
    int lisp_pass = 0;
    int lisp_fail = 0;
    int suite_count = 0;
    int suite_fail_total = 0;

    int stack_code = 0;
    int scope_code = 0;
    int lisp_code = 0;
    int affinity_code = 0;
    int affinity_pass = 0;
    int affinity_fail = 0;

    io::print("{\"requested_suite\":");
    print_json_escaped_string(test_suite_display_name(suite_name));
    io::print(",\"suites\":[");

    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "stack")) {
        run_stack_engine_tests(&stack_pass, &stack_fail);
        if (suite_count > 0) io::print(",");
        stack_code = stack_fail == 0 ? 0 : 1;
        suite_fail_total += stack_fail;
        suite_count++;
        print_test_suite_json_entry("stack", stack_pass, stack_fail, stack_code);
    }

    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "scope")) {
        run_scope_region_tests(&scope_pass, &scope_fail);
        if (suite_count > 0) io::print(",");
        scope_code = scope_fail == 0 ? 0 : 1;
        suite_fail_total += scope_fail;
        suite_count++;
        print_test_suite_json_entry("scope", scope_pass, scope_fail, scope_code);
    }

    if (test_suite_is_all(suite_name) || test_suite_matches(suite_name, "lisp")) {
        if (suite_count > 0) io::print(",");
        lisp_code = run_lisp_test_suite_with_registry(&lisp_pass, &lisp_fail);
        if (lisp_code != 0 && lisp_fail == 0) suite_fail_total++;
        suite_fail_total += lisp_fail;
        suite_count++;
        print_test_suite_json_entry("lisp", lisp_pass, lisp_fail, lisp_code);
    }

    if (include_affinity) {
        if (suite_count > 0) io::print(",");
        bool affinity_ok = run_stack_affinity_harness(self_exe);
        affinity_pass = affinity_ok ? 1 : 0;
        affinity_fail = affinity_ok ? 0 : 1;
        affinity_code = affinity_ok ? 0 : 1;
        suite_fail_total += affinity_fail;
        suite_count++;
        print_test_suite_json_entry("stack_affinity_harness", affinity_pass, affinity_fail, affinity_code);
    }

    bool all_ok = suite_fail_total == 0 &&
        stack_code == 0 && scope_code == 0 && lisp_code == 0 && affinity_code == 0;

    io::print("],\"ok\":");
    io::print(all_ok ? "true" : "false");
    io::print(",\"totals\":{\"suites\":");
    io::printf("%d", suite_count);
    io::print(",\"pass\":");
    io::printf("%d", stack_pass + scope_pass + lisp_pass + affinity_pass);
    io::print(",\"fail\":");
    io::printf("%d", suite_fail_total);
    io::print(",\"ok\":");
    io::print(all_ok ? "true" : "false");
    io::print(",\"code\":");
    io::printf("%d", all_ok ? 0 : 1);
    io::print("}}\n");

    return all_ok ? 0 : 1;
}

fn int run_test_mode() {
    return run_test_mode_for_suite(null);
}

fn int run_test_mode_with_self_for_suite(char* self_exe, char* suite_name, bool json) {
    if (suite_name != null) {
        usz suite_name_len = 0;
        if (!cli_cstr_len(suite_name, &suite_name_len)) {
            return print_cli_cstr_too_long("--test-suite name", json);
        }
    }

    if (json) {
        return run_test_mode_for_suite_json(suite_name, self_exe);
    }

    int rc = run_test_mode_for_suite(suite_name);
    if (rc != 0) return rc;

    if (!test_suite_is_all(suite_name)) return 0;

    if (c_getenv("OMNI_STACK_AFFINITY_HARNESS") == null) return 0;

    if (!lisp::test_quiet_requested()) {
        io::printn("");
        io::printn("=== Stack Affinity Harness ===");
    }
    bool ok = run_stack_affinity_harness(self_exe);
    if (ok) {
        if (!lisp::test_quiet_requested()) {
            io::printn("PASS: stack affinity misuse probe failed fast as expected");
        }
        if (c_getenv("OMNI_TEST_SUMMARY") != null) {
            io::printfn("OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0");
        }
        return 0;
    }

    if (!lisp::test_quiet_requested()) {
        io::printn("FAIL: stack affinity misuse probe did not show expected fail-fast signal");
    }
    if (c_getenv("OMNI_TEST_SUMMARY") != null) {
        io::printfn("OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=0 fail=1");
    }
    return 1;
}

fn int run_test_mode_with_self(char* self_exe) {
    return run_test_mode_with_self_for_suite(self_exe, null, false);
}
