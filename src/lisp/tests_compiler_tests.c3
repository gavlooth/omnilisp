module lisp;


import std::io;
import main;
// =============================================================================
// SECTION 12: COMPILER TESTS
// =============================================================================

extern fn char* compiler_c_getenv(char* name) @extern("getenv");

/**
 * Check if haystack contains needle as a substring.
 */
fn bool str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn usz compiler_str_count(char[] haystack, char[] needle) {
    if (needle.len == 0) return 0;
    if (needle.len > haystack.len) return 0;

    usz count = 0;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) count++;
    }
    return count;
}

fn void compiler_print_pass(char[] msg) {
    if (test_quiet_requested()) return;
    io::printfn("[PASS] %s", (ZString)msg);
}

fn bool run_compiler_tests(Interp* _unused_interp, int* out_pass = null, int* out_fail = null) {
    if (!test_quiet_requested()) {
        io::printn("\n=== Compiler Tests ===");
    }
    usz pass = 0;
    usz fail = 0;

    // Use a fresh interp for compiler tests since each compile_to_c3 call
    // parses the entire stdlib prelude, consuming many expr pool entries.
    Interp* interp = (Interp*)mem::malloc(Interp.sizeof);
    if (interp == null) {
        io::printn("[FAIL] Compiler bootstrap allocation");
        emit_test_summary("compiler", 0, 1);
        if (out_pass != null) *out_pass = 0;
        if (out_fail != null) *out_fail = 1;
        return false;
    }
    interp.init();
    if (!bootstrap_runtime_interp(interp)) {  // Needed for map, filter, etc.
        interp.destroy();
        mem::free(interp);
        io::printn("[FAIL] Compiler bootstrap runtime surface");
        emit_test_summary("compiler", 0, 1);
        if (out_pass != null) *out_pass = 0;
        if (out_fail != null) *out_fail = 1;
        return false;
    }

    run_compiler_group_syntax_basics(interp, &pass, &fail);
    run_compiler_group_stdlib_existing(interp, &pass, &fail);
    run_compiler_group_set_path_tests(interp, &pass, &fail);
    run_compiler_group_continuation_native_tests(interp, &pass, &fail);
    run_compiler_group_multiarg_tco_tests(interp, &pass, &fail);
    run_compiler_group_qq_macro_primitive_tests(interp, &pass, &fail);
    run_compiler_group_mutable_integration_iterative_tests(interp, &pass, &fail);
    run_compiler_group_jit_print_tests(interp, &pass, &fail);

    interp.destroy();
    mem::free(interp);
    if (!test_quiet_requested()) {
        io::printfn("\n=== Compiler Tests: %d passed, %d failed ===", pass, fail);
    }
    emit_test_summary("compiler", (int)pass, (int)fail);
    if (out_pass != null) *out_pass = (int)pass;
    if (out_fail != null) *out_fail = (int)fail;
    return fail == 0;
}

// =============================================================================
// Escape-scope optimization tests
// =============================================================================
