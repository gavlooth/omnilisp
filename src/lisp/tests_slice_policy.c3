module lisp;

import std::io;
import main;

extern fn char* test_c_getenv(char* name) @extern("getenv");

bool g_test_quiet_output = false;

fn bool test_summary_enabled() @inline {
    return test_c_getenv("OMNI_TEST_SUMMARY") != null;
}

fn bool test_quiet_requested() @inline {
    return test_c_getenv("OMNI_TEST_QUIET") != null ||
        (test_summary_enabled() && test_c_getenv("OMNI_TEST_VERBOSE") == null);
}

fn bool test_quiet_enabled() @inline {
    return g_test_quiet_output;
}

fn bool lisp_slice_matches(char* active_slice, char[] expected) @inline {
    return active_slice != null && main::str_eq(active_slice, expected);
}

fn bool lisp_slice_any(char* active_slice) @inline {
    return active_slice == null || lisp_slice_matches(active_slice, "all");
}

fn bool lisp_slice_known(char* active_slice) @inline {
    return lisp_slice_any(active_slice) ||
        lisp_slice_matches(active_slice, "basic") ||
        lisp_slice_matches(active_slice, "memory-lifetime") ||
        lisp_slice_matches(active_slice, "memory-lifetime-smoke") ||
        lisp_slice_matches(active_slice, "memory-lifetime-policy") ||
        lisp_slice_matches(active_slice, "memory-lifetime-soak") ||
        lisp_slice_matches(active_slice, "memory-lifetime-bench") ||
        lisp_slice_matches(active_slice, "memory-stress") ||
        lisp_slice_matches(active_slice, "allocator-validation") ||
        lisp_slice_matches(active_slice, "allocator-bench") ||
        lisp_slice_matches(active_slice, "list-closure") ||
        lisp_slice_matches(active_slice, "arithmetic-comparison") ||
        lisp_slice_matches(active_slice, "string-type") ||
        lisp_slice_matches(active_slice, "diagnostics") ||
        lisp_slice_matches(active_slice, "jit-policy") ||
        lisp_slice_matches(active_slice, "advanced") ||
        lisp_slice_matches(active_slice, "escape-scope") ||
        lisp_slice_matches(active_slice, "limit-busting") ||
        lisp_slice_matches(active_slice, "tco-recycling") ||
        lisp_slice_matches(active_slice, "closure-lifecycle") ||
        lisp_slice_matches(active_slice, "pika") ||
        lisp_slice_matches(active_slice, "unicode") ||
        lisp_slice_matches(active_slice, "compression") ||
        lisp_slice_matches(active_slice, "data-format") ||
        lisp_slice_matches(active_slice, "json") ||
        lisp_slice_matches(active_slice, "async") ||
        lisp_slice_matches(active_slice, "reader-dispatch") ||
        lisp_slice_matches(active_slice, "schema") ||
        lisp_slice_matches(active_slice, "deduce") ||
        lisp_slice_matches(active_slice, "scheduler") ||
        lisp_slice_matches(active_slice, "http") ||
        lisp_slice_matches(active_slice, "atomic") ||
        lisp_slice_matches(active_slice, "compiler");
}

fn bool lisp_slice_high_memory(char* active_slice) @inline {
    return lisp_slice_any(active_slice) ||
        lisp_slice_matches(active_slice, "memory-lifetime-soak") ||
        lisp_slice_matches(active_slice, "memory-lifetime-bench") ||
        lisp_slice_matches(active_slice, "allocator-bench") ||
        lisp_slice_matches(active_slice, "memory-stress");
}

fn bool lisp_slice_memory_ownership(char* active_slice) @inline {
    return lisp_slice_matches(active_slice, "memory-lifetime") ||
        lisp_slice_matches(active_slice, "memory-lifetime-smoke") ||
        lisp_slice_matches(active_slice, "memory-lifetime-policy") ||
        lisp_slice_matches(active_slice, "memory-lifetime-soak") ||
        lisp_slice_matches(active_slice, "memory-lifetime-bench") ||
        lisp_slice_matches(active_slice, "memory-stress") ||
        lisp_slice_matches(active_slice, "allocator-validation") ||
        lisp_slice_matches(active_slice, "allocator-bench");
}

fn bool in_validation_container() @inline {
    return test_c_getenv("OMNI_IN_VALIDATION_CONTAINER") != null;
}

fn bool allow_all_lisp_slice() @inline {
    return test_c_getenv("OMNI_ALLOW_ALL_LISP_SLICE") != null;
}

fn bool explicit_lisp_slice_requested() @inline {
    return test_c_getenv("OMNI_LISP_TEST_SLICE") != null;
}

fn bool lisp_slice_deprecated_alias(char* active_slice) @inline {
    return lisp_slice_matches(active_slice, "memory-soak") ||
        lisp_slice_matches(active_slice, "syntax");
}

fn void reject_deprecated_slice_aliases(char* active_slice) {
    if (!lisp_slice_deprecated_alias(active_slice)) return;

    io::printfn("Unsupported OMNI_LISP_TEST_SLICE='%s' is no longer supported.", (ZString)active_slice);
    io::printn("Use explicit ownership slices instead:");
    io::printn("- memory-lifetime-smoke          (boundary/scoping correctness)");
    io::printn("- memory-lifetime-policy          (boundary-policy parser contracts)");
    io::printn("- memory-lifetime-bench           (boundary benchmark)");
    io::printn("- allocator-validation            (AST arena correctness)");
    io::printn("- allocator-bench                (AST throughput/benchmark)");
    io::printn("- memory-lifetime-soak            (heavy saturation/stress)");
    io::printn("- memory-stress                   (global stress)");
    unreachable("unsupported OMNI_LISP_TEST_SLICE selected");
}

fn char* resolved_lisp_slice() {
    char* requested = test_c_getenv("OMNI_LISP_TEST_SLICE");
    if (requested != null) return requested;

    if (allow_all_lisp_slice()) {
        return (char*)"all";
    }

    if (test_summary_enabled()) {
        io::printfn("OMNI_TEST_SUMMARY slice default: OMNI_LISP_TEST_SLICE not set; using 'basic'. Set OMNI_ALLOW_ALL_LISP_SLICE=1 for full-suite run.");
    }
    return (char*)"basic";
}

fn void enforce_container_only_slice_execution(char* active_slice) {
    if (in_validation_container()) return;

    if (!explicit_lisp_slice_requested()) return;
    if (lisp_slice_matches(active_slice, "basic")) return;
    if (!lisp_slice_memory_ownership(active_slice)) return;

    io::printfn(
        "Refusing host-side memory-lifetime/allocator lisp test run for slice '%s'.",
        (ZString)active_slice
    );
    io::printn(
        "Run memory ownership slices via scripts/run_validation_container.sh so CPU and memory stay bounded."
    );
    unreachable("host-side memory ownership slices are container-only");
}

fn void enforce_container_only_high_memory(char* active_slice) {
    if (!lisp_slice_high_memory(active_slice)) return;

    if (in_validation_container()) return;

    io::printn("Refusing high-memory lisp slice outside validation container.");
    io::printfn(
        "Run this test selection via scripts/run_validation_container.sh.");
    unreachable("high-memory lisp slices are container-only");
}
