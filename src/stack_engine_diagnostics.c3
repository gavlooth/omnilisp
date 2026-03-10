module main;

import std::io;

bool g_stack_quiet_output = false;

fn bool stack_summary_enabled() @inline {
    return getenv("OMNI_TEST_SUMMARY") != null;
}

fn bool stack_quiet_enabled() @inline {
    return g_stack_quiet_output;
}

fn bool stack_defer_metrics_enabled() @inline {
    return getenv("OMNI_STACK_DEFER_METRICS") != null;
}

usz g_stack_defer_push_count = 0;
usz g_stack_defer_undefer_count = 0;
usz g_stack_defer_destroy_callback_count = 0;
usz g_stack_defer_clone_callback_count = 0;
usz g_stack_defer_update_arg_count = 0;
usz g_stack_defer_entries_cloned = 0;
usz g_stack_defer_peak_depth = 0;
usz g_stack_defer_heap_alloc_count = 0;

fn void stack_defer_metrics_reset() {
    g_stack_defer_push_count = 0;
    g_stack_defer_undefer_count = 0;
    g_stack_defer_destroy_callback_count = 0;
    g_stack_defer_clone_callback_count = 0;
    g_stack_defer_update_arg_count = 0;
    g_stack_defer_entries_cloned = 0;
    g_stack_defer_peak_depth = 0;
    g_stack_defer_heap_alloc_count = 0;
}

fn void emit_stack_defer_summary() {
    bool summary = stack_summary_enabled();
    bool verbose = stack_defer_metrics_enabled();
    if (!summary && !verbose) return;
    if (verbose) {
        io::printfn(
            "STACK_DEFER_METRICS push=%d undefer=%d destroy_cb=%d clone_cb=%d update_arg=%d cloned_entries=%d peak_depth=%d heap_alloc=%d",
            (uint)g_stack_defer_push_count,
            (uint)g_stack_defer_undefer_count,
            (uint)g_stack_defer_destroy_callback_count,
            (uint)g_stack_defer_clone_callback_count,
            (uint)g_stack_defer_update_arg_count,
            (uint)g_stack_defer_entries_cloned,
            (uint)g_stack_defer_peak_depth,
            (uint)g_stack_defer_heap_alloc_count
        );
    }
    if (summary) {
        io::printfn(
            "OMNI_TEST_SUMMARY suite=stack_defer push=%d undefer=%d destroy_cb=%d clone_cb=%d update_arg=%d cloned_entries=%d peak_depth=%d heap_alloc=%d",
            (uint)g_stack_defer_push_count,
            (uint)g_stack_defer_undefer_count,
            (uint)g_stack_defer_destroy_callback_count,
            (uint)g_stack_defer_clone_callback_count,
            (uint)g_stack_defer_update_arg_count,
            (uint)g_stack_defer_entries_cloned,
            (uint)g_stack_defer_peak_depth,
            (uint)g_stack_defer_heap_alloc_count
        );
    }
}

fn void emit_stack_summary(int pass, int fail) {
    if (!stack_summary_enabled()) return;
    io::printfn("OMNI_TEST_SUMMARY suite=stack_engine pass=%d fail=%d", pass, fail);
}

fn void stack_print_pass(char[] msg) @inline {
    if (stack_quiet_enabled()) return;
    io::printfn("  PASS: %s", (ZString)msg);
}
