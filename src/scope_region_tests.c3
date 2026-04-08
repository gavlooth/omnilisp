module main;

import std::io;

fn void run_scope_region_tests(int* out_pass, int* out_fail) {
    if (!scope_test_quiet_requested()) {
        io::print("  Scope region tests... ");
    }
    ScopeRegionTestCounts counts = run_scope_region_core_tests();
    usz passed = counts.passed;
    usz failed = counts.failed;

    // Test 16: Fiber TEMP pool invariants (flagged only)
    {
        if (!scope_fiber_temp_flag_enabled()) {
            // Feature-gated: keep default suite behavior stable when flag is off.
            passed++;
        } else {
            usz returns_before = g_fiber_temp_pool_stats.return_count;
            usz drops_before = g_fiber_temp_pool_stats.drop_frees;
            usz hits_before = g_fiber_temp_pool_stats.take_hits;
            usz misses_before = g_fiber_temp_pool_stats.take_misses;
            usz pooled_before = g_fiber_temp_chunk_pool_count;

            ScopeChunk* offered = scope_chunk_alloc_raw(SCOPE_CHUNK_INITIAL);
            scope_chunk_reclaim_temp(offered);

            bool returned = g_fiber_temp_pool_stats.return_count == returns_before + 1;
            bool dropped = g_fiber_temp_pool_stats.drop_frees == drops_before + 1;
            bool ok = returned != dropped;

            usz pooled_after_reclaim = g_fiber_temp_chunk_pool_count;
            if (returned) {
                ok = ok && pooled_after_reclaim == pooled_before + 1;
            } else {
                ok = ok && pooled_after_reclaim == pooled_before;
            }

            ScopeChunk* taken = fiber_temp_chunk_try_take(SCOPE_CHUNK_INITIAL);
            if (returned) {
                ok = ok && taken != null;
                ok = ok && g_fiber_temp_pool_stats.take_hits == hits_before + 1;
                ok = ok && g_fiber_temp_chunk_pool_count == pooled_before;
            } else {
                ok = ok && taken == null;
                ok = ok && g_fiber_temp_pool_stats.take_hits == hits_before;
            }

            ScopeChunk* miss = fiber_temp_chunk_try_take(SCOPE_CHUNK_MAX + 1);
            ok = ok && miss == null;
            ok = ok && g_fiber_temp_pool_stats.take_misses == misses_before + 1;

            if (taken != null) {
                scope_chunk_reclaim_temp(taken);
            }

            if (ok) {
                passed++;
            } else {
                io::printn("FAIL: fiber-temp pool invariants");
                failed++;
            }
        }
    }

    if (!scope_test_quiet_requested()) {
        io::printfn("%d passed, %d failed", (int)passed, (int)failed);
    }
    if (scope_c_getenv("OMNI_TEST_SUMMARY") != null) {
        io::printfn("OMNI_TEST_SUMMARY suite=scope_region pass=%d fail=%d", (int)passed, (int)failed);
        io::printfn(
            "OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=%d hits=%d misses=%d returns=%d drop_frees=%d pooled=%d peak=%d ctx_hits=%d ctx_returns=%d ctx_pools=%d lc_clone=%d lc_destroy=%d lc_defer=%d lc_flush=%d eligible_slow=%d bypass_large=%d bypass_escape=%d",
            (int)(scope_fiber_temp_flag_enabled() ? 1 : 0),
            (int)g_fiber_temp_pool_stats.take_hits,
            (int)g_fiber_temp_pool_stats.take_misses,
            (int)g_fiber_temp_pool_stats.return_count,
            (int)g_fiber_temp_pool_stats.drop_frees,
            (int)g_fiber_temp_chunk_pool_count,
            (int)g_fiber_temp_pool_stats.peak_pool_count,
            (int)g_fiber_temp_pool_stats.ctx_take_hits,
            (int)g_fiber_temp_pool_stats.ctx_return_count,
            (int)g_fiber_temp_pool_stats.ctx_pool_created,
            (int)g_fiber_temp_pool_stats.lifecycle_clone_callbacks,
            (int)g_fiber_temp_pool_stats.lifecycle_destroy_callbacks,
            (int)g_fiber_temp_pool_stats.lifecycle_destroy_deferred,
            (int)g_fiber_temp_pool_stats.lifecycle_destroy_flush_chunks,
            (int)g_fiber_temp_pool_stats.eligible_slow_allocs,
            (int)g_fiber_temp_pool_stats.bypass_large_allocs,
            (int)g_fiber_temp_pool_stats.bypass_escape_activity_allocs
        );
    }

    if (out_pass != null) *out_pass = (int)passed;
    if (out_fail != null) *out_fail = (int)failed;
}
