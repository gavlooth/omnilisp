module main;

import std::io;
import std::thread;

const usz SCOPE_TELEMETRY_THREAD_WORKERS = 8;
const usz SCOPE_TELEMETRY_THREAD_ITERS = 256;
const usz SCOPE_TELEMETRY_THREAD_ALLOC_SIZE = 16;

struct ScopeTelemetryThreadState {
    bool ok;
}

fn int scope_region_telemetry_thread_worker(void* arg) {
    ScopeTelemetryThreadState* state = (ScopeTelemetryThreadState*)arg;
    if (state != null) state.ok = true;
    for (usz i = 0; i < SCOPE_TELEMETRY_THREAD_ITERS; i++) {
        ScopeRegion* scope = scope_create(null);
        if (scope == null) {
            if (state != null) state.ok = false;
            return 1;
        }
        void* temp_ptr = scope.alloc(SCOPE_TELEMETRY_THREAD_ALLOC_SIZE);
        void* escape_ptr = scope.alloc_escape(SCOPE_TELEMETRY_THREAD_ALLOC_SIZE);
        if (temp_ptr == null || escape_ptr == null) {
            if (state != null) state.ok = false;
            scope_release(scope);
            return 1;
        }
        scope_release(scope);
    }
    return 0;
}

fn bool run_scope_region_threaded_telemetry_test() {
    ScopeMemoryTelemetryStats before = scope_memory_telemetry_stats_snapshot();
    Thread[SCOPE_TELEMETRY_THREAD_WORKERS] threads;
    ScopeTelemetryThreadState[SCOPE_TELEMETRY_THREAD_WORKERS] states;
    bool[SCOPE_TELEMETRY_THREAD_WORKERS] created;
    bool ok = true;

    foreach (i, &t : threads) {
        states[i].ok = false;
        created[i] = false;
        if (catch err = t.create(&scope_region_telemetry_thread_worker, (void*)&states[i])) {
            (void)err;
            ok = false;
            states[i].ok = false;
        } else {
            created[i] = true;
        }
    }

    foreach (i, &t : threads) {
        if (!created[i]) continue;
        if (catch err = t.join()) {
            (void)err;
            ok = false;
        }
        ok = ok && states[i].ok;
    }

    ScopeMemoryTelemetryStats after = scope_memory_telemetry_stats_snapshot();
    usz expected_ops = SCOPE_TELEMETRY_THREAD_WORKERS * SCOPE_TELEMETRY_THREAD_ITERS;
    usz expected_bytes = expected_ops * SCOPE_TELEMETRY_THREAD_ALLOC_SIZE;
    return ok &&
        after.scope_create_count >= before.scope_create_count + expected_ops &&
        after.scope_release_count >= before.scope_release_count + expected_ops &&
        after.scope_destroy_count >= before.scope_destroy_count + expected_ops &&
        after.total_temp_bytes_allocated >= before.total_temp_bytes_allocated + expected_bytes &&
        after.total_escape_bytes_allocated >= before.total_escape_bytes_allocated + expected_bytes;
}

fn bool run_scope_region_telemetry_saturation_test() {
    ScopeMemoryTelemetryStats saved_scope = scope_memory_telemetry_stats_snapshot();
    ScopeTransferStats saved_transfer = scope_transfer_stats_snapshot();
    FiberTempPoolStats saved_fiber = fiber_temp_pool_stats_snapshot();

    scope_telemetry_store(&g_scope_memory_telemetry_stats.total_temp_bytes_allocated, usz.max - (usz)1);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.total_escape_bytes_allocated, usz.max - (usz)1);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_chunk_bytes_allocated, usz.max - (usz)1);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_live_chunk_count, usz.max - (usz)1);
    scope_telemetry_store(&g_scope_transfer_stats.splice_calls, usz.max - (usz)1);
    scope_telemetry_store(&g_scope_transfer_stats.splice_escape_chunk_bytes, usz.max - (usz)1);
    scope_telemetry_store(&g_fiber_temp_pool_stats.take_hit_chunk_bytes, usz.max - (usz)1);
    scope_telemetry_store(&g_fiber_temp_pool_stats.return_chunk_bytes, usz.max - (usz)1);
    scope_telemetry_store(&g_fiber_temp_pool_stats.drop_free_chunk_bytes, usz.max - (usz)1);

    ScopeChunk chunk = {};
    chunk.capacity = 8;
    scope_telemetry_note_temp_alloc(8);
    scope_telemetry_note_escape_alloc(8);
    scope_telemetry_note_temp_chunk_alloc(&chunk);
    usz live = scope_telemetry_add(&g_scope_memory_telemetry_stats.temp_live_chunk_count, 8);
    scope_telemetry_max(&g_scope_memory_telemetry_stats.temp_peak_live_chunk_count, live);
    scope_telemetry_inc(&g_scope_transfer_stats.splice_calls);
    scope_telemetry_add(&g_scope_transfer_stats.splice_escape_chunk_bytes, 8);
    scope_telemetry_add(&g_fiber_temp_pool_stats.take_hit_chunk_bytes, 8);
    scope_telemetry_add(&g_fiber_temp_pool_stats.return_chunk_bytes, 8);
    scope_telemetry_add(&g_fiber_temp_pool_stats.drop_free_chunk_bytes, 8);

    ScopeMemoryTelemetryStats scope_after = scope_memory_telemetry_stats_snapshot();
    ScopeTransferStats transfer_after = scope_transfer_stats_snapshot();
    FiberTempPoolStats fiber_after = fiber_temp_pool_stats_snapshot();

    ScopeChunk first = {};
    ScopeChunk second = {};
    first.capacity = usz.max - (usz)1;
    first.next = &second;
    second.capacity = 8;
    bool local_saturated =
        scope_chunk_bytes(&first) == usz.max &&
        scope_telemetry_saturating_add_local(usz.max - (usz)1, 8) == usz.max &&
        scope_telemetry_saturating_inc_local(usz.max) == usz.max;

    bool staging_saturated = true;
    if (scope_memory_shape_telemetry_enabled()) {
        ScopeRegion fake = {};
        fake.temp_slow_sequence_active = true;
        fake.temp_slow_sequence_followup_count = usz.max - (usz)1;
        fake.temp_slow_sequence_followup_bytes = usz.max - (usz)1;
        scope_telemetry_note_temp_sequence_followup(&fake, 8);
        staging_saturated = staging_saturated &&
            fake.temp_slow_sequence_followup_count == usz.max &&
            fake.temp_slow_sequence_followup_bytes == usz.max;

        fake = {};
        fake.escape_slow_sequence_active = true;
        fake.escape_slow_sequence_followup_count = usz.max - (usz)1;
        fake.escape_slow_sequence_followup_bytes = usz.max - (usz)1;
        scope_telemetry_note_escape_sequence_followup(&fake, 8);
        staging_saturated = staging_saturated &&
            fake.escape_slow_sequence_followup_count == usz.max &&
            fake.escape_slow_sequence_followup_bytes == usz.max;
    }

    bool saturated =
        scope_after.total_temp_bytes_allocated == usz.max &&
        scope_after.total_escape_bytes_allocated == usz.max &&
        scope_after.temp_chunk_bytes_allocated == usz.max &&
        scope_after.temp_live_chunk_count == usz.max &&
        scope_after.temp_peak_live_chunk_count == usz.max &&
        transfer_after.splice_calls == usz.max &&
        transfer_after.splice_escape_chunk_bytes == usz.max &&
        fiber_after.take_hit_chunk_bytes == usz.max &&
        fiber_after.return_chunk_bytes == usz.max &&
        fiber_after.drop_free_chunk_bytes == usz.max &&
        local_saturated &&
        staging_saturated;

    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_live_chunk_count, 0);
    scope_telemetry_sub_one_if_nonzero(&g_scope_memory_telemetry_stats.temp_live_chunk_count);
    bool underflow_guarded = scope_telemetry_load(&g_scope_memory_telemetry_stats.temp_live_chunk_count) == 0;

    scope_telemetry_store(&g_scope_memory_telemetry_stats.total_temp_bytes_allocated, saved_scope.total_temp_bytes_allocated);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.total_escape_bytes_allocated, saved_scope.total_escape_bytes_allocated);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_chunk_bytes_allocated, saved_scope.temp_chunk_bytes_allocated);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_live_chunk_count, saved_scope.temp_live_chunk_count);
    scope_telemetry_store(&g_scope_memory_telemetry_stats.temp_peak_live_chunk_count, saved_scope.temp_peak_live_chunk_count);
    scope_telemetry_store(&g_scope_transfer_stats.splice_calls, saved_transfer.splice_calls);
    scope_telemetry_store(&g_scope_transfer_stats.splice_escape_chunk_bytes, saved_transfer.splice_escape_chunk_bytes);
    scope_telemetry_store(&g_fiber_temp_pool_stats.take_hit_chunk_bytes, saved_fiber.take_hit_chunk_bytes);
    scope_telemetry_store(&g_fiber_temp_pool_stats.return_chunk_bytes, saved_fiber.return_chunk_bytes);
    scope_telemetry_store(&g_fiber_temp_pool_stats.drop_free_chunk_bytes, saved_fiber.drop_free_chunk_bytes);

    return saturated && underflow_guarded;
}

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
            FiberTempPoolStats fiber_before = fiber_temp_pool_stats_snapshot();
            usz returns_before = fiber_before.return_count;
            usz drops_before = fiber_before.drop_frees;
            usz hits_before = fiber_before.take_hits;
            usz misses_before = fiber_before.take_misses;
            usz pooled_before = fiber_temp_chunk_pool_count_snapshot();

            ScopeChunk* offered = scope_chunk_alloc_raw(SCOPE_CHUNK_INITIAL);
            scope_chunk_reclaim_temp(offered);

            FiberTempPoolStats fiber_after_reclaim = fiber_temp_pool_stats_snapshot();
            bool returned = fiber_after_reclaim.return_count == returns_before + 1;
            bool dropped = fiber_after_reclaim.drop_frees == drops_before + 1;
            bool ok = returned != dropped;

            usz pooled_after_reclaim = fiber_temp_chunk_pool_count_snapshot();
            if (returned) {
                ok = ok && pooled_after_reclaim == pooled_before + 1;
            } else {
                ok = ok && pooled_after_reclaim == pooled_before;
            }

            ScopeChunk* taken = fiber_temp_chunk_try_take(SCOPE_CHUNK_INITIAL);
            FiberTempPoolStats fiber_after_take = fiber_temp_pool_stats_snapshot();
            if (returned) {
                ok = ok && taken != null;
                ok = ok && fiber_after_take.take_hits == hits_before + 1;
                ok = ok && fiber_temp_chunk_pool_count_snapshot() == pooled_before;
            } else {
                ok = ok && taken == null;
                ok = ok && fiber_after_take.take_hits == hits_before;
            }

            ScopeChunk* miss = fiber_temp_chunk_try_take(SCOPE_CHUNK_MAX + 1);
            FiberTempPoolStats fiber_after_miss = fiber_temp_pool_stats_snapshot();
            ok = ok && miss == null;
            ok = ok && fiber_after_miss.take_misses == misses_before + 1;

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

    // Test 17: process-wide scope telemetry is safe across independent owner threads.
    {
        if (run_scope_region_threaded_telemetry_test()) {
            passed++;
        } else {
            io::printn("FAIL: threaded scope telemetry counters");
            failed++;
        }
    }

    // Test 18: process-wide telemetry counters saturate instead of wrapping.
    {
        if (run_scope_region_telemetry_saturation_test()) {
            passed++;
        } else {
            io::printn("FAIL: scope telemetry counters saturate");
            failed++;
        }
    }

    if (!scope_test_quiet_requested()) {
        io::printfn("%d passed, %d failed", (int)passed, (int)failed);
    }
    if (scope_c_getenv("OMNI_TEST_SUMMARY") != null) {
        FiberTempPoolStats fiber_stats = fiber_temp_pool_stats_snapshot();
        io::printfn("OMNI_TEST_SUMMARY suite=scope_region pass=%d fail=%d", (int)passed, (int)failed);
        io::printfn(
            "OMNI_TEST_SUMMARY suite=fiber_temp_pool enabled=%d hits=%d misses=%d returns=%d drop_frees=%d pooled=%d peak=%d ctx_hits=%d ctx_returns=%d ctx_pools=%d lc_clone=%d lc_destroy=%d lc_defer=%d lc_flush=%d eligible_slow=%d bypass_large=%d bypass_escape=%d",
            (int)(scope_fiber_temp_flag_enabled() ? 1 : 0),
            (int)fiber_stats.take_hits,
            (int)fiber_stats.take_misses,
            (int)fiber_stats.return_count,
            (int)fiber_stats.drop_frees,
            (int)fiber_temp_chunk_pool_count_snapshot(),
            (int)fiber_stats.peak_pool_count,
            (int)fiber_stats.ctx_take_hits,
            (int)fiber_stats.ctx_return_count,
            (int)fiber_stats.ctx_pool_created,
            (int)fiber_stats.lifecycle_clone_callbacks,
            (int)fiber_stats.lifecycle_destroy_callbacks,
            (int)fiber_stats.lifecycle_destroy_deferred,
            (int)fiber_stats.lifecycle_destroy_flush_chunks,
            (int)fiber_stats.eligible_slow_allocs,
            (int)fiber_stats.bypass_large_allocs,
            (int)fiber_stats.bypass_escape_activity_allocs
        );
    }

    if (out_pass != null) *out_pass = (int)passed;
    if (out_fail != null) *out_fail = (int)failed;
}
