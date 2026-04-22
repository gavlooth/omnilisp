module lisp;

import std::io;
import main;

fn Value* runtime_memory_stats_oom(Interp* interp) @inline {
    return runtime_raise(interp, "runtime/out-of-memory", "runtime-memory-stats: out of memory");
}

fn Value* runtime_memory_make_dict(Interp* interp, uint capacity) @inline {
    return make_hashmap_checked(interp, capacity, "runtime-memory-stats: out of memory");
}

fn Value* runtime_memory_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return runtime_memory_stats_oom(interp);
    if (value != null && value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return runtime_memory_stats_oom(interp);
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) return runtime_memory_stats_oom(interp);
    return null;
}

fn Value* runtime_memory_dict_set_usz(Value* dict, Interp* interp, char[] key, usz value) @inline {
    return runtime_memory_dict_set(dict, interp, key, make_int(interp, (long)value));
}

fn Value* runtime_memory_dict_set_symbol(Value* dict, Interp* interp, char[] key, char[] symbol_name) @inline {
    SymbolId value_sym = interp.symbols.intern(symbol_name);
    if (value_sym == INVALID_SYMBOL_ID) return runtime_memory_stats_oom(interp);
    return runtime_memory_dict_set(dict, interp, key, make_symbol(interp, value_sym));
}

fn Value* runtime_memory_scope_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 32);
    if (out == null || out.tag == ERROR) return out;
    main::ScopeMemoryTelemetryStats stats = main::g_scope_memory_telemetry_stats;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-bytes-allocated", stats.total_temp_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-bytes-allocated", stats.total_escape_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-chunk-bytes-allocated", stats.temp_chunk_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-chunk-bytes-allocated", stats.escape_chunk_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-bytes-freed", stats.temp_bytes_freed)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-bytes-freed", stats.escape_bytes_freed)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-create-count", stats.scope_create_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-release-count", stats.scope_release_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-destroy-count", stats.scope_destroy_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-recycle-count", stats.scope_recycle_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "freelist-count", main::g_scope_freelist_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "generation-counter", main::g_scope_generation_counter)) return err;
    if (interp.current_scope != null) {
        if (Value* err = runtime_memory_dict_set_usz(out, interp, "current-scope-alloc-bytes", interp.current_scope.alloc_bytes)) return err;
        if (Value* err = runtime_memory_dict_set_usz(out, interp, "current-scope-alloc-count", interp.current_scope.alloc_count)) return err;
    } else {
        if (Value* err = runtime_memory_dict_set_usz(out, interp, "current-scope-alloc-bytes", 0)) return err;
        if (Value* err = runtime_memory_dict_set_usz(out, interp, "current-scope-alloc-count", 0)) return err;
    }
    return out;
}

fn Value* runtime_memory_transfer_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 16);
    if (out == null || out.tag == ERROR) return out;
    main::ScopeTransferStats stats = main::g_scope_transfer_stats;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "splice-calls", stats.splice_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-bytes-spliced", stats.splice_escape_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-bytes-freed-by-splice", stats.splice_temp_chunk_bytes_freed)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-dtors-run-by-splice", stats.splice_temp_dtors_run)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-dtors-spliced", stats.splice_escape_dtors_transferred)) return err;
    return out;
}

fn Value* runtime_memory_fiber_temp_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 32);
    if (out == null || out.tag == ERROR) return out;
    main::FiberTempPoolStats stats = main::g_fiber_temp_pool_stats;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "take-hits", stats.take_hits)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "take-misses", stats.take_misses)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "return-count", stats.return_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "drop-frees", stats.drop_frees)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "peak-pool-count", stats.peak_pool_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "current-pool-count", main::g_fiber_temp_chunk_pool_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ctx-take-hits", stats.ctx_take_hits)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ctx-return-count", stats.ctx_return_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ctx-pool-created", stats.ctx_pool_created)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-clone-callbacks", stats.lifecycle_clone_callbacks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-callbacks", stats.lifecycle_destroy_callbacks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-deferred", stats.lifecycle_destroy_deferred)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-flush-chunks", stats.lifecycle_destroy_flush_chunks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "eligible-slow-allocs", stats.eligible_slow_allocs)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "bypass-large-allocs", stats.bypass_large_allocs)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "bypass-escape-activity-allocs", stats.bypass_escape_activity_allocs)) return err;
    return out;
}

fn Value* runtime_memory_copy_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 32);
    if (out == null || out.tag == ERROR) return out;
    CopyToParentStats stats = copy_to_parent_stats_snapshot();
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "total-calls", stats.total_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "fast-reuse-hits", stats.fast_reuse_hits)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "defensive-copies", stats.defensive_copies)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "copy-to-parent-bytes", stats.copy_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "nil-calls", stats.nil_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "int-calls", stats.int_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "double-calls", stats.double_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "string-calls", stats.string_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "symbol-calls", stats.symbol_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-calls", stats.cons_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-calls", stats.closure_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "other-calls", stats.other_calls)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-copy-samples", stats.cons_spine_copy_samples)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-copy-total-len", stats.cons_spine_copy_total_len)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-copy-peak-len", stats.cons_spine_copy_peak_len)) return err;
    return out;
}

fn Value* prim_runtime_memory_stats(Value*[] args, Env* env, Interp* interp) {
    (void)args;
    (void)env;
    Value* out = runtime_memory_make_dict(interp, 16);
    if (out == null || out.tag == ERROR) return out;
    if (Value* err = runtime_memory_dict_set_symbol(out, interp, "kind", "runtime-memory-stats")) return err;
    if (Value* err = runtime_memory_dict_set_symbol(out, interp, "status", "ok")) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "scope", runtime_memory_scope_stats(interp))) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "scope-transfer", runtime_memory_transfer_stats(interp))) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "fiber-temp", runtime_memory_fiber_temp_stats(interp))) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "copy-to-parent", runtime_memory_copy_stats(interp))) return err;
    return out;
}

fn bool runtime_memory_telemetry_enabled() @inline {
    return boundary_env_flag_enabled("OMNI_MEM_TELEMETRY");
}

fn void runtime_memory_print_telemetry() {
    if (!runtime_memory_telemetry_enabled()) return;
    main::ScopeMemoryTelemetryStats scope_stats = main::g_scope_memory_telemetry_stats;
    main::ScopeTransferStats transfer_stats = main::g_scope_transfer_stats;
    main::FiberTempPoolStats fiber_stats = main::g_fiber_temp_pool_stats;
    CopyToParentStats copy_stats = copy_to_parent_stats_snapshot();

    io::printn("{\"kind\":\"runtime-memory-stats\",\"status\":\"ok\",");
    io::printfn("\"scope\":{\"temp_bytes_allocated\":%d,\"escape_bytes_allocated\":%d,\"temp_chunk_bytes_allocated\":%d,\"escape_chunk_bytes_allocated\":%d,\"temp_bytes_freed\":%d,\"escape_bytes_freed\":%d,\"scope_create_count\":%d,\"scope_release_count\":%d,\"scope_destroy_count\":%d,\"scope_recycle_count\":%d},",
        scope_stats.total_temp_bytes_allocated,
        scope_stats.total_escape_bytes_allocated,
        scope_stats.temp_chunk_bytes_allocated,
        scope_stats.escape_chunk_bytes_allocated,
        scope_stats.temp_bytes_freed,
        scope_stats.escape_bytes_freed,
        scope_stats.scope_create_count,
        scope_stats.scope_release_count,
        scope_stats.scope_destroy_count,
        scope_stats.scope_recycle_count);
    io::printfn("\"scope_transfer\":{\"splice_calls\":%d,\"escape_bytes_spliced\":%d,\"temp_bytes_freed_by_splice\":%d},",
        transfer_stats.splice_calls,
        transfer_stats.splice_escape_chunk_bytes,
        transfer_stats.splice_temp_chunk_bytes_freed);
    io::printfn("\"fiber_temp\":{\"take_hits\":%d,\"take_misses\":%d,\"return_count\":%d,\"drop_frees\":%d,\"current_pool_count\":%d},",
        fiber_stats.take_hits,
        fiber_stats.take_misses,
        fiber_stats.return_count,
        fiber_stats.drop_frees,
        main::g_fiber_temp_chunk_pool_count);
    io::printfn("\"copy_to_parent\":{\"total_calls\":%d,\"fast_reuse_hits\":%d,\"defensive_copies\":%d,\"copy_to_parent_bytes\":%d}}",
        copy_stats.total_calls,
        copy_stats.fast_reuse_hits,
        copy_stats.defensive_copies,
        copy_stats.copy_bytes);
}
