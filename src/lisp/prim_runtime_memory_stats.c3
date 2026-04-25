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
    Value* out = runtime_memory_make_dict(interp, 112);
    if (out == null || out.tag == ERROR) return out;
    main::ScopeMemoryTelemetryStats stats = main::g_scope_memory_telemetry_stats;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-bytes-allocated", stats.total_temp_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-bytes-allocated", stats.total_escape_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-chunk-bytes-allocated", stats.temp_chunk_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-chunk-bytes-allocated", stats.escape_chunk_bytes_allocated)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-bytes-freed", stats.temp_bytes_freed)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-bytes-freed", stats.escape_bytes_freed)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-alloc-count", stats.temp_slow_alloc_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-alloc-count", stats.escape_slow_alloc_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-requested-bytes", stats.temp_slow_requested_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-requested-bytes", stats.escape_slow_requested_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-selected-chunk-bytes", stats.temp_slow_selected_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-selected-chunk-bytes", stats.escape_slow_selected_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-slack-exact-count", stats.temp_slow_slack_exact_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-slack-le-512-count", stats.temp_slow_slack_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-slack-le-4096-count", stats.temp_slow_slack_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-slack-gt-4096-count", stats.temp_slow_slack_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-slack-exact-count", stats.escape_slow_slack_exact_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-slack-le-512-count", stats.escape_slow_slack_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-slack-le-4096-count", stats.escape_slow_slack_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-slack-gt-4096-count", stats.escape_slow_slack_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-closed-count", stats.temp_slow_sequence_closed_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-closed-count", stats.escape_slow_sequence_closed_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-followup-alloc-count", stats.temp_slow_sequence_followup_alloc_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-followup-alloc-count", stats.escape_slow_sequence_followup_alloc_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-followup-bytes", stats.temp_slow_sequence_followup_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-followup-bytes", stats.escape_slow_sequence_followup_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-unused-at-close-bytes", stats.temp_slow_sequence_unused_at_close_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-unused-at-close-bytes", stats.escape_slow_sequence_unused_at_close_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-count", stats.temp_slow_sequence_no_followup_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-count", stats.escape_slow_sequence_no_followup_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-request-le-512-count", stats.temp_slow_sequence_no_followup_request_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-request-le-4096-count", stats.temp_slow_sequence_no_followup_request_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-request-gt-4096-count", stats.temp_slow_sequence_no_followup_request_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-request-le-512-count", stats.escape_slow_sequence_no_followup_request_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-request-le-4096-count", stats.escape_slow_sequence_no_followup_request_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-request-gt-4096-count", stats.escape_slow_sequence_no_followup_request_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-unused-exact-count", stats.temp_slow_sequence_no_followup_unused_exact_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-unused-le-512-count", stats.temp_slow_sequence_no_followup_unused_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-unused-le-4096-count", stats.temp_slow_sequence_no_followup_unused_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-no-followup-unused-gt-4096-count", stats.temp_slow_sequence_no_followup_unused_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-unused-exact-count", stats.escape_slow_sequence_no_followup_unused_exact_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-unused-le-512-count", stats.escape_slow_sequence_no_followup_unused_le_512_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-unused-le-4096-count", stats.escape_slow_sequence_no_followup_unused_le_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-unused-gt-4096-count", stats.escape_slow_sequence_no_followup_unused_gt_4096_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-unknown-count", stats.escape_slow_sequence_no_followup_source_unknown_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-direct-count", stats.escape_slow_sequence_no_followup_source_direct_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-dtor-count", stats.escape_slow_sequence_no_followup_source_dtor_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-interp-value-count", stats.escape_slow_sequence_no_followup_source_interp_value_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-interp-env-count", stats.escape_slow_sequence_no_followup_source_interp_env_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-boundary-payload-count", stats.escape_slow_sequence_no_followup_source_boundary_payload_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-promotion-signature-count", stats.escape_slow_sequence_no_followup_source_promotion_signature_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-promotion-closure-count", stats.escape_slow_sequence_no_followup_source_promotion_closure_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-no-followup-source-jit-args-count", stats.escape_slow_sequence_no_followup_source_jit_args_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-large-slack-count", stats.temp_slow_sequence_large_slack_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-large-slack-count", stats.escape_slow_sequence_large_slack_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-large-slack-followup-bytes", stats.temp_slow_sequence_large_slack_followup_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-large-slack-followup-bytes", stats.escape_slow_sequence_large_slack_followup_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-large-slack-unused-at-close-bytes", stats.temp_slow_sequence_large_slack_unused_at_close_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-large-slack-unused-at-close-bytes", stats.escape_slow_sequence_large_slack_unused_at_close_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-slow-sequence-large-slack-no-followup-count", stats.temp_slow_sequence_large_slack_no_followup_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-slow-sequence-large-slack-no-followup-count", stats.escape_slow_sequence_large_slack_no_followup_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-live-chunk-count", stats.temp_live_chunk_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-live-chunk-count", stats.escape_live_chunk_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-peak-live-chunk-count", stats.temp_peak_live_chunk_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-peak-live-chunk-count", stats.escape_peak_live_chunk_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-unused-bytes-at-reset", stats.temp_unused_bytes_at_reset)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-unused-bytes-at-reset", stats.escape_unused_bytes_at_reset)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "temp-unused-bytes-at-destroy", stats.temp_unused_bytes_at_destroy)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "escape-unused-bytes-at-destroy", stats.escape_unused_bytes_at_destroy)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-create-count", stats.scope_create_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-release-count", stats.scope_release_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-destroy-count", stats.scope_destroy_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-recycle-count", stats.scope_recycle_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-fresh-alloc-count", stats.scope_fresh_alloc_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "scope-recycle-hit-count", stats.scope_recycle_hit_count)) return err;
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
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "take-hit-chunk-bytes", stats.take_hit_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "return-chunk-bytes", stats.return_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "drop-free-chunk-bytes", stats.drop_free_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ctx-take-hit-chunk-bytes", stats.ctx_take_hit_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ctx-return-chunk-bytes", stats.ctx_return_chunk_bytes)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-clone-callbacks", stats.lifecycle_clone_callbacks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-callbacks", stats.lifecycle_destroy_callbacks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-deferred", stats.lifecycle_destroy_deferred)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "lifecycle-destroy-flush-chunks", stats.lifecycle_destroy_flush_chunks)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "eligible-slow-allocs", stats.eligible_slow_allocs)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "bypass-large-allocs", stats.bypass_large_allocs)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "bypass-escape-activity-allocs", stats.bypass_escape_activity_allocs)) return err;
    return out;
}

fn Value* runtime_memory_value_shape_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 96);
    if (out == null || out.tag == ERROR) return out;
    BoundaryValueShapeStats stats = boundary_value_shape_stats_snapshot();
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-construct-count", stats.array_construct_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-capacity-total", stats.array_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-capacity-peak", stats.array_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-growth-count", stats.array_growth_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-growth-old-capacity-total", stats.array_growth_old_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-growth-new-capacity-total", stats.array_growth_new_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-growth-new-capacity-peak", stats.array_growth_new_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-construct-count", stats.hashmap_construct_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-capacity-total", stats.hashmap_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-capacity-peak", stats.hashmap_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-growth-count", stats.hashmap_growth_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-growth-old-capacity-total", stats.hashmap_growth_old_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-growth-new-capacity-total", stats.hashmap_growth_new_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-growth-new-capacity-peak", stats.hashmap_growth_new_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-construct-count", stats.set_construct_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-capacity-total", stats.set_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-capacity-peak", stats.set_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-growth-count", stats.set_growth_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-growth-old-capacity-total", stats.set_growth_old_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-growth-new-capacity-total", stats.set_growth_new_capacity_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-growth-new-capacity-peak", stats.set_growth_new_capacity_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-env-copy-frame-count", stats.closure_env_copy_frame_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-env-copy-depth-total", stats.closure_env_copy_depth_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-env-copy-depth-peak", stats.closure_env_copy_depth_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-env-copy-binding-count-total", stats.closure_env_copy_binding_count_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-env-copy-binding-count-peak", stats.closure_env_copy_binding_count_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "string-payload-count", stats.string_payload_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "string-payload-bytes-total", stats.string_payload_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "string-payload-bytes-peak", stats.string_payload_bytes_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "error-payload-count", stats.error_payload_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "error-payload-bytes-total", stats.error_payload_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "error-payload-bytes-peak", stats.error_payload_bytes_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "big-integer-wrapper-count", stats.big_integer_wrapper_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "big-integer-decimal-input-count", stats.big_integer_decimal_input_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "big-integer-decimal-input-bytes-total", stats.big_integer_decimal_input_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "big-integer-decimal-input-bytes-peak", stats.big_integer_decimal_input_bytes_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "tensor-construct-count", stats.tensor_construct_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "tensor-payload-bytes-total", stats.tensor_payload_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "tensor-payload-bytes-peak", stats.tensor_payload_bytes_peak)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-wrapper-count", stats.ffi_handle_wrapper_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-borrowed-count", stats.ffi_handle_borrowed_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-owned-count", stats.ffi_handle_owned_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-manual-count", stats.ffi_handle_manual_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-releasable-count", stats.ffi_handle_releasable_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-buffer-count", stats.ffi_handle_buffer_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-library-count", stats.ffi_handle_library_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-passport-valid-count", stats.stable_passport_valid_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-passport-owner-scope-invalid-count", stats.stable_passport_owner_scope_invalid_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-passport-owner-escape-changed-count", stats.stable_passport_owner_escape_changed_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-passport-root-not-in-owner-chain-count", stats.stable_passport_root_not_in_owner_chain_count)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-passport-prepared-graph-stale-count", stats.stable_passport_prepared_graph_stale_count)) return err;
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

fn Value* runtime_memory_escape_shape_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 32);
    if (out == null || out.tag == ERROR) return out;
    BoundaryEscapeShapeStats stats = boundary_escape_shape_stats_snapshot();
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "candidate-roots", stats.candidate_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "graph-roots", stats.graph_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "unsafe-roots", stats.unsafe_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-roots", stats.cons_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-samples", stats.cons_spine_samples)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-total-len", stats.cons_spine_total_len)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "cons-spine-peak-len", stats.cons_spine_peak_len)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "array-roots", stats.array_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "hashmap-roots", stats.hashmap_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "set-roots", stats.set_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "closure-roots", stats.closure_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "partial-roots", stats.partial_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "iterator-roots", stats.iterator_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "continuation-roots", stats.continuation_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "coroutine-roots", stats.coroutine_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "tensor-roots", stats.tensor_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "instance-roots", stats.instance_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "method-table-roots", stats.method_table_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "module-roots", stats.module_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "type-info-roots", stats.type_info_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "ffi-handle-roots", stats.ffi_handle_roots)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-publication-hooks", stable_escape_store_publication_hook_count_snapshot())) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-prepared-publication-hooks", stable_escape_store_prepared_publication_hook_count_snapshot())) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-raw-publication-hooks", stable_escape_store_raw_publication_hook_count_snapshot())) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-prepare-failure-fallbacks", stable_escape_store_prepare_failure_fallback_count_snapshot())) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-alias-unsafe-skips", stable_escape_store_alias_unsafe_skip_count_snapshot())) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "stable-resolve-failures", stable_escape_store_resolve_failure_count_snapshot())) return err;
    return out;
}

fn Value* runtime_memory_boundary_decision_stats(Interp* interp) {
    Value* out = runtime_memory_make_dict(interp, 96);
    if (out == null || out.tag == ERROR) return out;
    BoundaryDecisionStats stats = boundary_decision_stats_snapshot();
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "splice-attempted", stats.splice_attempted)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "splice-succeeded", stats.splice_succeeded)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "promotion-attempted", stats.promotion_attempted)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "copy-fallback-total", stats.copy_fallback_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "planned-stable-publish", stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "planned-stable-materialize", stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "planned-transplant", stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "planned-compatibility", stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "planned-mixed", stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-publish", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-transplant", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-compatibility", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-mixed", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-fail-closed", stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_FAIL_CLOSED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "route-fail-destination", stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_DESTINATION_FAILED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "route-fail-transplant-rejected", stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_TRANSPLANT_REJECTED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "route-fail-promotion-aborted", stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_PROMOTION_ABORTED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "route-fail-mixed-destination", stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_MIXED_DESTINATION_FAILED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "route-fail-fallback-disallowed", stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_FALLBACK_DISALLOWED])) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-success", stats.stable_materialization_success)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-node-count", stats.stable_materialization_node_count_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes", stats.stable_materialization_copy_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-optimizer", boundary_decision_optimizer_materialization_copy_bytes(stats))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-forced-no-splice", stats.forced_no_splice_materialization_copy_bytes_total)) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-cons", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CONS))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-array", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, ARRAY))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-hashmap", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, HASHMAP))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-set", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, SET))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-closure", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CLOSURE))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-stable-materialize-big-integer", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, BIG_INTEGER))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-compatibility-cons", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION, CONS))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-compatibility-partial", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION, PARTIAL_PRIM))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "selected-compatibility-iterator", boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION, ITERATOR))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-cons", boundary_decision_materialization_copy_bytes_for_tag(stats, CONS))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-array", boundary_decision_materialization_copy_bytes_for_tag(stats, ARRAY))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-hashmap", boundary_decision_materialization_copy_bytes_for_tag(stats, HASHMAP))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-set", boundary_decision_materialization_copy_bytes_for_tag(stats, SET))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-closure", boundary_decision_materialization_copy_bytes_for_tag(stats, CLOSURE))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-big-integer", boundary_decision_materialization_copy_bytes_for_tag(stats, BIG_INTEGER))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-optimizer-closure", boundary_decision_optimizer_materialization_copy_bytes_for_tag(stats, CLOSURE))) return err;
    if (Value* err = runtime_memory_dict_set_usz(out, interp, "materialization-copy-bytes-forced-no-splice-closure", boundary_decision_forced_no_splice_materialization_copy_bytes_for_tag(stats, CLOSURE))) return err;
    return out;
}

<*
@require interp != null
*>
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
    if (Value* err = runtime_memory_dict_set(out, interp, "escape-shape", runtime_memory_escape_shape_stats(interp))) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "boundary-decisions", runtime_memory_boundary_decision_stats(interp))) return err;
    if (Value* err = runtime_memory_dict_set(out, interp, "value-shape", runtime_memory_value_shape_stats(interp))) return err;
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
    BoundaryEscapeShapeStats shape_stats = boundary_escape_shape_stats_snapshot();
    BoundaryDecisionStats boundary_stats = boundary_decision_stats_snapshot();
    BoundaryValueShapeStats value_stats = boundary_value_shape_stats_snapshot();

    io::printn("{\"kind\":\"runtime-memory-stats\",\"status\":\"ok\",");
    io::printfn("\"scope\":{\"temp_bytes_allocated\":%d,\"escape_bytes_allocated\":%d,\"temp_chunk_bytes_allocated\":%d,\"escape_chunk_bytes_allocated\":%d,\"temp_bytes_freed\":%d,\"escape_bytes_freed\":%d,\"temp_slow_alloc_count\":%d,\"escape_slow_alloc_count\":%d,\"temp_slow_slack_exact_count\":%d,\"temp_slow_slack_le_512_count\":%d,\"temp_slow_slack_le_4096_count\":%d,\"temp_slow_slack_gt_4096_count\":%d,\"escape_slow_slack_exact_count\":%d,\"escape_slow_slack_le_512_count\":%d,\"escape_slow_slack_le_4096_count\":%d,\"escape_slow_slack_gt_4096_count\":%d,\"temp_slow_sequence_closed_count\":%d,\"escape_slow_sequence_closed_count\":%d,\"temp_slow_sequence_followup_bytes\":%d,\"escape_slow_sequence_followup_bytes\":%d,\"temp_slow_sequence_unused_at_close_bytes\":%d,\"escape_slow_sequence_unused_at_close_bytes\":%d,\"temp_slow_sequence_no_followup_count\":%d,\"escape_slow_sequence_no_followup_count\":%d,\"temp_slow_sequence_no_followup_request_le_512_count\":%d,\"temp_slow_sequence_no_followup_request_le_4096_count\":%d,\"temp_slow_sequence_no_followup_request_gt_4096_count\":%d,\"escape_slow_sequence_no_followup_request_le_512_count\":%d,\"escape_slow_sequence_no_followup_request_le_4096_count\":%d,\"escape_slow_sequence_no_followup_request_gt_4096_count\":%d,\"temp_slow_sequence_no_followup_unused_exact_count\":%d,\"temp_slow_sequence_no_followup_unused_le_512_count\":%d,\"temp_slow_sequence_no_followup_unused_le_4096_count\":%d,\"temp_slow_sequence_no_followup_unused_gt_4096_count\":%d,\"escape_slow_sequence_no_followup_unused_exact_count\":%d,\"escape_slow_sequence_no_followup_unused_le_512_count\":%d,\"escape_slow_sequence_no_followup_unused_le_4096_count\":%d,\"escape_slow_sequence_no_followup_unused_gt_4096_count\":%d,\"escape_slow_sequence_no_followup_source_unknown_count\":%d,\"escape_slow_sequence_no_followup_source_direct_count\":%d,\"escape_slow_sequence_no_followup_source_dtor_count\":%d,\"escape_slow_sequence_no_followup_source_interp_value_count\":%d,\"escape_slow_sequence_no_followup_source_interp_env_count\":%d,\"escape_slow_sequence_no_followup_source_boundary_payload_count\":%d,\"escape_slow_sequence_no_followup_source_promotion_signature_count\":%d,\"escape_slow_sequence_no_followup_source_promotion_closure_count\":%d,\"escape_slow_sequence_no_followup_source_jit_args_count\":%d,\"temp_slow_sequence_large_slack_count\":%d,\"escape_slow_sequence_large_slack_count\":%d,\"temp_slow_sequence_large_slack_followup_bytes\":%d,\"escape_slow_sequence_large_slack_followup_bytes\":%d,\"temp_slow_sequence_large_slack_unused_at_close_bytes\":%d,\"escape_slow_sequence_large_slack_unused_at_close_bytes\":%d,\"temp_slow_sequence_large_slack_no_followup_count\":%d,\"escape_slow_sequence_large_slack_no_followup_count\":%d,\"temp_peak_live_chunk_count\":%d,\"escape_peak_live_chunk_count\":%d,\"temp_unused_bytes_at_reset\":%d,\"escape_unused_bytes_at_reset\":%d,\"temp_unused_bytes_at_destroy\":%d,\"escape_unused_bytes_at_destroy\":%d,\"scope_create_count\":%d,\"scope_release_count\":%d,\"scope_destroy_count\":%d,\"scope_recycle_count\":%d,\"scope_fresh_alloc_count\":%d,\"scope_recycle_hit_count\":%d},",
        scope_stats.total_temp_bytes_allocated,
        scope_stats.total_escape_bytes_allocated,
        scope_stats.temp_chunk_bytes_allocated,
        scope_stats.escape_chunk_bytes_allocated,
        scope_stats.temp_bytes_freed,
        scope_stats.escape_bytes_freed,
        scope_stats.temp_slow_alloc_count,
        scope_stats.escape_slow_alloc_count,
        scope_stats.temp_slow_slack_exact_count,
        scope_stats.temp_slow_slack_le_512_count,
        scope_stats.temp_slow_slack_le_4096_count,
        scope_stats.temp_slow_slack_gt_4096_count,
        scope_stats.escape_slow_slack_exact_count,
        scope_stats.escape_slow_slack_le_512_count,
        scope_stats.escape_slow_slack_le_4096_count,
        scope_stats.escape_slow_slack_gt_4096_count,
        scope_stats.temp_slow_sequence_closed_count,
        scope_stats.escape_slow_sequence_closed_count,
        scope_stats.temp_slow_sequence_followup_bytes,
        scope_stats.escape_slow_sequence_followup_bytes,
        scope_stats.temp_slow_sequence_unused_at_close_bytes,
        scope_stats.escape_slow_sequence_unused_at_close_bytes,
        scope_stats.temp_slow_sequence_no_followup_count,
        scope_stats.escape_slow_sequence_no_followup_count,
        scope_stats.temp_slow_sequence_no_followup_request_le_512_count,
        scope_stats.temp_slow_sequence_no_followup_request_le_4096_count,
        scope_stats.temp_slow_sequence_no_followup_request_gt_4096_count,
        scope_stats.escape_slow_sequence_no_followup_request_le_512_count,
        scope_stats.escape_slow_sequence_no_followup_request_le_4096_count,
        scope_stats.escape_slow_sequence_no_followup_request_gt_4096_count,
        scope_stats.temp_slow_sequence_no_followup_unused_exact_count,
        scope_stats.temp_slow_sequence_no_followup_unused_le_512_count,
        scope_stats.temp_slow_sequence_no_followup_unused_le_4096_count,
        scope_stats.temp_slow_sequence_no_followup_unused_gt_4096_count,
        scope_stats.escape_slow_sequence_no_followup_unused_exact_count,
        scope_stats.escape_slow_sequence_no_followup_unused_le_512_count,
        scope_stats.escape_slow_sequence_no_followup_unused_le_4096_count,
        scope_stats.escape_slow_sequence_no_followup_unused_gt_4096_count,
        scope_stats.escape_slow_sequence_no_followup_source_unknown_count,
        scope_stats.escape_slow_sequence_no_followup_source_direct_count,
        scope_stats.escape_slow_sequence_no_followup_source_dtor_count,
        scope_stats.escape_slow_sequence_no_followup_source_interp_value_count,
        scope_stats.escape_slow_sequence_no_followup_source_interp_env_count,
        scope_stats.escape_slow_sequence_no_followup_source_boundary_payload_count,
        scope_stats.escape_slow_sequence_no_followup_source_promotion_signature_count,
        scope_stats.escape_slow_sequence_no_followup_source_promotion_closure_count,
        scope_stats.escape_slow_sequence_no_followup_source_jit_args_count,
        scope_stats.temp_slow_sequence_large_slack_count,
        scope_stats.escape_slow_sequence_large_slack_count,
        scope_stats.temp_slow_sequence_large_slack_followup_bytes,
        scope_stats.escape_slow_sequence_large_slack_followup_bytes,
        scope_stats.temp_slow_sequence_large_slack_unused_at_close_bytes,
        scope_stats.escape_slow_sequence_large_slack_unused_at_close_bytes,
        scope_stats.temp_slow_sequence_large_slack_no_followup_count,
        scope_stats.escape_slow_sequence_large_slack_no_followup_count,
        scope_stats.temp_peak_live_chunk_count,
        scope_stats.escape_peak_live_chunk_count,
        scope_stats.temp_unused_bytes_at_reset,
        scope_stats.escape_unused_bytes_at_reset,
        scope_stats.temp_unused_bytes_at_destroy,
        scope_stats.escape_unused_bytes_at_destroy,
        scope_stats.scope_create_count,
        scope_stats.scope_release_count,
        scope_stats.scope_destroy_count,
        scope_stats.scope_recycle_count,
        scope_stats.scope_fresh_alloc_count,
        scope_stats.scope_recycle_hit_count);
    io::printfn("\"scope_transfer\":{\"splice_calls\":%d,\"escape_bytes_spliced\":%d,\"temp_bytes_freed_by_splice\":%d},",
        transfer_stats.splice_calls,
        transfer_stats.splice_escape_chunk_bytes,
        transfer_stats.splice_temp_chunk_bytes_freed);
    io::printfn("\"fiber_temp\":{\"take_hits\":%d,\"take_misses\":%d,\"return_count\":%d,\"drop_frees\":%d,\"current_pool_count\":%d,\"take_hit_chunk_bytes\":%d,\"return_chunk_bytes\":%d,\"drop_free_chunk_bytes\":%d,\"ctx_take_hit_chunk_bytes\":%d,\"ctx_return_chunk_bytes\":%d},",
        fiber_stats.take_hits,
        fiber_stats.take_misses,
        fiber_stats.return_count,
        fiber_stats.drop_frees,
        main::g_fiber_temp_chunk_pool_count,
        fiber_stats.take_hit_chunk_bytes,
        fiber_stats.return_chunk_bytes,
        fiber_stats.drop_free_chunk_bytes,
        fiber_stats.ctx_take_hit_chunk_bytes,
        fiber_stats.ctx_return_chunk_bytes);
    io::printfn("\"copy_to_parent\":{\"total_calls\":%d,\"fast_reuse_hits\":%d,\"defensive_copies\":%d,\"copy_to_parent_bytes\":%d},",
        copy_stats.total_calls,
        copy_stats.fast_reuse_hits,
        copy_stats.defensive_copies,
        copy_stats.copy_bytes);
    io::printfn("\"escape_shape\":{\"candidate_roots\":%d,\"graph_roots\":%d,\"unsafe_roots\":%d,\"cons_roots\":%d,\"cons_spine_samples\":%d,\"cons_spine_total_len\":%d,\"cons_spine_peak_len\":%d,\"array_roots\":%d,\"hashmap_roots\":%d,\"set_roots\":%d,\"closure_roots\":%d,\"partial_roots\":%d,\"iterator_roots\":%d,\"continuation_roots\":%d,\"coroutine_roots\":%d,\"tensor_roots\":%d,\"instance_roots\":%d,\"method_table_roots\":%d,\"module_roots\":%d,\"type_info_roots\":%d,\"ffi_handle_roots\":%d},",
        shape_stats.candidate_roots,
        shape_stats.graph_roots,
        shape_stats.unsafe_roots,
        shape_stats.cons_roots,
        shape_stats.cons_spine_samples,
        shape_stats.cons_spine_total_len,
        shape_stats.cons_spine_peak_len,
        shape_stats.array_roots,
        shape_stats.hashmap_roots,
        shape_stats.set_roots,
        shape_stats.closure_roots,
        shape_stats.partial_roots,
        shape_stats.iterator_roots,
        shape_stats.continuation_roots,
        shape_stats.coroutine_roots,
        shape_stats.tensor_roots,
        shape_stats.instance_roots,
        shape_stats.method_table_roots,
        shape_stats.module_roots,
        shape_stats.type_info_roots,
        shape_stats.ffi_handle_roots);
    io::printfn("\"value_shape\":{\"array_construct_count\":%d,\"array_growth_count\":%d,\"hashmap_construct_count\":%d,\"hashmap_growth_count\":%d,\"set_construct_count\":%d,\"set_growth_count\":%d,\"closure_env_copy_frame_count\":%d,\"string_payload_bytes_total\":%d,\"error_payload_bytes_total\":%d,\"big_integer_wrapper_count\":%d,\"big_integer_decimal_input_bytes_total\":%d,\"tensor_payload_bytes_total\":%d,\"ffi_handle_wrapper_count\":%d,\"ffi_handle_releasable_count\":%d,\"stable_passport_owner_scope_invalid_count\":%d,\"stable_passport_owner_escape_changed_count\":%d,\"stable_passport_root_not_in_owner_chain_count\":%d,\"stable_passport_prepared_graph_stale_count\":%d},",
        value_stats.array_construct_count,
        value_stats.array_growth_count,
        value_stats.hashmap_construct_count,
        value_stats.hashmap_growth_count,
        value_stats.set_construct_count,
        value_stats.set_growth_count,
        value_stats.closure_env_copy_frame_count,
        value_stats.string_payload_bytes_total,
        value_stats.error_payload_bytes_total,
        value_stats.big_integer_wrapper_count,
        value_stats.big_integer_decimal_input_bytes_total,
        value_stats.tensor_payload_bytes_total,
        value_stats.ffi_handle_wrapper_count,
        value_stats.ffi_handle_releasable_count,
        value_stats.stable_passport_owner_scope_invalid_count,
        value_stats.stable_passport_owner_escape_changed_count,
        value_stats.stable_passport_root_not_in_owner_chain_count,
        value_stats.stable_passport_prepared_graph_stale_count);
    io::printfn("\"boundary_decisions\":{\"planned_stable_publish\":%d,\"planned_stable_materialize\":%d,\"planned_transplant\":%d,\"selected_stable_publish\":%d,\"selected_stable_materialize\":%d,\"selected_transplant\":%d,\"selected_fail_closed\":%d,\"route_fail_transplant_rejected\":%d,\"materialization_node_count\":%d,\"materialization_copy_bytes\":%d,\"materialization_copy_bytes_optimizer\":%d,\"materialization_copy_bytes_forced_no_splice\":%d,\"selected_stable_materialize_cons\":%d,\"selected_stable_materialize_array\":%d,\"selected_stable_materialize_hashmap\":%d,\"selected_stable_materialize_set\":%d,\"selected_stable_materialize_closure\":%d,\"selected_stable_materialize_big_integer\":%d,\"materialization_copy_bytes_cons\":%d,\"materialization_copy_bytes_array\":%d,\"materialization_copy_bytes_hashmap\":%d,\"materialization_copy_bytes_set\":%d,\"materialization_copy_bytes_closure\":%d,\"materialization_copy_bytes_big_integer\":%d,\"materialization_copy_bytes_optimizer_closure\":%d,\"materialization_copy_bytes_forced_no_splice_closure\":%d}}",
        (long)boundary_stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE],
        (long)boundary_stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
        (long)boundary_stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
        (long)boundary_stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE],
        (long)boundary_stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
        (long)boundary_stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
        (long)boundary_stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_FAIL_CLOSED],
        (long)boundary_stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_TRANSPLANT_REJECTED],
        (long)boundary_stats.stable_materialization_node_count_total,
        (long)boundary_stats.stable_materialization_copy_bytes_total,
        (long)boundary_decision_optimizer_materialization_copy_bytes(boundary_stats),
        (long)boundary_stats.forced_no_splice_materialization_copy_bytes_total,
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CONS),
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, ARRAY),
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, HASHMAP),
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, SET),
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CLOSURE),
        (long)boundary_decision_selected_route_tag_count(boundary_stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, BIG_INTEGER),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, CONS),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, ARRAY),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, HASHMAP),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, SET),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, CLOSURE),
        (long)boundary_decision_materialization_copy_bytes_for_tag(boundary_stats, BIG_INTEGER),
        (long)boundary_decision_optimizer_materialization_copy_bytes_for_tag(boundary_stats, CLOSURE),
        (long)boundary_decision_forced_no_splice_materialization_copy_bytes_for_tag(boundary_stats, CLOSURE));
}
