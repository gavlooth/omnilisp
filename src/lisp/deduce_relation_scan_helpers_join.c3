module lisp;

import std::core::mem;

fn bool deduce_hash_join_count(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols,
    Value*[] inner_static_vals,
    long* join_count_out,
    DeduceJoinExecStats* stats_out,
    Interp* interp
) {
    if (join_count_out == null || interp == null) return false;
    *join_count_out = 0;
    if (stats_out != null) {
        *stats_out = {};
        stats_out.used_hash = false;
        stats_out.used_index = false;
        stats_out.fallback_applied = false;
    }

    if (!deduce_join_validate_column_map(outer_rel, inner_rel, outer_join_cols, inner_join_cols)) {
        return false;
    }
    if (inner_static_vals.len != 0 && inner_static_vals.len != inner_rel.col_count) {
        return false;
    }

    Value*[] inner_key_vals = {};
    if (inner_join_cols.len > 0) {
        inner_key_vals = mem::new_array(Value*, inner_join_cols.len);
        if (inner_key_vals.ptr == null) return false;
    }
    defer if (inner_key_vals.ptr != null) mem::free(inner_key_vals.ptr);

    Value*[] outer_key_vals = {};
    if (outer_join_cols.len > 0) {
        outer_key_vals = mem::new_array(Value*, outer_join_cols.len);
        if (outer_key_vals.ptr == null) return false;
    }
    defer if (outer_key_vals.ptr != null) mem::free(outer_key_vals.ptr);

    Value* key_count_map = make_hashmap_checked(interp, 128, "deduce/analyze: out of memory");
    if (key_count_map == null || key_count_map.tag != HASHMAP || key_count_map.hashmap_val == null) return false;

    DeduceTupleIterator inner_it = {};
    if (!deduce_tuple_iterator_open(&inner_it, inner_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    bool inner_it_open = true;
    defer if (inner_it_open) deduce_tuple_iterator_close(&inner_it);

    while (deduce_tuple_iterator_next(&inner_it)) {
        if (stats_out != null) stats_out.inner_rows++;
        if (!deduce_join_inner_row_matches_static_values(inner_it.vals, inner_rel.col_count, inner_static_vals)) {
            continue;
        }
        if (!deduce_join_extract_key_values(inner_it.vals, inner_rel.col_count, inner_join_cols, inner_key_vals)) {
            return false;
        }
        char[4096] key_buf;
        usz? key_len = encode_tuple(inner_key_vals, key_buf[..], key_buf.len);
        if (catch err = key_len) return false;

        Value* key_val = make_string(interp, key_buf[:key_len]);
        Value* existing = hashmap_get(key_count_map.hashmap_val, key_val);
        long next_count = existing != null && existing.tag == INT ? existing.int_val + 1 : 1;
        if (!hashmap_set_checked(key_count_map.hashmap_val, key_val, make_int(interp, next_count), interp)) return false;
    }
    deduce_tuple_iterator_close(&inner_it);
    inner_it_open = false;

    DeduceTupleIterator outer_it = {};
    if (!deduce_tuple_iterator_open(&outer_it, outer_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    defer deduce_tuple_iterator_close(&outer_it);

    while (deduce_tuple_iterator_next(&outer_it)) {
        if (stats_out != null) stats_out.outer_rows++;
        if (!deduce_join_extract_key_values(outer_it.vals, outer_rel.col_count, outer_join_cols, outer_key_vals)) {
            return false;
        }
        char[4096] key_buf;
        usz? key_len = encode_tuple(outer_key_vals, key_buf[..], key_buf.len);
        if (catch err = key_len) return false;
        if (stats_out != null) stats_out.inner_probes++;
        Value* key_val = make_string(interp, key_buf[:key_len]);
        Value* count_val = hashmap_get(key_count_map.hashmap_val, key_val);
        long matched = count_val != null && count_val.tag == INT ? count_val.int_val : 0;
        if (matched > 0) {
            *join_count_out = *join_count_out + matched;
            if (stats_out != null) stats_out.rows_emitted += matched;
        }
    }
    if (stats_out != null) stats_out.used_hash = true;

    return true;
}

fn bool deduce_adaptive_join_count(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols,
    Value*[] inner_static_vals,
    long fallback_probe_threshold,
    long fallback_miss_ratio_percent,
    long* join_count_out,
    DeduceJoinExecStats* stats_out,
    Interp* interp
) {
    if (join_count_out == null || interp == null) return false;
    bool explicit_policy = fallback_probe_threshold > 0 || fallback_miss_ratio_percent > 0;
    bool use_index_first = explicit_policy || deduce_join_should_use_index_first(
        outer_rel,
        inner_rel,
        inner_join_cols,
        inner_static_vals
    );
    long probe_threshold = fallback_probe_threshold > 0 ? fallback_probe_threshold : DEDUCE_JOIN_ADAPTIVE_DEFAULT_PROBE_THRESHOLD;
    long miss_ratio_percent = fallback_miss_ratio_percent > 0 ? fallback_miss_ratio_percent : DEDUCE_JOIN_ADAPTIVE_DEFAULT_MISS_RATIO_PERCENT;

    if (!use_index_first) {
        long hash_count = 0;
        DeduceJoinExecStats hash_stats = {};
        bool hash_ok = deduce_hash_join_count(
            outer_rel,
            inner_rel,
            outer_join_cols,
            inner_join_cols,
            inner_static_vals,
            &hash_count,
            &hash_stats,
            interp
        );
        if (!hash_ok) return false;

        *join_count_out = hash_count;
        if (stats_out != null) *stats_out = hash_stats;
        return true;
    }

    long index_count = 0;
    DeduceJoinExecStats index_stats = {};
    bool index_ok = deduce_index_nested_loop_join_count(
        outer_rel,
        inner_rel,
        outer_join_cols,
        inner_join_cols,
        inner_static_vals,
        &index_count,
        &index_stats,
        interp
    );

    bool should_fallback = false;
    if (!index_ok) {
        should_fallback = true;
    } else if (index_stats.inner_probes >= probe_threshold) {
        should_fallback = (index_stats.index_misses * 100) >= (index_stats.inner_probes * miss_ratio_percent);
    }

    if (!should_fallback) {
        *join_count_out = index_count;
        if (stats_out != null) *stats_out = index_stats;
        return index_ok;
    }

    long hash_count = 0;
    DeduceJoinExecStats hash_stats = {};
    bool hash_ok = deduce_hash_join_count(
        outer_rel,
        inner_rel,
        outer_join_cols,
        inner_join_cols,
        inner_static_vals,
        &hash_count,
        &hash_stats,
        interp
    );
    if (!hash_ok) return false;

    hash_stats.fallback_applied = true;
    *join_count_out = hash_count;
    if (stats_out != null) *stats_out = hash_stats;
    return true;
}
