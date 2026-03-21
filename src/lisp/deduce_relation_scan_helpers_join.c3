module lisp;

import std::core::mem;

struct DeduceJoinExecStats {
    bool used_index;
    bool used_hash;
    bool fallback_applied;
    long outer_rows;
    long inner_rows;
    long inner_probes;
    long index_hits;
    long index_misses;
    long rows_emitted;
}

const long DEDUCE_JOIN_ADAPTIVE_DEFAULT_PROBE_THRESHOLD = 32;
const long DEDUCE_JOIN_ADAPTIVE_DEFAULT_MISS_RATIO_PERCENT = 70;
const long DEDUCE_JOIN_ADAPTIVE_SMALL_OUTER_ROWS = 16;

fn bool deduce_join_validate_column_map(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols
) {
    if (outer_rel == null || inner_rel == null) return false;
    if (outer_join_cols.len == 0) return false;
    if (outer_join_cols.len != inner_join_cols.len) return false;
    for (usz i = 0; i < outer_join_cols.len; i++) {
        if (outer_join_cols[i] >= outer_rel.col_count) return false;
        if (inner_join_cols[i] >= inner_rel.col_count) return false;
    }
    return true;
}

fn bool deduce_join_all_bound(Value*[] vals) {
    for (usz i = 0; i < vals.len; i++) {
        if (vals[i] == null) return false;
    }
    return true;
}

fn bool deduce_join_probe_inner_exact(
    MdbTxn* inner_txn,
    Relation* inner_rel,
    Value*[] probe_vals
) {
    if (inner_txn == null || inner_rel == null) return false;
    char[4096] key_buf;
    usz? key_len = encode_tuple(probe_vals, key_buf[..], key_buf.len);
    if (catch err = key_len) return false;

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf[0];
    MdbVal data = {};
    return mdb_get(inner_txn, inner_rel.dbi, &key, &data) == MDB_SUCCESS;
}

fn bool deduce_join_extract_key_values(Value*[] tuple_vals, usz tuple_col_count, usz[] join_cols, Value*[] key_vals) {
    if (join_cols.len != key_vals.len) return false;
    for (usz i = 0; i < join_cols.len; i++) {
        usz col = join_cols[i];
        if (col >= tuple_col_count || col >= tuple_vals.len) return false;
        Value* v = tuple_vals[col];
        if (v == null) return false;
        key_vals[i] = v;
    }
    return true;
}

fn bool deduce_join_inner_row_matches_static_values(Value*[] tuple_vals, usz tuple_col_count, Value*[] static_vals) {
    if (static_vals.len == 0) return true;
    if (static_vals.len != tuple_col_count || tuple_vals.len < tuple_col_count) return false;
    for (usz i = 0; i < tuple_col_count; i++) {
        Value* expected = static_vals[i];
        if (expected == null) continue;
        if (!values_equal(tuple_vals[i], expected)) return false;
    }
    return true;
}

fn ulong deduce_join_shape_bound_mask(usz rel_col_count, usz[] join_cols, Value*[] static_vals) {
    ulong mask = 0;
    usz width = (usz)(ulong.sizeof * 8);
    for (usz i = 0; i < join_cols.len; i++) {
        usz col = join_cols[i];
        if (col < width) mask |= ((ulong)1 << col);
    }
    if (static_vals.len == rel_col_count) {
        for (usz i = 0; i < rel_col_count; i++) {
            if (static_vals[i] != null && i < width) {
                mask |= ((ulong)1 << i);
            }
        }
    }
    return mask;
}

fn bool deduce_join_index_fully_bound(DeduceIndexDescriptor* idx, ulong bound_mask) {
    if (idx == null || idx.column_positions == null || idx.column_count == 0) return false;
    for (usz c = 0; c < idx.column_count; c++) {
        if (!deduce_index_column_bound(bound_mask, idx.column_positions[c])) return false;
    }
    return true;
}

fn bool deduce_join_should_use_index_first(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] inner_join_cols,
    Value*[] inner_static_vals
) {
    if (inner_rel == null || inner_rel.db == null) return true;

    DeduceRelationSchema* inner_schema = deduce_db_lookup_relation_schema(inner_rel.db, inner_rel.name);
    if (inner_schema == null) return true;

    ulong bound_mask = deduce_join_shape_bound_mask(inner_rel.col_count, inner_join_cols, inner_static_vals);
    int selected_index = deduce_schema_select_index_for_bound_mask(inner_schema, bound_mask);
    if (selected_index < 0) return false;

    DeduceIndexDescriptor* selected = &inner_schema.indexes[selected_index];
    if (selected != null && selected.unique && deduce_join_index_fully_bound(selected, bound_mask)) return true;

    if (outer_rel != null && outer_rel.db != null) {
        DeduceRelationSchema* outer_schema = deduce_db_lookup_relation_schema(outer_rel.db, outer_rel.name);
        if (outer_schema != null && outer_schema.cardinality_estimate > 0 &&
            outer_schema.cardinality_estimate <= (ulong)DEDUCE_JOIN_ADAPTIVE_SMALL_OUTER_ROWS) {
            return true;
        }
    }

    return false;
}

fn bool deduce_index_nested_loop_join_count(
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
        stats_out.used_index = false;
        stats_out.used_hash = false;
        stats_out.fallback_applied = false;
    }

    if (!deduce_join_validate_column_map(outer_rel, inner_rel, outer_join_cols, inner_join_cols)) {
        return false;
    }
    if (inner_static_vals.len != 0 && inner_static_vals.len != inner_rel.col_count) {
        return false;
    }

    Value*[] probe_vals = {};
    if (inner_rel.col_count > 0) {
        probe_vals = mem::new_array(Value*, inner_rel.col_count);
        if (probe_vals.ptr == null) return false;
    }
    defer if (probe_vals.ptr != null) mem::free(probe_vals.ptr);

    DeduceTupleIterator outer_it = {};
    if (!deduce_tuple_iterator_open(&outer_it, outer_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    defer deduce_tuple_iterator_close(&outer_it);

    MdbTxn* inner_txn = outer_it.txn;
    bool owns_inner_txn = false;
    if (outer_rel.db.env != inner_rel.db.env) {
        if (mdb_txn_begin(inner_rel.db.env, null, MDB_RDONLY, &inner_txn) != MDB_SUCCESS || inner_txn == null) {
            return false;
        }
        owns_inner_txn = true;
    }
    defer if (owns_inner_txn && inner_txn != null) mdb_txn_abort(inner_txn);

    if (stats_out != null) stats_out.used_index = true;

    while (deduce_tuple_iterator_next(&outer_it)) {
        if (stats_out != null) stats_out.outer_rows++;

        for (usz i = 0; i < probe_vals.len; i++) {
            probe_vals[i] = inner_static_vals.len == inner_rel.col_count ? inner_static_vals[i] : null;
        }
        for (usz i = 0; i < outer_join_cols.len; i++) {
            usz outer_col = outer_join_cols[i];
            usz inner_col = inner_join_cols[i];
            probe_vals[inner_col] = outer_it.vals[outer_col];
        }

        if (!deduce_join_all_bound(probe_vals)) {
            return false;
        }

        if (stats_out != null) stats_out.inner_probes++;
        if (deduce_join_probe_inner_exact(inner_txn, inner_rel, probe_vals)) {
            *join_count_out = *join_count_out + 1;
            if (stats_out != null) {
                stats_out.index_hits++;
                stats_out.rows_emitted++;
            }
        } else if (stats_out != null) {
            stats_out.index_misses++;
        }
    }

    return true;
}

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

    Value* key_count_map = make_hashmap(interp, 128);
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
        hashmap_set(key_count_map.hashmap_val, key_val, make_int(interp, next_count), interp);
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
