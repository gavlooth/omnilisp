module lisp;

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
