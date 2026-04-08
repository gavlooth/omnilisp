module lisp;

import std::core::mem;

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
