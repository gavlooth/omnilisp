module lisp;

import std::core::mem;

struct DeduceScanRangeBenchStats {
    bool enabled;
    usz calls;
    usz rows_materialized;
    usz key_symbols_allocated;
    long materialize_ms;
}

DeduceScanRangeBenchStats deduce_scan_range_bench_stats;

struct DeduceTupleIterator {
    Relation* rel;
    Interp*   interp;
    MdbTxn*   txn;
    MdbCursor* cursor;
    MdbVal    key;
    MdbVal    data;
    Value*[]  vals;
    Value*[]  lower_bound;
    Value*[]  upper_bound;
    char*     start_key;
    usz       start_key_len;
    bool      has_start;
    bool      has_end;
    bool      started;
    bool      open;
}

fn void deduce_scan_range_bench_reset() {
    deduce_scan_range_bench_stats.calls = 0;
    deduce_scan_range_bench_stats.rows_materialized = 0;
    deduce_scan_range_bench_stats.key_symbols_allocated = 0;
    deduce_scan_range_bench_stats.materialize_ms = 0;
}

fn void deduce_scan_range_bench_set_enabled(bool enabled) {
    deduce_scan_range_bench_stats.enabled = enabled;
    deduce_scan_range_bench_reset();
}

fn DeduceScanRangeBenchStats deduce_scan_range_bench_snapshot() {
    return deduce_scan_range_bench_stats;
}

fn bool deduce_tuple_iterator_open(
    DeduceTupleIterator* it,
    Relation* rel,
    Interp* interp,
    char* start_key,
    usz start_key_len,
    bool has_start,
    Value*[] lower_bound,
    Value*[] upper_bound,
    bool has_end
) {
    if (it == null || rel == null || interp == null) return false;
    if (rel.db == null || !rel.db.open) return false;

    *it = {};
    it.rel = rel;
    it.interp = interp;
    it.start_key = start_key;
    it.start_key_len = start_key_len;
    it.has_start = has_start;
    it.lower_bound = lower_bound;
    it.upper_bound = upper_bound;
    it.has_end = has_end;

    if (rel.col_count > 0) {
        it.vals = mem::new_array(Value*, rel.col_count);
        if (it.vals.ptr == null) return false;
    }

    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &it.txn);
    if (rc != MDB_SUCCESS || it.txn == null) {
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        return false;
    }

    rc = mdb_cursor_open(it.txn, rel.dbi, &it.cursor);
    if (rc != MDB_SUCCESS || it.cursor == null) {
        mdb_txn_abort(it.txn);
        it.txn = null;
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        return false;
    }

    it.started = false;
    it.open = true;
    return true;
}

fn void deduce_tuple_iterator_close(DeduceTupleIterator* it) {
    if (it == null) return;
    if (it.cursor != null) {
        mdb_cursor_close(it.cursor);
        it.cursor = null;
    }
    if (it.txn != null) {
        mdb_txn_abort(it.txn);
        it.txn = null;
    }
    if (it.vals.ptr != null) {
        mem::free(it.vals.ptr);
        it.vals = {};
    }
    it.open = false;
}

fn bool deduce_tuple_iterator_next(DeduceTupleIterator* it) {
    if (it == null || !it.open || it.cursor == null || it.rel == null) return false;

    int rc = MDB_NOTFOUND;
    if (!it.started) {
        it.started = true;
        if (it.has_start) {
            it.key.mv_data = it.start_key;
            it.key.mv_size = it.start_key_len;
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_SET_RANGE);
        } else {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_FIRST);
        }
    } else {
        rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
    }

    while (rc == MDB_SUCCESS) {
        usz? count_opt = decode_tuple((char*)it.key.mv_data, it.key.mv_size, it.vals, it.interp);
        if (catch err = count_opt) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        if (count_opt != it.rel.col_count) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }

        if (it.has_start && deduce_compare_tuples(it.vals, it.lower_bound, it.rel.col_count) < 0) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        if (it.has_end && deduce_compare_tuples(it.vals, it.upper_bound, it.rel.col_count) > 0) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        return true;
    }
    return false;
}
