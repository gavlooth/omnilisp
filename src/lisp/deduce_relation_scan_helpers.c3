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

// Test hooks for read-path failure propagation. Keep them local to Deduce so
// regressions can force LMDB-like failures without corrupting real environments.
bool g_deduce_force_read_txn_begin_fail = false;
bool g_deduce_force_read_cursor_open_fail = false;
bool g_deduce_force_read_value_array_alloc_fail = false;
int g_deduce_force_read_cursor_get_fail_after = -1;

struct DeduceTupleIterator {
    Relation* rel;
    Interp*   interp;
    MdbTxn*   txn;
    bool      owns_txn;
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
    bool      eof;
    Value*    error;
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

fn int deduce_read_cursor_get(MdbCursor* cursor, MdbVal* key, MdbVal* data, int op) @inline {
    if (g_deduce_force_read_cursor_get_fail_after == 0) return -1;
    if (g_deduce_force_read_cursor_get_fail_after > 0) g_deduce_force_read_cursor_get_fail_after--;
    return mdb_cursor_get(cursor, key, data, op);
}

fn Value* deduce_tuple_iterator_set_error(DeduceTupleIterator* it, char[] code, char[] message) {
    Interp* interp = it != null ? it.interp : null;
    if (interp == null) return null;
    Value* err = deduce_raise(interp, code, message);
    it.error = err;
    return err;
}

fn Value* deduce_tuple_iterator_error(DeduceTupleIterator* it) @inline {
    return it != null ? it.error : null;
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
    if (it == null) return false;
    *it = {};
    it.rel = rel;
    it.interp = interp;
    if (rel == null || interp == null) {
        deduce_tuple_iterator_set_error(it, "deduce/scan-invalid-state", "deduce-scan: invalid iterator state");
        return false;
    }
    if (rel.db == null || !rel.db.open) {
        deduce_tuple_iterator_set_error(it, "deduce/scan-database-not-open", "deduce-scan: database not open");
        return false;
    }
    it.start_key = start_key;
    it.start_key_len = start_key_len;
    it.has_start = has_start;
    it.lower_bound = lower_bound;
    it.upper_bound = upper_bound;
    it.has_end = has_end;

    if (rel.col_count > 0) {
        if (g_deduce_force_read_value_array_alloc_fail) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-out-of-memory", "deduce-scan: out of memory");
            return false;
        }
        it.vals = mem::new_array(Value*, rel.col_count);
        if (it.vals.ptr == null) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-out-of-memory", "deduce-scan: out of memory");
            return false;
        }
    }

    int rc = g_deduce_force_read_txn_begin_fail ? -1 : mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &it.txn);
    if (rc != MDB_SUCCESS || it.txn == null) {
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        deduce_tuple_iterator_set_error(it, "deduce/scan-transaction-failed", "deduce-scan: transaction open failed");
        return false;
    }

    rc = g_deduce_force_read_cursor_open_fail ? -1 : mdb_cursor_open(it.txn, rel.dbi, &it.cursor);
    if (rc != MDB_SUCCESS || it.cursor == null) {
        mdb_txn_abort(it.txn);
        it.txn = null;
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        deduce_tuple_iterator_set_error(it, "deduce/scan-cursor-open-failed", "deduce-scan: cursor open failed");
        return false;
    }

    it.started = false;
    it.open = true;
    it.owns_txn = true;
    return true;
}

fn bool deduce_tuple_iterator_open_in_txn(
    DeduceTupleIterator* it,
    Relation* rel,
    Interp* interp,
    MdbTxn* txn,
    char* start_key,
    usz start_key_len,
    bool has_start,
    Value*[] lower_bound,
    Value*[] upper_bound,
    bool has_end
) {
    if (it == null) return false;
    *it = {};
    it.rel = rel;
    it.interp = interp;
    it.txn = txn;
    it.owns_txn = false;
    if (rel == null || interp == null || txn == null) {
        deduce_tuple_iterator_set_error(it, "deduce/scan-invalid-state", "deduce-scan: invalid iterator state");
        return false;
    }
    if (rel.db == null || !rel.db.open) {
        deduce_tuple_iterator_set_error(it, "deduce/scan-database-not-open", "deduce-scan: database not open");
        return false;
    }
    it.start_key = start_key;
    it.start_key_len = start_key_len;
    it.has_start = has_start;
    it.lower_bound = lower_bound;
    it.upper_bound = upper_bound;
    it.has_end = has_end;

    if (rel.col_count > 0) {
        if (g_deduce_force_read_value_array_alloc_fail) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-out-of-memory", "deduce-scan: out of memory");
            return false;
        }
        it.vals = mem::new_array(Value*, rel.col_count);
        if (it.vals.ptr == null) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-out-of-memory", "deduce-scan: out of memory");
            return false;
        }
    }

    int rc = g_deduce_force_read_cursor_open_fail ? -1 : mdb_cursor_open(txn, rel.dbi, &it.cursor);
    if (rc != MDB_SUCCESS || it.cursor == null) {
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        deduce_tuple_iterator_set_error(it, "deduce/scan-cursor-open-failed", "deduce-scan: cursor open failed");
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
    if (it.txn != null && it.owns_txn) {
        mdb_txn_abort(it.txn);
    }
    it.txn = null;
    if (it.vals.ptr != null) {
        mem::free(it.vals.ptr);
        it.vals = {};
    }
    it.open = false;
}

fn bool deduce_tuple_iterator_next(DeduceTupleIterator* it) {
    if (it == null) return false;
    if (!it.open || it.cursor == null || it.rel == null) {
        deduce_tuple_iterator_set_error(it, "deduce/scan-invalid-state", "deduce-scan: invalid iterator state");
        return false;
    }

    int rc = MDB_NOTFOUND;
    if (!it.started) {
        it.started = true;
        if (it.has_start) {
            it.key.mv_data = it.start_key;
            it.key.mv_size = it.start_key_len;
            rc = deduce_read_cursor_get(it.cursor, &it.key, &it.data, MDB_SET_RANGE);
        } else {
            rc = deduce_read_cursor_get(it.cursor, &it.key, &it.data, MDB_FIRST);
        }
    } else {
        rc = deduce_read_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
    }

    while (rc == MDB_SUCCESS) {
        usz? count_opt = decode_tuple((char*)it.key.mv_data, it.key.mv_size, it.vals, it.interp);
        if (catch err = count_opt) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-tuple-decode-failed", "deduce-scan: tuple decode failed");
            return false;
        }
        if (count_opt != it.rel.col_count) {
            deduce_tuple_iterator_set_error(it, "deduce/scan-tuple-arity-mismatch", "deduce-scan: tuple arity mismatch");
            return false;
        }

        if (it.has_start && deduce_compare_tuples(it.vals, it.lower_bound, it.rel.col_count) < 0) {
            rc = deduce_read_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        if (it.has_end && deduce_compare_tuples(it.vals, it.upper_bound, it.rel.col_count) > 0) {
            rc = deduce_read_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        return true;
    }
    if (rc == MDB_NOTFOUND) {
        it.eof = true;
        return false;
    }
    deduce_tuple_iterator_set_error(it, "deduce/scan-cursor-read-failed", "deduce-scan: cursor read failed");
    return false;
}
