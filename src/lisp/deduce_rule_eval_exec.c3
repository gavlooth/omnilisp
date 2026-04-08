module lisp;

import std::core::mem;

fn Value* deduce_naive_insert_head_tuple(DeduceNaiveRuleExec* exec) {
    if (exec == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }
    if (exec.dry_run) {
        return null;
    }

    MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    Value* integrity_err = deduce_assert_schema_derived_write_integrity(
        exec.db,
        exec.head_schema,
        exec.txn,
        exec.head_dbi,
        exec.head_tuple,
        &key,
        exec.interp
    );
    if (integrity_err != null) return integrity_err;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }
    return null;
}

fn Value* deduce_validate_component_ref_head_snapshots(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    Interp* interp
) {
    if (db == null || plan == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid constrained head validation state");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
        if (schema == null || schema.reference_constraint_count == 0) continue;

        MdbDbi dbi = 0;
        if (!deduce_open_relation_dbi_by_symbol(db, txn, predicate, &dbi, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found",
                "deduce/analyze: head predicate relation is not registered");
        }

        Value* err = deduce_validate_schema_fact_integrity_snapshot(db, schema, txn, dbi, interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_assert_encoded_head_tuple_integrity(
    DeduceDb* db,
    DeduceRelationSchema* head_schema,
    MdbTxn* txn,
    MdbDbi head_dbi,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (db == null || head_schema == null || txn == null || interp == null) return null;
    if (deduce_relation_schema_constraint_count(head_schema) == 0) return null;

    Value*[] decoded = {};
    if (head_schema.arity > 0) {
        decoded = mem::new_array(Value*, head_schema.arity);
        if (decoded.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (decoded.ptr != null) mem::free(decoded.ptr);

    char empty_key = 0;
    char* key_bytes = key_len > 0 ? (char*)key_data : &empty_key;
    usz? decoded_count = decode_tuple(key_bytes, key_len, decoded, interp);
    if (catch err = decoded_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: failed to decode derived head tuple for integrity validation");
    }
    if (decoded_count != head_schema.arity) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: decoded derived head tuple arity mismatch");
    }

    MdbVal key = { .mv_size = key_len, .mv_data = key_bytes };
    return deduce_assert_schema_derived_write_integrity(db, head_schema, txn, head_dbi, decoded, &key, interp);
}

fn Value* deduce_naive_insert_encoded_head_tuple(
    DeduceNaiveRuleExec* exec,
    char* key_bytes,
    usz key_len
) {
    if (exec == null || exec.txn == null || exec.interp == null || (key_bytes == null && key_len > 0)) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }

    char empty_key = 0;
    MdbVal key = { .mv_size = key_len, .mv_data = key_len > 0 ? key_bytes : &empty_key };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
        exec.db,
        exec.head_schema,
        exec.txn,
        exec.head_dbi,
        key.mv_data,
        key_len,
        exec.interp
    );
    if (integrity_err != null) return integrity_err;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }
    return null;
}

fn Value* deduce_naive_emit_head_fact(
    DeduceNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (exec.sig.head_term_count != exec.head_tuple.len) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid head term metadata");
    }
    if (exec.sig.head_term_is_variable == null || exec.sig.head_term_var_ids == null || exec.sig.head_term_literals == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing head term metadata");
    }
    if (deduce_rule_signature_has_aggregates(exec.sig)) {
        return deduce_naive_accumulate_aggregate_binding(exec, var_bound, var_values);
    }

    for (usz i = 0; i < exec.sig.head_term_count; i++) {
        if (exec.sig.head_term_is_variable[i]) {
            uint var_id = exec.sig.head_term_var_ids[i];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
                return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound head variable at runtime");
            }
            exec.head_tuple[i] = var_values[var_id];
        } else {
            exec.head_tuple[i] = exec.sig.head_term_literals[i];
        }
    }
    return deduce_naive_insert_head_tuple(exec);
}
