module lisp;

import std::core::mem;

fn Value* deduce_seminaive_build_proof_key(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values,
    Value** key_out
) {
    if (exec == null || exec.interp == null || key_out == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp,
                "deduce/analyze-invalid-state",
                "deduce/analyze: invalid proof key state");
        }
        return null;
    }

    if (var_bound.len > (usz.max - (usz)1) / (usz)2) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: proof key is too large");
    }
    usz proof_value_count = (usz)1 + var_bound.len * (usz)2;
    Value*[] proof_values = mem::new_array(Value*, proof_value_count);
    if (proof_values.ptr == null && proof_value_count > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (proof_values.ptr != null) mem::free(proof_values.ptr);

    proof_values[0] = make_symbol(
        exec.interp,
        exec.anchor_delta_is_removal
            ? exec.interp.symbols.intern("proof-remove")
            : exec.interp.symbols.intern("proof-add")
    );
    usz proof_idx = 1;
    for (usz i = 0; i < var_bound.len; i++) {
        proof_values[proof_idx++] = make_int(exec.interp, var_bound[i] ? 1 : 0);
        proof_values[proof_idx++] =
            i < var_values.len && var_bound[i] ? var_values[i] : make_nil(exec.interp);
    }

    return deduce_encode_value_vector_to_owned_string(proof_values, key_out, exec.interp);
}

fn Value* deduce_seminaive_mark_current_proof_seen(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values,
    bool* already_seen_out
) {
    if (already_seen_out == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp,
                "deduce/analyze-invalid-state",
                "deduce/analyze: invalid proof dedupe target");
        }
        return null;
    }
    *already_seen_out = false;
    if (exec == null || exec.interp == null || !exec.aggregate_component_mode) return null;

    if (exec.proof_seen_map == null) {
        exec.proof_seen_map = make_hashmap_checked(exec.interp, 64, "deduce/analyze: out of memory");
        if (exec.proof_seen_map == null || exec.proof_seen_map.tag != HASHMAP || exec.proof_seen_map.hashmap_val == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }

    Value* proof_key = null;
    Value* err = deduce_seminaive_build_proof_key(exec, var_bound, var_values, &proof_key);
    if (err != null) return err;

    if (hashmap_get(exec.proof_seen_map.hashmap_val, proof_key) != null) {
        *already_seen_out = true;
        return null;
    }

    if (!hashmap_set_checked(
            exec.proof_seen_map.hashmap_val,
            proof_key,
            make_symbol(exec.interp, exec.interp.sym_true),
            exec.interp
        )) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    return null;
}

fn Value* deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len,
    bool delta_is_removal
) {
    if (exec == null || exec.plan == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid supported head state");
        }
        return null;
    }
    if (exec.support_tables.len == 0 || !exec.aggregate_component_mode) return null;

    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.support_tables.len ||
        (usz)pred_idx >= exec.plan.predicate_component.len ||
        (usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid supported head predicate");
    }

    bool became_present = false;
    bool became_absent = false;
    long delta = delta_is_removal ? -1 : 1;
    Value* err = deduce_tuple_support_table_adjust_recursive_count(
        &exec.support_tables[(usz)pred_idx],
        key_data,
        key_len,
        delta,
        null,
        &became_present,
        &became_absent,
        exec.interp
    );
    if (err != null) return err;
    if (!became_present && !became_absent) return null;

    char empty_key = 0;
    MdbVal key = { .mv_size = key_len, .mv_data = key_len > 0 ? key_data : &empty_key };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    if (became_present) {
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
        if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) != MDB_SUCCESS) {
            int put_rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
            if (put_rc != MDB_SUCCESS) {
                return deduce_raise(exec.interp, "deduce/analyze-put-failed",
                    "deduce/analyze: failed to insert supported derived tuple");
            }
        }
        if (exec.changed_out != null) *exec.changed_out = true;
        if (exec.derived_out != null && *exec.derived_out < long.max) *exec.derived_out = *exec.derived_out + 1;
        if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
            exec.head_schema.cardinality_estimate++;
            if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
                exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
            }
        }
        return deduce_seminaive_record_new_delta_tuple(exec, predicate, key_data, key_len);
    }

    int del_rc = mdb_del(exec.txn, exec.head_dbi, &key, null);
    if (del_rc != MDB_SUCCESS && del_rc != MDB_NOTFOUND) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed",
            "deduce/analyze: failed to retract supported derived tuple");
    }
    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null && *exec.derived_out < long.max) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate > 0) {
        exec.head_schema.cardinality_estimate--;
        exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
    }
    return deduce_seminaive_record_removed_delta_tuple(exec, predicate, key_data, key_len);
}

fn Value* deduce_seminaive_apply_supported_encoded_head_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    return deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
        exec,
        predicate,
        key_data,
        key_len,
        exec != null && exec.anchor_delta_is_removal
    );
}
