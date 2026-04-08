module lisp;

import std::core::mem;

fn bool deduce_rule_build_step_prefix_probe(
    DeduceRuleSignature* sig,
    usz step_idx,
    bool[] var_bound,
    Value*[] var_values,
    Value*[] out_prefix_values,
    usz* out_prefix_len
) {
    if (out_prefix_len == null) return false;
    *out_prefix_len = 0;
    if (sig == null || step_idx >= sig.body_count || out_prefix_values.ptr == null) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) return false;

    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > out_prefix_values.len) return false;

    usz prefix_len = 0;
    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) break;
            out_prefix_values[prefix_len++] = var_values[var_id];
            continue;
        }
        out_prefix_values[prefix_len++] = sig.body_term_literals[term_idx];
    }

    *out_prefix_len = prefix_len;
    return prefix_len > 0;
}

fn bool deduce_tuple_matches_prefix(
    Value*[] tuple_vals,
    Value*[] prefix_values,
    usz prefix_len
) @inline {
    if (prefix_len == 0) return true;
    if (tuple_vals.len < prefix_len || prefix_values.len < prefix_len) return false;
    for (usz i = 0; i < prefix_len; i++) {
        if (!values_equal(tuple_vals[i], prefix_values[i])) return false;
    }
    return true;
}

fn bool deduce_bytes_equal(char* left, usz left_len, char* right, usz right_len) @inline {
    if (left_len != right_len) return false;
    for (usz i = 0; i < left_len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn Value* deduce_encode_value_vector_to_owned_string(
    Value*[] values,
    Value** key_out,
    Interp* interp
) {
    if (key_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid encoded tuple target");
    }
    *key_out = null;

    usz buf_size = values.len > 0 ? values.len * 16 : 16;
    if (buf_size < 64) buf_size = 64;

    while (true) {
        if (buf_size == 0 || buf_size > 1_048_576) {
            return deduce_raise(interp, "deduce/analyze-tuple-too-large",
                "deduce/analyze: encoded tuple key exceeds supported size");
        }

        char* owned = (char*)mem::malloc(buf_size + 1);
        if (owned == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }

        usz? encoded_len = encode_tuple(values, owned, buf_size);
        if (catch err = encoded_len) {
            mem::free(owned);
            if (buf_size > usz.max / 2) {
                return deduce_raise(interp, "deduce/analyze-tuple-too-large",
                    "deduce/analyze: encoded tuple key exceeds supported size");
            }
            buf_size *= 2;
            continue;
        }

        owned[encoded_len] = 0;
        *key_out = make_string_owned(interp, owned, encoded_len);
        return null;
    }
}

fn bool deduce_delta_set_contains(DeduceDeltaTupleSet* set, void* key_data, usz key_len, Interp* interp) {
    if (set == null || interp == null || (key_data == null && key_len > 0)) return false;
    if (set.seen_map == null || set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) return false;
    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    return hashmap_get(set.seen_map.hashmap_val, key_val) != null;
}

fn bool deduce_rule_open_existing_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    MdbDbi* dbi_out,
    DeduceRelationSchema** schema_out,
    Interp* interp
) {
    if (db == null || txn == null || dbi_out == null || schema_out == null || interp == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
    if (schema == null) return false;

    char[] name = interp.symbols.get_name(predicate);
    char* name_c = deduce_copy_c_string(name);
    if (name_c == null) return false;
    defer mem::free(name_c);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, name_c, 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    *schema_out = schema;
    return true;
}

fn bool deduce_rule_match_positive_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (var_bound[var_id]) {
                if (!values_equal(var_values[var_id], tuple_v)) return false;
            } else {
                var_bound[var_id] = true;
                var_values[var_id] = tuple_v;
            }
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}

fn bool deduce_rule_match_negated_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (!var_bound[var_id]) return false;
            if (!values_equal(var_values[var_id], tuple_v)) return false;
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}
