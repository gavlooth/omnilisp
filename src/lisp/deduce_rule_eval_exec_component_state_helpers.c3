module lisp;

import std::core::mem;

struct DeduceTupleSupportTable {
    Value* base_counts;
    Value* recursive_counts;
}

fn void deduce_tuple_support_table_release(DeduceTupleSupportTable* table) {
    if (table == null) return;
    table.base_counts = null;
    table.recursive_counts = null;
}

fn Value* deduce_delta_set_insert(DeduceDeltaTupleSet* set, void* key_data, usz key_len, Interp* interp) {
    if (set == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set state");
    }
    if (set.seen_map == null) {
        set.seen_map = make_hashmap_checked(interp, 64, "deduce/analyze: out of memory");
        if (set.seen_map == null || set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }

    if (set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set payload");
    }

    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    if (hashmap_get(set.seen_map.hashmap_val, key_val) != null) return null;
    if (!hashmap_set_checked(set.seen_map.hashmap_val, key_val, make_symbol(interp, interp.sym_true), interp)) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    if (set.count >= set.capacity) {
        usz new_cap = set.capacity > 0 ? set.capacity * 2 : 4;
        if (new_cap < set.count + 1) new_cap = set.count + 1;
        DeduceEncodedTuple* next = (DeduceEncodedTuple*)mem::realloc(
            set.tuples,
            DeduceEncodedTuple.sizeof * new_cap
        );
        if (next == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = set.capacity; i < new_cap; i++) next[i] = {};
        set.tuples = next;
        set.capacity = new_cap;
    }

    DeduceEncodedTuple* tuple = &set.tuples[set.count];
    *tuple = {};
    if (key_len > 0) {
        tuple.key_bytes = (char*)mem::malloc(key_len);
        if (tuple.key_bytes == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < key_len; i++) tuple.key_bytes[i] = ((char*)key_data)[i];
    }
    tuple.key_len = key_len;
    set.count++;
    return null;
}

fn Value* deduce_tuple_support_table_ensure_map(Value** map_out, Interp* interp) {
    if (map_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table state");
    }
    if (*map_out == null) {
        *map_out = make_hashmap_checked(interp, 64, "deduce/analyze: out of memory");
        if (*map_out == null || (*map_out).tag != HASHMAP || (*map_out).hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    if ((*map_out).tag != HASHMAP || (*map_out).hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table payload");
    }
    return null;
}

fn long deduce_tuple_support_table_lookup_count(
    Value* map,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (interp == null || (key_data == null && key_len > 0)) return 0;
    if (map == null || map.tag != HASHMAP || map.hashmap_val == null) return 0;
    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    Value* count_val = hashmap_get(map.hashmap_val, key_val);
    if (count_val == null || count_val.tag != INT || count_val.int_val <= 0) return 0;
    return count_val.int_val;
}

fn Value* deduce_tuple_support_table_set_count(
    Value** map_out,
    void* key_data,
    usz key_len,
    long count,
    Interp* interp
) {
    if (map_out == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table update");
    }
    if (count < 0) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter underflow");
    }

    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    if (count == 0) {
        if (*map_out != null && (*map_out).tag == HASHMAP && (*map_out).hashmap_val != null) {
            hashmap_remove((*map_out).hashmap_val, key_val);
        }
        return null;
    }

    Value* err = deduce_tuple_support_table_ensure_map(map_out, interp);
    if (err != null) return err;
    if (!hashmap_set_checked((*map_out).hashmap_val, key_val, make_int(interp, count), interp)) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    return null;
}

fn long deduce_tuple_support_table_total_count(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (table == null || interp == null) return 0;
    long base_count = deduce_tuple_support_table_lookup_count(table.base_counts, key_data, key_len, interp);
    long recursive_count = deduce_tuple_support_table_lookup_count(table.recursive_counts, key_data, key_len, interp);
    if (base_count > long.max - recursive_count) return long.max;
    return base_count + recursive_count;
}

fn Value* deduce_tuple_support_table_seed_base_tuple(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    long* total_count_out,
    Interp* interp
) {
    if (table == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed");
    }
    long base_count = deduce_tuple_support_table_lookup_count(table.base_counts, key_data, key_len, interp);
    if (base_count >= long.max) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter overflow");
    }
    Value* err = deduce_tuple_support_table_set_count(&table.base_counts, key_data, key_len, base_count + 1, interp);
    if (err != null) return err;
    if (total_count_out != null) {
        *total_count_out = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    }
    return null;
}

fn Value* deduce_tuple_support_table_adjust_recursive_count(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    long delta,
    long* total_count_out,
    bool* became_present_out,
    bool* became_absent_out,
    Interp* interp
) {
    if (table == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table delta");
    }
    if (became_present_out != null) *became_present_out = false;
    if (became_absent_out != null) *became_absent_out = false;

    long old_total = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    long recursive_count = deduce_tuple_support_table_lookup_count(table.recursive_counts, key_data, key_len, interp);
    long next_recursive = recursive_count;
    if (delta > 0) {
        if (recursive_count > long.max - delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter overflow");
        }
        next_recursive = recursive_count + delta;
    } else if (delta < 0) {
        if (recursive_count < -delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter underflow");
        }
        next_recursive = recursive_count + delta;
    }

    Value* err = deduce_tuple_support_table_set_count(&table.recursive_counts, key_data, key_len, next_recursive, interp);
    if (err != null) return err;

    long next_total = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    if (became_present_out != null) *became_present_out = old_total == 0 && next_total > 0;
    if (became_absent_out != null) *became_absent_out = old_total > 0 && next_total == 0;
    if (total_count_out != null) *total_count_out = next_total;
    return null;
}

fn int deduce_scc_predicate_index(DeduceSccPlan* plan, SymbolId predicate) {
    if (plan == null || plan.predicates.len == 0) return -1;
    return deduce_scc_find_predicate_index(plan.predicates, predicate);
}

fn bool deduce_scc_predicate_in_component(DeduceSccPlan* plan, SymbolId predicate, usz component_id) {
    if (plan == null || component_id >= plan.component_count) return false;
    int pred_idx = deduce_scc_predicate_index(plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= plan.predicate_component.len) return false;
    return (usz)plan.predicate_component[(usz)pred_idx] == component_id;
}
