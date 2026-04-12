module lisp;

import main;
import std::core::mem;

bool g_deduce_force_row_dict_oom = false;
bool g_deduce_force_column_key_values_oom = false;

fn uint deduce_relation_row_dict_capacity(Relation* rel) {
    uint dict_capacity = 8;
    if (rel == null) return dict_capacity;
    uint needed_capacity = (uint)(rel.col_count * 2);
    while (dict_capacity < needed_capacity) dict_capacity *= 2;
    return dict_capacity;
}

fn Value* deduce_relation_materialize_row_dict(
    Relation* rel,
    Value*[] vals,
    Interp* interp,
    uint dict_capacity
) {
    Value* dict = make_hashmap_checked(interp, dict_capacity, "deduce-scan-range: out of memory");
    if (g_deduce_force_row_dict_oom || main::getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        dict = null;
    }
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-scan-range: out of memory");
    }
    for (usz i = 0; i < rel.col_count && i < vals.len; i++) {
        Value* k = rel.column_key_values[i];
        if (!hashmap_set_checked(dict.hashmap_val, k, vals[i], interp)) {
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-scan-range: out of memory");
        }
    }
    return dict;
}

fn Value* deduce_reverse_list(Value* list, Interp* interp) {
    Value* reversed = make_nil(interp);
    Value* curr = list;
    while (is_cons(curr)) {
        reversed = make_cons(interp, car(curr), reversed);
        curr = cdr(curr);
    }
    return reversed;
}

fn Value* deduce_relation_ensure_column_key_values(Relation* rel, Interp* interp, usz* created_out) {
    if (created_out != null) *created_out = 0;
    if (rel == null) return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: relation state is unavailable");
    if (rel.col_count == 0) return null;
    if (rel.column_key_values != null) return null;

    Value** key_values = (Value**)mem::malloc(Value*.sizeof * rel.col_count);
    if (g_deduce_force_column_key_values_oom || main::getenv("OMNI_DEDUCE_FORCE_COLUMN_KEY_VALUES_OOM") != null) {
        key_values = null;
    }
    if (key_values == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }

    for (usz i = 0; i < rel.col_count; i++) {
        Value* key = boundary_alloc_value_in_root(interp);
        if (key == null) {
            mem::free(key_values);
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
        }
        key.tag = SYMBOL;
        key.sym_val = rel.columns[i];
        key_values[i] = key;
    }

    rel.column_key_values = key_values;
    if (created_out != null) *created_out = rel.col_count;
    return null;
}
