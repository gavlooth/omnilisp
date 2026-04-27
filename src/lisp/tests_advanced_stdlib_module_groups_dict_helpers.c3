module lisp;


fn bool advanced_collections_float64_hash_regression(Interp* interp) {
    double[16] values = {
        -4096.5, -257.125, -17.25, -0.125,
        0.125, 1.5, 2.5, 3.75,
        8.125, 16.25, 31.5, 64.875,
        128.0625, 257.5, 1024.03125, 4096.75
    };
    uint[16] hashes = {};
    Value* dict = make_hashmap_checked(interp, 4, "test: Float64 dictionary allocation failed");
    Value* set = make_set_checked(interp, 4, "test: Float64 set allocation failed");
    Value* true_val = make_symbol(interp, interp.sym_true);
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null ||
        set == null || set.tag != SET || set.hashmap_val == null || true_val == null) {
        return false;
    }

    for (usz i = 0; i < values.len; i++) {
        Value* key = make_double(interp, values[i]);
        hashes[i] = hash_value(key);
        if (hashes[i] == 0) return false;
        for (usz j = 0; j < i; j++) {
            if (hashes[i] == hashes[j]) return false;
        }
        if (!hashmap_set_checked(dict.hashmap_val, key, make_int(interp, (long)i + 1000), interp)) return false;
        if (!hashmap_set_checked(set.hashmap_val, key, true_val, interp)) return false;
    }

    for (usz i = 0; i < values.len; i++) {
        Value* lookup_key = make_double(interp, values[i]);
        Value* dict_value = hashmap_get(dict.hashmap_val, lookup_key);
        Value* set_value = hashmap_get(set.hashmap_val, lookup_key);
        if (dict_value == null || dict_value.tag != INT || dict_value.int_val != (long)i + 1000) return false;
        if (set_value == null) return false;
    }

    Value* pos_zero = make_double(interp, 0.0);
    Value* neg_zero = make_double(interp, -0.0);
    if (!hashmap_key_equals(pos_zero, neg_zero)) return false;
    if (hash_value(pos_zero) != hash_value(neg_zero)) return false;

    double nan = 0.0 / 0.0;
    Value* nan_a = make_double(interp, nan);
    Value* nan_b = make_double(interp, nan);
    if (hashmap_key_equals(nan_a, nan_b)) return false;
    if (!hashmap_set_checked(dict.hashmap_val, nan_a, make_int(interp, 9090), interp)) return false;
    if (hashmap_get(dict.hashmap_val, nan_b) != null) return false;
    if (hashmap_get(dict.hashmap_val, nan_a) == null) return false;

    return true;
}
