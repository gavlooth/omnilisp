module lisp;

// ============================================================
// json-parse: JSON string → Omni value
// ============================================================

fn Value* json_val_to_omni(YyjsonVal* val, Interp* interp) {
    if (val == null) return make_nil(interp);

    if (omni_yyjson_is_null(val) != 0) {
        return make_nil(interp);
    }
    if (omni_yyjson_is_true(val) != 0) {
        return interp.global_env.lookup(interp.symbols.intern("true"));
    }
    if (omni_yyjson_is_false(val) != 0) {
        return make_nil(interp);  // false = nil in Omni
    }
    if (omni_yyjson_is_int(val) != 0) {
        return make_int(interp, omni_yyjson_get_sint(val));
    }
    if (omni_yyjson_is_real(val) != 0) {
        return make_double(interp, omni_yyjson_get_real(val));
    }
    if (omni_yyjson_is_str(val) != 0) {
        char* s = omni_yyjson_get_str(val);
        usz len = omni_yyjson_get_len(val);
        if (s == null) return make_string(interp, "");
        return make_string(interp, s[:len]);
    }
    if (omni_yyjson_is_arr(val) != 0) {
        usz count = omni_yyjson_arr_size(val);
        Value* arr = make_array(interp, count);
        YyjsonVal* elem = omni_yyjson_arr_get_first(val);
        for (usz i = 0; i < count; i++) {
            Value* v = json_val_to_omni(elem, interp);
            arr.array_val.items[i] = boundary_promote_to_root(v, interp);
            arr.array_val.length++;
            elem = omni_yyjson_next(elem);
        }
        return arr;
    }
    if (omni_yyjson_is_obj(val) != 0) {
        usz count = omni_yyjson_obj_size(val);
        Value* dict = make_hashmap(interp, (uint)(count < 8 ? 8 : count * 2));
        YyjsonVal* kv = omni_yyjson_obj_get_first(val);
        for (usz i = 0; i < count; i++) {
            // kv points to key, kv+1 points to value
            YyjsonVal* key = kv;
            YyjsonVal* v = omni_yyjson_next(kv);

            char* ks = omni_yyjson_get_str(key);
            usz klen = omni_yyjson_get_len(key);
            Value* key_val = make_string(interp, ks[:klen]);
            Value* val_v = json_val_to_omni(v, interp);
            hashmap_set(dict.hashmap_val, key_val, val_v, interp);

            // Advance past key and value (2 slots)
            kv = omni_yyjson_next(v);
        }
        return dict;
    }

    return make_nil(interp);
}

fn Value* prim_json_get(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return raise_error(interp, "json-get: expected 2 arguments");
    if (args[0] == null) return make_nil(interp);
    if (!is_string(args[1])) return raise_error(interp, "json-get: path must be string");

    char[] pointer = args[1].str_chars[:args[1].str_len];
    if (pointer.len == 0) return args[0];
    if (pointer[0] != '/') return raise_error(interp, "json-get: pointer must be empty for root or start with '/'");

    Value* current = args[0];
    usz i = 0;
    while (i < pointer.len) {
        if (pointer[i] != '/') return make_nil(interp);
        i++;

        usz start = i;
        while (i < pointer.len && pointer[i] != '/') i++;
        char[] token_raw = pointer[start:i - start];

        JsonPointerToken token = json_pointer_decode_token(token_raw);
        if (!token.ok) return raise_error(interp, "json-get: invalid pointer escape sequence");

        current = json_pointer_lookup(current, token.text, interp);
        if (token.owned) json_pointer_token_release(&token);
        if (is_nil(current)) return make_nil(interp);
    }

    return current;
}

fn Value* prim_json_parse(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return raise_error(interp, "json-parse: expected 1-2 arguments");
    }
    if (!is_string(args[0])) return raise_error(interp, "json-parse: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    usz parse_flags = 0;
    if (args.len == 2) {
        Value* parse_err = parse_json_options(args[1], &parse_flags, interp);
        if (parse_err != null) return parse_err;
    }

    YyjsonDoc* doc = omni_yyjson_read(src.ptr, src.len, parse_flags);
    if (doc == null) return raise_error(interp, "json-parse: invalid JSON");

    YyjsonVal* root = omni_yyjson_doc_get_root(doc);
    Value* result = json_val_to_omni(root, interp);

    omni_yyjson_doc_free(doc);
    return result;
}
