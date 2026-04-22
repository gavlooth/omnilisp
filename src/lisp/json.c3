module lisp;

fn Value* json_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "parser", code, message, null);
}

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
        return make_symbol(interp, interp.sym_false);
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
        Value* arr = make_array_checked(interp, count, "json-parse: out of memory");
        if (arr == null || arr.tag != ARRAY) return arr;
        if (count == 0) return arr;
        YyjsonArrIter* iter = omni_yyjson_arr_iter_new(val);
        if (iter == null) return make_nil(interp);
        defer omni_yyjson_arr_iter_free(iter);
        for (usz i = 0; i < count; i++) {
            YyjsonVal* elem = omni_yyjson_arr_iter_next(iter);
            if (elem == null) return make_nil(interp);
            Value* v = json_val_to_omni(elem, interp);
            if (v != null && v.tag == ERROR) return v;
            Value* promoted = boundary_promote_to_root(v, interp);
            if (promoted == null) return json_raise(interp, "parser/out-of-memory", "json-parse: out of memory");
            if (promoted.tag == ERROR) return promoted;
            arr.array_val.items[i] = promoted;
            arr.array_val.length++;
        }
        return arr;
    }
    if (omni_yyjson_is_obj(val) != 0) {
        usz count = omni_yyjson_obj_size(val);
        Value* dict = make_hashmap_checked(interp, (uint)(count < 8 ? 8 : count * 2), "json-parse: out of memory");
        if (dict == null || dict.tag != HASHMAP) return dict;
        if (count == 0) return dict;
        YyjsonObjIter* iter = omni_yyjson_obj_iter_new(val);
        if (iter == null) return make_nil(interp);
        defer omni_yyjson_obj_iter_free(iter);
        for (usz i = 0; i < count; i++) {
            YyjsonVal* key = omni_yyjson_obj_iter_next(iter);
            if (key == null) return make_nil(interp);
            YyjsonVal* v = omni_yyjson_obj_iter_get_val(key);
            if (v == null) return make_nil(interp);

            char* ks = omni_yyjson_get_str(key);
            usz klen = omni_yyjson_get_len(key);
            Value* key_val = make_string(interp, ks[:klen]);
            Value* val_v = json_val_to_omni(v, interp);
            if ((key_val != null && key_val.tag == ERROR) ||
                (val_v != null && val_v.tag == ERROR)) {
                return key_val != null && key_val.tag == ERROR ? key_val : val_v;
            }
            if (!hashmap_set_checked(dict.hashmap_val, key_val, val_v, interp)) {
                return json_raise(interp, "parser/out-of-memory", "json-parse: out of memory");
            }
        }
        return dict;
    }

    return make_nil(interp);
}

fn Value* prim_json_get(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return json_raise(interp, "parser/arity", "json-get: expected 2 arguments");
    if (args[0] == null) return make_nil(interp);
    if (!is_string(args[1])) return json_raise(interp, "parser/invalid-argument", "json-get: path must be string");

    char[] pointer = args[1].str_chars[:args[1].str_len];
    if (pointer.len == 0) return args[0];
    if (pointer[0] != '/') return json_raise(interp, "parser/invalid-argument", "json-get: pointer must be empty for root or start with '/'");

    Value* current = args[0];
    usz i = 0;
    while (i < pointer.len) {
        if (pointer[i] != '/') return make_nil(interp);
        i++;

        usz start = i;
        while (i < pointer.len && pointer[i] != '/') i++;
        char[] token_raw = pointer[start:i - start];

        JsonPointerToken token = json_pointer_decode_token(token_raw);
        if (!token.ok) return json_raise(interp, "parser/invalid-argument", "json-get: invalid pointer escape sequence");

        current = json_pointer_lookup(current, token.text, interp);
        if (token.owned) json_pointer_token_release(&token);
        if (is_nil(current)) return make_nil(interp);
    }

    return current;
}

fn Value* prim_json_parse(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return json_raise(interp, "parser/arity", "json-parse: expected 1-2 arguments");
    }
    if (!is_string(args[0])) return json_raise(interp, "parser/invalid-argument", "json-parse: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    usz parse_flags = 0;
    if (args.len == 2) {
        Value* parse_err = parse_json_options(args[1], &parse_flags, interp);
        if (parse_err != null) return parse_err;
    }

    YyjsonDoc* doc = omni_yyjson_read(src.ptr, src.len, parse_flags);
    if (doc == null) return json_raise(interp, "parser/syntax-error", "json-parse: invalid JSON");

    YyjsonVal* root = omni_yyjson_doc_get_root(doc);
    Value* result = json_val_to_omni(root, interp);

    omni_yyjson_doc_free(doc);
    return result;
}
