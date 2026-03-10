module lisp;

fn YyjsonMutVal* omni_symbol_to_json(SymbolId sym, YyjsonMutDoc* doc, Interp* interp) {
    char[] name = interp.symbols.get_name(sym);
    // "true" → JSON true, "false" → JSON false.
    if (name.len == 4 && name[0] == 't' && name[1] == 'r' && name[2] == 'u' && name[3] == 'e') {
        return omni_yyjson_mut_bool(doc, 1);
    }
    if (name.len == 5 && name[0] == 'f' && name[1] == 'a' && name[2] == 'l' && name[3] == 's' && name[4] == 'e') {
        return omni_yyjson_mut_bool(doc, 0);
    }
    // Other symbols → string.
    return omni_yyjson_mut_strn(doc, name.ptr, name.len);
}

fn YyjsonMutVal* omni_cons_list_to_json(Value* list, YyjsonMutDoc* doc, Interp* interp) {
    YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
    Value* curr = list;
    while (is_cons(curr)) {
        YyjsonMutVal* elem = omni_to_json_val(car(curr), doc, interp);
        omni_yyjson_mut_arr_append(arr, elem);
        curr = cdr(curr);
    }
    return arr;
}

fn YyjsonMutVal* omni_array_to_json(Value* arr_v, YyjsonMutDoc* doc, Interp* interp) {
    YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
    for (usz i = 0; i < arr_v.array_val.length; i++) {
        YyjsonMutVal* elem = omni_to_json_val(arr_v.array_val.items[i], doc, interp);
        omni_yyjson_mut_arr_append(arr, elem);
    }
    return arr;
}

fn YyjsonMutVal* omni_hash_key_to_json(Value* key_v, YyjsonMutDoc* doc, Interp* interp) {
    if (is_string(key_v)) {
        return omni_yyjson_mut_strn(doc, key_v.str_chars, key_v.str_len);
    }
    if (key_v.tag == SYMBOL) {
        char[] name = interp.symbols.get_name(key_v.sym_val);
        return omni_yyjson_mut_strn(doc, name.ptr, name.len);
    }
    return omni_yyjson_mut_strn(doc, "?", 1);
}

fn YyjsonMutVal* omni_hashmap_to_json(Value* map_v, YyjsonMutDoc* doc, Interp* interp) {
    YyjsonMutVal* obj = omni_yyjson_mut_obj(doc);
    HashMap* hm = map_v.hashmap_val;
    for (usz i = 0; i < hm.capacity; i++) {
        HashEntry* entry = &hm.entries[i];
        if (entry.key == null) continue;

        YyjsonMutVal* key = omni_hash_key_to_json(entry.key, doc, interp);
        YyjsonMutVal* v = omni_to_json_val(entry.value, doc, interp);
        omni_yyjson_mut_obj_add(obj, key, v);
    }
    return obj;
}

fn YyjsonMutVal* omni_to_json_val(Value* val, YyjsonMutDoc* doc, Interp* interp) {
    if (val == null || val.tag == NIL) {
        return omni_yyjson_mut_null(doc);
    }

    switch (val.tag) {
        case INT:
            return omni_yyjson_mut_sint(doc, val.int_val);
        case DOUBLE:
            return omni_yyjson_mut_real(doc, val.double_val);
        case STRING:
            return omni_yyjson_mut_strn(doc, val.str_chars, val.str_len);
        case SYMBOL:
            return omni_symbol_to_json(val.sym_val, doc, interp);
        case CONS:
            // List → JSON array.
            return omni_cons_list_to_json(val, doc, interp);
        case ARRAY:
            return omni_array_to_json(val, doc, interp);
        case HASHMAP:
            return omni_hashmap_to_json(val, doc, interp);
        case TIME_POINT: {
            char[64] ts_buf;
            char[] ts = time_point_to_string(val.time_point_val, ts_buf[..]);
            return omni_yyjson_mut_strn(doc, ts.ptr, ts.len);
        }
        default:
            return omni_yyjson_mut_null(doc);
    }
}

fn Value* json_emit_impl(Value*[] args, Interp* interp, bool pretty_default) {
    if (args.len < 1 || args.len > 2) {
        return raise_error(interp, "json-emit: expected 1-2 arguments");
    }

    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return raise_error(interp, "json-emit: failed to create document");

    YyjsonMutVal* root = omni_to_json_val(args[0], doc, interp);
    omni_yyjson_mut_doc_set_root(doc, root);

    JsonEmitOptions opts;
    opts.pretty = pretty_default;
    opts.has_pretty_override = false;
    opts.flags = 0;
    opts.has_precision = false;
    opts.precision = 0;

    if (pretty_default) {
        opts.flags |= JSON_WRITE_OPTION_PRETTY;
    }
    if (args.len == 2) {
        Value* parse_err = parse_json_emit_options(args[1], &opts, interp);
        if (parse_err != null) return parse_err;
    }

    if (opts.has_pretty_override) {
        if (opts.pretty) {
            opts.flags |= JSON_WRITE_OPTION_PRETTY;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_PRETTY;
        }
    }

    if (opts.has_precision) {
        usz fixed_flag = omni_yyjson_fp_to_fixed_flag(opts.precision);
        if (fixed_flag == 0) {
            omni_yyjson_mut_doc_free(doc);
            return raise_error(interp, "json-emit: precision must be in range 1..15");
        }
        opts.flags |= fixed_flag;
    }

    usz len = 0;
    char* json_str = omni_yyjson_mut_write_with_flags(doc, opts.flags, &len);

    omni_yyjson_mut_doc_free(doc);

    if (json_str == null) return raise_error(interp, "json-emit: serialization failed");

    Value* result = make_string(interp, json_str[:len]);
    c_free_json(json_str);
    return result;
}

fn Value* prim_json_emit(Value*[] args, Env* env, Interp* interp) {
    return json_emit_impl(args, interp, false);
}

fn Value* prim_json_emit_pretty(Value*[] args, Env* env, Interp* interp) {
    return json_emit_impl(args, interp, true);
}
