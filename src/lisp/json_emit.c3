module lisp;

import std::io;

fn Value* json_emit_set_allocated_value(
    YyjsonMutVal* json_v,
    char[] what,
    YyjsonMutVal** out_json,
    Interp* interp
) {
    if (json_v != null) {
        *out_json = json_v;
        return null;
    }
    char[128] ebuf;
    char[] msg;
    if (try char[] formatted = io::bprintf(&ebuf, "json-emit: failed to allocate %s", (ZString)what)) {
        msg = formatted;
    } else {
        msg = "json-emit: allocation failed";
    }
    return json_raise(interp, "parser/out-of-memory", msg);
}

fn Value* json_emit_raise_unsupported_value_type(Value* val, Interp* interp) {
    char[128] ebuf;
    char[] type_name = interp.symbols.get_name(value_type_name(val, interp));
    char[] msg;
    if (try char[] formatted = io::bprintf(&ebuf, "json-emit: unsupported value type '%s'", (ZString)type_name)) {
        msg = formatted;
    } else {
        msg = "json-emit: unsupported value type";
    }
    return json_raise(interp, "parser/type-mismatch", msg);
}

fn Value* json_emit_raise_unsupported_dict_key_type(Value* key_v, Interp* interp) {
    char[128] ebuf;
    char[] type_name = interp.symbols.get_name(value_type_name(key_v, interp));
    char[] msg;
    if (try char[] formatted = io::bprintf(&ebuf, "json-emit: unsupported dictionary key type '%s'", (ZString)type_name)) {
        msg = formatted;
    } else {
        msg = "json-emit: unsupported dictionary key type";
    }
    return json_raise(interp, "parser/type-mismatch", msg);
}

fn bool json_emit_object_key_name(Value* key_v, Interp* interp, char[]* out_name) {
    if (is_string(key_v)) {
        *out_name = key_v.str_chars[:key_v.str_len];
        return true;
    }
    if (key_v != null && key_v.tag == SYMBOL) {
        *out_name = interp.symbols.get_name(key_v.sym_val);
        return true;
    }
    return false;
}

fn bool json_emit_key_names_equal(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn Value* json_emit_reject_duplicate_object_key(Interp* interp) @inline {
    return json_raise(interp, "parser/invalid-argument", "json-emit: duplicate JSON object key after string normalization");
}

fn Value* json_emit_check_duplicate_object_key(HashMap* hm, usz current_index, char[] key_name, Interp* interp) {
    for (usz j = 0; j < current_index; j++) {
        HashEntry* prior = &hm.entries[j];
        if (prior.key == null) continue;

        char[] prior_name = {};
        if (!json_emit_object_key_name(prior.key, interp, &prior_name)) continue;
        if (json_emit_key_names_equal(prior_name, key_name)) return json_emit_reject_duplicate_object_key(interp);
    }
    return null;
}

fn Value* json_emit_append_array_value(YyjsonMutVal* arr, YyjsonMutVal* elem, Interp* interp) {
    if (!omni_yyjson_mut_arr_append(arr, elem)) {
        return json_raise(interp, "parser/out-of-memory", "json-emit: failed to append array element");
    }
    return null;
}

fn Value* json_emit_add_object_value(YyjsonMutVal* obj, YyjsonMutVal* key, YyjsonMutVal* value, Interp* interp) {
    if (!omni_yyjson_mut_obj_add(obj, key, value)) {
        return json_raise(interp, "parser/out-of-memory", "json-emit: failed to add object field");
    }
    return null;
}

fn Value* omni_symbol_to_json(SymbolId sym, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_json) {
    char[] name = interp.symbols.get_name(sym);
    // "true" → JSON true, "false" → JSON false.
    if (name.len == 4 && name[0] == 't' && name[1] == 'r' && name[2] == 'u' && name[3] == 'e') {
        return json_emit_set_allocated_value(omni_yyjson_mut_bool(doc, 1), "JSON boolean", out_json, interp);
    }
    if (name.len == 5 && name[0] == 'f' && name[1] == 'a' && name[2] == 'l' && name[3] == 's' && name[4] == 'e') {
        return json_emit_set_allocated_value(omni_yyjson_mut_bool(doc, 0), "JSON boolean", out_json, interp);
    }
    // Other symbols → string.
    return json_emit_set_allocated_value(omni_yyjson_mut_strn(doc, name.ptr, name.len), "JSON string", out_json, interp);
}

fn Value* omni_cons_list_to_json(Value* list, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_json) {
    YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
    if (arr == null) return json_raise(interp, "parser/out-of-memory", "json-emit: failed to allocate JSON array");
    Value* curr = list;
    while (is_cons(curr)) {
        YyjsonMutVal* elem = null;
        Value* elem_err = omni_to_json_val(car(curr), doc, interp, &elem);
        if (elem_err != null) return elem_err;
        Value* append_err = json_emit_append_array_value(arr, elem, interp);
        if (append_err != null) return append_err;
        curr = cdr(curr);
    }
    if (!is_nil(curr)) {
        return json_raise(interp, "parser/invalid-argument", "json-emit: expected proper list");
    }
    *out_json = arr;
    return null;
}

fn Value* omni_array_to_json(Value* arr_v, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_json) {
    YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
    if (arr == null) return json_raise(interp, "parser/out-of-memory", "json-emit: failed to allocate JSON array");
foreach (item : arr_v.array_val.items[0:arr_v.array_val.length]) {
        YyjsonMutVal* elem = null;
        Value* elem_err = omni_to_json_val(item, doc, interp, &elem);
        if (elem_err != null) return elem_err;
        Value* append_err = json_emit_append_array_value(arr, elem, interp);
        if (append_err != null) return append_err;
}
    *out_json = arr;
    return null;
}

fn Value* omni_hash_key_to_json(Value* key_v, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_key) {
    char[] name = {};
    if (json_emit_object_key_name(key_v, interp, &name)) {
        return json_emit_set_allocated_value(
            omni_yyjson_mut_strn(doc, name.ptr, name.len),
            "JSON object key string",
            out_key,
            interp
        );
    }
    return json_emit_raise_unsupported_dict_key_type(key_v, interp);
}

fn Value* omni_hashmap_to_json(Value* map_v, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_json) {
    YyjsonMutVal* obj = omni_yyjson_mut_obj(doc);
    if (obj == null) return json_raise(interp, "parser/out-of-memory", "json-emit: failed to allocate JSON object");
    HashMap* hm = map_v.hashmap_val;
    for (usz i = 0; i < hm.capacity; i++) {
        HashEntry* entry = &hm.entries[i];
        if (entry.key == null) continue;

        YyjsonMutVal* key = null;
        char[] key_name = {};
        if (!json_emit_object_key_name(entry.key, interp, &key_name)) {
            return json_emit_raise_unsupported_dict_key_type(entry.key, interp);
        }
        Value* duplicate_err = json_emit_check_duplicate_object_key(hm, i, key_name, interp);
        if (duplicate_err != null) return duplicate_err;

        Value* key_err = omni_hash_key_to_json(entry.key, doc, interp, &key);
        if (key_err != null) return key_err;
        YyjsonMutVal* v = null;
        Value* value_err = omni_to_json_val(entry.value, doc, interp, &v);
        if (value_err != null) return value_err;
        Value* add_err = json_emit_add_object_value(obj, key, v, interp);
        if (add_err != null) return add_err;
    }
    *out_json = obj;
    return null;
}

fn Value* omni_to_json_val(Value* val, YyjsonMutDoc* doc, Interp* interp, YyjsonMutVal** out_json) {
    if (val == null || val.tag == NIL) {
        return json_emit_set_allocated_value(omni_yyjson_mut_null(doc), "JSON null", out_json, interp);
    }

    switch (val.tag) {
        case INT:
            return json_emit_set_allocated_value(omni_yyjson_mut_sint(doc, val.int_val), "JSON integer", out_json, interp);
        case DOUBLE:
            return json_emit_set_allocated_value(omni_yyjson_mut_real(doc, val.double_val), "JSON number", out_json, interp);
        case FLOAT32:
            return json_emit_set_allocated_value(omni_yyjson_mut_real(doc, (double)val.float32_val), "JSON number", out_json, interp);
        case STRING:
            return json_emit_set_allocated_value(
                omni_yyjson_mut_strn(doc, val.str_chars, val.str_len),
                "JSON string",
                out_json,
                interp
            );
        case SYMBOL:
            return omni_symbol_to_json(val.sym_val, doc, interp, out_json);
        case CONS:
            // List → JSON array.
            return omni_cons_list_to_json(val, doc, interp, out_json);
        case ARRAY:
            return omni_array_to_json(val, doc, interp, out_json);
        case HASHMAP:
            return omni_hashmap_to_json(val, doc, interp, out_json);
        case TIME_POINT: {
            char[64] ts_buf;
            char[] ts = time_point_to_string(val.time_point_val, ts_buf[..]);
            return json_emit_set_allocated_value(
                omni_yyjson_mut_strn(doc, ts.ptr, ts.len),
                "JSON string",
                out_json,
                interp
            );
        }
        default:
            return json_emit_raise_unsupported_value_type(val, interp);
    }
}

fn Value* json_emit_impl(Value*[] args, Interp* interp, bool pretty_default) {
    if (args.len < 1 || args.len > 2) {
        return json_raise(interp, "parser/arity", "json-emit: expected 1-2 arguments");
    }

    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return json_raise(interp, "parser/out-of-memory", "json-emit: failed to create document");

    YyjsonMutVal* root = null;
    Value* root_err = omni_to_json_val(args[0], doc, interp, &root);
    if (root_err != null) {
        omni_yyjson_mut_doc_free(doc);
        return root_err;
    }
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
        if (parse_err != null) {
            omni_yyjson_mut_doc_free(doc);
            return parse_err;
        }
    }

    if (opts.has_pretty_override) {
        if (opts.pretty) {
            opts.flags |= JSON_WRITE_OPTION_PRETTY;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_PRETTY;
        }
    }

    if (opts.has_precision) {
        if (opts.precision < 0 || opts.precision > 15) {
            omni_yyjson_mut_doc_free(doc);
            return json_raise(interp, "parser/invalid-argument", "json-emit: precision must be in range 0..15");
        }
        usz fixed_flag = omni_yyjson_fp_to_fixed_flag(opts.precision);
        if (fixed_flag != 0) opts.flags |= fixed_flag;
    }

    usz len = 0;
    char* json_str = omni_yyjson_mut_write_with_flags(doc, opts.flags, &len);

    omni_yyjson_mut_doc_free(doc);

    if (json_str == null) return json_raise(interp, "parser/serialization-failed", "json-emit: serialization failed");

    Value* result = make_string(interp, json_str[:len]);
    c_free_json(json_str);
    return result;
}

<*
@require interp != null
*>
fn Value* prim_json_emit(Value*[] args, Env* env, Interp* interp) {
    return json_emit_impl(args, interp, false);
}

<*
@require interp != null
*>
fn Value* prim_json_emit_pretty(Value*[] args, Env* env, Interp* interp) {
    return json_emit_impl(args, interp, true);
}
