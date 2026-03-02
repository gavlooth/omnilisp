module lisp;

import std::core::mem;
import std::io;

// ============================================================
// yyjson extern declarations (via C wrapper in csrc/json_helpers.c)
// ============================================================

alias YyjsonDoc = void;
alias YyjsonVal = void;
alias YyjsonMutDoc = void;
alias YyjsonMutVal = void;

// Read API
extern fn YyjsonDoc* omni_yyjson_read(char* dat, usz len) @extern("omni_yyjson_read");
extern fn void omni_yyjson_doc_free(YyjsonDoc* doc) @extern("omni_yyjson_doc_free");
extern fn YyjsonVal* omni_yyjson_doc_get_root(YyjsonDoc* doc) @extern("omni_yyjson_doc_get_root");

// Type checking
extern fn int omni_yyjson_get_type(YyjsonVal* val) @extern("omni_yyjson_get_type");
extern fn int omni_yyjson_is_null(YyjsonVal* val) @extern("omni_yyjson_is_null");
extern fn int omni_yyjson_is_true(YyjsonVal* val) @extern("omni_yyjson_is_true");
extern fn int omni_yyjson_is_false(YyjsonVal* val) @extern("omni_yyjson_is_false");
extern fn int omni_yyjson_is_int(YyjsonVal* val) @extern("omni_yyjson_is_int");
extern fn int omni_yyjson_is_real(YyjsonVal* val) @extern("omni_yyjson_is_real");
extern fn int omni_yyjson_is_str(YyjsonVal* val) @extern("omni_yyjson_is_str");
extern fn int omni_yyjson_is_arr(YyjsonVal* val) @extern("omni_yyjson_is_arr");
extern fn int omni_yyjson_is_obj(YyjsonVal* val) @extern("omni_yyjson_is_obj");

// Value getters
extern fn long omni_yyjson_get_sint(YyjsonVal* val) @extern("omni_yyjson_get_sint");
extern fn double omni_yyjson_get_real(YyjsonVal* val) @extern("omni_yyjson_get_real");
extern fn char* omni_yyjson_get_str(YyjsonVal* val) @extern("omni_yyjson_get_str");
extern fn usz omni_yyjson_get_len(YyjsonVal* val) @extern("omni_yyjson_get_len");
extern fn int omni_yyjson_get_bool(YyjsonVal* val) @extern("omni_yyjson_get_bool");

// Array/Object
extern fn usz omni_yyjson_arr_size(YyjsonVal* arr) @extern("omni_yyjson_arr_size");
extern fn YyjsonVal* omni_yyjson_arr_get_first(YyjsonVal* arr) @extern("omni_yyjson_arr_get_first");
extern fn usz omni_yyjson_obj_size(YyjsonVal* obj) @extern("omni_yyjson_obj_size");
extern fn YyjsonVal* omni_yyjson_obj_get_first(YyjsonVal* obj) @extern("omni_yyjson_obj_get_first");
extern fn YyjsonVal* omni_yyjson_next(YyjsonVal* val) @extern("omni_yyjson_next");

// Mutable API (for json-emit)
extern fn YyjsonMutDoc* omni_yyjson_mut_doc_new() @extern("omni_yyjson_mut_doc_new");
extern fn void omni_yyjson_mut_doc_free(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_doc_free");
extern fn YyjsonMutVal* omni_yyjson_mut_null(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_null");
extern fn YyjsonMutVal* omni_yyjson_mut_bool(YyjsonMutDoc* doc, int val) @extern("omni_yyjson_mut_bool");
extern fn YyjsonMutVal* omni_yyjson_mut_sint(YyjsonMutDoc* doc, long val) @extern("omni_yyjson_mut_sint");
extern fn YyjsonMutVal* omni_yyjson_mut_real(YyjsonMutDoc* doc, double val) @extern("omni_yyjson_mut_real");
extern fn YyjsonMutVal* omni_yyjson_mut_strn(YyjsonMutDoc* doc, char* str, usz len) @extern("omni_yyjson_mut_strn");
extern fn YyjsonMutVal* omni_yyjson_mut_arr(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_arr");
extern fn void omni_yyjson_mut_arr_append(YyjsonMutVal* arr, YyjsonMutVal* val) @extern("omni_yyjson_mut_arr_append");
extern fn YyjsonMutVal* omni_yyjson_mut_obj(YyjsonMutDoc* doc) @extern("omni_yyjson_mut_obj");
extern fn void omni_yyjson_mut_obj_add(YyjsonMutVal* obj, YyjsonMutVal* key, YyjsonMutVal* val) @extern("omni_yyjson_mut_obj_add");
extern fn void omni_yyjson_mut_doc_set_root(YyjsonMutDoc* doc, YyjsonMutVal* root) @extern("omni_yyjson_mut_doc_set_root");
extern fn char* omni_yyjson_mut_write(YyjsonMutDoc* doc, usz* len) @extern("omni_yyjson_mut_write");
extern fn char* omni_yyjson_mut_write_pretty(YyjsonMutDoc* doc, usz* len) @extern("omni_yyjson_mut_write_pretty");

extern fn void c_free_json(void* ptr) @extern("free");

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
            arr.array_val.items[i] = v;
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

fn Value* prim_json_parse(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "json-parse: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "json-parse: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];

    YyjsonDoc* doc = omni_yyjson_read(src.ptr, src.len);
    if (doc == null) return raise_error(interp, "json-parse: invalid JSON");

    YyjsonVal* root = omni_yyjson_doc_get_root(doc);
    Value* result = json_val_to_omni(root, interp);

    omni_yyjson_doc_free(doc);
    return result;
}

// ============================================================
// json-emit: Omni value → JSON string
// ============================================================

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
            return omni_yyjson_mut_strn(doc, val.str_val.chars, val.str_val.len);
        case SYMBOL: {
            char[] name = interp.symbols.get_name(val.sym_val);
            // "true" → JSON true, "false" → JSON false
            if (name.len == 4 && name[0] == 't' && name[1] == 'r' && name[2] == 'u' && name[3] == 'e') {
                return omni_yyjson_mut_bool(doc, 1);
            }
            if (name.len == 5 && name[0] == 'f' && name[1] == 'a' && name[2] == 'l' && name[3] == 's' && name[4] == 'e') {
                return omni_yyjson_mut_bool(doc, 0);
            }
            // Other symbols → string
            return omni_yyjson_mut_strn(doc, name.ptr, name.len);
        }
        case CONS: {
            // List → JSON array
            YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
            Value* curr = val;
            while (is_cons(curr)) {
                YyjsonMutVal* elem = omni_to_json_val(car(curr), doc, interp);
                omni_yyjson_mut_arr_append(arr, elem);
                curr = cdr(curr);
            }
            return arr;
        }
        case ARRAY: {
            YyjsonMutVal* arr = omni_yyjson_mut_arr(doc);
            for (usz i = 0; i < val.array_val.length; i++) {
                YyjsonMutVal* elem = omni_to_json_val(val.array_val.items[i], doc, interp);
                omni_yyjson_mut_arr_append(arr, elem);
            }
            return arr;
        }
        case HASHMAP: {
            YyjsonMutVal* obj = omni_yyjson_mut_obj(doc);
            HashMap* hm = val.hashmap_val;
            for (usz i = 0; i < hm.capacity; i++) {
                HashEntry* entry = &hm.entries[i];
                if (entry.key != null) {
                    // Key as string
                    YyjsonMutVal* key;
                    if (is_string(entry.key)) {
                        key = omni_yyjson_mut_strn(doc, entry.key.str_val.chars, entry.key.str_val.len);
                    } else if (entry.key.tag == SYMBOL) {
                        char[] name = interp.symbols.get_name(entry.key.sym_val);
                        key = omni_yyjson_mut_strn(doc, name.ptr, name.len);
                    } else {
                        key = omni_yyjson_mut_strn(doc, "?", 1);
                    }
                    YyjsonMutVal* v = omni_to_json_val(entry.value, doc, interp);
                    omni_yyjson_mut_obj_add(obj, key, v);
                }
            }
            return obj;
        }
        default:
            return omni_yyjson_mut_null(doc);
    }
}

fn Value* json_emit_impl(Value*[] args, Interp* interp, bool pretty) {
    if (args.len < 1) return raise_error(interp, "json-emit: expected 1 argument");

    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return raise_error(interp, "json-emit: failed to create document");

    YyjsonMutVal* root = omni_to_json_val(args[0], doc, interp);
    omni_yyjson_mut_doc_set_root(doc, root);

    usz len = 0;
    char* json_str;
    if (pretty) {
        json_str = omni_yyjson_mut_write_pretty(doc, &len);
    } else {
        json_str = omni_yyjson_mut_write(doc, &len);
    }

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
