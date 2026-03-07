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
extern fn YyjsonDoc* omni_yyjson_read(char* dat, usz len, usz flags) @extern("omni_yyjson_read");
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
extern fn char* omni_yyjson_mut_write_with_flags(YyjsonMutDoc* doc, usz flags, usz* len) @extern("omni_yyjson_mut_write_with_flags");
extern fn usz omni_yyjson_fp_to_fixed_flag(int precision) @extern("omni_yyjson_fp_to_fixed_flag");

extern fn void c_free_json(void* ptr) @extern("free");

// Parse option bits (mirrors yyjson read flag bits used by the C wrapper).
const usz JSON_PARSE_OPTION_ALLOW_COMMENTS = 1 << 3;
const usz JSON_PARSE_OPTION_ALLOW_TRAILING_COMMAS = 1 << 2;
const usz JSON_PARSE_OPTION_ALLOW_NAN_INF = 1 << 4;

const usz JSON_WRITE_OPTION_PRETTY = 1 << 0;
const usz JSON_WRITE_OPTION_ESCAPE_UNICODE = 1 << 1;
const usz JSON_WRITE_OPTION_ESCAPE_SLASHES = 1 << 2;
const usz JSON_WRITE_OPTION_ALLOW_INF_AND_NAN = 1 << 3;
const usz JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL = 1 << 4;
const usz JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE = 1 << 5;
const usz JSON_WRITE_OPTION_PRETTY_TWO_SPACES = 1 << 6;
const usz JSON_WRITE_OPTION_NEWLINE_AT_END = 1 << 7;

fn bool string_equals(char[] a, char[] b) @inline {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

struct JsonPointerToken {
    char[] text;
    bool ok;
    bool owned;
}

fn JsonPointerToken json_pointer_decode_token(char[] token) {
    JsonPointerToken result;
    result.text = token;
    result.ok = true;
    result.owned = false;

    bool has_escape = false;
    for (usz i = 0; i < token.len; i++) {
        if (token[i] == '~') {
            has_escape = true;
            break;
        }
    }
    if (!has_escape) return result;

    char* out = (char*)mem::malloc(token.len);
    if (out == null) {
        result.ok = false;
        return result;
    }

    usz out_len = 0;
    for (usz i = 0; i < token.len; ) {
        char ch = token[i];
        if (ch != '~') {
            out[out_len++] = ch;
            i++;
            continue;
        }

        if (i + 1 >= token.len) {
            mem::free(out);
            result.ok = false;
            return result;
        }

        char esc = token[i + 1];
        if (esc == '0') {
            out[out_len++] = '~';
        } else if (esc == '1') {
            out[out_len++] = '/';
        } else {
            mem::free(out);
            result.ok = false;
            return result;
        }
        i += 2;
    }

    result.text = out[:out_len];
    result.owned = true;
    return result;
}

fn void json_pointer_token_release(JsonPointerToken* token) {
    if (token.owned && token.text.ptr != null) {
        mem::free(token.text.ptr);
        token.text = {};
        token.owned = false;
    }
}

fn bool json_pointer_parse_array_index(char[] token, usz* out_idx) {
    if (token.len == 0) return false;

    usz idx = 0;
    for (usz i = 0; i < token.len; i++) {
        char ch = token[i];
        if (ch < '0' || ch > '9') return false;
        usz digit = (usz)(ch - '0');
        if (idx > (usz.max - digit) / 10) return false;
        idx = (idx * 10) + digit;
    }

    *out_idx = idx;
    return true;
}

fn Value* json_pointer_lookup(Value* value, char[] token, Interp* interp) {
    if (value == null) return make_nil(interp);

    if (value.tag == ARRAY) {
        usz idx = 0;
        if (!json_pointer_parse_array_index(token, &idx)) return make_nil(interp);
        if (idx >= value.array_val.length) return make_nil(interp);
        return value.array_val.items[idx];
    }

    if (value.tag == HASHMAP) {
        HashMap* map = value.hashmap_val;
        Value* by_string = hashmap_get(map, make_string(interp, token));
        if (by_string != null) return by_string;

        Value* by_symbol = make_symbol(interp, interp.symbols.intern(token));
        return hashmap_get(map, by_symbol);
    }

    return make_nil(interp);
}

fn usz json_parse_option_flag(char[] name) {
    if (string_equals(name, "allow-comments") || string_equals(name, "comments")) {
        return JSON_PARSE_OPTION_ALLOW_COMMENTS;
    }
    if (string_equals(name, "allow-trailing-commas") || string_equals(name, "trailing-commas")) {
        return JSON_PARSE_OPTION_ALLOW_TRAILING_COMMAS;
    }
    if (string_equals(name, "allow-nan-inf") || string_equals(name, "nan-inf") || string_equals(name, "naninf")) {
        return JSON_PARSE_OPTION_ALLOW_NAN_INF;
    }
    return 0;
}

struct JsonEmitOptions {
    bool pretty;
    bool has_pretty_override;
    usz flags;
    bool has_precision;
    int precision;
}

fn Value* parse_bool_option_value(
    Value* value,
    bool* out_value,
    char[] option_name,
    char[] context,
    Interp* interp
) {
    if (is_nil(value)) {
        *out_value = false;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_true) {
        *out_value = true;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_false) {
        *out_value = false;
        return null;
    }

    char[240] msg_buf;
    char[] msg = io::bprintf(&msg_buf, "%s: %s must be a boolean (true/false)", context, option_name)!!;
    return raise_error(interp, msg);
}

fn Value* parse_json_emit_option_entry(Value* key, Value* value, JsonEmitOptions* opts, Interp* interp) {
    if (!is_symbol(key)) {
        return raise_error(interp, "json-emit: options must be key/value pairs with symbol keys");
    }

    char[] key_name = interp.symbols.get_name(key.sym_val);

    if (string_equals(key_name, "pretty")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "pretty", "json-emit", interp);
        if (err != null) return err;
        opts.pretty = parsed;
        opts.has_pretty_override = true;
        return null;
    }
    if (string_equals(key_name, "pretty-two-spaces")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "pretty-two-spaces", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_PRETTY_TWO_SPACES;
            opts.flags &= ~JSON_WRITE_OPTION_PRETTY;
            return null;
        }
        opts.flags &= ~JSON_WRITE_OPTION_PRETTY_TWO_SPACES;
        return null;
    }
    if (string_equals(key_name, "escape-unicode")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "escape-unicode", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ESCAPE_UNICODE;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ESCAPE_UNICODE;
        }
        return null;
    }
    if (string_equals(key_name, "escape-slashes")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "escape-slashes", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ESCAPE_SLASHES;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ESCAPE_SLASHES;
        }
        return null;
    }
    if (string_equals(key_name, "allow-invalid-unicode")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "allow-invalid-unicode", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE;
        }
        return null;
    }
    if (string_equals(key_name, "allow-inf-and-nan")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "allow-inf-and-nan", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
            if ((opts.flags & JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL) != 0) {
                opts.flags &= ~JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
            }
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
        }
        return null;
    }
    if (string_equals(key_name, "inf-and-nan-as-null")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "inf-and-nan-as-null", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
            if ((opts.flags & JSON_WRITE_OPTION_ALLOW_INF_AND_NAN) != 0) {
                opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
            }
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
        }
        return null;
    }
    if (string_equals(key_name, "newline-at-end")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "newline-at-end", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_NEWLINE_AT_END;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_NEWLINE_AT_END;
        }
        return null;
    }
    if (string_equals(key_name, "fp-to-float")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "fp-to-float", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= (usz)(1 << (32 - 5));
        } else {
            opts.flags &= ~((usz)(1 << (32 - 5)));
        }
        return null;
    }
    if (string_equals(key_name, "precision") || string_equals(key_name, "fp-precision")) {
        if (value == null || value.tag != INT) {
            return raise_error(interp,
                "json-emit: precision must be an integer between 0 and 15");
        }
        if (value.int_val < 0 || value.int_val > 15) {
            return raise_error(interp,
                "json-emit: precision must be an integer between 0 and 15");
        }
        if (value.int_val == 0) {
            opts.has_precision = false;
            return null;
        }
        opts.has_precision = true;
        opts.precision = (int)value.int_val;
        return null;
    }

    return raise_error(interp,
        "json-emit: unknown option key (supported: pretty, pretty-two-spaces, escape-unicode, escape-slashes, allow-invalid-unicode, allow-inf-and-nan, inf-and-nan-as-null, newline-at-end, fp-to-float, precision)");
}

fn Value* parse_json_emit_options(Value* options, JsonEmitOptions* opts, Interp* interp) {
    if (options == null || options.tag == NIL) return null;
    if (!is_cons(options)) {
        return raise_error(interp,
            "json-emit: expected second argument to be an option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);

        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return raise_error(interp,
                    "json-emit: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_json_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return raise_error(interp,
                    "json-emit: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_json_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }

    return null;
}

fn Value* parse_json_options(Value* options, usz* flags_out, Interp* interp) {
    if (options == null || options.tag == NIL) return null;
    if (!is_cons(options)) {
        return raise_error(interp,
            "json-parse: expected second argument to be a list of option symbols");
    }

    usz flags = 0;
    Value* cur = options;
    while (is_cons(cur)) {
        Value* opt = car(cur);
        if (!is_symbol(opt)) {
            return raise_error(interp,
                "json-parse: options list must contain only symbols");
        }
        usz option_flag = json_parse_option_flag(interp.symbols.get_name(opt.sym_val));
        if (option_flag == 0) {
            return raise_error(interp,
                "json-parse: unknown option symbol (supported: allow-comments, allow-trailing-commas, allow-nan-inf)");
        }
        flags |= option_flag;
        cur = cdr(cur);
    }
    if (!is_nil(cur)) {
        return raise_error(interp,
            "json-parse: options list must be a proper list");
    }

    *flags_out = flags;
    return null;
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

// ============================================================
// json-emit: Omni value → JSON string
// ============================================================

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
