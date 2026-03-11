module lisp;

import std::core::mem;
import std::io;

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
