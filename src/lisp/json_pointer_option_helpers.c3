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
    bool allocation_failed;
}

bool g_json_pointer_force_decode_alloc_fail = false;

fn void json_pointer_force_decode_alloc_fail_for_test(bool enabled) {
    g_json_pointer_force_decode_alloc_fail = enabled;
}

fn JsonPointerToken json_pointer_decode_token(char[] token) {
    JsonPointerToken result;
    result.text = token;
    result.ok = true;
    result.owned = false;
    result.allocation_failed = false;

    bool has_escape = false;
    for (usz i = 0; i < token.len; i++) {
        if (token[i] == '~') {
            has_escape = true;
            break;
        }
    }
    if (!has_escape) return result;

    char* out = g_json_pointer_force_decode_alloc_fail ? null : (char*)mem::malloc(token.len);
    if (out == null) {
        result.ok = false;
        result.allocation_failed = true;
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

fn SymbolId json_pointer_lookup_existing_symbol(SymbolTable* symbols, char[] name) {
    if (symbols == null || symbols.entries == null || symbols.hash_index == null) return INVALID_SYMBOL_ID;
    if (symbols.capacity == 0 || symbols.hash_capacity == 0 || name.ptr == null) return INVALID_SYMBOL_ID;
    if (name.len > SYMBOL_MAX_NAME_LEN) return INVALID_SYMBOL_ID;

    usz h = hash_symbol(name);
    usz mask = symbols.hash_capacity - 1;
    for (usz probe = 0; probe < symbols.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        SymbolId sid = symbols.hash_index[slot];
        if (sid == INVALID_SYMBOL_ID) return INVALID_SYMBOL_ID;

        usz idx = (usz)sid;
        if (idx >= symbols.count) return INVALID_SYMBOL_ID;
        if (symbols.entries[idx].len != name.len) continue;

        bool match = true;
        for (usz j = 0; j < name.len; j++) {
            if (symbols.entries[idx].name[j] != name[j]) {
                match = false;
                break;
            }
        }
        if (match) return sid;
    }
    return INVALID_SYMBOL_ID;
}

fn bool json_pointer_parse_array_index(char[] token, usz* out_idx) {
    if (token.len == 0) return false;

    usz idx = 0;
foreach (item : token) {
        char ch = item;
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
        if (value.array_val == null) return make_nil(interp);
        usz idx = 0;
        if (!json_pointer_parse_array_index(token, &idx)) return make_nil(interp);
        if (idx >= value.array_val.length) return make_nil(interp);
        return value.array_val.items[idx];
    }

    if (value.tag == HASHMAP) {
        HashMap* map = value.hashmap_val;
        Value* key_v = make_string(interp, token);
        if (key_v == null) return runtime_raise(interp, "runtime/out-of-memory", "json-get: failed to allocate pointer token");
        if (key_v.tag == ERROR) return key_v;
        Value* by_string = hashmap_get(map, key_v);
        if (by_string != null) return by_string;

        SymbolId token_sym = json_pointer_lookup_existing_symbol(&interp.symbols, token);
        if (token_sym == INVALID_SYMBOL_ID) return make_nil(interp);
        Value* by_symbol = make_symbol(interp, token_sym);
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
    char[] msg;
    if (try char[] formatted = io::bprintf(&msg_buf, "%s: %s must be a boolean (true/false)", context, option_name)) {
        msg = formatted;
    } else {
        msg = "json-emit: option must be a boolean (true/false)";
    }
    return runtime_raise(interp, "runtime/evaluation-error", msg);
}
