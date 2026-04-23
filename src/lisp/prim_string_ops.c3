module lisp;


import main;

// =============================================================================
// SECTION: STRING OPERATIONS
// =============================================================================
// Search, indexing, and conversion primitives remain here. Transform and
// slice primitives now live in prim_string_transform.c3.

fn Value* prim_string_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return string_raise(interp, "type/arity", "string-contains?: expected 2 strings");
    char[] haystack = args[0].str_chars[:args[0].str_len];
    char[] needle = args[1].str_chars[:args[1].str_len];
    if (needle.len == 0) return make_symbol(interp, interp.sym_true);
    if (needle.len > haystack.len) return make_nil(interp);
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) { match = false; break; }
        }
        if (match) return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

fn Value* prim_string_index_of(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return string_raise(interp, "type/arity", "string-index-of: expected 2 strings");
    char[] haystack = args[0].str_chars[:args[0].str_len];
    char[] needle = args[1].str_chars[:args[1].str_len];
    if (needle.len == 0) return make_int(interp, 0);
    if (needle.len > haystack.len) return make_int(interp, -1);
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) { match = false; break; }
        }
        if (match) return make_int(interp, (long)i);
    }
    return make_int(interp, -1);
}

fn Value* prim_string_replace(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || args[0].tag != STRING || args[1].tag != STRING || args[2].tag != STRING) {
        return string_raise(interp, "type/arg-mismatch", "string-replace: expected 3 strings");
    }
    char[] str = args[0].str_chars[:args[0].str_len];
    char[] old = args[1].str_chars[:args[1].str_len];
    char[] new_str = args[2].str_chars[:args[2].str_len];
    if (old.len == 0) return args[0]; // nothing to replace
    StringVal* builder = strval_new(str.len);
    if (builder == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "string-replace: failed to allocate builder");
    }
    usz i = 0;
    while (i < str.len) {
        if (i + old.len <= str.len) {
            bool match = true;
            for (usz j = 0; j < old.len; j++) {
                if (str[i + j] != old[j]) { match = false; break; }
            }
            if (match) {
                if (!strval_append(builder, new_str)) {
                    return strval_raise_oom(builder, interp, "string-replace: failed to grow builder");
                }
                i += old.len;
                continue;
            }
        }
        if (!strval_push(builder, str[i])) {
            return strval_raise_oom(builder, interp, "string-replace: failed to grow builder");
        }
        i++;
    }
    return strval_materialize_result(builder, interp, "string-replace: failed to allocate result value");
}

fn Value* prim_char_at(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return string_raise(interp, "type/arg-mismatch", "char-at: expected string and int");
    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_count = utf8_strlen(bytes);
    long idx = args[1].int_val;
    if (idx < 0) idx += (long)cp_count;
    if (idx < 0 || idx >= (long)cp_count) return string_raise(interp, "type/arg-mismatch", "char-at: index out of range");
    // Find byte offset of the idx-th codepoint
    usz byte_off = utf8_byte_offset(bytes, (usz)idx);
    usz cplen = utf8_codepoint_len(bytes[byte_off]);
    if (byte_off + cplen > bytes.len) cplen = bytes.len - byte_off;
    return make_string(interp, bytes[byte_off:cplen]);
}

fn Value* prim_string_repeat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return string_raise(interp, "type/arg-mismatch", "string-repeat: expected string and int");
    long n = args[1].int_val;
    if (n <= 0) return make_string(interp, "");
    StringVal* builder = strval_new(args[0].str_len * (usz)n);
    if (builder == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "string-repeat: failed to allocate builder");
    }
    for (long i = 0; i < n; i++) {
        if (!strval_append(builder, args[0].str_chars[:args[0].str_len])) {
            return strval_raise_oom(builder, interp, "string-repeat: failed to grow builder");
        }
    }
    return strval_materialize_result(builder, interp, "string-repeat: failed to allocate result value");
}

/**
 * (string-length s) -> int
 */
fn Value* prim_string_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_int(interp, 0);
    }
    // Count UTF-8 codepoints, not bytes
    usz cp_len = utf8_strlen(args[0].str_chars[:args[0].str_len]);
    return make_int(interp, (long)cp_len);
}

fn Value* prim_string_byte_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_int(interp, 0);
    }
    return make_int(interp, (long)args[0].str_len);
}

/**
 * List(String) helper: split a string into one-codepoint strings.
 */
fn Value* string_to_list_value(Value* arg, Interp* interp, char[] expected_msg, char[] alloc_msg) {
    if (!is_string(arg)) {
        return string_raise(interp, "type/arg-mismatch", expected_msg);
    }

    char[] bytes = arg.str_chars[:arg.str_len];

    // First pass: count codepoints
    usz cp_count = 0;
    usz pos = 0;
    while (pos < bytes.len) {
        usz cplen = utf8_codepoint_len(bytes[pos]);
        if (pos + cplen > bytes.len) cplen = bytes.len - pos;
        pos += cplen;
        cp_count++;
    }

    if (cp_count == 0) return make_nil(interp);

    // Allocate exact-size arrays via temp allocator
    usz[] offsets = mem::temp_array(usz, cp_count);
    usz[] cp_lens = mem::temp_array(usz, cp_count);

    // Second pass: fill offsets and lens
    pos = 0;
    for (usz i = 0; i < cp_count; i++) {
        offsets[i] = pos;
        usz cplen = utf8_codepoint_len(bytes[pos]);
        if (pos + cplen > bytes.len) cplen = bytes.len - pos;
        cp_lens[i] = cplen;
        pos += cplen;
    }

    // Build list in reverse order
    Value* result = make_nil(interp);
    for (isz i = (isz)cp_count - 1; i >= 0; i--) {
        usz off = offsets[(usz)i];
        usz clen = cp_lens[(usz)i];
        Value* char_str = make_string(interp, bytes[off:clen]);
        if (char_str != null && char_str.tag == ERROR) return char_str;
        result = make_cons_or_error(interp, char_str, result, alloc_msg);
        if (result == null || result.tag == ERROR) return result;
    }
    return result;
}

fn Value* prim_string_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string->list: expected 1 argument");
    return string_to_list_value(args[0], interp,
        "string->list: expected string argument",
        "string->list: failed to allocate result list");
}

/**
 * String(List) helper: concatenate a proper list of string fragments.
 */
fn Value* list_to_string_value(Value* arg, Interp* interp, char[] expected_list_msg, char[] expected_elem_msg, char[] proper_list_msg) {
    // Allow nil to return empty string
    if (is_nil(arg)) {
        return make_string(interp, "");
    }

    if (!is_cons(arg)) {
        return string_raise(interp, "type/arg-mismatch", expected_list_msg);
    }

    DString ds;
    ds.init(mem);
    defer ds.free();

    Value* list = arg;
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (!is_string(elem)) {
            return string_raise(interp, "type/arg-mismatch", expected_elem_msg);
        }
        ds.append_string((String)elem.str_chars[:elem.str_len]);
    }
    if (!is_nil(list)) {
        return string_raise(interp, "type/arg-mismatch", proper_list_msg);
    }

    return make_string(interp, ds.str_view());
}

fn Value* prim_list_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return string_raise(interp, "type/arity", "list->string: expected 1 argument");
    }
    return list_to_string_value(args[0], interp,
        "list->string: expected a list",
        "list->string: expected string elements",
        "list->string: expected a proper list");
}
