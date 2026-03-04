module lisp;

import std::io;
import main;

// =============================================================================
// SECTION: STRING OPERATIONS
// =============================================================================
fn Value* prim_string_append(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_string(interp, "");

    // Validate all args are strings
    for (usz i = 0; i < args.len; i++) {
        if (!is_string(args[i])) {
            return raise_error(interp, "string-append: expected string arguments");
        }
    }

    DString ds;
    ds.init(mem);
    for (usz i = 0; i < args.len; i++) {
        ds.append_string((String)args[i].str_chars[:args[i].str_len]);
    }
    Value* result = make_string(interp, ds.str_view());
    ds.free();
    return result;
}

/**
 * (string-upcase s) -> uppercase string
 */
fn Value* prim_string_upcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-upcase: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-upcase: expected string argument");
    }

    // Allocate result, then transform in place
    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    for (usz i = 0; i < result.str_len; i++) {
        char c = result.str_chars[i];
        if (c >= 'a' && c <= 'z') {
            result.str_chars[i] = c - 32;
        }
    }
    return result;
}

/**
 * (string-downcase s) -> lowercase string
 */
fn Value* prim_string_downcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-downcase: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-downcase: expected string argument");
    }

    // Allocate result, then transform in place
    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    for (usz i = 0; i < result.str_len; i++) {
        char c = result.str_chars[i];
        if (c >= 'A' && c <= 'Z') {
            result.str_chars[i] = c + 32;
        }
    }
    return result;
}

/**
 * (string-trim s) -> string with leading/trailing whitespace removed
 */
fn Value* prim_string_trim(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-trim: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-trim: expected string argument");
    }

    char* str = args[0].str_chars;
    usz len = args[0].str_len;

    // Find start (skip leading whitespace)
    usz start = 0;
    while (start < len && (str[start] == ' ' || str[start] == '\t' ||
                           str[start] == '\n' || str[start] == '\r')) {
        start++;
    }

    // Find end (skip trailing whitespace)
    usz end = len;
    while (end > start && (str[end - 1] == ' ' || str[end - 1] == '\t' ||
                           str[end - 1] == '\n' || str[end - 1] == '\r')) {
        end--;
    }

    // Empty result
    if (start >= end) {
        return make_string(interp, "");
    }

    // Direct slice — no temp buffer needed
    return make_string(interp, args[0].str_chars[start:end - start]);
}

/**
 * (string-join sep list) -> joined string
 * Join list of strings with separator
 */
fn Value* prim_string_join(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-join: expected 2 arguments");
    if (!is_string(args[0])) return raise_error(interp, "string-join: expected string separator");

    // Allow nil list to return empty string
    if (is_nil(args[1])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[1])) {
        return raise_error(interp, "string-join: expected list as second argument");
    }

    char[] sep = args[0].str_chars[:args[0].str_len];

    DString ds;
    ds.init(mem);
    defer ds.free();
    bool first = true;

    Value* list = args[1];
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (!first) {
            ds.append_string((String)sep);
        }
        first = false;

        if (is_string(elem)) {
            ds.append_string((String)elem.str_chars[:elem.str_len]);
        }
    }

    return make_string(interp, ds.str_view());
}

/**
 * (substring s start end) -> substring
 */
fn Value* prim_substring(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "substring: expected 3 arguments");
    if (!is_string(args[0])) return raise_error(interp, "substring: expected string as first argument");
    if (!is_int(args[1]) || !is_int(args[2])) {
        return raise_error(interp, "substring: expected integer indices");
    }

    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_len = utf8_strlen(bytes);
    long start = args[1].int_val;
    long end = args[2].int_val;

    // Handle negative indices (Python-style) — based on codepoint count
    if (start < 0) start += (long)cp_len;
    if (end < 0) end += (long)cp_len;

    // Clamp to bounds
    if (start < 0) start = 0;
    if (end > (long)cp_len) end = (long)cp_len;
    if (start >= end) return make_string(interp, "");

    // Convert codepoint indices to byte offsets
    usz byte_start = utf8_byte_offset(bytes, (usz)start);
    usz byte_end = utf8_byte_offset(bytes, (usz)end);
    usz sub_len = byte_end - byte_start;
    return make_string(interp, bytes[byte_start:sub_len]);
}

/**
 * (string-split s sep) -> list of strings
 */
fn Value* prim_string_split(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-split: expected 2 arguments");
    if (!is_string(args[0]) || !is_string(args[1])) {
        return raise_error(interp, "string-split: expected string arguments");
    }
    if (args[1].str_len == 0) {
        return raise_error(interp, "string-split: empty separator");
    }

    char* str = args[0].str_chars;
    usz str_len = args[0].str_len;
    // NOTE: Currently only supports single-character delimiters.
    // The first character of the separator string is used.
    char delim = args[1].str_chars[0];

    // Collect parts in reverse order, then reverse
    Value* parts = make_nil(interp);
    usz start = 0;

    for (usz i = 0; i <= str_len; i++) {
        if (i == str_len || str[i] == delim) {
            // Extract substring [start..i) — direct slice, no temp buffer
            usz part_len = i - start;
            Value* part;
            if (part_len == 0) {
                part = make_string(interp, "");
            } else {
                part = make_string(interp, args[0].str_chars[start:part_len]);
            }
            parts = make_cons(interp, part, parts);
            start = i + 1;
        }
    }

    // Reverse the list
    Value* result = make_nil(interp);
    while (is_cons(parts)) {
        result = make_cons(interp, parts.cons_val.car, result);
        parts = parts.cons_val.cdr;
    }
    return result;
}

fn Value* prim_string_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return raise_error(interp, "string-contains?: expected 2 strings");
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
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return raise_error(interp, "string-index-of: expected 2 strings");
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
        return raise_error(interp, "string-replace: expected 3 strings");
    }
    char[] str = args[0].str_chars[:args[0].str_len];
    char[] old = args[1].str_chars[:args[1].str_len];
    char[] new_str = args[2].str_chars[:args[2].str_len];
    if (old.len == 0) return args[0]; // nothing to replace
    StringVal* builder = strval_new(str.len);
    usz i = 0;
    while (i < str.len) {
        if (i + old.len <= str.len) {
            bool match = true;
            for (usz j = 0; j < old.len; j++) {
                if (str[i + j] != old[j]) { match = false; break; }
            }
            if (match) {
                strval_ensure(builder, new_str.len);
                for (usz j = 0; j < new_str.len; j++) {
                    builder.chars[builder.len++] = new_str[j];
                }
                i += old.len;
                continue;
            }
        }
        strval_push(builder, str[i]);
        i++;
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
}

fn Value* prim_char_at(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return raise_error(interp, "char-at: expected string and int");
    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_count = utf8_strlen(bytes);
    long idx = args[1].int_val;
    if (idx < 0) idx += (long)cp_count;
    if (idx < 0 || idx >= (long)cp_count) return raise_error(interp, "char-at: index out of range");
    // Find byte offset of the idx-th codepoint
    usz byte_off = utf8_byte_offset(bytes, (usz)idx);
    usz cplen = utf8_codepoint_len(bytes[byte_off]);
    if (byte_off + cplen > bytes.len) cplen = bytes.len - byte_off;
    return make_string(interp, bytes[byte_off:cplen]);
}

fn Value* prim_string_repeat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return raise_error(interp, "string-repeat: expected string and int");
    long n = args[1].int_val;
    if (n <= 0) return make_string(interp, "");
    StringVal* builder = strval_new(args[0].str_len * (usz)n);
    for (long i = 0; i < n; i++) {
        strval_ensure(builder, args[0].str_len);
        for (usz j = 0; j < args[0].str_len; j++) {
            builder.chars[builder.len++] = args[0].str_chars[j];
        }
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
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
 * (string->list s) -> list of single-char strings
 */
fn Value* prim_string_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string->list: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string->list: expected string argument");
    }

    char[] bytes = args[0].str_chars[:args[0].str_len];

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
        result = make_cons(interp, char_str, result);
    }
    return result;
}

/**
 * (list->string chars) -> string
 * Convert a list of single-character strings to a string
 */
fn Value* prim_list_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return raise_error(interp, "list->string: expected 1 argument");
    }

    // Allow nil to return empty string
    if (is_nil(args[0])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[0])) {
        return raise_error(interp, "list->string: expected a list");
    }

    DString ds;
    ds.init(mem);
    defer ds.free();

    Value* list = args[0];
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (is_string(elem) && elem.str_len > 0) {
            ds.append_char(elem.str_chars[0]);
        }
    }

    return make_string(interp, ds.str_view());
}

