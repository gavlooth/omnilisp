module lisp;


import main;

// =============================================================================
// SECTION: STRING TRANSFORMATIONS
// =============================================================================
<*
@require interp != null
*>
fn Value* prim_string_append(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_string(interp, "");

foreach (arg : args) {
        if (!is_string(arg)) {
            return string_raise(interp, "type/arity", "string-append: expected string arguments");
        }
}

    DString ds;
    ds.init(mem);
foreach (arg : args) {
        ds.append_string((String)arg.str_chars[:arg.str_len]);
}
    Value* result = make_string(interp, ds.str_view());
    ds.free();
    return result;
}

/**
 * (string-upcase s) -> uppercase string
 */
<*
@require interp != null
*>
fn Value* prim_string_upcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-upcase: expected 1 argument");
    if (!is_string(args[0])) {
        return string_raise(interp, "type/arity", "string-upcase: expected string argument");
    }

    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    if (result != null && result.tag == ERROR) return result;
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
<*
@require interp != null
*>
fn Value* prim_string_downcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-downcase: expected 1 argument");
    if (!is_string(args[0])) {
        return string_raise(interp, "type/arity", "string-downcase: expected string argument");
    }

    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    if (result != null && result.tag == ERROR) return result;
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
<*
@require interp != null
*>
fn Value* prim_string_trim(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-trim: expected 1 argument");
    if (!is_string(args[0])) {
        return string_raise(interp, "type/arity", "string-trim: expected string argument");
    }

    char* str = args[0].str_chars;
    usz len = args[0].str_len;

    usz start = 0;
    while (start < len && (str[start] == ' ' || str[start] == '\t' ||
                           str[start] == '\n' || str[start] == '\r')) {
        start++;
    }

    usz end = len;
    while (end > start && (str[end - 1] == ' ' || str[end - 1] == '\t' ||
                           str[end - 1] == '\n' || str[end - 1] == '\r')) {
        end--;
    }

    if (start >= end) {
        return make_string(interp, "");
    }

    return make_string(interp, args[0].str_chars[start:end - start]);
}

/**
 * (string-join sep list) -> joined string
 * Join list of strings with separator
 */
<*
@require interp != null
*>
fn Value* prim_string_join(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return string_raise(interp, "type/arity", "string-join: expected 2 arguments");
    if (!is_string(args[0])) return string_raise(interp, "type/arg-mismatch", "string-join: expected string separator");

    if (is_nil(args[1])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[1])) {
        return string_raise(interp, "type/arity", "string-join: expected list as second argument");
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

        if (!is_string(elem)) {
            return string_raise(interp, "type/arg-mismatch", "string-join: expected string elements");
        }
        ds.append_string((String)elem.str_chars[:elem.str_len]);
    }
    if (!is_nil(list)) {
        return string_raise(interp, "type/arg-mismatch", "string-join: expected proper list");
    }

    return make_string(interp, ds.str_view());
}

/**
 * (substring s start end) -> substring
 */
<*
@require interp != null
*>
fn Value* prim_substring(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return string_raise(interp, "type/arity", "substring: expected 3 arguments");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "substring: expected string as first argument");
    if (!is_int(args[1]) || !is_int(args[2])) {
        return string_raise(interp, "type/arg-mismatch", "substring: expected integer indices");
    }

    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_len = utf8_strlen(bytes);
    long start = args[1].int_val;
    long end = args[2].int_val;

    if (start < 0) start += (long)cp_len;
    if (end < 0) end += (long)cp_len;

    if (start < 0) start = 0;
    if (end > (long)cp_len) end = (long)cp_len;
    if (start >= end) return make_string(interp, "");

    usz byte_start = utf8_byte_offset(bytes, (usz)start);
    usz byte_end = utf8_byte_offset(bytes, (usz)end);
    usz sub_len = byte_end - byte_start;
    return make_string(interp, bytes[byte_start:sub_len]);
}

/**
 * (string-split s sep) -> list of strings
 */
fn bool string_split_separator_matches(char* str, usz str_len, usz start, char* sep, usz sep_len) {
    if (sep_len == 0 || start > str_len || sep_len > str_len - start) return false;
    for (usz i = 0; i < sep_len; i++) {
        if (str[start + i] != sep[i]) return false;
    }
    return true;
}

<*
@require interp != null
*>
fn Value* prim_string_split(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return string_raise(interp, "type/arity", "string-split: expected 2 arguments");
    if (!is_string(args[0]) || !is_string(args[1])) {
        return string_raise(interp, "type/arity", "string-split: expected string arguments");
    }
    if (args[1].str_len == 0) {
        return string_raise(interp, "type/arg-mismatch", "string-split: empty separator");
    }

    char* str = args[0].str_chars;
    usz str_len = args[0].str_len;
    char* sep = args[1].str_chars;
    usz sep_len = args[1].str_len;

    Value* parts = make_nil(interp);
    usz start = 0;

    usz i = 0;
    while (i <= str_len) {
        bool at_end = i == str_len;
        bool at_sep = !at_end && string_split_separator_matches(str, str_len, i, sep, sep_len);
        if (at_end || at_sep) {
            usz part_len = i - start;
            Value* part;
            if (part_len == 0) {
                part = make_string(interp, "");
            } else {
                part = make_string(interp, args[0].str_chars[start:part_len]);
            }
            if (part != null && part.tag == ERROR) return part;
            Value* next = make_cons_or_error(interp, part, parts, "string-split: failed to allocate result list");
            if (next == null || next.tag == ERROR) return next;
            parts = next;
            if (at_sep) {
                i += sep_len;
                start = i;
            } else {
                i++;
            }
        } else {
            i++;
        }
    }

    Value* result = make_nil(interp);
    while (is_cons(parts)) {
        Value* next = make_cons_or_error(interp, parts.cons_val.car, result, "string-split: failed to allocate result list");
        if (next == null || next.tag == ERROR) return next;
        result = next;
        parts = parts.cons_val.cdr;
    }
    return result;
}
