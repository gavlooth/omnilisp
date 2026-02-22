module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 7: PRIMITIVES
// =============================================================================

// Curried primitive helper - stores first argument and waits for second
struct CurriedPrim {
    Value* first_arg;
    PrimitiveFn binary_fn;
}

fn Value* prim_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "+: expected at least 1 argument");

    if (args.len == 1) {
        if (!is_number(args[0])) return make_error(interp, "+: expected number argument");
        return args[0];
    }

    if (!is_number(args[0])) return make_error(interp, "+: expected number argument");
    if (!is_number(args[1])) return make_error(interp, "+: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) + to_double(args[1]));
    }
    return make_int(interp, args[0].int_val + args[1].int_val);
}

fn Value* prim_sub(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "-: expected at least 1 argument");
    if (args.len == 1) {
        if (!is_number(args[0])) return make_error(interp, "-: expected number argument");
        if (is_double(args[0])) return make_double(interp, -args[0].double_val);
        return make_int(interp, -args[0].int_val);
    }
    if (!is_number(args[0])) return make_error(interp, "-: expected number argument");
    if (!is_number(args[1])) return make_error(interp, "-: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) - to_double(args[1]));
    }
    return make_int(interp, args[0].int_val - args[1].int_val);
}

fn Value* prim_mul(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "*: expected 2 arguments");
    if (!is_number(args[0])) return make_error(interp, "*: expected number argument");
    if (!is_number(args[1])) return make_error(interp, "*: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) * to_double(args[1]));
    }
    return make_int(interp, args[0].int_val * args[1].int_val);
}

fn Value* prim_div(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "/: expected 2 arguments");
    if (!is_number(args[0])) return make_error(interp, "/: expected number argument");
    if (!is_number(args[1])) return make_error(interp, "/: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        double b = to_double(args[1]);
        if (b == 0.0) return make_error(interp, "/: division by zero");
        return make_double(interp, to_double(args[0]) / b);
    }
    long b = args[1].int_val;
    if (b == 0) return make_error(interp, "/: division by zero");
    return make_int(interp, args[0].int_val / b);
}

fn Value* prim_mod(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "modulo: expected 2 arguments");
    if (!is_number(args[0])) return make_error(interp, "modulo: expected number argument");
    if (!is_number(args[1])) return make_error(interp, "modulo: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_error(interp, "modulo: expected integer arguments");
    }
    long a = args[0].int_val;
    long b = args[1].int_val;
    if (b == 0) return make_error(interp, "modulo: division by zero");
    return make_int(interp, a % b);
}

fn Value* prim_eq(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "=: expected 2 arguments");
    bool eq = values_equal(args[0], args[1]);
    return eq ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_lt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "<: expected 2 arguments");
    if (!is_number(args[0]) || !is_number(args[1])) return make_error(interp, "<: expected number arguments");
    bool lt = to_double(args[0]) < to_double(args[1]);
    return lt ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_gt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, ">: expected 2 arguments");
    if (!is_number(args[0]) || !is_number(args[1])) return make_error(interp, ">: expected number arguments");
    bool gt = to_double(args[0]) > to_double(args[1]);
    return gt ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_le(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "<=: expected 2 arguments");
    if (!is_number(args[0]) || !is_number(args[1])) return make_error(interp, "<=: expected number arguments");
    bool le = to_double(args[0]) <= to_double(args[1]);
    return le ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_ge(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, ">=: expected 2 arguments");
    if (!is_number(args[0]) || !is_number(args[1])) return make_error(interp, ">=: expected number arguments");
    bool ge = to_double(args[0]) >= to_double(args[1]);
    return ge ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_cons(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "cons: expected 2 arguments");
    return make_cons(interp, args[0], args[1]);
}

fn Value* prim_car(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "car: expected 1 argument");
    if (!is_cons(args[0])) return make_error(interp, "car: argument must be a pair");
    return args[0].cons_val.car;
}

fn Value* prim_cdr(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "cdr: expected 1 argument");
    if (!is_cons(args[0])) return make_error(interp, "cdr: argument must be a pair");
    return args[0].cons_val.cdr;
}

fn Value* prim_null(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_symbol(interp, interp.sym_true);
    bool is_null = is_nil(args[0]);
    return is_null ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_pair(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool is_pair = is_cons(args[0]);
    return is_pair ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (length lst) -> int
 * Returns the number of elements in a list
 */
fn Value* prim_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "length: expected 1 argument");

    // nil has length 0
    if (is_nil(args[0])) {
        return make_int(interp, 0);
    }

    // String length
    if (is_string(args[0])) {
        return make_int(interp, (long)args[0].str_val.len);
    }

    // Array length
    if (is_array(args[0])) {
        return make_int(interp, (long)args[0].array_val.length);
    }

    // Dict count
    if (args[0] != null && args[0].tag == HASHMAP) {
        return make_int(interp, (long)args[0].hashmap_val.count);
    }

    // Count cons cells
    if (!is_cons(args[0])) {
        return make_error(interp, "length: expected list, array, dict, or string");
    }

    long count = 0;
    Value* current = args[0];
    while (is_cons(current)) {
        count++;
        current = current.cons_val.cdr;
    }
    return make_int(interp, count);
}

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        print_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        println_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    io::printn("");
    return make_nil(interp);
}

fn Value* prim_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_symbol(interp, interp.sym_true);
    bool falsy = is_falsy(args[0], interp);
    return falsy ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_nil(interp);

    // Single array arg → convert to list
    if (args.len == 1 && is_array(args[0])) {
        Array* arr = args[0].array_val;
        Value* result = make_nil(interp);
        for (isz i = (isz)arr.length - 1; i >= 0; i--) {
            result = make_cons(interp, arr.items[(usz)i], result);
        }
        return result;
    }

    Value* result = make_nil(interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) {
        result = make_cons(interp, args[(usz)i], result);
    }
    return result;
}

// =============================================================================

// =============================================================================
// SECTION 7.5: STRING PRIMITIVES
// =============================================================================

/**
 * (string-append s1 s2 ...) -> concatenated string
 * Variadic: concatenates all string arguments
 */
fn Value* prim_string_append(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_string(interp, "");

    // Calculate total length and validate all args are strings
    usz total_len = 0;
    for (usz i = 0; i < args.len; i++) {
        if (!is_string(args[i])) {
            return make_error(interp, "string-append: expected string arguments");
        }
        total_len += args[i].str_val.len;
    }

    // Clamp to max length
    if (total_len > MAX_STRING_LEN - 1) total_len = MAX_STRING_LEN - 1;

    char[MAX_STRING_LEN] buffer;
    usz pos = 0;

    for (usz i = 0; i < args.len && pos < MAX_STRING_LEN - 1; i++) {
        usz arg_len = args[i].str_val.len;
        for (usz j = 0; j < arg_len && pos < MAX_STRING_LEN - 1; j++) {
            buffer[pos++] = args[i].str_val.chars[j];
        }
    }
    buffer[pos] = 0;

    return make_string(interp, buffer[:pos]);
}

/**
 * (string-join sep list) -> joined string
 * Join list of strings with separator
 */
fn Value* prim_string_join(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "string-join: expected 2 arguments");
    if (!is_string(args[0])) return make_error(interp, "string-join: expected string separator");

    // Allow nil list to return empty string
    if (is_nil(args[1])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[1])) {
        return make_error(interp, "string-join: expected list as second argument");
    }

    char* sep = &args[0].str_val.chars;
    usz sep_len = args[0].str_val.len;

    char[MAX_STRING_LEN] buffer;
    usz pos = 0;
    bool first = true;

    Value* list = args[1];
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (!first && pos < MAX_STRING_LEN - 1) {
            // Add separator
            for (usz i = 0; i < sep_len && pos < MAX_STRING_LEN - 1; i++) {
                buffer[pos++] = sep[i];
            }
        }
        first = false;

        if (is_string(elem)) {
            for (usz i = 0; i < elem.str_val.len && pos < MAX_STRING_LEN - 1; i++) {
                buffer[pos++] = elem.str_val.chars[i];
            }
        }
    }
    buffer[pos] = 0;

    // Truncate if too long
    if (pos > MAX_STRING_LEN - 1) pos = MAX_STRING_LEN - 1;
    return make_string(interp, buffer[:pos]);
}

/**
 * (substring s start end) -> substring
 */
fn Value* prim_substring(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return make_error(interp, "substring: expected 3 arguments");
    if (!is_string(args[0])) return make_error(interp, "substring: expected string as first argument");
    if (!is_int(args[1]) || !is_int(args[2])) {
        return make_error(interp, "substring: expected integer indices");
    }

    usz len = args[0].str_val.len;
    long start = args[1].int_val;
    long end = args[2].int_val;

    // Handle negative indices (Python-style)
    if (start < 0) start = (long)len + start;
    if (end < 0) end = (long)len + end;

    // Clamp to bounds
    if (start < 0) start = 0;
    if (end > (long)len) end = (long)len;
    if (start >= end) return make_string(interp, "");

    usz sub_len = (usz)(end - start);
    char[MAX_STRING_LEN] buffer;
    for (usz i = 0; i < sub_len && i < MAX_STRING_LEN - 1; i++) {
        buffer[i] = args[0].str_val.chars[(usz)start + i];
    }
    if (sub_len > MAX_STRING_LEN - 1) sub_len = MAX_STRING_LEN - 1;
    buffer[sub_len] = 0;

    return make_string(interp, buffer[:sub_len]);
}

/**
 * (string-split s sep) -> list of strings
 */
fn Value* prim_string_split(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "string-split: expected 2 arguments");
    if (!is_string(args[0]) || !is_string(args[1])) {
        return make_error(interp, "string-split: expected string arguments");
    }
    if (args[1].str_val.len == 0) {
        return make_error(interp, "string-split: empty separator");
    }

    char* str = &args[0].str_val.chars;
    usz str_len = args[0].str_val.len;
    // NOTE: Currently only supports single-character delimiters.
    // The first character of the separator string is used.
    char delim = args[1].str_val.chars[0];

    // Collect parts in reverse order, then reverse
    Value* parts = make_nil(interp);
    usz start = 0;

    for (usz i = 0; i <= str_len; i++) {
        if (i == str_len || str[i] == delim) {
            // Extract substring [start..i)
            usz part_len = i - start;
            if (part_len > MAX_STRING_LEN - 1) part_len = MAX_STRING_LEN - 1;
            char[MAX_STRING_LEN] buffer;
            for (usz j = 0; j < part_len; j++) {
                buffer[j] = str[start + j];
            }
            buffer[part_len] = 0;
            Value* part = make_string(interp, buffer[:part_len]);
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

/**
 * (string-length s) -> int
 */
fn Value* prim_string_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_int(interp, 0);
    }
    return make_int(interp, (long)args[0].str_val.len);
}

/**
 * (string->list s) -> list of single-char strings
 */
fn Value* prim_string_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "string->list: expected 1 argument");
    if (!is_string(args[0])) {
        return make_error(interp, "string->list: expected string argument");
    }

    Value* result = make_nil(interp);
    usz len = args[0].str_val.len;

    // Build list in reverse order
    for (isz i = (isz)len - 1; i >= 0; i--) {
        char[2] ch;
        ch[0] = args[0].str_val.chars[(usz)i];
        ch[1] = 0;
        Value* char_str = make_string(interp, ch[0..1]);
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
        return make_error(interp, "list->string: expected 1 argument");
    }

    // Allow nil to return empty string
    if (is_nil(args[0])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[0])) {
        return make_error(interp, "list->string: expected a list");
    }

    char[MAX_STRING_LEN] buffer;
    usz pos = 0;

    Value* list = args[0];
    while (is_cons(list) && pos < MAX_STRING_LEN - 1) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (is_string(elem) && elem.str_val.len > 0) {
            // Take first character of the string
            buffer[pos++] = elem.str_val.chars[0];
        }
    }
    buffer[pos] = 0;

    return make_string(interp, buffer[:pos]);
}

/**
 * (string-upcase s) -> uppercase string
 */
fn Value* prim_string_upcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "string-upcase: expected 1 argument");
    if (!is_string(args[0])) {
        return make_error(interp, "string-upcase: expected string argument");
    }

    char[MAX_STRING_LEN] buffer;
    usz len = args[0].str_val.len;
    if (len > MAX_STRING_LEN - 1) len = MAX_STRING_LEN - 1;

    for (usz i = 0; i < len; i++) {
        char c = args[0].str_val.chars[i];
        // Convert lowercase a-z to uppercase A-Z
        if (c >= 'a' && c <= 'z') {
            buffer[i] = c - 32;  // 'a' - 'A' = 32
        } else {
            buffer[i] = c;
        }
    }
    buffer[len] = 0;

    return make_string(interp, buffer[:len]);
}

/**
 * (string-downcase s) -> lowercase string
 */
fn Value* prim_string_downcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "string-downcase: expected 1 argument");
    if (!is_string(args[0])) {
        return make_error(interp, "string-downcase: expected string argument");
    }

    char[MAX_STRING_LEN] buffer;
    usz len = args[0].str_val.len;
    if (len > MAX_STRING_LEN - 1) len = MAX_STRING_LEN - 1;

    for (usz i = 0; i < len; i++) {
        char c = args[0].str_val.chars[i];
        // Convert uppercase A-Z to lowercase a-z
        if (c >= 'A' && c <= 'Z') {
            buffer[i] = c + 32;  // 'a' - 'A' = 32
        } else {
            buffer[i] = c;
        }
    }
    buffer[len] = 0;

    return make_string(interp, buffer[:len]);
}

/**
 * (string-trim s) -> string with leading/trailing whitespace removed
 */
fn Value* prim_string_trim(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "string-trim: expected 1 argument");
    if (!is_string(args[0])) {
        return make_error(interp, "string-trim: expected string argument");
    }

    char* str = &args[0].str_val.chars;
    usz len = args[0].str_val.len;

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

    // Copy trimmed string
    usz trim_len = end - start;
    if (trim_len > MAX_STRING_LEN - 1) trim_len = MAX_STRING_LEN - 1;

    char[MAX_STRING_LEN] buffer;
    for (usz i = 0; i < trim_len; i++) {
        buffer[i] = str[start + i];
    }
    buffer[trim_len] = 0;

    return make_string(interp, buffer[:trim_len]);
}

/**
 * (string? v) -> true if v is a string
 */
fn Value* prim_is_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = is_string(args[0]);
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (int? v) -> true if v is an integer
 */
fn Value* prim_is_int(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = is_int(args[0]);
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (symbol? v) -> true if v is a symbol
 */
fn Value* prim_is_symbol(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = args[0] != null && args[0].tag == SYMBOL;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (closure? v) -> true if v is a closure (user-defined function)
 */
fn Value* prim_is_closure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = args[0] != null && args[0].tag == CLOSURE;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (continuation? v) -> true if v is a continuation
 */
fn Value* prim_is_continuation(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = args[0] != null && args[0].tag == CONTINUATION;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

// =============================================================================
// SECTION 7.6: FILE I/O PRIMITIVES
// =============================================================================

/**
 * (read-file path) -> string contents or nil on error
 */
fn Value* prim_read_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_val.chars[:args[0].str_val.len];

    // Try to load file using C3's file::load_temp
    if (try content = io::file::load_temp((String)path)) {
        return make_string(interp, content);
    }
    return make_nil(interp);
}

/**
 * (write-file path content) -> true on success, nil on error
 */
fn Value* prim_write_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return make_nil(interp);
    }

    // Get path and content as slices (using :len for exclusive end)
    char[] path = args[0].str_val.chars[:args[0].str_val.len];
    char[] content = args[1].str_val.chars[:args[1].str_val.len];

    // Open file for writing
    if (try file = io::file::open((String)path, "w")) {
        if (try bytes_written = file.write(content)) {
            file.close()!!;
            return make_symbol(interp, interp.sym_true);
        }
        file.close()!!;
    }
    return make_nil(interp);
}

/**
 * (file-exists? path) -> true if file exists, nil otherwise
 */
fn Value* prim_file_exists(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_val.chars[:args[0].str_val.len];

    // Use file::is_file to check existence
    if (io::file::is_file((String)path)) {
        return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

/**
 * (read-lines path) -> list of strings (one per line)
 */
fn Value* prim_read_lines(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_val.chars[:args[0].str_val.len];

    // Try to load file using C3's file::load_temp
    if (try content = io::file::load_temp((String)path)) {
        // Split content into lines
        Value* lines = make_nil(interp);
        usz start = 0;
        usz content_len = content.len;

        for (usz i = 0; i <= content_len; i++) {
            if (i == content_len || content[i] == '\n') {
                // Extract line [start..i)
                usz line_len = i - start;
                // Skip trailing \r if present (Windows line endings)
                if (line_len > 0 && content[start + line_len - 1] == '\r') {
                    line_len--;
                }
                if (line_len > MAX_STRING_LEN - 1) line_len = MAX_STRING_LEN - 1;
                char[MAX_STRING_LEN] buffer;
                for (usz j = 0; j < line_len; j++) {
                    buffer[j] = content[start + j];
                }
                buffer[line_len] = 0;
                Value* line = make_string(interp, buffer[:line_len]);
                lines = make_cons(interp, line, lines);
                start = i + 1;
            }
        }

        // Reverse the list to get correct order
        Value* result = make_nil(interp);
        while (is_cons(lines)) {
            result = make_cons(interp, lines.cons_val.car, result);
            lines = lines.cons_val.cdr;
        }
        return result;
    }
    return make_nil(interp);
}

/**
 * (load path) -> evaluates all expressions in the file, returns last result
 * Reads a script file and evaluates it in the current environment.
 */
fn Value* prim_load(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_val.chars[:args[0].str_val.len];

    // Try to load file
    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;

        // Run all expressions in the file
        EvalResult r = run_program(source, interp);
        if (r.error.has_error) {
            return make_nil(interp);
        }
        return r.value;
    }
    return make_nil(interp);
}

fn Value* prim_string_to_number(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) return make_nil(interp);
    char[] s = args[0].str_val.chars[:args[0].str_val.len];
    if (s.len == 0) return make_nil(interp);
    // Check if it contains a dot or exponent → parse as float
    bool is_float = false;
    for (usz j = 0; j < s.len; j++) {
        if (s[j] == '.' || s[j] == 'e' || s[j] == 'E') { is_float = true; break; }
    }
    if (is_float) {
        // Parse as double: [+-]digits[.digits][e[+-]digits]
        usz i = 0;
        bool negative = false;
        if (i < s.len && (s[i] == '-' || s[i] == '+')) { negative = s[i] == '-'; i++; }
        if (i >= s.len) return make_nil(interp);
        double val = 0.0;
        bool has_digits = false;
        while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val * 10.0 + (double)(s[i] - '0'); i++; has_digits = true; }
        if (i < s.len && s[i] == '.') {
            i++;
            double frac = 0.1;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val + (double)(s[i] - '0') * frac; frac = frac * 0.1; i++; has_digits = true; }
        }
        if (!has_digits) return make_nil(interp);
        if (i < s.len && (s[i] == 'e' || s[i] == 'E')) {
            i++;
            bool exp_neg = false;
            if (i < s.len && (s[i] == '-' || s[i] == '+')) { exp_neg = s[i] == '-'; i++; }
            long exp_val = 0;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') { exp_val = exp_val * 10 + (long)(s[i] - '0'); i++; }
            double mult = 1.0;
            for (long e = 0; e < exp_val; e++) mult = mult * 10.0;
            if (exp_neg) { val = val / mult; } else { val = val * mult; }
        }
        if (i != s.len) return make_nil(interp);
        if (negative) val = -val;
        return make_double(interp, val);
    }
    // Integer parse
    usz i = 0;
    bool negative = false;
    if (s[0] == '-') { negative = true; i = 1; }
    if (i >= s.len) return make_nil(interp);
    long val = 0;
    while (i < s.len) {
        if (s[i] < '0' || s[i] > '9') return make_nil(interp);
        val = val * 10 + (long)(s[i] - '0');
        i++;
    }
    if (negative) val = -val;
    return make_int(interp, val);
}

fn Value* prim_number_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "number->string: expected number");
    if (is_double(args[0])) {
        char[64] dbuf;
        char[] dslice = io::bprintf(&dbuf, "%.15g", args[0].double_val)!!;
        return make_string(interp, dslice);
    }
    long n = args[0].int_val;
    char[24] buf;
    usz len = 0;
    if (n == 0) {
        buf[0] = '0';
        len = 1;
    } else {
        bool negative = n < 0;
        if (negative) n = -n;
        // Reverse digit extraction
        usz start = 0;
        while (n > 0) {
            buf[len] = (char)('0' + (char)(n % 10));
            len++;
            n = n / 10;
        }
        if (negative) { buf[len] = '-'; len++; }
        // Reverse in-place
        for (usz j = 0; j < len / 2; j++) {
            char tmp = buf[j];
            buf[j] = buf[len - 1 - j];
            buf[len - 1 - j] = tmp;
        }
    }
    return make_string(interp, buf[:len]);
}

fn Value* prim_gensym(Value*[] args, Env* env, Interp* interp) {
    char[64] buf;
    buf[0] = 'g'; buf[1] = '#';
    usz len = 2;
    interp.gensym_counter++;
    long n = (long)interp.gensym_counter;
    // Convert number to string
    char[20] digits;
    usz dlen = 0;
    if (n == 0) { digits[0] = '0'; dlen = 1; }
    else {
        while (n > 0) { digits[dlen] = (char)('0' + (char)(n % 10)); dlen++; n = n / 10; }
    }
    for (usz j = 0; j < dlen; j++) {
        buf[len + j] = digits[dlen - 1 - j];
    }
    len += dlen;
    SymbolId id = interp.symbols.intern(buf[:len]);
    if (id == INVALID_SYMBOL_ID) return make_error(interp, "symbol table exhausted");
    return make_symbol(interp, id);
}

fn Value* prim_is_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    if (args[0] != null && args[0].tag == HASHMAP) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

// =============================================================================
// HASH MAP OPERATIONS
// =============================================================================

fn uint fnv1a(char[] data) {
    uint h = 2166136261;
    for (usz i = 0; i < data.len; i++) { h ^= (uint)data[i]; h *= 16777619; }
    return h;
}

fn uint murmur_finalizer(uint h) {
    h ^= h >> 16; h *= 0x85ebca6b; h ^= h >> 13; h *= 0xc2b2ae35; h ^= h >> 16;
    return h;
}

fn uint hash_value(Value* key) {
    if (key == null) return 0;
    switch (key.tag) {
        case INT: return murmur_finalizer((uint)key.int_val);
        case STRING: return fnv1a(key.str_val.chars[:key.str_val.len]);
        case SYMBOL: return murmur_finalizer((uint)key.sym_val);
        default: return 0;
    }
}

fn HashMap* hashmap_new(uint capacity, Interp* interp) {
    // Allocate HashMap struct in root_region
    HashMap hm;
    hm.capacity = capacity;
    hm.count = 0;
    hm.mask = capacity - 1;
    main::ObjectHandle hm_handle = main::allocate_in(interp.root_region, HashMap, hm);
    HashMap* map = main::dereference_as(HashMap, hm_handle);

    // Allocate entries array via malloc (contiguous needed for indexing)
    map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * capacity);
    for (uint i = 0; i < capacity; i++) {
        map.entries[i].key = null;
        map.entries[i].value = null;
    }

    return map;
}

fn Value* hashmap_get(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return null;  // empty slot
        if (values_equal(entry.key, key)) return entry.value;
    }
    return null;
}

fn void hashmap_grow(HashMap* map, Interp* interp) {
    uint old_cap = map.capacity;
    HashEntry* old_entries = map.entries;
    uint new_cap = old_cap * 2;
    uint new_mask = new_cap - 1;

    // Allocate new entries array via malloc
    HashEntry* new_entries = (HashEntry*)mem::malloc(HashEntry.sizeof * new_cap);
    for (uint i = 0; i < new_cap; i++) {
        new_entries[i].key = null;
        new_entries[i].value = null;
    }

    map.entries = new_entries;
    map.capacity = new_cap;
    map.mask = new_mask;
    map.count = 0;

    // Rehash
    for (uint i = 0; i < old_cap; i++) {
        if (old_entries[i].key != null) {
            uint h = hash_value(old_entries[i].key) & new_mask;
            for (uint j = 0; j < new_cap; j++) {
                uint idx = (h + j) & new_mask;
                if (new_entries[idx].key == null) {
                    new_entries[idx].key = old_entries[i].key;
                    new_entries[idx].value = old_entries[i].value;
                    map.count++;
                    break;
                }
            }
        }
    }
    mem::free(old_entries);
}

fn void hashmap_set(HashMap* map, Value* key, Value* value, Interp* interp) {
    // Grow at 70% load
    if (map.count * 10 >= map.capacity * 7) {
        hashmap_grow(map, interp);
    }

    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) {
            entry.key = key;
            entry.value = value;
            map.count++;
            return;
        }
        if (values_equal(entry.key, key)) {
            entry.value = value;
            return;
        }
    }
}

fn bool hashmap_remove(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return false;
        if (values_equal(entry.key, key)) {
            // Backward shift delete
            uint j = idx;
            for (;;) {
                uint next = (j + 1) & map.mask;
                if (map.entries[next].key == null) break;
                uint natural = hash_value(map.entries[next].key) & map.mask;
                // Check if next entry would be displaced by removing j
                // If natural position is between (j+1) and next, it's fine where it is
                bool should_move;
                if (j < next) {
                    should_move = natural <= j || natural > next;
                } else {
                    should_move = natural <= j && natural > next;
                }
                if (!should_move) break;
                map.entries[j] = map.entries[next];
                j = next;
            }
            map.entries[j].key = null;
            map.entries[j].value = null;
            map.count--;
            return true;
        }
    }
    return false;
}

fn Value* make_hashmap(Interp* interp, uint capacity) {
    // Allocate value in root_region so it persists
    main::RegionHandle saved = interp.current_frame;
    interp.current_frame = interp.root_region;
    Value* v = interp.alloc_value();
    v.tag = HASHMAP;
    v.hashmap_val = hashmap_new(capacity, interp);
    interp.current_frame = saved;
    return v;
}

// Dict primitives
fn Value* prim_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len % 2 != 0) return make_error(interp, "dict: expected even number of args");
    uint cap = 16;
    while (cap < (uint)args.len * 2) cap *= 2;
    Value* v = make_hashmap(interp, cap);
    for (usz i = 0; i < args.len; i += 2) {
        hashmap_set(v.hashmap_val, args[i], args[i + 1], interp);
    }
    return v;
}

fn Value* prim_dict_ref(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "dict-ref: expected 2 args");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "dict-ref: expected dict");
    Value* result = hashmap_get(args[0].hashmap_val, args[1]);
    if (result == null) return make_nil(interp);
    return result;
}

fn Value* prim_dict_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return make_error(interp, "dict-set!: expected 3 args");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "dict-set!: expected dict");
    hashmap_set(args[0].hashmap_val, args[1], args[2], interp);
    return args[0];
}

fn Value* prim_dict_has(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "dict-has?: expected 2 args");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "dict-has?: expected dict");
    Value* result = hashmap_get(args[0].hashmap_val, args[1]);
    if (result != null) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

fn Value* prim_dict_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "dict-remove!: expected 2 args");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "dict-remove!: expected dict");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return args[0];
}

fn Value* prim_dict_keys(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "keys: expected dict");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].key, result);
        }
    }
    return result;
}

fn Value* prim_dict_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "values: expected dict");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].value, result);
        }
    }
    return result;
}

fn Value* prim_dict_count(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "dict-count: expected dict");
    return make_int(interp, (long)args[0].hashmap_val.count);
}

// =============================================================================
// SECTION 7.8: FFI PRIMITIVES
// =============================================================================

fn bool ffi_is_double_type(char[] type_name) @inline {
    return type_name.len == 6 && type_name[0] == 'd' && type_name[1] == 'o' && type_name[2] == 'u'
        && type_name[3] == 'b' && type_name[4] == 'l' && type_name[5] == 'e';
}

fn double ffi_value_to_double(Value* val) @inline {
    if (val != null && is_double(val)) return val.double_val;
    if (val != null && val.tag == INT) return (double)val.int_val;
    return 0.0;
}

/**
 * Convert a Pika value to a long based on FFI type symbol.
 * Returns the value as a long suitable for passing through function pointer casts.
 */
fn long ffi_value_to_long(Value* val, char[] type_name, Interp* interp) {
    if (type_name.len == 3 && type_name[0] == 'i' && type_name[1] == 'n' && type_name[2] == 't') {
        // 'int
        if (val != null && val.tag == INT) return val.int_val;
        return 0;
    }
    if (type_name.len == 4 && type_name[0] == 's' && type_name[1] == 'i' && type_name[2] == 'z' && type_name[3] == 'e') {
        // 'size
        if (val != null && val.tag == INT) return val.int_val;
        return 0;
    }
    if (type_name.len == 6 && type_name[0] == 's' && type_name[1] == 't' && type_name[2] == 'r') {
        // 'string
        if (val != null && val.tag == STRING) return (long)&val.str_val.chars;
        return 0;
    }
    if (type_name.len == 3 && type_name[0] == 'p' && type_name[1] == 't' && type_name[2] == 'r') {
        // 'ptr
        if (val != null && val.tag == FFI_HANDLE) return (long)val.ffi_val.lib_handle;
        if (val != null && val.tag == INT) return val.int_val;
        return 0;
    }
    if (ffi_is_double_type(type_name)) {
        // 'double — use ffi_value_to_double() and proper double calling convention.
        // This path returns 0; actual double args are handled in prim_ffi_call dispatch.
        return 0;
    }
    return 0;
}

/**
 * Convert a C long result back to a Pika value based on FFI return type symbol.
 */
fn Value* ffi_long_to_value(long result, char[] type_name, Interp* interp) {
    if (type_name.len == 3 && type_name[0] == 'i' && type_name[1] == 'n' && type_name[2] == 't') {
        return make_int(interp, result);
    }
    if (type_name.len == 4 && type_name[0] == 's' && type_name[1] == 'i' && type_name[2] == 'z' && type_name[3] == 'e') {
        return make_int(interp, result);
    }
    if (type_name.len == 6 && type_name[0] == 's' && type_name[1] == 't' && type_name[2] == 'r') {
        // 'string — result is a char*
        char* ptr = (char*)(uptr)result;
        if (ptr == null) return make_nil(interp);
        // Measure length
        usz len = 0;
        while (ptr[len] != 0 && len < MAX_STRING_LEN - 1) len++;
        return make_string(interp, ptr[:len]);
    }
    if (type_name.len == 4 && type_name[0] == 'v' && type_name[1] == 'o' && type_name[2] == 'i' && type_name[3] == 'd') {
        return make_nil(interp);
    }
    if (type_name.len == 3 && type_name[0] == 'p' && type_name[1] == 't' && type_name[2] == 'r') {
        return make_int(interp, result);
    }
    if (ffi_is_double_type(type_name)) {
        // 'double returns are handled directly in prim_ffi_call dispatch
        // (proper XMM register convention). Should not reach here.
        return make_double(interp, 0.0);
    }
    return make_nil(interp);
}

/**
 * (ffi-open "libc.so.6") => V_FFI_HANDLE
 */
fn Value* prim_ffi_open(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != STRING) {
        return make_error(interp, "ffi-open: expected string argument");
    }
    char[] name = args[0].str_val.chars[:args[0].str_val.len];
    void* handle = dlopen((ZString)&args[0].str_val.chars, RTLD_LAZY);
    if (handle == null) {
        char* err = dlerror();
        if (err != null) {
            // Build error message
            usz elen = 0;
            while (err[elen] != 0 && elen < 200) elen++;
            return make_error(interp, err[:elen]);
        }
        return make_error(interp, "ffi-open: dlopen failed");
    }
    return make_ffi_handle(interp, handle, name);
}

/**
 * (ffi-call handle "func_name" 'return-type arg1 'type1 arg2 'type2 ...)
 * Variadic: minimum 3 args (handle, name, return type).
 * After return type, args come in pairs: (value, type_symbol).
 */
fn Value* prim_ffi_call(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) {
        return make_error(interp, "ffi-call: expected at least 3 args (handle, name, return-type)");
    }
    // Arg 0: FFI handle
    if (args[0] == null || args[0].tag != FFI_HANDLE) {
        return make_error(interp, "ffi-call: first arg must be an ffi-handle");
    }
    if (args[0].ffi_val.lib_handle == null) {
        return make_error(interp, "ffi-call: handle is closed");
    }
    // Arg 1: function name (string)
    if (args[1] == null || args[1].tag != STRING) {
        return make_error(interp, "ffi-call: second arg must be a string (function name)");
    }
    // Arg 2: return type (symbol)
    if (args[2] == null || args[2].tag != SYMBOL) {
        return make_error(interp, "ffi-call: third arg must be a symbol (return type)");
    }

    // Look up the function (with dlsym cache)
    char[] fn_name = args[1].str_val.chars[:args[1].str_val.len];
    void* fn_ptr = null;
    // Check cache first
    for (usz ci = 0; ci < args[0].ffi_val.sym_cache_count; ci++) {
        bool match = fn_name.len < 64;
        if (match) {
            for (usz cj = 0; cj < fn_name.len; cj++) {
                if (args[0].ffi_val.sym_cache_names[ci][cj] != fn_name[cj]) { match = false; break; }
            }
            if (match && args[0].ffi_val.sym_cache_names[ci][fn_name.len] != 0) match = false;
        }
        if (match) { fn_ptr = args[0].ffi_val.sym_cache_ptrs[ci]; break; }
    }
    if (fn_ptr == null) {
        fn_ptr = dlsym(args[0].ffi_val.lib_handle, (ZString)&args[1].str_val.chars);
        if (fn_ptr == null) {
            char* err = dlerror();
            if (err != null) {
                usz elen = 0;
                while (err[elen] != 0 && elen < 200) elen++;
                return make_error(interp, err[:elen]);
            }
            return make_error(interp, "ffi-call: dlsym failed");
        }
        // Populate cache
        if (args[0].ffi_val.sym_cache_count < 32 && fn_name.len < 64) {
            usz ci = args[0].ffi_val.sym_cache_count;
            args[0].ffi_val.sym_cache_ptrs[ci] = fn_ptr;
            for (usz cj = 0; cj < fn_name.len; cj++) {
                args[0].ffi_val.sym_cache_names[ci][cj] = fn_name[cj];
            }
            args[0].ffi_val.sym_cache_names[ci][fn_name.len] = 0;
            args[0].ffi_val.sym_cache_count++;
        }
    }

    // Parse arg pairs: (value, type_symbol) after the first 3 args
    usz remaining = args.len - 3;
    if (remaining % 2 != 0) {
        return make_error(interp, "ffi-call: args after return-type must be (value type) pairs");
    }
    usz c_argc = remaining / 2;
    if (c_argc > 6) {
        return make_error(interp, "ffi-call: max 6 C arguments supported");
    }

    // Classify arg types (double vs integer)
    char[][6] arg_types;
    long[6] c_args;
    double[6] d_args;
    usz double_count = 0;
    for (usz i = 0; i < c_argc; i++) {
        Value* val = args[3 + i * 2];
        Value* type_sym = args[3 + i * 2 + 1];
        if (type_sym == null || type_sym.tag != SYMBOL) {
            return make_error(interp, "ffi-call: type annotation must be a symbol");
        }
        arg_types[i] = interp.symbols.get_name(type_sym.sym_val);
        if (ffi_is_double_type(arg_types[i])) {
            d_args[i] = ffi_value_to_double(val);
            double_count++;
        } else {
            c_args[i] = ffi_value_to_long(val, arg_types[i], interp);
        }
    }

    // Get return type
    char[] ret_type = interp.symbols.get_name(args[2].sym_val);
    bool ret_is_double = ffi_is_double_type(ret_type);

    // Dispatch based on arg types and return type.
    // All-double args with double return (most common: sqrt, sin, pow, etc.)
    if (double_count > 0 && double_count == c_argc && ret_is_double) {
        double dresult;
        switch (c_argc) {
            case 0: dresult = ((FfiFnD0)(uptr)fn_ptr)();
            case 1: dresult = ((FfiFnD1)(uptr)fn_ptr)(d_args[0]);
            case 2: dresult = ((FfiFnD2)(uptr)fn_ptr)(d_args[0], d_args[1]);
            case 3: dresult = ((FfiFnD3)(uptr)fn_ptr)(d_args[0], d_args[1], d_args[2]);
            default: return make_error(interp, "ffi-call: max 3 double args supported");
        }
        return make_double(interp, dresult);
    }

    // All-double args with integer return (e.g., lround)
    if (double_count > 0 && double_count == c_argc && !ret_is_double) {
        long result;
        switch (c_argc) {
            case 1: result = ((FfiFnDI1)(uptr)fn_ptr)(d_args[0]);
            case 2: result = ((FfiFnDI2)(uptr)fn_ptr)(d_args[0], d_args[1]);
            default: return make_error(interp, "ffi-call: unsupported double-arg int-return combo");
        }
        return ffi_long_to_value(result, ret_type, interp);
    }

    // All-integer args with double return (e.g., converting int to double)
    if (double_count == 0 && ret_is_double) {
        double dresult;
        switch (c_argc) {
            case 0: dresult = ((FfiFnID0)(uptr)fn_ptr)();
            case 1: dresult = ((FfiFnID1)(uptr)fn_ptr)(c_args[0]);
            case 2: dresult = ((FfiFnID2)(uptr)fn_ptr)(c_args[0], c_args[1]);
            default: return make_error(interp, "ffi-call: unsupported int-arg double-return combo");
        }
        return make_double(interp, dresult);
    }

    // All-integer args with integer return (original behavior)
    if (double_count == 0) {
        long result;
        switch (c_argc) {
            case 0: result = ((FfiFn0)(uptr)fn_ptr)();
            case 1: result = ((FfiFn1)(uptr)fn_ptr)(c_args[0]);
            case 2: result = ((FfiFn2)(uptr)fn_ptr)(c_args[0], c_args[1]);
            case 3: result = ((FfiFn3)(uptr)fn_ptr)(c_args[0], c_args[1], c_args[2]);
            case 4: result = ((FfiFn4)(uptr)fn_ptr)(c_args[0], c_args[1], c_args[2], c_args[3]);
            case 5: result = ((FfiFn5)(uptr)fn_ptr)(c_args[0], c_args[1], c_args[2], c_args[3], c_args[4]);
            case 6: result = ((FfiFn6)(uptr)fn_ptr)(c_args[0], c_args[1], c_args[2], c_args[3], c_args[4], c_args[5]);
            default: return make_error(interp, "ffi-call: unreachable arg count");
        }
        return ffi_long_to_value(result, ret_type, interp);
    }

    return make_error(interp, "ffi-call: mixed int/double args not yet supported");
}

/**
 * (ffi-close handle) => nil
 */
fn Value* prim_ffi_close(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != FFI_HANDLE) {
        return make_error(interp, "ffi-close: expected ffi-handle argument");
    }
    if (args[0].ffi_val.lib_handle != null) {
        dlclose(args[0].ffi_val.lib_handle);
        args[0].ffi_val.lib_handle = null;
        args[0].ffi_val.sym_cache_count = 0;
    }
    return make_nil(interp);
}

/**
 * (ffi-sym handle "func_name") => int (pointer as integer)
 */
fn Value* prim_ffi_sym(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "ffi-sym: expected 2 args");
    if (args[0] == null || args[0].tag != FFI_HANDLE) {
        return make_error(interp, "ffi-sym: first arg must be ffi-handle");
    }
    if (args[0].ffi_val.lib_handle == null) {
        return make_error(interp, "ffi-sym: handle is closed");
    }
    if (args[1] == null || args[1].tag != STRING) {
        return make_error(interp, "ffi-sym: second arg must be a string");
    }
    void* sym = dlsym(args[0].ffi_val.lib_handle, (ZString)&args[1].str_val.chars);
    if (sym == null) return make_int(interp, 0);
    return make_int(interp, (long)(uptr)sym);
}

// =============================================================================
// PHASE 1-7 NEW PRIMITIVES
// =============================================================================

// --- Phase 1: Float predicates & conversions ---

fn Value* prim_is_double(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    return is_double(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_number(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    return is_number(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_exact_to_inexact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "exact->inexact: expected number");
    return make_double(interp, to_double(args[0]));
}

fn Value* prim_inexact_to_exact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "inexact->exact: expected number");
    return make_int(interp, (long)to_double(args[0]));
}

// --- Phase 2: Math library ---

import std::math;

// C math functions (linked via -lm)
extern fn double c_sin(double x) @extern("sin");
extern fn double c_cos(double x) @extern("cos");
extern fn double c_tan(double x) @extern("tan");
extern fn double c_asin(double x) @extern("asin");
extern fn double c_acos(double x) @extern("acos");
extern fn double c_atan(double x) @extern("atan");
extern fn double c_atan2(double y, double x) @extern("atan2");
extern fn double c_exp(double x) @extern("exp");
extern fn double c_log(double x) @extern("log");
extern fn double c_log10(double x) @extern("log10");
extern fn double c_pow(double x, double y) @extern("pow");
extern fn double c_sqrt(double x) @extern("sqrt");
extern fn double c_floor(double x) @extern("floor");
extern fn double c_ceil(double x) @extern("ceil");
extern fn double c_round(double x) @extern("round");
extern fn double c_fabs(double x) @extern("fabs");

fn Value* prim_sin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "sin: expected number");
    return make_double(interp, c_sin(to_double(args[0])));
}

fn Value* prim_cos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "cos: expected number");
    return make_double(interp, c_cos(to_double(args[0])));
}

fn Value* prim_tan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "tan: expected number");
    return make_double(interp, c_tan(to_double(args[0])));
}

fn Value* prim_asin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "asin: expected number");
    return make_double(interp, c_asin(to_double(args[0])));
}

fn Value* prim_acos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "acos: expected number");
    return make_double(interp, c_acos(to_double(args[0])));
}

fn Value* prim_atan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "atan: expected number");
    return make_double(interp, c_atan(to_double(args[0])));
}

fn Value* prim_atan2(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return make_error(interp, "atan2: expected 2 numbers");
    return make_double(interp, c_atan2(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_exp(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "exp: expected number");
    return make_double(interp, c_exp(to_double(args[0])));
}

fn Value* prim_log(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "log: expected number");
    return make_double(interp, c_log(to_double(args[0])));
}

fn Value* prim_log10(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "log10: expected number");
    return make_double(interp, c_log10(to_double(args[0])));
}

fn Value* prim_pow(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return make_error(interp, "pow: expected 2 numbers");
    return make_double(interp, c_pow(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_sqrt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "sqrt: expected number");
    return make_double(interp, c_sqrt(to_double(args[0])));
}

fn Value* prim_floor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "floor: expected number");
    return make_int(interp, (long)c_floor(to_double(args[0])));
}

fn Value* prim_ceiling(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "ceiling: expected number");
    return make_int(interp, (long)c_ceil(to_double(args[0])));
}

fn Value* prim_round(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "round: expected number");
    return make_int(interp, (long)c_round(to_double(args[0])));
}

fn Value* prim_truncate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "truncate: expected number");
    return make_int(interp, (long)to_double(args[0]));
}

fn Value* prim_abs(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "abs: expected number");
    if (is_double(args[0])) return make_double(interp, c_fabs(args[0].double_val));
    long n = args[0].int_val;
    return make_int(interp, n < 0 ? -n : n);
}

fn Value* prim_min(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return make_error(interp, "min: expected 2 numbers");
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a < b ? a : b);
    }
    return make_int(interp, args[0].int_val < args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_max(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return make_error(interp, "max: expected 2 numbers");
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a > b ? a : b);
    }
    return make_int(interp, args[0].int_val > args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_gcd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "gcd: expected 2 integers");
    long a = args[0].int_val; long b = args[1].int_val;
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    while (b != 0) { long t = b; b = a % b; a = t; }
    return make_int(interp, a);
}

fn Value* prim_lcm(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "lcm: expected 2 integers");
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == 0 || b == 0) return make_int(interp, 0);
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    // gcd
    long ga = a; long gb = b;
    while (gb != 0) { long t = gb; gb = ga % gb; ga = t; }
    return make_int(interp, (a / ga) * b);
}

// --- Phase 3: Sorting & Bitwise ---

fn Value* prim_sort(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "sort: expected list argument");
    if (is_nil(args[0])) return make_nil(interp);
    // Convert list to array
    Value*[256] arr;
    usz len = 0;
    Value* cur = args[0];
    while (is_cons(cur) && len < 256) {
        arr[len++] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    // Insertion sort (stable)
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        // Compare: numbers by value, strings lexicographic
        while (j >= 0) {
            bool gt = false;
            if (is_number(arr[(usz)j]) && is_number(key)) {
                gt = to_double(arr[(usz)j]) > to_double(key);
            } else if (arr[(usz)j].tag == STRING && key.tag == STRING) {
                // Lexicographic string comparison
                usz al = arr[(usz)j].str_val.len; usz bl = key.str_val.len;
                usz ml = al < bl ? al : bl;
                int cmp = 0;
                for (usz k = 0; k < ml; k++) {
                    if (arr[(usz)j].str_val.chars[k] != key.str_val.chars[k]) {
                        cmp = arr[(usz)j].str_val.chars[k] > key.str_val.chars[k] ? 1 : -1;
                        break;
                    }
                }
                if (cmp == 0) { cmp = al > bl ? 1 : (al < bl ? -1 : 0); }
                gt = cmp > 0;
            }
            if (!gt) break;
            arr[(usz)(j + 1)] = arr[(usz)j];
            j--;
        }
        arr[(usz)(j + 1)] = key;
    }
    // Rebuild list
    Value* result = make_nil(interp);
    for (isz i = (isz)len - 1; i >= 0; i--) {
        result = make_cons(interp, arr[(usz)i], result);
    }
    return result;
}

fn Value* prim_sort_by(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "sort-by: expected comparator and list");
    Value* cmp_fn = args[0];
    if (is_nil(args[1])) return make_nil(interp);
    // Convert list to array
    Value*[256] arr;
    usz len = 0;
    Value* cur = args[1];
    while (is_cons(cur) && len < 256) {
        arr[len++] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    // Insertion sort with custom comparator
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        while (j >= 0) {
            // Call comparator: (cmp a b) should return negative/0/positive or truthy for a>b
            Value* r1 = jit_apply_value(cmp_fn, arr[(usz)j], interp);
            if (r1 == null || r1.tag == ERROR) break;
            Value* r2 = jit_apply_value(r1, key, interp);
            if (r2 == null || r2.tag == ERROR) break;
            // If result > 0 (or truthy for >), swap
            bool gt = false;
            if (is_int(r2)) { gt = r2.int_val > 0; }
            else if (is_double(r2)) { gt = r2.double_val > 0.0; }
            else { gt = !is_nil(r2); }
            if (!gt) break;
            arr[(usz)(j + 1)] = arr[(usz)j];
            j--;
        }
        arr[(usz)(j + 1)] = key;
    }
    // Rebuild list
    Value* result = make_nil(interp);
    for (isz i = (isz)len - 1; i >= 0; i--) {
        result = make_cons(interp, arr[(usz)i], result);
    }
    return result;
}

fn Value* prim_bitwise_and(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "bitwise-and: expected 2 integers");
    return make_int(interp, args[0].int_val & args[1].int_val);
}

fn Value* prim_bitwise_or(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "bitwise-or: expected 2 integers");
    return make_int(interp, args[0].int_val | args[1].int_val);
}

fn Value* prim_bitwise_xor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "bitwise-xor: expected 2 integers");
    return make_int(interp, args[0].int_val ^ args[1].int_val);
}

fn Value* prim_bitwise_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return make_error(interp, "bitwise-not: expected integer");
    return make_int(interp, ~args[0].int_val);
}

fn Value* prim_lshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "lshift: expected 2 integers");
    return make_int(interp, args[0].int_val << args[1].int_val);
}

fn Value* prim_rshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return make_error(interp, "rshift: expected 2 integers");
    return make_int(interp, args[0].int_val >> args[1].int_val);
}

// --- Phase 5: String operations & type predicates ---

fn Value* prim_string_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return make_error(interp, "string-contains?: expected 2 strings");
    char[] haystack = args[0].str_val.chars[:args[0].str_val.len];
    char[] needle = args[1].str_val.chars[:args[1].str_val.len];
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
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return make_error(interp, "string-index-of: expected 2 strings");
    char[] haystack = args[0].str_val.chars[:args[0].str_val.len];
    char[] needle = args[1].str_val.chars[:args[1].str_val.len];
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
        return make_error(interp, "string-replace: expected 3 strings");
    }
    char[] str = args[0].str_val.chars[:args[0].str_val.len];
    char[] old = args[1].str_val.chars[:args[1].str_val.len];
    char[] new_str = args[2].str_val.chars[:args[2].str_val.len];
    if (old.len == 0) return args[0]; // nothing to replace
    Value* result = interp.alloc_value();
    result.tag = STRING;
    result.str_val = (StringVal*)mem::malloc(StringVal.sizeof);
    result.str_val.len = 0;
    usz i = 0;
    while (i < str.len) {
        if (i + old.len <= str.len) {
            bool match = true;
            for (usz j = 0; j < old.len; j++) {
                if (str[i + j] != old[j]) { match = false; break; }
            }
            if (match) {
                for (usz j = 0; j < new_str.len && result.str_val.len < MAX_STRING_LEN - 1; j++) {
                    result.str_val.chars[result.str_val.len++] = new_str[j];
                }
                i += old.len;
                continue;
            }
        }
        if (result.str_val.len < MAX_STRING_LEN - 1) {
            result.str_val.chars[result.str_val.len++] = str[i];
        }
        i++;
    }
    result.str_val.chars[result.str_val.len] = 0;
    return result;
}

fn Value* prim_char_at(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return make_error(interp, "char-at: expected string and int");
    long idx = args[1].int_val;
    if (idx < 0 || (usz)idx >= args[0].str_val.len) return make_error(interp, "char-at: index out of range");
    Value* result = interp.alloc_value();
    result.tag = STRING;
    result.str_val = (StringVal*)mem::malloc(StringVal.sizeof);
    result.str_val.chars[0] = args[0].str_val.chars[(usz)idx];
    result.str_val.chars[1] = 0;
    result.str_val.len = 1;
    return result;
}

fn Value* prim_string_repeat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return make_error(interp, "string-repeat: expected string and int");
    long n = args[1].int_val;
    if (n <= 0) return make_string(interp, "");
    Value* result = interp.alloc_value();
    result.tag = STRING;
    result.str_val = (StringVal*)mem::malloc(StringVal.sizeof);
    result.str_val.len = 0;
    for (long i = 0; i < n; i++) {
        for (usz j = 0; j < args[0].str_val.len && result.str_val.len < MAX_STRING_LEN - 1; j++) {
            result.str_val.chars[result.str_val.len++] = args[0].str_val.chars[j];
        }
    }
    result.str_val.chars[result.str_val.len] = 0;
    return result;
}

fn Value* prim_is_boolean(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    if (is_nil(args[0])) return make_symbol(interp, interp.sym_true); // false is nil
    if (is_symbol(args[0]) && (uint)args[0].sym_val == (uint)interp.sym_true) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

fn Value* prim_is_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    Value* v = args[0];
    if (is_nil(v)) return make_symbol(interp, interp.sym_true); // empty list is a list
    // Check if proper list (nil-terminated cons chain)
    while (is_cons(v)) { v = v.cons_val.cdr; }
    return is_nil(v) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_procedure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    ValueTag t = args[0].tag;
    return (t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_zero(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "zero?: expected number");
    return to_double(args[0]) == 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_positive(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "positive?: expected number");
    return to_double(args[0]) > 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_negative(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return make_error(interp, "negative?: expected number");
    return to_double(args[0]) < 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_even(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return make_error(interp, "even?: expected integer");
    return (args[0].int_val % 2 == 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_odd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return make_error(interp, "odd?: expected integer");
    return (args[0].int_val % 2 != 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return make_error(interp, "format: expected format string");
    char[] fmt = args[0].str_val.chars[:args[0].str_val.len];
    Value* result = interp.alloc_value();
    result.tag = STRING;
    result.str_val = (StringVal*)mem::malloc(StringVal.sizeof);
    result.str_val.len = 0;
    usz arg_idx = 1;
    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '~' && i + 1 < fmt.len) {
            char directive = fmt[i + 1];
            i++;
            if (directive == 'a' || directive == 's') {
                if (arg_idx < args.len) {
                    Value* a = args[arg_idx++];
                    if (a == null || is_nil(a)) {
                        char[] s = "nil";
                        for (usz k = 0; k < 3 && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = s[k];
                        }
                    } else if (is_int(a)) {
                        char[32] nbuf;
                        char[] nslice = io::bprintf(&nbuf, "%d", a.int_val)!!;
                        for (usz k = 0; k < nslice.len && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = nslice[k];
                        }
                    } else if (is_double(a)) {
                        char[64] nbuf;
                        char[] nslice = io::bprintf(&nbuf, "%.15g", a.double_val)!!;
                        for (usz k = 0; k < nslice.len && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = nslice[k];
                        }
                    } else if (a.tag == STRING) {
                        if (directive == 's') {
                            if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = '"'; }
                        }
                        for (usz k = 0; k < a.str_val.len && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = a.str_val.chars[k];
                        }
                        if (directive == 's') {
                            if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = '"'; }
                        }
                    } else if (is_symbol(a)) {
                        char[] sname = interp.symbols.get_name(a.sym_val);
                        for (usz k = 0; k < sname.len && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = sname[k];
                        }
                    } else {
                        char[] s = "#<obj>";
                        for (usz k = 0; k < s.len && result.str_val.len < MAX_STRING_LEN - 1; k++) {
                            result.str_val.chars[result.str_val.len++] = s[k];
                        }
                    }
                }
            } else if (directive == '~') {
                if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = '~'; }
            } else {
                if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = '~'; }
                if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = directive; }
            }
        } else {
            if (result.str_val.len < MAX_STRING_LEN - 1) { result.str_val.chars[result.str_val.len++] = fmt[i]; }
        }
    }
    result.str_val.chars[result.str_val.len] = 0;
    return result;
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "display: expected argument");
    Value* v = args[0];
    if (v == null || is_nil(v)) {
        io::print("nil");
    } else if (v.tag == STRING) {
        io::printf("%s", (ZString)&v.str_val.chars); // No quotes for display
    } else {
        print_value(v, &interp.symbols);
    }
    return make_nil(interp);
}

// --- Phase 7: Macroexpand, eval, apply, bound? ---

fn Value* prim_macroexpand(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "macroexpand: expected quoted expression");
    Value* form = args[0];
    // Form should be a list (macro-name arg...)
    if (!is_cons(form)) return form; // not a macro call, return as-is
    Value* head = form.cons_val.car;
    if (head == null || !is_symbol(head)) return form;
    SymbolId sym = head.sym_val;
    // Check macro table using hash index
    usz mh = (usz)((uint)sym * 2654435761);
    usz mmask = MACRO_HASH_SIZE - 1;
    for (usz probe = 0; probe < MACRO_HASH_SIZE; probe++) {
        usz slot = (mh + probe) & mmask;
        usz midx = interp.macro_hash_index[slot];
        if (midx == usz.max) break; // empty slot
        if ((uint)interp.macro_table[midx].name == (uint)sym) {
            Expr* call_expr = value_to_expr(form, interp);
            if (call_expr == null) return form;
            EvalResult r = expand_pattern_macro(&interp.macro_table[midx], call_expr, interp);
            if (!r.error.has_error && r.value != null) return r.value;
            return form;
        }
    }
    return form;
}

fn Value* prim_eval(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "eval: expected expression");
    Expr* expr = value_to_expr(args[0], interp);
    if (expr == null) return make_error(interp, "eval: could not convert to expression");
    Value* result = jit_eval(expr, interp.global_env, interp);
    if (result != null && result.tag == ERROR) return make_error(interp, "eval: error during evaluation");
    return result;
}

fn Value* prim_apply(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "apply: expected function and argument list");
    Value* func = args[0];
    Value* arg_list = args[1];
    // Apply one arg at a time via JIT path
    Value* result = func;
    Value* cur = arg_list;
    while (is_cons(cur)) {
        result = jit_apply_value(result, cur.cons_val.car, interp);
        if (result != null && result.tag == ERROR) return make_error(interp, "apply: error during application");
        cur = cur.cons_val.cdr;
    }
    return result;
}

fn Value* prim_bound(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return make_error(interp, "bound?: expected symbol");
    SymbolId sym = args[0].sym_val;
    Env* lookup_env = env != null ? env : interp.global_env;
    Value* v = lookup_env.lookup(sym);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_error(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return make_error(interp, "error: expected string message");
    return make_error(interp, args[0].str_val.chars[:args[0].str_val.len]);
}

fn Value* prim_error_message(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != ERROR) return make_error(interp, "error-message: expected error value");
    return make_string(interp, args[0].str_val.chars[:args[0].str_val.len]);
}

// --- Phase 8: Array primitives ---

fn Value* prim_array(Value*[] args, Env* env, Interp* interp) {
    // Single list/nil arg → convert to array
    if (args.len == 1 && (is_cons(args[0]) || is_nil(args[0]))) {
        usz count = 0;
        Value* cur = args[0];
        while (is_cons(cur) && count < 65536) { count++; cur = cur.cons_val.cdr; }
        Value* v = make_array(interp, count);
        cur = args[0];
        for (usz i = 0; i < count; i++) {
            v.array_val.items[i] = cur.cons_val.car;
            cur = cur.cons_val.cdr;
        }
        v.array_val.length = count;
        return v;
    }

    Value* v = make_array(interp, args.len < 4 ? 4 : args.len);
    for (usz i = 0; i < args.len; i++) {
        v.array_val.items[i] = args[i];
    }
    v.array_val.length = args.len;
    return v;
}

fn Value* prim_make_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0])) return make_error(interp, "make-array: expected size and fill");
    long sz = args[0].int_val;
    if (sz < 0 || sz > 65536) return make_error(interp, "make-array: invalid size");
    Value* v = make_array(interp, (usz)sz);
    for (usz i = 0; i < (usz)sz; i++) { v.array_val.items[i] = args[1]; }
    v.array_val.length = (usz)sz;
    return v;
}

fn Value* prim_array_ref(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_array(args[0]) || !is_int(args[1])) return make_error(interp, "array-ref: expected array and int");
    long idx = args[1].int_val;
    if (idx < 0 || (usz)idx >= args[0].array_val.length) return make_error(interp, "array-ref: index out of range");
    return args[0].array_val.items[(usz)idx];
}

fn Value* prim_array_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || !is_array(args[0]) || !is_int(args[1])) return make_error(interp, "array-set!: expected array, int, value");
    long idx = args[1].int_val;
    if (idx < 0 || (usz)idx >= args[0].array_val.length) return make_error(interp, "array-set!: index out of range");
    args[0].array_val.items[(usz)idx] = args[2];
    return make_nil(interp);
}

fn Value* prim_array_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return make_error(interp, "array-length: expected array");
    return make_int(interp, (long)args[0].array_val.length);
}

fn Value* prim_array_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return make_error(interp, "array->list: expected array");
    Array* vec = args[0].array_val;
    Value* result = make_nil(interp);
    for (isz i = (isz)vec.length - 1; i >= 0; i--) {
        result = make_cons(interp, vec.items[(usz)i], result);
    }
    return result;
}

fn Value* prim_list_to_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "list->array: expected list");
    // Count elements
    usz count = 0;
    Value* cur = args[0];
    while (is_cons(cur) && count < 65536) { count++; cur = cur.cons_val.cdr; }
    Value* v = make_array(interp, count);
    cur = args[0];
    for (usz i = 0; i < count; i++) {
        v.array_val.items[i] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    v.array_val.length = count;
    return v;
}

fn Value* prim_is_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    return is_array(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_array_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_array(args[0])) return make_error(interp, "push!: expected array and value");
    Array* vec = args[0].array_val;
    if (vec.length >= vec.capacity) {
        usz new_cap = vec.capacity * 2;
        Value** new_items = (Value**)mem::malloc(Value*.sizeof * new_cap);
        for (usz i = 0; i < vec.length; i++) { new_items[i] = vec.items[i]; }
        mem::free(vec.items);
        vec.items = new_items;
        vec.capacity = new_cap;
    }
    vec.items[vec.length++] = args[1];
    return make_nil(interp);
}

// --- Generic collection primitives ---

fn Value* prim_ref(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "ref: expected collection and key");
    Value* coll = args[0];
    if (coll == null) return make_error(interp, "ref: nil collection");

    // Array: (ref arr idx)
    if (coll.tag == ARRAY) {
        if (!is_int(args[1])) return make_error(interp, "ref: array requires int index");
        long idx = args[1].int_val;
        if (coll.array_val == null || idx < 0 || (usz)idx >= coll.array_val.length) { return make_error(interp, "ref: array index out of bounds"); }
        return coll.array_val.items[(usz)idx];
    }

    // Dict: (ref dict key)
    if (coll.tag == HASHMAP) {
        Value* result = hashmap_get(coll.hashmap_val, args[1]);
        if (result == null) return make_nil(interp);
        return result;
    }

    // Cons: (ref pair 0) = car, (ref pair 1) = cdr
    if (coll.tag == CONS) {
        if (!is_int(args[1])) return make_error(interp, "ref: cons requires int index (0=car, 1=cdr)");
        long idx = args[1].int_val;
        if (idx == 0) return coll.cons_val.car;
        if (idx == 1) return coll.cons_val.cdr;
        return make_error(interp, "ref: cons index must be 0 (car) or 1 (cdr)");
    }

    // String: (ref str idx) = char as int
    if (coll.tag == STRING) {
        if (!is_int(args[1])) return make_error(interp, "ref: string requires int index");
        long idx = args[1].int_val;
        if (idx < 0 || (usz)idx >= coll.str_val.len) { return make_error(interp, "ref: string index out of bounds"); }
        return make_int(interp, (long)coll.str_val.chars[(usz)idx]);
    }

    return make_error(interp, "ref: expected array, dict, cons, or string");
}

fn Value* prim_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "push!: expected collection and value");
    if (!is_array(args[0])) return make_error(interp, "push!: expected array");
    return prim_array_push(args, env, interp);
}

fn Value* prim_keys(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "keys: expected dict");
    return prim_dict_keys(args, env, interp);
}

fn Value* prim_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "values: expected dict");
    return prim_dict_values(args, env, interp);
}

fn Value* prim_has(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "has?: expected collection and key");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "has?: expected dict");
    return prim_dict_has(args, env, interp);
}

fn Value* prim_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "remove!: expected collection and key");
    if (args[0] == null || args[0].tag != HASHMAP) return make_error(interp, "remove!: expected dict");
    return prim_dict_remove(args, env, interp);
}

// --- Phase 9: Set primitives (built on dict) ---

fn Value* prim_set(Value*[] args, Env* env, Interp* interp) {
    // Create a new hash map, using args as keys, value = true
    Value* map_val = interp.alloc_value();
    map_val.tag = HASHMAP;
    uint cap = 16;
    HashMap* map = (HashMap*)mem::malloc(HashMap.sizeof);
    map.capacity = cap;
    map.count = 0;
    map.mask = cap - 1;
    map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * cap);
    for (uint i = 0; i < cap; i++) { map.entries[i].key = null; }
    map_val.hashmap_val = map;
    Value* true_val = make_symbol(interp, interp.sym_true);
    for (usz i = 0; i < args.len; i++) {
        hashmap_set(map, args[i], true_val, interp);
    }
    map_val.tag = HASHMAP; // Use HASHMAP tag - sets are hashmaps with true values
    return map_val;
}

fn Value* prim_set_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return make_error(interp, "set-add: expected set and value");
    Value* true_val = make_symbol(interp, interp.sym_true);
    hashmap_set(args[0].hashmap_val, args[1], true_val, interp);
    return args[0];
}

fn Value* prim_set_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return make_error(interp, "set-remove: expected set and value");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return args[0];
}

fn Value* prim_set_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return make_error(interp, "set-contains?: expected set and value");
    Value* v = hashmap_get(args[0].hashmap_val, args[1]);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_set_size(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != HASHMAP) return make_error(interp, "set-size: expected set");
    return make_int(interp, (long)args[0].hashmap_val.count);
}

fn Value* prim_set_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != HASHMAP) return make_error(interp, "set->list: expected set");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].key, result);
        }
    }
    return result;
}

// --- Phase 12: Additional I/O and convenience ---

fn Value* prim_read_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return make_error(interp, "read-string: expected string");
    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    EvalResult r = run(src, interp);
    if (r.error.has_error) return make_error(interp, "read-string: parse/eval error");
    return r.value;
}

fn Value* prim_string_to_symbol(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return make_error(interp, "string->symbol: expected string");
    SymbolId sym = interp.symbols.intern(args[0].str_val.chars[:args[0].str_val.len]);
    return make_symbol(interp, sym);
}

fn Value* prim_symbol_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return make_error(interp, "symbol->string: expected symbol");
    char[] name = interp.symbols.get_name(args[0].sym_val);
    return make_string(interp, name);
}

/// =============================================================================
// SECTION 7.5: TYPE SYSTEM PRIMITIVES
// =============================================================================

/**
 * (type-of v) -> symbol representing the type name
 */
fn Value* prim_type_of(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "type-of: expected 1 argument");
    SymbolId type_name = value_type_name(args[0], interp);
    return make_symbol(interp, type_name);
}

/**
 * (is? v 'TypeName) -> true if v's type is or is subtype of TypeName
 */
fn Value* prim_is_type(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_error(interp, "is?: expected 2 arguments");
    if (args[1] == null || args[1].tag != SYMBOL) {
        return make_error(interp, "is?: second argument must be a symbol");
    }

    SymbolId target_name = args[1].sym_val;
    TypeId target_type = interp.types.lookup(target_name, &interp.symbols);
    if (target_type == INVALID_TYPE_ID) {
        return make_nil(interp);  // Unknown type
    }

    TypeId value_type = infer_value_type(args[0], interp);
    if (value_type == INVALID_TYPE_ID) {
        return make_nil(interp);
    }

    // Check for Any type (everything matches)
    TypeId any_type = interp.types.lookup(interp.sym_Any, &interp.symbols);
    if (target_type == any_type) {
        return make_symbol(interp, interp.sym_true);
    }

    // Check exact match or subtype
    if (interp.types.is_subtype(value_type, target_type)) {
        return make_symbol(interp, interp.sym_true);
    }

    return make_nil(interp);
}

/**
 * (instance? v) -> true if v is a user-defined type instance
 */
fn Value* prim_is_instance(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    bool result = args[0] != null && args[0].tag == INSTANCE;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (type-args v) -> list of inferred type argument symbols, or nil if not parametric.
 * Example: (type-args (Box 42)) => '(Int)
 */
fn Value* prim_type_args(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "type-args: expected 1 argument");
    if (args[0] == null || args[0].tag != INSTANCE) return make_nil(interp);
    Instance* inst = args[0].instance_val;
    if (inst == null || inst.type_arg_count == 0) return make_nil(interp);

    // Build a list of type name symbols from the inferred type args
    Value* result = make_nil(interp);
    // Build in reverse so first type arg is at head
    for (usz i = inst.type_arg_count; i > 0; i--) {
        TypeId tid = inst.type_args[i - 1];
        if (tid != INVALID_TYPE_ID) {
            TypeInfo* ti = interp.types.get(tid);
            if (ti != null) {
                result = make_cons(interp, make_symbol(interp, ti.name), result);
            } else {
                result = make_cons(interp, make_nil(interp), result);
            }
        } else {
            result = make_cons(interp, make_nil(interp), result);
        }
    }
    return result;
}

// --- Memory reclamation ---

fn Value* prim_free_bang(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_error(interp, "free!: expected 1 argument");
    Value* v = args[0];
    if (v == null) return make_nil(interp);

    // Free heap-allocated backing storage and nullify the value.
    // Note: Array struct is malloc'd, but HashMap/Instance structs are region-allocated.
    // Only free what was actually malloc'd.
    switch (v.tag) {
        case ARRAY:
            if (v.array_val != null) {
                if (v.array_val.items != null) mem::free(v.array_val.items);
                mem::free(v.array_val);  // Array struct is malloc'd in make_array
                v.array_val = null;
            }
        case HASHMAP:
            // HashMap struct is region-allocated, only entries buffer is malloc'd
            if (v.hashmap_val != null) {
                if (v.hashmap_val.entries != null) {
                    mem::free(v.hashmap_val.entries);
                    v.hashmap_val.entries = null;
                }
                v.hashmap_val.count = 0;
                v.hashmap_val.capacity = 0;
            }
        case INSTANCE:
            // Instance struct is malloc'd in make_instance
            if (v.instance_val != null) {
                mem::free(v.instance_val);
                v.instance_val = null;
            }
        case STRING:
        case ERROR:
            if (v.str_val != null) {
                mem::free(v.str_val);
                v.str_val = null;
            }
        default: return make_nil(interp);  // No backing to free
    }
    v.tag = NIL;  // Mark as nil so aliases see a safe value
    return make_nil(interp);
}

