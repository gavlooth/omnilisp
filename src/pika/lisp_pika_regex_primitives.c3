module pika;

import std::core::mem;
import std::io;
import lisp;

fn lisp::Value* regex_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "regex", code, message, data);
}

fn lisp::Value* parser_raise(lisp::Interp* interp, char[] code, char[] message, lisp::Value* data = null) @inline {
    return lisp::raise_error_with_payload_names(interp, "parser", code, message, data);
}

fn lisp::Value* regex_raise_invalid_pattern_if_any(lisp::Interp* interp, char[] code, char[] message) {
    char[] detail;
    if (!regex_take_last_compile_error(&detail)) return null;
    lisp::Value* data = detail.len > 0 ? lisp::make_string(interp, detail) : null;
    return regex_raise(interp, code, message, data);
}

// (re-match pattern input) -> matched-string or nil
fn lisp::Value* prim_re_match(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-match-expected-pattern-input", "re-match: expected (re-match pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pattern-not-string", "re-match: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-input-not-string", "re-match: input must be a string");

    // C3 slice [a..b] is INCLUSIVE on both ends, [:n] means first n elements
    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_search(pattern, input);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-match-invalid-pattern",
            "re-match: invalid regex pattern")) {
        return compile_err;
    }

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-fullmatch pattern input) -> matched-string or nil
fn lisp::Value* prim_re_fullmatch(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-fullmatch-expected-pattern-input", "re-fullmatch: expected (re-fullmatch pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-fullmatch-pattern-not-string", "re-fullmatch: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-fullmatch-input-not-string", "re-fullmatch: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_fullmatch(pattern, input);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-fullmatch-invalid-pattern",
            "re-fullmatch: invalid regex pattern")) {
        return compile_err;
    }

    if (m.matched) {
        return lisp::make_string(interp, m.view);
    }

    return lisp::make_nil(interp);
}

// (re-find-all pattern input) -> list of matched strings
fn lisp::Value* prim_re_find_all(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-find-all-expected-pattern-input", "re-find-all: expected (re-find-all pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pattern-not-string", "re-find-all: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-input-not-string", "re-find-all: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-find-all-invalid-pattern",
            "re-find-all: invalid regex pattern")) {
        return compile_err;
    }

    // Build list of matched strings
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* s = lisp::make_string(interp, matches[i].view);
        result = lisp::make_cons(interp, s, result);
    }

    return result;
}

// (re-split pattern input) -> list of strings
fn lisp::Value* prim_re_split(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-split-expected-pattern-input", "re-split: expected (re-split pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-split-pattern-not-string", "re-split: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-split-input-not-string", "re-split: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    // Find all matches
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-split-invalid-pattern",
            "re-split: invalid regex pattern")) {
        return compile_err;
    }

    // Build result list without fixed-size part buffers.
    int right = (int)input.len;
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        int part_start = matches[i].end + 1;
        if (part_start < right) {
            char[] part = input[part_start..right - 1];
            result = lisp::make_cons(interp, lisp::make_string(interp, part), result);
        }
        right = matches[i].start;
    }
    if (right > 0) {
        char[] part = input[0..right - 1];
        result = lisp::make_cons(interp, lisp::make_string(interp, part), result);
    }

    return result;
}

// (re-replace pattern replacement input) -> new string
// (re-replace pattern replacement input 'global) -> replace all
fn bool re_replace_has_global_flag(lisp::Value*[] args) @inline {
    return args.len >= 4 && args[3].tag == lisp::ValueTag.SYMBOL;
}

struct ReReplaceBuffer {
    char* chars;
    usz len;
    usz capacity;
}

fn bool re_replace_buffer_init(ReReplaceBuffer* buf, usz initial_len) {
    if (initial_len == usz.max) return false;
    usz capacity = initial_len + 1;
    if (capacity < 32) capacity = 32;

    buf.chars = (char*)mem::malloc(capacity);
    if (buf.chars == null) {
        buf.len = 0;
        buf.capacity = 0;
        return false;
    }

    buf.len = 0;
    buf.capacity = capacity;
    buf.chars[0] = 0;
    return true;
}

fn void re_replace_buffer_free(ReReplaceBuffer* buf) @inline {
    if (buf.chars != null) mem::free(buf.chars);
    buf.chars = null;
    buf.len = 0;
    buf.capacity = 0;
}

fn bool re_replace_buffer_ensure(ReReplaceBuffer* buf, usz needed) {
    if (buf.len == usz.max) return false;
    if (needed >= usz.max - buf.len) return false;

    usz required = buf.len + needed + 1;
    if (required <= buf.capacity) return true;

    usz new_capacity = buf.capacity < 32 ? 32 : buf.capacity;
    while (new_capacity < required) {
        if (new_capacity > usz.max / 2) {
            new_capacity = required;
            break;
        }
        new_capacity *= 2;
    }

    char* next = (char*)mem::malloc(new_capacity);
    if (next == null) return false;
    for (usz i = 0; i < buf.len; i++) next[i] = buf.chars[i];
    next[buf.len] = 0;

    mem::free(buf.chars);
    buf.chars = next;
    buf.capacity = new_capacity;
    return true;
}

fn bool re_replace_append_range(ReReplaceBuffer* result, char[] input, int begin, int end_exclusive) {
    if (begin >= end_exclusive) return true;

    usz count = (usz)(end_exclusive - begin);
    if (!re_replace_buffer_ensure(result, count)) return false;

    for (int j = begin; j < end_exclusive; j++) {
        result.chars[result.len] = input[j];
        result.len++;
    }
    result.chars[result.len] = 0;
    return true;
}

fn bool re_replace_append_text(ReReplaceBuffer* result, char[] text) {
    if (!re_replace_buffer_ensure(result, text.len)) return false;
    for (usz j = 0; j < text.len; j++) {
        result.chars[result.len] = text[j];
        result.len++;
    }
    result.chars[result.len] = 0;
    return true;
}

fn bool re_replace_global(ReReplaceBuffer* result, char[] pattern, char[] replacement, char[] input) {
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);

    int last_end = 0;
    for (int i = 0; i < count; i++) {
        if (!re_replace_append_range(result, input, last_end, matches[i].start)) return false;
        if (!re_replace_append_text(result, replacement)) return false;
        last_end = matches[i].end + 1;
    }
    return re_replace_append_range(result, input, last_end, (int)input.len);
}

fn bool re_replace_first(ReReplaceBuffer* result, char[] pattern, char[] replacement, char[] input) {
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        if (!re_replace_append_range(result, input, 0, m.start)) return false;
        if (!re_replace_append_text(result, replacement)) return false;
        return re_replace_append_range(result, input, m.end + 1, (int)input.len);
    } else {
        return re_replace_append_text(result, input);
    }
}

fn lisp::Value* prim_re_replace(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) return regex_raise(interp, "regex/re-replace-expected-pattern-replacement-input", "re-replace: expected (re-replace pattern replacement input ['global])");

    lisp::Value* pattern_val = args[0];
    lisp::Value* replacement_val = args[1];
    lisp::Value* input_val = args[2];
    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-pattern-not-string", "re-replace: pattern must be a string");
    if (replacement_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-replacement-not-string", "re-replace: replacement must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-replace-input-not-string", "re-replace: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] replacement = replacement_val.str_chars[:replacement_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    ReReplaceBuffer result = { .chars = null, .len = 0, .capacity = 0 };
    if (!re_replace_buffer_init(&result, input.len)) {
        return regex_raise(interp, "regex/re-replace-out-of-memory", "re-replace: failed to allocate result buffer");
    }

    bool ok = re_replace_has_global_flag(args)
        ? re_replace_global(&result, pattern, replacement, input)
        : re_replace_first(&result, pattern, replacement, input);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-replace-invalid-pattern",
            "re-replace: invalid regex pattern")) {
        re_replace_buffer_free(&result);
        return compile_err;
    }
    if (!ok) {
        re_replace_buffer_free(&result);
        return regex_raise(interp, "regex/re-replace-out-of-memory", "re-replace: failed to grow result buffer");
    }

    return lisp::make_string_owned(interp, result.chars, result.len);
}

// ============================================================
// Grammar Definition Primitives
// ============================================================

// Stored grammars by name
