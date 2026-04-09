module pika;

import std::core::mem;
import lisp;

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

// (re-replace pattern replacement input) -> new string
// (re-replace pattern replacement input 'global) -> replace all
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
