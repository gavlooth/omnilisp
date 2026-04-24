module pika;

import std::core::mem;
import lisp;

int g_regex_force_result_string_alloc_fail_nth = 0;

fn bool regex_result_string_should_force_alloc_fail() @inline {
    if (g_regex_force_result_string_alloc_fail_nth <= 0) return false;
    g_regex_force_result_string_alloc_fail_nth--;
    return g_regex_force_result_string_alloc_fail_nth == 0;
}

fn lisp::Value* regex_make_string_or_error(lisp::Interp* interp, char[] text, char[] code, char[] message) {
    if (regex_result_string_should_force_alloc_fail()) {
        return regex_raise(interp, code, message);
    }

    lisp::Value* result = lisp::make_string(interp, text);
    if (result == null || result.tag == lisp::ValueTag.ERROR) {
        return regex_raise(interp, code, message);
    }
    return result;
}

fn lisp::Value* regex_make_cons_or_error(
    lisp::Interp* interp,
    lisp::Value* car,
    lisp::Value* cdr,
    char[] code,
    char[] message
) {
    lisp::Value* result = lisp::make_cons_or_error(interp, car, cdr, message);
    if (result == null || result.tag == lisp::ValueTag.ERROR) {
        return regex_raise(interp, code, message);
    }
    return result;
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
        return regex_make_string_or_error(
            interp,
            m.view,
            "regex/re-match-out-of-memory",
            "re-match: failed to allocate result string");
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
        return regex_make_string_or_error(
            interp,
            m.view,
            "regex/re-fullmatch-out-of-memory",
            "re-fullmatch: failed to allocate result string");
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

    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* s = regex_make_string_or_error(
            interp,
            matches[i].view,
            "regex/re-find-all-out-of-memory",
            "re-find-all: failed to allocate result string");
        if (s == null || s.tag == lisp::ValueTag.ERROR) return s;
        lisp::Value* next = regex_make_cons_or_error(
            interp,
            s,
            result,
            "regex/re-find-all-out-of-memory",
            "re-find-all: failed to allocate result list");
        if (next == null || next.tag == lisp::ValueTag.ERROR) return next;
        result = next;
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

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-split-invalid-pattern",
            "re-split: invalid regex pattern")) {
        return compile_err;
    }

    int right = (int)input.len;
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        int part_start = matches[i].end < matches[i].start ? matches[i].start : matches[i].end + 1;
        char[] part = part_start < right ? input[part_start..right - 1] : "";
        lisp::Value* part_value = regex_make_string_or_error(
            interp,
            part,
            "regex/re-split-out-of-memory",
            "re-split: failed to allocate result string");
        if (part_value == null || part_value.tag == lisp::ValueTag.ERROR) return part_value;
        lisp::Value* next = regex_make_cons_or_error(
            interp,
            part_value,
            result,
            "regex/re-split-out-of-memory",
            "re-split: failed to allocate result list");
        if (next == null || next.tag == lisp::ValueTag.ERROR) return next;
        result = next;
        right = matches[i].start;
    }
    char[] part = right > 0 ? input[0..right - 1] : "";
    lisp::Value* part_value = regex_make_string_or_error(
        interp,
        part,
        "regex/re-split-out-of-memory",
        "re-split: failed to allocate result string");
    if (part_value == null || part_value.tag == lisp::ValueTag.ERROR) return part_value;
    lisp::Value* next = regex_make_cons_or_error(
        interp,
        part_value,
        result,
        "regex/re-split-out-of-memory",
        "re-split: failed to allocate result list");
    if (next == null || next.tag == lisp::ValueTag.ERROR) return next;
    result = next;

    return result;
}
