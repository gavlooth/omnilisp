module pika;

import std::core::mem;
import lisp;

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
