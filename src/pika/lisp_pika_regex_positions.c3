module pika;

import std::core::mem;
import lisp;

fn lisp::Value* prim_re_match_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-match-pos-expected-pattern-input", "re-match-pos: expected (re-match-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-pattern-not-string", "re-match-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-input-not-string", "re-match-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_search(pattern, input);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-match-pos-invalid-pattern",
            "re-match-pos: invalid regex pattern")) {
        return compile_err;
    }

    if (m.matched) {
        // Return (start end) pair
        lisp::Value* start = lisp::make_int(interp, m.start);
        lisp::Value* end = lisp::make_int(interp, m.end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        return lisp::make_cons(interp, start, end_list);
    }

    return lisp::make_nil(interp);
}

// (re-find-all-pos pattern input) -> ((start1 end1) (start2 end2) ...)
// Returns list of (start end) pairs for all matches
fn lisp::Value* prim_re_find_all_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-find-all-pos-expected-pattern-input", "re-find-all-pos: expected (re-find-all-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-pattern-not-string", "re-find-all-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-input-not-string", "re-find-all-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-find-all-pos-invalid-pattern",
            "re-find-all-pos: invalid regex pattern")) {
        return compile_err;
    }

    // Build list of (start end) pairs
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* start = lisp::make_int(interp, matches[i].start);
        lisp::Value* end = lisp::make_int(interp, matches[i].end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        lisp::Value* pair = lisp::make_cons(interp, start, end_list);
        result = lisp::make_cons(interp, pair, result);
    }

    return result;
}
