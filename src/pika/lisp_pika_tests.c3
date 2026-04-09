module pika;

import std::core::mem;
import std::io;
import lisp;

fn bool pika_test_quiet_requested() @inline {
    return lisp::test_quiet_requested();
}

fn void pika_emit_pass(char[] msg) @inline {
    if (pika_test_quiet_requested()) return;
    io::printfn("%s", (ZString)msg);
}

fn void pika_emit_banner(char[] msg) @inline {
    if (pika_test_quiet_requested()) return;
    io::printfn("%s", (ZString)msg);
}

fn void regex_test_literal() {
    char[] pattern = "hello";
    char[] input = "say hello world";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched && m.start == 4 && m.end == 8) {
        pika_emit_pass("[PASS] re-match literal: 'hello' in 'say hello world'");
    } else {
        io::printfn("[FAIL] re-match literal: matched=%d start=%d end=%d", m.matched, m.start, m.end);
    }
}

fn void regex_test_char_class() {
    char[] pattern = "[0-9]+";
    char[] input = "abc123def";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        pika_emit_pass("[PASS] re-match char class: '[0-9]+' in 'abc123def'");
    } else {
        io::printn("[FAIL] re-match char class");
    }
}

fn void regex_test_dot() {
    char[] pattern = "a.c";
    char[] input = "abc";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        pika_emit_pass("[PASS] re-match dot: 'a.c' matches 'abc'");
    } else {
        io::printn("[FAIL] re-match dot");
    }
}

fn void regex_test_fullmatch() {
    char[] pattern = "hello";
    char[] input = "hello";
    RegexMatch m = regex_fullmatch(pattern, input);
    if (m.matched) {
        pika_emit_pass("[PASS] re-fullmatch: 'hello' == 'hello'");
    } else {
        io::printn("[FAIL] re-fullmatch");
    }
}

fn void regex_test_fullmatch_partial() {
    char[] pattern = "hello";
    char[] input = "hello world";
    RegexMatch m = regex_fullmatch(pattern, input);
    if (!m.matched) {
        pika_emit_pass("[PASS] re-fullmatch rejects partial: 'hello' != 'hello world'");
    } else {
        io::printn("[FAIL] re-fullmatch should reject partial");
    }
}

fn void regex_test_find_all() {
    char[] pattern = "a";
    char[] input = "abracadabra";
    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (count == 5) {
        pika_emit_pass("[PASS] re-find-all: found 5 'a' in 'abracadabra'");
    } else {
        io::printfn("[FAIL] re-find-all: expected 5, got %d", count);
    }
}

fn void regex_test_digit_escape() {
    char[] pattern = "\\d";
    char[] input = "abc1def";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched && m.start == 3) {
        pika_emit_pass("[PASS] re-match \\d: finds digit");
    } else {
        io::printfn("[FAIL] re-match \\d: matched=%d start=%d", m.matched, m.start);
    }
}

fn void regex_test_word_escape() {
    char[] pattern = "\\w+";
    char[] input = "hello_world";
    RegexMatch m = regex_search(pattern, input);
    if (m.matched) {
        pika_emit_pass("[PASS] re-match \\w+: matches word chars");
    } else {
        io::printn("[FAIL] re-match \\w+");
    }
}

// ============================================================
// Tests
// ============================================================

fn void test_pika_regex(lisp::Interp* interp) {
    pika_emit_banner("=== Pika Regex Tests ===");
    regex_test_literal();
    regex_test_char_class();
    regex_test_dot();
    regex_test_fullmatch();
    regex_test_fullmatch_partial();
    regex_test_find_all();
    regex_test_digit_escape();
    regex_test_word_escape();
    pika_emit_banner("=== Pika Regex Tests Complete ===");
    pika_emit_banner("");
}

fn void test_pika_primitive_re_match_pos(lisp::Interp* interp) {
    lisp::Value*[2] args;
    args[0] = lisp::make_string(interp, "hello");
    args[1] = lisp::make_string(interp, "say hello world");
    lisp::Value* result = prim_re_match_pos(args[..], null, interp);

    if (!lisp::is_cons(result)) {
        io::printn("[FAIL] re-match-pos: expected cons");
        return;
    }

    lisp::Value* start = result.cons_val.car;
    lisp::Value* rest = result.cons_val.cdr;
    if (start.tag != lisp::ValueTag.INT || start.int_val != 4) {
        io::printn("[FAIL] re-match-pos: wrong start position");
        return;
    }
    if (!lisp::is_cons(rest)) {
        io::printn("[FAIL] re-match-pos: missing end");
        return;
    }

    lisp::Value* end = rest.cons_val.car;
    if (end.tag == lisp::ValueTag.INT && end.int_val == 8) {
        pika_emit_pass("[PASS] re-match-pos: returns (4 8)");
    } else {
        io::printn("[FAIL] re-match-pos: wrong end position");
    }
}

fn void test_pika_primitive_re_find_all_pos(lisp::Interp* interp) {
    lisp::Value*[2] args;
    args[0] = lisp::make_string(interp, "a");
    args[1] = lisp::make_string(interp, "abaca");
    lisp::Value* result = prim_re_find_all_pos(args[..], null, interp);

    int count = 0;
    lisp::Value* curr = result;
    while (lisp::is_cons(curr)) {
        count++;
        curr = curr.cons_val.cdr;
    }

    if (count == 3) {
        pika_emit_pass("[PASS] re-find-all-pos: found 3 matches");
    } else {
        io::printfn("[FAIL] re-find-all-pos: expected 3 matches, got %d", count);
    }
}

fn void test_pika_primitive_parse_lisp(lisp::Interp* interp) {
    lisp::Value*[1] args;
    args[0] = lisp::make_string(interp, "(+ 1 2)");
    lisp::Value* result = prim_pika_parse_lisp(args[..], null, interp);

    if (!lisp::is_cons(result)) {
        io::printn("[FAIL] pika/parse-lisp: expected cons");
        return;
    }

    lisp::Value* name = result.cons_val.car;
    if (name.tag == lisp::ValueTag.SYMBOL) {
        pika_emit_pass("[PASS] pika/parse-lisp: returns parsed structure");
    } else {
        io::printn("[FAIL] pika/parse-lisp: first element should be symbol");
    }
}

fn void test_pika_primitive_eval_lisp(lisp::Interp* interp) {
    lisp::Value*[1] args;
    args[0] = lisp::make_string(interp, "(+ 1 2)");
    lisp::Value* result = prim_pika_eval_lisp(args[..], null, interp);

    if (result.tag == lisp::ValueTag.INT && result.int_val == 3) {
        pika_emit_pass("[PASS] pika/eval-lisp: (+ 1 2) evaluates to 3");
    } else if (result.tag == lisp::ValueTag.INT) {
        io::printfn("[FAIL] pika/eval-lisp: expected 3, got %d", (int)result.int_val);
    } else {
        io::printfn("[FAIL] pika/eval-lisp: expected INT, got tag %d", (int)result.tag);
    }
}

fn void test_pika_new_primitives(lisp::Interp* interp) {
    pika_emit_banner("=== Pika New Primitives Tests ===");
    test_pika_primitive_re_match_pos(interp);
    test_pika_primitive_re_find_all_pos(interp);
    test_pika_primitive_parse_lisp(interp);
    test_pika_primitive_eval_lisp(interp);
    pika_emit_banner("=== Pika New Primitives Tests Complete ===");
    pika_emit_banner("");
}
