module lisp;

import std::io;

fn bool compiler_code_contains_all(char[] code, char[][] needles) {
    foreach (needle : needles) {
        if (!str_contains(code, needle)) return false;
    }
    return true;
}

fn bool compiler_code_contains_none(char[] code, char[][] needles) {
    foreach (needle : needles) {
        if (str_contains(code, needle)) return false;
    }
    return true;
}

fn usz compiler_code_count_occurrences(char[] haystack, char[] needle) {
    if (needle.len == 0) return 0;

    usz count = 0;
    usz search = 0;
    while (search < haystack.len) {
        isz pos = compiler_code_find_from(haystack, needle, search);
        if (pos < 0) break;
        count++;
        search = (usz)pos + needle.len;
    }
    return count;
}

fn bool compiler_code_contains_ordered(char[] code, char[][] needles) {
    usz search = 0;
    foreach (needle : needles) {
        isz pos = compiler_code_find_from(code, needle, search);
        if (pos < 0) return false;
        search = (usz)pos + needle.len;
    }
    return true;
}

fn isz compiler_code_find_from(char[] haystack, char[] needle, usz start) {
    if (needle.len == 0) return (isz)start;
    if (needle.len > haystack.len || start > haystack.len - needle.len) return -1;

    for (usz i = start; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return (isz)i;
    }
    return -1;
}

fn bool compiler_code_range_equals(char[] haystack, usz start, char[] needle) {
    if (start + needle.len > haystack.len) return false;
    for (usz i = 0; i < needle.len; i++) {
        if (haystack[start + i] != needle[i]) return false;
    }
    return true;
}

fn bool compiler_code_digit_ranges_equal(
    char[] haystack,
    usz left_start,
    usz left_end,
    usz right_start,
    usz right_end
) {
    usz left_len = left_end - left_start;
    usz right_len = right_end - right_start;
    if (left_len != right_len) return false;
    for (usz i = 0; i < left_len; i++) {
        if (haystack[left_start + i] != haystack[right_start + i]) return false;
    }
    return true;
}

fn bool compiler_char_is_digit(char c) @inline {
    return c >= '0' && c <= '9';
}

fn bool compiler_match_guard_uses_scanned_lambda(char[] code) {
    char[] guard_needle = "match_guard_eval(";
    char[] closure_prefix = "aot::make_generated_closure_with_retention(null, &invoke_lambda_";
    char[] sig_prefix = "fn lisp::Value* invoke_lambda_";
    char[] sig_suffix = "(void* _self, lisp::Value* v)";

    isz guard_pos = compiler_code_find_from(code, guard_needle, 0);
    if (guard_pos < 0) return false;

    isz closure_pos = -1;
    usz search = 0;
    while (true) {
        isz next_pos = compiler_code_find_from(code, closure_prefix, search);
        if (next_pos < 0 || next_pos > guard_pos) break;
        closure_pos = next_pos;
        search = (usz)next_pos + 1;
    }
    if (closure_pos < 0) return false;

    usz lambda_digit_start = (usz)closure_pos + closure_prefix.len;
    usz lambda_digit_end = lambda_digit_start;
    while (lambda_digit_end < code.len && compiler_char_is_digit(code[lambda_digit_end])) {
        lambda_digit_end++;
    }
    if (lambda_digit_end == lambda_digit_start) return false;

    search = 0;
    while (true) {
        isz sig_pos = compiler_code_find_from(code, sig_prefix, search);
        if (sig_pos < 0) return false;

        usz sig_digit_start = (usz)sig_pos + sig_prefix.len;
        usz sig_digit_end = sig_digit_start;
        while (sig_digit_end < code.len && compiler_char_is_digit(code[sig_digit_end])) {
            sig_digit_end++;
        }

        if (sig_digit_end > sig_digit_start &&
            compiler_code_digit_ranges_equal(code, lambda_digit_start, lambda_digit_end, sig_digit_start, sig_digit_end) &&
            compiler_code_range_equals(code, sig_digit_end, sig_suffix)) {
            return true;
        }
        search = (usz)sig_pos + 1;
    }
}

fn void compiler_test_finish_bool(
    bool ok,
    char[] pass_msg,
    char[] fail_msg,
    usz* pass,
    usz* fail
) {
    if (ok) {
        (*pass)++;
        compiler_print_pass(pass_msg);
        return;
    }
    (*fail)++;
    io::printn((ZString)fail_msg);
}
