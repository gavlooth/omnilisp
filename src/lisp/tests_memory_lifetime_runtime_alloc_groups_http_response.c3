module lisp;

import std::io;

fn void run_memory_lifetime_runtime_alloc_groups_http_response_tests(Interp* interp, int* pass, int* fail) {
    bool first_ok = false;
    bool second_ok = false;
    bool third_ok = false;

    int saved_nth = g_http_force_parse_response_key_alloc_fail_nth;

    g_http_force_parse_response_key_alloc_fail_nth = 1;
    Value* first_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
    first_ok = runtime_alloc_error_matches(first_fail, "http: failed to allocate response field key");

    g_http_force_parse_response_key_alloc_fail_nth = 2;
    Value* second_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
    second_ok = runtime_alloc_error_matches(second_fail, "http: failed to allocate response field key");

    g_http_force_parse_response_key_alloc_fail_nth = 3;
    Value* third_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
    third_ok = runtime_alloc_error_matches(third_fail, "http: failed to allocate response field key");

    g_http_force_parse_response_key_alloc_fail_nth = saved_nth;

    bool ok = first_ok && second_ok && third_ok;
    if (ok) {
        emit_pass_literal("[PASS] lifetime: http response key alloc failures fail closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] lifetime: http response key alloc failures fail closed (first=%s second=%s third=%s)",
            first_ok ? "yes" : "no",
            second_ok ? "yes" : "no",
            third_ok ? "yes" : "no");
        (*fail)++;
    }
}
