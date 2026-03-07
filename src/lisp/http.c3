module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// HTTP/1.1 Client primitives
// ============================================================
// ============================================================
// (http-get url) → dict with 'status 'headers 'body
// ============================================================

fn Value* prim_http_get(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "http-get: expected (http-get url)");
    }

    // In a fiber context, offload the entire request (DNS + TLS + read) to the
    // worker thread so the scheduler can service other fibers concurrently.
    if (scheduler_is_running_fiber_context()) {
        Value* op  = make_symbol(interp, interp.symbols.intern("http-get"));
        Value* job = make_cons(interp, op, make_cons(interp, args[0], make_nil(interp)));
        Value* raw = scheduler_run_internal_io_offload_job(job, interp);
        if (raw == null || raw.tag == ERROR) return raw;
        if (!is_string(raw)) return raise_error(interp, "http-get: offload returned non-string");
        return parse_response(raw.str_chars[:raw.str_len], interp);
    }

    char[] url = args[0].str_chars[:args[0].str_len];

    ParsedUrl parsed;
    // fault: lisp::INVALID_SYNTAX
    if (catch err = parse_url(url, &parsed)) {
        return raise_error(interp, "http-get: invalid URL");
    }

    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* tcp = null;
    Value* conn = http_connect_for_request(&parsed, host_val, &tcp, interp);
    if (conn == null || conn.tag == ERROR) return conn;

    char[4096] req_buf;  // buffer: accumulator (keep) — HTTP request assembly, passed to tcp/tls_write
    usz? req_len = build_request("GET", &parsed, "", "", &req_buf, 4096);
    if (catch err = req_len) {
        return raise_error(interp, "http-get: request too large for buffer");
    }
    Value* req_str = make_string(interp, req_buf[:req_len]);

    Value* write_result = http_write_connection(conn, parsed.is_https, req_str, interp);
    if (write_result != null && write_result.tag == ERROR) return write_result;

    // Heap-allocate: 65KB on stack overflows a 64KB StackCtx when called from a handle body.
    char* resp_buf = (char*)mem::malloc(65536);
    if (resp_buf == null) return raise_error(interp, "http-get: out of memory for response buffer");
    defer mem::free(resp_buf);
    usz resp_len = http_read_connection(conn, parsed.is_https, resp_buf, 65000, interp);
    http_close_connection(conn, tcp, parsed.is_https, interp);

    return parse_response(resp_buf[:resp_len], interp);
}

// ============================================================
// (http-request method url headers body) → dict
// ============================================================

fn Value* prim_http_request(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "http-request: expected (http-request method url [headers] [body])");
    if (!is_string(args[0]) || !is_string(args[1])) {
        return raise_error(interp, "http-request: method and url must be strings");
    }

    char[] method = args[0].str_chars[:args[0].str_len];
    char[] url = args[1].str_chars[:args[1].str_len];
    char[] headers = "";
    char[] body = "";
    if (args.len > 2 && is_string(args[2])) headers = args[2].str_chars[:args[2].str_len];
    if (args.len > 3 && is_string(args[3])) body = args[3].str_chars[:args[3].str_len];

    ParsedUrl parsed;
    // fault: lisp::INVALID_SYNTAX
    if (catch err = parse_url(url, &parsed)) return raise_error(interp, "http-request: invalid URL");

    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* tcp = null;
    Value* conn = http_connect_for_request(&parsed, host_val, &tcp, interp);
    if (conn == null || conn.tag == ERROR) return conn;

    // Send request
    char[8192] req_buf;  // buffer: accumulator (keep) — HTTP request assembly, passed to tcp/tls_write
    usz? req_len = build_request(method, &parsed, headers, body, &req_buf, 8192);
    if (catch err = req_len) {
        return raise_error(interp, "http-request: request too large for buffer");
    }
    Value* req_str = make_string(interp, req_buf[:req_len]);
    // Preserve existing behavior: ignore write errors and continue to read/parse.
    http_write_connection(conn, parsed.is_https, req_str, interp);

    // Read response — heap-allocate to avoid StackCtx stack overflow.
    char* resp_buf = (char*)mem::malloc(65536);
    if (resp_buf == null) return raise_error(interp, "http-request: out of memory for response buffer");
    defer mem::free(resp_buf);
    usz resp_len = http_read_connection(conn, parsed.is_https, resp_buf, 65000, interp);
    http_close_connection(conn, tcp, parsed.is_https, interp);

    return parse_response(resp_buf[:resp_len], interp);
}
