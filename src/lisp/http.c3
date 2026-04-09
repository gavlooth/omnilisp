module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// HTTP/1.1 Client primitives
// ============================================================
fn Value* http_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, "http op: requires running fiber context");
}

fn Value* http_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, "http op: libuv loop unavailable");
}

// ============================================================
// (http-get url) → dict with 'status 'headers 'body
// ============================================================

fn Value* prim_http_get(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "http-get: expected (http-get url)");
    }

    char[] url = args[0].str_chars[:args[0].str_len];

    if (!scheduler_is_running_fiber_context()) {
        return http_raise_fiber_required(interp, "http-get", "io/http-get-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return http_raise_loop_unavailable(interp, "http-get", "io/http-get-libuv-unavailable");
    }

    ParsedUrl parsed;
    // fault: lisp::INVALID_SYNTAX
    if (catch err = parse_url(url, &parsed)) {
        return raise_error(interp, "http-get: invalid URL");
    }

    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* tcp = null;
    Value* conn = http_connect_for_request(&parsed, host_val, &tcp, interp);
    if (conn == null || conn.tag == ERROR) return conn;
    defer http_close_connection(conn, tcp, parsed.is_https, interp);

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
    usz resp_len = 0;
    Value* read_result = http_read_connection(conn, parsed.is_https, resp_buf, 65000, &resp_len, interp);
    if (read_result != null && read_result.tag == ERROR) return read_result;

    return parse_response(resp_buf[:resp_len], interp);
}

// ============================================================
// (http-request method url headers body) → dict
// ============================================================

fn Value* prim_http_request(Value*[] args, Env* env, Interp* interp) {
    Value* method_v = null;
    Value* url_v = null;
    Value* headers_v = null;
    Value* body_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        method_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return raise_error(interp, "http-request: expected (http-request method url [headers] [body])");
        url_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            headers_v = car(packed);
            packed = cdr(packed);
            if (is_cons(packed)) body_v = car(packed);
        }
    } else {
        if (args.len < 2) return raise_error(interp, "http-request: expected (http-request method url [headers] [body])");
        method_v = args[0];
        url_v = args[1];
        if (args.len > 2) headers_v = args[2];
        if (args.len > 3) body_v = args[3];
    }
    if (!is_string(method_v) || !is_string(url_v)) {
        return raise_error(interp, "http-request: method and url must be strings");
    }
    if (headers_v != null && !is_string(headers_v)) {
        return raise_error(interp, "http-request: headers must be a string");
    }
    if (body_v != null && !is_string(body_v)) {
        return raise_error(interp, "http-request: body must be a string");
    }

    char[] method = method_v.str_chars[:method_v.str_len];
    char[] url = url_v.str_chars[:url_v.str_len];

    if (!scheduler_is_running_fiber_context()) {
        return http_raise_fiber_required(interp, "http-request", "io/http-request-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return http_raise_loop_unavailable(interp, "http-request", "io/http-request-libuv-unavailable");
    }

    char[] headers = "";
    char[] body = "";
    if (headers_v != null) headers = headers_v.str_chars[:headers_v.str_len];
    if (body_v != null) body = body_v.str_chars[:body_v.str_len];

    ParsedUrl parsed;
    // fault: lisp::INVALID_SYNTAX
    if (catch err = parse_url(url, &parsed)) return raise_error(interp, "http-request: invalid URL");

    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* tcp = null;
    Value* conn = http_connect_for_request(&parsed, host_val, &tcp, interp);
    if (conn == null || conn.tag == ERROR) return conn;
    defer http_close_connection(conn, tcp, parsed.is_https, interp);

    // Send request
    char[8192] req_buf;  // buffer: accumulator (keep) — HTTP request assembly, passed to tcp/tls_write
    usz? req_len = build_request(method, &parsed, headers, body, &req_buf, 8192);
    if (catch err = req_len) {
        return raise_error(interp, "http-request: request too large for buffer");
    }
    Value* req_str = make_string(interp, req_buf[:req_len]);
    Value* write_result = http_write_connection(conn, parsed.is_https, req_str, interp);
    if (write_result != null && write_result.tag == ERROR) return write_result;

    // Read response — heap-allocate to avoid StackCtx stack overflow.
    char* resp_buf = (char*)mem::malloc(65536);
    if (resp_buf == null) return raise_error(interp, "http-request: out of memory for response buffer");
    defer mem::free(resp_buf);
    usz resp_len = 0;
    Value* read_result = http_read_connection(conn, parsed.is_https, resp_buf, 65000, &resp_len, interp);
    if (read_result != null && read_result.tag == ERROR) return read_result;

    return parse_response(resp_buf[:resp_len], interp);
}
