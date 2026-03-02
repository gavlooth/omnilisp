module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// HTTP/1.1 Client — composes TCP + TLS + string parsing
//
// (http-get url) → dict with 'status, 'headers, 'body
// (http-request method url headers body) → dict
// ============================================================

// Parse URL into (scheme host port path)
struct ParsedUrl {
    bool   is_https;
    char[256] host;   // buffer: C-interop (keep) — null-terminated, passed to tcp-connect
    usz    host_len;
    int    port;
    char[1024] path;  // buffer: C-interop (keep) — null-terminated, copied into request line
    usz    path_len;
}

fn bool parse_url(char[] url, ParsedUrl* out) {
    usz pos = 0;

    // Scheme
    out.is_https = false;
    out.port = 80;
    if (url.len > 8 && url[0] == 'h' && url[1] == 't' && url[2] == 't' && url[3] == 'p') {
        if (url[4] == 's' && url[5] == ':' && url[6] == '/' && url[7] == '/') {
            out.is_https = true;
            out.port = 443;
            pos = 8;
        } else if (url[4] == ':' && url[5] == '/' && url[6] == '/') {
            pos = 7;
        } else {
            return false;
        }
    } else {
        return false;
    }

    // Host
    out.host_len = 0;
    while (pos < url.len && url[pos] != '/' && url[pos] != ':' && out.host_len < 255) {
        out.host[out.host_len++] = url[pos++];
    }
    out.host[out.host_len] = 0;

    // Optional port
    if (pos < url.len && url[pos] == ':') {
        pos++;  // skip :
        int port = 0;
        while (pos < url.len && url[pos] >= '0' && url[pos] <= '9') {
            port = port * 10 + (int)(url[pos] - '0');
            pos++;
        }
        if (port > 0) out.port = port;
    }

    // Path (default /)
    out.path_len = 0;
    if (pos < url.len && url[pos] == '/') {
        while (pos < url.len && out.path_len < 1023) {
            out.path[out.path_len++] = url[pos++];
        }
    } else {
        out.path[0] = '/';
        out.path_len = 1;
    }
    out.path[out.path_len] = 0;

    return out.host_len > 0;
}

// Build HTTP/1.1 request string
fn usz build_request(char[] method, ParsedUrl* url, char[] headers, char[] body, char* buf, usz buf_size) {
    usz pos = 0;

    // Request line: GET /path HTTP/1.1\r\n
    for (usz i = 0; i < method.len && pos < buf_size; i++) buf[pos++] = method[i];
    if (pos < buf_size) buf[pos++] = ' ';
    for (usz i = 0; i < url.path_len && pos < buf_size; i++) buf[pos++] = url.path[i];
    char[] ver = " HTTP/1.1\r\n";
    for (usz i = 0; i < ver.len && pos < buf_size; i++) buf[pos++] = ver[i];

    // Host header
    char[] host_hdr = "Host: ";
    for (usz i = 0; i < host_hdr.len && pos < buf_size; i++) buf[pos++] = host_hdr[i];
    for (usz i = 0; i < url.host_len && pos < buf_size; i++) buf[pos++] = url.host[i];
    if (pos + 1 < buf_size) { buf[pos++] = '\r'; buf[pos++] = '\n'; }

    // Connection: close
    char[] conn = "Connection: close\r\n";
    for (usz i = 0; i < conn.len && pos < buf_size; i++) buf[pos++] = conn[i];

    // User headers
    if (headers.len > 0) {
        for (usz i = 0; i < headers.len && pos < buf_size; i++) buf[pos++] = headers[i];
        if (pos + 1 < buf_size) { buf[pos++] = '\r'; buf[pos++] = '\n'; }
    }

    // Content-Length for body
    if (body.len > 0) {
        char[32] cl_buf;  // buffer: build-string (DString candidate) — formats Content-Length header
        usz cl_len = io::bprintf(&cl_buf, "Content-Length: %d\r\n", (int)body.len)!!.len;
        for (usz i = 0; i < cl_len && pos < buf_size; i++) buf[pos++] = cl_buf[i];
    }

    // End of headers
    if (pos + 1 < buf_size) { buf[pos++] = '\r'; buf[pos++] = '\n'; }

    // Body
    for (usz i = 0; i < body.len && pos < buf_size; i++) buf[pos++] = body[i];

    return pos;
}

// Parse HTTP response: extract status code, headers, body
fn Value* parse_response(char[] response, Interp* interp) {
    Value* result = make_hashmap(interp, 8);

    // Find status code: "HTTP/1.1 200 OK\r\n"
    usz pos = 0;
    // Skip "HTTP/x.x "
    while (pos < response.len && response[pos] != ' ') pos++;
    if (pos < response.len) pos++;  // skip space

    // Parse status code
    int status = 0;
    while (pos < response.len && response[pos] >= '0' && response[pos] <= '9') {
        status = status * 10 + (int)(response[pos] - '0');
        pos++;
    }

    Value* status_key = interp.alloc_value(); status_key.tag = SYMBOL;
    status_key.sym_val = interp.symbols.intern("status");
    hashmap_set(result.hashmap_val, status_key, make_int(interp, (long)status), interp);

    // Find end of headers: \r\n\r\n
    usz header_end = 0;
    for (usz i = 0; i + 3 < response.len; i++) {
        if (response[i] == '\r' && response[i+1] == '\n' && response[i+2] == '\r' && response[i+3] == '\n') {
            header_end = i + 4;
            break;
        }
    }

    // Headers as string
    if (header_end > 0) {
        Value* headers_key = interp.alloc_value(); headers_key.tag = SYMBOL;
        headers_key.sym_val = interp.symbols.intern("headers");
        // Skip first line (status line)
        usz hdr_start = 0;
        while (hdr_start < header_end && !(response[hdr_start] == '\n')) hdr_start++;
        if (hdr_start < header_end) hdr_start++;  // skip \n
        hashmap_set(result.hashmap_val, headers_key,
            make_string(interp, response[hdr_start..header_end - 3]), interp);
    }

    // Body
    if (header_end > 0 && header_end < response.len) {
        Value* body_key = interp.alloc_value(); body_key.tag = SYMBOL;
        body_key.sym_val = interp.symbols.intern("body");
        hashmap_set(result.hashmap_val, body_key,
            make_string(interp, response[header_end..]), interp);
    }

    return result;
}

// ============================================================
// (http-get url) → dict with 'status 'headers 'body
// ============================================================

fn Value* prim_http_get(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "http-get: expected (http-get url)");
    }

    char[] url = args[0].str_chars[:args[0].str_len];

    ParsedUrl parsed;
    if (!parse_url(url, &parsed)) {
        return raise_error(interp, "http-get: invalid URL");
    }

    // Connect via TCP
    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* port_val = make_int(interp, (long)parsed.port);
    Value*[2] conn_args;
    conn_args[0] = host_val;
    conn_args[1] = port_val;
    Value* tcp = prim_tcp_connect(conn_args[..], null, interp);
    if (tcp == null || tcp.tag == ERROR) return tcp;

    // Wrap with TLS if HTTPS
    Value* conn = tcp;
    if (parsed.is_https) {
        Value*[2] tls_args;
        tls_args[0] = tcp;
        tls_args[1] = host_val;
        conn = prim_tls_connect(tls_args[..], null, interp);
        if (conn == null || conn.tag == ERROR) {
            Value*[1] close_args; close_args[0] = tcp;
            prim_tcp_close(close_args[..], null, interp);
            return conn;
        }
    }

    // Build and send request
    char[4096] req_buf;  // buffer: accumulator (keep) — HTTP request assembly, passed to tcp/tls_write
    usz req_len = build_request("GET", &parsed, "", "", &req_buf, 4096);
    Value* req_str = make_string(interp, req_buf[:req_len]);

    Value*[2] write_args;
    write_args[0] = conn;
    write_args[1] = req_str;
    Value* write_result;
    if (parsed.is_https) {
        write_result = prim_tls_write(write_args[..], null, interp);
    } else {
        write_result = prim_tcp_write(write_args[..], null, interp);
    }
    if (write_result != null && write_result.tag == ERROR) return write_result;

    // Read response (accumulate until connection closes)
    char[65536] resp_buf;  // buffer: accumulator (keep) — HTTP response bytes, passed to parse_response
    usz resp_len = 0;

    while (resp_len < 65000) {
        Value*[1] read_args;
        read_args[0] = conn;
        Value* chunk;
        if (parsed.is_https) {
            chunk = prim_tls_read(read_args[..], null, interp);
        } else {
            chunk = prim_tcp_read(read_args[..], null, interp);
        }

        if (chunk == null || chunk.tag == ERROR) break;
        if (!is_string(chunk) || chunk.str_len == 0) break;

        // Append to buffer
        usz copy_len = chunk.str_len;
        if (resp_len + copy_len > 65000) copy_len = 65000 - resp_len;
        for (usz i = 0; i < copy_len; i++) resp_buf[resp_len++] = chunk.str_chars[i];
    }

    // Close connection
    Value*[1] close_args;
    close_args[0] = conn;
    if (parsed.is_https) {
        prim_tls_close(close_args[..], null, interp);
        close_args[0] = tcp;
    }
    prim_tcp_close(close_args[..], null, interp);

    // Parse response
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
    if (!parse_url(url, &parsed)) return raise_error(interp, "http-request: invalid URL");

    // Connect
    Value* host_val = make_string(interp, parsed.host[:parsed.host_len]);
    Value* port_val = make_int(interp, (long)parsed.port);
    Value*[2] conn_args = { host_val, port_val };
    Value* tcp = prim_tcp_connect(conn_args[..], null, interp);
    if (tcp == null || tcp.tag == ERROR) return tcp;

    Value* conn = tcp;
    if (parsed.is_https) {
        Value*[2] tls_args = { tcp, host_val };
        conn = prim_tls_connect(tls_args[..], null, interp);
        if (conn == null || conn.tag == ERROR) {
            Value*[1] ca = { tcp }; prim_tcp_close(ca[..], null, interp);
            return conn;
        }
    }

    // Send request
    char[8192] req_buf;  // buffer: accumulator (keep) — HTTP request assembly, passed to tcp/tls_write
    usz req_len = build_request(method, &parsed, headers, body, &req_buf, 8192);
    Value* req_str = make_string(interp, req_buf[:req_len]);

    Value*[2] wa = { conn, req_str };
    if (parsed.is_https) { prim_tls_write(wa[..], null, interp); }
    else { prim_tcp_write(wa[..], null, interp); }

    // Read response
    char[65536] resp_buf;  // buffer: accumulator (keep) — HTTP response bytes, passed to parse_response
    usz resp_len = 0;
    while (resp_len < 65000) {
        Value*[1] ra = { conn };
        Value* chunk;
        if (parsed.is_https) { chunk = prim_tls_read(ra[..], null, interp); }
        else { chunk = prim_tcp_read(ra[..], null, interp); }
        if (chunk == null || chunk.tag == ERROR || !is_string(chunk) || chunk.str_len == 0) break;
        usz cl = chunk.str_len;
        if (resp_len + cl > 65000) cl = 65000 - resp_len;
        for (usz i = 0; i < cl; i++) resp_buf[resp_len++] = chunk.str_chars[i];
    }

    // Close
    Value*[1] ca = { conn };
    if (parsed.is_https) { prim_tls_close(ca[..], null, interp); ca[0] = tcp; }
    prim_tcp_close(ca[..], null, interp);

    return parse_response(resp_buf[:resp_len], interp);
}
