module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// HTTP URL parsing and response parsing
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

fn void? parse_url(char[] url, ParsedUrl* out) {
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
            return INVALID_SYNTAX~;
        }
    } else {
        return INVALID_SYNTAX~;
    }

    // Host
    out.host_len = 0;
    while (pos < url.len && url[pos] != '/' && url[pos] != ':' && out.host_len < 255) {
        out.host[out.host_len++] = url[pos++];
    }
    out.host[out.host_len] = 0;
    if (out.host_len == 0) return INVALID_SYNTAX~;

    // Optional port
    if (pos < url.len && url[pos] == ':') {
        pos++;  // skip :
        int port = 0;
        usz port_digits = 0;
        while (pos < url.len && url[pos] >= '0' && url[pos] <= '9') {
            if (port > 6553 || (port == 6553 && (int)(url[pos] - '0') > 5)) return INVALID_SYNTAX~;
            port = port * 10 + (int)(url[pos] - '0');
            port_digits++;
            pos++;
        }
        if (port_digits == 0 || port <= 0 || port > 65535) return INVALID_SYNTAX~;
        out.port = port;
        if (pos < url.len && url[pos] != '/') return INVALID_SYNTAX~;
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

    if (pos < url.len) return INVALID_SYNTAX~;
}

// Build HTTP/1.1 request string
fn usz? build_request(char[] method, ParsedUrl* url, char[] headers, char[] body, char* buf, usz buf_size) {
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
        usz cl_len = 0;
        if (try cl_line = io::bprintf(&cl_buf, "Content-Length: %d\r\n", (int)body.len)) {
            cl_len = cl_line.len;
        } else {
            return WRITE_FAILED~;
        }
        for (usz i = 0; i < cl_len && pos < buf_size; i++) buf[pos++] = cl_buf[i];
    }

    // End of headers
    if (pos + 1 < buf_size) { buf[pos++] = '\r'; buf[pos++] = '\n'; }

    // Body
    for (usz i = 0; i < body.len && pos < buf_size; i++) buf[pos++] = body[i];

    // Buffer too small to hold the full request
    if (pos >= buf_size) return WRITE_FAILED~;

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
            make_string(interp, response[hdr_start..header_end - 5]), interp);
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
