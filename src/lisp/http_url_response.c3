module lisp;

import std::core::mem;
import std::io;
import main;

int g_http_force_parse_response_key_alloc_fail_nth = 0;

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

fn bool http_request_append_bytes(char* buf, usz buf_size, usz* pos, char[] bytes) @inline {
    if (buf == null || pos == null) return false;
    if (*pos > buf_size || bytes.len > buf_size - *pos) return false;
    for (usz i = 0; i < bytes.len; i++) buf[*pos + i] = bytes[i];
    *pos += bytes.len;
    return true;
}

fn bool http_request_append_char_slice(char* buf, usz buf_size, usz* pos, char* bytes, usz len) @inline {
    if (bytes == null && len > 0) return false;
    if (*pos > buf_size || len > buf_size - *pos) return false;
    for (usz i = 0; i < len; i++) buf[*pos + i] = bytes[i];
    *pos += len;
    return true;
}

// Build HTTP/1.1 request string
fn usz? build_request(char[] method, ParsedUrl* url, char[] headers, char[] body, char* buf, usz buf_size) {
    usz pos = 0;

    // Request line: GET /path HTTP/1.1\r\n
    if (!http_request_append_bytes(buf, buf_size, &pos, method)) return WRITE_FAILED~;
    if (!http_request_append_bytes(buf, buf_size, &pos, " ")) return WRITE_FAILED~;
    if (!http_request_append_char_slice(buf, buf_size, &pos, &url.path[0], url.path_len)) return WRITE_FAILED~;
    if (!http_request_append_bytes(buf, buf_size, &pos, " HTTP/1.1\r\n")) return WRITE_FAILED~;

    // Host header
    if (!http_request_append_bytes(buf, buf_size, &pos, "Host: ")) return WRITE_FAILED~;
    if (!http_request_append_char_slice(buf, buf_size, &pos, &url.host[0], url.host_len)) return WRITE_FAILED~;
    if (!http_request_append_bytes(buf, buf_size, &pos, "\r\n")) return WRITE_FAILED~;

    // Connection: close
    if (!http_request_append_bytes(buf, buf_size, &pos, "Connection: close\r\n")) return WRITE_FAILED~;

    // User headers
    if (headers.len > 0) {
        if (!http_request_append_bytes(buf, buf_size, &pos, headers)) return WRITE_FAILED~;
        if (!http_request_append_bytes(buf, buf_size, &pos, "\r\n")) return WRITE_FAILED~;
    }

    // Content-Length for body
    if (body.len > 0) {
        char[32] cl_buf;  // buffer: build-string (DString candidate) — formats Content-Length header
        if (try content_length_line = io::bprintf(&cl_buf, "Content-Length: %d\r\n", (int)body.len)) {
            if (!http_request_append_bytes(buf, buf_size, &pos, content_length_line)) return WRITE_FAILED~;
        } else {
            return WRITE_FAILED~;
        }
    }

    // End of headers
    if (!http_request_append_bytes(buf, buf_size, &pos, "\r\n")) return WRITE_FAILED~;

    // Body
    if (!http_request_append_bytes(buf, buf_size, &pos, body)) return WRITE_FAILED~;

    return pos;
}

fn Value* parse_response_make_symbol_key(Interp* interp, char[] name) {
    if (g_http_force_parse_response_key_alloc_fail_nth > 0) {
        g_http_force_parse_response_key_alloc_fail_nth--;
        if (g_http_force_parse_response_key_alloc_fail_nth == 0) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "http: failed to allocate response field key");
        }
    }

    Value* key = interp.alloc_value();
    if (key == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "http: failed to allocate response field key");
    }
    key.tag = SYMBOL;
    SymbolId key_sym = interp.symbols.intern(name);
    if (key_sym == INVALID_SYMBOL_ID) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "http: failed to allocate response field key");
    }
    key.sym_val = key_sym;
    return key;
}

fn Value* parse_response_oom(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "http: failed to allocate response payload");
}

fn Value* parse_response_malformed(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-http-response", "http: malformed response");
}

// Parse HTTP response: extract status code, headers, body
fn Value* parse_response(char[] response, Interp* interp) {
    usz header_delim_start = response.len;
    for (usz i = 0; i + 3 < response.len; i++) {
        if (response[i] == '\r' && response[i+1] == '\n' && response[i+2] == '\r' && response[i+3] == '\n') {
            header_delim_start = i;
            break;
        }
    }
    if (header_delim_start == response.len) return parse_response_malformed(interp);

    usz status_line_end = response.len;
    for (usz i = 0; i + 1 < response.len && i <= header_delim_start; i++) {
        if (response[i] == '\r' && response[i+1] == '\n') {
            status_line_end = i;
            break;
        }
    }
    if (status_line_end == response.len || status_line_end < 10) return parse_response_malformed(interp);

    if (response[0] != 'H' || response[1] != 'T' || response[2] != 'T' || response[3] != 'P' || response[4] != '/') {
        return parse_response_malformed(interp);
    }

    usz pos = 5;
    while (pos < status_line_end && response[pos] != ' ') pos++;
    if (pos == 5 || pos >= status_line_end) return parse_response_malformed(interp);
    pos++;

    if (pos + 2 >= status_line_end) return parse_response_malformed(interp);
    if (response[pos] < '0' || response[pos] > '9' ||
        response[pos + 1] < '0' || response[pos + 1] > '9' ||
        response[pos + 2] < '0' || response[pos + 2] > '9') {
        return parse_response_malformed(interp);
    }

    int status = ((int)(response[pos] - '0') * 100) +
        ((int)(response[pos + 1] - '0') * 10) +
        (int)(response[pos + 2] - '0');
    pos += 3;
    if (pos < status_line_end && response[pos] != ' ') return parse_response_malformed(interp);

    Value* result = make_hashmap_checked(interp, 8, "http: failed to allocate response payload");
    if (result == null || result.tag != HASHMAP) return parse_response_oom(interp);

    Value* status_key = parse_response_make_symbol_key(interp, "status");
    if (status_key == null || is_error(status_key)) return status_key;
    if (!hashmap_set_checked(result.hashmap_val, status_key, make_int(interp, (long)status), interp)) {
        return parse_response_oom(interp);
    }

    usz header_end = header_delim_start + 4;

    // Headers as string
    Value* headers_key = parse_response_make_symbol_key(interp, "headers");
    if (headers_key == null || is_error(headers_key)) return headers_key;
    usz hdr_start = status_line_end + 2;
    char[] headers = "";
    if (hdr_start < header_delim_start) {
        headers = response[hdr_start..header_delim_start - 1];
    }
    Value* headers_value = make_string(interp, headers);
    if (headers_value == null || is_error(headers_value)) return headers_value;
    if (!hashmap_set_checked(result.hashmap_val, headers_key, headers_value, interp)) {
        return parse_response_oom(interp);
    }

    // Body
    if (header_end > 0 && header_end < response.len) {
        Value* body_key = parse_response_make_symbol_key(interp, "body");
        if (body_key == null || is_error(body_key)) return body_key;
        Value* body_value = make_string(interp, response[header_end..]);
        if (body_value == null || is_error(body_value)) return body_value;
        if (!hashmap_set_checked(result.hashmap_val, body_key, body_value, interp)) {
            return parse_response_oom(interp);
        }
    }

    return result;
}
