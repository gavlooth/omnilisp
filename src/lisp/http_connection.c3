module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// HTTP transport helpers (tcp/tls)
// ============================================================
fn Value* http_connect_for_request(ParsedUrl* parsed, Value* host_val, Value** out_tcp, Interp* interp) {
    Value* port_val = make_int(interp, (long)parsed.port);
    Value*[2] conn_args;
    conn_args[0] = host_val;
    conn_args[1] = port_val;
    Value* tcp = prim_tcp_connect(conn_args[..], null, interp);
    if (tcp == null || tcp.tag == ERROR) return tcp;
    *out_tcp = tcp;

    if (!parsed.is_https) return tcp;

    Value*[2] tls_args;
    tls_args[0] = tcp;
    tls_args[1] = host_val;
    Value* conn = prim_tls_connect(tls_args[..], null, interp);
    if (conn == null || conn.tag == ERROR) {
        Value*[1] close_args;
        close_args[0] = tcp;
        prim_tcp_close(close_args[..], null, interp);
        return conn;
    }
    return conn;
}

fn Value* http_write_connection(Value* conn, bool is_https, Value* req_str, Interp* interp) {
    Value*[2] write_args;
    write_args[0] = conn;
    write_args[1] = req_str;
    if (is_https) return prim_tls_write(write_args[..], null, interp);
    return prim_tcp_write(write_args[..], null, interp);
}

fn bool http_ascii_eq_ignore_case(char left, char right) @inline {
    if (left >= 'A' && left <= 'Z') left = (char)(left + 32);
    if (right >= 'A' && right <= 'Z') right = (char)(right + 32);
    return left == right;
}

fn bool http_response_matches_content_length_name(char[] response, usz start, usz colon) {
    char[] expected = "content-length";
    if (colon < start || colon - start != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (!http_ascii_eq_ignore_case(response[start + i], expected[i])) return false;
    }
    return true;
}

fn bool http_is_ascii_digit(char ch) @inline {
    return ch >= '0' && ch <= '9';
}

fn bool http_is_optional_whitespace(char ch) @inline {
    return ch == ' ' || ch == '\t';
}

fn bool http_response_declares_over_read_cap(char[] response, usz max_len) {
    usz header_delim_start = response.len;
    for (usz i = 0; i + 3 < response.len; i++) {
        if (response[i] == '\r' && response[i + 1] == '\n' && response[i + 2] == '\r' && response[i + 3] == '\n') {
            header_delim_start = i;
            break;
        }
    }
    if (header_delim_start == response.len) return false;

    usz header_end = header_delim_start + 4;
    if (header_end > max_len) return true;

    usz line_start = 0;
    while (line_start < header_delim_start) {
        usz line_end = line_start;
        while (line_end + 1 < header_delim_start && !(response[line_end] == '\r' && response[line_end + 1] == '\n')) {
            line_end++;
        }

        usz colon = line_start;
        while (colon < line_end && response[colon] != ':') colon++;
        if (http_response_matches_content_length_name(response, line_start, colon)) {
            usz pos = colon + 1;
            while (pos < line_end && http_is_optional_whitespace(response[pos])) pos++;

            usz declared_len = 0;
            bool saw_digit = false;
            while (pos < line_end && http_is_ascii_digit(response[pos])) {
                saw_digit = true;
                uint digit = (uint)(response[pos] - '0');
                if (declared_len > max_len / 10) return true;
                declared_len = declared_len * 10 + (usz)digit;
                if (declared_len > max_len) return true;
                pos++;
            }

            while (pos < line_end && http_is_optional_whitespace(response[pos])) pos++;
            if (saw_digit && pos == line_end) {
                return declared_len > max_len - header_end;
            }
        }

        if (line_end + 2 > header_delim_start) break;
        line_start = line_end + 2;
    }

    return false;
}

fn Value* http_read_connection(Value* conn, bool is_https, char* resp_buf, usz max_len, usz* out_len, Interp* interp) {
    usz resp_len = 0;
    if (out_len != null) *out_len = 0;
    while (resp_len < max_len) {
        Value*[2] read_args;
        read_args[0] = conn;
        read_args[1] = make_int(interp, (long)(max_len - resp_len));
        if (read_args[1] == null || read_args[1].tag == ERROR) {
            return http_raise(interp, "io/http-response-too-large", "http: response read allocation failed");
        }
        Value* chunk;
        if (is_https) {
            chunk = prim_tls_read(read_args[..], null, interp);
        } else {
            chunk = prim_tcp_read(read_args[..], null, interp);
        }
        if (chunk == null) break;
        if (chunk.tag == ERROR) return chunk;
        if (!is_string(chunk) || chunk.str_len == 0) break;

        usz copy_len = chunk.str_len;
        if (resp_len + copy_len > max_len) {
            return http_raise(interp, "io/http-response-too-large", "http: response too large");
        }
        for (usz i = 0; i < copy_len; i++) resp_buf[resp_len++] = chunk.str_chars[i];
        if (http_response_declares_over_read_cap(resp_buf[:resp_len], max_len)) {
            return http_raise(interp, "io/http-response-too-large", "http: response too large");
        }
    }
    if (resp_len == max_len) {
        Value*[2] probe_args;
        probe_args[0] = conn;
        probe_args[1] = make_int(interp, 1);
        if (probe_args[1] == null || probe_args[1].tag == ERROR) {
            return http_raise(interp, "io/http-response-too-large", "http: response read allocation failed");
        }
        Value* extra = is_https
            ? prim_tls_read(probe_args[..], null, interp)
            : prim_tcp_read(probe_args[..], null, interp);
        if (extra != null && extra.tag == ERROR) return extra;
        if (extra != null && is_string(extra) && extra.str_len > 0) {
            return http_raise(interp, "io/http-response-too-large", "http: response too large");
        }
    }
    if (out_len != null) *out_len = resp_len;
    return null;
}

fn void http_close_connection(Value* conn, Value* tcp, bool is_https, Interp* interp) {
    Value*[1] close_args;
    close_args[0] = conn;
    if (is_https) {
        prim_tls_close(close_args[..], null, interp);
        close_args[0] = tcp;
    }
    prim_tcp_close(close_args[..], null, interp);
}
