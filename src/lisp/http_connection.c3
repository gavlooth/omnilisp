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

fn usz http_read_connection(Value* conn, bool is_https, char* resp_buf, usz max_len, Interp* interp) {
    usz resp_len = 0;
    while (resp_len < max_len) {
        Value*[1] read_args;
        read_args[0] = conn;
        Value* chunk;
        if (is_https) {
            chunk = prim_tls_read(read_args[..], null, interp);
        } else {
            chunk = prim_tcp_read(read_args[..], null, interp);
        }
        if (chunk == null || chunk.tag == ERROR) break;
        if (!is_string(chunk) || chunk.str_len == 0) break;

        usz copy_len = chunk.str_len;
        if (resp_len + copy_len > max_len) copy_len = max_len - resp_len;
        for (usz i = 0; i < copy_len; i++) resp_buf[resp_len++] = chunk.str_chars[i];
    }
    return resp_len;
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
