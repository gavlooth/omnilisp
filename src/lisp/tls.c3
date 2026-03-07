module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// BearSSL extern declarations — direct calls, no wrapper
// ============================================================

// BearSSL struct sizes (from bearssl.h on x86_64):
//   br_ssl_client_context: 3720 bytes
//   br_x509_minimal_context: 3168 bytes
//   br_sslio_context: 40 bytes
//   br_ssl_engine_context is at offset 0 inside br_ssl_client_context
//   BR_SSL_BUFSIZE_BIDI: 33178 bytes

const usz BEARSSL_CLIENT_CTX_SIZE = 3720;
const usz BEARSSL_X509_CTX_SIZE  = 3168;
const usz BEARSSL_SSLIO_CTX_SIZE = 40;
const usz BEARSSL_IOBUF_SIZE     = 33178;

// Callback type for br_sslio_init: int (*)(void *ctx, unsigned char *buf, size_t len)
alias BrSslioCallback = fn int(void* ctx, char* buf, usz len);

extern fn void br_ssl_client_init_full(void* cc, void* xc, void* tas, usz num_tas) @extern("br_ssl_client_init_full");
extern fn void br_ssl_engine_set_buffer(void* eng, void* iobuf, usz iobuf_len, int bidi) @extern("br_ssl_engine_set_buffer");
extern fn int br_ssl_client_reset(void* cc, char* server_name, int resume_session) @extern("br_ssl_client_reset");
extern fn void br_ssl_server_init_full_rsa(void* cc, void* chain, usz chain_len, void* sk) @extern("br_ssl_server_init_full_rsa");
extern fn int br_ssl_server_reset(void* cc) @extern("br_ssl_server_reset");
extern fn void br_sslio_init(void* ctx, void* engine, BrSslioCallback low_read, void* read_ctx, BrSslioCallback low_write, void* write_ctx) @extern("br_sslio_init");
extern fn int br_sslio_read(void* ctx, char* buf, usz len) @extern("br_sslio_read");
extern fn int br_sslio_write_all(void* ctx, char* buf, usz len) @extern("br_sslio_write_all");
extern fn int br_sslio_flush(void* ctx) @extern("br_sslio_flush");
extern fn int br_sslio_close(void* ctx) @extern("br_sslio_close");

// Socket I/O callbacks + trust-store helpers (from csrc/tls_helpers.c)
extern fn int omni_tls_sock_read(void* ctx, char* buf, usz len) @extern("omni_tls_sock_read");
extern fn int omni_tls_sock_write(void* ctx, char* buf, usz len) @extern("omni_tls_sock_write");
extern fn void* omni_tls_trust_store_load_file(char* path) @extern("omni_tls_trust_store_load_file");
extern fn void* omni_tls_trust_store_load_default() @extern("omni_tls_trust_store_load_default");
extern fn void* omni_tls_trust_store_get_anchors(void* store_handle) @extern("omni_tls_trust_store_get_anchors");
extern fn usz omni_tls_trust_store_get_anchor_count(void* store_handle) @extern("omni_tls_trust_store_get_anchor_count");
extern fn void omni_tls_trust_store_free(void* store_handle) @extern("omni_tls_trust_store_free");
extern fn usz omni_tls_server_context_size() @extern("omni_tls_server_context_size");
extern fn void* omni_tls_server_creds_load(char* cert_pem_path, char* key_pem_path) @extern("omni_tls_server_creds_load");
extern fn void* omni_tls_server_creds_chain(void* creds_handle) @extern("omni_tls_server_creds_chain");
extern fn usz omni_tls_server_creds_chain_len(void* creds_handle) @extern("omni_tls_server_creds_chain_len");
extern fn void* omni_tls_server_creds_rsa_key(void* creds_handle) @extern("omni_tls_server_creds_rsa_key");
extern fn void omni_tls_server_creds_free(void* creds_handle) @extern("omni_tls_server_creds_free");
extern fn int omni_tls_client_set_single_rsa(void* client_ctx, void* creds_handle) @extern("omni_tls_client_set_single_rsa");
extern fn int omni_tls_client_session_cache_load(void* client_ctx, ZString hostname) @extern("omni_tls_client_session_cache_load");
extern fn int omni_tls_client_session_cache_store(void* client_ctx, ZString hostname) @extern("omni_tls_client_session_cache_store");

// ============================================================
// TLS Handle — all BearSSL state in one allocation
// ============================================================

struct TlsHandle {
    void* client_ctx;   // br_ssl_client_context OR br_ssl_server_context
    void* x509_ctx;     // br_x509_minimal_context (malloc'd, 3168 bytes)
    void* sslio_ctx;    // br_sslio_context (malloc'd, 40 bytes)
    void* iobuf;        // I/O buffer (malloc'd, 33178 bytes)
    int*  fd_ptr;       // pointer to fd (stable address for callbacks)
    void* trust_store;  // CA trust anchors (malloc'd by c helper)
    void* server_creds; // local certificate chain + RSA key (malloc'd by c helper)
    char* session_host; // copied hostname for optional client session cache
    bool  resume_session;
    bool  connected;
}

fn Value* make_tls_handle_val(Interp* interp, TlsHandle* th) {
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tls-handle", &tls_handle_finalizer, true, false);
    if (v == null) {
        return raise_error(interp, "tls-connect: out of memory");
    }
    return v;
}

fn TlsHandle* get_tls_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (TlsHandle*)v.ffi_val.lib_handle;
}

fn void tls_handle_free(TlsHandle* th) {
    if (th == null) return;
    if (th.client_ctx != null) { mem::free(th.client_ctx); th.client_ctx = null; }
    if (th.x509_ctx != null) { mem::free(th.x509_ctx); th.x509_ctx = null; }
    if (th.sslio_ctx != null) { mem::free(th.sslio_ctx); th.sslio_ctx = null; }
    if (th.iobuf != null) { mem::free(th.iobuf); th.iobuf = null; }
    if (th.fd_ptr != null) { mem::free(th.fd_ptr); th.fd_ptr = null; }
    if (th.session_host != null) { mem::free(th.session_host); th.session_host = null; }
    if (th.trust_store != null) { omni_tls_trust_store_free(th.trust_store); th.trust_store = null; }
    if (th.server_creds != null) { omni_tls_server_creds_free(th.server_creds); th.server_creds = null; }
}

fn void tls_maybe_store_client_session(TlsHandle* th) {
    if (th == null || !th.resume_session || th.session_host == null || th.client_ctx == null) return;
    omni_tls_client_session_cache_store(th.client_ctx, (ZString)th.session_host);
}

fn void tls_handle_finalizer(void* handle) {
    TlsHandle* th = (TlsHandle*)handle;
    if (th == null) return;
    if (th.connected && th.sslio_ctx != null) {
        tls_maybe_store_client_session(th);
        br_sslio_close(th.sslio_ctx);
        th.connected = false;
    }
    tls_handle_free(th);
}

// ============================================================
// (tls-connect tcp-handle hostname [ca-bundle-path] [client-cert-pem-path client-key-pem-path] [resume-session?]) → tls-handle
// ============================================================

fn bool tls_is_resume_policy_value(Value* v, Interp* interp) {
    return is_symbol(v) && (v.sym_val == interp.sym_true || v.sym_val == interp.sym_false);
}

fn Value* prim_tls_connect(Value*[] args, Env* env, Interp* interp) {
    Value* tcp_v = null;
    Value* host_v = null;
    Value* ca_bundle_v = null;
    Value* client_cert_v = null;
    Value* client_key_v = null;
    Value* resume_policy_v = null;
    Value*[6] argv;
    usz argc = 0;
    char[] expected = "tls-connect: expected (tls-connect tcp-handle hostname [ca-bundle-path] [client-cert-pem-path client-key-pem-path] [resume-session?])";

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        while (is_cons(packed)) {
            if (argc >= argv.len) return raise_error(interp, expected);
            argv[argc++] = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) return raise_error(interp, expected);
    } else {
        if (args.len < 2 || args.len > argv.len) return raise_error(interp, expected);
        for (usz i = 0; i < args.len; i++) argv[i] = args[i];
        argc = args.len;
    }

    if (argc < 2) return raise_error(interp, expected);

    tcp_v = argv[0];
    host_v = argv[1];
    usz tail_len = argc - 2;

    switch (tail_len) {
        case 0:
            break;
        case 1:
            if (tls_is_resume_policy_value(argv[2], interp)) {
                resume_policy_v = argv[2];
            } else {
                ca_bundle_v = argv[2];
            }
            break;
        case 2:
            if (tls_is_resume_policy_value(argv[3], interp)) {
                ca_bundle_v = argv[2];
                resume_policy_v = argv[3];
            } else {
                client_cert_v = argv[2];
                client_key_v = argv[3];
            }
            break;
        case 3:
            if (tls_is_resume_policy_value(argv[4], interp)) {
                client_cert_v = argv[2];
                client_key_v = argv[3];
                resume_policy_v = argv[4];
            } else {
                ca_bundle_v = argv[2];
                client_cert_v = argv[3];
                client_key_v = argv[4];
            }
            break;
        case 4:
            ca_bundle_v = argv[2];
            client_cert_v = argv[3];
            client_key_v = argv[4];
            resume_policy_v = argv[5];
            break;
        default:
            return raise_error(interp, expected);
    }

    if (!is_string(host_v)) return raise_error(interp, "tls-connect: hostname must be a string");
    if (ca_bundle_v != null && !is_string(ca_bundle_v)) return raise_error(interp, "tls-connect: ca-bundle-path must be a string");
    if (client_cert_v != null && !is_string(client_cert_v)) return raise_error(interp, "tls-connect: client-cert-pem-path must be a string");
    if (client_key_v != null && !is_string(client_key_v)) return raise_error(interp, "tls-connect: client-key-pem-path must be a string");
    if (resume_policy_v != null && !tls_is_resume_policy_value(resume_policy_v, interp)) {
        return raise_error(interp, "tls-connect: resume-session? must be true or false");
    }
    if ((client_cert_v == null) != (client_key_v == null)) {
        return raise_error(interp, "tls-connect: client cert and key paths must be provided together");
    }

    bool resume_session = resume_policy_v != null && resume_policy_v.sym_val == interp.sym_true;
    TcpHandle* tcp = get_tcp_handle(tcp_v);
    // fault: lisp::TYPE_MISMATCH
    if (tcp == null || !tcp.connected) return raise_error(interp, "tls-connect: invalid TCP handle");

    char[] hostname = host_v.str_chars[:host_v.str_len];

    // Null-terminate hostname
    char[256] host_buf;  // buffer: C-interop (keep) — passed to br_ssl_client_reset
    usz hlen = hostname.len < 255 ? hostname.len : 255;
    for (usz i = 0; i < hlen; i++) host_buf[i] = hostname[i];
    host_buf[hlen] = 0;

    // Allocate BearSSL state
    TlsHandle* th = (TlsHandle*)mem::malloc(TlsHandle.sizeof);
    // fault: lisp::TLS_HANDSHAKE_FAILED
    if (th == null) return raise_error(interp, "tls-connect: out of memory");
    *th = {};

    th.client_ctx = mem::malloc(BEARSSL_CLIENT_CTX_SIZE);
    th.x509_ctx = mem::malloc(BEARSSL_X509_CTX_SIZE);
    th.sslio_ctx = mem::malloc(BEARSSL_SSLIO_CTX_SIZE);
    th.iobuf = mem::malloc(BEARSSL_IOBUF_SIZE);
    th.fd_ptr = (int*)mem::malloc(int.sizeof);

    if (th.client_ctx == null || th.x509_ctx == null || th.sslio_ctx == null ||
        th.iobuf == null || th.fd_ptr == null) {
        tls_handle_free(th);
        mem::free(th);
        // fault: lisp::TLS_HANDSHAKE_FAILED
        return raise_error(interp, "tls-connect: out of memory");
    }

    *th.fd_ptr = tcp.fd;
    th.connected = true;
    th.resume_session = resume_session;

    if (th.resume_session) {
        th.session_host = (char*)mem::malloc(hlen + 1);
        if (th.session_host == null) {
            tls_handle_free(th);
            mem::free(th);
            return raise_error(interp, "tls-connect: out of memory");
        }
        for (usz i = 0; i < hlen; i++) th.session_host[i] = host_buf[i];
        th.session_host[hlen] = 0;
    }

    if (ca_bundle_v != null) {
        th.trust_store = omni_tls_trust_store_load_file((ZString)ca_bundle_v.str_chars);
    } else {
        th.trust_store = omni_tls_trust_store_load_default();
    }
    if (th.trust_store == null) {
        tls_handle_free(th);
        mem::free(th);
        if (ca_bundle_v != null) {
            return raise_error(interp, "tls-connect: failed to load CA trust store from ca-bundle-path");
        }
        return raise_error(interp, "tls-connect: failed to load CA trust store (set OMNI_TLS_CA_FILE or SSL_CERT_FILE)");
    }

    void* trust_anchors = omni_tls_trust_store_get_anchors(th.trust_store);
    usz trust_anchor_count = omni_tls_trust_store_get_anchor_count(th.trust_store);
    if (trust_anchors == null || trust_anchor_count == 0) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-connect: trust store has no usable CA certificates");
    }

    // Initialize BearSSL with configured trust anchors.
    br_ssl_client_init_full(th.client_ctx, th.x509_ctx, trust_anchors, trust_anchor_count);

    if (client_cert_v != null) {
        th.server_creds = omni_tls_server_creds_load((ZString)client_cert_v.str_chars, (ZString)client_key_v.str_chars);
        if (th.server_creds == null || !omni_tls_client_set_single_rsa(th.client_ctx, th.server_creds)) {
            tls_handle_free(th);
            mem::free(th);
            return raise_error(interp, "tls-connect: failed to configure client cert/key (PEM RSA required)");
        }
    }

    // eng is at offset 0 inside client_ctx, so client_ctx IS the engine pointer
    br_ssl_engine_set_buffer(th.client_ctx, th.iobuf, BEARSSL_IOBUF_SIZE, 1);
    if (th.resume_session && th.session_host != null) {
        omni_tls_client_session_cache_load(th.client_ctx, (ZString)th.session_host);
    }

    int resume_flag = th.resume_session ? 1 : 0;
    int reset_ok = br_ssl_client_reset(th.client_ctx, &host_buf, resume_flag);
    if (reset_ok == 0) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-connect: TLS client reset failed");
    }

    // Init I/O wrapper with socket callbacks
    br_sslio_init(th.sslio_ctx, th.client_ctx,
        (BrSslioCallback)&omni_tls_sock_read, th.fd_ptr,
        (BrSslioCallback)&omni_tls_sock_write, th.fd_ptr);

    return make_tls_handle_val(interp, th);
}

// ============================================================
// (tls-server-wrap tcp-handle cert-pem-path key-pem-path) → tls-handle
// ============================================================

fn Value* prim_tls_server_wrap(Value*[] args, Env* env, Interp* interp) {
    Value* tcp_v = null;
    Value* cert_path_v = null;
    Value* key_path_v = null;

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        tcp_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return raise_error(interp, "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        cert_path_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return raise_error(interp, "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        key_path_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) return raise_error(interp, "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
    } else {
        if (args.len < 3) return raise_error(interp, "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        tcp_v = args[0];
        cert_path_v = args[1];
        key_path_v = args[2];
    }

    if (!is_string(cert_path_v)) return raise_error(interp, "tls-server-wrap: cert-pem-path must be a string");
    if (!is_string(key_path_v)) return raise_error(interp, "tls-server-wrap: key-pem-path must be a string");

    TcpHandle* tcp = get_tcp_handle(tcp_v);
    if (tcp == null || !tcp.connected || tcp.mode != TCP_STREAM) {
        return raise_error(interp, "tls-server-wrap: invalid TCP stream handle");
    }

    TlsHandle* th = (TlsHandle*)mem::malloc(TlsHandle.sizeof);
    if (th == null) return raise_error(interp, "tls-server-wrap: out of memory");
    *th = {};

    usz server_ctx_size = omni_tls_server_context_size();
    if (server_ctx_size == 0) {
        mem::free(th);
        return raise_error(interp, "tls-server-wrap: invalid server context size");
    }

    th.client_ctx = mem::malloc(server_ctx_size);
    th.sslio_ctx = mem::malloc(BEARSSL_SSLIO_CTX_SIZE);
    th.iobuf = mem::malloc(BEARSSL_IOBUF_SIZE);
    th.fd_ptr = (int*)mem::malloc(int.sizeof);
    if (th.client_ctx == null || th.sslio_ctx == null || th.iobuf == null || th.fd_ptr == null) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-server-wrap: out of memory");
    }

    th.server_creds = omni_tls_server_creds_load((ZString)cert_path_v.str_chars, (ZString)key_path_v.str_chars);
    if (th.server_creds == null) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-server-wrap: failed to load server cert/key PEM files (RSA key required)");
    }

    void* chain = omni_tls_server_creds_chain(th.server_creds);
    usz chain_len = omni_tls_server_creds_chain_len(th.server_creds);
    void* rsa_key = omni_tls_server_creds_rsa_key(th.server_creds);
    if (chain == null || chain_len == 0 || rsa_key == null) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-server-wrap: invalid server credentials");
    }

    *th.fd_ptr = tcp.fd;
    th.connected = true;

    br_ssl_server_init_full_rsa(th.client_ctx, chain, chain_len, rsa_key);
    br_ssl_engine_set_buffer(th.client_ctx, th.iobuf, BEARSSL_IOBUF_SIZE, 1);
    if (br_ssl_server_reset(th.client_ctx) == 0) {
        tls_handle_free(th);
        mem::free(th);
        return raise_error(interp, "tls-server-wrap: TLS server reset failed");
    }

    br_sslio_init(th.sslio_ctx, th.client_ctx,
        (BrSslioCallback)&omni_tls_sock_read, th.fd_ptr,
        (BrSslioCallback)&omni_tls_sock_write, th.fd_ptr);

    return make_tls_handle_val(interp, th);
}

// ============================================================
// (tls-read tls-handle [max-bytes]) → string
// ============================================================

fn Value* prim_tls_read(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "tls-read: expected (tls-read handle [max-bytes])");

    TlsHandle* th = get_tls_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected) return raise_error(interp, "tls-read: invalid or closed handle");

    usz max_bytes = 4096;
    if (args.len >= 2 && is_int(args[1])) {
        max_bytes = (usz)args[1].int_val;
        if (max_bytes > 65536) max_bytes = 65536;
    }

    char* buf = (char*)mem::malloc(max_bytes);
    // fault: lisp::READ_FAILED
    if (buf == null) return raise_error(interp, "tls-read: out of memory");
    defer mem::free(buf);

    int received = br_sslio_read(th.sslio_ctx, buf, max_bytes);
    if (received < 0) {
        // fault: lisp::READ_FAILED
        return raise_error(interp, "tls-read: read failed");
    }
    if (received == 0) {
        return make_string(interp, "");
    }

    Value* result = make_string(interp, buf[:(usz)received]);
    return result;
}

// ============================================================
// (tls-write tls-handle data) → bytes written
// ============================================================

fn Value* prim_tls_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return raise_error(interp, "tls-write: expected (tls-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return raise_error(interp, "tls-write: data must be a string");

    TlsHandle* th = get_tls_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected) return raise_error(interp, "tls-write: invalid or closed handle");

    char[] data = args[1].str_chars[:args[1].str_len];
    int status = br_sslio_write_all(th.sslio_ctx, data.ptr, data.len);
    // fault: lisp::WRITE_FAILED
    if (status < 0) return raise_error(interp, "tls-write: write failed");

    int flush_status = br_sslio_flush(th.sslio_ctx);
    // fault: lisp::WRITE_FAILED
    if (flush_status < 0) return raise_error(interp, "tls-write: flush failed");

    return make_int(interp, (long)data.len);
}

// ============================================================
// (tls-close tls-handle) → nil
// ============================================================

fn Value* prim_tls_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "tls-close: expected (tls-close handle)");

    TlsHandle* th = get_tls_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null) return raise_error(interp, "tls-close: invalid handle");

    if (th.connected) {
        tls_maybe_store_client_session(th);
        br_sslio_close(th.sslio_ctx);
        tls_handle_free(th);
        th.connected = false;
    }

    return make_nil(interp);
}
