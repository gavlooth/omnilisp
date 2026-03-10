module lisp;

import std::core::mem;
import std::io;
import std::atomic::types;

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
    types::Atomic{int} in_flight;
}

fn Value* make_tls_handle_val(Interp* interp, TlsHandle* th) {
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tls-handle", &tls_handle_finalizer, true, false);
    if (v == null) {
        return tls_raise_async(interp, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
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

fn bool tls_try_mark_in_flight(TlsHandle* th) @inline {
    if (th == null) return false;
    int prev = th.in_flight.add(1);
    if (prev > 0) {
        th.in_flight.add(-1);
        return false;
    }
    return true;
}

fn void tls_unmark_in_flight(TlsHandle* th) @inline {
    if (th == null) return;
    th.in_flight.add(-1);
}

fn Value* tls_raise_handle_busy(Interp* interp, char[] op_name) {
    char[112] ebuf;
    char[] msg = io::bprintf(&ebuf, "%s: handle has operation in flight", (ZString)op_name)!!;
    return tls_raise_async(interp, "io/tls-handle-busy", msg);
}

fn void tls_destroy_handle(TlsHandle* th) {
    if (th == null) return;
    tls_handle_free(th);
    mem::free(th);
}
