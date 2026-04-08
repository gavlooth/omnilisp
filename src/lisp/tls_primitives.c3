module lisp;

import main;

// ============================================================
// (tls-read tls-handle [max-bytes]) -> string
// ============================================================

fn Value* prim_tls_read(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    Value* max_bytes_v = null;

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        handle_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            max_bytes_v = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) {
            return tls_raise_async(interp, "io/tls-read-expected-handle-max-bytes", "tls-read: expected (tls-read handle [max-bytes])");
        }
    } else {
        // fault: lisp::ARITY_MISMATCH
        if (args.len < 1 || args.len > 2) return tls_raise_async(interp, "io/tls-read-expected-handle-max-bytes", "tls-read: expected (tls-read handle [max-bytes])");
        handle_v = args[0];
        if (args.len == 2) max_bytes_v = args[1];
    }

    usz max_bytes = 4096;
    if (max_bytes_v != null) {
        if (!is_int(max_bytes_v)) {
            return tls_raise_async(interp, "io/tls-read-max-bytes-not-integer", "tls-read: max-bytes must be an integer");
        }
        long requested = max_bytes_v.int_val;
        if (requested <= 0) {
            return tls_raise_async(interp, "io/tls-read-max-bytes-invalid", "tls-read: max-bytes must be > 0");
        }
        max_bytes = (usz)requested;
        if (max_bytes > 65536) max_bytes = 65536;
    }

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-read",
        "io/tls-read-fiber-required",
        "io/tls-read-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(handle_v);
    if (th == null) return tls_raise_async(interp, "io/tls-read-invalid-or-closed-handle", "tls-read: invalid or closed handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-read");
    defer tls_unmark_in_flight(th);
    if (!th.connected) return tls_raise_async(interp, "io/tls-read-invalid-or-closed-handle", "tls-read: invalid or closed handle");

    OffloadWork work = {};
    work.op = OFFLOAD_TLS_READ;
    work.int_arg = (long)(uptr)th;
    work.int_arg2 = (long)max_bytes;
    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (out == null) return make_string(interp, "");
    return out;
}

// ============================================================
// (tls-write tls-handle data) -> bytes written
// ============================================================

fn Value* prim_tls_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return tls_raise_async(interp, "io/tls-write-expected-handle-data", "tls-write: expected (tls-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return tls_raise_async(interp, "io/tls-write-expected-data-string", "tls-write: data must be a string");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-write",
        "io/tls-write-fiber-required",
        "io/tls-write-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(args[0]);
    if (th == null) return tls_raise_async(interp, "io/tls-write-invalid-or-closed-handle", "tls-write: invalid or closed handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-write");
    defer tls_unmark_in_flight(th);
    if (!th.connected) return tls_raise_async(interp, "io/tls-write-invalid-or-closed-handle", "tls-write: invalid or closed handle");

    char[] data = args[1].str_chars[:args[1].str_len];
    OffloadWork work = {};
    work.op = OFFLOAD_TLS_WRITE;
    work.int_arg = (long)(uptr)th;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), data);
    if (work.shared == null) return tls_raise_async(interp, "io/tls-write-out-of-memory", "tls-write: out of memory");

    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (is_int(out)) return out;
    return make_int(interp, 0);
}

// ============================================================
// (tls-close tls-handle) -> void
// ============================================================

fn Value* prim_tls_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return tls_raise_async(interp, "io/tls-close-expected-handle", "tls-close: expected (tls-close handle)");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-close",
        "io/tls-close-fiber-required",
        "io/tls-close-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(args[0]);
    if (th == null) return tls_raise_async(interp, "io/tls-close-invalid-handle", "tls-close: invalid handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-close");
    defer tls_unmark_in_flight(th);

    OffloadWork work = {};
    work.op = OFFLOAD_TLS_CLOSE;
    work.int_arg = (long)(uptr)th;
    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    return make_void(interp);
}
