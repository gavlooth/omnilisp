module lisp;

import std::core::mem;

struct FsHandle {
    int fd;
    bool open;
}

fn Value* io_fs_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, data);
}

fn void fs_handle_finalizer(void* handle) {
    FsHandle* fh = (FsHandle*)handle;
    if (fh == null) return;
    if (fh.open && fh.fd >= 0) {
        (void)fs_uv_close(fh.fd);
        fh.fd = -1;
        fh.open = false;
    }
}

fn FsHandle* get_fs_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "fs-handle")) return null;
    return (FsHandle*)v.ffi_val.lib_handle;
}

fn Value* make_fs_handle(Interp* interp, int fd) {
    FsHandle* fh = (FsHandle*)mem::malloc(FsHandle.sizeof);
    if (fh == null) {
        (void)fs_uv_close(fd);
        return io_fs_raise(interp, "io/fs-open-out-of-memory", "fs-open: out of memory");
    }
    fh.fd = fd;
    fh.open = true;

    bool ctor_took_ownership = false;
    Value* v = make_ffi_handle_ex(interp, (void*)fh, "fs-handle", &fs_handle_finalizer, true, false, &ctor_took_ownership);
    if (v == null) {
        if (!ctor_took_ownership) {
            (void)fs_uv_close(fd);
            mem::free(fh);
        }
        return io_fs_raise(interp, "io/fs-open-out-of-memory", "fs-open: out of memory");
    }
    return v;
}

// (fs-open path flags [mode]) -> fs-handle
fn Value* prim_fs_open(Value*[] args, Env* env, Interp* interp) {
    Value* path_v = null;
    Value* flags_v = null;
    Value* mode_v = null;

    // Effect fast-path variadic call shape: one packed cons payload.
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        path_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_fs_raise(interp, "io/fs-open-expected-path-flags", "fs-open: expected (fs-open path flags [mode])");
        }
        flags_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) mode_v = car(packed);
    } else if (args.len >= 2) {
        path_v = args[0];
        flags_v = args[1];
        if (args.len >= 3) mode_v = args[2];
    } else {
        return io_fs_raise(interp, "io/fs-open-expected-path-flags", "fs-open: expected (fs-open path flags [mode])");
    }

    if (!is_string(path_v) || !is_int(flags_v)) {
        return io_fs_raise(interp, "io/fs-open-expected-path-flags", "fs-open: expected (fs-open path flags [mode])");
    }

    int mode = 420;
    if (mode_v != null) {
        if (!is_int(mode_v)) {
            return io_fs_raise(interp, "io/fs-open-mode-not-integer", "fs-open: mode must be an integer");
        }
        mode = (int)mode_v.int_val;
    }

    int fd = fs_uv_open((ZString)path_v.str_chars, (int)flags_v.int_val, mode);
    if (fd < 0) {
        return io_fs_raise(interp, "io/fs-open-failed", "fs-open: open failed");
    }
    return make_fs_handle(interp, fd);
}

// (fs-read handle n) -> string
fn Value* prim_fs_read(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[1])) {
        return io_fs_raise(interp, "io/fs-read-expected-handle-bytes", "fs-read: expected (fs-read handle n)");
    }
    FsHandle* fh = get_fs_handle(args[0]);
    if (fh == null || !fh.open || fh.fd < 0) {
        return io_fs_raise(interp, "io/fs-read-invalid-handle", "fs-read: invalid or closed handle");
    }

    long req_n = args[1].int_val;
    if (req_n < 0) {
        return io_fs_raise(interp, "io/fs-read-negative-bytes", "fs-read: n must be >= 0");
    }
    usz n = (usz)req_n;
    if (n == 0) return make_string(interp, "");
    if (n > 8 * 1024 * 1024) n = 8 * 1024 * 1024;

    char* buf = (char*)mem::malloc(n);
    if (buf == null) {
        return io_fs_raise(interp, "io/fs-read-out-of-memory", "fs-read: out of memory");
    }
    defer mem::free(buf);

    long read_n = fs_uv_read(fh.fd, buf, n);
    if (read_n < 0) {
        return io_fs_raise(interp, "io/fs-read-failed", "fs-read: read failed");
    }
    if (read_n == 0) return make_string(interp, "");
    return make_string(interp, buf[:(usz)read_n]);
}

// (fs-write handle data) -> int bytes-written
fn Value* prim_fs_write(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[1])) {
        return io_fs_raise(interp, "io/fs-write-expected-handle-data", "fs-write: expected (fs-write handle data)");
    }
    FsHandle* fh = get_fs_handle(args[0]);
    if (fh == null || !fh.open || fh.fd < 0) {
        return io_fs_raise(interp, "io/fs-write-invalid-handle", "fs-write: invalid or closed handle");
    }

    char[] data = args[1].str_chars[:args[1].str_len];
    long wrote_n = fs_uv_write(fh.fd, data.ptr, data.len);
    if (wrote_n < 0) {
        return io_fs_raise(interp, "io/fs-write-failed", "fs-write: write failed");
    }
    return make_int(interp, wrote_n);
}

// (fs-close handle) -> void
fn Value* prim_fs_close(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return io_fs_raise(interp, "io/fs-close-expected-handle", "fs-close: expected (fs-close handle)");
    }
    FsHandle* fh = get_fs_handle(args[0]);
    if (fh == null) {
        return io_fs_raise(interp, "io/fs-close-invalid-handle", "fs-close: invalid handle");
    }
    if (!fh.open || fh.fd < 0) return make_void(interp);

    int status = fs_uv_close(fh.fd);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-close-failed", "fs-close: close failed");
    }
    fh.fd = -1;
    fh.open = false;
    return make_void(interp);
}
