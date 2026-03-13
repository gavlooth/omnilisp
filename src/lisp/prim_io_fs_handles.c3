module lisp;

import std::core::mem;

// libuv filesystem helpers (csrc/uv_helpers.c)
extern fn int fs_uv_open(char* path, int flags, int mode) @extern("omni_uv_fs_open");
extern fn int fs_uv_close(int fd) @extern("omni_uv_fs_close");
extern fn long fs_uv_read(int fd, char* buf, usz len) @extern("omni_uv_fs_read");
extern fn long fs_uv_write(int fd, char* buf, usz len) @extern("omni_uv_fs_write");
extern fn int fs_uv_rename(char* src, char* dst) @extern("omni_uv_fs_rename");
extern fn int fs_uv_unlink(char* path) @extern("omni_uv_fs_unlink");
extern fn int fs_uv_stat_basic(char* path, ulong* size_out, uint* mode_out) @extern("omni_uv_fs_stat_basic");
extern fn int fs_uv_scandir_join(char* path, char** out_buf, usz* out_len) @extern("omni_uv_fs_scandir_join");
extern fn void fs_uv_free(void* ptr) @extern("free");

// =============================================================================
// SECTION 7.6b: libuv FILE HANDLE PRIMITIVES
// =============================================================================

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
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return null;
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

    Value* v = make_ffi_handle_ex(interp, (void*)fh, "fs-handle", &fs_handle_finalizer, true, false);
    if (v == null) {
        (void)fs_uv_close(fd);
        mem::free(fh);
        return io_fs_raise(interp, "io/fs-open-out-of-memory", "fs-open: out of memory");
    }
    return v;
}

fn bool fs_array_push(Value* arr_v, Value* val, Interp* interp) {
    if (arr_v == null || arr_v.tag != ARRAY || arr_v.array_val == null) return false;
    Array* arr = arr_v.array_val;
    if (arr.length >= arr.capacity) {
        usz new_cap = arr.capacity * 2;
        Value** new_items = (Value**)mem::malloc(Value*.sizeof * new_cap);
        if (new_items == null) return false;
        for (usz i = 0; i < arr.length; i++) new_items[i] = arr.items[i];
        mem::free(arr.items);
        arr.items = new_items;
        arr.capacity = new_cap;
    }
    arr.items[arr.length++] = boundary_promote_to_root(val, interp);
    return true;
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
    fh.fd = -1;
    fh.open = false;
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-close-failed", "fs-close: close failed");
    }
    return make_void(interp);
}

// (fs-stat path) -> dict {'size n 'mode n}
fn Value* prim_fs_stat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_fs_raise(interp, "io/fs-stat-expected-path", "fs-stat: expected (fs-stat path)");
    }

    ulong size_out = 0;
    uint mode_out = 0;
    int status = fs_uv_stat_basic((ZString)args[0].str_chars, &size_out, &mode_out);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-stat-failed", "fs-stat: stat failed");
    }

    Value* out = make_hashmap(interp, 8);
    Value* size_key = make_symbol(interp, interp.symbols.intern("size"));
    Value* mode_key = make_symbol(interp, interp.symbols.intern("mode"));
    hashmap_set(out.hashmap_val, size_key, make_int(interp, (long)size_out), interp);
    hashmap_set(out.hashmap_val, mode_key, make_int(interp, (long)mode_out), interp);
    return out;
}

// (fs-readdir path) -> array of entry names
fn Value* prim_fs_readdir(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_fs_raise(interp, "io/fs-readdir-expected-path", "fs-readdir: expected (fs-readdir path)");
    }

    char* joined = null;
    usz joined_len = 0;
    int status = fs_uv_scandir_join((ZString)args[0].str_chars, &joined, &joined_len);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-readdir-failed", "fs-readdir: scandir failed");
    }
    defer if (joined != null) fs_uv_free(joined);

    Value* out = make_array(interp, 8);
    if (joined == null || joined_len == 0) return out;

    usz start = 0;
    for (usz i = 0; i <= joined_len; i++) {
        if (i == joined_len || joined[i] == '\n') {
            usz line_len = i - start;
            if (line_len > 0) {
                Value* name_v = make_string(interp, joined[start:line_len]);
                if (!fs_array_push(out, name_v, interp)) {
                    return io_fs_raise(interp, "io/fs-readdir-out-of-memory", "fs-readdir: out of memory");
                }
            }
            start = i + 1;
        }
    }
    return out;
}

// (fs-rename src dst) -> void
fn Value* prim_fs_rename(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return io_fs_raise(interp, "io/fs-rename-expected-src-dst", "fs-rename: expected (fs-rename src dst)");
    }
    int status = fs_uv_rename((ZString)args[0].str_chars, (ZString)args[1].str_chars);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-rename-failed", "fs-rename: rename failed");
    }
    return make_void(interp);
}

// (fs-unlink path) -> void
fn Value* prim_fs_unlink(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_fs_raise(interp, "io/fs-unlink-expected-path", "fs-unlink: expected (fs-unlink path)");
    }
    int status = fs_uv_unlink((ZString)args[0].str_chars);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-unlink-failed", "fs-unlink: unlink failed");
    }
    return make_void(interp);
}
