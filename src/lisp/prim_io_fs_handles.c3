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
    Value* promoted = boundary_promote_to_root(val, interp);
    if (promoted == null || promoted.tag == ERROR) return false;
    arr.items[arr.length++] = promoted;
    return true;
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

    Value* out = make_hashmap_checked(interp, 8, "fs-stat: out of memory");
    if (out == null || out.tag != HASHMAP) {
        return io_fs_raise(interp, "io/fs-stat-out-of-memory", "fs-stat: out of memory");
    }
    Value* size_key = make_symbol(interp, interp.symbols.intern("size"));
    Value* mode_key = make_symbol(interp, interp.symbols.intern("mode"));
    if (!hashmap_set_checked(out.hashmap_val, size_key, make_int(interp, (long)size_out), interp) ||
        !hashmap_set_checked(out.hashmap_val, mode_key, make_int(interp, (long)mode_out), interp)) {
        return io_fs_raise(interp, "io/fs-stat-out-of-memory", "fs-stat: out of memory");
    }
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

    Value* out = make_array_checked(interp, 8, "fs-readdir: out of memory");
    if (out == null || out.tag != ARRAY) {
        return io_fs_raise(interp, "io/fs-readdir-out-of-memory", "fs-readdir: out of memory");
    }
    if (joined == null || joined_len == 0) return out;

    usz start = 0;
    for (usz i = 0; i <= joined_len; i++) {
        if (i == joined_len || joined[i] == '\n') {
            usz line_len = i - start;
            if (line_len > 0) {
                Value* name_v = make_string(interp, joined[start:line_len]);
                if (name_v == null || name_v.tag == ERROR) return name_v;
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
