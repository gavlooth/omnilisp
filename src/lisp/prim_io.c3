module lisp;

import std::io;
import std::core::mem;
import main;

// libuv filesystem helpers (csrc/uv_helpers.c)
extern fn int omni_uv_fs_open(char* path, int flags, int mode) @extern("omni_uv_fs_open");
extern fn int omni_uv_fs_close(int fd) @extern("omni_uv_fs_close");
extern fn long omni_uv_fs_read(int fd, char* buf, usz len) @extern("omni_uv_fs_read");
extern fn long omni_uv_fs_write(int fd, char* buf, usz len) @extern("omni_uv_fs_write");
extern fn int omni_uv_fs_rename(char* src, char* dst) @extern("omni_uv_fs_rename");
extern fn int omni_uv_fs_unlink(char* path) @extern("omni_uv_fs_unlink");
extern fn int omni_uv_fs_stat_basic(char* path, ulong* size_out, uint* mode_out) @extern("omni_uv_fs_stat_basic");
extern fn int omni_uv_fs_scandir_join(char* path, char** out_buf, usz* out_len) @extern("omni_uv_fs_scandir_join");
extern fn void omni_uv_free(void* ptr) @extern("free");

// =============================================================================
// I/O PRIMITIVES
// =============================================================================

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        print_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        println_value(args[0], &interp.symbols);
    }
    return make_nil(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    io::printn("");
    return make_nil(interp);
}

// =============================================================================
// SECTION 7.6: FILE I/O PRIMITIVES
// =============================================================================

/**
 * (read-file path) -> string contents or nil on error
 */
fn bool io_uv_read_all_file(char[] path, char** out_buf, usz* out_len) {
    *out_buf = null;
    *out_len = 0;

    int fd = omni_uv_fs_open((ZString)path.ptr, 0, 0);
    if (fd < 0) return false;

    ulong size_out = 0;
    uint mode_out = 0;
    int stat_status = omni_uv_fs_stat_basic((ZString)path.ptr, &size_out, &mode_out);
    if (stat_status < 0) {
        (void)omni_uv_fs_close(fd);
        return false;
    }

    usz cap = (usz)size_out;
    char* buf = (char*)mem::malloc(cap + 1);
    if (buf == null) {
        (void)omni_uv_fs_close(fd);
        return false;
    }

    usz total = 0;
    while (total < cap) {
        long read_n = omni_uv_fs_read(fd, buf + total, cap - total);
        if (read_n < 0) {
            (void)omni_uv_fs_close(fd);
            mem::free(buf);
            return false;
        }
        if (read_n == 0) break;
        total += (usz)read_n;
    }
    (void)omni_uv_fs_close(fd);
    buf[total] = 0;

    *out_buf = buf;
    *out_len = total;
    return true;
}

fn Value* prim_read_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    char* content_buf = null;
    usz content_len = 0;
    if (!io_uv_read_all_file(path, &content_buf, &content_len)) return make_nil(interp);
    defer mem::free(content_buf);

    return make_string(interp, content_buf[:content_len]);
}

/**
 * (write-file path content) -> true on success, nil on error
 */
fn Value* prim_write_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return make_nil(interp);
    }

    // Get path and content as slices (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];
    char[] content = args[1].str_chars[:args[1].str_len];

    int fd = omni_uv_fs_open((ZString)path.ptr, 577, 0o644);
    if (fd < 0) return make_nil(interp);
    defer (void)omni_uv_fs_close(fd);

    usz written = 0;
    while (written < content.len) {
        long n = omni_uv_fs_write(fd, content.ptr + written, content.len - written);
        if (n <= 0) return make_nil(interp);
        written += (usz)n;
    }
    return make_symbol(interp, interp.sym_true);
}

/**
 * (file-exists? path) -> true if file exists, nil otherwise
 */
fn Value* prim_file_exists(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    ulong size_out = 0;
    uint mode_out = 0;
    if (omni_uv_fs_stat_basic((ZString)path.ptr, &size_out, &mode_out) >= 0) {
        return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

/**
 * (read-lines path) -> list of strings (one per line)
 */
fn Value* prim_read_lines(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_nil(interp);
    }

    // Get path as slice (using :len for exclusive end)
    char[] path = args[0].str_chars[:args[0].str_len];

    char* content_buf = null;
    usz content_len = 0;
    if (!io_uv_read_all_file(path, &content_buf, &content_len)) return make_nil(interp);
    defer mem::free(content_buf);

    char[] content = content_buf[:content_len];

    // Split content into lines
    Value* lines = make_nil(interp);
    usz start = 0;
    for (usz i = 0; i <= content_len; i++) {
        if (i == content_len || content[i] == '\n') {
            // Extract line [start..i)
            usz line_len = i - start;
            // Skip trailing \r if present (Windows line endings)
            if (line_len > 0 && content[start + line_len - 1] == '\r') {
                line_len--;
            }
            Value* line = make_string(interp, content[start:line_len]);
            lines = make_cons(interp, line, lines);
            start = i + 1;
        }
    }

    // Reverse the list to get correct order
    Value* result = make_nil(interp);
    while (is_cons(lines)) {
        result = make_cons(interp, lines.cons_val.car, result);
        lines = lines.cons_val.cdr;
    }
    return result;
}

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
        (void)omni_uv_fs_close(fh.fd);
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
        (void)omni_uv_fs_close(fd);
        return io_fs_raise(interp, "io/fs-open-out-of-memory", "fs-open: out of memory");
    }
    fh.fd = fd;
    fh.open = true;

    Value* v = make_ffi_handle_ex(interp, (void*)fh, "fs-handle", &fs_handle_finalizer, true, false);
    if (v == null) {
        (void)omni_uv_fs_close(fd);
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

    int fd = omni_uv_fs_open((ZString)path_v.str_chars, (int)flags_v.int_val, mode);
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

    long read_n = omni_uv_fs_read(fh.fd, buf, n);
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
    long wrote_n = omni_uv_fs_write(fh.fd, data.ptr, data.len);
    if (wrote_n < 0) {
        return io_fs_raise(interp, "io/fs-write-failed", "fs-write: write failed");
    }
    return make_int(interp, wrote_n);
}

// (fs-close handle) -> nil
fn Value* prim_fs_close(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return io_fs_raise(interp, "io/fs-close-expected-handle", "fs-close: expected (fs-close handle)");
    }
    FsHandle* fh = get_fs_handle(args[0]);
    if (fh == null) {
        return io_fs_raise(interp, "io/fs-close-invalid-handle", "fs-close: invalid handle");
    }
    if (!fh.open || fh.fd < 0) return make_nil(interp);

    int status = omni_uv_fs_close(fh.fd);
    fh.fd = -1;
    fh.open = false;
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-close-failed", "fs-close: close failed");
    }
    return make_nil(interp);
}

// (fs-stat path) -> dict {'size n 'mode n}
fn Value* prim_fs_stat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_fs_raise(interp, "io/fs-stat-expected-path", "fs-stat: expected (fs-stat path)");
    }

    ulong size_out = 0;
    uint mode_out = 0;
    int status = omni_uv_fs_stat_basic((ZString)args[0].str_chars, &size_out, &mode_out);
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
    int status = omni_uv_fs_scandir_join((ZString)args[0].str_chars, &joined, &joined_len);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-readdir-failed", "fs-readdir: scandir failed");
    }
    defer if (joined != null) omni_uv_free(joined);

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

// (fs-rename src dst) -> nil
fn Value* prim_fs_rename(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return io_fs_raise(interp, "io/fs-rename-expected-src-dst", "fs-rename: expected (fs-rename src dst)");
    }
    int status = omni_uv_fs_rename((ZString)args[0].str_chars, (ZString)args[1].str_chars);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-rename-failed", "fs-rename: rename failed");
    }
    return make_nil(interp);
}

// (fs-unlink path) -> nil
fn Value* prim_fs_unlink(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_fs_raise(interp, "io/fs-unlink-expected-path", "fs-unlink: expected (fs-unlink path)");
    }
    int status = omni_uv_fs_unlink((ZString)args[0].str_chars);
    if (status < 0) {
        return io_fs_raise(interp, "io/fs-unlink-failed", "fs-unlink: unlink failed");
    }
    return make_nil(interp);
}

/**
 * (load path) -> evaluates all expressions in the file, returns last result
 * Reads a script file and evaluates it in the current environment.
 */
fn Value* prim_load(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "load: expected a string path");
    }

    // Resolve path relative to current source directory
    char[] rel_path = args[0].str_chars[:args[0].str_len];
    char[512] resolved;
    usz rlen = resolve_import_path(rel_path, interp, &resolved, 512);
    char[] path = resolved[:rlen];

    // Try to load file
    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;

        // Push source directory for nested loads/imports
        if (!push_source_dir(path, interp)) {
            return raise_error(interp, "load: failed to allocate source directory context");
        }

        // Run all expressions in the file
        EvalResult r = run_program(source, interp);

        pop_source_dir(interp);

        if (r.error.has_error) {
            char[512] ebuf;
            char[] msg = io::bprintf(&ebuf, "load: error in '%s': %s",
                (ZString)path.ptr, (ZString)&r.error.message)!!;
            return raise_error(interp, msg);
        }
        return r.value;
    }
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "load: file not found '%s'", (ZString)path.ptr)!!;
    return raise_error(interp, msg);
}

// === System primitives (libc-backed) ===

extern fn void* c_popen(char* command, char* mode) @extern("popen");
extern fn int c_pclose(void* stream) @extern("pclose");
extern fn usz c_fread(void* ptr, usz size, usz count, void* stream) @extern("fread");
extern fn int c_feof(void* stream) @extern("feof");
extern fn char* c_getenv(char* name) @extern("getenv");
extern fn long c_time(long* t) @extern("time");
extern fn int c_usleep(uint usec) @extern("usleep");
extern fn void c_exit(int status) @extern("_exit");
/**
 * (shell cmd) -> string (stdout of command)
 * (shell cmd true) -> list (stdout-string exit-code)
 * Runs cmd via popen, captures stdout, returns output as string.
 */
fn Value* prim_shell(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "shell: expected string command");
    }

    void* fp = c_popen((ZString)args[0].str_chars, "r");
    if (fp == null) {
        return raise_error(interp, "shell: popen failed");
    }

    // Read output into buffer
    char[8192] buf;
    usz total = 0;
    while (c_feof(fp) == 0) {
        usz n = c_fread(&buf[total], 1, 8192 - total, fp);
        if (n == 0) break;
        total += n;
        if (total >= 8192) break;
    }
    int status = c_pclose(fp);
    int exit_code = status >> 8;  // WEXITSTATUS

    // Strip trailing newline
    if (total > 0 && buf[total - 1] == '\n') total--;

    Value* output = make_string(interp, buf[:total]);

    // If second arg is truthy, return (output exit-code)
    if (args.len >= 2 && !is_falsy(args[1], interp)) {
        Value* code = make_int(interp, exit_code);
        return make_cons(interp, output, make_cons(interp, code, make_nil(interp)));
    }
    return output;
}

/**
 * (getenv name) -> string or nil
 */
fn Value* prim_getenv(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return raise_error(interp, "getenv: expected string");
    }
    char* val = c_getenv((ZString)args[0].str_chars);
    if (val == null) return make_nil(interp);
    // Measure length
    usz len = 0;
    while (val[len] != 0) len++;
    return make_string(interp, val[:len]);
}

/**
 * (time) -> integer epoch seconds
 * (time-ms) -> integer epoch milliseconds (via clock_gettime)
 */
fn Value* prim_time(Value*[] args, Env* env, Interp* interp) {
    long t = c_time(null);
    return make_int(interp, t);
}

// clock_gettime for millisecond precision
extern fn int c_clock_gettime(int clk_id, void* tp) @extern("clock_gettime");
const int CLOCK_REALTIME = 0;

fn Value* prim_time_ms(Value*[] args, Env* env, Interp* interp) {
    long[2] ts;  // tv_sec, tv_nsec
    c_clock_gettime(CLOCK_REALTIME, &ts);
    long ms = ts[0] * 1000 + ts[1] / 1000000;
    return make_int(interp, ms);
}

/**
 * (exit) or (exit code) -> exits process
 */
fn Value* prim_exit(Value*[] args, Env* env, Interp* interp) {
    int code = 0;
    if (args.len >= 1 && args[0].tag == INT) code = (int)args[0].int_val;
    c_exit(code);
    return make_nil(interp);  // unreachable
}

/**
 * (sleep secs) -> nil
 * secs can be int or double (fractional seconds supported)
 */
fn Value* prim_sleep(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "sleep: expected number of seconds");
    double secs;
    if (args[0].tag == INT) { secs = (double)args[0].int_val; }
    else if (args[0].tag == DOUBLE) { secs = args[0].double_val; }
    else { return raise_error(interp, "sleep: expected number"); }
    uint usec = (uint)(secs * 1000000.0);
    c_usleep(usec);
    return make_nil(interp);
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "display: expected argument");
    Value* v = args[0];
    if (v == null || is_nil(v)) {
        io::print("nil");
    } else if (v.tag == STRING) {
        io::printf("%s", (ZString)v.str_chars); // No quotes for display
    } else {
        print_value(v, &interp.symbols);
    }
    return make_nil(interp);
}
