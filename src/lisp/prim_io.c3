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

const int IO_STDOUT_FD = 1;
const usz IO_CONSOLE_RENDER_INITIAL_CAP = 256;
const usz IO_CONSOLE_RENDER_MAX_CAP = 16 * 1024 * 1024;

fn bool io_console_write_all(char[] text) {
    usz offset = 0;
    while (offset < text.len) {
        long wrote_n = omni_uv_fs_write(IO_STDOUT_FD, text.ptr + offset, text.len - offset);
        if (wrote_n <= 0) return false;
        offset += (usz)wrote_n;
    }
    return true;
}

fn bool io_console_copy_slice(char[] src, char** out_buf, usz* out_len) {
    char* copy = (char*)mem::malloc(src.len + 1);
    if (copy == null) return false;
    for (usz i = 0; i < src.len; i++) copy[i] = src[i];
    copy[src.len] = 0;
    *out_buf = copy;
    *out_len = src.len;
    return true;
}

fn bool io_console_render_value(Value* value, SymbolTable* syms, bool display_mode, char** out_buf, usz* out_len) {
    *out_buf = null;
    *out_len = 0;

    if (value == null || is_nil(value)) {
        return io_console_copy_slice("nil", out_buf, out_len);
    }
    if (display_mode && value.tag == STRING) {
        return io_console_copy_slice(value.str_chars[:value.str_len], out_buf, out_len);
    }

    usz cap = IO_CONSOLE_RENDER_INITIAL_CAP;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return false;

    while (true) {
        usz rendered = print_value_to_buf(value, syms, buf, cap);
        if (rendered + 1 < cap) {
            *out_buf = buf;
            *out_len = rendered;
            return true;
        }

        if (cap >= IO_CONSOLE_RENDER_MAX_CAP) {
            mem::free(buf);
            return false;
        }
        usz next_cap = cap * 2;
        if (next_cap > IO_CONSOLE_RENDER_MAX_CAP) next_cap = IO_CONSOLE_RENDER_MAX_CAP;
        if (next_cap <= cap) {
            mem::free(buf);
            return false;
        }

        mem::free(buf);
        cap = next_cap;
        buf = (char*)mem::malloc(cap);
        if (buf == null) return false;
    }
}

fn Value* io_console_emit(Value* value, bool display_mode, bool append_newline, Interp* interp) {
    if (value != null) {
        char* rendered = null;
        usz rendered_len = 0;
        if (io_console_render_value(value, &interp.symbols, display_mode, &rendered, &rendered_len)) {
            defer mem::free(rendered);
            (void)io_console_write_all(rendered[:rendered_len]);
        }
    }
    if (append_newline) {
        char[1] nl;
        nl[0] = '\n';
        (void)io_console_write_all(nl[..]);
    }
    return make_nil(interp);
}

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, false, interp);
    }
    return make_nil(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, true, interp);
    }
    return make_nil(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    return io_console_emit(null, false, true, interp);
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "display: expected argument");
    return io_console_emit(args[0], true, false, interp);
}

// =============================================================================
// SECTION 7.6: FILE I/O PRIMITIVES
// =============================================================================

/**
 * (read-file path) -> string contents or nil on error
 */
fn Value* io_file_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, null);
}

fn Value* io_file_require_async_runtime(Interp* interp, char[] op_name, char[] fiber_code, char[] loop_code) {
    if (!scheduler_is_running_fiber_context()) {
        char[96] ebuf;
        char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)!!;
        return io_file_raise(interp, fiber_code, msg);
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        char[96] ebuf;
        char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)!!;
        return io_file_raise(interp, loop_code, msg);
    }
    return null;
}

fn Value* io_run_fiber_offload(OffloadWork* work, Interp* interp) {
    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(work, fiber_id, interp);
    if (start_err != null) return start_err;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    return scheduler_consume_pending_offload(fiber_id, interp);
}

enum IoReadFileStatus {
    IO_READ_FILE_OK,
    IO_READ_FILE_OUT_OF_MEMORY,
    IO_READ_FILE_FAILED,
    IO_READ_FILE_RUNTIME_ERROR,
}

struct IoReadFileResult {
    IoReadFileStatus status;
    Value* value;
}

fn IoReadFileResult io_async_read_file_result(char[] path, Interp* interp) {
    IoReadFileResult result = {
        .status = IO_READ_FILE_FAILED,
        .value = null
    };

    OffloadWork work = {};
    work.op = OFFLOAD_READ_FILE;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), path);
    if (work.shared == null) {
        result.status = IO_READ_FILE_OUT_OF_MEMORY;
        return result;
    }

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) {
        result.status = IO_READ_FILE_RUNTIME_ERROR;
        result.value = out;
        return result;
    }
    if (out == null || is_nil(out) || !is_string(out)) {
        result.status = IO_READ_FILE_FAILED;
        return result;
    }
    result.status = IO_READ_FILE_OK;
    result.value = out;
    return result;
}

fn Value* io_async_read_file_value(char[] path, Interp* interp) {
    IoReadFileResult result = io_async_read_file_result(path, interp);
    switch (result.status) {
        case IO_READ_FILE_OK:
            return result.value;
        case IO_READ_FILE_OUT_OF_MEMORY:
            return io_file_raise(interp, "io/read-file-out-of-memory", "read-file: out of memory");
        case IO_READ_FILE_RUNTIME_ERROR:
            return result.value;
        case IO_READ_FILE_FAILED:
        default:
            return io_file_raise(interp, "io/read-file-failed", "read-file: read failed");
    }
}

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
        return io_file_raise(interp, "io/read-file-expected-path", "read-file: expected (read-file path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "read-file",
        "io/read-file-fiber-required",
        "io/read-file-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    return io_async_read_file_value(path, interp);
}

/**
 * (write-file path content) -> true on success, nil on error
 */
fn Value* prim_write_file(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_string(args[0]) || !is_string(args[1])) {
        return io_file_raise(interp, "io/write-file-expected-path-content", "write-file: expected (write-file path content)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "write-file",
        "io/write-file-fiber-required",
        "io/write-file-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    char[] content = args[1].str_chars[:args[1].str_len];

    usz payload_len = path.len + 1 + content.len;
    char* payload = (char*)mem::malloc(payload_len + 1);
    if (payload == null) return io_file_raise(interp, "io/write-file-out-of-memory", "write-file: out of memory");
    for (usz i = 0; i < path.len; i++) payload[i] = path[i];
    payload[path.len] = 0;
    for (usz i = 0; i < content.len; i++) payload[path.len + 1 + i] = content[i];
    payload[payload_len] = 0;

    OffloadWork work = {};
    work.op = OFFLOAD_WRITE_FILE;
    work.shared = scheduler_domain_publish_shared_owned(scheduler_current_domain(), payload, payload_len);
    if (work.shared == null) return io_file_raise(interp, "io/write-file-out-of-memory", "write-file: out of memory");

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (is_int(out) && out.int_val >= 0) return make_symbol(interp, interp.sym_true);
    return io_file_raise(interp, "io/write-file-failed", "write-file: write failed");
}

/**
 * (file-exists? path) -> true if file exists, nil otherwise
 */
fn Value* prim_file_exists(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_file_raise(interp, "io/file-exists?-expected-path", "file-exists?: expected (file-exists? path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "file-exists?",
        "io/file-exists?-fiber-required",
        "io/file-exists?-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];

    OffloadWork work = {};
    work.op = OFFLOAD_FILE_EXISTS;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), path);
    if (work.shared == null) return io_file_raise(interp, "io/file-exists?-out-of-memory", "file-exists?: out of memory");

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (is_int(out) && out.int_val != 0) return make_symbol(interp, interp.sym_true);
    if (is_int(out) && out.int_val == 0) return make_nil(interp);
    if (out == null || is_nil(out)) return make_nil(interp);
    return io_file_raise(interp, "io/file-exists?-failed", "file-exists?: existence probe failed");
}

/**
 * (read-lines path) -> list of strings (one per line)
 */
fn Value* prim_read_lines(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_file_raise(interp, "io/read-lines-expected-path", "read-lines: expected (read-lines path)");
    }

    Value* runtime_err = io_file_require_async_runtime(
        interp,
        "read-lines",
        "io/read-lines-fiber-required",
        "io/read-lines-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    char[] path = args[0].str_chars[:args[0].str_len];
    IoReadFileResult read_result = io_async_read_file_result(path, interp);
    if (read_result.status == IO_READ_FILE_OUT_OF_MEMORY) {
        return io_file_raise(interp, "io/read-lines-out-of-memory", "read-lines: out of memory");
    }
    if (read_result.status == IO_READ_FILE_RUNTIME_ERROR) return read_result.value;
    if (read_result.status != IO_READ_FILE_OK || !is_string(read_result.value)) {
        return io_file_raise(interp, "io/read-lines-failed", "read-lines: read failed");
    }

    Value* content_v = read_result.value;
    char[] content = content_v.str_chars[:content_v.str_len];
    usz content_len = content.len;

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
