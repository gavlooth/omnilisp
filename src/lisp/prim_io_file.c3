module lisp;

import std::core::mem;
import std::io;
import main;

/**
 * (read-file path) -> string contents, or raise a canonical I/O error
 */
fn Value* io_file_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, data);
}

fn Value* io_file_wrap_runtime_error(
    Interp* interp,
    Value* runtime_err,
    char[] code,
    char[] message
) {
    Value* cause_data = runtime_err;
    if (interp.flags.raise_pending) {
        if (interp.raise_payload != null) cause_data = interp.raise_payload;
        interp.flags.raise_pending = false;
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        interp.raise_msg[0] = 0;
    }
    return io_file_raise(interp, code, message, cause_data);
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
 * (write-file path content) -> Void on success
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
    if (out != null && out.tag == ERROR) {
        return io_file_wrap_runtime_error(interp, out, "io/write-file-failed", "write-file: write failed");
    }
    if (is_int(out) && out.int_val >= 0) return make_void(interp);
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

    Value* lines = make_nil(interp);
    usz start = 0;
    for (usz i = 0; i <= content_len; i++) {
        if (i == content_len || content[i] == '\n') {
            usz line_len = i - start;
            if (line_len > 0 && content[start + line_len - 1] == '\r') {
                line_len--;
            }
            Value* line = make_string(interp, content[start:line_len]);
            lines = make_cons(interp, line, lines);
            start = i + 1;
        }
    }

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

    char[] rel_path = args[0].str_chars[:args[0].str_len];
    char[512] resolved;
    usz rlen = resolve_import_path(rel_path, interp, &resolved, 512);
    char[] path = resolved[:rlen];

    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;

        if (!push_source_dir(path, interp)) {
            char[512] ebuf;
            char[] msg = io::bprintf(&ebuf, "load: failed to allocate source directory context for '%s'", (ZString)path.ptr)!!;
            return raise_error(interp, msg);
        }

        EvalResult r = run_program(source, interp);

        pop_source_dir(interp);

        if (r.error.has_error) {
            char[512] ebuf;
            char[] msg = io::bprintf(&ebuf, "load: error in '%s': %s", (ZString)path.ptr, (ZString)&r.error.message)!!;
            return raise_error(interp, msg);
        }
        return r.value;
    }
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "load: file not found '%s'", (ZString)path.ptr)!!;
    return raise_error(interp, msg);
}
