module lisp;

import std::core::mem;
import std::io;
import main;

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
        if (try char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)) {
            return io_file_raise(interp, fiber_code, msg);
        }
        return io_file_raise(interp, fiber_code, "async file op: requires running fiber context");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
            return io_file_raise(interp, loop_code, msg);
        }
        return io_file_raise(interp, loop_code, "async file op: libuv loop unavailable");
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
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_offload(fiber_id);
        return y;
    }

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

    if (c_api_string_contains_nul(path)) {
        result.status = IO_READ_FILE_RUNTIME_ERROR;
        result.value = io_file_raise(interp, "io/read-file-invalid-path", "read-file: path contains embedded NUL byte");
        return result;
    }

    OffloadWork work = {};
    work.op = OFFLOAD_READ_FILE;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), path);
    if (work.shared == null) {
        result.status = IO_READ_FILE_OUT_OF_MEMORY;
        return result;
    }

    Value* out = io_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) {
        interp.flags.raise_pending = false;
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        interp.raise_msg[0] = 0;
        result.status = IO_READ_FILE_FAILED;
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
    if (c_api_string_contains_nul(path)) return false;

    int fd = omni_uv_fs_open((ZString)path.ptr, 0, 0);
    if (fd < 0) return false;

    ulong size_out = 0;
    uint mode_out = 0;
    int stat_status = omni_uv_fs_stat_basic((ZString)path.ptr, &size_out, &mode_out);
    if (stat_status < 0) {
        (void)omni_uv_fs_close(fd);
        return false;
    }

    if (size_out > (ulong)(usz.max - (usz)1)) {
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
    int close_status = omni_uv_fs_close(fd);
    if (close_status < 0) {
        mem::free(buf);
        return false;
    }
    buf[total] = 0;

    *out_buf = buf;
    *out_len = total;
    return true;
}
