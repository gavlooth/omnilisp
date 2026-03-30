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
const int IO_STDERR_FD = 2;
const usz IO_CONSOLE_RENDER_INITIAL_CAP = 256;
const usz IO_CONSOLE_RENDER_MAX_CAP = 16 * 1024 * 1024;
const usz IO_INPUT_INITIAL_CAP = 256;
const usz IO_INPUT_MAX_CAP = 16 * 1024 * 1024;
const usz IO_READ_LINE_BUFFER_MAX = 65536;

enum IoInputReadStatus : char {
    IO_INPUT_READ_OK,
    IO_INPUT_READ_EOF,
    IO_INPUT_READ_TRUNCATED,
    IO_INPUT_READ_FAILED,
    IO_INPUT_READ_INTERRUPTED,
}

struct IoInputReadResult {
    usz len;
    IoInputReadStatus status;
}

fn bool io_console_write_all(int fd, char[] text) {
    usz offset = 0;
    while (offset < text.len) {
        long wrote_n = omni_uv_fs_write(fd, text.ptr + offset, text.len - offset);
        if (wrote_n <= 0) return false;
        offset += (usz)wrote_n;
    }
    return true;
}

fn int io_console_fd(Interp* interp) @inline {
    return interp.flags.console_to_stderr ? IO_STDERR_FD : IO_STDOUT_FD;
}

fn bool io_console_capture_append(Interp* interp, char[] text) {
    if (text.len == 0) return true;

    usz needed = interp.console_capture_len + text.len + 1;
    if (needed > interp.console_capture_cap) {
        usz next_cap = interp.console_capture_cap > 0 ? interp.console_capture_cap : IO_CONSOLE_RENDER_INITIAL_CAP;
        while (next_cap < needed) {
            if (next_cap >= IO_CONSOLE_RENDER_MAX_CAP) {
                next_cap = IO_CONSOLE_RENDER_MAX_CAP;
                break;
            }
            usz grown = next_cap * 2;
            if (grown <= next_cap) {
                next_cap = IO_CONSOLE_RENDER_MAX_CAP;
                break;
            }
            next_cap = grown;
        }
        if (next_cap < needed) return false;

        char* next = interp.console_capture_buf == null
            ? (char*)mem::malloc(next_cap)
            : (char*)mem::realloc(interp.console_capture_buf, next_cap);
        if (next == null) return false;
        interp.console_capture_buf = next;
        interp.console_capture_cap = next_cap;
    }

    mem::copy(interp.console_capture_buf + interp.console_capture_len, text.ptr, text.len);
    interp.console_capture_len += text.len;
    interp.console_capture_buf[interp.console_capture_len] = 0;
    return true;
}

fn bool io_console_write_text(Interp* interp, char[] text) {
    if (interp.console_capture_active) {
        return io_console_capture_append(interp, text);
    }
    return io_console_write_all(io_console_fd(interp), text);
}

fn void io_input_state_init(InterpInputState* state) {
    if (state == null) return;
    *state = {};
    if (catch err = state.mu.init()) {
        state.mu_ready = false;
    } else {
        state.mu_ready = true;
    }
    if (catch err = state.cv.init()) {
        state.cv_ready = false;
    } else {
        state.cv_ready = true;
    }
}

fn void io_input_state_destroy(InterpInputState* state) {
    if (state == null) return;
    if (state.mu_ready) {
        state.mu.lock();
        state.closed = true;
        if (state.cv_ready) (void)state.cv.broadcast();
        state.mu.unlock();
    }
    if (state.buf != null) mem::free(state.buf);
    *state = {};
}

fn void io_input_state_wake(InterpInputState* state) {
    if (state == null || !state.mu_ready || !state.cv_ready) return;
    state.mu.lock();
    (void)state.cv.broadcast();
    state.mu.unlock();
}

fn bool io_input_state_ensure_capacity_locked(InterpInputState* state, usz needed) {
    if (state == null) return false;
    if (needed <= state.cap) return true;

    usz next_cap = state.cap > 0 ? state.cap : IO_INPUT_INITIAL_CAP;
    while (next_cap < needed) {
        if (next_cap >= IO_INPUT_MAX_CAP) {
            next_cap = IO_INPUT_MAX_CAP;
            break;
        }
        usz grown = next_cap * 2;
        if (grown <= next_cap) {
            next_cap = IO_INPUT_MAX_CAP;
            break;
        }
        next_cap = grown;
    }
    if (next_cap < needed) return false;

    char* next = state.buf == null
        ? (char*)mem::malloc(next_cap)
        : (char*)mem::realloc(state.buf, next_cap);
    if (next == null) return false;
    state.buf = next;
    state.cap = next_cap;
    return true;
}

fn bool io_input_state_append(InterpInputState* state, char[] text) {
    if (state == null || !state.mu_ready || !state.cv_ready) return false;
    if (text.len == 0) return true;

    state.mu.lock();
    defer state.mu.unlock();

    if (state.closed) return false;
    usz needed = state.len + text.len;
    if (!io_input_state_ensure_capacity_locked(state, needed)) return false;
    mem::copy(state.buf + state.len, text.ptr, text.len);
    state.len += text.len;
    (void)state.cv.broadcast();
    return true;
}

fn bool io_input_state_close(InterpInputState* state) {
    if (state == null || !state.mu_ready || !state.cv_ready) return false;
    state.mu.lock();
    state.closed = true;
    (void)state.cv.broadcast();
    state.mu.unlock();
    return true;
}

fn usz io_input_state_find_newline_locked(InterpInputState* state) @inline {
    if (state == null || state.buf == null) return usz.max;
    for (usz i = 0; i < state.len; i++) {
        if (state.buf[i] == '\n') return i;
    }
    return usz.max;
}

fn void io_input_state_consume_locked(InterpInputState* state, usz consume_len) {
    if (state == null || consume_len == 0) return;
    if (consume_len >= state.len) {
        state.len = 0;
        return;
    }
    usz remain = state.len - consume_len;
    for (usz i = 0; i < remain; i++) {
        state.buf[i] = state.buf[consume_len + i];
    }
    state.len = remain;
}

fn IoInputReadResult io_input_state_read_line(InterpInputState* state, char[] buffer, Interp* interp) {
    IoInputReadResult out = { .len = 0, .status = IO_INPUT_READ_FAILED };
    if (state == null || !state.mu_ready || !state.cv_ready || buffer.len == 0) return out;

    usz max_len = buffer.len - 1;
    state.mu.lock();
    defer state.mu.unlock();

    while (true) {
        if (interp_interrupt_requested(interp)) {
            interp_interrupt_clear(interp);
            buffer[0] = 0;
            out.status = IO_INPUT_READ_INTERRUPTED;
            return out;
        }

        usz newline_idx = io_input_state_find_newline_locked(state);
        if (newline_idx != usz.max) {
            usz copy_len = newline_idx < max_len ? newline_idx : max_len;
            if (copy_len > 0) mem::copy(buffer.ptr, state.buf, copy_len);
            buffer[copy_len] = 0;
            io_input_state_consume_locked(state, newline_idx + 1);
            out.len = copy_len;
            out.status = newline_idx > max_len ? IO_INPUT_READ_TRUNCATED : IO_INPUT_READ_OK;
            return out;
        }

        if (state.closed) {
            if (state.len == 0) {
                buffer[0] = 0;
                out.status = IO_INPUT_READ_EOF;
                return out;
            }
            usz copy_len = state.len < max_len ? state.len : max_len;
            if (copy_len > 0) mem::copy(buffer.ptr, state.buf, copy_len);
            buffer[copy_len] = 0;
            bool truncated = state.len > max_len;
            io_input_state_consume_locked(state, state.len);
            out.len = copy_len;
            out.status = truncated ? IO_INPUT_READ_TRUNCATED : IO_INPUT_READ_OK;
            return out;
        }

        (void)state.cv.wait(&state.mu);
    }
}

fn IoInputReadResult io_runtime_read_line(char[] buffer, Interp* interp) {
    if (interp != null && interp.input_state != null) {
        return io_input_state_read_line(interp.input_state, buffer, interp);
    }

    ReadLineResult read = read_line(buffer);
    if (read.truncated) return { .len = read.len, .status = IO_INPUT_READ_TRUNCATED };
    if (read.eof && read.len == 0) return { .len = 0, .status = IO_INPUT_READ_EOF };
    return { .len = read.len, .status = IO_INPUT_READ_OK };
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
            (void)io_console_write_text(interp, rendered[:rendered_len]);
        }
    }
    if (append_newline) {
        char[1] nl;
        nl[0] = '\n';
        (void)io_console_write_text(interp, nl[..]);
    }
    return make_void(interp);
}

fn Value* prim_print(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, false, interp);
    }
    return make_void(interp);
}

fn Value* prim_println(Value*[] args, Env* env, Interp* interp) {
    if (args.len >= 1) {
        return io_console_emit(args[0], false, true, interp);
    }
    return make_void(interp);
}

fn Value* prim_newline(Value*[] args, Env* env, Interp* interp) {
    return io_console_emit(null, false, true, interp);
}

fn Value* prim_display(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "display: expected argument");
    return io_console_emit(args[0], true, false, interp);
}

fn Value* prim_read_line(Value*[] args, Env* env, Interp* interp) {
    if (args.len > 1 || (args.len == 1 && !is_nil(args[0]))) {
        return io_raise(interp, "io/read-line-expected-no-args", "read-line: expected (read-line)");
    }

    char[IO_READ_LINE_BUFFER_MAX] line_buf = {};
    IoInputReadResult read = io_runtime_read_line(line_buf[..], interp);
    switch (read.status) {
        case IO_INPUT_READ_OK:
            return make_string(interp, line_buf[:read.len]);
        case IO_INPUT_READ_EOF:
            return make_nil(interp);
        case IO_INPUT_READ_TRUNCATED:
            return io_raise(interp, "io/read-line-too-long", "read-line: input line exceeds supported length");
        case IO_INPUT_READ_INTERRUPTED:
            return interp_interrupt_error(interp);
        default:
            return io_raise(interp, "io/read-line-failed", "read-line: failed to read input");
    }
}
