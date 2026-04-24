module lisp;

import std::io;
import std::core::mem;
import main;

// libuv filesystem helpers (csrc/uv_helpers.c)
extern fn int omni_uv_fs_open(char* path, int flags, int mode) @extern("omni_uv_fs_open");
extern fn int omni_uv_fs_close(int fd) @extern("omni_uv_fs_close");
extern fn long omni_uv_fs_read(int fd, char* buf, usz len) @extern("omni_uv_fs_read");
extern fn long omni_uv_fs_write(int fd, char* buf, usz len) @extern("omni_uv_fs_write");
extern fn usz omni_uv_fs_max_buf_len() @extern("omni_uv_fs_max_buf_len");
extern fn void omni_uv_fs_debug_set_max_buf_len(usz len) @extern("omni_uv_fs_debug_set_max_buf_len");
extern fn int omni_uv_fs_rename(char* src, char* dst) @extern("omni_uv_fs_rename");
extern fn int omni_uv_fs_unlink(char* path) @extern("omni_uv_fs_unlink");
extern fn int omni_uv_fs_stat_basic(char* path, ulong* size_out, uint* mode_out) @extern("omni_uv_fs_stat_basic");
extern fn int omni_uv_fs_scandir_join(char* path, char** out_buf, usz* out_len) @extern("omni_uv_fs_scandir_join");
extern fn void omni_uv_free(void* ptr) @extern("free");

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
        if (state.buf != null) {
            mem::free(state.buf);
            state.buf = null;
        }
        state.len = 0;
        state.cap = 0;
        state.mu.unlock();
    } else {
        if (state.buf != null) {
            mem::free(state.buf);
            state.buf = null;
        }
        state.len = 0;
        state.cap = 0;
        state.closed = true;
    }
    if (state.cv_ready) {
        (void)state.cv.destroy();
        state.cv_ready = false;
    }
    if (state.mu_ready) {
        (void)state.mu.destroy();
        state.mu_ready = false;
    }
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
    if (state.len > usz.max - text.len) return false;
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
