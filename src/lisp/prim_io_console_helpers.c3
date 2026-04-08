module lisp;

import std::core::mem;
import main;

const int IO_STDOUT_FD = 1;
const int IO_STDERR_FD = 2;
const usz IO_CONSOLE_RENDER_INITIAL_CAP = 256;
const usz IO_CONSOLE_RENDER_MAX_CAP = 16 * 1024 * 1024;

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
