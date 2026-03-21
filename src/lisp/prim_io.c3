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
