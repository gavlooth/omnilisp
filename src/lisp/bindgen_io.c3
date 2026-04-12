module lisp;

import std::io;

extern fn int bindgen_getpid() @extern("getpid");

fn bool bindgen_copy_output_path_cstr(char[] output_path, char[] path_buf, char[]* out_path) {
    if (out_path != null) *out_path = "";
    if (output_path.len == 0 || output_path.len >= path_buf.len) return false;
    for (usz i = 0; i < output_path.len; i++) {
        path_buf[i] = output_path[i];
    }
    path_buf[output_path.len] = 0;
    if (out_path != null) *out_path = path_buf[:output_path.len];
    return true;
}

fn bool bindgen_build_atomic_temp_path(char[] output_path, char[] temp_buf, char[]* out_temp_path) {
    if (out_temp_path != null) *out_temp_path = "";

    char[32] suffix_buf;
    char[] suffix = "";
    if (try rendered = io::bprintf(&suffix_buf, ".tmp.%d", bindgen_getpid())) {
        suffix = rendered;
    } else {
        return false;
    }

    if (output_path.len == 0 || output_path.len > temp_buf.len || suffix.len > temp_buf.len - output_path.len) {
        return false;
    }
    usz temp_len = output_path.len + suffix.len;
    if (temp_len >= temp_buf.len) return false;
    for (usz i = 0; i < output_path.len; i++) {
        temp_buf[i] = output_path[i];
    }
    for (usz i = 0; i < suffix.len; i++) {
        temp_buf[output_path.len + i] = suffix[i];
    }
    temp_buf[temp_len] = 0;
    if (out_temp_path != null) *out_temp_path = temp_buf[:temp_len];
    return true;
}

fn bool bindgen_write_text_file(char[] output_path, char[] text) {
    char[640] final_path_buf;
    char[704] temp_path_buf;
    char[] final_path = "";
    char[] temp_path = "";
    if (!bindgen_copy_output_path_cstr(output_path, final_path_buf[..], &final_path)) return false;
    if (!bindgen_build_atomic_temp_path(final_path, temp_path_buf[..], &temp_path)) return false;

    (void)omni_uv_fs_unlink((ZString)temp_path.ptr);

    bool committed = false;
    if (try file = io::file::open((String)temp_path, "w")) {
        defer if (!committed) (void)omni_uv_fs_unlink((ZString)temp_path.ptr);
        char[] remaining = text;
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        if (!write_ok) return false;
        if (omni_uv_fs_rename((ZString)temp_path.ptr, (ZString)final_path.ptr) != 0) return false;
        committed = true;
        return true;
    }
    return false;
}

fn bool bindgen_output_file_exists(char[] output_path) {
    if (try file = io::file::open((String)output_path, "r")) {
        (void)file.close();
        return true;
    }
    return false;
}

fn bool bindgen_delete_output_file_if_exists(char[] output_path) {
    if (bindgen_output_file_exists(output_path)) {
        if (catch err = io::file::delete((String)output_path)) return false;
    }
    return true;
}
