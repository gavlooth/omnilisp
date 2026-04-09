module main;

import std::io;
import libc;

usz g_aot_temp_seq = 0;

fn bool aot_temp_append_char(char[] buf, usz* n, char c) @inline {
    if (*n >= buf.len - 1) return false;
    buf[*n] = c;
    *n += 1;
    return true;
}

fn bool aot_temp_append_str(char[] buf, usz* n, char[] src) {
    for (usz i = 0; i < src.len; i++) {
        if (!aot_temp_append_char(buf, n, src[i])) return false;
    }
    return true;
}

fn bool aot_temp_append_u64_dec(char[] buf, usz* n, usz value) {
    char[32] tmp;
    usz tmp_len = 0;
    if (value == 0) return aot_temp_append_char(buf, n, '0');

    while (value > 0 && tmp_len < tmp.len) {
        tmp[tmp_len++] = (char)('0' + (value % 10));
        value /= 10;
    }
    if (value > 0) return false;

    for (usz i = 0; i < tmp_len; i++) {
        if (!aot_temp_append_char(buf, n, tmp[tmp_len - i - 1])) return false;
    }
    return true;
}

fn bool make_aot_temp_path(char[] temp_path_buf, usz* path_len_out) {
    usz n = 0;
    if (!aot_temp_append_str(temp_path_buf, &n, "build/_aot_temp_")) return false;
    if (!aot_temp_append_u64_dec(temp_path_buf, &n, (usz)c_getpid())) return false;
    if (!aot_temp_append_char(temp_path_buf, &n, '_')) return false;
    if (!aot_temp_append_u64_dec(temp_path_buf, &n, g_aot_temp_seq)) return false;
    if (!aot_temp_append_str(temp_path_buf, &n, ".c3")) return false;
    temp_path_buf[n] = 0;
    *path_len_out = n;
    g_aot_temp_seq++;
    return true;
}

fn bool write_aot_temp_file(char[] c3_code, char[] temp_path_buf, usz* path_len_out) {
    for (usz attempt = 0; attempt < 64; attempt++) {
        usz path_len = 0;
        if (!make_aot_temp_path(temp_path_buf, &path_len)) break;
        if (try file = io::file::open((String)temp_path_buf[:path_len], "wx")) {
            char[] remaining = c3_code;
            bool write_ok = true;
            EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            while (write_ok && remaining.len > 0) {
                usz? written_opt = file.write(remaining);
                if (catch err = written_opt) {
                    write_kind = classify_file_write_errno();
                    write_ok = false;
                    break;
                }
                if (written_opt == 0) {
                    write_ok = false;
                    break;
                }
                remaining = remaining[written_opt..];
            }
            if (catch err = file.close()) {
                write_kind = classify_file_write_errno();
                write_ok = false;
            }
            if (!write_ok) {
                print_file_write_error_text("AOT temp file", temp_path_buf[:path_len], write_kind);
                return false;
            }
            *path_len_out = path_len;
            return true;
        }
        if (libc::errno() != EEXIST) {
            print_file_write_error_text("AOT temp file", temp_path_buf[:path_len], classify_file_write_errno());
            return false;
        }
    }
    io::printn("Error: cannot write unique AOT temp file in build/");
    return false;
}
