module main;

import std::io;
import std::io::path;
import libc;

extern fn int c_getpid() @extern("getpid");
extern fn isz c_readlink(char* path, char* buf, usz size) @extern("readlink");
extern fn int c_access(char* path, int mode) @extern("access");

const int BUILD_ACCESS_EXEC = 1;
const int BUILD_ACCESS_WRITE = 2;

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

fn char* find_build_output_arg(int argc, char** argv, int build_idx, bool* missing_value = null) {
    if (missing_value != null) *missing_value = false;
    for (int i = build_idx + 2; i < argc; i++) {
        if (str_eq(argv[i], "-o")) {
            if (i + 1 < argc) {
                char* output_arg = argv[i + 1];
                if (cli_arg_starts_with_dash(output_arg)) {
                    if (missing_value != null) *missing_value = true;
                    return null;
                }
                return output_arg;
            }
            if (missing_value != null) *missing_value = true;
            return null;
        }
    }
    return null;
}

fn bool path_ends_with(char[] path_str, char[] suffix) @inline {
    if (suffix.len > path_str.len) return false;
    usz start = path_str.len - suffix.len;
    for (usz i = 0; i < suffix.len; i++) {
        if (path_str[start + i] != suffix[i]) return false;
    }
    return true;
}

fn bool repo_root_has_markers(char[] root_path) {
    if (root_path.len == 0) return false;
    char[4096] helper_buf;
    char[] helper_path;
    if (try hp = io::bprintf(&helper_buf, "%s/scripts/build_omni_chelpers.sh", (ZString)&root_path[0])) {
        helper_path = hp;
    } else {
        return false;
    }
    if (catch err = io::file::open((String)helper_path, "r")) {
        return false;
    } else if (try helper_file = io::file::open((String)helper_path, "r")) {
        defer (void)helper_file.close();
    }

    char[4096] main_src_buf;
    char[] main_src_path;
    if (try mp = io::bprintf(&main_src_buf, "%s/src/main.c3", (ZString)&root_path[0])) {
        main_src_path = mp;
    } else {
        return false;
    }
    if (catch err = io::file::open((String)main_src_path, "r")) {
        return false;
    } else if (try main_file = io::file::open((String)main_src_path, "r")) {
        defer (void)main_file.close();
    }
    return true;
}

fn bool path_move_to_parent(char[] repo_root_buf, usz* len_io) {
    usz before = *len_io;
    usz len = before;
    if (len == 0) return false;

    while (len > 1 && repo_root_buf[len - 1] == '/') len--;

    usz slash = len;
    while (slash > 0 && repo_root_buf[slash - 1] != '/') slash--;
    if (slash == 0) return false;
    if (slash > 1) slash--;
    if (slash == before) return false;

    repo_root_buf[slash] = 0;
    *len_io = slash;
    return true;
}

fn bool resolve_build_repo_root(char* argv0, char[] repo_root_buf, usz* repo_root_len_out) {
    char* env_root = c_getenv("OMNI_REPO_ROOT");
    if (env_root != null && env_root[0] != 0) {
        usz env_len = cstr_len(env_root);
        if (env_len > 0 && env_len < repo_root_buf.len) {
            for (usz i = 0; i < env_len; i++) repo_root_buf[i] = env_root[i];
            repo_root_buf[env_len] = 0;
            if (repo_root_has_markers(repo_root_buf[..env_len])) {
                if (repo_root_len_out != null) *repo_root_len_out = env_len;
                return true;
            }
        }
    }

    usz n = 0;
    isz link_len = c_readlink("/proc/self/exe", &repo_root_buf[0], repo_root_buf.len - 1);
    if (link_len > 0 && (usz)link_len < repo_root_buf.len) {
        n = (usz)link_len;
        repo_root_buf[n] = 0;
    } else {
        if (argv0 == null || argv0[0] == 0) return false;
        n = cstr_len(argv0);
        if (n == 0 || n >= repo_root_buf.len) return false;
        for (usz i = 0; i < n; i++) repo_root_buf[i] = argv0[i];
        repo_root_buf[n] = 0;
    }

    // Drop executable filename component.
    usz dir_len = n;
    while (dir_len > 0 && repo_root_buf[dir_len - 1] != '/') dir_len--;
    if (dir_len == 0) return false;
    if (dir_len > 1) dir_len--;
    repo_root_buf[dir_len] = 0;

    // Start from parent if executable lives in <repo>/build/.
    if (path_ends_with(repo_root_buf[..dir_len], "/build")) {
        if (!path_move_to_parent(repo_root_buf, &dir_len)) return false;
    }

    // Validate expected repo markers; if not present, walk up parents.
    // This avoids chdir into an unrelated tree when argv0/proc paths are odd.
    usz walk_len = dir_len;
    while (walk_len > 0) {
        if (repo_root_has_markers(repo_root_buf[..walk_len])) {
            if (repo_root_len_out != null) *repo_root_len_out = walk_len;
            return true;
        }
        if (!path_move_to_parent(repo_root_buf, &walk_len)) break;
    }

    return false;
}

fn char* derive_default_output_name(char* input_file, char[] default_out) {
    usz input_len = cstr_len(input_file);
    isz last_dot = -1;
    isz last_slash = -1;
    for (usz i = 0; i < input_len; i++) {
        if (input_file[i] == '/') {
            last_slash = (isz)i;
            continue;
        }
        if (input_file[i] == '.') last_dot = (isz)i;
    }
    usz out_len = input_len;
    // Only trim extension dots in the basename segment.
    if (last_dot > last_slash + 1) out_len = (usz)last_dot;
    if (out_len > default_out.len - 1) return null;
    for (usz i = 0; i < out_len; i++) default_out[i] = input_file[i];
    default_out[out_len] = 0;
    return &default_out[0];
}

fn bool load_build_source(char* input_file, char[]* source_out, EntryFileReadKind* kind_out = null) {
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try src = io::file::load_temp((String)input_path)) {
        *source_out = src;
        if (kind_out != null) *kind_out = ENTRY_FILE_READ_OK;
        return true;
    }
    if (kind_out != null && !classify_file_read_path(input_path, kind_out)) {
        return false;
    }
    if (kind_out != null) *kind_out = ENTRY_FILE_READ_GENERIC_FAILURE;
    return false;
}

fn bool validate_output_target_path(char* output_path_cstr, EntryFileWriteKind* kind_out = null) {
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    if (output_path_cstr == null || output_path_cstr[0] == 0) return false;

    char[] output_path = output_path_cstr[:cstr_len(output_path_cstr)];
    if (output_path.len == 0) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }

    if (try out_path = path::temp((String)output_path)) {
        if (path::is_dir(out_path)) {
            if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
            return false;
        }
    }

    while (output_path.len > 1 && output_path[output_path.len - 1] == '/') {
        output_path = output_path[..output_path.len - 1];
    }
    if (output_path.len == 0 || output_path[output_path.len - 1] == '/') {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }

    isz last_slash = -1;
    for (usz i = 0; i < output_path.len; i++) {
        if (output_path[i] == '/') last_slash = (isz)i;
    }

    char[] parent_dir = ".";
    if (last_slash == 0) {
        parent_dir = "/";
    } else if (last_slash > 0) {
        parent_dir = output_path[..(usz)last_slash];
    }

    if (!io::file::is_dir((String)parent_dir)) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }

    char[1024] parent_buf = {};
    if (parent_dir.len >= parent_buf.len) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }
    for (usz i = 0; i < parent_dir.len; i++) parent_buf[i] = parent_dir[i];
    parent_buf[parent_dir.len] = 0;

    if (c_access(&parent_buf[0], BUILD_ACCESS_WRITE | BUILD_ACCESS_EXEC) != 0) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PERMISSION_DENIED;
        return false;
    }

    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_OK;
    return true;
}

fn void parse_build_print_flags(int argc, char** argv, bool* print_last, bool* print_all) {
    *print_last = false;
    *print_all = false;
    for (int i = 0; i < argc; i++) {
        if (str_eq(argv[i], "--print-last")) *print_last = true;
        if (str_eq(argv[i], "--print-all")) *print_all = true;
    }
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
        if (libc::errno() != errno::EEXIST) {
            print_file_write_error_text("AOT temp file", temp_path_buf[:path_len], classify_file_write_errno());
            return false;
        }
    }
    io::printn("Error: cannot write unique AOT temp file in build/");
    return false;
}
