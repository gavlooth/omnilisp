module main;

import std::io;
import std::io::path;

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

    usz dir_len = n;
    while (dir_len > 0 && repo_root_buf[dir_len - 1] != '/') dir_len--;
    if (dir_len == 0) return false;
    if (dir_len > 1) dir_len--;
    repo_root_buf[dir_len] = 0;

    if (path_ends_with(repo_root_buf[..dir_len], "/build")) {
        if (!path_move_to_parent(repo_root_buf, &dir_len)) return false;
    }

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
    if (last_dot > last_slash + 1) out_len = (usz)last_dot;
    if (out_len > default_out.len - 1) return null;
    for (usz i = 0; i < out_len; i++) default_out[i] = input_file[i];
    default_out[out_len] = 0;
    return &default_out[0];
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
