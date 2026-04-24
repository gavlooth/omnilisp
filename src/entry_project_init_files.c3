module main;

import std::io;
import libc;

extern fn int mkdir(char* path, uint mode) @extern("mkdir");
extern fn int access(char* path, int mode) @extern("access");
extern fn int rmdir(char* path) @extern("rmdir");

const int INIT_PATH_EXISTS = 0;

fn bool build_project_path(char[] path_buf, usz* path_len_out, char* name, usz name_len, char[] suffix = "") {
    usz plen = 0;
    for (usz i = 0; i < name_len && plen < path_buf.len - 1; i++) {
        path_buf[plen++] = name[i];
    }
    if (name_len > 0 && plen < name_len) return false;
    if (suffix.len > 0 && plen < path_buf.len - 1) {
        path_buf[plen++] = '/';
        for (usz i = 0; i < suffix.len && plen < path_buf.len - 1; i++) {
            path_buf[plen++] = suffix[i];
        }
    }
    if (suffix.len > 0 && plen < name_len + 1 + suffix.len) return false;
    path_buf[plen] = 0;
    *path_len_out = plen;
    return true;
}

fn bool cstr_eq_chars(char* cstr, char[] expected) {
    for (usz i = 0; i < expected.len; i++) {
        if (cstr[i] != expected[i]) return false;
    }
    return cstr[expected.len] == 0;
}

fn bool path_has_suffix(char[] path, char[] suffix) {
    if (suffix.len > path.len) return false;
    usz start = path.len - suffix.len;
    for (usz i = 0; i < suffix.len; i++) {
        if (path[start + i] != suffix[i]) return false;
    }
    return true;
}

fn bool init_project_name_is_valid(char[] name) {
    if (name.len == 0) return false;
    if (name.len == 1 && name[0] == '.') return false;
    if (name.len == 2 && name[0] == '.' && name[1] == '.') return false;
    if (name[0] == '/' || name[0] == '\\') return false;

    for (usz i = 0; i < name.len; i++) {
        uint byte = (uint)name[i] & 0xFF;
        if (byte < 0x20 || byte == 0x7F) return false;
        if (name[i] == '/' || name[i] == '\\') return false;
    }
    return true;
}

fn int print_init_invalid_project_name() {
    io::printn("Error: invalid project name for --init: use a single directory name without path separators, '.' or '..'");
    return 1;
}

fn void maybe_force_init_test_file_collision(char* path_cstr, char[] suffix) {
    if (suffix.len == 0) return;
    char* forced_suffix = c_getenv("OMNI_INIT_TEST_FILE_COLLISION_SUFFIX");
    if (forced_suffix == null || !cstr_eq_chars(forced_suffix, suffix)) return;
    if (io::file::exists(((ZString)path_cstr).str_view())) return;
    if (try file = io::file::open(((ZString)path_cstr).str_view(), "w")) {
        (void)file.close();
    }
}

fn bool init_should_force_write_failure(char[] path) {
    char* forced_suffix = c_getenv("OMNI_INIT_TEST_FAIL_WRITE_SUFFIX");
    if (forced_suffix == null) return false;
    char[] suffix;
    if (!cli_cstr_slice(forced_suffix, &suffix)) {
        (void)print_cli_cstr_too_long("OMNI_INIT_TEST_FAIL_WRITE_SUFFIX");
        return false;
    }
    return path_has_suffix(path, suffix);
}

fn void cleanup_project_file_path(char* name, usz name_len, char[] suffix) {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, suffix)) return;
    (void)plen;
    (void)libc::remove((ZString)&path_buf[0]);
}

fn void cleanup_project_dir_path(char* name, usz name_len, char[] suffix = "") {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, suffix)) return;
    (void)plen;
    (void)libc::remove((ZString)&path_buf[0]);
    (void)rmdir(&path_buf[0]);
}

fn void cleanup_init_project(char* name, usz name_len) {
    cleanup_project_file_path(name, name_len, "build/project.json");
    cleanup_project_file_path(name, name_len, "src/main.omni");
    cleanup_project_file_path(name, name_len, "omni.toml");
    cleanup_project_dir_path(name, name_len, "lib/ffi");
    cleanup_project_dir_path(name, name_len, "lib");
    cleanup_project_dir_path(name, name_len, "include");
    cleanup_project_dir_path(name, name_len, "build");
    cleanup_project_dir_path(name, name_len, "src");
    cleanup_project_dir_path(name, name_len);
}

fn bool mkdir_project_path(char* name, usz name_len, char[] suffix = "") {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, suffix)) {
        io::printn("Error: project path too long during --init");
        return false;
    }
    (void)plen;
    maybe_force_init_test_file_collision(&path_buf[0], suffix);
    if (mkdir(&path_buf[0], 0o755) == 0) return true;
    if (libc::errno() == EEXIST) {
        if (io::file::is_dir(((ZString)&path_buf[0]).str_view())) return true;
        io::printfn("Error: cannot create directory %s: non-directory already exists", (ZString)&path_buf[0]);
        return false;
    }
    io::printfn("Error: failed to create %s", (ZString)&path_buf[0]);
    return false;
}

fn bool init_project_root_is_fresh(char* name, usz name_len) {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len)) {
        io::printn("Error: project path too long during --init");
        return false;
    }
    (void)plen;
    if (access(&path_buf[0], INIT_PATH_EXISTS) == 0) {
        io::printfn("Error: project already exists: %s", (ZString)&path_buf[0]);
        return false;
    }
    return true;
}

fn bool write_project_text_file(char[] path, char[] text) {
    if (init_should_force_write_failure(path)) return false;
    EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    return entry_write_text_atomic(path, text, &write_kind);
}
