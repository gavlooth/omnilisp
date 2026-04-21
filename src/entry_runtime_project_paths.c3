module main;

extern fn int repl_access(char* path, int mode) @extern("access");
extern fn char* repl_getcwd(char* buf, usz size) @extern("getcwd");

const int REPL_PROJECT_PATH_EXISTS = 0;
const usz REPL_PROJECT_PATH_BUFFER_CAP = 4096;

fn bool repl_cstr_len_bounded(char* text, usz max_len, usz* out_len) @inline {
    *out_len = 0;
    if (text == null) return true;
    for (usz len = 0; len < max_len; len++) {
        if (text[len] == 0) {
            *out_len = len;
            return true;
        }
    }
    return false;
}

fn bool repl_copy_project_root(char* project_dir, char[] root_buf, usz* root_len_out) {
    if (project_dir == null) {
        char* cwd = repl_getcwd(&root_buf[0], root_buf.len);
        if (cwd == null) return false;
        usz root_len = 0;
        if (!repl_cstr_len_bounded(cwd, root_buf.len, &root_len)) return false;
        while (root_len > 1 && root_buf[root_len - 1] == '/') {
            root_len--;
        }
        root_buf[root_len] = 0;
        *root_len_out = root_len;
        return true;
    }

    usz root_len = 0;
    if (!repl_cstr_len_bounded(project_dir, root_buf.len, &root_len)) return false;
    if (root_len >= root_buf.len) return false;
    for (usz i = 0; i < root_len; i++) {
        root_buf[i] = project_dir[i];
    }
    while (root_len > 1 && root_buf[root_len - 1] == '/') {
        root_len--;
    }
    root_buf[root_len] = 0;
    *root_len_out = root_len;
    return true;
}

fn bool repl_build_project_path(char[] path_buf, usz* path_len_out, char* root, usz root_len, char[] suffix) {
    usz pos = 0;
    for (usz i = 0; i < root_len; i++) {
        if (pos >= path_buf.len - 1) return false;
        path_buf[pos++] = root[i];
    }
    if (root_len > 0 && root[root_len - 1] != '/') {
        if (pos >= path_buf.len - 1) return false;
        path_buf[pos++] = '/';
    }
    for (usz i = 0; i < suffix.len; i++) {
        if (pos >= path_buf.len - 1) return false;
        path_buf[pos++] = suffix[i];
    }
    path_buf[pos] = 0;
    *path_len_out = pos;
    return true;
}

fn bool repl_resolve_project_main_path(char* project_dir, char[] main_path_buf, usz* main_path_len_out) {
    char[REPL_PROJECT_PATH_BUFFER_CAP] root_buf = {};
    usz root_len = 0;
    if (!repl_copy_project_root(project_dir, root_buf[..], &root_len)) {
        return false;
    }

    char[REPL_PROJECT_PATH_BUFFER_CAP] toml_path_buf = {};
    usz toml_path_len = 0;
    if (!repl_build_project_path(toml_path_buf[..], &toml_path_len, &root_buf[0], root_len, "omni.toml")) {
        return false;
    }
    if (repl_access(&toml_path_buf[0], REPL_PROJECT_PATH_EXISTS) != 0) {
        return false;
    }

    return repl_build_project_path(main_path_buf, main_path_len_out, &root_buf[0], root_len, "src/main.omni");
}
