module main;

import std::core::mem;
import std::io;
import libc;
import lisp;

extern fn int repl_shared_access(char* path, int mode) @extern("access");
extern fn char* repl_shared_getcwd(char* buf, usz size) @extern("getcwd");

const int REPL_SHARED_PATH_EXISTS = 0;

enum ReplFileReadKind {
    REPL_FILE_READ_OK,
    REPL_FILE_READ_NOT_FOUND,
    REPL_FILE_READ_PERMISSION_DENIED,
    REPL_FILE_READ_PATH_FAILURE,
    REPL_FILE_READ_GENERIC_FAILURE,
}

fn usz repl_shared_cstr_len(char* text) @inline {
    usz len = 0;
    if (text == null) return 0;
    while (text[len] != 0) len++;
    return len;
}

fn void repl_json_print_hex_byte(char c) @inline {
    char[] hex = "0123456789abcdef";
    uint byte = (uint)c;
    io::printf("%c%c", hex[(usz)((byte >> 4) & 0xF)], hex[(usz)(byte & 0xF)]);
}

fn void repl_json_print_escaped_string(char[] value) {
    io::print("\"");
    foreach (c : value) {
        switch (c) {
            case '"':
                io::print("\\\"");
            case '\\':
                io::print("\\\\");
            case '\n':
                io::print("\\n");
            case '\r':
                io::print("\\r");
            case '\t':
                io::print("\\t");
            default:
                uint byte = (uint)c;
                if (byte < 0x20 || byte >= 0x7f) {
                    io::print("\\u00");
                    repl_json_print_hex_byte(c);
                } else {
                    io::printf("%c", c);
                }
        }
    }
    io::print("\"");
}

fn void repl_emit_cli_error_json(char[] code, char[] message) {
    io::print("{\"ok\":false,\"code\":");
    repl_json_print_escaped_string(code);
    io::print(",\"severity\":\"error\",\"message\":");
    repl_json_print_escaped_string(message);
    io::print("}\n");
}

fn void repl_emit_cli_usage_json(char[] message) {
    repl_emit_cli_error_json("cli/usage", message);
}

fn lisp::Interp* repl_create_plain_interp() {
    lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    if (interp == null) return null;
    interp.init();
    return interp;
}

fn void repl_destroy_plain_interp(lisp::Interp* interp) {
    if (interp == null) return;
    interp.destroy();
    mem::free(interp);
}

fn bool repl_classify_file_read_path(char[] path, ReplFileReadKind* kind_out) {
    if (kind_out == null) return false;
    if (try file = io::file::open((String)path, "r")) {
        defer (void)file.close();
        *kind_out = REPL_FILE_READ_OK;
        return true;
    }

    switch (libc::errno()) {
        case ENOENT:
            *kind_out = REPL_FILE_READ_NOT_FOUND;
        case EACCES:
        case EPERM:
            *kind_out = REPL_FILE_READ_PERMISSION_DENIED;
        case ENOTDIR:
        case EISDIR:
        case ELOOP:
        case ENAMETOOLONG:
            *kind_out = REPL_FILE_READ_PATH_FAILURE;
        default:
            *kind_out = REPL_FILE_READ_GENERIC_FAILURE;
    }
    return false;
}

fn char[] repl_file_read_kind_code(ReplFileReadKind kind) {
    switch (kind) {
        case REPL_FILE_READ_NOT_FOUND: return "io/file-not-found";
        case REPL_FILE_READ_PERMISSION_DENIED: return "io/file-read-permission-denied";
        case REPL_FILE_READ_PATH_FAILURE: return "io/file-read-path-failed";
        default: return "io/read-failed";
    }
}

fn char[] repl_file_read_kind_reason(ReplFileReadKind kind) {
    switch (kind) {
        case REPL_FILE_READ_NOT_FOUND: return "file not found";
        case REPL_FILE_READ_PERMISSION_DENIED: return "permission denied";
        case REPL_FILE_READ_PATH_FAILURE: return "invalid file path";
        default: return "unable to read file";
    }
}

fn bool repl_shared_copy_project_root(char* project_dir, char[] root_buf, usz* root_len_out) {
    if (project_dir == null) {
        char* cwd = repl_shared_getcwd(&root_buf[0], root_buf.len);
        if (cwd == null) return false;
        usz root_len = repl_shared_cstr_len(cwd);
        while (root_len > 1 && root_buf[root_len - 1] == '/') {
            root_len--;
        }
        root_buf[root_len] = 0;
        *root_len_out = root_len;
        return true;
    }

    usz root_len = 0;
    while (project_dir[root_len] != 0) {
        if (root_len >= root_buf.len - 1) return false;
        root_buf[root_len] = project_dir[root_len];
        root_len++;
    }
    while (root_len > 1 && root_buf[root_len - 1] == '/') {
        root_len--;
    }
    root_buf[root_len] = 0;
    *root_len_out = root_len;
    return true;
}

fn bool repl_shared_build_project_path(char[] path_buf, usz* path_len_out, char* root, usz root_len, char[] suffix) {
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

fn bool repl_shared_resolve_project_main_path(char* project_dir, char[] main_path_buf, usz* main_path_len_out) {
    char[1024] root_buf = {};
    usz root_len = 0;
    if (!repl_shared_copy_project_root(project_dir, root_buf[..], &root_len)) {
        return false;
    }

    char[1280] toml_path_buf = {};
    usz toml_path_len = 0;
    if (!repl_shared_build_project_path(toml_path_buf[..], &toml_path_len, &root_buf[0], root_len, "omni.toml")) {
        return false;
    }
    if (repl_shared_access(&toml_path_buf[0], REPL_SHARED_PATH_EXISTS) != 0) {
        return false;
    }

    return repl_shared_build_project_path(main_path_buf, main_path_len_out, &root_buf[0], root_len, "src/main.omni");
}
