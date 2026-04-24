module main;

import std::core::mem;
import std::io;
import libc;
import lisp;

enum ReplFileReadKind {
    REPL_FILE_READ_OK,
    REPL_FILE_READ_NOT_FOUND,
    REPL_FILE_READ_PERMISSION_DENIED,
    REPL_FILE_READ_PATH_FAILURE,
    REPL_FILE_READ_GENERIC_FAILURE,
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
            break;
        case EACCES:
        case EPERM:
            *kind_out = REPL_FILE_READ_PERMISSION_DENIED;
            break;
        case ENOTDIR:
        case EISDIR:
        case ELOOP:
        case ENAMETOOLONG:
            *kind_out = REPL_FILE_READ_PATH_FAILURE;
            break;
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

fn bool repl_shared_resolve_project_main_path(char* project_dir, char[] main_path_buf, usz* main_path_len_out) {
    return repl_resolve_project_main_path(project_dir, main_path_buf, main_path_len_out);
}
