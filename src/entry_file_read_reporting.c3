module main;

import std::io;
import libc;

extern fn int entry_file_uv_rename(char* src, char* dst) @extern("omni_uv_fs_rename");
extern fn int entry_file_uv_unlink(char* path) @extern("omni_uv_fs_unlink");

enum EntryFileReadKind {
    ENTRY_FILE_READ_OK,
    ENTRY_FILE_READ_NOT_FOUND,
    ENTRY_FILE_READ_PERMISSION_DENIED,
    ENTRY_FILE_READ_PATH_FAILURE,
    ENTRY_FILE_READ_GENERIC_FAILURE,
}

enum EntryFileWriteKind {
    ENTRY_FILE_WRITE_OK,
    ENTRY_FILE_WRITE_PERMISSION_DENIED,
    ENTRY_FILE_WRITE_PATH_FAILURE,
    ENTRY_FILE_WRITE_NO_SPACE,
    ENTRY_FILE_WRITE_GENERIC_FAILURE,
}

usz g_entry_atomic_temp_seq = 0;

fn bool classify_file_read_path(char[] path, EntryFileReadKind* kind_out) {
    if (kind_out == null) return false;
    if (try file = io::file::open((String)path, "r")) {
        defer (void)file.close();
        *kind_out = ENTRY_FILE_READ_OK;
        return true;
    }

    switch (libc::errno()) {
        case ENOENT:
            *kind_out = ENTRY_FILE_READ_NOT_FOUND;
        case EACCES:
        case EPERM:
            *kind_out = ENTRY_FILE_READ_PERMISSION_DENIED;
        case ENOTDIR:
        case EISDIR:
        case ELOOP:
        case ENAMETOOLONG:
            *kind_out = ENTRY_FILE_READ_PATH_FAILURE;
        default:
            *kind_out = ENTRY_FILE_READ_GENERIC_FAILURE;
    }
    return false;
}

fn EntryFileWriteKind classify_file_write_errno() {
    switch (libc::errno()) {
        case EACCES:
        case EPERM:
        case EROFS:
            return ENTRY_FILE_WRITE_PERMISSION_DENIED;
        case ENOENT:
        case ENOTDIR:
        case EISDIR:
        case ELOOP:
        case ENAMETOOLONG:
            return ENTRY_FILE_WRITE_PATH_FAILURE;
        case ENOSPC:
            return ENTRY_FILE_WRITE_NO_SPACE;
        default:
            return ENTRY_FILE_WRITE_GENERIC_FAILURE;
    }
}

fn char[] file_read_kind_code(EntryFileReadKind kind) {
    switch (kind) {
        case ENTRY_FILE_READ_NOT_FOUND: return "io/file-not-found";
        case ENTRY_FILE_READ_PERMISSION_DENIED: return "io/file-read-permission-denied";
        case ENTRY_FILE_READ_PATH_FAILURE: return "io/file-read-path-failed";
        case ENTRY_FILE_READ_GENERIC_FAILURE:
        default:
            return "io/read-failed";
    }
}

fn char[] file_read_kind_reason(EntryFileReadKind kind) {
    switch (kind) {
        case ENTRY_FILE_READ_NOT_FOUND: return "file not found";
        case ENTRY_FILE_READ_PERMISSION_DENIED: return "permission denied";
        case ENTRY_FILE_READ_PATH_FAILURE: return "invalid path";
        case ENTRY_FILE_READ_GENERIC_FAILURE:
        default:
            return "read failed";
    }
}

fn char[] file_write_kind_code(EntryFileWriteKind kind) {
    switch (kind) {
        case ENTRY_FILE_WRITE_PERMISSION_DENIED: return "io/file-write-permission-denied";
        case ENTRY_FILE_WRITE_PATH_FAILURE: return "io/file-write-path-failed";
        case ENTRY_FILE_WRITE_NO_SPACE: return "io/file-write-no-space";
        case ENTRY_FILE_WRITE_GENERIC_FAILURE:
        default:
            return "io/write-failed";
    }
}

fn char[] file_write_kind_reason(EntryFileWriteKind kind) {
    switch (kind) {
        case ENTRY_FILE_WRITE_PERMISSION_DENIED: return "permission denied";
        case ENTRY_FILE_WRITE_PATH_FAILURE: return "invalid path";
        case ENTRY_FILE_WRITE_NO_SPACE: return "no space left on device";
        case ENTRY_FILE_WRITE_GENERIC_FAILURE:
        default:
            return "write failed";
    }
}

fn void print_file_read_error_text(char[] subject, char[] path, EntryFileReadKind kind) {
    io::printfn(
        "Error: cannot read %s '%s': %s",
        (ZString)subject,
        (ZString)path,
        (ZString)file_read_kind_reason(kind)
    );
}

fn void print_file_write_error_text(char[] subject, char[] path, EntryFileWriteKind kind) {
    io::printfn(
        "Error: cannot write %s '%s': %s",
        (ZString)subject,
        (ZString)path,
        (ZString)file_write_kind_reason(kind)
    );
}

fn bool entry_copy_path_cstr(char[] path, char[] path_buf, char[]* out_path = null) {
    if (out_path != null) *out_path = "";
    if (path.len == 0 || path.len >= path_buf.len) return false;
    for (usz i = 0; i < path.len; i++) path_buf[i] = path[i];
    path_buf[path.len] = 0;
    if (out_path != null) *out_path = path_buf[:path.len];
    return true;
}

fn bool entry_build_atomic_temp_path(char[] final_path, char[] temp_buf, char[]* out_temp_path) {
    if (out_temp_path != null) *out_temp_path = "";

    char[64] suffix_buf;
    char[] suffix = "";
    usz seq = g_entry_atomic_temp_seq++;
    if (try rendered = io::bprintf(&suffix_buf, ".tmp.%d.%d", c_getpid(), (int)seq)) {
        suffix = rendered;
    } else {
        return false;
    }

    if (final_path.len == 0 || final_path.len >= temp_buf.len || suffix.len >= temp_buf.len - final_path.len) {
        return false;
    }
    usz temp_len = final_path.len + suffix.len;
    if (temp_len >= temp_buf.len) return false;
    for (usz i = 0; i < final_path.len; i++) temp_buf[i] = final_path[i];
    for (usz i = 0; i < suffix.len; i++) temp_buf[final_path.len + i] = suffix[i];
    temp_buf[temp_len] = 0;
    if (out_temp_path != null) *out_temp_path = temp_buf[:temp_len];
    return true;
}

fn bool entry_write_all_to_file(io::File file, char[] text, EntryFileWriteKind* kind_out = null) {
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    char[] remaining = text;
    while (remaining.len > 0) {
        usz? written_opt = file.write(remaining);
        if (catch err = written_opt) {
            if (kind_out != null) *kind_out = classify_file_write_errno();
            return false;
        }
        if (written_opt == 0) return false;
        remaining = remaining[written_opt..];
    }
    if (catch err = file.close()) {
        if (kind_out != null) *kind_out = classify_file_write_errno();
        return false;
    }
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_OK;
    return true;
}

fn bool entry_write_text_staged(
    char[] final_path,
    char[] text,
    char[] temp_path_buf,
    char[]* temp_path_out,
    EntryFileWriteKind* kind_out = null
) {
    if (temp_path_out != null) *temp_path_out = "";
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;

    char[4096] final_path_buf;
    char[] final_path_cstr = "";
    if (!entry_copy_path_cstr(final_path, final_path_buf[..], &final_path_cstr)) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }

    for (usz attempt = 0; attempt < 64; attempt++) {
        char[] temp_path = "";
        if (!entry_build_atomic_temp_path(final_path_cstr, temp_path_buf, &temp_path)) {
            if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
            return false;
        }

        if (try file = io::file::open((String)temp_path, "wx")) {
            bool staged = false;
            defer if (!staged) (void)entry_file_uv_unlink((ZString)temp_path.ptr);
            EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            if (!entry_write_all_to_file(file, text, &write_kind)) {
                if (kind_out != null) *kind_out = write_kind;
                return false;
            }
            staged = true;
            if (temp_path_out != null) *temp_path_out = temp_path;
            if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_OK;
            return true;
        }

        if (libc::errno() == EEXIST) continue;
        if (kind_out != null) *kind_out = classify_file_write_errno();
        return false;
    }

    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    return false;
}

fn bool entry_commit_staged_text(char[] temp_path, char[] final_path, EntryFileWriteKind* kind_out = null) {
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    char[4096] final_path_buf;
    char[] final_path_cstr = "";
    if (!entry_copy_path_cstr(final_path, final_path_buf[..], &final_path_cstr)) {
        if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_PATH_FAILURE;
        return false;
    }
    if (entry_file_uv_rename((ZString)temp_path.ptr, (ZString)final_path_cstr.ptr) != 0) {
        if (kind_out != null) *kind_out = classify_file_write_errno();
        return false;
    }
    if (kind_out != null) *kind_out = ENTRY_FILE_WRITE_OK;
    return true;
}

fn void entry_cleanup_staged_text(char[] temp_path) {
    if (temp_path.len == 0) return;
    (void)entry_file_uv_unlink((ZString)temp_path.ptr);
}

fn bool entry_write_text_atomic(char[] final_path, char[] text, EntryFileWriteKind* kind_out = null) {
    char[4160] temp_path_buf;
    char[] temp_path = "";
    if (!entry_write_text_staged(final_path, text, temp_path_buf[..], &temp_path, kind_out)) return false;
    bool committed = false;
    defer if (!committed) entry_cleanup_staged_text(temp_path);
    if (!entry_commit_staged_text(temp_path, final_path, kind_out)) return false;
    committed = true;
    return true;
}
