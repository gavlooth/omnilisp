module main;

import std::io;
import libc;

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
