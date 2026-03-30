module main;

import std::io;
import libc;

extern fn int mkdir(char* path, uint mode) @extern("mkdir");
extern fn int access(char* path, int mode) @extern("access");

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

fn bool mkdir_project_path(char* name, usz name_len, char[] suffix = "") {
    char[512] path_buf;
    usz plen = 0;
    if (!build_project_path(path_buf[..], &plen, name, name_len, suffix)) {
        io::printn("Error: project path too long during --init");
        return false;
    }
    (void)plen;
    if (mkdir(&path_buf[0], 0o755) == 0) return true;
    if (libc::errno() == errno::EEXIST) return true;
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
    if (try file = io::file::open((String)path, "w")) {
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
        return write_ok;
    }
    return false;
}
