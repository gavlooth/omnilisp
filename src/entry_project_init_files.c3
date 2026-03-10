module main;

extern fn int mkdir(char* path, uint mode) @extern("mkdir");

fn usz build_project_path(char[] path_buf, char* name, usz name_len, char[] suffix = "") {
    usz plen = 0;
    for (usz i = 0; i < name_len && plen < path_buf.len - 1; i++) {
        path_buf[plen++] = name[i];
    }
    if (suffix.len > 0 && plen < path_buf.len - 1) {
        path_buf[plen++] = '/';
        for (usz i = 0; i < suffix.len && plen < path_buf.len - 1; i++) {
            path_buf[plen++] = suffix[i];
        }
    }
    path_buf[plen] = 0;
    return plen;
}

fn void mkdir_project_path(char* name, usz name_len, char[] suffix = "") {
    char[512] path_buf;
    usz plen = build_project_path(path_buf[..], name, name_len, suffix);
    (void)plen;
    mkdir(&path_buf[0], 0o755);
}
