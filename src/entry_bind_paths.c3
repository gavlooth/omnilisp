module main;

fn usz resolve_bind_project_dir(int argc, char** argv, int bind_idx, char[] proj_dir) {
    if (bind_idx + 1 < argc) {
        char* dir = argv[bind_idx + 1];
        usz len = 0;
        while (dir[len] != 0 && len < proj_dir.len - 1) {
            proj_dir[len] = dir[len];
            len++;
        }
        if (len > 0 && proj_dir[len - 1] == '/') len--;
        proj_dir[len] = 0;
        return len;
    }
    proj_dir[0] = '.';
    proj_dir[1] = 0;
    return 1;
}

fn usz build_bind_toml_path(char[] toml_path, char[] proj_dir) {
    usz tlen = 0;
    for (usz i = 0; i < proj_dir.len && tlen < toml_path.len - 1; i++) toml_path[tlen++] = proj_dir[i];
    if (tlen < toml_path.len - 1) toml_path[tlen++] = '/';
    char[] toml_suffix = "omni.toml";
    for (usz i = 0; i < toml_suffix.len && tlen < toml_path.len - 1; i++) toml_path[tlen++] = toml_suffix[i];
    toml_path[tlen] = 0;
    return tlen;
}

fn usz build_bind_output_path(char[] out_path, char[] proj_dir, char* dep_name) {
    usz olen = 0;
    for (usz i = 0; i < proj_dir.len && olen < out_path.len - 1; i++) out_path[olen++] = proj_dir[i];
    char[] ffi_mid = "/lib/ffi/";
    for (usz i = 0; i < ffi_mid.len && olen < out_path.len - 1; i++) out_path[olen++] = ffi_mid[i];
    for (usz i = 0; dep_name[i] != 0 && olen < out_path.len - 1; i++) out_path[olen++] = dep_name[i];
    char[] omni_ext = ".omni";
    for (usz i = 0; i < omni_ext.len && olen < out_path.len - 1; i++) out_path[olen++] = omni_ext[i];
    out_path[olen] = 0;
    return olen;
}
