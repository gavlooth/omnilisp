module main;

fn bool resolve_bind_project_dir(int argc, char** argv, int bind_idx, char[] proj_dir, usz* proj_dir_len_out) {
    if (bind_idx + 1 < argc) {
        char* dir = argv[bind_idx + 1];
        usz len = 0;
        while (dir[len] != 0) {
            if (len >= proj_dir.len - 1) return false;
            proj_dir[len] = dir[len];
            len++;
        }
        if (len > 0 && proj_dir[len - 1] == '/') len--;
        proj_dir[len] = 0;
        *proj_dir_len_out = len;
        return true;
    }
    if (proj_dir.len < 2) return false;
    proj_dir[0] = '.';
    proj_dir[1] = 0;
    *proj_dir_len_out = 1;
    return true;
}

fn bool build_bind_toml_path(char[] toml_path, usz* path_len_out, char* proj_dir, usz proj_dir_len) {
    usz tlen = 0;
    for (usz i = 0; i < proj_dir_len; i++) {
        if (tlen >= toml_path.len - 1) return false;
        toml_path[tlen++] = proj_dir[i];
    }
    if (tlen >= toml_path.len - 1) return false;
    toml_path[tlen++] = '/';
    char[] toml_suffix = "omni.toml";
    for (usz i = 0; i < toml_suffix.len; i++) {
        if (tlen >= toml_path.len - 1) return false;
        toml_path[tlen++] = toml_suffix[i];
    }
    toml_path[tlen] = 0;
    *path_len_out = tlen;
    return true;
}

fn bool bind_dep_name_is_safe_output_name_char(char ch) @inline {
    return (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= '0' && ch <= '9') ||
        ch == '_' ||
        ch == '-';
}

fn bool bind_dep_name_is_safe_output_name(char* dep_name) {
    if (dep_name == null || dep_name[0] == 0) return false;
    for (usz i = 0; i < 256 && dep_name[i] != 0; i++) {
        if (!bind_dep_name_is_safe_output_name_char(dep_name[i])) return false;
    }
    return true;
}

fn bool build_bind_module_path(char[] out_path, usz* path_len_out, char* proj_dir, usz proj_dir_len, char* dep_name, char[] suffix) {
    if (!bind_dep_name_is_safe_output_name(dep_name)) return false;

    usz olen = 0;
    for (usz i = 0; i < proj_dir_len; i++) {
        if (olen >= out_path.len - 1) return false;
        out_path[olen++] = proj_dir[i];
    }
    char[] ffi_mid = "/lib/ffi/";
    for (usz i = 0; i < ffi_mid.len; i++) {
        if (olen >= out_path.len - 1) return false;
        out_path[olen++] = ffi_mid[i];
    }
    for (usz i = 0; i < 256 && dep_name[i] != 0; i++) {
        if (olen >= out_path.len - 1) return false;
        out_path[olen++] = dep_name[i];
    }
    for (usz i = 0; i < suffix.len; i++) {
        if (olen >= out_path.len - 1) return false;
        out_path[olen++] = suffix[i];
    }
    out_path[olen] = 0;
    *path_len_out = olen;
    return true;
}

fn bool build_bind_output_path(char[] out_path, usz* path_len_out, char* proj_dir, usz proj_dir_len, char* dep_name) {
    return build_bind_module_path(out_path, path_len_out, proj_dir, proj_dir_len, dep_name, ".omni");
}

fn bool build_bind_raw_output_path(char[] out_path, usz* path_len_out, char* proj_dir, usz proj_dir_len, char* dep_name) {
    return build_bind_module_path(out_path, path_len_out, proj_dir, proj_dir_len, dep_name, "_raw.omni");
}

fn bool build_bind_manifest_output_path(char[] out_path, usz* path_len_out, char* proj_dir, usz proj_dir_len, char* dep_name) {
    return build_bind_module_path(out_path, path_len_out, proj_dir, proj_dir_len, dep_name, "_manifest.toml");
}
