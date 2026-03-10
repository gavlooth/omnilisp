module main;

import std::io;

fn char* find_build_output_arg(int argc, char** argv, int build_idx) {
    for (int i = build_idx + 2; i < argc; i++) {
        if (str_eq(argv[i], "-o") && i + 1 < argc) {
            return argv[i + 1];
        }
    }
    return null;
}

fn char* derive_default_output_name(char* input_file, char[] default_out) {
    usz input_len = cstr_len(input_file);
    isz last_dot = -1;
    for (usz i = 0; i < input_len; i++) {
        if (input_file[i] == '.') last_dot = (isz)i;
    }
    usz out_len = last_dot > 0 ? (usz)last_dot : input_len;
    if (out_len > default_out.len - 1) out_len = default_out.len - 1;
    for (usz i = 0; i < out_len; i++) default_out[i] = input_file[i];
    default_out[out_len] = 0;
    return &default_out[0];
}

fn bool load_build_source(char* input_file, char[]* source_out) {
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try src = io::file::load_temp((String)input_path)) {
        *source_out = src;
        return true;
    }
    io::printfn("Error: cannot read input file '%s'", (ZString)input_file);
    return false;
}

fn void parse_build_print_flags(int argc, char** argv, bool* print_last, bool* print_all) {
    *print_last = false;
    *print_all = false;
    for (int i = 0; i < argc; i++) {
        if (str_eq(argv[i], "--print-last")) *print_last = true;
        if (str_eq(argv[i], "--print-all")) *print_all = true;
    }
}

fn bool write_aot_temp_file(char[] c3_code) {
    char[] temp_path = "build/_aot_temp.c3";
    if (try file = io::file::open((String)temp_path, "w")) {
        defer (void)file.close();
        file.write(c3_code)!!;
        return true;
    }
    io::printn("Error: cannot write temp file build/_aot_temp.c3");
    return false;
}
