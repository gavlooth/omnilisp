module main;

import std::io;

fn bool load_build_source(char* input_file, char[]* source_out, EntryFileReadKind* kind_out = null) {
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try src = io::file::load_temp((String)input_path)) {
        *source_out = src;
        if (kind_out != null) *kind_out = ENTRY_FILE_READ_OK;
        return true;
    }
    if (kind_out != null && !classify_file_read_path(input_path, kind_out)) {
        return false;
    }
    if (kind_out != null) *kind_out = ENTRY_FILE_READ_GENERIC_FAILURE;
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
