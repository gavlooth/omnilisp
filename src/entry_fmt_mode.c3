module main;

import std::io;

fn int run_fmt_mode(int argc, char** argv, int fmt_idx) {
    bool write = find_flag_index(argc, argv, "--write") >= 0;
    bool check = find_flag_index(argc, argv, "--check") >= 0;
    if (write && check) return print_fmt_usage();

    char* input_file = find_fmt_input_file(argc, argv, fmt_idx);
    if (input_file == null) {
        char* unexpected = find_fmt_option_like_arg(argc, argv, fmt_idx);
        if (unexpected != null) {
            return print_unexpected_mode_arg("--fmt", unexpected);
        }
        return print_fmt_usage();
    }
    char* extra = find_extra_mode_arg(argc, argv, fmt_idx, 1, false, true, true);
    if (extra != null) {
        return print_unexpected_mode_arg("--fmt", extra);
    }

    char[] input_path;
    if (!cli_cstr_slice(input_file, &input_path)) return print_cli_cstr_too_long("--fmt input file");
    if (try source = io::file::load_temp((String)input_path)) {
        FmtFormatResult formatted_result = formatted_text(source);
        if (!formatted_result.ok) return print_fmt_structure_error(input_file);
        char[] formatted = formatted_result.text;
        bool changed = !fmt_text_equal(formatted, source);

        if (check) {
            return changed ? 1 : 0;
        }

        if (write) {
            EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            if (!entry_write_text_atomic(input_path, formatted, &write_kind)) return print_fmt_write_error(input_file);
            return 0;
        }

        io::print((String)formatted);
        return 0;
    }
    return print_fmt_read_error(input_file);
}
