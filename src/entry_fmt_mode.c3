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

    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
        FmtFormatResult formatted_result = formatted_text(source);
        if (!formatted_result.ok) return print_fmt_structure_error(input_file);
        char[] formatted = formatted_result.text;
        bool changed = !fmt_text_equal(formatted, source);

        if (check) {
            return changed ? 1 : 0;
        }

        if (write) {
            if (try file = io::file::open((String)input_path, "w")) {
                char[] remaining = formatted;
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
                if (!write_ok) {
                    return print_fmt_write_error(input_file);
                }
                return 0;
            }
            return print_fmt_write_error(input_file);
        }

        io::print((String)formatted);
        return 0;
    }
    return print_fmt_read_error(input_file);
}
