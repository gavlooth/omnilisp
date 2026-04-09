module main;

fn char* find_fmt_input_file(int argc, char** argv, int fmt_idx) {
    for (int i = fmt_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--write")) continue;
        if (str_eq(argv[i], "--check")) continue;
        if (cli_arg_starts_with_dash(argv[i])) return null;
        return argv[i];
    }
    return null;
}

fn char* find_fmt_option_like_arg(int argc, char** argv, int fmt_idx) {
    for (int i = fmt_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--write")) continue;
        if (str_eq(argv[i], "--check")) continue;
        if (cli_arg_starts_with_dash(argv[i])) return argv[i];
        return null;
    }
    return null;
}
