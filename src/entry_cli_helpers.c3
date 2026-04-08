module main;

extern fn char* c_getenv(char* name) @extern("getenv");

fn bool mem_stats_enabled() {
    char* raw = c_getenv("OMNI_MEM_STATS");
    return raw != null && raw[0] != 0 && raw[0] != '0';
}

fn usz cstr_len(char* s) @inline {
    usz n = 0;
    while (s[n] != 0) n++;
    return n;
}

fn bool cstr_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return true;
    if (haystack.len < needle.len) return false;
    usz end = haystack.len - needle.len;
    for (usz i = 0; i <= end; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn bool cli_arg_starts_with_double_dash(char* arg) @inline {
    return arg != null && arg[0] == '-' && arg[1] == '-';
}

fn bool cli_arg_starts_with_dash(char* arg) @inline {
    return arg != null && arg[0] == '-';
}

fn char* cli_first_mode_arg(int argc, char** argv) @inline {
    return argc > 1 ? argv[1] : null;
}

fn bool cli_first_arg_matches(int argc, char** argv, char[] f1, char[] f2 = "", char[] f3 = "") @inline {
    char* arg = cli_first_mode_arg(argc, argv);
    if (arg == null) return false;
    if (str_eq(arg, f1)) return true;
    if (f2.len > 0 && str_eq(arg, f2)) return true;
    if (f3.len > 0 && str_eq(arg, f3)) return true;
    return false;
}

fn char* find_unexpected_mode_arg(int argc, char** argv, int mode_idx, bool allow_json = false) {
    for (int i = mode_idx + 1; i < argc; i++) {
        if (allow_json && str_eq(argv[i], "--json")) continue;
        return argv[i];
    }
    return null;
}

fn char* find_extra_mode_arg(
    int argc,
    char** argv,
    int mode_idx,
    int allowed_positionals = 0,
    bool allow_json = false,
    bool allow_write = false,
    bool allow_check = false,
    bool allow_output_flag = false,
    bool allow_print_last = false,
    bool allow_print_all = false
) {
    int positionals = 0;
    for (int i = mode_idx + 1; i < argc; i++) {
        char* arg = argv[i];
        if (allow_json && str_eq(arg, "--json")) continue;
        if (allow_write && str_eq(arg, "--write")) continue;
        if (allow_check && str_eq(arg, "--check")) continue;
        if (allow_print_last && str_eq(arg, "--print-last")) continue;
        if (allow_print_all && str_eq(arg, "--print-all")) continue;
        if (allow_output_flag && str_eq(arg, "-o")) {
            if (i + 1 >= argc) return null;
            i++;
            continue;
        }
        if (positionals < allowed_positionals) {
            positionals++;
            continue;
        }
        return arg;
    }
    return null;
}

fn int find_flag_index(int argc, char** argv, char[] f1, char[] f2 = "", char[] f3 = "") {
    for (int i = 1; i < argc; i++) {
        if (str_eq(argv[i], f1)) return i;
        if (f2.len > 0 && str_eq(argv[i], f2)) return i;
        if (f3.len > 0 && str_eq(argv[i], f3)) return i;
    }
    return -1;
}
