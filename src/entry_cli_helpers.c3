module main;

import std::io;

extern fn char* c_getenv(char* name) @extern("getenv");

const usz CLI_CSTR_MAX_LEN = 65536;

fn bool mem_stats_enabled() {
    char* raw = c_getenv("OMNI_MEM_STATS");
    return raw != null && raw[0] != 0 && raw[0] != '0';
}

fn bool mem_telemetry_enabled() {
    char* raw = c_getenv("OMNI_MEM_TELEMETRY");
    return raw != null && raw[0] != 0 && raw[0] != '0';
}

fn bool cstr_len_bounded(char* s, usz max_len, usz* out_len, bool* out_truncated = null) @inline {
    if (out_len != null) *out_len = 0;
    if (out_truncated != null) *out_truncated = false;
    if (s == null) return true;

    usz n = 0;
    while (n < max_len && s[n] != 0) n++;
    bool truncated = n == max_len && s[n] != 0;
    if (out_len != null) *out_len = n;
    if (out_truncated != null) *out_truncated = truncated;
    return !truncated;
}

fn bool cli_cstr_len(char* s, usz* out_len) @inline {
    return cstr_len_bounded(s, CLI_CSTR_MAX_LEN, out_len);
}

fn bool cli_cstr_slice(char* s, char[]* out_slice) @inline {
    if (s == null) {
        *out_slice = "";
        return true;
    }
    usz len = 0;
    if (!cli_cstr_len(s, &len)) return false;
    *out_slice = s[:len];
    return true;
}

fn int print_cli_cstr_too_long(char[] label, bool json = false) {
    if (json) {
        char[192] msg_buf;
        if (try msg = io::bprintf(&msg_buf, "%s is too long (maximum %d bytes)",
                (ZString)label.ptr, (int)CLI_CSTR_MAX_LEN)) {
            print_cli_error_json("cli/argument-too-long", msg);
        } else {
            print_cli_error_json("cli/argument-too-long", "CLI argument is too long");
        }
        return 1;
    }
    io::printfn("Error: %s is too long (maximum %d bytes)",
        (ZString)label.ptr, (int)CLI_CSTR_MAX_LEN);
    return 1;
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
