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

fn int find_flag_index(int argc, char** argv, char[] f1, char[] f2 = "", char[] f3 = "") {
    for (int i = 1; i < argc; i++) {
        if (str_eq(argv[i], f1)) return i;
        if (f2.len > 0 && str_eq(argv[i], f2)) return i;
        if (f3.len > 0 && str_eq(argv[i], f3)) return i;
    }
    return -1;
}
