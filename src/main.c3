module main;

fn bool str_eq(char* s, char[] target) {
    for (usz i = 0; i < target.len; i++) if (s[i] != target[i]) return false;
    return s[target.len] == 0;
}
