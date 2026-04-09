module lisp;

// --- Buffer helpers ---

fn usz buf_append(char[] buf, usz pos, char[] str, bool* ok) {
    usz i = 0;
    for (; i < str.len && pos < buf.len - 1; i++) {
        buf[pos] = str[i];
        pos++;
    }
    if (i < str.len) *ok = false;
    return pos;
}

fn usz buf_append_char(char[] buf, usz pos, char c, bool* ok) {
    if (pos < buf.len - 1) {
        buf[pos] = c;
        pos++;
    } else {
        *ok = false;
    }
    return pos;
}

fn usz buf_append_cstr(char[] buf, usz pos, void* cstr_ptr, bool* ok) {
    char* s = (char*)cstr_ptr;
    usz i = 0;
    while (s[i] != 0 && pos < buf.len - 1) {
        buf[pos] = s[i];
        pos++;
        i++;
    }
    if (s[i] != 0) *ok = false;
    return pos;
}

/**
 * Append a C identifier converted to kebab-case.
 * snake_case → kebab-case: replace '_' with '-'
 */
fn usz buf_append_kebab(char[] buf, usz pos, void* cstr_ptr, bool* ok) {
    char* s = (char*)cstr_ptr;
    usz i = 0;
    while (s[i] != 0 && pos < buf.len - 1) {
        if (s[i] == '_') {
            buf[pos] = '-';
        } else {
            buf[pos] = s[i];
        }
        pos++;
        i++;
    }
    if (s[i] != 0) *ok = false;
    return pos;
}
