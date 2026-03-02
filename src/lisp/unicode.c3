module lisp;

import std::core::mem;
import std::io;

// ============================================================
// utf8proc extern declarations
// ============================================================

alias Utf8Char = char;  // uint8_t

extern fn long utf8proc_iterate(Utf8Char* str, long strlen, int* codepoint_ref) @extern("utf8proc_iterate");
extern fn long utf8proc_encode_char(int codepoint, Utf8Char* dst) @extern("utf8proc_encode_char");
extern fn int utf8proc_toupper(int c) @extern("utf8proc_toupper");
extern fn int utf8proc_tolower(int c) @extern("utf8proc_tolower");
extern fn long utf8proc_map(Utf8Char* str, long strlen, Utf8Char** dstptr, int options) @extern("utf8proc_map");
extern fn bool utf8proc_grapheme_break_stateful(int cp1, int cp2, int* state) @extern("utf8proc_grapheme_break_stateful");
extern fn ZString utf8proc_category_string(int codepoint) @extern("utf8proc_category_string");
extern fn void c_free(void* ptr) @extern("free");

// utf8proc option flags
const int UTF8PROC_STABLE    = (1 << 1);
const int UTF8PROC_COMPAT    = (1 << 2);
const int UTF8PROC_COMPOSE   = (1 << 3);
const int UTF8PROC_DECOMPOSE = (1 << 4);
const int UTF8PROC_CASEFOLD  = (1 << 10);
const int UTF8PROC_STRIPMARK = (1 << 13);

// NFC = COMPOSE | STABLE, NFD = DECOMPOSE | STABLE
// NFKC = COMPOSE | COMPAT | STABLE, NFKD = DECOMPOSE | COMPAT | STABLE

// ============================================================
// Unicode-aware string-upcase
// ============================================================

fn Value* prim_string_upcase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-upcase: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-upcase: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    // Worst case: each codepoint could expand (e.g. ß → SS = 2 bytes → 2 bytes, but some expand more)
    // Use utf8proc_map with an uppercase custom function? No, utf8proc_map doesn't have UPPERCASE.
    // Do it codepoint by codepoint.
    char[1024] buf;
    usz out = 0;
    usz pos = 0;

    while (pos < src.len && out < 1020) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            // Invalid UTF-8 byte — copy as-is
            buf[out++] = src[pos++];
            continue;
        }
        int upper = utf8proc_toupper(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(upper, (Utf8Char*)&enc);
        for (long j = 0; j < enc_len && out < 1020; j++) {
            buf[out++] = enc[j];
        }
        pos += (usz)consumed;
    }

    return make_string(interp, buf[:out]);
}

// ============================================================
// Unicode-aware string-downcase
// ============================================================

fn Value* prim_string_downcase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-downcase: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-downcase: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    char[1024] buf;
    usz out = 0;
    usz pos = 0;

    while (pos < src.len && out < 1020) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            buf[out++] = src[pos++];
            continue;
        }
        int lower = utf8proc_tolower(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(lower, (Utf8Char*)&enc);
        for (long j = 0; j < enc_len && out < 1020; j++) {
            buf[out++] = enc[j];
        }
        pos += (usz)consumed;
    }

    return make_string(interp, buf[:out]);
}

// ============================================================
// (string-normalize s mode) — NFC, NFD, NFKC, NFKD
// ============================================================

fn Value* prim_string_normalize(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-normalize: expected (string-normalize s mode)");
    if (!is_string(args[0])) return raise_error(interp, "string-normalize: first arg must be string");
    if (args[1].tag != SYMBOL) return raise_error(interp, "string-normalize: mode must be a symbol (NFC, NFD, NFKC, NFKD)");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];
    char[] mode_name = interp.symbols.get_name(args[1].sym_val);

    int options = UTF8PROC_STABLE;
    if (str_eq_z(mode_name, "NFC")) {
        options = UTF8PROC_STABLE | UTF8PROC_COMPOSE;
    } else if (str_eq_z(mode_name, "NFD")) {
        options = UTF8PROC_STABLE | UTF8PROC_DECOMPOSE;
    } else if (str_eq_z(mode_name, "NFKC")) {
        options = UTF8PROC_STABLE | UTF8PROC_COMPOSE | UTF8PROC_COMPAT;
    } else if (str_eq_z(mode_name, "NFKD")) {
        options = UTF8PROC_STABLE | UTF8PROC_DECOMPOSE | UTF8PROC_COMPAT;
    } else {
        return raise_error(interp, "string-normalize: mode must be NFC, NFD, NFKC, or NFKD");
    }

    Utf8Char* result = null;
    long new_len = utf8proc_map((Utf8Char*)src.ptr, (long)src.len, &result, options);
    if (new_len < 0 || result == null) {
        return raise_error(interp, "string-normalize: normalization failed");
    }

    Value* v = make_string(interp, ((char*)result)[:(usz)new_len]);
    c_free(result);
    return v;
}

// ============================================================
// (string-graphemes s) — split into list of grapheme clusters
// ============================================================

fn Value* prim_string_graphemes(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-graphemes: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-graphemes: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];

    // Collect grapheme cluster boundaries
    Value* result = make_nil(interp);
    Value*[512] clusters;
    int cluster_count = 0;

    usz cluster_start = 0;
    int prev_cp = -1;
    int state = 0;
    usz pos = 0;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1) { pos++; continue; }

        if (prev_cp >= 0 && utf8proc_grapheme_break_stateful(prev_cp, cp, &state)) {
            // Grapheme boundary — emit cluster
            if (cluster_count < 512) {
                clusters[cluster_count++] = make_string(interp, src[cluster_start..pos - 1]);
            }
            cluster_start = pos;
        }

        prev_cp = cp;
        pos += (usz)consumed;
    }

    // Last cluster
    if (cluster_start < src.len && cluster_count < 512) {
        clusters[cluster_count++] = make_string(interp, src[cluster_start..]);
    }

    // Build list in reverse
    for (int i = cluster_count - 1; i >= 0; i--) {
        result = make_cons(interp, clusters[i], result);
    }

    return result;
}

// ============================================================
// (string-codepoints s) — list of integer codepoints
// ============================================================

fn Value* prim_string_codepoints(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-codepoints: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-codepoints: expected string argument");

    char[] src = args[0].str_val.chars[:args[0].str_val.len];

    Value*[1024] cps;
    int count = 0;
    usz pos = 0;

    while (pos < src.len && count < 1024) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1) { pos++; continue; }
        cps[count++] = make_int(interp, (long)cp);
        pos += (usz)consumed;
    }

    Value* result = make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        result = make_cons(interp, cps[i], result);
    }

    return result;
}

// ============================================================
// (char-category codepoint) — Unicode category string (e.g. "Lu", "Ll")
// ============================================================

fn Value* prim_char_category(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "char-category: expected 1 argument");
    if (!is_int(args[0])) return raise_error(interp, "char-category: expected integer codepoint");

    int cp = (int)args[0].int_val;
    ZString cat = utf8proc_category_string(cp);
    if (cat == null) return make_nil(interp);

    // ZString → char[] for make_string
    usz len = 0;
    while (cat[len] != 0 && len < 8) len++;
    char[8] buf;
    for (usz i = 0; i < len; i++) buf[i] = cat[i];
    return make_string(interp, buf[:len]);
}

// Helper: compare char[] to ZString
fn bool str_eq_z(char[] a, char* b) {
    usz i = 0;
    while (i < a.len && b[i] != 0) {
        if (a[i] != b[i]) return false;
        i++;
    }
    return i == a.len && b[i] == 0;
}
