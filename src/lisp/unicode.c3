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
extern fn int utf8proc_totitle(int c) @extern("utf8proc_totitle");
extern fn long utf8proc_map(Utf8Char* str, long strlen, Utf8Char** dstptr, int options) @extern("utf8proc_map");
extern fn bool utf8proc_grapheme_break_stateful(int cp1, int cp2, int* state) @extern("utf8proc_grapheme_break_stateful");
extern fn ZString utf8proc_category_string(int codepoint) @extern("utf8proc_category_string");
extern fn int utf8proc_charwidth(int codepoint) @extern("utf8proc_charwidth");
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

fn bool ensure_utf8_buf_capacity(char** buf, usz* cap, usz required) {
    if (required <= *cap) return true;
    usz new_cap = *cap;
    while (new_cap < required) new_cap *= 2;
    char* grown = (char*)mem::realloc(*buf, new_cap);
    if (grown == null) return false;
    *buf = grown;
    *cap = new_cap;
    return true;
}

fn bool unicode_is_word_codepoint(int cp) {
    ZString cat = utf8proc_category_string(cp);
    if (cat == null) return false;
    return cat[0] == 'L' || cat[0] == 'N';
}

fn Value* make_bool_value(Interp* interp, bool b) {
    return b ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

// ============================================================
// Unicode-aware string-upcase
// ============================================================

fn Value* prim_string_upcase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-upcase: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-upcase: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return raise_error(interp, "string-upcase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (out + 1 > cap) {
                usz new_cap = cap * 2;
                if (new_cap < out + 1) new_cap = out + 1;
                char* grown = (char*)mem::realloc(buf, new_cap);
                if (grown == null) return raise_error(interp, "string-upcase: out of memory");
                buf = grown;
                cap = new_cap;
            }
            buf[out++] = src[pos++];
            continue;
        }

        int upper = utf8proc_toupper(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(upper, (Utf8Char*)&enc);
        if (enc_len < 0) {
            if (out + 1 > cap) {
                usz new_cap = cap * 2;
                if (new_cap < out + 1) new_cap = out + 1;
                char* grown = (char*)mem::realloc(buf, new_cap);
                if (grown == null) return raise_error(interp, "string-upcase: out of memory");
                buf = grown;
                cap = new_cap;
            }
            buf[out++] = src[pos++];
            continue;
        }
        if (out + (usz)enc_len > cap) {
            usz new_cap = cap;
            while (new_cap < out + (usz)enc_len) {
                new_cap *= 2;
            }
            char* grown = (char*)mem::realloc(buf, new_cap);
            if (grown == null) return raise_error(interp, "string-upcase: out of memory");
            buf = grown;
            cap = new_cap;
        }
        for (long j = 0; j < enc_len; j++) {
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

    char[] src = args[0].str_chars[:args[0].str_len];
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return raise_error(interp, "string-downcase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (out + 1 > cap) {
                usz new_cap = cap * 2;
                if (new_cap < out + 1) new_cap = out + 1;
                char* grown = (char*)mem::realloc(buf, new_cap);
                if (grown == null) return raise_error(interp, "string-downcase: out of memory");
                buf = grown;
                cap = new_cap;
            }
            buf[out++] = src[pos++];
            continue;
        }

        int lower = utf8proc_tolower(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(lower, (Utf8Char*)&enc);
        if (enc_len < 0) {
            if (out + 1 > cap) {
                usz new_cap = cap * 2;
                if (new_cap < out + 1) new_cap = out + 1;
                char* grown = (char*)mem::realloc(buf, new_cap);
                if (grown == null) return raise_error(interp, "string-downcase: out of memory");
                buf = grown;
                cap = new_cap;
            }
            buf[out++] = src[pos++];
            continue;
        }
        if (out + (usz)enc_len > cap) {
            usz new_cap = cap;
            while (new_cap < out + (usz)enc_len) {
                new_cap *= 2;
            }
            char* grown = (char*)mem::realloc(buf, new_cap);
            if (grown == null) return raise_error(interp, "string-downcase: out of memory");
            buf = grown;
            cap = new_cap;
        }
        for (long j = 0; j < enc_len; j++) {
            buf[out++] = enc[j];
        }
        pos += (usz)consumed;
    }

    return make_string(interp, buf[:out]);
}

// ============================================================
// Unicode-aware string-casefold
// ============================================================

fn Value* prim_string_casefold_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-casefold: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-casefold: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    Utf8Char* result = null;
    long new_len = utf8proc_map(
        (Utf8Char*)src.ptr,
        (long)src.len,
        &result,
        UTF8PROC_STABLE | UTF8PROC_CASEFOLD
    );
    if (new_len < 0 || result == null) {
        return raise_error(interp, "string-casefold: casefold failed");
    }

    Value* v = make_string(interp, ((char*)result)[:(usz)new_len]);
    c_free(result);
    return v;
}

// ============================================================
// Unicode-aware string-titlecase
// ============================================================

fn Value* prim_string_titlecase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-titlecase: expected 1 argument");
    if (!is_string(args[0])) return raise_error(interp, "string-titlecase: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return raise_error(interp, "string-titlecase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;
    bool at_word_start = true;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (!ensure_utf8_buf_capacity(&buf, &cap, out + 1)) {
                return raise_error(interp, "string-titlecase: out of memory");
            }
            buf[out++] = src[pos++];
            at_word_start = true;
            continue;
        }

        bool is_word_cp = unicode_is_word_codepoint(cp);
        int mapped = cp;
        if (is_word_cp) {
            mapped = at_word_start ? utf8proc_totitle(cp) : utf8proc_tolower(cp);
            at_word_start = false;
        } else {
            at_word_start = true;
        }

        char[4] enc;
        long enc_len = utf8proc_encode_char(mapped, (Utf8Char*)&enc);
        if (enc_len < 0) {
            if (!ensure_utf8_buf_capacity(&buf, &cap, out + (usz)consumed)) {
                return raise_error(interp, "string-titlecase: out of memory");
            }
            for (long j = 0; j < consumed; j++) {
                buf[out++] = src[pos + (usz)j];
            }
            pos += (usz)consumed;
            continue;
        }

        if (!ensure_utf8_buf_capacity(&buf, &cap, out + (usz)enc_len)) {
            return raise_error(interp, "string-titlecase: out of memory");
        }
        for (long j = 0; j < enc_len; j++) {
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

    char[] src = args[0].str_chars[:args[0].str_len];
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

    char[] src = args[0].str_chars[:args[0].str_len];

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

    char[] src = args[0].str_chars[:args[0].str_len];

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

// ============================================================
// (char-width codepoint) — display width hint (-1, 0, 1, 2)
// ============================================================

fn Value* prim_char_width(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "char-width: expected 1 argument");
    if (!is_int(args[0])) return raise_error(interp, "char-width: expected integer codepoint");

    int cp = (int)args[0].int_val;
    int width = utf8proc_charwidth(cp);
    return make_int(interp, (long)width);
}

// ============================================================
// (char-property codepoint property) — Unicode metadata lookup
// ============================================================

fn Value* prim_char_property(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "char-property: expected 2 arguments");
    if (!is_int(args[0])) return raise_error(interp, "char-property: first argument must be integer codepoint");
    if (args[1].tag != SYMBOL) return raise_error(interp, "char-property: second argument must be a symbol");

    int cp = (int)args[0].int_val;
    char[] prop = interp.symbols.get_name(args[1].sym_val);
    ZString cat = utf8proc_category_string(cp);

    if (str_eq_z(prop, "category")) {
        if (cat == null) return make_nil(interp);
        usz len = 0;
        while (cat[len] != 0 && len < 8) len++;
        char[8] buf;
        for (usz i = 0; i < len; i++) buf[i] = cat[i];
        return make_string(interp, buf[:len]);
    }

    if (str_eq_z(prop, "width")) {
        return make_int(interp, (long)utf8proc_charwidth(cp));
    }

    if (cat == null) return make_nil(interp);

    if (str_eq_z(prop, "upper?")) {
        return make_bool_value(interp, cat[0] == 'L' && cat[1] == 'u');
    }
    if (str_eq_z(prop, "lower?")) {
        return make_bool_value(interp, cat[0] == 'L' && cat[1] == 'l');
    }
    if (str_eq_z(prop, "letter?")) {
        return make_bool_value(interp, cat[0] == 'L');
    }
    if (str_eq_z(prop, "digit?")) {
        return make_bool_value(interp, cat[0] == 'N' && cat[1] == 'd');
    }
    if (str_eq_z(prop, "whitespace?")) {
        bool is_space_category = cat[0] == 'Z';
        bool is_ascii_space = cp == 9 || cp == 10 || cp == 11 || cp == 12 || cp == 13 || cp == 32;
        return make_bool_value(interp, is_space_category || is_ascii_space);
    }
    if (str_eq_z(prop, "word?")) {
        return make_bool_value(interp, cat[0] == 'L' || cat[0] == 'N' || cp == 95);
    }

    return raise_error(interp, "char-property: unknown property (use category, width, upper?, lower?, letter?, digit?, whitespace?, word?)");
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
