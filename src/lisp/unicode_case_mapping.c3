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
