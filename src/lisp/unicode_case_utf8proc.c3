module lisp;

import std::core::mem;

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
    if (buf == null || cap == null) return false;
    if (required <= *cap) return true;
    usz new_cap = *cap > 0 ? *cap : 16;
    while (new_cap < required) {
        if (new_cap > usz.max / (usz)2) return false;
        new_cap *= 2;
    }
    char* grown = (char*)mem::realloc(*buf, new_cap);
    if (grown == null) return false;
    *buf = grown;
    *cap = new_cap;
    return true;
}

fn bool ensure_utf8_buf_capacity_for_append(char** buf, usz* cap, usz out, usz extra) {
    if (out > usz.max - extra) return false;
    return ensure_utf8_buf_capacity(buf, cap, out + extra);
}

fn bool unicode_is_word_codepoint(int cp) {
    ZString cat = utf8proc_category_string(cp);
    if (cat == null) return false;
    return cat[0] == 'L' || cat[0] == 'N';
}

fn Value* make_bool_value(Interp* interp, bool b) {
    return b ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}
