module lisp;


import std::core::mem;

// ============================================================
// Unicode-aware string-upcase
// ============================================================

fn bool unicode_string_len_fits_utf8proc(char[] src) @inline {
    return src.len <= (usz)long.max;
}

<*
@require interp != null
*>
fn Value* prim_string_upcase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-upcase: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-upcase: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (!unicode_string_len_fits_utf8proc(src)) return string_raise(interp, "type/range-error", "string-upcase: input too large");
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return string_raise(interp, "type/out-of-memory", "string-upcase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)1)) return string_raise(interp, "type/out-of-memory", "string-upcase: out of memory");
            buf[out++] = src[pos++];
            continue;
        }

        int upper = utf8proc_toupper(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(upper, (Utf8Char*)&enc);
        if (enc_len < 0) {
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)1)) return string_raise(interp, "type/out-of-memory", "string-upcase: out of memory");
            buf[out++] = src[pos++];
            continue;
        }
        if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)enc_len)) return string_raise(interp, "type/out-of-memory", "string-upcase: out of memory");
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

<*
@require interp != null
*>
fn Value* prim_string_downcase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-downcase: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-downcase: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (!unicode_string_len_fits_utf8proc(src)) return string_raise(interp, "type/range-error", "string-downcase: input too large");
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return string_raise(interp, "type/out-of-memory", "string-downcase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)1)) return string_raise(interp, "type/out-of-memory", "string-downcase: out of memory");
            buf[out++] = src[pos++];
            continue;
        }

        int lower = utf8proc_tolower(cp);
        char[4] enc;
        long enc_len = utf8proc_encode_char(lower, (Utf8Char*)&enc);
        if (enc_len < 0) {
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)1)) return string_raise(interp, "type/out-of-memory", "string-downcase: out of memory");
            buf[out++] = src[pos++];
            continue;
        }
        if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)enc_len)) return string_raise(interp, "type/out-of-memory", "string-downcase: out of memory");
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

<*
@require interp != null
*>
fn Value* prim_string_casefold_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-casefold: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-casefold: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (!unicode_string_len_fits_utf8proc(src)) return string_raise(interp, "type/range-error", "string-casefold: input too large");
    Utf8Char* result = null;
    long new_len = utf8proc_map(
        (Utf8Char*)src.ptr,
        (long)src.len,
        &result,
        UTF8PROC_STABLE | UTF8PROC_CASEFOLD
    );
    if (new_len < 0 || result == null) {
        return string_raise(interp, "type/arg-mismatch", "string-casefold: casefold failed");
    }

    Value* v = make_string(interp, ((char*)result)[:(usz)new_len]);
    c_free(result);
    return v;
}

// ============================================================
// Unicode-aware string-titlecase
// ============================================================

<*
@require interp != null
*>
fn Value* prim_string_titlecase_unicode(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-titlecase: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-titlecase: expected string argument");

    char[] src = args[0].str_chars[:args[0].str_len];
    if (!unicode_string_len_fits_utf8proc(src)) return string_raise(interp, "type/range-error", "string-titlecase: input too large");
    usz cap = src.len > 0 ? src.len : 16;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return string_raise(interp, "type/out-of-memory", "string-titlecase: out of memory");
    defer mem::free(buf);

    usz out = 0;
    usz pos = 0;
    bool at_word_start = true;

    while (pos < src.len) {
        int cp = 0;
        long consumed = utf8proc_iterate((Utf8Char*)&src[pos], (long)(src.len - pos), &cp);
        if (consumed < 1 || cp < 0) {
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)1)) {
                return string_raise(interp, "type/out-of-memory", "string-titlecase: out of memory");
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
            if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)consumed)) {
                return string_raise(interp, "type/out-of-memory", "string-titlecase: out of memory");
            }
            for (long j = 0; j < consumed; j++) {
                buf[out++] = src[pos + (usz)j];
            }
            pos += (usz)consumed;
            continue;
        }

        if (!ensure_utf8_buf_capacity_for_append(&buf, &cap, out, (usz)enc_len)) {
            return string_raise(interp, "type/out-of-memory", "string-titlecase: out of memory");
        }
        for (long j = 0; j < enc_len; j++) {
            buf[out++] = enc[j];
        }
        pos += (usz)consumed;
    }

    return make_string(interp, buf[:out]);
}
