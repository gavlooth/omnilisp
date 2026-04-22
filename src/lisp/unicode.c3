module lisp;

fn Value* string_raise(Interp* interp, String code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, (String)message, null);
}


// ============================================================
// (string-normalize s mode) — NFC, NFD, NFKC, NFKD
// ============================================================

fn Value* prim_string_normalize(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return string_raise(interp, "type/arg-mismatch", "string-normalize: expected (string-normalize s mode)");
    if (!is_string(args[0])) return string_raise(interp, "type/arg-mismatch", "string-normalize: first arg must be string");
    if (args[1].tag != SYMBOL) return string_raise(interp, "type/arg-mismatch", "string-normalize: mode must be a symbol (NFC, NFD, NFKC, NFKD)");

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
        return string_raise(interp, "type/arg-mismatch", "string-normalize: mode must be NFC, NFD, NFKC, or NFKD");
    }

    Utf8Char* result = null;
    long new_len = utf8proc_map((Utf8Char*)src.ptr, (long)src.len, &result, options);
    if (new_len < 0 || result == null) {
        return string_raise(interp, "type/arg-mismatch", "string-normalize: normalization failed");
    }

    Value* v = make_string(interp, ((char*)result)[:(usz)new_len]);
    c_free(result);
    return v;
}

// ============================================================
// (string-graphemes s) — split into list of grapheme clusters
// ============================================================

fn Value* prim_string_graphemes(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return string_raise(interp, "type/arity", "string-graphemes: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-graphemes: expected string argument");

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
                Value* cluster = make_string(interp, src[cluster_start..pos - 1]);
                if (cluster != null && cluster.tag == ERROR) return cluster;
                clusters[cluster_count++] = cluster;
            }
            cluster_start = pos;
        }

        prev_cp = cp;
        pos += (usz)consumed;
    }

    // Last cluster
    if (cluster_start < src.len && cluster_count < 512) {
        Value* cluster = make_string(interp, src[cluster_start..]);
        if (cluster != null && cluster.tag == ERROR) return cluster;
        clusters[cluster_count++] = cluster;
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
    if (args.len < 1) return string_raise(interp, "type/arity", "string-codepoints: expected 1 argument");
    if (!is_string(args[0])) return string_raise(interp, "type/arity", "string-codepoints: expected string argument");

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
    if (args.len < 1) return string_raise(interp, "type/arity", "char-category: expected 1 argument");
    if (!is_int(args[0])) return string_raise(interp, "type/arg-mismatch", "char-category: expected integer codepoint");
    if (args[0].int_val < 0 || args[0].int_val > 0x10FFFF) {
        return string_raise(interp, "type/arg-mismatch", "char-category: codepoint out of range");
    }

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
    if (args.len < 1) return string_raise(interp, "type/arity", "char-width: expected 1 argument");
    if (!is_int(args[0])) return string_raise(interp, "type/arg-mismatch", "char-width: expected integer codepoint");
    if (args[0].int_val < 0 || args[0].int_val > 0x10FFFF) {
        return string_raise(interp, "type/arg-mismatch", "char-width: codepoint out of range");
    }

    int cp = (int)args[0].int_val;
    int width = utf8proc_charwidth(cp);
    return make_int(interp, (long)width);
}

// ============================================================
// (char-property codepoint property) — Unicode metadata lookup
// ============================================================

fn Value* prim_char_property(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return string_raise(interp, "type/arity", "char-property: expected 2 arguments");
    if (!is_int(args[0])) return string_raise(interp, "type/arg-mismatch", "char-property: first argument must be integer codepoint");
    if (args[1].tag != SYMBOL) return string_raise(interp, "type/arg-mismatch", "char-property: second argument must be a symbol");
    if (args[0].int_val < 0 || args[0].int_val > 0x10FFFF) {
        return string_raise(interp, "type/arg-mismatch", "char-property: codepoint out of range");
    }

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

    return string_raise(interp, "type/arg-mismatch", "char-property: unknown property (use category, width, upper?, lower?, letter?, digit?, whitespace?, word?)");
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
