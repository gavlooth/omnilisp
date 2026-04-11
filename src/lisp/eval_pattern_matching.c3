module lisp;

/**
 * Match a pattern against a value.
 */
fn MatchResult match_pattern(Pattern* pat, Value* val, Interp* interp) {
    switch (pat.tag) {
        case PAT_WILDCARD:
            // Wildcard matches anything
            return match_ok();
        case PAT_VAR:
            return match_pattern_var(pat, val, interp);

        case PAT_LIT:
            // Literal matches if values are equal
            if (values_equal(pat.lit_value, val)) {
                return match_ok();
            }
            return match_fail();

        case PAT_QUOTE:
            // Quoted pattern matches quoted datum
            if (values_equal(pat.quote_datum, val)) {
                return match_ok();
            }
            return match_fail();

        case PAT_CONS:
            return match_pattern_cons(pat, val, interp);

        case PAT_SEQ:
            // Sequence pattern matches lists and arrays
            return match_seq_pattern(pat, val, interp);
        case PAT_AS:
            return match_pattern_as(pat, val, interp);
        case PAT_DICT:
            return match_pattern_dict(pat, val, interp);
        case PAT_CONSTRUCTOR:
            return match_pattern_constructor(pat, val, interp);
        case PAT_GUARD:
            return match_pattern_guard(pat, val, interp);

        default:
            return match_fail();
    }
}

fn usz collect_seq_elements(Value* val, Value*[] elems) {
    usz list_len = 0;
    if (val != null && val.tag == ARRAY && val.array_val != null) {
        // ARRAY: read elements directly.
        usz alen = val.array_val.length;
        if (alen > elems.len) alen = elems.len;
        for (usz i = 0; i < alen; i++) {
            elems[i] = val.array_val.items[i];
        }
        return alen;
    }

    // CONS list: walk and collect.
    Value* cursor = val;
    while (is_cons(cursor) && list_len < elems.len) {
        elems[list_len] = car(cursor);
        list_len++;
        cursor = cdr(cursor);
    }
    return list_len;
}

fn bool match_seq_elements(Pattern* pat, Value*[] elems, usz start_idx, usz elem_count, Interp* interp, MatchResult* out_result) {
    for (usz i = 0; i < elem_count; i++) {
        MatchResult elem_result = match_pattern(pat.elements[i], elems[start_idx + i], interp);
        if (elem_result.has_error()) {
            out_result.cleanup();
            out_result.matched = false;
            out_result.error = elem_result.error;
            elem_result.cleanup();
            return false;
        }
        if (!elem_result.matched) {
            out_result.cleanup();
            return false;
        }
        if (!out_result.merge(&elem_result)) {
            out_result.cleanup();
            out_result.matched = false;
            out_result.error = raise_error(interp, "match: out of memory while binding sequence pattern");
            elem_result.cleanup();
            return false;
        }
        elem_result.cleanup();
    }
    return true;
}

/**
 * Match a sequence pattern against a value.
 */
fn MatchResult match_seq_pattern(Pattern* pat, Value* val, Interp* interp) {
    usz elem_count = pat.elem_count;
    RestPosition rest_pos = pat.rest_pos;

    // Collect elements into a flat array sized to the full sequence length.
    usz list_len = 0;
    if (val != null && val.tag == ARRAY && val.array_val != null) {
        list_len = val.array_val.length;
    } else {
        if (!is_list(val)) return match_fail();
        list_len = get_list_length(val);
    }

    Value** elems = null;
    if (list_len > 0) {
        if (list_len > usz.max / Value*.sizeof) {
            return match_error(raise_error(interp, "match: out of memory while collecting sequence elements"));
        }
        elems = (Value**)mem::malloc(Value*.sizeof * list_len);
        if (elems == null) {
            return match_error(raise_error(interp, "match: out of memory while collecting sequence elements"));
        }
        collect_seq_elements(val, elems[0:list_len]);
    }

    MatchResult seq_result = match_fail();

    switch (rest_pos) {
        case REST_NONE:
            if (list_len != elem_count) {
                break;
            }
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], 0, elem_count, interp, &seq_result)) {
                if (!seq_result.has_error()) seq_result = match_fail();
            }
            break;

        case REST_START:
            if (list_len < elem_count) {
                break;
            }
            usz skip = list_len - elem_count;
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], skip, elem_count, interp, &seq_result)) {
                if (!seq_result.has_error()) seq_result = match_fail();
            }
            break;

        case REST_MIDDLE:
            if (list_len < elem_count) {
                break;
            }
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], 0, elem_count, interp, &seq_result)) {
                if (!seq_result.has_error()) seq_result = match_fail();
                break;
            }
            Value* rest = get_list_rest(val, elem_count, interp);
            if (rest != null && rest.tag == ERROR) {
                seq_result.cleanup();
                seq_result = match_error(rest);
                break;
            }
            if (!seq_result.add_binding(pat.rest_binding, rest)) {
                seq_result.cleanup();
                seq_result = match_binding_oom(interp);
            }
            break;

        case REST_END:
            if (list_len < elem_count) {
                break;
            }
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], 0, elem_count, interp, &seq_result)) {
                if (!seq_result.has_error()) seq_result = match_fail();
            }
            break;

        default:
            break;
    }

    if (elems != null) {
        mem::free(elems);
    }
    return seq_result;
}
