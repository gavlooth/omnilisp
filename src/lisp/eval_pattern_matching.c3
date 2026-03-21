module lisp;

fn MatchResult match_pattern_var(Pattern* pat, Value* val, Interp* interp) {
    // Check if this symbol is a nullary constructor (e.g., None)
    TypeId tid = interp.types.lookup(pat.var_name, &interp.symbols);
    if (tid != INVALID_TYPE_ID) {
        TypeInfo* ti = interp.types.get(tid);
        if (ti != null && (ti.kind == TK_CONCRETE || ti.kind == TK_UNION) && ti.field_count == 0) {
            // Nullary constructor pattern: value must be INSTANCE of this type
            if (val != null && val.tag == INSTANCE && val.instance_val != null &&
                val.instance_val.type_id == tid) {
                return match_ok();
            }
            return match_fail();
        }
    }

    // Regular variable: matches anything and binds
    MatchResult r = match_ok();
    r.add_binding(pat.var_name, val);
    return r;
}

fn MatchResult match_pattern_cons(Pattern* pat, Value* val, Interp* interp) {
    if (!is_cons(val)) return match_fail();

    MatchResult car_result = match_pattern(pat.car_pat, car(val), interp);
    if (!car_result.matched) return match_fail();

    MatchResult cdr_result = match_pattern(pat.cdr_pat, cdr(val), interp);
    if (!cdr_result.matched) {
        car_result.cleanup();
        return match_fail();
    }
    car_result.merge(&cdr_result);
    cdr_result.cleanup();
    return car_result;
}

fn MatchResult match_pattern_dict(Pattern* pat, Value* val, Interp* interp) {
    // Dict pattern {name age} — extract 'name and 'age keys from hashmap
    if (val == null || val.tag != HASHMAP || val.hashmap_val == null) {
        return match_fail();
    }
    MatchResult dict_result = match_ok();
    for (usz di = 0; di < pat.dict_key_count; di++) {
        // Look up 'key in the dict
        Value* lookup_key = interp.alloc_value();
        lookup_key.tag = SYMBOL;
        lookup_key.sym_val = pat.dict_keys[di];
        Value* found = hashmap_get(val.hashmap_val, lookup_key);
        // Bind to the key name (nil if not found)
        if (found == null) found = make_nil(interp);
        dict_result.add_binding(pat.dict_keys[di], found);
    }
    return dict_result;
}

fn MatchResult match_pattern_constructor(Pattern* pat, Value* val, Interp* interp) {
    // Constructor pattern: (ConstructorName sub-patterns...)
    // Value must be INSTANCE of the named type
    if (val == null || val.tag != INSTANCE || val.instance_val == null) {
        return match_fail();
    }
    TypeId tid = interp.types.lookup(pat.constructor_name, &interp.symbols);
    if (tid == INVALID_TYPE_ID || val.instance_val.type_id != tid) {
        return match_fail();
    }
    // Match sub-patterns against fields
    if (pat.ctor_sub_count != val.instance_val.field_count) {
        return match_fail();
    }
    MatchResult result = match_ok();
    for (usz ci = 0; ci < pat.ctor_sub_count; ci++) {
        MatchResult sub = match_pattern(pat.ctor_sub_patterns[ci], &val.instance_val.fields[ci], interp);
        if (!sub.matched) { result.cleanup(); return match_fail(); }
        result.merge(&sub);
        sub.cleanup();
    }
    return result;
}

fn bool guard_pred_accepts_scrutinee(Value* pred) @inline {
    if (pred == null) return false;
    switch (pred.tag) {
        case CLOSURE:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case CONTINUATION:
        case METHOD_TABLE:
            return true;
        default:
            return false;
    }
}

fn bool match_guard_predicate_passes(Value* pred, Value* val, Interp* interp) @inline {
    if (pred == null || pred.tag == ERROR) return false;
    if (!guard_pred_accepts_scrutinee(pred)) {
        // Direct guard form: treat evaluated guard value itself as the condition.
        return !jit_is_falsy(pred, interp);
    }
    Value* result = jit_apply_value(pred, val, interp);
    if (result == null || result.tag == ERROR) return false;
    return !jit_is_falsy(result, interp);
}

fn MatchResult match_pattern_guard(Pattern* pat, Value* val, Interp* interp) {
    // Guard pattern: evaluate predicate in scope. Callable predicates are applied
    // to the scrutinee; non-callable results are treated as direct conditions.
    if (pat.guard_sub != null) {
        MatchResult sub_r = match_pattern(pat.guard_sub, val, interp);
        if (!sub_r.matched) return match_fail();

        // Evaluate guard with sub bindings in scope
        Env* guard_env = make_env(interp, interp.match_env);
        for (usz j = 0; j < sub_r.binding_count; j++) {
            env_define_with_barrier(guard_env, sub_r.bindings[j].name, sub_r.bindings[j].value, interp);
        }
        Value* pred = runtime_eval_expr(pat.guard_pred, guard_env, interp);
        if (!match_guard_predicate_passes(pred, val, interp)) {
            sub_r.cleanup();
            return match_fail();
        }
        return sub_r;
    }

    // No sub-pattern: evaluate in current match env and test.
    Value* pred = runtime_eval_expr(pat.guard_pred, interp.match_env, interp);
    if (!match_guard_predicate_passes(pred, val, interp)) {
        return match_fail();
    }
    return match_ok();
}

fn MatchResult match_pattern_as(Pattern* pat, Value* val, Interp* interp) {
    MatchResult result = match_pattern(pat.as_sub, val, interp);
    if (!result.matched) return result;
    result.add_binding(pat.as_name, val);
    return result;
}

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
        if (!elem_result.matched) {
            out_result.cleanup();
            return false;
        }
        out_result.merge(&elem_result);
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
        list_len = get_list_length(val);
    }

    Value** elems = null;
    if (list_len > 0) {
        elems = (Value**)mem::malloc(Value*.sizeof * list_len);
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
                seq_result = match_fail();
            }
            break;

        case REST_START:
            if (list_len < elem_count) {
                break;
            }
            usz skip = list_len - elem_count;
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], skip, elem_count, interp, &seq_result)) {
                seq_result = match_fail();
            }
            break;

        case REST_MIDDLE:
            if (list_len < elem_count) {
                break;
            }
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], 0, elem_count, interp, &seq_result)) {
                seq_result = match_fail();
                break;
            }
            Value* rest = get_list_rest(val, elem_count, interp);
            seq_result.add_binding(pat.rest_binding, rest);
            break;

        case REST_END:
            if (list_len < elem_count) {
                break;
            }
            seq_result = match_ok();
            if (!match_seq_elements(pat, elems[0:list_len], 0, elem_count, interp, &seq_result)) {
                seq_result = match_fail();
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
