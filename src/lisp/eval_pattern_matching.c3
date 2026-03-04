module lisp;

/**
 * MatchResult - Result of attempting to match a pattern against a value.
 * If successful, contains bindings produced by the match.
 * Bindings are heap-allocated and grow dynamically.
 */
const usz MATCH_INITIAL_CAPACITY = 32;

struct MatchResult {
    bool matched;
    Binding* bindings;
    usz binding_count;
    usz capacity;
}

/**
 * Auto-gensym support for pattern macros.
 * Symbols ending with # are auto-replaced with unique gensyms.
 * The mapping ensures the same #-symbol maps to the same gensym within one expansion.
 */
struct GensymMapping {
    SymbolId original;   // The foo# symbol
    SymbolId generated;  // The g#N gensym
}

struct GensymTable {
    GensymMapping[64] mappings;
    usz count;
}

fn MatchResult match_fail() @inline {
    return { .matched = false, .bindings = null, .binding_count = 0, .capacity = 0 };
}

fn MatchResult match_ok() @inline {
    Binding* b = (Binding*)mem::malloc(Binding.sizeof * MATCH_INITIAL_CAPACITY);
    return { .matched = true, .bindings = b, .binding_count = 0, .capacity = MATCH_INITIAL_CAPACITY };
}

fn void MatchResult.add_binding(MatchResult* self, SymbolId name, Value* value) {
    if (self.binding_count >= self.capacity) {
        usz new_cap = self.capacity < MATCH_INITIAL_CAPACITY ? MATCH_INITIAL_CAPACITY : self.capacity * 2;
        Binding* new_bindings = (Binding*)mem::malloc(Binding.sizeof * new_cap);
        for (usz i = 0; i < self.binding_count; i++) {
            new_bindings[i] = self.bindings[i];
        }
        if (self.bindings != null) mem::free(self.bindings);
        self.bindings = new_bindings;
        self.capacity = new_cap;
    }
    self.bindings[self.binding_count].name = name;
    self.bindings[self.binding_count].value = value;
    self.binding_count++;
}

fn void MatchResult.merge(MatchResult* self, MatchResult* other) {
    for (usz i = 0; i < other.binding_count; i++) {
        self.add_binding(other.bindings[i].name, other.bindings[i].value);
    }
}

fn void MatchResult.cleanup(MatchResult* self) {
    if (self.bindings != null) {
        mem::free(self.bindings);
        self.bindings = null;
    }
}

/**
 * Check if two values are equal (for literal pattern matching).
 */
fn bool values_equal(Value* a, Value* b, usz depth = 0) {
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;
    if (depth >= 256) return false;
    // Allow cross-type numeric comparison: (= 1 1.0) => true
    if (is_number(a) && is_number(b)) {
        return to_double(a) == to_double(b);
    }
    if (a.tag != b.tag) return false;

    switch (a.tag) {
        case NIL:
            return true;
        case INT:
            return a.int_val == b.int_val;
        case DOUBLE:
            return a.double_val == b.double_val;
        case STRING:
            if (a.str_len != b.str_len) return false;
            for (usz i = 0; i < a.str_len; i++) {
                if (a.str_chars[i] != b.str_chars[i]) return false;
            }
            return true;
        case SYMBOL:
            return a.sym_val == b.sym_val;
        case CONS:
            return values_equal(a.cons_val.car, b.cons_val.car, depth + 1) &&
                   values_equal(a.cons_val.cdr, b.cons_val.cdr, depth + 1);
        case HASHMAP:
            return a == b;  // Pointer equality for mutable containers
        case ARRAY:
            if (a.array_val.length != b.array_val.length) return false;
            for (usz i = 0; i < a.array_val.length; i++) {
                if (!values_equal(a.array_val.items[i], b.array_val.items[i], depth + 1)) return false;
            }
            return true;
        default:
            return a == b;  // Pointer equality for closures, etc.
    }
}

/**
 * Get the length of a list.
 */
fn usz get_list_length(Value* v) {
    usz len = 0;
    while (is_cons(v)) {
        len++;
        v = cdr(v);
    }
    return len;
}

/**
 * Get the nth element of a list.
 */
fn Value* get_list_nth(Value* v, usz n) {
    for (usz i = 0; i < n; i++) {
        if (!is_cons(v)) return null;
        v = cdr(v);
    }
    if (!is_cons(v)) return null;
    return car(v);
}

/**
 * Get the rest of a sequence starting at index n.
 * Works for both CONS lists and ARRAY values.
 */
fn Value* get_list_rest(Value* v, usz n, Interp* interp) {
    if (v != null && v.tag == ARRAY && v.array_val != null) {
        // ARRAY: build cons list from remaining elements
        Value* result = make_nil(interp);
        usz alen = v.array_val.length;
        for (usz i = alen; i > n; i--) {
            result = make_cons(interp, v.array_val.items[i - 1], result);
        }
        return result;
    }
    // CONS list: walk to position n
    for (usz i = 0; i < n; i++) {
        if (!is_cons(v)) return make_nil(interp);
        v = cdr(v);
    }
    return v;
}

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

fn MatchResult match_pattern_guard(Pattern* pat, Value* val, Interp* interp) {
    // Guard pattern: evaluate pred, apply to val
    if (pat.guard_sub != null) {
        MatchResult sub_r = match_pattern(pat.guard_sub, val, interp);
        if (!sub_r.matched) return match_fail();

        // Evaluate guard with sub bindings in scope
        Env* guard_env = make_env(interp, interp.match_env);
        for (usz j = 0; j < sub_r.binding_count; j++) {
            env_define_with_barrier(guard_env, sub_r.bindings[j].name, sub_r.bindings[j].value, interp);
        }
        Value* pred = jit_eval(pat.guard_pred, guard_env, interp);
        if (pred == null || pred.tag == ERROR) { sub_r.cleanup(); return match_fail(); }
        Value* result = jit_apply_value(pred, val, interp);
        if (result == null || result.tag == ERROR || jit_is_falsy(result, interp)) {
            sub_r.cleanup();
            return match_fail();
        }
        return sub_r;
    }

    // No sub-pattern: just evaluate pred and apply to val
    Value* pred = jit_eval(pat.guard_pred, interp.match_env, interp);
    if (pred == null || pred.tag == ERROR) return match_fail();
    Value* result = jit_apply_value(pred, val, interp);
    if (result == null || result.tag == ERROR || jit_is_falsy(result, interp)) {
        return match_fail();
    }
    return match_ok();
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

    // Collect elements into flat array — supports both CONS lists and ARRAY values
    Value*[64] elems;
    usz list_len = collect_seq_elements(val, elems[0:64]);

    switch (rest_pos) {
        case REST_NONE:
            if (list_len != elem_count) {
                return match_fail();
            }
            MatchResult result = match_ok();
            if (!match_seq_elements(pat, elems[0:64], 0, elem_count, interp, &result)) return match_fail();
            return result;

        case REST_START:
            if (list_len < elem_count) {
                return match_fail();
            }
            usz skip = list_len - elem_count;
            MatchResult start_result = match_ok();
            if (!match_seq_elements(pat, elems[0:64], skip, elem_count, interp, &start_result)) return match_fail();
            return start_result;

        case REST_MIDDLE:
            if (list_len < elem_count) {
                return match_fail();
            }
            MatchResult mid_result = match_ok();
            if (!match_seq_elements(pat, elems[0:64], 0, elem_count, interp, &mid_result)) return match_fail();
            Value* rest = get_list_rest(val, elem_count, interp);
            mid_result.add_binding(pat.rest_binding, rest);
            return mid_result;

        case REST_END:
            if (list_len < elem_count) {
                return match_fail();
            }
            MatchResult end_result = match_ok();
            if (!match_seq_elements(pat, elems[0:64], 0, elem_count, interp, &end_result)) return match_fail();
            return end_result;

        default:
            return match_fail();
    }
}
