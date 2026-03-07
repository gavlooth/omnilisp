module lisp;

/**
 * MatchResult - Result of attempting to match a pattern against a value.
 * If successful, contains bindings produced by the match.
 * Bindings are heap-allocated and grow dynamically.
 */
const usz MATCH_INITIAL_CAPACITY = 32;
const usz GENSYM_INITIAL_CAPACITY = 16;

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
    GensymMapping* mappings;
    usz count;
    usz capacity;
}

fn void gensym_table_init(GensymTable* self) {
    self.count = 0;
    self.capacity = GENSYM_INITIAL_CAPACITY;
    self.mappings = (GensymMapping*)mem::malloc(GensymMapping.sizeof * self.capacity);
    if (self.mappings == null) self.capacity = 0;
}

fn void gensym_table_destroy(GensymTable* self) {
    if (self.mappings != null) mem::free(self.mappings);
    self.mappings = null;
    self.count = 0;
    self.capacity = 0;
}

fn bool gensym_table_ensure_capacity(GensymTable* self, usz needed) {
    if (needed <= self.capacity) return true;
    usz new_cap = self.capacity < GENSYM_INITIAL_CAPACITY ? GENSYM_INITIAL_CAPACITY : self.capacity;
    while (new_cap < needed) new_cap *= 2;
    GensymMapping* next = (GensymMapping*)mem::malloc(GensymMapping.sizeof * new_cap);
    if (next == null) return false;
    for (usz i = 0; i < self.count; i++) next[i] = self.mappings[i];
    if (self.mappings != null) mem::free(self.mappings);
    self.mappings = next;
    self.capacity = new_cap;
    return true;
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
const usz EQUAL_STACK_INITIAL_CAPACITY = 256;
const usz EQUAL_SEEN_INITIAL_CAPACITY = 256;

struct EqualPair {
    Value* left;
    Value* right;
}

fn bool values_equal(Value* a, Value* b) {
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;

    // Fast path for non-recursive values.
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
        case TIME_POINT:
            if (a.time_point_val.year != b.time_point_val.year) return false;
            if (a.time_point_val.month != b.time_point_val.month) return false;
            if (a.time_point_val.day != b.time_point_val.day) return false;
            if (a.time_point_val.hour != b.time_point_val.hour) return false;
            if (a.time_point_val.minute != b.time_point_val.minute) return false;
            if (a.time_point_val.second != b.time_point_val.second) return false;
            if (a.time_point_val.microsecond != b.time_point_val.microsecond) return false;
            if (a.time_point_val.tz_offset != b.time_point_val.tz_offset) return false;
            return a.time_point_val.kind == b.time_point_val.kind;
        case HASHMAP:
            return a == b;  // Pointer equality for mutable containers
        case CONS:
        case ARRAY:
            break;
        default:
            return a == b;  // Pointer equality for closures, etc.
    }

    usz stack_cap = EQUAL_STACK_INITIAL_CAPACITY;
    usz stack_count = 0;
    EqualPair* stack = (EqualPair*)mem::malloc(EqualPair.sizeof * stack_cap);

    usz seen_cap = EQUAL_SEEN_INITIAL_CAPACITY;
    usz seen_count = 0;
    EqualPair* seen = (EqualPair*)mem::malloc(EqualPair.sizeof * seen_cap);

    stack[stack_count++] = { .left = a, .right = b };
    bool ok = true;

    while (stack_count > 0 && ok) {
        EqualPair pair = stack[stack_count - 1];
        stack_count--;
        Value* left = pair.left;
        Value* right = pair.right;

        if (left == null && right == null) continue;
        if (left == null || right == null) {
            ok = false;
            break;
        }

        // Allow cross-type numeric comparison: (= 1 1.0) => true
        if (is_number(left) && is_number(right)) {
            if (to_double(left) != to_double(right)) {
                ok = false;
                break;
            }
            continue;
        }
        if (left.tag != right.tag) {
            ok = false;
            break;
        }

        switch (left.tag) {
            case NIL:
                break;
            case INT:
                if (left.int_val != right.int_val) ok = false;
                break;
            case DOUBLE:
                if (left.double_val != right.double_val) ok = false;
                break;
            case STRING:
                if (left.str_len != right.str_len) {
                    ok = false;
                    break;
                }
                for (usz i = 0; i < left.str_len; i++) {
                    if (left.str_chars[i] != right.str_chars[i]) {
                        ok = false;
                        break;
                    }
                }
                break;
            case SYMBOL:
                if (left.sym_val != right.sym_val) ok = false;
                break;
            case TIME_POINT: {
                if (left.time_point_val.year != right.time_point_val.year ||
                    left.time_point_val.month != right.time_point_val.month ||
                    left.time_point_val.day != right.time_point_val.day ||
                    left.time_point_val.hour != right.time_point_val.hour ||
                    left.time_point_val.minute != right.time_point_val.minute ||
                    left.time_point_val.second != right.time_point_val.second ||
                    left.time_point_val.microsecond != right.time_point_val.microsecond ||
                    left.time_point_val.tz_offset != right.time_point_val.tz_offset ||
                    left.time_point_val.kind != right.time_point_val.kind) {
                    ok = false;
                }
                break;
            }
            case CONS:
            {
                bool already_seen = false;
                for (usz i = 0; i < seen_count; i++) {
                    if (seen[i].left == left && seen[i].right == right) {
                        already_seen = true;
                        break;
                    }
                }
                if (already_seen) break;

                if (seen_count >= seen_cap) {
                    usz new_cap = seen_cap * 2;
                    EqualPair* new_seen = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    for (usz i = 0; i < seen_count; i++) new_seen[i] = seen[i];
                    mem::free(seen);
                    seen = new_seen;
                    seen_cap = new_cap;
                }
                seen[seen_count++] = { .left = left, .right = right };

                usz needed = stack_count + 2;
                if (needed > stack_cap) {
                    usz new_cap = stack_cap;
                    while (new_cap < needed) new_cap *= 2;
                    EqualPair* new_stack = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                    mem::free(stack);
                    stack = new_stack;
                    stack_cap = new_cap;
                }
                // Push cdr first so car is processed next (matches recursive order).
                stack[stack_count++] = { .left = left.cons_val.cdr, .right = right.cons_val.cdr };
                stack[stack_count++] = { .left = left.cons_val.car, .right = right.cons_val.car };
                break;
            }
            case HASHMAP:
                if (left != right) ok = false;  // Pointer equality for mutable containers
                break;
            case ARRAY:
            {
                if (left.array_val.length != right.array_val.length) {
                    ok = false;
                    break;
                }

                bool already_seen = false;
                for (usz i = 0; i < seen_count; i++) {
                    if (seen[i].left == left && seen[i].right == right) {
                        already_seen = true;
                        break;
                    }
                }
                if (already_seen) break;

                if (seen_count >= seen_cap) {
                    usz new_cap = seen_cap * 2;
                    EqualPair* new_seen = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    for (usz i = 0; i < seen_count; i++) new_seen[i] = seen[i];
                    mem::free(seen);
                    seen = new_seen;
                    seen_cap = new_cap;
                }
                seen[seen_count++] = { .left = left, .right = right };

                usz len = left.array_val.length;
                usz needed = stack_count + len;
                if (needed > stack_cap) {
                    usz new_cap = stack_cap;
                    while (new_cap < needed) new_cap *= 2;
                    EqualPair* new_stack = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                    mem::free(stack);
                    stack = new_stack;
                    stack_cap = new_cap;
                }
                for (usz i = len; i > 0; i--) {
                    usz idx = i - 1;
                    stack[stack_count++] = {
                        .left = left.array_val.items[idx],
                        .right = right.array_val.items[idx]
                    };
                }
                break;
            }
            default:
                if (left != right) ok = false;  // Pointer equality for closures, etc.
                break;
        }
    }

    mem::free(seen);
    mem::free(stack);
    return ok;
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
