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
    Value* error;
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
    return { .matched = false, .error = null, .bindings = null, .binding_count = 0, .capacity = 0 };
}

fn MatchResult match_ok() @inline {
    return { .matched = true, .error = null, .bindings = null, .binding_count = 0, .capacity = 0 };
}

fn MatchResult match_error(Value* error) @inline {
    return { .matched = false, .error = error, .bindings = null, .binding_count = 0, .capacity = 0 };
}

fn bool MatchResult.has_error(MatchResult* self) @inline {
    return self != null && self.error != null && self.error.tag == ERROR;
}

fn MatchResult match_take_error(MatchResult* self) @inline {
    Value* err = self != null ? self.error : null;
    if (self != null) self.cleanup();
    return match_error(err);
}

fn bool MatchResult.add_binding(MatchResult* self, SymbolId name, Value* value) {
    if (self.binding_count >= self.capacity) {
        usz new_cap = self.capacity < MATCH_INITIAL_CAPACITY ? MATCH_INITIAL_CAPACITY : self.capacity * 2;
        Binding* new_bindings = (Binding*)mem::malloc(Binding.sizeof * new_cap);
        if (new_bindings == null) return false;
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
    return true;
}

fn bool MatchResult.merge(MatchResult* self, MatchResult* other) {
    for (usz i = 0; i < other.binding_count; i++) {
        if (!self.add_binding(other.bindings[i].name, other.bindings[i].value)) {
            return false;
        }
    }
    return true;
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
// Keep common nested equality cases on-stack first to avoid avoidable heap churn.
const usz EQUAL_STACK_INLINE_CAPACITY = 1024;
const usz EQUAL_SEEN_INLINE_CAPACITY = 1024;

struct EqualityWorkspaceStats {
    usz stack_heap_growths;
    usz seen_heap_growths;
    usz stack_heap_bytes;
    usz seen_heap_bytes;
}

struct EqualPair {
    Value* left;
    Value* right;
}

fn bool values_equal_with_workspace_stats(Value* a, Value* b, EqualityWorkspaceStats* workspace_stats = null) {
    if (workspace_stats != null) {
        workspace_stats.stack_heap_growths = 0;
        workspace_stats.seen_heap_growths = 0;
        workspace_stats.stack_heap_bytes = 0;
        workspace_stats.seen_heap_bytes = 0;
    }

    if (a == null && b == null) return true;
    if (a == null || b == null) return false;

    // Fast path for non-recursive values.
    if (is_number(a) && is_number(b)) {
        return to_double(a) == to_double(b);
    }
    if (a.tag != b.tag) return false;

    switch (a.tag) {
        case NIL:
        case VOID:
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
        case SET:
            return a == b;  // Pointer equality for mutable containers
        case CONS:
        case ARRAY:
            break;
        default:
            return a == b;  // Pointer equality for closures, etc.
    }

    EqualPair[EQUAL_STACK_INLINE_CAPACITY] stack_inline = {};
    EqualPair[EQUAL_SEEN_INLINE_CAPACITY] seen_inline = {};

    EqualPair* stack = &stack_inline[0];
    EqualPair* seen = &seen_inline[0];
    bool stack_heap = false;
    bool seen_heap = false;

    usz stack_cap = EQUAL_STACK_INLINE_CAPACITY;
    usz stack_count = 0;
    usz seen_cap = EQUAL_SEEN_INLINE_CAPACITY;
    usz seen_count = 0;

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
            case VOID:
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
                    if (new_seen == null) {
                        ok = false;
                        break;
                    }
                    for (usz i = 0; i < seen_count; i++) new_seen[i] = seen[i];
                    if (seen_heap) mem::free(seen);
                    seen = new_seen;
                    seen_heap = true;
                    seen_cap = new_cap;
                    if (workspace_stats != null) {
                        workspace_stats.seen_heap_growths++;
                        workspace_stats.seen_heap_bytes += EqualPair.sizeof * new_cap;
                    }
                }
                seen[seen_count++] = { .left = left, .right = right };

                usz needed = stack_count + 2;
                if (needed > stack_cap) {
                    usz new_cap = stack_cap;
                    while (new_cap < needed) new_cap *= 2;
                    EqualPair* new_stack = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    if (new_stack == null) {
                        ok = false;
                        break;
                    }
                    for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                    if (stack_heap) mem::free(stack);
                    stack = new_stack;
                    stack_heap = true;
                    stack_cap = new_cap;
                    if (workspace_stats != null) {
                        workspace_stats.stack_heap_growths++;
                        workspace_stats.stack_heap_bytes += EqualPair.sizeof * new_cap;
                    }
                }
                if (!ok) break;
                // Push cdr first so car is processed next (matches recursive order).
                stack[stack_count++] = { .left = left.cons_val.cdr, .right = right.cons_val.cdr };
                stack[stack_count++] = { .left = left.cons_val.car, .right = right.cons_val.car };
                break;
            }
            case HASHMAP:
            case SET:
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
                    if (new_seen == null) {
                        ok = false;
                        break;
                    }
                    for (usz i = 0; i < seen_count; i++) new_seen[i] = seen[i];
                    if (seen_heap) mem::free(seen);
                    seen = new_seen;
                    seen_heap = true;
                    seen_cap = new_cap;
                    if (workspace_stats != null) {
                        workspace_stats.seen_heap_growths++;
                        workspace_stats.seen_heap_bytes += EqualPair.sizeof * new_cap;
                    }
                }
                if (!ok) break;
                seen[seen_count++] = { .left = left, .right = right };

                usz len = left.array_val.length;
                usz needed = stack_count + len;
                if (needed > stack_cap) {
                    usz new_cap = stack_cap;
                    while (new_cap < needed) new_cap *= 2;
                    EqualPair* new_stack = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
                    if (new_stack == null) {
                        ok = false;
                        break;
                    }
                    for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                    if (stack_heap) mem::free(stack);
                    stack = new_stack;
                    stack_heap = true;
                    stack_cap = new_cap;
                    if (workspace_stats != null) {
                        workspace_stats.stack_heap_growths++;
                        workspace_stats.stack_heap_bytes += EqualPair.sizeof * new_cap;
                    }
                }
                if (!ok) break;
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

    if (seen_heap) mem::free(seen);
    if (stack_heap) mem::free(stack);
    return ok;
}

fn bool values_equal(Value* a, Value* b) {
    return values_equal_with_workspace_stats(a, b);
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
