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
    while (new_cap < needed) {
        if (new_cap > usz.max / (usz)2) return false;
        new_cap *= 2;
    }
    if (new_cap > usz.max / GensymMapping.sizeof) return false;
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
    if (self == null) return false;
    if (self.binding_count >= self.capacity) {
        if (self.capacity >= MATCH_INITIAL_CAPACITY && self.capacity > usz.max / (usz)2) return false;
        usz new_cap = self.capacity < MATCH_INITIAL_CAPACITY ? MATCH_INITIAL_CAPACITY : self.capacity * 2;
        if (new_cap > usz.max / Binding.sizeof) return false;
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
