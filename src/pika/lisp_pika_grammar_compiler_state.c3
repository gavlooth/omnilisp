module pika;

import std::core::mem;
import lisp;

// Grammar compiler context
struct GrammarCompiler {
    RuleBuilder rb;
    lisp::Interp* interp;
    // Map rule names to indices
    lisp::SymbolId* rule_names;
    int* rule_indices;
    int rule_count;
    int rule_capacity;
    bool has_error;
    char[] error_code;
    char[] error_message;
}

fn void GrammarCompiler.init(GrammarCompiler* self, lisp::Interp* interp) {
    self.rb = rb_new();
    self.interp = interp;
    self.rule_names = null;
    self.rule_indices = null;
    self.rule_count = 0;
    self.rule_capacity = 0;
    self.has_error = false;
    self.error_code = "";
    self.error_message = "";

    self.rule_capacity = 32;
    if (self.rule_capacity > usz.max / lisp::SymbolId.sizeof ||
        self.rule_capacity > usz.max / int.sizeof ||
        self.rule_capacity > int.max / 2) {
        self.rule_capacity = 0;
        self.has_error = true;
        self.error_code = "parser/grammar-oom";
        self.error_message = "pika/grammar: failed to allocate rule table";
        return;
    }

    usz rule_name_bytes = lisp::SymbolId.sizeof * (usz)self.rule_capacity;
    usz rule_idx_bytes = int.sizeof * (usz)self.rule_capacity;
    self.rule_names = (lisp::SymbolId*)mem::malloc(rule_name_bytes);
    self.rule_indices = (int*)mem::malloc(rule_idx_bytes);
    if (self.rule_names == null || self.rule_indices == null) {
        if (self.rule_names != null) mem::free(self.rule_names);
        if (self.rule_indices != null) mem::free(self.rule_indices);
        self.rule_names = null;
        self.rule_indices = null;
        self.rule_capacity = 0;
        self.has_error = true;
        self.error_code = "parser/grammar-oom";
        self.error_message = "pika/grammar: failed to allocate rule table";
    }
}

fn void gc_set_error(GrammarCompiler* gc, char[] code, char[] message) {
    if (gc.has_error) return;
    gc.has_error = true;
    gc.error_code = code;
    gc.error_message = message;
}

fn bool gc_ensure_rule_capacity(GrammarCompiler* gc) {
    if (gc.rule_count < gc.rule_capacity) return true;
    if (gc.rule_names == null || gc.rule_indices == null) {
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }
    if (gc.rule_count < 0 || gc.rule_count > gc.rule_capacity) {
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }

    if (gc.rule_capacity > int.max / 2) {
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }

    int new_cap = gc.rule_capacity < 32 ? 32 : gc.rule_capacity * 2;
    if ((usz)new_cap > usz.max / lisp::SymbolId.sizeof ||
        (usz)new_cap > usz.max / int.sizeof) {
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }

    usz new_name_bytes = lisp::SymbolId.sizeof * (usz)new_cap;
    usz new_idx_bytes = int.sizeof * (usz)new_cap;
    lisp::SymbolId* new_names = (lisp::SymbolId*)mem::malloc(new_name_bytes);
    int* new_indices = (int*)mem::malloc(new_idx_bytes);
    if (new_names == null || new_indices == null) {
        if (new_names != null) mem::free(new_names);
        if (new_indices != null) mem::free(new_indices);
        gc_set_error(gc, "parser/grammar-oom", "pika/grammar: failed to grow rule table");
        return false;
    }

    for (int i = 0; i < gc.rule_count; i++) {
        new_names[i] = gc.rule_names[i];
        new_indices[i] = gc.rule_indices[i];
    }
    mem::free(gc.rule_names);
    mem::free(gc.rule_indices);
    gc.rule_names = new_names;
    gc.rule_indices = new_indices;
    gc.rule_capacity = new_cap;
    return true;
}

fn int GrammarCompiler.find_rule(GrammarCompiler* self, lisp::SymbolId name) {
    for (int i = 0; i < self.rule_count; i++) {
        if ((uint)self.rule_names[i] == (uint)name) {
            return self.rule_indices[i];
        }
    }
    return -1;
}

fn int GrammarCompiler.add_rule(GrammarCompiler* self, lisp::SymbolId name, Clause clause) {
    char[] name_str = self.interp.symbols.get_name(name);
    int idx = self.rb.add((String)name_str, clause);
    if (!gc_ensure_rule_capacity(self)) return -1;
    self.rule_names[self.rule_count] = name;
    self.rule_indices[self.rule_count] = idx;
    self.rule_count++;
    return idx;
}

fn bool str_eq_c(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}
