module lisp;

import std::io;
import main;

fn bool Env.define(Env* self, SymbolId name, Value* value) @nodiscard {
    if (self == null || name == INVALID_SYMBOL_ID) return false;
    // Fast path: use hash table if available
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            self.bindings[idx].value = value;
            return true;
        }
    } else {
        // Linear scan for small frames
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                self.bindings[i].value = value;
                return true;
            }
        }
    }
    // Grow if needed
    if (self.binding_count >= self.capacity) {
        if (self.capacity > 0 && self.capacity > usz.max / 2) return false;
        usz new_cap = self.capacity > 0 ? self.capacity * 2 : ENV_INITIAL_CAPACITY;
        if (new_cap > usz.max / Binding.sizeof) return false;
        Binding* new_bindings = g_env_force_bindings_alloc_oom
            ? null
            : (Binding*)mem::malloc(Binding.sizeof * new_cap);
        if (new_bindings == null) return false;
        for (usz i = 0; i < self.binding_count; i++) {
            new_bindings[i] = self.bindings[i];
        }
        if (!self.is_inline) {
            mem::free(self.bindings);
        }
        self.is_inline = false;
        self.bindings = new_bindings;
        self.capacity = new_cap;
    }
    usz new_idx = self.binding_count;
    self.bindings[new_idx].name = name;
    self.bindings[new_idx].value = value;
    self.binding_count++;

    // Build hash table if we crossed the threshold
    if (self.hash_table == null && self.binding_count > ENV_HASH_THRESHOLD) {
        if (!self.build_hash_table()) {
            io::eprintfn("WARNING: env hash table build failed; falling back to linear lookup");
        }
    } else if (self.hash_table != null) {
        // Check load factor: rebuild if > 70%
        bool rebuild_hash = self.binding_count > usz.max / (usz)10
            || self.hash_capacity > usz.max / (usz)7
            || self.binding_count * (usz)10 > self.hash_capacity * (usz)7;
        if (rebuild_hash) {
            if (!self.build_hash_table()) {
                io::eprintfn("WARNING: env hash table rebuild failed; falling back to linear lookup");
            }
        } else {
            self.hash_insert(name, new_idx);
        }
    }
    return true;
}

fn Value* Env.lookup(Env* self, SymbolId name) {
    if (self == null || name == INVALID_SYMBOL_ID) return null;
    // Fast path: use hash table if available
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            return self.bindings[idx].value;
        }
    } else {
        // Linear scan for small frames
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                return self.bindings[i].value;
            }
        }
    }
    // Search parent
    if (self.parent != null) {
        return self.parent.lookup(name);
    }
    return null;  // Not found
}

fn Value* Env.lookup_local(Env* self, SymbolId name) {
    if (self == null || name == INVALID_SYMBOL_ID) return null;
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) return self.bindings[idx].value;
        return null;
    }
    for (usz i = 0; i < self.binding_count; i++) {
        if (self.bindings[i].name == name) return self.bindings[i].value;
    }
    return null;
}

fn void? Env.set(Env* self, SymbolId name, Value* value) {
    if (self == null || name == INVALID_SYMBOL_ID) return UNBOUND_VARIABLE~;
    if (self.hash_table != null) {
        usz idx = self.hash_lookup(name);
        if (idx != ~(usz)0) {
            self.bindings[idx].value = value;
            return;
        }
    } else {
        for (usz i = 0; i < self.binding_count; i++) {
            if (self.bindings[i].name == name) {
                self.bindings[i].value = value;
                return;
            }
        }
    }
    if (self.parent != null) return self.parent.set(name, value);
    return UNBOUND_VARIABLE~;
}

fn Env* Env.extend(Env* self, Interp* interp, SymbolId name, Value* value) {
    Env* new_env = make_env(interp, self);
    if (!new_env.define(name, value)) return null;
    return new_env;
}
