module lisp;

import main;

fn bool Env.define(Env* self, SymbolId name, Value* value) @nodiscard {
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
        if (self.capacity > usz.max / 2) {
            return false;
        }
        usz new_cap = self.capacity * 2;
        if (new_cap > usz.max / Binding.sizeof) {
            return false;
        }
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
        (void)self.build_hash_table();
    } else if (self.hash_table != null) {
        // Check load factor: rebuild if > 70%. If the rebuild fails, keep
        // the binding visible by falling back to linear lookup.
        if (self.binding_count > (self.hash_capacity / 10) * 7) {
            if (!self.build_hash_table()) {
                mem::free(self.hash_table);
                self.hash_table = null;
                self.hash_capacity = 0;
            }
        } else {
            self.hash_insert(name, new_idx);
        }
    }
    return true;
}

fn Value* Env.lookup(Env* self, SymbolId name) {
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

fn void? Env.set(Env* self, SymbolId name, Value* value) {
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

