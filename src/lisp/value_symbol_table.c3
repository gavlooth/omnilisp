module lisp;

fn void SymbolTable.init(SymbolTable* self) {
    self.count = 0;
    self.capacity = SYMBOL_INITIAL_CAPACITY;
    self.hash_capacity = SYMBOL_HASH_INITIAL_CAPACITY;
    self.entries = (SymbolEntry*)mem::malloc(SymbolEntry.sizeof * self.capacity);
    self.hash_index = (SymbolId*)mem::malloc(SymbolId.sizeof * self.hash_capacity);
    for (usz i = 0; i < self.hash_capacity; i++) {
        self.hash_index[i] = INVALID_SYMBOL_ID;
    }
}

fn void SymbolTable.destroy(SymbolTable* self) {
    for (usz i = 0; i < self.count; i++) {
        if (self.entries[i].name != null) {
            mem::free(self.entries[i].name);
        }
    }
    if (self.entries != null) {
        mem::free(self.entries);
        self.entries = null;
    }
    if (self.hash_index != null) {
        mem::free(self.hash_index);
        self.hash_index = null;
    }
}

fn void SymbolTable.grow(SymbolTable* self) {
    // Double entries capacity
    usz new_cap = self.capacity * 2;
    SymbolEntry* new_entries = (SymbolEntry*)mem::malloc(SymbolEntry.sizeof * new_cap);
    for (usz i = 0; i < self.count; i++) {
        new_entries[i] = self.entries[i];
    }
    mem::free(self.entries);
    self.entries = new_entries;
    self.capacity = new_cap;

    // Double hash capacity and rebuild
    usz new_hash_cap = self.hash_capacity * 2;
    mem::free(self.hash_index);
    self.hash_index = (SymbolId*)mem::malloc(SymbolId.sizeof * new_hash_cap);
    self.hash_capacity = new_hash_cap;
    for (usz i = 0; i < new_hash_cap; i++) {
        self.hash_index[i] = INVALID_SYMBOL_ID;
    }
    // Re-insert all entries into new hash table
    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.count; i++) {
        usz h = hash_symbol(self.entries[i].name[:self.entries[i].len]);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (self.hash_index[slot] == INVALID_SYMBOL_ID) {
                self.hash_index[slot] = (SymbolId)i;
                break;
            }
        }
    }
}

/**
 * hash_symbol — FNV-1a hash for symbol names.
 */
fn usz hash_symbol(char[] name) {
    usz h = 2166136261;  // FNV offset basis (32-bit)
    for (usz i = 0; i < name.len; i++) {
        h ^= (usz)name[i];
        h *= 16777619;   // FNV prime (32-bit)
    }
    return h;
}

fn SymbolId SymbolTable.intern(SymbolTable* self, char[] name) {
    usz h = hash_symbol(name);
    usz mask = self.hash_capacity - 1;

    // Hash table lookup with linear probing
    for (usz probe = 0; probe < self.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        SymbolId sid = self.hash_index[slot];
        if (sid == INVALID_SYMBOL_ID) {
            // Empty slot: symbol not in hash table, stop probing
            break;
        }
        // Check if this slot's entry matches
        usz idx = (usz)sid;
        if (self.entries[idx].len == name.len) {
            bool match = true;
            for (usz j = 0; j < name.len; j++) {
                if (self.entries[idx].name[j] != name[j]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                return sid;
            }
        }
    }

    // Grow at 70% load factor
    if (self.count * 10 >= self.capacity * 7) {
        self.grow();
        mask = self.hash_capacity - 1;
        h = hash_symbol(name);  // recompute with new mask
    }

    // Insert new symbol
    SymbolId id = (SymbolId)self.count;
    usz len = name.len;
    // Heap-allocate symbol name
    char* heap_name = (char*)mem::malloc(len + 1);
    for (usz i = 0; i < len; i++) {
        heap_name[i] = name[i];
    }
    heap_name[len] = 0;
    self.entries[self.count].name = heap_name;
    self.entries[self.count].len = len;
    self.count++;

    // Insert into hash table
    for (usz probe = 0; probe < self.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        if (self.hash_index[slot] == INVALID_SYMBOL_ID) {
            self.hash_index[slot] = id;
            break;
        }
    }

    return id;
}

fn char[] SymbolTable.get_name(SymbolTable* self, SymbolId id) {
    if (self == null || self.entries == null) return "<invalid-symbol>";
    if (id == INVALID_SYMBOL_ID) return "<invalid-symbol>";
    usz idx = (usz)id;
    if (idx >= self.count) return "<invalid-symbol>";
    if (self.entries[idx].name == null) return "<invalid-symbol>";
    return self.entries[idx].name[:(usz)self.entries[idx].len];
}
