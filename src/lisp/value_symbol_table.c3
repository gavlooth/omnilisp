module lisp;

const SYMBOL_MAX_NAME_LEN = 8192;

bool g_symbol_table_force_entries_alloc_fail = false;
bool g_symbol_table_force_hash_alloc_fail = false;
bool g_symbol_table_force_grow_entries_alloc_fail = false;
bool g_symbol_table_force_grow_hash_alloc_fail = false;
bool g_symbol_table_force_name_alloc_fail = false;

fn void SymbolTable.init(SymbolTable* self) {
    self.count = 0;
    self.capacity = 0;
    self.hash_capacity = 0;
    self.entries = null;
    self.hash_index = null;

    SymbolEntry* entries = g_symbol_table_force_entries_alloc_fail
        ? null
        : (SymbolEntry*)mem::malloc(SymbolEntry.sizeof * SYMBOL_INITIAL_CAPACITY);
    if (entries == null) return;

    SymbolId* hash_index = g_symbol_table_force_hash_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(SymbolId.sizeof * SYMBOL_HASH_INITIAL_CAPACITY);
    if (hash_index == null) {
        mem::free(entries);
        return;
    }

    self.capacity = SYMBOL_INITIAL_CAPACITY;
    self.hash_capacity = SYMBOL_HASH_INITIAL_CAPACITY;
    self.entries = entries;
    self.hash_index = hash_index;
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

fn bool SymbolTable.grow(SymbolTable* self) {
    if (self == null || self.entries == null || self.hash_index == null) return false;
    if (self.capacity == 0 || self.hash_capacity == 0) return false;
    if (self.capacity > usz.max / 2 || self.hash_capacity > usz.max / 2) return false;

    usz new_cap = self.capacity * 2;
    usz new_hash_cap = self.hash_capacity * 2;

    SymbolEntry* new_entries = g_symbol_table_force_grow_entries_alloc_fail
        ? null
        : (SymbolEntry*)mem::malloc(SymbolEntry.sizeof * new_cap);
    if (new_entries == null) return false;

    SymbolId* new_hash_index = g_symbol_table_force_grow_hash_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(SymbolId.sizeof * new_hash_cap);
    if (new_hash_index == null) {
        mem::free(new_entries);
        return false;
    }

    for (usz i = 0; i < self.count; i++) {
        new_entries[i] = self.entries[i];
    }
    for (usz i = 0; i < new_hash_cap; i++) {
        new_hash_index[i] = INVALID_SYMBOL_ID;
    }

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.count; i++) {
        usz h = hash_symbol(new_entries[i].name[:new_entries[i].len]);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash_index[slot] == INVALID_SYMBOL_ID) {
                new_hash_index[slot] = (SymbolId)i;
                break;
            }
        }
    }

    mem::free(self.entries);
    mem::free(self.hash_index);
    self.entries = new_entries;
    self.hash_index = new_hash_index;
    self.capacity = new_cap;
    self.hash_capacity = new_hash_cap;
    return true;
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
    if (self == null || self.entries == null || self.hash_index == null) {
        return INVALID_SYMBOL_ID;
    }
    if (self.capacity == 0 || self.hash_capacity == 0) {
        return INVALID_SYMBOL_ID;
    }
    if (name.ptr == null) {
        return INVALID_SYMBOL_ID;
    }

    // Defensive guardrail: symbol names beyond documented limits indicate a
    // corrupted slice/ABI boundary and must not flow into allocator paths.
    usz len = name.len;
    if (len > SYMBOL_MAX_NAME_LEN) {
        return INVALID_SYMBOL_ID;
    }

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
        if (self.entries[idx].len == len) {
            bool match = true;
            for (usz j = 0; j < len; j++) {
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
        if (!self.grow()) return INVALID_SYMBOL_ID;
        mask = self.hash_capacity - 1;
        h = hash_symbol(name);  // recompute with new mask
    }

    // Insert new symbol
    SymbolId id = (SymbolId)self.count;
    // Heap-allocate symbol name
    char* heap_name = g_symbol_table_force_name_alloc_fail
        ? null
        : (char*)mem::malloc(len + 1);
    if (heap_name == null) return INVALID_SYMBOL_ID;
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
