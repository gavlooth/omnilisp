module lisp;

fn void TypeRegistry.init(TypeRegistry* self) {
    self.capacity = TYPE_INITIAL_CAPACITY;
    self.type_count = 0;
    self.types = (TypeInfo*)mem::malloc(TypeInfo.sizeof * self.capacity);
    self.hash_capacity = self.capacity * 2;
    self.type_hash_index = (TypeId*)mem::malloc(TypeId.sizeof * self.hash_capacity);
    for (usz i = 0; i < self.hash_capacity; i++) {
        self.type_hash_index[i] = INVALID_TYPE_ID;
    }
}

fn void TypeRegistry.grow(TypeRegistry* self, SymbolTable* syms) {
    usz new_cap = self.capacity * 2;
    TypeInfo* new_types = (TypeInfo*)mem::malloc(TypeInfo.sizeof * new_cap);
    for (usz i = 0; i < self.type_count; i++) new_types[i] = self.types[i];
    mem::free(self.types);
    self.types = new_types;
    self.capacity = new_cap;

    // Rebuild hash table at 2x new capacity
    usz new_hash_cap = new_cap * 2;
    TypeId* new_hash = (TypeId*)mem::malloc(TypeId.sizeof * new_hash_cap);
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = INVALID_TYPE_ID;
    mem::free(self.type_hash_index);
    self.type_hash_index = new_hash;
    self.hash_capacity = new_hash_cap;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.type_count; i++) {
        char[] name_str = syms.get_name(self.types[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == INVALID_TYPE_ID) {
                new_hash[slot] = (TypeId)i;
                break;
            }
        }
    }
}

fn void TypeRegistry.destroy(TypeRegistry* self) {
    if (self.types != null) { mem::free(self.types); self.types = null; }
    if (self.type_hash_index != null) { mem::free(self.type_hash_index); self.type_hash_index = null; }
}

fn TypeId TypeRegistry.register_type(TypeRegistry* self, TypeInfo info, SymbolTable* syms) {
    if (self.type_count >= self.capacity) {
        self.grow(syms);
    }
    TypeId id = (TypeId)self.type_count;
    self.types[self.type_count] = info;
    self.type_count++;

    // Insert into hash table
    char[] name = syms.get_name(info.name);
    usz h = hash_symbol(name);
    usz mask = self.hash_capacity - 1;
    for (usz probe = 0; probe < self.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        if (self.type_hash_index[slot] == INVALID_TYPE_ID) {
            self.type_hash_index[slot] = id;
            break;
        }
    }
    return id;
}

fn TypeId TypeRegistry.lookup(TypeRegistry* self, SymbolId name, SymbolTable* syms) {
    char[] name_str = syms.get_name(name);
    usz h = hash_symbol(name_str);
    usz mask = self.hash_capacity - 1;
    for (usz probe = 0; probe < self.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        TypeId tid = self.type_hash_index[slot];
        if (tid == INVALID_TYPE_ID) return INVALID_TYPE_ID;
        if (self.types[(usz)tid].name == name) return tid;
    }
    return INVALID_TYPE_ID;
}

fn TypeInfo* TypeRegistry.get(TypeRegistry* self, TypeId id) {
    if (id == INVALID_TYPE_ID || (usz)id >= self.type_count) return null;
    return &self.types[(usz)id];
}

fn bool TypeRegistry.is_subtype(TypeRegistry* self, TypeId child, TypeId parent) {
    if (child == parent) return true;
    // Walk parent chain (depth limit 16)
    TypeId current = child;
    for (usz depth = 0; depth < 16; depth++) {
        TypeInfo* info = self.get(current);
        if (info == null) return false;
        if (info.parent == INVALID_TYPE_ID) return false;
        if (info.parent == parent) return true;
        current = info.parent;
    }
    return false;
}
