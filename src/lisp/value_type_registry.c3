module lisp;

fn void type_registry_destroy_union_variants(UnionVariant* variants, usz count) {
    if (variants == null) return;
    for (usz i = 0; i < count; i++) {
        if (variants[i].fields != null) mem::free(variants[i].fields);
    }
    mem::free(variants);
}

fn void type_registry_destroy_type_info(TypeInfo* info) {
    if (info == null) return;
    if (info.fields != null) mem::free(info.fields);
    if (info.type_params != null) mem::free(info.type_params);
    if (info.constraints != null) mem::free(info.constraints);
    type_registry_destroy_union_variants(info.variants, info.variant_count);
    info.fields = null;
    info.type_params = null;
    info.constraints = null;
    info.variants = null;
    info.field_count = 0;
    info.type_param_count = 0;
    info.constraint_count = 0;
    info.variant_count = 0;
}

fn bool type_registry_symbol_valid(SymbolTable* syms, SymbolId id) @inline {
    if (syms == null || syms.entries == null) return false;
    if (id == INVALID_SYMBOL_ID) return false;
    return (usz)id < syms.count;
}

fn bool type_registry_symbol_hashable(SymbolTable* syms, SymbolId id) @inline {
    if (!type_registry_symbol_valid(syms, id)) return false;
    SymbolEntry* entry = &syms.entries[(usz)id];
    return entry.name != null;
}

fn usz type_registry_symbol_hash(SymbolTable* syms, SymbolId id) @inline {
    SymbolEntry* entry = &syms.entries[(usz)id];
    usz h = 2166136261;  // FNV-1a offset basis (32-bit)
    for (usz i = 0; i < (usz)entry.len; i++) {
        h ^= (usz)entry.name[i];
        h *= 16777619;   // FNV-1a prime (32-bit)
    }
    return h;
}

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
        if (!type_registry_symbol_hashable(syms, self.types[i].name)) continue;
        usz h = type_registry_symbol_hash(syms, self.types[i].name);
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
    if (self.types != null) {
        for (usz i = 0; i < self.type_count; i++) type_registry_destroy_type_info(&self.types[i]);
    }
    if (self.types != null) { mem::free(self.types); self.types = null; }
    if (self.type_hash_index != null) { mem::free(self.type_hash_index); self.type_hash_index = null; }
}

fn TypeId TypeRegistry.register_type(TypeRegistry* self, TypeInfo info, SymbolTable* syms) {
    // If a type with this name already exists, update it in-place and return the existing TypeId.
    // This ensures idempotent re-registration (e.g., from (load ...) re-evaluating define forms).
    if (type_registry_symbol_hashable(syms, info.name)) {
        TypeId existing = self.lookup(info.name, syms);
        if (existing != INVALID_TYPE_ID) {
            type_registry_destroy_type_info(&self.types[(usz)existing]);
            self.types[(usz)existing] = info;
            return existing;
        }
    }

    if (self.type_count >= self.capacity) {
        self.grow(syms);
    }
    TypeId id = (TypeId)self.type_count;
    self.types[self.type_count] = info;
    self.type_count++;

    // Insert into hash table
    if (!type_registry_symbol_hashable(syms, info.name)) return id;
    usz h = type_registry_symbol_hash(syms, info.name);
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
    if (self == null || syms == null) return INVALID_TYPE_ID;
    if (self.hash_capacity == 0 || self.type_hash_index == null || self.types == null) {
        return INVALID_TYPE_ID;
    }
    if (!type_registry_symbol_hashable(syms, name)) return INVALID_TYPE_ID;
    usz h = type_registry_symbol_hash(syms, name);
    usz mask = self.hash_capacity - 1;
    for (usz probe = 0; probe < self.hash_capacity; probe++) {
        usz slot = (h + probe) & mask;
        TypeId tid = self.type_hash_index[slot];
        if (tid == INVALID_TYPE_ID) return INVALID_TYPE_ID;
        if ((usz)tid >= self.type_count) continue;
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
