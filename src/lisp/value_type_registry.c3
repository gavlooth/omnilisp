module lisp;

bool g_type_registry_force_types_alloc_fail = false;
bool g_type_registry_force_hash_alloc_fail = false;
bool g_type_registry_force_grow_types_alloc_fail = false;
bool g_type_registry_force_grow_hash_alloc_fail = false;

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

fn bool TypeRegistry.init(TypeRegistry* self) {
    if (self == null) return false;
    self.type_count = 0;
    self.capacity = 0;
    self.hash_capacity = 0;
    self.types = null;
    self.type_hash_index = null;

    if (TYPE_INITIAL_CAPACITY == 0) return false;
    if (TYPE_INITIAL_CAPACITY > usz.max / 2) return false;
    if (TYPE_INITIAL_CAPACITY > usz.max / TypeInfo.sizeof) return false;
    usz initial_hash_capacity = TYPE_INITIAL_CAPACITY * 2;
    if (initial_hash_capacity == 0 || initial_hash_capacity > usz.max / TypeId.sizeof) return false;

    TypeInfo* types = g_type_registry_force_types_alloc_fail
        ? null
        : (TypeInfo*)mem::malloc(TypeInfo.sizeof * TYPE_INITIAL_CAPACITY);
    if (types == null) return false;

    TypeId* type_hash_index = g_type_registry_force_hash_alloc_fail
        ? null
        : (TypeId*)mem::malloc(TypeId.sizeof * initial_hash_capacity);
    if (type_hash_index == null) {
        mem::free(types);
        return false;
    }

    self.capacity = TYPE_INITIAL_CAPACITY;
    self.hash_capacity = initial_hash_capacity;
    self.types = types;
    self.type_hash_index = type_hash_index;
    for (usz i = 0; i < self.hash_capacity; i++) {
        self.type_hash_index[i] = INVALID_TYPE_ID;
    }
    return true;
}

fn bool TypeRegistry.grow(TypeRegistry* self, SymbolTable* syms) {
    if (self == null || self.types == null || self.type_hash_index == null || syms == null) return false;
    if (self.capacity == 0 || self.capacity > usz.max / (usz)2) return false;
    usz new_cap = self.capacity * 2;
    if (new_cap > usz.max / (usz)2) return false;
    usz new_hash_cap = new_cap * 2;
    if (new_cap > usz.max / TypeInfo.sizeof) return false;
    if (new_hash_cap > usz.max / TypeId.sizeof) return false;
    if (new_cap >= (usz)TypeId.max) return false;

    TypeInfo* new_types = g_type_registry_force_grow_types_alloc_fail
        ? null
        : (TypeInfo*)mem::malloc(TypeInfo.sizeof * new_cap);
    if (new_types == null) return false;
    TypeId* new_hash = g_type_registry_force_grow_hash_alloc_fail
        ? null
        : (TypeId*)mem::malloc(TypeId.sizeof * new_hash_cap);
    if (new_hash == null) {
        mem::free(new_types);
        return false;
    }

    for (usz i = 0; i < self.type_count; i++) new_types[i] = self.types[i];
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = INVALID_TYPE_ID;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.type_count; i++) {
        if (!type_registry_symbol_hashable(syms, new_types[i].name)) continue;
        usz h = type_registry_symbol_hash(syms, new_types[i].name);
        bool placed = false;
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == INVALID_TYPE_ID) {
                new_hash[slot] = (TypeId)i;
                placed = true;
                break;
            }
        }
        if (!placed) {
            mem::free(new_types);
            mem::free(new_hash);
            return false;
        }
    }

    mem::free(self.types);
    mem::free(self.type_hash_index);
    self.types = new_types;
    self.capacity = new_cap;
    self.type_hash_index = new_hash;
    self.hash_capacity = new_hash_cap;
    return true;
}

fn void TypeRegistry.destroy(TypeRegistry* self) {
    if (self.types != null) {
        for (usz i = 0; i < self.type_count; i++) type_registry_destroy_type_info(&self.types[i]);
    }
    if (self.types != null) { mem::free(self.types); self.types = null; }
    if (self.type_hash_index != null) { mem::free(self.type_hash_index); self.type_hash_index = null; }
}

fn TypeId TypeRegistry.register_type(TypeRegistry* self, TypeInfo info, SymbolTable* syms) {
    if (self == null || self.types == null || self.type_hash_index == null) return INVALID_TYPE_ID;
    if (self.capacity == 0 || self.hash_capacity == 0) return INVALID_TYPE_ID;

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
        if (!self.grow(syms)) return INVALID_TYPE_ID;
    }
    if (self.type_count >= (usz)INVALID_TYPE_ID) return INVALID_TYPE_ID;
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

fn void TypeRegistry.rebuild_hash(TypeRegistry* self, SymbolTable* syms) {
    if (self == null || syms == null || self.types == null || self.type_hash_index == null) return;
    if (self.hash_capacity == 0) return;
    for (usz i = 0; i < self.hash_capacity; i++) self.type_hash_index[i] = INVALID_TYPE_ID;

    usz mask = self.hash_capacity - 1;
    for (usz i = 0; i < self.type_count; i++) {
        if (!type_registry_symbol_hashable(syms, self.types[i].name)) continue;
        usz h = type_registry_symbol_hash(syms, self.types[i].name);
        for (usz probe = 0; probe < self.hash_capacity; probe++) {
            usz slot = (h + probe) & mask;
            if (self.type_hash_index[slot] == INVALID_TYPE_ID) {
                self.type_hash_index[slot] = (TypeId)i;
                break;
            }
        }
    }
}

fn void TypeRegistry.rollback_recent_new_type(TypeRegistry* self, TypeId id, SymbolTable* syms) {
    if (self == null || syms == null || self.types == null || self.type_hash_index == null) return;
    if (id == INVALID_TYPE_ID || self.type_count == 0) return;
    if ((usz)id + (usz)1 != self.type_count) return;

    type_registry_destroy_type_info(&self.types[(usz)id]);
    self.types[(usz)id] = {};
    self.type_count--;
    self.rebuild_hash(syms);
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

fn TypeId TypeRegistry.resolve_alias(TypeRegistry* self, TypeId id) {
    TypeId current = id;
    for (usz depth = 0; depth < 16; depth++) {
        TypeInfo* info = self.get(current);
        if (info == null) return INVALID_TYPE_ID;
        if (info.kind != TK_ALIAS || info.alias_target == INVALID_TYPE_ID) return current;
        current = info.alias_target;
    }
    return current;
}

fn bool TypeRegistry.is_subtype(TypeRegistry* self, TypeId child, TypeId parent) {
    child = self.resolve_alias(child);
    parent = self.resolve_alias(parent);
    if (child == INVALID_TYPE_ID || parent == INVALID_TYPE_ID) return false;
    if (child == parent) return true;
    // Walk parent chain (depth limit 16)
    TypeId current = child;
    for (usz depth = 0; depth < 16; depth++) {
        TypeInfo* info = self.get(current);
        if (info == null) return false;
        if (info.kind == TK_ALIAS && info.alias_target != INVALID_TYPE_ID) {
            current = info.alias_target;
            if (current == parent) return true;
            continue;
        }
        if (info.parent == INVALID_TYPE_ID) return false;
        if (info.parent == parent) return true;
        current = info.parent;
    }
    return false;
}
