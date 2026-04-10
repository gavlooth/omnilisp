module lisp;

import std::collections::list;

struct BindingEntry {
    SymbolId name;
    Value* value;
}

struct Bindings {
    List{BindingEntry} entries;
}

fn void bindings_init(Bindings* b) {
    *b = {};
}

fn void bindings_deinit(Bindings* b) {
    b.entries.free();
}

fn Value* bindings_lookup(Bindings* b, SymbolId vname) {
    for (usz i = 0; i < b.entries.len(); i++) {
        if ((uint)b.entries[i].name == (uint)vname) return b.entries[i].value;
    }
    return null;
}

fn void? bindings_bind(Bindings* b, SymbolId vname, Value* val) {
    // Check if already bound
    Value* existing = bindings_lookup(b, vname);
    if (existing != null) {
        // Already bound — check consistency
        if (!deduce_val_eq(existing, val)) return TYPE_MISMATCH~;
        return;
    }
    b.entries.push({ .name = vname, .value = val });
}

fn bool deduce_val_eq(Value* a, Value* b) {
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;
    if (a.tag != b.tag) return false;
    switch (a.tag) {
        case INT: return a.int_val == b.int_val;
        case DOUBLE: return a.double_val == b.double_val;
        case STRING: {
            if (a.str_len != b.str_len) return false;
            for (usz i = 0; i < a.str_len; i++) {
                if (a.str_chars[i] != b.str_chars[i]) return false;
            }
            return true;
        }
        case SYMBOL: return a.sym_val == b.sym_val;
        case NIL: return true;
        case VOID: return true;
        default: return a == b;  // pointer equality for other types
    }
}

fn bool is_logic_var(Value* v, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] name = interp.symbols.get_name(v.sym_val);
    return name.len > 0 && name[0] == '?';
}

fn bool is_wildcard(Value* v, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] name = interp.symbols.get_name(v.sym_val);
    return name.len == 1 && name[0] == '_';
}

fn uint bindings_result_dict_capacity(usz binding_count) {
    uint cap = 8;
    usz target = binding_count * 2;
    if (target < binding_count) target = binding_count;
    while ((usz)cap < target && cap <= uint.max / 2) {
        cap *= 2;
    }
    return cap;
}

fn void? match_clause_against_row(
    Value* clause,       // list: (rel-name arg1 arg2 ...)
    Value* row,          // dict from deduce-scan
    Relation* rel,       // relation for column names
    Bindings* bindings,
    Interp* interp
) {
    // Skip relation name (first element)
    Value* args = cdr(clause);

    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        // Get column value from row dict
        Value* key = interp.alloc_value();
        key.tag = SYMBOL;
        key.sym_val = rel.columns[col];
        Value* col_val = hashmap_get(row.hashmap_val, key);

        if (is_wildcard(pattern_elem, interp)) {
            // _ matches anything
        } else if (is_logic_var(pattern_elem, interp)) {
            // ?var — bind or check consistency
            if (catch err = bindings_bind(bindings, pattern_elem.sym_val, col_val)) {
                return TYPE_MISMATCH~;  // inconsistent binding
            }
        } else {
            // Literal — must match exactly
            if (!deduce_val_eq(pattern_elem, col_val)) {
                return TYPE_MISMATCH~;
            }
        }
        args = cdr(args);
    }
}

fn void? match_clause_against_tuple(
    Value* clause,         // list: (rel-name arg1 arg2 ...)
    Value*[] tuple_vals,   // decoded tuple values in relation column order
    Relation* rel,
    Bindings* bindings,
    Interp* interp
) {
    Value* args = cdr(clause);

    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        Value* col_val = col < tuple_vals.len ? tuple_vals[col] : make_nil(interp);

        if (is_wildcard(pattern_elem, interp)) {
            // _ matches anything
        } else if (is_logic_var(pattern_elem, interp)) {
            if (catch err = bindings_bind(bindings, pattern_elem.sym_val, col_val)) {
                return TYPE_MISMATCH~;
            }
        } else {
            if (!deduce_val_eq(pattern_elem, col_val)) {
                return TYPE_MISMATCH~;
            }
        }
        args = cdr(args);
    }
}

fn Value* build_result_dict(Bindings* bindings, Value* projection, Interp* interp) {
    usz binding_count = bindings.entries.len();
    uint dict_capacity = bindings_result_dict_capacity(binding_count);
    Value* dict = make_hashmap_checked(interp, dict_capacity, "deduce-match: out of memory");
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) {
        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
    }

    if (projection == null || projection.tag == NIL) {
        // Return all bindings
        for (usz i = 0; i < binding_count; i++) {
            // Strip ? from variable name
            char[] name = interp.symbols.get_name(bindings.entries[i].name);
            SymbolId stripped;
            if (name.len > 1 && name[0] == '?') {
                stripped = interp.symbols.intern(name[1..]);
            } else {
                stripped = bindings.entries[i].name;
            }
            Value* key = interp.alloc_value();
            if (key == null) {
                return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
            }
            key.tag = SYMBOL;
            key.sym_val = stripped;
            if (!hashmap_set_checked(dict.hashmap_val, key, bindings.entries[i].value, interp)) {
                return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
            }
        }
    } else {
        // Return only projected variables
        Value* proj = projection;
        while (is_cons(proj)) {
            Value* pvar = car(proj);
            if (pvar.tag == SYMBOL) {
                Value* val = bindings_lookup(bindings, pvar.sym_val);
                if (val != null) {
                    char[] name = interp.symbols.get_name(pvar.sym_val);
                    SymbolId stripped;
                    if (name.len > 1 && name[0] == '?') {
                        stripped = interp.symbols.intern(name[1..]);
                    } else {
                        stripped = pvar.sym_val;
                    }
                    Value* key = interp.alloc_value();
                    if (key == null) {
                        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
                    }
                    key.tag = SYMBOL;
                    key.sym_val = stripped;
                    if (!hashmap_set_checked(dict.hashmap_val, key, val, interp)) {
                        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
                    }
                }
            }
            proj = cdr(proj);
        }
    }

    return dict;
}
