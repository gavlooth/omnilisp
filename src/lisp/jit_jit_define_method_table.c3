module lisp;

import std::core::mem;

fn bool jit_val_literals_equal(ValueLiteralKey* a, ValueLiteralKey* b) @inline {
    if (a.tag != b.tag) return false;
    switch (a.tag) {
        case INT:
            return a.int_value == b.int_value;
        case SYMBOL:
        case STRING:
            return (uint)a.sym_value == (uint)b.sym_value;
        default:
            return a.int_value == b.int_value && (uint)a.sym_value == (uint)b.sym_value;
    }
}

fn Value* jit_make_method_table_value(Interp* interp, MethodTable* mt) {
    Value* mt_val = boundary_alloc_value_in_root(interp, true);
    if (mt_val == null) return null;
    mt_val.tag = METHOD_TABLE;
    mt_val.method_table_val = mt;
    return mt_val;
}

fn bool jit_method_sigs_equal(MethodSignature* a, MethodSignature* b) @inline {
    if (a.param_count != b.param_count) return false;
    if (a.constraint_count != b.constraint_count) return false;
    for (usz i = 0; i < a.param_count; i++) {
        if (a.param_types[i] != b.param_types[i]) return false;
        if ((uint)a.param_type_vars[i] != (uint)b.param_type_vars[i]) return false;
        if (a.has_val_literal[i] != b.has_val_literal[i]) return false;
        if (a.has_val_literal[i] && !jit_val_literals_equal(&a.val_literals[i], &b.val_literals[i])) return false;
    }
    for (usz i = 0; i < a.constraint_count; i++) {
        if (a.constraints[i].param_sym != b.constraints[i].param_sym) return false;
        if (a.constraints[i].bound_type != b.constraints[i].bound_type) return false;
    }
    return true;
}

fn bool jit_method_table_append_entry_with_sig(MethodTable* mt, MethodSignature* new_sig, Value* impl) {
    // Replace existing entry with same signature (idempotent re-define).
    for (usz i = 0; i < mt.entry_count; i++) {
        if (jit_method_sigs_equal(&mt.entries[i].sig, new_sig)) {
            mt.entries[i].implementation = impl;
            return true;
        }
    }
    if (mt.entry_count >= mt.capacity) {
        usz new_cap = mt.capacity * 2;
        MethodEntry* new_entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * new_cap);
        if (new_entries == null) return false;
        for (usz ei = 0; ei < mt.entry_count; ei++) new_entries[ei] = mt.entries[ei];
        if (!method_signature_copy_to_heap(&new_entries[mt.entry_count].sig, new_sig)) {
            mem::free(new_entries);
            return false;
        }
        mem::free(mt.entries);
        mt.entries = new_entries;
        mt.capacity = new_cap;
        mt.entries[mt.entry_count].implementation = impl;
        mt.entry_count++;
        return true;
    }
    if (!method_signature_copy_to_heap(&mt.entries[mt.entry_count].sig, new_sig)) return false;
    mt.entries[mt.entry_count].implementation = impl;
    mt.entry_count++;
    return true;
}

fn bool jit_method_table_append_entry(MethodTable* mt, Value* impl) {
    MethodSignature* new_sig = impl.closure_val.type_sig;
    return jit_method_table_append_entry_with_sig(mt, new_sig, impl);
}

fn MethodTable* jit_new_method_table_with_sig(SymbolId name, MethodSignature* sig, Value* impl, Value* fallback) {
    MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (mt == null) return null;
    mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * METHOD_INITIAL_CAPACITY);
    if (mt.entries == null) {
        mem::free(mt);
        return null;
    }
    mt.name = name;
    mt.capacity = METHOD_INITIAL_CAPACITY;
    mt.entry_count = 1;
    mt.fallback = fallback;
    if (!method_signature_copy_to_heap(&mt.entries[0].sig, sig)) {
        mem::free(mt.entries);
        mem::free(mt);
        return null;
    }
    mt.entries[0].implementation = impl;
    return mt;
}

fn MethodTable* jit_new_method_table(SymbolId name, Value* impl, Value* fallback) {
    return jit_new_method_table_with_sig(name, impl.closure_val.type_sig, impl, fallback);
}

fn Value* jit_eval_define_typed_callable(Interp* interp, SymbolId name, Value* stored_val, MethodSignature* sig) {
    Value* existing = interp.global_env.lookup(name);
    if (existing != null && existing.tag == METHOD_TABLE) {
        if (!jit_method_table_append_entry_with_sig(existing.method_table_val, sig, stored_val)) {
            return raise_error(interp, "jit: failed to extend method table");
        }
        return existing;
    }
    Value* fallback = null;
    if (existing != null && (existing.tag == CLOSURE || existing.tag == PRIMITIVE)) {
        fallback = existing;
    }
    MethodTable* mt = jit_new_method_table_with_sig(name, sig, stored_val, fallback);
    if (mt == null) return raise_error(interp, "jit: failed to allocate method table");
    Value* mt_val = jit_make_method_table_value(interp, mt);
    if (mt_val == null) return raise_error(interp, "jit: failed to allocate method table value");
    if (env_define_with_barrier_result(interp.global_env, name, mt_val, interp) != null) {
        return raise_error(interp, "jit: failed to publish method table");
    }
    return mt_val;
}

fn Value* jit_eval_define_typed_closure(Interp* interp, SymbolId name, Value* stored_val) {
    return jit_eval_define_typed_callable(interp, name, stored_val, stored_val.closure_val.type_sig);
}

<* @require interp != null : "jit_eval_define: null interp"
   @require value != null : "jit_eval_define: null value" *>
fn Value* jit_eval_define(Interp* interp, SymbolId name, Value* value) {
    // Promote to root scope/region for globals (so value survives scope release).
    Value* stored_val = boundary_promote_to_root_site(
        value,
        interp,
        COPY_SITE_JIT_EVAL_DEFINE
    );
    if (stored_val != null && stored_val.tag == ERROR) return stored_val;

    // Check for typed closure → method table dispatch
    if (stored_val != null && stored_val.tag == CLOSURE && stored_val.closure_val.has_typed_params) {
        return jit_eval_define_typed_closure(interp, name, stored_val);
    }

    // Check if we're adding an untyped fallback to an existing method table
    Value* existing_for_fallback = interp.global_env.lookup(name);
    if (existing_for_fallback != null && existing_for_fallback.tag == METHOD_TABLE) {
        existing_for_fallback.method_table_val.fallback = stored_val;
        return existing_for_fallback;
    }

    // Tag closure with its bound name for display
    if (stored_val != null && stored_val.tag == CLOSURE) {
        stored_val.closure_val.name = name;
    }

    // Normal (untyped) define
    if (env_define_with_barrier_result(interp.global_env, name, stored_val, interp) != null) {
        return raise_error(interp, "jit: failed to define binding");
    }
    return stored_val;
}
