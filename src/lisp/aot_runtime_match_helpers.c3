module lisp::aot;

import lisp;

fn bool is_truthy(lisp::Value* v) @inline {
    if (v == null) return false;
    if (v.tag == lisp::ValueTag.NIL) return false;
    lisp::Interp* interp = aot_dispatch_interp();
    if (interp != null && v.tag == lisp::ValueTag.SYMBOL && v.sym_val == interp.sym_false) return false;
    return true;
}

fn bool is_error(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.ERROR;
}

fn bool guard_pred_accepts_scrutinee(lisp::Value* pred) @inline {
    if (pred == null) return false;
    switch (pred.tag) {
        case lisp::ValueTag.CLOSURE:
        case lisp::ValueTag.PRIMITIVE:
        case lisp::ValueTag.PARTIAL_PRIM:
        case lisp::ValueTag.CONTINUATION:
        case lisp::ValueTag.METHOD_TABLE:
            return true;
        default:
            return false;
    }
}

fn lisp::Value* match_guard_eval(lisp::Value* pred, lisp::Value* val) {
    if (pred == null || is_error(pred)) return pred;
    if (!guard_pred_accepts_scrutinee(pred)) return pred;
    lisp::Value* result = invoke(pred, val);
    if (result == null) {
        lisp::Interp* interp = aot_bridge_interp();
        if (interp == null) return aot_make_error_best_effort(null, "aot match-guard-eval: runtime interpreter unavailable");
        return lisp::make_nil(interp);
    }
    return result;
}

fn bool values_equal(lisp::Value* a, lisp::Value* b) {
    return lisp::values_equal(a, b);
}

fn bool is_cons(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.CONS;
}

fn bool is_dict(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.HASHMAP && v.hashmap_val != null;
}

fn bool match_var_pattern(lisp::Value* v, char[] name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return false;
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return false;
    lisp::TypeId tid = interp.types.lookup(sid, &interp.symbols);
    if (tid == lisp::INVALID_TYPE_ID) return true;

    lisp::TypeInfo* ti = interp.types.get(tid);
    if (ti == null || (ti.kind != TK_CONCRETE && ti.kind != TK_UNION) || ti.field_count != 0) {
        return true;
    }
    return v != null && v.tag == lisp::ValueTag.INSTANCE && v.instance_val != null && v.instance_val.type_id == tid;
}

fn bool match_constructor(lisp::Value* v, char[] ctor_name, long field_count) {
    if (field_count < 0) return false;
    if (v == null || v.tag != lisp::ValueTag.INSTANCE || v.instance_val == null) return false;

    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return false;
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, ctor_name, &sid)) return false;
    lisp::TypeId tid = interp.types.lookup(sid, &interp.symbols);
    if (tid == lisp::INVALID_TYPE_ID || v.instance_val.type_id != tid) return false;
    return v.instance_val.field_count == (usz)field_count;
}

fn lisp::Value* instance_field(lisp::Value* v, long idx) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot instance-field: runtime interpreter unavailable");
    if (idx < 0) return lisp::make_nil(interp);
    if (v == null || v.tag != lisp::ValueTag.INSTANCE || v.instance_val == null) return lisp::make_nil(interp);
    usz field_index = (usz)idx;
    if (field_index >= v.instance_val.field_count) return lisp::make_nil(interp);
    return &v.instance_val.fields[field_index];
}

fn lisp::Value* dict_get_symbol(lisp::Value* v, char[] key) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot dict-get-symbol: runtime interpreter unavailable");
    if (!is_dict(v)) return lisp::make_nil(interp);
    lisp::SymbolId key_sym = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, key, &key_sym)) return aot_make_error_best_effort(interp, "aot dict-get-symbol: invalid or uninternable key");
    lisp::Value* lookup_key = lisp::make_symbol(interp, key_sym);
    lisp::Value* found = lisp::hashmap_get(v.hashmap_val, lookup_key);
    return found != null ? found : lisp::make_nil(interp);
}

fn usz seq_length(lisp::Value* v) {
    if (v != null && v.tag == lisp::ValueTag.ARRAY && v.array_val != null) {
        return v.array_val.length;
    }
    return lisp::get_list_length(v);
}

fn lisp::Value* seq_nth(lisp::Value* v, long n) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot seq-nth: runtime interpreter unavailable");
    if (n < 0) return lisp::make_nil(interp);
    if (v != null && v.tag == lisp::ValueTag.ARRAY && v.array_val != null) {
        usz idx = (usz)n;
        if (idx >= v.array_val.length) return lisp::make_nil(interp);
        return v.array_val.items[idx];
    }
    lisp::Value* out = lisp::get_list_nth(v, (usz)n);
    return out != null ? out : lisp::make_nil(interp);
}

fn lisp::Value* seq_rest(lisp::Value* v, long n) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot seq-rest: runtime interpreter unavailable");
    if (n < 0) return lisp::make_nil(interp);
    return lisp::get_list_rest(v, (usz)n, interp);
}

fn bool is_list(lisp::Value* v) {
    return lisp::is_list(v);
}

fn usz list_length(lisp::Value* v) {
    return lisp::list_length(v);
}

fn lisp::Value* list_nth(lisp::Value* v, long n) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot list-nth: runtime interpreter unavailable");
    if (n < 0) return lisp::make_nil(interp);
    return lisp::get_list_nth(v, (usz)n);
}

fn lisp::Value* list_rest(lisp::Value* v, long n) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot list-rest: runtime interpreter unavailable");
    if (n < 0) return lisp::make_nil(interp);
    return lisp::get_list_rest(v, (usz)n, interp);
}
