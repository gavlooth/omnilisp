module lisp;

import std::io;

fn Value* eval_path_lookup_root(SymbolId root, Env* env, Interp* interp) {
    Value* current = env.lookup(root);
    if (current != null) return current;
    return interp.global_env.lookup(root);
}

fn bool module_exports_key(Module* mod, SymbolId key) @inline {
    for (usz ei = 0; ei < mod.export_count; ei++) {
        if ((uint)mod.exports[ei] == (uint)key) return true;
    }
    return false;
}

fn EvalResult eval_path_module_step(Value* current, SymbolId key, Interp* interp) {
    Module* mod = current.module_val;
    if (!module_exports_key(mod, key)) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "'%s' is not exported from module",
            (ZString)interp.symbols.get_name(key))) {
            msg = formatted;
        } else {
            msg = "symbol is not exported from module";
        }
        return eval_error(msg);
    }
    Value* next = mod.env.lookup(key);
    if (next == null) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "symbol '%s' not found in module",
            (ZString)interp.symbols.get_name(key))) {
            msg = formatted;
        } else {
            msg = "symbol not found in module";
        }
        return eval_error(msg);
    }
    return eval_ok(next);
}

fn EvalResult eval_path_instance_step(Value* current, SymbolId key, Interp* interp) {
    TypeInfo* ti = interp.types.get(current.instance_val.type_id);
    if (ti != null) {
        for (usz fi = 0; fi < ti.field_count; fi++) {
            if (ti.fields[fi].name == key && fi < current.instance_val.field_count) {
                return eval_ok(&current.instance_val.fields[fi]);
            }
        }
    }
    char[256] ebuf;
    char[] type_n = "<unknown>";
    if (ti != null) type_n = interp.symbols.get_name(ti.name);
    char[] field_n = interp.symbols.get_name(key);
    char[] msg;
    if (try formatted = io::bprintf(&ebuf, "field '%s' not found in type '%s'",
        (ZString)field_n, (ZString)type_n)) {
        msg = formatted;
    } else {
        msg = "field not found in type";
    }
    return eval_error(msg);
}

fn EvalResult eval_path_dict_step(Value* current, SymbolId key, Interp* interp) {
    Value* lookup_key = make_symbol(interp, key);
    Value* found = hashmap_get(current.hashmap_val, lookup_key);
    if (found != null) return eval_ok(found);

    char[256] ebuf;
    char[] seg_n = interp.symbols.get_name(key);
    char[] msg;
    if (try formatted = io::bprintf(&ebuf, "cannot access '.%s' on %s value",
        (ZString)seg_n, (ZString)interp.symbols.get_name(value_type_name(current, interp)))) {
        msg = formatted;
    } else {
        msg = "invalid path access";
    }
    return eval_error(msg);
}

fn bool eval_path_cons_step(Value* current, SymbolId key, Interp* interp, Value** out_next) @inline {
    if (current == null || current.tag != CONS) return false;
    if ((uint)key == (uint)interp.sym_car) {
        *out_next = current.cons_val.car;
        return true;
    }
    if ((uint)key == (uint)interp.sym_cdr) {
        *out_next = current.cons_val.cdr;
        return true;
    }
    return false;
}

fn EvalResult eval_path_invalid_step(Value* current, SymbolId key, Interp* interp) {
    char[256] ebuf;
    char[] seg_n = interp.symbols.get_name(key);
    char[] msg;
    if (try formatted = io::bprintf(&ebuf, "cannot access '.%s' on %s value",
        (ZString)seg_n, (ZString)interp.symbols.get_name(value_type_name(current, interp)))) {
        msg = formatted;
    } else {
        msg = "invalid path access";
    }
    return eval_error(msg);
}

fn EvalResult eval_path_step(Value* current, SymbolId key, Interp* interp) {
    if (current != null && current.tag == MODULE && current.module_val != null) {
        return eval_path_module_step(current, key, interp);
    }
    if (current != null && current.tag == INSTANCE && current.instance_val != null) {
        return eval_path_instance_step(current, key, interp);
    }
    if (current != null && current.tag == HASHMAP && current.hashmap_val != null) {
        return eval_path_dict_step(current, key, interp);
    }

    Value* cons_step = null;
    if (eval_path_cons_step(current, key, interp, &cons_step)) {
        return eval_ok(cons_step);
    }
    return eval_path_invalid_step(current, key, interp);
}

/**
 * Evaluate a path expression: point.x, person.address.city
 * Field access notation.
 *
 * For multi-segment paths like person.address.city:
 * 1. Look up the first segment as a variable
 * 2. For each subsequent segment, resolve it on modules, instances,
 *    dictionaries, or cons `.car`/`.cdr`
 */
fn EvalResult eval_path(Expr* expr, Env* env, Interp* interp) {
    if (expr.path.segment_count == 0) {
        return eval_error("empty field access path (e.g., point.x)");
    }

    // Look up the first segment as a variable
    Value* current = eval_path_lookup_root(expr.path.segments[0], env, interp);
    if (current == null) {
        return eval_error("unbound path root variable");
    }

    // For single-segment paths, just return the value
    if (expr.path.segment_count == 1) {
        return eval_ok(current);
    }

    // For each remaining segment, look up the next path step.
    for (usz i = 1; i < expr.path.segment_count; i++) {
        SymbolId key = expr.path.segments[i];
        EvalResult step = eval_path_step(current, key, interp);
        if (step.error.has_error) return step;
        current = step.value;
    }

    return eval_ok(current);
}
