module lisp;

import std::io;

fn bool jit_try_index_list(Interp* interp, Value* collection, Value* index, Value** out) {
    if (!is_cons(collection) || !is_int(index)) return false;

    long idx = index.int_val;
    if (idx < 0) {
        long len = 0;
        Value* c = collection;
        while (is_cons(c)) {
            len++;
            c = cdr(c);
        }
        idx += len;
        if (idx < 0) {
            *out = raise_error(interp, "list index out of bounds");
            return true;
        }
    }

    Value* current = collection;
    for (long i = 0; i < idx; i++) {
        if (!is_cons(current)) {
            *out = raise_error(interp, "list index out of bounds");
            return true;
        }
        current = cdr(current);
    }
    if (!is_cons(current)) {
        *out = raise_error(interp, "list index out of bounds");
        return true;
    }
    *out = car(current);
    return true;
}

fn bool jit_try_index_string(Interp* interp, Value* collection, Value* index, Value** out) {
    if (!is_string(collection) || !is_int(index)) return false;

    char[] bytes = collection.str_chars[:collection.str_len];
    usz cp_count = utf8_strlen(bytes);
    long idx = index.int_val;
    if (idx < 0) idx += (long)cp_count;
    if (idx < 0 || idx >= (long)cp_count) {
        *out = raise_error(interp, "string index out of bounds");
        return true;
    }
    usz byte_off = utf8_byte_offset(bytes, (usz)idx);
    usz pos = byte_off;
    uint cp = utf8_decode(bytes, &pos);
    *out = make_int(interp, (long)cp);
    return true;
}

fn bool jit_try_index_array(Interp* interp, Value* collection, Value* index, Value** out) {
    if (collection.tag != ARRAY || !is_int(index)) return false;

    long idx = index.int_val;
    long alen = (long)collection.array_val.length;
    if (idx < 0) idx += alen;
    if (idx < 0 || idx >= alen) {
        *out = raise_error(interp, "array index out of bounds");
        return true;
    }
    *out = collection.array_val.items[(usz)idx];
    return true;
}

fn bool jit_try_index_dict(Interp* interp, Value* collection, Value* index, Value** out) {
    if (collection.tag != HASHMAP) return false;
    Value* result = hashmap_get(collection.hashmap_val, index);
    if (result == null) result = make_nil(interp);
    *out = result;
    return true;
}

fn bool jit_try_index_instance(Interp* interp, Value* collection, Value* index, Value** out) {
    if (collection.tag != INSTANCE) return false;

    SymbolId ref_sym = interp.symbols.intern("ref");
    Value* ref_fn = interp.global_env.lookup(ref_sym);
    if (ref_fn == null || ref_fn.tag != METHOD_TABLE) return false;
    Value* args = make_cons(interp, collection, make_cons(interp, index, make_nil(interp)));
    *out = jit_apply_multi_args(interp, ref_fn, args, 2);
    return true;
}

fn Value* jit_index_type_error(Interp* interp, Value* collection) {
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "cannot index %s (expected array, dict, list, or string)",
        (ZString)interp.symbols.get_name(value_type_name(collection, interp)))!!;
    return raise_error(interp, msg);
}

fn Value* jit_do_index(Interp* interp, Value* collection, Value* index) {
    if (collection == null) return raise_error(interp, "index: nil collection");
    Value* result = null;
    if (jit_try_index_list(interp, collection, index, &result)) return result;
    if (jit_try_index_string(interp, collection, index, &result)) return result;
    if (jit_try_index_array(interp, collection, index, &result)) return result;
    if (jit_try_index_dict(interp, collection, index, &result)) return result;
    if (jit_try_index_instance(interp, collection, index, &result)) return result;
    return jit_index_type_error(interp, collection);
}

fn Value* jit_do_path(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_path(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_match(Interp* interp, Value* scrutinee, Expr* expr, Env* env) {
    interp.match_env = env;
    for (usz i = 0; i < expr.match.clause_count; i++) {
        MatchClause* clause = &expr.match.clauses[i];
        MatchResult match_r = match_pattern(clause.pattern, scrutinee, interp);
        if (match_r.has_error()) {
            Value* err = match_r.error;
            match_r.cleanup();
            return err;
        }
        if (match_r.matched) {
            Env* clause_env = make_env(interp, env);
            if (clause_env == null) {
                match_r.cleanup();
                return raise_error(interp, "out of memory while binding match clause");
            }
            for (usz j = 0; j < match_r.binding_count; j++) {
                Value* bind_err = env_define_with_barrier_result(
                    clause_env,
                    match_r.bindings[j].name,
                    match_r.bindings[j].value,
                    interp
                );
                if (bind_err != null) {
                    match_r.cleanup();
                    return raise_error(interp, "out of memory while binding match clause");
                }
            }
            match_r.cleanup();
            return jit_eval(clause.result, clause_env, interp);
        }
    }
    return format_match_error(scrutinee, expr, interp);
}

fn Value* jit_do_deftype(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_deftype(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defabstract(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_defabstract(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defunion(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_defunion(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defalias(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_defalias(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defeffect(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_defeffect(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_ffi_lib(Interp* interp, Expr* expr, Env* env) {
    return eval_ffi_lib(expr, env, interp);
}

fn Value* jit_do_ffi_fn(Interp* interp, Expr* expr, Env* env) {
    return eval_ffi_fn(expr, env, interp);
}

fn Value* jit_do_module(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = jit_eval_module_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_import(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = jit_eval_import_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn void jit_set_call_name(Interp* interp, SymbolId name) {
    interp.last_call_name = name;
}

fn void jit_clear_call_name(Interp* interp) {
    interp.last_call_name = (SymbolId)0;
}
