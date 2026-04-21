module lisp;

fn bool jit_try_index_instance(Interp* interp, Value* collection, Value* index, Value** out) {
    if (collection.tag != INSTANCE) return false;

    SymbolId ref_sym = interp.symbols.intern("ref");
    if (ref_sym == INVALID_SYMBOL_ID) {
        *out = raise_error(interp, "ref: failed to intern dispatch symbol");
        return true;
    }
    Value* ref_fn = interp.global_env.lookup(ref_sym);
    if (ref_fn == null || ref_fn.tag != METHOD_TABLE) return false;
    Value* args = make_list2_or_error(interp, collection, index, "ref: failed to construct argument list");
    if (args == null || args.tag == ERROR) {
        *out = args;
        return true;
    }
    *out = jit_apply_multi_args(interp, ref_fn, args, 2);
    return true;
}

fn Value* jit_do_index(Interp* interp, Value* collection, Value* index) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* result = null;
    if (ref_try_lookup_collection(interp, collection, index, &result)) return result;
    if (jit_try_index_instance(interp, collection, index, &result)) return result;
    return ref_type_error(interp);
}

fn Value* jit_do_path(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_path(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_match(Interp* interp, Value* scrutinee, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
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
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_deftype(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defabstract(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_defabstract(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defunion(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_defunion(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defalias(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_defalias(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_defeffect(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_defeffect(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_ffi_lib(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return eval_ffi_lib(expr, env, interp);
}

fn Value* jit_do_ffi_fn(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return eval_ffi_fn(expr, env, interp);
}

fn Value* jit_do_module(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_eval_module_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_do_import(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_eval_import_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn void jit_set_call_name(Interp* interp, SymbolId name) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return;
    interp.last_call_name = name;
}

fn void jit_clear_call_name(Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return;
    interp.last_call_name = (SymbolId)0;
}
