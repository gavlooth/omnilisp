module lisp;

import std::io;
import std::collections::list;

// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (module/import)
// =============================================================================

fn EvalResult load_import_target_module_spec(ExprImport* spec, SymbolId name, Interp* interp, Module** mod_out) {
    if (spec.has_path) {
        char[] rel_path = spec.path[:spec.path_len];
        char[512] resolved;
        usz rlen = 0;
        if (!resolve_import_path_checked(rel_path, interp, &resolved, 512, &rlen)) {
            return eval_error("module path too long");
        }
        char[] path = resolved[:rlen];

        Module* path_mod = find_module_by_path_any(path, interp);
        if (path_mod != null) {
            if (!path_mod.loaded) return eval_error("circular import detected");
            *mod_out = path_mod;
            return eval_ok(make_void(interp));
        }

        EvalResult load_result = jit_load_module_from_file(path, name, interp);
        if (load_result.error.has_error) return load_result;
        *mod_out = resolve_loaded_module(name, path, interp);
        return eval_ok(make_void(interp));
    }

    Module* mod = find_module(name, interp);
    if (mod != null) {
        if (!mod.loaded) return eval_error("circular import detected");
        *mod_out = mod;
        return eval_ok(make_void(interp));
    }

    char[256] rel_buf;
    usz rel_len = 0;
    if (!build_default_module_rel_path(name, interp, &rel_buf, &rel_len)) {
        return eval_error("module path too long");
    }
    char[512] resolved;
    usz rlen = 0;
    if (!resolve_import_path_checked(rel_buf[:rel_len], interp, &resolved, 512, &rlen)) {
        return eval_error("module path too long");
    }
    char[] path = resolved[:rlen];
    EvalResult load_result = jit_load_module_from_file(path, name, interp);
    if (load_result.error.has_error) return load_result;
    *mod_out = resolve_loaded_module(name, path, interp);
    return eval_ok(make_void(interp));
}

fn EvalResult load_import_target_module(Expr* expr, SymbolId name, Interp* interp, Module** mod_out) {
    return load_import_target_module_spec(&expr.import_expr, name, interp, mod_out);
}

fn bool module_exports_symbol(Module* mod, SymbolId name) {
    for (usz i = 0; i < mod.export_count; i++) {
        if ((uint)mod.exports[i] == (uint)name) return true;
    }
    return false;
}

fn EvalResult bind_import_symbols_spec(ExprImport* spec, Env* env, Module* mod, Interp* interp) {
    Value* mod_val = make_module(interp, mod);
    if (mod_val == null || mod_val.tag == ERROR) {
        return eval_error("import: out of memory while binding module");
    }
    if (env_define_with_barrier_result(env, spec.name, mod_val, interp) != null) {
        return eval_error("import: out of memory while binding module");
    }

    if (spec.import_all) {
        for (usz i = 0; i < mod.export_count; i++) {
            Value* val = mod.env.lookup(mod.exports[i]);
            if (val != null &&
                env_define_with_barrier_result(env, mod.exports[i], val, interp) != null) {
                return eval_error("import: out of memory while binding symbol");
            }
        }
        return eval_ok(make_void(interp));
    }

    for (usz i = 0; i < spec.import_count; i++) {
        SymbolId imp_name = spec.imports[i];
        SymbolId alias_name = spec.aliases[i];
        if (!module_exports_symbol(mod, imp_name)) {
            return eval_error("symbol not exported from module");
        }
        Value* val = mod.env.lookup(imp_name);
        if (val == null) {
            return eval_error("symbol not found in module");
        }
        SymbolId bind_name = (uint)alias_name != 0 ? alias_name : imp_name;
        if (env_define_with_barrier_result(env, bind_name, val, interp) != null) {
            return eval_error("import: out of memory while binding symbol");
        }
    }
    return eval_ok(make_void(interp));
}

fn EvalResult bind_import_symbols(Expr* expr, Env* env, Module* mod, Interp* interp) {
    return bind_import_symbols_spec(&expr.import_expr, env, mod, interp);
}

fn EvalResult jit_load_module_from_file(char[] path, SymbolId name, Interp* interp) {
    if (catch err = io::file::load_temp((String)path)) {
        (void)err;
        return eval_error("cannot read module file");
    }
    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;
        if (!push_source_dir(path, interp)) return eval_error("module import source-dir allocation failed");
        defer pop_source_dir(interp);

        Lexer lex;
        lex.init(source);
        defer lex.destroy();
        Parser p;
        p.init(&lex, interp);

        List{Expr*} expr_list;
        defer expr_list.free();
        while (!lex.at_end() && !p.has_error) {
            Expr* expr = p.parse_expr();
            if (expr != null && !p.has_error) {
                expr_list.push(expr);
            }
        }

        if (p.has_error) {
            return parser_error_to_eval_result(&p);
        }

        usz expr_count = expr_list.len();
        if (expr_count > 0 && expr_list[0].tag == E_MODULE) {
            return jit_eval_declared_module_file(&expr_list, expr_count, path, interp);
        }
        return jit_eval_implicit_module_file(name, path, &expr_list, expr_count, interp);
    }
    return eval_error("cannot read module file");
}

fn EvalResult jit_eval_import_impl(Expr* expr, Env* env, Interp* interp) {
    SymbolId name = expr.import_expr.name;
    Module* mod = null;
    EvalResult load_result = load_import_target_module(expr, name, interp, &mod);
    if (load_result.error.has_error) return load_result;
    if (mod == null) return eval_error("module not found");
    return bind_import_symbols(expr, env, mod, interp);
}

fn EvalResult jit_eval_with_module_open_impl(Expr* expr, Env* env, Interp* interp) {
    if (expr.with_module == null) return eval_error("with: missing module open body");
    ExprImport spec = expr.with_module.import_expr;
    Module* mod = null;
    EvalResult load_result = load_import_target_module_spec(&spec, spec.name, interp, &mod);
    if (load_result.error.has_error) return load_result;
    if (mod == null) return eval_error("with: module not found");

    Env* scoped_env = make_env(interp, env);
    if (scoped_env == null) return eval_error("with: out of memory while creating scoped environment");

    spec.import_all = true;
    spec.import_count = 0;
    EvalResult bind_result = bind_import_symbols_spec(&spec, scoped_env, mod, interp);
    if (bind_result.error.has_error) return bind_result;

    EvalResult last_result = eval_ok(make_void(interp));
    for (usz i = 0; i < expr.with_module.body_count; i++) {
        last_result = jit_eval_to_result(expr.with_module.body[i], scoped_env, interp);
        if (last_result.error.has_error) return last_result;
    }
    return last_result;
}
