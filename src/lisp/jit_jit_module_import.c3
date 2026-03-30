module lisp;

import std::io;
import std::collections::list;

// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (module/import)
// =============================================================================

fn EvalResult jit_eval_module_impl(Expr* expr, Env* env, Interp* interp) {
    SymbolId name = expr.module_expr.name;

    Module* existing = find_module(name, interp);
    if (existing != null) {
        if (existing.loaded) {
            return eval_error("module already defined");
        }
        existing.name = (SymbolId)0;
    }

    if (interp.module_count >= interp.module_capacity) interp.grow_module_table();
    Module* mod = &interp.modules[interp.module_count];
    mod.name = name;
    mod.loaded = false;
    mod.path = null;
    mod.path_len = 0;
    usz exp_cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
    mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * exp_cap);
    mod.export_capacity = exp_cap;
    mod.export_count = expr.module_expr.export_count;
    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        mod.exports[i] = expr.module_expr.exports[i];
    }

    Env* mod_env = boundary_make_env_in_root(interp, interp.global_env);
    mod.env = mod_env;
    interp.module_count++;

    usz mod_idx = interp.module_count - 1;
    usz mh_slot = (usz)name % interp.module_hash_capacity;
    while (interp.module_hash_index[mh_slot] != usz.max) {
        mh_slot = (mh_slot + 1) % interp.module_hash_capacity;
    }
    interp.module_hash_index[mh_slot] = mod_idx;

    Env* saved_global = interp.global_env;
    interp.global_env = mod_env;

    EvalResult last_result = eval_ok(make_void(interp));
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        last_result = jit_eval_to_result(expr.module_expr.body[i], mod_env, interp);
        if (last_result.error.has_error) {
            interp.global_env = saved_global;
            return last_result;
        }
    }

    interp.global_env = saved_global;
    mod.loaded = true;
    return eval_ok(make_void(interp));
}

fn usz path_dir_len(char[] path) {
    usz last_slash = 0;
    bool found = false;
    for (usz i = 0; i < path.len; i++) {
        if (path[i] == '/') {
            last_slash = i;
            found = true;
        }
    }
    return found ? last_slash + 1 : 0;
}

fn usz resolve_import_path(char[] rel_path, Interp* interp, char* buf, usz buf_cap) {
    usz pos = 0;
    if (interp.source_dir_count > 0) {
        char* dir = interp.source_dirs[interp.source_dir_count - 1];
        usz di = 0;
        while (dir[di] != 0 && pos < buf_cap - 1) {
            buf[pos] = dir[di];
            pos++;
            di++;
        }
    }
    for (usz i = 0; i < rel_path.len && pos < buf_cap - 1; i++) {
        buf[pos] = rel_path[i];
        pos++;
    }
    buf[pos] = 0;
    return pos;
}

fn bool ensure_source_dir_capacity(Interp* interp, usz needed) {
    if (needed <= interp.source_dir_capacity) return true;
    usz new_cap = interp.source_dir_capacity > 0 ? interp.source_dir_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    char** new_dirs = (char**)mem::realloc(interp.source_dirs, uptr.sizeof * new_cap);
    if (new_dirs == null) return false;
    interp.source_dirs = new_dirs;
    interp.source_dir_capacity = new_cap;
    return true;
}

fn bool push_source_dir(char[] file_path, Interp* interp) {
    usz dlen = path_dir_len(file_path);
    if (dlen == 0) return true;
    if (!ensure_source_dir_capacity(interp, interp.source_dir_count + 1)) return false;
    char* dir = (char*)mem::malloc(dlen + 1);
    if (dir == null) return false;
    usz slot = interp.source_dir_count;
    for (usz i = 0; i < dlen; i++) {
        dir[i] = file_path[i];
    }
    dir[dlen] = 0;
    interp.source_dirs[slot] = dir;
    interp.source_dir_count++;
    return true;
}

fn void pop_source_dir(Interp* interp) {
    if (interp.source_dir_count == 0) return;
    usz top = interp.source_dir_count - 1;
    if (interp.source_dirs[top] != null) {
        mem::free(interp.source_dirs[top]);
        interp.source_dirs[top] = null;
    }
    interp.source_dir_count--;
}

fn bool module_copy_path(Module* mod, char[] path) {
    if (mod.path != null) {
        mem::free(mod.path);
        mod.path = null;
    }
    char* stored = (char*)mem::malloc(path.len + 1);
    if (stored == null) {
        mod.path_len = 0;
        return false;
    }
    for (usz i = 0; i < path.len; i++) {
        stored[i] = path[i];
    }
    stored[path.len] = 0;
    mod.path = stored;
    mod.path_len = path.len;
    return true;
}

fn void module_ensure_export_capacity(Module* mod) {
    if (mod.export_count < mod.export_capacity) return;
    usz new_cap = mod.export_capacity * 2;
    SymbolId* new_exports = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
    for (usz i = 0; i < mod.export_count; i++) new_exports[i] = mod.exports[i];
    mem::free(mod.exports);
    mod.exports = new_exports;
    mod.export_capacity = new_cap;
}

fn void module_append_export(Module* mod, SymbolId name) {
    module_ensure_export_capacity(mod);
    mod.exports[mod.export_count] = name;
    mod.export_count++;
}

fn Module* jit_create_file_module(SymbolId name, char[] path, Interp* interp) {
    if (interp.module_count >= interp.module_capacity) interp.grow_module_table();
    Module* mod = &interp.modules[interp.module_count];
    mod.name = name;
    mod.loaded = false;
    mod.path = null;
    mod.path_len = 0;
    if (!module_copy_path(mod, path)) return null;
    mod.export_count = 0;
    mod.export_capacity = 32;
    mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * 32);

    mod.env = boundary_make_env_in_root(interp, interp.global_env);

    interp.module_count++;
    usz mod_idx = interp.module_count - 1;
    usz slot = (usz)name % interp.module_hash_capacity;
    while (interp.module_hash_index[slot] != usz.max) {
        slot = (slot + 1) % interp.module_hash_capacity;
    }
    interp.module_hash_index[slot] = mod_idx;
    return mod;
}

fn EvalResult jit_eval_module_top_level(List{Expr*}* expr_list, usz start, usz end, Env* env, Interp* interp) {
    for (usz i = start; i < end; i++) {
        EvalResult r = jit_eval_to_result((*expr_list)[i], env, interp);
        if (r.error.has_error) return r;
    }
    return eval_ok(make_void(interp));
}

fn EvalResult jit_eval_declared_module_file(List{Expr*}* expr_list, usz expr_count, char[] path, Interp* interp) {
    SymbolId declared_name = (*expr_list)[0].module_expr.name;
    Module* already = find_module(declared_name, interp);
    if (already != null && already.loaded) {
        return eval_ok(make_void(interp));
    }

    EvalResult mod_result = jit_eval_to_result((*expr_list)[0], interp.global_env, interp);
    if (mod_result.error.has_error) return mod_result;

    Module* mod = find_module(declared_name, interp);
    if (mod != null && mod.path_len == 0) {
        if (!module_copy_path(mod, path)) return eval_error("module path allocation failed");
    }

    EvalResult top_level = jit_eval_module_top_level(expr_list, 1, expr_count, interp.global_env, interp);
    if (top_level.error.has_error) return top_level;
    return mod_result;
}

fn EvalResult jit_eval_implicit_module_file(SymbolId name, char[] path, List{Expr*}* expr_list, usz expr_count, Interp* interp) {
    Module* mod = jit_create_file_module(name, path, interp);
    if (mod == null) return eval_error("module path allocation failed");

    Env* saved_global = interp.global_env;
    interp.global_env = mod.env;
    defer interp.global_env = saved_global;

    for (usz i = 0; i < expr_count; i++) {
        Expr* expr = (*expr_list)[i];
        bool export_candidate = expr != null && expr.tag == E_DEFINE;
        SymbolId export_name = export_candidate ? expr.define.name : (SymbolId)0;
        EvalResult r = jit_eval_to_result(expr, mod.env, interp);
        if (r.error.has_error) return r;
        if (export_candidate) {
            module_append_export(mod, export_name);
        }
    }
    mod.loaded = true;
    return eval_ok(make_void(interp));
}

fn bool module_path_matches(Module* mod, char[] path) {
    if (mod.path_len != path.len || !mod.loaded) return false;
    if (mod.path == null) return false;
    for (usz i = 0; i < path.len; i++) {
        if (mod.path[i] != path[i]) return false;
    }
    return true;
}

fn Module* find_loaded_module_by_path(char[] path, Interp* interp) {
    for (usz i = 0; i < interp.module_count; i++) {
        Module* m = &interp.modules[i];
        if (module_path_matches(m, path)) return m;
    }
    return null;
}

fn Module* resolve_loaded_module(SymbolId name, char[] path, usz mod_count_before, Interp* interp) {
    Module* mod = find_module(name, interp);
    if (mod == null && interp.module_count > mod_count_before) {
        mod = &interp.modules[mod_count_before];
    }
    if (mod == null) {
        mod = find_loaded_module_by_path(path, interp);
    }
    return mod;
}

fn bool build_default_module_rel_path(SymbolId name, Interp* interp, char* rel_buf, usz* len_out) {
    char[] sym_name = interp.symbols.get_name(name);
    if (4 + sym_name.len + 5 > 255) return false;
    rel_buf[0] = 'l';
    rel_buf[1] = 'i';
    rel_buf[2] = 'b';
    rel_buf[3] = '/';
    usz rp = 4;
    for (usz i = 0; i < sym_name.len; i++) {
        rel_buf[rp] = sym_name[i] == '.' ? '/' : sym_name[i];
        rp++;
    }
    rel_buf[rp] = '.';
    rp++;
    rel_buf[rp] = 'o';
    rp++;
    rel_buf[rp] = 'm';
    rp++;
    rel_buf[rp] = 'n';
    rp++;
    rel_buf[rp] = 'i';
    rp++;
    rel_buf[rp] = 0;
    *len_out = rp;
    return true;
}

fn EvalResult load_import_target_module(Expr* expr, SymbolId name, Interp* interp, Module** mod_out) {
    Module* mod = find_module(name, interp);
    if (mod != null) {
        if (!mod.loaded) return eval_error("circular import detected");
        *mod_out = mod;
        return eval_ok(make_void(interp));
    }

    if (expr.import_expr.has_path) {
        char[] rel_path = expr.import_expr.path[:expr.import_expr.path_len];
        char[512] resolved;
        usz rlen = resolve_import_path(rel_path, interp, &resolved, 512);
        char[] path = resolved[:rlen];
        usz mod_count_before = interp.module_count;
        EvalResult load_result = jit_load_module_from_file(path, name, interp);
        if (load_result.error.has_error) return load_result;
        *mod_out = resolve_loaded_module(name, path, mod_count_before, interp);
        return eval_ok(make_void(interp));
    }

    char[256] rel_buf;
    usz rel_len = 0;
    if (!build_default_module_rel_path(name, interp, &rel_buf, &rel_len)) {
        return eval_error("module path too long");
    }
    char[512] resolved;
    usz rlen = resolve_import_path(rel_buf[:rel_len], interp, &resolved, 512);
    char[] path = resolved[:rlen];
    EvalResult load_result = jit_load_module_from_file(path, name, interp);
    if (load_result.error.has_error) return load_result;
    *mod_out = find_module(name, interp);
    return eval_ok(make_void(interp));
}

fn bool module_exports_symbol(Module* mod, SymbolId name) {
    for (usz i = 0; i < mod.export_count; i++) {
        if ((uint)mod.exports[i] == (uint)name) return true;
    }
    return false;
}

fn EvalResult bind_import_symbols(Expr* expr, Env* env, Module* mod, Interp* interp) {
    Value* mod_val = make_module(interp, mod);
    if (env_define_with_barrier_result(env, expr.import_expr.name, mod_val, interp) != null) {
        return eval_error("import: out of memory while binding module");
    }

    if (expr.import_expr.import_all) {
        for (usz i = 0; i < mod.export_count; i++) {
            Value* val = mod.env.lookup(mod.exports[i]);
            if (val != null &&
                env_define_with_barrier_result(env, mod.exports[i], val, interp) != null) {
                return eval_error("import: out of memory while binding symbol");
            }
        }
        return eval_ok(make_void(interp));
    }

    for (usz i = 0; i < expr.import_expr.import_count; i++) {
        SymbolId imp_name = expr.import_expr.imports[i];
        SymbolId alias_name = expr.import_expr.aliases[i];
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
        while (!lex.at_end()) {
            expr_list.push(p.parse_expr());
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
