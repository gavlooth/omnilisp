module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (module/import + dispatch helpers)
// =============================================================================
fn Value* jit_do_index(Interp* interp, Value* collection, Value* index) {
    if (collection == null) return raise_error(interp, "index: nil collection");
    // List/cons indexing — supports negative indexing
    if (is_cons(collection) && is_int(index)) {
        long idx = index.int_val;
        if (idx < 0) {
            // Count list length for negative index resolution
            long len = 0;
            Value* c = collection;
            while (is_cons(c)) { len++; c = cdr(c); }
            idx += len;
            if (idx < 0) return raise_error(interp, "list index out of bounds");
        }
        Value* current = collection;
        for (long i = 0; i < idx; i++) {
            if (!is_cons(current)) return raise_error(interp, "list index out of bounds");
            current = cdr(current);
        }
        if (!is_cons(current)) return raise_error(interp, "list index out of bounds");
        return car(current);
    }
    // String indexing — supports negative indexing, UTF-8 codepoint index
    if (is_string(collection) && is_int(index)) {
        char[] bytes = collection.str_chars[:collection.str_len];
        usz cp_count = utf8_strlen(bytes);
        long idx = index.int_val;
        if (idx < 0) idx += (long)cp_count;
        if (idx < 0 || idx >= (long)cp_count) {
            return raise_error(interp, "string index out of bounds");
        }
        usz byte_off = utf8_byte_offset(bytes, (usz)idx);
        usz pos = byte_off;
        uint cp = utf8_decode(bytes, &pos);
        return make_int(interp, (long)cp);
    }
    // Array indexing — supports negative indexing
    if (collection.tag == ARRAY && is_int(index)) {
        long idx = index.int_val;
        long alen = (long)collection.array_val.length;
        if (idx < 0) idx += alen;
        if (idx < 0 || idx >= alen) {
            return raise_error(interp, "array index out of bounds");
        }
        return collection.array_val.items[(usz)idx];
    }
    // Dict indexing
    if (collection.tag == HASHMAP) {
        Value* result = hashmap_get(collection.hashmap_val, index);
        if (result == null) return make_nil(interp);
        return result;
    }
    // Instance indexing — dispatch through `ref` method table if available
    if (collection.tag == INSTANCE) {
        SymbolId ref_sym = interp.symbols.intern("ref");
        Value* ref_fn = interp.global_env.lookup(ref_sym);
        if (ref_fn != null && ref_fn.tag == METHOD_TABLE) {
            Value* args = make_cons(interp, collection, make_cons(interp, index, make_nil(interp)));
            return jit_apply_multi_args(interp, ref_fn, args, 2);
        }
    }
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "cannot index %s (expected array, dict, list, or string)",
        (ZString)interp.symbols.get_name(value_type_name(collection, interp)))!!;
    return raise_error(interp, msg);
}

// Path helper — follows field access chain on Instance values.
// Calls eval_path directly (it doesn't use eval internally).
fn Value* jit_do_path(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = eval_path(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

// Match helper — evaluates scrutinee, runs pattern matching, evals matched result.
// Eliminates one level of eval dispatch compared to jit_eval_match.
fn Value* jit_do_match(Interp* interp, Value* scrutinee, Expr* expr, Env* env) {
    // Set match_env so guard predicates can resolve free variables
    interp.match_env = env;
    for (usz i = 0; i < expr.match.clause_count; i++) {
        MatchClause* clause = &expr.match.clauses[i];
        MatchResult match_r = match_pattern(clause.pattern, scrutinee, interp);
        if (match_r.matched) {
            Env* clause_env = make_env(interp, env);
            for (usz j = 0; j < match_r.binding_count; j++) {
                env_define_with_barrier(clause_env, match_r.bindings[j].name, match_r.bindings[j].value, interp);
            }
            match_r.cleanup();
            // Evaluate clause body directly — TCO bounce doesn't work when
            // match is nested inside define/let/args (outer compiled code
            // would receive the sentinel instead of the actual value).
            return jit_eval(clause.result, clause_env, interp);
        }
    }
    return format_match_error(scrutinee, expr, interp);
}

// Type definition helpers — call eval_* directly, no eval dispatch.
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

// JIT-native module/import — no eval() dependency.
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

fn EvalResult jit_eval_module_impl(Expr* expr, Env* env, Interp* interp) {
    SymbolId name = expr.module_expr.name;

    Module* existing = find_module(name, interp);
    if (existing != null) {
        if (existing.loaded) {
            return eval_error("module already defined");
        }
        // Previous load failed — allow re-definition by removing the stale entry
        existing.name = (SymbolId)0;
    }

    if (interp.module_count >= interp.module_capacity) interp.grow_module_table();
    Module* mod = &interp.modules[interp.module_count];
    mod.name = name;
    mod.loaded = false;
    mod.path_len = 0;
    usz exp_cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
    mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * exp_cap);
    mod.export_capacity = exp_cap;
    mod.export_count = expr.module_expr.export_count;
    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        mod.exports[i] = expr.module_expr.exports[i];
    }

    // Module env must live in root_scope (module persists beyond run() scope)
    main::ScopeRegion* saved_mod_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Env* mod_env = make_env(interp, interp.global_env);
    interp.current_scope = saved_mod_scope;
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

    EvalResult last_result = eval_ok(make_nil(interp));
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        last_result = jit_eval_to_result(expr.module_expr.body[i], mod_env, interp);
        if (last_result.error.has_error) {
            interp.global_env = saved_global;
            return last_result;
        }
    }

    interp.global_env = saved_global;
    mod.loaded = true;
    return eval_ok(make_nil(interp));
}

// Extract directory from a file path (e.g. "lib/ffi/torch.omni" → "lib/ffi/")
// Returns length of directory prefix (including trailing /).  0 if no directory.
fn usz path_dir_len(char[] path) {
    usz last_slash = 0;
    bool found = false;
    for (usz i = 0; i < path.len; i++) {
        if (path[i] == '/') { last_slash = i; found = true; }
    }
    return found ? last_slash + 1 : 0;
}

// Resolve a relative path against the current source directory stack.
// Writes resolved path into buf, returns length.  If no source dir on stack, copies path as-is.
fn usz resolve_import_path(char[] rel_path, Interp* interp, char* buf, usz buf_cap) {
    usz pos = 0;
    if (interp.source_dir_count > 0) {
        // Copy current source directory
        char* dir = &interp.source_dirs[interp.source_dir_count - 1];
        usz di = 0;
        while (di < 255 && dir[di] != 0 && pos < buf_cap - 1) {
            buf[pos] = dir[di];
            pos++;
            di++;
        }
    }
    // Copy relative path
    for (usz i = 0; i < rel_path.len && pos < buf_cap - 1; i++) {
        buf[pos] = rel_path[i];
        pos++;
    }
    buf[pos] = 0;
    return pos;
}

// Push a source directory derived from a file path.
fn void push_source_dir(char[] file_path, Interp* interp) {
    if (interp.source_dir_count >= 16) return;
    usz dlen = path_dir_len(file_path);
    usz slot = interp.source_dir_count;
    for (usz i = 0; i < dlen && i < 255; i++) {
        interp.source_dirs[slot][i] = file_path[i];
    }
    interp.source_dirs[slot][dlen] = 0;
    interp.source_dir_count++;
}

fn void pop_source_dir(Interp* interp) {
    if (interp.source_dir_count > 0) interp.source_dir_count--;
}

fn void module_copy_path(Module* mod, char[] path) {
    for (usz i = 0; i < path.len && i < 255; i++) {
        mod.path[i] = path[i];
    }
    mod.path_len = path.len;
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
    module_copy_path(mod, path);
    mod.export_count = 0;
    mod.export_capacity = 32;
    mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * 32);

    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    mod.env = make_env(interp, interp.global_env);
    interp.current_scope = saved_scope;

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
    return eval_ok(make_nil(interp));
}

fn EvalResult jit_eval_declared_module_file(List{Expr*}* expr_list, usz expr_count, char[] path, Interp* interp) {
    SymbolId declared_name = (*expr_list)[0].module_expr.name;
    Module* already = find_module(declared_name, interp);
    if (already != null && already.loaded) {
        return eval_ok(make_nil(interp));
    }

    EvalResult mod_result = jit_eval_to_result((*expr_list)[0], interp.global_env, interp);
    if (mod_result.error.has_error) return mod_result;

    Module* mod = find_module(declared_name, interp);
    if (mod != null && mod.path_len == 0) {
        module_copy_path(mod, path);
    }

    EvalResult top_level = jit_eval_module_top_level(expr_list, 1, expr_count, interp.global_env, interp);
    if (top_level.error.has_error) return top_level;
    return mod_result;
}

fn EvalResult jit_eval_implicit_module_file(SymbolId name, char[] path, List{Expr*}* expr_list, usz expr_count, Interp* interp) {
    Module* mod = jit_create_file_module(name, path, interp);

    Env* saved_global = interp.global_env;
    interp.global_env = mod.env;
    defer interp.global_env = saved_global;

    for (usz i = 0; i < expr_count; i++) {
        EvalResult r = jit_eval_to_result((*expr_list)[i], mod.env, interp);
        if (r.error.has_error) return r;
        if ((*expr_list)[i].tag == E_DEFINE) {
            module_append_export(mod, (*expr_list)[i].define.name);
        }
    }
    mod.loaded = true;
    return eval_ok(make_nil(interp));
}

fn bool module_path_matches(Module* mod, char[] path) {
    if (mod.path_len != path.len || !mod.loaded) return false;
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
    rel_buf[0] = 'l'; rel_buf[1] = 'i'; rel_buf[2] = 'b'; rel_buf[3] = '/';
    usz rp = 4;
    for (usz i = 0; i < sym_name.len; i++) {
        rel_buf[rp] = sym_name[i];
        rp++;
    }
    rel_buf[rp] = '.'; rp++;
    rel_buf[rp] = 'o'; rp++;
    rel_buf[rp] = 'm'; rp++;
    rel_buf[rp] = 'n'; rp++;
    rel_buf[rp] = 'i'; rp++;
    rel_buf[rp] = 0;
    *len_out = rp;
    return true;
}

fn EvalResult load_import_target_module(Expr* expr, SymbolId name, Interp* interp, Module** mod_out) {
    Module* mod = find_module(name, interp);
    if (mod != null) {
        if (!mod.loaded) return eval_error("circular import detected");
        *mod_out = mod;
        return eval_ok(make_nil(interp));
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
        return eval_ok(make_nil(interp));
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
    return eval_ok(make_nil(interp));
}

fn bool module_exports_symbol(Module* mod, SymbolId name) {
    for (usz i = 0; i < mod.export_count; i++) {
        if ((uint)mod.exports[i] == (uint)name) return true;
    }
    return false;
}

fn EvalResult bind_import_symbols(Expr* expr, Env* env, Module* mod, Interp* interp) {
    Value* mod_val = make_module(interp, mod);
    env_define_with_barrier(env, expr.import_expr.name, mod_val, interp);

    if (expr.import_expr.import_all) {
        for (usz i = 0; i < mod.export_count; i++) {
            Value* val = mod.env.lookup(mod.exports[i]);
            if (val != null) env_define_with_barrier(env, mod.exports[i], val, interp);
        }
        return eval_ok(make_nil(interp));
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
        env_define_with_barrier(env, bind_name, val, interp);
    }
    return eval_ok(make_nil(interp));
}

fn EvalResult jit_load_module_from_file(char[] path, SymbolId name, Interp* interp) {
    if (catch err = io::file::load_temp((String)path)) {
        (void)err;
        return eval_error("cannot read module file");
    }
    if (try content = io::file::load_temp((String)path)) {
        char[] source = content;
        push_source_dir(path, interp);
        defer pop_source_dir(interp);

        Lexer lex;
        lex.init(source);
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

// Set last_call_name for error messages — called from JIT before apply.
fn void jit_set_call_name(Interp* interp, SymbolId name) {
    interp.last_call_name = name;
}

// Clear last_call_name (for computed function positions)
fn void jit_clear_call_name(Interp* interp) {
    interp.last_call_name = (SymbolId)0;
}
