module lisp;

import std::collections::list;
import main;

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

    usz mod_idx = find_module_index(declared_name, interp);
    Module* mod = mod_idx != usz.max ? &interp.modules[mod_idx] : null;
    if (mod != null && mod.path_len == 0) {
        if (!module_copy_path(mod, path)) {
            rollback_module_publication(interp, mod_idx);
            return eval_error("module path allocation failed");
        }
    }

    EvalResult top_level = jit_eval_module_top_level(expr_list, 1, expr_count, interp.global_env, interp);
    if (top_level.error.has_error) {
        rollback_module_publication(interp, mod_idx);
        return top_level;
    }
    return mod_result;
}

fn EvalResult jit_eval_implicit_module_file(SymbolId name, char[] path, List{Expr*}* expr_list, usz expr_count, Interp* interp) {
    Module* mod = jit_create_file_module(name, path, interp);
    if (mod == null) return eval_error("module path allocation failed");
    usz mod_idx = find_module_index(name, interp);
    if (mod_idx == usz.max) return eval_error("module table state invalid");
    Env* mod_env = mod.env;

    Env* saved_global = interp.global_env;
    interp.global_env = mod_env;
    defer interp.global_env = saved_global;

    for (usz i = 0; i < expr_count; i++) {
        Expr* expr = (*expr_list)[i];
        bool export_candidate = expr != null && expr.tag == E_DEFINE;
        SymbolId export_name = export_candidate ? expr.define.name : (SymbolId)0;
        EvalResult r = jit_eval_to_result(expr, mod_env, interp);
        if (r.error.has_error) {
            rollback_module_publication(interp, mod_idx);
            return r;
        }
        if (export_candidate) {
            Module* current_mod = &interp.modules[mod_idx];
            if (!module_append_export(current_mod, export_name)) {
                rollback_module_publication(interp, mod_idx);
                return eval_error("module export allocation failed");
            }
        }
    }
    interp.modules[mod_idx].loaded = true;
    return eval_ok(make_void(interp));
}

fn bool module_path_matches(Module* mod, char[] path, bool require_loaded) {
    if (mod.path_len != path.len) return false;
    if (require_loaded && !mod.loaded) return false;
    if (mod.path == null) return false;
    for (usz i = 0; i < path.len; i++) {
        if (mod.path[i] != path[i]) return false;
    }
    return true;
}

fn Module* find_module_by_path(char[] path, bool require_loaded, Interp* interp) {
    for (usz i = 0; i < interp.module_count; i++) {
        Module* m = &interp.modules[i];
        if (module_path_matches(m, path, require_loaded)) return m;
    }
    return null;
}

fn Module* find_module_by_path_any(char[] path, Interp* interp) {
    return find_module_by_path(path, false, interp);
}

fn Module* find_loaded_module_by_path(char[] path, Interp* interp) {
    return find_module_by_path(path, true, interp);
}

fn Module* resolve_loaded_module(SymbolId name, char[] path, Interp* interp) {
    Module* mod = find_module(name, interp);
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
