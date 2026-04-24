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

fn usz find_module_index_by_path(char[] path, bool require_loaded, Interp* interp) {
    if (interp == null) return usz.max;
    for (usz i = 0; i < interp.module_count; i++) {
        if (module_path_matches(&interp.modules[i], path, require_loaded)) return i;
    }
    return usz.max;
}

fn usz reacquire_file_module_index(SymbolId name, char[] path, usz expected_idx, Interp* interp) {
    usz idx = reacquire_module_index(name, expected_idx, interp);
    if (idx != usz.max && module_path_matches(&interp.modules[idx], path, false)) return idx;

    idx = find_module_index_by_path(path, false, interp);
    if (idx != usz.max && interp.modules[idx].name == name) return idx;
    return usz.max;
}

fn Module* reacquire_file_module(SymbolId name, char[] path, usz expected_idx, Interp* interp) {
    usz idx = reacquire_file_module_index(name, path, expected_idx, interp);
    return idx != usz.max ? &interp.modules[idx] : null;
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
    Module* mod = reacquire_module(declared_name, mod_idx, interp);
    if (mod == null) {
        if (mod_idx != usz.max) rollback_module_publication(interp, mod_idx);
        return eval_error("module publication lost during evaluation");
    }
    if (mod != null && mod.path_len == 0) {
        if (!module_copy_path(mod, path)) {
            rollback_module_publication(interp, mod_idx);
            return eval_error("module path allocation failed");
        }
    }

    EvalResult top_level = jit_eval_module_top_level(expr_list, 1, expr_count, interp.global_env, interp);
    usz published_idx = mod_idx;
    mod_idx = reacquire_file_module_index(declared_name, path, mod_idx, interp);
    if (top_level.error.has_error) {
        if (published_idx != usz.max) rollback_module_publication(interp, published_idx);
        return top_level;
    }
    if (mod_idx == usz.max) {
        rollback_module_publication(interp, published_idx);
        return eval_error("module publication lost during evaluation");
    }
    return mod_result;
}

fn EvalResult jit_eval_implicit_module_file(SymbolId name, char[] path, List{Expr*}* expr_list, usz expr_count, Interp* interp) {
    usz mod_idx = usz.max;
    if (!jit_create_file_module_index(name, path, interp, &mod_idx)) {
        return eval_error("module path allocation failed");
    }

    Env* saved_global = interp.global_env;
    Env* mod_env = interp.modules[mod_idx].env;
    interp.global_env = mod_env;
    defer interp.global_env = saved_global;

    for (usz i = 0; i < expr_count; i++) {
        Expr* expr = (*expr_list)[i];
        bool export_candidate = expr != null && expr.tag == E_DEFINE;
        SymbolId export_name = export_candidate ? expr.define.name : (SymbolId)0;
        EvalResult r = jit_eval_to_result(expr, mod_env, interp);
        usz current_idx = reacquire_file_module_index(name, path, mod_idx, interp);
        if (r.error.has_error) {
            if (mod_idx != usz.max) rollback_module_publication(interp, mod_idx);
            interp.global_env = saved_global;
            return r;
        }
        if (current_idx == usz.max) {
            if (mod_idx != usz.max) rollback_module_publication(interp, mod_idx);
            interp.global_env = saved_global;
            return eval_error("module publication lost during evaluation");
        }
        mod_idx = current_idx;
        if (export_candidate) {
            Module* mod = reacquire_file_module(name, path, mod_idx, interp);
            if (mod == null) {
                if (mod_idx != usz.max) rollback_module_publication(interp, mod_idx);
                interp.global_env = saved_global;
                return eval_error("module publication lost during evaluation");
            }
            if (!module_append_export(mod, export_name)) {
                interp.global_env = saved_global;
                rollback_module_publication(interp, mod_idx);
                return eval_error("module: out of memory while growing exports");
            }
        }
    }
    Module* mod = reacquire_file_module(name, path, mod_idx, interp);
    if (mod == null) {
        if (mod_idx != usz.max) rollback_module_publication(interp, mod_idx);
        return eval_error("module publication lost during evaluation");
    }
    mod.loaded = true;
    return eval_ok(make_void(interp));
}

fn bool module_path_matches(Module* mod, char[] path, bool require_loaded) {
    if (mod == null) return false;
    if (mod.path_len != path.len) return false;
    if (require_loaded && !mod.loaded) return false;
    if (mod.path == null) return false;
    for (usz i = 0; i < path.len; i++) {
        if (mod.path[i] != path[i]) return false;
    }
    return true;
}

fn Module* find_module_by_path(char[] path, bool require_loaded, Interp* interp) {
    usz idx = find_module_index_by_path(path, require_loaded, interp);
    return idx != usz.max ? &interp.modules[idx] : null;
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
