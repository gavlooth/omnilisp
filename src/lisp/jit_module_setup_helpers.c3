module lisp;

import std::collections::list;
import main;

bool g_jit_force_module_export_alloc_fail = false;

fn SymbolId* jit_alloc_module_exports(usz capacity) @inline {
    if (g_jit_force_module_export_alloc_fail) return null;
    if (capacity > usz.max / SymbolId.sizeof) return null;
    return (SymbolId*)mem::malloc(SymbolId.sizeof * capacity);
}

fn void module_release_storage(Module* mod) {
    if (mod == null) return;
    if (mod.exports != null) {
        mem::free(mod.exports);
        mod.exports = null;
    }
    if (mod.path != null) {
        mem::free(mod.path);
        mod.path = null;
    }
    *mod = {};
}

fn bool interp_rebuild_module_hash(Interp* interp) {
    if (interp == null || interp.module_hash_index == null || interp.module_hash_capacity == 0) return false;
    for (usz i = 0; i < interp.module_hash_capacity; i++) {
        interp.module_hash_index[i] = usz.max;
    }
    for (usz i = 0; i < interp.module_count; i++) {
        SymbolId name = interp.modules[i].name;
        if (name == (SymbolId)0 || name == INVALID_SYMBOL_ID) continue;
        usz slot = (usz)name % interp.module_hash_capacity;
        bool inserted = false;
        for (usz probe = 0; probe < interp.module_hash_capacity; probe++) {
            if (interp.module_hash_index[slot] == usz.max) {
                interp.module_hash_index[slot] = i;
                inserted = true;
                break;
            }
            slot = (slot + 1) % interp.module_hash_capacity;
        }
        if (!inserted) return false;
    }
    return true;
}

fn usz find_module_index(SymbolId name, Interp* interp) {
    if (interp == null || interp.module_hash_index == null || interp.module_hash_capacity == 0) return usz.max;
    if (name == (SymbolId)0 || name == INVALID_SYMBOL_ID) return usz.max;
    usz hash_slot = (usz)name % interp.module_hash_capacity;
    for (usz probe = 0; probe < interp.module_hash_capacity; probe++) {
        usz idx = interp.module_hash_index[hash_slot];
        if (idx == usz.max) return usz.max;
        if (idx < interp.module_count && interp.modules[idx].name == name) return idx;
        hash_slot = (hash_slot + 1) % interp.module_hash_capacity;
    }
    return usz.max;
}

fn usz reacquire_module_index(SymbolId name, usz expected_idx, Interp* interp) {
    if (interp == null) return usz.max;
    if (expected_idx < interp.module_count && interp.modules[expected_idx].name == name) {
        return expected_idx;
    }
    return find_module_index(name, interp);
}

fn Module* reacquire_module(SymbolId name, usz expected_idx, Interp* interp) {
    usz idx = reacquire_module_index(name, expected_idx, interp);
    return idx != usz.max ? &interp.modules[idx] : null;
}

fn void rollback_module_publication(Interp* interp, usz mod_idx) {
    if (interp == null || mod_idx >= interp.module_count) return;
    module_release_storage(&interp.modules[mod_idx]);
    if (mod_idx + 1 == interp.module_count) {
        interp.module_count--;
        while (interp.module_count > 0 && interp.modules[interp.module_count - 1].name == (SymbolId)0) {
            interp.module_count--;
        }
    }
    (void)interp_rebuild_module_hash(interp);
}

fn EvalResult jit_eval_module_impl(Expr* expr, Env* env, Interp* interp) {
    SymbolId name = expr.module_expr.name;

    Module* existing = find_module(name, interp);
    if (existing != null) {
        if (existing.loaded) {
            return eval_error("module already defined");
        }
        existing.name = (SymbolId)0;
    }

    if (interp.module_count >= interp.module_capacity && !interp.grow_module_table()) {
        return eval_error("module table allocation failed");
    }
    Module* mod = &interp.modules[interp.module_count];
    mod.name = name;
    mod.loaded = false;
    mod.path = null;
    mod.path_len = 0;
    usz exp_cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
    mod.exports = jit_alloc_module_exports(exp_cap);
    if (mod.exports == null) {
        *mod = {};
        return eval_error("module export allocation failed");
    }
    mod.export_capacity = exp_cap;
    mod.export_count = expr.module_expr.export_count;
    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        mod.exports[i] = expr.module_expr.exports[i];
    }

    Env* mod_env = boundary_make_env_in_root(interp, interp.global_env);
    if (mod_env == null) {
        mem::free(mod.exports);
        *mod = {};
        return eval_error("module environment allocation failed");
    }
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
        usz current_idx = reacquire_module_index(name, mod_idx, interp);
        if (last_result.error.has_error) {
            interp.global_env = saved_global;
            if (current_idx != usz.max) rollback_module_publication(interp, current_idx);
            return last_result;
        }
        if (current_idx == usz.max) {
            interp.global_env = saved_global;
            return eval_error("module publication lost during evaluation");
        }
        mod_idx = current_idx;
    }

    interp.global_env = saved_global;
    Module* loaded_mod = reacquire_module(name, mod_idx, interp);
    if (loaded_mod == null) return eval_error("module publication lost during evaluation");
    loaded_mod.loaded = true;
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

fn bool resolve_import_path_checked(char[] rel_path, Interp* interp, char* buf, usz buf_cap, usz* out_len) {
    if (buf == null || out_len == null || buf_cap == 0) return false;

    usz pos = 0;
    if (interp.source_dir_count > 0) {
        char* dir = interp.source_dirs[interp.source_dir_count - 1];
        usz di = 0;
        while (dir[di] != 0) {
            if (pos >= buf_cap - 1) return false;
            buf[pos] = dir[di];
            pos++;
            di++;
        }
    }

foreach (item : rel_path) {
        if (pos >= buf_cap - 1) return false;
        buf[pos] = item;
        pos++;
}

    buf[pos] = 0;
    *out_len = pos;
    return true;
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
    if (new_cap > usz.max / uptr.sizeof) return false;
    char** new_dirs = (char**)mem::realloc(interp.source_dirs, uptr.sizeof * new_cap);
    if (new_dirs == null) return false;
    interp.source_dirs = new_dirs;
    interp.source_dir_capacity = new_cap;
    return true;
}

fn bool push_source_dir(char[] file_path, Interp* interp) {
    usz dlen = path_dir_len(file_path);
    if (dlen == 0) return true;
    if (interp.source_dir_count == usz.max) return false;
    if (!ensure_source_dir_capacity(interp, interp.source_dir_count + 1)) return false;
    if (dlen == usz.max) return false;
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
    if (path.len == usz.max) {
        mod.path_len = 0;
        return false;
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

fn bool module_ensure_export_capacity(Module* mod) {
    if (mod == null) return false;
    if (mod.export_count < mod.export_capacity) return true;
    if (mod.export_capacity > usz.max / 2) return false;
    usz new_cap = mod.export_capacity * 2;
    if (new_cap <= mod.export_capacity) return false;
    if (new_cap > usz.max / SymbolId.sizeof) return false;
    SymbolId* new_exports = jit_alloc_module_exports(new_cap);
    if (new_exports == null) return false;
    for (usz i = 0; i < mod.export_count; i++) new_exports[i] = mod.exports[i];
    mem::free(mod.exports);
    mod.exports = new_exports;
    mod.export_capacity = new_cap;
    return true;
}

fn bool module_append_export(Module* mod, SymbolId name) {
    if (!module_ensure_export_capacity(mod)) return false;
    mod.exports[mod.export_count] = name;
    mod.export_count++;
    return true;
}

fn bool jit_create_file_module_index(SymbolId name, char[] path, Interp* interp, usz* mod_idx_out) {
    if (mod_idx_out == null) return false;
    *mod_idx_out = usz.max;
    if (interp.module_count >= interp.module_capacity && !interp.grow_module_table()) return false;
    Module* mod = &interp.modules[interp.module_count];
    mod.name = name;
    mod.loaded = false;
    mod.path = null;
    mod.path_len = 0;
    if (!module_copy_path(mod, path)) return false;
    mod.export_count = 0;
    mod.export_capacity = 32;
    mod.exports = jit_alloc_module_exports(32);
    if (mod.exports == null) {
        if (mod.path != null) mem::free(mod.path);
        *mod = {};
        return false;
    }

    mod.env = boundary_make_env_in_root(interp, interp.global_env);
    if (mod.env == null) {
        if (mod.path != null) mem::free(mod.path);
        mem::free(mod.exports);
        *mod = {};
        return false;
    }

    interp.module_count++;
    usz mod_idx = interp.module_count - 1;
    usz slot = (usz)name % interp.module_hash_capacity;
    while (interp.module_hash_index[slot] != usz.max) {
        slot = (slot + 1) % interp.module_hash_capacity;
    }
    interp.module_hash_index[slot] = mod_idx;
    *mod_idx_out = mod_idx;
    return true;
}

fn Module* jit_create_file_module(SymbolId name, char[] path, Interp* interp) {
    usz mod_idx = usz.max;
    if (!jit_create_file_module_index(name, path, interp, &mod_idx)) return null;
    return &interp.modules[mod_idx];
}
