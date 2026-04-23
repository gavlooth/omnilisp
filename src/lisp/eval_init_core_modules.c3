module lisp;

struct CoreModuleExport {
    char[] export_name;
    char[] source_name;
}

struct CoreModulePrimitiveExport {
    char[] export_name;
    char[] primitive_name;
    PrimitiveFn func;
    int arity;
}

fn bool core_module_insert_hash(Interp* interp, SymbolId name, usz mod_idx) {
    if (interp == null || interp.module_hash_index == null || interp.module_hash_capacity == 0) return false;
    usz slot = (usz)name % interp.module_hash_capacity;
    for (usz probe = 0; probe < interp.module_hash_capacity; probe++) {
        if (interp.module_hash_index[slot] == usz.max) {
            interp.module_hash_index[slot] = mod_idx;
            return true;
        }
        slot = (slot + 1) % interp.module_hash_capacity;
    }
    return false;
}

fn bool core_module_bind_export(Interp* interp, Module* mod, CoreModuleExport spec) {
    SymbolId export_sym = interp.symbols.intern(spec.export_name);
    SymbolId source_sym = interp.symbols.intern(spec.source_name);
    if (export_sym == INVALID_SYMBOL_ID || source_sym == INVALID_SYMBOL_ID) return false;
    if (mod == null || mod.exports == null || mod.env == null) return false;
    if (mod.export_count >= mod.export_capacity) return false;

    Value* source = interp.global_env.lookup(source_sym);
    if (source == null) return false;
    if (!env_define_with_barrier_checked(mod.env, export_sym, source, interp)) return false;
    mod.exports[mod.export_count] = export_sym;
    mod.export_count++;
    return true;
}

fn bool core_module_bind_primitive_export(Interp* interp, Module* mod, CoreModulePrimitiveExport spec) {
    SymbolId export_sym = interp.symbols.intern(spec.export_name);
    if (export_sym == INVALID_SYMBOL_ID) return false;
    if (mod == null || mod.exports == null || mod.env == null) return false;
    if (mod.export_count >= mod.export_capacity) return false;

    Value* prim = make_primitive(interp, spec.primitive_name, spec.func, spec.arity);
    if (prim == null) return false;
    if (!env_define_with_barrier_checked(mod.env, export_sym, prim, interp)) return false;
    mod.exports[mod.export_count] = export_sym;
    mod.export_count++;
    return true;
}

fn bool register_core_module_facade(
    Interp* interp,
    char[] module_name,
    CoreModuleExport[] exports,
    CoreModulePrimitiveExport[] primitive_exports
) {
    SymbolId module_sym = interp.symbols.intern(module_name);
    if (module_sym == INVALID_SYMBOL_ID) return false;

    Module* existing = find_module(module_sym, interp);
    if (existing != null) {
        if (!existing.loaded) return false;
        Value* existing_val = make_module(interp, existing);
        if (existing_val == null || existing_val.tag == ERROR) return false;
        return env_define_with_barrier_checked(interp.global_env, module_sym, existing_val, interp);
    }

    if (interp.module_count >= interp.module_capacity && !interp.grow_module_table()) return false;

    Module* mod = &interp.modules[interp.module_count];
    *mod = {};
    mod.name = module_sym;
    mod.loaded = false;
    mod.path = null;
    mod.path_len = 0;
    usz total_exports = exports.len + primitive_exports.len;
    mod.export_capacity = total_exports > 0 ? total_exports : 1;
    mod.exports = jit_alloc_module_exports(mod.export_capacity);
    if (mod.exports == null) {
        *mod = {};
        return false;
    }
    mod.env = boundary_make_env_in_root(interp, interp.global_env);
    if (mod.env == null) {
        mem::free(mod.exports);
        *mod = {};
        return false;
    }

    foreach (spec : exports) {
        if (!core_module_bind_export(interp, mod, spec)) {
            mem::free(mod.exports);
            *mod = {};
            return false;
        }
    }
    foreach (spec : primitive_exports) {
        if (!core_module_bind_primitive_export(interp, mod, spec)) {
            mem::free(mod.exports);
            *mod = {};
            return false;
        }
    }

    usz mod_idx = interp.module_count;
    interp.module_count++;
    if (!core_module_insert_hash(interp, module_sym, mod_idx)) {
        rollback_module_publication(interp, mod_idx);
        return false;
    }

    mod.loaded = true;
    Value* mod_val = make_module(interp, mod);
    if (mod_val == null || mod_val.tag == ERROR) {
        rollback_module_publication(interp, mod_idx);
        return false;
    }
    if (!env_define_with_barrier_checked(interp.global_env, module_sym, mod_val, interp)) {
        rollback_module_publication(interp, mod_idx);
        return false;
    }
    return true;
}

fn bool register_math_stats_core_modules(Interp* interp) {
    CoreModuleExport[] math_exports = {
        { "abs", "abs" },
        { "sqrt", "sqrt" },
        { "sin", "sin" },
        { "cos", "cos" },
        { "tan", "tan" },
        { "asin", "asin" },
        { "acos", "acos" },
        { "atan", "atan" },
        { "atan2", "atan2" },
        { "sinh", "sinh" },
        { "cosh", "cosh" },
        { "tanh", "tanh" },
        { "exp", "exp" },
        { "log", "log" },
        { "log10", "log10" },
        { "pow", "pow" },
        { "floor", "floor" },
        { "ceiling", "ceiling" },
        { "round", "round" },
        { "truncate", "truncate" },
        { "real-part", "real-part" },
        { "imag-part", "imag-part" },
        { "conjugate", "conjugate" },
        { "gcd", "gcd" },
        { "lcm", "lcm" },
    };
    CoreModulePrimitiveExport[] math_primitives = {
        { "lgamma", "math.lgamma", &prim_math_lgamma, 1 },
        { "erf", "math.erf", &prim_math_erf, 1 },
        { "erfc", "math.erfc", &prim_math_erfc, 1 },
    };
    CoreModuleExport[] stats_exports = {
    };
    CoreModulePrimitiveExport[] stats_primitives = {
        { "normal-cdf", "stats.normal-cdf", &prim_stats_normal_cdf, 1 },
        { "normal-quantile", "stats.normal-quantile", &prim_stats_normal_quantile, 1 },
    };

    if (!register_core_module_facade(interp, "math", math_exports, math_primitives)) return false;
    return register_core_module_facade(interp, "stats", stats_exports, stats_primitives);
}
