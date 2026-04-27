module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (utilities)
// =============================================================================
fn void Compiler.collect_pattern_bindings(Compiler* self, Pattern* pat, List{SymbolId}* bindings) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            bindings.push(pat.var_name);

        case PAT_CONS:
            self.collect_pattern_bindings(pat.car_pat, bindings);
            self.collect_pattern_bindings(pat.cdr_pat, bindings);

        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                self.collect_pattern_bindings(pat.ctor_sub_patterns[i], bindings);
            }

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                self.collect_pattern_bindings(pat.elements[i], bindings);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                bindings.push(pat.rest_binding);
            }

        case PAT_AS:
            self.collect_pattern_bindings(pat.as_sub, bindings);
            bindings.push(pat.as_name);

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.collect_pattern_bindings(pat.guard_sub, bindings);
            }

        case PAT_DICT:
            for (usz i = 0; i < pat.dict_key_count; i++) {
                bindings.push(pat.dict_keys[i]);
            }

        case PAT_WILDCARD:
        case PAT_LIT:
        case PAT_QUOTE:
            return;

        default:
            self.set_compile_error("compiler: unknown pattern tag in free-variable binding collection");
            return;
    }
}

fn bool pattern_binds_name(Pattern* pat, SymbolId name) {
    if (pat == null) return false;
    switch (pat.tag) {
        case PAT_VAR:
            return (uint)pat.var_name == (uint)name;
        case PAT_AS:
            return (uint)pat.as_name == (uint)name || pattern_binds_name(pat.as_sub, name);
        case PAT_GUARD:
            return pattern_binds_name(pat.guard_sub, name);
        case PAT_CONS:
            return pattern_binds_name(pat.car_pat, name) || pattern_binds_name(pat.cdr_pat, name);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (pattern_binds_name(pat.ctor_sub_patterns[i], name)) return true;
            }
            return false;
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (pattern_binds_name(pat.elements[i], name)) return true;
            }
            if (pat.rest_pos == REST_MIDDLE && (uint)pat.rest_binding == (uint)name) return true;
            return false;
        case PAT_DICT:
            for (usz i = 0; i < pat.dict_key_count; i++) {
                if ((uint)pat.dict_keys[i] == (uint)name) return true;
            }
            return false;
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_WILDCARD:
            return false;
    }
}

fn bool Compiler.is_compiler_builtin_symbol(Compiler* self, SymbolId sym) {
    if (prim_hash_lookup(sym).len > 0) return true;

    // Non-value type annotation / legacy UI bridge entries that were historically
    // excluded from closure capture but do not have normal AOT value lowering.
    char[] name = self.interp.symbols.get_name(sym);
    return self.str_eq(name, "ForeignHandle") ||
           self.str_eq(name, "__ui-ftxui-run") ||
           self.str_eq(name, "__ui-ftxui-dispatch") ||
           self.str_eq(name, "__ui-ftxui-loop");
}

fn bool Compiler.is_primitive(Compiler* self, SymbolId sym) {
    if (self.is_compiler_builtin_symbol(sym)) return true;

    // Check if it's a defined global
    foreach (g : self.defined_globals) {
        if ((uint)g == (uint)sym) return true;
    }

    return false;
}

fn bool Compiler.is_mutable_captured_var(Compiler* self, SymbolId sym) {
    foreach (mc : self.mutable_captures) {
        if ((uint)mc == (uint)sym) return true;
    }
    return false;
}

fn bool Compiler.str_eq(Compiler* self, char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}
