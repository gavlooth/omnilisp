module lisp;


import std::io;
import main;
fn bool advanced_test_append(char* buf, usz cap, usz* len, char[] piece) {
    if (*len + piece.len + 1 > cap) return false;
    for (usz i = 0; i < piece.len; i++) {
        buf[*len + i] = piece[i];
    }
    *len += piece.len;
    buf[*len] = 0;
    return true;
}

fn bool advanced_test_append_int(char* buf, usz cap, usz* len, usz value) {
    char[32] num_buf;
    char[] num_text = io::bprintf(&num_buf, "%d", (int)value)!!;
    return advanced_test_append(buf, cap, len, num_text);
}

fn usz advanced_test_cstr_len(char* buf) {
    usz len = 0;
    while (buf[len] != 0) len++;
    return len;
}

fn bool advanced_build_wide_type_define(char* buf, usz cap, usz field_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [type] WideRecord")) return false;
    for (usz i = 0; i < field_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (^Int f")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, ")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_wide_type_access(char* buf, usz cap, usz field_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(let (p (WideRecord")) return false;
    for (usz i = 0; i < field_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")) p.f")) return false;
    if (!advanced_test_append_int(buf, cap, &len, field_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_wide_union_define(char* buf, usz cap, usz variant_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [union] WideChoice")) return false;
    for (usz i = 0; i + 1 < variant_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " V")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (V")) return false;
    if (!advanced_test_append_int(buf, cap, &len, variant_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, " Int))")) return false;
    return true;
}

fn bool advanced_build_wide_union_match(char* buf, usz cap, usz variant_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(match (V")) return false;
    if (!advanced_test_append_int(buf, cap, &len, variant_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, " ")) return false;
    if (!advanced_test_append_int(buf, cap, &len, variant_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    for (usz i = 0; i + 1 < variant_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (V")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, " 0)")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " ((V")) return false;
    if (!advanced_test_append_int(buf, cap, &len, variant_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, " x) x))")) return false;
    return true;
}

fn bool advanced_build_many_type_params_define(char* buf, usz cap, usz param_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [type] (ParamTower")) return false;
    for (usz i = 0; i < param_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " T")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ") (^T")) return false;
    if (!advanced_test_append_int(buf, cap, &len, param_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, " tail))")) return false;
    return true;
}

fn bool advanced_build_many_meta_entries_define(char* buf, usz cap, usz meta_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [type] (MetaTower")) return false;
    for (usz i = 0; i < meta_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " T")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ") (^{")) return false;
    for (usz i = 0; i < meta_count; i++) {
        if (!advanced_test_append(buf, cap, &len, "'T")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, " Number")) return false;
        if (i + 1 < meta_count) {
            if (!advanced_test_append(buf, cap, &len, " ")) return false;
        }
    }
    if (!advanced_test_append(buf, cap, &len, "} value))")) return false;
    return true;
}

fn bool advanced_build_many_clause_macro_define(char* buf, usz cap, usz clause_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [macro] choose-wide")) return false;
    for (usz i = 0; i < clause_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " ([")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "] ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, ")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_many_pattern_vars_macro_define(char* buf, usz cap, usz var_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [macro] pick-last ([")) return false;
    for (usz i = 0; i < var_count; i++) {
        if (i > 0 && !advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append(buf, cap, &len, "v")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, "] v")) return false;
    if (!advanced_test_append_int(buf, cap, &len, var_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, "))")) return false;
    return true;
}

fn bool advanced_build_many_arg_macro_call(char* buf, usz cap, char[] macro_name, usz arg_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(")) return false;
    if (!advanced_test_append(buf, cap, &len, macro_name)) return false;
    for (usz i = 0; i < arg_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_many_capture_bindings_setup(char* buf, usz cap, usz binding_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(begin")) return false;
    for (usz i = 0; i < binding_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (define cap")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, ")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_many_capture_macro_define(char* buf, usz cap, usz binding_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [macro] capture-wide ([] (begin")) return false;
    for (usz i = 0; i < binding_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " cap")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")))")) return false;
    return true;
}

fn bool advanced_build_many_gensym_macro_define(char* buf, usz cap, usz gensym_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define [macro] gensym-wide ([] (let (")) return false;
    for (usz i = 0; i < gensym_count; i++) {
        if (i > 0 && !advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append(buf, cap, &len, "g")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "# ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ") g")) return false;
    if (!advanced_test_append_int(buf, cap, &len, gensym_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, "#)))")) return false;
    return true;
}

fn bool advanced_build_dispatch_define(char* buf, usz cap, char[] name, usz arg_count, char[] last_type, long result) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define (")) return false;
    if (!advanced_test_append(buf, cap, &len, name)) return false;
    for (usz i = 0; i + 1 < arg_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " a")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (^")) return false;
    if (!advanced_test_append(buf, cap, &len, last_type)) return false;
    if (!advanced_test_append(buf, cap, &len, " a")) return false;
    if (!advanced_test_append_int(buf, cap, &len, arg_count - 1)) return false;
    if (!advanced_test_append(buf, cap, &len, ")) ")) return false;
    if (!advanced_test_append_int(buf, cap, &len, (usz)result)) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_dispatch_call(char* buf, usz cap, char[] name, usz arg_count, bool string_last) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(")) return false;
    if (!advanced_test_append(buf, cap, &len, name)) return false;
    for (usz i = 0; i + 1 < arg_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " ")) return false;
    if (string_last) {
        if (!advanced_test_append(buf, cap, &len, "\"forty\"")) return false;
    } else {
        if (!advanced_test_append_int(buf, cap, &len, arg_count - 1)) return false;
    }
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool advanced_build_constraint_define(char* buf, usz cap, usz constraint_count, char[] final_bound, long result) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(define (constraint-wide (^{")) return false;
    for (usz i = 0; i < constraint_count; i++) {
        if (i > 0 && !advanced_test_append(buf, cap, &len, " ")) return false;
        if (!advanced_test_append(buf, cap, &len, "'T")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, " ")) return false;
        if (i + 1 == constraint_count) {
            if (!advanced_test_append(buf, cap, &len, final_bound)) return false;
        } else {
            if (!advanced_test_append(buf, cap, &len, "Number")) return false;
        }
    }
    if (!advanced_test_append(buf, cap, &len, "} x)) ")) return false;
    if (!advanced_test_append_int(buf, cap, &len, (usz)result)) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}


fn void run_advanced_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Advanced Feature Tests ---");
    run_advanced_core_semantics_tests(interp, pass, fail);
    run_advanced_unicode_iterator_tests(interp, pass, fail);
    run_advanced_logic_tco_tests(interp, pass, fail);
    run_advanced_effect_continuation_tests(interp, pass, fail);
    run_advanced_runtime_control_tests(interp, pass, fail);
    run_advanced_lambda_syntax_tests(interp, pass, fail);
    run_advanced_binding_mutation_tests(interp, pass, fail);
    run_advanced_macro_hygiene_tests(interp, pass, fail);
    run_advanced_collections_module_tests(interp, pass, fail);
    run_advanced_stdlib_numeric_tests(interp, pass, fail);
    run_advanced_type_parametric_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_tests(interp, pass, fail);
    run_advanced_mutation_hierarchy_dispatch_tests(interp, pass, fail);
    run_advanced_effect_union_limit_tests(interp, pass, fail);
    run_advanced_ffi_system_tests(interp, pass, fail);
}
