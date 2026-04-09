module lisp;

import std::io;
import std::core::mem;

fn bool deduce_rule_test_build_recursive_limit_expr(char* buf, usz cap, usz fact_count, char[] payload_expr) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(block ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define db-limit (deduce/open 'memory)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] parent-limit (src) (dst)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] ancestor-limit (src) (dst)) ")) return false;
    for (usz i = 0; i + 1 < fact_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (deduce/fact! parent-limit \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "\" \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i + 1)) return false;
        if (!advanced_test_append(buf, cap, &len, "\")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?y) '(parent-limit ?x ?y)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?z) '(parent-limit ?x ?y) '(ancestor-limit ?y ?z)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, "(handle (deduce/analyze db-limit) (raise msg ")) return false;
    if (!advanced_test_append(buf, cap, &len, payload_expr)) return false;
    if (!advanced_test_append(buf, cap, &len, "))")) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool deduce_rule_test_build_recursive_limit_error_expr(char* buf, usz cap, usz fact_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(block ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define db-limit (deduce/open 'memory)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] parent-limit (src) (dst)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] ancestor-limit (src) (dst)) ")) return false;
    for (usz i = 0; i + 1 < fact_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (deduce/fact! parent-limit \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "\" \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i + 1)) return false;
        if (!advanced_test_append(buf, cap, &len, "\")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?y) '(parent-limit ?x ?y)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?z) '(parent-limit ?x ?y) '(ancestor-limit ?y ?z)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, "(deduce/analyze db-limit)")) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool deduce_rule_test_capture_recursive_limit_code(
    Interp* interp,
    usz fact_count,
    char[] expected_code
) {
    if (interp == null || fact_count < 2) return false;

    SymbolId sym_memory = interp.symbols.intern("memory");
    SymbolId rel_parent = interp.symbols.intern("parent_limit_direct");
    SymbolId rel_ancestor = interp.symbols.intern("ancestor_limit_direct");
    SymbolId col_src = interp.symbols.intern("src");
    SymbolId col_dst = interp.symbols.intern("dst");
    SymbolId var_x = interp.symbols.intern("?x");
    SymbolId var_y = interp.symbols.intern("?y");
    SymbolId var_z = interp.symbols.intern("?z");

    Value*[1] open_args;
    open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
    Value* db_val = prim_deduce_open(open_args[..], null, interp);
    if (db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return false;
    }

    Value* parent_rel = setup_deduce_relation_for_db(interp, db_val, rel_parent, col_src, col_dst);
    Value* ancestor_rel = setup_deduce_relation_for_db(interp, db_val, rel_ancestor, col_src, col_dst);
    if (parent_rel == null || parent_rel.tag != FFI_HANDLE || ancestor_rel == null || ancestor_rel.tag != FFI_HANDLE) {
        return false;
    }

    for (usz i = 0; i + 1 < fact_count; i++) {
        char[32] src_buf = {};
        char[32] dst_buf = {};
        char[] src;
        char[] dst;
        if (try char[] rendered_src = io::bprintf(&src_buf, "%d", (int)i)) {
            src = rendered_src;
        } else {
            return false;
        }
        if (try char[] rendered_dst = io::bprintf(&dst_buf, "%d", (int)(i + 1))) {
            dst = rendered_dst;
        } else {
            return false;
        }
        Value*[3] fact_args = {
            parent_rel,
            make_string(interp, src),
            make_string(interp, dst),
        };
        Value* fact_rv = prim_assert_fact(fact_args[..], null, interp);
        if (fact_rv == null || fact_rv.tag != VOID) return false;
    }

    Value* head_base = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* body_base = deduce_rule_test_make_atom2(
        interp,
        rel_parent,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* head_rec = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_z)
    );
    Value* body_rec_left = deduce_rule_test_make_atom2(
        interp,
        rel_parent,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* body_rec_right = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_y),
        deduce_rule_test_make_symbol_value(interp, var_z)
    );

    Value*[3] rule_base_args = { db_val, head_base, body_base };
    Value*[4] rule_rec_args = { db_val, head_rec, body_rec_left, body_rec_right };
    Value* rule_base_rv = prim_deduce_rule(rule_base_args[..], null, interp);
    Value* rule_rec_rv = prim_deduce_rule(rule_rec_args[..], null, interp);
    if (rule_base_rv == null || rule_base_rv.tag != VOID || rule_rec_rv == null || rule_rec_rv.tag != VOID) {
        return false;
    }

    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    SymbolId[1] handler_tags = { interp.sym_raise };
    EffectHandler handler = {
        .tags = &handler_tags[0],
        .clauses = null,
        .closures = null,
        .clause_count = 1,
        .handler_env = null,
        .body_expr = null,
        .body_env = null,
        .strict_mode = false,
        .effect_state = null,
    };
    interp.handler_stack[interp.handler_count++] = handler;

    Value*[1] analyze_args = { db_val };
    Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
    interp.handler_count--;

    bool ok = analyzed != null &&
              analyzed.tag == ERROR &&
              interp.flags.raise_pending &&
              interp.raise_payload != null &&
              interp.raise_payload.tag == HASHMAP &&
              interp.raise_payload.hashmap_val != null;
    if (ok) {
        Value* code = deduce_rule_test_dict_get(interp, interp.raise_payload, "code");
        ok = code != null &&
             code.tag == SYMBOL &&
             deduce_name_equals(interp.symbols.get_name(code.sym_val), expected_code);
    }

    run_clear_stale_raise_state(interp);
    return ok;
}

fn bool deduce_rule_test_capture_recursive_limit_message_contains(
    Interp* interp,
    usz fact_count,
    char[] expected_substr
) {
    if (interp == null || fact_count < 2) return false;

    SymbolId sym_memory = interp.symbols.intern("memory");
    SymbolId rel_parent = interp.symbols.intern("parent_limit_message");
    SymbolId rel_ancestor = interp.symbols.intern("ancestor_limit_message");
    SymbolId col_src = interp.symbols.intern("src");
    SymbolId col_dst = interp.symbols.intern("dst");
    SymbolId var_x = interp.symbols.intern("?x");
    SymbolId var_y = interp.symbols.intern("?y");
    SymbolId var_z = interp.symbols.intern("?z");

    Value*[1] open_args;
    open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
    Value* db_val = prim_deduce_open(open_args[..], null, interp);
    if (db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return false;
    }

    Value* parent_rel = setup_deduce_relation_for_db(interp, db_val, rel_parent, col_src, col_dst);
    Value* ancestor_rel = setup_deduce_relation_for_db(interp, db_val, rel_ancestor, col_src, col_dst);
    if (parent_rel == null || parent_rel.tag != FFI_HANDLE || ancestor_rel == null || ancestor_rel.tag != FFI_HANDLE) {
        return false;
    }

    for (usz i = 0; i + 1 < fact_count; i++) {
        char[32] src_buf = {};
        char[32] dst_buf = {};
        char[] src;
        char[] dst;
        if (try char[] rendered_src = io::bprintf(&src_buf, "%d", (int)i)) {
            src = rendered_src;
        } else {
            return false;
        }
        if (try char[] rendered_dst = io::bprintf(&dst_buf, "%d", (int)(i + 1))) {
            dst = rendered_dst;
        } else {
            return false;
        }
        Value*[3] fact_args = {
            parent_rel,
            make_string(interp, src),
            make_string(interp, dst),
        };
        Value* fact_rv = prim_assert_fact(fact_args[..], null, interp);
        if (fact_rv == null || fact_rv.tag != VOID) return false;
    }

    Value* head_base = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* body_base = deduce_rule_test_make_atom2(
        interp,
        rel_parent,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* head_rec = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_z)
    );
    Value* body_rec_left = deduce_rule_test_make_atom2(
        interp,
        rel_parent,
        deduce_rule_test_make_symbol_value(interp, var_x),
        deduce_rule_test_make_symbol_value(interp, var_y)
    );
    Value* body_rec_right = deduce_rule_test_make_atom2(
        interp,
        rel_ancestor,
        deduce_rule_test_make_symbol_value(interp, var_y),
        deduce_rule_test_make_symbol_value(interp, var_z)
    );

    Value*[3] rule_base_args = { db_val, head_base, body_base };
    Value*[4] rule_rec_args = { db_val, head_rec, body_rec_left, body_rec_right };
    Value* rule_base_rv = prim_deduce_rule(rule_base_args[..], null, interp);
    Value* rule_rec_rv = prim_deduce_rule(rule_rec_args[..], null, interp);
    if (rule_base_rv == null || rule_base_rv.tag != VOID || rule_rec_rv == null || rule_rec_rv.tag != VOID) {
        return false;
    }

    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    SymbolId[1] handler_tags = { interp.sym_raise };
    EffectHandler handler = {
        .tags = &handler_tags[0],
        .clauses = null,
        .closures = null,
        .clause_count = 1,
        .handler_env = null,
        .body_expr = null,
        .body_env = null,
        .strict_mode = false,
        .effect_state = null,
    };
    interp.handler_stack[interp.handler_count++] = handler;

    Value*[1] analyze_args = { db_val };
    Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
    interp.handler_count--;

    bool ok = analyzed != null &&
              analyzed.tag == ERROR &&
              interp.flags.raise_pending &&
              interp.raise_payload != null &&
              interp.raise_payload.tag == HASHMAP &&
              interp.raise_payload.hashmap_val != null;
    if (ok) {
        Value* message = deduce_rule_test_dict_get(interp, interp.raise_payload, "message");
        ok = message != null &&
             message.tag == STRING &&
             str_starts_with(message.str_chars[:message.str_len], expected_substr);
    }

    run_clear_stale_raise_state(interp);
    return ok;
}

fn void run_deduce_rule_validation_tests_more_tail(Interp* interp, int* pass, int* fail) {
    run_deduce_rule_validation_tests_more_tail_schema_tests(interp, pass, fail);
    run_deduce_rule_validation_tests_more_tail_support_prelude_tests(interp, pass, fail);
    run_deduce_rule_validation_tests_more_tail_analysis_tests(interp, pass, fail);
    run_deduce_rule_validation_tests_more_tail_support_followup_tests(interp, pass, fail);
    run_deduce_rule_validation_tests_more_tail_recursive_tests(interp, pass, fail);
}
