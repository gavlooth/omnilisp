module lisp;

import std::io;
import std::core::mem;

fn Value* deduce_rule_test_make_symbol_value(Interp* interp, SymbolId sym) {
    Value* v = interp.alloc_value();
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

fn Value* deduce_rule_test_make_atom2(Interp* interp, SymbolId predicate, Value* a, Value* b) {
    Value* pred = deduce_rule_test_make_symbol_value(interp, predicate);
    return make_cons(interp, pred, make_cons(interp, a, make_cons(interp, b, make_nil(interp))));
}

fn Value* deduce_rule_test_dict_get(Interp* interp, Value* dict, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    Value* key_val = make_symbol(interp, interp.symbols.intern(key));
    return hashmap_get(dict.hashmap_val, key_val);
}

fn bool deduce_rule_test_index_matches(DeduceIndexDescriptor* idx, usz[] positions, bool require_unique) {
    if (idx == null || idx.column_positions == null) return false;
    if (idx.column_count != positions.len) return false;
    for (usz i = 0; i < positions.len; i++) {
        if (idx.column_positions[i] != positions[i]) return false;
    }
    if (require_unique && !idx.unique) return false;
    return true;
}

fn bool deduce_rule_test_schema_has_index(DeduceRelationSchema* schema, usz[] positions, bool require_unique) {
    if (schema == null || schema.indexes == null || schema.index_count == 0) return false;
    for (usz i = 0; i < schema.index_count; i++) {
        if (deduce_rule_test_index_matches(&schema.indexes[i], positions, require_unique)) {
            return true;
        }
    }
    return false;
}

fn bool deduce_rule_test_validate_default_indexes(DeduceRelationSchema* schema) {
    if (schema == null || schema.arity != 3) return false;
    usz[3] full = { 0, 1, 2 };
    usz[1] col0 = { 0 };
    usz[1] col1 = { 1 };
    usz[1] col2 = { 2 };
    return deduce_rule_test_schema_has_index(schema, full[..], true) &&
           deduce_rule_test_schema_has_index(schema, col0[..], false) &&
           deduce_rule_test_schema_has_index(schema, col1[..], false) &&
           deduce_rule_test_schema_has_index(schema, col2[..], false);
}

fn void run_deduce_rule_validation_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-ok (deduce/open 'memory)) "
        "  (define [relation db-rule-ok] parent (child) (ancestor)) "
        "  (define [relation db-rule-ok] ancestor (child) (ancestor)) "
        "  (deduce/rule! db-rule-ok '(ancestor ?x ?y) '(parent ?x ?y)) "
        "  true)",
        "[PASS] deduce rule! accepts valid rule",
        "[FAIL] deduce rule! accepts valid rule",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects arity mismatch",
        "(block "
        "  (define db-rule-arity (deduce/open 'memory)) "
        "  (define [relation db-rule-arity] parent (child) (ancestor)) "
        "  (define [relation db-rule-arity] ancestor (child) (ancestor)) "
        "  (deduce/rule! db-rule-arity '(ancestor ?x ?y) '(parent ?x)))",
        "predicate arity mismatch",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects relation-not-found",
        "(block "
        "  (define db-rule-missing-rel (deduce/open 'memory)) "
        "  (define [relation db-rule-missing-rel] ancestor (child) (ancestor)) "
        "  (deduce/rule! db-rule-missing-rel '(ancestor ?x ?y) '(parent ?x ?y)))",
        "predicate relation is not registered",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects unsafe head variable",
        "(block "
        "  (define db-rule-head-safe (deduce/open 'memory)) "
        "  (define [relation db-rule-head-safe] parent (child) (ancestor)) "
        "  (define [relation db-rule-head-safe] ancestor (child) (ancestor)) "
        "  (deduce/rule! db-rule-head-safe '(ancestor ?x ?z) '(parent ?x ?y)))",
        "head variable is not grounded by a positive body atom",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects unsafe negated variable",
        "(block "
        "  (define db-rule-neg-safe (deduce/open 'memory)) "
        "  (define [relation db-rule-neg-safe] parent (child) (ancestor)) "
        "  (define [relation db-rule-neg-safe] blocked (child) (ancestor)) "
        "  (define [relation db-rule-neg-safe] ancestor (child) (ancestor)) "
        "  (deduce/rule! db-rule-neg-safe '(ancestor ?x ?y) '(parent ?x ?y) '(not (blocked ?x ?z))))",
        "negated atom uses an unbound variable",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects negative recursion cycle",
        "(block "
        "  (define db-rule-cycle (deduce/open 'memory)) "
        "  (define [relation db-rule-cycle] base (x)) "
        "  (define [relation db-rule-cycle] p (x)) "
        "  (define [relation db-rule-cycle] q (x)) "
        "  (deduce/rule! db-rule-cycle '(p ?x) '(base ?x) '(q ?x)) "
        "  (deduce/rule! db-rule-cycle '(q ?x) '(base ?x) '(not (p ?x))))",
        "negative recursion cycle",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects aggregate terms in body atoms",
        "(block "
        "  (define db-rule-agg-body (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-body] parent (child) (ancestor)) "
        "  (define [relation db-rule-agg-body] agg_out (child) (total)) "
        "  (deduce/rule! db-rule-agg-body '(agg_out ?x ?y) '(parent ?x (sum ?y))))",
        "aggregate terms are only allowed in head projections",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects deferred aggregate functions",
        "(block "
        "  (define db-rule-agg-deferred (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-deferred] parent (child) (ancestor)) "
        "  (define [relation db-rule-agg-deferred] agg_out (child) (total)) "
        "  (deduce/rule! db-rule-agg-deferred '(agg_out ?x (avg ?y)) '(parent ?x ?y)))",
        "aggregate function is deferred",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects unknown aggregate functions",
        "(block "
        "  (define db-rule-agg-unknown (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-unknown] parent (child) (ancestor)) "
        "  (define [relation db-rule-agg-unknown] agg_out (child) (total)) "
        "  (deduce/rule! db-rule-agg-unknown '(agg_out ?x (median ?y)) '(parent ?x ?y)))",
        "unknown aggregate function",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects ungrounded aggregate inputs",
        "(block "
        "  (define db-rule-agg-unsafe (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-unsafe] parent (child) (ancestor)) "
        "  (define [relation db-rule-agg-unsafe] agg_out (child) (total)) "
        "  (deduce/rule! db-rule-agg-unsafe '(agg_out ?x (sum ?z)) '(parent ?x ?y)))",
        "aggregate input variable is not grounded by a positive body atom",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects shared aggregate head definitions",
        "(block "
        "  (define db-rule-agg-shared (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-shared] sale (dept) (amount)) "
        "  (define [relation db-rule-agg-shared] agg_out (dept) (total)) "
        "  (deduce/rule! db-rule-agg-shared '(agg_out ?x ?y) '(sale ?x ?y)) "
        "  (deduce/rule! db-rule-agg-shared '(agg_out ?x (sum ?y)) '(sale ?x ?y)))",
        "aggregate head predicates may not share definitions with other rules",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-rec (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-rec] edge (node) (dst)) "
        "  (define [relation db-rule-agg-rec] unit (node) (weight)) "
        "  (define [relation db-rule-agg-rec] path-total (node) (total)) "
        "  (deduce/fact! unit 0 1) "
        "  (deduce/fact! unit 1 1) "
        "  (deduce/fact! unit 2 1) "
        "  (deduce/fact! unit 3 1) "
        "  (deduce/fact! edge 0 1) "
        "  (deduce/fact! edge 1 2) "
        "  (deduce/fact! edge 2 3) "
        "  (deduce/rule! db-rule-agg-rec '(path-total ?x (sum ?n)) '(unit ?x ?n)) "
        "  (deduce/rule! db-rule-agg-rec '(path-total ?x (sum ?n)) '(edge ?x ?y) '(path-total ?y ?n)) "
        "  true)",
        "[PASS] deduce rule! accepts recursive aggregate sum rules",
        "[FAIL] deduce rule! accepts recursive aggregate sum rules",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "[PASS] deduce rule! rejects recursive avg aggregate rules",
        "(block "
        "  (define db-rule-agg-rec-avg (deduce/open 'memory)) "
        "  (define [relation db-rule-agg-rec-avg] edge (from) (to)) "
        "  (define [relation db-rule-agg-rec-avg] path-avg (from) (total)) "
        "  (deduce/rule! db-rule-agg-rec-avg '(path-avg ?x (avg ?n)) '(edge ?x ?y) '(path-avg ?y ?n)))",
        "aggregate function is deferred",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-dispatch (deduce/open 'memory)) "
        "  (define [relation db-rule-dispatch] parent (child) (ancestor)) "
        "  (define [relation db-rule-dispatch] ancestor (child) (ancestor)) "
        "  (deduce 'rule! db-rule-dispatch '(ancestor ?x ?y) '(parent ?x ?y)) "
        "  true)",
        "[PASS] deduce 'rule! dispatch alias",
        "[FAIL] deduce 'rule! dispatch alias",
        pass,
        fail
    );

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            SymbolId col_left = interp.symbols.intern("left");
            SymbolId col_right = interp.symbols.intern("right");
            SymbolId parent_sym = interp.symbols.intern("parent_mask");
            SymbolId path_sym = interp.symbols.intern("path_mask");
            Value* rel_parent = setup_deduce_relation_for_db(interp, db_val, parent_sym, col_left, col_right);
            Value* rel_path = setup_deduce_relation_for_db(interp, db_val, path_sym, col_left, col_right);
            ok = rel_parent != null && rel_parent.tag == FFI_HANDLE && rel_path != null && rel_path.tag == FFI_HANDLE;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            SymbolId var_z = interp.symbols.intern("?z");
            SymbolId parent_sym = interp.symbols.intern("parent_mask");
            SymbolId path_sym = interp.symbols.intern("path_mask");

            Value* head = deduce_rule_test_make_atom2(
                interp,
                path_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body1 = deduce_rule_test_make_atom2(
                interp,
                parent_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body2 = deduce_rule_test_make_atom2(
                interp,
                parent_sym,
                deduce_rule_test_make_symbol_value(interp, var_y),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );

            Value*[4] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body1;
            rule_args[3] = body2;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            ok = rv != null && rv.tag == VOID;
        }

        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            SymbolId parent_sym = interp.symbols.intern("parent_mask");
            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, parent_sym);
            bool schema_ok = schema != null && schema.index_count >= 3;
            bool sig_ok = db.rule_signature_count > 0;
            if (sig_ok) {
                DeduceRuleSignature sig = db.rule_signatures[db.rule_signature_count - 1];
                sig_ok = sig.body_count == 2 &&
                         sig.body_bound_masks != null &&
                         sig.body_selected_indexes != null &&
                         sig.body_bound_masks[0] == 0 &&
                         sig.body_selected_indexes[0] == -1 &&
                         sig.body_bound_masks[1] == 1 &&
                         sig.body_selected_indexes[1] >= 0;
            }

            if (schema_ok && sig_ok) {
                emit_pass_literal("[PASS] deduce rule! stores logical index metadata + bound masks");
                (*pass)++;
            } else {
                io::printn("[FAIL] deduce rule! stores logical index metadata + bound masks");
                (*fail)++;
            }
        } else {
            io::printn("[FAIL] deduce rule! stores logical index metadata + bound masks");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            SymbolId col_left = interp.symbols.intern("left");
            SymbolId col_right = interp.symbols.intern("right");
            SymbolId parent_sym = interp.symbols.intern("parent_lit");
            SymbolId out_sym = interp.symbols.intern("out_lit");
            Value* rel_parent = setup_deduce_relation_for_db(interp, db_val, parent_sym, col_left, col_right);
            Value* rel_out = setup_deduce_relation_for_db(interp, db_val, out_sym, col_left, col_right);
            ok = rel_parent != null && rel_parent.tag == FFI_HANDLE && rel_out != null && rel_out.tag == FFI_HANDLE;
        }

        if (ok) {
            SymbolId var_z = interp.symbols.intern("?z");
            SymbolId parent_sym = interp.symbols.intern("parent_lit");
            SymbolId out_sym = interp.symbols.intern("out_lit");
            Value* lit_alice_a = make_string(interp, "Alice");
            Value* lit_alice_b = make_string(interp, "Alice");

            Value* head = deduce_rule_test_make_atom2(
                interp,
                out_sym,
                lit_alice_a,
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body = deduce_rule_test_make_atom2(
                interp,
                parent_sym,
                lit_alice_b,
                deduce_rule_test_make_symbol_value(interp, var_z)
            );

            Value*[3] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            ok = rv != null && rv.tag == VOID;
        }

        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            bool sig_ok = db.rule_signature_count > 0;
            if (sig_ok) {
                DeduceRuleSignature sig = db.rule_signatures[db.rule_signature_count - 1];
                sig_ok = sig.body_count == 1 &&
                         sig.body_bound_masks != null &&
                         sig.body_selected_indexes != null &&
                         sig.body_bound_masks[0] == 1 &&
                         sig.body_selected_indexes[0] >= 0;
            }
            if (sig_ok) {
                emit_pass_literal("[PASS] deduce rule! bound-mask marks literal-bound columns");
                (*pass)++;
            } else {
                io::printn("[FAIL] deduce rule! bound-mask marks literal-bound columns");
                (*fail)++;
            }
        } else {
            io::printn("[FAIL] deduce rule! bound-mask marks literal-bound columns");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_src = interp.symbols.intern("plan_src");
        SymbolId rel_filter = interp.symbols.intern("plan_filter");
        SymbolId rel_out = interp.symbols.intern("plan_out");
        SymbolId col_left = interp.symbols.intern("left");
        SymbolId col_right = interp.symbols.intern("right");
        if (ok) {
            Value* src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_left, col_right);
            Value* filter = setup_deduce_relation_for_db(interp, db_val, rel_filter, col_left, col_right);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = src != null && src.tag == FFI_HANDLE &&
                 filter != null && filter.tag == FFI_HANDLE &&
                 out != null && out.tag == FFI_HANDLE;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            Value* lit_alice_a = make_string(interp, "Alice");
            Value* lit_alice_b = make_string(interp, "Alice");

            Value* head = deduce_rule_test_make_atom2(
                interp,
                rel_out,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_src = deduce_rule_test_make_atom2(
                interp,
                rel_src,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_filter = deduce_rule_test_make_atom2(
                interp,
                rel_filter,
                lit_alice_a,
                deduce_rule_test_make_symbol_value(interp, var_y)
            );

            Value*[4] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body_src;
            rule_args[3] = body_filter;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            ok = rv != null && rv.tag == VOID;
        }

        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            bool sig_ok = db.rule_signature_count > 0;
            if (sig_ok) {
                DeduceRuleSignature sig = db.rule_signatures[db.rule_signature_count - 1];
                sig_ok = sig.body_count == 2 &&
                         sig.body_predicates != null &&
                         sig.body_bound_masks != null &&
                         sig.body_selected_indexes != null &&
                         sig.body_filter_pushdown != null &&
                         sig.body_projection_masks != null &&
                         sig.body_predicates[0] == rel_filter &&
                         sig.body_bound_masks[0] == 1 &&
                         sig.body_filter_pushdown[0] &&
                         sig.body_projection_masks[0] == 2 &&
                         sig.body_selected_indexes[0] >= 0 &&
                         sig.body_predicates[1] == rel_src &&
                         sig.body_bound_masks[1] == 2 &&
                         sig.body_filter_pushdown[1] &&
                         sig.body_projection_masks[1] == 3 &&
                         sig.body_selected_indexes[1] >= 0;
            }
            ok = sig_ok;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce rule! planner orders conjunctive body by bound-index selectivity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! planner orders conjunctive body by bound-index selectivity");
            (*fail)++;
        }
    }

    run_deduce_rule_validation_tests_explain(interp, pass, fail);
}
