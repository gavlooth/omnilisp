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
    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-ok (deduce/open 'memory)) "
            "  (define [relation db-rule-ok] parent (child) (ancestor)) "
            "  (define [relation db-rule-ok] ancestor (child) (ancestor)) "
            "  (deduce/rule! db-rule-ok '(ancestor ?x ?y) '(parent ?x ?y)) "
            "  true)",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce rule! accepts valid rule");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! accepts valid rule");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-arity (deduce/open 'memory)) "
            "  (define [relation db-rule-arity] parent (child) (ancestor)) "
            "  (define [relation db-rule-arity] ancestor (child) (ancestor)) "
            "  (deduce/rule! db-rule-arity '(ancestor ?x ?y) '(parent ?x)))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce rule! rejects arity mismatch");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! rejects arity mismatch");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-head-safe (deduce/open 'memory)) "
            "  (define [relation db-rule-head-safe] parent (child) (ancestor)) "
            "  (define [relation db-rule-head-safe] ancestor (child) (ancestor)) "
            "  (deduce/rule! db-rule-head-safe '(ancestor ?x ?z) '(parent ?x ?y)))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce rule! rejects ungrounded head variable");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! rejects ungrounded head variable");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-neg-safe (deduce/open 'memory)) "
            "  (define [relation db-rule-neg-safe] parent (child) (ancestor)) "
            "  (define [relation db-rule-neg-safe] blocked (child) (ancestor)) "
            "  (define [relation db-rule-neg-safe] ancestor (child) (ancestor)) "
            "  (deduce/rule! db-rule-neg-safe '(ancestor ?x ?y) '(parent ?x ?y) '(not (blocked ?x ?z))))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce rule! rejects unbound negated variable");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! rejects unbound negated variable");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-cycle (deduce/open 'memory)) "
            "  (define [relation db-rule-cycle] p (x)) "
            "  (define [relation db-rule-cycle] q (x)) "
            "  (deduce/rule! db-rule-cycle '(p ?x) '(q ?x)) "
            "  (deduce/rule! db-rule-cycle '(q ?x) '(not (p ?x))))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce rule! rejects negative recursion cycle");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule! rejects negative recursion cycle");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-dispatch (deduce/open 'memory)) "
            "  (define [relation db-rule-dispatch] parent (child) (ancestor)) "
            "  (define [relation db-rule-dispatch] ancestor (child) (ancestor)) "
            "  (deduce 'rule! db-rule-dispatch '(ancestor ?x ?y) '(parent ?x ?y)) "
            "  true)",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce 'rule! dispatch alias");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'rule! dispatch alias");
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

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_src = interp.symbols.intern("plan_explain_src");
        SymbolId rel_filter = interp.symbols.intern("plan_explain_filter");
        SymbolId rel_out = interp.symbols.intern("plan_explain_out");
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
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, rel_out);
            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null &&
                 explained.tag == HASHMAP &&
                 explained.hashmap_val != null;
            if (ok) {
                SymbolId sym_deduce_plan = interp.symbols.intern("deduce-plan");
                SymbolId sym_index_scan = interp.symbols.intern("index-scan");

                Value* kind = deduce_rule_test_dict_get(interp, explained, "kind");
                Value* head_pred = deduce_rule_test_dict_get(interp, explained, "head-predicate");
                Value* join_order = deduce_rule_test_dict_get(interp, explained, "join-order");
                Value* steps = deduce_rule_test_dict_get(interp, explained, "steps");

                ok = kind != null && kind.tag == SYMBOL && kind.sym_val == sym_deduce_plan &&
                     head_pred != null && head_pred.tag == SYMBOL && head_pred.sym_val == rel_out &&
                     is_cons(join_order) &&
                     car(join_order) != null &&
                     car(join_order).tag == SYMBOL &&
                     car(join_order).sym_val == rel_filter &&
                     is_cons(cdr(join_order)) &&
                     car(cdr(join_order)) != null &&
                     car(cdr(join_order)).tag == SYMBOL &&
                     car(cdr(join_order)).sym_val == rel_src &&
                     is_nil(cdr(cdr(join_order))) &&
                     is_cons(steps) &&
                     is_cons(cdr(steps)) &&
                     is_nil(cdr(cdr(steps)));

                if (ok) {
                    Value* step0 = car(steps);
                    Value* step1 = car(cdr(steps));
                    Value* step0_pred = deduce_rule_test_dict_get(interp, step0, "predicate");
                    Value* step0_op = deduce_rule_test_dict_get(interp, step0, "operator");
                    Value* step0_mask = deduce_rule_test_dict_get(interp, step0, "bound-mask");
                    Value* step0_idx = deduce_rule_test_dict_get(interp, step0, "selected-index");
                    Value* step0_filter_pushdown = deduce_rule_test_dict_get(interp, step0, "filter-pushdown");
                    Value* step0_projection_mask = deduce_rule_test_dict_get(interp, step0, "projection-mask");
                    Value* step0_counters = deduce_rule_test_dict_get(interp, step0, "counters");
                    Value* step1_pred = deduce_rule_test_dict_get(interp, step1, "predicate");
                    Value* step1_op = deduce_rule_test_dict_get(interp, step1, "operator");
                    Value* step1_mask = deduce_rule_test_dict_get(interp, step1, "bound-mask");
                    Value* step1_idx = deduce_rule_test_dict_get(interp, step1, "selected-index");
                    Value* step1_filter_pushdown = deduce_rule_test_dict_get(interp, step1, "filter-pushdown");
                    Value* step1_projection_mask = deduce_rule_test_dict_get(interp, step1, "projection-mask");
                    Value* step1_counters = deduce_rule_test_dict_get(interp, step1, "counters");
                    SymbolId sym_estimated = interp.symbols.intern("estimated");

                    Value* step0_rows_read = deduce_rule_test_dict_get(interp, step0_counters, "rows-read");
                    Value* step0_rows_emitted = deduce_rule_test_dict_get(interp, step0_counters, "rows-emitted");
                    Value* step0_index_hits = deduce_rule_test_dict_get(interp, step0_counters, "index-hits");
                    Value* step0_index_misses = deduce_rule_test_dict_get(interp, step0_counters, "index-misses");
                    Value* step0_counter_kind = deduce_rule_test_dict_get(interp, step0_counters, "counter-kind");
                    Value* step1_rows_read = deduce_rule_test_dict_get(interp, step1_counters, "rows-read");
                    Value* step1_rows_emitted = deduce_rule_test_dict_get(interp, step1_counters, "rows-emitted");
                    Value* step1_index_hits = deduce_rule_test_dict_get(interp, step1_counters, "index-hits");
                    Value* step1_index_misses = deduce_rule_test_dict_get(interp, step1_counters, "index-misses");
                    Value* step1_counter_kind = deduce_rule_test_dict_get(interp, step1_counters, "counter-kind");

                    ok = step0_pred != null && step0_pred.tag == SYMBOL && step0_pred.sym_val == rel_filter &&
                         step0_op != null && step0_op.tag == SYMBOL && step0_op.sym_val == sym_index_scan &&
                         step0_mask != null && step0_mask.tag == INT && step0_mask.int_val == 1 &&
                         step0_idx != null && step0_idx.tag == INT && step0_idx.int_val >= 0 &&
                         step0_filter_pushdown != null && step0_filter_pushdown.tag == SYMBOL &&
                         step0_filter_pushdown.sym_val == interp.sym_true &&
                         step0_projection_mask != null && step0_projection_mask.tag == INT &&
                         step0_projection_mask.int_val == 2 &&
                         step1_pred != null && step1_pred.tag == SYMBOL && step1_pred.sym_val == rel_src &&
                         step1_op != null && step1_op.tag == SYMBOL && step1_op.sym_val == sym_index_scan &&
                         step1_mask != null && step1_mask.tag == INT && step1_mask.int_val == 2 &&
                         step1_idx != null && step1_idx.tag == INT && step1_idx.int_val >= 0 &&
                         step1_filter_pushdown != null && step1_filter_pushdown.tag == SYMBOL &&
                         step1_filter_pushdown.sym_val == interp.sym_true &&
                         step1_projection_mask != null && step1_projection_mask.tag == INT &&
                         step1_projection_mask.int_val == 3 &&
                         step0_counters != null && step0_counters.tag == HASHMAP &&
                         step0_rows_read != null && step0_rows_read.tag == INT &&
                         step0_rows_emitted != null && step0_rows_emitted.tag == INT &&
                         step0_index_hits != null && step0_index_hits.tag == INT &&
                         step0_index_misses != null && step0_index_misses.tag == INT &&
                         step0_counter_kind != null && step0_counter_kind.tag == SYMBOL &&
                         step0_counter_kind.sym_val == sym_estimated &&
                         step1_counters != null && step1_counters.tag == HASHMAP &&
                         step1_rows_read != null && step1_rows_read.tag == INT &&
                         step1_rows_emitted != null && step1_rows_emitted.tag == INT &&
                         step1_index_hits != null && step1_index_hits.tag == INT &&
                         step1_index_misses != null && step1_index_misses.tag == INT &&
                         step1_counter_kind != null && step1_counter_kind.tag == SYMBOL &&
                         step1_counter_kind.sym_val == sym_estimated;
                }
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain emits deterministic operator + join-order payload");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain emits deterministic operator + join-order payload");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_exact");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_exact");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "B");
            i2[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[2] outer_cols = { 0, 1 };
            usz[2] inner_cols = { 0, 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_index_nested_loop_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                &join_count,
                &stats,
                interp
            );
            bool join_ok = joined &&
                stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 2 &&
                join_count == 2;
            ok = join_ok;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce index nested-loop join exact probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce index nested-loop join exact probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_lit");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_lit");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "C");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID;
        }

        Value*[] static_vals = {};
        if (ok) {
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            if (inner_rel == null || inner_rel.col_count != 2) {
                ok = false;
            } else {
                static_vals = mem::new_array(Value*, inner_rel.col_count);
                if (static_vals.ptr == null) {
                    ok = false;
                } else {
                    for (usz i = 0; i < static_vals.len; i++) static_vals[i] = null;
                    static_vals[0] = make_string(interp, "A");
                }
            }
        }
        defer if (static_vals.ptr != null) mem::free(static_vals.ptr);

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 1 };
            usz[1] inner_cols = { 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_index_nested_loop_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                static_vals,
                &join_count,
                &stats,
                interp
            );
            bool join_ok = joined &&
                stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 2 &&
                join_count == 2;
            ok = join_ok;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce index nested-loop join literal+bound probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce index nested-loop join literal+bound probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_hash");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_hash");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "X1");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "X2");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "B");
            i3[2] = make_string(interp, "X3");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            Value* r6 = prim_assert_fact(i3[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 0 };
            usz[1] inner_cols = { 0 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_hash_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                !stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 5 &&
                join_count == 5;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce hash join multi-match probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce hash join multi-match probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_hash_lit");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_hash_lit");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "C");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "A");
            i3[2] = make_string(interp, "D");
            Value*[3] i4;
            i4[0] = inner_rel_val;
            i4[1] = make_string(interp, "B");
            i4[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            Value* r6 = prim_assert_fact(i3[..], null, interp);
            Value* r7 = prim_assert_fact(i4[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID &&
                 r7 != null && r7.tag == VOID;
        }

        Value*[] static_vals = {};
        if (ok) {
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            if (inner_rel == null || inner_rel.col_count != 2) {
                ok = false;
            } else {
                static_vals = mem::new_array(Value*, inner_rel.col_count);
                if (static_vals.ptr == null) {
                    ok = false;
                } else {
                    for (usz i = 0; i < static_vals.len; i++) static_vals[i] = null;
                    static_vals[0] = make_string(interp, "A");
                }
            }
        }
        defer if (static_vals.ptr != null) mem::free(static_vals.ptr);

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 1 };
            usz[1] inner_cols = { 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_hash_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                static_vals,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                !stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 3 &&
                join_count == 3;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce hash join literal+bound probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce hash join literal+bound probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_adaptive_hash");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_adaptive_hash");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "B");
            o2[2] = make_string(interp, "B");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "C");
            o3[2] = make_string(interp, "B");
            Value*[3] o4;
            o4[0] = outer_rel_val;
            o4[1] = make_string(interp, "D");
            o4[2] = make_string(interp, "B");
            Value*[3] o5;
            o5[0] = outer_rel_val;
            o5[1] = make_string(interp, "E");
            o5[2] = make_string(interp, "B");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "X");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(o4[..], null, interp);
            Value* r5 = prim_assert_fact(o5[..], null, interp);
            Value* r6 = prim_assert_fact(i1[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 0 };
            usz[1] inner_cols = { 0 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_adaptive_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                4,
                70,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                stats.used_hash &&
                !stats.used_index &&
                stats.fallback_applied &&
                stats.outer_rows == 5 &&
                join_count == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce adaptive join fallback to hash");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce adaptive join fallback to hash");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_adaptive_idx");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_adaptive_idx");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "B");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "C");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "B");
            i2[2] = make_string(interp, "C");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "C");
            i3[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            Value* r6 = prim_assert_fact(i3[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[2] outer_cols = { 0, 1 };
            usz[2] inner_cols = { 0, 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_adaptive_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                3,
                70,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                stats.used_index &&
                !stats.used_hash &&
                !stats.fallback_applied &&
                stats.index_hits == 3 &&
                stats.index_misses == 0 &&
                join_count == 3;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce adaptive join keeps index");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce adaptive join keeps index");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_sym = interp.symbols.intern("schema_idx_memory");
        SymbolId col_a = interp.symbols.intern("name");
        SymbolId col_b = interp.symbols.intern("age");
        SymbolId col_c = interp.symbols.intern("email");

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            Value*[5] define_args;
            define_args[0] = db_val;
            define_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            define_args[2] = deduce_rule_test_make_symbol_value(interp, col_a);
            define_args[3] = deduce_rule_test_make_symbol_value(interp, col_b);
            define_args[4] = deduce_rule_test_make_symbol_value(interp, col_c);
            Value* rel_val = prim_define_relation(define_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }
        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
            ok = deduce_rule_test_validate_default_indexes(schema);
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce relation schema indexes (memory-backed)");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (memory-backed)");
            (*fail)++;
        }
    }

    {
        uint pid = (uint)deduce_test_getpid();
        usz seq = g_deduce_test_long_path_seq++;
        char[256] db_path_buf;
        char[] db_path = io::bprintf(
            &db_path_buf,
            "/tmp/omni-deduce-index-open-%d-%d",
            (int)pid,
            (int)seq
        )!!;

        char[320] cleanup_cmd_buf;
        char[] cleanup_cmd = io::bprintf(&cleanup_cmd_buf, "rm -rf %s", (ZString)db_path)!!;
        cleanup_cmd_buf[cleanup_cmd.len] = 0;
        deduce_test_system(&cleanup_cmd_buf[0]);
        defer deduce_test_system(&cleanup_cmd_buf[0]);

        Value*[1] open_args;
        open_args[0] = make_string(interp, db_path);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        SymbolId rel_sym = interp.symbols.intern("schema_idx_file");
        SymbolId col_a = interp.symbols.intern("name");
        SymbolId col_b = interp.symbols.intern("age");
        SymbolId col_c = interp.symbols.intern("email");

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            Value*[6] open_named_args;
            open_named_args[0] = db_val;
            open_named_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            open_named_args[2] = make_string(interp, "schema_idx_file_v1");
            open_named_args[3] = deduce_rule_test_make_symbol_value(interp, col_a);
            open_named_args[4] = deduce_rule_test_make_symbol_value(interp, col_b);
            open_named_args[5] = deduce_rule_test_make_symbol_value(interp, col_c);
            Value* rel_val = prim_deduce_open_named(open_named_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }
        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
            ok = deduce_rule_test_validate_default_indexes(schema);
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce relation schema indexes (file-backed open-named)");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (file-backed open-named)");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_sym = interp.symbols.intern("schema_stats");
        SymbolId col_name = interp.symbols.intern("name");
        SymbolId col_age = interp.symbols.intern("age");

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;

        Value* rel_val = null;
        if (ok) {
            Value*[4] define_args;
            define_args[0] = db_val;
            define_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            define_args[2] = deduce_rule_test_make_symbol_value(interp, col_name);
            define_args[3] = deduce_rule_test_make_symbol_value(interp, col_age);
            rel_val = prim_define_relation(define_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }

        DeduceDb* db = ok ? (DeduceDb*)db_val.ffi_val.lib_handle : null;
        DeduceRelationSchema* schema = ok ? deduce_db_lookup_relation_schema(db, rel_sym) : null;
        ok = ok &&
             schema != null &&
             schema.cardinality_estimate == 0 &&
             schema.distinct_estimate == 0;

        if (ok) {
            Value*[3] fact1;
            fact1[0] = rel_val;
            fact1[1] = make_string(interp, "Alice");
            fact1[2] = make_int(interp, 30);
            Value* r1 = prim_assert_fact(fact1[..], null, interp);

            Value*[3] fact2;
            fact2[0] = rel_val;
            fact2[1] = make_string(interp, "Bob");
            fact2[2] = make_int(interp, 31);
            Value* r2 = prim_assert_fact(fact2[..], null, interp);

            Value*[3] fact2_dup;
            fact2_dup[0] = rel_val;
            fact2_dup[1] = make_string(interp, "Bob");
            fact2_dup[2] = make_int(interp, 31);
            Value* r3 = prim_assert_fact(fact2_dup[..], null, interp);

            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 schema.cardinality_estimate == 2 &&
                 schema.distinct_estimate == 2;
        }

        if (ok) {
            Value*[3] retract1;
            retract1[0] = rel_val;
            retract1[1] = make_string(interp, "Bob");
            retract1[2] = make_int(interp, 31);
            Value* rr1 = prim_retract_fact(retract1[..], null, interp);

            Value*[3] retract_miss;
            retract_miss[0] = rel_val;
            retract_miss[1] = make_string(interp, "Zoe");
            retract_miss[2] = make_int(interp, 99);
            Value* rr2 = prim_retract_fact(retract_miss[..], null, interp);

            ok = rr1 != null && rr1.tag == VOID &&
                 rr2 != null && rr2.tag == VOID &&
                 schema.cardinality_estimate == 1 &&
                 schema.distinct_estimate == 1;
        }

        if (ok) {
            Value*[1] clear_args;
            clear_args[0] = rel_val;
            Value* cleared = prim_deduce_clear(clear_args[..], null, interp);
            ok = cleared != null &&
                 cleared.tag == VOID &&
                 schema.cardinality_estimate == 0 &&
                 schema.distinct_estimate == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce schema captures baseline cardinality/distinct estimates");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce schema captures baseline cardinality/distinct estimates");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-ana (deduce/open 'memory)) "
            "  (define [relation db-ana] parent (src) (dst)) "
            "  (define [relation db-ana] ancestor (src) (dst)) "
            "  (deduce/fact! parent \"A\" \"B\") "
            "  (deduce/fact! parent \"B\" \"C\") "
            "  (deduce/fact! parent \"C\" \"D\") "
            "  (deduce/rule! db-ana '(ancestor ?x ?y) '(parent ?x ?y)) "
            "  (deduce/rule! db-ana '(ancestor ?x ?z) '(parent ?x ?y) '(ancestor ?y ?z)) "
            "  (define a1 (deduce/analyze db-ana)) "
            "  (define a2 (deduce 'analyze db-ana)) "
            "  (define ok1 (= (deduce/count ancestor) 6)) "
            "  (define ok2 (= ('mode a1) 'naive-bottom-up)) "
            "  (define ok3 (= ('status a1) 'ok)) "
            "  (define ok4 (= ('execution-engine a1) 'semi-naive-scc)) "
            "  (define ok5 (= ('mode a2) 'naive-bottom-up)) "
            "  (define ok6 (= ('execution-engine a2) 'semi-naive-scc)) "
            "  (define ok7 (= ('strata-count a1) 2)) "
            "  (define ok8 (= ('recursive-strata a1) 1)) "
            "  (define ok9 (>= ('iterations a1) 1)) "
            "  (define ok10 (= ('iteration-limit a1) 256)) "
            "  (define ok11 (>= ('max-component-iterations a1) 1)) "
            "  (and ok1 "
            "       (and ok2 "
            "            (and ok3 "
            "                 (and ok4 "
            "                      (and ok5 "
            "                           (and ok6 "
            "                                (and ok7 "
            "                                     (and ok8 "
            "                                          (and ok9 "
            "                                               (and ok10 ok11)))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze runs semi-naive recursive fixpoint and derives closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze runs semi-naive recursive fixpoint and derives closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze runs semi-naive recursive fixpoint and derives closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] parent-n (src) (dst)) "
            "  (define [relation db-naive] ancestor-n (src) (dst)) "
            "  (deduce/fact! parent-n \"A\" \"B\") "
            "  (deduce/fact! parent-n \"B\" \"C\") "
            "  (deduce/fact! parent-n \"C\" \"D\") "
            "  (deduce/rule! db-naive '(ancestor-n ?x ?y) '(parent-n ?x ?y)) "
            "  (deduce/rule! db-naive '(ancestor-n ?x ?z) '(parent-n ?x ?y) '(ancestor-n ?y ?z)) "
            "  (define an (deduce/analyze db-naive 'naive)) "
            "  (define cn (deduce/count ancestor-n)) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] parent-s (src) (dst)) "
            "  (define [relation db-semi] ancestor-s (src) (dst)) "
            "  (deduce/fact! parent-s \"A\" \"B\") "
            "  (deduce/fact! parent-s \"B\" \"C\") "
            "  (deduce/fact! parent-s \"C\" \"D\") "
            "  (deduce/rule! db-semi '(ancestor-s ?x ?y) '(parent-s ?x ?y)) "
            "  (deduce/rule! db-semi '(ancestor-s ?x ?z) '(parent-s ?x ?y) '(ancestor-s ?y ?z)) "
            "  (define as (deduce/analyze db-semi 'semi-naive)) "
            "  (define cs (deduce/count ancestor-s)) "
            "  (define ok1 (= cn cs)) "
            "  (define ok2 (= cn 6)) "
            "  (define ok3 (= ('derived-facts an) ('derived-facts as))) "
            "  (define ok4 (= ('execution-engine an) 'naive-scc)) "
            "  (define ok5 (= ('execution-engine as) 'semi-naive-scc)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 ok5)))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze naive and semi-naive engines produce parity on recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze naive and semi-naive engines produce parity on recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze naive and semi-naive engines produce parity on recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-strata (deduce/open 'memory)) "
            "  (define [relation db-strata] base-neg (x)) "
            "  (define [relation db-strata] deny-neg (x)) "
            "  (define [relation db-strata] allow-neg (x)) "
            "  (deduce/fact! base-neg \"A\") "
            "  (deduce/rule! db-strata '(deny-neg ?x) '(base-neg ?x)) "
            "  (deduce/rule! db-strata '(allow-neg ?x) '(base-neg ?x) '(not (deny-neg ?x))) "
            "  (define a (deduce/analyze db-strata)) "
            "  (define ok1 (= (deduce/count deny-neg) 1)) "
            "  (define ok2 (= (deduce/count allow-neg) 0)) "
            "  (define ok3 (>= ('stratum-count a) 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze enforces negation stratum order");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze enforces negation stratum order (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze enforces negation stratum order");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-mixed (deduce/open 'memory)) "
            "  (define [relation db-mixed] edge-m (src) (dst)) "
            "  (define [relation db-mixed] reach-m (src) (dst)) "
            "  (define [relation db-mixed] banned-m (node)) "
            "  (define [relation db-mixed] reach-ok-m (src) (dst)) "
            "  (deduce/fact! edge-m \"A\" \"B\") "
            "  (deduce/fact! edge-m \"B\" \"C\") "
            "  (deduce/fact! banned-m \"C\") "
            "  (deduce/rule! db-mixed '(reach-m ?x ?y) '(edge-m ?x ?y)) "
            "  (deduce/rule! db-mixed '(reach-m ?x ?z) '(edge-m ?x ?y) '(reach-m ?y ?z)) "
            "  (deduce/rule! db-mixed '(reach-ok-m ?x ?y) '(reach-m ?x ?y) '(not (banned-m ?y))) "
            "  (define a (deduce/analyze db-mixed)) "
            "  (define ok1 (= (deduce/count reach-m) 3)) "
            "  (define ok2 (= (deduce/count reach-ok-m) 1)) "
            "  (define ok3 (>= ('stratum-count a) 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze handles mixed recursive and negated stratum boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-inc (deduce/open 'memory)) "
            "  (define [relation db-inc] base-inc (x)) "
            "  (define [relation db-inc] derived-inc (x)) "
            "  (deduce/rule! db-inc '(derived-inc ?x) '(base-inc ?x)) "
            "  (define a0 (deduce/analyze db-inc)) "
            "  (deduce/fact! base-inc \"A\") "
            "  (define a1 (deduce/analyze db-inc)) "
            "  (define tx1 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx1 base-inc \"B\") "
            "  (deduce 'commit tx1) "
            "  (define a2 (deduce/analyze db-inc)) "
            "  (define tx2 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx2 base-inc \"C\") "
            "  (deduce 'abort tx2) "
            "  (define a3 (deduce/analyze db-inc)) "
            "  (define ok1 (= ('incremental-dependency-edges a0) 1)) "
            "  (define ok2 (= ('incremental-dirty-predicate-count a0) 0)) "
            "  (define ok3 (= ('incremental-invalidation-mode a0) 'tracked)) "
            "  (define ok4 (>= ('incremental-dirty-predicate-count a1) 2)) "
            "  (define ok5 (= ('incremental-invalidation-mode a1) 'tracked)) "
            "  (define ok6 (= ('incremental-dirty-predicate-count a2) 2)) "
            "  (define ok7 (= ('incremental-invalidation-mode a2) 'tracked)) "
            "  (define ok8 (= (deduce/count base-inc) 2)) "
            "  (define ok9 (= ('incremental-dirty-predicate-count a3) 0)) "
            "  (define ok10 (= ('incremental-invalidation-mode a3) 'tracked)) "
            "  (and ok1 "
            "       (and ok2 "
            "            (and ok3 "
            "                 (and ok4 "
            "                      (and ok5 "
            "                           (and ok6 "
            "                                (and ok7 "
            "                                     (and ok8 "
            "                                          (and ok9 ok10))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            }
            (*fail)++;
        }
    }
}
