module lisp;

import std::io;

fn void run_deduce_relation_attr_define_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-attr (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-attr] rel-attr-test (id) (name) (qty)) "
        "  (deduce 'count rel-attr-test))",
        interp
    );
    if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
        emit_pass_literal("[PASS] deduce [relation] define attr");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce [relation] define attr");
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized materialized] rel-attr-materialized (src) (dst)) "
        "  ddb-rel-materialized)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("manual") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_manual_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized-manual");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-manual (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-manual materialized manual] rel-attr-materialized-manual (src) (dst)) "
        "  ddb-rel-materialized-manual)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("manual") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized manual] declares manual materialization policy explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized manual] declares manual materialization policy explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized manual] declares manual materialization policy explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_on_read_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized-on-read");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-on-read (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-on-read materialized on-read] rel-attr-materialized-on-read (src) (dst)) "
        "  ddb-rel-materialized-on-read)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("on-read") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_schema_policy_surface_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-schema (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-schema materialized] rel-attr-materialized-schema-implicit (src) (dst)) "
        "  (define [relation ddb-rel-materialized-schema materialized manual] rel-attr-materialized-schema-explicit (src) (dst)) "
        "  (define [relation ddb-rel-materialized-schema materialized on-read] rel-attr-materialized-schema-on-read (src) (dst)) "
        "  (define implicit-schema (deduce/schema rel-attr-materialized-schema-implicit)) "
        "  (define explicit-schema (deduce/schema rel-attr-materialized-schema-explicit)) "
        "  (define on-read-schema (deduce/schema rel-attr-materialized-schema-on-read)) "
        "  (and "
        "    (= (ref implicit-schema 'materialized-view) true) "
        "    (= (ref implicit-schema 'materialized-refresh-policy) 'manual) "
        "    (= (ref implicit-schema 'materialized-derived-ready) false) "
        "    (= (ref implicit-schema 'materialized-stale) true) "
        "    (= (ref implicit-schema 'materialized-last-stale-reason) 'never-refreshed) "
        "    (= (ref explicit-schema 'materialized-view) true) "
        "    (= (ref explicit-schema 'materialized-refresh-policy) 'manual) "
        "    (= (ref explicit-schema 'materialized-derived-ready) false) "
        "    (= (ref explicit-schema 'materialized-stale) true) "
        "    (= (ref explicit-schema 'materialized-last-stale-reason) 'never-refreshed) "
        "    (= (ref on-read-schema 'materialized-view) true) "
        "    (= (ref on-read-schema 'materialized-refresh-policy) 'on-read) "
        "    (= (ref on-read-schema 'materialized-derived-ready) false) "
        "    (= (ref on-read-schema 'materialized-stale) true) "
        "    (= (ref on-read-schema 'materialized-last-stale-reason) 'never-refreshed)))",
        interp
    );

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce/schema exposes declared materialization policies faithfully");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce/schema exposes declared materialization policies faithfully (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce/schema exposes declared materialization policies faithfully");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_unknown_policy_test(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "[PASS] deduce [relation ... materialized] rejects unknown refresh policies",
        "(block "
        "  (define ddb-rel-materialized-policy (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-policy materialized eager] rel-attr-materialized-policy (id)) "
        "  0)",
        "unknown materialized refresh policy",
        pass,
        fail
    );
}

fn void run_deduce_relation_attr_check_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-check");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-check (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-check] rel-attr-check (id) (check positive amount)) "
        "  ddb-rel-check)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.check_constraint_count == 1 &&
             schema.check_constraints != null &&
             schema.check_constraints[0].local_column == interp.symbols.intern("amount") &&
             schema.check_constraints[0].predicate == interp.symbols.intern("positive");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... check predicate column] declares check constraint metadata explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... check predicate column] declares check constraint metadata explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... check predicate column] declares check constraint metadata explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_unknown_attribute_test(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "[PASS] deduce [relation] rejects unknown relation attributes",
        "(block "
        "  (define ddb-rel-unknown-attr (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unknown-attr mystery] rel-attr-unknown (id)) "
        "  0)",
        "unknown relation attribute",
        pass,
        fail
    );
}

fn void run_deduce_relation_attr_typed_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-schema");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-schema (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-schema] rel-attr-schema "
        "    (key ^String name) "
        "    (index ^Integer age) "
        "    (^String email)) "
        "  ddb-rel-schema)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[3] full = { 0, 1, 2 };
        usz[1] key_col = { 0 };
        usz[1] index_col = { 1 };
        usz[2] key_index = { 0, 1 };
        ok = schema != null &&
             schema.arity == 3 &&
             deduce_rule_test_schema_has_index(schema, full[..], true) &&
             deduce_rule_test_schema_has_index(schema, key_col[..], true) &&
             deduce_rule_test_schema_has_index(schema, index_col[..], false) &&
             deduce_rule_test_schema_has_index(schema, key_index[..], false);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] typed fields with key/index roles");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] typed fields with key/index roles (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] typed fields with key/index roles");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_unique_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-unique");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-unique (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unique] rel-attr-unique "
        "    (^Integer id) "
        "    (unique ^String email) "
        "    (index ^String dept)) "
        "  ddb-rel-unique)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[1] unique_col = { 1 };
        usz[1] index_col = { 2 };
        ok = schema != null &&
             schema.arity == 3 &&
             schema.unique_column == interp.symbols.intern("email") &&
             deduce_rule_test_schema_has_index(schema, unique_col[..], true) &&
             deduce_rule_test_schema_has_index(schema, index_col[..], false);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] unique role adds enforced unique index");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] unique role adds enforced unique index (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] unique role adds enforced unique index");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_composite_unique_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-unique-composite");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-unique-composite (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unique-composite] rel-attr-unique-composite "
        "    (^Integer id) "
        "    (unique ^String email) "
        "    (unique ^String dept)) "
        "  ddb-rel-unique-composite)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[2] unique_cols = { 1, 2 };
        ok = schema != null &&
             schema.arity == 3 &&
             schema.unique_column == (SymbolId)0 &&
             schema.unique_column_count == 2 &&
             schema.unique_columns != null &&
             schema.unique_columns[0] == interp.symbols.intern("email") &&
             schema.unique_columns[1] == interp.symbols.intern("dept") &&
             deduce_rule_test_schema_has_index(schema, unique_cols[..], true);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] composite unique role adds enforced composite index");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] composite unique role adds enforced composite index (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] composite unique role adds enforced composite index");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_reference_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-reference");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-reference (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-reference] users (key id) (name)) "
        "  (define [relation ddb-rel-reference] rel-attr-reference "
        "    (ref users id ^Integer user-id) "
        "    (^String action)) "
        "  ddb-rel-reference)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.arity == 2 &&
             schema.reference_constraint_count == 1 &&
             schema.reference_constraints != null &&
             schema.reference_constraints[0].local_column == interp.symbols.intern("user-id") &&
             schema.reference_constraints[0].target_relation == interp.symbols.intern("users") &&
             schema.reference_constraints[0].target_column == interp.symbols.intern("id");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] ref role records enforced reference metadata");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] ref role records enforced reference metadata (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] ref role records enforced reference metadata");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_combined_role_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-combined");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-combined (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-combined] rel-attr-combined "
        "    (key index ^String id) "
        "    (^String email)) "
        "  ddb-rel-combined)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[2] full = { 0, 1 };
        usz[1] key_index_col = { 0 };
        ok = schema != null &&
             schema.arity == 2 &&
             deduce_rule_test_schema_has_index(schema, full[..], true) &&
             deduce_rule_test_schema_has_index(schema, key_index_col[..], true);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] combined key/index role markers");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] combined key/index role markers (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] combined key/index role markers");
        }
        (*fail)++;
    }
}
