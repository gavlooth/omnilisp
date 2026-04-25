module lisp;

import std::io;

fn bool deduce_test_payload_code_equals(Interp* interp, Value* payload, char[] expected_code) {
    Value* code = deduce_rule_test_dict_get(interp, payload, "code");
    return code != null &&
           code.tag == SYMBOL &&
           deduce_name_equals(interp.symbols.get_name(code.sym_val), expected_code);
}

fn void run_deduce_fact_lmdb_read_failure_tests(Interp* interp, int* pass, int* fail) {
    bool ok = false;
    EvalResult setup_result = run(
        "(block "
        "  (define db-fact-read-fail (deduce 'open 'memory)) "
        "  (define [relation db-fact-read-fail] rel-fact-read-fail (key id) (name)) "
        "  true)",
        interp
    );

    Value* rel_val = interp.global_env.lookup(interp.symbols.intern("rel-fact-read-fail"));
    if (!setup_result.error.has_error &&
        rel_val != null &&
        rel_val.tag == FFI_HANDLE &&
        rel_val.ffi_val != null &&
        rel_val.ffi_val.lib_handle != null) {
        bool saved_force = g_deduce_force_fact_existing_read_fail;
        g_deduce_force_fact_existing_read_fail = true;
        EvalResult r = run(
            "(handle (deduce 'fact! rel-fact-read-fail 1 \"Alice\") (raise payload payload))",
            interp
        );
        g_deduce_force_fact_existing_read_fail = saved_force;
        ok = !r.error.has_error &&
             deduce_test_payload_code_equals(interp, r.value, "deduce/fact-read-failed");
        run_clear_stale_raise_state(interp);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce fact! fails closed when existing-fact LMDB read fails");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce fact! fails closed when existing-fact LMDB read fails");
        (*fail)++;
    }
}

fn void run_deduce_reference_scan_open_failure_tests(Interp* interp, int* pass, int* fail) {
    bool ok = false;
    EvalResult setup_result = run(
        "(block "
        "  (define db-ref-scan-open-fail (deduce 'open 'memory)) "
        "  (define [relation db-ref-scan-open-fail] ref-scan-users (key id) (name)) "
        "  (define [relation db-ref-scan-open-fail] ref-scan-audit (ref ref-scan-users id user-id) (action)) "
        "  (deduce 'fact! ref-scan-users 7 \"Ada\") "
        "  true)",
        interp
    );

    Value* users_val = interp.global_env.lookup(interp.symbols.intern("ref-scan-users"));
    if (!setup_result.error.has_error &&
        users_val != null &&
        users_val.tag == FFI_HANDLE &&
        users_val.ffi_val != null &&
        users_val.ffi_val.lib_handle != null) {
        Relation* users = (Relation*)users_val.ffi_val.lib_handle;
        DeduceDb* db = users.db;
        if (db != null && db.relation_schemas != null) {
            SymbolId saved_name = (SymbolId)0;
            DeduceRelationSchema* audit_schema = null;
            for (usz i = 0; i < db.relation_schema_count; i++) {
                DeduceRelationSchema* schema = &db.relation_schemas[i];
                if (schema.reference_constraint_count > 0 &&
                    schema.reference_constraints != null &&
                    schema.reference_constraints[0].target_relation == users.name) {
                    audit_schema = schema;
                    saved_name = schema.name;
                    schema.name = interp.symbols.intern("__deduce_missing_source_for_scan");
                    break;
                }
            }

            EvalResult r = run(
                "(handle (deduce/retract! ref-scan-users 7 \"Ada\") (raise payload payload))",
                interp
            );
            if (audit_schema != null) audit_schema.name = saved_name;
            ok = !r.error.has_error &&
                 deduce_test_payload_code_equals(interp, r.value, "deduce/retract-reference-integrity-scan-failed");
            run_clear_stale_raise_state(interp);
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce retract! fails closed when reference source relation cannot open");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce retract! fails closed when reference source relation cannot open");
        (*fail)++;
    }
}

fn void run_deduce_fact_reference_scan_failure_tests(Interp* interp, int* pass, int* fail) {
    bool saved_force = g_deduce_force_fact_reference_scan_end_fail;
    g_deduce_force_fact_reference_scan_end_fail = true;
    EvalResult r = run(
        "(block "
        "  (define db-ref-scan-fail (deduce 'open 'memory)) "
        "  (define [relation db-ref-scan-fail] ref-scan-fail-users (key id) (name)) "
        "  (define [relation db-ref-scan-fail] ref-scan-fail-audit (ref ref-scan-fail-users id user-id) (action)) "
        "  (handle (deduce 'fact! ref-scan-fail-audit 99 \"login\") (raise payload payload)))",
        interp
    );
    g_deduce_force_fact_reference_scan_end_fail = saved_force;

    bool ok = !r.error.has_error &&
              deduce_test_payload_code_equals(interp, r.value, "deduce/fact-reference-integrity-scan-failed");
    run_clear_stale_raise_state(interp);

    if (ok) {
        emit_pass_literal("[PASS] deduce fact! fails closed when reference target scan fails");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce fact! fails closed when reference target scan fails");
        (*fail)++;
    }
}

fn void run_deduce_key_integrity_fact_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-ok (deduce 'open 'memory)) "
        "  (define [relation db-key-ok] rel-key-ok (key id) (name)) "
        "  (deduce 'fact! rel-key-ok 1 \"Alice\") "
        "  (deduce 'fact! rel-key-ok 1 \"Alice\") "
        "  (= (deduce 'count rel-key-ok) 1))",
        "[PASS] deduce keyed fact! remains idempotent for identical tuple",
        "[FAIL] deduce keyed fact! remains idempotent for identical tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-conflict (deduce 'open 'memory)) "
        "  (define [relation db-key-conflict] rel-key-conflict (key id) (name)) "
        "  (deduce 'fact! rel-key-conflict 1 \"Alice\") "
        "  (define payload (handle (deduce 'fact! rel-key-conflict 1 \"Bob\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-key-conflict) "
        "                             (and (= (ref data 'key-column) 'id) "
        "                                  (and (= (ref data 'key-value) 1) "
        "                                       (and (= (ref data 'violation-kind) 'key-conflict) "
        "                                            (= (car (ref data 'existing-tuple)) 1)))))))))))",
        "[PASS] deduce keyed fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce keyed fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        EvalResult r = run(
            "(block "
            "  (define db-key-conflict-oom (deduce 'open 'memory)) "
            "  (define [relation db-key-conflict-oom] rel-key-conflict-oom (key id) (name)) "
            "  (deduce 'fact! rel-key-conflict-oom 1 \"Alice\") "
            "  (define code "
            "    (handle (deduce 'fact! rel-key-conflict-oom 1 \"Bob\") "
            "      (raise payload "
            "        (if (dict? payload) "
            "            (String (ref payload 'code)) "
            "            \"deduce/integrity-key-conflict\")))) "
            "  (cons code (cons (deduce 'count rel-key-conflict-oom) nil)))",
            interp
        );
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok =
            !r.error.has_error &&
            is_cons(r.value) &&
            is_string(car(r.value)) &&
            deduce_name_equals(car(r.value).str_chars[:car(r.value).str_len], "deduce/integrity-key-conflict") &&
            is_cons(cdr(r.value)) &&
            is_int(car(cdr(r.value))) &&
            car(cdr(r.value)).int_val == 1;
        if (ok) {
            emit_pass_literal("[PASS] deduce keyed fact! degrades to payload-less conflict under payload-map oom");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce keyed fact! degrades to payload-less conflict under payload-map oom");
            (*fail)++;
        }
    }

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-tx (deduce 'open 'memory)) "
        "  (define [relation db-key-tx] rel-key-tx (key id) (name)) "
        "  (deduce 'fact! rel-key-tx 1 \"Alice\") "
        "  (define tx (deduce 'block db-key-tx)) "
        "  (define payload (handle (deduce 'fact! tx rel-key-tx 1 \"Bob\") (raise msg msg))) "
        "  (deduce 'abort tx) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "            (= (deduce 'count rel-key-tx) 1))))",
        "[PASS] deduce keyed fact! rejects conflicting transaction write",
        "[FAIL] deduce keyed fact! rejects conflicting transaction write",
        pass,
        fail
    );

    {
        EvalResult setup = run(
            "(block "
            "  (define db-key-scan-end-fail (deduce 'open 'memory)) "
            "  (define [relation db-key-scan-end-fail] rel-key-scan-end-fail (key id) (name)) "
            "  (deduce 'fact! rel-key-scan-end-fail 1 \"Alice\") "
            "  true)",
            interp
        );
        bool saved_force = g_deduce_force_immediate_integrity_scan_end_fail;
        g_deduce_force_immediate_integrity_scan_end_fail = true;
        EvalResult r = run(
            "(handle (deduce 'fact! rel-key-scan-end-fail 2 \"Bob\") (raise payload payload))",
            interp
        );
        g_deduce_force_immediate_integrity_scan_end_fail = saved_force;

        bool ok = !setup.error.has_error &&
                  !r.error.has_error &&
                  deduce_test_payload_code_equals(interp, r.value, "deduce/fact-key-integrity-scan-failed");
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce keyed fact! fails closed when immediate key scan termination fails");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce keyed fact! fails closed when immediate key scan termination fails");
            (*fail)++;
        }
    }

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-conflict (deduce 'open 'memory)) "
        "  (define [relation db-unique-conflict] rel-unique-conflict (id) (unique email)) "
        "  (deduce 'fact! rel-unique-conflict 1 \"alice@x\") "
        "  (define payload (handle (deduce 'fact! rel-unique-conflict 2 \"alice@x\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-unique-conflict) "
        "                             (and (= (ref data 'unique-column) 'email) "
        "                                  (and (= (ref data 'key-value) \"alice@x\") "
        "                                       (and (= (ref data 'violation-kind) 'unique-conflict) "
        "                                            (= (car (ref data 'existing-tuple)) 1)))))))))))",
        "[PASS] deduce unique fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce unique fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    {
        EvalResult setup = run(
            "(block "
            "  (define db-unique-scan-end-fail (deduce 'open 'memory)) "
            "  (define [relation db-unique-scan-end-fail] rel-unique-scan-end-fail (id) (unique email)) "
            "  (deduce 'fact! rel-unique-scan-end-fail 1 \"alice@x\") "
            "  true)",
            interp
        );
        bool saved_force = g_deduce_force_immediate_integrity_scan_end_fail;
        g_deduce_force_immediate_integrity_scan_end_fail = true;
        EvalResult r = run(
            "(handle (deduce 'fact! rel-unique-scan-end-fail 2 \"bob@x\") (raise payload payload))",
            interp
        );
        g_deduce_force_immediate_integrity_scan_end_fail = saved_force;

        bool ok = !setup.error.has_error &&
                  !r.error.has_error &&
                  deduce_test_payload_code_equals(interp, r.value, "deduce/fact-unique-integrity-scan-failed");
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce unique fact! fails closed when immediate unique scan termination fails");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce unique fact! fails closed when immediate unique scan termination fails");
            (*fail)++;
        }
    }

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-composite-conflict (deduce 'open 'memory)) "
        "  (define [relation db-unique-composite-conflict] rel-unique-composite-conflict (id) (unique email) (unique dept)) "
        "  (deduce 'fact! rel-unique-composite-conflict 1 \"alice@x\" \"ops\") "
        "  (deduce 'fact! rel-unique-composite-conflict 2 \"alice@x\" \"sales\") "
        "  (define payload (handle (deduce 'fact! rel-unique-composite-conflict 3 \"alice@x\" \"ops\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-unique-composite-conflict) "
        "                             (and (= (car (ref data 'columns)) 'email) "
        "                                  (and (= (car (cdr (ref data 'columns))) 'dept) "
        "                                       (and (= (car (ref data 'constraint-values)) \"alice@x\") "
        "                                            (and (= (car (cdr (ref data 'constraint-values))) \"ops\") "
        "                                                 (and (= (ref data 'violation-kind) 'unique-conflict) "
        "                                                      (= (car (ref data 'existing-tuple)) 1)))))))))))))",
        "[PASS] deduce composite unique fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce composite unique fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-ref-ok] ref-users-ok (key id) (name)) "
        "  (define [relation db-ref-ok] ref-audit-ok (ref ref-users-ok id user-id) (action)) "
        "  (deduce 'fact! ref-users-ok 42 \"Alice\") "
        "  (deduce 'fact! ref-audit-ok 42 \"login\") "
        "  (= (deduce 'count ref-audit-ok) 1))",
        "[PASS] deduce reference fact! accepts existing referenced tuple",
        "[FAIL] deduce reference fact! accepts existing referenced tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-missing (deduce 'open 'memory)) "
        "  (define [relation db-ref-missing] ref-users-missing (key id) (name)) "
        "  (define [relation db-ref-missing] ref-audit-missing (ref ref-users-missing id user-id) (action)) "
        "  (define payload (handle (deduce 'fact! ref-audit-missing 42 \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'ref-audit-missing) "
        "                             (and (= (ref data 'referenced-relation) 'ref-users-missing) "
        "                                  (and (= (ref data 'referenced-column) 'id) "
        "                                       (and (= (ref data 'key-value) 42) "
        "                                            (= (ref data 'violation-kind) 'reference-missing)))))))))))",
        "[PASS] deduce reference fact! exposes machine-checkable missing-target payload",
        "[FAIL] deduce reference fact! exposes machine-checkable missing-target payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-nonunique (deduce 'open 'memory)) "
        "  (define [relation db-ref-nonunique] ref-users-nonunique (id) (email)) "
        "  (define [relation db-ref-nonunique] ref-audit-nonunique (ref ref-users-nonunique email user-email) (action)) "
        "  (deduce 'fact! ref-users-nonunique 1 \"alice@x\") "
        "  (define payload (handle (deduce 'fact! ref-audit-nonunique \"alice@x\" \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-reference-target-not-unique) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'ref-audit-nonunique) "
        "                             (and (= (ref data 'referenced-relation) 'ref-users-nonunique) "
        "                                  (and (= (ref data 'referenced-column) 'email) "
        "                                       (and (= (ref data 'key-value) \"alice@x\") "
        "                                            (= (ref data 'violation-kind) 'reference-target-not-unique)))))))))))",
        "[PASS] deduce reference fact! rejects non-unique target columns",
        "[FAIL] deduce reference fact! rejects non-unique target columns",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-dropped (deduce 'open 'memory)) "
        "  (define [relation db-ref-dropped] ref-users-dropped (key id) (name)) "
        "  (define [relation db-ref-dropped] ref-audit-dropped (ref ref-users-dropped id user-id) (action)) "
        "  (deduce 'drop! ref-users-dropped) "
        "  (define payload (handle (deduce 'fact! ref-audit-dropped 42 \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (= (ref payload 'code) 'deduce/integrity-reference-target-missing))))",
        "[PASS] deduce reference fact! stays consistent after target relation is dropped",
        "[FAIL] deduce reference fact! stays consistent after target relation is dropped",
        pass,
        fail
    );
}

fn void run_deduce_check_integrity_fact_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-ok (deduce 'open 'memory)) "
        "  (define [relation db-check-ok] rel-check-ok (id) (check int? amount)) "
        "  (deduce 'fact! rel-check-ok 1 7) "
        "  (= (deduce 'count rel-check-ok) 1))",
        "[PASS] deduce check fact! accepts tuples that satisfy the predicate",
        "[FAIL] deduce check fact! accepts tuples that satisfy the predicate",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-fail (deduce 'open 'memory)) "
        "  (define [relation db-check-fail] rel-check-fail (id) (check int? amount)) "
        "  (define payload (handle (deduce 'fact! rel-check-fail 1 \"oops\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-check-failed) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-check-fail) "
        "                             (and (= (ref data 'predicate) 'int?) "
            "                                  (and (= (ref data 'key-value) \"oops\") "
            "                                       (= (ref data 'violation-kind) 'check-failed))))))))))",
        "[PASS] deduce check fact! exposes machine-checkable failure payload",
        "[FAIL] deduce check fact! exposes machine-checkable failure payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-missing (deduce 'open 'memory)) "
        "  (define [relation db-check-missing] rel-check-missing (id) (check missing-check-predicate amount)) "
        "  (define payload (handle (deduce 'fact! rel-check-missing 1 7) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/check-predicate-missing) "
        "       (= (deduce 'count rel-check-missing) 0)))",
        "[PASS] deduce check fact! rejects missing predicate symbols deterministically",
        "[FAIL] deduce check fact! rejects missing predicate symbols deterministically",
        pass,
        fail
    );
}
