module lisp;

import std::io;

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

fn void run_deduce_rule_head_reference_snapshot_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-ref-ok] user-src (id) (name)) "
        "  (define [relation db-rule-ref-ok] audit-src (user-id) (action)) "
        "  (define [relation db-rule-ref-ok] users (key id) (name)) "
        "  (define [relation db-rule-ref-ok] audit-out (ref users id user-id) (action)) "
        "  (deduce 'fact! user-src 1 \"Alice\") "
        "  (deduce 'fact! audit-src 1 \"login\") "
        "  (deduce/rule! db-rule-ref-ok '(audit-out ?u ?a) '(audit-src ?u ?a)) "
        "  (deduce/rule! db-rule-ref-ok '(audit-out ?u ?a) '(users ?u ?name) '(audit-src ?u ?a)) "
        "  (deduce/rule! db-rule-ref-ok '(users ?u ?name) '(audit-out ?u ?a) '(user-src ?u ?name)) "
        "  (define analyzed (deduce 'analyze db-rule-ref-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (and (= (deduce 'count users) 1) "
        "            (= (deduce 'count audit-out) 1))))",
        "[PASS] deduce rule-derived reference heads validate against final recursive component snapshot",
        "[FAIL] deduce rule-derived reference heads validate against final recursive component snapshot",
        pass,
        fail
    );
}

fn void run_deduce_rule_head_integrity_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-key-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-key-conflict] user-src (id) (name)) "
            "  (define [relation db-rule-key-conflict] users (key id) (name)) "
            "  (deduce 'fact! user-src 1 \"Alice\") "
            "  (deduce 'fact! user-src 1 \"Bob\") "
            "  (deduce/rule! db-rule-key-conflict '(users ?id ?name) '(user-src ?id ?name)) "
            "  db-rule-key-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* key_column = deduce_rule_test_dict_get(interp, data, "key-column");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-key-conflict") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "users") &&
                 key_column != null &&
                 key_column.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(key_column.sym_val), "id") &&
                 is_int(key_value) &&
                 key_value.int_val == 1;
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived keyed tuples raise machine-checkable conflict payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived keyed tuples raise machine-checkable conflict payloads");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-unique-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-unique-conflict] email-src (id) (email)) "
            "  (define [relation db-rule-unique-conflict] emails (id) (unique email)) "
            "  (deduce 'fact! email-src 1 \"alice@x\") "
            "  (deduce 'fact! email-src 2 \"alice@x\") "
            "  (deduce/rule! db-rule-unique-conflict '(emails ?id ?email) '(email-src ?id ?email)) "
            "  db-rule-unique-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* unique_column = deduce_rule_test_dict_get(interp, data, "unique-column");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-unique-conflict") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "emails") &&
                 unique_column != null &&
                 unique_column.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(unique_column.sym_val), "email") &&
                 deduce_test_string_equals(key_value, "alice@x");
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived unique tuples raise machine-checkable conflict payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived unique tuples raise machine-checkable conflict payloads");
            (*fail)++;
        }
    }

    run_deduce_rule_head_reference_snapshot_test(interp, pass, fail);

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-ref-missing (deduce 'open 'memory)) "
        "  (define [relation db-rule-ref-missing] audit-src (user-id) (action)) "
        "  (define [relation db-rule-ref-missing] users (key id) (name)) "
        "  (define [relation db-rule-ref-missing] audit-out (ref users id user-id) (action)) "
        "  (deduce 'fact! audit-src 42 \"login\") "
        "  (deduce/rule! db-rule-ref-missing '(audit-out ?u ?a) '(audit-src ?u ?a)) "
        "  (define payload (handle (deduce 'analyze db-rule-ref-missing) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "            (let (data (ref payload 'data)) "
        "              (and (dict? data) "
        "                   (and (= (ref data 'relation) 'audit-out) "
        "                        (and (= (ref data 'referenced-relation) 'users) "
        "                             (and (= (ref data 'referenced-column) 'id) "
        "                                  (and (= (ref data 'key-value) 42) "
        "                                       (= (ref data 'violation-kind) 'reference-missing))))))))))",
        "[PASS] deduce rule-derived reference heads raise machine-checkable missing-target payloads",
        "[FAIL] deduce rule-derived reference heads raise machine-checkable missing-target payloads",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-key-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-key-ok] sale (dept) (amount)) "
        "  (define [relation db-rule-agg-key-ok] total-by-dept (key dept) (total)) "
        "  (deduce 'fact! sale \"eng\" 10) "
        "  (deduce 'fact! sale \"eng\" 15) "
        "  (deduce 'fact! sale \"sales\" 3) "
        "  (deduce/rule! db-rule-agg-key-ok '(total-by-dept ?dept (sum ?amount)) '(sale ?dept ?amount)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-key-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (length (deduce 'scan total-by-dept)) 2)))",
        "[PASS] deduce aggregate keyed rule heads publish with integrity enforcement",
        "[FAIL] deduce aggregate keyed rule heads publish with integrity enforcement",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-unique-conflict (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-unique-conflict] sale (dept) (amount)) "
        "  (define [relation db-rule-agg-unique-conflict] total-by-dept (dept) (unique total)) "
        "  (deduce 'fact! sale \"eng\" 10) "
        "  (deduce 'fact! sale \"sales\" 10) "
        "  (deduce/rule! db-rule-agg-unique-conflict '(total-by-dept ?dept (sum ?amount)) '(sale ?dept ?amount)) "
        "  (define payload (handle (deduce 'analyze db-rule-agg-unique-conflict) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "            (let (data (ref payload 'data)) "
        "              (and (dict? data) "
        "                   (and (= (ref data 'relation) 'total-by-dept) "
        "                        (and (= (ref data 'unique-column) 'total) "
        "                             (= (ref data 'key-value) 10))))))))",
        "[PASS] deduce aggregate unique rule heads raise machine-checkable conflict payloads",
        "[FAIL] deduce aggregate unique rule heads raise machine-checkable conflict payloads",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-ref-ok] users (key id) (name)) "
        "  (define [relation db-rule-agg-ref-ok] sale (user-id) (amount)) "
        "  (define [relation db-rule-agg-ref-ok] totals (ref users id user-id) (total)) "
        "  (deduce 'fact! users 1 \"Alice\") "
        "  (deduce 'fact! sale 1 10) "
        "  (deduce 'fact! sale 1 15) "
        "  (deduce/rule! db-rule-agg-ref-ok '(totals ?u (sum ?amount)) '(sale ?u ?amount)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-ref-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (deduce 'count totals) 1)))",
        "[PASS] deduce aggregate reference rule heads publish with final-snapshot integrity validation",
        "[FAIL] deduce aggregate reference rule heads publish with final-snapshot integrity validation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-key-rec (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-key-rec] edge (src) (dst)) "
        "  (define [relation db-rule-agg-key-rec] unit (node) (n)) "
        "  (define [relation db-rule-agg-key-rec] total (key node) (total)) "
        "  (deduce 'fact! edge 1 2) "
        "  (deduce 'fact! edge 2 3) "
        "  (deduce 'fact! unit 3 1) "
        "  (deduce/rule! db-rule-agg-key-rec '(total ?x (sum ?n)) '(unit ?x ?n)) "
        "  (deduce/rule! db-rule-agg-key-rec '(total ?x (sum ?n)) '(edge ?x ?y) '(total ?y ?n)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-key-rec)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (length (deduce 'scan total)) 3)))",
        "[PASS] deduce recursive aggregate keyed rule heads run through seminaive integrity enforcement",
        "[FAIL] deduce recursive aggregate keyed rule heads run through seminaive integrity enforcement",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-check-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-check-conflict] amount-src (id) (amount)) "
            "  (define [relation db-rule-check-conflict] checked-out (id) (check int? amount)) "
            "  (deduce 'fact! amount-src 1 7) "
            "  (deduce 'fact! amount-src 2 \"oops\") "
            "  (deduce/rule! db-rule-check-conflict '(checked-out ?id ?amount) '(amount-src ?id ?amount)) "
            "  db-rule-check-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* predicate = deduce_rule_test_dict_get(interp, data, "predicate");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-check-failed") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "checked-out") &&
                 predicate != null &&
                 predicate.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(predicate.sym_val), "int?") &&
                 deduce_test_string_equals(key_value, "oops");
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived check constraints raise machine-checkable failure payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived check constraints raise machine-checkable failure payloads");
            (*fail)++;
        }
    }
}

fn void run_deduce_deferred_integrity_commit_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-deferred-ok (deduce 'open 'memory)) "
        "  (define [relation db-deferred-ok] users-deferred-ok (key id) (name)) "
        "  (define [relation db-deferred-ok] audit-deferred-ok (ref users-deferred-ok id user-id) (action)) "
        "  (define tx (deduce 'block db-deferred-ok 'write-deferred)) "
        "  (deduce 'fact! tx users-deferred-ok 42 \"Alice\") "
        "  (deduce 'fact! tx audit-deferred-ok 42 \"login\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-deferred-ok) 1) "
        "       (= (deduce 'count audit-deferred-ok) 1)))",
        "[PASS] deduce deferred write commit validates staged reference success",
        "[FAIL] deduce deferred write commit validates staged reference success",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-deferred (deduce 'open 'memory)) "
        "  (define [relation db-key-deferred] rel-key-deferred (key id) (name)) "
        "  (deduce 'fact! rel-key-deferred 1 \"Alice\") "
        "  (define tx (deduce 'block db-key-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-key-deferred 1 \"Bob\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "       (= (deduce 'count rel-key-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects keyed conflict and rolls back",
        "[FAIL] deduce deferred write commit rejects keyed conflict and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-deferred (deduce 'open 'memory)) "
        "  (define [relation db-unique-deferred] rel-unique-deferred (id) (unique email)) "
        "  (deduce 'fact! rel-unique-deferred 1 \"alice@x\") "
        "  (define tx (deduce 'block db-unique-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-unique-deferred 2 \"alice@x\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "       (= (deduce 'count rel-unique-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects unique conflict and rolls back",
        "[FAIL] deduce deferred write commit rejects unique conflict and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-deferred] users-ref-deferred (key id) (name)) "
        "  (define [relation db-ref-deferred] audit-ref-deferred (ref users-ref-deferred id user-id) (action)) "
        "  (define tx (deduce 'block db-ref-deferred 'write-deferred)) "
        "  (deduce 'fact! tx audit-ref-deferred 42 \"login\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "       (= (deduce 'count audit-ref-deferred) 0)))",
        "[PASS] deduce deferred write commit rejects missing reference and rolls back",
        "[FAIL] deduce deferred write commit rejects missing reference and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-deferred (deduce 'open 'memory)) "
        "  (define [relation db-check-deferred] rel-check-deferred (id) (check int? amount)) "
        "  (define tx (deduce 'block db-check-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-check-deferred 1 \"oops\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-check-failed) "
        "       (= (deduce 'count rel-check-deferred) 0)))",
        "[PASS] deduce deferred write commit rejects failed check constraints and rolls back",
        "[FAIL] deduce deferred write commit rejects failed check constraints and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-delete-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-delete-deferred] users-ref-delete-deferred (key id) (name)) "
        "  (define [relation db-ref-delete-deferred] audit-ref-delete-deferred (ref users-ref-delete-deferred id user-id) (action)) "
        "  (deduce 'fact! users-ref-delete-deferred 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-delete-deferred 42 \"login\") "
        "  (define tx (deduce 'block db-ref-delete-deferred 'write-deferred)) "
        "  (deduce 'retract! tx users-ref-delete-deferred 42 \"Alice\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "       (= (deduce 'count users-ref-delete-deferred) 1) "
        "       (= (deduce 'count audit-ref-delete-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects delete that leaves dangling reference",
        "[FAIL] deduce deferred write commit rejects delete that leaves dangling reference",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-delete-ok-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-delete-ok-deferred] users-ref-delete-ok-deferred (key id) (name)) "
        "  (define [relation db-ref-delete-ok-deferred] audit-ref-delete-ok-deferred (ref users-ref-delete-ok-deferred id user-id) (action)) "
        "  (deduce 'fact! users-ref-delete-ok-deferred 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-delete-ok-deferred 42 \"login\") "
        "  (define tx (deduce 'block db-ref-delete-ok-deferred 'write-deferred)) "
        "  (deduce 'retract! tx audit-ref-delete-ok-deferred 42 \"login\") "
        "  (deduce 'retract! tx users-ref-delete-ok-deferred 42 \"Alice\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-ref-delete-ok-deferred) 0) "
        "       (= (deduce 'count audit-ref-delete-ok-deferred) 0)))",
        "[PASS] deduce deferred write commit allows delete after staged blocker removal",
        "[FAIL] deduce deferred write commit allows delete after staged blocker removal",
        pass,
        fail
    );
}

fn void run_deduce_reference_retract_integrity_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-retract (deduce 'open 'memory)) "
        "  (define [relation db-ref-retract] users-ref-retract (key id) (name)) "
        "  (define [relation db-ref-retract] audit-ref-retract (ref users-ref-retract id user-id) (action)) "
        "  (deduce 'fact! users-ref-retract 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-retract 42 \"login\") "
        "  (define payload (handle (deduce 'retract! users-ref-retract 42 \"Alice\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-retract) 1)))",
        "[PASS] deduce retract! rejects deleting referenced target tuple",
        "[FAIL] deduce retract! rejects deleting referenced target tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-retract-tx (deduce 'open 'memory)) "
        "  (define [relation db-ref-retract-tx] users-ref-retract-tx (key id) (name)) "
        "  (define [relation db-ref-retract-tx] audit-ref-retract-tx (ref users-ref-retract-tx id user-id) (action)) "
        "  (deduce 'fact! users-ref-retract-tx 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-retract-tx 42 \"login\") "
        "  (define tx (deduce 'block db-ref-retract-tx 'write)) "
        "  (deduce 'retract! tx audit-ref-retract-tx 42 \"login\") "
        "  (deduce 'retract! tx users-ref-retract-tx 42 \"Alice\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-ref-retract-tx) 0) "
        "       (= (deduce 'count audit-ref-retract-tx) 0)))",
        "[PASS] deduce retract! allows deleting target after blocker is removed in same txn",
        "[FAIL] deduce retract! allows deleting target after blocker is removed in same txn",
        pass,
        fail
    );
}

fn void run_deduce_reference_bulk_delete_integrity_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-clear (deduce 'open 'memory)) "
        "  (define [relation db-ref-clear] users-ref-clear (key id) (name)) "
        "  (define [relation db-ref-clear] audit-ref-clear (ref users-ref-clear id user-id) (action)) "
        "  (deduce 'fact! users-ref-clear 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-clear 42 \"login\") "
        "  (define payload (handle (deduce 'clear! users-ref-clear) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-clear) 1) "
        "       (= (deduce 'count audit-ref-clear) 1)))",
        "[PASS] deduce clear! rejects clearing referenced target relation",
        "[FAIL] deduce clear! rejects clearing referenced target relation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-drop (deduce 'open 'memory)) "
        "  (define [relation db-ref-drop] users-ref-drop (key id) (name)) "
        "  (define [relation db-ref-drop] audit-ref-drop (ref users-ref-drop id user-id) (action)) "
        "  (deduce 'fact! users-ref-drop 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-drop 42 \"login\") "
        "  (define payload (handle (deduce 'drop! users-ref-drop) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-drop) 1) "
        "       (= (deduce 'count audit-ref-drop) 1)))",
        "[PASS] deduce drop! rejects dropping referenced target relation",
        "[FAIL] deduce drop! rejects dropping referenced target relation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-clear-ok (deduce 'open 'memory)) "
        "  (define [relation db-ref-clear-ok] users-ref-clear-ok (key id) (name)) "
        "  (define [relation db-ref-clear-ok] audit-ref-clear-ok (ref users-ref-clear-ok id user-id) (action)) "
        "  (deduce 'fact! users-ref-clear-ok 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-clear-ok 42 \"login\") "
        "  (deduce 'clear! audit-ref-clear-ok) "
        "  (deduce 'clear! users-ref-clear-ok) "
        "  (and (= (deduce 'count users-ref-clear-ok) 0) "
        "       (= (deduce 'count audit-ref-clear-ok) 0)))",
        "[PASS] deduce clear! allows clearing target after clearing blockers",
        "[FAIL] deduce clear! allows clearing target after clearing blockers",
        pass,
        fail
    );
}
