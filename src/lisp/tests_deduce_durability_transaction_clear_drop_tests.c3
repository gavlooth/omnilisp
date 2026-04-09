module lisp;

import std::io;

fn void run_deduce_transaction_commit_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-commit (deduce 'open 'memory)) "
            "  (define [relation txn-db-commit] txn_person_commit (name) (age)) "
            "  (define txc (deduce 'block txn-db-commit)) "
            "  (deduce 'fact! txc txn_person_commit \"Alice\" 33) "
            "  (define before (deduce 'count txn_person_commit)) "
            "  (deduce 'commit txc) "
            "  (define after (deduce 'count txn_person_commit)) "
            "  (and (= before 0) (= after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn commit");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn commit");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_abort_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-abort (deduce 'open 'memory)) "
            "  (define [relation txn-db-abort] txn_person_abort (name) (age)) "
            "  (define txa (deduce 'block txn-db-abort)) "
            "  (deduce 'fact! txa txn_person_abort \"Bob\" 21) "
            "  (deduce 'abort txa) "
            "  (= (deduce 'count txn_person_abort) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn abort");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn abort");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_db_mismatch_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-a (deduce 'open 'memory)) "
            "  (define txn-db-b (deduce 'open 'memory)) "
            "  (define [relation txn-db-b] txn_person_mismatch (name) (age)) "
            "  (define txm (deduce 'block txn-db-a)) "
            "  (deduce 'fact! txm txn_person_mismatch \"Eve\" 44))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce txn db mismatch");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn db mismatch");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_transaction_commit_test(interp, pass, fail);
    run_deduce_transaction_abort_test(interp, pass, fail);
    run_deduce_transaction_db_mismatch_test(interp, pass, fail);
}
fn void run_deduce_clear_preserves_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define clr-db (deduce 'open 'memory)) "
            "  (define [relation clr-db] clr-person (name) (age)) "
            "  (deduce 'fact! clr-person \"Alice\" 30) "
            "  (deduce 'fact! clr-person \"Bob\" 31) "
            "  (deduce 'clear! clr-person) "
            "  (define c0 (deduce 'count clr-person)) "
            "  (deduce 'fact! clr-person \"Carol\" 40) "
            "  (define c1 (deduce 'count clr-person)) "
            "  (and (= c0 0) (= c1 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce clear! preserves relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce clear! preserves relation");
            (*fail)++;
        }
    }
}

fn void run_deduce_drop_invalidates_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define drp-db (deduce 'open 'memory)) "
            "  (define [relation drp-db] drp-person (name) (age)) "
            "  (deduce 'fact! drp-person \"Alice\" 30) "
            "  (deduce 'drop! drp-person) "
            "  (deduce 'count drp-person))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce drop! invalidates relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce drop! invalidates relation");
            (*fail)++;
        }
    }
}

fn void run_deduce_clear_empty_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define clr-empty-db (deduce 'open 'memory)) "
            "  (define [relation clr-empty-db] clr-empty-person (name) (age)) "
            "  (define before (deduce 'count clr-empty-person)) "
            "  (deduce 'clear! clr-empty-person) "
            "  (define after (deduce 'count clr-empty-person)) "
            "  (and (= before 0) (= after 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce clear! on empty relation is deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce clear! on empty relation is deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce clear! on empty relation is deterministic");
            }
            (*fail)++;
        }
    }
}

fn void run_deduce_retract_missing_tuple_noop_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define retract-empty-db (deduce 'open 'memory)) "
        "  (define [relation retract-empty-db] retract-empty-person (name) (age)) "
        "  (deduce 'retract! retract-empty-person \"Alice\" 30))",
        "[PASS] deduce retract! on missing tuple is deterministic no-op",
        "[FAIL] deduce retract! on missing tuple is deterministic no-op (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_drop_repeat_noop_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define drop-repeat-db (deduce 'open 'memory)) "
        "  (define [relation drop-repeat-db] drop-repeat-person (name) (age)) "
        "  (deduce 'drop! drop-repeat-person) "
        "  (deduce 'drop! drop-repeat-person))",
        "[PASS] deduce drop! on dropped relation is deterministic no-op",
        "[FAIL] deduce drop! on dropped relation is deterministic no-op (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_dropped_cleanup_error_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define dropped-cleanup-db (deduce 'open 'memory)) "
        "  (define [relation dropped-cleanup-db] dropped-cleanup-person (name) (age)) "
        "  (deduce 'drop! dropped-cleanup-person) "
        "  (define retract-payload (handle (deduce 'retract! dropped-cleanup-person \"Alice\" 30) (raise msg msg))) "
        "  (define clear-payload (handle (deduce 'clear! dropped-cleanup-person) (raise msg msg))) "
        "  (and "
        "    (dict? retract-payload) "
        "    (= (ref retract-payload 'code) 'deduce/retract-relation-dropped) "
        "    (dict? clear-payload) "
        "    (= (ref clear-payload 'code) 'deduce/clear-relation-dropped)))",
        "[PASS] deduce cleanup paths keep dropped relation errors deterministic",
        "[FAIL] deduce cleanup paths keep dropped relation errors deterministic",
        pass,
        fail
    );
}

fn void run_deduce_clear_drop_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_clear_preserves_relation_test(interp, pass, fail);
    run_deduce_drop_invalidates_relation_test(interp, pass, fail);
    run_deduce_clear_empty_relation_test(interp, pass, fail);
    run_deduce_retract_missing_tuple_noop_test(interp, pass, fail);
    run_deduce_drop_repeat_noop_test(interp, pass, fail);
    run_deduce_dropped_cleanup_error_test(interp, pass, fail);

    test_error_contains(
        interp,
        "[PASS] deduce open-named rejects relation arity conflict deterministically",
        "(block "
        "  (define arity-conflict-db (deduce 'open 'memory)) "
        "  (define rel-a (deduce 'open-named arity-conflict-db 'arity_conflict \"arity_conflict_v1\" 'name 'age)) "
        "  (deduce 'open-named arity-conflict-db 'arity_conflict \"arity_conflict_v2\" 'name 'age 'email))",
        "relation name already registered with different arity",
        pass,
        fail
    );
}
