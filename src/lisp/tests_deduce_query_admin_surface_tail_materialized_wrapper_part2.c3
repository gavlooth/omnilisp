module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_materialized_wrapper_tests_part2(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-empty (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-empty] edge-empty-refresh (src) (dst)) "
            "  (define [relation db-admin-materialized-empty] ancestor-empty-refresh (src) (dst)) "
            "  (deduce/rule! db-admin-materialized-empty '(ancestor-empty-refresh ?x ?y) '(edge-empty-refresh ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-empty '(ancestor-empty-refresh ?x ?z) '(edge-empty-refresh ?x ?y) '(ancestor-empty-refresh ?y ?z)) "
            "  (deduce/materialize! ancestor-empty-refresh) "
            "  (define stats0 (deduce/stats ancestor-empty-refresh)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized-empty)) "
            "  (define refreshed (deduce/refresh! ancestor-empty-refresh)) "
            "  (define stats1 (deduce/stats ancestor-empty-refresh)) "
            "  (and "
            "    (= (ref stats0 'materialized-derived-ready) true) "
            "    (= (ref stats0 'materialized-refresh-count) 0) "
            "    (= (ref stats0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats0 'materialized-stale) true) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref analyze0 'stale-materialized-view-count) 1) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-empty-refresh) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (deduce 'count ancestor-empty-refresh) 0) "
            "    (= (ref stats1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'materialized-last-stale-reason) nil) "
            "    (= (ref stats1 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce refresh! keeps empty materialized relations deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce refresh! keeps empty materialized relations deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce refresh! keeps empty materialized relations deterministic");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-missing-maintenance (deduce 'open 'memory)) "
            "  (define [relation db-admin-missing-maintenance] edge-missing (src) (dst)) "
            "  (define [relation db-admin-missing-maintenance] ancestor-missing (src) (dst)) "
            "  (deduce/rule! db-admin-missing-maintenance '(ancestor-missing ?x ?y) '(edge-missing ?x ?y)) "
            "  (deduce/materialize! ancestor-missing) "
            "  (deduce 'drop! ancestor-missing) "
            "  (define stats-payload (deduce/stats ancestor-missing)) "
            "  (define refresh-payload (handle (deduce/refresh! ancestor-missing) (raise msg msg))) "
            "  (and "
            "    (= (ref stats-payload 'kind) 'deduce-stats) "
            "    (= (ref stats-payload 'status) 'ok) "
            "    (= (ref stats-payload 'relation) 'ancestor-missing) "
            "    (= (ref stats-payload 'materialized-view) true) "
            "    (= (ref stats-payload 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats-payload 'dropped) true) "
            "    (dict? refresh-payload) "
            "    (= (ref refresh-payload 'code) 'deduce/refresh-relation-dropped)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats/refresh keep dropped relation handling deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats/refresh keep dropped relation handling deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats/refresh keep dropped relation handling deterministic");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-refresh-reject (deduce 'open 'memory)) "
            "  (define [relation db-admin-refresh-reject] edge-r (src) (dst)) "
            "  (define [relation db-admin-refresh-reject] ancestor-r (src) (dst)) "
            "  (deduce/rule! db-admin-refresh-reject '(ancestor-r ?x ?y) '(edge-r ?x ?y)) "
            "  (define payload (handle (deduce/refresh! ancestor-r) (raise msg msg))) "
            "  (and (dict? payload) "
            "       (= (ref payload 'code) 'deduce/refresh-relation-not-materialized)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce refresh! rejects non-materialized relation handles");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce refresh! rejects non-materialized relation handles (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce refresh! rejects non-materialized relation handles");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-cleanup-errors (deduce 'open 'memory)) "
            "  (define [relation db-admin-cleanup-errors] cleanup-person (name) (age)) "
            "  (define db-admin-cleanup-other (deduce 'open 'memory)) "
            "  (define [relation db-admin-cleanup-other] cleanup-other (name) (age)) "
            "  (define tx (deduce 'block db-admin-cleanup-errors 'write)) "
            "  (define arity-payload (handle (deduce 'retract! cleanup-person \"Alice\") (raise msg msg))) "
            "  (define mismatch-payload (handle (deduce 'retract! tx cleanup-other \"Alice\" 30) (raise msg msg))) "
            "  (deduce 'abort tx) "
            "  (define clear-payload (handle (deduce 'clear! db-admin-cleanup-errors) (raise msg msg))) "
            "  (define drop-payload (handle (deduce 'drop! db-admin-cleanup-errors) (raise msg msg))) "
            "  (and "
            "    (dict? arity-payload) "
            "    (= (ref arity-payload 'code) 'deduce/retract-arity-mismatch) "
            "    (dict? mismatch-payload) "
            "    (= (ref mismatch-payload 'code) 'deduce/retract-transaction-database-mismatch) "
            "    (dict? clear-payload) "
            "    (= (ref clear-payload 'code) 'deduce/clear-relation-not-deduce-relation) "
            "    (dict? drop-payload) "
            "    (= (ref drop-payload 'code) 'deduce/drop-relation-not-deduce-relation) "
            "    (= (deduce 'count cleanup-person) 0) "
            "    (= (deduce 'count cleanup-other) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce cleanup write-intent errors stay code-first under schema and handle mismatch");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce cleanup write-intent errors stay code-first under schema and handle mismatch (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce cleanup write-intent errors stay code-first under schema and handle mismatch");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rel (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rel] edge-rel (src) (dst)) "
            "  (define [relation db-admin-materialized-rel] ancestor-rel (src) (dst)) "
            "  (define [relation db-admin-materialized-rel] ancestor-peer-rel (src) (dst)) "
            "  (deduce 'fact! edge-rel 1 2) "
            "  (deduce/rule! db-admin-materialized-rel '(ancestor-rel ?x ?y) '(edge-rel ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rel '(ancestor-peer-rel ?x ?y) '(edge-rel ?x ?y)) "
            "  (deduce/materialize! ancestor-rel) "
            "  (deduce/materialize! ancestor-peer-rel) "
            "  (deduce/refresh! db-admin-materialized-rel) "
            "  (deduce 'fact! edge-rel 2 3) "
            "  (define stale-schema (deduce/schema ancestor-rel)) "
            "  (define stale-peer-schema (deduce/schema ancestor-peer-rel)) "
            "  (define stale-stats (deduce/stats ancestor-rel)) "
            "  (define stale-peer-stats (deduce/stats ancestor-peer-rel)) "
            "  (define refreshed (deduce/refresh! ancestor-rel)) "
            "  (define fresh-schema (deduce/schema ancestor-rel)) "
            "  (define still-stale-peer-schema (deduce/schema ancestor-peer-rel)) "
            "  (define still-stale-peer-stats (deduce/stats ancestor-peer-rel)) "
            "  (define analyze-after (deduce/analyze db-admin-materialized-rel)) "
            "  (and "
            "    (= (ref stale-schema 'materialized-stale) true) "
            "    (= (ref stale-peer-schema 'materialized-stale) true) "
            "    (= (ref stale-schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (> (ref stale-schema 'materialized-last-stale-mutation-epoch) 0) "
            "    (= (car (ref stale-stats 'dirty-predicates)) 'edge-rel) "
            "    (= (car (cdr (ref stale-stats 'dirty-predicates))) 'ancestor-rel) "
            "    (= (car (cdr (cdr (ref stale-stats 'dirty-predicates)))) 'ancestor-peer-rel) "
            "    (= (ref stale-stats 'dirty-self) true) "
            "    (= (ref stale-peer-stats 'dirty-self) true) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor-rel) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-rel) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-rel) "
            "    (= (car (ref refreshed 'cleared-dirty-predicates)) 'edge-rel) "
            "    (= (car (cdr (ref refreshed 'cleared-dirty-predicates))) 'ancestor-rel) "
            "    (= (car (ref refreshed 'remaining-dirty-predicates)) 'ancestor-peer-rel) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'ancestor-peer-rel) "
            "    (= (deduce 'count ancestor-rel) 2) "
            "    (= (deduce 'count ancestor-peer-rel) 2) "
            "    (= (ref refreshed 'materialized-stale) false) "
            "    (= (ref fresh-schema 'materialized-stale) nil) "
            "    (= (ref fresh-schema 'materialized-refresh-count) 2) "
            "    (= (ref fresh-schema 'materialized-last-stale-reason) nil) "
            "    (= (ref still-stale-peer-schema 'materialized-stale) true) "
            "    (= (ref still-stale-peer-stats 'dirty-self) true) "
            "    (= (ref analyze-after 'stale-materialized-view-count) 1) "
            "    (= (car (ref analyze-after 'stale-materialized-relations)) 'ancestor-peer-rel)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce relation-scoped refresh and stale-reason metadata stay aligned");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce relation-scoped refresh and stale-reason metadata stay aligned (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce relation-scoped refresh and stale-reason metadata stay aligned");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-chain] edge-chain (src) (dst)) "
            "  (define [relation db-admin-materialized-chain] mid-chain (src) (dst)) "
            "  (define [relation db-admin-materialized-chain] top-chain (src) (dst)) "
            "  (deduce 'fact! edge-chain 1 2) "
            "  (deduce/rule! db-admin-materialized-chain '(mid-chain ?x ?y) '(edge-chain ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-chain '(top-chain ?x ?y) '(mid-chain ?x ?y)) "
            "  (deduce/materialize! mid-chain) "
            "  (deduce/materialize! top-chain) "
            "  (deduce/refresh! db-admin-materialized-chain) "
            "  (deduce 'fact! edge-chain 2 3) "
            "  (define mid-refreshed (deduce/refresh! mid-chain)) "
            "  (define top-stale-schema (deduce/schema top-chain)) "
            "  (define top-stale-count (deduce 'count top-chain)) "
            "  (define top-refreshed (deduce/refresh! top-chain)) "
            "  (define top-fresh-schema (deduce/schema top-chain)) "
            "  (and "
            "    (= (ref mid-refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref mid-refreshed 'remaining-dirty-predicates) '(top-chain)) "
            "    (= (ref top-stale-schema 'materialized-stale) true) "
            "    (= (ref top-stale-schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (= top-stale-count 1) "
            "    (= (ref top-refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref top-refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref top-refreshed 'refreshed-relation) 'top-chain) "
            "    (= (deduce 'count mid-chain) 2) "
            "    (= (deduce 'count top-chain) 2) "
            "    (= (ref top-fresh-schema 'materialized-refresh-count) 2) "
            "    (= (ref top-fresh-schema 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce incremental refresh can propagate one step through a refreshed materialized source");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce incremental refresh can propagate one step through a refreshed materialized source (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce incremental refresh can propagate one step through a refreshed materialized source");
            }
            (*fail)++;
        }
    }
}
