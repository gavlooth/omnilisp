module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_materialized_wrapper_tests_part3(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rule-change (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rule-change] edge-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] edge-b (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] ancestor-rule-change (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] ancestor-rule-change-peer (src) (dst)) "
            "  (deduce 'fact! edge-a 1 2) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change-peer ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/materialize! ancestor-rule-change) "
            "  (deduce/materialize! ancestor-rule-change-peer) "
            "  (deduce/refresh! ancestor-rule-change) "
            "  (deduce/refresh! ancestor-rule-change-peer) "
            "  (define schema0 (deduce/schema ancestor-rule-change)) "
            "  (define stats0 (deduce/stats ancestor-rule-change)) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change ?x ?y) '(edge-b ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-rule-change)) "
            "  (define stats1 (deduce/stats ancestor-rule-change)) "
            "  (define refreshed (deduce/refresh! ancestor-rule-change)) "
            "  (define stats2 (deduce/stats ancestor-rule-change)) "
            "  (and "
            "    (= (ref schema0 'materialized-stale) false) "
            "    (= (ref schema0 'materialized-last-stale-reason) nil) "
            "    (= (ref stats0 'full-recompute-required) false) "
            "    (= (ref schema1 'materialized-stale) true) "
            "    (= (ref schema1 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref stats1 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref stats1 'full-recompute-required) true) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'db-wide) "
            "    (= (ref refreshed 'refresh-fallback-reason) 'full-recompute-required) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor-rule-change) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 2) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-rule-change) "
            "    (= (car (cdr (ref refreshed 'refreshed-materialized-relations))) 'ancestor-rule-change-peer) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (ref stats2 'full-recompute-required) false) "
            "    (= (ref stats2 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce rule install invalidates refreshed materialized views with rule-set-change");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce rule install invalidates refreshed materialized views with rule-set-change (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce rule install invalidates refreshed materialized views with rule-set-change");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rule-change-aligned (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] edge-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] edge-b (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] ancestor-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] ancestor-peer-a (src) (dst)) "
            "  (deduce 'fact! edge-a 1 2) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-a ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-peer-a ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/materialize! ancestor-a) "
            "  (deduce/materialize! ancestor-peer-a) "
            "  (deduce/refresh! ancestor-a) "
            "  (deduce/refresh! ancestor-peer-a) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-a ?x ?y) '(edge-b ?x ?y)) "
            "  (define stats1 (deduce/stats ancestor-a)) "
            "  (define analyze1 (deduce/analyze db-admin-materialized-rule-change-aligned)) "
            "  (define refreshed (deduce/refresh! ancestor-a)) "
            "  (define analyze2 (deduce/analyze db-admin-materialized-rule-change-aligned)) "
            "  (define stats2 (deduce/stats ancestor-a)) "
            "  (and "
            "    (= (ref stats1 'full-recompute-required) true) "
            "    (= (ref stats1 'materialized-stale) true) "
            "    (= (ref analyze1 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (ref analyze1 'stale-materialized-view-count) 2) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'ancestor-peer-a) "
            "    (= (ref analyze2 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze2 'stale-materialized-view-count) 1) "
            "    (= (car (ref analyze2 'stale-materialized-relations)) 'ancestor-peer-a) "
            "    (= (ref stats2 'full-recompute-required) false) "
            "    (= (ref stats2 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-dematerialize (deduce 'open 'memory)) "
            "  (define [relation db-admin-dematerialize] edge-dem (src) (dst)) "
            "  (define [relation db-admin-dematerialize] ancestor-dem (src) (dst)) "
            "  (deduce 'fact! edge-dem 1 2) "
            "  (deduce/rule! db-admin-dematerialize '(ancestor-dem ?x ?y) '(edge-dem ?x ?y)) "
            "  (deduce/materialize! ancestor-dem) "
            "  (deduce/refresh! ancestor-dem) "
            "  (define schema0 (deduce/schema ancestor-dem)) "
            "  (define analyze0 (deduce/analyze db-admin-dematerialize)) "
            "  (deduce/dematerialize! ancestor-dem) "
            "  (define schema1 (deduce/schema ancestor-dem)) "
            "  (define stats1 (deduce/stats ancestor-dem)) "
            "  (define analyze1 (deduce/analyze db-admin-dematerialize)) "
            "  (define refresh-payload (handle (deduce/refresh! ancestor-dem) (raise msg msg))) "
            "  (deduce/materialize! ancestor-dem) "
            "  (define schema2 (deduce/schema ancestor-dem)) "
            "  (define analyze2 (deduce/analyze db-admin-dematerialize)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-refresh-count) 1) "
            "    (= (ref schema0 'materialized-stale) nil) "
            "    (= (ref analyze0 'materialized-view-count) 1) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref schema1 'materialized-view) nil) "
            "    (= (ref schema1 'materialized-derived-ready) nil) "
            "    (= (ref schema1 'materialized-refresh-policy) nil) "
            "    (= (ref schema1 'materialized-refresh-count) 0) "
            "    (= (ref schema1 'materialized-last-refresh-mutation-epoch) nil) "
            "    (= (ref schema1 'materialized-last-stale-mutation-epoch) nil) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) nil) "
            "    (= (ref stats1 'materialized-view) nil) "
            "    (= (ref stats1 'materialized-refresh-count) 0) "
            "    (= (ref analyze1 'materialized-view-count) 0) "
            "    (= (ref analyze1 'ready-materialized-view-count) 0) "
            "    (= (ref analyze1 'unready-materialized-view-count) 0) "
            "    (= (ref analyze1 'stale-materialized-view-count) 0) "
            "    (= (dict? refresh-payload) true) "
            "    (= (ref refresh-payload 'code) 'deduce/refresh-relation-not-materialized) "
            "    (= (ref schema2 'materialized-view) true) "
            "    (= (ref schema2 'materialized-derived-ready) true) "
            "    (= (ref schema2 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema2 'materialized-refresh-count) 0) "
            "    (= (ref schema2 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema2 'materialized-stale) true) "
            "    (= (ref analyze2 'materialized-view-count) 1) "
            "    (= (ref analyze2 'ready-materialized-view-count) 1) "
            "    (= (ref analyze2 'stale-materialized-view-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce dematerialize! clears materialized lifecycle state without dropping the relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce dematerialize! clears materialized lifecycle state without dropping the relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce dematerialize! clears materialized lifecycle state without dropping the relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rec-agg (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rec-agg] rec-unit (node) (n)) "
            "  (define [relation db-admin-materialized-rec-agg] rec-edge (src) (dst)) "
            "  (define [relation db-admin-materialized-rec-agg materialized] rec-score (node) (total)) "
            "  (define [relation db-admin-materialized-rec-agg materialized] rec-chosen (node) (total)) "
            "  (deduce/rule! db-admin-materialized-rec-agg '(rec-chosen ?x ?n) '(rec-score ?x ?n)) "
            "  (deduce/rule! db-admin-materialized-rec-agg '(rec-score ?x (sum ?n)) '(rec-unit ?x ?n)) "
            "  (deduce/rule! db-admin-materialized-rec-agg '(rec-score ?x (sum ?n)) '(rec-edge ?x ?y) '(rec-chosen ?y ?n)) "
            "  (deduce/fact! rec-unit 0 1) "
            "  (deduce/fact! rec-edge 0 1) "
            "  (deduce/fact! rec-unit 1 1) "
            "  (deduce/fact! rec-edge 1 2) "
            "  (deduce/fact! rec-unit 2 1) "
            "  (deduce/fact! rec-edge 2 3) "
            "  (deduce/fact! rec-unit 3 1) "
            "  (define analyze0 (deduce/analyze db-admin-materialized-rec-agg)) "
            "  (define refreshed (deduce/refresh! rec-score)) "
            "  (define score-schema (deduce/schema rec-score)) "
            "  (define chosen-schema (deduce/schema rec-chosen)) "
            "  (define analyze1 (deduce/analyze db-admin-materialized-rec-agg)) "
            "  (and "
            "    (= (ref analyze0 'stale-materialized-view-count) 2) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'requested-refresh-relation) 'rec-score) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 2) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'rec-score) "
            "    (= (car (cdr (ref refreshed 'refreshed-materialized-relations))) 'rec-chosen) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (deduce 'count rec-score) 4) "
            "    (= (deduce 'count rec-chosen) 4) "
            "    (= (ref score-schema 'materialized-stale) false) "
            "    (= (ref chosen-schema 'materialized-stale) false) "
            "    (= (ref analyze1 'stale-materialized-view-count) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce relation-scoped refresh preserves recursive aggregate materialized SCC accounting");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce relation-scoped refresh preserves recursive aggregate materialized SCC accounting (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce relation-scoped refresh preserves recursive aggregate materialized SCC accounting");
            }
            (*fail)++;
        }
    }
}
