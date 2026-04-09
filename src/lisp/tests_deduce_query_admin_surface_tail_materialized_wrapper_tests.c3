module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_materialized_wrapper_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-unready (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-unready materialized] ancestor-unready (src) (dst)) "
            "  (define schema0 (deduce/schema ancestor-unready)) "
            "  (define stats0 (deduce/stats ancestor-unready)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized-unready)) "
            "  (define payload0 (handle (deduce/refresh! ancestor-unready) (raise msg msg))) "
            "  (define [relation db-admin-materialized-unready] edge-unready (src) (dst)) "
            "  (deduce 'fact! edge-unready 1 2) "
            "  (deduce/rule! db-admin-materialized-unready '(ancestor-unready ?x ?y) '(edge-unready ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-unready)) "
            "  (define analyze1 (deduce/analyze db-admin-materialized-unready)) "
            "  (define refreshed (deduce/refresh! ancestor-unready)) "
            "  (define schema2 (deduce/schema ancestor-unready)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) false) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats0 'materialized-derived-ready) false) "
            "    (= (ref stats0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze0 'ready-materialized-view-count) 0) "
            "    (= (ref analyze0 'unready-materialized-view-count) 1) "
            "    (dict? payload0) "
            "    (= (ref payload0 'code) 'deduce/refresh-materialized-relation-not-derived) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze1 'ready-materialized-view-count) 1) "
            "    (= (ref analyze1 'unready-materialized-view-count) 0) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-unready) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (ref schema2 'materialized-stale) false) "
            "    (= (ref schema2 'materialized-derived-ready) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce declared materialized relations become refreshable only after rule install");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce declared materialized relations become refreshable only after rule install (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce declared materialized relations become refreshable only after rule install");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-decl (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-decl] edge-decl (src) (dst)) "
            "  (define [relation db-admin-materialized-decl materialized] ancestor-decl (src) (dst)) "
            "  (deduce 'fact! edge-decl 1 2) "
            "  (deduce/rule! db-admin-materialized-decl '(ancestor-decl ?x ?y) '(edge-decl ?x ?y)) "
            "  (define schema0 (deduce/schema ancestor-decl)) "
            "  (define refreshed (deduce/refresh! ancestor-decl)) "
            "  (define schema1 (deduce/schema ancestor-decl)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-decl) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref schema1 'materialized-refresh-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce [relation ... materialized] declaration syntax aligns with refresh metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce [relation ... materialized] declaration syntax aligns with refresh metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce [relation ... materialized] declaration syntax aligns with refresh metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-on-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-on-read materialized on-read] ancestor-on-read (src) (dst)) "
            "  (define schema0 (deduce/schema ancestor-on-read)) "
            "  (define payload0 (handle (deduce 'count ancestor-on-read) (raise msg msg))) "
            "  (define [relation db-admin-materialized-on-read] edge-on-read (src) (dst)) "
            "  (deduce 'fact! edge-on-read 1 2) "
            "  (deduce/rule! db-admin-materialized-on-read '(ancestor-on-read ?x ?y) '(edge-on-read ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-on-read)) "
            "  (define count1 (deduce 'count ancestor-on-read)) "
            "  (define schema2 (deduce/schema ancestor-on-read)) "
            "  (deduce 'fact! edge-on-read 2 3) "
            "  (define count2 (deduce 'count ancestor-on-read)) "
            "  (define schema3 (deduce/schema ancestor-on-read)) "
            "  (and "
            "    (= (ref schema0 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema0 'materialized-derived-ready) false) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (dict? payload0) "
            "    (= (ref payload0 'code) 'deduce/on-read-refresh-unavailable) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-stale) true) "
            "    (= count1 1) "
            "    (= (ref schema2 'materialized-stale) false) "
            "    (= (ref schema2 'materialized-refresh-count) 1) "
            "    (= (ref schema2 'materialized-refresh-policy) 'on-read) "
            "    (= count2 2) "
            "    (= (ref schema3 'materialized-refresh-count) 2) "
            "    (= (ref schema3 'materialized-last-stale-reason) nil) "
            "    (= (ref schema3 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce on-read materialized relations refresh on ordinary stale reads once derived");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce on-read materialized relations refresh on ordinary stale reads once derived (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce on-read materialized relations refresh on ordinary stale reads once derived");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialize-miss (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialize-miss] ext-only (id) (name)) "
            "  (define payload (handle (deduce 'materialize! ext-only) (raise msg msg))) "
            "  (and (dict? payload) "
            "       (= (ref payload 'code) 'deduce/materialize-relation-not-derived)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce materialize! rejects non-derived relations");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce materialize! rejects non-derived relations (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce materialize! rejects non-derived relations");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized] edge (src) (dst)) "
            "  (define [relation db-admin-materialized] ancestor (src) (dst)) "
            "  (define [relation db-admin-materialized materialized] pending-view (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db-admin-materialized '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define pending0 (deduce/schema pending-view)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized)) "
            "  (define refreshed (deduce/refresh! db-admin-materialized)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define stats1 (deduce/stats ancestor)) "
            "  (define pending-stats1 (deduce/stats pending-view)) "
            "  (define pending1 (deduce/schema pending-view)) "
            "  (deduce 'fact! edge 2 3) "
            "  (define schema2 (deduce/schema ancestor)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref pending0 'materialized-derived-ready) false) "
            "    (= (ref pending0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze0 'materialized-view-count) 2) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref analyze0 'unready-materialized-view-count) 1) "
            "    (= (ref analyze0 'stale-materialized-view-count) 2) "
            "    (= (car (ref analyze0 'ready-materialized-relations)) 'ancestor) "
            "    (= (car (ref analyze0 'unready-materialized-relations)) 'pending-view) "
            "    (= (car (ref analyze0 'stale-materialized-relations)) 'ancestor) "
            "    (= (car (cdr (ref analyze0 'stale-materialized-relations))) 'pending-view) "
            "    (= (car (ref analyze0 'incremental-dirty-predicates)) 'edge) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'database) "
            "    (= (ref refreshed 'refresh-execution-path) 'db-wide) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'status) 'ok) "
            "    (= (ref refreshed 'stale-materialized-view-count) 1) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'pending-view) "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref schema1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'materialized-view) true) "
            "    (= (ref stats1 'materialized-derived-ready) true) "
            "    (= (ref stats1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'dirty-predicates) nil) "
            "    (= (ref stats1 'dirty-self) false) "
            "    (= (ref stats1 'materialized-stale) false) "
            "    (= (ref pending1 'materialized-stale) true) "
            "    (= (ref pending1 'materialized-derived-ready) false) "
            "    (= (ref pending-stats1 'dirty-self) false) "
            "    (= (ref schema2 'materialized-stale) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking");
            }
            (*fail)++;
        }
    }

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
