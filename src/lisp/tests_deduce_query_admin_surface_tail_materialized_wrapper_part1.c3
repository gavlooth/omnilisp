module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_materialized_wrapper_tests_part1(Interp* interp, int* pass, int* fail) {
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
}
