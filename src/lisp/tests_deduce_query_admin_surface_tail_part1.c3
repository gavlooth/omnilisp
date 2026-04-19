module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_tests_part1(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-txn-commit-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-insert] edge-txc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-insert] reach-txc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?y) '(edge-txc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?z) '(edge-txc ?x ?y) '(reach-txc ?y ?z)) "
            "  (define analyze0 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define txc (deduce 'block db-admin-txn-commit-insert 'write)) "
            "  (deduce 'fact! txc edge-txc 1 2) "
            "  (deduce 'fact! txc edge-txc 2 3) "
            "  (deduce 'commit txc) "
            "  (define analyze1 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define db-admin-txn-abort-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-abort-insert] edge-txa (src) (dst)) "
            "  (define [relation db-admin-txn-abort-insert] reach-txa (src) (dst)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?y) '(edge-txa ?x ?y)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?z) '(edge-txa ?x ?y) '(reach-txa ?y ?z)) "
            "  (define analyze2 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define txa (deduce 'block db-admin-txn-abort-insert 'write)) "
            "  (deduce 'fact! txa edge-txa 1 2) "
            "  (deduce 'fact! txa edge-txa 2 3) "
            "  (deduce 'abort txa) "
            "  (define analyze3 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define db-admin-txn-commit-delete (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-delete] edge-tdc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-delete] reach-tdc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?y) '(edge-tdc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?z) '(edge-tdc ?x ?y) '(reach-tdc ?y ?z)) "
            "  (deduce 'fact! edge-tdc 1 2) "
            "  (deduce 'fact! edge-tdc 2 3) "
            "  (define txd (deduce 'block db-admin-txn-commit-delete 'write)) "
            "  (deduce 'retract! txd edge-tdc 2 3) "
            "  (deduce 'commit txd) "
            "  (define analyze4 (deduce/analyze db-admin-txn-commit-delete)) "
            "  (and "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze1 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze1 'incremental-invalidation-mode) 'tracked) "
            "    (= (car (ref analyze1 'incremental-dirty-predicates)) 'edge-txc) "
            "    (= (car (cdr (ref analyze1 'incremental-dirty-predicates))) 'reach-txc) "
            "    (= (deduce 'count edge-txc) 2) "
            "    (= (ref analyze2 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze2 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze3 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicates) nil) "
            "    (= (deduce 'count edge-txa) 0) "
            "    (= (ref analyze4 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze4 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (car (ref analyze4 'incremental-dirty-predicates)) 'edge-tdc) "
            "    (= (car (cdr (ref analyze4 'incremental-dirty-predicates))) 'reach-tdc) "
            "    (= (deduce 'count edge-tdc) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce commit applies deferred mutation logs while abort drops them");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce commit applies deferred mutation logs while abort drops them (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce commit applies deferred mutation logs while abort drops them");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-degraded-analyze (deduce 'open 'memory)) "
            "  (define [relation db-admin-degraded-analyze] edge-dr (src) (dst)) "
            "  (define [relation db-admin-degraded-analyze] reach-dr (src) (dst)) "
            "  (deduce 'fact! edge-dr 1 2) "
            "  (deduce 'fact! edge-dr 2 3) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?y) '(edge-dr ?x ?y)) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?z) '(edge-dr ?x ?y) '(reach-dr ?y ?z)) "
            "  (deduce/analyze db-admin-degraded-analyze) "
            "  (deduce 'retract! edge-dr 2 3) "
            "  (define stats0 (deduce/stats reach-dr)) "
            "  (define analyze0 (deduce/analyze db-admin-degraded-analyze)) "
            "  (define stats1 (deduce/stats reach-dr)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 2) "
            "    (= (car (ref analyze0 'incremental-dirty-predicates)) 'edge-dr) "
            "    (= (car (cdr (ref analyze0 'incremental-dirty-predicates))) 'reach-dr) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= (ref stats1 'dirty-predicate-count) 0) "
            "    (= (deduce 'count reach-dr) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze snapshots degraded full-recompute state before clearing it");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-aggregate-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-edge-a (src) (dst)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-score-a (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?y) '(gd-score-a ?y ?n)) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-aggregate-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match] gd-edge-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-reach-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-src-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-out-m (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?y) '(gd-edge-m ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?z) '(gd-edge-m ?x ?y) '(gd-reach-m ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-other-out-m ?x ?y) '(gd-other-src-m ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (deduce/fact! gd-edge-m 1 2) "
            "  (deduce/fact! gd-edge-m 2 3) "
            "  (deduce/fact! gd-other-src-m 9 10) "
            "  (define selected (deduce/match gd-reach-m '(gd-reach-m ?x ?y) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-m)) "
            "  (define other-before (deduce/count gd-other-out-m)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (define other-after (deduce/count gd-other-out-m)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-edge-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-reach-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-src-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-out-md (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?y) '(gd-edge-md ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?z) '(gd-reach-md ?x ?y) '(gd-edge-md ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-other-out-md ?x ?y) '(gd-other-src-md ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand) "
            "  (deduce/fact! gd-edge-md 1 2) "
            "  (deduce/fact! gd-edge-md 2 3) "
            "  (deduce/fact! gd-edge-md 10 11) "
            "  (deduce/fact! gd-edge-md 11 12) "
            "  (deduce/fact! gd-other-src-md 20 21) "
            "  (define selected (deduce/match gd-reach-md '(gd-reach-md 1 ?y) 1)) "
            "  (define reach-stats (deduce/stats gd-reach-md)) "
            "  (define other-stats (deduce/stats gd-other-out-md)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (or (= (ref row 'y) 2) (= (ref row 'y) 3))) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-edge-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-reach-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-src-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-out-mm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?y) '(gd-edge-mm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?z) '(gd-edge-mm ?x ?y) '(gd-reach-mm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-other-out-mm ?x ?y) '(gd-other-src-mm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-mismatch) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-other-out-mm '(gd-other-out-mm ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-mm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-edge-ma (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-score-ma (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?y) '(gd-score-ma ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-score-ma '(gd-score-ma ?x ?n) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }
}
