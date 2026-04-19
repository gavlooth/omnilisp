module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_fallback_demand_tests_part2(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-edge-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-reach-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-other-src-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-other-out-pmd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-reach-pmd ?x ?y) '(gd-edge-pmd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-reach-pmd ?x ?z) '(gd-reach-pmd ?x ?y) '(gd-edge-pmd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-other-out-pmd ?x ?y) '(gd-other-src-pmd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-plain) "
            "  (deduce/fact! gd-edge-pmd 1 2) "
            "  (deduce/fact! gd-edge-pmd 2 3) "
            "  (deduce/fact! gd-edge-pmd 10 11) "
            "  (deduce/fact! gd-edge-pmd 11 12) "
            "  (deduce/fact! gd-other-src-pmd 20 21) "
            "  (define selected (deduce/match gd-reach-pmd '(gd-reach-pmd 1 ?y))) "
            "  (define reach-stats (deduce/stats gd-reach-pmd)) "
            "  (define other-stats (deduce/stats gd-other-out-pmd)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (or (= (ref row 'y) 2) (= (ref row 'y) 3))) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match with bound literals can use ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match with bound literals can use ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match with bound literals can use ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-projected] gd-edge-pmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-projected] gd-reach-pmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-projected '(gd-reach-pmp ?x ?y) '(gd-edge-pmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-projected '(gd-reach-pmp ?x ?z) '(gd-reach-pmp ?x ?y) '(gd-edge-pmp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-projected) "
            "  (deduce/fact! gd-edge-pmp 1 2) "
            "  (deduce/fact! gd-edge-pmp 2 3) "
            "  (deduce/fact! gd-edge-pmp 10 11) "
            "  (define selected (deduce/match gd-reach-pmp '(gd-reach-pmp 1 3))) "
            "  (define stats-after (deduce/stats gd-reach-pmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-pmp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match can project unsupported bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match can project unsupported bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match can project unsupported bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-selector-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-selector-projected] gd-edge-smp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-selector-projected] gd-reach-smp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-selector-projected '(gd-reach-smp ?x ?y) '(gd-edge-smp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-selector-projected '(gd-reach-smp ?x ?z) '(gd-reach-smp ?x ?y) '(gd-edge-smp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-selector-projected) "
            "  (deduce/fact! gd-edge-smp 1 2) "
            "  (deduce/fact! gd-edge-smp 2 3) "
            "  (deduce/fact! gd-edge-smp 10 11) "
            "  (define selected (deduce/match gd-reach-smp '(gd-reach-smp 1 3) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-smp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-selector-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stats-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-smp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match can project unsupported bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match can project unsupported bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match can project unsupported bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-prefix (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix] gd-edge-pfx (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix] gd-reach-pfx (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix '(gd-reach-pfx ?x ?y) '(gd-edge-pfx ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix '(gd-reach-pfx ?x ?z) '(gd-edge-pfx ?x ?y) '(gd-reach-pfx ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-prefix) "
            "  (deduce/fact! gd-edge-pfx 1 2) "
            "  (deduce/fact! gd-edge-pfx 2 5) "
            "  (deduce/fact! gd-edge-pfx 3 4) "
            "  (deduce/fact! gd-edge-pfx 4 5) "
            "  (deduce/fact! gd-edge-pfx 9 10) "
            "  (deduce/fact! gd-edge-pfx 10 5) "
            "  (define selected (deduce/match gd-reach-pfx '(gd-reach-pfx ?src 5) 1 'naive)) "
            "  (define stats-after (deduce/stats gd-reach-pfx)) "
            "  (define step-counters (ref stats-after 'last-goal-directed-read-step-counters)) "
            "  (define step0 (car step-counters)) "
            "  (define step1 (car (cdr step-counters))) "
            "  (and "
            "    (= (length selected) 6) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (length step-counters) 2) "
            "    (= (ref step0 'rows-read) 12) "
            "    (= (ref step1 'rows-read) 16) "
            "    (= (ref step0 'counter-kind) 'observed) "
            "    (= (ref step1 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-prefix-base (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-edge-pfxb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-gate-pfxb (src) (mid)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-tail-pfxb (mid) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-path-pfxb (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix-base '(gd-path-pfxb ?x ?y) '(gd-edge-pfxb ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix-base '(gd-path-pfxb ?x ?z) '(gd-path-pfxb ?x ?y) '(gd-gate-pfxb ?x ?m) '(gd-tail-pfxb ?m ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-prefix-base) "
            "  (deduce/fact! gd-edge-pfxb 1 2) "
            "  (deduce/fact! gd-edge-pfxb 1 3) "
            "  (deduce/fact! gd-edge-pfxb 9 90) "
            "  (deduce/fact! gd-edge-pfxb 10 100) "
            "  (deduce/fact! gd-gate-pfxb 1 7) "
            "  (deduce/fact! gd-gate-pfxb 1 8) "
            "  (deduce/fact! gd-gate-pfxb 9 70) "
            "  (deduce/fact! gd-gate-pfxb 9 71) "
            "  (deduce/fact! gd-gate-pfxb 10 72) "
            "  (deduce/fact! gd-gate-pfxb 10 73) "
            "  (deduce/fact! gd-tail-pfxb 7 10) "
            "  (deduce/fact! gd-tail-pfxb 8 11) "
            "  (deduce/fact! gd-tail-pfxb 70 12) "
            "  (deduce/fact! gd-tail-pfxb 71 13) "
            "  (deduce/fact! gd-tail-pfxb 72 14) "
            "  (deduce/fact! gd-tail-pfxb 73 15) "
            "  (define selected (deduce/match gd-path-pfxb '(gd-path-pfxb 1 ?dst) 1 'naive)) "
            "  (define stats-after (deduce/stats gd-path-pfxb)) "
            "  (define step-counters (ref stats-after 'last-goal-directed-read-step-counters)) "
            "  (define step0 (car step-counters)) "
            "  (define step1 (car (cdr step-counters))) "
            "  (define step2 (car (cdr (cdr step-counters)))) "
            "  (and "
            "    (= (length selected) 4) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (length step-counters) 3) "
            "    (= (ref step0 'counter-kind) 'observed) "
            "    (= (ref step1 'counter-kind) 'observed) "
            "    (= (ref step2 'counter-kind) 'observed) "
            "    (= (ref step0 'rows-read) 8) "
            "    (= (ref step1 'rows-read) 24) "
            "    (= (ref step2 'rows-read) 32)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match prefix-prunes positive non-recursive body scans");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match prefix-prunes positive non-recursive body scans (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match prefix-prunes positive non-recursive body scans");
            }
            (*fail)++;
        }
    }
}
