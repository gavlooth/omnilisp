module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-edge-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-reach-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-other-src-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-other-out-qf (src) (dst)) "
            "  (deduce/fact! gd-edge-qf 1 2) "
            "  (deduce/fact! gd-edge-qf 2 3) "
            "  (deduce/fact! gd-other-src-qf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-reach-qf ?x ?y) '(gd-edge-qf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-reach-qf ?x ?z) '(gd-edge-qf ?x ?y) '(gd-reach-qf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-other-out-qf ?x ?y) '(gd-other-src-qf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-other-out-qf ?x ?y) '(gd-edge-qf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-full) "
            "  (deduce/retract! gd-edge-qf 2 3) "
            "  (define stats0 (deduce/stats gd-other-out-qf)) "
            "  (define selected (deduce/query gd-reach-qf (lambda (row) true))) "
            "  (define stats1 (deduce/stats gd-other-out-qf)) "
            "  (define other-after (deduce/count gd-other-out-qf)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (length selected) 1) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-edge-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-reach-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-other-src-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-other-out-mf (src) (dst)) "
            "  (deduce/fact! gd-edge-mf 1 2) "
            "  (deduce/fact! gd-edge-mf 2 3) "
            "  (deduce/fact! gd-other-src-mf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-reach-mf ?x ?y) '(gd-edge-mf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-reach-mf ?x ?z) '(gd-edge-mf ?x ?y) '(gd-reach-mf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-other-out-mf ?x ?y) '(gd-other-src-mf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-other-out-mf ?x ?y) '(gd-edge-mf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-full) "
            "  (deduce/retract! gd-edge-mf 2 3) "
            "  (define stats0 (deduce/stats gd-other-out-mf)) "
            "  (define selected (deduce/match gd-reach-mf '(gd-reach-mf ?x ?y))) "
            "  (define stats1 (deduce/stats gd-other-out-mf)) "
            "  (define other-after (deduce/count gd-other-out-mf)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (length selected) 1) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match falls back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match falls back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match falls back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-edge-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-reach-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-other-src-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-other-out-cf (src) (dst)) "
            "  (deduce/fact! gd-edge-cf 1 2) "
            "  (deduce/fact! gd-edge-cf 2 3) "
            "  (deduce/fact! gd-other-src-cf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-reach-cf ?x ?y) '(gd-edge-cf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-reach-cf ?x ?z) '(gd-edge-cf ?x ?y) '(gd-reach-cf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-other-out-cf ?x ?y) '(gd-other-src-cf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-other-out-cf ?x ?y) '(gd-edge-cf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-full) "
            "  (deduce/retract! gd-edge-cf 2 3) "
            "  (define count-selected (deduce/count gd-reach-cf)) "
            "  (define count-stats1 (deduce/stats gd-other-out-cf)) "
            "  (define count-other-after (deduce/count gd-other-out-cf)) "
            "  (define db-admin-goal-directed-scan-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-edge-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-reach-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-other-src-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-other-out-sf (src) (dst)) "
            "  (deduce/fact! gd-edge-sf 1 2) "
            "  (deduce/fact! gd-edge-sf 2 3) "
            "  (deduce/fact! gd-other-src-sf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-reach-sf ?x ?y) '(gd-edge-sf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-reach-sf ?x ?z) '(gd-edge-sf ?x ?y) '(gd-reach-sf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-other-out-sf ?x ?y) '(gd-other-src-sf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-other-out-sf ?x ?y) '(gd-edge-sf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-full) "
            "  (deduce/retract! gd-edge-sf 2 3) "
            "  (define scan-selected (deduce/scan gd-reach-sf)) "
            "  (define scan-stats1 (deduce/stats gd-other-out-sf)) "
            "  (define scan-other-after (deduce/count gd-other-out-sf)) "
            "  (define db-admin-goal-directed-scan-range-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-edge-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-reach-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-other-src-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-other-out-srf (src) (dst)) "
            "  (deduce/fact! gd-edge-srf 1 2) "
            "  (deduce/fact! gd-edge-srf 2 3) "
            "  (deduce/fact! gd-other-src-srf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-reach-srf ?x ?y) '(gd-edge-srf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-reach-srf ?x ?z) '(gd-edge-srf ?x ?y) '(gd-reach-srf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-other-out-srf ?x ?y) '(gd-other-src-srf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-other-out-srf ?x ?y) '(gd-edge-srf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-full) "
            "  (deduce/retract! gd-edge-srf 2 3) "
            "  (define range-selected (deduce/scan-range gd-reach-srf '(1 2) '(1 2))) "
            "  (define range-stats1 (deduce/stats gd-other-out-srf)) "
            "  (define range-other-after (deduce/count gd-other-out-srf)) "
            "  (and "
            "    (= count-selected 1) "
            "    (= (ref count-stats1 'full-recompute-required) false) "
            "    (= count-other-after 2) "
            "    (= (length scan-selected) 1) "
            "    (= (ref scan-stats1 'full-recompute-required) false) "
            "    (= scan-other-after 2) "
            "    (= (length range-selected) 1) "
            "    (= (ref range-stats1 'full-recompute-required) false) "
            "    (= range-other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-selector-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-selector-full] gd-edge-sel-full (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selector-full] gd-reach-sel-full (src) (dst)) "
            "  (deduce/fact! gd-edge-sel-full 1 2) "
            "  (deduce/fact! gd-edge-sel-full 2 3) "
            "  (deduce/rule! db-admin-goal-directed-selector-full '(gd-reach-sel-full ?x ?y) '(gd-edge-sel-full ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-selector-full '(gd-reach-sel-full ?x ?z) '(gd-edge-sel-full ?x ?y) '(gd-reach-sel-full ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-selector-full) "
            "  (deduce/retract! gd-edge-sel-full 2 3) "
            "  (define query-payload "
            "    (handle "
            "      (deduce/query gd-reach-sel-full (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define match-payload "
            "    (handle "
            "      (deduce/match gd-reach-sel-full '(gd-reach-sel-full ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define count-payload "
            "    (handle "
            "      (deduce/count gd-reach-sel-full 1) "
            "      (raise msg msg))) "
            "  (define scan-payload "
            "    (handle "
            "      (deduce/scan gd-reach-sel-full 1) "
            "      (raise msg msg))) "
            "  (define range-payload "
            "    (handle "
            "      (deduce/scan-range gd-reach-sel-full '(1 2) '(9 9) 1) "
            "      (raise msg msg))) "
            "  (and "
            "    (= (ref query-payload 'code) 'deduce/query-goal-directed-selector-full-recompute-required) "
            "    (= (ref match-payload 'code) 'deduce/match-goal-directed-selector-full-recompute-required) "
            "    (= (ref count-payload 'code) 'deduce/count-goal-directed-selector-full-recompute-required) "
            "    (= (ref scan-payload 'code) 'deduce/scan-goal-directed-selector-full-recompute-required) "
            "    (= (ref range-payload 'code) 'deduce/scan-range-goal-directed-selector-full-recompute-required)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-edge-md0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-reach-md0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md0 ?x ?y) '(gd-edge-md0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md0 ?x ?z) '(gd-reach-md0 ?x ?y) '(gd-edge-md0 ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand) "
            "  (deduce/fact! gd-edge-md0 1 2) "
            "  (deduce/fact! gd-edge-md0 2 3) "
            "  (deduce/fact! gd-edge-md0 10 11) "
            "  (deduce/fact! gd-edge-md0 11 12) "
            "  (define selected (deduce/match gd-reach-md0 '(gd-reach-md0 1 ?to) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-md0)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'to)) selected)) 5) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-mutation-epoch) (ref stats-after 'mutation-epoch)) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-md0) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution");
            }
            (*fail)++;
        }
    }

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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-multi (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-edge-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-path-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-src-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-out-mdm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?y) '(gd-edge-mdm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?z) '(gd-path-mdm ?x ?y) '(gd-path-mdm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-other-out-mdm ?x ?y) '(gd-other-src-mdm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-multi) "
            "  (deduce/fact! gd-edge-mdm 1 2) "
            "  (deduce/fact! gd-edge-mdm 2 3) "
            "  (deduce/fact! gd-edge-mdm 3 4) "
            "  (deduce/fact! gd-other-src-mdm 20 21) "
            "  (define selected (deduce/match gd-path-mdm '(gd-path-mdm 1 4))) "
            "  (define path-stats (deduce/stats gd-path-mdm)) "
            "  (define other-stats (deduce/stats gd-other-out-mdm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-multi)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref path-stats 'dirty-self) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-surface) 'match) "
            "    (= (ref path-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-path-mdm) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-edge-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-reach-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-src-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-out-srd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?y) '(gd-edge-srd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?z) '(gd-reach-srd ?x ?y) '(gd-edge-srd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-other-out-srd ?x ?y) '(gd-other-src-srd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand) "
            "  (deduce/fact! gd-edge-srd 1 2) "
            "  (deduce/fact! gd-edge-srd 2 3) "
            "  (deduce/fact! gd-edge-srd 10 11) "
            "  (deduce/fact! gd-edge-srd 11 12) "
            "  (deduce/fact! gd-other-src-srd 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srd '(1 0) '(1 999) 1 'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-srd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-edge-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-sym-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-src-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-out-srds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-edge-srds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-sym-srds ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-other-out-srds ?x ?y) '(gd-other-src-srds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-swapped) "
            "  (deduce/fact! gd-edge-srds 1 2) "
            "  (deduce/fact! gd-edge-srds 10 11) "
            "  (deduce/fact! gd-other-src-srds 20 21) "
            "  (define selected (deduce/scan-range gd-sym-srds '(1 0) '(1 999))) "
            "  (define sym-stats (deduce/stats gd-sym-srds)) "
            "  (define other-stats (deduce/stats gd-other-out-srds)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-edge-srp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-reach-srp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?y) '(gd-edge-srp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?z) '(gd-reach-srp ?x ?y) '(gd-edge-srp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-projected) "
            "  (deduce/fact! gd-edge-srp 1 2) "
            "  (deduce/fact! gd-edge-srp 2 3) "
            "  (deduce/fact! gd-edge-srp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-srp '(1 3) '(1 3))) "
            "  (define stats-after (deduce/stats gd-reach-srp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-selector-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-edge-ssp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-reach-ssp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?z) '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected) "
            "  (deduce/fact! gd-edge-ssp 1 2) "
            "  (deduce/fact! gd-edge-ssp 2 3) "
            "  (deduce/fact! gd-edge-ssp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-ssp '(1 3) '(1 3) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-ssp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stats-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-ssp) "
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
            emit_pass_literal("[PASS] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

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
            "    (= (deduce 'count ancestor-peer-rel) 1) "
            "    (= (ref refreshed 'materialized-stale) false) "
            "    (= (ref fresh-schema 'materialized-stale) false) "
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
            "  (define db-admin-goal-directed-positive (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-positive] gd-edge (src) (dst)) "
            "  (define [relation db-admin-goal-directed-positive] gd-reach (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?y) '(gd-edge ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?z) '(gd-edge ?x ?y) '(gd-reach ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-positive)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 1) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 0) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 2) "
            "    (= (ref first 'goal-directed-eligible) true) "
            "    (= (ref first 'goal-directed-shape) 'positive-recursive-closure) "
            "    (= (ref first 'goal-directed-blockers) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed eligible recursive component counts");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-blocked (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-edge-b (src) (dst)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-blocked-b (node)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-chosen-b (src) (score)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-score-b (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-chosen-b ?x ?n) '(gd-score-b ?x ?n) '(not (gd-blocked-b ?x))) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?y) '(gd-chosen-b ?y ?n)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-blocked)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (define blockers (ref first 'goal-directed-blockers)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 0) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 1) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 3) "
            "    (= (ref first 'goal-directed-eligible) nil) "
            "    (= (ref first 'goal-directed-shape) nil) "
            "    (= (car blockers) 'aggregates-present) "
            "    (= (car (cdr blockers)) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-batches (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-batches)) "
            "  (and "
            "    (= (ref analyze 'parallel-runtime-mode) 'metadata-only) "
            "    (= (ref analyze 'parallel-recursive-component-count) 3) "
            "    (= (ref analyze 'parallel-batched-component-count) 2) "
            "    (= (ref analyze 'parallel-batch-count) 2) "
            "    (= (ref analyze 'parallel-max-batch-size) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-stats (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-stats)) "
            "  (define stats (deduce/stats pb-reach-a)) "
            "  (define analyze-topology (ref analyze 'parallel-batch-topology)) "
            "  (define stats-topology (ref stats 'parallel-batch-topology)) "
            "  (and "
            "    (= (ref stats 'parallel-runtime-mode) 'metadata-only) "
            "    (= (ref stats 'parallel-recursive-component-count) 3) "
            "    (= (ref stats 'parallel-batched-component-count) 2) "
            "    (= (ref stats 'parallel-batch-count) 2) "
            "    (= (ref stats 'parallel-max-batch-size) 2) "
            "    (= (length analyze-topology) 2) "
            "    (= (length stats-topology) 2) "
            "    (= (ref (car analyze-topology) 'stratum) 0) "
            "    (= (ref (car analyze-topology) 'wave) 1) "
            "    (= (ref (car analyze-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr analyze-topology)) 'wave) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'batch-size) 1) "
            "    (= (ref (car stats-topology) 'stratum) 0) "
            "    (= (ref (car stats-topology) 'wave) 1) "
            "    (= (ref (car stats-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr stats-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr stats-topology)) 'wave) 2) "
            "    (= (ref (car (cdr stats-topology)) 'batch-size) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-selected] gd-edge-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-reach-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-src-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?y) '(gd-edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?z) '(gd-edge-s ?x ?y) '(gd-reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-other-out-s ?x ?y) '(gd-other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-selected) "
            "  (deduce/fact! gd-edge-s 1 2) "
            "  (deduce/fact! gd-edge-s 2 3) "
            "  (deduce/fact! gd-other-src-s 9 10) "
            "  (define selected (deduce/analyze db-admin-goal-directed-selected 1)) "
            "  (define selected-components (ref selected 'goal-directed-selected-components)) "
            "  (define selected-predicates (ref selected 'goal-directed-selected-predicates)) "
            "  (define other-before (deduce/count gd-other-out-s)) "
            "  (define full (deduce/analyze db-admin-goal-directed-selected)) "
            "  (define other-after (deduce/count gd-other-out-s)) "
            "  (and "
            "    (= (ref selected 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref selected 'goal-directed-selector-rule-index) 1) "
            "    (= (ref selected 'goal-directed-selected-component-id) 1) "
            "    (= (length selected-components) 2) "
            "    (= (car selected-components) 0) "
            "    (= (car (cdr selected-components)) 1) "
            "    (= (car selected-predicates) 'gd-reach-s) "
            "    (= (car (cdr selected-predicates)) 'gd-edge-s) "
            "    (= (length selected-predicates) 2) "
            "    (= (ref selected 'incremental-dirty-predicate-count) 4) "
            "    (= (deduce/count gd-reach-s) 3) "
            "    (= other-before 0) "
            "    (= (ref full 'goal-directed-execution-path) 'full-db-fixpoint) "
            "    (= (ref full 'goal-directed-selected-components) nil) "
            "    (= (ref full 'goal-directed-selected-predicates) nil) "
            "    (= (ref full 'incremental-dirty-predicate-count) 2) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-dirty-frontier-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-dirty-frontier-selected] edge-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] reach-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-src-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?y) '(edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?z) '(edge-s ?x ?y) '(reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(other-out-s ?x ?y) '(other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-dirty-frontier-selected) "
            "  (deduce/fact! edge-s 1 2) "
            "  (deduce/fact! edge-s 2 3) "
            "  (deduce/fact! other-src-s 9 10) "
            "  (deduce/analyze db-admin-dirty-frontier-selected 1) "
            "  (define reach-stats (deduce/stats reach-s)) "
            "  (define other-stats (deduce/stats other-out-s)) "
            "  (and "
            "    (= (ref reach-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref reach-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref reach-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref reach-stats 'dirty-self) false) "
            "    (= (ref other-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref other-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref other-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-explain-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-explain-classification] gd-edge-ex (src) (dst)) "
            "  (define [relation db-admin-explain-classification] gd-reach-ex (src) (dst)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?y) '(gd-edge-ex ?x ?y)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?z) '(gd-edge-ex ?x ?y) '(gd-reach-ex ?y ?z)) "
            "  (deduce/fact! gd-edge-ex 1 2) "
            "  (deduce/fact! gd-edge-ex 2 3) "
            "  (deduce/query gd-reach-ex (lambda (row) (= (ref row 'src) 1))) "
            "  (define explain (deduce/explain db-admin-explain-classification 1)) "
            "  (define steps (ref explain 'steps)) "
            "  (define step0 (car steps)) "
            "  (define step1 (car (cdr steps))) "
            "  (and "
            "    (= (ref explain 'surface-kind) 'planner-snapshot) "
            "    (= (ref explain 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref explain 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref explain 'last-goal-directed-read-surface) 'query) "
            "    (= (ref explain 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref explain 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref explain 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length steps) 2) "
            "    (= (ref (ref step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain keeps planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-analyze-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-analyze-classification] gd-edge-an (src) (dst)) "
            "  (define [relation db-admin-analyze-classification] gd-reach-an (src) (dst)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?y) '(gd-edge-an ?x ?y)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?z) '(gd-edge-an ?x ?y) '(gd-reach-an ?y ?z)) "
            "  (deduce/fact! gd-edge-an 1 2) "
            "  (deduce/fact! gd-edge-an 2 3) "
            "  (deduce/query gd-reach-an (lambda (row) (= (ref row 'src) 1))) "
            "  (define analyze (deduce/analyze db-admin-analyze-classification 1)) "
            "  (define rules (ref analyze 'rule-execution)) "
            "  (define rule0 (car rules)) "
            "  (define rule1 (car (cdr rules))) "
            "  (define rule0-step0 (car (ref rule0 'steps))) "
            "  (define rule1-step0 (car (ref rule1 'steps))) "
            "  (define rule1-step1 (car (cdr (ref rule1 'steps)))) "
            "  (and "
            "    (= (ref analyze 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref analyze 'goal-directed-selector-rule-index) 1) "
            "    (= (ref analyze 'goal-directed-selected-component-id) 1) "
            "    (= (ref analyze 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze 'last-goal-directed-read-relation) 'gd-reach-an) "
            "    (= (ref analyze 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (length rules) 2) "
            "    (= (ref rule0 'rule-index) 0) "
            "    (= (ref rule1 'rule-index) 1) "
            "    (= (ref rule0 'head-predicate) 'gd-reach-an) "
            "    (= (ref rule1 'head-predicate) 'gd-reach-an) "
            "    (= (length (ref rule0 'steps)) 1) "
            "    (= (length (ref rule1 'steps)) 2) "
            "    (= (ref (ref rule0-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-conjunctive-order (deduce 'open 'memory)) "
            "  (define [relation db-admin-conjunctive-order] ord-src (id) (k)) "
            "  (define [relation db-admin-conjunctive-order] ord-dim (k) (v)) "
            "  (define [relation db-admin-conjunctive-order] ord-out (id) (v)) "
            "  (deduce/rule! db-admin-conjunctive-order '(ord-out ?id ?v) '(ord-src ?id ?k) '(ord-dim ?k ?v)) "
            "  (deduce/fact! ord-src 1 10) "
            "  (deduce/fact! ord-src 2 20) "
            "  (deduce/fact! ord-dim 10 100) "
            "  (deduce/fact! ord-dim 20 200) "
            "  (deduce/fact! ord-dim 30 300) "
            "  (define explain (deduce/explain db-admin-conjunctive-order 0)) "
            "  (define analyze (deduce/analyze db-admin-conjunctive-order)) "
            "  (define rule0 (car (ref analyze 'rule-execution))) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (define analyze-steps (ref rule0 'steps)) "
            "  (define explain-step0 (car explain-steps)) "
            "  (define explain-step1 (car (cdr explain-steps))) "
            "  (define analyze-step0 (car analyze-steps)) "
            "  (define analyze-step1 (car (cdr analyze-steps))) "
            "  (and "
            "    (= (ref explain 'join-order) '(ord-src ord-dim)) "
            "    (= (ref rule0 'join-order) '(ord-src ord-dim)) "
            "    (= (ref explain-step0 'predicate) 'ord-src) "
            "    (= (ref explain-step1 'predicate) 'ord-dim) "
            "    (= (ref analyze-step0 'predicate) 'ord-src) "
            "    (= (ref analyze-step1 'predicate) 'ord-dim) "
            "    (= (ref explain-step0 'operator) 'full-scan) "
            "    (= (ref explain-step1 'operator) 'index-scan) "
            "    (= (ref analyze-step0 'operator) 'full-scan) "
            "    (= (ref analyze-step1 'operator) 'index-scan) "
            "    (= (ref explain-step0 'selected-index) nil) "
            "    (= (ref explain-step1 'selected-index) 0) "
            "    (= (ref analyze-step0 'selected-index) nil) "
            "    (= (ref analyze-step1 'selected-index) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-counter-truth (deduce 'open 'memory)) "
            "  (define [relation db-admin-counter-truth] ctr-edge (src) (dst)) "
            "  (define [relation db-admin-counter-truth] ctr-reach (src) (dst)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?y) '(ctr-edge ?x ?y)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?z) '(ctr-edge ?x ?y) '(ctr-reach ?y ?z)) "
            "  (deduce/fact! ctr-edge 1 2) "
            "  (deduce/fact! ctr-edge 2 5) "
            "  (deduce/fact! ctr-edge 3 4) "
            "  (deduce/fact! ctr-edge 4 5) "
            "  (deduce/fact! ctr-edge 9 10) "
            "  (deduce/fact! ctr-edge 10 5) "
            "  (deduce/match ctr-reach '(ctr-reach ?src 5) 1 'naive) "
            "  (define stats (deduce/stats ctr-reach)) "
            "  (define analyze (deduce/analyze db-admin-counter-truth 1)) "
            "  (define explain (deduce/explain db-admin-counter-truth 1)) "
            "  (define stats-steps (ref stats 'last-goal-directed-read-step-counters)) "
            "  (define analyze-rule1 (car (cdr (ref analyze 'rule-execution)))) "
            "  (define analyze-steps (ref analyze-rule1 'steps)) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (and "
            "    (= (length stats-steps) 2) "
            "    (= (ref (car stats-steps) 'counter-kind) 'observed) "
            "    (= (ref (car (cdr stats-steps)) 'counter-kind) 'observed) "
            "    (= (ref (car stats-steps) 'rows-read) 12) "
            "    (= (ref (car (cdr stats-steps)) 'rows-read) 16) "
            "    (= (length analyze-steps) 2) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'rows-read) 18) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'rows-read) 54) "
            "    (= (length explain-steps) 2) "
            "    (= (ref (ref (car explain-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car explain-steps) 'counters) 'rows-read) 6) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'rows-read) 54)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            }
            (*fail)++;
        }
    }

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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-edge-mn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-blocked-mn (node)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-reach-mn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?y) '(gd-edge-mn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?z) '(gd-edge-mn ?x ?y) '(gd-reach-mn ?y ?z) '(not (gd-blocked-mn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-reach-mn '(gd-reach-mn ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-negated-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-edge-n (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-blocked-n (node)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-reach-n (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?y) '(gd-edge-n ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?z) '(gd-edge-n ?x ?y) '(gd-reach-n ?y ?z) '(not (gd-blocked-n ?z))) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-negated-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query] gd-edge-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-reach-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-src-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-out-q (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?y) '(gd-edge-q ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?z) '(gd-edge-q ?x ?y) '(gd-reach-q ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-other-out-q ?x ?y) '(gd-other-src-q ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (deduce/fact! gd-edge-q 1 2) "
            "  (deduce/fact! gd-edge-q 2 3) "
            "  (deduce/fact! gd-other-src-q 9 10) "
            "  (define selected (deduce/query gd-reach-q (lambda (row) true) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-q)) "
            "  (define other-before (deduce/count gd-other-out-q)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (define other-after (deduce/count gd-other-out-q)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-edge-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-reach-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-src-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-out-qm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?y) '(gd-edge-qm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?z) '(gd-edge-qm ?x ?y) '(gd-reach-qm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-other-out-qm ?x ?y) '(gd-other-src-qm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-other-out-qm (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-qm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-edge-qa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-score-qa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?y) '(gd-score-qa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-score-qa (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-edge-qn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-blocked-qn (node)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-reach-qn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?y) '(gd-edge-qn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?z) '(gd-edge-qn ?x ?y) '(gd-reach-qn ?y ?z) '(not (gd-blocked-qn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-reach-qn (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-readers (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-readers] gd-edge-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-reach-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-src-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-out-r (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?y) '(gd-edge-r ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?z) '(gd-edge-r ?x ?y) '(gd-reach-r ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-other-out-r ?x ?y) '(gd-other-src-r ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (deduce/fact! gd-edge-r 1 2) "
            "  (deduce/fact! gd-edge-r 2 3) "
            "  (deduce/fact! gd-other-src-r 9 10) "
            "  (define count-selected (deduce/count gd-reach-r 1)) "
            "  (define scan-selected (deduce/scan gd-reach-r 1)) "
            "  (define range-selected (deduce/scan-range gd-reach-r '(1 2) '(2 3) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-r)) "
            "  (define other-before (deduce/count gd-other-out-r)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (define other-after (deduce/count gd-other-out-r)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= (length scan-selected) 3) "
            "    (= (length range-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-edge-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-reach-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-src-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-out-cm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?y) '(gd-edge-cm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?z) '(gd-edge-cm ?x ?y) '(gd-reach-cm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-other-out-cm ?x ?y) '(gd-other-src-cm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/count gd-other-out-cm 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/count-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-cm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-edge-sa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-score-sa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?y) '(gd-score-sa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan gd-score-sa 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-edge-srn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-blocked-srn (node)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-reach-srn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?y) '(gd-edge-srn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?z) '(gd-edge-srn ?x ?y) '(gd-reach-srn ?y ?z) '(not (gd-blocked-srn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan-range gd-reach-srn '(1 2) '(9 9) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-range-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan-range rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-edge-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-reach-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-src-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-out-qd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?y) '(gd-edge-qd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?z) '(gd-edge-qd ?x ?y) '(gd-reach-qd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-other-out-qd ?x ?y) '(gd-other-src-qd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand) "
            "  (deduce/fact! gd-edge-qd 1 2) "
            "  (deduce/fact! gd-edge-qd 2 3) "
            "  (deduce/fact! gd-edge-qd 10 11) "
            "  (deduce/fact! gd-other-src-qd 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qd "
            "      (lambda (row) (= (ref row 'dst) 3)) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            }
            (*fail)++;
        }
    }

    run_deduce_admin_surface_demand_tests(interp, pass, fail);
}
