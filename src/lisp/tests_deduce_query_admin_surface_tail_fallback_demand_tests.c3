module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_fallback_demand_tests(Interp* interp, int* pass, int* fail) {
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
}
