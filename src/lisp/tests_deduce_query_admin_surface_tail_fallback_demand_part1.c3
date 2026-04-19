module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_fallback_demand_tests_part1(Interp* interp, int* pass, int* fail) {
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
}
