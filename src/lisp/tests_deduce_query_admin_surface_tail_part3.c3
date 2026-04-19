module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_tests_part3(Interp* interp, int* pass, int* fail) {
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

}
