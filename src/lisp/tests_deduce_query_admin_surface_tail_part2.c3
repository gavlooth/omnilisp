module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_tests_part2(Interp* interp, int* pass, int* fail) {
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
}
