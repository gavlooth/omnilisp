module lisp;

import std::io;

fn void run_deduce_admin_surface_query_demand_tests_part2(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pow (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-edge-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-reach-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-src-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-out-qdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?y) '(gd-edge-qdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?z) '(gd-edge-qdp ?x ?y) '(gd-reach-qdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-other-out-qdp ?x ?y) '(gd-other-src-qdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pow) "
            "  (deduce/fact! gd-edge-qdp 1 2) "
            "  (deduce/fact! gd-edge-qdp 2 3) "
            "  (deduce/fact! gd-edge-qdp 3 4) "
            "  (deduce/fact! gd-other-src-qdp 9 10) "
            "  (define selected "
            "    (let (base 2 exponent 2) "
            "      (deduce/query gd-reach-qdp "
            "        (lambda (row) "
            "          (= (ref row 'dst) (pow base exponent))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pow)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-atan2 (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-edge-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-reach-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-src-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-out-qdat (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?y) '(gd-edge-qdat ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?z) '(gd-edge-qdat ?x ?y) '(gd-reach-qdat ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-other-out-qdat ?x ?y) '(gd-other-src-qdat ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-atan2) "
            "  (deduce/fact! gd-edge-qdat 1 2) "
            "  (deduce/fact! gd-edge-qdat 2 3) "
            "  (deduce/fact! gd-edge-qdat 3 0) "
            "  (deduce/fact! gd-other-src-qdat 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdat "
            "      (lambda (row) "
            "        (= (ref row 'dst) (atan2 0 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdat)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-atan2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 0) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdat) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed atan2 wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-edge-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-reach-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-src-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-out-qdics (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?y) '(gd-edge-qdics ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?z) '(gd-edge-qdics ?x ?y) '(gd-reach-qdics ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-other-out-qdics ?x ?y) '(gd-other-src-qdics ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector) "
            "  (deduce/fact! gd-edge-qdics 1 2) "
            "  (deduce/fact! gd-edge-qdics 2 3) "
            "  (deduce/fact! gd-edge-qdics 10 11) "
            "  (deduce/fact! gd-other-src-qdics 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdics "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdics)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdics) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-edge-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-reach-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-src-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-out-qdicp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?y) '(gd-edge-qdicp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?z) '(gd-edge-qdicp ?x ?y) '(gd-reach-qdicp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-other-out-qdicp ?x ?y) '(gd-other-src-qdicp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain) "
            "  (deduce/fact! gd-edge-qdicp 1 2) "
            "  (deduce/fact! gd-edge-qdicp 2 3) "
            "  (deduce/fact! gd-edge-qdicp 10 11) "
            "  (deduce/fact! gd-other-src-qdicp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdicp "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdicp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdicp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-edge-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-reach-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-src-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-out-qdos (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?y) '(gd-edge-qdos ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?z) '(gd-edge-qdos ?x ?y) '(gd-reach-qdos ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-other-out-qdos ?x ?y) '(gd-other-src-qdos ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-selector) "
            "  (deduce/fact! gd-edge-qdos 1 2) "
            "  (deduce/fact! gd-edge-qdos 2 3) "
            "  (deduce/fact! gd-edge-qdos 10 11) "
            "  (deduce/fact! gd-other-src-qdos 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdos "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdos)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdos) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }
}
