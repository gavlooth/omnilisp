module lisp;

import std::io;

fn void run_deduce_admin_surface_query_demand_tests_part3(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-edge-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-reach-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-src-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-out-qdop (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?y) '(gd-edge-qdop ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?z) '(gd-edge-qdop ?x ?y) '(gd-reach-qdop ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-other-out-qdop ?x ?y) '(gd-other-src-qdop ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-plain) "
            "  (deduce/fact! gd-edge-qdop 1 2) "
            "  (deduce/fact! gd-edge-qdop 2 3) "
            "  (deduce/fact! gd-edge-qdop 10 11) "
            "  (deduce/fact! gd-other-src-qdop 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdop "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdop)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdop)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-edge-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-reach-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-src-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-out-qdls (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?y) '(gd-edge-qdls ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?z) '(gd-edge-qdls ?x ?y) '(gd-reach-qdls ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-other-out-qdls ?x ?y) '(gd-other-src-qdls ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-selector) "
            "  (deduce/fact! gd-edge-qdls 1 2) "
            "  (deduce/fact! gd-edge-qdls 2 3) "
            "  (deduce/fact! gd-edge-qdls 10 11) "
            "  (deduce/fact! gd-other-src-qdls 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdls "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdls)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdls) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-edge-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-reach-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-src-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-out-qdlp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?y) '(gd-edge-qdlp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?z) '(gd-edge-qdlp ?x ?y) '(gd-reach-qdlp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-other-out-qdlp ?x ?y) '(gd-other-src-qdlp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-plain) "
            "  (deduce/fact! gd-edge-qdlp 1 2) "
            "  (deduce/fact! gd-edge-qdlp 2 3) "
            "  (deduce/fact! gd-edge-qdlp 10 11) "
            "  (deduce/fact! gd-other-src-qdlp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdlp "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdlp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdlp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-edge-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-sym-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-src-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-out-pms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-edge-pms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-sym-pms ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-other-out-pms ?x ?y) '(gd-other-src-pms ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-swapped) "
            "  (deduce/fact! gd-edge-pms 1 2) "
            "  (deduce/fact! gd-edge-pms 10 11) "
            "  (deduce/fact! gd-other-src-pms 20 21) "
            "  (define selected (deduce/match gd-sym-pms '(gd-sym-pms 1 ?y))) "
            "  (define sym-stats (deduce/stats gd-sym-pms)) "
            "  (define other-stats (deduce/stats gd-other-out-pms)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'y) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-edge-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-reach-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-src-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-out-srdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?z) '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-other-out-srdp ?x ?y) '(gd-other-src-srdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-plain) "
            "  (deduce/fact! gd-edge-srdp 1 2) "
            "  (deduce/fact! gd-edge-srdp 2 3) "
            "  (deduce/fact! gd-edge-srdp 10 11) "
            "  (deduce/fact! gd-edge-srdp 11 12) "
            "  (deduce/fact! gd-other-src-srdp 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srdp '(1 0) '(1 999))) "
            "  (define reach-stats (deduce/stats gd-reach-srdp)) "
            "  (define other-stats (deduce/stats gd-other-out-srdp)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            }
            (*fail)++;
        }
    }
}
