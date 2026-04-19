module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_recursive_tests_part3(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-pair-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-plain] gd-edge-qdspp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-plain] gd-sym-qdspp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-plain '(gd-sym-qdspp ?x ?y) '(gd-edge-qdspp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-plain '(gd-sym-qdspp ?x ?y) '(gd-sym-qdspp ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-plain) "
            "  (deduce/fact! gd-edge-qdspp 1 2) "
            "  (deduce/fact! gd-edge-qdspp 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdspp "
            "      (lambda (row) (and (= (ref row 'src) 2) (= (ref row 'dst) 1))))) "
            "  (define sym-stats (deduce/stats gd-sym-qdspp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 2) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on jointly-supported recursive pair permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on jointly-supported recursive pair permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on jointly-supported recursive pair permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-selector] gd-edge-qdsds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-selector] gd-sym-qdsds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-selector '(gd-sym-qdsds ?x ?y) '(gd-edge-qdsds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-selector '(gd-sym-qdsds ?x ?y) '(gd-sym-qdsds ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdsds 1 2) "
            "  (deduce/fact! gd-edge-qdsds 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 2) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 11) (= (ref row 'dst) 10)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define sym-stats (deduce/stats gd-sym-qdsds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 11) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-plain] gd-edge-qdsdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-plain] gd-sym-qdsdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-plain '(gd-sym-qdsdp ?x ?y) '(gd-edge-qdsdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-plain '(gd-sym-qdsdp ?x ?y) '(gd-sym-qdsdp ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdsdp 1 2) "
            "  (deduce/fact! gd-edge-qdsdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 2) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 11) (= (ref row 'dst) 10)))))) "
            "  (define sym-stats (deduce/stats gd-sym-qdsdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 11) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-multi-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-selector] gd-edge-qdms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-selector] gd-path-qdms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-selector '(gd-path-qdms ?x ?y) '(gd-edge-qdms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-selector '(gd-path-qdms ?x ?z) '(gd-path-qdms ?x ?y) '(gd-path-qdms ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-multi-selector) "
            "  (deduce/fact! gd-edge-qdms 1 2) "
            "  (deduce/fact! gd-edge-qdms 2 3) "
            "  (deduce/fact! gd-edge-qdms 3 4) "
            "  (define selected "
            "    (deduce/query gd-path-qdms "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 4))) "
            "      1 "
            "      'semi-naive)) "
            "  (define path-stats (deduce/stats gd-path-qdms)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-multi-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-multi-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-plain] gd-edge-qdmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-plain] gd-path-qdmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-plain '(gd-path-qdmp ?x ?y) '(gd-edge-qdmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-plain '(gd-path-qdmp ?x ?z) '(gd-path-qdmp ?x ?y) '(gd-path-qdmp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-multi-plain) "
            "  (deduce/fact! gd-edge-qdmp 1 2) "
            "  (deduce/fact! gd-edge-qdmp 2 3) "
            "  (deduce/fact! gd-edge-qdmp 3 4) "
            "  (define selected "
            "    (deduce/query gd-path-qdmp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 4))))) "
            "  (define path-stats (deduce/stats gd-path-qdmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-multi-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-edge-qdmscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-a-qdmscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-b-qdmscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-a-qdmscc ?x ?y) '(gd-edge-qdmscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-a-qdmscc ?x ?y) '(gd-b-qdmscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-b-qdmscc ?x ?y) '(gd-a-qdmscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-selector) "
            "  (deduce/fact! gd-edge-qdmscc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmscc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }
}
