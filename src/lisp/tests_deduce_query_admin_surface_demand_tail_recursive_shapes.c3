module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_tail_recursive_shape_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-edge-qdmpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-a-qdmpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-b-qdmpcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-a-qdmpcc ?x ?y) '(gd-edge-qdmpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-a-qdmpcc ?x ?y) '(gd-b-qdmpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-b-qdmpcc ?x ?y) '(gd-a-qdmpcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-plain) "
            "  (deduce/fact! gd-edge-qdmpcc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmpcc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmpcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-edge-qdmdsscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-a-qdmdsscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-b-qdmdsscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-a-qdmdsscc ?x ?y) '(gd-edge-qdmdsscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-a-qdmdsscc ?x ?y) '(gd-b-qdmdsscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-b-qdmdsscc ?x ?y) '(gd-a-qdmdsscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdmdsscc 1 2) "
            "  (deduce/fact! gd-edge-qdmdsscc 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmdsscc "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmdsscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-edge-qdmdspcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-a-qdmdspcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-b-qdmdspcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-a-qdmdspcc ?x ?y) '(gd-edge-qdmdspcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-a-qdmdspcc ?x ?y) '(gd-b-qdmdspcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-b-qdmdspcc ?x ?y) '(gd-a-qdmdspcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdmdspcc 1 2) "
            "  (deduce/fact! gd-edge-qdmdspcc 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmdspcc "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmdspcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-three-cycle-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-edge-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-a-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-b-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-c-qdtcscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-a-qdtcscc ?x ?y) '(gd-edge-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-a-qdtcscc ?x ?y) '(gd-b-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-b-qdtcscc ?x ?y) '(gd-c-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-c-qdtcscc ?x ?y) '(gd-a-qdtcscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-selector) "
            "  (deduce/fact! gd-edge-qdtcscc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdtcscc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtcscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-selector)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-three-cycle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-edge-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-a-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-b-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-c-qdtcpcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-a-qdtcpcc ?x ?y) '(gd-edge-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-a-qdtcpcc ?x ?y) '(gd-b-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-b-qdtcpcc ?x ?y) '(gd-c-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-c-qdtcpcc ?x ?y) '(gd-a-qdtcpcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-plain) "
            "  (deduce/fact! gd-edge-qdtcpcc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdtcpcc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtcpcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-edge-qdmrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-a-qdmrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-b-qdmrs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-a-qdmrs ?x ?y) '(gd-edge-qdmrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-a-qdmrs ?x ?y) '(gd-b-qdmrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-b-qdmrs ?y ?x) '(gd-a-qdmrs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-selector) "
            "  (deduce/fact! gd-edge-qdmrs 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrs "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmrs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-selector)) "
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
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-edge-qdmrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-a-qdmrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-b-qdmrp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-a-qdmrp ?x ?y) '(gd-edge-qdmrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-a-qdmrp ?x ?y) '(gd-b-qdmrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-b-qdmrp ?y ?x) '(gd-a-qdmrp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-plain) "
            "  (deduce/fact! gd-edge-qdmrp 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmrp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-edge-qdmrds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-a-qdmrds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-b-qdmrds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-a-qdmrds ?x ?y) '(gd-edge-qdmrds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-a-qdmrds ?x ?y) '(gd-b-qdmrds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-b-qdmrds ?y ?x) '(gd-a-qdmrds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdmrds 1 2) "
            "  (deduce/fact! gd-edge-qdmrds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmrds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-edge-qdmrdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-a-qdmrdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-b-qdmrdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-a-qdmrdp ?x ?y) '(gd-edge-qdmrdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-a-qdmrdp ?x ?y) '(gd-b-qdmrdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-b-qdmrdp ?y ?x) '(gd-a-qdmrdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdmrdp 1 2) "
            "  (deduce/fact! gd-edge-qdmrdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmrdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }
}
