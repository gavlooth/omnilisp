module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_recursive_tests_part1(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-plain-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-fallback] gd-edge-qdppf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-fallback] gd-reach-qdppf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-fallback '(gd-reach-qdppf ?x ?y) '(gd-edge-qdppf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-fallback '(gd-reach-qdppf ?x ?z) '(gd-edge-qdppf ?x ?y) '(gd-reach-qdppf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-fallback) "
            "  (deduce/fact! gd-edge-qdppf 1 2) "
            "  (deduce/fact! gd-edge-qdppf 2 3) "
            "  (deduce/fact! gd-edge-qdppf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdppf "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdppf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-fallback)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback] gd-edge-qddpsf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback] gd-reach-qddpsf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback '(gd-reach-qddpsf ?x ?y) '(gd-edge-qddpsf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback '(gd-reach-qddpsf ?x ?z) '(gd-edge-qddpsf ?x ?y) '(gd-reach-qddpsf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback) "
            "  (deduce/fact! gd-edge-qddpsf 1 2) "
            "  (deduce/fact! gd-edge-qddpsf 2 3) "
            "  (deduce/fact! gd-edge-qddpsf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpsf "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddpsf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback] gd-edge-qddppf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback] gd-reach-qddppf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback '(gd-reach-qddppf ?x ?y) '(gd-edge-qddppf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback '(gd-reach-qddppf ?x ?z) '(gd-edge-qddppf ?x ?y) '(gd-reach-qddppf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback) "
            "  (deduce/fact! gd-edge-qddppf 1 2) "
            "  (deduce/fact! gd-edge-qddppf 2 3) "
            "  (deduce/fact! gd-edge-qddppf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddppf "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddppf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-selector-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-swapped] gd-edge-qdpss (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-swapped] gd-reach-qdpss (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-swapped '(gd-reach-qdpss ?y ?x) '(gd-edge-qdpss ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-swapped '(gd-reach-qdpss ?z ?x) '(gd-edge-qdpss ?x ?y) '(gd-reach-qdpss ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-swapped) "
            "  (deduce/fact! gd-edge-qdpss 1 2) "
            "  (deduce/fact! gd-edge-qdpss 2 3) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpss "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpss)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-swapped)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-plain-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-swapped] gd-edge-qdpps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-swapped] gd-reach-qdpps (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-swapped '(gd-reach-qdpps ?y ?x) '(gd-edge-qdpps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-swapped '(gd-reach-qdpps ?z ?x) '(gd-edge-qdpps ?x ?y) '(gd-reach-qdpps ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-swapped) "
            "  (deduce/fact! gd-edge-qdpps 1 2) "
            "  (deduce/fact! gd-edge-qdpps 2 3) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpps "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdpps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-swapped)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps swapped recursive pair demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps swapped recursive pair demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps swapped recursive pair demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped] gd-edge-qddpss (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped] gd-reach-qddpss (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped '(gd-reach-qddpss ?y ?x) '(gd-edge-qddpss ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped '(gd-reach-qddpss ?z ?x) '(gd-edge-qddpss ?x ?y) '(gd-reach-qddpss ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped) "
            "  (deduce/fact! gd-edge-qddpss 1 2) "
            "  (deduce/fact! gd-edge-qddpss 2 3) "
            "  (deduce/fact! gd-edge-qddpss 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpss "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddpss)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }
}
