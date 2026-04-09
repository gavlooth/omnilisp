module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_recursive_tests(Interp* interp, int* pass, int* fail) {
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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped] gd-edge-qddpps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped] gd-reach-qddpps (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped '(gd-reach-qddpps ?y ?x) '(gd-edge-qddpps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped '(gd-reach-qddpps ?z ?x) '(gd-edge-qddpps ?x ?y) '(gd-reach-qddpps ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped) "
            "  (deduce/fact! gd-edge-qddpps 1 2) "
            "  (deduce/fact! gd-edge-qddpps 2 3) "
            "  (deduce/fact! gd-edge-qddpps 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpps "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddpps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-pair-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-selector] gd-edge-qdsps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-selector] gd-sym-qdsps (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-selector '(gd-sym-qdsps ?x ?y) '(gd-edge-qdsps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-selector '(gd-sym-qdsps ?x ?y) '(gd-sym-qdsps ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-selector) "
            "  (deduce/fact! gd-edge-qdsps 1 2) "
            "  (deduce/fact! gd-edge-qdsps 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsps "
            "      (lambda (row) (and (= (ref row 'src) 2) (= (ref row 'dst) 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define sym-stats (deduce/stats gd-sym-qdsps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 2) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-pair-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-selector] gd-edge-qdstps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-selector] gd-stable-qdstps (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-selector '(gd-stable-qdstps ?x ?y) '(gd-edge-qdstps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-selector '(gd-stable-qdstps ?x ?y) '(gd-stable-qdstps ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-selector) "
            "  (deduce/fact! gd-edge-qdstps 1 2) "
            "  (deduce/fact! gd-edge-qdstps 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstps "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define stable-stats (deduce/stats gd-stable-qdstps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-pair-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-plain] gd-edge-qdstpp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-plain] gd-stable-qdstpp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-plain '(gd-stable-qdstpp ?x ?y) '(gd-edge-qdstpp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-plain '(gd-stable-qdstpp ?x ?y) '(gd-stable-qdstpp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-plain) "
            "  (deduce/fact! gd-edge-qdstpp 1 2) "
            "  (deduce/fact! gd-edge-qdstpp 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstpp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define stable-stats (deduce/stats gd-stable-qdstpp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query preserves jointly-supported same-index recursive pair demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query preserves jointly-supported same-index recursive pair demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query preserves jointly-supported same-index recursive pair demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-selector] gd-edge-qdstds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-selector] gd-stable-qdstds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-selector '(gd-stable-qdstds ?x ?y) '(gd-edge-qdstds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-selector '(gd-stable-qdstds ?x ?y) '(gd-stable-qdstds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdstds 1 2) "
            "  (deduce/fact! gd-edge-qdstds 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define stable-stats (deduce/stats gd-stable-qdstds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-plain] gd-edge-qdstdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-plain] gd-stable-qdstdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-plain '(gd-stable-qdstdp ?x ?y) '(gd-edge-qdstdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-plain '(gd-stable-qdstdp ?x ?y) '(gd-stable-qdstdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdstdp 1 2) "
            "  (deduce/fact! gd-edge-qdstdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define stable-stats (deduce/stats gd-stable-qdstdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query preserves jointly-supported same-index recursive disjunctive demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query preserves jointly-supported same-index recursive disjunctive demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query preserves jointly-supported same-index recursive disjunctive demands");
            }
            (*fail)++;
        }
    }

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
