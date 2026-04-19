module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_filter_part3_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-wrapper-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-edge-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-reach-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-other-src-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-other-out-qddwp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-reach-qddwp ?x ?y) '(gd-edge-qddwp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-reach-qddwp ?x ?z) '(gd-edge-qddwp ?x ?y) '(gd-reach-qddwp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-other-out-qddwp ?x ?y) '(gd-other-src-qddwp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-plain) "
            "  (deduce/fact! gd-edge-qddwp 1 2) "
            "  (deduce/fact! gd-edge-qddwp 2 3) "
            "  (deduce/fact! gd-edge-qddwp 10 11) "
            "  (deduce/fact! gd-other-src-qddwp 9 10) "
            "  (define selected "
            "    (let (pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want2 (lambda (actual) (= actual 2)) "
            "          want3 (lambda (actual) (= actual 3))) "
            "      (deduce/query gd-reach-qddwp "
            "        (lambda (row) (or (want2 (pickdst row)) (want3 (pickdst row))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddwp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-plain)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union same-position disjunctive branches with row-column wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union same-position disjunctive branches with row-column wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union same-position disjunctive branches with row-column wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-residual-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-edge-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-reach-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-other-src-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-other-out-qddrs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-reach-qddrs ?x ?y) '(gd-edge-qddrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-reach-qddrs ?x ?z) '(gd-edge-qddrs ?x ?y) '(gd-reach-qddrs ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-other-out-qddrs ?x ?y) '(gd-other-src-qddrs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-selector) "
            "  (deduce/fact! gd-edge-qddrs 1 2) "
            "  (deduce/fact! gd-edge-qddrs 2 3) "
            "  (deduce/fact! gd-edge-qddrs 10 11) "
            "  (deduce/fact! gd-other-src-qddrs 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddrs "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'dst) 2) (> (ref row 'src) 0)) "
            "          (and (= (ref row 'dst) 3) (> (ref row 'src) 1)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddrs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-residual-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-edge-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-reach-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-other-src-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-other-out-qddrp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-reach-qddrp ?x ?y) '(gd-edge-qddrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-reach-qddrp ?x ?z) '(gd-edge-qddrp ?x ?y) '(gd-reach-qddrp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-other-out-qddrp ?x ?y) '(gd-other-src-qddrp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-plain) "
            "  (deduce/fact! gd-edge-qddrp 1 2) "
            "  (deduce/fact! gd-edge-qddrp 2 3) "
            "  (deduce/fact! gd-edge-qddrp 10 11) "
            "  (deduce/fact! gd-other-src-qddrp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddrp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'dst) 3) (> (ref row 'src) 1)) "
            "          (and (= (ref row 'dst) 11) (> (ref row 'src) 5)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddrp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 12) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union same-position disjunctive branches with residual conjunct filtering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union same-position disjunctive branches with residual conjunct filtering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union same-position disjunctive branches with residual conjunct filtering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-mixed-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-edge-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-reach-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-other-src-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-other-out-qddms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-reach-qddms ?x ?y) '(gd-edge-qddms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-reach-qddms ?x ?z) '(gd-edge-qddms ?x ?y) '(gd-reach-qddms ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-other-out-qddms ?x ?y) '(gd-other-src-qddms ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-selector) "
            "  (deduce/fact! gd-edge-qddms 1 2) "
            "  (deduce/fact! gd-edge-qddms 2 3) "
            "  (deduce/fact! gd-edge-qddms 10 11) "
            "  (deduce/fact! gd-other-src-qddms 9 10) "
            "  (define selected "
            "    (let (src-target 1 "
            "          dst-target 11 "
            "          want-src (lambda (candidate expected) (= (ref candidate 'src) expected)) "
            "          pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want-dst (lambda (actual expected) (= actual expected))) "
            "      (deduce/query gd-reach-qddms "
            "        (lambda (row) "
            "          (or "
            "            (want-src row src-target) "
            "            (want-dst (pickdst row) dst-target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qddms)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-mixed-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-edge-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-reach-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-other-src-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-other-out-qddmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-reach-qddmp ?x ?y) '(gd-edge-qddmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-reach-qddmp ?x ?z) '(gd-edge-qddmp ?x ?y) '(gd-reach-qddmp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-other-out-qddmp ?x ?y) '(gd-other-src-qddmp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-plain) "
            "  (deduce/fact! gd-edge-qddmp 1 2) "
            "  (deduce/fact! gd-edge-qddmp 2 3) "
            "  (deduce/fact! gd-edge-qddmp 10 11) "
            "  (deduce/fact! gd-other-src-qddmp 9 10) "
            "  (define selected "
            "    (let (src-target 1 "
            "          dst-target 11 "
            "          want-src (lambda (candidate expected) (= (ref candidate 'src) expected)) "
            "          pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want-dst (lambda (actual expected) (= actual expected))) "
            "      (deduce/query gd-reach-qddmp "
            "        (lambda (row) "
            "          (or "
            "            (want-src row src-target) "
            "            (want-dst (pickdst row) dst-target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            }
            (*fail)++;
        }
    }
}
