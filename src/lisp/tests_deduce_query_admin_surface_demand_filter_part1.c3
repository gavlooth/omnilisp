module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_filter_part1_tests(Interp* interp, int* pass, int* fail) {
    deduce_query_test_expect_true(
        interp,
        run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-edge-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-reach-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-other-src-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-other-out-qdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-reach-qdp ?x ?y) '(gd-edge-qdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-reach-qdp ?x ?z) '(gd-edge-qdp ?x ?y) '(gd-reach-qdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-other-out-qdp ?x ?y) '(gd-other-src-qdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-plain) "
            "  (deduce/fact! gd-edge-qdp 1 2) "
            "  (deduce/fact! gd-edge-qdp 2 3) "
            "  (deduce/fact! gd-edge-qdp 10 11) "
            "  (deduce/fact! gd-other-src-qdp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdp "
            "      (lambda (row) (= (ref row 'dst) 3)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        ),
        "[PASS] plain deduce query can use ephemeral demand execution for simple equality filters",
        "[FAIL] plain deduce query can use ephemeral demand execution for simple equality filters",
        pass,
        fail
    );

    deduce_query_test_expect_true(
        interp,
        run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-edge-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-reach-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-other-src-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-other-out-qdc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-reach-qdc ?x ?y) '(gd-edge-qdc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-reach-qdc ?x ?z) '(gd-edge-qdc ?x ?y) '(gd-reach-qdc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-other-out-qdc ?x ?y) '(gd-other-src-qdc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured) "
            "  (deduce/fact! gd-edge-qdc 1 2) "
            "  (deduce/fact! gd-edge-qdc 2 3) "
            "  (deduce/fact! gd-edge-qdc 10 11) "
            "  (deduce/fact! gd-other-src-qdc 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdc "
            "        (lambda (row) (= (ref row 'dst) target)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdc) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        ),
        "[PASS] selector-scoped deduce query can use captured constant equality filters in the demand path",
        "[FAIL] selector-scoped deduce query can use captured constant equality filters in the demand path",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-edge-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-reach-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-other-src-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-other-out-qdcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-reach-qdcp ?x ?y) '(gd-edge-qdcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-reach-qdcp ?x ?z) '(gd-edge-qdcp ?x ?y) '(gd-reach-qdcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-other-out-qdcp ?x ?y) '(gd-other-src-qdcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-plain) "
            "  (deduce/fact! gd-edge-qdcp 1 2) "
            "  (deduce/fact! gd-edge-qdcp 2 3) "
            "  (deduce/fact! gd-edge-qdcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdcp "
            "        (lambda (row) (= (ref row 'dst) target))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use captured constant equality filters in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use captured constant equality filters in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use captured constant equality filters in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-edge-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-reach-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-other-src-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-other-out-qdcw (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-reach-qdcw ?x ?y) '(gd-edge-qdcw ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-reach-qdcw ?x ?z) '(gd-edge-qdcw ?x ?y) '(gd-reach-qdcw ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-other-out-qdcw ?x ?y) '(gd-other-src-qdcw ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper) "
            "  (deduce/fact! gd-edge-qdcw 1 2) "
            "  (deduce/fact! gd-edge-qdcw 2 3) "
            "  (deduce/fact! gd-edge-qdcw 10 11) "
            "  (deduce/fact! gd-other-src-qdcw 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (deduce/query gd-reach-qdcw "
            "          (lambda (row) (want row)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcw)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-edge-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-reach-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-other-src-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-other-out-qdcwp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-reach-qdcwp ?x ?y) '(gd-edge-qdcwp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-reach-qdcwp ?x ?z) '(gd-edge-qdcwp ?x ?y) '(gd-reach-qdcwp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-other-out-qdcwp ?x ?y) '(gd-other-src-qdcwp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-plain) "
            "  (deduce/fact! gd-edge-qdcwp 1 2) "
            "  (deduce/fact! gd-edge-qdcwp 2 3) "
            "  (deduce/fact! gd-edge-qdcwp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (deduce/query gd-reach-qdcwp "
            "          (lambda (row) (want row)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured closure wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured closure wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured closure wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-row-middle (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-edge-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-reach-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-other-src-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-other-out-qdcwrm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-reach-qdcwrm ?x ?y) '(gd-edge-qdcwrm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-reach-qdcwrm ?x ?z) '(gd-edge-qdcwrm ?x ?y) '(gd-reach-qdcwrm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-other-out-qdcwrm ?x ?y) '(gd-other-src-qdcwrm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle) "
            "  (deduce/fact! gd-edge-qdcwrm 1 2) "
            "  (deduce/fact! gd-edge-qdcwrm 2 3) "
            "  (deduce/fact! gd-edge-qdcwrm 10 11) "
            "  (deduce/fact! gd-other-src-qdcwrm 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (delta 0) "
            "        (let (want (lambda (expected candidate shift) (= (ref candidate 'dst) (+ expected shift)))) "
            "          (deduce/query gd-reach-qdcwrm "
            "            (lambda (row) (want target row delta)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwrm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-edge-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-reach-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-other-src-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-other-out-qdcwrmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-reach-qdcwrmp ?x ?y) '(gd-edge-qdcwrmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-reach-qdcwrmp ?x ?z) '(gd-edge-qdcwrmp ?x ?y) '(gd-reach-qdcwrmp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-other-out-qdcwrmp ?x ?y) '(gd-other-src-qdcwrmp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain) "
            "  (deduce/fact! gd-edge-qdcwrmp 1 2) "
            "  (deduce/fact! gd-edge-qdcwrmp 2 3) "
            "  (deduce/fact! gd-edge-qdcwrmp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwrmp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (delta 0) "
            "        (let (want (lambda (expected candidate shift) (= (ref candidate 'dst) (+ expected shift)))) "
            "          (deduce/query gd-reach-qdcwrmp "
            "            (lambda (row) (want target row delta))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwrmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }
}
