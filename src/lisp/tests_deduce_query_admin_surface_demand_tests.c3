module lisp;

import std::io;

fn bool deduce_query_test_eval_true(Interp* interp, EvalResult r) @inline {
    return !r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true;
}

fn void deduce_query_test_expect_true(
    Interp* interp,
    EvalResult r,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    if (deduce_query_test_eval_true(interp, r)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    if (r.error.has_error) {
        io::printfn("[FAIL] %s (error: %s)", (ZString)fail_msg, (ZString)r.error.message[:256]);
    } else {
        io::printn((ZString)fail_msg);
    }
    (*fail)++;
}

fn void run_deduce_admin_surface_demand_tests(Interp* interp, int* pass, int* fail) {
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
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcw) "
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
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcwp) "
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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-edge-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-reach-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-other-src-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-other-out-qdcwla (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-reach-qdcwla ?x ?y) '(gd-edge-qdcwla ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-reach-qdcwla ?x ?z) '(gd-edge-qdcwla ?x ?y) '(gd-reach-qdcwla ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-other-out-qdcwla ?x ?y) '(gd-other-src-qdcwla ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args) "
            "  (deduce/fact! gd-edge-qdcwla 1 2) "
            "  (deduce/fact! gd-edge-qdcwla 2 3) "
            "  (deduce/fact! gd-edge-qdcwla 10 11) "
            "  (deduce/fact! gd-other-src-qdcwla 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwla "
            "          (lambda (row) (want row target)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwla)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-edge-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-reach-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-other-src-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-other-out-qdcwlap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-reach-qdcwlap ?x ?y) '(gd-edge-qdcwlap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-reach-qdcwlap ?x ?z) '(gd-edge-qdcwlap ?x ?y) '(gd-reach-qdcwlap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-other-out-qdcwlap ?x ?y) '(gd-other-src-qdcwlap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain) "
            "  (deduce/fact! gd-edge-qdcwlap 1 2) "
            "  (deduce/fact! gd-edge-qdcwlap 2 3) "
            "  (deduce/fact! gd-edge-qdcwlap 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlap 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlap "
            "          (lambda (row) (want row target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain)) "
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
            emit_pass_literal("[PASS] plain deduce query can unwrap captured wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap captured wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap captured wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-scalar-wrapper (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-edge-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-reach-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-other-src-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-other-out-qdcsw (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-reach-qdcsw ?x ?y) '(gd-edge-qdcsw ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-reach-qdcsw ?x ?z) '(gd-edge-qdcsw ?x ?y) '(gd-reach-qdcsw ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-other-out-qdcsw ?x ?y) '(gd-other-src-qdcsw ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper) "
            "  (deduce/fact! gd-edge-qdcsw 1 2) "
            "  (deduce/fact! gd-edge-qdcsw 2 3) "
            "  (deduce/fact! gd-edge-qdcsw 10 11) "
            "  (deduce/fact! gd-other-src-qdcsw 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "        (let (want (lambda (expected actual) (= actual expected))) "
            "          (deduce/query gd-reach-qdcsw "
            "            (lambda (row) (want target (pickdst row))) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcsw)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-edge-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-reach-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-other-src-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-other-out-qdcswcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-reach-qdcswcp ?x ?y) '(gd-edge-qdcswcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-reach-qdcswcp ?x ?z) '(gd-edge-qdcswcp ?x ?y) '(gd-reach-qdcswcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-other-out-qdcswcp ?x ?y) '(gd-other-src-qdcswcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain) "
            "  (deduce/fact! gd-edge-qdcswcp 1 2) "
            "  (deduce/fact! gd-edge-qdcswcp 2 3) "
            "  (deduce/fact! gd-edge-qdcswcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcswcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "        (let (want0 (lambda (actual expected) (= actual expected))) "
            "          (let (want1 (lambda (expected actual) (want0 actual expected))) "
            "            (deduce/query gd-reach-qdcswcp "
            "              (lambda (row) (want1 target (pickdst row))))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcswcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain)) "
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
            emit_pass_literal("[PASS] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-edge-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-reach-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-other-src-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-other-out-qdds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-reach-qdds ?x ?y) '(gd-edge-qdds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-reach-qdds ?x ?z) '(gd-edge-qdds ?x ?y) '(gd-reach-qdds ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-other-out-qdds ?x ?y) '(gd-other-src-qdds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdds 1 2) "
            "  (deduce/fact! gd-edge-qdds 2 3) "
            "  (deduce/fact! gd-edge-qdds 10 11) "
            "  (deduce/fact! gd-other-src-qdds 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdds "
            "      (lambda (row) "
            "        (or "
            "          (= (ref row 'dst) 2) "
            "          (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 8) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-edge-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-reach-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-other-src-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-other-out-qddp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-reach-qddp ?x ?y) '(gd-edge-qddp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-reach-qddp ?x ?z) '(gd-edge-qddp ?x ?y) '(gd-reach-qddp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-other-out-qddp ?x ?y) '(gd-other-src-qddp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-plain) "
            "  (deduce/fact! gd-edge-qddp 1 2) "
            "  (deduce/fact! gd-edge-qddp 2 3) "
            "  (deduce/fact! gd-edge-qddp 10 11) "
            "  (deduce/fact! gd-other-src-qddp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddp "
            "      (lambda (row) "
            "        (or "
            "          (= (ref row 'dst) 3) "
            "          (= (ref row 'dst) 11))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 17) "
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
            emit_pass_literal("[PASS] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-wrapper-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-edge-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-reach-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-other-src-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-other-out-qddws (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-reach-qddws ?x ?y) '(gd-edge-qddws ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-reach-qddws ?x ?z) '(gd-edge-qddws ?x ?y) '(gd-reach-qddws ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-other-out-qddws ?x ?y) '(gd-other-src-qddws ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-selector) "
            "  (deduce/fact! gd-edge-qddws 1 2) "
            "  (deduce/fact! gd-edge-qddws 2 3) "
            "  (deduce/fact! gd-edge-qddws 10 11) "
            "  (deduce/fact! gd-other-src-qddws 9 10) "
            "  (define selected "
            "    (let (want2 (lambda (candidate) (= (ref candidate 'dst) 2)) "
            "          want3 (lambda (candidate) (= (ref candidate 'dst) 3))) "
            "      (deduce/query gd-reach-qddws "
            "        (lambda (row) (or (want2 row) (want3 row))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qddws)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-selector)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers");
            }
            (*fail)++;
        }
    }

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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-selector-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-fallback] gd-edge-qdpsf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-fallback] gd-reach-qdpsf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-fallback '(gd-reach-qdpsf ?x ?y) '(gd-edge-qdpsf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-fallback '(gd-reach-qdpsf ?x ?z) '(gd-edge-qdpsf ?x ?y) '(gd-reach-qdpsf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-fallback) "
            "  (deduce/fact! gd-edge-qdpsf 1 2) "
            "  (deduce/fact! gd-edge-qdpsf 2 3) "
            "  (deduce/fact! gd-edge-qdpsf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpsf "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpsf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-fallback)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

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

    run_deduce_admin_surface_demand_tail_tests(interp, pass, fail);
}
