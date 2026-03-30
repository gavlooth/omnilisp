module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_wrapper_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-edge-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-reach-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-other-src-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-other-out-qdcwc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-reach-qdcwc ?x ?y) '(gd-edge-qdcwc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-reach-qdcwc ?x ?z) '(gd-edge-qdcwc ?x ?y) '(gd-reach-qdcwc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-other-out-qdcwc ?x ?y) '(gd-other-src-qdcwc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain) "
            "  (deduce/fact! gd-edge-qdcwc 1 2) "
            "  (deduce/fact! gd-edge-qdcwc 2 3) "
            "  (deduce/fact! gd-edge-qdcwc 10 11) "
            "  (deduce/fact! gd-other-src-qdcwc 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (let (want1 (lambda (candidate) (want0 candidate))) "
            "          (deduce/query gd-reach-qdcwc "
            "            (lambda (row) (want1 row)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-edge-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-reach-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-other-src-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-other-out-qdcwcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-reach-qdcwcp ?x ?y) '(gd-edge-qdcwcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-reach-qdcwcp ?x ?z) '(gd-edge-qdcwcp ?x ?y) '(gd-reach-qdcwcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-other-out-qdcwcp ?x ?y) '(gd-other-src-qdcwcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain-plain) "
            "  (deduce/fact! gd-edge-qdcwcp 1 2) "
            "  (deduce/fact! gd-edge-qdcwcp 2 3) "
            "  (deduce/fact! gd-edge-qdcwcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (let (want1 (lambda (candidate) (want0 candidate))) "
            "          (deduce/query gd-reach-qdcwcp "
            "            (lambda (row) (want1 row))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain-plain)) "
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
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-residual (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-edge-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-reach-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-other-src-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-other-out-qdr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-reach-qdr ?x ?y) '(gd-edge-qdr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-reach-qdr ?x ?z) '(gd-edge-qdr ?x ?y) '(gd-reach-qdr ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-other-out-qdr ?x ?y) '(gd-other-src-qdr ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-residual) "
            "  (deduce/fact! gd-edge-qdr 1 2) "
            "  (deduce/fact! gd-edge-qdr 2 3) "
            "  (deduce/fact! gd-edge-qdr 10 11) "
            "  (deduce/fact! gd-other-src-qdr 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdr "
            "      (lambda (row) "
            "        (and (= (ref row 'dst) 3) (< (ref row 'src) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdr)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-residual)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdr) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected] gd-edge-qdpr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected] gd-reach-qdpr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected '(gd-reach-qdpr ?x ?y) '(gd-edge-qdpr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected '(gd-reach-qdpr ?x ?z) '(gd-edge-qdpr ?x ?y) '(gd-reach-qdpr ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-projected) "
            "  (deduce/fact! gd-edge-qdpr 1 2) "
            "  (deduce/fact! gd-edge-qdpr 2 3) "
            "  (deduce/fact! gd-edge-qdpr 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpr "
            "      (lambda (row) "
            "        (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpr)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdpr) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can project unsupported equality filters out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can project unsupported equality filters out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can project unsupported equality filters out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-projected-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected-plain] gd-edge-qdpp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected-plain] gd-reach-qdpp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected-plain '(gd-reach-qdpp ?x ?y) '(gd-edge-qdpp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected-plain '(gd-reach-qdpp ?x ?z) '(gd-edge-qdpp ?x ?y) '(gd-reach-qdpp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-projected-plain) "
            "  (deduce/fact! gd-edge-qdpp 1 2) "
            "  (deduce/fact! gd-edge-qdpp 2 3) "
            "  (deduce/fact! gd-edge-qdpp 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpp "
            "      (lambda (row) "
            "        (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdpp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-projected-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdpp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can project unsupported equality filters out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can project unsupported equality filters out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can project unsupported equality filters out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-builtin-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-edge-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-reach-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-other-src-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-other-out-qdfs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-reach-qdfs ?x ?y) '(gd-edge-qdfs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-reach-qdfs ?x ?z) '(gd-edge-qdfs ?x ?y) '(gd-reach-qdfs ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-other-out-qdfs ?x ?y) '(gd-other-src-qdfs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-builtin-selector) "
            "  (deduce/fact! gd-edge-qdfs 1 2) "
            "  (deduce/fact! gd-edge-qdfs 2 3) "
            "  (deduce/fact! gd-edge-qdfs 10 11) "
            "  (deduce/fact! gd-other-src-qdfs 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdfs "
            "        (lambda (row) (= (ref row 'dst) (+ target 0))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdfs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-builtin-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdfs) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed builtin numeric expressions in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed builtin numeric expressions in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed builtin numeric expressions in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-edge-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-reach-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-src-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-out-qdf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdf ?x ?y) '(gd-edge-qdf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdf ?x ?z) '(gd-edge-qdf ?x ?y) '(gd-reach-qdf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-other-out-qdf ?x ?y) '(gd-other-src-qdf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-fallback) "
            "  (deduce/fact! gd-edge-qdf 1 2) "
            "  (deduce/fact! gd-edge-qdf 2 3) "
            "  (deduce/fact! gd-edge-qdf 10 11) "
            "  (deduce/fact! gd-other-src-qdf 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdf "
            "        (lambda (row) (= (ref row 'dst) (+ target 0)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-fallback)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdf)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed builtin numeric expressions in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed builtin numeric expressions in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed builtin numeric expressions in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-edge-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-reach-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-other-src-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-other-out-qdis (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-reach-qdis ?x ?y) '(gd-edge-qdis ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-reach-qdis ?x ?z) '(gd-edge-qdis ?x ?y) '(gd-reach-qdis ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-other-out-qdis ?x ?y) '(gd-other-src-qdis ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-selector) "
            "  (deduce/fact! gd-edge-qdis 1 2) "
            "  (deduce/fact! gd-edge-qdis 2 3) "
            "  (deduce/fact! gd-edge-qdis 10 11) "
            "  (deduce/fact! gd-other-src-qdis 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (use-target true) "
            "        (deduce/query gd-reach-qdis "
            "          (lambda (row) "
            "            (if use-target "
            "              (= (ref row 'dst) target) "
            "              (= (ref row 'dst) 11))) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdis)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdis) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use row-independent if wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use row-independent if wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use row-independent if wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-edge-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-reach-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-other-src-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-other-out-qdip (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-reach-qdip ?x ?y) '(gd-edge-qdip ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-reach-qdip ?x ?z) '(gd-edge-qdip ?x ?y) '(gd-reach-qdip ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-other-out-qdip ?x ?y) '(gd-other-src-qdip ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-plain) "
            "  (deduce/fact! gd-edge-qdip 1 2) "
            "  (deduce/fact! gd-edge-qdip 2 3) "
            "  (deduce/fact! gd-edge-qdip 10 11) "
            "  (deduce/fact! gd-other-src-qdip 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (use-target true) "
            "        (deduce/query gd-reach-qdip "
            "          (lambda (row) "
            "            (if use-target "
            "              (= (ref row 'dst) target) "
            "              (= (ref row 'dst) 11))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdip)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdip)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use row-independent if wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use row-independent if wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use row-independent if wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-edge-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-reach-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-src-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-out-qdfu (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdfu ?x ?y) '(gd-edge-qdfu ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdfu ?x ?z) '(gd-edge-qdfu ?x ?y) '(gd-reach-qdfu ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-other-out-qdfu ?x ?y) '(gd-other-src-qdfu ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-fallback) "
            "  (deduce/fact! gd-edge-qdfu 1 2) "
            "  (deduce/fact! gd-edge-qdfu 2 3) "
            "  (deduce/fact! gd-edge-qdfu 10 11) "
            "  (deduce/fact! gd-other-src-qdfu 9 10) "
            "  (define selected "
            "    (let (k 1) "
            "      (let (id (lambda (x) x)) "
            "        (deduce/query gd-reach-qdfu "
            "          (lambda (row) (= (ref row 'src) (id k))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdfu)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-fallback)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 2) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdfu)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query still falls back to selected closure for dirty closure reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query still falls back to selected closure for dirty closure reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query still falls back to selected closure for dirty closure reads");
            }
            (*fail)++;
        }
    }


    run_deduce_admin_surface_fallback_tests(interp, pass, fail);
}
