module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_filter_part2_tests(Interp* interp, int* pass, int* fail) {
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
}
