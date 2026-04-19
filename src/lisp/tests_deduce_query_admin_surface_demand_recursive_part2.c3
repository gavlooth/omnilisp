module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_recursive_tests_part2(Interp* interp, int* pass, int* fail) {
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
}
