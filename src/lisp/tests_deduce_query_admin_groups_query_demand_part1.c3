module lisp;

import std::io;

fn void run_deduce_admin_surface_query_demand_tests_part1(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-not-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-edge-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-reach-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-other-src-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-other-out-qdns (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-reach-qdns ?x ?y) '(gd-edge-qdns ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-reach-qdns ?x ?z) '(gd-edge-qdns ?x ?y) '(gd-reach-qdns ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-other-out-qdns ?x ?y) '(gd-other-src-qdns ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-not-selector) "
            "  (deduce/fact! gd-edge-qdns 1 2) "
            "  (deduce/fact! gd-edge-qdns 2 3) "
            "  (deduce/fact! gd-edge-qdns 10 11) "
            "  (deduce/fact! gd-other-src-qdns 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdns "
            "        (lambda (row) "
            "          (if (not (> target 9)) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdns)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-not-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdns) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed not guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed not guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed not guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-not-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-edge-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-reach-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-other-src-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-other-out-qdnp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-reach-qdnp ?x ?y) '(gd-edge-qdnp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-reach-qdnp ?x ?z) '(gd-edge-qdnp ?x ?y) '(gd-reach-qdnp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-other-out-qdnp ?x ?y) '(gd-other-src-qdnp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-not-plain) "
            "  (deduce/fact! gd-edge-qdnp 1 2) "
            "  (deduce/fact! gd-edge-qdnp 2 3) "
            "  (deduce/fact! gd-edge-qdnp 10 11) "
            "  (deduce/fact! gd-other-src-qdnp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdnp "
            "        (lambda (row) "
            "          (if (not (> target 9)) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdnp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-not-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdnp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed not guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed not guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed not guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-and-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-edge-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-reach-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-other-src-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-other-out-qdias (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-reach-qdias ?x ?y) '(gd-edge-qdias ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-reach-qdias ?x ?z) '(gd-edge-qdias ?x ?y) '(gd-reach-qdias ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-other-out-qdias ?x ?y) '(gd-other-src-qdias ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-and-selector) "
            "  (deduce/fact! gd-edge-qdias 1 2) "
            "  (deduce/fact! gd-edge-qdias 2 3) "
            "  (deduce/fact! gd-edge-qdias 10 11) "
            "  (deduce/fact! gd-other-src-qdias 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdias "
            "        (lambda (row) "
            "          (if (and true (> target 0)) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdias)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-and-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdias) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed and guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed and guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed and guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-and-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-edge-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-reach-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-other-src-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-other-out-qdiap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-reach-qdiap ?x ?y) '(gd-edge-qdiap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-reach-qdiap ?x ?z) '(gd-edge-qdiap ?x ?y) '(gd-reach-qdiap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-other-out-qdiap ?x ?y) '(gd-other-src-qdiap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-and-plain) "
            "  (deduce/fact! gd-edge-qdiap 1 2) "
            "  (deduce/fact! gd-edge-qdiap 2 3) "
            "  (deduce/fact! gd-edge-qdiap 10 11) "
            "  (deduce/fact! gd-other-src-qdiap 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdiap "
            "        (lambda (row) "
            "          (if (and true (> target 0)) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdiap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-and-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdiap)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed and guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed and guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed and guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-numeric (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-edge-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-reach-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-other-src-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-other-out-qdn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-reach-qdn ?x ?y) '(gd-edge-qdn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-reach-qdn ?x ?z) '(gd-edge-qdn ?x ?y) '(gd-reach-qdn ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-other-out-qdn ?x ?y) '(gd-other-src-qdn ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-numeric) "
            "  (deduce/fact! gd-edge-qdn 1 2) "
            "  (deduce/fact! gd-edge-qdn 2 3) "
            "  (deduce/fact! gd-edge-qdn 10 11) "
            "  (deduce/fact! gd-other-src-qdn 9 10) "
            "  (define selected "
            "    (let (target -3) "
            "      (deduce/query gd-reach-qdn "
            "        (lambda (row) "
            "          (= (ref row 'dst) (abs target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdn)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-numeric)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdn) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed numeric wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed numeric wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed numeric wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }
}
