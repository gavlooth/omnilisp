module lisp;

import std::io;

fn void run_deduce_admin_surface_query_demand_tests(Interp* interp, int* pass, int* fail) {
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

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pow (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-edge-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-reach-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-src-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-out-qdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?y) '(gd-edge-qdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?z) '(gd-edge-qdp ?x ?y) '(gd-reach-qdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-other-out-qdp ?x ?y) '(gd-other-src-qdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pow) "
            "  (deduce/fact! gd-edge-qdp 1 2) "
            "  (deduce/fact! gd-edge-qdp 2 3) "
            "  (deduce/fact! gd-edge-qdp 3 4) "
            "  (deduce/fact! gd-other-src-qdp 9 10) "
            "  (define selected "
            "    (let (base 2 exponent 2) "
            "      (deduce/query gd-reach-qdp "
            "        (lambda (row) "
            "          (= (ref row 'dst) (pow base exponent))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pow)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-atan2 (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-edge-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-reach-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-src-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-out-qdat (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?y) '(gd-edge-qdat ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?z) '(gd-edge-qdat ?x ?y) '(gd-reach-qdat ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-other-out-qdat ?x ?y) '(gd-other-src-qdat ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-atan2) "
            "  (deduce/fact! gd-edge-qdat 1 2) "
            "  (deduce/fact! gd-edge-qdat 2 3) "
            "  (deduce/fact! gd-edge-qdat 3 0) "
            "  (deduce/fact! gd-other-src-qdat 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdat "
            "      (lambda (row) "
            "        (= (ref row 'dst) (atan2 0 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdat)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-atan2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 0) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdat) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed atan2 wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-edge-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-reach-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-src-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-out-qdics (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?y) '(gd-edge-qdics ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?z) '(gd-edge-qdics ?x ?y) '(gd-reach-qdics ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-other-out-qdics ?x ?y) '(gd-other-src-qdics ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector) "
            "  (deduce/fact! gd-edge-qdics 1 2) "
            "  (deduce/fact! gd-edge-qdics 2 3) "
            "  (deduce/fact! gd-edge-qdics 10 11) "
            "  (deduce/fact! gd-other-src-qdics 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdics "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdics)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdics) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-edge-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-reach-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-src-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-out-qdicp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?y) '(gd-edge-qdicp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?z) '(gd-edge-qdicp ?x ?y) '(gd-reach-qdicp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-other-out-qdicp ?x ?y) '(gd-other-src-qdicp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain) "
            "  (deduce/fact! gd-edge-qdicp 1 2) "
            "  (deduce/fact! gd-edge-qdicp 2 3) "
            "  (deduce/fact! gd-edge-qdicp 10 11) "
            "  (deduce/fact! gd-other-src-qdicp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdicp "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdicp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdicp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-edge-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-reach-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-src-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-out-qdos (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?y) '(gd-edge-qdos ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?z) '(gd-edge-qdos ?x ?y) '(gd-reach-qdos ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-other-out-qdos ?x ?y) '(gd-other-src-qdos ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-selector) "
            "  (deduce/fact! gd-edge-qdos 1 2) "
            "  (deduce/fact! gd-edge-qdos 2 3) "
            "  (deduce/fact! gd-edge-qdos 10 11) "
            "  (deduce/fact! gd-other-src-qdos 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdos "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdos)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdos) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-edge-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-reach-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-src-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-out-qdop (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?y) '(gd-edge-qdop ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?z) '(gd-edge-qdop ?x ?y) '(gd-reach-qdop ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-other-out-qdop ?x ?y) '(gd-other-src-qdop ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-plain) "
            "  (deduce/fact! gd-edge-qdop 1 2) "
            "  (deduce/fact! gd-edge-qdop 2 3) "
            "  (deduce/fact! gd-edge-qdop 10 11) "
            "  (deduce/fact! gd-other-src-qdop 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdop "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdop)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdop)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-edge-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-reach-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-src-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-out-qdls (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?y) '(gd-edge-qdls ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?z) '(gd-edge-qdls ?x ?y) '(gd-reach-qdls ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-other-out-qdls ?x ?y) '(gd-other-src-qdls ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-selector) "
            "  (deduce/fact! gd-edge-qdls 1 2) "
            "  (deduce/fact! gd-edge-qdls 2 3) "
            "  (deduce/fact! gd-edge-qdls 10 11) "
            "  (deduce/fact! gd-other-src-qdls 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdls "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdls)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-selector)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdls) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-edge-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-reach-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-src-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-out-qdlp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?y) '(gd-edge-qdlp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?z) '(gd-edge-qdlp ?x ?y) '(gd-reach-qdlp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-other-out-qdlp ?x ?y) '(gd-other-src-qdlp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-plain) "
            "  (deduce/fact! gd-edge-qdlp 1 2) "
            "  (deduce/fact! gd-edge-qdlp 2 3) "
            "  (deduce/fact! gd-edge-qdlp 10 11) "
            "  (deduce/fact! gd-other-src-qdlp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdlp "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdlp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-plain)) "
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
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdlp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-edge-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-sym-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-src-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-out-pms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-edge-pms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-sym-pms ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-other-out-pms ?x ?y) '(gd-other-src-pms ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-swapped) "
            "  (deduce/fact! gd-edge-pms 1 2) "
            "  (deduce/fact! gd-edge-pms 10 11) "
            "  (deduce/fact! gd-other-src-pms 20 21) "
            "  (define selected (deduce/match gd-sym-pms '(gd-sym-pms 1 ?y))) "
            "  (define sym-stats (deduce/stats gd-sym-pms)) "
            "  (define other-stats (deduce/stats gd-other-out-pms)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'y) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-edge-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-reach-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-src-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-out-srdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?z) '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-other-out-srdp ?x ?y) '(gd-other-src-srdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-plain) "
            "  (deduce/fact! gd-edge-srdp 1 2) "
            "  (deduce/fact! gd-edge-srdp 2 3) "
            "  (deduce/fact! gd-edge-srdp 10 11) "
            "  (deduce/fact! gd-edge-srdp 11 12) "
            "  (deduce/fact! gd-other-src-srdp 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srdp '(1 0) '(1 999))) "
            "  (define reach-stats (deduce/stats gd-reach-srdp)) "
            "  (define other-stats (deduce/stats gd-other-out-srdp)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            }
            (*fail)++;
        }
    }

}
