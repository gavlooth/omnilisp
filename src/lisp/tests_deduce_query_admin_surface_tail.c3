module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_admin_surface_tail_fallback_demand_tests(interp, pass, fail);
    run_deduce_admin_surface_tail_materialized_wrapper_tests(interp, pass, fail);

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-positive (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-positive] gd-edge (src) (dst)) "
            "  (define [relation db-admin-goal-directed-positive] gd-reach (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?y) '(gd-edge ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?z) '(gd-edge ?x ?y) '(gd-reach ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-positive)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 1) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 0) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 2) "
            "    (= (ref first 'goal-directed-eligible) true) "
            "    (= (ref first 'goal-directed-shape) 'positive-recursive-closure) "
            "    (= (ref first 'goal-directed-blockers) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed eligible recursive component counts");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-blocked (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-edge-b (src) (dst)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-blocked-b (node)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-chosen-b (src) (score)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-score-b (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-chosen-b ?x ?n) '(gd-score-b ?x ?n) '(not (gd-blocked-b ?x))) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?y) '(gd-chosen-b ?y ?n)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-blocked)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (define blockers (ref first 'goal-directed-blockers)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 0) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 1) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 3) "
            "    (= (ref first 'goal-directed-eligible) nil) "
            "    (= (ref first 'goal-directed-shape) nil) "
            "    (= (car blockers) 'aggregates-present) "
            "    (= (car (cdr blockers)) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-batches (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-batches)) "
            "  (and "
            "    (= (ref analyze 'parallel-runtime-mode) 'serial-runtime-parallel-topology-planned) "
            "    (= (ref analyze 'parallel-recursive-component-count) 3) "
            "    (= (ref analyze 'parallel-batched-component-count) 2) "
            "    (= (ref analyze 'parallel-batch-count) 2) "
            "    (= (ref analyze 'parallel-max-batch-size) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-stats (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-stats)) "
            "  (define stats (deduce/stats pb-reach-a)) "
            "  (define analyze-topology (ref analyze 'parallel-batch-topology)) "
            "  (define stats-topology (ref stats 'parallel-batch-topology)) "
            "  (and "
            "    (= (ref stats 'parallel-runtime-mode) 'serial-runtime-parallel-topology-planned) "
            "    (= (ref stats 'parallel-recursive-component-count) 3) "
            "    (= (ref stats 'parallel-batched-component-count) 2) "
            "    (= (ref stats 'parallel-batch-count) 2) "
            "    (= (ref stats 'parallel-max-batch-size) 2) "
            "    (= (length analyze-topology) 2) "
            "    (= (length stats-topology) 2) "
            "    (= (ref (car analyze-topology) 'stratum) 0) "
            "    (= (ref (car analyze-topology) 'wave) 1) "
            "    (= (ref (car analyze-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr analyze-topology)) 'wave) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'batch-size) 1) "
            "    (= (ref (car stats-topology) 'stratum) 0) "
            "    (= (ref (car stats-topology) 'wave) 1) "
            "    (= (ref (car stats-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr stats-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr stats-topology)) 'wave) 2) "
            "    (= (ref (car (cdr stats-topology)) 'batch-size) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-selected] gd-edge-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-reach-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-src-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?y) '(gd-edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?z) '(gd-edge-s ?x ?y) '(gd-reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-other-out-s ?x ?y) '(gd-other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-selected) "
            "  (deduce/fact! gd-edge-s 1 2) "
            "  (deduce/fact! gd-edge-s 2 3) "
            "  (deduce/fact! gd-other-src-s 9 10) "
            "  (define selected (deduce/analyze db-admin-goal-directed-selected 1)) "
            "  (define selected-components (ref selected 'goal-directed-selected-components)) "
            "  (define selected-predicates (ref selected 'goal-directed-selected-predicates)) "
            "  (define other-before (deduce/count gd-other-out-s)) "
            "  (define full (deduce/analyze db-admin-goal-directed-selected)) "
            "  (define other-after (deduce/count gd-other-out-s)) "
            "  (and "
            "    (= (ref selected 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref selected 'goal-directed-selector-rule-index) 1) "
            "    (= (ref selected 'goal-directed-selected-component-id) 1) "
            "    (= (length selected-components) 2) "
            "    (= (car selected-components) 0) "
            "    (= (car (cdr selected-components)) 1) "
            "    (= (car selected-predicates) 'gd-reach-s) "
            "    (= (car (cdr selected-predicates)) 'gd-edge-s) "
            "    (= (length selected-predicates) 2) "
            "    (= (ref selected 'incremental-dirty-predicate-count) 4) "
            "    (= (deduce/count gd-reach-s) 3) "
            "    (= other-before 0) "
            "    (= (ref full 'goal-directed-execution-path) 'full-db-fixpoint) "
            "    (= (ref full 'goal-directed-selected-components) nil) "
            "    (= (ref full 'goal-directed-selected-predicates) nil) "
            "    (= (ref full 'incremental-dirty-predicate-count) 2) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-dirty-frontier-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-dirty-frontier-selected] edge-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] reach-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-src-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?y) '(edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?z) '(edge-s ?x ?y) '(reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(other-out-s ?x ?y) '(other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-dirty-frontier-selected) "
            "  (deduce/fact! edge-s 1 2) "
            "  (deduce/fact! edge-s 2 3) "
            "  (deduce/fact! other-src-s 9 10) "
            "  (deduce/analyze db-admin-dirty-frontier-selected 1) "
            "  (define reach-stats (deduce/stats reach-s)) "
            "  (define other-stats (deduce/stats other-out-s)) "
            "  (and "
            "    (= (ref reach-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref reach-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref reach-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref reach-stats 'dirty-self) false) "
            "    (= (ref other-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref other-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref other-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-explain-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-explain-classification] gd-edge-ex (src) (dst)) "
            "  (define [relation db-admin-explain-classification] gd-reach-ex (src) (dst)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?y) '(gd-edge-ex ?x ?y)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?z) '(gd-edge-ex ?x ?y) '(gd-reach-ex ?y ?z)) "
            "  (deduce/fact! gd-edge-ex 1 2) "
            "  (deduce/fact! gd-edge-ex 2 3) "
            "  (deduce/query gd-reach-ex (lambda (row) (= (ref row 'src) 1))) "
            "  (define explain (deduce/explain db-admin-explain-classification 1)) "
            "  (define steps (ref explain 'steps)) "
            "  (define step0 (car steps)) "
            "  (define step1 (car (cdr steps))) "
            "  (and "
            "    (= (ref explain 'surface-kind) 'planner-snapshot) "
            "    (= (ref explain 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref explain 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref explain 'last-goal-directed-read-surface) 'query) "
            "    (= (ref explain 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref explain 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref explain 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length steps) 2) "
            "    (= (ref (ref step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain keeps planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-analyze-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-analyze-classification] gd-edge-an (src) (dst)) "
            "  (define [relation db-admin-analyze-classification] gd-reach-an (src) (dst)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?y) '(gd-edge-an ?x ?y)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?z) '(gd-edge-an ?x ?y) '(gd-reach-an ?y ?z)) "
            "  (deduce/fact! gd-edge-an 1 2) "
            "  (deduce/fact! gd-edge-an 2 3) "
            "  (deduce/query gd-reach-an (lambda (row) (= (ref row 'src) 1))) "
            "  (define analyze (deduce/analyze db-admin-analyze-classification 1)) "
            "  (define rules (ref analyze 'rule-execution)) "
            "  (define rule0 (car rules)) "
            "  (define rule1 (car (cdr rules))) "
            "  (define rule0-step0 (car (ref rule0 'steps))) "
            "  (define rule1-step0 (car (ref rule1 'steps))) "
            "  (define rule1-step1 (car (cdr (ref rule1 'steps)))) "
            "  (and "
            "    (= (ref analyze 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref analyze 'goal-directed-selector-rule-index) 1) "
            "    (= (ref analyze 'goal-directed-selected-component-id) 1) "
            "    (= (ref analyze 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze 'last-goal-directed-read-relation) 'gd-reach-an) "
            "    (= (ref analyze 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (length rules) 2) "
            "    (= (ref rule0 'rule-index) 0) "
            "    (= (ref rule1 'rule-index) 1) "
            "    (= (ref rule0 'head-predicate) 'gd-reach-an) "
            "    (= (ref rule1 'head-predicate) 'gd-reach-an) "
            "    (= (length (ref rule0 'steps)) 1) "
            "    (= (length (ref rule1 'steps)) 2) "
            "    (= (ref (ref rule0-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-conjunctive-order (deduce 'open 'memory)) "
            "  (define [relation db-admin-conjunctive-order] ord-src (id) (k)) "
            "  (define [relation db-admin-conjunctive-order] ord-dim (k) (v)) "
            "  (define [relation db-admin-conjunctive-order] ord-out (id) (v)) "
            "  (deduce/rule! db-admin-conjunctive-order '(ord-out ?id ?v) '(ord-src ?id ?k) '(ord-dim ?k ?v)) "
            "  (deduce/fact! ord-src 1 10) "
            "  (deduce/fact! ord-src 2 20) "
            "  (deduce/fact! ord-dim 10 100) "
            "  (deduce/fact! ord-dim 20 200) "
            "  (deduce/fact! ord-dim 30 300) "
            "  (define explain (deduce/explain db-admin-conjunctive-order 0)) "
            "  (define analyze (deduce/analyze db-admin-conjunctive-order)) "
            "  (define rule0 (car (ref analyze 'rule-execution))) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (define analyze-steps (ref rule0 'steps)) "
            "  (define explain-step0 (car explain-steps)) "
            "  (define explain-step1 (car (cdr explain-steps))) "
            "  (define analyze-step0 (car analyze-steps)) "
            "  (define analyze-step1 (car (cdr analyze-steps))) "
            "  (and "
            "    (= (ref explain 'join-order) '(ord-src ord-dim)) "
            "    (= (ref rule0 'join-order) '(ord-src ord-dim)) "
            "    (= (ref explain-step0 'predicate) 'ord-src) "
            "    (= (ref explain-step1 'predicate) 'ord-dim) "
            "    (= (ref analyze-step0 'predicate) 'ord-src) "
            "    (= (ref analyze-step1 'predicate) 'ord-dim) "
            "    (= (ref explain-step0 'operator) 'full-scan) "
            "    (= (ref explain-step1 'operator) 'index-scan) "
            "    (= (ref analyze-step0 'operator) 'full-scan) "
            "    (= (ref analyze-step1 'operator) 'index-scan) "
            "    (= (ref explain-step0 'selected-index) nil) "
            "    (= (ref explain-step1 'selected-index) 0) "
            "    (= (ref analyze-step0 'selected-index) nil) "
            "    (= (ref analyze-step1 'selected-index) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-counter-truth (deduce 'open 'memory)) "
            "  (define [relation db-admin-counter-truth] ctr-edge (src) (dst)) "
            "  (define [relation db-admin-counter-truth] ctr-reach (src) (dst)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?y) '(ctr-edge ?x ?y)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?z) '(ctr-edge ?x ?y) '(ctr-reach ?y ?z)) "
            "  (deduce/fact! ctr-edge 1 2) "
            "  (deduce/fact! ctr-edge 2 5) "
            "  (deduce/fact! ctr-edge 3 4) "
            "  (deduce/fact! ctr-edge 4 5) "
            "  (deduce/fact! ctr-edge 9 10) "
            "  (deduce/fact! ctr-edge 10 5) "
            "  (deduce/match ctr-reach '(ctr-reach ?src 5) 1 'naive) "
            "  (define stats (deduce/stats ctr-reach)) "
            "  (define analyze (deduce/analyze db-admin-counter-truth 1)) "
            "  (define explain (deduce/explain db-admin-counter-truth 1)) "
            "  (define stats-steps (ref stats 'last-goal-directed-read-step-counters)) "
            "  (define analyze-rule1 (car (cdr (ref analyze 'rule-execution)))) "
            "  (define analyze-steps (ref analyze-rule1 'steps)) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (and "
            "    (= (length stats-steps) 2) "
            "    (= (ref (car stats-steps) 'counter-kind) 'observed) "
            "    (= (ref (car (cdr stats-steps)) 'counter-kind) 'observed) "
            "    (= (ref (car stats-steps) 'rows-read) 12) "
            "    (= (ref (car (cdr stats-steps)) 'rows-read) 16) "
            "    (= (length analyze-steps) 2) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'rows-read) 18) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'rows-read) 54) "
            "    (= (length explain-steps) 2) "
            "    (= (ref (ref (car explain-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car explain-steps) 'counters) 'rows-read) 6) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'rows-read) 54)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-txn-commit-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-insert] edge-txc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-insert] reach-txc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?y) '(edge-txc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?z) '(edge-txc ?x ?y) '(reach-txc ?y ?z)) "
            "  (define analyze0 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define txc (deduce 'block db-admin-txn-commit-insert 'write)) "
            "  (deduce 'fact! txc edge-txc 1 2) "
            "  (deduce 'fact! txc edge-txc 2 3) "
            "  (deduce 'commit txc) "
            "  (define analyze1 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define db-admin-txn-abort-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-abort-insert] edge-txa (src) (dst)) "
            "  (define [relation db-admin-txn-abort-insert] reach-txa (src) (dst)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?y) '(edge-txa ?x ?y)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?z) '(edge-txa ?x ?y) '(reach-txa ?y ?z)) "
            "  (define analyze2 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define txa (deduce 'block db-admin-txn-abort-insert 'write)) "
            "  (deduce 'fact! txa edge-txa 1 2) "
            "  (deduce 'fact! txa edge-txa 2 3) "
            "  (deduce 'abort txa) "
            "  (define analyze3 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define db-admin-txn-commit-delete (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-delete] edge-tdc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-delete] reach-tdc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?y) '(edge-tdc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?z) '(edge-tdc ?x ?y) '(reach-tdc ?y ?z)) "
            "  (deduce 'fact! edge-tdc 1 2) "
            "  (deduce 'fact! edge-tdc 2 3) "
            "  (define txd (deduce 'block db-admin-txn-commit-delete 'write)) "
            "  (deduce 'retract! txd edge-tdc 2 3) "
            "  (deduce 'commit txd) "
            "  (define analyze4 (deduce/analyze db-admin-txn-commit-delete)) "
            "  (and "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze1 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze1 'incremental-invalidation-mode) 'tracked) "
            "    (= (car (ref analyze1 'incremental-dirty-predicates)) 'edge-txc) "
            "    (= (car (cdr (ref analyze1 'incremental-dirty-predicates))) 'reach-txc) "
            "    (= (deduce 'count edge-txc) 2) "
            "    (= (ref analyze2 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze2 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze3 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicates) nil) "
            "    (= (deduce 'count edge-txa) 0) "
            "    (= (ref analyze4 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze4 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (car (ref analyze4 'incremental-dirty-predicates)) 'edge-tdc) "
            "    (= (car (cdr (ref analyze4 'incremental-dirty-predicates))) 'reach-tdc) "
            "    (= (deduce 'count edge-tdc) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce commit applies deferred mutation logs while abort drops them");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce commit applies deferred mutation logs while abort drops them (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce commit applies deferred mutation logs while abort drops them");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-degraded-analyze (deduce 'open 'memory)) "
            "  (define [relation db-admin-degraded-analyze] edge-dr (src) (dst)) "
            "  (define [relation db-admin-degraded-analyze] reach-dr (src) (dst)) "
            "  (deduce 'fact! edge-dr 1 2) "
            "  (deduce 'fact! edge-dr 2 3) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?y) '(edge-dr ?x ?y)) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?z) '(edge-dr ?x ?y) '(reach-dr ?y ?z)) "
            "  (deduce/analyze db-admin-degraded-analyze) "
            "  (deduce 'retract! edge-dr 2 3) "
            "  (define stats0 (deduce/stats reach-dr)) "
            "  (define analyze0 (deduce/analyze db-admin-degraded-analyze)) "
            "  (define stats1 (deduce/stats reach-dr)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 2) "
            "    (= (car (ref analyze0 'incremental-dirty-predicates)) 'edge-dr) "
            "    (= (car (cdr (ref analyze0 'incremental-dirty-predicates))) 'reach-dr) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= (ref stats1 'dirty-predicate-count) 0) "
            "    (= (deduce 'count reach-dr) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze snapshots degraded full-recompute state before clearing it");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-aggregate-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-edge-a (src) (dst)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-score-a (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?y) '(gd-score-a ?y ?n)) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-aggregate-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match] gd-edge-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-reach-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-src-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-out-m (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?y) '(gd-edge-m ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?z) '(gd-edge-m ?x ?y) '(gd-reach-m ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-other-out-m ?x ?y) '(gd-other-src-m ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (deduce/fact! gd-edge-m 1 2) "
            "  (deduce/fact! gd-edge-m 2 3) "
            "  (deduce/fact! gd-other-src-m 9 10) "
            "  (define selected (deduce/match gd-reach-m '(gd-reach-m ?x ?y) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-m)) "
            "  (define other-before (deduce/count gd-other-out-m)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (define other-after (deduce/count gd-other-out-m)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-edge-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-reach-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-src-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-out-md (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?y) '(gd-edge-md ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?z) '(gd-reach-md ?x ?y) '(gd-edge-md ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-other-out-md ?x ?y) '(gd-other-src-md ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand) "
            "  (deduce/fact! gd-edge-md 1 2) "
            "  (deduce/fact! gd-edge-md 2 3) "
            "  (deduce/fact! gd-edge-md 10 11) "
            "  (deduce/fact! gd-edge-md 11 12) "
            "  (deduce/fact! gd-other-src-md 20 21) "
            "  (define selected (deduce/match gd-reach-md '(gd-reach-md 1 ?y) 1)) "
            "  (define reach-stats (deduce/stats gd-reach-md)) "
            "  (define other-stats (deduce/stats gd-other-out-md)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (or (= (ref row 'y) 2) (= (ref row 'y) 3))) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-edge-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-reach-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-src-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-out-mm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?y) '(gd-edge-mm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?z) '(gd-edge-mm ?x ?y) '(gd-reach-mm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-other-out-mm ?x ?y) '(gd-other-src-mm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-mismatch) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-other-out-mm '(gd-other-out-mm ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-mm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-edge-ma (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-score-ma (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?y) '(gd-score-ma ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-score-ma '(gd-score-ma ?x ?n) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-edge-mn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-blocked-mn (node)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-reach-mn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?y) '(gd-edge-mn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?z) '(gd-edge-mn ?x ?y) '(gd-reach-mn ?y ?z) '(not (gd-blocked-mn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-reach-mn '(gd-reach-mn ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-negated-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-edge-n (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-blocked-n (node)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-reach-n (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?y) '(gd-edge-n ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?z) '(gd-edge-n ?x ?y) '(gd-reach-n ?y ?z) '(not (gd-blocked-n ?z))) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-negated-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query] gd-edge-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-reach-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-src-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-out-q (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?y) '(gd-edge-q ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?z) '(gd-edge-q ?x ?y) '(gd-reach-q ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-other-out-q ?x ?y) '(gd-other-src-q ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (deduce/fact! gd-edge-q 1 2) "
            "  (deduce/fact! gd-edge-q 2 3) "
            "  (deduce/fact! gd-other-src-q 9 10) "
            "  (define selected (deduce/query gd-reach-q (lambda (row) true) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-q)) "
            "  (define other-before (deduce/count gd-other-out-q)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (define other-after (deduce/count gd-other-out-q)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-edge-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-reach-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-src-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-out-qm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?y) '(gd-edge-qm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?z) '(gd-edge-qm ?x ?y) '(gd-reach-qm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-other-out-qm ?x ?y) '(gd-other-src-qm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-other-out-qm (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-qm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-edge-qa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-score-qa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?y) '(gd-score-qa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-score-qa (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-edge-qn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-blocked-qn (node)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-reach-qn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?y) '(gd-edge-qn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?z) '(gd-edge-qn ?x ?y) '(gd-reach-qn ?y ?z) '(not (gd-blocked-qn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-reach-qn (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-readers (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-readers] gd-edge-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-reach-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-src-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-out-r (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?y) '(gd-edge-r ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?z) '(gd-edge-r ?x ?y) '(gd-reach-r ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-other-out-r ?x ?y) '(gd-other-src-r ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (deduce/fact! gd-edge-r 1 2) "
            "  (deduce/fact! gd-edge-r 2 3) "
            "  (deduce/fact! gd-other-src-r 9 10) "
            "  (define count-selected (deduce/count gd-reach-r 1)) "
            "  (define scan-selected (deduce/scan gd-reach-r 1)) "
            "  (define range-selected (deduce/scan-range gd-reach-r '(1 2) '(2 3) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-r)) "
            "  (define other-before (deduce/count gd-other-out-r)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (define other-after (deduce/count gd-other-out-r)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= (length scan-selected) 3) "
            "    (= (length range-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-edge-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-reach-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-src-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-out-cm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?y) '(gd-edge-cm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?z) '(gd-edge-cm ?x ?y) '(gd-reach-cm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-other-out-cm ?x ?y) '(gd-other-src-cm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/count gd-other-out-cm 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/count-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-cm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-edge-sa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-score-sa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?y) '(gd-score-sa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan gd-score-sa 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-edge-srn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-blocked-srn (node)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-reach-srn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?y) '(gd-edge-srn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?z) '(gd-edge-srn ?x ?y) '(gd-reach-srn ?y ?z) '(not (gd-blocked-srn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan-range gd-reach-srn '(1 2) '(9 9) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-range-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan-range rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-edge-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-reach-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-src-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-out-qd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?y) '(gd-edge-qd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?z) '(gd-edge-qd ?x ?y) '(gd-reach-qd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-other-out-qd ?x ?y) '(gd-other-src-qd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand) "
            "  (deduce/fact! gd-edge-qd 1 2) "
            "  (deduce/fact! gd-edge-qd 2 3) "
            "  (deduce/fact! gd-edge-qd 10 11) "
            "  (deduce/fact! gd-other-src-qd 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qd "
            "      (lambda (row) (= (ref row 'dst) 3)) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            }
            (*fail)++;
        }
    }

}
