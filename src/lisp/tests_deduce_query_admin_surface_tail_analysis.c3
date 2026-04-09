module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_analysis_tests(Interp* interp, int* pass, int* fail) {
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
}
