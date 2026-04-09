module lisp;

import std::io;

fn void run_deduce_admin_surface_demand_tail_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_admin_surface_demand_tail_recursive_shape_tests(interp, pass, fail);

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-edge-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-a-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-b-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-c-qdttcs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-a-qdttcs ?x ?y) '(gd-edge-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-a-qdttcs ?x ?y) '(gd-b-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-b-qdttcs ?y ?x) '(gd-c-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-c-qdttcs ?x ?y) '(gd-a-qdttcs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-selector) "
            "  (deduce/fact! gd-edge-qdttcs 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcs "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdttcs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-selector)) "
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
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-edge-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-a-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-b-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-c-qdttcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-a-qdttcp ?x ?y) '(gd-edge-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-a-qdttcp ?x ?y) '(gd-b-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-b-qdttcp ?y ?x) '(gd-c-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-c-qdttcp ?x ?y) '(gd-a-qdttcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-plain) "
            "  (deduce/fact! gd-edge-qdttcp 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdttcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-edge-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-a-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-b-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-c-qdttcds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-a-qdttcds ?x ?y) '(gd-edge-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-a-qdttcds ?x ?y) '(gd-b-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-b-qdttcds ?y ?x) '(gd-c-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-c-qdttcds ?x ?y) '(gd-a-qdttcds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdttcds 1 2) "
            "  (deduce/fact! gd-edge-qdttcds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdttcds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-edge-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-a-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-b-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-c-qdttcdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-a-qdttcdp ?x ?y) '(gd-edge-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-a-qdttcdp ?x ?y) '(gd-b-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-b-qdttcdp ?y ?x) '(gd-c-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-c-qdttcdp ?x ?y) '(gd-a-qdttcdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdttcdp 1 2) "
            "  (deduce/fact! gd-edge-qdttcdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdttcdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-edge-qdtmas (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-a-qdtmas (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-b-qdtmas (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-a-qdtmas ?x ?y) '(gd-edge-qdtmas ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-a-qdtmas ?x ?z) '(gd-b-qdtmas ?x ?y) '(gd-b-qdtmas ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-b-qdtmas ?y ?x) '(gd-a-qdtmas ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-selector) "
            "  (deduce/fact! gd-edge-qdtmas 1 2) "
            "  (deduce/fact! gd-edge-qdtmas 2 3) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmas "
            "      (lambda (row) (and (= (ref row 'src) 3) (= (ref row 'dst) 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtmas)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 3) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-edge-qdtmap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-a-qdtmap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-b-qdtmap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-a-qdtmap ?x ?y) '(gd-edge-qdtmap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-a-qdtmap ?x ?z) '(gd-b-qdtmap ?x ?y) '(gd-b-qdtmap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-b-qdtmap ?y ?x) '(gd-a-qdtmap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-plain) "
            "  (deduce/fact! gd-edge-qdtmap 1 2) "
            "  (deduce/fact! gd-edge-qdtmap 2 3) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmap "
            "      (lambda (row) (and (= (ref row 'src) 3) (= (ref row 'dst) 1))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtmap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 3) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-edge-qdtmds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-a-qdtmds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-b-qdtmds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-a-qdtmds ?x ?y) '(gd-edge-qdtmds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-a-qdtmds ?x ?z) '(gd-b-qdtmds ?x ?y) '(gd-b-qdtmds ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-b-qdtmds ?y ?x) '(gd-a-qdtmds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdtmds 1 2) "
            "  (deduce/fact! gd-edge-qdtmds 2 3) "
            "  (deduce/fact! gd-edge-qdtmds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 3) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtmds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 12) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-edge-qdtmdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-a-qdtmdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-b-qdtmdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-a-qdtmdp ?x ?y) '(gd-edge-qdtmdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-a-qdtmdp ?x ?z) '(gd-b-qdtmdp ?x ?y) '(gd-b-qdtmdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-b-qdtmdp ?y ?x) '(gd-a-qdtmdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdtmdp 1 2) "
            "  (deduce/fact! gd-edge-qdtmdp 2 3) "
            "  (deduce/fact! gd-edge-qdtmdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 3) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtmdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 12) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-edge-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-reach-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-other-src-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-other-out-qdcwlars (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-reach-qdcwlars ?x ?y) '(gd-edge-qdcwlars ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-reach-qdcwlars ?x ?z) '(gd-edge-qdcwlars ?x ?y) '(gd-reach-qdcwlars ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-other-out-qdcwlars ?x ?y) '(gd-other-src-qdcwlars ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second) "
            "  (deduce/fact! gd-edge-qdcwlars 1 2) "
            "  (deduce/fact! gd-edge-qdcwlars 2 3) "
            "  (deduce/fact! gd-edge-qdcwlars 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlars 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (expected candidate) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlars "
            "          (lambda (row) (want target row)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlars)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-edge-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-reach-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-other-src-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-other-out-qdcwlarsp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdcwlarsp ?x ?y) '(gd-edge-qdcwlarsp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdcwlarsp ?x ?z) '(gd-edge-qdcwlarsp ?x ?y) '(gd-reach-qdcwlarsp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-other-out-qdcwlarsp ?x ?y) '(gd-other-src-qdcwlarsp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain) "
            "  (deduce/fact! gd-edge-qdcwlarsp 1 2) "
            "  (deduce/fact! gd-edge-qdcwlarsp 2 3) "
            "  (deduce/fact! gd-edge-qdcwlarsp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlarsp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (expected candidate) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlarsp "
            "          (lambda (row) (want target row)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlarsp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain)) "
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
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-edge-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-reach-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-other-src-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-other-out-qdcwlac (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-reach-qdcwlac ?x ?y) '(gd-edge-qdcwlac ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-reach-qdcwlac ?x ?z) '(gd-edge-qdcwlac ?x ?y) '(gd-reach-qdcwlac ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-other-out-qdcwlac ?x ?y) '(gd-other-src-qdcwlac ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain) "
            "  (deduce/fact! gd-edge-qdcwlac 1 2) "
            "  (deduce/fact! gd-edge-qdcwlac 2 3) "
            "  (deduce/fact! gd-edge-qdcwlac 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlac 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (let (want1 (lambda (candidate expected) (want0 candidate expected))) "
            "          (deduce/query gd-reach-qdcwlac "
            "            (lambda (row) (want1 row target)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlac)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain)) "
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
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-edge-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-reach-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-other-src-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-other-out-qdcwlacp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdcwlacp ?x ?y) '(gd-edge-qdcwlacp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdcwlacp ?x ?z) '(gd-edge-qdcwlacp ?x ?y) '(gd-reach-qdcwlacp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-other-out-qdcwlacp ?x ?y) '(gd-other-src-qdcwlacp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain) "
            "  (deduce/fact! gd-edge-qdcwlacp 1 2) "
            "  (deduce/fact! gd-edge-qdcwlacp 2 3) "
            "  (deduce/fact! gd-edge-qdcwlacp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlacp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (let (want1 (lambda (candidate expected) (want0 candidate expected))) "
            "          (deduce/query gd-reach-qdcwlacp "
            "            (lambda (row) (want1 row target))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlacp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain)) "
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
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    run_deduce_admin_surface_demand_wrapper_tests(interp, pass, fail);
}
