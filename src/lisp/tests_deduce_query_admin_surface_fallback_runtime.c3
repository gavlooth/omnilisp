module lisp;

import std::io;

fn void run_deduce_admin_surface_fallback_runtime_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-auto-id (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-edge-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-reach-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-other-src-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-other-out-qa1 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-reach-qa1 ?x ?y) '(gd-edge-qa1 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-reach-qa1 ?x ?z) '(gd-edge-qa1 ?x ?y) '(gd-reach-qa1 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-other-out-qa1 ?x ?y) '(gd-other-src-qa1 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto-id) "
            "  (deduce/fact! gd-edge-qa1 1 2) "
            "  (deduce/fact! gd-edge-qa1 2 3) "
            "  (deduce/fact! gd-other-src-qa1 9 10) "
            "  (define selected "
            "    (let (k 1) "
            "      (let (id (lambda (x) x)) "
            "        (deduce/query gd-reach-qa1 "
            "          (lambda (row) (= (ref row 'src) (id k))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qa1)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-auto-id)) "
            "  (define other-after (deduce/count gd-other-out-qa1)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 2) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qa1) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-edge-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-reach-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-src-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-out-ma0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma0 ?x ?y) '(gd-edge-ma0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma0 ?x ?z) '(gd-edge-ma0 ?x ?y) '(gd-reach-ma0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-other-out-ma0 ?x ?y) '(gd-other-src-ma0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (deduce/fact! gd-edge-ma0 1 2) "
            "  (deduce/fact! gd-edge-ma0 2 3) "
            "  (deduce/fact! gd-other-src-ma0 9 10) "
            "  (define selected (deduce/match gd-reach-ma0 '(gd-reach-ma0 ?x ?y))) "
            "  (define other-stats (deduce/stats gd-other-out-ma0)) "
            "  (define other-before (deduce/count gd-other-out-ma0)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (define other-after (deduce/count gd-other-out-ma0)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce match auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce match auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce match auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-edge-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-reach-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-src-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-out-ca0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca0 ?x ?y) '(gd-edge-ca0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca0 ?x ?z) '(gd-edge-ca0 ?x ?y) '(gd-reach-ca0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-other-out-ca0 ?x ?y) '(gd-other-src-ca0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (deduce/fact! gd-edge-ca0 1 2) "
            "  (deduce/fact! gd-edge-ca0 2 3) "
            "  (deduce/fact! gd-other-src-ca0 9 10) "
            "  (define count-selected (deduce/count gd-reach-ca0)) "
            "  (define other-stats (deduce/stats gd-other-out-ca0)) "
            "  (define other-before (deduce/count gd-other-out-ca0)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (define other-after (deduce/count gd-other-out-ca0)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce count auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce count auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce count auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-edge-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-reach-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-src-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-out-sa0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa0 ?x ?y) '(gd-edge-sa0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa0 ?x ?z) '(gd-edge-sa0 ?x ?y) '(gd-reach-sa0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-other-out-sa0 ?x ?y) '(gd-other-src-sa0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (deduce/fact! gd-edge-sa0 1 2) "
            "  (deduce/fact! gd-edge-sa0 2 3) "
            "  (deduce/fact! gd-other-src-sa0 9 10) "
            "  (define scan-selected (deduce/scan gd-reach-sa0)) "
            "  (define other-stats (deduce/stats gd-other-out-sa0)) "
            "  (define other-before (deduce/count gd-other-out-sa0)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (define other-after (deduce/count gd-other-out-sa0)) "
            "  (and "
            "    (= (length scan-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce scan auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce scan auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce scan auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-edge-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-reach-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-src-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-out-sra0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra0 ?x ?y) '(gd-edge-sra0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra0 ?x ?z) '(gd-edge-sra0 ?x ?y) '(gd-reach-sra0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-other-out-sra0 ?x ?y) '(gd-other-src-sra0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (deduce/fact! gd-edge-sra0 1 2) "
            "  (deduce/fact! gd-edge-sra0 2 3) "
            "  (deduce/fact! gd-other-src-sra0 9 10) "
            "  (define range-selected (deduce/scan-range gd-reach-sra0 '(1 2) '(2 3))) "
            "  (define other-stats (deduce/stats gd-other-out-sra0)) "
            "  (define other-before (deduce/count gd-other-out-sra0)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (define other-after (deduce/count gd-other-out-sra0)) "
            "  (and "
            "    (= (length range-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce scan-range auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce scan-range auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce scan-range auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-clean-target (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-edge-cta (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-reach-cta (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-edge-ctb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-reach-ctb (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-cta ?x ?y) '(gd-edge-cta ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-cta ?x ?z) '(gd-edge-cta ?x ?y) '(gd-reach-cta ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-ctb ?x ?y) '(gd-edge-ctb ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-ctb ?x ?z) '(gd-edge-ctb ?x ?y) '(gd-reach-ctb ?y ?z)) "
            "  (deduce/fact! gd-edge-cta 1 2) "
            "  (deduce/analyze db-admin-goal-directed-clean-target) "
            "  (deduce/fact! gd-edge-ctb 9 10) "
            "  (define stats0 (deduce/stats gd-reach-ctb)) "
            "  (define clean-read (deduce/count gd-reach-cta)) "
            "  (define clean-target-stats (deduce/stats gd-reach-cta)) "
            "  (define stats1 (deduce/stats gd-reach-ctb)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-clean-target)) "
            "  (deduce/analyze db-admin-goal-directed-clean-target) "
            "  (define sibling-count (deduce/count gd-reach-ctb)) "
            "  (and "
            "    (= clean-read 1) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= (ref clean-target-stats 'last-goal-directed-read-execution-path) 'no-op) "
            "    (= (ref clean-target-stats 'last-goal-directed-read-mutation-epoch) (ref clean-target-stats 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'no-op) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-cta) "
            "    (= sibling-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on clean eligible recursive targets do not execute unrelated dirty closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on clean eligible recursive targets do not execute unrelated dirty closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on clean eligible recursive targets do not execute unrelated dirty closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-unrelated-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-edge-ur (src) (dst)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-reach-ur (src) (dst)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-plain-ur (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-unrelated-read '(gd-reach-ur ?x ?y) '(gd-edge-ur ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-unrelated-read '(gd-reach-ur ?x ?z) '(gd-edge-ur ?x ?y) '(gd-reach-ur ?y ?z)) "
            "  (deduce/fact! gd-plain-ur 50 60) "
            "  (deduce/analyze db-admin-goal-directed-unrelated-read) "
            "  (deduce/fact! gd-edge-ur 7 8) "
            "  (define stats0 (deduce/stats gd-reach-ur)) "
            "  (define plain-count (deduce/count gd-plain-ur)) "
            "  (define stats1 (deduce/stats gd-reach-ur)) "
            "  (deduce/analyze db-admin-goal-directed-unrelated-read) "
            "  (define reach-count (deduce/count gd-reach-ur)) "
            "  (and "
            "    (= plain-count 1) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= reach-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on unrelated non-recursive targets do not execute dirty recursive closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on unrelated non-recursive targets do not execute dirty recursive closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on unrelated non-recursive targets do not execute dirty recursive closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-negated-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-edge-na1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-reach-na1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-edge-na2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-blocked-na2 (node)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-reach-na2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na1 ?x ?y) '(gd-edge-na1 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na1 ?x ?z) '(gd-edge-na1 ?x ?y) '(gd-reach-na1 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na2 ?x ?y) '(gd-edge-na2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na2 ?x ?z) '(gd-edge-na2 ?x ?y) '(gd-reach-na2 ?y ?z) '(not (gd-blocked-na2 ?z))) "
            "  (deduce/analyze db-admin-goal-directed-negated-auto) "
            "  (deduce/fact! gd-edge-na1 3 4) "
            "  (define stats0 (deduce/stats gd-reach-na1)) "
            "  (define blocked-read (deduce/count gd-reach-na2)) "
            "  (define stats1 (deduce/stats gd-reach-na1)) "
            "  (deduce/analyze db-admin-goal-directed-negated-auto) "
            "  (define positive-count (deduce/count gd-reach-na1)) "
            "  (and "
            "    (= blocked-read 0) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= positive-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on blocked negated recursive targets do not execute unrelated eligible closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on blocked negated recursive targets do not execute unrelated eligible closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on blocked negated recursive targets do not execute unrelated eligible closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-auto-blocked (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-auto-blocked] gd-edge-ab0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-auto-blocked] gd-score-ab0 (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-auto-blocked '(gd-score-ab0 ?x (sum ?n)) '(gd-edge-ab0 ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-auto-blocked '(gd-score-ab0 ?x (sum ?n)) '(gd-edge-ab0 ?x ?y) '(gd-score-ab0 ?y ?n)) "
            "  (deduce/analyze db-admin-goal-directed-auto-blocked) "
            "  (deduce/fact! gd-edge-ab0 1 2) "
            "  (deduce/fact! gd-edge-ab0 2 3) "
            "  (define before (deduce/count gd-score-ab0)) "
            "  (define stats-before (deduce/stats gd-score-ab0)) "
            "  (deduce/analyze db-admin-goal-directed-auto-blocked) "
            "  (define after (deduce/count gd-score-ab0)) "
            "  (and "
            "    (= before 0) "
            "    (= (ref stats-before 'dirty-self) true) "
            "    (> after 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary relation reads do not auto-execute blocked recursive aggregate closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary relation reads do not auto-execute blocked recursive aggregate closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary relation reads do not auto-execute blocked recursive aggregate closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-edge-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-reach-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-src-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-out-qa2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa2 ?x ?y) '(gd-edge-qa2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa2 ?x ?z) '(gd-edge-qa2 ?x ?y) '(gd-reach-qa2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-other-out-qa2 ?x ?y) '(gd-other-src-qa2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (deduce/fact! gd-edge-qa2 1 2) "
            "  (deduce/fact! gd-edge-qa2 2 3) "
            "  (deduce/fact! gd-other-src-qa2 9 10) "
            "  (define selected (deduce/query gd-reach-qa2 (lambda (row) true))) "
            "  (define reach-stats (deduce/stats gd-reach-qa2)) "
            "  (define other-stats (deduce/stats gd-other-out-qa2)) "
            "  (define other-before (deduce/count gd-other-out-qa2)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (define other-after (deduce/count gd-other-out-qa2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 8) "
            "    (= (ref reach-stats 'dirty-self) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-mutation-epoch) (ref reach-stats 'mutation-epoch)) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query auto-executes an eligible dirty goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query auto-executes an eligible dirty goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query auto-executes an eligible dirty goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-last-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-last-read] gd-edge-qlr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-last-read] gd-reach-qlr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-last-read '(gd-reach-qlr ?x ?y) '(gd-edge-qlr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-last-read '(gd-reach-qlr ?x ?z) '(gd-edge-qlr ?x ?y) '(gd-reach-qlr ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-last-read) "
            "  (deduce/fact! gd-edge-qlr 1 2) "
            "  (deduce/fact! gd-edge-qlr 2 3) "
            "  (deduce/query gd-reach-qlr (lambda (row) true)) "
            "  (define stats0 (deduce/stats gd-reach-qlr)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-query-last-read)) "
            "  (and "
            "    (= (ref stats0 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref stats0 'last-goal-directed-read-surface) 'query) "
            "    (= (ref stats0 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref stats0 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats0 'last-goal-directed-read-mutation-epoch) (ref stats0 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze0 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze0 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-qlr) "
            "    (= (ref analyze0 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze0 'last-goal-directed-read-mutation-epoch) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain selected-closure reads record the last actual goal-directed read path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain selected-closure reads record the last actual goal-directed read path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain selected-closure reads record the last actual goal-directed read path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-full-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-full-read] gd-edge-fr0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-full-read] gd-reach-fr0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-full-read '(gd-reach-fr0 ?x ?y) '(gd-edge-fr0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-full-read '(gd-reach-fr0 ?x ?z) '(gd-edge-fr0 ?x ?y) '(gd-reach-fr0 ?y ?z)) "
            "  (deduce/fact! gd-edge-fr0 1 2) "
            "  (deduce/fact! gd-edge-fr0 2 3) "
            "  (deduce/analyze db-admin-goal-directed-full-read) "
            "  (deduce/retract! gd-edge-fr0 2 3) "
            "  (define selected (deduce/query gd-reach-fr0 (lambda (row) true))) "
            "  (define reach-stats (deduce/stats gd-reach-fr0)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-full-read)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref reach-stats 'dirty-self) false) "
            "    (= (ref reach-stats 'full-recompute-required) false) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'full-db-fixpoint) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) 'full-recompute-required) "
            "    (= (ref reach-stats 'last-goal-directed-read-mutation-epoch) (ref reach-stats 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'full-db-fixpoint) "
            "    (= (ref analyze0 'last-goal-directed-read-fallback-reason) 'full-recompute-required) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-fr0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain derived reads record full-db fixpoint when destructive changes force recompute");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain derived reads record full-db fixpoint when destructive changes force recompute (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain derived reads record full-db fixpoint when destructive changes force recompute");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-edge-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-reach-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-src-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-out-ma2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma2 ?x ?y) '(gd-edge-ma2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma2 ?x ?z) '(gd-edge-ma2 ?x ?y) '(gd-reach-ma2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-other-out-ma2 ?x ?y) '(gd-other-src-ma2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (deduce/fact! gd-edge-ma2 1 2) "
            "  (deduce/fact! gd-edge-ma2 2 3) "
            "  (deduce/fact! gd-other-src-ma2 9 10) "
            "  (define selected (deduce/match gd-reach-ma2 '(gd-reach-ma2 ?x ?y))) "
            "  (define other-stats (deduce/stats gd-other-out-ma2)) "
            "  (define other-before (deduce/count gd-other-out-ma2)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (define other-after (deduce/count gd-other-out-ma2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match auto-executes an eligible dirty goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match auto-executes an eligible dirty goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match auto-executes an eligible dirty goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-edge-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-reach-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-src-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-out-ca2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca2 ?x ?y) '(gd-edge-ca2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca2 ?x ?z) '(gd-edge-ca2 ?x ?y) '(gd-reach-ca2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-other-out-ca2 ?x ?y) '(gd-other-src-ca2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (deduce/fact! gd-edge-ca2 1 2) "
            "  (deduce/fact! gd-edge-ca2 2 3) "
            "  (deduce/fact! gd-other-src-ca2 9 10) "
            "  (define count-selected (deduce/count gd-reach-ca2)) "
            "  (define count-other-stats (deduce/stats gd-other-out-ca2)) "
            "  (define count-other-before (deduce/count gd-other-out-ca2)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (define count-other-after (deduce/count gd-other-out-ca2)) "
            "  (define db-admin-goal-directed-scan-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-edge-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-reach-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-src-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-out-sa2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa2 ?x ?y) '(gd-edge-sa2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa2 ?x ?z) '(gd-edge-sa2 ?x ?y) '(gd-reach-sa2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-other-out-sa2 ?x ?y) '(gd-other-src-sa2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (deduce/fact! gd-edge-sa2 1 2) "
            "  (deduce/fact! gd-edge-sa2 2 3) "
            "  (deduce/fact! gd-other-src-sa2 9 10) "
            "  (define scan-selected (deduce/scan gd-reach-sa2)) "
            "  (define scan-other-stats (deduce/stats gd-other-out-sa2)) "
            "  (define scan-other-before (deduce/count gd-other-out-sa2)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (define scan-other-after (deduce/count gd-other-out-sa2)) "
            "  (define db-admin-goal-directed-scan-range-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-edge-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-reach-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-src-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-out-sra2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra2 ?x ?y) '(gd-edge-sra2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra2 ?x ?z) '(gd-edge-sra2 ?x ?y) '(gd-reach-sra2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-other-out-sra2 ?x ?y) '(gd-other-src-sra2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (deduce/fact! gd-edge-sra2 1 2) "
            "  (deduce/fact! gd-edge-sra2 2 3) "
            "  (deduce/fact! gd-other-src-sra2 9 10) "
            "  (define range-selected (deduce/scan-range gd-reach-sra2 '(1 2) '(2 3))) "
            "  (define range-other-stats (deduce/stats gd-other-out-sra2)) "
            "  (define range-other-before (deduce/count gd-other-out-sra2)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (define range-other-after (deduce/count gd-other-out-sra2)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= count-other-before 0) "
            "    (= (ref count-other-stats 'dirty-self) true) "
            "    (= count-other-after 1) "
            "    (= (length scan-selected) 3) "
            "    (= scan-other-before 0) "
            "    (= (ref scan-other-stats 'dirty-self) true) "
            "    (= scan-other-after 1) "
            "    (= (length range-selected) 3) "
            "    (= range-other-before 0) "
            "    (= (ref range-other-stats 'dirty-self) true) "
            "    (= range-other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures");
            }
            (*fail)++;
        }
    }
}
