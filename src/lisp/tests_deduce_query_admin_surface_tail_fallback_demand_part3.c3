module lisp;

import std::io;

fn void run_deduce_admin_surface_tail_fallback_demand_tests_part3(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-multi (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-edge-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-path-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-src-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-out-mdm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?y) '(gd-edge-mdm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?z) '(gd-path-mdm ?x ?y) '(gd-path-mdm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-other-out-mdm ?x ?y) '(gd-other-src-mdm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-multi) "
            "  (deduce/fact! gd-edge-mdm 1 2) "
            "  (deduce/fact! gd-edge-mdm 2 3) "
            "  (deduce/fact! gd-edge-mdm 3 4) "
            "  (deduce/fact! gd-other-src-mdm 20 21) "
            "  (define selected (deduce/match gd-path-mdm '(gd-path-mdm 1 4))) "
            "  (define path-stats (deduce/stats gd-path-mdm)) "
            "  (define other-stats (deduce/stats gd-other-out-mdm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-multi)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref path-stats 'dirty-self) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-surface) 'match) "
            "    (= (ref path-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-path-mdm) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-edge-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-reach-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-src-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-out-srd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?y) '(gd-edge-srd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?z) '(gd-reach-srd ?x ?y) '(gd-edge-srd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-other-out-srd ?x ?y) '(gd-other-src-srd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand) "
            "  (deduce/fact! gd-edge-srd 1 2) "
            "  (deduce/fact! gd-edge-srd 2 3) "
            "  (deduce/fact! gd-edge-srd 10 11) "
            "  (deduce/fact! gd-edge-srd 11 12) "
            "  (deduce/fact! gd-other-src-srd 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srd '(1 0) '(1 999) 1 'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-srd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-edge-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-sym-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-src-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-out-srds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-edge-srds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-sym-srds ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-other-out-srds ?x ?y) '(gd-other-src-srds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-swapped) "
            "  (deduce/fact! gd-edge-srds 1 2) "
            "  (deduce/fact! gd-edge-srds 10 11) "
            "  (deduce/fact! gd-other-src-srds 20 21) "
            "  (define selected (deduce/scan-range gd-sym-srds '(1 0) '(1 999))) "
            "  (define sym-stats (deduce/stats gd-sym-srds)) "
            "  (define other-stats (deduce/stats gd-other-out-srds)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-edge-srp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-reach-srp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?y) '(gd-edge-srp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?z) '(gd-reach-srp ?x ?y) '(gd-edge-srp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-projected) "
            "  (deduce/fact! gd-edge-srp 1 2) "
            "  (deduce/fact! gd-edge-srp 2 3) "
            "  (deduce/fact! gd-edge-srp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-srp '(1 3) '(1 3))) "
            "  (define stats-after (deduce/stats gd-reach-srp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-selector-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-edge-ssp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-reach-ssp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?z) '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected) "
            "  (deduce/fact! gd-edge-ssp 1 2) "
            "  (deduce/fact! gd-edge-ssp 2 3) "
            "  (deduce/fact! gd-edge-ssp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-ssp '(1 3) '(1 3) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-ssp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stats-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-ssp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }
}
