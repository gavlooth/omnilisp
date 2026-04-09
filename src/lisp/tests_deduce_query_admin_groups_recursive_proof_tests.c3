module lisp;

import std::io;

fn void run_deduce_admin_surface_recursive_proof_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-rec (deduce 'open 'memory)) "
            "  (define [relation db-why-result-rec] edge-rec-why (src) (dst)) "
            "  (define [relation db-why-result-rec materialized] reach-rec-why (src) (dst)) "
            "  (deduce/rule! db-why-result-rec '(reach-rec-why ?x ?y) '(edge-rec-why ?x ?y)) "
            "  (deduce/rule! db-why-result-rec '(reach-rec-why ?x ?z) '(edge-rec-why ?x ?y) '(reach-rec-why ?y ?z)) "
            "  (deduce/fact! edge-rec-why 1 2) "
            "  (deduce/fact! edge-rec-why 2 3) "
            "  (deduce/fact! edge-rec-why 1 4) "
            "  (deduce/fact! edge-rec-why 4 3) "
            "  (deduce/fact! edge-rec-why 8 9) "
            "  (deduce/fact! edge-rec-why 9 10) "
            "  (deduce/refresh! reach-rec-why) "
            "  (define direct (deduce/why-result reach-rec-why 1 2)) "
            "  (define recursive-ok (deduce/why-result reach-rec-why 8 10)) "
            "  (define recursive-partial (deduce/why-result reach-rec-why 1 3)) "
            "  (define direct-path (car (ref direct 'paths))) "
            "  (define recursive-ok-path (car (ref recursive-ok 'paths))) "
            "  (define recursive-partial-path (car (ref recursive-partial 'paths))) "
            "  (define recursive-ok-support (ref recursive-ok-path 'support)) "
            "  (define recursive-ok-step (car (reverse recursive-ok-support))) "
            "  (define recursive-partial-support (ref recursive-partial-path 'support)) "
            "  (define recursive-partial-step (car (reverse recursive-partial-support))) "
            "  (and "
            "    (= (ref direct 'status) 'ok) "
            "    (= (ref direct 'path-count) 1) "
            "    (= (ref direct 'max-depth) 2) "
            "    (= (ref direct 'truncated) false) "
            "    (= (ref direct-path 'path-kind) 'derived) "
            "    (= (ref direct-path 'rule-index) 0) "
            "    (= (length (ref direct-path 'support)) 1) "
            "    (= (ref recursive-ok 'status) 'ok) "
            "    (= (ref recursive-ok 'path-count) 1) "
            "    (= (ref recursive-ok 'max-depth) 3) "
            "    (= (ref recursive-ok 'truncated) false) "
            "    (= (ref recursive-ok-path 'path-kind) 'derived) "
            "    (= (ref recursive-ok-path 'rule-index) 1) "
            "    (= (length recursive-ok-support) 3) "
            "    (= (ref recursive-ok-step 'kind) 'rule-step) "
            "    (= (ref recursive-ok-step 'predicate) 'reach-rec-why) "
            "    (= (ref recursive-ok-step 'rule-index) 0) "
            "    (= (ref recursive-ok-step 'step-index) 1) "
            "    (= (ref recursive-partial 'status) 'partial) "
            "    (= (ref recursive-partial 'path-count) 1) "
            "    (= (ref recursive-partial 'max-depth) 3) "
            "    (= (ref recursive-partial 'truncated) true) "
            "    (= (ref recursive-partial-path 'path-kind) 'derived) "
            "    (= (ref recursive-partial-path 'rule-index) 1) "
            "    (= (length recursive-partial-support) 3) "
            "    (= (ref recursive-partial-step 'kind) 'rule-step) "
            "    (= (ref recursive-partial-step 'predicate) 'reach-rec-why) "
            "    (= (ref recursive-partial-step 'rule-index) 0) "
            "    (= (ref recursive-partial-step 'step-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes positive recursive closure ok/partial payloads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes positive recursive closure ok/partial payloads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes positive recursive closure ok/partial payloads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd] parent-proof-gd (src) (dst)) "
            "  (define [relation db-why-result-proof-gd materialized] ancestor-proof-gd (src) (dst)) "
            "  (define [relation db-why-result-proof-gd materialized] reach-proof-gd (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd '(ancestor-proof-gd ?x ?y) '(parent-proof-gd ?x ?y)) "
            "  (deduce/rule! db-why-result-proof-gd '(reach-proof-gd ?x ?z) '(parent-proof-gd ?x ?y) '(ancestor-proof-gd ?y ?z)) "
            "  (deduce/fact! parent-proof-gd 8 9) "
            "  (deduce/fact! parent-proof-gd 9 10) "
            "  (deduce/refresh! ancestor-proof-gd) "
            "  (deduce/refresh! reach-proof-gd) "
            "  (deduce/scan-range ancestor-proof-gd '(9 10) '(9 10)) "
            "  (define why (deduce/why-result reach-proof-gd 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define derived-step (car (reverse support))) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define top-gd (ref why 'goal-directed-read-context)) "
            "  (define step-gd (ref derived-step 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? top-gd) nil) "
            "    (= (dict? path-gd) true) "
            "    (= (ref derived-step 'kind) 'rule-step) "
            "    (= (ref derived-step 'predicate) 'ancestor-proof-gd) "
            "    (= (ref derived-step 'tuple) '(9 10)) "
            "    (= (ref path-gd 'surface) 'scan-range) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (dict? step-gd) true) "
            "    (= (ref step-gd 'surface) 'scan-range) "
            "    (= (ref step-gd 'execution-path) 'no-op) "
            "    (= (ref step-gd 'selector-rule-index) nil) "
            "    (= (ref step-gd 'requested-bound-positions) nil) "
            "    (= (ref step-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches goal-directed context to matching derived support frames");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches goal-directed context to matching derived support frames (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches goal-directed context to matching derived support frames");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd-facts (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd-facts] parent-proof-gd-facts (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-facts materialized] reach-proof-gd-facts (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd-facts '(reach-proof-gd-facts ?x ?z) '(parent-proof-gd-facts ?x ?y) '(parent-proof-gd-facts ?y ?z)) "
            "  (deduce/fact! parent-proof-gd-facts 8 9) "
            "  (deduce/fact! parent-proof-gd-facts 9 10) "
            "  (deduce/refresh! reach-proof-gd-facts) "
            "  (deduce/query parent-proof-gd-facts (lambda (row) true)) "
            "  (define why (deduce/why-result reach-proof-gd-facts 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define fact-1 (car support)) "
            "  (define fact-2 (car (cdr support))) "
            "  (define top-gd (ref why 'goal-directed-read-context)) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define fact-1-gd (ref fact-1 'goal-directed-read-context)) "
            "  (define fact-2-gd (ref fact-2 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? top-gd) nil) "
            "    (= (dict? path-gd) true) "
            "    (= (ref fact-1 'kind) 'fact) "
            "    (= (ref fact-1 'predicate) 'parent-proof-gd-facts) "
            "    (= (ref fact-1 'tuple) '(8 9)) "
            "    (= (ref fact-2 'kind) 'fact) "
            "    (= (ref fact-2 'predicate) 'parent-proof-gd-facts) "
            "    (= (ref fact-2 'tuple) '(9 10)) "
            "    (= (ref path-gd 'surface) 'query) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (dict? fact-1-gd) true) "
            "    (= (dict? fact-2-gd) true) "
            "    (= (ref fact-1-gd 'surface) 'query) "
            "    (= (ref fact-1-gd 'execution-path) 'no-op) "
            "    (= (ref fact-1-gd 'selector-rule-index) nil) "
            "    (= (ref fact-1-gd 'requested-bound-positions) nil) "
            "    (= (ref fact-1-gd 'applied-bound-positions) nil) "
            "    (= (ref fact-2-gd 'surface) 'query) "
            "    (= (ref fact-2-gd 'execution-path) 'no-op) "
            "    (= (ref fact-2-gd 'selector-rule-index) nil) "
            "    (= (ref fact-2-gd 'requested-bound-positions) nil) "
            "    (= (ref fact-2-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches goal-directed context to matching fact support frames");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches goal-directed context to matching fact support frames (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches goal-directed context to matching fact support frames");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd-mixed (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd-mixed] parent-proof-gd-mixed (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-mixed materialized] ancestor-proof-gd-mixed (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-mixed materialized] reach-proof-gd-mixed (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd-mixed '(ancestor-proof-gd-mixed ?x ?y) '(parent-proof-gd-mixed ?x ?y)) "
            "  (deduce/rule! db-why-result-proof-gd-mixed '(reach-proof-gd-mixed ?x ?z) '(parent-proof-gd-mixed ?x ?y) '(ancestor-proof-gd-mixed ?y ?z)) "
            "  (deduce/fact! parent-proof-gd-mixed 8 9) "
            "  (deduce/fact! parent-proof-gd-mixed 9 10) "
            "  (deduce/refresh! ancestor-proof-gd-mixed) "
            "  (deduce/refresh! reach-proof-gd-mixed) "
            "  (deduce/query parent-proof-gd-mixed (lambda (row) true)) "
            "  (deduce/scan-range ancestor-proof-gd-mixed '(9 10) '(9 10)) "
            "  (define why (deduce/why-result reach-proof-gd-mixed 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define fact-frame (car support)) "
            "  (define step-frame (car (reverse support))) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define path-gds (ref path 'goal-directed-read-contexts)) "
            "  (define path-gd-1 (car path-gds)) "
            "  (define path-gd-2 (car (cdr path-gds))) "
            "  (define path-rel-1 (ref path-gd-1 'relation)) "
            "  (define path-rel-2 (ref path-gd-2 'relation)) "
            "  (define fact-gd (ref fact-frame 'goal-directed-read-context)) "
            "  (define step-gd (ref step-frame 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= path-gd nil) "
            "    (= (length path-gds) 2) "
            "    (or "
            "      (and (= path-rel-1 'parent-proof-gd-mixed) (= path-rel-2 'ancestor-proof-gd-mixed)) "
            "      (and (= path-rel-1 'ancestor-proof-gd-mixed) (= path-rel-2 'parent-proof-gd-mixed))) "
            "    (= (dict? fact-gd) true) "
            "    (= (dict? step-gd) true) "
            "    (= (ref fact-gd 'relation) 'parent-proof-gd-mixed) "
            "    (= (ref step-gd 'relation) 'ancestor-proof-gd-mixed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-noop-query (deduce 'open 'memory)) "
            "  (define [relation db-why-result-noop-query] parent-noop-query (src) (dst)) "
            "  (define [relation db-why-result-noop-query materialized] ancestor-noop-query (src) (dst)) "
            "  (deduce/rule! db-why-result-noop-query '(ancestor-noop-query ?x ?y) '(parent-noop-query ?x ?y)) "
            "  (deduce/fact! parent-noop-query 8 9) "
            "  (deduce/fact! parent-noop-query 9 10) "
            "  (deduce/refresh! ancestor-noop-query) "
            "  (deduce/query ancestor-noop-query (lambda (row) (= (ref row 'src) 9))) "
            "  (define why (deduce/why-result ancestor-noop-query 9 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'surface) 'query) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-positions) nil) "
            "    (= (ref path-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches bounded path-local context after plain no-op query reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op query reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op query reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-noop-match (deduce 'open 'memory)) "
            "  (define [relation db-why-result-noop-match] parent-noop-match (src) (dst)) "
            "  (define [relation db-why-result-noop-match materialized] ancestor-noop-match (src) (dst)) "
            "  (deduce/rule! db-why-result-noop-match '(ancestor-noop-match ?x ?y) '(parent-noop-match ?x ?y)) "
            "  (deduce/fact! parent-noop-match 8 9) "
            "  (deduce/fact! parent-noop-match 9 10) "
            "  (deduce/refresh! ancestor-noop-match) "
            "  (deduce/match ancestor-noop-match '(ancestor-noop-match 9 10)) "
            "  (define why (deduce/why-result ancestor-noop-match 9 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'surface) 'match) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-positions) nil) "
            "    (= (ref path-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches bounded path-local context after plain no-op match reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op match reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op match reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-sel-shapes-query (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-query] edge-sel-shapes-query (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-query materialized] reach-sel-shapes-query (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-query '(reach-sel-shapes-query ?x ?y) '(edge-sel-shapes-query ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-query '(reach-sel-shapes-query ?x ?z) '(edge-sel-shapes-query ?x ?y) '(reach-sel-shapes-query ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-query 9 10) "
            "  (deduce/refresh! reach-sel-shapes-query) "
            "  (deduce/query reach-sel-shapes-query (lambda (row) (= (ref row 'src) 9)) 1) "
            "  (define why-query (deduce/why-result reach-sel-shapes-query 9 10)) "
            "  (define query-path-gd (ref (car (ref why-query 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-match (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-match] edge-sel-shapes-match (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-match materialized] reach-sel-shapes-match (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-match '(reach-sel-shapes-match ?x ?y) '(edge-sel-shapes-match ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-match '(reach-sel-shapes-match ?x ?z) '(edge-sel-shapes-match ?x ?y) '(reach-sel-shapes-match ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-match 9 10) "
            "  (deduce/refresh! reach-sel-shapes-match) "
            "  (deduce/match reach-sel-shapes-match '(reach-sel-shapes-match 9 10) 1) "
            "  (define why-match (deduce/why-result reach-sel-shapes-match 9 10)) "
            "  (define match-path-gd (ref (car (ref why-match 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-scan (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-scan] edge-sel-shapes-scan (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-scan materialized] reach-sel-shapes-scan (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-scan '(reach-sel-shapes-scan ?x ?y) '(edge-sel-shapes-scan ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-scan '(reach-sel-shapes-scan ?x ?z) '(edge-sel-shapes-scan ?x ?y) '(reach-sel-shapes-scan ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-scan 9 10) "
            "  (deduce/refresh! reach-sel-shapes-scan) "
            "  (deduce/scan reach-sel-shapes-scan 1) "
            "  (define why-scan (deduce/why-result reach-sel-shapes-scan 9 10)) "
            "  (define scan-path-gd (ref (car (ref why-scan 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-range (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-range] edge-sel-shapes-range (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-range materialized] reach-sel-shapes-range (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-range '(reach-sel-shapes-range ?x ?y) '(edge-sel-shapes-range ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-range '(reach-sel-shapes-range ?x ?z) '(edge-sel-shapes-range ?x ?y) '(reach-sel-shapes-range ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-range 9 10) "
            "  (deduce/refresh! reach-sel-shapes-range) "
            "  (deduce/scan-range reach-sel-shapes-range '(9 10) '(9 10) 1) "
            "  (define why-range (deduce/why-result reach-sel-shapes-range 9 10)) "
            "  (define range-path-gd (ref (car (ref why-range 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (dict? query-path-gd) true) "
            "    (= (ref query-path-gd 'surface) 'query) "
            "    (= (ref query-path-gd 'execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref query-path-gd 'selector-rule-index) 1) "
            "    (= (dict? match-path-gd) true) "
            "    (= (ref match-path-gd 'surface) 'match) "
            "    (= (ref match-path-gd 'execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref match-path-gd 'selector-rule-index) 1) "
            "    (= (ref match-path-gd 'requested-bound-count) 2) "
            "    (= (length (ref match-path-gd 'requested-bound-positions)) 2) "
            "    (= (car (ref match-path-gd 'requested-bound-positions)) 0) "
            "    (= (car (cdr (ref match-path-gd 'requested-bound-positions))) 1) "
            "    (= (ref match-path-gd 'applied-bound-count) 1) "
            "    (= (length (ref match-path-gd 'applied-bound-positions)) 1) "
            "    (= (car (ref match-path-gd 'applied-bound-positions)) 1) "
            "    (= (dict? scan-path-gd) true) "
            "    (= (ref scan-path-gd 'surface) 'scan) "
            "    (= (ref scan-path-gd 'execution-path) 'selected-component-closure) "
            "    (= (ref scan-path-gd 'selector-rule-index) 1) "
            "    (= (ref scan-path-gd 'fallback-reason) 'dirty-closure) "
            "    (= (ref scan-path-gd 'requested-bound-count) 0) "
            "    (= (ref scan-path-gd 'requested-bound-positions) nil) "
            "    (= (ref scan-path-gd 'applied-bound-count) 0) "
            "    (= (ref scan-path-gd 'applied-bound-positions) nil) "
            "    (= (dict? range-path-gd) true) "
            "    (= (ref range-path-gd 'surface) 'scan-range) "
            "    (= (ref range-path-gd 'execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref range-path-gd 'selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result (deduce 'open 'memory)) "
            "  (define [relation db-why-result] parent-why (src) (dst)) "
            "  (define [relation db-why-result materialized] ancestor-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reverse-parent-why (dst) (src)) "
            "  (define [relation db-why-result materialized] chain3-why (src) (mid) (dst)) "
            "  (define [relation db-why-result materialized] reach-ext-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reach-mixed-one-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reach-why (src) (dst)) "
            "  (deduce/rule! db-why-result '(ancestor-why ?x ?y) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(reverse-parent-why ?y ?x) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(chain3-why ?x ?y ?z) '(parent-why ?x ?y) '(parent-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-ext-why ?x ?z) '(parent-why ?x ?y) '(parent-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-mixed-one-why ?x ?z) '(parent-why ?x ?y) '(ancestor-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-why ?x ?y) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(reach-why ?x ?z) '(parent-why ?x ?y) '(ancestor-why ?y ?z)) "
            "  (deduce/fact! parent-why 1 2) "
            "  (deduce/fact! parent-why 2 3) "
            "  (deduce/fact! parent-why 1 4) "
            "  (deduce/fact! parent-why 4 3) "
            "  (deduce/fact! parent-why 8 9) "
            "  (deduce/fact! parent-why 9 10) "
            "  (deduce/refresh! ancestor-why) "
            "  (deduce/refresh! reverse-parent-why) "
            "  (deduce/refresh! chain3-why) "
            "  (deduce/refresh! reach-ext-why) "
            "  (deduce/refresh! reach-mixed-one-why) "
            "  (deduce/refresh! reach-why) "
            "  (define seed (deduce/why-result parent-why 1 2)) "
            "  (define missing (deduce/why-result parent-why 9 10)) "
            "  (define derived (deduce/why-result ancestor-why 1 2)) "
            "  (define projected-derived (deduce/why-result reverse-parent-why 2 1)) "
            "  (define multi-derived (deduce/why-result chain3-why 1 2 3)) "
            "  (define search-derived (deduce/why-result reach-ext-why 8 10)) "
            "  (define search-derived-partial (deduce/why-result reach-ext-why 1 3)) "
            "  (define mixed-derived (deduce/why-result reach-mixed-one-why 8 10)) "
            "  (define multi-rule-direct (deduce/why-result reach-why 1 2)) "
            "  (define multi-rule-derived (deduce/why-result reach-why 1 3)) "
            "  (define seed-paths (ref seed 'paths)) "
            "  (define seed-path (car seed-paths)) "
            "  (define seed-support (ref seed-path 'support)) "
            "  (define seed-frame (car seed-support)) "
            "  (define derived-paths (ref derived 'paths)) "
            "  (define derived-path (car derived-paths)) "
            "  (define derived-support (ref derived-path 'support)) "
            "  (define derived-frame (car derived-support)) "
            "  (define projected-derived-path (car (ref projected-derived 'paths))) "
            "  (define projected-derived-frame (car (ref projected-derived-path 'support))) "
            "  (define multi-derived-path (car (ref multi-derived 'paths))) "
            "  (define multi-derived-support (ref multi-derived-path 'support)) "
            "  (define multi-derived-frame0 (car multi-derived-support)) "
            "  (define multi-derived-frame1 (car (cdr multi-derived-support))) "
            "  (define search-derived-path (car (ref search-derived 'paths))) "
            "  (define search-derived-partial-path (car (ref search-derived-partial 'paths))) "
            "  (define mixed-derived-path (car (ref mixed-derived 'paths))) "
            "  (define mixed-derived-support (ref mixed-derived-path 'support)) "
            "  (define mixed-derived-step (car (reverse mixed-derived-support))) "
            "  (printn (list "
            "    (ref seed 'status) (ref seed 'path-count) (ref seed 'max-depth) (ref seed 'truncated) "
            "    (ref derived 'status) (ref derived 'path-count) (ref derived 'max-depth) (ref derived 'truncated) "
            "    (ref projected-derived 'status) (ref projected-derived 'path-count) "
            "    (ref multi-derived 'status) (ref multi-derived 'path-count) (length multi-derived-support) "
            "    (ref search-derived 'status) (ref search-derived 'path-count) (ref search-derived 'truncated) (length (ref search-derived-path 'support)) "
            "    (ref search-derived-partial 'status) (ref search-derived-partial 'path-count) (ref search-derived-partial 'truncated) (length (ref search-derived-partial-path 'support)) "
            "    (ref mixed-derived 'status) (ref mixed-derived 'path-count) (ref mixed-derived 'truncated) (length mixed-derived-support) "
            "    (ref multi-rule-direct 'status) (ref multi-rule-direct 'path-count) (ref multi-rule-direct 'max-depth) (ref multi-rule-direct 'truncated) (ref (car (ref multi-rule-direct 'paths)) 'rule-index) "
            "    (length (ref (car (ref multi-rule-direct 'paths)) 'support)) "
            "    (ref multi-rule-derived 'status) (ref multi-rule-derived 'path-count) (ref multi-rule-derived 'max-depth) (ref multi-rule-derived 'truncated) (ref (car (ref multi-rule-derived 'paths)) 'rule-index) "
            "    (length (ref (car (ref multi-rule-derived 'paths)) 'support)))) "
            "  (and "
            "    (= (ref seed 'kind) 'why-result) "
            "    (= (ref seed 'status) 'ok) "
            "    (= (ref seed 'surface-kind) 'provenance-snapshot) "
            "    (= (ref seed 'subject-kind) 'row) "
            "    (= (ref (ref seed 'subject) 'relation) 'parent-why) "
            "    (= (length (ref (ref seed 'subject) 'tuple)) 2) "
            "    (= (ref seed 'path-count) 1) "
            "    (= (ref seed 'max-depth) 1) "
            "    (= (ref seed 'truncated) nil) "
            "    (= (length seed-paths) 1) "
            "    (= (ref seed-path 'path-kind) 'seed) "
            "    (= (ref seed-path 'head-predicate) 'parent-why) "
            "    (= (ref seed-path 'rule-index) nil) "
            "    (= (ref seed-path 'truncated) nil) "
            "    (= (length seed-support) 1) "
            "    (= (ref seed-frame 'kind) 'fact) "
            "    (= (ref seed-frame 'predicate) 'parent-why) "
            "    (= (ref missing 'status) 'ok) "
            "    (= (ref missing 'path-count) 1) "
            "    (= (ref missing 'max-depth) 1) "
            "    (= (ref missing 'truncated) nil) "
            "    (= (length (ref missing 'paths)) 1) "
            "    (= (ref (car (ref missing 'paths)) 'path-kind) 'seed) "
            "    (= (ref (car (ref missing 'paths)) 'head-predicate) 'parent-why) "
            "    (= (ref (car (ref missing 'paths)) 'rule-index) nil) "
            "    (= (ref (car (ref missing 'paths)) 'truncated) nil) "
            "    (= (length (ref (car (ref missing 'paths)) 'support)) 1) "
            "    (= (ref (car (ref (car (ref missing 'paths)) 'support)) 'kind) 'fact) "
            "    (= (ref (car (ref (car (ref missing 'paths)) 'support)) 'predicate) 'parent-why) "
            "    (= (ref derived 'status) 'ok) "
            "    (= (ref derived 'path-count) 1) "
            "    (= (ref derived 'max-depth) 2) "
            "    (= (ref derived 'truncated) nil) "
            "    (= (length derived-paths) 1) "
            "    (= (ref derived-path 'path-kind) 'derived) "
            "    (= (ref derived-path 'head-predicate) 'ancestor-why) "
            "    (= (ref derived-path 'rule-index) 0) "
            "    (= (length derived-support) 1) "
            "    (= (ref derived-frame 'kind) 'fact) "
            "    (= (ref derived-frame 'predicate) 'parent-why) "
            "    (= (ref projected-derived 'status) 'ok) "
            "    (= (ref projected-derived 'path-count) 1) "
            "    (= (ref projected-derived-path 'path-kind) 'derived) "
            "    (= (ref projected-derived-path 'head-predicate) 'reverse-parent-why) "
            "    (= (ref projected-derived-frame 'predicate) 'parent-why) "
            "    (= (ref multi-derived 'status) 'ok) "
            "    (= (ref multi-derived 'path-count) 1) "
            "    (= (ref multi-derived-path 'path-kind) 'derived) "
            "    (= (ref multi-derived-path 'head-predicate) 'chain3-why) "
            "    (= (length multi-derived-support) 2) "
            "    (= (ref multi-derived-frame0 'predicate) 'parent-why) "
            "    (= (ref multi-derived-frame1 'predicate) 'parent-why) "
            "    (= (ref search-derived 'status) 'ok) "
            "    (= (ref search-derived 'path-count) 1) "
            "    (= (ref search-derived 'truncated) nil) "
            "    (= (ref search-derived-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-path 'support)) 2) "
            "    (= (ref search-derived-partial 'status) 'partial) "
            "    (= (ref search-derived-partial 'path-count) 1) "
            "    (= (ref search-derived-partial 'truncated) true) "
            "    (= (ref search-derived-partial-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-partial-path 'support)) 2) "
            "    (= (ref mixed-derived 'status) 'ok) "
            "    (= (ref mixed-derived 'path-count) 1) "
            "    (= (ref mixed-derived 'truncated) nil) "
            "    (= (ref mixed-derived-path 'path-kind) 'derived) "
            "    (= (length mixed-derived-support) 3) "
            "    (= (ref mixed-derived-step 'kind) 'rule-step) "
            "    (= (ref mixed-derived-step 'predicate) 'ancestor-why) "
            "    (= (ref multi-rule-direct 'status) 'ok) "
            "    (= (ref multi-rule-direct 'path-count) 1) "
            "    (= (ref multi-rule-direct 'max-depth) 2) "
            "    (= (ref multi-rule-direct 'truncated) nil) "
            "    (= (length (ref multi-rule-direct 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-direct 'paths)) 'rule-index) 5) "
            "    (= (length (ref (car (ref multi-rule-direct 'paths)) 'support)) 1) "
            "    (= (ref multi-rule-derived 'status) 'partial) "
            "    (= (ref multi-rule-derived 'path-count) 1) "
            "    (= (ref multi-rule-derived 'max-depth) 3) "
            "    (= (ref multi-rule-derived 'truncated) true) "
            "    (= (length (ref multi-rule-derived 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'path-kind) 'derived) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'head-predicate) 'reach-why) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'rule-index) 6) "
            "    (= (length (ref (car (ref multi-rule-derived 'paths)) 'support)) 3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads");
            }
            (*fail)++;
        }
    }
}
