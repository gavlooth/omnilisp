module lisp;


import std::io;

fn void run_http_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("HTTP Tests");

    // http-get is defined (bound in env)
    test_tag(interp, "http-get is bound",
        "(procedure? http-get)", SYMBOL, pass, fail);

    // http-get with bad URL returns error
    {
        EvalResult r = run("(http-get \"not-a-url\")", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] http-get rejects bad URL");
            (*pass)++;
        } else {
            io::printn("[FAIL] http-get should reject bad URL");
            (*fail)++;
        }
    }

    test_error_contains(interp, "http-get requires fiber context",
        "(http-get \"http://127.0.0.1:9/\")",
        "http-get: requires running fiber context", pass, fail);

    test_error_contains(interp, "http-request requires fiber context",
        "(http-request \"GET\" \"http://127.0.0.1:9/\" \"\" \"\")",
        "http-request: requires running fiber context", pass, fail);

    test_eq(interp, "typed wrapper dispatch score tcp-connect",
        "(ref (ref (explain 'dispatch (tcp-connect \"127.0.0.1\" 80)) 'decision) 'best-score)",
        200, pass, fail);

    test_truthy(interp, "typed wrapper dispatch reason http-request",
        "(let (r (ref (ref (explain 'dispatch (http-request \"GET\" \"http://example.com\" \"\" \"\")) 'decision) 'reason)) (or (= r 'method-match) (= r 'fallback)))",
        pass, fail);

    test_truthy(interp, "helper dispatch request GET route",
        "(= (ref (ref (explain 'dispatch (request \"http://example.com\")) 'decision) 'reason) 'method-match)",
        pass, fail);

    test_truthy(interp, "helper dispatch request method/url route",
        "(= (ref (ref (explain 'dispatch (request \"GET\" \"http://example.com\" \"\" \"\")) 'decision) 'reason) 'method-match)",
        pass, fail);

    test_error_contains(interp, "request helper single-arg requires fiber context",
        "(request \"http://127.0.0.1:9/\")",
        "http-get: requires running fiber context", pass, fail);

    test_error_contains(interp, "request helper method/url requires fiber context",
        "(request \"GET\" \"http://127.0.0.1:9/\" \"\" \"\")",
        "http-request: requires running fiber context", pass, fail);

    test_truthy(interp, "http-get connect failure in fiber surfaces connection failure",
        "(block "
        "  (define f (spawn (lambda () "
        "    (try (lambda (ignored) (http-get \"http://127.0.0.1:9/\")) (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))))) "
        "  (define msg (await f)) "
        "  (and (string? msg) (string-contains? msg \"connection failed\")))",
        pass, fail);

    test_truthy(interp, "http-request dns failure in fiber surfaces resolution failure",
        "(block "
        "  (define f (spawn (lambda () "
        "    (try (lambda (ignored) (http-request \"GET\" \"http://bad host/\" \"\" \"\")) (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))))) "
        "  (define msg (await f)) "
        "  (and (string? msg) (string-contains? msg \"resolution failed\")))",
        pass, fail);

    test_truthy(interp, "http-get in fiber via libuv tcp path",
        "(block "
        "  (define hf (spawn (lambda () "
        "    (define p (+ 36200 (% (time-ms) 20000))) "
        "    (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "    (define sf (spawn (lambda () "
        "      (define c (tcp-accept listener)) "
        "      (try (lambda (ignored) (tcp-read-timeout c 1000)) (lambda (e) \"\")) "
        "      (tcp-write c \"HTTP/1.1 200 OK\\r\\nContent-Length: 2\\r\\nConnection: close\\r\\n\\r\\nok\") "
        "      (tcp-close c)))) "
        "    (define cf (spawn (lambda () (http-get (string-append \"http://127.0.0.1:\" (number->string p) \"/\"))))) "
        "    (define res (await cf)) "
        "    (await sf) "
        "    (tcp-close listener) "
        "    (and (= ('status res) 200) (= ('body res) \"ok\"))))) "
        "  (await hf))",
        pass, fail);

    test_truthy(interp, "http-request in fiber via libuv tcp path",
        "(block "
        "  (define hf (spawn (lambda () "
        "    (define p (+ 36300 (% (time-ms) 20000))) "
        "    (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "    (define sf (spawn (lambda () "
        "      (define c (tcp-accept listener)) "
        "      (try (lambda (ignored) (tcp-read-timeout c 1000)) (lambda (e) \"\")) "
        "      (tcp-write c \"HTTP/1.1 201 Created\\r\\nContent-Length: 3\\r\\nConnection: close\\r\\n\\r\\nyes\") "
        "      (tcp-close c)))) "
        "    (define cf (spawn (lambda () "
        "      (http-request \"GET\" (string-append \"http://127.0.0.1:\" (number->string p) \"/q\") \"\" \"\")))) "
        "    (define res (await cf)) "
        "    (await sf) "
        "    (tcp-close listener) "
        "    (and (= ('status res) 201) (= ('body res) \"yes\"))))) "
        "  (await hf))",
        pass, fail);

    // Finwatch normal poll tick should also complete inside a spawned fiber
    // when the fetchers are stubbed, guarding the real startup path without
    // depending on public network availability.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/finwatch/server.omni\") "
            "  (define (feed/fetch-fx) (Ok nil)) "
            "  (define (feed/fetch-crypto) (Ok nil)) "
            "  (define (feed/fetch-news) (Ok nil)) "
            "  (await (spawn (lambda () (block (server-poll-once!) true)))))",
            interp
        );
        if (!r.error.has_error && r.value != null && r.value.tag != NIL) {
            emit_pass_literal("[PASS] finwatch poll tick runs in a spawned fiber");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] finwatch poll tick runs in a spawned fiber (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] finwatch poll tick runs in a spawned fiber (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Finwatch canonical boot smoke (bounded): module/import boot path, interp-only by design.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/finwatch/boot_smoke.omni\") "
            "  true)",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] finwatch canonical boot path smoke (non-blocking)");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] finwatch canonical boot path smoke (non-blocking) (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] finwatch canonical boot path smoke (non-blocking) (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/finwatch/dispatch_smoke.omni\") "
            "  true)",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] finwatch dispatch route/failure smoke");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] finwatch dispatch route/failure smoke (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] finwatch dispatch route/failure smoke (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD server pipeline: tolerate LF-only header/body split.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (let (req (pipeline/decode-request \"GET /items HTTP/1.1\\nHost: x\\n\\n\")) "
            "    (and (= ('method req) \"GET\") (= ('path req) \"/items\"))))",
            interp);
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline decode LF fallback");
            (*pass)++;
        } else {
            io::printn("[FAIL] crud pipeline decode LF fallback");
            (*fail)++;
        }
    }

    // Malformed request line should degrade to route-not-found.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define req (pipeline/decode-request \"BROKENLINE\\r\\n\\r\\n\")) "
            "  (define out (pipeline/result->response (pipeline/execute req))) "
            "  (and (= ('status out) \"error\") (= ('error out) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline malformed request line");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline malformed request line (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline malformed request line (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Missing path token should also degrade to route-not-found.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define req (pipeline/decode-request \"GET\\r\\n\\r\\n\")) "
            "  (define out (pipeline/result->response (pipeline/execute req))) "
            "  (and (= ('status out) \"error\") (= ('error out) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline missing path token");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline missing path token (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline missing path token (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
    // Example CRUD server pipeline: POST create then GET by id.
    {
        setup(interp, "(load \"examples/deduce_crud_server.omni\")");
        setup(interp, "(define post-req {'method \"POST\" 'path \"/items\" 'body {'id 11 'name \"paper\" 'qty 3}})");
        setup(interp, "(define post-res (pipeline/result->response (pipeline/execute post-req)))");
        setup(interp, "(define get-res (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items/11\" 'body nil})))");
        EvalResult r = run(
            "(and (= ('status post-res) \"ok\") "
            "     (and (= ('status get-res) \"ok\") "
            "          (= ('name ('data get-res)) \"paper\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline post/get");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline post/get (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline post/get (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD server pipeline: PUT update then DELETE.
    {
        setup(interp, "(load \"examples/deduce_crud_server.omni\")");
        setup(interp, "(pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 12 'name \"pen\" 'qty 1}})");
        setup(interp, "(define put-res (pipeline/result->response (pipeline/execute {'method \"PUT\" 'path \"/items\" 'body {'id 12 'name \"pen\" 'qty 9}})))");
        setup(interp, "(define row-after-put (repo/get-by-id 12))");
        setup(interp, "(define del-res (pipeline/result->response (pipeline/execute {'method \"DELETE\" 'path \"/items\" 'body {'id 12}})))");
        setup(interp, "(define row-after-del (repo/get-by-id 12))");
        EvalResult r = run(
            "(and (= ('status put-res) \"ok\") "
            "     (and (= row-after-put.qty 9) "
            "          (and (= ('status del-res) \"ok\") "
            "               (null? row-after-del))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline put/delete");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline put/delete (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline put/delete (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline routing: non-numeric item id should not match item route.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define bad (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items/not-int\" 'body nil}))) "
            "  (and (= ('status bad) \"error\") (= ('error bad) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline invalid path id");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline invalid path id (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline invalid path id (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline routing: unknown route returns route-not-found.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define miss (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/unknown\" 'body nil}))) "
            "  (and (= ('status miss) \"error\") (= ('error miss) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline unknown route");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline unknown route (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline unknown route (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
    // Example CRUD server response formatting (deterministic, no live listener).
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define payload (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items\" 'body nil}))) "
            "  (define raw (http/response \"200 OK\" payload)) "
            "  (and (string-contains? raw \"HTTP/1.1 200 OK\") "
            "       (string-contains? raw \"\\\"status\\\":\\\"ok\\\"\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud response formatting");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud response formatting (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud response formatting (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline schema gate: invalid POST payload fails validation.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (null? (validate 'item-write {'id 21 'name \"bad\"})))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline schema gate");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline schema gate (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline schema gate (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline concurrency: duplicate POST race on same id.
    {
        EvalResult r = run(
            "(block "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define c1 "
            "    (spawn (lambda () "
            "      (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 31 'name \"race\" 'qty 1}})))) "
            "  (define c2 "
            "    (spawn (lambda () "
            "      (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 31 'name \"race\" 'qty 1}})))) "
            "  (define r1 (pipeline/result->response (await c1))) "
            "  (define r2 (pipeline/result->response (await c2))) "
            "  (define r1-ok (= ('status r1) \"ok\")) "
            "  (define r1-dup (= ('error r1) \"item already exists\")) "
            "  (define r2-ok (= ('status r2) \"ok\")) "
            "  (define r2-dup (= ('error r2) \"item already exists\")) "
            "  (define cnt (deduce 'count items)) "
            "  (and (= cnt 1) "
            "       (or (and r1-ok r2-dup) (and r2-ok r1-dup)))) )",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline duplicate-post race");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline duplicate-post race (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline duplicate-post race (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
}

fn void run_http_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_http_tests, pass, fail);
}
