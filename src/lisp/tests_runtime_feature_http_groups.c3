module lisp;


import std::io;


fn void run_http_request_fiber_path_test(Interp* interp, int* pass, int* fail) {
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
        "    (and (= res.status 201) (= res.body \"yes\"))))) "
        "  (await hf))",
        pass, fail);
}

fn void run_http_crud_duplicate_post_reject_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (define item-id (+ 31000 (% (time-ms) 100000))) "
        "  (define r1 (pipeline/result->response (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id item-id 'name \"race\" 'qty 1}}))) "
        "  (define r2 (pipeline/result->response (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id item-id 'name \"race-dup\" 'qty 2}}))) "
        "  (define matches (deduce 'query items (lambda (row) (= (ref row 'id) item-id)))) "
        "  {'count (length matches) "
        "   'r1-status r1.status 'r1-error r1.error "
        "   'r2-status r2.status 'r2-error r2.error})",
        interp
    );

    if (!r.error.has_error && r.value != null && r.value.tag == HASHMAP && r.value.hashmap_val != null) {
        Value* count_key = make_symbol(interp, interp.symbols.intern("count"));
        Value* r1_status_key = make_symbol(interp, interp.symbols.intern("r1-status"));
        Value* r1_error_key = make_symbol(interp, interp.symbols.intern("r1-error"));
        Value* r2_status_key = make_symbol(interp, interp.symbols.intern("r2-status"));
        Value* r2_error_key = make_symbol(interp, interp.symbols.intern("r2-error"));
        Value* count_v = hashmap_get(r.value.hashmap_val, count_key);
        Value* r1_status_v = hashmap_get(r.value.hashmap_val, r1_status_key);
        Value* r1_error_v = hashmap_get(r.value.hashmap_val, r1_error_key);
        Value* r2_status_v = hashmap_get(r.value.hashmap_val, r2_status_key);
        Value* r2_error_v = hashmap_get(r.value.hashmap_val, r2_error_key);

        bool ok = count_v != null && count_v.tag == INT && count_v.int_val == 1 &&
            deduce_test_string_equals(r1_status_v, "ok") &&
            deduce_test_string_equals(r2_error_v, "item already exists");
        if (ok) {
            emit_pass_literal("[PASS] crud pipeline duplicate-post rejects existing id");
            (*pass)++;
            return;
        }

        char[512] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (len > 0) {
            io::printfn("[FAIL] crud pipeline duplicate-post rejects existing id (payload=%s)", (ZString)&rendered);
        } else {
            io::printfn("[FAIL] crud pipeline duplicate-post rejects existing id (payload=<empty>)");
        }
        (*fail)++;
        return;
    }

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] crud pipeline duplicate-post rejects existing id");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] crud pipeline duplicate-post rejects existing id (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] crud pipeline duplicate-post rejects existing id (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_http_crud_concurrent_write_guard_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (await (spawn (lambda () "
        "    (atomic-cas! crud-write-hold-ms 0 25) "
        "    (define item-id (+ 41000 (% (time-ms) 100000))) "
        "    (define slow (spawn (lambda () "
        "      (pipeline/result->response (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id item-id 'name \"slow\" 'qty 1}}))))) "
        "    (async-sleep 1) "
        "    (define fast (spawn (lambda () "
        "      (pipeline/result->response (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id item-id 'name \"fast\" 'qty 2}}))))) "
        "    (define r1 (await slow)) "
        "    (define r2 (await fast)) "
        "    (atomic-cas! crud-write-hold-ms 25 0) "
        "    (define matches (deduce 'query items (lambda (row) (= (ref row 'id) item-id)))) "
        "    {'count (length matches) 'r1-status r1.status 'r1-error r1.error 'r2-status r2.status 'r2-error r2.error}))) )",
        interp
    );

    if (!r.error.has_error && r.value != null && r.value.tag == HASHMAP && r.value.hashmap_val != null) {
        Value* count_key = make_symbol(interp, interp.symbols.intern("count"));
        Value* r1_status_key = make_symbol(interp, interp.symbols.intern("r1-status"));
        Value* r1_error_key = make_symbol(interp, interp.symbols.intern("r1-error"));
        Value* r2_status_key = make_symbol(interp, interp.symbols.intern("r2-status"));
        Value* r2_error_key = make_symbol(interp, interp.symbols.intern("r2-error"));
        Value* count_v = hashmap_get(r.value.hashmap_val, count_key);
        Value* r1_status_v = hashmap_get(r.value.hashmap_val, r1_status_key);
        Value* r1_error_v = hashmap_get(r.value.hashmap_val, r1_error_key);
        Value* r2_status_v = hashmap_get(r.value.hashmap_val, r2_status_key);
        Value* r2_error_v = hashmap_get(r.value.hashmap_val, r2_error_key);

        bool r1_ok = deduce_test_string_equals(r1_status_v, "ok");
        bool r2_ok = deduce_test_string_equals(r2_status_v, "ok");
        bool r1_conflict = deduce_test_string_equals(r1_error_v, "crud write already in progress") ||
                           deduce_test_string_equals(r1_error_v, "item already exists");
        bool r2_conflict = deduce_test_string_equals(r2_error_v, "crud write already in progress") ||
                           deduce_test_string_equals(r2_error_v, "item already exists");
        bool one_ok_one_conflict =
            count_v != null && count_v.tag == INT && count_v.int_val == 1 &&
            ((r1_ok && r2_conflict) || (r2_ok && r1_conflict));

        if (one_ok_one_conflict) {
            emit_pass_literal("[PASS] crud pipeline concurrent write returns normalized conflict");
            (*pass)++;
            return;
        }

        char[512] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (len > 0) {
            io::printfn("[FAIL] crud pipeline concurrent write returns normalized conflict (payload=%s)", (ZString)&rendered);
        } else {
            io::printfn("[FAIL] crud pipeline concurrent write returns normalized conflict (payload=<empty>)");
        }
        (*fail)++;
        return;
    }

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] crud pipeline concurrent write returns normalized conflict");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] crud pipeline concurrent write returns normalized conflict (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] crud pipeline concurrent write returns normalized conflict (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_http_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("HTTP Tests");

    {
        ParsedUrl parsed = {};
        bool rejected_trailing_port_garbage = false;
        bool rejected_empty_port = false;
        if (catch err = parse_url("http://example.com:80oops", &parsed)) {
            rejected_trailing_port_garbage = true;
        }
        if (catch err = parse_url("http://example.com:/path", &parsed)) {
            rejected_empty_port = true;
        }
        if (rejected_trailing_port_garbage && rejected_empty_port) {
            emit_pass_literal("[PASS] http url parser rejects malformed port suffixes");
            (*pass)++;
        } else {
            io::printn("[FAIL] http url parser rejects malformed port suffixes");
            (*fail)++;
        }
    }

    {
        Value* parsed = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\nConnection: close\r\n\r\nbody", interp);
        Value* headers_key = make_symbol(interp, interp.symbols.intern("headers"));
        Value* body_key = make_symbol(interp, interp.symbols.intern("body"));
        Value* headers_v = parsed != null && parsed.tag == HASHMAP && parsed.hashmap_val != null
            ? hashmap_get(parsed.hashmap_val, headers_key)
            : null;
        Value* body_v = parsed != null && parsed.tag == HASHMAP && parsed.hashmap_val != null
            ? hashmap_get(parsed.hashmap_val, body_key)
            : null;
        bool ok = headers_v != null &&
            is_string(headers_v) &&
            headers_v.str_len == "X-Test: yes\r\nConnection: close".len &&
            headers_v.str_chars[headers_v.str_len - 1] != '\r' &&
            body_v != null &&
            is_string(body_v) &&
            body_v.str_chars[:body_v.str_len] == "body";
        if (ok) {
            emit_pass_literal("[PASS] http response parser trims header delimiter residue");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] http response parser trims header delimiter residue (headers_len=%d last=%d body_len=%d)",
                headers_v != null && is_string(headers_v) ? (int)headers_v.str_len : -1,
                headers_v != null && is_string(headers_v) && headers_v.str_len > 0 ? (int)headers_v.str_chars[headers_v.str_len - 1] : -1,
                body_v != null && is_string(body_v) ? (int)body_v.str_len : -1
            );
            (*fail)++;
        }
    }

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
        "    (and (= res.status 200) (= res.body \"ok\"))))) "
        "  (await hf))",
        pass, fail);

    run_group_isolated(&run_http_request_fiber_path_test, pass, fail);

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
            "    (and (= req.method \"GET\") (= req.path \"/items\"))))",
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
            "  (and (= out.status \"error\") (= out.error \"route not found\")))",
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
            "  (and (= out.status \"error\") (= out.error \"route not found\")))",
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
            "(and (= post-res.status \"ok\") "
            "     (and (= get-res.status \"ok\") "
            "          (= get-res.data.name \"paper\")))",
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
            "(and (= put-res.status \"ok\") "
            "     (and (= row-after-put.qty 9) "
            "          (and (= del-res.status \"ok\") "
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
            "  (and (= bad.status \"error\") (= bad.error \"route not found\")))",
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
            "  (and (= miss.status \"error\") (= miss.error \"route not found\")))",
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

    // Broad HTTP coverage keeps deterministic duplicate-id rejection here.
    run_group_isolated(&run_http_crud_duplicate_post_reject_test, pass, fail);
}

fn void run_http_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_http_tests, pass, fail);
}

fn void run_http_crud_concurrency_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_http_crud_concurrent_write_guard_test, pass, fail);
}
