module lisp;


import std::io;

alias AdvancedFfiSystemGroupFn = fn void(Interp* interp, int* pass, int* fail);

fn bool advanced_group_filter_matches_selected_exact(char[] name) {
    char* filter = test_c_getenv("OMNI_ADVANCED_GROUP_FILTER");
    if (filter == null) return false;
    return advanced_group_filter_name_exact(filter, name);
}

fn void run_advanced_ffi_system_group(
    char[] name,
    AdvancedFfiSystemGroupFn group_fn,
    Interp* interp,
    int* pass,
    int* fail
) {
    if (!advanced_group_filter_matches_exact(name) &&
        !advanced_group_filter_matches_exact("advanced-ffi-system")) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_ffi_system_selected_exact_group(
    char[] name,
    AdvancedFfiSystemGroupFn group_fn,
    Interp* interp,
    int* pass,
    int* fail
) {
    if (!advanced_group_filter_matches_selected_exact(name)) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_process_wait_event_driven_test(Interp* interp, int* pass, int* fail) {
    omni_uv_process_debug_reset_counters();

    EvalResult waited = run(
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.05\"] nil)) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(= (ref st 'exit-code) 0))",
        interp
    );

    int poll_calls = omni_uv_process_debug_poll_calls();
    int wait_calls = omni_uv_process_debug_wait_calls();
    bool ok = !waited.error.has_error &&
        waited.value != null &&
        waited.value.tag != ERROR &&
        waited.value.tag != NIL &&
        wait_calls >= 1 &&
        poll_calls == 0;

    if (ok) {
        emit_pass("process wait uses event-driven worker wakeup path");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] process wait uses event-driven worker wakeup path (wait_err=%s wait_calls=%d poll_calls=%d)",
            waited.error.has_error ? "yes" : "no",
            wait_calls,
            poll_calls
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_burst_delivery_test(Interp* interp, int* pass, int* fail) {
    EvalResult setup_hits = run("(define signal-burst-hits {'n 0})", interp);
    EvalResult setup_handle = run(
        "(define signal-burst-h (signal-handle 10 (lambda (sig) nil)))",
        interp
    );

    bool ok = !setup_hits.error.has_error &&
        !setup_handle.error.has_error;
    int set_status = -1;
    int pending_before = -1;
    int pending_after = -1;
    int dispatch_count = -1;
    if (ok) {
        Value* handle_v = interp.global_env.lookup(interp.symbols.intern("signal-burst-h"));
        SignalHandle* signal_handle = get_signal_handle(handle_v);
        ok = signal_handle != null &&
            signal_handle.open &&
            signal_handle.uv_signal_watch != null;
        if (ok) {
            signal_debug_reset_dispatch_count();
            set_status = omni_uv_signal_debug_set_pending(signal_handle.uv_signal_watch, 1205);
            pending_before = omni_uv_signal_debug_pending(signal_handle.uv_signal_watch);
            dispatch_pending_signal_callbacks(interp);
            pending_after = omni_uv_signal_debug_pending(signal_handle.uv_signal_watch);
            dispatch_count = (int)signal_debug_dispatch_count();
        }

        ok = set_status == 0 &&
            pending_before >= 1205 &&
            pending_after == 0 &&
            dispatch_count >= 1205;
    }

    EvalResult cleanup = run("(signal-unhandle signal-burst-h)", interp);
    ok = ok &&
        !cleanup.error.has_error &&
        cleanup.value != null &&
        cleanup.value.tag == VOID;

    if (ok) {
        emit_pass("signal callback burst delivery drains all pending callbacks without 1024-cap loss");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal callback burst delivery drains all pending callbacks without 1024-cap loss (setup_hits_err=%s setup_handle_err=%s set_status=%d pending_before=%d pending_after=%d dispatch_count=%d cleanup_err=%s)",
            setup_hits.error.has_error ? "yes" : "no",
            setup_handle.error.has_error ? "yes" : "no",
            set_status,
            pending_before,
            pending_after,
            dispatch_count,
            cleanup.error.has_error ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_advanced_sigint_interrupt_routing_test(Interp* interp, int* pass, int* fail) {
    Interp other = {};
    other.init();
    defer other.destroy();

    interp_interrupt_clear(interp);
    interp_interrupt_clear(&other);

    interp_sigint_bind(interp);
    defer interp_sigint_unbind(interp);
    interp_sigint_debug_mark_pending();

    bool active_requested = interp_interrupt_requested(interp);
    bool other_requested = interp_interrupt_requested(&other);
    interp_interrupt_clear(interp);
    interp_interrupt_clear(&other);

    if (active_requested && !other_requested) {
        emit_pass("sigint interrupt routing targets only bound interpreter");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] sigint interrupt routing targets only bound interpreter (active_requested=%s other_requested=%s)",
            active_requested ? "true" : "false",
            other_requested ? "true" : "false"
        );
        (*fail)++;
    }
}

fn Value* build_signal_noop_callback(Interp* interp) {
    EvalResult callback_eval = run("(lambda (sig) nil)", interp);
    if (callback_eval.error.has_error) return null;
    if (callback_eval.value == null || callback_eval.value.tag == ERROR) return null;
    if (!signal_callback_callable(callback_eval.value)) return null;
    return callback_eval.value;
}

fn void run_advanced_signal_handle_registry_scaling_test(Interp* interp, int* pass, int* fail) {
    const usz HANDLE_COUNT = 40;
    Value*[HANDLE_COUNT] handles;
    for (usz i = 0; i < HANDLE_COUNT; i++) handles[i] = null;

    bool ok = true;
    int create_fail_index = -1;
    int cleanup_fail_index = -1;
    Value* callback = build_signal_noop_callback(interp);
    if (callback == null) {
        ok = false;
    }

    usz created_count = 0;
    if (ok) {
        for (usz i = 0; i < HANDLE_COUNT; i++) {
            Value*[2] create_args;
            create_args[0] = make_int(interp, 10);
            create_args[1] = callback;
            Value* created = prim_signal_handle(create_args[..], null, interp);
            if (created == null || created.tag == ERROR) {
                ok = false;
                create_fail_index = (int)i;
                break;
            }
            handles[i] = created;
            created_count++;
        }
    }

    for (usz i = 0; i < created_count; i++) {
        Value*[1] close_args;
        close_args[0] = handles[i];
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        if (closed == null || closed.tag == ERROR || closed.tag != VOID) {
            ok = false;
            if (cleanup_fail_index < 0) cleanup_fail_index = (int)i;
        }
    }

    if (ok) {
        emit_pass("signal-handle supports >32 active handles without fixed registry cap");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal-handle supports >32 active handles without fixed registry cap (created_count=%d create_fail_index=%d cleanup_fail_index=%d)",
            (int)created_count,
            create_fail_index,
            cleanup_fail_index
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_dispatch_owner_interp_isolation_test(Interp* interp, int* pass, int* fail) {
    Value* callback = build_signal_noop_callback(interp);
    bool ok = callback != null && signal_callback_callable(callback);
    int set_status = -1;
    int after_other = -1;
    int after_owner = -1;
    int cleanup_ok = 0;

    Value* handle_value = null;
    SignalHandle* sh = null;
    if (ok) {
        Value*[2] create_args;
        create_args[0] = make_int(interp, 10);
        create_args[1] = callback;
        handle_value = prim_signal_handle(create_args[..], null, interp);
        sh = get_signal_handle(handle_value);
        ok = handle_value != null &&
            handle_value.tag != ERROR &&
            sh != null &&
            sh.open &&
            sh.uv_signal_watch != null;
    }

    if (ok) {
        Interp other = {};
        other.init();
        defer other.destroy();

        signal_debug_reset_dispatch_count();
        set_status = omni_uv_signal_debug_set_pending(sh.uv_signal_watch, 3);
        dispatch_pending_signal_callbacks(&other);
        after_other = (int)signal_debug_dispatch_count();
        dispatch_pending_signal_callbacks(interp);
        after_owner = (int)signal_debug_dispatch_count();

        ok = set_status == 0 &&
            after_other == 0 &&
            after_owner >= 3;
    }

    if (handle_value != null && handle_value.tag != ERROR) {
        Value*[1] close_args;
        close_args[0] = handle_value;
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        cleanup_ok = (closed != null && closed.tag == VOID) ? 1 : 0;
        ok = ok && cleanup_ok == 1;
    }

    if (ok) {
        emit_pass("signal callback dispatch stays isolated to owning interpreter");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal callback dispatch stays isolated to owning interpreter (set_status=%d after_other=%d after_owner=%d cleanup_ok=%d)",
            set_status,
            after_other,
            after_owner,
            cleanup_ok
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_callback_scope_release_test(Interp* interp, int* pass, int* fail) {
    bool ok = interp != null && interp.root_scope != null;
    uint root_before = ok ? interp.root_scope.refcount : 0;
    uint root_after_create = 0;
    uint root_after_unhandle = 0;
    int cleanup_ok = 0;

    Value* handle_value = null;
    if (ok) {
        Value* callback = build_signal_noop_callback(interp);
        ok = callback != null && signal_callback_callable(callback);
        if (ok) {
            Value*[2] create_args;
            create_args[0] = make_int(interp, 10);
            create_args[1] = callback;
            handle_value = prim_signal_handle(create_args[..], null, interp);
            ok = handle_value != null && handle_value.tag != ERROR;
            if (ok) {
                root_after_create = interp.root_scope.refcount;
            }
        }
    }

    if (handle_value != null && handle_value.tag != ERROR) {
        Value*[1] close_args;
        close_args[0] = handle_value;
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        cleanup_ok = (closed != null && closed.tag == VOID) ? 1 : 0;
        if (interp.root_scope != null) {
            root_after_unhandle = interp.root_scope.refcount;
        }
        ok = ok &&
            cleanup_ok == 1 &&
            root_after_create == (root_before + 1) &&
            root_after_unhandle == root_before;
    } else {
        ok = false;
    }

    if (ok) {
        emit_pass("signal-handle releases retained callback owner scope on unhandle");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal-handle releases retained callback owner scope on unhandle (root_before=%d root_after_create=%d root_after_unhandle=%d cleanup_ok=%d)",
            (int)root_before,
            (int)root_after_create,
            (int)root_after_unhandle,
            cleanup_ok
        );
        (*fail)++;
    }
}

fn void run_advanced_io_typed_effect_tests(Interp* interp, int* pass, int* fail) {
    // === I/O EFFECTS TESTS (Phase 5) ===
    // print/println go through effects now (fast path to __raw-*). Capture the
    // payload through handlers here so the broad validation gates stay quiet.
    test_truthy_interp(interp, "io effect print",
        "(= (handle (block (print 42) nil) (io/print x x)) 42)",
        pass, fail);
    test_truthy_interp(interp, "io effect println",
        "(= (handle (block (println 99) nil) (io/println x x)) 99)",
        pass, fail);

    // Custom handler can suppress output
    test_eq_interp(interp, "io handle suppress",
        "(handle (block (println \"suppressed\") 42) (io/println x (resolve nil)))",
        42, pass, fail);

    // Custom handler can capture output
    test_truthy_interp(interp, "io handle capture",
        "(= (handle (block (println \"captured\") nil) (io/println x x)) \"captured\")",
        pass, fail);

    // Keep one direct raw I/O primitive probe in the broad gate, but use
    // display-mode empty-string emission so it stays silent.
    test_tag_interp(interp, "raw-display direct", "(__raw-display \"\")", VOID, pass, fail);

    // === TYPED EFFECT DECLARATIONS TESTS ===
    // Declaring effects doesn't error
    test_nil_interp(interp, "defeffect decl", "(define [effect] (test/greet (^String name)))", pass, fail);
    test_nil_interp(interp, "defeffect no arg", "(define [effect] (test/ping))", pass, fail);
    test_nil_interp(interp, "defeffect int arg", "(define [effect] (test/count (^Integer n)))", pass, fail);

    // Perform with correct type works
    test_str_interp(interp, "effect correct type",
        "(handle (signal test/greet \"hello\") (test/greet x x))", pass, fail);
    test_eq_interp(interp, "effect int correct",
        "(handle (signal test/count 42) (test/count x x))", 42, pass, fail);

    // Perform with wrong type errors
    test_error(interp, "effect wrong type int",
        "(handle (signal test/greet 42) (test/greet x x))", pass, fail);
    test_error(interp, "effect wrong type str",
        "(handle (signal test/count \"bad\") (test/count x x))", pass, fail);

    // Undeclared effects are canonical (no declaration => no type check)
    test_eq_interp(interp, "effect undeclared canonical",
        "(handle (signal custom-tag 42) (custom-tag x (+ x 1)))", 43, pass, fail);

    // Nullary effect (no arg type) accepts anything
    test_eq_interp(interp, "effect nullary any",
        "(handle (signal test/ping 99) (test/ping x x))", 99, pass, fail);

    // ^Any effects accept all types
    test_truthy_interp(interp, "io effect Any int",
        "(= (handle (block (print 42) nil) (io/print x x)) 42)",
        pass, fail);
    test_truthy_interp(interp, "io effect Any str",
        "(= (handle (block (print \"hello\") nil) (io/print x x)) \"hello\")",
        pass, fail);

    // io/read-file with wrong type should error (^String)
    test_error(interp, "io/read-file wrong type",
        "(handle (signal io/read-file 42) (io/read-file x x))", pass, fail);

    // io/read-file with correct type works
    test_str_interp(interp, "io/read-file correct type",
        "(handle (signal io/read-file \"test.txt\") (io/read-file x x))", pass, fail);

    // libuv fs primitives round-trip and metadata path
    test_str_interp(interp, "fs roundtrip read/write",
        "(block "
        "(define p \"/tmp/omni_fs_roundtrip.txt\") "
        "(define h (fs-open p 577 420)) "
        "(fs-write h \"abc\") "
        "(fs-close h) "
        "(define r (fs-open p 0)) "
        "(define s (fs-read r 16)) "
        "(fs-close r) "
        "(fs-unlink p) "
        "s)",
        pass, fail);

    test_str_interp(interp, "fs-* alias roundtrip read/write",
        "(block "
        "(define p \"/tmp/omni_filesystem_alias_roundtrip.txt\") "
        "(define h (fs-open p 577 420)) "
        "(fs-write h \"alias-ok\") "
        "(fs-close h) "
        "(define r (fs-open p 0)) "
        "(define s (fs-read r 32)) "
        "(fs-close r) "
        "(fs-unlink p) "
        "s)",
        pass, fail);

    test_truthy_interp(interp, "fs-readdir returns array",
        "(array? (fs-readdir \"/tmp\"))", pass, fail);

    test_str_val(interp, "transmission-control alias payload code",
        "(handle (tcp-connect 42 80) "
        "  (raise msg (symbol->string (ref msg 'code))))",
        "io/tcp-connect-host-not-string", pass, fail);

    test_tag_interp(interp, "user-datagram alias socket create/close",
        "(block "
        "(define u (udp-socket)) "
        "(udp-close u))",
        VOID, pass, fail);

    test_str_val(interp, "dns-resolve alias localhost",
        "(await (spawn (lambda () (dns-resolve \"localhost\"))))",
        "127.0.0.1", pass, fail);

    test_str_val(interp, "tls-connect alias payload code",
        "(handle (tls-connect nil \"localhost\" true) "
        "  (raise msg (symbol->string (ref msg 'code))))",
        "io/tls-connect-invalid-tcp-handle", pass, fail);

    test_truthy_interp(interp, "fs stat returns dict",
        "(block "
        "(define p \"/tmp/omni_fs_stat.txt\") "
        "(define h (fs-open p 577 420)) "
        "(fs-write h \"x\") "
        "(fs-close h) "
        "(define st (fs-stat p)) "
        "(fs-unlink p) "
        "(dict? st))",
        pass, fail);

    test_tag_interp(interp, "udp socket create/close",
        "(block "
        "(define u (udp-socket)) "
        "(udp-close u))",
        VOID, pass, fail);

    test_str_interp(interp, "udp loopback roundtrip",
        "(block "
        "(define p (+ 45000 (% (time-ms) 10000))) "
        "(define recv (udp-socket)) "
        "(udp-bind recv \"127.0.0.1\" p) "
        "(define snd (udp-socket)) "
        "(udp-send snd \"127.0.0.1\" p \"ping\") "
        "(define got (udp-recv recv)) "
        "(udp-close snd) "
        "(udp-close recv) "
        "got)",
        pass, fail);

    test_str_interp(interp, "pipe loopback roundtrip",
        "(block "
        "(define f (spawn (lambda () "
        "  (define p (string-append \"/tmp/omni_pipe_\" (number->string (% (time-ms) 1000000)) \".sock\")) "
        "  (define listener (pipe-listen p)) "
        "  (define client (pipe-connect p)) "
        "  (define server (tcp-accept listener)) "
        "  (tcp-write client \"pipe-ok\") "
        "  (define got (tcp-read server)) "
        "  (tcp-close client) "
        "  (tcp-close server) "
        "  (tcp-close listener) "
        "  (fs-unlink p) "
        "  got))) "
        "(await f))",
        pass, fail);

    test_truthy_interp(interp, "process spawn stdio + wait",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"printf omni\"] nil)) "
        "(define out (fs-read (ref p 'stdout) 32)) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= out \"omni\") (= (ref st 'exit-code) 0)))",
        pass, fail);

    test_truthy_interp(interp, "process spawn env override",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"printf $OMNI_PROC_TEST\"] [\"OMNI_PROC_TEST=ok\"])) "
        "(define out (fs-read (ref p 'stdout) 32)) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= out \"ok\") (= (ref st 'exit-code) 0)))",
        pass, fail);

    test_truthy_interp(interp, "process wait in fiber context",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.05\"] nil)) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(= (ref st 'exit-code) 0))",
        pass, fail);

    test_truthy_interp(interp, "process wait requires fiber context",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.05\"] nil)) "
        "(define msg (try (lambda (ignored) (process-wait (ref p 'handle))) (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= (ref st 'exit-code) 0) (string-contains? msg \"requires running fiber context\")))",
        pass, fail);
    run_advanced_process_wait_event_driven_test(interp, pass, fail);

    test_truthy_interp(interp, "process kill terminates child",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 2\"] nil)) "
        "(process-kill (ref p 'handle) 15) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(> (ref st 'term-signal) 0))",
        pass, fail);

    test_truthy_interp(interp, "signal handle/unhandle callback",
        "(block "
        "(define hits {'n 0}) "
        "(define h (signal-handle 10 (lambda (sig) (set! hits 'n (+ (ref hits 'n) 1))))) "
        "(shell \"kill -USR1 $PPID\") "
        "(async-sleep 20) "
        "(signal-unhandle h) "
        "(= (ref hits 'n) 1))",
        pass, fail);
    run_advanced_signal_burst_delivery_test(interp, pass, fail);
    run_advanced_signal_handle_registry_scaling_test(interp, pass, fail);
    run_advanced_signal_dispatch_owner_interp_isolation_test(interp, pass, fail);
    run_advanced_signal_callback_scope_release_test(interp, pass, fail);
    run_advanced_sigint_interrupt_routing_test(interp, pass, fail);
    // Subtype check: Number accepts Integer and Double
    test_nil_interp(interp, "defeffect number arg", "(define [effect] (test/num (^Number n)))", pass, fail);
    test_eq_interp(interp, "effect number int",
        "(handle (signal test/num 42) (test/num x x))", 42, pass, fail);
    test_error(interp, "effect number str",
        "(handle (signal test/num \"bad\") (test/num x x))", pass, fail);
}

fn void run_advanced_union_memory_limit_tests(Interp* interp, int* pass, int* fail) {
    // === UNION PATTERN MATCHING TESTS (Phase 6) ===
    // Define Option locally so this isolated group does not depend on other suites.
    setup(interp, "(define [union] (Option T) None (Some T))");

    // Nullary constructor match
    test_eq_interp(interp, "match None", "(match None (None 0) ((Some x) x))", 0, pass, fail);

    // Constructor with field match
    test_eq_interp(interp, "match Some", "(match (Some 42) (None 0) ((Some x) x))", 42, pass, fail);

    // Result type
    setup(interp, "(define [union] (Result T E) (Ok T) (Err E))");
    test_eq_interp(interp, "match Ok", "(match (Ok 1) ((Ok v) v) ((Err e) (- 0 1)))", 1, pass, fail);
    test_eq_interp(interp, "match Err", "(match (Err 99) ((Ok v) v) ((Err e) e))", 99, pass, fail);

    // Nested constructor match
    test_eq_interp(interp, "match nested Some", "(match (Some (Some 7)) ((Some (Some x)) x) (_ 0))", 7, pass, fail);

    // Wildcard in constructor
    test_eq_interp(interp, "match ctor wildcard", "(match (Ok 42) ((Ok _) 1) ((Err _) 2))", 1, pass, fail);

    // === MEMORY RECLAMATION TESTS ===
    // unsafe-free! on array: releases backing, accessing freed value errors
    test_error(interp, "unsafe-free! array UAF", "(let (a [1 2 3]) (block (unsafe-free! a) a))", pass, fail);
    // unsafe-free! on dict: releases backing, accessing freed value errors
    test_error(interp, "unsafe-free! dict UAF", "(let (d {'a 1 'b 2}) (block (unsafe-free! d) d))", pass, fail);
    // unsafe-free! on non-collection: no-op, returns Void
    test_tag(interp, "unsafe-free! int noop returns Void", "(unsafe-free! 42)", VOID, pass, fail);
    // unsafe-free! on nil: no-op, returns Void
    test_tag(interp, "unsafe-free! nil noop returns Void", "(unsafe-free! nil)", VOID, pass, fail);

    // === RAISED LIMITS TESTS ===
    // Match with 17 clauses (was max 16, now 32)
    test_eq(interp, "match 17 clauses",
        "(match 17 (1 101) (2 102) (3 103) (4 104) (5 105) (6 106) (7 107) (8 108) (9 109) (10 110) (11 111) (12 112) (13 113) (14 114) (15 115) (16 116) (17 117))",
        117, pass, fail);
    // Handle with 9 effect clauses (was max 8, now 16)
    test_eq(interp, "handle 9 effects",
        "(handle (signal e9 42) (e1 v (resolve v)) (e2 v (resolve v)) (e3 v (resolve v)) (e4 v (resolve v)) (e5 v (resolve v)) (e6 v (resolve v)) (e7 v (resolve v)) (e8 v (resolve v)) (e9 v (+ v 1)))",
        43, pass, fail);
    // Large method table (>32 methods, was max 32, now 64)
    setup(interp, "(define [type] TestMT33 (^Integer val))");
    setup(interp, "(define (mt33-f (^(Value 1) n)) 1) (define (mt33-f (^(Value 2) n)) 2) (define (mt33-f (^(Value 3) n)) 3) (define (mt33-f (^(Value 4) n)) 4) (define (mt33-f (^(Value 5) n)) 5) (define (mt33-f (^(Value 6) n)) 6) (define (mt33-f (^(Value 7) n)) 7) (define (mt33-f (^(Value 8) n)) 8)");
    setup(interp, "(define (mt33-f (^(Value 9) n)) 9) (define (mt33-f (^(Value 10) n)) 10) (define (mt33-f (^(Value 11) n)) 11) (define (mt33-f (^(Value 12) n)) 12) (define (mt33-f (^(Value 13) n)) 13) (define (mt33-f (^(Value 14) n)) 14) (define (mt33-f (^(Value 15) n)) 15) (define (mt33-f (^(Value 16) n)) 16)");
    setup(interp, "(define (mt33-f (^(Value 17) n)) 17) (define (mt33-f (^(Value 18) n)) 18) (define (mt33-f (^(Value 19) n)) 19) (define (mt33-f (^(Value 20) n)) 20) (define (mt33-f (^(Value 21) n)) 21) (define (mt33-f (^(Value 22) n)) 22) (define (mt33-f (^(Value 23) n)) 23) (define (mt33-f (^(Value 24) n)) 24)");
    setup(interp, "(define (mt33-f (^(Value 25) n)) 25) (define (mt33-f (^(Value 26) n)) 26) (define (mt33-f (^(Value 27) n)) 27) (define (mt33-f (^(Value 28) n)) 28) (define (mt33-f (^(Value 29) n)) 29) (define (mt33-f (^(Value 30) n)) 30) (define (mt33-f (^(Value 31) n)) 31) (define (mt33-f (^(Value 32) n)) 32)");
    setup(interp, "(define (mt33-f (^(Value 33) n)) 33)");
    test_eq(interp, "method table >32", "(mt33-f 33)", 33, pass, fail);
}

fn void run_advanced_effect_union_limit_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_io_typed_effect_tests(interp, pass, fail);
    run_advanced_union_memory_limit_tests(interp, pass, fail);
}

fn void run_advanced_ffi_system_surface_tests(Interp* interp, int* pass, int* fail) {
    // === FFI TESTS (define [ffi lib] / define [ffi λ]) ===
    // Library declaration
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    test_tag(interp, "ffi lib handle", "libc", FFI_HANDLE, pass, fail);

    // Function bindings — integer functions
    setup(interp, "(define [ffi λ libc] (strlen (^String s)) ^Integer)");
    test_eq(interp, "ffi λ strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi λ strlen empty", "(strlen \"\")", 0, pass, fail);

    setup(interp, "(define [ffi λ libc] (abs (^Integer n)) ^Integer)");
    test_eq(interp, "ffi λ abs", "(abs -42)", 42, pass, fail);
    test_eq(interp, "ffi λ abs zero", "(abs 0)", 0, pass, fail);

    setup(interp, "(define [ffi λ libc] (atoi (^String s)) ^Integer)");
    test_eq(interp, "ffi λ atoi", "(atoi \"12345\")", 12345, pass, fail);
    test_eq(interp, "ffi λ atoi neg", "(atoi \"-99\")", -99, pass, fail);

    // Zero-arg function
    setup(interp, "(define [ffi λ libc] (getpid) ^Integer)");
    test_gt(interp, "ffi λ getpid", "(getpid)", 0, pass, fail);

    // Math library — double functions
    setup(interp, "(define [ffi lib] libm \"libm.so.6\")");
    setup(interp, "(define [ffi λ libm] (sqrt (^Double x)) ^Double)");
    test_eq_double(interp, "ffi λ sqrt", "(sqrt 4.0)", 2.0, pass, fail);
    test_eq_double(interp, "ffi λ sqrt 9", "(sqrt 9.0)", 3.0, pass, fail);

    setup(interp, "(define [ffi λ libm] (pow (^Double x) (^Double y)) ^Double)");
    test_eq_double(interp, "ffi λ pow", "(pow 2.0 10.0)", 1024.0, pass, fail);

    // Mixed int/double (libffi enables this — old FFI couldn't)
    setup(interp, "(define [ffi λ libm] (ldexp (^Double x) (^Integer exp)) ^Double)");
    test_eq_double(interp, "ffi λ ldexp", "(ldexp 1.0 10)", 1024.0, pass, fail);

    // Void return (canonical pointer annotation)
    setup(interp, "(define [ffi λ libc] (free (^Pointer p)))");
    test_truthy(interp, "ffi λ free null (^Pointer)", "(= (type-of (free 0)) 'Void)", pass, fail);
    setup(interp, "(define [ffi λ libc] (free (^Pointer p)))");
    test_truthy(interp, "ffi λ free null (^Pointer alias)", "(= (type-of (free 0)) 'Void)", pass, fail);

    // Error cases
    test_error_contains(interp, "ffi lib bad",
        "(define [ffi lib] bad-lib \"nonexistent_xyz.so\")",
        "dlopen failed for", pass, fail);
    test_error_contains(interp, "ffi lib bad preserves path cause",
        "(define [ffi lib] bad-lib2 \"nonexistent_xyz.so\")",
        "nonexistent_xyz.so", pass, fail);
    setup(interp, "(define [ffi λ libc] (definitely_missing_ffi_symbol (^Integer n)) ^Integer)");
    test_error_contains(interp, "ffi λ missing symbol cause",
        "(definitely_missing_ffi_symbol 1)",
        "dlsym failed for", pass, fail);
    test_error_contains(interp, "ffi λ missing symbol preserves loader cause",
        "(definitely_missing_ffi_symbol 1)",
        "undefined symbol", pass, fail);
    test_error_contains(interp, "ffi λ unsupported parameter annotation",
        "(block (define [ffi lib] bad-ffi-lib \"libc.so.6\") (define [ffi λ bad-ffi-lib] (strlen (^Symbol s)) ^Integer))",
        "unsupported parameter type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ unsupported return annotation",
        "(block (define [ffi lib] bad-ffi-lib2 \"libc.so.6\") (define [ffi λ bad-ffi-lib2] (strlen (^String s)) ^Symbol))",
        "unsupported return type annotation '^Symbol'", pass, fail);

    // === SYSTEM PRIMITIVES TESTS ===
    // random — returns double in [0,1)
    test_truthy(interp, "random range", "(let (r (random)) (and (>= r 0.0) (< r 1.0)))", pass, fail);
    test_truthy(interp, "random is double", "(double? (random))", pass, fail);
    // random-int — returns int in [0,n)
    test_truthy(interp, "random-int range", "(let (r (random-int 10)) (and (>= r 0) (< r 10)))", pass, fail);
    test_truthy(interp, "random-int is int", "(int? (random-int 100))", pass, fail);
    // time — returns epoch seconds (should be > 2020-01-01)
    test_gt(interp, "time epoch", "(time)", 1577836800, pass, fail);
    // time-ms — returns epoch milliseconds (> time * 1000)
    test_gt(interp, "time-ms epoch", "(time-ms)", 1577836800000, pass, fail);
    // getenv — HOME should exist, nonexistent returns nil
    test_str(interp, "getenv HOME", "(getenv \"HOME\")", pass, fail);
    test_nil(interp, "getenv missing", "(getenv \"OMNI_NONEXISTENT_VAR_XYZ\")", pass, fail);
    // shell — captures stdout
    test_str_val(interp, "shell echo", "(shell \"echo hello\")", "hello", pass, fail);
    // sleep — returns void (tiny sleep to not slow tests)
    test_tag_interp(interp, "sleep tiny", "(sleep 0.001)", VOID, pass, fail);
}

fn void run_advanced_ffi_system_long_symbol_tests(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    setup(interp, "(define [ffi λ libc] (ffi_superlong_symbol_name_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_tailmarkerzz (^Integer n)) ^Integer)");
    test_error_contains(interp,
        "ffi λ long symbol name preserves tail",
        "(ffi_superlong_symbol_name_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_tailmarkerzz 7)",
        "tailmarkerzz",
        pass, fail);
}

fn void run_advanced_ffi_system_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_ffi_system_group("advanced-ffi-system-surface",
        &run_advanced_ffi_system_surface_tests, interp, pass, fail);
    run_advanced_ffi_system_selected_exact_group("advanced-ffi-system-long-symbol",
        &run_advanced_ffi_system_long_symbol_tests, interp, pass, fail);
}
