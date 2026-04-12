module lisp;

import std::io;
import std::core::mem;

alias AdvancedFfiSystemGroupFn = fn void(Interp* interp, int* pass, int* fail);

int g_double_release_finalizer_calls = 0;

fn void ffi_double_release_test_finalizer(void* payload) {
    g_double_release_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn bool ffi_test_error_contains(Value* value, char[] needle) {
    if (value == null || value.tag != ERROR || value.str_chars == null) return false;
    char[] msg = value.str_chars[:value.str_len];
    if (needle.len > msg.len) return false;
    for (usz i = 0; i <= msg.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (msg[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

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
    run_advanced_process_wait_event_driven_test_impl(interp, pass, fail);
}

fn void run_advanced_process_handle_concurrent_reuse_test(Interp* interp, int* pass, int* fail) {
    run_advanced_process_handle_concurrent_reuse_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_burst_delivery_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_burst_delivery_test_impl(interp, pass, fail);
}

fn void run_advanced_sigint_interrupt_routing_test(Interp* interp, int* pass, int* fail) {
    run_advanced_sigint_interrupt_routing_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_handle_registry_scaling_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_handle_registry_scaling_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_dispatch_owner_interp_isolation_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_dispatch_owner_interp_isolation_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_callback_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_callback_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_register_invoke_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_register_invoke_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_unhandle_idempotence_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_unhandle_idempotence_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_handle_isolation_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_handle_isolation_test_impl(interp, pass, fail);
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

    test_str_interp(interp, "fs-* second roundtrip read/write",
        "(block "
        "(define p \"/tmp/omni_fs_second_roundtrip.txt\") "
        "(define h (fs-open p 577 420)) "
        "(fs-write h \"fs-ok\") "
        "(fs-close h) "
        "(define r (fs-open p 0)) "
        "(define s (fs-read r 32)) "
        "(fs-close r) "
        "(fs-unlink p) "
        "s)",
        pass, fail);
    test_nil_interp(interp, "filesystem-open alias removed", "(bound? 'filesystem-open)", pass, fail);
    test_nil_interp(interp, "filesystem-read-directory alias removed", "(bound? 'filesystem-read-directory)", pass, fail);

    test_truthy_interp(interp, "fs-readdir returns array",
        "(array? (fs-readdir \"/tmp\"))", pass, fail);
    test_error_contains(interp, "fs-open rejects flags out of range",
        "(fs-open \"/tmp/omni_fs_bad_flags.txt\" 9223372036854775807)",
        "flags out of range", pass, fail);
    test_error_contains(interp, "fs-open rejects mode out of range",
        "(fs-open \"/tmp/omni_fs_bad_mode.txt\" 0 9223372036854775807)",
        "mode out of range", pass, fail);

    test_str_val(interp, "transmission-control alias payload code",
        "(handle (tcp-connect 42 80) "
        "  (raise msg (String (ref msg 'code))))",
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
        "  (raise msg (String (ref msg 'code))))",
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

    {
        Value* fs_handle_v = make_fs_handle(interp, 987654322);
        Value* tcp_handle_v = make_tcp_stream_handle(interp, -1);
        Value* udp_handle_v = make_udp_handle(interp, -1);
        bool ok = fs_handle_v != null &&
            fs_handle_v.tag != ERROR &&
            get_tcp_handle(fs_handle_v) == null &&
            get_udp_handle(fs_handle_v) == null &&
            get_tls_handle(fs_handle_v) == null &&
            get_fs_handle(tcp_handle_v) == null &&
            get_fs_handle(udp_handle_v) == null;
        FsHandle* fs_handle = get_fs_handle(fs_handle_v);
        if (fs_handle != null) {
            fs_handle.open = false;
            fs_handle.fd = -1;
        }
        TcpHandle* tcp_handle = get_tcp_handle(tcp_handle_v);
        if (tcp_handle != null) {
            tcp_handle.connected = false;
            tcp_handle.fd = -1;
        }
        UdpHandle* udp_handle = get_udp_handle(udp_handle_v);
        if (udp_handle != null) {
            udp_handle.open = false;
            udp_handle.fd = -1;
        }
        if (ok) {
            emit_pass_literal("[PASS] ffi handle getters reject mismatched handle names");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi handle getters reject mismatched handle names");
            (*fail)++;
        }
    }
    {
        Value* fh_value = make_fs_handle(interp, 987654321);
        FsHandle* fh = get_fs_handle(fh_value);
        Value*[1] close_args = { fh_value };
        Value* close_result = prim_fs_close(close_args[..], null, interp);
        bool close_failed = close_result != null && close_result.tag == ERROR;
        bool handle_preserved = fh != null && fh.open && fh.fd == 987654321;
        if (fh != null) {
            fh.open = false;
            fh.fd = -1;
        }
        if (close_failed && handle_preserved) {
            emit_pass_literal("[PASS] fs-close failure preserves live handle state");
            (*pass)++;
        } else {
            io::printn("[FAIL] fs-close failure preserves live handle state");
            (*fail)++;
        }
    }
    test_error_contains(interp, "fs-close invalidates subsequent read",
        "(block "
        "(define p \"/tmp/omni_fs_close_invalidates_read.txt\") "
        "(define h (fs-open p 577 420)) "
        "(fs-write h \"x\") "
        "(fs-close h) "
        "(fs-read h 1))",
        "fs-read: invalid or closed handle", pass, fail);

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
        "  (define p (string-append \"/tmp/omni_pipe_\" (String (% (time-ms) 1000000)) \".sock\")) "
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

    test_str_val(interp, "process-spawn invalid cmd payload code",
        "(handle (process-spawn 42 nil nil) "
        "  (raise msg (String (ref msg 'code))))",
        "io/process-spawn-cmd-not-string", pass, fail);
    test_str_val(interp, "process-kill invalid sig payload code",
        "(handle (process-kill nil \"TERM\") "
        "  (raise msg (String (ref msg 'code))))",
        "io/process-kill-sig-not-integer", pass, fail);

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
    run_advanced_process_handle_concurrent_reuse_test(interp, pass, fail);

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
    run_advanced_memory_reclamation_tests(interp, pass, fail);

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
    test_truthy(interp, "foreign-describe uses array capabilities metadata",
        "(let (m (foreign-describe libc)) "
        "  (and (= (ref m 'type) 'ForeignHandle) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'library) "
        "       (= (ref m 'name) \"libc.so.6\") "
        "       (array? (ref m 'capabilities))"
        "       (= (length (ref m 'capabilities)) 3)"
        "       (= (ref (ref m 'capabilities) 0) 'load)"
        "       (= (ref (ref m 'capabilities) 1) 'resolve)"
        "       (= (ref (ref m 'capabilities) 2) 'reflect)))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects non-releasable library handle",
        "(foreign-release libc)",
        "handle is not releasable",
        pass, fail);
    {
        bool saved_force = g_env_force_barrier_promote_failure;
        g_env_force_barrier_promote_failure = true;
        test_error_contains(interp, "ffi lib registration failure releases constructed wrapper",
            "(define [ffi lib] ffi-lib-fail \"libc.so.6\")",
            "ffi lib: out of memory",
            pass, fail);
        g_env_force_barrier_promote_failure = saved_force;
    }
    setup(interp, "(define [ffi lib] libc-recover \"libc.so.6\")");
    test_tag(interp, "ffi lib registration still works after failed wrapper release",
        "libc-recover", FFI_HANDLE, pass, fail);

    // Function bindings — integer functions
    setup(interp, "(define [ffi λ libc] (strlen (^String s)) ^Integer)");
    test_eq(interp, "ffi λ strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi λ strlen empty", "(strlen \"\")", 0, pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata after call",
        "(block "
        "(strlen \"hello\") "
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect))))",
        pass, fail);

    setup(interp,
        "(define [ffi module] libc-grouped \"libc.so.6\" "
        "  (strlen (^String s)) ^Integer "
        "  (abs (^Integer n)) ^Integer "
        "  (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose} "
        "  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)");
    test_eq(interp, "ffi module grouped strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi module grouped abs", "(abs -7)", 7, pass, fail);
    test_error_contains(interp, "ffi module grouped fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle",
        pass, fail);
    test_truthy(interp, "ffi module grouped foreign-describe callable metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "ffi module grouped ForeignHandle return metadata",
        "(let (r (ref (foreign-describe fopen) 'returns)) "
        "  (and (= (ref r 'type) 'ForeignHandle) "
        "       (= (ref r 'name) 'File) "
        "       (= (ref r 'ownership) 'owned) "
        "       (= (ref r 'finalizer) 'fclose)))",
        pass, fail);

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

    setup(interp, "(define [ffi λ libc] (strtol (^String s) (^ForeignHandle endptr) (^Integer base)) ^Integer)");
    test_eq(interp, "ffi λ strtol mixed scalar foreign handle", "(strtol \"12345\" nil 10)", 12345, pass, fail);

    setup(interp, "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^String)");
    test_str_val(interp, "ffi λ string return copies C string", "(strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi λ string return null maps to nil", "(strchr \"hello\" 120)", pass, fail);

    // Void return (canonical ForeignHandle annotation)
    setup(interp, "(define [ffi λ libc] (free (^ForeignHandle p)))");
    test_truthy(interp, "ffi λ free null (^ForeignHandle)", "(= (type-of (free nil)) 'Void)", pass, fail);
    test_error_contains(interp, "ffi λ free rejects integer address (^ForeignHandle)",
        "(free 0)",
        "argument 1 expected ForeignHandle, got Integer", pass, fail);

    // Error cases
    test_error_contains(interp, "ffi λ strlen rejects non-string arg",
        "(strlen 123)",
        "argument 1 expected String, got Integer", pass, fail);
    test_error_contains(interp, "ffi λ abs rejects boolean arg",
        "(abs false)",
        "argument 1 expected Integer, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ sqrt rejects boolean arg",
        "(sqrt false)",
        "argument 1 expected Double, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ free rejects string arg",
        "(free \"hello\")",
        "argument 1 expected ForeignHandle, got String", pass, fail);
    test_error_contains(interp, "ffi λ exact arity rejects extra args",
        "(getpid 1)",
        "arity mismatch: expected 0 argument(s), got 1", pass, fail);
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
    test_error_contains(interp, "ffi λ legacy Pointer annotation is rejected",
        "(block (define [ffi lib] bad-ffi-lib3 \"libc.so.6\") (define [ffi λ bad-ffi-lib3] (free (^Pointer p))))",
        "unsupported parameter type annotation '^Pointer'", pass, fail);
    test_error_contains(interp, "ffi λ unsupported return annotation",
        "(block (define [ffi lib] bad-ffi-lib2 \"libc.so.6\") (define [ffi λ bad-ffi-lib2] (strlen (^String s)) ^Symbol))",
        "unsupported return type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ variadic declarations are rejected",
        "(block (define [ffi lib] libc-var \"libc.so.6\") (define [ffi λ libc-var variadic] (printf (^String fmt)) ^Integer))",
        "variadic bindings are not supported yet", pass, fail);
    test_error_contains(interp, "ffi λ Boolean requires real booleans",
        "(block (define [ffi λ libc] (abs (^Boolean flag)) ^Integer) (abs 1))",
        "argument 1 expected Boolean, got Integer", pass, fail);
    test_error_contains(interp, "ffi module grouped rejects missing return annotation",
        "(define [ffi module] bad-grouped \"libc.so.6\" (strlen (^String s)))",
        "expected return type annotation after [ffi module] function signature", pass, fail);

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

    run_advanced_uv_timer_callback_register_invoke_test(interp, pass, fail);
    run_advanced_uv_timer_callback_unhandle_idempotence_test(interp, pass, fail);
    run_advanced_uv_timer_callback_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_handle_isolation_test(interp, pass, fail);
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

fn void run_advanced_ffi_system_foreign_handle_metadata_dict_tests(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    setup(interp, "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})");
    test_tag(interp, "ffi dict fopen return is foreign handle", "(fopen \"/etc/hosts\" \"r\")", FFI_HANDLE, pass, fail);
    test_truthy(interp, "foreign-describe FFI return handle signature metadata",
        "(block "
        "(define m (foreign-describe fopen)) "
        "(define r (ref m 'returns)) "
        "(and (= (ref r 'type) 'ForeignHandle) "
        "     (= (ref r 'abi-type) 'ptr) "
        "     (= (ref r 'name) 'File) "
        "     (= (ref r 'ownership) 'owned) "
        "     (= (ref r 'nullability) 'nullable) "
        "     (= (ref r 'finalizer) 'fclose)))",
        pass, fail);
    test_truthy(interp, "foreign-describe owned return handle metadata",
        "(block "
        "(define h (fopen \"/etc/hosts\" \"r\")) "
        "(define m (foreign-describe h)) "
        "(define ok (and (= (ref m 'runtime) 'c-abi) "
        "                (= (ref m 'kind) 'opaque) "
        "                (= (ref m 'name) 'File) "
        "                (= (ref m 'ownership) 'owned) "
        "                (array? (ref m 'capabilities)) "
        "                (= (ref (ref m 'capabilities) 0) 'release))) "
        "ok)",
        pass, fail);
    test_truthy(interp, "foreign-release closes owned return handle once",
        "(block "
        "(define h (fopen \"/etc/hosts\" \"r\")) "
        "(define before (foreign-describe h)) "
        "(foreign-release h) "
        "(foreign-release h) "
        "(define after (foreign-describe h)) "
        "(and (ref before 'live) "
        "     (nil? (ref after 'live)) "
        "     (not (= (ref after 'ownership) 'owned)) "
        "     (= (length (ref after 'capabilities)) 0)))",
        pass, fail);
    test_truthy(interp, "foreign-describe owned return handle includes release capability and runtime",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'kind) 'opaque)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (ref m 'name) 'File)"
        "         (array? (ref m 'capabilities))"
        "         (= (length (ref m 'capabilities)) 1)"
        "         (= (ref (ref m 'capabilities) 0) 'release))))",
        pass, fail);
    {
        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        test_error_contains(interp, "ffi owned return handle wrapper alloc failure finalizes return",
            "(fopen \"/etc/hosts\" \"r\")",
            "ffi: out of memory while wrapping return ForeignHandle",
            pass, fail);
        g_boundary_force_alloc_value_fail_nth = saved_force;
    }
    {
        Value* bad_library_handle = make_ffi_handle_ex_with_descriptor(
            interp,
            null,
            "bad-library-release",
            null,
            false,
            FOREIGN_RUNTIME_C_ABI,
            FOREIGN_HANDLE_LIBRARY,
            FOREIGN_CAP_RELEASE,
            true);
        bool setup_ok = bad_library_handle != null && bad_library_handle.tag == FFI_HANDLE && bad_library_handle.ffi_val != null;
        bool release_rejected = false;
        if (setup_ok) {
            Value*[] args = { bad_library_handle };
            Value* released = prim_foreign_release(args, null, interp);
            release_rejected = ffi_test_error_contains(released, "handle is not releasable");
        }
        if (setup_ok && release_rejected) {
            emit_pass_literal("[PASS] ffi release rejects malformed c-abi library descriptor with release capability");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi release rejects malformed c-abi library descriptor with release capability");
            (*fail)++;
        }
    }
    {
        Value* bad_opaque_handle = make_ffi_handle_ex_with_descriptor(
            interp,
            (void*)0x1234,
            "bad-opaque-release",
            null,
            false,
            FOREIGN_RUNTIME_C_ABI,
            FOREIGN_HANDLE_OPAQUE,
            FOREIGN_CAP_RELEASE,
            false);
        bool setup_ok = bad_opaque_handle != null && bad_opaque_handle.tag == FFI_HANDLE && bad_opaque_handle.ffi_val != null;
        bool release_rejected = false;
        if (setup_ok) {
            Value*[] args = { bad_opaque_handle };
            Value* before = prim_foreign_describe(args, null, interp);
            Value* ownership_key = make_symbol(interp, interp.symbols.intern("ownership"));
            Value* capabilities_key = make_symbol(interp, interp.symbols.intern("capabilities"));
            Value* ownership = before != null && before.tag == HASHMAP && before.hashmap_val != null
                ? hashmap_get(before.hashmap_val, ownership_key)
                : null;
            Value* capabilities = before != null && before.tag == HASHMAP && before.hashmap_val != null
                ? hashmap_get(before.hashmap_val, capabilities_key)
                : null;
            bool reflected_release = false;
            if (capabilities != null && capabilities.tag == ARRAY && capabilities.array_val != null &&
                capabilities.array_val.length > 0) {
                Value* first_capability = capabilities.array_val.items[0];
                reflected_release = first_capability != null && first_capability.tag == SYMBOL &&
                    first_capability.sym_val == interp.symbols.intern("release");
            }
            Value* released = prim_foreign_release(args, null, interp);
            release_rejected = ffi_test_error_contains(released, "handle is not releasable") &&
                before != null &&
                before.tag == HASHMAP &&
                before.hashmap_val != null &&
                bad_opaque_handle.ffi_val.lib_handle == (void*)0x1234 &&
                (ownership == null || ownership.tag != SYMBOL || ownership.sym_val != interp.symbols.intern("owned")) &&
                !reflected_release;
        }
        if (setup_ok && release_rejected) {
            emit_pass_literal("[PASS] ffi release rejects release-capability handle without release authority");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi release rejects release-capability handle without release authority");
            (*fail)++;
        }
    }

    setup(interp,
        "(block "
        "  (define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'name File 'ownership borrowed 'nullability non-null}) "
        "  (define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file))))");
    test_error_contains(interp, "foreign-release rejects borrowed FFI return handle",
        "(block"
        " (define h (fopen \"/etc/hosts\" \"r\"))"
        " (fclose h)"
        " (foreign-release h))",
        "handle is not releasable",
        pass, fail);
    {
        int* payload = (int*)mem::malloc(int.sizeof);
        int* before_release_payload = payload;
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0xBEEF;
        }
        g_double_release_finalizer_calls = 0;
        Value* mixed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "mixed-release-test", &ffi_double_release_test_finalizer, true, false)
            : null;
        bool setup_ok = payload_ok && mixed != null && mixed.tag == FFI_HANDLE && mixed.ffi_val != null &&
            mixed.ffi_val.free_lib_handle == false;
        bool release_ok = false;
        if (setup_ok) {
            Value*[] args = { mixed };
            Value* released = prim_foreign_release(args, null, interp);
            release_ok = released != null && released.tag != ERROR &&
                g_double_release_finalizer_calls == 1 &&
                (before_release_payload != null) && (*before_release_payload == 0);
        }
        if (setup_ok && release_ok) {
            emit_pass_literal("[PASS] ffi handle mixed finalizer/free construction is normalized before release");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi handle mixed finalizer/free construction is normalized before release");
            (*fail)++;
        }
        if (payload != null) {
            mem::free(payload);
        }
    }
    test_truthy(interp, "foreign-describe borrowed return handle has no release capability",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (length (ref m 'capabilities)) 0))))",
        pass, fail);
    setup(interp,
        "(define [ffi λ libc] (fopen (^String path) (^String mode))"
        " ^{'name File 'ownership manual})");
    test_truthy(interp, "foreign-describe manual return handle preserves manual ownership",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (let (m (foreign-describe h))"
        "    (and (= (ref m 'type) 'ForeignHandle)"
        "         (= (ref m 'runtime) 'c-abi)"
        "         (= (ref m 'ownership) 'manual)"
        "         (= (length (ref m 'capabilities)) 0))))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects manual return handle",
        "(let (h (fopen \"/etc/hosts\" \"r\"))"
        "  (foreign-release h))",
        "handle is not releasable",
        pass, fail);

    setup(interp, "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)))");
    test_error_contains(interp, "ffi dict fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle", pass, fail);

    setup(interp, "(block (define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose}) (define [ffi λ libc] (free (^{'name SqliteDb 'ownership borrowed 'nullability non-null} p))))");
    test_error_contains(interp, "ffi dict wrong family is rejected",
        "(block (define h (fopen \"/etc/hosts\" \"r\")) (free h))",
        "expected ForeignHandle SqliteDb", pass, fail);

    test_error_contains(interp, "ffi dict unknown key is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose 'bogus true})",
        "unsupported return type annotation metadata dictionary", pass, fail);
    test_error_contains(interp, "ffi dict invalid ownership value is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership lease 'finalizer fclose})",
        "unsupported return type annotation metadata dictionary", pass, fail);
    test_error_contains(interp, "ffi dict owned without finalizer is rejected",
        "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned})",
        "unsupported return type annotation metadata dictionary", pass, fail);
}

fn void run_advanced_ffi_system_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_ffi_system_group("advanced-ffi-system-surface",
        &run_advanced_ffi_system_surface_tests, interp, pass, fail);
    run_advanced_ffi_system_group("advanced-ffi-system-foreign-handle-metadata-dict",
        &run_advanced_ffi_system_foreign_handle_metadata_dict_tests, interp, pass, fail);
    run_advanced_ffi_system_selected_exact_group("advanced-ffi-system-long-symbol",
        &run_advanced_ffi_system_long_symbol_tests, interp, pass, fail);
}
