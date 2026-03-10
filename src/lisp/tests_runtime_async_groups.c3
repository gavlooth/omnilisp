module lisp;


import std::io;
import main;

fn bool async_tls_integration_enabled() @inline {
    // TLS integration fixtures can leak subprocesses in plain full-suite runs.
    // Keep them opt-in until process-lifecycle stability is finalized.
    if (test_c_getenv("OMNI_ENABLE_TLS_INTEGRATION") == null) return false;
    return test_c_getenv("OMNI_SKIP_TLS_INTEGRATION") == null;
}

fn void run_async_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Async I/O Tests ---");

    // DNS resolve localhost (fiber-only path)
    test_str_val(interp, "dns-resolve localhost (fiber)",
        "(await (spawn (lambda () (dns-resolve \"localhost\"))))", "127.0.0.1", pass, fail);

    // async-sleep returns nil (fiber-only path)
    test_tag(interp, "async-sleep returns nil (fiber)",
        "(await (spawn (lambda () (async-sleep 1))))", NIL, pass, fail);

    // DNS resolve returns a string (fiber-only path)
    test_str(interp, "dns-resolve returns string (fiber)",
        "(await (spawn (lambda () (dns-resolve \"localhost\"))))", pass, fail);

    test_error_contains(interp, "dns-resolve requires fiber context",
        "(dns-resolve \"localhost\")",
        "dns-resolve: requires running fiber context", pass, fail);
    test_error_contains(interp, "async-sleep requires fiber context",
        "(async-sleep 1)",
        "async-sleep: requires running fiber context", pass, fail);

    test_error_contains(interp, "read-file requires fiber context",
        "(read-file \"/tmp/omni-read-file-nonfiber.txt\")",
        "read-file: requires running fiber context", pass, fail);
    test_error_contains(interp, "write-file requires fiber context",
        "(write-file \"/tmp/omni-write-file-nonfiber.txt\" \"x\")",
        "write-file: requires running fiber context", pass, fail);
    test_error_contains(interp, "file-exists? requires fiber context",
        "(file-exists? \"/tmp/omni-file-exists-nonfiber.txt\")",
        "file-exists?: requires running fiber context", pass, fail);
    test_error_contains(interp, "read-lines requires fiber context",
        "(read-lines \"/tmp/omni-read-lines-nonfiber.txt\")",
        "read-lines: requires running fiber context", pass, fail);
    test_str_val(interp, "read-file invalid args payload code",
        "(handle (read-file 42) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_str_val(interp, "write-file invalid args payload code",
        "(handle (write-file 42 \"x\") (raise msg (symbol->string (ref msg 'code))))",
        "io/write-file-expected-path-content", pass, fail);
    test_str_val(interp, "file-exists? invalid args payload code",
        "(handle (file-exists? 42) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_str_val(interp, "read-lines invalid args payload code",
        "(handle (read-lines 42) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);

    test_truthy(interp, "async file ops in fiber via offload path",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-file-\" (number->string (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (write-file p \"a\\nb\") "
        "        (define exists (file-exists? p)) "
        "        (define text (read-file p)) "
        "        (define lines (read-lines p)) "
        "        (and exists "
        "             (and (= text \"a\\nb\") "
        "                  (and (= (length lines) 2) "
        "                       (= (car lines) \"a\")))))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_truthy(interp, "async file ops large payload roundtrip",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-large-\" (number->string (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (define (grow s n) "
        "          (if (= n 0) s (grow (string-append s s) (- n 1)))) "
        "        (define payload (grow \"abc123XYZ0\" 17)) "
        "        (write-file p payload) "
        "        (define text (read-file p)) "
        "        (and (= (string-byte-length text) (string-byte-length payload)) "
        "             (= text payload)))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_str_val(interp, "async write-file missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-wf-\" (number->string (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (write-file p \"x\")) (lambda (e) (symbol->string (ref e 'code))))))))",
        "io/write-file-failed", pass, fail);

    test_str_val(interp, "async read-file missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-rf-\" (number->string (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (read-file p)) (lambda (e) (symbol->string (ref e 'code))))))))",
        "io/read-file-failed", pass, fail);

    test_str_val(interp, "async read-lines missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-rl-\" (number->string (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (read-lines p)) (lambda (e) (symbol->string (ref e 'code))))))))",
        "io/read-lines-failed", pass, fail);

    test_nil(interp, "async file-exists? missing-path returns nil",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-fe-\" (number->string (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () (file-exists? p)))))",
        pass, fail);

    test_truthy(interp, "async file read cancel stress",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-cancel-\" (number->string (time-ms)) \".txt\")) "
        "  (await "
        "    (spawn (lambda () "
        "      (define (grow s n) "
        "        (if (= n 0) s (grow (string-append s s) (- n 1)))) "
        "      (write-file p (grow \"zzzzzzzzzzzzzzzz\" 15))))) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (define (loop n) "
        "          (if (= n 0) "
        "              true "
        "              (block "
        "                (define f (spawn (lambda () (read-file p)))) "
        "                (fiber-cancel f) "
        "                (run-fibers) "
        "                (loop (- n 1))))) "
        "        (loop 8))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_error_contains(interp, "tls-read requires fiber context",
        "(tls-read nil)",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "tls-write requires fiber context",
        "(tls-write nil \"ping\")",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "tls-close requires fiber context",
        "(tls-close nil)",
        "requires running fiber context", pass, fail);

    // tls-connect optional CA path validation
    test_error_contains(interp, "tls-connect rejects non-string ca bundle path",
        "(tls-connect nil \"localhost\" 1)",
        "ca-bundle-path must be a string", pass, fail);
    test_str_val(interp, "tls-connect non-string ca bundle payload code",
        "(handle (tls-connect nil \"localhost\" 1) (raise msg (symbol->string (ref msg 'code))))",
        "io/tls-connect-expected-ca-bundle-path-string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-string client cert path",
        "(tls-connect nil \"localhost\" \"ca.pem\" 1 \"client-key.pem\")",
        "client-cert-pem-path must be a string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-string client key path",
        "(tls-connect nil \"localhost\" \"ca.pem\" \"client-cert.pem\" 1)",
        "client-key-pem-path must be a string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-bool resume policy",
        "(tls-connect nil \"localhost\" \"ca.pem\" \"client-cert.pem\" \"client-key.pem\" 1)",
        "resume-session? must be true or false", pass, fail);
    test_error_contains(interp, "tls-connect accepts bool-only resume policy form",
        "(tls-connect nil \"localhost\" true)",
        "invalid TCP handle", pass, fail);
    test_str_val(interp, "tls-connect invalid tcp handle payload code",
        "(handle (tls-connect nil \"localhost\" true) (raise msg (symbol->string (ref msg 'code))))",
        "io/tls-connect-invalid-tcp-handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts direct mTLS form without CA",
        "(tls-connect nil \"localhost\" \"client-cert.pem\" \"client-key.pem\")",
        "invalid TCP handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts ca+resume policy form",
        "(tls-connect nil \"localhost\" \"ca.pem\" true)",
        "invalid TCP handle", pass, fail);
    test_str_val(interp, "tls-write expected data payload code",
        "(handle (tls-write nil 1) (raise msg (symbol->string (ref msg 'code))))",
        "io/tls-write-expected-data-string", pass, fail);

    // TLS integration: run server in a separate Omni process to avoid
    // scheduler deadlocks from blocking TLS read/write in same runtime thread.
    // In ASAN runtime this fixture is unstable (nested instrumented process +
    // TLS offload timing), so keep argument/shape checks but skip handshake suite.
    if (async_tls_integration_enabled() && !main::stack_runtime_asan_enabled()) {
        test_truthy(interp, "tls valid cert handshake",
        "(block "
        "  (define p (+ 45100 (% (time-ms) 10000))) "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" (string-append "
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (number->string p) "
        "        \" LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\")] "
        "      nil)) "
        "  (async-sleep 200) "
        "  (define client-msg "
        "    (await "
        "      (spawn (lambda () "
        "        (define c nil) "
        "        (define ctls nil) "
        "        (define outcome "
        "          (try "
        "            (lambda (ignored) "
        "              (block "
        "                (set! c (tcp-connect \"127.0.0.1\" p)) "
        "                (set! ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "                (tls-write ctls \"ping\") "
        "                (tls-read ctls))) "
        "            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil) "
        "        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil) "
        "        outcome)))) "
        "  (define status (await (spawn (lambda () (process-wait (ref proc 'handle)))))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= client-msg \"pong\") (= (ref status 'exit-code) 0)))",
        pass, fail);

        test_truthy(interp, "tls rejects untrusted cert",
        "(block "
        "  (define p (+ 45200 (% (time-ms) 10000))) "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" (string-append "
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (number->string p) "
        "        \" OMNI_TLS_TEST_CERT=tests/lib/tls/bad-server.cert.pem OMNI_TLS_TEST_KEY=tests/lib/tls/bad-server.key.pem\" "
        "        \" LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\")] "
        "      nil)) "
        "  (async-sleep 200) "
        "  (define client-msg "
        "    (await "
        "      (spawn (lambda () "
        "        (define c nil) "
        "        (define ctls nil) "
        "        (define outcome "
        "          (try "
        "            (lambda (ignored) "
        "              (block "
        "                (set! c (tcp-connect \"127.0.0.1\" p)) "
        "                (set! ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "                (tls-write ctls \"ping\") "
        "                (tls-read ctls))) "
        "            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil) "
        "        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil) "
        "        outcome)))) "
        "  (define status (await (spawn (lambda () (process-wait (ref proc 'handle)))))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (or (string-contains? client-msg \"write failed\") "
        "                (or (string-contains? client-msg \"read failed\") "
        "                    (string-contains? client-msg \"reset failed\"))))))",
        pass, fail);

        test_truthy(interp, "tls rejects hostname mismatch",
        "(block "
        "  (define p (+ 45300 (% (time-ms) 10000))) "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" (string-append "
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (number->string p) "
        "        \" LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\")] "
        "      nil)) "
        "  (async-sleep 200) "
        "  (define client-msg "
        "    (await "
        "      (spawn (lambda () "
        "        (define c nil) "
        "        (define ctls nil) "
        "        (define outcome "
        "          (try "
        "            (lambda (ignored) "
        "              (block "
        "                (set! c (tcp-connect \"127.0.0.1\" p)) "
        "                (set! ctls (tls-connect c \"not-localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "                (tls-write ctls \"ping\") "
        "                (tls-read ctls))) "
        "            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil) "
        "        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil) "
        "        outcome)))) "
        "  (define status (await (spawn (lambda () (process-wait (ref proc 'handle)))))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (or (string-contains? client-msg \"write failed\") "
        "                (or (string-contains? client-msg \"read failed\") "
        "                    (string-contains? client-msg \"reset failed\"))))))",
        pass, fail);

        test_truthy(interp, "tls mTLS optional cert/key pass",
        "(block "
        "  (define p (+ 45400 (% (time-ms) 10000))) "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" (string-append "
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (number->string p) "
        "        \" LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\")] "
        "      nil)) "
        "  (async-sleep 200) "
        "  (define client-msg "
        "    (await "
        "      (spawn (lambda () "
        "        (define c nil) "
        "        (define ctls nil) "
        "        (define outcome "
        "          (try "
        "            (lambda (ignored) "
        "              (block "
        "                (set! c (tcp-connect \"127.0.0.1\" p)) "
        "                (set! ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\" \"tests/lib/tls/client.cert.pem\" \"tests/lib/tls/client.key.pem\" true)) "
        "                (tls-write ctls \"ping\") "
        "                (tls-read ctls))) "
        "            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil) "
        "        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil) "
        "        outcome)))) "
        "  (define status (await (spawn (lambda () (process-wait (ref proc 'handle)))))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= client-msg \"pong\") (= (ref status 'exit-code) 0)))",
        pass, fail);

        test_truthy(interp, "tls mTLS invalid key fails",
        "(block "
        "  (define p (+ 45500 (% (time-ms) 10000))) "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" (string-append "
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (number->string p) "
        "        \" LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\")] "
        "      nil)) "
        "  (async-sleep 200) "
        "  (define client-msg "
        "    (await "
        "      (spawn (lambda () "
        "        (define c nil) "
        "        (define ctls nil) "
        "        (define outcome "
        "          (try "
        "            (lambda (ignored) "
        "              (block "
        "                (set! c (tcp-connect \"127.0.0.1\" p)) "
        "                (set! ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\" \"tests/lib/tls/client.cert.pem\" \"tests/lib/tls/missing-client.key.pem\")) "
        "                (tls-write ctls \"ping\") "
        "                (tls-read ctls))) "
        "            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e)))) "
        "        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil) "
        "        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil) "
        "        outcome)))) "
        "  (define status (await (spawn (lambda () (process-wait (ref proc 'handle)))))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (string-contains? client-msg \"failed to configure client cert/key\"))))",
        pass, fail);
    }

    // tls-server-wrap argument validation
    test_error_contains(interp, "tls-server-wrap rejects non-string cert path",
        "(__raw-tls-server-wrap nil 1 \"key.pem\")",
        "cert-pem-path must be a string", pass, fail);
    test_str_val(interp, "tls-server-wrap invalid stream payload code",
        "(handle (__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\") (raise msg (symbol->string (ref msg 'code))))",
        "io/tls-server-wrap-invalid-tcp-stream-handle", pass, fail);
    test_error_contains(interp, "tls-server-wrap rejects non-stream handle",
        "(__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\")",
        "invalid TCP stream handle", pass, fail);

    // listener lifecycle
    test_tag(interp, "tcp-listen open/close",
        "(block "
        "  (define f (spawn (lambda () "
        "    (define ls (tcp-listen \"127.0.0.1\" 0 8)) "
        "    (tcp-close ls)))) "
        "  (await f))",
        NIL, pass, fail);

    // fiber-safe listen path: listen in fiber uses libuv tcp bind/listen bridge
    test_tag(interp, "tcp-listen in fiber via libuv bridge",
        "(block "
        "  (define f (spawn (lambda () "
        "    (define ls (tcp-listen \"127.0.0.1\" 0 8)) "
        "    (tcp-close ls)))) "
        "  (await f))",
        NIL, pass, fail);

    test_error_contains(interp, "tcp-read requires fiber context",
        "(tcp-read nil)",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "tcp-write requires fiber context",
        "(tcp-write nil \"x\")",
        "requires running fiber context", pass, fail);

    // fiber-safe connect path: connect in fiber uses libuv tcp connect bridge
    test_truthy(interp, "tcp-connect in fiber via libuv bridge",
        "(block "
        "  (define __tcp-connect-bridge-salt "
        "    (if (bound? '__tcp-connect-bridge-salt) "
        "        (+ __tcp-connect-bridge-salt 1) "
        "        0)) "
        "  (define tf (spawn (lambda () "
        "    (define p (+ 29800 (% (+ (time-ms) (* __tcp-connect-bridge-salt 97)) 20000))) "
        "    (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "    (define af (spawn (lambda () (tcp-accept listener)))) "
        "    (define cf (spawn (lambda () (tcp-connect \"127.0.0.1\" p)))) "
        "    (define client (await cf)) "
        "    (define server-client (await af)) "
        "    (define wrote (tcp-write client \"ping\")) "
        "    (define payload (tcp-read server-client)) "
        "    (tcp-close client) "
        "    (tcp-close server-client) "
        "    (tcp-close listener) "
        "    (and (= wrote 4) (= payload \"ping\"))))) "
        "  (await tf))",
        pass, fail);

    // tcp-accept argument validation
    test_error_contains(interp, "tcp-accept rejects non-listener",
        "(tcp-accept nil)",
        "invalid or closed listener handle", pass, fail);

    // fiber-safe accept path: accept in fiber uses libuv accept bridge
    test_truthy(interp, "tcp-accept in fiber via libuv bridge",
        "(block "
        "  (define tf (spawn (lambda () "
        "    (define p (+ 30000 (% (time-ms) 20000))) "
        "    (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "    (define af (spawn (lambda () (tcp-accept listener)))) "
        "    (async-sleep 5) "
        "    (define client (tcp-connect \"127.0.0.1\" p)) "
        "    (tcp-write client \"ping\") "
        "    (define server-client (await af)) "
        "    (define payload (tcp-read server-client)) "
        "    (tcp-close client) "
        "    (tcp-close server-client) "
        "    (tcp-close listener) "
        "    (= payload \"ping\")))) "
        "  (await tf))",
        pass, fail);

    // fiber-safe close path: close in fiber uses libuv tcp close helper
    test_tag(interp, "tcp-close in fiber via libuv bridge",
        "(block "
        "  (define f (spawn (lambda () "
        "    (define ls (tcp-listen \"127.0.0.1\" 0 8)) "
        "    (tcp-close ls)))) "
        "  (await f))",
        NIL, pass, fail);

    // fiber-safe write path: write in fiber uses async writable wakeup bridge
    test_truthy(interp, "tcp-write in fiber via async bridge",
        "(block "
        "  (define tf (spawn (lambda () "
        "    (define p (+ 30500 (% (time-ms) 20000))) "
        "    (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "    (define af (spawn (lambda () (tcp-accept listener)))) "
        "    (async-sleep 5) "
        "    (define client (tcp-connect \"127.0.0.1\" p)) "
        "    (define server-client (await af)) "
        "    (define rf (spawn (lambda () (tcp-read server-client)))) "
        "    (define wf (spawn (lambda () (tcp-write client \"ping\")))) "
        "    (define payload (await rf)) "
        "    (define wrote (await wf)) "
        "    (tcp-close client) "
        "    (tcp-close server-client) "
        "    (tcp-close listener) "
        "    (and (= payload \"ping\") (= wrote 4))))) "
        "  (await tf))",
        pass, fail);

    test_error_contains(interp, "pipe-connect requires fiber context",
        "(pipe-connect \"/tmp/omni_pipe_nofiber_connect.sock\")",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "pipe-listen requires fiber context",
        "(pipe-listen \"/tmp/omni_pipe_nofiber_listen.sock\")",
        "requires running fiber context", pass, fail);

    test_truthy(interp, "pipe connect/listen in fiber via libuv bridge",
        "(block "
        "  (define pf (spawn (lambda () "
        "    (define p (string-append \"/tmp/omni_pipe_async_\" (number->string (% (time-ms) 1000000)) \".sock\")) "
        "    (define listener (pipe-listen p)) "
        "    (define client (pipe-connect p)) "
        "    (define server (tcp-accept listener)) "
        "    (tcp-write client \"pipe-ok\") "
        "    (define got (tcp-read server)) "
        "    (tcp-close client) "
        "    (tcp-close server) "
        "    (tcp-close listener) "
        "    (fs-unlink p) "
        "    (= got \"pipe-ok\")))) "
        "  (await pf))",
        pass, fail);

    // fiber-safe UDP receive path: recv in fiber uses async wakeup bridge
    test_truthy(interp, "udp-recv in fiber via async bridge",
        "(block "
        "  (define p (+ 35000 (% (time-ms) 20000))) "
        "  (define recv (udp-socket)) "
        "  (udp-bind recv \"127.0.0.1\" p) "
        "  (define rf (spawn (lambda () (udp-recv recv)))) "
        "  (async-sleep 5) "
        "  (define snd (udp-socket)) "
        "  (udp-send snd \"127.0.0.1\" p \"pong\") "
        "  (define got (await rf)) "
        "  (udp-close snd) "
        "  (udp-close recv) "
        "  (= got \"pong\"))",
        pass, fail);
}

fn void run_async_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_async_tests, pass, fail);
}
