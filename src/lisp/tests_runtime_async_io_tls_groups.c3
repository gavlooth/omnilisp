module lisp;

import std::io;
import main;

fn bool async_tls_integration_enabled() @inline {
    // TLS integration fixtures can leak subprocesses in plain full-suite runs.
    // Keep them opt-in until process-lifecycle stability is finalized.
    if (test_c_getenv("OMNI_ENABLE_TLS_INTEGRATION") == null) return false;
    return test_c_getenv("OMNI_SKIP_TLS_INTEGRATION") == null;
}

fn bool dns_test_value_equals(Value* value, char[] expected) {
    if (value == null || !is_string(value)) return false;
    if (value.str_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (value.str_chars[i] != expected[i]) return false;
    }
    return true;
}

fn bool dns_test_error_contains(Value* value, char[] expected) {
    if (value == null || value.tag != ERROR || value.str_chars == null) return false;
    if (expected.len > value.str_len) return false;
    for (usz i = 0; i <= value.str_len - expected.len; i++) {
        bool match = true;
        for (usz j = 0; j < expected.len; j++) {
            if (value.str_chars[i + j] != expected[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn Value* dns_test_nul_string(Interp* interp, char[] prefix, char[] suffix) {
    char[256] bytes;
    if (prefix.len + suffix.len + 1 > bytes.len) return null;
    usz n = 0;
    for (usz i = 0; i < prefix.len; i++) bytes[n++] = prefix[i];
    bytes[n++] = (char)0;
    for (usz i = 0; i < suffix.len; i++) bytes[n++] = suffix[i];
    return make_string(interp, bytes[0:n]);
}

fn void run_async_dns_c_api_string_boundary_test(Interp* interp, int* pass, int* fail) {
    Value* bad_host = dns_test_nul_string(interp, "local", "host");
    Value*[1] args = { bad_host };
    bool ok = bad_host != null &&
        bad_host.tag == STRING &&
        dns_test_error_contains(prim_dns_resolve(args[..], null, interp), "hostname contains embedded NUL byte");
    if (ok) {
        emit_pass_literal("[PASS] dns-resolve rejects embedded NUL hostname before native call");
        (*pass)++;
    } else {
        io::printn("[FAIL] dns-resolve rejects embedded NUL hostname before native call");
        (*fail)++;
    }
}

fn void run_async_dns_render_tests(Interp* interp, int* pass, int* fail) {
    void* ai4 = null;
    void* ai6 = null;
    bool resolved4 = resolve_host_and_port("127.0.0.1", 0, &ai4);
    bool resolved6 = resolve_host_and_port("::1", 0, &ai6);
    defer if (ai4 != null) c_freeaddrinfo(ai4);
    defer if (ai6 != null) c_freeaddrinfo(ai6);

    Value* rendered4 = resolved4 ? dns_result_to_ip_string(interp, ai4) : null;
    Value* rendered6 = resolved6 ? dns_result_to_ip_string(interp, ai6) : null;
    if (resolved4 &&
        resolved6 &&
        dns_test_value_equals(rendered4, "127.0.0.1") &&
        dns_test_value_equals(rendered6, "::1")) {
        emit_pass_literal("[PASS] dns-resolve renders IPv4 and IPv6 addrinfo payloads");
        (*pass)++;
    } else {
        io::printn("[FAIL] dns-resolve renders IPv4 and IPv6 addrinfo payloads");
        (*fail)++;
    }
}

fn void run_async_tests_io_tls(Interp* interp, int* pass, int* fail) {
    run_async_dns_render_tests(interp, pass, fail);
    run_async_dns_c_api_string_boundary_test(interp, pass, fail);

    // DNS resolve localhost (fiber-only path)
    test_str_val(interp, "dns-resolve localhost (fiber)",
        "(await (spawn (lambda () (dns-resolve \"localhost\"))))", "127.0.0.1", pass, fail);

    // async-sleep returns void (fiber-only path)
    test_tag(interp, "async-sleep returns void (fiber)",
        "(await (spawn (lambda () (async-sleep 1))))", VOID, pass, fail);
    test_error_contains(interp, "async-sleep rejects zero duration",
        "(await (spawn (lambda () (async-sleep 0))))",
        "async-sleep: ms must be positive", pass, fail);
    test_error_contains(interp, "async-sleep rejects negative duration",
        "(await (spawn (lambda () (async-sleep -1))))",
        "async-sleep: ms must be positive", pass, fail);
    test_error_contains(interp, "async-sleep rejects too-large duration",
        "(await (spawn (lambda () (async-sleep 4294968))))",
        "async-sleep: ms must be positive", pass, fail);

    // DNS resolve returns a string (fiber-only path)
    test_str(interp, "dns-resolve returns string (fiber)",
        "(await (spawn (lambda () (dns-resolve \"localhost\"))))", pass, fail);

    test_error_contains(interp, "dns-resolve requires fiber context",
        "(dns-resolve \"localhost\")",
        "dns-resolve: requires running fiber context", pass, fail);
    test_error_contains(interp, "async-sleep requires fiber context",
        "(async-sleep 1)",
        "async-sleep: requires running fiber context", pass, fail);
    test_error_contains(interp, "tls-connect rejects overlong hostname before native offload",
        "(tls-connect nil (string-repeat \"a\" 256))",
        "tls-connect: hostname must be <= 255 bytes", pass, fail);

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
        "(handle (read-file 42) (raise msg (String (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_str_val(interp, "write-file invalid args payload code",
        "(handle (write-file 42 \"x\") (raise msg (String (ref msg 'code))))",
        "io/write-file-expected-path-content", pass, fail);
    test_str_val(interp, "file-exists? invalid args payload code",
        "(handle (file-exists? 42) (raise msg (String (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_str_val(interp, "read-lines invalid args payload code",
        "(handle (read-lines 42) (raise msg (String (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_tag(interp, "write-file returns Void on success",
        "(block "
        "  (define p (string-append \"/tmp/omni-write-file-void-\" (String (time-ms)) \".txt\")) "
        "  (define result (await (spawn (lambda () (write-file p \"ok\"))))) "
        "  (fs-unlink p) "
        "  result)",
        VOID, pass, fail);

    test_truthy(interp, "async file ops in fiber via offload path",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-file-\" (String (time-ms)) \".txt\")) "
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

    test_truthy(interp, "async read-lines empty file returns empty list",
        "(block "
        "  (define p (string-append \"/tmp/omni-read-lines-empty-\" (String (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (write-file p \"\") "
        "        (define lines (read-lines p)) "
        "        (= (length lines) 0))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_truthy(interp, "async read-lines preserves final unterminated line",
        "(block "
        "  (define p (string-append \"/tmp/omni-read-lines-no-trailing-\" (String (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (write-file p \"a\\nb\") "
        "        (define lines (read-lines p)) "
        "        (and (= (length lines) 2) "
        "             (and (= (car lines) \"a\") "
        "                  (= (car (cdr lines)) \"b\"))))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_truthy(interp, "async read-lines preserves trailing newline line shape",
        "(block "
        "  (define p (string-append \"/tmp/omni-read-lines-trailing-\" (String (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (write-file p \"a\\nb\\n\") "
        "        (define lines (read-lines p)) "
        "        (and (= (length lines) 3) "
        "             (and (= (car lines) \"a\") "
        "                  (and (= (car (cdr lines)) \"b\") "
        "                       (= (car (cdr (cdr lines))) \"\")))))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);

    test_truthy(interp, "async file ops large payload roundtrip",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-large-\" (String (time-ms)) \".txt\")) "
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

    omni_uv_fs_debug_set_max_buf_len(3);
    test_truthy(interp, "async write-file chunks across native buffer boundary",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-chunked-write-\" (String (time-ms)) \".txt\")) "
        "  (define ok "
        "    (await "
        "      (spawn (lambda () "
        "        (write-file p \"abcdefghi\") "
        "        (define text (read-file p)) "
        "        (= text \"abcdefghi\"))))) "
        "  (fs-unlink p) "
        "  ok)",
        pass, fail);
    omni_uv_fs_debug_set_max_buf_len(0);

    test_str_val(interp, "async write-file missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-wf-\" (String (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (write-file p \"x\")) (lambda (e) (String (ref e 'code))))))))",
        "io/write-file-failed", pass, fail);

    test_str_val(interp, "async read-file missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-rf-\" (String (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (read-file p)) (lambda (e) (String (ref e 'code))))))))",
        "io/read-file-failed", pass, fail);

    test_str_val(interp, "async read-lines missing-path payload code",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-rl-\" (String (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () "
        "    (try (lambda (ignored) (read-lines p)) (lambda (e) (String (ref e 'code))))))))",
        "io/read-lines-failed", pass, fail);

    test_nil(interp, "async file-exists? missing-path returns nil",
        "(block "
        "  (define p (string-append \"/tmp/omni-missing-dir-fe-\" (String (time-ms)) \"/x.txt\")) "
        "  (await (spawn (lambda () (file-exists? p)))))",
        pass, fail);

    test_truthy(interp, "async file read cancel stress",
        "(block "
        "  (define p (string-append \"/tmp/omni-async-cancel-\" (String (time-ms)) \".txt\")) "
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
    test_error_contains(interp, "tls-read rejects non-integer max-bytes",
        "(tls-read nil \"x\")",
        "max-bytes must be an integer", pass, fail);
    test_str_val(interp, "tls-read non-integer max-bytes payload code",
        "(handle (tls-read nil \"x\") (raise msg (String (ref msg 'code))))",
        "io/tls-read-max-bytes-not-integer", pass, fail);
    test_error_contains(interp, "tls-read rejects non-positive max-bytes",
        "(tls-read nil 0)",
        "max-bytes must be > 0", pass, fail);
    test_str_val(interp, "tls-read non-positive max-bytes payload code",
        "(handle (tls-read nil 0) (raise msg (String (ref msg 'code))))",
        "io/tls-read-max-bytes-invalid", pass, fail);
    test_error_contains(interp, "tls-read rejects requests above read cap",
        "(tls-read nil 65537)",
        "max-bytes must be <= 65536", pass, fail);
    test_str_val(interp, "tls-read above read cap payload code",
        "(handle (tls-read nil 65537) (raise msg (String (ref msg 'code))))",
        "io/tls-read-max-bytes-too-large", pass, fail);
    test_error_contains(interp, "tls-write requires fiber context",
        "(tls-write nil \"ping\")",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "tls-close requires fiber context",
        "(tls-close nil)",
        "requires running fiber context", pass, fail);

    {
        TlsHandle th = {};
        bool first = tls_try_mark_in_flight(&th);
        bool second = tls_try_mark_in_flight(&th);
        tls_unmark_in_flight(&th);
        bool third = tls_try_mark_in_flight(&th);
        tls_unmark_in_flight(&th);

        bool ok = first && !second && third;
        if (ok) {
            emit_pass_literal("[PASS] tls in-flight gate rejects concurrent read/write/close entry");
            (*pass)++;
        } else {
            io::printn("[FAIL] tls in-flight gate rejects concurrent read/write/close entry");
            (*fail)++;
        }
    }

    test_error_contains(interp, "tcp-connect rejects out-of-range port",
        "(tcp-connect \"127.0.0.1\" 65536)",
        "port must be between 0 and 65535", pass, fail);
    test_str_val(interp, "tcp-connect invalid port payload code",
        "(handle (tcp-connect \"127.0.0.1\" 65536) (raise msg (String (ref msg 'code))))",
        "io/tcp-connect-invalid-port", pass, fail);

    test_error_contains(interp, "signal-handle rejects out-of-range sig",
        "(signal-handle 0 (lambda (sig) sig))",
        "sig must be > 0", pass, fail);
    test_str_val(interp, "signal-handle invalid sig payload code",
        "(handle (signal-handle 0 (lambda (sig) sig)) (raise msg (String (ref msg 'code))))",
        "io/signal-handle-invalid-sig", pass, fail);

    // tls-connect optional CA path validation
    test_error_contains(interp, "tls-connect rejects non-string ca bundle path",
        "(tls-connect nil \"localhost\" 1)",
        "ca-bundle-path must be a string", pass, fail);
    test_str_val(interp, "tls-connect non-string ca bundle payload code",
        "(handle (tls-connect nil \"localhost\" 1) (raise msg (String (ref msg 'code))))",
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
        "(handle (tls-connect nil \"localhost\" true) (raise msg (String (ref msg 'code))))",
        "io/tls-connect-invalid-tcp-handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts direct mTLS form without CA",
        "(tls-connect nil \"localhost\" \"client-cert.pem\" \"client-key.pem\")",
        "invalid TCP handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts ca+resume policy form",
        "(tls-connect nil \"localhost\" \"ca.pem\" true)",
        "invalid TCP handle", pass, fail);
    test_str_val(interp, "tls-write expected data payload code",
        "(handle (tls-write nil 1) (raise msg (String (ref msg 'code))))",
        "io/tls-write-expected-data-string", pass, fail);

    test_truthy(interp, "tls-connect missing CA bundle preserves path cause",
        "(block "
        "  (define missing (string-append \"/tmp/omni-missing-ca-\" (String (time-ms)) \".pem\")) "
        "  (define msg (__raw-tls-trust-store-cause missing)) "
        "  (and (string? msg) "
        "       (string-contains? msg missing) "
        "       (string-contains? msg \"failed to open trust store\")))",
        pass, fail);

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
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (String p) "
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
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (String p) "
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
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (String p) "
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
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (String p) "
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
        "        \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=\" (String p) "
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
        "            (and (string-contains? client-msg \"failed to configure client cert/key\") "
        "                 (string-contains? client-msg \"missing-client.key.pem\")))))",
        pass, fail);
    }

    // tls-server-wrap argument validation
    test_error_contains(interp, "tls-server-wrap rejects non-string cert path",
        "(__raw-tls-server-wrap nil 1 \"key.pem\")",
        "cert-pem-path must be a string", pass, fail);
    test_str_val(interp, "tls-server-wrap invalid stream payload code",
        "(handle (__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\") (raise msg (String (ref msg 'code))))",
        "io/tls-server-wrap-invalid-tcp-stream-handle", pass, fail);
    test_error_contains(interp, "tls-server-wrap rejects non-stream handle",
        "(__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\")",
        "invalid TCP stream handle", pass, fail);
    test_error_contains(interp, "tls-server-wrap rejects extra direct arg",
        "(__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\" 'extra)",
        "expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)", pass, fail);
    test_error_contains(interp, "tls-server-wrap rejects extra packed arg",
        "(__raw-tls-server-wrap '(nil \"cert.pem\" \"key.pem\" extra))",
        "expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)", pass, fail);
}
