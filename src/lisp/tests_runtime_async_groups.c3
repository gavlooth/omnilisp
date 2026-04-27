module lisp;


import std::io;
import main;

fn bool async_error_contains(Value* value, char[] expected) {
    return value != null &&
        value.tag == ERROR &&
        value.str_chars != null &&
        str_contains(value.str_chars[:value.str_len], expected);
}

fn void test_raw_tcp_connect_finite_contract(Interp* interp, int* pass, int* fail) {
    Value* host = make_string(interp, "127.0.0.1");
    Value* port = make_int(interp, 1);
    Value* ignored = make_nil(interp);
    Value*[3] direct_args = { host, port, ignored };
    Value* direct_result = prim_tcp_connect(direct_args[..], null, interp);

    Value* packed = make_cons(
        interp,
        host,
        make_cons(interp, port, make_cons(interp, ignored, make_nil(interp)))
    );
    Value*[1] packed_args = { packed };
    Value* packed_result = prim_tcp_connect(packed_args[..], null, interp);

    bool ok = async_error_contains(direct_result, "tcp-connect: expected (tcp-connect host port)") &&
        async_error_contains(packed_result, "tcp-connect: expected (tcp-connect host port)");
    if (ok) {
        emit_pass("raw tcp-connect primitive rejects trailing direct and packed args");
        (*pass)++;
    } else {
        io::printn("[FAIL] raw tcp-connect primitive rejects trailing direct and packed args");
        (*fail)++;
    }
}

fn void run_async_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Async I/O Tests");
    run_async_tests_io_tls(interp, pass, fail);
    run_async_tests_repl_server(interp, pass, fail);
    test_raw_tcp_connect_finite_contract(interp, pass, fail);

    // listener lifecycle
    test_tag(interp, "tcp-listen open/close",
        "(block "
        "  (define f (spawn (lambda () "
        "    (define ls (tcp-listen \"127.0.0.1\" 0 8)) "
        "    (tcp-close ls)))) "
        "  (await f))",
        VOID, pass, fail);

    // fiber-safe listen path: listen in fiber uses libuv tcp bind/listen bridge
    test_tag(interp, "tcp-listen in fiber via libuv bridge",
        "(block "
        "  (define f (spawn (lambda () "
        "    (define ls (tcp-listen \"127.0.0.1\" 0 8)) "
        "    (tcp-close ls)))) "
        "  (await f))",
        VOID, pass, fail);

    test_error_contains(interp, "tcp-read requires fiber context",
        "(tcp-read nil)",
        "requires running fiber context", pass, fail);
    test_error_contains(interp, "tcp-read rejects non-integer max-bytes",
        "(await (spawn (lambda () (tcp-read nil \"x\"))))",
        "max-bytes must be an integer", pass, fail);
    test_error_contains(interp, "tcp-read rejects non-integer timeout",
        "(await (spawn (lambda () (tcp-read nil 1 \"x\"))))",
        "timeout-ms must be an integer", pass, fail);
    test_error_contains(interp, "tcp-read rejects requests above read cap",
        "(await (spawn (lambda () (tcp-read nil 65537))))",
        "max-bytes must be <= 65536", pass, fail);
    test_error_contains(interp, "raw tcp-listen rejects trailing direct arg",
        "(__raw-tcp-listen \"127.0.0.1\" 0 8 'ignored)",
        "tcp-listen: expected (tcp-listen host port [backlog])", pass, fail);
    test_error_contains(interp, "raw tcp-listen rejects trailing packed arg",
        "(__raw-tcp-listen (cons \"127.0.0.1\" (cons 0 (cons 8 (cons 'ignored nil)))))",
        "tcp-listen: expected (tcp-listen host port [backlog])", pass, fail);
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
        VOID, pass, fail);

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
        "    (define p (string-append \"/tmp/omni_pipe_async_\" (String (% (time-ms) 1000000)) \".sock\")) "
        "    (define listener (pipe-listen p)) "
        "    (define client (pipe-connect p)) "
        "    (define server (tcp-accept listener)) "
        "    (tcp-write client \"pipe-ok\") "
        "    (define got (tcp-read server)) "
        "    (tcp-close client) "
        "    (tcp-close server) "
        "    (tcp-close listener) "
        "    (handle (fs-unlink p) (raise msg nil)) "
        "    (= got \"pipe-ok\")))) "
        "  (await pf))",
        pass, fail);

    test_truthy(interp, "pipe-listen preserves pre-existing regular file",
        "(block "
        "  (define p (string-append \"/tmp/omni_pipe_regular_guard_\" (String (% (time-ms) 1000000)) \".sock\")) "
        "  (define h (fs-open p 577 420)) "
        "  (fs-write h \"keep\") "
        "  (fs-close h) "
        "  (define pf (spawn (lambda () "
        "    (handle "
        "      (block "
        "        (define listener (pipe-listen p)) "
        "        (tcp-close listener) "
        "        false) "
        "      (raise msg true))))) "
        "  (define failed-closed (await pf)) "
        "  (define got "
        "    (handle "
        "      (block "
        "        (define rh (fs-open p 0)) "
        "        (define payload (fs-read rh 4)) "
        "        (fs-close rh) "
        "        payload) "
        "      (raise msg \"__missing__\"))) "
        "  (handle (fs-unlink p) (raise msg nil)) "
        "  (and failed-closed (= got \"keep\")))",
        pass, fail);

    // fiber-safe UDP receive path: recv in fiber uses async wakeup bridge
    test_truthy(interp, "udp-recv in fiber via async bridge",
        "(block "
        "  (define p (+ 35000 (% (time-ms) 20000))) "
        "  (define recv (udp-socket)) "
        "  (udp-bind recv \"127.0.0.1\" p) "
        "  (define wf (spawn (lambda () "
        "    (define rf (spawn (lambda () (udp-recv recv)))) "
        "    (define sf (spawn (lambda () "
        "      (async-sleep 5) "
        "      (define snd (udp-socket)) "
        "      (udp-send snd \"127.0.0.1\" p \"pong\") "
        "      (udp-close snd)))) "
        "    (define got (await rf)) "
        "    (await sf) "
        "    got))) "
        "  (define got (await wf)) "
        "  (udp-close recv) "
        "  (= got \"pong\"))",
        pass, fail);
    test_error_contains(interp, "udp-recv rejects requests above read cap",
        "(udp-recv nil 65537)",
        "max-bytes must be <= 65536", pass, fail);
    test_error_contains(interp, "raw udp-bind rejects trailing direct arg",
        "(__raw-udp-bind nil \"127.0.0.1\" 0 'ignored)",
        "udp-bind: expected (udp-bind handle host port)", pass, fail);
    test_error_contains(interp, "raw udp-bind rejects trailing packed arg",
        "(__raw-udp-bind (cons nil (cons \"127.0.0.1\" (cons 0 (cons 'ignored nil)))))",
        "udp-bind: expected (udp-bind handle host port)", pass, fail);
    test_error_contains(interp, "raw udp-send rejects trailing direct arg",
        "(__raw-udp-send nil \"127.0.0.1\" 9 \"x\" 'ignored)",
        "udp-send: expected (udp-send handle host port data)", pass, fail);
    test_error_contains(interp, "raw udp-send rejects trailing packed arg",
        "(__raw-udp-send (cons nil (cons \"127.0.0.1\" (cons 9 (cons \"x\" (cons 'ignored nil))))))",
        "udp-send: expected (udp-send handle host port data)", pass, fail);
}

fn void run_async_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_async_tests, pass, fail);
}
