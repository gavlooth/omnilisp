module lisp;


import std::io;
import main;

fn void run_async_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Async I/O Tests");
    run_async_tests_io_tls(interp, pass, fail);

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
