module lisp;

import std::io;
import std::core::mem;

extern fn int advanced_io_test_mkdir(char* path, uint mode) @extern("mkdir");
extern fn int advanced_io_test_rmdir(char* path) @extern("rmdir");

fn bool advanced_io_string_value_equals(Value* value, char[] expected) {
    if (value == null || value.tag != STRING || value.str_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (value.str_chars[i] != expected[i]) return false;
    }
    return true;
}

fn bool advanced_io_error_contains(Value* value, char[] expected) {
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

fn void advanced_io_record_result(char[] name, bool ok, int* pass, int* fail) {
    if (ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn Value* advanced_io_nul_string(Interp* interp, char[] prefix, char[] suffix) {
    char[256] bytes;
    if (prefix.len + suffix.len + 1 > bytes.len) return null;
    usz n = 0;
    for (usz i = 0; i < prefix.len; i++) bytes[n++] = prefix[i];
    bytes[n++] = (char)0;
    for (usz i = 0; i < suffix.len; i++) bytes[n++] = suffix[i];
    return make_string(interp, bytes[0:n]);
}

fn void run_advanced_c_api_string_boundary_tests(Interp* interp, int* pass, int* fail) {
    Value* bad_path = advanced_io_nul_string(interp, "/tmp/omni_capi_", "suffix");
    Value* good_path = make_string(interp, "/tmp/omni_capi_good");
    if (bad_path == null || bad_path.tag == ERROR || good_path == null || good_path.tag == ERROR) {
        advanced_io_record_result("C API string boundary test setup", false, pass, fail);
        return;
    }

    Value*[1] one_bad = { bad_path };
    Value*[2] tls_connect_args = { make_nil(interp), bad_path };
    Value*[2] rename_args = { bad_path, good_path };
    Value*[3] open_args = { bad_path, make_int(interp, 0), make_int(interp, 420) };

    advanced_io_record_result(
        "fs-open rejects embedded NUL path before native call",
        advanced_io_error_contains(prim_fs_open(open_args[..], null, interp), "embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "fs-stat rejects embedded NUL path before native call",
        advanced_io_error_contains(prim_fs_stat(one_bad[..], null, interp), "embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "fs-readdir rejects embedded NUL path before native call",
        advanced_io_error_contains(prim_fs_readdir(one_bad[..], null, interp), "embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "fs-unlink rejects embedded NUL path before native call",
        advanced_io_error_contains(prim_fs_unlink(one_bad[..], null, interp), "embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "fs-rename rejects embedded NUL src before native call",
        advanced_io_error_contains(prim_fs_rename(rename_args[..], null, interp), "embedded NUL byte"),
        pass, fail);

    advanced_io_record_result(
        "tls trust-store helper rejects embedded NUL path before native call",
        advanced_io_error_contains(prim_tls_trust_store_load_cause(one_bad[..], null, interp), "embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "tls-connect rejects embedded NUL hostname before native call",
        advanced_io_error_contains(prim_tls_connect(tls_connect_args[..], null, interp), "embedded NUL byte"),
        pass, fail);
}

fn void run_advanced_process_spawn_c_string_boundary_tests(Interp* interp, int* pass, int* fail) {
    Value* bad_cmd = advanced_io_nul_string(interp, "/bin/sh", "suffix");
    Value* bad_arg = advanced_io_nul_string(interp, "-c", "suffix");
    Value* bad_env = advanced_io_nul_string(interp, "OMNI_PROC_TEST=ok", "suffix");
    Value* argv = make_array(interp, 1);
    Value* envv = make_array(interp, 1);
    Value* sh = make_string(interp, "/bin/sh");
    if (bad_cmd == null || bad_cmd.tag == ERROR ||
        bad_arg == null || bad_arg.tag == ERROR ||
        bad_env == null || bad_env.tag == ERROR ||
        argv == null || argv.tag != ARRAY || argv.array_val == null ||
        envv == null || envv.tag != ARRAY || envv.array_val == null ||
        sh == null || sh.tag == ERROR) {
        advanced_io_record_result("process-spawn C API string boundary test setup", false, pass, fail);
        return;
    }

    argv.array_val.items[0] = bad_arg;
    argv.array_val.length = 1;
    envv.array_val.items[0] = bad_env;
    envv.array_val.length = 1;

    Value*[3] cmd_args = { bad_cmd, make_nil(interp), make_nil(interp) };
    Value*[3] argv_args = { sh, argv, make_nil(interp) };
    Value*[3] env_args = { sh, make_nil(interp), envv };

    advanced_io_record_result(
        "process-spawn rejects embedded NUL cmd before native call",
        advanced_io_error_contains(prim_process_spawn(cmd_args[..], null, interp), "cmd contains embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "process-spawn rejects embedded NUL argv before native call",
        advanced_io_error_contains(prim_process_spawn(argv_args[..], null, interp), "string argument contains embedded NUL byte"),
        pass, fail);
    advanced_io_record_result(
        "process-spawn rejects embedded NUL env before native call",
        advanced_io_error_contains(prim_process_spawn(env_args[..], null, interp), "string argument contains embedded NUL byte"),
        pass, fail);
}

fn void run_advanced_c_api_process_boundary_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_c_api_process_boundary_tests(interp, pass, fail);
    run_advanced_process_spawn_c_string_boundary_tests(interp, pass, fail);
}

fn bool advanced_fs_readdir_preserves_newline_entry_names(Interp* interp) {
    char[] dir = "/tmp/omni_fs_readdir_newline_case";
    char[] entry_name = "line\nbreak";
    char[256] path_buf;
    (void)advanced_io_test_rmdir((ZString)dir);
    if (advanced_io_test_mkdir((ZString)dir, 0o755) != 0) return false;
    defer (void)advanced_io_test_rmdir((ZString)dir);

    if (try char[] path = io::bprintf(&path_buf, "%s/%s", (ZString)dir, (ZString)entry_name)) {
        int fd = fs_uv_open((ZString)path, 577, 0o644);
        if (fd < 0) return false;
        (void)fs_uv_close(fd);
        defer (void)fs_uv_unlink((ZString)path);

        Value*[1] args = { make_string(interp, dir) };
        Value* entries = prim_fs_readdir(args[..], null, interp);
        if (entries == null || entries.tag != ARRAY || entries.array_val == null) return false;

        int exact = 0;
        int split_line = 0;
        int split_break = 0;
        for (usz i = 0; i < entries.array_val.length; i++) {
            Value* item = entries.array_val.items[i];
            if (advanced_io_string_value_equals(item, entry_name)) exact++;
            if (advanced_io_string_value_equals(item, "line")) split_line++;
            if (advanced_io_string_value_equals(item, "break")) split_break++;
        }
        return exact == 1 && split_line == 0 && split_break == 0;
    }
    return false;
}

fn bool advanced_process_kill_invalid_signal_case(Interp* interp, int sig) {
    char[1400] code_buf;
    if (try char[] code = io::bprintf(
        &code_buf,
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 2\"] nil)) "
        "(define code (handle (process-kill (ref p 'handle) %d) (raise msg (String (ref msg 'code))))) "
        "(if (= code \"io/process-kill-invalid-sig\") (process-kill (ref p 'handle) 15) nil) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= code \"io/process-kill-invalid-sig\") (> (ref st 'term-signal) 0)))",
        sig
    )) {
        EvalResult result = run(code, interp);
        return !result.error.has_error &&
            result.value != null &&
            result.value.tag != ERROR &&
            result.value.tag != NIL;
    }
    return false;
}

fn bool advanced_process_kill_valid_signal_case(Interp* interp, int sig) {
    char[1400] code_buf;
    if (try char[] code = io::bprintf(
        &code_buf,
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.2\"] nil)) "
        "(define code (handle (process-kill (ref p 'handle) %d) (raise msg (String (ref msg 'code))))) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (if (= code \"io/process-kill-invalid-sig\") false true) (> (ref st 'term-signal) 0)))",
        sig
    )) {
        EvalResult result = run(code, interp);
        return !result.error.has_error &&
            result.value != null &&
            result.value.tag != ERROR &&
            result.value.tag != NIL;
    }
    return false;
}

fn void run_advanced_process_kill_signal_validation_tests(Interp* interp, int* pass, int* fail) {
    int max_sig = omni_uv_process_max_signal();
    bool zero_ok = advanced_process_kill_invalid_signal_case(interp, 0);
    bool negative_ok = advanced_process_kill_invalid_signal_case(interp, -1);
    bool max_ok = max_sig > 0 && advanced_process_kill_valid_signal_case(interp, max_sig);
    bool above_max_ok = max_sig > 0 &&
        max_sig < int.max &&
        advanced_process_kill_invalid_signal_case(interp, max_sig + 1);

    if (zero_ok && negative_ok && max_ok && above_max_ok) {
        emit_pass("process-kill validates signal bounds without SIGKILL fallback");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] process-kill validates signal bounds without SIGKILL fallback (max_sig=%d zero=%s negative=%s max=%s above_max=%s)",
            max_sig,
            zero_ok ? "yes" : "no",
            negative_ok ? "yes" : "no",
            max_ok ? "yes" : "no",
            above_max_ok ? "yes" : "no"
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
    if (advanced_fs_readdir_preserves_newline_entry_names(interp)) {
        emit_pass_literal("[PASS] fs-readdir preserves newline-containing entry names");
        (*pass)++;
    } else {
        io::printn("[FAIL] fs-readdir preserves newline-containing entry names");
        (*fail)++;
    }
    test_error_contains(interp, "fs-open rejects flags out of range",
        "(fs-open \"/tmp/omni_fs_bad_flags.txt\" 9223372036854775807)",
        "flags out of range", pass, fail);
    test_error_contains(interp, "fs-open rejects mode out of range",
        "(fs-open \"/tmp/omni_fs_bad_mode.txt\" 0 9223372036854775807)",
        "mode out of range", pass, fail);
    test_error_contains(interp, "fs-open rejects extra direct arg",
        "(fs-open \"/dev/null\" 0 420 'extra)",
        "expected (fs-open path flags [mode])", pass, fail);
    test_error_contains(interp, "fs-open rejects extra packed arg",
        "(__raw-fs-open '(\"/dev/null\" 0 420 extra))",
        "expected (fs-open path flags [mode])", pass, fail);
    run_advanced_c_api_string_boundary_tests(interp, pass, fail);

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
    test_error_contains(interp, "fs-read rejects requests above single-read cap",
        "(block "
        "(define r (fs-open \"/dev/null\" 0)) "
        "(fs-read r 8388609))",
        "fs-read: requested byte count exceeds maximum single read", pass, fail);

    omni_uv_fs_debug_set_max_buf_len(2);
    test_truthy_interp(interp, "fs-write chunks across native buffer boundary",
        "(block "
        "(define p (string-append \"/tmp/omni_fs_write_chunk_\" (String (% (time-ms) 1000000)) \".txt\")) "
        "(define h (fs-open p 577 420)) "
        "(define n (fs-write h \"abcdef\")) "
        "(fs-close h) "
        "(define rh (fs-open p 0)) "
        "(define got (fs-read rh 16)) "
        "(fs-close rh) "
        "(fs-unlink p) "
        "(and (= n 6) (= got \"abcdef\")))",
        pass, fail);
    omni_uv_fs_debug_set_max_buf_len(0);

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
    test_truthy_interp(interp, "process-kill rejects trailing raw direct arg before kill",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 2\"] nil)) "
        "(define code (handle (__raw-process-kill (ref p 'handle) 15 'ignored) "
        "  (raise msg (String (ref msg 'code))))) "
        "(process-kill (ref p 'handle) 15) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= code \"io/process-kill-expected-handle-sig\") (> (ref st 'term-signal) 0)))",
        pass, fail);
    test_truthy_interp(interp, "process-kill rejects trailing packed arg before kill",
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 2\"] nil)) "
        "(define code (handle (__raw-process-kill (list (ref p 'handle) 15 'ignored)) "
        "  (raise msg (String (ref msg 'code))))) "
        "(process-kill (ref p 'handle) 15) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(and (= code \"io/process-kill-expected-handle-sig\") (> (ref st 'term-signal) 0)))",
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
    run_advanced_process_wait_status_retry_after_result_oom_test(interp, pass, fail);
    run_advanced_process_handle_concurrent_reuse_test(interp, pass, fail);
    run_advanced_process_kill_signal_validation_tests(interp, pass, fail);

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
    test_truthy_interp(interp, "signal-handle rejects trailing raw direct arg",
        "(block "
        "(define result (handle (__raw-signal-handle 10 (lambda (sig) nil) 'ignored) "
        "  (raise msg (String (ref msg 'code))))) "
        "(if (= result \"io/signal-handle-expected-sig-callback\") "
        "  true "
        "  (block (signal-unhandle result) false)))",
        pass, fail);
    test_truthy_interp(interp, "signal-handle rejects trailing packed arg",
        "(block "
        "(define result (handle (__raw-signal-handle (list 10 (lambda (sig) nil) 'ignored)) "
        "  (raise msg (String (ref msg 'code))))) "
        "(if (= result \"io/signal-handle-expected-sig-callback\") "
        "  true "
        "  (block (signal-unhandle result) false)))",
        pass, fail);
    run_advanced_signal_burst_delivery_test(interp, pass, fail);
    run_advanced_signal_handle_registry_scaling_test(interp, pass, fail);
    run_advanced_signal_dispatch_owner_interp_isolation_test(interp, pass, fail);
    run_advanced_signal_callback_scope_release_test(interp, pass, fail);
    run_advanced_sigint_interrupt_routing_test(interp, pass, fail);
    // Subtype check: Number accepts Integer and Float64
    test_nil_interp(interp, "defeffect number arg", "(define [effect] (test/num (^Number n)))", pass, fail);
    test_eq_interp(interp, "effect number int",
        "(handle (signal test/num 42) (test/num x x))", 42, pass, fail);
    test_error(interp, "effect number str",
        "(handle (signal test/num \"bad\") (test/num x x))", pass, fail);
}
