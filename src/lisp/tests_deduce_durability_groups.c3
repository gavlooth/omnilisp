module lisp;


import std::io;

fn bool deduce_test_write_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[2048] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (deduce 'fact! rel \"Alice\" 30) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (define refreshed (deduce/refresh! ancestor)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define db-refresh (deduce/refresh! db)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-refresh-count) 1) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref db-refresh 'kind) 'deduce-refresh) "
            "    (= (ref db-refresh 'refresh-scope) 'database) "
            "    (= (ref db-refresh 'refresh-execution-path) 'db-wide) "
            "    (= (ref db-refresh 'refresh-fallback-reason) nil) "
            "    (= (ref db-refresh 'refreshed-materialized-view-count) 0) "
            "    (= (ref db-refresh 'stale-materialized-view-count) 0) "
            "    (= (ref db-refresh 'refreshed-materialized-relations) nil) "
            "    (= (ref db-refresh 'remaining-stale-materialized-relations) nil)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_refresh_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (> (ref schema 'materialized-last-refresh-mutation-epoch) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_stale_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce 'fact! edge 2 3) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (> (ref schema 'materialized-last-refresh-mutation-epoch) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (> (ref schema 'materialized-last-stale-mutation-epoch) "
            "       (ref schema 'materialized-last-refresh-mutation-epoch)) "
            "    (= (ref schema 'materialized-stale) true) "
            "    (= (ref analyze 'materialized-view-count) 1) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref analyze 'stale-materialized-view-count) 1)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_on_read_refresh_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db materialized on-read] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (= (deduce 'count ancestor) 1))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema 'materialized-view) true) "
            "    (= (ref schema 'materialized-derived-ready) true) "
            "    (= (ref schema 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema 'materialized-refresh-count) 1) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_on_read_stale_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db materialized on-read] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (= (deduce 'count ancestor) 1) "
            "  (deduce 'fact! edge 2 3) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define count1 (deduce 'count ancestor)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema0 'materialized-refresh-count) 1) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= count1 2) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-refresh-count) 2) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref analyze 'rule-count) 1) "
            "    (= (ref analyze 'incremental-dependency-edges) 1) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (ref analyze 'stale-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_drop_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db materialized] doomed (src) (dst)) "
            "  (deduce 'drop! doomed) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define doomed (deduce 'open-named db 'doomed \"doomed\" 'src 'dst)) "
            "  (define schema (deduce/schema doomed)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref schema 'materialized-view) false) "
            "    (= (ref schema 'materialized-derived-ready) false) "
            "    (= (ref analyze 'materialized-view-count) 0) "
            "    (= (ref analyze 'ready-materialized-view-count) 0) "
            "    (= (ref analyze 'unready-materialized-view-count) 0)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_write_materialized_dematerialize_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce/dematerialize! ancestor) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (define payload (handle (deduce/refresh! ancestor) (raise msg msg))) "
            "  (and "
            "    (= (ref schema 'materialized-view) false) "
            "    (= (ref schema 'materialized-derived-ready) false) "
            "    (= (ref schema 'materialized-refresh-policy) nil) "
            "    (= (ref schema 'materialized-refresh-count) 0) "
            "    (= (ref schema 'materialized-last-stale-reason) nil) "
            "    (= (ref schema 'materialized-stale) false) "
            "    (= (ref analyze 'materialized-view-count) 0) "
            "    (= (ref analyze 'ready-materialized-view-count) 0) "
            "    (= (ref analyze 'unready-materialized-view-count) 0) "
            "    (= (dict? payload) true) "
            "    (= (ref payload 'code) 'deduce/refresh-relation-not-materialized)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn bool deduce_test_run_main_script(char[] script_path) {
    char[1024] cmd_buf;
    char[] cmd;
    if (try char[] rendered = io::bprintf(
        &cmd_buf,
        "ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=1 "
        "LD_LIBRARY_PATH=/usr/local/lib ./build/main %s",
        (ZString)script_path
    )) {
        cmd = rendered;
    } else {
        return false;
    }
    cmd_buf[cmd.len] = 0;
    return deduce_test_system(&cmd_buf[0]) == 0;
}

fn void run_deduce_durability_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-restart-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce durability across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-restart-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce durability across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-restart-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce durability across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce durability across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce durability across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce durability across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_intent_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-restart-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce materialized intent across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized intent across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized intent across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce materialized intent across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce materialized intent across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce materialized intent across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_refresh_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-refresh-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce materialized refresh metadata across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-refresh-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized refresh metadata across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-refresh-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized refresh metadata across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_refresh_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_refresh_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce materialized refresh metadata across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce materialized refresh metadata across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce materialized refresh metadata across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_stale_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-stale-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce materialized stale metadata across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-stale-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized stale metadata across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-stale-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized stale metadata across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_stale_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_stale_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce materialized stale metadata across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce materialized stale metadata across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce materialized stale metadata across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_on_read_refresh_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-on-read-refresh-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized refresh stays readable across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-on-read-refresh-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized refresh stays readable across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-on-read-refresh-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized refresh stays readable across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_on_read_refresh_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_on_read_refresh_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce on-read materialized refresh stays readable across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce on-read materialized refresh stays readable across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce on-read materialized refresh stays readable across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_on_read_stale_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-on-read-stale-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized stale reads fail truthfully across restart (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-on-read-stale-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized stale reads fail truthfully across restart (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-on-read-stale-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce on-read materialized stale reads fail truthfully across restart (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_on_read_stale_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_on_read_stale_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce on-read materialized stale reads fail truthfully across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce on-read materialized stale reads fail truthfully across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce on-read materialized stale reads fail truthfully across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_drop_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-drop-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce materialized drop clears persisted intent (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-drop-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized drop clears persisted intent (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-drop-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce materialized drop clears persisted intent (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_drop_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_drop_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce materialized drop clears persisted intent (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce materialized drop clears persisted intent");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce materialized drop clears persisted intent (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_dematerialize_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-dematerialize-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce dematerialize clears persisted materialized intent (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-dematerialize-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce dematerialize clears persisted materialized intent (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-dematerialize-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce dematerialize clears persisted materialized intent (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_dematerialize_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_dematerialize_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce dematerialize clears persisted materialized intent (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce dematerialize clears persisted materialized intent");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce dematerialize clears persisted materialized intent (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_materialized_selective_rule_install_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path;
    if (try char[] rendered = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-materialized-selective-rule-install-%d-%d",
        (int)pid,
        (int)seq
    )) {
        db_path = rendered;
    } else {
        io::printn("[FAIL] deduce selective rule-install invalidates only ready materialized views (db path)");
        (*fail)++;
        return;
    }

    char[512] writer_script_buf;
    char[] writer_script;
    if (try char[] rendered = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-materialized-selective-rule-install-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        writer_script = rendered;
    } else {
        io::printn("[FAIL] deduce selective rule-install invalidates only ready materialized views (writer path)");
        (*fail)++;
        return;
    }
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script;
    if (try char[] rendered = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-materialized-selective-rule-install-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )) {
        reader_script = rendered;
    } else {
        io::printn("[FAIL] deduce selective rule-install invalidates only ready materialized views (reader path)");
        (*fail)++;
        return;
    }
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_materialized_selective_rule_install_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_materialized_selective_rule_install_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce selective rule-install invalidates only ready materialized views (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce selective rule-install invalidates only ready materialized views");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce selective rule-install invalidates only ready materialized views (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_transaction_commit_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-commit (deduce 'open 'memory)) "
            "  (define [relation txn-db-commit] txn_person_commit (name) (age)) "
            "  (define txc (deduce 'block txn-db-commit)) "
            "  (deduce 'fact! txc txn_person_commit \"Alice\" 33) "
            "  (define before (deduce 'count txn_person_commit)) "
            "  (deduce 'commit txc) "
            "  (define after (deduce 'count txn_person_commit)) "
            "  (and (= before 0) (= after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn commit");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn commit");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_abort_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-abort (deduce 'open 'memory)) "
            "  (define [relation txn-db-abort] txn_person_abort (name) (age)) "
            "  (define txa (deduce 'block txn-db-abort)) "
            "  (deduce 'fact! txa txn_person_abort \"Bob\" 21) "
            "  (deduce 'abort txa) "
            "  (= (deduce 'count txn_person_abort) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn abort");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn abort");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_db_mismatch_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define txn-db-a (deduce 'open 'memory)) "
            "  (define txn-db-b (deduce 'open 'memory)) "
            "  (define [relation txn-db-b] txn_person_mismatch (name) (age)) "
            "  (define txm (deduce 'block txn-db-a)) "
            "  (deduce 'fact! txm txn_person_mismatch \"Eve\" 44))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce txn db mismatch");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn db mismatch");
            (*fail)++;
        }
    }
}

fn void run_deduce_transaction_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_transaction_commit_test(interp, pass, fail);
    run_deduce_transaction_abort_test(interp, pass, fail);
    run_deduce_transaction_db_mismatch_test(interp, pass, fail);
}

fn bool deduce_test_write_materialized_selective_rule_install_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[4096] script_buf;
    char[] script;
    if (writer_phase) {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define [relation db materialized] pending (src) (dst)) "
            "  (define [relation db] edge (src) (dst)) "
            "  (define [relation db] ancestor (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (deduce/refresh! ancestor) "
            "  (deduce/rule! db '(ancestor ?x ?z) '(edge ?x ?y) '(ancestor ?y ?z)) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    } else {
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db (deduce 'open \"%s\")) "
            "  (define pending (deduce 'open-named db 'pending \"pending\" 'src 'dst)) "
            "  (define ancestor (deduce 'open-named db 'ancestor \"ancestor\" 'src 'dst)) "
            "  (define pending-schema (deduce/schema pending)) "
            "  (define ancestor-schema (deduce/schema ancestor)) "
            "  (define analyze (deduce/analyze db)) "
            "  (and "
            "    (= (ref pending-schema 'materialized-view) true) "
            "    (= (ref pending-schema 'materialized-derived-ready) false) "
            "    (= (ref pending-schema 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref pending-schema 'materialized-stale) true) "
            "    (= (ref ancestor-schema 'materialized-view) true) "
            "    (= (ref ancestor-schema 'materialized-derived-ready) true) "
            "    (= (ref ancestor-schema 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref ancestor-schema 'materialized-stale) true) "
            "    (= (ref analyze 'ready-materialized-view-count) 1) "
            "    (= (ref analyze 'unready-materialized-view-count) 1) "
            "    (= (ref analyze 'stale-materialized-view-count) 2)))",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            return false;
        }
    }
    return write_text_file(script_path, script);
}

fn void run_deduce_clear_preserves_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define clr-db (deduce 'open 'memory)) "
            "  (define [relation clr-db] clr-person (name) (age)) "
            "  (deduce 'fact! clr-person \"Alice\" 30) "
            "  (deduce 'fact! clr-person \"Bob\" 31) "
            "  (deduce 'clear! clr-person) "
            "  (define c0 (deduce 'count clr-person)) "
            "  (deduce 'fact! clr-person \"Carol\" 40) "
            "  (define c1 (deduce 'count clr-person)) "
            "  (and (= c0 0) (= c1 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce clear! preserves relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce clear! preserves relation");
            (*fail)++;
        }
    }
}

fn void run_deduce_drop_invalidates_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define drp-db (deduce 'open 'memory)) "
            "  (define [relation drp-db] drp-person (name) (age)) "
            "  (deduce 'fact! drp-person \"Alice\" 30) "
            "  (deduce 'drop! drp-person) "
            "  (deduce 'count drp-person))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce drop! invalidates relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce drop! invalidates relation");
            (*fail)++;
        }
    }
}

fn void run_deduce_clear_empty_relation_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define clr-empty-db (deduce 'open 'memory)) "
            "  (define [relation clr-empty-db] clr-empty-person (name) (age)) "
            "  (define before (deduce 'count clr-empty-person)) "
            "  (deduce 'clear! clr-empty-person) "
            "  (define after (deduce 'count clr-empty-person)) "
            "  (and (= before 0) (= after 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce clear! on empty relation is deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce clear! on empty relation is deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce clear! on empty relation is deterministic");
            }
            (*fail)++;
        }
    }
}

fn void run_deduce_retract_missing_tuple_noop_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define retract-empty-db (deduce 'open 'memory)) "
        "  (define [relation retract-empty-db] retract-empty-person (name) (age)) "
        "  (deduce 'retract! retract-empty-person \"Alice\" 30))",
        "[PASS] deduce retract! on missing tuple is deterministic no-op",
        "[FAIL] deduce retract! on missing tuple is deterministic no-op (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_drop_repeat_noop_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define drop-repeat-db (deduce 'open 'memory)) "
        "  (define [relation drop-repeat-db] drop-repeat-person (name) (age)) "
        "  (deduce 'drop! drop-repeat-person) "
        "  (deduce 'drop! drop-repeat-person))",
        "[PASS] deduce drop! on dropped relation is deterministic no-op",
        "[FAIL] deduce drop! on dropped relation is deterministic no-op (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_dropped_cleanup_error_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define dropped-cleanup-db (deduce 'open 'memory)) "
        "  (define [relation dropped-cleanup-db] dropped-cleanup-person (name) (age)) "
        "  (deduce 'drop! dropped-cleanup-person) "
        "  (define retract-payload (handle (deduce 'retract! dropped-cleanup-person \"Alice\" 30) (raise msg msg))) "
        "  (define clear-payload (handle (deduce 'clear! dropped-cleanup-person) (raise msg msg))) "
        "  (and "
        "    (dict? retract-payload) "
        "    (= (ref retract-payload 'code) 'deduce/retract-relation-dropped) "
        "    (dict? clear-payload) "
        "    (= (ref clear-payload 'code) 'deduce/clear-relation-dropped)))",
        "[PASS] deduce cleanup paths keep dropped relation errors deterministic",
        "[FAIL] deduce cleanup paths keep dropped relation errors deterministic",
        pass,
        fail
    );
}

fn void run_deduce_clear_drop_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_clear_preserves_relation_test(interp, pass, fail);
    run_deduce_drop_invalidates_relation_test(interp, pass, fail);
    run_deduce_clear_empty_relation_test(interp, pass, fail);
    run_deduce_retract_missing_tuple_noop_test(interp, pass, fail);
    run_deduce_drop_repeat_noop_test(interp, pass, fail);
    run_deduce_dropped_cleanup_error_test(interp, pass, fail);

    test_error_contains(
        interp,
        "[PASS] deduce open-named rejects relation arity conflict deterministically",
        "(block "
        "  (define arity-conflict-db (deduce 'open 'memory)) "
        "  (define rel-a (deduce 'open-named arity-conflict-db 'arity_conflict \"arity_conflict_v1\" 'name 'age)) "
        "  (deduce 'open-named arity-conflict-db 'arity_conflict \"arity_conflict_v2\" 'name 'age 'email))",
        "relation name already registered with different arity",
        pass,
        fail
    );
}
