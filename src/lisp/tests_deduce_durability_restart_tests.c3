module lisp;

import std::io;

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
