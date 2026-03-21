module lisp;


import std::io;

fn bool deduce_test_write_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[2048] script_buf;
    char[] script = writer_phase
        ? io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (deduce 'fact! rel \"Alice\" 30) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )!!
        : io::bprintf(
            &script_buf,
            "(block "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )!!;
    return write_text_file(script_path, script);
}

fn bool deduce_test_run_main_script(char[] script_path) {
    char[1024] cmd_buf;
    char[] cmd = io::bprintf(
        &cmd_buf,
        "ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=1 "
        "LD_LIBRARY_PATH=/usr/local/lib ./build/main %s",
        (ZString)script_path
    )!!;
    cmd_buf[cmd.len] = 0;
    return deduce_test_system(&cmd_buf[0]) == 0;
}

fn void run_deduce_durability_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-restart-%d-%d",
        (int)pid,
        (int)seq
    )!!;

    char[512] writer_script_buf;
    char[] writer_script = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-restart-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )!!;
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-restart-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )!!;
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

fn void run_deduce_transaction_tests(Interp* interp, int* pass, int* fail) {
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

fn void run_deduce_clear_drop_tests(Interp* interp, int* pass, int* fail) {
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
