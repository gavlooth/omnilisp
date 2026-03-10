module lisp;


import std::io;

fn void setup_deduce_person_relation(Interp* interp) {
    setup(interp, "(define ddb (deduce 'open 'memory))");
    {
        Value* db_val = interp.global_env.lookup(interp.symbols.intern("ddb"));
        if (db_val != null && db_val.tag == FFI_HANDLE) {
            Value* name_sym = interp.alloc_value(); name_sym.tag = SYMBOL; name_sym.sym_val = interp.symbols.intern("person");
            Value* col1 = interp.alloc_value(); col1.tag = SYMBOL; col1.sym_val = interp.symbols.intern("name");
            Value* col2 = interp.alloc_value(); col2.tag = SYMBOL; col2.sym_val = interp.symbols.intern("age");
            Value* spec = make_cons(interp, name_sym, make_cons(interp, col1, make_cons(interp, col2, make_nil(interp))));
            Value*[1] fargs;
            fargs[0] = make_cons(interp, db_val, spec);
            Value* rel = prim_define_relation(fargs[..], null, interp);
            if (rel != null && rel.tag == FFI_HANDLE) {
                env_define_with_barrier(interp.global_env, interp.symbols.intern("person"), rel, interp);
            }
        }
    }
    setup(interp, "(deduce 'fact! person \"Alice\" 30)");
    setup(interp, "(deduce 'fact! person \"Bob\" 25)");
}

fn Value* setup_deduce_relation_for_db(
    Interp* interp,
    Value* db_val,
    SymbolId rel_name,
    SymbolId col1,
    SymbolId col2
) {
    if (interp == null || db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null) return null;

    Value* name_sym = interp.alloc_value();
    name_sym.tag = SYMBOL;
    name_sym.sym_val = rel_name;

    Value* c1_sym = interp.alloc_value();
    c1_sym.tag = SYMBOL;
    c1_sym.sym_val = col1;

    Value* c2_sym = interp.alloc_value();
    c2_sym.tag = SYMBOL;
    c2_sym.sym_val = col2;

    Value* spec = make_cons(interp, name_sym, make_cons(interp, c1_sym, make_cons(interp, c2_sym, make_nil(interp))));
    Value*[1] fargs;
    fargs[0] = make_cons(interp, db_val, spec);
    return prim_define_relation(fargs[..], null, interp);
}

extern fn int deduce_test_mkdir(char* path, uint mode) @extern("mkdir");
extern fn int deduce_test_getpid() @extern("getpid");
extern fn int deduce_test_system(char* command) @extern("system");
extern fn int deduce_test_unlink(char* path) @extern("unlink");

usz g_deduce_test_long_path_seq = 0;

fn SymbolId deduce_test_make_long_relation_name(Interp* interp, char variant) {
    char[80] name_buf;
    for (usz i = 0; i < name_buf.len; i++) name_buf[i] = 'r';
    name_buf[name_buf.len - 1] = variant;
    return interp.symbols.intern(name_buf[..]);
}

fn Value* deduce_test_make_long_open_path(Interp* interp, char variant) {
    char[512] path_buf;
    usz len = 0;
    path_buf[len++] = '/';
    path_buf[len++] = 't';
    path_buf[len++] = 'm';
    path_buf[len++] = 'p';

    usz seq = g_deduce_test_long_path_seq++;
    uint pid = (uint)deduce_test_getpid();
    path_buf[len++] = '/';
    path_buf[len++] = 'o';
    path_buf[len++] = 'd';
    path_buf[len++] = 'l';
    path_buf[len++] = 'p';
    path_buf[len++] = '-';
    for (usz i = 0; i < 8; i++) {
        usz shift = (7 - i) * 4;
        path_buf[len++] = DEDUCE_HEX_DIGITS[(pid >> shift) & 0xF];
    }
    path_buf[len++] = '-';
    for (usz i = 0; i < 4; i++) {
        usz shift = (3 - i) * 4;
        path_buf[len++] = DEDUCE_HEX_DIGITS[(seq >> shift) & 0xF];
    }
    path_buf[len] = 0;
    deduce_test_mkdir(&path_buf[0], 0o755);

    for (usz seg = 0; seg < 6; seg++) {
        path_buf[len++] = '/';
        char fill = (char)('a' + (int)seg);
        for (usz i = 0; i < 37; i++) path_buf[len++] = fill;
        path_buf[len] = 0;
        deduce_test_mkdir(&path_buf[0], 0o755);
    }

    path_buf[len++] = '/';
    path_buf[len++] = 'd';
    path_buf[len++] = 'b';
    path_buf[len++] = '-';
    path_buf[len++] = variant;
    return make_string(interp, path_buf[..len]);
}

fn void run_deduce_open_smoke_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run("(deduce 'open 'memory)", interp);
    bool ok = !r.error.has_error &&
        r.value != null &&
        r.value.tag == FFI_HANDLE &&
        r.value.ffi_val != null;
    if (ok) {
        emit_pass_literal("[PASS] deduce 'open returns handle");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            char[] msg = r.error.message[:256];
            io::printfn("[FAIL] deduce 'open returns handle (error: %s)", (ZString)msg);
        } else {
            io::printfn("[FAIL] deduce 'open returns handle (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_open_named_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define ndb (deduce 'open 'memory)) "
            "  (define rel-a (deduce 'open-named ndb 'nrel_a \"named_a\" 'name 'age)) "
            "  (define rel-b (deduce 'open-named ndb 'nrel_b \"named_b\" 'name 'age)) "
            "  (deduce 'fact! rel-a \"Alice\" 30) "
            "  (and (= (deduce 'count rel-a) 1) (= (deduce 'count rel-b) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named isolates db names");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named isolates db names");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define ndb2 (deduce 'open 'memory)) "
            "  (define rel-c (deduce 'open-named ndb2 'nrel_c 'named_c 'name 'age)) "
            "  (= (deduce 'count rel-c) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named accepts symbol db-name");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named accepts symbol db-name");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(deduce 'open-named (deduce 'open 'memory) 'bad 42 'name)",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce open-named validates db-name type");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named validates db-name type");
            (*fail)++;
        }
    }
}

fn void run_deduce_relation_attr_define_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-attr (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-attr] rel-attr-test (id) (name) (qty)) "
        "  (deduce 'count rel-attr-test))",
        interp
    );
    if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
        emit_pass_literal("[PASS] deduce [relation] define attr");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce [relation] define attr");
        (*fail)++;
    }
}

fn void run_deduce_crud_example_flow_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (let (create1 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        create2 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        existing (repo/get-by-id 101) "
        "        missing (repo/get-by-id 9999)) "
        "    (and (= ('status create1) \"ok\") "
        "         (and (= ('status create2) \"error\") "
        "              (and (= ('error create2) \"item already exists\") "
        "                   (and (= existing.name \"paper\") "
        "                        (null? missing)))))))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce CRUD example flow");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce CRUD example flow (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] deduce CRUD example flow (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_legacy_alias_rejection_tests(Interp* interp, int* pass, int* fail) {
    test_error(interp, "legacy deduce-open rejected", "(deduce-open 'memory)", pass, fail);
    test_error(interp, "legacy fact! rejected", "(fact! nil)", pass, fail);
    test_error(interp, "legacy retract! rejected", "(retract! nil)", pass, fail);
    test_error(interp, "legacy deduce-scan rejected", "(deduce-scan nil)", pass, fail);
    test_error(interp, "legacy deduce-query rejected", "(deduce-query nil nil)", pass, fail);
    test_error(interp, "legacy deduce-count rejected", "(deduce-count nil)", pass, fail);
    test_error(interp, "legacy deduce-match rejected", "(deduce-match nil nil)", pass, fail);
}

fn void run_deduce_group_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_open_smoke_test(interp, pass, fail);
    run_deduce_open_named_test(interp, pass, fail);
    run_deduce_durability_restart_test(interp, pass, fail);
    run_deduce_relation_attr_define_test(interp, pass, fail);
    run_deduce_crud_example_flow_test(interp, pass, fail);
    run_deduce_memory_open_isolation_test(interp, pass, fail);
    run_deduce_reopen_stress_test(interp, pass, fail);
    run_deduce_large_relation_schema_test(interp, pass, fail);
    run_deduce_long_relation_name_isolation_test(interp, pass, fail);
    run_deduce_long_open_path_isolation_test(interp, pass, fail);
    run_deduce_transaction_tests(interp, pass, fail);
    run_deduce_clear_drop_tests(interp, pass, fail);
    run_deduce_legacy_alias_rejection_tests(interp, pass, fail);
    setup_deduce_person_relation(interp);
    run_deduce_query_tests(interp, pass, fail);
}

fn void run_deduce_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    io::printn("\n--- Deduce Tests ---");
    run_group_isolated(&run_deduce_group_tests, pass, fail);
}
