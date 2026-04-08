module lisp;

import std::io;

extern fn char* deduce_test_getenv(char* name) @extern("getenv");

fn bool deduce_group_filter_matches(char* filter, char[] name) {
    if (filter == null) return false;
    usz len = 0;
    while (filter[len] != 0) len++;
    if (len != name.len) return false;
    for (usz i = 0; i < len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return true;
}

fn bool deduce_group_filter_contains_token(char* filter, char[] name) {
    if (filter == null) return false;

    usz i = 0;
    while (filter[i] != 0) {
        while (filter[i] == ' ' || filter[i] == ',') i++;
        if (filter[i] == 0) break;

        usz start = i;
        while (filter[i] != 0 && filter[i] != ',') i++;
        usz end = i;
        while (end > start && filter[end - 1] == ' ') end--;

        usz token_len = end - start;
        if (token_len == name.len) {
            bool matches = true;
            for (usz j = 0; j < token_len; j++) {
                if (filter[start + j] != name[j]) {
                    matches = false;
                    break;
                }
            }
            if (matches) return true;
        }

        if (filter[i] == ',') i++;
    }
    return false;
}

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

    Value*[4] args;
    args[0] = db_val;
    args[1] = name_sym;
    args[2] = c1_sym;
    args[3] = c2_sym;
    return prim_define_relation(args[..], null, interp);
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

fn void run_deduce_namespaced_alias_smoke_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define adb (deduce/open 'memory)) "
        "  (define arel (deduce/open-named adb 'people \"people\" 'name 'age)) "
        "  (deduce/fact! arel \"Alice\" 30) "
        "  (define txr (deduce/block adb 'read)) "
        "  (deduce/abort txr) "
        "  (define txw (deduce/block adb 'write)) "
        "  (deduce/commit txw) "
        "  (define m (deduce/match arel '(people ?name ?age))) "
        "  (define q (deduce/query arel (lambda (row) true))) "
        "  (define s (deduce/scan arel)) "
        "  (define sr (deduce/scan-range arel '(\"Alice\" 30) '(\"Alice\" 30))) "
        "  (deduce/retract! arel \"Alice\" 30) "
        "  (define after-retract (deduce/count arel)) "
        "  (deduce/fact! arel \"Alice\" 30) "
        "  (deduce/clear! arel) "
        "  (define after-clear (deduce/count arel)) "
        "  (deduce/drop! arel) "
        "  (and (= (length m) 1) "
        "       (and (= (length q) 1) "
        "            (and (= (length s) 1) "
        "                 (and (= (length sr) 1) "
        "                      (and (= after-retract 0) "
        "                           (= after-clear 0)))))))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce namespaced aliases");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce namespaced aliases (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] deduce namespaced aliases (tag=%d)", r.value != null ? (int)r.value.tag : -1);
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

fn void run_deduce_relation_open_commit_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force_commit_fail = g_deduce_force_relation_open_commit_fail;
    g_deduce_force_relation_open_commit_fail = true;
    EvalResult open_named_result = run(
        "(handle (deduce 'open-named (deduce 'open 'memory) 'force_commit_fail \"force_commit_fail\" 'name) "
        "  (raise payload (symbol->string (ref payload 'code))))",
        interp
    );
    EvalResult define_relation_result = run(
        "(handle (block "
        "    (define db-force-commit-fail (deduce 'open 'memory)) "
        "    (__define-relation db-force-commit-fail 'force_commit_fail_rel 'name)) "
        "  (raise payload (symbol->string (ref payload 'code))))",
        interp
    );
    g_deduce_force_relation_open_commit_fail = saved_force_commit_fail;

    bool open_named_ok = !open_named_result.error.has_error &&
        open_named_result.value != null &&
        open_named_result.value.tag == STRING &&
        deduce_name_equals(open_named_result.value.str_chars[:open_named_result.value.str_len], "deduce/open-named-txn-commit-failed");
    bool define_relation_ok = !define_relation_result.error.has_error &&
        define_relation_result.value != null &&
        define_relation_result.value.tag == STRING &&
        deduce_name_equals(
            define_relation_result.value.str_chars[:define_relation_result.value.str_len],
            "deduce/define-relation-txn-commit-failed"
        );

    if (open_named_ok && define_relation_ok) {
        emit_pass_literal("[PASS] deduce relation-open surfaces txn commit failure for open-named and define-relation");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce relation-open surfaces txn commit failure for open-named and define-relation");
        (*fail)++;
    }
}

fn void run_deduce_non_transaction_mutation_post_commit_atomic_surface_test(Interp* interp, int* pass, int* fail) {
    EvalResult setup = run(
        "(block "
        "  (define db-post-commit-atomic (deduce 'open 'memory)) "
        "  (define rel-post-commit-atomic (deduce 'open-named db-post-commit-atomic 'post_commit_atomic \"post_commit_atomic\" 'id)) "
        "  (define [relation db-post-commit-atomic materialized] rel-post-commit-atomic-mv (id)) "
        "  true)",
        interp
    );
    if (setup.error.has_error) {
        io::printn("[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures (setup)");
        (*fail)++;
        return;
    }

    bool saved_force_meta_fail = g_deduce_force_materialized_meta_persist_fail;
    g_deduce_force_materialized_meta_persist_fail = true;
    EvalResult r = run(
        "(block "
        "  (define mutation-result (handle (deduce 'fact! rel-post-commit-atomic 7) (raise payload (symbol->string (ref payload 'code))))) "
        "  (and (void? mutation-result) (= (deduce 'count rel-post-commit-atomic) 1)))",
        interp
    );
    g_deduce_force_materialized_meta_persist_fail = saved_force_meta_fail;

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce non-transaction mutations keep success surface after post-commit metadata failures");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn(
                "[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures (error: %s)",
                (ZString)r.error.message[:256]
            );
        } else {
            io::printn("[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures");
        }
        (*fail)++;
    }
}

fn void run_deduce_transaction_mutation_log_failure_bookkeeping_test(Interp* interp, int* pass, int* fail) {
    bool saved_force_log_fail = g_deduce_force_txn_mutation_log_fail;
    g_deduce_force_txn_mutation_log_fail = true;
    EvalResult r = run(
        "(block "
        "  (define db-txn-log-fail (deduce 'open 'memory)) "
        "  (define rel-txn-log-fail (deduce 'open-named db-txn-log-fail 'txn_log_fail \"txn_log_fail\" 'id)) "
        "  (define tx-txn-log-fail (deduce 'block db-txn-log-fail)) "
        "  (deduce 'fact! tx-txn-log-fail rel-txn-log-fail 1) "
        "  (define commit-result (handle (deduce 'commit tx-txn-log-fail) (raise payload (symbol->string (ref payload 'code))))) "
        "  (define analyzed (deduce 'analyze db-txn-log-fail)) "
        "  (and (void? commit-result) "
        "       (= (deduce 'count rel-txn-log-fail) 1) "
        "       (= analyzed.incremental-invalidation-mode 'full-recompute)))",
        interp
    );
    g_deduce_force_txn_mutation_log_fail = saved_force_log_fail;

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce transaction commit records full-recompute bookkeeping when mutation logging fails");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn(
                "[FAIL] deduce transaction commit records full-recompute bookkeeping when mutation logging fails (error: %s)",
                (ZString)r.error.message[:256]
            );
        } else {
            io::printn("[FAIL] deduce transaction commit records full-recompute bookkeeping when mutation logging fails");
        }
        (*fail)++;
    }
}

fn void run_deduce_fact_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vfact (deduce 'open 'memory)) "
        "  (define [relation db-vfact] rel-vfact (name) (age)) "
        "  (deduce 'fact! rel-vfact \"Alice\" 30))",
        "[PASS] deduce fact! returns Void",
        "[FAIL] deduce fact! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_retract_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vretract (deduce 'open 'memory)) "
        "  (define [relation db-vretract] rel-vretract (name) (age)) "
        "  (deduce 'fact! rel-vretract \"Alice\" 30) "
        "  (deduce 'retract! rel-vretract \"Alice\" 30))",
        "[PASS] deduce retract! returns Void",
        "[FAIL] deduce retract! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_commit_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vcommit (deduce 'open 'memory)) "
        "  (define [relation db-vcommit] rel-vcommit (name) (age)) "
        "  (define tx (deduce 'block db-vcommit)) "
        "  (deduce 'fact! tx rel-vcommit \"Alice\" 30) "
        "  (deduce 'commit tx))",
        "[PASS] deduce commit returns Void",
        "[FAIL] deduce commit returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_abort_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vabort (deduce 'open 'memory)) "
        "  (define [relation db-vabort] rel-vabort (name) (age)) "
        "  (define tx (deduce 'block db-vabort 'read)) "
        "  (deduce 'abort tx))",
        "[PASS] deduce abort returns Void",
        "[FAIL] deduce abort returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_clear_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vclear (deduce 'open 'memory)) "
        "  (define [relation db-vclear] rel-vclear (name) (age)) "
        "  (deduce 'fact! rel-vclear \"Alice\" 30) "
        "  (deduce 'clear! rel-vclear))",
        "[PASS] deduce clear! returns Void",
        "[FAIL] deduce clear! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_drop_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vdrop (deduce 'open 'memory)) "
        "  (define [relation db-vdrop] rel-vdrop (name) (age)) "
        "  (deduce 'drop! rel-vdrop))",
        "[PASS] deduce drop! returns Void",
        "[FAIL] deduce drop! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_command_void_return_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_fact_void_return_test(interp, pass, fail);
    run_deduce_retract_void_return_test(interp, pass, fail);
    run_deduce_commit_void_return_test(interp, pass, fail);
    run_deduce_abort_void_return_test(interp, pass, fail);
    run_deduce_clear_void_return_test(interp, pass, fail);
    run_deduce_drop_void_return_test(interp, pass, fail);
}

fn void run_deduce_crud_example_flow_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (let (create1 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        create2 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        existing (repo/get-by-id 101) "
        "        missing (repo/get-by-id 9999)) "
        "    (and (= create1.status \"ok\") "
        "         (and (= create2.status \"error\") "
        "              (and (= create2.error \"item already exists\") "
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
    test_error(interp, "deduce-open rejected", "(deduce-open 'memory)", pass, fail);
    test_error(interp, "fact! rejected", "(fact! nil)", pass, fail);
    test_error(interp, "retract! rejected", "(retract! nil)", pass, fail);
    test_error(interp, "deduce-scan rejected", "(deduce-scan nil)", pass, fail);
    test_error(interp, "deduce-query rejected", "(deduce-query nil nil)", pass, fail);
    test_error(interp, "deduce-count rejected", "(deduce-count nil)", pass, fail);
    test_error(interp, "deduce-match rejected", "(deduce-match nil nil)", pass, fail);
}

fn void run_deduce_basics_block(Interp* interp, int* pass, int* fail) {
    run_deduce_open_smoke_test(interp, pass, fail);
    run_deduce_namespaced_alias_smoke_test(interp, pass, fail);
    run_deduce_open_named_test(interp, pass, fail);
    run_deduce_relation_open_commit_failure_test(interp, pass, fail);
    run_deduce_non_transaction_mutation_post_commit_atomic_surface_test(interp, pass, fail);
    run_deduce_transaction_mutation_log_failure_bookkeeping_test(interp, pass, fail);
    run_deduce_durability_restart_test(interp, pass, fail);
}

fn void run_deduce_materialized_block(Interp* interp, int* pass, int* fail) {
    run_deduce_materialized_intent_restart_test(interp, pass, fail);
    run_deduce_materialized_refresh_restart_test(interp, pass, fail);
    run_deduce_materialized_stale_restart_test(interp, pass, fail);
    run_deduce_materialized_on_read_refresh_restart_test(interp, pass, fail);
    run_deduce_materialized_on_read_stale_restart_test(interp, pass, fail);
    run_deduce_materialized_selective_rule_install_restart_test(interp, pass, fail);
    run_deduce_materialized_drop_restart_test(interp, pass, fail);
    run_deduce_materialized_dematerialize_restart_test(interp, pass, fail);
}

fn void run_deduce_relation_attrs_block(Interp* interp, int* pass, int* fail) {
    run_deduce_relation_attr_define_test(interp, pass, fail);
    run_deduce_relation_attr_materialized_define_test(interp, pass, fail);
    run_deduce_relation_attr_materialized_manual_define_test(interp, pass, fail);
    run_deduce_relation_attr_materialized_on_read_define_test(interp, pass, fail);
    run_deduce_relation_attr_materialized_schema_policy_surface_test(interp, pass, fail);
    run_deduce_relation_attr_materialized_unknown_policy_test(interp, pass, fail);
    run_deduce_relation_attr_check_define_test(interp, pass, fail);
    run_deduce_relation_attr_unknown_attribute_test(interp, pass, fail);
    run_deduce_relation_attr_typed_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_unique_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_composite_unique_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_reference_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_combined_role_test(interp, pass, fail);
}

fn void run_deduce_core_runtime_block(Interp* interp, int* pass, int* fail) {
    run_deduce_crud_example_flow_test(interp, pass, fail);
    run_deduce_memory_open_isolation_test(interp, pass, fail);
    run_deduce_reopen_stress_test(interp, pass, fail);
    run_deduce_large_relation_schema_test(interp, pass, fail);
    run_deduce_long_relation_name_isolation_test(interp, pass, fail);
    run_deduce_long_open_path_isolation_test(interp, pass, fail);
}

fn void run_deduce_integrity_block(Interp* interp, int* pass, int* fail) {
    run_deduce_key_integrity_fact_tests(interp, pass, fail);
    run_deduce_check_integrity_fact_tests(interp, pass, fail);
    run_deduce_rule_head_integrity_tests(interp, pass, fail);
    run_deduce_deferred_integrity_commit_tests(interp, pass, fail);
    run_deduce_reference_retract_integrity_tests(interp, pass, fail);
    run_deduce_reference_bulk_delete_integrity_tests(interp, pass, fail);
}

fn void run_deduce_command_surface_block(Interp* interp, int* pass, int* fail) {
    run_deduce_command_void_return_tests(interp, pass, fail);
    run_deduce_transaction_tests(interp, pass, fail);
    run_deduce_clear_drop_tests(interp, pass, fail);
    run_deduce_legacy_alias_rejection_tests(interp, pass, fail);
    run_deduce_rule_validation_tests(interp, pass, fail);
}

fn void run_deduce_parallel_block(Interp* interp, int* pass, int* fail) {
    run_deduce_parallel_component_delta_payload_roundtrip_test(interp, pass, fail);
    run_deduce_parallel_component_scratch_pass_test(interp, pass, fail);
    run_deduce_parallel_component_scratch_closure_test(interp, pass, fail);
    run_deduce_parallel_component_scratch_multi_atom_closure_test(interp, pass, fail);
    run_deduce_parallel_component_delta_apply_test(interp, pass, fail);
}

fn void run_deduce_query_block(Interp* interp, int* pass, int* fail) {
    setup_deduce_person_relation(interp);
    run_deduce_query_tests(interp, pass, fail);
}

fn void run_deduce_group_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_test_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        run_deduce_query_tests(interp, pass, fail);
        return;
    }

    char* deduce_group_filter = deduce_test_getenv("OMNI_DEDUCE_GROUP_FILTER");
    if (deduce_group_filter != null) {
        bool matched = false;
        if (deduce_group_filter_contains_token(deduce_group_filter, "basics")) {
            run_deduce_basics_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "materialized")) {
            run_deduce_materialized_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "relation-attrs")) {
            run_deduce_relation_attrs_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "core-runtime")) {
            run_deduce_core_runtime_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "integrity")) {
            run_deduce_integrity_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "command-surface")) {
            run_deduce_command_surface_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "command-void")) {
            run_deduce_command_void_return_tests(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "fact-void")) {
            run_deduce_fact_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "retract-void")) {
            run_deduce_retract_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "commit-void")) {
            run_deduce_commit_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "abort-void")) {
            run_deduce_abort_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "clear-void")) {
            run_deduce_clear_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "drop-void")) {
            run_deduce_drop_void_return_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "transaction")) {
            run_deduce_transaction_tests(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "txn-commit")) {
            run_deduce_transaction_commit_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "txn-abort")) {
            run_deduce_transaction_abort_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "txn-db-mismatch")) {
            run_deduce_transaction_db_mismatch_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "clear-drop")) {
            run_deduce_clear_drop_tests(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "clear-preserve")) {
            run_deduce_clear_preserves_relation_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "drop-invalidates")) {
            run_deduce_drop_invalidates_relation_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "clear-empty")) {
            run_deduce_clear_empty_relation_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "retract-missing-noop")) {
            run_deduce_retract_missing_tuple_noop_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "drop-repeat-noop")) {
            run_deduce_drop_repeat_noop_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "dropped-cleanup-error")) {
            run_deduce_dropped_cleanup_error_test(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "legacy-alias")) {
            run_deduce_legacy_alias_rejection_tests(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "rule-validation")) {
            run_deduce_rule_validation_tests(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "parallel")) {
            run_deduce_parallel_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_contains_token(deduce_group_filter, "query")) {
            run_deduce_query_block(interp, pass, fail);
            matched = true;
        }
        if (deduce_group_filter_matches(deduce_group_filter, "parallel_component_scratch_pass")) {
            run_deduce_parallel_component_scratch_pass_test(interp, pass, fail);
            matched = true;
        } else if (deduce_group_filter_matches(deduce_group_filter, "rule_head_reference_snapshot")) {
            run_deduce_rule_head_reference_snapshot_test(interp, pass, fail);
            matched = true;
        }
        if (!matched) {
            io::printfn("[FAIL] deduce group filter matched no tests: %s", (ZString)deduce_group_filter);
            (*fail)++;
        }
        return;
    }

    char* deduce_query_filter = deduce_test_getenv("OMNI_DEDUCE_QUERY_FILTER");
    if (deduce_query_filter != null) {
        run_deduce_query_block(interp, pass, fail);
        return;
    }

    run_deduce_basics_block(interp, pass, fail);
    run_deduce_materialized_block(interp, pass, fail);
    run_deduce_relation_attrs_block(interp, pass, fail);
    run_deduce_core_runtime_block(interp, pass, fail);
    run_deduce_integrity_block(interp, pass, fail);
    run_deduce_command_surface_block(interp, pass, fail);
    run_deduce_parallel_block(interp, pass, fail);
    run_deduce_query_block(interp, pass, fail);
}

fn bool run_named_deduce_group_isolated(
    char* filter,
    char[] token,
    IsolatedTestGroupFn group_fn,
    int* pass,
    int* fail
) {
    if (!deduce_group_filter_contains_token(filter, token)) return false;
    run_group_isolated(group_fn, pass, fail);
    return true;
}

fn bool run_exact_deduce_group_isolated(
    char* filter,
    char[] name,
    IsolatedTestGroupFn group_fn,
    int* pass,
    int* fail
) {
    if (!deduce_group_filter_matches(filter, name)) return false;
    run_group_isolated(group_fn, pass, fail);
    return true;
}

fn void run_deduce_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    emit_test_section_banner("Deduce Tests");

    if (deduce_test_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        run_group_isolated(&run_deduce_query_tests, pass, fail);
        return;
    }

    char* deduce_group_filter = deduce_test_getenv("OMNI_DEDUCE_GROUP_FILTER");
    if (deduce_group_filter != null) {
        bool matched_any = false;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "basics", &run_deduce_basics_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "materialized", &run_deduce_materialized_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "relation-attrs", &run_deduce_relation_attrs_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "core-runtime", &run_deduce_core_runtime_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "integrity", &run_deduce_integrity_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "command-surface", &run_deduce_command_surface_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "command-void", &run_deduce_command_void_return_tests, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "fact-void", &run_deduce_fact_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "retract-void", &run_deduce_retract_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "commit-void", &run_deduce_commit_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "abort-void", &run_deduce_abort_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "clear-void", &run_deduce_clear_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "drop-void", &run_deduce_drop_void_return_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "transaction", &run_deduce_transaction_tests, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "txn-commit", &run_deduce_transaction_commit_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "txn-abort", &run_deduce_transaction_abort_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "txn-db-mismatch", &run_deduce_transaction_db_mismatch_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "clear-drop", &run_deduce_clear_drop_tests, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "clear-preserve", &run_deduce_clear_preserves_relation_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "drop-invalidates", &run_deduce_drop_invalidates_relation_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "clear-empty", &run_deduce_clear_empty_relation_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "retract-missing-noop", &run_deduce_retract_missing_tuple_noop_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "drop-repeat-noop", &run_deduce_drop_repeat_noop_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "dropped-cleanup-error", &run_deduce_dropped_cleanup_error_test, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "legacy-alias", &run_deduce_legacy_alias_rejection_tests, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "rule-validation", &run_deduce_rule_validation_tests, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "parallel", &run_deduce_parallel_block, pass, fail
        ) || matched_any;
        matched_any = run_named_deduce_group_isolated(
            deduce_group_filter, "query", &run_deduce_query_block, pass, fail
        ) || matched_any;
        matched_any = run_exact_deduce_group_isolated(
            deduce_group_filter, "parallel_component_scratch_pass",
            &run_deduce_parallel_component_scratch_pass_test, pass, fail
        ) || matched_any;
        matched_any = run_exact_deduce_group_isolated(
            deduce_group_filter, "rule_head_reference_snapshot",
            &run_deduce_rule_head_reference_snapshot_test, pass, fail
        ) || matched_any;
        if (!matched_any) {
            io::printfn("[FAIL] deduce group filter matched no tests: %s", (ZString)deduce_group_filter);
            (*fail)++;
        }
        return;
    }

    if (deduce_test_getenv("OMNI_DEDUCE_QUERY_FILTER") != null) {
        run_group_isolated(&run_deduce_query_block, pass, fail);
        return;
    }

    run_group_isolated(&run_deduce_basics_block, pass, fail);
    run_group_isolated(&run_deduce_materialized_block, pass, fail);
    run_group_isolated(&run_deduce_relation_attrs_block, pass, fail);
    run_group_isolated(&run_deduce_core_runtime_block, pass, fail);
    run_group_isolated(&run_deduce_integrity_block, pass, fail);
    run_group_isolated(&run_deduce_command_surface_block, pass, fail);
    run_group_isolated(&run_deduce_parallel_block, pass, fail);
    run_group_isolated(&run_deduce_query_block, pass, fail);
}
