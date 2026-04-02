module lisp;

import std::core::mem;
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
        "       (= ('incremental-invalidation-mode analyzed) 'full-recompute)))",
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

fn void run_deduce_relation_attr_materialized_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized materialized] rel-attr-materialized (src) (dst)) "
        "  ddb-rel-materialized)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("manual") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized] marks relation schema for manual materialization but not readiness");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_manual_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized-manual");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-manual (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-manual materialized manual] rel-attr-materialized-manual (src) (dst)) "
        "  ddb-rel-materialized-manual)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("manual") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized manual] declares manual materialization policy explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized manual] declares manual materialization policy explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized manual] declares manual materialization policy explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_on_read_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-materialized-on-read");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-on-read (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-on-read materialized on-read] rel-attr-materialized-on-read (src) (dst)) "
        "  ddb-rel-materialized-on-read)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.materialized_view &&
             schema.materialized_refresh_policy == interp.symbols.intern("on-read") &&
             schema.materialized_refresh_count == 0 &&
             !deduce_relation_schema_is_materialized_ready(db, schema);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... materialized on-read] declares on-read materialization policy explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_schema_policy_surface_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-materialized-schema (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-schema materialized] rel-attr-materialized-schema-implicit (src) (dst)) "
        "  (define [relation ddb-rel-materialized-schema materialized manual] rel-attr-materialized-schema-explicit (src) (dst)) "
        "  (define [relation ddb-rel-materialized-schema materialized on-read] rel-attr-materialized-schema-on-read (src) (dst)) "
        "  (define implicit-schema (deduce/schema rel-attr-materialized-schema-implicit)) "
        "  (define explicit-schema (deduce/schema rel-attr-materialized-schema-explicit)) "
        "  (define on-read-schema (deduce/schema rel-attr-materialized-schema-on-read)) "
        "  (and "
        "    (= (ref implicit-schema 'materialized-view) true) "
        "    (= (ref implicit-schema 'materialized-refresh-policy) 'manual) "
        "    (= (ref implicit-schema 'materialized-derived-ready) false) "
        "    (= (ref implicit-schema 'materialized-stale) true) "
        "    (= (ref implicit-schema 'materialized-last-stale-reason) 'never-refreshed) "
        "    (= (ref explicit-schema 'materialized-view) true) "
        "    (= (ref explicit-schema 'materialized-refresh-policy) 'manual) "
        "    (= (ref explicit-schema 'materialized-derived-ready) false) "
        "    (= (ref explicit-schema 'materialized-stale) true) "
        "    (= (ref explicit-schema 'materialized-last-stale-reason) 'never-refreshed) "
        "    (= (ref on-read-schema 'materialized-view) true) "
        "    (= (ref on-read-schema 'materialized-refresh-policy) 'on-read) "
        "    (= (ref on-read-schema 'materialized-derived-ready) false) "
        "    (= (ref on-read-schema 'materialized-stale) true) "
        "    (= (ref on-read-schema 'materialized-last-stale-reason) 'never-refreshed)))",
        interp
    );

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce/schema exposes declared materialization policies faithfully");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce/schema exposes declared materialization policies faithfully (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce/schema exposes declared materialization policies faithfully");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_materialized_unknown_policy_test(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "[PASS] deduce [relation ... materialized] rejects unknown refresh policies",
        "(block "
        "  (define ddb-rel-materialized-policy (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-materialized-policy materialized eager] rel-attr-materialized-policy (id)) "
        "  0)",
        "unknown materialized refresh policy",
        pass,
        fail
    );
}

fn void run_deduce_relation_attr_check_define_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-check");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-check (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-check] rel-attr-check (id) (check positive amount)) "
        "  ddb-rel-check)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.check_constraint_count == 1 &&
             schema.check_constraints != null &&
             schema.check_constraints[0].local_column == interp.symbols.intern("amount") &&
             schema.check_constraints[0].predicate == interp.symbols.intern("positive");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation ... check predicate column] declares check constraint metadata explicitly");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation ... check predicate column] declares check constraint metadata explicitly (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation ... check predicate column] declares check constraint metadata explicitly");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_unknown_attribute_test(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "[PASS] deduce [relation] rejects unknown relation attributes",
        "(block "
        "  (define ddb-rel-unknown-attr (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unknown-attr mystery] rel-attr-unknown (id)) "
        "  0)",
        "unknown relation attribute",
        pass,
        fail
    );
}

fn void run_deduce_relation_attr_typed_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-schema");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-schema (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-schema] rel-attr-schema "
        "    (key ^String name) "
        "    (index ^Integer age) "
        "    (^String email)) "
        "  ddb-rel-schema)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[3] full = { 0, 1, 2 };
        usz[1] key_col = { 0 };
        usz[1] index_col = { 1 };
        usz[2] key_index = { 0, 1 };
        ok = schema != null &&
             schema.arity == 3 &&
             deduce_rule_test_schema_has_index(schema, full[..], true) &&
             deduce_rule_test_schema_has_index(schema, key_col[..], true) &&
             deduce_rule_test_schema_has_index(schema, index_col[..], false) &&
             deduce_rule_test_schema_has_index(schema, key_index[..], false);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] typed fields with key/index roles");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] typed fields with key/index roles (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] typed fields with key/index roles");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_unique_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-unique");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-unique (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unique] rel-attr-unique "
        "    (^Integer id) "
        "    (unique ^String email) "
        "    (index ^String dept)) "
        "  ddb-rel-unique)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[1] unique_col = { 1 };
        usz[1] index_col = { 2 };
        ok = schema != null &&
             schema.arity == 3 &&
             schema.unique_column == interp.symbols.intern("email") &&
             deduce_rule_test_schema_has_index(schema, unique_col[..], true) &&
             deduce_rule_test_schema_has_index(schema, index_col[..], false);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] unique role adds enforced unique index");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] unique role adds enforced unique index (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] unique role adds enforced unique index");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_composite_unique_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-unique-composite");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-unique-composite (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-unique-composite] rel-attr-unique-composite "
        "    (^Integer id) "
        "    (unique ^String email) "
        "    (unique ^String dept)) "
        "  ddb-rel-unique-composite)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[2] unique_cols = { 1, 2 };
        ok = schema != null &&
             schema.arity == 3 &&
             schema.unique_column == (SymbolId)0 &&
             schema.unique_column_count == 2 &&
             schema.unique_columns != null &&
             schema.unique_columns[0] == interp.symbols.intern("email") &&
             schema.unique_columns[1] == interp.symbols.intern("dept") &&
             deduce_rule_test_schema_has_index(schema, unique_cols[..], true);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] composite unique role adds enforced composite index");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] composite unique role adds enforced composite index (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] composite unique role adds enforced composite index");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_reference_role_schema_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-reference");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-reference (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-reference] users (key id) (name)) "
        "  (define [relation ddb-rel-reference] rel-attr-reference "
        "    (ref users id ^Integer user-id) "
        "    (^String action)) "
        "  ddb-rel-reference)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        ok = schema != null &&
             schema.arity == 2 &&
             schema.reference_constraint_count == 1 &&
             schema.reference_constraints != null &&
             schema.reference_constraints[0].local_column == interp.symbols.intern("user-id") &&
             schema.reference_constraints[0].target_relation == interp.symbols.intern("users") &&
             schema.reference_constraints[0].target_column == interp.symbols.intern("id");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] ref role records enforced reference metadata");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] ref role records enforced reference metadata (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] ref role records enforced reference metadata");
        }
        (*fail)++;
    }
}

fn void run_deduce_relation_attr_combined_role_test(Interp* interp, int* pass, int* fail) {
    SymbolId rel_sym = interp.symbols.intern("rel-attr-combined");
    EvalResult r = run(
        "(block "
        "  (define ddb-rel-combined (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-combined] rel-attr-combined "
        "    (key index ^String id) "
        "    (^String email)) "
        "  ddb-rel-combined)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
        usz[2] full = { 0, 1 };
        usz[1] key_index_col = { 0 };
        ok = schema != null &&
             schema.arity == 2 &&
             deduce_rule_test_schema_has_index(schema, full[..], true) &&
             deduce_rule_test_schema_has_index(schema, key_index_col[..], true);
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce [relation] combined key/index role markers");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce [relation] combined key/index role markers (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce [relation] combined key/index role markers");
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

fn void run_deduce_key_integrity_fact_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-ok (deduce 'open 'memory)) "
        "  (define [relation db-key-ok] rel-key-ok (key id) (name)) "
        "  (deduce 'fact! rel-key-ok 1 \"Alice\") "
        "  (deduce 'fact! rel-key-ok 1 \"Alice\") "
        "  (= (deduce 'count rel-key-ok) 1))",
        "[PASS] deduce keyed fact! remains idempotent for identical tuple",
        "[FAIL] deduce keyed fact! remains idempotent for identical tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-conflict (deduce 'open 'memory)) "
        "  (define [relation db-key-conflict] rel-key-conflict (key id) (name)) "
        "  (deduce 'fact! rel-key-conflict 1 \"Alice\") "
        "  (define payload (handle (deduce 'fact! rel-key-conflict 1 \"Bob\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-key-conflict) "
        "                             (and (= (ref data 'key-column) 'id) "
        "                                  (and (= (ref data 'key-value) 1) "
        "                                       (and (= (ref data 'violation-kind) 'key-conflict) "
        "                                            (= (car (ref data 'existing-tuple)) 1)))))))))))",
        "[PASS] deduce keyed fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce keyed fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-tx (deduce 'open 'memory)) "
        "  (define [relation db-key-tx] rel-key-tx (key id) (name)) "
        "  (deduce 'fact! rel-key-tx 1 \"Alice\") "
        "  (define tx (deduce 'block db-key-tx)) "
        "  (define payload (handle (deduce 'fact! tx rel-key-tx 1 \"Bob\") (raise msg msg))) "
        "  (deduce 'abort tx) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "            (= (deduce 'count rel-key-tx) 1))))",
        "[PASS] deduce keyed fact! rejects conflicting transaction write",
        "[FAIL] deduce keyed fact! rejects conflicting transaction write",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-conflict (deduce 'open 'memory)) "
        "  (define [relation db-unique-conflict] rel-unique-conflict (id) (unique email)) "
        "  (deduce 'fact! rel-unique-conflict 1 \"alice@x\") "
        "  (define payload (handle (deduce 'fact! rel-unique-conflict 2 \"alice@x\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-unique-conflict) "
        "                             (and (= (ref data 'unique-column) 'email) "
        "                                  (and (= (ref data 'key-value) \"alice@x\") "
        "                                       (and (= (ref data 'violation-kind) 'unique-conflict) "
        "                                            (= (car (ref data 'existing-tuple)) 1)))))))))))",
        "[PASS] deduce unique fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce unique fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-composite-conflict (deduce 'open 'memory)) "
        "  (define [relation db-unique-composite-conflict] rel-unique-composite-conflict (id) (unique email) (unique dept)) "
        "  (deduce 'fact! rel-unique-composite-conflict 1 \"alice@x\" \"ops\") "
        "  (deduce 'fact! rel-unique-composite-conflict 2 \"alice@x\" \"sales\") "
        "  (define payload (handle (deduce 'fact! rel-unique-composite-conflict 3 \"alice@x\" \"ops\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-unique-composite-conflict) "
        "                             (and (= (car (ref data 'columns)) 'email) "
        "                                  (and (= (car (cdr (ref data 'columns))) 'dept) "
        "                                       (and (= (car (ref data 'constraint-values)) \"alice@x\") "
        "                                            (and (= (car (cdr (ref data 'constraint-values))) \"ops\") "
        "                                                 (and (= (ref data 'violation-kind) 'unique-conflict) "
        "                                                      (= (car (ref data 'existing-tuple)) 1)))))))))))))",
        "[PASS] deduce composite unique fact! exposes machine-checkable conflict payload",
        "[FAIL] deduce composite unique fact! exposes machine-checkable conflict payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-ref-ok] ref-users-ok (key id) (name)) "
        "  (define [relation db-ref-ok] ref-audit-ok (ref ref-users-ok id user-id) (action)) "
        "  (deduce 'fact! ref-users-ok 42 \"Alice\") "
        "  (deduce 'fact! ref-audit-ok 42 \"login\") "
        "  (= (deduce 'count ref-audit-ok) 1))",
        "[PASS] deduce reference fact! accepts existing referenced tuple",
        "[FAIL] deduce reference fact! accepts existing referenced tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-missing (deduce 'open 'memory)) "
        "  (define [relation db-ref-missing] ref-users-missing (key id) (name)) "
        "  (define [relation db-ref-missing] ref-audit-missing (ref ref-users-missing id user-id) (action)) "
        "  (define payload (handle (deduce 'fact! ref-audit-missing 42 \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'ref-audit-missing) "
        "                             (and (= (ref data 'referenced-relation) 'ref-users-missing) "
        "                                  (and (= (ref data 'referenced-column) 'id) "
        "                                       (and (= (ref data 'key-value) 42) "
        "                                            (= (ref data 'violation-kind) 'reference-missing)))))))))))",
        "[PASS] deduce reference fact! exposes machine-checkable missing-target payload",
        "[FAIL] deduce reference fact! exposes machine-checkable missing-target payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-nonunique (deduce 'open 'memory)) "
        "  (define [relation db-ref-nonunique] ref-users-nonunique (id) (email)) "
        "  (define [relation db-ref-nonunique] ref-audit-nonunique (ref ref-users-nonunique email user-email) (action)) "
        "  (deduce 'fact! ref-users-nonunique 1 \"alice@x\") "
        "  (define payload (handle (deduce 'fact! ref-audit-nonunique \"alice@x\" \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-reference-target-not-unique) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'ref-audit-nonunique) "
        "                             (and (= (ref data 'referenced-relation) 'ref-users-nonunique) "
        "                                  (and (= (ref data 'referenced-column) 'email) "
        "                                       (and (= (ref data 'key-value) \"alice@x\") "
        "                                            (= (ref data 'violation-kind) 'reference-target-not-unique)))))))))))",
        "[PASS] deduce reference fact! rejects non-unique target columns",
        "[FAIL] deduce reference fact! rejects non-unique target columns",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-dropped (deduce 'open 'memory)) "
        "  (define [relation db-ref-dropped] ref-users-dropped (key id) (name)) "
        "  (define [relation db-ref-dropped] ref-audit-dropped (ref ref-users-dropped id user-id) (action)) "
        "  (deduce 'drop! ref-users-dropped) "
        "  (define payload (handle (deduce 'fact! ref-audit-dropped 42 \"login\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (= (ref payload 'code) 'deduce/integrity-reference-target-missing))))",
        "[PASS] deduce reference fact! stays consistent after target relation is dropped",
        "[FAIL] deduce reference fact! stays consistent after target relation is dropped",
        pass,
        fail
    );
}

fn void run_deduce_check_integrity_fact_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-ok (deduce 'open 'memory)) "
        "  (define [relation db-check-ok] rel-check-ok (id) (check int? amount)) "
        "  (deduce 'fact! rel-check-ok 1 7) "
        "  (= (deduce 'count rel-check-ok) 1))",
        "[PASS] deduce check fact! accepts tuples that satisfy the predicate",
        "[FAIL] deduce check fact! accepts tuples that satisfy the predicate",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-fail (deduce 'open 'memory)) "
        "  (define [relation db-check-fail] rel-check-fail (id) (check int? amount)) "
        "  (define payload (handle (deduce 'fact! rel-check-fail 1 \"oops\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'domain) 'deduce) "
        "            (and (= (ref payload 'code) 'deduce/integrity-check-failed) "
        "                 (let (data (ref payload 'data)) "
        "                   (and (dict? data) "
        "                        (and (= (ref data 'relation) 'rel-check-fail) "
        "                             (and (= (ref data 'predicate) 'int?) "
        "                                  (and (= (ref data 'key-value) \"oops\") "
        "                                       (= (ref data 'violation-kind) 'check-failed)))))))))))",
        "[PASS] deduce check fact! exposes machine-checkable failure payload",
        "[FAIL] deduce check fact! exposes machine-checkable failure payload",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-missing (deduce 'open 'memory)) "
        "  (define [relation db-check-missing] rel-check-missing (id) (check missing-check-predicate amount)) "
        "  (define payload (handle (deduce 'fact! rel-check-missing 1 7) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/check-predicate-missing) "
        "       (= (deduce 'count rel-check-missing) 0)))",
        "[PASS] deduce check fact! rejects missing predicate symbols deterministically",
        "[FAIL] deduce check fact! rejects missing predicate symbols deterministically",
        pass,
        fail
    );
}

fn void run_deduce_rule_head_reference_snapshot_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-ref-ok] user-src (id) (name)) "
        "  (define [relation db-rule-ref-ok] audit-src (user-id) (action)) "
        "  (define [relation db-rule-ref-ok] users (key id) (name)) "
        "  (define [relation db-rule-ref-ok] audit-out (ref users id user-id) (action)) "
        "  (deduce 'fact! user-src 1 \"Alice\") "
        "  (deduce 'fact! audit-src 1 \"login\") "
        "  (deduce/rule! db-rule-ref-ok '(audit-out ?u ?a) '(audit-src ?u ?a)) "
        "  (deduce/rule! db-rule-ref-ok '(audit-out ?u ?a) '(users ?u ?name) '(audit-src ?u ?a)) "
        "  (deduce/rule! db-rule-ref-ok '(users ?u ?name) '(audit-out ?u ?a) '(user-src ?u ?name)) "
        "  (define analyzed (deduce 'analyze db-rule-ref-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (and (= (deduce 'count users) 1) "
        "            (= (deduce 'count audit-out) 1))))",
        "[PASS] deduce rule-derived reference heads validate against final recursive component snapshot",
        "[FAIL] deduce rule-derived reference heads validate against final recursive component snapshot",
        pass,
        fail
    );
}

fn void run_deduce_rule_head_integrity_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-key-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-key-conflict] user-src (id) (name)) "
            "  (define [relation db-rule-key-conflict] users (key id) (name)) "
            "  (deduce 'fact! user-src 1 \"Alice\") "
            "  (deduce 'fact! user-src 1 \"Bob\") "
            "  (deduce/rule! db-rule-key-conflict '(users ?id ?name) '(user-src ?id ?name)) "
            "  db-rule-key-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* key_column = deduce_rule_test_dict_get(interp, data, "key-column");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-key-conflict") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "users") &&
                 key_column != null &&
                 key_column.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(key_column.sym_val), "id") &&
                 is_int(key_value) &&
                 key_value.int_val == 1;
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived keyed tuples raise machine-checkable conflict payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived keyed tuples raise machine-checkable conflict payloads");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-unique-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-unique-conflict] email-src (id) (email)) "
            "  (define [relation db-rule-unique-conflict] emails (id) (unique email)) "
            "  (deduce 'fact! email-src 1 \"alice@x\") "
            "  (deduce 'fact! email-src 2 \"alice@x\") "
            "  (deduce/rule! db-rule-unique-conflict '(emails ?id ?email) '(email-src ?id ?email)) "
            "  db-rule-unique-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* unique_column = deduce_rule_test_dict_get(interp, data, "unique-column");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-unique-conflict") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "emails") &&
                 unique_column != null &&
                 unique_column.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(unique_column.sym_val), "email") &&
                 deduce_test_string_equals(key_value, "alice@x");
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived unique tuples raise machine-checkable conflict payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived unique tuples raise machine-checkable conflict payloads");
            (*fail)++;
        }
    }

    run_deduce_rule_head_reference_snapshot_test(interp, pass, fail);

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-ref-missing (deduce 'open 'memory)) "
        "  (define [relation db-rule-ref-missing] audit-src (user-id) (action)) "
        "  (define [relation db-rule-ref-missing] users (key id) (name)) "
        "  (define [relation db-rule-ref-missing] audit-out (ref users id user-id) (action)) "
        "  (deduce 'fact! audit-src 42 \"login\") "
        "  (deduce/rule! db-rule-ref-missing '(audit-out ?u ?a) '(audit-src ?u ?a)) "
        "  (define payload (handle (deduce 'analyze db-rule-ref-missing) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "            (let (data (ref payload 'data)) "
        "              (and (dict? data) "
        "                   (and (= (ref data 'relation) 'audit-out) "
        "                        (and (= (ref data 'referenced-relation) 'users) "
        "                             (and (= (ref data 'referenced-column) 'id) "
        "                                  (and (= (ref data 'key-value) 42) "
        "                                       (= (ref data 'violation-kind) 'reference-missing))))))))))",
        "[PASS] deduce rule-derived reference heads raise machine-checkable missing-target payloads",
        "[FAIL] deduce rule-derived reference heads raise machine-checkable missing-target payloads",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-key-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-key-ok] sale (dept) (amount)) "
        "  (define [relation db-rule-agg-key-ok] total-by-dept (key dept) (total)) "
        "  (deduce 'fact! sale \"eng\" 10) "
        "  (deduce 'fact! sale \"eng\" 15) "
        "  (deduce 'fact! sale \"sales\" 3) "
        "  (deduce/rule! db-rule-agg-key-ok '(total-by-dept ?dept (sum ?amount)) '(sale ?dept ?amount)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-key-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (length (deduce 'scan total-by-dept)) 2)))",
        "[PASS] deduce aggregate keyed rule heads publish with integrity enforcement",
        "[FAIL] deduce aggregate keyed rule heads publish with integrity enforcement",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-unique-conflict (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-unique-conflict] sale (dept) (amount)) "
        "  (define [relation db-rule-agg-unique-conflict] total-by-dept (dept) (unique total)) "
        "  (deduce 'fact! sale \"eng\" 10) "
        "  (deduce 'fact! sale \"sales\" 10) "
        "  (deduce/rule! db-rule-agg-unique-conflict '(total-by-dept ?dept (sum ?amount)) '(sale ?dept ?amount)) "
        "  (define payload (handle (deduce 'analyze db-rule-agg-unique-conflict) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "            (let (data (ref payload 'data)) "
        "              (and (dict? data) "
        "                   (and (= (ref data 'relation) 'total-by-dept) "
        "                        (and (= (ref data 'unique-column) 'total) "
        "                             (= (ref data 'key-value) 10))))))))",
        "[PASS] deduce aggregate unique rule heads raise machine-checkable conflict payloads",
        "[FAIL] deduce aggregate unique rule heads raise machine-checkable conflict payloads",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-ref-ok (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-ref-ok] users (key id) (name)) "
        "  (define [relation db-rule-agg-ref-ok] sale (user-id) (amount)) "
        "  (define [relation db-rule-agg-ref-ok] totals (ref users id user-id) (total)) "
        "  (deduce 'fact! users 1 \"Alice\") "
        "  (deduce 'fact! sale 1 10) "
        "  (deduce 'fact! sale 1 15) "
        "  (deduce/rule! db-rule-agg-ref-ok '(totals ?u (sum ?amount)) '(sale ?u ?amount)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-ref-ok)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (deduce 'count totals) 1)))",
        "[PASS] deduce aggregate reference rule heads publish with final-snapshot integrity validation",
        "[FAIL] deduce aggregate reference rule heads publish with final-snapshot integrity validation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-rule-agg-key-rec (deduce 'open 'memory)) "
        "  (define [relation db-rule-agg-key-rec] edge (src) (dst)) "
        "  (define [relation db-rule-agg-key-rec] unit (node) (n)) "
        "  (define [relation db-rule-agg-key-rec] total (key node) (total)) "
        "  (deduce 'fact! edge 1 2) "
        "  (deduce 'fact! edge 2 3) "
        "  (deduce 'fact! unit 3 1) "
        "  (deduce/rule! db-rule-agg-key-rec '(total ?x (sum ?n)) '(unit ?x ?n)) "
        "  (deduce/rule! db-rule-agg-key-rec '(total ?x (sum ?n)) '(edge ?x ?y) '(total ?y ?n)) "
        "  (define analyzed (deduce 'analyze db-rule-agg-key-rec)) "
        "  (and (= (ref analyzed 'status) 'ok) "
        "       (= (length (deduce 'scan total)) 3)))",
        "[PASS] deduce recursive aggregate keyed rule heads run through seminaive integrity enforcement",
        "[FAIL] deduce recursive aggregate keyed rule heads run through seminaive integrity enforcement",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(block "
            "  (define db-rule-check-conflict (deduce 'open 'memory)) "
            "  (define [relation db-rule-check-conflict] amount-src (id) (amount)) "
            "  (define [relation db-rule-check-conflict] checked-out (id) (check int? amount)) "
            "  (deduce 'fact! amount-src 1 7) "
            "  (deduce 'fact! amount-src 2 \"oops\") "
            "  (deduce/rule! db-rule-check-conflict '(checked-out ?id ?amount) '(amount-src ?id ?amount)) "
            "  db-rule-check-conflict)",
            interp
        );
        Value* payload = null;
        bool ok = !r.error.has_error &&
                  deduce_test_capture_analyze_raise_payload(interp, r.value, &payload);
        if (ok) {
            Value* code = deduce_rule_test_dict_get(interp, payload, "code");
            Value* data = deduce_rule_test_dict_get(interp, payload, "data");
            Value* relation = deduce_rule_test_dict_get(interp, data, "relation");
            Value* predicate = deduce_rule_test_dict_get(interp, data, "predicate");
            Value* key_value = deduce_rule_test_dict_get(interp, data, "key-value");
            ok = code != null &&
                 code.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(code.sym_val), "deduce/integrity-check-failed") &&
                 relation != null &&
                 relation.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(relation.sym_val), "checked-out") &&
                 predicate != null &&
                 predicate.tag == SYMBOL &&
                 deduce_name_equals(interp.symbols.get_name(predicate.sym_val), "int?") &&
                 deduce_test_string_equals(key_value, "oops");
        }
        run_clear_stale_raise_state(interp);
        if (ok) {
            emit_pass_literal("[PASS] deduce rule-derived check constraints raise machine-checkable failure payloads");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce rule-derived check constraints raise machine-checkable failure payloads");
            (*fail)++;
        }
    }

}

fn void run_deduce_deferred_integrity_commit_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-deferred-ok (deduce 'open 'memory)) "
        "  (define [relation db-deferred-ok] users-deferred-ok (key id) (name)) "
        "  (define [relation db-deferred-ok] audit-deferred-ok (ref users-deferred-ok id user-id) (action)) "
        "  (define tx (deduce 'block db-deferred-ok 'write-deferred)) "
        "  (deduce 'fact! tx users-deferred-ok 42 \"Alice\") "
        "  (deduce 'fact! tx audit-deferred-ok 42 \"login\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-deferred-ok) 1) "
        "       (= (deduce 'count audit-deferred-ok) 1)))",
        "[PASS] deduce deferred write commit validates staged reference success",
        "[FAIL] deduce deferred write commit validates staged reference success",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-key-deferred (deduce 'open 'memory)) "
        "  (define [relation db-key-deferred] rel-key-deferred (key id) (name)) "
        "  (deduce 'fact! rel-key-deferred 1 \"Alice\") "
        "  (define tx (deduce 'block db-key-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-key-deferred 1 \"Bob\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-key-conflict) "
        "       (= (deduce 'count rel-key-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects keyed conflict and rolls back",
        "[FAIL] deduce deferred write commit rejects keyed conflict and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-unique-deferred (deduce 'open 'memory)) "
        "  (define [relation db-unique-deferred] rel-unique-deferred (id) (unique email)) "
        "  (deduce 'fact! rel-unique-deferred 1 \"alice@x\") "
        "  (define tx (deduce 'block db-unique-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-unique-deferred 2 \"alice@x\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-unique-conflict) "
        "       (= (deduce 'count rel-unique-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects unique conflict and rolls back",
        "[FAIL] deduce deferred write commit rejects unique conflict and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-deferred] users-ref-deferred (key id) (name)) "
        "  (define [relation db-ref-deferred] audit-ref-deferred (ref users-ref-deferred id user-id) (action)) "
        "  (define tx (deduce 'block db-ref-deferred 'write-deferred)) "
        "  (deduce 'fact! tx audit-ref-deferred 42 \"login\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "       (= (deduce 'count audit-ref-deferred) 0)))",
        "[PASS] deduce deferred write commit rejects missing reference and rolls back",
        "[FAIL] deduce deferred write commit rejects missing reference and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-check-deferred (deduce 'open 'memory)) "
        "  (define [relation db-check-deferred] rel-check-deferred (id) (check int? amount)) "
        "  (define tx (deduce 'block db-check-deferred 'write-deferred)) "
        "  (deduce 'fact! tx rel-check-deferred 1 \"oops\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-check-failed) "
        "       (= (deduce 'count rel-check-deferred) 0)))",
        "[PASS] deduce deferred write commit rejects failed check constraints and rolls back",
        "[FAIL] deduce deferred write commit rejects failed check constraints and rolls back",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-delete-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-delete-deferred] users-ref-delete-deferred (key id) (name)) "
        "  (define [relation db-ref-delete-deferred] audit-ref-delete-deferred (ref users-ref-delete-deferred id user-id) (action)) "
        "  (deduce 'fact! users-ref-delete-deferred 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-delete-deferred 42 \"login\") "
        "  (define tx (deduce 'block db-ref-delete-deferred 'write-deferred)) "
        "  (deduce 'retract! tx users-ref-delete-deferred 42 \"Alice\") "
        "  (define payload (handle (deduce 'commit tx) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-missing) "
        "       (= (deduce 'count users-ref-delete-deferred) 1) "
        "       (= (deduce 'count audit-ref-delete-deferred) 1)))",
        "[PASS] deduce deferred write commit rejects delete that leaves dangling reference",
        "[FAIL] deduce deferred write commit rejects delete that leaves dangling reference",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-delete-ok-deferred (deduce 'open 'memory)) "
        "  (define [relation db-ref-delete-ok-deferred] users-ref-delete-ok-deferred (key id) (name)) "
        "  (define [relation db-ref-delete-ok-deferred] audit-ref-delete-ok-deferred (ref users-ref-delete-ok-deferred id user-id) (action)) "
        "  (deduce 'fact! users-ref-delete-ok-deferred 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-delete-ok-deferred 42 \"login\") "
        "  (define tx (deduce 'block db-ref-delete-ok-deferred 'write-deferred)) "
        "  (deduce 'retract! tx audit-ref-delete-ok-deferred 42 \"login\") "
        "  (deduce 'retract! tx users-ref-delete-ok-deferred 42 \"Alice\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-ref-delete-ok-deferred) 0) "
        "       (= (deduce 'count audit-ref-delete-ok-deferred) 0)))",
        "[PASS] deduce deferred write commit allows delete after staged blocker removal",
        "[FAIL] deduce deferred write commit allows delete after staged blocker removal",
        pass,
        fail
    );
}

fn void run_deduce_reference_retract_integrity_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-retract (deduce 'open 'memory)) "
        "  (define [relation db-ref-retract] users-ref-retract (key id) (name)) "
        "  (define [relation db-ref-retract] audit-ref-retract (ref users-ref-retract id user-id) (action)) "
        "  (deduce 'fact! users-ref-retract 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-retract 42 \"login\") "
        "  (define payload (handle (deduce 'retract! users-ref-retract 42 \"Alice\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-retract) 1)))",
        "[PASS] deduce retract! rejects deleting referenced target tuple",
        "[FAIL] deduce retract! rejects deleting referenced target tuple",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-retract-tx (deduce 'open 'memory)) "
        "  (define [relation db-ref-retract-tx] users-ref-retract-tx (key id) (name)) "
        "  (define [relation db-ref-retract-tx] audit-ref-retract-tx (ref users-ref-retract-tx id user-id) (action)) "
        "  (deduce 'fact! users-ref-retract-tx 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-retract-tx 42 \"login\") "
        "  (define tx (deduce 'block db-ref-retract-tx 'write)) "
        "  (deduce 'retract! tx audit-ref-retract-tx 42 \"login\") "
        "  (deduce 'retract! tx users-ref-retract-tx 42 \"Alice\") "
        "  (deduce 'commit tx) "
        "  (and (= (deduce 'count users-ref-retract-tx) 0) "
        "       (= (deduce 'count audit-ref-retract-tx) 0)))",
        "[PASS] deduce retract! allows deleting target after blocker is removed in same txn",
        "[FAIL] deduce retract! allows deleting target after blocker is removed in same txn",
        pass,
        fail
    );
}

fn void run_deduce_reference_bulk_delete_integrity_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-clear (deduce 'open 'memory)) "
        "  (define [relation db-ref-clear] users-ref-clear (key id) (name)) "
        "  (define [relation db-ref-clear] audit-ref-clear (ref users-ref-clear id user-id) (action)) "
        "  (deduce 'fact! users-ref-clear 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-clear 42 \"login\") "
        "  (define payload (handle (deduce 'clear! users-ref-clear) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-clear) 1) "
        "       (= (deduce 'count audit-ref-clear) 1)))",
        "[PASS] deduce clear! rejects clearing referenced target relation",
        "[FAIL] deduce clear! rejects clearing referenced target relation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-drop (deduce 'open 'memory)) "
        "  (define [relation db-ref-drop] users-ref-drop (key id) (name)) "
        "  (define [relation db-ref-drop] audit-ref-drop (ref users-ref-drop id user-id) (action)) "
        "  (deduce 'fact! users-ref-drop 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-drop 42 \"login\") "
        "  (define payload (handle (deduce 'drop! users-ref-drop) (raise msg msg))) "
        "  (and (dict? payload) "
        "       (= (ref payload 'code) 'deduce/integrity-reference-target-in-use) "
        "       (= (deduce 'count users-ref-drop) 1) "
        "       (= (deduce 'count audit-ref-drop) 1)))",
        "[PASS] deduce drop! rejects dropping referenced target relation",
        "[FAIL] deduce drop! rejects dropping referenced target relation",
        pass,
        fail
    );

    deduce_test_expect_true_symbol(
        interp,
        "(block "
        "  (define db-ref-clear-ok (deduce 'open 'memory)) "
        "  (define [relation db-ref-clear-ok] users-ref-clear-ok (key id) (name)) "
        "  (define [relation db-ref-clear-ok] audit-ref-clear-ok (ref users-ref-clear-ok id user-id) (action)) "
        "  (deduce 'fact! users-ref-clear-ok 42 \"Alice\") "
        "  (deduce 'fact! audit-ref-clear-ok 42 \"login\") "
        "  (deduce 'clear! audit-ref-clear-ok) "
        "  (deduce 'clear! users-ref-clear-ok) "
        "  (and (= (deduce 'count users-ref-clear-ok) 0) "
        "       (= (deduce 'count audit-ref-clear-ok) 0)))",
        "[PASS] deduce clear! allows clearing target after clearing blockers",
        "[FAIL] deduce clear! allows clearing target after clearing blockers",
        pass,
        fail
    );
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
    test_error(interp, "deduce-open rejected", "(deduce-open 'memory)", pass, fail);
    test_error(interp, "fact! rejected", "(fact! nil)", pass, fail);
    test_error(interp, "retract! rejected", "(retract! nil)", pass, fail);
    test_error(interp, "deduce-scan rejected", "(deduce-scan nil)", pass, fail);
    test_error(interp, "deduce-query rejected", "(deduce-query nil nil)", pass, fail);
    test_error(interp, "deduce-count rejected", "(deduce-count nil)", pass, fail);
    test_error(interp, "deduce-match rejected", "(deduce-match nil nil)", pass, fail);
}

fn bool deduce_test_component_delta_tuple_matches(DeduceDeltaTupleSet* set, usz idx, char[] expected) {
    if (set == null || idx >= set.count) return false;
    return deduce_bytes_equal(
        set.tuples[idx].key_bytes,
        set.tuples[idx].key_len,
        expected.ptr,
        expected.len
    );
}

fn void run_deduce_parallel_component_delta_payload_roundtrip_test(Interp* interp, int* pass, int* fail) {
    SymbolId[3] predicates = {
        interp.symbols.intern("parallel-component-a"),
        interp.symbols.intern("parallel-component-b"),
        interp.symbols.intern("parallel-component-c")
    };
    int[3] predicate_component = { 0, 0, 1 };
    DeduceSccPlan plan = {
        .predicates = predicates[..],
        .predicate_component = predicate_component[..],
        .component_count = 2
    };

    DeduceSignedDeltaSet[3] delta_sets = {};
    bool ok =
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].additions, "add-a-0".ptr, "add-a-0".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].additions, "add-a-1".ptr, "add-a-1".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].removals, "remove-a".ptr, "remove-a".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[1].removals, "remove-b".ptr, "remove-b".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[2].additions, "ignored-c".ptr, "ignored-c".len);
    defer foreach (signed : delta_sets) deduce_signed_delta_set_release(&signed);

    char* encoded = null;
    usz encoded_len = 0;
    if (ok) {
        Value* err = deduce_component_delta_payload_serialize(&plan, 0, delta_sets[..], &encoded, &encoded_len, interp);
        ok = err == null && encoded != null && encoded_len > 0;
    }
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    if (ok) {
        Value* err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
        ok = err == null;
    }
    defer deduce_serialized_component_delta_payload_release(&payload);

    if (ok) {
        ok = payload.component_id == 0 &&
             payload.entry_count == 2 &&
             payload.entries != null &&
             payload.entries[0].predicate_index == 0 &&
             payload.entries[0].predicate == predicates[0] &&
             payload.entries[0].delta_set.additions.count == 2 &&
             payload.entries[0].delta_set.removals.count == 1 &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.additions, 0, "add-a-0") &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.additions, 1, "add-a-1") &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.removals, 0, "remove-a") &&
             payload.entries[1].predicate_index == 1 &&
             payload.entries[1].predicate == predicates[1] &&
             payload.entries[1].delta_set.additions.count == 0 &&
             payload.entries[1].delta_set.removals.count == 1 &&
             deduce_test_component_delta_tuple_matches(&payload.entries[1].delta_set.removals, 0, "remove-b");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel component delta payload roundtrips worker-scratch deltas");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce parallel component delta payload roundtrips worker-scratch deltas");
        (*fail)++;
    }
}

fn bool deduce_test_component_delta_set_has_tuple(DeduceDeltaTupleSet* set, char[] expected) {
    if (set == null) return false;
    for (usz i = 0; i < set.count; i++) {
        if (deduce_bytes_equal(set.tuples[i].key_bytes, set.tuples[i].key_len, expected.ptr, expected.len)) {
            return true;
        }
    }
    return false;
}

fn void run_deduce_parallel_component_scratch_pass_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-scratch-pass (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-scratch-pass] edge-parallel-scratch-pass (src) (dst)) "
        "  (define [relation ddb-parallel-scratch-pass] path-parallel-scratch-pass (src) (dst)) "
        "  (deduce/rule! ddb-parallel-scratch-pass '(path-parallel-scratch-pass ?x ?y) '(edge-parallel-scratch-pass ?x ?y)) "
        "  (deduce/rule! ddb-parallel-scratch-pass '(path-parallel-scratch-pass ?x ?z) '(edge-parallel-scratch-pass ?x ?y) '(path-parallel-scratch-pass ?y ?z)) "
        "  (deduce 'fact! edge-parallel-scratch-pass 1 2) "
        "  (deduce 'fact! path-parallel-scratch-pass 2 3) "
        "  ddb-parallel-scratch-pass)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    Value* expected_1_2 = null;
    Value* expected_1_3 = null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = null;
        err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-scratch-pass")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_pass_payload(
                db,
                &plan,
                component_id,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            Value*[2] tuple_1_2 = { make_int(interp, 1), make_int(interp, 2) };
            Value*[2] tuple_1_3 = { make_int(interp, 1), make_int(interp, 3) };
            err = deduce_encode_value_vector_to_owned_string(tuple_1_2[..], &expected_1_2, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_1_3[..], &expected_1_3, interp);
            ok = err == null && expected_1_2 != null && expected_1_3 != null;
        }
        if (ok) {
            ok = payload.entry_count == 1 &&
                 payload.entries != null &&
                 payload.entries[0].predicate == interp.symbols.intern("path-parallel-scratch-pass") &&
                 payload.entries[0].delta_set.additions.count == 2 &&
                 payload.entries[0].delta_set.removals.count == 0 &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_2.str_chars[:expected_1_2.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_3.str_chars[:expected_1_3.str_len]
                 );
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch component pass computes serialized recursive deltas");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas (entries=%d adds=%d removes=%d)",
                (int)payload.entry_count,
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.additions.count : 0),
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.removals.count : 0)
            );
            io::printn("[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas");
        }
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_scratch_closure_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-closure (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-closure] edge-parallel-closure (src) (dst)) "
        "  (define [relation ddb-parallel-closure] path-parallel-closure (src) (dst)) "
        "  (deduce/rule! ddb-parallel-closure '(path-parallel-closure ?x ?y) '(edge-parallel-closure ?x ?y)) "
        "  (deduce/rule! ddb-parallel-closure '(path-parallel-closure ?x ?z) '(path-parallel-closure ?x ?y) '(edge-parallel-closure ?y ?z)) "
        "  (deduce 'fact! edge-parallel-closure 1 2) "
        "  (deduce 'fact! edge-parallel-closure 2 3) "
        "  ddb-parallel-closure)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    Value* expected_1_2 = null;
    Value* expected_2_3 = null;
    Value* expected_1_3 = null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-closure")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            Value*[2] tuple_1_2 = { make_int(interp, 1), make_int(interp, 2) };
            Value*[2] tuple_2_3 = { make_int(interp, 2), make_int(interp, 3) };
            Value*[2] tuple_1_3 = { make_int(interp, 1), make_int(interp, 3) };
            err = deduce_encode_value_vector_to_owned_string(tuple_1_2[..], &expected_1_2, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_2_3[..], &expected_2_3, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_1_3[..], &expected_1_3, interp);
            ok = err == null && expected_1_2 != null && expected_2_3 != null && expected_1_3 != null;
        }
        if (ok) {
            ok = payload.entry_count == 1 &&
                 payload.entries != null &&
                 payload.entries[0].predicate == interp.symbols.intern("path-parallel-closure") &&
                 payload.entries[0].delta_set.additions.count == 3 &&
                 payload.entries[0].delta_set.removals.count == 0 &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_2.str_chars[:expected_1_2.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_2_3.str_chars[:expected_2_3.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_3.str_chars[:expected_1_3.str_len]
                 );
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas (entries=%d adds=%d removes=%d)",
                (int)payload.entry_count,
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.additions.count : 0),
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.removals.count : 0)
            );
            io::printn("[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas");
        }
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_scratch_multi_atom_closure_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-multiatom (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-multiatom] edge-parallel-multiatom (src) (dst)) "
        "  (define [relation ddb-parallel-multiatom] path-parallel-multiatom (src) (dst)) "
        "  (deduce/rule! ddb-parallel-multiatom '(path-parallel-multiatom ?x ?y) '(edge-parallel-multiatom ?x ?y)) "
        "  (deduce/rule! ddb-parallel-multiatom '(path-parallel-multiatom ?x ?z) '(path-parallel-multiatom ?x ?y) '(path-parallel-multiatom ?y ?z)) "
        "  (deduce 'fact! edge-parallel-multiatom 1 2) "
        "  (deduce 'fact! edge-parallel-multiatom 2 3) "
        "  (deduce 'fact! edge-parallel-multiatom 3 4) "
        "  ddb-parallel-multiatom)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-multiatom")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err != null &&
                 err.tag == ERROR;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch closure rejects positive multi-atom recursive SCC rules");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce parallel worker-scratch closure rejects positive multi-atom recursive SCC rules");
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_delta_apply_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-apply (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-apply] edge-parallel-apply (src) (dst)) "
        "  (define [relation ddb-parallel-apply] path-parallel-apply (src) (dst)) "
        "  (deduce/rule! ddb-parallel-apply '(path-parallel-apply ?x ?y) '(edge-parallel-apply ?x ?y)) "
        "  (deduce/rule! ddb-parallel-apply '(path-parallel-apply ?x ?z) '(path-parallel-apply ?x ?y) '(edge-parallel-apply ?y ?z)) "
        "  (deduce 'fact! edge-parallel-apply 1 2) "
        "  (deduce 'fact! edge-parallel-apply 2 3) "
        "  ddb-parallel-apply)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    long applied = 0;
    bool changed = false;
    long tuple_count = -1;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-apply")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(db.env, null, 0, &txn);
            ok = rc == MDB_SUCCESS && txn != null;
            if (ok) {
                err = deduce_apply_serialized_component_delta_payload(
                    db,
                    &plan,
                    txn,
                    &payload,
                    &applied,
                    &changed,
                    interp
                );
                ok = err == null;
            }
            if (ok) {
                rc = mdb_txn_commit(txn);
                ok = rc == MDB_SUCCESS;
                txn = null;
            }
            if (txn != null) mdb_txn_abort(txn);
        }
        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
            ok = rc == MDB_SUCCESS && txn != null;
            MdbCursor* cursor = null;
            if (ok) {
                MdbDbi path_dbi = 0;
                DeduceRelationSchema* schema = null;
                ok = deduce_rule_open_existing_dbi(
                    db,
                    txn,
                    interp.symbols.intern("path-parallel-apply"),
                    &path_dbi,
                    &schema,
                    interp
                ) && schema != null;
                if (ok) {
                    rc = mdb_cursor_open(txn, path_dbi, &cursor);
                    ok = rc == MDB_SUCCESS && cursor != null;
                }
                if (ok) {
                    MdbVal key = {};
                    MdbVal data = {};
                    tuple_count = 0;
                    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
                    while (rc == MDB_SUCCESS) {
                        tuple_count++;
                        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                    }
                    ok = tuple_count == 3 &&
                        schema.cardinality_estimate == 3 &&
                        schema.distinct_estimate == 3;
                }
            }
            if (cursor != null) mdb_cursor_close(cursor);
            if (txn != null) mdb_txn_abort(txn);
        }
        if (ok) {
            ok = payload.entry_count == 1 && applied == 3 && changed;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-computed component deltas apply on the main thread");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-computed component deltas apply on the main thread (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-computed component deltas apply on the main thread (entries=%d applied=%d changed=%d tuples=%d)",
                (int)payload.entry_count,
                (int)applied,
                changed ? 1 : 0,
                (int)tuple_count
            );
            io::printn("[FAIL] deduce parallel worker-computed component deltas apply on the main thread");
        }
        (*fail)++;
    }
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
