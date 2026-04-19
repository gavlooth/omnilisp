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
    run_deduce_capacity_growth_overflow_guard_test(interp, pass, fail);
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
