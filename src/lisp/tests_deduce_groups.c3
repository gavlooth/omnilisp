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

fn void run_deduce_command_void_return_tests(Interp* interp, int* pass, int* fail) {
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
    run_deduce_namespaced_alias_smoke_test(interp, pass, fail);
    run_deduce_open_named_test(interp, pass, fail);
    run_deduce_durability_restart_test(interp, pass, fail);
    run_deduce_relation_attr_define_test(interp, pass, fail);
    run_deduce_relation_attr_typed_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_unique_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_composite_unique_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_reference_role_schema_test(interp, pass, fail);
    run_deduce_relation_attr_combined_role_test(interp, pass, fail);
    run_deduce_crud_example_flow_test(interp, pass, fail);
    run_deduce_memory_open_isolation_test(interp, pass, fail);
    run_deduce_reopen_stress_test(interp, pass, fail);
    run_deduce_large_relation_schema_test(interp, pass, fail);
    run_deduce_long_relation_name_isolation_test(interp, pass, fail);
    run_deduce_long_open_path_isolation_test(interp, pass, fail);
    run_deduce_key_integrity_fact_tests(interp, pass, fail);
    run_deduce_command_void_return_tests(interp, pass, fail);
    run_deduce_transaction_tests(interp, pass, fail);
    run_deduce_clear_drop_tests(interp, pass, fail);
    run_deduce_legacy_alias_rejection_tests(interp, pass, fail);
    run_deduce_rule_validation_tests(interp, pass, fail);
    setup_deduce_person_relation(interp);
    run_deduce_query_tests(interp, pass, fail);
}

fn void run_deduce_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    io::printn("\n--- Deduce Tests ---");
    run_group_isolated(&run_deduce_group_tests, pass, fail);
}
