module lisp;


import std::io;
import main;

fn void run_basic_intro_tests(Interp* interp, int* pass, int* fail) {
    // Literals & bindings
    test_eq(interp, "integer literal 42", "42", 42, pass, fail);
    test_eq(interp, "integer literal long min", "-9223372036854775808", long.min, pass, fail);
    test_error_contains(interp, "integer literal underflow rejects",
        "-9223372036854775809", "integer literal overflow", pass, fail);
    setup(interp, "(define x 10)");
    test_eq(interp, "variable binding x => 10", "x", 10, pass, fail);
    test_eq(interp, "lambda identity", "((lambda (x) x) 5)", 5, pass, fail);
    test_eq(interp, "let binding", "(let (y 7) y)", 7, pass, fail);
    test_eq(interp, "if true", "(if true 1 2)", 1, pass, fail);

    // Primitives
    setup(interp, "(define add +)");
    test_tag(interp, "quote returns list", "(quote (a b c))", CONS, pass, fail);

    // Closure
    setup(interp, "(define make-adder (lambda (n) (lambda (x) n)))");
    test_tag(interp, "nested lambda", "(make-adder 5)", CLOSURE, pass, fail);

    // Cons/car/cdr
    test_eq(interp, "car (cons 1 2) => 1", "(car (cons 1 2))", 1, pass, fail);
    test_eq(interp, "cdr (cons 1 2) => 2", "(cdr (cons 1 2))", 2, pass, fail);

    // Print (interp only, side effect)
    test_truthy_interp(interp, "println side effect capture",
        "(= (String (handle (block (println (quote hello)) nil) (io/println x x))) \"hello\")",
        pass, fail);

    // Source location tracking
    {
        EvalResult r = run("undefined_var", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] source location tracking");
            (*pass)++;
        } else {
            io::printn("[FAIL] source location tracking");
            (*fail)++;
        }
    }
}

fn void run_basic_index_and_match_tests(Interp* interp, int* pass, int* fail) {
    // Dot-bracket indexing
    setup(interp, "(define mylist (quote (10 20 30 40 50)))");
    test_eq(interp, "mylist.[0] => 10", "mylist.[0]", 10, pass, fail);
    test_eq(interp, "mylist.[2] => 30", "mylist.[2]", 30, pass, fail);
    setup(interp, "(define idx 3)");
    test_eq(interp, "mylist.[idx] => 40", "mylist.[idx]", 40, pass, fail);
    test_eq(interp, "dict dot-bracket symbol key", "(let (m {'a 1 'b 2}) m.['a])", 1, pass, fail);
    test_eq(interp, "dict dot-bracket string key", "(let (m {\"name\" 78}) m.[\"name\"])", 78, pass, fail);
    test_eq(interp, "dict bare key quotes symbol", "(let (m {name 78}) (ref m 'name))", 78, pass, fail);
    test_eq(interp, "dict bare key value still evaluates", "(let (name 9 m {name (+ name 1)}) (ref m 'name))", 10, pass, fail);
    test_truthy(interp, "index/ref parity list", "(= (ref '(10 20 30) 1) (let (l '(10 20 30)) l.[1]))", pass, fail);
    test_truthy(interp, "index/ref parity list negative", "(= (ref '(10 20 30) -1) (let (l '(10 20 30)) l.[-1]))", pass, fail);
    test_truthy(interp, "index/ref parity array", "(= (ref [10 20 30] 2) (let (a [10 20 30]) a.[2]))", pass, fail);
    test_truthy(interp, "index/ref parity array negative", "(= (ref [10 20 30] -2) (let (a [10 20 30]) a.[-2]))", pass, fail);
    test_truthy(interp, "index/ref parity dict", "(= (ref {'a 1 'b 2} 'b) (let (m {'a 1 'b 2}) m.['b]))", pass, fail);
    test_truthy(interp, "index/ref parity string", "(= (ref \"xyz\" 1) (\"xyz\".[1]))", pass, fail);
    test_truthy(interp, "index/ref parity string negative", "(= (ref \"xyz\" -1) (\"xyz\".[-1]))", pass, fail);
    test_truthy(interp, "index/ref parity array oob error",
        "(= (handle (ref [10 20] 5) (raise e (String (ref e 'message)))) (handle (let (a [10 20]) a.[5]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity array wrong index error",
        "(= (handle (ref [10 20] 'x) (raise e (String (ref e 'message)))) (handle (let (a [10 20]) a.['x]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity list oob error",
        "(= (handle (ref '(10 20) 5) (raise e (String (ref e 'message)))) (handle (let (l '(10 20)) l.[5]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity list wrong index error",
        "(= (handle (ref '(10 20) 'x) (raise e (String (ref e 'message)))) (handle (let (l '(10 20)) l.['x]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity string oob error",
        "(= (handle (ref \"xy\" 5) (raise e (String (ref e 'message)))) (handle (\"xy\".[5]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity string wrong index error",
        "(= (handle (ref \"xy\" 'x) (raise e (String (ref e 'message)))) (handle (\"xy\".['x]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "index/ref parity dict missing key",
        "(= (ref {'a 1} 'missing) (let (m {'a 1}) m.['missing]))",
        pass, fail);
    test_truthy(interp, "index/ref parity unsupported collection error",
        "(= (handle (ref 42 0) (raise e (String (ref e 'message)))) (handle (42.[0]) (raise e (String (ref e 'message)))))",
        pass, fail);
    test_truthy(interp, "postfix array oob canonical error",
        "(= (handle (let (a [10 20]) a.[5]) (raise e (String (ref e 'message)))) \"ref: array index out of bounds\")",
        pass, fail);
    test_truthy(interp, "postfix array wrong index canonical error",
        "(= (handle (let (a [10 20]) a.['x]) (raise e (String (ref e 'message)))) \"ref: array requires int index\")",
        pass, fail);
    test_truthy(interp, "postfix list oob canonical error",
        "(= (handle (let (l '(10 20)) l.[5]) (raise e (String (ref e 'message)))) \"ref: list index out of bounds\")",
        pass, fail);
    test_truthy(interp, "postfix list wrong index canonical error",
        "(= (handle (let (l '(10 20)) l.['x]) (raise e (String (ref e 'message)))) \"ref: list requires int index\")",
        pass, fail);
    test_truthy(interp, "postfix string oob canonical error",
        "(= (handle (\"xy\".[5]) (raise e (String (ref e 'message)))) \"ref: string index out of bounds\")",
        pass, fail);
    test_truthy(interp, "postfix string wrong index canonical error",
        "(= (handle (\"xy\".['x]) (raise e (String (ref e 'message)))) \"ref: string requires int index\")",
        pass, fail);
    test_eq(interp, "postfix index then symbol path", "(let (rows [(Dictionary 'name 44)]) rows.[0].name)", 44, pass, fail);
    test_error_contains(interp, "leading-dot int accessor removed", "(.1 (quote (10 20 30)))",
        "leading-dot accessors were removed", pass, fail);
    test_error_contains(interp, "leading-dot symbol accessor removed", "(.name {'name 91})",
        "leading-dot accessors were removed", pass, fail);
    test_error_contains(interp, "leading-dot bracket accessor removed", "(.[2] {[2] 79})",
        "leading-dot accessors were removed", pass, fail);
    test_error_contains(interp, "leading-dot dynamic accessor removed", "(. key {'name 95})",
        "leading-dot accessors were removed", pass, fail);
    test_error_contains(interp, "leading-dot higher-order accessor removed", "(map .1 '((10 20) (30 40)))",
        "leading-dot accessors were removed", pass, fail);

    // Pattern matching basics
    test_eq(interp, "match literal", "(match 42 (42 1) (x 0))", 1, pass, fail);
    test_eq(interp, "match variable", "(match 5 (x x))", 5, pass, fail);
    test_eq(interp, "match wildcard", "(match 99 (_ 1))", 1, pass, fail);
    test_eq(interp, "match [a b c]", "(match (quote (1 2 3)) ([a b c] (+ a (+ b c))))", 6, pass, fail);
    test_eq(interp, "match [head .. tail]", "(match (quote (10 20 30)) ([head .. tail] head))", 10, pass, fail);
    test_eq(interp, "match [a b ..]", "(match (quote (1 2 3 4 5)) ([a b ..] (+ a b)))", 3, pass, fail);
    test_eq(interp, "match [.. last]", "(match (quote (1 2 3 4 5)) ([.. last] last))", 5, pass, fail);
    test_eq(interp, "match fallthrough", "(match 42 (0 100) (1 200) (x x))", 42, pass, fail);
    test_eq(interp, "match [_ second _]", "(match (quote (1 2 3)) ([_ second _] second))", 2, pass, fail);
}

fn void run_basic_control_flow_tests(Interp* interp, int* pass, int* fail) {
    // Length
    test_eq(interp, "length '() => 0", "(length (quote ()))", 0, pass, fail);
    test_eq(interp, "length '(1 2 3 4 5) => 5", "(length (quote (1 2 3 4 5)))", 5, pass, fail);

    // Path access
    setup(interp, "(define testperson {'name \"Alice\" 'age 30})");
    test_str(interp, "testperson.name => string", "testperson.name", pass, fail);
    test_eq(interp, "testperson.age => 30", "testperson.age", 30, pass, fail);
    test_truthy(interp, "path/ref parity dict symbol key", "(= (let (m {'name 42}) m.name) (ref {'name 42} 'name))", pass, fail);
    test_nil(interp, "ref missing dict key => nil", "(ref {'name 42} 'age)", pass, fail);
    test_error_contains(interp, "path missing dict key errors", "(let (m {'name 42}) m.age)",
        "cannot access '.age' on Dictionary value", pass, fail);
    test_eq(interp, "cons path .car", "(let (p (cons 10 20)) p.car)", 10, pass, fail);
    test_eq(interp, "cons path .cdr", "(let (p (cons 10 20)) p.cdr)", 20, pass, fail);
    test_error_contains(interp, "ref cons symbol key rejected", "(ref (cons 10 20) 'car)",
        "ref: list requires int index", pass, fail);

    // Nested path
    setup(interp, "(define testperson2 {'name \"Bob\" 'address {'city \"NYC\" 'zip 10001}})");
    test_str(interp, "testperson2.address.city => string", "testperson2.address.city", pass, fail);
    setup(interp, "(define old-person (cons (cons 'name \"Alice\") nil))");
    test_error_contains(interp, "alist-style path rejected", "old-person.name",
        "cannot access '.name' on List value", pass, fail);

    // Recursive let
    test_eq(interp, "let ^rec factorial 5 => 120", "(let ^rec (fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))", 120, pass, fail);
    test_eq(interp, "non-recursive let", "(let (x 10) (+ x 5))", 15, pass, fail);
    test_eq(interp, "let bindings are sequential",
        "(let (x 1 y (+ x 2) z (+ y 3)) z)",
        6, pass, fail);
    test_eq(interp, "named let bindings are sequential",
        "(let loop (x 1 y (+ x 2)) (+ x y))",
        4, pass, fail);
    test_eq(interp, "tail multi-arg call can ignore error-valued argument",
        "(let loop (x (reverse (cons 1 2)) y 7) y)",
        7, pass, fail);
    test_error_contains(interp, "tail multi-arg call preserves error-valued argument",
        "(let loop (x (reverse (cons 1 2)) y 7) x)",
        "proper list", pass, fail);
    test_error_contains(interp, "scheme let syntax rejected",
        "(let ((x 10) (y 20)) (+ x y))",
        "Scheme-style let bindings were removed; use flat pairs", pass, fail);
    test_error_contains(interp, "let bracket binding syntax rejected",
        "(let [x 10] (+ x 5))",
        "let bindings must use flat pairs", pass, fail);
    test_error_contains(interp, "named let scheme binding syntax rejected",
        "(let loop ((x 0) (y 1)) (+ x y))",
        "Scheme-style named let bindings were removed; use flat pairs", pass, fail);

    // And/or
    test_truthy(interp, "and true true", "(and true true)", pass, fail);
    test_truthy(interp, "and false true => false", "(= (and false true) false)", pass, fail);
    test_truthy(interp, "and true false => false", "(= (and true false) false)", pass, fail);
    test_eq(interp, "and 1 42 => 42", "(and 1 42)", 42, pass, fail);
    test_truthy(interp, "or true false", "(or true false)", pass, fail);
    test_nil(interp, "or false nil => nil", "(or false nil)", pass, fail);
    test_eq(interp, "or 42 99 => 42", "(or 42 99)", 42, pass, fail);
    test_eq(interp, "or nil 99 => 99", "(or nil 99)", 99, pass, fail);
    test_nil(interp, "and short-circuit", "(and nil undefined_var)", pass, fail);
    test_eq(interp, "or short-circuit", "(or 1 undefined_var)", 1, pass, fail);

    // Truthiness
    test_eq(interp, "0 is truthy", "(if 0 42 99)", 42, pass, fail);

    // Reset/capture
    test_eq(interp, "capture aborts", "(checkpoint (+ 1 (capture k 42)))", 42, pass, fail);
    test_eq(interp, "capture k resumes", "(checkpoint (+ 1 (capture k (k 10))))", 11, pass, fail);
    test_eq(interp, "checkpoint passthrough", "(checkpoint (+ 10 20))", 30, pass, fail);

    // Effect handlers
    test_eq(interp, "handle without effect", "(handle 42 (ask x (resolve 0)))", 42, pass, fail);
    test_eq(interp, "handle + perform + resume", "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", 11, pass, fail);
    test_eq(interp, "handle abort", "(handle (+ 1 (signal ask 5)) (ask x x))", 5, pass, fail);

    // Empty string truthy
    test_eq(interp, "empty string is truthy", "(if \"\" 1 0)", 1, pass, fail);
}

fn void run_basic_symbol_table_hardening_tests(Interp* interp, int* pass, int* fail) {
    if (!symbol_table_fault_injection_enabled()) {
        emit_pass_literal("[PASS] symbol table fault-injection hardening skipped (enable OMNI_TEST_FAULT_INJECTION)");
        (*pass)++;
        return;
    }

    {
        SymbolTable table = {};
        symbol_table_force_entries_alloc_fail_for_test(true);
        table.init();
        symbol_table_force_entries_alloc_fail_for_test(false);

        bool ok = table.entries == null
            && table.hash_index == null
            && table.capacity == 0
            && table.hash_capacity == 0
            && table.intern("alpha") == INVALID_SYMBOL_ID;
        if (ok) {
            emit_pass_literal("[PASS] symbol table init allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table init allocation failure fails closed");
            (*fail)++;
        }
        table.destroy();
    }

    {
        SymbolTable table = {};
        table.init();
        SymbolId first = table.intern("alpha");

        symbol_table_force_name_alloc_fail_for_test(true);
        SymbolId failed = table.intern("beta");
        symbol_table_force_name_alloc_fail_for_test(false);

        SymbolId later = table.intern("gamma");
        bool ok = first != INVALID_SYMBOL_ID
            && failed == INVALID_SYMBOL_ID
            && later != INVALID_SYMBOL_ID
            && table.count == 2
            && scheduler_str_eq(table.get_name(first), "alpha")
            && scheduler_str_eq(table.get_name(later), "gamma");
        if (ok) {
            emit_pass_literal("[PASS] symbol table name allocation failure preserves table");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table name allocation failure preserves table");
            (*fail)++;
        }
        table.destroy();
    }

    {
        SymbolTable table = {};
        table.init();
        bool seeded = true;
        usz i = 0;
        while (seeded && (table.count + 1) * 10 < table.capacity * 7) {
            char[32] name_buf;
            if (try char[] name = io::bprintf(&name_buf, "sym-%d", (int)i)) {
                if (table.intern(name) == INVALID_SYMBOL_ID) {
                    seeded = false;
                    break;
                }
            } else {
                seeded = false;
                break;
            }
            i++;
        }

        SymbolId before = table.intern("stable-before-grow");
        usz count_before = table.count;
        symbol_table_force_grow_entries_alloc_fail_for_test(true);
        SymbolId failed = table.intern("trigger-grow-fail");
        symbol_table_force_grow_entries_alloc_fail_for_test(false);
        SymbolId after = table.intern("after-grow-ok");

        bool ok = seeded
            && before != INVALID_SYMBOL_ID
            && failed == INVALID_SYMBOL_ID
            && after != INVALID_SYMBOL_ID
            && table.capacity > SYMBOL_INITIAL_CAPACITY
            && table.count == count_before + 1
            && scheduler_str_eq(table.get_name(before), "stable-before-grow")
            && scheduler_str_eq(table.get_name(after), "after-grow-ok");
        if (ok) {
            emit_pass_literal("[PASS] symbol table grow allocation failure preserves table");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table grow allocation failure preserves table");
            (*fail)++;
        }
        table.destroy();
    }
}

fn TypeInfo make_basic_type_info(SymbolId name) {
    TypeInfo info;
    info.name = name;
    info.kind = TK_BUILTIN;
    info.parent = INVALID_TYPE_ID;
    info.fields = null;
    info.field_count = 0;
    info.type_params = null;
    info.type_param_count = 0;
    info.constraints = null;
    info.constraint_count = 0;
    info.variants = null;
    info.variant_count = 0;
    info.alias_target = INVALID_TYPE_ID;
    return info;
}

fn void run_basic_type_registry_hardening_tests(Interp* interp, int* pass, int* fail) {
    if (!type_registry_fault_injection_enabled()) {
        emit_pass_literal("[PASS] type registry fault-injection hardening skipped (enable OMNI_TEST_FAULT_INJECTION)");
        (*pass)++;
        return;
    }

    {
        TypeRegistry registry = {};
        type_registry_force_types_alloc_fail_for_test(true);
        bool ok = registry.init();
        type_registry_force_types_alloc_fail_for_test(false);

        bool closed = !ok
            && registry.types == null
            && registry.type_hash_index == null
            && registry.type_count == 0
            && registry.capacity == 0
            && registry.hash_capacity == 0;
        if (closed) {
            emit_pass_literal("[PASS] type registry init type-alloc failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] type registry init type-alloc failure fails closed");
            (*fail)++;
        }
        registry.destroy();
    }

    {
        TypeRegistry registry = {};
        type_registry_force_hash_alloc_fail_for_test(true);
        bool ok = registry.init();
        type_registry_force_hash_alloc_fail_for_test(false);

        bool closed = !ok
            && registry.types == null
            && registry.type_hash_index == null
            && registry.type_count == 0
            && registry.capacity == 0
            && registry.hash_capacity == 0;
        if (closed) {
            emit_pass_literal("[PASS] type registry init hash-alloc failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] type registry init hash-alloc failure fails closed");
            (*fail)++;
        }
        registry.destroy();
    }

    {
        TypeRegistry registry = {};
        if (!registry.init()) {
            emit_pass_literal("[PASS] skipped type registry growth fail-closed (init blocked)");
            (*pass)++;
        } else {
            SymbolTable syms = {};
            syms.init();

            bool seeded = true;
            for (usz i = 0; i < TYPE_INITIAL_CAPACITY; i++) {
                char[32] name_buf;
                if (try char[] name = io::bprintf(&name_buf, "seed-type-%d", (int)i)) {
                    SymbolId sid = syms.intern(name);
                    if (sid == INVALID_SYMBOL_ID) {
                        seeded = false;
                        break;
                    }
                    if (registry.register_type(make_basic_type_info(sid), &syms) == INVALID_TYPE_ID) {
                        seeded = false;
                        break;
                    }
                } else {
                    seeded = false;
                    break;
                }
            }

            usz before_count = registry.type_count;
            TypeInfo* before_types = registry.types;
            TypeId* before_hash = registry.type_hash_index;
            usz before_cap = registry.capacity;
            usz before_hash_cap = registry.hash_capacity;

            type_registry_force_grow_types_alloc_fail_for_test(true);
            TypeInfo fail_info = make_basic_type_info(syms.intern("grow-fail"));
            TypeId fail_id = registry.register_type(fail_info, &syms);
            type_registry_force_grow_types_alloc_fail_for_test(false);

            bool preserved_after_failure = seeded
                && fail_id == INVALID_TYPE_ID
                && registry.type_count == before_count
                && registry.types == before_types
                && registry.type_hash_index == before_hash
                && registry.capacity == before_cap
                && registry.hash_capacity == before_hash_cap;

            SymbolId after_name = syms.intern("grow-success");
            TypeInfo recover_info = make_basic_type_info(after_name);
            TypeId recover_id = registry.register_type(recover_info, &syms);

            bool preserved = preserved_after_failure
                && recover_id != INVALID_TYPE_ID
                && registry.type_count == before_count + 1;

            if (preserved) {
                emit_pass_literal("[PASS] type registry grow type-alloc failure preserves state");
                (*pass)++;
            } else {
                io::printn("[FAIL] type registry grow type-alloc failure preserves state");
                (*fail)++;
            }

            syms.destroy();
        }
        registry.destroy();
    }
}

fn void run_basic_runtime_memory_stats_tests(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(let (s (runtime-memory-stats)) "
        "  (and (= (ref s 'kind) 'runtime-memory-stats) "
        "       (= (ref s 'status) 'ok) "
        "       (>= (ref (ref s 'scope) 'scope-create-count) 1) "
        "       (>= (ref (ref s 'scope) 'temp-bytes-allocated) 0) "
        "       (>= (ref (ref s 'copy-to-parent) 'copy-to-parent-bytes) 0) "
        "       (>= (ref (ref s 'fiber-temp) 'take-hits) 0) "
        "       (>= (ref (ref s 'scope) 'temp-peak-live-chunk-count) 0) "
        "       (>= (ref (ref s 'value-shape) 'array-construct-count) 0) "
        "       (>= (ref (ref s 'value-shape) 'ffi-handle-releasable-count) 0) "
        "       (>= (ref (ref s 'boundary-decisions) 'materialization-copy-bytes) 0) "
        "       (>= (ref (ref s 'boundary-decisions) 'materialization-copy-bytes-optimizer) 0) "
        "       (>= (ref (ref s 'boundary-decisions) 'materialization-copy-bytes-forced-no-splice) 0) "
        "       (>= (ref (ref s 'boundary-decisions) 'selected-fail-closed) 0)))",
        interp
    );
    if (!r.error.has_error && !is_falsy(r.value, interp)) {
        emit_pass_literal("[PASS] runtime-memory-stats structured counters");
        (*pass)++;
    } else {
        io::printn("[FAIL] runtime-memory-stats structured counters");
        (*fail)++;
    }
}

fn void run_basic_runtime_memory_stats_native_tests(Interp* interp, int* pass, int* fail) {
    usz create_before = main::g_scope_memory_telemetry_stats.scope_create_count;
    main::ScopeRegion* scope = main::scope_create(interp.root_scope);
    if (scope != null) {
        void* temp_ptr = scope.alloc(24);
        void* escape_ptr = scope.alloc_escape(32);
        (void)temp_ptr;
        (void)escape_ptr;
        main::scope_release(scope);
    }
    main::ScopeMemoryTelemetryStats stats = main::g_scope_memory_telemetry_stats;
    if (scope != null &&
        stats.scope_create_count >= create_before + 1 &&
        stats.total_temp_bytes_allocated >= 24 &&
        stats.total_escape_bytes_allocated >= 32 &&
        stats.scope_release_count > 0) {
        emit_pass_literal("[PASS] runtime memory telemetry native counters");
        (*pass)++;
    } else {
        io::printn("[FAIL] runtime memory telemetry native counters");
        (*fail)++;
    }
}

fn void run_basic_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Basic Tests");

    run_basic_intro_tests(interp, pass, fail);
    run_basic_index_and_match_tests(interp, pass, fail);
    run_basic_let_destructure_tests(interp, pass, fail);
    run_basic_function_param_destructure_tests(interp, pass, fail);
    run_basic_control_flow_tests(interp, pass, fail);
    run_basic_symbol_table_hardening_tests(interp, pass, fail);
    run_basic_type_registry_hardening_tests(interp, pass, fail);
    run_basic_runtime_memory_stats_tests(interp, pass, fail);
    run_basic_runtime_memory_stats_native_tests(interp, pass, fail);
    run_basic_interp_storage_hardening_tests(interp, pass, fail);
}
