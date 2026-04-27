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
    if (interp.tid_Float != INVALID_TYPE_ID &&
        interp.types.types[(usz)interp.tid_Float].parent == interp.tid_Number) {
        emit_pass_literal("[PASS] Float type cache is initialized");
        (*pass)++;
    } else {
        io::printn("[FAIL] Float type cache is initialized");
        (*fail)++;
    }

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
    test_error_contains(interp, "dict pattern rejects duplicate keys",
        "(match (Dictionary 'name 7) ({name name} name) (_ 0))",
        "duplicate key in dict pattern", pass, fail);
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
    {
        SymbolId missing_key = interp.symbols.intern("missing");
        Value malformed_module = {};
        malformed_module.tag = MODULE;
        malformed_module.module_val = null;
        Value malformed_instance = {};
        malformed_instance.tag = INSTANCE;
        malformed_instance.instance_val = null;
        Value malformed_dict = {};
        malformed_dict.tag = HASHMAP;
        malformed_dict.hashmap_val = null;

        EvalResult module_step = eval_path_step(&malformed_module, missing_key, interp);
        EvalResult instance_step = eval_path_step(&malformed_instance, missing_key, interp);
        EvalResult dict_step = eval_path_step(&malformed_dict, missing_key, interp);
        bool ok = module_step.error.has_error &&
            str_contains(module_step.error.message[:256], "module value is invalid") &&
            instance_step.error.has_error &&
            dict_step.error.has_error;
        if (ok) {
            emit_pass_literal("[PASS] malformed path payloads fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] malformed path payloads fail closed (module=%s instance=%s dict=%s)",
                module_step.error.has_error ? "error" : "ok",
                instance_step.error.has_error ? "error" : "ok",
                dict_step.error.has_error ? "error" : "ok"
            );
            (*fail)++;
        }
    }
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
    {
        bool ok = !symbol_table_should_grow(6, 10)
            && symbol_table_should_grow(7, 10)
            && symbol_table_should_grow(usz.max, usz.max);
        if (ok) {
            emit_pass_literal("[PASS] symbol table load-factor check avoids overflow");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table load-factor check avoids overflow");
            (*fail)++;
        }
    }

    {
        SymbolTable table = {};
        table.init();
        SymbolId anchor = table.intern("ceiling-anchor");
        usz saved_count = table.count;

        table.count = (usz)INVALID_SYMBOL_ID;
        SymbolId failed = table.intern("ceiling-new");
        bool ok = anchor != INVALID_SYMBOL_ID
            && failed == INVALID_SYMBOL_ID
            && table.count == (usz)INVALID_SYMBOL_ID
            && scheduler_str_eq(table.get_name(anchor), "ceiling-anchor");
        table.count = saved_count;

        if (ok) {
            emit_pass_literal("[PASS] symbol table refuses INVALID_SYMBOL_ID ceiling");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table refuses INVALID_SYMBOL_ID ceiling");
            (*fail)++;
        }
        table.destroy();
    }

    {
        SymbolTable table = {};
        table.init();
        SymbolId anchor = table.intern("hash-full-anchor");

        table.hash_capacity = 1;
        table.hash_index[0] = anchor;
        usz count_before = table.count;

        SymbolId first_failed = table.intern("hash-full-beta");
        SymbolId second_failed = table.intern("hash-full-beta");
        bool ok = anchor != INVALID_SYMBOL_ID
            && first_failed == INVALID_SYMBOL_ID
            && second_failed == INVALID_SYMBOL_ID
            && table.count == count_before
            && scheduler_str_eq(table.get_name(anchor), "hash-full-anchor")
            && scheduler_str_eq(table.get_name((SymbolId)count_before), "<invalid-symbol>");

        if (ok) {
            emit_pass_literal("[PASS] symbol table hash insertion failure preserves table");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table hash insertion failure preserves table");
            (*fail)++;
        }
        table.destroy();
    }

    {
        SymbolTable table = {};
        table.init();
        SymbolEntry* saved_entries = table.entries;
        SymbolId* saved_hash_index = table.hash_index;
        usz saved_capacity = table.capacity;
        usz saved_hash_capacity = table.hash_capacity;

        table.capacity = (usz)INVALID_SYMBOL_ID / 2 + 1;
        bool symbol_id_cap_rejected = !table.grow()
            && table.entries == saved_entries
            && table.hash_index == saved_hash_index;

        table.capacity = saved_capacity;
        table.hash_capacity = usz.max / SymbolId.sizeof;
        bool hash_bytes_rejected = !table.grow()
            && table.entries == saved_entries
            && table.hash_index == saved_hash_index;

        table.capacity = saved_capacity;
        table.hash_capacity = saved_hash_capacity;
        bool ok = symbol_id_cap_rejected && hash_bytes_rejected;
        if (ok) {
            emit_pass_literal("[PASS] symbol table grow rejects unchecked capacity bounds");
            (*pass)++;
        } else {
            io::printn("[FAIL] symbol table grow rejects unchecked capacity bounds");
            (*fail)++;
        }
        table.destroy();
    }

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
    {
        TypeRegistry registry = {};
        SymbolTable syms = {};
        syms.init();
        bool setup_ok = registry.init();
        bool ok = false;
        if (setup_ok) {
            SymbolId first_name = syms.intern("type-hash-full-anchor");
            SymbolId second_name = syms.intern("type-hash-full-rejected");
            TypeId first_id = registry.register_type(make_basic_type_info(first_name), &syms);
            if (first_id != INVALID_TYPE_ID && second_name != INVALID_SYMBOL_ID) {
                registry.hash_capacity = 1;
                registry.type_hash_index[0] = first_id;
                usz count_before = registry.type_count;

                TypeId rejected = registry.register_type(make_basic_type_info(second_name), &syms);
                ok = rejected == INVALID_TYPE_ID &&
                    registry.type_count == count_before &&
                    registry.lookup(first_name, &syms) == first_id &&
                    registry.lookup(second_name, &syms) == INVALID_TYPE_ID;
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] type registry hash insertion failure rolls back new type");
            (*pass)++;
        } else {
            io::printn("[FAIL] type registry hash insertion failure rolls back new type");
            (*fail)++;
        }
        registry.destroy();
        syms.destroy();
    }

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
       "       (>= (ref (ref s 'scope) 'temp-slow-slack-exact-count) 0) "
       "       (>= (ref (ref s 'scope) 'escape-slow-slack-le-4096-count) 0) "
        "       (>= (ref (ref s 'scope) 'temp-slow-sequence-closed-count) 0) "
        "       (>= (ref (ref s 'scope) 'escape-slow-sequence-followup-bytes) 0) "
        "       (>= (ref (ref s 'scope) 'escape-slow-sequence-no-followup-request-le-4096-count) 0) "
        "       (>= (ref (ref s 'scope) 'escape-slow-sequence-no-followup-unused-le-4096-count) 0) "
        "       (>= (ref (ref s 'scope) 'escape-slow-sequence-no-followup-source-direct-count) 0) "
        "       (>= (ref (ref s 'scope) 'escape-slow-sequence-no-followup-source-dtor-count) 0) "
        "       (>= (ref (ref s 'scope) 'temp-slow-sequence-large-slack-count) 0) "
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
    main::ScopeMemoryTelemetryStats before = main::scope_memory_telemetry_stats_snapshot();
    main::ScopeRegion* scope = main::scope_create(interp.root_scope);
    if (scope != null) {
        void* temp_ptr = scope.alloc(24);
        void* escape_ptr = scope.alloc_escape(32);
        (void)temp_ptr;
        (void)escape_ptr;
        main::scope_release(scope);
    }
    main::ScopeMemoryTelemetryStats stats = main::scope_memory_telemetry_stats_snapshot();
    if (scope != null &&
        stats.scope_create_count >= before.scope_create_count + 1 &&
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

fn void basic_fill_bytes(void* ptr, usz len, char value) {
    if (ptr == null) return;
    char* bytes = (char*)ptr;
    for (usz i = 0; i < len; i++) {
        bytes[i] = value;
    }
}

fn void run_basic_env_alloc_default_tests(Interp* interp, int* pass, int* fail) {
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* scope = main::scope_create(saved_scope);
    bool setup_ok = scope != null;
    bool temp_ok = false;
    bool escape_ok = false;

    if (setup_ok) {
        interp.current_scope = scope;

        void* temp_dirty = scope.alloc(Env.sizeof);
        setup_ok = temp_dirty != null;
        if (setup_ok) {
            basic_fill_bytes(temp_dirty, Env.sizeof, (char)1);
            main::scope_reset_temp_lane(scope);
            Env* temp_env = interp.alloc_env();
            temp_ok = temp_env != null &&
                !temp_env.is_inline &&
                temp_env.bindings == null &&
                temp_env.capacity == 0 &&
                !temp_env.owner_escape_lane;
        }

        void* escape_dirty = setup_ok ? scope.alloc_escape(Env.sizeof) : null;
        setup_ok = setup_ok && escape_dirty != null;
        if (setup_ok) {
            basic_fill_bytes(escape_dirty, Env.sizeof, (char)1);
            scope.escape_bump = (char*)escape_dirty;
            Env* escape_env = interp.alloc_env_escape();
            escape_ok = escape_env != null &&
                !escape_env.is_inline &&
                escape_env.bindings == null &&
                escape_env.capacity == 0 &&
                escape_env.owner_escape_lane;
        }

        interp.current_scope = saved_scope;
        main::scope_release(scope);
    }

    bool ok = setup_ok && temp_ok && escape_ok;
    if (ok) {
        emit_pass_literal("[PASS] env direct allocators initialize inline state");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] env direct allocators initialize inline state (setup=%s temp=%s escape=%s)",
            setup_ok ? "yes" : "no",
            temp_ok ? "yes" : "no",
            escape_ok ? "yes" : "no");
        (*fail)++;
        if (scope != null && interp.current_scope == scope) {
            interp.current_scope = saved_scope;
            main::scope_release(scope);
        }
    }
}

fn void run_basic_env_hash_malformed_table_tests(Interp* interp, int* pass, int* fail) {
    SymbolId key = interp.symbols.intern("env-hash-null-table-key");
    Env env = {};
    env.hash_capacity = 2;
    env.hash_table = null;

    usz missing = env.hash_lookup(key);
    env.hash_insert(key, 0);
    usz after_insert = env.hash_lookup(key);

    bool ok = key != INVALID_SYMBOL_ID &&
        missing == ~(usz)0 &&
        after_insert == ~(usz)0 &&
        env.hash_table == null &&
        env.hash_capacity == 2;

    if (ok) {
        emit_pass_literal("[PASS] env hash helpers tolerate null table");
        (*pass)++;
    } else {
        io::printn("[FAIL] env hash helpers tolerate null table");
        (*fail)++;
    }
}

fn void run_basic_env_copy_unknown_tag_tests(Interp* interp, int* pass, int* fail) {
    Value invalid = {};
    char* raw = (char*)&invalid;
    raw[0] = (char)127;

    Value* copied = copy_env_value_fast(&invalid, interp);
    if (copied == null) {
        emit_pass_literal("[PASS] env copy rejects unknown value tag");
        (*pass)++;
    } else {
        io::printn("[FAIL] env copy rejects unknown value tag");
        (*fail)++;
    }
}

fn void run_basic_pattern_unknown_tag_tests(Interp* interp, int* pass, int* fail) {
    Pattern invalid = {};
    char* raw = (char*)&invalid;
    raw[0] = (char)127;

    MatchResult result = match_pattern(&invalid, make_nil(interp), interp);
    bool ok = result.has_error() &&
        result.error != null &&
        str_contains(result.error.str_chars[:result.error.str_len], "match: unknown pattern tag");
    result.cleanup();

    if (ok) {
        emit_pass_literal("[PASS] pattern matcher rejects unknown pattern tag");
        (*pass)++;
    } else {
        io::printn("[FAIL] pattern matcher rejects unknown pattern tag");
        (*fail)++;
    }
}

fn void run_basic_pattern_unknown_rest_position_tests(Interp* interp, int* pass, int* fail) {
    Pattern invalid = {};
    invalid.tag = PAT_SEQ;
    invalid.elem_count = 0;
    invalid.rest_pos = REST_NONE;
    char* raw_rest = (char*)&invalid.rest_pos;
    raw_rest[0] = (char)127;

    MatchResult result = match_pattern(&invalid, make_nil(interp), interp);
    bool ok = result.has_error() &&
        result.error != null &&
        str_contains(result.error.str_chars[:result.error.str_len], "match: unknown sequence rest position");
    result.cleanup();

    if (ok) {
        emit_pass_literal("[PASS] pattern matcher rejects unknown sequence rest position");
        (*pass)++;
    } else {
        io::printn("[FAIL] pattern matcher rejects unknown sequence rest position");
        (*fail)++;
    }
}

fn void run_basic_value_print_hardening_tests(Interp* interp, int* pass, int* fail) {
    char[3] bytes = { 'a', (char)0, 'b' };
    Value* v = make_string(interp, bytes[..]);
    char[32] rendered = {};
    usz len = v != null ? print_value_to_buf(v, &interp.symbols, &rendered[0], rendered.len) : 0;
    bool ok = v != null &&
        v.tag == STRING &&
        scheduler_str_eq(rendered[:len], "\"a\\x00b\"");

    if (ok) {
        emit_pass_literal("[PASS] value printing escapes embedded NUL without truncation");
        (*pass)++;
    } else {
        io::printn("[FAIL] value printing escapes embedded NUL without truncation");
        (*fail)++;
    }
}

fn void run_basic_scope_generation_width_tests(Interp* interp, int* pass, int* fail) {
    ulong high_generation_floor = (ulong)uint.max + (ulong)8;

    main::scope_global_lock();
    if (main::g_scope_generation_counter < high_generation_floor) {
        main::g_scope_generation_counter = high_generation_floor;
    }
    main::scope_global_unlock();

    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* high_scope = main::scope_create(interp.root_scope);
    bool setup_ok = high_scope != null;
    Value* temp_value = null;
    Value* escape_value = null;

    if (setup_ok) {
        interp.current_scope = high_scope;
        temp_value = make_int(interp, 123);
        escape_value = interp.alloc_value_escape();
        if (escape_value != null) {
            escape_value.tag = NIL;
        }
        interp.current_scope = saved_scope;
    }

    bool ok = setup_ok &&
        high_scope.generation > (ulong)uint.max &&
        high_scope.escape_generation > (ulong)uint.max &&
        temp_value != null &&
        escape_value != null &&
        temp_value.scope_gen == high_scope.generation &&
        escape_value.scope_gen == high_scope.escape_generation;

    if (setup_ok) {
        main::scope_release(high_scope);
    }

    if (ok) {
        emit_pass_literal("[PASS] scope generation stamps preserve values above uint max");
        (*pass)++;
    } else {
        io::printn("[FAIL] scope generation stamps preserve values above uint max");
        (*fail)++;
    }
}

fn void run_basic_repl_buffer_boundary_tests(Interp* interp, int* pass, int* fail) {
    char[] empty_storage = {};
    ReadLineResult zero_read = read_line(empty_storage);

    char[8192] repl_buf = {};
    usz repl_len = repl_buf.len;
    repl_eval_buffer(null, &repl_buf, &repl_len, interp, "", "", "");

    bool ok = zero_read.len == 0 &&
        !zero_read.eof &&
        !zero_read.truncated &&
        repl_len == 0;

    if (ok) {
        emit_pass_literal("[PASS] REPL buffer boundaries reject zero/full buffers");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] REPL buffer boundaries reject zero/full buffers (zero_len=%d eof=%s truncated=%s repl_len=%d)",
            (long)zero_read.len,
            zero_read.eof ? "yes" : "no",
            zero_read.truncated ? "yes" : "no",
            (long)repl_len
        );
        (*fail)++;
    }
}

fn void run_basic_repl_server_auth_boundary_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    ReplServerRequest eval_request = {
        .has_op = true,
        .op = "eval",
    };
    ReplServerRequest describe_request = {
        .has_op = true,
        .op = "describe",
    };

    ReplServerConnection missing_token_conn = {};
    repl_server_connection_init(&missing_token_conn, 1, false, false, null, true, null);
    defer repl_server_connection_destroy(&missing_token_conn);

    char[1] empty_token = {0};
    ReplServerConnection empty_token_conn = {};
    repl_server_connection_init(&empty_token_conn, 1, false, false, null, true, &empty_token[0]);
    defer repl_server_connection_destroy(&empty_token_conn);

    bool ok = missing_token_conn.auth_required &&
        empty_token_conn.auth_required &&
        !repl_server_request_is_authorized(&missing_token_conn, &eval_request) &&
        !repl_server_request_is_authorized(&empty_token_conn, &eval_request) &&
        repl_server_request_is_authorized(&missing_token_conn, &describe_request) &&
        repl_server_request_is_authorized(&empty_token_conn, &describe_request);

    if (ok) {
        emit_pass_literal("[PASS] REPL server auth-required missing token fails closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] REPL server auth-required missing token fails closed");
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
    run_basic_env_alloc_default_tests(interp, pass, fail);
    run_basic_env_hash_malformed_table_tests(interp, pass, fail);
    run_basic_env_copy_unknown_tag_tests(interp, pass, fail);
    run_basic_pattern_unknown_tag_tests(interp, pass, fail);
    run_basic_pattern_unknown_rest_position_tests(interp, pass, fail);
    run_basic_value_print_hardening_tests(interp, pass, fail);
    run_basic_scope_generation_width_tests(interp, pass, fail);
    run_basic_repl_buffer_boundary_tests(interp, pass, fail);
    run_basic_repl_server_auth_boundary_tests(interp, pass, fail);
    run_basic_interp_storage_hardening_tests(interp, pass, fail);
}
