module lisp;


import std::io;

fn void run_basic_intro_tests(Interp* interp, int* pass, int* fail) {
    // Literals & bindings
    test_eq(interp, "integer literal 42", "42", 42, pass, fail);
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
        "(= (symbol->string (handle (block (println (quote hello)) nil) (io/println x x))) \"hello\")",
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
    test_truthy(interp, "index/ref parity list", "(= (ref '(10 20 30) 1) (let (l '(10 20 30)) l.[1]))", pass, fail);
    test_truthy(interp, "index/ref parity list negative", "(= (ref '(10 20 30) -1) (let (l '(10 20 30)) l.[-1]))", pass, fail);
    test_truthy(interp, "index/ref parity array", "(= (ref [10 20 30] 2) (let (a [10 20 30]) a.[2]))", pass, fail);
    test_truthy(interp, "index/ref parity array negative", "(= (ref [10 20 30] -2) (let (a [10 20 30]) a.[-2]))", pass, fail);
    test_truthy(interp, "index/ref parity dict", "(= (ref {'a 1 'b 2} 'b) (let (m {'a 1 'b 2}) m.['b]))", pass, fail);
    test_truthy(interp, "index/ref parity string", "(= (ref \"xyz\" 1) (\"xyz\".[1]))", pass, fail);
    test_truthy(interp, "index/ref parity string negative", "(= (ref \"xyz\" -1) (\"xyz\".[-1]))", pass, fail);
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
        SymbolTable table = {};
        g_symbol_table_force_entries_alloc_fail = true;
        table.init();
        g_symbol_table_force_entries_alloc_fail = false;

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

        g_symbol_table_force_name_alloc_fail = true;
        SymbolId failed = table.intern("beta");
        g_symbol_table_force_name_alloc_fail = false;

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
        g_symbol_table_force_grow_entries_alloc_fail = true;
        SymbolId failed = table.intern("trigger-grow-fail");
        g_symbol_table_force_grow_entries_alloc_fail = false;
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

fn void run_basic_interp_storage_hardening_tests(Interp* interp, int* pass, int* fail) {
    {
        Interp local = {};
        g_interp_force_macro_table_alloc_fail = true;
        local.init();
        g_interp_force_macro_table_alloc_fail = false;

        bool ok = local.macro_table == null
            && local.macro_hash_index == null
            && local.macro_capacity == 0
            && local.macro_hash_capacity == 0
            && local.ensure_macro_capacity(1)
            && local.macro_table != null
            && local.macro_hash_index != null;
        if (ok) {
            emit_pass_literal("[PASS] interp macro init allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro init allocation failure fails closed");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        g_interp_force_module_table_alloc_fail = true;
        local.init();
        g_interp_force_module_table_alloc_fail = false;

        bool ok = local.modules == null
            && local.module_hash_index == null
            && local.module_capacity == 0
            && local.module_hash_capacity == 0
            && local.ensure_module_capacity(1)
            && local.modules != null
            && local.module_hash_index != null;
        if (ok) {
            emit_pass_literal("[PASS] interp module init allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp module init allocation failure fails closed");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        g_interp_force_handler_stack_alloc_fail = true;
        local.init();
        g_interp_force_handler_stack_alloc_fail = false;

        bool ok = local.handler_stack == null
            && local.handler_capacity == 0
            && local.ensure_handler_capacity(1)
            && local.handler_stack != null
            && local.handler_capacity >= 1;
        if (ok) {
            emit_pass_literal("[PASS] interp handler stack init allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp handler stack init allocation failure fails closed");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;
        bool failed = !local.ensure_macro_capacity(usz.max);
        bool preserved = local.macro_table == before_table
            && local.macro_hash_index == before_hash
            && local.macro_capacity == before_cap
            && local.macro_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp macro capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;
        usz max_macro_needed = usz.max / MacroDef.sizeof + 1;
        bool failed = !local.ensure_macro_capacity(max_macro_needed);
        bool preserved = local.macro_table == before_table
            && local.macro_hash_index == before_hash
            && local.macro_capacity == before_cap
            && local.macro_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp macro item-bytes overflow guards growth");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro item-bytes overflow guards growth");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        Module* before_modules = local.modules;
        usz* before_hash = local.module_hash_index;
        usz before_cap = local.module_capacity;
        usz before_hash_cap = local.module_hash_capacity;
        bool failed = !local.ensure_module_capacity(usz.max);
        bool preserved = local.modules == before_modules
            && local.module_hash_index == before_hash
            && local.module_capacity == before_cap
            && local.module_hash_capacity == before_hash_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp module capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp module capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        EffectHandler* before_stack = local.handler_stack;
        usz before_cap = local.handler_capacity;
        bool failed = !local.ensure_handler_capacity(usz.max);
        bool preserved = local.handler_stack == before_stack
            && local.handler_capacity == before_cap;

        if (failed && preserved) {
            emit_pass_literal("[PASS] interp handler capacity overflow guards allocation bytes");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp handler capacity overflow guards allocation bytes");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        SymbolId name = local.symbols.intern("macro-grow-check");
        local.macro_table[0] = (MacroDef){
            .name = name,
            .clauses = null,
            .clause_count = 0,
            .captured_bindings = null,
            .captured_count = 0,
        };
        local.macro_count = 1;
        usz slot = (usz)name % local.macro_hash_capacity;
        while (local.macro_hash_index[slot] != usz.max) {
            slot = (slot + 1) % local.macro_hash_capacity;
        }
        local.macro_hash_index[slot] = 0;

        MacroDef* before_table = local.macro_table;
        usz* before_hash = local.macro_hash_index;
        usz before_cap = local.macro_capacity;
        usz before_hash_cap = local.macro_hash_capacity;

        g_interp_force_grow_macro_table_alloc_fail = true;
        bool failed = !local.ensure_macro_capacity(local.macro_capacity + 1);
        g_interp_force_grow_macro_table_alloc_fail = false;
        bool preserved = local.macro_count == 1
            && before_table == local.macro_table
            && before_hash == local.macro_hash_index
            && before_cap == local.macro_capacity
            && before_hash_cap == local.macro_hash_capacity;
        bool recovered = local.ensure_macro_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && lookup_macro_index(name, &local) == 0;
        if (ok) {
            emit_pass_literal("[PASS] interp macro grow allocation failure preserves lookup");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp macro grow allocation failure preserves lookup");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        SymbolId name = local.symbols.intern("module-grow-check");
        local.modules[0] = (Module){
            .name = name,
            .env = null,
            .exports = null,
            .export_count = 0,
            .export_capacity = 0,
            .loaded = true,
            .path = null,
            .path_len = 0,
        };
        local.module_count = 1;
        usz slot = (usz)name % local.module_hash_capacity;
        while (local.module_hash_index[slot] != usz.max) {
            slot = (slot + 1) % local.module_hash_capacity;
        }
        local.module_hash_index[slot] = 0;

        Module* before_modules = local.modules;
        usz* before_hash = local.module_hash_index;
        usz before_cap = local.module_capacity;
        usz before_hash_cap = local.module_hash_capacity;

        g_interp_force_grow_module_table_alloc_fail = true;
        bool failed = !local.ensure_module_capacity(local.module_capacity + 1);
        g_interp_force_grow_module_table_alloc_fail = false;
        bool preserved = local.module_count == 1
            && before_modules == local.modules
            && before_hash == local.module_hash_index
            && before_cap == local.module_capacity
            && before_hash_cap == local.module_hash_capacity;
        bool recovered = local.ensure_module_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && find_module(name, &local) == &local.modules[0];
        if (ok) {
            emit_pass_literal("[PASS] interp module grow allocation failure preserves lookup");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp module grow allocation failure preserves lookup");
            (*fail)++;
        }
        local.destroy();
    }

    {
        Interp local = {};
        local.init();
        local.handler_stack[0] = (EffectHandler){
            .tags = null,
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = true,
            .effect_state = null,
        };
        local.handler_count = 1;

        EffectHandler* before_stack = local.handler_stack;
        usz before_cap = local.handler_capacity;

        g_interp_force_grow_handler_stack_alloc_fail = true;
        bool failed = !local.ensure_handler_capacity(local.handler_capacity + 1);
        g_interp_force_grow_handler_stack_alloc_fail = false;
        bool preserved = before_stack == local.handler_stack
            && before_cap == local.handler_capacity
            && local.handler_count == 1;
        bool recovered = local.ensure_handler_capacity(before_cap + 1);

        bool ok = failed
            && preserved
            && recovered
            && local.handler_stack[0].clause_count == 1
            && local.handler_stack[0].strict_mode;
        if (ok) {
            emit_pass_literal("[PASS] interp handler stack grow allocation failure preserves handlers");
            (*pass)++;
        } else {
            io::printn("[FAIL] interp handler stack grow allocation failure preserves handlers");
            (*fail)++;
        }
        local.destroy();
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
    run_basic_interp_storage_hardening_tests(interp, pass, fail);
}

fn void run_memory_stress_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Memory Stress Tests");

    // Deep recursion
    setup(interp, "(define sum-to (lambda (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))");
    test_eq(interp, "deep recursion sum 1..50 => 1275", "(sum-to 50)", 1275, pass, fail);

    // Closure factory
    setup(interp, "(define make-counter (lambda (start) (lambda (x) (+ start x))))");
    setup(interp, "(define c1 (make-counter 10))");
    setup(interp, "(define c2 (make-counter 20))");
    setup(interp, "(define c3 (make-counter 30))");
    test_eq(interp, "closure c1 5 => 15", "(c1 5)", 15, pass, fail);
    test_eq(interp, "closure c2 5 => 25", "(c2 5)", 25, pass, fail);
    test_eq(interp, "closure c3 5 => 35", "(c3 5)", 35, pass, fail);

    // String operations
    test_eq(interp, "string-length hello world => 11", "(string-length \"hello world\")", 11, pass, fail);
    test_str(interp, "string-upcase hello => string", "(string-upcase \"hello\")", pass, fail);

    // Pattern matching stress
    setup(interp, "(define data (quote (1 2 3 4 5 6 7 8 9 10)))");
    test_eq(interp, "match [a b c .. rest]", "(match data ([a b c .. rest] (+ a (+ b c))))", 6, pass, fail);
    test_eq(interp, "match [.. last] => 10", "(match data ([.. last] last))", 10, pass, fail);

    // Fibonacci
    setup(interp, "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))");
    test_eq(interp, "fib 15 => 610", "(fib 15)", 610, pass, fail);

    // Map over list
    setup(interp, "(define mymap (lambda (f) (lambda (lst) (if (null? lst) (quote ()) (cons (f (car lst)) ((mymap f) (cdr lst)))))))");
    setup(interp, "(define double (lambda (x) (* x 2)))");
    test_eq(interp, "map double car => 2", "(car ((mymap double) (quote (1 2 3 4 5))))", 2, pass, fail);
    setup(interp, "(define mapped ((mymap double) (quote (1 2 3 4 5))))");
    test_eq(interp, "map double last => 10", "(match mapped ([.. last] last))", 10, pass, fail);

    // Nested let 10 levels
    test_eq(interp, "nested let 10 levels => 55", "(let (a 1) (let (b 2) (let (c 3) (let (d 4) (let (e 5) (let (f 6) (let (g 7) (let (h 8) (let (i 9) (let (j 10) (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h (+ i j)))))))))))))))))))))", 55, pass, fail);

    // Repeated evaluation
    setup(interp, "(define mem-counter 0)");
    for (int i = 0; i < 100; i++) { run("(define mem-counter (+ mem-counter 1))", interp); }
    test_eq(interp, "repeated eval 100x", "mem-counter", 100, pass, fail);

    // Large list
    setup(interp, "(define build-list-helper (lambda (n) (lambda (acc) (if (= n 0) acc ((build-list-helper (- n 1)) (cons n acc))))))");
    setup(interp, "(define biglist ((build-list-helper 50) (quote ())))");
    test_eq(interp, "build-list 50 elements", "(length biglist)", 50, pass, fail);

    // Fold
    setup(interp, "(define fold-helper (lambda (f) (lambda (init) (lambda (lst) (if (null? lst) init (((fold-helper f) (f init (car lst))) (cdr lst)))))))");
    test_eq(interp, "fold sum 1..10 => 55", "(((fold-helper +) 0) (quote (1 2 3 4 5 6 7 8 9 10)))", 55, pass, fail);
}

fn void run_arithmetic_basic_ops_tests(Interp* interp, int* pass, int* fail) {
    // Addition
    test_eq(interp, "(+ 1 2) => 3", "(+ 1 2)", 3, pass, fail);
    test_eq(interp, "(+ 0 0) => 0", "(+ 0 0)", 0, pass, fail);
    test_eq(interp, "(+ -5 3) => -2", "(+ -5 3)", -2, pass, fail);
    test_eq(interp, "(+ 100 -100) => 0", "(+ 100 -100)", 0, pass, fail);
    test_eq(interp, "(+ 1000000 2000000) => 3000000", "(+ 1000000 2000000)", 3000000, pass, fail);

    // Subtraction
    test_eq(interp, "(- 10 3) => 7", "(- 10 3)", 7, pass, fail);
    test_eq(interp, "(- 3 10) => -7", "(- 3 10)", -7, pass, fail);
    test_eq(interp, "(- 0 0) => 0", "(- 0 0)", 0, pass, fail);
    test_eq(interp, "(- -5 -3) => -2", "(- -5 -3)", -2, pass, fail);

    // Multiplication
    test_eq(interp, "(* 3 4) => 12", "(* 3 4)", 12, pass, fail);
    test_eq(interp, "(* 0 999) => 0", "(* 0 999)", 0, pass, fail);
    test_eq(interp, "(* 1 42) => 42", "(* 1 42)", 42, pass, fail);
    test_eq(interp, "(* -3 4) => -12", "(* -3 4)", -12, pass, fail);
    test_eq(interp, "(* -3 -4) => 12", "(* -3 -4)", 12, pass, fail);

    // Division
    test_eq(interp, "(/ 10 2) => 5", "(/ 10 2)", 5, pass, fail);
    test_eq(interp, "(/ 7 2) => 3", "(/ 7 2)", 3, pass, fail);
    test_eq(interp, "(/ 0 5) => 0", "(/ 0 5)", 0, pass, fail);
    test_eq(interp, "(/ -10 3) => -3", "(/ -10 3)", -3, pass, fail);

    // Modulo
    test_eq(interp, "(% 10 3) => 1", "(% 10 3)", 1, pass, fail);
    test_eq(interp, "(% 9 3) => 0", "(% 9 3)", 0, pass, fail);
    test_eq(interp, "(% 7 10) => 7", "(% 7 10)", 7, pass, fail);

    // Nested arithmetic
    test_eq(interp, "(+ 1 (+ 2 3)) => 6", "(+ 1 (+ 2 3))", 6, pass, fail);
    test_eq(interp, "(* 2 (+ 3 4)) => 14", "(* 2 (+ 3 4))", 14, pass, fail);
    test_eq(interp, "(- (* 5 5) (+ 10 5)) => 10", "(- (* 5 5) (+ 10 5))", 10, pass, fail);
    test_eq(interp, "(/ (+ 10 20) (- 10 4)) => 5", "(/ (+ 10 20) (- 10 4))", 5, pass, fail);
}

fn void run_arithmetic_comparison_ops_tests(Interp* interp, int* pass, int* fail) {
    // Equality
    test_truthy(interp, "(= 5 5) => true", "(= 5 5)", pass, fail);
    test_nil(interp, "(= 5 6) => nil", "(= 5 6)", pass, fail);
    test_truthy(interp, "(= 0 0) => true", "(= 0 0)", pass, fail);
    test_truthy(interp, "(= -1 -1) => true", "(= -1 -1)", pass, fail);

    // Less than
    test_truthy(interp, "(< 1 2) => true", "(< 1 2)", pass, fail);
    test_nil(interp, "(< 2 1) => nil", "(< 2 1)", pass, fail);
    test_nil(interp, "(< 1 1) => nil", "(< 1 1)", pass, fail);
    test_truthy(interp, "(< -5 0) => true", "(< -5 0)", pass, fail);

    // Greater than
    test_truthy(interp, "(> 2 1) => true", "(> 2 1)", pass, fail);
    test_nil(interp, "(> 1 2) => nil", "(> 1 2)", pass, fail);
    test_nil(interp, "(> 1 1) => nil", "(> 1 1)", pass, fail);

    // Less or equal
    test_truthy(interp, "(<= 1 2) => true", "(<= 1 2)", pass, fail);
    test_truthy(interp, "(<= 2 2) => true", "(<= 2 2)", pass, fail);
    test_nil(interp, "(<= 3 2) => nil", "(<= 3 2)", pass, fail);

    // Greater or equal
    test_truthy(interp, "(>= 2 1) => true", "(>= 2 1)", pass, fail);
    test_truthy(interp, "(>= 2 2) => true", "(>= 2 2)", pass, fail);
    test_nil(interp, "(>= 1 2) => nil", "(>= 1 2)", pass, fail);

    // Comparison in conditionals
    test_eq(interp, "(if (< 1 2) 10 20) => 10", "(if (< 1 2) 10 20)", 10, pass, fail);
    test_eq(interp, "(if (> 1 2) 10 20) => 20", "(if (> 1 2) 10 20)", 20, pass, fail);
    test_eq(interp, "(if (= (+ 2 3) 5) 1 0) => 1", "(if (= (+ 2 3) 5) 1 0)", 1, pass, fail);
}

fn void run_arithmetic_comparison_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Arithmetic & Comparison Tests");
    run_arithmetic_basic_ops_tests(interp, pass, fail);
    run_arithmetic_comparison_ops_tests(interp, pass, fail);
}

fn void run_string_type_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("String & Type Predicate Tests");

    // String length
    test_eq(interp, "string-length hello => 5", "(string-length \"hello\")", 5, pass, fail);
    test_eq(interp, "string-length empty => 0", "(string-length \"\")", 0, pass, fail);
    test_eq(interp, "string-length a => 1", "(string-length \"a\")", 1, pass, fail);

    // String case/trim/append/substring
    test_str(interp, "string-upcase hello", "(string-upcase \"hello\")", pass, fail);
    test_str(interp, "string-downcase HELLO", "(string-downcase \"HELLO\")", pass, fail);
    test_str(interp, "string-upcase empty", "(string-upcase \"\")", pass, fail);
    test_str(interp, "string-trim", "(string-trim \"  hello  \")", pass, fail);
    test_str(interp, "string-trim noop", "(string-trim \"hello\")", pass, fail);
    test_str(interp, "string-append", "(string-append \"hello\" \" world\")", pass, fail);
    test_str(interp, "string-append empty", "(string-append \"\" \"abc\")", pass, fail);
    test_str(interp, "substring 0 5", "(substring \"hello world\" 0 5)", pass, fail);
    test_str(interp, "substring 6 11", "(substring \"hello world\" 6 11)", pass, fail);

    // String predicates
    test_truthy(interp, "string? hello => true", "(string? \"hello\")", pass, fail);
    test_nil(interp, "string? 42 => nil", "(string? 42)", pass, fail);
    test_nil(interp, "string? nil => nil", "(string? nil)", pass, fail);

    // int?
    test_truthy(interp, "int? 42 => true", "(int? 42)", pass, fail);
    test_truthy(interp, "int? 0 => true", "(int? 0)", pass, fail);
    test_truthy(interp, "int? -5 => true", "(int? -5)", pass, fail);
    test_nil(interp, "int? hello => nil", "(int? \"hello\")", pass, fail);
    test_nil(interp, "int? nil => nil", "(int? nil)", pass, fail);

    // symbol?
    test_truthy(interp, "symbol? foo => true", "(symbol? (quote foo))", pass, fail);
    test_nil(interp, "symbol? 42 => nil", "(symbol? 42)", pass, fail);
    test_nil(interp, "symbol? hello => nil", "(symbol? \"hello\")", pass, fail);

    // closure?
    test_truthy(interp, "closure? lambda => true", "(closure? (lambda (x) x))", pass, fail);
    test_nil(interp, "closure? 42 => nil", "(closure? 42)", pass, fail);
    test_nil(interp, "closure? hello => nil", "(closure? \"hello\")", pass, fail);

    // null?
    test_truthy(interp, "null? nil => true", "(null? nil)", pass, fail);
    test_truthy(interp, "null? '() => true", "(null? (quote ()))", pass, fail);
    test_nil(interp, "null? 42 => nil", "(null? 42)", pass, fail);
    test_nil(interp, "null? '(1 2) => nil", "(null? (quote (1 2)))", pass, fail);

    // pair?
    test_truthy(interp, "pair? cons => true", "(pair? (cons 1 2))", pass, fail);
    test_truthy(interp, "pair? list => true", "(pair? (quote (1 2 3)))", pass, fail);
    test_nil(interp, "pair? nil => nil", "(pair? nil)", pass, fail);
    test_nil(interp, "pair? 42 => nil", "(pair? 42)", pass, fail);

    // not
    test_truthy(interp, "not nil => true", "(not nil)", pass, fail);
    test_truthy(interp, "not false => true", "(not false)", pass, fail);
    test_nil(interp, "not true => nil", "(not true)", pass, fail);
    test_nil(interp, "not 42 => nil", "(not 42)", pass, fail);
    test_nil(interp, "not 0 => nil (truthy)", "(not 0)", pass, fail);
    test_nil(interp, "not empty-str => nil (truthy)", "(not \"\")", pass, fail);
}

fn void run_list_closure_cons_quote_tests(Interp* interp, int* pass, int* fail) {
    // cons/car/cdr
    test_eq(interp, "car (cons 1 2) => 1", "(car (cons 1 2))", 1, pass, fail);
    test_eq(interp, "cdr (cons 1 2) => 2", "(cdr (cons 1 2))", 2, pass, fail);
    test_eq(interp, "car (cons (+ 1 2) 4) => 3", "(car (cons (+ 1 2) 4))", 3, pass, fail);
    test_eq(interp, "cadr => 2", "(car (cdr (cons 1 (cons 2 (cons 3 nil)))))", 2, pass, fail);
    test_eq(interp, "caddr => 3", "(car (cdr (cdr (cons 1 (cons 2 (cons 3 nil))))))", 3, pass, fail);

    // Length
    test_eq(interp, "length '() => 0", "(length (quote ()))", 0, pass, fail);
    test_eq(interp, "length '(1) => 1", "(length (quote (1)))", 1, pass, fail);
    test_eq(interp, "length '(1..10) => 10", "(length (quote (1 2 3 4 5 6 7 8 9 10)))", 10, pass, fail);
    test_eq(interp, "length cons => 2", "(length (cons 1 (cons 2 nil)))", 2, pass, fail);

    // Quote
    test_eq(interp, "car '(10 20 30) => 10", "(car (quote (10 20 30)))", 10, pass, fail);
    test_eq(interp, "length '(a b c d e) => 5", "(length (quote (a b c d e)))", 5, pass, fail);
    test_truthy(interp, "null? '() => true", "(null? (quote ()))", pass, fail);

    // List indexing
    setup(interp, "(define idx-list (quote (10 20 30)))");
    test_eq(interp, "idx-list.[0] => 10", "idx-list.[0]", 10, pass, fail);
    test_eq(interp, "idx-list.[1] => 20", "idx-list.[1]", 20, pass, fail);
    test_eq(interp, "idx-list.[2] => 30", "idx-list.[2]", 30, pass, fail);
}

fn void run_list_closure_factory_compose_tests(Interp* interp, int* pass, int* fail) {
    // Closures
    setup(interp, "(define make-adder2 (lambda (n) (lambda (x) (+ n x))))");
    setup(interp, "(define add10 (make-adder2 10))");
    setup(interp, "(define add20 (make-adder2 20))");
    test_eq(interp, "add10 5 => 15", "(add10 5)", 15, pass, fail);
    test_eq(interp, "add20 5 => 25", "(add20 5)", 25, pass, fail);

    // Composition
    setup(interp, "(define compose (lambda (f) (lambda (g) (lambda (x) (f (g x))))))");
    setup(interp, "(define double2 (lambda (x) (* x 2)))");
    setup(interp, "(define inc (lambda (x) (+ x 1)))");
    setup(interp, "(define double-then-inc (compose inc double2))");
    test_eq(interp, "compose inc double 5 => 11", "(double-then-inc 5)", 11, pass, fail);

    // Multi-capture
    setup(interp, "(define make-linear (lambda (a) (lambda (b) (lambda (x) (+ (* a x) b)))))");
    setup(interp, "(define f2x3 ((make-linear 2) 3))");
    test_eq(interp, "f(x)=2x+3, f(10)=23", "(f2x3 10)", 23, pass, fail);
}

fn void run_list_closure_scope_hof_tests(Interp* interp, int* pass, int* fail) {
    // Scoping
    test_eq(interp, "let shadowing", "(let (x 10) (let (x 20) x))", 20, pass, fail);
    setup(interp, "(define outer-x 100)");
    setup(interp, "(let (outer-x 999) outer-x)");
    test_eq(interp, "let no leak", "outer-x", 100, pass, fail);

    setup(interp, "(define scope-test 42)");
    setup(interp, "(define scope-fn (lambda (scope-test) scope-test))");
    test_eq(interp, "lambda scope param", "(scope-fn 99)", 99, pass, fail);
    test_eq(interp, "global preserved", "scope-test", 42, pass, fail);

    // Recursive let
    test_eq(interp, "let ^rec fact 6 => 720", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 6))", 720, pass, fail);

    // Mutual recursion
    setup(interp, "(define is-even (lambda (n) (if (= n 0) true (is-odd (- n 1)))))");
    setup(interp, "(define is-odd (lambda (n) (if (= n 0) false (is-even (- n 1)))))");
    test_truthy(interp, "even 10 => true", "(is-even 10)", pass, fail);
    test_truthy(interp, "odd 7 => true", "(is-odd 7)", pass, fail);

    // Higher-order
    setup(interp, "(define apply-twice (lambda (f) (lambda (x) (f (f x)))))");
    setup(interp, "(define add3 (lambda (x) (+ x 3)))");
    test_eq(interp, "apply-twice add3 10 => 16", "((apply-twice add3) 10)", 16, pass, fail);

    // Identity
    setup(interp, "(define id (lambda (x) x))");
    test_eq(interp, "id 42 => 42", "(id 42)", 42, pass, fail);
    test_str(interp, "id hello => string", "(id \"hello\")", pass, fail);
    test_nil(interp, "id nil => nil", "(id nil)", pass, fail);
}

fn void run_list_closure_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("List & Closure Tests");
    run_list_closure_cons_quote_tests(interp, pass, fail);
    run_list_closure_factory_compose_tests(interp, pass, fail);
    run_list_closure_scope_hof_tests(interp, pass, fail);
}
