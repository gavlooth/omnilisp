module lisp;


import std::io;
import main;
fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BEGIN tests ---
    {
        char[] code = compile_to_c3("(begin 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (begin 42)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (begin 42)"); }
    }
    {
        char[] code = compile_to_c3("(begin 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (begin 1 2 3)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (begin 1 2 3)"); }
    }
    {
        char[] code = compile_to_c3("(begin (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (begin (define x 1) x)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (begin (define x 1) x)"); }
    }
    {
        char[] code = compile_to_c3("(lambda (x) (begin x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (lambda (x) (begin x x))"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (lambda (x) (begin x x))"); }
    }

    // --- Zero-arg lambda tests ---
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: ((lambda () 42))"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: ((lambda () 42))"); }
    }
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: zero-arg define + call"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: zero-arg define + call"); }
    }
    {
        char[] code = compile_to_c3("(let (f (lambda () 10)) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: zero-arg in let"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: zero-arg in let"); }
    }
}

fn void run_compiler_group_syntax_multiarg_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Multi-param + multi-binding (parser desugar) ---
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: multi-param lambda"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: multi-param lambda"); }
    }
    {
        char[] code = compile_to_c3("(let (x 1 y 2) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: multi-binding let"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: multi-binding let"); }
    }
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: 3-param lambda"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: 3-param lambda"); }
    }
}

fn void run_compiler_group_syntax_basics(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_syntax_begin_zero_tests(interp, &pass, &fail);
    run_compiler_group_syntax_multiarg_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_stdlib_availability_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Stdlib availability ---
    {
        char[] code = compile_to_c3("(map (lambda (x) (+ x 1)) (list 1))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: map"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: map"); }
    }
    {
        char[] code = compile_to_c3("(filter (lambda (x) (> x 2)) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: filter"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: filter"); }
    }
    {
        char[] code = compile_to_c3("(foldl + 0 (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: foldl"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: foldl"); }
    }
}

fn void run_compiler_group_existing_feature_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Existing features still work ---
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match"); }
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle/perform"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle/perform"); }
    }
    {
        char[] code = compile_to_c3("(let (x 42) (let (f (lambda () x)) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: zero-arg closure capture"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: zero-arg closure capture"); }
    }
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: id"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: id"); }
    }
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: range"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: range"); }
    }
}

fn void run_compiler_group_type_dispatch_bridge_tests(Interp* interp, usz* pass, usz* fail) {
    // Type forms: currently delegated explicitly via serialized eval bridge.
    {
        char[] code = compile_to_c3("(define [type] Box (^Int value)) (define b (Box 1))", interp);
        bool ok = str_contains(code, "aot::eval_serialized_expr")
            && str_contains(code, "Box = aot::lookup_var(\"Box\")")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] bridge syncs constructor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] bridge syncs constructor global"); }
    }

    // Union forms must sync generated variant constructors into AOT globals.
    {
        char[] code = compile_to_c3("(define [union] Maybe None (Some Int)) (Some 1)", interp);
        bool ok = str_contains(code, "aot::eval_serialized_expr")
            && str_contains(code, "None = aot::lookup_var(\"None\")")
            && str_contains(code, "Some = aot::lookup_var(\"Some\")")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [union] bridge syncs variant globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [union] bridge syncs variant globals"); }
    }

    // Typed define should use explicit eval bridge (method-table semantics stay in runtime path).
    {
        char[] code = compile_to_c3("(define (describe (^Int x)) \"int\")", interp);
        bool ok = str_contains(code, "aot::eval_serialized_expr")
            && str_contains(code, "aot::lookup_var(\"describe\")")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define uses explicit eval bridge"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define uses explicit eval bridge"); }
    }

    // Repeated typed define names should not emit duplicate global declarations.
    {
        char[] code = compile_to_c3(
            "(define (describe (^Int x)) \"int\") (define (describe (^String x)) \"str\")",
            interp
        );
        bool dup = str_contains(code, "lisp::Value* describe;\nlisp::Value* describe;");
        bool ok = !dup;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define globals are deduplicated"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define globals are deduplicated"); }
    }

    // Explain primitives should be mapped through primitive lookup/caching.
    {
        char[] code = compile_to_c3("(ref (ref (explain 'dispatch (+ 1 2)) 'decision) 'reason)", interp);
        bool ok = str_contains(code, "aot::lookup_prim(\"explain\")")
            || str_contains(code, "_prim_explain")
            || (str_contains(code, "aot::eval_serialized_expr")
                && str_contains(code, "(explain 'dispatch (+ 1 2))"));
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain is mapped as a primitive"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain is mapped as a primitive"); }
    }

    // Parsed explain forms should delegate through serialized eval to preserve thunk semantics.
    {
        char[] code = compile_to_c3("(ref (explain 'dispatch (+ 1 2)) 'status)", interp);
        bool ok = str_contains(code, "aot::eval_serialized_expr")
            && str_contains(code, "(explain 'dispatch (+ 1 2))")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain forms use eval bridge"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain forms use eval bridge"); }
    }
}

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_set_mutation_tests(Interp* interp, usz* pass, usz* fail) {
    // --- set! tests ---
    {
        char[] code = compile_to_c3("(define x 10) (set! x 20)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "x = ");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (set! x 20)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (set! x 20)"); }
    }
    {
        char[] code = compile_to_c3("(define x 10) (+ (set! x 20) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! in expression"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! in expression"); }
    }
    {
        char[] code = compile_to_c3("(define counter 0) (define inc! (lambda (n) (set! counter (+ counter n))))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! inside lambda"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! inside lambda"); }
    }
}

fn void run_compiler_group_dot_index_tests(Interp* interp, usz* pass, usz* fail) {
    // --- dot-bracket indexing tests ---
    {
        char[] code = compile_to_c3("(define lst (list 10 20 30)) lst.[1]", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "aot::index");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lst.[1]"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lst.[1]"); }
    }
    {
        char[] code = compile_to_c3("(define lst (list 1 2 3)) lst.[(+ 0 1)]", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "aot::index");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lst.[(+ 0 1)]"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lst.[(+ 0 1)]"); }
    }
}

fn void run_compiler_group_path_notation_tests(Interp* interp, usz* pass, usz* fail) {
    // --- path notation tests ---
    {
        char[] code = compile_to_c3("(define point (list (cons 'x 10) (cons 'y 20))) point.x", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "aot::field_access");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: point.x"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: point.x"); }
    }
    {
        char[] code = compile_to_c3("(define obj (list (cons 'inner (list (cons 'val 42))))) obj.inner.val", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "aot::field_access");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: obj.inner.val"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: obj.inner.val"); }
    }
    {
        char[] code = compile_to_c3("point.x", interp);
        bool ok = str_contains(code, "make_string");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: path uses make_string for field name"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: path uses make_string for field name"); }
    }
}

fn void run_compiler_group_set_path_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_set_mutation_tests(interp, &pass, &fail);
    run_compiler_group_dot_index_tests(interp, &pass, &fail);
    run_compiler_group_path_notation_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_continuation_set_begin_tests(Interp* interp, usz* pass, usz* fail) {
    // --- reset/shift via interpreter delegation ---
    {
        char[] code = compile_to_c3("(reset (+ 1 (shift k (k 10))))", interp);
        bool ok = str_contains(code, "compiled_reset") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: reset/shift compiles natively"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: reset/shift compiles natively"); }
    }
    {
        char[] code = compile_to_c3("(define x 0) (begin (set! x 1) (set! x (+ x 1)) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! inside begin"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! inside begin"); }
    }
}

fn void run_compiler_group_continuation_form_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Continuation form tests (native compilation) ---
    {
        char[] code = compile_to_c3("(define x 5) (reset (+ x (shift k (k 10))))", interp);
        bool ok = str_contains(code, "compiled_reset");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: reset uses compiled_reset"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: reset uses compiled_reset"); }
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = str_contains(code, "compiled_handle") && str_contains(code, "compiled_signal");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle/signal uses native compilation"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle/signal uses native compilation"); }
    }
    {
        char[] code = compile_to_c3("(signal raise \"error\")", interp);
        bool ok = str_contains(code, "compiled_signal");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: signal uses compiled_signal"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: signal uses compiled_signal"); }
    }
    {
        char[] code = compile_to_c3("(shift k 42)", interp);
        bool ok = str_contains(code, "compiled_shift");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: shift uses compiled_shift"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: shift uses compiled_shift"); }
    }
    {
        char[] code = compile_to_c3("(reset (+ 1 2))", interp);
        bool ok = str_contains(code, "compiled_reset");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: reset compiles natively"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: reset compiles natively"); }
    }
    {
        char[] code = compile_to_c3("(handle (begin (signal get 0) (signal set 1)) (get x (resolve 42)) (set v (resolve v)))", interp);
        bool ok = str_contains(code, "compiled_handle");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle with multiple clauses"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle with multiple clauses"); }
    }
    {
        char[] code = compile_to_c3("(try (lambda (xx) (+ 1 2)) (lambda (msg) 0))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: try from stdlib"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: try from stdlib"); }
    }
    {
        char[] code = compile_to_c3("(reset (+ 1 (shift k (+ (k 10) (k 20)))))", interp);
        bool ok = str_contains(code, "compiled_reset") && str_contains(code, "compiled_shift");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: reset with nested shift"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: reset with nested shift"); }
    }
}

fn void run_compiler_group_continuation_native_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_continuation_set_begin_tests(interp, &pass, &fail);
    run_compiler_group_continuation_form_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
