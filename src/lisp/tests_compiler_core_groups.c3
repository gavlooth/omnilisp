module lisp;


import std::io;
import main;
fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BLOCK tests ---
    {
        char[] code = compile_to_c3("(block 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (block 42)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (block 42)"); }
    }
    {
        char[] code = compile_to_c3("(block 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (block 1 2 3)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (block 1 2 3)"); }
    }
    {
        char[] code = compile_to_c3("(block (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (block (define x 1) x)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (block (define x 1) x)"); }
    }
    {
        char[] code = compile_to_c3("(lambda (x) (block x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (lambda (x) (block x x))"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (lambda (x) (block x x))"); }
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
    // Type forms should lower to direct AOT helpers and still sync generated globals.
    {
        char[] code = compile_to_c3("(define [type] Box (^Int value)) (define b (Box 1))", interp);
        bool ok = str_contains(code, "aot::define_type(")
            && str_contains(code, "Box = aot::lookup_var(\"Box\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] lowers directly and syncs constructor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] lowers directly and syncs constructor global"); }
    }

    // Union forms must lower directly and sync generated variant constructors into AOT globals.
    {
        char[] code = compile_to_c3("(define [union] Maybe None (Some Int)) (Some 1)", interp);
        bool ok = str_contains(code, "aot::define_union(")
            && str_contains(code, "None = aot::lookup_var(\"None\")")
            && str_contains(code, "Some = aot::lookup_var(\"Some\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [union] lowers directly and syncs variant globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [union] lowers directly and syncs variant globals"); }
    }

    // Abstract forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [abstract] Entity)", interp);
        bool ok = str_contains(code, "aot::define_abstract(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [abstract] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [abstract] lowers directly"); }
    }

    // Alias forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [alias] UserAlias User)", interp);
        bool ok = str_contains(code, "aot::define_alias(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [alias] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [alias] lowers directly"); }
    }

    // Effect forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [effect] (test/ping (^String msg)))", interp);
        bool ok = str_contains(code, "aot::define_effect(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [effect] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [effect] lowers directly"); }
    }

    // Typed define should install dispatch entries directly through the typed-method helper.
    {
        char[] code = compile_to_c3("(define (describe (^Int x)) \"int\")", interp);
        bool ok = str_contains(code, "aot::define_typed_method(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define lowers through typed-method helper"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define lowers through typed-method helper"); }
    }

    // Typed define should compile captures normally rather than injecting globals into eval.
    {
        char[] code = compile_to_c3(
            "(define y 10) (define (describe (^Int x)) (+ x y))",
            interp
        );
        bool ok = str_contains(code, "aot::define_typed_method(")
            && !str_contains(code, "aot::define_var(\"y\", y);")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define no longer uses eval injection"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define no longer uses eval injection"); }
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

    // Repeated typed methods should all stay on direct helper lowering (no eval fallback path).
    {
        char[] code = compile_to_c3(
            "(define (route (^Int x)) (+ x 1)) "
            "(define (route (^String x)) x) "
            "(define (route (^Double x)) (+ x 1.0))",
            interp
        );
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(");
        bool ok = typed_calls == 3
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define multi-method extension stays direct"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define multi-method extension stays direct"); }
    }

    // Typed + untyped mix should preserve direct typed registration and plain fallback define.
    {
        char[] code = compile_to_c3(
            "(define delta 7) "
            "(define (mix (^Int x)) (+ x delta)) "
            "(define (mix (^String x)) x) "
            "(define (mix x) x)",
            interp
        );
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(");
        bool ok = typed_calls == 2
            && str_contains(code, "mix = ")
            && !str_contains(code, "aot::define_var(\"delta\", delta);")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define fallback mix avoids eval injection"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define fallback mix avoids eval injection"); }
    }

    // Explain dispatch should lower directly instead of delegating through eval.
    {
        char[] code = compile_to_c3("(ref (ref (explain 'dispatch (+ 1 2)) 'decision) 'reason)", interp);
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch lowers directly"); }
    }

    // Explain dispatch direct path: explicit multi-arg call target should stay on helper lowering.
    {
        char[] code = compile_to_c3(
            "(ref (ref (explain 'dispatch (route3 1 2 3)) 'decision) 'reason)",
            interp
        );
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch multi-arg call lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch multi-arg call lowers directly"); }
    }

    // Explain dispatch direct path: explicit zero-arg call target should stay on helper lowering.
    {
        char[] code = compile_to_c3(
            "(ref (ref (explain 'dispatch (route0)) 'decision) 'reason)",
            interp
        );
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch zero-arg call lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch zero-arg call lowers directly"); }
    }

    // Explain effect should also lower directly for signal forms.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (signal io/print \"x\")) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect lowers directly"); }
    }

    // Explain effect should lower directly for perform call-surface spelling.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (perform io/print \"x\")) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect perform call-surface lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect perform call-surface lowers directly"); }
    }

    // Explain effect should lower directly for resolve forms.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (resolve 42)) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_resolve(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect resolve lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect resolve lowers directly"); }
    }

    // Compiler output should not emit serialized-eval bridge calls in normal lowering paths.
    {
        char[] code = compile_to_c3(
            "(define [type] Box (^Int value)) "
            "(define (render (^Int x)) (+ x 1)) "
            "(define (render x) x) "
            "(ref (ref (explain 'dispatch (render 1)) 'decision) 'reason)",
            interp
        );
        bool ok = !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serialized-eval bridge is quarantined from normal lowering"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serialized-eval bridge is quarantined from normal lowering"); }
    }

    // Compiled module forms are static: body lowers inline and form returns Void.
    {
        char[] code = compile_to_c3(
            "(module cmod (export add) (define (add x y) (+ x y)))",
            interp
        );
        bool ok = str_contains(code, "add = ")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module lowers as static inline form"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module lowers as static inline form"); }
    }

    // Compiled import form is intentionally static no-op and returns Void.
    {
        char[] code = compile_to_c3(
            "(module imod (export v) (define v 1)) (import imod)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import lowers as static no-op"); }
    }

    // Compiled export-from form is intentionally static no-op and returns Void.
    {
        char[] code = compile_to_c3(
            "(module exp-src (export z) (define z 9)) "
            "(module exp-dst (export z) (export-from exp-src (z)))",
            interp
        );
        bool ok = str_contains(code, "export-from: no-op in compiled mode")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: export-from lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: export-from lowers as static no-op"); }
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
        bool ok = !str_contains(code, "unsupported")
            && str_contains(code, "x = ")
            && str_contains(code, "aot::make_void()");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: (set! x 20)"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: (set! x 20)"); }
    }
    {
        char[] code = compile_to_c3("(define x 10) (block (set! x 20) x)", interp);
        bool ok = !str_contains(code, "unsupported")
            && str_contains(code, "aot::make_void()");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! command form"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! command form"); }
    }
    {
        char[] code = compile_to_c3("(define counter 0) (define inc! (lambda (n) (set! counter (+ counter n))))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! inside lambda"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! inside lambda"); }
    }
    {
        char[] code = compile_to_c3("(define arr (Array 1 2 3)) (set! arr 1 9)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! generic collection form"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! generic collection form"); }
    }
    {
        char[] code = compile_to_c3("(define hm (Dictionary 'k 1)) (set! hm 'k 9)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! generic dictionary form"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! generic dictionary form"); }
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
    // --- checkpoint/capture via interpreter delegation ---
    {
        char[] code = compile_to_c3("(checkpoint (+ 1 (capture k (k 10))))", interp);
        bool ok = str_contains(code, "compiled_reset") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint/capture compiles natively"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint/capture compiles natively"); }
    }
    {
        char[] code = compile_to_c3("(define x 0) (block (set! x 1) (set! x (+ x 1)) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: set! inside block"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: set! inside block"); }
    }
}

fn void run_compiler_group_continuation_form_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Continuation form tests (native compilation) ---
    {
        char[] code = compile_to_c3("(define x 5) (checkpoint (+ x (capture k (k 10))))", interp);
        bool ok = str_contains(code, "compiled_reset");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint uses compiled_reset"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint uses compiled_reset"); }
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
        char[] code = compile_to_c3("(capture k 42)", interp);
        bool ok = str_contains(code, "compiled_shift");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: capture uses compiled_shift"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: capture uses compiled_shift"); }
    }
    {
        char[] code = compile_to_c3("(checkpoint (+ 1 2))", interp);
        bool ok = str_contains(code, "compiled_reset");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint compiles natively"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint compiles natively"); }
    }
    {
        char[] code = compile_to_c3("(handle (block (signal get 0) (signal set 1)) (get x (resolve 42)) (set v (resolve v)))", interp);
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
        char[] code = compile_to_c3("(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))", interp);
        bool ok = str_contains(code, "compiled_reset") && str_contains(code, "compiled_shift");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint with nested capture"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint with nested capture"); }
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
