module lisp;


import std::collections::list;
import std::io;
import main;

fn bool compiler_str_eq(char[] left, char[] right) {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool compiler_serializes_to_roundtrip(Interp* interp, char[] source, char[] expected) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);
    Expr* expr = p.parse_expr();
    if (expr == null || p.has_error) {
        io::printfn("[DEBUG] serializer source parse failed: %s", (ZString)source);
        return false;
    }

    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();

    List{char} buf;
    defer buf.free();
    compiler.serialize_expr_to_buf(expr, &buf);

    char[] actual = "";
    if (buf.len() > 0) {
        actual = buf.entries[:buf.len()];
    }
    if (!compiler_str_eq(actual, expected)) {
        io::print("[DEBUG] serializer expected: ");
        io::printn(expected);
        io::print("[DEBUG] serializer actual:   ");
        io::printn(actual);
        return false;
    }

    Expr* reparsed = parse(actual, interp);
    if (reparsed == null) {
        io::printfn("[DEBUG] serializer reparse failed: %s", (ZString)actual);
        return false;
    }
    return true;
}

fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BLOCK tests ---
    {
        char[] code = compile_to_c3("(block 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block 42)", "[FAIL] Compiler: (block 42)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        compiler_test_finish_bool(ok, "Compiler: (block 1 2 3)", "[FAIL] Compiler: (block 1 2 3)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block (define x 1) x)", "[FAIL] Compiler: (block (define x 1) x)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(lambda (x) (block x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (lambda (x) (block x x))", "[FAIL] Compiler: (lambda (x) (block x x))", pass, fail);
    }

    // --- Zero-arg lambda tests ---
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        compiler_test_finish_bool(ok, "Compiler: ((lambda () 42))", "[FAIL] Compiler: ((lambda () 42))", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg define + call", "[FAIL] Compiler: zero-arg define + call", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (f (lambda () 10)) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg in let", "[FAIL] Compiler: zero-arg in let", pass, fail);
    }
}

fn void run_compiler_group_syntax_multiarg_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Multi-param + multi-binding (parser desugar) ---
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-param lambda", "[FAIL] Compiler: multi-param lambda", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 1 y 2) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-binding let", "[FAIL] Compiler: multi-binding let", pass, fail);
    }
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: 3-param lambda", "[FAIL] Compiler: 3-param lambda", pass, fail);
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
        compiler_test_finish_bool(ok, "Compiler: map", "[FAIL] Compiler: map", pass, fail);
    }
    {
        char[] code = compile_to_c3("(filter (lambda (x) (> x 2)) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: filter", "[FAIL] Compiler: filter", pass, fail);
    }
    {
        char[] code = compile_to_c3("(foldl + 0 (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: foldl", "[FAIL] Compiler: foldl", pass, fail);
    }
}

fn void run_compiler_group_existing_feature_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Existing features still work ---
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: match", "[FAIL] Compiler: match", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: handle/perform", "[FAIL] Compiler: handle/perform", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 42) (let (f (lambda () x)) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg closure capture", "[FAIL] Compiler: zero-arg closure capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: id", "[FAIL] Compiler: id", pass, fail);
    }
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: range", "[FAIL] Compiler: range", pass, fail);
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(signal ui.open 1)",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified signal lowers directly",
            "[FAIL] Compiler: module-qualified signal lowers directly",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && str_contains(code, "aot::compiled_handle(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified handle lowers directly",
            "[FAIL] Compiler: module-qualified handle lowers directly",
            pass,
            fail
        );
    }
}

fn void run_compiler_group_type_dispatch_bridge_tests(Interp* interp, usz* pass, usz* fail) {
    // Type forms should lower to direct AOT helpers and still sync generated globals.
    {
        char[] code = compile_to_c3("(define [type] Box (^Integer value)) (define b (Box 1))", interp);
        bool ok = str_contains(code, "aot::define_type(")
            && str_contains(code, "Box = aot::lookup_var(\"Box\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] lowers directly and syncs constructor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] lowers directly and syncs constructor global"); }
    }

    // Union forms must lower directly and sync generated variant constructors into AOT globals.
    {
        char[] code = compile_to_c3("(define [union] Maybe None (Some Integer)) (Some 1)", interp);
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
        char[] code = compile_to_c3("(define (describe (^Integer x)) \"int\")", interp);
        bool ok = str_contains(code, "aot::define_typed_method(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define lowers through typed-method helper"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define lowers through typed-method helper"); }
    }

    // Typed define should compile captures normally rather than injecting globals into eval.
    {
        char[] code = compile_to_c3(
            "(define y 10) (define (describe (^Integer x)) (+ x y))",
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
            "(define (describe (^Integer x)) \"int\") (define (describe (^String x)) \"str\")",
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
            "(define (route (^Integer x)) (+ x 1)) "
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
            "(define (mix (^Integer x)) (+ x delta)) "
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
            "(define [type] Box (^Integer value)) "
            "(define (render (^Integer x)) (+ x 1)) "
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

fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))",
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import serializer preserves path and alias syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import serializer preserves path and alias syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (+ 1 (signal ask 0)) (ask x (resolve x)))",
            "(handle (+ 1 (perform ask 0)) (ask x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle serializer emits parser-compatible clause syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle serializer emits parser-compatible clause syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))",
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves clause parens"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves clause parens"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (cons 1 2) ((a . b) a) (_ 0))",
            "(match (cons 1 2) ((a . b) a) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves cons patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves cons patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Dictionary 'name 7) ({name} name) (_ 0))",
            "(match (Dictionary 'name 7) ({name} name) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves dict patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves dict patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 1) ((Some x) x) (None 0))",
            "(match (Some 1) ((Some x) x) (None 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves constructor patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves constructor patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(signal ui.open 1)",
            "(perform ui.open 1)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified signal tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified signal tag paths"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            "(handle (perform ui.open 1) (ui.open x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified handle tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified handle tag paths"); }
    }
}

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    run_compiler_group_serializer_roundtrip_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
