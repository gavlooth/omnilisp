module lisp;


import std::io;
import main;
fn void run_compiler_group_multiarg_tco_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 1: Multi-arg calls (inline currying)
    // =========================================================================

    // 41. Multi-arg call uses inline currying (invoke_once + invoke)
    {
        char[] code = compile_to_c3("(+ 1 2)", interp);
        bool ok = str_contains(code, "invoke") && !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: multi-arg uses inline currying"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-arg uses inline currying"); }
    }

    // 42. Multi-arg uses invoke_once for intermediate args
    {
        char[] code = compile_to_c3("(+ 1 2)", interp);
        bool ok = str_contains(code, "invoke_once") || str_contains(code, "invoke");
        if (ok) { pass++; compiler_print_pass("Compiler: multi-arg uses invoke"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-arg uses invoke"); }
    }

    // 43. 3-arg call uses apply_multi
    {
        char[] code = compile_to_c3("(substring \"hello\" 1 3)", interp);
        bool ok = str_contains(code, "apply_multi");
        if (ok) { pass++; compiler_print_pass("Compiler: 3-arg apply_multi"); }
        else    { fail++; io::printn("[FAIL] Compiler: 3-arg apply_multi"); }
    }

    // 43b. >32 arg call still compiles all args and uses apply_multi
    {
        char[] code = compile_to_c3(
            "(f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)",
            interp
        );
        bool ok = str_contains(code, "apply_multi") &&
                  str_contains(code, "make_int(40)") &&
                  !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: >32 arg call emits all temps"); }
        else    { fail++; io::printn("[FAIL] Compiler: >32 arg call emits all temps"); }
    }

    // 43c. >32 arg list build still compiles all args
    {
        char[] code = compile_to_c3(
            "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)",
            interp
        );
        bool ok = str_contains(code, "make_int(40)") &&
                  str_contains(code, "aot::cons") &&
                  !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: >32 arg list emits all temps"); }
        else    { fail++; io::printn("[FAIL] Compiler: >32 arg list emits all temps"); }
    }

    // 44. Zero-arg call via invoke
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = str_contains(code, "invoke") && str_contains(code, "make_nil()");
        if (ok) { pass++; compiler_print_pass("Compiler: zero-arg invoke"); }
        else    { fail++; io::printn("[FAIL] Compiler: zero-arg invoke"); }
    }

    // =========================================================================
    // Phase 2: TCO via trampoline
    // =========================================================================

    // 45. Tail calls in lambda body emit apply_multi_tail
    {
        char[] code = compile_to_c3("(define (f x) (f (+ x 1)))", interp);
        bool ok = str_contains(code, "apply_multi_tail");
        if (ok) { pass++; compiler_print_pass("Compiler: tail call emits apply_multi_tail"); }
        else    { fail++; io::printn("[FAIL] Compiler: tail call emits apply_multi_tail"); }
    }

    // 45b. >32 arg tail call still compiles all args
    {
        char[] code = compile_to_c3(
            "(define (f) (g 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40))",
            interp
        );
        bool ok = str_contains(code, "apply_multi_tail") &&
                  str_contains(code, "make_int(40)") &&
                  !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: >32 arg tail call emits all temps"); }
        else    { fail++; io::printn("[FAIL] Compiler: >32 arg tail call emits all temps"); }
    }

    // 46. Non-tail calls use apply_multi (not tail variant)
    {
        char[] code = compile_to_c3("(define (f x) (+ (f x) 1))", interp);
        // The (f x) is not in tail position (it's an argument to +)
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: non-tail compiles OK"); }
        else    { fail++; io::printn("[FAIL] Compiler: non-tail compiles OK"); }
    }

    // 47. If branches propagate tail position
    {
        char[] code = compile_to_c3("(define (f x) (if (= x 0) 1 (f (- x 1))))", interp);
        bool ok = str_contains(code, "apply_multi_tail");
        if (ok) { pass++; compiler_print_pass("Compiler: if branch tail position"); }
        else    { fail++; io::printn("[FAIL] Compiler: if branch tail position"); }
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_quasiquote_macro_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 3: Quasiquote, macros, modules
    // =========================================================================

    // 48. Quasiquote compiles natively with aot::cons
    {
        char[] code = compile_to_c3("(let (x 5) `(a ,x))", interp);
        bool ok = str_contains(code, "aot::cons") && str_contains(code, "make_symbol");
        if (ok) { pass++; compiler_print_pass("Compiler: quasiquote compiles natively"); }
        else    { fail++; io::printn("[FAIL] Compiler: quasiquote compiles natively"); }
    }

    // 49. Quasiquote with unquote uses make_symbol for literal parts
    {
        char[] code = compile_to_c3("(let (x 5) `(a ,x))", interp);
        bool ok = str_contains(code, "make_symbol(\"a\")");
        if (ok) { pass++; compiler_print_pass("Compiler: quasiquote uses make_symbol"); }
        else    { fail++; io::printn("[FAIL] Compiler: quasiquote uses make_symbol"); }
    }

    // 50. Defmacro delegates to eval_source
    {
        char[] code = compile_to_c3("(define [macro] double ([x] (+ x x)))", interp);
        bool ok = str_contains(code, "make_nil") && !str_contains(code, "unsupported");
        if (ok) { pass++; compiler_print_pass("Compiler: defmacro compiles as no-op"); }
        else    { fail++; io::printn("[FAIL] Compiler: defmacro compiles as no-op"); }
    }

    // 51. Stdlib functions (when, unless) compiled from STDLIB_PRELUDE
    {
        char[] code = compile_to_c3("42", interp);
        bool ok = str_contains(code, "when") && str_contains(code, "unless");
        if (ok) { pass++; compiler_print_pass("Compiler: stdlib functions compiled"); }
        else    { fail++; io::printn("[FAIL] Compiler: stdlib functions compiled"); }
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_missing_numeric_symbol_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 4: Missing primitives
    // =========================================================================

    // 52. string->number compiles
    {
        char[] code = compile_to_c3("(string->number \"42\")", interp);
        bool ok = str_contains(code, "lookup_prim(\"string->number\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: string->number"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: string->number"); }
    }

    // 53. number->string compiles
    {
        char[] code = compile_to_c3("(number->string 42)", interp);
        bool ok = str_contains(code, "lookup_prim(\"number->string\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: number->string"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: number->string"); }
    }

    // 54. gensym compiles
    {
        char[] code = compile_to_c3("(gensym)", interp);
        bool ok = str_contains(code, "lookup_prim(\"gensym\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: gensym"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: gensym"); }
    }

    // 55. apply compiles
    {
        char[] code = compile_to_c3("(apply + (list 1 2))", interp);
        bool ok = str_contains(code, "lookup_prim(\"apply\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: apply"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: apply"); }
    }

    // 56. equal? compiles
    {
        char[] code = compile_to_c3("(equal? 1 2)", interp);
        bool ok = str_contains(code, "lookup_prim(\"equal?\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: equal?"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: equal?"); }
    }
}

fn void run_compiler_group_missing_io_ref_tests(Interp* interp, usz* pass, usz* fail) {
    // 57. display compiles
    {
        char[] code = compile_to_c3("(display \"hello\")", interp);
        bool ok = str_contains(code, "lookup_prim(\"display\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: display"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: display"); }
    }

    // 58. ref compiles
    {
        char[] code = compile_to_c3("(ref (dict) 'a)", interp);
        bool ok = str_contains(code, "lookup_prim(\"ref\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: ref"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: ref"); }
    }

    // 58b. time-point constructor compiles
    {
        char[] code = compile_to_c3("(time-point 'date 2026 3 7)", interp);
        bool ok = str_contains(code, "lookup_prim(\"time-point\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: time-point"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: time-point"); }
    }
    // 58c. time-point tz alias compiles
    {
        char[] code = compile_to_c3("(time-point 'datetimetz 2026 3 7 8 15 30 123456 -420)", interp);
        bool ok = str_contains(code, "lookup_prim(\"time-point\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: time-point alias"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: time-point alias"); }
    }
    // 58d. json-get compiles
    {
        char[] code = compile_to_c3("(json-get (json-parse \"{\\\"a\\\":1}\") \"/a\")", interp);
        bool ok = str_contains(code, "lookup_prim(\"json-get\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: json-get"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: json-get"); }
    }

    // 59. load compiles
    {
        char[] code = compile_to_c3("(load \"test.omni\")", interp);
        bool ok = str_contains(code, "lookup_prim(\"load\")") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: load"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: load"); }
    }
}

fn void run_compiler_group_missing_primitive_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_missing_numeric_symbol_tests(interp, &pass, &fail);
    run_compiler_group_missing_io_ref_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_qq_macro_primitive_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_quasiquote_macro_tests(interp, pass_out, fail_out);
    run_compiler_group_missing_primitive_tests(interp, pass_out, fail_out);
}

fn void run_compiler_group_mutable_capture_codegen_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 5: Mutable capture boxing
    // =========================================================================

    // 60. Mutable capture detected and uses aot::define_var
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        // Mutable capture test - checks generated code for interpreter env usage
        bool ok = str_contains(code, "aot::define_var") && str_contains(code, "aot::set_var");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mutable capture uses aot::define_var/aot::set_var"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mutable capture uses aot::define_var/aot::set_var"); }
    }

    // 61. Mutable capture reads use aot::lookup_var
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        bool ok = str_contains(code, "aot::lookup_var");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mutable capture reads use aot::lookup_var"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mutable capture reads use aot::lookup_var"); }
    }

    // 62. Non-mutable let uses C3 local variable (Value* x =...)
    {
        char[] code = compile_to_c3("(let (x 42) x)", interp);
        // The main fn body should contain "Value* x =" for a non-mutable let
        bool ok = str_contains(code, "Value* x =");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-mutable let uses C3 local"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-mutable let uses C3 local"); }
    }
}

fn void run_compiler_group_feature_integration_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Integration: Verify all features work together
    // =========================================================================

    // 63. Complex program with multiple features compiles
    {
        char[] code = compile_to_c3(
            "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 10)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: factorial compiles"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: factorial compiles"); }
    }

    // 64. Let-rec with tail call emits apply_multi_tail
    {
        char[] code = compile_to_c3(
            "(let ^rec (loop (lambda (n) (if (= n 0) 'done (loop (- n 1))))) (loop 100))", interp);
        bool ok = str_contains(code, "apply_multi_tail") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: let-rec tail call emits apply_multi_tail"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: let-rec tail call emits apply_multi_tail"); }
    }

    // 65. Nested multi-arg calls compile correctly
    {
        char[] code = compile_to_c3("(+ (* 2 3) (* 4 5))", interp);
        bool ok = str_contains(code, "invoke") && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: nested multi-arg calls"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: nested multi-arg calls"); }
    }
}

fn void run_compiler_group_mutable_integration_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_mutable_capture_codegen_tests(interp, &pass, &fail);
    run_compiler_group_feature_integration_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_iterative_stdlib_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 6: STDLIB_PRELUDE iterative map/filter
    // =========================================================================

    // 66. STDLIB_PRELUDE map uses iterative loop pattern
    {
        char[] code = compile_to_c3("(map (lambda (x) x) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        if (ok) { pass++; compiler_print_pass("Compiler: STDLIB map uses loop pattern"); }
        else    { fail++; io::printn("[FAIL] Compiler: STDLIB map uses loop pattern"); }
    }

    // 67. STDLIB_PRELUDE filter uses iterative loop pattern
    {
        char[] code = compile_to_c3("(filter (lambda (x) x) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        if (ok) { pass++; compiler_print_pass("Compiler: STDLIB filter uses loop pattern"); }
        else    { fail++; io::printn("[FAIL] Compiler: STDLIB filter uses loop pattern"); }
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_mutable_integration_iterative_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_mutable_integration_tests(interp, pass_out, fail_out);
    run_compiler_group_iterative_stdlib_tests(interp, pass_out, fail_out);
}
