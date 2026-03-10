module lisp;


import std::io;
import main;
fn void run_compiler_group_jit_collection_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 7: JIT infrastructure tests (via lisp::run)
    // =========================================================================

    // 68. map correctness via JIT
    {
        EvalResult r = run("(map (lambda (x) (+ x 1)) (list 1 2 3))", interp);
        bool ok = false;
        if (!r.error.has_error && r.value != null) {
            if (r.value.tag == ValueTag.CONS) {
                Value* car = r.value.cons_val.car;
                ok = (car.tag == ValueTag.INT && car.int_val == 2);
            }
        }
        if (ok) { (*pass)++; compiler_print_pass("JIT: map correctness"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: map correctness"); }
    }

    // 69. filter correctness via JIT
    {
        EvalResult r = run("(filter (lambda (x) (> x 2)) (list 1 2 3 4))", interp);
        bool ok = false;
        if (!r.error.has_error && r.value != null && r.value.tag == ValueTag.CONS) {
            ok = (r.value.cons_val.car.tag == ValueTag.INT && r.value.cons_val.car.int_val == 3);
        }
        if (ok) { (*pass)++; compiler_print_pass("JIT: filter correctness"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: filter correctness"); }
    }

    // 70. TCO depth via JIT (named-let loop to 5000)
    {
        EvalResult r = run("(let loop (n 5000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", interp);
        bool ok = (!r.error.has_error && r.value != null && r.value.tag == ValueTag.INT && r.value.int_val == 5000);
        if (ok) { (*pass)++; compiler_print_pass("JIT: TCO depth 5000"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: TCO depth 5000"); }
    }
}

fn void run_compiler_group_jit_language_tests(Interp* interp, usz* pass, usz* fail) {
    // 71. multi-arg via JIT
    {
        EvalResult r = run("((lambda (x y z) (+ x (+ y z))) 1 2 3)", interp);
        bool ok = (!r.error.has_error && r.value != null && r.value.tag == ValueTag.INT && r.value.int_val == 6);
        if (ok) { (*pass)++; compiler_print_pass("JIT: multi-arg call"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: multi-arg call"); }
    }

    // 72. mutable capture via JIT (using block + define for set!)
    {
        EvalResult r = run("(block (define x 0) (set! x (+ x 1)) (set! x (+ x 1)) x)", interp);
        bool ok = (!r.error.has_error && r.value != null && r.value.tag == ValueTag.INT && r.value.int_val == 2);
        if (ok) { (*pass)++; compiler_print_pass("JIT: mutable capture"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: mutable capture"); }
    }

    // 73. macro via JIT
    {
        EvalResult r = run("(block (define [macro] double ([x] (+ x x))) (double 21))", interp);
        bool ok = (!r.error.has_error && r.value != null && r.value.tag == ValueTag.INT && r.value.int_val == 42);
        if (ok) { (*pass)++; compiler_print_pass("JIT: macro expansion"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: macro expansion"); }
    }
}

fn void run_compiler_group_jit_runtime_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_jit_collection_tests(interp, &pass, &fail);
    run_compiler_group_jit_language_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_print_codegen_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 8: print-mode codegen tests (via compile_to_c3_ext)
    // =========================================================================

    // 74. print_last works for literals
    {
        char[] code = compile_to_c3_ext("42", interp, true, false);
        bool ok = str_contains(code, "aot::print_value");
        if (ok) { pass++; compiler_print_pass("Compiler: print_last literal"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last literal"); }
    }

    // 75. print_last works for calls
    {
        char[] code = compile_to_c3_ext("(+ 1 2)", interp, true, false);
        bool ok = str_contains(code, "aot::print_value");
        if (ok) { pass++; compiler_print_pass("Compiler: print_last call"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last call"); }
    }

    // 76. print_last skips defines, wraps last non-define
    {
        char[] code = compile_to_c3_ext("(define x 1) x", interp, true, false);
        bool ok = str_contains(code, "aot::print_value");
        if (ok) { pass++; compiler_print_pass("Compiler: print_last skips defines"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last skips defines"); }
    }

    // 77. default case warning text present
    {
        char[] code = compile_to_c3("42", interp);
        // The default case emits "WARNING" in comment — but only for unsupported exprs.
        // We verify the constant string is in the source by checking the STDLIB compiles without it.
        bool ok = !str_contains(code, "WARNING");
        if (ok) { pass++; compiler_print_pass("Compiler: no WARNING in normal compilation"); }
        else    { fail++; io::printn("[FAIL] Compiler: no WARNING in normal compilation"); }
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_jit_print_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_jit_runtime_tests(interp, pass_out, fail_out);
    run_compiler_group_print_codegen_tests(interp, pass_out, fail_out);
}

/**
 * Compiler tests — verify that the compiler produces valid C3 output
 * for all supported features. Tests check that compile_to_c3() succeeds
 * and the output does not contain "unsupported".
 */
