module lisp;

import std::collections::list;
import std::io;
import main;

fn void run_compiler_group_set_mutation_tests(Interp* interp, usz* pass, usz* fail) {
    // --- set! tests ---
    {
        char[] code = compile_to_c3("(define x 10) (set! x 20)", interp);
        bool ok = compiler_code_contains_all(code, {"x = ", "aot::make_void()"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: (set! x 20)", "[FAIL] Compiler: (set! x 20)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define x 10) (block (set! x 20) x)", interp);
        bool ok = compiler_code_contains_all(code, {"aot::make_void()"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: set! command form", "[FAIL] Compiler: set! command form", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define counter 0) (define inc! (lambda (n) (set! counter (+ counter n))))", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: set! inside lambda", "[FAIL] Compiler: set! inside lambda", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define arr (Array 1 2 3)) (set! arr 1 9)", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: set! generic collection form", "[FAIL] Compiler: set! generic collection form", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define hm (Dictionary 'k 1)) (set! hm 'k 9)", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: set! generic dictionary form", "[FAIL] Compiler: set! generic dictionary form", pass, fail);
    }
}

fn void run_compiler_group_dot_index_tests(Interp* interp, usz* pass, usz* fail) {
    // --- dot-bracket indexing tests ---
    {
        char[] code = compile_to_c3("(define lst (list 10 20 30)) lst.[1]", interp);
        bool ok = compiler_code_contains_all(code, {"aot::index"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: lst.[1]", "[FAIL] Compiler: lst.[1]", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define lst (list 1 2 3)) lst.[(+ 0 1)]", interp);
        bool ok = compiler_code_contains_all(code, {"aot::index"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: lst.[(+ 0 1)]", "[FAIL] Compiler: lst.[(+ 0 1)]", pass, fail);
    }
}

fn void run_compiler_group_path_notation_tests(Interp* interp, usz* pass, usz* fail) {
    // --- path notation tests ---
    {
        char[] code = compile_to_c3("(define point (list (cons 'x 10) (cons 'y 20))) point.x", interp);
        bool ok = compiler_code_contains_all(code, {"aot::field_access"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: point.x", "[FAIL] Compiler: point.x", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define obj (list (cons 'inner (list (cons 'val 42))))) obj.inner.val", interp);
        bool ok = compiler_code_contains_all(code, {"aot::field_access"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: obj.inner.val", "[FAIL] Compiler: obj.inner.val", pass, fail);
    }
    {
        char[] code = compile_to_c3("point.x", interp);
        bool ok = str_contains(code, "make_string");
        compiler_test_finish_bool(ok, "Compiler: path uses make_string for field name", "[FAIL] Compiler: path uses make_string for field name", pass, fail);
    }
}

fn void run_compiler_group_constructor_dispatch_shadowing_tests(Interp* interp, usz* pass, usz* fail) {
    {
        char[] code = compile_to_c3("(let (list (lambda (a b) a)) (list 1 2))", interp);
        bool ok = compiler_code_contains_all(code, {"aot::apply_multi"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: shadowed list uses call dispatch", "[FAIL] Compiler: shadowed list uses call dispatch", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (Dictionary (lambda (a b) a)) (Dictionary 1 2))", interp);
        bool ok = compiler_code_contains_all(code, {"aot::apply_multi"})
            && compiler_code_contains_none(code, {"aot::dict_from_args", "unsupported"});
        compiler_test_finish_bool(ok, "Compiler: shadowed Dictionary uses call dispatch", "[FAIL] Compiler: shadowed Dictionary uses call dispatch", pass, fail);
    }
    {
        char[] code = compile_to_c3("(((lambda (false) (lambda () false)) 9))", interp);
        bool ok = str_contains(code, "_capture_source_")
            && str_contains(code, "_omni_sym_false_")
            && str_contains(code, "captured__omni_sym_false_")
            && !str_contains(code, "captured__omni_sym_false_ = aot::make_false()")
            && !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: nested closure captures shadowed false", "[FAIL] Compiler: nested closure captures shadowed false", pass, fail);
    }
    {
        char[] code = compile_to_c3("(((lambda (+) (lambda (x) (+ x 2))) (lambda (a b) 88)) 1)", interp);
        bool ok = str_contains(code, "_capture_source_")
            && str_contains(code, "_omni_sym__plus__")
            && str_contains(code, "captured__omni_sym__plus__")
            && !str_contains(code, "captured__omni_sym__plus__ = _prim__omni_sym__plus__")
            && !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: nested closure captures shadowed callable primitive", "[FAIL] Compiler: nested closure captures shadowed callable primitive", pass, fail);
    }
}

fn void run_compiler_group_set_path_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_set_mutation_tests(interp, &pass, &fail);
    run_compiler_group_dot_index_tests(interp, &pass, &fail);
    run_compiler_group_path_notation_tests(interp, &pass, &fail);
    run_compiler_group_constructor_dispatch_shadowing_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_continuation_set_begin_tests(Interp* interp, usz* pass, usz* fail) {
    // --- checkpoint/capture via interpreter delegation ---
    {
        char[] code = compile_to_c3("(checkpoint (+ 1 (capture k (k 10))))", interp);
        bool ok = compiler_code_contains_all(code, {"compiled_reset"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: checkpoint/capture compiles natively", "[FAIL] Compiler: checkpoint/capture compiles natively", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define x 0) (block (set! x 1) (set! x (+ x 1)) x)", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: set! inside block", "[FAIL] Compiler: set! inside block", pass, fail);
    }
}

fn void run_compiler_group_continuation_form_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Continuation form tests (native compilation) ---
    {
        char[] code = compile_to_c3("(define x 5) (checkpoint (+ x (capture k (k 10))))", interp);
        bool ok = str_contains(code, "compiled_reset");
        compiler_test_finish_bool(ok, "Compiler: checkpoint uses compiled_reset", "[FAIL] Compiler: checkpoint uses compiled_reset", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = str_contains(code, "compiled_handle") && str_contains(code, "compiled_signal");
        compiler_test_finish_bool(ok, "Compiler: handle/signal uses native compilation", "[FAIL] Compiler: handle/signal uses native compilation", pass, fail);
    }
    {
        char[] code = compile_to_c3("(signal raise \"error\")", interp);
        bool ok = str_contains(code, "compiled_signal");
        compiler_test_finish_bool(ok, "Compiler: signal uses compiled_signal", "[FAIL] Compiler: signal uses compiled_signal", pass, fail);
    }
    {
        char[] code = compile_to_c3("(capture k 42)", interp);
        bool ok = str_contains(code, "compiled_shift");
        compiler_test_finish_bool(ok, "Compiler: capture uses compiled_shift", "[FAIL] Compiler: capture uses compiled_shift", pass, fail);
    }
    {
        char[] code = compile_to_c3("(checkpoint (+ 1 2))", interp);
        bool ok = str_contains(code, "compiled_reset");
        compiler_test_finish_bool(ok, "Compiler: checkpoint compiles natively", "[FAIL] Compiler: checkpoint compiles natively", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (block (signal get 0) (signal set 1)) (get x (resolve 42)) (set v (resolve v)))", interp);
        bool ok = str_contains(code, "compiled_handle");
        compiler_test_finish_bool(ok, "Compiler: handle with multiple clauses", "[FAIL] Compiler: handle with multiple clauses", pass, fail);
    }
    {
        char[] code = compile_to_c3("(try (lambda (xx) (+ 1 2)) (lambda (msg) 0))", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: try from stdlib", "[FAIL] Compiler: try from stdlib", pass, fail);
    }
    {
        char[] code = compile_to_c3("(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))", interp);
        bool ok = str_contains(code, "compiled_reset") && str_contains(code, "compiled_shift");
        compiler_test_finish_bool(ok, "Compiler: checkpoint with nested capture", "[FAIL] Compiler: checkpoint with nested capture", pass, fail);
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
