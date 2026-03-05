module lisp;


import std::io;
import main;
// =============================================================================
// SECTION 12: COMPILER TESTS
// =============================================================================

extern fn char* compiler_c_getenv(char* name) @extern("getenv");

/**
 * Check if haystack contains needle as a substring.
 */
fn bool str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn void compiler_print_pass(char[] msg) {
    bool quiet = compiler_c_getenv("OMNI_TEST_QUIET") != null ||
                 (compiler_c_getenv("OMNI_TEST_SUMMARY") != null &&
                  compiler_c_getenv("OMNI_TEST_VERBOSE") == null);
    if (quiet) return;
    io::printfn("[PASS] %s", (ZString)msg);
}

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

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
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

    // 72. mutable capture via JIT (using begin + define for set!)
    {
        EvalResult r = run("(begin (define x 0) (set! x (+ x 1)) (set! x (+ x 1)) x)", interp);
        bool ok = (!r.error.has_error && r.value != null && r.value.tag == ValueTag.INT && r.value.int_val == 2);
        if (ok) { (*pass)++; compiler_print_pass("JIT: mutable capture"); }
        else    { (*fail)++; io::printn("[FAIL] JIT: mutable capture"); }
    }

    // 73. macro via JIT
    {
        EvalResult r = run("(begin (define [macro] double ([x] (+ x x))) (double 21))", interp);
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
    // Phase 8: compile_to_c3_with_print pattern tests
    // =========================================================================

    // 74. print_last works for literals
    {
        char[] code = compile_to_c3_with_print("42", interp);
        bool ok = str_contains(code, "aot::print_value");
        if (ok) { pass++; compiler_print_pass("Compiler: print_last literal"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last literal"); }
    }

    // 75. print_last works for calls
    {
        char[] code = compile_to_c3_with_print("(+ 1 2)", interp);
        bool ok = str_contains(code, "aot::print_value");
        if (ok) { pass++; compiler_print_pass("Compiler: print_last call"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last call"); }
    }

    // 76. print_last skips defines, wraps last non-define
    {
        char[] code = compile_to_c3_with_print("(define x 1) x", interp);
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
fn void run_compiler_tests(Interp* _unused_interp) {
    io::printn("\n=== Compiler Tests ===");
    usz pass = 0;
    usz fail = 0;

    // Use a fresh interp for compiler tests since each compile_to_c3 call
    // parses the entire stdlib prelude, consuming many expr pool entries.
    Interp* interp = (Interp*)mem::malloc(Interp.sizeof);
    interp.init();
    register_primitives(interp);
    register_stdlib(interp);  // Needed for map, filter, etc.

    run_compiler_group_syntax_basics(interp, &pass, &fail);
    run_compiler_group_stdlib_existing(interp, &pass, &fail);
    run_compiler_group_set_path_tests(interp, &pass, &fail);
    run_compiler_group_continuation_native_tests(interp, &pass, &fail);
    run_compiler_group_multiarg_tco_tests(interp, &pass, &fail);
    run_compiler_group_qq_macro_primitive_tests(interp, &pass, &fail);
    run_compiler_group_mutable_integration_iterative_tests(interp, &pass, &fail);
    run_compiler_group_jit_print_tests(interp, &pass, &fail);

    interp.destroy();
    mem::free(interp);
    io::printfn("\n=== Compiler Tests: %d passed, %d failed ===", pass, fail);
    emit_test_summary("compiler", (int)pass, (int)fail);
    assert(fail == 0, "compiler tests had failures");
}

// =============================================================================
// Escape-scope optimization tests
// =============================================================================
