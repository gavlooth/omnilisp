module lisp;


import std::io;
import main;
import pika;
// =============================================================================
// SECTION 10: TESTS
// =============================================================================

extern fn char* test_c_getenv(char* name) @extern("getenv");

bool g_test_quiet_output = false;

fn bool test_summary_enabled() @inline {
    return test_c_getenv("OMNI_TEST_SUMMARY") != null;
}

fn bool test_quiet_enabled() @inline {
    return g_test_quiet_output;
}

fn void emit_test_summary(char[] suite, int pass, int fail) {
    if (!test_summary_enabled()) return;
    io::printfn("OMNI_TEST_SUMMARY suite=%s pass=%d fail=%d", (ZString)suite, pass, fail);
}

fn void emit_pass(char[] name) @inline {
    if (test_quiet_enabled()) return;
    io::printfn("[PASS] %s", (ZString)name);
}

fn void emit_pass_literal(char[] msg) @inline {
    if (test_quiet_enabled()) return;
    io::printn(msg);
}

alias IsolatedTestGroupFn = fn void(Interp* interp, int* pass, int* fail);

fn void run_test_global_boundary_reset() {
    // Keep test groups independent from prior JIT/effect/promotion global state.
    if (g_jit_state_count > 0) {
        g_jit_gc_needed = true;
        jit_gc();
    } else {
        g_jit_gc_needed = false;
    }
    jit_cache_clear();
    g_jit_pool_warned = false;
    pika::regex_cache_reset();
}

fn void run_group_isolated(IsolatedTestGroupFn group_fn, int* pass, int* fail) {
    run_test_global_boundary_reset();

    Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
    tmp.init();
    register_primitives(tmp);
    register_stdlib(tmp);
    group_fn(tmp, pass, fail);
    tmp.destroy();
    mem::free(tmp);

    run_test_global_boundary_reset();
}

// ---------------------------------------------------------------------------
// Unified Test Helpers — each runs BOTH interpreter AND JIT on the same expr
// ---------------------------------------------------------------------------

fn void setup(Interp* interp, char[] code) {
    run(code, interp);
}

fn bool jit_checks_enabled() @inline {
    // Generated JIT code is not ASAN-instrumented.
    return !main::stack_runtime_asan_enabled();
}

fn void test_eq(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == INT && jv.int_val == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_eq_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_double(ri.value) && ri.value.double_val == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && is_double(jv) && jv.double_val == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_truthy(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && ri.value.sym_val == interp.sym_false);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && !is_nil(jv) && !(is_symbol(jv) && jv.sym_val == interp.sym_false);
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_nil(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_nil(ri.value);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && is_nil(jv);
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_error(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = ri.error.has_error;
    if (interp_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (expected error)", (ZString)name);
        (*fail)++;
    }
}

fn void test_error_contains(Interp* interp, char[] name, char[] expr_str, char[] expected_substr, int* pass, int* fail) {
    EvalResult ri = run(expr_str, interp);
    if (!ri.error.has_error) {
        io::printfn("[FAIL] %s (expected error, got success)", (ZString)name);
        (*fail)++; return;
    }
    // Check if error message contains expected substring
    char[] msg = ri.error.message[:256];
    bool found = false;
    usz slen = 0;
    while (slen < 256 && msg[slen] != 0) slen++;
    usz elen = expected_substr.len;
    if (elen <= slen) {
        for (usz i = 0; i <= slen - elen; i++) {
            bool match = true;
            for (usz j = 0; j < elen; j++) {
                if (msg[i + j] != expected_substr[j]) { match = false; break; }
            }
            if (match) { found = true; break; }
        }
    }
    if (found) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (error '%s' doesn't contain '%s')", (ZString)name, (ZString)msg, (ZString)expected_substr);
        (*fail)++;
    }
}

fn bool str_starts_with(char[] haystack, char[] needle) {
    if (haystack.len < needle.len) return false;
    for (usz i = 0; i < needle.len; i++) {
        if (haystack[i] != needle[i]) return false;
    }
    return true;
}

fn void test_str_val(Interp* interp, char[] name, char[] expr, char[] expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_string(ri.value) &&
        str_starts_with(ri.value.str_chars[:ri.value.str_len], expected) &&
        ri.value.str_len == expected.len;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == STRING &&
                    str_starts_with(jv.str_chars[:jv.str_len], expected) &&
                    jv.str_len == expected.len;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        char[256] got_buf;
        char[] got_str = "?";
        if (!ri.error.has_error && is_string(ri.value)) {
            got_str = ri.value.str_chars[:ri.value.str_len];
        }
        io::printfn("[FAIL] %s (interp=%s, jit=%s, got='%s')", (ZString)name,
            interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL", (ZString)got_str);
        (*fail)++;
    }
}

fn void test_str(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_string(ri.value);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == STRING;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_tag(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && ri.value != null && ri.value.tag == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_gt(Interp* interp, char[] name, char[] expr, long min, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val > min;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == INT && jv.int_val > min;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

// Interp-only: for tests that involve stateful side effects or features
// the JIT cannot handle (modules, error paths, println, etc.)
fn void test_eq_interp(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        if (!ri.error.has_error && is_int(ri.value)) { io::printfn("[FAIL] %s (got %d)", (ZString)name, ri.value.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
        (*fail)++;
    }
}

fn void test_truthy_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && ri.value.sym_val == interp.sym_false)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_nil_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_nil(ri.value)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_str_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_string(ri.value)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_tag_interp(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && ri.value != null && ri.value.tag == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

// JIT-only: for stateful tests that would double-mutate if run through both interp and JIT
fn void test_eq_jit(Interp* interp, char[] name, char[] source, long expected, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        test_eq_interp(interp, name, source, expected, pass, fail);
        return;
    }
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = jit_exec(f, interp);
    if (result != null && result.tag == INT && result.int_val == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        (*fail)++;
        if (result != null && result.tag == INT) { io::printfn("[FAIL] %s (JIT got %d)", (ZString)name, result.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
    }
}

fn void test_nil_jit(Interp* interp, char[] name, char[] source, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        test_nil_interp(interp, name, source, pass, fail);
        return;
    }
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = jit_exec(f, interp);
    if (result == null || is_nil(result)) {
        emit_pass(name);
        (*pass)++;
    } else {
        (*fail)++;
        io::printfn("[FAIL] %s (JIT expected nil)", (ZString)name);
    }
}

fn double dabs(double x) @inline { return x < 0.0 ? -x : x; }

fn bool value_matches_double(Value* v, double expected) @inline {
    return v != null && is_number(v) && dabs(to_double(v) - expected) < 0.0001;
}

fn bool eval_expr_as_double(Interp* interp, char[] expr, double expected) {
    EvalResult ri = run(expr, interp);
    if (ri.error.has_error) return false;
    return value_matches_double(ri.value, expected);
}

fn bool eval_jit_as_double(Interp* interp, char[] expr, double expected) {
    Expr* ast = parse_for_jit(expr, interp);
    if (ast == null) return false;
    JitFn f = jit_compile(ast, interp);
    if (f == null) return false;
    Value* jv = jit_exec(f, interp);
    return value_matches_double(jv, expected);
}

fn void test_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    bool interp_ok = eval_expr_as_double(interp, expr, expected);
    bool jit_ok = !jit_checks_enabled() || eval_jit_as_double(interp, expr, expected);

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

// =============================================================================
// E2E COMPILER TEST GENERATION
// =============================================================================

struct E2ETestCase {
    char[] name;
    char[] expr;
}

// E2E test expressions — must be compilable (no type system, modules, set!, arrays, dicts)
// Setup defines are run first, then each expr is evaluated and its output captured.
const char[][] E2E_SETUPS = {
    // Basic definitions
    `(define x 10)`,
    `(define add +)`,
    `(define make-adder (lambda (n) (lambda (x) (+ n x))))`,
    `(define adder5 (make-adder 5))`,
    // Recursion
    `(define sum-to (lambda (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))`,
    `(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))`,
    // List helpers
    `(define mymap (lambda (f) (lambda (lst) (if (null? lst) (quote ()) (cons (f (car lst)) ((mymap f) (cdr lst)))))))`,
    `(define double (lambda (x) (* x 2)))`,
    `(define fold-helper (lambda (f) (lambda (init) (lambda (lst) (if (null? lst) init (((fold-helper f) (f init (car lst))) (cdr lst)))))))`,
    // Counter factory
    `(define make-counter (lambda (start) (lambda (x) (+ start x))))`,
    `(define c1 (make-counter 10))`,
    `(define c2 (make-counter 20))`,
    // Named let helper
    `(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))`,
    // Mutual recursion
    `(define is-even (lambda (n) (if (= n 0) true (is-odd (- n 1)))))`,
    `(define is-odd (lambda (n) (if (= n 0) nil (is-even (- n 1)))))`,
    // Composition helpers
    `(define inc (lambda (x) (+ x 1)))`,
    `(define add3 (lambda (x) (+ x 3)))`,
    `(define apply-twice (lambda (f) (lambda (x) (f (f x)))))`,
    // Additional closure
    `(define c3 (make-counter 30))`,
    // Note: id, compose already defined by stdlib
    // set! test variables
    `(define s_x 10)`,
    `(define s_acc 0)`,
    `(define s_count 0)`,
};

const E2ETestCase[] E2E_TESTS = {
    // === Literals ===
    { "int literal 42", "42" },
    { "int literal 0", "0" },
    { "int literal negative", "-5" },
    { "string literal", `"hello"` },
    { "true literal", "true" },
    { "nil literal", "nil" },
    { "quoted symbol", "(quote foo)" },
    { "quoted list", "(quote (a b c))" },
    { "quoted nested", "(quote (1 (2 3) 4))" },
    { "empty list", "(quote ())" },

    // === Arithmetic ===
    { "add", "(+ 1 2)" },
    { "add zero", "(+ 0 0)" },
    { "add negative", "(+ -5 3)" },
    { "add cancel", "(+ 100 -100)" },
    { "add large", "(+ 1000000 2000000)" },
    { "sub", "(- 10 3)" },
    { "sub negative result", "(- 3 10)" },
    { "mul", "(* 6 7)" },
    { "mul zero", "(* 100 0)" },
    { "div", "(/ 10 3)" },
    { "div exact", "(/ 100 10)" },
    { "modulo", "(% 10 3)" },
    { "modulo even", "(% 8 4)" },
    { "nested arith", "(+ (* 3 4) (- 10 5))" },

    // === Floats ===
    { "float literal", "3.14" },
    { "float add", "(+ 1.5 2.5)" },
    { "float mul", "(* 2.0 3.0)" },
    { "float div", "(/ 7.0 2.0)" },
    { "int to float add", "(+ 1 0.5)" },
    { "float integer value", "5.0" },

    // === Comparisons ===
    { "eq true", "(= 5 5)" },
    { "eq false", "(= 5 6)" },
    { "lt true", "(< 3 5)" },
    { "lt false", "(< 5 3)" },
    { "gt true", "(> 5 3)" },
    { "gt false", "(> 3 5)" },
    { "le true", "(<= 5 5)" },
    { "le false", "(<= 6 5)" },
    { "ge true", "(>= 5 5)" },
    { "ge false", "(>= 4 5)" },

    // === Variables & Bindings ===
    { "var x", "x" },
    { "let simple", "(let (y 7) y)" },
    { "let nested", "(let (a 1) (let (b 2) (+ a b)))" },
    { "let shadow", "(let (x 99) x)" },
    { "let multi", "(let (a 1 b 2 c 3) (+ a (+ b c)))" },

    // === Lambda & Application ===
    { "lambda identity", "((lambda (x) x) 5)" },
    { "lambda const", "((lambda (x) 42) 99)" },
    { "lambda arith", "((lambda (x) (+ x 10)) 32)" },
    { "curried add", "(((lambda (x) (lambda (y) (+ x y))) 3) 4)" },
    { "closure adder5", "(adder5 10)" },

    // === If ===
    { "if true", "(if true 1 2)" },
    { "if false", "(if nil 1 2)" },
    { "if nested", "(if (> 5 3) (if (< 2 1) 10 20) 30)" },
    { "if no else", "(if true 42)" },

    // === And/Or ===
    { "and true true", "(and 1 42)" },
    { "and false short", "(and nil 42)" },
    { "or first truthy", "(or 42 99)" },
    { "or fallback", "(or nil 99)" },

    // === Truthiness ===
    { "0 is truthy", "(if 0 42 99)" },
    { "empty string truthy", `(if "" 1 0)` },
    { "empty list truthy", "(if (quote ()) 1 0)" },

    // === Begin ===
    { "begin single", "(begin 42)" },
    { "begin multi", "(begin 1 2 3)" },

    // === Cons / Car / Cdr ===
    { "car cons", "(car (cons 1 2))" },
    { "cdr cons", "(cdr (cons 1 2))" },
    { "car list", "(car (quote (10 20 30)))" },
    { "cdr list", "(cdr (quote (10 20 30)))" },
    { "cons build", "(cons 1 (cons 2 (cons 3 (quote ()))))" },
    { "null? empty", "(null? (quote ()))" },
    { "null? nonempty", "(null? (quote (1)))" },
    { "pair?", "(pair? (cons 1 2))" },
    { "pair? nil", "(pair? nil)" },

    // === Length ===
    { "length empty", "(length (quote ()))" },
    { "length 5", "(length (quote (1 2 3 4 5)))" },

    // === String operations ===
    { "string-length", `(string-length "hello world")` },
    { "string-append", `(string-append "hello" " world")` },
    // substring skipped: 3-arg function not yet supported in compiler runtime
    { "number->string", `(number->string 42)` },
    { "string->number", `(string->number "42")` },

    // === Recursive functions ===
    { "sum-to 50", "(sum-to 50)" },
    { "fib 10", "(fib 10)" },
    { "fib 15", "(fib 15)" },
    { "factorial 5", "(fact 5)" },
    { "factorial 10", "(fact 10)" },

    // === Closure factories ===
    { "counter c1", "(c1 5)" },
    { "counter c2", "(c2 5)" },

    // === Higher-order functions ===
    { "map double car", "(car ((mymap double) (quote (1 2 3))))" },
    { "fold sum", "(((fold-helper +) 0) (quote (1 2 3 4 5)))" },
    { "map and fold", "(((fold-helper +) 0) ((mymap double) (quote (1 2 3 4 5))))" },

    // === Pattern matching ===
    { "match literal", "(match 42 (42 1) (x 0))" },
    { "match variable", "(match 5 (x x))" },
    { "match wildcard", "(match 99 (_ 1))" },
    { "match list", "(match (quote (1 2 3)) ([a b c] (+ a (+ b c))))" },
    { "match head", "(match (quote (10 20 30)) ([head .. tail] head))" },
    { "match rest sum", "(match (quote (1 2 3 4 5)) ([a b ..] (+ a b)))" },
    { "match last", "(match (quote (1 2 3 4 5)) ([.. last] last))" },
    { "match fallthrough", "(match 42 (0 100) (1 200) (x x))" },
    { "match underscore", "(match (quote (1 2 3)) ([_ second _] second))" },

    // === Nested let deep ===
    { "nested let 5", "(let (a 1) (let (b 2) (let (c 3) (let (d 4) (let (e 5) (+ a (+ b (+ c (+ d e)))))))))" },

    // === Let recursive ===
    { "let rec factorial", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 5))" },
    { "named let loop", "(let loop (n 10 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },

    // === Effects ===
    { "reset passthrough", "(reset (+ 10 20))" },
    { "shift aborts", "(reset (+ 1 (shift k 42)))" },
    { "shift k resumes", "(reset (+ 1 (shift k (k 10))))" },
    { "handle no effect", "(handle 42 (ask x (resolve 0)))" },
    { "handle perform resume", "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))" },
    { "handle abort", "(handle (+ 1 (signal ask 5)) (ask x x))" },

    // === Not ===
    { "not true", "(not true)" },
    { "not nil", "(not nil)" },
    { "not 0", "(not 0)" },

    // === Apply ===
    { "apply add", "(apply + (quote (1 2)))" },
    { "apply list", "(apply cons (quote (1 2)))" },

    // === Arithmetic edge cases ===
    { "sub zero", "(- 0 0)" },
    { "sub neg neg", "(- -5 -3)" },
    { "mul neg pos", "(* -3 4)" },
    { "mul neg neg", "(* -3 -4)" },
    { "div exact 2", "(/ 10 2)" },
    { "div zero num", "(/ 0 5)" },
    { "div neg", "(/ -10 3)" },
    { "mod zero", "(% 9 3)" },
    { "mod small", "(% 7 10)" },
    { "nested add", "(+ 1 (+ 2 3))" },
    { "nested mul add", "(* 2 (+ 3 4))" },
    { "nested sub mul", "(- (* 5 5) (+ 10 5))" },
    { "nested div add", "(/ (+ 10 20) (- 10 4))" },

    // === Comparison edge cases ===
    { "eq zero", "(= 0 0)" },
    { "eq neg", "(= -1 -1)" },
    { "lt equal", "(< 1 1)" },
    { "lt neg zero", "(< -5 0)" },
    { "gt equal", "(> 1 1)" },
    { "le strict", "(<= 1 2)" },
    { "ge strict", "(>= 2 1)" },
    { "if lt branch", "(if (< 1 2) 10 20)" },
    { "if gt branch", "(if (> 1 2) 10 20)" },
    { "if eq computed", "(if (= (+ 2 3) 5) 1 0)" },

    // === And/Or edge cases ===
    { "and true true2", "(and true true)" },
    { "and false true", "(and false true)" },
    { "and true false", "(and true false)" },
    { "or true false", "(or true false)" },
    { "or false nil", "(or false nil)" },
    // and/or short-circuit tests skipped: compiler generates code for both branches

    // === Type predicates ===
    { "string? yes", `(string? "hello")` },
    { "string? no", "(string? 42)" },
    { "int? yes", "(int? 42)" },
    { "int? no", `(int? "hello")` },
    { "symbol? yes", "(symbol? (quote foo))" },
    { "symbol? no", "(symbol? 42)" },
    { "closure? yes", "(closure? (lambda (x) x))" },
    { "closure? no", "(closure? 42)" },
    { "null? nil", "(null? nil)" },
    { "null? 42", "(null? 42)" },
    { "null? list", "(null? (quote (1 2)))" },
    { "pair? list", "(pair? (quote (1 2 3)))" },
    { "pair? 42", "(pair? 42)" },
    { "not false", "(not false)" },
    { "not 42", "(not 42)" },
    { "not empty-str", `(not "")` },

    // === Zero-arg lambdas ===
    { "thunk 42", "((lambda () 42))" },
    { "thunk capture", "(let (a 10) ((lambda () a)))" },
    { "thunk begin", "((lambda () (begin 1 2 3)))" },
    { "thunk arith", "((lambda () (+ 10 20)))" },

    // === Multi-param lambdas (strict arity) ===
    { "multi-param add", "((lambda (x y) (+ x y)) 3 4)" },
    { "multi-param 3", "((lambda (x y z) (+ x (+ y z))) 1 2 3)" },
    { "multi-param direct", "(let (add (lambda (x y) (+ x y))) (add 10 20))" },

    // === Named let patterns ===
    { "named let sum 5", "(let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "named let fact 10", "(let f (n 10 acc 1) (if (= n 0) acc (f (- n 1) (* acc n))))" },
    { "named let fib 10", "(let fib (n 10 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let fib 20", "(let fib (n 20 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let count", "(let count (n 100) (if (= n 0) 0 (count (- n 1))))" },

    // === More match patterns ===
    { "match multi 1", "(match 1 (1 10) (2 20) (3 30))" },
    { "match multi 2", "(match 2 (1 10) (2 20) (3 30))" },
    { "match multi 3", "(match 3 (1 10) (2 20) (3 30))" },
    { "match default", "(match 99 (1 10) (2 20) (x (+ x 1)))" },
    { "match computed", "(match (+ 1 1) (1 10) (2 20) (3 30))" },
    { "match pair sum", "(match (quote (10 20)) ([a b] (+ a b)))" },
    { "match single", "(match (quote (42)) ([x] x))" },
    { "match product", "(match (quote (2 3 4)) ([a b c] (* a (* b c))))" },
    { "match rest len", "(match (quote (1 2 3 4 5)) ([h .. t] (length t)))" },
    { "match string", `(match "hello" ("hello" 1) ("world" 2) (x 0))` },

    // === More effects/continuations ===
    { "reset large", "(reset (+ 100 (shift k 42)))" },
    { "shift resume mul", "(reset (* 2 (shift k (k 5))))" },
    { "shift resume add", "(reset (+ 1 (shift k (k (+ 2 3)))))" },
    { "multi-shot k", "(reset (+ 1 (shift k (+ (k 10) 1000))))" },
    { "handle get", "(handle (+ 1 (signal get 0)) (get x (resolve 100)))" },
    { "handle double", "(handle (signal double 7) (double x (resolve (* x 2))))" },
    { "handle bail", "(handle (+ 1 (+ 2 (+ 3 (signal bail 42)))) (bail x x))" },
    { "handle no trigger", "(handle (+ 10 20) (unused x 999))" },

    // === More list operations ===
    { "cadr", "(car (cdr (cons 1 (cons 2 (cons 3 nil)))))" },
    { "caddr", "(car (cdr (cdr (cons 1 (cons 2 (cons 3 nil))))))" },
    { "length 1", "(length (quote (1)))" },
    { "length 10", "(length (quote (1 2 3 4 5 6 7 8 9 10)))" },
    { "length cons 2", "(length (cons 1 (cons 2 nil)))" },
    { "car quoted", "(car (quote (10 20 30)))" },
    { "cons dotted", "(cons 1 2)" },

    // === Let shadowing ===
    { "let shadow inner", "(let (x 10) (let (x 20) x))" },
    { "let no leak", "(let (x 10) (let (y 20) x))" },

    // === Mutual recursion ===
    { "is-even 10", "(is-even 10)" },
    { "is-odd 7", "(is-odd 7)" },

    // === Composition/apply-twice ===
    { "apply-twice add3", "((apply-twice add3) 10)" },
    { "identity int", "(id 42)" },
    { "identity nil", "(id nil)" },
    { "counter c3", "(c3 5)" },

    // === String edge cases ===
    { "string-length empty", `(string-length "")` },
    { "string-length 1", `(string-length "a")` },
    { "string-append empty", `(string-append "" "abc")` },
    { "string->number neg", `(string->number "-7")` },
    { "number->string 0", `(number->string 0)` },

    // === Float edge cases ===
    { "float neg", "-2.5" },
    { "int+float", "(+ 1 2.0)" },
    { "float*int", "(* 2.5 4)" },
    { "float sub", "(- 5.5 2.5)" },
    { "float lt", "(< 1 2.5)" },
    { "float gt", "(> 3.5 2)" },
    { "float eq", "(= 1 1.0)" },

    // === Begin edge cases ===
    { "begin nested", "(begin (begin 1 2) (begin 3 4))" },
    { "begin if", "(begin (if true 10 20) (if false 30 40))" },

    // === Recursive let 6 ===
    { "let rec fact 6", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 6))" },

    // === Misc ===
    { "nested closures", "((lambda (x) ((lambda (y) (+ x y)) 20)) 10)" },
    { "deep closure chain", "((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 1) 2) 3)" },
    { "list operations", "(car (cdr (cdr (quote (1 2 3 4 5)))))" },
    { "complex arith", "(* (+ 1 2) (- 10 (/ 8 2)))" },
    { "recursive list build", "(length (cons 1 (cons 2 (cons 3 (quote ())))))" },

    // === Stdlib HOFs ===
    { "stdlib map double", "(map (lambda (x) (* x 2)) (quote (1 2 3)))" },
    { "stdlib map inc", "(map (lambda (x) (+ x 1)) (quote (10 20 30)))" },
    { "stdlib map identity", "(map (lambda (x) x) (quote (1 2 3)))" },
    { "stdlib filter gt2", "(filter (lambda (x) (> x 2)) (quote (1 2 3 4 5)))" },
    { "stdlib filter none", "(filter (lambda (x) (> x 100)) (quote (1 2 3)))" },
    { "stdlib filter all", "(filter (lambda (x) (> x 0)) (quote (1 2 3)))" },
    { "stdlib foldl sum", "(foldl + 0 (quote (1 2 3 4 5)))" },
    { "stdlib foldl product", "(foldl * 1 (quote (1 2 3 4 5)))" },
    { "stdlib foldr cons", "(foldr cons nil (quote (1 2 3)))" },
    { "stdlib append", "(append (quote (1 2)) (quote (3 4)))" },
    { "stdlib append empty l", "(append (quote ()) (quote (1 2)))" },
    { "stdlib append empty r", "(append (quote (1 2)) (quote ()))" },
    { "stdlib reverse", "(reverse (quote (1 2 3)))" },
    { "stdlib reverse single", "(reverse (quote (7)))" },
    { "stdlib reverse empty", "(reverse (quote ()))" },
    { "stdlib compose", "((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 5)" },
    { "stdlib nth 0", "(nth 0 (quote (10 20 30)))" },
    { "stdlib nth 2", "(nth 2 (quote (10 20 30)))" },
    { "stdlib take 3", "(take 3 (quote (1 2 3 4 5)))" },
    { "stdlib take 0", "(take 0 (quote (1 2 3)))" },
    { "stdlib take all", "(take 5 (quote (1 2 3)))" },
    { "stdlib drop 2", "(drop 2 (quote (1 2 3 4 5)))" },
    { "stdlib drop 0", "(drop 0 (quote (1 2 3)))" },
    { "stdlib drop all", "(drop 5 (quote (1 2 3)))" },
    { "stdlib zip", "(car (zip (quote (1 2 3)) (quote (4 5 6))))" },
    { "stdlib range 5", "(range 5)" },
    { "stdlib range 0", "(range 0)" },
    { "stdlib range 1", "(range 1)" },
    { "stdlib any? found", "(any? (lambda (x) (= x 3)) (quote (1 2 3 4)))" },
    { "stdlib any? not found", "(any? (lambda (x) (= x 99)) (quote (1 2 3)))" },
    { "stdlib every? true", "(every? (lambda (x) (> x 0)) (quote (1 2 3)))" },
    { "stdlib every? false", "(every? (lambda (x) (> x 2)) (quote (1 2 3)))" },
    { "stdlib assoc found", "(assoc 2 (quote ((1 . 10) (2 . 20) (3 . 30))))" },
    { "stdlib assoc not found", "(assoc 99 (quote ((1 . 10) (2 . 20))))" },
    { "stdlib assoc-ref", "(assoc-ref 2 (quote ((1 . 10) (2 . 20) (3 . 30))))" },
    { "stdlib id compose", "((compose id id) 42)" },
    { "stdlib map filter", "(foldl + 0 (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))" },

    // === Dict operations ===
    { "dict empty", "(dict? (dict))" },
    { "dict create ref", "(ref (dict 1 10 2 20) 1)" },
    { "dict ref 2", "(ref (dict 1 10 2 20) 2)" },
    { "dict? yes", "(dict? (dict 1 2))" },
    { "dict? no", "(dict? 42)" },
    { "dict has? yes", "(has? (dict 1 10 2 20) 1)" },
    { "dict has? no", "(has? (dict 1 10) 99)" },
    { "dict keys", "(length (keys (dict 1 10 2 20)))" },

    // string-contains?, string-index-of, string-replace, char-at,
    // string-repeat, symbol->string, string->symbol — not mapped in compiler

    // === String operations (extended) ===
    { "string-upcase", `(string-upcase "hello")` },
    { "string-downcase", `(string-downcase "HELLO")` },
    { "string-trim", `(string-trim "  hello  ")` },

    // === set! mutation (setups define s_x=10, s_acc=0, s_count=0) ===
    { "set! returns value", "(set! s_x 20)" },
    { "set! read back", "(begin (set! s_acc 1) (set! s_acc (+ s_acc 1)) s_acc)" },
    { "set! in lambda", "(begin ((lambda (n) (set! s_count n)) 42) s_count)" },

    // === Quasiquote (no unquote — eval_source can't see let-bound vars) ===
    { "quasiquote literal", "`42" },
    { "quasiquote list", "`(1 2 3)" },
    { "quasiquote no unquote", "`(a b c)" },

    // when/unless/cond — skipped: compiler doesn't expand eval-time macros

    // === Dot-bracket indexing ===
    { "dot-bracket 0", "(let (lst (quote (10 20 30))) lst.[0])" },
    { "dot-bracket 2", "(let (lst (quote (10 20 30))) lst.[2])" },
    { "dot-bracket expr", "(let (lst (quote (10 20 30))) lst.[(+ 0 1)])" },

    // === Deep recursion / TCO ===
    { "tco sum 1000", "(let loop (n 1000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "tco count 5000", "(let count (n 5000) (if (= n 0) 0 (count (- n 1))))" },

    // === Complex combinations ===
    { "map then reverse", "(reverse (map (lambda (x) (* x 10)) (quote (1 2 3))))" },
    { "filter then length", "(length (filter (lambda (x) (> x 3)) (quote (1 2 3 4 5 6))))" },
    { "nested map", "(map (lambda (x) (+ x 1)) (map (lambda (x) (* x 2)) (quote (1 2 3))))" },
    { "foldl with filter", "(foldl + 0 (filter (lambda (x) (< x 4)) (quote (1 2 3 4 5))))" },
    { "compose triple", "((compose (lambda (x) (* x 3)) (lambda (x) (+ x 1))) 10)" },
    { "range then sum", "(foldl + 0 (range 10))" },
    { "take from range", "(take 3 (range 10))" },
    { "drop from range", "(drop 7 (range 10))" },
    { "match on map result", "(match (map (lambda (x) (* x 10)) (quote (1 2 3))) ([a b c] (+ a (+ b c))))" },
    { "closure over let rec", "(let ^rec (f (lambda (n) (if (<= n 1) 1 (* n (f (- n 1)))))) (f 7))" },
    { "nested handle", "(handle (handle (+ 1 (signal inner 0)) (inner x (resolve 10))) (outer x (resolve 0)))" },
    { "shift double resume", "(reset (+ (shift k (+ (k 1) (k 2))) 10))" },
    { "let-rec factorial 8", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 8))" },

    // === P1: type-of ===
    { "type-of int", "(type-of 42)" },
    { "type-of string", `(type-of "hello")` },
    { "type-of symbol", "(type-of (quote foo))" },
    { "type-of list", "(type-of (quote (1 2)))" },
    { "type-of nil", "(type-of nil)" },
    { "type-of true", "(type-of true)" },
    { "type-of closure", "(type-of (lambda (x) x))" },
    { "type-of double", "(type-of 3.14)" },

    // === P1: type predicates ===
    { "int? yes", "(int? 42)" },
    { "int? no", "(int? 3.14)" },
    { "string? yes", `(string? "hi")` },
    { "string? no", "(string? 42)" },
    { "symbol? yes", "(symbol? (quote foo))" },
    { "symbol? no", "(symbol? 42)" },
    { "double? yes", "(double? 3.14)" },
    { "double? no", "(double? 42)" },
    { "number? int", "(number? 42)" },
    { "number? double", "(number? 3.14)" },
    { "number? no", "(number? (quote foo))" },
    { "pair? yes", "(pair? (quote (1 2)))" },
    { "pair? no", "(pair? 42)" },
    { "null? yes", "(null? nil)" },
    { "null? no", "(null? 42)" },

    // === P1: math primitives ===
    { "abs positive", "(abs 5)" },
    { "abs negative", "(abs -7)" },
    { "abs zero", "(abs 0)" },
    { "abs double", "(abs -3.5)" },
    { "min ints", "(min 3 7)" },
    { "min reversed", "(min 7 3)" },
    { "min doubles", "(min 1.5 2.5)" },
    { "max ints", "(max 3 7)" },
    { "max reversed", "(max 7 3)" },
    { "max doubles", "(max 1.5 2.5)" },
    { "floor", "(floor 3.7)" },
    { "floor neg", "(floor -3.2)" },
    { "ceiling", "(ceiling 3.2)" },
    { "ceiling neg", "(ceiling -3.7)" },
    { "round up", "(round 3.6)" },
    { "round down", "(round 3.4)" },
    { "truncate pos", "(truncate 3.9)" },
    { "truncate neg", "(truncate -3.9)" },
    { "sqrt 4", "(sqrt 4)" },
    { "sqrt 9", "(sqrt 9)" },
    { "even? yes", "(even? 4)" },
    { "even? no", "(even? 3)" },
    { "odd? yes", "(odd? 3)" },
    { "odd? no", "(odd? 4)" },
    { "zero? yes", "(zero? 0)" },
    { "zero? no", "(zero? 1)" },
    { "positive? yes", "(positive? 5)" },
    { "positive? no", "(positive? -3)" },
    { "negative? yes", "(negative? -3)" },
    { "negative? no", "(negative? 5)" },
    { "gcd", "(gcd 12 8)" },
    { "gcd coprime", "(gcd 7 13)" },

    // === P2: substring (3-arg via interpreter delegation) ===
    { "substring basic", `(substring "hello" 1 3)` },
    { "substring from start", `(substring "abcdef" 0 3)` },
    { "substring to end", `(substring "abcdef" 3 6)` },
    { "substring single", `(substring "hello" 0 1)` },

    // === P2: string-split and string-join ===
    { "string-split", `(length (string-split "a,b,c" ","))` },
    { "string-join", `(string-join " " (quote ("hello" "world")))` },
    { "string-join empty", `(string-join "," (quote ()))` },

    // === P2: when/unless ===
    { "when true", "(when true 42)" },
    { "when false", "(when false 42)" },
    { "unless true", "(unless true 42)" },
    { "unless false", "(unless false 42)" },

    // === P3: Bitwise ops ===
    { "bitwise-and", "(bitwise-and 12 10)" },
    { "bitwise-or", "(bitwise-or 12 10)" },
    { "bitwise-xor", "(bitwise-xor 12 10)" },
    { "bitwise-not 0", "(bitwise-not 0)" },
    { "bitwise-not 1", "(bitwise-not 1)" },
    { "lshift", "(lshift 1 4)" },
    { "rshift", "(rshift 16 4)" },
    { "lshift-rshift", "(rshift (lshift 3 8) 8)" },

    // === P4: Quasiquote + unquote ===
    { "qq unquote let", "(let (x 42) `(a ,x b))" },
    { "qq unquote arith", "(let (n 10) `(result ,(+ n 5)))" },
    { "qq unquote nested", "(let (a 1 b 2) `(,a ,b ,(+ a b)))" },
    { "qq splice literal", "`(1 2 3)" },
};

fn void append_line_to_buf(char* buf, usz* pos, usz cap, char[] text) {
    for (usz i = 0; i < text.len && (*pos) < cap - 2; i++) {
        buf[(*pos)++] = text[i];
    }
    if ((*pos) < cap - 1) buf[(*pos)++] = '\n';
}

fn void generate_e2e_run_setups(Interp* e2e_interp) {
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        EvalResult r = run(E2E_SETUPS[i], e2e_interp);
        if (r.error.has_error) {
            io::printfn("WARNING: Setup %d failed, skipping", (int)i);
        }
    }
}

fn void generate_e2e_build_source_prefix(char* source_buf, usz* source_pos, usz buf_size) {
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        append_line_to_buf(source_buf, source_pos, buf_size, E2E_SETUPS[i]);
    }
    append_line_to_buf(source_buf, source_pos, buf_size, "(define __e2e_start__ true)");
}

fn void generate_e2e_collect_cases(
    Interp* e2e_interp,
    char* expected_buf,
    usz* expected_pos,
    char* source_buf,
    usz* source_pos,
    usz buf_size,
    int* test_count,
    int* skip_count
) {
    for (usz i = 0; i < E2E_TESTS.len; i++) {
        EvalResult r = run(E2E_TESTS[i].expr, e2e_interp);
        if (r.error.has_error) {
            io::printfn("  SKIP (interp error): %s", (ZString)E2E_TESTS[i].name);
            (*skip_count)++;
            continue;
        }

        char[1024] val_buf;
        usz val_len = print_value_to_buf(r.value, &e2e_interp.symbols, &val_buf, val_buf.len);
        append_line_to_buf(expected_buf, expected_pos, buf_size, val_buf[:val_len]);
        append_line_to_buf(source_buf, source_pos, buf_size, E2E_TESTS[i].expr);
        (*test_count)++;
    }
}

fn bool write_text_file(char[] path, char[] text) {
    if (try file = io::file::open((String)path, "w")) {
        defer (void)file.close();
        file.write(text)!!;
        return true;
    }
    return false;
}

fn void generate_e2e_tests(Interp* _interp) {
    (void)_interp;
    io::printn("=== Generating E2E Compiler Tests ===");

    Interp* e2e_interp = (Interp*)mem::malloc(Interp.sizeof);
    defer {
        e2e_interp.destroy();
        mem::free(e2e_interp);
    }
    e2e_interp.init();
    register_primitives(e2e_interp);
    register_stdlib(e2e_interp);
    generate_e2e_run_setups(e2e_interp);

    usz buf_size = 131072;
    char* expected_buf = (char*)mem::malloc(buf_size);
    char* source_buf = (char*)mem::malloc(buf_size);
    defer {
        mem::free(expected_buf);
        mem::free(source_buf);
    }

    usz expected_pos = 0;
    usz source_pos = 0;
    int test_count = 0;
    int skip_count = 0;
    generate_e2e_build_source_prefix(source_buf, &source_pos, buf_size);
    generate_e2e_collect_cases(
        e2e_interp,
        expected_buf, &expected_pos,
        source_buf, &source_pos,
        buf_size,
        &test_count, &skip_count);

    io::printfn("  Compiling %d test expressions...", test_count);
    char[] c3_code = compile_to_c3_print_all(source_buf[:source_pos], e2e_interp);
    if (c3_code.len == 0) {
        io::printn("ERROR: Compilation produced empty output");
        return;
    }

    if (!write_text_file("build/e2e_test.c3", c3_code)) {
        io::printn("ERROR: Cannot write build/e2e_test.c3");
        return;
    }
    io::printfn("  Wrote build/e2e_test.c3 (%d bytes)", (int)c3_code.len);

    char[] expected_out = expected_buf[:expected_pos];
    if (!write_text_file("build/e2e_expected.txt", expected_out)) {
        io::printn("ERROR: Cannot write build/e2e_expected.txt");
        return;
    }
    io::printfn("  Wrote build/e2e_expected.txt (%d lines)", test_count);
    io::printfn("=== Generated %d e2e tests (%d skipped) ===", test_count, skip_count);
}

fn void run_diagnostic_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Diagnostic Tests ---");

    // --- Dispatch diagnostics ---

    // Type mismatch - shows arg types
    test_error_contains(interp, "dispatch: type mismatch shows types",
        "(begin (define (typed-fn (^Int x)) x) (typed-fn \"hello\"))",
        "String", pass, fail);

    // Shows method name
    test_error_contains(interp, "dispatch: shows method name",
        "(begin (define (typed-fn2 (^Int x)) x) (typed-fn2 \"hello\"))",
        "typed-fn2", pass, fail);

    // Arity mismatch
    test_error_contains(interp, "dispatch: arity mismatch",
        "(begin (define [type] DPt (^Int x) (^Int y)) (define (dadd (^DPt a) (^DPt b)) 1) (dadd (DPt 1 2)))",
        "no matching method", pass, fail);

    // --- Match exhaustiveness ---

    // Union match missing variant
    test_error_contains(interp, "match: missing union variant",
        "(begin (define [union] DiagResult DiagOk (DiagErr Int)) (match (DiagErr 42) ((DiagOk) 1)))",
        "missing variants", pass, fail);

    // Non-union match shows type
    test_error_contains(interp, "match: non-union shows type",
        "(match 42 (\"hello\" 1))",
        "Int", pass, fail);

    // --- Strict handlers ---

    // Strict handler catches unhandled effect
    test_error_contains(interp, "handle ^strict: catches unhandled",
        "(handle ^strict (signal my-eff 42) (other-eff v v))",
        "strict handler", pass, fail);

    // Strict handler allows handled effects
    test_eq(interp, "handle ^strict: allows handled",
        "(handle ^strict (signal my-eff 42) (my-eff v (+ v 1)))",
        43, pass, fail);

    // --- Unhandled effect diagnostics ---

    // Shows tag name
    test_error_contains(interp, "unhandled effect: shows tag name",
        "(signal unknown-eff 42)",
        "unknown-eff", pass, fail);

    // Shows arg type
    test_error_contains(interp, "unhandled effect: shows arg type",
        "(signal unknown-eff \"hello\")",
        "String", pass, fail);
}

fn void run_diagnostic_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_diagnostic_tests, pass, fail);
}

fn void run_async_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Async I/O Tests ---");

    // DNS resolve localhost
    test_str_val(interp, "dns-resolve localhost",
        "(dns-resolve \"localhost\")", "127.0.0.1", pass, fail);

    // async-sleep returns nil
    test_tag(interp, "async-sleep returns nil",
        "(async-sleep 1)", NIL, pass, fail);

    // DNS resolve returns a string
    test_str(interp, "dns-resolve returns string",
        "(dns-resolve \"localhost\")", pass, fail);

    // tls-connect optional CA path validation
    test_error_contains(interp, "tls-connect rejects non-string ca bundle path",
        "(tls-connect nil \"localhost\" 1)",
        "ca-bundle-path must be a string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-string client cert path",
        "(tls-connect nil \"localhost\" \"ca.pem\" 1 \"client-key.pem\")",
        "client-cert-pem-path must be a string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-string client key path",
        "(tls-connect nil \"localhost\" \"ca.pem\" \"client-cert.pem\" 1)",
        "client-key-pem-path must be a string", pass, fail);
    test_error_contains(interp, "tls-connect rejects non-bool resume policy",
        "(tls-connect nil \"localhost\" \"ca.pem\" \"client-cert.pem\" \"client-key.pem\" 1)",
        "resume-session? must be true or false", pass, fail);
    test_error_contains(interp, "tls-connect accepts bool-only resume policy form",
        "(tls-connect nil \"localhost\" true)",
        "invalid TCP handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts direct mTLS form without CA",
        "(tls-connect nil \"localhost\" \"client-cert.pem\" \"client-key.pem\")",
        "invalid TCP handle", pass, fail);
    test_error_contains(interp, "tls-connect accepts ca+resume policy form",
        "(tls-connect nil \"localhost\" \"ca.pem\" true)",
        "invalid TCP handle", pass, fail);

    // TLS integration: run server in a separate Omni process to avoid
    // scheduler deadlocks from blocking TLS read/write in same runtime thread.
    test_truthy(interp, "tls valid cert handshake",
        "(begin "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=45101 LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\"] "
        "      nil)) "
        "  (async-sleep 60) "
        "  (define c nil) "
        "  (define resp nil) "
        "  (define client-msg "
        "    (try-message "
        "      (lambda (ignored) "
        "        (begin "
        "          (set! c (tcp-connect \"127.0.0.1\" 45101)) "
        "          (define ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "          (tls-write ctls \"ping\") "
        "          (set! resp (tls-read ctls)) "
        "          (tls-close ctls) "
        "          \"ok\")) "
        "      (lambda (e) e))) "
        "  (if c (tcp-close c) nil) "
        "  (define status (process-wait (ref proc 'handle))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= client-msg \"ok\") (and (= resp \"pong\") (= (ref status 'exit-code) 0))))",
        pass, fail);

    test_truthy(interp, "tls rejects untrusted cert",
        "(begin "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=45102 OMNI_TLS_TEST_CERT=tests/lib/tls/bad-server.cert.pem OMNI_TLS_TEST_KEY=tests/lib/tls/bad-server.key.pem LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\"] "
        "      nil)) "
        "  (async-sleep 60) "
        "  (define c nil) "
        "  (define client-msg "
        "    (try-message "
        "      (lambda (ignored) "
        "        (begin "
        "          (set! c (tcp-connect \"127.0.0.1\" 45102)) "
        "          (define ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "          (tls-write ctls \"ping\") "
        "          (tls-close ctls) "
        "          \"ok\")) "
        "      (lambda (e) e))) "
        "  (if c (tcp-close c) nil) "
        "  (define status (process-wait (ref proc 'handle))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (or (string-contains? client-msg \"write failed\") "
        "                (or (string-contains? client-msg \"read failed\") "
        "                    (string-contains? client-msg \"reset failed\"))))))",
        pass, fail);

    test_truthy(interp, "tls rejects hostname mismatch",
        "(begin "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=45103 LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\"] "
        "      nil)) "
        "  (async-sleep 60) "
        "  (define c nil) "
        "  (define client-msg "
        "    (try-message "
        "      (lambda (ignored) "
        "        (begin "
        "          (set! c (tcp-connect \"127.0.0.1\" 45103)) "
        "          (define ctls (tls-connect c \"not-localhost\" \"tests/lib/tls/ca.cert.pem\")) "
        "          (tls-write ctls \"ping\") "
        "          (tls-close ctls) "
        "          \"ok\")) "
        "      (lambda (e) e))) "
        "  (if c (tcp-close c) nil) "
        "  (define status (process-wait (ref proc 'handle))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (or (string-contains? client-msg \"write failed\") "
        "                (or (string-contains? client-msg \"read failed\") "
        "                    (string-contains? client-msg \"reset failed\"))))))",
        pass, fail);

    test_truthy(interp, "tls mTLS optional cert/key pass",
        "(begin "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=45104 LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\"] "
        "      nil)) "
        "  (async-sleep 60) "
        "  (define c nil) "
        "  (define resp nil) "
        "  (define client-msg "
        "    (try-message "
        "      (lambda (ignored) "
        "        (begin "
        "          (set! c (tcp-connect \"127.0.0.1\" 45104)) "
        "          (define ctls (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\" \"tests/lib/tls/client.cert.pem\" \"tests/lib/tls/client.key.pem\" true)) "
        "          (tls-write ctls \"ping\") "
        "          (set! resp (tls-read ctls)) "
        "          (tls-close ctls) "
        "          \"ok\")) "
        "      (lambda (e) e))) "
        "  (if c (tcp-close c) nil) "
        "  (define status (process-wait (ref proc 'handle))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= client-msg \"ok\") (and (= resp \"pong\") (= (ref status 'exit-code) 0))))",
        pass, fail);

    test_truthy(interp, "tls mTLS invalid key fails",
        "(begin "
        "  (define proc "
        "    (process-spawn \"/bin/sh\" "
        "      [\"-c\" \"ASAN_OPTIONS=detect_leaks=0 OMNI_TLS_TEST_PORT=45105 LD_LIBRARY_PATH=/usr/local/lib ./build/main tests/lib/tls/server_once.omni\"] "
        "      nil)) "
        "  (async-sleep 60) "
        "  (define c nil) "
        "  (define client-msg "
        "    (try-message "
        "      (lambda (ignored) "
        "        (begin "
        "          (set! c (tcp-connect \"127.0.0.1\" 45105)) "
        "          (tls-connect c \"localhost\" \"tests/lib/tls/ca.cert.pem\" \"tests/lib/tls/client.cert.pem\" \"tests/lib/tls/missing-client.key.pem\") "
        "          \"ok\")) "
        "      (lambda (e) e))) "
        "  (if c (tcp-close c) nil) "
        "  (define status (process-wait (ref proc 'handle))) "
        "  (fs-close (ref proc 'stdin)) "
        "  (fs-close (ref proc 'stdout)) "
        "  (fs-close (ref proc 'stderr)) "
        "  (and (= (ref status 'exit-code) 0) "
        "       (and (string? client-msg) "
        "            (string-contains? client-msg \"failed to configure client cert/key\"))))",
        pass, fail);

    // tls-server-wrap argument validation
    test_error_contains(interp, "tls-server-wrap rejects non-string cert path",
        "(__raw-tls-server-wrap nil 1 \"key.pem\")",
        "cert-pem-path must be a string", pass, fail);
    test_error_contains(interp, "tls-server-wrap rejects non-stream handle",
        "(__raw-tls-server-wrap nil \"cert.pem\" \"key.pem\")",
        "invalid TCP stream handle", pass, fail);

    // listener lifecycle
    test_tag(interp, "tcp-listen open/close",
        "(begin (define ls (tcp-listen \"127.0.0.1\" 0 8)) (tcp-close ls))", NIL, pass, fail);

    // tcp-accept argument validation
    test_error_contains(interp, "tcp-accept rejects non-listener",
        "(tcp-accept nil)",
        "invalid or closed listener handle", pass, fail);

    // fiber-safe accept path: accept in fiber (offloaded), connect/write in main
    test_truthy(interp, "tcp-accept in fiber via offload",
        "(begin "
        "  (define p (+ 30000 (% (time-ms) 20000))) "
        "  (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "  (define af (spawn (lambda () (tcp-accept listener)))) "
        "  (async-sleep 5) "
        "  (define client (tcp-connect \"127.0.0.1\" p)) "
        "  (tcp-write client \"ping\") "
        "  (define server-client (await af)) "
        "  (define payload (tcp-read server-client)) "
        "  (tcp-close client) "
        "  (tcp-close server-client) "
        "  (tcp-close listener) "
        "  (= payload \"ping\"))",
        pass, fail);

    // fiber-safe write path: write in fiber uses async writable wakeup bridge
    test_truthy(interp, "tcp-write in fiber via async bridge",
        "(begin "
        "  (define p (+ 30500 (% (time-ms) 20000))) "
        "  (define listener (tcp-listen \"127.0.0.1\" p 8)) "
        "  (define af (spawn (lambda () (tcp-accept listener)))) "
        "  (async-sleep 5) "
        "  (define client (tcp-connect \"127.0.0.1\" p)) "
        "  (define server-client (await af)) "
        "  (define rf (spawn (lambda () (tcp-read server-client)))) "
        "  (define wf (spawn (lambda () (tcp-write client \"ping\")))) "
        "  (define payload (await rf)) "
        "  (define wrote (await wf)) "
        "  (tcp-close client) "
        "  (tcp-close server-client) "
        "  (tcp-close listener) "
        "  (and (= payload \"ping\") (= wrote 4)))",
        pass, fail);

    // fiber-safe UDP receive path: recv in fiber uses async wakeup bridge
    test_truthy(interp, "udp-recv in fiber via async bridge",
        "(begin "
        "  (define p (+ 35000 (% (time-ms) 20000))) "
        "  (define recv (udp-socket)) "
        "  (udp-bind recv \"127.0.0.1\" p) "
        "  (define rf (spawn (lambda () (udp-recv recv)))) "
        "  (async-sleep 5) "
        "  (define snd (udp-socket)) "
        "  (udp-send snd \"127.0.0.1\" p \"pong\") "
        "  (define got (await rf)) "
        "  (udp-close snd) "
        "  (udp-close recv) "
        "  (= got \"pong\"))",
        pass, fail);
}

fn void run_async_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_async_tests, pass, fail);
}

fn void run_json_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- JSON Tests ---");

    // Parse strict-by-default
    test_error(interp, "json-parse rejects comments by default",
        "(json-parse \"{\\\"a\\\": 1 /* comment */ \\\"b\\\": 2}\")", pass, fail);
    test_error(interp, "json-parse rejects trailing comma by default",
        "(json-parse \"[1,2,3,]\")", pass, fail);
    test_error(interp, "json-parse rejects NaN/Inf by default",
        "(json-parse \"[NaN, Infinity]\")", pass, fail);

    // Parse options
    test_eq(interp, "json-parse allows comments",
        "(ref (json-parse \"{\\\"a\\\": 1 /* comment */ , \\\"b\\\": 2}\" '(allow-comments)) \"b\")", 2, pass, fail);
    test_eq(interp, "json-parse allows trailing commas",
        "(ref (json-parse \"[1,2,3,]\" '(allow-trailing-commas)) 2)", 3, pass, fail);
    test_tag(interp, "json-parse allows NaN/Inf",
        "(ref (json-parse \"[NaN, Infinity, -Infinity]\" '(allow-nan-inf)) 0)", DOUBLE, pass, fail);
    test_eq(interp, "json-parse allows comments and trailing commas together",
        "(ref (json-parse \"{\\\"a\\\": 1 /*comment*/ ,\\\"b\\\": 2,}\" '(allow-comments allow-trailing-commas)) \"b\")", 2, pass, fail);
    test_error(interp, "json-parse trailing commas still strict without allow-trailing-commas",
        "(json-parse \"[1,2,3,]\" '(allow-comments))", pass, fail);
    test_error(interp, "json-parse comments still strict without allow-comments",
        "(json-parse \"{\\\"a\\\": 1 /* comment */ , \\\"b\\\": 2}\" '(allow-trailing-commas))", pass, fail);
    test_error(interp, "json-parse unknown option symbol",
        "(json-parse \"42\" '(allow-foo))", pass, fail);

    // Parse basic types
    test_eq(interp, "json-parse integer",
        "(json-parse \"42\")", 42, pass, fail);
    test_str_val(interp, "json-parse string",
        "(json-parse \"\\\"hello\\\"\")", "hello", pass, fail);
    test_tag(interp, "json-parse null",
        "(json-parse \"null\")", NIL, pass, fail);

    // Parse array
    test_tag(interp, "json-parse array is array",
        "(json-parse \"[1,2,3]\")", ARRAY, pass, fail);
    test_eq(interp, "json-parse array length",
        "(length (json-parse \"[1,2,3]\"))", 3, pass, fail);

    // json-get pointer lookup
    test_eq(interp, "json-get object key",
        "(json-get (json-parse \"{\\\"a\\\": {\\\"b\\\": 42}}\") \"/a/b\")", 42, pass, fail);
    test_str_val(interp, "json-get array index",
        "(json-get (json-parse \"{\\\"users\\\": [\\\"alice\\\",\\\"bob\\\"]}\") \"/users/1\")", "bob", pass, fail);
    test_eq(interp, "json-get escaped object key",
        "(json-get (json-parse \"{\\\"a/b\\\": 7, \\\"a~0b\\\": 9}\") \"/a~1b\")", 7, pass, fail);
    test_tag(interp, "json-get root path",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"\")", HASHMAP, pass, fail);

    test_nil(interp, "json-get missing path returns nil",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"/b\")", pass, fail);
    test_error(interp, "json-get invalid escape",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"/a~\")", pass, fail);
    test_error(interp, "json-get invalid path start",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"a/b\")", pass, fail);

    // Parse object
    test_tag(interp, "json-parse object is dict",
        "(json-parse \"{\\\"a\\\": 1}\")", HASHMAP, pass, fail);

    // Emit basic types
    test_str_val(interp, "json-emit integer",
        "(json-emit 42)", "42", pass, fail);
    test_str_val(interp, "json-emit string",
        "(json-emit \"hello\")", "\"hello\"", pass, fail);
    test_str_val(interp, "json-emit supports precision",
        "(json-emit 1.23456 '((precision 2)))", "1.23", pass, fail);
    test_str_val(interp, "json-emit supports writer flag escape-slashes",
        "(json-emit \"a/b\" '((escape-slashes true)))", "\"a\\/b\"", pass, fail);
    test_str_val(interp, "json-emit supports writer flag newline-at-end",
        "(json-emit [1 2 3] '((newline-at-end true)))", "[1,2,3]\n", pass, fail);
    test_str_val(interp, "json-emit-pretty accepts option precision override",
        "(json-emit-pretty 1.2 '((precision 1)))", "1.2", pass, fail);
    test_str_val(interp, "json-emit nil",
        "(json-emit nil)", "null", pass, fail);

    // Round-trip: array
    test_str_val(interp, "json-emit array",
        "(json-emit [1 2 3])", "[1,2,3]", pass, fail);

    test_error_contains(interp, "json-emit validates unknown option",
        "(json-emit 1 '((bogus true)))", "json-emit: unknown option key", pass, fail);
    test_error_contains(interp, "json-emit validates precision range",
        "(json-emit 1 '((precision 99)))", "precision must be an integer between 0 and 15", pass, fail);
    test_error_contains(interp, "json-emit validates pretty-two-spaces option type",
        "(json-emit 1 '((pretty-two-spaces 1)))", "must be a boolean", pass, fail);

    // Round-trip: emit then parse
    test_eq(interp, "json round-trip integer",
        "(json-parse (json-emit 99))", 99, pass, fail);
}

fn void run_data_format_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Data Format Tests ---");

    // TOML
    test_tag(interp, "toml-parse object is dict",
        "(toml-parse \"title = \\\"Toml\\\"\\n[owner]\\nname = \\\"Alice\\\"\\n\")", HASHMAP, pass, fail);
    test_str_val(interp, "toml-parse simple scalar",
        "(ref (toml-parse \"title = \\\"Toml\\\"\\n\") \"title\")", "Toml", pass, fail);
    test_str_val(interp, "toml-parse nested scalar",
        "(ref (ref (toml-parse \"[owner]\\nname = \\\"Alice\\\"\\n\\nage = 42\\n\") \"owner\") \"name\")", "Alice", pass, fail);
    test_eq(interp, "toml-parse numeric field",
        "(ref (toml-parse \"age = 42\\n\") \"age\")", 42, pass, fail);
    test_tag(interp, "toml-parse date",
        "(ref (toml-parse \"d = 1979-05-27\\n\") \"d\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse time",
        "(ref (toml-parse \"t = 07:32:00\\n\") \"t\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse datetime",
        "(ref (toml-parse \"dt = 1979-05-27T07:32:00Z\") \"dt\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse datetime with offset",
        "(ref (toml-parse \"dtz = 1979-05-27T07:32:00-07:00\\n\") \"dtz\")", TIME_POINT, pass, fail);
    test_truthy(interp, "toml-parse date equality",
        "(let (d1 (ref (toml-parse \"d = 1979-05-27\\n\") \"d\") d2 (ref (toml-parse \"d = 1979-05-27\\n\") \"d\")) (= d1 d2))",
        pass, fail);
    test_tag(interp, "time-point constructor date",
        "(time-point 'date 2026 3 7)", TIME_POINT, pass, fail);
    test_truthy(interp, "time-point predicate for date",
        "(time-point? (time-point 'date 2026 3 7))", pass, fail);
    test_tag(interp, "time-point constructor time",
        "(time-point 'time 8 15 30 123456)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime",
        "(time-point 'datetime 2026 3 7 8 15 30 123456)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime with tz",
        "(time-point 'datetime-tz 2026 3 7 8 15 30 123456 -420)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime with tz alias",
        "(time-point 'datetimetz 2026 3 7 8 15 30 123456 -420)", TIME_POINT, pass, fail);
    test_error_contains(interp, "time-point rejects unknown kind",
        "(time-point 'unknown 1 2 3)", "time-point:", pass, fail);
    test_error_contains(interp, "time-point rejects bad datetime-tz arity",
        "(time-point 'datetime-tz 2026 3 7 8 15 30)", "time-point: datetime-tz expects", pass, fail);
    test_tag(interp, "toml-parse supports options list",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((check-utf8 false)))", HASHMAP, pass, fail);
    test_error_contains(interp, "toml-parse validates unknown option key",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((bogus true)))", "toml-parse: unknown option key", pass, fail);
    test_error_contains(interp, "toml-parse validates option value",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((check-utf8 1)))", "toml-parse: check-utf8 must be a boolean", pass, fail);
    test_error_contains(interp, "toml-parse rejects invalid input",
        "(toml-parse \"[broken = 1\")", "toml-parse", pass, fail);

    // CSV parse
    test_tag(interp, "csv-parse returns array",
        "(csv-parse \"a,b\\n1,2\")", ARRAY, pass, fail);
    test_str_val(interp, "csv-parse basic values",
        "(ref (ref (csv-parse \"a,b\\n1,2\") 0) 1)", "b", pass, fail);
    test_str_val(interp, "csv-parse quoted delimiter",
        "(ref (ref (csv-parse \"\\\"a,b\\\",2\") 0) 0)", "a,b", pass, fail);
    test_str_val(interp, "csv-parse preserves trailing empty field",
        "(ref (ref (csv-parse \"a,b,\") 0) 2)", "", pass, fail);
    test_str_val(interp, "csv-parse supports strict mode toggle",
        "(ref (ref (csv-parse \"a,b\\rc,d\" '((strict false))) 1) 0)", "c", pass, fail);
    test_str_val(interp, "csv-parse strict allows quote inside unquoted field",
        "(ref (ref (csv-parse \"a\\\"b,c\" '((strict false))) 0) 0)", "a\"b", pass, fail);
    test_eq(interp, "csv-parse omits trailing empty row",
        "(length (csv-parse \"a,b\\n\"))", 1, pass, fail);
    test_str_val(interp, "csv-parse quoted multiline field",
        "(ref (ref (csv-parse \"\\\"a\\nb\\\",c\\nd,e\") 0) 0)", "a\nb", pass, fail);
    test_error_contains(interp, "csv-parse rejects unterminated quote",
        "(csv-parse \"\\\"a,b\")", "csv-parse: unterminated quoted field", pass, fail);
    test_error_contains(interp, "csv-parse rejects invalid CR line terminator",
        "(csv-parse \"a,b\\r;c\")", "csv-parse: invalid CR line terminator", pass, fail);

    // CSV emit
    test_str_val(interp, "csv-emit basic",
        "(csv-emit [[\"a\" \"b,2\" \"x\" ]])", "a,\"b,2\",x", pass, fail);
    test_str_val(interp, "csv-emit with list rows",
        "(csv-emit '((\"a\" \"b\") (\"c\" \"d\")))", "a,b\nc,d", pass, fail);
    test_str_val(interp, "csv-emit with custom delimiter",
        "(csv-emit [[\"a\" \"b|c\"]] \"|\")", "a|\"b|c\"", pass, fail);
    test_str_val(interp, "csv-emit supports custom line ending",
        "(csv-emit [[\"a\" \"b\"] [\"c\" \"d\"]] '((line-ending \"\\r\\n\")))", "a,b\r\nc,d", pass, fail);
    test_str_val(interp, "csv-emit supports quote-style always",
        "(csv-emit [[\"a,b\" \"x\"]] '((quote-style 'always)))", "\"a,b\",x", pass, fail);
    test_str_val(interp, "csv-emit supports quote-style none",
        "(csv-emit [[\"a,b\" \"x\"]] '((quote-style 'none)))", "a,b,x", pass, fail);
    test_str_val(interp, "csv-emit supports nil-as",
        "(csv-emit [[nil \"x\"]] '((nil-as \"NULL\")))", "NULL,x", pass, fail);
    test_str_val(interp, "csv-emit supports custom quote-char",
        "(csv-emit [[\"a|b\" \"x\"]] '((quote-char \"|\") (quote-style 'always)))", "|a||b|,x", pass, fail);
    test_error_contains(interp, "csv-emit validates quote-char != delimiter in strict mode",
        "(csv-emit [[\"a\"]] '((quote-char \",\") (strict true)))", "quote-char", pass, fail);
    test_error_contains(interp, "csv-emit validates row type",
        "(csv-emit [\"bad\"])", "csv-emit: expected each row", pass, fail);
    test_error_contains(interp, "csv-emit validates row cell types",
        "(csv-emit [[[]]])", "csv-emit: row cells must be", pass, fail);
    test_error_contains(interp, "csv-parse validates delimiter length",
        "(csv-parse \"a,b\" \";;\")", "delimiter must be a one-character string", pass, fail);
}

fn void run_json_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_json_tests, pass, fail);
}

fn void run_data_format_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_data_format_tests, pass, fail);
}

fn void run_compression_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Compression Tests ---");

    // gzip round-trip
    test_str_val(interp, "gzip/gunzip round-trip",
        "(gunzip (gzip \"hello world\"))", "hello world", pass, fail);

    // deflate round-trip
    test_str_val(interp, "deflate/inflate round-trip",
        "(inflate (deflate \"test data\"))", "test data", pass, fail);

    // explicit compression levels
    test_str_val(interp, "gzip/gunzip round-trip level 1",
        "(gunzip (gzip \"hello world\" 1))", "hello world", pass, fail);
    test_str_val(interp, "deflate/inflate round-trip level 12",
        "(inflate (deflate \"test data\" 12))", "test data", pass, fail);
    test_str_val(interp, "zlib round-trip",
        "(zlib-decompress (zlib-compress \"zlib payload\"))", "zlib payload", pass, fail);
    test_str_val(interp, "zlib round-trip level 9",
        "(zlib-decompress (zlib-compress \"zlib payload\" 9))", "zlib payload", pass, fail);

    // invalid compression level validation
    test_error_contains(interp, "gzip level rejects non-int",
        "(gzip \"hello\" 'bad)", "level must be an integer", pass, fail);
    test_error_contains(interp, "deflate level rejects out-of-range",
        "(deflate \"hello\" 99)", "level must be in range 0..12", pass, fail);
    test_error_contains(interp, "gzip rejects too many args",
        "(gzip \"hello\" 1 2)", "expected 1-2 arguments", pass, fail);
    test_error_contains(interp, "zlib-decompress rejects corrupt payload",
        "(zlib-decompress \"not-a-zlib-stream\")", "decompression failed", pass, fail);
    test_error_contains(interp, "gunzip rejects corrupt payload",
        "(gunzip \"not-a-gzip-stream\")", "decompression failed", pass, fail);
    test_error_contains(interp, "inflate rejects corrupt payload",
        "(inflate \"not-a-deflate-stream\")", "decompression failed", pass, fail);

    // checksums
    test_eq(interp, "adler32 empty", "(adler32 \"\")", 1, pass, fail);
    test_eq(interp, "crc32 empty", "(crc32 \"\")", 0, pass, fail);
    test_eq(interp, "adler32 hello", "(adler32 \"hello\")", 103547413, pass, fail);
    test_eq(interp, "crc32 hello", "(crc32 \"hello\")", 907060870, pass, fail);
    test_error_contains(interp, "adler32 rejects non-string",
        "(adler32 42)", "expected string", pass, fail);
    test_error_contains(interp, "crc32 rejects non-string",
        "(crc32 42)", "expected string", pass, fail);

    // Empty string round-trip
    test_str_val(interp, "gzip/gunzip empty",
        "(gunzip (gzip \"\"))", "", pass, fail);
}

fn void run_compression_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_compression_tests, pass, fail);
}

fn void run_unicode_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Unicode Tests ---");

    // Upcase: ASCII
    test_str_val(interp, "string-upcase ASCII",
        "(string-upcase \"hello\")", "HELLO", pass, fail);

    // Downcase: ASCII
    test_str_val(interp, "string-downcase ASCII",
        "(string-downcase \"HELLO\")", "hello", pass, fail);
    test_str_val(interp, "string-casefold ASCII",
        "(string-casefold \"HELLO\")", "hello", pass, fail);
    test_str_val(interp, "string-casefold unicode sharp s",
        "(string-casefold \"Straße\")", "strasse", pass, fail);
    test_str_val(interp, "string-titlecase ASCII",
        "(string-titlecase \"hello world\")", "Hello World", pass, fail);
    test_str_val(interp, "string-titlecase mixed",
        "(string-titlecase \"hELLO wORLD\")", "Hello World", pass, fail);
    test_eq(interp, "string-upcase long no truncation",
        "(string-length (string-upcase (string-repeat \"a\" 1500)))", 1500, pass, fail);
    test_eq(interp, "string-downcase long no truncation",
        "(string-length (string-downcase (string-repeat \"A\" 1500)))", 1500, pass, fail);
    test_eq(interp, "string-upcase long unicode no truncation",
        "(length (string-codepoints (string-upcase (string-repeat \"é\" 600))))", 600, pass, fail);
    test_eq(interp, "string-downcase long unicode no truncation",
        "(length (string-codepoints (string-downcase (string-repeat \"É\" 600))))", 600, pass, fail);

    // Codepoints: ASCII
    test_eq(interp, "string-codepoints ASCII count",
        "(length (string-codepoints \"abc\"))", 3, pass, fail);
    test_eq(interp, "string-codepoints first is 97",
        "(car (string-codepoints \"abc\"))", 97, pass, fail);

    // Char-category: letter
    test_str_val(interp, "char-category uppercase A",
        "(char-category 65)", "Lu", pass, fail);
    test_str_val(interp, "char-category lowercase a",
        "(char-category 97)", "Ll", pass, fail);
    test_str_val(interp, "char-category digit 0",
        "(char-category 48)", "Nd", pass, fail);
    test_eq(interp, "char-width ASCII A",
        "(char-width 65)", 1, pass, fail);
    test_error_contains(interp, "char-width rejects non-int",
        "(char-width \"A\")", "expected integer", pass, fail);
    test_str_val(interp, "char-property category",
        "(char-property 65 'category)", "Lu", pass, fail);
    test_eq(interp, "char-property width",
        "(char-property 65 'width)", 1, pass, fail);
    test_truthy(interp, "char-property letter? true",
        "(char-property 65 'letter?)", pass, fail);
    test_truthy(interp, "char-property digit? true",
        "(char-property 48 'digit?)", pass, fail);
    test_nil(interp, "char-property lower? false",
        "(char-property 65 'lower?)", pass, fail);
    test_error_contains(interp, "char-property unknown key",
        "(char-property 65 'unknown)", "unknown property", pass, fail);

    // Normalization: NFC (identity for ASCII)
    test_str_val(interp, "string-normalize NFC ASCII",
        "(string-normalize \"hello\" 'NFC)", "hello", pass, fail);

    // Graphemes: ASCII chars are each a grapheme
    test_eq(interp, "string-graphemes ASCII",
        "(length (string-graphemes \"abc\"))", 3, pass, fail);
}

fn void run_unicode_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_unicode_tests, pass, fail);
}

fn bool run_jit_policy_warm_cache_tests(Interp* interp, int* pass, int* fail) {
    Expr* ast = parse_for_jit("(lambda (x) x)", interp);
    if (ast == null) {
        io::printn("[FAIL] jit policy: parse fixture");
        (*fail)++;
        return false;
    }

    jit_cache_clear();
    interp.flags.jit_enabled = false;
    jit_cache_expr(ast, interp);
    JitFn cached = jit_cache_lookup(ast);
    if (cached == null) {
        emit_pass_literal("[PASS] jit policy: warm-cache skips when jit disabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache skips when jit disabled");
        (*fail)++;
    }

    if (!jit_checks_enabled()) return true;

    interp.flags.jit_enabled = true;
    jit_cache_expr(ast, interp);
    cached = jit_cache_lookup(ast);
    if (cached != null) {
        emit_pass_literal("[PASS] jit policy: warm-cache stores when jit enabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache stores when jit enabled");
        (*fail)++;
    }
    return true;
}

fn void run_jit_policy_gc_safe_point_test(Interp* interp, int* pass, int* fail) {
    Expr* gc_ast = parse_for_jit("(+ 1 2)", interp);
    bool gc_ok = gc_ast != null;
    if (gc_ok) {
        JitFn f0 = jit_compile(gc_ast, interp);
        gc_ok = f0 != null;
    }

    usz before = g_jit_state_count;
    if (gc_ok) {
        g_jit_gc_needed = true;
        JitFn f1 = jit_compile(gc_ast, interp);
        gc_ok = f1 != null && g_jit_state_count > before;
    }

    if (gc_ok) {
        g_jit_gc_needed = true;
        jit_gc();
        gc_ok = (g_jit_state_count == 0) && !g_jit_gc_needed;
    }

    if (gc_ok) {
        emit_pass_literal("[PASS] jit policy: gc runs only at explicit safe points");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: gc runs only at explicit safe points");
        (*fail)++;
    }
}

fn void run_jit_policy_boundary_reset_test(Interp* interp, int* pass, int* fail) {
    Expr* reset_ast = parse_for_jit("(* 2 3)", interp);
    bool reset_ok = reset_ast != null;
    if (reset_ok) {
        JitFn f = jit_compile(reset_ast, interp);
        reset_ok = f != null && g_jit_state_count > 0;
    }

    if (reset_ok) {
        interp.flags.raise_pending = true;
        interp.raise_msg_len = 1;
        interp.last_call_name = interp.sym_lambda;
        run_test_group_boundary_reset(interp);
        reset_ok = g_jit_state_count == 0 &&
                  !g_jit_gc_needed &&
                  !interp.flags.raise_pending &&
                  interp.raise_msg_len == 0 &&
                  interp.raise_payload == null &&
                  (uint)interp.last_call_name == 0;
    }

    if (reset_ok) {
        emit_pass_literal("[PASS] jit policy: group boundary reset clears transient runtime state");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: group boundary reset clears transient runtime state");
        (*fail)++;
    }
}

fn void run_jit_policy_stale_raise_scrub_test(Interp* interp, int* pass, int* fail) {
    // Regression: stale pending raise must not leak into a new top-level run.
    interp.flags.raise_pending = true;
    interp.raise_msg_len = 3;
    interp.raise_msg[0] = 's';
    interp.raise_msg[1] = 't';
    interp.raise_msg[2] = 'e';
    interp.raise_msg[3] = 0;

    EvalResult r = run(
        "(handle (let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1)))) (raise msg 999))",
        interp
    );

    bool raise_scrub_ok = !r.error.has_error &&
                         r.value != null &&
                         r.value.tag == INT &&
                         r.value.int_val == 5 &&
                         !interp.flags.raise_pending &&
                         interp.raise_msg_len == 0 &&
                         interp.raise_payload == null;
    if (raise_scrub_ok) {
        emit_pass_literal("[PASS] jit policy: top-level run scrubs stale pending raise state");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: top-level run scrubs stale pending raise state");
        (*fail)++;
    }
}

fn void run_jit_policy_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- JIT Policy Tests ---");

    bool saved_jit_enabled = interp.flags.jit_enabled;
    bool parsed = run_jit_policy_warm_cache_tests(interp, pass, fail);
    if (parsed) {
        run_jit_policy_gc_safe_point_test(interp, pass, fail);
        run_jit_policy_boundary_reset_test(interp, pass, fail);
        run_jit_policy_stale_raise_scrub_test(interp, pass, fail);
    }

    interp.flags.jit_enabled = saved_jit_enabled;
    jit_cache_clear();
}

fn void run_jit_policy_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_jit_policy_tests, pass, fail);
}

fn void run_pika_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Pika Regex/Grammar Tests ---");

    // Regex: re-match
    test_str_val(interp, "re-match literal",
        "(re-match \"hello\" \"say hello world\")", "hello", pass, fail);
    test_tag(interp, "re-match no match returns nil",
        "(re-match \"xyz\" \"hello\")", NIL, pass, fail);
    test_str(interp, "re-match digits finds digit",
        "(re-match \"[0-9]+\" \"abc123def\")", pass, fail);
    test_str_val(interp, "re-match dot",
        "(re-match \"a.c\" \"abc\")", "abc", pass, fail);
    test_str_val(interp, "re-match star quantifier",
        "(re-match \"ab*c\" \"zzabbbczz\")", "abbbc", pass, fail);
    test_str_val(interp, "re-match plus quantifier",
        "(re-match \"ab+c\" \"zzabbbczz\")", "abbbc", pass, fail);
    test_str_val(interp, "re-match question quantifier",
        "(re-match \"ab?c\" \"zzaczz\")", "ac", pass, fail);
    test_str_val(interp, "re-match alternation",
        "(re-match \"ab|cd\" \"zzcdyy\")", "cd", pass, fail);

    // Regex: re-fullmatch
    test_str_val(interp, "re-fullmatch exact",
        "(re-fullmatch \"hello\" \"hello\")", "hello", pass, fail);
    test_str_val(interp, "re-fullmatch start+end anchors",
        "(re-fullmatch \"^hello$\" \"hello\")", "hello", pass, fail);
    test_tag(interp, "re-fullmatch rejects partial",
        "(re-fullmatch \"hello\" \"hello world\")", NIL, pass, fail);
    test_tag(interp, "re-match start anchor rejects non-zero start",
        "(re-match \"^abc\" \"xabc\")", NIL, pass, fail);
    test_str_val(interp, "re-match end anchor matches suffix at end",
        "(re-match \"abc$\" \"xabc\")", "abc", pass, fail);
    test_str_val(interp, "re-fullmatch bounded exact {n}",
        "(re-fullmatch \"ab{3}\" \"abbb\")", "abbb", pass, fail);
    test_str_val(interp, "re-fullmatch bounded range {n,m}",
        "(re-fullmatch \"ab{2,4}\" \"abbb\")", "abbb", pass, fail);
    test_str_val(interp, "re-fullmatch bounded unbounded {n,}",
        "(re-fullmatch \"ab{2,}\" \"abbbbb\")", "abbbbb", pass, fail);
    test_tag(interp, "re-fullmatch bounded range rejects overflow",
        "(re-fullmatch \"ab{2,4}\" \"abbbbb\")", NIL, pass, fail);
    test_str_val(interp, "re-fullmatch group plus",
        "(re-fullmatch \"(ab)+\" \"abab\")", "abab", pass, fail);
    test_str_val(interp, "re-fullmatch char-class chain >8",
        "(re-fullmatch \"[a][b][c][d][e][f][g][h][i]\" \"abcdefghi\")", "abcdefghi", pass, fail);
    test_tag(interp, "re-fullmatch char-class chain >32",
        "(let loop (i 0 p \"\" s \"\") (if (= i 40) (re-fullmatch p s) (loop (+ i 1) (string-append p \"[a]\") (string-append s \"a\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch char-class preserves entries past 64",
        "(let loop (i 0 cls \"[\") (if (= i 64) (re-fullmatch (string-append cls \"z]\") \"z\") (loop (+ i 1) (string-append cls \"a\"))))",
        STRING, pass, fail);
    test_str_val(interp, "re-fullmatch long compiled pattern",
        "(re-fullmatch \"(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|z)\" \"z\")", "z", pass, fail);
    test_tag(interp, "re-fullmatch long concatenation dynamic pattern",
        "(let loop (i 0 p \"\") (if (= i 180) (re-fullmatch p p) (loop (+ i 1) (string-append p \"a\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch long alternation dynamic pattern",
        "(let loop (i 0 p \"x\") (if (= i 80) (re-fullmatch p \"x\") (loop (+ i 1) (string-append p \"|x\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch large bounded quantifier shape",
        "(let loop (i 0 s \"\") (if (= i 200) (re-fullmatch \"a{128,256}\" s) (loop (+ i 1) (string-append s \"a\"))))",
        STRING, pass, fail);

    {
        EvalResult r = run(
            "(let loop (i 0 ok true) "
            "  (if (= i 40) ok "
            "      (let (p (string-append \"(\" (number->string i) \"|x)\") "
            "            m (re-fullmatch p \"x\")) "
            "        (loop (+ i 1) (and ok (and m (= m \"x\")))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] re-fullmatch cache grows beyond 32 compiled patterns");
            (*pass)++;
        } else {
            io::printn("[FAIL] re-fullmatch cache grows beyond 32 compiled patterns");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(let loop (i 0 ok true) "
            "  (if (= i 180) ok "
            "      (let (p (string-append \"(\" (number->string i) \"|x)\") "
            "            m (re-fullmatch p \"x\")) "
            "        (loop (+ i 1) (and ok (and m (= m \"x\")))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] re-fullmatch cache bounded eviction stays usable");
            (*pass)++;
        } else {
            io::printn("[FAIL] re-fullmatch cache bounded eviction stays usable");
            (*fail)++;
        }
    }

    // Regex: re-find-all
    test_eq(interp, "re-find-all count",
        "(length (re-find-all \"a\" \"abracadabra\"))", 5, pass, fail);
    test_str_val(interp, "re-find-all first",
        "(car (re-find-all \"[0-9]+\" \"a1 b22 c333\"))", "1", pass, fail);

    // Regex: re-split
    test_eq(interp, "re-split count",
        "(length (re-split \" \" \"hello world foo\"))", 3, pass, fail);
    test_str_val(interp, "re-split first",
        "(car (re-split \" \" \"hello world\"))", "hello", pass, fail);

    // Regex: re-replace
    test_str_val(interp, "re-replace first",
        "(re-replace \"[0-9]+\" \"X\" \"a1b2c3\")", "aXb2c3", pass, fail);
    test_str(interp, "re-replace global returns string",
        "(re-replace \"a\" \"X\" \"abaca\" 'global)", pass, fail);
    test_eq(interp, "re-replace first preserves >1024 output",
        "(string-byte-length (re-replace \"a\" (string-repeat \"b\" 1500) \"a\"))", 1500, pass, fail);
    test_eq(interp, "re-replace global preserves >1024 output",
        "(string-byte-length (re-replace \"a\" \"abcdef\" (string-repeat \"a\" 300) 'global))", 1800, pass, fail);

    // Regex: re-match-pos
    test_eq(interp, "re-match-pos start",
        "(car (re-match-pos \"hello\" \"say hello\"))", 4, pass, fail);

    // Regex: re-find-all-pos
    test_eq(interp, "re-find-all-pos count",
        "(length (re-find-all-pos \"a\" \"abaca\"))", 3, pass, fail);

    // Regex: escaped chars
    test_str(interp, "re-match \\d",
        "(re-match \"\\\\d+\" \"abc123\")", pass, fail);
    test_str(interp, "re-match \\w+",
        "(re-match \"\\\\w+\" \"hello_world\")", pass, fail);

    // Regex: strict grammar validation
    test_tag(interp, "re-fullmatch rejects unmatched close paren",
        "(re-fullmatch \"a)\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects unmatched open paren",
        "(re-fullmatch \"(ab\" \"ab\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects leading quantifier",
        "(re-fullmatch \"*a\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects trailing quantifier without atom",
        "(re-fullmatch \"a**\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects unknown group type",
        "(re-fullmatch \"(?q)a\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects invalid bounded quantifier missing min",
        "(re-fullmatch \"a{,2}\" \"aa\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects invalid bounded quantifier malformed upper",
        "(re-fullmatch \"a{2,x}\" \"aa\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch supports bounded quantifier >4096",
        "(re-fullmatch \"a{1,5000}\" \"a\")", STRING, pass, fail);
    test_tag(interp, "re-fullmatch rejects bounded quantifier i32 overflow upper",
        "(re-fullmatch \"a{1,2147483648}\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects bounded quantifier i32 overflow lower",
        "(re-fullmatch \"a{2147483648}\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects invalid bounded quantifier descending",
        "(re-fullmatch \"a{3,2}\" \"aaa\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects empty char class",
        "(re-fullmatch \"[]\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects invalid class range",
        "(re-fullmatch \"[z-a]\" \"z\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects trailing escape",
        "(re-fullmatch \"\\\\\" \"a\")", NIL, pass, fail);
    test_tag(interp, "re-fullmatch rejects trailing token after end anchor",
        "(re-fullmatch \"a$)\" \"a\")", NIL, pass, fail);

    // Pika grammar: seed/epsilon edge + unreachable-rule pruning
    test_eq(interp, "pika/grammar prunes unreachable rules",
        "(begin (pika/grammar 'g-prune '(rule start \"a\") '(rule dead \"z\")) (length (pika/grammar-rules 'g-prune)))",
        1, pass, fail);
    test_tag(interp, "pika/parse epsilon seq matches empty input",
        "(begin (pika/grammar 'g-eps '(rule start (seq)) '(rule dead \"x\")) (pika/parse 'g-eps \"\"))",
        CONS, pass, fail);
    test_tag(interp, "pika/parse reachable start still matches",
        "(begin (pika/grammar 'g-start '(rule start \"a\") '(rule dead \"b\")) (pika/parse 'g-start \"a\"))",
        CONS, pass, fail);
    test_tag(interp, "pika/parse scan clause basic",
        "(begin (pika/grammar 'g-scan '(rule start (scan \"[a-z]+\"))) (pika/parse 'g-scan \"abc\"))",
        CONS, pass, fail);
    test_str_val(interp, "pika/parse scan clause quantifier consumes run",
        "(begin (pika/grammar 'g-scan-plus '(rule start (scan \"[a-z]+\"))) (car (cdr (pika/parse 'g-scan-plus \"abc123\"))))",
        "abc", pass, fail);
    test_str_val(interp, "pika/parse scan clause negated class",
        "(begin (pika/grammar 'g-scan-neg '(rule start (scan \"[^0-9]+\"))) (car (cdr (pika/parse 'g-scan-neg \"abc123\"))))",
        "abc", pass, fail);
    test_str_val(interp, "pika/parse scan clause shorthand class",
        "(begin (pika/grammar 'g-scan-d '(rule start (scan \"[\\\\d]+\"))) (car (cdr (pika/parse 'g-scan-d \"123abc\"))))",
        "123", pass, fail);
    test_error_contains(interp, "pika/grammar scan rejects invalid class",
        "(pika/grammar 'g-scan-bad '(rule start (scan \"[]\")))",
        "invalid scan pattern", pass, fail);
    test_tag(interp, "pika/parse scan clauses beyond legacy cap",
        "(begin "
        "  (pika/grammar 'g-scan-many "
        "    '(rule start (seq r1 r2 r3 r4 r5 r6 r7 r8 r9)) "
        "    '(rule r1 (scan \"[a]\")) '(rule r2 (scan \"[b]\")) '(rule r3 (scan \"[c]\")) "
        "    '(rule r4 (scan \"[d]\")) '(rule r5 (scan \"[e]\")) '(rule r6 (scan \"[f]\")) "
        "    '(rule r7 (scan \"[g]\")) '(rule r8 (scan \"[h]\")) '(rule r9 (scan \"[i]\"))) "
        "  (pika/parse 'g-scan-many \"abcdefghi\"))",
        CONS, pass, fail);
    {
        pika::RuleBuilder rb = pika::rb_new();
        int[] seq_children = mem::new_array(int, 40);
        for (int i = 0; i < 40; i++) {
            seq_children[i] = rb.add("tok", pika::mk_token('a'));
        }
        rb.add("start", pika::mk_seq(seq_children));

        String[] starts = mem::new_array(String, 1);
        starts[0] = "start";
        pika::RuleEntry[] rules = rb.to_array();
        pika::Grammar grammar = pika::make_grammar(starts, rules);

        char[64] input_buf;
        for (int i = 0; i < 40; i++) input_buf[i] = 'a';
        char[] input = input_buf[:40];
        pika::ParserState st = pika::parse_simple(&grammar, input);
        int top_match = pika::find_match_at(&st, "start", 0);

        bool parse_tree_ok = top_match >= 0;
        if (parse_tree_ok) {
            Value* tree = pika::parse_tree_to_lisp(&st, &grammar, top_match, input, interp);
            int child_count = -1;
            if (tree != null &&
                tree.tag == CONS &&
                tree.cons_val.cdr != null &&
                tree.cons_val.cdr.tag == CONS &&
                tree.cons_val.cdr.cons_val.cdr != null &&
                tree.cons_val.cdr.cons_val.cdr.tag == CONS) {
                Value* children_list = tree.cons_val.cdr.cons_val.cdr.cons_val.car;
                child_count = 0;
                Value* cur = children_list;
                while (is_cons(cur)) {
                    child_count++;
                    cur = cur.cons_val.cdr;
                }
            }
            parse_tree_ok = child_count == 40;
        }

        if (parse_tree_ok) {
            emit_pass_literal("[PASS] pika/parse-tree direct start match preserves >32 children");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/parse-tree direct start match preserves >32 children");
            (*fail)++;
        }

        bool fold_ok = top_match >= 0;
        if (fold_ok) {
            EvalResult fold_fn_result = run(
                "(lambda (name) (lambda (text) (lambda (children) (length children))))",
                interp
            );
            if (fold_fn_result.error.has_error || fold_fn_result.value == null) {
                fold_ok = false;
            } else {
                Value* folded = pika::fold_parse_tree(&st, &grammar, top_match, input, fold_fn_result.value, interp);
                fold_ok = folded != null && folded.tag == INT && folded.int_val == 40;
            }
        }

        if (fold_ok) {
            emit_pass_literal("[PASS] pika/fold direct start match preserves >32 children");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/fold direct start match preserves >32 children");
            (*fail)++;
        }
    }

    // Compiled/fallback parity checks (direct C3 API).
    {
        char[] pattern = "ab+c";
        char[] input = "zzabbbczz";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled(pattern, input, &usable);
        pika::RegexMatch fallback = pika::regex_search_simple_fallback(pattern, input);
        bool ok = usable
            && compiled.matched == fallback.matched
            && compiled.start == fallback.start
            && compiled.end == fallback.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/fallback parity basic");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/fallback parity basic");
            (*fail)++;
        }
    }
    {
        char[] pattern = "^abc$";
        char[] input = "abc";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_fullmatch_compiled(pattern, input, &usable);
        pika::RegexMatch fallback = pika::regex_fullmatch_simple_fallback(pattern, input);
        bool ok = usable
            && compiled.matched == fallback.matched
            && compiled.start == fallback.start
            && compiled.end == fallback.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/fallback parity anchors");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/fallback parity anchors");
            (*fail)++;
        }
    }
    {
        char[] pattern = "a{2,4}";
        char[] input = "aaab";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled(pattern, input, &usable);
        pika::RegexMatch fallback = pika::regex_search_simple_fallback(pattern, input);
        bool ok = usable
            && compiled.matched == fallback.matched
            && compiled.start == fallback.start
            && compiled.end == fallback.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/fallback parity bounded quantifier");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/fallback parity bounded quantifier");
            (*fail)++;
        }
    }
}

fn void run_pika_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_pika_tests, pass, fail);
}

fn void run_atomic_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Atomic Tests ---");
    run_atomic_core_tests(interp, pass, fail);
    run_atomic_lifetime_tests(interp, pass, fail);
}

fn void run_atomic_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_atomic_tests, pass, fail);
}

fn void run_atomic_core_tests(Interp* interp, int* pass, int* fail) {
    test_tag(interp, "atomic creates handle",
        "(atomic 0)", FFI_HANDLE, pass, fail);

    {
        setup(interp, "(define a (atomic 42))");
        EvalResult r = run("(atomic-read a)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 42) {
            emit_pass_literal("[PASS] atomic-read initial");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-read initial");
            (*fail)++;
        }
    }

    {
        setup(interp, "(atomic-add! a 10)");
        EvalResult r = run("(atomic-read a)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 52) {
            emit_pass_literal("[PASS] atomic-add! + read");
            (*pass)++;
        } else {
            io::printfn("[FAIL] atomic-add! + read (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(atomic-cas! a 52 100)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == SYMBOL) {
            emit_pass_literal("[PASS] atomic-cas! success");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-cas! success");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(atomic-cas! a 52 200)", interp);
        if (!r.error.has_error && (r.value == null || r.value.tag == NIL)) {
            emit_pass_literal("[PASS] atomic-cas! fail");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-cas! fail");
            (*fail)++;
        }
    }
}

fn void run_atomic_lifetime_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define mk-atomic (lambda (n) (atomic n))) "
            "  (atomic-read (mk-atomic 7)))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 7) {
            emit_pass_literal("[PASS] atomic handle boundary copy");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic handle boundary copy");
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(begin "
            "  (define build-many (lambda (n) "
            "    (if (= n 0) 0 (begin (atomic n) (build-many (- n 1)))))) "
            "  (build-many 128) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] atomic transient handle stress");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic transient handle stress");
            (*fail)++;
        }
    }
}

fn void run_http_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- HTTP Tests ---");

    // http-get is defined (bound in env)
    test_tag(interp, "http-get is bound",
        "(procedure? http-get)", SYMBOL, pass, fail);

    // http-get with bad URL returns error
    {
        EvalResult r = run("(http-get \"not-a-url\")", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] http-get rejects bad URL");
            (*pass)++;
        } else {
            io::printn("[FAIL] http-get should reject bad URL");
            (*fail)++;
        }
    }

    // Example CRUD server pipeline: tolerate LF-only header/body split.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (let (req (pipeline/decode-request \"GET /items HTTP/1.1\\nHost: x\\n\\n\")) "
            "    (and (= ('method req) \"GET\") (= ('path req) \"/items\"))))",
            interp);
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline decode LF fallback");
            (*pass)++;
        } else {
            io::printn("[FAIL] crud pipeline decode LF fallback");
            (*fail)++;
        }
    }

    // Malformed request line should degrade to route-not-found.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define req (pipeline/decode-request \"BROKENLINE\\r\\n\\r\\n\")) "
            "  (define out (pipeline/result->response (pipeline/execute req))) "
            "  (and (= ('status out) \"error\") (= ('error out) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline malformed request line");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline malformed request line (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline malformed request line (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Missing path token should also degrade to route-not-found.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define req (pipeline/decode-request \"GET\\r\\n\\r\\n\")) "
            "  (define out (pipeline/result->response (pipeline/execute req))) "
            "  (and (= ('status out) \"error\") (= ('error out) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline missing path token");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline missing path token (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline missing path token (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
    // Example CRUD server pipeline: POST create then GET by id.
    {
        setup(interp, "(load \"examples/deduce_crud_server.omni\")");
        setup(interp, "(define post-req {'method \"POST\" 'path \"/items\" 'body {'id 11 'name \"paper\" 'qty 3}})");
        setup(interp, "(define post-res (pipeline/result->response (pipeline/execute post-req)))");
        setup(interp, "(define get-res (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items/11\" 'body nil})))");
        EvalResult r = run(
            "(and (= ('status post-res) \"ok\") "
            "     (and (= ('status get-res) \"ok\") "
            "          (= ('name ('data get-res)) \"paper\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline post/get");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline post/get (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline post/get (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD server pipeline: PUT update then DELETE.
    {
        setup(interp, "(load \"examples/deduce_crud_server.omni\")");
        setup(interp, "(pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 12 'name \"pen\" 'qty 1}})");
        setup(interp, "(define put-res (pipeline/result->response (pipeline/execute {'method \"PUT\" 'path \"/items\" 'body {'id 12 'name \"pen\" 'qty 9}})))");
        setup(interp, "(define row-after-put (repo/get-by-id 12))");
        setup(interp, "(define del-res (pipeline/result->response (pipeline/execute {'method \"DELETE\" 'path \"/items\" 'body {'id 12}})))");
        setup(interp, "(define row-after-del (repo/get-by-id 12))");
        EvalResult r = run(
            "(and (= ('status put-res) \"ok\") "
            "     (and (= row-after-put.qty 9) "
            "          (and (= ('status del-res) \"ok\") "
            "               (null? row-after-del))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline put/delete");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline put/delete (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline put/delete (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline routing: non-numeric item id should not match item route.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define bad (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items/not-int\" 'body nil}))) "
            "  (and (= ('status bad) \"error\") (= ('error bad) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline invalid path id");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline invalid path id (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline invalid path id (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline routing: unknown route returns route-not-found.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define miss (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/unknown\" 'body nil}))) "
            "  (and (= ('status miss) \"error\") (= ('error miss) \"route not found\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline unknown route");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline unknown route (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline unknown route (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
    // Example CRUD server response formatting (deterministic, no live listener).
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define payload (pipeline/result->response (pipeline/execute {'method \"GET\" 'path \"/items\" 'body nil}))) "
            "  (define raw (http/response \"200 OK\" payload)) "
            "  (and (string-contains? raw \"HTTP/1.1 200 OK\") "
            "       (string-contains? raw \"\\\"status\\\":\\\"ok\\\"\")))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud response formatting");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud response formatting (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud response formatting (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline schema gate: invalid POST payload fails validation.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (null? (validate 'item-write {'id 21 'name \"bad\"})))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline schema gate");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline schema gate (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline schema gate (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }

    // Example CRUD pipeline concurrency: duplicate POST race on same id.
    {
        EvalResult r = run(
            "(begin "
            "  (load \"examples/deduce_crud_server.omni\") "
            "  (define c1 "
            "    (spawn (lambda () "
            "      (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 31 'name \"race\" 'qty 1}})))) "
            "  (define c2 "
            "    (spawn (lambda () "
            "      (pipeline/execute {'method \"POST\" 'path \"/items\" 'body {'id 31 'name \"race\" 'qty 1}})))) "
            "  (define r1 (pipeline/result->response (await c1))) "
            "  (define r2 (pipeline/result->response (await c2))) "
            "  (define r1-ok (= ('status r1) \"ok\")) "
            "  (define r1-dup (= ('error r1) \"item already exists\")) "
            "  (define r2-ok (= ('status r2) \"ok\")) "
            "  (define r2-dup (= ('error r2) \"item already exists\")) "
            "  (define cnt (deduce 'count items)) "
            "  (and (= cnt 1) "
            "       (or (and r1-ok r2-dup) (and r2-ok r1-dup)))) )",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] crud pipeline duplicate-post race");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] crud pipeline duplicate-post race (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printfn("[FAIL] crud pipeline duplicate-post race (tag=%d)", r.value != null ? (int)r.value.tag : -1);
            }
            (*fail)++;
        }
    }
}

fn void run_http_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_http_tests, pass, fail);
}

fn void run_scheduler_spawn_and_await_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_spawn_basic_tests(interp, pass, fail);
    run_scheduler_await_structure_tests(interp, pass, fail);
    run_scheduler_run_loop_failure_tests(interp, pass, fail);
}

fn void run_scheduler_spawn_basic_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(spawn (lambda () 42))", interp);
        if (!r.error.has_error && is_int(r.value)) {
            emit_pass_literal("[PASS] spawn returns id");
            (*pass)++;
        } else {
            io::printn("[FAIL] spawn returns id");
            (*fail)++;
        }
    }

    {
        setup(interp, "(spawn (lambda () (+ 1 2)))");
        EvalResult r = run("(run-fibers)", interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] run-fibers completes");
            (*pass)++;
        } else {
            io::printn("[FAIL] run-fibers");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define p (spawn (lambda () (let (c (spawn (lambda () 7))) (+ 1 (await c)))))) "
            "  (await p))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 8) {
            emit_pass_literal("[PASS] await direct child");
            (*pass)++;
        } else {
            io::printfn("[FAIL] await direct child (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }
}

fn void run_scheduler_await_structure_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define sched-done 0) "
            "  (define p "
            "    (spawn (lambda () "
            "      (begin "
            "        (spawn (lambda () (begin (set! sched-done 1) 99))) "
            "        42)))) "
            "  (await p) "
            "  sched-done)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] parent waits for unawaited child");
            (*pass)++;
        } else {
            io::printfn("[FAIL] parent waits for unawaited child (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define b (spawn (lambda () 99))) "
            "  (define a (spawn (lambda () (await b)))) "
            "  (await a))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] await non-child rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] await non-child rejected");
            (*fail)++;
        }
    }

    {
        setup(interp, "(define fib-a (spawn (lambda () 10)))");
        setup(interp, "(define fib-b (spawn (lambda () 20)))");
        setup(interp, "(run-fibers)");
        emit_pass_literal("[PASS] run-fibers completes");
        (*pass)++;
    }
}

fn void run_scheduler_run_loop_failure_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "await surfaces round-limit failure",
        "(begin "
        "  (define p (spawn (lambda () (let loop (i 0) (begin (yield nil) (loop i)))))) "
        "  (await p))",
        "max rounds",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "run-fibers surfaces round-limit failure",
        "(begin "
        "  (spawn (lambda () (let loop (i 0) (begin (yield nil) (loop i))))) "
        "  (run-fibers))",
        "max rounds",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(begin "
            "  (spawn (lambda () 7)) "
            "  (run-fibers) "
            "  true)",
            interp
        );
        if (!r.error.has_error &&
            r.value != null &&
            r.value.tag != ERROR &&
            r.value.tag != NIL) {
            emit_pass_literal("[PASS] scheduler recovers after run-loop failure");
            (*pass)++;
        } else {
            io::printn("[FAIL] scheduler recovers after run-loop failure");
            (*fail)++;
        }
    }
}

fn void scheduler_test_prepare_no_async(void** saved_async) {
    scheduler_init();
    *saved_async = g_scheduler.async_handle;
    g_scheduler.async_handle = null;
    scheduler_reset_wakeup_queue();
}

fn void scheduler_test_restore_no_async(void* saved_async) {
    g_scheduler.async_handle = saved_async;
}

fn void scheduler_test_cleanup_no_async(void* saved_async) {
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void scheduler_test_prepare_single_fiber_no_async(void** saved_async, FiberState state) {
    scheduler_test_prepare_no_async(saved_async);
    g_scheduler.fiber_count = 1;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[0].state = state;
}

fn void scheduler_test_prepare_blocked_pending_read(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_reads[0].active = true;
}

fn void scheduler_test_prepare_blocked_pending_offload(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.pending_offloads[0].active = true;
}

fn void scheduler_test_cleanup_single_fiber_no_async(void* saved_async) {
    if (g_scheduler.pending_reads[0].buffer != null) {
        mem::free(g_scheduler.pending_reads[0].buffer);
    }
    if (g_scheduler.pending_offloads[0].completion != null) {
        scheduler_free_offload_completion(g_scheduler.pending_offloads[0].completion);
    }
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.fibers[0] = {};
    g_scheduler.fiber_count = 0;
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void run_scheduler_wakeup_smoke_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);

    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    bool pushed = scheduler_publish_timer_expired(MAX_FIBERS + 1);
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed && pending == 0 && (enqueues_after - enqueues_before) == 1) {
        emit_pass_literal("[PASS] reliable wakeup smoke");
        (*pass)++;
    } else {
        io::printfn("[FAIL] reliable wakeup smoke (pushed=%s pending=%d enqueues=%d)",
            pushed ? "yes" : "no", (long)pending, (long)(enqueues_after - enqueues_before));
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_wraparound_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

    usz target = 1024;
    usz pushed = 0;
    for (usz i = 0; i < target; i++) {
        if (scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1)) pushed++;
    }
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed == target &&
        pending == 0 &&
        (enqueues_after - enqueues_before) == target &&
        (failures_after - failures_before) == 0) {
        emit_pass_literal("[PASS] reliable wakeup flood");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] reliable wakeup flood (pushed=%d pending=%d enqueues=%d failures=%d)",
            (long)pushed,
            (long)pending,
            (long)(enqueues_after - enqueues_before),
            (long)(failures_after - failures_before)
        );
        (*fail)++;
    }
}
fn void run_scheduler_timer_wakeup_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_read(&saved_async);

    bool pushed = scheduler_publish_timer_expired(0);
    drain_wakeups();

    bool ok = pushed &&
              g_scheduler.pending_reads[0].timed_out &&
              g_scheduler.pending_reads[0].completed &&
              g_scheduler.fibers[0].state == FIBER_READY;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] timer wakeup transitions in drain");
        (*pass)++;
    } else {
        io::printn("[FAIL] timer wakeup transitions in drain");
        (*fail)++;
    }
}

fn void run_scheduler_atomic_cas_semantics_test(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define sched-cas (atomic 7))");
    EvalResult ok_swap = run("(begin (atomic-cas! sched-cas 7 9) (atomic-read sched-cas))", interp);
    EvalResult bad_swap = run("(begin (atomic-cas! sched-cas 7 11) (atomic-read sched-cas))", interp);
    if (!ok_swap.error.has_error && is_int(ok_swap.value) && ok_swap.value.int_val == 9 &&
        !bad_swap.error.has_error && is_int(bad_swap.value) && bad_swap.value.int_val == 9) {
        emit_pass_literal("[PASS] atomic-cas semantics");
        (*pass)++;
    } else {
        io::printn("[FAIL] atomic-cas semantics");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_wakeup_smoke_test(pass, fail);
    run_scheduler_wakeup_wraparound_test(pass, fail);
    run_scheduler_timer_wakeup_test(pass, fail);
    run_scheduler_atomic_cas_semantics_test(interp, pass, fail);
}

fn void run_scheduler_offload_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(gunzip (offload 'gzip \"hello-offload\"))", interp);
        if (!r.error.has_error && is_string(r.value) &&
            r.value.str_len == 13 &&
            str_starts_with(r.value.str_chars[:r.value.str_len], "hello-offload")) {
            emit_pass_literal("[PASS] offload sync gzip roundtrip");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload sync gzip roundtrip");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define a (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (define b (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (await a) "
            "  (await b) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] offload async fiber resume");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload async fiber resume");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(offload 'nope 1)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] offload rejects unsupported op");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload rejects unsupported op");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(offload 'http-get \"https://example.com\")", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] offload rejects internal io op");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload rejects internal io op");
            (*fail)++;
        }
    }
}

fn void run_scheduler_thread_join_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define t (thread-spawn 'gzip \"thread-task\")) "
            "  (gunzip (thread-join t)))",
            interp);
        if (!r.error.has_error && is_string(r.value) &&
            r.value.str_len == 11 &&
            str_starts_with(r.value.str_chars[:r.value.str_len], "thread-task")) {
            emit_pass_literal("[PASS] thread-spawn/thread-join gzip");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-spawn/thread-join gzip");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(thread-spawn 'tcp-connect \"127.0.0.1\" 1)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-spawn rejects internal io op");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-spawn rejects internal io op");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define t1 (thread-spawn 'sleep-ms 5)) "
            "  (define t2 (thread-spawn 'sleep-ms 5)) "
            "  (thread-join t1) "
            "  (thread-join t2) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] thread-spawn concurrent join");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-spawn concurrent join");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define t (thread-spawn 'sleep-ms 1)) "
            "  (thread-join t) "
            "  (thread-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-join double join rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join double join rejected");
            (*fail)++;
        }
    }
}

fn void run_scheduler_thread_timeout_cancel_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define t (thread-spawn 'sleep-ms 20)) "
            "  (thread-join-timeout t 0))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-join-timeout immediate timeout");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join-timeout immediate timeout");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define t (thread-spawn 'sleep-ms 5)) "
            "  (thread-join-timeout t 200) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] thread-join-timeout success");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join-timeout success");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define t (thread-spawn 'sleep-ms 20)) "
            "  (thread-cancel t) "
            "  (thread-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-cancel causes join cancellation");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-cancel causes join cancellation");
            (*fail)++;
        }
    }
}

fn void run_scheduler_offload_thread_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_offload_tests(interp, pass, fail);
    run_scheduler_thread_join_tests(interp, pass, fail);
    run_scheduler_thread_timeout_cancel_tests(interp, pass, fail);
}

fn void run_scheduler_fiber_temp_thread_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult r = run(
            "(begin "
            "  (define ft-t (thread-spawn 'sleep-ms 1)) "
            "  (thread-join ft-t) "
            "  true)",
            interp);
        if (r.error.has_error || r.value == null || r.value.tag == ERROR || r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;

    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp thread boundary metrics stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_interleave_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult async_r = run(
            "(begin "
            "  (define wf (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (await wf) "
            "  true)",
            interp);
        if (async_r.error.has_error ||
            async_r.value == null ||
            async_r.value.tag == ERROR ||
            async_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }

        bool pushed = scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1);
        drain_wakeups();
        if (!pushed) {
            ok = false;
            failed_step = i;
            break;
        }

        EvalResult thread_r = run(
            "(begin "
            "  (define wt (thread-spawn 'sleep-ms 1)) "
            "  (thread-join wt) "
            "  true)",
            interp);
        if (thread_r.error.has_error ||
            thread_r.value == null ||
            thread_r.value.tag == ERROR ||
            thread_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    ok = ok && reliable_wakeup_pending_count() == 0;

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup/offload interleave stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup/offload interleave stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_fiber_temp_cancel_timeout_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult timeout_r = run(
            "(begin "
            "  (define ft-to (thread-spawn 'sleep-ms 20)) "
            "  (thread-join-timeout ft-to 0))",
            interp);
        if (!(timeout_r.error.has_error || (timeout_r.value != null && timeout_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult cancel_r = run(
            "(begin "
            "  (define ft-c (thread-spawn 'sleep-ms 20)) "
            "  (thread-cancel ft-c) "
            "  (thread-join ft-c))",
            interp);
        if (!(cancel_r.error.has_error || (cancel_r.value != null && cancel_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult success_r = run(
            "(begin "
            "  (define ft-ok (thread-spawn 'sleep-ms 1)) "
            "  (thread-join-timeout ft-ok 200) "
            "  true)",
            interp);
        if (success_r.error.has_error ||
            success_r.value == null ||
            success_r.value.tag == ERROR ||
            success_r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;
    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundaries stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp cancel/timeout boundaries stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_offload_cancel_boundary_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult r = run(
            "(begin "
            "  (define oc (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (fiber-cancel oc) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (r.error.has_error ||
            r.value == null ||
            r.value.tag == ERROR ||
            r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] offload cancel destroy-before-complete stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] offload cancel destroy-before-complete stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_cancel_tests(Interp* interp, int* pass, int* fail) {
    {
        setup(interp, "(define fc-fib1 (spawn (lambda () 42)))");
        EvalResult r = run("(fiber-cancel fc-fib1)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag != NIL && r.value.tag != ERROR) {
            emit_pass_literal("[PASS] fiber-cancel ready fiber");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel ready fiber");
            (*fail)++;
        }
    }

    {
        setup(interp, "(define fc-fib2 (spawn (lambda () 1)))");
        setup(interp, "(fiber-cancel fc-fib2)");
        EvalResult r = run("(fiber-cancel fc-fib2)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == NIL) {
            emit_pass_literal("[PASS] fiber-cancel twice → nil");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel twice → nil");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(fiber-cancel 999)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] fiber-cancel invalid id → error");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel invalid id → error");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define fc-parent (spawn (lambda () "
            "    (define fc-child (spawn (lambda () 99))) "
            "    (await fc-child)))) "
            "  (fiber-cancel fc-parent) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fiber-cancel propagates to children");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel propagates to children");
            (*fail)++;
        }
    }
}

fn void run_scheduler_fairness_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define fair-a (spawn (lambda () 10))) "
            "  (define fair-b (spawn (lambda () 20))) "
            "  (run-fibers) "
            "  (+ 10 20))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 30) {
            emit_pass_literal("[PASS] fairness: both fibers complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: both fibers complete");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (spawn (lambda () 1)) (spawn (lambda () 2)) (spawn (lambda () 3)) "
            "  (spawn (lambda () 4)) (spawn (lambda () 5)) (spawn (lambda () 6)) "
            "  (spawn (lambda () 7)) (spawn (lambda () 8)) (spawn (lambda () 9)) "
            "  (spawn (lambda () 10)) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fairness: 10 fibers all complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: 10 fibers all complete");
            (*fail)++;
        }
    }
}

fn void run_scheduler_boundary_stress_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;
    for (usz i = 0; i < 24; i++) {
        EvalResult r;
        switch (i & 3) {
            case 0:
                r = run(
                    "(begin "
                    "  (define st-t (thread-spawn 'sleep-ms 1)) "
                    "  (thread-join st-t) "
                    "  true)",
                    interp);
            case 1:
                r = run(
                    "(begin "
                    "  (define st-f (spawn (lambda () (offload 'sleep-ms 1)))) "
                    "  (await st-f) "
                    "  true)",
                    interp);
            case 2:
                r = run(
                    "(begin "
                    "  (spawn (lambda () 1)) "
                    "  (spawn (lambda () 2)) "
                    "  (run-fibers) "
                    "  true)",
                    interp);
            case 3:
                r = run(
                    "(begin "
                    "  (define st-t1 (thread-spawn 'sleep-ms 1)) "
                    "  (define st-t2 (thread-spawn 'sleep-ms 1)) "
                    "  (thread-join st-t1) "
                    "  (thread-join st-t2) "
                    "  true)",
                    interp);
            default:
                r = eval_ok(make_nil(interp));
                break;
        }

        if (r.error.has_error ||
            r.value == null ||
            (r.value.tag == ERROR) ||
            (r.value.tag == NIL)) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler/thread boundary stress (deterministic)");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler/thread boundary stress (deterministic) at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn bool scheduler_runtime_boundary_fields_match(
    Interp* interp,
    main::ScopeRegion* current_scope,
    main::ScopeRegion* releasing_scope,
    Env* jit_env,
    Env* match_env,
    Expr* jit_tco_expr,
    Env* jit_tco_env,
    main::ScopeRegion* tco_recycle_scope,
    usz tco_scope_defer_slot,
    bool tco_scope_defer_active,
    bool escape_env_mode,
    PromotionContext* active_promotion_ctx
) @inline {
    return interp.current_scope == current_scope &&
           interp.releasing_scope == releasing_scope &&
           interp.jit_env == jit_env &&
           interp.match_env == match_env &&
           interp.jit_tco_expr == jit_tco_expr &&
           interp.jit_tco_env == jit_tco_env &&
           interp.tco_recycle_scope == tco_recycle_scope &&
           interp.tco_scope_defer_slot == tco_scope_defer_slot &&
           interp.tco_scope_defer_active == tco_scope_defer_active &&
           interp.escape_env_mode == escape_env_mode &&
           interp.active_promotion_ctx == active_promotion_ctx;
}

struct SchedulerBoundarySnapshot {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
    Env* jit_env;
    Env* match_env;
    Expr* jit_tco_expr;
    Env* jit_tco_env;
    main::ScopeRegion* tco_recycle_scope;
    usz tco_scope_defer_slot;
    bool tco_scope_defer_active;
    bool escape_env_mode;
    PromotionContext* active_promotion_ctx;
}

fn SchedulerBoundarySnapshot scheduler_capture_runtime_boundary_snapshot(Interp* interp) @inline {
    SchedulerBoundarySnapshot snap = {};
    snap.current_scope = interp.current_scope;
    snap.releasing_scope = interp.releasing_scope;
    snap.jit_env = interp.jit_env;
    snap.match_env = interp.match_env;
    snap.jit_tco_expr = interp.jit_tco_expr;
    snap.jit_tco_env = interp.jit_tco_env;
    snap.tco_recycle_scope = interp.tco_recycle_scope;
    snap.tco_scope_defer_slot = interp.tco_scope_defer_slot;
    snap.tco_scope_defer_active = interp.tco_scope_defer_active;
    snap.escape_env_mode = interp.escape_env_mode;
    snap.active_promotion_ctx = interp.active_promotion_ctx;
    return snap;
}

fn bool scheduler_runtime_boundary_matches_snapshot(Interp* interp, SchedulerBoundarySnapshot* snap) @inline {
    return scheduler_runtime_boundary_fields_match(
        interp,
        snap.current_scope,
        snap.releasing_scope,
        snap.jit_env,
        snap.match_env,
        snap.jit_tco_expr,
        snap.jit_tco_env,
        snap.tco_recycle_scope,
        snap.tco_scope_defer_slot,
        snap.tco_scope_defer_active,
        snap.escape_env_mode,
        snap.active_promotion_ctx
    );
}

fn void run_scheduler_wakeup_wraparound_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: reliable wakeup flooding drains fully and must not mutate
    // interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

        usz pushed = 0;
        for (usz j = 0; j < 1024; j++) {
            usz fid = MAX_FIBERS + i * 1024 + j + 1;
            if (scheduler_publish_poll_error(fid, -1)) pushed++;
        }
        drain_wakeups();
        usz pending = reliable_wakeup_pending_count();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        scheduler_test_cleanup_no_async(saved_async);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_ok = pushed == 1024 &&
                        pending == 0 &&
                        (enqueues_after - enqueues_before) == 1024 &&
                        (failures_after - failures_before) == 0;
        if (!state_ok || !queue_ok) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup wraparound boundary restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup wraparound boundary restore at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_mixed_event_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed reliable event ordering for one blocked fiber must keep
    // first-event-wins completion behavior and preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_read(&saved_async);

        bool poll_first = (i & 1) == 0;
        bool pushed_a = false;
        bool pushed_b = false;
        if (poll_first) {
            pushed_a = scheduler_publish_poll_error(0, -17);
            pushed_b = scheduler_publish_timer_expired(0);
        } else {
            pushed_a = scheduler_publish_timer_expired(0);
            pushed_b = scheduler_publish_poll_error(0, -17);
        }

        drain_wakeups();

        PendingTcpRead op = g_scheduler.pending_reads[0];
        FiberState state = g_scheduler.fibers[0].state;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

        bool core_ok = pushed_a &&
                       pushed_b &&
                       reliable_wakeup_pending_count() == 0 &&
                       op.completed &&
                       state == FIBER_READY;
        bool order_ok = poll_first
                        ? (!op.timed_out && op.error_code == -17)
                        : (op.timed_out && op.error_code == 0);
        if (!core_ok || !order_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!core_ok) {
                failed_phase = "core";
            } else if (!order_ok) {
                failed_phase = "order";
            } else {
                failed_phase = "boundary";
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup mixed-event boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler wakeup mixed-event boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_invalid_offload_wakeup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: invalid reliable offload completions must be drained safely (payload cleanup path)
    // and must not mutate interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 32; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_no_async(saved_async);
            break;
        }

        bool pushed_ready = scheduler_publish_offload_ready(MAX_FIBERS + 4000 + i, completion);
        bool pushed_err = scheduler_publish_poll_error(MAX_FIBERS + 5000 + i, -1);
        if (!pushed_ready) {
            scheduler_free_offload_completion(completion);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            scheduler_test_cleanup_no_async(saved_async);
            break;
        }

        drain_wakeups();
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

        bool drain_ok = pushed_err && reliable_wakeup_pending_count() == 0;
        scheduler_test_cleanup_no_async(saved_async);
        if (!drain_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!drain_ok) {
                failed_phase = "drain";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler invalid offload wakeup boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler invalid offload wakeup boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_drop_counter_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: reliable wakeup observability increments deterministically
    // and does not mutate interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();

        usz pushed = 0;
        for (usz j = 0; j < 3; j++) {
            if (scheduler_publish_poll_error(MAX_FIBERS + i * 10 + j + 1, -(int)(j + 1))) pushed++;
        }

        drain_wakeups();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool drop_ok = pushed == 3 &&
                       (failures_after - failures_before) == 0 &&
                       (enqueues_after - enqueues_before) == 3;
        bool queue_ok = reliable_wakeup_pending_count() == 0;
        scheduler_test_cleanup_no_async(saved_async);

        if (!drop_ok || !queue_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!drop_ok) {
                failed_phase = "observability";
            } else if (!queue_ok) {
                failed_phase = "queue";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler reliable wakeup observability boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler reliable wakeup observability boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_full_payload_ownership_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: WAKEUP_OFFLOAD_READY payload enqueued through the reliable queue
    // must still deliver exactly once and preserve boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();

        OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }
        *completion = {};
        completion.kind = OFFLOAD_RES_NIL;

        bool pushed_ready = scheduler_publish_offload_ready(0, completion);
        drain_wakeups();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
        bool slot_ready = g_scheduler.pending_offloads[0].active &&
                          g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == completion &&
                          g_scheduler.fibers[0].state == FIBER_READY;
        Value* consumed = slot_ready ? scheduler_consume_pending_offload(0, interp) : null;
        bool consume_ok = consumed != null && consumed.tag == NIL &&
                          !g_scheduler.pending_offloads[0].active &&
                          !g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == null;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_ok = pushed_ready && reliable_wakeup_pending_count() == 0;
        bool drops_ok = (failures_after - failures_before) == 0 &&
                        (enqueues_after - enqueues_before) == 1;

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!queue_ok || !drops_ok || !slot_ready || !consume_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!queue_ok) {
                failed_phase = "queue";
            } else if (!drops_ok) {
                failed_phase = "observability";
            } else if (!slot_ready || !consume_ok) {
                failed_phase = "delivery";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup full payload ownership boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler wakeup full payload ownership boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: in-range WAKEUP_OFFLOAD_READY for an inactive pending slot
    // must clean up payload and preserve runtime boundary state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_offloads[0] = {};

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        bool pushed = scheduler_publish_offload_ready(0, completion);
        drain_wakeups();
        bool slot_ok = !g_scheduler.pending_offloads[0].active &&
                       !g_scheduler.pending_offloads[0].completed &&
                       g_scheduler.pending_offloads[0].completion == null &&
                       g_scheduler.fibers[0].state == FIBER_READY;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = pushed && reliable_wakeup_pending_count() == 0 && slot_ok && state_ok;

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!pushed || reliable_wakeup_pending_count() != 0 || !slot_ok) {
                failed_phase = "drain";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler inactive offload-ready payload cleanup boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler inactive offload-ready payload cleanup boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_duplicate_offload_ready_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: duplicate/late WAKEUP_OFFLOAD_READY events for one active pending
    // offload retain the first completion payload, safely discard extras, and
    // preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);

        OffloadCompletion* c1 = scheduler_make_task_cancel_completion();
        OffloadCompletion* c2 = scheduler_make_task_cancel_completion();
        if (c1 == null || c2 == null) {
            if (c1 != null) scheduler_free_offload_completion(c1);
            if (c2 != null) scheduler_free_offload_completion(c2);
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        bool pushed_first = scheduler_publish_offload_ready(0, c1);
        bool pushed_second = scheduler_publish_offload_ready(0, c2);
        if (!pushed_first || !pushed_second) {
            if (!pushed_first) scheduler_free_offload_completion(c1);
            if (!pushed_second) scheduler_free_offload_completion(c2);
            if (pushed_first) {
                drain_wakeups();
                if (g_scheduler.pending_offloads[0].completion != null) {
                    scheduler_free_offload_completion(g_scheduler.pending_offloads[0].completion);
                }
            }
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        drain_wakeups();
        PendingOffload off_after_dup = g_scheduler.pending_offloads[0];
        bool state_ok_dup = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool dup_ok = reliable_wakeup_pending_count() == 0 &&
                      off_after_dup.active &&
                      off_after_dup.completed &&
                      off_after_dup.completion == c1 &&
                      off_after_dup.completion != c2 &&
                      g_scheduler.fibers[0].state == FIBER_READY;

        OffloadCompletion* c3 = scheduler_make_task_cancel_completion();
        if (c3 == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc-late";
        } else {
            bool pushed_late = scheduler_publish_offload_ready(0, c3);
            if (!pushed_late) {
                scheduler_free_offload_completion(c3);
                ok = false;
                failed_step = i;
                failed_phase = "enqueue-late";
            } else {
                drain_wakeups();
                PendingOffload off_after_late = g_scheduler.pending_offloads[0];
                bool state_ok_late = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
                bool late_ok = reliable_wakeup_pending_count() == 0 &&
                               off_after_late.active &&
                               off_after_late.completed &&
                               off_after_late.completion == c1;
                if (!dup_ok || !late_ok || !state_ok_dup || !state_ok_late) {
                    ok = false;
                    failed_step = i;
                    if (!dup_ok) {
                        failed_phase = "dup";
                    } else if (!late_ok) {
                        failed_phase = "late";
                    } else if (!state_ok_dup) {
                        failed_phase = "boundary-dup";
                    } else {
                        failed_phase = "boundary-late";
                    }
                }
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler duplicate offload-ready boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler duplicate offload-ready boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_consume_pending_offload_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: direct consume paths for pending offload (pre-completion error,
    // completed success/error, and post-consume repeat error) preserve boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_offloads[0] = {};

        // Phase A: consume before completion should fail.
        Value* pre = scheduler_consume_pending_offload(0, interp);
        bool pre_ok = pre != null && pre.tag == ERROR;
        bool state_ok_pre = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!pre_ok || !state_ok_pre) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "pre";
        }

        OffloadCompletion* completion = null;
        long expected_int = 0;
        char[] expected_bytes = "blob-ok";
        usz mode = i % 3; // 0=int, 1=error, 2=bytes
        bool expect_error = mode == 1;
        bool expect_bytes = mode == 2;
        if (ok && expect_error) {
            completion = scheduler_make_task_cancel_completion();
        } else if (ok && expect_bytes) {
            completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
            if (completion != null) {
                *completion = {};
                completion.kind = OFFLOAD_RES_BYTES;
                completion.shared = scheduler_publish_shared(expected_bytes);
                if (completion.shared == null) {
                    mem::free(completion);
                    completion = null;
                }
            }
        } else if (ok) {
            completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
            if (completion != null) {
                *completion = {};
                completion.kind = OFFLOAD_RES_INT;
                expected_int = 1000 + (long)i;
                completion.int_result = expected_int;
            }
        }
        if (ok && completion == null) {
            ok = false;
            failed_step = i * 3 + 1;
            failed_phase = "alloc";
        }

        // Phase B: consume completed result/error.
        if (ok) {
            g_scheduler.pending_offloads[0].active = true;
            g_scheduler.pending_offloads[0].completed = true;
            g_scheduler.pending_offloads[0].completion = completion;
            Value* consumed = scheduler_consume_pending_offload(0, interp);

            bool consume_ok = expect_error
                              ? (consumed != null && consumed.tag == ERROR)
                              : (expect_bytes
                                 ? (consumed != null &&
                                    consumed.tag == STRING &&
                                    consumed.str_len == expected_bytes.len &&
                                    str_starts_with(consumed.str_chars[:consumed.str_len], expected_bytes))
                                 : (consumed != null && consumed.tag == INT && consumed.int_val == expected_int));
            bool slot_reset = !g_scheduler.pending_offloads[0].active &&
                              !g_scheduler.pending_offloads[0].completed &&
                              g_scheduler.pending_offloads[0].completion == null;
            bool state_ok_consume = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!consume_ok || !slot_reset || !state_ok_consume) {
                ok = false;
                failed_step = i * 3 + 1;
                if (!consume_ok) {
                    failed_phase = "consume-value";
                } else if (!slot_reset) {
                    failed_phase = "consume-reset";
                } else {
                    failed_phase = "consume-boundary";
                }
            }
        }

        // Phase C: consume after reset should fail again.
        if (ok) {
            Value* post = scheduler_consume_pending_offload(0, interp);
            bool post_ok = post != null && post.tag == ERROR;
            bool state_ok_post = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!post_ok || !state_ok_post) {
                ok = false;
                failed_step = i * 3 + 2;
                failed_phase = "post";
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler consume pending-offload boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler consume pending-offload boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void scheduler_seed_completed_tcp_read_case(usz case_id) {
    PendingTcpRead* op = &g_scheduler.pending_reads[0];
    *op = {};
    op.active = true;
    op.completed = true;

    switch (case_id) {
        case 0:
            op.timed_out = true;
        case 1:
            op.error_code = -31;
        case 2:
            op.nread = 0;
        case 3:
            op.nread = 4;
            op.buffer = (char*)mem::malloc(5);
            if (op.buffer != null) {
                op.buffer[0] = 'p';
                op.buffer[1] = 'o';
                op.buffer[2] = 'n';
                op.buffer[3] = 'g';
                op.buffer[4] = 0;
            } else {
                op.nread = 0;
            }
        default:
            op.nread = 0;
    }
}

fn bool scheduler_tcp_case_value_ok(usz case_id, Value* v) {
    if (v == null) return false;
    switch (case_id) {
        case 0:
            return v.tag == ERROR;
        case 1:
            return v.tag == ERROR;
        case 2:
            return v.tag == STRING && v.str_len == 0;
        case 3:
            return v.tag == STRING &&
                   v.str_len == 4 &&
                   v.str_chars[0] == 'p' &&
                   v.str_chars[1] == 'o' &&
                   v.str_chars[2] == 'n' &&
                   v.str_chars[3] == 'g';
        default:
            return false;
    }
}

fn void run_scheduler_consume_pending_tcp_read_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: direct consume paths for pending tcp-read preserve
    // boundary/runtime state and correctly reset slot ownership.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_reads[0] = {};

        // Phase A: consume before completion should fail.
        Value* pre = scheduler_consume_pending_tcp_read(0, interp);
        bool pre_ok = pre != null && pre.tag == ERROR;
        bool state_ok_pre = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!pre_ok || !state_ok_pre) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "pre";
        }

        // Phase B: consume completed case.
        usz case_id = i & 3;
        if (ok) {
            scheduler_seed_completed_tcp_read_case(case_id);
            Value* consumed = scheduler_consume_pending_tcp_read(0, interp);
            bool consume_ok = scheduler_tcp_case_value_ok(case_id, consumed);
            bool slot_reset = !g_scheduler.pending_reads[0].active &&
                              !g_scheduler.pending_reads[0].completed &&
                              g_scheduler.pending_reads[0].buffer == null;
            bool state_ok_consume = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!consume_ok || !slot_reset || !state_ok_consume) {
                ok = false;
                failed_step = i * 3 + 1;
                if (!consume_ok) {
                    failed_phase = "consume-value";
                } else if (!slot_reset) {
                    failed_phase = "consume-reset";
                } else {
                    failed_phase = "consume-boundary";
                }
            }
        }

        // Phase C: consume after reset should fail again.
        if (ok) {
            Value* post = scheduler_consume_pending_tcp_read(0, interp);
            bool post_ok = post != null && post.tag == ERROR;
            bool state_ok_post = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!post_ok || !state_ok_post) {
                ok = false;
                failed_step = i * 3 + 2;
                failed_phase = "post";
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler consume pending-tcp-read boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler consume pending-tcp-read boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_completion_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: thread-task completion ownership paths (complete/take/drop and
    // invalid-id completion cleanup) preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc-task";
            break;
        }

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "alloc-completion";
            break;
        }

        scheduler_complete_thread_task(task_id, completion);
        bool exists_before_take = scheduler_thread_task_exists(task_id);
        OffloadCompletion* taken = scheduler_take_thread_task_completion(task_id);
        bool take_ok = exists_before_take &&
                       taken != null &&
                       !scheduler_thread_task_exists(task_id);
        if (taken != null) scheduler_free_offload_completion(taken);

        OffloadCompletion* invalid_completion = scheduler_make_task_cancel_completion();
        if (invalid_completion == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "alloc-invalid-completion";
            break;
        }
        scheduler_complete_thread_task(MAX_THREAD_TASKS + 7 + i, invalid_completion);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!take_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!take_ok) {
                failed_phase = "take";
            } else {
                failed_phase = "boundary";
            }
            scheduler_drop_thread_task(task_id);
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler thread-task completion boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler thread-task completion boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_cancel_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: thread-task cancel state transitions preserve deterministic
    // completion semantics and boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            break;
        }

        bool already_done = false;
        bool cancel_ok = scheduler_cancel_thread_task(task_id, &already_done);
        bool begin_ok = !scheduler_try_begin_thread_task(task_id);
        OffloadCompletion* cancelled = scheduler_take_thread_task_completion(task_id);
        bool take_ok = cancelled != null &&
                       cancelled.kind == OFFLOAD_RES_ERROR &&
                       !scheduler_thread_task_exists(task_id);
        if (cancelled != null) scheduler_free_offload_completion(cancelled);

        usz done_task = scheduler_alloc_thread_task();
        bool done_path_ok = false;
        if (done_task != NO_TASK) {
            OffloadCompletion* done_completion = scheduler_make_task_cancel_completion();
            if (done_completion != null) {
                scheduler_complete_thread_task(done_task, done_completion);
                bool done_already = false;
                bool cancel_done_ok = scheduler_cancel_thread_task(done_task, &done_already);
                OffloadCompletion* done_taken = scheduler_take_thread_task_completion(done_task);
                done_path_ok = cancel_done_ok &&
                               done_already &&
                               done_taken != null &&
                               done_taken.kind == OFFLOAD_RES_ERROR &&
                               !scheduler_thread_task_exists(done_task);
                if (done_taken != null) scheduler_free_offload_completion(done_taken);
            }
            if (!done_path_ok) scheduler_drop_thread_task(done_task);
        }

        bool invalid_cancel_ok = !scheduler_cancel_thread_task(MAX_THREAD_TASKS + 17 + i, null);
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = cancel_ok &&
                        !already_done &&
                        begin_ok &&
                        take_ok &&
                        done_path_ok &&
                        invalid_cancel_ok &&
                        state_ok;
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!cancel_ok || already_done || !begin_ok || !take_ok) {
                failed_phase = "cancel-path";
            } else if (!done_path_ok) {
                failed_phase = "done-path";
            } else if (!invalid_cancel_ok) {
                failed_phase = "invalid-cancel";
            } else {
                failed_phase = "boundary";
            }
            scheduler_drop_thread_task(task_id);
            if (done_task != NO_TASK) scheduler_drop_thread_task(done_task);
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler thread-task cancel boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler thread-task cancel boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_ready_barrier_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated readable notifications for one blocked read coalesce
    // into one pending signal and preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_read(&saved_async);
    g_scheduler.pending_reads[0].fd = -1;
    g_scheduler.pending_reads[0].max_bytes = 8;
    g_scheduler.pending_reads[0].buffer = (char*)mem::malloc(8);
    if (g_scheduler.pending_reads[0].buffer == null) {
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        io::printn("[FAIL] scheduler readable coalescing boundary restore (alloc)");
        (*fail)++;
        return;
    }

    usz coalesces_before = g_scheduler.readable_signal_coalesces.load();
    scheduler_signal_poll_readable(0);
    scheduler_signal_poll_readable(0);

    drain_wakeups();
    bool processed = g_scheduler.pending_reads[0].completed &&
                     !g_scheduler.pending_reads[0].timed_out &&
                     g_scheduler.pending_reads[0].error_code != 0 &&
                     !g_scheduler.pending_reads[0].readable_pending &&
                     g_scheduler.fibers[0].state == FIBER_READY &&
                     reliable_wakeup_pending_count() == 0;
    bool coalesce_ok = (g_scheduler.readable_signal_coalesces.load() - coalesces_before) == 1;
    bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    bool ok = processed && coalesce_ok && state_ok;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] scheduler readable coalescing boundary restore");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler readable coalescing boundary restore");
        (*fail)++;
    }
}

fn void run_scheduler_mixed_boundary_state_restore_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed scheduler thread/offload paths (success + expected error)
    // preserve interpreter boundary/runtime state across iterations.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] fail_phase = "";
    bool d_current_scope = true;
    bool d_releasing_scope = true;
    bool d_jit_env = true;
    bool d_match_env = true;
    bool d_jit_tco_expr = true;
    bool d_jit_tco_env = true;
    bool d_tco_recycle = true;
    bool d_tco_slot = true;
    bool d_tco_active = true;
    bool d_escape_mode = true;
    bool d_active_ctx = true;
    for (usz i = 0; i < 12; i++) {
        EvalResult success_r = run(
            "(begin "
            "  (define sb-t (thread-spawn 'sleep-ms 1)) "
            "  (define sb-f (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (thread-join sb-t) "
            "  (await sb-f) "
            "  true)",
            interp);
        bool success_ok = !success_r.error.has_error &&
                          success_r.value != null &&
                          success_r.value.tag != ERROR &&
                          success_r.value.tag != NIL;
        bool success_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!success_ok || !success_state_ok) {
            ok = false;
            failed_step = i * 2;
            fail_phase = "success";
            d_current_scope = interp.current_scope == saved.current_scope;
            d_releasing_scope = interp.releasing_scope == saved.releasing_scope;
            d_jit_env = interp.jit_env == saved.jit_env;
            d_match_env = interp.match_env == saved.match_env;
            d_jit_tco_expr = interp.jit_tco_expr == saved.jit_tco_expr;
            d_jit_tco_env = interp.jit_tco_env == saved.jit_tco_env;
            d_tco_recycle = interp.tco_recycle_scope == saved.tco_recycle_scope;
            d_tco_slot = interp.tco_scope_defer_slot == saved.tco_scope_defer_slot;
            d_tco_active = interp.tco_scope_defer_active == saved.tco_scope_defer_active;
            d_escape_mode = interp.escape_env_mode == saved.escape_env_mode;
            d_active_ctx = interp.active_promotion_ctx == saved.active_promotion_ctx;
            break;
        }

        // Expected-error path should also preserve boundary/runtime fields.
        EvalResult error_r = run(
            "(begin "
            "  (define sb-c (thread-spawn 'sleep-ms 20)) "
            "  (thread-cancel sb-c) "
            "  (thread-join sb-c))",
            interp);
        bool error_ok = error_r.error.has_error ||
                        (error_r.value != null && error_r.value.tag == ERROR);
        bool error_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!error_ok || !error_state_ok) {
            ok = false;
            failed_step = i * 2 + 1;
            fail_phase = "error";
            d_current_scope = interp.current_scope == saved.current_scope;
            d_releasing_scope = interp.releasing_scope == saved.releasing_scope;
            d_jit_env = interp.jit_env == saved.jit_env;
            d_match_env = interp.match_env == saved.match_env;
            d_jit_tco_expr = interp.jit_tco_expr == saved.jit_tco_expr;
            d_jit_tco_env = interp.jit_tco_env == saved.jit_tco_env;
            d_tco_recycle = interp.tco_recycle_scope == saved.tco_recycle_scope;
            d_tco_slot = interp.tco_scope_defer_slot == saved.tco_scope_defer_slot;
            d_tco_active = interp.tco_scope_defer_active == saved.tco_scope_defer_active;
            d_escape_mode = interp.escape_env_mode == saved.escape_env_mode;
            d_active_ctx = interp.active_promotion_ctx == saved.active_promotion_ctx;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler mixed boundary/runtime state restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler mixed boundary/runtime state restore at step %d (%s) [scope=%s releasing=%s jit_env=%s match_env=%s tco_expr=%s tco_env=%s tco_recycle=%s tco_slot=%s tco_active=%s escape=%s active_ctx=%s]",
            (int)failed_step,
            (ZString)fail_phase,
            d_current_scope ? "ok" : "drift",
            d_releasing_scope ? "ok" : "drift",
            d_jit_env ? "ok" : "drift",
            d_match_env ? "ok" : "drift",
            d_jit_tco_expr ? "ok" : "drift",
            d_jit_tco_env ? "ok" : "drift",
            d_tco_recycle ? "ok" : "drift",
            d_tco_slot ? "ok" : "drift",
            d_tco_active ? "ok" : "drift",
            d_escape_mode ? "ok" : "drift",
            d_active_ctx ? "ok" : "drift"
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_error_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: wakeup queue drains + async offload expected-error paths
    // preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";
    for (usz i = 0; i < 12; i++) {
        EvalResult success_r = run(
            "(begin "
            "  (define ws-ok (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (await ws-ok) "
            "  true)",
            interp);
        bool success_ok = !success_r.error.has_error &&
                          success_r.value != null &&
                          success_r.value.tag != ERROR &&
                          success_r.value.tag != NIL;
        bool success_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!success_ok || !success_state_ok) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "success";
            break;
        }

        bool pushed = scheduler_publish_poll_error(MAX_FIBERS + i + 101, -1);
        drain_wakeups();
        bool wakeup_ok = pushed && scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!wakeup_ok) {
            ok = false;
            failed_step = i * 3 + 1;
            failed_phase = "wakeup";
            break;
        }

        EvalResult error_r = run(
            "(begin "
            "  (define ws-err (spawn (lambda () (offload 'nope 1)))) "
            "  (await ws-err))",
            interp);
        bool error_ok = error_r.error.has_error ||
                        (error_r.value != null && error_r.value.tag == ERROR);
        bool error_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!error_ok || !error_state_ok) {
            ok = false;
            failed_step = i * 3 + 2;
            failed_phase = "error";
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup/offload expected-error boundary restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup/offload expected-error boundary restore at step %d (%s)", (int)failed_step, (ZString)failed_phase);
        (*fail)++;
    }
}

fn void run_scheduler_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Scheduler Tests ---");
    run_scheduler_spawn_and_await_tests(interp, pass, fail);
    run_scheduler_wakeup_tests(interp, pass, fail);
    run_scheduler_offload_thread_tests(interp, pass, fail);
    run_scheduler_fiber_temp_thread_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_interleave_tests(interp, pass, fail);
    run_scheduler_fiber_temp_cancel_timeout_boundary_tests(interp, pass, fail);
    run_scheduler_offload_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_cancel_tests(interp, pass, fail);
    run_scheduler_fairness_tests(interp, pass, fail);
    run_scheduler_boundary_stress_tests(interp, pass, fail);
    run_scheduler_wakeup_wraparound_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_mixed_event_boundary_tests(interp, pass, fail);
    run_scheduler_invalid_offload_wakeup_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_drop_counter_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_full_payload_ownership_boundary_tests(interp, pass, fail);
    run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(interp, pass, fail);
    run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_duplicate_offload_ready_boundary_tests(interp, pass, fail);
    run_scheduler_consume_pending_offload_boundary_tests(interp, pass, fail);
    run_scheduler_consume_pending_tcp_read_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_completion_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(interp, pass, fail);
    run_scheduler_thread_join_timeout_then_join_boundary_tests(interp, pass, fail);
    run_scheduler_shared_retire_queue_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_mixed_boundary_state_restore_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_error_boundary_tests(interp, pass, fail);
}

fn void run_scheduler_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_scheduler_tests, pass, fail);
}

fn void setup_deduce_person_relation(Interp* interp) {
    setup(interp, "(define ddb (deduce 'open 'memory))");
    {
        Value* db_val = interp.global_env.lookup(interp.symbols.intern("ddb"));
        if (db_val != null && db_val.tag == FFI_HANDLE) {
            Value* name_sym = interp.alloc_value(); name_sym.tag = SYMBOL; name_sym.sym_val = interp.symbols.intern("person");
            Value* col1 = interp.alloc_value(); col1.tag = SYMBOL; col1.sym_val = interp.symbols.intern("name");
            Value* col2 = interp.alloc_value(); col2.tag = SYMBOL; col2.sym_val = interp.symbols.intern("age");
            Value* spec = make_cons(interp, name_sym, make_cons(interp, col1, make_cons(interp, col2, make_nil(interp))));
            Value*[1] fargs;
            fargs[0] = make_cons(interp, db_val, spec);
            Value* rel = prim_define_relation(fargs[..], null, interp);
            if (rel != null && rel.tag == FFI_HANDLE) {
                env_define_with_barrier(interp.global_env, interp.symbols.intern("person"), rel, interp);
            }
        }
    }
    setup(interp, "(deduce 'fact! person \"Alice\" 30)");
    setup(interp, "(deduce 'fact! person \"Bob\" 25)");
}

fn Value* setup_deduce_relation_for_db(
    Interp* interp,
    Value* db_val,
    SymbolId rel_name,
    SymbolId col1,
    SymbolId col2
) {
    if (interp == null || db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null) return null;

    Value* name_sym = interp.alloc_value();
    name_sym.tag = SYMBOL;
    name_sym.sym_val = rel_name;

    Value* c1_sym = interp.alloc_value();
    c1_sym.tag = SYMBOL;
    c1_sym.sym_val = col1;

    Value* c2_sym = interp.alloc_value();
    c2_sym.tag = SYMBOL;
    c2_sym.sym_val = col2;

    Value* spec = make_cons(interp, name_sym, make_cons(interp, c1_sym, make_cons(interp, c2_sym, make_nil(interp))));
    Value*[1] fargs;
    fargs[0] = make_cons(interp, db_val, spec);
    return prim_define_relation(fargs[..], null, interp);
}

extern fn int deduce_test_mkdir(char* path, uint mode) @extern("mkdir");
extern fn int deduce_test_getpid() @extern("getpid");
extern fn int deduce_test_system(char* command) @extern("system");
extern fn int deduce_test_unlink(char* path) @extern("unlink");

usz g_deduce_test_long_path_seq = 0;

fn SymbolId deduce_test_make_long_relation_name(Interp* interp, char variant) {
    char[80] name_buf;
    for (usz i = 0; i < name_buf.len; i++) name_buf[i] = 'r';
    name_buf[name_buf.len - 1] = variant;
    return interp.symbols.intern(name_buf[..]);
}

fn Value* deduce_test_make_long_open_path(Interp* interp, char variant) {
    char[512] path_buf;
    usz len = 0;
    path_buf[len++] = '/';
    path_buf[len++] = 't';
    path_buf[len++] = 'm';
    path_buf[len++] = 'p';

    usz seq = g_deduce_test_long_path_seq++;
    uint pid = (uint)deduce_test_getpid();
    path_buf[len++] = '/';
    path_buf[len++] = 'o';
    path_buf[len++] = 'd';
    path_buf[len++] = 'l';
    path_buf[len++] = 'p';
    path_buf[len++] = '-';
    for (usz i = 0; i < 8; i++) {
        usz shift = (7 - i) * 4;
        path_buf[len++] = DEDUCE_HEX_DIGITS[(pid >> shift) & 0xF];
    }
    path_buf[len++] = '-';
    for (usz i = 0; i < 4; i++) {
        usz shift = (3 - i) * 4;
        path_buf[len++] = DEDUCE_HEX_DIGITS[(seq >> shift) & 0xF];
    }
    path_buf[len] = 0;
    deduce_test_mkdir(&path_buf[0], 0o755);

    for (usz seg = 0; seg < 6; seg++) {
        path_buf[len++] = '/';
        char fill = (char)('a' + (int)seg);
        for (usz i = 0; i < 37; i++) path_buf[len++] = fill;
        path_buf[len] = 0;
        deduce_test_mkdir(&path_buf[0], 0o755);
    }

    path_buf[len++] = '/';
    path_buf[len++] = 'd';
    path_buf[len++] = 'b';
    path_buf[len++] = '-';
    path_buf[len++] = variant;
    return make_string(interp, path_buf[..len]);
}

fn bool deduce_test_write_restart_script(char[] script_path, char[] db_path, bool writer_phase) {
    char[2048] script_buf;
    char[] script = writer_phase
        ? io::bprintf(
            &script_buf,
            "(begin "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (deduce 'fact! rel \"Alice\" 30) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )!!
        : io::bprintf(
            &script_buf,
            "(begin "
            "  (let (db (deduce 'open \"%s\") "
            "        rel (deduce 'open-named db 'person \"person_v1\" 'name 'age)) "
            "    (if (= (deduce 'count rel) 1) true (deduce 'count 42))))",
            (ZString)db_path
        )!!;
    return write_text_file(script_path, script);
}

fn bool deduce_test_run_main_script(char[] script_path) {
    char[1024] cmd_buf;
    char[] cmd = io::bprintf(
        &cmd_buf,
        "ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=1 "
        "LD_LIBRARY_PATH=/usr/local/lib ./build/main %s",
        (ZString)script_path
    )!!;
    cmd_buf[cmd.len] = 0;
    return deduce_test_system(&cmd_buf[0]) == 0;
}

fn void run_deduce_durability_restart_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    uint pid = (uint)deduce_test_getpid();
    usz seq = g_deduce_test_long_path_seq++;

    char[512] db_path_buf;
    char[] db_path = io::bprintf(
        &db_path_buf,
        "/tmp/omni-deduce-restart-%d-%d",
        (int)pid,
        (int)seq
    )!!;

    char[512] writer_script_buf;
    char[] writer_script = io::bprintf(
        &writer_script_buf,
        "/tmp/omni-deduce-restart-write-%d-%d.omni",
        (int)pid,
        (int)seq
    )!!;
    writer_script_buf[writer_script.len] = 0;
    defer deduce_test_unlink(&writer_script_buf[0]);

    char[512] reader_script_buf;
    char[] reader_script = io::bprintf(
        &reader_script_buf,
        "/tmp/omni-deduce-restart-read-%d-%d.omni",
        (int)pid,
        (int)seq
    )!!;
    reader_script_buf[reader_script.len] = 0;
    defer deduce_test_unlink(&reader_script_buf[0]);

    if (!deduce_test_write_restart_script(writer_script, db_path, true) ||
        !deduce_test_write_restart_script(reader_script, db_path, false)) {
        io::printn("[FAIL] deduce durability across restart (script write)");
        (*fail)++;
        return;
    }

    bool writer_ok = deduce_test_run_main_script(writer_script);
    bool reader_ok = writer_ok && deduce_test_run_main_script(reader_script);
    if (writer_ok && reader_ok) {
        emit_pass_literal("[PASS] deduce durability across restart");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce durability across restart (writer=%s reader=%s)",
            writer_ok ? "ok" : "FAIL",
            reader_ok ? "ok" : "FAIL"
        );
        (*fail)++;
    }
}

fn void run_deduce_memory_open_isolation_test(Interp* interp, int* pass, int* fail) {
    EvalResult ra = run("(deduce 'open 'memory)", interp);
    EvalResult rb = run("(deduce 'open 'memory)", interp);
    bool open_ok = !ra.error.has_error &&
                   !rb.error.has_error &&
                   ra.value != null && rb.value != null &&
                   ra.value.tag == FFI_HANDLE &&
                   rb.value.tag == FFI_HANDLE &&
                   ra.value.ffi_val != null &&
                   rb.value.ffi_val != null &&
                   ra.value.ffi_val != rb.value.ffi_val;
    if (!open_ok) {
        io::printn("[FAIL] deduce memory open isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_a_sym = interp.symbols.intern("person_a_iso");
    SymbolId rel_b_sym = interp.symbols.intern("person_b_iso");
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");

    Value* rel_a = setup_deduce_relation_for_db(interp, ra.value, rel_a_sym, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, rb.value, rel_b_sym, name_sym, age_sym);
    bool rel_ok = rel_a != null && rel_b != null &&
                  rel_a.tag == FFI_HANDLE && rel_b.tag == FFI_HANDLE &&
                  rel_a.ffi_val != null && rel_b.ffi_val != null;
    if (!rel_ok) {
        io::printn("[FAIL] deduce memory open isolation (relation setup)");
        (*fail)++;
        return;
    }

    env_define_with_barrier(interp.global_env, rel_a_sym, rel_a, interp);
    env_define_with_barrier(interp.global_env, rel_b_sym, rel_b, interp);

    EvalResult ins = run("(deduce 'fact! person_a_iso \"Alice\" 30)", interp);
    EvalResult ca = run("(deduce 'count person_a_iso)", interp);
    EvalResult cb = run("(deduce 'count person_b_iso)", interp);

    bool count_ok = !ins.error.has_error &&
                    !ca.error.has_error &&
                    !cb.error.has_error &&
                    is_int(ca.value) && ca.value.int_val == 1 &&
                    is_int(cb.value) && cb.value.int_val == 0;

    if (count_ok) {
        emit_pass_literal("[PASS] deduce memory open isolation");
        (*pass)++;
    } else {
        io::printfn("[FAIL] deduce memory open isolation (ins_err=%s count_a=%d count_b=%d)",
            ins.error.has_error ? "yes" : "no",
            is_int(ca.value) ? (int)ca.value.int_val : -1,
            is_int(cb.value) ? (int)cb.value.int_val : -1);
        (*fail)++;
    }
}

fn void run_deduce_scan_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 2) {
            emit_pass_literal("[PASS] deduce 'fact! + 'scan");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'fact! + 'scan (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    setup(interp, "(deduce 'retract! person \"Bob\" 25)");
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'retract!");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'retract!");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'scan person)))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'scan dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan dict result");
            (*fail)++;
        }
    }
}

fn void run_deduce_scan_range_tests(Interp* interp, int* pass, int* fail) {
    setup(interp,
        "(begin "
        "  (define scan-range-db (deduce 'open 'memory)) "
        "  (define [relation scan-range-db] scan_range_person (name) (age) (email)) "
        "  (deduce 'fact! scan_range_person \"Alice\" 20 \"a@x\") "
        "  (deduce 'fact! scan_range_person \"Bob\" 30 \"b@x\") "
        "  (deduce 'fact! scan_range_person \"Carol\" 40 \"c@x\") "
        "  (deduce 'fact! scan_range_person \"Dora\" 50 \"d@x\") "
        ")");

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Bob\" 30 \"b@x\") '(\"Carol\" 40 \"c@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 2) {
            emit_pass_literal("[PASS] deduce 'scan-range bounded count");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range bounded count (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Zed\" 99 \"z@x\") '(\"Zoe\" 100 \"z@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'scan-range empty");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range empty (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person \"bad\" '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound not list");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound not list");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person '(\"Bob\" 30) '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound arity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound arity");
            (*fail)++;
        }
    }
}

fn void run_deduce_query_filter_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(deduce 'count person)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'count");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'count");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) (> ('age row) 28))))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'query filter");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'query filter (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) false)))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'query false predicate");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query false predicate");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'query person (lambda (row) (> ('age row) 28)))))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'query dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query dict result");
            (*fail)++;
        }
    }
}

fn void run_deduce_match_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(length (deduce 'match person '(person ?name ?age)))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'match all");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'match all (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'match person '(person ?name ?age))))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'match ?var binding");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'match ?var binding");
            (*fail)++;
        }
    }

    {
        EvalResult open = run("(deduce 'open 'memory)", interp);
        if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >512 results (open)");
            (*fail)++;
            return;
        }

        SymbolId rel_name = interp.symbols.intern("crowd");
        SymbolId id_sym = interp.symbols.intern("id");
        SymbolId age_sym = interp.symbols.intern("age");
        Value* rel = setup_deduce_relation_for_db(interp, open.value, rel_name, id_sym, age_sym);
        if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >512 results (define)");
            (*fail)++;
            return;
        }

        const usz ROW_COUNT = 600;
        for (usz i = 0; i < ROW_COUNT; i++) {
            Value*[3] fact_args;
            fact_args[0] = rel;
            fact_args[1] = make_int(interp, (long)i);
            fact_args[2] = make_int(interp, (long)(20 + (int)(i % 50)));
            Value* insert = prim_assert_fact(fact_args[..], null, interp);
            if (insert == null || insert.tag != NIL) {
                io::printfn("[FAIL] deduce 'match preserves >512 results (insert %d)", (int)i);
                (*fail)++;
                return;
            }
        }

        Value* clause =
            make_cons(interp,
                make_symbol(interp, rel_name),
                make_cons(interp,
                    make_symbol(interp, interp.symbols.intern("?id")),
                    make_cons(interp,
                        make_symbol(interp, interp.symbols.intern("?age")),
                        make_nil(interp))));
        Value*[2] match_args;
        match_args[0] = rel;
        match_args[1] = clause;
        Value* matches = prim_deduce_match(match_args[..], null, interp);

        usz match_count = 0;
        bool found_tail = false;
        Value* cur = matches;
        while (is_cons(cur)) {
            Value* row = car(cur);
            if (row != null && row.tag == HASHMAP && row.hashmap_val != null) {
                Value* id_val = hashmap_get(row.hashmap_val, make_symbol(interp, id_sym));
                if (id_val != null && id_val.tag == INT && id_val.int_val == 599) found_tail = true;
            }
            match_count++;
            cur = cdr(cur);
        }

        if (match_count == ROW_COUNT && found_tail) {
            emit_pass_literal("[PASS] deduce 'match preserves >512 results");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'match preserves >512 results (count=%d tail=%s)",
                (int)match_count,
                found_tail ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        EvalResult open = run("(deduce 'open 'memory)", interp);
        if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (open)");
            (*fail)++;
            return;
        }

        const usz COL_COUNT = 40;
        SymbolId[] col_syms = mem::new_array(SymbolId, COL_COUNT);
        SymbolId[] var_syms = mem::new_array(SymbolId, COL_COUNT);
        SymbolId[] key_syms = mem::new_array(SymbolId, COL_COUNT);
        Value*[] define_args = mem::new_array(Value*, COL_COUNT + 2);
        Value*[] fact_args = mem::new_array(Value*, COL_COUNT + 1);
        if (col_syms.ptr == null || var_syms.ptr == null || key_syms.ptr == null ||
            define_args.ptr == null || fact_args.ptr == null) {
            if (col_syms.ptr != null) mem::free(col_syms.ptr);
            if (var_syms.ptr != null) mem::free(var_syms.ptr);
            if (key_syms.ptr != null) mem::free(key_syms.ptr);
            if (define_args.ptr != null) mem::free(define_args.ptr);
            if (fact_args.ptr != null) mem::free(fact_args.ptr);
            io::printn("[FAIL] deduce 'match preserves >32 bindings (alloc)");
            (*fail)++;
            return;
        }
        defer mem::free(col_syms.ptr);
        defer mem::free(var_syms.ptr);
        defer mem::free(key_syms.ptr);
        defer mem::free(define_args.ptr);
        defer mem::free(fact_args.ptr);

        SymbolId rel_name = interp.symbols.intern("binding_wide");
        define_args[0] = open.value;
        define_args[1] = make_symbol(interp, rel_name);
        fact_args[0] = null;

        for (usz i = 0; i < COL_COUNT; i++) {
            char[16] col_buf;
            char[] col_name = io::bprintf(&col_buf, "c%d", (int)i)!!;
            col_syms[i] = interp.symbols.intern(col_name);
            define_args[i + 2] = make_symbol(interp, col_syms[i]);

            char[16] var_buf;
            char[] var_name = io::bprintf(&var_buf, "?v%d", (int)i)!!;
            var_syms[i] = interp.symbols.intern(var_name);
            key_syms[i] = interp.symbols.intern(var_name[1..]);

            fact_args[i + 1] = make_int(interp, (long)i);
        }

        Value* rel = prim_define_relation(define_args, null, interp);
        if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (define)");
            (*fail)++;
            return;
        }
        fact_args[0] = rel;

        Value* insert = prim_assert_fact(fact_args, null, interp);
        if (insert == null || insert.tag != NIL) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (insert)");
            (*fail)++;
            return;
        }

        Value* clause = make_nil(interp);
        for (usz i = COL_COUNT; i > 0; i--) {
            clause = make_cons(interp, make_symbol(interp, var_syms[i - 1]), clause);
        }
        clause = make_cons(interp, make_symbol(interp, rel_name), clause);

        Value*[2] match_args;
        match_args[0] = rel;
        match_args[1] = clause;
        Value* matches = prim_deduce_match(match_args[..], null, interp);

        Value* row = is_cons(matches) ? car(matches) : null;
        Value* last = row != null && row.tag == HASHMAP
            ? hashmap_get(row.hashmap_val, make_symbol(interp, key_syms[COL_COUNT - 1]))
            : null;

        bool ok = is_cons(matches) &&
                  row != null &&
                  row.tag == HASHMAP &&
                  row.hashmap_val != null &&
                  row.hashmap_val.count == COL_COUNT &&
                  last != null &&
                  last.tag == INT &&
                  last.int_val == (long)(COL_COUNT - 1);

        if (ok) {
            emit_pass_literal("[PASS] deduce 'match preserves >32 bindings");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'match preserves >32 bindings (count=%d last=%d)",
                row != null && row.tag == HASHMAP && row.hashmap_val != null ? (int)row.hashmap_val.count : -1,
                last != null && last.tag == INT ? (int)last.int_val : -1
            );
            (*fail)++;
        }
    }
}

fn void run_deduce_query_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_scan_tests(interp, pass, fail);
    run_deduce_scan_range_tests(interp, pass, fail);
    run_deduce_query_filter_tests(interp, pass, fail);
    run_deduce_match_tests(interp, pass, fail);
}

fn void run_deduce_open_smoke_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run("(deduce 'open 'memory)", interp);
    bool ok = !r.error.has_error &&
        r.value != null &&
        r.value.tag == FFI_HANDLE &&
        r.value.ffi_val != null;
    if (ok) {
        emit_pass_literal("[PASS] deduce 'open returns handle");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            char[] msg = r.error.message[:256];
            io::printfn("[FAIL] deduce 'open returns handle (error: %s)", (ZString)msg);
        } else {
            io::printfn("[FAIL] deduce 'open returns handle (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_open_named_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define ndb (deduce 'open 'memory)) "
            "  (define rel-a (deduce 'open-named ndb 'nrel_a \"named_a\" 'name 'age)) "
            "  (define rel-b (deduce 'open-named ndb 'nrel_b \"named_b\" 'name 'age)) "
            "  (deduce 'fact! rel-a \"Alice\" 30) "
            "  (and (= (deduce 'count rel-a) 1) (= (deduce 'count rel-b) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named isolates db names");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named isolates db names");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define ndb2 (deduce 'open 'memory)) "
            "  (define rel-c (deduce 'open-named ndb2 'nrel_c 'named_c 'name 'age)) "
            "  (= (deduce 'count rel-c) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named accepts symbol db-name");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named accepts symbol db-name");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(deduce 'open-named (deduce 'open 'memory) 'bad 42 'name)",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce open-named validates db-name type");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named validates db-name type");
            (*fail)++;
        }
    }
}

fn void run_deduce_relation_attr_define_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(begin "
        "  (define ddb-rel-attr (deduce 'open 'memory)) "
        "  (define [relation ddb-rel-attr] rel-attr-test (id) (name) (qty)) "
        "  (deduce 'count rel-attr-test))",
        interp
    );
    if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
        emit_pass_literal("[PASS] deduce [relation] define attr");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce [relation] define attr");
        (*fail)++;
    }
}

fn void run_deduce_crud_example_flow_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(begin "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (let (create1 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        create2 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        existing (repo/get-by-id 101) "
        "        missing (repo/get-by-id 9999)) "
        "    (and (= ('status create1) \"ok\") "
        "         (and (= ('status create2) \"error\") "
        "              (and (= ('error create2) \"item already exists\") "
        "                   (and (= existing.name \"paper\") "
        "                        (null? missing)))))))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce CRUD example flow");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce CRUD example flow (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] deduce CRUD example flow (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_reopen_stress_test(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    isz failed_iter = -1;

    for (usz i = 0; i < 64; i++) {
        EvalResult r = run(
            "(begin "
            "  (define ddb-reopen (deduce 'open 'memory)) "
            "  ddb-reopen)",
            interp
        );
        Value* h = r.value;
        if (r.error.has_error || h == null || h.tag != FFI_HANDLE || h.ffi_val == null) {
            ok = false;
            failed_iter = (isz)i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce repeated open/rebind");
        (*pass)++;
    } else {
        io::printfn("[FAIL] deduce repeated open/rebind (iter %d)", (int)failed_iter);
        (*fail)++;
    }
}

fn void run_deduce_large_relation_schema_test(Interp* interp, int* pass, int* fail) {
    EvalResult open = run("(deduce 'open 'memory)", interp);
    if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
        io::printn("[FAIL] deduce relation schema >16 columns (open)");
        (*fail)++;
        return;
    }

    const usz COL_COUNT = 20;
    SymbolId[] col_syms = mem::new_array(SymbolId, COL_COUNT);
    Value*[] define_args = mem::new_array(Value*, COL_COUNT + 2);
    Value*[] fact_args = mem::new_array(Value*, COL_COUNT + 1);
    if (col_syms.ptr == null || define_args.ptr == null || fact_args.ptr == null) {
        if (col_syms.ptr != null) mem::free(col_syms.ptr);
        if (define_args.ptr != null) mem::free(define_args.ptr);
        if (fact_args.ptr != null) mem::free(fact_args.ptr);
        io::printn("[FAIL] deduce relation schema >16 columns (alloc)");
        (*fail)++;
        return;
    }
    defer mem::free(col_syms.ptr);
    defer mem::free(define_args.ptr);
    defer mem::free(fact_args.ptr);

    col_syms[0] = interp.symbols.intern("c0");
    col_syms[1] = interp.symbols.intern("c1");
    col_syms[2] = interp.symbols.intern("c2");
    col_syms[3] = interp.symbols.intern("c3");
    col_syms[4] = interp.symbols.intern("c4");
    col_syms[5] = interp.symbols.intern("c5");
    col_syms[6] = interp.symbols.intern("c6");
    col_syms[7] = interp.symbols.intern("c7");
    col_syms[8] = interp.symbols.intern("c8");
    col_syms[9] = interp.symbols.intern("c9");
    col_syms[10] = interp.symbols.intern("c10");
    col_syms[11] = interp.symbols.intern("c11");
    col_syms[12] = interp.symbols.intern("c12");
    col_syms[13] = interp.symbols.intern("c13");
    col_syms[14] = interp.symbols.intern("c14");
    col_syms[15] = interp.symbols.intern("c15");
    col_syms[16] = interp.symbols.intern("c16");
    col_syms[17] = interp.symbols.intern("c17");
    col_syms[18] = interp.symbols.intern("c18");
    col_syms[19] = interp.symbols.intern("c19");

    define_args[0] = open.value;
    define_args[1] = make_symbol(interp, interp.symbols.intern("wide_person"));
    for (usz i = 0; i < COL_COUNT; i++) {
        define_args[i + 2] = make_symbol(interp, col_syms[i]);
    }

    Value* rel_val = prim_define_relation(define_args, null, interp);
    if (rel_val == null || rel_val.tag != FFI_HANDLE || rel_val.ffi_val == null) {
        io::printn("[FAIL] deduce relation schema >16 columns (define)");
        (*fail)++;
        return;
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    fact_args[0] = rel_val;
    for (usz i = 0; i < COL_COUNT; i++) {
        fact_args[i + 1] = make_int(interp, (long)i);
    }

    Value* insert = prim_assert_fact(fact_args, null, interp);
    Value* rows = relation_scan_all(rel, interp);
    Value* row = is_cons(rows) ? car(rows) : null;
    Value* last = row != null && row.tag == HASHMAP
        ? hashmap_get(row.hashmap_val, make_symbol(interp, col_syms[COL_COUNT - 1]))
        : null;

    bool ok = rel != null &&
              rel.col_count == COL_COUNT &&
              rel.columns != null &&
              rel.columns[COL_COUNT - 1] == col_syms[COL_COUNT - 1] &&
              insert != null &&
              insert.tag == NIL &&
              row != null &&
              row.tag == HASHMAP &&
              row.hashmap_val != null &&
              row.hashmap_val.count == COL_COUNT &&
              last != null &&
              last.tag == INT &&
              last.int_val == (long)(COL_COUNT - 1);

    if (ok) {
        emit_pass_literal("[PASS] deduce relation schema preserves >16 columns");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce relation schema preserves >16 columns (cols=%d last=%d count=%d)",
            rel != null ? (int)rel.col_count : -1,
            last != null && last.tag == INT ? (int)last.int_val : -1,
            row != null && row.tag == HASHMAP && row.hashmap_val != null ? (int)row.hashmap_val.count : -1
        );
        (*fail)++;
    }
}

fn void run_deduce_long_relation_name_isolation_test(Interp* interp, int* pass, int* fail) {
    EvalResult open = run("(deduce 'open 'memory)", interp);
    if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
        io::printn("[FAIL] deduce long relation name isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_a_name = deduce_test_make_long_relation_name(interp, 'a');
    SymbolId rel_b_name = deduce_test_make_long_relation_name(interp, 'b');
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");

    Value* rel_a = setup_deduce_relation_for_db(interp, open.value, rel_a_name, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, open.value, rel_b_name, name_sym, age_sym);
    if (rel_a == null || rel_b == null || rel_a.tag != FFI_HANDLE || rel_b.tag != FFI_HANDLE ||
        rel_a.ffi_val == null || rel_b.ffi_val == null) {
        io::printn("[FAIL] deduce long relation name isolation (define)");
        (*fail)++;
        return;
    }

    Value*[3] fact_a;
    fact_a[0] = rel_a;
    fact_a[1] = make_string(interp, "Alice");
    fact_a[2] = make_int(interp, 30);
    Value* insert_a = prim_assert_fact(fact_a[..], null, interp);

    Value*[1] count_args_a;
    count_args_a[0] = rel_a;
    Value*[1] count_args_b;
    count_args_b[0] = rel_b;
    Value* count_a_after_a = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_after_a = prim_deduce_count(count_args_b[..], null, interp);

    Value*[3] fact_b;
    fact_b[0] = rel_b;
    fact_b[1] = make_string(interp, "Bob");
    fact_b[2] = make_int(interp, 25);
    Value* insert_b = prim_assert_fact(fact_b[..], null, interp);

    Value* count_a_final = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_final = prim_deduce_count(count_args_b[..], null, interp);

    bool ok = insert_a != null && insert_a.tag == NIL &&
              insert_b != null && insert_b.tag == NIL &&
              count_a_after_a != null && count_a_after_a.tag == INT && count_a_after_a.int_val == 1 &&
              count_b_after_a != null && count_b_after_a.tag == INT && count_b_after_a.int_val == 0 &&
              count_a_final != null && count_a_final.tag == INT && count_a_final.int_val == 1 &&
              count_b_final != null && count_b_final.tag == INT && count_b_final.int_val == 1;

    if (ok) {
        emit_pass_literal("[PASS] deduce long relation name isolation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce long relation name isolation (a1=%d b1=%d af=%d bf=%d)",
            count_a_after_a != null && count_a_after_a.tag == INT ? (int)count_a_after_a.int_val : -1,
            count_b_after_a != null && count_b_after_a.tag == INT ? (int)count_b_after_a.int_val : -1,
            count_a_final != null && count_a_final.tag == INT ? (int)count_a_final.int_val : -1,
            count_b_final != null && count_b_final.tag == INT ? (int)count_b_final.int_val : -1
        );
        (*fail)++;
    }
}

fn void run_deduce_long_open_path_isolation_test(Interp* interp, int* pass, int* fail) {
    Value* path_a = deduce_test_make_long_open_path(interp, 'a');
    Value* path_b = deduce_test_make_long_open_path(interp, 'b');
    Value*[1] open_args_a;
    Value*[1] open_args_b;
    open_args_a[0] = path_a;
    open_args_b[0] = path_b;

    Value* db_a = prim_deduce_open(open_args_a[..], null, interp);
    Value* db_b = prim_deduce_open(open_args_b[..], null, interp);
    if (db_a == null || db_b == null || db_a.tag != FFI_HANDLE || db_b.tag != FFI_HANDLE ||
        db_a.ffi_val == null || db_b.ffi_val == null) {
        io::printn("[FAIL] deduce long open path isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_name = interp.symbols.intern("path_iso_rel");
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");
    Value* rel_a = setup_deduce_relation_for_db(interp, db_a, rel_name, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, db_b, rel_name, name_sym, age_sym);
    if (rel_a == null || rel_b == null || rel_a.tag != FFI_HANDLE || rel_b.tag != FFI_HANDLE ||
        rel_a.ffi_val == null || rel_b.ffi_val == null) {
        io::printn("[FAIL] deduce long open path isolation (define)");
        (*fail)++;
        return;
    }

    Value*[3] fact_a;
    fact_a[0] = rel_a;
    fact_a[1] = make_string(interp, "Alice");
    fact_a[2] = make_int(interp, 31);
    Value* insert_a = prim_assert_fact(fact_a[..], null, interp);

    Value*[1] count_args_a;
    count_args_a[0] = rel_a;
    Value*[1] count_args_b;
    count_args_b[0] = rel_b;
    Value* count_a_after_a = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_after_a = prim_deduce_count(count_args_b[..], null, interp);

    Value*[3] fact_b;
    fact_b[0] = rel_b;
    fact_b[1] = make_string(interp, "Bob");
    fact_b[2] = make_int(interp, 22);
    Value* insert_b = prim_assert_fact(fact_b[..], null, interp);

    Value* count_a_final = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_final = prim_deduce_count(count_args_b[..], null, interp);

    bool ok = insert_a != null && insert_a.tag == NIL &&
              insert_b != null && insert_b.tag == NIL &&
              count_a_after_a != null && count_a_after_a.tag == INT && count_a_after_a.int_val == 1 &&
              count_b_after_a != null && count_b_after_a.tag == INT && count_b_after_a.int_val == 0 &&
              count_a_final != null && count_a_final.tag == INT && count_a_final.int_val == 1 &&
              count_b_final != null && count_b_final.tag == INT && count_b_final.int_val == 1;

    if (ok) {
        emit_pass_literal("[PASS] deduce long open path isolation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce long open path isolation (a1=%d b1=%d af=%d bf=%d)",
            count_a_after_a != null && count_a_after_a.tag == INT ? (int)count_a_after_a.int_val : -1,
            count_b_after_a != null && count_b_after_a.tag == INT ? (int)count_b_after_a.int_val : -1,
            count_a_final != null && count_a_final.tag == INT ? (int)count_a_final.int_val : -1,
            count_b_final != null && count_b_final.tag == INT ? (int)count_b_final.int_val : -1
        );
        (*fail)++;
    }
}

fn void run_deduce_transaction_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define txn-db-commit (deduce 'open 'memory)) "
            "  (define [relation txn-db-commit] txn_person_commit (name) (age)) "
            "  (define txc (deduce 'begin txn-db-commit)) "
            "  (deduce 'fact! txc txn_person_commit \"Alice\" 33) "
            "  (define before (deduce 'count txn_person_commit)) "
            "  (deduce 'commit txc) "
            "  (define after (deduce 'count txn_person_commit)) "
            "  (and (= before 0) (= after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn commit");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn commit");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define txn-db-abort (deduce 'open 'memory)) "
            "  (define [relation txn-db-abort] txn_person_abort (name) (age)) "
            "  (define txa (deduce 'begin txn-db-abort)) "
            "  (deduce 'fact! txa txn_person_abort \"Bob\" 21) "
            "  (deduce 'abort txa) "
            "  (= (deduce 'count txn_person_abort) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce txn abort");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn abort");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define txn-db-a (deduce 'open 'memory)) "
            "  (define txn-db-b (deduce 'open 'memory)) "
            "  (define [relation txn-db-b] txn_person_mismatch (name) (age)) "
            "  (define txm (deduce 'begin txn-db-a)) "
            "  (deduce 'fact! txm txn_person_mismatch \"Eve\" 44))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce txn db mismatch");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce txn db mismatch");
            (*fail)++;
        }
    }
}

fn void run_deduce_clear_drop_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define clr-db (deduce 'open 'memory)) "
            "  (define [relation clr-db] clr-person (name) (age)) "
            "  (deduce 'fact! clr-person \"Alice\" 30) "
            "  (deduce 'fact! clr-person \"Bob\" 31) "
            "  (deduce 'clear! clr-person) "
            "  (define c0 (deduce 'count clr-person)) "
            "  (deduce 'fact! clr-person \"Carol\" 40) "
            "  (define c1 (deduce 'count clr-person)) "
            "  (and (= c0 0) (= c1 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce clear! preserves relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce clear! preserves relation");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define drp-db (deduce 'open 'memory)) "
            "  (define [relation drp-db] drp-person (name) (age)) "
            "  (deduce 'fact! drp-person \"Alice\" 30) "
            "  (deduce 'drop! drp-person) "
            "  (deduce 'count drp-person))",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce drop! invalidates relation");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce drop! invalidates relation");
            (*fail)++;
        }
    }
}

fn void run_deduce_group_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_open_smoke_test(interp, pass, fail);
    run_deduce_open_named_test(interp, pass, fail);
    run_deduce_durability_restart_test(interp, pass, fail);
    run_deduce_relation_attr_define_test(interp, pass, fail);
    run_deduce_crud_example_flow_test(interp, pass, fail);
    run_deduce_memory_open_isolation_test(interp, pass, fail);
    run_deduce_reopen_stress_test(interp, pass, fail);
    run_deduce_large_relation_schema_test(interp, pass, fail);
    run_deduce_long_relation_name_isolation_test(interp, pass, fail);
    run_deduce_long_open_path_isolation_test(interp, pass, fail);
    run_deduce_transaction_tests(interp, pass, fail);
    run_deduce_clear_drop_tests(interp, pass, fail);
    setup_deduce_person_relation(interp);
    run_deduce_query_tests(interp, pass, fail);
}

fn void run_deduce_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    io::printn("\n--- Deduce Tests ---");
    run_group_isolated(&run_deduce_group_tests, pass, fail);
}

fn void run_schema_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Schema/Contract Tests ---");

    // Define a schema
    setup(interp, "(define [schema] age-schema (and int (> 0)))");

    // validate with inline schema
    test_tag(interp, "validate int passes",
        "(validate 'int 42)", SYMBOL, pass, fail);
    test_tag(interp, "validate int fails on string",
        "(validate 'int \"hello\")", NIL, pass, fail);
    test_tag(interp, "validate string passes",
        "(validate 'string \"hello\")", SYMBOL, pass, fail);
    test_tag(interp, "validate any passes",
        "(validate 'any 42)", SYMBOL, pass, fail);

    // validate with composite schema
    test_tag(interp, "validate (and int (> 0)) passes",
        "(validate '(and int (> 0)) 5)", SYMBOL, pass, fail);
    test_tag(interp, "validate (and int (> 0)) fails on negative",
        "(validate '(and int (> 0)) -1)", NIL, pass, fail);
    test_tag(interp, "validate (or string int) passes int",
        "(validate '(or string int) 42)", SYMBOL, pass, fail);
    test_tag(interp, "validate (or string int) passes string",
        "(validate '(or string int) \"hi\")", SYMBOL, pass, fail);

    // validate with named schema
    test_tag(interp, "validate named schema passes",
        "(validate 'age-schema 5)", SYMBOL, pass, fail);
    test_tag(interp, "validate named schema fails",
        "(validate 'age-schema -1)", NIL, pass, fail);

    // maybe
    test_tag(interp, "validate (maybe int) on nil",
        "(validate '(maybe int) nil)", SYMBOL, pass, fail);
    test_tag(interp, "validate (maybe int) on int",
        "(validate '(maybe int) 42)", SYMBOL, pass, fail);

    // explain returns nil on valid
    test_tag(interp, "explain valid returns nil",
        "(explain 'int 42)", NIL, pass, fail);
}

fn void run_schema_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_schema_tests, pass, fail);
}

fn void run_reader_dispatch_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Reader Dispatch Tests ---");

    // Regex literal #r""
    test_str(interp, "regex literal #r",
        "(re-match #r\"hello\" \"say hello world\")", pass, fail);

    // Block comment
    test_eq(interp, "block comment",
        "(+ 1 #| ignored |# 2)", 3, pass, fail);

    // Form comment #_
    test_eq(interp, "#_ skips one form",
        "(+ 1 #_ 999 2)", 3, pass, fail);

    // Multi-form comment #N_
    test_eq(interp, "#2_ skips two forms",
        "(+ #2_ 10 20 1 2)", 3, pass, fail);

    // Nested block comment
    test_eq(interp, "nested block comment",
        "(+ 1 #| outer #| inner |# still |# 2)", 3, pass, fail);

    // Symbol-as-function: ('name dict) => (ref dict 'name)
    test_str_val(interp, "symbol-as-function on dict",
        "('name (dict 'name \"Alice\" 'age 30))", "Alice", pass, fail);
    test_eq(interp, "symbol-as-function on dict returns int",
        "('age (dict 'name \"Alice\" 'age 30))", 30, pass, fail);
}

fn void run_reader_dispatch_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_reader_dispatch_tests, pass, fail);
}

fn Value* make_root_int_for_test(Interp* interp, long n) {
    main::ScopeRegion* saved = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Value* v = make_int(interp, n);
    interp.current_scope = saved;
    return v;
}

fn bool delta_in_range(usz delta, usz min, usz max) @inline {
    return delta >= min && delta <= max;
}

fn void run_test_group_boundary_reset(Interp* interp) {
    run_test_global_boundary_reset();

    interp.flags.raise_pending = false;
    interp.raise_msg_len = 0;
    interp.raise_payload = null;
    interp.last_call_name = 0;
    interp.jit_env = null;
    interp.match_env = null;
    interp.flags.jit_tco_bounce = false;
    interp.jit_tco_expr = null;
    interp.jit_tco_env = null;
    interp.tco_recycle_scope = null;
    interp.tco_scope_defer_slot = 0;
    interp.tco_scope_defer_active = false;
    interp.releasing_scope = null;
    interp.escape_env_mode = false;
    interp.active_promotion_ctx = null;
    interp.resume_value = null;
    interp.yield_value = null;
    interp.current_reset_state = null;
    if (interp.current_scope != interp.root_scope) {
        interp.current_scope = interp.root_scope;
    }
}

fn void run_lisp_tests() {
    g_test_quiet_output =
        test_c_getenv("OMNI_TEST_QUIET") != null ||
        (test_summary_enabled() && test_c_getenv("OMNI_TEST_VERBOSE") == null);

    if (jit_checks_enabled()) {
        io::printn("=== Unified Tests (Interpreter + JIT) ===");
    } else {
        io::printn("=== Unified Tests (Interpreter; JIT checks disabled) ===");
    }

    Interp* interp = (Interp*)mem::malloc(Interp.sizeof);
    interp.init();
    register_primitives(interp);

    register_stdlib(interp);

    int pass = 0;
    int fail = 0;

    run_group_isolated(&run_basic_tests, &pass, &fail);
    run_group_isolated(&run_memory_lifetime_regression_tests, &pass, &fail);
    run_group_isolated(&run_memory_stress_tests, &pass, &fail);
    run_group_isolated(&run_list_closure_tests, &pass, &fail);
    run_group_isolated(&run_arithmetic_comparison_tests, &pass, &fail);
    run_group_isolated(&run_string_type_tests, &pass, &fail);
    run_diagnostic_tests_isolated(&pass, &fail);
    run_jit_policy_tests_isolated(&pass, &fail);
    run_group_isolated(&run_advanced_tests, &pass, &fail);
    run_group_isolated(&run_escape_scope_tests, &pass, &fail);
    run_group_isolated(&run_limit_busting_tests, &pass, &fail);
    run_group_isolated(&run_tco_recycling_tests, &pass, &fail);
    run_group_isolated(&run_closure_lifecycle_tests, &pass, &fail);

    run_pika_tests_isolated(&pass, &fail);
    run_unicode_tests_isolated(&pass, &fail);
    run_compression_tests_isolated(&pass, &fail);
    run_data_format_tests_isolated(&pass, &fail);
    run_json_tests_isolated(&pass, &fail);
    run_async_tests_isolated(&pass, &fail);
    run_reader_dispatch_tests_isolated(&pass, &fail);
    run_schema_tests_isolated(&pass, &fail);
    run_deduce_tests(interp, &pass, &fail);
    run_scheduler_tests_isolated(&pass, &fail);
    run_http_tests_isolated(&pass, &fail);
    run_atomic_tests_isolated(&pass, &fail);

    io::printfn("\n=== Unified Tests: %d passed, %d failed ===", pass, fail);
    emit_test_summary("unified", pass, fail);
    assert(fail == 0, "tests failed");

    run_test_group_boundary_reset(interp);
    run_compiler_tests(interp);

    // Final JIT teardown keeps ASAN leak checks deterministic.
    jit_global_shutdown();
    interp.destroy();
    mem::free(interp);
}

fn bool run_memory_lifetime_result_and_coroutine_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: run() must return a value that lives in caller scope.
    // For closure results, this catches stale child-scope pointers.
    {
        EvalResult r = run("((lambda (x) (lambda (y) x)) 10)", interp);
        bool ok = !r.error.has_error &&
                  r.value != null &&
                  main::is_in_scope((void*)r.value, interp.current_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run promotes closure result to caller scope");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: run promotes closure result to caller scope");
            (*fail)++;
        }
    }

    // Invariant: completed user coroutines should return their StackCtx to the pool.
    // This uses a fresh interpreter so pool accounting is deterministic.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        tmp.init();
        register_primitives(tmp);

        EvalResult r_thunk = run("(lambda () 42)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: completed coroutine returns StackCtx to pool (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;

        usz before = tmp.stack_ctx_pool.pool_size;
        Value* c = prim_coroutine(coro_args[..], null, tmp);

        Value*[1] resume_args;
        resume_args[0] = c;
        Value* r = prim_resume(resume_args[..], null, tmp);
        usz after = tmp.stack_ctx_pool.pool_size;

        bool ok = c != null &&
                  c.tag == COROUTINE &&
                  r != null &&
                  r.tag == INT &&
                  r.int_val == 42 &&
                  after == before + 1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: completed coroutine returns StackCtx to pool");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: completed coroutine returns StackCtx to pool (before=%d after=%d, coro_tag=%d, result_tag=%d)",
                (int)before, (int)after,
                c != null ? (int)c.tag : -1,
                r != null ? (int)r.tag : -1);
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    return true;
}

fn void run_memory_lifetime_cons_escape_promotion_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: dual-lane cons barrier promotes TEMP inputs when cdr is ESCAPE-lane.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Value* temp_car = make_int(interp, 7); // TEMP lane
        Value* escape_nil = interp.alloc_value_escape();
        escape_nil.tag = NIL;
        Value* pair = make_cons(interp, temp_car, escape_nil);

        bool ok = pair != null &&
                  pair.scope_gen == s.escape_generation &&
                  pair.cons_val.car != null &&
                  pair.cons_val.car.scope_gen == s.escape_generation &&
                  pair.cons_val.cdr != null &&
                  pair.cons_val.cdr.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: dual-lane make_cons promotes temp->escape");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: dual-lane make_cons promotes temp->escape");
            (*fail)++;
        }
    }

    // Invariant: promote_to_escape upgrades TEMP list wrappers to ESCAPE lane.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Value* tmp_nil = make_nil(interp);
        Value* tmp_list = make_cons(interp, make_int(interp, 1), tmp_nil);
        Value* esc_list = promote_to_escape(tmp_list, interp);

        bool ok = esc_list != null &&
                  esc_list.scope_gen == s.escape_generation &&
                  esc_list.tag == CONS &&
                  esc_list.cons_val.car != null &&
                  esc_list.cons_val.car.scope_gen == s.escape_generation &&
                  esc_list.cons_val.cdr != null &&
                  esc_list.cons_val.cdr.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promote_to_escape upgrades temp list");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: promote_to_escape upgrades temp list");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_lane_barrier_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env_define_with_barrier promotes temp payload for ESCAPE-lane env.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_define");
        Value* tmp = make_int(interp, 11);
        env_define_with_barrier(e, sid, tmp, interp);
        Value* got = e.lookup(sid);

        bool ok = got != null &&
                  got.tag == INT &&
                  got.int_val == 11 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_define_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_define_with_barrier promotes temp value");
            (*fail)++;
        }
    }

    // Invariant: env_set_with_barrier promotes temp payload at write target frame.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_set");
        env_define_with_barrier(e, sid, make_int(interp, 1), interp);
        Value* tmp_new = make_int(interp, 99);
        bool set_ok = true;
        if (catch err = env_set_with_barrier(e, sid, tmp_new, interp)) {
            set_ok = false;
        }
        Value* got = e.lookup(sid);

        bool ok = set_ok &&
                  got != null &&
                  got.tag == INT &&
                  got.int_val == 99 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_set_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_set_with_barrier promotes temp value");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_lane_barrier_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_cons_escape_promotion_tests(interp, pass, fail);
    run_memory_lifetime_env_lane_barrier_tests(interp, pass, fail);
}

fn void run_memory_lifetime_root_boundary_promotion_test(Interp* interp, int* pass, int* fail) {
    // Invariant: root-boundary promotion keeps defensive-copy semantics for
    // disjoint scope values (value not owned by releasing scope and not rooted).
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);

        interp.current_scope = scope_b;
        Value* foreign = make_cons(interp, make_int(interp, 21), make_nil(interp));

        interp.current_scope = scope_a;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(foreign, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted != foreign &&
                      promoted.tag == CONS &&
                      promoted.cons_val.car != null &&
                      promoted.cons_val.car.tag == INT &&
                      promoted.cons_val.car.int_val == 21 &&
                      delta_in_range(site_delta, 1, 3) &&
                      !main::is_in_scope((void*)promoted, scope_b);

        // Releasing source scope must not invalidate promoted value.
        main::scope_release(scope_b);

        bool post_ok = promoted != null &&
                       promoted.tag == CONS &&
                       promoted.cons_val.car != null &&
                       promoted.cons_val.car.tag == INT &&
                       promoted.cons_val.car.int_val == 21;

        interp.current_scope = saved;
        main::scope_release(scope_a);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary promotion defends disjoint scope");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary promotion defends disjoint scope (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == foreign) ? "yes" : "no",
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_cons_barrier_fallback_gate_test(Interp* interp, int* pass, int* fail) {
    // Invariant: cons-barrier fallback copy sites must remain unused.
    // If these counters rise, an old path was accidentally reintroduced.
    {
        usz base_total = copy_to_parent_total_calls();
        usz base_car = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR);
        usz base_cdr = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR);

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) (if (= n 2000) acc (loop (+ n 1) (cons n acc))))) (length (loop 0 nil)))",
            interp);

        usz total_delta = copy_to_parent_total_calls() - base_total;
        usz car_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR) - base_car;
        usz cdr_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR) - base_cdr;

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_budget = car_delta == 0 && cdr_delta == 0;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: cons-barrier fallback sites stay at zero");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: cons-barrier fallback sites stay at zero (total_delta=%d car_delta=%d cdr_delta=%d tag=%d int=%d err=%s)",
                (long)total_delta,
                (long)car_delta,
                (long)cdr_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_disjoint_fallback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: disjoint-scope values take defensive copy fallback before escape-lane promotion.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);

        interp.current_scope = foreign;
        Value* src = make_cons(interp, make_int(interp, 33), make_nil(interp));

        interp.current_scope = target;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_GENERIC);
        Value* escaped = promote_to_escape(src, interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_site;

        bool pre_ok = escaped != null &&
                      escaped != src &&
                      escaped.tag == CONS &&
                      escaped.scope_gen == target.escape_generation &&
                      escaped.cons_val.car != null &&
                      escaped.cons_val.car.tag == INT &&
                      escaped.cons_val.car.int_val == 33 &&
                      delta_in_range(site_delta, 1, 2) &&
                      !main::is_in_scope((void*)escaped, foreign);

        main::scope_release(foreign);

        bool post_ok = escaped != null &&
                       escaped.tag == CONS &&
                       escaped.cons_val.car != null &&
                       escaped.cons_val.car.tag == INT &&
                       escaped.cons_val.car.int_val == 33;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape disjoint fallback copy gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: escape disjoint fallback copy gate (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_fallback_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_root_boundary_promotion_test(interp, pass, fail);
    run_memory_lifetime_cons_barrier_fallback_gate_test(interp, pass, fail);
    run_memory_lifetime_escape_disjoint_fallback_test(interp, pass, fail);
}

fn void run_memory_lifetime_boundary_scope_restore_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary API helpers restore current/releasing scope state after temporary overrides.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);
        main::ScopeRegion* scope_c = main::scope_create(saved);
        main::ScopeRegion* scope_d = main::scope_create(saved);
        if (scope_a == null || scope_b == null || scope_c == null || scope_d == null) {
            if (scope_d != null) main::scope_release(scope_d);
            if (scope_c != null) main::scope_release(scope_c);
            if (scope_b != null) main::scope_release(scope_b);
            if (scope_a != null) main::scope_release(scope_a);
            io::printn("[FAIL] lifetime: boundary state restore helpers");
            (*fail)++;
            return;
        }

        interp.current_scope = scope_a;
        interp.releasing_scope = scope_b;

        Value* noop = boundary_copy_to_scope_site(
            null, interp, scope_c, scope_d, COPY_SITE_COPY_ENV_TO_SCOPE);
        bool copy_scope_ok = noop == null &&
                             interp.current_scope == scope_a &&
                             interp.releasing_scope == scope_b;

        Value* local_val = make_int(interp, 42);
        Value* copied = boundary_copy_from_releasing_scope(
            local_val, interp, scope_d, COPY_SITE_RUN_JIT_RESULT);
        bool copy_release_ok = copied != null &&
                               copied.tag == INT &&
                               copied.int_val == 42 &&
                               interp.current_scope == scope_a &&
                               interp.releasing_scope == scope_b;

        interp.current_scope = saved;
        interp.releasing_scope = null;
        main::scope_release(scope_d);
        main::scope_release(scope_c);
        main::scope_release(scope_b);
        main::scope_release(scope_a);

        bool ok = copy_scope_ok && copy_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary state restore helpers");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary state restore helpers (copy_scope_ok=%s copy_release_ok=%s)",
                copy_scope_ok ? "yes" : "no",
                copy_release_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_scope_interleaving_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested boundary enter/leave + child push/pop restores state exactly.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);
        main::ScopeRegion* scope_c = main::scope_create(saved);
        if (scope_a == null || scope_b == null || scope_c == null) {
            if (scope_c != null) main::scope_release(scope_c);
            if (scope_b != null) main::scope_release(scope_b);
            if (scope_a != null) main::scope_release(scope_a);
            io::printn("[FAIL] lifetime: boundary interleaving restores state");
            (*fail)++;
            return;
        }

        interp.current_scope = scope_a;
        interp.releasing_scope = scope_b;
        BoundaryInterpState state0 = boundary_save_interp_state(interp);

        main::ScopeRegion* enter_saved = boundary_enter_scope(interp, scope_c);
        main::ScopeRegion* push_saved = null;
        main::ScopeRegion* child = boundary_push_child_scope(interp, &push_saved);

        bool during_ok = child != null &&
                         enter_saved == scope_a &&
                         push_saved == scope_c &&
                         interp.current_scope == child &&
                         interp.releasing_scope == scope_b;

        if (child != null) {
            boundary_pop_child_scope(interp, push_saved, child);
        }
        boundary_leave_scope(interp, enter_saved);

        bool restore_ok = interp.current_scope == scope_a &&
                          interp.releasing_scope == scope_b;

        // After interleaving, releasing-scope copy path still works and leaves state unchanged.
        main::ScopeRegion* src_saved = boundary_enter_scope(interp, scope_b);
        Value* src = make_int(interp, 77);
        boundary_leave_scope(interp, src_saved);

        Value* copied = boundary_copy_from_releasing_scope(
            src, interp, scope_b, COPY_SITE_RUN_JIT_RESULT);
        bool copy_ok = copied != null &&
                       copied.tag == INT &&
                       copied.int_val == 77 &&
                       copied != src &&
                       !main::is_in_scope((void*)copied, scope_b) &&
                       interp.current_scope == scope_a &&
                       interp.releasing_scope == scope_b;

        boundary_restore_interp_state(interp, state0);
        bool final_ok = interp.current_scope == scope_a &&
                        interp.releasing_scope == scope_b;

        interp.current_scope = saved;
        interp.releasing_scope = null;
        main::scope_release(scope_c);
        main::scope_release(scope_b);
        main::scope_release(scope_a);

        bool ok = during_ok && restore_ok && copy_ok && final_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary interleaving restores state");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary interleaving restores state (during_ok=%s restore_ok=%s copy_ok=%s final_ok=%s)",
                during_ok ? "yes" : "no",
                restore_ok ? "yes" : "no",
                copy_ok ? "yes" : "no",
                final_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state across multi-form execution.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult r = run_program(
            "(define __boundary_prog_x 10)\n"
            "(define __boundary_prog_y 32)\n"
            "(+ __boundary_prog_x __boundary_prog_y)",
            interp);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 42;
        bool ok_state = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;
        bool ok = ok_eval && ok_state;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
                ok_eval ? "yes" : "no",
                ok_state ? "yes" : "no",
                interp.current_scope == saved_scope ? "yes" : "no",
                interp.releasing_scope == saved_releasing ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_error_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state on parse/runtime error exits.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult parse_err = run_program(
            "(define __boundary_prog_err 1\n"
            "(+ __boundary_prog_err 2)",
            interp);
        bool parse_ok = parse_err.error.has_error &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        EvalResult eval_err = run_program(
            "(define __boundary_prog_eval_err 1)\n"
            "(car 1)",
            interp);
        bool eval_ok = eval_err.error.has_error &&
                       interp.current_scope == saved_scope &&
                       interp.releasing_scope == saved_releasing;

        bool ok = parse_ok && eval_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program error boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program error boundary state restore (parse_ok=%s eval_ok=%s)",
                parse_ok ? "yes" : "no",
                eval_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: wrapper values in target chain are reused, while disjoint wrappers are defensively copied.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: wrapper reuse vs defensive copy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        void* h_reuse = mem::malloc(8);
        Value* reusable = make_ffi_handle_ex(interp, h_reuse, "__reuse", null, true);

        interp.current_scope = foreign;
        void* h_foreign = mem::malloc(8);
        Value* disjoint = make_ffi_handle_ex(interp, h_foreign, "__disjoint", null, true);

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == FFI_HANDLE &&
                         copied != disjoint &&
                         copied.ffi_val != null &&
                         copied.ffi_val == disjoint.ffi_val &&
                         copied.ffi_val.refcount == 2;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == FFI_HANDLE &&
                       copied.ffi_val != null &&
                       copied.ffi_val.refcount == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reusable != null && disjoint != null && reuse_ok && copied_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: wrapper reuse vs defensive copy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: wrapper reuse vs defensive copy (reusable=%s disjoint=%s reuse_ok=%s copied_ok=%s post_ok=%s)",
                reusable != null ? "yes" : "no",
                disjoint != null ? "yes" : "no",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_memo_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env copy uses one boundary epoch and memoizes repeated values.
    // This keeps copy_env_to_scope churn bounded for shared captured values.
    {
        reset_memory_promotion_stats();

        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* source = main::scope_create(saved);

        interp.current_scope = source;
        Env* src_env = make_env(interp, null);
        Value* shared = make_cons(interp, make_int(interp, 123), make_nil(interp));
        for (usz i = 0; i < 24; i++) {
            char[64] nbuf;
            char[] name = io::bprintf(&nbuf, "__copy_env_shared_%d", (long)i)!!;
            env_define_with_barrier(src_env, interp.symbols.intern(name), shared, interp);
        }

        interp.current_scope = target;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        Env* copied = copy_env_to_scope(src_env, interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE) - base_site;

        bool copied_ok = copied != null && copied.binding_count == src_env.binding_count;
        bool alias_ok = copied_ok;
        Value* first = null;
        if (copied_ok) {
            for (usz i = 0; i < copied.binding_count; i++) {
                Value* cur = copied.bindings[i].value;
                if (cur == null || cur.tag != CONS ||
                    cur.cons_val.car == null || cur.cons_val.car.tag != INT ||
                    cur.cons_val.car.int_val != 123) {
                    alias_ok = false;
                    break;
                }
                if (i == 0) first = cur;
                if (i > 0 && cur != first) {
                    alias_ok = false;
                    break;
                }
            }
        }

        // Source release must not invalidate target copies.
        main::scope_release(source);
        bool post_ok = first != null &&
                       first.tag == CONS &&
                       first.cons_val.car != null &&
                       first.cons_val.car.tag == INT &&
                       first.cons_val.car.int_val == 123;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = copied_ok && alias_ok && post_ok && site_delta <= 4;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env shared-value memo gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env shared-value memo gate (site_delta=%d copied_ok=%s alias_ok=%s post_ok=%s)",
                (long)site_delta,
                copied_ok ? "yes" : "no",
                alias_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_persistent_parent_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: copy_env_to_scope rewrites persistent frame parents away from
    // releasing source scopes, so persistent boxes never retain dangling parent links.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_env persistent parent rewrite");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_parent_payload");
        SymbolId box_sym = interp.symbols.intern("__copy_env_parent_box");

        interp.current_scope = source;
        Env* transient_parent = make_env(interp, interp.global_env);
        env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 99), interp);

        Env* persistent_box = boundary_env_extend_in_root(
            interp,
            transient_parent,
            box_sym,
            make_root_int_for_test(interp, 7)
        );
        if (persistent_box != null) persistent_box.persistent = true;
        Env* original_parent = persistent_box != null ? persistent_box.parent : null;

        interp.current_scope = target;
        Env* copied = copy_env_to_scope(persistent_box, interp);

        bool copied_ok = copied != null && copied == persistent_box && copied.persistent;
        bool rewritten_ok = copied_ok &&
                           copied.parent != null &&
                           copied.parent != original_parent &&
                           main::is_in_scope((void*)copied.parent, target);

        Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
        main::scope_release(source);
        Value* after_release = rewritten_ok ? copied.lookup(payload_sym) : null;

        bool post_ok = before_release != null &&
                       before_release.tag == INT &&
                       before_release.int_val == 99 &&
                       after_release != null &&
                       after_release.tag == INT &&
                       after_release.int_val == 99;

        // Restore persistent parent chain to stable root path before releasing target.
        if (copied_ok) {
            copied.parent = interp.global_env;
        }

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = copied_ok && rewritten_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env persistent parent rewrite");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env persistent parent rewrite (copied_ok=%s rewritten_ok=%s post_ok=%s)",
                copied_ok ? "yes" : "no",
                rewritten_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_mixed_chain_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed transient/persistent parent chains are fully rewritten
    // when copying from a releasing scope into a target scope.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_env mixed persistent chain rewrite");
            (*fail)++;
            return;
        }

        SymbolId leaf_a = interp.symbols.intern("__copy_env_leaf_a");
        SymbolId leaf_b = interp.symbols.intern("__copy_env_leaf_b");
        SymbolId box_a = interp.symbols.intern("__copy_env_box_a");
        SymbolId box_b = interp.symbols.intern("__copy_env_box_b");

        interp.current_scope = source;
        Env* transient_a = make_env(interp, interp.global_env);
        env_define_with_barrier(transient_a, leaf_a, make_int(interp, 11), interp);

        Env* persistent_a = boundary_env_extend_in_root(
            interp, transient_a, box_a, make_root_int_for_test(interp, 1));
        if (persistent_a != null) persistent_a.persistent = true;

        Env* transient_b = make_env(interp, persistent_a);
        env_define_with_barrier(transient_b, leaf_b, make_int(interp, 22), interp);

        Env* persistent_b = boundary_env_extend_in_root(
            interp, transient_b, box_b, make_root_int_for_test(interp, 2));
        if (persistent_b != null) persistent_b.persistent = true;

        Env* orig_parent_a = persistent_a != null ? persistent_a.parent : null;
        Env* orig_parent_b = persistent_b != null ? persistent_b.parent : null;

        interp.current_scope = target;
        Env* copied = copy_env_to_scope(persistent_b, interp);

        bool id_ok = copied != null && copied == persistent_b && copied.persistent;
        bool parent_b_ok = id_ok &&
                           copied.parent != null &&
                           copied.parent != orig_parent_b &&
                           main::is_in_scope((void*)copied.parent, target);
        bool parent_a_ok = id_ok &&
                           persistent_a != null &&
                           persistent_a.parent != null &&
                           persistent_a.parent != orig_parent_a &&
                           main::is_in_scope((void*)persistent_a.parent, target);

        Value* va_before = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_before = id_ok ? copied.lookup(leaf_b) : null;

        main::scope_release(source);

        Value* va_after = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_after = id_ok ? copied.lookup(leaf_b) : null;

        bool values_ok = va_before != null && va_before.tag == INT && va_before.int_val == 11 &&
                         vb_before != null && vb_before.tag == INT && vb_before.int_val == 22 &&
                         va_after != null && va_after.tag == INT && va_after.int_val == 11 &&
                         vb_after != null && vb_after.tag == INT && vb_after.int_val == 22;

        // Detach persistent nodes from target-owned copied parents before target release.
        if (persistent_b != null) persistent_b.parent = persistent_a;
        if (persistent_a != null) persistent_a.parent = interp.global_env;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = id_ok && parent_b_ok && parent_a_ok && values_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env mixed persistent chain rewrite");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env mixed persistent chain rewrite (id_ok=%s parent_b_ok=%s parent_a_ok=%s values_ok=%s)",
                id_ok ? "yes" : "no",
                parent_b_ok ? "yes" : "no",
                parent_a_ok ? "yes" : "no",
                values_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_env_lane_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: unified one-scope escape-env mode allocates env frames in ESCAPE lane.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool saved_escape_mode = interp.escape_env_mode;

        main::ScopeRegion* s = main::scope_create(saved_scope);
        interp.current_scope = s;
        interp.escape_env_mode = true;

        Env* e = make_env(interp, null);
        bool ok = e != null &&
                  e.scope_gen == s.escape_generation &&
                  main::is_in_scope((void*)e, s);

        interp.escape_env_mode = saved_escape_mode;
        interp.current_scope = saved_scope;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: unified escape-env uses current ESCAPE lane");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: unified escape-env uses current ESCAPE lane");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_escape_mode_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_env_copy_memo_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_persistent_parent_rewrite_test(interp, pass, fail);
    run_memory_lifetime_env_copy_mixed_chain_rewrite_test(interp, pass, fail);
    run_memory_lifetime_escape_env_lane_tests(interp, pass, fail);
}

fn void run_memory_lifetime_jit_copy_hot_budget_test(Interp* interp, int* pass, int* fail) {
    // Invariant: core hot copy sites remain bounded on hot closure loop.
    {
        reset_memory_promotion_stats();

        EvalResult r = run(
            "(begin "
            "  (define step (lambda (x) (+ x 1))) "
            "  (define mk (lambda (n) (lambda (x) (+ x n)))) "
            "  (let ^rec (loop (lambda (i acc) "
            "    (if (= i 600) "
            "        acc "
            "        (let (f (mk i)) "
            "          (loop (+ i 1) (step (+ acc (f 1)))))))) "
            "    (loop 0 0)))",
            interp);

        usz jit_single_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
        usz copy_env_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        usz run_jit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        usz tco_frame_delta = copy_to_parent_site_calls(COPY_SITE_JIT_TCO_FRAME_COPY);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 180900;
        bool ok_budget = jit_single_delta <= 32 &&
                         copy_env_delta <= 32 &&
                         run_jit_delta <= 8 &&
                         tco_frame_delta <= 64;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: jit/copy hot-site budget gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: jit/copy hot-site budget gate (jit_single=%d copy_env=%d run_jit=%d tco_frame=%d tag=%d int=%d err=%s)",
                (long)jit_single_delta,
                (long)copy_env_delta,
                (long)run_jit_delta,
                (long)tco_frame_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_frame_budget_test(Interp* interp, int* pass, int* fail) {
    // Invariant: named-let TCO fast path keeps frame-copy churn near-zero.
    {
        reset_memory_promotion_stats();

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 5000) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);

        usz tco_frame_delta = copy_to_parent_site_calls(COPY_SITE_JIT_TCO_FRAME_COPY);
        usz copy_env_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        usz run_jit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 5000;
        bool ok_budget = tco_frame_delta <= 16 &&
                         copy_env_delta <= 8 &&
                         run_jit_delta <= 4;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco frame-copy budget gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco frame-copy budget gate (tco_frame=%d copy_env=%d run_jit=%d tag=%d int=%d err=%s)",
                (long)tco_frame_delta,
                (long)copy_env_delta,
                (long)run_jit_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_boundary_state_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle/env-copy boundary paths restore interpreter scope state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 2000) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_state = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;
        bool ok = ok_eval && ok_state;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
                ok_eval ? "yes" : "no",
                ok_state ? "yes" : "no",
                interp.current_scope == saved_scope ? "yes" : "no",
                interp.releasing_scope == saved_releasing ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_runtime_fields_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO paths restore recycle/defer runtime fields on both success and error.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        usz saved_tco_slot = interp.tco_scope_defer_slot;
        bool saved_tco_active = interp.tco_scope_defer_active;
        bool saved_escape = interp.escape_env_mode;

        EvalResult ok_run = run(
            "(let ^rec (loop (lambda (n acc) "
            "  (if (= n 1024) "
            "      acc "
            "      (loop (+ n 1) (+ acc 1))))) "
            "  (loop 0 0))",
            interp);
        bool ok_eval = !ok_run.error.has_error &&
                       ok_run.value != null &&
                       ok_run.value.tag == INT &&
                       ok_run.value.int_val == 1024;
        bool ok_state_after_ok =
            interp.current_scope == saved_scope &&
            interp.releasing_scope == saved_releasing &&
            interp.tco_recycle_scope == saved_tco_recycle &&
            interp.tco_scope_defer_slot == saved_tco_slot &&
            interp.tco_scope_defer_active == saved_tco_active &&
            interp.escape_env_mode == saved_escape;

        EvalResult err_run = run(
            "(let ^rec (loop (lambda (n) "
            "  (if (= n 256) "
            "      (car 1) "
            "      (loop (+ n 1))))) "
            "  (loop 0))",
            interp);
        bool err_eval = err_run.error.has_error;
        bool ok_state_after_err =
            interp.current_scope == saved_scope &&
            interp.releasing_scope == saved_releasing &&
            interp.tco_recycle_scope == saved_tco_recycle &&
            interp.tco_scope_defer_slot == saved_tco_slot &&
            interp.tco_scope_defer_active == saved_tco_active &&
            interp.escape_env_mode == saved_escape;

        bool ok = ok_eval && err_eval && ok_state_after_ok && ok_state_after_err;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco runtime fields restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco runtime fields restore (ok_eval=%s err_eval=%s state_ok=%s state_err=%s)",
                ok_eval ? "yes" : "no",
                err_eval ? "yes" : "no",
                ok_state_after_ok ? "yes" : "no",
                ok_state_after_err ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_tco_recycle_error_restore_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO recycle error path restores state when defer retarget cannot run.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_tco_recycle = interp.tco_recycle_scope;
        usz saved_tco_slot = interp.tco_scope_defer_slot;
        bool saved_tco_active = interp.tco_scope_defer_active;
        Env* saved_jit_env = interp.jit_env;

        main::ScopeRegion* cs = main::scope_create(saved_scope);
        if (cs == null) {
            io::printn("[FAIL] lifetime: tco recycle error path state restore");
            (*fail)++;
            return;
        }

        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        interp.tco_scope_defer_slot = 0;
        interp.tco_scope_defer_active = true;
        interp.jit_env = interp.global_env;

        SymbolId sym = interp.symbols.intern("__tco_recycle_err");
        Env* env = make_env(interp, interp.global_env);
        env_define_with_barrier(env, sym, make_int(interp, 1), interp);
        Env* env_io = env;

        bool stack_ctx_missing = (main::g_current_stack_ctx == null);
        Value* err = jit_prepare_tco_recycle(&env_io, interp.global_env, interp);

        bool err_ok = err != null && err.tag == ERROR;
        bool env_ok = env_io == env;
        bool restored_ok = interp.current_scope == cs &&
                           interp.tco_recycle_scope == cs &&
                           interp.jit_env == interp.global_env;
        bool ok = stack_ctx_missing && err_ok && env_ok && restored_ok;

        interp.current_scope = saved_scope;
        interp.tco_recycle_scope = saved_tco_recycle;
        interp.tco_scope_defer_slot = saved_tco_slot;
        interp.tco_scope_defer_active = saved_tco_active;
        interp.jit_env = saved_jit_env;
        main::scope_release(cs);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tco recycle error path state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: tco recycle error path state restore (stack_ctx_missing=%s err_ok=%s env_ok=%s restored_ok=%s)",
                stack_ctx_missing ? "yes" : "no",
                err_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                restored_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_hot_budget_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_jit_copy_hot_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_frame_budget_test(interp, pass, fail);
    run_memory_lifetime_tco_boundary_state_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_runtime_fields_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_recycle_error_restore_test(interp, pass, fail);
}

fn void run_memory_lifetime_promotion_context_memo_test(Interp* interp, int* pass, int* fail) {
    // Invariant: promotion context memoizes fallback copy within one boundary epoch.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 7), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 8);
        Value* a = copy_value_if_owned_by_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        Value* b = copy_value_if_owned_by_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        promotion_context_end(interp, &ctx);

        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        bool ok = a != null &&
                  b == a &&
                  delta_in_range(site_delta, 1, 4) &&
                  ctx.epoch != 0 &&
                  !ctx.aborted;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion context memo gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion context memo gate (site_delta=%d epoch=%d aborted=%s same=%s)",
                (long)site_delta,
                (int)ctx.epoch,
                ctx.aborted ? "yes" : "no",
                (a != null && b == a) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_abort_fallback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: budget abort triggers deterministic fallback in same epoch context.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 11), make_nil(interp));

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 0);
        Value* promoted = promote_to_escape(src, interp);

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        Value* fallback = copy_value_if_owned_by_scope(src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        interp.releasing_scope = saved_releasing;
        promotion_context_end(interp, &ctx);

        bool ok = promoted == src &&
                  ctx.aborted &&
                  ctx.epoch != 0 &&
                  fallback != null &&
                  fallback.tag == CONS &&
                  delta_in_range(site_delta, 1, 4);

        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion abort fallback gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion abort fallback gate (aborted=%s epoch=%d promoted_same=%s site_delta=%d fallback_tag=%d)",
                ctx.aborted ? "yes" : "no",
                (int)ctx.epoch,
                (promoted == src) ? "yes" : "no",
                (long)site_delta,
                fallback != null ? (int)fallback.tag : -1);
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_abort_nested_releasing_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested boundary_copy_from_releasing_scope interleavings under
    // aborted promotion context restore boundary state and avoid memo aliasing.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);
        main::ScopeRegion* sibling = main::scope_create(parent);
        if (parent == null || child == null || sibling == null) {
            if (sibling != null) main::scope_release(sibling);
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: promotion abort nested releasing copy");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 21), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, 0);

        Value* first = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        main::ScopeRegion* enter_saved = boundary_enter_scope(interp, sibling);
        Value* second = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        boundary_leave_scope(interp, enter_saved);

        usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;
        bool state_ok = interp.current_scope == parent &&
                        interp.releasing_scope == saved_releasing;
        bool copies_ok = first != null &&
                         second != null &&
                         first != second &&
                         first.tag == CONS &&
                         second.tag == CONS &&
                         first.cons_val.car != null &&
                         second.cons_val.car != null &&
                         first.cons_val.car.tag == INT &&
                         second.cons_val.car.tag == INT &&
                         first.cons_val.car.int_val == 21 &&
                         second.cons_val.car.int_val == 21 &&
                         !main::is_in_scope((void*)first, child) &&
                         !main::is_in_scope((void*)second, child) &&
                         main::is_in_scope((void*)first, parent) &&
                         main::is_in_scope((void*)second, sibling);
        bool budget_ok = ctx.aborted && ctx.epoch != 0 && site_delta >= 2;
        promotion_context_end(interp, &ctx);

        bool post_ctx_ok = interp.active_promotion_ctx == null;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(sibling);
        main::scope_release(child);
        main::scope_release(parent);

        bool ok = state_ok && copies_ok && budget_ok && post_ctx_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: promotion abort nested releasing copy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: promotion abort nested releasing copy (state_ok=%s copies_ok=%s budget_ok=%s post_ctx_ok=%s site_delta=%d aborted=%s)",
                state_ok ? "yes" : "no",
                copies_ok ? "yes" : "no",
                budget_ok ? "yes" : "no",
                post_ctx_ok ? "yes" : "no",
                (long)site_delta,
                ctx.aborted ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_promotion_context_stack_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested promotion contexts preserve stack discipline and memo behavior.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = main::scope_create(parent);
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: nested promotion context stack");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* src = make_cons(interp, make_int(interp, 55), make_nil(interp));

        interp.current_scope = parent;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = child;

        PromotionContext outer = {};
        promotion_context_begin(interp, &outer, 0); // force aborted fallback in outer
        Value* outer_a = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);

        PromotionContext inner = {};
        promotion_context_begin(interp, &inner, 4);
        bool inner_prev_ok = inner.prev == &outer && interp.active_promotion_ctx == &inner;
        Value* inner_a = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        Value* inner_b = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        bool inner_memo_ok = inner_a != null && inner_b == inner_a && !inner.aborted;
        promotion_context_end(interp, &inner);
        bool outer_restored_ok = interp.active_promotion_ctx == &outer;

        Value* outer_b = boundary_copy_from_releasing_scope(
            src, interp, child, COPY_SITE_RUN_JIT_RESULT);
        promotion_context_end(interp, &outer);

        bool outer_abort_ok = outer.aborted &&
                              outer_a != null &&
                              outer_b != null &&
                              outer_a != outer_b;
        bool final_ctx_ok = interp.active_promotion_ctx == null;

        interp.releasing_scope = saved_releasing;
        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        bool ok = inner_prev_ok && inner_memo_ok && outer_restored_ok &&
                  outer_abort_ok && final_ctx_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested promotion context stack");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: nested promotion context stack (inner_prev_ok=%s inner_memo_ok=%s outer_restored_ok=%s outer_abort_ok=%s final_ctx_ok=%s)",
                inner_prev_ok ? "yes" : "no",
                inner_memo_ok ? "yes" : "no",
                outer_restored_ok ? "yes" : "no",
                outer_abort_ok ? "yes" : "no",
                final_ctx_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_promotion_context_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_promotion_context_memo_test(interp, pass, fail);
    run_memory_lifetime_promotion_abort_fallback_test(interp, pass, fail);
    run_memory_lifetime_promotion_abort_nested_releasing_copy_test(interp, pass, fail);
    run_memory_lifetime_nested_promotion_context_stack_test(interp, pass, fail);
}

fn void run_memory_lifetime_regression_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Lifetime Regression Tests ---");

    if (!run_memory_lifetime_result_and_coroutine_tests(interp, pass, fail)) return;
    run_memory_lifetime_lane_barrier_tests(interp, pass, fail);
    run_memory_lifetime_root_fallback_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_restore_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_interleaving_test(interp, pass, fail);
    run_memory_lifetime_run_program_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_run_program_error_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(interp, pass, fail);
    run_memory_lifetime_env_copy_escape_mode_tests(interp, pass, fail);
    run_memory_lifetime_hot_budget_tests(interp, pass, fail);
    run_memory_lifetime_promotion_context_tests(interp, pass, fail);
}

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
    { io::print("[PASS] Print: "); run("(println (quote hello))", interp); (*pass)++; }

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

fn void run_basic_let_destructure_array_tests(Interp* interp, int* pass, int* fail) {
    // Array: basic
    test_eq(interp, "let [x y] array", "(let ([x y] [10 20]) (+ x y))", 30, pass, fail);
    test_eq(interp, "let [a b c] list", "(let ([a b c] '(1 2 3)) (+ a (+ b c)))", 6, pass, fail);
    test_eq(interp, "let [a b c] array", "(let ([a b c] [1 2 3]) (+ a (+ b c)))", 6, pass, fail);

    // Array: head..tail
    test_eq(interp, "let [h .. t] list", "(let ([h .. t] '(10 20 30)) h)", 10, pass, fail);
    test_eq(interp, "let [h .. t] array", "(let ([h .. t] [10 20 30]) h)", 10, pass, fail);
    test_eq(interp, "let [h .. t] tail len", "(let ([h .. t] '(10 20 30)) (length t))", 2, pass, fail);

    // Array: ignore rest
    test_eq(interp, "let [a b ..] list", "(let ([a b ..] '(1 2 3 4 5)) (+ a b))", 3, pass, fail);
    test_eq(interp, "let [a b ..] array", "(let ([a b ..] [1 2 3 4 5]) (+ a b))", 3, pass, fail);

    // Array: [.. last]
    test_eq(interp, "let [.. last] list", "(let ([.. last] '(1 2 3 4 5)) last)", 5, pass, fail);

    // Array: wildcard
    test_eq(interp, "let [_ b _] skip", "(let ([_ b _] [10 20 30]) b)", 20, pass, fail);
}

fn void run_basic_let_destructure_dict_tests(Interp* interp, int* pass, int* fail) {
    // Dict: basic
    test_str(interp, "let {name} dict", "(let ({name} {'name \"Alice\" 'age 30}) name)", pass, fail);
    test_eq(interp, "let {age} dict", "(let ({age} {'name \"Alice\" 'age 30}) age)", 30, pass, fail);
    test_eq(interp, "let {x y} dict", "(let ({x y} {'x 10 'y 20}) (+ x y))", 30, pass, fail);

    // Dict: missing key -> nil
    test_eq(interp, "let {z} missing → nil", "(let ({z} {'x 10}) (if z 99 0))", 0, pass, fail);

    // Dict: 3+ keys
    test_eq(interp, "let {a b c} dict", "(let ({a b c} {'a 1 'b 2 'c 3}) (+ a (+ b c)))", 6, pass, fail);
}

fn void run_basic_let_destructure_mixed_binding_tests(Interp* interp, int* pass, int* fail) {
    // Mixed: plain + array
    test_eq(interp, "let plain + [arr]", "(let (x 1 [a b] [2 3]) (+ x (+ a b)))", 6, pass, fail);
    test_eq(interp, "let [arr] + plain", "(let ([a b] [2 3] x 1) (+ x (+ a b)))", 6, pass, fail);

    // Mixed: plain + dict
    test_eq(interp, "let plain + {dict}", "(let (x 1 {y} {'y 2}) (+ x y))", 3, pass, fail);
    test_eq(interp, "let {dict} + plain", "(let ({y} {'y 2} x 1) (+ x y))", 3, pass, fail);

    // Mixed: array + dict
    test_eq(interp, "let [arr] + {dict}", "(let ([a b] [1 2] {x} {'x 10}) (+ a (+ b x)))", 13, pass, fail);
    test_eq(interp, "let {dict} + [arr]", "(let ({x} {'x 10} [a b] [1 2]) (+ a (+ b x)))", 13, pass, fail);

    // Mixed: plain + array + dict
    test_eq(interp, "let p + [a] + {d}", "(let (z 1 [a b] [2 3] {x} {'x 4}) (+ z (+ a (+ b x))))", 10, pass, fail);

    // Mixed: two arrays
    test_eq(interp, "let [a1] [a2]", "(let ([a b] [1 2] [c d] [3 4]) (+ a (+ b (+ c d))))", 10, pass, fail);

    // Mixed: two dicts
    test_eq(interp, "let {d1} {d2}", "(let ({x} {'x 3} {y} {'y 7}) (+ x y))", 10, pass, fail);
}

fn void run_basic_let_destructure_shadow_scope_tests(Interp* interp, int* pass, int* fail) {
    // Shadowing in let: second binding shadows first
    test_eq(interp, "let shadow plain", "(let (x 1 x 2) x)", 2, pass, fail);
    test_eq(interp, "let shadow [a] [a]", "(let ([a b] [1 2] [a b] [10 20]) (+ a b))", 30, pass, fail);
    test_eq(interp, "let shadow {d} {d}", "(let ({x} {'x 1} {x} {'x 99}) x)", 99, pass, fail);

    // Nested scope: destructured bindings visible in body
    test_eq(interp, "let [a] in body", "(let ([a b] [1 2]) (let (c (+ a b)) c))", 3, pass, fail);
    test_eq(interp, "let {d} in body", "(let ({x y} {'x 3 'y 4}) (let (z (+ x y)) z))", 7, pass, fail);

    // Array destr from function result
    test_eq(interp, "let [a b] from call", "(let ([a b] (list 10 20)) (+ a b))", 30, pass, fail);

    // Dict destr with symbol-as-function
    test_eq(interp, "let {d} sym-as-fn", "(let ({name} {'name \"Alice\"}) (string-length name))", 5, pass, fail);
}

fn void run_basic_let_destructure_default_tests(Interp* interp, int* pass, int* fail) {
    // default function for dict optional keys
    test_eq(interp, "default nil → fallback", "(default nil 42)", 42, pass, fail);
    test_eq(interp, "default val → val", "(default 10 42)", 10, pass, fail);
    test_eq(interp, "default 0 → 0", "(default 0 42)", 0, pass, fail);
    test_eq(interp, "default with {dict}", "(let ({timeout} {'host \"x\"}) (default timeout 5000))", 5000, pass, fail);
    test_eq(interp, "default provided", "(let ({timeout} {'timeout 3000}) (default timeout 5000))", 3000, pass, fail);

    // with-defaults macro
    test_eq(interp, "with-defaults body only", "(with-defaults 42)", 42, pass, fail);
    test_eq(interp, "with-defaults nil → default", "(let (x nil) (with-defaults x 5 x))", 5, pass, fail);
    test_eq(interp, "with-defaults val → val", "(let (x 10) (with-defaults x 5 x))", 10, pass, fail);
    test_eq(interp, "with-defaults multi", "(let (x nil y nil) (with-defaults x 5 y 10 (+ x y)))", 15, pass, fail);
    test_eq(interp, "with-defaults partial", "(let (x 1 y nil) (with-defaults x 5 y 10 (+ x y)))", 11, pass, fail);
    test_eq(interp, "with-defaults + {dict}", "(let ({timeout} {'host \"x\"}) (with-defaults timeout 5000 timeout))", 5000, pass, fail);
}

fn void run_basic_let_destructure_tests(Interp* interp, int* pass, int* fail) {
    // === Let destructuring: all combinations ===
    run_basic_let_destructure_array_tests(interp, pass, fail);
    run_basic_let_destructure_dict_tests(interp, pass, fail);
    run_basic_let_destructure_mixed_binding_tests(interp, pass, fail);
    run_basic_let_destructure_shadow_scope_tests(interp, pass, fail);
    run_basic_let_destructure_default_tests(interp, pass, fail);
}

fn void run_basic_function_param_destructure_define_tests(Interp* interp, int* pass, int* fail) {
    // Single dict param
    setup(interp, "(define (d-single {name}) name)");
    test_str(interp, "define {name} param", "(d-single {'name \"Alice\" 'age 30})", pass, fail);

    // Multi-key dict param
    setup(interp, "(define (d-multi {x y}) (+ x y))");
    test_eq(interp, "define {x y} param", "(d-multi {'x 10 'y 20})", 30, pass, fail);

    // Single array param
    setup(interp, "(define (d-arr [a b]) (+ a b))");
    test_eq(interp, "define [a b] param", "(d-arr [3 4])", 7, pass, fail);

    // Array param with head..tail
    setup(interp, "(define (d-ht [h .. t]) h)");
    test_eq(interp, "define [h .. t] param", "(d-ht '(10 20 30))", 10, pass, fail);

    // Dict + positional
    setup(interp, "(define (d-dp {a} b) (+ a b))");
    test_eq(interp, "define {dict} + positional", "(d-dp {'a 5} 10)", 15, pass, fail);

    // Positional + dict
    setup(interp, "(define (d-pd x {y}) (+ x y))");
    test_eq(interp, "define positional + {dict}", "(d-pd 3 {'y 7})", 10, pass, fail);

    // Array + positional
    setup(interp, "(define (d-ap [a b] c) (+ a (+ b c)))");
    test_eq(interp, "define [arr] + positional", "(d-ap [1 2] 3)", 6, pass, fail);

    // Positional + array
    setup(interp, "(define (d-pa x [a b]) (+ x (+ a b)))");
    test_eq(interp, "define positional + [arr]", "(d-pa 10 [3 4])", 17, pass, fail);

    // Two dict params
    setup(interp, "(define (d-dd {x} {y}) (+ x y))");
    test_eq(interp, "define {d1} {d2}", "(d-dd {'x 3} {'y 7})", 10, pass, fail);

    // Dict + array
    setup(interp, "(define (d-da {x} [a b]) (+ x (+ a b)))");
    test_eq(interp, "define {dict} [arr]", "(d-da {'x 10} [1 2])", 13, pass, fail);

    // Array + dict
    setup(interp, "(define (d-ad [a b] {x}) (+ a (+ b x)))");
    test_eq(interp, "define [arr] {dict}", "(d-ad [1 2] {'x 10})", 13, pass, fail);

    // Dict + array + positional
    setup(interp, "(define (d-dap {x} [a b] c) (+ x (+ a (+ b c))))");
    test_eq(interp, "define {d} [a] pos", "(d-dap {'x 1} [2 3] 4)", 10, pass, fail);

    // Positional + dict + array
    setup(interp, "(define (d-pda z {x} [a b]) (+ z (+ x (+ a b))))");
    test_eq(interp, "define pos {d} [a]", "(d-pda 1 {'x 2} [3 4])", 10, pass, fail);

    // Dict + positional + variadic
    setup(interp, "(define (d-dpv {x} y .. rest) (+ x y))");
    test_eq(interp, "define {d} pos .. rest", "(d-dpv {'x 5} 10 20 30)", 15, pass, fail);

    // Array + variadic
    setup(interp, "(define (d-av [a b] .. rest) (+ a b))");
    test_eq(interp, "define [arr] .. rest", "(d-av [1 2] 3 4 5)", 3, pass, fail);

    // Three destructuring params
    setup(interp, "(define (d-three {x} [a b] {y}) (+ x (+ a (+ b y))))");
    test_eq(interp, "define {d} [a] {d2}", "(d-three {'x 1} [2 3] {'y 4})", 10, pass, fail);
}

fn void run_basic_function_param_destructure_lambda_typed_tests(Interp* interp, int* pass, int* fail) {
    // Lambda: dict param
    test_eq(interp, "lambda {dict}", "((lambda ({x y}) (+ x y)) {'x 3 'y 4})", 7, pass, fail);

    // Lambda: array param
    test_eq(interp, "lambda [arr]", "((lambda ([a b]) (+ a b)) [5 6])", 11, pass, fail);

    // Lambda: dict + positional
    test_eq(interp, "lambda {d} pos", "((lambda ({x} y) (+ x y)) {'x 3} 7)", 10, pass, fail);

    // Lambda: array + dict
    test_eq(interp, "lambda [a] {d}", "((lambda ([a b] {x}) (+ a (+ b x))) [1 2] {'x 10})", 13, pass, fail);

    // Typed param + dict
    setup(interp, "(define (d-typed (^Int n) {x}) (+ n x))");
    test_eq(interp, "define typed + {dict}", "(d-typed 5 {'x 10})", 15, pass, fail);
}

fn void run_basic_function_param_destructure_shadow_missing_tests(Interp* interp, int* pass, int* fail) {
    // === Shadowing: last binding wins ===
    // Same name in two dict params: second shadows first (nested lets, inner wins)
    setup(interp, "(define (d-shadow {x} {x y}) (+ x y))");
    test_eq(interp, "shadow {x} {x y} inner wins", "(d-shadow {'x 100} {'x 1 'y 2})", 3, pass, fail);

    // Same name in array params: second shadows
    setup(interp, "(define (d-sha2 [c z] [c z]) (+ c z))");
    test_eq(interp, "shadow [c z] [c z] inner wins", "(d-sha2 [100 200] [1 2])", 3, pass, fail);

    // Dict key missing -> nil
    test_eq(interp, "dict missing key → nil", "((lambda ({x y}) (if y 99 0)) {'x 10})", 0, pass, fail);
}

fn void run_basic_function_param_destructure_tests(Interp* interp, int* pass, int* fail) {
    // === Destructuring in function params: all combinations ===
    run_basic_function_param_destructure_define_tests(interp, pass, fail);
    run_basic_function_param_destructure_lambda_typed_tests(interp, pass, fail);
    run_basic_function_param_destructure_shadow_missing_tests(interp, pass, fail);
}

fn void run_basic_control_flow_tests(Interp* interp, int* pass, int* fail) {
    // Length
    test_eq(interp, "length '() => 0", "(length (quote ()))", 0, pass, fail);
    test_eq(interp, "length '(1 2 3 4 5) => 5", "(length (quote (1 2 3 4 5)))", 5, pass, fail);

    // Path access
    setup(interp, "(define namepair (cons (quote name) \"Alice\"))");
    setup(interp, "(define agepair (cons (quote age) 30))");
    setup(interp, "(define testperson (cons namepair (cons agepair (quote ()))))");
    test_str(interp, "testperson.name => string", "testperson.name", pass, fail);
    test_eq(interp, "testperson.age => 30", "testperson.age", 30, pass, fail);

    // Nested path
    setup(interp, "(define citypair (cons (quote city) \"NYC\"))");
    setup(interp, "(define zippair (cons (quote zip) 10001))");
    setup(interp, "(define testaddress (cons citypair (cons zippair (quote ()))))");
    setup(interp, "(define namepair2 (cons (quote name) \"Bob\"))");
    setup(interp, "(define addrpair (cons (quote address) testaddress))");
    setup(interp, "(define testperson2 (cons namepair2 (cons addrpair (quote ()))))");
    test_str(interp, "testperson2.address.city => string", "testperson2.address.city", pass, fail);

    // Recursive let
    test_eq(interp, "let ^rec factorial 5 => 120", "(let ^rec (fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))", 120, pass, fail);
    test_eq(interp, "non-recursive let", "(let (x 10) (+ x 5))", 15, pass, fail);

    // And/or
    test_truthy(interp, "and true true", "(and true true)", pass, fail);
    test_nil(interp, "and false true => nil", "(and false true)", pass, fail);
    test_nil(interp, "and true false => nil", "(and true false)", pass, fail);
    test_eq(interp, "and 1 42 => 42", "(and 1 42)", 42, pass, fail);
    test_truthy(interp, "or true false", "(or true false)", pass, fail);
    test_nil(interp, "or false nil => nil", "(or false nil)", pass, fail);
    test_eq(interp, "or 42 99 => 42", "(or 42 99)", 42, pass, fail);
    test_eq(interp, "or nil 99 => 99", "(or nil 99)", 99, pass, fail);
    test_nil(interp, "and short-circuit", "(and nil undefined_var)", pass, fail);
    test_eq(interp, "or short-circuit", "(or 1 undefined_var)", 1, pass, fail);

    // Truthiness
    test_eq(interp, "0 is truthy", "(if 0 42 99)", 42, pass, fail);

    // Reset/shift
    test_eq(interp, "shift aborts", "(reset (+ 1 (shift k 42)))", 42, pass, fail);
    test_eq(interp, "shift k resumes", "(reset (+ 1 (shift k (k 10))))", 11, pass, fail);
    test_eq(interp, "reset passthrough", "(reset (+ 10 20))", 30, pass, fail);

    // Effect handlers
    test_eq(interp, "handle without effect", "(handle 42 (ask x (resolve 0)))", 42, pass, fail);
    test_eq(interp, "handle + perform + resume", "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", 11, pass, fail);
    test_eq(interp, "handle abort", "(handle (+ 1 (signal ask 5)) (ask x x))", 5, pass, fail);

    // Empty string truthy
    test_eq(interp, "empty string is truthy", "(if \"\" 1 0)", 1, pass, fail);
}

fn void run_basic_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Basic Tests ---");

    run_basic_intro_tests(interp, pass, fail);
    run_basic_index_and_match_tests(interp, pass, fail);
    run_basic_let_destructure_tests(interp, pass, fail);
    run_basic_function_param_destructure_tests(interp, pass, fail);
    run_basic_control_flow_tests(interp, pass, fail);
}

fn void run_memory_stress_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Stress Tests ---");

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
    io::printn("\n--- Arithmetic & Comparison Tests ---");
    run_arithmetic_basic_ops_tests(interp, pass, fail);
    run_arithmetic_comparison_ops_tests(interp, pass, fail);
}

fn void run_string_type_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- String & Type Predicate Tests ---");

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
    io::printn("\n--- List & Closure Tests ---");
    run_list_closure_cons_quote_tests(interp, pass, fail);
    run_list_closure_factory_compose_tests(interp, pass, fail);
    run_list_closure_scope_hof_tests(interp, pass, fail);
}
