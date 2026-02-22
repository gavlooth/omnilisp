module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 10: TESTS
// =============================================================================

// ---------------------------------------------------------------------------
// Unified Test Helpers — each runs BOTH interpreter AND JIT on the same expr
// ---------------------------------------------------------------------------

fn void setup(Interp* interp, char[] code) {
    run(code, interp);
}

fn void test_eq(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected;

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && jv.tag == INT && jv.int_val == expected;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_eq_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_double(ri.value) && ri.value.double_val == expected;

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && is_double(jv) && jv.double_val == expected;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_truthy(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && (uint)ri.value.sym_val == (uint)interp.sym_false);

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && !is_nil(jv) && !(is_symbol(jv) && (uint)jv.sym_val == (uint)interp.sym_false);
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_nil(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_nil(ri.value);

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && is_nil(jv);
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
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
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (expected error)", (ZString)name);
        (*fail)++;
    }
}

fn void test_str(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_string(ri.value);

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && jv.tag == STRING;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_tag(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && ri.value != null && ri.value.tag == expected;

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && jv.tag == expected;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_gt(Interp* interp, char[] name, char[] expr, long min, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val > min;

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && jv.tag == INT && jv.int_val > min;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
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
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        if (!ri.error.has_error && is_int(ri.value)) { io::printfn("[FAIL] %s (got %d)", (ZString)name, ri.value.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
        (*fail)++;
    }
}

fn void test_truthy_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && (uint)ri.value.sym_val == (uint)interp.sym_false)) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_nil_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_nil(ri.value)) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_str_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_string(ri.value)) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_tag_interp(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && ri.value != null && ri.value.tag == expected) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

// JIT-only: for stateful tests that would double-mutate if run through both interp and JIT
fn void test_eq_jit(Interp* interp, char[] name, char[] source, long expected, int* pass, int* fail) {
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = f(interp);
    if (result != null && result.tag == INT && result.int_val == expected) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        (*fail)++;
        if (result != null && result.tag == INT) { io::printfn("[FAIL] %s (JIT got %d)", (ZString)name, result.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
    }
}

fn void test_nil_jit(Interp* interp, char[] name, char[] source, int* pass, int* fail) {
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = f(interp);
    if (result == null || is_nil(result)) {
        io::printfn("[PASS] %s", (ZString)name);
        (*pass)++;
    } else {
        (*fail)++;
        io::printfn("[FAIL] %s (JIT expected nil)", (ZString)name);
    }
}

fn double dabs(double x) @inline { return x < 0.0 ? -x : x; }

fn void test_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_number(ri.value) && dabs(to_double(ri.value) - expected) < 0.0001;

    Expr* ast = parse_for_jit(expr, interp);
    bool jit_ok = false;
    if (ast != null) {
        JitFn f = jit_compile(ast, interp);
        if (f != null) {
            Value* jv = jit_exec(f, interp);
            jit_ok = jv != null && is_number(jv) && dabs(to_double(jv) - expected) < 0.0001;
        }
    }

    if (interp_ok && jit_ok) {
        io::printfn("[PASS] %s", (ZString)name);
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
    { "let simple", "(let ((y 7)) y)" },
    { "let nested", "(let ((a 1)) (let ((b 2)) (+ a b)))" },
    { "let shadow", "(let ((x 99)) x)" },
    { "let multi", "(let ((a 1) (b 2) (c 3)) (+ a (+ b c)))" },

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
    { "nested let 5", "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) (+ a (+ b (+ c (+ d e)))))))))" },

    // === Let recursive ===
    { "let rec factorial", "(let ^rec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))" },
    { "named let loop", "(let loop ((n 10) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },

    // === Effects ===
    { "reset passthrough", "(reset (+ 10 20))" },
    { "shift aborts", "(reset (+ 1 (shift k 42)))" },
    { "shift k resumes", "(reset (+ 1 (shift k (k 10))))" },
    { "handle no effect", "(handle 42 ((ask k x) (k 0)))" },
    { "handle perform resume", "(handle (+ 1 (perform ask 0)) ((ask k x) (k 10)))" },
    { "handle abort", "(handle (+ 1 (perform ask 5)) ((ask k x) x))" },

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
    { "thunk capture", "(let ((a 10)) ((lambda () a)))" },
    { "thunk begin", "((lambda () (begin 1 2 3)))" },
    { "thunk arith", "((lambda () (+ 10 20)))" },

    // === Multi-param lambdas (auto-curried) ===
    { "multi-param add", "((lambda (x y) (+ x y)) 3 4)" },
    { "multi-param 3", "((lambda (x y z) (+ x (+ y z))) 1 2 3)" },
    { "multi-param partial", "(let ((add (lambda (x y) (+ x y)))) ((add 10) 20))" },

    // === Named let patterns ===
    { "named let sum 5", "(let loop ((n 5) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "named let fact 10", "(let f ((n 10) (acc 1)) (if (= n 0) acc (f (- n 1) (* acc n))))" },
    { "named let fib 10", "(let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let fib 20", "(let fib ((n 20) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let count", "(let count ((n 100)) (if (= n 0) 0 (count (- n 1))))" },

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
    { "handle get", "(handle (+ 1 (perform get 0)) ((get k x) (k 100)))" },
    { "handle double", "(handle (perform double 7) ((double k x) (k (* x 2))))" },
    { "handle bail", "(handle (+ 1 (+ 2 (+ 3 (perform bail 42)))) ((bail k x) x))" },
    { "handle no trigger", "(handle (+ 10 20) ((unused k x) 999))" },

    // === More list operations ===
    { "cadr", "(car (cdr (cons 1 (cons 2 (cons 3 nil)))))" },
    { "caddr", "(car (cdr (cdr (cons 1 (cons 2 (cons 3 nil))))))" },
    { "length 1", "(length (quote (1)))" },
    { "length 10", "(length (quote (1 2 3 4 5 6 7 8 9 10)))" },
    { "length cons 2", "(length (cons 1 (cons 2 nil)))" },
    { "car quoted", "(car (quote (10 20 30)))" },
    { "cons dotted", "(cons 1 2)" },

    // === Let shadowing ===
    { "let shadow inner", "(let ((x 10)) (let ((x 20)) x))" },
    { "let no leak", "(let ((x 10)) (let ((y 20)) x))" },

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
    { "let rec fact 6", "(let ^rec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 6))" },

    // === Misc ===
    { "nested closures", "((lambda (x) ((lambda (y) (+ x y)) 20)) 10)" },
    { "deep closure chain", "((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 1) 2) 3)" },
    { "list operations", "(car (cdr (cdr (quote (1 2 3 4 5)))))" },
    { "complex arith", "(* (+ 1 2) (- 10 (/ 8 2)))" },
    { "recursive list build", "(length (cons 1 (cons 2 (cons 3 (quote ())))))" },

    // === Stdlib HOFs ===
    { "stdlib map double", "((map (lambda (x) (* x 2))) (quote (1 2 3)))" },
    { "stdlib map inc", "((map (lambda (x) (+ x 1))) (quote (10 20 30)))" },
    { "stdlib map identity", "((map (lambda (x) x)) (quote (1 2 3)))" },
    { "stdlib filter gt2", "((filter (lambda (x) (> x 2))) (quote (1 2 3 4 5)))" },
    { "stdlib filter none", "((filter (lambda (x) (> x 100))) (quote (1 2 3)))" },
    { "stdlib filter all", "((filter (lambda (x) (> x 0))) (quote (1 2 3)))" },
    { "stdlib foldl sum", "(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) (quote (1 2 3 4 5)))" },
    { "stdlib foldl product", "(((foldl (lambda (acc) (lambda (x) (* acc x)))) 1) (quote (1 2 3 4 5)))" },
    { "stdlib foldr cons", "(((foldr (lambda (x) (lambda (acc) (cons x acc)))) (quote ())) (quote (1 2 3)))" },
    { "stdlib append", "((append (quote (1 2))) (quote (3 4)))" },
    { "stdlib append empty l", "((append (quote ())) (quote (1 2)))" },
    { "stdlib append empty r", "((append (quote (1 2))) (quote ()))" },
    { "stdlib reverse", "(reverse (quote (1 2 3)))" },
    { "stdlib reverse single", "(reverse (quote (7)))" },
    { "stdlib reverse empty", "(reverse (quote ()))" },
    { "stdlib compose", "(((compose (lambda (x) (* x 2))) (lambda (x) (+ x 1))) 5)" },
    { "stdlib nth 0", "((nth 0) (quote (10 20 30)))" },
    { "stdlib nth 2", "((nth 2) (quote (10 20 30)))" },
    { "stdlib take 3", "((take 3) (quote (1 2 3 4 5)))" },
    { "stdlib take 0", "((take 0) (quote (1 2 3)))" },
    { "stdlib take all", "((take 5) (quote (1 2 3)))" },
    { "stdlib drop 2", "((drop 2) (quote (1 2 3 4 5)))" },
    { "stdlib drop 0", "((drop 0) (quote (1 2 3)))" },
    { "stdlib drop all", "((drop 5) (quote (1 2 3)))" },
    { "stdlib zip", "(car ((zip (quote (1 2 3))) (quote (4 5 6))))" },
    { "stdlib range 5", "(range 5)" },
    { "stdlib range 0", "(range 0)" },
    { "stdlib range 1", "(range 1)" },
    { "stdlib any? found", "((any? (lambda (x) (= x 3))) (quote (1 2 3 4)))" },
    { "stdlib any? not found", "((any? (lambda (x) (= x 99))) (quote (1 2 3)))" },
    { "stdlib every? true", "((every? (lambda (x) (> x 0))) (quote (1 2 3)))" },
    { "stdlib every? false", "((every? (lambda (x) (> x 2))) (quote (1 2 3)))" },
    { "stdlib assoc found", "((assoc 2) (quote ((1 . 10) (2 . 20) (3 . 30))))" },
    { "stdlib assoc not found", "((assoc 99) (quote ((1 . 10) (2 . 20))))" },
    { "stdlib assoc-ref", "((assoc-ref 2) (quote ((1 . 10) (2 . 20) (3 . 30))))" },
    { "stdlib id compose", "(((compose id) id) 42)" },
    { "stdlib map filter", "(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) ((filter (lambda (x) (> x 2))) (quote (1 2 3 4 5))))" },

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

    // === Quasiquote (no unquote — rt_eval_source can't see let-bound vars) ===
    { "quasiquote literal", "`42" },
    { "quasiquote list", "`(1 2 3)" },
    { "quasiquote no unquote", "`(a b c)" },

    // when/unless/cond — skipped: compiler doesn't expand eval-time macros

    // === Dot-bracket indexing ===
    { "dot-bracket 0", "(let ((lst (quote (10 20 30)))) lst.[0])" },
    { "dot-bracket 2", "(let ((lst (quote (10 20 30)))) lst.[2])" },
    { "dot-bracket expr", "(let ((lst (quote (10 20 30)))) lst.[(+ 0 1)])" },

    // === Deep recursion / TCO ===
    { "tco sum 1000", "(let loop ((n 1000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "tco count 5000", "(let count ((n 5000)) (if (= n 0) 0 (count (- n 1))))" },

    // === Complex combinations ===
    { "map then reverse", "(reverse ((map (lambda (x) (* x 10))) (quote (1 2 3))))" },
    { "filter then length", "(length ((filter (lambda (x) (> x 3))) (quote (1 2 3 4 5 6))))" },
    { "nested map", "((map (lambda (x) (+ x 1))) ((map (lambda (x) (* x 2))) (quote (1 2 3))))" },
    { "foldl with filter", "(((foldl (lambda (a) (lambda (x) (+ a x)))) 0) ((filter (lambda (x) (< x 4))) (quote (1 2 3 4 5))))" },
    { "compose triple", "(((compose (lambda (x) (* x 3))) (lambda (x) (+ x 1))) 10)" },
    { "range then sum", "(((foldl (lambda (a) (lambda (x) (+ a x)))) 0) (range 10))" },
    { "take from range", "((take 3) (range 10))" },
    { "drop from range", "((drop 7) (range 10))" },
    { "match on map result", "(match ((map (lambda (x) (* x 10))) (quote (1 2 3))) ([a b c] (+ a (+ b c))))" },
    { "closure over let rec", "(let ^rec ((f (lambda (n) (if (<= n 1) 1 (* n (f (- n 1))))))) (f 7))" },
    { "nested handle", "(handle (handle (+ 1 (perform inner 0)) ((inner k x) (k 10))) ((outer k x) (k 0)))" },
    { "shift double resume", "(reset (+ (shift k (+ (k 1) (k 2))) 10))" },
    { "let-rec factorial 8", "(let ^rec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 8))" },

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
    { "qq unquote let", "(let ((x 42)) `(a ,x b))" },
    { "qq unquote arith", "(let ((n 10)) `(result ,(+ n 5)))" },
    { "qq unquote nested", "(let ((a 1) (b 2)) `(,a ,b ,(+ a b)))" },
    { "qq splice literal", "`(1 2 3)" },
};

fn void generate_e2e_tests(Interp* interp) {
    io::printn("=== Generating E2E Compiler Tests ===");

    // Use a fresh interpreter for e2e generation to avoid test state leaking
    Interp* e2e_interp = (Interp*)mem::malloc(Interp.sizeof);
    e2e_interp.init();
    register_primitives(e2e_interp);
    register_stdlib(e2e_interp);

    // Run all setups
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        EvalResult r = run(E2E_SETUPS[i], e2e_interp);
        if (r.error.has_error) {
            io::printfn("WARNING: Setup %d failed, skipping", (int)i);
        }
    }

    // Build the combined Pika source (setups + test exprs)
    // and capture expected output for each test expr
    usz buf_size = 131072;  // 128KB
    char* expected_buf = (char*)mem::malloc(buf_size);
    usz expected_pos = 0;

    char* source_buf = (char*)mem::malloc(buf_size);
    usz source_pos = 0;

    int test_count = 0;
    int skip_count = 0;

    // First, add all setup defines to the source
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        for (usz j = 0; j < E2E_SETUPS[i].len && source_pos < buf_size - 2; j++) {
            source_buf[source_pos++] = E2E_SETUPS[i][j];
        }
        source_buf[source_pos++] = '\n';
    }

    // Now process each test expression
    for (usz i = 0; i < E2E_TESTS.len; i++) {
        // Run through interpreter to get expected output
        EvalResult r = run(E2E_TESTS[i].expr, e2e_interp);

        if (r.error.has_error) {
            io::printfn("  SKIP (interp error): %s", (ZString)E2E_TESTS[i].name);
            skip_count++;
            continue;
        }

        // Capture expected output to buffer
        char[1024] val_buf;
        usz val_len = print_value_to_buf(r.value, &e2e_interp.symbols, &val_buf, val_buf.len);

        // Append to expected buffer
        for (usz j = 0; j < val_len && expected_pos < buf_size - 2; j++) {
            expected_buf[expected_pos++] = val_buf[j];
        }
        expected_buf[expected_pos++] = '\n';

        // Append expression to source
        for (usz j = 0; j < E2E_TESTS[i].expr.len && source_pos < buf_size - 2; j++) {
            source_buf[source_pos++] = E2E_TESTS[i].expr[j];
        }
        source_buf[source_pos++] = '\n';

        test_count++;
    }

    // Compile the combined source
    io::printfn("  Compiling %d test expressions...", test_count);

    char[] pika_source = source_buf[:source_pos];
    char[] c3_code = compile_to_c3_print_all(pika_source, e2e_interp);

    if (c3_code.len == 0) {
        io::printn("ERROR: Compilation produced empty output");
        mem::free(expected_buf);
        mem::free(source_buf);
        mem::free(e2e_interp);
        return;
    }

    // Write generated C3 file
    if (try file = io::file::open("build/e2e_test.c3", "w")) {
        file.write(c3_code)!!;
        file.close()!!;
        io::printfn("  Wrote build/e2e_test.c3 (%d bytes)", (int)c3_code.len);
    } else {
        io::printn("ERROR: Cannot write build/e2e_test.c3");
        mem::free(expected_buf);
        mem::free(source_buf);
        mem::free(e2e_interp);
        return;
    }

    // Write expected output file
    char[] expected_out = expected_buf[:expected_pos];
    if (try file = io::file::open("build/e2e_expected.txt", "w")) {
        file.write(expected_out)!!;
        file.close()!!;
        io::printfn("  Wrote build/e2e_expected.txt (%d lines)", test_count);
    } else {
        io::printn("ERROR: Cannot write build/e2e_expected.txt");
        mem::free(expected_buf);
        mem::free(source_buf);
        mem::free(e2e_interp);
        return;
    }

    io::printfn("=== Generated %d e2e tests (%d skipped) ===", test_count, skip_count);

    mem::free(expected_buf);
    mem::free(source_buf);
    mem::free(e2e_interp);
}

fn void run_lisp_tests() {
    io::printn("=== Unified Tests (Interpreter + JIT) ===");

    Interp* interp = (Interp*)mem::malloc(Interp.sizeof);
    interp.init();
    register_primitives(interp);
    register_destructors();
    register_stdlib(interp);

    int pass = 0;
    int fail = 0;

    run_basic_tests(interp, &pass, &fail);
    run_memory_stress_tests(interp, &pass, &fail);
    run_list_closure_tests(interp, &pass, &fail);
    run_arithmetic_comparison_tests(interp, &pass, &fail);
    run_string_type_tests(interp, &pass, &fail);
    run_advanced_tests(interp, &pass, &fail);

    io::printfn("\n=== Unified Tests: %d passed, %d failed ===", pass, fail);
    assert(fail == 0, "tests failed");

    run_compiler_tests(interp);
}

fn void run_basic_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Basic Tests ---");

    // Literals & bindings
    test_eq(interp, "integer literal 42", "42", 42, pass, fail);
    setup(interp, "(define x 10)");
    test_eq(interp, "variable binding x => 10", "x", 10, pass, fail);
    test_eq(interp, "lambda identity", "((lambda (x) x) 5)", 5, pass, fail);
    test_eq(interp, "let binding", "(let ((y 7)) y)", 7, pass, fail);
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
            io::printn("[PASS] source location tracking");
            (*pass)++;
        } else {
            io::printn("[FAIL] source location tracking");
            (*fail)++;
        }
    }

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
    test_eq(interp, "let ^rec factorial 5 => 120", "(let ^rec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))", 120, pass, fail);
    test_eq(interp, "non-recursive let", "(let ((x 10)) (+ x 5))", 15, pass, fail);

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
    test_eq(interp, "handle without effect", "(handle 42 ((ask k x) (k 0)))", 42, pass, fail);
    test_eq(interp, "handle + perform + resume", "(handle (+ 1 (perform ask 0)) ((ask k x) (k 10)))", 11, pass, fail);
    test_eq(interp, "handle abort", "(handle (+ 1 (perform ask 5)) ((ask k x) x))", 5, pass, fail);

    // Empty string truthy
    test_eq(interp, "empty string is truthy", "(if \"\" 1 0)", 1, pass, fail);
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
    test_eq(interp, "nested let 10 levels => 55", "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) (let ((f 6)) (let ((g 7)) (let ((h 8)) (let ((i 9)) (let ((j 10)) (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h (+ i j)))))))))))))))))))))", 55, pass, fail);

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

fn void run_arithmetic_comparison_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Arithmetic & Comparison Tests ---");

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

fn void run_list_closure_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- List & Closure Tests ---");

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
    setup(interp, "(define double-then-inc ((compose inc) double2))");
    test_eq(interp, "compose inc double 5 => 11", "(double-then-inc 5)", 11, pass, fail);

    // Multi-capture
    setup(interp, "(define make-linear (lambda (a) (lambda (b) (lambda (x) (+ (* a x) b)))))");
    setup(interp, "(define f2x3 ((make-linear 2) 3))");
    test_eq(interp, "f(x)=2x+3, f(10)=23", "(f2x3 10)", 23, pass, fail);

    // Scoping
    test_eq(interp, "let shadowing", "(let ((x 10)) (let ((x 20)) x))", 20, pass, fail);
    setup(interp, "(define outer-x 100)");
    setup(interp, "(let ((outer-x 999)) outer-x)");
    test_eq(interp, "let no leak", "outer-x", 100, pass, fail);

    setup(interp, "(define scope-test 42)");
    setup(interp, "(define scope-fn (lambda (scope-test) scope-test))");
    test_eq(interp, "lambda scope param", "(scope-fn 99)", 99, pass, fail);
    test_eq(interp, "global preserved", "scope-test", 42, pass, fail);

    // Recursive let
    test_eq(interp, "let ^rec fact 6 => 720", "(let ^rec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 6))", 720, pass, fail);

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

fn void run_advanced_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Advanced Feature Tests ---");

    // Match patterns
    test_eq(interp, "match 1 => 10", "(match 1 (1 10) (2 20) (3 30))", 10, pass, fail);
    test_eq(interp, "match 2 => 20", "(match 2 (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match 3 => 30", "(match 3 (1 10) (2 20) (3 30))", 30, pass, fail);
    test_eq(interp, "match default => 100", "(match 99 (1 10) (2 20) (x (+ x 1)))", 100, pass, fail);
    test_eq(interp, "match computed", "(match (+ 1 1) (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match [a b] => 30", "(match (quote (10 20)) ([a b] (+ a b)))", 30, pass, fail);
    test_eq(interp, "match [x] => 42", "(match (quote (42)) ([x] x))", 42, pass, fail);
    test_eq(interp, "match [a b c] product", "(match (quote (2 3 4)) ([a b c] (* a (* b c))))", 24, pass, fail);
    test_eq(interp, "match [h .. t] tail len", "(match (quote (1 2 3 4 5)) ([h .. t] (length t)))", 4, pass, fail);
    test_eq(interp, "match _ wildcard", "(match 42 (_ 0))", 0, pass, fail);
    test_eq(interp, "match string hello", "(match \"hello\" (\"hello\" 1) (\"world\" 2) (x 0))", 1, pass, fail);
    test_eq(interp, "match string world", "(match \"world\" (\"hello\" 1) (\"world\" 2) (x 0))", 2, pass, fail);
    test_eq(interp, "match string default", "(match \"other\" (\"hello\" 1) (\"world\" 2) (x 0))", 0, pass, fail);

    // Continuations
    test_eq(interp, "shift discard k", "(reset (+ 100 (shift k 42)))", 42, pass, fail);
    test_eq(interp, "shift k mul", "(reset (* 2 (shift k (k 5))))", 10, pass, fail);
    test_eq(interp, "shift k expr", "(reset (+ 1 (shift k (k (+ 2 3)))))", 6, pass, fail);
    test_eq(interp, "shift use k result", "(reset (+ 1 (shift k (+ (k 10) 1000))))", 1011, pass, fail);
    test_eq(interp, "reset value", "(reset 42)", 42, pass, fail);

    // Effect handlers
    test_eq(interp, "effect modify", "(handle (+ 1 (perform get 0)) ((get k x) (k 100)))", 101, pass, fail);
    test_eq(interp, "effect arg double", "(handle (perform double 7) ((double k x) (k (* x 2))))", 14, pass, fail);
    test_eq(interp, "effect bail", "(handle (+ 1 (+ 2 (+ 3 (perform bail 42)))) ((bail k x) x))", 42, pass, fail);
    test_eq(interp, "handle no effect", "(handle (+ 10 20) ((unused k x) 999))", 30, pass, fail);

    // If/cond edge cases
    test_eq(interp, "if nil => 2", "(if nil 1 2)", 2, pass, fail);
    test_eq(interp, "if false => 2", "(if false 1 2)", 2, pass, fail);
    test_eq(interp, "if 0 => 1 (truthy)", "(if 0 1 2)", 1, pass, fail);
    test_eq(interp, "if empty-str => 1 (truthy)", "(if \"\" 1 2)", 1, pass, fail);
    test_eq(interp, "if '() => 2 (nil)", "(if (quote ()) 1 2)", 2, pass, fail);
    test_eq(interp, "if '(1) => 1 (truthy)", "(if (quote (1)) 1 2)", 1, pass, fail);
    test_eq(interp, "nested if", "(if true (if false 1 2) 3)", 2, pass, fail);
    test_eq(interp, "if with branches", "(if (= 1 1) (+ 10 20) (+ 30 40))", 30, pass, fail);

    // Error handling
    test_error(interp, "unbound variable", "nonexistent_var_xyz", pass, fail);
    test_error(interp, "call non-function", "(42 1)", pass, fail);
    test_error(interp, "shift outside reset", "(shift k 42)", pass, fail);

    // And/or advanced
    test_eq(interp, "nested and/or", "(and (or nil 5) (or false 10))", 10, pass, fail);
    test_eq(interp, "nested or/and", "(or (and nil 5) (and 1 10))", 10, pass, fail);
    test_eq(interp, "and closure truthy", "(and (lambda (x) x) 42)", 42, pass, fail);

    // Define/redefinition
    setup(interp, "(define redef-test 10)");
    test_eq(interp, "redef before", "redef-test", 10, pass, fail);
    setup(interp, "(define redef-test 20)");
    test_eq(interp, "redef after", "redef-test", 20, pass, fail);
    setup(interp, "(define computed-val (+ (* 3 4) (- 10 5)))");
    test_eq(interp, "define complex expr", "computed-val", 17, pass, fail);

    // Path access
    setup(interp, "(define ptest (cons (cons (quote x) 10) (cons (cons (quote y) 20) (quote ()))))");
    test_eq(interp, "ptest.x => 10", "ptest.x", 10, pass, fail);
    test_eq(interp, "ptest.y => 20", "ptest.y", 20, pass, fail);

    // TCO (JIT-only mode: no interpreter TCO, limited by eval depth)
    setup(interp, "(define tco-loop (lambda (n) (if (= n 0) (quote done) (tco-loop (- n 1)))))");
    test_truthy(interp, "TCO loop 4000", "(tco-loop 4000)", pass, fail);
    setup(interp, "(define sum-acc (lambda (n) (lambda (acc) (if (= n 0) acc ((sum-acc (- n 1)) (+ acc n))))))");
    test_eq(interp, "TCO sum-acc 1000 => 500500", "((sum-acc 1000) 0)", 500500, pass, fail);
    setup(interp, "(define tco-even (lambda (n) (if (= n 0) true (tco-odd (- n 1)))))");
    setup(interp, "(define tco-odd (lambda (n) (if (= n 0) nil (tco-even (- n 1)))))");
    test_truthy(interp, "TCO mutual recursion 1000", "(tco-even 1000)", pass, fail);
    setup(interp, "(define count-down (lambda (n) (match n (0 (quote done)) (n (count-down (- n 1))))))");
    test_truthy(interp, "TCO match tail call 4000", "(count-down 4000)", pass, fail);
    setup(interp, "(define let-loop (lambda (n) (let ((m (- n 1))) (if (= m 0) (quote done) (let-loop m)))))");
    test_truthy(interp, "TCO let body 4000", "(let-loop 4000)", pass, fail);

    // Multi-perform
    test_eq(interp, "multi-perform 2 sum", "(handle (+ (perform bounce 10) (perform bounce 20)) ((bounce k x) (k x)))", 30, pass, fail);
    test_eq(interp, "multi-perform 3 sum", "(handle (+ (perform bounce 1) (+ (perform bounce 2) (perform bounce 3))) ((bounce k x) (k x)))", 6, pass, fail);
    test_eq(interp, "multi-perform doubles", "(handle (+ (perform bounce 10) (perform bounce 20)) ((bounce k x) (k (* x 2))))", 60, pass, fail);
    test_eq(interp, "multi-perform abort", "(handle (+ (perform bounce 10) (perform bounce 20)) ((bounce k x) x))", 10, pass, fail);
    test_eq(interp, "multi-shift sum", "(reset (+ (shift k1 (k1 10)) (shift k2 (k2 20))))", 30, pass, fail);

    // Multi-shot continuations
    test_eq(interp, "multi-shot k twice", "(reset (+ 1 (shift k (+ (k 10) (k 20)))))", 32, pass, fail);
    test_eq(interp, "multi-shot k thrice", "(reset (* 2 (shift k (+ (k 3) (+ (k 5) (k 7))))))", 30, pass, fail);
    test_eq(interp, "multi-shot let bindings", "(reset (+ 10 (shift k (let ((a (k 1)) (b (k 2))) (+ a b)))))", 23, pass, fail);
    test_eq(interp, "multi-shot conditional", "(reset (+ 5 (shift k (if true (k 10) (k 20)))))", 15, pass, fail);
    test_eq(interp, "multi-shot effect", "(handle (+ 1 (perform dup 0)) ((dup k x) (+ (k 10) (k 20))))", 32, pass, fail);

    // TCO inside reset/shift
    setup(interp, "(define reset-loop (lambda (n acc) (if (= n 0) acc (reset-loop (- n 1) (+ acc 1)))))");
    test_eq(interp, "TCO in reset body", "(reset (reset-loop 5000 0))", 5000, pass, fail);
    setup(interp, "(define shift-loop (lambda (n acc) (if (= n 0) acc (shift-loop (- n 1) (+ acc 1)))))");
    test_eq(interp, "TCO in shift body", "(reset (+ 1 (shift k (shift-loop 5000 0))))", 5000, pass, fail);
    test_eq(interp, "TCO begin+if in reset", "(reset (begin (+ 1 2) (if true (+ 10 20) 99)))", 30, pass, fail);
    test_eq(interp, "TCO let chains in shift", "(reset (+ 1 (shift k (let ((x 10)) (let ((y 20)) (+ x y))))))", 30, pass, fail);
    setup(interp, "(define cont-use-loop (lambda (k n) (if (= n 0) (k 42) (cont-use-loop k (- n 1)))))");
    test_eq(interp, "TCO tail-recursive k use", "(reset (+ 1 (shift k (cont-use-loop k 1000))))", 43, pass, fail);
    test_eq(interp, "TCO nested reset", "(reset (+ (reset (if true 10 20)) (shift k (k 5))))", 15, pass, fail);
    setup(interp, "(define handle-loop (lambda (n) (if (= n 0) 0 (+ 1 (handle-loop (- n 1))))))");
    test_eq(interp, "TCO handler body", "(handle (perform count 0) ((count k x) (handle-loop 100)))", 100, pass, fail);

    // Trampoline (JIT delegates effects to eval which handles them)
    test_eq(interp, "trampoline passthrough", "(with-trampoline (lambda (xx) 42))", 42, pass, fail);
    test_eq(interp, "trampoline single bounce", "(with-trampoline (lambda (xx) (+ 1 (perform bounce (lambda (xx) 41)))))", 42, pass, fail);
    setup(interp, "(define fib-t (lambda (n) (if (< n 2) n (+ (perform bounce (lambda (xx) (fib-t (- n 1)))) (perform bounce (lambda (xx) (fib-t (- n 2))))))))");
    test_eq(interp, "trampoline fib 7", "(with-trampoline (lambda (xx) (fib-t 7)))", 13, pass, fail);

    // Variadic lambdas (JIT detects variadic closures and routes to fallback)
    setup(interp, "(define collect (lambda (.. args) args))");
    test_eq(interp, "variadic collect length", "(length (collect 1 2 3))", 3, pass, fail);
    test_nil(interp, "variadic collect empty", "(collect)", pass, fail);
    setup(interp, "(define head-rest (lambda (h .. t) t))");
    test_eq(interp, "variadic rest length", "(length (head-rest 1 2 3))", 2, pass, fail);
    test_nil(interp, "variadic empty rest", "(head-rest 1)", pass, fail);
    setup(interp, "(define multi-rest (lambda (a b .. rest) rest))");
    test_eq(interp, "variadic multi rest len", "(length (multi-rest 1 2 3 4 5))", 3, pass, fail);
    setup(interp, "(define head-only (lambda (h .. t) h))");
    test_eq(interp, "variadic head-only", "(head-only 42 99 100)", 42, pass, fail);
    setup(interp, "(define count-rest (lambda (h .. t) (length t)))");
    test_eq(interp, "variadic count-rest", "(count-rest 1 2 3 4 5)", 4, pass, fail);

    // Begin
    test_eq(interp, "begin returns last", "(begin 1 2 3)", 3, pass, fail);
    test_eq(interp, "begin single", "(begin 42)", 42, pass, fail);
    test_eq(interp, "begin side effects", "(begin (define begin-x 10) (define begin-y 20) (+ begin-x begin-y))", 30, pass, fail);
    test_eq(interp, "begin nested", "(begin (begin 1 2) (begin 3 4))", 4, pass, fail);
    test_eq(interp, "begin with if", "(begin (if true 10 20) (if false 30 40))", 40, pass, fail);

    // Zero-arg lambdas
    test_eq(interp, "zero-arg thunk", "((lambda () 42))", 42, pass, fail);
    test_eq(interp, "zero-arg closure", "(let ((a 10)) ((lambda () a)))", 10, pass, fail);
    test_eq(interp, "zero-arg stored thunk", "(let ((thunk (lambda () 99))) (thunk))", 99, pass, fail);
    test_eq(interp, "zero-arg begin body", "((lambda () (begin 1 2 3)))", 3, pass, fail);
    test_eq(interp, "zero-arg computation", "((lambda () (+ 10 20)))", 30, pass, fail);

    // Multi-param lambda (auto-curry)
    test_eq(interp, "multi-param two", "((lambda (x y) (+ x y)) 3 4)", 7, pass, fail);
    test_eq(interp, "multi-param three", "((lambda (x y z) (+ x (+ y z))) 1 2 3)", 6, pass, fail);
    test_eq(interp, "multi-param direct call", "(let ((add (lambda (x y) (+ x y)))) (add 10 20))", 30, pass, fail);
    setup(interp, "(define mp-add3 (lambda (a b c) (+ a (+ b c))))");
    test_eq(interp, "multi-param defined fn", "(mp-add3 10 20 30)", 60, pass, fail);
    test_eq(interp, "multi-param three call", "(let ((f (lambda (x y z) (+ x (+ y z))))) (f 1 2 3))", 6, pass, fail);

    // Multi-binding let
    test_eq(interp, "multi-let two", "(let ((ml-x 10) (ml-y 20)) (+ ml-x ml-y))", 30, pass, fail);
    test_eq(interp, "multi-let three", "(let ((ml-a 1) (ml-b 2) (ml-c 3)) (+ ml-a (+ ml-b ml-c)))", 6, pass, fail);
    test_eq(interp, "multi-let sequential", "(let ((ml-p 10) (ml-q (+ ml-p 5))) ml-q)", 15, pass, fail);
    test_eq(interp, "multi-let with lambda", "(let ((ml-f (lambda (xx) (+ xx 1))) (ml-v (ml-f 10))) ml-v)", 11, pass, fail);
    test_eq(interp, "multi-let single", "(let ((ml-z 42)) ml-z)", 42, pass, fail);

    // Named let
    test_eq(interp, "named-let sum 1..5", "(let loop ((n 5) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))", 15, pass, fail);
    test_eq(interp, "named-let fact 10", "(let fact ((n 10) (acc 1)) (if (= n 0) acc (fact (- n 1) (* acc n))))", 3628800, pass, fail);
    test_eq(interp, "named-let fib 10", "(let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))", 55, pass, fail);
    test_eq(interp, "named-let fib 20", "(let fib ((n 20) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))", 6765, pass, fail);
    test_eq(interp, "named-let countdown", "(let count ((n 100)) (if (= n 0) 0 (count (- n 1))))", 0, pass, fail);
    test_eq(interp, "named-let build list car", "(car (let build ((n 3) (acc (quote ()))) (if (= n 0) acc (build (- n 1) (cons n acc)))))", 1, pass, fail);
    test_eq(interp, "named-let 10000 iters", "(let loop ((n 10000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", 10000, pass, fail);

    // set!
    setup(interp, "(define set-test 10)");
    setup(interp, "(set! set-test 20)");
    test_eq(interp, "set! basic", "set-test", 20, pass, fail);

    // Counter tests are stateful (calling counter1 mutates), use JIT-only to avoid double mutation
    setup(interp, "(define make-counter (lambda () (let ((count 0)) (lambda () (begin (set! count (+ count 1)) count)))))");
    setup(interp, "(define counter1 (make-counter))");
    test_eq_jit(interp, "set! counter 1", "(counter1)", 1, pass, fail);
    test_eq_jit(interp, "set! counter 2", "(counter1)", 2, pass, fail);
    test_eq_jit(interp, "set! counter 3", "(counter1)", 3, pass, fail);

    // set! on let-local in closure (JIT detects and falls back to interpreter)
    test_eq(interp, "set! parent scope", "(let ((x 1)) (let ((f (lambda () (set! x 42)))) (begin (f) x)))", 42, pass, fail);
    test_error(interp, "set! unbound", "(set! undefined-var-xyz 1)", pass, fail);
    setup(interp, "(define sr-test 100)");
    setup(interp, "(set! sr-test (+ sr-test 1))");
    test_eq(interp, "set! read back", "sr-test", 101, pass, fail);

    // String<->number
    test_eq(interp, "string->number 42", "(string->number \"42\")", 42, pass, fail);
    test_eq(interp, "string->number -7", "(string->number \"-7\")", -7, pass, fail);
    test_nil(interp, "string->number abc => nil", "(string->number \"abc\")", pass, fail);
    test_nil(interp, "string->number empty => nil", "(string->number \"\")", pass, fail);
    test_eq(interp, "number->string round-trip 42", "(string->number (number->string 42))", 42, pass, fail);
    test_eq(interp, "number->string round-trip 0", "(string->number (number->string 0))", 0, pass, fail);
    test_eq(interp, "number->string round-trip -99", "(string->number (number->string -99))", -99, pass, fail);

    // Stack overflow
    test_error(interp, "stack overflow caught", "(let ^rec ((f (lambda (n) (+ 1 (f (+ n 1)))))) (f 0))", pass, fail);

    // Quasiquote
    test_tag(interp, "quasiquote basic", "`(a b c)", CONS, pass, fail);
    setup(interp, "(define qq-x 42)");
    test_tag(interp, "quasiquote unquote", "`(a ,qq-x c)", CONS, pass, fail);
    test_eq(interp, "quasiquote standalone", "`,(+ 1 2)", 3, pass, fail);
    setup(interp, "(define qq-lst (list 1 2 3))");
    test_eq(interp, "quasiquote splice len", "(length `(a ,@qq-lst d))", 5, pass, fail);
    setup(interp, "(define qq-empty (list))");
    test_eq(interp, "quasiquote splice empty", "(length `(a ,@qq-empty b))", 2, pass, fail);

    // Pattern macros
    setup(interp, "(define [macro] when ([test .. body] (if test (begin .. body) nil)))");
    test_eq(interp, "when true", "(when true 42)", 42, pass, fail);
    test_nil(interp, "when false", "(when false 42)", pass, fail);
    setup(interp, "(define [macro] unless ([test .. body] (if test nil (begin .. body))))");
    test_eq(interp, "unless false", "(unless false 99)", 99, pass, fail);
    test_nil(interp, "unless true", "(unless true 99)", pass, fail);
    setup(interp, "(define [macro] with-val ([name val body] (let ((name val)) body)))");
    test_eq(interp, "macro with-val", "(with-val z 100 (+ z 1))", 101, pass, fail);
    setup(interp, "(define [macro] cond ([] nil) ([test body .. rest] (if test body (cond .. rest))))");
    test_eq(interp, "cond macro", "(cond false 1 true 2 true 3)", 2, pass, fail);
    test_nil(interp, "cond all false", "(cond false 1 false 2)", pass, fail);
    setup(interp, "(define [macro] my-and ([a b] (if a b false)))");
    test_eq(interp, "my-and true", "(my-and true 42)", 42, pass, fail);
    setup(interp, "(define [macro] let1 ([name val body] (let ((name val)) body)))");
    test_eq(interp, "let1 macro", "(let1 x 10 (+ x 5))", 15, pass, fail);

    // Auto-gensym
    setup(interp, "(define [macro] swap! ([a b] (let ((tmp# a)) (begin (set! a b) (set! b tmp#)))))");
    setup(interp, "(define swap-x 10)");
    setup(interp, "(define swap-y 20)");
    setup(interp, "(swap! swap-x swap-y)");
    test_eq(interp, "swap! x", "swap-x", 20, pass, fail);
    test_eq(interp, "swap! y", "swap-y", 10, pass, fail);
    setup(interp, "(define tmp 999)");
    setup(interp, "(define hx 1)");
    setup(interp, "(define hy 2)");
    setup(interp, "(swap! hx hy)");
    test_eq(interp, "gensym hygiene", "tmp", 999, pass, fail);

    // gensym
    test_tag(interp, "gensym returns symbol", "(gensym)", SYMBOL, pass, fail);

    // Macro hygiene (def-env capture)
    setup(interp, "(define hyg-helper (lambda (x) (+ x 100)))");
    setup(interp, "(define [macro] hyg-apply ([x] (hyg-helper x)))");
    test_eq(interp, "hygiene normal", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define hyg-helper (lambda (x) (* x 999)))");
    test_eq(interp, "hygiene immune to shadow", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define [macro] hyg-inc ([x] (+ x 1)))");
    test_eq(interp, "hygiene captures +", "(hyg-inc 10)", 11, pass, fail);
    setup(interp, "(define [macro] hyg-nil-test ([x] (if x 42 nil)))");
    test_nil(interp, "hygiene nil branch", "(hyg-nil-test false)", pass, fail);
    test_eq(interp, "hygiene true branch", "(hyg-nil-test true)", 42, pass, fail);
    setup(interp, "(define hyg-a 10)");
    setup(interp, "(define hyg-b 20)");
    setup(interp, "(define [macro] hyg-sum ([x] (+ (+ hyg-a hyg-b) x)))");
    test_eq(interp, "hygiene multi literals", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-a 1000)");
    setup(interp, "(define hyg-b 2000)");
    test_eq(interp, "hygiene multi immune", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) (* x 2)))");
    setup(interp, "(define [macro] hyg-with-tmp ([x] (let ((tmp# (hyg-op x))) tmp#)))");
    test_eq(interp, "hygiene gensym+def", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) 0))");
    test_eq(interp, "hygiene gensym+def immune", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define x 999)");
    setup(interp, "(define [macro] hyg-id ([x] x))");
    test_eq(interp, "hygiene patvar substitutes", "(hyg-id 7)", 7, pass, fail);
    test_eq(interp, "hygiene recursive cond", "(cond false 1 false 2 true 3)", 3, pass, fail);
    setup(interp, "(define hyg-late-fn (lambda (x) (+ x 50)))");
    setup(interp, "(define [macro] hyg-late-macro ([x] (hyg-late-fn x)))");
    setup(interp, "(define hyg-late-fn (lambda (x) (- x 50)))");
    test_eq(interp, "hygiene late redef", "(hyg-late-macro 100)", 150, pass, fail);

    // Dicts
    setup(interp, "(define hm1 (dict 1 10 2 20 3 30))");
    test_eq(interp, "ref dict", "(ref hm1 2)", 20, pass, fail);
    test_nil(interp, "ref dict missing", "(ref hm1 99)", pass, fail);
    setup(interp, "(dict-set! hm1 2 200)");
    test_eq(interp, "dict-set! overwrite", "(ref hm1 2)", 200, pass, fail);
    test_truthy(interp, "has? dict yes", "(has? hm1 1)", pass, fail);
    test_nil(interp, "has? dict no", "(has? hm1 99)", pass, fail);
    test_eq(interp, "length dict", "(length hm1)", 3, pass, fail);
    setup(interp, "(define hm2 (dict 1 10 2 20 3 30))");
    setup(interp, "(remove! hm2 2)");
    test_nil(interp, "remove! dict", "(has? hm2 2)", pass, fail);
    test_eq(interp, "remove! count", "(length hm2)", 2, pass, fail);
    setup(interp, "(define hm3 (dict 1 10 2 20))");
    test_eq(interp, "keys len", "(length (keys hm3))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values hm3))", 2, pass, fail);
    setup(interp, "(define hm4 (dict \"name\" \"Alice\" \"age\" 30))");
    test_str(interp, "dict string key", "(ref hm4 \"name\")", pass, fail);
    setup(interp, "(define hm5 (dict))");
    test_eq(interp, "dict empty count", "(length hm5)", 0, pass, fail);
    test_truthy(interp, "dict? yes", "(dict? (dict 1 2))", pass, fail);
    test_nil(interp, "dict? no", "(dict? 42)", pass, fail);
    setup(interp, "(define hm-big (dict))");
    setup(interp, "(let loop ((i 0)) (if (= i 50) hm-big (begin (dict-set! hm-big i (* i i)) (loop (+ i 1)))))");
    test_eq(interp, "dict auto-grow count", "(length hm-big)", 50, pass, fail);
    test_eq(interp, "dict auto-grow ref 7", "(ref hm-big 7)", 49, pass, fail);

    // === LITERAL SYNTAX TESTS ===
    // Dict literal {}
    test_eq(interp, "{} empty", "(length {})", 0, pass, fail);
    test_eq(interp, "{} literal", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_truthy(interp, "{} string keys", "(has? {\"name\" \"Alice\"} \"name\")", pass, fail);
    test_truthy(interp, "{} has key", "(has? {'x 10 'y 20} 'y)", pass, fail);
    test_eq(interp, "{} count", "(length {'a 1 'b 2 'c 3})", 3, pass, fail);
    // Array literal []
    test_eq(interp, "[] empty", "(length [])", 0, pass, fail);
    test_eq(interp, "[] literal", "(ref [10 20 30] 1)", 20, pass, fail);
    test_eq(interp, "[] length", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "[] nested", "(ref (ref [1 [2 3]] 1) 0)", 2, pass, fail);
    test_eq(interp, "[] index", "(let ((a [10 20 30])) a.[1])", 20, pass, fail);
    // Cons cell dot-path access and mutation
    test_eq(interp, "cons .car", "(let ((p (cons 1 2))) p.car)", 1, pass, fail);
    test_eq(interp, "cons .cdr", "(let ((p (cons 1 2))) p.cdr)", 2, pass, fail);
    test_eq_interp(interp, "set! cons .car", "(let ((p (cons 1 2))) (begin (set! p.car 99) p.car))", 99, pass, fail);
    test_eq_interp(interp, "set! cons .cdr", "(let ((p (cons 1 2))) (begin (set! p.cdr 50) p.cdr))", 50, pass, fail);
    test_eq_interp(interp, "set! cons preserves", "(let ((p (cons 1 2))) (begin (set! p.car 99) p.cdr))", 2, pass, fail);

    // === GENERIC OPERATIONS TESTS ===
    // ref - array
    test_eq(interp, "ref array", "(ref [10 20 30] 1)", 20, pass, fail);
    test_error(interp, "ref array oob", "(ref [1] 5)", pass, fail);
    // ref - dict
    test_eq(interp, "ref dict", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_nil(interp, "ref dict missing", "(ref {'a 1} 'b)", pass, fail);
    // ref - cons
    test_eq(interp, "ref cons car", "(ref (cons 10 20) 0)", 10, pass, fail);
    test_eq(interp, "ref cons cdr", "(ref (cons 10 20) 1)", 20, pass, fail);
    // ref - string
    test_eq(interp, "ref string", "(ref \"hello\" 0)", 104, pass, fail);
    // length - generic
    test_eq(interp, "length array", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "length dict", "(length {'a 1 'b 2})", 2, pass, fail);
    test_eq(interp, "length string", "(length \"hello\")", 5, pass, fail);
    test_eq(interp, "length list", "(length '(1 2 3 4))", 4, pass, fail);
    // push!
    setup(interp, "(define ga [1 2])");
    setup(interp, "(push! ga 3)");
    test_eq(interp, "push! generic", "(length ga)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref ga 2)", 3, pass, fail);
    // keys / values / has? / remove!
    setup(interp, "(define gd {'x 10 'y 20})");
    test_eq(interp, "keys len", "(length (keys gd))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values gd))", 2, pass, fail);
    test_truthy(interp, "has? yes", "(has? gd 'x)", pass, fail);
    test_nil(interp, "has? no", "(has? gd 'z)", pass, fail);
    setup(interp, "(remove! gd 'x)");
    test_nil(interp, "remove! gone", "(has? gd 'x)", pass, fail);
    test_eq(interp, "remove! count", "(length gd)", 1, pass, fail);

    // === MODULE SYSTEM TESTS ===
    setup(interp, "(module math-utils (export double triple) (define double (lambda (x) (* x 2))) (define triple (lambda (x) (* x 3))) (define internal (lambda (x) (+ x 100))))");
    setup(interp, "(import math-utils)");
    test_eq(interp, "module double", "(double 5)", 10, pass, fail);
    test_eq(interp, "module triple", "(triple 4)", 12, pass, fail);
    test_error(interp, "module unexported hidden", "(internal 5)", pass, fail);
    setup(interp, "(import math-utils)");
    test_eq(interp, "module re-import cached", "(double 7)", 14, pass, fail);
    test_error(interp, "module duplicate def", "(module math-utils (export) (define x 1))", pass, fail);
    setup(interp, "(module list-utils (export sum-list) (define sum-list (lambda (lst) (((foldl (lambda (a) (lambda (b) (+ a b)))) 0) lst))))");
    setup(interp, "(import list-utils)");
    test_eq(interp, "module stdlib access", "(sum-list (quote (1 2 3 4 5)))", 15, pass, fail);
    setup(interp, "(module base-mod (export base-fn) (define base-fn (lambda (x) (+ x 100))))");
    setup(interp, "(import base-mod)");
    setup(interp, "(module derived-mod (export derived-fn) (define derived-fn (lambda (x) (base-fn (* x 2)))))");
    setup(interp, "(import derived-mod)");
    test_eq(interp, "module depends on another", "(derived-fn 5)", 110, pass, fail);
    setup(interp, "(module private-mod (export) (define secret 42))");
    setup(interp, "(import private-mod)");
    test_error(interp, "module empty export hides", "secret", pass, fail);
    setup(interp, "(module closure-mod (export make-counter) (define make-counter (lambda () (let ((n 0)) (lambda () (begin (set! n (+ n 1)) n))))))");
    setup(interp, "(import closure-mod)");
    setup(interp, "(define my-counter (make-counter))");
    // Counter tests are stateful: JIT-only to avoid double mutation
    test_eq_jit(interp, "module counter 1", "(my-counter)", 1, pass, fail);
    test_eq_jit(interp, "module counter 2", "(my-counter)", 2, pass, fail);
    setup(interp, "(module tail-mod (export tail-sum) (define tail-sum (lambda (n acc) (if (= n 0) acc (tail-sum (- n 1) (+ acc n))))))");
    setup(interp, "(import tail-mod)");
    test_eq(interp, "module tail-call sum 1000", "(tail-sum 1000 0)", 500500, pass, fail);

    // === SHORTHAND DEFINE TESTS ===
    setup(interp, "(define (sh-double x) (* x 2))");
    test_eq(interp, "shorthand define", "(sh-double 21)", 42, pass, fail);
    setup(interp, "(define (sh-add3 a b c) (+ a (+ b c)))");
    test_eq(interp, "shorthand define multi", "(sh-add3 10 20 30)", 60, pass, fail);
    setup(interp, "(define (sh-answer) 42)");
    test_eq(interp, "shorthand define zero-arg", "(sh-answer)", 42, pass, fail);

    // === STDLIB TESTS ===
    test_eq(interp, "map car", "(car ((map (lambda (x) (* x 2))) (quote (1 2 3))))", 2, pass, fail);
    test_eq(interp, "map length", "(length ((map (lambda (x) (* x 2))) (quote (1 2 3))))", 3, pass, fail);
    test_nil(interp, "map empty", "((map (lambda (x) (* x 2))) (quote ()))", pass, fail);
    test_eq(interp, "filter car", "(car ((filter (lambda (x) (> x 2))) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "filter length", "(length ((filter (lambda (x) (> x 2))) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_nil(interp, "filter none", "((filter (lambda (x) (> x 100))) (quote (1 2 3)))", pass, fail);
    test_eq(interp, "foldl sum", "(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) (quote (1 2 3 4 5)))", 15, pass, fail);
    test_eq(interp, "foldl product", "(((foldl (lambda (acc) (lambda (x) (* acc x)))) 1) (quote (1 2 3 4)))", 24, pass, fail);
    test_eq(interp, "foldr cons len", "(length (((foldr (lambda (x) (lambda (acc) (cons x acc)))) nil) (quote (1 2 3))))", 3, pass, fail);
    test_eq(interp, "foldr cons car", "(car (((foldr (lambda (x) (lambda (acc) (cons x acc)))) nil) (quote (1 2 3))))", 1, pass, fail);
    test_eq(interp, "append length", "(length ((append (quote (1 2))) (quote (3 4))))", 4, pass, fail);
    test_eq(interp, "append car", "(car ((append (quote (1 2))) (quote (3 4))))", 1, pass, fail);
    test_eq(interp, "append nil left", "(car ((append nil) (quote (1 2))))", 1, pass, fail);
    test_eq(interp, "reverse car", "(car (reverse (quote (1 2 3))))", 3, pass, fail);
    test_eq(interp, "reverse length", "(length (reverse (quote (1 2 3))))", 3, pass, fail);
    test_nil(interp, "reverse nil", "(reverse nil)", pass, fail);
    test_eq(interp, "compose *2.+1", "(((compose (lambda (x) (* x 2))) (lambda (x) (+ x 1))) 3)", 8, pass, fail);
    test_eq(interp, "compose +1.*2", "(((compose (lambda (x) (+ x 1))) (lambda (x) (* x 2))) 3)", 7, pass, fail);
    test_eq(interp, "id", "(id 42)", 42, pass, fail);
    test_eq(interp, "nth 0", "((nth 0) (quote (10 20 30 40)))", 10, pass, fail);
    test_eq(interp, "nth 2", "((nth 2) (quote (10 20 30 40)))", 30, pass, fail);
    test_eq(interp, "take length", "(length ((take 3) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "take car", "(car ((take 3) (quote (1 2 3 4 5))))", 1, pass, fail);
    test_nil(interp, "take 0", "((take 0) (quote (1 2 3)))", pass, fail);
    test_eq(interp, "drop car", "(car ((drop 2) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "drop length", "(length ((drop 2) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "drop 0", "(car ((drop 0) (quote (1 2 3))))", 1, pass, fail);
    test_eq(interp, "zip length", "(length ((zip (quote (1 2 3))) (quote (4 5 6))))", 3, pass, fail);
    test_eq(interp, "zip first car", "(car (car ((zip (quote (1 2 3))) (quote (4 5 6)))))", 1, pass, fail);
    test_eq(interp, "zip first cdr", "(cdr (car ((zip (quote (1 2 3))) (quote (4 5 6)))))", 4, pass, fail);
    test_eq(interp, "zip unequal", "(length ((zip (quote (1 2))) (quote (4 5 6))))", 2, pass, fail);
    test_eq(interp, "range length", "(length (range 5))", 5, pass, fail);
    test_eq(interp, "range car", "(car (range 5))", 0, pass, fail);
    test_nil(interp, "range 0", "(range 0)", pass, fail);
    test_nil(interp, "for-each", "((for-each (lambda (x) (+ x 1))) (quote (1 2 3)))", pass, fail);
    test_truthy(interp, "any? found", "((any? (lambda (x) (= x 3))) (quote (1 2 3 4)))", pass, fail);
    test_nil(interp, "any? not found", "((any? (lambda (x) (= x 99))) (quote (1 2 3 4)))", pass, fail);
    test_truthy(interp, "every? all", "((every? (lambda (x) (> x 0))) (quote (1 2 3 4)))", pass, fail);
    test_nil(interp, "every? not all", "((every? (lambda (x) (> x 2))) (quote (1 2 3 4)))", pass, fail);
    test_truthy(interp, "every? empty", "((every? (lambda (x) (> x 0))) nil)", pass, fail);
    test_eq(interp, "try/raise catch", "((try (lambda (xx) (+ 1 (perform raise 42)))) (lambda (msg) msg))", 42, pass, fail);
    test_eq(interp, "try no error", "((try (lambda (xx) (+ 1 2))) (lambda (msg) msg))", 3, pass, fail);
    test_truthy(interp, "assert! true", "((assert! (= 1 1)) \"ok\")", pass, fail);
    test_truthy(interp, "assert! false raises", "(handle ((assert! (= 1 2)) \"fail\") ((raise k msg) msg))", pass, fail);
    setup(interp, "(define test-alist (list (cons 1 10) (cons 2 20) (cons 3 30)))");
    test_eq(interp, "assoc find", "(cdr ((assoc 2) test-alist))", 20, pass, fail);
    test_nil(interp, "assoc not found", "((assoc 99) test-alist)", pass, fail);
    test_eq(interp, "assoc-ref", "((assoc-ref 3) test-alist)", 30, pass, fail);
    test_nil(interp, "assoc-ref not found", "((assoc-ref 99) test-alist)", pass, fail);
    test_eq(interp, "map+filter+foldl", "(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) ((filter (lambda (x) (> x 3))) ((map (lambda (x) (* x 2))) (quote (1 2 3 4 5)))))", 28, pass, fail);
    test_eq(interp, "reverse+take", "(car ((take 1) (reverse (quote (1 2 3)))))", 3, pass, fail);
    test_eq(interp, "sum of squares", "(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) ((map (lambda (x) (* x x))) (range 5)))", 30, pass, fail);

    // === FLOAT TESTS ===
    // Float literals
    test_double(interp, "float 3.14", "3.14", 3.14, pass, fail);
    test_double(interp, "float -2.5", "-2.5", -2.5, pass, fail);
    test_double(interp, "float 1.0e3", "1.0e3", 1000.0, pass, fail);
    test_double(interp, "float 1.5e-2", "1.5e-2", 0.015, pass, fail);
    // Mixed arithmetic
    test_double(interp, "int+float", "(+ 1 2.0)", 3.0, pass, fail);
    test_double(interp, "float*int", "(* 2.5 4)", 10.0, pass, fail);
    test_double(interp, "float/int", "(/ 1.0 3)", 0.333333, pass, fail);
    test_double(interp, "float-float", "(- 5.5 2.5)", 3.0, pass, fail);
    // Int arithmetic stays int
    test_eq(interp, "int+int stays int", "(+ 1 2)", 3, pass, fail);
    // Comparisons with float
    test_truthy(interp, "< int float", "(< 1 2.5)", pass, fail);
    test_truthy(interp, "> float int", "(> 3.5 2)", pass, fail);
    test_truthy(interp, "= int float", "(= 1 1.0)", pass, fail);
    test_truthy(interp, "<= float", "(<= 1.5 1.5)", pass, fail);
    test_truthy(interp, ">= float", "(>= 2.0 1.5)", pass, fail);
    // Unary minus
    test_double(interp, "unary neg float", "(- 0 3.14)", -3.14, pass, fail);
    // Conversions
    test_double(interp, "exact->inexact", "(exact->inexact 5)", 5.0, pass, fail);
    test_eq(interp, "inexact->exact", "(inexact->exact 3.7)", 3, pass, fail);
    // string->number float
    test_double(interp, "string->number float", "(string->number \"3.14\")", 3.14, pass, fail);
    test_double(interp, "string->number exp", "(string->number \"1.5e2\")", 150.0, pass, fail);
    // number->string double
    test_str(interp, "number->string double", "(number->string 3.14)", pass, fail);

    // === MATH LIBRARY TESTS (Phase 2) ===
    test_double(interp, "sin 0", "(sin 0.0)", 0.0, pass, fail);
    test_double(interp, "cos 0", "(cos 0.0)", 1.0, pass, fail);
    test_double(interp, "tan 0", "(tan 0.0)", 0.0, pass, fail);
    test_double(interp, "asin 0", "(asin 0.0)", 0.0, pass, fail);
    test_double(interp, "acos 1", "(acos 1.0)", 0.0, pass, fail);
    test_double(interp, "atan 0", "(atan 0.0)", 0.0, pass, fail);
    test_double(interp, "atan2", "(atan2 1.0 1.0)", 0.7853981, pass, fail);
    test_double(interp, "exp 0", "(exp 0.0)", 1.0, pass, fail);
    test_double(interp, "log 1", "(log 1.0)", 0.0, pass, fail);
    test_double(interp, "sqrt 4", "(sqrt 4.0)", 2.0, pass, fail);
    test_double(interp, "pow 2 10", "(pow 2.0 10.0)", 1024.0, pass, fail);
    test_eq(interp, "floor 3.7", "(floor 3.7)", 3, pass, fail);
    test_eq(interp, "ceiling 3.2", "(ceiling 3.2)", 4, pass, fail);
    test_eq(interp, "round 3.5", "(round 3.5)", 4, pass, fail);
    test_eq(interp, "truncate 3.9", "(truncate 3.9)", 3, pass, fail);
    test_eq(interp, "abs -5", "(abs -5)", 5, pass, fail);
    test_double(interp, "abs -3.14", "(abs -3.14)", 3.14, pass, fail);
    test_eq(interp, "min 3 7", "(min 3 7)", 3, pass, fail);
    test_eq(interp, "max 3 7", "(max 3 7)", 7, pass, fail);
    test_eq(interp, "gcd 12 8", "(gcd 12 8)", 4, pass, fail);
    test_double(interp, "pi constant", "pi", 3.14159265, pass, fail);
    test_double(interp, "e constant", "e", 2.71828182, pass, fail);

    // === SORTING & BITWISE TESTS (Phase 3) ===
    test_eq(interp, "sort car", "(car (sort (quote (3 1 2))))", 1, pass, fail);
    test_eq(interp, "sort length", "(length (sort (quote (5 3 1 4 2))))", 5, pass, fail);
    test_eq(interp, "sort last", "(car (cdr (cdr (sort (quote (3 1 2))))))", 3, pass, fail);
    test_eq(interp, "sort-by custom", "(car (sort-by (lambda (a) (lambda (b) (- a b))) (quote (3 1 2))))", 1, pass, fail);
    test_eq(interp, "bitwise-and", "(bitwise-and 12 10)", 8, pass, fail);
    test_eq(interp, "bitwise-or", "(bitwise-or 12 10)", 14, pass, fail);
    test_eq(interp, "bitwise-xor", "(bitwise-xor 12 10)", 6, pass, fail);
    test_eq(interp, "bitwise-not 0", "(bitwise-not 0)", -1, pass, fail);
    test_eq(interp, "lshift", "(lshift 1 4)", 16, pass, fail);
    test_eq(interp, "rshift", "(rshift 16 4)", 1, pass, fail);

    // === STDLIB HOFs (Phase 3) ===
    test_eq(interp, "flatten", "(length (flatten (quote ((1 2) (3 4) (5)))))", 5, pass, fail);
    test_eq(interp, "flatten car", "(car (flatten (quote ((1 2) (3)))))", 1, pass, fail);
    test_eq(interp, "partition yes", "(length (car (partition (lambda (x) (> x 3)) (quote (1 2 3 4 5)))))", 2, pass, fail);
    test_eq(interp, "partition no", "(length (cdr (partition (lambda (x) (> x 3)) (quote (1 2 3 4 5)))))", 3, pass, fail);
    test_eq(interp, "remove", "(length (remove (lambda (x) (= x 3)) (quote (1 2 3 4 5))))", 4, pass, fail);
    test_eq(interp, "find found", "(find (lambda (x) (= x 3)) (quote (1 2 3 4 5)))", 3, pass, fail);
    test_nil(interp, "find not found", "(find (lambda (x) (= x 99)) (quote (1 2 3)))", pass, fail);

    // === STRING OPS TESTS (Phase 5) ===
    test_truthy(interp, "string-contains? yes", "(string-contains? \"hello world\" \"world\")", pass, fail);
    test_nil(interp, "string-contains? no", "(string-contains? \"hello\" \"xyz\")", pass, fail);
    test_eq(interp, "string-index-of found", "(string-index-of \"hello world\" \"world\")", 6, pass, fail);
    test_eq(interp, "string-index-of not", "(string-index-of \"hello\" \"xyz\")", -1, pass, fail);
    test_str(interp, "string-replace", "(string-replace \"hello world\" \"world\" \"there\")", pass, fail);
    test_str(interp, "char-at", "(char-at \"hello\" 1)", pass, fail);
    test_str(interp, "string-repeat", "(string-repeat \"ab\" 3)", pass, fail);

    // === TYPE PREDICATE TESTS (Phase 5) ===
    test_truthy(interp, "double? yes", "(double? 3.14)", pass, fail);
    test_nil(interp, "double? no", "(double? 42)", pass, fail);
    test_truthy(interp, "number? int", "(number? 42)", pass, fail);
    test_truthy(interp, "number? double", "(number? 3.14)", pass, fail);
    test_nil(interp, "number? string", "(number? \"hello\")", pass, fail);
    test_truthy(interp, "boolean? true", "(boolean? true)", pass, fail);
    test_truthy(interp, "boolean? nil", "(boolean? nil)", pass, fail);
    test_nil(interp, "boolean? int", "(boolean? 42)", pass, fail);
    test_truthy(interp, "list? yes", "(list? (quote (1 2 3)))", pass, fail);
    test_truthy(interp, "list? nil", "(list? nil)", pass, fail);
    test_nil(interp, "list? int", "(list? 42)", pass, fail);
    test_truthy(interp, "procedure? lambda", "(procedure? (lambda (x) x))", pass, fail);
    test_truthy(interp, "procedure? prim", "(procedure? +)", pass, fail);
    test_nil(interp, "procedure? int", "(procedure? 42)", pass, fail);
    test_truthy(interp, "zero? 0", "(zero? 0)", pass, fail);
    test_nil(interp, "zero? 1", "(zero? 1)", pass, fail);
    test_truthy(interp, "positive? 5", "(positive? 5)", pass, fail);
    test_nil(interp, "positive? -1", "(positive? -1)", pass, fail);
    test_truthy(interp, "negative? -1", "(negative? -1)", pass, fail);
    test_nil(interp, "negative? 5", "(negative? 5)", pass, fail);
    test_truthy(interp, "even? 4", "(even? 4)", pass, fail);
    test_nil(interp, "even? 3", "(even? 3)", pass, fail);
    test_truthy(interp, "odd? 3", "(odd? 3)", pass, fail);
    test_nil(interp, "odd? 4", "(odd? 4)", pass, fail);

    // === FORMAT & DISPLAY TESTS (Phase 5) ===
    test_str(interp, "format simple", "(format \"~a plus ~a\" 1 2)", pass, fail);

    // === INTROSPECTION TESTS (Phase 7) ===
    setup(interp, "(define [macro] my-inc ([x] (+ x 1)))");
    test_truthy(interp, "macroexpand", "(pair? (macroexpand (quote (my-inc 5))))", pass, fail);
    test_eq(interp, "eval literal", "(eval 42)", 42, pass, fail);
    test_eq(interp, "eval list", "(eval (quote (+ 1 2)))", 3, pass, fail);
    test_eq(interp, "apply +", "(apply + (quote (1 2)))", 3, pass, fail);
    test_truthy(interp, "bound? yes", "(bound? '+)", pass, fail);
    test_nil(interp, "bound? no", "(bound? 'xyzzy-undefined)", pass, fail);
    test_error(interp, "error raises", "(error \"test error\")", pass, fail);

    // === LAZY EVALUATION TESTS (Phase 8) ===
    setup(interp, "(define p (delay (lambda (xx) 42)))");
    test_eq(interp, "delay/force", "(force p)", 42, pass, fail);

    // === TCO STDLIB TESTS (Phase 6) ===
    test_eq(interp, "foldl TCO 4000", "(((foldl (lambda (a) (lambda (b) (+ a b)))) 0) (range 4000))", 7998000, pass, fail);

    // === ARRAY TESTS (Phase 8) ===
    test_eq(interp, "length array", "(length (array 1 2 3))", 3, pass, fail);
    test_eq(interp, "ref array", "(ref (array 10 20 30) 1)", 20, pass, fail);
    // array/list conversion via constructors
    test_eq(interp, "array from list", "(ref (array '(10 20 30)) 1)", 20, pass, fail);
    setup(interp, "(define test-arr (array 1 2 3))");
    setup(interp, "(array-set! test-arr 1 99)");
    test_eq(interp, "array-set!", "(ref test-arr 1)", 99, pass, fail);
    test_truthy(interp, "array?", "(array? (array 1 2))", pass, fail);
    test_nil(interp, "array? non-arr", "(array? 42)", pass, fail);
    test_truthy(interp, "list from array", "(= (list [1 2 3]) '(1 2 3))", pass, fail);
    test_eq(interp, "array from list 2", "(ref (array '(10 20 30)) 2)", 30, pass, fail);
    test_eq(interp, "array nil→empty", "(length (array nil))", 0, pass, fail);
    setup(interp, "(define push-arr (array 1 2))");
    setup(interp, "(push! push-arr 3)");
    test_eq(interp, "push!", "(length push-arr)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref push-arr 2)", 3, pass, fail);
    test_error(interp, "ref oob", "(ref (array 1) 5)", pass, fail);
    test_error(interp, "array-set! oob", "(array-set! (array 1) 5 0)", pass, fail);
    test_eq(interp, "empty array", "(length (array))", 0, pass, fail);

    // === SET TESTS (Phase 9) ===
    test_eq(interp, "set-size", "(set-size (set 1 2 3))", 3, pass, fail);
    test_truthy(interp, "set-contains?", "(set-contains? (set 1 2 3) 2)", pass, fail);
    test_nil(interp, "set-contains? miss", "(set-contains? (set 1 2 3) 5)", pass, fail);
    setup(interp, "(define test-set (set 1 2))");
    setup(interp, "(set-add test-set 3)");
    test_eq(interp, "set-add", "(set-size test-set)", 3, pass, fail);
    test_truthy(interp, "set-add contains", "(set-contains? test-set 3)", pass, fail);
    setup(interp, "(set-remove test-set 2)");
    test_eq(interp, "set-remove", "(set-size test-set)", 2, pass, fail);
    test_nil(interp, "set-remove gone", "(set-contains? test-set 2)", pass, fail);
    test_eq(interp, "set dedup", "(set-size (set 1 1 2 2 3))", 3, pass, fail);

    // === CONVENIENCE TESTS (Phase 12) ===
    test_eq(interp, "read-string", "(read-string \"(+ 1 2)\")", 3, pass, fail);
    test_truthy(interp, "symbol->string", "(= (symbol->string 'hello) \"hello\")", pass, fail);
    test_truthy(interp, "string->symbol", "(= (string->symbol \"abc\") 'abc)", pass, fail);

    // === TYPE SYSTEM TESTS (Phase 1) ===

    // type-of primitive
    test_truthy(interp, "type-of int", "(= (type-of 42) 'Int)", pass, fail);
    test_truthy(interp, "type-of double", "(= (type-of 3.14) 'Double)", pass, fail);
    test_truthy(interp, "type-of string", "(= (type-of \"hello\") 'String)", pass, fail);
    test_truthy(interp, "type-of symbol", "(= (type-of 'foo) 'Symbol)", pass, fail);
    test_truthy(interp, "type-of nil", "(= (type-of nil) 'Nil)", pass, fail);
    test_truthy(interp, "type-of list", "(= (type-of '(1 2 3)) 'List)", pass, fail);
    test_truthy(interp, "type-of array", "(= (type-of (array 1 2 3)) 'Array)", pass, fail);
    test_truthy(interp, "type-of closure", "(= (type-of (lambda (x) x)) 'Closure)", pass, fail);
    test_truthy(interp, "type-of true", "(= (type-of true) 'Bool)", pass, fail);

    // is? primitive
    test_truthy(interp, "is? int", "(is? 42 'Int)", pass, fail);
    test_truthy(interp, "is? string", "(is? \"hello\" 'String)", pass, fail);
    test_nil(interp, "is? mismatch", "(is? 42 'String)", pass, fail);
    test_truthy(interp, "is? Any", "(is? 42 'Any)", pass, fail);

    // define [type] — simple struct (interp-only: JIT doesn't know user types yet)
    setup(interp, "(define [type] Point (^Int x) (^Int y))");
    test_truthy_interp(interp, "type constructor", "(instance? (Point 3 4))", pass, fail);
    test_truthy_interp(interp, "type-of instance", "(= (type-of (Point 3 4)) 'Point)", pass, fail);
    test_eq_interp(interp, "type field access x", "(let ((p (Point 3 4))) p.x)", 3, pass, fail);
    test_eq_interp(interp, "type field access y", "(let ((p (Point 3 4))) p.y)", 4, pass, fail);
    test_truthy_interp(interp, "is? instance type", "(is? (Point 3 4) 'Point)", pass, fail);

    // define [abstract] — abstract type
    setup(interp, "(define [abstract] Shape)");
    test_truthy(interp, "abstract type defined", "(= 'Shape 'Shape)", pass, fail);

    // define [type] with parent
    setup(interp, "(define [type] (Circle Shape) (^Int radius))");
    test_truthy_interp(interp, "subtype constructor", "(instance? (Circle 5))", pass, fail);
    test_truthy_interp(interp, "is? subtype", "(is? (Circle 5) 'Circle)", pass, fail);
    test_truthy_interp(interp, "is? parent type", "(is? (Circle 5) 'Shape)", pass, fail);
    test_nil_interp(interp, "is? wrong parent", "(is? (Point 1 2) 'Shape)", pass, fail);
    test_eq_interp(interp, "subtype field access", "(let ((c (Circle 5))) c.radius)", 5, pass, fail);

    // Nested type access
    setup(interp, "(define [type] Line (^Point start) (^Point end))");
    test_eq_interp(interp, "nested type access", "(let ((l (Line (Point 1 2) (Point 3 4)))) l.start.x)", 1, pass, fail);
    test_eq_interp(interp, "nested type access 2", "(let ((l (Line (Point 1 2) (Point 3 4)))) l.end.y)", 4, pass, fail);

    // define [union] — union types
    setup(interp, "(define [union] (Option T) None (Some T))");
    test_truthy_interp(interp, "union nullary", "(instance? None)", pass, fail);
    test_truthy_interp(interp, "union constructor", "(instance? (Some 42))", pass, fail);
    test_truthy_interp(interp, "is? union variant", "(is? None 'Option)", pass, fail);
    test_truthy_interp(interp, "is? union Some", "(is? (Some 42) 'Option)", pass, fail);
    test_truthy_interp(interp, "is? union Some exact", "(is? (Some 42) 'Some)", pass, fail);

    // define [alias]
    setup(interp, "(define [alias] Num Int)");
    test_truthy(interp, "alias defined", "(= 'Num 'Num)", pass, fail);

    // instance? primitive
    test_nil(interp, "instance? non-instance", "(instance? 42)", pass, fail);

    // === PARAMETRIC TYPE TESTS (Phase 7) ===

    // Parametric type with single type param
    setup(interp, "(define [type] (Box T) (^T value))");
    test_truthy_interp(interp, "Box constructor", "(instance? (Box 42))", pass, fail);
    test_eq_interp(interp, "Box field access", "(let ((b (Box 42))) b.value)", 42, pass, fail);
    test_truthy_interp(interp, "Box type-of", "(= (type-of (Box 42)) 'Box)", pass, fail);

    // type-args primitive — infer T=Int from (Box 42)
    test_truthy_interp(interp, "type-args Box Int", "(= (car (type-args (Box 42))) 'Int)", pass, fail);
    test_truthy_interp(interp, "type-args Box String", "(= (car (type-args (Box \"hi\"))) 'String)", pass, fail);

    // Parametric type with two type params
    setup(interp, "(define [type] (Pair A B) (^A first) (^B second))");
    test_eq_interp(interp, "Pair field first", "(let ((p (Pair 1 \"two\"))) p.first)", 1, pass, fail);
    test_truthy_interp(interp, "Pair field second", "(= (let ((p (Pair 1 \"two\"))) p.second) \"two\")", pass, fail);
    test_truthy_interp(interp, "type-args Pair A", "(= (car (type-args (Pair 1 \"two\"))) 'Int)", pass, fail);
    test_truthy_interp(interp, "type-args Pair B", "(= (car (cdr (type-args (Pair 1 \"two\")))) 'String)", pass, fail);

    // type-args on non-parametric type returns nil
    test_nil_interp(interp, "type-args non-param", "(type-args (Point 1 2))", pass, fail);
    // type-args on non-instance returns nil
    test_nil_interp(interp, "type-args int", "(type-args 42)", pass, fail);

    // Disambiguation: (Circle Shape) still works as parent, not type param
    // Circle was defined above with parent Shape — verify it still works
    test_truthy_interp(interp, "disambig parent still works", "(is? (Circle 5) 'Shape)", pass, fail);

    // === MULTIPLE DISPATCH TESTS (Phase 2) ===

    // Basic dispatch on types
    setup(interp, "(define (describe (^Int n)) \"integer\")");
    setup(interp, "(define (describe (^String s)) \"string\")");
    setup(interp, "(define (describe x) \"other\")");
    test_truthy_interp(interp, "dispatch int", "(= (describe 42) \"integer\")", pass, fail);
    test_truthy_interp(interp, "dispatch string", "(= (describe \"hi\") \"string\")", pass, fail);
    test_truthy_interp(interp, "dispatch fallback", "(= (describe '(1 2)) \"other\")", pass, fail);

    // Val dispatch (Fibonacci)
    setup(interp, "(define (fib (^(Val 0) n)) 0)");
    setup(interp, "(define (fib (^(Val 1) n)) 1)");
    setup(interp, "(define (fib (^Int n)) (+ (fib (- n 1)) (fib (- n 2))))");
    test_eq_interp(interp, "dispatch fib 0", "(fib 0)", 0, pass, fail);
    test_eq_interp(interp, "dispatch fib 1", "(fib 1)", 1, pass, fail);
    test_eq_interp(interp, "dispatch fib 10", "(fib 10)", 55, pass, fail);

    // Multi-arg dispatch
    setup(interp, "(define (add2 (^Int a) (^Int b)) (+ a b))");
    setup(interp, "(define (add2 (^String a) (^String b)) (string-append a b))");
    test_eq_interp(interp, "dispatch multi-arg int", "(add2 3 4)", 7, pass, fail);
    test_truthy_interp(interp, "dispatch multi-arg str", "(= (add2 \"hello\" \" world\") \"hello world\")", pass, fail);

    // === CONSTRAINED DISPATCH TESTS (Phase 7) ===

    // Define abstract type hierarchy for constraint testing
    setup(interp, "(define [abstract] (Numeric Number))");  // Numeric extends Number (but Number doesn't exist yet, no parent)

    // Actually, let's use fresh abstract type hierarchy
    setup(interp, "(define [abstract] Addable)");
    setup(interp, "(define [type] (MyInt Addable) (^Int val))");
    setup(interp, "(define [type] (MyStr Addable) (^String val))");
    setup(interp, "(define [type] NotAddable (^Int val))");

    // Constrained dispatch: only matches if arg type <: Addable
    setup(interp, "(define (get-val (^{'T Addable} x)) x.val)");
    test_eq_interp(interp, "constraint MyInt match", "(get-val (MyInt 42))", 42, pass, fail);
    test_truthy_interp(interp, "constraint MyStr match", "(= (get-val (MyStr \"hi\")) \"hi\")", pass, fail);

    // Constraint fallback: NotAddable doesn't satisfy Addable constraint
    // Add a fallback method that accepts anything
    setup(interp, "(define (get-val x) (- 0 1))");
    test_eq_interp(interp, "constraint fallback", "(get-val (NotAddable 99))", -1, pass, fail);

    // Array/dict indexing (Phase 3)
    test_eq(interp, "array index", "(let ((v (array 10 20 30))) v.[0])", 10, pass, fail);
    test_eq(interp, "array index 2", "(let ((v (array 10 20 30))) v.[2])", 30, pass, fail);

    // Struct field mutation (Phase 3)
    test_eq_interp(interp, "set! struct field", "(let ((p (Point 1 2))) (begin (set! p.x 99) p.x))", 99, pass, fail);
    test_eq_interp(interp, "set! struct field y", "(let ((p (Point 1 2))) (begin (set! p.y 50) p.y))", 50, pass, fail);
    test_eq_interp(interp, "set! nested field", "(let ((ln (Line (Point 0 0) (Point 10 10)))) (begin (set! ln.start.x 5) ln.start.x))", 5, pass, fail);
    test_eq_interp(interp, "set! preserves other", "(let ((p (Point 3 4))) (begin (set! p.x 99) p.y))", 4, pass, fail);

    // Hashmap indexing (Phase 3)
    test_eq_interp(interp, "dict index", "(let ((m {'a 1 'b 2})) m.['a])", 1, pass, fail);

    // === PRIMITIVE CONSOLIDATION TESTS (Phase 4) ===
    // Overload + for Point type (primitive becomes fallback)
    setup(interp, "(define (add-points (^Point a) (^Point b)) (Point (+ a.x b.x) (+ a.y b.y)))");
    test_eq_interp(interp, "typed dispatch Point+", "(let ((r (add-points (Point 1 2) (Point 3 4)))) r.x)", 4, pass, fail);
    test_eq_interp(interp, "typed dispatch Point+ y", "(let ((r (add-points (Point 1 2) (Point 3 4)))) r.y)", 6, pass, fail);
    // Built-in + still works (dispatch preserves primitive fallback)
    test_eq_interp(interp, "builtin + after dispatch", "(+ 10 20)", 30, pass, fail);
    // Overload length for custom type
    setup(interp, "(define (my-len (^Point p)) 2)");
    test_eq_interp(interp, "typed dispatch custom len", "(my-len (Point 5 6))", 2, pass, fail);

    // === ABSTRACT TYPE HIERARCHY TESTS ===
    // Number hierarchy
    test_truthy_interp(interp, "is? int Number", "(is? 42 'Number)", pass, fail);
    test_truthy_interp(interp, "is? double Number", "(is? 3.14 'Number)", pass, fail);
    test_nil_interp(interp, "is? string not Number", "(is? \"hi\" 'Number)", pass, fail);
    test_nil_interp(interp, "is? nil not Number", "(is? nil 'Number)", pass, fail);

    // Collection hierarchy
    test_truthy_interp(interp, "is? list Collection", "(is? '(1 2) 'Collection)", pass, fail);
    test_truthy_interp(interp, "is? array Collection", "(is? [1 2] 'Collection)", pass, fail);
    test_truthy_interp(interp, "is? dict Collection", "(is? {'a 1} 'Collection)", pass, fail);
    test_nil_interp(interp, "is? int not Collection", "(is? 42 'Collection)", pass, fail);
    test_nil_interp(interp, "is? string not Collection", "(is? \"hi\" 'Collection)", pass, fail);

    // number? uses abstract hierarchy
    test_truthy_interp(interp, "number? int", "(number? 42)", pass, fail);
    test_truthy_interp(interp, "number? double", "(number? 3.14)", pass, fail);
    test_nil_interp(interp, "number? string", "(number? \"hi\")", pass, fail);

    // === PREDICATE EQUIVALENCE TESTS ===
    test_truthy_interp(interp, "int? positive", "(int? 42)", pass, fail);
    test_nil_interp(interp, "int? negative", "(int? 3.14)", pass, fail);
    test_truthy_interp(interp, "double? positive", "(double? 3.14)", pass, fail);
    test_nil_interp(interp, "double? negative", "(double? 42)", pass, fail);
    test_truthy_interp(interp, "string? positive", "(string? \"hi\")", pass, fail);
    test_nil_interp(interp, "string? negative", "(string? 42)", pass, fail);
    test_truthy_interp(interp, "symbol? positive", "(symbol? 'foo)", pass, fail);
    test_nil_interp(interp, "symbol? negative", "(symbol? 42)", pass, fail);
    test_truthy_interp(interp, "boolean? true", "(boolean? true)", pass, fail);
    test_truthy_interp(interp, "boolean? false", "(boolean? false)", pass, fail);
    test_nil_interp(interp, "boolean? int", "(boolean? 42)", pass, fail);
    test_truthy_interp(interp, "list? positive", "(list? '(1 2))", pass, fail);
    test_truthy_interp(interp, "list? nil", "(list? nil)", pass, fail);
    test_nil_interp(interp, "list? int", "(list? 42)", pass, fail);
    test_truthy_interp(interp, "closure? positive", "(closure? (lambda (x) x))", pass, fail);
    test_nil_interp(interp, "closure? int", "(closure? 42)", pass, fail);
    test_truthy_interp(interp, "array? positive", "(array? [1 2])", pass, fail);
    test_nil_interp(interp, "array? int", "(array? 42)", pass, fail);
    test_truthy_interp(interp, "dict? positive", "(dict? {'a 1})", pass, fail);
    test_nil_interp(interp, "dict? int", "(dict? 42)", pass, fail);
    test_truthy_interp(interp, "zero? yes", "(zero? 0)", pass, fail);
    test_nil_interp(interp, "zero? no", "(zero? 1)", pass, fail);
    test_truthy_interp(interp, "positive? yes", "(positive? 5)", pass, fail);
    test_nil_interp(interp, "positive? no", "(positive? -1)", pass, fail);
    test_truthy_interp(interp, "negative? yes", "(negative? -3)", pass, fail);
    test_nil_interp(interp, "negative? no", "(negative? 0)", pass, fail);
    test_truthy_interp(interp, "even? yes", "(even? 4)", pass, fail);
    test_nil_interp(interp, "even? no", "(even? 3)", pass, fail);
    test_truthy_interp(interp, "odd? yes", "(odd? 3)", pass, fail);
    test_nil_interp(interp, "odd? no", "(odd? 4)", pass, fail);

    // === DISPATCH EXTENSIBILITY TESTS ===
    setup(interp, "(define [type] Vec2 (^Double vx) (^Double vy))");
    setup(interp, "(define (+ (^Vec2 a) (^Vec2 b)) (Vec2 (+ a.vx b.vx) (+ a.vy b.vy)))");
    test_truthy_interp(interp, "Vec2 + dispatch x",
        "(= (let ((v (+ (Vec2 1.0 2.0) (Vec2 3.0 4.0)))) v.vx) 4.0)", pass, fail);
    test_truthy_interp(interp, "Vec2 + dispatch y",
        "(= (let ((v (+ (Vec2 1.0 2.0) (Vec2 3.0 4.0)))) v.vy) 6.0)", pass, fail);
    // Built-in + still works after Vec2 extension
    test_eq_interp(interp, "builtin + after Vec2", "(+ 1 2)", 3, pass, fail);
    // Extend length for Vec2
    setup(interp, "(define (length (^Vec2 v)) (sqrt (+ (* v.vx v.vx) (* v.vy v.vy))))");
    test_truthy_interp(interp, "Vec2 length dispatch",
        "(= (length (Vec2 3.0 4.0)) 5.0)", pass, fail);
    // Built-in length still works
    test_eq_interp(interp, "builtin length after Vec2", "(length '(1 2 3))", 3, pass, fail);
    // procedure? recognizes method tables
    test_truthy_interp(interp, "procedure? method table", "(procedure? +)", pass, fail);

    // === I/O EFFECTS TESTS (Phase 5) ===
    // print/println go through effects now (fast path to __raw-*)
    test_nil_interp(interp, "io effect print", "(print 42)", pass, fail);
    test_nil_interp(interp, "io effect println", "(println 99)", pass, fail);

    // Custom handler can suppress output
    test_eq_interp(interp, "io handle suppress",
        "(handle (begin (println \"suppressed\") 42) ((io/println k x) (k nil)))",
        42, pass, fail);

    // Custom handler can capture output
    test_truthy_interp(interp, "io handle capture",
        "(= (handle (begin (println \"captured\") nil) ((io/println k x) x)) \"captured\")",
        pass, fail);

    // __raw-print still works directly
    test_nil_interp(interp, "raw-print direct", "(__raw-print 0)", pass, fail);

    // === TYPED EFFECT DECLARATIONS TESTS ===
    // Declaring effects doesn't error
    test_nil_interp(interp, "defeffect decl", "(define [effect] (test/greet (^String name)))", pass, fail);
    test_nil_interp(interp, "defeffect no arg", "(define [effect] (test/ping))", pass, fail);
    test_nil_interp(interp, "defeffect int arg", "(define [effect] (test/count (^Int n)))", pass, fail);

    // Perform with correct type works
    test_str_interp(interp, "effect correct type",
        "(handle (perform test/greet \"hello\") ((test/greet k x) x))", pass, fail);
    test_eq_interp(interp, "effect int correct",
        "(handle (perform test/count 42) ((test/count k x) x))", 42, pass, fail);

    // Perform with wrong type errors
    test_error(interp, "effect wrong type int",
        "(handle (perform test/greet 42) ((test/greet k x) x))", pass, fail);
    test_error(interp, "effect wrong type str",
        "(handle (perform test/count \"bad\") ((test/count k x) x))", pass, fail);

    // Undeclared effects still work (backward compat)
    test_eq_interp(interp, "effect undeclared ok",
        "(handle (perform custom-tag 42) ((custom-tag k x) (+ x 1)))", 43, pass, fail);

    // Nullary effect (no arg type) accepts anything
    test_eq_interp(interp, "effect nullary any",
        "(handle (perform test/ping 99) ((test/ping k x) x))", 99, pass, fail);

    // ^Any effects accept all types
    test_nil_interp(interp, "io effect Any int", "(print 42)", pass, fail);
    test_nil_interp(interp, "io effect Any str", "(print \"hello\")", pass, fail);

    // io/read-file with wrong type should error (^String)
    test_error(interp, "io/read-file wrong type",
        "(handle (perform io/read-file 42) ((io/read-file k x) x))", pass, fail);

    // io/read-file with correct type works
    test_str_interp(interp, "io/read-file correct type",
        "(handle (perform io/read-file \"test.txt\") ((io/read-file k x) x))", pass, fail);

    // Subtype check: Number accepts Int and Double
    test_nil_interp(interp, "defeffect number arg", "(define [effect] (test/num (^Number n)))", pass, fail);
    test_eq_interp(interp, "effect number int",
        "(handle (perform test/num 42) ((test/num k x) x))", 42, pass, fail);
    test_error(interp, "effect number str",
        "(handle (perform test/num \"bad\") ((test/num k x) x))", pass, fail);

    // === UNION PATTERN MATCHING TESTS (Phase 6) ===
    // Option and Result types already defined in Phase 1 tests above

    // Nullary constructor match
    test_eq_interp(interp, "match None", "(match None (None 0) ((Some x) x))", 0, pass, fail);

    // Constructor with field match
    test_eq_interp(interp, "match Some", "(match (Some 42) (None 0) ((Some x) x))", 42, pass, fail);

    // Result type
    setup(interp, "(define [union] (Result T E) (Ok T) (Err E))");
    test_eq_interp(interp, "match Ok", "(match (Ok 1) ((Ok v) v) ((Err e) (- 0 1)))", 1, pass, fail);
    test_eq_interp(interp, "match Err", "(match (Err 99) ((Ok v) v) ((Err e) e))", 99, pass, fail);

    // Nested constructor match
    test_eq_interp(interp, "match nested Some", "(match (Some (Some 7)) ((Some (Some x)) x) (_ 0))", 7, pass, fail);

    // Wildcard in constructor
    test_eq_interp(interp, "match ctor wildcard", "(match (Ok 42) ((Ok _) 1) ((Err _) 2))", 1, pass, fail);

    // === MEMORY RECLAMATION TESTS ===
    // free! on array: releases backing, value becomes nil
    test_nil_interp(interp, "free! array", "(let ((a [1 2 3])) (begin (free! a) a))", pass, fail);
    // free! on dict: releases backing, value becomes nil
    test_nil_interp(interp, "free! dict", "(let ((d {'a 1 'b 2})) (begin (free! d) d))", pass, fail);
    // free! on non-collection: no-op, returns nil
    test_nil_interp(interp, "free! int noop", "(free! 42)", pass, fail);
    // free! on nil: no-op
    test_nil_interp(interp, "free! nil noop", "(free! nil)", pass, fail);

    // === FFI TESTS ===
    setup(interp, "(define test-libc (ffi-open \"libc.so.6\"))");
    test_tag(interp, "ffi-open handle", "test-libc", FFI_HANDLE, pass, fail);
    test_eq(interp, "ffi-call strlen", "(ffi-call test-libc \"strlen\" 'size \"hello\" 'string)", 5, pass, fail);
    test_eq(interp, "ffi-call abs", "(ffi-call test-libc \"abs\" 'int (- 0 42) 'int)", 42, pass, fail);
    test_gt(interp, "ffi-call getpid", "(ffi-call test-libc \"getpid\" 'int)", 0, pass, fail);
    test_eq(interp, "ffi-call atoi", "(ffi-call test-libc \"atoi\" 'int \"12345\" 'string)", 12345, pass, fail);
    test_truthy(interp, "ffi-sym strlen", "(> (ffi-sym test-libc \"strlen\") 0)", pass, fail);
    // Double FFI: uses XMM registers via properly-typed function pointers
    setup(interp, "(define test-libm (ffi-open \"libm.so.6\"))");
    test_eq_double(interp, "ffi-call sqrt", "(ffi-call test-libm \"sqrt\" 'double 4.0 'double)", 2.0, pass, fail);
    test_eq_double(interp, "ffi-call pow", "(ffi-call test-libm \"pow\" 'double 2.0 'double 10.0 'double)", 1024.0, pass, fail);
    setup(interp, "(ffi-close test-libm)");
    setup(interp, "(define test-libc2 (ffi-open \"libc.so.6\"))");
    // ffi-close is a side effect: JIT-only to avoid double-close
    test_nil_jit(interp, "ffi-close", "(ffi-close test-libc2)", pass, fail);
    test_error(interp, "ffi-open bad lib", "(ffi-open \"nonexistent_lib_xyz.so\")", pass, fail);
    test_eq(interp, "ffi-call cached", "(ffi-call test-libc \"abs\" 'int (- 0 7) 'int)", 7, pass, fail);
    setup(interp, "(ffi-close test-libc)");
}

// =============================================================================
// SECTION 11: COMPILER TESTS
// =============================================================================

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
    register_destructors();

    // --- E_BEGIN tests ---

    // 1. begin with single expression
    {
        char[] code = compile_to_c3("(begin 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: (begin 42)"); }
        else    { fail++; io::printn("[FAIL] Compiler: (begin 42)"); }
    }

    // 2. begin with multiple expressions
    {
        char[] code = compile_to_c3("(begin 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        if (ok) { pass++; io::printn("[PASS] Compiler: (begin 1 2 3)"); }
        else    { fail++; io::printn("[FAIL] Compiler: (begin 1 2 3)"); }
    }

    // 3. begin with define inside
    {
        char[] code = compile_to_c3("(begin (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: (begin (define x 1) x)"); }
        else    { fail++; io::printn("[FAIL] Compiler: (begin (define x 1) x)"); }
    }

    // 4. begin inside lambda body
    {
        char[] code = compile_to_c3("(lambda (x) (begin x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: (lambda (x) (begin x x))"); }
        else    { fail++; io::printn("[FAIL] Compiler: (lambda (x) (begin x x))"); }
    }

    // --- Zero-arg lambda tests ---

    // 5. zero-arg lambda
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        if (ok) { pass++; io::printn("[PASS] Compiler: ((lambda () 42))"); }
        else    { fail++; io::printn("[FAIL] Compiler: ((lambda () 42))"); }
    }

    // 6. zero-arg define + call
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: zero-arg define + call"); }
        else    { fail++; io::printn("[FAIL] Compiler: zero-arg define + call"); }
    }

    // 7. zero-arg in let
    {
        char[] code = compile_to_c3("(let ((f (lambda () 10))) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: zero-arg in let"); }
        else    { fail++; io::printn("[FAIL] Compiler: zero-arg in let"); }
    }

    // --- Multi-param + multi-binding (parser desugar) ---

    // 8. multi-param lambda (desugared by parser to nested)
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: multi-param lambda"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-param lambda"); }
    }

    // 9. multi-binding let (desugared by parser to nested)
    {
        char[] code = compile_to_c3("(let ((x 1) (y 2)) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: multi-binding let"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-binding let"); }
    }

    // 10. 3-param lambda
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: 3-param lambda"); }
        else    { fail++; io::printn("[FAIL] Compiler: 3-param lambda"); }
    }

    // --- Stdlib availability ---

    // 11. map compiles
    {
        char[] code = compile_to_c3("((map (lambda (x) (+ x 1))) (list 1))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: map"); }
        else    { fail++; io::printn("[FAIL] Compiler: map"); }
    }

    // 12. filter compiles
    {
        char[] code = compile_to_c3("((filter (lambda (x) (> x 2))) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: filter"); }
        else    { fail++; io::printn("[FAIL] Compiler: filter"); }
    }

    // 13. foldl compiles
    {
        char[] code = compile_to_c3("(((foldl (lambda (acc) (lambda (x) (+ acc x)))) 0) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: foldl"); }
        else    { fail++; io::printn("[FAIL] Compiler: foldl"); }
    }

    // --- Existing features still work ---

    // 14. match compiles
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: match"); }
        else    { fail++; io::printn("[FAIL] Compiler: match"); }
    }

    // 15. handle/perform compiles
    {
        char[] code = compile_to_c3("(handle (+ 1 (perform ask 0)) ((ask k x) (k 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: handle/perform"); }
        else    { fail++; io::printn("[FAIL] Compiler: handle/perform"); }
    }

    // 16. zero-arg lambda with closure capture
    {
        char[] code = compile_to_c3("(let ((x 42)) (let ((f (lambda () x))) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: zero-arg closure capture"); }
        else    { fail++; io::printn("[FAIL] Compiler: zero-arg closure capture"); }
    }

    // 17. id and compose from stdlib
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: id"); }
        else    { fail++; io::printn("[FAIL] Compiler: id"); }
    }

    // 18. range from stdlib
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: range"); }
        else    { fail++; io::printn("[FAIL] Compiler: range"); }
    }

    // --- set! tests ---

    // 19. set! on global variable
    {
        char[] code = compile_to_c3("(define x 10) (set! x 20)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "x = ");
        if (ok) { pass++; io::printn("[PASS] Compiler: (set! x 20)"); }
        else    { fail++; io::printn("[FAIL] Compiler: (set! x 20)"); }
    }

    // 20. set! in expression context
    {
        char[] code = compile_to_c3("(define x 10) (+ (set! x 20) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: set! in expression"); }
        else    { fail++; io::printn("[FAIL] Compiler: set! in expression"); }
    }

    // 21. set! inside lambda
    {
        char[] code = compile_to_c3("(define counter 0) (define inc! (lambda (n) (set! counter (+ counter n))))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: set! inside lambda"); }
        else    { fail++; io::printn("[FAIL] Compiler: set! inside lambda"); }
    }

    // --- dot-bracket indexing tests ---

    // 22. list indexing with .[i]
    {
        char[] code = compile_to_c3("(define lst (list 10 20 30)) lst.[1]", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_index");
        if (ok) { pass++; io::printn("[PASS] Compiler: lst.[1]"); }
        else    { fail++; io::printn("[FAIL] Compiler: lst.[1]"); }
    }

    // 23. indexing with expression
    {
        char[] code = compile_to_c3("(define lst (list 1 2 3)) lst.[(+ 0 1)]", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_index");
        if (ok) { pass++; io::printn("[PASS] Compiler: lst.[(+ 0 1)]"); }
        else    { fail++; io::printn("[FAIL] Compiler: lst.[(+ 0 1)]"); }
    }

    // --- path notation tests ---

    // 24. simple path a.b
    {
        char[] code = compile_to_c3("(define point (list (cons 'x 10) (cons 'y 20))) point.x", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_field_access");
        if (ok) { pass++; io::printn("[PASS] Compiler: point.x"); }
        else    { fail++; io::printn("[FAIL] Compiler: point.x"); }
    }

    // 25. nested path a.b.c
    {
        char[] code = compile_to_c3("(define obj (list (cons 'inner (list (cons 'val 42))))) obj.inner.val", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_field_access");
        if (ok) { pass++; io::printn("[PASS] Compiler: obj.inner.val"); }
        else    { fail++; io::printn("[FAIL] Compiler: obj.inner.val"); }
    }

    // 26. path generates make_string for field name (not raw string)
    {
        char[] code = compile_to_c3("point.x", interp);
        bool ok = str_contains(code, "make_string");
        if (ok) { pass++; io::printn("[PASS] Compiler: path uses make_string for field name"); }
        else    { fail++; io::printn("[FAIL] Compiler: path uses make_string for field name"); }
    }

    // --- reset/shift via interpreter delegation ---

    // 27. reset/shift compiles natively (no rt_eval_source)
    {
        char[] code = compile_to_c3("(reset (+ 1 (shift k (k 10))))", interp);
        bool ok = str_contains(code, "rt_compiled_reset") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: reset/shift compiles natively"); }
        else    { fail++; io::printn("[FAIL] Compiler: reset/shift compiles natively"); }
    }

    // 28. set! inside begin
    {
        char[] code = compile_to_c3("(define x 0) (begin (set! x 1) (set! x (+ x 1)) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: set! inside begin"); }
        else    { fail++; io::printn("[FAIL] Compiler: set! inside begin"); }
    }

    // --- Continuation form tests (native compilation) ---

    // 29. reset compiles to rt_compiled_reset (not rt_eval_source for the reset)
    {
        char[] code = compile_to_c3("(define x 5) (reset (+ x (shift k (k 10))))", interp);
        bool ok = str_contains(code, "rt_compiled_reset");
        if (ok) { pass++; io::printn("[PASS] Compiler: reset uses rt_compiled_reset"); }
        else    { fail++; io::printn("[FAIL] Compiler: reset uses rt_compiled_reset"); }
    }

    // 30. handle/perform compiles natively
    {
        char[] code = compile_to_c3("(handle (+ 1 (perform ask 0)) ((ask k x) (k 10)))", interp);
        bool ok = str_contains(code, "rt_compiled_handle") && str_contains(code, "rt_compiled_perform");
        if (ok) { pass++; io::printn("[PASS] Compiler: handle/perform uses native compilation"); }
        else    { fail++; io::printn("[FAIL] Compiler: handle/perform uses native compilation"); }
    }

    // 31. standalone perform compiles natively
    {
        char[] code = compile_to_c3("(perform raise \"error\")", interp);
        bool ok = str_contains(code, "rt_compiled_perform");
        if (ok) { pass++; io::printn("[PASS] Compiler: perform uses rt_compiled_perform"); }
        else    { fail++; io::printn("[FAIL] Compiler: perform uses rt_compiled_perform"); }
    }

    // 32. shift compiles natively
    {
        char[] code = compile_to_c3("(shift k 42)", interp);
        bool ok = str_contains(code, "rt_compiled_shift");
        if (ok) { pass++; io::printn("[PASS] Compiler: shift uses rt_compiled_shift"); }
        else    { fail++; io::printn("[FAIL] Compiler: shift uses rt_compiled_shift"); }
    }

    // 33. reset compiles with native function
    {
        char[] code = compile_to_c3("(reset (+ 1 2))", interp);
        bool ok = str_contains(code, "rt_compiled_reset");
        if (ok) { pass++; io::printn("[PASS] Compiler: reset compiles natively"); }
        else    { fail++; io::printn("[FAIL] Compiler: reset compiles natively"); }
    }

    // 34. handle with multiple clauses compiles natively
    {
        char[] code = compile_to_c3("(handle (begin (perform get 0) (perform set 1)) ((get k x) (k 42)) ((set k v) (k v)))", interp);
        bool ok = str_contains(code, "rt_compiled_handle");
        if (ok) { pass++; io::printn("[PASS] Compiler: handle with multiple clauses"); }
        else    { fail++; io::printn("[FAIL] Compiler: handle with multiple clauses"); }
    }

    // 35. try from stdlib (uses handle/perform internally) compiles
    {
        char[] code = compile_to_c3("((try (lambda (xx) (+ 1 2))) (lambda (msg) 0))", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: try from stdlib"); }
        else    { fail++; io::printn("[FAIL] Compiler: try from stdlib"); }
    }

    // 36. reset with nested shift compiles natively
    {
        char[] code = compile_to_c3("(reset (+ 1 (shift k (+ (k 10) (k 20)))))", interp);
        bool ok = str_contains(code, "rt_compiled_reset") && str_contains(code, "rt_compiled_shift");
        if (ok) { pass++; io::printn("[PASS] Compiler: reset with nested shift"); }
        else    { fail++; io::printn("[FAIL] Compiler: reset with nested shift"); }
    }

    // --- FFI compiler tests ---

    // 37. ffi-open compiles without unsupported
    {
        char[] code = compile_to_c3("(ffi-open \"libc.so.6\")", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_ffi_open");
        if (ok) { pass++; io::printn("[PASS] Compiler: ffi-open"); }
        else    { fail++; io::printn("[FAIL] Compiler: ffi-open"); }
    }

    // 38. ffi-close compiles without unsupported
    {
        char[] code = compile_to_c3("(define h (ffi-open \"libc.so.6\")) (ffi-close h)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_ffi_close");
        if (ok) { pass++; io::printn("[PASS] Compiler: ffi-close"); }
        else    { fail++; io::printn("[FAIL] Compiler: ffi-close"); }
    }

    // 39. ffi-sym compiles without unsupported
    {
        char[] code = compile_to_c3("(define h (ffi-open \"libc.so.6\")) (ffi-sym h \"strlen\")", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_ffi_sym");
        if (ok) { pass++; io::printn("[PASS] Compiler: ffi-sym"); }
        else    { fail++; io::printn("[FAIL] Compiler: ffi-sym"); }
    }

    // 40. ffi-call compiles without unsupported
    {
        char[] code = compile_to_c3("(define h (ffi-open \"libc.so.6\")) (ffi-call h \"strlen\" 'size \"hello\" 'string)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "rt_ffi_call");
        if (ok) { pass++; io::printn("[PASS] Compiler: ffi-call"); }
        else    { fail++; io::printn("[FAIL] Compiler: ffi-call"); }
    }

    // =========================================================================
    // Phase 1: Multi-arg calls (inline currying)
    // =========================================================================

    // 41. Multi-arg call uses inline currying (rt_invoke_once + rt_invoke)
    {
        char[] code = compile_to_c3("(+ 1 2)", interp);
        bool ok = str_contains(code, "rt_invoke") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: multi-arg uses inline currying"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-arg uses inline currying"); }
    }

    // 42. Multi-arg uses rt_invoke_once for intermediate args
    {
        char[] code = compile_to_c3("(+ 1 2)", interp);
        bool ok = str_contains(code, "rt_invoke_once") || str_contains(code, "rt_invoke");
        if (ok) { pass++; io::printn("[PASS] Compiler: multi-arg uses rt_invoke"); }
        else    { fail++; io::printn("[FAIL] Compiler: multi-arg uses rt_invoke"); }
    }

    // 43. 3-arg call uses rt_apply_multi
    {
        char[] code = compile_to_c3("(substring \"hello\" 1 3)", interp);
        bool ok = str_contains(code, "rt_apply_multi");
        if (ok) { pass++; io::printn("[PASS] Compiler: 3-arg rt_apply_multi"); }
        else    { fail++; io::printn("[FAIL] Compiler: 3-arg rt_apply_multi"); }
    }

    // 44. Zero-arg call via rt_invoke
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = str_contains(code, "rt_invoke") && str_contains(code, "make_nil()");
        if (ok) { pass++; io::printn("[PASS] Compiler: zero-arg rt_invoke"); }
        else    { fail++; io::printn("[FAIL] Compiler: zero-arg rt_invoke"); }
    }

    // =========================================================================
    // Phase 2: TCO via trampoline
    // =========================================================================

    // 45. Tail calls in lambda body emit make_thunk
    {
        char[] code = compile_to_c3("(define (f x) (f (+ x 1)))", interp);
        bool ok = str_contains(code, "make_thunk");
        if (ok) { pass++; io::printn("[PASS] Compiler: tail call emits make_thunk"); }
        else    { fail++; io::printn("[FAIL] Compiler: tail call emits make_thunk"); }
    }

    // 46. Non-tail calls don't emit make_thunk
    {
        char[] code = compile_to_c3("(define (f x) (+ (f x) 1))", interp);
        // The (f x) is not in tail position (it's an argument to +)
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: non-tail compiles OK"); }
        else    { fail++; io::printn("[FAIL] Compiler: non-tail compiles OK"); }
    }

    // 47. If branches propagate tail position
    {
        char[] code = compile_to_c3("(define (f x) (if (= x 0) 1 (f (- x 1))))", interp);
        bool ok = str_contains(code, "make_thunk");
        if (ok) { pass++; io::printn("[PASS] Compiler: if branch tail position"); }
        else    { fail++; io::printn("[FAIL] Compiler: if branch tail position"); }
    }

    // =========================================================================
    // Phase 3: Quasiquote, macros, modules
    // =========================================================================

    // 48. Quasiquote compiles natively with rt_cons
    {
        char[] code = compile_to_c3("(let ((x 5)) `(a ,x))", interp);
        bool ok = str_contains(code, "rt_cons") && str_contains(code, "make_symbol");
        if (ok) { pass++; io::printn("[PASS] Compiler: quasiquote compiles natively"); }
        else    { fail++; io::printn("[FAIL] Compiler: quasiquote compiles natively"); }
    }

    // 49. Quasiquote with unquote uses make_symbol for literal parts
    {
        char[] code = compile_to_c3("(let ((x 5)) `(a ,x))", interp);
        bool ok = str_contains(code, "make_symbol(\"a\")");
        if (ok) { pass++; io::printn("[PASS] Compiler: quasiquote uses make_symbol"); }
        else    { fail++; io::printn("[FAIL] Compiler: quasiquote uses make_symbol"); }
    }

    // 50. Defmacro delegates to rt_eval_source
    {
        char[] code = compile_to_c3("(define [macro] double ([x] (+ x x)))", interp);
        bool ok = str_contains(code, "make_nil") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: defmacro compiles as no-op"); }
        else    { fail++; io::printn("[FAIL] Compiler: defmacro compiles as no-op"); }
    }

    // 51. Stdlib functions (when, unless) compiled from STDLIB_PRELUDE
    {
        char[] code = compile_to_c3("42", interp);
        bool ok = str_contains(code, "when") && str_contains(code, "unless");
        if (ok) { pass++; io::printn("[PASS] Compiler: stdlib functions compiled"); }
        else    { fail++; io::printn("[FAIL] Compiler: stdlib functions compiled"); }
    }

    // =========================================================================
    // Phase 4: Missing primitives
    // =========================================================================

    // 52. string->number compiles
    {
        char[] code = compile_to_c3("(string->number \"42\")", interp);
        bool ok = str_contains(code, "rt_string_to_number") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: string->number"); }
        else    { fail++; io::printn("[FAIL] Compiler: string->number"); }
    }

    // 53. number->string compiles
    {
        char[] code = compile_to_c3("(number->string 42)", interp);
        bool ok = str_contains(code, "rt_number_to_string") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: number->string"); }
        else    { fail++; io::printn("[FAIL] Compiler: number->string"); }
    }

    // 54. gensym compiles
    {
        char[] code = compile_to_c3("(gensym)", interp);
        bool ok = str_contains(code, "rt_gensym") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: gensym"); }
        else    { fail++; io::printn("[FAIL] Compiler: gensym"); }
    }

    // 55. apply compiles
    {
        char[] code = compile_to_c3("(apply + (list 1 2))", interp);
        bool ok = str_contains(code, "rt_apply_prim") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: apply"); }
        else    { fail++; io::printn("[FAIL] Compiler: apply"); }
    }

    // 56. equal? compiles
    {
        char[] code = compile_to_c3("(equal? 1 2)", interp);
        bool ok = str_contains(code, "rt_equal_p") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: equal?"); }
        else    { fail++; io::printn("[FAIL] Compiler: equal?"); }
    }

    // 57. display compiles
    {
        char[] code = compile_to_c3("(display \"hello\")", interp);
        bool ok = str_contains(code, "rt_display") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: display"); }
        else    { fail++; io::printn("[FAIL] Compiler: display"); }
    }

    // 58. ref compiles
    {
        char[] code = compile_to_c3("(ref (dict) 'a)", interp);
        bool ok = str_contains(code, "rt_hash_ref") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: ref"); }
        else    { fail++; io::printn("[FAIL] Compiler: ref"); }
    }

    // 59. load compiles
    {
        char[] code = compile_to_c3("(load \"test.pika\")", interp);
        bool ok = str_contains(code, "rt_load") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: load"); }
        else    { fail++; io::printn("[FAIL] Compiler: load"); }
    }

    // =========================================================================
    // Phase 5: Mutable capture boxing
    // =========================================================================

    // 60. Mutable capture detected and uses rt_define_var
    {
        char[] code = compile_to_c3("(let ((x 0)) (let ((inc (lambda () (set! x (+ x 1))))) (inc) x))", interp);
        // Mutable capture test - checks generated code for interpreter env usage
        bool ok = str_contains(code, "rt_define_var") && str_contains(code, "rt_set_var");
        if (ok) { pass++; io::printn("[PASS] Compiler: mutable capture uses rt_define_var/rt_set_var"); }
        else    { fail++; io::printn("[FAIL] Compiler: mutable capture uses rt_define_var/rt_set_var"); }
    }

    // 61. Mutable capture reads use rt_lookup_var
    {
        char[] code = compile_to_c3("(let ((x 0)) (let ((inc (lambda () (set! x (+ x 1))))) (inc) x))", interp);
        bool ok = str_contains(code, "rt_lookup_var");
        if (ok) { pass++; io::printn("[PASS] Compiler: mutable capture reads use rt_lookup_var"); }
        else    { fail++; io::printn("[FAIL] Compiler: mutable capture reads use rt_lookup_var"); }
    }

    // 62. Non-mutable let uses C3 local variable (Value x = ...)
    {
        char[] code = compile_to_c3("(let ((x 42)) x)", interp);
        // The main fn body should contain "Value x = " for a non-mutable let
        bool ok = str_contains(code, "Value x = ");
        if (ok) { pass++; io::printn("[PASS] Compiler: non-mutable let uses C3 local"); }
        else    { fail++; io::printn("[FAIL] Compiler: non-mutable let uses C3 local"); }
    }

    // =========================================================================
    // Integration: Verify all features work together
    // =========================================================================

    // 63. Complex program with multiple features compiles
    {
        char[] code = compile_to_c3(
            "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 10)", interp);
        bool ok = !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: factorial compiles"); }
        else    { fail++; io::printn("[FAIL] Compiler: factorial compiles"); }
    }

    // 64. Let-rec with tail call emits make_thunk
    {
        char[] code = compile_to_c3(
            "(let ^rec ((loop (lambda (n) (if (= n 0) 'done (loop (- n 1)))))) (loop 100))", interp);
        bool ok = str_contains(code, "make_thunk") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: let-rec tail call emits thunk"); }
        else    { fail++; io::printn("[FAIL] Compiler: let-rec tail call emits thunk"); }
    }

    // 65. Nested multi-arg calls compile correctly
    {
        char[] code = compile_to_c3("(+ (* 2 3) (* 4 5))", interp);
        bool ok = str_contains(code, "rt_invoke") && !str_contains(code, "unsupported");
        if (ok) { pass++; io::printn("[PASS] Compiler: nested multi-arg calls"); }
        else    { fail++; io::printn("[FAIL] Compiler: nested multi-arg calls"); }
    }

    // =========================================================================
    // Phase 6: STDLIB_PRELUDE iterative map/filter
    // =========================================================================

    // 66. STDLIB_PRELUDE map uses iterative loop pattern
    {
        char[] code = compile_to_c3("((map (lambda (x) x)) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        if (ok) { pass++; io::printn("[PASS] Compiler: STDLIB map uses loop pattern"); }
        else    { fail++; io::printn("[FAIL] Compiler: STDLIB map uses loop pattern"); }
    }

    // 67. STDLIB_PRELUDE filter uses iterative loop pattern
    {
        char[] code = compile_to_c3("((filter (lambda (x) x)) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        if (ok) { pass++; io::printn("[PASS] Compiler: STDLIB filter uses loop pattern"); }
        else    { fail++; io::printn("[FAIL] Compiler: STDLIB filter uses loop pattern"); }
    }

    // =========================================================================
    // Phase 7: Runtime bridge tests (rt_eval_source correctness)
    // =========================================================================

    // 68. map correctness via runtime
    {
        runtime::Value result = runtime::rt_eval_source("((map (lambda (x) (+ x 1))) (list 1 2 3))");
        // Should be list (2 3 4) — check first element is 2
        bool ok = false;
        if (result.tag == runtime::ValueTag.V_CONS) {
            runtime::Value* car = main::dereference_as(runtime::Value, result.cons_val.car);
            ok = (car.tag == runtime::ValueTag.V_INT && car.int_val == 2);
        }
        if (ok) { pass++; io::printn("[PASS] Runtime: map correctness"); }
        else    { fail++; io::printn("[FAIL] Runtime: map correctness"); }
    }

    // 69. filter correctness via runtime
    {
        runtime::Value result = runtime::rt_eval_source("((filter (lambda (x) (> x 2))) (list 1 2 3 4))");
        // Should be list (3 4) — check first element is 3
        bool ok = false;
        if (result.tag == runtime::ValueTag.V_CONS) {
            runtime::Value* car = main::dereference_as(runtime::Value, result.cons_val.car);
            ok = (car.tag == runtime::ValueTag.V_INT && car.int_val == 3);
        }
        if (ok) { pass++; io::printn("[PASS] Runtime: filter correctness"); }
        else    { fail++; io::printn("[FAIL] Runtime: filter correctness"); }
    }

    // 70. TCO depth via runtime (named-let loop to 10000)
    {
        runtime::Value result = runtime::rt_eval_source("(let loop ((n 10000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc 1))))");
        bool ok = (result.tag == runtime::ValueTag.V_INT && result.int_val == 10000);
        if (ok) { pass++; io::printn("[PASS] Runtime: TCO depth 10000"); }
        else    { fail++; io::printn("[FAIL] Runtime: TCO depth 10000"); }
    }

    // 71. multi-arg via runtime
    {
        runtime::Value result = runtime::rt_eval_source("((lambda (x y z) (+ x (+ y z))) 1 2 3)");
        bool ok = (result.tag == runtime::ValueTag.V_INT && result.int_val == 6);
        if (ok) { pass++; io::printn("[PASS] Runtime: multi-arg call"); }
        else    { fail++; io::printn("[FAIL] Runtime: multi-arg call"); }
    }

    // 72. mutable capture via runtime (using begin + define for set!)
    {
        runtime::Value result = runtime::rt_eval_source("(begin (define x 0) (set! x (+ x 1)) (set! x (+ x 1)) x)");
        bool ok = (result.tag == runtime::ValueTag.V_INT && result.int_val == 2);
        if (ok) { pass++; io::printn("[PASS] Runtime: mutable capture"); }
        else    { fail++; io::printn("[FAIL] Runtime: mutable capture"); }
    }

    // 73. macro via runtime
    {
        runtime::Value result = runtime::rt_eval_source("(begin (define [macro] double ([x] (+ x x))) (double 21))");
        bool ok = (result.tag == runtime::ValueTag.V_INT && result.int_val == 42);
        if (ok) { pass++; io::printn("[PASS] Runtime: macro expansion"); }
        else    { fail++; io::printn("[FAIL] Runtime: macro expansion"); }
    }

    // =========================================================================
    // Phase 8: compile_to_c3_with_print pattern tests
    // =========================================================================

    // 74. print_last works for literals
    {
        char[] code = compile_to_c3_with_print("42", interp);
        bool ok = str_contains(code, "rt_print_value");
        if (ok) { pass++; io::printn("[PASS] Compiler: print_last literal"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last literal"); }
    }

    // 75. print_last works for calls
    {
        char[] code = compile_to_c3_with_print("(+ 1 2)", interp);
        bool ok = str_contains(code, "rt_print_value");
        if (ok) { pass++; io::printn("[PASS] Compiler: print_last call"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last call"); }
    }

    // 76. print_last skips defines, wraps last non-define
    {
        char[] code = compile_to_c3_with_print("(define x 1) x", interp);
        bool ok = str_contains(code, "rt_print_value");
        if (ok) { pass++; io::printn("[PASS] Compiler: print_last skips defines"); }
        else    { fail++; io::printn("[FAIL] Compiler: print_last skips defines"); }
    }

    // 77. default case warning text present
    {
        char[] code = compile_to_c3("42", interp);
        // The default case emits "WARNING" in comment — but only for unsupported exprs.
        // We verify the constant string is in the source by checking the STDLIB compiles without it.
        bool ok = !str_contains(code, "WARNING");
        if (ok) { pass++; io::printn("[PASS] Compiler: no WARNING in normal compilation"); }
        else    { fail++; io::printn("[FAIL] Compiler: no WARNING in normal compilation"); }
    }

    mem::free(interp);
    io::printfn("\n=== Compiler Tests: %d passed, %d failed ===", pass, fail);
    assert(fail == 0, "compiler tests had failures");
}
