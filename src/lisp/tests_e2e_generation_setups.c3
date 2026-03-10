module lisp;

// E2E setup forms are evaluated first in interpreter, then compiled into the
// generated AOT program before per-expression comparisons.
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

    // Type/dispatch parity fixtures
    `(define [abstract] Entity)`,
    `(define [struct] (User Entity) (^String name))`,
    `(define [struct] (Admin User) (^String role))`,
    `(define [union] MaybeNum None (Some Int))`,
    `(define [alias] UserAlias User)`,
    `(define (who (^User u)) "user")`,
    `(define (who (^Admin a)) "admin")`,
    `(define (who-parent (^User u)) "parent")`,
    `(define (who (^Double n)) "double")`,
    `(define (ping (^(Value ping) x)) "pong")`,
    `(define (ping (^Symbol x)) "sym")`,
    `(define (alias-check (^UserAlias u)) "alias-ok")`,
    `(define (amb (^Number x)) "n1")`,
    `(define (amb (^Number y)) "n2")`,
};
