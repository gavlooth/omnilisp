module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4d: STDLIB PRELUDE
// =============================================================================

/**
 * Standard library definitions prepended to all compiled programs.
 * These are the same HOFs defined in register_stdlib() in eval.c3.
 */
const char[] STDLIB_PRELUDE =
`(define with-trampoline (lambda (thunk) (handle (thunk nil) (bounce next-thunk (resolve (with-trampoline next-thunk))))))
(define (__proper-list? xs) (let loop (cur xs) (if (null? cur) true (if (pair? cur) (loop (cdr cur)) nil))))
(define (__require-proper-list xs name) (if (__proper-list? xs) nil (error (string-append name ": expected a proper list"))))
(define (reverse lst) (let (bad (__require-proper-list lst "reverse")) (if (error? bad) bad (let loop (xs lst acc nil) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))))
(define (map f lst) (let (bad (__require-proper-list lst "map")) (if (error? bad) bad (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (loop (cdr xs) (cons (f (car xs)) acc)))))))
(define (filter pred lst) (let (bad (__require-proper-list lst "filter")) (if (error? bad) bad (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (if (pred (car xs)) (loop (cdr xs) (cons (car xs) acc)) (loop (cdr xs) acc)))))))
(define (foldl f acc lst) (let (bad (__require-proper-list lst "foldl")) (if (error? bad) bad (let loop (a acc xs lst) (if (null? xs) a (loop (f a (car xs)) (cdr xs)))))))
(define (foldr f init lst) (let (bad (__require-proper-list lst "foldr")) (if (error? bad) bad (foldl (lambda (acc x) (f x acc)) init (reverse lst)))))
(define (append a b) (let (bad-b (__require-proper-list b "append")) (if (error? bad-b) bad-b (let (xs (reverse a)) (if (error? xs) xs (let loop (xs xs acc b) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))))))
(define (compose f g) (lambda (x) (f (g x))))
(define (id x) x)
(define (nth n lst) (let (bad (__require-proper-list lst "nth")) (if (error? bad) bad (let loop (i n xs lst) (if (= i 0) (car xs) (loop (- i 1) (cdr xs)))))))
(define (take n lst) (let (bad (__require-proper-list lst "take")) (if (error? bad) bad (let loop (i n xs lst acc nil) (if (= i 0) (reverse acc) (if (null? xs) (reverse acc) (loop (- i 1) (cdr xs) (cons (car xs) acc))))))))
(define (drop n lst) (let (bad (__require-proper-list lst "drop")) (if (error? bad) bad (let loop (i n xs lst) (if (= i 0) xs (if (null? xs) nil (loop (- i 1) (cdr xs))))))))
(define (zip a b) (let (bad-a (__require-proper-list a "zip")) (if (error? bad-a) bad-a (let (bad-b (__require-proper-list b "zip")) (if (error? bad-b) bad-b (let loop (xs a ys b acc nil) (if (or (null? xs) (null? ys)) (reverse acc) (loop (cdr xs) (cdr ys) (cons (cons (car xs) (car ys)) acc)))))))))
(define (range n) (let loop (i (- n 1) acc nil) (if (< i 0) acc (loop (- i 1) (cons i acc)))))
(define (for-each f lst) (let (bad (__require-proper-list lst "for-each")) (if (error? bad) bad (let loop (xs lst) (if (null? xs) nil (block (f (car xs)) (loop (cdr xs))))))))
(define (any? pred lst) (let (bad (__require-proper-list lst "any?")) (if (error? bad) bad (let loop (xs lst) (if (null? xs) nil (if (pred (car xs)) true (loop (cdr xs))))))))
(define (every? pred lst) (let (bad (__require-proper-list lst "every?")) (if (error? bad) bad (let loop (xs lst) (if (null? xs) true (if (pred (car xs)) (loop (cdr xs)) nil))))))
(define (try thunk handler) (handle (thunk nil) (raise msg (handler msg))))
(define (assert! condition msg) (if condition true (signal raise msg)))
(define when (lambda (test body) (if test body nil)))
(define unless (lambda (test body) (if test nil body)))`;
