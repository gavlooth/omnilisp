;; Omni Lisp Standard Library
;; Embedded at compile time via $embed("stdlib/stdlib.lisp")

;; =========================================================================
;; Built-in Effect Declarations (typed I/O effects)
;; =========================================================================
(define [effect] (io/print (^Any x)))
(define [effect] (io/println (^Any x)))
(define [effect] (io/display (^Any x)))
(define [effect] (io/newline (^Any x)))
(define [effect] (io/read-file (^String path)))
(define [effect] (io/write-file (^Any x)))
(define [effect] (io/file-exists? (^String path)))
(define [effect] (io/read-lines (^String path)))

;; =========================================================================
;; Standard Macros (defined before HOFs since macros may be used by them)
;; =========================================================================
(define [macro] when ([test .. body] (if test (begin .. body) nil)))
(define [macro] unless ([test .. body] (if test nil (begin .. body))))
(define [macro] cond ([] nil) ([test body .. rest] (if test body (cond .. rest))))
(define with-trampoline (lambda (thunk) (handle (thunk nil) ((bounce k next-thunk) (k (with-trampoline next-thunk))))))

;; =========================================================================
;; Type Predicates (defined via is? and abstract type hierarchy)
;; =========================================================================
(define (int? x) (is? x 'Int))
(define (double? x) (is? x 'Double))
(define (number? x) (is? x 'Number))
(define (string? x) (is? x 'String))
(define (symbol? x) (is? x 'Symbol))
(define (boolean? x) (or (null? x) (= x true)))
(define (list? x) (or (null? x) (pair? x)))
(define (closure? x) (is? x 'Closure))
(define (array? x) (is? x 'Array))
(define (dict? x) (is? x 'Dict))

;; =========================================================================
;; Numeric Predicates (defined via dispatched comparison ops)
;; =========================================================================
(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? x) (= (% x 2) 0))
(define (odd? x) (not (= (% x 2) 0)))

;; =========================================================================
;; Higher-Order Functions (all curried, list-based)
;; =========================================================================

;; map: (map f lst) - apply f to each element, return new list
(define map (lambda (f) (lambda (lst) (let loop ((xs lst) (acc nil)) (if (null? xs) (reverse acc) (loop (cdr xs) (cons (f (car xs)) acc)))))))

;; filter: (filter pred lst) - keep elements where (pred x) is truthy
(define filter (lambda (pred) (lambda (lst) (let loop ((xs lst) (acc nil)) (if (null? xs) (reverse acc) (if (pred (car xs)) (loop (cdr xs) (cons (car xs) acc)) (loop (cdr xs) acc)))))))

;; foldl: (foldl f acc lst) - left fold
(define foldl (lambda (f) (lambda (acc) (lambda (lst) (if (null? lst) acc (((foldl f) ((f acc) (car lst))) (cdr lst)))))))

;; foldr: (foldr f init lst) - right fold (iterative: reverse + foldl with flipped f)
(define foldr (lambda (f) (lambda (init) (lambda (lst) (((foldl (lambda (acc) (lambda (x) ((f x) acc)))) init) (reverse lst))))))

;; append: (append a b) - concatenate two lists (iterative: reverse + prepend)
(define append (lambda (a) (lambda (b) (let loop ((xs (let rev ((l a) (r nil)) (if (null? l) r (rev (cdr l) (cons (car l) r))))) (acc b)) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))))

;; reverse: (reverse lst) - reverse a list (iterative)
(define reverse (lambda (lst) (let loop ((xs lst) (acc nil)) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc))))))

;; compose: (compose f g) - function composition, returns (lambda (x) (f (g x)))
(define compose (lambda (f) (lambda (g) (lambda (x) (f (g x))))))

;; partial: (partial f . initial-args) - partial application
(define (partial f .. initial-args) (lambda (.. new-args) (apply f ((append initial-args) new-args))))

;; id: identity function
(define id (lambda (x) x))

;; nth: (nth n lst) - get nth element (0-indexed)
(define nth (lambda (n) (lambda (lst) (if (= n 0) (car lst) ((nth (- n 1)) (cdr lst))))))

;; take: (take n lst) - first n elements (iterative)
(define take (lambda (n) (lambda (lst) (let loop ((i n) (xs lst) (acc nil)) (if (= i 0) (reverse acc) (if (null? xs) (reverse acc) (loop (- i 1) (cdr xs) (cons (car xs) acc))))))))

;; drop: (drop n lst) - skip first n elements
(define drop (lambda (n) (lambda (lst) (if (= n 0) lst (if (null? lst) nil ((drop (- n 1)) (cdr lst)))))))

;; zip: (zip a b) - zip two lists into list of pairs (iterative)
(define zip (lambda (a) (lambda (b) (let loop ((xs a) (ys b) (acc nil)) (if (or (null? xs) (null? ys)) (reverse acc) (loop (cdr xs) (cdr ys) (cons (cons (car xs) (car ys)) acc)))))))

;; range: (range n) - list from 0 to n-1 (iterative, builds in reverse)
(define range (lambda (n) (let loop ((i (- n 1)) (acc nil)) (if (< i 0) acc (loop (- i 1) (cons i acc))))))

;; for-each: (for-each f lst) - apply f to each element for side effects, return nil
(define for-each (lambda (f) (lambda (lst) (if (null? lst) nil (let ((_r (f (car lst)))) ((for-each f) (cdr lst)))))))

;; any?: (any? pred lst) - true if pred is truthy for any element
(define any? (lambda (pred) (lambda (lst) (if (null? lst) nil (if (pred (car lst)) true ((any? pred) (cdr lst)))))))

;; every?: (every? pred lst) - true if pred is truthy for all elements
(define every? (lambda (pred) (lambda (lst) (if (null? lst) true (if (pred (car lst)) ((every? pred) (cdr lst)) nil)))))

;; =========================================================================
;; Error Handling Convention (via algebraic effects)
;; =========================================================================

;; try: run a thunk, catch errors via 'raise' effect
(define try (lambda (thunk) (lambda (handler) (handle (thunk nil) ((raise k msg) (handler msg))))))

;; assert!: check condition, raise if false
(define assert! (lambda (condition) (lambda (msg) (if condition true (perform raise msg)))))

;; =========================================================================
;; Association List Helpers
;; =========================================================================

;; assoc: (assoc key alist) - find pair with matching key
(define assoc (lambda (key) (lambda (alist) (if (null? alist) nil (if (= (car (car alist)) key) (car alist) ((assoc key) (cdr alist)))))))

;; assoc-ref: (assoc-ref key alist) - get value for key (cdr of pair)
(define assoc-ref (lambda (key) (lambda (alist) (let ((pair ((assoc key) alist))) (if (null? pair) nil (cdr pair))))))

;; =========================================================================
;; Mathematical Constants
;; =========================================================================
(define pi 3.141592653589793)
(define e 2.718281828459045)

;; =========================================================================
;; Additional HOFs
;; =========================================================================

;; flatten: flatten nested lists into a flat list
(define (flatten lst) (let loop ((l lst) (acc nil)) (if (null? l) (reverse acc) (if (pair? (car l)) (loop (cdr l) (let loop2 ((inner (car l)) (a acc)) (if (null? inner) a (loop2 (cdr inner) (cons (car inner) a))))) (loop (cdr l) (cons (car l) acc))))))

;; partition: split list by predicate, returns (kept . rejected)
(define (partition pred lst) (let loop ((l lst) (yes nil) (no nil)) (if (null? l) (cons (reverse yes) (reverse no)) (if (pred (car l)) (loop (cdr l) (cons (car l) yes) no) (loop (cdr l) yes (cons (car l) no))))))

;; remove: remove elements matching predicate (uses filter)
(define (remove pred lst) ((filter (lambda (x) (not (pred x)))) lst))

;; find: first element matching predicate, or nil
(define (find pred lst) (let loop ((l lst)) (if (null? l) nil (if (pred (car l)) (car l) (loop (cdr l))))))

;; =========================================================================
;; Generators & Lazy Streams (using existing effects)
;; =========================================================================

;; yield: (yield val) inside a generator — returns (cons val continuation)
(define [macro] yield ([val] (shift k (cons val k))))

;; stream-take: consume n values from a generator continuation
(define (stream-take n gen) (let loop ((i n) (g gen) (acc nil)) (if (= i 0) (reverse acc) (if (null? g) (reverse acc) (let ((pair (if (procedure? g) (g nil) g))) (if (null? pair) (reverse acc) (loop (- i 1) (cdr pair) (cons (car pair) acc))))))))

;; delay/force: promise (lazy evaluation)
(define (delay thunk) (let ((result nil) (forced nil)) (lambda () (if forced result (begin (set! result (thunk nil)) (set! forced true) result)))))
(define (force p) (p))

;; =========================================================================
;; I/O Effect Wrappers
;; These redefine I/O operations to go through the effects system.
;; When no handler is installed, eval_perform's fast path calls __raw-* directly.
;; =========================================================================
(define print (lambda (x) (perform io/print x)))
(define println (lambda (x) (perform io/println x)))
(define display (lambda (x) (perform io/display x)))
(define newline (lambda () (perform io/newline nil)))
(define read-file (lambda (path) (perform io/read-file path)))
(define write-file (lambda (path content) (perform io/write-file (cons path content))))
(define file-exists? (lambda (path) (perform io/file-exists? path)))
(define read-lines (lambda (path) (perform io/read-lines path)))
