;; test_vector_nth_or.lisp - Tests for nth-or function from immer.omni library
;;
;; Tests;; nth-or function which retrieves an element at a specific index
;; from a persistent vector, returning a default value if the index is out of bounds.
;;
;; Run with: ./omni tests/test_vector_nth_or.lisp

;; Load immer library
(load "lib/immer.omni")

;; ============================================================
;; Test Framework
;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define test-eq [name] [expected] [actual]
  (set! test-count (+ test-count 1))
  (if (= expected actual)
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name)
        (print "  Expected:" expected)
        (print "  Got:" actual))))

(define test-nil [name] [actual]
  (test-eq name nil (if (null? actual) nil actual)))

;; ============================================================
;; Test 1: Basic nth-or operations - happy path
;; ============================================================

(print "")
(print "=== Test 1: Basic nth-or Operations ===")

;; Test 1.1: Get first element with default
(do
  (define v (vector 10 20 30 40 50))
  (test-eq "get first element" 10 (nth-or v 0 :not-found)))

;; Test 1.2: Get middle element with default
(do
  (define v (vector 1 2 3 4 5))
  (test-eq "get middle element" 3 (nth-or v 2 :default)))

;; Test 1.3: Get last element with default
(do
  (define v (vector 100 200 300))
  (test-eq "get last element" 300 (nth-or v 2 :fallback)))

;; ============================================================
;; Test 2: Out of bounds - return default value
;; ============================================================

(print "")
(print "=== Test 2: Out of Bounds Returns Default ===")

;; Test 2.1: Index equal to vector size
(do
  (define v (vector 1 2 3))
  (test-eq "index equal to size returns default" :not-found (nth-or v 3 :not-found)))

;; Test 2.2: Index greater than vector size
(do
  (define v (vector "a" "b" "c"))
  (test-eq "index greater than size returns default" :default (nth-or v 10 :default)))

;; Test 2.3: Large out of bounds index
(do
  (define v (vector 42))
  (test-eq "huge index returns default" :fallback (nth-or v 9999 :fallback)))

;; Test 2.4: Negative index returns default
(do
  (define v (vector 1 2 3))
  (test-eq "negative index returns default" :error (nth-or v -1 :error)))

;; ============================================================
;; Test 3: Different default value types
;; ============================================================

(print "")
(print "=== Test 3: Different Default Value Types ===")

;; Test 3.1: Integer default
(do
  (define v (vector 1 2))
  (test-eq "integer default type" -1 (nth-or v 5 -1)))

;; Test 3.2: String default
(do
  (define v (vector "a" "b"))
  (test-eq "string default type" "not-found" (nth-or v 10 "not-found")))

;; Test 3.3: Symbol default
(do
  (define v (vector 10 20))
  (test-eq "symbol default type" 'missing (nth-or v 5 'missing)))

;; Test 3.4: Nil default
(do
  (define v (vector 1 2 3))
  (test-nil "nil default type" (nth-or v 100 nil)))

;; ============================================================
;; Test 4: Empty vector
;; ============================================================

(print "")
(print "=== Test 4: Empty Vector ===")

;; Test 4.1: Any index on empty vector returns default
(do
  (define v (vector))
  (test-eq "empty vector index 0 returns default" :empty (nth-or v 0 :empty)))

;; Test 4.2: Out of bounds on empty vector
(do
  (define v (vector))
  (test-eq "empty vector index 100 returns default" :empty (nth-or v 100 :empty)))

;; ============================================================
;; Test 5: Vector with special values
;; ============================================================

(print "")
(print "=== Test 5: Special Values ===")

;; Test 5.1: Boolean values
(do
  (define v (vector true false true))
  (test-eq "get boolean element" false (nth-or v 1 :default)))

;; Test 5.2: Nothing value
(do
  (define v (vector nothing 42))
  (test-eq "get nothing element" nothing (nth-or v 0 :default)))

;; Test 5.3: Mixed types with out of bounds
(do
  (define v (vector 1 "hello" 3.14 'symbol))
  (test-eq "out of bounds returns default" :not-found (nth-or v 10 :not-found)))

;; ============================================================
;; Test 6: Single element vector
;; ============================================================

(print "")
(print "=== Test 6: Single Element Vector ===")

;; Test 6.1: Get only element
(do
  (define v (vector 99))
  (test-eq "get only element" 99 (nth-or v 0 :not-found)))

;; Test 6.2: Out of bounds on single element
(do
  (define v (vector 99))
  (test-eq "index 1 on single element returns default" :default (nth-or v 1 :default)))

;; Test 6.3: Negative index on single element
(do
  (define v (vector 99))
  (test-eq "negative index on single element" :error (nth-or v -1 :error)))

;; ============================================================
;; Test Results
;; ============================================================

(print "")
(print "=== Test Results ===")
(print "Total:" test-count)
(print "Passed:" pass-count)
(print "Failed:" fail-count)

(if (= fail-count 0)
    (print "ALL TESTS PASSED!")
    (print "SOME TESTS FAILED"))

;; Return count of failures (0 = success)
fail-count
