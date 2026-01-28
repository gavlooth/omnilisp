;; test_immer_nth.lisp - Tests for nth function from immer.omni library
;;
;; Tests the nth function which retrieves an element at a specific index
;; from a persistent vector backed by the Immer C++ library.
;;
;; Run with: ./omni tests/test_immer_nth.lisp

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

(define test-true [name] [actual]
  (set! test-count (+ test-count 1))
  (if actual
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name)
        (print "  Expected: true")
        (print "  Got: false"))))

(define test-nil [name] [actual]
  (test-true name (null? actual)))

;; ============================================================
;; Test 1: Basic nth operations - happy path
;; ============================================================

(print "")
(print "=== Test 1: Basic nth Operations ===")

;; Test 1.1: Get first element
(do
  (define v (vector 10 20 30 40 50))
  (test-eq "get first element" 10 (nth v 0)))

;; Test 1.2: Get middle element
(do
  (define v (vector 1 2 3 4 5))
  (test-eq "get middle element" 3 (nth v 2)))

;; Test 1.3: Get last element
(do
  (define v (vector 100 200 300))
  (test-eq "get last element" 300 (nth v 2)))

;; Test 1.4: Get elements with string values
(do
  (define v (vector "apple" "banana" "cherry"))
  (test-eq "get string element" "banana" (nth v 1)))

;; Test 1.5: Get elements with mixed types
(do
  (define v (vector 42 "hello" 3.14 'symbol))
  (test-eq "get integer from mixed" 42 (nth v 0))
  (test-eq "get string from mixed" "hello" (nth v 1))
  (test-eq "get float from mixed" 3.14 (nth v 2))
  (test-eq "get symbol from mixed" 'symbol (nth v 3)))

;; Test 1.6: Get element from single-element vector
(do
  (define v (vector 99))
  (test-eq "get only element" 99 (nth v 0)))

;; ============================================================
;; Test 2: Edge cases - index boundaries
;; ============================================================

(print "")
(print "=== Test 2: Boundary Cases ===")

;; Test 2.1: Index 0 on small vector
(do
  (define v (vector "a" "b"))
  (test-eq "index 0 on 2-element vector" "a" (nth v 0)))

;; Test 2.2: Index equal to size - 1 (last element)
(do
  (define v (vector 1 2 3 4 5 6 7 8 9 10))
  (test-eq "last index (size-1)" 10 (nth v 9)))

;; Test 2.3: Large index value
(do
  (define v (vector (range 100)))
  (test-eq "access element at index 50" 50 (nth v 50)))

;; ============================================================
;; Test 3: Out of bounds - error handling
;; ============================================================

(print "")
(print "=== Test 3: Out of Bounds ===")

;; Test 3.1: Negative index should return nil
(do
  (define v (vector 1 2 3))
  (test-nil "negative index returns nil" (nth v -1)))

;; Test 3.2: Index equal to vector size should return nil
(do
  (define v (vector 1 2 3))
  (test-nil "index equal to size returns nil" (nth v 3)))

;; Test 3.3: Index greater than vector size should return nil
(do
  (define v (vector 1 2 3))
  (test-nil "index greater than size returns nil" (nth v 10)))

;; Test 3.4: Index much greater than vector size
(do
  (define v (vector 42))
  (test-nil "huge index returns nil" (nth v 9999)))

;; ============================================================
;; Test 4: Empty vector
;; ============================================================

(print "")
(print "=== Test 4: Empty Vector ===")

;; Test 4.1: Access element from empty vector
(do
  (define v (vector))
  (test-nil "nth on empty vector returns nil" (nth v 0)))

;; ============================================================
;; Test 5: Persistent behavior - immutability
;; ============================================================

(print "")
(print "=== Test 5: Persistence ===")

;; Test 5.1: Original vector unchanged after nth access
(do
  (define v1 (vector 1 2 3))
  (define _elem (nth v1 1))
  (test-eq "original unchanged after nth" 2 (nth v1 1)))

;; Test 5.2: Multiple nth calls on same vector
(do
  (define v (vector 10 20 30 40 50))
  (test-eq "first call to nth" 10 (nth v 0))
  (test-eq "second call to nth" 30 (nth v 2))
  (test-eq "third call to nth" 50 (nth v 4)))

;; ============================================================
;; Test 6: Nested vectors
;; ============================================================

(print "")
(print "=== Test 6: Nested Vectors ===")

;; Test 6.1: Get element from nested vectors
(do
  (define inner (vector 1 2 3))
  (define outer (vector inner (vector 4 5 6)))
  (test-eq "get nested vector as element" inner (nth outer 0)))

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
