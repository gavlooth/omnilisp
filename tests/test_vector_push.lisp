;; test_vector_push.lisp - Tests for vector-push function from immer.omni library
;;
;; Tests vector-push which adds an element to the end of a persistent vector,
;; returning a new vector (original unchanged).
;;
;; Run with: ./omni tests/test_vector_push.lisp

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

(define test-bool [name] [expected] [actual]
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

;; ============================================================
;; Test 1: Basic push operations - happy path
;; ============================================================

(print "")
(print "=== Test 1: Basic Push Operations ===")

;; Test 1.1: Push to empty vector
(do
  (define v (vector))
  (define v2 (vector-push v 42))
  (test-eq "push to empty vector creates single element" 1 (vector-count v2))
  (test-eq "pushed element is at index 0" 42 (nth v2 0)))

;; Test 1.2: Push to non-empty vector
(do
  (define v (vector 1 2 3))
  (define v2 (vector-push v 4))
  (test-eq "push increases length" 4 (vector-count v2))
  (test-eq "new element at end" 4 (nth v2 3)))

;; Test 1.3: Push multiple times
(do
  (define v (vector))
  (define v2 (vector-push v 1))
  (define v3 (vector-push v2 2))
  (define v4 (vector-push v3 3))
  (test-eq "three pushes create 3 elements" 3 (vector-count v4))
  (test-eq "first element" 1 (nth v4 0))
  (test-eq "second element" 2 (nth v4 1))
  (test-eq "third element" 3 (nth v4 2)))

;; ============================================================
;; Test 2: Original Vector Unchanged (Immutability)
;; ============================================================

(print "")
(print "=== Test 2: Original Vector Unchanged ===")

;; Test 2.1: Original vector remains unchanged after push
(do
  (define v (vector 1 2 3))
  (define v2 (vector-push v 99))
  (test-eq "original length unchanged" 3 (vector-count v))
  (test-eq "original first element unchanged" 1 (nth v 0))
  (test-eq "original last element unchanged" 3 (nth v 2)))

;; Test 2.2: Multiple pushes don't affect intermediate vectors
(do
  (define v0 (vector 1))
  (define v1 (vector-push v0 2))
  (define v2 (vector-push v1 3))
  (test-eq "v0 has 1 element" 1 (vector-count v0))
  (test-eq "v1 has 2 elements" 2 (vector-count v1))
  (test-eq "v2 has 3 elements" 3 (vector-count v2)))

;; ============================================================
;; Test 3: Different Value Types
;; ============================================================

(print "")
(print "=== Test 3: Different Value Types ===")

;; Test 3.1: Push integers
(do
  (define v (vector))
  (define v2 (vector-push v 42))
  (test-eq "push integer" 42 (nth v2 0)))

;; Test 3.2: Push strings
(do
  (define v (vector))
  (define v2 (vector-push v "hello"))
  (test-eq "push string" "hello" (nth v2 0)))

;; Test 3.3: Push floats
(do
  (define v (vector))
  (define v2 (vector-push v 3.14))
  (test-eq "push float" 3.14 (nth v2 0)))

;; Test 3.4: Push symbols
(do
  (define v (vector))
  (define v2 (vector-push v 'symbol))
  (test-eq "push symbol" 'symbol (nth v2 0)))

;; Test 3.5: Push boolean
(do
  (define v (vector))
  (define v2 (vector-push v true))
  (test-eq "push boolean" true (nth v2 0)))

;; Test 3.6: Push nothing
(do
  (define v (vector))
  (define v2 (vector-push v nothing))
  (test-eq "push nothing" nothing (nth v2 0)))

;; ============================================================
;; Test 4: Mixed Types
;; ============================================================

(print "")
(print "=== Test 4: Mixed Types ===")

;; Test 4.1: Push different types to same vector
(do
  (define v (vector))
  (define v2 (vector-push v 1))
  (define v3 (vector-push v2 "two"))
  (define v4 (vector-push v3 3.0))
  (define v5 (vector-push v4 'four))
  (test-eq "mixed types count" 4 (vector-count v5))
  (test-eq "first element integer" 1 (nth v5 0))
  (test-eq "second element string" "two" (nth v5 1))
  (test-eq "third element float" 3.0 (nth v5 2))
  (test-eq "fourth element symbol" 'four (nth v5 3)))

;; ============================================================
;; Test 5: Edge Cases
;; ============================================================

(print "")
(print "=== Test 5: Edge Cases ===")

;; Test 5.1: Push zero
(do
  (define v (vector))
  (define v2 (vector-push v 0))
  (test-eq "push zero" 0 (nth v2 0)))

;; Test 5.2: Push negative number
(do
  (define v (vector))
  (define v2 (vector-push v -42))
  (test-eq "push negative" -42 (nth v2 0)))

;; Test 5.3: Push empty string
(do
  (define v (vector))
  (define v2 (vector-push v ""))
  (test-eq "push empty string" "" (nth v2 0)))

;; Test 5.4: Push nil (if supported)
(do
  (define v (vector 1 2))
  (define v2 (vector-push v nil))
  (test-eq "push nil" nil (nth v2 2)))

;; ============================================================
;; Test 6: Building Vectors
;; ============================================================

(print "")
(print "=== Test 6: Building Vectors ===")

;; Test 6.1: Build vector from range using push
(do
  (define v (vector))
  (define v1 (vector-push v 0))
  (define v2 (vector-push v1 1))
  (define v3 (vector-push v2 2))
  (define v4 (vector-push v3 3))
  (define v5 (vector-push v4 4))
  (test-eq "built vector length" 5 (vector-count v5))
  (test-eq "element at index 2" 2 (nth v5 2))
  (test-eq "last element" 4 (nth v5 4)))

;; Test 6.2: Chain multiple pushes
(do
  (define v (vector 1))
  (define v2 (vector-push (vector-push (vector-push v 2) 3) 4))
  (test-eq "chained pushes length" 4 (vector-count v2))
  (test-eq "elements in order" 1 (nth v2 0))
  (test-eq "last element from chain" 4 (nth v2 3)))

;; ============================================================
;; Test 7: Large Vectors
;; ============================================================

(print "")
(print "=== Test 7: Large Vectors ===")

;; Test 7.1: Push many elements
(do
  (define v (vector))
  (define (build-vector vec n)
    (if (= n 0)
        vec
        (build-vector (vector-push vec n) (- n 1))))
  (define v100 (build-vector v 100))
  (test-eq "100 elements" 100 (vector-count v100))
  (test-eq "first element" 100 (nth v100 0))
  (test-eq "last element" 1 (nth v100 99)))

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
