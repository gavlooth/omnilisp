;; test_vector_assoc.lisp - Tests for vector-assoc function from immer.omni library
;;
;; Tests vector-assoc which sets an element at a specific index in a persistent vector,
;; returning a new vector with the updated element (original unchanged).
;;
;; Run with: ./omni tests/test_vector_assoc.lisp

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

;; ============================================================
;; Test 1: Basic assoc operations - happy path
;; ============================================================

(print "")
(print "=== Test 1: Basic Assoc Operations ===")

;; Test 1.1: Replace first element
(do
  (define v (vector 1 2 3 4 5))
  (define v2 (vector-assoc v 0 99))
  (test-eq "replace first element" 99 (nth v2 0))
  (test-eq "length unchanged" 5 (vector-count v2)))

;; Test 1.2: Replace middle element
(do
  (define v (vector 1 2 3 4 5))
  (define v2 (vector-assoc v 2 99))
  (test-eq "replace middle element" 99 (nth v2 2))
  (test-eq "other elements unchanged" 1 (nth v2 0)))

;; Test 1.3: Replace last element
(do
  (define v (vector 1 2 3 4 5))
  (define v2 (vector-assoc v 4 99))
  (test-eq "replace last element" 99 (nth v2 4))
  (test-eq "length unchanged" 5 (vector-count v2)))

;; Test 1.4: Replace with same value
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 2))
  (test-eq "same value replacement" 2 (nth v2 1))
  (test-eq "length unchanged" 3 (vector-count v2)))

;; ============================================================
;; Test 2: Original Vector Unchanged (Immutability)
;; ============================================================

(print "")
(print "=== Test 2: Original Vector Unchanged ===")

;; Test 2.1: Original vector remains unchanged after assoc
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 99))
  (test-eq "original first element unchanged" 1 (nth v 0))
  (test-eq "original replaced element unchanged" 2 (nth v 1))
  (test-eq "original last element unchanged" 3 (nth v 2))
  (test-eq "original length unchanged" 3 (vector-count v)))

;; Test 2.2: Multiple assocs don't affect intermediate vectors
(do
  (define v0 (vector 1 2 3))
  (define v1 (vector-assoc v0 0 10))
  (define v2 (vector-assoc v1 1 20))
  (test-eq "v0 element unchanged" 1 (nth v0 0))
  (test-eq "v1 has its value" 10 (nth v1 0))
  (test-eq "v1 other elements unchanged" 2 (nth v1 1))
  (test-eq "v2 has v1's changes" 10 (nth v2 0))
  (test-eq "v2 has its own change" 20 (nth v2 1))
  (test-eq "v2 third element from v0" 3 (nth v2 2)))

;; ============================================================
;; Test 3: Different Value Types
;; ============================================================

(print "")
(print "=== Test 3: Different Value Types ===")

;; Test 3.1: Replace with integer
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 42))
  (test-eq "replace with integer" 42 (nth v2 1)))

;; Test 3.2: Replace with string
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 "hello"))
  (test-eq "replace with string" "hello" (nth v2 1)))

;; Test 3.3: Replace with float
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 3.14))
  (test-eq "replace with float" 3.14 (nth v2 1)))

;; Test 3.4: Replace with symbol
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 'symbol))
  (test-eq "replace with symbol" 'symbol (nth v2 1)))

;; Test 3.5: Replace with boolean
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 true))
  (test-eq "replace with boolean" true (nth v2 1)))

;; Test 3.6: Replace with nothing
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 nothing))
  (test-eq "replace with nothing" nothing (nth v2 1)))

;; ============================================================
;; Test 4: Single Element Vector
;; ============================================================

(print "")
(print "=== Test 4: Single Element Vector ===")

;; Test 4.1: Replace only element
(do
  (define v (vector 1))
  (define v2 (vector-assoc v 0 99))
  (test-eq "replace only element" 99 (nth v2 0))
  (test-eq "length unchanged" 1 (vector-count v2)))

;; ============================================================
;; Test 5: Large Vector Operations
;; ============================================================

(print "")
(print "=== Test 5: Large Vector Operations ===")

;; Test 5.1: Assoc at various positions in larger vector
(do
  (define v (vector 1 2 3 4 5 6 7 8 9 10))
  (define v2 (vector-assoc v 5 99))
  (test-eq "assoc at index 5" 99 (nth v2 5))
  (test-eq "element before unchanged" 5 (nth v2 4))
  (test-eq "element after unchanged" 7 (nth v2 6))
  (test-eq "length unchanged" 10 (vector-count v2)))

;; Test 5.2: Multiple assocs on large vector
(do
  (define v (vector 1 2 3 4 5 6 7 8 9 10))
  (define v2 (vector-assoc v 0 10))
  (define v3 (vector-assoc v2 5 55))
  (define v4 (vector-assoc v3 9 90))
  (test-eq "first change applied" 10 (nth v4 0))
  (test-eq "middle change applied" 55 (nth v4 5))
  (test-eq "last change applied" 90 (nth v4 9))
  (test-eq "other elements unchanged" 2 (nth v4 1)))

;; ============================================================
;; Test 6: Edge Cases and Special Values
;; ============================================================

(print "")
(print "=== Test 6: Edge Cases and Special Values ===")

;; Test 6.1: Assoc zero value
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 0))
  (test-eq "replace with zero" 0 (nth v2 1)))

;; Test 6.2: Assoc negative value
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 -42))
  (test-eq "replace with negative" -42 (nth v2 1)))

;; Test 6.3: Assoc empty string
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 ""))
  (test-eq "replace with empty string" "" (nth v2 1)))

;; Test 6.4: Assoc nil
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 nil))
  (test-eq "replace with nil" nil (nth v2 1)))

;; ============================================================
;; Test 7: Chain Multiple Assocs
;; ============================================================

(print "")
(print "=== Test 7: Chain Multiple Assocs ===")

;; Test 7.1: Chain assocs to update multiple positions
(do
  (define v (vector 0 0 0 0 0))
  (define v2 (vector-assoc (vector-assoc (vector-assoc v 0 1) 2 2) 4 3))
  (test-eq "first assoc applied" 1 (nth v2 0))
  (test-eq "second assoc applied" 2 (nth v2 2))
  (test-eq "third assoc applied" 3 (nth v2 4))
  (test-eq "untouched positions unchanged" 0 (nth v2 1)))

;; ============================================================
;; Test 8: Structural Sharing Behavior
;; ============================================================

(print "")
(print "=== Test 8: Structural Sharing Behavior ===")

;; Test 8.1: Verify assoc creates new vector, not reference
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 99))
  (test-eq "original has original value" 2 (nth v 1))
  (test-eq "new vector has new value" 99 (nth v2 1))
  (test-eq "both have same length" 3 (vector-count v))
  (test-eq "new has same length" 3 (vector-count v2)))

;; ============================================================
;; Test 9: Overwriting Different Types
;; ============================================================

(print "")
(print "=== Test 9: Overwriting Different Types ===")

;; Test 9.1: Overwrite integer with string
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 "replaced"))
  (test-eq "integer replaced with string" "replaced" (nth v2 1)))

;; Test 9.2: Overwrite string with integer
(do
  (define v (vector "a" "b" "c"))
  (define v2 (vector-assoc v 1 99))
  (test-eq "string replaced with integer" 99 (nth v2 1)))

;; Test 9.3: Overwrite with different type each time
(do
  (define v (vector 1 2 3))
  (define v2 (vector-assoc v 1 "string"))
  (define v3 (vector-assoc v2 1 3.14))
  (define v4 (vector-assoc v3 1 'symbol))
  (test-eq "final value is symbol" 'symbol (nth v4 1)))

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
