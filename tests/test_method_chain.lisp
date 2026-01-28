;; test_method_chain.lisp - Tests for method-chain primitive
;;
;; Tests prim_method_chain function which chains method calls in an
;; object-oriented style. This enables compact syntax for chaining
;; multiple function calls where each call receives result of the
;; previous one as its first argument.
;;
;; Run with: ./omni tests/test_method_chain.lisp

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
;; Helper Functions
;; ============================================================

;; Using single-argument helper functions that compiler handles well
(define add-two [x]
  (+ x 2))

(define mul-three [x]
  (* x 3))

(define sub-one [x]
  (- x 1))

(define double [x]
  (* x 2))

;; ============================================================
;; Test 1: Single method call (basic functionality)
;; ============================================================

(print "")
(print "=== Test 1: Single Method Call ===")

;; method-chain with one call should work with single-arg function
(define result1 (method-chain 10 [add-two]))
(test-eq "single method call - add-two" 12 result1)

(define result2 (method-chain 10 [mul-three]))
(test-eq "single method call - multiply-by-three" 30 result2)

;; ============================================================
;; Test 2: Chain multiple arithmetic operations
;; ============================================================

(print "")
(print "=== Test 2: Arithmetic Chains ===")

;; Chain: start with 10, add-two, then mul-three
;; Should be: mul-three (add-two 10) = mul-three 12 = 36
(define result3 (method-chain 10 [add-two mul-three]))
(test-eq "chain add-two then mul-three" 36 result3)

;; Chain: start with 20, sub-one, then sub-one
;; Should be: sub-one (sub-one 20) = sub-one 19 = 18
(define result4 (method-chain 20 [sub-one sub-one]))
(test-eq "chain subtract operations" 18 result4)

;; Chain: 5 -> double -> double -> double
;; Should be: double (double (double 5)) = double (double 10) = double 20 = 40
(define result5 (method-chain 5 [double double double]))
(test-eq "three double operations" 40 result5)

;; ============================================================
;; Test 3: Chain with mixed operations
;; ============================================================

(print "")
(print "=== Test 3: Mixed Operations ===")

;; Chain arithmetic operations
;; 5 -> double -> sub-one -> mul-three
;; Should be: mul-three (sub-one (double 5)) = mul-three (sub-one 10) = mul-three 9 = 27
(define result6 (method-chain 5 [double sub-one mul-three]))
(test-eq "mixed arithmetic operations" 27 result6)

;; ============================================================
;; Test 4: Empty method chain (edge case)
;; ============================================================

(print "")
(print "=== Test 4: Edge Cases ===")

;; Empty chain should return original object unchanged
(define result7 (method-chain 42 []))
(test-eq "empty chain returns original value" 42 result7)

;; ============================================================
;; Test 5: Verify result propagation
;; ============================================================

(print "")
(print "=== Test 5: Result Propagation ===")

;; Chain that requires each step to properly receive previous result
;; Start: 1
;; Step 1: add-two -> 3
;; Step 2: mul-three -> 9
;; Step 3: double -> 18
(define result8 (method-chain 1 [add-two mul-three double]))
(test-eq "verify result propagation through chain" 18 result8)

;; ============================================================
;; Test 6: Identity-like operations
;; ============================================================

(print "")
(print "=== Test 6: Identity and No-op ===")

;; Chain where operations multiply
;; 10 -> double -> mul-three -> double
;; (double: 20, mul-three: 60, double: 120)
(define result9 (method-chain 10 [double mul-three double]))
(test-eq "chain of multiplicative operations" 120 result9)

;; ============================================================
;; Test 7: Nested method chain
;; ============================================================

(print "")
(print "=== Test 7: Nested Method Chains ===")

;; Use result of one chain as input to another
(define inner-result (method-chain 5 [double]))
(define result10 (method-chain inner-result [mul-three]))
(test-eq "nested method chains" 30 result10)

;; ============================================================
;; Test 8: Chain with zero
;; ============================================================

(print "")
(print "=== Test 8: Arithmetic with Zero ===")

;; Chain operations with zero
;; 0 -> double -> add-two -> mul-three
;; (double: 0, add-two: 2, mul-three: 6)
(define result11 (method-chain 0 [double add-two mul-three]))
(test-eq "chain from zero" 6 result11)

;; ============================================================
;; Test 9: Large value chains
;; ============================================================

(print "")
(print "=== Test 9: Large Values ===")

;; Chain starting with large value
;; 100 -> double -> sub-one -> mul-three
;; (double: 200, sub-one: 199, mul-three: 597)
(define result12 (method-chain 100 [double sub-one mul-three]))
(test-eq "chain with large values" 597 result12)

;; ============================================================
;; Test 10: Four-step chain
;; ============================================================

(print "")
(print "=== Test 10: Four-Step Chain ===")

;; Chain with four operations
;; 2 -> add-two -> double -> add-two -> mul-three
;; (add-two: 4, double: 8, add-two: 10, mul-three: 30)
(define result13 (method-chain 2 [add-two double add-two mul-three]))
(test-eq "four-step chain" 30 result13)

;; ============================================================
;; Test 11: Negative numbers in chain
;; ============================================================

(print "")
(print "=== Test 11: Negative Numbers ===")

;; Chain with negative starting value
;; -10 -> double -> add-two -> mul-three
;; (double: -20, add-two: -18, mul-three: -54)
(define result14 (method-chain -10 [double add-two mul-three]))
(test-eq "chain with negative numbers" -54 result14)

;; ============================================================
;; Test 12: Single operation repeated
;; ============================================================

(print "")
(print "=== Test 12: Repeated Operations ===")

;; Chain same operation multiple times
;; 3 -> double -> double -> double -> double
;; (double: 6, double: 12, double: 24, double: 48)
(define result15 (method-chain 3 [double double double double]))
(test-eq "chain with repeated double" 48 result15)

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
