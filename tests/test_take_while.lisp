;; test_take_while.lisp - Tests for take-while collection primitive
;;
;; Tests take-while function which takes elements from the beginning
;; of a collection while a predicate function returns true:
;;   - take-while pred coll: Returns leading elements that satisfy pred
;;
;; This function is critical for:
;;   - Lazy sequence processing
;;   - Filtering from the beginning
;;   - Stopping conditions based on element values
;;
;; Run with: ./omni tests/test_take_while.lisp

;; ============================================================
;; Test Framework
;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define test-list-equal [name] [expected] [actual]
  (set! test-count (+ test-count 1))
  (if (list-equal expected actual)
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name)
        (print "  Expected list:" expected)
        (print "  Got:" actual))))

(define test-array-length [name] [expected-length] [actual-arr]
  (set! test-count (+ test-count 1))
  (if (= expected-length (array-length actual-arr))
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name)
        (print "  Expected length:" expected-length)
        (print "  Got:" (array-length actual-arr)))))

;; ============================================================
;; take-while Tests - Arrays
;; ============================================================

(print "")
(print "=== take-while Tests (Arrays) ===")

;; Test 1: Basic take-while with positive numbers
(let ([result (take-while (fn [x] (> x 0)) [1 2 3 -1 4 5])])
  (test-array-length "take-while positive" 3 result)
  (if (= (array-length result) 3)
      (do
        (print "  Values:" (array-ref result 0) (array-ref result 1) (array-ref result 2)))))

;; Test 2: Take-while with even numbers
(let ([result (take-while (fn [x] (= (rem x 2) 0)) [2 4 6 7 8 10])])
  (test-array-length "take-while even" 3 result))

;; Test 3: Take-while stops at first failure
(let ([result (take-while (fn [x] (< x 10)) [1 2 3 10 4 5])])
  (test-array-length "take-while < 10 stops at 10" 3 result))

;; Test 4: Take-while on all elements satisfying predicate
(let ([result (take-while (fn [x] (> x 0)) [1 2 3 4 5])])
  (test-array-length "take-while all satisfy" 5 result))

;; Test 5: Take-while on empty array
(let ([result (take-while (fn [x] true) [])])
  (test-array-length "take-while empty array" 0 result))

;; Test 6: Take-where first element fails
(let ([result (take-while (fn [x] (> x 100)) [1 2 3 4 5])])
  (test-array-length "take-where first fails" 0 result))

;; Test 7: Take-while with strings
(let ([result (take-while (fn [s] (string-contains s "a")) ["apple" "apricot" "banana" "cherry"])])
  (test-array-length "take-while strings with 'a'" 3 result))

;; Test 8: Take-while with less-than predicate
(let ([result (take-while (fn [x] (< x 5)) [1 2 3 4 5 6 7])])
  (test-array-length "take-while < 5" 4 result))

;; ============================================================
;; take-while Tests - Lists
;; ============================================================

(print "")
(print "=== take-while Tests (Lists) ===")

;; Test 9: Basic take-while from list
(let ([result (take-while (fn [x] (> x 0)) '(1 2 3 -1 4 5))])
  (test-list-equal "take-while from list" '(1 2 3) result))

;; Test 10: Take-while with odd numbers
(let ([result (take-while (fn [x] (= (rem x 2) 1)) '(1 3 5 2 7 9))])
  (test-list-equal "take-while odd" '(1 3 5) result))

;; Test 11: Take-where all satisfy on list
(let ([result (take-while (fn [x] (< x 10)) '(1 2 3 4 5))])
  (test-list-equal "take-where all satisfy list" '(1 2 3 4 5) result))

;; Test 12: Take-while from empty list
(let ([result (take-while (fn [x] true) '())])
  (test-list-equal "take-while empty list" '() result))

;; Test 13: Take-where first element fails on list
(let ([result (take-while (fn [x] (< 0 x)) '(-1 -2 -3))])
  (test-list-equal "take-where first fails list" '() result))

;; Test 14: Take-while with complex predicate
(let ([result (take-while (fn [x] (and (> x 0) (< x 5))) '(1 2 3 4 5 6 7))])
  (test-list-equal "take-while and predicate" '(1 2 3 4) result))

;; ============================================================
;; Edge Cases and Complex Scenarios
;; ============================================================

(print "")
(print "=== Edge Cases and Complex Scenarios ===")

;; Test 15: Take-while with negation
(let ([result (take-while (fn [x] (not (= x 0))) [1 2 3 0 4 5])])
  (test-array-length "take-while not zero" 3 result))

;; Test 16: Take-where single element matches
(let ([result (take-while (fn [x] (= x 1)) [1 2 3 4])])
  (test-array-length "take-where only first matches" 1 result))

;; Test 17: Composition - map then take-while
(let ([result (take-while (fn [x] (< x 20)) (map (fn [x] (* x x)) [1 2 3 4 5])])])
  (test-array-length "map then take-while" 4 result))

;; Test 18: Take-while with boolean values
(let ([result (take-while (fn [x] x) [true true false true true])])
  (test-array-length "take-while booleans" 2 result))

;; Test 19: Take-while preserves order
(let ([result (take-while (fn [x] (< x 5)) [1 2 3 4 5 6])])
  (if (= (array-length result) 4)
      (do
        (test-array-length "take-while preserves order" 4 result)
        (if (and (= (array-ref result 0) 1)
                 (= (array-ref result 1) 2)
                 (= (array-ref result 2) 3)
                 (= (array-ref result 3) 4))
            (print "PASS: order preserved correctly")
            (do
              (set! fail-count (+ fail-count 1))
              (print "FAIL: order not preserved"))))))

;; Test 20: Take-while with equality check
(let ([result (take-while (fn [x] (= x "test")) ["test" "test" "test" "other" "test"])])
  (test-array-length "take-while string equality" 3 result))

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
