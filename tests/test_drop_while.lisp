;; test_drop_while.lisp - Tests for drop-while primitive
;;
;; Tests prim_drop_while function which drops elements from collection
;; while predicate function returns true. Stops at first false result.
;; Supports both arrays and lists as input.
;; Returns array for array input, list for list input.

;; ============================================================
;; Test Framework
;; ============================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-eq (name expected actual)
  (setq *test-count* (+ *test-count* 1))
  (if (equal expected actual)
      (progn
        (setq *pass-count* (+ *pass-count* 1))
        (format t "PASS: ~a~%" name))
      (progn
        (setq *fail-count* (+ *fail-count* 1))
        (format t "FAIL: ~a~%" name)
        (format t "  Expected: ~a~%" expected)
        (format t "  Got: ~a~%" actual))))

;; ============================================================
;; Basic drop-while Tests - Arrays
;; ============================================================

(format t "~%=== Basic drop-while Tests - Arrays ===~%")

;; Test 1: drop-while with positive predicate
(test-eq "drop-while positive numbers"
  '(4 5 6)
  (drop-while (lambda (x) (< x 4)) (array 1 2 3 4 5 6)))

;; Test 2: drop-while with all elements matching
(test-eq "drop-while all match"
  '()
  (drop-while (lambda (x) (> x 0)) (array 1 2 3 4)))

;; Test 3: drop-while with no elements matching
(test-eq "drop-while none match"
  '(1 2 3 4 5)
  (drop-while (lambda (x) (< x 0)) (array 1 2 3 4 5)))

;; Test 4: drop-while from empty array
(test-eq "drop-while empty array"
  '()
  (drop-while (lambda (x) t) (array)))

;; ============================================================
;; drop-while Tests - Lists
;; ============================================================

(format t "~%=== drop-while Tests - Lists ===~%")

;; Test 5: drop-while from list
(test-eq "drop-while list"
  '(c d e)
  (drop-while (lambda (x) (or (equal x 'a) (equal x 'b))) '(a b c d e)))

;; Test 6: drop-while with even predicate
(test-eq "drop-while even numbers"
  '(3 5 7)
  (drop-while (lambda (x) (= (% x 2) 0)) '(2 4 6 3 5 7)))

;; Test 7: drop-while with string prefix
(test-eq "drop-while string prefix"
  '(banana cherry date)
  (drop-while (lambda (x) (equal (substring x 0 1) "a")) '(apple apricot banana cherry date)))

;; ============================================================
;; Complex Predicates
;; ============================================================

(format t "~%=== Complex Predicates ===~%")

;; Test 8: drop-while with complex predicate
(test-eq "drop-while complex condition"
  '(50 60 70)
  (drop-while (lambda (x) (and (> x 0) (< x 50))) (array 10 20 30 40 50 60 70)))

;; Test 9: drop-while checking type
(test-eq "drop-while by type"
  '("hello" 42 3.14)
  (drop-while (lambda (x) (numberp x)) (array 1 2 3 "hello" 42 3.14)))

;; Test 10: drop-while with length predicate
(test-eq "drop-while by length"
  '("hello" "world")
  (drop-while (lambda (x) (< (length x) 4)) (array "hi" "hey" "hello" "world")))

;; ============================================================
;; Edge Cases
;; ============================================================

(format t "~%=== Edge Cases ===~%")

;; Test 11: drop-while with nil/false predicate
(test-eq "drop-while nil predicate"
  '(1 2 3)
  (drop-while nil '(1 2 3)))

;; Test 12: drop-while with always true predicate
(test-eq "drop-while always true"
  '()
  (drop-while t '(1 2 3)))

;; Test 13: drop-while with always false predicate
(test-eq "drop-while always false"
  '(1 2 3 4 5)
  (drop-while (lambda (x) nil) '(1 2 3 4 5)))

;; Test 14: drop-while single element matching
(test-eq "drop-while single match"
  '(2 3 4)
  (drop-while (lambda (x) (= x 1)) '(1 2 3 4)))

;; Test 15: drop-while single element not matching
(test-eq "drop-while single no match"
  '(1)
  (drop-while (lambda (x) (> x 1)) '(1)))

;; ============================================================
;; Boolean and nil handling
;; ============================================================

(format t "~%=== Boolean and nil Handling ===~%")

;; Test 16: drop-while with booleans
(test-eq "drop-while booleans"
  '(false false false)
  (drop-while (lambda (x) x) (array t t t nil nil)))

;; Test 17: drop-while with nil values
(test-eq "drop-while with nils"
  '(nil nil nil)
  (drop-while (lambda (x) (numberp x)) (array 1 2 3 nil nil)))

;; ============================================================
;; Large Collections
;; ============================================================

(format t "~%=== Large Collections ===~%")

;; Test 18: drop-while large array
(test-eq "drop-while large array"
  (collect-array (range 50 100))
  (drop-while (lambda (x) (< x 50)) (collect-array (range 100))))

;; Test 19: drop-while large list
(test-eq "drop-while large list"
  '(50 51 52 53 54)
  (drop-while (lambda (x) (< x 50)) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54)))

;; ============================================================
;; Results
;; ============================================================

(format t "~%=== Test Results ===~%")
(format t "Total: ~a~%" *test-count*)
(format t "Passed: ~a~%" *pass-count*)
(format t "Failed: ~a~%" *fail-count*)

(if (= *fail-count* 0)
    (format t "~%ALL TESTS PASSED!~%")
    (format t "~%SOME TESTS FAILED!~%"))

;; Return count of failures (0 = success)
*fail-count*
