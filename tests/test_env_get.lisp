;; test_env_get.lisp - Tests for env-get primitive
;;
;; Tests prim_env_get function which looks up a symbol in the current environment.
;; Run with: ./csrc/omnilisp tests/test_env_get.lisp

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
;; Basic env-get Tests
;; ============================================================

(print "")
(print "=== Basic env-get Tests ===")

;; Test 1: Get defined variable
(define x 42)
(test-eq "get defined integer variable" 42 (env-get 'x))

;; Test 2: Get string variable
(define greeting "hello")
(test-eq "get string variable" "hello" (env-get 'greeting))

;; Test 3: Get boolean variable
(define flag true)
(test-bool "get boolean variable" true (env-get 'flag))

;; ============================================================
;; Multiple Variables
;; ============================================================

(print "")
(print "=== Multiple Variables ===")

;; Test 4: Multiple independent variables
(define a 1)
(define b 2)
(define c 3)
(test-eq "get variable a" 1 (env-get 'a))
(test-eq "get variable b" 2 (env-get 'b))
(test-eq "get variable c" 3 (env-get 'c))

;; ============================================================
;; Shadowing Tests
;; ============================================================

(print "")
(print "=== Variable Shadowing ===")

;; Test 5: Variable shadowing
(define outer "outer-value")
(do
  (define outer "inner-value")
  (test-eq "inner variable shadows outer" "inner-value" (env-get 'outer)))
;; After block, should get outer
(test-eq "outer variable accessible after block" "outer-value" (env-get 'outer))

;; ============================================================
;; Nil/Nothing Tests
;; ============================================================

(print "")
(print "=== Nil/Nothing Handling ===")

;; Test 6: Get nil variable
(define nothing-var nothing)
(test-eq "get nothing variable" nothing (env-get 'nothing-var))

;; Test 7: Get zero value
(define zero 0)
(test-eq "get zero value" 0 (env-get 'zero))

;; ============================================================
;; Complex Types
;; ============================================================

(print "")
(print "=== Complex Types ===")

;; Test 8: Get list variable
(define lst '(1 2 3))
(test-eq "get list variable" '(1 2 3) (env-get 'lst))

;; Test 9: Get array variable
(define arr [1 2 3 4 5])
(test-eq "get array variable" [1 2 3 4 5] (env-get 'arr))

;; Test 10: Get dict variable
(define d (dict "key" "value"))
(test-eq "dict key exists" "value" (dict-get "key" (env-get 'd)))

;; Test 11: Get function variable
(define inc (lambda (x) (+ x 1)))
(test-eq "get function variable" 42 ((env-get 'inc) 41))

;; ============================================================
;; Rebinding Tests
;; ============================================================

(print "")
(print "=== Variable Rebinding ===")

;; Test 12: Rebind variable
(define counter 10)
(test-eq "counter initial value" 10 (env-get 'counter))
(define counter 20)
(test-eq "counter after rebinding" 20 (env-get 'counter))

;; ============================================================
;; Floating Point
;; ============================================================

(print "")
(print "=== Floating Point Values ===")

;; Test 13: Get float variable
(define pi 3.14159)
(test-eq "get float variable" 3.14159 (env-get 'pi))

;; Test 14: Get negative float
(define negative-float -2.5)
(test-eq "get negative float" -2.5 (env-get 'negative-float))

;; ============================================================
;; Special Values
;; ============================================================

(print "")
(print "=== Special Values ===")

;; Test 15: Get negative integer
(define neg-int -42)
(test-eq "get negative integer" -42 (env-get 'neg-int))

;; Test 16: Get large positive integer
(define large-int 999999)
(test-eq "get large integer" 999999 (env-get 'large-int))

;; ============================================================
;; Variable Name Patterns
;; ============================================================

(print "")
(print "=== Variable Name Patterns ===")

;; Test 17: Hyphenated variable name
(define my-var "hyphenated")
(test-eq "get hyphenated variable" "hyphenated" (env-get 'my-var))

;; Test 18: Variable with numbers
(define var2 "numbered")
(test-eq "get numbered variable" "numbered" (env-get 'var2))

;; Test 19: Variable with question mark
(define done? true)
(test-bool "get question-marked variable" true (env-get 'done?))

;; Test 20: Variable with exclamation mark
(define important! "high-priority")
(test-eq "get exclamation-marked variable" "high-priority" (env-get 'important!))

;; ============================================================
;; Nested Structures
;; ============================================================

(print "")
(print "=== Nested Structures ===")

;; Test 21: Get nested list
(define nested '((1 2) (3 4) (5 6)))
(test-eq "get nested list" '((1 2) (3 4) (5 6)) (env-get 'nested))

;; Test 22: Get array of arrays
(define matrix [[1 2] [3 4]])
(test-eq "get array of arrays" [[1 2] [3 4]] (env-get 'matrix))

;; ============================================================
;; Results
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
