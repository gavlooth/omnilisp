;; test_keywords.lisp - Tests for keyword functions from immer.omni library
;;
;; Tests:
;;   - keyword: Create a keyword from a string
;;   - keyword?: Check if a value is a keyword
;;
;; Run with: ./omni tests/test_keywords.lisp

;; ============================================================
;; Test Framework
;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

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

(define test-false [name] [actual]
  (set! test-count (+ test-count 1))
  (if (not actual)
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name)
        (print "  Expected: false")
        (print "  Got: true"))))

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
;; Test 1: keyword function - create keywords
;; ============================================================

(print "")
(print "=== Test 1: keyword Function ===")

;; Test 1.1: Create simple keyword
(do
  (define k (keyword "name"))
  (print "Created keyword:" k)
  (test-true "keyword returns symbol" (symbol? k)))

;; Test 1.2: Create keyword with underscore
(do
  (define k (keyword "first_name"))
  (test-true "keyword with underscore" (symbol? k)))

;; Test 1.3: Create keyword from multi-character string
(do
  (define k (keyword "id"))
  (test-true "keyword from 'id'" (symbol? k)))

;; Test 1.4: Create keyword with numbers in name
(do
  (define k (keyword "item1"))
  (test-true "keyword with number in name" (symbol? k)))

;; ============================================================
;; Test 2: keyword? predicate - identify keywords
;; ============================================================

(print "")
(print "=== Test 2: keyword? Predicate ===")

;; Test 2.1: Check keyword created with keyword function
(do
  (define k (keyword "name"))
  (test-true "keyword? on created keyword" (keyword? k)))

;; Test 2.2: Check colon-quoted symbol
(do
  (define k :name)
  (test-true "keyword? on colon-quoted :name" (keyword? k)))

;; Test 2.3: Check different colon-quoted symbols
(do
  (test-true "keyword? on :id" (keyword? :id))
  (test-true "keyword? on :user" (keyword? :user))
  (test-true "keyword? on :age" (keyword? :age)))

;; Test 2.4: Regular symbol should not be keyword
(do
  (define sym 'name)
  (test-false "keyword? on regular symbol" (keyword? sym)))

;; Test 2.5: String should not be keyword
(do
  (test-false "keyword? on string" (keyword? "name")))

;; Test 2.6: Number should not be keyword
(do
  (test-false "keyword? on number" (keyword? 42)))

;; Test 2.7: nil should not be keyword
(do
  (test-false "keyword? on nil" (keyword? nil)))

;; ============================================================
;; Test 3: keyword string representation
;; ============================================================

(print "")
(print "=== Test 3: Keyword String Representation ===")

;; Test 3.1: Verify keyword starts with colon
(do
  (define k (keyword "test"))
  (define s (symbol->string k))
  (test-true "keyword string starts with colon" (and (> (string-length s) 0) (eq? (string-ref s 0) #\:))))

;; Test 3.2: Verify keyword contains original name
(do
  (define k (keyword "mykey"))
  (define s (symbol->string k))
  (print "Keyword as string:" s)
  (test-true "keyword contains original name" (string-contains? s "mykey")))

;; ============================================================
;; Test 4: Keyword equality
;; ============================================================

(print "")
(print "=== Test 4: Keyword Equality ===")

;; Test 4.1: Keywords created from same string are equal
(do
  (define k1 (keyword "name"))
  (define k2 (keyword "name"))
  (test-eq "equal keywords from same string" k1 k2))

;; Test 4.2: Keyword created vs colon-quoted
(do
  (define k1 (keyword "user"))
  (define k2 :user)
  (test-eq "created keyword equals colon-quoted" k1 k2))

;; Test 4.3: Different keywords are not equal
(do
  (define k1 (keyword "name"))
  (define k2 (keyword "age"))
  (test-false "different keywords not equal" (= k1 k2)))

;; ============================================================
;; Test 5: Edge cases
;; ============================================================

(print "")
(print "=== Test 5: Edge Cases ===")

;; Test 5.1: Empty keyword name
(do
  (define k (keyword ""))
  (test-true "keyword from empty string" (symbol? k)))

;; Test 5.2: Keyword with special characters (allowed ones)
(do
  (define k (keyword "my-key"))
  (test-true "keyword with hyphen" (keyword? k)))

;; Test 5.3: Keyword with uppercase
(do
  (define k (keyword "Name"))
  (test-true "keyword with uppercase letters" (keyword? k)))

;; Test 5.4: Keyword with multiple words connected by underscore
(do
  (define k (keyword "first_last_name"))
  (test-true "keyword with multiple underscores" (keyword? k)))

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
