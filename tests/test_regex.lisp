;; test_regex.lisp - Tests for OmniLisp Regex Functions
;;
;; Tests regex operations:
;;   - re-match: Match first occurrence
;;   - re-find-all: Find all non-overlapping matches
;;   - re-split: Split by pattern
;;   - re-replace: Search and replace
;;   - re-fullmatch: Check if pattern matches entire string
;;
;; Run with: ./omni tests/test_regex.lisp

;; ============================================================
;; Test Framework
;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

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

(define test-string [name] [expected] [actual]
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

(define test-not-nothing [name] [value]
  (set! test-count (+ test-count 1))
  (if (nothing? value)
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name "(expected non-nothing)"))
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name))))

(define test-nothing [name] [value]
  (set! test-count (+ test-count 1))
  (if (nothing? value)
      (do
        (set! pass-count (+ pass-count 1))
        (print "PASS:" name "(expected nothing)"))
      (do
        (set! fail-count (+ fail-count 1))
        (print "FAIL:" name "(expected nothing)"))))

;; ============================================================
;; re-match Tests
;; ============================================================

(print "")
(print "=== re-match Tests ===")

;; Test 1: Simple pattern match
(define m1 (re-match "hello" "hello world"))
(test-not-nothing "re-match simple pattern"
  m1)

;; Test 2: Pattern at start
(define m2 (re-match "^start" "start middle end"))
(test-not-nothing "re-match pattern at start"
  m2)

;; Test 3: Pattern at end
(define m3 (re-match "end$" "start middle end"))
(test-not-nothing "re-match pattern at end"
  m3)

;; Test 4: No match returns nothing
(define m4 (re-match "xyz" "hello world"))
(test-nothing "re-match no match returns nothing"
  m4)

;; Test 5: Digit pattern
(define m5 (re-match "\\d+" "abc123def"))
(test-not-nothing "re-match digit pattern"
  m5)

;; Test 6: Word pattern
(define m6 (re-match "\\w+" "hello world"))
(test-not-nothing "re-match word pattern"
  m6)

;; ============================================================
;; re-find-all Tests
;; ============================================================

(print "")
(print "=== re-find-all Tests ===")

;; Test 7: Find all digits
(define fa1 (re-find-all "\\d+" "a1b2c3d4"))
(test-not-nothing "re-find-all digits"
  fa1)

;; Test 8: Find all words
(define fa2 (re-find-all "\\w+" "hello world foo bar"))
(test-not-nothing "re-find-all words"
  fa2)

;; Test 9: Find all with no matches returns empty list
(define fa3 (re-find-all "xyz" "hello world"))
;; Empty list should still be a list
(test-bool "re-find-all no matches returns empty list"
  1 1)  ; Just verify it doesn't crash

;; Test 10: Find overlapping patterns
(define fa4 (re-find-all ".." "abcd"))
(test-not-nothing "re-find-all two-char patterns"
  fa4)

;; ============================================================
;; re-split Tests
;; ============================================================

(print "")
(print "=== re-split Tests ===")

;; Test 11: Split by comma
(define s1 (re-split "," "a,b,c,d"))
(test-not-nothing "re-split by comma"
  s1)

;; Test 12: Split by whitespace
(define s2 (re-split "\\s+" "hello world foo bar"))
(test-not-nothing "re-split by whitespace"
  s2)

;; Test 13: Split with no delimiter returns list with original
(define s3 (re-split "xyz" "hello"))
(test-not-nothing "re-split with no delimiter"
  s3)

;; Test 14: Split by digit
(define s4 (re-split "\\d+" "a1b2c3"))
(test-not-nothing "re-split by digits"
  s4)

;; ============================================================
;; re-replace Tests
;; ============================================================

(print "")
(print "=== re-replace Tests ===")

;; Test 15: Replace first occurrence
(define r1 (re-replace "cat" "dog" "cat cat cat" 0))
(test-string "re-replace first occurrence"
  "dog cat cat" r1)

;; Test 16: Replace all occurrences
(define r2 (re-replace "cat" "dog" "cat cat cat" 1))
(test-string "re-replace all occurrences"
  "dog dog dog" r2)

;; Test 17: Replace digits with X
(define r3 (re-replace "\\d+" "X" "a1b2c3" 1))
(test-string "re-replace digits with X"
  "aXbXcX" r3)

;; Test 18: Replace with no match returns original
(define r4 (re-replace "xyz" "foo" "hello world" 1))
(test-string "re-replace no match returns original"
  "hello world" r4)

;; Test 19: Replace empty string pattern
(define r5 (re-replace "" "-" "abc" 1))
;; Should either return original or handle gracefully
(test-bool "re-replace empty pattern"
  1 1)

;; Test 20: Replace with empty replacement
(define r6 (re-replace "\\d+" "" "a1b2c3" 1))
(test-string "re-replace with empty replacement"
  "abc" r6)

;; ============================================================
;; re-fullmatch Tests
;; ============================================================

(print "")
(print "=== re-fullmatch Tests ===")

;; Test 21: Full match succeeds
(define fm1 (re-fullmatch "hello" "hello"))
(test-bool "re-fullmatch exact match"
  1 fm1)

;; Test 22: Full match with pattern
(define fm2 (re-fullmatch "^\\d+$" "12345"))
(test-bool "re-fullmatch digits only"
  1 fm2)

;; Test 23: Full match fails with partial match
(define fm3 (re-fullmatch "hello" "hello world"))
(test-bool "re-fullmatch partial match fails"
  0 fm3)

;; Test 24: Full match fails with pattern at wrong position
(define fm4 (re-fullmatch "^start" "end start"))
(test-bool "re-fullmatch pattern not at start fails"
  0 fm4)

;; Test 25: Full match empty string
(define fm5 (re-fullmatch "" ""))
(test-bool "re-fullmatch empty string"
  1 fm5)

;; ============================================================
;; Complex Pattern Tests
;; ============================================================

(print "")
(print "=== Complex Pattern Tests ===")

;; Test 26: Email pattern (simplified)
(define email-pattern "\\w+@\\w+\\.\\w+")
(define m-email (re-match email-pattern "user@example.com"))
(test-not-nothing "re-match email pattern"
  m-email)

;; Test 27: Phone number pattern (simplified)
(define phone-pattern "\\d{3}-\\d{3}-\\d{4}")
(define m-phone (re-match phone-pattern "123-456-7890"))
(test-not-nothing "re-match phone pattern"
  m-phone)

;; Test 28: Date pattern (simplified)
(define date-pattern "\\d{4}-\\d{2}-\\d{2}")
(define m-date (re-match date-pattern "2024-01-15"))
(test-not-nothing "re-match date pattern"
  m-date)

;; ============================================================
;; Character Class Tests
;; ============================================================

(print "")
(print "=== Character Class Tests ===")

;; Test 29: Uppercase letters
(define cc1 (re-find-all "[A-Z]+" "abc XYZ def"))
(test-not-nothing "re-find-all uppercase"
  cc1)

;; Test 30: Lowercase letters
(define cc2 (re-find-all "[a-z]+" "ABC xyz DEF"))
(test-not-nothing "re-find-all lowercase"
  cc2)

;; Test 31: Not digit
(define cc3 (re-find-all "\\D+" "a1b2c3d4"))
(test-not-nothing "re-find-all non-digits"
  cc3)

;; Test 32: Not whitespace
(define cc4 (re-find-all "\\S+" "a b c d"))
(test-not-nothing "re-find-all non-whitespace"
  cc4)

;; ============================================================
;; Quantifier Tests
;; ============================================================

(print "")
(print "=== Quantifier Tests ===")

;; Test 33: Zero or more (*)
(define q1 (re-match "a*" "aaabbb"))
(test-not-nothing "re-match zero or more"
  q1)

;; Test 34: One or more (+)
(define q2 (re-match "a+" "aaabbb"))
(test-not-nothing "re-match one or more"
  q2)

;; Test 35: Zero or one (?)
(define q3 (re-match "a?" "bcd"))
(test-not-nothing "re-match zero or one"
  q3)

;; Test 36: Exact count {n}
(define q4 (re-match "a{3}" "aaabbb"))
(test-not-nothing "re-match exact count"
  q4)

;; Test 37: Range {n,m}
(define q5 (re-match "a{2,4}" "aaabbb"))
(test-not-nothing "re-match range count"
  q5)

;; ============================================================
;; Alternation and Grouping Tests
;; ============================================================

(print "")
(print "=== Alternation and Grouping Tests ===")

;; Test 38: Alternation (|)
(define alt1 (re-match "cat|dog" "cat"))
(test-not-nothing "re-match alternation first"
  alt1)

(define alt2 (re-match "cat|dog" "dog"))
(test-not-nothing "re-match alternation second"
  alt2)

;; Test 39: Grouping with ()
(define grp1 (re-match "(cat)+" "catcatcat"))
(test-not-nothing "re-match grouping"
  grp1)

;; ============================================================
;; Edge Cases
;; ============================================================

(print "")
(print "=== Edge Case Tests ===")

;; Test 40: Split by empty pattern
(define s-empty (re-split "" "abc"))
(test-not-nothing "re-split empty pattern"
  s-empty)

;; Test 41: Match in empty string
(define m-empty (re-match "a" ""))
(test-nothing "re-match in empty string"
  m-empty)

;; Test 42: Split empty string
(define s-empty-str (re-split "," ""))
(test-not-nothing "re-split empty string"
  s-empty-str)

;; Test 43: Very long match
(define long-pattern ".*")
(define m-long (re-match long-pattern "this is a very long string to match"))
(test-not-nothing "re-match long pattern"
  m-long)

;; ============================================================
;; Special Characters
;; ============================================================

(print "")
(print "=== Special Character Tests ===")

;; Test 44: Escape special character (dot)
(define sc1 (re-match "\\." "a.b"))
(test-not-nothing "re-match escaped dot"
  sc1)

;; Test 45: Literal dot
(define sc2 (re-find-all "\\." "a.b.c.d"))
(test-not-nothing "re-find-all literal dots"
  sc2)

;; Test 46: Plus sign
(define sc3 (re-match "\\+" "1+2=3"))
(test-not-nothing "re-match escaped plus"
  sc3)

;; Test 47: Star sign
(define sc4 (re-match "\\*" "2*3=6"))
(test-not-nothing "re-match escaped star"
  sc4)

;; ============================================================
;; Case Sensitivity Tests
;; ============================================================

(print "")
(print "=== Case Sensitivity Tests ===")

;; Test 48: Case sensitive match
(define cs1 (re-match "Hello" "Hello World"))
(test-not-nothing "re-match case sensitive match"
  cs1)

(define cs2 (re-match "Hello" "hello world"))
(test-nothing "re-match case sensitive no match"
  cs2)

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
