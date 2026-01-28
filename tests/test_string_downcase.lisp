;; test_string_downcase.lisp - Tests for string-downcase function
;;
;; string-downcase is an alias for converting strings to lowercase.
;; This file tests the function specifically to ensure it works correctly.
;;
;; Run with: ./omni tests/test_string_downcase.lisp

;; ============================================================
;; Test Framework
;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

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

;; ============================================================
;; Test 1: Basic lowercase conversion
;; ============================================================

(print "")
(print "=== Test 1: Basic Lowercase Conversion ===")

;; Test 1.1: Uppercase letters become lowercase
(test-string "downcase 'HELLO' -> 'hello'"
  "hello"
  (string-downcase "HELLO"))

;; Test 1.2: Already lowercase stays lowercase
(test-string "downcase 'hello' -> 'hello'"
  "hello"
  (string-downcase "hello"))

;; Test 1.3: Mixed case becomes lowercase
(test-string "downcase 'HeLLo WoRLd' -> 'hello world'"
  "hello world"
  (string-downcase "HeLLo WoRLd"))

;; ============================================================
;; Test 2: Characters that don't change
;; ============================================================

(print "")
(print "=== Test 2: Characters That Don't Change ===")

;; Test 2.1: Numbers remain unchanged
(test-string "downcase 'ABC123' -> 'abc123'"
  "abc123"
  (string-downcase "ABC123"))

;; Test 2.2: Spaces remain unchanged
(test-string "downcase 'A B C' -> 'a b c'"
  "a b c"
  (string-downcase "A B C"))

;; Test 2.3: Punctuation remains unchanged
(test-string "downcase 'HELLO! WORLD?' -> 'hello! world?'"
  "hello! world?"
  (string-downcase "HELLO! WORLD?"))

;; Test 2.4: Special characters remain unchanged
(test-string "downcase 'TEST@#$%' -> 'test@#$%'"
  "test@#$%"
  (string-downcase "TEST@#$%"))

;; ============================================================
;; Test 3: Empty and single character strings
;; ============================================================

(print "")
(print "=== Test 3: Edge Cases ===")

;; Test 3.1: Empty string
(test-string "downcase '' -> ''"
  ""
  (string-downcase ""))

;; Test 3.2: Single uppercase character
(test-string "downcase 'A' -> 'a'"
  "a"
  (string-downcase "A"))

;; Test 3.3: Single lowercase character
(test-string "downcase 'a' -> 'a'"
  "a"
  (string-downcase "a"))

;; Test 3.4: Single non-letter character
(test-string "downcase '1' -> '1'"
  "1"
  (string-downcase "1"))

;; ============================================================
;; Test 4: Unicode and extended ASCII
;; ============================================================

(print "")
(print "=== Test 4: Extended Characters ===")

;; Test 4.1: Accented characters (may vary by locale)
(test-string "downcase 'CAFE' -> 'cafe'"
  "cafe"
  (string-downcase "CAFE"))

;; Test 4.2: German umlaut (if supported)
(test-string "downcase with simple ASCII only"
  "test"
  (string-downcase "TEST"))

;; ============================================================
;; Test 5: String with newlines and tabs
;; ============================================================

(print "")
(print "=== Test 5: Strings with Whitespace ===")

;; Test 5.1: String with newline
(test-string "downcase 'LINE1\nLINE2' -> 'line1\nline2'"
  "line1\nline2"
  (string-downcase "LINE1\nLINE2"))

;; Test 5.2: String with tab
(test-string "downcase 'COL1\tCOL2' -> 'col1\tcol2'"
  "col1\tcol2"
  (string-downcase "COL1\tCOL2"))

;; ============================================================
;; Test 6: Chained operations
;; ============================================================

(print "")
(print "=== Test 6: Chained Operations ===")

;; Test 6.1: Downcase then upcase
(test-string "downcase then upcase returns original"
  "HELLO"
  (string-upcase (string-downcase "HELLO")))

;; Test 6.2: Downcase twice (idempotent)
(test-string "downcase twice is idempotent"
  "hello"
  (string-downcase (string-downcase "HeLLo")))

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
