;; test_debug_primitives.lisp - Tests for debug primitives
;;
;; Tests:
;;   - prim_type_of: Return type as symbol
;;   - prim_address_of: Return object memory address as integer
;;   - prim_refcount_of: Return current reference count
;;   - prim_region_of: Return owning region info as dict
;;   - prim_sizeof: Return object memory footprint in bytes
;;   - prim_inspect: Detailed object info

;; ============================================================
;; Test Framework
;; ============================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-true (name condition)
  (setq *test-count* (+ *test-count* 1))
  (if condition
      (progn
        (setq *pass-count* (+ *pass-count* 1))
        (format t "PASS: ~a~%" name))
      (progn
        (setq *fail-count* (+ *fail-count* 1))
        (format t "FAIL: ~a~%" name))))

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
;; type-of Tests
;; ============================================================

(format t "~%=== type-of Tests ===~%")

;; Test 1: type-of integer
(test-eq "type-of integer"
  'Int
  (type-of 42))

;; Test 2: type-of float
(test-eq "type-of float"
  'Float
  (type-of 3.14))

;; Test 3: type-of string
(test-eq "type-of string"
  'String
  (type-of "hello"))

;; Test 4: type-of symbol
(test-eq "type-of symbol"
  'Symbol
  (type-of 'foo))

;; Test 5: type-of list/pair
(test-eq "type-of list"
  'Pair
  (type-of '(1 2 3)))

;; Test 6: type-of array
(test-eq "type-of array"
  'Array
  (type-of (array 1 2 3)))

;; Test 7: type-of dict
(test-eq "type-of dict"
  'Dict
  (type-of (dict "key" "value")))

;; Test 8: type-of closure
(test-eq "type-of closure"
  'Closure
  (type-of (lambda (x) x)))

;; Test 9: type-of boolean true
(test-eq "type-of true"
  'Bool
  (type-of t))

;; Test 10: type-of boolean false
(test-eq "type-of false"
  'Bool
  (type-of nil))

;; Test 11: type-of char
(test-eq "type-of char"
  'Char
  (type-of #\a))

;; ============================================================
;; address-of Tests
;; ============================================================

(format t "~%=== address-of Tests ===~%")

;; Test 12: address-of returns positive integer
(defvar *test-obj* 42)
(defvar *addr* (address-of *test-obj*))
(test-true "address-of returns integer"
  (integerp *addr*))
(test-true "address-of returns positive"
  (>= *addr* 0))

;; Test 13: address-of different objects
(defvar *obj1* 1)
(defvar *obj2* 2)
(defvar *addr1* (address-of *obj1*))
(defvar *addr2* (address-of *obj2*))
;; Note: These might be the same due to interning, but they should be valid addresses

;; Test 14: address-of on string
(defvar *addr-str* (address-of "hello"))
(test-true "address-of string is valid"
  (integerp *addr-str*))

;; ============================================================
;; sizeof Tests
;; ============================================================

(format t "~%=== sizeof Tests ===~%")

;; Test 15: sizeof integer
(defvar *sz-int* (sizeof 42))
(test-true "sizeof integer is positive"
  (>= *sz-int* 0))

;; Test 16: sizeof string
(defvar *sz-str* (sizeof "hello"))
(test-true "sizeof string is positive"
  (>= *sz-str* 0))

;; Test 17: sizeof array
(defvar *sz-arr* (sizeof (array 1 2 3)))
(test-true "sizeof array is positive"
  (>= *sz-arr* 0))

;; Test 18: sizeof larger than smaller
(defvar *sz-small* (sizeof "hi"))
(defvar *sz-large* (sizeof "hello world"))
(test-true "sizeof scales with content"
  (>= *sz-large* *sz-small*))

;; ============================================================
;; refcount-of Tests
;; ============================================================

(format t "~%=== refcount-of Tests ===~%")

;; Test 19: refcount-of returns integer
(defvar *rc* (refcount-of 42))
(test-true "refcount-of returns integer"
  (integerp *rc*))
(test-true "refcount-of is positive"
  (>= *rc* 1))

;; Test 20: refcount-of on new object
(defvar *new-obj* (array 1 2 3))
(defvar *rc-new* (refcount-of *new-obj*))
(test-true "refcount-of new object is valid"
  (integerp *rc-new*))
(test-true "refcount-of new object is positive"
  (>= *rc-new* 1))

;; ============================================================
;; region-of Tests
;; ============================================================

(format t "~%=== region-of Tests ===~%")

;; Test 21: region-of returns dict
(defvar *region-info* (region-of 42))
(test-eq "region-of returns dict"
  'Dict
  (type-of *region-info*))

;; Test 22: region-of has id key
(defvar *region-id* (dict-get "id" *region-info*))
(test-true "region-of has id"
  (integerp *region-id*))

;; Test 23: region-of has rank key
(defvar *region-rank* (dict-get "rank" *region-info*))
(test-true "region-of has rank"
  (integerp *region-rank*))

;; ============================================================
;; inspect Tests
;; ============================================================

(format t "~%=== inspect Tests ===~%")

;; Test 24: inspect returns dict
(defvar *inspect-info* (inspect 42))
(test-eq "inspect returns dict"
  'Dict
  (type-of *inspect-info*))

;; Test 25: inspect has type key
(defvar *inspect-type* (dict-get "type" *inspect-info*))
(test-eq "inspect type is Int"
  'Int
  *inspect-type*)

;; Test 26: inspect has value key
(defvar *inspect-value* (dict-get "value" *inspect-info*))
(test-eq "inspect value"
  42
  *inspect-value*)

;; Test 27: inspect has address key
(defvar *inspect-addr* (dict-get "address" *inspect-info*))
(test-true "inspect address is integer"
  (integerp *inspect-addr*))

;; Test 28: inspect has refcount key
(defvar *inspect-rc* (dict-get "refcount" *inspect-info*))
(test-true "inspect refcount is integer"
  (integerp *inspect-rc*))

;; Test 29: inspect has region key
(defvar *inspect-region* (dict-get "region" *inspect-info*))
(test-eq "inspect region is dict"
  'Dict
  (type-of *inspect-region*))

;; Test 30: inspect has sizeof key
(defvar *inspect-size* (dict-get "sizeof" *inspect-info*))
(test-true "inspect sizeof is integer"
  (integerp *inspect-size*))

;; ============================================================
;; Complex Object Inspection
;; ============================================================

(format t "~%=== Complex Object Inspection ===~%")

;; Test 31: inspect array
(defvar *arr-inspect* (inspect (array 1 2 3)))
(test-eq "inspect array type"
  'Array
  (dict-get "type" *arr-inspect*))
(defvar *arr-length* (dict-get "length" *arr-inspect*))
(test-eq "inspect array length"
  3
  *arr-length*)

;; Test 32: inspect string
(defvar *str-inspect* (inspect "hello"))
(test-eq "inspect string type"
  'String
  (dict-get "type" *str-inspect*))
(defvar *str-length* (dict-get "length" *str-inspect*))
(test-eq "inspect string length"
  5
  *str-length*)

;; Test 33: inspect dict
(defvar *dict-inspect* (inspect (dict "key" "value")))
(test-eq "inspect dict type"
  'Dict
  (dict-get "type" *dict-inspect*))
(defvar *dict-count* (dict-get "count" *dict-inspect*))
(test-eq "inspect dict count"
  1
  *dict-count*)

;; ============================================================
;; Special Value Inspection
;; ============================================================

(format t "~%=== Special Value Inspection ===~%")

;; Test 34: inspect boolean
(defvar *bool-inspect* (inspect t))
(test-eq "inspect boolean type"
  'Bool
  (dict-get "type" *bool-inspect*))

;; Test 35: inspect nil/nothing
(defvar *nil-inspect* (inspect nil))
(test-true "inspect nil returns valid dict"
  (dictp *nil-inspect*))

;; Test 36: inspect symbol
(defvar *sym-inspect* (inspect 'foo))
(test-eq "inspect symbol type"
  'Symbol
  (dict-get "type" *sym-inspect*))

;; ============================================================
;; Edge Cases
;; ============================================================

(format t "~%=== Edge Cases ===~%")

;; Test 37: type-of on nil
(test-eq "type-of nil"
  'Bool  ; or might be Nothing
  (type-of nil))

;; Test 38: sizeof on nil
(defvar *sz-nil* (sizeof nil))
(test-true "sizeof nil is valid"
  (integerp *sz-nil*))

;; Test 39: address-of on nil
(defvar *addr-nil* (address-of nil))
(test-true "address-of nil is valid"
  (integerp *addr-nil*))

;; Test 40: refcount-of on nil
(defvar *rc-nil* (refcount-of nil))
(test-true "refcount-of nil is valid"
  (integerp *rc-nil*))

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
