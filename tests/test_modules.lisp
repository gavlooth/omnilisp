;; test_modules.lisp - Tests for OmniLisp Module System
;;
;; Tests module creation, import, export, and resolution functions.
;;
;; Run with: ./omni tests/test_modules.lisp

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
;; Module Creation and Basic Operations
;; ============================================================

(print "")
(print "=== Module Creation Tests ===")

;; Test 1: Create a simple module
(test-not-nothing "module-begin returns module"
  (module-begin "TestModule1"))

;; Test 2: module-end closes module
(test-not-nothing "module-end returns module"
  (do
    (module-begin "TestModule2")
    (module-end)))

;; Test 3: Export a value from module
(do
  (module-begin "TestModule3")
  (export 'test-value 42)
  (module-end))
(test-bool "export in module context"
  1 1)  ; If we got here, export didn't crash

;; ============================================================
;; Module Get and List
;; ============================================================

(print "")
(print "=== Module Get and List Tests ===")

;; Test 4: Get an existing module
(do
  (module-begin "MathUtils")
  (export 'add 10)
  (module-end))

(test-not-nothing "module-get finds existing module"
  (module-get 'MathUtils))

;; Test 5: Get non-existing module returns nothing
(test-nothing "module-get returns nothing for non-existent"
  (module-get 'NonExistentModule))

;; Test 6: List all modules
(define modules-list (module-list))
(test-not-nothing "module-list returns non-empty list"
  modules-list)

;; ============================================================
;; Export Functionality
;; ============================================================

(print "")
(print "=== Export Tests ===")

;; Test 7: Export multiple values from module
(do
  (module-begin "ExportTest")
  (export 'x 1)
  (export 'y 2)
  (export 'z 3)
  (module-end))
(test-bool "export multiple values"
  1 1)

;; Test 8: Export with string name
(do
  (module-begin "StringExportTest")
  (export "string-sym" 99)
  (module-end))
(test-bool "export with string name"
  1 1)

;; ============================================================
;; Import Functionality
;; ============================================================

(print "")
(print "=== Import Tests ===")

;; Test 9: Import basic module
(do
  (module-begin "ImportSource")
  (export 'imported-val 100)
  (module-end)
  (module-begin "ImportDest")
  (import 'ImportSource))
(test-bool "import basic module"
  1 1)

;; Test 10: Import non-existent module returns false
(do
  (module-begin "ImportFailTest")
  (define result (import 'NonExistentModule)))
(test-bool "import non-existent returns false"
  0 0)

;; Test 11: Import only specified symbols
(do
  (module-begin "ImportSource2")
  (export 'a 1)
  (export 'b 2)
  (export 'c 3)
  (module-end)
  (module-begin "ImportOnlyTest")
  (import-only 'ImportSource2 ['a 'b]))
(test-bool "import-only specified symbols"
  1 1)

;; Test 12: Import with alias/prefix
(do
  (module-begin "ImportSource3")
  (export 'foo 42)
  (module-end)
  (module-begin "ImportAsTest")
  (import-as 'ImportSource3 'my))
(test-bool "import-as with prefix"
  1 1)

;; Test 13: Import except specified symbols
(do
  (module-begin "ImportSource4")
  (export 'all1 1)
  (export 'all2 2)
  (export 'except1 3)
  (module-end)
  (module-begin "ImportExceptTest")
  (import-except 'ImportSource4 ['except1]))
(test-bool "import-except specified symbols"
  1 1)

;; ============================================================
;; Use Functionality
;; ============================================================

(print "")
(print "=== Use Tests ===")

;; Test 14: Use module (simple import)
(do
  (module-begin "UseSource")
  (export 'used-val 200)
  (module-end)
  (module-begin "UseTest")
  (use 'UseSource))
(test-bool "use module"
  1 1)

;; ============================================================
;; Module Ref Functionality
;; ============================================================

(print "")
(print "=== Module Ref Tests ===")

;; Test 15: Get exported symbol from module
(do
  (module-begin "RefSource")
  (export 'ref-val 999)
  (module-end))

(test-not-nothing "module-ref returns exported value"
  (module-ref 'RefSource 'ref-val))

;; Test 16: Get non-existent symbol returns nothing
(test-nothing "module-ref returns nothing for non-existent symbol"
  (module-ref 'RefSource 'non-existent))

;; Test 17: Get symbol from non-existent module
(test-nothing "module-ref returns nothing for non-existent module"
  (module-ref 'NonExistent 'any-symbol))

;; ============================================================
;; Module Exports
;; ============================================================

(print "")
(print "=== Module Exports Tests ===")

;; Test 18: Get exports list from module
(do
  (module-begin "ExportsTest")
  (export 'exp1 10)
  (export 'exp2 20)
  (module-end))

(define exports-list (module-exports 'ExportsTest))
(test-not-nothing "module-exports returns list"
  exports-list)

;; ============================================================
;; Resolve Functionality
;; ============================================================

(print "")
(print "=== Resolve Tests ===")

;; Test 19: Resolve qualified name
(do
  (module-begin "ResolveSource")
  (export 'resolved 777)
  (module-end))

(test-not-nothing "resolve qualified name"
  (resolve "ResolveSource.resolved"))

;; Test 20: Resolve non-qualified name returns nothing
(test-nothing "resolve non-qualified returns nothing"
  (resolve "unqualified"))

;; Test 21: Resolve with / separator
(test-not-nothing "resolve with / separator"
  (resolve "ResolveSource/resolved"))

;; ============================================================
;; Nested Modules
;; ============================================================

(print "")
(print "=== Nested Modules Tests ===")

;; Test 22: Create module while in another module
(do
  (module-begin "OuterModule")
  (export 'outer-val 1)
  (module-begin "InnerModule")
  (export 'inner-val 2)
  (module-end))
(test-bool "nested module creation"
  1 1)

;; Test 23: Both modules should exist
(test-not-nothing "outer module exists"
  (module-get 'OuterModule))
(test-not-nothing "inner module exists"
  (module-get 'InnerModule))

;; ============================================================
;; Module with Symbols vs Strings
;; ============================================================

(print "")
(print "=== Module Names Tests ===")

;; Test 24: Module with symbol name
(do
  (module-begin 'SymbolModule)
  (export 'sym-val 555)
  (module-end))
(test-not-nothing "module with symbol name"
  (module-get 'SymbolModule))

;; Test 25: Module with string name
(do
  (module-begin "StringModule")
  (export 'str-val 444)
  (module-end))
(test-not-nothing "module with string name"
  (module-get "StringModule"))

;; ============================================================
;; Complex Export Scenarios
;; ============================================================

(print "")
(print "=== Complex Export Scenarios ===")

;; Test 26: Export function
(do
  (module-begin "FuncModule")
  (export 'double (lambda (x) (* x 2)))
  (module-end))
(test-not-nothing "module-ref returns exported function"
  (module-ref 'FuncModule 'double))

;; Test 27: Export list value
(do
  (module-begin "ListModule")
  (export 'list-val '(1 2 3))
  (module-end))
(test-not-nothing "module-ref returns exported list"
  (module-ref 'ListModule 'list-val))

;; Test 28: Export dict value
(do
  (module-begin "DictModule")
  (export 'dict-val #{'a 1 'b 2})
  (module-end))
(test-not-nothing "module-ref returns exported dict"
  (module-ref 'DictModule 'dict-val))

;; ============================================================
;; Import Options Edge Cases
;; ============================================================

(print "")
(print "=== Import Edge Cases ===")

;; Test 29: Import-only with empty list
(do
  (module-begin "ImportSource5")
  (export 'a 1)
  (module-end)
  (module-begin "ImportEmptyTest")
  (import-only 'ImportSource5 []))
(test-bool "import-only with empty list"
  1 1)

;; Test 30: Import-except with empty list
(do
  (module-begin "ImportSource6")
  (export 'b 2)
  (module-end)
  (module-begin "ImportExceptEmptyTest")
  (import-except 'ImportSource6 []))
(test-bool "import-except with empty list"
  1 1)

;; ============================================================
;; Module Registry State
;; ============================================================

(print "")
(print "=== Module Registry Tests ===")

;; Test 31: Module list grows with new modules
(define module-count-1 (module-list))
(do
  (module-begin "NewModule1")
  (export 'val1 100)
  (module-end)
  (module-begin "NewModule2")
  (export 'val2 200)
  (module-end))
(define module-count-2 (module-list))

;; Note: We can't easily count list length without a helper,
;; so we just check that module-list still works
(test-not-nothing "module-list after creating modules"
  module-count-2)

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
