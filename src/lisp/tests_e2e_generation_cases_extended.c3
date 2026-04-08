module lisp;

const E2ETestCase[] E2E_TESTS_EXTENDED = {
    // === Misc ===
    { "nested closures", "((lambda (x) ((lambda (y) (+ x y)) 20)) 10)" },
    { "deep closure chain", "((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 1) 2) 3)" },
    { "list operations", "(car (cdr (cdr (quote (1 2 3 4 5)))))" },
    { "complex arith", "(* (+ 1 2) (- 10 (/ 8 2)))" },
    { "recursive list build", "(length (cons 1 (cons 2 (cons 3 (quote ())))))" },

    // === Stdlib HOFs ===
    { "stdlib map double", "(map (lambda (x) (* x 2)) (quote (1 2 3)))" },
    { "stdlib map inc", "(map (lambda (x) (+ x 1)) (quote (10 20 30)))" },
    { "stdlib map identity", "(map (lambda (x) x) (quote (1 2 3)))" },
    { "stdlib filter gt2", "(filter (lambda (x) (> x 2)) (quote (1 2 3 4 5)))" },
    { "stdlib filter none", "(filter (lambda (x) (> x 100)) (quote (1 2 3)))" },
    { "stdlib filter all", "(filter (lambda (x) (> x 0)) (quote (1 2 3)))" },
    { "stdlib foldl sum", "(foldl + 0 (quote (1 2 3 4 5)))" },
    { "stdlib foldl product", "(foldl * 1 (quote (1 2 3 4 5)))" },
    { "stdlib foldr cons", "(foldr cons nil (quote (1 2 3)))" },
    { "stdlib append", "(append (quote (1 2)) (quote (3 4)))" },
    { "stdlib append empty l", "(append (quote ()) (quote (1 2)))" },
    { "stdlib append empty r", "(append (quote (1 2)) (quote ()))" },
    { "stdlib reverse", "(reverse (quote (1 2 3)))" },
    { "stdlib reverse single", "(reverse (quote (7)))" },
    { "stdlib reverse empty", "(reverse (quote ()))" },
    { "stdlib compose", "((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 5)" },
    { "stdlib nth 0", "(nth 0 (quote (10 20 30)))" },
    { "stdlib nth 2", "(nth 2 (quote (10 20 30)))" },
    { "stdlib take 3", "(take 3 (quote (1 2 3 4 5)))" },
    { "stdlib take 0", "(take 0 (quote (1 2 3)))" },
    { "stdlib take all", "(take 5 (quote (1 2 3)))" },
    { "stdlib drop 2", "(drop 2 (quote (1 2 3 4 5)))" },
    { "stdlib drop 0", "(drop 0 (quote (1 2 3)))" },
    { "stdlib drop all", "(drop 5 (quote (1 2 3)))" },
    { "stdlib zip", "(car (zip (quote (1 2 3)) (quote (4 5 6))))" },
    { "stdlib range 5", "(range 5)" },
    { "stdlib range 0", "(range 0)" },
    { "stdlib range 1", "(range 1)" },
    { "stdlib any? found", "(any? (lambda (x) (= x 3)) (quote (1 2 3 4)))" },
    { "stdlib any? not found", "(any? (lambda (x) (= x 99)) (quote (1 2 3)))" },
    { "stdlib every? true", "(every? (lambda (x) (> x 0)) (quote (1 2 3)))" },
    { "stdlib every? false", "(every? (lambda (x) (> x 2)) (quote (1 2 3)))" },
    { "stdlib id compose", "((compose id id) 42)" },
    { "stdlib map filter", "(foldl + 0 (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))" },

    // === Dictionary operations ===
    { "dict empty", "(dict? (Dictionary))" },
    { "dict create ref", "(ref (Dictionary 1 10 2 20) 1)" },
    { "dict ref 2", "(ref (Dictionary 1 10 2 20) 2)" },
    { "dict? yes", "(dict? (Dictionary 1 2))" },
    { "dict? no", "(dict? 42)" },
    { "dict has? yes", "(has? (Dictionary 1 10 2 20) 1)" },
    { "dict has? no", "(has? (Dictionary 1 10) 99)" },
    { "dict keys", "(length (keys (Dictionary 1 10 2 20)))" },
    { "dict canonical keys", "(keys (Dictionary 'b 2 'a 1 'c 3))" },
    { "dict canonical values", "(values (Dictionary 'b 2 'a 1 'c 3))" },
    { "set canonical list", "(set->list (Set 3 1 2 2))" },

    // string-contains?, string-index-of, string-replace, char-at,
    // string-repeat, symbol->string, string->symbol — not mapped in compiler

    // === String operations (extended) ===
    { "string-upcase", `(string-upcase "hello")` },
    { "string-downcase", `(string-downcase "HELLO")` },
    { "string-trim", `(string-trim "  hello  ")` },

    // === set! mutation (setups define s_x=10, s_acc=0, s_count=0) ===
    { "set! returns Void", "(set! s_x 20)" },
    { "set! read back", "(block (set! s_acc 1) (set! s_acc (+ s_acc 1)) s_acc)" },
    { "set! in lambda", "(block ((lambda (n) (set! s_count n)) 42) s_count)" },
    { "set! generic array returns Void", "(let (a [1 2 3]) (type-of (set! a 1 99)))" },
    { "set! generic dict returns Void", "(let (d {'k 1}) (type-of (set! d 'k 9)))" },
    { "set! generic array", "(let (a [1 2 3]) (block (set! a 1 99) (ref a 1)))" },
    { "set! generic dict", "(let (d {'k 1}) (block (set! d 'k 9) (ref d 'k)))" },
    // Top-level define/set! command predicates still generate invalid AOT locals.
    { "if command Void predicate truthy", "(if (block 0 1) 1 0)" },

    // === Quasiquote (no unquote — eval_source can't see let-bound vars) ===
    { "quasiquote literal", "`42" },
    { "quasiquote list", "`(1 2 3)" },
    { "quasiquote no unquote", "`(a b c)" },

    // when/unless — skipped: compiler doesn't expand eval-time macros

    // === Dot-bracket indexing ===
    { "dot-bracket 0", "(let (lst (quote (10 20 30))) lst.[0])" },
    { "dot-bracket 2", "(let (lst (quote (10 20 30))) lst.[2])" },
    { "dot-bracket expr", "(let (lst (quote (10 20 30))) lst.[(+ 0 1)])" },

    // === Deep recursion / TCO ===
    { "tco sum 1000", "(let loop (n 1000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "tco count 5000", "(let count (n 5000) (if (= n 0) 0 (count (- n 1))))" },

    // === Complex combinations ===
    { "map then reverse", "(reverse (map (lambda (x) (* x 10)) (quote (1 2 3))))" },
    { "filter then length", "(length (filter (lambda (x) (> x 3)) (quote (1 2 3 4 5 6))))" },
    { "nested map", "(map (lambda (x) (+ x 1)) (map (lambda (x) (* x 2)) (quote (1 2 3))))" },
    { "foldl with filter", "(foldl + 0 (filter (lambda (x) (< x 4)) (quote (1 2 3 4 5))))" },
    { "compose triple", "((compose (lambda (x) (* x 3)) (lambda (x) (+ x 1))) 10)" },
    { "range then sum", "(foldl + 0 (range 10))" },
    { "take from range", "(take 3 (range 10))" },
    { "drop from range", "(drop 7 (range 10))" },
    { "match on map result", "(match (map (lambda (x) (* x 10)) (quote (1 2 3))) ([a b c] (+ a (+ b c))))" },
    // Command-valued guard subjects with top-level define/set! still generate invalid AOT locals.
    { "match command Void guard truthy", "(match (block 0 1) ((? (lambda (v) v)) 7) (_ 0))" },
    { "match guard false falls through", "(match 5 ((? false) 1) (_ 0))" },
    { "match guard subpattern binding", "(match 5 ((? (> x 3) x) x) (_ 0))" },
    { "match guard error propagates", "(match 5 ((? (/ 1 0)) 1) (_ 0))" },
    // Legacy AOT guard-trace parity rows; runtime coverage stays elsewhere.
    { "closure over let rec", "(let ^rec (f (lambda (n) (if (<= n 1) 1 (* n (f (- n 1)))))) (f 7))" },
    // Legacy AOT nested-handle parity row; runtime/effect coverage stays elsewhere.
    { "effect abort skips remainder", "(let (x 0) (block (handle (block (signal bail 1) (set! x 99) 7) (bail v 5)) x))" },
    { "resolve is terminal in clause",
      "(let (x 0) (block (handle (signal once 10) (once v (block (resolve (+ v 1)) (set! x 99) 0))) x))" },
    { "capture double resume", "(checkpoint (+ (capture k (+ (k 1) (k 2))) 10))" },
    // Top-level replay bookkeeping defines still generate invalid AOT locals.
    { "let-rec factorial 8", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 8))" },

    // === P1: type-of ===
    { "type-of int", "(type-of 42)" },
    { "type-of string", `(type-of "hello")` },
    { "type-of symbol", "(type-of (quote foo))" },
    { "type-of list", "(type-of (quote (1 2)))" },
    { "type-of nil", "(type-of nil)" },
    { "type-of true", "(type-of true)" },
    { "type-of closure", "(type-of (lambda (x) x))" },
    { "type-of double", "(type-of 3.14)" },

    // === P1: type predicates ===
    { "int? yes", "(int? 42)" },
    { "int? no", "(int? 3.14)" },
    { "string? yes", `(string? "hi")` },
    { "string? no", "(string? 42)" },
    { "symbol? yes", "(symbol? (quote foo))" },
    { "symbol? no", "(symbol? 42)" },
    { "double? yes", "(double? 3.14)" },
    { "double? no", "(double? 42)" },
    { "number? int", "(number? 42)" },
    { "number? double", "(number? 3.14)" },
    { "number? no", "(number? (quote foo))" },
    { "pair? yes", "(pair? (quote (1 2)))" },
    { "pair? no", "(pair? 42)" },
    { "null? yes", "(null? nil)" },
    { "null? no", "(null? 42)" },

    // === P1: math primitives ===
    { "abs positive", "(abs 5)" },
    { "abs negative", "(abs -7)" },
    { "abs zero", "(abs 0)" },
    { "abs double", "(abs -3.5)" },
    { "min ints", "(min 3 7)" },
    { "min reversed", "(min 7 3)" },
    { "min doubles", "(min 1.5 2.5)" },
    { "max ints", "(max 3 7)" },
    { "max reversed", "(max 7 3)" },
    { "max doubles", "(max 1.5 2.5)" },
    { "floor", "(floor 3.7)" },
    { "floor neg", "(floor -3.2)" },
    { "ceiling", "(ceiling 3.2)" },
    { "ceiling neg", "(ceiling -3.7)" },
    { "round up", "(round 3.6)" },
    { "round down", "(round 3.4)" },
    { "truncate pos", "(truncate 3.9)" },
    { "truncate neg", "(truncate -3.9)" },
    // inexact->exact, symbol->string, and pow are still not mapped in the AOT e2e compiler path.
    { "string->number int overflow type", "(type-of (string->number \"9223372036854775808\"))" },
    { "sqrt 4", "(sqrt 4)" },
    { "sqrt 9", "(sqrt 9)" },
    { "even? yes", "(even? 4)" },
    { "even? no", "(even? 3)" },
    { "odd? yes", "(odd? 3)" },
    { "odd? no", "(odd? 4)" },
    { "zero? yes", "(zero? 0)" },
    { "zero? no", "(zero? 1)" },
    { "positive? yes", "(positive? 5)" },
    { "positive? no", "(positive? -3)" },
    { "negative? yes", "(negative? -3)" },
    { "negative? no", "(negative? 5)" },
    { "gcd", "(gcd 12 8)" },
    { "gcd coprime", "(gcd 7 13)" },

    // === P2: substring (3-arg via interpreter delegation) ===
    { "substring basic", `(substring "hello" 1 3)` },
    { "substring from start", `(substring "abcdef" 0 3)` },
    { "substring to end", `(substring "abcdef" 3 6)` },
    { "substring single", `(substring "hello" 0 1)` },

    // === P2: string-split and string-join ===
    { "string-split", `(length (string-split "a,b,c" ","))` },
    { "string-join", `(string-join " " (quote ("hello" "world")))` },
    { "string-join empty", `(string-join "," (quote ()))` },

    // === P2: when/unless ===
    { "when true", "(when true 42)" },
    { "when false", "(when false 42)" },
    // Command-valued when predicates with nested define/set! still generate invalid AOT locals.
    { "when command Void predicate", "(let (e2e-void-when 0) (block (when (block 0 1) (set! e2e-void-when 9)) e2e-void-when))" },
    { "unless true", "(unless true 42)" },
    { "unless false", "(unless false 42)" },

    // === P3: Bitwise ops ===
    { "bitwise-and", "(bitwise-and 12 10)" },
    { "bitwise-or", "(bitwise-or 12 10)" },
    { "bitwise-xor", "(bitwise-xor 12 10)" },
    { "bitwise-not 0", "(bitwise-not 0)" },
    { "bitwise-not 1", "(bitwise-not 1)" },
    { "lshift", "(lshift 1 4)" },
    { "rshift", "(rshift 16 4)" },
    { "lshift-rshift", "(rshift (lshift 3 8) 8)" },

    // === P4: Quasiquote + unquote ===
    { "qq unquote let", "(let (x 42) `(a ,x b))" },
    { "qq unquote arith", "(let (n 10) `(result ,(+ n 5)))" },
    { "qq unquote nested", "(let (a 1 b 2) `(,a ,b ,(+ a b)))" },
    { "qq splice literal", "`(1 2 3)" },

    // === L4 backend parity: type definitions + dispatch ===
    { "type struct ctor", `(type-of (User "bob"))` },
    { "type union ctor", "(type-of (Some 5))" },
    { "dispatch exact subtype", `(who (Admin "ops"))` },
    { "dispatch parent subtype", `(who-parent (Admin "ops"))` },
    { "dispatch numeric explicit conversion", "(who (Double 7))" },
    { "dispatch value literal exact", "(ping 'ping)" },
    { "dispatch value literal fallback", "(ping 'other)" },
    { "alias annotation dispatch", `(alias-check (User "amy"))` },
    { "dispatch ambiguity reason",
      "(ref (ref (explain 'dispatch (amb 1)) 'decision) 'reason)" },
    // Typed-method + formatted ambiguity payload generation still depends on unmapped AOT helpers.
};
