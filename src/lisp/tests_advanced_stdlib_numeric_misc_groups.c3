module lisp;

import std::io;
import main;

fn void run_advanced_stdlib_numeric_array_set_tests(Interp* interp, int* pass, int* fail) {
    // === ARRAY TESTS (Phase 8) ===
    test_eq(interp, "length Array", "(length (Array 1 2 3))", 3, pass, fail);
    test_eq(interp, "ref Array", "(ref (Array 10 20 30) 1)", 20, pass, fail);
    test_eq(interp, "Array from list", "(ref (Array '(10 20 30)) 1)", 20, pass, fail);
    test_eq(interp, "Array canonical constructor", "(ref (Array 10 20 30) 1)", 20, pass, fail);
    setup(interp, "(define test-arr (Array 1 2 3))");
    test_tag_interp(interp, "set! array returns void", "(set! test-arr 1 99)", VOID, pass, fail);
    test_eq(interp, "set! array write", "(ref test-arr 1)", 99, pass, fail);
    test_tag_interp(interp, "set! generic array returns void", "(set! test-arr 1 77)", VOID, pass, fail);
    test_eq(interp, "set! generic array write", "(ref test-arr 1)", 77, pass, fail);
    test_truthy(interp, "array? Array", "(array? (Array 1 2))", pass, fail);
    test_nil(interp, "array? non-arr", "(array? 42)", pass, fail);
    test_truthy(interp, "list from array", "(= (list [1 2 3]) '(1 2 3))", pass, fail);
    test_eq(interp, "Array from list 2", "(ref (Array '(10 20 30)) 2)", 30, pass, fail);
    test_eq(interp, "Array nil→empty", "(length (Array nil))", 0, pass, fail);
    setup(interp, "(define push-arr (Array 1 2))");
    test_tag_interp(interp, "push! returns void", "(push! push-arr 3)", VOID, pass, fail);
    test_eq(interp, "push!", "(length push-arr)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref push-arr 2)", 3, pass, fail);
    test_error_contains(interp, "ref oob", "(ref (Array 1) 5)", "array index out of bounds", pass, fail);
    test_error_contains(interp, "set! array oob", "(set! (Array 1) 5 0)", "array index out of range", pass, fail);
    test_eq(interp, "empty Array", "(length (Array))", 0, pass, fail);

    // === SET TESTS (Phase 9) ===
    test_eq(interp, "length", "(length (Set 1 2 3))", 3, pass, fail);
    test_eq(interp, "set length", "(length (Set 1 2 3))", 3, pass, fail);
    test_truthy(interp, "set-contains?", "(set-contains? (Set 1 2 3) 2)", pass, fail);
    test_nil(interp, "set-contains? miss", "(set-contains? (Set 1 2 3) 5)", pass, fail);
    setup(interp, "(define test-set (Set 1 2))");
    test_tag_interp(interp, "set-add returns void", "(set-add test-set 3)", VOID, pass, fail);
    test_eq(interp, "set-add", "(length test-set)", 3, pass, fail);
    test_truthy(interp, "set-add contains", "(set-contains? test-set 3)", pass, fail);
    test_tag_interp(interp, "set-remove returns void", "(set-remove test-set 2)", VOID, pass, fail);
    test_eq(interp, "set-remove", "(length test-set)", 2, pass, fail);
    test_nil(interp, "set-remove gone", "(set-contains? test-set 2)", pass, fail);
    test_eq(interp, "set dedup", "(length (Set 1 1 2 2 3))", 3, pass, fail);
    test_truthy(interp, "List canonical order",
        "(= (List (Set 3 1 2 2)) '(1 2 3))", pass, fail);
    test_truthy(interp, "List canonical after mutations",
        "(let (s (Set 5 1 3)) (block (set-add s 4) (set-remove s 3) (= (List s) '(1 4 5))))", pass, fail);
    test_truthy(interp, "set type-of is Set", "(= (type-of (Set 1 2 3)) 'Set)", pass, fail);

    // === CONVENIENCE TESTS (Phase 12) ===
    test_eq(interp, "read-string", "(read-string \"(+ 1 2)\")", 3, pass, fail);
    test_truthy(interp, "String", "(= (String 'hello) \"hello\")", pass, fail);
    test_truthy(interp, "Symbol", "(= (Symbol \"abc\") 'abc)", pass, fail);
    test_str_val(interp, "Symbol invalid arg payload code",
        "(handle (Symbol 42) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
}

fn void run_advanced_stdlib_sort_bitwise_hof_tests(Interp* interp, int* pass, int* fail) {
    // === SORTING & BITWISE TESTS (Phase 3) ===
    test_eq(interp, "sort car", "(car (sort (quote (3 1 2))))", 1, pass, fail);
    test_eq(interp, "sort length", "(length (sort (quote (5 3 1 4 2))))", 5, pass, fail);
    test_eq(interp, "sort last", "(car (cdr (cdr (sort (quote (3 1 2))))))", 3, pass, fail);
    test_eq(interp, "sort-by custom", "(car (sort-by (lambda (a) (lambda (b) (- a b))) (quote (3 1 2))))", 1, pass, fail);
    test_truthy(interp, "sort orders BigInteger exactly",
        "(= (map String (sort (list (BigInteger \"9007199254740993\") (BigInteger \"9007199254740991\") (BigInteger \"9007199254740992\")))) (quote (\"9007199254740991\" \"9007199254740992\" \"9007199254740993\")))",
        pass, fail);
    test_truthy(interp, "sort orders BigFloat numerically",
        "(= (map String (sort (list (BigFloat \"2.5\") (BigFloat \"1.25\")))) (quote (\"1.25\" \"2.5\")))",
        pass, fail);
    test_eq(interp, "sort handles list longer than old fixed buffer", "(length (sort (range 257)))", 257, pass, fail);
    test_eq(interp, "sort-by two-arg comparator handles list longer than old fixed buffer",
        "(car (sort-by (lambda (a b) (> a b)) (range 257)))", 256, pass, fail);
    test_truthy(interp, "sort-by boolean false comparator result is falsy",
        "(= (sort-by (lambda (a b) false) (quote (3 1 2))) (quote (3 1 2)))", pass, fail);
    test_error_contains(interp, "sort rejects non-list",
        "(sort 42)", "sort: expected list argument", pass, fail);
    test_error_contains(interp, "sort rejects improper list tail",
        "(sort (cons 2 1))", "sort: expected a proper list", pass, fail);
    test_error_contains(interp, "sort-by rejects non-list",
        "(sort-by (lambda (a) (lambda (b) (- a b))) 42)", "sort-by: expected list argument", pass, fail);
    test_error_contains(interp, "sort-by rejects improper list tail",
        "(sort-by (lambda (a) (lambda (b) (- a b))) (cons 2 1))", "sort-by: expected a proper list", pass, fail);
    test_eq(interp, "bitwise-and", "(bitwise-and 12 10)", 8, pass, fail);
    test_eq(interp, "bitwise-or", "(bitwise-or 12 10)", 14, pass, fail);
    test_eq(interp, "bitwise-xor", "(bitwise-xor 12 10)", 6, pass, fail);
    test_eq(interp, "bitwise-not 0", "(bitwise-not 0)", -1, pass, fail);
    test_eq(interp, "lshift", "(lshift 1 4)", 16, pass, fail);
    test_eq(interp, "rshift", "(rshift 16 4)", 1, pass, fail);
    test_str_val(interp, "BigInteger bitwise-and",
        "(String (bitwise-and (BigInteger \"18446744073709551615\") 255))",
        "255", pass, fail);
    test_str_val(interp, "BigInteger bitwise-or",
        "(String (bitwise-or (BigInteger \"18446744073709551360\") 255))",
        "18446744073709551615", pass, fail);
    test_str_val(interp, "BigInteger bitwise-xor",
        "(String (bitwise-xor (BigInteger \"18446744073709551615\") 255))",
        "18446744073709551360", pass, fail);
    test_str_val(interp, "BigInteger bitwise-not",
        "(String (bitwise-not (BigInteger \"0\")))",
        "-1", pass, fail);
    test_str_val(interp, "Integer lshift overflow promotes",
        "(String (lshift 1 64))",
        "18446744073709551616", pass, fail);
    test_str_val(interp, "BigInteger lshift",
        "(String (lshift (BigInteger \"1\") 100))",
        "1267650600228229401496703205376", pass, fail);
    test_str_val(interp, "BigInteger rshift",
        "(String (rshift (BigInteger \"1267650600228229401496703205376\") 100))",
        "1", pass, fail);
    test_eq(interp, "BigInteger negative lshift count returns zero",
        "(lshift (BigInteger \"1\") -1)", 0, pass, fail);
    test_error_contains(interp, "BigInteger shift count out of range",
        "(lshift 1 (BigInteger \"9223372036854775808\"))", "shift count out of range", pass, fail);
    test_error_contains(interp, "Integer shift count allocation cap",
        "(lshift 1 1048577)", "shift count out of range", pass, fail);
    {
        Value malformed = {};
        malformed.tag = BIG_INTEGER;
        malformed.big_integer_val = null;
        Value* one = make_int(interp, 1);
        Value* int_over_cap = big_integer_shift_value(
            interp, one, (ulong)BIG_INTEGER_MAX_SHIFT_BITS + (ulong)1, true, true, "direct shift");
        Value* big_over_cap = big_integer_shift_value(
            interp, make_big_integer_from_string(interp, "1"), (ulong)BIG_INTEGER_MAX_SHIFT_BITS + (ulong)1, false, false, "direct shift");
        Value* malformed_shift = big_integer_shift_value(
            interp, &malformed, 1, true, false, "malformed BigInteger shift");
        bool ok =
            advanced_numeric_error_value_contains(int_over_cap, "shift count out of range") &&
            advanced_numeric_error_value_contains(big_over_cap, "shift count out of range") &&
            advanced_numeric_error_value_contains(malformed_shift, "malformed BigInteger shift");
        if (ok) {
            (*pass)++;
            emit_pass_literal("[PASS] BigInteger shift helper enforces bounds and payload validity");
        } else {
            (*fail)++;
            io::printn("[FAIL] BigInteger shift helper enforces bounds and payload validity");
        }
    }

    // === STDLIB HOFs (Phase 3) ===
    test_eq(interp, "flatten", "(length (flatten (quote ((1 2) (3 4) (5)))))", 5, pass, fail);
    test_eq(interp, "flatten car", "(car (flatten (quote ((1 2) (3)))))", 1, pass, fail);
    test_eq(interp, "partition yes", "(length (car (partition (lambda (x) (> x 3)) (quote (1 2 3 4 5)))))", 2, pass, fail);
    test_eq(interp, "partition no", "(length (cdr (partition (lambda (x) (> x 3)) (quote (1 2 3 4 5)))))", 3, pass, fail);
    test_error_contains(interp, "flatten rejects improper outer list", "(flatten (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "flatten rejects improper nested list", "(flatten (list (cons 1 2)))", "proper list", pass, fail);
    test_error_contains(interp, "partition rejects improper list", "(partition (lambda (x) true) (cons 1 2))", "proper list", pass, fail);
    test_eq(interp, "remove", "(length (remove (lambda (x) (= x 3)) (quote (1 2 3 4 5))))", 4, pass, fail);
    test_eq(interp, "find found", "(find (lambda (x) (= x 3)) (quote (1 2 3 4 5)))", 3, pass, fail);
    test_nil(interp, "find not found", "(find (lambda (x) (= x 99)) (quote (1 2 3)))", pass, fail);
    test_error_contains(interp, "find rejects improper list before early match", "(find odd? (cons 1 2))", "proper list", pass, fail);
}

fn void run_advanced_stdlib_string_predicate_format_tests(Interp* interp, int* pass, int* fail) {
    // === STRING OPS TESTS (Phase 5) ===
    test_truthy(interp, "string-contains? yes", "(string-contains? \"hello world\" \"world\")", pass, fail);
    test_nil(interp, "string-contains? no", "(string-contains? \"hello\" \"xyz\")", pass, fail);
    test_eq(interp, "string-index-of found", "(string-index-of \"hello world\" \"world\")", 6, pass, fail);
    test_eq(interp, "string-index-of not", "(string-index-of \"hello\" \"xyz\")", -1, pass, fail);
    test_eq(interp, "string-index-of codepoint after UTF-8 prefix",
        "(string-index-of \"éx\" \"x\")", 1, pass, fail);
    test_eq(interp, "string-index-of multibyte needle returns codepoint index",
        "(string-index-of \"aéx\" \"éx\")", 1, pass, fail);
    test_eq(interp, "string-index-of after mixed UTF-8 prefix",
        "(string-index-of \"éx🙂z\" \"z\")", 3, pass, fail);
    test_str(interp, "string-replace", "(string-replace \"hello world\" \"world\" \"there\")", pass, fail);
    test_str(interp, "char-at", "(char-at \"hello\" 1)", pass, fail);
    test_str(interp, "string-repeat", "(string-repeat \"ab\" 3)", pass, fail);
    test_error_contains(interp, "string-repeat rejects output size overflow",
        "(string-repeat \"abc\" 9223372036854775807)",
        "string-repeat: output size overflow", pass, fail);
    test_error_contains(interp, "string-join rejects non-string element",
        "(string-join \",\" (quote (\"a\" 1 \"b\")))", "string-join: expected string elements", pass, fail);

    // === TYPE PREDICATE TESTS (Phase 5) ===
    test_truthy(interp, "float64? yes", "(float64? 3.14)", pass, fail);
    test_nil(interp, "float64? no", "(float64? 42)", pass, fail);
    test_truthy(interp, "number? int", "(number? 42)", pass, fail);
    test_truthy(interp, "number? Float64", "(number? 3.14)", pass, fail);
    test_double(interp, "numeric helper converts BigInteger through Float64 path",
        "(BigInteger \"42\")", 42.0, pass, fail);
    test_double(interp, "numeric helper converts BigFloat through Float64 path",
        "(BigFloat \"1.25\")", 1.25, pass, fail);
    test_nil(interp, "number? string", "(number? \"hello\")", pass, fail);
    test_truthy(interp, "boolean? true", "(boolean? true)", pass, fail);
    test_nil(interp, "boolean? nil", "(boolean? nil)", pass, fail);
    test_nil(interp, "boolean? int", "(boolean? 42)", pass, fail);
    test_truthy(interp, "list? yes", "(list? (quote (1 2 3)))", pass, fail);
    test_truthy(interp, "list? nil", "(list? nil)", pass, fail);
    test_nil(interp, "list? int", "(list? 42)", pass, fail);
    test_nil(interp, "list? improper list", "(list? (cons 1 2))", pass, fail);
    test_truthy(interp, "procedure? lambda", "(procedure? (lambda (x) x))", pass, fail);
    test_truthy(interp, "procedure? prim", "(procedure? +)", pass, fail);
    test_nil(interp, "procedure? int", "(procedure? 42)", pass, fail);
    test_truthy(interp, "zero? 0", "(zero? 0)", pass, fail);
    test_nil(interp, "zero? 1", "(zero? 1)", pass, fail);
    test_truthy(interp, "positive? 5", "(positive? 5)", pass, fail);
    test_nil(interp, "positive? -1", "(positive? -1)", pass, fail);
    test_truthy(interp, "negative? -1", "(negative? -1)", pass, fail);
    test_nil(interp, "negative? 5", "(negative? 5)", pass, fail);
    test_truthy(interp, "even? 4", "(even? 4)", pass, fail);
    test_nil(interp, "even? 3", "(even? 3)", pass, fail);
    test_truthy(interp, "odd? 3", "(odd? 3)", pass, fail);
    test_nil(interp, "odd? 4", "(odd? 4)", pass, fail);
    test_str_val(interp, "abs long min promotes",
        "(String (abs (Integer \"-9223372036854775808\")))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "divide long min by negative one promotes",
        "(String (/ (Integer \"-9223372036854775808\") -1))",
        "9223372036854775808", pass, fail);
    test_eq(interp, "mod long min by negative one returns zero",
        "(% (Integer \"-9223372036854775808\") -1)", 0, pass, fail);
    test_eq(interp, "gcd long min narrows when possible",
        "(gcd (Integer \"-9223372036854775808\") 2)", 2, pass, fail);
    test_str_val(interp, "lcm long min promotes",
        "(String (lcm (Integer \"-9223372036854775808\") 2))",
        "9223372036854775808", pass, fail);

    // === FORMAT & DISPLAY TESTS (Phase 5) ===
    test_str_val(interp, "cl-format ~a", "(cl-format \"~a\" 42)", "42", pass, fail);
    test_str_val(interp, "cl-format ~a ~a", "(cl-format \"~a plus ~a\" 1 2)", "1 plus 2", pass, fail);
    test_str_val(interp, "cl-format tilde", "(cl-format \"100~~\")", "100~", pass, fail);
    test_str_val(interp, "format %s int", "(format \"%s\" 42)", "42", pass, fail);
    test_str_val(interp, "format %s str", "(format \"%s\" \"hello\")", "hello", pass, fail);
    test_str_val(interp, "format %s nil", "(format \"%s\" nil)", "nil", pass, fail);
    test_str_val(interp, "format %s type descriptor Integer", "(format \"%s\" Integer)", "#<type Integer>", pass, fail);
    test_str_val(interp, "format %s type descriptor Dictionary alias", "(format \"%s\" Dictionary)", "#<type Dictionary>", pass, fail);
    test_str_val(interp, "format %s primitive fallback", "(format \"%s\" +)", "#<primitive +>", pass, fail);
    test_str_val(interp, "format %d", "(format \"%d\" 42)", "42", pass, fail);
    test_str_val(interp, "format %x", "(format \"%x\" 255)", "ff", pass, fail);
    test_str_val(interp, "format %X", "(format \"%X\" 255)", "FF", pass, fail);
    test_str_val(interp, "format %o", "(format \"%o\" 8)", "10", pass, fail);
    test_str_val(interp, "format %b", "(format \"%b\" 10)", "1010", pass, fail);
    test_str(interp, "format %f", "(format \"%f\" 3.14)", pass, fail);
    test_str_val(interp, "format %.2f", "(format \"%.2f\" 3.14159)", "3.14", pass, fail);
    test_str_val(interp, "format %%", "(format \"100%%\")", "100%", pass, fail);
    test_str_val(interp, "format mixed", "(format \"%s is %d\" \"x\" 42)", "x is 42", pass, fail);
    test_str_val(interp, "format %b zero", "(format \"%b\" 0)", "0", pass, fail);
    test_error_contains(interp, "format %b long min overflow",
        "(format \"%b\" (Integer \"-9223372036854775808\"))", "format: %b integer overflow", pass, fail);
    test_error_contains(interp, "format width overflow",
        "(format \"%2147483648s\" 1)", "format: width or precision too large", pass, fail);
    test_str_val(interp, "format %d neg", "(format \"%d\" -7)", "-7", pass, fail);
    test_str_val(interp, "format %.0f", "(format \"%.0f\" 3.7)", "3", pass, fail);
}
