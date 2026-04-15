module lisp;

import std::io;
import main;

alias AdvancedStdlibNumericGroupFn = fn void(Interp* interp, int* pass, int* fail);

fn void trace_env_chain_state(char[] label, Interp* interp, Env* env) {
    if (!advanced_group_trace_enabled()) {
        return;
    }
    if (env == null) {
        std::io::printfn("OMNI_ADVANCED_GROUP_TRACE state %s env=null", (ZString)label);
        return;
    }
    Env* cur = env;
    for (int depth = 0; depth < 6 && cur != null; depth++) {
        int name0 = cur.binding_count > 0 ? (int)cur.bindings[0].name : -1;
        int tag0 = cur.binding_count > 0 && cur.bindings[0].value != null ? (int)cur.bindings[0].value.tag : -1;
        int name1 = cur.binding_count > 1 ? (int)cur.bindings[1].name : -1;
        int tag1 = cur.binding_count > 1 && cur.bindings[1].value != null ? (int)cur.bindings[1].value.tag : -1;
        std::io::printfn(
            "OMNI_ADVANCED_GROUP_TRACE state %s frame=%d ptr_in_current=%d is_root_box=%d scope_gen=%d count=%d name0=%d tag0=%d name1=%d tag1=%d parent_global=%d",
            (ZString)label,
            depth,
            boundary_ptr_in_scope((void*)cur, interp.current_scope) ? 1 : 0,
            cur.is_root_persistent_box() ? 1 : 0,
            (int)cur.scope_gen,
            (int)cur.binding_count,
            name0,
            tag0,
            name1,
            tag1,
            cur.parent == interp.global_env ? 1 : 0
        );
        cur = cur.parent;
    }
}

fn void run_advanced_stdlib_numeric_group(char[] name, AdvancedStdlibNumericGroupFn group_fn,
    Interp* interp, int* pass, int* fail) {
    if (!advanced_group_filter_matches_exact(name) &&
        !advanced_group_filter_matches_exact("advanced-stdlib-numeric")) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_stdlib_numeric_exact_group(char[] name, AdvancedStdlibNumericGroupFn group_fn,
    Interp* interp, int* pass, int* fail) {
    if (!advanced_group_filter_matches_exact(name)) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_stdlib_numeric_collection_tests(Interp* interp, int* pass, int* fail) {
    // === STDLIB TESTS ===
    test_eq(interp, "map car", "(car (map (lambda (x) (* x 2)) (quote (1 2 3))))", 2, pass, fail);
    test_eq(interp, "map length", "(length (map (lambda (x) (* x 2)) (quote (1 2 3))))", 3, pass, fail);
    test_nil(interp, "map empty", "(map (lambda (x) (* x 2)) (quote ()))", pass, fail);
    test_eq(interp, "filter car", "(car (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "filter length", "(length (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))", 3, pass, fail);
    test_nil(interp, "filter none", "(filter (lambda (x) (> x 100)) (quote (1 2 3)))", pass, fail);
    test_eq(interp, "foldl sum", "(foldl + 0 (quote (1 2 3 4 5)))", 15, pass, fail);
    test_eq(interp, "foldl product", "(foldl * 1 (quote (1 2 3 4)))", 24, pass, fail);
    test_eq(interp, "foldr cons len", "(length (foldr cons nil (quote (1 2 3))))", 3, pass, fail);
    test_eq(interp, "foldr cons car", "(car (foldr cons nil (quote (1 2 3))))", 1, pass, fail);
    test_error_contains(interp, "map rejects improper list", "(map (lambda (x) x) (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "filter rejects improper list", "(filter (lambda (x) true) (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "foldl rejects improper list", "(foldl + 0 (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "foldr rejects improper list", "(foldr cons nil (cons 1 2))", "proper list", pass, fail);
    test_eq(interp, "append length", "(length (append (quote (1 2)) (quote (3 4))))", 4, pass, fail);
    test_eq(interp, "append car", "(car (append (quote (1 2)) (quote (3 4))))", 1, pass, fail);
    test_eq(interp, "append nil left", "(car (append nil (quote (1 2))))", 1, pass, fail);
    test_eq(interp, "reverse car", "(car (reverse (quote (1 2 3))))", 3, pass, fail);
    test_eq(interp, "reverse length", "(length (reverse (quote (1 2 3))))", 3, pass, fail);
    test_nil(interp, "reverse nil", "(reverse nil)", pass, fail);
    test_error_contains(interp, "reverse rejects improper list", "(reverse (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "append rejects improper left list", "(append (cons 1 2) '(3))", "proper list", pass, fail);
    test_error_contains(interp, "append rejects improper right list", "(append '(1) (cons 2 3))", "proper list", pass, fail);
    test_eq(interp, "compose *2.+1", "((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3)", 8, pass, fail);
    test_eq(interp, "compose +1.*2", "((compose (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 3)", 7, pass, fail);
    test_eq(interp, "id", "(id 42)", 42, pass, fail);
    test_eq(interp, "nth 0", "(nth 0 (quote (10 20 30 40)))", 10, pass, fail);
    test_eq(interp, "nth 2", "(nth 2 (quote (10 20 30 40)))", 30, pass, fail);
    test_error_contains(interp, "nth rejects improper list", "(nth 0 (cons 1 2))", "proper list", pass, fail);
    test_eq(interp, "take length", "(length (take 3 (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "take car", "(car (take 3 (quote (1 2 3 4 5))))", 1, pass, fail);
    test_nil(interp, "take 0", "(take 0 (quote (1 2 3)))", pass, fail);
    test_eq(interp, "drop car", "(car (drop 2 (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "drop length", "(length (drop 2 (quote (1 2 3 4 5))))", 3, pass, fail);
    test_eq(interp, "drop 0", "(car (drop 0 (quote (1 2 3))))", 1, pass, fail);
    test_eq(interp, "zip length", "(length (zip (quote (1 2 3)) (quote (4 5 6))))", 3, pass, fail);
    test_eq(interp, "zip first car", "(car (car (zip (quote (1 2 3)) (quote (4 5 6)))))", 1, pass, fail);
    test_eq(interp, "zip first cdr", "(cdr (car (zip (quote (1 2 3)) (quote (4 5 6)))))", 4, pass, fail);
    test_eq(interp, "zip unequal", "(length (zip (quote (1 2)) (quote (4 5 6))))", 2, pass, fail);
    test_error_contains(interp, "take rejects improper list", "(take 1 (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "drop rejects improper list", "(drop 0 (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "zip rejects improper left list", "(zip (cons 1 2) '(3))", "proper list", pass, fail);
    test_error_contains(interp, "zip rejects improper right list", "(zip '(1) (cons 3 4))", "proper list", pass, fail);
    test_eq(interp, "range length", "(length (range 5))", 5, pass, fail);
    test_eq(interp, "range car", "(car (range 5))", 0, pass, fail);
    test_nil(interp, "range 0", "(range 0)", pass, fail);
    test_nil(interp, "for-each", "(for-each (lambda (x) (+ x 1)) (quote (1 2 3)))", pass, fail);
    test_error_contains(interp, "for-each rejects improper list", "(for-each (lambda (x) x) (cons 1 2))", "proper list", pass, fail);
    test_truthy(interp, "any? found", "(any? (lambda (x) (= x 3)) (quote (1 2 3 4)))", pass, fail);
    test_nil(interp, "any? not found", "(any? (lambda (x) (= x 99)) (quote (1 2 3 4)))", pass, fail);
    test_truthy(interp, "every? all", "(every? (lambda (x) (> x 0)) (quote (1 2 3 4)))", pass, fail);
    test_nil(interp, "every? not all", "(every? (lambda (x) (> x 2)) (quote (1 2 3 4)))", pass, fail);
    test_error_contains(interp, "any? rejects improper list before truthy tail", "(any? (lambda (x) false) (cons 1 2))", "proper list", pass, fail);
    test_error_contains(interp, "every? rejects improper list before early false", "(every? (lambda (x) false) (cons 1 2))", "proper list", pass, fail);
    test_truthy(interp, "every? empty", "(every? (lambda (x) (> x 0)) nil)", pass, fail);
    test_eq(interp, "try/raise catch payload data", "(try (lambda (xx) (+ 1 (signal raise 42))) (lambda (payload) (ref payload 'data)))", 42, pass, fail);
    test_eq(interp, "try no error", "(try (lambda (xx) (+ 1 2)) (lambda (msg) msg))", 3, pass, fail);
    test_truthy(interp, "assert! true", "(assert! (= 1 1) \"ok\")", pass, fail);
    test_truthy(interp, "assert! false raises canonical payload",
        "(let (payload (handle (assert! (= 1 2) \"fail\") (raise msg msg))) "
        "  (and (dict? payload) "
        "       (and (= (String (ref payload 'domain)) \"stdlib\") "
        "            (and (= (String (ref payload 'code)) \"stdlib/assert-failed\") "
        "                 (= (ref payload 'message) \"fail\")))))",
        pass, fail);
    test_eq(interp, "map+filter+foldl", "(foldl + 0 (filter (lambda (x) (> x 3)) (map (lambda (x) (* x 2)) (quote (1 2 3 4 5)))))", 28, pass, fail);
    test_eq(interp, "reverse+take", "(car (take 1 (reverse (quote (1 2 3)))))", 3, pass, fail);
    test_eq(interp, "sum of squares", "(foldl + 0 (map (lambda (x) (* x x)) (range 5)))", 30, pass, fail);
}

fn void run_advanced_stdlib_numeric_float_math_tests(Interp* interp, int* pass, int* fail) {
    // === FLOAT TESTS ===
    test_double(interp, "float 3.14", "3.14", 3.14, pass, fail);
    test_double(interp, "float -2.5", "-2.5", -2.5, pass, fail);
    test_double(interp, "float 1.0e3", "1.0e3", 1000.0, pass, fail);
    test_double(interp, "float 1.5e-2", "1.5e-2", 0.015, pass, fail);
    test_double(interp, "int+float", "(+ 1 2.0)", 3.0, pass, fail);
    test_double(interp, "float*int", "(* 2.5 4)", 10.0, pass, fail);
    test_double(interp, "float/int", "(/ 1.0 3)", 0.333333, pass, fail);
    test_double(interp, "float-float", "(- 5.5 2.5)", 3.0, pass, fail);
    test_eq(interp, "int+int stays int", "(+ 1 2)", 3, pass, fail);
    test_error_contains(interp, "+ rejects trailing arg", "(+ 10 20 30)", "arity mismatch", pass, fail);
    test_error_contains(interp, "* rejects trailing arg", "(* 2 3 4)", "arity mismatch", pass, fail);
    test_truthy(interp, "< int float", "(< 1 2.5)", pass, fail);
    test_truthy(interp, "> float int", "(> 3.5 2)", pass, fail);
    test_truthy(interp, "= int float", "(= 1 1.0)", pass, fail);
    test_truthy(interp, "<= float", "(<= 1.5 1.5)", pass, fail);
    test_truthy(interp, ">= float", "(>= 2.0 1.5)", pass, fail);
    test_double(interp, "unary neg float", "(- 0 3.14)", -3.14, pass, fail);
    test_double(interp, "Double", "(Double 5)", 5.0, pass, fail);
    test_eq(interp, "Integer", "(Integer 3.7)", 3, pass, fail);
    test_eq(interp, "Integer negative truncates toward zero", "(Integer -3.7)", -3, pass, fail);
    test_double(interp, "parse-number float", "(parse-number \"3.14\")", 3.14, pass, fail);
    test_double(interp, "parse-number exp", "(parse-number \"1.5e2\")", 150.0, pass, fail);
    test_str_val(interp, "parse-number integer overflow => BigInteger",
        "(String (parse-number \"9223372036854775808\"))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "parse-number integer underflow => BigInteger",
        "(String (parse-number \"-9223372036854775809\"))",
        "-9223372036854775809", pass, fail);
    test_truthy(interp, "parse-number overflow type is BigInteger",
        "(= (type-of (parse-number \"9223372036854775808\")) 'BigInteger)", pass, fail);
    test_truthy(interp, "parse-number long-min type is Integer",
        "(= (type-of (parse-number \"-9223372036854775808\")) 'Integer)", pass, fail);
    test_nil(interp, "parse-number invalid overflow suffix => nil",
        "(parse-number \"9223372036854775808x\")", pass, fail);
    test_str_val(interp, "BigInteger ctor large string",
        "(String (BigInteger \"9223372036854775808\"))",
        "9223372036854775808", pass, fail);
    test_truthy(interp, "BigInteger type-of",
        "(= (type-of (BigInteger \"1\")) 'BigInteger)", pass, fail);
    test_truthy(interp, "BigInteger is Number",
        "(number? (BigInteger \"1\"))", pass, fail);
    test_str_val(interp, "BigFloat ctor string",
        "(String (BigFloat \"1.25\"))",
        "1.25", pass, fail);
    test_truthy(interp, "BigFloat type-of",
        "(= (type-of (BigFloat \"1.25\")) 'BigFloat)", pass, fail);
    test_truthy(interp, "BigFloat is Number",
        "(number? (BigFloat \"1.25\"))", pass, fail);
    test_str_val(interp, "BigFloat addition",
        "(String (+ (BigFloat \"1.25\") 2))",
        "3.25", pass, fail);
    test_str_val(interp, "BigFloat subtraction",
        "(String (- (BigFloat \"5.5\") 2))",
        "3.5", pass, fail);
    test_str_val(interp, "BigFloat multiplication with BigInteger",
        "(String (* (BigFloat \"1.5\") (BigInteger \"4\")))",
        "6", pass, fail);
    test_str_val(interp, "BigFloat division",
        "(String (/ (BigFloat \"10\") 4))",
        "2.5", pass, fail);
    test_error_contains(interp, "BigFloat division by zero",
        "(/ (BigFloat \"1\") 0)", "/: division by zero", pass, fail);
    test_truthy(interp, "BigFloat less-than",
        "(< (BigFloat \"1.25\") (BigFloat \"2.5\"))", pass, fail);
    test_str_val(interp, "BigFloat abs",
        "(String (abs (BigFloat \"-3.5\")))",
        "3.5", pass, fail);
    test_str_val(interp, "BigFloat max",
        "(String (max (BigFloat \"1.25\") 2))",
        "2", pass, fail);
    test_truthy(interp, "parse-number double overflow type is BigFloat",
        "(= (type-of (parse-number \"1e309\")) 'BigFloat)", pass, fail);
    test_str_val(interp, "parse-number double overflow value",
        "(String (parse-number \"1e309\"))",
        "1e+309", pass, fail);
    test_str_val(interp, "Double rejects out-of-range BigFloat payload code",
        "(handle (Double (BigFloat \"1e309\")) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "BigInteger + overflow promotion",
        "(String (+ 9223372036854775807 1))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "BigInteger subtraction",
        "(String (- (BigInteger \"9223372036854775808\") 1))",
        "9223372036854775807", pass, fail);
    test_str_val(interp, "BigInteger multiplication",
        "(String (* (BigInteger \"9223372036854775808\") 2))",
        "18446744073709551616", pass, fail);
    test_str_val(interp, "BigInteger division",
        "(String (/ (BigInteger \"18446744073709551616\") 2))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "BigInteger modulo",
        "(String (% (BigInteger \"9223372036854775810\") 3))",
        "1", pass, fail);
    test_str_val(interp, "BigInteger abs",
        "(String (abs (BigInteger \"-9223372036854775808\")))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "Integer abs overflow promotes",
        "(String (abs (Integer \"-9223372036854775808\")))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "BigInteger min",
        "(String (min (BigInteger \"9223372036854775808\") (BigInteger \"9223372036854775809\")))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "BigInteger max",
        "(String (max (BigInteger \"9223372036854775808\") 1))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "BigInteger gcd",
        "(String (gcd (BigInteger \"18446744073709551616\") 24))",
        "8", pass, fail);
    test_str_val(interp, "BigInteger lcm",
        "(String (lcm (BigInteger \"9223372036854775808\") 3))",
        "27670116110564327424", pass, fail);
    test_str_val(interp, "Integer division overflow promotes",
        "(String (/ (- -9223372036854775807 1) -1))",
        "9223372036854775808", pass, fail);
    test_eq(interp, "Integer modulo overflow boundary returns zero",
        "(% (- -9223372036854775807 1) -1)",
        0, pass, fail);
    test_eq(interp, "Integer gcd long-min boundary narrows when possible",
        "(gcd (Integer \"-9223372036854775808\") 2)",
        2, pass, fail);
    test_str_val(interp, "Integer lcm long-min boundary promotes",
        "(String (lcm (Integer \"-9223372036854775808\") 2))",
        "9223372036854775808", pass, fail);
    test_error_contains(interp, "BigInteger division by zero",
        "(/ (BigInteger \"10\") 0)", "/: division by zero", pass, fail);
    test_error_contains(interp, "BigInteger modulo by zero",
        "(% (BigInteger \"10\") 0)", "%: division by zero", pass, fail);
    test_truthy(interp, "BigInteger less-than",
        "(< (BigInteger \"9223372036854775808\") (BigInteger \"9223372036854775809\"))", pass, fail);
    test_truthy(interp, "BigInteger greater-than Integer",
        "(> (BigInteger \"9223372036854775808\") 9223372036854775807)", pass, fail);
    test_truthy(interp, "BigInteger less-equal equality",
        "(<= (BigInteger \"9223372036854775808\") (BigInteger \"9223372036854775808\"))", pass, fail);
    test_truthy(interp, "BigInteger greater-equal equality",
        "(>= (BigInteger \"9223372036854775808\") (BigInteger \"9223372036854775808\"))", pass, fail);
    test_truthy(interp, "BigInteger compare with Double in range",
        "(< (BigInteger \"42\") 42.5)", pass, fail);
    test_eq(interp, "Integer ctor accepts in-range BigInteger",
        "(Integer (BigInteger \"9223372036854775807\"))",
        9223372036854775807, pass, fail);
    test_str_val(interp, "Integer ctor overflow payload code",
        "(handle (Integer \"9223372036854775808\") (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer ctor underflow payload code",
        "(handle (Integer \"-9223372036854775809\") (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer ctor huge-double payload code",
        "(handle (Integer (pow 10.0 400.0)) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer huge-double payload code",
        "(handle (Integer (pow 10.0 400.0)) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "BigInteger invalid string payload code",
        "(handle (BigInteger \"12x\") (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer rejects out-of-range BigInteger payload code",
        "(handle (Integer (BigInteger \"9223372036854775808\")) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str(interp, "String double", "(String 3.14)", pass, fail);
    test_error_contains(interp, "String rejects trailing arg", "(String 42 99)", "arity mismatch", pass, fail);
    test_str_val(interp, "String invalid array payload domain",
        "(handle (String [1]) (raise msg (String (ref msg 'domain))))",
        "type", pass, fail);
    test_str_val(interp, "String invalid array payload code",
        "(handle (String [1]) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "read-string parse failure payload code",
        "(handle (read-string \"(+ 1\") (raise msg (String (ref msg 'code))))",
        "runtime/read-string-failed", pass, fail);

    // === MATH LIBRARY TESTS (Phase 2) ===
    test_double(interp, "sin 0", "(sin 0.0)", 0.0, pass, fail);
    test_double(interp, "cos 0", "(cos 0.0)", 1.0, pass, fail);
    test_double(interp, "tan 0", "(tan 0.0)", 0.0, pass, fail);
    test_double(interp, "asin 0", "(asin 0.0)", 0.0, pass, fail);
    test_double(interp, "acos 1", "(acos 1.0)", 0.0, pass, fail);
    test_double(interp, "atan 0", "(atan 0.0)", 0.0, pass, fail);
    test_double(interp, "atan2", "(atan2 1.0 1.0)", 0.7853981, pass, fail);
    test_double(interp, "exp 0", "(exp 0.0)", 1.0, pass, fail);
    test_double(interp, "log 1", "(log 1.0)", 0.0, pass, fail);
    test_double(interp, "sqrt 4", "(sqrt 4.0)", 2.0, pass, fail);
    test_double(interp, "pow 2 10", "(pow 2.0 10.0)", 1024.0, pass, fail);
    test_double(interp, "math/lgamma 6", "(math/lgamma 6.0)", 4.78749174278, pass, fail);
    test_double(interp, "math/lgamma half", "(math/lgamma 0.5)", 0.57236494292, pass, fail);
    test_error_contains(interp, "math/lgamma pole domain error",
        "(math/lgamma 0.0)", "math/lgamma: domain error", pass, fail);
    test_error_contains(interp, "math/lgamma rejects out-of-range BigInteger",
        "(math/lgamma (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "math/lgamma: value out of Double range", pass, fail);
    test_double(interp, "math/erf 1", "(math/erf 1.0)", 0.84270079295, pass, fail);
    test_double(interp, "math/erfc 1", "(math/erfc 1.0)", 0.15729920705, pass, fail);
    test_error_contains(interp, "math/erf rejects out-of-range BigInteger",
        "(math/erf (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "math/erf: value out of Double range", pass, fail);
    test_double(interp, "stats/normal-cdf 0", "(stats/normal-cdf 0.0)", 0.5, pass, fail);
    test_double(interp, "stats/normal-cdf 1.96", "(stats/normal-cdf 1.96)", 0.97500210485, pass, fail);
    test_double(interp, "stats/normal-quantile median", "(stats/normal-quantile 0.5)", 0.0, pass, fail);
    test_double(interp, "stats/normal-quantile 0.975", "(stats/normal-quantile 0.975)", 1.95996398454, pass, fail);
    test_error_contains(interp, "stats/normal-quantile rejects zero probability",
        "(stats/normal-quantile 0.0)", "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
    test_error_contains(interp, "stats/normal-cdf rejects out-of-range BigInteger",
        "(stats/normal-cdf (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "stats/normal-cdf: value out of Double range", pass, fail);
    test_str_val(interp, "BigFloat sin zero",
        "(String (sin (BigFloat \"0\")))", "0", pass, fail);
    test_str_val(interp, "BigFloat cos zero",
        "(String (cos (BigFloat \"0\")))", "1", pass, fail);
    test_str_val(interp, "BigFloat exp zero",
        "(String (exp (BigFloat \"0\")))", "1", pass, fail);
    test_str_val(interp, "BigFloat sqrt exact",
        "(String (sqrt (BigFloat \"4\")))", "2", pass, fail);
    test_str_val(interp, "BigFloat pow exact",
        "(String (pow (BigFloat \"2\") 10))", "1024", pass, fail);
    test_str_val(interp, "BigFloat atan2 zero",
        "(String (atan2 (BigFloat \"0\") 1))", "0", pass, fail);
    test_str_val(interp, "BigFloat normal cdf median",
        "(String (stats/normal-cdf (BigFloat \"0\")))", "0.5", pass, fail);
    test_str_val(interp, "BigFloat normal quantile median",
        "(String (stats/normal-quantile (BigFloat \"0.5\")))", "0", pass, fail);
    test_str_val(interp, "BigFloat erfc zero",
        "(String (math/erfc (BigFloat \"0\")))", "1", pass, fail);
    test_truthy(interp, "BigFloat lgamma precision",
        "(< (abs (- (math/lgamma (BigFloat \"6\")) (BigFloat \"4.787491742782045994247700934523243048399592315172\"))) (BigFloat \"1e-45\"))",
        pass, fail);
    test_truthy(interp, "BigFloat exp beyond Double range stays BigFloat",
        "(= (type-of (exp (BigFloat \"1000\"))) 'BigFloat)", pass, fail);
    test_error_contains(interp, "BigFloat normal quantile rejects zero probability",
        "(stats/normal-quantile (BigFloat \"0\"))",
        "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
    test_eq(interp, "floor 3.7", "(floor 3.7)", 3, pass, fail);
    test_eq(interp, "ceiling 3.2", "(ceiling 3.2)", 4, pass, fail);
    test_eq(interp, "round 3.5", "(round 3.5)", 4, pass, fail);
    test_eq(interp, "truncate 3.9", "(truncate 3.9)", 3, pass, fail);
    test_eq(interp, "truncate -3.9", "(truncate -3.9)", -3, pass, fail);
    test_eq(interp, "BigFloat floor narrows to Integer",
        "(floor (BigFloat \"3.7\"))", 3, pass, fail);
    test_eq(interp, "BigFloat ceiling narrows to Integer",
        "(ceiling (BigFloat \"3.2\"))", 4, pass, fail);
    test_eq(interp, "BigFloat round negative half away from zero",
        "(round (BigFloat \"-3.5\"))", -4, pass, fail);
    test_eq(interp, "BigFloat truncate negative toward zero",
        "(truncate (BigFloat \"-3.9\"))", -3, pass, fail);
    test_str_val(interp, "BigFloat floor promotes large integer result",
        "(String (floor (BigFloat \"9223372036854775808.9\")))",
        "9223372036854775808", pass, fail);
    test_truthy(interp, "BigFloat floor large result is BigInteger",
        "(= (type-of (floor (BigFloat \"9223372036854775808.9\"))) 'BigInteger)", pass, fail);
    test_error_contains(interp, "BigFloat floor rejects huge integer allocation",
        "(floor (BigFloat \"1e400000\"))",
        "floor: BigFloat integer result out of supported range", pass, fail);
    test_error_contains(interp, "truncate overflow raises",
        "(truncate (pow 10.0 400.0))", "out of Integer range", pass, fail);
    test_eq(interp, "abs -5", "(abs -5)", 5, pass, fail);
    test_double(interp, "abs -3.14", "(abs -3.14)", 3.14, pass, fail);
    test_eq(interp, "min 3 7", "(min 3 7)", 3, pass, fail);
    test_eq(interp, "max 3 7", "(max 3 7)", 7, pass, fail);
    test_eq(interp, "gcd 12 8", "(gcd 12 8)", 4, pass, fail);
    test_double(interp, "pi constant", "pi", 3.14159265, pass, fail);
    test_double(interp, "e constant", "e", 2.71828182, pass, fail);
}

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
    test_error(interp, "ref oob", "(ref (Array 1) 5)", pass, fail);
    test_error(interp, "set! array oob", "(set! (Array 1) 5 0)", pass, fail);
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
    test_str(interp, "string-replace", "(string-replace \"hello world\" \"world\" \"there\")", pass, fail);
    test_str(interp, "char-at", "(char-at \"hello\" 1)", pass, fail);
    test_str(interp, "string-repeat", "(string-repeat \"ab\" 3)", pass, fail);
    test_error_contains(interp, "string-join rejects non-string element",
        "(string-join \",\" (quote (\"a\" 1 \"b\")))", "string-join: expected string elements", pass, fail);

    // === TYPE PREDICATE TESTS (Phase 5) ===
    test_truthy(interp, "double? yes", "(double? 3.14)", pass, fail);
    test_nil(interp, "double? no", "(double? 42)", pass, fail);
    test_truthy(interp, "number? int", "(number? 42)", pass, fail);
    test_truthy(interp, "number? double", "(number? 3.14)", pass, fail);
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

fn void run_advanced_stdlib_introspection_lazy_tco_phase_tests(Interp* interp, int* pass, int* fail,
    bool do_introspection, bool do_lazy_force, bool do_tco) {
    if (do_introspection) {
        // === INTROSPECTION TESTS (Phase 7) ===
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco setup-macro");
        setup(interp, "(define [macro] my-inc (syntax-match ([x] (template (+ (insert x) 1)))))");
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco macroexpand");
        test_truthy(interp, "macroexpand", "(pair? (macroexpand (quote (my-inc 5))))", pass, fail);
        test_error_contains(interp, "macroexpand surfaces improper form conversion error", "(macroexpand (cons 'my-inc (cons 1 2)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco eval-literal");
        test_eq(interp, "eval literal", "(eval 42)", 42, pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco eval-list");
        test_eq(interp, "eval list", "(eval (quote (+ 1 2)))", 3, pass, fail);
        test_eq(interp, "eval lambda preserves implicit block body", "((eval '(lambda (x) (set! x 2) x)) 1)",
            2, pass, fail);
        test_eq(interp, "eval let preserves implicit block body", "(eval '(let (x 1) (set! x 2) x))",
            2, pass, fail);
        test_error_contains(interp, "eval rejects improper call form", "(eval (cons '+ (cons 1 2)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects improper and form", "(eval (cons 'and (cons true false)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects improper lambda params", "(eval (list 'lambda (cons 'x 'y) 'x))",
            "value->expr: expected lambda parameters to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects short if form", "(eval '(if true 1))",
            "value->expr: if expects exactly 3 arguments", pass, fail);
        test_error_contains(interp, "eval rejects long if form", "(eval '(if true 1 2 3))",
            "value->expr: if expects exactly 3 arguments", pass, fail);
        test_error_contains(interp, "eval rejects long quote form", "(eval '(quote a b))",
            "value->expr: quote expects exactly 1 argument", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol define name", "(eval '(define 42 \"x\"))",
            "value->expr: define name must be a symbol", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol set name", "(eval '(set! 42 1))",
            "value->expr: set! name must be a symbol", pass, fail);
        test_eq(interp, "eval lowers generic set call", "(block (define eval-set-d (Dictionary 'a 1)) (eval '(set! eval-set-d 'b 2)) (ref eval-set-d 'b))",
            2, pass, fail);
        test_error_contains(interp, "eval rejects long checkpoint form", "(eval '(checkpoint 1 2))",
            "value->expr: checkpoint expects exactly 1 argument", pass, fail);
        test_error_contains(interp, "eval rejects long capture form", "(eval '(capture k 1 2))",
            "value->expr: capture expects continuation name and body", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol signal tag", "(eval '(signal 42 1))",
            "value->expr: signal tag must be a symbol", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco apply");
        test_eq(interp, "apply +", "(apply + (quote (1 2)))", 3, pass, fail);
        test_error_contains(interp, "apply rejects non-list args", "(apply + 1)",
            "apply: expected argument list", pass, fail);
        test_error_contains(interp, "apply rejects improper args", "(apply + (cons 1 2))",
            "apply: expected a proper argument list", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco bound-yes");
        test_truthy(interp, "bound? yes", "(bound? '+)", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco bound-no");
        test_nil(interp, "bound? no", "(bound? 'xyzzy-undefined)", pass, fail);
        test_truthy(interp, "bound? Dictionary canonical", "(bound? 'Dictionary)", pass, fail);
        test_truthy(interp, "bound? Dict shorthand", "(bound? 'Dict)", pass, fail);
        test_nil(interp, "bound? dictionary removed", "(bound? 'dictionary)", pass, fail);
        test_nil(interp, "bound? dict removed", "(bound? 'dict)", pass, fail);
        test_truthy(interp, "bound? String canonical", "(bound? 'String)", pass, fail);
        test_nil(interp, "bound? number->string removed", "(bound? 'number->string)", pass, fail);
        test_truthy(interp, "bound? Array canonical", "(bound? 'Array)", pass, fail);
        test_nil(interp, "bound? array removed", "(bound? 'array)", pass, fail);
        test_truthy(interp, "bound? Coroutine canonical", "(bound? 'Coroutine)", pass, fail);
        test_nil(interp, "bound? coroutine removed", "(bound? 'coroutine)", pass, fail);
        test_truthy(interp, "bound? TimePoint canonical", "(bound? 'TimePoint)", pass, fail);
        test_nil(interp, "bound? time-point removed", "(bound? 'time-point)", pass, fail);
        test_nil(interp, "bound? dict-set! removed", "(bound? 'dict-set!)", pass, fail);
        test_nil(interp, "bound? array-set! removed", "(bound? 'array-set!)", pass, fail);
        test_nil(interp, "bound? iterator removed", "(bound? 'iterator)", pass, fail);
        test_nil(interp, "bound? make-iterator removed", "(bound? 'make-iterator)", pass, fail);
        test_nil(interp, "bound? collect removed", "(bound? 'collect)", pass, fail);
        test_nil(interp, "bound? to-array removed", "(bound? 'to-array)", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco error");
        test_error(interp, "error raises", "(error \"test error\")", pass, fail);
    }

    if (do_lazy_force) {
        // === LAZY EVALUATION TESTS (Phase 8) ===
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco setup-delay");
    setup(interp, "(define p (delay (lambda (xx) 42)))");
    SymbolId p_sym = interp.symbols.intern("p");
    Value* p_val = interp.global_env.lookup(p_sym);
    bool p_is_closure = p_val != null && p_val.tag == CLOSURE && p_val.closure_val != null;
    Env* p_env = p_is_closure ? p_val.closure_val.env : null;
    bool p_env_is_global = p_is_closure && p_val.closure_val.env == interp.global_env;
    bool p_env_in_current = p_is_closure && boundary_ptr_in_scope((void*)p_val.closure_val.env, interp.current_scope);
    bool p_env_scope_in_current = p_is_closure &&
        p_val.closure_val.env_scope != null &&
        p_val.closure_val.env_scope == interp.current_scope;
    bool p_env_parent_self = p_env != null && p_env.parent == p_env;
    bool p_env_parent_global = p_env != null && p_env.parent == interp.global_env;
    bool p_env_in_detached_scope = p_is_closure &&
        p_val.closure_val.env_scope != null &&
        boundary_ptr_in_scope((void*)p_env, p_val.closure_val.env_scope);
    bool p_env_cycle = false;
    bool p_env_hits_global = false;
    int p_env_depth = 0;
    SymbolId thunk_sym = interp.symbols.intern("thunk");
    SymbolId result_sym = interp.symbols.intern("result");
    SymbolId forced_sym = interp.symbols.intern("forced");
    Value* thunk_val = p_env != null ? p_env.lookup(thunk_sym) : null;
    Value* result_slot_val = p_env != null ? p_env.lookup(result_sym) : null;
    Value* forced_slot_val = p_env != null ? p_env.lookup(forced_sym) : null;
    Env*[8] p_seen = {};
    Env* p_walk = p_env;
    while (p_walk != null && p_env_depth < 8) {
        for (int i = 0; i < p_env_depth; i++) {
            if (p_seen[i] == p_walk) {
                p_env_cycle = true;
                p_walk = null;
                break;
            }
        }
        if (p_walk == null) break;
        p_seen[p_env_depth] = p_walk;
        if (p_walk == interp.global_env) {
            p_env_hits_global = true;
            p_env_depth++;
            break;
        }
        p_walk = p_walk.parent;
        p_env_depth++;
    }
    if (advanced_group_trace_enabled()) {
        std::io::printfn(
            "OMNI_ADVANCED_GROUP_TRACE state delay-force p_is_closure=%d p_env_scope=%d p_env_is_global=%d p_env_in_current=%d p_env_scope_in_current=%d p_env_in_detached_scope=%d p_env_parent_self=%d p_env_parent_global=%d p_env_cycle=%d p_env_hits_global=%d p_env_depth=%d thunk_sym=%d result_sym=%d forced_sym=%d thunk_tag=%d thunk_scope_gen=%d thunk_env_scope=%d thunk_env_scope_gen=%d result_tag=%d forced_tag=%d raise_pending=%d handler_count=%d p_env_scope_gen=%d current_gen=%d current_escape_gen=%d env_scope_gen=%d",
            p_is_closure ? 1 : 0,
            p_is_closure && p_val.closure_val.env_scope != null ? 1 : 0,
            p_env_is_global ? 1 : 0,
            p_env_in_current ? 1 : 0,
            p_env_scope_in_current ? 1 : 0,
            p_env_in_detached_scope ? 1 : 0,
            p_env_parent_self ? 1 : 0,
            p_env_parent_global ? 1 : 0,
            p_env_cycle ? 1 : 0,
            p_env_hits_global ? 1 : 0,
            p_env_depth,
            (int)thunk_sym,
            (int)result_sym,
            (int)forced_sym,
            thunk_val != null ? (int)thunk_val.tag : -1,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null ? (int)thunk_val.scope_gen : -1,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null && thunk_val.closure_val.env_scope != null ? 1 : 0,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null && thunk_val.closure_val.env_scope != null ? (int)thunk_val.closure_val.env_scope.generation : -1,
            result_slot_val != null ? (int)result_slot_val.tag : -1,
            forced_slot_val != null ? (int)forced_slot_val.tag : -1,
            interp.flags.raise_pending ? 1 : 0,
            (int)interp.handler_count,
            p_env != null ? (int)p_env.scope_gen : -1,
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            interp.current_scope != null ? (int)interp.current_scope.escape_generation : -1,
            p_is_closure && p_val.closure_val.env_scope != null ? (int)p_val.closure_val.env_scope.generation : -1
        );
    }
    trace_env_chain_state("delay-force-before", interp, p_env);
    if (advanced_group_trace_enabled() && p_env != null && p_env.binding_count >= 3) {
        std::io::printfn(
            "OMNI_ADVANCED_GROUP_TRACE state delay-force env-bindings-before count=%d b0_name=%d b0_tag=%d b1_name=%d b1_tag=%d b2_name=%d b2_tag=%d",
            (int)p_env.binding_count,
            (int)p_env.bindings[0].name,
            p_env.bindings[0].value != null ? (int)p_env.bindings[0].value.tag : -1,
            (int)p_env.bindings[1].name,
            p_env.bindings[1].value != null ? (int)p_env.bindings[1].value.tag : -1,
            (int)p_env.bindings[2].name,
            p_env.bindings[2].value != null ? (int)p_env.bindings[2].value.tag : -1
        );
    }
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force");
    bool saved_jit_enabled = interp.flags.jit_enabled;
    interp.flags.jit_enabled = false;
    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);

    EvalResult force_interp;
    Lexer force_lex;
    force_lex.init("(force p)");
    defer force_lex.destroy();
    Parser force_parser;
    force_parser.init(&force_lex, interp);
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-parse");
    Expr* force_expr = force_parser.parse_expr();
    if (force_parser.has_error) {
        force_interp = parser_error_to_eval_result(&force_parser);
    } else {
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-expand");
        force_expr = expand_macros_in_expr(force_expr, interp);
        main::ScopeRegion* force_saved_scope = null;
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-push-child");
        main::ScopeRegion* force_child_scope = boundary_push_child_scope(interp, &force_saved_scope);
        if (force_child_scope == null) {
            force_interp = eval_error("force: failed to create child scope");
        } else {
            bool force_child_spliced = false;
            Value* force_value = null;
            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-execute");
            bool force_eval_ok = false;
            if (force_expr != null && force_expr.tag == E_CALL && force_expr.call.arg_count == 1) {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-eval-func");
                Value* force_func = runtime_eval_expr(force_expr.call.func, interp.global_env, interp);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-func");
                if (force_func != null && force_func.tag == ERROR) {
                    force_interp = eval_error_expr(force_func.str_chars[:force_func.str_len], force_expr.call.func);
                } else {
                    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-eval-arg");
                    Value* force_arg = runtime_eval_expr(force_expr.call.args[0], interp.global_env, interp);
                    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-arg");
                    if (force_arg != null && force_arg.tag == ERROR) {
                        force_interp = eval_error_expr(force_arg.str_chars[:force_arg.str_len], force_expr.call.args[0]);
                    } else {
                        if (advanced_group_trace_enabled()) {
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force arg_same_value=%d arg_is_closure=%d arg_env_same=%d arg_env_scope_same=%d",
                                force_arg == p_val ? 1 : 0,
                                force_arg != null && force_arg.tag == CLOSURE && force_arg.closure_val != null ? 1 : 0,
                                force_arg != null &&
                                    force_arg.tag == CLOSURE &&
                                    force_arg.closure_val != null &&
                                    force_arg.closure_val.env == p_env ? 1 : 0,
                                force_arg != null &&
                                    force_arg.tag == CLOSURE &&
                                    force_arg.closure_val != null &&
                                    force_arg.closure_val.env_scope == p_val.closure_val.env_scope ? 1 : 0
                            );
                            if (p_env != null && p_env.binding_count >= 3) {
                                std::io::printfn(
                                    "OMNI_ADVANCED_GROUP_TRACE state delay-force env-bindings-after-arg count=%d b0_name=%d b0_tag=%d b1_name=%d b1_tag=%d b2_name=%d b2_tag=%d",
                                    (int)p_env.binding_count,
                                    (int)p_env.bindings[0].name,
                                    p_env.bindings[0].value != null ? (int)p_env.bindings[0].value.tag : -1,
                                    (int)p_env.bindings[1].name,
                                    p_env.bindings[1].value != null ? (int)p_env.bindings[1].value.tag : -1,
                                    (int)p_env.bindings[2].name,
                                    p_env.bindings[2].value != null ? (int)p_env.bindings[2].value.tag : -1
                                );
                            }
                        }
                        if (advanced_group_trace_enabled() &&
                            force_arg != null &&
                            force_arg.tag == CLOSURE &&
                            force_arg.closure_val != null &&
                            force_arg.closure_val.env != null) {
                            trace_env_chain_state("delay-force-after-arg", interp, force_arg.closure_val.env);
                            Value* direct_thunk = force_arg.closure_val.env.lookup(thunk_sym);
                            Value* direct_thunk_before = direct_thunk;
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force direct_thunk_before_tag=%d direct_thunk_before_scope_gen=%d direct_thunk_before_env_scope=%d direct_thunk_before_env_scope_gen=%d direct_thunk_same_as_initial=%d direct_thunk_in_current=%d direct_thunk_in_root=%d direct_thunk_payload_in_current=%d direct_thunk_payload_in_root=%d",
                                direct_thunk_before != null ? (int)direct_thunk_before.tag : -1,
                                direct_thunk_before != null ? (int)direct_thunk_before.scope_gen : -1,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    direct_thunk_before.closure_val.env_scope != null ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    direct_thunk_before.closure_val.env_scope != null ? (int)direct_thunk_before.closure_val.env_scope.generation : -1,
                                direct_thunk_before == thunk_val ? 1 : 0,
                                direct_thunk_before != null && boundary_ptr_in_scope((void*)direct_thunk_before, interp.current_scope) ? 1 : 0,
                                direct_thunk_before != null && boundary_ptr_in_scope((void*)direct_thunk_before, interp.root_scope) ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    boundary_ptr_in_scope((void*)direct_thunk_before.closure_val, interp.current_scope) ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    boundary_ptr_in_scope((void*)direct_thunk_before.closure_val, interp.root_scope) ? 1 : 0
                            );
                            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-direct-thunk-before-apply");
                            Value* direct_thunk_result = direct_thunk != null
                                ? jit_apply_value(direct_thunk, make_nil(interp), interp)
                                : null;
                            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-direct-thunk-after-apply");
                            Value* direct_thunk_after = force_arg.closure_val.env.lookup(thunk_sym);
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force direct_thunk_tag=%d direct_thunk_after_tag=%d direct_thunk_after_same_ptr=%d direct_thunk_result_tag=%d direct_thunk_result_int=%d",
                                direct_thunk != null ? (int)direct_thunk.tag : -1,
                                direct_thunk_after != null ? (int)direct_thunk_after.tag : -1,
                                direct_thunk_after == direct_thunk_before ? 1 : 0,
                                direct_thunk_result != null ? (int)direct_thunk_result.tag : -1,
                                direct_thunk_result != null && direct_thunk_result.tag == INT ? (int)direct_thunk_result.int_val : -1
                            );
                        }
                        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-apply");
                        force_value = jit_apply_value(force_func, force_arg, interp);
                        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-apply");
                        force_eval_ok = true;
                    }
                }
            } else if (!run_execute_expr(force_expr, interp, &force_value, &force_interp)) {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-execute-error");
            } else {
                force_eval_ok = true;
            }

            if (!force_eval_ok) {
                boundary_pop_child_scope(interp, force_saved_scope, force_child_scope);
            } else {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-execute");
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-promote");
                force_value = run_promote_result(force_value, interp, force_child_scope, force_saved_scope, &force_child_spliced);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-promote");
                main::ScopeRegion* force_release_scope = force_child_spliced ? null : force_child_scope;
                if (force_value != null && force_value.tag == ERROR) {
                    force_interp = eval_error_expr(force_value.str_chars[:force_value.str_len], force_expr);
                } else {
                    force_interp = eval_ok(force_value);
                }
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-pop");
                boundary_pop_child_scope(interp, force_saved_scope, force_release_scope);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-pop");
                dispatch_pending_signal_callbacks(interp);
            }
        }
    }
    interp.flags.jit_enabled = saved_jit_enabled;
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-interp");
    bool force_interp_ok =
        !force_interp.error.has_error &&
        is_int(force_interp.value) &&
        force_interp.value.int_val == 42;

    bool force_jit_ok = !jit_checks_enabled();
    char[] force_jit_stage = "disabled";
    Value* force_jit_value = null;
    if (jit_checks_enabled()) {
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-jit");
        JitTopLevelEval force_jit = eval_jit_top_level(interp, "(force p)");
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-jit");
        force_jit_stage = force_jit.stage;
        force_jit_value = force_jit.value;
        force_jit_ok = force_jit.ok &&
            force_jit_value != null &&
            force_jit_value.tag == INT &&
            force_jit_value.int_val == 42;
    }

    if (force_interp_ok && force_jit_ok) {
        emit_pass("delay/force");
        (*pass)++;
    } else {
        std::io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)"delay/force",
            force_interp_ok ? "ok" : "FAIL",
            force_jit_ok ? "ok" : "FAIL");
        if (!force_jit_ok && jit_checks_enabled()) {
            print_jit_failure_debug(interp, "delay/force", force_jit_stage, force_jit_value);
        }
        (*fail)++;
    }
    }

    if (do_tco) {
        run_advanced_stdlib_tco_tests(interp, pass, fail);
    }
}

fn void run_advanced_stdlib_introspection_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_stdlib_introspection_lazy_tco_phase_tests(interp, pass, fail, true, false, false);
}

fn void run_advanced_stdlib_lazy_force_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_stdlib_introspection_lazy_tco_phase_tests(interp, pass, fail, false, true, false);
}

fn void run_advanced_stdlib_introspection_lazy_tco_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_stdlib_introspection_lazy_tco_phase_tests(interp, pass, fail, true, true, true);
}

fn void run_advanced_stdlib_tco_tests(Interp* interp, int* pass, int* fail) {
    long range_tco_target = main::stack_runtime_asan_enabled() ? 2000 : 4000;
    char[] range_tco_expr = main::stack_runtime_asan_enabled()
        ? "(length (range 2000))"
        : "(length (range 4000))";
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-tco range-length-before-interp");
    EvalResult range_interp = run(range_tco_expr, interp);
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-tco range-length-after-interp");
    bool range_interp_ok =
        !range_interp.error.has_error &&
        is_int(range_interp.value) &&
        range_interp.value.int_val == range_tco_target;

    bool range_jit_ok = !jit_checks_enabled();
    char[] range_jit_stage = "disabled";
    Value* range_jit_value = null;
    if (jit_checks_enabled()) {
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-tco range-length-before-jit");
        JitTopLevelEval range_jit = eval_jit_top_level(interp, range_tco_expr);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-tco range-length-after-jit");
        range_jit_stage = range_jit.stage;
        range_jit_value = range_jit.value;
        range_jit_ok = range_jit.ok &&
            range_jit_value != null &&
            range_jit_value.tag == INT &&
            range_jit_value.int_val == range_tco_target;
    }

    if (range_interp_ok && range_jit_ok) {
        emit_pass("range length TCO headroom");
        (*pass)++;
    } else {
        std::io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)"range length TCO headroom",
            range_interp_ok ? "ok" : "FAIL",
            range_jit_ok ? "ok" : "FAIL");
        if (!range_jit_ok && jit_checks_enabled()) {
            print_jit_failure_debug(interp, "range length TCO headroom", range_jit_stage, range_jit_value);
        }
        (*fail)++;
    }
}

fn void run_advanced_stdlib_numeric_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-collections",
        &run_advanced_stdlib_numeric_collection_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-float-math",
        &run_advanced_stdlib_numeric_float_math_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-sort-bitwise-hof",
        &run_advanced_stdlib_sort_bitwise_hof_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-string-predicate-format",
        &run_advanced_stdlib_string_predicate_format_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-introspection-lazy-tco",
        &run_advanced_stdlib_introspection_lazy_tco_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_exact_group("advanced-stdlib-numeric-introspection",
        &run_advanced_stdlib_introspection_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_exact_group("advanced-stdlib-numeric-lazy-force",
        &run_advanced_stdlib_lazy_force_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_exact_group("advanced-stdlib-numeric-tco",
        &run_advanced_stdlib_tco_tests, interp, pass, fail);
    run_advanced_stdlib_numeric_group("advanced-stdlib-numeric-array-set",
        &run_advanced_stdlib_numeric_array_set_tests, interp, pass, fail);
}
