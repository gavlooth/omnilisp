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
    test_double(interp, "Float64", "(Float64 5)", 5.0, pass, fail);
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
    test_str_val(interp, "BigComplex constructor stringifies parts",
        "(String (BigComplex 1 2))",
        "1+2i", pass, fail);
    test_truthy(interp, "BigComplex type-of",
        "(= (type-of (BigComplex 1 2)) 'BigComplex)", pass, fail);
    test_truthy(interp, "BigComplex is Number",
        "(is? (BigComplex 1 2) 'Number)", pass, fail);
    test_str_val(interp, "BigComplex scalar addition",
        "(String (+ (BigComplex 1 2) 3))",
        "4+2i", pass, fail);
    test_str_val(interp, "BigComplex multiplication",
        "(String (* (BigComplex 1 2) (BigComplex 3 4)))",
        "-5+10i", pass, fail);
    test_str_val(interp, "BigComplex division",
        "(String (/ (BigComplex 1 2) (BigComplex 3 -4)))",
        "-0.2+0.4i", pass, fail);
    test_str_val(interp, "BigComplex abs returns BigFloat magnitude",
        "(String (abs (BigComplex 3 4)))",
        "5", pass, fail);
    test_str_val(interp, "BigComplex real-part returns BigFloat component",
        "(String (real-part (BigComplex 1 2)))",
        "1", pass, fail);
    test_str_val(interp, "BigComplex imag-part returns BigFloat component",
        "(String (imag-part (BigComplex 1 2)))",
        "2", pass, fail);
    test_str_val(interp, "BigComplex conjugate flips imaginary sign",
        "(String (conjugate (BigComplex 1 2)))",
        "1-2i", pass, fail);
    test_truthy(interp, "BigComplex real-part preserves BigFloat component type",
        "(= (type-of (real-part (BigComplex 1 2))) 'BigFloat)", pass, fail);
    test_eq(interp, "real-part on real Integer is identity",
        "(real-part 42)", 42, pass, fail);
    test_eq(interp, "imag-part on real Integer is zero",
        "(imag-part 42)", 0, pass, fail);
    test_str_val(interp, "real-part on BigFloat preserves scalar value",
        "(String (real-part (BigFloat \"1.25\")))",
        "1.25", pass, fail);
    test_str_val(interp, "conjugate on BigFloat preserves scalar value",
        "(String (conjugate (BigFloat \"1.25\")))",
        "1.25", pass, fail);
    test_error_contains(interp, "real-part rejects non-number",
        "(real-part \"x\")", "number", pass, fail);
    test_truthy(interp, "BigComplex equality",
        "(= (BigComplex 1 2) (BigComplex 1 2))", pass, fail);
    test_truthy(interp, "BigComplex zero predicate",
        "(zero? (BigComplex 0 0))", pass, fail);
    test_error_contains(interp, "BigComplex ordering rejected",
        "(< (BigComplex 1 2) (BigComplex 2 3))", "complex numbers are not ordered", pass, fail);
    test_error_contains(interp, "BigComplex division by zero",
        "(/ (BigComplex 1 2) (BigComplex 0 0))", "/: division by zero", pass, fail);
    test_str_val(interp, "BigComplex sin zero",
        "(String (sin (BigComplex 0 0)))",
        "0+0i", pass, fail);
    test_str_val(interp, "BigComplex cos zero",
        "(String (cos (BigComplex 0 0)))",
        "1+0i", pass, fail);
    test_str_val(interp, "BigComplex exp zero",
        "(String (exp (BigComplex 0 0)))",
        "1+0i", pass, fail);
    test_str_val(interp, "BigComplex sinh zero",
        "(String (sinh (BigComplex 0 0)))",
        "0+0i", pass, fail);
    test_str_val(interp, "BigComplex cosh zero",
        "(String (cosh (BigComplex 0 0)))",
        "1+0i", pass, fail);
    test_str_val(interp, "BigComplex tanh zero",
        "(String (tanh (BigComplex 0 0)))",
        "0+0i", pass, fail);
    test_truthy(interp, "BigComplex sinh preserves type",
        "(= (type-of (sinh (BigComplex 0 0))) 'BigComplex)", pass, fail);
    test_str_val(interp, "BigComplex log one",
        "(String (log (BigComplex 1 0)))",
        "0+0i", pass, fail);
    test_str_val(interp, "BigComplex sqrt negative one",
        "(String (sqrt (BigComplex -1 0)))",
        "0+1i", pass, fail);
    test_truthy(interp, "BigComplex pow within tolerance",
        "(< (abs (- (pow (BigComplex 0 1) 2) (BigComplex -1 0))) (BigFloat \"1e-40\"))",
        pass, fail);
    test_error_contains(interp, "BigComplex atan2 rejected",
        "(atan2 (BigComplex 1 0) 1)", "atan2: complex numbers are not supported", pass, fail);
    test_truthy(interp, "parse-number Float64 overflow type is BigFloat",
        "(= (type-of (parse-number \"1e309\")) 'BigFloat)", pass, fail);
    test_str_val(interp, "parse-number Float64 overflow value",
        "(String (parse-number \"1e309\"))",
        "1e+309", pass, fail);
    test_str_val(interp, "Float64 rejects out-of-range BigFloat payload code",
        "(handle (Float64 (BigFloat \"1e309\")) (raise msg (String (ref msg 'code))))",
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
    test_truthy(interp, "BigInteger compare with Float64 in range",
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
    test_str_val(interp, "Integer ctor huge-Float64 payload code",
        "(handle (Integer (pow 10.0 400.0)) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer huge-Float64 payload code",
        "(handle (Integer (pow 10.0 400.0)) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "BigInteger invalid string payload code",
        "(handle (BigInteger \"12x\") (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp, "Integer rejects out-of-range BigInteger payload code",
        "(handle (Integer (BigInteger \"9223372036854775808\")) (raise msg (String (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str(interp, "String Float64", "(String 3.14)", pass, fail);
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
    test_double(interp, "sinh 0", "(sinh 0.0)", 0.0, pass, fail);
    test_double(interp, "cosh 0", "(cosh 0.0)", 1.0, pass, fail);
    test_double(interp, "tanh 0", "(tanh 0.0)", 0.0, pass, fail);
    test_double(interp, "asin 0", "(asin 0.0)", 0.0, pass, fail);
    test_double(interp, "acos 1", "(acos 1.0)", 0.0, pass, fail);
    test_double(interp, "atan 0", "(atan 0.0)", 0.0, pass, fail);
    test_double(interp, "atan2", "(atan2 1.0 1.0)", 0.7853981, pass, fail);
    test_double(interp, "exp 0", "(exp 0.0)", 1.0, pass, fail);
    test_double(interp, "log 1", "(log 1.0)", 0.0, pass, fail);
    test_double(interp, "sqrt 4", "(sqrt 4.0)", 2.0, pass, fail);
    test_double(interp, "pow 2 10", "(pow 2.0 10.0)", 1024.0, pass, fail);
    test_double(interp, "math.lgamma 6", "(math.lgamma 6.0)", 4.78749174278, pass, fail);
    test_double(interp, "math.lgamma half", "(math.lgamma 0.5)", 0.57236494292, pass, fail);
    test_error_contains(interp, "math.lgamma pole domain error",
        "(math.lgamma 0.0)", "math.lgamma: domain error", pass, fail);
    test_error_contains(interp, "math.lgamma rejects out-of-range BigInteger",
        "(math.lgamma (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "math.lgamma: value out of Float64 range", pass, fail);
    test_double(interp, "math.erf 1", "(math.erf 1.0)", 0.84270079295, pass, fail);
    test_double(interp, "math.erfc 1", "(math.erfc 1.0)", 0.15729920705, pass, fail);
    test_error_contains(interp, "math.erf rejects out-of-range BigInteger",
        "(math.erf (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "math.erf: value out of Float64 range", pass, fail);
    test_double(interp, "stats.normal-cdf 0", "(stats.normal-cdf 0.0)", 0.5, pass, fail);
    test_double(interp, "stats.normal-cdf 1.96", "(stats.normal-cdf 1.96)", 0.97500210485, pass, fail);
    test_double(interp, "stats.normal-quantile median", "(stats.normal-quantile 0.5)", 0.0, pass, fail);
    test_double(interp, "stats.normal-quantile 0.975", "(stats.normal-quantile 0.975)", 1.95996398454, pass, fail);
    test_error_contains(interp, "stats.normal-quantile rejects zero probability",
        "(stats.normal-quantile 0.0)", "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
    test_error_contains(interp, "stats.normal-cdf rejects out-of-range BigInteger",
        "(stats.normal-cdf (BigInteger \"10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"))",
        "stats.normal-cdf: value out of Float64 range", pass, fail);
    test_str_val(interp, "BigFloat sin zero",
        "(String (sin (BigFloat \"0\")))", "0", pass, fail);
    test_str_val(interp, "BigFloat cos zero",
        "(String (cos (BigFloat \"0\")))", "1", pass, fail);
    test_str_val(interp, "BigFloat exp zero",
        "(String (exp (BigFloat \"0\")))", "1", pass, fail);
    test_str_val(interp, "BigFloat sinh zero",
        "(String (sinh (BigFloat \"0\")))", "0", pass, fail);
    test_str_val(interp, "BigFloat cosh zero",
        "(String (cosh (BigFloat \"0\")))", "1", pass, fail);
    test_str_val(interp, "BigFloat tanh zero",
        "(String (tanh (BigFloat \"0\")))", "0", pass, fail);
    test_truthy(interp, "BigFloat sinh preserves type",
        "(= (type-of (sinh (BigFloat \"0\"))) 'BigFloat)", pass, fail);
    test_str_val(interp, "BigFloat sqrt exact",
        "(String (sqrt (BigFloat \"4\")))", "2", pass, fail);
    test_str_val(interp, "BigFloat pow exact",
        "(String (pow (BigFloat \"2\") 10))", "1024", pass, fail);
    test_str_val(interp, "BigFloat atan2 zero",
        "(String (atan2 (BigFloat \"0\") 1))", "0", pass, fail);
    test_str_val(interp, "BigFloat normal cdf median",
        "(String (stats.normal-cdf (BigFloat \"0\")))", "0.5", pass, fail);
    test_str_val(interp, "BigFloat normal quantile median",
        "(String (stats.normal-quantile (BigFloat \"0.5\")))", "0", pass, fail);
    test_str_val(interp, "BigFloat erfc zero",
        "(String (math.erfc (BigFloat \"0\")))", "1", pass, fail);
    test_truthy(interp, "BigFloat lgamma precision",
        "(< (abs (- (math.lgamma (BigFloat \"6\")) (BigFloat \"4.787491742782045994247700934523243048399592315172\"))) (BigFloat \"1e-45\"))",
        pass, fail);
    test_truthy(interp, "BigFloat exp beyond Float64 range stays BigFloat",
        "(= (type-of (exp (BigFloat \"1000\"))) 'BigFloat)", pass, fail);
    test_error_contains(interp, "BigFloat normal quantile rejects zero probability",
        "(stats.normal-quantile (BigFloat \"0\"))",
        "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
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
