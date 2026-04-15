module lisp;


import std::io;
import main;

fn void run_advanced_collections_dict_literal_tests(Interp* interp, int* pass, int* fail) {
    // Dicts
    setup(interp, "(define hm1 (Dictionary 1 10 2 20 3 30))");
    test_eq(interp, "ref dict", "(ref hm1 2)", 20, pass, fail);
    test_nil(interp, "ref dict missing", "(ref hm1 99)", pass, fail);
    test_tag_interp(interp, "set! dict returns void", "(set! hm1 2 200)", VOID, pass, fail);
    test_eq(interp, "set! dict overwrite", "(ref hm1 2)", 200, pass, fail);
    test_tag_interp(interp, "set! generic dict returns void", "(set! hm1 2 250)", VOID, pass, fail);
    test_eq(interp, "set! generic dict overwrite", "(ref hm1 2)", 250, pass, fail);
    test_truthy(interp, "has? dict yes", "(has? hm1 1)", pass, fail);
    test_nil(interp, "has? dict no", "(has? hm1 99)", pass, fail);
    test_eq(interp, "length dict", "(length hm1)", 3, pass, fail);
    setup(interp, "(define hm2 (Dictionary 1 10 2 20 3 30))");
    test_tag_interp(interp, "remove! returns void", "(remove! hm2 2)", VOID, pass, fail);
    test_nil(interp, "remove! dict", "(has? hm2 2)", pass, fail);
    test_eq(interp, "remove! count", "(length hm2)", 2, pass, fail);
    setup(interp, "(define hm3 (Dictionary 1 10 2 20))");
    test_eq(interp, "keys len", "(length (keys hm3))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values hm3))", 2, pass, fail);
    test_truthy(interp, "keys canonical order symbol",
        "(= (keys (Dictionary 'b 2 'a 1 'c 3)) '(a b c))", pass, fail);
    test_truthy(interp, "values canonical follow keys",
        "(= (values (Dictionary 'b 2 'a 1 'c 3)) '(1 2 3))", pass, fail);
    test_truthy(interp, "keys canonical after remove+insert",
        "(let (d (Dictionary 'c 3 'a 1 'b 2)) (block (remove! d 'b) (set! d 'b 20) (= (keys d) '(a b c))))", pass, fail);
    test_truthy(interp, "values canonical after remove+insert",
        "(let (d (Dictionary 'c 3 'a 1 'b 2)) (block (remove! d 'b) (set! d 'b 20) (= (values d) '(1 20 3))))", pass, fail);
    setup(interp, "(define hm4 (Dictionary \"name\" \"Alice\" \"age\" 30))");
    test_str(interp, "dict string key", "(ref hm4 \"name\")", pass, fail);
    setup(interp, "(define hm5 (Dictionary))");
    test_eq(interp, "dict empty count", "(length hm5)", 0, pass, fail);
    test_truthy(interp, "dict? Dictionary", "(dict? (Dictionary 1 2))", pass, fail);
    test_truthy(interp, "dict? Dict shorthand", "(dict? (Dict 1 2))", pass, fail);
    test_eq(interp, "Dictionary canonical constructor", "(ref (Dictionary 1 10 2 20) 1)", 10, pass, fail);
    test_eq(interp, "Dict shorthand constructor", "(ref (Dict 1 10 2 20) 1)", 10, pass, fail);
    test_eq(interp, "Dict shorthand type annotation dispatch",
        "(block (define (dict-ann (^Dict d)) (ref d 'x)) (dict-ann (Dictionary 'x 5)))", 5, pass, fail);
    test_nil(interp, "dict? no", "(dict? 42)", pass, fail);
    setup(interp, "(define hm-big (Dictionary))");
    setup(interp, "(let loop (i 0) (if (= i 50) hm-big (block (set! hm-big i (* i i)) (loop (+ i 1)))))");
    test_eq(interp, "dict auto-grow count", "(length hm-big)", 50, pass, fail);
    test_eq(interp, "dict auto-grow ref 7", "(ref hm-big 7)", 49, pass, fail);

    // === LITERAL SYNTAX TESTS ===
    // Dictionary literal {}
    test_eq(interp, "{} empty", "(length {})", 0, pass, fail);
    test_eq(interp, "{} literal", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_truthy(interp, "{} string keys", "(has? {\"name\" \"Alice\"} \"name\")", pass, fail);
    test_truthy(interp, "{} has key", "(has? {'x 10 'y 20} 'y)", pass, fail);
    test_eq(interp, "{} count", "(length {'a 1 'b 2 'c 3})", 3, pass, fail);
    // Array literal []
    test_eq(interp, "[] empty", "(length [])", 0, pass, fail);
    test_eq(interp, "[] literal", "(ref [10 20 30] 1)", 20, pass, fail);
    test_eq(interp, "[] length", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "[] nested", "(ref (ref [1 [2 3]] 1) 0)", 2, pass, fail);
    test_eq(interp, "[] index", "(let (a [10 20 30]) a.[1])", 20, pass, fail);
    // Cons cell dot-path access and mutation
    test_eq(interp, "cons .car", "(let (p (cons 1 2)) p.car)", 1, pass, fail);
    test_eq(interp, "cons .cdr", "(let (p (cons 1 2)) p.cdr)", 2, pass, fail);
    test_tag(interp, "set! cons returns void", "(let (p (cons 1 2)) (set! p.car 99))", VOID, pass, fail);
    test_eq_interp(interp, "set! cons .car", "(let (p (cons 1 2)) (block (set! p.car 99) p.car))", 99, pass, fail);
    test_eq_interp(interp, "set! cons .cdr", "(let (p (cons 1 2)) (block (set! p.cdr 50) p.cdr))", 50, pass, fail);
    test_eq_interp(interp, "set! cons preserves", "(let (p (cons 1 2)) (block (set! p.car 99) p.cdr))", 2, pass, fail);
}

fn void run_advanced_collections_generic_ops_tests(Interp* interp, int* pass, int* fail) {
    // === GENERIC OPERATIONS TESTS ===
    // ref - array
    test_eq(interp, "ref array", "(ref [10 20 30] 1)", 20, pass, fail);
    test_error(interp, "ref array oob", "(ref [1] 5)", pass, fail);
    // ref - dict
    test_eq(interp, "ref dict", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_nil(interp, "ref dict missing", "(ref {'a 1} 'b)", pass, fail);
    // ref - cons
    test_eq(interp, "ref cons car", "(ref (cons 10 20) 0)", 10, pass, fail);
    test_eq(interp, "ref cons cdr", "(ref (cons 10 20) 1)", 20, pass, fail);
    test_eq(interp, "ref cons negative cdr", "(ref (cons 10 20) -1)", 20, pass, fail);
    // ref - string
    test_str_val(interp, "ref string", "(ref \"hello\" 0)", "h", pass, fail);
    // negative indexing (Python-style: -1 = last, -2 = second-to-last)
    test_eq(interp, "ref array -1", "(ref [10 20 30] -1)", 30, pass, fail);
    test_eq(interp, "ref array -2", "(ref [10 20 30] -2)", 20, pass, fail);
    test_eq(interp, "ref array -3", "(ref [10 20 30] -3)", 10, pass, fail);
    test_error(interp, "ref array -4 oob", "(ref [10 20 30] -4)", pass, fail);
    test_str_val(interp, "ref string -1", "(ref \"hello\" -1)", "o", pass, fail);
    test_str_val(interp, "ref string -5", "(ref \"hello\" -5)", "h", pass, fail);
    test_error(interp, "ref string -6 oob", "(ref \"hello\" -6)", pass, fail);
    // ref - tensor
    test_eq_double(interp, "ref tensor rank-0 scalar",
        "(ref (Tensor Double [] 3.0) [])",
        3.0, pass, fail);
    test_error_contains(interp, "ref tensor rank-0 scalar with vector index",
        "(ref (Tensor Double [] 3.0) [0])",
        "ref: tensor index rank mismatch", pass, fail);
    test_error_contains(interp, "ref tensor rank-0 scalar with scalar index",
        "(ref (Tensor Double [] 3.0) 0)",
        "ref: tensor requires array or proper list index", pass, fail);
    test_eq_double(interp, "ref tensor rank-2", "(ref (Tensor Double [2 3] [1 2 3 4 5 6]) [1 2])", 6.0, pass, fail);
    test_eq_double(interp, "ref tensor negative axis", "(ref (Tensor Double [2 3] [1 2 3 4 5 6]) [1 -1])", 6.0, pass, fail);
    test_eq_double(interp, "ref tensor list index", "(ref (Tensor Double '(2 3) '(1 2 3 4 5 6)) '(1 2))", 6.0, pass, fail);
    test_eq_double(interp, "Tensor inferred vector ref",
        "(ref (Tensor [1 2 3]) [2])", 3.0, pass, fail);
    test_str_val(interp, "Tensor inferred matrix shape",
        "(format \"%s\" (shape (Tensor [[1 2] [3 4]])))", "[2 2]", pass, fail);
    test_eq_double(interp, "Tensor inferred matrix row-major ref",
        "(ref (Tensor [[1 2] [3 4]]) [1 0])", 3.0, pass, fail);
    test_eq_double(interp, "Tensor inferred scalar rank-0",
        "(ref (Tensor 7) [])", 7.0, pass, fail);
    test_eq_double(interp, "Tensor inferred data dtype suffix",
        "(ref (Tensor [1 2] Double) [1])", 2.0, pass, fail);
    test_eq_double(interp, "Tensor inferred data dtype prefix",
        "(ref (Tensor Double [1 2]) [1])", 2.0, pass, fail);
    test_eq_double(interp, "Tensor inferred quoted dtype suffix",
        "(ref (Tensor [1 2] 'Double) [1])", 2.0, pass, fail);
    test_eq_double(interp, "Tensor inferred BigInteger narrows to Double",
        "(ref (Tensor [(BigInteger \"9223372036854775808\")]) [0])",
        9223372036854775808.0, pass, fail);
    test_eq_double(interp, "Tensor inferred BigFloat narrows to Double",
        "(ref (Tensor [(BigFloat \"1.25\")]) [0])", 1.25, pass, fail);
    test_eq_double(interp, "Tensor explicit BigFloat scalar fill narrows",
        "(ref (Tensor Double [2] (BigFloat \"2.5\")) [1])", 2.5, pass, fail);
    test_eq_double(interp, "Tensor explicit BigInteger data narrows",
        "(ref (Tensor Double [1] [(BigInteger \"42\")]) [0])", 42.0, pass, fail);
    test_eq(interp, "Tensor inferred empty vector length",
        "(length (Tensor []))", 0, pass, fail);
    test_error_contains(interp, "Tensor inferred BigFloat outside Double range rejected",
        "(Tensor [(BigFloat \"1e309\")])", "representable as Double", pass, fail);
    test_error_contains(interp, "Tensor inferred complex data rejected",
        "(Tensor [(BigComplex 1 2)])", "representable as Double", pass, fail);
    test_error_contains(interp, "Tensor inferred ragged data rejected",
        "(Tensor [[1] [2 3]])", "rectangular", pass, fail);
    test_error_contains(interp, "Tensor inferred non-numeric data rejected",
        "(Tensor [1 'bad])", "nested arrays/lists of numbers", pass, fail);
    test_str_val(interp, "Tensor BigInteger dtype",
        "(format \"%s\" (dtype (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])))", "BigInteger", pass, fail);
    test_str_val(interp, "Tensor BigInteger explicit ref preserves exact range",
        "(String (ref (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) [0]))", "9223372036854775808", pass, fail);
    test_str_val(interp, "Tensor BigInteger inferred suffix ref",
        "(String (ref (Tensor [(BigInteger \"9223372036854775808\")] BigInteger) [0]))", "9223372036854775808", pass, fail);
    test_str_val(interp, "Tensor BigInteger inferred prefix ref",
        "(String (ref (Tensor BigInteger [(BigInteger \"9223372036854775809\")]) [0]))", "9223372036854775809", pass, fail);
    test_str_val(interp, "Array BigInteger tensor conversion preserves element",
        "(String (ref (Array (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) 0))", "9223372036854775808", pass, fail);
    test_str_val(interp, "List BigInteger tensor conversion preserves element",
        "(String (ref (List (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) 1))", "2", pass, fail);
    test_str_val(interp, "realize BigInteger scalar fill into tensor",
        "(let (dst (Tensor BigInteger [2] 0)) (block (realize (BigInteger \"3\") dst) (String (ref dst [1]))))",
        "3", pass, fail);
    test_str_val(interp, "realize BigInteger tensor copy",
        "(let (src (Tensor BigInteger [1] [(BigInteger \"7\")]) dst (Tensor BigInteger [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7", pass, fail);
    test_error_contains(interp, "Tensor BigInteger rejects inexact data",
        "(Tensor BigInteger [1] [1.5])", "exact integers", pass, fail);
    test_str_val(interp, "Tensor BigFloat dtype",
        "(format \"%s\" (dtype (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])))", "BigFloat", pass, fail);
    test_str_val(interp, "Tensor BigFloat explicit ref preserves precision range",
        "(String (ref (Tensor BigFloat [1] [(BigFloat \"1e309\")]) [0]))", "1e+309", pass, fail);
    test_str_val(interp, "Tensor BigFloat inferred suffix ref",
        "(String (ref (Tensor [(BigFloat \"1.25\")] BigFloat) [0]))", "1.25", pass, fail);
    test_str_val(interp, "Tensor BigFloat inferred prefix ref",
        "(String (ref (Tensor BigFloat [(BigFloat \"2.5\")]) [0]))", "2.5", pass, fail);
    test_str_val(interp, "Array BigFloat tensor conversion preserves element",
        "(String (ref (Array (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) 1))", "2", pass, fail);
    test_str_val(interp, "List BigFloat tensor conversion preserves element",
        "(String (ref (List (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) 0))", "1.25", pass, fail);
    test_str_val(interp, "realize BigFloat scalar fill into tensor",
        "(let (dst (Tensor BigFloat [2] 0)) (block (realize (BigFloat \"3.5\") dst) (String (ref dst [1]))))",
        "3.5", pass, fail);
    test_str_val(interp, "realize BigFloat tensor copy",
        "(let (src (Tensor BigFloat [1] [(BigFloat \"7.25\")]) dst (Tensor BigFloat [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7.25", pass, fail);
    test_str_val(interp, "map BigFloat tensor scalar preserves dtype",
        "(String (ref (map + (Tensor BigFloat [2] [(BigFloat \"1.25\") 2]) (BigFloat \"2\")) [0]))",
        "3.25", pass, fail);
    test_str_val(interp, "Tensor BigComplex dtype",
        "(format \"%s\" (dtype (Tensor BigComplex [2] [(BigComplex 1 2) 3])))", "BigComplex", pass, fail);
    test_str_val(interp, "Tensor BigComplex explicit ref preserves complex value",
        "(String (ref (Tensor BigComplex [1] [(BigComplex 1 2)]) [0]))", "1+2i", pass, fail);
    test_str_val(interp, "Tensor BigComplex inferred suffix ref",
        "(String (ref (Tensor [(BigComplex 1 2)] BigComplex) [0]))", "1+2i", pass, fail);
    test_str_val(interp, "Tensor BigComplex inferred prefix ref",
        "(String (ref (Tensor BigComplex [(BigComplex 3 -4)]) [0]))", "3-4i", pass, fail);
    test_str_val(interp, "Array BigComplex tensor conversion preserves element",
        "(String (ref (Array (Tensor BigComplex [2] [(BigComplex 1 2) 3])) 0))", "1+2i", pass, fail);
    test_str_val(interp, "List BigComplex tensor conversion preserves element",
        "(String (ref (List (Tensor BigComplex [2] [(BigComplex 1 2) 3])) 1))", "3+0i", pass, fail);
    test_str_val(interp, "realize BigComplex scalar fill into tensor",
        "(let (dst (Tensor BigComplex [2] 0)) (block (realize (BigComplex 3 4) dst) (String (ref dst [1]))))",
        "3+4i", pass, fail);
    test_str_val(interp, "realize BigComplex tensor copy",
        "(let (src (Tensor BigComplex [1] [(BigComplex 7 8)]) dst (Tensor BigComplex [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7+8i", pass, fail);
    test_str_val(interp, "real-part BigComplex tensor returns BigFloat tensor",
        "(String (ref (real-part (Tensor BigComplex [2] [(BigComplex 1 2) (BigComplex 3 4)])) [1]))",
        "3", pass, fail);
    test_str_val(interp, "imag-part BigComplex tensor returns BigFloat tensor",
        "(String (ref (imag-part (Tensor BigComplex [2] [(BigComplex 1 2) (BigComplex 3 4)])) [0]))",
        "2", pass, fail);
    test_str_val(interp, "conjugate BigComplex tensor flips imaginary part",
        "(String (ref (conjugate (Tensor BigComplex [1] [(BigComplex 1 2)])) [0]))",
        "1-2i", pass, fail);
    test_str_val(interp, "real-part lazy BigComplex tensor realizes source",
        "(String (ref (real-part (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 3 4))) [0]))",
        "4", pass, fail);
    test_str_val(interp, "conjugate lazy BigComplex tensor realizes source",
        "(String (ref (conjugate (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 3 4))) [0]))",
        "4-6i", pass, fail);
    test_eq_double(interp, "real-part Double tensor preserves dtype and value",
        "(ref (real-part (Tensor Double [1] [1.5])) [0])", 1.5, pass, fail);
    test_eq_double(interp, "imag-part Double tensor returns zero tensor",
        "(ref (imag-part (Tensor Double [1] [1.5])) [0])", 0.0, pass, fail);
    test_str_val(interp, "conjugate BigInteger tensor preserves exact value",
        "(String (ref (conjugate (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "imag-part BigFloat tensor returns zero tensor",
        "(String (ref (imag-part (Tensor BigFloat [1] [(BigFloat \"1.25\")])) [0]))",
        "0", pass, fail);
    test_eq_double(interp, "abs Double tensor preserves dtype and magnitude",
        "(ref (abs (Tensor Double [2] [-1.5 2.5])) [0])", 1.5, pass, fail);
    test_str_val(interp, "abs BigInteger tensor preserves exact dtype",
        "(String (ref (abs (Tensor BigInteger [1] [(BigInteger \"-9223372036854775808\")])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "abs BigFloat tensor preserves high precision dtype",
        "(String (ref (abs (Tensor BigFloat [1] [(BigFloat \"-1e309\")])) [0]))",
        "1e+309", pass, fail);
    test_str_val(interp, "abs BigComplex tensor returns BigFloat magnitude tensor",
        "(String (ref (abs (Tensor BigComplex [1] [(BigComplex 3 4)])) [0]))",
        "5", pass, fail);
    test_str_val(interp, "abs lazy BigComplex tensor realizes source",
        "(String (ref (abs (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 2))) [0]))",
        "5", pass, fail);
    test_eq_double(interp, "sqrt Double tensor preserves Double dtype",
        "(ref (sqrt (Tensor Double [1] [9.0])) [0])", 3.0, pass, fail);
    test_eq_double(interp, "sqrt BigInteger tensor returns Double tensor",
        "(ref (sqrt (Tensor BigInteger [1] [9])) [0])", 3.0, pass, fail);
    test_str_val(interp, "sqrt BigFloat tensor preserves high precision dtype",
        "(String (ref (sqrt (Tensor BigFloat [1] [(BigFloat \"4\")])) [0]))",
        "2", pass, fail);
    test_str_val(interp, "sqrt BigComplex tensor preserves complex dtype",
        "(String (ref (sqrt (Tensor BigComplex [1] [(BigComplex -1 0)])) [0]))",
        "0+1i", pass, fail);
    test_str_val(interp, "sqrt lazy BigComplex tensor realizes source",
        "(String (ref (sqrt (map + (Tensor BigComplex [1] [(BigComplex -2 0)]) (BigComplex 1 0))) [0]))",
        "0+1i", pass, fail);
    test_error_contains(interp, "sqrt BigInteger tensor rejects out of Double range",
        "(sqrt (Tensor BigInteger [1] [(BigInteger \"100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\")]))",
        "math: BigInteger tensor element out of Double range", pass, fail);
    test_eq_double(interp, "sin Double tensor",
        "(ref (sin (Tensor Double [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "cos BigInteger tensor returns Double tensor",
        "(ref (cos (Tensor BigInteger [1] [0])) [0])", 1.0, pass, fail);
    test_eq_double(interp, "tan Double tensor",
        "(ref (tan (Tensor Double [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "asin Double tensor",
        "(ref (asin (Tensor Double [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "acos Double tensor",
        "(ref (acos (Tensor Double [1] [1.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "atan Double tensor",
        "(ref (atan (Tensor Double [1] [0.0])) [0])", 0.0, pass, fail);
    test_str_val(interp, "sinh BigFloat tensor preserves dtype",
        "(String (ref (sinh (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "0", pass, fail);
    test_str_val(interp, "cosh BigFloat tensor preserves dtype",
        "(String (ref (cosh (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "1", pass, fail);
    test_str_val(interp, "tanh BigComplex tensor preserves dtype",
        "(String (ref (tanh (Tensor BigComplex [1] [(BigComplex 0 0)])) [0]))",
        "0+0i", pass, fail);
    test_eq_double(interp, "exp BigInteger tensor returns Double tensor",
        "(ref (exp (Tensor BigInteger [1] [0])) [0])", 1.0, pass, fail);
    test_str_val(interp, "log BigFloat tensor preserves dtype",
        "(String (ref (log (Tensor BigFloat [1] [(BigFloat \"1\")])) [0]))",
        "0", pass, fail);
    test_str_val(interp, "log10 lazy BigComplex tensor realizes source",
        "(String (ref (log10 (map + (Tensor BigComplex [1] [(BigComplex 0 0)]) (BigComplex 1 0))) [0]))",
        "0+0i", pass, fail);
    test_error_contains(interp, "Tensor BigComplex rejects non-numeric data",
        "(Tensor BigComplex [1] ['bad])", "numbers representable as BigComplex", pass, fail);
    test_eq(interp, "Array tensor conversion length",
        "(length (Array (Tensor Double [2 2] [1 2 3 4])))", 4, pass, fail);
    test_eq_double(interp, "Array tensor conversion row-major value",
        "(ref (Array (Tensor Double [2 2] [1 2 3 4])) 2)", 3.0, pass, fail);
    test_eq(interp, "List tensor conversion length",
        "(length (List (Tensor Double [2 2] [1 2 3 4])))", 4, pass, fail);
    test_eq_double(interp, "List tensor conversion row-major value",
        "(ref (List (Tensor Double [2 2] [1 2 3 4])) 2)", 3.0, pass, fail);
    test_eq_double(interp, "Array lazy tensor map conversion realizes",
        "(ref (Array (map + (Tensor Double [2] [1 2]) 1)) 1)", 3.0, pass, fail);
    test_eq_double(interp, "List lazy tensor contract conversion realizes",
        "(ref (List (contract (Tensor Double [2] [1 2]) (Tensor Double [2] [10 20]) [0] [0])) 0)", 50.0, pass, fail);
    test_error_contains(interp, "ref tensor wrong rank",
        "(ref (Tensor Double [2 3] [1 2 3 4 5 6]) [1])", "ref: tensor index rank mismatch", pass, fail);
    test_error_contains(interp, "ref tensor oob",
        "(ref (Tensor Double [2 3] [1 2 3 4 5 6]) [2 0])", "ref: tensor index out of bounds", pass, fail);
    // realize - tensor expression boundary
    test_eq_double(interp, "realize concrete tensor",
        "(ref (realize (Tensor Double [2 3] [1 2 3 4 5 6])) [1 2])", 6.0, pass, fail);
    test_eq_double(interp, "realize rank-0 concrete tensor",
        "(let (dst (Tensor Double [] 0.0)) (block (realize (Tensor Double [] 3.0) dst) (ref dst [])))",
        3.0, pass, fail);
    test_eq(interp, "realize scalar into zero-size destination",
        "(let (dst (Tensor Double [0] 0.0)) (block (realize 9 dst) (length dst)))",
        0, pass, fail);
    test_eq(interp, "realize lazy zero-size source into zero-size destination",
        "(let (dst (Tensor Double [0] 0.0)) (block (realize (map + (Tensor Double [0] 0.0) 1.0) dst) (length dst)))",
        0, pass, fail);
    test_eq_double(interp, "realize tensor into destination",
        "(let (src (Tensor Double [2] [7 8]) dst (Tensor Double [2] 0.0)) (block (realize src dst) (ref dst [1])))",
        8.0, pass, fail);
    test_eq_double(interp, "realize scalar fill into destination",
        "(let (dst (Tensor Double [2 2] 0.0)) (block (realize 9 dst) (ref dst [1 1])))",
        9.0, pass, fail);
    test_eq_double(interp, "realize aliased destination no-op",
        "(let (dst (Tensor Double [2] [3 4])) (block (realize dst dst) (ref dst [1])))",
        4.0, pass, fail);
    test_error_contains(interp, "realize rejects scalar without destination",
        "(realize 1)", "realize: expected Tensor source", pass, fail);
    test_error_contains(interp, "realize rejects non-tensor destination",
        "(realize 1 [0])", "realize: destination must be a Tensor", pass, fail);
    test_error_contains(interp, "realize tensor destination shape mismatch",
        "(realize (Tensor Double [2] [1 2]) (Tensor Double [3] 0.0))", "realize: tensor shape mismatch", pass, fail);
    {
        usz[1] tensor_shape = { 2 };
        Value* src = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        bool setup_ok = src != null && src.tag == TENSOR && src.tensor_val != null && src.tensor_val.data != null;
        void* saved_data = setup_ok ? src.tensor_val.data : null;
        if (setup_ok) src.tensor_val.data = null;
        Value*[1] args = { src };
        Value* result = setup_ok ? prim_realize(args[0:1], null, interp) : null;
        if (setup_ok) src.tensor_val.data = saved_data;
        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            str_contains(result.str_chars[:result.str_len], "realize: source tensor backing storage is missing");
        if (ok) {
            emit_pass_literal("[PASS] realize malformed concrete tensor rejects missing source storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] realize malformed concrete tensor rejects missing source storage (setup_ok=%s tag=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1
            );
            (*fail)++;
        }
    }
    {
        usz[1] tensor_shape = { 2 };
        Value* src = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        Value* dst = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        bool setup_ok = src != null && src.tag == TENSOR && src.tensor_val != null && src.tensor_val.data != null &&
            dst != null && dst.tag == TENSOR && dst.tensor_val != null && dst.tensor_val.data != null;
        void* saved_data = setup_ok ? src.tensor_val.data : null;
        if (setup_ok) src.tensor_val.data = null;
        Value*[2] args = { src, dst };
        Value* result = setup_ok ? prim_realize(args[0:2], null, interp) : null;
        if (setup_ok) src.tensor_val.data = saved_data;
        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            str_contains(result.str_chars[:result.str_len], "realize: source tensor backing storage is missing");
        if (ok) {
            emit_pass_literal("[PASS] realize malformed tensor into destination rejects missing source storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] realize malformed tensor into destination rejects missing source storage (setup_ok=%s tag=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1
            );
            (*fail)++;
        }
    }
    // map - tensor elementwise lazy expression surface
    test_eq_double(interp, "map tensor unary",
        "(ref (map (lambda (x) (* x 2)) (Tensor Double [2 2] [1 2 3 4])) [1 0])",
        6.0, pass, fail);
    test_eq_double(interp, "map tensor scalar right",
        "(ref (map + (Tensor Double [2] [1 2]) 10) [1])",
        12.0, pass, fail);
    test_eq_double(interp, "map tensor scalar left",
        "(ref (map + 10 (Tensor Double [2] [1 2])) [1])",
        12.0, pass, fail);
    test_eq_double(interp, "map tensor tensor exact shape",
        "(ref (map + (Tensor Double [2] [1 2]) (Tensor Double [2] [10 20])) [1])",
        22.0, pass, fail);
    test_eq_double(interp, "map tensor tensor broadcast trailing singleton axis",
        "(ref (map + (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [1 3] [10 20 30])) [1 2])",
        36.0, pass, fail);
    test_eq_double(interp, "map tensor tensor broadcast inner singleton axis",
        "(ref (map + (Tensor Double [2 1] [1 2]) (Tensor Double [1 3] [10 20 30])) [1 2])",
        32.0, pass, fail);
    test_eq_double(interp, "map rank-0 tensor with rank-1 tensor",
        "(ref (map + (Tensor Double [] 7.0) (Tensor Double [2] [1 2])) [1])",
        9.0, pass, fail);
    test_eq_double(interp, "map rank-0 tensor with rank-2 tensor",
        "(ref (map + (Tensor Double [] 7.0) (Tensor Double [2 2] [1 2 3 4])) [1 1])",
        11.0, pass, fail);
    test_eq_double(interp, "realize mapped tensor into destination",
        "(let (src (map + (Tensor Double [2] [1 2]) 3) dst (Tensor Double [2] 0.0)) (block (realize src dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "realize mapped expression directly into destination",
        "(let (dst (Tensor Double [2] 0.0)) (block (realize (map + (Tensor Double [2] [1 2]) 3) dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "failed realize mapped expression leaves destination unchanged",
        "(let (dst (Tensor Double [2] [7 8])) (block (handle (realize (map (lambda (x) (if (= x 2) 'bad (+ x 1))) (Tensor Double [2] [1 2])) dst) (raise msg nil)) (+ (* (ref dst [0]) 10) (ref dst [1]))))",
        78.0, pass, fail);
    test_eq_double(interp, "realize aliased map destination",
        "(let (dst (Tensor Double [2 3] [1 2 3 4 5 6])) (block (realize (map + dst dst) dst) (ref dst [1 1])))",
        10.0, pass, fail);
    test_eq_double(interp, "map rank-0 scalar",
        "(ref (map + (Tensor Double [] 7.0) 1.0) [])",
        8.0, pass, fail);
    test_eq_double(interp, "realize mapped rank-0 scalar",
        "(ref (realize (map + (Tensor Double [] 7.0) 1.0)) [])",
        8.0, pass, fail);
    test_eq_double(interp, "realize mapped expression aliased destination",
        "(let (dst (Tensor Double [2] [3 4])) (block (realize (map + dst 1) dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "lazy map tensor survives function return boundary",
        "(block (define (mk-map) (map + (Tensor Double [2] [1 2]) 3)) (ref (mk-map) [1]))",
        5.0, pass, fail);
    test_eq_double(interp, "lazy map tensor survives closure capture",
        "(let (src (map + (Tensor Double [2] [1 2]) 3)) (let (f (lambda () (ref src [1]))) (f)))",
        5.0, pass, fail);
    test_str_val(interp, "map BigInteger unary preserves exact range",
        "(String (ref (map (lambda (x) x) (Tensor [(BigInteger \"9223372036854775808\")] BigInteger)) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "map BigInteger scalar left",
        "(String (ref (map + (BigInteger \"2\") (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) [1]))",
        "4", pass, fail);
    test_str_val(interp, "map BigInteger tensor broadcast",
        "(String (ref (map + (Tensor BigInteger [2 1] [(BigInteger \"1\") (BigInteger \"2\")]) (Tensor BigInteger [1 2] [(BigInteger \"10\") (BigInteger \"20\")])) [1 1]))",
        "22", pass, fail);
    test_str_val(interp, "realize mapped BigInteger expression into destination",
        "(let (dst (Tensor BigInteger [2] 0)) (block (realize (map + (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (BigInteger \"2\")) dst) (String (ref dst [0]))))",
        "9223372036854775810", pass, fail);
    test_str_val(interp, "lazy BigInteger map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) (BigInteger \"2\"))) (String (ref (mk) [0])))",
        "9223372036854775810", pass, fail);
    test_str_val(interp, "lazy BigInteger map survives closure capture",
        "(let (src (map + (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) (BigInteger \"2\"))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "9223372036854775810", pass, fail);
    test_error_contains(interp, "map mixed BigInteger Double tensors rejected",
        "(map + (Tensor BigInteger [1] [(BigInteger \"1\")]) (Tensor Double [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_error_contains(interp, "map BigInteger rejects inexact scalar",
        "(map + (Tensor BigInteger [1] [(BigInteger \"1\")]) 1.5)",
        "map: tensor inputs must be exact integers", pass, fail);
    test_str_val(interp, "contract BigInteger vector dot",
        "(String (ref (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0]) []))",
        "18446744073709551622", pass, fail);
    test_str_val(interp, "contract BigInteger matrix product",
        "(String (ref (contract (Tensor BigInteger [2 3] [1 2 3 4 5 6]) (Tensor BigInteger [3 2] [7 8 9 10 11 12]) [1] [0]) [1 1]))",
        "154", pass, fail);
    test_str_val(interp, "contract BigInteger zero-size identity",
        "(String (ref (contract (Tensor BigInteger [0] []) (Tensor BigInteger [0] []) [0] [0]) []))",
        "0", pass, fail);
    test_str_val(interp, "realize BigInteger contract into destination",
        "(let (dst (Tensor BigInteger [2 2] 0)) (block (realize (contract (Tensor BigInteger [2 3] [1 2 3 4 5 6]) (Tensor BigInteger [3 2] [7 8 9 10 11 12]) [1] [0]) dst) (String (ref dst [1 1]))))",
        "154", pass, fail);
    test_str_val(interp, "lazy BigInteger contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0])) (String (ref (mk) [])))",
        "18446744073709551622", pass, fail);
    test_str_val(interp, "lazy BigInteger contract survives closure capture",
        "(let (src (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "18446744073709551622", pass, fail);
    test_error_contains(interp, "contract mixed BigInteger Double tensors rejected",
        "(contract (Tensor BigInteger [1] [(BigInteger \"1\")]) (Tensor Double [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "map BigFloat unary preserves outside Double range",
        "(String (ref (map (lambda (x) x) (Tensor [(BigFloat \"1e309\")] BigFloat)) [0]))",
        "1e+309", pass, fail);
    test_str_val(interp, "map BigFloat scalar left",
        "(String (ref (map + (BigFloat \"2\") (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) [1]))",
        "4", pass, fail);
    test_str_val(interp, "map BigFloat tensor broadcast",
        "(String (ref (map + (Tensor BigFloat [2 1] [(BigFloat \"1\") (BigFloat \"2\")]) (Tensor BigFloat [1 2] [(BigFloat \"10\") (BigFloat \"20\")])) [1 1]))",
        "22", pass, fail);
    test_str_val(interp, "realize mapped BigFloat expression into destination",
        "(let (dst (Tensor BigFloat [2] 0)) (block (realize (map + (Tensor BigFloat [2] [(BigFloat \"1.25\") 2]) (BigFloat \"2\")) dst) (String (ref dst [0]))))",
        "3.25", pass, fail);
    test_str_val(interp, "lazy BigFloat map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigFloat [1] [(BigFloat \"1.25\")]) (BigFloat \"2\"))) (String (ref (mk) [0])))",
        "3.25", pass, fail);
    test_str_val(interp, "lazy BigFloat map survives closure capture",
        "(let (src (map + (Tensor BigFloat [1] [(BigFloat \"1.25\")]) (BigFloat \"2\"))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "3.25", pass, fail);
    test_error_contains(interp, "map mixed BigFloat Double tensors rejected",
        "(map + (Tensor BigFloat [1] [(BigFloat \"1\")]) (Tensor Double [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "contract BigFloat vector dot",
        "(String (ref (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0]) []))",
        "55", pass, fail);
    test_str_val(interp, "contract BigFloat matrix product",
        "(String (ref (contract (Tensor BigFloat [2 3] [(BigFloat \"1\") (BigFloat \"2\") (BigFloat \"3\") (BigFloat \"4\") (BigFloat \"5\") (BigFloat \"6\")]) (Tensor BigFloat [3 2] [(BigFloat \"7\") (BigFloat \"8\") (BigFloat \"9\") (BigFloat \"10\") (BigFloat \"11\") (BigFloat \"12\")]) [1 0]) [1 1]))",
        "154", pass, fail);
    test_str_val(interp, "contract BigFloat zero-size identity",
        "(String (ref (contract (Tensor BigFloat [0] []) (Tensor BigFloat [0] []) [0] [0]) []))",
        "0", pass, fail);
    test_str_val(interp, "realize BigFloat contract into destination",
        "(let (dst (Tensor BigFloat [2 2] 0)) (block (realize (contract (Tensor BigFloat [2 3] [(BigFloat \"1\") (BigFloat \"2\") (BigFloat \"3\") (BigFloat \"4\") (BigFloat \"5\") (BigFloat \"6\")]) (Tensor BigFloat [3 2] [(BigFloat \"7\") (BigFloat \"8\") (BigFloat \"9\") (BigFloat \"10\") (BigFloat \"11\") (BigFloat \"12\")]) [1 0]) dst) (String (ref dst [1 1]))))",
        "154", pass, fail);
    test_str_val(interp, "lazy BigFloat contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0])) (String (ref (mk) [])))",
        "55", pass, fail);
    test_str_val(interp, "lazy BigFloat contract survives closure capture",
        "(let (src (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "55", pass, fail);
    test_error_contains(interp, "contract mixed BigFloat Double tensors rejected",
        "(contract (Tensor BigFloat [1] [(BigFloat \"1\")]) (Tensor Double [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "map BigComplex unary preserves complex value",
        "(String (ref (map (lambda (x) x) (Tensor [(BigComplex 1 2)] BigComplex)) [0]))",
        "1+2i", pass, fail);
    test_str_val(interp, "map BigComplex scalar left",
        "(String (ref (map + (BigComplex 2 1) (Tensor BigComplex [2] [(BigComplex 1 2) 3])) [1]))",
        "5+1i", pass, fail);
    test_str_val(interp, "map BigComplex tensor broadcast",
        "(String (ref (map + (Tensor BigComplex [2 1] [(BigComplex 1 1) (BigComplex 2 2)]) (Tensor BigComplex [1 2] [(BigComplex 10 10) (BigComplex 20 20)])) [1 1]))",
        "22+22i", pass, fail);
    test_str_val(interp, "realize mapped BigComplex expression into destination",
        "(let (dst (Tensor BigComplex [2] 0)) (block (realize (map + (Tensor BigComplex [2] [(BigComplex 1 2) 3]) (BigComplex 2 3)) dst) (String (ref dst [0]))))",
        "3+5i", pass, fail);
    test_str_val(interp, "lazy BigComplex map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 3))) (String (ref (mk) [0])))",
        "3+5i", pass, fail);
    test_str_val(interp, "lazy BigComplex map survives closure capture",
        "(let (src (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 3))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "3+5i", pass, fail);
    test_error_contains(interp, "map mixed BigComplex Double tensors rejected",
        "(map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (Tensor Double [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "contract BigComplex vector dot",
        "(String (ref (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0]) []))",
        "11+3i", pass, fail);
    test_str_val(interp, "contract BigComplex matrix product",
        "(String (ref (contract (Tensor BigComplex [2 2] [(BigComplex 1 1) 2 3 4]) (Tensor BigComplex [2 2] [5 6 7 8]) [1] [0]) [0 1]))",
        "22+6i", pass, fail);
    test_str_val(interp, "contract BigComplex zero-size identity",
        "(String (ref (contract (Tensor BigComplex [0] []) (Tensor BigComplex [0] []) [0] [0]) []))",
        "0+0i", pass, fail);
    test_str_val(interp, "realize BigComplex contract into destination",
        "(let (dst (Tensor BigComplex [] 0)) (block (realize (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0]) dst) (String (ref dst []))))",
        "11+3i", pass, fail);
    test_str_val(interp, "lazy BigComplex contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0])) (String (ref (mk) [])))",
        "11+3i", pass, fail);
    test_str_val(interp, "lazy BigComplex contract survives closure capture",
        "(let (src (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "11+3i", pass, fail);
    test_error_contains(interp, "contract mixed BigComplex Double tensors rejected",
        "(contract (Tensor BigComplex [1] [(BigComplex 1 2)]) (Tensor Double [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_error_contains(interp, "map tensor broadcast shape mismatch",
        "(map + (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [2 2] [1 2 3 4]))",
        "map: tensor shape mismatch", pass, fail);
    test_error_contains(interp, "map tensor shape mismatch",
        "(map + (Tensor Double [2] [1 2]) (Tensor Double [3] [1 2 3]))",
        "map: tensor shape mismatch", pass, fail);
    test_error_contains(interp, "map tensor function dtype mismatch",
        "(realize (map (lambda (x) 'bad) (Tensor Double [1] [1])))",
        "map: function result dtype mismatch", pass, fail);
    test_error_contains(interp, "map tensor rejects nonnumeric source",
        "(map + (Tensor Double [1] [1]) 'bad)",
        "map: tensor inputs must be Tensors or numbers", pass, fail);
    test_error_contains(interp, "map tensor rejects noncallable",
        "(map nil (Tensor Double [1] [1]))",
        "map: function must be callable", pass, fail);
    // contract - tensor summed-index lazy expression surface
    test_eq_double(interp, "contract matrix product rank-2",
        "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) [1] [0]) [1 1])",
        154.0, pass, fail);
    test_eq(interp, "contract matrix product rank",
        "(rank (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) [1] [0]))",
        2, pass, fail);
    test_eq_double(interp, "contract vector dot scalar",
        "(ref (contract (Tensor Double [3] [1 2 3]) (Tensor Double [3] [10 20 30]) [0] [0]) [])",
        140.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_ddot_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_ddot_call_count();
        test_eq_double(interp, "contract vector dot BLAS candidate",
            "(ref (contract (Tensor Double [3] [1 2 3]) (Tensor Double [3] [10 20 30]) [0 0]) [])",
            140.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_ddot_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend ddot contract path"
                : "[PASS] tensor BLAS backend ddot unavailable; contract fallback path retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend ddot contract path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "contract list axes",
        "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) '(1) '(0)) [1 1])",
        154.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemm_call_count();
        test_eq_double(interp, "contract single axis pair shorthand",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) [1 0]) [1 1])",
            154.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemm_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemm contract path"
                : "[PASS] tensor BLAS backend unavailable; contract fallback path retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemm contract path");
            (*fail)++;
        }
    }
    {
        bool blas_available = omni_tensor_backend_blas_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemm_call_count();
        test_eq_double(interp, "contract rank-2 transpose left axis pair",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [2 2] [7 8 9 10]) [0 0]) [2 1])",
            84.0, pass, fail);
        test_eq_double(interp, "contract rank-2 transpose right axis pair",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [2 3] [7 8 9 10 11 12]) [1 1]) [0 1])",
            68.0, pass, fail);
        test_eq_double(interp, "contract rank-2 transpose both axis pair",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [4 2] [7 8 9 10 11 12 13 14]) [0 1]) [2 3])",
            123.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemm_call_count() >= blas_calls_before + 3;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemm transpose contract paths"
                : "[PASS] tensor BLAS backend unavailable; transpose contract fallback paths retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemm transpose contract paths");
            (*fail)++;
        }
    }
    {
        bool blas_available = omni_tensor_backend_blas_dgemv_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemv_call_count();
        test_eq_double(interp, "contract matrix vector BLAS candidate",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3] [10 20 30]) [1 0]) [1])",
            320.0, pass, fail);
        test_eq_double(interp, "contract transposed matrix vector BLAS candidate",
            "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [2] [10 20]) [0 0]) [2])",
            150.0, pass, fail);
        test_eq_double(interp, "contract vector matrix BLAS candidate",
            "(ref (contract (Tensor Double [3] [10 20 30]) (Tensor Double [3 2] [1 2 3 4 5 6]) [0 0]) [1])",
            280.0, pass, fail);
        test_eq_double(interp, "contract vector transposed matrix BLAS candidate",
            "(ref (contract (Tensor Double [3] [10 20 30]) (Tensor Double [2 3] [1 2 3 4 5 6]) [0 1]) [1])",
            320.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemv_call_count() >= blas_calls_before + 4;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemv contract paths"
                : "[PASS] tensor BLAS backend dgemv unavailable; contract fallback paths retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemv contract paths");
            (*fail)++;
        }
    }
    test_eq_double(interp, "contract nested axis pair shorthand",
        "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) [[1 0]]) [1 1])",
        154.0, pass, fail);
    test_eq_double(interp, "contract negative axes",
        "(ref (contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [3 2] [7 8 9 10 11 12]) [-1] [-2]) [1 1])",
        154.0, pass, fail);
    test_eq_double(interp, "contract zero contracted dimension scalar",
        "(ref (contract (Tensor Double [0] 0.0) (Tensor Double [0] 0.0) [0] [0]) [])",
        0.0, pass, fail);
    test_eq_double(interp, "contract rank-0 scalar",
        "(ref (contract (Tensor Double [] 3.0) (Tensor Double [] 4.0) [] []) [])",
        12.0, pass, fail);
    test_eq_double(interp, "contract zero contracted dimension non-scalar",
        "(ref (contract (Tensor Double [2 0 1] 0.0) (Tensor Double [2 0 1] 0.0) [1] [1]) [1 0 1 0])",
        0.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_dger_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dger_call_count();
        test_eq_double(interp, "contract zero axes outer product",
            "(ref (contract (Tensor Double [2] [1 2]) (Tensor Double [3] [10 20 30]) [] []) [1 2])",
            60.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dger_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dger outer-product path"
                : "[PASS] tensor BLAS backend dger unavailable; outer-product fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dger outer-product path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "contract multi-axis",
        "(ref (contract (Tensor Double [2 2 2] [1 2 3 4 5 6 7 8]) (Tensor Double [2 3 2] 1.0) [1 2] [0 2]) [1 2])",
        26.0, pass, fail);
    test_eq_double(interp, "contract multi-axis pair shorthand",
        "(ref (contract (Tensor Double [2 2 2] [1 2 3 4 5 6 7 8]) (Tensor Double [2 3 2] 1.0) [[1 0] [2 2]]) [1 2])",
        26.0, pass, fail);
    test_eq_double(interp, "realize contracted tensor into destination",
        "(let (src (contract (Tensor Double [2 2] [1 2 3 4]) (Tensor Double [2 2] [5 6 7 8]) [1] [0]) dst (Tensor Double [2 2] 0.0)) (block (realize src dst) (ref dst [0 1])))",
        22.0, pass, fail);
    test_eq_double(interp, "realize contracted expression directly into destination",
        "(let (dst (Tensor Double [2 2] 0.0)) (block (realize (contract (Tensor Double [2 2] [1 2 3 4]) (Tensor Double [2 2] [5 6 7 8]) [1] [0]) dst) (ref dst [0 1])))",
        22.0, pass, fail);
    test_eq_double(interp, "failed realize contracted expression leaves destination unchanged",
        "(let (dst (Tensor Double [] 99.0)) (block (handle (realize (contract (map (lambda (x) (if (= x 2) 'bad x)) (Tensor Double [2] [1 2])) (Tensor Double [2] [10 20]) [0] [0]) dst) (raise msg nil)) (ref dst [])))",
        99.0, pass, fail);
    test_eq_double(interp, "lazy contract tensor survives function return boundary",
        "(block (define (mk-contract) (contract (Tensor Double [2] [1 2]) (Tensor Double [2] [10 20]) [0] [0])) (ref (mk-contract) []))",
        50.0, pass, fail);
    test_eq_double(interp, "lazy contract tensor survives closure capture",
        "(let (src (contract (Tensor Double [2] [1 2]) (Tensor Double [2] [10 20]) [0] [0])) (let (f (lambda () (ref src []))) (f)))",
        50.0, pass, fail);
    test_error_contains(interp, "realize contracted expression rejects aliased destination",
        "(let (dst (Tensor Double [2 2] [1 2 3 4])) (realize (contract dst dst [1] [0]) dst))",
        "contract: destination aliases source tensor", pass, fail);
    test_error_contains(interp, "realize nested contracted expression rejects aliased destination",
        "(let (dst (Tensor Double [2 2] [1 2 3 4])) (let (src (map + dst 0)) (realize (contract src dst [1] [0]) dst)))",
        "contract: destination aliases source tensor", pass, fail);
    test_eq(interp, "realize zero-size contracted expression into zero-size destination",
        "(let (dst (Tensor Double [0] 0.0)) (let (src (contract (Tensor Double [0 1] 0.0) (Tensor Double [1] [10]) [1] [0])) (block (realize src dst) (length dst))))",
        0, pass, fail);
    test_error_contains(interp, "contract axis length mismatch",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) [0] [])",
        "contract: axis list length mismatch", pass, fail);
    test_error_contains(interp, "contract left axis out of range",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) [1] [0])",
        "contract: left axis out of range", pass, fail);
    test_error_contains(interp, "contract right axis out of range",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) [0] [1])",
        "contract: right axis out of range", pass, fail);
    test_error_contains(interp, "contract duplicate axis",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2 2] 1.0) [0 0] [0 1])",
        "contract: duplicate contracted axis", pass, fail);
    test_error_contains(interp, "contract duplicate axis with negative index normalization",
        "(contract (Tensor Double [2 3] [1 2 3 4 5 6]) (Tensor Double [2 3] [7 8 9 10 11 12]) [1 -1] [0 1])",
        "contract: duplicate contracted axis", pass, fail);
    test_error_contains(interp, "contract dimension mismatch",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [3] [1 2 3]) [0] [0])",
        "contract: contracted dimensions must match", pass, fail);
    test_error_contains(interp, "contract rejects non-tensor operand",
        "(contract [1 2] (Tensor Double [2] [1 2]) [0] [0])",
        "contract: expected Tensor operands", pass, fail);
    test_error_contains(interp, "contract rejects scalar axes",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) 0 [0])",
        "contract: axes must be arrays or proper lists of integers", pass, fail);
    test_error_contains(interp, "contract rejects malformed axis pair shorthand",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) [0])",
        "contract: axis pairs must contain two-integer arrays or proper lists", pass, fail);
    test_error_contains(interp, "contract rejects non-integer axes",
        "(contract (Tensor Double [2] [1 2]) (Tensor Double [2] [1 2]) ['bad] [0])",
        "contract: axes must be integers", pass, fail);
    // ref on lists (walks cons chain, not just pair 0/1)
    test_eq(interp, "ref list 0", "(ref '(10 20 30) 0)", 10, pass, fail);
    test_eq(interp, "ref list 2", "(ref '(10 20 30) 2)", 30, pass, fail);
    test_eq(interp, "ref list -1", "(ref '(10 20 30) -1)", 30, pass, fail);
    test_error(interp, "ref list oob", "(ref '(10 20 30) 5)", pass, fail);
    test_eq(interp, ".[-1] array", "(let (a [10 20 30]) a.[-1])", 30, pass, fail);
    test_eq(interp, ".[-1] list", "(let (l '(10 20 30)) l.[-1])", 30, pass, fail);
    test_eq(interp, ".[-2] list", "(let (l '(10 20 30)) l.[-2])", 20, pass, fail);
    test_error(interp, ".[-4] list oob", "(let (l '(10 20 30)) l.[-4])", pass, fail);
    test_str(interp, "char-at -1", "(char-at \"abc\" -1)", pass, fail);
    setup(interp, "(define neg-arr [1 2 3])");
    test_tag_interp(interp, "set! array returns void", "(set! neg-arr -1 99)", VOID, pass, fail);
    test_eq(interp, "set! array -1", "(ref neg-arr 2)", 99, pass, fail);
    // length - generic
    test_eq(interp, "length array", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "length dict", "(length {'a 1 'b 2})", 2, pass, fail);
    test_eq(interp, "length string", "(length \"hello\")", 5, pass, fail);
    test_eq(interp, "length utf8 string", "(length \"caf\xc3\xa9\")", 4, pass, fail);
    test_str_val(interp, "ref utf8 string", "(ref \"caf\xc3\xa9\" 3)", "\xc3\xa9", pass, fail);
    test_str_val(interp, ".[ ] utf8 string", "(let (s \"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\") s.[1])", "\xe6\x9c\xac", pass, fail);
    test_eq(interp, "length list", "(length '(1 2 3 4))", 4, pass, fail);
    test_eq(interp, "length dotted cons", "(length (cons 1 2))", 2, pass, fail);
    test_eq(interp, "length dotted cons chain", "(length (cons 1 (cons 2 3)))", 3, pass, fail);
    test_error_contains(interp, "length rejects trailing arg", "(length [1 2] 99)", "arity mismatch", pass, fail);
    test_error_contains(interp, "ref rejects trailing arg", "(ref [10 20] 0 99)", "arity mismatch", pass, fail);
    // push!
    setup(interp, "(define ga [1 2])");
    test_tag_interp(interp, "push! returns void", "(push! ga 3)", VOID, pass, fail);
    test_eq(interp, "push! generic", "(length ga)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref ga 2)", 3, pass, fail);
    // keys / values / has? / remove!
    setup(interp, "(define gd {'x 10 'y 20})");
    test_eq(interp, "keys len", "(length (keys gd))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values gd))", 2, pass, fail);
    test_truthy(interp, "has? yes", "(has? gd 'x)", pass, fail);
    test_nil(interp, "has? no", "(has? gd 'z)", pass, fail);
    test_tag_interp(interp, "remove! returns void", "(remove! gd 'x)", VOID, pass, fail);
    test_nil(interp, "remove! gone", "(has? gd 'x)", pass, fail);
    test_eq(interp, "remove! count", "(length gd)", 1, pass, fail);
}

fn void run_advanced_module_qualified_cached_tests(Interp* interp, int* pass, int* fail) {
    test_tag_interp(
        interp,
        "module returns void",
        "(module cmd-void-mod (export value) (define value 1))",
        VOID,
        pass,
        fail
    );
    test_tag_interp(interp, "import returns void", "(import cmd-void-mod)", VOID, pass, fail);

    // Qualified access (default import = qualified-only)
    setup(interp, "(module math-utils (export dbl triple) (define dbl (lambda (x) (* x 2))) (define triple (lambda (x) (* x 3))) (define internal (lambda (x) (+ x 100))))");
    setup(interp, "(import math-utils)");
    test_eq(interp, "module qualified dbl", "(math-utils.dbl 5)", 10, pass, fail);
    test_eq(interp, "module qualified triple", "(math-utils.triple 4)", 12, pass, fail);
    test_error(interp, "module unexported hidden", "(math-utils.internal 5)", pass, fail);

    // Module-owned effects should resolve like any other qualified member.
    setup(interp, "(module effect-mod (export open) (define [effect] (open (^Any payload))))");
    setup(interp, "(import effect-mod)");
    test_truthy(interp, "module effect member value", "(= effect-mod.open 'open)", pass, fail);
    test_eq_interp(interp, "module qualified effect signal",
        "(handle (signal effect-mod.open 42) (effect-mod.open x x))",
        42, pass, fail);

    // Unqualified access should fail without selective import
    test_error(interp, "module unqualified fails", "(triple 4)", pass, fail);
    // Re-import (cached)
    setup(interp, "(import math-utils)");
    test_eq(interp, "module re-import cached", "(math-utils.dbl 7)", 14, pass, fail);
    test_error(interp, "module duplicate def", "(module math-utils (export) (define x 1))", pass, fail);
}

fn void run_advanced_module_selective_rename_tests(Interp* interp, int* pass, int* fail) {
    // Selective import: (import mod (sym1 sym2))
    setup(interp, "(module sel-mod (export add-one sub-one) (define add-one (lambda (x) (+ x 1))) (define sub-one (lambda (x) (- x 1))))");
    setup(interp, "(import sel-mod (add-one))");
    test_eq(interp, "selective import add-one", "(add-one 5)", 6, pass, fail);
    test_error(interp, "selective import sub-one hidden", "(sub-one 5)", pass, fail);
    // Qualified still works alongside selective
    test_eq(interp, "selective + qualified", "(sel-mod.sub-one 5)", 4, pass, fail);

    // Rename import: (import mod (sym 'as alias))
    setup(interp, "(module rename-mod (export compute) (define compute (lambda (x) (* x 10))))");
    setup(interp, "(import rename-mod (compute 'as calc))");
    test_eq(interp, "renamed import", "(calc 3)", 30, pass, fail);
    test_error(interp, "original name hidden after rename", "(compute 3)", pass, fail);
}

fn void run_advanced_module_all_dependency_tests(Interp* interp, int* pass, int* fail) {
    // Import 'all (explicit opt-in)
    setup(interp, "(module all-mod (export foo bar) (define foo (lambda () 42)) (define bar (lambda () 99)))");
    setup(interp, "(import all-mod 'all)");
    test_eq(interp, "import all foo", "(foo)", 42, pass, fail);
    test_eq(interp, "import all bar", "(bar)", 99, pass, fail);

    // Original tests updated to use selective import
    setup(interp, "(module list-utils (export sum-list) (define sum-list (lambda (lst) (foldl + 0 lst))))");
    setup(interp, "(import list-utils (sum-list))");
    test_eq(interp, "module stdlib access", "(sum-list (quote (1 2 3 4 5)))", 15, pass, fail);
    setup(interp, "(module base-mod (export base-fn) (define base-fn (lambda (x) (+ x 100))))");
    setup(interp, "(import base-mod 'all)");
    setup(interp, "(module derived-mod (export derived-fn) (define derived-fn (lambda (x) (base-fn (* x 2)))))");
    setup(interp, "(import derived-mod (derived-fn))");
    test_eq(interp, "module depends on another", "(derived-fn 5)", 110, pass, fail);
    setup(interp, "(module private-mod (export) (define secret 42))");
    setup(interp, "(import private-mod)");
    test_error(interp, "module empty export hides", "secret", pass, fail);
    setup(interp, "(module closure-mod (export make-counter) (define make-counter (lambda () (let (n 0) (lambda () (block (set! n (+ n 1)) n))))))");
    setup(interp, "(import closure-mod (make-counter))");
    setup(interp, "(define my-counter (make-counter))");
    // Counter tests are stateful: JIT-only to avoid double mutation
    test_eq_jit(interp, "module counter 1", "(my-counter)", 1, pass, fail);
    test_eq_jit(interp, "module counter 2", "(my-counter)", 2, pass, fail);
    setup(interp, "(module tail-mod (export tail-sum) (define tail-sum (lambda (n acc) (if (= n 0) acc (tail-sum (- n 1) (+ acc n))))))");
    setup(interp, "(import tail-mod (tail-sum))");
    test_eq(interp, "module tail-call sum 1000", "(tail-sum 1000 0)", 500500, pass, fail);
}

fn void run_advanced_module_reexport_shorthand_tests(Interp* interp, int* pass, int* fail) {
    // Export-from (re-export)
    setup(interp, "(module inner-mod (export inner-val) (define inner-val 42))");
    setup(interp, "(module outer-mod (export inner-val) (import inner-mod) (export-from inner-mod (inner-val)))");
    setup(interp, "(import outer-mod (inner-val))");
    test_eq(interp, "export-from re-export", "inner-val", 42, pass, fail);
    setup(interp, "(module inner-all-mod (export all-a all-b) (define all-a 7) (define all-b 9))");
    setup(interp, "(module outer-all-mod (export all-a all-b) (export-from inner-all-mod 'all))");
    setup(interp, "(import outer-all-mod (all-a all-b))");
    test_eq(interp, "export-from 'all all-a", "all-a", 7, pass, fail);
    test_eq(interp, "export-from 'all all-b", "all-b", 9, pass, fail);

    // Module as first-class value
    setup(interp, "(module val-mod (export x) (define x 77))");
    setup(interp, "(import val-mod)");
    test_eq(interp, "module as value", "(val-mod.x)", 77, pass, fail);

    // === SHORTHAND DEFINE TESTS ===
    setup(interp, "(define (sh-double x) (* x 2))");
    test_eq(interp, "shorthand define", "(sh-double 21)", 42, pass, fail);
    setup(interp, "(define (sh-add3 a b c) (+ a (+ b c)))");
    test_eq(interp, "shorthand define multi", "(sh-add3 10 20 30)", 60, pass, fail);
    setup(interp, "(define (sh-answer) 42)");
    test_eq(interp, "shorthand define zero-arg", "(sh-answer)", 42, pass, fail);

    // export-from is command-style and should evaluate to Void.
    setup(interp, "(module export-ret-source (export val) (define val 5))");
    setup(interp, "(module export-ret-target (export ret-type val) (define ret-type (type-of (export-from export-ret-source (val)))) (export-from export-ret-source (val)))");
    setup(interp, "(import export-ret-target (ret-type val))");
    test_truthy(interp, "export-from return type is Void", "(= ret-type 'Void)", pass, fail);
    test_eq(interp, "export-from still re-exports symbol", "val", 5, pass, fail);
}

fn void run_advanced_parser_export_from_edge_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "parser export-from missing module",
        "(module bad-exp-missing (export) (export-from))",
        "expected module name after export-from",
        pass, fail);

    test_error_contains(interp,
        "parser export-from missing specifier",
        "(module bad-exp-nospec (export) (export-from inner-mod))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from bad specifier",
        "(module bad-exp-spec (export) (export-from inner-mod bogus))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from old :all rejected",
        "(module bad-exp-old (export) (export-from inner-mod :all))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from list non-symbol",
        "(module bad-exp-list (export) (export-from inner-mod (123)))",
        "expected symbol in export-from list",
        pass, fail);

    test_error_contains(interp,
        "parser export-from missing close paren",
        "(module bad-exp-close (export) (export-from inner-mod (a b))",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser export-from nested list element",
        "(module bad-exp-nested (export) (export-from inner-mod ((a))))",
        "expected symbol in export-from list",
        pass, fail);
}

fn void run_advanced_parser_import_edge_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "parser import old :all rejected",
        "(import bad-mod :all)",
        "expected 'all or (specifiers...) after import module",
        pass, fail);

    test_error_contains(interp,
        "parser import old :as rejected",
        "(import bad-mod (x :as y))",
        "expected 'as in import specifier",
        pass, fail);

    test_error_contains(interp,
        "parser import paren missing 'as",
        "(import bad-mod ((x y)))",
        "expected 'as in import specifier",
        pass, fail);

    test_error_contains(interp,
        "parser import paren missing alias",
        "(import bad-mod ((x 'as)))",
        "expected alias after 'as",
        pass, fail);

    test_error_contains(interp,
        "parser import symbol missing alias",
        "(import bad-mod (x 'as))",
        "expected alias after 'as",
        pass, fail);

    test_error_contains(interp,
        "parser import list non-symbol",
        "(import bad-mod (123))",
        "expected symbol in import list",
        pass, fail);

    test_error_contains(interp,
        "parser import missing close paren",
        "(import bad-mod (x y)",
        ")",
        pass, fail);

    {
        // Regression: import path parsing must not truncate at 255 bytes.
        char[384] long_path;
        usz long_path_len = 320;
        for (usz i = 0; i < long_path_len - 1; i++) long_path[i] = 'a';
        long_path[long_path_len - 1] = 'z';
        long_path[long_path_len] = 0;

        char[512] expr_buf;
        Expr* ast = null;
        if (try char[] expr_src = io::bprintf(&expr_buf, "(import \"%s\")", (ZString)&long_path)) {
            ast = parse_for_jit(expr_src, interp);
        }

        bool ok = ast != null &&
                  ast.tag == E_IMPORT &&
                  ast.import_expr.has_path &&
                  ast.import_expr.path != null &&
                  ast.import_expr.path_len == long_path_len &&
                  ast.import_expr.path[long_path_len - 1] == 'z';
        if (ok) {
            emit_pass_literal("[PASS] parser import long path >255");
            (*pass)++;
        } else {
            io::printfn("[FAIL] parser import long path >255 (parsed_len=%d expected=%d)",
                ast != null && ast.tag == E_IMPORT ? (long)ast.import_expr.path_len : -1,
                (long)long_path_len);
            (*fail)++;
        }
    }
}

fn void run_advanced_parser_module_edge_tests(Interp* interp, int* pass, int* fail) {
    Expr* ast = parse_for_jit(
        "(module bad-mod-fail-closed (export x) (define x 1) (if 1 2))",
        interp
    );
    if (ast == null) {
        emit_pass_literal("[PASS] parser module malformed body fails closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] parser module malformed body fails closed");
        (*fail)++;
    }
}

fn void run_advanced_runtime_import_path_edge_tests(Interp* interp, int* pass, int* fail) {
    {
        // Regression: module export growth must fail closed and preserve
        // existing exports when the replacement table cannot be allocated.
        Module mod = {};
        mod.export_capacity = 1;
        mod.export_count = 1;
        mod.exports = jit_alloc_module_exports(1);
        bool ok = mod.exports != null;
        if (ok) {
            mod.exports[0] = interp.symbols.intern("module-export-growth-existing");
            SymbolId* original_exports = mod.exports;
            bool saved_force = g_jit_force_module_export_alloc_fail;
            g_jit_force_module_export_alloc_fail = true;
            bool appended = module_append_export(&mod, interp.symbols.intern("module-export-growth-next"));
            g_jit_force_module_export_alloc_fail = saved_force;
            ok = !appended &&
                 mod.exports == original_exports &&
                 mod.export_capacity == 1 &&
                 mod.export_count == 1 &&
                 mod.exports[0] == interp.symbols.intern("module-export-growth-existing");
        }
        if (mod.exports != null) mem::free(mod.exports);

        if (ok) {
            emit_pass_literal("[PASS] runtime module export growth allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime module export growth allocation failure fails closed");
            (*fail)++;
        }
    }

    {
        // Regression: checked import path resolution must fail closed on truncation.
        char[8] short_buf;
        usz short_len = 0;
        bool ok = !resolve_import_path_checked("long/path", interp, &short_buf, short_buf.len, &short_len);
        if (ok) {
            emit_pass_literal("[PASS] runtime import path checked resolver fails closed on overflow");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import path checked resolver fails closed on overflow");
            (*fail)++;
        }
    }

    {
        // Regression: string-path import cache must key by resolved file path, not raw import symbol.
        SymbolId cached_name = interp.symbols.intern("lib/util.omni");
        Module* cached = jit_create_file_module(cached_name, "/tmp/omni-cached-module-path.omni", interp);
        if (cached != null) cached.loaded = true;

        Expr* ast = parse_for_jit("(import \"lib/util.omni\")", interp);
        bool pushed = push_source_dir("/__omni_missing_source__/main.omni", interp);
        EvalResult load_result = eval_error("test bootstrap failed");
        if (pushed && ast != null) {
            Module* mod_out = null;
            load_result = load_import_target_module(ast, ast.import_expr.name, interp, &mod_out);
        }
        if (pushed) pop_source_dir(interp);

        bool ok = cached != null && ast != null && load_result.error.has_error;
        if (ok) {
            emit_pass_literal("[PASS] runtime import path cache ignores mismatched symbol-only cache entries");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import path cache ignores mismatched symbol-only cache entries");
            (*fail)++;
        }
    }

    {
        // Regression: post-load module resolution falls back to resolved file path.
        SymbolId requested_name = interp.symbols.intern("runtime-import-requested");
        SymbolId loaded_name = interp.symbols.intern("runtime-import-loaded");
        Module* loaded = jit_create_file_module(loaded_name, "/tmp/omni-runtime-path-resolution.omni", interp);
        if (loaded != null) loaded.loaded = true;
        Module* resolved = resolve_loaded_module(requested_name, "/tmp/omni-runtime-path-resolution.omni", interp);

        bool ok = loaded != null && resolved == loaded;
        if (ok) {
            emit_pass_literal("[PASS] runtime import resolves module by path when names differ");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import resolves module by path when names differ");
            (*fail)++;
        }
    }
}

fn void run_advanced_module_system_tests(Interp* interp, int* pass, int* fail) {
    // === MODULE SYSTEM TESTS ===
    run_advanced_module_qualified_cached_tests(interp, pass, fail);
    run_advanced_module_selective_rename_tests(interp, pass, fail);
    run_advanced_module_all_dependency_tests(interp, pass, fail);
    run_advanced_module_reexport_shorthand_tests(interp, pass, fail);
    run_advanced_parser_import_edge_tests(interp, pass, fail);
    run_advanced_parser_export_from_edge_tests(interp, pass, fail);
    run_advanced_parser_module_edge_tests(interp, pass, fail);
    run_advanced_runtime_import_path_edge_tests(interp, pass, fail);
}

fn void run_advanced_collections_module_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_collections_dict_literal_tests(interp, pass, fail);
    run_advanced_collections_generic_ops_tests(interp, pass, fail);
    run_advanced_module_system_tests(interp, pass, fail);
}
