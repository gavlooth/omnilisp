module lisp;


import std::io;
import main;

fn void run_advanced_collections_dict_literal_tests(Interp* interp, int* pass, int* fail) {
    // Dicts
    setup(interp, "(define hm1 (dict 1 10 2 20 3 30))");
    test_eq(interp, "ref dict", "(ref hm1 2)", 20, pass, fail);
    test_nil(interp, "ref dict missing", "(ref hm1 99)", pass, fail);
    setup(interp, "(dict-set! hm1 2 200)");
    test_eq(interp, "dict-set! overwrite", "(ref hm1 2)", 200, pass, fail);
    test_truthy(interp, "has? dict yes", "(has? hm1 1)", pass, fail);
    test_nil(interp, "has? dict no", "(has? hm1 99)", pass, fail);
    test_eq(interp, "length dict", "(length hm1)", 3, pass, fail);
    setup(interp, "(define hm2 (dict 1 10 2 20 3 30))");
    setup(interp, "(remove! hm2 2)");
    test_nil(interp, "remove! dict", "(has? hm2 2)", pass, fail);
    test_eq(interp, "remove! count", "(length hm2)", 2, pass, fail);
    setup(interp, "(define hm3 (dict 1 10 2 20))");
    test_eq(interp, "keys len", "(length (keys hm3))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values hm3))", 2, pass, fail);
    setup(interp, "(define hm4 (dict \"name\" \"Alice\" \"age\" 30))");
    test_str(interp, "dict string key", "(ref hm4 \"name\")", pass, fail);
    setup(interp, "(define hm5 (dict))");
    test_eq(interp, "dict empty count", "(length hm5)", 0, pass, fail);
    test_truthy(interp, "dict? yes", "(dict? (dict 1 2))", pass, fail);
    test_nil(interp, "dict? no", "(dict? 42)", pass, fail);
    setup(interp, "(define hm-big (dict))");
    setup(interp, "(let loop (i 0) (if (= i 50) hm-big (block (dict-set! hm-big i (* i i)) (loop (+ i 1)))))");
    test_eq(interp, "dict auto-grow count", "(length hm-big)", 50, pass, fail);
    test_eq(interp, "dict auto-grow ref 7", "(ref hm-big 7)", 49, pass, fail);

    // === LITERAL SYNTAX TESTS ===
    // Dict literal {}
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
    // ref - string
    test_eq(interp, "ref string", "(ref \"hello\" 0)", 104, pass, fail);
    // negative indexing (Python-style: -1 = last, -2 = second-to-last)
    test_eq(interp, "ref array -1", "(ref [10 20 30] -1)", 30, pass, fail);
    test_eq(interp, "ref array -2", "(ref [10 20 30] -2)", 20, pass, fail);
    test_eq(interp, "ref array -3", "(ref [10 20 30] -3)", 10, pass, fail);
    test_error(interp, "ref array -4 oob", "(ref [10 20 30] -4)", pass, fail);
    test_eq(interp, "ref string -1", "(ref \"hello\" -1)", 111, pass, fail);
    test_eq(interp, "ref string -5", "(ref \"hello\" -5)", 104, pass, fail);
    test_error(interp, "ref string -6 oob", "(ref \"hello\" -6)", pass, fail);
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
    setup(interp, "(array-set! neg-arr -1 99)");
    test_eq(interp, "array-set! -1", "(ref neg-arr 2)", 99, pass, fail);
    // length - generic
    test_eq(interp, "length array", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "length dict", "(length {'a 1 'b 2})", 2, pass, fail);
    test_eq(interp, "length string", "(length \"hello\")", 5, pass, fail);
    test_eq(interp, "length list", "(length '(1 2 3 4))", 4, pass, fail);
    // push!
    setup(interp, "(define ga [1 2])");
    setup(interp, "(push! ga 3)");
    test_eq(interp, "push! generic", "(length ga)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref ga 2)", 3, pass, fail);
    // keys / values / has? / remove!
    setup(interp, "(define gd {'x 10 'y 20})");
    test_eq(interp, "keys len", "(length (keys gd))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values gd))", 2, pass, fail);
    test_truthy(interp, "has? yes", "(has? gd 'x)", pass, fail);
    test_nil(interp, "has? no", "(has? gd 'z)", pass, fail);
    setup(interp, "(remove! gd 'x)");
    test_nil(interp, "remove! gone", "(has? gd 'x)", pass, fail);
    test_eq(interp, "remove! count", "(length gd)", 1, pass, fail);
}

fn void run_advanced_module_qualified_cached_tests(Interp* interp, int* pass, int* fail) {
    // Qualified access (default import = qualified-only)
    setup(interp, "(module math-utils (export dbl triple) (define dbl (lambda (x) (* x 2))) (define triple (lambda (x) (* x 3))) (define internal (lambda (x) (+ x 100))))");
    setup(interp, "(import math-utils)");
    test_eq(interp, "module qualified dbl", "(math-utils.dbl 5)", 10, pass, fail);
    test_eq(interp, "module qualified triple", "(math-utils.triple 4)", 12, pass, fail);
    test_error(interp, "module unexported hidden", "(math-utils.internal 5)", pass, fail);
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
        "parser export-from legacy :all rejected",
        "(module bad-exp-legacy (export) (export-from inner-mod :all))",
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
        "parser import legacy :all rejected",
        "(import bad-mod :all)",
        "expected 'all or (specifiers...) after import module",
        pass, fail);

    test_error_contains(interp,
        "parser import legacy :as rejected",
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
        char[] expr_src = io::bprintf(&expr_buf, "(import \"%s\")", (ZString)&long_path)!!;
        Expr* ast = parse_for_jit(expr_src, interp);

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

fn void run_advanced_module_system_tests(Interp* interp, int* pass, int* fail) {
    // === MODULE SYSTEM TESTS ===
    run_advanced_module_qualified_cached_tests(interp, pass, fail);
    run_advanced_module_selective_rename_tests(interp, pass, fail);
    run_advanced_module_all_dependency_tests(interp, pass, fail);
    run_advanced_module_reexport_shorthand_tests(interp, pass, fail);
    run_advanced_parser_import_edge_tests(interp, pass, fail);
    run_advanced_parser_export_from_edge_tests(interp, pass, fail);
}

fn void run_advanced_collections_module_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_collections_dict_literal_tests(interp, pass, fail);
    run_advanced_collections_generic_ops_tests(interp, pass, fail);
    run_advanced_module_system_tests(interp, pass, fail);
}

