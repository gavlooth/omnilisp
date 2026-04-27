module lisp;

import std::io;
import main;

fn void run_compiler_group_type_dispatch_bridge_tests(Interp* interp, usz* pass, usz* fail) {
    run_compiler_group_type_dispatch_bridge_core_tests(interp, pass, fail);

    // Non-top-level import 'all keeps runtime bridge side effects and import-all flag.
    {
        char[] code = compile_to_c3("(block (import external_mod 'all) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, true)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level import all keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level import all keeps runtime bridge side effects"); }
    }

    // Non-top-level path import 'all keeps runtime bridge side effects and import-all flag.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" 'all) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, true)")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path import all keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path import all keeps runtime bridge side effects"); }
    }

    // Non-top-level selective alias import preserves runtime alias binding.
    {
        char[] code = compile_to_c3("(block (import external_mod (sum 'as plus)) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, false)")
            && str_contains(code, "aot::define_var(\"plus\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level selective alias import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level selective alias import keeps runtime binding"); }
    }

    // Non-top-level path selective import preserves runtime binding.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" (sum)) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "aot::define_var(\"sum\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path selective import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path selective import keeps runtime binding"); }
    }

    // Non-top-level path selective alias import preserves runtime alias binding.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" (sum 'as plus)) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "aot::define_var(\"plus\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path selective alias import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path selective alias import keeps runtime binding"); }
    }

    // Non-top-level inline selective import binds runtime env from private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-block-sel (export v) (define v 1)) (block (import imod-block-sel (v)) 0)",
            interp
        );
        bool ok = str_contains(code, "aot::define_var(\"v\", _aot_mod_")
            && str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-block-sel\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline selective import binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline selective import binds private backing"); }
    }

    // Non-top-level inline selective alias import binds runtime env alias from private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-block-sel-alias (export v) (define v 1)) (block (import imod-block-sel-alias (v 'as vv)) 0)",
            interp
        );
        bool ok = str_contains(code, "aot::define_var(\"vv\", _aot_mod_")
            && str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-block-sel-alias\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline selective alias import binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline selective alias import binds private backing"); }
    }

    // Non-top-level inline import 'all binds runtime env exports from private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-block-all (export v) (define v 1)) (block (import imod-block-all 'all) 0)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && str_contains(code, "aot::define_var(\"v\", _aot_mod_")
            && !str_contains(code, "aot::import_module(\"imod-block-all\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline import all binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline import all binds private backing"); }
    }

    // Non-top-level inline import remains static no-op when module is compiled inline.
    {
        char[] code = compile_to_c3(
            "(module imod-block (export v) (define v 1)) (block (import imod-block) 0)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-block\"")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline import lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline import lowers as static no-op"); }
    }

    // Compiled export-from form is intentionally static no-op and returns Void.
    {
        char[] code = compile_to_c3(
            "(module exp-src (export z) (define z 9)) "
            "(module exp-dst (export z) (export-from exp-src (z)))",
            interp
        );
        bool ok = str_contains(code, "export-from: compiled inline bridge")
            && str_contains(code, "_aot_mod_")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: export-from lowers through private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: export-from lowers through private backing"); }
    }

    // Inline export-from re-exports become public module exports, not only private locals.
    {
        char[] code = compile_to_c3(
            "(module reexp-src (export x) (define x 1)) "
            "(module reexp-dst (export) (export-from reexp-src (x))) "
            "(import reexp-dst (x)) x",
            interp
        );
        bool ok = str_contains(code, "export-from: compiled inline bridge")
            && str_contains(code, "x = _aot_mod_")
            && !str_contains(code, "inline selective import could not resolve export backing")
            && !str_contains(code, "inline module export has no compiled private backing")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: export-from selected re-export is importable"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: export-from selected re-export is importable"); }
    }

    // Runtime-module export-from would otherwise compile as a successful no-op.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(module runtime-reexp (export abs) (export-from math (abs))) "
            "(with runtime-reexp (abs -4))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "export-from runtime modules is not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: runtime export-from fails closed in AOT"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: runtime export-from fails closed in AOT"); }
    }

    // Module-local scanner must see definitions introduced inside module-body with forms.
    {
        char[] code = compile_to_c3(
            "(module with-src (export x) (define x 7)) "
            "(module with-dst (export y) (with with-src (define y x))) "
            "(import with-dst (y))",
            interp
        );
        bool ok = str_contains(code, "_aot_with_")
            && str_contains(code, "y = _aot_mod_")
            && !str_contains(code, "inline module export has no compiled private backing")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module with-body defines receive private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module with-body defines receive private backing"); }
    }

    // Selective imports inside module bodies bind module-private exported values.
    {
        char[] code = compile_to_c3(
            "(module import-src (export x) (define x 7)) "
            "(module import-dst (export x) (import import-src (x))) "
            "(import import-dst (x))",
            interp
        );
        bool ok = str_contains(code, "aot::define_var(\"x\"")
            && str_contains(code, "x = _aot_mod_")
            && !str_contains(code, "inline module export has no compiled private backing")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module import exports sync private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module import exports sync private backing"); }
    }

    // Runtime-module import-all inside an inline module is unsupported until AOT can enumerate exports.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(module runtime-import-all (export abs) (import math 'all)) "
            "(with runtime-import-all (abs -3))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "module-body import-all from runtime modules is not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: runtime module import-all fails closed in AOT modules"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: runtime module import-all fails closed in AOT modules"); }
    }

    // Lexical effect declarations inside inline modules must not publish private bindings.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(module module-lex-eff (export modLetEff) "
            "  (let (x 1) (define [effect] (modLetEff (^Any p))))) "
            "(with module-lex-eff modLetEff)",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "lexical effect declarations are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module lexical effect declaration fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module lexical effect declaration fails closed"); }
    }

    // Inline module bodies can contain nested definitions without leaking public globals.
    {
        char[] code = compile_to_c3(
            "(module nested-mod (export nested-value) "
            "  (if true (define nested-value 42) nil)) "
            "(with nested-mod nested-value)",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && !str_contains(code, "lisp::Value* nested_value;")
            && !str_contains(code, "nested_value =")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module nested defines use private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module nested defines use private backing"); }
    }

    // Nested module-body FFI bindings write through private backing targets.
    {
        char[] code = compile_to_c3(
            "(module nested-ffi-mod (export strlen) "
            "  (if true "
            "    (block "
            "      (define [ffi lib] libc \"libc.so.6\") "
            "      (define [ffi λ libc] (strlen (^String s)) ^Integer)) "
            "    nil)) "
            "(with nested-ffi-mod strlen)",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && !str_contains(code, "lisp::Value* strlen;")
            && !str_contains(code, "strlen =")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module nested FFI uses private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module nested FFI uses private backing"); }
    }

    // Nested module-body effect tags write through private backing targets.
    {
        char[] code = compile_to_c3(
            "(module nested-effect-mod (export opened) "
            "  (if true (block (define [effect] (opened (^Any payload)))) nil)) "
            "(with nested-effect-mod opened)",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && str_contains(code, "aot::define_effect(\"opened\"")
            && str_contains(code, "aot::lookup_var(\"opened\")")
            && !str_contains(code, "lisp::Value* opened;")
            && !str_contains(code, "opened =")
            && !str_contains(code, "_aot_mod_0_0 = _r")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module nested effects use private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module nested effects use private backing"); }
    }

    // Nested module-body type forms sync helper-published bindings through private backing targets.
    {
        char[] code = compile_to_c3(
            "(module nested-type-mod (export NestedShape NestedMaybe NestedSome) "
            "  (if true "
            "    (block "
            "      (define [abstract] NestedShape) "
            "      (define [union] NestedMaybe NestedNone (NestedSome Integer))) "
            "    nil)) "
            "(with nested-type-mod NestedShape)",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && str_contains(code, "aot::define_abstract(\"NestedShape\"")
            && str_contains(code, "aot::define_union(\"NestedMaybe\"")
            && str_contains(code, "aot::lookup_var(\"NestedShape\")")
            && str_contains(code, "aot::lookup_var(\"NestedMaybe\")")
            && str_contains(code, "aot::lookup_var(\"NestedSome\")")
            && !str_contains(code, "lisp::Value* NestedShape;")
            && !str_contains(code, "NestedShape =")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module nested type forms use private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module nested type forms use private backing"); }
    }

    // Direct module-body effect exports bind the published effect symbol, not the command return value.
    {
        char[] code = compile_to_c3(
            "(module effect-export-mod (export openedM) "
            "  (define [effect] (openedM (^Any payload)))) "
            "(import effect-export-mod (openedM)) "
            "openedM",
            interp
        );
        bool ok = str_contains(code, "openedM = _aot_mod_")
            && str_contains(code, "aot::define_effect(\"openedM\"")
            && str_contains(code, "aot::lookup_var(\"openedM\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module effect export syncs symbol backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module effect export syncs symbol backing"); }
    }

    // Inline modules execute helper-published declarations in an isolated runtime module env.
    {
        char[] code = compile_to_c3(
            "(module isolated-effect-mod (export openedIso) "
            "  (define [effect] (privateIso (^Any payload))) "
            "  (define openedIso 1)) "
            "(with isolated-effect-mod openedIso)",
            interp
        );
        bool ok = compiler_code_contains_ordered(code, {
                "aot::push_module_env()",
                "defer aot::pop_module_env(",
                "aot::define_effect(\"privateIso\"",
                "aot::lookup_var(\"privateIso\")"
            })
            && str_contains(code, "_aot_mod_")
            && !str_contains(code, "lisp::Value* privateIso;")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module helper declarations use isolated runtime env"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module helper declarations use isolated runtime env"); }
    }

    // Terminal resolve in an inline module body must stop later module parent work.
    {
        char[] code = compile_to_c3(
            "(handle (signal e 0) "
            "  (e v (module terminal-mod (export x y) "
            "         (define x (resolve 1)) "
            "         (define y 2))))",
            interp
        );
        bool ok = compiler_code_contains_ordered(code, {
                "aot::push_module_env()",
                "defer aot::pop_module_env(",
                "aot::compiled_resolve(",
                "aot::sequence_should_stop(",
                "return _r"
            })
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module terminal resolve guards parent work"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module terminal resolve guards parent work"); }
    }

    // Multi-export export-from all grows private backing storage and re-exports every name.
    {
        char[] code = compile_to_c3(
            "(module exp-all-src (export a b c d e f g h i j k l m n o p q r) "
            "  (define a 1) (define b 2) (define c 3) (define d 4) (define e 5) "
            "  (define f 6) (define g 7) (define h 8) (define i 9) (define j 10) "
            "  (define k 11) (define l 12) (define m 13) (define n 14) (define o 15) "
            "  (define p 16) (define q 17) (define r 18)) "
            "(module exp-all-dst (export a b c d e f g h i j k l m n o p q r) "
            "  (export-from exp-all-src 'all)) "
            "(import exp-all-dst 'all)",
            interp
        );
        bool ok = str_contains(code, "export-from: compiled inline bridge")
            && str_contains(code, "r = _aot_mod_")
            && !str_contains(code, "private binding capacity exhausted")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: export-from all grows private backing table"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: export-from all grows private backing table"); }
    }

    // Exported names must have an inline backing; dangling exports fail closed.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(module missing-backing (export missing-value) (define other-value 1))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "inline module export has no compiled private backing");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module dangling export fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module dangling export fails closed"); }
    }

    // Nested inline module declarations receive private backing globals before lowering.
    {
        char[] code = compile_to_c3(
            "(module outer-nested (export y) "
            "  (module inner-nested (export x) (define x 1)) "
            "  (define y (with inner-nested x))) "
            "(import outer-nested (y))",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && str_contains(code, "y = _aot_mod_")
            && !str_contains(code, "lisp::Value* x;")
            && !str_contains(code, "\n    x =")
            && !str_contains(code, "missing private backing")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: nested inline modules declare private backing globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: nested inline modules declare private backing globals"); }
    }

    // Duplicate inline module names must fail closed instead of reusing the first backing table.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(module dup-outer-a (export x) "
            "  (module dup-inner (export v) (define v 1)) "
            "  (define x (with dup-inner v))) "
            "(module dup-outer-b (export y) "
            "  (module dup-inner (export v) (define v 2)) "
            "  (define y (with dup-inner v)))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "duplicate inline module name");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: duplicate inline module names fail closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: duplicate inline module names fail closed"); }
    }

    // Literal-dead branches do not define modules at runtime and should not trip duplicate detection.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (if false (module dup-dead (export x) (define x 1)) nil) "
            "  (module dup-dead (export x) (define x 2)) "
            "  (import dup-dead (x)) "
            "  x)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && str_contains(code, "aot::define_var(\"x\", _aot_mod_")
            && !str_contains(code, "duplicate inline module name")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: literal-dead inline module does not reserve duplicate metadata"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: literal-dead inline module does not reserve duplicate metadata"); }
    }

    // Primitive-shadow scopes must not poison later literal branch folding.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (let (false true) 0) "
            "  (if false (module dup-post-shadow (export x) (define x 1)) nil) "
            "  (module dup-post-shadow (export x) (define x 2)) "
            "  (with dup-post-shadow x))",
            interp
        );
        bool ok = str_contains(code, "aot::push_module_env()")
            && str_contains(code, "_aot_mod_")
            && str_contains(code, "_aot_with_")
            && !str_contains(code, "duplicate inline module name")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: primitive shadow scope does not poison later literal module pruning"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: primitive shadow scope does not poison later literal module pruning"); }
    }

    // Branch-local primitive shadow declarations must not suppress sibling declarations.
    {
        char[] code = compile_to_c3(
            "(if (= 1 2) (let (false 1) false) (let (false 2) false))",
            interp
        );
        bool ok = str_contains(code, "lisp::Value* _aot_let_")
            && !str_contains(code, "lisp::Value* _omni_false")
            && !str_contains(code, "_omni_false =")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: primitive shadow declarations are branch scoped"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: primitive shadow declarations are branch scoped"); }
    }

    // Let locals should not leak into later expressions in the same block.
    {
        char[] code = compile_to_c3("(block (let (x 1) x) x)", interp);
        bool ok = str_contains(code, "aot::lookup_var(\"x\")")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: let locals do not leak after lexical scope"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: let locals do not leak after lexical scope"); }
    }

    // Short-circuit branch-local lets should not leak to the following expression.
    {
        char[] code = compile_to_c3("(block (and (let (auditLetScope true) auditLetScope) (let (auditLetScope 2) auditLetScope)) auditLetScope)", interp);
        bool ok = str_contains(code, "aot::lookup_var(\"auditLetScope\")")
            && str_contains(code, "lisp::Value* _aot_let_")
            && !str_contains(code, "lisp::Value* auditLetScope")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: short-circuit let locals stay branch scoped"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: short-circuit let locals stay branch scoped"); }
    }

    // Match clause pattern locals are sibling C-scope declarations, not shared declarations.
    {
        char[] code = compile_to_c3("(match 2 (auditMatchScope auditMatchScope) (auditMatchScope auditMatchScope))", interp);
        bool ok = compiler_str_count(code, "lisp::Value* auditMatchScope") == 2
            && !str_contains(code, "} else {\n        auditMatchScope =")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match sibling pattern declarations are clause scoped"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match sibling pattern declarations are clause scoped"); }
    }

    // Runtime-dependent module exports must fail closed instead of leaving nil backings.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (define flag false) "
            "  (module dynmod (export x) (if flag (define x 1) nil)) "
            "  (with dynmod x))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "conditional inline module exports are not definitely assigned in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: conditional inline module exports fail closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: conditional inline module exports fail closed"); }
    }

    // Nested inline modules in executable containers are still usable by export-from.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (module outerblk (export x) "
            "    (block "
            "      (module innerblk (export x) (define x 1)) "
            "      (export-from innerblk (x)))) "
            "  (with outerblk x))",
            interp
        );
        bool ok = str_contains(code, "export-from: compiled inline bridge")
            && !str_contains(code, "export-from runtime modules is not supported in AOT")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: nested inline export-from source metadata is registered"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: nested inline export-from source metadata is registered"); }
    }

    // Checkpoint wrapper bodies cannot currently publish module exports safely in AOT.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(module reset-type-mod (export out) "
            "  (checkpoint (block (define [abstract] ResetShape) (define out ResetShape)))) "
            "(with reset-type-mod out)",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "checkpoint inline module exports are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint inline module export publication fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint inline module export publication fails closed"); }
    }

    // Checkpoint match guards cannot publish exports through runtime-dependent predicates.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (module reset-guard-mod (export out) "
            "    (checkpoint (match 1 ((? (block (define out 7) true)) out) (_ 0)))) "
            "  (with reset-guard-mod out))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "checkpoint inline module exports are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: checkpoint guard export publication fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: checkpoint guard export publication fails closed"); }
    }

    // Short-circuit module exports must not publish a nil backing when the RHS does not run.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (module and-export-mod (export x) (and false (define x 1))) "
            "  (with and-export-mod x))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "short-circuit inline module exports are not definitely assigned in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: and RHS inline module export fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: and RHS inline module export fails closed"); }
    }

    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (module or-export-mod (export x) (or true (define x 1))) "
            "  (with or-export-mod x))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "short-circuit inline module exports are not definitely assigned in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: or RHS inline module export fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: or RHS inline module export fails closed"); }
    }

    // Match clause exports are runtime-dependent until AOT models definite assignment per clause.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (module match-export-mod (export x) (match 0 (1 (define x 1)) (_ nil))) "
            "  (with match-export-mod x))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "match inline module exports are not definitely assigned in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match clause inline module export fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match clause inline module export fails closed"); }
    }

    // Handle clauses cannot currently publish module exports safely through the AOT bridge.
    {
        char[] err;
        char[] code = compile_to_c3(
            "(block "
            "  (module handle-clause-mod (export out) "
            "    (handle (signal make 0) (make v (block (define out 7) out)))) "
            "  (with handle-clause-mod out))",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "handle inline module exports are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle clause inline module export fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle clause inline module export fails closed"); }
    }

    // Strict handles must thread the strict bit through the AOT bridge.
    {
        char[] code = compile_to_c3(
            "(handle ^strict (signal my-eff 42) (other-eff v v))",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_handle(")
            && str_contains(code, ", true);")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: strict handle lowering preserves strict mode in AOT bridge"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: strict handle lowering preserves strict mode in AOT bridge"); }
    }

    // Handle lowering must emit every clause, not just the first eight.
    {
        char[] code = compile_to_c3(
            "(handle (signal eff0 0) "
            "(eff0 x x) (eff1 x x) (eff2 x x) (eff3 x x) (eff4 x x) "
            "(eff5 x x) (eff6 x x) (eff7 x x) (eff8 x x))",
            interp
        );
        bool ok = str_contains(code, "aot::make_symbol(\"eff8\")")
            && str_contains(code, "[9] _eff_tags_")
            && str_contains(code, "[9] _eff_hdlrs_")
            && str_contains(code, "] = _r")
            && str_contains(code, ", 9, ")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT handle lowering emits every clause beyond eight"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT handle lowering emits every clause beyond eight"); }
    }
}
