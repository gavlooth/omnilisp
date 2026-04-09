module lisp;

import std::io;
import main;

fn void run_compiler_group_type_dispatch_bridge_tests(Interp* interp, usz* pass, usz* fail) {
    // Type forms should lower to direct AOT helpers and still sync generated globals.
    {
        char[] code = compile_to_c3("(define [type] Box (^Integer value)) (define b (Box 1))", interp);
        bool ok = str_contains(code, "aot::define_type(")
            && str_contains(code, "Box = aot::lookup_var(\"Box\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] lowers directly and syncs constructor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] lowers directly and syncs constructor global"); }
    }

    // Union forms must lower directly and sync generated variant constructors into AOT globals.
    {
        char[] code = compile_to_c3("(define [union] Maybe None (Some Integer)) (Some 1)", interp);
        bool ok = str_contains(code, "aot::define_union(")
            && str_contains(code, "None = aot::lookup_var(\"None\")")
            && str_contains(code, "Some = aot::lookup_var(\"Some\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [union] lowers directly and syncs variant globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [union] lowers directly and syncs variant globals"); }
    }

    // Abstract forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [abstract] Entity)", interp);
        bool ok = str_contains(code, "aot::define_abstract(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [abstract] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [abstract] lowers directly"); }
    }

    // Alias forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [alias] UserAlias User)", interp);
        bool ok = str_contains(code, "aot::define_alias(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [alias] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [alias] lowers directly"); }
    }

    // Effect forms should lower directly through the dedicated helper.
    {
        char[] code = compile_to_c3("(define [effect] (test/ping (^String msg)))", interp);
        bool ok = str_contains(code, "aot::define_effect(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [effect] lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [effect] lowers directly"); }
    }

    // Typed define should install dispatch entries directly through the typed-method helper.
    {
        char[] code = compile_to_c3("(define (describe (^Integer x)) \"int\")", interp);
        bool ok = str_contains(code, "aot::define_typed_method(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define lowers through typed-method helper"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define lowers through typed-method helper"); }
    }

    // Typed define should compile captures normally rather than injecting globals into eval.
    {
        char[] code = compile_to_c3(
            "(define y 10) (define (describe (^Integer x)) (+ x y))",
            interp
        );
        bool ok = str_contains(code, "aot::define_typed_method(")
            && !str_contains(code, "aot::define_var(\"y\", y);")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define no longer uses eval injection"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define no longer uses eval injection"); }
    }

    // Repeated typed define names should not emit duplicate global declarations.
    {
        char[] code = compile_to_c3(
            "(define (describe (^Integer x)) \"int\") (define (describe (^String x)) \"str\")",
            interp
        );
        bool dup = str_contains(code, "lisp::Value* describe;\nlisp::Value* describe;");
        bool ok = !dup;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define globals are deduplicated"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define globals are deduplicated"); }
    }

    // Repeated typed methods should all stay on direct helper lowering (no eval fallback path).
    {
        char[] code = compile_to_c3(
            "(define (route (^Integer x)) (+ x 1)) "
            "(define (route (^String x)) x) "
            "(define (route (^Double x)) (+ x 1.0))",
            interp
        );
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(");
        bool ok = typed_calls == 3
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define multi-method extension stays direct"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define multi-method extension stays direct"); }
    }

    // Typed + untyped mix should preserve direct typed registration and plain fallback define.
    {
        char[] code = compile_to_c3(
            "(define delta 7) "
            "(define (mix (^Integer x)) (+ x delta)) "
            "(define (mix (^String x)) x) "
            "(define (mix x) x)",
            interp
        );
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(");
        bool ok = typed_calls == 2
            && str_contains(code, "mix = ")
            && !str_contains(code, "aot::define_var(\"delta\", delta);")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: typed define fallback mix avoids eval injection"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: typed define fallback mix avoids eval injection"); }
    }

    // Explain dispatch should lower directly instead of delegating through eval.
    {
        char[] code = compile_to_c3("(ref (ref (explain 'dispatch (+ 1 2)) 'decision) 'reason)", interp);
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch lowers directly"); }
    }

    // Explain dispatch direct path: explicit multi-arg call target should stay on helper lowering.
    {
        char[] code = compile_to_c3(
            "(ref (ref (explain 'dispatch (route3 1 2 3)) 'decision) 'reason)",
            interp
        );
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch multi-arg call lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch multi-arg call lowers directly"); }
    }

    // Explain dispatch direct path: explicit zero-arg call target should stay on helper lowering.
    {
        char[] code = compile_to_c3(
            "(ref (ref (explain 'dispatch (route0)) 'decision) 'reason)",
            interp
        );
        bool ok = str_contains(code, "aot::explain_dispatch(")
            && !str_contains(code, "aot::eval_serialized_expr");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch zero-arg call lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch zero-arg call lowers directly"); }
    }

    // Explain effect should also lower directly for signal forms.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (signal io/print \"x\")) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect lowers directly"); }
    }

    // Explain effect should lower directly for perform call-surface spelling.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (perform io/print \"x\")) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect perform call-surface lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect perform call-surface lowers directly"); }
    }

    // Explain effect should lower directly for resolve forms.
    {
        char[] code = compile_to_c3("(ref (explain 'effect (resolve 42)) 'status)", interp);
        bool ok = str_contains(code, "aot::explain_effect_resolve(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain effect resolve lowers directly"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain effect resolve lowers directly"); }
    }

    // Compiler output should not emit serialized-eval bridge calls in normal lowering paths.
    {
        char[] code = compile_to_c3(
            "(define [type] Box (^Integer value)) "
            "(define (render (^Integer x)) (+ x 1)) "
            "(define (render x) x) "
            "(ref (ref (explain 'dispatch (render 1)) 'decision) 'reason)",
            interp
        );
        bool ok = !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serialized-eval bridge is quarantined from normal lowering"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serialized-eval bridge is quarantined from normal lowering"); }
    }

    // Compiled module forms are static: body lowers inline and form returns Void.
    {
        char[] code = compile_to_c3(
            "(module cmod (export add) (define (add x y) (+ x y)))",
            interp
        );
        bool ok = str_contains(code, "add = ")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module lowers as static inline form"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module lowers as static inline form"); }
    }

    // Compiled import form is intentionally static no-op and returns Void.
    {
        char[] code = compile_to_c3(
            "(module imod (export v) (define v 1)) (import imod)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import lowers as static no-op"); }
    }

    // Plain import should keep runtime bridge lowering when module is not compiled inline.
    {
        char[] code = compile_to_c3("(import external_mod)", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, false)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: plain import keeps runtime bridge lowering"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: plain import keeps runtime bridge lowering"); }
    }

    // Plain import 'all keeps runtime bridge lowering and propagates import-all flag.
    {
        char[] code = compile_to_c3("(import external_mod 'all)", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, true)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: plain import all keeps runtime bridge lowering"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: plain import all keeps runtime bridge lowering"); }
    }

    // Path import 'all keeps runtime bridge lowering and propagates import-all flag.
    {
        char[] code = compile_to_c3("(import \"pkg/math.omni\" 'all)", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, true)")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: path import all keeps runtime bridge lowering"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: path import all keeps runtime bridge lowering"); }
    }

    // Path selective import keeps import-all disabled and still performs symbol binding.
    {
        char[] code = compile_to_c3("(import \"pkg/math.omni\" (sum))", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "sum = aot::lookup_var(\"sum\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: path selective import keeps symbol binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: path selective import keeps symbol binding"); }
    }

    // Plain selective alias import keeps alias target binding and original symbol lookup.
    {
        char[] code = compile_to_c3("(import external_mod (sum 'as plus))", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, false)")
            && str_contains(code, "plus = aot::lookup_var(\"sum\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: plain selective alias import keeps symbol binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: plain selective alias import keeps symbol binding"); }
    }

    // Path selective alias import keeps alias target binding and original symbol lookup.
    {
        char[] code = compile_to_c3("(import \"pkg/math.omni\" (sum 'as plus))", interp);
        bool ok = str_contains(code, "aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "plus = aot::lookup_var(\"sum\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: path selective alias import keeps symbol binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: path selective alias import keeps symbol binding"); }
    }

    // Inline import 'all remains static no-op because inline module exports are already global.
    {
        char[] code = compile_to_c3(
            "(module imod-all (export v) (define v 1)) (import imod-all 'all)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-all\"")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline import all lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline import all lowers as static no-op"); }
    }

    // Inline selective import still binds requested names instead of no-oping.
    {
        char[] code = compile_to_c3(
            "(module imod-sel (export v) (define v 1)) (import imod-sel (v))",
            interp
        );
        bool ok = str_contains(code, "aot::import_module(\"imod-sel\", \"\", false, false)")
            && str_contains(code, "v = aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline selective import keeps symbol binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline selective import keeps symbol binding"); }
    }

    // Inline selective alias import keeps alias target binding and original symbol lookup.
    {
        char[] code = compile_to_c3(
            "(module imod-sel-alias (export v) (define v 1)) (import imod-sel-alias (v 'as vv))",
            interp
        );
        bool ok = str_contains(code, "aot::import_module(\"imod-sel-alias\", \"\", false, false)")
            && str_contains(code, "vv = aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline selective alias import keeps symbol binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline selective alias import keeps symbol binding"); }
    }

    // Non-top-level import keeps runtime bridge side effects in expression lowering.
    {
        char[] code = compile_to_c3("(block (import external_mod) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"external_mod\", \"\", false, false)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level import keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level import keeps runtime bridge side effects"); }
    }

    // Non-top-level import 'all keeps runtime bridge side effects and import-all flag.
    {
        char[] code = compile_to_c3("(block (import external_mod 'all) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"external_mod\", \"\", false, true)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level import all keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level import all keeps runtime bridge side effects"); }
    }

    // Non-top-level path import 'all keeps runtime bridge side effects and import-all flag.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" 'all) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, true)")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path import all keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path import all keeps runtime bridge side effects"); }
    }

    // Non-top-level selective alias import preserves runtime alias binding.
    {
        char[] code = compile_to_c3("(block (import external_mod (sum 'as plus)) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"external_mod\", \"\", false, false)")
            && str_contains(code, "aot::define_var(\"plus\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level selective alias import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level selective alias import keeps runtime binding"); }
    }

    // Non-top-level path selective import preserves runtime binding.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" (sum)) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "aot::define_var(\"sum\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path selective import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path selective import keeps runtime binding"); }
    }

    // Non-top-level path selective alias import preserves runtime alias binding.
    {
        char[] code = compile_to_c3("(block (import \"pkg/math.omni\" (sum 'as plus)) 0)", interp);
        bool ok = str_contains(code, "(void)aot::import_module(\"pkg/math.omni\", \"pkg/math.omni\", true, false)")
            && str_contains(code, "aot::define_var(\"plus\", aot::lookup_var(\"sum\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level path selective alias import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level path selective alias import keeps runtime binding"); }
    }

    // Non-top-level inline selective import is not no-op: it keeps runtime binding semantics.
    {
        char[] code = compile_to_c3(
            "(module imod-block-sel (export v) (define v 1)) (block (import imod-block-sel (v)) 0)",
            interp
        );
        bool ok = str_contains(code, "(void)aot::import_module(\"imod-block-sel\", \"\", false, false)")
            && str_contains(code, "aot::define_var(\"v\", aot::lookup_var(\"v\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline selective import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline selective import keeps runtime binding"); }
    }

    // Non-top-level inline selective alias import is not no-op: it keeps runtime alias binding.
    {
        char[] code = compile_to_c3(
            "(module imod-block-sel-alias (export v) (define v 1)) (block (import imod-block-sel-alias (v 'as vv)) 0)",
            interp
        );
        bool ok = str_contains(code, "(void)aot::import_module(\"imod-block-sel-alias\", \"\", false, false)")
            && str_contains(code, "aot::define_var(\"vv\", aot::lookup_var(\"v\"))")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline selective alias import keeps runtime binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline selective alias import keeps runtime binding"); }
    }

    // Non-top-level inline import 'all remains static no-op for compiled-inline modules.
    {
        char[] code = compile_to_c3(
            "(module imod-block-all (export v) (define v 1)) (block (import imod-block-all 'all) 0)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && !str_contains(code, "(void)aot::import_module(\"imod-block-all\"")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level inline import all lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level inline import all lowers as static no-op"); }
    }

    // Non-top-level inline import remains static no-op when module is compiled inline.
    {
        char[] code = compile_to_c3(
            "(module imod-block (export v) (define v 1)) (block (import imod-block) 0)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && !str_contains(code, "(void)aot::import_module(\"imod-block\"")
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
        bool ok = str_contains(code, "export-from: no-op in compiled mode")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: export-from lowers as static no-op"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: export-from lowers as static no-op"); }
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
