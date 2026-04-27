module lisp;

import std::io;
import main;

fn void run_compiler_group_type_dispatch_bridge_core_tests(Interp* interp, usz* pass, usz* fail) {
    // Type forms should lower to direct AOT helpers and still sync generated globals.
    {
        char[] code = compile_to_c3("(define [type] Box (^Integer value)) (define b (Box 1))", interp);
        bool ok = str_contains(code, "aot::define_type(")
            && str_contains(code, "aot::lookup_var(\"Box\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] lowers directly and syncs constructor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] lowers directly and syncs constructor global"); }
    }

    // Dict metadata annotations have no base type; the AOT bridge must keep the metadata payload.
    {
        char[] code = compile_to_c3("(define [type] (NumBox T) (^{'T Number} value))", interp);
        bool ok = str_contains(code, "aot::define_type(")
            && str_contains(code, ".is_dict = true")
            && str_contains(code, ".base_type = \"\"")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [type] lowers dict metadata annotations without a base type"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [type] lowers dict metadata annotations without a base type"); }
    }

    // Union forms must lower directly and sync generated variant constructors into AOT globals.
    {
        char[] code = compile_to_c3("(define [union] Maybe None (Some Integer)) (format \"%s\" Maybe) (Some 1)", interp);
        bool ok = str_contains(code, "aot::define_union(")
            && str_contains(code, "aot::lookup_var(\"Maybe\")")
            && str_contains(code, "aot::lookup_var(\"None\")")
            && str_contains(code, "aot::lookup_var(\"Some\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [union] lowers directly and syncs head plus variant globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [union] lowers directly and syncs head plus variant globals"); }
    }

    // Abstract forms should lower directly through the dedicated helper and sync the descriptor global.
    {
        char[] code = compile_to_c3("(define [abstract] Entity) (format \"%s\" Entity)", interp);
        bool ok = str_contains(code, "aot::define_abstract(")
            && str_contains(code, "aot::lookup_var(\"Entity\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [abstract] lowers directly and syncs descriptor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [abstract] lowers directly and syncs descriptor global"); }
    }

    // Alias forms should lower directly through the dedicated helper and sync the descriptor global.
    {
        char[] code = compile_to_c3("(define [alias] UserAlias User) (format \"%s\" UserAlias)", interp);
        bool ok = str_contains(code, "aot::define_alias(")
            && str_contains(code, "aot::lookup_var(\"UserAlias\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [alias] lowers directly and syncs descriptor global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [alias] lowers directly and syncs descriptor global"); }
    }

    // Effect forms should lower directly through the dedicated helper and sync the generated global.
    {
        char[] code = compile_to_c3("(define [effect] (testPing (^String msg))) testPing", interp);
        bool ok = str_contains(code, "aot::define_effect(")
            && str_contains(code, "lisp::Value* testPing;")
            && str_contains(code, "testPing = aot::lookup_var(\"testPing\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: [effect] lowers directly and syncs effect global"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: [effect] lowers directly and syncs effect global"); }
    }

    // Block-local effect forms publish through the runtime environment; the
    // command-style nil return must not be assigned to the generated C3 binding.
    {
        char[] code = compile_to_c3(
            "(block (define [effect] (blocked (^Any payload))) blocked)",
            interp
        );
        bool ok = str_contains(code, "aot::define_effect(\"blocked\"")
            && str_contains(code, "aot::lookup_var(\"blocked\")")
            && str_contains(code, "lisp::Value* blocked;")
            && str_contains(code, "blocked = aot::lookup_var(\"blocked\")")
            && !str_contains(code, "blocked = _r")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: block effect declaration syncs helper-published binding"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: block effect declaration syncs helper-published binding"); }
    }

    // Lexical effect declarations would leak through AOT's global helper env;
    // until lexical helper envs exist, they must fail closed instead.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(block (if true (let (x 1) (define [effect] (branchLetEff (^Any p)))) nil) branchLetEff)",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "lexical effect declarations are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexical let effect declaration fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexical let effect declaration fails closed"); }
    }

    // Match clause result scopes must not publish effect declarations as globals.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(block (match 1 (1 (block (let (x 1) (define [effect] (matchLetEff (^Any p)))))) (_ nil)) matchLetEff)",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "lexical effect declarations are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexical match effect declaration fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexical match effect declaration fails closed"); }
    }

    // Match guard predicates have their own lexical scope and must not publish effects.
    {
        char[] err = "";
        char[] code = compile_to_c3(
            "(block (match 1 ((? (block (define [effect] (guardLetEff (^Any p))) true) x) x) (_ nil)) guardLetEff)",
            interp,
            &err
        );
        bool ok = code.len == 0
            && str_contains(err, "lexical effect declarations are not supported in AOT");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexical match guard effect declaration fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexical match guard effect declaration fails closed"); }
    }

    // Block-local type forms sync helper-published descriptor and constructor globals.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (define [abstract] BlockShape) "
            "  (define [union] BlockMaybe BlockNone (BlockSome Integer)) "
            "  (define [alias] BlockAlias Integer) "
            "  (BlockSome 1))",
            interp
        );
        bool ok = str_contains(code, "aot::define_abstract(\"BlockShape\"")
            && str_contains(code, "aot::define_union(\"BlockMaybe\"")
            && str_contains(code, "aot::define_alias(\"BlockAlias\"")
            && str_contains(code, "aot::lookup_var(\"BlockShape\")")
            && str_contains(code, "aot::lookup_var(\"BlockMaybe\")")
            && str_contains(code, "aot::lookup_var(\"BlockNone\")")
            && str_contains(code, "aot::lookup_var(\"BlockSome\")")
            && str_contains(code, "aot::lookup_var(\"BlockAlias\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: block type forms sync helper-published globals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: block type forms sync helper-published globals"); }
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

    // Literal-singleton nil annotations should stay nil literals through AOT metadata emission.
    {
        char[] code = compile_to_c3("(define (nil-route (^#nil x)) 1)", interp);
        bool ok = str_contains(code, ".has_val_literal = true")
            && str_contains(code, ".val_tag = lisp::ValueTag.NIL")
            && str_contains(code, ".val_sym = \"\"");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: Literal nil singleton metadata emits as NIL"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: Literal nil singleton metadata emits as NIL"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        TypeAnnotation ann = {};
        ann.has_annotation = true;
        ann.is_compound = true;
        ann.base_type = interp.sym_Literal;
        ann.has_val_literal = true;
        ann.val_literal.tag = TENSOR;

        compiler.emit_aot_type_annotation_spec_init("_ann", &ann);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unsupported type literal tag in AOT metadata");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: Literal metadata rejects unsupported value tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: Literal metadata rejects unsupported value tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        compiler.emit_value_tag_literal(TENSOR);
        char[] code = compiler.get_output();
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "cannot emit unsupported value tag literal") &&
            str_contains(code, "lisp::ValueTag.NIL");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value tag literal emitter rejects unsupported tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value tag literal emitter rejects unsupported tags"); }
    }

    // Repeated typed methods should all stay on direct helper lowering (no eval fallback path).
    {
        char[] code = compile_to_c3(
            "(define (route (^Integer x)) (+ x 1)) "
            "(define (route (^String x)) x) "
            "(define (route (^Float64 x)) (+ x 1.0))",
            interp
        );
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(\"route\"");
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
        usz typed_calls = compiler_str_count(code, "aot::define_typed_method(\"mix\"");
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
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "aot::make_void()")
            && !str_contains(code, "lisp::Value* add;")
            && !str_contains(code, "add =")
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

    // Scoped module open lowers to local aliases and module-export lookups.
    {
        char[] code = compile_to_c3("(with math (< (abs (- (+ (sin 0.0) (cos 0.0)) 1.0)) 0.000001))", interp);
        bool ok = str_contains(code, "aot::lookup_module_export(\"math\", \"abs\")")
            && str_contains(code, "aot::lookup_module_export(\"math\", \"sin\")")
            && str_contains(code, "aot::lookup_module_export(\"math\", \"cos\")")
            && !str_contains(code, "lookup_prim(\"abs\")")
            && !str_contains(code, "lookup_prim(\"sin\")")
            && !str_contains(code, "lookup_prim(\"cos\")")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: scoped module open lowers through local aliases"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: scoped module open lowers through local aliases"); }
    }

    // AOT control-flow lowering must propagate ERROR values before truthiness or later expressions.
    {
        char[] code = compile_to_c3("(if (audit232-missing-if 1) 2 3)", interp);
        bool ok = str_contains(code, "if (aot::is_error(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT if predicate errors bypass truthiness"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT if predicate errors bypass truthiness"); }
    }

    {
        char[] code = compile_to_c3("(and (audit232-missing-and 1) 2)", interp);
        bool ok = str_contains(code, "if (aot::is_error(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT and predicate errors short-circuit"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT and predicate errors short-circuit"); }
    }

    {
        char[] code = compile_to_c3("(or (audit232-missing-or 1) 2)", interp);
        bool ok = str_contains(code, "if (aot::is_error(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT or predicate errors short-circuit"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT or predicate errors short-circuit"); }
    }

    {
        char[] code = compile_to_c3("(block 1 (audit232-missing-block 2) 3)", interp);
        bool ok = str_contains(code, "if (aot::is_error(")
            && str_contains(code, "audit232-missing-block")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT block stops after non-final errors"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT block stops after non-final errors"); }
    }

    {
        char[] code = compile_to_c3("(with math (block (audit232-missing-with 1) 2))", interp);
        bool ok = str_contains(code, "if (aot::is_error(")
            && str_contains(code, "audit232-missing-with")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT scoped-open body stops after non-final errors"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT scoped-open body stops after non-final errors"); }
    }

    // Inline import 'all binds exported globals from module-private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-all (export v) (define v 1)) (import imod-all 'all)",
            interp
        );
        bool ok = str_contains(code, "import: inline module")
            && str_contains(code, "v = _aot_mod_")
            && !str_contains(code, "aot::import_module(\"imod-all\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline import all binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline import all binds private backing"); }
    }

    // Inline selective import binds requested names from module-private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-sel (export v) (define v 1)) (import imod-sel (v))",
            interp
        );
        bool ok = str_contains(code, "v = _aot_mod_")
            && str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-sel\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline selective import binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline selective import binds private backing"); }
    }

    // Inline selective alias import binds the alias target from module-private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-sel-alias (export v) (define v 1)) (import imod-sel-alias (v 'as vv))",
            interp
        );
        bool ok = str_contains(code, "vv = _aot_mod_")
            && str_contains(code, "import: inline module")
            && !str_contains(code, "aot::import_module(\"imod-sel-alias\"")
            && !str_contains(code, "aot::lookup_var(\"v\")")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline selective alias import binds private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline selective alias import binds private backing"); }
    }

    // Inline modules export descriptor-producing type forms through private backing storage.
    {
        char[] code = compile_to_c3(
            "(module imod-type-desc (export ShapeM MaybeM AliasM SomeM) "
            "  (define [abstract] ShapeM) "
            "  (define [union] MaybeM NoneM (SomeM Integer)) "
            "  (define [alias] AliasM Integer)) "
            "(import imod-type-desc 'all) "
            "(format \"%s\" ShapeM)",
            interp
        );
        bool ok = str_contains(code, "_aot_mod_")
            && str_contains(code, "aot::lookup_var(\"ShapeM\")")
            && str_contains(code, "aot::lookup_var(\"MaybeM\")")
            && str_contains(code, "aot::lookup_var(\"AliasM\")")
            && str_contains(code, "aot::lookup_var(\"SomeM\")")
            && !str_contains(code, "inline module export has no compiled private backing")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module type descriptors bind private backing"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module type descriptors bind private backing"); }
    }

    // Non-top-level import keeps runtime bridge side effects in expression lowering.
    {
        char[] code = compile_to_c3("(block (import external_mod) 0)", interp);
        bool ok = str_contains(code, "aot::import_module(\"external_mod\", \"\", false, false)")
            && !str_contains(code, "import: inline module")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: non-top-level import keeps runtime bridge side effects"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: non-top-level import keeps runtime bridge side effects"); }
    }
}
