module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        List{char} buf;
        defer buf.free();
        compiler.serialize_value_to_buf(make_int(interp, long.min), &buf);

        char[] actual = "";
        if (buf.len() > 0) {
            actual = buf.entries[:buf.len()];
        }
        bool ok = compiler_str_eq(actual, "-9223372036854775808");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: integer serializer preserves long.min"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: integer serializer preserves long.min"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value* arr = make_array(interp, 2);
        bool setup_ok = arr != null && arr.tag == ARRAY && arr.array_val != null;
        if (setup_ok) {
            arr.array_val.items[0] = make_int(interp, 1);
            arr.array_val.items[1] = make_int(interp, 2);
            arr.array_val.length = 2;
        }

        List{char} buf;
        defer buf.free();
        if (setup_ok) compiler.serialize_value_to_buf(arr, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = setup_ok && !compiler.has_error && compiler_str_eq(actual, "[1 2]");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer preserves array literals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer preserves array literals"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value* dict = make_hashmap(interp, 2);
        SymbolId name_sym = interp.symbols.intern("name");
        bool setup_ok = dict != null && dict.tag == HASHMAP && dict.hashmap_val != null &&
            name_sym != INVALID_SYMBOL_ID &&
            hashmap_set_checked(dict.hashmap_val, make_symbol(interp, name_sym), make_int(interp, 7), interp);

        List{char} buf;
        defer buf.free();
        if (setup_ok) compiler.serialize_value_to_buf(dict, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = setup_ok && !compiler.has_error && compiler_str_eq(actual, "{name 7}");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer preserves dictionary literals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer preserves dictionary literals"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        HashMap malformed = {};
        malformed.count = 1;
        malformed.capacity = 2;
        malformed.entries = null;
        Value malformed_dict = {};
        malformed_dict.tag = HASHMAP;
        malformed_dict.hashmap_val = &malformed;

        uint[] slots = hashmap_sorted_slots(&malformed, interp);
        bool slots_empty = slots.ptr == null && slots.len == 0;
        if (slots.ptr != null) mem::free(slots.ptr);

        List{char} buf;
        defer buf.free();
        if (slots_empty) compiler.serialize_value_to_buf(&malformed_dict, &buf);
        bool ok = slots_empty &&
            compiler.has_error &&
            str_contains(compiler.error_message, "dictionary backing entries are missing");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: dictionary serializer rejects malformed backing storage"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: dictionary serializer rejects malformed backing storage"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value* set_value = make_set_checked(interp, 4, "serializer set test: out of memory");
        bool setup_ok = set_value != null && set_value.tag == SET && set_value.hashmap_val != null &&
            hashmap_set_checked(set_value.hashmap_val, make_int(interp, 1), make_symbol(interp, interp.sym_true), interp) &&
            hashmap_set_checked(set_value.hashmap_val, make_int(interp, 2), make_symbol(interp, interp.sym_true), interp);

        List{char} buf;
        defer buf.free();
        if (setup_ok) compiler.serialize_value_to_buf(set_value, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = setup_ok && !compiler.has_error && compiler_str_eq(actual, "(Set 1 2)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer preserves set values"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer preserves set values"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value* big = make_big_integer_from_string(interp, "9223372036854775808");
        List{char} buf;
        defer buf.free();
        if (big != null && big.tag == BIG_INTEGER) compiler.serialize_value_to_buf(big, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = big != null && big.tag == BIG_INTEGER && !compiler.has_error &&
            compiler_str_eq(actual, "(BigInteger \"9223372036854775808\")");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer preserves BigInteger literals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer preserves BigInteger literals"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value* time_value = make_time_point(interp, TIME_POINT_DATETIMETZ, 2026, 3, 7, 8, 15, 30, 123456, -420);
        List{char} buf;
        defer buf.free();
        if (time_value != null && time_value.tag == TIME_POINT) compiler.serialize_value_to_buf(time_value, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = time_value != null && time_value.tag == TIME_POINT && !compiler.has_error &&
            compiler_str_eq(actual, "(TimePoint 'datetime-tz 2026 3 7 8 15 30 123456 -420)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer preserves TimePoint literals"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer preserves TimePoint literals"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value unsupported = {};
        unsupported.tag = CLOSURE;
        List{char} buf;
        defer buf.free();
        compiler.serialize_value_to_buf(&unsupported, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "cannot serialize closure value") &&
            !compiler_str_eq(actual, "nil");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: value serializer fails closed on opaque values"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: value serializer fails closed on opaque values"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(lambda (x) x)",
            "(\xCE\xBB (x) x)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda serializer emits canonical unicode spelling"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda serializer emits canonical unicode spelling"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))",
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import serializer preserves path and alias syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import serializer preserves path and alias syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (+ 1 (signal ask 0)) (ask x (resolve x)))",
            "(handle (+ 1 (perform ask 0)) (ask x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle serializer emits parser-compatible clause syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle serializer emits parser-compatible clause syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))",
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves clause parens"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves clause parens"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{char} buf;
        defer buf.free();
        compiler.serialize_expr_to_buf(&invalid, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "cannot serialize unknown expression tag") &&
            !compiler_str_eq(actual, "nil");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: expression serializer rejects unknown tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: expression serializer rejects unknown tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{char} buf;
        defer buf.free();
        compiler.serialize_pattern_to_buf(&invalid, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "cannot serialize unknown pattern tag") &&
            !compiler_str_eq(actual, "_");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: pattern serializer rejects unknown tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: pattern serializer rejects unknown tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        TypeAnnotation ann = {};
        ann.has_annotation = true;
        ann.is_compound = true;
        ann.base_type = interp.symbols.intern("Literal");
        ann.has_val_literal = true;
        ann.val_literal.tag = TENSOR;

        List{char} buf;
        defer buf.free();
        compiler.serialize_type_annotation_to_buf(&ann, &buf);
        char[] actual = buf.len() > 0 ? buf.entries[:buf.len()] : "";
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "cannot serialize unsupported type literal annotation") &&
            !compiler_str_eq(actual, "^(Literal nil)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: type literal serializer rejects unsupported tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: type literal serializer rejects unsupported tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.collect_expr_generated_globals(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in generated global collection");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: generated global collector rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: generated global collector rejects unknown expression tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.collect_inline_module_backing_globals_in_expr(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in inline module backing global collection");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module backing collector rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module backing collector rejects unknown expression tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Value invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.collect_literal_generated_globals(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown value tag in generated global collection");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: generated global collector rejects unknown literal value tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: generated global collector rejects unknown literal value tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.collect_pattern_generated_globals(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in generated global collection");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: generated global collector rejects unknown pattern tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: generated global collector rejects unknown pattern tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{SymbolId} bound;
        List{bool} mutable_flags;
        defer mutable_flags.free();
        defer bound.free();
        bool found = compiler.scan_lambdas_with_scope(&invalid, &bound, &mutable_flags);
        bool ok = !found &&
            compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in lambda scan");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda scan rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda scan rejects unknown expression tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{SymbolId} bound;
        List{bool} mutable_flags;
        defer mutable_flags.free();
        defer bound.free();
        bool found = compiler.scan_lambdas_pattern(&invalid, &bound, &mutable_flags);
        bool ok = !found &&
            compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in lambda scan");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda scan rejects unknown pattern tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda scan rejects unknown pattern tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{SymbolId} bound;
        List{SymbolId} free_vars;
        List{SymbolId} enclosing;
        defer enclosing.free();
        defer free_vars.free();
        defer bound.free();
        compiler.find_free_vars(&invalid, &bound, &free_vars, &enclosing);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in free-variable analysis");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: free-variable analysis rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: free-variable analysis rejects unknown expression tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{SymbolId} bindings;
        defer bindings.free();
        compiler.collect_pattern_bindings(&invalid, &bindings);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in free-variable binding collection") &&
            bindings.len() == 0;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: free-variable binding collection rejects unknown pattern tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: free-variable binding collection rejects unknown pattern tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.prescan_mutable_captures(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in mutable-capture prescan");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mutable-capture prescan rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mutable-capture prescan rejects unknown expression tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.prescan_match_pattern(&invalid);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in mutable-capture prescan");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mutable-capture prescan rejects unknown pattern tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mutable-capture prescan rejects unknown pattern tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        List{SymbolId} bound;
        List{SymbolId} free_vars;
        List{SymbolId} enclosing;
        defer enclosing.free();
        defer free_vars.free();
        defer bound.free();
        compiler.find_free_vars_in_qq(&invalid, &bound, &free_vars, &enclosing);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown expression tag in quasiquote free-variable analysis");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: quasiquote free-variable analysis rejects unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: quasiquote free-variable analysis rejects unknown expression tags"); }
    }
    {
        CompiledModule mod = {};

        Expr invalid_expr = {};
        char* expr_raw = (char*)&invalid_expr;
        expr_raw[0] = (char)127;

        Pattern invalid_pat = {};
        char* pat_raw = (char*)&invalid_pat;
        pat_raw[0] = (char)127;

        Compiler expr_classifier;
        expr_classifier.init(interp);
        defer expr_classifier.free();
        bool expr_binds = expr_classifier.compiled_module_expr_binds_export(&mod, &invalid_expr);
        bool expr_class_ok = expr_classifier.has_error &&
            !expr_binds &&
            str_contains(expr_classifier.error_message, "unknown expression tag in inline module export classification");

        Compiler pat_classifier;
        pat_classifier.init(interp);
        defer pat_classifier.free();
        bool pat_binds = pat_classifier.compiled_module_pattern_binds_export(&mod, &invalid_pat);
        bool pat_class_ok = pat_classifier.has_error &&
            !pat_binds &&
            str_contains(pat_classifier.error_message, "unknown pattern tag in inline module export classification");

        Compiler expr_collector;
        expr_collector.init(interp);
        defer expr_collector.free();
        bool expr_collect_ok = !expr_collector.add_compiled_module_locals_from_expr(&mod, 0, &invalid_expr) &&
            expr_collector.has_error &&
            str_contains(expr_collector.error_message, "unknown expression tag in inline module local collection");

        Compiler pat_collector;
        pat_collector.init(interp);
        defer pat_collector.free();
        bool pat_collect_ok = !pat_collector.add_compiled_module_locals_from_pattern(&mod, 0, &invalid_pat) &&
            pat_collector.has_error &&
            str_contains(pat_collector.error_message, "unknown pattern tag in inline module local collection");

        bool ok = expr_class_ok && pat_class_ok && expr_collect_ok && pat_collect_ok;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: inline module collectors reject unknown AST tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: inline module collectors reject unknown AST tags"); }
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        int[1] param_tags = { 127 };
        compiler.emit_ffi_manifest_signature_json(&param_tags[0], 1, true, (int)FfiTypeTag.FFI_TYPE_INT);
        char[] code = compiler.get_output();
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "invalid FFI type tag in contract manifest") &&
            str_contains(code, "\"Invalid\"") &&
            !str_contains(code, "\"Void\"");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: FFI manifest rejects invalid parameter type tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: FFI manifest rejects invalid parameter type tags"); }
    }
    {
        Expr invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        Compiler discovery;
        discovery.init(interp);
        defer discovery.free();
        bool has_preload = discovery.has_ffi_preload_expr(&invalid);
        bool discovery_ok = discovery.has_error &&
            !has_preload &&
            str_contains(discovery.error_message, "unknown expression tag in FFI preload discovery");

        Compiler preload;
        preload.init(interp);
        defer preload.free();
        preload.emit_ffi_preload_expr_phase(&invalid, true);
        bool preload_ok = preload.has_error &&
            preload.get_output().len == 0 &&
            str_contains(preload.error_message, "unknown expression tag in FFI preload emission");

        bool first_lib = true;
        Compiler lib_manifest;
        lib_manifest.init(interp);
        defer lib_manifest.free();
        lib_manifest.emit_ffi_manifest_lib_entries(&invalid, &first_lib);
        bool lib_ok = lib_manifest.has_error &&
            first_lib &&
            lib_manifest.get_output().len == 0 &&
            str_contains(lib_manifest.error_message, "unknown expression tag in FFI manifest library discovery");

        bool first_fn = true;
        Compiler fn_manifest;
        fn_manifest.init(interp);
        defer fn_manifest.free();
        fn_manifest.emit_ffi_manifest_fn_entries(&invalid, &first_fn);
        bool fn_ok = fn_manifest.has_error &&
            first_fn &&
            fn_manifest.get_output().len == 0 &&
            str_contains(fn_manifest.error_message, "unknown expression tag in FFI manifest function discovery");

        bool ok = discovery_ok && preload_ok && lib_ok && fn_ok;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: FFI preload and manifest collectors reject unknown expression tags"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: FFI preload and manifest collectors reject unknown expression tags"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (cons 1 2) ((a . b) a) (_ 0))",
            "(match (cons 1 2) ((a . b) a) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves cons patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves cons patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Dictionary 'name 7) ({name} name) (_ 0))",
            "(match (Dictionary 'name 7) ({name} name) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves dict patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves dict patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 1) ((Some x) x) (None 0))",
            "(match (Some 1) ((Some x) x) (None 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves constructor patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves constructor patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 7) ((? (> x 0) (Some x)) x) (_ 0))",
            "(match (Some 7) ((? (> x 0) (Some x)) x) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves guarded patterns with subpatterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves guarded patterns with subpatterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match 7 ((? (> x 0)) 1) (_ 0))",
            "(match 7 ((? (> x 0)) 1) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves guard-only patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves guard-only patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 7) ((? (> x 0) (? (> x 0) (Some x))) x) (_ 0))",
            "(match (Some 7) ((? (> x 0) (? (> x 0) (Some x))) x) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves nested guarded patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves nested guarded patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(signal ui.open 1)",
            "(perform ui.open 1)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified signal tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified signal tag paths"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            "(handle (perform ui.open 1) (ui.open x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified handle tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified handle tag paths"); }
    }
}

fn void run_compiler_group_foreign_handle_metadata_dict_tests(Interp* interp, usz* pass, usz* fail) {
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})",
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)",
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves standalone ForeignHandle return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves standalone ForeignHandle return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)",
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)",
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_manifest_atomic-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }

            char[512] final_manifest_path_buf;
            char[] final_manifest_path;
            if (try final_rendered = io::bprintf(
                &final_manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path
            )) {
                final_manifest_path = final_rendered;
                defer (void)io::file::delete((String)final_manifest_path);

                if (try file = io::file::open((String)final_manifest_path, "w")) {
                    defer (void)file.close();
                    (void)file.write("old manifest");
                }

                main::g_compile_manifest_force_fail_before_rename = true;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "(define [ffi module] libc \"libc.so.6\" (strlen (^String s)) ^Integer)",
                    "manifest-\b-\f-\v-source.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                main::g_compile_manifest_force_fail_before_rename = false;

                bool final_manifest_removed = false;
                if (catch err = io::file::load_temp((String)final_manifest_path)) {
                    final_manifest_removed = true;
                }
                compiler_test_finish_bool(
                    !wrote_manifest && final_manifest_removed,
                    "Compiler: grouped ffi manifest write removes stale final file on pre-rename failure",
                    "[FAIL] Compiler: grouped ffi manifest write removes stale final file on pre-rename failure",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: compose atomic manifest final path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose atomic manifest output path");
        }
    }
    {
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[512] manifest_path_buf;
        if (try manifest_rendered = io::bprintf(
            &manifest_path_buf,
            "/tmp/omni_compile_output_commit_fail-%d-%d.c3.ffi-manifest.json",
            manifest_pid,
            (int)manifest_seq
        )) {
            char[] manifest_path = manifest_rendered;
            defer (void)io::file::delete((String)manifest_path);
            bool setup = compiler_write_text_file(manifest_path, "stale manifest");
            main::EntryFileWriteKind manifest_cleanup_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            bool cleanup_ok = main::cleanup_compile_sidecar_on_output_commit_failure(manifest_path, &manifest_cleanup_kind);

            bool manifest_missing = false;
            if (try ignored = io::file::load_temp((String)manifest_path)) {
                (void)ignored;
                manifest_missing = false;
            } else {
                manifest_missing = true;
            }
            compiler_test_finish_bool(
                setup && cleanup_ok && manifest_missing,
                "Compiler: compile output commit failure removes sidecar manifest",
                "[FAIL] Compiler: compile output commit failure removes sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose output commit failure manifest path");
        }
    }
    {
        char[512] manifest_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        if (try manifest_rendered = io::bprintf(
            &manifest_path_buf,
            "/tmp/omni_compile_output_commit_cleanup_fail-%d-%d.c3.ffi-manifest.json",
            manifest_pid,
            (int)manifest_seq
        )) {
            char[] manifest_path = manifest_rendered;
            bool setup = compiler_write_text_file(manifest_path, "stale manifest");
            main::EntryFileWriteKind manifest_cleanup_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            main::g_compile_runner_force_sidecar_cleanup_fail = true;
            bool cleanup_ok = main::cleanup_compile_sidecar_on_output_commit_failure(manifest_path, &manifest_cleanup_kind);
            main::g_compile_runner_force_sidecar_cleanup_fail = false;
            defer (void)io::file::delete((String)manifest_path);
            compiler_test_finish_bool(
                setup && !cleanup_ok && manifest_cleanup_kind != ENTRY_FILE_WRITE_OK,
                "Compiler: compile output commit failure reports sidecar cleanup failure",
                "[FAIL] Compiler: compile output commit failure reports sidecar cleanup failure",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose output commit cleanup failure manifest path");
        }
    }
    {
        char[512] output_path_buf;
        char[512] temp_path_buf;
        int output_pid = compiler_test_getpid();
        usz output_seq = g_compiler_bindgen_temp_seq++;
        if (try output_rendered = io::bprintf(
            &output_path_buf,
            "/tmp/omni_build_publish-%d-%d.bin",
            output_pid,
            (int)output_seq
        )) {
            char[] output_path = output_rendered;
            char[] temp_path = "";
            if (main::entry_build_atomic_temp_path(output_path, temp_path_buf[..], &temp_path)) {
                bool staged_ok = compiler_write_text_file(temp_path, "new binary");
                main::EntryFileWriteKind publish_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
                bool publish_ok = false;
                bool published = false;
                defer {
                    if (temp_path.len > 0) {
                        (void)io::file::delete((String)temp_path);
                    }
                    (void)io::file::delete((String)output_path);
                }

                publish_ok = main::entry_commit_staged_text(temp_path, output_path, &publish_kind);
                if (try generated = io::file::load_temp((String)output_path)) {
                    published = compiler_str_eq(generated, "new binary");
                }
                compiler_test_finish_bool(
                    staged_ok && publish_ok && published && publish_kind == ENTRY_FILE_WRITE_OK,
                    "Compiler: build output publishes staged binary on backend success",
                    "[FAIL] Compiler: build output publishes staged binary on backend success",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: compose staged build output path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose build output path");
        }
    }
}
