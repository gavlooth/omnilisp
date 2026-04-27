module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import lisp::aot;

fn bool compiler_fail_closed_datum_root_alloc_case(
    Interp* interp,
    char[] source,
    bool template_mode,
    int fail_nth
) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();
    if (lex.current.type == T_ERROR) return false;

    Parser p;
    p.init(&lex, interp);

    int saved_force = g_interp_force_alloc_value_root_fail_nth;
    g_interp_force_alloc_value_root_fail_nth = fail_nth;
    Value* datum = template_mode ? p.parse_template_datum() : p.parse_datum();
    g_interp_force_alloc_value_root_fail_nth = saved_force;

    return datum == null &&
           p.has_error &&
           str_contains(p.error_msg[:p.error_msg_len], "out of memory while parsing datum");
}

fn bool compiler_fail_closed_datum_root_alloc_tests(Interp* interp) {
    return compiler_fail_closed_datum_root_alloc_case(interp, "1", false, 1) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "foo", false, 1) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "\"abc\"", false, 1) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "'x", false, 2) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "()", false, 1) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "(1 2)", false, 2) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "(1 2)", false, 4) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "[1 2]", true, 2) &&
           compiler_fail_closed_datum_root_alloc_case(interp, "{a 1}", true, 2);
}

fn bool compiler_fail_closed_symbol_name_mangling_test(Interp* interp) {
    char[180] long_symbol_a;
    char[180] long_symbol_b;
    for (usz i = 0; i < long_symbol_a.len; i++) {
        long_symbol_a[i] = '+';
        long_symbol_b[i] = '+';
    }
    long_symbol_a[long_symbol_a.len - 1] = '*';
    long_symbol_b[long_symbol_b.len - 1] = '/';

    SymbolId sym_a = interp.symbols.intern(long_symbol_a[..]);
    SymbolId sym_b = interp.symbols.intern(long_symbol_b[..]);
    SymbolId pred_sym = interp.symbols.intern("a?");
    SymbolId raw_sym = interp.symbols.intern("a_p");
    SymbolId colon_sym = interp.symbols.intern("name:part");
    SymbolId generated_raw_sym = interp.symbols.intern("_omni_sym_a_p_h9bac2000c65b5bd9_l2");
    SymbolId reserved_sym = interp.symbols.intern("try");
    SymbolId reserved_raw_sym = interp.symbols.intern("_omni_try");
    SymbolId prefixed_sym = interp.symbols.intern("1x");
    SymbolId prefixed_raw_sym = interp.symbols.intern("_omni_v_1x");

    Compiler compiler_a;
    compiler_a.init(interp);
    defer compiler_a.free();
    Compiler compiler_b;
    compiler_b.init(interp);
    defer compiler_b.free();
    Compiler compiler_pred;
    compiler_pred.init(interp);
    defer compiler_pred.free();
    Compiler compiler_raw;
    compiler_raw.init(interp);
    defer compiler_raw.free();
    Compiler compiler_colon;
    compiler_colon.init(interp);
    defer compiler_colon.free();
    Compiler compiler_generated_raw;
    compiler_generated_raw.init(interp);
    defer compiler_generated_raw.free();
    Compiler compiler_reserved;
    compiler_reserved.init(interp);
    defer compiler_reserved.free();
    Compiler compiler_reserved_raw;
    compiler_reserved_raw.init(interp);
    defer compiler_reserved_raw.free();
    Compiler compiler_prefixed;
    compiler_prefixed.init(interp);
    defer compiler_prefixed.free();
    Compiler compiler_prefixed_raw;
    compiler_prefixed_raw.init(interp);
    defer compiler_prefixed_raw.free();

    compiler_a.emit_symbol_name(sym_a);
    compiler_b.emit_symbol_name(sym_b);
    compiler_pred.emit_symbol_name(pred_sym);
    compiler_raw.emit_symbol_name(raw_sym);
    compiler_colon.emit_symbol_name(colon_sym);
    compiler_generated_raw.emit_symbol_name(generated_raw_sym);
    compiler_reserved.emit_symbol_name(reserved_sym);
    compiler_reserved_raw.emit_symbol_name(reserved_raw_sym);
    compiler_prefixed.emit_symbol_name(prefixed_sym);
    compiler_prefixed_raw.emit_symbol_name(prefixed_raw_sym);
    char[] emitted_a = compiler_a.get_output();
    char[] emitted_b = compiler_b.get_output();
    char[] emitted_pred = compiler_pred.get_output();
    char[] emitted_raw = compiler_raw.get_output();
    char[] emitted_colon = compiler_colon.get_output();
    char[] emitted_generated_raw = compiler_generated_raw.get_output();
    char[] emitted_reserved = compiler_reserved.get_output();
    char[] emitted_reserved_raw = compiler_reserved_raw.get_output();
    char[] emitted_prefixed = compiler_prefixed.get_output();
    char[] emitted_prefixed_raw = compiler_prefixed_raw.get_output();

    return sym_a != INVALID_SYMBOL_ID &&
        sym_b != INVALID_SYMBOL_ID &&
        pred_sym != INVALID_SYMBOL_ID &&
        raw_sym != INVALID_SYMBOL_ID &&
        colon_sym != INVALID_SYMBOL_ID &&
        generated_raw_sym != INVALID_SYMBOL_ID &&
        reserved_sym != INVALID_SYMBOL_ID &&
        reserved_raw_sym != INVALID_SYMBOL_ID &&
        prefixed_sym != INVALID_SYMBOL_ID &&
        prefixed_raw_sym != INVALID_SYMBOL_ID &&
        emitted_a.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_b.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_pred.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_generated_raw.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_reserved.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_reserved_raw.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_prefixed.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        emitted_prefixed_raw.len <= COMPILER_SYMBOL_NAME_MAX_EMITTED &&
        str_contains(emitted_a, "_omni_sym_") &&
        str_contains(emitted_a, "_h") &&
        str_contains(emitted_a, "_l") &&
        !compiler_str_eq(emitted_a, emitted_b) &&
        !compiler_str_eq(emitted_pred, emitted_raw) &&
        !compiler_str_eq(emitted_pred, emitted_generated_raw) &&
        !compiler_str_eq(emitted_reserved, emitted_reserved_raw) &&
        !compiler_str_eq(emitted_prefixed, emitted_prefixed_raw) &&
        !str_contains(emitted_colon, ":");
}

fn bool compiler_fail_closed_generated_name_bounds_tests(Interp* interp) {
    char[4] id_exact;
    char[] id_name = compiler_format_generated_name_id(id_exact[..], "r", 123);
    char[3] id_short;
    char[] id_overflow = compiler_format_generated_name_id(id_short[..], "r", 123);

    char[4] index_exact;
    char[] index_name = compiler_format_generated_name_index(index_exact[..], "a", 7);
    char[3] index_short;
    char[] index_overflow = compiler_format_generated_name_index(index_short[..], "a", 7);

    char[3] suffix_exact;
    char[] suffix_name = compiler_format_generated_name_suffix(suffix_exact[..], "a", ".b");
    char[2] suffix_short;
    char[] suffix_overflow = compiler_format_generated_name_suffix(suffix_short[..], "a", ".b");

    char[4] pair_exact;
    char[] pair_name = compiler_format_generated_name_pair(pair_exact[..], "p", 1, 2);
    char[3] pair_short;
    char[] pair_overflow = compiler_format_generated_name_pair(pair_short[..], "p", 1, 2);

    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();
    char[3] compiler_short;
    char[] compiler_overflow = compiler.generated_name_pair(compiler_short[..], "p", 1, 2);

    return compiler_str_eq(id_name, "r123") &&
        id_overflow.len == 0 &&
        compiler_str_eq(index_name, "a[7]") &&
        index_overflow.len == 0 &&
        compiler_str_eq(suffix_name, "a.b") &&
        suffix_overflow.len == 0 &&
        compiler_str_eq(pair_name, "p1_2") &&
        pair_overflow.len == 0 &&
        compiler_overflow.len == 0 &&
        compiler.has_error &&
        str_contains(compiler.error_message, COMPILER_GENERATED_NAME_OVERFLOW_ERROR);
}

fn char[] compiler_test_build_oversized_program_source(char** source_buf_out) {
    *source_buf_out = null;
    usz source_len = PARSER_MAX_PROGRAM_EXPRS * 2 + 2;
    char* source_buf = (char*)mem::malloc(source_len);
    if (source_buf == null) return "";

    for (usz i = 0; i <= PARSER_MAX_PROGRAM_EXPRS; i++) {
        source_buf[i * 2] = '1';
        source_buf[i * 2 + 1] = ' ';
    }
    source_buf[source_len - 1] = 0;
    *source_buf_out = source_buf;
    return source_buf[0:source_len - 1];
}

fn bool compiler_fail_closed_direct_program_expr_limit_test(Interp* interp) {
    char* source_buf = null;
    char[] source = compiler_test_build_oversized_program_source(&source_buf);
    if (source_buf == null) return false;
    List{Expr*} exprs = parse_program(source, interp);
    bool ok = exprs.len() == 0;
    exprs.free();
    mem::free(source_buf);
    return ok;
}

fn bool compiler_fail_closed_compile_program_expr_limit_test(Interp* interp) {
    char* source_buf = null;
    char[] source = compiler_test_build_oversized_program_source(&source_buf);
    if (source_buf == null) return false;
    char[] err = "";
    char[] code = compile_to_c3(source, interp, &err);
    bool ok = code.len == 0 &&
        str_contains(err, "program has too many top-level expressions");
    mem::free(source_buf);
    return ok;
}

fn bool compiler_fail_closed_aot_no_interp_diagnostic_test() {
    aot::aot_shutdown();

    aot::aot_out_of_band_diagnostic_reset_for_tests();
    Value* lookup_result = aot::lookup_prim("+");
    bool lookup_ok = lookup_result == null &&
        aot::aot_out_of_band_diagnostic_count_for_tests() == 1 &&
        str_contains(aot::aot_last_out_of_band_diagnostic_for_tests(), "aot lookup-prim: runtime interpreter unavailable");

    aot::aot_out_of_band_diagnostic_reset_for_tests();
    Value*[] no_args = {};
    Value* apply_result = aot::aot_closure_apply(no_args, null, null);
    bool apply_ok = apply_result == null &&
        aot::aot_out_of_band_diagnostic_count_for_tests() == 1 &&
        str_contains(aot::aot_last_out_of_band_diagnostic_for_tests(), "aot closure apply: runtime interpreter unavailable");

    aot::aot_out_of_band_diagnostic_reset_for_tests();
    Value* closure_result = aot::make_closure(null, null);
    bool closure_ok = closure_result == null &&
        aot::aot_out_of_band_diagnostic_count_for_tests() == 1 &&
        str_contains(aot::aot_last_out_of_band_diagnostic_for_tests(), "aot make_closure: runtime interpreter unavailable");

    return lookup_ok && apply_ok && closure_ok;
}

fn void run_compiler_group_module_fail_closed_tail_tests(Interp* interp, usz* pass, usz* fail) {
    {
        g_compiler_force_lambda_lookup_miss = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_lookup_miss = false;
        bool ok = code.len == 0 && str_contains(err, "missing lambda definition during AOT lambda lowering");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda lookup miss fails closed without panic"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda lookup miss fails closed without panic"); }
    }

    {
        g_compiler_force_lambda_emit_lookup_miss = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_emit_lookup_miss = false;
        bool ok = code.len == 0 && str_contains(err, "missing lambda definition during closure emission");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda closure emission lookup miss fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda closure emission lookup miss fails closed"); }
    }

    {
        Lexer lex;
        lex.init("(+ 1 2)");
        defer lex.destroy();
        lex.expect(T_RPAREN, "expected ')'");
        bool ok = lex.current.type == T_ERROR && lex.current.text_len > 0;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexer expect mismatch fails closed without panic"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexer expect mismatch fails closed without panic"); }
    }

    {
        g_compiler_force_module_exports_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(module oom-mod (export v) (define v 1))", interp, &err);
        g_compiler_force_module_exports_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating module export table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module export allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module export allocation failure fails closed"); }
    }

    {
        g_compiler_force_module_exports_size_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3("(module oom-mod (export v) (define v 1))", interp, &err);
        g_compiler_force_module_exports_size_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "module export table size overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module export size overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module export size overflow fails closed"); }
    }

    {
        g_compiler_force_module_registry_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3(
            "(module m0 (export v0) (define v0 0)) "
            "(module m1 (export v1) (define v1 1)) "
            "(module m2 (export v2) (define v2 2)) "
            "(module m3 (export v3) (define v3 3)) "
            "(module m4 (export v4) (define v4 4)) "
            "(module m5 (export v5) (define v5 5)) "
            "(module m6 (export v6) (define v6 6)) "
            "(module m7 (export v7) (define v7 7)) "
            "(module m8 (export v8) (define v8 8)) "
            "(module m9 (export v9) (define v9 9)) "
            "(module m10 (export v10) (define v10 10)) "
            "(module m11 (export v11) (define v11 11)) "
            "(module m12 (export v12) (define v12 12)) "
            "(module m13 (export v13) (define v13 13)) "
            "(module m14 (export v14) (define v14 14)) "
            "(module m15 (export v15) (define v15 15)) "
            "(module m16 (export v16) (define v16 16))",
            interp,
            &err
        );
        g_compiler_force_module_registry_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "growing compiled module registry");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module registry growth allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module registry growth allocation failure fails closed"); }
    }

    {
        g_compiler_force_module_registry_growth_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3(
            "(module m0 (export v0) (define v0 0)) "
            "(module m1 (export v1) (define v1 1)) "
            "(module m2 (export v2) (define v2 2)) "
            "(module m3 (export v3) (define v3 3)) "
            "(module m4 (export v4) (define v4 4)) "
            "(module m5 (export v5) (define v5 5)) "
            "(module m6 (export v6) (define v6 6)) "
            "(module m7 (export v7) (define v7 7)) "
            "(module m8 (export v8) (define v8 8)) "
            "(module m9 (export v9) (define v9 9)) "
            "(module m10 (export v10) (define v10 10)) "
            "(module m11 (export v11) (define v11 11)) "
            "(module m12 (export v12) (define v12 12)) "
            "(module m13 (export v13) (define v13 13)) "
            "(module m14 (export v14) (define v14 14)) "
            "(module m15 (export v15) (define v15 15)) "
            "(module m16 (export v16) (define v16 16))",
            interp,
            &err
        );
        g_compiler_force_module_registry_growth_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "compiled module registry capacity overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module registry growth overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module registry growth overflow fails closed"); }
    }
}
