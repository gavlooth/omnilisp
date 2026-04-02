module lisp;


import std::collections::list;
import std::io;
import main;
import lisp::aot;

fn bool compiler_str_eq(char[] left, char[] right) {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool compiler_serializes_to_roundtrip(Interp* interp, char[] source, char[] expected) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);
    Expr* expr = p.parse_expr();
    if (expr == null || p.has_error) {
        io::printfn("[DEBUG] serializer source parse failed: %s", (ZString)source);
        return false;
    }

    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();

    List{char} buf;
    defer buf.free();
    compiler.serialize_expr_to_buf(expr, &buf);

    char[] actual = "";
    if (buf.len() > 0) {
        actual = buf.entries[:buf.len()];
    }
    if (!compiler_str_eq(actual, expected)) {
        io::print("[DEBUG] serializer expected: ");
        io::printn(expected);
        io::print("[DEBUG] serializer actual:   ");
        io::printn(actual);
        return false;
    }

    Expr* reparsed = parse(actual, interp);
    if (reparsed == null) {
        io::printfn("[DEBUG] serializer reparse failed: %s", (ZString)actual);
        return false;
    }
    return true;
}

fn Value* compiler_aot_test_unary_invoke(void* data, Value* arg) {
    return arg != null ? arg : aot::make_nil();
}

fn Value* compiler_aot_test_variadic_invoke(void* data, Value* arg_list) {
    return arg_list != null ? arg_list : aot::make_nil();
}

fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BLOCK tests ---
    {
        char[] code = compile_to_c3("(block 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block 42)", "[FAIL] Compiler: (block 42)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        compiler_test_finish_bool(ok, "Compiler: (block 1 2 3)", "[FAIL] Compiler: (block 1 2 3)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block (define x 1) x)", "[FAIL] Compiler: (block (define x 1) x)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(lambda (x) (block x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (lambda (x) (block x x))", "[FAIL] Compiler: (lambda (x) (block x x))", pass, fail);
    }

    // --- Zero-arg lambda tests ---
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        compiler_test_finish_bool(ok, "Compiler: ((lambda () 42))", "[FAIL] Compiler: ((lambda () 42))", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg define + call", "[FAIL] Compiler: zero-arg define + call", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (f (lambda () 10)) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg in let", "[FAIL] Compiler: zero-arg in let", pass, fail);
    }
}

fn void run_compiler_group_syntax_multiarg_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Multi-param + multi-binding (parser desugar) ---
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-param lambda", "[FAIL] Compiler: multi-param lambda", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 1 y 2) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-binding let", "[FAIL] Compiler: multi-binding let", pass, fail);
    }
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: 3-param lambda", "[FAIL] Compiler: 3-param lambda", pass, fail);
    }
}

fn void run_compiler_group_syntax_basics(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_syntax_begin_zero_tests(interp, &pass, &fail);
    run_compiler_group_syntax_multiarg_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_stdlib_availability_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Stdlib availability ---
    {
        char[] code = compile_to_c3("(map (lambda (x) (+ x 1)) (list 1))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: map", "[FAIL] Compiler: map", pass, fail);
    }
    {
        char[] code = compile_to_c3("(filter (lambda (x) (> x 2)) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: filter", "[FAIL] Compiler: filter", pass, fail);
    }
    {
        char[] code = compile_to_c3("(foldl + 0 (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: foldl", "[FAIL] Compiler: foldl", pass, fail);
    }
}

fn void run_compiler_group_allocation_failure_seam_tests(Interp* interp, usz* pass, usz* fail) {
    {
        g_compiler_force_program_source_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_program_source_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "program source buffer");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: program source allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: program source allocation failure fails closed"); }
    }

    {
        g_compiler_force_compiled_modules_init_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_compiled_modules_init_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "initializing compiled module registry");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module registry init allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module registry init allocation failure fails closed"); }
    }

    {
        g_compiler_force_compiled_modules_init_size_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_compiled_modules_init_size_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "compiled module registry allocation size overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: module registry init size overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: module registry init size overflow fails closed"); }
    }

    {
        g_compiler_force_lambda_capture_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_capture_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating lambda capture table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda capture allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda capture allocation failure fails closed"); }
    }

    {
        g_compiler_force_lambda_param_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_param_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating lambda parameter table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda parameter allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda parameter allocation failure fails closed"); }
    }

    {
        g_compiler_force_lambda_capture_size_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_capture_size_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "lambda capture table size overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda capture size overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda capture size overflow fails closed"); }
    }

    {
        g_compiler_force_lambda_param_size_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda (x) x)", interp, &err);
        g_compiler_force_lambda_param_size_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "lambda parameter table size overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda parameter size overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda parameter size overflow fails closed"); }
    }

    {
        g_compiler_force_lambda_capture_growth_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3(
            "(let (a0 0 a1 1 a2 2 a3 3 a4 4 a5 5 a6 6 a7 7 a8 8 a9 9 a10 10 a11 11 a12 12 a13 13 a14 14 a15 15 a16 16) "
            "(lambda () (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)))",
            interp,
            &err
        );
        g_compiler_force_lambda_capture_growth_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "growing lambda capture table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lambda capture growth allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lambda capture growth allocation failure fails closed"); }
    }

    {
        char[] code = compile_to_c3("(let (x 42) (lambda () x))", interp);
        bool ok = compiler_code_contains_all(code, {
            "mem::malloc(Lambda_",
            "if (_closure_data == null) return aot::make_error(\"compiler: out of memory while allocating closure capture data\");",
            "if (_result != null && _result.tag == lisp::ValueTag.ERROR) mem::free(_closure_data);"
        });
        if (ok) { (*pass)++; compiler_print_pass("Compiler: closure capture emission guards allocation failure"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: closure capture emission guards allocation failure"); }
    }

    {
        bool ok = aot::aot_init();
        if (ok) {
            defer aot::aot_shutdown();

            aot::g_aot_force_closure_data_alloc_fail = true;
            Value* closure_data_fail = aot::make_closure(null, &compiler_aot_test_unary_invoke);
            aot::g_aot_force_closure_data_alloc_fail = false;

            aot::g_aot_force_closure_primitive_alloc_fail = true;
            Value* closure_prim_fail = aot::make_closure(null, &compiler_aot_test_unary_invoke);
            aot::g_aot_force_closure_primitive_alloc_fail = false;

            aot::g_aot_force_variadic_closure_data_alloc_fail = true;
            Value* variadic_data_fail = aot::make_variadic_closure(null, &compiler_aot_test_variadic_invoke);
            aot::g_aot_force_variadic_closure_data_alloc_fail = false;

            aot::g_aot_force_variadic_closure_primitive_alloc_fail = true;
            Value* variadic_prim_fail = aot::make_variadic_closure(null, &compiler_aot_test_variadic_invoke);
            aot::g_aot_force_variadic_closure_primitive_alloc_fail = false;

            Value* closure_missing_invoke = aot::make_closure(null, null);
            Value* variadic_missing_invoke = aot::make_variadic_closure(null, null);

            Interp* aot_interp = aot::aot_interp();
            void* saved_prim_user_data = aot_interp.prim_user_data;
            aot_interp.prim_user_data = null;
            Value*[] no_args = {};
            Value* apply_missing_payload = aot::aot_closure_apply(no_args, null, aot_interp);
            Value* variadic_apply_missing_payload = aot::aot_variadic_apply(no_args, null, aot_interp);
            Value* apply_null_interp = aot::aot_closure_apply(no_args, null, null);
            Value* variadic_apply_null_interp = aot::aot_variadic_apply(no_args, null, null);

            Value* variadic_invoke_arg_list_alloc_fail = null;
            Value* variadic_apply_arg_list_alloc_fail = null;
            Value* invoke_dispatch_interp_unavailable = null;
            Value* apply_multi_dispatch_interp_unavailable = null;
            Value* invoke_tail_dispatch_interp_unavailable = null;
            Value* apply_multi_tail_dispatch_interp_unavailable = null;
            Value* lookup_prim_bridge_interp_unavailable = null;
            Value* lookup_var_bridge_interp_unavailable = null;
            Value* import_bridge_interp_unavailable = null;
            Value* field_access_bridge_interp_unavailable = null;
            Value* explain_dispatch_bridge_interp_unavailable = null;
            Value* instance_field_bridge_interp_unavailable = null;
            Value* dict_get_symbol_bridge_interp_unavailable = null;
            Value* seq_nth_bridge_interp_unavailable = null;
            Value* seq_rest_bridge_interp_unavailable = null;
            Value* list_rest_bridge_interp_unavailable = null;
            Value* dict_from_args_bridge_interp_unavailable = null;
            Value* index_bridge_interp_unavailable = null;
            Value* compiled_signal_bridge_interp_unavailable = null;
            Value* compiled_handle_bridge_interp_unavailable = null;
            Value* compiled_reset_bridge_interp_unavailable = null;
            Value* compiled_shift_bridge_interp_unavailable = null;
            Value* compiled_resolve_bridge_interp_unavailable = null;
            Value* explain_effect_signal_bridge_interp_unavailable = null;
            Value* explain_effect_resolve_bridge_interp_unavailable = null;
            Value* invoke_variadic = aot::make_variadic_closure(null, &compiler_aot_test_variadic_invoke);
            if (invoke_variadic != null && invoke_variadic.tag == ValueTag.PRIMITIVE) {
                aot::g_aot_force_invoke_variadic_arg_list_alloc_fail = true;
                variadic_invoke_arg_list_alloc_fail = aot::invoke(invoke_variadic, aot::make_int(7));
                aot::g_aot_force_invoke_variadic_arg_list_alloc_fail = false;

                void* saved_apply_payload = aot_interp.prim_user_data;
                aot_interp.prim_user_data = invoke_variadic.prim_val.user_data;
                Value*[] one_arg = { aot::make_int(9) };
                aot::g_aot_force_variadic_apply_arg_list_alloc_fail = true;
                variadic_apply_arg_list_alloc_fail = aot::aot_variadic_apply(one_arg, null, aot_interp);
                aot::g_aot_force_variadic_apply_arg_list_alloc_fail = false;
                aot_interp.prim_user_data = saved_apply_payload;

                Value* multi_list = aot::cons(aot::make_int(11), aot::make_nil());
                aot::g_aot_force_dispatch_interp_unavailable = true;
                invoke_dispatch_interp_unavailable = aot::invoke(invoke_variadic, aot::make_int(10));
                apply_multi_dispatch_interp_unavailable = aot::apply_multi(invoke_variadic, multi_list, 1);
                invoke_tail_dispatch_interp_unavailable = aot::invoke_tail(invoke_variadic, aot::make_int(12));
                apply_multi_tail_dispatch_interp_unavailable = aot::apply_multi_tail(invoke_variadic, multi_list, 1);
                aot::g_aot_force_dispatch_interp_unavailable = false;
            }

            aot::g_aot_force_bridge_interp_unavailable = true;
            lookup_prim_bridge_interp_unavailable = aot::lookup_prim("+");
            lookup_var_bridge_interp_unavailable = aot::lookup_var("x");
            import_bridge_interp_unavailable = aot::import_module("missing_mod", "", false, false);
            field_access_bridge_interp_unavailable = aot::field_access(aot::make_int(1), aot::make_string("field"));
            explain_dispatch_bridge_interp_unavailable = aot::explain_dispatch("aot-test", aot::make_nil(), aot::make_nil(), 0);
            instance_field_bridge_interp_unavailable = aot::instance_field(aot::make_int(1), 0);
            dict_get_symbol_bridge_interp_unavailable = aot::dict_get_symbol(aot::make_nil(), "x");
            seq_nth_bridge_interp_unavailable = aot::seq_nth(aot::make_nil(), 0);
            seq_rest_bridge_interp_unavailable = aot::seq_rest(aot::make_nil(), 0);
            list_rest_bridge_interp_unavailable = aot::list_rest(aot::make_nil(), 0);
            dict_from_args_bridge_interp_unavailable = aot::dict_from_args(aot::make_nil());
            index_bridge_interp_unavailable = aot::index(aot::make_nil(), aot::make_int(0));
            compiled_signal_bridge_interp_unavailable = aot::compiled_signal(aot::make_symbol("ask"), aot::make_nil());
            compiled_handle_bridge_interp_unavailable = aot::compiled_handle(null, null, 0, aot::make_nil());
            compiled_reset_bridge_interp_unavailable = aot::compiled_reset(aot::make_nil());
            compiled_shift_bridge_interp_unavailable = aot::compiled_shift(aot::make_nil());
            compiled_resolve_bridge_interp_unavailable = aot::compiled_resolve(aot::make_nil(), aot::make_nil());
            explain_effect_signal_bridge_interp_unavailable = aot::explain_effect_signal("aot-test", "ask", aot::make_nil());
            explain_effect_resolve_bridge_interp_unavailable = aot::explain_effect_resolve("aot-test", aot::make_nil());
            aot::g_aot_force_bridge_interp_unavailable = false;

            aot_interp.prim_user_data = saved_prim_user_data;

            ok = closure_data_fail != null && closure_data_fail.tag == ValueTag.ERROR
                && closure_prim_fail != null && closure_prim_fail.tag == ValueTag.ERROR
                && variadic_data_fail != null && variadic_data_fail.tag == ValueTag.ERROR
                && variadic_prim_fail != null && variadic_prim_fail.tag == ValueTag.ERROR
                && closure_missing_invoke != null && closure_missing_invoke.tag == ValueTag.ERROR
                && variadic_missing_invoke != null && variadic_missing_invoke.tag == ValueTag.ERROR
                && apply_missing_payload != null && apply_missing_payload.tag == ValueTag.ERROR
                && variadic_apply_missing_payload != null && variadic_apply_missing_payload.tag == ValueTag.ERROR
                && apply_null_interp != null && apply_null_interp.tag == ValueTag.ERROR
                && variadic_apply_null_interp != null && variadic_apply_null_interp.tag == ValueTag.ERROR
                && variadic_invoke_arg_list_alloc_fail != null && variadic_invoke_arg_list_alloc_fail.tag == ValueTag.ERROR
                && variadic_apply_arg_list_alloc_fail != null && variadic_apply_arg_list_alloc_fail.tag == ValueTag.ERROR
                && invoke_dispatch_interp_unavailable != null && invoke_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_dispatch_interp_unavailable != null && apply_multi_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && invoke_tail_dispatch_interp_unavailable != null && invoke_tail_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_tail_dispatch_interp_unavailable != null && apply_multi_tail_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && lookup_prim_bridge_interp_unavailable != null && lookup_prim_bridge_interp_unavailable.tag == ValueTag.ERROR
                && lookup_var_bridge_interp_unavailable != null && lookup_var_bridge_interp_unavailable.tag == ValueTag.ERROR
                && import_bridge_interp_unavailable != null && import_bridge_interp_unavailable.tag == ValueTag.ERROR
                && field_access_bridge_interp_unavailable != null && field_access_bridge_interp_unavailable.tag == ValueTag.ERROR
                && explain_dispatch_bridge_interp_unavailable != null && explain_dispatch_bridge_interp_unavailable.tag == ValueTag.ERROR
                && instance_field_bridge_interp_unavailable != null && instance_field_bridge_interp_unavailable.tag == ValueTag.ERROR
                && dict_get_symbol_bridge_interp_unavailable != null && dict_get_symbol_bridge_interp_unavailable.tag == ValueTag.ERROR
                && seq_nth_bridge_interp_unavailable != null && seq_nth_bridge_interp_unavailable.tag == ValueTag.ERROR
                && seq_rest_bridge_interp_unavailable != null && seq_rest_bridge_interp_unavailable.tag == ValueTag.ERROR
                && list_rest_bridge_interp_unavailable != null && list_rest_bridge_interp_unavailable.tag == ValueTag.ERROR
                && dict_from_args_bridge_interp_unavailable != null && dict_from_args_bridge_interp_unavailable.tag == ValueTag.ERROR
                && index_bridge_interp_unavailable != null && index_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_signal_bridge_interp_unavailable != null && compiled_signal_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_handle_bridge_interp_unavailable != null && compiled_handle_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_reset_bridge_interp_unavailable != null && compiled_reset_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_shift_bridge_interp_unavailable != null && compiled_shift_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_resolve_bridge_interp_unavailable != null && compiled_resolve_bridge_interp_unavailable.tag == ValueTag.ERROR
                && explain_effect_signal_bridge_interp_unavailable != null && explain_effect_signal_bridge_interp_unavailable.tag == ValueTag.ERROR
                && explain_effect_resolve_bridge_interp_unavailable != null && explain_effect_resolve_bridge_interp_unavailable.tag == ValueTag.ERROR;
        }
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT closure factory allocation failures fail closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT closure factory allocation failures fail closed"); }
    }

    {
        g_parser_force_ffi_fn_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp, &err);
        g_parser_force_ffi_fn_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "creating FFI function expression");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser FFI function allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser FFI function allocation failure fails closed"); }
    }

    {
        g_parser_force_ffi_param_names_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp, &err);
        g_parser_force_ffi_param_names_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating FFI parameter name table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser FFI parameter name allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser FFI parameter name allocation failure fails closed"); }
    }

    {
        g_parser_force_ffi_param_types_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp, &err);
        g_parser_force_ffi_param_types_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating FFI parameter type table");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser FFI parameter type allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser FFI parameter type allocation failure fails closed"); }
    }

    {
        g_parser_force_ffi_param_table_size_overflow = true;
        char[] err = "";
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp, &err);
        g_parser_force_ffi_param_table_size_overflow = false;
        bool ok = code.len == 0 && str_contains(err, "FFI parameter table size overflow");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser FFI parameter table size overflow fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser FFI parameter table size overflow fails closed"); }
    }

    {
        g_lexer_force_text_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_lexer_force_text_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "initializing lexer token buffer");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexer token buffer allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexer token buffer allocation failure fails closed"); }
    }

    {
        char[1500] src_buf;
        usz pos = 0;
        src_buf[pos++] = '"';
        for (usz i = 0; i < 1300; i++) src_buf[pos++] = 'a';
        src_buf[pos++] = '"';
        char[] source = src_buf[:pos];

        g_lexer_force_text_growth_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3(source, interp, &err);
        g_lexer_force_text_growth_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "out of memory while lexing string literal");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: lexer token buffer growth allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: lexer token buffer growth allocation failure fails closed"); }
    }

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

fn void run_compiler_group_existing_feature_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Existing features still work ---
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: match", "[FAIL] Compiler: match", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: handle/perform", "[FAIL] Compiler: handle/perform", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 42) (let (f (lambda () x)) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg closure capture", "[FAIL] Compiler: zero-arg closure capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: id", "[FAIL] Compiler: id", pass, fail);
    }
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: range", "[FAIL] Compiler: range", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define [ffi lib] libc \"libc.so.6\")", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lib is rejected in AOT mode",
            "[FAIL] Compiler: declarative ffi lib is rejected in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi function is rejected in AOT mode",
            "[FAIL] Compiler: declarative ffi function is rejected in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(signal ui.open 1)",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified signal lowers directly",
            "[FAIL] Compiler: module-qualified signal lowers directly",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && str_contains(code, "aot::compiled_handle(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified handle lowers directly",
            "[FAIL] Compiler: module-qualified handle lowers directly",
            pass,
            fail
        );
    }
}

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
}

fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
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

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_allocation_failure_seam_tests(interp, &pass, &fail);
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    run_compiler_group_serializer_roundtrip_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
