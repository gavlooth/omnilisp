module lisp;

import std::collections::list;
import std::io;
import main;
import lisp::aot;

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
        char[] code = compile_to_c3("(lambda (x y) (+ x y))", interp);
        bool ok = compiler_code_contains_all(code, {
            "lisp::Value* _arg_count_error = aot::count_arg_list(_arg_list, &_arg_count);",
            "if (_arg_count < 2) return aot::make_arity_error(2, _arg_count);",
            "if (_arg_count > 2) return aot::apply_multi("
        });
        if (ok) { (*pass)++; compiler_print_pass("Compiler: multi-arg lambda emission guards arity and chains extras"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: multi-arg lambda emission guards arity and chains extras"); }
    }

    {
        char[] code = compile_to_c3("(lambda (x .. xs) x)", interp);
        bool ok = compiler_code_contains_all(code, {
            "lisp::Value* _arg_count_error = aot::count_arg_list(_arg_list, &_arg_count);",
            "if (_arg_count < 1) return aot::make_variadic_min_arity_error(1, _arg_count);"
        });
        if (ok) { (*pass)++; compiler_print_pass("Compiler: variadic lambda emission guards minimum arity"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: variadic lambda emission guards minimum arity"); }
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
            Value* apply_multi_negative_argc = null;
            Value* invoke_tail_dispatch_interp_unavailable = null;
            Value* apply_multi_tail_dispatch_interp_unavailable = null;
            Value* apply_multi_tail_negative_argc = null;
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
            Value* dict_from_args_alloc_fail = null;
            Value* dict_from_args_grow_fail = null;
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
                apply_multi_negative_argc = aot::apply_multi(invoke_variadic, multi_list, -1);
                apply_multi_tail_negative_argc = aot::apply_multi_tail(invoke_variadic, multi_list, -1);
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
            compiled_handle_bridge_interp_unavailable = aot::compiled_handle(null, null, 0, aot::make_nil(), false);
            compiled_reset_bridge_interp_unavailable = aot::compiled_reset(aot::make_nil());
            compiled_shift_bridge_interp_unavailable = aot::compiled_shift(aot::make_nil());
            compiled_resolve_bridge_interp_unavailable = aot::compiled_resolve(aot::make_nil(), aot::make_nil());
            explain_effect_signal_bridge_interp_unavailable = aot::explain_effect_signal("aot-test", "ask", aot::make_nil());
            explain_effect_resolve_bridge_interp_unavailable = aot::explain_effect_resolve("aot-test", aot::make_nil());
            aot::g_aot_force_bridge_interp_unavailable = false;

            bool saved_hashmap_struct_fail = g_hashmap_force_struct_alloc_fail;
            g_hashmap_force_struct_alloc_fail = true;
            dict_from_args_alloc_fail =
                aot::dict_from_args(
                    aot::cons(
                        aot::make_symbol("k"),
                        aot::cons(aot::make_int(1), aot::make_nil())));
            g_hashmap_force_struct_alloc_fail = saved_hashmap_struct_fail;

            Value* grow_arg_list = aot::make_nil();
            for (usz i = 13; i > 0; i--) {
                grow_arg_list = aot::cons(aot::make_int((long)i), grow_arg_list);
                grow_arg_list = aot::cons(aot::make_int((long)(100 + i)), grow_arg_list);
            }
            bool saved_hashmap_grow_fail = g_hashmap_force_grow_entries_alloc_fail;
            g_hashmap_force_grow_entries_alloc_fail = true;
            dict_from_args_grow_fail = aot::dict_from_args(grow_arg_list);
            g_hashmap_force_grow_entries_alloc_fail = saved_hashmap_grow_fail;

            aot_interp.prim_user_data = saved_prim_user_data;

            Value* ffi_optional_lib = aot::ffi_declare_lib("libc", "libc.so.6");

            Value* ffi_optional_fn = aot::ffi_declare_fn(
                "libc",
                "optional_missing_symbol",
                "definitely_missing_symbol",
                null,
                0,
                false,
                0,
                true,
                false
            );
            Value*[] ffi_optional_args = {};
            Value* ffi_optional_call = null;
            if (ffi_optional_fn != null && ffi_optional_fn.tag == ValueTag.PRIMITIVE) {
                ffi_optional_call = aot::apply_multi(ffi_optional_fn, aot::make_nil(), 0);
            }

            bool closure_factory_errors_ok =
                closure_data_fail != null && closure_data_fail.tag == ValueTag.ERROR
                && closure_prim_fail != null && closure_prim_fail.tag == ValueTag.ERROR
                && variadic_data_fail != null && variadic_data_fail.tag == ValueTag.ERROR
                && variadic_prim_fail != null && variadic_prim_fail.tag == ValueTag.ERROR
                && closure_missing_invoke != null && closure_missing_invoke.tag == ValueTag.ERROR
                && variadic_missing_invoke != null && variadic_missing_invoke.tag == ValueTag.ERROR;

            bool closure_apply_errors_ok =
                apply_missing_payload != null && apply_missing_payload.tag == ValueTag.ERROR
                && variadic_apply_missing_payload != null && variadic_apply_missing_payload.tag == ValueTag.ERROR
                && apply_null_interp != null && apply_null_interp.tag == ValueTag.ERROR
                && variadic_apply_null_interp != null && variadic_apply_null_interp.tag == ValueTag.ERROR
                && variadic_invoke_arg_list_alloc_fail != null && variadic_invoke_arg_list_alloc_fail.tag == ValueTag.ERROR
                && variadic_apply_arg_list_alloc_fail != null && variadic_apply_arg_list_alloc_fail.tag == ValueTag.ERROR
                && invoke_dispatch_interp_unavailable != null && invoke_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_dispatch_interp_unavailable != null && apply_multi_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_negative_argc != null && apply_multi_negative_argc.tag == ValueTag.ERROR
                && invoke_tail_dispatch_interp_unavailable != null && invoke_tail_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_tail_dispatch_interp_unavailable != null && apply_multi_tail_dispatch_interp_unavailable.tag == ValueTag.ERROR
                && apply_multi_tail_negative_argc != null && apply_multi_tail_negative_argc.tag == ValueTag.ERROR;

            bool bridge_interp_errors_ok =
                lookup_prim_bridge_interp_unavailable != null && lookup_prim_bridge_interp_unavailable.tag == ValueTag.ERROR
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
                && dict_from_args_alloc_fail != null && dict_from_args_alloc_fail.tag == ValueTag.ERROR
                && dict_from_args_grow_fail != null && dict_from_args_grow_fail.tag == ValueTag.ERROR
                && index_bridge_interp_unavailable != null && index_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_signal_bridge_interp_unavailable != null && compiled_signal_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_handle_bridge_interp_unavailable != null && compiled_handle_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_reset_bridge_interp_unavailable != null && compiled_reset_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_shift_bridge_interp_unavailable != null && compiled_shift_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiled_resolve_bridge_interp_unavailable != null && compiled_resolve_bridge_interp_unavailable.tag == ValueTag.ERROR
                && explain_effect_signal_bridge_interp_unavailable != null && explain_effect_signal_bridge_interp_unavailable.tag == ValueTag.ERROR
                && explain_effect_resolve_bridge_interp_unavailable != null && explain_effect_resolve_bridge_interp_unavailable.tag == ValueTag.ERROR;

            bool ffi_optional_path_ok =
                ffi_optional_lib != null && ffi_optional_lib.tag == ValueTag.FFI_HANDLE
                && ffi_optional_fn != null && ffi_optional_fn.tag == ValueTag.PRIMITIVE
                && ffi_optional_call != null && ffi_optional_call.tag == ValueTag.ERROR
                && str_contains(ffi_optional_call.str_chars[:ffi_optional_call.str_len], "unavailable");

            ok = closure_factory_errors_ok &&
                closure_apply_errors_ok &&
                bridge_interp_errors_ok &&
                ffi_optional_path_ok;
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
        g_parser_force_ast_expr_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(import external_mod)", interp, &err);
        g_parser_force_ast_expr_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating expression");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser expression allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser expression allocation failure fails closed"); }
    }

    {
        g_parser_force_ast_pattern_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(match 1 (_ 2))", interp, &err);
        g_parser_force_ast_pattern_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating pattern");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser pattern allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser pattern allocation failure fails closed"); }
    }

    {
        g_string_force_builder_alloc_fail_nth = 1;
        char[] err = "";
        char[] code = compile_to_c3("\"abc\"", interp, &err);
        g_string_force_builder_alloc_fail_nth = 0;
        bool ok = code.len == 0 && str_contains(err, "out of memory while parsing string literal");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser string literal builder allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser string literal builder allocation failure fails closed"); }
    }

    {
        List{Expr*} exprs = parse_program("1 2.[", interp);
        defer exprs.free();
        bool ok = exprs.len() == 0;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parse_program fails closed on malformed postfix partial expr"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parse_program fails closed on malformed postfix partial expr"); }
    }

    {
        Expr* expr = parse("1 2", interp);
        bool ok = expr == null;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parse rejects trailing tokens in single-expression mode"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parse rejects trailing tokens in single-expression mode"); }
    }

    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        List{Expr*} exprs;
        defer exprs.free();
        bool parsed = compiler.parse_program_exprs("(define x 1) 2.[", &exprs);
        bool ok = !parsed &&
            exprs.len() == 0 &&
            str_contains(compiler.error_message, "expected ']'");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: compiler parser clears prior exprs on malformed postfix partial expr"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: compiler parser clears prior exprs on malformed postfix partial expr"); }
    }

    {
        char[] err = "";
        char[] code = compile_to_c3("(define x 1)\n2.[", interp, &err);
        bool ok = code.len == 0 &&
            str_contains(err, "compiler syntax error at line 2, column") &&
            str_contains(err, "expected ']'");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: prelude-stripped parser coordinates report user line numbers"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: prelude-stripped parser coordinates report user line numbers"); }
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
