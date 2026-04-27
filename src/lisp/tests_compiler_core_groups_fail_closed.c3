module lisp;

import std::collections::list;
import std::core::mem;
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
        int saved_force = g_compiler_force_result_copy_alloc_fail_nth;
        g_compiler_force_result_copy_alloc_fail_nth = 1;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_result_copy_alloc_fail_nth = saved_force;
        bool ok = code.len == 0 && str_contains(err, "copying generated C3 output");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: generated output copy allocation failure reports diagnostic"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: generated output copy allocation failure reports diagnostic"); }
    }

    {
        bool saved_force = g_compiler_force_result_dtor_register_fail;
        g_compiler_force_result_dtor_register_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_result_dtor_register_fail = saved_force;
        bool ok = code.len == 0 && str_contains(err, "copying generated C3 output");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: generated output dtor registration failure reports diagnostic"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: generated output dtor registration failure reports diagnostic"); }
    }

    {
        int saved_force = g_compiler_force_result_copy_alloc_fail_nth;
        g_compiler_force_result_copy_alloc_fail_nth = 1;
        char[] err = "";
        char[] code = compile_to_c3("(define x 1)\n2.[", interp, &err);
        g_compiler_force_result_copy_alloc_fail_nth = saved_force;
        bool ok = code.len == 0 && str_contains(err, "copying compile error message");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: compile-error message copy allocation failure reports diagnostic"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: compile-error message copy allocation failure reports diagnostic"); }
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
        g_compiler_force_call_arg_temps_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(+ 1 2)", interp, &err);
        g_compiler_force_call_arg_temps_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating call argument temporaries");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: normal call argument temp allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: normal call argument temp allocation failure fails closed"); }
    }

    {
        g_compiler_force_call_arg_temps_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(lambda () (+ 1 2))", interp, &err);
        g_compiler_force_call_arg_temps_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating call argument temporaries");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: tail call argument temp allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: tail call argument temp allocation failure fails closed"); }
    }

    {
        g_compiler_force_call_arg_temps_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(list 1 2)", interp, &err);
        g_compiler_force_call_arg_temps_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating call argument temporaries");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: list construction argument temp allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: list construction argument temp allocation failure fails closed"); }
    }

    {
        g_compiler_force_call_arg_temps_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(Dictionary 'k 1)", interp, &err);
        g_compiler_force_call_arg_temps_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating call argument temporaries");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: dictionary construction argument temp allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: dictionary construction argument temp allocation failure fails closed"); }
    }

    {
        g_compiler_force_call_arg_temps_alloc_fail = true;
        char[] err = "";
        char[] code = compile_to_c3("(explain 'dispatch (+ 1 2))", interp, &err);
        g_compiler_force_call_arg_temps_alloc_fail = false;
        bool ok = code.len == 0 && str_contains(err, "allocating call argument temporaries");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: explain dispatch argument temp allocation failure fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: explain dispatch argument temp allocation failure fails closed"); }
    }

    {
        Compiler compiler;
        compiler.init(interp);
        Value malformed_literal = {};
        malformed_literal.tag = FFI_HANDLE;
        compiler.compile_literal(&malformed_literal);
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "runtime object literal value");
        compiler.free();
        if (ok) { (*pass)++; compiler_print_pass("Compiler: runtime object native literal lowering fails closed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: runtime object native literal lowering fails closed"); }
    }

    {
        char[] code = compile_to_c3("(let (x 1) (block (define f (lambda () x)) (set! x 2) (f)))", interp);
        bool ok = compiler_code_contains_all(code, {
            "aot::make_mutable_cell_checked(",
            "aot::mutable_cell_set_checked(",
            "aot::mutable_cell_get(",
            "aot::AotCaptureRetention* _closure_retention_",
            " = null;",
            "aot::make_generated_closure_with_retention("
        }) && !str_contains(code, "aot::define_var(\"x\"")
            && !str_contains(code, ", true)");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mutable-capture AOT cell path is observed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mutable-capture AOT cell path is observed"); }
    }

    {
        char[] code = compile_to_c3(
            "(let (x 1) "
            "  (let (y 2) "
            "    (lambda () (block x (set! y 3) y))))",
            interp
        );
        bool ok = str_contains(code, "aot::capture_retention_retain_value(")
            && str_contains(code, "if (_closure_capture_ok_")
            && str_contains(code, ".captured_y")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && !str_contains(code, "unsupported");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: mixed closure captures guard mutable writes after retention failure"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: mixed closure captures guard mutable writes after retention failure"); }
    }

    {
        char[] code = compile_to_c3(
            "(block "
            "  (let (audit242_leak 1) "
            "    (let (f (lambda () (block (set! audit242_leak 2) audit242_leak))) "
            "      (f))) "
            "  audit242_leak)",
            interp
        );
        bool ok = compiler_code_contains_all(code, {
            "aot::make_mutable_cell_checked(",
            "aot::mutable_cell_set_checked(",
            "aot::mutable_cell_get("
        }) && compiler_code_contains_none(code, {
            "aot::define_var(\"audit242_leak\"",
            "aot::set_var(\"audit242_leak\"",
            "captured_audit242_leak = aot::lookup_var(\"audit242_leak\")"
        });
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-242 lexical mutable let leak uses cells"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-242 lexical mutable let leak uses cells"); }
    }

    {
        char[] code = compile_to_c3(
            "(let (audit242_shadow 10) "
            "  (block "
            "    (let (audit242_shadow 1) "
            "      (let (f (lambda () (block (set! audit242_shadow 2) audit242_shadow))) "
            "        (f))) "
            "    audit242_shadow))",
            interp
        );
        bool ok = compiler_code_count_occurrences(code, "aot::make_mutable_cell_checked(") >= 1
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && !str_contains(code, "aot::define_var(\"audit242_shadow\"")
            && !str_contains(code, "aot::set_var(\"audit242_shadow\"");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-242 same-name mutable capture uses cell identity"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-242 same-name mutable capture uses cell identity"); }
    }

    {
        char[] code = compile_to_c3(
            "(block "
            "  (let (audit242_mix 1) "
            "    (let (f (lambda () (block (set! audit242_mix 2) audit242_mix))) "
            "      (f))) "
            "  (let (audit242_mix 10) "
            "    (lambda () audit242_mix)))",
            interp
        );
        bool ok = str_contains(code, "aot::make_mutable_cell_checked(")
            && str_contains(code, "aot::AotMutableCell* captured_audit242_mix")
            && str_contains(code, "lisp::Value* captured_audit242_mix")
            && compiler_code_contains_none(code, {"unsupported expr type", "aot::lookup_var(\"audit242_mix\""});
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-242 same-name immutable capture remains value capture"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-242 same-name immutable capture remains value capture"); }
    }

    {
        char[] code = compile_to_c3("(lambda () (set! audit242_unbound 1))", interp);
        bool ok = str_contains(code, "aot::set_var_result(\"audit242_unbound\"")
            && !str_contains(code, "captured_audit242_unbound")
            && !str_contains(code, "audit242_unbound = _r");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-242 unbound lambda set! fails closed through env set"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-242 unbound lambda set! fails closed through env set"); }
    }

    {
        char[] code = compile_to_c3(
            "(let ^rec (audit243_rec "
            "  (lambda (n) "
            "    (if (= n 0) "
            "        0 "
            "        (block "
            "          (set! audit243_rec (lambda (m) 99)) "
            "          (audit243_rec 0))))) "
            "  (audit243_rec 1))",
            interp
        );
        bool ok = str_contains(code, "aot::make_mutable_cell_checked(aot::make_nil()")
            && str_contains(code, "aot::AotMutableCell* captured_audit243_rec")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && str_contains(code, "aot::mutable_cell_get(")
            && !str_contains(code, "captured_audit243_rec = _aot_let_")
            && !str_contains(code, "aot::set_var_result(\"audit243_rec\"");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-243 recursive mutable self-capture uses cell"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-243 recursive mutable self-capture uses cell"); }
    }

    {
        char[] code = compile_to_c3(
            "(let (audit243_x 0) "
            "  (block "
            "    (let (audit243_f (lambda () (set! audit243_x 9))) "
            "      (block "
            "        (module audit243_m (export audit243_x) "
            "          (block "
            "            (define audit243_x 1) "
            "            (with audit243_m (set! audit243_x 2)) "
            "            audit243_x)) "
            "        audit243_x))))",
            interp
        );
        bool ok = str_contains(code, "aot::AotMutableCell* captured_audit243_x")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && str_contains(code, "_aot_mod_")
            && str_contains(code, " = _r")
            && !str_contains(code, "aot::set_var_result(\"audit243_x\"");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AUDIT-243 module set beats older mutable cell"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AUDIT-243 module set beats older mutable cell"); }
    }

    {
        char[] code = compile_to_c3(
            "(block "
            "  (module aot_define_import_src (export value) (define value 7)) "
            "  (import (aot_define_import_src (value 'as imported_value))) "
            "  imported_value)",
            interp
        );
        bool ok = str_contains(code, " = aot::define_var(\"imported_value\", _aot_mod_")
            && str_contains(code, "aot::is_error(_r");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import AOT define_var result is observed"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import AOT define_var result is observed"); }
    }

    {
        char[] code = compile_to_c3("(let (x 42) (lambda () x))", interp);
        bool ok = compiler_code_contains_all(code, {
            "mem::malloc(Lambda_",
            "if (_closure_data == null) return aot::make_error(\"compiler: out of memory while allocating closure capture data\");",
            "aot::capture_retention_begin()",
            "aot::capture_retention_retain_value(_closure_retention, _capture_source_",
            "aot::make_generated_closure_with_retention((void*)_closure_data, &invoke_lambda_"
        }) && !str_contains(code, ".tag == lisp::ValueTag.ERROR) mem::free(_closure_data");
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

            aot::AotCaptureRetention* dtor_registration_retention = aot::capture_retention_begin();
            bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            Value* closure_dtor_registration_fail = aot::make_closure_with_retention(
                null,
                &compiler_aot_test_unary_invoke,
                dtor_registration_retention,
                false);
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;

            void* owned_closure_dtor_data = mem::malloc(8);
            int owned_dtor_free_before = aot::g_aot_owned_data_free_count;
            saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            Value* owned_closure_dtor_fail = owned_closure_dtor_data != null
                ? aot::make_closure_with_retention(
                    owned_closure_dtor_data,
                    &compiler_aot_test_unary_invoke,
                    null,
                    true)
                : null;
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            int owned_dtor_free_after = aot::g_aot_owned_data_free_count;

            void* owned_closure_data = mem::malloc(8);
            int owned_data_free_before = aot::g_aot_owned_data_free_count;
            aot::g_aot_force_closure_primitive_alloc_fail = true;
            Value* owned_closure_prim_fail = owned_closure_data != null
                ? aot::make_closure_with_retention(
                    owned_closure_data,
                    &compiler_aot_test_unary_invoke,
                    null,
                    true)
                : null;
            aot::g_aot_force_closure_primitive_alloc_fail = false;
            int owned_data_free_after = aot::g_aot_owned_data_free_count;

            void* owned_variadic_data = mem::malloc(8);
            int owned_variadic_free_before = aot::g_aot_owned_data_free_count;
            aot::g_aot_force_variadic_closure_primitive_alloc_fail = true;
            Value* owned_variadic_prim_fail = owned_variadic_data != null
                ? aot::make_variadic_closure_with_retention(
                    owned_variadic_data,
                    &compiler_aot_test_variadic_invoke,
                    null,
                    true)
                : null;
            aot::g_aot_force_variadic_closure_primitive_alloc_fail = false;
            int owned_variadic_free_after = aot::g_aot_owned_data_free_count;

            void* owned_variadic_dtor_data = mem::malloc(8);
            int owned_variadic_dtor_free_before = aot::g_aot_owned_data_free_count;
            saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            Value* owned_variadic_dtor_fail = owned_variadic_dtor_data != null
                ? aot::make_variadic_closure_with_retention(
                    owned_variadic_dtor_data,
                    &compiler_aot_test_variadic_invoke,
                    null,
                    true)
                : null;
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            int owned_variadic_dtor_free_after = aot::g_aot_owned_data_free_count;

            Interp* aot_interp = aot::aot_interp();
            main::ScopeRegion* saved_scope = null;
            main::ScopeRegion* child_scope = aot_interp != null ? boundary_push_child_scope(aot_interp, &saved_scope) : null;
            Value* capture_audit_fail = null;
            if (child_scope != null) {
                Value* captured = aot::lookup_prim("+");
                aot::AotCaptureRetention* capture_audit_retention = aot::capture_retention_begin();
                aot::g_aot_force_capture_reachability_audit_fail = true;
                capture_audit_fail = aot::capture_retention_retain_value(capture_audit_retention, captured);
                aot::g_aot_force_capture_reachability_audit_fail = false;
                aot::capture_retention_release(capture_audit_retention);
                boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
                child_scope = null;
            }
            if (child_scope != null && aot_interp != null) {
                boundary_pop_child_scope(aot_interp, saved_scope, child_scope);
            }
            aot::g_aot_force_capture_reachability_audit_fail = false;

            Value* closure_missing_invoke = aot::make_closure(null, null);
            Value* variadic_missing_invoke = aot::make_variadic_closure(null, null);

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
            Value* match_guard_null_result = null;
            Value* lookup_prim_missing = null;
            Value* lookup_prim_bridge_interp_unavailable = null;
            Value* define_var_success = null;
            Value* define_var_lookup = null;
            Value* define_var_error_input = null;
            Value* define_var_bridge_interp_unavailable = null;
            Value* define_var_barrier_fail = null;
            Value* lookup_var_missing = null;
            Value* lookup_var_bridge_interp_unavailable = null;
            Value* lookup_module_export_missing_binding = null;
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
            Value* null_guard = aot::make_closure(null, &compiler_aot_test_null_invoke);
            if (null_guard != null && null_guard.tag == ValueTag.PRIMITIVE) {
                match_guard_null_result = aot::match_guard_eval(null_guard, aot::make_int(1));
            }

            aot::g_aot_force_bridge_interp_unavailable = true;
            lookup_prim_bridge_interp_unavailable = aot::lookup_prim("+");
            define_var_bridge_interp_unavailable = aot::define_var("__aot_define_var_bridge_unavailable", aot::make_int(1));
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

            define_var_success = aot::define_var("__aot_define_var_success", aot::make_int(314));
            define_var_lookup = aot::lookup_var("__aot_define_var_success");
            lookup_prim_missing = aot::lookup_prim("__aot_missing_primitive_for_fail_closed_test");
            lookup_var_missing = aot::lookup_var("__aot_missing_var_for_fail_closed_test");
            define_var_error_input = aot::define_var(
                "__aot_define_var_error_input",
                lookup_var_missing);

            SymbolId malformed_module_name = aot_interp.symbols.intern("__aot_malformed_module_for_fail_closed_test");
            SymbolId malformed_export_name = aot_interp.symbols.intern("__aot_missing_export_for_fail_closed_test");
            usz malformed_mod_idx = usz.max;
            if (malformed_module_name != INVALID_SYMBOL_ID &&
                malformed_export_name != INVALID_SYMBOL_ID &&
                find_module(malformed_module_name, aot_interp) == null &&
                aot_interp.ensure_module_capacity(aot_interp.module_count + 1)) {
                malformed_mod_idx = aot_interp.module_count;
                Module* malformed_mod = &aot_interp.modules[malformed_mod_idx];
                *malformed_mod = {};
                malformed_mod.name = malformed_module_name;
                malformed_mod.loaded = true;
                malformed_mod.env = boundary_make_env_in_root(aot_interp, aot_interp.global_env);
                malformed_mod.exports = jit_alloc_module_exports(1);
                if (malformed_mod.env != null && malformed_mod.exports != null) {
                    malformed_mod.export_capacity = 1;
                    malformed_mod.export_count = 1;
                    malformed_mod.exports[0] = malformed_export_name;
                    aot_interp.module_count++;
                    if (interp_module_hash_insert(aot_interp, malformed_module_name, malformed_mod_idx)) {
                        lookup_module_export_missing_binding = aot::lookup_module_export(
                            "__aot_malformed_module_for_fail_closed_test",
                            "__aot_missing_export_for_fail_closed_test");
                    }
                    rollback_module_publication(aot_interp, malformed_mod_idx);
                } else {
                    module_release_storage(malformed_mod);
                    malformed_mod_idx = usz.max;
                }
            }

            bool saved_barrier_promote_failure = g_env_force_barrier_promote_failure;
            g_env_force_barrier_promote_failure = true;
            define_var_barrier_fail = aot::define_var("__aot_define_var_barrier_fail", aot::make_int(2));
            g_env_force_barrier_promote_failure = saved_barrier_promote_failure;

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
            Value* ffi_optional_lib_redeclare = aot::ffi_declare_lib("libc", "libm.so.6");

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
                && closure_dtor_registration_fail != null && closure_dtor_registration_fail.tag == ValueTag.ERROR
                && owned_closure_dtor_data != null
                && owned_closure_dtor_fail != null && owned_closure_dtor_fail.tag == ValueTag.ERROR
                && owned_dtor_free_after == owned_dtor_free_before + 1
                && owned_closure_data != null
                && owned_closure_prim_fail != null && owned_closure_prim_fail.tag == ValueTag.ERROR
                && owned_data_free_after == owned_data_free_before + 1
                && owned_variadic_data != null
                && owned_variadic_prim_fail != null && owned_variadic_prim_fail.tag == ValueTag.ERROR
                && owned_variadic_free_after == owned_variadic_free_before + 1
                && owned_variadic_dtor_data != null
                && owned_variadic_dtor_fail != null && owned_variadic_dtor_fail.tag == ValueTag.ERROR
                && owned_variadic_dtor_free_after == owned_variadic_dtor_free_before + 1
                && capture_audit_fail != null && capture_audit_fail.tag == ValueTag.ERROR
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
                && apply_multi_tail_negative_argc != null && apply_multi_tail_negative_argc.tag == ValueTag.ERROR
                && compiler_error_value_contains(match_guard_null_result, "callable guard returned null");

            bool bridge_interp_errors_ok =
                lookup_prim_bridge_interp_unavailable != null && lookup_prim_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiler_error_value_contains(lookup_prim_missing, "primitive '__aot_missing_primitive_for_fail_closed_test' not found")
                && define_var_success == null
                && define_var_lookup != null && define_var_lookup.tag == ValueTag.INT && define_var_lookup.int_val == 314
                && compiler_error_value_contains(lookup_var_missing, "unbound variable '__aot_missing_var_for_fail_closed_test'")
                && define_var_error_input == lookup_var_missing
                && define_var_bridge_interp_unavailable != null && define_var_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiler_error_value_contains(define_var_barrier_fail, "out of memory while defining binding")
                && lookup_var_bridge_interp_unavailable != null && lookup_var_bridge_interp_unavailable.tag == ValueTag.ERROR
                && compiler_error_value_contains(lookup_module_export_missing_binding, "exported symbol '__aot_missing_export_for_fail_closed_test' is not bound")
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
                && ffi_optional_lib_redeclare != null && ffi_optional_lib_redeclare.tag == ValueTag.ERROR
                && str_contains(ffi_optional_lib_redeclare.str_chars[:ffi_optional_lib_redeclare.str_len], "different soname")
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
        bool ok = compiler_fail_closed_aot_no_interp_diagnostic_test();
        if (ok) { (*pass)++; compiler_print_pass("Compiler: AOT helpers emit diagnostics without runtime interpreter"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: AOT helpers emit diagnostics without runtime interpreter"); }
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
        char[SYMBOL_MAX_NAME_LEN + 1] too_long_symbol;
        for (usz i = 0; i < too_long_symbol.len; i++) too_long_symbol[i] = 'a';
        char[] err = "";
        char[] code = compile_to_c3(too_long_symbol[..], interp, &err);
        bool ok = code.len == 0 && str_contains(err, "failed to intern symbol");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser symbol interning fails closed on overlong symbol"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser symbol interning fails closed on overlong symbol"); }
    }

    compiler_test_finish_bool(compiler_fail_closed_datum_root_alloc_tests(interp),
        "Compiler: parser datum root allocation failures fail closed",
        "[FAIL] Compiler: parser datum root allocation failures fail closed", pass, fail);

    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();
        compiler.emit_int(long.min);
        char[] code = compiler.get_output();
        bool ok = code.len > 0 && str_contains(code, "-9223372036854775808");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: emit_int preserves long.min without overflow"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: emit_int preserves long.min without overflow"); }
    }

    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();
        compiler.emit_usz(usz.max);
        char[] code = compiler.get_output();
        bool ok = code.len > 0 && str_contains(code, "18446744073709551615");
        if (ok) { (*pass)++; compiler_print_pass("Compiler: emit_usz preserves max-width usz values"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: emit_usz preserves max-width usz values"); }
    }

    compiler_test_finish_bool(compiler_fail_closed_symbol_name_mangling_test(interp), "Compiler: emitted symbol names are capped and hash-disambiguated", "[FAIL] Compiler: emitted symbol names are capped and hash-disambiguated", pass, fail);

    compiler_test_finish_bool(compiler_fail_closed_generated_name_bounds_tests(interp), "Compiler: generated names fail closed on fixed-buffer overflow", "[FAIL] Compiler: generated names fail closed on fixed-buffer overflow", pass, fail);

    {
        List{Expr*} exprs = parse_program("1 2.[", interp);
        defer exprs.free();
        bool ok = exprs.len() == 0;
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parse_program fails closed on malformed postfix partial expr"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parse_program fails closed on malformed postfix partial expr"); }
    }

    {
        bool ok = compiler_fail_closed_direct_program_expr_limit_test(interp);
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parse_program rejects too many top-level expressions"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parse_program rejects too many top-level expressions"); }
    }

    {
        bool ok = compiler_fail_closed_compile_program_expr_limit_test(interp);
        if (ok) { (*pass)++; compiler_print_pass("Compiler: parser rejects too many top-level expressions"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: parser rejects too many top-level expressions"); }
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

    run_compiler_group_module_fail_closed_tail_tests(interp, pass, fail);
}
