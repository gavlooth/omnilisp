module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn void run_compiler_group_aot_runtime_parity_tests(Interp* interp, usz* pass, usz* fail) {
    bool ok = aot::aot_init();
    if (!ok) {
        (*fail)++;
        io::printn("[FAIL] Compiler: AOT runtime parity test bootstrap");
        return;
    }
    defer aot::aot_shutdown();

    {
        aot::AotMetaSpec[1] meta;
        meta[0].key = "T";
        meta[0].sym_value = "Number";
        meta[0].int_value = 0;
        meta[0].is_int = false;

        aot::AotTypeAnnotationSpec spec = {};
        spec.has_annotation = true;
        spec.is_dict = true;
        spec.base_type = "";
        spec.meta = &meta[0];
        spec.meta_count = 1;

        TypeAnnotation ann;
        bool init_ok = aot::aot_type_annotation_init_from_spec(&ann, &spec);
        bool parity_ok =
            init_ok &&
            ann.has_annotation &&
            ann.is_dict &&
            ann.base_type == (SymbolId)0 &&
            ann.meta_count == 1 &&
            ann.meta != null &&
            ann.meta[0].key != INVALID_SYMBOL_ID &&
            ann.meta[0].sym_value != INVALID_SYMBOL_ID;
        aot::aot_type_annotation_destroy(&ann);

        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT dict metadata annotation materializes without a base type"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT dict metadata annotation materializes without a base type"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20 22)", interp);
        Value* closure = aot::make_closure(null, &compiler_aot_test_capture_first);
        Value* arg_list = aot::cons(aot::make_int(20), aot::cons(aot::make_int(22), aot::make_nil()));
        Value* aot_result = aot::apply_multi(closure, arg_list, 2);
        bool parity_ok =
            !jit_result.error.has_error &&
            jit_result.value != null &&
            jit_result.value.tag == ValueTag.INT &&
            jit_result.value.int_val == 42 &&
            aot_result != null &&
            aot_result.tag == ValueTag.INT &&
            aot_result.int_val == 42;
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT multi-arg closure apply matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT multi-arg closure apply matches JIT"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20)", interp);
        Value* closure = aot::make_variadic_closure(null, &compiler_aot_test_exact_pair_sum);
        Value* aot_result = aot::invoke(closure, aot::make_int(20));
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "arity mismatch: expected 2 argument(s), got 1") &&
            compiler_error_value_contains(aot_result, "arity mismatch: expected 2 argument(s), got 1");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT fixed multi-arg under-application matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT fixed multi-arg under-application matches JIT"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20 22 1)", interp);
        Value* closure = aot::make_variadic_closure(null, &compiler_aot_test_exact_pair_sum);
        Value* arg_list = aot::cons(aot::make_int(20), aot::cons(aot::make_int(22), aot::cons(aot::make_int(1), aot::make_nil())));
        Value* aot_result = aot::apply_multi(closure, arg_list, 3);
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "not a function") &&
            compiler_error_value_contains(aot_result, "not a function");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT fixed multi-arg over-application matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT fixed multi-arg over-application matches JIT"); }
    }

    {
        EvalResult jit_result = run("(block (define cfg (Dictionary 'tag 42)) (signal cfg.tag 1))", interp);
        Value* aot_result = aot::compiled_signal(aot::make_int(42), aot::make_int(1));
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "effect tag must resolve to symbol") &&
            compiler_error_value_contains(aot_result, "effect tag must resolve to symbol");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT signal invalid-tag path matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT signal invalid-tag path matches JIT"); }
    }

    {
        EvalResult jit_result = run("(handle ^strict (signal my-eff 42) (other-eff v v))", interp);
        Value* strict_tag = aot::make_symbol("other-eff");
        Value* strict_handler = aot::make_closure(null, &compiler_aot_test_other_eff_handler);
        Value* strict_body = aot::make_closure(null, &compiler_aot_test_signal_my_eff);
        Value*[1] tags = { strict_tag };
        Value*[1] handlers = { strict_handler };
        Value* aot_result = aot::compiled_handle(&tags[0], &handlers[0], 1, strict_body, true);
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "strict handler") &&
            compiler_error_value_contains(aot_result, "strict handler");
        if (parity_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT strict handle boundary matches JIT");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT strict handle boundary matches JIT");
            if (jit_result.error.has_error) io::printfn("  JIT error: %s", (ZString)jit_result.error.message[..]);
            if (aot_result != null && aot_result.tag == ValueTag.ERROR) {
                io::printfn("  AOT error: %s", (ZString)aot_result.str_chars[:aot_result.str_len]);
            } else if (aot_result != null) {
                io::printfn("  AOT result tag: %d", (int)aot_result.tag);
            } else {
                io::printn("  AOT result: nil");
            }
        }
    }

    {
        Value* invalid_tag = aot::make_int(7);
        Value* handler = aot::make_closure(null, &compiler_aot_test_other_eff_handler);
        Value* body = aot::make_closure(null, &compiler_aot_test_constant_zero);
        Value*[1] tags = { invalid_tag };
        Value*[1] handlers = { handler };
        Value* aot_result = aot::compiled_handle(&tags[0], &handlers[0], 1, body, false);
        bool ok_invalid_tag = compiler_error_value_contains(aot_result, "effect tag must resolve to symbol");
        if (ok_invalid_tag) { (*pass)++; compiler_print_pass("Compiler: AOT handle invalid tag fails closed"); }
        else                { (*fail)++; io::printn("[FAIL] Compiler: AOT handle invalid tag fails closed"); }
    }

    {
        Value* libc = aot::ffi_declare_lib("libc_aot_policy", "libc.so.6");
        Interp* aot_interp = aot::aot_interp();

        EvalResult libc_meta = run(
            "(let (m (foreign-describe libc_aot_policy))"
            "  (if (and (= (ref m 'type) 'ForeignHandle)"
            "           (= (ref m 'runtime) 'c-abi)"
            "           (= (ref m 'kind) 'library)"
            "           (= (ref m 'name) \"libc.so.6\")"
            "           (array? (ref m 'capabilities))"
            "           (= (length (ref m 'capabilities)) 3)"
            "           (= (ref (ref m 'capabilities) 0) 'load)"
            "           (= (ref (ref m 'capabilities) 1) 'resolve)"
            "           (= (ref (ref m 'capabilities) 2) 'reflect))"
            "      1"
            "      0))",
            aot_interp);
        bool lib_meta_ok =
            aot_interp != null &&
            !libc_meta.error.has_error &&
            libc_meta.value != null &&
            libc_meta.value.tag == ValueTag.INT &&
            libc_meta.value.int_val == 1;
        if (lib_meta_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ForeignHandle library descriptor exposes load/resolve/reflect only");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ForeignHandle library descriptor exposes load/resolve/reflect only");
        }

        int[2] fopen_tags = { (int)FfiTypeTag.FFI_TYPE_STRING, (int)FfiTypeTag.FFI_TYPE_STRING };
        aot::AotFfiHandlePolicySpec fopen_return_policy;
        fopen_return_policy.is_foreign_handle = true;
        fopen_return_policy.name = "File";
        fopen_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_OWNED;
        fopen_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        fopen_return_policy.finalizer = "fclose";
        Value* fopen_prim = aot::ffi_declare_fn(
            "libc_aot_policy", "fopen_aot_policy", "fopen",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_PTR,
            true, true, null, &fopen_return_policy
        );
        Value* file = null;
        if (fopen_prim != null && fopen_prim.tag == PRIMITIVE) {
            Value* fopen_args = aot::cons(
                aot::make_string("/dev/null"),
                aot::cons(aot::make_string("r"), aot::make_nil())
            );
            file = aot::apply_multi(fopen_prim, fopen_args, 2);
        }
        bool owned_return_ok =
            file != null
            && file.tag == FFI_HANDLE
            && file.ffi_val != null
            && file.ffi_val.finalizer != null
            && compiler_str_eq(file.ffi_val.lib_name[:file.ffi_val.name_len], "File");
        if (owned_return_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle owned return resolves finalizer"); }
        else                 { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle owned return resolves finalizer"); }

        aot::AotFfiHandlePolicySpec bad_return_policy;
        bad_return_policy.is_foreign_handle = true;
        bad_return_policy.name = "File";
        bad_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_MANUAL;
        bad_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        bad_return_policy.finalizer = "fclose";
        Value* bad_manual_return = aot::ffi_declare_fn(
            "libc_aot_policy", "fclose_bad_policy_aot", "fclose",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, null, &bad_return_policy
        );
        bool manual_with_finalizer_rejected = compiler_error_value_contains(
            bad_manual_return,
            "manual ForeignHandle return cannot specify a finalizer"
        );
        if (manual_with_finalizer_rejected) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ffi rejects manual ownership with finalizer in policy");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ffi rejects manual ownership with finalizer in policy");
        }

        aot::AotFfiHandlePolicySpec borrowed_return_policy;
        borrowed_return_policy.is_foreign_handle = true;
        borrowed_return_policy.name = "File";
        borrowed_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_BORROWED;
        borrowed_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        borrowed_return_policy.finalizer = "fclose";
        Value* borrowed_with_finalizer_rejected = aot::ffi_declare_fn(
            "libc_aot_policy", "fclose_bad_policy_aot_borrowed", "fclose",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, null, &borrowed_return_policy
        );
        bool borrowed_with_finalizer_rejected_ok = compiler_error_value_contains(
            borrowed_with_finalizer_rejected,
            "finalizer-bearing ForeignHandle return must use owned ownership"
        );
        if (borrowed_with_finalizer_rejected_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ffi rejects borrowed ownership with finalizer in policy");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ffi rejects borrowed ownership with finalizer in policy");
        }

        int[1] fflush_tags = { (int)FfiTypeTag.FFI_TYPE_PTR };
        aot::AotFfiHandlePolicySpec[1] fflush_param_policies;
        fflush_param_policies[0].is_foreign_handle = true;
        fflush_param_policies[0].name = "File";
        fflush_param_policies[0].ownership = (int)FfiHandleOwnership.FFI_HANDLE_BORROWED;
        fflush_param_policies[0].nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        fflush_param_policies[0].finalizer = "";
        Value* fflush_prim = aot::ffi_declare_fn(
            "libc_aot_policy", "fflush_aot_policy", "fflush",
            &fflush_tags[0], 1, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, &fflush_param_policies[0], null
        );

        Value* nil_result = null;
        Value* wrong_family_result = null;
        if (fflush_prim != null && fflush_prim.tag == PRIMITIVE) {
            nil_result = aot::apply_multi(fflush_prim, aot::cons(aot::make_nil(), aot::make_nil()), 1);
            wrong_family_result = aot::apply_multi(fflush_prim, aot::cons(libc, aot::make_nil()), 1);
        }
        bool non_null_ok = compiler_error_value_contains(nil_result, "expected non-null ForeignHandle");
        if (non_null_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle non-null policy rejects nil"); }
        else             { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle non-null policy rejects nil"); }

        bool wrong_family_ok = compiler_error_value_contains(wrong_family_result, "expected ForeignHandle File");
        if (wrong_family_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle family policy rejects wrong handle"); }
        else                 { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle family policy rejects wrong handle"); }
    }
}
