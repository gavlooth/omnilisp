module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_tensor_promotion_tests(Interp* interp, int* pass, int* fail) {

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        bool map_func_value_in_child = true;
        bool map_func_env_in_child = true;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(let (bias 10) (map (lambda (x) (+ x bias)) (Tensor Float64 [2] [1 2])))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;
            promoted = setup_ok ? promote_to_root(expr_eval.value, interp) : null;
            if (promoted != null &&
                promoted.tag == TENSOR &&
                promoted.tensor_val != null &&
                promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                promoted.tensor_val.map_func != null &&
                promoted.tensor_val.map_func.tag == CLOSURE &&
                promoted.tensor_val.map_func.closure_val != null) {
                map_func_value_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_func, child);
                map_func_env_in_child = env_chain_has_frame_in_scope(
                    promoted.tensor_val.map_func.closure_val.env,
                    child,
                    interp.global_env
                );
            }
            interp.current_scope = saved;
            interp.releasing_scope = null;
        }

        bool ok = setup_ok &&
            promoted != null &&
            promoted.tag == TENSOR &&
            promoted.scope_gen == saved.escape_generation &&
            promoted.tensor_val != null &&
            promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !map_func_value_in_child &&
            !map_func_env_in_child;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor closure map root promotion detaches callable env");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor closure map root promotion detaches callable env (setup=%s promoted=%s func_child=%s env_child=%s)",
                setup_ok ? "yes" : "no",
                promoted != null && promoted.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(promoted),
                map_func_value_in_child ? "yes" : "no",
                map_func_env_in_child ? "yes" : "no");
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult expr_eval = run("(realize (map (lambda (x) 'bad) (Tensor Float64 [1] [1])))", interp);
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = expr_eval.error.has_error &&
            str_starts_with(expr_eval.error.message[:256], "map: function result dtype mismatch") &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: failed lazy tensor realize cleans fresh result tensor");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: failed lazy tensor realize cleans fresh result tensor (error=%s msg=%s before=%d after=%d)",
                expr_eval.error.has_error ? "yes" : "no",
                expr_eval.error.has_error ? (ZString)expr_eval.error.message[:256] : runtime_alloc_error_text(expr_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult expr_eval = run("(Tensor Float64 [2] [1 'bad])", interp);
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = expr_eval.error.has_error &&
            str_starts_with(expr_eval.error.message[:256], "Tensor: data elements must be real numbers representable as Float64") &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: failed tensor constructor cleans partial tensor result");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: failed tensor constructor cleans partial tensor result (error=%s msg=%s before=%d after=%d)",
                expr_eval.error.has_error ? "yes" : "no",
                expr_eval.error.has_error ? (ZString)expr_eval.error.message[:256] : runtime_alloc_error_text(expr_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        int* opaque_payload = null;
        Value* opaque = null;
        Primitive* opaque_prim = null;
        Value* list = null;
        Value* iterator = null;
        Value* array_value = null;

        bool setup_ok = foreign != null;
        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_payload = (int*)interp.current_scope.alloc(int.sizeof);
            opaque = interp.alloc_value();
            if (opaque != null) {
                opaque.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            }
            setup_ok = opaque_payload != null && opaque != null;
        }

        if (setup_ok) {
            opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            setup_ok = opaque_prim != null;
        }

        if (setup_ok) {
            *opaque_payload = 701;
            *opaque_prim = {};
            opaque_prim.name[0] = 'o';
            opaque_prim.name[1] = 0;
            opaque_prim.func = &prim_add;
            opaque_prim.arity = 2;
            opaque_prim.tag = 701;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.tag = PRIMITIVE;
            opaque.prim_val = opaque_prim;

            list = make_cons(interp, opaque, make_nil(interp));
            iterator = iterator_from_list_value(list, interp);
            interp.current_scope = saved;
            array_value = make_array(interp, 1);
            setup_ok = list != null && list.tag == CONS &&
                iterator != null && iterator.tag == ITERATOR &&
                array_value != null && array_value.tag == ARRAY &&
                array_value.array_val != null;
        }
        interp.current_scope = saved;

        if (setup_ok) {
            array_value.array_val.items[0] = make_int(interp, 1);
            array_value.array_val.length = 1;
        }

        Value*[1] prim_array_args = { opaque };
        Value*[1] list_to_array_args = { list };
        Value*[3] array_set_args = { array_value, make_int(interp, 0), opaque };
        Value*[1] to_array_args = { iterator };

        if (setup_ok) interp.current_scope = foreign;
        Value* prim_array_boundary_fail = setup_ok ? prim_array(prim_array_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* list_to_array_boundary_fail = setup_ok ? prim_list_to_array(list_to_array_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* array_set_boundary_fail = setup_ok ? prim_array_set(array_set_args[0:3], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* to_array_boundary_fail = setup_ok ? prim_to_array(to_array_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        interp.current_scope = saved;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(prim_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(list_to_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(array_set_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(to_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            array_value.array_val.length == 1 &&
            array_value.array_val.items[0] != null &&
            array_value.array_val.items[0].tag == INT &&
            array_value.array_val.items[0].int_val == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: array write paths fail closed on boundary promotion faults");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: array write paths fail closed on boundary promotion faults (setup_ok=%s ctor=%s list=%s set=%s iter=%s array_intact=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(prim_array_boundary_fail),
                runtime_alloc_error_text(list_to_array_boundary_fail),
                runtime_alloc_error_text(array_set_boundary_fail),
                runtime_alloc_error_text(to_array_boundary_fail),
                setup_ok &&
                    array_value != null &&
                    array_value.tag == ARRAY &&
                    array_value.array_val != null &&
                    array_value.array_val.length == 1 &&
                    array_value.array_val.items[0] != null &&
                    array_value.array_val.items[0].tag == INT &&
                    array_value.array_val.items[0].int_val == 1 ? "yes" : "no");
            (*fail)++;
        }
        if (foreign != null) main::scope_release(foreign);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        SymbolId type_sym = interp.symbols.intern("__JitFieldBoundaryBox");
        SymbolId field_sym = interp.symbols.intern("slot");
        TypeFieldInfo* type_fields = (TypeFieldInfo*)mem::malloc(TypeFieldInfo.sizeof);
        bool setup_ok = foreign != null &&
            type_sym != INVALID_SYMBOL_ID &&
            field_sym != INVALID_SYMBOL_ID &&
            type_fields != null;

        TypeId type_id = INVALID_TYPE_ID;
        if (setup_ok) {
            type_fields[0] = {
                .name = field_sym,
                .field_type = INVALID_TYPE_ID,
                .annotation_sym = INVALID_SYMBOL_ID,
            };
            TypeInfo info = {
                .name = type_sym,
                .kind = TK_CONCRETE,
                .parent = INVALID_TYPE_ID,
                .fields = type_fields,
                .field_count = 1,
                .type_params = null,
                .type_param_count = 0,
                .constraints = null,
                .constraint_count = 0,
                .variants = null,
                .variant_count = 0,
                .alias_target = INVALID_TYPE_ID,
            };
            type_id = interp.types.register_type(info, &interp.symbols);
            setup_ok = type_id != INVALID_TYPE_ID;
            if (!setup_ok && type_fields != null) {
                mem::free(type_fields);
                type_fields = null;
            }
        }

        Value* initial = setup_ok ? make_int(interp, 1) : null;
        Value*[1] fields = { initial };
        Value* inst = setup_ok ? make_instance(interp, type_id, &fields[0], 1) : null;
        setup_ok = setup_ok &&
            inst != null &&
            inst.tag == INSTANCE &&
            inst.instance_val != null &&
            inst.instance_val.fields != null;

        int* opaque_payload = null;
        Value* opaque = null;
        Primitive* opaque_prim = null;
        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_payload = (int*)interp.current_scope.alloc(int.sizeof);
            opaque = interp.alloc_value();
            if (opaque != null) {
                opaque.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            }
            setup_ok = opaque_payload != null && opaque != null;
        }

        if (setup_ok) {
            opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            setup_ok = opaque_prim != null;
        }

        if (setup_ok) {
            *opaque_payload = 719;
            *opaque_prim = {};
            opaque_prim.name[0] = 'f';
            opaque_prim.name[1] = 0;
            opaque_prim.func = &prim_add;
            opaque_prim.arity = 2;
            opaque_prim.tag = 719;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* set_fail = null;
        if (setup_ok) {
            set_fail = jit_set_path_mutate_instance(inst, field_sym, opaque, interp);
        } else {
            set_fail = runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        }
        interp.current_scope = saved;

        bool field_intact = inst != null &&
            inst.tag == INSTANCE &&
            inst.instance_val != null &&
            inst.instance_val.fields != null &&
            inst.instance_val.fields[0].tag == INT &&
            inst.instance_val.fields[0].int_val == 1;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(set_fail, "boundary: refusing primitive copy with opaque user_data") &&
            field_intact;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: JIT instance field write fails closed on boundary promotion faults");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: JIT instance field write fails closed on boundary promotion faults (setup_ok=%s set=%s field_intact=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(set_fail),
                field_intact ? "yes" : "no");
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
    }

    {
        SymbolId export_sym = interp.symbols.intern("__module_growth_value");
        SymbolId first_name = interp.symbols.intern("__module_growth_first");
        Module* first = first_name != INVALID_SYMBOL_ID
            ? jit_create_file_module(first_name, "/tmp/omni-module-growth-first.omni", interp)
            : null;
        bool setup_ok = export_sym != INVALID_SYMBOL_ID &&
            first != null &&
            first.env != null &&
            module_append_export(first, export_sym) &&
            env_define_with_barrier_result(first.env, export_sym, make_int(interp, 42), interp) == null;
        if (setup_ok) first.loaded = true;

        Module* table_before = interp.modules;
        usz initial_capacity = interp.module_capacity;
        Value* mod_val = setup_ok ? make_module(interp, first) : null;
        setup_ok = setup_ok &&
            mod_val != null &&
            mod_val.tag == MODULE &&
            mod_val.module_val != null;

        usz target_count = initial_capacity + 2;
        for (usz i = 0; setup_ok && interp.module_count < target_count; i++) {
            char[80] name_buf;
            if (try char[] name = io::bprintf(
                &name_buf,
                "__module_growth_extra_%d_%d",
                (int)initial_capacity,
                (int)i
            )) {
                SymbolId name_sym = interp.symbols.intern(name);
                Module* extra = name_sym != INVALID_SYMBOL_ID
                    ? jit_create_file_module(name_sym, "/tmp/omni-module-growth-extra.omni", interp)
                    : null;
                setup_ok = extra != null;
                if (setup_ok) extra.loaded = true;
            } else {
                setup_ok = false;
            }
        }

        bool grew = table_before != null &&
            interp.modules != null &&
            interp.modules != table_before &&
            interp.module_capacity > initial_capacity;
        EvalResult step = setup_ok ? eval_path_step(mod_val, export_sym, interp) : eval_error("setup failed");
        bool ok = setup_ok &&
            grew &&
            !step.error.has_error &&
            step.value != null &&
            step.value.tag == INT &&
            step.value.int_val == 42;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: module values survive module table growth");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: module values survive module table growth (setup_ok=%s grew=%s value_ok=%s)",
                setup_ok ? "yes" : "no",
                grew ? "yes" : "no",
                step.value != null && step.value.tag == INT && step.value.int_val == 42 ? "yes" : "no");
            (*fail)++;
        }
    }
}
