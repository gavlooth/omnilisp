module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_schema_tests(Interp* interp, int* pass, int* fail) {
    {

        MethodSignature sig = {
            .param_count = 1,
        };
        MethodConstraint[1] constraints = {
            { .param_sym = interp.symbols.intern("T"), .bound_type = INVALID_TYPE_ID }
        };
        sig.constraints = &constraints[0];
        sig.constraint_count = 1;

        TypeId[1] arg_types = { INVALID_TYPE_ID };
        SymbolId[1] handler_tags = { interp.symbols.intern("sample") };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        MethodEntry[1] entries = {
            {
                .sig = sig,
                .implementation = make_nil(interp),
            }
        };
        MethodTable mt = {
            .name = interp.symbols.intern("explain-test"),
            .entries = &entries[0],
            .entry_count = 1,
            .fallback = null,
        };
        Value* callee = interp.alloc_value();
        bool setup_ok = callee != null;
        if (setup_ok) {
            callee.tag = METHOD_TABLE;
            callee.method_table_val = &mt;
        }

        Value* params_fail = null;
        Value* constraints_fail = null;
        Value* arg_types_fail = null;
        Value* handler_tags_fail = null;
        Value* dispatch_fail = null;
        Value* effect_fail = null;
        Value*[] no_args = {};

        int saved_cons_fail = g_schema_explain_force_cons_alloc_fail_nth;

        g_schema_explain_force_cons_alloc_fail_nth = 1;
        params_fail = explain_signature_params(interp, &sig);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        constraints_fail = explain_constraints(interp, &sig);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        arg_types_fail = explain_arg_types(interp, arg_types[0:1]);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        handler_tags_fail = explain_effect_handler_tags(interp, &handler);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        dispatch_fail = setup_ok
            ? explain_dispatch_result_from_evaluated_call(interp, make_nil(interp), callee, no_args)
            : raise_error(interp, "setup failed");

        if (interp.handler_count >= interp.handler_capacity && !interp.grow_handler_stack()) setup_ok = false;
        if (setup_ok) {
            interp.handler_stack[interp.handler_count++] = handler;
            g_schema_explain_force_cons_alloc_fail_nth = 1;
            effect_fail = explain_effect_result_from_compiled_signal(
                interp,
                "(perform sample 1)",
                interp.symbols.intern("sample"),
                make_int(interp, 1));
            interp.handler_count--;
        } else {
            effect_fail = raise_error(interp, "setup failed");
        }

        g_schema_explain_force_cons_alloc_fail_nth = saved_cons_fail;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(params_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(constraints_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(arg_types_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(handler_tags_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(dispatch_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(effect_fail, "schema explain: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: schema explain list builders fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: schema explain list builders fail closed (setup_ok=%s params=%s constraints=%s arg_types=%s handler_tags=%s dispatch=%s effect=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(params_fail),
                runtime_alloc_error_text(constraints_fail),
                runtime_alloc_error_text(arg_types_fail),
                runtime_alloc_error_text(handler_tags_fail),
                runtime_alloc_error_text(dispatch_fail),
                runtime_alloc_error_text(effect_fail));
            (*fail)++;
        }
    }

    {
        MethodSignature sig = { .param_count = 1 };
        MethodTable ambiguous_mt = { .name = interp.symbols.intern("ambiguous-test") };
        DispatchMatchBreakdown breakdown = {
            .failure_reason = DISPATCH_MATCH_FAIL_ARITY,
            .failure_arg_index = usz.max
        };
        Value*[] no_args = {};
        TypeId[] no_types = {};
        Value*[1] ambiguous_args = { make_int(interp, 1) };
        Value* tie_indices =
            make_cons(interp, make_int(interp, 0), make_cons(interp, make_int(interp, 1), make_nil(interp)));

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* lambda_fail = lambda_call_type_error(null, &sig, no_args, 0, no_types, &breakdown, interp);
        Value* ambiguous_fail = dispatch_ambiguous_error(
            &ambiguous_mt,
            ambiguous_args[0:1],
            10,
            2,
            tie_indices,
            interp);
        Value* ctor_data = ctor_mismatch_data(
            interp,
            interp.symbols.intern("Box"),
            make_cons(interp, make_symbol(interp, interp.sym_Integer), make_nil(interp)),
            make_nil(interp),
            false,
            0,
            null);
        Value* runtime_effect_fail =
            jit_unhandled_effect_error(interp.symbols.intern("sample"), make_int(interp, 1), interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(lambda_fail, "lambda: expected 1 argument(s), got 0") &&
            runtime_alloc_error_matches(runtime_effect_fail, "runtime effect payload: out of memory") &&
            runtime_alloc_error_matches(ambiguous_fail, "ambiguous method call: multiple methods match with equal specificity") &&
            ctor_data == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: dispatch and ctor payload constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: dispatch and ctor payload constructors fail closed (lambda=%s runtime=%s ambiguous=%s ctor_null=%s)",
                runtime_alloc_error_text(lambda_fail),
                runtime_alloc_error_text(runtime_effect_fail),
                runtime_alloc_error_text(ambiguous_fail),
                ctor_data == null ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        usz saved_handler_count = interp.handler_count;
        bool setup_ok = interp.handler_count < interp.handler_capacity || interp.grow_handler_stack();
        SymbolId[1] handler_tags = { interp.sym_raise };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        Value* ctor_data = null;
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        if (setup_ok) {
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.handler_stack[interp.handler_count++] = handler;
            g_hashmap_force_struct_alloc_fail = true;
            ctor_data = ctor_mismatch_data(
                interp,
                interp.symbols.intern("Box"),
                make_cons(interp, make_symbol(interp, interp.sym_Integer), make_nil(interp)),
                make_nil(interp),
                false,
                0,
                null);
            g_hashmap_force_struct_alloc_fail = saved_struct_fail;
            interp.handler_count = saved_handler_count;
        }
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = setup_ok &&
                  ctor_data == null &&
                  !interp.flags.raise_pending &&
                  interp.raise_payload == null &&
                  interp.raise_msg_len == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ctor mismatch payload allocation failure does not preseed raise");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ctor mismatch payload allocation failure does not preseed raise (setup_ok=%s ctor_null=%s pending=%s)",
                setup_ok ? "yes" : "no",
                ctor_data == null ? "yes" : "no",
                interp.flags.raise_pending ? "yes" : "no");
            interp.handler_count = saved_handler_count;
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            (*fail)++;
        }
    }

    {
        usz saved_handler_count = interp.handler_count;
        bool setup_ok = interp.handler_count < interp.handler_capacity || interp.grow_handler_stack();
        SymbolId[1] handler_tags = { interp.sym_raise };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        SymbolId rel_sym = interp.symbols.intern("integrity-audit-rel");
        SymbolId col_sym = interp.symbols.intern("id");
        SymbolId[1] columns = { col_sym };
        Relation rel = {};
        rel.name = rel_sym;
        rel.columns = &columns[0];
        rel.col_count = 1;
        rel.key_col = col_sym;

        Value*[1] attempted = { make_int(interp, 1) };
        Value*[1] existing = { make_int(interp, 2) };
        Value* integrity_data = null;
        Value* iteration_data = null;
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        if (setup_ok) {
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.handler_stack[interp.handler_count++] = handler;
            g_hashmap_force_struct_alloc_fail = true;
            integrity_data = deduce_build_key_integrity_payload(&rel, attempted[..], existing[..], 0, interp);
            iteration_data = deduce_analyze_iteration_limit_payload(interp, 1, 32);
            g_hashmap_force_struct_alloc_fail = saved_struct_fail;
            interp.handler_count = saved_handler_count;
        }
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = setup_ok &&
                  integrity_data == null &&
                  iteration_data == null &&
                  !interp.flags.raise_pending &&
                  interp.raise_payload == null &&
                  interp.raise_msg_len == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: deduce optional payload allocation failures do not preseed raise");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: deduce optional payload allocation failures do not preseed raise (setup_ok=%s integrity_null=%s iteration_null=%s pending=%s)",
                setup_ok ? "yes" : "no",
                integrity_data == null ? "yes" : "no",
                iteration_data == null ? "yes" : "no",
                interp.flags.raise_pending ? "yes" : "no");
            interp.handler_count = saved_handler_count;
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            (*fail)++;
        }
    }

    {
        char[] response = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n";
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* response_fail = parse_response(response, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = runtime_alloc_error_matches(response_fail, "http: failed to allocate response payload");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: http response payload constructor fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: http response payload constructor fails closed (msg=%s)",
                runtime_alloc_error_text(response_fail));
            (*fail)++;
        }
    }

    {
        char key_byte = 'k';
        Value* support_map = null;
        DeduceDeltaTupleSet delta_set = {};
        DeduceSemiNaiveRuleExec proof_exec = {};
        proof_exec.interp = interp;
        proof_exec.aggregate_component_mode = true;
        bool already_seen = false;
        bool[] empty_bound = {};
        Value*[] empty_values = {};

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* delta_err = deduce_delta_set_insert(&delta_set, &key_byte, 1, interp);
        Value* support_err = deduce_tuple_support_table_set_count(&support_map, &key_byte, 1, 1, interp);
        Value* proof_err = deduce_seminaive_mark_current_proof_seen(
            &proof_exec,
            empty_bound,
            empty_values,
            &already_seen
        );
        Value* selector_err = deduce_analyze_goal_directed_selector_error(7, 1, make_nil(interp), interp);
        Value* surface_err = deduce_goal_directed_selector_surface_error(
            "deduce/test-selector",
            "deduce: selector test",
            7,
            1,
            make_nil(interp),
            interp
        );
        Value* relation_err = deduce_goal_directed_relation_surface_error(
            "deduce/test-relation",
            "deduce: relation test",
            7,
            1,
            interp.symbols.intern("rel"),
            interp
        );
        Value* iteration_payload = deduce_analyze_iteration_limit_payload(interp, 1, 32);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;
        bool ok =
            runtime_alloc_error_matches(delta_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(support_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(proof_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(selector_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(surface_err, "deduce: out of memory") &&
            runtime_alloc_error_matches(relation_err, "deduce: out of memory") &&
            iteration_payload == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: deduce runtime helper state payload constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: deduce runtime helper state payload constructors fail closed (delta=%s support=%s proof=%s selector=%s surface=%s relation=%s iteration_null=%s)",
                runtime_alloc_error_text(delta_err),
                runtime_alloc_error_text(support_err),
                runtime_alloc_error_text(proof_err),
                runtime_alloc_error_text(selector_err),
                runtime_alloc_error_text(surface_err),
                runtime_alloc_error_text(relation_err),
                iteration_payload == null ? "yes" : "no");
            (*fail)++;
        }
    }
}
