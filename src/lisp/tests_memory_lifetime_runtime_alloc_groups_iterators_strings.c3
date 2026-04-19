module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_iterators_strings_tests(Interp* interp, int* pass, int* fail) {
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* pair = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* temp_string = make_string(interp, "x");
            Value* escape_nil = interp.alloc_value_escape();
            if (temp_string == null || temp_string.tag == ERROR || escape_nil == null) {
                setup_ok = false;
            } else {
                escape_nil.tag = NIL;
                bool saved_force = g_boundary_force_escape_string_or_error_promotion_alloc_fail;
                g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
                pair = make_cons(interp, temp_string, escape_nil);
                g_boundary_force_escape_string_or_error_promotion_alloc_fail = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(pair, "boundary: failed to allocate string/error escape copy");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_cons escape promotion faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_cons escape promotion faults fail closed (setup_ok=%s pair=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(pair));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* pair = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* escape_car = interp.alloc_value_escape();
            Value* escape_nil = interp.alloc_value_escape();
            if (escape_car == null || escape_nil == null) {
                setup_ok = false;
            } else {
                escape_car.tag = INT;
                escape_car.int_val = 7;
                escape_nil.tag = NIL;
                int saved_force = g_boundary_force_alloc_escape_value_fail_nth;
                g_boundary_force_alloc_escape_value_fail_nth = 1;
                pair = make_cons(interp, escape_car, escape_nil);
                g_boundary_force_alloc_escape_value_fail_nth = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(pair, "cons: failed to allocate escape pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_cons escape wrapper alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_cons escape wrapper alloc faults fail closed (setup_ok=%s pair=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(pair));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        int saved_force = g_value_force_closure_scope_alloc_fail_nth;
        g_value_force_closure_scope_alloc_fail_nth = 1;
        Value* closure_fail = make_closure_no_param(interp, null, interp.global_env);
        g_value_force_closure_scope_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(closure_fail, "closure: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure payload alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: closure payload alloc faults fail closed (msg=%s)",
                runtime_alloc_error_text(closure_fail));
            (*fail)++;
        }
    }

    {
        int saved_force = g_value_force_closure_scope_alloc_fail_nth;
        g_value_force_closure_scope_alloc_fail_nth = 2;
        Value* closure_fail = make_closure(interp, interp.symbols.intern("__closure_param_oom"), null, interp.global_env);
        g_value_force_closure_scope_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(closure_fail, "closure: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure param alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: closure param alloc faults fail closed (msg=%s)",
                runtime_alloc_error_text(closure_fail));
            (*fail)++;
        }
    }

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;

        Value*[] no_args = {};
        Value* dispatch_fail =
            explain_dispatch_result_from_evaluated_call(interp, make_nil(interp), make_nil(interp), no_args);
        Value* effect_fail =
            explain_effect_result_from_compiled_signal(
                interp,
                "(perform sample 1)",
                interp.symbols.intern("sample"),
                make_int(interp, 1));
        Value* helper_fail = explain_method_source(interp, make_nil(interp));

        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(dispatch_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(effect_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(helper_fail, "schema explain: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: schema explain constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: schema explain constructor alloc failures fail closed (dispatch=%s effect=%s helper=%s)",
                runtime_alloc_error_text(dispatch_fail),
                runtime_alloc_error_text(effect_fail),
                runtime_alloc_error_text(helper_fail));
            (*fail)++;
        }
    }

    {
        bool saved_partial_force = g_iterator_force_partial_alloc_fail;
        g_iterator_force_partial_alloc_fail = true;
        Value* iterator_fail = iterator_from_list_value(make_nil(interp), interp);
        g_iterator_force_partial_alloc_fail = saved_partial_force;

        bool saved_wrapper_force = g_value_force_make_iterator_alloc_fail;
        g_value_force_make_iterator_alloc_fail = true;
        Value* wrapper_fail = make_iterator(interp, make_nil(interp));
        g_value_force_make_iterator_alloc_fail = saved_wrapper_force;

        bool ok = runtime_alloc_error_matches(iterator_fail, "iterator: failed to allocate thunk") &&
                  runtime_alloc_error_matches(wrapper_fail, "iterator: failed to allocate iterator value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator helper alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator helper alloc failures fail closed (thunk_ok=%s wrapper_ok=%s)",
                runtime_alloc_error_matches(iterator_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_matches(wrapper_fail, "iterator: failed to allocate iterator value") ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* base_iterator = iterator_from_list_value(list, interp);
        Value*[1] repeat_args = { make_int(interp, 7) };
        Value*[2] take_args = { make_int(interp, 2), base_iterator };

        bool saved_force = g_iterator_force_partial_alloc_fail;
        g_iterator_force_partial_alloc_fail = true;
        Value* repeat_fail = prim_iterator_repeat_thunk(repeat_args[0:1], null, interp);
        Value* take_fail = prim_iterator_take_thunk(take_args[0:2], null, interp);
        g_iterator_force_partial_alloc_fail = saved_force;

        bool ok = runtime_alloc_error_matches(repeat_fail, "iterator: failed to allocate thunk") &&
                  runtime_alloc_error_matches(take_fail, "iterator: failed to allocate thunk");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator tail constructor errors propagate");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator tail constructor errors propagate (repeat=%s take=%s repeat_msg=%s take_msg=%s)",
                runtime_alloc_error_matches(repeat_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_matches(take_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_text(repeat_fail),
                runtime_alloc_error_text(take_fail));
            (*fail)++;
        }
    }

    {
        Value* left_list = make_cons(interp, make_int(interp, 1), make_nil(interp));
        Value* right_list = make_cons(interp, make_int(interp, 2), make_nil(interp));
        Value* left_iterator = iterator_from_list_value(left_list, interp);
        Value* right_iterator = iterator_from_list_value(right_list, interp);
        Value*[2] zip_args = { left_iterator, right_iterator };

        int saved_force = g_iterator_force_cons_alloc_fail_nth;
        g_iterator_force_cons_alloc_fail_nth = 1;
        Value* zip_fail = prim_iterator_zip_thunk(zip_args[0:2], null, interp);
        g_iterator_force_cons_alloc_fail_nth = saved_force;

        bool setup_ok = left_iterator != null && left_iterator.tag == ITERATOR &&
            right_iterator != null && right_iterator.tag == ITERATOR;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(zip_fail, "__iterator-zip: failed to allocate item pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator zip item pair allocation failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator zip item pair allocation failures fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(zip_fail));
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value*[1] one_arg = { make_int(interp, 1) };
        Value*[2] filter_bad_args = { make_int(interp, 1), make_nil(interp) };
        Value* map_missing_state = prim_iterator_map_thunk(no_args[0:0], null, interp);
        Value* filter_missing_state = prim_iterator_filter_thunk(no_args[0:0], null, interp);
        Value* take_missing_state = prim_iterator_take_thunk(no_args[0:0], null, interp);
        Value* zip_missing_state = prim_iterator_zip_thunk(no_args[0:0], null, interp);
        Value* list_missing_state = prim_iterator_from_list_thunk(no_args[0:0], null, interp);
        Value* array_missing_state = prim_iterator_from_array_thunk(one_arg[0:1], null, interp);
        Value* filter_bad_iter_state = prim_iterator_filter_thunk(filter_bad_args[0:2], null, interp);

        bool ok =
            runtime_alloc_error_matches(map_missing_state, "__iterator-map: expected iterator state") &&
            runtime_alloc_error_matches(filter_missing_state, "__iterator-filter: expected iterator state") &&
            runtime_alloc_error_matches(take_missing_state, "__iterator-take: expected iterator state") &&
            runtime_alloc_error_matches(zip_missing_state, "__iterator-zip: expected iterators") &&
            runtime_alloc_error_matches(list_missing_state, "__iterator-from-list: expected list state") &&
            runtime_alloc_error_matches(array_missing_state, "__iterator-from-array: expected array state") &&
            runtime_alloc_error_matches(filter_bad_iter_state, "__iterator-filter: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator thunk state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator thunk state fails closed (map=%s filter=%s take=%s zip=%s list=%s array=%s bad_filter=%s)",
                runtime_alloc_error_text(map_missing_state),
                runtime_alloc_error_text(filter_missing_state),
                runtime_alloc_error_text(take_missing_state),
                runtime_alloc_error_text(zip_missing_state),
                runtime_alloc_error_text(list_missing_state),
                runtime_alloc_error_text(array_missing_state),
                runtime_alloc_error_text(filter_bad_iter_state)
            );
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value*[1] list_null_args = { null };
        Value*[1] range_bad_args = { make_nil(interp) };
        Value*[2] cycle_bad_args = { make_int(interp, 1), make_nil(interp) };
        Value* range_missing_state = prim_iterator_range_from_thunk(no_args[0:0], null, interp);
        Value* list_null_state = prim_iterator_from_list_thunk(list_null_args[0:1], null, interp);
        Value* range_bad_state = prim_iterator_range_from_thunk(range_bad_args[0:1], null, interp);
        Value* repeat_missing_state = prim_iterator_repeat_thunk(no_args[0:0], null, interp);
        Value* cycle_missing_state = prim_iterator_cycle_thunk(no_args[0:0], null, interp);
        Value* cycle_bad_state = prim_iterator_cycle_thunk(cycle_bad_args[0:2], null, interp);

        bool ok =
            runtime_alloc_error_matches(range_missing_state, "__iterator-range-from: expected integer state") &&
            runtime_alloc_error_matches(list_null_state, "__iterator-from-list: expected list state") &&
            runtime_alloc_error_matches(range_bad_state, "__iterator-range-from: expected integer state") &&
            runtime_alloc_error_matches(repeat_missing_state, "__iterator-repeat: expected item state") &&
            runtime_alloc_error_matches(cycle_missing_state, "__iterator-cycle: expected iterator state") &&
            runtime_alloc_error_matches(cycle_bad_state, "__iterator-cycle: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator source state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator source state fails closed (range_missing=%s list_null=%s range_bad=%s repeat=%s cycle_missing=%s cycle_bad=%s)",
                runtime_alloc_error_text(range_missing_state),
                runtime_alloc_error_text(list_null_state),
                runtime_alloc_error_text(range_bad_state),
                runtime_alloc_error_text(repeat_missing_state),
                runtime_alloc_error_text(cycle_missing_state),
                runtime_alloc_error_text(cycle_bad_state)
            );
            (*fail)++;
        }
    }

    {
        Value* null_next = make_primitive(interp, "__test-null-iterator-next", &runtime_alloc_null_iterator_next, 1);
        Value* null_iter = null_next != null && null_next.tag != ERROR ? make_iterator(interp, null_next) : null_next;
        Value*[3] fold_args = {
            make_primitive(interp, "+", &prim_add, 2),
            make_int(interp, 0),
            null_iter
        };
        Value* fold_null_pair = prim_iterator_foldl(fold_args[0:3], null, interp);

        bool ok = runtime_alloc_error_matches(fold_null_pair, "__iterator-foldl: expected iterator pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator fold state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator fold state fails closed (msg=%s)",
                runtime_alloc_error_text(fold_null_pair)
            );
            (*fail)++;
        }
    }

    {
        EvalResult func_run = run("(lambda (a x) (+ a x))", interp);
        Value* list = make_cons(interp, make_int(interp, 1), make_nil(interp));
        Value* iterator = iterator_from_list_value(list, interp);
        Value*[3] fold_args = {
            func_run.error.has_error ? null : func_run.value,
            make_int(interp, 0),
            iterator
        };

        int saved_force = g_iterator_force_cons_alloc_fail_nth;
        g_iterator_force_cons_alloc_fail_nth = 1;
        Value* fold_fail =
            !func_run.error.has_error && iterator != null && iterator.tag == ITERATOR
            ? prim_iterator_foldl(fold_args[0:3], null, interp)
            : raise_error(interp, "setup failed");
        g_iterator_force_cons_alloc_fail_nth = saved_force;

        bool setup_ok = !func_run.error.has_error && iterator != null && iterator.tag == ITERATOR;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(fold_fail, "__iterator-foldl: failed to allocate call args");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator foldl arg list allocation failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator foldl arg list allocation failures fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(fold_fail));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_string_force_result_wrapper_alloc_fail;
        g_string_force_result_wrapper_alloc_fail = true;
        StringVal* builder = strval_new_from_slice("hello");
        Value* wrapper_fail = strval_materialize_result(builder, interp, "string helper: failed to allocate result value");
        g_string_force_result_wrapper_alloc_fail = saved_force;

        bool ok = runtime_alloc_error_matches(wrapper_fail, "string helper: failed to allocate result value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string helper wrapper alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string helper wrapper alloc failures fail closed (ok=%s msg=%s)",
                runtime_alloc_error_matches(wrapper_fail, "string helper: failed to allocate result value") ? "yes" : "no",
                runtime_alloc_error_text(wrapper_fail));
            (*fail)++;
        }
    }

    {
        Value*[3] replace_args = {
            make_string(interp, "aaaa"),
            make_string(interp, "a"),
            make_string(interp, "0123456789")
        };
        Value*[2] repeat_args = {
            make_string(interp, "ab"),
            make_int(interp, 3)
        };
        Value*[2] format_args = {
            make_string(interp, "%s"),
            make_string(interp, "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
        };
        Value*[2] cl_format_args = {
            make_string(interp, "~a"),
            make_string(interp, "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
        };

        int saved_alloc_nth = g_string_force_builder_alloc_fail_nth;
        int saved_grow_nth = g_string_force_builder_grow_fail_nth;

        g_string_force_builder_alloc_fail_nth = 1;
        Value* replace_alloc_fail = prim_string_replace(replace_args[0:3], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* repeat_alloc_fail = prim_string_repeat(repeat_args[0:2], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* format_alloc_fail = prim_format(format_args[0:2], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* cl_format_alloc_fail = prim_cl_format(cl_format_args[0:2], null, interp);

        g_string_force_builder_alloc_fail_nth = saved_alloc_nth;
        g_string_force_builder_grow_fail_nth = 1;
        Value* replace_grow_fail = prim_string_replace(replace_args[0:3], null, interp);
        g_string_force_builder_grow_fail_nth = 1;
        Value* format_grow_fail = prim_format(format_args[0:2], null, interp);
        g_string_force_builder_grow_fail_nth = 1;
        Value* cl_format_grow_fail = prim_cl_format(cl_format_args[0:2], null, interp);
        g_string_force_builder_grow_fail_nth = saved_grow_nth;

        bool ok =
            runtime_alloc_error_matches(replace_alloc_fail, "string-replace: failed to allocate builder") &&
            runtime_alloc_error_matches(repeat_alloc_fail, "string-repeat: failed to allocate builder") &&
            runtime_alloc_error_matches(format_alloc_fail, "format: failed to allocate builder") &&
            runtime_alloc_error_matches(cl_format_alloc_fail, "cl-format: failed to allocate builder") &&
            runtime_alloc_error_matches(replace_grow_fail, "string-replace: failed to grow builder") &&
            runtime_alloc_error_matches(format_grow_fail, "format: failed to grow temporary builder") &&
            runtime_alloc_error_matches(cl_format_grow_fail, "cl-format: failed to grow result buffer");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string builder alloc and growth failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string builder alloc and growth failures fail closed (replace_alloc=%s repeat_alloc=%s format_alloc=%s cl_alloc=%s replace_grow=%s format_grow=%s cl_grow=%s replace_alloc_msg=%s repeat_alloc_msg=%s format_alloc_msg=%s cl_alloc_msg=%s replace_grow_msg=%s format_grow_msg=%s cl_grow_msg=%s)",
                runtime_alloc_error_matches(replace_alloc_fail, "string-replace: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(repeat_alloc_fail, "string-repeat: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(format_alloc_fail, "format: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(cl_format_alloc_fail, "cl-format: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(replace_grow_fail, "string-replace: failed to grow builder") ? "yes" : "no",
                runtime_alloc_error_matches(format_grow_fail, "format: failed to grow temporary builder") ? "yes" : "no",
                runtime_alloc_error_matches(cl_format_grow_fail, "cl-format: failed to grow result buffer") ? "yes" : "no",
                runtime_alloc_error_text(replace_alloc_fail),
                runtime_alloc_error_text(repeat_alloc_fail),
                runtime_alloc_error_text(format_alloc_fail),
                runtime_alloc_error_text(cl_format_alloc_fail),
                runtime_alloc_error_text(replace_grow_fail),
                runtime_alloc_error_text(format_grow_fail),
                runtime_alloc_error_text(cl_format_grow_fail));
            (*fail)++;
        }
    }

    {
        Value*[1] upcase_args = { make_string(interp, "abc") };
        Value*[1] downcase_args = { make_string(interp, "ABC") };
        Value*[1] to_list_args = { make_string(interp, "abc") };
        Value*[2] split_args = { make_string(interp, "a,b"), make_string(interp, ",") };
        Value*[1] grapheme_args = { make_string(interp, "abc") };

        bool setup_ok =
            upcase_args[0] != null && upcase_args[0].tag == STRING &&
            downcase_args[0] != null && downcase_args[0].tag == STRING &&
            to_list_args[0] != null && to_list_args[0].tag == STRING &&
            split_args[0] != null && split_args[0].tag == STRING &&
            split_args[1] != null && split_args[1].tag == STRING &&
            grapheme_args[0] != null && grapheme_args[0].tag == STRING;

        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* upcase_fail = setup_ok ? prim_string_upcase(upcase_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* downcase_fail = setup_ok ? prim_string_downcase(downcase_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* to_list_fail = setup_ok ? prim_string_to_list(to_list_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* split_fail = setup_ok ? prim_string_split(split_args[0:2], null, interp) : raise_error(interp, "setup failed");
        Value* grapheme_fail = setup_ok ? prim_string_graphemes(grapheme_args[0:1], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_string_alloc_fail = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(upcase_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(downcase_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(to_list_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(split_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(grapheme_fail, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string list materializers fail closed on string wrapper alloc");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string list materializers fail closed on string wrapper alloc (setup_ok=%s upcase=%s downcase=%s to_list=%s split=%s grapheme=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(upcase_fail),
                runtime_alloc_error_text(downcase_fail),
                runtime_alloc_error_text(to_list_fail),
                runtime_alloc_error_text(split_fail),
                runtime_alloc_error_text(grapheme_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] shell_args = { make_string(interp, "printf ok"), make_symbol(interp, interp.sym_true) };
        Value*[1] readdir_args = { make_string(interp, "/") };
        Value*[2] schema_explain_args = { make_symbol(interp, interp.symbols.intern("int")), make_string(interp, "nope") };

        bool setup_ok =
            shell_args[0] != null && shell_args[0].tag == STRING &&
            shell_args[1] != null &&
            readdir_args[0] != null && readdir_args[0].tag == STRING &&
            schema_explain_args[0] != null &&
            schema_explain_args[1] != null && schema_explain_args[1].tag == STRING;

        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* shell_fail = setup_ok ? prim_shell(shell_args[0:2], null, interp) : raise_error(interp, "setup failed");
        Value* readdir_fail = setup_ok ? prim_fs_readdir(readdir_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* schema_explain_fail = setup_ok ? prim_schema_explain(schema_explain_args[0:2], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_string_alloc_fail = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(shell_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(readdir_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(schema_explain_fail, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string-backed payload materializers fail closed on string wrapper alloc");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string-backed payload materializers fail closed on string wrapper alloc (setup_ok=%s shell=%s readdir=%s schema=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(shell_fail),
                runtime_alloc_error_text(readdir_fail),
                runtime_alloc_error_text(schema_explain_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] shell_args = { make_string(interp, "printf ok"), make_symbol(interp, interp.sym_true) };
        bool setup_ok =
            shell_args[0] != null && shell_args[0].tag == STRING &&
            shell_args[1] != null;

        int saved_force = g_value_force_make_list2_alloc_fail_nth;
        g_value_force_make_list2_alloc_fail_nth = 1;
        Value* shell_fail = setup_ok ? prim_shell(shell_args[0:2], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_list2_alloc_fail_nth = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(shell_fail, "shell: failed to construct result list");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shell result list construction fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: shell result list construction fails closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(shell_fail));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        EvalResult raise_run = run("(handle (/ 1 0) (raise msg 42))", interp);
        g_value_force_make_string_alloc_fail = saved_force;

        char[] msg = raise_run.error.message[:256];
        bool ok = raise_run.error.has_error &&
            str_starts_with(msg, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise string materialization fails closed before handler bind");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise string materialization fails closed before handler bind (run_error=%s msg=%s)",
                raise_run.error.has_error ? "yes" : "no",
                raise_run.error.has_error ? (ZString)msg : runtime_alloc_error_text(raise_run.value));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* raise_payload = make_raise_payload_names(interp, "runtime", "runtime/error", "boom", make_int(interp, 42));
        g_hashmap_force_struct_alloc_fail = saved_force;

        bool ok = raise_payload == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise payload map helper fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise payload map helper fails closed (tag=%d msg=%s)",
                raise_payload != null ? (int)raise_payload.tag : -1,
                runtime_alloc_error_text(raise_payload));
            (*fail)++;
        }
    }

    {
        usz root_hashmaps_before = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* raise_payload = make_raise_payload_names(interp, "runtime", "runtime/error", "boom", make_int(interp, 42));
        g_value_force_make_string_alloc_fail = saved_force;
        usz root_hashmaps_after = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);

        bool ok = raise_payload == null &&
            root_hashmaps_after == root_hashmaps_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise payload mid-build failure cleans partial root dictionary");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise payload mid-build failure cleans partial root dictionary (result=%s before=%d after=%d)",
                raise_payload == null ? "null" : "value",
                (long)root_hashmaps_before,
                (long)root_hashmaps_after);
            (*fail)++;
        }
    }

    {
        bool saved_force = g_value_force_alloc_lisp_continuation_fail;
        g_value_force_alloc_lisp_continuation_fail = true;
        EvalResult continuation_run = run("(handle (+ 1 (signal ask 0)) (ask x x))", interp);
        g_value_force_alloc_lisp_continuation_fail = saved_force;

        char[] msg = continuation_run.error.message[:256];
        bool ok = continuation_run.error.has_error &&
            str_starts_with(msg, "handle: failed to allocate continuation");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: handled effect continuation allocation fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: handled effect continuation allocation fails closed (run_error=%s msg=%s)",
                continuation_run.error.has_error ? "yes" : "no",
                continuation_run.error.has_error ? (ZString)msg : runtime_alloc_error_text(continuation_run.value));
            (*fail)++;
        }
    }

    {
        EvalResult cmp_run = run("(lambda (a) (lambda (b) (error \"boom\")))", interp);
        Value* list = make_cons(interp, make_int(interp, 3), make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp))));
        Value*[2] sort_args = { cmp_run.error.has_error ? null : cmp_run.value, list };
        Value* sort_fail = !cmp_run.error.has_error && cmp_run.value != null
            ? prim_sort_by(sort_args[0:2], null, interp)
            : raise_error(interp, "setup failed");

        bool ok = !cmp_run.error.has_error &&
            cmp_run.value != null &&
            runtime_alloc_error_matches(sort_fail, "boom");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: sort-by comparator failures propagate directly");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: sort-by comparator failures propagate directly (setup_ok=%s msg=%s)",
                !cmp_run.error.has_error && cmp_run.value != null ? "yes" : "no",
                runtime_alloc_error_text(sort_fail));
            (*fail)++;
        }
    }

    run_memory_lifetime_runtime_alloc_groups_http_response_tests(interp, pass, fail);
}
