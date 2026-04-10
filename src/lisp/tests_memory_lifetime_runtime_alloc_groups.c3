module lisp;

import std::io;

fn bool runtime_alloc_error_matches(Value* value, char[] expected) @inline {
    return value != null &&
        value.tag == ERROR &&
        value.str_chars != null &&
        str_starts_with(value.str_chars[:value.str_len], expected);
}

fn ZString runtime_alloc_error_text(Value* value) @inline {
    return value != null && value.tag == ERROR && value.str_chars != null
        ? (ZString)value.str_chars[:value.str_len]
        : (ZString)"<none>";
}

fn void run_memory_lifetime_runtime_helper_alloc_tests(Interp* interp, int* pass, int* fail) {
    {
        int saved_force = g_value_force_make_error_chars_alloc_fail_nth;
        g_value_force_make_error_chars_alloc_fail_nth = 1;
        Value* err = make_error(interp, "constructor test message");
        g_value_force_make_error_chars_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(err, "out of memory while allocating error message");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_error alloc failure keeps printable fallback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_error alloc failure keeps printable fallback (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(err));
            (*fail)++;
        }
    }

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* iter_raise = iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-map: expected iterator state");
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = runtime_alloc_error_matches(iter_raise, "__iterator-map: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator ctor raise survives payload hashmap alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator ctor raise survives payload hashmap alloc failure (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(iter_raise));
            (*fail)++;
        }
    }

    {
        Value* partial = interp.alloc_value();
        Value* arg = make_int(interp, 42);
        bool setup_ok = partial != null && arg != null && arg.tag == INT;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.first_arg = make_int(interp, 1);
            partial.partial_val.second_arg = null;
            partial.partial_val.remaining = 2;
        }

        bool saved_force = g_eval_force_partial_alloc_fail;
        g_eval_force_partial_alloc_fail = true;
        EvalResult result = setup_ok
            ? apply_partial(partial, arg, interp)
            : eval_error("setup failed");
        g_eval_force_partial_alloc_fail = saved_force;

        bool ok = setup_ok &&
            result.error.has_error &&
            str_starts_with(result.error.message[:256], "out of memory while allocating partial application");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply_partial alloc failure returns eval error");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply_partial alloc failure returns eval error (setup_ok=%s error=%s)",
                setup_ok ? "yes" : "no",
                result.error.has_error ? (ZString)result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* iterator = iterator_from_list_value(list, interp);
        Value*[1] to_array_args = { iterator };

        bool saved_array_items_fail = g_value_force_make_array_items_alloc_fail;
        g_value_force_make_array_items_alloc_fail = true;
        Value* array_fail = prim_to_array(to_array_args[0:1], null, interp);
        g_value_force_make_array_items_alloc_fail = saved_array_items_fail;

        bool ok = runtime_alloc_error_matches(array_fail, "to-array: failed to allocate result array");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: to-array constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: to-array constructor alloc failures fail closed (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(array_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] dict_args = { make_symbol(interp, interp.symbols.intern("k")), make_int(interp, 1) };
        Value*[2] set_args = { make_int(interp, 1), make_int(interp, 2) };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        bool saved_entries_fail = g_hashmap_force_entries_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* dict_fail = prim_dict(dict_args[0:2], null, interp);
        g_hashmap_force_struct_alloc_fail = false;
        g_hashmap_force_entries_alloc_fail = true;
        Value* set_fail = prim_set(set_args[0:2], null, interp);
        g_hashmap_force_entries_alloc_fail = saved_entries_fail;
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        Value* checked_map = make_hashmap_checked(interp, 2, "hashmap test: ctor oom");
        bool grow_ok = false;
        if (checked_map != null && checked_map.tag == HASHMAP) {
            bool saved_grow_fail = g_hashmap_force_grow_entries_alloc_fail;
            grow_ok =
                hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 1), make_int(interp, 1), interp) &&
                hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 2), make_int(interp, 2), interp);
            g_hashmap_force_grow_entries_alloc_fail = true;
            bool grow_fail = hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 3), make_int(interp, 3), interp);
            g_hashmap_force_grow_entries_alloc_fail = saved_grow_fail;
            grow_ok = grow_ok && !grow_fail;
        }

        bool ok =
            runtime_alloc_error_matches(dict_fail, "Dictionary: out of memory") &&
            runtime_alloc_error_matches(set_fail, "Set: out of memory") &&
            grow_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked collection constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked collection constructors fail closed (dict_ok=%s set_ok=%s grow_ok=%s dict_msg=%s set_msg=%s)",
                runtime_alloc_error_matches(dict_fail, "Dictionary: out of memory") ? "yes" : "no",
                runtime_alloc_error_matches(set_fail, "Set: out of memory") ? "yes" : "no",
                grow_ok ? "yes" : "no",
                runtime_alloc_error_text(dict_fail),
                runtime_alloc_error_text(set_fail));
            (*fail)++;
        }
    }

    {
        Value*[1] json_obj_args = { make_string(interp, "{\"a\":1}") };
        Value*[1] json_nested_args = { make_string(interp, "{\"a\":[1]}") };
        Value*[1] toml_table_args = { make_string(interp, "a = 1\n") };
        Value*[1] toml_nested_args = { make_string(interp, "a = [1]\n") };
        Value*[1] csv_args = { make_string(interp, "") };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        bool saved_array_items_fail = g_value_force_make_array_items_alloc_fail;

        g_hashmap_force_struct_alloc_fail = true;
        Value* json_obj_fail = prim_json_parse(json_obj_args[0:1], null, interp);
        Value* toml_table_fail = prim_toml_parse(toml_table_args[0:1], null, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        g_value_force_make_array_items_alloc_fail = true;
        Value* json_nested_fail = prim_json_parse(json_nested_args[0:1], null, interp);
        Value* toml_nested_fail = prim_toml_parse(toml_nested_args[0:1], null, interp);
        Value* csv_fail = prim_csv_parse(csv_args[0:1], null, interp);
        g_value_force_make_array_items_alloc_fail = saved_array_items_fail;

        bool ok =
            runtime_alloc_error_matches(json_obj_fail, "json-parse: out of memory") &&
            runtime_alloc_error_matches(json_nested_fail, "json-parse: out of memory") &&
            runtime_alloc_error_matches(toml_table_fail, "toml-parse: out of memory") &&
            runtime_alloc_error_matches(toml_nested_fail, "toml-parse: out of memory") &&
            runtime_alloc_error_matches(csv_fail, "csv-parse: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: data-format constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: data-format constructor alloc failures fail closed (json_obj=%s json_nested=%s toml_table=%s toml_nested=%s csv=%s)",
                runtime_alloc_error_text(json_obj_fail),
                runtime_alloc_error_text(json_nested_fail),
                runtime_alloc_error_text(toml_table_fail),
                runtime_alloc_error_text(toml_nested_fail),
                runtime_alloc_error_text(csv_fail));
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
        bool first_ok = false;
        bool second_ok = false;
        bool third_ok = false;

        int saved_nth = g_http_force_parse_response_key_alloc_fail_nth;

        g_http_force_parse_response_key_alloc_fail_nth = 1;
        Value* first_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        first_ok = runtime_alloc_error_matches(first_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = 2;
        Value* second_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        second_ok = runtime_alloc_error_matches(second_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = 3;
        Value* third_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        third_ok = runtime_alloc_error_matches(third_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = saved_nth;

        bool ok = first_ok && second_ok && third_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: http response key alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: http response key alloc failures fail closed (first=%s second=%s third=%s)",
                first_ok ? "yes" : "no",
                second_ok ? "yes" : "no",
                third_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}
