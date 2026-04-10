module lisp;

import std::io;

fn bool runtime_alloc_error_matches(Value* value, char[] expected) @inline {
    return value != null &&
        value.tag == ERROR &&
        value.str_chars != null &&
        str_starts_with(value.str_chars[:value.str_len], expected);
}

fn void run_memory_lifetime_runtime_helper_alloc_tests(Interp* interp, int* pass, int* fail) {
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
        Value*[3] replace_args = {
            make_string(interp, "hello world"),
            make_string(interp, "world"),
            make_string(interp, "there")
        };
        Value*[2] repeat_args = {
            make_string(interp, "ab"),
            make_int(interp, 3)
        };
        Value*[2] format_args = {
            make_string(interp, "%s"),
            make_string(interp, "ok")
        };
        Value*[2] cl_format_args = {
            make_string(interp, "~a"),
            make_int(interp, 42)
        };

        bool saved_force = g_string_force_result_wrapper_alloc_fail;
        g_string_force_result_wrapper_alloc_fail = true;
        Value* replace_fail = prim_string_replace(replace_args[0:3], null, interp);
        Value* repeat_fail = prim_string_repeat(repeat_args[0:2], null, interp);
        Value* format_fail = prim_format(format_args[0:2], null, interp);
        Value* cl_format_fail = prim_cl_format(cl_format_args[0:2], null, interp);
        g_string_force_result_wrapper_alloc_fail = saved_force;

        bool ok = runtime_alloc_error_matches(replace_fail, "string-replace: failed to allocate result value") &&
                  runtime_alloc_error_matches(repeat_fail, "string-repeat: failed to allocate result value") &&
                  runtime_alloc_error_matches(format_fail, "format: failed to allocate result value") &&
                  runtime_alloc_error_matches(cl_format_fail, "cl-format: failed to allocate result value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string helper wrapper alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string helper wrapper alloc failures fail closed (replace=%s repeat=%s format=%s cl_format=%s)",
                runtime_alloc_error_matches(replace_fail, "string-replace: failed to allocate result value") ? "yes" : "no",
                runtime_alloc_error_matches(repeat_fail, "string-repeat: failed to allocate result value") ? "yes" : "no",
                runtime_alloc_error_matches(format_fail, "format: failed to allocate result value") ? "yes" : "no",
                runtime_alloc_error_matches(cl_format_fail, "cl-format: failed to allocate result value") ? "yes" : "no");
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
