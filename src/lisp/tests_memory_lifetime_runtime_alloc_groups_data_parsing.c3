module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_data_parsing_tests(Interp* interp, int* pass, int* fail) {
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
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        main::ScopeRegion* saved = null;
        main::ScopeRegion* foreign = null;
        Value* toml_fail = null;
        if (setup_ok) {
            tmp.init();
            register_primitives(tmp);
            saved = tmp.current_scope;
            foreign = main::scope_create(saved);
            setup_ok = foreign != null;
        }

        if (setup_ok) {
            char[] toml_src = "a = [1979-05-27]\n";
            char[256] toml_err = {};
            OmniTomlParse* toml_handle =
                omni_toml_parse_with_options(
                    toml_src.ptr,
                    toml_src.len,
                    &toml_err,
                    toml_err.len,
                    false
                );
            bool saved_force = g_boundary_force_copy_time_point_wrapper_alloc_fail;
            g_boundary_force_copy_time_point_wrapper_alloc_fail = true;
            tmp.current_scope = foreign;
            if (toml_handle != null) {
                toml_fail = toml_datum_to_omni(omni_toml_result_root(toml_handle), tmp);
            }
            tmp.current_scope = saved;
            g_boundary_force_copy_time_point_wrapper_alloc_fail = saved_force;
            if (toml_handle != null) omni_toml_result_free(toml_handle);
            if (toml_fail == null) toml_fail = runtime_raise(tmp, "runtime/evaluation-error", "toml setup failed");
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(toml_fail, "boundary: failed to allocate time-point copy value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: toml array element promotion faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: toml array element promotion faults fail closed (setup_ok=%s toml=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(toml_fail));
            (*fail)++;
        }

        if (tmp != null) {
            if (foreign != null) main::scope_release(foreign);
            tmp.destroy();
            mem::free(tmp);
        }
    }
}
