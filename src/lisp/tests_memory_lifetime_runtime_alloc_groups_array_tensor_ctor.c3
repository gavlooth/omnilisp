module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_array_tensor_ctor_tests(Interp* interp, int* pass, int* fail) {

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
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* direct_fail = null;
        Value* prim_array_fail = null;
        Value* list_to_array_fail = null;

        bool saved_struct_fail = g_value_force_make_array_struct_alloc_fail;
        bool saved_items_fail = g_value_force_make_array_items_alloc_fail;

        g_value_force_make_array_struct_alloc_fail = true;
        direct_fail = make_array(interp, 2);
        g_value_force_make_array_struct_alloc_fail = false;
        g_value_force_make_array_items_alloc_fail = true;

        Value*[2] prim_array_args = { make_int(interp, 1), make_int(interp, 2) };
        prim_array_fail = prim_array(prim_array_args[0:2], null, interp);
        Value*[1] list_to_array_args = { list };
        list_to_array_fail = prim_list_to_array(list_to_array_args[0:1], null, interp);

        g_value_force_make_array_items_alloc_fail = saved_items_fail;
        g_value_force_make_array_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(direct_fail, "array: out of memory") &&
            runtime_alloc_error_matches(prim_array_fail, "array: out of memory") &&
            runtime_alloc_error_matches(list_to_array_fail, "array: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: raw array constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: raw array constructor alloc failures fail closed (direct=%s prim=%s list=%s)",
                runtime_alloc_error_text(direct_fail),
                runtime_alloc_error_text(prim_array_fail),
                runtime_alloc_error_text(list_to_array_fail));
            (*fail)++;
        }
    }

    {
        usz[2] shape_dims = { 2, 3 };
        Value* tensor = make_tensor_double_uninitialized(interp, shape_dims[0:2]);
        TensorVal* clone = tensor != null && tensor.tag == TENSOR ? tensor_clone_payload(tensor.tensor_val) : null;
        Value*[1] tensor_args = { tensor };
        Value* dtype_val = tensor != null ? prim_dtype(tensor_args[0:1], null, interp) : null;
        Value* rank_val = tensor != null ? prim_rank(tensor_args[0:1], null, interp) : null;
        Value* shape_val = tensor != null ? prim_shape(tensor_args[0:1], null, interp) : null;
        Value* length_val = tensor != null ? prim_length(tensor_args[0:1], null, interp) : null;
        char[128] printed;
        usz printed_len = tensor != null ? print_value_to_buf(tensor, &interp.symbols, &printed[0], 128) : 0;

        bool ok = tensor != null &&
            tensor.tag == TENSOR &&
            tensor.tensor_val != null &&
            tensor.tensor_val.dtype == TENSOR_DTYPE_DOUBLE &&
            tensor.tensor_val.rank == 2 &&
            tensor.tensor_val.shape != null &&
            tensor.tensor_val.shape[0] == 2 &&
            tensor.tensor_val.shape[1] == 3 &&
            tensor.tensor_val.strides != null &&
            tensor.tensor_val.strides[0] == 3 &&
            tensor.tensor_val.strides[1] == 1 &&
            tensor.tensor_val.element_count == 6 &&
            tensor.tensor_val.byte_len == double.sizeof * 6 &&
            tensor.tensor_val.data != null &&
            clone != null &&
            clone != tensor.tensor_val &&
            clone.shape != tensor.tensor_val.shape &&
            clone.strides != tensor.tensor_val.strides &&
            clone.data != tensor.tensor_val.data &&
            clone.element_count == 6 &&
            is_tensor(tensor) &&
            value_type_name(tensor, interp) == interp.sym_Tensor &&
            infer_value_type(tensor, interp) == interp.tid_Tensor &&
            dtype_val != null &&
            dtype_val.tag == SYMBOL &&
            dtype_val.sym_val == interp.sym_Double &&
            rank_val != null &&
            rank_val.tag == INT &&
            rank_val.int_val == 2 &&
            shape_val != null &&
            shape_val.tag == ARRAY &&
            shape_val.array_val != null &&
            shape_val.array_val.length == 2 &&
            shape_val.array_val.items[0] != null &&
            shape_val.array_val.items[0].tag == INT &&
            shape_val.array_val.items[0].int_val == 2 &&
            shape_val.array_val.items[1] != null &&
            shape_val.array_val.items[1].tag == INT &&
            shape_val.array_val.items[1].int_val == 3 &&
            length_val != null &&
            length_val.tag == INT &&
            length_val.int_val == 6 &&
            printed_len > 0 &&
            str_contains(printed[:printed_len], "#<tensor Float64 shape [2 3]>");

        tensor_free_payload(clone);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor payload and introspection primitives");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor payload and introspection primitives (tag=%d dtype=%s rank=%s shape=%s length=%s print=%s)",
                tensor != null ? (int)tensor.tag : -1,
                dtype_val != null && dtype_val.tag == SYMBOL ? "yes" : "no",
                rank_val != null && rank_val.tag == INT ? "yes" : "no",
                shape_val != null && shape_val.tag == ARRAY ? "yes" : "no",
                length_val != null && length_val.tag == INT ? "yes" : "no",
                printed_len > 0 ? (ZString)&printed : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        usz[1] shape_dims = { 3 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_FLOAT32, shape_dims[0:1], true, "test tensor allocation failed");
        bool zero_initialized = false;
        if (tensor != null && tensor.data != null) {
            float* values = (float*)tensor.data;
            zero_initialized = values[0] == 0.0f && values[1] == 0.0f && values[2] == 0.0f;
            values[1] = 2.5f;
        }
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        bool clone_valid = clone != null &&
            clone != tensor &&
            clone.data != tensor.data &&
            clone.dtype == TENSOR_DTYPE_FLOAT32 &&
            clone.rank == 1 &&
            clone.element_count == 3 &&
            clone.byte_len == float.sizeof * 3 &&
            clone.data != null &&
            ((float*)clone.data)[1] == 2.5f;

        bool ok = tensor != null &&
            tensor.dtype == TENSOR_DTYPE_FLOAT32 &&
            tensor.rank == 1 &&
            tensor.shape != null &&
            tensor.shape[0] == 3 &&
            tensor.strides != null &&
            tensor.strides[0] == 1 &&
            tensor.element_count == 3 &&
            tensor.byte_len == float.sizeof * 3 &&
            zero_initialized &&
            clone_valid;

        tensor_free_payload(clone);
        tensor_free_payload(tensor);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor Float32 payload clone and byte length");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor Float32 payload clone and byte length (tensor=%s clone=%s zero=%s)",
                tensor != null ? "yes" : "no",
                clone_valid ? "yes" : "no",
                zero_initialized ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        g_runtime_alloc_tensor_device_finalizer_calls = 0;
        int device_marker = 123;
        usz[1] shape_dims = { 1 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
        bool cpu_storage_valid = tensor_has_valid_storage(tensor);
        if (tensor != null) {
            if (tensor.data != null) {
                mem::free(tensor.data);
                tensor.data = null;
            }
            tensor.device = TENSOR_DEVICE_CUDA;
            tensor.device_handle = (void*)&device_marker;
            tensor.device_finalizer = &runtime_alloc_tensor_device_finalizer;
        }
        bool cuda_device_metadata_valid = tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_CUDA);
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        tensor_free_payload(tensor);

        bool ok = cpu_storage_valid &&
            cuda_device_metadata_valid &&
            clone == null &&
            g_runtime_alloc_tensor_device_finalizer_calls == 1 &&
            device_marker == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor opaque device payload finalizer");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor opaque device payload finalizer (cpu_valid=%s cuda_metadata=%s clone_null=%s calls=%d marker=%d)",
                cpu_storage_valid ? "yes" : "no",
                cuda_device_metadata_valid ? "yes" : "no",
                clone == null ? "yes" : "no",
                g_runtime_alloc_tensor_device_finalizer_calls,
                device_marker);
            (*fail)++;
            tensor_free_payload(clone);
        }
    }

    {
        g_runtime_alloc_tensor_cuda_free_finalizer_calls = 0;
        bool cuda_available = omni_tensor_backend_cuda_available() != 0;
        bool ok = true;

        if (cuda_available) {
            usz[1] shape_dims = { 2 };
            double[2] host_values = { 1.25, 2.5 };
            double[2] clone_values = { 0.0, 0.0 };
            void* source_device = null;
            int alloc_status = omni_tensor_backend_cuda_alloc(double.sizeof * 2, &source_device);
            int copy_status = alloc_status == OMNI_TENSOR_CUDA_SUCCESS
                ? omni_tensor_backend_cuda_copy_to_existing_device((void*)&host_values[0], double.sizeof * 2, source_device)
                : OMNI_TENSOR_CUDA_UNAVAILABLE;

            TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
            bool source_owned_by_tensor = false;
            if (tensor != null && copy_status == OMNI_TENSOR_CUDA_SUCCESS && source_device != null) {
                if (tensor.data != null) {
                    mem::free(tensor.data);
                    tensor.data = null;
                }
                tensor.device = TENSOR_DEVICE_CUDA;
                tensor.device_handle = source_device;
                tensor.device_finalizer = &runtime_alloc_tensor_cuda_free_finalizer;
                source_owned_by_tensor = true;
            }

            TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
            int copyback_status = clone != null && clone.device_handle != null
                ? omni_tensor_backend_cuda_copy_to_host(clone.device_handle, clone.byte_len, (void*)&clone_values[0])
                : OMNI_TENSOR_CUDA_UNAVAILABLE;

            ok = alloc_status == OMNI_TENSOR_CUDA_SUCCESS &&
                copy_status == OMNI_TENSOR_CUDA_SUCCESS &&
                source_device != null &&
                tensor != null &&
                clone != null &&
                clone.device == TENSOR_DEVICE_CUDA &&
                clone.device_handle != null &&
                clone.device_handle != source_device &&
                clone.device_finalizer == &tensor_cuda_device_finalizer &&
                copyback_status == OMNI_TENSOR_CUDA_SUCCESS &&
                clone_values[0] == 1.25 &&
                clone_values[1] == 2.5;

            if (clone != null && clone.device_handle == source_device) {
                clone.device_handle = null;
                clone.device_finalizer = null;
            }

            tensor_free_payload(tensor);
            tensor_free_payload(clone);
            if (!source_owned_by_tensor && source_device != null) {
                omni_tensor_backend_cuda_free(source_device);
            }

            ok = ok && g_runtime_alloc_tensor_cuda_free_finalizer_calls == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor foreign CUDA payload clone deep-copies to Omni-owned storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor foreign CUDA payload clone deep-copies to Omni-owned storage (cuda=%s calls=%d)",
                cuda_available ? "yes" : "no",
                g_runtime_alloc_tensor_cuda_free_finalizer_calls);
            (*fail)++;
        }
    }

    {
        g_runtime_alloc_tensor_device_finalizer_calls = 0;
        int device_marker = 124;
        usz[1] shape_dims = { 1 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
        bool cpu_storage_valid = tensor_has_valid_storage(tensor);
        if (tensor != null) {
            if (tensor.data != null) {
                mem::free(tensor.data);
                tensor.data = null;
            }
            tensor.device = TENSOR_DEVICE_VULKAN;
            tensor.device_handle = (void*)&device_marker;
            tensor.device_finalizer = &runtime_alloc_tensor_device_finalizer;
        }
        bool vulkan_storage_rejected = tensor != null && !tensor_has_valid_storage(tensor);
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        tensor_free_payload(tensor);

        bool ok = cpu_storage_valid &&
            vulkan_storage_rejected &&
            clone == null &&
            g_runtime_alloc_tensor_device_finalizer_calls == 1 &&
            device_marker == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor opaque Vulkan device payload finalizer");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor opaque Vulkan device payload finalizer (cpu_valid=%s vulkan_rejected=%s clone_null=%s calls=%d marker=%d)",
                cpu_storage_valid ? "yes" : "no",
                vulkan_storage_rejected ? "yes" : "no",
                clone == null ? "yes" : "no",
                g_runtime_alloc_tensor_device_finalizer_calls,
                device_marker);
            (*fail)++;
            tensor_free_payload(clone);
        }
    }

    {
        usz[1] tensor_shape = { 4 };
        bool saved_struct_fail = g_value_force_make_tensor_struct_alloc_fail;
        bool saved_shape_fail = g_value_force_make_tensor_shape_alloc_fail;
        bool saved_strides_fail = g_value_force_make_tensor_strides_alloc_fail;
        bool saved_data_fail = g_value_force_make_tensor_data_alloc_fail;

        g_value_force_make_tensor_struct_alloc_fail = true;
        Value* struct_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_struct_alloc_fail = false;
        g_value_force_make_tensor_shape_alloc_fail = true;
        Value* shape_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_shape_alloc_fail = false;
        g_value_force_make_tensor_strides_alloc_fail = true;
        Value* strides_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_strides_alloc_fail = false;
        g_value_force_make_tensor_data_alloc_fail = true;
        Value* data_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");

        g_value_force_make_tensor_data_alloc_fail = saved_data_fail;
        g_value_force_make_tensor_strides_alloc_fail = saved_strides_fail;
        g_value_force_make_tensor_shape_alloc_fail = saved_shape_fail;
        g_value_force_make_tensor_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(struct_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(shape_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(strides_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(data_fail, "Tensor: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor constructor alloc failures fail closed (struct=%s shape=%s strides=%s data=%s)",
                runtime_alloc_error_text(struct_fail),
                runtime_alloc_error_text(shape_fail),
                runtime_alloc_error_text(strides_fail),
                runtime_alloc_error_text(data_fail));
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) 3)", interp);
        EvalResult index_eval = run("[1]", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !index_eval.error.has_error &&
            index_eval.value != null;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* ref_result = setup_ok ? tensor_ref_value(expr_eval.value, index_eval.value, interp) : null;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            ref_result != null &&
            ref_result.tag == DOUBLE &&
            ref_result.double_val == 5.0 &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor ref cleans temporary realized tensor");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor ref cleans temporary realized tensor (setup=%s result=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                ref_result != null && ref_result.tag == DOUBLE ? (ZString)"double" : runtime_alloc_error_text(ref_result),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (map + (Tensor Float64 [2] [1 2]) 1) 2)", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 4.0 && data[1] == 5.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run(
            "(contract (contract (Tensor Float64 [2 2] [1 2 3 4]) (Tensor Float64 [2 2] [1 0 0 1]) [1] [0]) (Tensor Float64 [2] [10 20]) [1] [0])",
            interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 50.0 && data[1] == 110.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor contract realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor contract realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* copied = null;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(saved);
        usz active_tensor_dtors_after = active_tensor_dtors_before;

        int saved_force = g_tensor_force_expr_edge_copy_fail_nth;
        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            g_tensor_force_expr_edge_copy_fail_nth = 3;
            copied = setup_ok ? copy_to_parent_with_fault(expr_eval.value, interp, &copy_fault) : null;
            g_tensor_force_expr_edge_copy_fail_nth = saved_force;
            interp.releasing_scope = null;
            active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(saved);
        }
        g_tensor_force_expr_edge_copy_fail_nth = saved_force;
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor edge copy rollback cleans partial child copies");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor edge copy rollback cleans partial child copies (setup=%s copied=%s fault=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                copied == null ? (ZString)"null" : runtime_alloc_error_text(copied),
                (ZString)boundary_copy_fault_name(copy_fault),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        bool map_left_in_child = true;
        bool map_right_in_child = true;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;
            promoted = setup_ok ? promote_to_root(expr_eval.value, interp) : null;
            if (promoted != null &&
                promoted.tag == TENSOR &&
                promoted.tensor_val != null &&
                promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP) {
                map_left_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_left, child);
                map_right_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_right, child);
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
            !map_left_in_child &&
            !map_right_in_child;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor root promotion keeps expression edges out of child scope");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor root promotion keeps expression edges out of child scope (setup=%s promoted=%s left_child=%s right_child=%s)",
                setup_ok ? "yes" : "no",
                promoted != null && promoted.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(promoted),
                map_left_in_child ? "yes" : "no",
                map_right_in_child ? "yes" : "no");
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }
}
