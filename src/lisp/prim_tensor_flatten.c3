module lisp;

import std::core::mem;
import main;

fn Value* tensor_to_flat_list_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "List: expected Tensor");
    TensorVal* source_tensor = tensor_value.tensor_val;
    Value* concrete_value = source_tensor != null && source_tensor.kind == TENSOR_PAYLOAD_VIEW
        ? tensor_value
        : tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (!tensor_has_valid_metadata(tensor) || tensor.device != TENSOR_DEVICE_CPU || (tensor.byte_len > 0 && tensor.data == null)) return tensor_runtime_raise(interp, "List: tensor backing storage is missing");
    Value* result = make_nil(interp);
    usz remaining = tensor.element_count;
    while (remaining > 0) {
        remaining--;
        usz storage_index = 0;
        if (!tensor_storage_offset_for_logical_index(tensor, remaining, &storage_index)) return tensor_runtime_raise(interp, "List: tensor index offset is invalid");
        Value* elem = null;
        switch (tensor.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                elem = make_double(interp, ((double*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_FLOAT32:
                elem = make_float32(interp, ((float*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_COMPLEX128:
                elem = make_complex128(interp, ((Complex128Val*)tensor.data)[storage_index].real, ((Complex128Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_COMPLEX64:
                elem = make_complex64(interp, ((Complex64Val*)tensor.data)[storage_index].real, ((Complex64Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigInteger element is missing");
                elem = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[storage_index]), "List: failed to allocate BigInteger tensor element");
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigFloat element is missing");
                elem = make_big_float_from_owned_handle(interp, omni_big_float_clone(values[storage_index]), "List: failed to allocate BigFloat tensor element");
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigComplex element is missing");
                elem = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(values[storage_index]), "List: failed to allocate BigComplex tensor element");
            }
        }
        if (elem == null || elem.tag == ERROR) return elem;
        result = make_cons_or_error(interp, elem, result, "List: failed to allocate tensor conversion result");
        if (result == null || result.tag == ERROR) return result;
    }
    return result;
}
