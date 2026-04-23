module lisp;

import std::core::mem;
import main;

fn Value* tensor_big_complex_component_value(Value* tensor_value, bool imaginary, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, imaginary ? "imag-part: expected Tensor" : "real-part: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(
        tensor_value,
        &source,
        &concrete_value,
        interp,
        imaginary ? "imag-part: tensor backing storage is missing" : "real-part: tensor backing storage is missing"
    );
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_vulkan_map_complex_to_real_value(
                source,
                imaginary ? 4 : 3,
                result_dtype,
                shape_slice,
                interp,
                imaginary ? "imag-part: failed to allocate Vulkan tensor metadata" : "real-part: failed to allocate Vulkan tensor metadata",
                imaginary ? "imag-part: failed to allocate Vulkan tensor value" : "real-part: failed to allocate Vulkan tensor value",
                imaginary ? "imag-part: Vulkan currently supports dense row-major Complex128 and Complex64 tensors" : "real-part: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                imaginary ? "imag-part: Vulkan tensor backing storage is missing" : "real-part: Vulkan tensor backing storage is missing",
                imaginary ? "imag-part: Vulkan complex component kernel failed" : "real-part: Vulkan complex component kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            imaginary ? 4 : 3,
            shape_slice,
            interp,
            imaginary ? "imag-part: failed to allocate Vulkan tensor metadata" : "real-part: failed to allocate Vulkan tensor metadata",
            imaginary ? "imag-part: failed to allocate Vulkan tensor value" : "real-part: failed to allocate Vulkan tensor value",
            imaginary ? "imag-part: Vulkan currently supports dense row-major Float64 and Float32 tensors" : "real-part: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            imaginary ? "imag-part: Vulkan tensor backing storage is missing" : "real-part: Vulkan tensor backing storage is missing",
            imaginary ? "imag-part: Vulkan unary kernel failed" : "real-part: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_cuda_map_complex_to_real_value(
                source,
                imaginary ? 4 : 3,
                result_dtype,
                shape_slice,
                interp,
                imaginary ? "imag-part: failed to allocate CUDA tensor metadata" : "real-part: failed to allocate CUDA tensor metadata",
                imaginary ? "imag-part: failed to allocate CUDA tensor value" : "real-part: failed to allocate CUDA tensor value",
                imaginary ? "imag-part: CUDA currently supports dense row-major Complex128 and Complex64 tensors" : "real-part: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                imaginary ? "imag-part: CUDA tensor backing storage is missing" : "real-part: CUDA tensor backing storage is missing",
                imaginary ? "imag-part: CUDA complex component kernel failed" : "real-part: CUDA complex component kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            imaginary ? 4 : 3,
            shape_slice,
            interp,
            imaginary ? "imag-part: failed to allocate CUDA tensor metadata" : "real-part: failed to allocate CUDA tensor metadata",
            imaginary ? "imag-part: failed to allocate CUDA tensor value" : "real-part: failed to allocate CUDA tensor value",
            imaginary ? "imag-part: CUDA currently supports dense row-major Float64 and Float32 tensors" : "real-part: CUDA currently supports dense row-major Float64 and Float32 tensors",
            imaginary ? "imag-part: CUDA tensor backing storage is missing" : "real-part: CUDA tensor backing storage is missing",
            imaginary ? "imag-part: CUDA unary kernel failed" : "real-part: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor backing storage is missing" : "real-part: tensor backing storage is missing");
    }

    if (source.dtype == TENSOR_DTYPE_COMPLEX128) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_DOUBLE, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        Complex128Val* src_values = (Complex128Val*)source.data;
        double* dst_values = (double*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i] = imaginary ? src_values[i].imag : src_values[i].real;
        }
        keep_out = true;
        return out_value;
    }
    if (source.dtype == TENSOR_DTYPE_COMPLEX64) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        Complex64Val* src_values = (Complex64Val*)source.data;
        float* dst_values = (float*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i] = imaginary ? src_values[i].imag : src_values[i].real;
        }
        keep_out = true;
        return out_value;
    }

    if (source.dtype != TENSOR_DTYPE_BIG_COMPLEX) {
        Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        if (imaginary) {
            Value* zero = make_int(interp, 0);
            Value* err = tensor_fill_from_data_arg(out_value.tensor_val, zero, interp);
            if (err != null) return err;
        } else if (!tensor_copy_concrete_storage(out_value.tensor_val, source)) {
            return tensor_runtime_raise(interp, "real-part: failed to copy tensor storage");
        }
        keep_out = true;
        return out_value;
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_FLOAT, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
    }

    void** src_values = (void**)source.data;
    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        if (src_values[i] == null) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor BigComplex element is missing" : "real-part: tensor BigComplex element is missing");
        }
        void* component = imaginary ? omni_big_complex_imag_part(src_values[i]) : omni_big_complex_real_part(src_values[i]);
        if (component == null) {
            return tensor_raise(interp, "tensor/runtime-error", imaginary ? "imag-part: failed to extract tensor component" : "real-part: failed to extract tensor component");
        }
        if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
        dst_values[i] = component;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_abs_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "abs: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "abs: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_vulkan_map_complex_to_real_value(
                source,
                0,
                result_dtype,
                shape_slice,
                interp,
                "abs: failed to allocate Vulkan tensor metadata",
                "abs: failed to allocate Vulkan tensor value",
                "abs: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                "abs: Vulkan tensor backing storage is missing",
                "abs: Vulkan complex magnitude kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            0,
            shape_slice,
            interp,
            "abs: failed to allocate Vulkan tensor metadata",
            "abs: failed to allocate Vulkan tensor value",
            "abs: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            "abs: Vulkan tensor backing storage is missing",
            "abs: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_cuda_map_complex_to_real_value(
                source,
                0,
                result_dtype,
                shape_slice,
                interp,
                "abs: failed to allocate CUDA tensor metadata",
                "abs: failed to allocate CUDA tensor value",
                "abs: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                "abs: CUDA tensor backing storage is missing",
                "abs: CUDA complex magnitude kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            0,
            shape_slice,
            interp,
            "abs: failed to allocate CUDA tensor metadata",
            "abs: failed to allocate CUDA tensor value",
            "abs: CUDA currently supports dense row-major Float64 and Float32 tensors",
            "abs: CUDA tensor backing storage is missing",
            "abs: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "abs: tensor backing storage is missing");
    TensorDType result_dtype = source.dtype == TENSOR_DTYPE_BIG_COMPLEX ? TENSOR_DTYPE_BIG_FLOAT
        : source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE
        : source.dtype == TENSOR_DTYPE_COMPLEX64 ? TENSOR_DTYPE_FLOAT32
        : source.dtype;

    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "abs: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "abs: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = tensor_c_fabs(src_values[i]);
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = (float)tensor_c_fabs((double)src_values[i]);
            }
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src_values = (Complex128Val*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = complex128_abs_value(src_values[i]);
            }
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src_values = (Complex64Val*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = complex64_abs_value(src_values[i]);
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigInteger element is missing");
                void* result = omni_big_integer_compare_i64(src_values[i], 0) < 0
                    ? omni_big_integer_neg(src_values[i])
                    : omni_big_integer_clone(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "abs: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigFloat element is missing");
                void* result = omni_big_float_abs(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "abs: failed to allocate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigComplex element is missing");
                void* result = omni_big_complex_abs(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "abs: failed to allocate BigComplex tensor magnitude");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_neg_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "-: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "-: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                source,
                1,
                shape_slice,
                interp,
                "-: failed to allocate Vulkan tensor metadata",
                "-: failed to allocate Vulkan tensor value",
                "-: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                "-: Vulkan tensor backing storage is missing",
                "-: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            1,
            shape_slice,
            interp,
            "-: failed to allocate Vulkan tensor metadata",
            "-: failed to allocate Vulkan tensor value",
            "-: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            "-: Vulkan tensor backing storage is missing",
            "-: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_cuda_map_complex_unary_value(
                source,
                1,
                shape_slice,
                interp,
                "-: failed to allocate CUDA tensor metadata",
                "-: failed to allocate CUDA tensor value",
                "-: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                "-: CUDA tensor backing storage is missing",
                "-: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            1,
            shape_slice,
            interp,
            "-: failed to allocate CUDA tensor metadata",
            "-: failed to allocate CUDA tensor value",
            "-: CUDA currently supports dense row-major Float64 and Float32 tensors",
            "-: CUDA tensor backing storage is missing",
            "-: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "-: tensor backing storage is missing");
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "-: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "-: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = -src_values[i];
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = -src_values[i];
            }
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src_values = (Complex128Val*)source.data;
            Complex128Val* dst_values = (Complex128Val*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i].real = -src_values[i].real;
                dst_values[i].imag = -src_values[i].imag;
            }
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src_values = (Complex64Val*)source.data;
            Complex64Val* dst_values = (Complex64Val*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i].real = -src_values[i].real;
                dst_values[i].imag = -src_values[i].imag;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigInteger element is missing");
                void* result = omni_big_integer_neg(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "-: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigFloat element is missing");
                void* result = omni_big_float_neg(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "-: failed to allocate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigComplex element is missing");
                void* result = omni_big_complex_neg(src_values[i]);
                if (result == null) return tensor_raise(interp, "tensor/out-of-memory", "-: failed to allocate BigComplex tensor element");
                if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
    }

    keep_out = true;
    return out_value;
}

fn double tensor_apply_double_unary_math(double input, int big_float_op) {
    switch (big_float_op) {
        case BIG_FLOAT_MATH_SIN:
            return tensor_c_sin(input);
        case BIG_FLOAT_MATH_COS:
            return tensor_c_cos(input);
        case BIG_FLOAT_MATH_TAN:
            return tensor_c_tan(input);
        case BIG_FLOAT_MATH_ASIN:
            return tensor_c_asin(input);
        case BIG_FLOAT_MATH_ACOS:
            return tensor_c_acos(input);
        case BIG_FLOAT_MATH_ATAN:
            return tensor_c_atan(input);
        case BIG_FLOAT_MATH_SINH:
            return tensor_c_sinh(input);
        case BIG_FLOAT_MATH_COSH:
            return tensor_c_cosh(input);
        case BIG_FLOAT_MATH_TANH:
            return tensor_c_tanh(input);
        case BIG_FLOAT_MATH_EXP:
            return tensor_c_exp(input);
        case BIG_FLOAT_MATH_LOG:
        return tensor_c_log(input);
        case BIG_FLOAT_MATH_LOG10:
            return tensor_c_log10(input);
        case BIG_FLOAT_MATH_ERF:
            return tensor_c_erf(input);
        case BIG_FLOAT_MATH_ERFC:
            return tensor_c_erfc(input);
        case BIG_FLOAT_MATH_NORMAL_CDF:
            return 0.5 * tensor_c_erfc(-input * TENSOR_INV_SQRT_2);
        case BIG_FLOAT_MATH_SQRT:
            return tensor_c_sqrt(input);
        default:
            return input;
    }
}

fn Value* tensor_apply_double_unary_math_checked(double input, int big_float_op, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "math: tensor result storage is missing");
    if (big_float_op == BIG_FLOAT_MATH_NORMAL_QUANTILE) {
        double output = 0.0;
        int status = omni_boost_math_standard_normal_quantile(input, &output);
        switch (status) {
            case BOOST_MATH_STATUS_OK:
                *out = output;
                return null;
            case BOOST_MATH_STATUS_INVALID_ARGUMENT:
                return tensor_raise(interp, "tensor/invalid-argument", "stats/normal-quantile: expected finite numeric input");
            case BOOST_MATH_STATUS_DOMAIN_ERROR:
                return tensor_raise(interp, "tensor/invalid-argument", "stats/normal-quantile: probability must be between 0 and 1");
            case BOOST_MATH_STATUS_RANGE_ERROR:
                return tensor_raise(interp, "tensor/range-error", "stats/normal-quantile: range error");
            default:
                return tensor_raise(interp, "tensor/invalid-argument", "stats/normal-quantile: evaluation error");
        }
    }
    *out = tensor_apply_double_unary_math(input, big_float_op);
    return null;
}

fn Value* tensor_unary_math_value(Value* tensor_value, int big_float_op, int big_complex_op, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "math: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "math: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        uint vulkan_op = 0;
        if (!tensor_unary_math_vulkan_op(big_float_op, &vulkan_op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "math: Vulkan currently supports dense row-major Float64 and Float32 unary operations", null);
        }
        return tensor_vulkan_map_unary_value(
            source,
            vulkan_op,
            shape_slice,
            interp,
            "math: failed to allocate Vulkan tensor metadata",
            "math: failed to allocate Vulkan tensor value",
            "math: Vulkan currently supports dense row-major Float64 and Float32 unary operations",
            "math: Vulkan tensor backing storage is missing",
            "math: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        uint cuda_op = 0;
        if (!tensor_unary_math_cuda_dispatch_op(big_float_op, &cuda_op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "math: CUDA currently supports dense row-major Float64 and Float32 arithmetic and scientific unary operations", null);
        }
        return tensor_cuda_map_unary_value(
            source,
            cuda_op,
            shape_slice,
            interp,
            "math: failed to allocate CUDA tensor metadata",
            "math: failed to allocate CUDA tensor value",
            "math: CUDA currently supports dense row-major Float64 and Float32 arithmetic and scientific unary operations",
            "math: CUDA tensor backing storage is missing",
            "math: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "math: tensor backing storage is missing");
    TensorDType result_dtype = source.dtype == TENSOR_DTYPE_BIG_INTEGER ? TENSOR_DTYPE_DOUBLE : source.dtype;

    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "math: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "math: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked(src_values[i], big_float_op, &result, interp);
                if (err != null) return err;
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked((double)src_values[i], big_float_op, &result, interp);
                if (err != null) return err;
                float narrowed = 0.0f;
                if (!tensor_double_to_float32(result, &narrowed)) {
                    return tensor_raise(interp, "tensor/invalid-argument", "math: tensor Float32 result out of supported range");
                }
                dst_values[i] = narrowed;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigInteger element is missing");
                double input = 0.0;
                if (omni_big_integer_to_double(src_values[i], &input) == 0) {
                    return tensor_raise(interp, "tensor/invalid-argument", "math: BigInteger tensor element out of Float64 range");
                }
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked(input, big_float_op, &result, interp);
                if (err != null) return err;
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigFloat element is missing");
                void* result = omni_big_float_unary_math(src_values[i], big_float_op);
                if (result == null && big_float_op == BIG_FLOAT_MATH_NORMAL_QUANTILE) {
                    return tensor_raise(interp, "tensor/invalid-argument", "stats/normal-quantile: probability must be between 0 and 1");
                }
                if (result == null) return tensor_raise(interp, "tensor/invalid-argument", "math: failed to evaluate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            if (!tensor_unary_math_has_complex_op(big_complex_op)) {
                return tensor_raise(interp, "tensor/invalid-argument", "math: tensor dtype is not supported for this operation");
            }
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigComplex element is missing");
                void* result = omni_big_complex_unary_math(src_values[i], big_complex_op);
                if (result == null) return tensor_raise(interp, "tensor/invalid-argument", "math: failed to evaluate BigComplex tensor element");
                if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_COMPLEX128:
        case TENSOR_DTYPE_COMPLEX64:
            return tensor_raise(interp, "tensor/invalid-argument", "math: fixed-width complex Tensor scientific operations are not supported");
    }

    keep_out = true;
    return out_value;
}
