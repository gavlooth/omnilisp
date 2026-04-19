module lisp;

import std::core::mem;
import main;

fn Value* tensor_map_apply_unary(Value* func, double a, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg_list = make_cons_or_error(interp, make_double(interp, a), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!tensor_value_to_double(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary(Value* func, double a, double b, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_double(interp, b), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_double(interp, a), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!tensor_value_to_double(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_complex128(Value* func, Complex128Val a, Complex128Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg = make_complex128(interp, a.real, a.imag);
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex128(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_complex128(Value* func, Complex128Val a, Complex128Val b, Complex128Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_complex128(interp, b.real, b.imag), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_complex128(interp, a.real, a.imag), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex128(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_complex64(Value* func, Complex64Val a, Complex64Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg = make_complex64(interp, a.real, a.imag);
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex64(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_complex64(Value* func, Complex64Val a, Complex64Val b, Complex64Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_complex64(interp, b.real, b.imag), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_complex64(interp, a.real, a.imag), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex64(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_big_integer(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigInteger tensor input is missing");
    Value* arg = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_integer_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_integer(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigInteger tensor input is missing");
    Value* right = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_integer_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_big_float(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigFloat tensor input is missing");
    Value* arg = make_big_float_from_owned_handle(interp, omni_big_float_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_float_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_float(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigFloat tensor input is missing");
    Value* right = make_big_float_from_owned_handle(interp, omni_big_float_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_float_from_owned_handle(interp, omni_big_float_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_float_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_big_complex(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigComplex tensor input is missing");
    Value* arg = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_complex_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_complex(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigComplex tensor input is missing");
    Value* right = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_complex_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}
