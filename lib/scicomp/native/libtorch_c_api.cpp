/*
 * libtorch_c_api.cpp - C wrapper implementation for LibTorch
 *
 * Implements the C API defined in libtorch_c_api.h by wrapping
 * the LibTorch C++ API (ATen).
 *
 * Build: g++ -std=c++17 -shared -fPIC -o libomnitorch.so libtorch_c_api.cpp \
 *        -I$LIBTORCH/include -I$LIBTORCH/include/torch/csrc/api/include \
 *        -L$LIBTORCH/lib -ltorch -ltorch_cpu -lc10
 *
 * Copyright (c) 2026 OmniLisp Project
 * SPDX-License-Identifier: MIT
 */

#include "libtorch_c_api.h"
#include <torch/torch.h>
#include <ATen/ATen.h>
#include <c10/core/ScalarType.h>
#include <c10/core/Device.h>
#include <exception>
#include <cstring>
#include <mutex>
#include <sstream>

/*
 * ===========================================
 * Internal Helpers
 * ===========================================
 */

/* Thread-local error state */
static thread_local TorchError g_last_error = {0, ""};

/* Internal: Set error message */
static void set_error(int code, const char* msg) {
    g_last_error.code = code;
    strncpy(g_last_error.message, msg, TORCH_ERROR_MSG_SIZE - 1);
    g_last_error.message[TORCH_ERROR_MSG_SIZE - 1] = '\0';
}

/* Internal: Set error from exception */
static void set_error_from_exception(const std::exception& e) {
    set_error(1, e.what());
}

/* Internal: Clear error state */
static void clear_error() {
    g_last_error.code = 0;
    g_last_error.message[0] = '\0';
}

/* Internal: Convert TorchDType to at::ScalarType */
static at::ScalarType to_scalar_type(TorchDType dtype) {
    switch (dtype) {
        case TORCH_FLOAT16:    return at::kHalf;
        case TORCH_FLOAT32:    return at::kFloat;
        case TORCH_FLOAT64:    return at::kDouble;
        case TORCH_BFLOAT16:   return at::kBFloat16;
        case TORCH_INT8:       return at::kChar;
        case TORCH_INT16:      return at::kShort;
        case TORCH_INT32:      return at::kInt;
        case TORCH_INT64:      return at::kLong;
        case TORCH_UINT8:      return at::kByte;
        case TORCH_BOOL:       return at::kBool;
        case TORCH_COMPLEX64:  return at::kComplexFloat;
        case TORCH_COMPLEX128: return at::kComplexDouble;
        default:               return at::kFloat;
    }
}

/* Internal: Convert at::ScalarType to TorchDType */
static TorchDType from_scalar_type(at::ScalarType dtype) {
    switch (dtype) {
        case at::kHalf:          return TORCH_FLOAT16;
        case at::kFloat:         return TORCH_FLOAT32;
        case at::kDouble:        return TORCH_FLOAT64;
        case at::kBFloat16:      return TORCH_BFLOAT16;
        case at::kChar:          return TORCH_INT8;
        case at::kShort:         return TORCH_INT16;
        case at::kInt:           return TORCH_INT32;
        case at::kLong:          return TORCH_INT64;
        case at::kByte:          return TORCH_UINT8;
        case at::kBool:          return TORCH_BOOL;
        case at::kComplexFloat:  return TORCH_COMPLEX64;
        case at::kComplexDouble: return TORCH_COMPLEX128;
        default:                 return TORCH_FLOAT32;
    }
}

/* Internal: Convert TorchDeviceType to c10::DeviceType */
static c10::DeviceType to_device_type(TorchDeviceType device) {
    switch (device) {
        case TORCH_CPU:  return c10::kCPU;
        case TORCH_CUDA: return c10::kCUDA;
        case TORCH_MPS:  return c10::kMPS;
        case TORCH_XPU:  return c10::kXPU;
        default:         return c10::kCPU;
    }
}

/* Internal: Convert c10::DeviceType to TorchDeviceType */
static TorchDeviceType from_device_type(c10::DeviceType device) {
    switch (device) {
        case c10::kCPU:  return TORCH_CPU;
        case c10::kCUDA: return TORCH_CUDA;
        case c10::kMPS:  return TORCH_MPS;
        case c10::kXPU:  return TORCH_XPU;
        default:         return TORCH_CPU;
    }
}

/* Internal: Wrap at::Tensor in TensorHandle */
static TensorHandle wrap_tensor(at::Tensor&& t) {
    return reinterpret_cast<TensorHandle>(new at::Tensor(std::move(t)));
}

static TensorHandle wrap_tensor(const at::Tensor& t) {
    return reinterpret_cast<TensorHandle>(new at::Tensor(t));
}

/* Internal: Unwrap TensorHandle to at::Tensor reference */
static at::Tensor& unwrap_tensor(TensorHandle h) {
    return *reinterpret_cast<at::Tensor*>(h);
}

/* Internal: Try block macro for consistent error handling */
#define TRY_BLOCK(expr) \
    do { \
        clear_error(); \
        try { \
            return (expr); \
        } catch (const std::exception& e) { \
            set_error_from_exception(e); \
            return nullptr; \
        } \
    } while (0)

#define TRY_BLOCK_VOID(expr) \
    do { \
        clear_error(); \
        try { \
            expr; \
        } catch (const std::exception& e) { \
            set_error_from_exception(e); \
        } \
    } while (0)

/*
 * ===========================================
 * Error Handling Implementation
 * ===========================================
 */

extern "C" {

TorchError* torch_get_last_error(void) {
    return &g_last_error;
}

void torch_clear_error(void) {
    clear_error();
}

int torch_ok(void) {
    return g_last_error.code == 0 ? 1 : 0;
}

/*
 * ===========================================
 * Tensor Creation Implementation
 * ===========================================
 */

TensorHandle torch_tensor_empty(
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype,
    TorchDeviceType device
) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        auto options = at::TensorOptions()
            .dtype(to_scalar_type(dtype))
            .device(to_device_type(device));
        wrap_tensor(at::empty(dims, options));
    }));
}

TensorHandle torch_tensor_from_data(
    const void* data,
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype
) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        auto options = at::TensorOptions().dtype(to_scalar_type(dtype));

        /* Calculate total elements */
        int64_t numel = 1;
        for (size_t i = 0; i < ndim; i++) numel *= dims[i];

        /* Create tensor and copy data */
        at::Tensor t = at::empty(dims, options);
        std::memcpy(t.data_ptr(), data, numel * t.element_size());
        wrap_tensor(std::move(t));
    }));
}

TensorHandle torch_tensor_from_blob(
    void* data,
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype
) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        auto options = at::TensorOptions().dtype(to_scalar_type(dtype));
        /* from_blob does not copy data, tensor is a view */
        wrap_tensor(at::from_blob(data, dims, options));
    }));
}

TensorHandle torch_tensor_zeros(const int64_t* shape, size_t ndim, TorchDType dtype) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::zeros(dims, to_scalar_type(dtype)));
    }));
}

TensorHandle torch_tensor_ones(const int64_t* shape, size_t ndim, TorchDType dtype) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::ones(dims, to_scalar_type(dtype)));
    }));
}

TensorHandle torch_tensor_full(const int64_t* shape, size_t ndim, double value, TorchDType dtype) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::full(dims, value, to_scalar_type(dtype)));
    }));
}

TensorHandle torch_tensor_eye(int64_t n, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(at::eye(n, to_scalar_type(dtype))));
}

TensorHandle torch_tensor_eye_m_n(int64_t m, int64_t n, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(at::eye(m, n, to_scalar_type(dtype))));
}

TensorHandle torch_tensor_arange(double start, double end, double step, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(at::arange(start, end, step, to_scalar_type(dtype))));
}

TensorHandle torch_tensor_linspace(double start, double end, int64_t steps, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(at::linspace(start, end, steps, to_scalar_type(dtype))));
}

TensorHandle torch_tensor_logspace(double start, double end, int64_t steps, double base, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(at::logspace(start, end, steps, base, to_scalar_type(dtype))));
}

TensorHandle torch_tensor_rand(const int64_t* shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::rand(dims));
    }));
}

TensorHandle torch_tensor_randn(const int64_t* shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::randn(dims));
    }));
}

TensorHandle torch_tensor_randint(int64_t low, int64_t high, const int64_t* shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(at::randint(low, high, dims));
    }));
}

TensorHandle torch_tensor_rand_like(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::rand_like(unwrap_tensor(t))));
}

TensorHandle torch_tensor_randn_like(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::randn_like(unwrap_tensor(t))));
}

TensorHandle torch_tensor_clone(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).clone()));
}

void torch_tensor_copy_(TensorHandle dst, TensorHandle src) {
    TRY_BLOCK_VOID(unwrap_tensor(dst).copy_(unwrap_tensor(src)));
}

void torch_tensor_free(TensorHandle t) {
    if (t) {
        delete reinterpret_cast<at::Tensor*>(t);
    }
}

/*
 * ===========================================
 * Tensor Properties Implementation
 * ===========================================
 */

size_t torch_tensor_ndim(TensorHandle t) {
    return unwrap_tensor(t).dim();
}

size_t torch_tensor_shape(TensorHandle t, int64_t* shape_out, size_t max_dims) {
    const at::Tensor& tensor = unwrap_tensor(t);
    size_t ndim = tensor.dim();
    size_t copy_dims = std::min(ndim, max_dims);
    for (size_t i = 0; i < copy_dims; i++) {
        shape_out[i] = tensor.size(i);
    }
    return ndim;
}

int64_t torch_tensor_size(TensorHandle t, int64_t dim) {
    return unwrap_tensor(t).size(dim);
}

int64_t torch_tensor_numel(TensorHandle t) {
    return unwrap_tensor(t).numel();
}

TorchDType torch_tensor_dtype(TensorHandle t) {
    return from_scalar_type(unwrap_tensor(t).scalar_type());
}

TorchDeviceType torch_tensor_device(TensorHandle t) {
    return from_device_type(unwrap_tensor(t).device().type());
}

int torch_tensor_device_index(TensorHandle t) {
    return unwrap_tensor(t).device().index();
}

int torch_tensor_requires_grad(TensorHandle t) {
    return unwrap_tensor(t).requires_grad() ? 1 : 0;
}

int torch_tensor_is_contiguous(TensorHandle t) {
    return unwrap_tensor(t).is_contiguous() ? 1 : 0;
}

size_t torch_tensor_strides(TensorHandle t, int64_t* strides_out, size_t max_dims) {
    const at::Tensor& tensor = unwrap_tensor(t);
    size_t ndim = tensor.dim();
    size_t copy_dims = std::min(ndim, max_dims);
    for (size_t i = 0; i < copy_dims; i++) {
        strides_out[i] = tensor.stride(i);
    }
    return ndim;
}

size_t torch_tensor_element_size(TensorHandle t) {
    return unwrap_tensor(t).element_size();
}

size_t torch_tensor_nbytes(TensorHandle t) {
    return unwrap_tensor(t).nbytes();
}

/*
 * ===========================================
 * Data Access Implementation
 * ===========================================
 */

void* torch_tensor_data_ptr(TensorHandle t) {
    return unwrap_tensor(t).data_ptr();
}

int64_t torch_tensor_copy_data(TensorHandle t, void* out, size_t out_bytes) {
    clear_error();
    try {
        const at::Tensor& tensor = unwrap_tensor(t);
        at::Tensor contiguous = tensor.contiguous();
        size_t nbytes = contiguous.nbytes();
        if (out_bytes < nbytes) {
            set_error(2, "Output buffer too small");
            return -1;
        }
        std::memcpy(out, contiguous.data_ptr(), nbytes);
        return static_cast<int64_t>(nbytes);
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return -1;
    }
}

double torch_tensor_item(TensorHandle t) {
    clear_error();
    try {
        return unwrap_tensor(t).item<double>();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0.0;
    }
}

int64_t torch_tensor_item_int(TensorHandle t) {
    clear_error();
    try {
        return unwrap_tensor(t).item<int64_t>();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

double torch_tensor_get_1d(TensorHandle t, int64_t i) {
    clear_error();
    try {
        return unwrap_tensor(t)[i].item<double>();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0.0;
    }
}

double torch_tensor_get_2d(TensorHandle t, int64_t i, int64_t j) {
    clear_error();
    try {
        return unwrap_tensor(t)[i][j].item<double>();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0.0;
    }
}

void torch_tensor_set_1d(TensorHandle t, int64_t i, double value) {
    TRY_BLOCK_VOID(unwrap_tensor(t)[i] = value);
}

void torch_tensor_set_2d(TensorHandle t, int64_t i, int64_t j, double value) {
    TRY_BLOCK_VOID(unwrap_tensor(t)[i][j] = value);
}

/*
 * ===========================================
 * Arithmetic Operations Implementation
 * ===========================================
 */

TensorHandle torch_add(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::add(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_sub(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::sub(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_mul(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::mul(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_div(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::div(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_fmod(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::fmod(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_remainder(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::remainder(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_pow(TensorHandle base, TensorHandle exp) {
    TRY_BLOCK(wrap_tensor(at::pow(unwrap_tensor(base), unwrap_tensor(exp))));
}

TensorHandle torch_add_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::add(unwrap_tensor(t), scalar)));
}

TensorHandle torch_sub_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::sub(unwrap_tensor(t), scalar)));
}

TensorHandle torch_mul_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::mul(unwrap_tensor(t), scalar)));
}

TensorHandle torch_div_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::div(unwrap_tensor(t), scalar)));
}

TensorHandle torch_pow_scalar(TensorHandle t, double exp) {
    TRY_BLOCK(wrap_tensor(at::pow(unwrap_tensor(t), exp)));
}

TensorHandle torch_scalar_sub(double scalar, TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sub(scalar, unwrap_tensor(t))));
}

TensorHandle torch_scalar_div(double scalar, TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::div(scalar, unwrap_tensor(t))));
}

TensorHandle torch_matmul(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::matmul(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_mm(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::mm(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_bmm(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::bmm(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_mv(TensorHandle mat, TensorHandle vec) {
    TRY_BLOCK(wrap_tensor(at::mv(unwrap_tensor(mat), unwrap_tensor(vec))));
}

TensorHandle torch_dot(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::dot(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_outer(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::outer(unwrap_tensor(a), unwrap_tensor(b))));
}

/*
 * ===========================================
 * Reduction Operations Implementation
 * ===========================================
 */

TensorHandle torch_sum(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sum(unwrap_tensor(t))));
}

TensorHandle torch_prod(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::prod(unwrap_tensor(t))));
}

TensorHandle torch_mean(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::mean(unwrap_tensor(t))));
}

TensorHandle torch_std(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::std(unwrap_tensor(t))));
}

TensorHandle torch_var(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::var(unwrap_tensor(t))));
}

TensorHandle torch_max(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::max(unwrap_tensor(t))));
}

TensorHandle torch_min(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::min(unwrap_tensor(t))));
}

TensorHandle torch_argmax(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::argmax(unwrap_tensor(t))));
}

TensorHandle torch_argmin(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::argmin(unwrap_tensor(t))));
}

TensorHandle torch_norm(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::norm(unwrap_tensor(t))));
}

TensorHandle torch_sum_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::sum(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_prod_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::prod(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_mean_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::mean(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_std_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::std(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_var_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::var(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_cumsum(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::cumsum(unwrap_tensor(t), dim)));
}

TensorHandle torch_cumprod(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::cumprod(unwrap_tensor(t), dim)));
}

int torch_all(TensorHandle t) {
    clear_error();
    try {
        return at::all(unwrap_tensor(t)).item<bool>() ? 1 : 0;
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

int torch_any(TensorHandle t) {
    clear_error();
    try {
        return at::any(unwrap_tensor(t)).item<bool>() ? 1 : 0;
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

TensorHandle torch_max_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(std::get<0>(at::max(unwrap_tensor(t), dim, keepdim != 0))));
}

TensorHandle torch_min_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(std::get<0>(at::min(unwrap_tensor(t), dim, keepdim != 0))));
}

TensorHandle torch_argmax_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::argmax(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_argmin_dim(TensorHandle t, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::argmin(unwrap_tensor(t), dim, keepdim != 0)));
}

TensorHandle torch_norm_dim(TensorHandle t, double p, int64_t dim, int keepdim) {
    TRY_BLOCK(wrap_tensor(at::norm(unwrap_tensor(t), p, dim, keepdim != 0)));
}

/*
 * ===========================================
 * Unary Operations Implementation
 * ===========================================
 */

TensorHandle torch_neg(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::neg(unwrap_tensor(t))));
}

TensorHandle torch_abs(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::abs(unwrap_tensor(t))));
}

TensorHandle torch_sign(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sign(unwrap_tensor(t))));
}

TensorHandle torch_ceil(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::ceil(unwrap_tensor(t))));
}

TensorHandle torch_floor(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::floor(unwrap_tensor(t))));
}

TensorHandle torch_round(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::round(unwrap_tensor(t))));
}

TensorHandle torch_trunc(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::trunc(unwrap_tensor(t))));
}

TensorHandle torch_frac(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::frac(unwrap_tensor(t))));
}

TensorHandle torch_reciprocal(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::reciprocal(unwrap_tensor(t))));
}

TensorHandle torch_clamp(TensorHandle t, double min_val, double max_val) {
    TRY_BLOCK(wrap_tensor(at::clamp(unwrap_tensor(t), min_val, max_val)));
}

TensorHandle torch_sqrt(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sqrt(unwrap_tensor(t))));
}

TensorHandle torch_rsqrt(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::rsqrt(unwrap_tensor(t))));
}

TensorHandle torch_square(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::square(unwrap_tensor(t))));
}

TensorHandle torch_exp(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::exp(unwrap_tensor(t))));
}

TensorHandle torch_exp2(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::exp2(unwrap_tensor(t))));
}

TensorHandle torch_expm1(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::expm1(unwrap_tensor(t))));
}

TensorHandle torch_log(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::log(unwrap_tensor(t))));
}

TensorHandle torch_log2(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::log2(unwrap_tensor(t))));
}

TensorHandle torch_log10(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::log10(unwrap_tensor(t))));
}

TensorHandle torch_log1p(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::log1p(unwrap_tensor(t))));
}

TensorHandle torch_sin(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sin(unwrap_tensor(t))));
}

TensorHandle torch_cos(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::cos(unwrap_tensor(t))));
}

TensorHandle torch_tan(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::tan(unwrap_tensor(t))));
}

TensorHandle torch_asin(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::asin(unwrap_tensor(t))));
}

TensorHandle torch_acos(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::acos(unwrap_tensor(t))));
}

TensorHandle torch_atan(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::atan(unwrap_tensor(t))));
}

TensorHandle torch_atan2(TensorHandle y, TensorHandle x) {
    TRY_BLOCK(wrap_tensor(at::atan2(unwrap_tensor(y), unwrap_tensor(x))));
}

TensorHandle torch_sinh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sinh(unwrap_tensor(t))));
}

TensorHandle torch_cosh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::cosh(unwrap_tensor(t))));
}

TensorHandle torch_tanh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::tanh(unwrap_tensor(t))));
}

TensorHandle torch_asinh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::asinh(unwrap_tensor(t))));
}

TensorHandle torch_acosh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::acosh(unwrap_tensor(t))));
}

TensorHandle torch_atanh(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::atanh(unwrap_tensor(t))));
}

TensorHandle torch_sigmoid(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::sigmoid(unwrap_tensor(t))));
}

TensorHandle torch_relu(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::relu(unwrap_tensor(t))));
}

TensorHandle torch_relu6(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::clamp(unwrap_tensor(t), 0.0, 6.0)));
}

TensorHandle torch_leaky_relu(TensorHandle t, double negative_slope) {
    TRY_BLOCK(wrap_tensor(at::leaky_relu(unwrap_tensor(t), negative_slope)));
}

TensorHandle torch_elu(TensorHandle t, double alpha) {
    TRY_BLOCK(wrap_tensor(at::elu(unwrap_tensor(t), alpha)));
}

TensorHandle torch_selu(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::selu(unwrap_tensor(t))));
}

TensorHandle torch_gelu(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::gelu(unwrap_tensor(t))));
}

TensorHandle torch_silu(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::silu(unwrap_tensor(t))));
}

TensorHandle torch_mish(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::mish(unwrap_tensor(t))));
}

TensorHandle torch_softplus(TensorHandle t, double beta, double threshold) {
    TRY_BLOCK(wrap_tensor(at::softplus(unwrap_tensor(t), beta, threshold)));
}

TensorHandle torch_softsign(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::softsign(unwrap_tensor(t))));
}

TensorHandle torch_softmax(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::softmax(unwrap_tensor(t), dim)));
}

TensorHandle torch_log_softmax(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::log_softmax(unwrap_tensor(t), dim)));
}

/*
 * ===========================================
 * Shape Manipulation Implementation
 * ===========================================
 */

TensorHandle torch_reshape(TensorHandle t, const int64_t* shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(unwrap_tensor(t).reshape(dims));
    }));
}

TensorHandle torch_view(TensorHandle t, const int64_t* shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> dims(shape, shape + ndim);
        wrap_tensor(unwrap_tensor(t).view(dims));
    }));
}

TensorHandle torch_transpose(TensorHandle t, int64_t dim0, int64_t dim1) {
    TRY_BLOCK(wrap_tensor(at::transpose(unwrap_tensor(t), dim0, dim1)));
}

TensorHandle torch_permute(TensorHandle t, const int64_t* dims, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> perm(dims, dims + ndim);
        wrap_tensor(unwrap_tensor(t).permute(perm));
    }));
}

TensorHandle torch_squeeze(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::squeeze(unwrap_tensor(t))));
}

TensorHandle torch_squeeze_dim(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::squeeze(unwrap_tensor(t), dim)));
}

TensorHandle torch_unsqueeze(TensorHandle t, int64_t dim) {
    TRY_BLOCK(wrap_tensor(at::unsqueeze(unwrap_tensor(t), dim)));
}

TensorHandle torch_flatten(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::flatten(unwrap_tensor(t))));
}

TensorHandle torch_flatten_range(TensorHandle t, int64_t start_dim, int64_t end_dim) {
    TRY_BLOCK(wrap_tensor(at::flatten(unwrap_tensor(t), start_dim, end_dim)));
}

TensorHandle torch_contiguous(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).contiguous()));
}

TensorHandle torch_t(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::t(unwrap_tensor(t))));
}

TensorHandle torch_cat(TensorHandle* tensors, size_t count, int64_t dim) {
    TRY_BLOCK(({
        std::vector<at::Tensor> vec;
        vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(unwrap_tensor(tensors[i]));
        }
        wrap_tensor(at::cat(vec, dim));
    }));
}

TensorHandle torch_stack(TensorHandle* tensors, size_t count, int64_t dim) {
    TRY_BLOCK(({
        std::vector<at::Tensor> vec;
        vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(unwrap_tensor(tensors[i]));
        }
        wrap_tensor(at::stack(vec, dim));
    }));
}

TensorHandle torch_hstack(TensorHandle* tensors, size_t count) {
    TRY_BLOCK(({
        std::vector<at::Tensor> vec;
        vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(unwrap_tensor(tensors[i]));
        }
        wrap_tensor(at::hstack(vec));
    }));
}

TensorHandle torch_vstack(TensorHandle* tensors, size_t count) {
    TRY_BLOCK(({
        std::vector<at::Tensor> vec;
        vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(unwrap_tensor(tensors[i]));
        }
        wrap_tensor(at::vstack(vec));
    }));
}

TensorHandle torch_dstack(TensorHandle* tensors, size_t count) {
    TRY_BLOCK(({
        std::vector<at::Tensor> vec;
        vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(unwrap_tensor(tensors[i]));
        }
        wrap_tensor(at::dstack(vec));
    }));
}

TensorHandle torch_expand(TensorHandle t, const int64_t* sizes, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> sz(sizes, sizes + ndim);
        wrap_tensor(unwrap_tensor(t).expand(sz));
    }));
}

TensorHandle torch_repeat(TensorHandle t, const int64_t* repeats, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> reps(repeats, repeats + ndim);
        wrap_tensor(unwrap_tensor(t).repeat(reps));
    }));
}

size_t torch_split(TensorHandle t, int64_t split_size, int64_t dim,
                   TensorHandle* out, size_t max_out) {
    clear_error();
    try {
        std::vector<at::Tensor> splits = at::split(unwrap_tensor(t), split_size, dim);
        size_t count = std::min(splits.size(), max_out);
        for (size_t i = 0; i < count; i++) {
            out[i] = wrap_tensor(std::move(splits[i]));
        }
        return splits.size();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

size_t torch_chunk(TensorHandle t, size_t chunks, int64_t dim,
                   TensorHandle* out, size_t max_out) {
    clear_error();
    try {
        std::vector<at::Tensor> chunked = at::chunk(unwrap_tensor(t), static_cast<int64_t>(chunks), dim);
        size_t count = std::min(chunked.size(), max_out);
        for (size_t i = 0; i < count; i++) {
            out[i] = wrap_tensor(std::move(chunked[i]));
        }
        return chunked.size();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

/*
 * ===========================================
 * Comparison Operations Implementation
 * ===========================================
 */

TensorHandle torch_eq(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::eq(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_ne(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::ne(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_lt(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::lt(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_le(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::le(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_gt(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::gt(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_ge(TensorHandle a, TensorHandle b) {
    TRY_BLOCK(wrap_tensor(at::ge(unwrap_tensor(a), unwrap_tensor(b))));
}

TensorHandle torch_eq_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::eq(unwrap_tensor(t), scalar)));
}

TensorHandle torch_ne_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::ne(unwrap_tensor(t), scalar)));
}

TensorHandle torch_lt_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::lt(unwrap_tensor(t), scalar)));
}

TensorHandle torch_le_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::le(unwrap_tensor(t), scalar)));
}

TensorHandle torch_gt_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::gt(unwrap_tensor(t), scalar)));
}

TensorHandle torch_ge_scalar(TensorHandle t, double scalar) {
    TRY_BLOCK(wrap_tensor(at::ge(unwrap_tensor(t), scalar)));
}

TensorHandle torch_isnan(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::isnan(unwrap_tensor(t))));
}

TensorHandle torch_isinf(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::isinf(unwrap_tensor(t))));
}

TensorHandle torch_isfinite(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::isfinite(unwrap_tensor(t))));
}

TensorHandle torch_where(TensorHandle condition, TensorHandle x, TensorHandle y) {
    TRY_BLOCK(wrap_tensor(at::where(unwrap_tensor(condition), unwrap_tensor(x), unwrap_tensor(y))));
}

/*
 * ===========================================
 * Indexing Operations Implementation
 * ===========================================
 */

TensorHandle torch_index_select(TensorHandle t, int64_t dim, TensorHandle index) {
    TRY_BLOCK(wrap_tensor(at::index_select(unwrap_tensor(t), dim, unwrap_tensor(index))));
}

TensorHandle torch_gather(TensorHandle t, int64_t dim, TensorHandle index) {
    TRY_BLOCK(wrap_tensor(at::gather(unwrap_tensor(t), dim, unwrap_tensor(index))));
}

TensorHandle torch_slice(TensorHandle t, int64_t dim, int64_t start, int64_t end, int64_t step) {
    TRY_BLOCK(wrap_tensor(at::slice(unwrap_tensor(t), dim, start, end, step)));
}

TensorHandle torch_narrow(TensorHandle t, int64_t dim, int64_t start, int64_t length) {
    TRY_BLOCK(wrap_tensor(at::narrow(unwrap_tensor(t), dim, start, length)));
}

TensorHandle torch_masked_select(TensorHandle t, TensorHandle mask) {
    TRY_BLOCK(wrap_tensor(at::masked_select(unwrap_tensor(t), unwrap_tensor(mask))));
}

TensorHandle torch_take(TensorHandle t, TensorHandle indices) {
    TRY_BLOCK(wrap_tensor(at::take(unwrap_tensor(t), unwrap_tensor(indices))));
}

TensorHandle torch_select(TensorHandle t, int64_t dim, int64_t index) {
    TRY_BLOCK(wrap_tensor(at::select(unwrap_tensor(t), dim, index)));
}

void torch_index_put_(TensorHandle t, TensorHandle indices, TensorHandle values) {
    TRY_BLOCK_VOID({
        std::vector<at::Tensor> idx_vec = {unwrap_tensor(indices)};
        unwrap_tensor(t).index_put_(idx_vec, unwrap_tensor(values));
    });
}

void torch_scatter_(TensorHandle t, int64_t dim, TensorHandle index, TensorHandle src) {
    TRY_BLOCK_VOID(unwrap_tensor(t).scatter_(dim, unwrap_tensor(index), unwrap_tensor(src)));
}

void torch_masked_fill_(TensorHandle t, TensorHandle mask, double value) {
    TRY_BLOCK_VOID(unwrap_tensor(t).masked_fill_(unwrap_tensor(mask), value));
}

/*
 * ===========================================
 * Linear Algebra Implementation
 * ===========================================
 */

TensorHandle torch_inverse(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::inverse(unwrap_tensor(t))));
}

TensorHandle torch_pinverse(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::pinverse(unwrap_tensor(t))));
}

TensorHandle torch_det(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::det(unwrap_tensor(t))));
}

TensorHandle torch_logdet(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::logdet(unwrap_tensor(t))));
}

TensorHandle torch_trace(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::trace(unwrap_tensor(t))));
}

TensorHandle torch_diag(TensorHandle t, int64_t diagonal) {
    TRY_BLOCK(wrap_tensor(at::diag(unwrap_tensor(t), diagonal)));
}

TensorHandle torch_diagonal(TensorHandle t, int64_t offset, int64_t dim1, int64_t dim2) {
    TRY_BLOCK(wrap_tensor(at::diagonal(unwrap_tensor(t), offset, dim1, dim2)));
}

TensorHandle torch_tril(TensorHandle t, int64_t diagonal) {
    TRY_BLOCK(wrap_tensor(at::tril(unwrap_tensor(t), diagonal)));
}

TensorHandle torch_triu(TensorHandle t, int64_t diagonal) {
    TRY_BLOCK(wrap_tensor(at::triu(unwrap_tensor(t), diagonal)));
}

SVDResult torch_svd(TensorHandle t) {
    SVDResult result = {nullptr, nullptr, nullptr};
    clear_error();
    try {
        auto [U, S, Vh] = at::linalg_svd(unwrap_tensor(t));
        result.U = wrap_tensor(std::move(U));
        result.S = wrap_tensor(std::move(S));
        result.Vh = wrap_tensor(std::move(Vh));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
    }
    return result;
}

void torch_svd_free(SVDResult* result) {
    if (result) {
        torch_tensor_free(result->U);
        torch_tensor_free(result->S);
        torch_tensor_free(result->Vh);
        result->U = nullptr;
        result->S = nullptr;
        result->Vh = nullptr;
    }
}

QRResult torch_qr(TensorHandle t) {
    QRResult result = {nullptr, nullptr};
    clear_error();
    try {
        auto [Q, R] = at::linalg_qr(unwrap_tensor(t));
        result.Q = wrap_tensor(std::move(Q));
        result.R = wrap_tensor(std::move(R));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
    }
    return result;
}

void torch_qr_free(QRResult* result) {
    if (result) {
        torch_tensor_free(result->Q);
        torch_tensor_free(result->R);
        result->Q = nullptr;
        result->R = nullptr;
    }
}

LUResult torch_lu(TensorHandle t) {
    LUResult result = {nullptr, nullptr};
    clear_error();
    try {
        auto [L, U] = at::lu_unpack(
            std::get<0>(at::linalg_lu_factor(unwrap_tensor(t))),
            std::get<1>(at::linalg_lu_factor(unwrap_tensor(t)))
        );
        result.L = wrap_tensor(std::move(L));
        result.U = wrap_tensor(std::move(U));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
    }
    return result;
}

void torch_lu_free(LUResult* result) {
    if (result) {
        torch_tensor_free(result->L);
        torch_tensor_free(result->U);
        result->L = nullptr;
        result->U = nullptr;
    }
}

EigResult torch_eig(TensorHandle t) {
    EigResult result = {nullptr, nullptr};
    clear_error();
    try {
        auto [eigenvalues, eigenvectors] = at::linalg_eig(unwrap_tensor(t));
        result.eigenvalues = wrap_tensor(std::move(eigenvalues));
        result.eigenvectors = wrap_tensor(std::move(eigenvectors));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
    }
    return result;
}

void torch_eig_free(EigResult* result) {
    if (result) {
        torch_tensor_free(result->eigenvalues);
        torch_tensor_free(result->eigenvectors);
        result->eigenvalues = nullptr;
        result->eigenvectors = nullptr;
    }
}

TensorHandle torch_cholesky(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(at::linalg_cholesky(unwrap_tensor(t))));
}

TensorHandle torch_solve(TensorHandle A, TensorHandle B) {
    TRY_BLOCK(wrap_tensor(at::linalg_solve(unwrap_tensor(A), unwrap_tensor(B))));
}

TensorHandle torch_lstsq(TensorHandle A, TensorHandle B) {
    TRY_BLOCK(wrap_tensor(std::get<0>(at::linalg_lstsq(unwrap_tensor(A), unwrap_tensor(B)))));
}

TensorHandle torch_triangular_solve(TensorHandle b, TensorHandle A, int upper, int transpose) {
    TRY_BLOCK(wrap_tensor(std::get<0>(at::triangular_solve(
        unwrap_tensor(b), unwrap_tensor(A), upper != 0, transpose != 0
    ))));
}

/*
 * ===========================================
 * In-Place Operations Implementation
 * ===========================================
 */

void torch_add_(TensorHandle a, TensorHandle b) {
    TRY_BLOCK_VOID(unwrap_tensor(a).add_(unwrap_tensor(b)));
}

void torch_sub_(TensorHandle a, TensorHandle b) {
    TRY_BLOCK_VOID(unwrap_tensor(a).sub_(unwrap_tensor(b)));
}

void torch_mul_(TensorHandle a, TensorHandle b) {
    TRY_BLOCK_VOID(unwrap_tensor(a).mul_(unwrap_tensor(b)));
}

void torch_div_(TensorHandle a, TensorHandle b) {
    TRY_BLOCK_VOID(unwrap_tensor(a).div_(unwrap_tensor(b)));
}

void torch_add_scalar_(TensorHandle t, double scalar) {
    TRY_BLOCK_VOID(unwrap_tensor(t).add_(scalar));
}

void torch_sub_scalar_(TensorHandle t, double scalar) {
    TRY_BLOCK_VOID(unwrap_tensor(t).sub_(scalar));
}

void torch_mul_scalar_(TensorHandle t, double scalar) {
    TRY_BLOCK_VOID(unwrap_tensor(t).mul_(scalar));
}

void torch_div_scalar_(TensorHandle t, double scalar) {
    TRY_BLOCK_VOID(unwrap_tensor(t).div_(scalar));
}

void torch_fill_(TensorHandle t, double value) {
    TRY_BLOCK_VOID(unwrap_tensor(t).fill_(value));
}

void torch_zero_(TensorHandle t) {
    TRY_BLOCK_VOID(unwrap_tensor(t).zero_());
}

void torch_ones_(TensorHandle t) {
    TRY_BLOCK_VOID(unwrap_tensor(t).fill_(1.0));
}

void torch_uniform_(TensorHandle t, double low, double high) {
    TRY_BLOCK_VOID(unwrap_tensor(t).uniform_(low, high));
}

void torch_normal_(TensorHandle t, double mean, double std) {
    TRY_BLOCK_VOID(unwrap_tensor(t).normal_(mean, std));
}

/*
 * ===========================================
 * Autograd Implementation
 * ===========================================
 */

void torch_set_requires_grad(TensorHandle t, int requires) {
    TRY_BLOCK_VOID(unwrap_tensor(t).set_requires_grad(requires != 0));
}

void torch_backward(TensorHandle loss) {
    TRY_BLOCK_VOID(unwrap_tensor(loss).backward());
}

void torch_backward_grad(TensorHandle loss, TensorHandle grad_output) {
    TRY_BLOCK_VOID(unwrap_tensor(loss).backward(unwrap_tensor(grad_output)));
}

TensorHandle torch_grad(TensorHandle t) {
    TRY_BLOCK(({
        at::Tensor g = unwrap_tensor(t).grad();
        if (!g.defined()) {
            set_error(3, "Tensor has no gradient");
            nullptr;
        } else {
            wrap_tensor(g);
        }
    }));
}

void torch_zero_grad(TensorHandle t) {
    TRY_BLOCK_VOID({
        at::Tensor& tensor = unwrap_tensor(t);
        if (tensor.grad().defined()) {
            tensor.grad().zero_();
        }
    });
}

TensorHandle torch_detach(TensorHandle t) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).detach()));
}

int torch_is_leaf(TensorHandle t) {
    return unwrap_tensor(t).is_leaf() ? 1 : 0;
}

void torch_retain_grad(TensorHandle t) {
    TRY_BLOCK_VOID(unwrap_tensor(t).retain_grad());
}

/* Global guards for no_grad, enable_grad, inference_mode contexts */
static thread_local std::unique_ptr<torch::NoGradGuard> g_no_grad_guard;
static thread_local std::unique_ptr<torch::AutoGradMode> g_enable_grad_guard;
static thread_local std::unique_ptr<torch::InferenceMode> g_inference_mode_guard;

void torch_no_grad_begin(void) {
    g_no_grad_guard = std::make_unique<torch::NoGradGuard>();
}

void torch_no_grad_end(void) {
    g_no_grad_guard.reset();
}

void torch_enable_grad_begin(void) {
    g_enable_grad_guard = std::make_unique<torch::AutoGradMode>(true);
}

void torch_enable_grad_end(void) {
    g_enable_grad_guard.reset();
}

void torch_inference_mode_begin(void) {
    g_inference_mode_guard = std::make_unique<torch::InferenceMode>();
}

void torch_inference_mode_end(void) {
    g_inference_mode_guard.reset();
}

/*
 * ===========================================
 * Type/Device Conversion Implementation
 * ===========================================
 */

TensorHandle torch_to_dtype(TensorHandle t, TorchDType dtype) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).to(to_scalar_type(dtype))));
}

TensorHandle torch_to_device(TensorHandle t, TorchDeviceType device) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).to(to_device_type(device))));
}

TensorHandle torch_to_device_index(TensorHandle t, TorchDeviceType device, int index) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).to(c10::Device(to_device_type(device), index))));
}

TensorHandle torch_to(TensorHandle t, TorchDType dtype, TorchDeviceType device) {
    TRY_BLOCK(wrap_tensor(unwrap_tensor(t).to(to_scalar_type(dtype)).to(to_device_type(device))));
}

/*
 * ===========================================
 * Device Management Implementation
 * ===========================================
 */

int torch_cuda_is_available(void) {
    return torch::cuda::is_available() ? 1 : 0;
}

int torch_cuda_device_count(void) {
    return static_cast<int>(torch::cuda::device_count());
}

void torch_cuda_synchronize(void) {
    TRY_BLOCK_VOID(torch::cuda::synchronize());
}

void torch_cuda_empty_cache(void) {
    TRY_BLOCK_VOID(c10::cuda::CUDACachingAllocator::emptyCache());
}

void torch_cuda_set_device(int device) {
    TRY_BLOCK_VOID(torch::cuda::set_device(device));
}

int torch_cuda_current_device(void) {
    return static_cast<int>(torch::cuda::current_device());
}

int torch_mps_is_available(void) {
#ifdef __APPLE__
    return at::hasMPS() ? 1 : 0;
#else
    return 0;
#endif
}

/*
 * ===========================================
 * Random Number Generation Implementation
 * ===========================================
 */

void torch_manual_seed(uint64_t seed) {
    TRY_BLOCK_VOID(torch::manual_seed(seed));
}

void torch_cuda_manual_seed(uint64_t seed) {
    TRY_BLOCK_VOID(at::cuda::manual_seed(seed));
}

void torch_cuda_manual_seed_all(uint64_t seed) {
    TRY_BLOCK_VOID(at::cuda::manual_seed_all(seed));
}

/*
 * ===========================================
 * Utility Implementation
 * ===========================================
 */

const char* torch_version(void) {
    return TORCH_VERSION;
}

void torch_tensor_print(TensorHandle t) {
    std::cout << unwrap_tensor(t) << std::endl;
}

char* torch_tensor_to_string(TensorHandle t) {
    try {
        std::ostringstream oss;
        oss << unwrap_tensor(t);
        std::string str = oss.str();
        char* result = static_cast<char*>(std::malloc(str.size() + 1));
        if (result) {
            std::strcpy(result, str.c_str());
        }
        return result;
    } catch (...) {
        return nullptr;
    }
}

void torch_free_string(char* s) {
    std::free(s);
}

size_t torch_cuda_memory_allocated(int device) {
#ifdef USE_CUDA
    clear_error();
    try {
        return c10::cuda::CUDACachingAllocator::currentMemoryAllocated(device);
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
#else
    (void)device;
    return 0;
#endif
}

size_t torch_cuda_memory_reserved(int device) {
#ifdef USE_CUDA
    clear_error();
    try {
        return c10::cuda::CUDACachingAllocator::currentMemoryReserved(device);
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
#else
    (void)device;
    return 0;
#endif
}

/*
 * ===========================================
 * Neural Network Modules Implementation
 * ===========================================
 */

/* Internal: Wrap torch::nn::Module in ModuleHandle */
struct ModuleWrapper {
    torch::nn::AnyModule module;
    std::vector<std::pair<std::string, at::Tensor>> params_cache;

    template<typename T>
    ModuleWrapper(T&& m) : module(std::forward<T>(m)) {
        refresh_params();
    }

    void refresh_params() {
        params_cache.clear();
        for (const auto& p : module.ptr()->named_parameters()) {
            params_cache.emplace_back(p.key(), p.value());
        }
    }
};

static ModuleHandle wrap_module(torch::nn::AnyModule&& m) {
    return reinterpret_cast<ModuleHandle>(new ModuleWrapper(std::move(m)));
}

static ModuleWrapper& unwrap_module(ModuleHandle h) {
    return *reinterpret_cast<ModuleWrapper*>(h);
}

ModuleHandle torch_nn_linear(int64_t in_features, int64_t out_features, int bias) {
    TRY_BLOCK(({
        auto options = torch::nn::LinearOptions(in_features, out_features).bias(bias != 0);
        wrap_module(torch::nn::AnyModule(torch::nn::Linear(options)));
    }));
}

ModuleHandle torch_nn_bilinear(int64_t in1, int64_t in2, int64_t out, int bias) {
    TRY_BLOCK(({
        auto options = torch::nn::BilinearOptions(in1, in2, out).bias(bias != 0);
        wrap_module(torch::nn::AnyModule(torch::nn::Bilinear(options)));
    }));
}

ModuleHandle torch_nn_conv1d(int64_t in_ch, int64_t out_ch, int64_t kernel,
                              int64_t stride, int64_t padding) {
    TRY_BLOCK(({
        auto options = torch::nn::Conv1dOptions(in_ch, out_ch, kernel)
            .stride(stride)
            .padding(padding);
        wrap_module(torch::nn::AnyModule(torch::nn::Conv1d(options)));
    }));
}

ModuleHandle torch_nn_conv2d(int64_t in_ch, int64_t out_ch, int64_t kernel,
                              int64_t stride, int64_t padding) {
    TRY_BLOCK(({
        auto options = torch::nn::Conv2dOptions(in_ch, out_ch, kernel)
            .stride(stride)
            .padding(padding);
        wrap_module(torch::nn::AnyModule(torch::nn::Conv2d(options)));
    }));
}

ModuleHandle torch_nn_batch_norm1d(int64_t num_features) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::BatchNorm1d(num_features))));
}

ModuleHandle torch_nn_batch_norm2d(int64_t num_features) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::BatchNorm2d(num_features))));
}

ModuleHandle torch_nn_layer_norm(const int64_t* normalized_shape, size_t ndim) {
    TRY_BLOCK(({
        std::vector<int64_t> shape(normalized_shape, normalized_shape + ndim);
        wrap_module(torch::nn::AnyModule(torch::nn::LayerNorm(
            torch::nn::LayerNormOptions(shape)
        )));
    }));
}

ModuleHandle torch_nn_dropout(double p) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::Dropout(p))));
}

ModuleHandle torch_nn_dropout2d(double p) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::Dropout2d(p))));
}

ModuleHandle torch_nn_max_pool1d(int64_t kernel, int64_t stride) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::MaxPool1d(
        torch::nn::MaxPool1dOptions(kernel).stride(stride)
    ))));
}

ModuleHandle torch_nn_max_pool2d(int64_t kernel, int64_t stride) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::MaxPool2d(
        torch::nn::MaxPool2dOptions(kernel).stride(stride)
    ))));
}

ModuleHandle torch_nn_avg_pool1d(int64_t kernel, int64_t stride) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::AvgPool1d(
        torch::nn::AvgPool1dOptions(kernel).stride(stride)
    ))));
}

ModuleHandle torch_nn_avg_pool2d(int64_t kernel, int64_t stride) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::AvgPool2d(
        torch::nn::AvgPool2dOptions(kernel).stride(stride)
    ))));
}

ModuleHandle torch_nn_adaptive_avg_pool1d(int64_t output_size) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::AdaptiveAvgPool1d(output_size))));
}

ModuleHandle torch_nn_adaptive_avg_pool2d(int64_t h, int64_t w) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::AdaptiveAvgPool2d(
        torch::nn::AdaptiveAvgPool2dOptions({h, w})
    ))));
}

ModuleHandle torch_nn_rnn(int64_t input_size, int64_t hidden_size, int64_t num_layers) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::RNN(
        torch::nn::RNNOptions(input_size, hidden_size).num_layers(num_layers)
    ))));
}

ModuleHandle torch_nn_lstm(int64_t input_size, int64_t hidden_size, int64_t num_layers) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::LSTM(
        torch::nn::LSTMOptions(input_size, hidden_size).num_layers(num_layers)
    ))));
}

ModuleHandle torch_nn_gru(int64_t input_size, int64_t hidden_size, int64_t num_layers) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::GRU(
        torch::nn::GRUOptions(input_size, hidden_size).num_layers(num_layers)
    ))));
}

ModuleHandle torch_nn_embedding(int64_t num_embeddings, int64_t embedding_dim) {
    TRY_BLOCK(wrap_module(torch::nn::AnyModule(torch::nn::Embedding(num_embeddings, embedding_dim))));
}

TensorHandle torch_nn_forward(ModuleHandle m, TensorHandle input) {
    TRY_BLOCK(wrap_tensor(unwrap_module(m).module.forward(unwrap_tensor(input)).toTensor()));
}

void torch_nn_train(ModuleHandle m) {
    TRY_BLOCK_VOID(unwrap_module(m).module.ptr()->train());
}

void torch_nn_eval(ModuleHandle m) {
    TRY_BLOCK_VOID(unwrap_module(m).module.ptr()->eval());
}

void torch_nn_free(ModuleHandle m) {
    if (m) {
        delete reinterpret_cast<ModuleWrapper*>(m);
    }
}

size_t torch_nn_num_parameters(ModuleHandle m) {
    return unwrap_module(m).params_cache.size();
}

TensorHandle torch_nn_parameter(ModuleHandle m, size_t idx) {
    clear_error();
    try {
        auto& wrapper = unwrap_module(m);
        if (idx >= wrapper.params_cache.size()) {
            set_error(4, "Parameter index out of bounds");
            return nullptr;
        }
        return wrap_tensor(wrapper.params_cache[idx].second);
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

const char* torch_nn_parameter_name(ModuleHandle m, size_t idx) {
    auto& wrapper = unwrap_module(m);
    if (idx >= wrapper.params_cache.size()) {
        return nullptr;
    }
    return wrapper.params_cache[idx].first.c_str();
}

void torch_nn_foreach_parameter(ModuleHandle m, ParameterCallback cb, void* user_data) {
    TRY_BLOCK_VOID({
        auto& wrapper = unwrap_module(m);
        for (const auto& p : wrapper.params_cache) {
            TensorHandle h = wrap_tensor(p.second);
            cb(p.first.c_str(), h, user_data);
            torch_tensor_free(h);
        }
    });
}

/*
 * ===========================================
 * Loss Functions Implementation
 * ===========================================
 */

TensorHandle torch_nn_mse_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::mse_loss(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_l1_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::l1_loss(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_smooth_l1_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::smooth_l1_loss(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_cross_entropy_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::cross_entropy(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_nll_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::nll_loss(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_binary_cross_entropy(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::binary_cross_entropy(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_binary_cross_entropy_with_logits(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::binary_cross_entropy_with_logits(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_kl_div_loss(TensorHandle input, TensorHandle target) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::kl_div(
        unwrap_tensor(input), unwrap_tensor(target)
    )));
}

TensorHandle torch_nn_huber_loss(TensorHandle input, TensorHandle target, double delta) {
    TRY_BLOCK(wrap_tensor(torch::nn::functional::huber_loss(
        unwrap_tensor(input), unwrap_tensor(target),
        torch::nn::functional::HuberLossFuncOptions().delta(delta)
    )));
}

/*
 * ===========================================
 * Optimizers Implementation
 * ===========================================
 */

struct OptimizerWrapper {
    std::unique_ptr<torch::optim::Optimizer> optimizer;
    std::vector<at::Tensor> params;

    OptimizerWrapper(std::unique_ptr<torch::optim::Optimizer>&& opt, std::vector<at::Tensor>&& p)
        : optimizer(std::move(opt)), params(std::move(p)) {}
};

static OptimizerHandle wrap_optimizer(std::unique_ptr<torch::optim::Optimizer>&& opt,
                                       std::vector<at::Tensor>&& params) {
    return reinterpret_cast<OptimizerHandle>(new OptimizerWrapper(std::move(opt), std::move(params)));
}

static OptimizerWrapper& unwrap_optimizer(OptimizerHandle h) {
    return *reinterpret_cast<OptimizerWrapper*>(h);
}

OptimizerHandle torch_optim_sgd(TensorHandle* params, size_t count,
                                 double lr, double momentum, double weight_decay) {
    clear_error();
    try {
        std::vector<at::Tensor> param_vec;
        param_vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            param_vec.push_back(unwrap_tensor(params[i]));
        }
        auto options = torch::optim::SGDOptions(lr)
            .momentum(momentum)
            .weight_decay(weight_decay);
        auto opt = std::make_unique<torch::optim::SGD>(param_vec, options);
        return wrap_optimizer(std::move(opt), std::move(param_vec));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

OptimizerHandle torch_optim_adam(TensorHandle* params, size_t count,
                                  double lr, double beta1, double beta2, double eps) {
    clear_error();
    try {
        std::vector<at::Tensor> param_vec;
        param_vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            param_vec.push_back(unwrap_tensor(params[i]));
        }
        auto options = torch::optim::AdamOptions(lr)
            .betas({beta1, beta2})
            .eps(eps);
        auto opt = std::make_unique<torch::optim::Adam>(param_vec, options);
        return wrap_optimizer(std::move(opt), std::move(param_vec));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

OptimizerHandle torch_optim_adamw(TensorHandle* params, size_t count,
                                   double lr, double beta1, double beta2, double eps,
                                   double weight_decay) {
    clear_error();
    try {
        std::vector<at::Tensor> param_vec;
        param_vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            param_vec.push_back(unwrap_tensor(params[i]));
        }
        auto options = torch::optim::AdamWOptions(lr)
            .betas({beta1, beta2})
            .eps(eps)
            .weight_decay(weight_decay);
        auto opt = std::make_unique<torch::optim::AdamW>(param_vec, options);
        return wrap_optimizer(std::move(opt), std::move(param_vec));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

OptimizerHandle torch_optim_rmsprop(TensorHandle* params, size_t count,
                                     double lr, double alpha, double eps) {
    clear_error();
    try {
        std::vector<at::Tensor> param_vec;
        param_vec.reserve(count);
        for (size_t i = 0; i < count; i++) {
            param_vec.push_back(unwrap_tensor(params[i]));
        }
        auto options = torch::optim::RMSpropOptions(lr)
            .alpha(alpha)
            .eps(eps);
        auto opt = std::make_unique<torch::optim::RMSprop>(param_vec, options);
        return wrap_optimizer(std::move(opt), std::move(param_vec));
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

void torch_optim_step(OptimizerHandle opt) {
    TRY_BLOCK_VOID(unwrap_optimizer(opt).optimizer->step());
}

void torch_optim_zero_grad(OptimizerHandle opt) {
    TRY_BLOCK_VOID(unwrap_optimizer(opt).optimizer->zero_grad());
}

void torch_optim_free(OptimizerHandle opt) {
    if (opt) {
        delete reinterpret_cast<OptimizerWrapper*>(opt);
    }
}

void torch_optim_set_lr(OptimizerHandle opt, double lr) {
    TRY_BLOCK_VOID({
        auto& wrapper = unwrap_optimizer(opt);
        for (auto& group : wrapper.optimizer->param_groups()) {
            group.options().set_lr(lr);
        }
    });
}

double torch_optim_get_lr(OptimizerHandle opt) {
    clear_error();
    try {
        auto& wrapper = unwrap_optimizer(opt);
        if (wrapper.optimizer->param_groups().empty()) {
            return 0.0;
        }
        return wrapper.optimizer->param_groups()[0].options().get_lr();
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0.0;
    }
}

/*
 * ===========================================
 * Serialization Implementation
 * ===========================================
 */

int torch_save_tensor(TensorHandle t, const char* path) {
    clear_error();
    try {
        torch::save(unwrap_tensor(t), path);
        return 1;
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

TensorHandle torch_load_tensor(const char* path) {
    TRY_BLOCK(({
        at::Tensor t;
        torch::load(t, path);
        wrap_tensor(std::move(t));
    }));
}

int torch_save_module(ModuleHandle m, const char* path) {
    clear_error();
    try {
        torch::save(unwrap_module(m).module.ptr(), path);
        return 1;
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return 0;
    }
}

ModuleHandle torch_load_module(const char* path) {
    clear_error();
    try {
        /* Loading modules requires knowing the type, so we use a generic approach */
        set_error(5, "torch_load_module requires type information - use torch_load_tensor for state dicts");
        return nullptr;
    } catch (const std::exception& e) {
        set_error_from_exception(e);
        return nullptr;
    }
}

} /* extern "C" */
