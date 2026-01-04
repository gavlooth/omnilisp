/*
 * libtorch_c_api.h - C wrapper for LibTorch
 *
 * Provides a C99-compatible interface to LibTorch (PyTorch C++ API).
 * All tensor operations are exposed through opaque handles with explicit
 * memory management compatible with OmniLisp's ASAP system.
 *
 * Copyright (c) 2026 OmniLisp Project
 * SPDX-License-Identifier: MIT
 */

#ifndef LIBTORCH_C_API_H
#define LIBTORCH_C_API_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ===========================================
 * Opaque Handle Types
 * ===========================================
 *
 * All LibTorch objects are wrapped in opaque handles.
 * Handles must be freed with the corresponding free function.
 */

typedef struct TorchTensor_*   TensorHandle;
typedef struct TorchModule_*   ModuleHandle;
typedef struct TorchOptimizer_* OptimizerHandle;
typedef struct TorchScalar_*   ScalarHandle;

/*
 * ===========================================
 * Data Types
 * ===========================================
 */

typedef enum TorchDType {
    TORCH_FLOAT16  = 0,
    TORCH_FLOAT32  = 1,
    TORCH_FLOAT64  = 2,
    TORCH_BFLOAT16 = 3,
    TORCH_INT8     = 4,
    TORCH_INT16    = 5,
    TORCH_INT32    = 6,
    TORCH_INT64    = 7,
    TORCH_UINT8    = 8,
    TORCH_BOOL     = 9,
    TORCH_COMPLEX64  = 10,
    TORCH_COMPLEX128 = 11
} TorchDType;

typedef enum TorchDeviceType {
    TORCH_CPU  = 0,
    TORCH_CUDA = 1,
    TORCH_MPS  = 2,   /* Apple Metal Performance Shaders */
    TORCH_XPU  = 3    /* Intel GPU */
} TorchDeviceType;

/*
 * ===========================================
 * Error Handling
 * ===========================================
 *
 * Thread-local error state. Check after any function that may fail.
 */

#define TORCH_ERROR_MSG_SIZE 512

typedef struct TorchError {
    int code;                           /* 0 = success, non-zero = error */
    char message[TORCH_ERROR_MSG_SIZE]; /* Human-readable error message */
} TorchError;

/* Get last error for current thread */
TorchError* torch_get_last_error(void);

/* Clear error state */
void torch_clear_error(void);

/* Check if last operation succeeded */
int torch_ok(void);

/*
 * ===========================================
 * Tensor Creation
 * ===========================================
 */

/**
 * Create empty (uninitialized) tensor.
 *
 * @param shape Array of dimension sizes
 * @param ndim  Number of dimensions
 * @param dtype Data type
 * @param device Device type (CPU, CUDA, etc.)
 * @return New tensor handle, or NULL on error
 */
TensorHandle torch_tensor_empty(
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype,
    TorchDeviceType device
);

/**
 * Create tensor from existing data (copies data).
 *
 * @param data Pointer to source data (must match dtype element size)
 * @param shape Array of dimension sizes
 * @param ndim Number of dimensions
 * @param dtype Data type
 * @return New tensor handle, or NULL on error
 */
TensorHandle torch_tensor_from_data(
    const void* data,
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype
);

/**
 * Create tensor from data without copying (view, careful with lifetime!).
 * The data must remain valid for the lifetime of the tensor.
 *
 * @param data Pointer to source data
 * @param shape Array of dimension sizes
 * @param ndim Number of dimensions
 * @param dtype Data type
 * @return New tensor handle, or NULL on error
 */
TensorHandle torch_tensor_from_blob(
    void* data,
    const int64_t* shape,
    size_t ndim,
    TorchDType dtype
);

/* Special tensor constructors */
TensorHandle torch_tensor_zeros(const int64_t* shape, size_t ndim, TorchDType dtype);
TensorHandle torch_tensor_ones(const int64_t* shape, size_t ndim, TorchDType dtype);
TensorHandle torch_tensor_full(const int64_t* shape, size_t ndim, double value, TorchDType dtype);
TensorHandle torch_tensor_eye(int64_t n, TorchDType dtype);
TensorHandle torch_tensor_eye_m_n(int64_t m, int64_t n, TorchDType dtype);
TensorHandle torch_tensor_arange(double start, double end, double step, TorchDType dtype);
TensorHandle torch_tensor_linspace(double start, double end, int64_t steps, TorchDType dtype);
TensorHandle torch_tensor_logspace(double start, double end, int64_t steps, double base, TorchDType dtype);

/* Random tensor constructors */
TensorHandle torch_tensor_rand(const int64_t* shape, size_t ndim);
TensorHandle torch_tensor_randn(const int64_t* shape, size_t ndim);
TensorHandle torch_tensor_randint(int64_t low, int64_t high, const int64_t* shape, size_t ndim);
TensorHandle torch_tensor_rand_like(TensorHandle t);
TensorHandle torch_tensor_randn_like(TensorHandle t);

/* Clone and copy */
TensorHandle torch_tensor_clone(TensorHandle t);
void torch_tensor_copy_(TensorHandle dst, TensorHandle src);

/* Free tensor (decrement reference count) */
void torch_tensor_free(TensorHandle t);

/*
 * ===========================================
 * Tensor Properties
 * ===========================================
 */

/* Number of dimensions */
size_t torch_tensor_ndim(TensorHandle t);

/* Get shape (fills shape_out, returns actual ndim) */
size_t torch_tensor_shape(TensorHandle t, int64_t* shape_out, size_t max_dims);

/* Get single dimension size */
int64_t torch_tensor_size(TensorHandle t, int64_t dim);

/* Total number of elements */
int64_t torch_tensor_numel(TensorHandle t);

/* Data type */
TorchDType torch_tensor_dtype(TensorHandle t);

/* Device type */
TorchDeviceType torch_tensor_device(TensorHandle t);

/* Device index (e.g., which GPU) */
int torch_tensor_device_index(TensorHandle t);

/* Gradient tracking */
int torch_tensor_requires_grad(TensorHandle t);

/* Memory layout */
int torch_tensor_is_contiguous(TensorHandle t);

/* Stride for each dimension */
size_t torch_tensor_strides(TensorHandle t, int64_t* strides_out, size_t max_dims);

/* Element size in bytes */
size_t torch_tensor_element_size(TensorHandle t);

/* Total storage size in bytes */
size_t torch_tensor_nbytes(TensorHandle t);

/*
 * ===========================================
 * Data Access
 * ===========================================
 */

/**
 * Get raw data pointer.
 * WARNING: Pointer lifetime tied to tensor! Use carefully.
 */
void* torch_tensor_data_ptr(TensorHandle t);

/**
 * Copy tensor data to external buffer (safe).
 *
 * @param t Tensor to copy from
 * @param out Output buffer
 * @param out_bytes Size of output buffer in bytes
 * @return Number of bytes copied, or -1 on error
 */
int64_t torch_tensor_copy_data(TensorHandle t, void* out, size_t out_bytes);

/**
 * Get single scalar value from 0-d or 1-element tensor.
 */
double torch_tensor_item(TensorHandle t);
int64_t torch_tensor_item_int(TensorHandle t);

/**
 * Get element at index (for 1-D tensor).
 */
double torch_tensor_get_1d(TensorHandle t, int64_t i);

/**
 * Get element at (i, j) (for 2-D tensor).
 */
double torch_tensor_get_2d(TensorHandle t, int64_t i, int64_t j);

/**
 * Set element at index (for 1-D tensor).
 */
void torch_tensor_set_1d(TensorHandle t, int64_t i, double value);

/**
 * Set element at (i, j) (for 2-D tensor).
 */
void torch_tensor_set_2d(TensorHandle t, int64_t i, int64_t j, double value);

/*
 * ===========================================
 * Arithmetic Operations (Return New Tensor)
 * ===========================================
 */

/* Element-wise arithmetic */
TensorHandle torch_add(TensorHandle a, TensorHandle b);
TensorHandle torch_sub(TensorHandle a, TensorHandle b);
TensorHandle torch_mul(TensorHandle a, TensorHandle b);
TensorHandle torch_div(TensorHandle a, TensorHandle b);
TensorHandle torch_fmod(TensorHandle a, TensorHandle b);
TensorHandle torch_remainder(TensorHandle a, TensorHandle b);
TensorHandle torch_pow(TensorHandle base, TensorHandle exp);

/* Scalar arithmetic */
TensorHandle torch_add_scalar(TensorHandle t, double scalar);
TensorHandle torch_sub_scalar(TensorHandle t, double scalar);
TensorHandle torch_mul_scalar(TensorHandle t, double scalar);
TensorHandle torch_div_scalar(TensorHandle t, double scalar);
TensorHandle torch_pow_scalar(TensorHandle t, double exp);
TensorHandle torch_scalar_sub(double scalar, TensorHandle t);
TensorHandle torch_scalar_div(double scalar, TensorHandle t);

/* Matrix operations */
TensorHandle torch_matmul(TensorHandle a, TensorHandle b);
TensorHandle torch_mm(TensorHandle a, TensorHandle b);       /* 2D only */
TensorHandle torch_bmm(TensorHandle a, TensorHandle b);      /* Batched */
TensorHandle torch_mv(TensorHandle mat, TensorHandle vec);   /* Matrix-vector */
TensorHandle torch_dot(TensorHandle a, TensorHandle b);      /* Dot product */
TensorHandle torch_outer(TensorHandle a, TensorHandle b);    /* Outer product */

/*
 * ===========================================
 * Reduction Operations
 * ===========================================
 */

/* Full reductions (return scalar tensor) */
TensorHandle torch_sum(TensorHandle t);
TensorHandle torch_prod(TensorHandle t);
TensorHandle torch_mean(TensorHandle t);
TensorHandle torch_std(TensorHandle t);
TensorHandle torch_var(TensorHandle t);
TensorHandle torch_max(TensorHandle t);
TensorHandle torch_min(TensorHandle t);
TensorHandle torch_argmax(TensorHandle t);
TensorHandle torch_argmin(TensorHandle t);
TensorHandle torch_norm(TensorHandle t);

/* Dimension reductions */
TensorHandle torch_sum_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_prod_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_mean_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_std_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_var_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_max_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_min_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_argmax_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_argmin_dim(TensorHandle t, int64_t dim, int keepdim);
TensorHandle torch_norm_dim(TensorHandle t, double p, int64_t dim, int keepdim);

/* Cumulative operations */
TensorHandle torch_cumsum(TensorHandle t, int64_t dim);
TensorHandle torch_cumprod(TensorHandle t, int64_t dim);

/* Boolean reductions */
int torch_all(TensorHandle t);   /* Returns 1 if all true */
int torch_any(TensorHandle t);   /* Returns 1 if any true */

/*
 * ===========================================
 * Unary Operations
 * ===========================================
 */

/* Basic unary */
TensorHandle torch_neg(TensorHandle t);
TensorHandle torch_abs(TensorHandle t);
TensorHandle torch_sign(TensorHandle t);
TensorHandle torch_ceil(TensorHandle t);
TensorHandle torch_floor(TensorHandle t);
TensorHandle torch_round(TensorHandle t);
TensorHandle torch_trunc(TensorHandle t);
TensorHandle torch_frac(TensorHandle t);
TensorHandle torch_reciprocal(TensorHandle t);
TensorHandle torch_clamp(TensorHandle t, double min_val, double max_val);

/* Exponential and logarithmic */
TensorHandle torch_sqrt(TensorHandle t);
TensorHandle torch_rsqrt(TensorHandle t);
TensorHandle torch_square(TensorHandle t);
TensorHandle torch_exp(TensorHandle t);
TensorHandle torch_exp2(TensorHandle t);
TensorHandle torch_expm1(TensorHandle t);
TensorHandle torch_log(TensorHandle t);
TensorHandle torch_log2(TensorHandle t);
TensorHandle torch_log10(TensorHandle t);
TensorHandle torch_log1p(TensorHandle t);

/* Trigonometric */
TensorHandle torch_sin(TensorHandle t);
TensorHandle torch_cos(TensorHandle t);
TensorHandle torch_tan(TensorHandle t);
TensorHandle torch_asin(TensorHandle t);
TensorHandle torch_acos(TensorHandle t);
TensorHandle torch_atan(TensorHandle t);
TensorHandle torch_atan2(TensorHandle y, TensorHandle x);
TensorHandle torch_sinh(TensorHandle t);
TensorHandle torch_cosh(TensorHandle t);
TensorHandle torch_tanh(TensorHandle t);
TensorHandle torch_asinh(TensorHandle t);
TensorHandle torch_acosh(TensorHandle t);
TensorHandle torch_atanh(TensorHandle t);

/* Activation functions */
TensorHandle torch_sigmoid(TensorHandle t);
TensorHandle torch_relu(TensorHandle t);
TensorHandle torch_relu6(TensorHandle t);
TensorHandle torch_leaky_relu(TensorHandle t, double negative_slope);
TensorHandle torch_elu(TensorHandle t, double alpha);
TensorHandle torch_selu(TensorHandle t);
TensorHandle torch_gelu(TensorHandle t);
TensorHandle torch_silu(TensorHandle t);     /* Swish */
TensorHandle torch_mish(TensorHandle t);
TensorHandle torch_softplus(TensorHandle t, double beta, double threshold);
TensorHandle torch_softsign(TensorHandle t);
TensorHandle torch_softmax(TensorHandle t, int64_t dim);
TensorHandle torch_log_softmax(TensorHandle t, int64_t dim);

/*
 * ===========================================
 * Comparison Operations
 * ===========================================
 */

TensorHandle torch_eq(TensorHandle a, TensorHandle b);
TensorHandle torch_ne(TensorHandle a, TensorHandle b);
TensorHandle torch_lt(TensorHandle a, TensorHandle b);
TensorHandle torch_le(TensorHandle a, TensorHandle b);
TensorHandle torch_gt(TensorHandle a, TensorHandle b);
TensorHandle torch_ge(TensorHandle a, TensorHandle b);

TensorHandle torch_eq_scalar(TensorHandle t, double scalar);
TensorHandle torch_ne_scalar(TensorHandle t, double scalar);
TensorHandle torch_lt_scalar(TensorHandle t, double scalar);
TensorHandle torch_le_scalar(TensorHandle t, double scalar);
TensorHandle torch_gt_scalar(TensorHandle t, double scalar);
TensorHandle torch_ge_scalar(TensorHandle t, double scalar);

TensorHandle torch_isnan(TensorHandle t);
TensorHandle torch_isinf(TensorHandle t);
TensorHandle torch_isfinite(TensorHandle t);

TensorHandle torch_where(TensorHandle condition, TensorHandle x, TensorHandle y);

/*
 * ===========================================
 * Shape Manipulation
 * ===========================================
 */

TensorHandle torch_reshape(TensorHandle t, const int64_t* shape, size_t ndim);
TensorHandle torch_view(TensorHandle t, const int64_t* shape, size_t ndim);
TensorHandle torch_transpose(TensorHandle t, int64_t dim0, int64_t dim1);
TensorHandle torch_permute(TensorHandle t, const int64_t* dims, size_t ndim);
TensorHandle torch_squeeze(TensorHandle t);
TensorHandle torch_squeeze_dim(TensorHandle t, int64_t dim);
TensorHandle torch_unsqueeze(TensorHandle t, int64_t dim);
TensorHandle torch_flatten(TensorHandle t);
TensorHandle torch_flatten_range(TensorHandle t, int64_t start_dim, int64_t end_dim);
TensorHandle torch_contiguous(TensorHandle t);
TensorHandle torch_expand(TensorHandle t, const int64_t* sizes, size_t ndim);
TensorHandle torch_repeat(TensorHandle t, const int64_t* repeats, size_t ndim);
TensorHandle torch_t(TensorHandle t);   /* 2D transpose shorthand */

/* Combining tensors */
TensorHandle torch_cat(TensorHandle* tensors, size_t count, int64_t dim);
TensorHandle torch_stack(TensorHandle* tensors, size_t count, int64_t dim);
TensorHandle torch_hstack(TensorHandle* tensors, size_t count);
TensorHandle torch_vstack(TensorHandle* tensors, size_t count);
TensorHandle torch_dstack(TensorHandle* tensors, size_t count);

/* Splitting tensors */
size_t torch_split(TensorHandle t, int64_t split_size, int64_t dim,
                   TensorHandle* out, size_t max_out);
size_t torch_chunk(TensorHandle t, size_t chunks, int64_t dim,
                   TensorHandle* out, size_t max_out);

/*
 * ===========================================
 * Indexing and Slicing
 * ===========================================
 */

TensorHandle torch_index_select(TensorHandle t, int64_t dim, TensorHandle index);
TensorHandle torch_gather(TensorHandle t, int64_t dim, TensorHandle index);
TensorHandle torch_slice(TensorHandle t, int64_t dim, int64_t start, int64_t end, int64_t step);
TensorHandle torch_narrow(TensorHandle t, int64_t dim, int64_t start, int64_t length);
TensorHandle torch_masked_select(TensorHandle t, TensorHandle mask);
TensorHandle torch_take(TensorHandle t, TensorHandle indices);

/* Index with single integer (returns tensor with one less dimension) */
TensorHandle torch_select(TensorHandle t, int64_t dim, int64_t index);

/* In-place indexed assignment */
void torch_index_put_(TensorHandle t, TensorHandle indices, TensorHandle values);
void torch_scatter_(TensorHandle t, int64_t dim, TensorHandle index, TensorHandle src);
void torch_masked_fill_(TensorHandle t, TensorHandle mask, double value);

/*
 * ===========================================
 * Linear Algebra
 * ===========================================
 */

TensorHandle torch_inverse(TensorHandle t);
TensorHandle torch_pinverse(TensorHandle t);
TensorHandle torch_det(TensorHandle t);
TensorHandle torch_logdet(TensorHandle t);
TensorHandle torch_trace(TensorHandle t);
TensorHandle torch_diag(TensorHandle t, int64_t diagonal);
TensorHandle torch_diagonal(TensorHandle t, int64_t offset, int64_t dim1, int64_t dim2);
TensorHandle torch_tril(TensorHandle t, int64_t diagonal);
TensorHandle torch_triu(TensorHandle t, int64_t diagonal);

/* Decompositions (return multiple tensors) */
typedef struct {
    TensorHandle U;
    TensorHandle S;
    TensorHandle Vh;
} SVDResult;

SVDResult torch_svd(TensorHandle t);
void torch_svd_free(SVDResult* result);

typedef struct {
    TensorHandle Q;
    TensorHandle R;
} QRResult;

QRResult torch_qr(TensorHandle t);
void torch_qr_free(QRResult* result);

typedef struct {
    TensorHandle L;
    TensorHandle U;
} LUResult;

LUResult torch_lu(TensorHandle t);
void torch_lu_free(LUResult* result);

typedef struct {
    TensorHandle eigenvalues;
    TensorHandle eigenvectors;
} EigResult;

EigResult torch_eig(TensorHandle t);
void torch_eig_free(EigResult* result);

TensorHandle torch_cholesky(TensorHandle t);

/* Linear system solvers */
TensorHandle torch_solve(TensorHandle A, TensorHandle B);
TensorHandle torch_lstsq(TensorHandle A, TensorHandle B);
TensorHandle torch_triangular_solve(TensorHandle b, TensorHandle A, int upper, int transpose);

/*
 * ===========================================
 * Type and Device Conversion
 * ===========================================
 */

TensorHandle torch_to_dtype(TensorHandle t, TorchDType dtype);
TensorHandle torch_to_device(TensorHandle t, TorchDeviceType device);
TensorHandle torch_to_device_index(TensorHandle t, TorchDeviceType device, int index);
TensorHandle torch_to(TensorHandle t, TorchDType dtype, TorchDeviceType device);

/*
 * ===========================================
 * In-Place Operations
 * ===========================================
 */

/* In-place arithmetic (modify first tensor) */
void torch_add_(TensorHandle a, TensorHandle b);
void torch_sub_(TensorHandle a, TensorHandle b);
void torch_mul_(TensorHandle a, TensorHandle b);
void torch_div_(TensorHandle a, TensorHandle b);

void torch_add_scalar_(TensorHandle t, double scalar);
void torch_sub_scalar_(TensorHandle t, double scalar);
void torch_mul_scalar_(TensorHandle t, double scalar);
void torch_div_scalar_(TensorHandle t, double scalar);

/* In-place utilities */
void torch_fill_(TensorHandle t, double value);
void torch_zero_(TensorHandle t);
void torch_ones_(TensorHandle t);
void torch_uniform_(TensorHandle t, double low, double high);
void torch_normal_(TensorHandle t, double mean, double std);

/*
 * ===========================================
 * Autograd
 * ===========================================
 */

/* Enable/disable gradient tracking */
void torch_set_requires_grad(TensorHandle t, int requires);

/* Compute gradients (backward pass) */
void torch_backward(TensorHandle loss);
void torch_backward_grad(TensorHandle loss, TensorHandle grad_output);

/* Get gradient tensor */
TensorHandle torch_grad(TensorHandle t);

/* Zero out gradients */
void torch_zero_grad(TensorHandle t);

/* Detach from computation graph */
TensorHandle torch_detach(TensorHandle t);

/* Check if tensor is leaf (created by user, not by operation) */
int torch_is_leaf(TensorHandle t);

/* Retain gradient for non-leaf tensors */
void torch_retain_grad(TensorHandle t);

/* No-grad context (disable gradient computation) */
void torch_no_grad_begin(void);
void torch_no_grad_end(void);

/* Enable-grad context (re-enable gradient computation) */
void torch_enable_grad_begin(void);
void torch_enable_grad_end(void);

/* Inference mode (more aggressive optimization than no_grad) */
void torch_inference_mode_begin(void);
void torch_inference_mode_end(void);

/*
 * ===========================================
 * Neural Network Modules
 * ===========================================
 */

/* Linear layers */
ModuleHandle torch_nn_linear(int64_t in_features, int64_t out_features, int bias);
ModuleHandle torch_nn_bilinear(int64_t in1, int64_t in2, int64_t out, int bias);

/* Convolutional layers */
ModuleHandle torch_nn_conv1d(int64_t in_ch, int64_t out_ch, int64_t kernel,
                              int64_t stride, int64_t padding);
ModuleHandle torch_nn_conv2d(int64_t in_ch, int64_t out_ch, int64_t kernel,
                              int64_t stride, int64_t padding);

/* Normalization layers */
ModuleHandle torch_nn_batch_norm1d(int64_t num_features);
ModuleHandle torch_nn_batch_norm2d(int64_t num_features);
ModuleHandle torch_nn_layer_norm(const int64_t* normalized_shape, size_t ndim);

/* Dropout */
ModuleHandle torch_nn_dropout(double p);
ModuleHandle torch_nn_dropout2d(double p);

/* Pooling */
ModuleHandle torch_nn_max_pool1d(int64_t kernel, int64_t stride);
ModuleHandle torch_nn_max_pool2d(int64_t kernel, int64_t stride);
ModuleHandle torch_nn_avg_pool1d(int64_t kernel, int64_t stride);
ModuleHandle torch_nn_avg_pool2d(int64_t kernel, int64_t stride);
ModuleHandle torch_nn_adaptive_avg_pool1d(int64_t output_size);
ModuleHandle torch_nn_adaptive_avg_pool2d(int64_t h, int64_t w);

/* RNN layers */
ModuleHandle torch_nn_rnn(int64_t input_size, int64_t hidden_size, int64_t num_layers);
ModuleHandle torch_nn_lstm(int64_t input_size, int64_t hidden_size, int64_t num_layers);
ModuleHandle torch_nn_gru(int64_t input_size, int64_t hidden_size, int64_t num_layers);

/* Embedding */
ModuleHandle torch_nn_embedding(int64_t num_embeddings, int64_t embedding_dim);

/* Module operations */
TensorHandle torch_nn_forward(ModuleHandle m, TensorHandle input);
void torch_nn_train(ModuleHandle m);     /* Set training mode */
void torch_nn_eval(ModuleHandle m);      /* Set evaluation mode */
void torch_nn_free(ModuleHandle m);

/* Parameter access */
size_t torch_nn_num_parameters(ModuleHandle m);
TensorHandle torch_nn_parameter(ModuleHandle m, size_t idx);
const char* torch_nn_parameter_name(ModuleHandle m, size_t idx);

/* Named parameters iteration */
typedef void (*ParameterCallback)(const char* name, TensorHandle param, void* user_data);
void torch_nn_foreach_parameter(ModuleHandle m, ParameterCallback cb, void* user_data);

/*
 * ===========================================
 * Loss Functions
 * ===========================================
 */

TensorHandle torch_nn_mse_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_l1_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_smooth_l1_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_cross_entropy_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_nll_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_binary_cross_entropy(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_binary_cross_entropy_with_logits(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_kl_div_loss(TensorHandle input, TensorHandle target);
TensorHandle torch_nn_huber_loss(TensorHandle input, TensorHandle target, double delta);

/*
 * ===========================================
 * Optimizers
 * ===========================================
 */

OptimizerHandle torch_optim_sgd(TensorHandle* params, size_t count,
                                 double lr, double momentum, double weight_decay);
OptimizerHandle torch_optim_adam(TensorHandle* params, size_t count,
                                  double lr, double beta1, double beta2, double eps);
OptimizerHandle torch_optim_adamw(TensorHandle* params, size_t count,
                                   double lr, double beta1, double beta2, double eps,
                                   double weight_decay);
OptimizerHandle torch_optim_rmsprop(TensorHandle* params, size_t count,
                                     double lr, double alpha, double eps, double momentum);

void torch_optim_step(OptimizerHandle opt);
void torch_optim_zero_grad(OptimizerHandle opt);
void torch_optim_free(OptimizerHandle opt);

/* Learning rate scheduling */
void torch_optim_set_lr(OptimizerHandle opt, double lr);
double torch_optim_get_lr(OptimizerHandle opt);

/*
 * ===========================================
 * Serialization
 * ===========================================
 */

int torch_save_tensor(TensorHandle t, const char* path);
TensorHandle torch_load_tensor(const char* path);

int torch_save_module(ModuleHandle m, const char* path);
ModuleHandle torch_load_module(const char* path);

/*
 * ===========================================
 * Device Management
 * ===========================================
 */

int torch_cuda_is_available(void);
int torch_cuda_device_count(void);
void torch_cuda_synchronize(void);
void torch_cuda_empty_cache(void);
size_t torch_cuda_memory_allocated(int device);
size_t torch_cuda_memory_reserved(int device);
void torch_cuda_set_device(int device);
int torch_cuda_current_device(void);

int torch_mps_is_available(void);

/*
 * ===========================================
 * Random Number Generation
 * ===========================================
 */

void torch_manual_seed(uint64_t seed);
void torch_cuda_manual_seed(uint64_t seed);
void torch_cuda_manual_seed_all(uint64_t seed);

/*
 * ===========================================
 * Utility
 * ===========================================
 */

/* Version info */
const char* torch_version(void);

/* Print tensor (for debugging) */
void torch_tensor_print(TensorHandle t);

/* String representation (caller must free with torch_free_string) */
char* torch_tensor_to_string(TensorHandle t);
void torch_free_string(char* s);

#ifdef __cplusplus
}
#endif

#endif /* LIBTORCH_C_API_H */
