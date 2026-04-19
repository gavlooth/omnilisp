module lisp;

const int OMNI_TENSOR_VULKAN_UNAVAILABLE = 0;
const int OMNI_TENSOR_VULKAN_SUCCESS = 1;
const int OMNI_TENSOR_VULKAN_INVALID = -2;
const int OMNI_TENSOR_VULKAN_ALLOCATION_FAILED = -3;
const int OMNI_TENSOR_VULKAN_COPY_FAILED = -4;
const int OMNI_TENSOR_VULKAN_UNSUPPORTED = -5;
const int OMNI_TENSOR_VULKAN_EXECUTION_FAILED = -6;
const int OMNI_TENSOR_VULKAN_SINGULAR = -7;
const int OMNI_TENSOR_VULKAN_NO_CONVERGENCE = -8;
const int OMNI_TENSOR_VULKAN_NOT_SYMMETRIC = -9;
const int OMNI_TENSOR_VULKAN_DOMAIN_ERROR = -10;
const int OMNI_TENSOR_VULKAN_INVALID_ARGUMENT = -11;

extern fn int omni_tensor_backend_vulkan_available() @extern("omni_tensor_backend_vulkan_available");
extern fn int omni_tensor_backend_vulkan_float64_available() @extern("omni_tensor_backend_vulkan_float64_available");
extern fn int omni_tensor_backend_vulkan_int64_available() @extern("omni_tensor_backend_vulkan_int64_available");
extern fn int omni_tensor_backend_vulkan_rounding_i64_available() @extern("omni_tensor_backend_vulkan_rounding_i64_available");
extern fn int omni_tensor_backend_vulkan_float32_available() @extern("omni_tensor_backend_vulkan_float32_available");
extern fn int omni_tensor_backend_vulkan_copy_to_device(
    void* host_ptr,
    usz byte_len,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_copy_to_device");
extern fn int omni_tensor_backend_vulkan_copy_to_host(
    void* device_ptr,
    usz byte_len,
    void* host_ptr
) @extern("omni_tensor_backend_vulkan_copy_to_host");
extern fn int omni_tensor_backend_vulkan_copy_to_existing_device(
    void* host_ptr,
    usz byte_len,
    void* device_ptr
) @extern("omni_tensor_backend_vulkan_copy_to_existing_device");
extern fn int omni_tensor_backend_vulkan_copy_device_to_existing_device(
    void* source_device_ptr,
    usz byte_len,
    void* dest_device_ptr
) @extern("omni_tensor_backend_vulkan_copy_device_to_existing_device");
extern fn int omni_tensor_backend_vulkan_fill_f64(
    void* device_ptr,
    usz element_count,
    double value
) @extern("omni_tensor_backend_vulkan_fill_f64");
extern fn int omni_tensor_backend_vulkan_fill_f32(
    void* device_ptr,
    usz element_count,
    float value
) @extern("omni_tensor_backend_vulkan_fill_f32");
extern fn int omni_tensor_backend_vulkan_map_add_scalar_f64(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    double scalar,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_add_scalar_f64");
extern fn int omni_tensor_backend_vulkan_map_f64(
    void* left_device_ptr,
    void* right_device_ptr,
    usz byte_len,
    usz element_count,
    double scalar,
    uint mode,
    uint op,
    usz out_rank,
    usz left_rank,
    usz right_rank,
    usz* out_shape,
    usz* out_strides,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_f64");
extern fn int omni_tensor_backend_vulkan_map_f32(
    void* left_device_ptr,
    void* right_device_ptr,
    usz byte_len,
    usz element_count,
    float scalar,
    uint mode,
    uint op,
    usz out_rank,
    usz left_rank,
    usz right_rank,
    usz* out_shape,
    usz* out_strides,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_f32");
extern fn int omni_tensor_backend_vulkan_map_complex128(
    void* left_device_ptr,
    void* right_device_ptr,
    usz byte_len,
    usz element_count,
    double scalar_real,
    double scalar_imag,
    uint mode,
    uint op,
    usz out_rank,
    usz left_rank,
    usz right_rank,
    usz* out_shape,
    usz* out_strides,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex128");
extern fn int omni_tensor_backend_vulkan_map_complex64(
    void* left_device_ptr,
    void* right_device_ptr,
    usz byte_len,
    usz element_count,
    float scalar_real,
    float scalar_imag,
    uint mode,
    uint op,
    usz out_rank,
    usz left_rank,
    usz right_rank,
    usz* out_shape,
    usz* out_strides,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex64");
extern fn int omni_tensor_backend_vulkan_map_unary_f64(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_unary_f64");
extern fn int omni_tensor_backend_vulkan_map_unary_f32(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_unary_f32");
extern fn int omni_tensor_backend_vulkan_map_complex128_unary(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex128_unary");
extern fn int omni_tensor_backend_vulkan_map_complex64_unary(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex64_unary");
extern fn int omni_tensor_backend_vulkan_map_complex128_to_real(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex128_to_real");
extern fn int omni_tensor_backend_vulkan_map_complex64_to_real(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_complex64_to_real");
extern fn int omni_tensor_backend_vulkan_map_normal_quantile_f32(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_normal_quantile_f32");
extern fn int omni_tensor_backend_vulkan_map_normal_quantile_f64(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_normal_quantile_f64");
extern fn int omni_tensor_backend_vulkan_round_i64_f64(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    long* host_out
) @extern("omni_tensor_backend_vulkan_round_i64_f64");
extern fn int omni_tensor_backend_vulkan_round_i64_f32(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    uint op,
    long* host_out
) @extern("omni_tensor_backend_vulkan_round_i64_f32");
extern fn int omni_tensor_backend_vulkan_contract_dot_f64(
    void* left_device_ptr,
    void* right_device_ptr,
    usz element_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_dot_f64");
extern fn int omni_tensor_backend_vulkan_contract_f64(
    void* left_device_ptr,
    void* right_device_ptr,
    usz left_rank,
    usz right_rank,
    usz axis_count,
    usz* left_axes,
    usz* right_axes,
    usz out_element_count,
    usz out_rank,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    usz* out_shape,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_f64");
extern fn int omni_tensor_backend_vulkan_contract_f32(
    void* left_device_ptr,
    void* right_device_ptr,
    usz left_rank,
    usz right_rank,
    usz axis_count,
    usz* left_axes,
    usz* right_axes,
    usz out_element_count,
    usz out_rank,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    usz* out_shape,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_f32");
extern fn int omni_tensor_backend_vulkan_contract_complex128(
    void* left_device_ptr,
    void* right_device_ptr,
    usz left_rank,
    usz right_rank,
    usz axis_count,
    usz* left_axes,
    usz* right_axes,
    usz out_element_count,
    usz out_rank,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    usz* out_shape,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_complex128");
extern fn int omni_tensor_backend_vulkan_contract_complex64(
    void* left_device_ptr,
    void* right_device_ptr,
    usz left_rank,
    usz right_rank,
    usz axis_count,
    usz* left_axes,
    usz* right_axes,
    usz out_element_count,
    usz out_rank,
    usz* left_shape,
    usz* left_strides,
    usz* right_shape,
    usz* right_strides,
    usz* out_shape,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_complex64");
extern fn int omni_tensor_backend_vulkan_transpose_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_transpose_f64");
extern fn int omni_tensor_backend_vulkan_transpose_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_transpose_f32");
extern fn int omni_tensor_backend_vulkan_transpose_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_transpose_complex128");
extern fn int omni_tensor_backend_vulkan_transpose_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_transpose_complex64");
extern fn int omni_tensor_backend_vulkan_diagonal_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_f64");
extern fn int omni_tensor_backend_vulkan_diagonal_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_f32");
extern fn int omni_tensor_backend_vulkan_diagonal_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_complex128");
extern fn int omni_tensor_backend_vulkan_diagonal_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_complex64");
extern fn int omni_tensor_backend_vulkan_diagonal_matrix_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_matrix_f64");
extern fn int omni_tensor_backend_vulkan_diagonal_matrix_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_matrix_f32");
extern fn int omni_tensor_backend_vulkan_diagonal_matrix_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_matrix_complex128");
extern fn int omni_tensor_backend_vulkan_diagonal_matrix_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_diagonal_matrix_complex64");
extern fn int omni_tensor_backend_vulkan_trace_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    double* out_trace
) @extern("omni_tensor_backend_vulkan_trace_f64");
extern fn int omni_tensor_backend_vulkan_trace_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    float* out_trace
) @extern("omni_tensor_backend_vulkan_trace_f32");
extern fn int omni_tensor_backend_vulkan_trace_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    double* out_real,
    double* out_imag
) @extern("omni_tensor_backend_vulkan_trace_complex128");
extern fn int omni_tensor_backend_vulkan_trace_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    float* out_real,
    float* out_imag
) @extern("omni_tensor_backend_vulkan_trace_complex64");
extern fn int omni_tensor_backend_vulkan_norm_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    double* out_norm
) @extern("omni_tensor_backend_vulkan_norm_f64");
extern fn int omni_tensor_backend_vulkan_norm_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    float* out_norm
) @extern("omni_tensor_backend_vulkan_norm_f32");
extern fn int omni_tensor_backend_vulkan_norm_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    double* out_norm
) @extern("omni_tensor_backend_vulkan_norm_complex128");
extern fn int omni_tensor_backend_vulkan_norm_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    float* out_norm
) @extern("omni_tensor_backend_vulkan_norm_complex64");
extern fn int omni_tensor_backend_vulkan_rank_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    double tolerance,
    double* out_rank
) @extern("omni_tensor_backend_vulkan_rank_f64");
extern fn int omni_tensor_backend_vulkan_rank_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    float tolerance,
    float* out_rank
) @extern("omni_tensor_backend_vulkan_rank_f32");
extern fn int omni_tensor_backend_vulkan_rank_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    double tolerance,
    double* out_rank
) @extern("omni_tensor_backend_vulkan_rank_complex128");
extern fn int omni_tensor_backend_vulkan_rank_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    float tolerance,
    float* out_rank
) @extern("omni_tensor_backend_vulkan_rank_complex64");
extern fn int omni_tensor_backend_vulkan_determinant_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    double* out_determinant
) @extern("omni_tensor_backend_vulkan_determinant_f64");
extern fn int omni_tensor_backend_vulkan_determinant_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    float* out_determinant
) @extern("omni_tensor_backend_vulkan_determinant_f32");
extern fn int omni_tensor_backend_vulkan_determinant_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    double* out_real,
    double* out_imag
) @extern("omni_tensor_backend_vulkan_determinant_complex128");
extern fn int omni_tensor_backend_vulkan_determinant_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    float* out_real,
    float* out_imag
) @extern("omni_tensor_backend_vulkan_determinant_complex64");
extern fn int omni_tensor_backend_vulkan_lu_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    usz* out_pivots,
    usz* out_swap_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_lu_f64");
extern fn int omni_tensor_backend_vulkan_lu_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    usz* out_pivots,
    usz* out_swap_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_lu_f32");
extern fn int omni_tensor_backend_vulkan_lu_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    usz* out_pivots,
    usz* out_swap_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_lu_complex128");
extern fn int omni_tensor_backend_vulkan_lu_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    usz* out_pivots,
    usz* out_swap_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_lu_complex64");
extern fn int omni_tensor_backend_vulkan_inverse_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_inverse_f64");
extern fn int omni_tensor_backend_vulkan_inverse_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_inverse_f32");
extern fn int omni_tensor_backend_vulkan_inverse_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_inverse_complex128");
extern fn int omni_tensor_backend_vulkan_inverse_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_inverse_complex64");
extern fn int omni_tensor_backend_vulkan_solve_f64(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_f64");
extern fn int omni_tensor_backend_vulkan_solve_f32(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_f32");
extern fn int omni_tensor_backend_vulkan_solve_complex128(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_complex128");
extern fn int omni_tensor_backend_vulkan_solve_complex64(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_complex64");
extern fn int omni_tensor_backend_vulkan_solve_parallel_f64(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_parallel_f64");
extern fn int omni_tensor_backend_vulkan_solve_parallel_f32(
    void* coefficients_device_ptr,
    usz coefficients_byte_len,
    void* rhs_device_ptr,
    usz rhs_byte_len,
    usz n,
    usz rhs_cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_solve_parallel_f32");
extern fn long omni_tensor_backend_vulkan_solve_parallel_call_count() @extern("omni_tensor_backend_vulkan_solve_parallel_call_count");
extern fn long omni_tensor_backend_vulkan_solve_multi_dispatch_call_count() @extern("omni_tensor_backend_vulkan_solve_multi_dispatch_call_count");
extern fn long omni_tensor_backend_vulkan_solve_pivot_reduce_call_count() @extern("omni_tensor_backend_vulkan_solve_pivot_reduce_call_count");
extern fn long omni_tensor_backend_vulkan_solve_factor_stage_call_count() @extern("omni_tensor_backend_vulkan_solve_factor_stage_call_count");
extern fn long omni_tensor_backend_vulkan_solve_serial_call_count() @extern("omni_tensor_backend_vulkan_solve_serial_call_count");
extern fn long omni_tensor_backend_vulkan_solve_parallel_f32_call_count() @extern("omni_tensor_backend_vulkan_solve_parallel_f32_call_count");
extern fn long omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count() @extern("omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count");
extern fn long omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count() @extern("omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count");
extern fn long omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count() @extern("omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count");
extern fn long omni_tensor_backend_vulkan_solve_serial_f32_call_count() @extern("omni_tensor_backend_vulkan_solve_serial_f32_call_count");
extern fn long omni_tensor_backend_vulkan_solve_parallel_f64_call_count() @extern("omni_tensor_backend_vulkan_solve_parallel_f64_call_count");
extern fn long omni_tensor_backend_vulkan_solve_multi_dispatch_f64_call_count() @extern("omni_tensor_backend_vulkan_solve_multi_dispatch_f64_call_count");
extern fn long omni_tensor_backend_vulkan_solve_pivot_reduce_f64_call_count() @extern("omni_tensor_backend_vulkan_solve_pivot_reduce_f64_call_count");
extern fn long omni_tensor_backend_vulkan_solve_factor_stage_f64_call_count() @extern("omni_tensor_backend_vulkan_solve_factor_stage_f64_call_count");
extern fn long omni_tensor_backend_vulkan_solve_serial_f64_call_count() @extern("omni_tensor_backend_vulkan_solve_serial_f64_call_count");
extern fn int omni_tensor_backend_vulkan_cholesky_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_cholesky_f64");
extern fn int omni_tensor_backend_vulkan_cholesky_f32(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_cholesky_f32");
extern fn int omni_tensor_backend_vulkan_cholesky_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_cholesky_complex128");
extern fn int omni_tensor_backend_vulkan_cholesky_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_cholesky_complex64");
extern fn int omni_tensor_backend_vulkan_qr_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) @extern("omni_tensor_backend_vulkan_qr_f64");
extern fn int omni_tensor_backend_vulkan_qr_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) @extern("omni_tensor_backend_vulkan_qr_f32");
extern fn int omni_tensor_backend_vulkan_qr_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) @extern("omni_tensor_backend_vulkan_qr_complex128");
extern fn int omni_tensor_backend_vulkan_qr_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) @extern("omni_tensor_backend_vulkan_qr_complex64");
extern fn int omni_tensor_backend_vulkan_singular_values_status_payload_probe_for_tests() @extern("omni_tensor_backend_vulkan_singular_values_status_payload_probe_for_tests");
extern fn int omni_tensor_backend_vulkan_singular_values_oversized_index_guard_for_tests() @extern("omni_tensor_backend_vulkan_singular_values_oversized_index_guard_for_tests");
extern fn int omni_tensor_backend_vulkan_singular_values_complex_iteration_guard_for_tests() @extern("omni_tensor_backend_vulkan_singular_values_complex_iteration_guard_for_tests");
extern fn int omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests() @extern("omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests");
extern fn int omni_tensor_backend_vulkan_singular_values_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_singular_values_f64");
extern fn int omni_tensor_backend_vulkan_singular_values_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_singular_values_f32");
extern fn int omni_tensor_backend_vulkan_singular_values_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_singular_values_complex128");
extern fn int omni_tensor_backend_vulkan_singular_values_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_singular_values_complex64");
extern fn int omni_tensor_backend_vulkan_svd_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) @extern("omni_tensor_backend_vulkan_svd_f64");
extern fn int omni_tensor_backend_vulkan_svd_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) @extern("omni_tensor_backend_vulkan_svd_f32");
extern fn int omni_tensor_backend_vulkan_symmetric_eigen_f64(
    void* input_device_ptr,
    usz byte_len,
    usz n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) @extern("omni_tensor_backend_vulkan_symmetric_eigen_f64");
extern fn int omni_tensor_backend_vulkan_singular_norm_f64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    double* out_norm
) @extern("omni_tensor_backend_vulkan_singular_norm_f64");
extern fn int omni_tensor_backend_vulkan_singular_norm_f32(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    float* out_norm
) @extern("omni_tensor_backend_vulkan_singular_norm_f32");
extern fn int omni_tensor_backend_vulkan_singular_norm_complex128(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    double* out_norm
) @extern("omni_tensor_backend_vulkan_singular_norm_complex128");
extern fn int omni_tensor_backend_vulkan_singular_norm_complex64(
    void* input_device_ptr,
    usz byte_len,
    usz rows,
    usz cols,
    uint mode,
    float* out_norm
) @extern("omni_tensor_backend_vulkan_singular_norm_complex64");
extern fn void omni_tensor_backend_vulkan_free(void* device_ptr) @extern("omni_tensor_backend_vulkan_free");
extern fn void* omni_tensor_backend_vulkan_retain(void* device_ptr) @extern("omni_tensor_backend_vulkan_retain");
