module lisp;

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
