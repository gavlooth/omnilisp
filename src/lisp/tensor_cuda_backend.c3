module lisp;

const int OMNI_TENSOR_CUDA_UNAVAILABLE = 0;
const int OMNI_TENSOR_CUDA_SUCCESS = 1;
const int OMNI_TENSOR_CUDA_INVALID = -2;
const int OMNI_TENSOR_CUDA_ALLOCATION_FAILED = -3;
const int OMNI_TENSOR_CUDA_COPY_FAILED = -4;
const int OMNI_TENSOR_CUDA_EXECUTION_FAILED = -5;
const int OMNI_TENSOR_CUDA_DOMAIN_ERROR = -6;
const int OMNI_TENSOR_CUDA_INVALID_ARGUMENT = -7;

extern fn int omni_tensor_backend_cuda_available() @extern("omni_tensor_backend_cuda_available");
extern fn void omni_tensor_backend_cuda_disable_for_tests(int disabled) @extern("omni_tensor_backend_cuda_disable_for_tests");
extern fn int omni_tensor_backend_cublas_available() @extern("omni_tensor_backend_cublas_available");
extern fn int omni_tensor_backend_cublas_float32_available() @extern("omni_tensor_backend_cublas_float32_available");
extern fn int omni_tensor_backend_cublas_complex_available() @extern("omni_tensor_backend_cublas_complex_available");
extern fn void omni_tensor_backend_cublas_disable_for_tests(int disabled) @extern("omni_tensor_backend_cublas_disable_for_tests");
extern fn int omni_tensor_backend_cusolver_available() @extern("omni_tensor_backend_cusolver_available");
extern fn int omni_tensor_backend_cusolver_complex_svd_available() @extern("omni_tensor_backend_cusolver_complex_svd_available");
extern fn void omni_tensor_backend_cusolver_disable_for_tests(int disabled) @extern("omni_tensor_backend_cusolver_disable_for_tests");
extern fn long omni_tensor_backend_cusolver_zgesvd_call_count() @extern("omni_tensor_backend_cusolver_zgesvd_call_count");
extern fn long omni_tensor_backend_cusolver_cgesvd_call_count() @extern("omni_tensor_backend_cusolver_cgesvd_call_count");
extern fn int omni_tensor_backend_cuda_grid_dim_oversized_guard_for_tests() @extern("omni_tensor_backend_cuda_grid_dim_oversized_guard_for_tests");
extern fn int omni_tensor_backend_cuda_map_available() @extern("omni_tensor_backend_cuda_map_available");
extern fn int omni_tensor_backend_cuda_scientific_map_available() @extern("omni_tensor_backend_cuda_scientific_map_available");
extern fn int omni_tensor_backend_cuda_complex_map_available() @extern("omni_tensor_backend_cuda_complex_map_available");
extern fn int omni_tensor_backend_cuda_complex_map_status_priority_for_tests() @extern("omni_tensor_backend_cuda_complex_map_status_priority_for_tests");
extern fn int omni_tensor_backend_cuda_complex_matrix_available() @extern("omni_tensor_backend_cuda_complex_matrix_available");
extern fn int omni_tensor_backend_cuda_rounding_i64_available() @extern("omni_tensor_backend_cuda_rounding_i64_available");
extern fn int omni_tensor_backend_cuda_ml_optimizer_available() @extern("omni_tensor_backend_cuda_ml_optimizer_available");
extern fn int omni_tensor_backend_cuda_ml_sgd_byte_len_overflow_guard_for_tests() @extern("omni_tensor_backend_cuda_ml_sgd_byte_len_overflow_guard_for_tests");
extern fn int omni_tensor_backend_cuda_ml_stateful_byte_len_overflow_guard_for_tests() @extern("omni_tensor_backend_cuda_ml_stateful_byte_len_overflow_guard_for_tests");
extern fn int omni_tensor_backend_cuda_svd_adapter_shape_guard_for_tests() @extern("omni_tensor_backend_cuda_svd_adapter_shape_guard_for_tests");
extern fn int omni_tensor_backend_cuda_alloc(
    usz byte_len,
    void** out_device_ptr
) @extern("omni_tensor_backend_cuda_alloc");
extern fn int omni_tensor_backend_cuda_copy_to_device(
    void* host_ptr,
    usz byte_len,
    void** out_device_ptr
) @extern("omni_tensor_backend_cuda_copy_to_device");
extern fn int omni_tensor_backend_cuda_copy_to_host(
    void* device_ptr,
    usz byte_len,
    void* host_ptr
) @extern("omni_tensor_backend_cuda_copy_to_host");
extern fn int omni_tensor_backend_cuda_copy_to_existing_device(
    void* host_ptr,
    usz byte_len,
    void* device_ptr
) @extern("omni_tensor_backend_cuda_copy_to_existing_device");
extern fn int omni_tensor_backend_cuda_copy_device_to_existing_device(
    void* source_device_ptr,
    usz byte_len,
    void* dest_device_ptr
) @extern("omni_tensor_backend_cuda_copy_device_to_existing_device");
extern fn int omni_tensor_backend_cuda_copy_null_alias_guard_for_tests() @extern("omni_tensor_backend_cuda_copy_null_alias_guard_for_tests");
extern fn int omni_tensor_backend_cuda_fill_f64(
    void* device_ptr,
    usz element_count,
    double value
) @extern("omni_tensor_backend_cuda_fill_f64");
extern fn int omni_tensor_backend_cuda_fill_f32(
    void* device_ptr,
    usz element_count,
    float value
) @extern("omni_tensor_backend_cuda_fill_f32");
extern fn int omni_tensor_backend_cuda_map_f64(
    void* left_device,
    void* right_device,
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
    void** out_device
) @extern("omni_tensor_backend_cuda_map_f64");
extern fn int omni_tensor_backend_cuda_map_f32(
    void* left_device,
    void* right_device,
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
    void** out_device
) @extern("omni_tensor_backend_cuda_map_f32");
extern fn int omni_tensor_backend_cuda_ml_sgd_f32(
    void* params_device,
    void* grads_device,
    void* velocity_device,
    usz byte_len,
    usz element_count,
    float learning_rate,
    float momentum,
    float weight_decay,
    uint has_velocity,
    void** out_params_device,
    void** out_velocity_device
) @extern("omni_tensor_backend_cuda_ml_sgd_f32");
extern fn int omni_tensor_backend_cuda_ml_adam_f32(
    void* params_device,
    void* grads_device,
    void* first_device,
    void* second_device,
    usz byte_len,
    usz element_count,
    float learning_rate,
    float beta1,
    float beta2,
    float epsilon,
    float weight_decay,
    float first_correction,
    float second_correction,
    uint decoupled_weight_decay,
    uint has_moments,
    void** out_params_device,
    void** out_first_device,
    void** out_second_device
) @extern("omni_tensor_backend_cuda_ml_adam_f32");
extern fn int omni_tensor_backend_cuda_ml_rmsprop_f32(
    void* params_device,
    void* grads_device,
    void* square_device,
    void* velocity_device,
    usz byte_len,
    usz element_count,
    float learning_rate,
    float alpha,
    float epsilon,
    float momentum,
    float weight_decay,
    uint has_square,
    uint has_velocity,
    void** out_params_device,
    void** out_square_device,
    void** out_velocity_device
) @extern("omni_tensor_backend_cuda_ml_rmsprop_f32");
extern fn int omni_tensor_backend_cuda_map_complex128(
    void* left_device,
    void* right_device,
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
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex128");
extern fn int omni_tensor_backend_cuda_map_complex64(
    void* left_device,
    void* right_device,
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
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex64");
extern fn int omni_tensor_backend_cuda_map_unary_f64(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_unary_f64");
extern fn int omni_tensor_backend_cuda_map_unary_f32(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_unary_f32");
extern fn int omni_tensor_backend_cuda_map_complex128_unary(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex128_unary");
extern fn int omni_tensor_backend_cuda_map_complex64_unary(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex64_unary");
extern fn int omni_tensor_backend_cuda_map_complex128_to_real(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex128_to_real");
extern fn int omni_tensor_backend_cuda_map_complex64_to_real(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_complex64_to_real");
extern fn int omni_tensor_backend_cuda_map_scientific_unary_f64(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_scientific_unary_f64");
extern fn int omni_tensor_backend_cuda_map_scientific_unary_f32(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    void** out_device
) @extern("omni_tensor_backend_cuda_map_scientific_unary_f32");
extern fn int omni_tensor_backend_cuda_round_i64_f64(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    long* host_out
) @extern("omni_tensor_backend_cuda_round_i64_f64");
extern fn int omni_tensor_backend_cuda_round_i64_f32(
    void* input_device,
    usz byte_len,
    usz element_count,
    uint op,
    long* host_out
) @extern("omni_tensor_backend_cuda_round_i64_f32");
extern fn int omni_tensor_backend_cuda_transpose_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_transpose_complex128");
extern fn int omni_tensor_backend_cuda_transpose_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_transpose_complex64");
extern fn int omni_tensor_backend_cuda_diagonal_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_diagonal_complex128");
extern fn int omni_tensor_backend_cuda_diagonal_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_diagonal_complex64");
extern fn int omni_tensor_backend_cuda_diagonal_matrix_complex128(
    void* input_device,
    usz byte_len,
    usz n,
    void** out_device
) @extern("omni_tensor_backend_cuda_diagonal_matrix_complex128");
extern fn int omni_tensor_backend_cuda_diagonal_matrix_complex64(
    void* input_device,
    usz byte_len,
    usz n,
    void** out_device
) @extern("omni_tensor_backend_cuda_diagonal_matrix_complex64");
extern fn int omni_tensor_backend_cuda_trace_complex128(
    void* input_device,
    usz byte_len,
    usz n,
    double* out_real,
    double* out_imag
) @extern("omni_tensor_backend_cuda_trace_complex128");
extern fn int omni_tensor_backend_cuda_trace_complex64(
    void* input_device,
    usz byte_len,
    usz n,
    float* out_real,
    float* out_imag
) @extern("omni_tensor_backend_cuda_trace_complex64");
extern fn int omni_tensor_backend_cuda_svd_input_column_major_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_input_column_major_complex128");
extern fn int omni_tensor_backend_cuda_svd_input_column_major_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_input_column_major_complex64");
extern fn int omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex128");
extern fn int omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex64");
extern fn int omni_tensor_backend_cuda_svd_u_row_major_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz k,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_u_row_major_complex128");
extern fn int omni_tensor_backend_cuda_svd_u_row_major_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz k,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_u_row_major_complex64");
extern fn int omni_tensor_backend_cuda_svd_v_from_vt_complex128(
    void* input_device,
    usz byte_len,
    usz k,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_v_from_vt_complex128");
extern fn int omni_tensor_backend_cuda_svd_v_from_vt_complex64(
    void* input_device,
    usz byte_len,
    usz k,
    usz cols,
    void** out_device
) @extern("omni_tensor_backend_cuda_svd_v_from_vt_complex64");
extern fn int omni_tensor_backend_cuda_svd_complex128(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_u_device,
    void** out_s_device,
    void** out_v_device
) @extern("omni_tensor_backend_cuda_svd_complex128");
extern fn int omni_tensor_backend_cuda_svd_complex64(
    void* input_device,
    usz byte_len,
    usz rows,
    usz cols,
    void** out_u_device,
    void** out_s_device,
    void** out_v_device
) @extern("omni_tensor_backend_cuda_svd_complex64");
extern fn void omni_tensor_backend_cuda_free(void* device_ptr) @extern("omni_tensor_backend_cuda_free");
extern fn int omni_tensor_backend_cublas_dgemm_rowmajor(
    usz m,
    usz n,
    usz k,
    int trans_left,
    int trans_right,
    void* left_device,
    usz left_leading_dim,
    void* right_device,
    usz right_leading_dim,
    void* out_device,
    usz out_leading_dim
) @extern("omni_tensor_backend_cublas_dgemm_rowmajor");
extern fn int omni_tensor_backend_cublas_sgemm_rowmajor(
    usz m,
    usz n,
    usz k,
    int trans_left,
    int trans_right,
    void* left_device,
    usz left_leading_dim,
    void* right_device,
    usz right_leading_dim,
    void* out_device,
    usz out_leading_dim
) @extern("omni_tensor_backend_cublas_sgemm_rowmajor");
extern fn int omni_tensor_backend_cublas_dgemv_rowmajor(
    usz rows,
    usz cols,
    int trans_matrix,
    void* matrix_device,
    usz matrix_leading_dim,
    void* vector_device,
    void* out_device
) @extern("omni_tensor_backend_cublas_dgemv_rowmajor");
extern fn int omni_tensor_backend_cublas_sgemv_rowmajor(
    usz rows,
    usz cols,
    int trans_matrix,
    void* matrix_device,
    usz matrix_leading_dim,
    void* vector_device,
    void* out_device
) @extern("omni_tensor_backend_cublas_sgemv_rowmajor");
extern fn int omni_tensor_backend_cublas_zgemm_rowmajor(
    usz m,
    usz n,
    usz k,
    int trans_left,
    int trans_right,
    void* left_device,
    usz left_leading_dim,
    void* right_device,
    usz right_leading_dim,
    void* out_device,
    usz out_leading_dim
) @extern("omni_tensor_backend_cublas_zgemm_rowmajor");
extern fn int omni_tensor_backend_cublas_cgemm_rowmajor(
    usz m,
    usz n,
    usz k,
    int trans_left,
    int trans_right,
    void* left_device,
    usz left_leading_dim,
    void* right_device,
    usz right_leading_dim,
    void* out_device,
    usz out_leading_dim
) @extern("omni_tensor_backend_cublas_cgemm_rowmajor");
extern fn int omni_tensor_backend_cublas_zgemv_rowmajor(
    usz rows,
    usz cols,
    int trans_matrix,
    void* matrix_device,
    usz matrix_leading_dim,
    void* vector_device,
    void* out_device
) @extern("omni_tensor_backend_cublas_zgemv_rowmajor");
extern fn int omni_tensor_backend_cublas_cgemv_rowmajor(
    usz rows,
    usz cols,
    int trans_matrix,
    void* matrix_device,
    usz matrix_leading_dim,
    void* vector_device,
    void* out_device
) @extern("omni_tensor_backend_cublas_cgemv_rowmajor");
