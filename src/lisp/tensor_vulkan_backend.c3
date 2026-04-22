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
extern fn int omni_tensor_backend_vulkan_ml_reduction_f64(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz axis_count,
    usz* axes,
    usz out_element_count,
    usz out_rank,
    usz* input_shape,
    usz* input_strides,
    usz* out_shape,
    usz* out_strides,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_reduction_f64");
extern fn int omni_tensor_backend_vulkan_ml_reduction_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz axis_count,
    usz* axes,
    usz out_element_count,
    usz out_rank,
    usz* input_shape,
    usz* input_strides,
    usz* out_shape,
    usz* out_strides,
    uint op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_reduction_f32");
extern fn int omni_tensor_backend_vulkan_ml_mse_f64(
    void* predictions_device_ptr,
    usz predictions_byte_len,
    void* targets_device_ptr,
    usz targets_byte_len,
    usz element_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_mse_f64");
extern fn int omni_tensor_backend_vulkan_ml_mse_f32(
    void* predictions_device_ptr,
    usz predictions_byte_len,
    void* targets_device_ptr,
    usz targets_byte_len,
    usz element_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_mse_f32");
extern fn int omni_tensor_backend_vulkan_ml_cross_entropy_f32(
    void* logits_device_ptr,
    usz logits_byte_len,
    void* targets_device_ptr,
    usz targets_byte_len,
    usz element_count,
    usz input_rank,
    usz axis,
    usz* input_shape,
    usz* logits_strides,
    usz* targets_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_cross_entropy_f32");
extern fn int omni_tensor_backend_vulkan_ml_conv1d_f32(
    void* input_device_ptr,
    usz input_byte_len,
    void* kernel_device_ptr,
    usz kernel_byte_len,
    usz batch,
    usz in_channels,
    usz width,
    usz out_channels,
    usz kernel_width,
    usz stride,
    usz padding,
    usz dilation,
    usz groups,
    usz out_width,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_conv1d_f32");
extern fn int omni_tensor_backend_vulkan_ml_conv2d_f32(
    void* input_device_ptr,
    usz input_byte_len,
    void* kernel_device_ptr,
    usz kernel_byte_len,
    usz batch,
    usz in_channels,
    usz height,
    usz width,
    usz out_channels,
    usz kernel_height,
    usz kernel_width,
    usz stride_height,
    usz stride_width,
    usz padding_height,
    usz padding_width,
    usz dilation_height,
    usz dilation_width,
    usz groups,
    usz out_height,
    usz out_width,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_conv2d_f32");
extern fn int omni_tensor_backend_vulkan_ml_pool2d_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz batch,
    usz channels,
    usz height,
    usz width,
    usz kernel_height,
    usz kernel_width,
    usz stride_height,
    usz stride_width,
    usz padding_height,
    usz padding_width,
    usz out_height,
    usz out_width,
    usz op,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_pool2d_f32");
extern fn int omni_tensor_backend_vulkan_ml_softmax_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz axis,
    usz* input_shape,
    usz* input_strides,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_softmax_f32");
extern fn int omni_tensor_backend_vulkan_ml_sgd_f32(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* velocity_device_ptr,
    usz velocity_byte_len,
    usz element_count,
    float learning_rate,
    float momentum,
    float weight_decay,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_velocity_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_sgd_f32");
extern fn int omni_tensor_backend_vulkan_ml_adam_f32(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* first_device_ptr,
    usz first_byte_len,
    void* second_device_ptr,
    usz second_byte_len,
    usz element_count,
    float learning_rate,
    float beta1,
    float beta2,
    float epsilon,
    float weight_decay,
    float first_correction,
    float second_correction,
    int decoupled_weight_decay,
    int has_moments,
    void** out_params_device_ptr,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_adam_f32");
extern fn int omni_tensor_backend_vulkan_ml_rmsprop_f32(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* square_device_ptr,
    usz square_byte_len,
    void* velocity_device_ptr,
    usz velocity_byte_len,
    usz element_count,
    float learning_rate,
    float alpha,
    float epsilon,
    float momentum,
    float weight_decay,
    int has_square,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_square_device_ptr,
    void** out_velocity_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_rmsprop_f32");
extern fn int omni_tensor_backend_vulkan_ml_sgd_f64(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* velocity_device_ptr,
    usz velocity_byte_len,
    usz element_count,
    double learning_rate,
    double momentum,
    double weight_decay,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_velocity_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_sgd_f64");
extern fn int omni_tensor_backend_vulkan_ml_adam_f64(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* first_device_ptr,
    usz first_byte_len,
    void* second_device_ptr,
    usz second_byte_len,
    usz element_count,
    double learning_rate,
    double beta1,
    double beta2,
    double epsilon,
    double weight_decay,
    double first_correction,
    double second_correction,
    int decoupled_weight_decay,
    int has_moments,
    void** out_params_device_ptr,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_adam_f64");
extern fn int omni_tensor_backend_vulkan_ml_rmsprop_f64(
    void* params_device_ptr,
    usz params_byte_len,
    void* grads_device_ptr,
    usz grads_byte_len,
    void* square_device_ptr,
    usz square_byte_len,
    void* velocity_device_ptr,
    usz velocity_byte_len,
    usz element_count,
    double learning_rate,
    double alpha,
    double epsilon,
    double momentum,
    double weight_decay,
    int has_square,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_square_device_ptr,
    void** out_velocity_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_rmsprop_f64");
extern fn int omni_tensor_backend_vulkan_ml_clip_sum_squares_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    float* out_sum
) @extern("omni_tensor_backend_vulkan_ml_clip_sum_squares_f32");
extern fn int omni_tensor_backend_vulkan_ml_clip_scale_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    float scale,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_clip_scale_f32");
extern fn int omni_tensor_backend_vulkan_kernel_source_scale_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    float scale,
    uint local_size,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_kernel_source_scale_f32");
extern fn int omni_tensor_backend_vulkan_kernel_source_scale_f32_spirv(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    float scale,
    uint local_size,
    uint* shader_words,
    usz shader_word_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_kernel_source_scale_f32_spirv");
extern fn int omni_tensor_backend_vulkan_kernel_source_unary_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    uint op,
    uint local_size,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_kernel_source_unary_f32");
extern fn int omni_tensor_backend_vulkan_kernel_source_unary_f32_spirv(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    uint op,
    uint local_size,
    uint* shader_words,
    usz shader_word_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_kernel_source_unary_f32_spirv");
extern fn int omni_tensor_backend_vulkan_kernel_source_binary_f32_spirv(
    void* left_device_ptr,
    usz left_byte_len,
    void* right_device_ptr,
    usz right_byte_len,
    usz element_count,
    uint local_size,
    uint* shader_words,
    usz shader_word_count,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_kernel_source_binary_f32_spirv");
extern fn int omni_tensor_backend_vulkan_ml_layer_norm_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz axis,
    float epsilon,
    usz* input_shape,
    usz* input_strides,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_layer_norm_f32");
extern fn int omni_tensor_backend_vulkan_ml_layer_norm_f64(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz axis,
    double epsilon,
    usz* input_shape,
    usz* input_strides,
    usz* out_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_layer_norm_f64");
extern fn int omni_tensor_backend_vulkan_ml_batch_norm_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz channel_axis,
    float epsilon,
    usz* input_shape,
    usz* input_strides,
    usz* out_strides,
    void* scale_device_ptr,
    usz scale_byte_len,
    void* bias_device_ptr,
    usz bias_byte_len,
    void* mean_device_ptr,
    usz mean_byte_len,
    void* variance_device_ptr,
    usz variance_byte_len,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_batch_norm_f32");
extern fn int omni_tensor_backend_vulkan_ml_batch_norm_f64(
    void* input_device_ptr,
    usz input_byte_len,
    usz input_element_count,
    usz input_rank,
    usz channel_axis,
    double epsilon,
    usz* input_shape,
    usz* input_strides,
    usz* out_strides,
    void* scale_device_ptr,
    usz scale_byte_len,
    void* bias_device_ptr,
    usz bias_byte_len,
    void* mean_device_ptr,
    usz mean_byte_len,
    void* variance_device_ptr,
    usz variance_byte_len,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_batch_norm_f64");
extern fn int omni_tensor_backend_vulkan_ml_attention_f32(
    void* query_device_ptr,
    usz query_byte_len,
    void* key_device_ptr,
    usz key_byte_len,
    void* value_device_ptr,
    usz value_byte_len,
    void* mask_device_ptr,
    usz mask_byte_len,
    usz batch_count,
    usz query_len,
    usz key_len,
    usz head_dim,
    usz value_dim,
    uint mask_kind,
    float scale,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_ml_attention_f32");
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
