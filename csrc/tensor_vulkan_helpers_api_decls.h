#ifndef OMNI_TENSOR_VULKAN_HELPERS_API_DECLS_H
#define OMNI_TENSOR_VULKAN_HELPERS_API_DECLS_H

int omni_tensor_vulkan_create_compute_pipeline_for_shader(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const uint32_t* shader_words,
    size_t shader_size,
    OmniVulkanShaderModule* out_shader_module,
    OmniVulkanPipeline* out_pipeline
);
int omni_tensor_vulkan_create_storage_descriptor_set_layout(
    OmniVulkanDevice device,
    uint32_t binding_count,
    OmniTensorVulkanDescriptorResources* resources
);
int omni_tensor_vulkan_allocate_storage_descriptor_set(
    OmniVulkanDevice device,
    const OmniTensorVulkanStorageBufferDescriptor* descriptors,
    uint32_t descriptor_count,
    OmniTensorVulkanDescriptorResources* resources
);
void omni_tensor_vulkan_destroy_storage_descriptor_resources(
    OmniVulkanDevice device,
    OmniTensorVulkanDescriptorResources* resources
);
int omni_tensor_vulkan_record_submit_single_dispatch(
    OmniVulkanDevice device,
    OmniVulkanQueue queue,
    uint32_t queue_family_index,
    OmniVulkanPipeline pipeline,
    OmniVulkanPipelineLayout pipeline_layout,
    OmniVulkanDescriptorSet descriptor_set,
    const void* push_data,
    uint32_t push_size,
    uint32_t group_count
);
int omni_tensor_backend_vulkan_map_chain2_scalar_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    float first_scalar,
    uint32_t first_mode,
    uint32_t first_op,
    float second_scalar,
    uint32_t second_mode,
    uint32_t second_op,
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    void** out_device_ptr
);
long omni_tensor_backend_vulkan_map_chain2_dispatch_call_count(void);
int omni_tensor_backend_vulkan_tail_status_from_payload_f64(double status_payload);
int omni_tensor_backend_vulkan_tail_status_from_payload_f32(float status_payload);
int omni_tensor_backend_vulkan_read_status_code_u32(void* status_device);
int omni_tensor_backend_vulkan_read_tail_status_f64(
    void* output_device,
    size_t status_offset_bytes
);
int omni_tensor_backend_vulkan_read_tail_status_f32(
    void* output_device,
    size_t status_offset_bytes
);
int omni_tensor_backend_vulkan_read_singular_values_status_f64(
    void* output_device,
    size_t status_offset_bytes
);
int omni_tensor_backend_vulkan_read_singular_values_status_f32(
    void* output_device,
    size_t status_offset_bytes
);
int omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(
    void* output_device,
    size_t status_offset_bytes
);
int omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t scalar_size,
    size_t* out_k,
    size_t* out_output_storage_count
);
int omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_output_storage_count
);
int omni_tensor_backend_vulkan_singular_values_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_singular_values_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_singular_values_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_read_tail_status_complex128(
    void* device_ptr,
    size_t byte_offset
);
int omni_tensor_backend_vulkan_read_tail_status_complex64(
    void* device_ptr,
    size_t byte_offset
);
int omni_tensor_vulkan_silence_stderr_begin(int* saved_stderr);
void omni_tensor_vulkan_silence_stderr_end(int saved_stderr);
int omni_tensor_backend_vulkan_available(void);
int omni_tensor_backend_vulkan_float64_available(void);
int omni_tensor_backend_vulkan_int64_available(void);
int omni_tensor_backend_vulkan_float32_available(void);
int omni_tensor_backend_vulkan_rounding_i64_available(void);
int omni_tensor_backend_vulkan_map_normal_quantile_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_map_normal_quantile_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_round_i64_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
);
int omni_tensor_backend_vulkan_round_i64_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
);
void omni_tensor_backend_vulkan_context_retain(OmniTensorVulkanContext* context);
void omni_tensor_backend_vulkan_context_release(OmniTensorVulkanContext* context);
void omni_tensor_backend_vulkan_destroy_buffer_handle(OmniTensorVulkanBuffer* handle);
int omni_tensor_backend_vulkan_get_shared_context(OmniTensorVulkanContext** out_context);
int omni_tensor_backend_vulkan_create_buffer_on_context(
    OmniTensorVulkanContext* context,
    size_t byte_len,
    OmniTensorVulkanBuffer** out_handle
);
int omni_tensor_backend_vulkan_create_buffer(size_t byte_len, OmniTensorVulkanBuffer** out_handle);
int omni_tensor_backend_vulkan_copy_to_device(const void* host_ptr, size_t byte_len, void** out_device_ptr);
int omni_tensor_backend_vulkan_copy_to_host(const void* device_ptr, size_t byte_len, void* host_ptr);
int omni_tensor_backend_vulkan_copy_to_existing_device(const void* host_ptr, size_t byte_len, void* device_ptr);
int omni_tensor_backend_vulkan_copy_device_to_existing_device(const void* source_device_ptr, size_t byte_len, void* dest_device_ptr);
int omni_tensor_backend_vulkan_fill_f64(void* device_ptr, size_t element_count, double value);
int omni_tensor_backend_vulkan_fill_f32(void* device_ptr, size_t element_count, float value);
void omni_tensor_backend_vulkan_free(void* device_ptr);
void* omni_tensor_backend_vulkan_retain(void* device_ptr);
int omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_clip_sum_squares_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float* out_sum
);
int omni_tensor_backend_vulkan_ml_clip_scale_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float scale,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_layer_norm_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis,
    float epsilon,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_strides,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_batch_norm_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t channel_axis,
    float epsilon,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_strides,
    const void* scale_device_ptr,
    size_t scale_byte_len,
    const void* bias_device_ptr,
    size_t bias_byte_len,
    const void* mean_device_ptr,
    size_t mean_byte_len,
    const void* variance_device_ptr,
    size_t variance_byte_len,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_attention_f32(
    const void* query_device_ptr,
    size_t query_byte_len,
    const void* key_device_ptr,
    size_t key_byte_len,
    const void* value_device_ptr,
    size_t value_byte_len,
    const void* mask_device_ptr,
    size_t mask_byte_len,
    size_t batch_count,
    size_t query_len,
    size_t key_len,
    size_t head_dim,
    size_t value_dim,
    unsigned int mask_kind,
    float scale,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** first_out_device_ptr,
    void** second_out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t third_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** first_out_device_ptr,
    void** second_out_device_ptr,
    void** third_out_device_ptr
);
int omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t output_byte_len,
    size_t status_byte_len,
    size_t init_work_item_count,
    size_t element_size,
    size_t n,
    size_t rhs_cols,
    size_t result_count,
    uint32_t local_size,
    int require_float64,
    const uint32_t* init_spv,
    size_t init_spv_size,
    const uint32_t* pivot_spv,
    size_t pivot_spv_size,
    const uint32_t* pivot_reduce_spv,
    size_t pivot_reduce_spv_size,
    const uint32_t* pivot_commit_spv,
    size_t pivot_commit_spv_size,
    const uint32_t* row_swap_spv,
    size_t row_swap_spv_size,
    const uint32_t* factor_spv,
    size_t factor_spv_size,
    const uint32_t* eliminate_spv,
    size_t eliminate_spv_size,
    const uint32_t* backsolve_spv,
    size_t backsolve_spv_size,
    long* pivot_reduce_counter,
    long* factor_stage_counter,
    void** out_output_device_ptr,
    void** out_status_device_ptr
);
int omni_tensor_backend_vulkan_ml_reduction_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis_count,
    const size_t* axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    uint32_t op,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_reduction_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis_count,
    const size_t* axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    uint32_t op,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_mse_f64(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_mse_f32(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_cross_entropy_f32(
    const void* logits_device_ptr,
    size_t logits_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    size_t input_rank,
    size_t axis,
    const size_t* input_shape,
    const size_t* logits_strides,
    const size_t* targets_strides,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_conv1d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* kernel_device_ptr,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t width,
    size_t out_channels,
    size_t kernel_width,
    size_t stride,
    size_t padding,
    size_t dilation,
    size_t groups,
    size_t out_width,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_conv2d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* kernel_device_ptr,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t height,
    size_t width,
    size_t out_channels,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t dilation_height,
    size_t dilation_width,
    size_t groups,
    size_t out_height,
    size_t out_width,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_pool2d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t batch,
    size_t channels,
    size_t height,
    size_t width,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t out_height,
    size_t out_width,
    size_t op,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_softmax_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_strides,
    void** out_device_ptr
);
int omni_tensor_backend_vulkan_ml_sgd_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* velocity_device_ptr,
    size_t velocity_byte_len,
    size_t element_count,
    float learning_rate,
    float momentum,
    float weight_decay,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_velocity_device_ptr
);
int omni_tensor_backend_vulkan_ml_adam_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* first_device_ptr,
    size_t first_byte_len,
    const void* second_device_ptr,
    size_t second_byte_len,
    size_t element_count,
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
);
int omni_tensor_backend_vulkan_ml_rmsprop_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* square_device_ptr,
    size_t square_byte_len,
    const void* velocity_device_ptr,
    size_t velocity_byte_len,
    size_t element_count,
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
);
int omni_tensor_backend_vulkan_copy_range_to_host(
    const void* device_ptr,
    size_t offset,
    size_t byte_len,
    void* host_ptr
);
int omni_tensor_vulkan_dense_required_elements(
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    size_t* out_required
);
int omni_tensor_vulkan_write_u32_metadata(
    uint32_t* metadata,
    size_t* cursor,
    const size_t* values,
    size_t count
);
void omni_tensor_vulkan_cmd_barrier_four_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_byte_len,
    OmniTensorVulkanBuffer* third_output,
    size_t third_byte_len,
    OmniTensorVulkanBuffer* fourth_output,
    size_t fourth_byte_len
);
extern long g_omni_tensor_vulkan_solve_parallel_call_count;
extern long g_omni_tensor_vulkan_solve_multi_dispatch_call_count;
extern long g_omni_tensor_vulkan_solve_pivot_reduce_call_count;
extern long g_omni_tensor_vulkan_solve_factor_stage_call_count;
extern long g_omni_tensor_vulkan_solve_serial_call_count;
extern long g_omni_tensor_vulkan_solve_parallel_f32_call_count;
extern long g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count;
extern long g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count;
extern long g_omni_tensor_vulkan_solve_factor_stage_f32_call_count;
extern long g_omni_tensor_vulkan_solve_serial_f32_call_count;
extern long g_omni_tensor_vulkan_solve_parallel_f64_call_count;
extern long g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count;
extern long g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count;
extern long g_omni_tensor_vulkan_solve_factor_stage_f64_call_count;
extern long g_omni_tensor_vulkan_solve_serial_f64_call_count;

#endif
