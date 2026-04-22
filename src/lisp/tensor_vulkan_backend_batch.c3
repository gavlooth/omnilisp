module lisp;

extern fn int omni_tensor_backend_vulkan_map_chain2_scalar_f32(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    float first_scalar,
    uint first_mode,
    uint first_op,
    float second_scalar,
    uint second_mode,
    uint second_op,
    usz rank,
    usz* shape,
    usz* strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_chain2_scalar_f32");

extern fn long omni_tensor_backend_vulkan_map_chain2_dispatch_call_count() @extern("omni_tensor_backend_vulkan_map_chain2_dispatch_call_count");

extern fn int omni_tensor_backend_vulkan_map_scalar_chain_f32(
    void* input_device_ptr,
    usz byte_len,
    usz element_count,
    usz op_count,
    float* scalars,
    uint* modes,
    uint* ops,
    usz rank,
    usz* shape,
    usz* strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_scalar_chain_f32");

extern fn long omni_tensor_backend_vulkan_map_scalar_chain_dispatch_call_count() @extern("omni_tensor_backend_vulkan_map_scalar_chain_dispatch_call_count");

extern fn int omni_tensor_backend_vulkan_map_view_scalar_chain_f32(
    void* input_device_ptr,
    usz input_byte_len,
    usz element_count,
    usz op_count,
    float* scalars,
    uint* modes,
    uint* ops,
    usz rank,
    usz* shape,
    usz* view_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_view_scalar_chain_f32");

extern fn long omni_tensor_backend_vulkan_map_view_scalar_chain_dispatch_call_count() @extern("omni_tensor_backend_vulkan_map_view_scalar_chain_dispatch_call_count");

extern fn int omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_f32(
    void* view_device_ptr,
    void* right_device_ptr,
    usz view_byte_len,
    usz right_byte_len,
    usz element_count,
    uint first_op,
    usz scalar_op_count,
    float* scalars,
    uint* modes,
    uint* ops,
    usz rank,
    usz* shape,
    usz* view_strides,
    usz* right_strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_f32");

extern fn long omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_dispatch_call_count() @extern("omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_dispatch_call_count");

extern fn int omni_tensor_backend_vulkan_map_tensor_scalar_chain_f32(
    void* left_device_ptr,
    void* right_device_ptr,
    usz byte_len,
    usz element_count,
    usz op_count,
    float* scalars,
    uint* modes,
    uint* ops,
    usz rank,
    usz* shape,
    usz* strides,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_map_tensor_scalar_chain_f32");

extern fn long omni_tensor_backend_vulkan_map_tensor_scalar_chain_dispatch_call_count() @extern("omni_tensor_backend_vulkan_map_tensor_scalar_chain_dispatch_call_count");

extern fn int omni_tensor_backend_vulkan_map_chain_f32_byte_len_overflow_guard_for_tests() @extern("omni_tensor_backend_vulkan_map_chain_f32_byte_len_overflow_guard_for_tests");

extern fn int omni_tensor_backend_vulkan_contract_scalar_chain_f32(
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
    usz scalar_op_count,
    float* scalars,
    uint* modes,
    uint* ops,
    void** out_device_ptr
) @extern("omni_tensor_backend_vulkan_contract_scalar_chain_f32");

extern fn long omni_tensor_backend_vulkan_contract_scalar_chain_dispatch_call_count() @extern("omni_tensor_backend_vulkan_contract_scalar_chain_dispatch_call_count");
