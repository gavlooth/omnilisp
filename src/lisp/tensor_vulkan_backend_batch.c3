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
