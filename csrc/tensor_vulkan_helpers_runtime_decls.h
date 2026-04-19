#ifndef OMNI_TENSOR_VULKAN_HELPERS_RUNTIME_DECLS_H
#define OMNI_TENSOR_VULKAN_HELPERS_RUNTIME_DECLS_H

typedef OmniVulkanResult (*omni_vulkan_create_instance_fn)(
    const OmniVulkanInstanceCreateInfo* create_info,
    const void* allocator,
    OmniVulkanInstance* instance
);
typedef void (*omni_vulkan_destroy_instance_fn)(OmniVulkanInstance instance, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_enumerate_physical_devices_fn)(
    OmniVulkanInstance instance,
    uint32_t* physical_device_count,
    OmniVulkanPhysicalDevice* physical_devices
);
typedef void (*omni_vulkan_get_queue_family_properties_fn)(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t* queue_family_property_count,
    OmniVulkanQueueFamilyProperties* queue_family_properties
);
typedef void (*omni_vulkan_get_physical_device_features_fn)(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceFeatures* features
);
typedef void (*omni_vulkan_get_physical_device_memory_properties_fn)(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceMemoryProperties* memory_properties
);
typedef OmniVulkanResult (*omni_vulkan_create_device_fn)(
    OmniVulkanPhysicalDevice physical_device,
    const OmniVulkanDeviceCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDevice* device
);
typedef void (*omni_vulkan_destroy_device_fn)(OmniVulkanDevice device, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_create_buffer_fn)(
    OmniVulkanDevice device,
    const OmniVulkanBufferCreateInfo* create_info,
    const void* allocator,
    OmniVulkanBuffer* buffer
);
typedef void (*omni_vulkan_destroy_buffer_fn)(OmniVulkanDevice device, OmniVulkanBuffer buffer, const void* allocator);
typedef void (*omni_vulkan_get_buffer_memory_requirements_fn)(
    OmniVulkanDevice device,
    OmniVulkanBuffer buffer,
    OmniVulkanMemoryRequirements* memory_requirements
);
typedef OmniVulkanResult (*omni_vulkan_allocate_memory_fn)(
    OmniVulkanDevice device,
    const OmniVulkanMemoryAllocateInfo* allocate_info,
    const void* allocator,
    OmniVulkanDeviceMemory* memory
);
typedef void (*omni_vulkan_free_memory_fn)(OmniVulkanDevice device, OmniVulkanDeviceMemory memory, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_bind_buffer_memory_fn)(
    OmniVulkanDevice device,
    OmniVulkanBuffer buffer,
    OmniVulkanDeviceMemory memory,
    OmniVulkanDeviceSize memory_offset
);
typedef OmniVulkanResult (*omni_vulkan_map_memory_fn)(
    OmniVulkanDevice device,
    OmniVulkanDeviceMemory memory,
    OmniVulkanDeviceSize offset,
    OmniVulkanDeviceSize size,
    OmniVulkanFlags flags,
    void** data
);
typedef void (*omni_vulkan_unmap_memory_fn)(OmniVulkanDevice device, OmniVulkanDeviceMemory memory);
typedef void (*omni_vulkan_get_device_queue_fn)(
    OmniVulkanDevice device,
    uint32_t queue_family_index,
    uint32_t queue_index,
    OmniVulkanQueue* queue
);
typedef OmniVulkanResult (*omni_vulkan_create_shader_module_fn)(
    OmniVulkanDevice device,
    const OmniVulkanShaderModuleCreateInfo* create_info,
    const void* allocator,
    OmniVulkanShaderModule* shader_module
);
typedef void (*omni_vulkan_destroy_shader_module_fn)(
    OmniVulkanDevice device,
    OmniVulkanShaderModule shader_module,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_descriptor_set_layout_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorSetLayoutCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDescriptorSetLayout* set_layout
);
typedef void (*omni_vulkan_destroy_descriptor_set_layout_fn)(
    OmniVulkanDevice device,
    OmniVulkanDescriptorSetLayout set_layout,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_descriptor_pool_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorPoolCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDescriptorPool* descriptor_pool
);
typedef void (*omni_vulkan_destroy_descriptor_pool_fn)(
    OmniVulkanDevice device,
    OmniVulkanDescriptorPool descriptor_pool,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_allocate_descriptor_sets_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorSetAllocateInfo* allocate_info,
    OmniVulkanDescriptorSet* descriptor_sets
);
typedef void (*omni_vulkan_update_descriptor_sets_fn)(
    OmniVulkanDevice device,
    uint32_t descriptor_write_count,
    const OmniVulkanWriteDescriptorSet* descriptor_writes,
    uint32_t descriptor_copy_count,
    const void* descriptor_copies
);
typedef OmniVulkanResult (*omni_vulkan_create_pipeline_layout_fn)(
    OmniVulkanDevice device,
    const OmniVulkanPipelineLayoutCreateInfo* create_info,
    const void* allocator,
    OmniVulkanPipelineLayout* pipeline_layout
);
typedef void (*omni_vulkan_destroy_pipeline_layout_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_compute_pipelines_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipelineCache pipeline_cache,
    uint32_t create_info_count,
    const OmniVulkanComputePipelineCreateInfo* create_infos,
    const void* allocator,
    OmniVulkanPipeline* pipelines
);
typedef void (*omni_vulkan_destroy_pipeline_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipeline pipeline,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_command_pool_fn)(
    OmniVulkanDevice device,
    const OmniVulkanCommandPoolCreateInfo* create_info,
    const void* allocator,
    OmniVulkanCommandPool* command_pool
);
typedef void (*omni_vulkan_destroy_command_pool_fn)(
    OmniVulkanDevice device,
    OmniVulkanCommandPool command_pool,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_allocate_command_buffers_fn)(
    OmniVulkanDevice device,
    const OmniVulkanCommandBufferAllocateInfo* allocate_info,
    OmniVulkanCommandBuffer* command_buffers
);
typedef OmniVulkanResult (*omni_vulkan_begin_command_buffer_fn)(
    OmniVulkanCommandBuffer command_buffer,
    const OmniVulkanCommandBufferBeginInfo* begin_info
);
typedef OmniVulkanResult (*omni_vulkan_end_command_buffer_fn)(OmniVulkanCommandBuffer command_buffer);
typedef void (*omni_vulkan_cmd_bind_pipeline_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t pipeline_bind_point,
    OmniVulkanPipeline pipeline
);
typedef void (*omni_vulkan_cmd_bind_descriptor_sets_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t pipeline_bind_point,
    OmniVulkanPipelineLayout layout,
    uint32_t first_set,
    uint32_t descriptor_set_count,
    const OmniVulkanDescriptorSet* descriptor_sets,
    uint32_t dynamic_offset_count,
    const uint32_t* dynamic_offsets
);
typedef void (*omni_vulkan_cmd_push_constants_fn)(
    OmniVulkanCommandBuffer command_buffer,
    OmniVulkanPipelineLayout layout,
    OmniVulkanFlags stage_flags,
    uint32_t offset,
    uint32_t size,
    const void* values
);
typedef void (*omni_vulkan_cmd_dispatch_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t group_count_x,
    uint32_t group_count_y,
    uint32_t group_count_z
);
typedef void (*omni_vulkan_cmd_pipeline_barrier_fn)(
    OmniVulkanCommandBuffer command_buffer,
    OmniVulkanFlags src_stage_mask,
    OmniVulkanFlags dst_stage_mask,
    OmniVulkanFlags dependency_flags,
    uint32_t memory_barrier_count,
    const OmniVulkanMemoryBarrier* memory_barriers,
    uint32_t buffer_memory_barrier_count,
    const OmniVulkanBufferMemoryBarrier* buffer_memory_barriers,
    uint32_t image_memory_barrier_count,
    const void* image_memory_barriers
);
typedef OmniVulkanResult (*omni_vulkan_queue_submit_fn)(
    OmniVulkanQueue queue,
    uint32_t submit_count,
    const OmniVulkanSubmitInfo* submits,
    OmniVulkanFence fence
);
typedef OmniVulkanResult (*omni_vulkan_queue_wait_idle_fn)(OmniVulkanQueue queue);
extern void* omni_tensor_vulkan_handle;
extern omni_vulkan_create_instance_fn omni_vulkan_create_instance;
extern omni_vulkan_destroy_instance_fn omni_vulkan_destroy_instance;
extern omni_vulkan_enumerate_physical_devices_fn omni_vulkan_enumerate_physical_devices;
extern omni_vulkan_get_queue_family_properties_fn omni_vulkan_get_queue_family_properties;
extern omni_vulkan_get_physical_device_features_fn omni_vulkan_get_physical_device_features;
extern omni_vulkan_get_physical_device_memory_properties_fn omni_vulkan_get_physical_device_memory_properties;
extern omni_vulkan_create_device_fn omni_vulkan_create_device;
extern omni_vulkan_destroy_device_fn omni_vulkan_destroy_device;
extern omni_vulkan_create_buffer_fn omni_vulkan_create_buffer;
extern omni_vulkan_destroy_buffer_fn omni_vulkan_destroy_buffer;
extern omni_vulkan_get_buffer_memory_requirements_fn omni_vulkan_get_buffer_memory_requirements;
extern omni_vulkan_allocate_memory_fn omni_vulkan_allocate_memory;
extern omni_vulkan_free_memory_fn omni_vulkan_free_memory;
extern omni_vulkan_bind_buffer_memory_fn omni_vulkan_bind_buffer_memory;
extern omni_vulkan_map_memory_fn omni_vulkan_map_memory;
extern omni_vulkan_unmap_memory_fn omni_vulkan_unmap_memory;
extern omni_vulkan_get_device_queue_fn omni_vulkan_get_device_queue;
extern omni_vulkan_create_shader_module_fn omni_vulkan_create_shader_module;
extern omni_vulkan_destroy_shader_module_fn omni_vulkan_destroy_shader_module;
extern omni_vulkan_create_descriptor_set_layout_fn omni_vulkan_create_descriptor_set_layout;
extern omni_vulkan_destroy_descriptor_set_layout_fn omni_vulkan_destroy_descriptor_set_layout;
extern omni_vulkan_create_descriptor_pool_fn omni_vulkan_create_descriptor_pool;
extern omni_vulkan_destroy_descriptor_pool_fn omni_vulkan_destroy_descriptor_pool;
extern omni_vulkan_allocate_descriptor_sets_fn omni_vulkan_allocate_descriptor_sets;
extern omni_vulkan_update_descriptor_sets_fn omni_vulkan_update_descriptor_sets;
extern omni_vulkan_create_pipeline_layout_fn omni_vulkan_create_pipeline_layout;
extern omni_vulkan_destroy_pipeline_layout_fn omni_vulkan_destroy_pipeline_layout;
extern omni_vulkan_create_compute_pipelines_fn omni_vulkan_create_compute_pipelines;
extern omni_vulkan_destroy_pipeline_fn omni_vulkan_destroy_pipeline;
extern omni_vulkan_create_command_pool_fn omni_vulkan_create_command_pool;
extern omni_vulkan_destroy_command_pool_fn omni_vulkan_destroy_command_pool;
extern omni_vulkan_allocate_command_buffers_fn omni_vulkan_allocate_command_buffers;
extern omni_vulkan_begin_command_buffer_fn omni_vulkan_begin_command_buffer;
extern omni_vulkan_end_command_buffer_fn omni_vulkan_end_command_buffer;
extern omni_vulkan_cmd_bind_pipeline_fn omni_vulkan_cmd_bind_pipeline;
extern omni_vulkan_cmd_bind_descriptor_sets_fn omni_vulkan_cmd_bind_descriptor_sets;
extern omni_vulkan_cmd_push_constants_fn omni_vulkan_cmd_push_constants;
extern omni_vulkan_cmd_dispatch_fn omni_vulkan_cmd_dispatch;
extern omni_vulkan_cmd_pipeline_barrier_fn omni_vulkan_cmd_pipeline_barrier;
extern omni_vulkan_queue_submit_fn omni_vulkan_queue_submit;
extern omni_vulkan_queue_wait_idle_fn omni_vulkan_queue_wait_idle;
extern int omni_tensor_vulkan_resolution_attempted;
extern int omni_tensor_vulkan_probe_attempted;
extern int omni_tensor_vulkan_available_cached;
extern int omni_tensor_vulkan_float64_cached;
extern int omni_tensor_vulkan_int64_cached;
// Process-lifetime backend context so independently placed buffers can bind
// together in binary Vulkan kernels.
extern OmniTensorVulkanContext* omni_tensor_vulkan_shared_context;

#endif
