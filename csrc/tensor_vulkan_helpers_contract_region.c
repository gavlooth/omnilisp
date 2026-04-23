#include "tensor_vulkan_helpers_internal.h"

long g_omni_tensor_vulkan_contract_scalar_chain_dispatch_call_count = 0;

long omni_tensor_backend_vulkan_contract_scalar_chain_dispatch_call_count(void) {
    return g_omni_tensor_vulkan_contract_scalar_chain_dispatch_call_count;
}

static void omni_tensor_vulkan_contract_region_barrier(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* buffer,
    size_t byte_len
) {
    OmniVulkanBufferMemoryBarrier barrier = {
        OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
        NULL,
        OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
        OMNI_VULKAN_ACCESS_SHADER_READ_BIT,
        OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
        OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
        buffer->buffer,
        0,
        (OmniVulkanDeviceSize)byte_len
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        1,
        &barrier,
        0,
        NULL
    );
}

static int omni_tensor_vulkan_contract_region_write_buffer(
    OmniVulkanDevice device,
    OmniTensorVulkanContext* context,
    const uint32_t* words,
    size_t word_count,
    OmniTensorVulkanBuffer** out_buffer
) {
    if (out_buffer == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_buffer = NULL;
    size_t byte_len = (word_count == 0 ? 1 : word_count) * sizeof(uint32_t);
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, out_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, (*out_buffer)->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(*out_buffer);
        *out_buffer = NULL;
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    if (word_count == 0) {
        memset(mapped, 0, byte_len);
    } else {
        memcpy(mapped, words, byte_len);
    }
    omni_vulkan_unmap_memory(device, (*out_buffer)->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_contract_region_write_map_metadata(
    OmniVulkanDevice device,
    OmniTensorVulkanContext* context,
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    OmniTensorVulkanBuffer** out_buffer
) {
    if (rank > SIZE_MAX / 6u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t word_count = rank == 0 ? 1 : rank * 6u;
    uint32_t* words = (uint32_t*)calloc(word_count, sizeof(uint32_t));
    if (words == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    size_t cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, strides, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, strides, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, strides, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        status = omni_tensor_vulkan_contract_region_write_buffer(device, context, words, word_count, out_buffer);
    }
    free(words);
    return status;
}

static int omni_tensor_vulkan_contract_region_write_contract_metadata(
    OmniVulkanDevice device,
    OmniTensorVulkanContext* context,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    OmniTensorVulkanBuffer** out_buffer
) {
    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t word_count = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
    if (word_count == 0) word_count = 1;
    uint32_t* words = (uint32_t*)calloc(word_count, sizeof(uint32_t));
    if (words == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    size_t cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, right_strides, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(words, &cursor, right_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        status = omni_tensor_vulkan_contract_region_write_buffer(device, context, words, word_count, out_buffer);
    }
    free(words);
    return status;
}

static void omni_tensor_vulkan_contract_region_write_set(
    OmniVulkanDevice device,
    OmniVulkanDescriptorSet set,
    OmniTensorVulkanBuffer* first,
    OmniTensorVulkanBuffer* second,
    OmniTensorVulkanBuffer* output,
    OmniTensorVulkanBuffer* metadata,
    size_t first_bytes,
    size_t second_bytes,
    size_t output_bytes,
    size_t metadata_bytes
) {
    OmniVulkanDescriptorBufferInfo infos[4] = {
        { first->buffer, 0, (OmniVulkanDeviceSize)first_bytes },
        { second != NULL ? second->buffer : first->buffer, 0, (OmniVulkanDeviceSize)(second != NULL ? second_bytes : first_bytes) },
        { output->buffer, 0, (OmniVulkanDeviceSize)output_bytes },
        { metadata->buffer, 0, (OmniVulkanDeviceSize)metadata_bytes }
    };
    OmniVulkanWriteDescriptorSet writes[4];
    for (uint32_t i = 0; i < 4; i++) {
        writes[i].sType = OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        writes[i].pNext = NULL;
        writes[i].dstSet = set;
        writes[i].dstBinding = i;
        writes[i].dstArrayElement = 0;
        writes[i].descriptorCount = 1;
        writes[i].descriptorType = OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        writes[i].pImageInfo = NULL;
        writes[i].pBufferInfo = &infos[i];
        writes[i].pTexelBufferView = NULL;
    }
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);
}

static int omni_tensor_vulkan_contract_region_validate(
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    size_t* left_count,
    size_t* right_count
) {
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL)) ||
        (out_rank > 0 && out_shape == NULL)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX || axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    int status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, left_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, right_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (*left_count > SIZE_MAX / sizeof(float) || *right_count > SIZE_MAX / sizeof(float) ||
        out_element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    size_t result_axis = 0;
    size_t expected_count = 1;
    if (out_rank != expected_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted = 0;
        for (size_t i = 0; i < axis_count; i++) if (left_axes[i] == axis) contracted = 1;
        if (contracted) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted = 0;
        for (size_t i = 0; i < axis_count; i++) if (right_axes[i] == axis) contracted = 1;
        if (contracted) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    return expected_count == out_element_count ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_UNSUPPORTED;
}

int omni_tensor_backend_vulkan_contract_scalar_chain_f32(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    size_t scalar_op_count,
    const float* scalars,
    const uint32_t* modes,
    const uint32_t* ops,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (out_rank > 0 && out_strides == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (scalar_op_count == 0 || scalar_op_count > UINT32_MAX || scalars == NULL || modes == NULL || ops == NULL) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (scalar_op_count > SIZE_MAX / sizeof(OmniTensorVulkanBuffer*) || scalar_op_count > SIZE_MAX / sizeof(OmniVulkanDescriptorSet)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((scalar_op_count + 1u) > UINT32_MAX / 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    for (size_t i = 0; i < scalar_op_count; i++) {
        if ((modes[i] != 0u && modes[i] != 1u) || ops[i] > 7u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int status = omni_tensor_vulkan_contract_region_validate(
        left_rank,
        right_rank,
        axis_count,
        left_axes,
        right_axes,
        out_element_count,
        out_rank,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        out_shape,
        &left_count,
        &right_count
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (out_element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t left_byte_len = left_count * sizeof(float);
    size_t right_byte_len = right_count * sizeof(float);
    size_t out_byte_len = out_element_count * sizeof(float);
    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL || right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* contract_output = NULL;
    OmniTensorVulkanBuffer* contract_metadata = NULL;
    OmniTensorVulkanBuffer* map_metadata = NULL;
    OmniTensorVulkanBuffer** scalar_outputs = NULL;
    OmniVulkanDescriptorSet* descriptor_sets = NULL;
    OmniVulkanDescriptorSetLayout* set_layouts = NULL;
    OmniVulkanQueue queue = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanPipelineLayout contract_layout = NULL;
    OmniVulkanPipelineLayout map_layout = NULL;
    OmniVulkanShaderModule contract_shader = NULL;
    OmniVulkanShaderModule map_shader = NULL;
    OmniVulkanPipeline contract_pipeline = NULL;
    OmniVulkanPipeline map_pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    OmniTensorVulkanBuffer* transferred_output = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &contract_output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    scalar_outputs = (OmniTensorVulkanBuffer**)calloc(scalar_op_count, sizeof(OmniTensorVulkanBuffer*));
    descriptor_sets = (OmniVulkanDescriptorSet*)calloc(scalar_op_count + 1u, sizeof(OmniVulkanDescriptorSet));
    set_layouts = (OmniVulkanDescriptorSetLayout*)calloc(scalar_op_count + 1u, sizeof(OmniVulkanDescriptorSetLayout));
    if (scalar_outputs == NULL || descriptor_sets == NULL || set_layouts == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    for (size_t i = 0; i < scalar_op_count; i++) {
        result = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &scalar_outputs[i]);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    }
    result = omni_tensor_vulkan_contract_region_write_contract_metadata(
        device, context, left_rank, right_rank, axis_count, left_axes, right_axes,
        out_rank, left_shape, left_strides, right_shape, right_strides, out_shape,
        out_strides, &contract_metadata
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_contract_region_write_map_metadata(device, context, out_rank, out_shape, out_strides, &map_metadata);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO, NULL, 0, 4, bindings };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanPushConstantRange contract_push = { OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(OmniTensorVulkanContractPushConstants) };
    OmniVulkanPushConstantRange map_push = { OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(OmniTensorVulkanMapF32PushConstants) };
    OmniVulkanPipelineLayoutCreateInfo contract_layout_info = { OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO, NULL, 0, 1, &descriptor_set_layout, 1, &contract_push };
    OmniVulkanPipelineLayoutCreateInfo map_layout_info = { OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO, NULL, 0, 1, &descriptor_set_layout, 1, &map_push };
    if (omni_vulkan_create_pipeline_layout(device, &contract_layout_info, NULL, &contract_layout) != OMNI_VULKAN_SUCCESS || contract_layout == NULL ||
        omni_vulkan_create_pipeline_layout(device, &map_layout_info, NULL, &map_layout) != OMNI_VULKAN_SUCCESS || map_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, contract_layout, omni_tensor_vulkan_contract_f32_spv, omni_tensor_vulkan_contract_f32_spv_size, &contract_shader, &contract_pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, map_layout, omni_tensor_vulkan_map_f32_spv, omni_tensor_vulkan_map_f32_spv_size, &map_shader, &map_pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, (uint32_t)((scalar_op_count + 1u) * 4u) };
    OmniVulkanDescriptorPoolCreateInfo pool_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO, NULL, 0, (uint32_t)(scalar_op_count + 1u), 1, &pool_size };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    for (size_t i = 0; i < scalar_op_count + 1u; i++) set_layouts[i] = descriptor_set_layout;
    OmniVulkanDescriptorSetAllocateInfo alloc_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO, NULL, descriptor_pool, (uint32_t)(scalar_op_count + 1u), set_layouts };
    if (omni_vulkan_allocate_descriptor_sets(device, &alloc_info, descriptor_sets) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    size_t contract_metadata_bytes = ((out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u));
    contract_metadata_bytes = (contract_metadata_bytes == 0 ? 1 : contract_metadata_bytes) * sizeof(uint32_t);
    size_t map_metadata_bytes = (out_rank == 0 ? 1 : out_rank * 6u) * sizeof(uint32_t);
    omni_tensor_vulkan_contract_region_write_set(device, descriptor_sets[0], left, right, contract_output, contract_metadata, left_byte_len, right_byte_len, out_byte_len, contract_metadata_bytes);
    for (size_t i = 0; i < scalar_op_count; i++) {
        OmniTensorVulkanBuffer* source = i == 0 ? contract_output : scalar_outputs[i - 1u];
        omni_tensor_vulkan_contract_region_write_set(device, descriptor_sets[i + 1u], source, NULL, scalar_outputs[i], map_metadata, out_byte_len, out_byte_len, out_byte_len, map_metadata_bytes);
    }

    OmniVulkanCommandPoolCreateInfo command_pool_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO, NULL, 0, context->queue_family_index };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferAllocateInfo command_buffer_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO, NULL, command_pool, OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY, 1 };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferBeginInfo begin_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO, NULL, OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, NULL };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    OmniTensorVulkanContractPushConstants contract_push_data = { (uint32_t)left_rank, (uint32_t)right_rank, (uint32_t)axis_count, (uint32_t)out_element_count, (uint32_t)out_rank };
    uint32_t contract_groups = (uint32_t)((out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE);
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, contract_pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, contract_layout, 0, 1, &descriptor_sets[0], 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, contract_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(contract_push_data), &contract_push_data);
    omni_vulkan_cmd_dispatch(command_buffer, contract_groups, 1, 1);
    omni_tensor_vulkan_contract_region_barrier(command_buffer, contract_output, out_byte_len);

    uint32_t map_groups = (uint32_t)((out_element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE);
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, map_pipeline);
    for (size_t i = 0; i < scalar_op_count; i++) {
        uint32_t left_rank_for_map = modes[i] == 1u ? 0u : (uint32_t)out_rank;
        uint32_t right_rank_for_map = modes[i] == 0u ? 0u : (uint32_t)out_rank;
        OmniTensorVulkanMapF32PushConstants map_push_data = { scalars[i], (uint32_t)out_element_count, ops[i], modes[i], (uint32_t)out_rank, left_rank_for_map, right_rank_for_map, 0 };
        omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, map_layout, 0, 1, &descriptor_sets[i + 1u], 0, NULL);
        omni_vulkan_cmd_push_constants(command_buffer, map_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(map_push_data), &map_push_data);
        omni_vulkan_cmd_dispatch(command_buffer, map_groups, 1, 1);
        if (i + 1u < scalar_op_count) omni_tensor_vulkan_contract_region_barrier(command_buffer, scalar_outputs[i], out_byte_len);
    }
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    OmniVulkanSubmitInfo submit_info = { OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO, NULL, 0, NULL, NULL, 1, &command_buffer, 0, NULL };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    transferred_output = scalar_outputs[scalar_op_count - 1u];
    scalar_outputs[scalar_op_count - 1u] = NULL;
    g_omni_tensor_vulkan_contract_scalar_chain_dispatch_call_count++;

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (contract_pipeline != NULL) omni_vulkan_destroy_pipeline(device, contract_pipeline, NULL);
    if (map_pipeline != NULL) omni_vulkan_destroy_pipeline(device, map_pipeline, NULL);
    if (contract_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, contract_layout, NULL);
    if (map_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, map_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (contract_shader != NULL) omni_vulkan_destroy_shader_module(device, contract_shader, NULL);
    if (map_shader != NULL) omni_vulkan_destroy_shader_module(device, map_shader, NULL);
    if (contract_metadata != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(contract_metadata);
    if (map_metadata != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(map_metadata);
    if (contract_output != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(contract_output);
    if (scalar_outputs != NULL) {
        for (size_t i = 0; i < scalar_op_count; i++) {
            if (scalar_outputs[i] != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(scalar_outputs[i]);
        }
    }
    free(scalar_outputs);
    free(descriptor_sets);
    free(set_layouts);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (transferred_output != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(transferred_output);
        return result;
    }
    *out_device_ptr = transferred_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
