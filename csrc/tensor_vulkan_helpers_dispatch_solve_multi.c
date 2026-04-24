#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
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
) {
    if (out_output_device_ptr == NULL || out_status_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_output_device_ptr = NULL;
    *out_status_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (require_float64) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (!omni_tensor_backend_vulkan_float32_available()) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (coefficients_device_ptr == NULL || rhs_device_ptr == NULL ||
        coefficients_byte_len == 0 || rhs_byte_len == 0 ||
        output_byte_len == 0 || status_byte_len < sizeof(uint32_t) * 4u ||
        init_work_item_count == 0 || element_size == 0 || n == 0 || rhs_cols == 0 ||
        local_size == 0 || n > UINT32_MAX || rhs_cols > UINT32_MAX ||
        result_count > UINT32_MAX || init_work_item_count > UINT32_MAX ||
        init_spv == NULL || pivot_spv == NULL || pivot_reduce_spv == NULL ||
        pivot_commit_spv == NULL || row_swap_spv == NULL || factor_spv == NULL ||
        eliminate_spv == NULL || backsolve_spv == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t max_pivot_groups = (n + local_size - 1u) / local_size;
    if (max_pivot_groups == 0 || max_pivot_groups > UINT32_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t pivot_scratch_slots = max_pivot_groups * 2u;
    if (pivot_scratch_slots > (SIZE_MAX / element_size)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t pivot_scratch_byte_len = pivot_scratch_slots * element_size;
    if (n > (SIZE_MAX / element_size)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t factor_scratch_byte_len = n * element_size;
    if (pivot_scratch_slots > (SIZE_MAX - 4u)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t required_status_slots = 4u + pivot_scratch_slots;
    if (required_status_slots > (SIZE_MAX / sizeof(uint32_t))) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (status_byte_len < required_status_slots * sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* coefficients = (OmniTensorVulkanBuffer*)coefficients_device_ptr;
    OmniTensorVulkanBuffer* rhs = (OmniTensorVulkanBuffer*)rhs_device_ptr;
    OmniTensorVulkanContext* context = coefficients->context;
    if (context == NULL || context->device == NULL ||
        rhs->context != context ||
        coefficients->buffer == NULL || rhs->buffer == NULL ||
        coefficients->byte_len < coefficients_byte_len || rhs->byte_len < rhs_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanBuffer* status_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, status_byte_len, &status_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* pivot_scratch = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, pivot_scratch_byte_len, &pivot_scratch);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* factor_scratch = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, factor_scratch_byte_len, &factor_scratch);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(pivot_scratch);
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule init_shader_module = NULL;
    OmniVulkanShaderModule pivot_shader_module = NULL;
    OmniVulkanShaderModule pivot_reduce_shader_module = NULL;
    OmniVulkanShaderModule pivot_commit_shader_module = NULL;
    OmniVulkanShaderModule row_swap_shader_module = NULL;
    OmniVulkanShaderModule factor_shader_module = NULL;
    OmniVulkanShaderModule eliminate_shader_module = NULL;
    OmniVulkanShaderModule backsolve_shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline init_pipeline = NULL;
    OmniVulkanPipeline pivot_pipeline = NULL;
    OmniVulkanPipeline pivot_reduce_pipeline = NULL;
    OmniVulkanPipeline pivot_commit_pipeline = NULL;
    OmniVulkanPipeline row_swap_pipeline = NULL;
    OmniVulkanPipeline factor_pipeline = NULL;
    OmniVulkanPipeline eliminate_pipeline = NULL;
    OmniVulkanPipeline backsolve_pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[6] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {4, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {5, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        6,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        sizeof(OmniTensorVulkanSolveMultiPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        init_spv,
        init_spv_size,
        &init_shader_module,
        &init_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_spv,
        pivot_spv_size,
        &pivot_shader_module,
        &pivot_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_reduce_spv,
        pivot_reduce_spv_size,
        &pivot_reduce_shader_module,
        &pivot_reduce_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_commit_spv,
        pivot_commit_spv_size,
        &pivot_commit_shader_module,
        &pivot_commit_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        row_swap_spv,
        row_swap_spv_size,
        &row_swap_shader_module,
        &row_swap_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        factor_spv,
        factor_spv_size,
        &factor_shader_module,
        &factor_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        eliminate_spv,
        eliminate_spv_size,
        &eliminate_shader_module,
        &eliminate_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        backsolve_spv,
        backsolve_spv_size,
        &backsolve_shader_module,
        &backsolve_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 6 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo coefficients_info = { coefficients->buffer, 0, (OmniVulkanDeviceSize)coefficients_byte_len };
    OmniVulkanDescriptorBufferInfo rhs_info = { rhs->buffer, 0, (OmniVulkanDeviceSize)rhs_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)output_byte_len };
    OmniVulkanDescriptorBufferInfo status_info = { status_output->buffer, 0, (OmniVulkanDeviceSize)status_byte_len };
    OmniVulkanDescriptorBufferInfo pivot_scratch_info = { pivot_scratch->buffer, 0, (OmniVulkanDeviceSize)pivot_scratch_byte_len };
    OmniVulkanDescriptorBufferInfo factor_scratch_info = { factor_scratch->buffer, 0, (OmniVulkanDeviceSize)factor_scratch_byte_len };
    OmniVulkanWriteDescriptorSet writes[6] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &coefficients_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &rhs_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &status_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 4, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &pivot_scratch_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 5, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &factor_scratch_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 6, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanSolveMultiPushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0,
        (uint32_t)init_work_item_count,
        0,
        0,
        0
    };
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, init_pipeline);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
    uint32_t init_group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(init_work_item_count, local_size, &init_group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    omni_vulkan_cmd_dispatch(command_buffer, init_group_count, 1, 1);
    omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

    for (uint32_t pivot = 0; pivot < (uint32_t)n; pivot++) {
        uint32_t pivot_candidate_count = (uint32_t)n - pivot;
        uint32_t pivot_group_count = 0u;
        result = omni_tensor_vulkan_group_count_1d((size_t)pivot_candidate_count, local_size, &pivot_group_count);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
        push.pivot = pivot;
        push.work_count = pivot_candidate_count;
        push.scratch_src = 0;
        push.scratch_dst = 0;
        push.scratch_count = (uint32_t)max_pivot_groups;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        omni_vulkan_cmd_dispatch(command_buffer, pivot_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        uint32_t current_count = pivot_group_count;
        uint32_t src_offset = 0;
        uint32_t dst_offset = (uint32_t)max_pivot_groups;
        if (current_count > 1u) {
            g_omni_tensor_vulkan_solve_pivot_reduce_call_count++;
            if (pivot_reduce_counter != NULL) (*pivot_reduce_counter)++;
        }
        while (current_count > 1u) {
            uint32_t reduced_count = 0u;
            result = omni_tensor_vulkan_group_count_1d((size_t)current_count, local_size, &reduced_count);
            if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
            push.work_count = current_count;
            push.scratch_src = src_offset;
            push.scratch_dst = dst_offset;
            push.scratch_count = (uint32_t)max_pivot_groups;
            omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_reduce_pipeline);
            omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
            omni_vulkan_cmd_dispatch(command_buffer, reduced_count, 1, 1);
            omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

            current_count = reduced_count;
            uint32_t tmp_offset = src_offset;
            src_offset = dst_offset;
            dst_offset = tmp_offset;
        }

        push.work_count = 1;
        push.scratch_src = src_offset;
        push.scratch_dst = dst_offset;
        push.scratch_count = (uint32_t)max_pivot_groups;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_commit_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        omni_vulkan_cmd_dispatch(command_buffer, 1, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        push.work_count = (uint32_t)n + (uint32_t)rhs_cols;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, row_swap_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t swap_group_count = 0u;
        result = omni_tensor_vulkan_group_count_1d((size_t)push.work_count, local_size, &swap_group_count);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
        omni_vulkan_cmd_dispatch(command_buffer, swap_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        uint32_t trailing = (uint32_t)n - pivot - 1u;
        if (trailing == 0u) continue;
        g_omni_tensor_vulkan_solve_factor_stage_call_count++;
        if (factor_stage_counter != NULL) (*factor_stage_counter)++;
        push.work_count = trailing;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, factor_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t factor_group_count = 0u;
        result = omni_tensor_vulkan_group_count_1d((size_t)trailing, local_size, &factor_group_count);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
        omni_vulkan_cmd_dispatch(command_buffer, factor_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        size_t coeff_updates = (size_t)trailing * (size_t)trailing;
        size_t rhs_updates = (size_t)trailing * rhs_cols;
        size_t eliminate_work = coeff_updates + rhs_updates;
        if (eliminate_work > UINT32_MAX) {
            result = OMNI_TENSOR_VULKAN_UNSUPPORTED;
            goto cleanup;
        }
        push.work_count = (uint32_t)eliminate_work;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, eliminate_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t eliminate_group_count = 0u;
        result = omni_tensor_vulkan_group_count_1d(eliminate_work, local_size, &eliminate_group_count);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
        omni_vulkan_cmd_dispatch(command_buffer, eliminate_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);
    }

    for (uint32_t i = (uint32_t)n; i > 0u; i--) {
        push.pivot = i - 1u;
        push.work_count = (uint32_t)rhs_cols;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, backsolve_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t backsolve_group_count = 0u;
        result = omni_tensor_vulkan_group_count_1d(rhs_cols, local_size, &backsolve_group_count);
        if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
        omni_vulkan_cmd_dispatch(command_buffer, backsolve_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);
    }

    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (backsolve_pipeline != NULL) omni_vulkan_destroy_pipeline(device, backsolve_pipeline, NULL);
    if (eliminate_pipeline != NULL) omni_vulkan_destroy_pipeline(device, eliminate_pipeline, NULL);
    if (factor_pipeline != NULL) omni_vulkan_destroy_pipeline(device, factor_pipeline, NULL);
    if (row_swap_pipeline != NULL) omni_vulkan_destroy_pipeline(device, row_swap_pipeline, NULL);
    if (pivot_commit_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_commit_pipeline, NULL);
    if (pivot_reduce_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_reduce_pipeline, NULL);
    if (pivot_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_pipeline, NULL);
    if (init_pipeline != NULL) omni_vulkan_destroy_pipeline(device, init_pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (backsolve_shader_module != NULL) omni_vulkan_destroy_shader_module(device, backsolve_shader_module, NULL);
    if (eliminate_shader_module != NULL) omni_vulkan_destroy_shader_module(device, eliminate_shader_module, NULL);
    if (factor_shader_module != NULL) omni_vulkan_destroy_shader_module(device, factor_shader_module, NULL);
    if (row_swap_shader_module != NULL) omni_vulkan_destroy_shader_module(device, row_swap_shader_module, NULL);
    if (pivot_commit_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_commit_shader_module, NULL);
    if (pivot_reduce_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_reduce_shader_module, NULL);
    if (pivot_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_shader_module, NULL);
    if (init_shader_module != NULL) omni_vulkan_destroy_shader_module(device, init_shader_module, NULL);
    if (factor_scratch != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(factor_scratch);
    if (pivot_scratch != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(pivot_scratch);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_output_device_ptr = output;
    *out_status_device_ptr = status_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
