static void omni_tensor_vulkan_probe(void) {
    if (omni_tensor_vulkan_probe_attempted) return;
    omni_tensor_vulkan_probe_attempted = 1;
    omni_tensor_vulkan_available_cached = 0;
    omni_tensor_vulkan_float64_cached = 0;
    omni_tensor_vulkan_int64_cached = 0;

    OmniVulkanInstance instance = NULL;
    if (!omni_tensor_vulkan_create_instance_silent(&instance)) return;

    OmniVulkanPhysicalDevice physical_device = NULL;
    uint32_t queue_family_index = 0;
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    int selected = omni_tensor_vulkan_select_physical_device(instance, &physical_device, &queue_family_index);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (selected) {
        (void)queue_family_index;
        omni_vulkan_get_physical_device_features_fn get_features = NULL;
        omni_vulkan_get_physical_device_memory_properties_fn get_memory_properties = NULL;
        if (!omni_tensor_vulkan_physical_device_query_functions_load(&get_features, &get_memory_properties)) {
            omni_vulkan_destroy_instance(instance, NULL);
            return;
        }
        (void)get_memory_properties;
        omni_tensor_vulkan_available_cached = 1;
        OmniVulkanPhysicalDeviceFeatures features = {0};
        get_features(physical_device, &features);
        if (features.shaderFloat64 != 0) omni_tensor_vulkan_float64_cached = 1;
        if (features.shaderInt64 != 0) omni_tensor_vulkan_int64_cached = 1;
    }

    omni_vulkan_destroy_instance(instance, NULL);
}

int omni_tensor_backend_vulkan_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_available_cached;
}

int omni_tensor_backend_vulkan_float64_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_float64_cached;
}

int omni_tensor_backend_vulkan_int64_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_int64_cached;
}

int omni_tensor_backend_vulkan_float32_available(void) {
    return omni_tensor_backend_vulkan_available();
}

int omni_tensor_backend_vulkan_rounding_i64_available(void) {
    return omni_tensor_backend_vulkan_int64_available();
}

static void omni_tensor_backend_vulkan_context_retain_locked(OmniTensorVulkanContext* context) {
    if (context == NULL) return;
    if (context->ref_count < UINT32_MAX) context->ref_count++;
}

static void omni_tensor_vulkan_test_destroy_instance(OmniVulkanInstance instance, const void* allocator) {
    (void)instance;
    (void)allocator;
}

static void omni_tensor_vulkan_test_destroy_device(OmniVulkanDevice device, const void* allocator) {
    (void)device;
    (void)allocator;
}

void omni_tensor_backend_vulkan_context_retain(OmniTensorVulkanContext* context) {
    if (context == NULL) return;
    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) return;
    omni_tensor_backend_vulkan_context_retain_locked(context);
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
}

void omni_tensor_backend_vulkan_context_release(OmniTensorVulkanContext* context) {
    if (context == NULL) return;
    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) return;
    if (context->ref_count > 1) {
        context->ref_count--;
        pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
        return;
    }
    if (omni_tensor_vulkan_shared_context == context) {
        omni_tensor_vulkan_shared_context = NULL;
    }
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
    if (context->device != NULL) omni_vulkan_destroy_device(context->device, NULL);
    if (context->instance != NULL) omni_vulkan_destroy_instance(context->instance, NULL);
    free(context);
}

void omni_tensor_backend_vulkan_destroy_buffer_handle(OmniTensorVulkanBuffer* handle) {
    if (handle == NULL) return;
    uint32_t previous_count = atomic_fetch_sub_explicit(&handle->ref_count, 1u, memory_order_acq_rel);
    if (previous_count > 1u) {
        return;
    }
    if (previous_count == 0u) return;
    OmniTensorVulkanContext* context = handle->context;
    if (context != NULL && context->device != NULL) {
        if (handle->buffer != NULL) omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        if (handle->memory != NULL) omni_vulkan_free_memory(context->device, handle->memory, NULL);
    }
    free(handle);
    omni_tensor_backend_vulkan_context_release(context);
}

static int omni_tensor_backend_vulkan_create_context(OmniTensorVulkanContext** out_context) {
    if (out_context == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_context = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;

    OmniTensorVulkanContext* context = (OmniTensorVulkanContext*)calloc(1, sizeof(OmniTensorVulkanContext));
    if (context == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    context->ref_count = 1;

    if (!omni_tensor_vulkan_create_instance_silent(&context->instance)) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    int selected = omni_tensor_vulkan_select_physical_device(context->instance, &context->physical_device, &context->queue_family_index);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (!selected) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }
    omni_vulkan_get_physical_device_features_fn get_features = NULL;
    omni_vulkan_get_physical_device_memory_properties_fn get_memory_properties = NULL;
    if (!omni_tensor_vulkan_physical_device_query_functions_load(&get_features, &get_memory_properties)) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }
    (void)get_memory_properties;

    OmniVulkanPhysicalDeviceFeatures available_features = {0};
    get_features(context->physical_device, &available_features);
    OmniVulkanPhysicalDeviceFeatures enabled_features = {0};
    const OmniVulkanPhysicalDeviceFeatures* enabled_feature_ptr = NULL;
    if (available_features.shaderFloat64 != 0) {
        enabled_features.shaderFloat64 = 1;
        enabled_feature_ptr = &enabled_features;
    }
    if (available_features.shaderInt64 != 0) {
        enabled_features.shaderInt64 = 1;
        enabled_feature_ptr = &enabled_features;
    }

    float queue_priority = 1.0f;
    OmniVulkanDeviceQueueCreateInfo queue_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index,
        1,
        &queue_priority
    };
    OmniVulkanDeviceCreateInfo device_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        NULL,
        0,
        1,
        &queue_info,
        0,
        NULL,
        0,
        NULL,
        enabled_feature_ptr
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult device_result = omni_vulkan_create_device(context->physical_device, &device_info, NULL, &context->device);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (device_result != OMNI_VULKAN_SUCCESS || context->device == NULL) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    *out_context = context;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_get_shared_context(OmniTensorVulkanContext** out_context) {
    if (out_context == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_context = NULL;
    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (omni_tensor_vulkan_shared_context != NULL) {
        omni_tensor_backend_vulkan_context_retain_locked(omni_tensor_vulkan_shared_context);
        *out_context = omni_tensor_vulkan_shared_context;
        pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);

    OmniTensorVulkanContext* context = NULL;
    int status = omni_tensor_backend_vulkan_create_context(&context);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_vulkan_shared_context != NULL) {
        OmniTensorVulkanContext* published = omni_tensor_vulkan_shared_context;
        omni_tensor_backend_vulkan_context_retain_locked(published);
        *out_context = published;
        pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    omni_tensor_vulkan_shared_context = context;
    omni_tensor_backend_vulkan_context_retain_locked(context);
    *out_context = context;
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_create_buffer_on_context(
    OmniTensorVulkanContext* context,
    size_t byte_len,
    OmniTensorVulkanBuffer** out_handle
) {
    if (out_handle == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_handle = NULL;
    if (context == NULL || context->device == NULL || context->physical_device == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)calloc(1, sizeof(OmniTensorVulkanBuffer));
    if (handle == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    handle->context = context;
    handle->byte_len = byte_len;
    atomic_init(&handle->ref_count, 1u);

    OmniVulkanBufferCreateInfo buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        NULL,
        0,
        (OmniVulkanDeviceSize)byte_len,
        OMNI_VULKAN_BUFFER_USAGE_STORAGE_BUFFER_BIT | OMNI_VULKAN_BUFFER_USAGE_TRANSFER_SRC_BIT | OMNI_VULKAN_BUFFER_USAGE_TRANSFER_DST_BIT,
        OMNI_VULKAN_SHARING_MODE_EXCLUSIVE,
        0,
        NULL
    };
    if (omni_vulkan_create_buffer(context->device, &buffer_info, NULL, &handle->buffer) != OMNI_VULKAN_SUCCESS || handle->buffer == NULL) {
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    OmniVulkanMemoryRequirements requirements = {0};
    omni_vulkan_get_buffer_memory_requirements(context->device, handle->buffer, &requirements);
    uint32_t memory_type_index = 0;
    if (!omni_tensor_vulkan_find_host_visible_memory_type(context->physical_device, requirements.memoryTypeBits, &memory_type_index)) {
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    OmniVulkanMemoryAllocateInfo alloc_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        NULL,
        requirements.size,
        memory_type_index
    };
    if (omni_vulkan_allocate_memory(context->device, &alloc_info, NULL, &handle->memory) != OMNI_VULKAN_SUCCESS || handle->memory == NULL) {
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    if (omni_vulkan_bind_buffer_memory(context->device, handle->buffer, handle->memory, 0) != OMNI_VULKAN_SUCCESS) {
        omni_vulkan_free_memory(context->device, handle->memory, NULL);
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    omni_tensor_backend_vulkan_context_retain(context);
    *out_handle = handle;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_create_buffer(size_t byte_len, OmniTensorVulkanBuffer** out_handle) {
    if (out_handle == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_handle = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanContext* context = NULL;
    int status = omni_tensor_backend_vulkan_get_shared_context(&context);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, out_handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_context_release(context);
        return status;
    }
    omni_tensor_backend_vulkan_context_release(context);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_device(const void* host_ptr, size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer(byte_len, &handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    void* mapped = NULL;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(handle);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, host_ptr, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);

    *out_device_ptr = handle;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_host(const void* device_ptr, size_t byte_len, void* host_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(host_ptr, mapped, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_existing_device(const void* host_ptr, size_t byte_len, void* device_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (host_ptr == NULL || device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, host_ptr, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_device_to_existing_device(const void* source_device_ptr, size_t byte_len, void* dest_device_ptr) {
    if (byte_len == 0 || source_device_ptr == dest_device_ptr) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (source_device_ptr == NULL || dest_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* source = (OmniTensorVulkanBuffer*)source_device_ptr;
    OmniTensorVulkanBuffer* dest = (OmniTensorVulkanBuffer*)dest_device_ptr;
    OmniVulkanDevice source_device = source->context != NULL ? source->context->device : NULL;
    OmniVulkanDevice dest_device = dest->context != NULL ? dest->context->device : NULL;
    if (source_device == NULL || dest_device == NULL ||
        source->memory == NULL || dest->memory == NULL ||
        source->byte_len < byte_len || dest->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (source->context != dest->context) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* source_mapped = NULL;
    if (omni_vulkan_map_memory(source_device, source->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &source_mapped) != OMNI_VULKAN_SUCCESS || source_mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    void* dest_mapped = NULL;
    if (omni_vulkan_map_memory(dest_device, dest->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &dest_mapped) != OMNI_VULKAN_SUCCESS || dest_mapped == NULL) {
        omni_vulkan_unmap_memory(source_device, source->memory);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    memcpy(dest_mapped, source_mapped, byte_len);
    omni_vulkan_unmap_memory(dest_device, dest->memory);
    omni_vulkan_unmap_memory(source_device, source->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_fill_f64(void* device_ptr, size_t element_count, double value) {
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || element_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(double);
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    double* values = (double*)mapped;
    for (size_t i = 0; i < element_count; i++) {
        values[i] = value;
    }
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_fill_f32(void* device_ptr, size_t element_count, float value) {
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(float);
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    float* values = (float*)mapped;
    for (size_t i = 0; i < element_count; i++) {
        values[i] = value;
    }
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
void omni_tensor_backend_vulkan_free(void* device_ptr) {
    if (device_ptr == NULL) return;
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)device_ptr);
}

void* omni_tensor_backend_vulkan_retain(void* device_ptr) {
    if (device_ptr == NULL) return NULL;
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    uint32_t count = atomic_load_explicit(&handle->ref_count, memory_order_acquire);
    while (count > 0u && count < UINT32_MAX) {
        if (atomic_compare_exchange_weak_explicit(
                &handle->ref_count,
                &count,
                count + 1u,
                memory_order_acq_rel,
                memory_order_acquire)) {
            return handle;
        }
    }
    if (count == 0u || count == UINT32_MAX) return NULL;
    return handle;
}

int omni_tensor_backend_vulkan_shared_context_release_clears_global_for_tests(void) {
    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (omni_tensor_vulkan_shared_context != NULL) {
        pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);

    omni_vulkan_destroy_instance_fn saved_destroy_instance = omni_vulkan_destroy_instance;
    omni_vulkan_destroy_device_fn saved_destroy_device = omni_vulkan_destroy_device;
    omni_vulkan_destroy_instance = omni_tensor_vulkan_test_destroy_instance;
    omni_vulkan_destroy_device = omni_tensor_vulkan_test_destroy_device;

    OmniTensorVulkanContext* context = (OmniTensorVulkanContext*)calloc(1, sizeof(OmniTensorVulkanContext));
    if (context == NULL) {
        omni_vulkan_destroy_instance = saved_destroy_instance;
        omni_vulkan_destroy_device = saved_destroy_device;
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    context->instance = (OmniVulkanInstance)(uintptr_t)1u;
    context->device = (OmniVulkanDevice)(uintptr_t)2u;
    context->ref_count = 1u;

    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) {
        free(context);
        omni_vulkan_destroy_instance = saved_destroy_instance;
        omni_vulkan_destroy_device = saved_destroy_device;
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    omni_tensor_vulkan_shared_context = context;
    pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);

    omni_tensor_backend_vulkan_context_release(context);

    int result = OMNI_TENSOR_VULKAN_SUCCESS;
    if (pthread_mutex_lock(&omni_tensor_vulkan_context_mutex) != 0) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    } else {
        if (omni_tensor_vulkan_shared_context != NULL) {
            omni_tensor_vulkan_shared_context = NULL;
            result = OMNI_TENSOR_VULKAN_INVALID;
        }
        pthread_mutex_unlock(&omni_tensor_vulkan_context_mutex);
    }

    omni_vulkan_destroy_instance = saved_destroy_instance;
    omni_vulkan_destroy_device = saved_destroy_device;
    return result;
}

typedef struct OmniTensorVulkanRefcountStress {
    OmniTensorVulkanBuffer* handle;
    atomic_int failed;
} OmniTensorVulkanRefcountStress;

static void* omni_tensor_vulkan_refcount_stress_worker(void* arg) {
    OmniTensorVulkanRefcountStress* stress = (OmniTensorVulkanRefcountStress*)arg;
    if (stress == NULL || stress->handle == NULL) return NULL;
    for (size_t i = 0; i < 20000u; i++) {
        OmniTensorVulkanBuffer* retained = (OmniTensorVulkanBuffer*)omni_tensor_backend_vulkan_retain(stress->handle);
        if (retained == NULL) {
            atomic_store_explicit(&stress->failed, 1, memory_order_release);
            return NULL;
        }
        omni_tensor_backend_vulkan_destroy_buffer_handle(retained);
    }
    return NULL;
}

int omni_tensor_backend_vulkan_buffer_refcount_concurrent_for_tests(void) {
    OmniTensorVulkanContext* context = (OmniTensorVulkanContext*)calloc(1, sizeof(OmniTensorVulkanContext));
    if (context == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    context->ref_count = 1u;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)calloc(1, sizeof(OmniTensorVulkanBuffer));
    if (handle == NULL) {
        free(context);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    handle->context = context;
    handle->byte_len = 64u;
    atomic_init(&handle->ref_count, 1u);

    enum { WORKER_COUNT = 8 };
    pthread_t workers[WORKER_COUNT];
    OmniTensorVulkanRefcountStress stress = { handle, ATOMIC_VAR_INIT(0) };
    size_t started = 0u;
    for (; started < WORKER_COUNT; started++) {
        if (pthread_create(&workers[started], NULL, omni_tensor_vulkan_refcount_stress_worker, &stress) != 0) {
            atomic_store_explicit(&stress.failed, 1, memory_order_release);
            break;
        }
    }
    for (size_t i = 0; i < started; i++) {
        if (pthread_join(workers[i], NULL) != 0) atomic_store_explicit(&stress.failed, 1, memory_order_release);
    }
    uint32_t final_count = atomic_load_explicit(&handle->ref_count, memory_order_acquire);
    int failed = atomic_load_explicit(&stress.failed, memory_order_acquire);
    int result = (!failed && final_count == 1u) ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_INVALID;
    omni_tensor_backend_vulkan_destroy_buffer_handle(handle);
    return result;
}
