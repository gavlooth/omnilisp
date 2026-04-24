#include "tensor_vulkan_helpers_internal.h"

void* omni_tensor_vulkan_handle = NULL;
omni_vulkan_create_instance_fn omni_vulkan_create_instance = NULL;
omni_vulkan_destroy_instance_fn omni_vulkan_destroy_instance = NULL;
omni_vulkan_enumerate_physical_devices_fn omni_vulkan_enumerate_physical_devices = NULL;
omni_vulkan_get_queue_family_properties_fn omni_vulkan_get_queue_family_properties = NULL;
omni_vulkan_get_physical_device_features_fn omni_vulkan_get_physical_device_features = NULL;
omni_vulkan_get_physical_device_memory_properties_fn omni_vulkan_get_physical_device_memory_properties = NULL;
omni_vulkan_get_physical_device_properties_fn omni_vulkan_get_physical_device_properties = NULL;
omni_vulkan_create_device_fn omni_vulkan_create_device = NULL;
omni_vulkan_destroy_device_fn omni_vulkan_destroy_device = NULL;
omni_vulkan_create_buffer_fn omni_vulkan_create_buffer = NULL;
omni_vulkan_destroy_buffer_fn omni_vulkan_destroy_buffer = NULL;
omni_vulkan_get_buffer_memory_requirements_fn omni_vulkan_get_buffer_memory_requirements = NULL;
omni_vulkan_allocate_memory_fn omni_vulkan_allocate_memory = NULL;
omni_vulkan_free_memory_fn omni_vulkan_free_memory = NULL;
omni_vulkan_bind_buffer_memory_fn omni_vulkan_bind_buffer_memory = NULL;
omni_vulkan_map_memory_fn omni_vulkan_map_memory = NULL;
omni_vulkan_unmap_memory_fn omni_vulkan_unmap_memory = NULL;
omni_vulkan_get_device_queue_fn omni_vulkan_get_device_queue = NULL;
omni_vulkan_create_shader_module_fn omni_vulkan_create_shader_module = NULL;
omni_vulkan_destroy_shader_module_fn omni_vulkan_destroy_shader_module = NULL;
omni_vulkan_create_descriptor_set_layout_fn omni_vulkan_create_descriptor_set_layout = NULL;
omni_vulkan_destroy_descriptor_set_layout_fn omni_vulkan_destroy_descriptor_set_layout = NULL;
omni_vulkan_create_descriptor_pool_fn omni_vulkan_create_descriptor_pool = NULL;
omni_vulkan_destroy_descriptor_pool_fn omni_vulkan_destroy_descriptor_pool = NULL;
omni_vulkan_allocate_descriptor_sets_fn omni_vulkan_allocate_descriptor_sets = NULL;
omni_vulkan_update_descriptor_sets_fn omni_vulkan_update_descriptor_sets = NULL;
omni_vulkan_create_pipeline_layout_fn omni_vulkan_create_pipeline_layout = NULL;
omni_vulkan_destroy_pipeline_layout_fn omni_vulkan_destroy_pipeline_layout = NULL;
omni_vulkan_create_compute_pipelines_fn omni_vulkan_create_compute_pipelines = NULL;
omni_vulkan_destroy_pipeline_fn omni_vulkan_destroy_pipeline = NULL;
omni_vulkan_create_command_pool_fn omni_vulkan_create_command_pool = NULL;
omni_vulkan_destroy_command_pool_fn omni_vulkan_destroy_command_pool = NULL;
omni_vulkan_allocate_command_buffers_fn omni_vulkan_allocate_command_buffers = NULL;
omni_vulkan_begin_command_buffer_fn omni_vulkan_begin_command_buffer = NULL;
omni_vulkan_end_command_buffer_fn omni_vulkan_end_command_buffer = NULL;
omni_vulkan_cmd_bind_pipeline_fn omni_vulkan_cmd_bind_pipeline = NULL;
omni_vulkan_cmd_bind_descriptor_sets_fn omni_vulkan_cmd_bind_descriptor_sets = NULL;
omni_vulkan_cmd_push_constants_fn omni_vulkan_cmd_push_constants = NULL;
omni_vulkan_cmd_dispatch_fn omni_vulkan_cmd_dispatch = NULL;
omni_vulkan_cmd_pipeline_barrier_fn omni_vulkan_cmd_pipeline_barrier = NULL;
omni_vulkan_queue_submit_fn omni_vulkan_queue_submit = NULL;
omni_vulkan_queue_wait_idle_fn omni_vulkan_queue_wait_idle = NULL;
int omni_tensor_vulkan_resolution_attempted = 0;
int omni_tensor_vulkan_probe_attempted = 0;
int omni_tensor_vulkan_available_cached = 0;
int omni_tensor_vulkan_float64_cached = 0;
int omni_tensor_vulkan_int64_cached = 0;
// Process-lifetime backend context so independently placed buffers can bind
// together in binary Vulkan kernels.
OmniTensorVulkanContext* omni_tensor_vulkan_shared_context = NULL;
static pthread_mutex_t omni_tensor_vulkan_context_mutex = PTHREAD_MUTEX_INITIALIZER;

static int omni_tensor_vulkan_resolve(void);
int omni_tensor_vulkan_find_host_visible_memory_type(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t memory_type_bits,
    uint32_t* out_memory_type_index
);

static int omni_tensor_vulkan_physical_device_query_functions_load(
    omni_vulkan_get_physical_device_features_fn* out_features,
    omni_vulkan_get_physical_device_memory_properties_fn* out_memory_properties
) {
    if (out_features == NULL || out_memory_properties == NULL) return 0;
    *out_features = omni_vulkan_get_physical_device_features;
    *out_memory_properties = omni_vulkan_get_physical_device_memory_properties;
    return *out_features != NULL && *out_memory_properties != NULL;
}

static int omni_tensor_vulkan_memory_property_query_function_load(
    omni_vulkan_get_physical_device_memory_properties_fn* out_memory_properties
) {
    if (out_memory_properties == NULL) return 0;
    *out_memory_properties = omni_vulkan_get_physical_device_memory_properties;
    return *out_memory_properties != NULL;
}

int omni_tensor_vulkan_silence_stderr_begin(int* saved_stderr) {
    if (saved_stderr == NULL) return 0;
    *saved_stderr = -1;

    int devnull = open("/dev/null", O_WRONLY);
    if (devnull < 0) return 0;

    int saved = dup(STDERR_FILENO);
    if (saved < 0) {
        close(devnull);
        return 0;
    }

    if (dup2(devnull, STDERR_FILENO) < 0) {
        close(saved);
        close(devnull);
        return 0;
    }

    close(devnull);
    *saved_stderr = saved;
    return 1;
}

void omni_tensor_vulkan_silence_stderr_end(int saved_stderr) {
    if (saved_stderr < 0) return;
    dup2(saved_stderr, STDERR_FILENO);
    close(saved_stderr);
}

int omni_tensor_vulkan_create_instance_silent(OmniVulkanInstance* out_instance) {
    if (out_instance == NULL) return 0;
    *out_instance = NULL;
    if (!omni_tensor_vulkan_resolve()) return 0;

    OmniVulkanApplicationInfo app_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_APPLICATION_INFO,
        NULL,
        "Omni",
        1,
        "Omni",
        1,
        OMNI_VULKAN_API_VERSION_1_0
    };
    OmniVulkanInstanceCreateInfo create_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        NULL,
        0,
        &app_info,
        0,
        NULL,
        0,
        NULL
    };

    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult result = omni_vulkan_create_instance(&create_info, NULL, out_instance);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    return result == OMNI_VULKAN_SUCCESS && *out_instance != NULL;
}

static uint32_t omni_tensor_vulkan_memory_score(const OmniVulkanPhysicalDeviceMemoryProperties* props) {
    if (props == NULL) return 0u;

    uint32_t score = 0u;
    uint32_t required = OMNI_VULKAN_MEMORY_PROPERTY_HOST_VISIBLE_BIT | OMNI_VULKAN_MEMORY_PROPERTY_HOST_COHERENT_BIT;
    int has_host_visible = 0;
    for (uint32_t i = 0; i < props->memoryTypeCount && i < 32u; i++) {
        if ((props->memoryTypes[i].propertyFlags & required) == required) {
            has_host_visible = 1;
            break;
        }
    }
    if (has_host_visible) score += 8u;

    uint64_t largest_heap = 0u;
    for (uint32_t i = 0; i < props->memoryHeapCount && i < 16u; i++) {
        if (props->memoryHeaps[i].size > largest_heap) largest_heap = props->memoryHeaps[i].size;
    }
    if (largest_heap >= (uint64_t)1 << 30) score += 1u;
    if (largest_heap >= (uint64_t)4 << 30) score += 1u;
    if (largest_heap >= (uint64_t)8 << 30) score += 1u;
    return score;
}

static uint32_t omni_tensor_vulkan_device_type_score(const OmniVulkanPhysicalDeviceProperties* props) {
    if (props == NULL) return 0u;
    switch (props->deviceType) {
        case OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU:
            return 64u;
        case OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU:
            return 32u;
        case OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU:
            return 16u;
        case OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_CPU:
            return 8u;
        default:
            return 0u;
    }
}

static uint32_t omni_tensor_vulkan_device_score(
    const OmniVulkanPhysicalDeviceProperties* properties,
    const OmniVulkanPhysicalDeviceFeatures* features,
    const OmniVulkanPhysicalDeviceMemoryProperties* props,
    const OmniVulkanQueueFamilyProperties* queue
) {
    if (features == NULL || queue == NULL || queue->queueCount == 0u) return 0u;

    uint32_t score = 0u;
    if ((queue->queueFlags & OMNI_VULKAN_QUEUE_COMPUTE_BIT) == 0u) return 0u;
    score += omni_tensor_vulkan_device_type_score(properties);
    if (features->shaderFloat64 != 0) score += 16u;
    if (features->shaderInt64 != 0) score += 8u;
    if (queue->queueCount > 1u) score += 1u;
    score += omni_tensor_vulkan_memory_score(props);
    return score;
}

static int omni_tensor_vulkan_queue_is_selectable(const OmniVulkanQueueFamilyProperties* queue) {
    if (queue == NULL || queue->queueCount == 0u) return 0;
    return (queue->queueFlags & OMNI_VULKAN_QUEUE_COMPUTE_BIT) != 0u;
}

int omni_tensor_vulkan_select_physical_device(
    OmniVulkanInstance instance,
    OmniVulkanPhysicalDevice* out_physical_device,
    uint32_t* out_queue_family_index
) {
    if (instance == NULL || out_physical_device == NULL || out_queue_family_index == NULL) return 0;
    *out_physical_device = NULL;
    *out_queue_family_index = 0;
    omni_vulkan_get_physical_device_features_fn get_features = NULL;
    omni_vulkan_get_physical_device_memory_properties_fn get_memory_properties = NULL;
    if (!omni_tensor_vulkan_physical_device_query_functions_load(&get_features, &get_memory_properties)) return 0;

    uint32_t device_count = 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, NULL) != OMNI_VULKAN_SUCCESS || device_count == 0) return 0;

    OmniVulkanPhysicalDevice* devices = (OmniVulkanPhysicalDevice*)calloc(device_count, sizeof(OmniVulkanPhysicalDevice));
    if (devices == NULL) return 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, devices) != OMNI_VULKAN_SUCCESS) {
        free(devices);
        return 0;
    }

    uint32_t best_score = 0u;
    int found = 0;
    for (uint32_t i = 0; i < device_count; i++) {
        uint32_t queue_count = 0;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, NULL);
        if (queue_count == 0) continue;

        uint32_t allocated_queue_count = queue_count;
        OmniVulkanQueueFamilyProperties* queues = (OmniVulkanQueueFamilyProperties*)calloc(
            allocated_queue_count,
            sizeof(OmniVulkanQueueFamilyProperties)
        );
        if (queues == NULL) continue;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, queues);
        if (queue_count == 0 || queue_count > allocated_queue_count) {
            free(queues);
            continue;
        }

        for (uint32_t q = 0; q < queue_count; q++) {
            if (!omni_tensor_vulkan_queue_is_selectable(&queues[q])) continue;
            OmniVulkanPhysicalDeviceProperties properties = {0};
            OmniVulkanPhysicalDeviceFeatures features = {0};
            OmniVulkanPhysicalDeviceMemoryProperties memory_props = {0};
            if (omni_vulkan_get_physical_device_properties != NULL) {
                omni_vulkan_get_physical_device_properties(devices[i], &properties);
            }
            get_features(devices[i], &features);
            get_memory_properties(devices[i], &memory_props);
            uint32_t score = omni_tensor_vulkan_device_score(&properties, &features, &memory_props, &queues[q]);
            if (!found || score > best_score) {
                *out_physical_device = devices[i];
                *out_queue_family_index = q;
                best_score = score;
                found = 1;
            }
        }
        free(queues);
    }

    free(devices);
    return found;
}

int omni_tensor_backend_vulkan_select_physical_device_preference_for_tests(void) {
    OmniVulkanPhysicalDeviceFeatures rich_features = {0};
    rich_features.shaderFloat64 = 1;
    rich_features.shaderInt64 = 1;
    OmniVulkanPhysicalDeviceMemoryProperties rich_memory = {0};
    rich_memory.memoryTypeCount = 1;
    rich_memory.memoryTypes[0].propertyFlags = OMNI_VULKAN_MEMORY_PROPERTY_HOST_VISIBLE_BIT | OMNI_VULKAN_MEMORY_PROPERTY_HOST_COHERENT_BIT;
    rich_memory.memoryTypes[0].heapIndex = 0;
    rich_memory.memoryHeapCount = 1;
    rich_memory.memoryHeaps[0].size = (uint64_t)8 << 30;
    rich_memory.memoryHeaps[0].flags = 0u;
    OmniVulkanQueueFamilyProperties rich_queue = {
        OMNI_VULKAN_QUEUE_COMPUTE_BIT,
        2u,
        0u,
        { 0u, 0u, 0u }
    };

    OmniVulkanPhysicalDeviceFeatures poor_features = {0};
    OmniVulkanPhysicalDeviceMemoryProperties poor_memory = {0};
    poor_memory.memoryTypeCount = 1;
    poor_memory.memoryTypes[0].propertyFlags = 0u;
    poor_memory.memoryTypes[0].heapIndex = 0;
    poor_memory.memoryHeapCount = 1;
    poor_memory.memoryHeaps[0].size = (uint64_t)512 << 20;
    poor_memory.memoryHeaps[0].flags = 0u;
    OmniVulkanQueueFamilyProperties poor_queue = {
        OMNI_VULKAN_QUEUE_COMPUTE_BIT,
        1u,
        0u,
        { 0u, 0u, 0u }
    };

    OmniVulkanPhysicalDeviceProperties rich_properties = {0};
    rich_properties.deviceType = OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU;
    OmniVulkanPhysicalDeviceProperties poor_properties = {0};
    poor_properties.deviceType = OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU;
    uint32_t rich_score = omni_tensor_vulkan_device_score(&rich_properties, &rich_features, &rich_memory, &rich_queue);
    uint32_t poor_score = omni_tensor_vulkan_device_score(&poor_properties, &poor_features, &poor_memory, &poor_queue);
    return rich_score > poor_score ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_INVALID;
}

int omni_tensor_backend_vulkan_device_type_preference_for_tests(void) {
    OmniVulkanPhysicalDeviceProperties discrete = {0};
    discrete.deviceType = OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU;
    OmniVulkanPhysicalDeviceProperties integrated = {0};
    integrated.deviceType = OMNI_VULKAN_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU;
    return omni_tensor_vulkan_device_type_score(&discrete) > omni_tensor_vulkan_device_type_score(&integrated)
        ? OMNI_TENSOR_VULKAN_SUCCESS
        : OMNI_TENSOR_VULKAN_INVALID;
}

int omni_tensor_backend_vulkan_select_physical_device_compute_gate_for_tests(void) {
    OmniVulkanQueueFamilyProperties compute_queue = {
        OMNI_VULKAN_QUEUE_COMPUTE_BIT,
        1u,
        0u,
        { 0u, 0u, 0u }
    };
    OmniVulkanQueueFamilyProperties non_compute_queue = {
        0u,
        4u,
        0u,
        { 0u, 0u, 0u }
    };

    return omni_tensor_vulkan_queue_is_selectable(&compute_queue) &&
           !omni_tensor_vulkan_queue_is_selectable(&non_compute_queue)
        ? OMNI_TENSOR_VULKAN_SUCCESS
        : OMNI_TENSOR_VULKAN_INVALID;
}

int omni_tensor_backend_vulkan_group_count_overflow_guard_for_tests(void) {
    uint32_t group_count = 0u;
    int status = omni_tensor_vulkan_group_count_1d((size_t)UINT32_MAX, OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE, &group_count);
    size_t expected = ((size_t)UINT32_MAX - 1u) / (size_t)OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE + 1u;
    return status == OMNI_TENSOR_VULKAN_SUCCESS && group_count == (uint32_t)expected
        ? OMNI_TENSOR_VULKAN_SUCCESS
        : OMNI_TENSOR_VULKAN_INVALID;
}

static void omni_tensor_vulkan_noop_get_physical_device_features(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceFeatures* features
) {
    (void)physical_device;
    if (features != NULL) memset(features, 0, sizeof(*features));
}

static void omni_tensor_vulkan_noop_get_physical_device_memory_properties(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceMemoryProperties* memory_properties
) {
    (void)physical_device;
    if (memory_properties != NULL) memset(memory_properties, 0, sizeof(*memory_properties));
}

int omni_tensor_backend_vulkan_physical_device_query_null_guard_for_tests(void) {
    omni_vulkan_get_physical_device_features_fn saved_features = omni_vulkan_get_physical_device_features;
    omni_vulkan_get_physical_device_memory_properties_fn saved_memory_properties = omni_vulkan_get_physical_device_memory_properties;

    OmniVulkanPhysicalDevice selected_device = (OmniVulkanPhysicalDevice)(uintptr_t)1u;
    uint32_t selected_queue = 77u;
    uint32_t memory_type_index = 77u;
    OmniVulkanInstance fake_instance = (OmniVulkanInstance)(uintptr_t)1u;
    OmniVulkanPhysicalDevice fake_device = (OmniVulkanPhysicalDevice)(uintptr_t)2u;
    int ok = 1;

    omni_vulkan_get_physical_device_features = NULL;
    omni_vulkan_get_physical_device_memory_properties = omni_tensor_vulkan_noop_get_physical_device_memory_properties;
    if (omni_tensor_vulkan_select_physical_device(fake_instance, &selected_device, &selected_queue) != 0 ||
        selected_device != NULL ||
        selected_queue != 0u) {
        ok = 0;
    }

    selected_device = (OmniVulkanPhysicalDevice)(uintptr_t)1u;
    selected_queue = 77u;
    omni_vulkan_get_physical_device_features = omni_tensor_vulkan_noop_get_physical_device_features;
    omni_vulkan_get_physical_device_memory_properties = NULL;
    if (omni_tensor_vulkan_select_physical_device(fake_instance, &selected_device, &selected_queue) != 0 ||
        selected_device != NULL ||
        selected_queue != 0u) {
        ok = 0;
    }

    memory_type_index = 77u;
    if (omni_tensor_vulkan_find_host_visible_memory_type(fake_device, 1u, &memory_type_index) != 0 ||
        memory_type_index != 0u) {
        ok = 0;
    }

    omni_vulkan_get_physical_device_features = saved_features;
    omni_vulkan_get_physical_device_memory_properties = saved_memory_properties;
    return ok ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_INVALID;
}

int omni_tensor_vulkan_find_host_visible_memory_type(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t memory_type_bits,
    uint32_t* out_memory_type_index
) {
    if (physical_device == NULL || out_memory_type_index == NULL) return 0;
    *out_memory_type_index = 0;
    omni_vulkan_get_physical_device_memory_properties_fn get_memory_properties = NULL;
    if (!omni_tensor_vulkan_memory_property_query_function_load(&get_memory_properties)) return 0;

    OmniVulkanPhysicalDeviceMemoryProperties props;
    memset(&props, 0, sizeof(props));
    get_memory_properties(physical_device, &props);

    for (uint32_t i = 0; i < props.memoryTypeCount && i < 32; i++) {
        uint32_t bit = 1u << i;
        uint32_t required = OMNI_VULKAN_MEMORY_PROPERTY_HOST_VISIBLE_BIT | OMNI_VULKAN_MEMORY_PROPERTY_HOST_COHERENT_BIT;
        if ((memory_type_bits & bit) != 0 && (props.memoryTypes[i].propertyFlags & required) == required) {
            *out_memory_type_index = i;
            return 1;
        }
    }

    return 0;
}

static int omni_tensor_vulkan_resolve(void) {
    if (omni_vulkan_create_instance != NULL &&
        omni_vulkan_destroy_instance != NULL &&
        omni_vulkan_enumerate_physical_devices != NULL &&
        omni_vulkan_get_queue_family_properties != NULL &&
        omni_vulkan_get_physical_device_features != NULL &&
        omni_vulkan_get_physical_device_memory_properties != NULL &&
        omni_vulkan_get_physical_device_properties != NULL &&
        omni_vulkan_create_device != NULL &&
        omni_vulkan_destroy_device != NULL &&
        omni_vulkan_create_buffer != NULL &&
        omni_vulkan_destroy_buffer != NULL &&
        omni_vulkan_get_buffer_memory_requirements != NULL &&
        omni_vulkan_allocate_memory != NULL &&
        omni_vulkan_free_memory != NULL &&
        omni_vulkan_bind_buffer_memory != NULL &&
        omni_vulkan_map_memory != NULL &&
        omni_vulkan_unmap_memory != NULL &&
        omni_vulkan_get_device_queue != NULL &&
        omni_vulkan_create_shader_module != NULL &&
        omni_vulkan_destroy_shader_module != NULL &&
        omni_vulkan_create_descriptor_set_layout != NULL &&
        omni_vulkan_destroy_descriptor_set_layout != NULL &&
        omni_vulkan_create_descriptor_pool != NULL &&
        omni_vulkan_destroy_descriptor_pool != NULL &&
        omni_vulkan_allocate_descriptor_sets != NULL &&
        omni_vulkan_update_descriptor_sets != NULL &&
        omni_vulkan_create_pipeline_layout != NULL &&
        omni_vulkan_destroy_pipeline_layout != NULL &&
        omni_vulkan_create_compute_pipelines != NULL &&
        omni_vulkan_destroy_pipeline != NULL &&
        omni_vulkan_create_command_pool != NULL &&
        omni_vulkan_destroy_command_pool != NULL &&
        omni_vulkan_allocate_command_buffers != NULL &&
        omni_vulkan_begin_command_buffer != NULL &&
        omni_vulkan_end_command_buffer != NULL &&
        omni_vulkan_cmd_bind_pipeline != NULL &&
        omni_vulkan_cmd_bind_descriptor_sets != NULL &&
        omni_vulkan_cmd_push_constants != NULL &&
        omni_vulkan_cmd_dispatch != NULL &&
        omni_vulkan_cmd_pipeline_barrier != NULL &&
        omni_vulkan_queue_submit != NULL &&
        omni_vulkan_queue_wait_idle != NULL) {
        return 1;
    }
    if (omni_tensor_vulkan_resolution_attempted) return 0;
    omni_tensor_vulkan_resolution_attempted = 1;

    const char* candidates[] = {
        "libvulkan.so.1",
        "libvulkan.so",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* create_instance_symbol = dlsym(handle, "vkCreateInstance");
        void* destroy_instance_symbol = dlsym(handle, "vkDestroyInstance");
        void* enumerate_physical_devices_symbol = dlsym(handle, "vkEnumeratePhysicalDevices");
        void* get_queue_family_properties_symbol = dlsym(handle, "vkGetPhysicalDeviceQueueFamilyProperties");
        void* get_physical_device_features_symbol = dlsym(handle, "vkGetPhysicalDeviceFeatures");
        void* get_physical_device_memory_properties_symbol = dlsym(handle, "vkGetPhysicalDeviceMemoryProperties");
        void* get_physical_device_properties_symbol = dlsym(handle, "vkGetPhysicalDeviceProperties");
        void* create_device_symbol = dlsym(handle, "vkCreateDevice");
        void* destroy_device_symbol = dlsym(handle, "vkDestroyDevice");
        void* create_buffer_symbol = dlsym(handle, "vkCreateBuffer");
        void* destroy_buffer_symbol = dlsym(handle, "vkDestroyBuffer");
        void* get_buffer_memory_requirements_symbol = dlsym(handle, "vkGetBufferMemoryRequirements");
        void* allocate_memory_symbol = dlsym(handle, "vkAllocateMemory");
        void* free_memory_symbol = dlsym(handle, "vkFreeMemory");
        void* bind_buffer_memory_symbol = dlsym(handle, "vkBindBufferMemory");
        void* map_memory_symbol = dlsym(handle, "vkMapMemory");
        void* unmap_memory_symbol = dlsym(handle, "vkUnmapMemory");
        void* get_device_queue_symbol = dlsym(handle, "vkGetDeviceQueue");
        void* create_shader_module_symbol = dlsym(handle, "vkCreateShaderModule");
        void* destroy_shader_module_symbol = dlsym(handle, "vkDestroyShaderModule");
        void* create_descriptor_set_layout_symbol = dlsym(handle, "vkCreateDescriptorSetLayout");
        void* destroy_descriptor_set_layout_symbol = dlsym(handle, "vkDestroyDescriptorSetLayout");
        void* create_descriptor_pool_symbol = dlsym(handle, "vkCreateDescriptorPool");
        void* destroy_descriptor_pool_symbol = dlsym(handle, "vkDestroyDescriptorPool");
        void* allocate_descriptor_sets_symbol = dlsym(handle, "vkAllocateDescriptorSets");
        void* update_descriptor_sets_symbol = dlsym(handle, "vkUpdateDescriptorSets");
        void* create_pipeline_layout_symbol = dlsym(handle, "vkCreatePipelineLayout");
        void* destroy_pipeline_layout_symbol = dlsym(handle, "vkDestroyPipelineLayout");
        void* create_compute_pipelines_symbol = dlsym(handle, "vkCreateComputePipelines");
        void* destroy_pipeline_symbol = dlsym(handle, "vkDestroyPipeline");
        void* create_command_pool_symbol = dlsym(handle, "vkCreateCommandPool");
        void* destroy_command_pool_symbol = dlsym(handle, "vkDestroyCommandPool");
        void* allocate_command_buffers_symbol = dlsym(handle, "vkAllocateCommandBuffers");
        void* begin_command_buffer_symbol = dlsym(handle, "vkBeginCommandBuffer");
        void* end_command_buffer_symbol = dlsym(handle, "vkEndCommandBuffer");
        void* cmd_bind_pipeline_symbol = dlsym(handle, "vkCmdBindPipeline");
        void* cmd_bind_descriptor_sets_symbol = dlsym(handle, "vkCmdBindDescriptorSets");
        void* cmd_push_constants_symbol = dlsym(handle, "vkCmdPushConstants");
        void* cmd_dispatch_symbol = dlsym(handle, "vkCmdDispatch");
        void* cmd_pipeline_barrier_symbol = dlsym(handle, "vkCmdPipelineBarrier");
        void* queue_submit_symbol = dlsym(handle, "vkQueueSubmit");
        void* queue_wait_idle_symbol = dlsym(handle, "vkQueueWaitIdle");
        if (create_instance_symbol != NULL &&
            destroy_instance_symbol != NULL &&
            enumerate_physical_devices_symbol != NULL &&
            get_queue_family_properties_symbol != NULL &&
            get_physical_device_features_symbol != NULL &&
            get_physical_device_memory_properties_symbol != NULL &&
            get_physical_device_properties_symbol != NULL &&
            create_device_symbol != NULL &&
            destroy_device_symbol != NULL &&
            create_buffer_symbol != NULL &&
            destroy_buffer_symbol != NULL &&
            get_buffer_memory_requirements_symbol != NULL &&
            allocate_memory_symbol != NULL &&
            free_memory_symbol != NULL &&
            bind_buffer_memory_symbol != NULL &&
            map_memory_symbol != NULL &&
            unmap_memory_symbol != NULL &&
            get_device_queue_symbol != NULL &&
            create_shader_module_symbol != NULL &&
            destroy_shader_module_symbol != NULL &&
            create_descriptor_set_layout_symbol != NULL &&
            destroy_descriptor_set_layout_symbol != NULL &&
            create_descriptor_pool_symbol != NULL &&
            destroy_descriptor_pool_symbol != NULL &&
            allocate_descriptor_sets_symbol != NULL &&
            update_descriptor_sets_symbol != NULL &&
            create_pipeline_layout_symbol != NULL &&
            destroy_pipeline_layout_symbol != NULL &&
            create_compute_pipelines_symbol != NULL &&
            destroy_pipeline_symbol != NULL &&
            create_command_pool_symbol != NULL &&
            destroy_command_pool_symbol != NULL &&
            allocate_command_buffers_symbol != NULL &&
            begin_command_buffer_symbol != NULL &&
            end_command_buffer_symbol != NULL &&
            cmd_bind_pipeline_symbol != NULL &&
            cmd_bind_descriptor_sets_symbol != NULL &&
            cmd_push_constants_symbol != NULL &&
            cmd_dispatch_symbol != NULL &&
            cmd_pipeline_barrier_symbol != NULL &&
            queue_submit_symbol != NULL &&
            queue_wait_idle_symbol != NULL) {
            omni_tensor_vulkan_handle = handle;
            omni_vulkan_create_instance = (omni_vulkan_create_instance_fn)create_instance_symbol;
            omni_vulkan_destroy_instance = (omni_vulkan_destroy_instance_fn)destroy_instance_symbol;
            omni_vulkan_enumerate_physical_devices = (omni_vulkan_enumerate_physical_devices_fn)enumerate_physical_devices_symbol;
            omni_vulkan_get_queue_family_properties = (omni_vulkan_get_queue_family_properties_fn)get_queue_family_properties_symbol;
            omni_vulkan_get_physical_device_features = (omni_vulkan_get_physical_device_features_fn)get_physical_device_features_symbol;
            omni_vulkan_get_physical_device_memory_properties = (omni_vulkan_get_physical_device_memory_properties_fn)get_physical_device_memory_properties_symbol;
            omni_vulkan_get_physical_device_properties = (omni_vulkan_get_physical_device_properties_fn)get_physical_device_properties_symbol;
            omni_vulkan_create_device = (omni_vulkan_create_device_fn)create_device_symbol;
            omni_vulkan_destroy_device = (omni_vulkan_destroy_device_fn)destroy_device_symbol;
            omni_vulkan_create_buffer = (omni_vulkan_create_buffer_fn)create_buffer_symbol;
            omni_vulkan_destroy_buffer = (omni_vulkan_destroy_buffer_fn)destroy_buffer_symbol;
            omni_vulkan_get_buffer_memory_requirements = (omni_vulkan_get_buffer_memory_requirements_fn)get_buffer_memory_requirements_symbol;
            omni_vulkan_allocate_memory = (omni_vulkan_allocate_memory_fn)allocate_memory_symbol;
            omni_vulkan_free_memory = (omni_vulkan_free_memory_fn)free_memory_symbol;
            omni_vulkan_bind_buffer_memory = (omni_vulkan_bind_buffer_memory_fn)bind_buffer_memory_symbol;
            omni_vulkan_map_memory = (omni_vulkan_map_memory_fn)map_memory_symbol;
            omni_vulkan_unmap_memory = (omni_vulkan_unmap_memory_fn)unmap_memory_symbol;
            omni_vulkan_get_device_queue = (omni_vulkan_get_device_queue_fn)get_device_queue_symbol;
            omni_vulkan_create_shader_module = (omni_vulkan_create_shader_module_fn)create_shader_module_symbol;
            omni_vulkan_destroy_shader_module = (omni_vulkan_destroy_shader_module_fn)destroy_shader_module_symbol;
            omni_vulkan_create_descriptor_set_layout = (omni_vulkan_create_descriptor_set_layout_fn)create_descriptor_set_layout_symbol;
            omni_vulkan_destroy_descriptor_set_layout = (omni_vulkan_destroy_descriptor_set_layout_fn)destroy_descriptor_set_layout_symbol;
            omni_vulkan_create_descriptor_pool = (omni_vulkan_create_descriptor_pool_fn)create_descriptor_pool_symbol;
            omni_vulkan_destroy_descriptor_pool = (omni_vulkan_destroy_descriptor_pool_fn)destroy_descriptor_pool_symbol;
            omni_vulkan_allocate_descriptor_sets = (omni_vulkan_allocate_descriptor_sets_fn)allocate_descriptor_sets_symbol;
            omni_vulkan_update_descriptor_sets = (omni_vulkan_update_descriptor_sets_fn)update_descriptor_sets_symbol;
            omni_vulkan_create_pipeline_layout = (omni_vulkan_create_pipeline_layout_fn)create_pipeline_layout_symbol;
            omni_vulkan_destroy_pipeline_layout = (omni_vulkan_destroy_pipeline_layout_fn)destroy_pipeline_layout_symbol;
            omni_vulkan_create_compute_pipelines = (omni_vulkan_create_compute_pipelines_fn)create_compute_pipelines_symbol;
            omni_vulkan_destroy_pipeline = (omni_vulkan_destroy_pipeline_fn)destroy_pipeline_symbol;
            omni_vulkan_create_command_pool = (omni_vulkan_create_command_pool_fn)create_command_pool_symbol;
            omni_vulkan_destroy_command_pool = (omni_vulkan_destroy_command_pool_fn)destroy_command_pool_symbol;
            omni_vulkan_allocate_command_buffers = (omni_vulkan_allocate_command_buffers_fn)allocate_command_buffers_symbol;
            omni_vulkan_begin_command_buffer = (omni_vulkan_begin_command_buffer_fn)begin_command_buffer_symbol;
            omni_vulkan_end_command_buffer = (omni_vulkan_end_command_buffer_fn)end_command_buffer_symbol;
            omni_vulkan_cmd_bind_pipeline = (omni_vulkan_cmd_bind_pipeline_fn)cmd_bind_pipeline_symbol;
            omni_vulkan_cmd_bind_descriptor_sets = (omni_vulkan_cmd_bind_descriptor_sets_fn)cmd_bind_descriptor_sets_symbol;
            omni_vulkan_cmd_push_constants = (omni_vulkan_cmd_push_constants_fn)cmd_push_constants_symbol;
            omni_vulkan_cmd_dispatch = (omni_vulkan_cmd_dispatch_fn)cmd_dispatch_symbol;
            omni_vulkan_cmd_pipeline_barrier = (omni_vulkan_cmd_pipeline_barrier_fn)cmd_pipeline_barrier_symbol;
            omni_vulkan_queue_submit = (omni_vulkan_queue_submit_fn)queue_submit_symbol;
            omni_vulkan_queue_wait_idle = (omni_vulkan_queue_wait_idle_fn)queue_wait_idle_symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}


#include "tensor_vulkan_helpers_core_context.inc"
