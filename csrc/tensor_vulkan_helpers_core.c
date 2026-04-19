#include "tensor_vulkan_helpers_internal.h"

void* omni_tensor_vulkan_handle = NULL;
omni_vulkan_create_instance_fn omni_vulkan_create_instance = NULL;
omni_vulkan_destroy_instance_fn omni_vulkan_destroy_instance = NULL;
omni_vulkan_enumerate_physical_devices_fn omni_vulkan_enumerate_physical_devices = NULL;
omni_vulkan_get_queue_family_properties_fn omni_vulkan_get_queue_family_properties = NULL;
omni_vulkan_get_physical_device_features_fn omni_vulkan_get_physical_device_features = NULL;
omni_vulkan_get_physical_device_memory_properties_fn omni_vulkan_get_physical_device_memory_properties = NULL;
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

static int omni_tensor_vulkan_resolve(void);

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

int omni_tensor_vulkan_select_physical_device(
    OmniVulkanInstance instance,
    OmniVulkanPhysicalDevice* out_physical_device,
    uint32_t* out_queue_family_index
) {
    if (instance == NULL || out_physical_device == NULL || out_queue_family_index == NULL) return 0;
    *out_physical_device = NULL;
    *out_queue_family_index = 0;

    uint32_t device_count = 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, NULL) != OMNI_VULKAN_SUCCESS || device_count == 0) return 0;

    OmniVulkanPhysicalDevice* devices = (OmniVulkanPhysicalDevice*)calloc(device_count, sizeof(OmniVulkanPhysicalDevice));
    if (devices == NULL) return 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, devices) != OMNI_VULKAN_SUCCESS) {
        free(devices);
        return 0;
    }

    int found = 0;
    for (uint32_t i = 0; i < device_count && !found; i++) {
        uint32_t queue_count = 0;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, NULL);
        if (queue_count == 0) continue;

        OmniVulkanQueueFamilyProperties* queues = (OmniVulkanQueueFamilyProperties*)calloc(queue_count, sizeof(OmniVulkanQueueFamilyProperties));
        if (queues == NULL) continue;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, queues);

        for (uint32_t q = 0; q < queue_count; q++) {
            if ((queues[q].queueFlags & OMNI_VULKAN_QUEUE_COMPUTE_BIT) != 0 && queues[q].queueCount > 0) {
                *out_physical_device = devices[i];
                *out_queue_family_index = q;
                found = 1;
                break;
            }
        }
        free(queues);
    }

    free(devices);
    return found;
}

int omni_tensor_vulkan_find_host_visible_memory_type(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t memory_type_bits,
    uint32_t* out_memory_type_index
) {
    if (physical_device == NULL || out_memory_type_index == NULL) return 0;
    *out_memory_type_index = 0;

    OmniVulkanPhysicalDeviceMemoryProperties props;
    memset(&props, 0, sizeof(props));
    omni_vulkan_get_physical_device_memory_properties(physical_device, &props);

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
