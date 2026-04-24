#include <stdio.h>

#define OMNI_TENSOR_VULKAN_SUCCESS 1

int omni_tensor_backend_vulkan_shared_context_release_clears_global_for_tests(void);
int omni_tensor_backend_vulkan_buffer_refcount_concurrent_for_tests(void);
int omni_tensor_backend_vulkan_copy_range_subrange_map_for_tests(void);
int omni_tensor_backend_vulkan_checked_element_bytes_overflow_guard_for_tests(void);

static int expect_success(const char* label, int status) {
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) return 1;
    fprintf(stderr, "%s: status %d\n", label, status);
    return 0;
}

int main(void) {
    if (!expect_success("shared context release clears global", omni_tensor_backend_vulkan_shared_context_release_clears_global_for_tests())) return 1;
    if (!expect_success("buffer refcount concurrent retain/release", omni_tensor_backend_vulkan_buffer_refcount_concurrent_for_tests())) return 1;
    if (!expect_success("copy range maps requested subrange", omni_tensor_backend_vulkan_copy_range_subrange_map_for_tests())) return 1;
    if (!expect_success("checked element bytes overflow guard", omni_tensor_backend_vulkan_checked_element_bytes_overflow_guard_for_tests())) return 1;
    return 0;
}
