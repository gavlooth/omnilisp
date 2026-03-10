#include <stdlib.h>
#include <uv.h>

typedef void (*omni_uv_thread_entry_cb)(void* arg);

typedef struct OmniUvThreadBootstrap {
    omni_uv_thread_entry_cb entry;
    void* arg;
} OmniUvThreadBootstrap;

static void omni_uv_thread_entry_trampoline(void* bootstrap_ptr) {
    OmniUvThreadBootstrap* bootstrap = (OmniUvThreadBootstrap*)bootstrap_ptr;
    if (bootstrap == NULL) return;

    omni_uv_thread_entry_cb entry = bootstrap->entry;
    void* arg = bootstrap->arg;
    free(bootstrap);

    if (entry != NULL) {
        entry(arg);
    }
}

void* omni_uv_thread_new(void) {
    return malloc(sizeof(uv_thread_t));
}

void omni_uv_thread_free(void* thread_handle) {
    if (thread_handle != NULL) free(thread_handle);
}

int omni_uv_thread_start(void* thread_handle, omni_uv_thread_entry_cb entry, void* arg) {
    if (thread_handle == NULL || entry == NULL) return UV_EINVAL;

    OmniUvThreadBootstrap* bootstrap = (OmniUvThreadBootstrap*)malloc(sizeof(OmniUvThreadBootstrap));
    if (bootstrap == NULL) return UV_ENOMEM;

    bootstrap->entry = entry;
    bootstrap->arg = arg;

    int status = uv_thread_create((uv_thread_t*)thread_handle, &omni_uv_thread_entry_trampoline, bootstrap);
    if (status != 0) {
        free(bootstrap);
    }
    return status;
}

int omni_uv_thread_join_handle(void* thread_handle) {
    if (thread_handle == NULL) return UV_EINVAL;
    return uv_thread_join((uv_thread_t*)thread_handle);
}

int omni_uv_thread_detach_handle(void* thread_handle) {
    if (thread_handle == NULL) return UV_EINVAL;
    return uv_thread_detach((uv_thread_t*)thread_handle);
}
