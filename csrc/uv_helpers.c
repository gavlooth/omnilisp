#include <stdlib.h>
#include <string.h>
#include "../deps/src/libuv/include/uv.h"

typedef void (*omni_uv_timer_callback_fn)(void* user_data);

typedef struct omni_uv_timer_callback_handle {
    omni_uv_timer_callback_fn callback;
    void* user_data;
    int open;
    int invoke_count;
} omni_uv_timer_callback_handle_t;

void* omni_uv_getaddrinfo_req_new(void) {
    return malloc(sizeof(uv_getaddrinfo_t));
}

void omni_uv_getaddrinfo_req_free(void* req) {
    if (req != NULL) free(req);
}

int omni_uv_getaddrinfo_start(
    void* loop,
    void* req,
    uv_getaddrinfo_cb cb,
    char* node,
    char* service
) {
    if (loop == NULL || req == NULL || cb == NULL || node == NULL) return UV_EINVAL;
    return uv_getaddrinfo((uv_loop_t*)loop, (uv_getaddrinfo_t*)req, cb, node, service, NULL);
}

int omni_uv_cancel_req(void* req) {
    if (req == NULL) return UV_EINVAL;
    return uv_cancel((uv_req_t*)req);
}

void omni_uv_req_set_data(void* req, void* data) {
    if (req == NULL) return;
    ((uv_req_t*)req)->data = data;
}

void* omni_uv_req_get_data(void* req) {
    if (req == NULL) return NULL;
    return ((uv_req_t*)req)->data;
}

void omni_uv_freeaddrinfo(void* res) {
    if (res != NULL) uv_freeaddrinfo((struct addrinfo*)res);
}

int omni_uv_fs_open(char* path, int flags, int mode) {
    if (path == NULL) return UV_EINVAL;
    uv_fs_t req;
    int r = (int)uv_fs_open(NULL, &req, path, flags, mode, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

int omni_uv_fs_close(int fd) {
    if (fd < 0) return UV_EINVAL;
    uv_fs_t req;
    int r = (int)uv_fs_close(NULL, &req, fd, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

long omni_uv_fs_read(int fd, char* buf, size_t len) {
    if (fd < 0 || buf == NULL) return UV_EINVAL;
    uv_fs_t req;
    uv_buf_t iov = uv_buf_init(buf, (unsigned int)len);
    long r = (long)uv_fs_read(NULL, &req, fd, &iov, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

long omni_uv_fs_write(int fd, char* buf, size_t len) {
    if (fd < 0 || buf == NULL) return UV_EINVAL;
    uv_fs_t req;
    uv_buf_t iov = uv_buf_init(buf, (unsigned int)len);
    long r = (long)uv_fs_write(NULL, &req, fd, &iov, 1, -1, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

int omni_uv_fs_rename(char* src, char* dst) {
    if (src == NULL || dst == NULL) return UV_EINVAL;
    uv_fs_t req;
    int r = (int)uv_fs_rename(NULL, &req, src, dst, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

int omni_uv_fs_unlink(char* path) {
    if (path == NULL) return UV_EINVAL;
    uv_fs_t req;
    int r = (int)uv_fs_unlink(NULL, &req, path, NULL);
    uv_fs_req_cleanup(&req);
    return r;
}

int omni_uv_fs_stat_basic(char* path, unsigned long long* size_out, unsigned int* mode_out) {
    if (path == NULL) return UV_EINVAL;
    uv_fs_t req;
    int r = (int)uv_fs_stat(NULL, &req, path, NULL);
    if (r >= 0) {
        if (size_out != NULL) *size_out = (unsigned long long)req.statbuf.st_size;
        if (mode_out != NULL) *mode_out = (unsigned int)req.statbuf.st_mode;
        r = 0;
    }
    uv_fs_req_cleanup(&req);
    return r;
}

int omni_uv_fs_scandir_join(char* path, char** out_buf, size_t* out_len) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_len != NULL) *out_len = 0;
    if (path == NULL || out_buf == NULL || out_len == NULL) return UV_EINVAL;

    uv_fs_t req;
    int r = (int)uv_fs_scandir(NULL, &req, path, 0, NULL);
    if (r < 0) {
        uv_fs_req_cleanup(&req);
        return r;
    }

    size_t cap = 256;
    size_t len = 0;
    char* out = (char*)malloc(cap);
    if (out == NULL) {
        uv_fs_req_cleanup(&req);
        return UV_ENOMEM;
    }

    uv_dirent_t ent;
    while (1) {
        int next = uv_fs_scandir_next(&req, &ent);
        if (next == UV_EOF) break;
        if (next < 0) {
            free(out);
            uv_fs_req_cleanup(&req);
            return next;
        }
        if (ent.name == NULL) continue;

        size_t name_len = strlen(ent.name);
        size_t need = len + name_len + 2;
        if (need > cap) {
            while (need > cap) cap *= 2;
            char* grown = (char*)realloc(out, cap);
            if (grown == NULL) {
                free(out);
                uv_fs_req_cleanup(&req);
                return UV_ENOMEM;
            }
            out = grown;
        }

        memcpy(out + len, ent.name, name_len);
        len += name_len;
        out[len++] = '\n';
    }

    uv_fs_req_cleanup(&req);

    if (len > 0) len--;  // trim trailing newline
    out[len] = '\0';
    *out_buf = out;
    *out_len = len;
    return 0;
}

int omni_uv_timer_callback_register(
    omni_uv_timer_callback_fn callback,
    void* user_data,
    void** out_handle
) {
    if (out_handle != NULL) *out_handle = NULL;
    if (out_handle == NULL || callback == NULL) return UV_EINVAL;

    omni_uv_timer_callback_handle_t* handle =
        (omni_uv_timer_callback_handle_t*)calloc(1, sizeof(omni_uv_timer_callback_handle_t));
    if (handle == NULL) return UV_ENOMEM;

    handle->callback = callback;
    handle->user_data = user_data;
    handle->open = 1;
    handle->invoke_count = 0;
    *out_handle = handle;
    return 0;
}

int omni_uv_timer_callback_invoke(void* callback_handle) {
    if (callback_handle == NULL) return UV_EINVAL;
    omni_uv_timer_callback_handle_t* handle =
        (omni_uv_timer_callback_handle_t*)callback_handle;

    if (!handle->open || handle->callback == NULL) return UV_ECANCELED;

    handle->invoke_count += 1;
    handle->callback(handle->user_data);
    return 0;
}

int omni_uv_timer_callback_unregister(void* callback_handle) {
    if (callback_handle == NULL) return UV_EINVAL;
    omni_uv_timer_callback_handle_t* handle =
        (omni_uv_timer_callback_handle_t*)callback_handle;

    if (!handle->open) return 0;

    handle->open = 0;
    handle->callback = NULL;
    handle->user_data = NULL;
    return 0;
}

void omni_uv_timer_callback_free(void* callback_handle) {
    if (callback_handle == NULL) return;
    free(callback_handle);
}

int omni_uv_timer_callback_debug_is_open(void* callback_handle) {
    if (callback_handle == NULL) return 0;
    omni_uv_timer_callback_handle_t* handle =
        (omni_uv_timer_callback_handle_t*)callback_handle;
    return handle->open ? 1 : 0;
}

int omni_uv_timer_callback_debug_invoke_count(void* callback_handle) {
    if (callback_handle == NULL) return UV_EINVAL;
    omni_uv_timer_callback_handle_t* handle =
        (omni_uv_timer_callback_handle_t*)callback_handle;
    return handle->invoke_count;
}
