#include <stdlib.h>
#include "../deps/src/libuv/include/uv.h"

void* omni_uv_work_req_new(void) {
    return malloc(sizeof(uv_work_t));
}

void omni_uv_work_req_free(void* req) {
    if (req != NULL) free(req);
}

int omni_uv_queue_work_start(void* loop, void* req, uv_work_cb work_cb, uv_after_work_cb after_cb) {
    if (loop == NULL || req == NULL || work_cb == NULL || after_cb == NULL) return UV_EINVAL;
    return uv_queue_work((uv_loop_t*)loop, (uv_work_t*)req, work_cb, after_cb);
}
