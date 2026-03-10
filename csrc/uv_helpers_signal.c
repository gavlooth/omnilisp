#include <stdlib.h>
#include <uv.h>

typedef struct omni_uv_signal_watch {
    uv_loop_t* loop;
    uv_signal_t signal;
    int signum;
    int active;
    int closed;
    int pending_count;
} omni_uv_signal_watch_t;

static void omni_uv_signal_cb(uv_signal_t* handle, int signum) {
    omni_uv_signal_watch_t* watch = (omni_uv_signal_watch_t*)handle->data;
    if (watch == NULL) return;
    if (signum == watch->signum) {
        watch->pending_count += 1;
    }
}

static void omni_uv_signal_close_cb(uv_handle_t* handle) {
    omni_uv_signal_watch_t* watch = (omni_uv_signal_watch_t*)handle->data;
    if (watch == NULL) return;
    watch->closed = 1;
}

int omni_uv_signal_start(int signum, void** out_watch) {
    if (out_watch != NULL) *out_watch = NULL;
    if (out_watch == NULL || signum <= 0) return UV_EINVAL;

    omni_uv_signal_watch_t* watch = (omni_uv_signal_watch_t*)calloc(1, sizeof(omni_uv_signal_watch_t));
    if (watch == NULL) return UV_ENOMEM;

    watch->loop = (uv_loop_t*)malloc(sizeof(uv_loop_t));
    if (watch->loop == NULL) {
        free(watch);
        return UV_ENOMEM;
    }
    if (uv_loop_init(watch->loop) != 0) {
        free(watch->loop);
        free(watch);
        return UV_EIO;
    }

    watch->signum = signum;
    watch->signal.data = watch;

    int init_status = uv_signal_init(watch->loop, &watch->signal);
    if (init_status < 0) {
        uv_loop_close(watch->loop);
        free(watch->loop);
        free(watch);
        return init_status;
    }

    int start_status = uv_signal_start(&watch->signal, omni_uv_signal_cb, signum);
    if (start_status < 0) {
        uv_close((uv_handle_t*)&watch->signal, omni_uv_signal_close_cb);
        while (!watch->closed) {
            int r = uv_run(watch->loop, UV_RUN_ONCE);
            if (r == 0 && !uv_loop_alive(watch->loop) && !watch->closed) break;
        }
        (void)uv_loop_close(watch->loop);
        free(watch->loop);
        free(watch);
        return start_status;
    }

    watch->active = 1;
    *out_watch = watch;
    return 0;
}

int omni_uv_signal_poll_pending(void* signal_watch) {
    if (signal_watch == NULL) return UV_EINVAL;
    omni_uv_signal_watch_t* watch = (omni_uv_signal_watch_t*)signal_watch;
    if (!watch->active || watch->loop == NULL) return 0;

    (void)uv_run(watch->loop, UV_RUN_NOWAIT);
    int pending = watch->pending_count;
    watch->pending_count = 0;
    return pending;
}

int omni_uv_signal_stop(void* signal_watch) {
    if (signal_watch == NULL) return UV_EINVAL;
    omni_uv_signal_watch_t* watch = (omni_uv_signal_watch_t*)signal_watch;

    int stop_status = 0;
    if (watch->active) {
        stop_status = uv_signal_stop(&watch->signal);
        watch->active = 0;
    }

    if (!watch->closed) {
        uv_close((uv_handle_t*)&watch->signal, omni_uv_signal_close_cb);
        while (!watch->closed) {
            int r = uv_run(watch->loop, UV_RUN_ONCE);
            if (r == 0 && !uv_loop_alive(watch->loop) && !watch->closed) break;
        }
    }

    while (uv_loop_alive(watch->loop)) {
        (void)uv_run(watch->loop, UV_RUN_DEFAULT);
    }
    (void)uv_loop_close(watch->loop);
    free(watch->loop);
    watch->loop = NULL;
    free(watch);
    return stop_status;
}
