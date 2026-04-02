#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdatomic.h>
#include <uv.h>

typedef struct omni_uv_process {
    uv_loop_t* loop;
    uv_process_t process;
    int spawned;
    int exited;
    int exit_status;
    int term_signal;
} omni_uv_process_t;

static _Atomic int g_omni_uv_process_poll_calls = 0;
static _Atomic int g_omni_uv_process_wait_calls = 0;

static void omni_uv_process_exit_cb(uv_process_t* req, int64_t exit_status, int term_signal) {
    omni_uv_process_t* proc = (omni_uv_process_t*)req->data;
    if (proc == NULL) return;
    proc->exited = 1;
    proc->spawned = 0;
    proc->exit_status = (int)exit_status;
    proc->term_signal = term_signal;
    if (!uv_is_closing((uv_handle_t*)req)) {
        uv_close((uv_handle_t*)req, NULL);
    }
}

static void omni_uv_close_fd_if_valid(int fd) {
    if (fd >= 0) (void)close(fd);
}

int omni_uv_process_spawn(
    char* file,
    char** argv,
    char** envp,
    void** out_process,
    int* stdin_fd_out,
    int* stdout_fd_out,
    int* stderr_fd_out
) {
    if (out_process != NULL) *out_process = NULL;
    if (stdin_fd_out != NULL) *stdin_fd_out = -1;
    if (stdout_fd_out != NULL) *stdout_fd_out = -1;
    if (stderr_fd_out != NULL) *stderr_fd_out = -1;
    if (file == NULL || argv == NULL || out_process == NULL ||
        stdin_fd_out == NULL || stdout_fd_out == NULL || stderr_fd_out == NULL) {
        return UV_EINVAL;
    }

    int stdin_pipe[2] = { -1, -1 };
    int stdout_pipe[2] = { -1, -1 };
    int stderr_pipe[2] = { -1, -1 };

    if (pipe(stdin_pipe) != 0) return UV_EPIPE;
    if (pipe(stdout_pipe) != 0) {
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        return UV_EPIPE;
    }
    if (pipe(stderr_pipe) != 0) {
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        omni_uv_close_fd_if_valid(stdout_pipe[0]);
        omni_uv_close_fd_if_valid(stdout_pipe[1]);
        return UV_EPIPE;
    }

    omni_uv_process_t* proc = (omni_uv_process_t*)calloc(1, sizeof(omni_uv_process_t));
    if (proc == NULL) {
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        omni_uv_close_fd_if_valid(stdout_pipe[0]);
        omni_uv_close_fd_if_valid(stdout_pipe[1]);
        omni_uv_close_fd_if_valid(stderr_pipe[0]);
        omni_uv_close_fd_if_valid(stderr_pipe[1]);
        return UV_ENOMEM;
    }

    proc->loop = (uv_loop_t*)malloc(sizeof(uv_loop_t));
    if (proc->loop == NULL) {
        free(proc);
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        omni_uv_close_fd_if_valid(stdout_pipe[0]);
        omni_uv_close_fd_if_valid(stdout_pipe[1]);
        omni_uv_close_fd_if_valid(stderr_pipe[0]);
        omni_uv_close_fd_if_valid(stderr_pipe[1]);
        return UV_ENOMEM;
    }
    if (uv_loop_init(proc->loop) != 0) {
        free(proc->loop);
        free(proc);
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        omni_uv_close_fd_if_valid(stdout_pipe[0]);
        omni_uv_close_fd_if_valid(stdout_pipe[1]);
        omni_uv_close_fd_if_valid(stderr_pipe[0]);
        omni_uv_close_fd_if_valid(stderr_pipe[1]);
        return UV_EIO;
    }

    uv_stdio_container_t stdio[3];
    memset(stdio, 0, sizeof(stdio));
    stdio[0].flags = UV_INHERIT_FD;
    stdio[0].data.fd = stdin_pipe[0];
    stdio[1].flags = UV_INHERIT_FD;
    stdio[1].data.fd = stdout_pipe[1];
    stdio[2].flags = UV_INHERIT_FD;
    stdio[2].data.fd = stderr_pipe[1];

    uv_process_options_t options;
    memset(&options, 0, sizeof(options));
    options.file = file;
    options.args = argv;
    options.env = envp;
    options.exit_cb = omni_uv_process_exit_cb;
    options.stdio_count = 3;
    options.stdio = stdio;

    proc->process.data = proc;
    int spawn_status = uv_spawn(proc->loop, &proc->process, &options);
    if (spawn_status < 0) {
        uv_loop_close(proc->loop);
        free(proc->loop);
        free(proc);
        omni_uv_close_fd_if_valid(stdin_pipe[0]);
        omni_uv_close_fd_if_valid(stdin_pipe[1]);
        omni_uv_close_fd_if_valid(stdout_pipe[0]);
        omni_uv_close_fd_if_valid(stdout_pipe[1]);
        omni_uv_close_fd_if_valid(stderr_pipe[0]);
        omni_uv_close_fd_if_valid(stderr_pipe[1]);
        return spawn_status;
    }

    proc->spawned = 1;

    omni_uv_close_fd_if_valid(stdin_pipe[0]);
    omni_uv_close_fd_if_valid(stdout_pipe[1]);
    omni_uv_close_fd_if_valid(stderr_pipe[1]);

    *stdin_fd_out = stdin_pipe[1];
    *stdout_fd_out = stdout_pipe[0];
    *stderr_fd_out = stderr_pipe[0];
    *out_process = proc;
    return 0;
}

int omni_uv_process_poll(
    void* process,
    int* completed_out,
    int* exit_status_out,
    int* term_signal_out
) {
    atomic_fetch_add_explicit(&g_omni_uv_process_poll_calls, 1, memory_order_relaxed);
    if (completed_out != NULL) *completed_out = 0;
    if (process == NULL) return UV_EINVAL;
    omni_uv_process_t* proc = (omni_uv_process_t*)process;

    if (!proc->exited) {
        (void)uv_run(proc->loop, UV_RUN_NOWAIT);
    }
    if (!proc->exited) return 0;

    if (completed_out != NULL) *completed_out = 1;
    if (exit_status_out != NULL) *exit_status_out = proc->exit_status;
    if (term_signal_out != NULL) *term_signal_out = proc->term_signal;
    return 0;
}

int omni_uv_process_wait(void* process, int* exit_status_out, int* term_signal_out) {
    atomic_fetch_add_explicit(&g_omni_uv_process_wait_calls, 1, memory_order_relaxed);
    if (process == NULL) return UV_EINVAL;
    omni_uv_process_t* proc = (omni_uv_process_t*)process;

    while (!proc->exited) {
        int r = uv_run(proc->loop, UV_RUN_ONCE);
        if (r == 0 && !uv_loop_alive(proc->loop) && !proc->exited) break;
    }
    if (!proc->exited) return UV_ESRCH;

    if (exit_status_out != NULL) *exit_status_out = proc->exit_status;
    if (term_signal_out != NULL) *term_signal_out = proc->term_signal;
    return 0;
}

int omni_uv_process_kill(void* process, int sig) {
    if (process == NULL) return UV_EINVAL;
    omni_uv_process_t* proc = (omni_uv_process_t*)process;
    if (!proc->spawned || proc->exited) return UV_ESRCH;
    return uv_kill(proc->process.pid, sig);
}

int omni_uv_process_pid(void* process) {
    if (process == NULL) return -1;
    omni_uv_process_t* proc = (omni_uv_process_t*)process;
    return (int)proc->process.pid;
}

void omni_uv_process_close(void* process) {
    if (process == NULL) return;
    omni_uv_process_t* proc = (omni_uv_process_t*)process;

    if (!proc->exited && proc->spawned) {
        (void)uv_process_kill(&proc->process, SIGKILL);
        while (!proc->exited) {
            int r = uv_run(proc->loop, UV_RUN_ONCE);
            if (r == 0 && !uv_loop_alive(proc->loop) && !proc->exited) break;
        }
    }

    while (uv_loop_alive(proc->loop)) {
        (void)uv_run(proc->loop, UV_RUN_DEFAULT);
    }
    (void)uv_loop_close(proc->loop);
    free(proc->loop);
    proc->loop = NULL;
    free(proc);
}

void omni_uv_process_debug_reset_counters(void) {
    atomic_store_explicit(&g_omni_uv_process_poll_calls, 0, memory_order_relaxed);
    atomic_store_explicit(&g_omni_uv_process_wait_calls, 0, memory_order_relaxed);
}

int omni_uv_process_debug_poll_calls(void) {
    return atomic_load_explicit(&g_omni_uv_process_poll_calls, memory_order_relaxed);
}

int omni_uv_process_debug_wait_calls(void) {
    return atomic_load_explicit(&g_omni_uv_process_wait_calls, memory_order_relaxed);
}
