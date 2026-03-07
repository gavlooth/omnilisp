#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <uv.h>

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

typedef struct omni_uv_pipe_connect_ctx {
    int done;
    int status;
} omni_uv_pipe_connect_ctx_t;

static void omni_uv_pipe_connect_cb(uv_connect_t* req, int status) {
    if (req == NULL) return;
    omni_uv_pipe_connect_ctx_t* ctx = (omni_uv_pipe_connect_ctx_t*)req->data;
    if (ctx == NULL) return;
    ctx->status = status;
    ctx->done = 1;
}

static void omni_uv_pipe_noop_connection_cb(uv_stream_t* server, int status) {
    (void)server;
    (void)status;
}

static void omni_uv_close_pipe_and_drain(uv_loop_t* loop, uv_pipe_t* pipe) {
    if (loop == NULL || pipe == NULL) return;
    if (!uv_is_closing((uv_handle_t*)pipe)) {
        uv_close((uv_handle_t*)pipe, NULL);
    }
    while (uv_loop_alive(loop)) {
        (void)uv_run(loop, UV_RUN_DEFAULT);
    }
}

static int omni_uv_set_fd_blocking(int fd) {
    if (fd < 0) return UV_EINVAL;
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) return UV_EIO;
    if ((flags & O_NONBLOCK) == 0) return 0;
    if (fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) < 0) return UV_EIO;
    return 0;
}

int omni_uv_pipe_connect_fd(char* path, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (path == NULL || out_fd == NULL) return UV_EINVAL;

    uv_loop_t loop;
    memset(&loop, 0, sizeof(loop));
    int loop_status = uv_loop_init(&loop);
    if (loop_status < 0) return loop_status;

    uv_pipe_t pipe;
    memset(&pipe, 0, sizeof(pipe));
    int init_status = uv_pipe_init(&loop, &pipe, 0);
    if (init_status < 0) {
        (void)uv_loop_close(&loop);
        return init_status;
    }

    omni_uv_pipe_connect_ctx_t ctx = { 0, 0 };
    uv_connect_t req;
    memset(&req, 0, sizeof(req));
    req.data = &ctx;
    uv_pipe_connect(&req, &pipe, path, omni_uv_pipe_connect_cb);

    while (!ctx.done) {
        int run_status = uv_run(&loop, UV_RUN_ONCE);
        if (run_status == 0 && !uv_loop_alive(&loop)) break;
    }

    if (ctx.status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &pipe);
        (void)uv_loop_close(&loop);
        return ctx.status;
    }

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)&pipe, &raw_fd);
    if (fd_status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &pipe);
        (void)uv_loop_close(&loop);
        return fd_status;
    }

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) {
        omni_uv_close_pipe_and_drain(&loop, &pipe);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        omni_uv_close_pipe_and_drain(&loop, &pipe);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }

    omni_uv_close_pipe_and_drain(&loop, &pipe);
    (void)uv_loop_close(&loop);
    *out_fd = dup_fd;
    return 0;
}

int omni_uv_pipe_listen_fd(char* path, int backlog, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (path == NULL || out_fd == NULL) return UV_EINVAL;
    if (backlog < 1) backlog = 1;

    uv_loop_t loop;
    memset(&loop, 0, sizeof(loop));
    int loop_status = uv_loop_init(&loop);
    if (loop_status < 0) return loop_status;

    uv_pipe_t server;
    memset(&server, 0, sizeof(server));
    int init_status = uv_pipe_init(&loop, &server, 0);
    if (init_status < 0) {
        (void)uv_loop_close(&loop);
        return init_status;
    }

    // Best-effort cleanup for stale socket path from previous runs.
    (void)unlink(path);

    int bind_status = uv_pipe_bind(&server, path);
    if (bind_status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return bind_status;
    }

    int listen_status = uv_listen((uv_stream_t*)&server, backlog, omni_uv_pipe_noop_connection_cb);
    if (listen_status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return listen_status;
    }

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)&server, &raw_fd);
    if (fd_status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return fd_status;
    }

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) {
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }

    omni_uv_close_pipe_and_drain(&loop, &server);
    (void)uv_loop_close(&loop);
    *out_fd = dup_fd;
    return 0;
}

typedef struct omni_uv_process {
    uv_loop_t* loop;
    uv_process_t process;
    int spawned;
    int exited;
    int exit_status;
    int term_signal;
} omni_uv_process_t;

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
    stdio[0].data.fd = stdin_pipe[0];   // child stdin read end
    stdio[1].flags = UV_INHERIT_FD;
    stdio[1].data.fd = stdout_pipe[1];  // child stdout write end
    stdio[2].flags = UV_INHERIT_FD;
    stdio[2].data.fd = stderr_pipe[1];  // child stderr write end

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

    // Parent process does not use child ends.
    omni_uv_close_fd_if_valid(stdin_pipe[0]);
    omni_uv_close_fd_if_valid(stdout_pipe[1]);
    omni_uv_close_fd_if_valid(stderr_pipe[1]);

    *stdin_fd_out = stdin_pipe[1];
    *stdout_fd_out = stdout_pipe[0];
    *stderr_fd_out = stderr_pipe[0];
    *out_process = proc;
    return 0;
}

int omni_uv_process_wait(void* process, int* exit_status_out, int* term_signal_out) {
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
