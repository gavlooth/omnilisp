#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "../deps/src/libuv/include/uv.h"

static void omni_uv_close_free_handle_cb(uv_handle_t* handle) {
    if (handle != NULL) free(handle);
}

static void omni_uv_pipe_noop_connection_cb(uv_stream_t* server, int status) {
    (void)server;
    (void)status;
}

static int omni_uv_set_fd_blocking(int fd) {
    if (fd < 0) return UV_EINVAL;
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) return UV_EIO;
    if ((flags & O_NONBLOCK) == 0) return 0;
    if (fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) < 0) return UV_EIO;
    return 0;
}

static int omni_uv_set_fd_cloexec(int fd) {
    if (fd < 0) return UV_EINVAL;
    int flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0) return UV_EIO;
    if ((flags & FD_CLOEXEC) != 0) return 0;
    if (fcntl(fd, F_SETFD, flags | FD_CLOEXEC) < 0) return UV_EIO;
    return 0;
}

static int omni_uv_pipe_fail_if_path_exists(char* path) {
    if (path == NULL) return UV_EINVAL;

    struct stat st;
    if (lstat(path, &st) == 0) return UV_EADDRINUSE;
    if (errno == ENOENT) return 0;
    return uv_translate_sys_error(errno);
}

static void omni_uv_pipe_unlink_owned_socket_path(char* path) {
    if (path == NULL) return;

    struct stat st;
    if (lstat(path, &st) != 0) return;
    if (S_ISSOCK(st.st_mode)) (void)unlink(path);
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

static void omni_uv_close_pipe_and_drain(uv_loop_t* loop, uv_pipe_t* pipe) {
    if (loop == NULL || pipe == NULL) return;
    if (!uv_is_closing((uv_handle_t*)pipe)) {
        uv_close((uv_handle_t*)pipe, NULL);
    }
    while (uv_loop_alive(loop)) {
        (void)uv_run(loop, UV_RUN_DEFAULT);
    }
}

int omni_uv_pipe_connect_start(
    void* loop,
    char* path,
    uv_connect_cb cb,
    void** out_req,
    void** out_pipe_handle
) {
    if (out_req != NULL) *out_req = NULL;
    if (out_pipe_handle != NULL) *out_pipe_handle = NULL;
    if (loop == NULL || path == NULL || cb == NULL || out_req == NULL || out_pipe_handle == NULL) {
        return UV_EINVAL;
    }

    uv_connect_t* req = (uv_connect_t*)calloc(1, sizeof(uv_connect_t));
    if (req == NULL) return UV_ENOMEM;
    uv_pipe_t* pipe = (uv_pipe_t*)calloc(1, sizeof(uv_pipe_t));
    if (pipe == NULL) {
        free(req);
        return UV_ENOMEM;
    }

    int init_status = uv_pipe_init((uv_loop_t*)loop, pipe, 0);
    if (init_status < 0) {
        free(pipe);
        free(req);
        return init_status;
    }

    req->data = pipe;
    uv_pipe_connect(req, pipe, path, cb);

    *out_req = req;
    *out_pipe_handle = pipe;
    return 0;
}

int omni_uv_pipe_listen_start(
    void* loop,
    char* path,
    int backlog,
    void** out_server_handle,
    int* out_fd
) {
    if (out_server_handle != NULL) *out_server_handle = NULL;
    if (out_fd != NULL) *out_fd = -1;
    if (loop == NULL || path == NULL || out_server_handle == NULL || out_fd == NULL) {
        return UV_EINVAL;
    }
    if (backlog < 1) backlog = 1;

    int path_status = omni_uv_pipe_fail_if_path_exists(path);
    if (path_status < 0) return path_status;

    uv_pipe_t* server = (uv_pipe_t*)calloc(1, sizeof(uv_pipe_t));
    if (server == NULL) return UV_ENOMEM;

    int init_status = uv_pipe_init((uv_loop_t*)loop, server, 0);
    if (init_status < 0) {
        free(server);
        return init_status;
    }

    int bind_status = uv_pipe_bind(server, path);
    if (bind_status < 0) {
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return bind_status;
    }

    int listen_status = uv_listen((uv_stream_t*)server, backlog, omni_uv_pipe_noop_connection_cb);
    if (listen_status < 0) {
        omni_uv_pipe_unlink_owned_socket_path(path);
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return listen_status;
    }

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)server, &raw_fd);
    if (fd_status < 0) {
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return fd_status;
    }

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) {
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return UV_EIO;
    }
    if (omni_uv_set_fd_cloexec(dup_fd) < 0) {
        (void)close(dup_fd);
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return UV_EIO;
    }
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return UV_EIO;
    }

    *out_server_handle = server;
    *out_fd = dup_fd;
    return 0;
}

int omni_uv_pipe_accept_prepare(
    void* loop,
    void* listener_handle,
    uv_connection_cb cb,
    void** out_client_handle
) {
    if (out_client_handle != NULL) *out_client_handle = NULL;
    if (loop == NULL || listener_handle == NULL || cb == NULL || out_client_handle == NULL) {
        return UV_EINVAL;
    }

    uv_pipe_t* client = (uv_pipe_t*)calloc(1, sizeof(uv_pipe_t));
    if (client == NULL) return UV_ENOMEM;

    int client_init_status = uv_pipe_init((uv_loop_t*)loop, client, 0);
    if (client_init_status < 0) {
        free(client);
        return client_init_status;
    }

    int listen_status = uv_listen((uv_stream_t*)listener_handle, 128, cb);
    if (listen_status < 0) {
        uv_close((uv_handle_t*)client, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return listen_status;
    }

    *out_client_handle = client;
    return 0;
}

int omni_uv_pipe_listener_set_noop(void* listener_handle) {
    if (listener_handle == NULL) return UV_EINVAL;
    return uv_listen((uv_stream_t*)listener_handle, 128, omni_uv_pipe_noop_connection_cb);
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
    if (omni_uv_set_fd_cloexec(dup_fd) < 0) {
        (void)close(dup_fd);
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

    int path_status = omni_uv_pipe_fail_if_path_exists(path);
    if (path_status < 0) return path_status;

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

    int bind_status = uv_pipe_bind(&server, path);
    if (bind_status < 0) {
        omni_uv_close_pipe_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return bind_status;
    }

    int listen_status = uv_listen((uv_stream_t*)&server, backlog, omni_uv_pipe_noop_connection_cb);
    if (listen_status < 0) {
        omni_uv_pipe_unlink_owned_socket_path(path);
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
    if (omni_uv_set_fd_cloexec(dup_fd) < 0) {
        (void)close(dup_fd);
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

int omni_unix_socket_listen_fd(char* path, int backlog, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (path == NULL || out_fd == NULL) return UV_EINVAL;
    if (backlog < 1) backlog = 1;

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;

    size_t path_len = strlen(path);
    if (path_len == 0 || path_len >= sizeof(addr.sun_path)) return UV_EINVAL;
    memcpy(addr.sun_path, path, path_len + 1);
    socklen_t addr_len = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + path_len + 1);

    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return UV_EIO;
    if (omni_uv_set_fd_cloexec(fd) < 0) {
        close(fd);
        return UV_EIO;
    }

    int path_status = omni_uv_pipe_fail_if_path_exists(path);
    if (path_status < 0) {
        close(fd);
        return path_status;
    }

    if (bind(fd, (const struct sockaddr*)&addr, addr_len) < 0) {
        close(fd);
        return UV_EIO;
    }
    if (chmod(path, S_IRUSR | S_IWUSR) < 0) {
        close(fd);
        omni_uv_pipe_unlink_owned_socket_path(path);
        return uv_translate_sys_error(errno);
    }
    if (listen(fd, backlog) < 0) {
        close(fd);
        omni_uv_pipe_unlink_owned_socket_path(path);
        return UV_EIO;
    }

    *out_fd = fd;
    return 0;
}
