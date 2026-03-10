#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <uv.h>

static void omni_uv_close_free_handle_cb(uv_handle_t* handle) {
    if (handle != NULL) free(handle);
}

static void omni_uv_tcp_noop_connection_cb(uv_stream_t* server, int status) {
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

static void omni_uv_close_tcp_and_drain(uv_loop_t* loop, uv_tcp_t* tcp) {
    if (loop == NULL || tcp == NULL) return;
    if (!uv_is_closing((uv_handle_t*)tcp)) {
        uv_close((uv_handle_t*)tcp, NULL);
    }
    while (uv_loop_alive(loop)) {
        (void)uv_run(loop, UV_RUN_DEFAULT);
    }
}

void omni_uv_tcp_connect_req_free(void* req) {
    if (req != NULL) free(req);
}

void omni_uv_tcp_close_and_free(void* tcp_handle) {
    if (tcp_handle == NULL) return;
    uv_handle_t* handle = (uv_handle_t*)tcp_handle;
    if (uv_is_closing(handle)) return;
    uv_close(handle, omni_uv_close_free_handle_cb);
}

int omni_uv_tcp_connect_detach_fd(void* tcp_handle, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (tcp_handle == NULL || out_fd == NULL) return UV_EINVAL;

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)tcp_handle, &raw_fd);
    if (fd_status < 0) return fd_status;

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) return UV_EIO;
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        return UV_EIO;
    }

    *out_fd = dup_fd;
    return 0;
}

int omni_uv_tcp_connect_start(
    void* loop,
    char* ip,
    int port,
    uv_connect_cb cb,
    void** out_req,
    void** out_tcp_handle
) {
    if (out_req != NULL) *out_req = NULL;
    if (out_tcp_handle != NULL) *out_tcp_handle = NULL;
    if (loop == NULL || ip == NULL || cb == NULL || out_req == NULL || out_tcp_handle == NULL) {
        return UV_EINVAL;
    }
    if (port <= 0 || port > 65535) return UV_EINVAL;

    uv_connect_t* req = (uv_connect_t*)calloc(1, sizeof(uv_connect_t));
    if (req == NULL) return UV_ENOMEM;
    uv_tcp_t* tcp = (uv_tcp_t*)calloc(1, sizeof(uv_tcp_t));
    if (tcp == NULL) {
        free(req);
        return UV_ENOMEM;
    }

    int init_status = uv_tcp_init((uv_loop_t*)loop, tcp);
    if (init_status < 0) {
        free(tcp);
        free(req);
        return init_status;
    }

    struct sockaddr_in addr;
    int addr_status = uv_ip4_addr(ip, port, &addr);
    if (addr_status < 0) {
        uv_close((uv_handle_t*)tcp, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        free(req);
        return addr_status;
    }

    req->data = tcp;
    int connect_status = uv_tcp_connect(req, tcp, (const struct sockaddr*)&addr, cb);
    if (connect_status < 0) {
        uv_close((uv_handle_t*)tcp, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        free(req);
        return connect_status;
    }

    *out_req = req;
    *out_tcp_handle = tcp;
    return 0;
}

int omni_uv_tcp_accept_start(
    void* loop,
    int listener_fd,
    uv_connection_cb cb,
    void** out_server_handle,
    void** out_client_handle
) {
    if (out_server_handle != NULL) *out_server_handle = NULL;
    if (out_client_handle != NULL) *out_client_handle = NULL;
    if (loop == NULL || cb == NULL || out_server_handle == NULL || out_client_handle == NULL) {
        return UV_EINVAL;
    }
    if (listener_fd < 0) return UV_EINVAL;

    int dup_listener_fd = dup(listener_fd);
    if (dup_listener_fd < 0) return UV_EIO;

    uv_tcp_t* server = (uv_tcp_t*)calloc(1, sizeof(uv_tcp_t));
    if (server == NULL) {
        (void)close(dup_listener_fd);
        return UV_ENOMEM;
    }
    uv_tcp_t* client = (uv_tcp_t*)calloc(1, sizeof(uv_tcp_t));
    if (client == NULL) {
        free(server);
        (void)close(dup_listener_fd);
        return UV_ENOMEM;
    }

    int server_init_status = uv_tcp_init((uv_loop_t*)loop, server);
    if (server_init_status < 0) {
        free(client);
        free(server);
        (void)close(dup_listener_fd);
        return server_init_status;
    }

    int open_status = uv_tcp_open(server, dup_listener_fd);
    if (open_status < 0) {
        (void)close(dup_listener_fd);
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        free(client);
        return open_status;
    }

    int client_init_status = uv_tcp_init((uv_loop_t*)loop, client);
    if (client_init_status < 0) {
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        free(client);
        return client_init_status;
    }

    int listen_status = uv_listen((uv_stream_t*)server, 128, cb);
    if (listen_status < 0) {
        uv_close((uv_handle_t*)server, omni_uv_close_free_handle_cb);
        uv_close((uv_handle_t*)client, omni_uv_close_free_handle_cb);
        (void)uv_run((uv_loop_t*)loop, UV_RUN_NOWAIT);
        return listen_status;
    }

    *out_server_handle = server;
    *out_client_handle = client;
    return 0;
}

int omni_uv_tcp_accept_detach_fd(void* server_handle, void* client_handle, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (server_handle == NULL || client_handle == NULL || out_fd == NULL) return UV_EINVAL;

    int accept_status = uv_accept((uv_stream_t*)server_handle, (uv_stream_t*)client_handle);
    if (accept_status < 0) return accept_status;

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)client_handle, &raw_fd);
    if (fd_status < 0) return fd_status;

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) return UV_EIO;
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        return UV_EIO;
    }

    *out_fd = dup_fd;
    return 0;
}

int omni_uv_tcp_close_fd(int fd) {
    if (fd < 0) return UV_EINVAL;

    uv_loop_t loop;
    memset(&loop, 0, sizeof(loop));
    int loop_status = uv_loop_init(&loop);
    if (loop_status < 0) return loop_status;

    uv_tcp_t tcp;
    memset(&tcp, 0, sizeof(tcp));
    int init_status = uv_tcp_init(&loop, &tcp);
    if (init_status < 0) {
        (void)uv_loop_close(&loop);
        return init_status;
    }

    int open_status = uv_tcp_open(&tcp, fd);
    if (open_status < 0) {
        (void)uv_loop_close(&loop);
        return open_status;
    }

    omni_uv_close_tcp_and_drain(&loop, &tcp);
    (void)uv_loop_close(&loop);
    return 0;
}

int omni_uv_tcp_listen_fd(char* ip, int port, int backlog, int* out_fd) {
    if (out_fd != NULL) *out_fd = -1;
    if (ip == NULL || out_fd == NULL) return UV_EINVAL;
    if (port < 0 || port > 65535) return UV_EINVAL;
    if (backlog < 1) backlog = 1;

    uv_loop_t loop;
    memset(&loop, 0, sizeof(loop));
    int loop_status = uv_loop_init(&loop);
    if (loop_status < 0) return loop_status;

    uv_tcp_t server;
    memset(&server, 0, sizeof(server));
    int init_status = uv_tcp_init(&loop, &server);
    if (init_status < 0) {
        (void)uv_loop_close(&loop);
        return init_status;
    }

    struct sockaddr_in addr;
    int addr_status = uv_ip4_addr(ip, port, &addr);
    if (addr_status < 0) {
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return addr_status;
    }

    int bind_status = uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);
    if (bind_status < 0) {
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return bind_status;
    }

    int listen_status = uv_listen((uv_stream_t*)&server, backlog, omni_uv_tcp_noop_connection_cb);
    if (listen_status < 0) {
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return listen_status;
    }

    uv_os_fd_t raw_fd = -1;
    int fd_status = uv_fileno((uv_handle_t*)&server, &raw_fd);
    if (fd_status < 0) {
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return fd_status;
    }

    int dup_fd = dup((int)raw_fd);
    if (dup_fd < 0) {
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }
    if (omni_uv_set_fd_blocking(dup_fd) < 0) {
        (void)close(dup_fd);
        omni_uv_close_tcp_and_drain(&loop, &server);
        (void)uv_loop_close(&loop);
        return UV_EIO;
    }

    omni_uv_close_tcp_and_drain(&loop, &server);
    (void)uv_loop_close(&loop);
    *out_fd = dup_fd;
    return 0;
}
