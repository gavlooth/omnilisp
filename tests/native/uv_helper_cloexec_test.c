#include "../../deps/src/libuv/include/uv.h"

#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int omni_uv_tcp_listen_fd(char* ip, int port, int backlog, int* out_fd);
int omni_uv_tcp_connect_start(void* loop, char* ip, int port, uv_connect_cb cb, void** out_req, void** out_tcp_handle);
int omni_uv_tcp_connect_detach_fd(void* tcp_handle, int* out_fd);
void omni_uv_tcp_connect_req_free(void* req);
void omni_uv_tcp_close_and_free(void* tcp_handle);
int omni_uv_pipe_listen_fd(char* path, int backlog, int* out_fd);
int omni_unix_socket_listen_fd(char* path, int backlog, int* out_fd);
int omni_uv_process_spawn(char* file, char** argv, char** envp, void** out_process, int* stdin_fd_out, int* stdout_fd_out, int* stderr_fd_out);
int omni_uv_process_wait(void* process, int* exit_status_out, int* term_signal_out);
void omni_uv_process_close(void* process);

typedef struct tcp_connect_ctx {
    int done;
    int status;
} tcp_connect_ctx_t;

static int fail(const char* label) {
    fprintf(stderr, "%s\n", label);
    return 1;
}

static int check_cloexec(const char* label, int fd) {
    int flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0) {
        perror(label);
        return 0;
    }
    if ((flags & FD_CLOEXEC) == 0) {
        fprintf(stderr, "%s: FD_CLOEXEC not set\n", label);
        return 0;
    }
    return 1;
}

static void tcp_connect_cb(uv_connect_t* req, int status) {
    if (req == NULL || req->data == NULL) return;
    tcp_connect_ctx_t* ctx = (tcp_connect_ctx_t*)((uv_tcp_t*)req->data)->data;
    if (ctx == NULL) return;
    ctx->status = status;
    ctx->done = 1;
}

static int test_tcp_connect_detach_cloexec(void) {
    int listen_fd = -1;
    int detached_fd = -1;
    void* req = NULL;
    void* tcp_handle = NULL;
    int status;
    uv_loop_t loop;
    tcp_connect_ctx_t ctx = { 0, 0 };
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);
    int port;

    status = omni_uv_tcp_listen_fd("127.0.0.1", 0, 1, &listen_fd);
    if (status < 0) {
        fprintf(stderr, "tcp listen fd helper failed: %d\n", status);
        return 1;
    }
    if (!check_cloexec("tcp listen fd", listen_fd)) {
        close(listen_fd);
        return 1;
    }

    memset(&addr, 0, sizeof(addr));
    if (getsockname(listen_fd, (struct sockaddr*)&addr, &addr_len) < 0) {
        perror("getsockname");
        close(listen_fd);
        return 1;
    }
    port = ntohs(addr.sin_port);

    memset(&loop, 0, sizeof(loop));
    if (uv_loop_init(&loop) < 0) {
        close(listen_fd);
        return fail("uv_loop_init");
    }

    status = omni_uv_tcp_connect_start(&loop, "127.0.0.1", port, tcp_connect_cb, &req, &tcp_handle);
    if (status < 0) {
        uv_loop_close(&loop);
        close(listen_fd);
        fprintf(stderr, "tcp connect start failed: %d\n", status);
        return 1;
    }

    ((uv_tcp_t*)tcp_handle)->data = &ctx;
    while (!ctx.done) {
        int run_status = uv_run(&loop, UV_RUN_ONCE);
        if (run_status == 0 && !uv_loop_alive(&loop) && !ctx.done) break;
    }

    if (!ctx.done || ctx.status < 0) {
        omni_uv_tcp_close_and_free(tcp_handle);
        while (uv_loop_alive(&loop)) {
            (void)uv_run(&loop, UV_RUN_DEFAULT);
        }
        omni_uv_tcp_connect_req_free(req);
        uv_loop_close(&loop);
        close(listen_fd);
        fprintf(stderr, "tcp connect did not complete: done=%d status=%d\n", ctx.done, ctx.status);
        return 1;
    }

    status = omni_uv_tcp_connect_detach_fd(tcp_handle, &detached_fd);
    if (status < 0) {
        omni_uv_tcp_close_and_free(tcp_handle);
        while (uv_loop_alive(&loop)) {
            (void)uv_run(&loop, UV_RUN_DEFAULT);
        }
        omni_uv_tcp_connect_req_free(req);
        uv_loop_close(&loop);
        close(listen_fd);
        fprintf(stderr, "tcp connect detach fd failed: %d\n", status);
        return 1;
    }
    if (!check_cloexec("tcp detached fd", detached_fd)) {
        close(detached_fd);
        omni_uv_tcp_close_and_free(tcp_handle);
        while (uv_loop_alive(&loop)) {
            (void)uv_run(&loop, UV_RUN_DEFAULT);
        }
        omni_uv_tcp_connect_req_free(req);
        uv_loop_close(&loop);
        close(listen_fd);
        return 1;
    }

    close(detached_fd);
    omni_uv_tcp_close_and_free(tcp_handle);
    while (uv_loop_alive(&loop)) {
        (void)uv_run(&loop, UV_RUN_DEFAULT);
    }
    omni_uv_tcp_connect_req_free(req);
    uv_loop_close(&loop);
    close(listen_fd);
    return 0;
}

static int test_pipe_helpers_cloexec(void) {
    char pipe_path[128] = "/tmp/omni_uv_helper_pipe_cloexec_XXXXXX";
    char unix_path[128] = "/tmp/omni_uv_helper_unix_cloexec_XXXXXX";
    int mk_fd = mkstemp(pipe_path);
    int listen_fd = -1;
    int unix_fd = -1;
    int status;

    if (mk_fd < 0) return fail("mkstemp");
    close(mk_fd);
    unlink(pipe_path);

    status = omni_uv_pipe_listen_fd(pipe_path, 1, &listen_fd);
    if (status < 0) {
        unlink(pipe_path);
        fprintf(stderr, "pipe listen fd helper failed: %d\n", status);
        return 1;
    }
    if (!check_cloexec("pipe listen fd", listen_fd)) {
        close(listen_fd);
        unlink(pipe_path);
        return 1;
    }

    close(listen_fd);
    unlink(pipe_path);

    mk_fd = mkstemp(unix_path);
    if (mk_fd < 0) return fail("mkstemp unix");
    close(mk_fd);
    unlink(unix_path);

    status = omni_unix_socket_listen_fd(unix_path, 1, &unix_fd);
    if (status < 0) {
        unlink(unix_path);
        fprintf(stderr, "unix socket listen fd helper failed: %d\n", status);
        return 1;
    }
    if (!check_cloexec("unix socket listen fd", unix_fd)) {
        close(unix_fd);
        unlink(unix_path);
        return 1;
    }

    close(unix_fd);
    unlink(unix_path);
    return 0;
}

static int test_process_spawn_cloexec(void) {
    char* argv[] = { "true", NULL };
    void* process = NULL;
    int stdin_fd = -1;
    int stdout_fd = -1;
    int stderr_fd = -1;
    int status;
    int exit_status = 0;
    int term_signal = 0;

    status = omni_uv_process_spawn("/bin/true", argv, NULL, &process, &stdin_fd, &stdout_fd, &stderr_fd);
    if (status < 0) {
        fprintf(stderr, "process spawn failed: %d\n", status);
        return 1;
    }
    if (!check_cloexec("process stdin fd", stdin_fd) ||
        !check_cloexec("process stdout fd", stdout_fd) ||
        !check_cloexec("process stderr fd", stderr_fd)) {
        close(stdin_fd);
        close(stdout_fd);
        close(stderr_fd);
        omni_uv_process_close(process);
        return 1;
    }

    close(stdin_fd);
    close(stdout_fd);
    close(stderr_fd);

    status = omni_uv_process_wait(process, &exit_status, &term_signal);
    if (status < 0 || exit_status != 0 || term_signal != 0) {
        omni_uv_process_close(process);
        fprintf(stderr, "process wait failed: status=%d exit=%d signal=%d\n", status, exit_status, term_signal);
        return 1;
    }

    omni_uv_process_close(process);
    return 0;
}

int main(void) {
    if (test_tcp_connect_detach_cloexec() != 0) return 1;
    if (test_pipe_helpers_cloexec() != 0) return 1;
    if (test_process_spawn_cloexec() != 0) return 1;
    return 0;
}
