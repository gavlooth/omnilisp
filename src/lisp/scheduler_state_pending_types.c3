module lisp;

struct PendingTcpRead {
    bool active;
    bool completed;
    bool consumed;
    bool timed_out;
    bool readable_pending;
    bool writable_pending;
    bool write_mode;
    int fd;
    usz max_bytes;
    char* buffer;
    char* write_buffer;
    usz write_len;
    usz write_offset;
    long nread;
    long nwritten;
    int error_code;
    void* poll_handle;
    void* timer_handle;
}

struct PendingOffload {
    bool active;
    bool completed;
    uint wake_epoch;
    void* completion; // OffloadCompletion*
}

struct PendingSleep {
    bool active;
    bool completed;
    void* timer_handle;
}

struct PendingDnsResolve {
    bool active;
    bool completed;
    int status;
    uint request_token;
    void* req_handle;
    void* addrinfo_result;
    char* host_copy;
}

struct PendingTcpConnect {
    bool active;
    bool completed;
    int status;
    uint request_token;
    void* req_handle; // uv_connect_t*
    void* tcp_handle; // uv_tcp_t*
}

struct PendingDnsCallbackCtx {
    usz fiber_id;
    uint request_token;
    char* host_copy;
}

struct PendingConnectCallbackCtx {
    usz fiber_id;
    uint request_token;
    void* tcp_handle;
}

struct PendingTcpAccept {
    bool active;
    bool completed;
    int status;
    bool borrowed_server_handle;
    bool has_accepted_fd;
    int accepted_fd;
    void* server_handle; // uv_stream_t* (opened on dup(listener_fd))
    void* client_handle; // uv_stream_t*
}
