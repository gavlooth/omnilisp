#include <arpa/inet.h>
#include <netdb.h>
#include <stddef.h>
#include <sys/socket.h>
#include <unistd.h>

enum {
    OMNI_ADDRINFO_RENDER_OK = 0,
    OMNI_ADDRINFO_RENDER_UNSUPPORTED_FAMILY = 1,
    OMNI_ADDRINFO_RENDER_FAILED = -1
};

int omni_addrinfo_connect_fd(void* result) {
    if (result == NULL) return -1;

    struct addrinfo* ai = (struct addrinfo*)result;
    if (ai->ai_addr == NULL || ai->ai_addrlen == 0) return -1;

    int fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (fd < 0) return -1;

    if (connect(fd, ai->ai_addr, ai->ai_addrlen) < 0) {
        close(fd);
        return -1;
    }

    return fd;
}

int omni_addrinfo_render_ip(void* result, char* dst, unsigned int dst_size) {
    if (result == NULL || dst == NULL || dst_size == 0) return OMNI_ADDRINFO_RENDER_FAILED;

    struct addrinfo* ai = (struct addrinfo*)result;
    if (ai->ai_addr == NULL) return OMNI_ADDRINFO_RENDER_FAILED;

    const void* source = NULL;
    if (ai->ai_family == AF_INET) {
        source = &((struct sockaddr_in*)ai->ai_addr)->sin_addr;
    } else if (ai->ai_family == AF_INET6) {
        source = &((struct sockaddr_in6*)ai->ai_addr)->sin6_addr;
    } else {
        return OMNI_ADDRINFO_RENDER_UNSUPPORTED_FAMILY;
    }

    if (inet_ntop(ai->ai_family, source, dst, dst_size) == NULL) {
        return OMNI_ADDRINFO_RENDER_FAILED;
    }

    return OMNI_ADDRINFO_RENDER_OK;
}
