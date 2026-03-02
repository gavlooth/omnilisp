/*
 * tls_helpers.c — Minimal BearSSL helpers for Omni Lisp
 * Only needed for: socket I/O callbacks (C3 can't easily match the exact signature)
 */

#include <unistd.h>
#include <errno.h>

/* Socket read callback for br_sslio_init */
int omni_tls_sock_read(void *ctx, unsigned char *buf, size_t len) {
    int fd = *(int *)ctx;
    for (;;) {
        long rlen = read(fd, buf, len);
        if (rlen <= 0) {
            if (rlen < 0 && errno == EINTR) continue;
            return -1;
        }
        return (int)rlen;
    }
}

/* Socket write callback for br_sslio_init */
int omni_tls_sock_write(void *ctx, const unsigned char *buf, size_t len) {
    int fd = *(int *)ctx;
    for (;;) {
        long wlen = write(fd, buf, len);
        if (wlen <= 0) {
            if (wlen < 0 && errno == EINTR) continue;
            return -1;
        }
        return (int)wlen;
    }
}
