#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

long omni_uv_fs_read(int fd, char* buf, size_t len);
long omni_uv_fs_write(int fd, char* buf, size_t len);
void omni_uv_fs_debug_set_max_buf_len(size_t len);
size_t omni_uv_fs_max_buf_len(void);

static int fail(const char* label) {
    perror(label);
    omni_uv_fs_debug_set_max_buf_len(0);
    return 1;
}

static int fail_msg(const char* label) {
    fprintf(stderr, "%s\n", label);
    omni_uv_fs_debug_set_max_buf_len(0);
    return 1;
}

int main(void) {
    char path[] = "/tmp/omni_uv_fs_chunk_test_XXXXXX";
    char got[32];
    char payload[] = "abcdefghijkl";
    int fd = mkstemp(path);
    if (fd < 0) return fail("mkstemp");

    omni_uv_fs_debug_set_max_buf_len(3);
    if (omni_uv_fs_max_buf_len() != 3) {
        close(fd);
        unlink(path);
        return fail_msg("debug max buffer override was not applied");
    }

    long written = omni_uv_fs_write(fd, payload, strlen(payload));
    if (written != (long)strlen(payload)) {
        close(fd);
        unlink(path);
        return fail_msg("chunked write did not report full payload length");
    }

    if (lseek(fd, 0, SEEK_SET) < 0) {
        close(fd);
        unlink(path);
        return fail("lseek");
    }

    memset(got, 0, sizeof(got));
    omni_uv_fs_debug_set_max_buf_len(2);
    long read = omni_uv_fs_read(fd, got, strlen(payload));
    if (read != (long)strlen(payload)) {
        close(fd);
        unlink(path);
        return fail_msg("chunked read did not report full payload length");
    }
    if (memcmp(got, payload, strlen(payload)) != 0) {
        close(fd);
        unlink(path);
        return fail_msg("chunked read payload mismatch");
    }

    omni_uv_fs_debug_set_max_buf_len(0);
    close(fd);
    unlink(path);
    return 0;
}
