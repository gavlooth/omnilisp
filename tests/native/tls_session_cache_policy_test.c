#include "../../deps/src/BearSSL/inc/bearssl.h"

#include <stdio.h>
#include <string.h>

int omni_tls_client_session_cache_load(void *client_ctx, const char *hostname);
int omni_tls_client_session_cache_store(void *client_ctx, const char *hostname);
void omni_tls_client_session_cache_clear(void);
size_t omni_tls_client_session_cache_size(void);
size_t omni_tls_client_session_cache_capacity(void);

static int expect_int(const char *label, int actual, int expected) {
    if (actual == expected) return 1;
    fprintf(stderr, "%s: got %d, expected %d\n", label, actual, expected);
    return 0;
}

static int expect_size(const char *label, size_t actual, size_t expected) {
    if (actual == expected) return 1;
    fprintf(stderr, "%s: got %zu, expected %zu\n", label, actual, expected);
    return 0;
}

static void seed_session(br_ssl_client_context *ctx, unsigned char seed) {
    br_ssl_session_parameters params;

    br_ssl_client_zero(ctx);
    memset(&params, 0, sizeof(params));
    params.session_id_len = 1;
    params.session_id[0] = seed;
    params.version = BR_TLS12;
    params.cipher_suite = BR_TLS_RSA_WITH_AES_128_GCM_SHA256;
    memset(params.master_secret, seed, sizeof(params.master_secret));
    br_ssl_engine_set_session_parameters((br_ssl_engine_context *)ctx, &params);
}

static int store_host(size_t index) {
    br_ssl_client_context ctx;
    char host[64];

    snprintf(host, sizeof(host), "host-%03zu.example", index);
    seed_session(&ctx, (unsigned char)(index + 1));
    return omni_tls_client_session_cache_store(&ctx, host);
}

static int load_host(size_t index) {
    br_ssl_client_context ctx;
    char host[64];

    snprintf(host, sizeof(host), "host-%03zu.example", index);
    seed_session(&ctx, 0);
    return omni_tls_client_session_cache_load(&ctx, host);
}

int main(void) {
    size_t cap;
    size_t i;

    omni_tls_client_session_cache_clear();
    cap = omni_tls_client_session_cache_capacity();
    if (!expect_size("capacity", cap, 64)) return 1;
    if (!expect_size("initial size", omni_tls_client_session_cache_size(), 0)) return 1;

    for (i = 0; i < cap + 3; i++) {
        if (!expect_int("store", store_host(i), 1)) return 1;
    }
    if (!expect_size("bounded size", omni_tls_client_session_cache_size(), cap)) return 1;
    if (!expect_int("oldest entry evicted", load_host(0), 0)) return 1;
    if (!expect_int("third oldest entry evicted", load_host(2), 0)) return 1;
    if (!expect_int("first retained entry loads", load_host(3), 1)) return 1;

    omni_tls_client_session_cache_clear();
    for (i = 0; i < cap; i++) {
        if (!expect_int("store refill", store_host(i), 1)) return 1;
    }
    if (!expect_int("load refreshes LRU", load_host(0), 1)) return 1;
    if (!expect_int("overflow store", store_host(cap), 1)) return 1;
    if (!expect_size("post-overflow bounded size", omni_tls_client_session_cache_size(), cap)) return 1;
    if (!expect_int("refreshed entry retained", load_host(0), 1)) return 1;
    if (!expect_int("least recently used entry evicted", load_host(1), 0)) return 1;

    omni_tls_client_session_cache_clear();
    if (!expect_size("cleared size", omni_tls_client_session_cache_size(), 0)) return 1;
    return 0;
}
