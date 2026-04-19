/*
 * tls_helpers.c — BearSSL helpers for Omni Lisp
 * - Socket I/O callbacks for br_sslio_init
 * - Trust store loading (PEM CA bundle -> BearSSL trust anchors)
 */

#include "../deps/src/BearSSL/inc/bearssl.h"
#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void omni_tls_set_last_errorf(const char *fmt, ...);
void omni_tls_clear_last_error(void);
const char *omni_tls_last_error(void);
int omni_tls_client_last_error(void *client_ctx);
const char *omni_tls_error_code_string(int err);

/* Socket read callback for br_sslio_init */
int omni_tls_sock_read(void *ctx, unsigned char *buf, size_t len) {
    int fd = *(int *)ctx;
    omni_tls_clear_last_error();
    for (;;) {
        long rlen = read(fd, buf, len);
        if (rlen <= 0) {
            if (rlen < 0 && errno == EINTR) continue;
            if (rlen < 0) {
                omni_tls_set_last_errorf("socket read failed: %s", strerror(errno));
            } else {
                omni_tls_set_last_errorf("socket read failed: end of stream");
            }
            return -1;
        }
        return (int)rlen;
    }
}

/* Socket write callback for br_sslio_init */
int omni_tls_sock_write(void *ctx, const unsigned char *buf, size_t len) {
    int fd = *(int *)ctx;
    omni_tls_clear_last_error();
    for (;;) {
        long wlen = write(fd, buf, len);
        if (wlen <= 0) {
            if (wlen < 0 && errno == EINTR) continue;
            if (wlen < 0) {
                omni_tls_set_last_errorf("socket write failed: %s", strerror(errno));
            } else {
                omni_tls_set_last_errorf("socket write failed: short write");
            }
            return -1;
        }
        return (int)wlen;
    }
}

typedef struct {
    unsigned char *data;
    size_t len;
    size_t cap;
    int oom;
} omni_tls_bytes;

typedef struct {
    br_x509_trust_anchor *items;
    size_t len;
    size_t cap;
} omni_tls_anchor_vec;

typedef struct {
    br_x509_trust_anchor *anchors;
    size_t count;
} omni_tls_trust_store;

static pthread_mutex_t g_default_store_lock = PTHREAD_MUTEX_INITIALIZER;
static omni_tls_trust_store *g_default_store = NULL;
static __thread char g_omni_tls_last_error[256];
static __thread char g_omni_tls_engine_error[128];

void omni_tls_clear_last_error(void) {
    g_omni_tls_last_error[0] = '\0';
}

const char *omni_tls_last_error(void) {
    if (g_omni_tls_last_error[0] == '\0') return NULL;
    return g_omni_tls_last_error;
}

static void omni_tls_set_last_errorf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(g_omni_tls_last_error, sizeof(g_omni_tls_last_error), fmt, ap);
    g_omni_tls_last_error[sizeof(g_omni_tls_last_error) - 1] = '\0';
    va_end(ap);
}

int omni_tls_client_last_error(void *client_ctx) {
    if (client_ctx == NULL) return BR_ERR_BAD_PARAM;
    return br_ssl_engine_last_error((const br_ssl_engine_context *)client_ctx);
}

const char *omni_tls_error_code_string(int err) {
    switch (err) {
        case BR_ERR_OK:
            return "no TLS error";
        case BR_ERR_BAD_PARAM:
            return "invalid TLS parameter";
        case BR_ERR_BAD_STATE:
            return "invalid TLS engine state";
        case BR_ERR_UNSUPPORTED_VERSION:
            return "unsupported TLS version";
        case BR_ERR_BAD_VERSION:
            return "unexpected TLS version";
        case BR_ERR_BAD_LENGTH:
            return "invalid TLS record length";
        case BR_ERR_TOO_LARGE:
            return "TLS record or handshake message too large";
        case BR_ERR_BAD_MAC:
            return "TLS record authentication failed";
        case BR_ERR_NO_RANDOM:
            return "TLS entropy source unavailable";
        case BR_ERR_UNKNOWN_TYPE:
            return "unknown TLS record type";
        case BR_ERR_UNEXPECTED:
            return "unexpected TLS handshake message";
        case BR_ERR_BAD_CCS:
            return "invalid TLS change-cipher-spec";
        case BR_ERR_BAD_ALERT:
            return "invalid TLS alert";
        case BR_ERR_BAD_HANDSHAKE:
            return "invalid TLS handshake payload";
        case BR_ERR_OVERSIZED_ID:
            return "oversized TLS session identifier";
        case BR_ERR_BAD_CIPHER_SUITE:
            return "unsupported TLS cipher suite";
        case BR_ERR_BAD_COMPRESSION:
            return "unsupported TLS compression";
        case BR_ERR_BAD_FRAGLEN:
            return "invalid TLS fragment length";
        case BR_ERR_BAD_SECRENEG:
            return "TLS secure renegotiation failed";
        case BR_ERR_EXTRA_EXTENSION:
            return "unexpected TLS extension";
        case BR_ERR_BAD_SNI:
            return "invalid TLS server name indication";
        case BR_ERR_BAD_HELLO_DONE:
            return "invalid TLS hello-done message";
        case BR_ERR_LIMIT_EXCEEDED:
            return "TLS implementation limit exceeded";
        case BR_ERR_BAD_FINISHED:
            return "TLS finished verification failed";
        case BR_ERR_RESUME_MISMATCH:
            return "TLS session resumption mismatch";
        case BR_ERR_INVALID_ALGORITHM:
            return "invalid TLS algorithm selection";
        case BR_ERR_BAD_SIGNATURE:
            return "invalid TLS signature";
        case BR_ERR_WRONG_KEY_USAGE:
            return "invalid TLS key usage";
        case BR_ERR_NO_CLIENT_AUTH:
            return "TLS client authentication failed";
        case BR_ERR_IO:
            return "TLS transport I/O failed";
        default:
            break;
    }

    if (err >= BR_ERR_SEND_FATAL_ALERT && err < (BR_ERR_SEND_FATAL_ALERT + 256)) {
        snprintf(g_omni_tls_engine_error, sizeof(g_omni_tls_engine_error),
            "TLS sent fatal alert %d", err - BR_ERR_SEND_FATAL_ALERT);
        g_omni_tls_engine_error[sizeof(g_omni_tls_engine_error) - 1] = '\0';
        return g_omni_tls_engine_error;
    }
    if (err >= BR_ERR_RECV_FATAL_ALERT && err < (BR_ERR_RECV_FATAL_ALERT + 256)) {
        snprintf(g_omni_tls_engine_error, sizeof(g_omni_tls_engine_error),
            "TLS received fatal alert %d", err - BR_ERR_RECV_FATAL_ALERT);
        g_omni_tls_engine_error[sizeof(g_omni_tls_engine_error) - 1] = '\0';
        return g_omni_tls_engine_error;
    }

    snprintf(g_omni_tls_engine_error, sizeof(g_omni_tls_engine_error),
        "unknown TLS engine error %d", err);
    g_omni_tls_engine_error[sizeof(g_omni_tls_engine_error) - 1] = '\0';
    return g_omni_tls_engine_error;
}

static void omni_tls_bytes_reset(omni_tls_bytes *b) {
    if (b == NULL) return;
    free(b->data);
    b->data = NULL;
    b->len = 0;
    b->cap = 0;
    b->oom = 0;
}

static int omni_tls_bytes_reserve(omni_tls_bytes *b, size_t needed) {
    size_t new_cap;
    unsigned char *p;
    if (needed <= b->cap) return 1;
    new_cap = b->cap == 0 ? 1024 : b->cap;
    while (new_cap < needed) {
        if (new_cap > ((size_t)-1) / 2) return 0;
        new_cap *= 2;
    }
    p = (unsigned char *)realloc(b->data, new_cap);
    if (p == NULL) return 0;
    b->data = p;
    b->cap = new_cap;
    return 1;
}

static void omni_tls_bytes_append(omni_tls_bytes *b, const void *src, size_t len) {
    if (b == NULL || src == NULL || len == 0 || b->oom) return;
    if (b->len > ((size_t)-1) - len) {
        b->oom = 1;
        return;
    }
    if (!omni_tls_bytes_reserve(b, b->len + len)) {
        b->oom = 1;
        return;
    }
    memcpy(b->data + b->len, src, len);
    b->len += len;
}

static void omni_tls_free_anchor(br_x509_trust_anchor *ta) {
    if (ta == NULL) return;
    free(ta->dn.data);
    ta->dn.data = NULL;
    ta->dn.len = 0;

    if (ta->pkey.key_type == BR_KEYTYPE_RSA) {
        free(ta->pkey.key.rsa.n);
        free(ta->pkey.key.rsa.e);
        ta->pkey.key.rsa.n = NULL;
        ta->pkey.key.rsa.e = NULL;
        ta->pkey.key.rsa.nlen = 0;
        ta->pkey.key.rsa.elen = 0;
    } else if (ta->pkey.key_type == BR_KEYTYPE_EC) {
        free(ta->pkey.key.ec.q);
        ta->pkey.key.ec.q = NULL;
        ta->pkey.key.ec.qlen = 0;
        ta->pkey.key.ec.curve = 0;
    }
    ta->flags = 0;
    ta->pkey.key_type = 0;
}

static void omni_tls_anchor_vec_free(omni_tls_anchor_vec *vec) {
    size_t i;
    if (vec == NULL) return;
    for (i = 0; i < vec->len; i++) {
        omni_tls_free_anchor(&vec->items[i]);
    }
    free(vec->items);
    vec->items = NULL;
    vec->len = 0;
    vec->cap = 0;
}

static int omni_tls_anchor_vec_push(omni_tls_anchor_vec *vec, const br_x509_trust_anchor *ta) {
    br_x509_trust_anchor *next;
    if (vec->len == vec->cap) {
        size_t new_cap = vec->cap == 0 ? 16 : (vec->cap * 2);
        if (new_cap < vec->cap) return 0;
        next = (br_x509_trust_anchor *)realloc(vec->items, new_cap * sizeof(*vec->items));
        if (next == NULL) return 0;
        vec->items = next;
        vec->cap = new_cap;
    }
    vec->items[vec->len++] = *ta;
    return 1;
}

static void omni_tls_append_bytes_cb(void *ctx, const void *src, size_t len) {
    omni_tls_bytes_append((omni_tls_bytes *)ctx, src, len);
}

static int omni_tls_decode_cert_to_anchor(const unsigned char *cert_der, size_t cert_len, br_x509_trust_anchor *out_ta) {
    br_x509_decoder_context dc;
    br_x509_pkey *pkey;
    omni_tls_bytes subject_dn = {0};
    br_x509_trust_anchor ta;

    if (cert_der == NULL || cert_len == 0 || out_ta == NULL) return 0;

    memset(&ta, 0, sizeof(ta));
    br_x509_decoder_init(&dc, &omni_tls_append_bytes_cb, &subject_dn);
    br_x509_decoder_push(&dc, cert_der, cert_len);

    if (subject_dn.oom || br_x509_decoder_last_error(&dc) != 0 || subject_dn.len == 0) {
        omni_tls_bytes_reset(&subject_dn);
        return 0;
    }

    pkey = br_x509_decoder_get_pkey(&dc);
    if (pkey == NULL) {
        omni_tls_bytes_reset(&subject_dn);
        return 0;
    }

    ta.dn.data = (unsigned char *)malloc(subject_dn.len);
    if (ta.dn.data == NULL) {
        omni_tls_bytes_reset(&subject_dn);
        return 0;
    }
    memcpy(ta.dn.data, subject_dn.data, subject_dn.len);
    ta.dn.len = subject_dn.len;

    ta.flags = br_x509_decoder_isCA(&dc) ? BR_X509_TA_CA : 0;
    ta.pkey.key_type = pkey->key_type;

    if (pkey->key_type == BR_KEYTYPE_RSA) {
        ta.pkey.key.rsa.n = (unsigned char *)malloc(pkey->key.rsa.nlen);
        ta.pkey.key.rsa.e = (unsigned char *)malloc(pkey->key.rsa.elen);
        if (ta.pkey.key.rsa.n == NULL || ta.pkey.key.rsa.e == NULL) {
            omni_tls_free_anchor(&ta);
            omni_tls_bytes_reset(&subject_dn);
            return 0;
        }
        memcpy(ta.pkey.key.rsa.n, pkey->key.rsa.n, pkey->key.rsa.nlen);
        memcpy(ta.pkey.key.rsa.e, pkey->key.rsa.e, pkey->key.rsa.elen);
        ta.pkey.key.rsa.nlen = pkey->key.rsa.nlen;
        ta.pkey.key.rsa.elen = pkey->key.rsa.elen;
    } else if (pkey->key_type == BR_KEYTYPE_EC) {
        ta.pkey.key.ec.q = (unsigned char *)malloc(pkey->key.ec.qlen);
        if (ta.pkey.key.ec.q == NULL) {
            omni_tls_free_anchor(&ta);
            omni_tls_bytes_reset(&subject_dn);
            return 0;
        }
        memcpy(ta.pkey.key.ec.q, pkey->key.ec.q, pkey->key.ec.qlen);
        ta.pkey.key.ec.qlen = pkey->key.ec.qlen;
        ta.pkey.key.ec.curve = pkey->key.ec.curve;
    } else {
        omni_tls_free_anchor(&ta);
        omni_tls_bytes_reset(&subject_dn);
        return 0;
    }

    *out_ta = ta;
    omni_tls_bytes_reset(&subject_dn);
    return 1;
}

static int omni_tls_parse_pem_bundle(FILE *f, omni_tls_anchor_vec *out_vec) {
    br_pem_decoder_context pc;
    omni_tls_bytes cert_buf = {0};
    unsigned char inbuf[4096];
    int in_cert_obj = 0;
    int saw_pem = 0;

    if (f == NULL || out_vec == NULL) return 0;

    br_pem_decoder_init(&pc);

    for (;;) {
        size_t rlen = fread(inbuf, 1, sizeof(inbuf), f);
        size_t off = 0;

        if (rlen == 0 && feof(f)) break;
        if (rlen == 0 && ferror(f)) {
            omni_tls_bytes_reset(&cert_buf);
            return 0;
        }

        while (off < rlen) {
            size_t consumed = br_pem_decoder_push(&pc, inbuf + off, rlen - off);
            int ev;
            off += consumed;

            while ((ev = br_pem_decoder_event(&pc)) != 0) {
                if (ev == BR_PEM_BEGIN_OBJ) {
                    const char *name = br_pem_decoder_name(&pc);
                    saw_pem = 1;
                    omni_tls_bytes_reset(&cert_buf);
                    if (name != NULL && strcmp(name, "CERTIFICATE") == 0) {
                        in_cert_obj = 1;
                        br_pem_decoder_setdest(&pc, &omni_tls_append_bytes_cb, &cert_buf);
                    } else {
                        in_cert_obj = 0;
                        br_pem_decoder_setdest(&pc, NULL, NULL);
                    }
                } else if (ev == BR_PEM_END_OBJ) {
                    if (in_cert_obj) {
                        br_x509_trust_anchor ta;
                        if (!cert_buf.oom && cert_buf.len > 0 &&
                                omni_tls_decode_cert_to_anchor(cert_buf.data, cert_buf.len, &ta) &&
                                omni_tls_anchor_vec_push(out_vec, &ta)) {
                            /* accepted */
                        } else {
                            omni_tls_bytes_reset(&cert_buf);
                            return 0;
                        }
                    }
                    in_cert_obj = 0;
                    br_pem_decoder_setdest(&pc, NULL, NULL);
                    omni_tls_bytes_reset(&cert_buf);
                } else if (ev == BR_PEM_ERROR) {
                    omni_tls_bytes_reset(&cert_buf);
                    return 0;
                }
            }
        }
    }

    omni_tls_bytes_reset(&cert_buf);

    /* If file had PEM envelopes, require at least one parsed anchor. */
    if (saw_pem && out_vec->len == 0) {
        return 0;
    }

    return out_vec->len > 0;
}

void *omni_tls_trust_store_load_file(const char *path) {
    FILE *f;
    omni_tls_anchor_vec vec = {0};
    omni_tls_trust_store *store;

    omni_tls_clear_last_error();
    if (path == NULL || path[0] == '\0') return NULL;

    f = fopen(path, "rb");
    if (f == NULL) {
        omni_tls_set_last_errorf("failed to open trust store '%s': %s", path, strerror(errno));
        return NULL;
    }

    if (!omni_tls_parse_pem_bundle(f, &vec)) {
        fclose(f);
        omni_tls_anchor_vec_free(&vec);
        omni_tls_set_last_errorf("failed to parse trust store '%s' as PEM certificates", path);
        return NULL;
    }
    fclose(f);

    store = (omni_tls_trust_store *)malloc(sizeof(*store));
    if (store == NULL) {
        omni_tls_anchor_vec_free(&vec);
        omni_tls_set_last_errorf("failed to allocate trust store for '%s'", path);
        return NULL;
    }

    store->anchors = vec.items;
    store->count = vec.len;
    return store;
}

static omni_tls_trust_store *omni_tls_try_load_default_store(void) {
    static const char *default_paths[] = {
        "/etc/ssl/certs/ca-certificates.crt",
        "/etc/pki/tls/certs/ca-bundle.crt",
        "/etc/ssl/ca-bundle.pem",
        "/etc/pki/tls/cacert.pem",
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",
        "/etc/ssl/cert.pem"
    };
    const char *override_path = getenv("OMNI_TLS_CA_FILE");
    const char *ssl_cert_file = getenv("SSL_CERT_FILE");
    size_t i;

    if (override_path != NULL && override_path[0] != '\0') {
        omni_tls_trust_store *store = (omni_tls_trust_store *)omni_tls_trust_store_load_file(override_path);
        if (store != NULL) return store;
    }

    if (ssl_cert_file != NULL && ssl_cert_file[0] != '\0') {
        omni_tls_trust_store *store = (omni_tls_trust_store *)omni_tls_trust_store_load_file(ssl_cert_file);
        if (store != NULL) return store;
    }

    for (i = 0; i < (sizeof(default_paths) / sizeof(default_paths[0])); i++) {
        omni_tls_trust_store *store = (omni_tls_trust_store *)omni_tls_trust_store_load_file(default_paths[i]);
        if (store != NULL) return store;
    }

    return NULL;
}

void *omni_tls_trust_store_load_default(void) {
    void *store;

    omni_tls_clear_last_error();
    if (pthread_mutex_lock(&g_default_store_lock) != 0) {
        omni_tls_set_last_errorf("failed to lock default trust store cache");
        return NULL;
    }

    if (g_default_store == NULL) {
        g_default_store = omni_tls_try_load_default_store();
    }
    store = g_default_store;
    pthread_mutex_unlock(&g_default_store_lock);
    return store;
}

void *omni_tls_trust_store_get_anchors(void *store_handle) {
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return NULL;
    return store->anchors;
}

#include "tls_helpers_server_creds.inc"
