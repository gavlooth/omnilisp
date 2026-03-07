/*
 * tls_helpers.c — BearSSL helpers for Omni Lisp
 * - Socket I/O callbacks for br_sslio_init
 * - Trust store loading (PEM CA bundle -> BearSSL trust anchors)
 */

#include <bearssl.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

    if (path == NULL || path[0] == '\0') return NULL;

    f = fopen(path, "rb");
    if (f == NULL) return NULL;

    if (!omni_tls_parse_pem_bundle(f, &vec)) {
        fclose(f);
        omni_tls_anchor_vec_free(&vec);
        return NULL;
    }
    fclose(f);

    store = (omni_tls_trust_store *)malloc(sizeof(*store));
    if (store == NULL) {
        omni_tls_anchor_vec_free(&vec);
        return NULL;
    }

    store->anchors = vec.items;
    store->count = vec.len;
    return store;
}

void *omni_tls_trust_store_load_default(void) {
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
        void *store = omni_tls_trust_store_load_file(override_path);
        if (store != NULL) return store;
    }

    if (ssl_cert_file != NULL && ssl_cert_file[0] != '\0') {
        void *store = omni_tls_trust_store_load_file(ssl_cert_file);
        if (store != NULL) return store;
    }

    for (i = 0; i < (sizeof(default_paths) / sizeof(default_paths[0])); i++) {
        void *store = omni_tls_trust_store_load_file(default_paths[i]);
        if (store != NULL) return store;
    }

    return NULL;
}

void *omni_tls_trust_store_get_anchors(void *store_handle) {
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return NULL;
    return store->anchors;
}

size_t omni_tls_trust_store_get_anchor_count(void *store_handle) {
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return 0;
    return store->count;
}

void omni_tls_trust_store_free(void *store_handle) {
    size_t i;
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return;

    if (store->anchors != NULL) {
        for (i = 0; i < store->count; i++) {
            omni_tls_free_anchor(&store->anchors[i]);
        }
        free(store->anchors);
    }

    store->anchors = NULL;
    store->count = 0;
    free(store);
}

typedef struct {
    br_x509_certificate *items;
    size_t len;
    size_t cap;
} omni_tls_cert_vec;

typedef struct {
    br_x509_certificate *chain;
    size_t chain_len;
    br_rsa_private_key rsa_key;
} omni_tls_server_creds;

static void omni_tls_rsa_private_key_free(br_rsa_private_key *key) {
    if (key == NULL) return;
    free(key->p);
    free(key->q);
    free(key->dp);
    free(key->dq);
    free(key->iq);
    memset(key, 0, sizeof(*key));
}

static void omni_tls_cert_vec_free(omni_tls_cert_vec *vec) {
    size_t i;
    if (vec == NULL) return;
    for (i = 0; i < vec->len; i++) {
        free(vec->items[i].data);
        vec->items[i].data = NULL;
        vec->items[i].data_len = 0;
    }
    free(vec->items);
    vec->items = NULL;
    vec->len = 0;
    vec->cap = 0;
}

static int omni_tls_cert_vec_push_der(omni_tls_cert_vec *vec, const unsigned char *der, size_t der_len) {
    br_x509_certificate *next;
    unsigned char *copy;

    if (vec == NULL || der == NULL || der_len == 0) return 0;

    copy = (unsigned char *)malloc(der_len);
    if (copy == NULL) return 0;
    memcpy(copy, der, der_len);

    if (vec->len == vec->cap) {
        size_t new_cap = vec->cap == 0 ? 4 : (vec->cap * 2);
        if (new_cap < vec->cap) {
            free(copy);
            return 0;
        }
        next = (br_x509_certificate *)realloc(vec->items, new_cap * sizeof(*vec->items));
        if (next == NULL) {
            free(copy);
            return 0;
        }
        vec->items = next;
        vec->cap = new_cap;
    }

    vec->items[vec->len].data = copy;
    vec->items[vec->len].data_len = der_len;
    vec->len++;
    return 1;
}

static int omni_tls_copy_part(unsigned char **dst, size_t *dst_len, const unsigned char *src, size_t src_len) {
    if (dst == NULL || dst_len == NULL) return 0;
    *dst = NULL;
    *dst_len = 0;
    if (src_len == 0) return 1;
    if (src == NULL) return 0;
    *dst = (unsigned char *)malloc(src_len);
    if (*dst == NULL) return 0;
    memcpy(*dst, src, src_len);
    *dst_len = src_len;
    return 1;
}

static int omni_tls_copy_rsa_private_key(br_rsa_private_key *dst, const br_rsa_private_key *src) {
    if (dst == NULL || src == NULL) return 0;
    memset(dst, 0, sizeof(*dst));
    dst->n_bitlen = src->n_bitlen;

    if (!omni_tls_copy_part(&dst->p, &dst->plen, src->p, src->plen) ||
            !omni_tls_copy_part(&dst->q, &dst->qlen, src->q, src->qlen) ||
            !omni_tls_copy_part(&dst->dp, &dst->dplen, src->dp, src->dplen) ||
            !omni_tls_copy_part(&dst->dq, &dst->dqlen, src->dq, src->dqlen) ||
            !omni_tls_copy_part(&dst->iq, &dst->iqlen, src->iq, src->iqlen)) {
        omni_tls_rsa_private_key_free(dst);
        return 0;
    }
    return 1;
}

static int omni_tls_parse_cert_chain_pem(FILE *f, omni_tls_cert_vec *out_chain) {
    br_pem_decoder_context pc;
    omni_tls_bytes cert_buf = {0};
    unsigned char inbuf[4096];
    int in_cert_obj = 0;
    int saw_pem = 0;

    if (f == NULL || out_chain == NULL) return 0;

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
                        if (cert_buf.oom || cert_buf.len == 0 ||
                                !omni_tls_cert_vec_push_der(out_chain, cert_buf.data, cert_buf.len)) {
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
    if (saw_pem && out_chain->len == 0) return 0;
    return out_chain->len > 0;
}

static int omni_tls_parse_rsa_key_pem(FILE *f, br_rsa_private_key *out_key) {
    br_pem_decoder_context pc;
    omni_tls_bytes key_buf = {0};
    unsigned char inbuf[4096];
    int in_key_obj = 0;

    if (f == NULL || out_key == NULL) return 0;
    memset(out_key, 0, sizeof(*out_key));
    br_pem_decoder_init(&pc);

    for (;;) {
        size_t rlen = fread(inbuf, 1, sizeof(inbuf), f);
        size_t off = 0;
        if (rlen == 0 && feof(f)) break;
        if (rlen == 0 && ferror(f)) {
            omni_tls_bytes_reset(&key_buf);
            return 0;
        }

        while (off < rlen) {
            size_t consumed = br_pem_decoder_push(&pc, inbuf + off, rlen - off);
            int ev;
            off += consumed;

            while ((ev = br_pem_decoder_event(&pc)) != 0) {
                if (ev == BR_PEM_BEGIN_OBJ) {
                    const char *name = br_pem_decoder_name(&pc);
                    omni_tls_bytes_reset(&key_buf);
                    if (name != NULL &&
                            (strcmp(name, "PRIVATE KEY") == 0 ||
                             strcmp(name, "RSA PRIVATE KEY") == 0)) {
                        in_key_obj = 1;
                        br_pem_decoder_setdest(&pc, &omni_tls_append_bytes_cb, &key_buf);
                    } else {
                        in_key_obj = 0;
                        br_pem_decoder_setdest(&pc, NULL, NULL);
                    }
                } else if (ev == BR_PEM_END_OBJ) {
                    if (in_key_obj) {
                        br_skey_decoder_context skd;
                        const br_rsa_private_key *decoded_rsa;
                        br_skey_decoder_init(&skd);
                        br_skey_decoder_push(&skd, key_buf.data, key_buf.len);
                        decoded_rsa = br_skey_decoder_get_rsa(&skd);
                        if (!key_buf.oom &&
                                br_skey_decoder_last_error(&skd) == 0 &&
                                decoded_rsa != NULL &&
                                omni_tls_copy_rsa_private_key(out_key, decoded_rsa)) {
                            omni_tls_bytes_reset(&key_buf);
                            return 1;
                        }
                        omni_tls_bytes_reset(&key_buf);
                        return 0;
                    }
                    in_key_obj = 0;
                    br_pem_decoder_setdest(&pc, NULL, NULL);
                    omni_tls_bytes_reset(&key_buf);
                } else if (ev == BR_PEM_ERROR) {
                    omni_tls_bytes_reset(&key_buf);
                    return 0;
                }
            }
        }
    }

    omni_tls_bytes_reset(&key_buf);
    return 0;
}

size_t omni_tls_server_context_size(void) {
    return sizeof(br_ssl_server_context);
}

void *omni_tls_server_creds_load(const char *cert_pem_path, const char *key_pem_path) {
    FILE *cert_f = NULL;
    FILE *key_f = NULL;
    omni_tls_cert_vec cert_chain = {0};
    br_rsa_private_key rsa_key = {0};
    omni_tls_server_creds *creds = NULL;

    if (cert_pem_path == NULL || cert_pem_path[0] == '\0' ||
            key_pem_path == NULL || key_pem_path[0] == '\0') {
        return NULL;
    }

    cert_f = fopen(cert_pem_path, "rb");
    if (cert_f == NULL) return NULL;
    if (!omni_tls_parse_cert_chain_pem(cert_f, &cert_chain)) {
        fclose(cert_f);
        omni_tls_cert_vec_free(&cert_chain);
        return NULL;
    }
    fclose(cert_f);

    key_f = fopen(key_pem_path, "rb");
    if (key_f == NULL) {
        omni_tls_cert_vec_free(&cert_chain);
        return NULL;
    }
    if (!omni_tls_parse_rsa_key_pem(key_f, &rsa_key)) {
        fclose(key_f);
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_rsa_private_key_free(&rsa_key);
        return NULL;
    }
    fclose(key_f);

    creds = (omni_tls_server_creds *)malloc(sizeof(*creds));
    if (creds == NULL) {
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_rsa_private_key_free(&rsa_key);
        return NULL;
    }
    memset(creds, 0, sizeof(*creds));

    creds->chain = cert_chain.items;
    creds->chain_len = cert_chain.len;
    creds->rsa_key = rsa_key;
    return creds;
}

void *omni_tls_server_creds_chain(void *creds_handle) {
    omni_tls_server_creds *creds = (omni_tls_server_creds *)creds_handle;
    if (creds == NULL) return NULL;
    return creds->chain;
}

size_t omni_tls_server_creds_chain_len(void *creds_handle) {
    omni_tls_server_creds *creds = (omni_tls_server_creds *)creds_handle;
    if (creds == NULL) return 0;
    return creds->chain_len;
}

void *omni_tls_server_creds_rsa_key(void *creds_handle) {
    omni_tls_server_creds *creds = (omni_tls_server_creds *)creds_handle;
    if (creds == NULL) return NULL;
    return &creds->rsa_key;
}

void omni_tls_server_creds_free(void *creds_handle) {
    size_t i;
    omni_tls_server_creds *creds = (omni_tls_server_creds *)creds_handle;
    if (creds == NULL) return;

    if (creds->chain != NULL) {
        for (i = 0; i < creds->chain_len; i++) {
            free(creds->chain[i].data);
            creds->chain[i].data = NULL;
            creds->chain[i].data_len = 0;
        }
        free(creds->chain);
    }

    creds->chain = NULL;
    creds->chain_len = 0;
    omni_tls_rsa_private_key_free(&creds->rsa_key);
    free(creds);
}

int omni_tls_client_set_single_rsa(void *client_ctx, void *creds_handle) {
    br_rsa_pkcs1_sign sign_impl;
    omni_tls_server_creds *creds = (omni_tls_server_creds *)creds_handle;
    if (client_ctx == NULL || creds == NULL || creds->chain == NULL || creds->chain_len == 0) {
        return 0;
    }

    sign_impl = br_rsa_pkcs1_sign_get_default();
    if (sign_impl == 0) return 0;

    br_ssl_client_set_single_rsa(
        (br_ssl_client_context *)client_ctx,
        creds->chain,
        creds->chain_len,
        &creds->rsa_key,
        sign_impl);
    return 1;
}

typedef struct omni_tls_session_cache_entry {
    char *hostname;
    br_ssl_session_parameters params;
    struct omni_tls_session_cache_entry *next;
} omni_tls_session_cache_entry;

static omni_tls_session_cache_entry *g_omni_tls_session_cache_head = NULL;
static pthread_mutex_t g_omni_tls_session_cache_lock = PTHREAD_MUTEX_INITIALIZER;

static void omni_tls_session_cache_entry_free(omni_tls_session_cache_entry *entry) {
    if (entry == NULL) return;
    free(entry->hostname);
    entry->hostname = NULL;
    free(entry);
}

static omni_tls_session_cache_entry *omni_tls_session_cache_find_locked(const char *hostname) {
    omni_tls_session_cache_entry *cur = g_omni_tls_session_cache_head;
    while (cur != NULL) {
        if (cur->hostname != NULL && strcmp(cur->hostname, hostname) == 0) {
            return cur;
        }
        cur = cur->next;
    }
    return NULL;
}

int omni_tls_client_session_cache_load(void *client_ctx, const char *hostname) {
    omni_tls_session_cache_entry *entry;
    if (client_ctx == NULL || hostname == NULL || hostname[0] == '\0') return 0;

    pthread_mutex_lock(&g_omni_tls_session_cache_lock);
    entry = omni_tls_session_cache_find_locked(hostname);
    if (entry == NULL) {
        pthread_mutex_unlock(&g_omni_tls_session_cache_lock);
        return 0;
    }

    br_ssl_engine_set_session_parameters((br_ssl_engine_context *)client_ctx, &entry->params);
    pthread_mutex_unlock(&g_omni_tls_session_cache_lock);
    return 1;
}

int omni_tls_client_session_cache_store(void *client_ctx, const char *hostname) {
    br_ssl_session_parameters params;
    omni_tls_session_cache_entry *entry;
    if (client_ctx == NULL || hostname == NULL || hostname[0] == '\0') return 0;

    memset(&params, 0, sizeof(params));
    br_ssl_engine_get_session_parameters((br_ssl_engine_context *)client_ctx, &params);

    pthread_mutex_lock(&g_omni_tls_session_cache_lock);
    entry = omni_tls_session_cache_find_locked(hostname);
    if (entry == NULL) {
        entry = (omni_tls_session_cache_entry *)malloc(sizeof(*entry));
        if (entry == NULL) {
            pthread_mutex_unlock(&g_omni_tls_session_cache_lock);
            return 0;
        }
        memset(entry, 0, sizeof(*entry));
        entry->hostname = strdup(hostname);
        if (entry->hostname == NULL) {
            omni_tls_session_cache_entry_free(entry);
            pthread_mutex_unlock(&g_omni_tls_session_cache_lock);
            return 0;
        }
        entry->next = g_omni_tls_session_cache_head;
        g_omni_tls_session_cache_head = entry;
    }

    entry->params = params;
    pthread_mutex_unlock(&g_omni_tls_session_cache_lock);
    return 1;
}
