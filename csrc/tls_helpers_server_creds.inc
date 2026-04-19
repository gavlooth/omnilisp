size_t omni_tls_trust_store_get_anchor_count(void *store_handle) {
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return 0;
    return store->count;
}

void omni_tls_trust_store_free(void *store_handle) {
    size_t i;
    omni_tls_trust_store *store = (omni_tls_trust_store *)store_handle;
    if (store == NULL) return;

    if (pthread_mutex_lock(&g_default_store_lock) == 0) {
        if (store == g_default_store) {
            pthread_mutex_unlock(&g_default_store_lock);
            return;
        }
        pthread_mutex_unlock(&g_default_store_lock);
    }

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

    omni_tls_clear_last_error();
    if (cert_pem_path == NULL || cert_pem_path[0] == '\0' ||
            key_pem_path == NULL || key_pem_path[0] == '\0') {
        omni_tls_set_last_errorf("server credentials require both certificate and key paths");
        return NULL;
    }

    cert_f = fopen(cert_pem_path, "rb");
    if (cert_f == NULL) {
        omni_tls_set_last_errorf("failed to open server certificate '%s': %s", cert_pem_path, strerror(errno));
        return NULL;
    }
    if (!omni_tls_parse_cert_chain_pem(cert_f, &cert_chain)) {
        fclose(cert_f);
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_set_last_errorf("failed to parse server certificate chain '%s' as PEM", cert_pem_path);
        return NULL;
    }
    fclose(cert_f);

    key_f = fopen(key_pem_path, "rb");
    if (key_f == NULL) {
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_set_last_errorf("failed to open server private key '%s': %s", key_pem_path, strerror(errno));
        return NULL;
    }
    if (!omni_tls_parse_rsa_key_pem(key_f, &rsa_key)) {
        fclose(key_f);
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_rsa_private_key_free(&rsa_key);
        omni_tls_set_last_errorf("failed to parse server private key '%s' as PEM RSA", key_pem_path);
        return NULL;
    }
    fclose(key_f);

    creds = (omni_tls_server_creds *)malloc(sizeof(*creds));
    if (creds == NULL) {
        omni_tls_cert_vec_free(&cert_chain);
        omni_tls_rsa_private_key_free(&rsa_key);
        omni_tls_set_last_errorf("failed to allocate server credentials for '%s' and '%s'", cert_pem_path, key_pem_path);
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
    omni_tls_clear_last_error();
    if (client_ctx == NULL || creds == NULL || creds->chain == NULL || creds->chain_len == 0) {
        omni_tls_set_last_errorf("client certificate credentials are missing");
        return 0;
    }

    sign_impl = br_rsa_pkcs1_sign_get_default();
    if (sign_impl == 0) {
        omni_tls_set_last_errorf("RSA signing support unavailable in BearSSL");
        return 0;
    }

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
