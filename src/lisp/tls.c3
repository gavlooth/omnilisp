module lisp;

// ============================================================
// BearSSL extern declarations — direct calls, no wrapper
// ============================================================

// BearSSL struct sizes (from bearssl.h on x86_64):
//   br_ssl_client_context: 3720 bytes
//   br_x509_minimal_context: 3168 bytes
//   br_sslio_context: 40 bytes
//   br_ssl_engine_context is at offset 0 inside br_ssl_client_context
//   BR_SSL_BUFSIZE_BIDI: 33178 bytes

const usz BEARSSL_CLIENT_CTX_SIZE = 3720;
const usz BEARSSL_X509_CTX_SIZE  = 3168;
const usz BEARSSL_SSLIO_CTX_SIZE = 40;
const usz BEARSSL_IOBUF_SIZE     = 33178;
const int TLS_READ_EMPTY_RETRY_MAX = 64;
const uint TLS_READ_EMPTY_RETRY_US = 1000;

// Callback type for br_sslio_init: int (*)(void *ctx, unsigned char *buf, size_t len)
alias BrSslioCallback = fn int(void* ctx, char* buf, usz len);

extern fn void br_ssl_client_init_full(void* cc, void* xc, void* tas, usz num_tas) @extern("br_ssl_client_init_full");
extern fn void br_ssl_engine_set_buffer(void* eng, void* iobuf, usz iobuf_len, int bidi) @extern("br_ssl_engine_set_buffer");
extern fn int br_ssl_client_reset(void* cc, char* server_name, int resume_session) @extern("br_ssl_client_reset");
extern fn void br_ssl_server_init_full_rsa(void* cc, void* chain, usz chain_len, void* sk) @extern("br_ssl_server_init_full_rsa");
extern fn int br_ssl_server_reset(void* cc) @extern("br_ssl_server_reset");
extern fn void br_sslio_init(void* ctx, void* engine, BrSslioCallback low_read, void* read_ctx, BrSslioCallback low_write, void* write_ctx) @extern("br_sslio_init");
extern fn int br_sslio_read(void* ctx, char* buf, usz len) @extern("br_sslio_read");
extern fn int br_sslio_write_all(void* ctx, char* buf, usz len) @extern("br_sslio_write_all");
extern fn int br_sslio_flush(void* ctx) @extern("br_sslio_flush");
extern fn int br_sslio_close(void* ctx) @extern("br_sslio_close");

// Socket I/O callbacks + trust-store helpers (from csrc/tls_helpers.c)
extern fn int omni_tls_sock_read(void* ctx, char* buf, usz len) @extern("omni_tls_sock_read");
extern fn int omni_tls_sock_write(void* ctx, char* buf, usz len) @extern("omni_tls_sock_write");
extern fn void* omni_tls_trust_store_load_file(char* path) @extern("omni_tls_trust_store_load_file");
extern fn void* omni_tls_trust_store_load_default() @extern("omni_tls_trust_store_load_default");
extern fn void* omni_tls_trust_store_get_anchors(void* store_handle) @extern("omni_tls_trust_store_get_anchors");
extern fn usz omni_tls_trust_store_get_anchor_count(void* store_handle) @extern("omni_tls_trust_store_get_anchor_count");
extern fn void omni_tls_trust_store_free(void* store_handle) @extern("omni_tls_trust_store_free");
extern fn usz omni_tls_server_context_size() @extern("omni_tls_server_context_size");
extern fn void* omni_tls_server_creds_load(char* cert_pem_path, char* key_pem_path) @extern("omni_tls_server_creds_load");
extern fn void* omni_tls_server_creds_chain(void* creds_handle) @extern("omni_tls_server_creds_chain");
extern fn usz omni_tls_server_creds_chain_len(void* creds_handle) @extern("omni_tls_server_creds_chain_len");
extern fn void* omni_tls_server_creds_rsa_key(void* creds_handle) @extern("omni_tls_server_creds_rsa_key");
extern fn void omni_tls_server_creds_free(void* creds_handle) @extern("omni_tls_server_creds_free");
extern fn int omni_tls_client_set_single_rsa(void* client_ctx, void* creds_handle) @extern("omni_tls_client_set_single_rsa");
extern fn int omni_tls_client_session_cache_load(void* client_ctx, ZString hostname) @extern("omni_tls_client_session_cache_load");
extern fn int omni_tls_client_session_cache_store(void* client_ctx, ZString hostname) @extern("omni_tls_client_session_cache_store");

