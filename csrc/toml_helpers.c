#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "../deps/src/libuv/include/uv.h"

#include "../third_party/tomlc17/tomlc17.h"

typedef struct {
    toml_result_t result;
    const char*   source;
} omni_toml_parse_result_t;

static uv_mutex_t g_omni_toml_option_mu;
static uv_once_t g_omni_toml_option_mu_once = UV_ONCE_INIT;

static void omni_toml_option_mu_init_once(void) {
    uv_mutex_init(&g_omni_toml_option_mu);
}

static void omni_toml_option_lock(void) {
    uv_once(&g_omni_toml_option_mu_once, omni_toml_option_mu_init_once);
    uv_mutex_lock(&g_omni_toml_option_mu);
}

static void omni_toml_option_unlock(void) {
    uv_mutex_unlock(&g_omni_toml_option_mu);
}

static void omni_copy_err(char* err_buf, size_t err_cap, const char* msg) {
    if (!err_buf || err_cap == 0) return;
    if (!msg) {
        err_buf[0] = '\0';
        return;
    }

    size_t n = strlen(msg);
    if (n >= err_cap) n = err_cap - 1;
    if (n > 0) memcpy(err_buf, msg, n);
    err_buf[n] = '\0';
}

void* omni_toml_parse(const char* src, size_t len, char* err_buf, size_t err_cap) {
    omni_copy_err(err_buf, err_cap, "");

    if (!src) {
        omni_copy_err(err_buf, err_cap, "toml-parse: null input");
        return NULL;
    }
    if (len > (size_t)INT32_MAX) {
        omni_copy_err(err_buf, err_cap, "toml-parse: input too large");
        return NULL;
    }

    char* zsrc = (char*)malloc(len + 1);
    if (!zsrc) {
        omni_copy_err(err_buf, err_cap, "toml-parse: out of memory");
        return NULL;
    }

    if (len > 0) {
        memcpy(zsrc, src, len);
    }
    zsrc[len] = '\0';

    omni_toml_parse_result_t* result = (omni_toml_parse_result_t*)malloc(sizeof(*result));
    if (!result) {
        free(zsrc);
        omni_copy_err(err_buf, err_cap, "toml-parse: out of memory");
        return NULL;
    }

    result->source = zsrc;
    omni_toml_option_lock();
    result->result = toml_parse(zsrc, (int)len);
    omni_toml_option_unlock();

    if (!result->result.ok) {
        omni_copy_err(err_buf, err_cap, result->result.errmsg[0] ? result->result.errmsg : "toml-parse: parse failed");
        toml_free(result->result);
        free((void*)result->source);
        free(result);
        return NULL;
    }

    return (void*)result;
}

void* omni_toml_parse_with_options(const char* src, size_t len, char* err_buf, size_t err_cap, bool check_utf8) {
    omni_copy_err(err_buf, err_cap, "");

    if (!src) {
        omni_copy_err(err_buf, err_cap, "toml-parse: null input");
        return NULL;
    }
    if (len > (size_t)INT32_MAX) {
        omni_copy_err(err_buf, err_cap, "toml-parse: input too large");
        return NULL;
    }

    char* zsrc = (char*)malloc(len + 1);
    if (!zsrc) {
        omni_copy_err(err_buf, err_cap, "toml-parse: out of memory");
        return NULL;
    }

    if (len > 0) {
        memcpy(zsrc, src, len);
    }
    zsrc[len] = '\0';

    omni_toml_parse_result_t* result = (omni_toml_parse_result_t*)malloc(sizeof(*result));
    if (!result) {
        free(zsrc);
        omni_copy_err(err_buf, err_cap, "toml-parse: out of memory");
        return NULL;
    }

    result->source = zsrc;
    omni_toml_option_lock();
    toml_option_t previous = toml_default_option();
    toml_option_t next = previous;
    next.check_utf8 = check_utf8;
    if (check_utf8 != previous.check_utf8) {
        toml_set_option(next);
    }
    result->result = toml_parse(zsrc, (int)len);
    if (check_utf8 != previous.check_utf8) {
        toml_set_option(previous);
    }
    omni_toml_option_unlock();

    if (!result->result.ok) {
        omni_copy_err(err_buf, err_cap, result->result.errmsg[0] ? result->result.errmsg : "toml-parse: parse failed");
        toml_free(result->result);
        free((void*)result->source);
        free(result);

        return NULL;
    }

    return (void*)result;
}

void omni_toml_result_free(void* result_ptr) {
    if (!result_ptr) return;
    omni_toml_parse_result_t* result = (omni_toml_parse_result_t*)result_ptr;
    toml_free(result->result);
    if (result->source) free((void*)result->source);
    free(result);
}

void* omni_toml_result_root(void* result_ptr) {
    if (!result_ptr) return NULL;
    return &((omni_toml_parse_result_t*)result_ptr)->result.toptab;
}

int omni_toml_datum_type(void* datum) {
    if (!datum) return TOML_UNKNOWN;
    return (int)((toml_datum_t*)datum)->type;
}

const char* omni_toml_datum_str_ptr(void* datum) {
    if (!datum) return NULL;
    return ((toml_datum_t*)datum)->u.str.ptr;
}

int omni_toml_datum_str_len(void* datum) {
    if (!datum) return 0;
    return ((toml_datum_t*)datum)->u.str.len;
}

long omni_toml_datum_int64(void* datum) {
    if (!datum) return 0;
    return ((toml_datum_t*)datum)->u.int64;
}

double omni_toml_datum_fp64(void* datum) {
    if (!datum) return 0.0;
    return ((toml_datum_t*)datum)->u.fp64;
}

int omni_toml_datum_bool(void* datum) {
    if (!datum) return 0;
    return ((toml_datum_t*)datum)->u.boolean ? 1 : 0;
}

int omni_toml_datum_table_size(void* datum) {
    if (!datum) return 0;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_TABLE) return 0;
    if (d->u.tab.size < 0) return 0;
    return d->u.tab.size;
}

const char* omni_toml_datum_table_key(void* datum, int index) {
    if (!datum) return NULL;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_TABLE) return NULL;
    if (index < 0 || index >= d->u.tab.size) return NULL;
    return d->u.tab.key[index];
}

int omni_toml_datum_table_key_len(void* datum, int index) {
    if (!datum) return 0;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_TABLE) return 0;
    if (index < 0 || index >= d->u.tab.size) return 0;
    return d->u.tab.len[index];
}

void* omni_toml_datum_table_value(void* datum, int index) {
    if (!datum) return NULL;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_TABLE) return NULL;
    if (index < 0 || index >= d->u.tab.size) return NULL;
    return (void*)&d->u.tab.value[index];
}

int omni_toml_datum_array_size(void* datum) {
    if (!datum) return 0;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_ARRAY) return 0;
    if (d->u.arr.size < 0) return 0;
    return d->u.arr.size;
}

void* omni_toml_datum_array_elem(void* datum, int index) {
    if (!datum) return NULL;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_ARRAY) return NULL;
    if (index < 0 || index >= d->u.arr.size) return NULL;
    return (void*)&d->u.arr.elem[index];
}

int omni_toml_datum_timestamp_parts(
    void* datum,
    int* year,
    int* month,
    int* day,
    int* hour,
    int* minute,
    int* second,
    int* usec,
    int* tz
) {
    if (!datum) return 0;
    toml_datum_t* d = (toml_datum_t*)datum;
    if (d->type != TOML_DATE && d->type != TOML_TIME && d->type != TOML_DATETIME && d->type != TOML_DATETIMETZ) {
        return 0;
    }

    if (year) *year = d->u.ts.year;
    if (month) *month = d->u.ts.month;
    if (day) *day = d->u.ts.day;
    if (hour) *hour = d->u.ts.hour;
    if (minute) *minute = d->u.ts.minute;
    if (second) *second = d->u.ts.second;
    if (usec) *usec = d->u.ts.usec;
    if (tz) *tz = d->u.ts.tz;
    return 1;
}
