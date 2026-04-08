#include "../deps/src/yyjson/src/yyjson.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* Wrapper functions for yyjson inline accessors (C3 can't call inline fns) */

yyjson_doc* omni_yyjson_read(const char* dat, size_t len, size_t flags) {
    return yyjson_read(dat, len, flags);
}

void omni_yyjson_doc_free(yyjson_doc* doc) {
    yyjson_doc_free(doc);
}

yyjson_val* omni_yyjson_doc_get_root(yyjson_doc* doc) {
    return yyjson_doc_get_root(doc);
}

/* Type checking */
int omni_yyjson_get_type(yyjson_val* val) {
    if (!val) return 0;
    return (int)yyjson_get_type(val);
}

int omni_yyjson_is_null(yyjson_val* val) { return yyjson_is_null(val); }
int omni_yyjson_is_true(yyjson_val* val) { return yyjson_is_true(val); }
int omni_yyjson_is_false(yyjson_val* val) { return yyjson_is_false(val); }
int omni_yyjson_is_int(yyjson_val* val) { return yyjson_is_int(val); }
int omni_yyjson_is_real(yyjson_val* val) { return yyjson_is_real(val); }
int omni_yyjson_is_str(yyjson_val* val) { return yyjson_is_str(val); }
int omni_yyjson_is_arr(yyjson_val* val) { return yyjson_is_arr(val); }
int omni_yyjson_is_obj(yyjson_val* val) { return yyjson_is_obj(val); }

/* Value getters */
long long omni_yyjson_get_sint(yyjson_val* val) { return yyjson_get_sint(val); }
double omni_yyjson_get_real(yyjson_val* val) { return yyjson_get_real(val); }
const char* omni_yyjson_get_str(yyjson_val* val) { return yyjson_get_str(val); }
size_t omni_yyjson_get_len(yyjson_val* val) { return yyjson_get_len(val); }
int omni_yyjson_get_bool(yyjson_val* val) { return yyjson_get_bool(val); }

/* Array iteration */
size_t omni_yyjson_arr_size(yyjson_val* arr) { return yyjson_arr_size(arr); }
yyjson_arr_iter* omni_yyjson_arr_iter_new(yyjson_val* arr) {
    yyjson_arr_iter* iter = (yyjson_arr_iter*)malloc(sizeof(yyjson_arr_iter));
    if (iter == NULL) return NULL;
    if (!yyjson_arr_iter_init(arr, iter)) {
        free(iter);
        return NULL;
    }
    return iter;
}
void omni_yyjson_arr_iter_free(yyjson_arr_iter* iter) {
    if (iter != NULL) free(iter);
}
yyjson_val* omni_yyjson_arr_iter_next(yyjson_arr_iter* iter) {
    if (iter == NULL) return NULL;
    return yyjson_arr_iter_next(iter);
}

/* Object iteration */
size_t omni_yyjson_obj_size(yyjson_val* obj) { return yyjson_obj_size(obj); }
yyjson_val* omni_yyjson_obj_getn(yyjson_val* obj, const char* key, size_t len) {
    return yyjson_obj_getn(obj, key, len);
}
yyjson_obj_iter* omni_yyjson_obj_iter_new(yyjson_val* obj) {
    yyjson_obj_iter* iter = (yyjson_obj_iter*)malloc(sizeof(yyjson_obj_iter));
    if (iter == NULL) return NULL;
    if (!yyjson_obj_iter_init(obj, iter)) {
        free(iter);
        return NULL;
    }
    return iter;
}
void omni_yyjson_obj_iter_free(yyjson_obj_iter* iter) {
    if (iter != NULL) free(iter);
}
yyjson_val* omni_yyjson_obj_iter_next(yyjson_obj_iter* iter) {
    if (iter == NULL) return NULL;
    return yyjson_obj_iter_next(iter);
}
yyjson_val* omni_yyjson_obj_iter_get_val(yyjson_val* key) {
    if (key == NULL) return NULL;
    return yyjson_obj_iter_get_val(key);
}

/* =================== Mutable API for json-emit =================== */

yyjson_mut_doc* omni_yyjson_mut_doc_new(void) {
    return yyjson_mut_doc_new(NULL);
}

void omni_yyjson_mut_doc_free(yyjson_mut_doc* doc) {
    yyjson_mut_doc_free(doc);
}

yyjson_mut_val* omni_yyjson_mut_null(yyjson_mut_doc* doc) { return yyjson_mut_null(doc); }
yyjson_mut_val* omni_yyjson_mut_bool(yyjson_mut_doc* doc, int val) { return yyjson_mut_bool(doc, val); }
yyjson_mut_val* omni_yyjson_mut_sint(yyjson_mut_doc* doc, long long val) { return yyjson_mut_sint(doc, val); }
yyjson_mut_val* omni_yyjson_mut_real(yyjson_mut_doc* doc, double val) { return yyjson_mut_real(doc, val); }
yyjson_mut_val* omni_yyjson_mut_strn(yyjson_mut_doc* doc, const char* str, size_t len) { return yyjson_mut_strncpy(doc, str, len); }

yyjson_mut_val* omni_yyjson_mut_arr(yyjson_mut_doc* doc) { return yyjson_mut_arr(doc); }
bool omni_yyjson_mut_arr_append(yyjson_mut_val* arr, yyjson_mut_val* val) { return yyjson_mut_arr_append(arr, val); }

yyjson_mut_val* omni_yyjson_mut_obj(yyjson_mut_doc* doc) { return yyjson_mut_obj(doc); }
bool omni_yyjson_mut_obj_add(yyjson_mut_val* obj, yyjson_mut_val* key, yyjson_mut_val* val) { return yyjson_mut_obj_add(obj, key, val); }

void omni_yyjson_mut_doc_set_root(yyjson_mut_doc* doc, yyjson_mut_val* root) { yyjson_mut_doc_set_root(doc, root); }

/* Write to string. Returns malloc'd string, caller must free. */
char* omni_yyjson_mut_write(yyjson_mut_doc* doc, size_t* len) {
    return yyjson_mut_write(doc, 0, len);
}

char* omni_yyjson_mut_write_pretty(yyjson_mut_doc* doc, size_t* len) {
    return yyjson_mut_write(doc, YYJSON_WRITE_PRETTY, len);
}

char* omni_yyjson_mut_write_with_flags(yyjson_mut_doc* doc, yyjson_write_flag flags, size_t* len) {
    return yyjson_mut_write(doc, flags, len);
}

yyjson_write_flag omni_yyjson_fp_to_fixed_flag(int precision) {
    if (precision < 0 || precision > 15) return YYJSON_WRITE_NOFLAG;
    return YYJSON_WRITE_FP_TO_FIXED((uint32_t)precision);
}
