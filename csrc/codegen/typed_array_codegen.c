/*
 * typed_array_codegen.c - Typed Array Code Generator Implementation
 *
 * Implementation of typed array code generation.
 *
 * Reference: docs/TYPE_SPECIALIZATION_DESIGN.md (Phase 4)
 */

#include "typed_array_codegen.h"
#include "spec_codegen.h"
#include "../../runtime/include/typed_array.h"
#include <stdlib.h>
#include <string.h>

/* strdup for C99 */
static char* omni_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* copy = malloc(len);
    if (copy) memcpy(copy, s, len);
    return copy;
}
#define strdup omni_strdup

/* ============== Type Name Utilities ============== */

static ArrayElementType concrete_type_to_array_type(ConcreteType* type) {
    if (!type || type->kind != TYPE_KIND_PRIMITIVE) {
        return ARRAY_TYPE_UNKNOWN;
    }

    switch (type->primitive.prim) {
        case PRIMITIVE_INT64:
            return ARRAY_TYPE_INT64;
        case PRIMITIVE_FLOAT64:
            return ARRAY_TYPE_FLOAT64;
        case PRIMITIVE_CHAR:
            return ARRAY_TYPE_CHAR;
        case PRIMITIVE_BOOL:
            return ARRAY_TYPE_BOOL;
        default:
            return ARRAY_TYPE_UNKNOWN;
    }
}

char* get_array_element_type_name(ConcreteType* type) {
    ArrayElementType array_type = concrete_type_to_array_type(type);

    switch (array_type) {
        case ARRAY_TYPE_INT64:
            return strdup("ARRAY_TYPE_INT64");
        case ARRAY_TYPE_FLOAT64:
            return strdup("ARRAY_TYPE_FLOAT64");
        case ARRAY_TYPE_CHAR:
            return strdup("ARRAY_TYPE_CHAR");
        case ARRAY_TYPE_BOOL:
            return strdup("ARRAY_TYPE_BOOL");
        default:
            return strdup("ARRAY_TYPE_UNKNOWN");
    }
}

char* get_typed_array_get_function(ConcreteType* type) {
    ArrayElementType array_type = concrete_type_to_array_type(type);

    switch (array_type) {
        case ARRAY_TYPE_INT64:
            return strdup("omni_typed_array_get_int");
        case ARRAY_TYPE_FLOAT64:
            return strdup("omni_typed_array_get_float");
        case ARRAY_TYPE_CHAR:
            return strdup("omni_typed_array_get_char");
        case ARRAY_TYPE_BOOL:
            return strdup("omni_typed_array_get_bool");
        default:
            return strdup("omni_typed_array_ref");
    }
}

char* get_typed_array_set_function(ConcreteType* type) {
    ArrayElementType array_type = concrete_type_to_array_type(type);

    switch (array_type) {
        case ARRAY_TYPE_INT64:
            return strdup("omni_typed_array_set_int");
        case ARRAY_TYPE_FLOAT64:
            return strdup("omni_typed_array_set_float");
        case ARRAY_TYPE_CHAR:
            return strdup("omni_typed_array_set_char");
        case ARRAY_TYPE_BOOL:
            return strdup("omni_typed_array_set_bool");
        default:
            return strdup("omni_typed_array_set");
    }
}

/* ============== Code Generation API ============== */

void generate_typed_array_alloc(CodeGenContext* ctx,
                               const char* var_name,
                               const char* type_name,
                               int rank,
                               int* dimensions) {
    if (!ctx || !var_name || !type_name) return;

// REVIEWED:OPTIMIZED - dynamic allocation instead of fixed 256-byte buffer
    /* Build dimensions array initialization */
    char* dims_init = NULL;
    if (dimensions && rank > 0) {
        /* Calculate required size: "(int[]){" + comma-separated ints + "}" */
        size_t dims_size = 16 + (size_t)rank * 22;  /* 22 chars per int with comma */
        dims_init = malloc(dims_size);
        if (dims_init) {
            char* pos = dims_init;
            pos += sprintf(pos, "(int[]){");
            for (int i = 0; i < rank; i++) {
                if (i > 0) pos += sprintf(pos, ", ");
                pos += sprintf(pos, "%d", dimensions[i]);
            }
            sprintf(pos, "}");
        }
    }

    /* Get ArrayElementType from type name */
    ArrayElementType elem_type = ARRAY_TYPE_UNKNOWN;
    if (strcmp(type_name, "Int") == 0 || strcmp(type_name, "Int64") == 0) {
        elem_type = ARRAY_TYPE_INT64;
    } else if (strcmp(type_name, "Float") == 0 || strcmp(type_name, "Float64") == 0) {
        elem_type = ARRAY_TYPE_FLOAT64;
    } else if (strcmp(type_name, "Char") == 0) {
        elem_type = ARRAY_TYPE_CHAR;
    } else if (strcmp(type_name, "Bool") == 0) {
        elem_type = ARRAY_TYPE_BOOL;
    }

    const char* elem_type_str = "";
    switch (elem_type) {
        case ARRAY_TYPE_INT64:   elem_type_str = "ARRAY_TYPE_INT64"; break;
        case ARRAY_TYPE_FLOAT64: elem_type_str = "ARRAY_TYPE_FLOAT64"; break;
        case ARRAY_TYPE_CHAR:    elem_type_str = "ARRAY_TYPE_CHAR"; break;
        case ARRAY_TYPE_BOOL:    elem_type_str = "ARRAY_TYPE_BOOL"; break;
        default:                 elem_type_str = "ARRAY_TYPE_UNKNOWN"; break;
    }

    omni_codegen_emit(ctx, "TypedArray* %s = omni_typed_array_create(_local_region, %s, %d, %s);",
                     var_name, elem_type_str, rank, dims_init ? dims_init : "NULL");
    free(dims_init);
}

void generate_typed_array_get(CodeGenContext* ctx,
                             const char* result_var,
                             const char* array_var,
                             const char** indices,
                             ConcreteType* elem_type) {
    if (!ctx || !result_var || !array_var || !indices || !elem_type) return;

    char* c_type = get_c_type_name(elem_type);
    char* get_fn = get_typed_array_get_function(elem_type);

// REVIEWED:OPTIMIZED - dynamic allocation instead of fixed 256-byte buffer
    /* Build indices array */
    char* indices_buf = NULL;
    if (elem_type->kind == TYPE_KIND_ARRAY && elem_type->array.rank > 0) {
        /* Calculate required size */
        size_t buf_size = 16;  /* "(int[]){" + "}" + null */
        for (int i = 0; i < elem_type->array.rank; i++) {
            buf_size += strlen(indices[i]) + 2;  /* +2 for ", " */
        }
        indices_buf = malloc(buf_size);
        if (indices_buf) {
            char* pos = indices_buf;
            pos += sprintf(pos, "(int[]){");
            for (int i = 0; i < elem_type->array.rank; i++) {
                if (i > 0) pos += sprintf(pos, ", ");
                pos += sprintf(pos, "%s", indices[i]);
            }
            sprintf(pos, "}");
        }
    }

    omni_codegen_emit(ctx, "%s %s = %s(%s, %s);",
                     c_type, result_var, get_fn, array_var, indices_buf ? indices_buf : "NULL");

    free(indices_buf);
    free(c_type);
    free(get_fn);
}

void generate_typed_array_set(CodeGenContext* ctx,
                             const char* array_var,
                             const char** indices,
                             const char* value_var,
                             ConcreteType* elem_type) {
    if (!ctx || !array_var || !indices || !value_var || !elem_type) return;

    char* set_fn = get_typed_array_set_function(elem_type);

// REVIEWED:OPTIMIZED - dynamic allocation instead of fixed 256-byte buffer
    /* Build indices array */
    char* indices_buf = NULL;
    if (elem_type->kind == TYPE_KIND_ARRAY && elem_type->array.rank > 0) {
        /* Calculate required size */
        size_t buf_size = 16;  /* "(int[]){" + "}" + null */
        for (int i = 0; i < elem_type->array.rank; i++) {
            buf_size += strlen(indices[i]) + 2;  /* +2 for ", " */
        }
        indices_buf = malloc(buf_size);
        if (indices_buf) {
            char* pos = indices_buf;
            pos += sprintf(pos, "(int[]){");
            for (int i = 0; i < elem_type->array.rank; i++) {
                if (i > 0) pos += sprintf(pos, ", ");
                pos += sprintf(pos, "%s", indices[i]);
            }
            sprintf(pos, "}");
        }
    }

    omni_codegen_emit(ctx, "%s(%s, %s, %s);",
                     set_fn, array_var, indices_buf ? indices_buf : "NULL", value_var);
    free(indices_buf);

    free(set_fn);
}

void generate_typed_array_fill(CodeGenContext* ctx,
                              const char* array_var,
                              const char* value_var) {
    if (!ctx || !array_var || !value_var) return;

    omni_codegen_emit(ctx, "omni_typed_array_fill(%s, %s);", array_var, value_var);
}

void generate_typed_array_to_list(CodeGenContext* ctx,
                                 const char* result_var,
                                 const char* array_var) {
    if (!ctx || !result_var || !array_var) return;

    omni_codegen_emit(ctx, "Obj* %s = omni_typed_array_to_list(%s, _local_region);",
                     result_var, array_var);
}

void generate_list_to_typed_array(CodeGenContext* ctx,
                                 const char* result_var,
                                 const char* list_var,
                                 ConcreteType* elem_type) {
    if (!ctx || !result_var || !list_var || !elem_type) return;

    char* elem_type_str = get_array_element_type_name(elem_type);

    omni_codegen_emit(ctx, "TypedArray* %s = omni_list_to_typed_array(_local_region, %s, %s);",
                     result_var, elem_type_str, list_var);

    free(elem_type_str);
}

void generate_is_typed_array_check(CodeGenContext* ctx,
                                   const char* value_var,
                                   ConcreteType* elem_type,
                                   const char* label_true,
                                   const char* label_false) {
    if (!ctx || !value_var) return;

    /* TODO: Implement type checking */
    /* For now, just check if it's a user type with array tag */
    omni_codegen_emit(ctx, "if (%s && %s->tag == OMNI_USER_TYPE) {", value_var, value_var);
    if (label_true) {
        omni_codegen_emit_raw(ctx, "goto %s;\n", label_true);
    }
    omni_codegen_emit(ctx, "} else {");
    if (label_false) {
        omni_codegen_emit_raw(ctx, "goto %s;\n", label_false);
    }
    omni_codegen_emit(ctx, "}");
}

void generate_typed_array_length(CodeGenContext* ctx,
                                const char* result_var,
                                const char* array_var) {
    if (!ctx || !result_var || !array_var) return;

    omni_codegen_emit(ctx, "int64_t %s = %s ? %s->total_size : 0;",
                     result_var, array_var, array_var);
}
