/*
 * typed_array.c - Typed Array Runtime Implementation
 *
 * Implementation of typed arrays for unboxed primitive storage.
 *
 * Reference: docs/TYPE_SPECIALIZATION_DESIGN.md (Phase 4)
 */

#include "../include/omni.h"
#include "../include/typed_array.h"
#include "../include/primitives_specialized.h"
#include "../src/memory/region_core.h"
#include "../src/memory/region_value.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>

/* External function declarations */
extern Obj* call_closure(Obj* clos, Obj** args, int argc);
extern int is_truthy(Obj* x);
extern Obj* mk_pair(Obj* a, Obj* b);
extern Obj* obj_car(Obj* pair);
extern Obj* obj_cdr(Obj* pair);

/* NIL constant - using NULL for empty list */
#define NIL NULL

/* ============== Utility Functions ============== */

size_t omni_typed_array_element_size(ArrayElementType type) {
    switch (type) {
        case ARRAY_TYPE_INT64:  return sizeof(int64_t);
        case ARRAY_TYPE_FLOAT64: return sizeof(double);
        case ARRAY_TYPE_CHAR:    return sizeof(char);
        case ARRAY_TYPE_BOOL:    return sizeof(bool);
        default:                 return sizeof(void*);
    }
}

const char* omni_typed_array_type_name(ArrayElementType type) {
    switch (type) {
        case ARRAY_TYPE_INT64:  return "Int64";
        case ARRAY_TYPE_FLOAT64: return "Float64";
        case ARRAY_TYPE_CHAR:    return "Char";
        case ARRAY_TYPE_BOOL:    return "Bool";
        default:                 return "Unknown";
    }
}

ArrayElementType omni_typed_array_type_from_string(const char* name) {
    if (!name) return ARRAY_TYPE_UNKNOWN;

    if (strcmp(name, "Int") == 0 ||
        strcmp(name, "Int64") == 0 ||
        strcmp(name, "int64_t") == 0) {
        return ARRAY_TYPE_INT64;
    }

    if (strcmp(name, "Float") == 0 ||
        strcmp(name, "Float64") == 0 ||
        strcmp(name, "double") == 0) {
        return ARRAY_TYPE_FLOAT64;
    }

    if (strcmp(name, "Char") == 0 ||
        strcmp(name, "char") == 0) {
        return ARRAY_TYPE_CHAR;
    }

    if (strcmp(name, "Bool") == 0 ||
        strcmp(name, "bool") == 0) {
        return ARRAY_TYPE_BOOL;
    }

    return ARRAY_TYPE_UNKNOWN;
}

int64_t omni_typed_array_linear_index(TypedArray* arr, int* indices) {
    if (!arr || !indices) return -1;

    int64_t index = 0;
    int64_t multiplier = 1;

    for (int i = arr->rank - 1; i >= 0; i--) {
        if (indices[i] < 0 || indices[i] >= arr->dimensions[i]) {
            return -1;  /* Out of bounds */
        }
        index += indices[i] * multiplier;
        multiplier *= arr->dimensions[i];
    }

    return index;
}

/* ============== TypedArray API ============== */

TypedArray* omni_typed_array_create(Region* r,
                                    ArrayElementType element_type,
                                    int rank,
                                    int* dimensions) {
    if (!dimensions || rank <= 0 || !r) return NULL;

    /* Allocate TypedArray structure from region */
    TypedArray* arr = region_alloc(r, sizeof(TypedArray));
    if (!arr) return NULL;

    arr->element_type = element_type;
    arr->rank = rank;
    arr->element_size = omni_typed_array_element_size(element_type);
    arr->region = r;
    arr->ref_count = 1;

    /* Copy dimensions (allocated from region) */
    arr->dimensions = region_alloc(r, sizeof(int) * rank);
    if (!arr->dimensions) {
        /* region_alloc doesn't need explicit free, but arr is already allocated
         * from region - it will be reclaimed when region exits */
        return NULL;
    }
    memcpy(arr->dimensions, dimensions, sizeof(int) * rank);

    /* Calculate total size */
    arr->total_size = 1;
    for (int i = 0; i < rank; i++) {
        arr->total_size *= dimensions[i];
    }

    /* Allocate data buffer from region and zero-initialize */
    size_t data_size = arr->total_size * arr->element_size;
    arr->data = region_alloc(r, data_size);
    if (!arr->data) {
        /* region_alloc doesn't need explicit free, but arr and dimensions are
         * already allocated from region - they will be reclaimed when region exits */
        return NULL;
    }
    memset(arr->data, 0, data_size);  /* Zero-initialize like calloc */

    return arr;
}

void omni_typed_array_free(TypedArray* arr) {
    if (!arr) return;

    free(arr->dimensions);
    free(arr->data);
    free(arr);
}

void omni_typed_array_inc_ref(TypedArray* arr) {
    if (arr) {
        arr->ref_count++;
    }
}

void omni_typed_array_dec_ref(TypedArray* arr) {
    if (!arr) return;

    arr->ref_count--;
    if (arr->ref_count <= 0) {
        omni_typed_array_free(arr);
    }
}

/* ============== Element Access (Generic) ============== */

Obj* omni_typed_array_ref(TypedArray* arr, int* indices) {
    if (!arr) return NULL;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return NULL;

    switch (arr->element_type) {
        case ARRAY_TYPE_INT64: {
            int64_t* data = (int64_t*)arr->data;
            return box_int(data[index]);
        }

        case ARRAY_TYPE_FLOAT64: {
            double* data = (double*)arr->data;
            return box_float(data[index]);
        }

        case ARRAY_TYPE_CHAR: {
            char* data = (char*)arr->data;
            return box_char(data[index]);
        }

        case ARRAY_TYPE_BOOL: {
            bool* data = (bool*)arr->data;
            return box_bool(data[index]);
        }

        default:
            return NULL;
    }
}

bool omni_typed_array_set(TypedArray* arr, int* indices, Obj* value) {
    if (!arr || !value) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    switch (arr->element_type) {
        case ARRAY_TYPE_INT64: {
            int64_t* data = (int64_t*)arr->data;
            data[index] = unbox_int(value);
            return true;
        }

        case ARRAY_TYPE_FLOAT64: {
            double* data = (double*)arr->data;
            data[index] = unbox_float(value);
            return true;
        }

        case ARRAY_TYPE_CHAR: {
            char* data = (char*)arr->data;
            data[index] = unbox_char(value);
            return true;
        }

        case ARRAY_TYPE_BOOL: {
            bool* data = (bool*)arr->data;
            data[index] = unbox_bool(value);
            return true;
        }

        default:
            return false;
    }
}

/* ============== Element Access (Typed) ============== */

int64_t omni_typed_array_get_int(TypedArray* arr, int* indices) {
    if (!arr || arr->element_type != ARRAY_TYPE_INT64) return 0;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return 0;

    int64_t* data = (int64_t*)arr->data;
    return data[index];
}

double omni_typed_array_get_float(TypedArray* arr, int* indices) {
    if (!arr || arr->element_type != ARRAY_TYPE_FLOAT64) return 0.0;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return 0.0;

    double* data = (double*)arr->data;
    return data[index];
}

char omni_typed_array_get_char(TypedArray* arr, int* indices) {
    if (!arr || arr->element_type != ARRAY_TYPE_CHAR) return '\0';

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return '\0';

    char* data = (char*)arr->data;
    return data[index];
}

bool omni_typed_array_get_bool(TypedArray* arr, int* indices) {
    if (!arr || arr->element_type != ARRAY_TYPE_BOOL) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    bool* data = (bool*)arr->data;
    return data[index];
}

bool omni_typed_array_set_int(TypedArray* arr, int* indices, int64_t value) {
    if (!arr || arr->element_type != ARRAY_TYPE_INT64) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    int64_t* data = (int64_t*)arr->data;
    data[index] = value;
    return true;
}

bool omni_typed_array_set_float(TypedArray* arr, int* indices, double value) {
    if (!arr || arr->element_type != ARRAY_TYPE_FLOAT64) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    double* data = (double*)arr->data;
    data[index] = value;
    return true;
}

bool omni_typed_array_set_char(TypedArray* arr, int* indices, char value) {
    if (!arr || arr->element_type != ARRAY_TYPE_CHAR) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    char* data = (char*)arr->data;
    data[index] = value;
    return true;
}

bool omni_typed_array_set_bool(TypedArray* arr, int* indices, bool value) {
    if (!arr || arr->element_type != ARRAY_TYPE_BOOL) return false;

    int64_t index = omni_typed_array_linear_index(arr, indices);
    if (index < 0) return false;

    bool* data = (bool*)arr->data;
    data[index] = value;
    return true;
}

/* ============== Conversion Functions ============== */

TypedArray* omni_list_to_typed_array(Region* r,
                                     ArrayElementType element_type,
                                     Obj* list) {
    if (!list) return NULL;

    /* Count list length */
    int length = 0;
    Obj* current = list;
    while (current && current != NIL) {
        length++;
        if (current->is_pair) {
            current = obj_cdr(current);
        } else {
            break;
        }
    }

    /* Create array */
    int dimensions[] = {length};
    TypedArray* arr = omni_typed_array_create(r, element_type, 1, dimensions);
    if (!arr) return NULL;

    /* Fill array */
    current = list;
    for (int i = 0; i < length && current; i++) {
        int indices[] = {i};
        omni_typed_array_set(arr, indices, obj_car(current));
        current = obj_cdr(current);
    }

    return arr;
}

Obj* omni_typed_array_to_list(TypedArray* arr, Region* r) {
    if (!arr) return mk_nil_region(r);

    Obj* result = mk_nil_region(r);

    for (int64_t i = arr->total_size - 1; i >= 0; i--) {
        int indices[] = {(int)i};
        Obj* elem = omni_typed_array_ref(arr, indices);
        /* Prepend element: new cell points to current result as cdr */
        result = mk_cell_region(r, elem, result);
    }

    return result;
}

/* ============== Array Operations ============== */

TypedArray* omni_typed_array_map(Obj* func,
                                 TypedArray* arr,
                                 ArrayElementType result_type,
                                 Region* r) {
    if (!func || !arr) return NULL;

    /* Create result array */
    TypedArray* result = omni_typed_array_create(r, result_type, arr->rank, arr->dimensions);
    if (!result) return NULL;

    /* Apply function to each element */
    for (int64_t i = 0; i < arr->total_size; i++) {
        int indices[] = {(int)i};
        
        /* Box element and call function */
        Obj* boxed_elem = omni_typed_array_ref(arr, indices);
        if (!boxed_elem) {
            return NULL;  /* Out of bounds or error */
        }
        
        Obj* boxed_result = call_closure(func, &boxed_elem, 1);
        if (!boxed_result) {
            return NULL;  /* Function returned NULL or error */
        }
        
        /* Unbox result and store in array */
        int result_indices[] = {(int)i};
        if (!omni_typed_array_set(result, result_indices, boxed_result)) {
            return NULL;  /* Type mismatch or error */
        }
    }

    return result;
}

TypedArray* omni_typed_array_filter(Obj* pred,
                                    TypedArray* arr,
                                    Region* r) {
    if (!pred || !arr) return NULL;

    /* First pass: count matching elements */
    int64_t match_count = 0;
    for (int64_t i = 0; i < arr->total_size; i++) {
        int indices[] = {(int)i};
        
        /* Box element and call predicate */
        Obj* boxed_elem = omni_typed_array_ref(arr, indices);
        if (!boxed_elem) {
            return NULL;  /* Out of bounds or error */
        }
        
        Obj* boxed_result = call_closure(pred, &boxed_elem, 1);
        if (!boxed_result) {
            return NULL;  /* Predicate returned NULL or error */
        }
        
        if (is_truthy(boxed_result)) {
            match_count++;
        }
    }
    
    /* Create result array (1D, same element type) */
    int result_dim = (int)match_count;
    TypedArray* result = omni_typed_array_create(r, arr->element_type, 1, &result_dim);
    if (!result) return NULL;
    
    /* Second pass: copy matching elements */
    int64_t write_idx = 0;
    for (int64_t i = 0; i < arr->total_size; i++) {
        int indices[] = {(int)i};
        
        /* Box element and call predicate */
        Obj* boxed_elem = omni_typed_array_ref(arr, indices);
        if (!boxed_elem) {
            return NULL;
        }
        
        Obj* boxed_result = call_closure(pred, &boxed_elem, 1);
        if (!boxed_result) {
            return NULL;
        }
        
        if (is_truthy(boxed_result)) {
            /* Copy element directly from source array */
            switch (arr->element_type) {
                case ARRAY_TYPE_INT64: {
                    int64_t* src_data = (int64_t*)arr->data;
                    int64_t* dst_data = (int64_t*)result->data;
                    dst_data[write_idx] = src_data[i];
                    break;
                }
                case ARRAY_TYPE_FLOAT64: {
                    double* src_data = (double*)arr->data;
                    double* dst_data = (double*)result->data;
                    dst_data[write_idx] = src_data[i];
                    break;
                }
                case ARRAY_TYPE_CHAR: {
                    char* src_data = (char*)arr->data;
                    char* dst_data = (char*)result->data;
                    dst_data[write_idx] = src_data[i];
                    break;
                }
                case ARRAY_TYPE_BOOL: {
                    bool* src_data = (bool*)arr->data;
                    bool* dst_data = (bool*)result->data;
                    dst_data[write_idx] = src_data[i];
                    break;
                }
                default:
                    return NULL;
            }
            write_idx++;
        }
    }
    
    return result;
}

Obj* omni_typed_array_reduce(Obj* func,
                            Obj* init,
                            TypedArray* arr) {
    if (!func || !arr) return init;

    Obj* acc = init;
    
    /* Reduce array */
    for (int64_t i = 0; i < arr->total_size; i++) {
        int indices[] = {(int)i};
        
        /* Box element */
        Obj* boxed_elem = omni_typed_array_ref(arr, indices);
        if (!boxed_elem) {
            return NULL;  /* Out of bounds or error */
        }
        
        /* Call reduction function: (acc, elem) -> new_acc */
        Obj* args[2] = { acc, boxed_elem };
        Obj* new_acc = call_closure(func, args, 2);
        if (!new_acc) {
            return NULL;  /* Function returned NULL or error */
        }
        
        acc = new_acc;
    }
    
    return acc;
}

bool omni_typed_array_fill(TypedArray* arr, Obj* value) {
    if (!arr || !value) return false;

    for (int64_t i = 0; i < arr->total_size; i++) {
        int indices[] = {(int)i};
        if (!omni_typed_array_set(arr, indices, value)) {
            return false;
        }
    }

    return true;
}
