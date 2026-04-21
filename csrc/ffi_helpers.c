// ffi_helpers.c — thin C wrapper around libffi for Omni's FFI system.
// Reason: libffi uses C structs (ffi_type, ffi_cif) that are hard to declare in C3.

#include <ffi.h>
#include <stdlib.h>

// Type codes matching Omni's FFI type enum (must stay in sync with value.c3 FfiTypeTag)
enum {
    OMNI_FFI_VOID    = 0,
    OMNI_FFI_INT     = 1,
    OMNI_FFI_DOUBLE  = 2,
    OMNI_FFI_PTR     = 3,  // foreign handle pointer ABI
    OMNI_FFI_BOOL    = 4,
    OMNI_FFI_STRING  = 5,
    OMNI_FFI_FLOAT32 = 6,
    OMNI_FFI_INT32   = 7,
    OMNI_FFI_INT64   = 8,
    OMNI_FFI_UINT32  = 9,
    OMNI_FFI_UINT64  = 10,
    OMNI_FFI_BUFFER  = 11,
    OMNI_FFI_STRUCT  = 12,
};

static ffi_type* omni_to_ffi_type(int t) {
    switch (t) {
        case OMNI_FFI_VOID:    return &ffi_type_void;
        case OMNI_FFI_INT:     return &ffi_type_sint64;
        case OMNI_FFI_DOUBLE:  return &ffi_type_double;
        case OMNI_FFI_PTR:     return &ffi_type_pointer;
        case OMNI_FFI_BOOL:    return &ffi_type_sint64;
        case OMNI_FFI_STRING:  return &ffi_type_pointer;
        case OMNI_FFI_FLOAT32: return &ffi_type_float;
        case OMNI_FFI_INT32:   return &ffi_type_sint32;
        case OMNI_FFI_INT64:   return &ffi_type_sint64;
        case OMNI_FFI_UINT32:  return &ffi_type_uint32;
        case OMNI_FFI_UINT64:  return &ffi_type_uint64;
        case OMNI_FFI_BUFFER:  return &ffi_type_pointer;
        case OMNI_FFI_STRUCT:  return &ffi_type_pointer;
        default:               return &ffi_type_pointer;
    }
}

// omni_ffi_call — prepare CIF and call function via libffi.
// fn_ptr:     dlsym'd function pointer
// nargs:      number of arguments
// arg_types:  array of OMNI_FFI_* type codes for each arg
// arg_values: array of pointers to argument storage (each points to int64_t, double, or void*)
// ret_type:   OMNI_FFI_* type code for return
// ret_value:  pointer to return value storage (must be large enough for the return type)
// Returns: 0 on success, -1 on error
int omni_ffi_call(void* fn_ptr, int nargs, int* arg_types, void** arg_values,
                  int ret_type, void* ret_value) {
    if (nargs < 0) return -1;

    ffi_cif cif;
    ffi_type** atypes = NULL;
    if (nargs > 0) {
        atypes = (ffi_type**)malloc((size_t)nargs * sizeof(ffi_type*));
        if (atypes == NULL) return -1;
    }
    for (int i = 0; i < nargs; i++) {
        atypes[i] = omni_to_ffi_type(arg_types[i]);
    }
    ffi_type* rtype = omni_to_ffi_type(ret_type);

    ffi_status status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned int)nargs, rtype, atypes);
    if (status != FFI_OK) {
        free(atypes);
        return -1;
    }

    ffi_call(&cif, (void (*)(void))fn_ptr, ret_value, arg_values);
    free(atypes);
    return 0;
}

// omni_ffi_call_var — variadic function call via libffi.
// fixed_count: number of fixed (non-variadic) arguments
int omni_ffi_call_var(void* fn_ptr, int nargs, int fixed_count,
                      int* arg_types, void** arg_values,
                      int ret_type, void* ret_value) {
    if (nargs < 0 || fixed_count < 0 || fixed_count > nargs) return -1;

    ffi_cif cif;
    ffi_type** atypes = NULL;
    if (nargs > 0) {
        atypes = (ffi_type**)malloc((size_t)nargs * sizeof(ffi_type*));
        if (atypes == NULL) return -1;
    }
    for (int i = 0; i < nargs; i++) {
        atypes[i] = omni_to_ffi_type(arg_types[i]);
    }
    ffi_type* rtype = omni_to_ffi_type(ret_type);

    ffi_status status = ffi_prep_cif_var(&cif, FFI_DEFAULT_ABI,
                                          (unsigned int)fixed_count,
                                          (unsigned int)nargs,
                                          rtype, atypes);
    if (status != FFI_OK) {
        free(atypes);
        return -1;
    }

    ffi_call(&cif, (void (*)(void))fn_ptr, ret_value, arg_values);
    free(atypes);
    return 0;
}

// =============================================================================
// FFI CALLBACK CLOSURES (libffi closure_alloc)
// =============================================================================

// Forward declaration — implemented in C3 (prim_ffi_callback.c3)
extern void omni_ffi_callback_dispatch(void* user_data, void* ret, void** args);

typedef struct {
    ffi_closure* closure;
    ffi_cif* cif;
    ffi_type** atypes;
} OmniFfiClosure;

static void omni_ffi_closure_handler(ffi_cif* cif, void* ret, void** args, void* user_data) {
    (void)cif;
    omni_ffi_callback_dispatch(user_data, ret, args);
}

// omni_ffi_closure_alloc — allocate a libffi closure for callbacks into Omni.
// nargs:      number of arguments
// arg_types:  array of OMNI_FFI_* type codes for each arg
// ret_type:   OMNI_FFI_* type code for return
// out_closure: receives opaque closure handle (free with omni_ffi_closure_free)
// out_code:    receives the C-callable function pointer
// user_data:   opaque pointer passed back to omni_ffi_callback_dispatch
// Returns: 0 on success, -1 on error
int omni_ffi_closure_alloc(int nargs, int* arg_types, int ret_type,
                           void** out_closure, void** out_code,
                           void* user_data) {
    if (nargs < 0 || out_closure == NULL || out_code == NULL) return -1;

    OmniFfiClosure* closure = (OmniFfiClosure*)calloc(1, sizeof(OmniFfiClosure));
    if (closure == NULL) return -1;

    closure->cif = (ffi_cif*)malloc(sizeof(ffi_cif));
    if (closure->cif == NULL) {
        free(closure);
        return -1;
    }

    if (nargs > 0) {
        closure->atypes = (ffi_type**)malloc((size_t)nargs * sizeof(ffi_type*));
        if (closure->atypes == NULL) {
            free(closure->cif);
            free(closure);
            return -1;
        }
        for (int i = 0; i < nargs; i++) {
            closure->atypes[i] = omni_to_ffi_type(arg_types[i]);
        }
    }

    ffi_type* rtype = omni_to_ffi_type(ret_type);
    ffi_status status = ffi_prep_cif(closure->cif, FFI_DEFAULT_ABI,
                                      (unsigned int)nargs, rtype, closure->atypes);
    if (status != FFI_OK) {
        free(closure->atypes);
        free(closure->cif);
        free(closure);
        return -1;
    }

    closure->closure = ffi_closure_alloc(sizeof(ffi_closure), out_code);
    if (closure->closure == NULL) {
        free(closure->atypes);
        free(closure->cif);
        free(closure);
        return -1;
    }

    status = ffi_prep_closure_loc(closure->closure, closure->cif,
                                   omni_ffi_closure_handler, user_data, *out_code);
    if (status != FFI_OK) {
        ffi_closure_free(closure->closure);
        free(closure->atypes);
        free(closure->cif);
        free(closure);
        return -1;
    }

    *out_closure = closure;
    return 0;
}

void omni_ffi_closure_free(void* closure_handle) {
    OmniFfiClosure* closure = (OmniFfiClosure*)closure_handle;
    if (closure == NULL) return;
    if (closure->closure != NULL) ffi_closure_free(closure->closure);
    free(closure->atypes);
    free(closure->cif);
    free(closure);
}

// Simple test helper: invoke a callback with two int args and return int.
int omni_ffi_test_callback_int_int(void* cb, int a, int b) {
    int (*fn)(int, int) = (int (*)(int, int))cb;
    return fn(a, b);
}
