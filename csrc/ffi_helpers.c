// ffi_helpers.c — thin C wrapper around libffi for Omni's FFI system.
// Reason: libffi uses C structs (ffi_type, ffi_cif) that are hard to declare in C3.

#include <ffi.h>

// Type codes matching Omni's FFI type enum (must stay in sync with value.c3 FfiTypeTag)
enum {
    OMNI_FFI_VOID   = 0,
    OMNI_FFI_INT    = 1,
    OMNI_FFI_DOUBLE = 2,
    OMNI_FFI_PTR    = 3,  // pointer (includes String, Ptr)
    OMNI_FFI_BOOL   = 4,
};

static ffi_type* omni_to_ffi_type(int t) {
    switch (t) {
        case OMNI_FFI_VOID:   return &ffi_type_void;
        case OMNI_FFI_INT:    return &ffi_type_sint64;
        case OMNI_FFI_DOUBLE: return &ffi_type_double;
        case OMNI_FFI_PTR:    return &ffi_type_pointer;
        case OMNI_FFI_BOOL:   return &ffi_type_sint64;
        default:              return &ffi_type_pointer;
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
    if (nargs > 16) return -1;

    ffi_cif cif;
    ffi_type* atypes[16];
    for (int i = 0; i < nargs; i++) {
        atypes[i] = omni_to_ffi_type(arg_types[i]);
    }
    ffi_type* rtype = omni_to_ffi_type(ret_type);

    ffi_status status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned int)nargs, rtype, atypes);
    if (status != FFI_OK) return -1;

    ffi_call(&cif, (void (*)(void))fn_ptr, ret_value, arg_values);
    return 0;
}

// omni_ffi_call_var — variadic function call via libffi.
// fixed_count: number of fixed (non-variadic) arguments
int omni_ffi_call_var(void* fn_ptr, int nargs, int fixed_count,
                      int* arg_types, void** arg_values,
                      int ret_type, void* ret_value) {
    if (nargs > 16) return -1;

    ffi_cif cif;
    ffi_type* atypes[16];
    for (int i = 0; i < nargs; i++) {
        atypes[i] = omni_to_ffi_type(arg_types[i]);
    }
    ffi_type* rtype = omni_to_ffi_type(ret_type);

    ffi_status status = ffi_prep_cif_var(&cif, FFI_DEFAULT_ABI,
                                          (unsigned int)fixed_count,
                                          (unsigned int)nargs,
                                          rtype, atypes);
    if (status != FFI_OK) return -1;

    ffi_call(&cif, (void (*)(void))fn_ptr, ret_value, arg_values);
    return 0;
}
