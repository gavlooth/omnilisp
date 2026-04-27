module lisp;

import std::io;
import std::core::mem;
import std::atomic::types;
import main;

// =============================================================================
// FFI CALLBACKS (libffi closures)
//
// (ffi-callback lambda ['Type1 'Type2 ...] 'ReturnType)
//   → ForeignHandle wrapping a C-callable function pointer
//
// When C code invokes the closure, libffi marshals args and calls
// omni_ffi_callback_dispatch (exported from C3 via @extern), which
// converts C args to Omni values, calls the lambda, and writes the
// result back.
// =============================================================================

alias FfiCallbackDispatchFn = fn void(void* user_data, void* ret, void** args);

extern fn int omni_ffi_closure_alloc(int nargs, int* arg_types, int ret_type,
                                     void** out_closure, void** out_code,
                                     void* user_data) @extern("omni_ffi_closure_alloc");
extern fn void omni_ffi_closure_free(void* closure) @extern("omni_ffi_closure_free");
extern fn int omni_ffi_test_callback_int_int(void* cb, int a, int b) @extern("omni_ffi_test_callback_int_int");
extern fn int omni_ffi_test_callback_string_len(void* cb, char* text) @extern("omni_ffi_test_callback_string_len");
extern fn int omni_ffi_call_null_arg_vectors_guard_for_tests() @extern("omni_ffi_call_null_arg_vectors_guard_for_tests");
extern fn int omni_ffi_call_var_null_arg_vectors_guard_for_tests() @extern("omni_ffi_call_var_null_arg_vectors_guard_for_tests");
extern fn int omni_ffi_closure_alloc_failure_clears_outputs_for_tests() @extern("omni_ffi_closure_alloc_failure_clears_outputs_for_tests");

fn Value* ffi_callback_make_arg_cons(Interp* interp, Value* arg_val, Value* arg_list) @inline {
    return make_cons_or_error(interp, arg_val, arg_list, "ffi-callback: failed to allocate callback argument list");
}

fn bool ffi_callback_value_to_type_tag(Value* value, Interp* interp, FfiTypeTag* out_tag, FfiHandlePolicy* out_policy) {
    if (value == null || interp == null || out_tag == null || out_policy == null) return false;
    SymbolId type_sym = INVALID_SYMBOL_ID;
    if (value.tag == SYMBOL) {
        type_sym = value.sym_val;
    } else if (value.tag == PRIMITIVE && value.prim_val != null) {
        usz name_len = 0;
        while (name_len < 64 && value.prim_val.name[name_len] != 0) name_len++;
        if (name_len == 0 || name_len >= 64) return false;
        type_sym = interp.symbols.intern(value.prim_val.name[0:name_len]);
    } else {
        return false;
    }
    if (type_sym == INVALID_SYMBOL_ID) return false;

    TypeAnnotation ann = {};
    ann.has_annotation = true;
    ann.is_dict = false;
    ann.base_type = type_sym;
    return ffi_type_annotation_to_tag(&ann, interp, out_tag, out_policy);
}

struct FfiCallbackContext {
    Value* callback;
    Interp* interp;
    main::ScopeRegion* owner_scope;
    FfiTypeTag return_type;
    FfiTypeTag* param_types;
    usz param_count;
    void* closure_handle;  // opaque handle for omni_ffi_closure_free
    void* code_ptr;        // C-callable function pointer
    types::Atomic{int} open;
    types::Atomic{int} active_calls;
    types::Atomic{int} finalize_requested;
    types::Atomic{int} resources_released;
}

fn bool ffi_callback_callable(Value* v) @inline {
    if (v == null) return false;
    ValueTag t = v.tag;
    return t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE;
}

fn main::ScopeRegion* resolve_ffi_callback_owner_scope(Value* callback_v, Interp* interp) {
    if (callback_v == null || interp == null) return null;
    main::ScopeRegion* scope = interp.current_scope;
    while (scope != null) {
        if (main::is_in_scope((void*)callback_v, scope)) return scope;
        scope = scope.parent;
    }
    if (interp.root_scope != null && main::is_in_scope((void*)callback_v, interp.root_scope)) {
        return interp.root_scope;
    }
    return null;
}

// Convert a runtime type Value to FfiTypeTag.
fn bool ffi_symbol_value_to_tag(Value* v, Interp* interp, FfiTypeTag* out_tag) {
    FfiHandlePolicy ignored_policy = {};
    return ffi_callback_value_to_type_tag(v, interp, out_tag, &ignored_policy);
}

fn bool ffi_callback_parameter_type_supported(FfiTypeTag tag) @inline {
    return tag != FFI_TYPE_VOID;
}

// Convert a single C argument (pointed to by arg_ptr) to an Omni Value.
fn Value* ffi_callback_arg_to_value(void* arg_ptr, FfiTypeTag tag, Interp* interp) {
    switch (tag) {
        case FFI_TYPE_INT:
            return make_int(interp, *(long*)arg_ptr);
        case FFI_TYPE_BOOL:
            return make_symbol(interp, (*(long*)arg_ptr) != 0 ? interp.sym_true : interp.sym_false);
        case FFI_TYPE_DOUBLE:
            return make_double(interp, *(double*)arg_ptr);
        case FFI_TYPE_FLOAT32:
            return make_float32(interp, *(float*)arg_ptr);
        case FFI_TYPE_INT32:
            return make_int(interp, (long)*(int*)arg_ptr);
        case FFI_TYPE_INT64:
            return make_int(interp, *(long*)arg_ptr);
        case FFI_TYPE_UINT32:
            return make_int(interp, (long)*(uint*)arg_ptr);
        case FFI_TYPE_UINT64: {
            ulong value = *(ulong*)arg_ptr;
            if (value > (ulong)long.max) {
                return ffi_raise(interp, "ffi/range-error", "ffi-callback: UInt64 argument exceeds Integer range");
            }
            return make_int(interp, (long)value);
        }
        case FFI_TYPE_STRING: {
            char* text = *(char**)arg_ptr;
            if (text == null) return make_nil(interp);
            return make_string(interp, text[:ffi_c_string_len(text)]);
        }
        case FFI_TYPE_PTR:
        case FFI_TYPE_BUFFER:
        case FFI_TYPE_STRUCT: {
            void* ptr = *(void**)arg_ptr;
            if (ptr == null) return make_nil(interp);
            char[] name = tag == FFI_TYPE_BUFFER ? "ffi-callback-buffer"
                        : tag == FFI_TYPE_STRUCT ? "ffi-callback-struct"
                        : "ffi-callback-ptr";
            return make_ffi_handle_ex(interp, ptr, name, null, false, false, null);
        }
        case FFI_TYPE_VOID:
            return make_nil(interp);
    }
}

// Convert an Omni result Value to C and write to ret_ptr.
fn void ffi_callback_value_to_c(Value* value, FfiTypeTag tag, void* ret_ptr, Interp* interp) {
    if (ret_ptr == null) return;
    switch (tag) {
        case FFI_TYPE_VOID:
            break;
        case FFI_TYPE_INT:
            if (value != null && value.tag == INT) {
                *(long*)ret_ptr = value.int_val;
            } else {
                *(long*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_BOOL:
            if (value != null && value.tag == SYMBOL && interp != null && value.sym_val == interp.sym_true) {
                *(long*)ret_ptr = 1;
            } else if (value != null && value.tag == INT) {
                *(long*)ret_ptr = value.int_val != 0 ? 1 : 0;
            } else {
                *(long*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_DOUBLE:
            if (value != null && value.tag == DOUBLE) {
                *(double*)ret_ptr = value.double_val;
            } else {
                *(double*)ret_ptr = 0.0;
            }
            break;
        case FFI_TYPE_FLOAT32:
            if (value != null && value.tag == FLOAT32) {
                *(float*)ret_ptr = value.float32_val;
            } else {
                *(float*)ret_ptr = 0.0f;
            }
            break;
        case FFI_TYPE_INT32:
            if (value != null && value.tag == INT) {
                *(int*)ret_ptr = (int)value.int_val;
            } else {
                *(int*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_INT64:
            if (value != null && value.tag == INT) {
                *(long*)ret_ptr = value.int_val;
            } else {
                *(long*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_UINT32:
            if (value != null && value.tag == INT && value.int_val >= 0 && value.int_val <= (long)uint.max) {
                *(uint*)ret_ptr = (uint)value.int_val;
            } else {
                *(uint*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_UINT64:
            if (value != null && value.tag == INT && value.int_val >= 0) {
                *(ulong*)ret_ptr = (ulong)value.int_val;
            } else {
                *(ulong*)ret_ptr = 0;
            }
            break;
        case FFI_TYPE_PTR:
        case FFI_TYPE_BUFFER:
        case FFI_TYPE_STRUCT:
            if (value != null && value.tag == FFI_HANDLE && value.ffi_val != null) {
                *(void**)ret_ptr = value.ffi_val.lib_handle;
            } else if (value == null || value.tag == NIL) {
                *(void**)ret_ptr = null;
            } else {
                *(void**)ret_ptr = null;
            }
            break;
        case FFI_TYPE_STRING:
            *(void**)ret_ptr = null;
            break;
    }
}

fn void release_ffi_callback_owner_scope(FfiCallbackContext* ctx) {
    if (ctx == null) return;
    if (ctx.owner_scope != null) {
        main::scope_release(ctx.owner_scope);
        ctx.owner_scope = null;
    }
    ctx.callback = null;
}

fn void ffi_callback_release_resources(FfiCallbackContext* ctx) {
    if (ctx == null) return;
    if (ctx.resources_released.add(1) != 0) return;
    if (ctx.closure_handle != null) {
        omni_ffi_closure_free(ctx.closure_handle);
        ctx.closure_handle = null;
    }
    release_ffi_callback_owner_scope(ctx);
    ctx.interp = null;
    ctx.code_ptr = null;
    if (ctx.param_types != null) {
        mem::free(ctx.param_types);
        ctx.param_types = null;
    }
}

fn void ffi_callback_context_finalizer(void* raw) {
    FfiCallbackContext* ctx = (FfiCallbackContext*)raw;
    if (ctx == null) return;
    ctx.open.store(0);
    ctx.finalize_requested.store(1);
    if (ctx.active_calls.load() == 0) {
        ffi_callback_release_resources(ctx);
    }
    // Keep the small context tombstone allocated: foreign code may still hold a
    // stale function pointer/user_data pair, and the open=false state makes it
    // fail closed without reading freed context memory.
}

fn bool ffi_callback_try_enter_context(FfiCallbackContext* ctx) {
    if (ctx == null) return false;
    ctx.active_calls.add(1);
    if (ctx.open.load() == 0 || ctx.callback == null || ctx.interp == null) {
        ffi_callback_leave((void*)ctx);
        return false;
    }
    if (!ffi_callback_callable(ctx.callback)) {
        ffi_callback_leave((void*)ctx);
        return false;
    }
    return true;
}

fn int ffi_callback_try_enter(void* user_data) @extern("omni_ffi_callback_try_enter") {
    return ffi_callback_try_enter_context((FfiCallbackContext*)user_data) ? 1 : 0;
}

fn void ffi_callback_leave(void* user_data) @extern("omni_ffi_callback_leave") {
    FfiCallbackContext* ctx = (FfiCallbackContext*)user_data;
    if (ctx == null) return;
    int previous = ctx.active_calls.add(-1);
    if (previous <= 0) {
        ctx.active_calls.store(0);
        return;
    }
    if (previous == 1 && ctx.finalize_requested.load() != 0) {
        ffi_callback_release_resources(ctx);
    }
}

// Dispatches a libffi closure callback into Omni after the C helper has entered
// the context through omni_ffi_callback_try_enter.
fn void ffi_callback_dispatch(void* user_data, void* ret, void** args) @extern("omni_ffi_callback_dispatch") {
    FfiCallbackContext* ctx = (FfiCallbackContext*)user_data;
    if (ctx == null || ctx.open.load() == 0 || ctx.callback == null || ctx.interp == null) return;

    Interp* interp = ctx.interp;
    ffi_callback_value_to_c(null, ctx.return_type, ret, interp);

    // Build argument list
    Value* arg_list = make_nil(interp);
    if (ctx.param_count > 0) {
        // Build list backwards for correct order
        for (usz i = ctx.param_count; i > 0; i--) {
            usz idx = i - 1;
            Value* arg_val = ffi_callback_arg_to_value(args[idx], ctx.param_types[idx], interp);
            if (arg_val == null || arg_val.tag == ERROR) return;
            arg_list = ffi_callback_make_arg_cons(interp, arg_val, arg_list);
            if (arg_list == null || arg_list.tag == ERROR) return;
        }
    }

    Value* result = jit_apply_multi_args(interp, ctx.callback, arg_list, ctx.param_count);
    if (result != null && result.tag == ERROR) return;

    ffi_callback_value_to_c(result, ctx.return_type, ret, interp);
}

// (ffi-callback lambda ['Type1 'Type2 ...] 'ReturnType)
//   OR
// (ffi-callback lambda 'Type1 'Type2 ... 'ReturnType)
<*
@require interp != null
*>
fn Value* prim_ffi_callback(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 3) {
        return ffi_raise(interp, "ffi/arity", "ffi-callback: expected (ffi-callback lambda param-types return-type)");
    }

    Value* callback_v = args[0];
    if (!ffi_callback_callable(callback_v)) {
        return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: first arg must be a callable");
    }

    // Parse parameter types and return type.
    // Support two forms:
    //   (ffi-callback lambda ['Int 'Int] 'Int)
    //   (ffi-callback lambda 'Int 'Int 'Int)
    usz param_count = 0;
    FfiTypeTag return_type = FFI_TYPE_VOID;
    FfiTypeTag* param_types = null;

    if (args.len == 3 && (args[1].tag == CONS || args[1].tag == ARRAY)) {
        // Form: (ffi-callback lambda ['Type1 'Type2 ...] 'ReturnType)
        // Supports both list ('(Type1 Type2)) and array ([Type1 Type2]) syntax.
        Value* type_list = args[1];
        usz count = 0;
        if (type_list.tag == CONS) {
            Value* cur = type_list;
            while (is_cons(cur)) {
                count++;
                cur = cur.cons_val.cdr;
            }
        } else {
            count = type_list.array_val != null ? type_list.array_val.length : 0;
        }
        param_count = count;
        if (param_count > (usz)int.max) {
            return ffi_raise(interp, "ffi/arity", "ffi-callback: parameter count exceeds supported arity");
        }
        if (param_count > usz.max / FfiTypeTag.sizeof) {
            return ffi_raise(interp, "ffi/invalid-argument", "ffi-callback: parameter table size overflow");
        }
        if (param_count > 0) {
            param_types = (FfiTypeTag*)mem::malloc(FfiTypeTag.sizeof * param_count);
            if (param_types == null) {
                return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: out of memory");
            }
            if (type_list.tag == CONS) {
                Value* cur = type_list;
                usz i = 0;
                while (is_cons(cur)) {
                    if (!ffi_symbol_value_to_tag(cur.cons_val.car, interp, &param_types[i]) ||
                        !ffi_callback_parameter_type_supported(param_types[i])) {
                        mem::free(param_types);
                        return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: unsupported parameter type (expected symbol like 'Integer)");
                    }
                    i++;
                    cur = cur.cons_val.cdr;
                }
            } else {
                for (usz i = 0; i < param_count; i++) {
                    if (!ffi_symbol_value_to_tag(type_list.array_val.items[i], interp, &param_types[i]) ||
                        !ffi_callback_parameter_type_supported(param_types[i])) {
                        mem::free(param_types);
                        return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: unsupported parameter type (expected symbol like 'Integer)");
                    }
                }
            }
        }
        // Return type is args[2]
        if (!ffi_symbol_value_to_tag(args[2], interp, &return_type)) {
            if (param_types != null) mem::free(param_types);
            return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: unsupported return type (expected symbol like 'Integer)");
        }
        if (return_type == FFI_TYPE_STRING) {
            if (param_types != null) mem::free(param_types);
            return ffi_raise(interp, "ffi/unsupported", "ffi-callback: String returns are unsupported because callback return string ownership is not defined");
        }
    } else {
        // Form: (ffi-callback lambda 'Type1 'Type2 ... 'ReturnType)
        param_count = args.len - 2;
        if (param_count > (usz)int.max) {
            return ffi_raise(interp, "ffi/arity", "ffi-callback: parameter count exceeds supported arity");
        }
        if (param_count > usz.max / FfiTypeTag.sizeof) {
            return ffi_raise(interp, "ffi/invalid-argument", "ffi-callback: parameter table size overflow");
        }
        if (param_count > 0) {
            param_types = (FfiTypeTag*)mem::malloc(FfiTypeTag.sizeof * param_count);
            if (param_types == null) {
                return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: out of memory");
            }
            for (usz i = 0; i < param_count; i++) {
                if (!ffi_symbol_value_to_tag(args[i + 1], interp, &param_types[i]) ||
                    !ffi_callback_parameter_type_supported(param_types[i])) {
                    mem::free(param_types);
                    return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: unsupported parameter type (expected symbol like 'Integer)");
                }
            }
        }
        if (!ffi_symbol_value_to_tag(args[args.len - 1], interp, &return_type)) {
            if (param_types != null) mem::free(param_types);
            return ffi_raise(interp, "ffi/type-mismatch", "ffi-callback: unsupported return type (expected symbol like 'Integer)");
        }
        if (return_type == FFI_TYPE_STRING) {
            if (param_types != null) mem::free(param_types);
            return ffi_raise(interp, "ffi/unsupported", "ffi-callback: String returns are unsupported because callback return string ownership is not defined");
        }
    }

    main::ScopeRegion* owner_scope = resolve_ffi_callback_owner_scope(callback_v, interp);
    if (owner_scope == null) {
        if (param_types != null) mem::free(param_types);
        return ffi_raise(interp, "ffi/invalid-state", "ffi-callback: callback lifetime is not owned by the active interpreter scope chain");
    }

    FfiCallbackContext* ctx = (FfiCallbackContext*)mem::malloc(FfiCallbackContext.sizeof);
    if (ctx == null) {
        if (param_types != null) mem::free(param_types);
        return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: out of memory");
    }
    *ctx = {};
    main::scope_retain(owner_scope);
    ctx.callback = callback_v;
    ctx.interp = interp;
    ctx.owner_scope = owner_scope;
    ctx.return_type = return_type;
    ctx.param_types = param_types;
    ctx.param_count = param_count;
    ctx.open.store(1);
    ctx.active_calls.store(0);
    ctx.finalize_requested.store(0);
    ctx.resources_released.store(0);

    // Allocate libffi closure
    int* c_arg_types = null;
    if (param_count > 0) {
        if (param_count > usz.max / int.sizeof) {
            release_ffi_callback_owner_scope(ctx);
            if (param_types != null) mem::free(param_types);
            mem::free(ctx);
            return ffi_raise(interp, "ffi/invalid-argument", "ffi-callback: parameter table size overflow");
        }
        c_arg_types = (int*)mem::malloc(int.sizeof * param_count);
        if (c_arg_types == null) {
            release_ffi_callback_owner_scope(ctx);
            if (param_types != null) mem::free(param_types);
            mem::free(ctx);
            return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: out of memory");
        }
        for (usz i = 0; i < param_count; i++) {
            c_arg_types[i] = (int)param_types[i];
        }
    }

    int rc = omni_ffi_closure_alloc(
        (int)param_count,
        c_arg_types,
        (int)return_type,
        &ctx.closure_handle,
        &ctx.code_ptr,
        (void*)ctx
    );

    if (c_arg_types != null) mem::free(c_arg_types);

    if (rc != 0 || ctx.closure_handle == null || ctx.code_ptr == null) {
        release_ffi_callback_owner_scope(ctx);
        if (param_types != null) mem::free(param_types);
        mem::free(ctx);
        return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: failed to allocate closure (libffi)");
    }

    Value* handle = make_ffi_handle_ex(
        interp,
        ctx.code_ptr,
        "ffi-callback",
        &ffi_callback_context_finalizer,
        true,
        false,
        null,
        ctx
    );
    if (handle == null) {
        omni_ffi_closure_free(ctx.closure_handle);
        ctx.closure_handle = null;
        release_ffi_callback_owner_scope(ctx);
        if (param_types != null) mem::free(param_types);
        mem::free(ctx);
        return ffi_raise(interp, "ffi/out-of-memory", "ffi-callback: out of memory");
    }

    // Keepalive: ensure ffi_callback_dispatch is not DCE'd by the C3 compiler.
    (void)ffi_callback_try_enter(null);
    ffi_callback_dispatch(null, null, null);
    ffi_callback_leave(null);

    return handle;
}
