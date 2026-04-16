module lisp;

alias ForeignAdapterDescribeFn = fn Value*(Value* value, Interp* interp);
alias ForeignAdapterReleaseFn = fn Value*(Value* handle_v, Interp* interp);
alias ForeignAdapterLoadBoundFn = fn Value*(FfiBoundFn* bound, Interp* interp);
alias ForeignAdapterResolveBoundFn = fn Value*(FfiBoundFn* bound, Interp* interp);
alias ForeignAdapterCallBoundFn = fn Value*(FfiBoundFn* bound, Value*[] args, Env* env, Interp* interp);
alias ForeignAdapterImportModuleFn = fn Value*(Value* runtime_v, Value* module_descriptor, Interp* interp);
alias ForeignAdapterResolveMemberFn = fn Value*(Value* owner_v, Value* member_descriptor, Interp* interp);
alias ForeignAdapterTensorBufferFn = fn Value*(Value* tensor_v, Interp* interp);

struct ForeignRuntimeAdapter {
    ForeignRuntimeKind runtime_kind;
    uint operation_capabilities;
    ForeignAdapterDescribeFn describe_handle;
    ForeignAdapterDescribeFn describe_callable;
    ForeignAdapterReleaseFn release_handle;
    ForeignAdapterLoadBoundFn load_bound;
    ForeignAdapterResolveBoundFn resolve_bound;
    ForeignAdapterCallBoundFn call_bound;
    ForeignAdapterImportModuleFn import_module;
    ForeignAdapterResolveMemberFn resolve_member;
    ForeignAdapterTensorBufferFn tensor_buffer;
}

fn char[] foreign_runtime_kind_name(ForeignRuntimeKind kind) {
    switch (kind) {
        case FOREIGN_RUNTIME_NATIVE:
            return "native";
        case FOREIGN_RUNTIME_C_ABI:
            return "c-abi";
        case FOREIGN_RUNTIME_PYTHON:
            return "python";
        case FOREIGN_RUNTIME_JULIA:
            return "julia";
        case FOREIGN_RUNTIME_CUDA:
            return "cuda";
        case FOREIGN_RUNTIME_CPP:
            return "cpp";
        case FOREIGN_RUNTIME_POLYGLOT:
            return "polyglot";
        default:
            return "unknown";
    }
}

fn bool foreign_runtime_adapter_supports(ForeignRuntimeAdapter* adapter, uint capability) @inline {
    return adapter != null && (adapter.operation_capabilities & capability) == capability;
}

fn char[] foreign_handle_kind_name(ForeignHandleKind kind) {
    switch (kind) {
        case FOREIGN_HANDLE_OPAQUE:
            return "opaque";
        case FOREIGN_HANDLE_LIBRARY:
            return "library";
        case FOREIGN_HANDLE_MODULE:
            return "module";
        case FOREIGN_HANDLE_CALLABLE:
            return "callable";
        case FOREIGN_HANDLE_OBJECT:
            return "object";
        case FOREIGN_HANDLE_BUFFER:
            return "buffer";
        case FOREIGN_HANDLE_RUNTIME:
            return "runtime";
        default:
            return "unknown";
    }
}

fn char[] foreign_handle_ownership_name(FfiHandle* handle) {
    if (handle == null) return "borrowed";
    if (handle.lib_handle == null) return "borrowed";
    switch (handle.ownership) {
        case FFI_HANDLE_OWNED:
            return "owned";
        case FFI_HANDLE_MANUAL:
            return "manual";
        default:
            break;
    }
    if (handle.finalizer != null || handle.free_lib_handle) return "owned";
    return "borrowed";
}

fn char[] foreign_handle_policy_ownership_name(FfiHandlePolicy* policy) {
    if (policy == null) return "borrowed";
    switch (policy.ownership) {
        case FFI_HANDLE_BORROWED:
            return "borrowed";
        case FFI_HANDLE_OWNED:
            return "owned";
        case FFI_HANDLE_MANUAL:
            return "manual";
        default:
            return "unknown";
    }
}

fn char[] foreign_handle_policy_nullability_name(FfiHandlePolicy* policy) {
    if (policy == null) return "nullable";
    switch (policy.nullability) {
        case FFI_HANDLE_NULLABLE:
            return "nullable";
        case FFI_HANDLE_NON_NULL:
            return "non-null";
        default:
            return "unknown";
    }
}

fn char[] foreign_ffi_abi_type_name(FfiTypeTag tag) {
    switch (tag) {
        case FFI_TYPE_VOID:
            return "void";
        case FFI_TYPE_INT:
            return "int";
        case FFI_TYPE_DOUBLE:
            return "double";
        case FFI_TYPE_PTR:
            return "ptr";
        case FFI_TYPE_BOOL:
            return "bool";
        case FFI_TYPE_STRING:
            return "string";
        default:
            return "unknown";
    }
}

fn char[] foreign_ffi_surface_type_name(FfiTypeTag tag, FfiHandlePolicy* policy) {
    switch (tag) {
        case FFI_TYPE_VOID:
            return "Void";
        case FFI_TYPE_INT:
            return "Integer";
        case FFI_TYPE_DOUBLE:
            return "Float64";
        case FFI_TYPE_PTR:
            if (policy != null && policy.is_foreign_handle) return "ForeignHandle";
            return "ForeignHandle";
        case FFI_TYPE_BOOL:
            return "Boolean";
        case FFI_TYPE_STRING:
            return "String";
        default:
            return "Unknown";
    }
}

fn Value* foreign_runtime_symbol_value(Interp* interp, char[] name) {
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return raise_error(interp, "foreign-describe: failed to intern metadata symbol");
    return make_symbol(interp, sym);
}

fn bool foreign_runtime_array_push_value(Value* arr_v, Interp* interp, Value* value) {
    if (arr_v == null || arr_v.tag != ARRAY || arr_v.array_val == null) return false;
    Array* arr = arr_v.array_val;
    if (arr.length >= arr.capacity) return false;
    value = boundary_promote_to_root(value, interp);
    if (value == null || value.tag == ERROR) return false;
    arr.items[arr.length++] = value;
    return true;
}

fn bool foreign_runtime_array_push_symbol(Value* arr_v, Interp* interp, char[] name) {
    Value* sym = foreign_runtime_symbol_value(interp, name);
    if (sym == null || sym.tag == ERROR) return false;
    return foreign_runtime_array_push_value(arr_v, interp, sym);
}

fn Value* foreign_runtime_capabilities_array(Interp* interp, uint capabilities) {
    Value* arr = make_array_checked(interp, 8, "foreign-describe: out of memory");
    if (arr == null || arr.tag == ERROR) return arr;
    if ((capabilities & FOREIGN_CAP_LOAD) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "load")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_RESOLVE) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "resolve")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_CALL) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "call")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_REFLECT) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "reflect")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_TENSOR_BUFFER) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "tensor-buffer")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_RELEASE) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "release")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_IMPORT) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "import")) return raise_error(interp, "foreign-describe: out of memory");
    if ((capabilities & FOREIGN_CAP_MEMBER) != 0
        && !foreign_runtime_array_push_symbol(arr, interp, "member")) return raise_error(interp, "foreign-describe: out of memory");
    return arr;
}

fn Value* foreign_runtime_dict_set_symbol(Value* dict, Interp* interp, char[] key, char[] value) {
    Value* val = foreign_runtime_symbol_value(interp, value);
    if (val == null || val.tag == ERROR) return val;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return raise_error(interp, "foreign-describe: failed to intern metadata key");
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, val, interp)) {
        return raise_error(interp, "foreign-describe: out of memory");
    }
    return null;
}

fn Value* foreign_runtime_dict_set_value(Value* dict, Interp* interp, char[] key, Value* value) {
    if (value == null || value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return raise_error(interp, "foreign-describe: failed to intern metadata key");
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) {
        return raise_error(interp, "foreign-describe: out of memory");
    }
    return null;
}

fn Value* foreign_runtime_dict_set_symbol_id(Value* dict, Interp* interp, char[] key, SymbolId value) {
    if (value == INVALID_SYMBOL_ID) return null;
    if (Value* err = foreign_runtime_dict_set_value(dict, interp, key, make_symbol(interp, value))) return err;
    return null;
}

fn Value* foreign_runtime_handle_name_value(FfiHandle* handle, Interp* interp) {
    if (handle == null || handle.name_len == 0) return make_nil(interp);
    char[] name = handle.lib_name[:handle.name_len];
    if (handle.handle_kind == FOREIGN_HANDLE_LIBRARY) return make_string(interp, name);
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return raise_error(interp, "foreign-describe: failed to intern handle name symbol");
    return make_symbol(interp, sym);
}

fn Value* foreign_runtime_ffi_type_descriptor(Interp* interp, FfiTypeTag tag, FfiHandlePolicy* policy) {
    Value* out = make_hashmap_checked(interp, 12, "foreign-describe: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "type",
        foreign_ffi_surface_type_name(tag, policy))) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "abi-type",
        foreign_ffi_abi_type_name(tag))) return err;
    if (tag == FFI_TYPE_PTR && policy != null && policy.is_foreign_handle) {
        if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "ownership",
            foreign_handle_policy_ownership_name(policy))) return err;
        if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "nullability",
            foreign_handle_policy_nullability_name(policy))) return err;
        if (Value* err = foreign_runtime_dict_set_symbol_id(out, interp, "name", policy.name)) return err;
        if (Value* err = foreign_runtime_dict_set_symbol_id(out, interp, "finalizer", policy.finalizer_symbol)) return err;
    }
    return out;
}

fn Value* foreign_runtime_parameters_array(FfiBoundFn* bound, Interp* interp) {
    if (bound == null) return raise_error(interp, "foreign-describe: missing FFI binding metadata");
    if (bound.param_count > 0 && bound.param_types == null) {
        return raise_error(interp, "foreign-describe: missing FFI parameter metadata");
    }
    Value* arr = make_array_checked(interp, bound.param_count, "foreign-describe: out of memory");
    if (arr == null || arr.tag == ERROR) return arr;
    for (usz i = 0; i < bound.param_count; i++) {
        FfiHandlePolicy* policy = null;
        if (bound.param_handle_policies != null) policy = &bound.param_handle_policies[i];
        FfiTypeTag tag = bound.param_types[i];
        Value* param = foreign_runtime_ffi_type_descriptor(interp, tag, policy);
        if (param == null || param.tag == ERROR) return param;
        if (Value* err = foreign_runtime_dict_set_value(param, interp, "index", make_int(interp, (long)i))) return err;
        if (!foreign_runtime_array_push_value(arr, interp, param)) {
            return raise_error(interp, "foreign-describe: out of memory");
        }
    }
    return arr;
}

fn Value* foreign_runtime_handle_descriptor_payload(FfiHandle* handle, Interp* interp) {
    Value* out = make_hashmap_checked(interp, 16, "foreign-describe: out of memory");
    if (out == null || out.tag == ERROR) return out;
    uint capabilities = handle.capabilities;
    if (!ffi_handle_can_release_payload(handle)) {
        capabilities = capabilities & ~FOREIGN_CAP_RELEASE;
    }

    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "type", "ForeignHandle")) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "runtime", foreign_runtime_kind_name(handle.runtime_kind))) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "kind", foreign_handle_kind_name(handle.handle_kind))) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "ownership", foreign_handle_ownership_name(handle))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "name",
        foreign_runtime_handle_name_value(handle, interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "live",
        handle.lib_handle != null ? make_symbol(interp, interp.sym_true) : make_nil(interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "capabilities",
        foreign_runtime_capabilities_array(interp, capabilities))) return err;
    return out;
}

fn usz foreign_runtime_primitive_name_len(Primitive* prim) {
    if (prim == null) return 0;
    for (usz i = 0; i < 64; i++) {
        if (prim.name[i] == 0) return i;
    }
    return 64;
}

fn Value* foreign_runtime_describe_bound_function(Value* callable_v, Interp* interp) {
    if (callable_v == null || callable_v.tag != PRIMITIVE || callable_v.prim_val == null ||
        callable_v.prim_val.func != &prim_ffi_bound_call || callable_v.prim_val.user_data == null) {
        return raise_error(interp, "foreign-describe: expected ForeignHandle or FFI-bound function");
    }
    Primitive* prim = callable_v.prim_val;
    FfiBoundFn* bound = (FfiBoundFn*)prim.user_data;
    Value* out = make_hashmap_checked(interp, 16, "foreign-describe: out of memory");
    if (out == null || out.tag == ERROR) return out;

    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "type", "ForeignCallable")) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "runtime",
        foreign_runtime_kind_name(foreign_runtime_bound_kind_for_metadata(bound)))) return err;
    if (Value* err = foreign_runtime_dict_set_symbol(out, interp, "kind", "function")) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "name",
        make_string(interp, prim.name[:foreign_runtime_primitive_name_len(prim)]))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "c-name",
        bound.c_name != null ? make_string(interp, bound.c_name[:bound.c_name_len]) : make_nil(interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "library",
        bound.lib_name != null ? make_string(interp, bound.lib_name[:bound.lib_name_len]) : make_nil(interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "parameters",
        foreign_runtime_parameters_array(bound, interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "returns",
        foreign_runtime_ffi_type_descriptor(interp, bound.return_type, &bound.return_handle_policy))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "resolved",
        bound.fn_ptr != null ? make_symbol(interp, interp.sym_true) : make_nil(interp))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "available",
        bound.is_unavailable ? make_nil(interp) : make_symbol(interp, interp.sym_true))) return err;
    if (Value* err = foreign_runtime_dict_set_value(out, interp, "capabilities",
        foreign_runtime_capabilities_array(interp, FOREIGN_CAP_CALL | FOREIGN_CAP_REFLECT))) return err;
    return out;
}

fn Value* foreign_runtime_describe_handle(Value* handle_v, Interp* interp) {
    if (handle_v == null || handle_v.tag != FFI_HANDLE || handle_v.ffi_val == null) {
        return raise_error(interp, "foreign-describe: expected ForeignHandle");
    }
    return foreign_runtime_handle_descriptor_payload(handle_v.ffi_val, interp);
}

fn Value* foreign_runtime_release_handle(Value* handle_v, Interp* interp) {
    if (handle_v == null || handle_v.tag != FFI_HANDLE || handle_v.ffi_val == null) {
        return raise_error(interp, "foreign-release: expected ForeignHandle");
    }
    FfiHandle* handle = handle_v.ffi_val;
    if (!ffi_handle_can_release_payload(handle)) {
        return raise_error(interp, "foreign-release: handle is not releasable");
    }
    if (handle.lib_handle == null) return make_void(interp);
    if (!ffi_handle_release_payload(handle)) {
        return raise_error(interp, "foreign-release: failed to release handle");
    }
    return make_void(interp);
}

fn ForeignRuntimeKind foreign_runtime_bound_kind_for_metadata(FfiBoundFn* bound) {
    if (bound == null || bound.runtime_kind == FOREIGN_RUNTIME_NATIVE) {
        return FOREIGN_RUNTIME_C_ABI;
    }
    return bound.runtime_kind;
}

fn ForeignRuntimeKind foreign_runtime_bound_kind_for_runtime_call(FfiBoundFn* bound) {
    return foreign_runtime_bound_kind_for_metadata(bound);
}

fn bool foreign_runtime_adapter_for_bound(FfiBoundFn* bound, ForeignRuntimeAdapter* out) {
    return foreign_runtime_adapter_for_kind(foreign_runtime_bound_kind_for_runtime_call(bound), out);
}

fn bool foreign_runtime_adapter_for_kind(ForeignRuntimeKind kind, ForeignRuntimeAdapter* out) {
    if (out == null) return false;
    *out = {};
    out.runtime_kind = kind;
    out.operation_capabilities = FOREIGN_CAP_REFLECT | FOREIGN_CAP_RELEASE;
    out.describe_handle = &foreign_runtime_describe_handle;
    out.release_handle = &foreign_runtime_release_handle;

    switch (kind) {
        case FOREIGN_RUNTIME_C_ABI:
            out.operation_capabilities = FOREIGN_CAP_LOAD | FOREIGN_CAP_RESOLVE |
                FOREIGN_CAP_CALL | FOREIGN_CAP_REFLECT | FOREIGN_CAP_RELEASE;
            out.describe_callable = &foreign_runtime_describe_bound_function;
            out.load_bound = &foreign_runtime_load_c_abi_bound;
            out.resolve_bound = &foreign_runtime_resolve_c_abi_bound;
            out.call_bound = &foreign_runtime_call_c_abi_bound;
            return true;
        case FOREIGN_RUNTIME_NATIVE:
        case FOREIGN_RUNTIME_PYTHON:
        case FOREIGN_RUNTIME_JULIA:
        case FOREIGN_RUNTIME_CUDA:
        case FOREIGN_RUNTIME_CPP:
        case FOREIGN_RUNTIME_POLYGLOT:
            return true;
        default:
            *out = {};
            return false;
    }
}

fn bool foreign_runtime_adapter_for_handle(Value* handle_v, ForeignRuntimeAdapter* out) {
    if (handle_v == null || handle_v.tag != FFI_HANDLE || handle_v.ffi_val == null) return false;
    return foreign_runtime_adapter_for_kind(handle_v.ffi_val.runtime_kind, out);
}

fn Value* foreign_runtime_import_module(Value* runtime_v, Value* module_descriptor, Interp* interp) {
    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_handle(runtime_v, &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_IMPORT) ||
        adapter.import_module == null) {
        return raise_error(interp, "foreign-import: runtime adapter does not support module import");
    }
    return adapter.import_module(runtime_v, module_descriptor, interp);
}

fn Value* foreign_runtime_resolve_member(Value* owner_v, Value* member_descriptor, Interp* interp) {
    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_handle(owner_v, &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_MEMBER) ||
        adapter.resolve_member == null) {
        return raise_error(interp, "foreign-resolve: runtime adapter does not support member lookup");
    }
    return adapter.resolve_member(owner_v, member_descriptor, interp);
}

fn Value* prim_foreign_describe(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return raise_error(interp, "foreign-describe: expected 1 argument");
    if (args[0] != null && args[0].tag == PRIMITIVE && args[0].prim_val != null &&
        args[0].prim_val.func == &prim_ffi_bound_call) {
        FfiBoundFn* bound = (FfiBoundFn*)args[0].prim_val.user_data;
        ForeignRuntimeAdapter adapter;
        if (!foreign_runtime_adapter_for_bound(bound, &adapter) ||
            !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_REFLECT) ||
            adapter.describe_callable == null) {
            return raise_error(interp, "foreign-describe: runtime adapter does not support callable reflection");
        }
        return adapter.describe_callable(args[0], interp);
    }

    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_handle(args[0], &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_REFLECT) ||
        adapter.describe_handle == null) {
        return raise_error(interp, "foreign-describe: runtime adapter does not support handle reflection");
    }
    return adapter.describe_handle(args[0], interp);
}

fn Value* prim_foreign_release(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return raise_error(interp, "foreign-release: expected 1 argument");
    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_handle(args[0], &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_RELEASE) ||
        adapter.release_handle == null) {
        return raise_error(interp, "foreign-release: runtime adapter does not support release");
    }
    return adapter.release_handle(args[0], interp);
}
