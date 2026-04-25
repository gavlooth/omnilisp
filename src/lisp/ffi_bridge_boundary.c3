module lisp;

enum FfiBridgeBoundaryMode : char {
    FFI_BRIDGE_BOUNDARY_OPAQUE,
    FFI_BRIDGE_BOUNDARY_KEEPALIVE,
    FFI_BRIDGE_BOUNDARY_COPY_HOOK,
    FFI_BRIDGE_BOUNDARY_TRACE_HOOK,
    FFI_BRIDGE_BOUNDARY_UNSAFE,
}

struct FfiBridgeBoundaryDeclaration {
    FfiBridgeBoundaryMode mode;
    bool opaque;
    bool keepalive;
    bool copy_hook;
    bool trace_hook;
    bool unsafe;
}

fn FfiBridgeBoundaryDeclaration ffi_bridge_boundary_declaration_for_mode(FfiBridgeBoundaryMode mode) @inline {
    return {
        .mode = mode,
        .opaque = mode == FFI_BRIDGE_BOUNDARY_OPAQUE,
        .keepalive = mode == FFI_BRIDGE_BOUNDARY_KEEPALIVE,
        .copy_hook = mode == FFI_BRIDGE_BOUNDARY_COPY_HOOK,
        .trace_hook = mode == FFI_BRIDGE_BOUNDARY_TRACE_HOOK,
        .unsafe = mode == FFI_BRIDGE_BOUNDARY_UNSAFE
    };
}

fn FfiBridgeBoundaryDeclaration ffi_bridge_boundary_declaration_for_handle(FfiHandle* handle) @inline {
    if (handle == null) {
        return ffi_bridge_boundary_declaration_for_mode(FFI_BRIDGE_BOUNDARY_UNSAFE);
    }
    return ffi_bridge_boundary_declaration_for_mode(handle.boundary_mode);
}

fn FfiBridgeBoundaryDeclaration ffi_bridge_boundary_declaration_for_value(Value* value) @inline {
    if (value == null || value.tag != FFI_HANDLE) {
        return ffi_bridge_boundary_declaration_for_mode(FFI_BRIDGE_BOUNDARY_UNSAFE);
    }
    return ffi_bridge_boundary_declaration_for_handle(value.ffi_val);
}

fn bool ffi_bridge_boundary_value_is_opaque(Value* value) @inline {
    return ffi_bridge_boundary_declaration_for_value(value).opaque;
}

fn bool ffi_bridge_boundary_value_requires_native_traversal(Value* value) @inline {
    FfiBridgeBoundaryDeclaration declaration = ffi_bridge_boundary_declaration_for_value(value);
    return declaration.copy_hook || declaration.trace_hook;
}

fn bool ffi_bridge_boundary_value_can_share_across_boundary(Value* value) @inline {
    FfiBridgeBoundaryDeclaration declaration = ffi_bridge_boundary_declaration_for_value(value);
    return declaration.opaque || declaration.keepalive;
}

fn bool ffi_bridge_boundary_value_is_transplant_safe(Value* value) @inline {
    return ffi_bridge_boundary_value_can_share_across_boundary(value);
}
