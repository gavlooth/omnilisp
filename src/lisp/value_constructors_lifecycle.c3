module lisp;

import std::core::mem;
import main;

// Scope destructor for Values with heap-allocated backing data.
// Mirrors destroy_value in eval.c3 but called from scope_region lifecycle.
fn void scope_dtor_value(void* ptr) {
    // Dtors re-enabled after fixing use-after-free (parser literals, module env)
    Value* v = (Value*)ptr;
    switch (v.tag) {
        case STRING:
        case ERROR:
            if (v.string_heap_owned && v.str_chars != null) {
                mem::free(v.str_chars);
                v.str_chars = null;
            }
            v.string_heap_owned = false;
        case CLOSURE:
            // Handled by scope_dtor_closure now
            break;
        case PRIMITIVE:
            if (v.prim_val != null) {
                mem::free(v.prim_val);
                v.prim_val = null;
            }
        case ARRAY:
            if (v.array_val != null) {
                if (v.array_val.items != null) mem::free(v.array_val.items);
                mem::free(v.array_val);
                v.array_val = null;
            }
        case TENSOR:
            if (v.tensor_val != null) {
                tensor_free_payload(v.tensor_val);
                v.tensor_val = null;
            }
        case INSTANCE:
            if (v.instance_val != null) {
                instance_release(v.instance_val);
                v.instance_val = null;
            }
        case METHOD_TABLE:
            if (v.method_table_val != null) {
                if (v.method_table_val.entries != null) {
                    for (usz i = 0; i < v.method_table_val.entry_count; i++) {
                        method_signature_destroy_heap(&v.method_table_val.entries[i].sig);
                    }
                    mem::free(v.method_table_val.entries);
                }
                mem::free(v.method_table_val);
                v.method_table_val = null;
            }
        case HASHMAP:
        case SET:
            if (v.hashmap_val != null) {
                if (v.hashmap_val.entries != null) mem::free(v.hashmap_val.entries);
                mem::free(v.hashmap_val);
                v.hashmap_val = null;
            }
        case FFI_HANDLE:
            if (v.ffi_val != null) {
                ffi_handle_release(v.ffi_val);
                v.ffi_val = null;
            }
        case COROUTINE:
            // Abandoned coroutines must return their StackCtx to the pool at scope teardown.
            if (v.coroutine_val != null) {
                if (v.coroutine_val.user_data != null) {
                    mem::free(v.coroutine_val.user_data);
                    v.coroutine_val.user_data = null;
                }
                main::StackPool* owner_pool = v.coroutine_val.owner_pool;
                if (owner_pool != null) {
                    main::stack_ctx_destroy(v.coroutine_val, owner_pool);
                }
                v.coroutine_val = null;
            }
        case CONTINUATION:
            if (v.continuation_boundary_owned && v.cont_val != null) {
                continuation_release_handle_state_for_boundary(v.cont_val);
                v.continuation_boundary_owned = false;
            }
        case VOID:
            break;
        // CONTINUATION lifetime remains managed by reset/shift/handle paths.
        default: {}
    }
}

// Scope destructor for Env objects — frees malloc'd bindings and hash table.
fn void scope_dtor_closure(void* ptr) {
    Value* v = (Value*)ptr;
    if (v.closure_val != null && v.closure_val.env_scope != null) {
        main::scope_release(v.closure_val.env_scope);
        v.closure_val.env_scope = null;
    }
}

fn bool closure_env_scope_is_root_owned(Value* v, Interp* interp) @inline {
    if (v == null || interp == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (v.closure_val.env_scope == null || interp.root_scope == null) return false;
    if (v.closure_val.env_scope != interp.root_scope || v.closure_val.env == null) return false;
    return boundary_ptr_in_scope((void*)v.closure_val.env, interp.root_scope);
}

fn void scope_dtor_env(void* ptr) {
    Env* env = (Env*)ptr;
    if (!env.is_inline && env.bindings != null) {
        mem::free(env.bindings);
        env.bindings = null;
    }
    if (env.hash_table != null) {
        mem::free(env.hash_table);
        env.hash_table = null;
    }
}
