module lisp;

import main;

// =============================================================================
// SECTION 2: VALUE CONSTRUCTORS (Core + Destructors)
// =============================================================================

// Global stack context pool pointer for scope_dtor_value — set by Interp.init()
main::StackPool* g_stack_ctx_pool = null;

// Scope destructor for Values with heap-allocated backing data.
// Mirrors destroy_value in eval.c3 but called from scope_region lifecycle.
fn void scope_dtor_value(void* ptr) {
    // Dtors re-enabled after fixing use-after-free (parser literals, module env)
    Value* v = (Value*)ptr;
    switch (v.tag) {
        case STRING:
        case ERROR:
            if (v.str_chars != null) {
                mem::free(v.str_chars);
                v.str_chars = null;
            }
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
            if (v.coroutine_val != null && g_stack_ctx_pool != null) {
                if (v.coroutine_val.user_data != null) {
                    mem::free(v.coroutine_val.user_data);
                    v.coroutine_val.user_data = null;
                }
                main::stack_ctx_destroy(v.coroutine_val, g_stack_ctx_pool);
                v.coroutine_val = null;
            }
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

<* @ensure return != null *>
fn Value* make_nil(Interp* interp) @inline {
    Value* v = interp.alloc_value();
    v.tag = NIL;
    return v;
}

<* @ensure return != null *>
fn Value* make_int(Interp* interp, long n) @inline {
    Value* v = interp.alloc_value();
    v.tag = INT;
    v.int_val = n;
    return v;
}

<* @ensure return != null *>
fn Value* make_double(Interp* interp, double d) @inline {
    Value* v = interp.alloc_value();
    v.tag = DOUBLE;
    v.double_val = d;
    return v;
}

<* @ensure return != null *>
fn Value* make_string(Interp* interp, char[] str) {
    Value* v = interp.alloc_value();
    v.tag = STRING;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    usz len = str.len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;  // minimum allocation
    v.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        v.str_chars[i] = str[i];
    }
    v.str_chars[len] = 0;
    v.str_len = len;

    return v;
}

<* @ensure return != null *>
fn Value* make_string_owned(Interp* interp, char* owned_chars, usz len) {
    if (owned_chars == null) return make_string(interp, "");

    Value* v = interp.alloc_value();
    v.tag = STRING;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    v.str_chars = owned_chars;
    v.str_len = len;
    // owned_chars must have capacity >= len + 1.
    v.str_chars[len] = 0;
    return v;
}

<* @ensure return != null *>
fn Value* make_symbol(Interp* interp, SymbolId sym) @inline {
    Value* v = interp.alloc_value();
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

<* @ensure return != null *>
fn Value* make_cons(Interp* interp, Value* car, Value* cdr) @inline {
    // Dual-lane path: if cdr is already in current scope's ESCAPE lane,
    // allocate pair in ESCAPE lane and promote inputs before linking.
    // In escape-env mode, treat NIL cdr as accumulator bootstrap and seed
    // ESCAPE lane directly.
    if (cdr != null &&
        (cdr.scope_gen == interp.current_scope.escape_generation ||
         (interp.escape_env_mode && cdr.tag == NIL))) {
        Value* v = interp.alloc_value_escape();
        v.tag = CONS;
        v.cons_val.car = boundary_promote_to_escape(car, interp);
        v.cons_val.cdr = boundary_promote_to_escape(cdr, interp);
        return v;
    }

    // Normal path: allocate in current_scope
    Value* v = interp.alloc_value();
    v.tag = CONS;
    v.cons_val.car = car;
    v.cons_val.cdr = cdr;
    return v;
}

fn Value* make_closure(Interp* interp, SymbolId param, Expr* body, Env* env) {
    Value* v = interp.alloc_value();
    v.tag = CLOSURE;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_closure);
    v.closure_val = (Closure*)interp.current_scope.alloc(Closure.sizeof);
    v.closure_val.scope_gen = interp.current_scope.generation;
    v.closure_val.env_scope = null;
    v.closure_val.param = param;
    v.closure_val.params = (SymbolId*)interp.current_scope.alloc(SymbolId.sizeof * 1);
    v.closure_val.params[0] = param;
    v.closure_val.param_count = 1;
    v.closure_val.has_rest = false;
    v.closure_val.rest_param = 0;
    v.closure_val.body = body;
    v.closure_val.env = env;
    v.closure_val.has_param = true;
    v.closure_val.has_typed_params = false;
    v.closure_val.type_sig = null;
    return v;
}

fn Value* make_closure_no_param(Interp* interp, Expr* body, Env* env) {
    Value* v = interp.alloc_value();
    v.tag = CLOSURE;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_closure);
    v.closure_val = (Closure*)interp.current_scope.alloc(Closure.sizeof);
    v.closure_val.scope_gen = interp.current_scope.generation;
    v.closure_val.env_scope = null;
    v.closure_val.param = 0;
    v.closure_val.params = null;
    v.closure_val.param_count = 0;
    v.closure_val.has_rest = false;
    v.closure_val.rest_param = 0;
    v.closure_val.body = body;
    v.closure_val.env = env;
    v.closure_val.has_param = false;
    v.closure_val.has_typed_params = false;
    v.closure_val.type_sig = null;

    return v;
}

fn Value* make_continuation(Interp* interp, Continuation* k) @inline {
    Value* v = interp.alloc_value();
    v.tag = CONTINUATION;
    // No dtor registered — lifecycle managed by effect handlers (reset/shift/handle).
    // Scope dtor would fire too early (before handler is done with the context).
    v.cont_val = k;
    return v;
}

<* @ensure return != null *>
fn Value* make_time_point(
    Interp* interp,
    TimePointKind kind,
    int year,
    int month,
    int day,
    int hour,
    int minute,
    int second,
    int microsecond,
    int tz_offset
) {
    Value* v = interp.alloc_value();
    v.tag = TIME_POINT;
    v.time_point_val.year = year;
    v.time_point_val.month = month;
    v.time_point_val.day = day;
    v.time_point_val.hour = hour;
    v.time_point_val.minute = minute;
    v.time_point_val.second = second;
    v.time_point_val.microsecond = microsecond;
    v.time_point_val.tz_offset = tz_offset;
    v.time_point_val.kind = kind;
    return v;
}
