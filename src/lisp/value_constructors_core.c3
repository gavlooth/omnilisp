module lisp;

import main;

bool g_value_force_make_string_alloc_fail = false;
int g_value_force_closure_scope_alloc_fail_nth = 0;

fn void* value_alloc_closure_payload(main::ScopeRegion* scope, usz size) @inline {
    if (scope == null) return null;
    if (g_value_force_closure_scope_alloc_fail_nth > 0) {
        g_value_force_closure_scope_alloc_fail_nth--;
        if (g_value_force_closure_scope_alloc_fail_nth == 0) return null;
    }
    return scope.alloc(size);
}

// =============================================================================
// SECTION 2: VALUE CONSTRUCTORS (Core Surface)
// =============================================================================

fn Value* make_string_alloc_error(Interp* interp) {
    Value* fail = interp.alloc_value();
    fail.tag = ERROR;
    char[] msg = "out of memory while allocating string";
    fail.str_chars = (char*)msg.ptr;
    fail.str_len = msg.len;
    fail.string_heap_owned = false;
    return fail;
}

<* @ensure return != null *>
fn Value* make_nil(Interp* interp) @inline {
    Value* v = interp.alloc_value();
    v.tag = NIL;
    return v;
}

<* @ensure return != null *>
fn Value* make_void(Interp* interp) @inline {
    Value* v = interp.alloc_value();
    v.tag = VOID;
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
fn Value* make_float32(Interp* interp, float f) @inline {
    Value* v = interp.alloc_value();
    v.tag = FLOAT32;
    v.float32_val = f;
    return v;
}

<* @ensure return != null *>
fn Value* make_complex128(Interp* interp, double real, double imag) @inline {
    Value* v = interp.alloc_value();
    v.tag = COMPLEX128;
    v.complex128_val.real = real;
    v.complex128_val.imag = imag;
    return v;
}

<* @ensure return != null *>
fn Value* make_complex64(Interp* interp, float real, float imag) @inline {
    Value* v = interp.alloc_value();
    v.tag = COMPLEX64;
    v.complex64_val.real = real;
    v.complex64_val.imag = imag;
    return v;
}

<* @ensure return != null *>
fn Value* make_string(Interp* interp, char[] str) {
    usz len = str.len;
    if (len == usz.max) return make_string_alloc_error(interp);
    usz cap = len + 1;
    if (cap < 32) cap = 32;  // minimum allocation
    char* chars = g_value_force_make_string_alloc_fail
        ? null
        : (char*)mem::malloc(cap);
    if (chars == null) {
        return make_string_alloc_error(interp);
    }

    Value* v = interp.alloc_value();
    v.tag = STRING;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    v.str_chars = chars;
    v.string_heap_owned = true;
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
    if (len == usz.max) {
        mem::free(owned_chars);
        return make_string_alloc_error(interp);
    }

    Value* v = interp.alloc_value();
    v.tag = STRING;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    v.str_chars = owned_chars;
    v.str_len = len;
    v.string_heap_owned = true;
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

fn bool make_cons_escape_identity_promotion_ok(Interp* interp, Value* original, Value* promoted) @inline {
    if (original == null || promoted == null || original != promoted) return true;
    if (original.scope_gen == interp.current_scope.escape_generation) return true;
    if (original.scope_gen != interp.current_scope.generation) {
        return boundary_ptr_in_target_scope_chain_with_hint((void*)original, interp, original.scope_gen);
    }
    return false;
}

fn bool make_cons_promoted_error_value_ok(Interp* interp, Value* original, Value* promoted) @inline {
    if (interp == null || original == null || promoted == null) return false;
    if (original.tag != ERROR || promoted.tag != ERROR) return false;
    if (promoted.scope_gen == interp.current_scope.escape_generation) return true;
    return promoted == original && make_cons_escape_identity_promotion_ok(interp, original, promoted);
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
        Value* promoted_car =
            car != null && car.scope_gen == interp.current_scope.escape_generation
            ? car
            : boundary_promote_to_escape(car, interp);
        if (car != null && car.scope_gen != interp.current_scope.escape_generation) {
            if (promoted_car == null) {
                return raise_error(interp, "cons: failed to promote car to escape");
            }
            if (promoted_car.tag == ERROR && !make_cons_promoted_error_value_ok(interp, car, promoted_car)) return promoted_car;
            if (!make_cons_escape_identity_promotion_ok(interp, car, promoted_car)) {
                return raise_error(interp, "cons: failed to promote car to escape");
            }
        }

        Value* promoted_cdr =
            cdr != null && cdr.scope_gen == interp.current_scope.escape_generation
            ? cdr
            : boundary_promote_to_escape(cdr, interp);
        if (cdr != null && cdr.scope_gen != interp.current_scope.escape_generation) {
            if (promoted_cdr == null) {
                boundary_cleanup_materialized_value(
                    car,
                    promoted_car,
                    interp.current_scope,
                    true
                );
                return raise_error(interp, "cons: failed to promote cdr to escape");
            }
            if (promoted_cdr.tag == ERROR && !make_cons_promoted_error_value_ok(interp, cdr, promoted_cdr)) {
                boundary_cleanup_materialized_value(
                    car,
                    promoted_car,
                    interp.current_scope,
                    true
                );
                return promoted_cdr;
            }
            if (!make_cons_escape_identity_promotion_ok(interp, cdr, promoted_cdr)) {
                boundary_cleanup_materialized_value(
                    car,
                    promoted_car,
                    interp.current_scope,
                    true
                );
                return raise_error(interp, "cons: failed to promote cdr to escape");
            }
        }

        Value* v = boundary_alloc_value_in_current_escape(interp);
        if (v == null) {
            boundary_cleanup_materialized_value(
                car,
                promoted_car,
                interp.current_scope,
                true
            );
            boundary_cleanup_materialized_value(
                cdr,
                promoted_cdr,
                interp.current_scope,
                true
            );
            return raise_error(interp, "cons: failed to allocate escape pair");
        }
        v.tag = CONS;
        // Reusing an existing current-scope ESCAPE tail is safe and avoids
        // rewalking the full accumulator spine on every cons in TCO loops.
        v.cons_val.car = promoted_car;
        v.cons_val.cdr = promoted_cdr;
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
    Closure* closure = (Closure*)value_alloc_closure_payload(interp.current_scope, Closure.sizeof);
    if (closure == null) {
        return raise_error(interp, "closure: out of memory");
    }
    SymbolId* params = (SymbolId*)value_alloc_closure_payload(interp.current_scope, SymbolId.sizeof * 1);
    if (params == null) {
        return raise_error(interp, "closure: out of memory");
    }
    params[0] = param;

    Value* v = interp.alloc_value();
    v.tag = CLOSURE;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_closure);
    v.closure_val = closure;
    v.closure_val.scope_gen = interp.current_scope.generation;
    v.closure_val.env_scope = null;
    v.closure_val.param = param;
    v.closure_val.params = params;
    v.closure_val.param_count = 1;
    v.closure_val.has_rest = false;
    v.closure_val.rest_param = (SymbolId)0;
    v.closure_val.body = body;
    v.closure_val.env = env;
    v.closure_val.has_param = true;
    v.closure_val.has_typed_params = false;
    v.closure_val.type_sig = null;
    return v;
}

fn Value* make_closure_no_param(Interp* interp, Expr* body, Env* env) {
    Closure* closure = (Closure*)value_alloc_closure_payload(interp.current_scope, Closure.sizeof);
    if (closure == null) {
        return raise_error(interp, "closure: out of memory");
    }

    Value* v = interp.alloc_value();
    v.tag = CLOSURE;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_closure);
    v.closure_val = closure;
    v.closure_val.scope_gen = interp.current_scope.generation;
    v.closure_val.env_scope = null;
    v.closure_val.param = (SymbolId)0;
    v.closure_val.params = null;
    v.closure_val.param_count = 0;
    v.closure_val.has_rest = false;
    v.closure_val.rest_param = (SymbolId)0;
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
