module lisp;

import main;

bool g_value_force_make_string_alloc_fail = false;

// =============================================================================
// SECTION 2: VALUE CONSTRUCTORS (Core Surface)
// =============================================================================

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
fn Value* make_string(Interp* interp, char[] str) {
    usz len = str.len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;  // minimum allocation
    char* chars = g_value_force_make_string_alloc_fail
        ? null
        : (char*)mem::malloc(cap);
    if (chars == null) {
        Value* fail = interp.alloc_value();
        fail.tag = ERROR;
        char[] msg = "out of memory while allocating string";
        fail.str_chars = (char*)msg.ptr;
        fail.str_len = msg.len;
        fail.string_heap_owned = false;
        return fail;
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
        // Reusing an existing current-scope ESCAPE tail is safe and avoids
        // rewalking the full accumulator spine on every cons in TCO loops.
        v.cons_val.car =
            car != null && car.scope_gen == interp.current_scope.escape_generation
            ? car
            : boundary_promote_to_escape(car, interp);
        v.cons_val.cdr =
            cdr != null && cdr.scope_gen == interp.current_scope.escape_generation
            ? cdr
            : boundary_promote_to_escape(cdr, interp);
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
