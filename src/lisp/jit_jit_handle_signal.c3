module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// STACK-CONTEXT-BASED HANDLE/SIGNAL/RESOLVE
// =============================================================================

/**
 * Shared state between jit_handle_impl (parent) and jit_signal_impl (body context).
 * Heap-allocated per handle activation and retained while escaped continuations exist.
 */
struct HandleEffectState {
    Expr*          body;
    Env*           env;
    Interp*        interp;
    bool           signaled;       // Set true by signal before suspending
    SymbolId       signal_tag;     // Which effect tag was signaled
    Value*         signal_arg;     // Argument passed to signal
    EffectHandler  handler_copy;   // Copy of handler for reinstallation by resolve
    usz            handler_idx;    // Handler stack index
    bool           is_aot;
    Value*         aot_body;
    uint           continuation_refcount; // Active continuations retaining this state
    bool           frame_active;          // True while enclosing handle frame is live
    HandleEffectState* live_next;         // Intrusive link for live-state registry
}

HandleEffectState* g_handle_effect_live_head = null;

fn bool handle_effect_state_is_live(HandleEffectState* state) {
    if (state == null) return false;
    HandleEffectState* it = g_handle_effect_live_head;
    while (it != null) {
        if (it == state) return true;
        it = it.live_next;
    }
    return false;
}

fn void handle_effect_state_register_live(HandleEffectState* state) {
    if (state == null) return;
    state.live_next = g_handle_effect_live_head;
    g_handle_effect_live_head = state;
}

fn void handle_effect_state_unregister_live(HandleEffectState* state) {
    if (state == null) return;
    HandleEffectState* prev = null;
    HandleEffectState* it = g_handle_effect_live_head;
    while (it != null) {
        if (it == state) {
            if (prev == null) {
                g_handle_effect_live_head = it.live_next;
            } else {
                prev.live_next = it.live_next;
            }
            state.live_next = null;
            return;
        }
        prev = it;
        it = it.live_next;
    }
}

fn void handle_effect_state_reset_handler_copy(HandleEffectState* state) @inline {
    state.handler_copy.tags = null;
    state.handler_copy.clauses = null;
    state.handler_copy.closures = null;
    state.handler_copy.clause_count = 0;
    state.handler_copy.handler_env = null;
    state.handler_copy.body_expr = null;
    state.handler_copy.body_env = null;
    state.handler_copy.strict_mode = false;
    state.handler_copy.effect_state = state;
}

fn void handle_effect_state_init(HandleEffectState* state, Interp* interp) {
    state.body = null;
    state.env = null;
    state.interp = interp;
    state.signaled = false;
    state.signal_tag = (SymbolId)0;
    state.signal_arg = null;
    state.handler_idx = 0;
    state.is_aot = false;
    state.aot_body = null;
    state.continuation_refcount = 0;
    state.frame_active = true;
    state.live_next = null;
    handle_effect_state_reset_handler_copy(state);
    handle_effect_state_register_live(state);
}

fn void handle_effect_state_scrub_continuations(HandleEffectState* state) {
    if (state == null || state.interp == null) return;
    Continuation* k = state.interp.continuation_head;
    while (k != null) {
        if ((HandleEffectState*)k.handle_state == state) {
            // State is being released: invalidate continuation frames that still
            // point at this handle state so later resolve paths fail closed.
            k.ctx = null;
            k.handle_state = null;
            k.handle_retained = false;
        }
        k = k.next_owned;
    }
}

fn void handle_effect_state_release(HandleEffectState* state) {
    if (state == null) return;
    if (!handle_effect_state_is_live(state)) return;
    handle_effect_state_unregister_live(state);
    handle_effect_state_scrub_continuations(state);
    if (state.handler_copy.tags != null) {
        mem::free(state.handler_copy.tags);
        state.handler_copy.tags = null;
    }
    if (state.handler_copy.clauses != null) {
        mem::free(state.handler_copy.clauses);
        state.handler_copy.clauses = null;
    }
    if (state.handler_copy.closures != null) {
        mem::free(state.handler_copy.closures);
        state.handler_copy.closures = null;
    }
    state.interp = null;
    mem::free(state);
}

fn bool handle_effect_state_capture_handler_copy(HandleEffectState* state, EffectHandler* src) {
    assert(state != null, "handle_effect_state_capture_handler_copy: state must not be null");
    assert(src != null, "handle_effect_state_capture_handler_copy: src must not be null");

    handle_effect_state_reset_handler_copy(state);
    state.handler_copy.effect_state = state;
    state.handler_copy.clause_count = src.clause_count;
    state.handler_copy.handler_env = src.handler_env;
    state.handler_copy.body_expr = src.body_expr;
    state.handler_copy.body_env = src.body_env;
    state.handler_copy.strict_mode = src.strict_mode;

    usz count = src.clause_count;
    if (count == 0) {
        return true;
    }

    SymbolId* copied_tags = null;
    EffectClause* copied_clauses = null;
    Value** copied_closures = null;

    if (src.tags != null) {
        copied_tags = (SymbolId*)mem::malloc(SymbolId.sizeof * count);
        if (copied_tags == null) return false;
        mem::copy(copied_tags, src.tags, SymbolId.sizeof * count);
    }

    if (src.clauses != null) {
        copied_clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * count);
        if (copied_clauses == null) {
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        mem::copy(copied_clauses, src.clauses, EffectClause.sizeof * count);
    }

    if (src.closures != null) {
        copied_closures = (Value**)mem::malloc(Value*.sizeof * count);
        if (copied_closures == null) {
            if (copied_clauses != null) mem::free(copied_clauses);
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        for (usz i = 0; i < count; i++) {
            Value* closure = src.closures[i];
            if (closure != null && state.interp != null) {
                Value* promoted = boundary_promote_to_root(closure, state.interp);
                if (promoted == null || promoted.tag == ERROR || promoted.tag != CLOSURE) {
                    mem::free(copied_closures);
                    if (copied_clauses != null) mem::free(copied_clauses);
                    if (copied_tags != null) mem::free(copied_tags);
                    return false;
                }
                closure = promoted;
            }
            copied_closures[i] = closure;
        }
    }

    state.handler_copy.tags = copied_tags;
    state.handler_copy.clauses = copied_clauses;
    state.handler_copy.closures = copied_closures;
    return true;
}

fn void continuation_retain_handle_state_for_boundary(Continuation* k) {
    if (k == null || k.handle_state == null || k.handle_retained) return;
    HandleEffectState* hstate = (HandleEffectState*)k.handle_state;
    hstate.continuation_refcount++;
    k.handle_retained = true;
}

const usz CONTINUATION_SCAN_MAX_VALUES = 1024;
const usz CONTINUATION_SCAN_MAX_ENVS = 256;

struct ContinuationEscapeScan {
    Continuation* target;
    Value*[CONTINUATION_SCAN_MAX_VALUES] value_stack;
    usz value_stack_len;
    Value*[CONTINUATION_SCAN_MAX_VALUES] value_seen;
    usz value_seen_len;
    Env*[CONTINUATION_SCAN_MAX_ENVS] env_stack;
    usz env_stack_len;
    Env*[CONTINUATION_SCAN_MAX_ENVS] env_seen;
    usz env_seen_len;
}

fn bool continuation_scan_push_value(ContinuationEscapeScan* scan, Value* v) {
    if (scan == null || v == null) return false;
    for (usz i = 0; i < scan.value_seen_len; i++) {
        if (scan.value_seen[i] == v) return false;
    }
    if (scan.value_seen_len >= CONTINUATION_SCAN_MAX_VALUES) return true;
    scan.value_seen[scan.value_seen_len] = v;
    scan.value_seen_len++;
    if (scan.value_stack_len >= CONTINUATION_SCAN_MAX_VALUES) return true;
    scan.value_stack[scan.value_stack_len] = v;
    scan.value_stack_len++;
    return false;
}

fn bool continuation_scan_push_env(ContinuationEscapeScan* scan, Env* env) {
    if (scan == null || env == null) return false;
    for (usz i = 0; i < scan.env_seen_len; i++) {
        if (scan.env_seen[i] == env) return false;
    }
    if (scan.env_seen_len >= CONTINUATION_SCAN_MAX_ENVS) return true;
    scan.env_seen[scan.env_seen_len] = env;
    scan.env_seen_len++;
    if (scan.env_stack_len >= CONTINUATION_SCAN_MAX_ENVS) return true;
    scan.env_stack[scan.env_stack_len] = env;
    scan.env_stack_len++;
    return false;
}

fn bool continuation_result_references_target(Value* root, Continuation* target) {
    if (root == null || target == null) return false;

    ContinuationEscapeScan scan = {};
    scan.target = target;
    if (continuation_scan_push_value(&scan, root)) return true;

    while (scan.value_stack_len > 0 || scan.env_stack_len > 0) {
        while (scan.value_stack_len > 0) {
            scan.value_stack_len--;
            Value* v = scan.value_stack[scan.value_stack_len];
            if (v == null) continue;
            if (v.tag == CONTINUATION && v.cont_val == scan.target) {
                return true;
            }

            switch (v.tag) {
                case CONS:
                    if (continuation_scan_push_value(&scan, v.cons_val.car)) return true;
                    if (continuation_scan_push_value(&scan, v.cons_val.cdr)) return true;
                case PARTIAL_PRIM:
                    if (continuation_scan_push_value(&scan, v.partial_val.first_arg)) return true;
                    if (continuation_scan_push_value(&scan, v.partial_val.second_arg)) return true;
                case ARRAY:
                    if (v.array_val != null && v.array_val.items != null) {
                        for (usz i = 0; i < v.array_val.length; i++) {
                            if (continuation_scan_push_value(&scan, v.array_val.items[i])) return true;
                        }
                    }
                case HASHMAP:
                case SET:
                    if (v.hashmap_val != null && v.hashmap_val.entries != null) {
                        for (usz i = 0; i < v.hashmap_val.capacity; i++) {
                            HashEntry* entry = &v.hashmap_val.entries[i];
                            if (entry.key != null) {
                                if (continuation_scan_push_value(&scan, entry.key)) return true;
                                if (continuation_scan_push_value(&scan, entry.value)) return true;
                            }
                        }
                    }
                case METHOD_TABLE:
                    if (v.method_table_val != null) {
                        if (continuation_scan_push_value(&scan, v.method_table_val.fallback)) return true;
                        if (v.method_table_val.entries != null) {
                            for (usz i = 0; i < v.method_table_val.entry_count; i++) {
                                if (continuation_scan_push_value(&scan, v.method_table_val.entries[i].implementation)) return true;
                            }
                        }
                    }
                case CLOSURE:
                    if (v.closure_val != null) {
                        if (continuation_scan_push_env(&scan, v.closure_val.env)) return true;
                    }
                case MODULE:
                    if (v.module_val != null) {
                        if (continuation_scan_push_env(&scan, v.module_val.env)) return true;
                    }
                case ITERATOR:
                    if (continuation_scan_push_value(&scan, v.iterator_val)) return true;
                case INSTANCE:
                    if (v.instance_val != null && v.instance_val.fields != null) {
                        for (usz i = 0; i < v.instance_val.field_count; i++) {
                            Value* field_ptr = &v.instance_val.fields[i];
                            if (continuation_scan_push_value(&scan, field_ptr)) return true;
                        }
                    }
                default:
                    break;
            }
        }

        while (scan.env_stack_len > 0) {
            scan.env_stack_len--;
            Env* env = scan.env_stack[scan.env_stack_len];
            if (env == null) continue;
            if (continuation_scan_push_env(&scan, env.parent)) return true;
            for (usz i = 0; i < env.binding_count; i++) {
                if (continuation_scan_push_value(&scan, env.bindings[i].value)) return true;
            }
        }
    }

    return false;
}

fn void continuation_retain_if_referenced_by_result(Value* result, Continuation* k) {
    if (k == null || result == null) return;
    if (continuation_result_references_target(result, k)) {
        continuation_retain_handle_state_for_boundary(k);
    }
}

fn Value* runtime_effect_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

fn Value* eval_result_or_runtime_raise(EvalResult r, Interp* interp, char[] code) @inline {
    if (r.error.has_error) return runtime_effect_raise(interp, code, r.error.message[:256]);
    return r.value;
}

fn void handle_effect_entry(void* arg) {
    HandleEffectState* state = (HandleEffectState*)arg;
    Value* v;
    if (state.is_aot) {
        v = jit_apply_value(state.aot_body, make_nil(state.interp), state.interp);
    } else {
        v = jit_eval(state.body, state.env, state.interp);
    }
    main::g_current_stack_ctx.result = v;
}

fn Value* jit_unhandled_effect_error(SymbolId tag, Value* arg_v, Interp* interp) {
    char[256] ubuf;
    char[] utag = interp.symbols.get_name(tag);
    char[] uarg = interp.symbols.get_name(value_type_name(arg_v, interp));
    char[] umsg = io::bprintf(&ubuf, "unhandled effect '%s' with arg type %s", (ZString)utag, (ZString)uarg)!!;

    Value* data = make_hashmap(interp, 8);
    if (data != null && data.tag == HASHMAP) {
        SymbolId key_tag = interp.symbols.intern("tag");
        SymbolId key_arg_type = interp.symbols.intern("arg-type");
        hashmap_set(data.hashmap_val, make_symbol(interp, key_tag), make_symbol(interp, tag), interp);
        hashmap_set(data.hashmap_val, make_symbol(interp, key_arg_type), make_symbol(interp, value_type_name(arg_v, interp)), interp);
    }
    return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", umsg, data);
}

fn bool effect_handler_has_tag(EffectHandler* h, SymbolId tag) {
    for (usz j = 0; j < h.clause_count; j++) {
        SymbolId ctag = h.clauses != null ? h.clauses[j].effect_tag : h.tags[j];
        if (ctag == tag) return true;
    }
    return false;
}

fn Value* jit_signal_type_check(SymbolId tag, Value* arg_v, Interp* interp) {
    TypeId effect_tid = interp.types.lookup(tag, &interp.symbols);
    if (effect_tid == INVALID_TYPE_ID) return null;

    TypeInfo* effect_info = interp.types.get(effect_tid);
    if (effect_info == null || effect_info.kind != TK_EFFECT || effect_info.field_count == 0) {
        return null;
    }

    TypeId expected_tid = effect_info.fields[0].field_type;
    if (expected_tid == INVALID_TYPE_ID || expected_tid == interp.tid_Any) return null;

    TypeId actual_tid = infer_value_type(arg_v, interp);
    if (actual_tid != INVALID_TYPE_ID && !interp.types.is_subtype(actual_tid, expected_tid)) {
        return runtime_effect_raise(interp, "runtime/effect-arg-mismatch", "perform: argument type mismatch");
    }
    return null;
}

fn Value* jit_signal_suspend(EffectHandler* h, SymbolId tag, Value* arg_v, Interp* interp, main::StackCtx* current) {
    if (h.effect_state == null) return null;

    HandleEffectState* hstate = (HandleEffectState*)h.effect_state;
    hstate.signaled = true;
    hstate.signal_tag = tag;
    hstate.signal_arg = arg_v;

    SavedInterpState saved_sig;
    save_interp_state(interp, &saved_sig);
    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        restore_interp_state_preserve_raise(interp, &saved_sig);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/suspend-scope-guard",
            "stack engine: failed to register suspend scope guard"
        );
    }
    restore_interp_state_preserve_raise(interp, &saved_sig);
    return interp_consume_resume_value_or_nil(interp);
}

fn Value* jit_signal_try_fast_path(SymbolId tag, Value* arg_v, Interp* interp) {
    for (usz i = 0; i < interp.fast_path_count; i++) {
        if (interp.fast_path_table[i].tag != tag) continue;

        Value* fp_prim = interp.fast_path_table[i].prim;
        int fp_arity = fp_prim.prim_val.arity;
        if (fp_arity == 0) {
            EvalResult r = apply_primitive(fp_prim, make_nil(interp), interp);
            return eval_result_or_runtime_raise(r, interp, "runtime/fast-path-primitive-failed");
        }
        if (fp_arity == 1 || fp_arity == -1) {
            EvalResult r = apply_primitive(fp_prim, arg_v, interp);
            return eval_result_or_runtime_raise(r, interp, "runtime/fast-path-primitive-failed");
        }
        if (arg_v != null && is_cons(arg_v)) {
            EvalResult r = apply_primitive(fp_prim, car(arg_v), interp);
            if (r.error.has_error) return runtime_effect_raise(interp, "runtime/fast-path-primitive-failed", r.error.message[:256]);
            Value* result = jit_apply_value(r.value, cdr(arg_v), interp);
            if (result != null && result.tag == ERROR) return result;
            return result;
        }
        return null;
    }
    return null;
}

/**
 * Stack-context-based signal: suspends the current context when a matching handler is found.
 *
 * Returns Value* directly (same pattern as jit_shift_impl — avoids the hidden
 * return pointer issue with EvalResult after stack_ctx_clone).
 *
 * Falls through to the I/O fast path or error if no handler matches.
 */
fn Value* jit_signal_impl(Expr* expr, Env* env, Interp* interp) {
    Value* arg_v = jit_eval(expr.perform.arg, env, interp);
    if (arg_v != null && arg_v.tag == ERROR) {
        return arg_v;
    }

    SymbolId tag = expr.perform.tag;
    Value* type_err = jit_signal_type_check(tag, arg_v, interp);
    if (type_err != null) return type_err;

    main::StackCtx* current = main::g_current_stack_ctx;
    if (current != null) {
        bool hit_strict = false;
        for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
            EffectHandler* h = &interp.handler_stack[(usz)i];
            if (effect_handler_has_tag(h, tag)) {
                Value* resumed = jit_signal_suspend(h, tag, arg_v, interp, current);
                if (resumed != null) return resumed;
            }
            if (h.strict_mode) {
                hit_strict = true;
                break;
            }
        }

        if (hit_strict) {
            return raise_error_with_payload_names(
                interp,
                "runtime",
                "runtime/strict-unhandled-effect",
                "strict handler: unhandled effect"
            );
        }
    }

    Value* fast = jit_signal_try_fast_path(tag, arg_v, interp);
    if (fast != null) return fast;
    return jit_unhandled_effect_error(tag, arg_v, interp);
}

/**
 * Dispatch a single signal to a handler clause: find the matching clause, create
 * a continuation, bind k and arg, evaluate the clause body.
 *
 * Returns the handler clause result (Value*). On unmatched clause, returns an
 * ERROR-tagged value. Caller checks ctx.status and hstate.signaled to determine
 * whether to loop, return body result, or abort.
 */
fn Value* jit_handle_dispatch_signal(HandleEffectState* hstate, Expr* expr, Env* env, Interp* interp, main::StackCtx* ctx) {
    hstate.signaled = false;

    // Pop handler for clause evaluation (clause may push its own)
    interp.handler_count = hstate.handler_idx;

    // Find matching clause
    EffectClause* clause = null;
    for (usz j = 0; j < hstate.handler_copy.clause_count; j++) {
        if ((uint)hstate.handler_copy.tags[j] == (uint)hstate.signal_tag) {
            clause = &hstate.handler_copy.clauses[j];
            break;
        }
    }

    if (clause == null) {
        // No matching clause — return error for caller to handle
        char[256] ubuf;
        char[] utag = interp.symbols.get_name(hstate.signal_tag);
        char[] uarg = interp.symbols.get_name(value_type_name(hstate.signal_arg, interp));
        char[] umsg = io::bprintf(&ubuf, "unhandled effect '%s' with arg type %s", (ZString)utag, (ZString)uarg)!!;
        return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", umsg);
    }

    // Create continuation wrapping the suspended context
    Continuation* k = interp.alloc_lisp_continuation();
    k.ctx = ctx;
    k.is_coroutine_based = true;
    k.handle_state = hstate;
    Value* k_val = make_continuation(interp, k);

    // Bind k and arg in clause environment
    Env* clause_env = env.extend(interp, clause.k_name, k_val);
    clause_env = clause_env.extend(interp, clause.arg_name, hstate.signal_arg);
    // Also bind __k for resolve
    clause_env = clause_env.extend(interp, interp.sym__k, k_val);

    // Evaluate handler clause
    Value* handler_v = jit_eval(clause.handler_body, clause_env, interp);
    // Retain before frame cleanup when the continuation escapes in the clause result graph.
    continuation_retain_if_referenced_by_result(handler_v, k);
    return handler_v;
}

fn EvalResult value_to_eval_result(Value* v) @inline {
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_chars[:v.str_len]);
    }
    return eval_ok(v);
}

fn Value* dispatch_pending_raise(Expr* expr, Env* env, Interp* interp) {
    if (!interp.flags.raise_pending) return null;
    SymbolId raise_sym = interp.sym_raise;
    for (usz ci = 0; ci < expr.handle.clause_count; ci++) {
        if ((uint)expr.handle.clauses[ci].effect_tag == (uint)raise_sym) {
            EffectClause* clause = &expr.handle.clauses[ci];
            interp.flags.raise_pending = false;
            Value* msg_val = interp.raise_payload;
            if (msg_val == null) {
                msg_val = make_string(interp, interp.raise_msg[:interp.raise_msg_len]);
            }
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.raise_msg[0] = 0;
            Value* k_val = make_nil(interp);
            Env* clause_env = env.extend(interp, clause.k_name, k_val);
            clause_env = clause_env.extend(interp, clause.arg_name, msg_val);
            Value* handler_v = jit_eval(clause.handler_body, clause_env, interp);
            if (handler_v == null) {
                return runtime_effect_raise(interp, "runtime/raise-handler-returned-nil", "raise handler returned nil");
            }
            return handler_v;
        }
    }
    return null;
}

/**
 * Stack-context-based handle: runs body on a context, dispatches signals to handler clauses.
 *
 * Signal suspends the context. The handle loop finds the matching clause, creates
 * a continuation k, and evaluates the clause in the parent context. Resolve
 * resumes the body context (single-shot).
 */
fn void jit_handle_warm_clauses(Expr* expr, Interp* interp) {
    jit_warm_expr_cache(expr.handle.body, interp);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        jit_warm_expr_cache(expr.handle.clauses[i].handler_body, interp);
    }
}

fn void jit_handle_init_state(HandleEffectState* state, Expr* expr, Env* env, Interp* interp) {
    handle_effect_state_init(state, interp);
    state.body = expr.handle.body;
    state.env = env;
}

fn void jit_handle_init_handler(EffectHandler* h, Expr* expr, Env* env, HandleEffectState* state) {
    h.clause_count = expr.handle.clause_count;
    h.handler_env = env;
    h.body_expr = expr.handle.body;
    h.body_env = env;
    h.strict_mode = expr.handle.strict_mode;
    h.effect_state = state;
}

fn bool jit_handle_copy_clauses(EffectHandler* h, Expr* expr) {
    h.tags = (SymbolId*)mem::malloc(SymbolId.sizeof * expr.handle.clause_count);
    h.clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * expr.handle.clause_count);
    h.closures = null;
    if (h.tags == null || h.clauses == null) {
        if (h.tags != null) mem::free(h.tags);
        if (h.clauses != null) mem::free(h.clauses);
        return false;
    }
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        h.tags[i] = expr.handle.clauses[i].effect_tag;
        h.clauses[i] = expr.handle.clauses[i];
    }
    return true;
}

fn bool jit_handle_body_needs_fiber_yield() @inline {
    if (!g_scheduler.running) return false;
    usz fid = g_scheduler.current_fiber_id;
    if (fid >= g_scheduler.fiber_count) return false;
    return g_scheduler.fibers[fid].state == FIBER_BLOCKED;
}

fn EvalResult jit_handle_switch_to_body(main::StackCtx* ctx, Interp* interp) {
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);
    restore_interp_state_preserve_raise(interp, &saved);

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return eval_error("stack overflow in handle body");
    }

    // Async I/O yield propagation: handle body suspended due to async I/O inside a fiber.
    // Yield the outer context (fiber or enclosing handle) until I/O completes.
    while (ctx.status == main::StackCtxStatus.CTX_SUSPENDED &&
           jit_handle_body_needs_fiber_yield()) {
        main::StackCtx* outer_ctx = main::g_current_stack_ctx;
        if (outer_ctx == null) break;
        if (!prim_yield_suspend_and_restore(outer_ctx, interp)) {
            return eval_error("handle: async yield propagation failed");
        }
        // Outer context resumed (I/O complete) — resume handle body.
        SavedInterpState saved2;
        save_interp_state(interp, &saved2);
        main::stack_ctx_resume(ctx, &parent_ctx);
        restore_interp_state_preserve_raise(interp, &saved2);
        if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
            return eval_error("stack overflow in handle body");
        }
    }

    return eval_ok(null);
}

fn bool jit_handle_dispatch_signals(
    HandleEffectState* state,
    Expr* expr,
    Env* env,
    Interp* interp,
    main::StackCtx* ctx,
    EvalResult* out_result
) {
    while (state.signaled) {
        Value* handler_v = jit_handle_dispatch_signal(state, expr, env, interp, ctx);

        // resolve was called, body completed
        if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
            *out_result = value_to_eval_result((Value*)ctx.result);
            return true;
        }

        // Body hit another signal after being resumed — loop to dispatch.
        if (state.signaled) {
            continue;
        }

        // Abort or unmatched clause: context still suspended, return handler result.
        *out_result = value_to_eval_result(handler_v);
        return true;
    }
    return false;
}

fn EvalResult jit_handle_finish_no_signal(Expr* expr, Env* env, Interp* interp, main::StackCtx* ctx) {
    Value* raise_v = dispatch_pending_raise(expr, env, interp);
    if (raise_v != null) return value_to_eval_result(raise_v);
    return value_to_eval_result((Value*)ctx.result);
}

fn EvalResult jit_handle_impl(Expr* expr, Env* env, Interp* interp) {
    jit_handle_warm_clauses(expr, interp);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) return eval_error("stack engine: failed to create context for handle");
    bool destroy_ctx_on_exit = true;
    defer if (destroy_ctx_on_exit) main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);

    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    if (state == null) return eval_error("handle: failed to allocate state");
    jit_handle_init_state(state, expr, env, interp);
    defer {
        state.frame_active = false;
        if (state.continuation_refcount == 0) {
            handle_effect_state_release(state);
        } else if (ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
            // Continuation escaped; transfer ctx ownership to continuation/resolution path.
            destroy_ctx_on_exit = false;
        }
    }

    main::stack_ctx_init(ctx, &handle_effect_entry, state);

    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    usz my_idx = interp.handler_count;
    EffectHandler* h = &interp.handler_stack[my_idx];
    jit_handle_init_handler(h, expr, env, state);

    if (!jit_handle_copy_clauses(h, expr)) {
        return eval_error("handle: failed to allocate clauses");
    }
    SymbolId* owned_tags = h.tags;
    EffectClause* owned_clauses = h.clauses;
    defer mem::free(owned_tags);
    defer mem::free(owned_clauses);

    interp.handler_count = my_idx + 1;
    defer interp.handler_count = my_idx;

    state.handler_idx = my_idx;
    if (!handle_effect_state_capture_handler_copy(state, h)) {
        return eval_error("handle: failed to capture handler state");
    }

    EvalResult run_result = jit_handle_switch_to_body(ctx, interp);
    if (run_result.error.has_error) return run_result;

    EvalResult dispatch_result;
    if (jit_handle_dispatch_signals(state, expr, env, interp, ctx, &dispatch_result)) {
        return dispatch_result;
    }
    return jit_handle_finish_no_signal(expr, env, interp, ctx);
}

// =============================================================================
// END CORO-BASED HANDLE/SIGNAL/RESOLVE
// =============================================================================

fn EvalResult jit_apply_continuation(Value* k_val, Value* arg, Interp* interp) {
    if (k_val.cont_val == null) {
        return eval_error("cannot resume continuation: null continuation");
    }
    return jit_apply_continuation_impl(k_val, arg, interp);
}
