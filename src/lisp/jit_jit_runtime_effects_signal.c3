module lisp;

import main;

fn Value* runtime_effects_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

fn Value* jit_signal_call_fast_path_primitive_preserving_error(Interp* interp, Value* fp_prim, Value* arg) @inline {
    Value*[8] args;
    args[0] = arg;
    interp.constructor_type_id = fp_prim.prim_val.tag;
    interp.prim_user_data = fp_prim.prim_val.user_data;
    Value* result = fp_prim.prim_val.func(args[:1], null, interp);
    if (result != null && result.tag == ERROR) return result;
    return result;
}

fn Value* jit_signal_call_fast_path_primitive_list_preserving_error(Interp* interp, Value* fp_prim, Value* arg_list) {
    usz argc = 0;
    Value* curr = arg_list;
    while (curr != null && curr.tag == CONS) {
        argc++;
        curr = curr.cons_val.cdr;
    }
    // stdlib wrappers often encode fixed-arity payloads as dotted cons spines.
    if (curr != null && curr.tag != NIL) argc++;

    Value** argv = argc > 0 ? (Value**)interp.current_scope.alloc(Value*.sizeof * argc) : null;
    if (argc > 0 && argv == null) {
        return runtime_effects_raise(interp, "runtime/fast-path-out-of-memory", "effect fast path: out of memory while staging args");
    }

    curr = arg_list;
    usz i = 0;
    while (curr != null && curr.tag == CONS) {
        argv[i++] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }
    if (curr != null && curr.tag != NIL) {
        argv[i++] = curr;
    }

    interp.constructor_type_id = fp_prim.prim_val.tag;
    interp.prim_user_data = fp_prim.prim_val.user_data;
    Value* result = fp_prim.prim_val.func(argv[..i], null, interp);
    if (result != null && result.tag == ERROR) return result;
    return result;
}

fn Value* jit_signal_apply_fast_path_prim(Interp* interp, Value* fp_prim, Value* arg, bool* handled) {
    int fp_arity = fp_prim.prim_val.arity;
    if (fp_arity == 0) {
        *handled = true;
        return jit_signal_call_fast_path_primitive_preserving_error(interp, fp_prim, make_nil(interp));
    }
    if (fp_arity == 1 || fp_arity == -1) {
        *handled = true;
        return jit_signal_call_fast_path_primitive_preserving_error(interp, fp_prim, arg);
    }
    if (arg == null || !is_cons(arg)) return null;

    *handled = true;
    return jit_signal_call_fast_path_primitive_list_preserving_error(interp, fp_prim, arg);
}

fn bool jit_signal_try_resume_handler(Interp* interp, main::StackCtx* current, SymbolId tag_sym, Value* arg, Value** out_result) {
    if (current == null) return false;
    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* h = &interp.handler_stack[(usz)i];
        for (usz j = 0; j < h.clause_count; j++) {
            if (h.tags[j] != tag_sym || h.effect_state == null) continue;
            HandleEffectState* hstate = (HandleEffectState*)h.effect_state;
            hstate.signaled = true;
            hstate.signal_tag = tag_sym;
            hstate.signal_arg = arg;
            SavedInterpState saved;
            save_interp_state(interp, &saved);
            if (!suspend_with_scope_guard(current, interp.current_scope)) {
                restore_interp_state_preserve_raise(interp, &saved);
                *out_result = raise_error_with_payload_names(
                    interp,
                    "runtime",
                    "runtime/suspend-scope-guard",
                    "stack engine: failed to register suspend scope guard"
                );
                return true;
            }
            restore_interp_state_preserve_raise(interp, &saved);
            *out_result = interp_consume_resume_value_or_nil(interp);
            return true;
        }
    }
    return false;
}

fn Value* jit_signal_try_fast_path_dispatch(Interp* interp, SymbolId tag_sym, Value* arg, bool* handled) {
    for (usz fp_i = 0; fp_i < interp.fast_path_count; fp_i++) {
        if ((uint)interp.fast_path_table[fp_i].tag != (uint)tag_sym) continue;
        return jit_signal_apply_fast_path_prim(interp, interp.fast_path_table[fp_i].prim, arg, handled);
    }
    return null;
}

fn bool jit_signal_try_resume_handler_or_strict_boundary(
    Interp* interp,
    main::StackCtx* current,
    SymbolId tag_sym,
    Value* arg,
    Value** out_result
) {
    if (current == null) return false;
    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* h = &interp.handler_stack[(usz)i];
        if (effect_handler_has_tag(h, tag_sym)) {
            if (h.effect_state != null) {
                HandleEffectState* hstate = (HandleEffectState*)h.effect_state;
                hstate.signaled = true;
                hstate.signal_tag = tag_sym;
                hstate.signal_arg = arg;
                SavedInterpState saved;
                save_interp_state(interp, &saved);
                if (!suspend_with_scope_guard(current, interp.current_scope)) {
                    restore_interp_state_preserve_raise(interp, &saved);
                    *out_result = raise_error_with_payload_names(
                        interp,
                        "runtime",
                        "runtime/suspend-scope-guard",
                        "stack engine: failed to register suspend scope guard"
                    );
                    return true;
                }
                restore_interp_state_preserve_raise(interp, &saved);
                *out_result = interp_consume_resume_value_or_nil(interp);
                return true;
            }
        }
        if (h.strict_mode) {
            *out_result = raise_error_with_payload_names(
                interp,
                "runtime",
                "runtime/strict-unhandled-effect",
                "strict handler: unhandled effect"
            );
            return true;
        }
    }
    return false;
}

fn Value* jit_signal_value(Interp* interp, Value* tag, Value* arg) {
    if (tag == null || tag.tag != SYMBOL) {
        return runtime_effect_invalid_tag(tag, interp, "signal");
    }
    SymbolId tag_sym = (SymbolId)tag.sym_val;
    Value* type_err = jit_signal_type_check(tag_sym, arg, interp);
    if (type_err != null) return type_err;

    Value* resumed = null;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (jit_signal_try_resume_handler_or_strict_boundary(interp, current, tag_sym, arg, &resumed)) {
        return resumed;
    }

    bool handled = false;
    Value* fast_result = jit_signal_try_fast_path_dispatch(interp, tag_sym, arg, &handled);
    if (handled) return fast_result;
    return jit_unhandled_effect_error(tag_sym, arg, interp);
}
