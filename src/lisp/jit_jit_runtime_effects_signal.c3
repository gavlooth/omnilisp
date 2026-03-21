module lisp;

import main;

fn Value* runtime_effects_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

fn Value* jit_signal_eval_result_or_error(EvalResult r, Interp* interp) @inline {
    if (r.error.has_error) return runtime_effects_raise(interp, "runtime/fast-path-primitive-failed", r.error.message[:256]);
    return r.value;
}

fn Value* jit_signal_apply_fast_path_prim(Interp* interp, Value* fp_prim, Value* arg, bool* handled) {
    int fp_arity = fp_prim.prim_val.arity;
    if (fp_arity == 0) {
        *handled = true;
        return jit_signal_eval_result_or_error(apply_primitive(fp_prim, make_nil(interp), interp), interp);
    }
    if (fp_arity == 1 || fp_arity == -1) {
        *handled = true;
        return jit_signal_eval_result_or_error(apply_primitive(fp_prim, arg, interp), interp);
    }
    if (arg == null || !is_cons(arg)) return null;

    *handled = true;
    EvalResult first = apply_primitive(fp_prim, car(arg), interp);
    Value* applied = jit_signal_eval_result_or_error(first, interp);
    if (applied != null && applied.tag == ERROR) return applied;
    Value* result = jit_apply_value(applied, cdr(arg), interp);
    if (result != null && result.tag == ERROR) return result;
    return result;
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

fn Value* jit_signal_value(Interp* interp, Value* tag, Value* arg) {
    Value* resumed = null;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (jit_signal_try_resume_handler(interp, current, (SymbolId)tag.sym_val, arg, &resumed)) {
        return resumed;
    }

    bool handled = false;
    Value* fast_result = jit_signal_try_fast_path_dispatch(interp, (SymbolId)tag.sym_val, arg, &handled);
    if (handled) return fast_result;
    return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", "unhandled effect");
}
