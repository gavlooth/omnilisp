module lisp;

import std::io;
import main;

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

fn Value* runtime_effect_invalid_tag(Value* tag_value, Interp* interp, char[] context) {
    char[256] ebuf;
    char[] type_name = interp.symbols.get_name(value_type_name(tag_value, interp));
    char[] msg = io::bprintf(&ebuf, "%s: effect tag must resolve to symbol, got %s",
        (ZString)context, (ZString)type_name)!!;
    return runtime_effect_raise(interp, "runtime/effect-invalid-tag", msg);
}

fn Value* eval_effect_tag_value(SymbolId tag, Expr* tag_expr, bool has_tag_expr, Env* env, Interp* interp) {
    if (!has_tag_expr) return make_symbol(interp, tag);
    return jit_eval(tag_expr, env, interp);
}

fn Value* resolve_effect_tag_symbol(
    SymbolId tag,
    Expr* tag_expr,
    bool has_tag_expr,
    Env* env,
    Interp* interp,
    SymbolId* out_tag,
    char[] context
) {
    Value* tag_value = eval_effect_tag_value(tag, tag_expr, has_tag_expr, env, interp);
    if (tag_value != null && tag_value.tag == ERROR) return tag_value;
    if (tag_value == null || tag_value.tag != SYMBOL) {
        return runtime_effect_invalid_tag(tag_value, interp, context);
    }
    *out_tag = tag_value.sym_val;
    return null;
}

fn bool effect_handler_has_tag(EffectHandler* h, SymbolId tag) {
    for (usz j = 0; j < h.clause_count; j++) {
        SymbolId ctag = h.tags != null ? h.tags[j] : h.clauses[j].effect_tag;
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
